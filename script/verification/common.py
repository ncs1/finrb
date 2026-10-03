"""Shared configuration and Ruby-worker plumbing for randomized checks."""

from __future__ import annotations

import concurrent.futures
from dataclasses import dataclass
import json
import os
from pathlib import Path
import shlex
import subprocess


@dataclass(frozen=True)
class VerificationConfig:
    count: int
    seed: int
    batch_size: int
    workers: int


def add_common_arguments(parser, *, count_help: str, default_batch_size: int = 50, default_seed: int = 20261003) -> None:
    parser.add_argument("--count", type=positive_integer, default=100, help=count_help)
    parser.add_argument("--seed", type=int, default=default_seed)
    parser.add_argument("--batch-size", type=positive_integer, default=default_batch_size)
    parser.add_argument("--workers", type=positive_integer, default=min(4, os.cpu_count() or 1))


def positive_integer(value: str) -> int:
    parsed = int(value)
    if parsed <= 0:
        raise ValueError("must be a positive integer")
    return parsed


def config_from_args(arguments) -> VerificationConfig:
    return VerificationConfig(
        count=arguments.count,
        seed=arguments.seed,
        batch_size=arguments.batch_size,
        workers=arguments.workers,
    )


def run_ruby_adapter(cases: list[dict], *, adapter: Path, root: Path, config: VerificationConfig) -> list[dict]:
    indexed_batches = [
        (start, cases[start : start + config.batch_size])
        for start in range(0, len(cases), config.batch_size)
    ]
    if not indexed_batches:
        return []

    worker_count = min(config.workers, len(indexed_batches))
    partitions = [indexed_batches[index::worker_count] for index in range(worker_count)]
    command = shlex.split(os.environ.get("RUBY", "ruby"))
    command.extend([f"-I{root / 'lib'}", str(adapter)])

    with concurrent.futures.ThreadPoolExecutor(max_workers=worker_count) as executor:
        completed = [
            batch
            for partition in executor.map(
                lambda batches: _run_partition(command, root, batches),
                partitions,
            )
            for batch in partition
        ]

    results = []
    for _, batch_results in sorted(completed):
        results.extend(batch_results)
    if len(results) != len(cases):
        raise RuntimeError(f"Ruby adapter returned {len(results)} results for {len(cases)} cases")
    return results


def _run_partition(command: list[str], root: Path, partition: list[tuple[int, list[dict]]]):
    process = subprocess.Popen(
        command,
        cwd=root,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1,
    )
    results = []
    assert process.stdin is not None and process.stdout is not None

    try:
        for start, batch in partition:
            process.stdin.write(json.dumps({"cases": batch}, separators=(",", ":")) + "\n")
            process.stdin.flush()
            response = process.stdout.readline()
            if not response:
                stderr = process.stderr.read() if process.stderr is not None else ""
                raise RuntimeError(f"finrb adapter stopped before batch {start}: {stderr.strip()}")
            decoded = json.loads(response)
            results.append((start, decoded["results"]))
    finally:
        process.stdin.close()

    stderr = process.stderr.read() if process.stderr is not None else ""
    if process.wait() != 0:
        raise RuntimeError(f"finrb adapter failed: {stderr.strip()}")
    return results


def assert_close(actual: float, expected: float, *, absolute_tolerance: float, relative_tolerance: float = 0.0, label: str) -> float:
    difference = abs(actual - expected)
    tolerance = max(absolute_tolerance, abs(expected) * relative_tolerance)
    if difference > tolerance:
        raise AssertionError(f"{label}: actual={actual}, expected={expected}, difference={difference}, tolerance={tolerance}")
    return difference
