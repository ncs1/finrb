#!/usr/bin/env python3
"""Cross-check finrb IRR/XIRR against SciPy and QuantLib.

This is an optional maintainer tool, not a gem dependency. Install its oracles
in the active Python environment from script/requirements-cross-validation.txt.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import datetime as dt
import json
import os
import random
import subprocess
import sys
from pathlib import Path

try:
    import QuantLib as ql
    from scipy.optimize import brentq
except ImportError as error:
    raise SystemExit(
        "Install oracle dependencies from script/requirements-cross-validation.txt"
    ) from error


ROOT = Path(__file__).resolve().parent.parent
ADAPTER = ROOT / "script" / "finrb_solver_adapter.rb"
LOWER_RATE = -0.999999999
UPPER_RATE = 100.0
MAX_ERROR = 2.0e-11
GUESSES = (0.01, 0.05, 0.5, 1.0, 3.0)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--count", type=positive_integer, default=100, help="cases of each kind")
    parser.add_argument("--seed", type=int, default=20260825)
    parser.add_argument("--batch-size", type=positive_integer, default=50)
    parser.add_argument("--workers", type=positive_integer, default=min(4, os.cpu_count() or 1))
    return parser.parse_args()


def positive_integer(value: str) -> int:
    parsed = int(value)
    if parsed <= 0:
        raise argparse.ArgumentTypeError("must be a positive integer")
    return parsed


def periodic_cases(randomizer: random.Random, count: int) -> list[dict]:
    cases = []
    for _ in range(count):
        expected = randomizer.uniform(-0.9, 5.0)
        positive = [randomizer.uniform(0.01, 1_000_000.0) for _ in range(randomizer.randint(1, 20))]
        initial = -sum(value / (1.0 + expected) ** period for period, value in enumerate(positive, 1))
        cases.append(
            {
                "kind": "irr",
                "amounts": [initial, *positive],
                "guess": randomizer.choice(GUESSES),
                "oracle_guess": expected,
            }
        )
    return cases


def dated_cases(randomizer: random.Random, count: int) -> list[dict]:
    cases = []
    start = dt.date(2000, 1, 1)
    for _ in range(count):
        expected = randomizer.uniform(-0.8, 3.0)
        days = sorted(randomizer.sample(range(1, 10_001), randomizer.randint(1, 12)))
        positive = [randomizer.uniform(0.01, 1_000_000.0) for _ in days]
        initial = -sum(value / (1.0 + expected) ** (day / 365.0) for value, day in zip(positive, days))
        transactions = [{"amount": initial, "date": start.isoformat()}]
        transactions.extend(
            {"amount": value, "date": (start + dt.timedelta(days=day)).isoformat()}
            for value, day in zip(positive, days)
        )
        cases.append(
            {
                "kind": "xirr",
                "transactions": transactions,
                "guess": randomizer.choice(GUESSES),
                "oracle_guess": expected,
            }
        )
    return cases


def batches(cases: list[dict], batch_size: int) -> list[tuple[int, list[dict]]]:
    return [(start, cases[start : start + batch_size]) for start in range(0, len(cases), batch_size)]


def run_ruby_partition(partition: list[tuple[int, list[dict]]]) -> list[tuple[int, list[dict]]]:
    process = subprocess.Popen(
        [os.environ.get("RUBY", "ruby"), f"-I{ROOT / 'lib'}", str(ADAPTER)],
        cwd=ROOT,
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
            results.append((start, json.loads(response)["results"]))
    finally:
        process.stdin.close()

    stderr = process.stderr.read() if process.stderr is not None else ""
    if process.wait() != 0:
        raise RuntimeError(f"finrb adapter failed: {stderr.strip()}")
    return results


def finrb_results(cases: list[dict], batch_size: int, workers: int) -> list[dict]:
    indexed_batches = batches(cases, batch_size)
    worker_count = min(workers, len(indexed_batches))
    partitions = [indexed_batches[index::worker_count] for index in range(worker_count)]

    with concurrent.futures.ThreadPoolExecutor(max_workers=worker_count) as executor:
        completed = [batch for partition in executor.map(run_ruby_partition, partitions) for batch in partition]

    results = []
    for _, batch_results in sorted(completed):
        results.extend(batch_results)
    return results


def npv(amounts: list[float], rate: float) -> float:
    return sum(value / (1.0 + rate) ** period for period, value in enumerate(amounts))


def xnpv(transactions: list[dict], rate: float) -> float:
    start = dt.date.fromisoformat(transactions[0]["date"])
    return sum(
        transaction["amount"]
        / (1.0 + rate) ** ((dt.date.fromisoformat(transaction["date"]) - start).days / 365.0)
        for transaction in transactions
    )


def scipy_result(test_case: dict) -> float:
    if test_case["kind"] == "irr":
        return brentq(lambda rate: npv(test_case["amounts"], rate), LOWER_RATE, UPPER_RATE, xtol=1.0e-14)
    return brentq(lambda rate: xnpv(test_case["transactions"], rate), LOWER_RATE, UPPER_RATE, xtol=1.0e-14)


def quantlib_result(test_case: dict) -> float:
    if test_case["kind"] == "irr":
        start = dt.date(2000, 1, 1)
        transactions = [{"amount": test_case["amounts"][0], "date": start.isoformat()}]
        transactions.extend(
            {"amount": amount, "date": (start + dt.timedelta(days=365 * period)).isoformat()}
            for period, amount in enumerate(test_case["amounts"][1:], 1)
        )
    else:
        transactions = test_case["transactions"]

    settlement = ql.DateParser.parseISO(transactions[0]["date"])
    cashflows = [
        ql.SimpleCashFlow(item["amount"], ql.DateParser.parseISO(item["date"])) for item in transactions[1:]
    ]
    leg = ql.Leg(cashflows)
    return ql.CashFlows.yieldRate(
        leg,
        -transactions[0]["amount"],
        ql.Actual365Fixed(),
        ql.Compounded,
        ql.Annual,
        False,
        settlement,
        settlement,
        1.0e-13,
        1000,
        test_case["oracle_guess"],
    )


def actual_result(item: tuple[int, dict, dict]) -> tuple[int, dict, float]:
    index, test_case, result = item
    if "error" in result:
        raise AssertionError(f"finrb case {index} failed: {result}")
    return index, test_case, float(result["value"])


def compare_scipy(item: tuple[int, dict, float]) -> float:
    index, test_case, actual = item
    expected = scipy_result(test_case)
    error = abs(actual - expected)
    if error > MAX_ERROR:
        raise AssertionError(f"case {index} differs: finrb={actual}, scipy={expected}, input={test_case}")
    return error


def compare_quantlib(item: tuple[int, dict, float]) -> float:
    index, test_case, actual = item
    expected = quantlib_result(test_case)
    if expected is None:
        raise RuntimeError(f"QuantLib returned no result for case {index}: {test_case}")
    error = abs(actual - expected)
    if error > MAX_ERROR:
        raise AssertionError(f"case {index} differs: finrb={actual}, quantlib={expected}, input={test_case}")
    return error


def main() -> None:
    args = parse_args()
    randomizer = random.Random(args.seed)
    cases = periodic_cases(randomizer, args.count) + dated_cases(randomizer, args.count)
    finrb = finrb_results(cases, args.batch_size, args.workers)
    raw_results = [(index, test_case, result) for index, (test_case, result) in enumerate(zip(cases, finrb, strict=True))]
    indexed = [actual_result(item) for item in raw_results]

    # QuantLib's Python binding returned invalid results under concurrent
    # access, so evaluate it sequentially before starting SciPy threads.
    quantlib_errors = [compare_quantlib(item) for item in indexed]

    with concurrent.futures.ThreadPoolExecutor(max_workers=args.workers) as executor:
        scipy_errors = list(executor.map(compare_scipy, indexed))

    worst_scipy = max(scipy_errors)
    worst_quantlib = max(quantlib_errors)

    print(f"seed={args.seed} cases={len(cases)} workers={args.workers} batch_size={args.batch_size}")
    print(f"SciPy {__import__('scipy').__version__}: worst absolute difference={worst_scipy:.3g}")
    print(f"QuantLib {ql.__version__}: worst absolute difference={worst_quantlib:.3g}")


if __name__ == "__main__":
    main()
