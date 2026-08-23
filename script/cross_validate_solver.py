#!/usr/bin/env python3
"""Cross-check finrb IRR/XIRR against SciPy and QuantLib.

This is an optional maintainer tool, not a gem dependency. Install its oracles
in an isolated environment with: pip install scipy QuantLib
"""

from __future__ import annotations

import argparse
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
    raise SystemExit("Install oracle dependencies with: pip install scipy QuantLib") from error


ROOT = Path(__file__).resolve().parent.parent
ADAPTER = ROOT / "script" / "finrb_solver_adapter.rb"
LOWER_RATE = -0.999999999
UPPER_RATE = 100.0
MAX_ERROR = 2.0e-11
GUESSES = (0.01, 0.05, 0.5, 1.0, 3.0)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--count", type=int, default=100, help="cases of each kind")
    parser.add_argument("--seed", type=int, default=20260825)
    return parser.parse_args()


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


def finrb_results(cases: list[dict]) -> list[dict]:
    completed = subprocess.run(
        [os.environ.get("RUBY", "ruby"), f"-I{ROOT / 'lib'}", str(ADAPTER)],
        cwd=ROOT,
        input=json.dumps({"cases": cases}),
        text=True,
        capture_output=True,
        check=True,
    )
    return json.loads(completed.stdout)["results"]


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
    leg = ql.Leg(
        [ql.SimpleCashFlow(item["amount"], ql.DateParser.parseISO(item["date"])) for item in transactions[1:]]
    )
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


def main() -> None:
    args = parse_args()
    randomizer = random.Random(args.seed)
    cases = periodic_cases(randomizer, args.count) + dated_cases(randomizer, args.count)
    finrb = finrb_results(cases)
    worst_scipy = 0.0
    worst_quantlib = 0.0

    for index, (test_case, result) in enumerate(zip(cases, finrb, strict=True)):
        if "error" in result:
            raise AssertionError(f"finrb case {index} failed: {result}")
        actual = float(result["value"])
        scipy = scipy_result(test_case)
        quantlib = quantlib_result(test_case)
        worst_scipy = max(worst_scipy, abs(actual - scipy))
        worst_quantlib = max(worst_quantlib, abs(actual - quantlib))
        if abs(actual - scipy) > MAX_ERROR or abs(actual - quantlib) > MAX_ERROR:
            raise AssertionError(
                f"case {index} differs: finrb={actual}, scipy={scipy}, quantlib={quantlib}, input={test_case}"
            )

    print(f"seed={args.seed} cases={len(cases)}")
    print(f"SciPy {__import__('scipy').__version__}: worst absolute difference={worst_scipy:.3g}")
    print(f"QuantLib {ql.__version__}: worst absolute difference={worst_quantlib:.3g}")


if __name__ == "__main__":
    main()
