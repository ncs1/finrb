#!/usr/bin/env python3
"""Cross-validate randomized finrb fixed-rate bonds against QuantLib 1.43."""

from __future__ import annotations

import argparse
from calendar import monthrange
from datetime import date, timedelta
import random
import time
from pathlib import Path

from common import add_common_arguments, assert_close, config_from_args, run_ruby_adapter

try:
    import QuantLib as ql
except ImportError as error:
    raise SystemExit("Install QuantLib from script/verification/requirements-bonds.txt") from error


ROOT = Path(__file__).resolve().parents[2]
ADAPTER = ROOT / "script" / "verification" / "adapters" / "finrb_reference_adapter.rb"
FREQUENCIES = {
    "monthly": (ql.Monthly, 1),
    "quarterly": (ql.Quarterly, 3),
    "semiannual": (ql.Semiannual, 6),
    "annual": (ql.Annual, 12),
}
PAYMENT_CONVENTIONS = {
    "unadjusted": ql.Unadjusted,
    "following": ql.Following,
    "modified_following": ql.ModifiedFollowing,
}
PRICE_ABSOLUTE_TOLERANCE = 1.0e-9
PRICE_RELATIVE_TOLERANCE = 1.0e-12
YIELD_TOLERANCE = 2.0e-10
LATEST_CALENDAR_MATURITY_YEAR = 2064


class UnsupportedQuantLibCase(Exception):
    """QuantLib's schedule-aware reference cannot value this boundary case."""


def ql_date(value: date):
    return ql.Date(value.day, value.month, value.year)


def anchored_date(anchor: date, month_offset: int) -> date:
    month_index = anchor.year * 12 + anchor.month - 1 + month_offset
    year, zero_based_month = divmod(month_index, 12)
    month = zero_based_month + 1
    target_last_day = monthrange(year, month)[1]
    anchor_is_month_end = anchor.day == monthrange(anchor.year, anchor.month)[1]
    day = target_last_day if anchor_is_month_end else min(anchor.day, target_last_day)
    return date(year, month, day)


def ql_calendar(name: str):
    if name == "null":
        return ql.NullCalendar()
    if name == "us_federal_reserve":
        return ql.UnitedStates(ql.UnitedStates.FederalReserve)
    raise ValueError(f"unsupported calendar {name}")


def bond_case(randomizer: random.Random, index: int) -> tuple[dict, dict]:
    frequency_name = randomizer.choice(tuple(FREQUENCIES))
    ql_frequency, months_per_period = FREQUENCIES[frequency_name]
    period_count = randomizer.randint(4, 60 if frequency_name == "monthly" else 32)
    term_years = (months_per_period * period_count + 11) // 12
    issue_year = randomizer.randint(1980, LATEST_CALENDAR_MATURITY_YEAR - term_years)
    issue_month = randomizer.randint(1, 12)
    issue_last_day = monthrange(issue_year, issue_month)[1]
    issue_day = randomizer.choice((1, 15, issue_last_day))
    issue_date = date(issue_year, issue_month, issue_day)
    maturity_date = anchored_date(issue_date, months_per_period * period_count)
    calendar_name = randomizer.choice(("null", "us_federal_reserve"))
    convention_name = "unadjusted" if calendar_name == "null" else randomizer.choice(tuple(PAYMENT_CONVENTIONS))
    calendar = ql_calendar(calendar_name)
    ql_schedule = ql.Schedule(
        ql_date(issue_date),
        ql_date(maturity_date),
        ql.Period(months_per_period, ql.Months),
        calendar,
        ql.Unadjusted,
        ql.Unadjusted,
        ql.DateGeneration.Forward,
        issue_date.day == monthrange(issue_date.year, issue_date.month)[1],
    )
    day_counter = ql.ActualActual(ql.ActualActual.ISMA, ql_schedule)
    face_value = f"{randomizer.uniform(100.0, 1_000_000.0):.2f}"
    coupon_rate = f"{randomizer.uniform(0.0, 0.15):.10f}"
    yield_rate = f"{randomizer.uniform(-0.03, 0.15):.10f}"
    bond = ql.FixedRateBond(
        0,
        float(face_value),
        ql_schedule,
        [float(coupon_rate)],
        day_counter,
        PAYMENT_CONVENTIONS[convention_name],
        100.0,
        ql_date(issue_date),
    )

    unadjusted_dates = [date.fromisoformat(item.ISO()) for item in ql_schedule.dates()[1:]]
    payment_dates = sorted({cashflow.date().ISO() for cashflow in bond.cashflows()})
    if date.fromisoformat(payment_dates[-1]) > maturity_date:
        raise UnsupportedQuantLibCase("rolled final payment exceeds Actual/Actual schedule end")
    if randomizer.random() < 0.3:
        settlement_date = randomizer.choice(unadjusted_dates[:-1])
    elif randomizer.random() < 0.3:
        candidates = [date.fromisoformat(value) for value in payment_dates[:-1]]
        settlement_date = randomizer.choice(candidates)
    else:
        settlement_offset = randomizer.randrange((maturity_date - issue_date).days)
        settlement_date = issue_date + timedelta(days=settlement_offset)
    if settlement_date >= date.fromisoformat(payment_dates[-1]):
        raise UnsupportedQuantLibCase("settlement is on or after the final payment date")

    case = {
        "name": f"bond_{index:04d}_{frequency_name}_{calendar_name}_{convention_name}",
        "kind": "fixed_rate_bond",
        "face_value": face_value,
        "coupon_rate": coupon_rate,
        "issue_date": issue_date.isoformat(),
        "maturity_date": maturity_date.isoformat(),
        "settlement_date": settlement_date.isoformat(),
        "frequency": frequency_name,
        "yield_rate": yield_rate,
        "calendar": calendar_name,
        "payment_convention": convention_name,
    }

    settlement = ql_date(settlement_date)
    cashflow_amounts: dict[str, float] = {}
    for cashflow in bond.cashflows():
        cashflow_date = cashflow.date().ISO()
        cashflow_amounts[cashflow_date] = cashflow_amounts.get(cashflow_date, 0.0) + cashflow.amount()
    future_cashflows = [
        {"date": cashflow_date, "amount": amount}
        for cashflow_date, amount in sorted(cashflow_amounts.items())
        if ql_date(date.fromisoformat(cashflow_date)) > settlement
    ]
    face = float(face_value)
    yield_value = float(yield_rate)
    dirty_price = bond.dirtyPrice(yield_value, day_counter, ql.Compounded, ql_frequency, settlement) * face / 100.0
    clean_price = bond.cleanPrice(yield_value, day_counter, ql.Compounded, ql_frequency, settlement) * face / 100.0
    accrued_interest = bond.accruedAmount(settlement) * face / 100.0
    clean_yield = bond.bondYield(
        ql.BondPrice(clean_price * 100.0 / face, ql.BondPrice.Clean),
        day_counter,
        ql.Compounded,
        ql_frequency,
        settlement,
        1e-12,
        1000,
        yield_value,
    )
    dirty_yield = bond.bondYield(
        ql.BondPrice(dirty_price * 100.0 / face, ql.BondPrice.Dirty),
        day_counter,
        ql.Compounded,
        ql_frequency,
        settlement,
        1e-12,
        1000,
        yield_value,
    )
    expected = {
        "payment_dates": [item.ISO() for item in ql_schedule.dates()[1:]],
        "adjusted_payment_dates": payment_dates,
        "cashflows": future_cashflows,
        "accrued_interest": accrued_interest,
        "dirty_price": dirty_price,
        "clean_price": clean_price,
        "yield_from_clean_price": clean_yield,
        "yield_from_dirty_price": dirty_yield,
    }
    return case, expected


def compare_case(index: int, case: dict, expected: dict, actual: dict) -> tuple[float, float, float]:
    if "error" in actual:
        raise AssertionError(f"finrb failed case {index} ({case['name']}): {actual}")

    for field in ("payment_dates", "adjusted_payment_dates"):
        if actual[field] != expected[field]:
            raise AssertionError(f"case {index} {field} differs: finrb={actual[field]}, QuantLib={expected[field]}, input={case}")

    expected_cashflows = expected["cashflows"]
    actual_cashflows = actual["cashflows"]
    if [item["date"] for item in actual_cashflows] != [item["date"] for item in expected_cashflows]:
        raise AssertionError(f"case {index} cashflow dates differ: finrb={actual_cashflows}, QuantLib={expected_cashflows}, input={case}")

    max_cashflow_error = max(
        (
            assert_close(
                float(actual_item["amount"]),
                float(expected_item["amount"]),
                absolute_tolerance=PRICE_ABSOLUTE_TOLERANCE,
                relative_tolerance=PRICE_RELATIVE_TOLERANCE,
                label=f"case {index} cashflow {expected_item['date']} differs",
            )
            for actual_item, expected_item in zip(actual_cashflows, expected_cashflows, strict=True)
        ),
        default=0.0,
    )
    max_price_error = 0.0
    for field in ("accrued_interest", "dirty_price", "clean_price"):
        difference = assert_close(
            float(actual[field]),
            float(expected[field]),
            absolute_tolerance=PRICE_ABSOLUTE_TOLERANCE,
            relative_tolerance=PRICE_RELATIVE_TOLERANCE,
            label=f"case {index} {field} differs; input={case}",
        )
        max_price_error = max(max_price_error, difference)

    max_yield_error = 0.0
    for field in ("yield_from_clean_price", "yield_from_dirty_price"):
        difference = assert_close(
            float(actual[field]),
            float(expected[field]),
            absolute_tolerance=YIELD_TOLERANCE,
            label=f"case {index} {field} differs; input={case}",
        )
        max_yield_error = max(max_yield_error, difference)
    return max_cashflow_error, max_price_error, max_yield_error


def main() -> None:
    if ql.__version__ != "1.43":
        raise RuntimeError(f"expected QuantLib 1.43, found {ql.__version__}")

    parser = argparse.ArgumentParser(description=__doc__)
    add_common_arguments(parser, count_help="randomized bond cases", default_batch_size=10)
    config = config_from_args(parser.parse_args())
    randomizer = random.Random(config.seed)
    generated = []
    rejected_boundary_cases: dict[str, int] = {}
    attempts = 0
    while len(generated) < config.count and attempts < config.count * 10:
        try:
            generated.append(bond_case(randomizer, attempts))
        except UnsupportedQuantLibCase as error:
            reason = str(error)
            rejected_boundary_cases[reason] = rejected_boundary_cases.get(reason, 0) + 1
        attempts += 1
    if len(generated) != config.count:
        raise RuntimeError(f"generated only {len(generated)} of {config.count} supported cases")
    cases = [case for case, _ in generated]
    expected = [reference for _, reference in generated]

    started = time.perf_counter()
    actual = run_ruby_adapter(cases, adapter=ADAPTER, root=ROOT, config=config)
    ruby_seconds = time.perf_counter() - started
    errors = [compare_case(index, case, reference, result) for index, (case, reference, result) in enumerate(zip(cases, expected, actual, strict=True))]

    print(f"QuantLib {ql.__version__}: {len(cases)} randomized regular fixed-rate bonds; seed={config.seed}")
    skipped = sum(rejected_boundary_cases.values())
    print(f"Skipped {skipped} unsupported QuantLib cases: {rejected_boundary_cases or 'none'}.")
    print(f"Ruby workers={config.workers} batch_size={config.batch_size} elapsed={ruby_seconds:.3f}s")
    print(f"worst absolute errors: cashflow={max(item[0] for item in errors):.3g} price/accrual={max(item[1] for item in errors):.3g} yield={max(item[2] for item in errors):.3g}")
    print("Compared schedule dates, payment dates, future cashflows, accrued interest, clean/dirty prices, and both yield inversions.")


if __name__ == "__main__":
    main()
