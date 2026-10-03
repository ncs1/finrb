#!/usr/bin/env python3
"""Generate dated-amortization fixtures from QuantLib's Actual/365F factors."""

import argparse
from decimal import Decimal, ROUND_HALF_UP
import json
from pathlib import Path

import QuantLib as ql


if ql.__version__ != "1.43":
    raise RuntimeError(f"expected QuantLib-Python 1.43, found {ql.__version__}")


CASES = [
    {
        "name": "leap_year_month_end_with_balloon",
        "principal": "100000.00",
        "apr": "0.05",
        "start_date": "2024-01-31",
        "maturity_date": "2024-07-31",
        "balloon": "15000.00",
        "end_of_month": True,
    },
    {
        "name": "mid_month_actual_days",
        "principal": "100000.00",
        "apr": "0.12",
        "start_date": "2025-01-15",
        "maturity_date": "2026-01-15",
        "balloon": "0.00",
        "end_of_month": False,
    },
    {
        "name": "negative_apr",
        "principal": "5000.00",
        "apr": "-0.12",
        "start_date": "2025-01-15",
        "maturity_date": "2025-07-15",
        "balloon": "0.00",
        "end_of_month": False,
    },
]

CENT = Decimal("0.01")
RATE_QUANTUM = Decimal("0.000000000000001")


def money(value):
    return value.quantize(CENT, rounding=ROUND_HALF_UP)


def ql_date(iso_date):
    year, month, day = map(int, iso_date.split("-"))
    return ql.Date(day, month, year)


def build_case(case):
    start = ql_date(case["start_date"])
    maturity = ql_date(case["maturity_date"])
    schedule = ql.Schedule(
        start,
        maturity,
        ql.Period(1, ql.Months),
        ql.NullCalendar(),
        ql.Unadjusted,
        ql.Unadjusted,
        ql.DateGeneration.Forward,
        case["end_of_month"],
    )
    dates = list(schedule)
    if len(dates) < 2:
        raise ValueError(f"{case['name']} must contain at least one period")

    day_counter = ql.Actual365Fixed()
    annual_rate = ql.InterestRate(
        float(case["apr"]), day_counter, ql.Simple, ql.Once
    )
    periods = []
    for start_date, end_date in zip(dates, dates[1:]):
        year_fraction = day_counter.yearFraction(start_date, end_date)
        growth_factor = annual_rate.compoundFactor(start_date, end_date)
        period_rate = (Decimal(str(growth_factor)) - Decimal("1")).quantize(
            RATE_QUANTUM, rounding=ROUND_HALF_UP
        )
        periods.append(
            {
                "date": end_date.ISO(),
                "actual_days": int(end_date - start_date),
                "year_fraction": format(Decimal(str(year_fraction)), ".15f"),
                "period_rate": format(period_rate, ".15f"),
            }
        )

    period_rates = [Decimal(item["period_rate"]) for item in periods]
    discount_factors = []
    cumulative_growth = Decimal("1")
    for period_rate in period_rates:
        cumulative_growth *= Decimal("1") + period_rate
        discount_factors.append(Decimal("1") / cumulative_growth)

    principal = Decimal(case["principal"])
    balloon = Decimal(case["balloon"])
    scheduled_payment = money(
        (principal - balloon * discount_factors[-1]) / sum(discount_factors)
    )
    balance = principal
    rows = []
    for period, period_rate in zip(periods, period_rates):
        opening_balance = balance
        interest = money(opening_balance * period_rate)
        balance += interest
        payment = -scheduled_payment
        if abs(payment) > balance:
            payment = -balance
        balance += payment
        rows.append(
            {
                "date": period["date"],
                "actual_days": period["actual_days"],
                "year_fraction": period["year_fraction"],
                "period_rate": period["period_rate"],
                "opening_balance": format(opening_balance, ".2f"),
                "interest": format(interest, ".2f"),
                "payment": format(payment, ".2f"),
                "balloon_payment": "0.00",
                "closing_balance": format(balance, ".2f"),
            }
        )

    if balance:
        rows[-1]["balloon_payment"] = format(min(balloon, balance), ".2f")
        final_payment = Decimal(rows[-1]["payment"]) - balance
        rows[-1]["payment"] = format(final_payment, ".2f")
        rows[-1]["closing_balance"] = "0.00"

    result = {
        "name": case["name"],
        "principal": case["principal"],
        "apr": case["apr"],
        "start_date": case["start_date"],
        "maturity_date": case["maturity_date"],
        "balloon": case["balloon"],
        "scheduled_payment": format(-scheduled_payment, ".2f"),
        "periods": rows,
    }
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("spec/fixtures/quantlib_dated_amortization.json"),
    )
    arguments = parser.parse_args()
    fixture = {
        "source": "QuantLib-Python 1.43",
        "conventions": {
            "schedule": "monthly, Forward, NullCalendar, Unadjusted",
            "day_count": "Actual/365 Fixed",
            "rate": "nominal APR, Simple compounding, Once frequency per period",
            "payment": "end of period; present-value annuity using QuantLib period factors",
            "money_rounding": "half up to cents",
        },
        "cases": [build_case(case) for case in CASES],
    }
    arguments.output.parent.mkdir(parents=True, exist_ok=True)
    arguments.output.write_text(json.dumps(fixture, indent=2) + "\n", encoding="utf-8")
    print(f"Wrote {len(fixture['cases'])} QuantLib cases to {arguments.output}")


if __name__ == "__main__":
    main()
