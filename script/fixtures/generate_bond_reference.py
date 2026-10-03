#!/usr/bin/env python3
"""Generate fixed-rate bond reference fixtures with QuantLib-Python 1.43."""

import json
from calendar import monthrange
from pathlib import Path

import QuantLib as ql


if ql.__version__ != "1.43":
    raise RuntimeError(f"expected QuantLib-Python 1.43, found {ql.__version__}")


ROOT = Path(__file__).resolve().parents[2]
FIXTURE = ROOT / "spec" / "fixtures" / "quantlib_fixed_rate_bonds.json"

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

CASES = [
    {
        "name": "semiannual_leap_year_mid_period",
        "face_value": "1000.00",
        "coupon_rate": "0.05",
        "issue_date": "2024-01-31",
        "maturity_date": "2027-01-31",
        "settlement_date": "2024-04-30",
        "frequency": "semiannual",
        "yield_rate": "0.0475",
        "calendar": "null",
        "payment_convention": "unadjusted",
    },
    {
        "name": "annual_coupon_leap_day_anchor",
        "face_value": "100.00",
        "coupon_rate": "0.04",
        "issue_date": "2024-02-29",
        "maturity_date": "2028-02-29",
        "settlement_date": "2026-02-28",
        "frequency": "annual",
        "yield_rate": "0.035",
        "calendar": "null",
        "payment_convention": "unadjusted",
    },
    {
        "name": "quarterly_calendar_adjusted_payments",
        "face_value": "1000.00",
        "coupon_rate": "0.06",
        "issue_date": "2025-08-31",
        "maturity_date": "2027-08-31",
        "settlement_date": "2026-01-15",
        "frequency": "quarterly",
        "yield_rate": "0.055",
        "calendar": "us_federal_reserve",
        "payment_convention": "modified_following",
    },
    {
        "name": "semiannual_negative_yield",
        "face_value": "500.00",
        "coupon_rate": "0.01",
        "issue_date": "2023-03-15",
        "maturity_date": "2028-03-15",
        "settlement_date": "2025-08-20",
        "frequency": "semiannual",
        "yield_rate": "-0.005",
        "calendar": "null",
        "payment_convention": "unadjusted",
    },
]


def ql_date(value):
    year, month, day = map(int, value.split("-"))
    return ql.Date(day, month, year)


def decimal_text(value):
    return format(value, ".15g")


def calendar_for(name):
    if name == "null":
        return ql.NullCalendar()
    if name == "us_federal_reserve":
        return ql.UnitedStates(ql.UnitedStates.FederalReserve)
    raise ValueError(f"unsupported calendar: {name}")


def build_case(case):
    issue_date = ql_date(case["issue_date"])
    maturity_date = ql_date(case["maturity_date"])
    settlement_date = ql_date(case["settlement_date"])
    frequency, tenor_months = FREQUENCIES[case["frequency"]]
    calendar = calendar_for(case["calendar"])
    payment_convention = PAYMENT_CONVENTIONS[case["payment_convention"]]
    schedule = ql.Schedule(
        issue_date,
        maturity_date,
        ql.Period(tenor_months, ql.Months),
        calendar,
        ql.Unadjusted,
        ql.Unadjusted,
        ql.DateGeneration.Forward,
        issue_date.dayOfMonth() == monthrange(issue_date.year(), issue_date.month())[1],
    )
    day_counter = ql.ActualActual(ql.ActualActual.ISMA, schedule)
    face_value = float(case["face_value"])
    coupon_rate = float(case["coupon_rate"])
    bond = ql.FixedRateBond(
        0,
        face_value,
        schedule,
        [coupon_rate],
        day_counter,
        payment_convention,
        100.0,
        issue_date,
    )

    payment_amounts = {}
    for cashflow in bond.cashflows():
        date = cashflow.date().ISO()
        payment_amounts[date] = payment_amounts.get(date, 0.0) + cashflow.amount()

    future_cashflows = [
        {"date": date, "amount": decimal_text(amount)}
        for date, amount in sorted(payment_amounts.items())
        if ql_date(date) > settlement_date
    ]
    yield_rate = float(case["yield_rate"])
    dirty_price = bond.dirtyPrice(yield_rate, day_counter, ql.Compounded, frequency, settlement_date) * face_value / 100.0
    clean_price = bond.cleanPrice(yield_rate, day_counter, ql.Compounded, frequency, settlement_date) * face_value / 100.0
    accrued_interest = bond.accruedAmount(settlement_date) * face_value / 100.0
    clean_yield = bond.bondYield(
        ql.BondPrice(clean_price * 100.0 / face_value, ql.BondPrice.Clean),
        day_counter,
        ql.Compounded,
        frequency,
        settlement_date,
        1e-12,
        1000,
        0.05,
    )
    dirty_yield = bond.bondYield(
        ql.BondPrice(dirty_price * 100.0 / face_value, ql.BondPrice.Dirty),
        day_counter,
        ql.Compounded,
        frequency,
        settlement_date,
        1e-12,
        1000,
        0.05,
    )

    return {
        **case,
        "payment_dates": [date.ISO() for date in schedule.dates()[1:]],
        "adjusted_payment_dates": sorted(payment_amounts),
        "future_cashflows": future_cashflows,
        "accrued_interest": decimal_text(accrued_interest),
        "dirty_price": decimal_text(dirty_price),
        "clean_price": decimal_text(clean_price),
        "yield_from_clean_price": decimal_text(clean_yield),
        "yield_from_dirty_price": decimal_text(dirty_yield),
    }


def main():
    result = {
        "quantlib_version": ql.__version__,
        "day_count": "Actual/Actual ICMA (ISMA)",
        "yield_compounding": "nominal annual, compounded at coupon frequency",
        "price_units": "cash amount in face_value units",
        "cases": [build_case(case) for case in CASES],
    }
    FIXTURE.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(f"Wrote {len(result['cases'])} QuantLib bond cases to {FIXTURE}")


if __name__ == "__main__":
    main()
