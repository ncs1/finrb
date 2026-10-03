# Solver reference fixtures

These fixtures validate finrb against independent implementations. They are test evidence, not runtime data.

## SciPy

`scipy_brentq_irr.json` was generated with `scipy.optimize.brentq` 1.18.1. SciPy solves the periodic NPV equation using binary double precision and a caller-supplied sign-changing bracket.

## QuantLib

`quantlib_yield_rate_xirr.json` was generated with QuantLib 1.43 `CashFlows.yieldRate`. The settings intentionally match finrb's current XIRR semantics:

- Actual/365 Fixed day count;
- annually compounded effective rate;
- settlement-date cashflows excluded from the future leg, with the initial outlay supplied as NPV;
- `1e-13` accuracy and 1000 maximum evaluations.

QuantLib's cached bond-yield fixtures are not copied here. Those values incorporate coupon schedules, accrued interest, clean/dirty prices, market calendars, and bond-specific day-count conventions that finrb XIRR does not currently model. Treating them as plain XIRR fixtures would compare different financial contracts.

`quantlib_dated_amortization.json` was generated with QuantLib-Python 1.43 by
`script/generate_amortization_reference.py`. The fixtures use monthly forward
schedules with a null calendar and unadjusted dates, Actual/365 Fixed, and
nominal APR with simple compounding over each individual period. QuantLib
provides the schedule dates and period growth factors; an independent Python
decimal calculation discounts the level end-of-period installments and
replays cent-rounded postings. Cases cover leap-year month-end dates, a
mid-month 28–31 day schedule, a balloon, and a negative APR. The RSpec fixture
test runs without Python or QuantLib installed because the expected results
are committed.

Regenerate the fixture in a Python environment with the pinned QuantLib
reference package installed:

```shell
python3 -m pip install QuantLib==1.43
python3 script/generate_amortization_reference.py
```

This reference does not validate business-day adjustment, holiday calendars,
alternate day counts, or an effective-APY accrual convention.

`quantlib_fixed_rate_bonds.json` was generated with QuantLib-Python 1.43
`FixedRateBond`, schedule-aware Actual/Actual ICMA, and nominal annual yield
compounded at coupon frequency. It compares regular semiannual, annual, and
quarterly bonds; leap-year anchors; a US Federal Reserve calendar with adjusted
payment dates; mid-period and coupon-date settlements; and a negative yield.
Prices are converted from QuantLib's per-100 quote to cash amounts in the
fixture's face-value units. The Ruby specs consume only this committed fixture.

The optional generator dependency is isolated from finrb runtime and test
dependencies:

```shell
python3 -m pip install --requirement script/requirements-bond-verification.txt
python3 script/generate_bond_reference.py
```

This is a deliberately narrow conventional bond comparison. It does not cover
stubs, ex-coupon dates, settlement lags, floating rates, curves, spreads, or
markets outside the selected calendar case.

## Live randomized comparison

Install the optional dependencies into a Python environment of your choice:

```shell
python3 -m pip install --requirement script/requirements-solver-verification.txt
```

Run the randomized comparison with that environment active:

```shell
bundle exec rake solver:verify
```

The task uses `python3` by default. Set `PYTHON` to select another interpreter without requiring any particular environment manager:

```shell
PYTHON=/path/to/python bundle exec rake solver:verify
```

The default run generates 100 periodic and 100 dated cases with seed `20260825`, four workers, and batches of 50. Override these settings through environment variables when needed:

```shell
COUNT=500 SEED=17 WORKERS=8 BATCH_SIZE=25 bundle exec rake solver:verify
```

Alternatively, build and run the isolated solver-verification Docker target.
It contains its own Python environment with the pinned reference dependencies:

```shell
bundle exec rake docker:verify_solver
```

The Docker wrapper accepts the same environment-variable overrides:

```shell
COUNT=500 SEED=17 WORKERS=8 BATCH_SIZE=25 bundle exec rake docker:verify_solver
```

The harness generates both periodic and irregularly dated conventional cashflows, selects finrb guesses independently from the constructed root, calls finrb through `script/solver_adapter.rb`, and compares each result with both external reference implementations. It sends bounded newline-delimited JSON batches through persistent Ruby worker processes instead of constructing one unbounded stdin payload. Ruby workers and SciPy comparisons run concurrently; QuantLib comparisons stay sequential because its Python binding returned invalid results under concurrent access.

Verification reports finrb's convergence count and worst normalized NPV
residual before the differences from the external references. A run fails when
any generated conventional cashflow does not converge, its normalized residual
exceeds `1e-12`, or its rate differs from a reference by more than `2e-11`.
Wall-clock timings are secondary diagnostics: they include different process
startup and concurrency costs and are not direct solver microbenchmarks.

QuantLib receives the constructed root as its guess because its linear auto-bracketing can cross invalid yield domains from poor guesses; the comparison still independently evaluates its NPV, derivative, and safeguarded Newton implementation. The versions used by the maintained fixtures are pinned in `script/requirements-solver-verification.txt`. Neither Python package is a finrb runtime dependency.

## Business calendars

`script/verify_calendars.py` compares every supported date against QuantLib
1.43's `UnitedStates::FederalReserve` and `Israel::TASE` business-day results.
The profiles intentionally bound their ranges to 1950–2065 (US) and
2000–2050 (TASE); date-only status is compared for every day, not just named
holiday examples. The US profile agrees across 42,369 dates. TASE has 75
classified differences: eight festival-eve dates are confirmed by annual TASE
schedules, while 57 are projections of the recurring rule. The remaining ten
are the 2001 Hebrew-date shift, the 2026 trading-week transition, and the 2038
statutory Independence Day adjustment. The verifier explicitly asserts the
dates and status directions of those ten researched differences; all 75 exact
rows are also fingerprinted and category-counted, so new or shifted mismatches
fail. The evidence and limits are described in [`docs/calendars.md`](../../docs/calendars.md).
The same check runs in CI. Install its optional reference dependency with:

```shell
python3 -m pip install --requirement script/requirements-calendar-verification.txt
bundle exec rake calendar:verify
```

This is an exhaustive comparison to QuantLib's implementation, not a claim
that QuantLib overrides an official TASE calendar or that annual schedules
have been published for every year in the supported future range.

`israel_2001_calendar_reference.json` records the independently corroborated
Hebrew holiday dates used to check the 2001 QuantLib date-table discrepancy.
The calendar specs assert the matching named closures and the four dates that
QuantLib closes one day early.

`tase_verified_festival_eves.json` records festival-eve closures from TASE
annual schedules for 2015, 2019, and 2021–2025. The 2025 Hebrew schedule
confirms June 1 and September 22 as no-trading eves. The three 2026 dates are
separate: official Bank of Israel and MFA calendars corroborate the eve dates,
and exchange-calendar listings corroborate the TASE closures. The maintainer
confirmed the dates and instructed finrb to assume trading was closed; the
TASE live page's dynamic rows were not captured directly. The calendar specs
preserve that evidence distinction. These examples do not establish the
recurring rule for every year through 2050. The 2022 schedule incorrectly lists May 16 as
Shavuot Eve; the actual eve was June 4. The fixture records this source error
and the specs ensure finrb does not encode it.
