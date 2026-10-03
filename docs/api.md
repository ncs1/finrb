# finrb API and examples

This guide documents finrb's public, namespaced API. All calculations use
`Flt::DecNum` decimal values unless a method explicitly returns another object.
The packaged [RBS declarations](../sig/finrb.rbs) provide the complete machine-
readable method signatures.

Examples marked “verified” are executed in isolated Ruby processes by
`bundle exec rake docs:verify` and by the normal `quality` task.

## Rates

`Finrb::Rate` distinguishes nominal annual percentage rates from effective
annual yields. `duration` is expressed in months for amortization.

<!-- verify-example -->
```ruby
rate = Finrb::Rate.new(0.12, :apr, duration: 360)

raise unless rate.apr == Flt::DecNum('0.12')
raise unless rate.monthly == Flt::DecNum('0.01')
raise unless rate.apy.round(6) == Flt::DecNum('0.126825')
```

An effective rate converts to the periodic rate that compounds back to the
same annual yield; it is not divided by twelve.

```ruby
Finrb::Rate.new(0.12, :apy)
Finrb::Rate.new(0.05, :nominal, compounds: :quarterly)
Finrb::Rate.new(0.05, :effective, compounds: :annually)
```

Class conversions:

- `Finrb::Rate.to_effective(nominal_rate, periods)`
- `Finrb::Rate.to_nominal(effective_rate, periods)`

## Cashflows

Periodic cashflows are equally spaced. IRR returns a per-period rate and NPV
accepts a per-period discount rate.

<!-- verify-example -->
```ruby
cashflows = [-4000, 1200, 1410, 1875, 1050]
rate = Finrb::Cashflow.irr(cashflows)

raise unless rate.round(6) == Flt::DecNum('0.142993')
raise unless Finrb::Cashflow.npv(cashflows, 0.10).round(2) == Flt::DecNum('382.08')
```

MIRR separates the rate paid to finance negative cashflows from the rate earned
by reinvesting positive cashflows.

<!-- verify-example -->
```ruby
rate = Finrb::Cashflow.mirr(
  [-100, 39, 59, 55, 20],
  finance_rate: 0.10,
  reinvestment_rate: 0.12
)

raise unless rate.round(6) == Flt::DecNum('0.204377')
```

XIRR and XNPV accept chronological `Finrb::Transaction` objects. The default
date convention is actual calendar days divided by 365.

<!-- verify-example -->
```ruby
transactions = [
  Finrb::Transaction.new(-10_000, date: Date.new(2020, 1, 1)),
  Finrb::Transaction.new(12_500, date: Date.new(2022, 1, 1))
]

rate = Finrb::Cashflow.xirr(transactions, 0.10)
raise unless rate.apy.round(6) == Flt::DecNum('0.117863')
raise unless Finrb::Cashflow.xnpv(transactions, rate.apy).abs < Flt::DecNum('1e-10')
```

Cashflows must include at least one positive and one negative amount. Discrete
rates and guesses must be greater than `-1`. Multiple IRRs can exist; the
optional guess selects the nearby sign-changing root finrb attempts to bracket.

## Business calendars

`Finrb::Calendars` ships dependency-free, date-only market calendars for the
US Federal Reserve and Tel Aviv Stock Exchange (TASE). They report named
market holidays, identify business days, adjust dates, and advance by business
days. `holiday?` includes weekends, while `holiday_names` returns names for
market-specific closures. Dates use Ruby's `Date`; no network lookup or
external holiday dataset is used.

`USFederalReserve` supports 1950-01-01 through 2065-12-31; `IsraelTase`
supports 2000-01-01 through 2050-12-31. Dates outside either range raise
`RangeError` rather than silently extrapolating. The US profile represents
Federal Reserve Bank payment days, not NYSE sessions or federal employee
leave. TASE models full-day closures only; short sessions are still business
days. Immutable per-instance `additional_holidays:` and `removed_holidays:`
overrides add or reopen named closures; weekly weekends remain closed.

<!-- verify-example -->
```ruby
us = Finrb::Calendars::USFederalReserve.new
tase = Finrb::Calendars::IsraelTase.new

raise unless us.business_day?(Date.new(2026, 7, 3))
raise unless us.holiday_names(Date.new(2026, 11, 26)).include?('Thanksgiving Day')
raise unless us.holidays_between(Date.new(2026, 11, 25), Date.new(2026, 11, 28)) == {
  Date.new(2026, 11, 26) => ['Thanksgiving Day']
}
raise unless tase.business_day?(Date.new(2025, 12, 28))
raise unless tase.business_day?(Date.new(2026, 1, 5))
raise unless tase.holiday_names(Date.new(2026, 1, 4)).include?('TASE trading-week transition')
```

For the full market-rule history, sources, validation ranges, QuantLib
comparison, and evidence behind known disagreements, see the
[business calendars reference](calendars.md).

Supported date adjustments are `:following`, `:modified_following`,
`:preceding`, `:modified_preceding`, `:half_month_modified_following`,
`:nearest`, and `:unadjusted`. A nearest-day tie rolls forward. For example:

<!-- verify-example -->
```ruby
calendar = Finrb::Calendars::USFederalReserve.new
date = Date.new(2026, 1, 31)

raise unless calendar.adjust(date, convention: :following) == Date.new(2026, 2, 2)
raise unless calendar.adjust(date, convention: :modified_following) == Date.new(2026, 1, 30)
raise unless calendar.advance(Date.new(2026, 10, 9), business_days: 1) == Date.new(2026, 10, 13)
```

Calendar-aware amortization is opt-in. Supply both a calendar and an explicit
business-day convention with `start_date:`; finrb adjusts each generated
payment date before calculating interest under the selected day-count
convention (Actual/365 Fixed by default). The origination date remains
unchanged. Without a calendar, dated schedules remain unadjusted as before.

<!-- verify-example -->
```ruby
rate = Finrb::Rate.new(0.12, :apr, duration: 2)
loan = Finrb::Amortization.new(
  100_000,
  rate,
  start_date: Date.new(2026, 1, 31),
  calendar: Finrb::Calendars::USFederalReserve.new,
  business_day_convention: :modified_following
)

raise unless loan.schedule.map(&:date) == [Date.new(2026, 2, 27), Date.new(2026, 3, 31)]
raise unless loan.schedule.first.interest == Flt::DecNum('887.67')
```

## Day counts

`Finrb::DayCount.year_fraction(start_date, end_date, convention:)` returns a
decimal year fraction. `:actual_365_fixed` (the default) and `:actual_360`
divide signed actual elapsed calendar days by a fixed denominator. The
`:actual_actual_icma` convention additionally requires the containing regular
coupon reference period and coupon frequency via `reference_period_start:`,
`reference_period_end:`, and `frequency:`. It calculates actual elapsed days
divided by reference-period days and coupon frequency. Callers with schedules
spanning multiple coupon periods must split the interval at each reference
period; finrb does not infer a schedule from two dates. Inputs are date-only
`Date` instances interpreted on the proleptic Gregorian calendar. Reversed
dates produce a negative fraction. This API does not imply support for
30/360 or other Actual/Actual variants, which have multiple distinct
definitions.

The same convention can be selected for dated amortization with `day_count:`.
Its default preserves existing Actual/365 Fixed results. A non-default
convention requires `start_date:`; undated amortization continues using its
existing monthly-rate behavior. This option affects loan accrual only; XIRR
and XNPV continue using their existing actual-days/365 convention.

<!-- verify-example -->
```ruby
start_date = Date.new(2025, 1, 15)
end_date = Date.new(2025, 2, 15)

raise unless Finrb::DayCount.year_fraction(start_date, end_date, convention: :actual_360) == Flt::DecNum(31) / Flt::DecNum(360)
```

Actual/Actual ICMA requires explicit reference-period context:

<!-- verify-example -->
```ruby
fraction = Finrb::DayCount.year_fraction(
  Date.new(2024, 1, 31),
  Date.new(2024, 4, 30),
  convention: :actual_actual_icma,
  reference_period_start: Date.new(2024, 1, 31),
  reference_period_end: Date.new(2024, 7, 31),
  frequency: 2
)

raise unless fraction == Flt::DecNum(90) / (Flt::DecNum(182) * 2)
```

## Payment schedules

`Finrb::Schedule` is a reusable, immutable date schedule for monthly,
quarterly, semiannual, or annual payment dates. It is date infrastructure,
not an amortization or bond calculator: it does not calculate accrual amounts,
cashflows, principal, or yields. Both `start_date:` and `maturity_date:` are
date-only `Date` values, and maturity must be later than the start date.

Regular dates are generated from the original start-date anchor, preserving
month-end or clamping the original day in shorter months. `:none` requires
maturity to fall on a regular date. `stub: :short_final` explicitly permits a
short final period; long or front stubs are not supported. If a calendar is
provided, a business-day convention is required, and each period retains both
its unadjusted date and adjusted payment date. Accrual starts at the previous
adjusted payment boundary (the first period starts on `start_date`). Calendars
may reject dates outside their documented support range.

`Schedule.from_months` is a convenience constructor for callers whose term is
expressed in months, including `Amortization`, where `Rate#duration` remains a
month count. The `Period` records have zero-based `index`,
`accrual_start_date`, `unadjusted_payment_date`, `payment_date`, and `stub`;
`stub` is `:short_final` on the shortened last period and `nil` otherwise;
`short_final_stub?` identifies the shortened last period. The schedule,
period records, and returned date arrays are frozen.

<!-- verify-example -->
```ruby
schedule = Finrb::Schedule.new(
  start_date: Date.new(2025, 1, 31),
  maturity_date: Date.new(2026, 3, 31),
  frequency: :quarterly,
  stub: :short_final
)

raise unless schedule.payment_dates == [
  Date.new(2025, 4, 30), Date.new(2025, 7, 31), Date.new(2025, 10, 31),
  Date.new(2026, 1, 31), Date.new(2026, 3, 31)
]
raise unless schedule.periods.last.short_final_stub?
```

## Fixed-rate bonds

`Finrb::FixedRateBond` models a regular, fixed-coupon bullet bond. It requires
positive `face_value`, non-negative annual `coupon_rate`, `issue_date`, and
`maturity_date`; `frequency:` defaults to `:semiannual` and accepts the four
`Finrb::Schedule` frequencies. Maturity must align to a regular schedule.
Irregular stubs, amortizing principal, floating rates, ex-coupon periods,
curves, spreads, and settlement lags are not modeled.

Coupons accrue under Actual/Actual ICMA using unadjusted regular coupon
boundaries. Each complete coupon is `face_value * coupon_rate / frequency`;
`accrued_interest` applies the same reference-period fraction from the current
coupon start to settlement, so it reconciles to the coupon amount at the
unadjusted period end. If a calendar and business-day convention are supplied,
they adjust payment dates only, not accrual boundaries. Settlement is passed
to each valuation method, must be on or after issue, and must precede maturity;
cashflows on settlement are excluded.

Bond price arguments and results are cash amounts in the same units as
`face_value` (not a per-100 quote). Dirty price is the present value of future
cashflows using a nominal annual yield compounded at the coupon frequency and
schedule-aware Actual/Actual ICMA time. Clean price is dirty price minus
accrued interest. `yield_to_maturity` accepts a clean price by default or
`price_type: :dirty`, and inverts that same price convention using finrb's
existing rate solver. The supported yield domain is greater than -100%.

<!-- verify-example -->
```ruby
bond = Finrb::FixedRateBond.new(
  face_value: 1_000,
  coupon_rate: 0.05,
  issue_date: Date.new(2024, 1, 31),
  maturity_date: Date.new(2026, 1, 31),
  frequency: :semiannual
)
settlement_date = Date.new(2024, 4, 30)
dirty = bond.dirty_price(settlement_date:, yield_rate: 0.0475)
clean = bond.clean_price(settlement_date:, yield_rate: 0.0475)

raise unless (dirty - clean - bond.accrued_interest(settlement_date:)).abs < Flt::DecNum('1e-20')
raise unless bond.yield_to_maturity(settlement_date:, price: clean).round(8) == Flt::DecNum('0.0475').round(8)
```

## Amortization

Rates used in amortization require a duration in months. Payments are negative
cash outflows; balances, interest, and principal repaid are non-negative.

<!-- verify-example -->
```ruby
rate = Finrb::Rate.new(0.0425, :apr, duration: 30 * 12)
loan = Finrb::Amortization.new(250_000, rate)
first = loan.schedule.first

raise unless loan.payment == Flt::DecNum('-1229.85')
raise unless first.opening_balance == Flt::DecNum('250000')
raise unless first.opening_balance + first.interest + first.payment == first.closing_balance
raise unless loan.schedule.last.closing_balance.zero?
```

Each frozen `Finrb::Amortization::Entry` exposes:

- `period`, `opening_balance`, and `closing_balance`;
- `date` when the loan was created with `start_date:`;
- `payment`, `interest`, and `principal`;
- `additional_payment` and `balloon_payment`;
- `interest_only?`.

Supplying `start_date: Date` opts into a dated schedule. Payment frequency is
monthly by default; `:quarterly`, `:semiannual`, and `:annual` are also
supported. `Rate#duration` remains a month count and the sum of rate durations
defines the loan term. Its dates are generated through `Finrb::Schedule` from
the original start-date anchor, preserving month-end or clamping the original
day in shorter months without date drift. A term not divisible by its payment
frequency is rejected unless `stub: :short_final` explicitly allows a shorter
final period;
`:none` is the default. Rate changes in a dated non-monthly schedule must land
on a payment date. `interest_only_periods` counts schedule periods.

Each period accrues simple nominal APR times the selected day-count year
fraction, with interest posted to cents. The default is Actual/365 Fixed
(`APR * actual_days / 365`); `day_count: :actual_360` uses
`APR * actual_days / 360`. These are explicit dated-mode conventions, not a
universal loan standard. Dates are unadjusted: weekends and holidays are not
shifted, and no business calendar is consulted unless a calendar and explicit
rolling convention are supplied. Without `start_date:`, the existing monthly
rate calculation and schedule-entry hash shape are unchanged; non-monthly
frequencies and non-default stub handling require a start date.

<!-- verify-example -->
```ruby
require 'date'

rate = Finrb::Rate.new(0.05, :apr, duration: 3)
loan = Finrb::Amortization.new(1000, rate, start_date: Date.new(2024, 1, 31))

raise unless loan.schedule.map(&:date) == [
  Date.new(2024, 2, 29), Date.new(2024, 3, 31), Date.new(2024, 4, 30)
]
raise unless loan.schedule.first.interest == Flt::DecNum('3.97')
raise unless loan.schedule.last.closing_balance.zero?
```

For a term that ends between regular quarterly dates, select the short final
stub explicitly. Rate durations still specify the total term in months.

<!-- verify-example -->
```ruby
rate = Finrb::Rate.new(0.06, :apr, duration: 14)
loan = Finrb::Amortization.new(
  1_000,
  rate,
  start_date: Date.new(2025, 1, 31),
  frequency: :quarterly,
  stub: :short_final
)

raise unless loan.schedule.map(&:date) == [
  Date.new(2025, 4, 30), Date.new(2025, 7, 31), Date.new(2025, 10, 31),
  Date.new(2026, 1, 31), Date.new(2026, 3, 31)
]
raise unless loan.schedule.last.closing_balance.zero?
```

Balloon targets, interest-only periods, and origination fees compose in one
schedule. An upfront fee reduces net proceeds; a financed fee increases the
opening balance.

<!-- verify-example -->
```ruby
rate = Finrb::Rate.new(0.06, :apr, duration: 12)
loan = Finrb::Amortization.new(
  100_000,
  rate,
  balloon: 20_000,
  interest_only_periods: 2,
  origination_fee: 2_000,
  finance_origination_fee: true
)

raise unless loan.principal == Flt::DecNum('100000')
raise unless loan.net_proceeds == Flt::DecNum('100000')
raise unless loan.amount_financed == Flt::DecNum('102000')
raise unless loan.schedule.first.interest_only?
raise unless loan.schedule.sum(&:principal) == loan.amount_financed
```

`cashflow_yield` is available for dated schedules. It runs finrb's XIRR over
the borrower's net proceeds at origination and each scheduled payment at its
due date. The final payment includes any balloon, which is therefore counted
once. The returned `Finrb::Rate` expresses the effective annual
cashflow-equivalent borrowing cost; it is not a jurisdiction-specific legal
APR and follows the current `Finrb::Cashflow.xirr` configuration. Undated
amortizations do not have enough timing information and are rejected.

<!-- verify-example -->
```ruby
require 'date'

rate = Finrb::Rate.new(0.06, :apr, duration: 12)
loan = Finrb::Amortization.new(
  100_000,
  rate,
  start_date: Date.new(2025, 1, 15),
  origination_fee: 2_000
)
borrower_yield = loan.cashflow_yield

raise unless borrower_yield.is_a?(Finrb::Rate)
raise unless borrower_yield.apy > rate.apy
```

A block can modify the scheduled payment template, commonly for a constant
extra payment. It is not yet a general period-indexed prepayment strategy.

```ruby
Finrb::Amortization.new(250_000, rate) { |payment| payment.amount - 150 }
```

## Time value of money

`Finrb::TVM` provides periodic calculations:

| Method | Purpose |
| --- | --- |
| `discount_rate` | Solve a periodic rate from PV, FV, payment, and periods |
| `fv`, `pv` | Combined lump-sum and annuity value |
| `fv_annuity`, `pv_annuity` | Annuity value |
| `fv_simple`, `pv_simple` | Single-sum value |
| `fv_uneven`, `pv_uneven` | Uneven periodic cashflows |
| `n_period` | Number of periods |
| `npv` | Net present value |
| `pmt` | Periodic payment |
| `pv_perpetuity`, `r_perpetuity` | Perpetuity value or rate |

`type: 0` means end-of-period payments and `type: 1` means beginning-of-period
payments.

Zero-rate annuities and payments use their linear mathematical limits. Other
periodic TVM rates may be negative but must remain greater than `-1`.
`discount_rate` searches that full domain around `guess:`; callers may instead
supply both `lower:` and `upper:` as an explicit sign-changing bracket.

<!-- verify-example -->
```ruby
future = Finrb::TVM.fv(r: 0.05, n: 10, pv: -1000, pmt: 0)
present = Finrb::TVM.pv(r: 0.05, n: 10, fv: -future, pmt: 0)

raise unless present.round(10) == Flt::DecNum('1000')
```

## Investment returns and risk

| Method | Convention |
| --- | --- |
| `cagr` | Compound growth over a positive integer number of periods |
| `geometric_mean`, `harmonic_mean` | Multi-period return or average price |
| `hpr`, `twrr`, `wpr` | Holding-period, time-weighted, or weighted portfolio return |
| `volatility` | Sample standard deviation by default; `sample: false` selects population |
| `downside_deviation` | Root-mean-square shortfall using every observation in the denominator |
| `sortino_ratio` | Mean excess return divided by downside deviation |
| `max_drawdown` | Largest peak-to-trough loss as a non-negative fraction |
| `annualize_return` | Compound periodic return |
| `annualize_volatility` | Square-root-of-time scaling |
| `sharpe_ratio`, `sf_ratio` | Supplied-return risk ratios |

<!-- verify-example -->
```ruby
returns = [-0.10, 0.05, -0.05]

raise unless Finrb::Returns.cagr(beginning_value: 100, ending_value: 121, periods: 2).round(6) == Flt::DecNum('0.1')
raise unless Finrb::Returns.volatility(returns: returns).positive?
raise unless Finrb::Returns.downside_deviation(returns: returns).positive?
raise unless Finrb::Returns.max_drawdown(values: [100, 120, 90, 150, 105]) == Flt::DecNum('0.3')
```

## Yields

`Finrb::Yields` contains money-market and annualized-yield conversions:

- `bdy`, `bdy2mmy`;
- `ear`, `ear_continuous`, `ear2bey`, `ear2hpr`;
- `eir`;
- `hpr2bey`, `hpr2ear`, `hpr2mmy`, `mmy2hpr`;
- `r_continuous`, `r_norminal`.

The historical public method name `r_norminal` is intentionally retained.

<!-- verify-example -->
```ruby
yield_rate = Finrb::Yields.ear(r: 0.12, m: 12)
raise unless yield_rate.round(6) == Flt::DecNum('0.126825')
```

## Ratios and accounting

`Finrb::Ratios` provides liquidity, leverage, profitability, earnings, and
share-count calculations:

```text
cash_ratio, current_ratio, quick_ratio, debt_ratio, financial_leverage,
lt_d2e, total_d2e, gpm, npm, eps, diluted_eps, iss, was
```

`Finrb::Accounting` provides inventory costing and depreciation:

- `cogs` using `FIFO`, `LIFO`, or weighted-average (`WAC`) inventory;
- `ddb` for double-declining-balance depreciation;
- `slde` for straight-line depreciation.

<!-- verify-example -->
```ruby
ratio = Finrb::Ratios.current_ratio(ca: 8000, cl: 2000)
inventory = Finrb::Accounting.cogs(
  uinv: 2,
  pinv: 2,
  units: [3, 5],
  price: [3, 5],
  sinv: 7,
  method: 'FIFO'
)

raise unless ratio == Flt::DecNum('4')
raise unless inventory.key?(:cost_of_goods)
raise unless inventory.key?(:ending_inventory)
```

## Configuration, precision, and errors

`Finrb.config` returns an immutable snapshot. Use `Finrb.configure` at startup
or `Finrb.with_config` for a temporary execution-local override.

```ruby
Finrb.configure { |config| config.eps = '1e-14' }
Finrb.with_config(guess: 0.25) { Finrb::Cashflow.irr([-100, 230, -132]) }
```

General calculations retain decimal precision until callers round for display.
Amortization postings use `Finrb::Precision.money`: two decimal places with
half-up rounding and final-payment reconciliation.

Public failures use:

- `Finrb::InvalidCashflowError` for malformed cashflows;
- `Finrb::DomainError` for illegal financial/numerical domains;
- `Finrb::ConvergenceError` when a root cannot be bracketed or solved;
- `ArgumentError` for invalid public arguments and options.

## Opt-in core extensions

Loading `finrb` does not modify Ruby core classes. Legacy fluent calls require
an explicit load:

<!-- verify-example -->
```ruby
require 'finrb/core_ext'

raise unless [-4000, 1200, 1410, 1875, 1050].irr.round(3) == Flt::DecNum('0.143')
rate = Finrb::Rate.new(0.05, :apr, duration: 12)
raise unless 10_000.amortize(rate).balance.zero?
```
