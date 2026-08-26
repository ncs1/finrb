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
- `payment`, `interest`, and `principal`;
- `additional_payment` and `balloon_payment`;
- `interest_only?`.

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
