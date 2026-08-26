# finrb

[![CI](https://github.com/ncs1/finrb/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/ncs1/finrb/actions/workflows/ci.yml)
[![CodeQL](https://github.com/ncs1/finrb/actions/workflows/codeql.yml/badge.svg)](https://github.com/ncs1/finrb/actions/workflows/codeql.yml)
[![RuboCop](https://github.com/ncs1/finrb/actions/workflows/rubocop.yml/badge.svg)](https://github.com/ncs1/finrb/actions/workflows/rubocop.yml)
[![MRI](https://img.shields.io/badge/MRI-3.3_%7C_3.4_%7C_4.0-CC342D?logo=ruby&logoColor=white)](https://github.com/ncs1/finrb/actions/workflows/ci.yml)
[![Architectures](https://img.shields.io/badge/architectures-x86__64_%7C_arm64-4169E1)](https://github.com/ncs1/finrb/actions/workflows/ci.yml)
[![Experimental engines](https://img.shields.io/badge/experimental-JRuby_%7C_TruffleRuby-F0AD4E)](https://github.com/ncs1/finrb/actions/workflows/ci.yml)

Precision-first financial mathematics for Ruby.

finrb provides decimal calculations for cashflows, interest rates, loan
amortization, time value of money, investment returns, financial ratios, and
basic accounting. Public APIs live under explicit `Finrb` namespaces, and
loading the gem does not modify Ruby's core classes.

finrb is a fork of the Ruby [finance](https://github.com/Edward-Intelligence/finance)
gem and includes calculations ported from R's
[FinCal](https://github.com/felixfan/FinCal) package.

## Installation

Add finrb to your bundle:

```shell
bundle add finrb
```

Or install it directly:

```shell
gem install finrb
```

finrb requires Ruby 3.3 or newer.

## Quick start

```ruby
require 'finrb'

cashflows = [-4000, 1200, 1410, 1875, 1050]

Finrb::Cashflow.irr(cashflows).round(6)
# => Flt::DecNum('0.142993')

Finrb::Cashflow.npv(cashflows, 0.10).round(2)
# => Flt::DecNum('382.08')
```

Inputs are validated and calculations return `Flt::DecNum` values unless an
API explicitly returns another financial object, such as `Finrb::Rate`.

## Financial domains

| Namespace | Purpose |
| --- | --- |
| `Finrb::Cashflow` | NPV, XNPV, IRR, and XIRR |
| `Finrb::Rate` | Nominal APR, effective APY, and compounding conversions |
| `Finrb::Amortization` | Fixed and adjustable-rate loan amortization |
| `Finrb::TVM` | Present value, future value, payments, periods, and perpetuities |
| `Finrb::Returns` | Holding-period, time-weighted, portfolio, and risk-adjusted returns |
| `Finrb::Yields` | Money-market, bond-equivalent, effective, and continuous yield conversions |
| `Finrb::Ratios` | Liquidity, leverage, profitability, and per-share ratios |
| `Finrb::Accounting` | Inventory costing and depreciation |

The [API and examples guide](docs/api.md) documents each financial domain and
its public calculations. Packaged RBS declarations are available under `sig/`.

## Cashflows

Periodic IRR is a per-period rate. XIRR evaluates irregularly dated
`Finrb::Transaction` objects and returns an effective annual `Finrb::Rate`.

```ruby
require 'date'

transactions = [
  Finrb::Transaction.new(-10_000, date: Date.new(2020, 1, 1)),
  Finrb::Transaction.new(12_500, date: Date.new(2022, 1, 1))
]

rate = Finrb::Cashflow.xirr(transactions, 0.10)
rate.apy.round(6)
# => Flt::DecNum('0.117863')
```

Cashflows must contain at least one positive and one negative amount. Dated
cashflows must be chronological and every transaction must have a date. For
ordinary discrete discounting, rates and guesses must be greater than `-1`.

IRR and XIRR can have more than one economically valid root. The optional
guess controls which nearby sign-changing root finrb selects:

```ruby
cashflows = [-100, 230, -132] # roots at 10% and 20%

Finrb::Cashflow.irr(cashflows, 0.05).round(2) # => 0.10
Finrb::Cashflow.irr(cashflows, 0.25).round(2) # => 0.20
```

## Rates and amortization

APR is a nominal annual rate; APY is an effective annual rate. They are not
aliases:

```ruby
rate = Finrb::Rate.new(0.12, :apr)

rate.monthly # => Flt::DecNum('0.01')
rate.apy.round(6) # => Flt::DecNum('0.126825')
```

Create a fixed-rate loan by giving the rate a duration in months:

```ruby
rate = Finrb::Rate.new(0.0425, :apr, duration: 30 * 12)
loan = Finrb::Amortization.new(250_000, rate)

loan.payment      # => Flt::DecNum('-1229.85')
loan.interest.sum # => Flt::DecNum('192745.98')
loan.balance      # => Flt::DecNum('0.00')

first = loan.schedule.first
first.opening_balance
first.interest
first.principal
first.payment
first.balloon_payment
first.interest_only?
first.closing_balance
```

Pass several duration-bearing rates for an adjustable-rate schedule. A block
can modify each scheduled payment, for example to add a $150 principal payment:

```ruby
faster = Finrb::Amortization.new(250_000, rate) do |period|
  period.payment - 150
end
```

Payments and interest follow the sign convention used throughout finrb:
money received is positive and money paid is negative.
Schedule balances, interest, principal repaid, and additional principal are
non-negative; the schedule's payment field is negative.

Set a residual principal target to create a balloon loan. Regular installments
amortize only the non-balloon portion, and the final payment settles the stated
balloon:

```ruby
balloon_loan = Finrb::Amortization.new(250_000, rate, balloon: 100_000)
balloon_loan.schedule.last.balloon_payment # => Flt::DecNum('100000')
```

`balloon` is the contractual residual target. Because regular postings are
rounded to cents, the actual `balloon_payment` in the final schedule row can
differ from that target by a few cents.

Leading interest-only periods defer scheduled principal repayment and amortize
the balance over the remaining term:

```ruby
interest_only = Finrb::Amortization.new(250_000, rate, interest_only_periods: 24)
interest_only.schedule.first.interest_only? # => true
interest_only.schedule.first.principal      # => Flt::DecNum('0')
```

Origination fees can either reduce the borrower's net proceeds or be added to
the financed balance:

```ruby
cash_fee = Finrb::Amortization.new(250_000, rate, origination_fee: 2_500)
cash_fee.net_proceeds    # => Flt::DecNum('247500')
cash_fee.amount_financed # => Flt::DecNum('250000')

financed_fee = Finrb::Amortization.new(
  250_000,
  rate,
  origination_fee: 2_500,
  finance_origination_fee: true
)
financed_fee.net_proceeds    # => Flt::DecNum('250000')
financed_fee.amount_financed # => Flt::DecNum('252500')
```

## Configuration

Configure process-wide defaults during application startup:

```ruby
Finrb.configure do |config|
  config.eps = '1e-12'
  config.guess = 0.10
  config.business_days = false
  config.periodic_compound = false
end
```

Configuration is validated and published as one immutable snapshot.
`Finrb.config` is read-only. Use `Finrb.with_config` for a temporary override
that is restored afterward and does not leak into another thread:

```ruby
Finrb.with_config(guess: 0.25) do
  Finrb::Cashflow.irr(cashflows)
end
```

The `business_days` compatibility option excludes weekends only. It is not a
holiday calendar or a market business-day convention.

## Precision, rounding, and failures

General calculations retain the active `Flt::DecNum` context and are not
rounded for display. Callers choose presentation precision with `round` or a
formatter. `Finrb.config.eps` controls root-solver convergence; it does not set
decimal arithmetic precision.

Amortization is deliberately different because payments and interest are
monetary postings. They are rounded to cents using half-up rounding, and any
remaining cent-level balance is allocated to the final payment. These policies
are exposed through `Finrb::Precision`.

finrb reports invalid financial or numerical states explicitly:

- `Finrb::InvalidCashflowError` for malformed cashflow sequences
- `Finrb::DomainError` for values outside a calculation's legal domain
- `Finrb::ConvergenceError` when a root cannot be bracketed or solved
- `ArgumentError` for invalid public inputs and options

## Migrating from the legacy API

The current API contains intentional breaking changes:

- `Finrb::Utils` was removed. Use `Finrb::TVM`, `Returns`, `Yields`, `Ratios`,
  or `Accounting` according to the calculation's domain.
- Loading `finrb` no longer adds methods to `Array` or `Numeric`.
- `Finrb.config` is immutable; use `Finrb.configure` or `Finrb.with_config`.
- APR is nominal and APY is effective, so conversions now follow their stated
  financial semantics.

Applications migrating gradually can explicitly load the legacy fluent core
extensions:

```ruby
require 'finrb/core_ext'

[-4000, 1200, 1410, 1875, 1050].irr
250_000.amortize(rate)
```

New code should use `Finrb::Cashflow.irr(cashflows)` and
`Finrb::Amortization.new(principal, rate)`.

## Development and verification

Install the bundle and run the self-contained quality checks:

```shell
bundle install
bundle exec rake quality
bundle exec rubocop
bundle exec rake security:audit
bundle exec rake package:verify
```

The quality task runs the RSpec suite with line and branch coverage, generated
IRR/XIRR properties, committed SciPy/QuantLib reference fixtures, and RBS
validation.

`security:audit` updates ruby-advisory-db and checks the locked dependencies.
`package:verify` builds the gem, validates its contents and metadata, installs
it with only its declared runtime dependencies, and runs packaged API smoke
tests without publishing or retaining the temporary installation.

CI runs every supported MRI version on x86-64, native ARM64, and Docker, plus
compatibility specs and Docker builds for the current stable JRuby and
TruffleRuby. Alternative Ruby jobs are initially informational while their
dependency and numerical compatibility is assessed.

Select the MRI version used by the ordinary Docker tasks with `RUBY_VER`:

```shell
RUBY_VER=3.3 bundle exec rake docker:build
RUBY_VER=3.3 bundle exec rake docker:test
RUBY_VER=3.3 bundle exec rake docker:run
```

On an x86-64 development machine with Docker Buildx and ARM64 emulation
available, build, test, and run the ARM64 development image with:

```shell
bundle exec rake docker:arm64:build
bundle exec rake docker:arm64:test
bundle exec rake docker:arm64:run
```

Docker Desktop normally provides the required emulation. A Linux Docker Engine
installation must have an ARM64-capable Buildx builder and binfmt/QEMU support
configured by the operator.

The alternative Ruby development images share a minimal, package-manager-
independent Dockerfile. Build, test, and run either implementation with:

```shell
bundle exec rake docker:jruby:build
bundle exec rake docker:jruby:test
bundle exec rake docker:jruby:run

bundle exec rake docker:truffleruby:build
bundle exec rake docker:truffleruby:test
bundle exec rake docker:truffleruby:run
```

The defaults track JRuby 10 on JDK 21 and the current TruffleRuby Community
image. Override them with `JRUBY_IMAGE` or `TRUFFLERUBY_IMAGE` when testing a
specific release. These images use `gemfiles/engines.gemfile`, which contains
only finrb's runtime dependencies and RSpec; MRI-only development tooling such
as RBS, RuboCop, and coverage is deliberately excluded from engine
compatibility runs.

Maintainers with the optional Python environment can run the larger seeded
solver verification campaign:

```shell
python3 -m pip install --requirement script/requirements-solver-verification.txt
bundle exec rake solver:verify
```

The Python packages are verification references, not gem dependencies. See
[the fixture documentation](spec/fixtures/README.md) for reproducibility,
Docker, batching, and tolerance details.

## Project links

- [RubyGems](https://rubygems.org/gems/finrb)
- [Source](https://github.com/ncs1/finrb)
- [Issue tracker](https://github.com/ncs1/finrb/issues)

## Acknowledgements

- Martin Bjeldbak Madsen, Bill Kranec, and the contributors to the original
  [finance](https://github.com/Edward-Intelligence/finance) gem
- Yanhui Fan and the contributors to R's
  [FinCal](https://github.com/felixfan/FinCal) package

## License

finrb is available under the GNU Lesser General Public License v3.0 or later.
See [COPYING](COPYING) and [COPYING.LESSER](COPYING.LESSER).
