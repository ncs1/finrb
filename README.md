# finrb

[![CI](https://github.com/ncs1/finrb/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/ncs1/finrb/actions/workflows/ci.yml)
[![CodeQL](https://github.com/ncs1/finrb/actions/workflows/codeql.yml/badge.svg)](https://github.com/ncs1/finrb/actions/workflows/codeql.yml)
[![RuboCop](https://github.com/ncs1/finrb/actions/workflows/rubocop.yml/badge.svg)](https://github.com/ncs1/finrb/actions/workflows/rubocop.yml)

Ruby gem for financial calculations/modeling.

finrb forked from the ruby [finance](https://github.com/Edward-Intelligence/finance) gem.

## Overview

### Features

Currently implemented features include:

- Uses the [flt](https://github.com/jgoizueta/flt) gem to ensure precision decimal arithmetic in all calculations.
- Fixed-rate mortgage amortization (30/360).
- Interest rates
- Various cash flow computations, such as NPV and IRR.
- Adjustable rate mortgage amortization.
- Payment modifications (i.e., how does paying an additional $75 per month affect the amortization?)
- Utils class provides basic financial calculation utilities (ported from R's [FinCal](https://github.com/felixfan/FinCal) library):

  - Basic Earnings Per Share

  - Bond-equivalent yield (BEY), 2 x the semiannual discount rate

  - Calculate the net increase in common shares from the potential exercise of stock options or warrants

  - Calculate weighted average shares - weighted average number of common shares

  - Cash ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.

  - Computing Coefficient of variation

  - Computing HPR, the holding period return

  - Computing IRR, the internal rate of return

  - Computing NPV, the PV of the cash flows less the initial (time = 0) outlay

  - Computing Roy's safety-first ratio

  - Computing Sampling error

  - Computing Sharpe Ratio

  - Computing TWRR, the time-weighted rate of return

  - Computing bank discount yield (BDY) for a T-bill

  - Computing money market yield (MMY) for a T-bill

  - Computing the future value of an uneven cash flow series

  - Computing the present value of an uneven cash flow series

  - Computing the rate of return for each period

  - Convert a given continuous compounded rate to a norminal rate

  - Convert a given norminal rate to a continuous compounded rate

  - Convert holding period return to the effective annual rate

  - Convert stated annual rate to the effective annual rate (with continuous compounding)

  - Cost of goods sold and ending inventory under three methods (FIFO,LIFO,Weighted average)

  - Current ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.

  - Debt ratio - Solvency ratios measure the firm's ability to satisfy its long-term obligations.

  - Depreciation Expense Recognition - Straight-line depreciation (SL) allocates an equal amount of depreciation each year over the asset's useful life

  - Depreciation Expense Recognition - double-declining balance (DDB), the most common declining balance method, which applies two times the straight-line rate to the declining balance.

  - Diluted Earnings Per Share

  - Equivalent/proportional Interest Rates

  - Estimate future value (fv) (of a single sum)

  - Estimate future value of an annuity

  - Estimate period payment

  - Estimate present value (pv) (of a single sum) (of an annuity)

  - Estimate present value of a perpetuity

  - Estimate the number of periods

  - Financial leverage - Solvency ratios measure the firm's ability to satisfy its long-term obligations.

  - Geometric mean return

  - Gross profit margin - Evaluate a company's financial performance

  - Harmonic mean, average price

  - Long-term debt-to-equity - Solvency ratios measure the firm's ability to satisfy its long-term obligations.

  - Net profit margin - Evaluate a company's financial performance

  - Quick ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.

  - Rate of return for a perpetuity

  - Total debt-to-equity - Solvency ratios measure the firm's ability to satisfy its long-term obligations.

  - Weighted mean as a portfolio return

### Configuration

In `config/initializers/finrb.rb` Finrb allows to set tolerance (eps) and default guess for IRR and XIRR calculations, such as:

```ruby
Finrb.configure do |config|
  config.eps = '1.0e-12'
  config.guess = 0.5
  config.business_days = false # only relevant when using Transaction object, skips weekends
  config.periodic_compound = false # only relevant when using Transaction object
end
```

### API and examples

See [api.md](docs/api.md)

## Resources

- [RubyGems Page](https://rubygems.org/gems/finrb)
- [Source Code](https://github.com/ncs1/finrb)
- [Bug Tracker](https://github.com/ncs1/finrb/issues)

## License

See [COPYING](./COPYING) and [COPYING.LESSER](./COPYING.LESSER)
