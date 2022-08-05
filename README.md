# finrb

Ruby gem for financial calculations/modeling.

finrb forked from the ruby [finance](https://github.com/Edward-Intelligence/finance) gem.

## Overview

### Features

Currently implemented features include:

* Uses the [flt](https://github.com/jgoizueta/flt) gem to ensure precision decimal arithmetic in all calculations.
* Fixed-rate mortgage amortization (30/360).
* Interest rates
* Various cash flow computations, such as NPV and IRR.
* Adjustable rate mortgage amortization.
* Payment modifications (i.e., how does paying an additional $75 per month affect the amortization?)
* Utils class provides basic financial calculation utilities (ported from R's [FinCal](https://github.com/felixfan/FinCal) library):
  * Computing bank discount yield (BDY) for a T-bill
  * Computing money market yield (MMY) for a T-bill
  * Cash ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
  * Computing Coefficient of variation
  * Cost of goods sold and ending inventory under three methods (FIFO,LIFO,Weighted average)
  * Current ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
  * Depreciation Expense Recognition - double-declining balance (DDB), the most common declining balance method, which applies two times the straight-line rate to the declining balance.
  * Debt ratio - Solvency ratios measure the firm's ability to satisfy its long-term obligations.
  * Diluted Earnings Per Share
  * Computing the rate of return for each period
  * Convert stated annual rate to the effective annual rate
  * Convert stated annual rate to the effective annual rate with continuous compounding
  * Bond-equivalent yield (BEY), 2 x the semiannual discount rate
  * Computing HPR, the holding period return
  * Equivalent/proportional Interest Rates
  * Basic Earnings Per Share
  * Financial leverage - Solvency ratios measure the firm's ability to satisfy its long-term obligations.
  * Estimate future value (fv)
  * Estimate future value of an annuity
  * Estimate future value (fv) of a single sum
  * Computing the future value of an uneven cash flow series
  * Geometric mean return
  * Gross profit margin - Evaluate a company's financial performance
  * Harmonic mean, average price
  * Computing HPR, the holding period return
  * Bond-equivalent yield (BEY), 2 x the semiannual discount rate
  * Convert holding period return to the effective annual rate
  * Computing money market yield (MMY) for a T-bill
  * Computing IRR, the internal rate of return
  * Calculate the net increase in common shares from the potential exercise of stock options or warrants
  * Long-term debt-to-equity - Solvency ratios measure the firm's ability to satisfy its long-term obligations.
  * Computing HPR, the holding period return
  * Estimate the number of periods
  * Net profit margin - Evaluate a company's financial performance
  * Computing NPV, the PV of the cash flows less the initial (time = 0) outlay
  * Estimate period payment
  * Estimate present value (pv)
  * Estimate present value (pv) of an annuity
  * Estimate present value of a perpetuity
  * Estimate present value (pv) of a single sum
  * Computing the present value of an uneven cash flow series
  * Quick ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
  * Convert a given norminal rate to a continuous compounded rate
  * Convert a given continuous compounded rate to a norminal rate
  * Rate of return for a perpetuity
  * Computing Sampling error
  * Computing Roy's safety-first ratio
  * Computing Sharpe Ratio
  * Depreciation Expense Recognition - Straight-line depreciation (SL) allocates an equal amount of depreciation each year over the asset's useful life
  * Total debt-to-equity - Solvency ratios measure the firm's ability to satisfy its long-term obligations.
  * Computing TWRR, the time-weighted rate of return
  * Calculate weighted average shares - weighted average number of common shares
  * Weighted mean as a portfolio return

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

* [RubyGems Page](https://rubygems.org/gems/finrb)
* [Source Code](http://github.com/ncs1/finrb)
* [Bug Tracker](https://github.com/ncs1/finrb/issues)

## License

See [COPYING](./COPYING) and [COPYING.LESSER](./COPYING.LESSER)
