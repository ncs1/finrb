# finrb changelog

## 1.1.0

### Investment returns and risk

- Add compound annual growth rate (CAGR) with explicit value and period-domain validation.
- Add modified internal rate of return (MIRR) with separate financing and reinvestment rates.
- Add sample and population volatility, downside deviation, Sortino ratio, and maximum drawdown.
- Add compound-return and square-root-of-time volatility annualization helpers.
- Define the statistical conventions explicitly: volatility is sample-based by default, downside deviation includes all observations in its denominator, and maximum drawdown is returned as a non-negative loss fraction.

### Loan schedules

- Expose each amortization period as an immutable `Finrb::Amortization::Entry` containing its period, opening and closing balances, payment, interest, principal, additional principal, balloon settlement, and interest-only state.
- Preserve the existing cashflow convention: payments are negative, while balances, interest, principal repaid, and additional principal are non-negative.
- Add contractual balloon targets. Regular installments amortize toward the target and the final payment settles the residual, including cent-rounding reconciliation.
- Add leading interest-only periods, including zero-rate periods and combinations with balloon loans. Remaining principal amortizes over the rest of the term.
- Add upfront and financed origination fees with separate `principal`, `net_proceeds`, and `amount_financed` values. Financed fees enter the opening balance; upfront fees reduce borrower proceeds.
- Correct schedule period numbering across rate segments and reused payment templates.

### API and documentation

- Add RBS declarations and API examples for all new return metrics and amortization features.
- Refine README compatibility badges to identify tested MRI versions, x86-64/ARM64 architectures, and experimental JRuby/TruffleRuby coverage.
- Remove redundant YARD `@api` annotations and make the internal amortization calculation methods genuinely private.

## 1.0.1

### Runtime compatibility

- Test Ruby 3.3, 3.4, and 4.0 across x86-64, native ARM64, and Docker environments.
- Add experimental JRuby and TruffleRuby compatibility suites and isolated development/test images.
- Add Docker Buildx tasks for developing and running the ARM64 image on x86-64 hosts.
- Add version-selectable, failure-aware Docker build, test, and run tasks.

### Packaging and assurance

- Verify gem metadata, packaged licenses, attribution, RBS declarations, and runtime dependencies.
- Install the built gem into an isolated gem home and smoke-test version loading, NPV, IRR, and opt-in core extensions.
- Add dependency review, ruby-advisory-db auditing, grouped Dependabot updates, workflow timeouts, and concurrency controls.
- Simplify CodeQL analysis and add reusable package and security verification tasks.
- Declare `ostruct` as a runtime dependency because it is required by `flt` and is no longer bundled with Ruby 4.

## 1.0.0

This release intentionally breaks parts of the 0.1 public API.

### Financial correctness

- Normalize XNPV date calculations and distinguish nominal APR from effective APY.
- Replace deprecated BigDecimal Newton solving with a decimal Brent–Dekker solver and explicit convergence/domain errors.
- Validate public financial inputs and centralize amortization rounding policy.
- Fix FIFO/LIFO ending inventory when a sale is satisfied before every layer is traversed.

### Public API

- Replace `Finrb::Utils` with `Finrb::TVM`, `Accounting`, `Ratios`, `Returns`, and `Yields`.
- Expose IRR, NPV, XIRR, and XNPV through `Finrb::Cashflow`.
- Stop modifying `Array` and `Numeric` by default; legacy fluent methods require `finrb/core_ext`.
- Publish immutable configuration snapshots with thread-scoped temporary overrides.

### Dependencies and assurance

- Remove ActiveSupport and `business_time` production dependencies.
- Add deterministic generated-root tests, SciPy/QuantLib reference verification, line and branch coverage, and packaged RBS signatures.
- Support Ruby 3.3, 3.4, and 4.0.

## 0.1.12

- bump gem versions
- rm ruby 3.2 support, set 3.3 as minimum

## 0.1.11

- fix activesupport configurable deprecation (thanks to @schinery)
- bump gem versions

## 0.1.10

- bump gem versions
- sanitize Float/BigDecimal return from Utils model, assert Flt::DecNum return type across.

## 0.1.9

- bump gem versions
- fix BigDecimal 3.2 compatibility

## 0.1.8

- bump gem versions

## 0.1.7

- rm ruby 3.1 support, set 3.2 as minimum
- adds ruby 3.4 to ci
- bump gem versions

## 0.1.6

- bump gem versions

## 0.1.4

- transactions rails fix (thanks to @MattHall)
- rm ruby 3.0 support

## 0.1.2

- refactoring

## 0.1.1

- documentation additions and fixes
- refactoring
- bugfixes and test suite migration to rspec

## 0.1.0

- sets ruby dependency to >= 3
- update/fix dependencies
- adds docker files for develop/test
- fix license files
- port [FinCal](https://github.com/felixfan/FinCal) library to ruby.
- readme update, api reference file outside main readme.

# Finance gem changelog

## Version 2.0.2

19 March 2019

- Fix BigDecimal deprecation warning
- Support Ruby 2.6.2
- Update dependencies

## Version 2.0.1

17 October 2017

- Added Support for configuration file to set up default eps and guess for IRR & XIRR
- Added guess rate for IRR & XIRR
- NVP now does not change the given cashflow array

## Version 2.0.0

23 Jul 2013

- Removed Integer#months, Integer#years, and replaced Numeric#to_d by Numeric#to_s in the interest of Rails compatibility.
- Converted unit tests from the shoulda framework to minitest.
- Removed octal numbers in test_cashflow.rb
- Thanks to @thadd, @bramswenson, and @xpe for their contributions to this release!

## Version 1.1.2

16 Jun 2012

- Bugfix: Array#irr and Array#xirr check for a valid sequence of cash flows.
- Bugfix: Integer#months and Integer#years no longer collide with Rails methods.

## Version 1.1.0

11 Sep 2011

- Added XNPV and XIRR functions, with basic testing.
- Bugfix: Array#sum no longer collides with the Array#sum defined in Rails.
- Bugfix: Numeric#amortize now correctly calls Finrb::Amortization#new.

## Version 1.0.0

20 Jul 2011

- Moved to Ruby 1.9.
- All classes are now contained within the +Finrb+ namespace.
- LOTS of additional documentation and examples.
- Introduced _shoulda_ for unit tests, to make things a little more readable.
- Bugfix: The +amortize+ Numeric method now accepts a variable number of rates.
- Some code refactoring and clean-up for a small performance increase.

## Version 0.2.0

28 Jun 2011

- Added support for adjustable rate mortgages.
- Added support for additional payments.

## Version 0.1.1

21 Jun 2011

- Code examples in README now display correctly in the online documentation.

## Version 0.1.0

21 Jun 2011

- Support for fixed-rate mortgage amortization.
- NPV, IRR array methods for cash flow analysis.
