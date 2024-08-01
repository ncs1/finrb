# finrb changelog

## 0.1.6

- bump gem versions

## 0.1.4

- transactions rails fix
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
