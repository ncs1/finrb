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
