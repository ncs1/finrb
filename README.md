# finrb

Ruby gem for financial calculations/modeling.

finrb forked from the ruby [finance](https://github.com/Edward-Intelligence/finance) gem.

## OVERVIEW

### GETTING STARTED

    >> require 'finrb'

*Note:* As of version 1.0.0, the entire library is contained under the
Finrb namespace.  Existing code will not work unless you add:

    >> include Finrb

for all of the examples below, we'll assume that you have done this.

### CONFIGURATION

In `config/initializers/finrb.rb` Finrb allows to set tolerance (eps) and default guess for IRR and XIRR calculations, such as:

```ruby
Finrb.configure do |config|
  config.eps = '1.0e-12'
  config.guess = 0.5
  config.business_days = false # only relevant when using Transaction object, skips weekends
  config.periodic_compound = false # only relevant when using Transaction object
end
```

### AMORTIZATION

You are interested in borrowing $250,000 under a 30 year, fixed-rate
loan with a 4.25% APR.

    >> rate = Rate.new(0.0425, :apr, :duration => (30 * 12))
    >> amortization = Amortization.new(250000, rate)

Find the standard monthly payment:

    >> amortization.payment
    => DecNum('-1229.91')

Find the total cost of the loan:

    >> amortization.payments.sum
    => DecNum('-442766.55')

How much will you pay in interest?

    >> amortization.interest.sum
    => DecNum('192766.55')

How much interest in the first six months?

    >> amortization.interest[0,6].sum
    => DecNum('5294.62')

If your loan has an adjustable rate, no problem.  You can pass an
arbitrary number of rates, and they will be used in the amortization.
For example, we can look at an amortization of $250000, where the APR
starts at 4.25%, and increases by 1% every five years.

    >> values = %w{ 0.0425 0.0525 0.0625 0.0725 0.0825 0.0925 }
    >> rates = values.collect { |value| Rate.new( value, :apr, :duration => (5  * 12) }
    >> arm = Amortization.new(250000, *rates)

Since we are looking at an ARM, there is no longer a single "payment" value.

    >> arm.payment
    => nil

But we can look at the different payments over time.

    >> arm.payments.uniq
    => [DecNum('-1229.85'), DecNum('-1360.41'), DecNum('-1475.65'), DecNum('-1571.07'), ... snipped ... ]

The other methods previously discussed can be accessed in the same way:

    >> arm.interest.sum
    => DecNum('287515.45')
    >> arm.payments.sum
    => DecNum('-537515.45')

Last, but not least, you may pass a block when creating an Amortization
which returns a modified monthly payment.  For example, to increase your
payment by $150, do:

    >> rate = Rate.new(0.0425, :apr, :duration => (30 * 12))
    >> extra_payments = 250000.amortize(rate){ |period| period.payment - 150 }

Disregarding the block, we have used the same parameters as the first
example.  Notice the difference in the results:

    >> amortization.payments.sum
    => DecNum('-442745.98')
    >> extra_payments.payments.sum
    => DecNum('-400566.24')
    >> amortization.interest.sum
    => DecNum('192745.98')
    >> extra_payments.interest.sum
    => DecNum('150566.24')

You can also increase your payment to a specific amount:

    >> extra_payments_2 = 250000.amortize(rate){ -1500 }

### IRR and XIRR

```ruby
guess = 0.1
transactions = []
transactions << Transaction.new(-10000, date: '2010-01-01'.to_time(:utc))
transactions << Transaction.new(123000, date: '2012-01-01'.to_time(:utc))
transactions.xirr(guess)
#  => Rate.new(2.507136, :apr)
```

## FEATURES

Currently implemented features include:

* Uses the [flt](http://flt.rubyforge.org/) library to ensure precision decimal arithmetic in all calculations.
* Fixed-rate mortgage amortization (30/360).
* Interest rates
* Various cash flow computations, such as NPV and IRR.
* Adjustable rate mortgage amortization.
* Payment modifications (i.e., how does paying an additional $75 per month affect the amortization?)

## RESOURCES

* [RubyGems Page](https://rubygems.org/gems/finrb)
* [Source Code](http://github.com/ncs1/finrb)
* [Bug Tracker](https://github.com/ncs1/finrb/issues)

## COPYRIGHT

See [COPYING](./COPYING) and [COPYING.LESSER](./COPYING.LESSER)
