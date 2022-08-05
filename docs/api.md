# finrb - API and examples

## Amortization

You are interested in borrowing $250,000 under a 30 year, fixed-rate
loan with a 4.25% APR.

```ruby
rate = Finrb::Rate.new(0.0425, :apr, :duration => (30 * 12))
amortization = Finrb::Amortization.new(250000, rate)
```

Find the standard monthly payment:

```ruby
amortization.payment
=> Flt::DecNum('-1229.91')
```

Find the total cost of the loan:

```ruby
amortization.payments.sum
=> Flt::DecNum('-442766.55')
```

How much will you pay in interest?

```ruby
amortization.interest.sum
=> Flt::DecNum('192766.55')
```

How much interest in the first six months?

```ruby
amortization.interest[0,6].sum
=> Flt::DecNum('5294.62')
```

If your loan has an adjustable rate, no problem.  You can pass an
arbitrary number of rates, and they will be used in the amortization.
For example, we can look at an amortization of $250000, where the APR
starts at 4.25%, and increases by 1% every five years.

```ruby
values = %w{ 0.0425 0.0525 0.0625 0.0725 0.0825 0.0925 }
rates = values.collect { |value| Finrb::Rate.new( value, :apr, :duration => (5  * 12) }
arm = Finrb::Amortization.new(250000, *rates)
```

Since we are looking at an ARM, there is no longer a single "payment" value.

```ruby
arm.payment
=> nil
```

But we can look at the different payments over time.

```ruby
arm.payments.uniq
=> [Flt::DecNum('-1229.85'), Flt::DecNum('-1360.41'), Flt::DecNum('-1475.65'), Flt::DecNum('-1571.07'), ... snipped ... ]
```

The other methods previously discussed can be accessed in the same way:

```ruby
arm.interest.sum
=> Flt::DecNum('287515.45')
arm.payments.sum
=> Flt::DecNum('-537515.45')
```

Last, but not least, you may pass a block when creating an Amortization
which returns a modified monthly payment.  For example, to increase your
payment by $150, do:

```ruby
rate = Finrb::Rate.new(0.0425, :apr, :duration => (30 * 12))
extra_payments = 250000.amortize(rate){ |period| period.payment - 150 }
```

Disregarding the block, we have used the same parameters as the first
example.  Notice the difference in the results:

```ruby
amortization.payments.sum
=> Flt::DecNum('-442745.98')
extra_payments.payments.sum
=> Flt::DecNum('-400566.24')
amortization.interest.sum
=> Flt::DecNum('192745.98')
extra_payments.interest.sum
=> Flt::DecNum('150566.24')
```

You can also increase your payment to a specific amount:

```ruby
extra_payments_2 = 250000.amortize(rate){ -1500 }
```

## IRR and XIRR

```ruby
guess = 0.1
transactions = []
transactions << Transaction.new(-10000, date: '2010-01-01'.to_time(:utc))
transactions << Transaction.new(123000, date: '2012-01-01'.to_time(:utc))
transactions.xirr(guess)
#  => Finrb::Rate.new(2.507136, :apr)
```
