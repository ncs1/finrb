# finrb - API and examples

## Utils

Utils is a static class providing basic financial functions for modeling.

Utils is based on R's [FinCal](https://github.com/felixfan/FinCal) library (ported to Ruby).

Provides the following functions:

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

### Computing bank discount yield (BDY) for a T-bill

- Param - d - the dollar discount, which is equal to the difference between the face value of the bill and the purchase price

- Param - f - the face value (par value) of the bill

- Param - t - number of days remaining until maturity

Examples:

```ruby
Finrb::Utils.bdy(d=1500,f=100000,t=120)
```

### Computing money market yield (MMY) for a T-bill

- Param - bdy - bank discount yield

- Param - t - number of days remaining until maturity

Examples:

```ruby
Finrb::Utils.bdy2mmy(bdy=0.045,t=120)
```

### Cash ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.

- Param - cash - cash

- Param - ms - marketable securities

- Param - cl - current liabilities

Examples:

```ruby
Finrb::Utils.cash_ratio(cash=3000,ms=2000,cl=2000)
```

### Computing Coefficient of variation

- Param - sd - standard deviation

- Param - avg - average value

Examples:

```ruby
Finrb::Utils.coefficient_variation(sd=0.15,avg=0.39)
```

### Cost of goods sold and ending inventory under three methods (FIFO,LIFO,Weighted average)

- Param - uinv - units of beginning inventory

- Param - pinv - prince of beginning inventory

- Param - units - nx1 vector of inventory units. inventory purchased ordered by time (from first to last)

- Param - price - nx1 vector of inventory price. same order as units

- Param - sinv - units of sold inventory

- Param - method - inventory methods: FIFO (first in first out, permitted under both US and IFRS), LIFO (late in first out, US only), WAC (weighted average cost,US and IFRS)

Examples:

```ruby
Finrb::Utils.cogs(uinv=2,pinv=2,units=[3,5],price=[3,5],sinv=7,method="FIFO")
```

```ruby
Finrb::Utils.cogs(uinv=2,pinv=2,units=[3,5],price=[3,5],sinv=7,method="LIFO")
```

```ruby
Finrb::Utils.cogs(uinv=2,pinv=2,units=[3,5],price=[3,5],sinv=7,method="WAC")
```

### Current ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.

- Param - ca - current assets

- Param - cl - current liabilities

Examples:

```ruby
Finrb::Utils.current_ratio(ca=8000,cl=2000)
```

### Depreciation Expense Recognition - double-declining balance (DDB), the most common declining balance method, which applies two times the straight-line rate to the declining balance.

- Param - cost - cost of long-lived assets

- Param - rv - residual value of the long-lived assets at the end of its useful life. DDB does not explicitly use the asset's residual value in the calculations, but depreciation ends once the estimated residual value has been reached. If the asset is expected to have no residual value, the DB method will never fully depreciate it, so the DB method is typically changed to straight-line at some point in the asset's life.

- Param - t - length of the useful life

Examples:

```ruby
Finrb::Utils.ddb(cost=1200,rv=200,t=5)
```

### Debt ratio - Solvency ratios measure the firm's ability to satisfy its long-term obligations.

- Param - td - total debt

- Param - ta - total assets

Examples:

```ruby
Finrb::Utils.debt_ratio(td=6000,ta=20000)
```

### Diluted Earnings Per Share

- Param - ni - net income

- Param - pd - preferred dividends

- Param - cpd - dividends on convertible preferred stock

- Param - cdi - interest on convertible debt

- Param - tax - tax rate

- Param - w - weighted average number of common shares outstanding

- Param - cps - shares from conversion of convertible preferred stock

- Param - cds - shares from conversion of convertible debt

- Param - iss - shares issuable from stock options

Examples:

```ruby
Finrb::Utils.diluted_eps(ni=115600,pd=10000,cdi=42000,tax=0.4,w=200000,cds=60000)
```

```ruby
Finrb::Utils.diluted_eps(ni=115600,pd=10000,cpd=10000,w=200000,cps=40000)
```

```ruby
Finrb::Utils.diluted_eps(ni=115600,pd=10000,w=200000,iss=2500)
```

```ruby
Finrb::Utils.diluted_eps(ni=115600,pd=10000,cpd=10000,cdi=42000,tax=0.4,w=200000,cps=40000,cds=60000,iss=2500)
```

### Computing the rate of return for each period

- Param - n - number of periods

- Param - pv - present value

- Param - fv - future value

- Param - pmt - payment per period

- Param - type - payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)

- Param - lower - the lower end points of the rate of return to be searched.

- Param - upper - the upper end points of the rate of return to be searched.
  @importFrom stats uniroot

Examples:

```ruby
Finrb::Utils.discount_rate(n=5,pv=0,fv=600,pmt=-100,type=0)
```

### Convert stated annual rate to the effective annual rate

- Param - r - stated annual rate

- Param - m - number of compounding periods per year

Examples:

```ruby
Finrb::Utils.ear(r=0.12,m=12)
```

```ruby
Finrb::Utils.ear(0.04,365)
```

### Convert stated annual rate to the effective annual rate with continuous compounding

- Param - r - stated annual rate

Examples:

```ruby
Finrb::Utils.ear_continuous(r=0.1)
```

```ruby
Finrb::Utils.ear_continuous(0.03)
```

### Bond-equivalent yield (BEY), 2 x the semiannual discount rate

- Param - ear - effective annual rate

Examples:

```ruby
Finrb::Utils.ear2bey(ear=0.08)
```

### Computing HPR, the holding period return

- Param - ear - effective annual rate

- Param - t - number of days remaining until maturity

Examples:

```ruby
Finrb::Utils.ear2hpr(ear=0.05039,t=150)
```

### Equivalent/proportional Interest Rates

@description An interest rate to be applied n times p.a. can be converted to an equivalent rate to be applied p times p.a.

- Param - r - interest rate to be applied n times per year (r is annual rate!)

- Param - n - times that the interest rate r were compounded per year

- Param - p - times that the equivalent rate were compounded per year

- Param - type - equivalent interest rates ('e',default) or proportional interest rates ('p')

Examples:

- monthly interest rat equivalent to 5% compounded per year

```ruby
Finrb::Utils.eir(r=0.05,n=1,p=12)
```

- monthly interest rat equivalent to 5% compounded per half year

```ruby
Finrb::Utils.eir(r=0.05,n=2,p=12)
```

- monthly interest rat equivalent to 5% compounded per quarter

```ruby
Finrb::Utils.eir(r=0.05,n=4,p=12)
```

- annual interest rate equivalent to 5% compounded per month

```ruby
Finrb::Utils.eir(r=0.05,n=12,p=1)
```

- this is equivalent to
  ear(r=0.05,m=12)

- quarter interest rate equivalent to 5% compounded per year

```ruby
Finrb::Utils.eir(r=0.05,n=1,p=4)
```

- quarter interest rate equivalent to 5% compounded per month

```ruby
Finrb::Utils.eir(r=0.05,n=12,p=4)
```

- monthly proportional interest rate which is equivalent to a simple annual interest

```ruby
Finrb::Utils.eir(r=0.05,p=12,type='p')
```

### Basic Earnings Per Share

- Param - ni - net income

- Param - pd - preferred dividends

- Param - w - weighted average number of common shares outstanding

Examples:

```ruby
Finrb::Utils.eps(ni=10000,pd=1000,w=11000)
```

### Financial leverage - Solvency ratios measure the firm's ability to satisfy its long-term obligations.

- Param - te - total equity

- Param - ta - total assets

Examples:

```ruby
Finrb::Utils.financial_leverage(te=16000,ta=20000)
```

### Estimate future value (fv)

- Param - r - discount rate, or the interest rate at which the amount will be compounded each period

- Param - n - number of periods

- Param - pv - present value

- Param - pmt - payment per period

- Param - type - payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)

Examples:

```ruby
Finrb::Utils.fv(r=0.07,n=10,pv=1000,pmt=10)
```

### Estimate future value of an annuity

- Param - r - discount rate, or the interest rate at which the amount will be compounded each period

- Param - n - number of periods

- Param - pmt - payment per period

- Param - type - payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)

Examples:

```ruby
Finrb::Utils.fv_annuity(0.03,12,-1000)
```

```ruby
Finrb::Utils.fv_annuity(r=0.03,n=12,pmt=-1000,type=1)
```

### Estimate future value (fv) of a single sum

- Param - r - discount rate, or the interest rate at which the amount will be compounded each period

- Param - n - number of periods

- Param - pv - present value

Examples:

```ruby
Finrb::Utils.fv_simple(0.08,10,-300)
```

```ruby
Finrb::Utils.fv_simple(r=0.04,n=20,pv=-50000)
```

### Computing the future value of an uneven cash flow series

- Param - r - stated annual rate

- Param - cf - uneven cash flow

Examples:

```ruby
Finrb::Utils.fv_uneven(r=0.1, cf=[-1000, -500, 0, 4000, 3500, 2000])
```

### Geometric mean return

- Param - r - returns over multiple periods

Examples:

```ruby
Finrb::Utils.geometric_mean(r=[-0.0934, 0.2345, 0.0892])
```

### Gross profit margin - Evaluate a company's financial performance

- Param - gp - gross profit, equal to revenue minus cost of goods sold (cogs)

- Param - rv - revenue (sales)

Examples:

```ruby
Finrb::Utils.gpm(gp=1000,rv=20000)
```

### Harmonic mean, average price

- Param - p - price over multiple periods

Examples:

```ruby
Finrb::Utils.harmonic_mean(p=[8,9,10])
```

### Computing HPR, the holding period return

- Param - ev - ending value

- Param - bv - beginning value

- Param - cfr - cash flow received

Examples:

```ruby
Finrb::Utils.hpr(ev=33,bv=30,cfr=0.5)
```

### Bond-equivalent yield (BEY), 2 x the semiannual discount rate

- Param - hpr - holding period return

- Param - t - number of month remaining until maturity

Examples:

```ruby
Finrb::Utils.hpr2bey(hpr=0.02,t=3)
```

### Convert holding period return to the effective annual rate

- Param - hpr - holding period return

- Param - t - number of days remaining until maturity

Examples:

```ruby
Finrb::Utils.hpr2ear(hpr=0.015228,t=120)
```

### Computing money market yield (MMY) for a T-bill

- Param - hpr - holding period return

- Param - t - number of days remaining until maturity

Examples:

```ruby
Finrb::Utils.hpr2mmy(hpr=0.01523,t=120)
```

### Computing IRR, the internal rate of return

- Param - cf - cash flow,the first cash flow is the initial outlay
  @importFrom stats uniroot

Examples:

```ruby
Finrb::Utils.irr(cf=[-5, 1.6, 2.4, 2.8])
```

### Computing IRR, the internal rate of return

@description This function is the same as irr but can calculate negative value. This function may take a very long time. You can use larger cutoff and larger step to get a less precision irr first. Then based on the result, change from and to, to narrow down the interval, and use a smaller step to get a more precision irr.

- Param - cf - cash flow,the first cash flow is the initial outlay

- Param - cutoff - threshold to take npv as zero

- Param - from - smallest irr to try

- Param - to - largest irr to try

- Param - step - increment of the irr

Examples:

```ruby
Finrb::Utils.irr2(cf=[-5, 1.6, 2.4, 2.8])
```

```ruby
Finrb::Utils.irr2(cf=[-200, 50, 60, -70, 30, 20])
```

### Calculate the net increase in common shares from the potential exercise of stock options or warrants

- Param - amp - average market price over the year

- Param - ep - exercise price of the options or warrants

- Param - n - number of common shares that the options and warrants can be convened into

Examples:

```ruby
Finrb::Utils.iss(amp=20,ep=15,n=10000)
```

### Long-term debt-to-equity - Solvency ratios measure the firm's ability to satisfy its long-term obligations.

- Param - ltd - long-term debt

- Param - te - total equity

Examples:

```ruby
Finrb::Utils.lt_d2e(ltd=8000,te=20000)
```

### Computing HPR, the holding period return

- Param - mmy - money market yield

- Param - t - number of days remaining until maturity

Examples:

```ruby
Finrb::Utils.mmy2hpr(mmy=0.04898,t=150)
```

### Estimate the number of periods

- Param - r - discount rate, or the interest rate at which the amount will be compounded each period

- Param - pv - present value

- Param - fv - future value

- Param - pmt - payment per period

- Param - type - payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)

Examples:

```ruby
Finrb::Utils.n_period(0.1,-10000,60000000,-50000,0)
```

```ruby
Finrb::Utils.n_period(r=0.1,pv=-10000,fv=60000000,pmt=-50000,type=1)
```

### Net profit margin - Evaluate a company's financial performance

- Param - ni - net income

- Param - rv - revenue (sales)

Examples:

```ruby
Finrb::Utils.npm(ni=8000,rv=20000)
```

### Computing NPV, the PV of the cash flows less the initial (time = 0) outlay

- Param - r - discount rate, or the interest rate at which the amount will be compounded each period

- Param - cf - cash flow,the first cash flow is the initial outlay

Examples:

```ruby
Finrb::Utils.npv(r=0.12, cf=[-5, 1.6, 2.4, 2.8])
```

### Estimate period payment

- Param - r - discount rate, or the interest rate at which the amount will be compounded each period

- Param - n - number of periods

- Param - pv - present value

- Param - fv - future value

- Param - type - payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)

Examples:

```ruby
Finrb::Utils.pmt(0.08,10,-1000,10)
```

```ruby
Finrb::Utils.pmt(r=0.08,n=10,pv=-1000,fv=0)
```

```ruby
Finrb::Utils.pmt(0.08,10,-1000,10,1)
```

### Estimate present value (pv)

- Param - r - discount rate, or the interest rate at which the amount will be compounded each period

- Param - n - number of periods

- Param - fv - future value

- Param - pmt - payment per period

- Param - type - payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)

Examples:

```ruby
Finrb::Utils.pv(0.07,10,1000,10)
```

```ruby
Finrb::Utils.pv(r=0.05,n=20,fv=1000,pmt=10,type=1)
```

### Estimate present value (pv) of an annuity

- Param - r - discount rate, or the interest rate at which the amount will be compounded each period

- Param - n - number of periods

- Param - pmt - payment per period

- Param - type - payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)

Examples:

```ruby
Finrb::Utils.pv_annuity(0.03,12,1000)
```

```ruby
Finrb::Utils.pv_annuity(r=0.0425,n=3,pmt=30000)
```

### Estimate present value of a perpetuity

- Param - r - discount rate, or the interest rate at which the amount will be compounded each period

- Param - g - growth rate of perpetuity

- Param - pmt - payment per period

- Param - type - payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)

Examples:

```ruby
Finrb::Utils.pv_perpetuity(r=0.1,pmt=1000,g=0.02)
```

```ruby
Finrb::Utils.pv_perpetuity(r=0.1,pmt=1000,type=1)
```

```ruby
Finrb::Utils.pv_perpetuity(r=0.1,pmt=1000)
```

### Estimate present value (pv) of a single sum

- Param - r - discount rate, or the interest rate at which the amount will be compounded each period

- Param - n - number of periods

- Param - fv - future value

Examples:

```ruby
Finrb::Utils.pv_simple(0.07,10,100)
```

```ruby
Finrb::Utils.pv_simple(r=0.03,n=3,fv=1000)
```

### Computing the present value of an uneven cash flow series

- Param - r - discount rate, or the interest rate at which the amount will be compounded each period

- Param - cf - uneven cash flow

Examples:

```ruby
Finrb::Utils.pv_uneven(r=0.1, cf=[-1000, -500, 0, 4000, 3500, 2000])
```

### Quick ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.

- Param - cash - cash

- Param - ms - marketable securities

- Param - rc - receivables

- Param - cl - current liabilities

Examples:

```ruby
Finrb::Utils.quick_ratio(cash=3000,ms=2000,rc=1000,cl=2000)
```

### Convert a given norminal rate to a continuous compounded rate

- Param - r - norminal rate

- Param - m - number of times compounded each year

Examples:

```ruby
Finrb::Utils.r_continuous(r=0.03,m=4)
```

### Convert a given continuous compounded rate to a norminal rate

- Param - rc - continuous compounded rate

- Param - m - number of desired times compounded each year

Examples:

```ruby
Finrb::Utils.r_norminal(0.03,1)
```

```ruby
Finrb::Utils.r_norminal(rc=0.03,m=4)
```

### Rate of return for a perpetuity

- Param - pmt - payment per period

- Param - pv - present value

Examples:

```ruby
Finrb::Utils.r_perpetuity(pmt=4.5,pv=-75)
```

### Computing Sampling error

- Param - sm - sample mean

- Param - mu - population mean

Examples:

```ruby
Finrb::Utils.sampling_error(sm=0.45, mu=0.5)
```

### Computing Roy's safety-first ratio

- Param - rp - portfolio return

- Param - rl - threshold level return

- Param - sd - standard deviation of portfolio retwns

Examples:

```ruby
Finrb::Utils.sf_ratio(rp=0.09,rl=0.03,sd=0.12)
```

### Computing Sharpe Ratio

- Param - rp - portfolio return

- Param - rf - risk-free return

- Param - sd - standard deviation of portfolio retwns

Examples:

```ruby
Finrb::Utils.sharpe_ratio(rp=0.038,rf=0.015,sd=0.07)
```

### Depreciation Expense Recognition - Straight-line depreciation (SL) allocates an equal amount of depreciation each year over the asset's useful life

- Param - cost - cost of long-lived assets

- Param - rv - residual value of the long-lived assets at the end of its useful life

- Param - t - length of the useful life

Examples:

```ruby
Finrb::Utils.slde(cost=1200,rv=200,t=5)
```

### Total debt-to-equity - Solvency ratios measure the firm's ability to satisfy its long-term obligations.

- Param - td - total debt

- Param - te - total equity

Examples:

```ruby
Finrb::Utils.total_d2e(td=6000,te=20000)
```

### Computing TWRR, the time-weighted rate of return

- Param - ev - ordered ending value list

- Param - bv - ordered beginning value list

- Param - cfr - ordered cash flow received list

Examples:

```ruby
Finrb::Utils.twrr(ev=[120,260],bv=[100,240],cfr=[2,4])
```

### Calculate weighted average shares - weighted average number of common shares

- Param - ns - n x 1 vector vector of number of shares

- Param - nm - n x 1 vector vector of number of months relate to ns

Examples:

s=[10000,2000];m=[12,6];

```ruby
Finrb::Utils.was(ns=s,nm=m)
```

s=[11000,4400,-3000];m=[12,9,4];

```ruby
Finrb::Utils.was(ns=s,nm=m)
```

### Weighted mean as a portfolio return

- Param - r - returns of the individual assets in the portfolio

- Param - w - corresponding weights associated with each of the individual assets

Examples:

```ruby
Finrb::Utils.wpr(r=[0.12, 0.07, 0.03],w=[0.5,0.4,0.1])
```

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

If your loan has an adjustable rate, no problem. You can pass an
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
which returns a modified monthly payment. For example, to increase your
payment by $150, do:

```ruby
rate = Finrb::Rate.new(0.0425, :apr, :duration => (30 * 12))
extra_payments = 250000.amortize(rate){ |period| period.payment - 150 }
```

Disregarding the block, we have used the same parameters as the first
example. Notice the difference in the results:

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
