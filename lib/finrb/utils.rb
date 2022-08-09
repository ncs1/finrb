# frozen_string_literal: true

require 'active_support/core_ext/array/wrap'
require_relative 'decimal'
require 'bigdecimal'
require 'bigdecimal/newton'

include Newton

module Finrb
  class Utils
    class NlFunctionStub
      attr_accessor :func

      values = { eps: Finrb.config.eps, one: '1.0', two: '2.0', ten: '10.0', zero: '0.0' }

      values.each do |key, value|
        define_method key do
          BigDecimal(value)
        end
      end

      def values(x)
        @func.call(x)
      end
    end

    # Computing bank discount yield (BDY) for a T-bill
    #
    # @param d the dollar discount, which is equal to the difference between the face value of the bill and the purchase price
    # @param f the face value (par value) of the bill
    # @param t number of days remaining until maturity
    # @export
    # @examples
    # bdy(d=1500,f=100000,t=120)
    def self.bdy(d, f, t)
      d = d.to_d
      f = f.to_d
      t = t.to_d

      (360 * d / f / t)
    end

    # Computing money market yield (MMY) for a T-bill
    #
    # @param bdy bank discount yield
    # @param t number of days remaining until maturity
    # @export
    # @examples
    # bdy2mmy(bdy=0.045,t=120)
    def self.bdy2mmy(bdy, t)
      bdy = bdy.to_d
      t = t.to_d

      (360 * bdy / (360 - (t * bdy)))
    end

    # cash ratio -- Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
    #
    # @param cash cash
    # @param ms   marketable securities
    # @param cl   current liabilities
    # @export
    # @examples
    # Finrb::Utils.cash_ratio(cash=3000,ms=2000,cl=2000)
    def self.cash_ratio(cash, ms, cl)
      cash = cash.to_d
      ms = ms.to_d
      cl = cl.to_d

      ((cash + ms) / cl)
    end

    # Computing Coefficient of variation
    #
    # @param sd standard deviation
    # @param avg average value
    # @export
    # @examples
    # Finrb::Utils.coefficient_variation(sd=0.15,avg=0.39)
    def self.coefficient_variation(sd, avg)
      sd = sd.to_d
      avg = avg.to_d

      (sd / avg)
    end

    # Cost of goods sold and ending inventory under three methods (FIFO,LIFO,Weighted average)
    #
    # @param uinv units of beginning inventory
    # @param pinv prince of beginning inventory
    # @param units nx1 vector of inventory units. inventory purchased ordered by time (from first to last)
    # @param price nx1 vector of inventory price. same order as units
    # @param sinv units of sold inventory
    # @param method inventory methods: FIFO (first in first out, permitted under both US and IFRS), LIFO (late in first out, US only), WAC (weighted average cost,US and IFRS)
    # @export
    # @examples
    # cogs(uinv=2,pinv=2,units=c(3,5),price=c(3,5),sinv=7,method="FIFO")
    #
    # cogs(uinv=2,pinv=2,units=c(3,5),price=c(3,5),sinv=7,method="LIFO")
    #
    # cogs(uinv=2,pinv=2,units=c(3,5),price=c(3,5),sinv=7,method="WAC")
    def self.cogs(uinv, pinv, units, price, sinv, method = 'FIFO')
      uinv = uinv.to_d
      pinv = pinv.to_d
      units = Array.wrap(units).map(&:to_d)
      price = Array.wrap(price).map(&:to_d)
      sinv = sinv.to_d
      method = method.to_s

      n = units.size
      m = price.size
      costOfGoods = 0
      endingInventory = 0
      if m == n
        case method
        when 'FIFO'
          if sinv <= uinv
            costOfGoods = sinv * pinv
            endingInventory = (uinv - sinv) * pinv
            (0...n).each do |i|
              endingInventory += (units[i] * price[i])
            end
          else
            costOfGoods = uinv * pinv
            sinv -= uinv
            (0...n).each do |i|
              if sinv <= units[i]
                costOfGoods += (sinv * price[i])
                endingInventory = (units[i] - sinv) * price[i]
                if i < n
                  temp = i + 1
                  (temp...n).each do |j|
                    endingInventory += (units[j] * price[j])
                  end
                end
                sinv = 0
                next
              else
                costOfGoods += (units[i] * price[i])
                sinv -= units[i]
              end
            end
            raise(FinrbError, "Inventory is not enough to sell\n") if sinv.positive?
          end
        when 'WAC'
          endingInventory = uinv * pinv
          tu = uinv
          (0...n).each do |i|
            endingInventory += (units[i] * price[i])
            tu += units[i]
          end
          if tu >= sinv
            costOfGoods = endingInventory / tu * sinv
            endingInventory = endingInventory / tu * (tu - sinv)
          else
            raise(FinrbError, "Inventory is not enough to sell\n")
          end

        when 'LIFO'
          (n - 1).downto(0).each do |i|
            if sinv <= units[i]
              costOfGoods += (sinv * price[i])
              endingInventory = (units[i] - sinv) * price[i]
              if i > 1
                temp = i - 1
                temp.downto(0).each do |j|
                  endingInventory += (units[j] * price[j])
                end
              end
              endingInventory += (uinv * pinv)
              sinv = 0
              next
            else
              costOfGoods += (units[i] * price[i])
              sinv -= units[i]
            end
          end
          if sinv.positive?
            if sinv <= uinv
              costOfGoods += (sinv * pinv)
              endingInventory += ((uinv - sinv) * pinv)
            else
              raise(FinrbError, "Inventory is not enough to sell\n")
            end
          end
        end

      else
        raise(FinrbError, "length of units and price are not the same\n")
      end

      [costOfGoods, endingInventory]
    end

    # current ratio -- Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
    #
    # @param ca current assets
    # @param cl current liabilities
    # @export
    # @examples
    # Finrb::Utils.current_ratio(ca=8000,cl=2000)
    def self.current_ratio(ca, cl)
      ca = ca.to_d
      cl = cl.to_d

      (ca / cl)
    end

    # Depreciation Expense Recognition -- double-declining balance (DDB), the most common declining balance method, which applies two times the straight-line rate to the declining balance.
    #
    # @param cost cost of long-lived assets
    # @param rv   residual value of the long-lived assets at the end of its useful life. DDB does not explicitly use the asset's residual value in the calculations, but depreciation ends once the estimated residual value has been reached. If the asset is expected to have no residual value, the DB method will never fully depreciate it, so the DB method is typically changed to straight-line at some point in the asset's life.
    # @param t    length of the useful life
    # @export
    # @examples
    # ddb(cost=1200,rv=200,t=5)
    def self.ddb(cost, rv, t)
      cost = cost.to_d
      rv = rv.to_d
      t = t.to_d

      raise(FinrbError, 't should be larger than 1') if t < 2

      ddb = [0] * t
      ddb[0] = 2 * cost / t
      if cost - ddb[0] <= rv
        ddb[0] = cost - rv
      else
        cost -= ddb[0]
        (1...t).each do |i|
          ddb[i] = 2 * cost / t
          if cost - ddb[i] <= rv
            ddb[i] = cost - rv
            break
          else
            cost -= ddb[i]
          end
        end
      end
      { t: (0...t).to_a, ddb: }
    end

    # debt ratio -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param td total debt
    # @param ta total assets
    # @export
    # @examples
    # Finrb::Utils.debt_ratio(td=6000,ta=20000)
    def self.debt_ratio(td, ta)
      td = td.to_d
      ta = ta.to_d

      (td / ta)
    end

    # diluted Earnings Per Share
    #
    # @param ni     net income
    # @param pd     preferred dividends
    # @param cpd    dividends on convertible preferred stock
    # @param cdi    interest on convertible debt
    # @param tax    tax rate
    # @param w      weighted average number of common shares outstanding
    # @param cps    shares from conversion of convertible preferred stock
    # @param cds    shares from conversion of convertible debt
    # @param iss    shares issuable from stock options
    # @export
    # @examples
    # Finrb::Utils.diluted_eps(ni=115600,pd=10000,cdi=42000,tax=0.4,w=200000,cds=60000)
    #
    # Finrb::Utils.diluted_eps(ni=115600,pd=10000,cpd=10000,w=200000,cps=40000)
    #
    # Finrb::Utils.diluted_eps(ni=115600,pd=10000,w=200000,iss=2500)
    #
    # Finrb::Utils.diluted_eps(ni=115600,pd=10000,cpd=10000,cdi=42000,tax=0.4,w=200000,cps=40000,cds=60000,iss=2500)
    def self.diluted_eps(ni, pd, w, cpd = 0, cdi = 0, tax = 0, cps = 0, cds = 0, iss = 0)
      ni = ni.to_d
      pd = pd.to_d
      w = w.to_d
      cpd = cpd.to_d
      cdi = cdi.to_d
      tax = tax.to_d
      cps = cps.to_d
      cds = cds.to_d
      iss = iss.to_d

      basic = (ni - pd) / w
      diluted = (ni - pd + cpd + (cdi * (1 - tax))) / (w + cps + cds + iss)
      diluted = (ni - pd + cpd) / (w + cps + iss) if diluted > basic
      diluted
    end

    # Computing the rate of return for each period
    #
    # @param n number of periods
    # @param pv present value
    # @param fv future value
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @param lower the lower end points of the rate of return to be searched.
    # @param upper the upper end points of the rate of return to be searched.
    # @importFrom stats uniroot
    # @export
    # @examples
    # Finrb::Utils.discount_rate(n=5,pv=0,fv=600,pmt=-100,type=0)
    def self.discount_rate(n, pv, fv, pmt, type = 0, lower = 0.0001, upper = 100)
      n = n.to_d
      pv = pv.to_d
      fv = fv.to_d
      pmt = pmt.to_d
      type = type.to_d
      lower = lower.to_d
      upper = upper.to_d

      nlfunc = NlFunctionStub.new
      nlfunc.func =
        lambda do |x|
          BigDecimal((Finrb::Utils.fv_simple(x[0], n, pv) + Finrb::Utils.fv_annuity(x[0], n, pmt, type) - fv).to_s)
        end

      root = [(upper - lower) / 2]
      nlsolve(nlfunc, root)
      root[0]
    end

    # Convert stated annual rate to the effective annual rate
    #
    # @param r stated annual rate
    # @param m number of compounding periods per year
    # @export
    # @examples
    # ear(r=0.12,m=12)
    #
    # ear(0.04,365)
    def self.ear(r, m)
      r = r.to_d
      m = m.to_d

      (((1 + (r / m))**m) - 1)
    end

    # Convert stated annual rate to the effective annual rate with continuous compounding
    #
    # @param r stated annual rate
    # @export
    # @examples
    # Finrb::Utils.ear_continuous(r=0.1)
    #
    # Finrb::Utils.ear_continuous(0.03)
    def self.ear_continuous(r)
      r = r.to_d

      (r.to_d.exp - 1)
    end

    # bond-equivalent yield (BEY), 2 x the semiannual discount rate
    #
    # @param ear effective annual rate
    # @export
    # @examples
    # ear2bey(ear=0.08)
    def self.ear2bey(ear)
      ear = ear.to_d

      ((((1 + ear)**0.5) - 1) * 2)
    end

    # Computing HPR, the holding period return
    #
    # @param ear effective annual rate
    # @param t number of days remaining until maturity
    # @export
    # @examples
    # ear2hpr(ear=0.05039,t=150)
    def self.ear2hpr(ear, t)
      ear = ear.to_d
      t = t.to_d

      (((1 + ear)**(t / 365)) - 1)
    end

    # Equivalent/proportional Interest Rates
    # @description An interest rate to be applied n times p.a. can be converted to an equivalent rate to be applied p times p.a.
    # @param r interest rate to be applied n times per year (r is annual rate!)
    # @param n times that the interest rate r were compounded per year
    # @param p times that the equivalent rate were compounded per year
    # @param type equivalent interest rates ('e',default) or proportional interest rates ('p')
    # @export
    # @examples
    # # monthly interest rat equivalent to 5% compounded per year
    # Finrb::Utils.eir(r=0.05,n=1,p=12)
    #
    # # monthly interest rat equivalent to 5% compounded per half year
    # Finrb::Utils.eir(r=0.05,n=2,p=12)
    #
    # # monthly interest rat equivalent to 5% compounded per quarter
    # Finrb::Utils.eir(r=0.05,n=4,p=12)
    #
    # # annual interest rate equivalent to 5% compounded per month
    # Finrb::Utils.eir(r=0.05,n=12,p=1)
    # # this is equivalent to
    # ear(r=0.05,m=12)
    #
    # # quarter interest rate equivalent to 5% compounded per year
    # Finrb::Utils.eir(r=0.05,n=1,p=4)
    #
    # # quarter interest rate equivalent to 5% compounded per month
    # Finrb::Utils.eir(r=0.05,n=12,p=4)
    #
    # # monthly proportional interest rate which is equivalent to a simple annual interest
    # Finrb::Utils.eir(r=0.05,p=12,type='p')
    def self.eir(r, n = 1, p = 12, type = 'e')
      r = r.to_d
      n = n.to_d
      p = p.to_d
      type = type.to_s

      case type
      when 'e'
        eir = ((1 + (r / n))**(n / p)) - 1
      when 'p'
        eir = r / p
      else
        raise(FinrbError, "type must be 'e' or 'p'")
      end
      eir
    end

    # Basic Earnings Per Share
    #
    # @param ni net income
    # @param pd preferred dividends
    # @param w  weighted average number of common shares outstanding
    # @export
    # @examples
    # Finrb::Utils.eps(ni=10000,pd=1000,w=11000)
    def self.eps(ni, pd, w)
      ni = ni.to_d
      pd = pd.to_d
      w = w.to_d

      ((ni - pd) / w)
    end

    # financial leverage -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param te total equity
    # @param ta total assets
    # @export
    # @examples
    # Finrb::Utils.financial_leverage(te=16000,ta=20000)
    def self.financial_leverage(te, ta)
      te = te.to_d
      ta = ta.to_d

      (ta / te)
    end

    # Estimate future value (fv)
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param pv present value
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @export
    # @examples
    # fv(r=0.07,n=10,pv=1000,pmt=10)
    def self.fv(r, n, pv = 0, pmt = 0, type = 0)
      r = r.to_d
      n = n.to_d
      pv = pv.to_d
      pmt = pmt.to_d
      type = type.to_d

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        (Finrb::Utils.fv_simple(r, n, pv) + Finrb::Utils.fv_annuity(r, n, pmt, type))
      end
    end

    # Estimate future value of an annuity
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @export
    # @examples
    # Finrb::Utils.fv_annuity(0.03,12,-1000)
    #
    # Finrb::Utils.fv_annuity(r=0.03,n=12,pmt=-1000,type=1)
    def self.fv_annuity(r, n, pmt, type = 0)
      r = r.to_d
      n = n.to_d
      pmt = pmt.to_d
      type = type.to_d

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        (pmt / r * (((1 + r)**n) - 1)) * ((1 + r)**type) * -1

      end
    end

    # Estimate future value (fv) of a single sum
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param pv present value
    # @export
    # @examples
    # Finrb::Utils.fv_simple(0.08,10,-300)
    #
    # Finrb::Utils.fv_simple(r=0.04,n=20,pv=-50000)
    def self.fv_simple(r, n, pv)
      r = r.to_d
      n = n.to_d
      pv = pv.to_d

      ((pv * ((1 + r)**n)) * -1)
    end

    # Computing the future value of an uneven cash flow series
    #
    # @param r stated annual rate
    # @param cf uneven cash flow
    # @export
    # @examples
    # Finrb::Utils.fv_uneven(r=0.1, cf=c(-1000, -500, 0, 4000, 3500, 2000))
    def self.fv_uneven(r, cf)
      r = r.to_d
      cf = Array.wrap(cf).map(&:to_d)

      m = cf.size
      sum = 0
      (0...m).each do |i|
        n = m - i
        sum += Finrb::Utils.fv_simple(r, n, cf[i])
      end
      sum
    end

    # Geometric mean return
    #
    # @param r returns over multiple periods
    # @export
    # @examples
    # Finrb::Utils.geometric_mean(r=c(-0.0934, 0.2345, 0.0892))
    def self.geometric_mean(r)
      r = Array.wrap(r).map(&:to_d)

      rs = r + 1
      ((rs.reduce(:*)**(1 / rs.size)) - 1)
    end

    # gross profit margin -- Evaluate a company's financial performance
    #
    # @param gp gross profit, equal to revenue minus cost of goods sold (cogs)
    # @param rv revenue (sales)
    # @export
    # @examples
    # gpm(gp=1000,rv=20000)
    def self.gpm(gp, rv)
      gp = gp.to_d
      rv = rv.to_d

      (gp / rv)
    end

    # harmonic mean, average price
    # @param p price over multiple periods
    # @export
    # @examples
    # Finrb::Utils.harmonic_mean(p=c(8,9,10))
    def self.harmonic_mean(p)
      p = Array.wrap(p).map(&:to_d)

      (1 / (p.sum { |val| 1 / val } / p.size))
    end

    # Computing HPR, the holding period return
    #
    # @param ev ending value
    # @param bv beginning value
    # @param cfr cash flow received
    # @export
    # @examples
    # hpr(ev=33,bv=30,cfr=0.5)
    def self.hpr(ev, bv, cfr = 0)
      ev = ev.to_d
      bv = bv.to_d
      cfr = cfr.to_d

      ((ev - bv + cfr) / bv)
    end

    # bond-equivalent yield (BEY), 2 x the semiannual discount rate
    #
    # @param hpr holding period return
    # @param t number of month remaining until maturity
    # @export
    # @examples
    # hpr2bey(hpr=0.02,t=3)
    def self.hpr2bey(hpr, t)
      hpr = hpr.to_d
      t = t.to_d

      ((((1 + hpr)**(6 / t)) - 1) * 2)
    end

    # Convert holding period return to the effective annual rate
    #
    # @param hpr holding period return
    # @param t number of days remaining until maturity
    # @export
    # @examples
    # hpr2ear(hpr=0.015228,t=120)
    def self.hpr2ear(hpr, t)
      hpr = hpr.to_d
      t = t.to_d

      (((1 + hpr)**(365 / t)) - 1)
    end

    # Computing money market yield (MMY) for a T-bill
    #
    # @param hpr holding period return
    # @param t number of days remaining until maturity
    # @export
    # @examples
    # hpr2mmy(hpr=0.01523,t=120)
    def self.hpr2mmy(hpr, t)
      hpr = hpr.to_d
      t = t.to_d

      (360 * hpr / t)
    end

    # Computing IRR, the internal rate of return
    #
    # @param cf cash flow,the first cash flow is the initial outlay
    # @importFrom stats uniroot
    # @export
    # @examples
    # irr(cf=c(-5, 1.6, 2.4, 2.8))
    def self.irr(cf)
      cf = Array.wrap(cf).map(&:to_d)

      n = cf.size
      subcf = cf.drop(1)
      nlfunc = NlFunctionStub.new
      nlfunc.func =
        lambda do |x|
          BigDecimal(((-1 * Finrb::Utils.pv_uneven(x[0], subcf)) + cf[0]).to_s)
        end

      root = [0]
      nlsolve(nlfunc, root)
      root[0]
    end

    # Computing IRR, the internal rate of return
    # @description This function is the same as irr but can calculate negative value. This function may take a very long time. You can use larger cutoff and larger step to get a less precision irr first. Then based on the result, change from and to, to narrow down the interval, and use a smaller step to get a more precision irr.
    # @param cf cash flow,the first cash flow is the initial outlay
    # @param cutoff threshold to take npv as zero
    # @param from smallest irr to try
    # @param to largest irr to try
    # @param step increment of the irr
    # @export
    # @examples
    # irr2(cf=c(-5, 1.6, 2.4, 2.8))
    # irr2(cf=c(-200, 50, 60, -70, 30, 20))
    def self.irr2(cf, cutoff = 0.1, from = -1, to = 10, step = 0.000001)
      cf = Array.wrap(cf).map(&:to_d)
      cutoff = cutoff.to_d
      from = from.to_d
      to = to.to_d
      step = step.to_d

      r0 = nil
      n = cf.size
      from.step((to - 1), step).each do |r|
        npv = cf[0]
        (1...n).each do |i|
          npv += (cf[i] / ((1 + r)**(i - 1)))
        end
        next if npv.nil?

        if npv.abs < cutoff
          r0 = r
          break
        end
      end

      if r0.nil?
        raise(
          FinrbError,
          'can not find irr in the given interval, you can try smaller step, and/or larger to, and/or larger cutoff'
        )
      end

      r0
    end

    # calculate the net increase in common shares from the potential exercise of stock options or warrants
    #
    # @param amp average market price over the year
    # @param ep  exercise price of the options or warrants
    # @param n   number of common shares that the options and warrants can be convened into
    # @export
    # @examples
    # iss(amp=20,ep=15,n=10000)
    def self.iss(amp, ep, n)
      amp = amp.to_d
      ep = ep.to_d
      n = n.to_d

      if amp > ep
        ((amp - ep) * n / amp)
      else
        raise(FinrbError, 'amp must larger than ep')
      end
    end

    # long-term debt-to-equity -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param ltd long-term debt
    # @param te  total equity
    # @export
    # @examples
    # Finrb::Utils.lt_d2e(ltd=8000,te=20000)
    def self.lt_d2e(ltd, te)
      ltd = ltd.to_d
      te = te.to_d

      (ltd / te)
    end

    # Computing HPR, the holding period return
    #
    # @param mmy money market yield
    # @param t number of days remaining until maturity
    # @export
    # @examples
    # mmy2hpr(mmy=0.04898,t=150)
    def self.mmy2hpr(mmy, t)
      mmy = mmy.to_d
      t = t.to_d

      (mmy * t / 360)
    end

    # Estimate the number of periods
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param pv present value
    # @param fv future value
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @export
    # @examples
    # Finrb::Utils.n_period(0.1,-10000,60000000,-50000,0)
    #
    # Finrb::Utils.n_period(r=0.1,pv=-10000,fv=60000000,pmt=-50000,type=1)
    def self.n_period(r, pv, fv, pmt, type = 0)
      r = r.to_d
      pv = pv.to_d
      fv = fv.to_d
      pmt = pmt.to_d
      type = type.to_d

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        (-1 * ((fv * r) - (pmt * ((1 + r)**type))) / ((pv * r) + (pmt * ((1 + r)**type)))).to_d.log / (1 + r).to_d.log

      end
    end

    # net profit margin -- Evaluate a company's financial performance
    #
    # @param ni net income
    # @param rv revenue (sales)
    # @export
    # @examples
    # npm(ni=8000,rv=20000)
    def self.npm(ni, rv)
      ni = ni.to_d
      rv = rv.to_d

      (ni / rv)
    end

    # Computing NPV, the PV of the cash flows less the initial (time = 0) outlay
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param cf cash flow,the first cash flow is the initial outlay
    # @export
    # @examples
    # npv(r=0.12, cf=c(-5, 1.6, 2.4, 2.8))
    def self.npv(r, cf)
      r = r.to_d
      cf = Array.wrap(cf).map(&:to_d)

      n = cf.size
      subcf = cf.drop(1)
      ((-1 * Finrb::Utils.pv_uneven(r, subcf)) + cf[0])
    end

    # Estimate period payment
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param pv present value
    # @param fv future value
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @export
    # @examples
    # pmt(0.08,10,-1000,10)
    #
    # pmt(r=0.08,n=10,pv=-1000,fv=0)
    #
    # pmt(0.08,10,-1000,10,1)
    def self.pmt(r, n, pv, fv, type = 0)
      r = r.to_d
      n = n.to_d
      pv = pv.to_d
      fv = fv.to_d
      type = type.to_d

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        (pv + (fv / ((1 + r)**n))) * r / (1 - (1 / ((1 + r)**n))) * -1 * ((1 + r)**(-1 * type))

      end
    end

    # Estimate present value (pv)
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param fv future value
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @export
    # @examples
    # pv(0.07,10,1000,10)
    #
    # pv(r=0.05,n=20,fv=1000,pmt=10,type=1)
    def self.pv(r, n, fv = 0, pmt = 0, type = 0)
      r = r.to_d
      n = n.to_d
      fv = fv.to_d
      pmt = pmt.to_d
      type = type.to_d

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        Finrb::Utils.pv_simple(r, n, fv) + Finrb::Utils.pv_annuity(r, n, pmt, type)

      end
    end

    # Estimate present value (pv) of an annuity
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @export
    # @examples
    # Finrb::Utils.pv_annuity(0.03,12,1000)
    #
    # Finrb::Utils.pv_annuity(r=0.0425,n=3,pmt=30000)
    def self.pv_annuity(r, n, pmt, type = 0)
      r = r.to_d
      n = n.to_d
      pmt = pmt.to_d
      type = type.to_d

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        (pmt / r * (1 - (1 / ((1 + r)**n)))) * ((1 + r)**type) * -1

      end
    end

    # Estimate present value of a perpetuity
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param g growth rate of perpetuity
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @export
    # @examples
    # Finrb::Utils.pv_perpetuity(r=0.1,pmt=1000,g=0.02)
    #
    # Finrb::Utils.pv_perpetuity(r=0.1,pmt=1000,type=1)
    #
    # Finrb::Utils.pv_perpetuity(r=0.1,pmt=1000)
    def self.pv_perpetuity(r, pmt, g = 0, type = 0)
      r = r.to_d
      pmt = pmt.to_d
      g = g.to_d
      type = type.to_d

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      elsif g >= r
        raise(FinrbError, 'Error: g is not smaller than r!')
      else
        (pmt / (r - g)) * ((1 + r)**type) * -1

      end
    end

    # Estimate present value (pv) of a single sum
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param fv future value
    # @export
    # @examples
    # Finrb::Utils.pv_simple(0.07,10,100)
    #
    # Finrb::Utils.pv_simple(r=0.03,n=3,fv=1000)
    def self.pv_simple(r, n, fv)
      r = r.to_d
      n = n.to_d
      fv = fv.to_d

      ((fv / ((1 + r)**n)) * -1)
    end

    # Computing the present value of an uneven cash flow series
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param cf uneven cash flow
    # @export
    # @examples
    # Finrb::Utils.pv_uneven(r=0.1, cf=c(-1000, -500, 0, 4000, 3500, 2000))
    def self.pv_uneven(r, cf)
      r = r.to_d
      cf = Array.wrap(cf).map(&:to_d)

      n = cf.size
      sum = 0
      (0...n).each do |i|
        sum += Finrb::Utils.pv_simple(r, i, cf[i])
      end
      sum
    end

    # quick ratio -- Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
    #
    # @param cash cash
    # @param ms   marketable securities
    # @param rc   receivables
    # @param cl   current liabilities
    # @export
    # @examples
    # Finrb::Utils.quick_ratio(cash=3000,ms=2000,rc=1000,cl=2000)
    def self.quick_ratio(cash, ms, rc, cl)
      cash = cash.to_d
      ms = ms.to_d
      rc = rc.to_d
      cl = cl.to_d

      ((cash + ms + rc) / cl)
    end

    # Convert a given norminal rate to a continuous compounded rate
    #
    # @param r norminal rate
    # @param m number of times compounded each year
    # @export
    # @examples
    # Finrb::Utils.r_continuous(r=0.03,m=4)
    def self.r_continuous(r, m)
      r = r.to_d
      m = m.to_d

      (m * (1 + (r / m)).to_d.log)
    end

    # Convert a given continuous compounded rate to a norminal rate
    #
    # @param rc continuous compounded rate
    # @param m number of desired times compounded each year
    # @export
    # @examples
    # Finrb::Utils.r_norminal(0.03,1)
    #
    # Finrb::Utils.r_norminal(rc=0.03,m=4)
    def self.r_norminal(rc, m)
      rc = rc.to_d
      m = m.to_d

      (m * ((rc / m).to_d.exp - 1))
    end

    # Rate of return for a perpetuity
    #
    # @param pmt payment per period
    # @param pv present value
    # @export
    # @examples
    # Finrb::Utils.r_perpetuity(pmt=4.5,pv=-75)
    def self.r_perpetuity(pmt, pv)
      pmt = pmt.to_d
      pv = pv.to_d

      (-1 * pmt / pv)
    end

    # Computing Sampling error
    #
    # @param sm sample mean
    # @param mu population mean
    # @export
    # @examples
    # Finrb::Utils.sampling_error(sm=0.45, mu=0.5)
    def self.sampling_error(sm, mu)
      sm = sm.to_d
      mu = mu.to_d

      (sm - mu)
    end

    # Computing Roy's safety-first ratio
    #
    # @param rp portfolio return
    # @param rl threshold level return
    # @param sd standard deviation of portfolio retwns
    # @export
    # @examples
    # Finrb::Utils.sf_ratio(rp=0.09,rl=0.03,sd=0.12)
    def self.sf_ratio(rp, rl, sd)
      rp = rp.to_d
      rl = rl.to_d
      sd = sd.to_d

      ((rp - rl) / sd)
    end

    # Computing Sharpe Ratio
    #
    # @param rp portfolio return
    # @param rf risk-free return
    # @param sd standard deviation of portfolio retwns
    # @export
    # @examples
    # Finrb::Utils.sharpe_ratio(rp=0.038,rf=0.015,sd=0.07)
    def self.sharpe_ratio(rp, rf, sd)
      rp = rp.to_d
      rf = rf.to_d
      sd = sd.to_d

      ((rp - rf) / sd)
    end

    # Depreciation Expense Recognition -- Straight-line depreciation (SL) allocates an equal amount of depreciation each year over the asset's useful life
    #
    # @param cost cost of long-lived assets
    # @param rv   residual value of the long-lived assets at the end of its useful life
    # @param t    length of the useful life
    # @export
    # @examples
    # slde(cost=1200,rv=200,t=5)
    def self.slde(cost, rv, t)
      cost = cost.to_d
      rv = rv.to_d
      t = t.to_d

      ((cost - rv) / t)
    end

    # total debt-to-equity -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param td total debt
    # @param te total equity
    # @export
    # @examples
    # Finrb::Utils.total_d2e(td=6000,te=20000)
    def self.total_d2e(td, te)
      td = td.to_d
      te = te.to_d

      (td / te)
    end

    # Computing TWRR, the time-weighted rate of return
    #
    # @param ev ordered ending value list
    # @param bv ordered beginning value list
    # @param cfr ordered cash flow received list
    # @export
    # @examples
    # twrr(ev=c(120,260),bv=c(100,240),cfr=c(2,4))
    def self.twrr(ev, bv, cfr)
      ev = Array.wrap(ev).map(&:to_d)
      bv = Array.wrap(bv).map(&:to_d)
      cfr = Array.wrap(cfr).map(&:to_d)

      r = ev.size
      s = bv.size
      t = cfr.size
      wr = 1
      if r != s || r != t || s != t
        raise(FinrbError, 'Different number of values!')
      else
        (0...r).each do |i|
          wr *= (hpr(ev[i], bv[i], cfr[i]) + 1)
        end
        ((wr**(1 / r)) - 1)
      end
    end

    # calculate weighted average shares -- weighted average number of common shares
    #
    # @param ns n x 1 vector vector of number of shares
    # @param nm n x 1 vector vector of number of months relate to ns
    # @export
    # @examples
    # s=c(10000,2000);m=c(12,6);was(ns=s,nm=m)
    #
    # s=c(11000,4400,-3000);m=c(12,9,4);was(ns=s,nm=m)
    def self.was(ns, nm)
      ns = Array.wrap(ns).map(&:to_d)
      nm = Array.wrap(nm).map(&:to_d)

      m = ns.size
      n = nm.size
      sum = 0
      if m == n
        (0...m).each do |i|
          sum += (ns[i] * nm[i])
        end
      else
        raise(FinrbError, 'length of ns and nm must be equal')
      end
      sum /= 12
      sum
    end

    # Weighted mean as a portfolio return
    #
    # @param r returns of the individual assets in the portfolio
    # @param w corresponding weights associated with each of the individual assets
    # @export
    # @examples
    # wpr(r=c(0.12, 0.07, 0.03),w=c(0.5,0.4,0.1))
    def self.wpr(r, w)
      r = Array.wrap(r).map(&:to_d)
      w = Array.wrap(w).map(&:to_d)

      if w.sum != 1
        puts('sum of weights is NOT equal to 1!') # TODO: need to change
      else
      end
      r.zip(w).sum { |arr| arr.reduce(:*) }
    end
  end
end
