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
    # @example
    #   Finrb::Utils.bdy(d=1500,f=100000,t=120)
    def self.bdy(d:, f:, t:)
      d = DecNum(d.to_s)
      f = DecNum(f.to_s)
      t = DecNum(t.to_s)

      (d * 360 / f / t)
    end

    # Computing money market yield (MMY) for a T-bill
    #
    # @param bdy bank discount yield
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Utils.bdy2mmy(bdy=0.045,t=120)
    def self.bdy2mmy(bdy:, t:)
      bdy = DecNum(bdy.to_s)
      t = DecNum(t.to_s)

      (bdy * 360 / (360 - (t * bdy)))
    end

    # cash ratio -- Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
    #
    # @param cash cash
    # @param ms   marketable securities
    # @param cl   current liabilities
    # @example
    #   Finrb::Utils.cash_ratio(cash=3000,ms=2000,cl=2000)
    def self.cash_ratio(cash:, ms:, cl:)
      cash = DecNum(cash.to_s)
      ms = DecNum(ms.to_s)
      cl = DecNum(cl.to_s)

      ((cash + ms) / cl)
    end

    # Computing Coefficient of variation
    #
    # @param sd standard deviation
    # @param avg average value
    # @example
    #   Finrb::Utils.coefficient_variation(sd=0.15,avg=0.39)
    def self.coefficient_variation(sd:, avg:)
      sd = DecNum(sd.to_s)
      avg = DecNum(avg.to_s)

      (sd / avg)
    end

    # Cost of goods sold and ending inventory under three methods (FIFO,LIFO,Weighted average)
    #
    # @param uinv units of beginning inventory
    # @param pinv price of beginning inventory
    # @param units nx1 vector of inventory units. inventory purchased ordered by time (from first to last)
    # @param price nx1 vector of inventory price. same order as units
    # @param sinv units of sold inventory
    # @param method inventory methods: FIFO (first in first out, permitted under both US and IFRS), LIFO (late in first out, US only), WAC (weighted average cost,US and IFRS)
    # @example
    #   Finrb::Utils.cogs(uinv=2,pinv=2,units=[3,5],price=[3,5],sinv=7,method="FIFO")
    #
    # @example
    #   Finrb::Utils.cogs(uinv=2,pinv=2,units=[3,5],price=[3,5],sinv=7,method="LIFO")
    #
    # @example
    #   Finrb::Utils.cogs(uinv=2,pinv=2,units=[3,5],price=[3,5],sinv=7,method="WAC")
    def self.cogs(uinv:, pinv:, units:, price:, sinv:, method: 'FIFO')
      uinv = DecNum(uinv.to_s)
      pinv = DecNum(pinv.to_s)
      units = Array.wrap(units).map { |value| DecNum(value.to_s) }
      price = Array.wrap(price).map { |value| DecNum(value.to_s) }
      sinv = DecNum(sinv.to_s)
      method = method.to_s

      n = units.size
      m = price.size
      cost_of_goods = 0
      ending_inventory = 0
      if m == n
        case method
        when 'FIFO'
          if sinv <= uinv
            cost_of_goods = sinv * pinv
            ending_inventory = (uinv - sinv) * pinv
            (0...n).each do |i|
              ending_inventory += (units[i] * price[i])
            end
          else
            cost_of_goods = uinv * pinv
            sinv -= uinv
            (0...n).each do |i|
              if sinv <= units[i]
                cost_of_goods += (sinv * price[i])
                ending_inventory = (units[i] - sinv) * price[i]
                if i < n
                  temp = i + 1
                  (temp...n).each do |j|
                    ending_inventory += (units[j] * price[j])
                  end
                end
                sinv = 0
                next
              else
                cost_of_goods += (units[i] * price[i])
                sinv -= units[i]
              end
            end
            raise(FinrbError, "Inventory is not enough to sell\n") if sinv.positive?
          end
        when 'WAC'
          ending_inventory = uinv * pinv
          tu = uinv
          (0...n).each do |i|
            ending_inventory += (units[i] * price[i])
            tu += units[i]
          end
          if tu >= sinv
            cost_of_goods = ending_inventory / tu * sinv
            ending_inventory = ending_inventory / tu * (tu - sinv)
          else
            raise(FinrbError, "Inventory is not enough to sell\n")
          end

        when 'LIFO'
          (n - 1).downto(0).each do |i|
            if sinv <= units[i]
              cost_of_goods += (sinv * price[i])
              ending_inventory = (units[i] - sinv) * price[i]
              if i > 1
                temp = i - 1
                temp.downto(0).each do |j|
                  ending_inventory += (units[j] * price[j])
                end
              end
              ending_inventory += (uinv * pinv)
              sinv = 0
              next
            else
              cost_of_goods += (units[i] * price[i])
              sinv -= units[i]
            end
          end
          if sinv.positive?
            if sinv <= uinv
              cost_of_goods += (sinv * pinv)
              ending_inventory += ((uinv - sinv) * pinv)
            else
              raise(FinrbError, "Inventory is not enough to sell\n")
            end
          end
        end

      else
        raise(FinrbError, "length of units and price are not the same\n")
      end

      {
        cost_of_goods: cost_of_goods,
        ending_inventory: ending_inventory
      }
    end

    # current ratio -- Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
    #
    # @param ca current assets
    # @param cl current liabilities
    # @example
    #   Finrb::Utils.current_ratio(ca=8000,cl=2000)
    def self.current_ratio(ca:, cl:)
      ca = DecNum(ca.to_s)
      cl = DecNum(cl.to_s)

      (ca / cl)
    end

    # Depreciation Expense Recognition -- double-declining balance (DDB), the most common declining balance method, which applies two times the straight-line rate to the declining balance.
    #
    # @param cost cost of long-lived assets
    # @param rv   residual value of the long-lived assets at the end of its useful life. DDB does not explicitly use the asset's residual value in the calculations, but depreciation ends once the estimated residual value has been reached. If the asset is expected to have no residual value, the DB method will never fully depreciate it, so the DB method is typically changed to straight-line at some point in the asset's life.
    # @param t    length of the useful life
    # @example
    #   Finrb::Utils.ddb(cost=1200,rv=200,t=5)
    def self.ddb(cost:, rv:, t:)
      cost = DecNum(cost.to_s)
      rv = DecNum(rv.to_s)
      t = DecNum(t.to_s)

      raise(FinrbError, 't should be larger than 1') if t < 2

      ddb = [0] * t
      ddb[0] = cost * 2 / t
      if cost - ddb[0] <= rv
        ddb[0] = cost - rv
      else
        cost -= ddb[0]
        (1...t).each do |i|
          ddb[i] = cost * 2 / t
          if cost - ddb[i] <= rv
            ddb[i] = cost - rv
            break
          else
            cost -= ddb[i]
          end
        end
      end
      { t: (0...t).to_a, ddb: ddb }
    end

    # debt ratio -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param td total debt
    # @param ta total assets
    # @example
    #   Finrb::Utils.debt_ratio(td=6000,ta=20000)
    def self.debt_ratio(td:, ta:)
      td = DecNum(td.to_s)
      ta = DecNum(ta.to_s)

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
    # @example
    #   Finrb::Utils.diluted_eps(ni=115600,pd=10000,cdi=42000,tax=0.4,w=200000,cds=60000)
    #
    # @example
    #   Finrb::Utils.diluted_eps(ni=115600,pd=10000,cpd=10000,w=200000,cps=40000)
    #
    # @example
    #   Finrb::Utils.diluted_eps(ni=115600,pd=10000,w=200000,iss=2500)
    #
    # @example
    #   Finrb::Utils.diluted_eps(ni=115600,pd=10000,cpd=10000,cdi=42000,tax=0.4,w=200000,cps=40000,cds=60000,iss=2500)
    def self.diluted_eps(ni:, pd:, w:, cpd: 0, cdi: 0, tax: 0, cps: 0, cds: 0, iss: 0)
      ni = DecNum(ni.to_s)
      pd = DecNum(pd.to_s)
      w = DecNum(w.to_s)
      cpd = DecNum(cpd.to_s)
      cdi = DecNum(cdi.to_s)
      tax = DecNum(tax.to_s)
      cps = DecNum(cps.to_s)
      cds = DecNum(cds.to_s)
      iss = DecNum(iss.to_s)

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
    # @example
    #   Finrb::Utils.discount_rate(n=5,pv=0,fv=600,pmt=-100,type=0)
    def self.discount_rate(n:, pv:, fv:, pmt:, type: 0, lower: 0.0001, upper: 100)
      n = DecNum(n.to_s)
      pv = DecNum(pv.to_s)
      fv = DecNum(fv.to_s)
      pmt = DecNum(pmt.to_s)
      type = DecNum(type.to_s)
      lower = DecNum(lower.to_s)
      upper = DecNum(upper.to_s)

      nlfunc = NlFunctionStub.new
      nlfunc.func =
        lambda do |x|
          [BigDecimal((Finrb::Utils.fv_simple(r: x[0], n: n, pv: pv) + Finrb::Utils.fv_annuity(r: x[0], n: n, pmt: pmt, type: type) - fv).to_s)]
        end

      root = [(upper - lower) / 2]
      nlsolve(nlfunc, root)
      root[0]
    end

    # Convert stated annual rate to the effective annual rate
    #
    # @param r stated annual rate
    # @param m number of compounding periods per year
    # @example
    #   Finrb::Utils.ear(r=0.12,m=12)
    #
    # @example
    #   Finrb::Utils.ear(0.04,365)
    def self.ear(r:, m:)
      r = DecNum(r.to_s)
      m = DecNum(m.to_s)

      ((((r / m) + 1)**m) - 1)
    end

    # Convert stated annual rate to the effective annual rate with continuous compounding
    #
    # @param r stated annual rate
    # @example
    #   Finrb::Utils.ear_continuous(r=0.1)
    #
    # @example
    #   Finrb::Utils.ear_continuous(0.03)
    def self.ear_continuous(r:)
      r = DecNum(r.to_s)

      (r.to_dec.exp - 1)
    end

    # bond-equivalent yield (BEY), 2 x the semiannual discount rate
    #
    # @param ear effective annual rate
    # @example
    #   Finrb::Utils.ear2bey(ear=0.08)
    def self.ear2bey(ear:)
      ear = DecNum(ear.to_s)

      ((((ear + 1)**0.5) - 1) * 2)
    end

    # Computing HPR, the holding period return
    #
    # @param ear effective annual rate
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Utils.ear2hpr(ear=0.05039,t=150)
    def self.ear2hpr(ear:, t:)
      ear = DecNum(ear.to_s)
      t = DecNum(t.to_s)

      (((ear + 1)**(t / 365)) - 1)
    end

    # Equivalent/proportional Interest Rates
    # @note An interest rate to be applied n times p.a. can be converted to an equivalent rate to be applied p times p.a.
    # @param r interest rate to be applied n times per year (r is annual rate!)
    # @param n times that the interest rate r were compounded per year
    # @param p times that the equivalent rate were compounded per year
    # @param type equivalent interest rates ('e',default) or proportional interest rates ('p')
    # @example
    #   # monthly interest rat equivalent to 5% compounded per year
    #   Finrb::Utils.eir(r=0.05,n=1,p=12)
    #
    # @example
    #   # monthly interest rat equivalent to 5% compounded per half year
    #   Finrb::Utils.eir(r=0.05,n=2,p=12)
    #
    # @example
    #   # monthly interest rat equivalent to 5% compounded per quarter
    #   Finrb::Utils.eir(r=0.05,n=4,p=12)
    #
    # @example
    #   # annual interest rate equivalent to 5% compounded per month
    #   Finrb::Utils.eir(r=0.05,n=12,p=1)
    #   # this is equivalent to
    #   Finrb::Utils.ear(r=0.05,m=12)
    #
    # @example
    #   # quarter interest rate equivalent to 5% compounded per year
    #   Finrb::Utils.eir(r=0.05,n=1,p=4)
    #
    # @example
    #   # quarter interest rate equivalent to 5% compounded per month
    #   Finrb::Utils.eir(r=0.05,n=12,p=4)
    #
    # @example
    #   # monthly proportional interest rate which is equivalent to a simple annual interest
    #   Finrb::Utils.eir(r=0.05,p=12,type='p')
    def self.eir(r:, n: 1, p: 12, type: 'e')
      r = DecNum(r.to_s)
      n = DecNum(n.to_s)
      p = DecNum(p.to_s)
      type = type.to_s

      case type
      when 'e'
        eir = (((r / n) + 1)**(n / p)) - 1
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
    # @example
    #   Finrb::Utils.eps(ni=10000,pd=1000,w=11000)
    def self.eps(ni:, pd:, w:)
      ni = DecNum(ni.to_s)
      pd = DecNum(pd.to_s)
      w = DecNum(w.to_s)

      ((ni - pd) / w)
    end

    # financial leverage -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param te total equity
    # @param ta total assets
    # @example
    #   Finrb::Utils.financial_leverage(te=16000,ta=20000)
    def self.financial_leverage(te:, ta:)
      te = DecNum(te.to_s)
      ta = DecNum(ta.to_s)

      (ta / te)
    end

    # Estimate future value (fv)
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param pv present value
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @example
    #   Finrb::Utils.fv(r=0.07,n=10,pv=1000,pmt=10)
    def self.fv(r:, n:, pv: 0, pmt: 0, type: 0)
      r = DecNum(r.to_s)
      n = DecNum(n.to_s)
      pv = DecNum(pv.to_s)
      pmt = DecNum(pmt.to_s)
      type = DecNum(type.to_s)

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        (Finrb::Utils.fv_simple(r: r, n: n, pv: pv) + Finrb::Utils.fv_annuity(r: r, n: n, pmt: pmt, type: type))
      end
    end

    # Estimate future value of an annuity
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @example
    #   Finrb::Utils.fv_annuity(0.03,12,-1000)
    #
    # @example
    #   Finrb::Utils.fv_annuity(r=0.03,n=12,pmt=-1000,type=1)
    def self.fv_annuity(r:, n:, pmt:, type: 0)
      r = DecNum(r.to_s)
      n = DecNum(n.to_s)
      pmt = DecNum(pmt.to_s)
      type = DecNum(type.to_s)

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        (pmt / r * (((r + 1)**n) - 1)) * ((r + 1)**type) * -1

      end
    end

    # Estimate future value (fv) of a single sum
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param pv present value
    # @example
    #   Finrb::Utils.fv_simple(0.08,10,-300)
    #
    # @example
    #   Finrb::Utils.fv_simple(r=0.04,n=20,pv=-50000)
    def self.fv_simple(r:, n:, pv:)
      r = DecNum(r.to_s)
      n = DecNum(n.to_s)
      pv = DecNum(pv.to_s)

      ((pv * ((r + 1)**n)) * -1)
    end

    # Computing the future value of an uneven cash flow series
    #
    # @param r stated annual rate
    # @param cf uneven cash flow
    # @example
    #   Finrb::Utils.fv_uneven(r=0.1, cf=[-1000, -500, 0, 4000, 3500, 2000])
    def self.fv_uneven(r:, cf:)
      r = DecNum(r.to_s)
      cf = Array.wrap(cf).map { |value| DecNum(value.to_s) }

      m = cf.size
      sum = 0
      (0...m).each do |i|
        n = m - (i + 1)
        sum += Finrb::Utils.fv_simple(r: r, n: n, pv: cf[i])
      end
      sum
    end

    # Geometric mean return
    #
    # @param r returns over multiple periods
    # @example
    #   Finrb::Utils.geometric_mean(r=[-0.0934, 0.2345, 0.0892])
    def self.geometric_mean(r:)
      r = Array.wrap(r).map { |value| DecNum(value.to_s) }

      rs = r.map { |value| value + 1 }
      ((rs.reduce(:*)**(1.to_f / rs.size)) - 1)
    end

    # gross profit margin -- Evaluate a company's financial performance
    #
    # @param gp gross profit, equal to revenue minus cost of goods sold (cogs)
    # @param rv revenue (sales)
    # @example
    #   Finrb::Utils.gpm(gp=1000,rv=20000)
    def self.gpm(gp:, rv:)
      gp = DecNum(gp.to_s)
      rv = DecNum(rv.to_s)

      (gp / rv)
    end

    # harmonic mean, average price
    # @param p price over multiple periods
    # @example
    #   Finrb::Utils.harmonic_mean(p=[8,9,10])
    def self.harmonic_mean(p:)
      p = Array.wrap(p).map { |value| DecNum(value.to_s) }

      (1.to_f / (p.sum { |val| 1.to_f / val } / p.size))
    end

    # Computing HPR, the holding period return
    #
    # @param ev ending value
    # @param bv beginning value
    # @param cfr cash flow received
    # @example
    #   Finrb::Utils.hpr(ev=33,bv=30,cfr=0.5)
    def self.hpr(ev:, bv:, cfr: 0)
      ev = DecNum(ev.to_s)
      bv = DecNum(bv.to_s)
      cfr = DecNum(cfr.to_s)

      ((ev - bv + cfr) / bv)
    end

    # bond-equivalent yield (BEY), 2 x the semiannual discount rate
    #
    # @param hpr holding period return
    # @param t number of month remaining until maturity
    # @example
    #   Finrb::Utils.hpr2bey(hpr=0.02,t=3)
    def self.hpr2bey(hpr:, t:)
      hpr = DecNum(hpr.to_s)
      t = DecNum(t.to_s)

      ((((hpr + 1)**(6 / t)) - 1) * 2)
    end

    # Convert holding period return to the effective annual rate
    #
    # @param hpr holding period return
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Utils.hpr2ear(hpr=0.015228,t=120)
    def self.hpr2ear(hpr:, t:)
      hpr = DecNum(hpr.to_s)
      t = DecNum(t.to_s)

      (((hpr + 1)**(365 / t)) - 1)
    end

    # Computing money market yield (MMY) for a T-bill
    #
    # @param hpr holding period return
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Utils.hpr2mmy(hpr=0.01523,t=120)
    def self.hpr2mmy(hpr:, t:)
      hpr = DecNum(hpr.to_s)
      t = DecNum(t.to_s)

      (hpr * 360 / t)
    end

    # Computing IRR, the internal rate of return
    #
    # @param cf cash flow,the first cash flow is the initial outlay
    # @example
    #   Finrb::Utils.irr(cf=[-5, 1.6, 2.4, 2.8])
    def self.irr(cf:)
      cf = Array.wrap(cf).map { |value| DecNum(value.to_s) }

      subcf = cf.drop(1)
      nlfunc = NlFunctionStub.new
      nlfunc.func =
        lambda do |x|
          [BigDecimal(((Finrb::Utils.pv_uneven(r: x[0], cf: subcf) * -1) + cf[0]).to_s)]
        end

      root = [0]
      nlsolve(nlfunc, root)
      root[0]
    end

    # calculate the net increase in common shares from the potential exercise of stock options or warrants
    #
    # @param amp average market price over the year
    # @param ep  exercise price of the options or warrants
    # @param n   number of common shares that the options and warrants can be convened into
    # @example
    #   Finrb::Utils.iss(amp=20,ep=15,n=10000)
    def self.iss(amp:, ep:, n:)
      amp = DecNum(amp.to_s)
      ep = DecNum(ep.to_s)
      n = DecNum(n.to_s)

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
    # @example
    #   Finrb::Utils.lt_d2e(ltd=8000,te=20000)
    def self.lt_d2e(ltd:, te:)
      ltd = DecNum(ltd.to_s)
      te = DecNum(te.to_s)

      (ltd / te)
    end

    # Computing HPR, the holding period return
    #
    # @param mmy money market yield
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Utils.mmy2hpr(mmy=0.04898,t=150)
    def self.mmy2hpr(mmy:, t:)
      mmy = DecNum(mmy.to_s)
      t = DecNum(t.to_s)

      (mmy * t / 360)
    end

    # Estimate the number of periods
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param pv present value
    # @param fv future value
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @example
    #   Finrb::Utils.n_period(0.1,-10000,60000000,-50000,0)
    #
    # @example
    #   Finrb::Utils.n_period(r=0.1,pv=-10000,fv=60000000,pmt=-50000,type=1)
    def self.n_period(r:, pv:, fv:, pmt:, type: 0)
      r = DecNum(r.to_s)
      pv = DecNum(pv.to_s)
      fv = DecNum(fv.to_s)
      pmt = DecNum(pmt.to_s)
      type = DecNum(type.to_s)

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        (((fv * r) - (pmt * ((r + 1)**type))) * -1 / ((pv * r) + (pmt * ((r + 1)**type)))).to_dec.log / (r + 1).to_dec.log

      end
    end

    # net profit margin -- Evaluate a company's financial performance
    #
    # @param ni net income
    # @param rv revenue (sales)
    # @example
    #   Finrb::Utils.npm(ni=8000,rv=20000)
    def self.npm(ni:, rv:)
      ni = DecNum(ni.to_s)
      rv = DecNum(rv.to_s)

      (ni / rv)
    end

    # Computing NPV, the PV of the cash flows less the initial (time = 0) outlay
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param cf cash flow,the first cash flow is the initial outlay
    # @example
    #   Finrb::Utils.npv(r=0.12, cf=[-5, 1.6, 2.4, 2.8])
    def self.npv(r:, cf:)
      r = DecNum(r.to_s)
      cf = Array.wrap(cf).map { |value| DecNum(value.to_s) }

      subcf = cf.drop(1)
      ((Finrb::Utils.pv_uneven(r: r, cf: subcf) * -1) + cf[0])
    end

    # Estimate period payment
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param pv present value
    # @param fv future value
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @example
    #   Finrb::Utils.pmt(0.08,10,-1000,10)
    #
    # @example
    #   Finrb::Utils.pmt(r=0.08,n=10,pv=-1000,fv=0)
    #
    # @example
    #   Finrb::Utils.pmt(0.08,10,-1000,10,1)
    def self.pmt(r:, n:, pv:, fv:, type: 0)
      r = DecNum(r.to_s)
      n = DecNum(n.to_s)
      pv = DecNum(pv.to_s)
      fv = DecNum(fv.to_s)
      type = DecNum(type.to_s)

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        (pv + (fv / ((r + 1)**n))) * r / (1 - (1.to_f / ((r + 1)**n))) * -1 * ((r + 1)**(type * -1))
      end
    end

    # Estimate present value (pv)
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param fv future value
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @example
    #   Finrb::Utils.pv(0.07,10,1000,10)
    #
    # @example
    #   Finrb::Utils.pv(r=0.05,n=20,fv=1000,pmt=10,type=1)
    def self.pv(r:, n:, fv: 0, pmt: 0, type: 0)
      r = DecNum(r.to_s)
      n = DecNum(n.to_s)
      fv = DecNum(fv.to_s)
      pmt = DecNum(pmt.to_s)
      type = DecNum(type.to_s)

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        Finrb::Utils.pv_simple(r: r, n: n, fv: fv) + Finrb::Utils.pv_annuity(r: r, n: n, pmt: pmt, type: type)

      end
    end

    # Estimate present value (pv) of an annuity
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @example
    #   Finrb::Utils.pv_annuity(0.03,12,1000)
    #
    # @example
    #   Finrb::Utils.pv_annuity(r=0.0425,n=3,pmt=30000)
    def self.pv_annuity(r:, n:, pmt:, type: 0)
      r = DecNum(r.to_s)
      n = DecNum(n.to_s)
      pmt = DecNum(pmt.to_s)
      type = DecNum(type.to_s)

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      else
        (pmt / r * (1 - (1.to_f / ((r + 1)**n)))) * ((r + 1)**type) * -1

      end
    end

    # Estimate present value of a perpetuity
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param g growth rate of perpetuity
    # @param pmt payment per period
    # @param type payments occur at the end of each period (type=0); payments occur at the beginning of each period (type=1)
    # @example
    #   Finrb::Utils.pv_perpetuity(r=0.1,pmt=1000,g=0.02)
    #
    # @example
    #   Finrb::Utils.pv_perpetuity(r=0.1,pmt=1000,type=1)
    #
    # @example
    #   Finrb::Utils.pv_perpetuity(r=0.1,pmt=1000)
    def self.pv_perpetuity(r:, pmt:, g: 0, type: 0)
      r = DecNum(r.to_s)
      pmt = DecNum(pmt.to_s)
      g = DecNum(g.to_s)
      type = DecNum(type.to_s)

      if type != 0 && type != 1
        raise(FinrbError, 'Error: type should be 0 or 1!')
      elsif g >= r
        raise(FinrbError, 'Error: g is not smaller than r!')
      else
        (pmt / (r - g)) * ((r + 1)**type) * -1

      end
    end

    # Estimate present value (pv) of a single sum
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param n number of periods
    # @param fv future value
    # @example
    #   Finrb::Utils.pv_simple(0.07,10,100)
    #
    # @example
    #   Finrb::Utils.pv_simple(r=0.03,n=3,fv=1000)
    def self.pv_simple(r:, n:, fv:)
      r = DecNum(r.to_s)
      n = DecNum(n.to_s)
      fv = DecNum(fv.to_s)

      ((fv / ((r + 1)**n)) * -1)
    end

    # Computing the present value of an uneven cash flow series
    #
    # @param r discount rate, or the interest rate at which the amount will be compounded each period
    # @param cf uneven cash flow
    # @example
    #   Finrb::Utils.pv_uneven(r=0.1, cf=[-1000, -500, 0, 4000, 3500, 2000])
    def self.pv_uneven(r:, cf:)
      r = DecNum(r.to_s)
      cf = Array.wrap(cf).map { |value| DecNum(value.to_s) }

      n = cf.size
      sum = 0
      (0...n).each do |i|
        sum += Finrb::Utils.pv_simple(r: r, n: i + 1, fv: cf[i])
      end
      sum
    end

    # quick ratio -- Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
    #
    # @param cash cash
    # @param ms   marketable securities
    # @param rc   receivables
    # @param cl   current liabilities
    # @example
    #   Finrb::Utils.quick_ratio(cash=3000,ms=2000,rc=1000,cl=2000)
    def self.quick_ratio(cash:, ms:, rc:, cl:)
      cash = DecNum(cash.to_s)
      ms = DecNum(ms.to_s)
      rc = DecNum(rc.to_s)
      cl = DecNum(cl.to_s)

      ((cash + ms + rc) / cl)
    end

    # Convert a given norminal rate to a continuous compounded rate
    #
    # @param r norminal rate
    # @param m number of times compounded each year
    # @example
    #   Finrb::Utils.r_continuous(r=0.03,m=4)
    def self.r_continuous(r:, m:)
      r = DecNum(r.to_s)
      m = DecNum(m.to_s)

      (m * ((r / m) + 1).to_dec.log)
    end

    # Convert a given continuous compounded rate to a norminal rate
    #
    # @param rc continuous compounded rate
    # @param m number of desired times compounded each year
    # @example
    #   Finrb::Utils.r_norminal(0.03,1)
    #
    # @example
    #   Finrb::Utils.r_norminal(rc=0.03,m=4)
    def self.r_norminal(rc:, m:)
      rc = DecNum(rc.to_s)
      m = DecNum(m.to_s)

      (m * ((rc / m).to_dec.exp - 1))
    end

    # Rate of return for a perpetuity
    #
    # @param pmt payment per period
    # @param pv present value
    # @example
    #   Finrb::Utils.r_perpetuity(pmt=4.5,pv=-75)
    def self.r_perpetuity(pmt:, pv:)
      pmt = DecNum(pmt.to_s)
      pv = DecNum(pv.to_s)

      (pmt * -1 / pv)
    end

    # Computing Sampling error
    #
    # @param sm sample mean
    # @param mu population mean
    # @example
    #   Finrb::Utils.sampling_error(sm=0.45, mu=0.5)
    def self.sampling_error(sm:, mu:)
      sm = DecNum(sm.to_s)
      mu = DecNum(mu.to_s)

      (sm - mu)
    end

    # Computing Roy's safety-first ratio
    #
    # @param rp portfolio return
    # @param rl threshold level return
    # @param sd standard deviation of portfolio retwns
    # @example
    #   Finrb::Utils.sf_ratio(rp=0.09,rl=0.03,sd=0.12)
    def self.sf_ratio(rp:, rl:, sd:)
      rp = DecNum(rp.to_s)
      rl = DecNum(rl.to_s)
      sd = DecNum(sd.to_s)

      ((rp - rl) / sd)
    end

    # Computing Sharpe Ratio
    #
    # @param rp portfolio return
    # @param rf risk-free return
    # @param sd standard deviation of portfolio retwns
    # @example
    #   Finrb::Utils.sharpe_ratio(rp=0.038,rf=0.015,sd=0.07)
    def self.sharpe_ratio(rp:, rf:, sd:)
      rp = DecNum(rp.to_s)
      rf = DecNum(rf.to_s)
      sd = DecNum(sd.to_s)

      ((rp - rf) / sd)
    end

    # Depreciation Expense Recognition -- Straight-line depreciation (SL) allocates an equal amount of depreciation each year over the asset's useful life
    #
    # @param cost cost of long-lived assets
    # @param rv   residual value of the long-lived assets at the end of its useful life
    # @param t    length of the useful life
    # @example
    #   Finrb::Utils.slde(cost=1200,rv=200,t=5)
    def self.slde(cost:, rv:, t:)
      cost = DecNum(cost.to_s)
      rv = DecNum(rv.to_s)
      t = DecNum(t.to_s)

      ((cost - rv) / t)
    end

    # total debt-to-equity -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param td total debt
    # @param te total equity
    # @example
    #   Finrb::Utils.total_d2e(td=6000,te=20000)
    def self.total_d2e(td:, te:)
      td = DecNum(td.to_s)
      te = DecNum(te.to_s)

      (td / te)
    end

    # Computing TWRR, the time-weighted rate of return
    #
    # @param ev ordered ending value list
    # @param bv ordered beginning value list
    # @param cfr ordered cash flow received list
    # @example
    #   Finrb::Utils.twrr(ev=[120,260],bv=[100,240],cfr=[2,4])
    def self.twrr(ev:, bv:, cfr:)
      ev = Array.wrap(ev).map { |value| DecNum(value.to_s) }
      bv = Array.wrap(bv).map { |value| DecNum(value.to_s) }
      cfr = Array.wrap(cfr).map { |value| DecNum(value.to_s) }

      r = ev.size
      s = bv.size
      t = cfr.size
      wr = 1
      if r != s || r != t || s != t
        raise(FinrbError, 'Different number of values!')
      else
        (0...r).each do |i|
          wr *= (Finrb::Utils.hpr(ev: ev[i], bv: bv[i], cfr: cfr[i]) + 1)
        end
        ((wr**(1.to_f / r)) - 1)
      end
    end

    # calculate weighted average shares -- weighted average number of common shares
    #
    # @param ns n x 1 vector vector of number of shares
    # @param nm n x 1 vector vector of number of months relate to ns
    # @example
    #   s=[10000,2000];m=[12,6];Finrb::Utils.was(ns=s,nm=m)
    #
    # @example
    #   s=[11000,4400,-3000];m=[12,9,4];Finrb::Utils.was(ns=s,nm=m)
    def self.was(ns:, nm:)
      ns = Array.wrap(ns).map { |value| DecNum(value.to_s) }
      nm = Array.wrap(nm).map { |value| DecNum(value.to_s) }

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
    # @example
    #   Finrb::Utils.wpr(r=[0.12, 0.07, 0.03],w=[0.5,0.4,0.1])
    def self.wpr(r:, w:)
      r = Array.wrap(r).map { |value| DecNum(value.to_s) }
      w = Array.wrap(w).map { |value| DecNum(value.to_s) }

      if w.sum != 1
        puts('sum of weights is NOT equal to 1!') # TODO: need to change
      end
      r.zip(w).sum { |arr| arr.reduce(:*) }
    end
  end
end
