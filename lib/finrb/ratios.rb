# frozen_string_literal: true

require_relative 'decimal'
require_relative 'errors'

module Finrb
  # Financial-statement, leverage, and per-share ratios.
  module Ratios
    def self.wrap_array(object)
      if object.nil?
        []
      elsif object.respond_to?(:to_ary)
        object.to_ary || [object]
      else
        [object]
      end
    end
    private_class_method :wrap_array

    # cash ratio -- Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
    #
    # @param cash cash
    # @param ms   marketable securities
    # @param cl   current liabilities
    # @example
    #   Finrb::Ratios.cash_ratio(cash=3000,ms=2000,cl=2000)
    def self.cash_ratio(cash:, ms:, cl:)
      cash = Flt::DecNum(cash.to_s)
      ms = Flt::DecNum(ms.to_s)
      cl = Flt::DecNum(cl.to_s)

      ((cash + ms) / cl)
    end

    # current ratio -- Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
    #
    # @param ca current assets
    # @param cl current liabilities
    # @example
    #   Finrb::Ratios.current_ratio(ca=8000,cl=2000)
    def self.current_ratio(ca:, cl:)
      ca = Flt::DecNum(ca.to_s)
      cl = Flt::DecNum(cl.to_s)

      (ca / cl)
    end

    # debt ratio -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param td total debt
    # @param ta total assets
    # @example
    #   Finrb::Ratios.debt_ratio(td=6000,ta=20000)
    def self.debt_ratio(td:, ta:)
      td = Flt::DecNum(td.to_s)
      ta = Flt::DecNum(ta.to_s)

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
    #   Finrb::Ratios.diluted_eps(ni=115600,pd=10000,cdi=42000,tax=0.4,w=200000,cds=60000)
    #
    # @example
    #   Finrb::Ratios.diluted_eps(ni=115600,pd=10000,cpd=10000,w=200000,cps=40000)
    #
    # @example
    #   Finrb::Ratios.diluted_eps(ni=115600,pd=10000,w=200000,iss=2500)
    #
    # @example
    #   Finrb::Ratios.diluted_eps(ni=115600,pd=10000,cpd=10000,cdi=42000,tax=0.4,w=200000,cps=40000,cds=60000,iss=2500)
    def self.diluted_eps(ni:, pd:, w:, cpd: 0, cdi: 0, tax: 0, cps: 0, cds: 0, iss: 0)
      ni = Flt::DecNum(ni.to_s)
      pd = Flt::DecNum(pd.to_s)
      w = Flt::DecNum(w.to_s)
      cpd = Flt::DecNum(cpd.to_s)
      cdi = Flt::DecNum(cdi.to_s)
      tax = Flt::DecNum(tax.to_s)
      cps = Flt::DecNum(cps.to_s)
      cds = Flt::DecNum(cds.to_s)
      iss = Flt::DecNum(iss.to_s)

      basic = (ni - pd) / w
      diluted = (ni - pd + cpd + (cdi * (1 - tax))) / (w + cps + cds + iss)
      diluted = (ni - pd + cpd) / (w + cps + iss) if diluted > basic
      diluted
    end

    # Basic Earnings Per Share
    #
    # @param ni net income
    # @param pd preferred dividends
    # @param w  weighted average number of common shares outstanding
    # @example
    #   Finrb::Ratios.eps(ni=10000,pd=1000,w=11000)
    def self.eps(ni:, pd:, w:)
      ni = Flt::DecNum(ni.to_s)
      pd = Flt::DecNum(pd.to_s)
      w = Flt::DecNum(w.to_s)

      ((ni - pd) / w)
    end

    # financial leverage -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param te total equity
    # @param ta total assets
    # @example
    #   Finrb::Ratios.financial_leverage(te=16000,ta=20000)
    def self.financial_leverage(te:, ta:)
      te = Flt::DecNum(te.to_s)
      ta = Flt::DecNum(ta.to_s)

      (ta / te)
    end

    # gross profit margin -- Evaluate a company's financial performance
    #
    # @param gp gross profit, equal to revenue minus cost of goods sold (cogs)
    # @param rv revenue (sales)
    # @example
    #   Finrb::Ratios.gpm(gp=1000,rv=20000)
    def self.gpm(gp:, rv:)
      gp = Flt::DecNum(gp.to_s)
      rv = Flt::DecNum(rv.to_s)

      (gp / rv)
    end

    # calculate the net increase in common shares from the potential exercise of stock options or warrants
    #
    # @param amp average market price over the year
    # @param ep  exercise price of the options or warrants
    # @param n   number of common shares that the options and warrants can be convened into
    # @example
    #   Finrb::Ratios.iss(amp=20,ep=15,n=10000)
    def self.iss(amp:, ep:, n:)
      amp = Flt::DecNum(amp.to_s)
      ep = Flt::DecNum(ep.to_s)
      n = Flt::DecNum(n.to_s)

      if amp > ep
        ((amp - ep) * n / amp)
      else
        raise(Error, 'amp must larger than ep')
      end
    end

    # long-term debt-to-equity -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param ltd long-term debt
    # @param te  total equity
    # @example
    #   Finrb::Ratios.lt_d2e(ltd=8000,te=20000)
    def self.lt_d2e(ltd:, te:)
      ltd = Flt::DecNum(ltd.to_s)
      te = Flt::DecNum(te.to_s)

      (ltd / te)
    end

    # net profit margin -- Evaluate a company's financial performance
    #
    # @param ni net income
    # @param rv revenue (sales)
    # @example
    #   Finrb::Ratios.npm(ni=8000,rv=20000)
    def self.npm(ni:, rv:)
      ni = Flt::DecNum(ni.to_s)
      rv = Flt::DecNum(rv.to_s)

      (ni / rv)
    end

    # quick ratio -- Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
    #
    # @param cash cash
    # @param ms   marketable securities
    # @param rc   receivables
    # @param cl   current liabilities
    # @example
    #   Finrb::Ratios.quick_ratio(cash=3000,ms=2000,rc=1000,cl=2000)
    def self.quick_ratio(cash:, ms:, rc:, cl:)
      cash = Flt::DecNum(cash.to_s)
      ms = Flt::DecNum(ms.to_s)
      rc = Flt::DecNum(rc.to_s)
      cl = Flt::DecNum(cl.to_s)

      ((cash + ms + rc) / cl)
    end

    # total debt-to-equity -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param td total debt
    # @param te total equity
    # @example
    #   Finrb::Ratios.total_d2e(td=6000,te=20000)
    def self.total_d2e(td:, te:)
      td = Flt::DecNum(td.to_s)
      te = Flt::DecNum(te.to_s)

      (td / te)
    end

    # calculate weighted average shares -- weighted average number of common shares
    #
    # @param ns n x 1 vector vector of number of shares
    # @param nm n x 1 vector vector of number of months relate to ns
    # @example
    #   s=[10000,2000];m=[12,6];Finrb::Ratios.was(ns=s,nm=m)
    #
    # @example
    #   s=[11000,4400,-3000];m=[12,9,4];Finrb::Ratios.was(ns=s,nm=m)
    def self.was(ns:, nm:)
      ns = wrap_array(ns).map { |value| Flt::DecNum(value.to_s) }
      nm = wrap_array(nm).map { |value| Flt::DecNum(value.to_s) }

      m = ns.size
      n = nm.size
      sum = 0
      if m == n
        (0...m).each do |i|
          sum += (ns[i] * nm[i])
        end
      else
        raise(Error, 'length of ns and nm must be equal')
      end
      sum /= 12
      sum
    end
  end
end
