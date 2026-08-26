# frozen_string_literal: true

require_relative 'decimal'
require_relative 'errors'
require_relative 'validation'

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
      cash = Validation.decimal(cash, name: 'cash')
      ms = Validation.decimal(ms, name: 'marketable securities')
      cl = Validation.non_zero_decimal(cl, name: 'current liabilities', error: DomainError)

      ((cash + ms) / cl)
    end

    # current ratio -- Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
    #
    # @param ca current assets
    # @param cl current liabilities
    # @example
    #   Finrb::Ratios.current_ratio(ca=8000,cl=2000)
    def self.current_ratio(ca:, cl:)
      ca = Validation.decimal(ca, name: 'current assets')
      cl = Validation.non_zero_decimal(cl, name: 'current liabilities', error: DomainError)

      (ca / cl)
    end

    # debt ratio -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param td total debt
    # @param ta total assets
    # @example
    #   Finrb::Ratios.debt_ratio(td=6000,ta=20000)
    def self.debt_ratio(td:, ta:)
      td = Validation.decimal(td, name: 'total debt')
      ta = Validation.non_zero_decimal(ta, name: 'total assets', error: DomainError)

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
      ni = Validation.decimal(ni, name: 'net income')
      pd = Validation.decimal(pd, name: 'preferred dividends')
      w = Validation.positive_decimal(w, name: 'weighted average common shares', error: DomainError)
      cpd = Validation.non_negative_decimal(cpd, name: 'convertible preferred dividends')
      cdi = Validation.non_negative_decimal(cdi, name: 'convertible debt interest')
      tax = Validation.decimal_between(tax, minimum: 0, maximum: 1, name: 'tax rate')
      cps = Validation.non_negative_decimal(cps, name: 'convertible preferred shares')
      cds = Validation.non_negative_decimal(cds, name: 'convertible debt shares')
      iss = Validation.non_negative_decimal(iss, name: 'incremental option shares')

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
      ni = Validation.decimal(ni, name: 'net income')
      pd = Validation.decimal(pd, name: 'preferred dividends')
      w = Validation.positive_decimal(w, name: 'weighted average common shares', error: DomainError)

      ((ni - pd) / w)
    end

    # financial leverage -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param te total equity
    # @param ta total assets
    # @example
    #   Finrb::Ratios.financial_leverage(te=16000,ta=20000)
    def self.financial_leverage(te:, ta:)
      te = Validation.non_zero_decimal(te, name: 'total equity', error: DomainError)
      ta = Validation.decimal(ta, name: 'total assets')

      (ta / te)
    end

    # gross profit margin -- Evaluate a company's financial performance
    #
    # @param gp gross profit, equal to revenue minus cost of goods sold (cogs)
    # @param rv revenue (sales)
    # @example
    #   Finrb::Ratios.gpm(gp=1000,rv=20000)
    def self.gpm(gp:, rv:)
      gp = Validation.decimal(gp, name: 'gross profit')
      rv = Validation.non_zero_decimal(rv, name: 'revenue', error: DomainError)

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
      amp = Validation.positive_decimal(amp, name: 'average market price', error: DomainError)
      ep = Validation.non_negative_decimal(ep, name: 'exercise price')
      n = Validation.non_negative_decimal(n, name: 'option shares')

      if amp > ep
        ((amp - ep) * n / amp)
      else
        raise(DomainError, 'Average market price must be greater than exercise price.')
      end
    end

    # long-term debt-to-equity -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param ltd long-term debt
    # @param te  total equity
    # @example
    #   Finrb::Ratios.lt_d2e(ltd=8000,te=20000)
    def self.lt_d2e(ltd:, te:)
      ltd = Validation.decimal(ltd, name: 'long-term debt')
      te = Validation.non_zero_decimal(te, name: 'total equity', error: DomainError)

      (ltd / te)
    end

    # net profit margin -- Evaluate a company's financial performance
    #
    # @param ni net income
    # @param rv revenue (sales)
    # @example
    #   Finrb::Ratios.npm(ni=8000,rv=20000)
    def self.npm(ni:, rv:)
      ni = Validation.decimal(ni, name: 'net income')
      rv = Validation.non_zero_decimal(rv, name: 'revenue', error: DomainError)

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
      cash = Validation.decimal(cash, name: 'cash')
      ms = Validation.decimal(ms, name: 'marketable securities')
      rc = Validation.decimal(rc, name: 'receivables')
      cl = Validation.non_zero_decimal(cl, name: 'current liabilities', error: DomainError)

      ((cash + ms + rc) / cl)
    end

    # total debt-to-equity -- Solvency ratios measure the firm's ability to satisfy its long-term obligations.
    #
    # @param td total debt
    # @param te total equity
    # @example
    #   Finrb::Ratios.total_d2e(td=6000,te=20000)
    def self.total_d2e(td:, te:)
      td = Validation.decimal(td, name: 'total debt')
      te = Validation.non_zero_decimal(te, name: 'total equity', error: DomainError)

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
      ns = wrap_array(ns).map { |value| Validation.decimal(value, name: 'share change') }
      nm = wrap_array(nm).map { |value| Validation.decimal_between(value, minimum: 0, maximum: 12, name: 'months outstanding') }

      m = ns.size
      n = nm.size
      sum = 0
      if m == n
        (0...m).each do |i|
          sum += (ns[i] * nm[i])
        end
      else
        raise(ArgumentError, 'Share changes and months outstanding must have equal lengths.')
      end
      sum /= 12
      sum
    end
  end
end
