# frozen_string_literal: true

require_relative 'config'
require_relative 'decimal'
require_relative 'errors'
require_relative 'numerical/brent'

module Finrb
  # Time-value-of-money calculations for periodic rates and cashflows.
  module TVM
    module_function

    def discount_rate(n:, pv:, fv:, pmt:, type: 0, lower: 0.0001, upper: 100)
      n, pv, fv, pmt, type, lower, upper = decimals(n, pv, fv, pmt, type, lower, upper)
      function = ->(rate) { fv_simple(r: rate, n:, pv:) + fv_annuity(r: rate, n:, pmt:, type:) - fv }

      Numerical::Brent.new(tolerance: Finrb.config.eps).solve(function, lower:, upper:)
    end

    def fv(r:, n:, pv: 0, pmt: 0, type: 0)
      r, n, pv, pmt, type = decimals(r, n, pv, pmt, type)
      validate_payment_type!(type)

      fv_simple(r:, n:, pv:) + fv_annuity(r:, n:, pmt:, type:)
    end

    def fv_annuity(r:, n:, pmt:, type: 0)
      r, n, pmt, type = decimals(r, n, pmt, type)
      validate_payment_type!(type)

      (pmt / r * (((r + 1)**n) - 1)) * ((r + 1)**type) * -1
    end

    def fv_simple(r:, n:, pv:)
      r, n, pv = decimals(r, n, pv)
      (pv * ((r + 1)**n)) * -1
    end

    def fv_uneven(r:, cf:)
      r = Flt::DecNum(r.to_s)
      cashflows = array(cf).map { |value| Flt::DecNum(value.to_s) }

      cashflows.each_with_index.sum do |cashflow, index|
        fv_simple(r:, n: cashflows.size - index - 1, pv: cashflow)
      end
    end

    def n_period(r:, pv:, fv:, pmt:, type: 0)
      r, pv, fv, pmt, type = decimals(r, pv, fv, pmt, type)
      validate_payment_type!(type)

      numerator = ((fv * r) - (pmt * ((r + 1)**type))) * -1
      denominator = (pv * r) + (pmt * ((r + 1)**type))
      (numerator / denominator).log / (r + 1).log
    end

    def npv(r:, cf:)
      cashflows = array(cf).map { |value| Flt::DecNum(value.to_s) }
      (pv_uneven(r:, cf: cashflows.drop(1)) * -1) + cashflows.first
    end

    def pmt(r:, n:, pv:, fv:, type: 0)
      r, n, pv, fv, type = decimals(r, n, pv, fv, type)
      validate_payment_type!(type)

      (pv + (fv / ((r + 1)**n))) * r / (1 - (Flt::DecNum(1) / ((r + 1)**n))) * -1 * ((r + 1)**(type * -1))
    end

    def pv(r:, n:, fv: 0, pmt: 0, type: 0)
      r, n, fv, pmt, type = decimals(r, n, fv, pmt, type)
      validate_payment_type!(type)

      pv_simple(r:, n:, fv:) + pv_annuity(r:, n:, pmt:, type:)
    end

    def pv_annuity(r:, n:, pmt:, type: 0)
      r, n, pmt, type = decimals(r, n, pmt, type)
      validate_payment_type!(type)

      (pmt / r * (1 - (Flt::DecNum(1) / ((r + 1)**n)))) * ((r + 1)**type) * -1
    end

    def pv_perpetuity(r:, pmt:, g: 0, type: 0)
      r, pmt, g, type = decimals(r, pmt, g, type)
      validate_payment_type!(type)
      raise(Error, 'Error: g is not smaller than r!') if g >= r

      (pmt / (r - g)) * ((r + 1)**type) * -1
    end

    def pv_simple(r:, n:, fv:)
      r, n, fv = decimals(r, n, fv)
      (fv / ((r + 1)**n)) * -1
    end

    def pv_uneven(r:, cf:)
      r = Flt::DecNum(r.to_s)
      array(cf).each_with_index.sum do |cashflow, index|
        pv_simple(r:, n: index + 1, fv: cashflow)
      end
    end

    def r_perpetuity(pmt:, pv:)
      pmt, pv = decimals(pmt, pv)
      pmt * -1 / pv
    end

    def array(value)
      return [] if value.nil?
      return value.to_ary || [value] if value.respond_to?(:to_ary)

      [value]
    end
    private_class_method :array

    def decimals(*values)
      values.map { |value| Flt::DecNum(value.to_s) }
    end
    private_class_method :decimals

    def validate_payment_type!(type)
      raise(Error, 'Error: type should be 0 or 1!') unless [Flt::DecNum(0), Flt::DecNum(1)].include?(type)
    end
    private_class_method :validate_payment_type!
  end
end
