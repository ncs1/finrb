# frozen_string_literal: true

require_relative 'config'
require_relative 'decimal'
require_relative 'errors'
require_relative 'numerical/brent'
require_relative 'numerical/rate_search'
require_relative 'validation'

module Finrb
  # Time-value-of-money calculations for periodic rates and cashflows.
  module TVM
    module_function

    UNSET_BOUND = Object.new.freeze
    private_constant :UNSET_BOUND

    def discount_rate(n:, pv:, fv:, pmt:, type: 0, guess: nil, lower: UNSET_BOUND, upper: UNSET_BOUND)
      n = period_count(n)
      pv, fv, pmt = decimal_inputs(pv:, fv:, pmt:).values
      type = payment_type(type)
      function = ->(rate) { fv_simple(r: rate, n:, pv:) + fv_annuity(r: rate, n:, pmt:, type:) - fv }

      bounds = rate_bounds(function, guess:, lower:, upper:)
      return bounds.first if bounds.first == bounds.last

      Numerical::Brent.new(tolerance: Finrb.config.eps).solve(function, lower: bounds.first, upper: bounds.last)
    end

    def fv(r:, n:, pv: 0, pmt: 0, type: 0)
      rate = periodic_rate(r)
      periods = period_count(n)
      payment_type(type)

      fv_simple(r: rate, n: periods, pv:) + fv_annuity(r: rate, n: periods, pmt:, type:)
    end

    def fv_annuity(r:, n:, pmt:, type: 0)
      rate = periodic_rate(r)
      periods = period_count(n)
      payment = Validation.decimal(pmt, name: 'pmt')
      payment_timing = payment_type(type)
      return -payment * periods if rate.zero?

      (payment / rate * (((rate + 1)**periods) - 1)) * ((rate + 1)**payment_timing) * -1
    end

    def fv_simple(r:, n:, pv:)
      rate = periodic_rate(r)
      periods = period_count(n)
      present_value = Validation.decimal(pv, name: 'pv')
      (present_value * ((rate + 1)**periods)) * -1
    end

    def fv_uneven(r:, cf:)
      rate = periodic_rate(r)
      cashflows = cashflow_values(cf)

      cashflows.each_with_index.sum do |cashflow, index|
        fv_simple(r: rate, n: cashflows.size - index - 1, pv: cashflow)
      end
    end

    def n_period(r:, pv:, fv:, pmt:, type: 0)
      rate = periodic_rate(r)
      values = decimal_inputs(pv:, fv:, pmt:)
      payment_timing = payment_type(type)

      return zero_rate_periods(**values) if rate.zero?

      numerator = ((values[:fv] * rate) - (values[:pmt] * ((rate + 1)**payment_timing))) * -1
      denominator = (values[:pv] * rate) + (values[:pmt] * ((rate + 1)**payment_timing))
      periods = (numerator / denominator).log / (rate + 1).log
      raise(DomainError, 'Inputs do not produce a finite non-negative period count.') unless periods.finite? && !periods.negative?

      periods
    rescue Flt::Num::Exception, Math::DomainError, ZeroDivisionError => e
      raise(DomainError, "Inputs do not produce a real period count: #{e.message}", e.backtrace)
    end

    def npv(r:, cf:)
      rate = periodic_rate(r)
      cashflows = cashflow_values(cf)
      return cashflows.first if cashflows.one?

      (pv_uneven(r: rate, cf: cashflows.drop(1)) * -1) + cashflows.first
    end

    def pmt(r:, n:, pv:, fv:, type: 0)
      rate = periodic_rate(r)
      periods = positive_period_count(n)
      values = decimal_inputs(pv:, fv:)
      payment_timing = payment_type(type)
      return -(values[:pv] + values[:fv]) / periods if rate.zero?

      (values[:pv] + (values[:fv] / ((rate + 1)**periods))) * rate / (1 - (Flt::DecNum(1) / ((rate + 1)**periods))) * -1 * ((rate + 1)**(payment_timing * -1))
    end

    def pv(r:, n:, fv: 0, pmt: 0, type: 0)
      rate = periodic_rate(r)
      periods = period_count(n)
      payment_type(type)

      pv_simple(r: rate, n: periods, fv:) + pv_annuity(r: rate, n: periods, pmt:, type:)
    end

    def pv_annuity(r:, n:, pmt:, type: 0)
      rate = periodic_rate(r)
      periods = period_count(n)
      payment = Validation.decimal(pmt, name: 'pmt')
      payment_timing = payment_type(type)
      return -payment * periods if rate.zero?

      (payment / rate * (1 - (Flt::DecNum(1) / ((rate + 1)**periods)))) * ((rate + 1)**payment_timing) * -1
    end

    def pv_perpetuity(r:, pmt:, g: 0, type: 0)
      rate = periodic_rate(r)
      payment = Validation.decimal(pmt, name: 'pmt')
      growth = periodic_rate(g, name: :g)
      payment_timing = payment_type(type)
      raise(DomainError, 'Growth rate must be smaller than the discount rate.') if growth >= rate

      (payment / (rate - growth)) * ((rate + 1)**payment_timing) * -1
    end

    def pv_simple(r:, n:, fv:)
      rate = periodic_rate(r)
      periods = period_count(n)
      future_value = Validation.decimal(fv, name: 'fv')
      (future_value / ((rate + 1)**periods)) * -1
    end

    def pv_uneven(r:, cf:)
      rate = periodic_rate(r)
      cashflow_values(cf).each_with_index.sum do |cashflow, index|
        pv_simple(r: rate, n: index + 1, fv: cashflow)
      end
    end

    def r_perpetuity(pmt:, pv:)
      payment = Validation.decimal(pmt, name: 'pmt')
      present_value = Validation.decimal(pv, name: 'pv')
      raise(DomainError, 'Present value must be non-zero.') if present_value.zero?

      payment * -1 / present_value
    end

    def cashflow_values(value)
      values =
        if value.nil?
          []
        elsif value.respond_to?(:to_ary)
          value.to_ary || [value]
        else
          [value]
        end
      raise(ArgumentError, 'cf cannot be empty.') if values.empty?

      values.map { |cashflow| Validation.decimal(cashflow, name: 'cashflow') }
    end
    private_class_method :cashflow_values

    def decimal_inputs(**values)
      values.to_h { |name, value| [name, Validation.decimal(value, name: name.to_s)] }
    end
    private_class_method :decimal_inputs

    def payment_type(value)
      value = Validation.decimal(value, name: 'type')
      raise(ArgumentError, 'type must be 0 or 1.') unless [Flt::DecNum(0), Flt::DecNum(1)].include?(value)

      value
    end
    private_class_method :payment_type

    def period_count(value)
      Validation.non_negative_decimal(value, name: 'n', error: DomainError, message: 'n must be non-negative.')
    end
    private_class_method :period_count

    def positive_period_count(value)
      Validation.positive_decimal(value, name: 'n', error: DomainError)
    end
    private_class_method :positive_period_count

    def periodic_rate(value, name: :r)
      Validation.decimal_greater_than(value, minimum: -1, name: name.to_s, error: DomainError)
    end
    private_class_method :periodic_rate

    def rate_bounds(function, guess:, lower:, upper:)
      return searched_rate_bounds(function, guess) if lower.equal?(UNSET_BOUND) && upper.equal?(UNSET_BOUND)

      lower = '0.0001' if lower.equal?(UNSET_BOUND)
      upper = 100 if upper.equal?(UNSET_BOUND)
      lower = periodic_rate(lower, name: :lower)
      upper = periodic_rate(upper, name: :upper)
      raise(ArgumentError, 'lower must be less than upper.') if lower >= upper

      [lower, upper]
    end
    private_class_method :rate_bounds

    def searched_rate_bounds(function, guess)
      guess = Finrb.config.guess if guess.nil?
      Numerical::RateSearch.new.bracket(function, guess: periodic_rate(guess, name: :guess))
    end
    private_class_method :searched_rate_bounds

    def zero_rate_periods(pv:, fv:, pmt:)
      raise(DomainError, 'pmt must be non-zero when solving periods at a zero rate.') if pmt.zero?

      periods = (-pv - fv) / pmt
      raise(DomainError, 'Inputs do not produce a non-negative period count.') if periods.negative?

      periods
    end
    private_class_method :zero_rate_periods
  end
end
