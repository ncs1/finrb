# frozen_string_literal: true

require_relative 'decimal'
require_relative 'errors'
require_relative 'validation'

module Finrb
  # Investment return and risk-adjusted performance calculations.
  module Returns
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

    # Compound annual growth rate over a positive number of periods.
    #
    # Beginning value must be positive. Ending value may be zero, representing
    # a total loss, but cannot be negative because a fractional growth root
    # would not have a generally meaningful real-valued result.
    #
    # @param beginning_value [Numeric] value at the start of the measurement
    # @param ending_value [Numeric] value at the end of the measurement
    # @param periods [Integer] number of equal annual periods
    # @return [Flt::DecNum] compound growth rate per period
    def self.cagr(beginning_value:, ending_value:, periods:)
      beginning_value = Validation.positive_decimal(beginning_value, name: 'beginning_value')
      ending_value = Validation.non_negative_decimal(ending_value, name: 'ending_value')
      periods = Validation.positive_integer(periods, name: 'periods')

      ((ending_value / beginning_value)**(Flt::DecNum(1) / periods)) - 1
    end

    def self.risk_values(values, name:)
      values = wrap_array(values)
      raise(ArgumentError, "#{name} cannot be empty.") if values.empty?

      values.map { |value| Validation.decimal(value, name:) }
    end
    private_class_method :risk_values

    # Compound a periodic return into an annual return.
    def self.annualize_return(rate:, periods_per_year:)
      rate = Validation.decimal_at_least(rate, minimum: -1, name: 'rate')
      periods_per_year = Validation.positive_integer(periods_per_year, name: 'periods_per_year')

      ((rate + 1)**periods_per_year) - 1
    end

    # Scale periodic volatility by the square root of periods per year.
    def self.annualize_volatility(volatility:, periods_per_year:)
      volatility = Validation.non_negative_decimal(volatility, name: 'volatility')
      periods_per_year = Validation.positive_integer(periods_per_year, name: 'periods_per_year')

      volatility * (Flt::DecNum(periods_per_year)**Flt::DecNum('0.5'))
    end

    # Standard deviation of periodic returns. Sample volatility uses n - 1;
    # population volatility uses n.
    def self.volatility(returns:, sample: true)
      raise(ArgumentError, 'sample must be true or false.') unless [true, false].include?(sample)

      returns = risk_values(returns, name: 'return')
      raise(ArgumentError, 'sample volatility requires at least two returns.') if sample && returns.size < 2

      mean = returns.sum / returns.size
      denominator = sample ? returns.size - 1 : returns.size
      variance = returns.sum { |value| (value - mean)**2 } / denominator
      variance**Flt::DecNum('0.5')
    end

    # Root-mean-square return shortfall below a target return. The denominator
    # includes every observation, including returns at or above the target.
    def self.downside_deviation(returns:, target: 0)
      returns = risk_values(returns, name: 'return')
      target = Validation.decimal(target, name: 'target')
      squared_shortfalls =
        returns.sum do |value|
          shortfall = [value - target, Flt::DecNum(0)].min
          shortfall**2
        end

      (squared_shortfalls / returns.size)**Flt::DecNum('0.5')
    end

    # Sortino ratio using arithmetic mean excess return and downside deviation.
    def self.sortino_ratio(returns:, target: 0, periods_per_year: nil)
      returns = risk_values(returns, name: 'return')
      target = Validation.decimal(target, name: 'target')
      downside = downside_deviation(returns:, target:)
      raise(ArgumentError, 'downside deviation must be greater than zero.') if downside.zero?

      ratio = ((returns.sum / returns.size) - target) / downside
      return ratio if periods_per_year.nil?

      periods_per_year = Validation.positive_integer(periods_per_year, name: 'periods_per_year')
      ratio * (Flt::DecNum(periods_per_year)**Flt::DecNum('0.5'))
    end

    # Largest peak-to-trough decline as a non-negative fraction.
    def self.max_drawdown(values:)
      values = risk_values(values, name: 'value')
      raise(ArgumentError, 'values must be greater than zero.') unless values.all?(&:positive?)

      peak = values.first
      values.reduce(Flt::DecNum(0)) do |maximum, value|
        peak = value if value > peak
        [maximum, (peak - value) / peak].max
      end
    end

    # Computing Coefficient of variation
    #
    # @param sd standard deviation
    # @param avg average value
    # @example
    #   Finrb::Returns.coefficient_variation(sd=0.15,avg=0.39)
    def self.coefficient_variation(sd:, avg:)
      sd = Flt::DecNum(sd.to_s)
      avg = Flt::DecNum(avg.to_s)

      (sd / avg)
    end

    # Geometric mean return
    #
    # @param r returns over multiple periods
    # @example
    #   Finrb::Returns.geometric_mean(r=[-0.0934, 0.2345, 0.0892])
    def self.geometric_mean(r:)
      r = wrap_array(r).map { |value| Flt::DecNum(value.to_s) }

      rs = r.map { |value| value + 1 }
      ((rs.reduce(:*)**(Flt::DecNum(1) / rs.size)) - 1)
    end

    # harmonic mean, average price
    # @param p price over multiple periods
    # @example
    #   Finrb::Returns.harmonic_mean(p=[8,9,10])
    def self.harmonic_mean(p:)
      p = wrap_array(p).map { |value| Flt::DecNum(value.to_s) }

      (Flt::DecNum(1) / (p.sum { |val| Flt::DecNum(1) / val } / p.size))
    end

    # Computing HPR, the holding period return
    #
    # @param ev ending value
    # @param bv beginning value
    # @param cfr cash flow received
    # @example
    #   Finrb::Returns.hpr(ev=33,bv=30,cfr=0.5)
    def self.hpr(ev:, bv:, cfr: 0)
      ev = Flt::DecNum(ev.to_s)
      bv = Flt::DecNum(bv.to_s)
      cfr = Flt::DecNum(cfr.to_s)

      ((ev - bv + cfr) / bv)
    end

    # Computing Sampling error
    #
    # @param sm sample mean
    # @param mu population mean
    # @example
    #   Finrb::Returns.sampling_error(sm=0.45, mu=0.5)
    def self.sampling_error(sm:, mu:)
      sm = Flt::DecNum(sm.to_s)
      mu = Flt::DecNum(mu.to_s)

      (sm - mu)
    end

    # Computing Roy's safety-first ratio
    #
    # @param rp portfolio return
    # @param rl threshold level return
    # @param sd standard deviation of portfolio retwns
    # @example
    #   Finrb::Returns.sf_ratio(rp=0.09,rl=0.03,sd=0.12)
    def self.sf_ratio(rp:, rl:, sd:)
      rp = Flt::DecNum(rp.to_s)
      rl = Flt::DecNum(rl.to_s)
      sd = Flt::DecNum(sd.to_s)

      ((rp - rl) / sd)
    end

    # Computing Sharpe Ratio
    #
    # @param rp portfolio return
    # @param rf risk-free return
    # @param sd standard deviation of portfolio retwns
    # @example
    #   Finrb::Returns.sharpe_ratio(rp=0.038,rf=0.015,sd=0.07)
    def self.sharpe_ratio(rp:, rf:, sd:)
      rp = Flt::DecNum(rp.to_s)
      rf = Flt::DecNum(rf.to_s)
      sd = Flt::DecNum(sd.to_s)

      ((rp - rf) / sd)
    end

    # Computing TWRR, the time-weighted rate of return
    #
    # @param ev ordered ending value list
    # @param bv ordered beginning value list
    # @param cfr ordered cash flow received list
    # @example
    #   Finrb::Returns.twrr(ev=[120,260],bv=[100,240],cfr=[2,4])
    def self.twrr(ev:, bv:, cfr:)
      ev = wrap_array(ev).map { |value| Flt::DecNum(value.to_s) }
      bv = wrap_array(bv).map { |value| Flt::DecNum(value.to_s) }
      cfr = wrap_array(cfr).map { |value| Flt::DecNum(value.to_s) }

      r = ev.size
      s = bv.size
      t = cfr.size
      wr = Flt::DecNum(1)
      if r != s || r != t || s != t
        raise(Error, 'Different number of values!')
      else
        (0...r).each do |i|
          wr *= (Finrb::Returns.hpr(ev: ev[i], bv: bv[i], cfr: cfr[i]) + 1)
        end
        ((wr**(Flt::DecNum(1) / r)) - 1)
      end
    end

    # Weighted mean as a portfolio return
    #
    # @param r returns of the individual assets in the portfolio
    # @param w corresponding weights associated with each of the individual assets
    # @example
    #   Finrb::Returns.wpr(r=[0.12, 0.07, 0.03],w=[0.5,0.4,0.1])
    def self.wpr(r:, w:)
      r = wrap_array(r).map { |value| Flt::DecNum(value.to_s) }
      w = wrap_array(w).map { |value| Flt::DecNum(value.to_s) }

      # TODO: need to change
      puts('sum of weights is NOT equal to 1!') if w.sum != 1

      r.zip(w).sum { |arr| arr.reduce(:*) }
    end
  end
end
