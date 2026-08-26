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
      beginning_value = Validation.positive_decimal(beginning_value, name: 'beginning value')
      ending_value = Validation.non_negative_decimal(ending_value, name: 'ending value')
      periods = Validation.positive_integer(periods, name: 'period count')

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
      rate = Validation.decimal_at_least(rate, minimum: -1, name: 'periodic rate')
      periods_per_year = Validation.positive_integer(periods_per_year, name: 'periods per year')

      ((rate + 1)**periods_per_year) - 1
    end

    # Scale periodic volatility by the square root of periods per year.
    def self.annualize_volatility(volatility:, periods_per_year:)
      volatility = Validation.non_negative_decimal(volatility, name: 'volatility')
      periods_per_year = Validation.positive_integer(periods_per_year, name: 'periods per year')

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

      periods_per_year = Validation.positive_integer(periods_per_year, name: 'periods per year')
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
      sd = Validation.non_negative_decimal(sd, name: 'standard deviation')
      avg = Validation.decimal(avg, name: 'average')
      raise(DomainError, 'Average must be non-zero.') if avg.zero?

      (sd / avg)
    end

    # Geometric mean return
    #
    # @param r returns over multiple periods
    # @example
    #   Finrb::Returns.geometric_mean(r=[-0.0934, 0.2345, 0.0892])
    def self.geometric_mean(r:)
      returns = risk_values(r, name: 'return')
      returns.each do |value|
        raise(DomainError, 'Returns must be greater than or equal to -1.') if value < -1
      end

      growth_factors = returns.map { |value| value + 1 }
      ((growth_factors.reduce(:*)**(Flt::DecNum(1) / growth_factors.size)) - 1)
    end

    # harmonic mean, average price
    # @param p price over multiple periods
    # @example
    #   Finrb::Returns.harmonic_mean(p=[8,9,10])
    def self.harmonic_mean(p:)
      prices = risk_values(p, name: 'price')
      raise(DomainError, 'Prices must be greater than zero.') unless prices.all?(&:positive?)

      (Flt::DecNum(1) / (prices.sum { |price| Flt::DecNum(1) / price } / prices.size))
    end

    # Computing HPR, the holding period return
    #
    # @param ev ending value
    # @param bv beginning value
    # @param cfr cash flow received
    # @example
    #   Finrb::Returns.hpr(ev=33,bv=30,cfr=0.5)
    def self.hpr(ev:, bv:, cfr: 0)
      ev = Validation.decimal(ev, name: 'ending value')
      bv = Validation.positive_decimal(bv, name: 'beginning value', error: DomainError)
      cfr = Validation.decimal(cfr, name: 'cashflow received')

      ((ev - bv + cfr) / bv)
    end

    # Computing Sampling error
    #
    # @param sm sample mean
    # @param mu population mean
    # @example
    #   Finrb::Returns.sampling_error(sm=0.45, mu=0.5)
    def self.sampling_error(sm:, mu:)
      sm = Validation.decimal(sm, name: 'sample mean')
      mu = Validation.decimal(mu, name: 'population mean')

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
      rp = Validation.decimal(rp, name: 'portfolio return')
      rl = Validation.decimal(rl, name: 'threshold return')
      sd = Validation.positive_decimal(sd, name: 'standard deviation', error: DomainError)

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
      rp = Validation.decimal(rp, name: 'portfolio return')
      rf = Validation.decimal(rf, name: 'risk-free return')
      sd = Validation.positive_decimal(sd, name: 'standard deviation', error: DomainError)

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
      ending_values = risk_values(ev, name: 'ending value')
      beginning_values = risk_values(bv, name: 'beginning value')
      cashflows_received = risk_values(cfr, name: 'cashflow received')
      sizes = [ending_values.size, beginning_values.size, cashflows_received.size]
      raise(ArgumentError, 'Ending values, beginning values, and cashflows received must have equal lengths.') unless sizes.uniq.one?

      wealth_relative =
        ending_values.each_index.reduce(Flt::DecNum(1)) do |product, index|
          period_growth = hpr(ev: ending_values[index], bv: beginning_values[index], cfr: cashflows_received[index]) + 1
          raise(DomainError, 'Each subperiod wealth relative must be greater than or equal to zero.') if period_growth.negative?

          product * period_growth
        end
      (wealth_relative**(Flt::DecNum(1) / ending_values.size)) - 1
    end

    # Weighted mean as a portfolio return
    #
    # @param r returns of the individual assets in the portfolio
    # @param w corresponding weights associated with each of the individual assets
    # @example
    #   Finrb::Returns.wpr(r=[0.12, 0.07, 0.03],w=[0.5,0.4,0.1])
    def self.wpr(r:, w:)
      returns = risk_values(r, name: 'return')
      weights = risk_values(w, name: 'weight')
      raise(ArgumentError, 'Returns and weights must have equal lengths.') unless returns.size == weights.size
      raise(ArgumentError, 'Weights must sum to 1.') unless weights.sum == 1

      returns.zip(weights).sum { |rate, weight| rate * weight }
    end
  end
end
