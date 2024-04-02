# frozen_string_literal: true

require_relative 'decimal'

module Finrb
  # the Rate class provides an interface for working with interest rates.
  # {render:Rate#new}
  # @api public
  class Rate
    include Comparable
    # Accepted rate types
    TYPES = { apr: 'effective', apy: 'effective', effective: 'effective', nominal: 'nominal' }.freeze
    public_constant :TYPES

    # convert a nominal interest rate to an effective interest rate
    # @return [Flt::DecNum] the effective interest rate
    # @param [Numeric] rate the nominal interest rate
    # @param [Numeric] periods the number of compounding periods per year
    # @example
    #   Rate.to_effective(0.05, 4) #=> Flt::DecNum('0.05095')
    # @api public
    def self.to_effective(rate, periods)
      rate = Flt::DecNum.new(rate.to_s)
      periods = Flt::DecNum.new(periods.to_s)

      if periods.infinite?
        rate.exp - 1
      else
        (((rate / periods) + 1)**periods) - 1
      end
    end

    # convert an effective interest rate to a nominal interest rate
    # @return [Flt::DecNum] the nominal interest rate
    # @param [Numeric] rate the effective interest rate
    # @param [Numeric] periods the number of compounding periods per year
    # @example
    #   Rate.to_nominal(0.06, 365) #=> Flt::DecNum('0.05827')
    # @see https://www.miniwebtool.com/nominal-interest-rate-calculator/
    # @api public
    def self.to_nominal(rate, periods)
      rate = Flt::DecNum.new(rate.to_s)
      periods = Flt::DecNum.new(periods.to_s)

      if periods.infinite?
        (rate + 1).log
      else
        periods * (((rate + 1)**(1.to_f / periods)) - 1)
      end
    end

    # create a new Rate instance
    # @return [Rate]
    # @param [Numeric] rate the decimal value of the interest rate
    # @param [Symbol] type a valid {TYPES rate type}
    # @param [optional, Hash] opts set optional attributes
    # @option opts [String] :duration a time interval for which the rate is valid
    # @option opts [String] :compounds (:monthly) the number of compounding periods per year
    # @example create a 3.5% APR rate
    #   Rate.new(0.035, :apr) #=> Rate(0.035, :apr)
    # @see https://en.wikipedia.org/wiki/Effective_interest_rate
    # @see https://en.wikipedia.org/wiki/Nominal_interest_rate
    # @api public
    def initialize(rate, type, opts = {})
      # Default monthly compounding.
      opts = { compounds: :monthly }.merge(opts)

      # Set optional attributes..
      opts.each do |key, value|
        __send__(:"#{key}=", value)
      end

      # Set the rate in the proper way, based on the value of type.
      begin
        __send__(:"#{TYPES.fetch(type)}=", Flt::DecNum.new(rate.to_s))
      rescue KeyError
        raise(ArgumentError, "type must be one of #{TYPES.keys.join(', ')}", caller)
      end
    end

    # @return [Integer] the duration for which the rate is valid, in months
    # @api public
    attr_accessor :duration
    # @return [Flt::DecNum] the effective interest rate
    # @api public
    attr_reader :effective
    # @return [Flt::DecNum] the nominal interest rate
    # @api public
    attr_reader :nominal

    # compare two Rates, using the effective rate
    # @return [Numeric] one of -1, 0, +1
    # @param [Rate] other the comparison Rate
    # @example Which is better, a nominal rate of 15% compounded monthly, or 15.5% compounded semiannually?
    #   r1 = Rate.new(0.15, :nominal) #=> Rate.new(0.160755, :apr)
    #   r2 = Rate.new(0.155, :nominal, :compounds => :semiannually) #=> Rate.new(0.161006, :apr)
    #   r1 <=> r2 #=> -1
    # @api public
    def <=>(other)
      @effective <=> other.effective
    end

    # (see #effective)
    # @api public
    def apr
      effective
    end

    # (see #effective)
    # @api public
    def apy
      effective
    end

    # a convenience method which sets the value of @periods
    # @return none
    # @param [Symbol, Numeric] input the compounding frequency
    # @raise [ArgumentError] if input is not an accepted keyword or Numeric
    # @api private
    def compounds=(input)
      @periods =
        case input
        when :annually     then Flt::DecNum.new(1)
        when :continuously then Flt::DecNum.infinity
        when :daily        then Flt::DecNum.new(365)
        when :monthly      then Flt::DecNum.new(12)
        when :quarterly    then Flt::DecNum.new(4)
        when :semiannually then Flt::DecNum.new(2)
        when Numeric       then Flt::DecNum.new(input.to_s)
        else raise(ArgumentError)
        end
    end

    # set the effective interest rate
    # @return none
    # @param [Flt::DecNum] rate the effective interest rate
    # @api private
    def effective=(rate)
      @effective = rate
      @nominal = Rate.to_nominal(rate, @periods)
    end

    def inspect
      "Rate.new(#{apr.round(6)}, :apr)"
    end

    # @return [Flt::DecNum] the monthly effective interest rate
    # @example
    #   rate = Rate.new(0.15, :nominal)
    #   rate.apr.round(6) #=> Flt::DecNum('0.160755')
    #   rate.monthly.round(6) #=> Flt::DecNum('0.013396')
    # @api public
    def monthly
      (effective / 12).round(15)
    end

    # set the nominal interest rate
    # @return none
    # @param [Flt::DecNum] rate the nominal interest rate
    # @api private
    def nominal=(rate)
      @nominal = rate
      @effective = Rate.to_effective(rate, @periods)
    end

    private :compounds=, :effective=, :nominal=
  end
end
