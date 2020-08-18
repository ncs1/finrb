# frozen_string_literal: true

require_relative 'decimal'
require_relative 'rates'

require 'bigdecimal'
require 'bigdecimal/newton'
require 'business_time'
include Newton

module Finance
  # Provides methods for working with cash flows (collections of transactions)
  # @api public
  module Cashflow
    # Base class for working with Newton's Method.
    # @api private
    class Function
      values = {
        eps: Finance.config.eps,
        one: '1.0',
        two: '2.0',
        ten: '10.0',
        zero: '0.0'
      }

      values.each do |key, value|
        define_method key do
          BigDecimal(value)
        end
      end

      def initialize(transactions, function)
        @transactions = transactions
        @function = function
      end

      def values(x)
        value = @transactions.send(@function, Flt::DecNum.new(x[0].to_s))
        begin
          [BigDecimal(value.to_s)]
        rescue ArgumentError
          [0]
        end
      end
    end

    # calculate the internal rate of return for a sequence of cash flows
    # @return [DecNum] the internal rate of return
    # @param [Numeric] Initial guess rate, Defaults to 1.0
    # @example
    #   [-4000,1200,1410,1875,1050].irr #=> 0.143
    # @see http://en.wikipedia.org/wiki/Internal_rate_of_return
    # @api public
    def irr(guess = nil)
      # Make sure we have a valid sequence of cash flows.
      positives, negatives = partition { |i| i >= 0 }
      if positives.empty? || negatives.empty?
        raise ArgumentError, 'Calculation does not converge.'
      end

      func = Function.new(self, :npv)
      rate = [valid(guess)]
      nlsolve(func, rate)
      rate[0]
    end

    def method_missing(name, *args, &block)
      return inject(:+) if name.to_s == 'sum'
      super
    end

    # calculate the net present value of a sequence of cash flows
    # @return [DecNum] the net present value
    # @param [Numeric] rate the discount rate to be applied
    # @example
    #   [-100.0, 60, 60, 60].npv(0.1) #=> 49.211
    # @see http://en.wikipedia.org/wiki/Net_present_value
    # @api public
    def npv(rate)
      cashflows = collect { |entry| Flt::DecNum.new(entry.to_s) }

      rate = Flt::DecNum.new(rate.to_s)
      total = Flt::DecNum.new(0.to_s)
      cashflows.each_with_index do |cashflow, index|
        total += cashflow / (1 + rate)**index
      end

      total
    end

    # calculate the internal rate of return for a sequence of cash flows with dates
    # @param[Numeric] Initial guess rate, Deafults to 1.0
    # @return [Rate] the internal rate of return
    # @example
    #   @transactions = []
    #   @transactions << Transaction.new(-1000, :date => Time.new(1985,01,01))
    #   @transactions << Transaction.new(  600, :date => Time.new(1990,01,01))
    #   @transactions << Transaction.new(  600, :date => Time.new(1995,01,01))
    #   @transactions.xirr(0.6) #=> Rate("0.024851", :apr, :compounds => :annually)
    # @api public
    def xirr(guess = nil)
      # Make sure we have a valid sequence of cash flows.
      positives, negatives = partition { |t| t.amount >= 0 }
      if positives.empty? || negatives.empty?
        raise ArgumentError, 'Calculation does not converge. Cashflow needs to have a least one positive and one negative value.'
      end

      func = Function.new(self, :xnpv)
      rate = [valid(guess)]
      nlsolve(func, rate)
      Rate.new(rate[0], :apr, compounds: Finance.config.periodic_compound ? :continuously : :annually)
    end

    # calculate the net present value of a sequence of cash flows
    # @return [DecNum]
    # @example
    #   @transactions = []
    #   @transactions << Transaction.new(-1000, :date => Time.new(1985,01,01))
    #   @transactions << Transaction.new(  600, :date => Time.new(1990,01,01))
    #   @transactions << Transaction.new(  600, :date => Time.new(1995,01,01))
    #   @transactions.xnpv(0.6).round(2) #=> -937.41
    # @api public
    def xnpv(rate)
      rate = BigDecimal(rate.to_s)

      sum do |t|
        BigDecimal(t.amount.to_s) / ((1 + rate)**(date_diff(start, t.date) / days_in_period))
      end
    end

    private

    def date_diff(from, to)
      if Finance.config.business_days
        from.to_date.business_days_until(to)
      else
        to - from
      end
    end

    def days_in_period
      if Finance.config.periodic_compound && Finance.config.business_days
        start.to_date.business_days_until(stop).to_f
      else
        Flt::DecNum.new(365.days.to_s)
      end
    end

    def start
      @start ||= self[0].date
    end

    def stop
      @stop ||= self[-1].date.to_date
    end

    def valid(guess)
      if guess.nil?
        raise ArgumentError, 'Invalid Guess. Default guess should be a [Numeric] value.' unless Finance.config.guess.is_a? Numeric
        Finance.config.guess
      else
        raise ArgumentError, 'Invalid Guess. Use a [Numeric] value.' unless guess.is_a? Numeric
        guess
      end.to_f
    end
  end
end

class Array
  include Finance::Cashflow
end
