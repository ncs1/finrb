# frozen_string_literal: true

require_relative 'config'
require_relative 'decimal'
require_relative 'errors'
require_relative 'numerical/brent'
require_relative 'numerical/rate_search'
require_relative 'rates'

require 'date'

module Finrb
  # Provides methods for working with cash flows (collections of transactions)
  # @api public
  module Cashflow
    # Calculate the per-period internal rate of return for an ordered sequence
    # of equally spaced cashflows.
    #
    # For cashflows with multiple sign-changing roots, the guess determines
    # which nearby root is selected. Rates must be greater than -1.
    # @return [Flt::DecNum] the per-period internal rate of return
    # @param [Numeric, nil] guess initial rate used for root selection; defaults
    #   to +Finrb.config.guess+
    # @raise [InvalidCashflowError] if the sequence lacks both cashflow signs
    # @raise [ArgumentError] if the guess is not numeric
    # @raise [DomainError] if the rate domain or function evaluation is invalid
    # @raise [ConvergenceError] if no root can be bracketed or solved
    # @example
    #   [-4000,1200,1410,1875,1050].irr #=> 0.143
    # @see https://en.wikipedia.org/wiki/Internal_rate_of_return
    # @api public
    def irr(guess = nil)
      # Make sure we have a valid sequence of cash flows.
      positives, negatives = partition { |i| i >= 0 }
      raise(InvalidCashflowError, 'Cashflow needs at least one positive and one negative value.') if positives.empty? || negatives.empty?

      solve(:npv, valid(guess))
    end

    def method_missing(name, *args, &)
      return sum if name.to_s == 'sum'

      super
    end

    def respond_to_missing?(name, include_private = false)
      name.to_s == 'sum' || super
    end

    # calculate the net present value of a sequence of cash flows
    # @return [Flt::DecNum] the net present value
    # @param [Numeric] rate the discount rate to be applied
    # @example
    #   [-100.0, 60, 60, 60].npv(0.1) #=> 49.211
    # @see https://en.wikipedia.org/wiki/Net_present_value
    # @api public
    def npv(rate)
      cashflows = map { |entry| Flt::DecNum.new(entry.to_s) }

      rate = Flt::DecNum.new(rate.to_s)
      total = Flt::DecNum.new(0.to_s)
      cashflows.each_with_index do |cashflow, index|
        total += cashflow / ((rate + 1)**index)
      end

      total
    end

    # Calculate the effective annual internal rate of return for an ordered
    # sequence of dated transactions.
    #
    # Under the default configuration, date offsets are actual calendar days
    # from the first transaction and a 365-day year is used. Transactions
    # should be supplied chronologically and their dates must respond to
    # +to_date+. For multiple roots, the guess determines which nearby root is
    # selected. Rates must be greater than -1.
    # @param [Numeric, nil] guess initial rate used for root selection; defaults
    #   to +Finrb.config.guess+
    # @return [Rate] the effective annual internal rate of return
    # @raise [InvalidCashflowError] if the sequence lacks both cashflow signs
    # @raise [ArgumentError] if the guess is not numeric
    # @raise [DomainError] if the rate domain or function evaluation is invalid
    # @raise [ConvergenceError] if no root can be bracketed or solved
    # @example
    #   @transactions = []
    #   @transactions << Transaction.new(-1000, :date => Time.new(1985,01,01))
    #   @transactions << Transaction.new(  600, :date => Time.new(1990,01,01))
    #   @transactions << Transaction.new(  600, :date => Time.new(1995,01,01))
    #   @transactions.xirr(0.6) #=> Rate("0.024851", :effective, :compounds => :annually)
    # @api public
    def xirr(guess = nil)
      # Make sure we have a valid sequence of cash flows.
      positives, negatives = partition { |t| t.amount >= 0 }
      raise(InvalidCashflowError, 'Cashflow needs at least one positive and one negative value.') if positives.empty? || negatives.empty?

      rate = solve(:xnpv, valid(guess))
      Rate.new(rate, :effective, compounds: Finrb.config.periodic_compound ? :continuously : :annually)
    end

    # calculate the net present value of a sequence of cash flows
    # @return [Flt::DecNum]
    # @example
    #   @transactions = []
    #   @transactions << Transaction.new(-1000, :date => Time.new(1985,01,01))
    #   @transactions << Transaction.new(  600, :date => Time.new(1990,01,01))
    #   @transactions << Transaction.new(  600, :date => Time.new(1995,01,01))
    #   @transactions.xnpv(0.6).round(2) #=> -937.41
    # @api public
    def xnpv(rate)
      rate = Flt::DecNum.new(rate.to_s)

      sum do |t|
        t.amount / ((rate + 1)**(date_diff(start, t.date) / days_in_period))
      end
    end

    private

    def date_diff(from, to)
      if Finrb.config.business_days
        business_days_between(from.to_date, to.to_date)
      else
        to.to_date - from.to_date
      end
    end

    def business_days_between(from, to)
      (from...to).count { |date| (1..5).cover?(date.wday) }
    end

    def days_in_period
      if Finrb.config.periodic_compound && Finrb.config.business_days
        business_days_between(start.to_date, stop).to_f
      else
        Flt::DecNum.new(365)
      end
    end

    def start
      @start ||= first.date
    end

    def solve(function, guess)
      rate_function = ->(rate) { public_send(function, rate) }
      bounds = Numerical::RateSearch.new.bracket(rate_function, guess:)
      return bounds.first if bounds.first == bounds.last

      Numerical::Brent.new(tolerance: Finrb.config.eps).solve(rate_function, lower: bounds.first, upper: bounds.last)
    end

    def stop
      @stop ||= last.date.to_date
    end

    def valid(guess)
      if guess.nil?
        raise(ArgumentError, 'Invalid Guess. Default guess should be a [Numeric] value.') unless Finrb.config.guess.is_a?(Numeric)

        Finrb.config.guess
      else
        raise(ArgumentError, 'Invalid Guess. Use a [Numeric] value.') unless guess.is_a?(Numeric)

        guess
      end.then { |value| Flt::DecNum.new(value.to_s) }
    end
  end
end

class Array
  include Finrb::Cashflow
end
