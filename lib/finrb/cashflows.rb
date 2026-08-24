# frozen_string_literal: true

require_relative 'config'
require_relative 'decimal'
require_relative 'errors'
require_relative 'numerical/brent'
require_relative 'numerical/rate_search'
require_relative 'rates'
require_relative 'validation'

require 'date'

module Finrb
  # Provides methods for working with cash flows (collections of transactions)
  # @api public
  module Cashflow
    class << self
      def irr(cashflows, guess = nil)
        sequence(cashflows).irr(guess)
      end

      def npv(cashflows, rate)
        sequence(cashflows).npv(rate)
      end

      def mirr(cashflows, finance_rate:, reinvestment_rate:)
        sequence(cashflows).mirr(finance_rate:, reinvestment_rate:)
      end

      def xirr(transactions, guess = nil)
        sequence(transactions).xirr(guess)
      end

      def xnpv(transactions, rate)
        sequence(transactions).xnpv(rate)
      end

      private

      def sequence(cashflows)
        raise(ArgumentError, 'cashflows must be an enumerable collection') unless cashflows.respond_to?(:to_a)

        cashflows.to_a.extend(Finrb::Cashflow)
      end
    end

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
    #   Finrb::Cashflow.irr([-4000,1200,1410,1875,1050]) #=> 0.143
    # @see https://en.wikipedia.org/wiki/Internal_rate_of_return
    # @api public
    def irr(guess = nil)
      validate_numeric_cashflows!

      # Make sure we have a valid sequence of cash flows.
      raise(InvalidCashflowError, 'Cashflow needs at least one positive and one negative value.') if none?(&:positive?) || none?(&:negative?)

      solve(:npv, valid(guess))
    end

    # calculate the net present value of a sequence of cash flows
    # @return [Flt::DecNum] the net present value
    # @param [Numeric] rate the discount rate to be applied
    # @example
    #   Finrb::Cashflow.npv([-100.0, 60, 60, 60], 0.1) #=> 49.211
    # @see https://en.wikipedia.org/wiki/Net_present_value
    # @api public
    def npv(rate)
      validate_numeric_cashflows!
      cashflows = map { |entry| Validation.decimal(entry, name: 'cashflow amount') }

      rate = Validation.decimal(rate, name: 'rate')
      raise(DomainError, 'Rate must be greater than -1.') if rate <= -1

      total = Flt::DecNum.new(0.to_s)
      cashflows.each_with_index do |cashflow, index|
        total += cashflow / ((rate + 1)**index)
      end

      total
    end

    # Calculate the modified internal rate of return for equally spaced
    # cashflows using separate financing and reinvestment assumptions.
    # @return [Flt::DecNum] modified per-period internal rate of return
    def mirr(finance_rate:, reinvestment_rate:)
      validate_numeric_cashflows!
      raise(InvalidCashflowError, 'MIRR requires at least two cashflows.') if size < 2

      cashflows = map { |entry| Validation.decimal(entry, name: 'cashflow amount') }
      raise(InvalidCashflowError, 'Cashflow needs at least one positive and one negative value.') if cashflows.none?(&:positive?) || cashflows.none?(&:negative?)

      finance_rate = Validation.decimal(finance_rate, name: 'finance_rate')
      reinvestment_rate = Validation.decimal(reinvestment_rate, name: 'reinvestment_rate')
      raise(DomainError, 'Finance and reinvestment rates must be greater than -1.') if finance_rate <= -1 || reinvestment_rate <= -1

      last_period = cashflows.size - 1
      future_positive =
        cashflows.each_with_index.sum do |amount, index|
          amount.positive? ? amount * ((reinvestment_rate + 1)**(last_period - index)) : Flt::DecNum(0)
        end
      present_negative =
        cashflows.each_with_index.sum do |amount, index|
          amount.negative? ? amount / ((finance_rate + 1)**index) : Flt::DecNum(0)
        end

      ((future_positive / -present_negative)**(Flt::DecNum(1) / last_period)) - 1
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
    #   Finrb::Cashflow.xirr(@transactions, 0.6) #=> Rate("0.024851", :effective, :compounds => :annually)
    # @api public
    def xirr(guess = nil)
      validate_dated_cashflows!

      # Make sure we have a valid sequence of cash flows.
      raise(InvalidCashflowError, 'Cashflow needs at least one positive and one negative value.') if none? { |transaction| transaction.amount.positive? } || none? { |transaction| transaction.amount.negative? }

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
    #   Finrb::Cashflow.xnpv(@transactions, 0.6).round(2) #=> -937.41
    # @api public
    def xnpv(rate)
      validate_dated_cashflows!
      rate = Validation.decimal(rate, name: 'rate')
      raise(DomainError, 'Rate must be greater than -1.') if rate <= -1

      sum do |t|
        t.amount / ((rate + 1)**(date_diff(start, t.date) / days_in_period))
      end
    end

    private

    def validate_numeric_cashflows!
      raise(InvalidCashflowError, 'Cashflow cannot be empty.') if empty?

      each { |amount| Validation.decimal(amount, name: 'cashflow amount') }
    rescue ArgumentError => e
      raise(InvalidCashflowError, e.message, e.backtrace)
    end

    def validate_dated_cashflows!
      raise(InvalidCashflowError, 'Dated cashflow cannot be empty.') if empty?
      raise(InvalidCashflowError, 'Dated cashflows require Finrb::Transaction instances with dates.') unless all? { |transaction| transaction.is_a?(Transaction) && transaction.date.respond_to?(:to_date) }
      raise(InvalidCashflowError, 'Dated cashflows must be in chronological order.') unless each_cons(2).all? { |left, right| left.date.to_date <= right.date.to_date }
    end

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
