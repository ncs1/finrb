# frozen_string_literal: true

require_relative 'cashflows'
require_relative 'decimal'
require_relative 'precision'
require_relative 'transaction'
require_relative 'validation'

module Finrb
  # the Amortization class provides an interface for working with loan amortizations.
  # @example Borrow $250,000 under a 30 year, fixed-rate loan with a 4.25% APR
  #   rate = Rate.new(0.0425, :apr, :duration => (30 * 12))
  #   amortization = Finrb::Amortization.new(250000, rate)
  # @example Borrow $250,000 under a 30 year, adjustable rate loan, with an APR starting at 4.25%, and increasing by 1% every five years
  #   values = %w{ 0.0425 0.0525 0.0625 0.0725 0.0825 0.0925 }
  #   rates = values.collect { |value| Rate.new( value, :apr, :duration = (5 * 12) ) }
  #   arm = Amortization.new(250000, *rates)
  # @example Borrow $250,000 under a 30 year, fixed-rate loan with a 4.25% APR, but pay $150 extra each month
  #   rate = Rate.new(0.0425, :apr, :duration => (5 * 12))
  #   extra_payments = Finrb::Amortization.new(250000, rate){ |period| period.payment - 150 }
  # @api public
  class Amortization
    # Immutable breakdown of one amortization period. Payments retain finrb's
    # cashflow sign convention and are negative; the other monetary fields are
    # non-negative.
    class Entry
      ATTRIBUTES = %i[period opening_balance payment interest principal additional_payment balloon_payment closing_balance].freeze
      private_constant :ATTRIBUTES

      attr_reader(*ATTRIBUTES)

      def initialize(period:, opening_balance:, payment:, interest:, principal:, additional_payment:, balloon_payment:, closing_balance:)
        raise(ArgumentError, 'period must be a non-negative integer.') unless period.is_a?(Integer) && !period.negative?

        @period = period
        ATTRIBUTES.drop(1).each do |name|
          value = binding.local_variable_get(name)
          instance_variable_set("@#{name}", Validation.decimal(value, name: name.to_s))
        end
        freeze
      end

      def ==(other)
        other.instance_of?(self.class) && ATTRIBUTES.all? { |name| public_send(name) == other.public_send(name) }
      end
      alias eql? ==

      def hash
        attributes = ATTRIBUTES.map { |name| public_send(name) }
        attributes.hash
      end

      def to_h
        ATTRIBUTES.to_h { |name| [name, public_send(name)] }
      end
    end

    # @return [Flt::DecNum] the balance of the loan at the end of the amortization period (usually zero)
    # @api public
    attr_reader :balance
    # @return [Flt::DecNum] contractual principal settled as a balloon in the final period
    # @api public
    attr_reader :balloon
    # @return [Flt::DecNum] the required monthly payment.  For loans with more than one rate, returns nil
    # @api public
    attr_reader :payment
    # @return [Flt::DecNum] the principal amount of the loan
    # @api public
    attr_reader :principal
    # @return [Array] the interest rates used for calculating the amortization
    # @api public
    attr_reader :rates
    # @return [Array<Entry>] immutable period-by-period loan breakdown
    # @api public
    attr_reader :schedule

    # @return [Flt::DecNum] the periodic payment due on a loan
    # @param [Flt::DecNum] principal the initial amount of the loan or investment
    # @param [Rate] rate the applicable interest rate (per period)
    # @param [Integer] periods the number of periods needed for repayment
    # @note in most cases, you will probably want to use rate.monthly when calling this function outside of an Amortization instance.
    # @example
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   rate.duration #=> 360
    #   Amortization.payment(200000, rate.monthly, rate.duration) #=> Flt::DecNum('-926.23')
    # @see https://en.wikipedia.org/wiki/Amortization_calculator
    # @api public
    def self.payment(principal, rate, periods, balloon: 0)
      principal = Validation.decimal(principal, name: 'principal')
      raise(ArgumentError, 'principal must be positive.') unless principal.positive?

      balloon = Validation.decimal(balloon, name: 'balloon')
      raise(ArgumentError, 'balloon must be non-negative and no greater than principal.') unless balloon.between?(0, principal)

      rate = Validation.decimal(rate, name: 'rate')
      raise(ArgumentError, 'periodic rate must be greater than -1.') if rate <= -1

      periods = Validation.positive_integer(periods, name: 'periods')

      if rate.zero?
        # simplified formula to avoid division-by-zero when interest rate is zero
        -Precision.money((principal - balloon) / periods)
      else
        growth = (rate + 1)**periods
        -Precision.money(((principal * growth) - balloon) * rate / (growth - 1))
      end
    end

    # create a new Amortization instance
    # @return [Amortization]
    # @param [Flt::DecNum] principal the initial amount of the loan or investment
    # @param [Rate] rates the applicable interest rates
    # @param [Proc] block
    # @api public
    def initialize(principal, *rates, balloon: 0, &block)
      @principal = Validation.decimal(principal, name: 'principal')
      raise(ArgumentError, 'principal must be positive.') unless @principal.positive?

      @balloon = Validation.decimal(balloon, name: 'balloon')
      raise(ArgumentError, 'balloon must be non-negative and less than principal.') if @balloon.negative? || @balloon >= @principal
      raise(ArgumentError, 'at least one rate is required.') if rates.empty?
      raise(ArgumentError, 'rates must be Finrb::Rate instances.') unless rates.all?(Rate)
      raise(ArgumentError, 'every rate must have a duration.') if rates.any? { |rate| rate.duration.nil? }

      @rates     = rates
      @block     = block

      # compute the total duration from all of the rates.
      @periods = rates.sum(&:duration)
      @period  = 0

      compute
    end

    # compare two Amortization instances
    # @return [Numeric] -1, 0, or +1
    # @param [Amortization] other
    # @api public
    def ==(other)
      (principal == other.principal) && (balloon == other.balloon) && (rates == other.rates) && (payments == other.payments)
    end

    # @return [Array] the amount of any additional payments in each period
    # @example
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = Finrb::Amortization.new(300000, rate){ |payment| payment.amount-100}
    #   amt.additional_payments #=> [Flt::DecNum('-100.00'), Flt::DecNum('-100.00'), ... ]
    # @api public
    def additional_payments
      @transactions.filter_map { |trans| trans.difference if trans.payment? }
    end

    # amortize the balance of loan with the given interest rate
    # @return none
    # @param [Rate] rate the interest rate to use in the amortization
    # @api private
    def amortize(rate)
      # For the purposes of calculating a payment, the relevant time
      # period is the remaining number of periods in the loan, not
      # necessarily the duration of the rate itself.
      periods = @periods - @period
      amount = Amortization.payment(@balance, rate.monthly, periods, balloon: @balloon)

      pmt = Payment.new(amount, period: @period)
      pmt.modify(&@block) if @block
      raise(ArgumentError, 'payment modification must produce a negative amount.') unless pmt.amount.negative?

      rate.duration.to_i.times do
        # Do this first in case the balance is zero already.
        break if @balance.zero?

        # Compute and record interest on the outstanding balance.
        int = Precision.money(@balance * rate.monthly)
        interest = Interest.new(int, period: @period)
        @balance += interest.amount
        @transactions << interest.dup

        # Record payment.  Don't pay more than the outstanding balance.
        pmt.amount = -@balance if pmt.amount.abs > @balance
        @additional_by_period << [-pmt.difference, Flt::DecNum(0)].max
        @transactions << pmt.dup
        @balance += pmt.amount

        @period += 1
      end
    end

    # compute the amortization of the principal
    # @return none
    # @api private
    def compute
      @balance = @principal
      @transactions = []
      @additional_by_period = []

      @rates.each do |rate|
        amortize(rate)
      end

      # Add the residual balloon and any rounding remainder to the last payment.
      @balloon_by_period = Array.new(@additional_by_period.length, Flt::DecNum(0))
      if @balance.nonzero?
        @balloon_by_period[-1] = [@balloon, @balance].min
        @transactions.reverse.find(&:payment?).amount -= @balance
        @balance = 0
      end

      @payment = (payments.first if @rates.length == 1)

      @transactions.freeze
      @additional_by_period.freeze
      @balloon_by_period.freeze
      @schedule = build_schedule.freeze
    end

    # @return [Integer] the time required to pay off the loan, in months
    # @example In most cases, the duration is equal to the total duration of all rates
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = Finrb::Amortization.new(300000, rate)
    #   amt.duration #=> 360
    # @example Extra payments may reduce the duration
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = Finrb::Amortization.new(300000, rate){ |payment| payment.amount-100}
    #   amt.duration #=> 319
    # @api public
    def duration
      payments.length
    end

    # @api public
    def inspect
      "Amortization.new(#{@principal})"
    end

    # @return [Array] the amount of interest charged in each period
    # @example find the total cost of interest for a loan
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = Finrb::Amortization.new(300000, rate)
    #   amt.interest.sum #=> Flt::DecNum('200163.94')
    # @example find the total interest charges in the first six months
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = Finrb::Amortization.new(300000, rate)
    #   amt.interest[0,6].sum #=> Flt::DecNum('5603.74')
    # @api public
    def interest
      @transactions.filter_map { |trans| trans.amount if trans.interest? }
    end

    # @return [Array] the amount of the payment in each period
    # @example find the total payments for a loan
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = Finrb::Amortization.new(300000, rate)
    #   amt.payments.sum #=> Flt::DecNum('-500163.94')
    # @api public
    def payments
      @transactions.filter_map { |trans| trans.amount if trans.payment? }
    end

    private

    def build_schedule
      opening_balance = @principal
      @transactions.each_slice(2).with_index.map do |(interest, payment), index|
        principal = -(payment.amount + interest.amount)
        closing_balance = opening_balance - principal
        entry = Entry.new(period: payment.period, opening_balance:, payment: payment.amount, interest: interest.amount, principal:, additional_payment: @additional_by_period.fetch(index), balloon_payment: @balloon_by_period.fetch(index), closing_balance:)
        opening_balance = closing_balance
        entry
      end
    end
  end
end
