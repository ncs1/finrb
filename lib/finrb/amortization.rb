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
  class Amortization
    # Immutable breakdown of one amortization period. Payments retain finrb's
    # cashflow sign convention and are negative; the other monetary fields are
    # non-negative.
    class Entry
      ATTRIBUTES = %i[period opening_balance payment interest principal additional_payment balloon_payment interest_only closing_balance].freeze
      MONETARY_ATTRIBUTES = ATTRIBUTES - %i[period interest_only]
      private_constant :ATTRIBUTES, :MONETARY_ATTRIBUTES

      attr_reader(*ATTRIBUTES)

      def initialize(period:, opening_balance:, payment:, interest:, principal:, additional_payment:, balloon_payment:, interest_only:, closing_balance:)
        raise(ArgumentError, 'period must be a non-negative integer.') unless period.is_a?(Integer) && !period.negative?
        raise(ArgumentError, 'interest_only must be true or false.') unless [true, false].include?(interest_only)

        @period = period
        @interest_only = interest_only
        MONETARY_ATTRIBUTES.each do |name|
          value = binding.local_variable_get(name)
          instance_variable_set("@#{name}", Validation.decimal(value, name: name.to_s.tr('_', ' ')))
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

      alias interest_only? interest_only
    end

    # @return [Flt::DecNum] the balance of the loan at the end of the amortization period (usually zero)
    attr_reader :balance
    # @return [Flt::DecNum] contractual principal settled as a balloon in the final period
    attr_reader :balloon
    # @return [Flt::DecNum] principal balance including any financed origination fee
    attr_reader :amount_financed
    # @return [Flt::DecNum] cash made available to the borrower after an unfinanced fee
    attr_reader :net_proceeds
    # @return [Flt::DecNum] fee charged when the loan is originated
    attr_reader :origination_fee
    # @return [Integer] number of leading periods that pay interest but no scheduled principal
    attr_reader :interest_only_periods
    # @return [Flt::DecNum] the required monthly payment.  For loans with more than one rate, returns nil
    attr_reader :payment
    # @return [Flt::DecNum] the principal amount of the loan
    attr_reader :principal
    # @return [Array] the interest rates used for calculating the amortization
    attr_reader :rates
    # @return [Array<Entry>] immutable period-by-period loan breakdown
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
    def self.payment(principal, rate, periods, balloon: 0)
      principal = Validation.positive_decimal(principal, name: 'principal', message: 'principal must be positive.')

      balloon = Validation.decimal(balloon, name: 'balloon')
      raise(ArgumentError, 'balloon must be non-negative and no greater than principal.') unless balloon.between?(0, principal)

      rate = Validation.decimal_greater_than(rate, minimum: -1, name: 'periodic rate')

      periods = Validation.positive_integer(periods, name: 'period count')

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
    def initialize(principal, *rates, balloon: 0, interest_only_periods: 0, origination_fee: 0, finance_origination_fee: false, &block)
      @principal = Validation.positive_decimal(principal, name: 'principal', message: 'principal must be positive.')

      @origination_fee = Validation.non_negative_decimal(origination_fee, name: 'origination fee')
      raise(ArgumentError, 'finance_origination_fee must be true or false.') unless [true, false].include?(finance_origination_fee)
      raise(ArgumentError, 'an unfinanced origination_fee must be less than principal.') if !finance_origination_fee && @origination_fee >= @principal

      @finance_origination_fee = finance_origination_fee
      @amount_financed = @principal + (finance_origination_fee ? @origination_fee : 0)
      @net_proceeds = @principal - (finance_origination_fee ? 0 : @origination_fee)

      @balloon = Validation.decimal(balloon, name: 'balloon')
      raise(ArgumentError, 'balloon must be non-negative and less than amount financed.') if @balloon.negative? || @balloon >= @amount_financed
      raise(ArgumentError, 'at least one rate is required.') if rates.empty?
      raise(ArgumentError, 'rates must be Finrb::Rate instances.') unless rates.all?(Rate)
      raise(ArgumentError, 'every rate must have a duration.') if rates.any? { |rate| rate.duration.nil? }

      @rates     = rates
      @block     = block

      # compute the total duration from all of the rates.
      @periods = rates.sum(&:duration)
      valid_interest_only = interest_only_periods.is_a?(Integer) && interest_only_periods.between?(0, @periods - 1)
      raise(ArgumentError, 'interest_only_periods must be a non-negative integer shorter than the loan term.') unless valid_interest_only

      @interest_only_periods = interest_only_periods
      @period = 0

      compute
    end

    # compare two Amortization instances
    # @return [Numeric] -1, 0, or +1
    # @param [Amortization] other
    def ==(other)
      (principal == other.principal) && (origination_fee == other.origination_fee) && (finance_origination_fee? == other.finance_origination_fee?) && (balloon == other.balloon) && (interest_only_periods == other.interest_only_periods) && (rates == other.rates) && (payments == other.payments)
    end

    attr_reader :finance_origination_fee
    alias finance_origination_fee? finance_origination_fee

    # @return [Array] the amount of any additional payments in each period
    # @example
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = Finrb::Amortization.new(300000, rate){ |payment| payment.amount-100}
    #   amt.additional_payments #=> [Flt::DecNum('-100.00'), Flt::DecNum('-100.00'), ... ]
    def additional_payments
      @transactions.filter_map { |trans| trans.difference if trans.payment? }
    end

    # amortize the balance of loan with the given interest rate
    # @return none
    # @param [Rate] rate the interest rate to use in the amortization
    def amortize(rate)
      regular_payment = nil

      rate.duration.to_i.times do
        # Do this first in case the balance is zero already.
        break if @balance.zero?

        interest_only = @period < @interest_only_periods
        regular_payment ||= build_regular_payment(rate) unless interest_only

        # Compute and record interest on the outstanding balance.
        int = Precision.money(@balance * rate.monthly)
        interest = Interest.new(int, period: @period)
        @balance += interest.amount
        @transactions << interest.dup

        payment = interest_only ? build_interest_only_payment(int) : regular_payment
        payment.period = @period
        payment.amount = -@balance if payment.amount.abs > @balance
        @additional_by_period << [-payment.difference, Flt::DecNum(0)].max
        @interest_only_by_period << interest_only
        @transactions << payment.dup
        @balance += payment.amount

        @period += 1
      end
    end

    # compute the amortization of the principal
    # @return none
    def compute
      @balance = @amount_financed
      @transactions = []
      @additional_by_period = []
      @interest_only_by_period = []

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

      @payment = (payments.first if @rates.length == 1 && @interest_only_periods.zero?)

      @transactions.freeze
      @additional_by_period.freeze
      @balloon_by_period.freeze
      @interest_only_by_period.freeze
      @schedule = build_schedule.freeze
    end

    private :amortize, :compute

    # @return [Integer] the time required to pay off the loan, in months
    # @example In most cases, the duration is equal to the total duration of all rates
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = Finrb::Amortization.new(300000, rate)
    #   amt.duration #=> 360
    # @example Extra payments may reduce the duration
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = Finrb::Amortization.new(300000, rate){ |payment| payment.amount-100}
    #   amt.duration #=> 319
    def duration
      payments.length
    end

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
    def interest
      @transactions.filter_map { |trans| trans.amount if trans.interest? }
    end

    # @return [Array] the amount of the payment in each period
    # @example find the total payments for a loan
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = Finrb::Amortization.new(300000, rate)
    #   amt.payments.sum #=> Flt::DecNum('-500163.94')
    def payments
      @transactions.filter_map { |trans| trans.amount if trans.payment? }
    end

    private

    def build_schedule
      opening_balance = @amount_financed
      @transactions.each_slice(2).with_index.map do |(interest, payment), index|
        principal = -(payment.amount + interest.amount)
        closing_balance = opening_balance - principal
        entry = Entry.new(period: payment.period, opening_balance:, payment: payment.amount, interest: interest.amount, principal:, additional_payment: @additional_by_period.fetch(index), balloon_payment: @balloon_by_period.fetch(index), interest_only: @interest_only_by_period.fetch(index), closing_balance:)
        opening_balance = closing_balance
        entry
      end
    end

    def build_regular_payment(rate)
      periods = @periods - @period
      amount = Amortization.payment(@balance, rate.monthly, periods, balloon: @balloon)
      Payment.new(amount, period: @period).tap do |payment|
        payment.modify(&@block) if @block
        validate_payment!(payment)
      end
    end

    def build_interest_only_payment(interest)
      Payment.new(-interest, period: @period).tap do |payment|
        payment.modify(&@block) if @block
        validate_payment!(payment, allow_zero: true)
      end
    end

    def validate_payment!(payment, allow_zero: false)
      valid = payment.amount.negative? || (allow_zero && payment.amount.zero?)
      return if valid

      requirement = allow_zero ? 'must not produce a positive amount' : 'must produce a negative amount'
      raise(ArgumentError, "payment modification #{requirement}.")
    end
  end
end
