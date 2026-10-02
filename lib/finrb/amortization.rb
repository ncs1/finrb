# frozen_string_literal: true

require_relative 'calendars'
require_relative 'cashflows'
require_relative 'day_count'
require_relative 'decimal'
require_relative 'precision'
require_relative 'transaction'
require_relative 'validation'
require 'date'

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
    FREQUENCY_MONTHS = { monthly: 1, quarterly: 3, semiannual: 6, annual: 12 }.freeze
    STUB_CONVENTIONS = %i[none short_final].freeze
    public_constant :FREQUENCY_MONTHS, :STUB_CONVENTIONS

    # Immutable breakdown of one amortization period. Payments retain finrb's
    # cashflow sign convention and are negative; the other monetary fields are
    # non-negative.
    class Entry
      ATTRIBUTES = %i[period opening_balance payment interest principal additional_payment balloon_payment interest_only closing_balance].freeze
      MONETARY_ATTRIBUTES = ATTRIBUTES - %i[period interest_only]
      DATE_ATTRIBUTE = :date
      private_constant :ATTRIBUTES, :DATE_ATTRIBUTE, :MONETARY_ATTRIBUTES

      attr_reader(*ATTRIBUTES, DATE_ATTRIBUTE)

      def initialize(period:, opening_balance:, payment:, interest:, principal:, additional_payment:, balloon_payment:, interest_only:, closing_balance:, date: nil)
        raise(ArgumentError, 'period must be a non-negative integer.') unless period.is_a?(Integer) && !period.negative?
        raise(ArgumentError, 'interest_only must be true or false.') unless [true, false].include?(interest_only)
        raise(ArgumentError, 'date must be a Date or nil.') unless date.nil? || date.instance_of?(Date)

        @period = period
        @interest_only = interest_only
        @date = date
        MONETARY_ATTRIBUTES.each do |name|
          value = binding.local_variable_get(name)
          instance_variable_set("@#{name}", Validation.decimal(value, name: name.to_s.tr('_', ' ')))
        end
        freeze
      end

      def ==(other)
        other.instance_of?(self.class) && ([DATE_ATTRIBUTE] + ATTRIBUTES).all? { |name| public_send(name) == other.public_send(name) }
      end
      alias eql? ==

      def hash
        [date, *ATTRIBUTES.map { |name| public_send(name) }].hash
      end

      def to_h
        attributes = ATTRIBUTES.to_h { |name| [name, public_send(name)] }
        attributes[DATE_ATTRIBUTE] = date unless date.nil?
        attributes
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
    # @return [Flt::DecNum] the required regular payment. For loans with more than one rate, returns nil
    attr_reader :payment
    # @return [Flt::DecNum] the principal amount of the loan
    attr_reader :principal
    # @return [Array] the interest rates used for calculating the amortization
    attr_reader :rates
    # @return [Array<Entry>] immutable period-by-period loan breakdown
    attr_reader :schedule
    # @return [Date, nil] the date from which payment dates are generated
    attr_reader :start_date
    # @return [Finrb::Calendars::Base, nil] the calendar used to adjust dated payments
    attr_reader :calendar
    # @return [Symbol, nil] business-day convention used when adjusting dated payments
    attr_reader :business_day_convention
    # @return [Symbol] day-count convention used for dated interest accrual
    attr_reader :day_count
    # @return [Symbol] interval between dated payments
    attr_reader :frequency
    # @return [Symbol] final partial-period handling
    attr_reader :stub

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
    # @param [Finrb::Calendars::Base, nil] calendar optional market calendar for dated payment adjustment
    # @param [Symbol, nil] business_day_convention required when calendar is supplied
    # @param [Symbol] frequency dated payment interval; rate durations remain in months
    # @param [Symbol] stub explicit handling for a final partial dated period
    def initialize(principal, *rates, balloon: 0, interest_only_periods: 0, origination_fee: 0, finance_origination_fee: false, start_date: nil, calendar: nil, business_day_convention: nil, day_count: DayCount::DEFAULT, frequency: :monthly, stub: :none, &block)
      @principal = Validation.positive_decimal(principal, name: 'principal', message: 'principal must be positive.')
      raise(ArgumentError, 'start_date must be a Date or nil.') unless start_date.nil? || start_date.instance_of?(Date)

      validate_day_count!(start_date, day_count)

      validate_calendar_options!(start_date, calendar, business_day_convention)

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

      initialize_schedule(start_date, frequency, stub, rates)

      valid_interest_only = interest_only_periods.is_a?(Integer) && interest_only_periods.between?(0, @periods - 1)
      raise(ArgumentError, 'interest_only_periods must be a non-negative integer shorter than the loan term.') unless valid_interest_only

      @interest_only_periods = interest_only_periods
      @start_date = start_date
      @calendar = calendar
      @business_day_convention = business_day_convention
      @day_count = day_count
      if @calendar
        @payment_dates.map! { |date| @calendar.adjust(date, convention: @business_day_convention) }
        previous_date = @start_date
        @payment_dates.each do |date|
          raise(ArgumentError, 'calendar adjustment must produce dates strictly after the prior payment date.') if date <= previous_date

          previous_date = date
        end
      end
      @period = 0

      compute
    end

    # compare two Amortization instances
    # @return [Numeric] -1, 0, or +1
    # @param [Amortization] other
    def ==(other)
      (principal == other.principal) && (start_date == other.start_date) && (calendar == other.calendar) && (business_day_convention == other.business_day_convention) && (day_count == other.day_count) && (frequency == other.frequency) && (stub == other.stub) && (origination_fee == other.origination_fee) && (finance_origination_fee? == other.finance_origination_fee?) && (balloon == other.balloon) && (interest_only_periods == other.interest_only_periods) && (rates == other.rates) && (payments == other.payments)
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

    # Calculate the effective annual yield implied by borrower proceeds and
    # scheduled loan payments. This is a cashflow-equivalent borrowing yield,
    # not a jurisdiction-specific legal APR.
    # @param [Numeric, nil] guess initial rate used by XIRR; defaults to Finrb.config.guess
    # @return [Rate] the effective annual cashflow-equivalent yield
    # @raise [ArgumentError] if the amortization has no start date
    # @example
    #   rate = Rate.new(0.05, :apr, duration: 12)
    #   loan = Amortization.new(10_000, rate, start_date: Date.new(2025, 1, 15), origination_fee: 250)
    #   loan.cashflow_yield.apy #=> effective annual borrower cost
    def cashflow_yield(guess = nil)
      raise(ArgumentError, 'cashflow_yield requires a start_date.') unless start_date

      transactions = [Transaction.new(net_proceeds, date: start_date)]
      transactions.concat(schedule.map { |entry| Transaction.new(entry.payment, date: entry.date) })
      Cashflow.xirr(transactions, guess)
    end

    # amortize the balance of loan with the given interest rate
    # @return none
    # @param [Rate] rate the interest rate to use in the amortization
    def amortize(rate, periods)
      regular_payment = nil

      periods.times do
        # Do this first in case the balance is zero already.
        break if @balance.zero?

        interest_only = @period < @interest_only_periods
        regular_payment ||= build_regular_payment(rate) unless interest_only

        # Compute and record interest on the outstanding balance.
        due_date = @payment_dates&.fetch(@period)
        periodic_rate = due_date ? dated_period_rate(rate, @period) : rate.monthly
        int = Precision.money(@balance * periodic_rate)
        interest = Interest.new(int, period: @period, date: due_date)
        @balance += interest.amount
        @transactions << interest.dup

        payment = interest_only ? build_interest_only_payment(int, due_date) : regular_payment
        payment.period = @period
        payment.date = due_date if due_date
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

      @rates.each_with_index do |rate, index|
        amortize(rate, @rate_period_counts.fetch(index))
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

    # @return [Integer] number of payments in the amortization schedule
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

    def validate_calendar_options!(start_date, calendar, convention)
      if calendar.nil?
        raise(ArgumentError, 'business_day_convention requires a calendar.') unless convention.nil?

        return
      end

      raise(ArgumentError, 'calendar must be a Finrb::Calendars::Base instance.') unless calendar.is_a?(Calendars::Base)
      raise(ArgumentError, 'calendar adjustment requires a start_date.') unless start_date
      raise(ArgumentError, "business_day_convention must be one of #{Calendars::Base::CONVENTIONS.join(', ')}.") unless Calendars::Base::CONVENTIONS.include?(convention)
    end

    def validate_day_count!(start_date, day_count)
      raise(ArgumentError, "day_count must be one of #{DayCount::CONVENTIONS.join(', ')}.") unless DayCount::CONVENTIONS.include?(day_count)
      raise(ArgumentError, 'a non-default day_count requires a start_date.') if start_date.nil? && day_count != DayCount::DEFAULT
    end

    def validate_schedule_options!(start_date, frequency, stub, term_months)
      raise(ArgumentError, "frequency must be one of #{FREQUENCY_MONTHS.keys.join(', ')}.") unless FREQUENCY_MONTHS.key?(frequency)
      raise(ArgumentError, "stub must be one of #{STUB_CONVENTIONS.join(', ')}.") unless STUB_CONVENTIONS.include?(stub)
      raise(ArgumentError, 'a non-monthly frequency requires a start_date.') if start_date.nil? && frequency != :monthly
      raise(ArgumentError, 'a non-default stub requires a start_date.') if start_date.nil? && stub != :none
      return if (term_months % FREQUENCY_MONTHS.fetch(frequency)).zero? || stub == :short_final

      raise(ArgumentError, 'loan term does not align with frequency; pass stub: :short_final to allow a short final period.')
    end

    def initialize_schedule(start_date, frequency, stub, rates)
      @term_months = rates.sum(&:duration)
      validate_schedule_options!(start_date, frequency, stub, @term_months)
      @frequency = frequency
      @stub = stub
      @period_month_offsets = start_date && payment_month_offsets(@term_months, frequency)
      @payment_dates = @period_month_offsets&.map { |month_offset| anchored_date(start_date, month_offset) }
      @rate_period_counts = start_date ? periods_per_rate(rates, @period_month_offsets) : rates.map(&:duration)
      @periods = @payment_dates ? @payment_dates.length : @term_months
    end

    def payment_month_offsets(term_months, frequency)
      interval = FREQUENCY_MONTHS.fetch(frequency)
      offsets = []
      month_offset = interval
      while month_offset < term_months
        offsets << month_offset
        month_offset += interval
      end
      offsets << term_months
      offsets
    end

    def periods_per_rate(rates, month_offsets)
      cumulative_months = 0
      previous_period_count = 0
      rates.each_with_index.map do |rate, index|
        cumulative_months += rate.duration
        if index == rates.length - 1
          month_offsets.length - previous_period_count
        else
          boundary_index = month_offsets.index(cumulative_months)
          raise(ArgumentError, 'rate changes must align with a payment date for dated non-monthly schedules.') unless boundary_index

          period_count = boundary_index + 1
          periods_in_segment = period_count - previous_period_count
          previous_period_count = period_count
          periods_in_segment
        end
      end
    end

    def build_schedule
      opening_balance = @amount_financed
      @transactions.each_slice(2).with_index.map do |(interest, payment), index|
        principal = -(payment.amount + interest.amount)
        closing_balance = opening_balance - principal
        entry = Entry.new(period: payment.period, opening_balance:, payment: payment.amount, interest: interest.amount, principal:, additional_payment: @additional_by_period.fetch(index), balloon_payment: @balloon_by_period.fetch(index), interest_only: @interest_only_by_period.fetch(index), closing_balance:, date: payment.date)
        opening_balance = closing_balance
        entry
      end
    end

    def build_regular_payment(rate)
      periods = @periods - @period
      amount =
        if @start_date
          dated_payment(@balance, rate, @period, @balloon)
        else
          Amortization.payment(@balance, rate.monthly, periods, balloon: @balloon)
        end
      Payment.new(amount, period: @period, date: @payment_dates && @payment_dates.fetch(@period)).tap do |payment|
        payment.modify(&@block) if @block
        validate_payment!(payment)
      end
    end

    def build_interest_only_payment(interest, date)
      Payment.new(-interest, period: @period, date:).tap do |payment|
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

    def anchored_date(anchor, month_offset)
      month_start = Date.new(anchor.year, anchor.month, 1) >> month_offset
      next_month_start = month_start >> 1
      month_end = next_month_start - 1
      anchor_month_end = (Date.new(anchor.year, anchor.month, 1) >> 1) - 1
      day =
        if anchor.day == anchor_month_end.day
          month_end.day
        else
          [anchor.day, month_end.day].min
        end
      Date.new(month_start.year, month_start.month, day)
    end

    def dated_period_rate(rate, period_index)
      previous_date = period_index.zero? ? @start_date : @payment_dates.fetch(period_index - 1)
      year_fraction = DayCount.year_fraction(previous_date, @payment_dates.fetch(period_index), convention: @day_count)
      periodic_rate = Precision.rate(rate.apr * year_fraction)
      Validation.decimal_greater_than(periodic_rate, minimum: -1, name: 'dated periodic rate')
    end

    def dated_payment(balance, rate, period_index, balloon)
      periods = (period_index...@periods).map { |index| dated_period_rate(rate, index) }
      discount_factors = []
      growth = Flt::DecNum('1')
      periods.each do |periodic_rate|
        growth *= periodic_rate + 1
        discount_factors << (Flt::DecNum('1') / growth)
      end
      balloon_discounted = balloon * discount_factors.last
      -Precision.money((balance - balloon_discounted) / discount_factors.sum)
    end
  end
end
