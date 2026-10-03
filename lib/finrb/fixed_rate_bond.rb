# frozen_string_literal: true

require_relative 'calendars'
require_relative 'config'
require_relative 'day_count'
require_relative 'errors'
require_relative 'numerical/brent'
require_relative 'numerical/rate_search'
require_relative 'schedule'
require_relative 'transaction'
require_relative 'validation'

module Finrb
  # Fixed-coupon bullet bond valuation under explicit Actual/Actual ICMA rules.
  class FixedRateBond
    PRICE_TYPES = %i[clean dirty].freeze
    public_constant :PRICE_TYPES

    CouponPeriod = Data.define(:accrual_start_date, :accrual_end_date, :payment_date, :amount)
    private_constant :CouponPeriod

    attr_reader :face_value, :coupon_rate, :issue_date, :maturity_date, :frequency, :schedule, :calendar, :business_day_convention

    def initialize(face_value:, coupon_rate:, issue_date:, maturity_date:, frequency: :semiannual, calendar: nil, business_day_convention: nil)
      @face_value = Validation.positive_decimal(face_value, name: 'face value')
      @coupon_rate = Validation.non_negative_decimal(coupon_rate, name: 'annual coupon rate')
      @schedule = Schedule.new(start_date: issue_date, maturity_date:, frequency:, calendar:, business_day_convention:)

      @issue_date = schedule.start_date
      @maturity_date = schedule.maturity_date
      @frequency = frequency
      @coupon_frequency = 12 / Schedule::FREQUENCY_MONTHS.fetch(frequency)
      @calendar = calendar
      @business_day_convention = business_day_convention
      @coupon_periods = build_coupon_periods.freeze
      freeze
    end

    def day_count
      :actual_actual_icma
    end

    # Return future coupon and redemption cashflows as dated positive transactions.
    def cashflows(settlement_date:)
      settlement_date = validate_settlement_date!(settlement_date)
      cashflows =
        @coupon_periods.filter_map.with_index do |period, index|
          next if period.payment_date <= settlement_date

          amount = period.amount
          amount += face_value if index == @coupon_periods.length - 1
          Transaction.new(amount, date: period.payment_date)
        end
      cashflows.freeze
    end

    def accrued_interest(settlement_date:)
      settlement_date = validate_settlement_date!(settlement_date)
      @coupon_periods.sum do |period|
        next Flt::DecNum(0) if period.payment_date <= settlement_date

        accrual_end = [settlement_date, period.accrual_end_date].min
        next Flt::DecNum(0) if accrual_end <= period.accrual_start_date

        fraction = icma_fraction(period.accrual_start_date, accrual_end, period.accrual_start_date, period.accrual_end_date)
        face_value * coupon_rate * fraction
      end
    end

    def dirty_price(settlement_date:, yield_rate:)
      settlement_date = validate_settlement_date!(settlement_date)
      yield_rate = Validation.decimal_greater_than(yield_rate, minimum: -1, name: 'bond yield', error: DomainError)
      periodic_yield = yield_rate / @coupon_frequency
      discount_base = periodic_yield + 1

      @coupon_periods.each_with_index.sum(Flt::DecNum(0)) do |period, index|
        next Flt::DecNum(0) if period.payment_date <= settlement_date

        amount = period.amount
        amount += face_value if index == @coupon_periods.length - 1
        elapsed_coupon_periods = schedule_year_fraction(settlement_date, period.payment_date) * @coupon_frequency
        amount / (discount_base**elapsed_coupon_periods)
      end
    end

    def clean_price(settlement_date:, yield_rate:)
      dirty_price(settlement_date:, yield_rate:) - accrued_interest(settlement_date:)
    end

    def yield_to_maturity(settlement_date:, price:, price_type: :clean, guess: Finrb.config.guess)
      settlement_date = validate_settlement_date!(settlement_date)
      raise(ArgumentError, "price_type must be one of #{PRICE_TYPES.join(', ')}.") unless PRICE_TYPES.include?(price_type)

      quoted_price = Validation.positive_decimal(price, name: "#{price_type} bond price", error: DomainError)
      dirty_price_target = price_type == :clean ? quoted_price + accrued_interest(settlement_date:) : quoted_price
      rate_function = ->(rate) { dirty_price(settlement_date:, yield_rate: rate) - dirty_price_target }
      bounds = Numerical::RateSearch.new.bracket(rate_function, guess:)
      return bounds.first if bounds.first == bounds.last

      Numerical::Brent.new(tolerance: Finrb.config.eps).solve(rate_function, lower: bounds.first, upper: bounds.last)
    end

    private

    def build_coupon_periods
      previous_unadjusted_date = issue_date
      schedule.periods.map do |period|
        accrual_start_date = previous_unadjusted_date
        accrual_end_date = period.unadjusted_payment_date
        fraction = icma_fraction(accrual_start_date, accrual_end_date, accrual_start_date, accrual_end_date)
        coupon_amount = face_value * coupon_rate * fraction
        previous_unadjusted_date = accrual_end_date
        CouponPeriod.new(accrual_start_date:, accrual_end_date:, payment_date: period.payment_date, amount: coupon_amount)
      end
    end

    def icma_fraction(start_date, end_date, reference_period_start, reference_period_end)
      DayCount.year_fraction(start_date, end_date, convention: :actual_actual_icma, reference_period_start:, reference_period_end:, frequency: @coupon_frequency)
    end

    def schedule_year_fraction(start_date, end_date)
      @coupon_periods.sum(Flt::DecNum(0)) do |period|
        overlap_start = [start_date, period.accrual_start_date].max
        overlap_end = [end_date, period.accrual_end_date].min
        next Flt::DecNum(0) if overlap_end <= overlap_start

        icma_fraction(overlap_start, overlap_end, period.accrual_start_date, period.accrual_end_date)
      end
    end

    def validate_settlement_date!(date)
      raise(ArgumentError, 'settlement_date must be a Date.') unless date.instance_of?(Date)

      date = Date.new(date.year, date.month, date.day, Date::GREGORIAN)
      raise(ArgumentError, 'settlement_date must be on or after issue_date.') if date < issue_date
      raise(ArgumentError, 'settlement_date must be before maturity_date.') if date >= maturity_date

      date
    end
  end
end
