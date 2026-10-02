# frozen_string_literal: true

require_relative 'calendars'
require 'date'

module Finrb
  # Immutable date schedule for recurring financial payments.
  class Schedule
    FREQUENCY_MONTHS = { monthly: 1, quarterly: 3, semiannual: 6, annual: 12 }.freeze
    STUB_CONVENTIONS = %i[none short_final].freeze
    public_constant :FREQUENCY_MONTHS, :STUB_CONVENTIONS

    # Shared date rules used by both the class constructor and period builder.
    module DateRules
      module_function

      def normalize(date, name)
        raise(ArgumentError, "#{name} must be a Date.") unless date.instance_of?(Date)

        Date.new(date.year, date.month, date.day, Date::GREGORIAN)
      end

      def anchored_date(anchor, month_offset)
        month_start = Date.new(anchor.year, anchor.month, 1, Date::GREGORIAN) >> month_offset
        next_month_start = month_start >> 1
        month_end = next_month_start - 1
        anchor_month_end = (Date.new(anchor.year, anchor.month, 1, Date::GREGORIAN) >> 1) - 1
        day = anchor.day == anchor_month_end.day ? month_end.day : [anchor.day, month_end.day].min
        Date.new(month_start.year, month_start.month, day, Date::GREGORIAN)
      end
    end
    private_constant :DateRules

    # Immutable dates and index for one accrual/payment period.
    class Period
      attr_reader :index, :accrual_start_date, :unadjusted_payment_date, :payment_date, :stub

      def initialize(index:, accrual_start_date:, unadjusted_payment_date:, payment_date:, stub: nil)
        @index = index
        @accrual_start_date = accrual_start_date
        @unadjusted_payment_date = unadjusted_payment_date
        @payment_date = payment_date
        @stub = stub
        freeze
      end

      def short_final_stub?
        stub == :short_final
      end
    end

    attr_reader :start_date, :maturity_date, :frequency, :stub, :calendar, :business_day_convention, :periods, :payment_dates, :unadjusted_payment_dates

    def self.from_months(start_date:, term_months:, frequency: :monthly, stub: :none, calendar: nil, business_day_convention: nil)
      raise(ArgumentError, 'term_months must be a positive integer.') unless term_months.is_a?(Integer) && term_months.positive?

      start_date = DateRules.normalize(start_date, 'start_date')
      maturity_date = DateRules.anchored_date(start_date, term_months)
      new(start_date:, maturity_date:, frequency:, stub:, calendar:, business_day_convention:)
    end

    def initialize(start_date:, maturity_date:, frequency: :monthly, stub: :none, calendar: nil, business_day_convention: nil)
      @start_date = DateRules.normalize(start_date, 'start_date')
      @maturity_date = DateRules.normalize(maturity_date, 'maturity_date')
      raise(ArgumentError, 'maturity_date must be after start_date.') if @maturity_date <= @start_date
      raise(ArgumentError, "frequency must be one of #{FREQUENCY_MONTHS.keys.join(', ')}.") unless FREQUENCY_MONTHS.key?(frequency)
      raise(ArgumentError, "stub must be one of #{STUB_CONVENTIONS.join(', ')}.") unless STUB_CONVENTIONS.include?(stub)

      validate_calendar_options!(calendar, business_day_convention)
      @frequency = frequency
      @stub = stub
      @calendar = calendar
      @business_day_convention = business_day_convention
      @unadjusted_payment_dates = build_unadjusted_payment_dates.freeze
      @periods = build_periods.freeze
      @payment_dates = @periods.map(&:payment_date).freeze
      freeze
    end

    private

    def validate_calendar_options!(calendar, convention)
      raise(ArgumentError, 'business_day_convention requires a calendar.') if calendar.nil? && !convention.nil?
      return if calendar.nil?

      raise(ArgumentError, 'calendar must be a Finrb::Calendars::Base instance.') unless calendar.is_a?(Calendars::Base)
      raise(ArgumentError, "business_day_convention must be one of #{Calendars::Base::CONVENTIONS.join(', ')}.") unless Calendars::Base::CONVENTIONS.include?(convention)
    end

    def build_unadjusted_payment_dates
      interval = FREQUENCY_MONTHS.fetch(frequency)
      dates = []
      month_offset = interval
      loop do
        regular_date = DateRules.anchored_date(start_date, month_offset)
        if regular_date < maturity_date
          dates << regular_date
          month_offset += interval
        elsif regular_date == maturity_date
          dates << regular_date
          break
        else
          raise(ArgumentError, 'maturity_date does not align with frequency; pass stub: :short_final to allow a short final period.') unless stub == :short_final

          dates << maturity_date
          break
        end
      end
      dates
    end

    def build_periods
      previous_payment_date = start_date
      unadjusted_payment_dates.each_with_index.map do |unadjusted_date, index|
        regular_date = DateRules.anchored_date(start_date, FREQUENCY_MONTHS.fetch(frequency) * (index + 1))
        period_stub = :short_final if index == unadjusted_payment_dates.length - 1 && regular_date != unadjusted_date
        payment_date = calendar ? calendar.adjust(unadjusted_date, convention: business_day_convention) : unadjusted_date
        raise(ArgumentError, 'calendar adjustment must produce dates strictly after the prior payment date.') if payment_date <= previous_payment_date

        period = Period.new(index:, accrual_start_date: previous_payment_date, unadjusted_payment_date: unadjusted_date, payment_date:, stub: period_stub)
        previous_payment_date = payment_date
        period
      end
    end
  end
end
