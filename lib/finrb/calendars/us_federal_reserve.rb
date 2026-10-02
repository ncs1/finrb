# frozen_string_literal: true

require_relative 'base'

module Finrb
  module Calendars
    # Federal Reserve Bank payment-business calendar (not NYSE or federal staff leave).
    class USFederalReserve < Base
      # Contract window: QuantLib 1.43 was exhaustively checked over these years;
      # pre-1950 history is intentionally outside this profile and later dates
      # are rejected rather than silently extrapolated.
      SUPPORTED_START_DATE = Date.new(1950, 1, 1, Date::GREGORIAN)
      SUPPORTED_END_DATE = Date.new(2065, 12, 31, Date::GREGORIAN)
      SUPPORTED_DATE_RANGE = (SUPPORTED_START_DATE..SUPPORTED_END_DATE)
      public_constant :SUPPORTED_START_DATE, :SUPPORTED_END_DATE, :SUPPORTED_DATE_RANGE

      WEEKEND_DAYS = [0, 6].freeze
      private_constant :WEEKEND_DAYS

      def name
        'US Federal Reserve'
      end

      protected

      def weekend?(date)
        WEEKEND_DAYS.include?(date.wday)
      end

      def holidays_for(date)
        year = date.year
        holidays = {}
        add_fixed_holiday(holidays, year, 1, 1, "New Year's Day")
        add_weekday_holiday(holidays, nth_weekday(year, 1, 1, 3), 'Birthday of Martin Luther King, Jr.') if year >= 1983
        if year < 1971
          add_pre_1971_fixed_holiday(holidays, Date.new(year, 2, 22, Date::GREGORIAN), "Washington's Birthday")
          add_pre_1971_fixed_holiday(holidays, Date.new(year, 5, 30, Date::GREGORIAN), 'Memorial Day')
        else
          add_weekday_holiday(holidays, nth_weekday(year, 2, 1, 3), "Washington's Birthday")
          add_weekday_holiday(holidays, last_weekday(year, 5, 1), 'Memorial Day')
        end
        add_fixed_holiday(holidays, year, 6, 19, 'Juneteenth National Independence Day') if year >= 2021
        add_fixed_holiday(holidays, year, 7, 4, 'Independence Day')
        add_weekday_holiday(holidays, nth_weekday(year, 9, 1, 1), 'Labor Day')
        add_weekday_holiday(holidays, nth_weekday(year, 10, 1, 2), 'Columbus Day') if year >= 1971
        add_fixed_holiday(holidays, year, 11, 11, 'Veterans Day') unless year.between?(1971, 1977)
        add_weekday_holiday(holidays, nth_weekday(year, 10, 1, 4), 'Veterans Day') if year.between?(1971, 1977)
        add_weekday_holiday(holidays, nth_weekday(year, 11, 4, 4), 'Thanksgiving Day')
        add_fixed_holiday(holidays, year, 12, 25, 'Christmas Day')
        holidays.fetch(date, [])
      end

      private

      def add_fixed_holiday(holidays, year, month, day, name)
        date = Date.new(year, month, day, Date::GREGORIAN)
        add_weekday_holiday(holidays, date, name)
        add_weekday_holiday(holidays, date + 1, name) if date.sunday?
      end

      def add_pre_1971_fixed_holiday(holidays, date, name)
        add_weekday_holiday(holidays, date, name)
        add_weekday_holiday(holidays, date - 1, name) if date.saturday?
        add_weekday_holiday(holidays, date + 1, name) if date.sunday?
      end

      def add_weekday_holiday(holidays, date, name)
        (holidays[date] ||= []) << name
      end

      def nth_weekday(year, month, weekday, occurrence)
        first = Date.new(year, month, 1, Date::GREGORIAN)
        first + ((weekday - first.wday) % 7) + ((occurrence - 1) * 7)
      end

      def last_weekday(year, month, weekday)
        last = Date.new(year, month, -1, Date::GREGORIAN)
        last - ((last.wday - weekday) % 7)
      end
    end
  end
end
