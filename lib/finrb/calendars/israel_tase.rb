# frozen_string_literal: true

require_relative 'base'
require_relative 'hebrew_calendar'

module Finrb
  module Calendars
    # Tel Aviv Stock Exchange full-day trading calendar.
    class IsraelTase < Base
      # Contract window: QuantLib 1.43 supplies TASE holiday data through 2050.
      # Earlier dates and later projections are not validated by this profile.
      SUPPORTED_START_DATE = Date.new(2000, 1, 1, Date::GREGORIAN)
      SUPPORTED_END_DATE = Date.new(2050, 12, 31, Date::GREGORIAN)
      SUPPORTED_DATE_RANGE = (SUPPORTED_START_DATE..SUPPORTED_END_DATE)
      public_constant :SUPPORTED_START_DATE, :SUPPORTED_END_DATE, :SUPPORTED_DATE_RANGE

      TRADING_WEEK_CHANGE = Date.new(2026, 1, 5, Date::GREGORIAN)
      WEEKEND_BEFORE_CHANGE = [5, 6].freeze
      WEEKEND_AFTER_CHANGE = [0, 6].freeze
      public_constant :TRADING_WEEK_CHANGE
      private_constant :WEEKEND_BEFORE_CHANGE, :WEEKEND_AFTER_CHANGE

      def name
        'Israel TASE'
      end

      protected

      def weekend?(date)
        weekend_days = date < TRADING_WEEK_CHANGE ? WEEKEND_BEFORE_CHANGE : WEEKEND_AFTER_CHANGE
        weekend_days.include?(date.wday)
      end

      def holidays_for(date)
        names = {}
        ((date.year + 3759)..(date.year + 3761)).each do |hebrew_year|
          hebrew_holidays(hebrew_year).each do |holiday_date, name|
            (names[holiday_date] ||= []).concat(name)
          end
        end
        add(names, Date.new(2026, 1, 4, Date::GREGORIAN), 'TASE trading-week transition')
        names.fetch(date, [])
      end

      private

      def hebrew_holidays(year)
        holidays = {}
        add(holidays, HebrewCalendar.date(year - 1, 6, 29), 'Rosh Hashanah Eve')
        add(holidays, HebrewCalendar.date(year, 7, 1), 'Rosh Hashanah I')
        add(holidays, HebrewCalendar.date(year, 7, 2), 'Rosh Hashanah II')
        add(holidays, HebrewCalendar.date(year, 7, 9), 'Yom Kippur Eve')
        add(holidays, HebrewCalendar.date(year, 7, 10), 'Yom Kippur')
        add(holidays, HebrewCalendar.date(year, 7, 14), 'Sukkot Eve')
        add(holidays, HebrewCalendar.date(year, 7, 15), 'Sukkot')
        add(holidays, HebrewCalendar.date(year, 7, 21), 'Shemini Atzeret Eve')
        add(holidays, HebrewCalendar.date(year, 7, 22), 'Shemini Atzeret / Simchat Torah')

        adar = HebrewCalendar.leap_year?(year) ? 13 : 12
        add(holidays, HebrewCalendar.date(year, adar, 14), 'Purim')

        add(holidays, HebrewCalendar.date(year, 1, 14), 'Passover Eve I')
        add(holidays, HebrewCalendar.date(year, 1, 15), 'Passover I')
        add(holidays, HebrewCalendar.date(year, 1, 20), 'Passover Eve VII')
        add(holidays, HebrewCalendar.date(year, 1, 21), 'Passover VII')
        add(holidays, israel_independence_day(year) - 1, 'Memorial Day')
        add(holidays, israel_independence_day(year), 'Independence Day')
        add(holidays, HebrewCalendar.date(year, 3, 5), 'Shavuot Eve')
        add(holidays, HebrewCalendar.date(year, 3, 6), 'Shavuot')
        add(holidays, tisha_bav(year), 'Tisha B’Av')
        holidays
      end

      def israel_independence_day(year)
        date = HebrewCalendar.date(year, 2, 5)
        day =
          case date.wday
          when 6 then 3
          when 5 then 4
          when 1 then 6
          else 5
          end
        HebrewCalendar.date(year, 2, day)
      end

      def tisha_bav(year)
        date = HebrewCalendar.date(year, 5, 9)
        date.saturday? ? date + 1 : date
      end

      def add(holidays, date, name)
        (holidays[date] ||= []) << name
      end
    end
  end
end
