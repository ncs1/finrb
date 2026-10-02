# frozen_string_literal: true

require 'date'

module Finrb
  module Calendars
    # Arithmetic Hebrew calendar dates used for the TASE holiday rules.
    module HebrewCalendar
      ANCHOR_HEBREW_YEAR = 5785
      ANCHOR_ROSH_HASHANAH = Date.new(2024, 10, 3, Date::GREGORIAN)
      private_constant :ANCHOR_HEBREW_YEAR, :ANCHOR_ROSH_HASHANAH

      module_function

      def leap_year?(year)
        ((year * 7) + 1) % 19 < 7
      end

      def date(year, month, day)
        months = months_in_year(year)
        raise(ArgumentError, 'invalid Hebrew month.') unless months.include?(month)

        month_length = days_in_month(year, month)
        raise(ArgumentError, 'invalid Hebrew day.') unless day.between?(1, month_length)

        offset = 0
        months.each do |candidate_month|
          break if candidate_month == month

          offset += days_in_month(year, candidate_month)
        end
        Date.jd(rosh_hashanah_jd(year) + offset + day - 1, Date::GREGORIAN)
      end

      def rosh_hashanah_jd(year)
        ANCHOR_ROSH_HASHANAH.jd + elapsed_days(year) - elapsed_days(ANCHOR_HEBREW_YEAR)
      end

      def months_in_year(year)
        after_tishri = (7..(leap_year?(year) ? 13 : 12)).to_a
        after_tishri + (1..6).to_a
      end

      def days_in_month(year, month)
        year_length = elapsed_days(year + 1) - elapsed_days(year)
        case month
        when 1, 3, 5, 7, 11
          30
        when 2, 4, 6, 10, 13
          29
        when 8
          year_length % 10 == 5 ? 30 : 29
        when 9
          year_length % 10 == 3 ? 29 : 30
        when 12
          leap_year?(year) ? 30 : 29
        else
          raise(ArgumentError, 'invalid Hebrew month.')
        end
      end

      def elapsed_days(year)
        elapsed_months = ((year * 235) - 234) / 19
        parts_elapsed = ((elapsed_months % 1080) * 793) + 204
        hours_elapsed = (elapsed_months * 12) + 5 + ((elapsed_months / 1080) * 793) + (parts_elapsed / 1080)
        day = (elapsed_months * 29) + 1 + (hours_elapsed / 24)
        parts = ((hours_elapsed % 24) * 1080) + (parts_elapsed % 1080)

        molad_postponement = parts >= 19_440 || ((day % 7) == 2 && parts >= 9_924 && !leap_year?(year)) || ((day % 7) == 1 && parts >= 16_789 && leap_year?(year - 1))
        day += 1 if molad_postponement
        day += 1 if [0, 3, 5].include?(day % 7)
        day
      end
      private_class_method :elapsed_days, :days_in_month, :months_in_year, :rosh_hashanah_jd
    end
  end
end
