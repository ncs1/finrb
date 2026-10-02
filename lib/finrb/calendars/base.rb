# frozen_string_literal: true

require 'date'

module Finrb
  module Calendars
    # A dated market calendar with explicit business-day adjustment rules.
    class Base
      CONVENTIONS = %i[following modified_following preceding modified_preceding half_month_modified_following nearest unadjusted].freeze
      public_constant :CONVENTIONS

      attr_reader :additional_holidays, :removed_holidays

      def initialize(additional_holidays: [], removed_holidays: [])
        @additional_holidays = validate_dates(additional_holidays, 'additional_holidays')
        @removed_holidays = validate_dates(removed_holidays, 'removed_holidays')
        freeze
      end

      def business_day?(date)
        date = normalize_date(date)
        !weekend?(date) && holiday_names(date).empty?
      end

      def holiday?(date)
        !business_day?(date)
      end

      def holiday_names(date)
        date = normalize_date(date)
        names = @removed_holidays.include?(date) ? [] : holidays_for(date)
        names += ['Additional market holiday'] if @additional_holidays.include?(date)
        names.uniq.freeze
      end

      def holidays_between(start_date, end_date, include_weekends: false)
        start_date = normalize_date(start_date)
        end_date = normalize_date(end_date)
        raise(ArgumentError, 'start_date must not be after end_date.') if start_date > end_date
        raise(ArgumentError, 'include_weekends must be true or false.') unless [true, false].include?(include_weekends)

        holidays =
          (start_date..end_date).each_with_object({}) do |date, result|
            names = holiday_names(date)
            names = ['Weekend'].freeze if include_weekends && weekend?(date) && names.empty?
            result[date] = names unless names.empty?
          end
        holidays.freeze
      end

      def ==(other)
        other.instance_of?(self.class) && additional_holidays == other.additional_holidays && removed_holidays == other.removed_holidays
      end
      alias eql? ==

      def hash
        [self.class, additional_holidays, removed_holidays].hash
      end

      def adjust(date, convention:)
        date = normalize_date(date)
        raise(ArgumentError, "business-day convention must be one of #{CONVENTIONS.join(', ')}.") unless CONVENTIONS.include?(convention)

        return date if convention == :unadjusted || business_day?(date)

        case convention
        when :following
          seek_business_day(date, 1)
        when :modified_following
          following = seek_business_day(date, 1)
          following.month == date.month ? following : seek_business_day(date, -1)
        when :preceding
          seek_business_day(date, -1)
        when :modified_preceding
          preceding = seek_business_day(date, -1)
          preceding.month == date.month ? preceding : seek_business_day(date, 1)
        when :half_month_modified_following
          following = seek_business_day(date, 1)
          crosses_month = following.month != date.month
          crosses_midmonth = date.day <= 15 && following.day > 15
          crosses_month || crosses_midmonth ? seek_business_day(date, -1) : following
        when :nearest
          nearest_business_day(date)
        else
          raise(ArgumentError, "unsupported business-day convention: #{convention}.")
        end
      end

      def advance(date, business_days:, convention: :following)
        date = normalize_date(date)
        raise(ArgumentError, 'business_days must be an integer.') unless business_days.is_a?(Integer)
        raise(ArgumentError, "business-day convention must be one of #{CONVENTIONS.join(', ')}.") unless CONVENTIONS.include?(convention)
        return adjust(date, convention:) if business_days.zero?

        direction = business_days.positive? ? 1 : -1
        remaining = business_days.abs
        advanced = date
        while remaining.positive?
          advanced += direction
          remaining -= 1 if business_day?(advanced)
        end
        advanced
      end

      protected

      def weekend?(_date)
        raise(NotImplementedError, 'calendar subclasses must define their weekend days.')
      end

      def holidays_for(_date)
        raise(NotImplementedError, 'calendar subclasses must define their market holidays.')
      end

      private

      def normalize_date(date)
        raise(ArgumentError, 'date must be a Date.') unless date.is_a?(Date)

        normalized_date = Date.new(date.year, date.month, date.day, Date::GREGORIAN)
        supported_dates = self.class::SUPPORTED_DATE_RANGE
        return normalized_date if supported_dates.cover?(normalized_date)

        raise(RangeError, "#{name} supports dates from #{supported_dates.begin} through #{supported_dates.end}.")
      end

      def validate_dates(dates, name)
        raise(ArgumentError, "#{name} must be an array of Dates.") unless dates.is_a?(Array) && dates.all?(Date)

        normalized_dates = []
        dates.each do |date|
          normalized_date = normalize_date(date)
          normalized_dates << normalized_date unless normalized_dates.include?(normalized_date)
        end
        normalized_dates.freeze
      end

      def seek_business_day(date, direction)
        candidate = date
        370.times do
          candidate += direction
          return candidate if business_day?(candidate)
        end
        raise(ArgumentError, 'no business day found within one year of the requested date.')
      end

      def nearest_business_day(date)
        distance = 1
        loop do
          preceding = date - distance
          following = date + distance
          return following if business_day?(following)
          return preceding if business_day?(preceding)

          distance += 1
          raise(ArgumentError, 'no business day found within one year of the requested date.') if distance > 370
        end
      end
    end
  end
end
