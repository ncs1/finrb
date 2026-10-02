# frozen_string_literal: true

require_relative 'decimal'
require 'date'

module Finrb
  # Year-fraction calculations used to express elapsed time under a named
  # financial day-count convention.
  module DayCount
    CONVENTIONS = %i[actual_365_fixed actual_360].freeze
    DEFAULT = :actual_365_fixed
    public_constant :CONVENTIONS, :DEFAULT

    module_function

    # Calculate signed elapsed days divided by the convention's fixed year
    # denominator. Dates are treated as date-only proleptic Gregorian dates.
    def year_fraction(start_date, end_date, convention: DEFAULT)
      raise(ArgumentError, "day-count convention must be one of #{CONVENTIONS.join(', ')}.") unless CONVENTIONS.include?(convention)

      start_date = normalize_date(start_date, 'start_date')
      end_date = normalize_date(end_date, 'end_date')
      denominator = convention == :actual_360 ? 360 : 365

      Flt::DecNum((end_date - start_date).to_i.to_s) / Flt::DecNum(denominator.to_s)
    end

    def normalize_date(date, name)
      raise(ArgumentError, "#{name} must be a Date.") unless date.instance_of?(Date)

      Date.new(date.year, date.month, date.day, Date::GREGORIAN)
    end
    private_class_method :normalize_date
  end
end
