# frozen_string_literal: true

require_relative 'decimal'
require 'date'

module Finrb
  # Year-fraction calculations used to express elapsed time under a named
  # financial day-count convention.
  module DayCount
    CONVENTIONS = %i[actual_365_fixed actual_360 actual_actual_icma].freeze
    DEFAULT = :actual_365_fixed
    public_constant :CONVENTIONS, :DEFAULT

    module_function

    # Calculate a signed year fraction. Actual/Actual ICMA requires the regular
    # coupon reference period and coupon frequency containing the date range.
    def year_fraction(start_date, end_date, convention: DEFAULT, reference_period_start: nil, reference_period_end: nil, frequency: nil)
      raise(ArgumentError, "day-count convention must be one of #{CONVENTIONS.join(', ')}.") unless CONVENTIONS.include?(convention)

      start_date = normalize_date(start_date, 'start_date')
      end_date = normalize_date(end_date, 'end_date')

      if convention == :actual_actual_icma
        icma_year_fraction(start_date, end_date, reference_period_start, reference_period_end, frequency)
      else
        reject_reference_period_arguments!(reference_period_start, reference_period_end, frequency)
        denominator = convention == :actual_360 ? 360 : 365

        Flt::DecNum((end_date - start_date).to_i.to_s) / Flt::DecNum(denominator.to_s)
      end
    end

    def icma_year_fraction(start_date, end_date, reference_period_start, reference_period_end, frequency)
      raise(ArgumentError, 'Actual/Actual ICMA requires reference_period_start, reference_period_end, and frequency.') unless reference_period_start && reference_period_end && frequency

      reference_period_start = normalize_date(reference_period_start, 'reference_period_start')
      reference_period_end = normalize_date(reference_period_end, 'reference_period_end')
      raise(ArgumentError, 'reference_period_end must be after reference_period_start.') if reference_period_end <= reference_period_start
      raise(ArgumentError, 'frequency must be a positive integer.') unless frequency.is_a?(Integer) && frequency.positive?

      elapsed_days = Flt::DecNum((end_date - start_date).to_i.to_s)
      reference_days = Flt::DecNum((reference_period_end - reference_period_start).to_i.to_s)
      elapsed_days / (reference_days * frequency)
    end
    private_class_method :icma_year_fraction

    def reject_reference_period_arguments!(reference_period_start, reference_period_end, frequency)
      return if reference_period_start.nil? && reference_period_end.nil? && frequency.nil?

      raise(ArgumentError, 'reference-period arguments are only valid for Actual/Actual ICMA.')
    end
    private_class_method :reject_reference_period_arguments!

    def normalize_date(date, name)
      raise(ArgumentError, "#{name} must be a Date.") unless date.instance_of?(Date)

      Date.new(date.year, date.month, date.day, Date::GREGORIAN)
    end
    private_class_method :normalize_date
  end
end
