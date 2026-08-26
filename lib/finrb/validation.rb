# frozen_string_literal: true

require_relative 'decimal'

module Finrb
  # Shared validation and decimal coercion for public financial inputs.
  module Validation
    module_function

    def decimal(value, name:)
      raise(ArgumentError, "#{name} must be numeric.") unless value.is_a?(Numeric)

      decimal = value.is_a?(Flt::DecNum) ? value : Flt::DecNum.new(value.to_s)
      raise(ArgumentError, "#{name} must be finite.") unless decimal.finite?

      decimal
    rescue Flt::Num::Exception, FloatDomainError, Math::DomainError => e
      raise(ArgumentError, "#{name} must be a finite numeric value.", e.backtrace)
    end

    def positive_integer(value, name:)
      raise(ArgumentError, "#{name} must be a positive integer.") unless value.is_a?(Integer) && value.positive?

      value
    end

    def positive_decimal(value, name:, error: ArgumentError, message: nil)
      decimal = decimal(value, name:)
      raise(error, message || "#{name} must be greater than zero.") unless decimal.positive?

      decimal
    end

    def non_negative_decimal(value, name:, error: ArgumentError, message: nil)
      decimal = decimal(value, name:)
      raise(error, message || "#{name} must be greater than or equal to zero.") if decimal.negative?

      decimal
    end

    def decimal_greater_than(value, minimum:, name:, error: ArgumentError)
      decimal = decimal(value, name:)
      raise(error, "#{name} must be greater than #{minimum}.") if decimal <= minimum

      decimal
    end

    def decimal_at_least(value, minimum:, name:, error: ArgumentError)
      decimal = decimal(value, name:)
      raise(error, "#{name} must be greater than or equal to #{minimum}.") if decimal < minimum

      decimal
    end

    def non_zero_decimal(value, name:, error: ArgumentError)
      decimal = decimal(value, name:)
      raise(error, "#{name} must be non-zero.") if decimal.zero?

      decimal
    end

    def decimal_between(value, minimum:, maximum:, name:, error: ArgumentError)
      decimal = decimal(value, name:)
      raise(error, "#{name} must be between #{minimum} and #{maximum}, inclusive.") unless decimal.between?(minimum, maximum)

      decimal
    end
  end
end
