# frozen_string_literal: true

require_relative 'decimal'

module Finrb
  # Shared validation and decimal coercion for public financial inputs.
  module Validation
    module_function

    def decimal(value, name:)
      raise(ArgumentError, "#{name} must be numeric.") unless value.is_a?(Numeric)

      decimal = Flt::DecNum.new(value.to_s)
      raise(ArgumentError, "#{name} must be finite.") unless decimal.finite?

      decimal
    rescue Flt::Num::Exception, FloatDomainError, Math::DomainError => e
      raise(ArgumentError, "#{name} must be a finite numeric value.", e.backtrace)
    end

    def positive_integer(value, name:)
      raise(ArgumentError, "#{name} must be a positive integer.") unless value.is_a?(Integer) && value.positive?

      value
    end
  end
end
