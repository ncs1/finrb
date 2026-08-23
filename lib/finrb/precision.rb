# frozen_string_literal: true

require_relative 'validation'

module Finrb
  # Explicit quantization policy for values rounded during calculations.
  # General calculations otherwise retain the active Flt::DecNum context.
  module Precision
    MONEY_PLACES = 2
    RATE_PLACES = 15
    ROUNDING_MODE = :half_up
    public_constant :MONEY_PLACES, :RATE_PLACES, :ROUNDING_MODE

    module_function

    def money(value)
      round(value, places: MONEY_PLACES)
    end

    def rate(value)
      round(value, places: RATE_PLACES)
    end

    def round(value, places:)
      Validation.decimal(value, name: 'value').round(places, rounding: ROUNDING_MODE)
    end
    private_class_method :round
  end
end
