# frozen_string_literal: true

module Finrb
  include ActiveSupport::Configurable

  default_values = { eps: '1.0e-16', guess: 1.0, business_days: false, periodic_compound: false }

  default_values.each do |key, value|
    config.__send__("#{key.to_sym}=", value)
  end
end
