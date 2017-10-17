# frozen_string_literal: true

module Finance
  include ActiveSupport::Configurable

  default_values = {
    eps: '1.0e-16',
    guess: 1.0,
    use_business_days: false
  }

  default_values.each do |key, value|
    config.send("#{key.to_sym}=", value)
  end
end
