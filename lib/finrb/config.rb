# frozen_string_literal: true

module Finrb
  Config = Struct.new(:eps, :guess, :business_days, :periodic_compound)
  private_constant :Config

  def self.config
    @config ||= Config.new(eps: '1.0e-16', guess: 1.0, business_days: false, periodic_compound: false)
  end

  def self.configure
    yield(config)
  end
end
