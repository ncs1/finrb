# frozen_string_literal: true

require_relative 'validation'

module Finrb
  Configuration = Data.define(:eps, :guess, :business_days, :periodic_compound)
  ConfigurationBuilder = Struct.new(:eps, :guess, :business_days, :periodic_compound)
  DEFAULT_CONFIGURATION = Configuration.new(eps: Flt::DecNum.new('1.0e-16'), guess: Flt::DecNum.new('1.0'), business_days: false, periodic_compound: false)
  CONFIG_OVERRIDE_KEY = :finrb_configuration_override
  private_constant :Configuration, :ConfigurationBuilder, :DEFAULT_CONFIGURATION, :CONFIG_OVERRIDE_KEY

  def self.config
    Thread.current[CONFIG_OVERRIDE_KEY] || (@config ||= DEFAULT_CONFIGURATION)
  end

  # Atomically replace the process-wide defaults. Configure the application at
  # startup; use +with_config+ for temporary or concurrent overrides.
  def self.configure
    raise(ArgumentError, 'configuration requires a block.') unless block_given?

    builder = ConfigurationBuilder.new(**config.to_h)
    yield(builder)
    @config = build_configuration(builder.to_h)
  end

  # Apply a validated configuration only for the current execution context and
  # restore the previous configuration even when the block raises.
  def self.with_config(**overrides)
    raise(ArgumentError, 'configuration override requires a block.') unless block_given?

    unknown = overrides.keys - config.to_h.keys
    raise(ArgumentError, "unknown configuration options: #{unknown.join(', ')}") unless unknown.empty?

    previous = Thread.current[CONFIG_OVERRIDE_KEY]
    Thread.current[CONFIG_OVERRIDE_KEY] = build_configuration(config.to_h.merge(overrides))
    begin
      yield
    ensure
      Thread.current[CONFIG_OVERRIDE_KEY] = previous
    end
  end

  def self.build_configuration(values)
    eps = configuration_decimal(values.fetch(:eps), name: 'eps')
    raise(ArgumentError, 'eps must be positive.') unless eps.positive?

    guess = configuration_decimal(values.fetch(:guess), name: 'guess')
    raise(ArgumentError, 'guess must be greater than -1.') if guess <= -1

    business_days = values.fetch(:business_days)
    periodic_compound = values.fetch(:periodic_compound)
    booleans = [business_days, periodic_compound].all? { |value| value.equal?(true) || value.equal?(false) }
    raise(ArgumentError, 'business_days and periodic_compound must be boolean.') unless booleans

    Configuration.new(eps:, guess:, business_days:, periodic_compound:)
  end

  def self.configuration_decimal(value, name:)
    value = Flt::DecNum.new(value) if value.is_a?(String)
    Validation.decimal(value, name:)
  end
  private_class_method :build_configuration, :configuration_decimal
end
