# frozen_string_literal: true

require('benchmark/ips')
require('date')
require('optparse')
require_relative('../lib/finrb')

# Deterministic benchmark scenarios and correctness preflight for finrb.
module FinrbBenchmark
  Scenario = Data.define(:name, :operation, :residual)
  class VerificationError < StandardError; end
  MAX_NORMALIZED_RESIDUAL = Flt::DecNum('1e-12')
  private_constant :Scenario, :VerificationError, :MAX_NORMALIZED_RESIDUAL

  # Counts solver objective evaluations without changing production APIs.
  module EvaluationCounter
    def npv(rate)
      FinrbBenchmark.increment_evaluations
      super
    end

    def xnpv(rate)
      FinrbBenchmark.increment_evaluations
      super
    end
  end
  private_constant :EvaluationCounter

  module_function

  def increment_evaluations
    Thread.current[:finrb_benchmark_evaluations] = evaluations + 1
  end

  def evaluations
    Thread.current[:finrb_benchmark_evaluations] || 0
  end

  def reset_evaluations
    Thread.current[:finrb_benchmark_evaluations] = 0
  end

  def periodic_cashflows(size:, rate:)
    terminal = Flt::DecNum(1000) * ((Flt::DecNum(1) + rate)**(size - 1))
    [-Flt::DecNum(1000), *Array.new(size - 2, Flt::DecNum(0)), terminal]
  end

  def dated_cashflows(size:, rate:, random:)
    start_date = Date.new(2000, 1, 1)
    offsets = Array.new(size) { |index| (index * 17) + random.rand(0..3) }
    offsets[0] = 0
    offsets.each_cons(2).with_index { |(left, right), index| offsets[index + 1] = left + 1 if right <= left }
    terminal = Flt::DecNum(1000) * ((Flt::DecNum(1) + rate)**(Flt::DecNum(offsets.last) / 365))

    offsets.each_with_index.map do |offset, index|
      amount = index.zero? ? -Flt::DecNum(1000) : Flt::DecNum(0)
      amount = terminal if index == offsets.size - 1
      Finrb::Transaction.new(amount, date: start_date + offset)
    end
  end

  def normalized_residual(cashflows, rate, function)
    residual = Finrb::Cashflow.public_send(function, cashflows, rate).abs
    scale = cashflows.sum { |entry| entry.respond_to?(:amount) ? entry.amount.abs : entry.abs }
    residual / scale
  end

  def solver_scenario(kind:, size:, rate:, guess:, random:)
    cashflows = kind == :irr ? periodic_cashflows(size:, rate:) : dated_cashflows(size:, rate:, random:)
    solve = kind == :irr ? :irr : :xirr
    residual_function = kind == :irr ? :npv : :xnpv
    operation =
      lambda do
        result = Finrb::Cashflow.public_send(solve, cashflows, guess)
        result.respond_to?(:effective) ? result.effective : result
      end

    Scenario.new("#{kind}/#{size}", operation, ->(result) { normalized_residual(cashflows, result, residual_function) })
  end

  def amortization_scenario(periods:)
    operation =
      lambda do
        rate = Finrb::Rate.new(0.045, :apr, duration: periods)
        Finrb::Amortization.new(250_000, rate)
      end
    residual =
      lambda do |amortization|
        reconciliations =
          amortization.schedule.map do |entry|
            (entry.opening_balance + entry.interest + entry.payment - entry.closing_balance).abs
          end
        reconciliations.max || Flt::DecNum(0)
      end

    Scenario.new("amortization/#{periods}", operation, residual)
  end

  def scenarios(seed:)
    random = Random.new(seed)
    [
      solver_scenario(kind: :irr, size: 10, rate: Flt::DecNum('0.1'), guess: Flt::DecNum('0.05'), random:),
      solver_scenario(kind: :irr, size: 120, rate: Flt::DecNum('-0.02'), guess: Flt::DecNum('0.1'), random:),
      solver_scenario(kind: :irr, size: 1000, rate: Flt::DecNum('0.03'), guess: Flt::DecNum('0.1'), random:),
      solver_scenario(kind: :irr, size: 30, rate: Flt::DecNum('2.5'), guess: Flt::DecNum('0.1'), random:),
      solver_scenario(kind: :xirr, size: 12, rate: Flt::DecNum('0.08'), guess: Flt::DecNum('0.1'), random:),
      solver_scenario(kind: :xirr, size: 250, rate: Flt::DecNum('-0.01'), guess: Flt::DecNum('0.1'), random:),
      amortization_scenario(periods: 120),
      amortization_scenario(periods: 360),
      amortization_scenario(periods: 600)
    ]
  end

  def verify(scenarios)
    puts('Correctness preflight:')
    scenarios.each do |scenario|
      reset_evaluations
      result = scenario.operation.call
      objective_evaluations = evaluations
      residual = scenario.residual.call(result)
      raise(VerificationError, "#{scenario.name} residual #{residual} exceeds #{MAX_NORMALIZED_RESIDUAL}") if residual > MAX_NORMALIZED_RESIDUAL

      evaluation_text = objective_evaluations.positive? ? " evaluations=#{objective_evaluations}" : ''
      puts("  #{scenario.name}: residual=#{residual}#{evaluation_text}")
    end
  end

  def run(argv)
    options = { time: 2, warmup: 1, seed: 20_260_826 }
    parser =
      OptionParser.new do |options_parser|
        options_parser.on('--time SECONDS', Integer) { |value| options[:time] = value }
        options_parser.on('--warmup SECONDS', Integer) { |value| options[:warmup] = value }
        options_parser.on('--seed SEED', Integer) { |value| options[:seed] = value }
      end
    parser.parse!(argv)
    raise(ArgumentError, 'time must be positive') unless options[:time].positive?
    raise(ArgumentError, 'warmup must be non-negative') if options[:warmup].negative?

    Finrb::Cashflow.prepend(EvaluationCounter)
    benchmark_scenarios = scenarios(seed: options[:seed])
    puts("ruby=#{RUBY_ENGINE} #{RUBY_VERSION} seed=#{options[:seed]}")
    verify(benchmark_scenarios)

    Benchmark.ips do |benchmark|
      benchmark.config(time: options[:time], warmup: options[:warmup])
      benchmark_scenarios.each do |scenario|
        benchmark.report(scenario.name, &scenario.operation)
      end
      benchmark.compare!
    end
  end
end

FinrbBenchmark.run(ARGV)
