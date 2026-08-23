# frozen_string_literal: true

require 'date'
require 'json'
require_relative '../lib/finrb'

# Evaluates bounded batches for the external solver oracle harness.
module FinrbSolverAdapter
  module_function

  def solve(test_case)
    case test_case.fetch('kind')
    when 'irr'
      test_case.fetch('amounts').irr(test_case.fetch('guess'))
    when 'xirr'
      transactions =
        test_case.fetch('transactions').map do |transaction|
          Finrb::Transaction.new(transaction.fetch('amount'), date: Date.iso8601(transaction.fetch('date')))
        end
      transactions.xirr(test_case.fetch('guess')).effective
    else
      raise(ArgumentError, "Unknown case kind: #{test_case.fetch('kind')}")
    end
  end
end

$stdin.each_line do |line|
  payload = JSON.parse(line)
  results =
    payload.fetch('cases').map do |test_case|
      { value: FinrbSolverAdapter.solve(test_case).to_s }
    rescue Finrb::Error, ArgumentError => e
      { error: e.class.name, message: e.message }
    end

  puts(JSON.generate(results:))
  $stdout.flush
end
