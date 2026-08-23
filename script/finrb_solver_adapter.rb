# frozen_string_literal: true

require 'date'
require 'json'
require_relative '../lib/finrb'

payload = JSON.parse($stdin.read)
results =
  payload.fetch('cases').map do |test_case|
    value =
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

    { value: value.to_s }
  rescue Finrb::Error, ArgumentError => e
    { error: e.class.name, message: e.message }
  end

puts(JSON.generate(results:))
