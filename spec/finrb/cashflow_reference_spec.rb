# frozen_string_literal: true

require 'json'

# Split from cashflow_spec.rb so external references have explicit provenance.

describe(Finrb::Cashflow) do
  let(:quantlib_fixture) { load_fixture('quantlib_yield_rate_xirr.json') }
  let(:scipy_fixture)    { load_fixture('scipy_brentq_irr.json')         }

  it('agrees with SciPy brentq under matching conventions') do
    scipy_fixture.fetch('cases').each do |test_case|
      expected = D(test_case.fetch('root'))
      cashflows = test_case.fetch('cashflows')
      actual = cashflows.irr(expected.to_f)

      expect(actual).to(be_within(D('2e-14')).of(expected))
      expect(cashflows.npv(actual).abs).to(be <= D('1e-8'))
    end
  end

  it('agrees with QuantLib CashFlows.yieldRate under matching conventions') do
    quantlib_fixture.fetch('cases').each do |test_case|
      expected = D(test_case.fetch('root'))
      transactions = transactions_for(test_case)
      actual = transactions.xirr(expected.to_f).effective

      expect(actual).to(be_within(D('2e-12')).of(expected))
      expect(transactions.xnpv(actual).abs).to(be <= D('1e-7'))
    end
  end

  def load_fixture(filename)
    JSON.parse(File.read(File.expand_path("../fixtures/#{filename}", __dir__)))
  end

  def transactions_for(test_case)
    test_case.fetch('transactions').map do |item|
      Finrb::Transaction.new(item.fetch('amount'), date: Date.iso8601(item.fetch('date')))
    end
  end
end
