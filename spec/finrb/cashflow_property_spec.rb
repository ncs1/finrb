# frozen_string_literal: true

describe(Finrb::Cashflow) do
  let(:randomizer) { Random.new(20_260_825) }

  it('recovers generated periodic roots across rates, scales, and horizons') do
    25.times do
      expected = D(randomizer.rand(-0.8..3.0).to_s)
      inflows = Array.new(randomizer.rand(1..12)) { D(randomizer.rand(1.0..1_000_000.0).to_s) }
      initial = -inflows.each_with_index.sum(D(0)) { |amount, index| amount / ((expected + 1)**(index + 1)) }
      cashflows = [initial, *inflows]

      actual = described_class.irr(cashflows, expected)

      expect(actual).to(be_within(D('1e-13')).of(expected))
      expect(described_class.npv(cashflows, actual).abs).to(be <= D('1e-8'))
    end
  end

  it('recovers generated dated roots across irregular date spacing') do
    start = Date.new(2000, 1, 1)

    15.times do
      transactions, expected = generated_dated_case(start)

      actual = described_class.xirr(transactions, expected).effective

      expect(actual).to(be_within(D('1e-12')).of(expected))
      expect(described_class.xnpv(transactions, actual).abs).to(be <= D('1e-7'))
    end
  end

  def generated_dated_case(start)
    expected = D(randomizer.rand(-0.7..2.0).to_s)
    days = Array.new(randomizer.rand(1..8)) { randomizer.rand(1..5000) }
    days.uniq!
    days.sort!
    inflows = days.map { D(randomizer.rand(1.0..1_000_000.0).to_s) }
    initial = -inflows.zip(days).sum(D(0)) { |amount, day| amount / ((expected + 1)**(D(day) / 365)) }
    dated_inflows = inflows.zip(days)
    dated_inflows.map! { |amount, day| Finrb::Transaction.new(amount, date: start + day) }

    [[Finrb::Transaction.new(initial, date: start), *dated_inflows], expected]
  end
end
