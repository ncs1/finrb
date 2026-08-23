# frozen_string_literal: true

require 'finrb/core_ext'

# Split from cashflow_spec.rb because these slower metamorphic scenarios form a
# distinct test concern.

describe(Finrb::Cashflow) do
  context('with financial invariants') do
    let(:amounts) { [-10_000, 1800, 2200, 2900, 4100] }

    it('does not change IRR when every cashflow is scaled or sign-inverted') do
      expected = amounts.irr(0.1)

      expect(
        amounts.map { |amount| amount * 1_000_000 }
        .irr(0.1)
      ).to(be_within(D('1e-15')).of(expected))
      expect(amounts.map(&:-@).irr(0.1)).to(be_within(D('1e-15')).of(expected))
    end

    it('matches the closed-form return for a single-period investment') do
      expect([-125, 200].irr).to(be_within(D('1e-15')).of(D('0.6')))
      expect([-125, 125].irr).to(be_within(D('1e-15')).of(D('0')))
    end

    it('does not change XIRR when all dates are shifted equally') do
      original = dated_transactions(Date.new(2000, 1, 1))
      shifted = dated_transactions(Date.new(2125, 7, 19))

      expect(shifted.xirr(0.1).effective).to(be_within(D('1e-15')).of(original.xirr(0.1).effective))
    end

    it('matches periodic IRR when dated periods are exactly 365 days apart') do
      start = Date.new(2001, 1, 1)
      transactions =
        amounts.each_with_index.map do |amount, period|
          Finrb::Transaction.new(amount, date: start + (period * 365))
        end

      expect(transactions.xirr(0.1).effective).to(be_within(D('1e-15')).of(amounts.irr(0.1)))
    end

    it('is unaffected by a dated zero cashflow') do
      transactions = dated_transactions(Date.new(2000, 1, 1))
      expected = transactions.xirr(0.1).effective
      transactions.insert(2, Finrb::Transaction.new(0, date: Date.new(2002, 6, 1)))

      expect(transactions.xirr(0.1).effective).to(be_within(D('1e-15')).of(expected))
    end

    it('finds a repeated root only when the exact non-sign-changing root is supplied') do
      repeated_root = [1, -2.2, 1.21]

      expect(repeated_root.irr(0.1)).to(be_within(D('1e-15')).of(D('0.1')))
      expect { repeated_root.irr(0) }
        .to(raise_error(Finrb::ConvergenceError, /Could not bracket/))
    end

    def dated_transactions(start)
      amounts.each_with_index.map do |amount, period|
        Finrb::Transaction.new(amount, date: start + (period * 487))
      end
    end
  end
end
