# frozen_string_literal: true

describe('Cashflows') do
  describe('an array of numeric cashflows') do
    it('has an Internal Rate of Return') do
      expect([-4000, 1200, 1410, 1875, 1050].irr.round(3)).to(eq(D('0.143')))
      expect { [10, 20, 30].irr }
        .to(raise_error(ArgumentError))
    end

    it('has a Net Present Value') do
      expect([-100.0, 60, 60, 60].npv(0.1).round(3)).to(eq(D('49.211')))
    end
  end

  describe('guess with business days') do
    before(:all) do
      Finrb.config.business_days = true
      Finrb.config.periodic_compound = true
      @transactions = []
      (@transactions << Transaction.new(-2_906_071.23, date: Date.new(2017, 8, 31)))
      (@transactions << Transaction.new(8000.0, date: Date.new(2017, 9, 4)))
      (@transactions << Transaction.new(2_876_570.16, date: Date.new(2017, 9, 29)))
    end

    after(:all) do
      Finrb.config.business_days = false
      Finrb.config.periodic_compound = false
    end

    it('fails to calculate with default guess (1.0)') do
      expect { @transactions.xirr.apr.to_i }
        .to(raise_error(Flt::Num::InvalidOperation))
    end

    it('calculates correct rate with new guess (0.5)') do
      expect(@transactions.xirr(0.5).effective.round(5)).to(eq(D('-0.00742')))
    end
  end

  describe('guess') do
    before(:all) do
      @transactions = []
      (@transactions << Transaction.new(-1000, date: Time.new(1957, 1, 1)))
      (@transactions << Transaction.new(390_000, date: Time.new(2013, 1, 1)))
    end

    it('fails to calculate with default guess (1.0)') do
      expect(@transactions.xirr.apr.to_i).to(eq(-9_999_999_999_998))
    end

    it('calculates correct rate with new guess (0.1)') do
      expect(@transactions.xirr(0.1).apr.round(5)).to(eq(D('0.11234')))
    end

    it('does not allow non-numeric guesses') do
      expect { @transactions.xirr('error') }
        .to(raise_error(ArgumentError))
    end
  end
end
