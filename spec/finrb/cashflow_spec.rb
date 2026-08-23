# frozen_string_literal: true

describe(Finrb::Cashflow) do
  describe('an array of numeric cashflows') do
    it('has an Internal Rate of Return') do
      expect([-4000, 1200, 1410, 1875, 1050].irr.round(3)).to(eq(D('0.143')))
    end

    it('has a Net Present Value') do
      expect([-100.0, 60, 60, 60].npv(0.1).round(3)).to(eq(D('49.211')))
    end

    it('rejects cashflows without both positive and negative values') do
      expect { [10, 20, 30].irr }
        .to(raise_error(Finrb::InvalidCashflowError))
    end

    it('selects between multiple IRRs using the guess') do
      cashflows = [-100, 230, -132]

      expect(cashflows.irr(0.05)).to(be_within(D('1e-14')).of(D('0.1')))
      expect(cashflows.irr(0.25)).to(be_within(D('1e-14')).of(D('0.2')))
    end

    it('reports a cashflow with no sign-changing IRR') do
      expect { [-100, 50, -100].irr(0) }
        .to(raise_error(Finrb::ConvergenceError, /Could not bracket/))
    end
  end

  describe('dated cashflows') do
    it('calculates the same XNPV for equivalent Date and Time values') do
      date_transactions = [Transaction.new(-1000, date: Date.new(2020, 1, 1)), Transaction.new(1100, date: Date.new(2021, 1, 1))]
      time_transactions = [Transaction.new(-1000, date: Time.utc(2020, 1, 1)), Transaction.new(1100, date: Time.utc(2021, 1, 1))]

      expect(date_transactions.xnpv(0.1)).to(eq(time_transactions.xnpv(0.1)))
    end
  end

  describe('guess with business days') do
    before do
      Finrb.config.business_days = true
      Finrb.config.periodic_compound = true
      @transactions = []
      (@transactions << Transaction.new(-2_906_071.23, date: Date.new(2017, 8, 31)))
      (@transactions << Transaction.new(8000.0, date: Date.new(2017, 9, 4)))
      (@transactions << Transaction.new(2_876_570.16, date: Date.new(2017, 9, 29)))
    end

    after do
      Finrb.config.business_days = false
      Finrb.config.periodic_compound = false
    end

    it('calculates with the default guess') do
      expect(@transactions.xirr.effective.round(5)).to(eq(D('-0.00742')))
    end

    it('calculates correct rate with new guess (0.5)') do
      expect(@transactions.xirr(0.5).effective.round(5)).to(eq(D('-0.00742')))
    end

    it('counts weekdays from the start date up to the end date') do
      expect(@transactions.__send__(:date_diff, Date.new(2026, 8, 21), Date.new(2026, 8, 24))).to(eq(1))
      expect(@transactions.__send__(:date_diff, Date.new(2026, 8, 22), Date.new(2026, 8, 24))).to(eq(0))
      expect(@transactions.__send__(:date_diff, Date.new(2026, 8, 24), Date.new(2026, 8, 31))).to(eq(5))
    end
  end

  describe('guess') do
    before do
      @transactions = []
      (@transactions << Transaction.new(-1000, date: Time.new(1957, 1, 1)))
      (@transactions << Transaction.new(390_000, date: Time.new(2013, 1, 1)))
    end

    it('calculates with the default guess') do
      expect(@transactions.xirr.apr.round(5)).to(eq(D('0.11234')))
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
