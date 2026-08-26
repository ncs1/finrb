# frozen_string_literal: true

describe(Finrb::Ratios) do
  describe('cash_ratio') do
    it('Example 1') do
      res = Ratios.cash_ratio(cash: 3000, ms: 2000, cl: 2000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('2.5')))
    end

    it('requires finite inputs and non-zero current liabilities') do
      expect { Ratios.cash_ratio(cash: Float::INFINITY, ms: 0, cl: 1) }
        .to(raise_error(ArgumentError, /cash must be finite/))
      expect { Ratios.cash_ratio(cash: 1, ms: 0, cl: 0) }
        .to(raise_error(Finrb::DomainError, /current liabilities must be non-zero/))
    end
  end

  describe('current_ratio') do
    it('Example 1') do
      res = Ratios.current_ratio(ca: 8000, cl: 2000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('4')))
    end
  end

  describe('debt_ratio') do
    it('Example 1') do
      res = Ratios.debt_ratio(td: 6000, ta: 20_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.3')))
    end
  end

  describe('diluted_eps') do
    it('Example 1') do
      res = Ratios.diluted_eps(ni: 115_600, pd: 10_000, cdi: 42_000, tax: 0.4, w: 200_000, cds: 60_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.5030769')))
    end

    it('Example 2') do
      res = Ratios.diluted_eps(ni: 115_600, pd: 10_000, cpd: 10_000, w: 200_000, cps: 40_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.4816667')))
    end

    it('Example 3') do
      res = Ratios.diluted_eps(ni: 115_600, pd: 10_000, w: 200_000, iss: 2500)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.5214815')))
    end

    it('Example 4') do
      res = Ratios.diluted_eps(ni: 115_600, pd: 10_000, cpd: 10_000, cdi: 42_000, tax: 0.4, w: 200_000, cps: 40_000, cds: 60_000, iss: 2500)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.4654545')))
    end

    it('validates share counts and tax rates') do
      expect { Ratios.diluted_eps(ni: 100, pd: 0, w: 0) }
        .to(raise_error(Finrb::DomainError, /weighted average common shares/))
      expect { Ratios.diluted_eps(ni: 100, pd: 0, w: 100, tax: 1.1) }
        .to(raise_error(ArgumentError, /tax rate must be between 0 and 1/))
      expect { Ratios.diluted_eps(ni: 100, pd: 0, w: 100, cds: -1) }
        .to(raise_error(ArgumentError, /convertible debt shares/))
    end
  end

  describe('eps') do
    it('Example 1') do
      res = Ratios.eps(ni: 10_000, pd: 1000, w: 11_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.8181818')))
    end

    it('requires positive weighted average shares') do
      expect { Ratios.eps(ni: 10_000, pd: 1000, w: 0) }
        .to(raise_error(Finrb::DomainError, /weighted average common shares/))
    end
  end

  describe('financial_leverage') do
    it('Example 1') do
      res = Ratios.financial_leverage(te: 16_000, ta: 20_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('1.25')))
    end
  end

  describe('gpm') do
    it('Example 1') do
      res = Ratios.gpm(gp: 1000, rv: 20_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.05')))
    end
  end

  describe('iss') do
    it('Example 1') do
      res = Ratios.iss(amp: 20, ep: 15, n: 10_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('2500')))
    end

    it('rejects options that are not in the money') do
      expect { Ratios.iss(amp: 15, ep: 15, n: 10_000) }
        .to(raise_error(Finrb::DomainError, /market price must be greater than exercise price/))
    end
  end

  describe('lt_d2e') do
    it('Example 1') do
      res = Ratios.lt_d2e(ltd: 8000, te: 20_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.4')))
    end
  end

  describe('npm') do
    it('Example 1') do
      res = Ratios.npm(ni: 8000, rv: 20_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.4')))
    end
  end

  describe('quick_ratio') do
    it('Example 1') do
      res = Ratios.quick_ratio(cash: 3000, ms: 2000, rc: 1000, cl: 2000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('3')))
    end
  end

  describe('total_d2e') do
    it('Example 1') do
      res = Ratios.total_d2e(td: 6000, te: 20_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.3')))
    end
  end

  describe('was') do
    it('Example 1') do
      s = [10_000, 2000]
      m = [12, 6]
      res = Ratios.was(ns: s, nm: m)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('11000')))
    end

    it('Example 2') do
      s = [11_000, 4400, -3000]
      m = [12, 9, 4]
      res = Ratios.was(ns: s, nm: m)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('13300')))
    end

    it('rejects mismatched share and month vectors') do
      expect { Ratios.was(ns: [100], nm: []) }
        .to(raise_error(ArgumentError, /must have equal lengths/))
    end

    it('accepts scalar and empty share histories') do
      expect(Ratios.was(ns: 100, nm: 12)).to(eq(D(100)))
      expect(Ratios.was(ns: nil, nm: nil)).to(eq(0))
    end

    it('validates share changes and months outstanding') do
      expect { Ratios.was(ns: ['100'], nm: [12]) }
        .to(raise_error(ArgumentError, /share change must be numeric/))
      expect { Ratios.was(ns: [100], nm: [13]) }
        .to(raise_error(ArgumentError, /months outstanding must be between 0 and 12/))
    end
  end

  describe('denominator validation') do
    {
      current_ratio: -> { Ratios.current_ratio(ca: 1, cl: 0) },
      debt_ratio: -> { Ratios.debt_ratio(td: 1, ta: 0) },
      financial_leverage: -> { Ratios.financial_leverage(te: 0, ta: 1) },
      gpm: -> { Ratios.gpm(gp: 1, rv: 0) },
      lt_d2e: -> { Ratios.lt_d2e(ltd: 1, te: 0) },
      npm: -> { Ratios.npm(ni: 1, rv: 0) },
      quick_ratio: -> { Ratios.quick_ratio(cash: 1, ms: 0, rc: 0, cl: 0) },
      total_d2e: -> { Ratios.total_d2e(td: 1, te: 0) }
    }.each do |name, calculation|
      it("rejects a zero denominator for #{name}") do
        expect(&calculation).to(raise_error(Finrb::DomainError, /must be non-zero/))
      end
    end
  end
end
