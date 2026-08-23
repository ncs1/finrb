# frozen_string_literal: true

describe(Finrb::Ratios) do
  describe('cash_ratio') do
    it('Example 1') do
      res = Ratios.cash_ratio(cash: 3000, ms: 2000, cl: 2000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('2.5')))
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
  end

  describe('eps') do
    it('Example 1') do
      res = Ratios.eps(ni: 10_000, pd: 1000, w: 11_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.8181818')))
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
  end
end
