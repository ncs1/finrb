# frozen_string_literal: true

describe(Finrb::Returns) do
  describe('coefficient_variation') do
    it('Example 1') do
      res = Returns.coefficient_variation(sd: 0.15, avg: 0.39)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.3846154')))
    end
  end

  describe('geometric_mean') do
    it('Example 1') do
      res = Returns.geometric_mean(r: [-0.0934, 0.2345, 0.0892])
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.0682465')))
    end

    it('accepts a single return') do
      expect(Returns.geometric_mean(r: 0.1)).to(eq(D('0.1')))
    end
  end

  describe('harmonic_mean') do
    it('Example 1') do
      res = Returns.harmonic_mean(p: [8, 9, 10])
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('8.92562')))
    end
  end

  describe('hpr') do
    it('Example 1') do
      res = Returns.hpr(ev: 33, bv: 30, cfr: 0.5)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.1166667')))
    end
  end

  describe('sampling_error') do
    it('Example 1') do
      res = Returns.sampling_error(sm: 0.45, mu: 0.5)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-0.05')))
    end
  end

  describe('sf_ratio') do
    it('Example 1') do
      res = Returns.sf_ratio(rp: 0.09, rl: 0.03, sd: 0.12)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.5')))
    end
  end

  describe('sharpe_ratio') do
    it('Example 1') do
      res = Returns.sharpe_ratio(rp: 0.038, rf: 0.015, sd: 0.07)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.3285714')))
    end
  end

  describe('twrr') do
    it('Example 1') do
      res = Returns.twrr(ev: [120, 260], bv: [100, 240], cfr: [2, 4])
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.1584472')))
    end
  end

  describe('wpr') do
    it('Example 1') do
      res = Returns.wpr(r: [0.12, 0.07, 0.03], w: [0.5, 0.4, 0.1])
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.091')))
    end
  end
end
