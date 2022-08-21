# frozen_string_literal: true

describe('Utils') do
  EPSILON_ERR = D('0.00001')

  describe('bdy') do
    it('Example 1') do
      expect(Utils.bdy(d: 1500, f: 100_000, t: 120)).to(be_within(EPSILON_ERR).of(D('0.045')))
    end
  end

  describe('bdy2mmy') do
    it('Example 1') do
      expect(Utils.bdy2mmy(bdy: 0.045, t: 120)).to(be_within(EPSILON_ERR).of(D('0.04568528')))
    end
  end

  describe('cash_ratio') do
    it('Example 1') do
      expect(Utils.cash_ratio(cash: 3000, ms: 2000, cl: 2000)).to(be_within(EPSILON_ERR).of(D('2.5')))
    end
  end

  describe('coefficient_variation') do
    it('Example 1') do
      expect(Utils.coefficient_variation(sd: 0.15, avg: 0.39)).to(be_within(EPSILON_ERR).of(D('0.3846154')))
    end
  end

end
