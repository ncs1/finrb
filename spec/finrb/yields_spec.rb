# frozen_string_literal: true

describe(Finrb::Yields) do
  describe('bdy') do
    it('Example 1') do
      res = Yields.bdy(d: 1500, f: 100_000, t: 120)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.045')))
    end
  end

  describe('bdy2mmy') do
    it('Example 1') do
      res = Yields.bdy2mmy(bdy: 0.045, t: 120)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.04568528')))
    end
  end

  describe('ear') do
    it('Example 1') do
      res = Yields.ear(r: 0.12, m: 12)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.126825')))
    end

    it('Example 2') do
      res = Yields.ear(r: 0.04, m: 365)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.04080849')))
    end
  end

  describe('ear_continuous') do
    it('Example 1') do
      res = Yields.ear_continuous(r: 0.1)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.1051709')))
    end

    it('Example 2') do
      res = Yields.ear_continuous(r: 0.03)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.03045453')))
    end
  end

  describe('ear2bey') do
    it('Example 1') do
      res = Yields.ear2bey(ear: 0.08)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.07846097')))
    end
  end

  describe('ear2hpr') do
    it('Example 1') do
      res = Yields.ear2hpr(ear: 0.05039, t: 150)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.02040884')))
    end
  end

  describe('eir') do
    it('Example 1') do
      res = Yields.eir(r: 0.05, n: 1, p: 12)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.004074124')))
    end

    it('Example 2') do
      res = Yields.eir(r: 0.05, n: 2, p: 12)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.004123915')))
    end

    it('Example 3') do
      res = Yields.eir(r: 0.05, n: 4, p: 12)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.004149425')))
    end

    it('Example 4') do
      res = Yields.eir(r: 0.05, n: 12, p: 1)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.0511619')))
    end

    it('Example 5') do
      res = Yields.ear(r: 0.05, m: 12)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.0511619')))
    end

    it('Example 6') do
      res = Yields.eir(r: 0.05, n: 1, p: 4)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.01227223')))
    end

    it('Example 7') do
      res = Yields.eir(r: 0.05, n: 12, p: 4)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.01255216')))
    end

    it('Example 8') do
      res = Yields.eir(r: 0.05, p: 12, type: 'p')
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.004166667')))
    end
  end

  describe('hpr2bey') do
    it('Example 1') do
      res = Yields.hpr2bey(hpr: 0.02, t: 3)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.0808')))
    end
  end

  describe('hpr2ear') do
    it('Example 1') do
      res = Yields.hpr2ear(hpr: 0.015228, t: 120)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.04704234')))
    end
  end

  describe('hpr2mmy') do
    it('Example 1') do
      res = Yields.hpr2mmy(hpr: 0.01523, t: 120)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.04569')))
    end
  end

  describe('mmy2hpr') do
    it('Example 1') do
      res = Yields.mmy2hpr(mmy: 0.04898, t: 150)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.02040833')))
    end
  end

  describe('r_continuous') do
    it('Example 1') do
      res = Yields.r_continuous(r: 0.03, m: 4)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.02988806')))
    end
  end

  describe('r_norminal') do
    it('Example 1') do
      res = Yields.r_norminal(rc: 0.03, m: 1)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.03045453')))
    end

    it('Example 2') do
      res = Yields.r_norminal(rc: 0.03, m: 4)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.03011278')))
    end
  end
end
