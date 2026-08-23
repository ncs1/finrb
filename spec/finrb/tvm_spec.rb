# frozen_string_literal: true

describe(Finrb::TVM) do
  describe('discount_rate') do
    it('Example 1') do
      res = TVM.discount_rate(n: 5, pv: 0, fv: 600, pmt: -100, type: 0)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.0912806')))
    end
  end

  describe('fv') do
    it('Example 1') do
      res = TVM.fv(r: 0.07, n: 10, pv: 1000, pmt: 10)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-2105.31583')))
    end
  end

  describe('fv_annuity') do
    it('Example 1') do
      res = TVM.fv_annuity(r: 0.03, n: 12, pmt: -1000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('14192.02956')))
    end

    it('Example 2') do
      res = TVM.fv_annuity(r: 0.03, n: 12, pmt: -1000, type: 1)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('14617.79044')))
    end
  end

  describe('fv_simple') do
    it('Example 1') do
      res = TVM.fv_simple(r: 0.08, n: 10, pv: -300)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('647.6775')))
    end

    it('Example 2') do
      res = TVM.fv_simple(r: 0.04, n: 20, pv: -50_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('109556.15715')))
    end
  end

  describe('fv_uneven') do
    it('Example 1') do
      res = TVM.fv_uneven(r: 0.1, cf: [-1000, -500, 0, 4000, 3500, 2000])
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-8347.44')))
    end
  end

  describe('n_period') do
    it('Example 1') do
      res = TVM.n_period(r: 0.1, pv: -10_000, fv: 60_000_000, pmt: -50_000, type: 0)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('50.10995')))
    end

    it('Example 2') do
      res = TVM.n_period(r: 0.1, pv: -10_000, fv: 60_000_000, pmt: -50_000, type: 1)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('49.13733')))
    end
  end

  describe('npv') do
    it('Example 1') do
      res = TVM.npv(r: 0.12, cf: [-5, 1.6, 2.4, 2.8])
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.3348214')))
    end
  end

  describe('pmt') do
    it('Example 1') do
      res = TVM.pmt(r: 0.08, n: 10, pv: -1000, fv: 10)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('148.3392')))
    end

    it('Example 2') do
      res = TVM.pmt(r: 0.08, n: 10, pv: -1000, fv: 0)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('149.029488')))
    end

    it('Example 3') do
      res = TVM.pmt(r: 0.08, n: 10, pv: -1000, fv: 10, type: 1)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('137.3511')))
    end
  end

  describe('pv') do
    it('Example 1') do
      res = TVM.pv(r: 0.07, n: 10, fv: 1000, pmt: 10)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-578.5851')))
    end

    it('Example 2') do
      res = TVM.pv(r: 0.05, n: 20, fv: 1000, pmt: 10, type: 1)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-507.7427')))
    end
  end

  describe('pv_annuity') do
    it('Example 1') do
      res = TVM.pv_annuity(r: 0.03, n: 12, pmt: 1000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-9954.004')))
    end

    it('Example 2') do
      res = TVM.pv_annuity(r: 0.0425, n: 3, pmt: 30_000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-82859.27543')))
    end
  end

  describe('pv_perpetuity') do
    it('Example 1') do
      res = TVM.pv_perpetuity(r: 0.1, pmt: 1000, g: 0.02)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-12500')))
    end

    it('Example 2') do
      res = TVM.pv_perpetuity(r: 0.1, pmt: 1000, type: 1)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-11000')))
    end

    it('Example 3') do
      res = TVM.pv_perpetuity(r: 0.1, pmt: 1000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-10000')))
    end
  end

  describe('pv_simple') do
    it('Example 1') do
      res = TVM.pv_simple(r: 0.07, n: 10, fv: 100)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-50.83493')))
    end

    it('Example 2') do
      res = TVM.pv_simple(r: 0.03, n: 3, fv: 1000)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-915.141659')))
    end
  end

  describe('pv_uneven') do
    it('Example 1') do
      res = TVM.pv_uneven(r: 0.1, cf: [-1000, -500, 0, 4000, 3500, 2000])
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('-4711.912262')))
    end
  end

  describe('r_perpetuity') do
    it('Example 1') do
      res = TVM.r_perpetuity(pmt: 4.5, pv: -75)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.06')))
    end
  end
end
