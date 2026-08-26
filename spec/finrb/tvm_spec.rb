# frozen_string_literal: true

describe(Finrb::TVM) do
  describe('discount_rate') do
    it('Example 1') do
      res = TVM.discount_rate(n: 5, pv: 0, fv: 600, pmt: -100, type: 0)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.0912806')))
    end

    it('finds exact zero and negative rates') do
      expect(TVM.discount_rate(n: 1, pv: -100, fv: 100, pmt: 0)).to(eq(D('0')))
      expect(TVM.discount_rate(n: 1, pv: -100, fv: 90, pmt: 0)).to(be_within(D('1e-14')).of(D('-0.1')))
    end

    it('accepts an explicit valid bracket') do
      result = TVM.discount_rate(n: 1, pv: -100, fv: 90, pmt: 0, lower: -0.2, upper: 0)

      expect(result).to(be_within(D('1e-14')).of(D('-0.1')))
    end

    it('preserves the default opposite bound when overriding one bound') do
      expect(TVM.discount_rate(n: 1, pv: -100, fv: 90, pmt: 0, lower: -0.2)).to(be_within(D('1e-14')).of(D('-0.1')))
    end

    it('validates explicit bounds') do
      expect { TVM.discount_rate(n: 1, pv: -100, fv: 90, pmt: 0, lower: 0, upper: -0.2) }
        .to(raise_error(ArgumentError, /lower must be less/))
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

    it('uses the zero-rate mathematical limit') do
      expect(TVM.fv_annuity(r: 0, n: 12, pmt: -100)).to(eq(D('1200')))
      expect(TVM.fv_annuity(r: 0, n: 12, pmt: -100, type: 1)).to(eq(D('1200')))
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

    it('solves the zero-rate linear case') do
      expect(TVM.n_period(r: 0, pv: -1000, fv: 0, pmt: 100)).to(eq(D('10')))
    end

    it('rejects an indeterminate zero-rate case') do
      expect { TVM.n_period(r: 0, pv: -1000, fv: 1000, pmt: 0) }
        .to(raise_error(Finrb::DomainError, /pmt must be non-zero/))
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

    it('uses the zero-rate mathematical limit') do
      expect(TVM.pmt(r: 0, n: 12, pv: -1200, fv: 0)).to(eq(D('100')))
    end

    it('rejects a zero payment period count') do
      expect { TVM.pmt(r: 0.1, n: 0, pv: -1000, fv: 0) }
        .to(raise_error(Finrb::DomainError, /greater than zero/))
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

    it('uses the zero-rate mathematical limit') do
      expect(TVM.pv_annuity(r: 0, n: 12, pmt: 100)).to(eq(D('-1200')))
      expect(TVM.pv_annuity(r: 0, n: 12, pmt: 100, type: 1)).to(eq(D('-1200')))
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

    it('accepts a single cashflow') do
      expect(TVM.pv_uneven(r: 0.1, cf: 100)).to(eq(D('-90.90909090909090909090909091')))
    end
  end

  describe('r_perpetuity') do
    it('Example 1') do
      res = TVM.r_perpetuity(pmt: 4.5, pv: -75)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('0.06')))
    end
  end

  describe('financial invariants') do
    it('round-trips present and future values at a negative rate') do
      future = TVM.fv(r: -0.02, n: 12, pv: -1000)
      present = TVM.pv(r: -0.02, n: 12, fv: -future)

      expect(present).to(be_within(D('1e-20')).of(D('1000')))
    end

    it('reconstructs the target future value from the solved payment') do
      payment = TVM.pmt(r: -0.02, n: 12, pv: -1000, fv: 0)

      expect(TVM.fv(r: -0.02, n: 12, pv: -1000, pmt: payment)).to(be_within(D('1e-20')).of(D('0')))
    end
  end

  describe('public input validation') do
    it('rejects non-numeric and non-finite inputs') do
      expect { TVM.fv_simple(r: Float::NAN, n: 2, pv: 100) }
        .to(raise_error(ArgumentError, /r must be finite/))
      expect { TVM.pv_simple(r: 0.1, n: Float::INFINITY, fv: 100) }
        .to(raise_error(ArgumentError, /n must be finite/))
      expect { TVM.fv_annuity(r: 0.1, n: 2, pmt: '100') }
        .to(raise_error(ArgumentError, /pmt must be numeric/))
    end

    it('rejects invalid rates and periods') do
      expect { TVM.fv_simple(r: -1, n: 2, pv: 100) }
        .to(raise_error(Finrb::DomainError, /greater than -1/))
      expect { TVM.pv_simple(r: 0.1, n: -1, fv: 100) }
        .to(raise_error(Finrb::DomainError, /non-negative/))
    end

    it('rejects invalid payment types and cashflows') do
      expect { TVM.fv(r: 0.1, n: 2, type: 2) }
        .to(raise_error(ArgumentError, /type must be 0 or 1/))
      expect { TVM.npv(r: 0.1, cf: []) }
        .to(raise_error(ArgumentError, /cf cannot be empty/))
      expect { TVM.pv_uneven(r: 0.1, cf: []) }
        .to(raise_error(ArgumentError, /cf cannot be empty/))
    end

    it('reports undefined perpetuity inputs') do
      expect { TVM.pv_perpetuity(r: 0.02, pmt: 100, g: 0.02) }
        .to(raise_error(Finrb::DomainError, /Growth rate/))
      expect { TVM.r_perpetuity(pmt: 100, pv: 0) }
        .to(raise_error(Finrb::DomainError, /non-zero/))
    end
  end
end
