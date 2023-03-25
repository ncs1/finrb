# frozen_string_literal: true

EPSILON_ERR = D('0.00001')

describe('Utils') do
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

  describe('cogs') do
    it('Example 1') do
      cogs = Utils.cogs(uinv: 2, pinv: 2, units: [3, 5], price: [3, 5], sinv: 7, method: 'FIFO')
      expect(cogs[:cost_of_goods]).to(be_within(EPSILON_ERR).of(D(23)))
      expect(cogs[:ending_inventory]).to(be_within(EPSILON_ERR).of(D(15)))
    end

    it('Example 2') do
      cogs = Utils.cogs(uinv: 2, pinv: 2, units: [3, 5], price: [3, 5], sinv: 7, method: 'LIFO')
      expect(cogs[:cost_of_goods]).to(be_within(EPSILON_ERR).of(D(31)))
      expect(cogs[:ending_inventory]).to(be_within(EPSILON_ERR).of(D(7)))
    end

    it('Example 3') do
      cogs = Utils.cogs(uinv: 2, pinv: 2, units: [3, 5], price: [3, 5], sinv: 7, method: 'WAC')
      expect(cogs[:cost_of_goods]).to(be_within(EPSILON_ERR).of(D('26.6')))
      expect(cogs[:ending_inventory]).to(be_within(EPSILON_ERR).of(D('11.4')))
    end
  end

  describe('current_ratio') do
    it('Example 1') do
      expect(Utils.current_ratio(ca: 8000, cl: 2000)).to(be_within(EPSILON_ERR).of(D('4')))
    end
  end

  describe('ddb') do
    it('Example 1') do
      ddb_result_hsh_expect = { t: (0..4).to_a, ddb: [D('480'), D('288'), D('172.8'), D('59.2'), D('0')] }

      ddb_result_hsh = Utils.ddb(cost: 1200, rv: 200, t: 5)

      expect(ddb_result_hsh.size).to(eq(ddb_result_hsh_expect.size))
      expect(ddb_result_hsh[:t]).to(eq(ddb_result_hsh_expect[:t]))

      ddb_result_hsh[:ddb].each_with_index do |ddb_result, idx|
        expect(ddb_result).to(be_within(EPSILON_ERR).of(ddb_result_hsh_expect[:ddb][idx]))
      end
    end
  end

  describe('debt_ratio') do
    it('Example 1') do
      expect(Utils.debt_ratio(td: 6000, ta: 20_000)).to(be_within(EPSILON_ERR).of(D('0.3')))
    end
  end

  describe('diluted_eps') do
    it('Example 1') do
      expect(Utils.diluted_eps(ni: 115_600, pd: 10_000, cdi: 42_000, tax: 0.4, w: 200_000, cds: 60_000)).to(be_within(EPSILON_ERR).of(D('0.5030769')))
    end

    it('Example 2') do
      expect(Utils.diluted_eps(ni: 115_600, pd: 10_000, cpd: 10_000, w: 200_000, cps: 40_000)).to(be_within(EPSILON_ERR).of(D('0.4816667')))
    end

    it('Example 3') do
      expect(Utils.diluted_eps(ni: 115_600, pd: 10_000, w: 200_000, iss: 2500)).to(be_within(EPSILON_ERR).of(D('0.5214815')))
    end

    it('Example 4') do
      expect(Utils.diluted_eps(ni: 115_600, pd: 10_000, cpd: 10_000, cdi: 42_000, tax: 0.4, w: 200_000, cps: 40_000, cds: 60_000, iss: 2500)).to(be_within(EPSILON_ERR).of(D('0.4654545')))
    end
  end

  describe('discount_rate') do
    it('Example 1') do
      expect(Utils.discount_rate(n: 5, pv: 0, fv: 600, pmt: -100, type: 0)).to(be_within(EPSILON_ERR).of(D('0.0912806')))
    end
  end

  describe('ear') do
    it('Example 1') do
      expect(Utils.ear(r: 0.12, m: 12)).to(be_within(EPSILON_ERR).of(D('0.126825')))
    end

    it('Example 2') do
      expect(Utils.ear(r: 0.04, m: 365)).to(be_within(EPSILON_ERR).of(D('0.04080849')))
    end
  end

  describe('ear_continuous') do
    it('Example 1') do
      expect(Utils.ear_continuous(r: 0.1)).to(be_within(EPSILON_ERR).of(D('0.1051709')))
    end

    it('Example 2') do
      expect(Utils.ear_continuous(r: 0.03)).to(be_within(EPSILON_ERR).of(D('0.03045453')))
    end
  end

  describe('ear2bey') do
    it('Example 1') do
      expect(Utils.ear2bey(ear: 0.08)).to(be_within(EPSILON_ERR).of(D('0.07846097')))
    end
  end

  describe('ear2hpr') do
    it('Example 1') do
      expect(Utils.ear2hpr(ear: 0.05039, t: 150)).to(be_within(EPSILON_ERR).of(D('0.02040884')))
    end
  end

  describe('eir') do
    it('Example 1') do
      expect(Utils.eir(r: 0.05, n: 1, p: 12)).to(be_within(EPSILON_ERR).of(D('0.004074124')))
    end

    it('Example 2') do
      expect(Utils.eir(r: 0.05, n: 2, p: 12)).to(be_within(EPSILON_ERR).of(D('0.004123915')))
    end

    it('Example 3') do
      expect(Utils.eir(r: 0.05, n: 4, p: 12)).to(be_within(EPSILON_ERR).of(D('0.004149425')))
    end

    it('Example 4') do
      expect(Utils.eir(r: 0.05, n: 12, p: 1)).to(be_within(EPSILON_ERR).of(D('0.0511619')))
    end

    it('Example 5') do
      expect(Utils.ear(r: 0.05, m: 12)).to(be_within(EPSILON_ERR).of(D('0.0511619')))
    end

    it('Example 6') do
      expect(Utils.eir(r: 0.05, n: 1, p: 4)).to(be_within(EPSILON_ERR).of(D('0.01227223')))
    end

    it('Example 7') do
      expect(Utils.eir(r: 0.05, n: 12, p: 4)).to(be_within(EPSILON_ERR).of(D('0.01255216')))
    end

    it('Example 8') do
      expect(Utils.eir(r: 0.05, p: 12, type: 'p')).to(be_within(EPSILON_ERR).of(D('0.004166667')))
    end
  end

  describe('eps') do
    it('Example 1') do
      expect(Utils.eps(ni: 10_000, pd: 1000, w: 11_000)).to(be_within(EPSILON_ERR).of(D('0.8181818')))
    end
  end

  describe('financial_leverage') do
    it('Example 1') do
      expect(Utils.financial_leverage(te: 16_000, ta: 20_000)).to(be_within(EPSILON_ERR).of(D('1.25')))
    end
  end

  describe('fv') do
    it('Example 1') do
      expect(Utils.fv(r: 0.07, n: 10, pv: 1000, pmt: 10)).to(be_within(EPSILON_ERR).of(D('-2105.31583')))
    end
  end

  describe('fv_annuity') do
    it('Example 1') do
      expect(Utils.fv_annuity(r: 0.03, n: 12, pmt: -1000)).to(be_within(EPSILON_ERR).of(D('14192.02956')))
    end

    it('Example 2') do
      expect(Utils.fv_annuity(r: 0.03, n: 12, pmt: -1000, type: 1)).to(be_within(EPSILON_ERR).of(D('14617.79044')))
    end
  end

  describe('fv_simple') do
    it('Example 1') do
      expect(Utils.fv_simple(r: 0.08, n: 10, pv: -300)).to(be_within(EPSILON_ERR).of(D('647.6775')))
    end

    it('Example 2') do
      expect(Utils.fv_simple(r: 0.04, n: 20, pv: -50_000)).to(be_within(EPSILON_ERR).of(D('109556.15715')))
    end
  end

  describe('fv_uneven') do
    it('Example 1') do
      expect(Utils.fv_uneven(r: 0.1, cf: [-1000, -500, 0, 4000, 3500, 2000])).to(be_within(EPSILON_ERR).of(D('-8347.44')))
    end
  end

  describe('geometric_mean') do
    it('Example 1') do
      expect(Utils.geometric_mean(r: [-0.0934, 0.2345, 0.0892])).to(be_within(EPSILON_ERR).of(D('0.0682465')))
    end
  end

  describe('gpm') do
    it('Example 1') do
      expect(Utils.gpm(gp: 1000, rv: 20_000)).to(be_within(EPSILON_ERR).of(D('0.05')))
    end
  end

  describe('harmonic_mean') do
    it('Example 1') do
      expect(Utils.harmonic_mean(p: [8, 9, 10])).to(be_within(EPSILON_ERR).of(D('8.92562')))
    end
  end

  describe('hpr') do
    it('Example 1') do
      expect(Utils.hpr(ev: 33, bv: 30, cfr: 0.5)).to(be_within(EPSILON_ERR).of(D('0.1166667')))
    end
  end

  describe('hpr2bey') do
    it('Example 1') do
      expect(Utils.hpr2bey(hpr: 0.02, t: 3)).to(be_within(EPSILON_ERR).of(D('0.0808')))
    end
  end

  describe('hpr2ear') do
    it('Example 1') do
      expect(Utils.hpr2ear(hpr: 0.015228, t: 120)).to(be_within(EPSILON_ERR).of(D('0.04704234')))
    end
  end

  describe('hpr2mmy') do
    it('Example 1') do
      expect(Utils.hpr2mmy(hpr: 0.01523, t: 120)).to(be_within(EPSILON_ERR).of(D('0.04569')))
    end
  end

  describe('irr') do
    it('Example 1') do
      expect(Utils.irr(cf: [-5, 1.6, 2.4, 2.8])).to(be_within(EPSILON_ERR).of(D('0.155175727')))
    end
  end

  describe('iss') do
    it('Example 1') do
      expect(Utils.iss(amp: 20, ep: 15, n: 10_000)).to(be_within(EPSILON_ERR).of(D('2500')))
    end
  end

  describe('lt_d2e') do
    it('Example 1') do
      expect(Utils.lt_d2e(ltd: 8000, te: 20_000)).to(be_within(EPSILON_ERR).of(D('0.4')))
    end
  end

  describe('mmy2hpr') do
    it('Example 1') do
      expect(Utils.mmy2hpr(mmy: 0.04898, t: 150)).to(be_within(EPSILON_ERR).of(D('0.02040833')))
    end
  end

  describe('n_period') do
    it('Example 1') do
      expect(Utils.n_period(r: 0.1, pv: -10_000, fv: 60_000_000, pmt: -50_000, type: 0)).to(be_within(EPSILON_ERR).of(D('50.10995')))
    end

    it('Example 2') do
      expect(Utils.n_period(r: 0.1, pv: -10_000, fv: 60_000_000, pmt: -50_000, type: 1)).to(be_within(EPSILON_ERR).of(D('49.13733')))
    end
  end

  describe('npm') do
    it('Example 1') do
      expect(Utils.npm(ni: 8000, rv: 20_000)).to(be_within(EPSILON_ERR).of(D('0.4')))
    end
  end

  describe('npv') do
    it('Example 1') do
      expect(Utils.npv(r: 0.12, cf: [-5, 1.6, 2.4, 2.8])).to(be_within(EPSILON_ERR).of(D('0.3348214')))
    end
  end

  describe('pmt') do
    it('Example 1') do
      expect(Utils.pmt(r: 0.08, n: 10, pv: -1000, fv: 10)).to(be_within(EPSILON_ERR).of(D('148.3392')))
    end

    it('Example 2') do
      expect(Utils.pmt(r: 0.08, n: 10, pv: -1000, fv: 0)).to(be_within(EPSILON_ERR).of(D('149.029488')))
    end

    it('Example 3') do
      expect(Utils.pmt(r: 0.08, n: 10, pv: -1000, fv: 10, type: 1)).to(be_within(EPSILON_ERR).of(D('137.3511')))
    end
  end

  describe('pv') do
    it('Example 1') do
      expect(Utils.pv(r: 0.07, n: 10, fv: 1000, pmt: 10)).to(be_within(EPSILON_ERR).of(D('-578.5851')))
    end

    it('Example 2') do
      expect(Utils.pv(r: 0.05, n: 20, fv: 1000, pmt: 10, type: 1)).to(be_within(EPSILON_ERR).of(D('-507.7427')))
    end
  end

  describe('pv_annuity') do
    it('Example 1') do
      expect(Utils.pv_annuity(r: 0.03, n: 12, pmt: 1000)).to(be_within(EPSILON_ERR).of(D('-9954.004')))
    end

    it('Example 2') do
      expect(Utils.pv_annuity(r: 0.0425, n: 3, pmt: 30_000)).to(be_within(EPSILON_ERR).of(D('-82859.27543')))
    end
  end

  describe('pv_perpetuity') do
    it('Example 1') do
      expect(Utils.pv_perpetuity(r: 0.1, pmt: 1000, g: 0.02)).to(be_within(EPSILON_ERR).of(D('-12500')))
    end

    it('Example 2') do
      expect(Utils.pv_perpetuity(r: 0.1, pmt: 1000, type: 1)).to(be_within(EPSILON_ERR).of(D('-11000')))
    end

    it('Example 3') do
      expect(Utils.pv_perpetuity(r: 0.1, pmt: 1000)).to(be_within(EPSILON_ERR).of(D('-10000')))
    end
  end

  describe('pv_simple') do
    it('Example 1') do
      expect(Utils.pv_simple(r: 0.07, n: 10, fv: 100)).to(be_within(EPSILON_ERR).of(D('-50.83493')))
    end

    it('Example 2') do
      expect(Utils.pv_simple(r: 0.03, n: 3, fv: 1000)).to(be_within(EPSILON_ERR).of(D('-915.141659')))
    end
  end

  describe('pv_uneven') do
    it('Example 1') do
      expect(Utils.pv_uneven(r: 0.1, cf: [-1000, -500, 0, 4000, 3500, 2000])).to(be_within(EPSILON_ERR).of(D('-4711.912262')))
    end
  end

  describe('quick_ratio') do
    it('Example 1') do
      expect(Utils.quick_ratio(cash: 3000, ms: 2000, rc: 1000, cl: 2000)).to(be_within(EPSILON_ERR).of(D('3')))
    end
  end

  describe('r_continuous') do
    it('Example 1') do
      expect(Utils.r_continuous(r: 0.03, m: 4)).to(be_within(EPSILON_ERR).of(D('0.02988806')))
    end
  end

  describe('r_norminal') do
    it('Example 1') do
      expect(Utils.r_norminal(rc: 0.03, m: 1)).to(be_within(EPSILON_ERR).of(D('0.03045453')))
    end

    it('Example 2') do
      expect(Utils.r_norminal(rc: 0.03, m: 4)).to(be_within(EPSILON_ERR).of(D('0.03011278')))
    end
  end

  describe('r_perpetuity') do
    it('Example 1') do
      expect(Utils.r_perpetuity(pmt: 4.5, pv: -75)).to(be_within(EPSILON_ERR).of(D('0.06')))
    end
  end

  describe('sampling_error') do
    it('Example 1') do
      expect(Utils.sampling_error(sm: 0.45, mu: 0.5)).to(be_within(EPSILON_ERR).of(D('-0.05')))
    end
  end

  describe('sf_ratio') do
    it('Example 1') do
      expect(Utils.sf_ratio(rp: 0.09, rl: 0.03, sd: 0.12)).to(be_within(EPSILON_ERR).of(D('0.5')))
    end
  end

  describe('sharpe_ratio') do
    it('Example 1') do
      expect(Utils.sharpe_ratio(rp: 0.038, rf: 0.015, sd: 0.07)).to(be_within(EPSILON_ERR).of(D('0.3285714')))
    end
  end

  describe('slde') do
    it('Example 1') do
      expect(Utils.slde(cost: 1200, rv: 200, t: 5)).to(be_within(EPSILON_ERR).of(D('200')))
    end
  end

  describe('total_d2e') do
    it('Example 1') do
      expect(Utils.total_d2e(td: 6000, te: 20_000)).to(be_within(EPSILON_ERR).of(D('0.3')))
    end
  end

  describe('twrr') do
    it('Example 1') do
      expect(Utils.twrr(ev: [120, 260], bv: [100, 240], cfr: [2, 4])).to(be_within(EPSILON_ERR).of(D('0.1584472')))
    end
  end

  describe('was') do
    it('Example 1') do
      s = [10_000, 2000]
      m = [12, 6]
      expect(Utils.was(ns: s, nm: m)).to(be_within(EPSILON_ERR).of(D('11000')))
    end

    it('Example 2') do
      s = [11_000, 4400, -3000]
      m = [12, 9, 4]
      expect(Utils.was(ns: s, nm: m)).to(be_within(EPSILON_ERR).of(D('13300')))
    end
  end

  describe('wpr') do
    it('Example 1') do
      expect(Utils.wpr(r: [0.12, 0.07, 0.03], w: [0.5, 0.4, 0.1])).to(be_within(EPSILON_ERR).of(D('0.091')))
    end
  end
end
