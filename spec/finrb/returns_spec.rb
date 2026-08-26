# frozen_string_literal: true

describe(Finrb::Returns) do
  describe('cagr') do
    it('computes compound annual growth') do
      result = Returns.cagr(beginning_value: 10_000, ending_value: 16_105.1, periods: 5)

      expect(result).to(be_an_instance_of(Flt::DecNum))
      expect(result).to(be_within(D('0.0000000001')).of(D('0.1')))
    end

    it('computes a declining compound rate') do
      result = Returns.cagr(beginning_value: 100, ending_value: 64, periods: 2)

      expect(result).to(be_within(D('0.0000000001')).of(D('-0.2')))
    end

    it('represents a total loss as negative one') do
      expect(Returns.cagr(beginning_value: 100, ending_value: 0, periods: 3)).to(eq(D('-1')))
    end

    it('requires a positive beginning value') do
      expect { Returns.cagr(beginning_value: 0, ending_value: 100, periods: 2) }
        .to(raise_error(ArgumentError, /beginning value must be greater than zero/))
    end

    it('rejects a negative ending value') do
      expect { Returns.cagr(beginning_value: 100, ending_value: -1, periods: 2) }
        .to(raise_error(ArgumentError, /ending value must be greater than or equal to zero/))
    end

    it('requires a positive integer number of periods') do
      expect { Returns.cagr(beginning_value: 100, ending_value: 110, periods: 0) }
        .to(raise_error(ArgumentError, /period count must be a positive integer/))
      expect { Returns.cagr(beginning_value: 100, ending_value: 110, periods: 1.5) }
        .to(raise_error(ArgumentError, /period count must be a positive integer/))
    end
  end

  describe('annualization') do
    it('compounds periodic returns') do
      expect(Returns.annualize_return(rate: 0.01, periods_per_year: 12)).to(be_within(D('1e-14')).of(D('0.12682503013196972')))
    end

    it('scales periodic volatility by the square-root of time') do
      expect(Returns.annualize_volatility(volatility: 0.02, periods_per_year: 252)).to(be_within(D('1e-14')).of(D('0.31749015732775088')))
    end

    it('validates annualization inputs') do
      expect { Returns.annualize_return(rate: -1.01, periods_per_year: 12) }
        .to(raise_error(ArgumentError, /rate must be greater/))
      expect { Returns.annualize_volatility(volatility: -0.1, periods_per_year: 12) }
        .to(raise_error(ArgumentError, /volatility must be greater/))
      expect { Returns.annualize_return(rate: 0.01, periods_per_year: 0) }
        .to(raise_error(ArgumentError, /positive integer/))
    end
  end

  describe('volatility') do
    it('computes sample volatility by default') do
      expect(Returns.volatility(returns: [0.1, 0.2, 0.3])).to(be_within(D('1e-14')).of(D('0.1')))
    end

    it('computes population volatility when requested') do
      expect(Returns.volatility(returns: [0.1, 0.2, 0.3], sample: false)).to(be_within(D('1e-14')).of(D('0.08164965809277261')))
    end

    it('requires enough observations') do
      expect { Returns.volatility(returns: [], sample: false) }
        .to(raise_error(ArgumentError, /cannot be empty/))
      expect { Returns.volatility(returns: [0.1]) }
        .to(raise_error(ArgumentError, /at least two/))
      expect { Returns.volatility(returns: [0.1, 0.2], sample: :yes) }
        .to(raise_error(ArgumentError, /sample must be true or false/))
    end
  end

  describe('downside_deviation') do
    it('uses all observations in the downside-risk denominator') do
      result = Returns.downside_deviation(returns: [-0.1, 0.05, -0.05])

      expect(result).to(be_within(D('1e-14')).of(D('0.06454972243679028')))
    end

    it('measures shortfalls relative to a target') do
      result = Returns.downside_deviation(returns: [0.01, 0.03], target: 0.02)

      expect(result).to(be_within(D('1e-14')).of(D('0.007071067811865476')))
    end
  end

  describe('sortino_ratio') do
    let(:returns) { [-0.1, 0.05, -0.05] }

    it('divides arithmetic excess return by downside deviation') do
      expect(Returns.sortino_ratio(returns:)).to(be_within(D('1e-14')).of(D('-0.5163977794943222')))
    end

    it('annualizes the periodic ratio by the square-root of time') do
      result = Returns.sortino_ratio(returns:, periods_per_year: 12)

      expect(result).to(be_within(D('1e-14')).of(D('-1.7888543819998317')))
    end

    it('rejects a sequence with no downside risk') do
      expect { Returns.sortino_ratio(returns: [0.01, 0.02]) }
        .to(raise_error(ArgumentError, /downside deviation/))
    end
  end

  describe('max_drawdown') do
    it('returns the largest peak-to-trough loss as a positive fraction') do
      expect(Returns.max_drawdown(values: [100, 120, 90, 150, 105, 140])).to(eq(D('0.3')))
    end

    it('returns zero for a monotonically increasing series') do
      expect(Returns.max_drawdown(values: [100, 110, 120])).to(eq(D('0')))
    end

    it('requires positive portfolio values') do
      expect { Returns.max_drawdown(values: [100, 0]) }
        .to(raise_error(ArgumentError, /greater than zero/))
    end
  end

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

    it('rejects mismatched valuation periods') do
      expect { Returns.twrr(ev: [120], bv: [100, 110], cfr: [2]) }
        .to(raise_error(Finrb::Error, /Different number/))
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
