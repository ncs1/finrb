# frozen_string_literal: true

require_relative 'decimal'
require_relative 'errors'

module Finrb
  # Investment return and risk-adjusted performance calculations.
  module Returns
    def self.wrap_array(object)
      if object.nil?
        []
      elsif object.respond_to?(:to_ary)
        object.to_ary || [object]
      else
        [object]
      end
    end
    private_class_method :wrap_array

    # Computing Coefficient of variation
    #
    # @param sd standard deviation
    # @param avg average value
    # @example
    #   Finrb::Returns.coefficient_variation(sd=0.15,avg=0.39)
    def self.coefficient_variation(sd:, avg:)
      sd = Flt::DecNum(sd.to_s)
      avg = Flt::DecNum(avg.to_s)

      (sd / avg)
    end

    # Geometric mean return
    #
    # @param r returns over multiple periods
    # @example
    #   Finrb::Returns.geometric_mean(r=[-0.0934, 0.2345, 0.0892])
    def self.geometric_mean(r:)
      r = wrap_array(r).map { |value| Flt::DecNum(value.to_s) }

      rs = r.map { |value| value + 1 }
      ((rs.reduce(:*)**(Flt::DecNum(1) / rs.size)) - 1)
    end

    # harmonic mean, average price
    # @param p price over multiple periods
    # @example
    #   Finrb::Returns.harmonic_mean(p=[8,9,10])
    def self.harmonic_mean(p:)
      p = wrap_array(p).map { |value| Flt::DecNum(value.to_s) }

      (Flt::DecNum(1) / (p.sum { |val| Flt::DecNum(1) / val } / p.size))
    end

    # Computing HPR, the holding period return
    #
    # @param ev ending value
    # @param bv beginning value
    # @param cfr cash flow received
    # @example
    #   Finrb::Returns.hpr(ev=33,bv=30,cfr=0.5)
    def self.hpr(ev:, bv:, cfr: 0)
      ev = Flt::DecNum(ev.to_s)
      bv = Flt::DecNum(bv.to_s)
      cfr = Flt::DecNum(cfr.to_s)

      ((ev - bv + cfr) / bv)
    end

    # Computing Sampling error
    #
    # @param sm sample mean
    # @param mu population mean
    # @example
    #   Finrb::Returns.sampling_error(sm=0.45, mu=0.5)
    def self.sampling_error(sm:, mu:)
      sm = Flt::DecNum(sm.to_s)
      mu = Flt::DecNum(mu.to_s)

      (sm - mu)
    end

    # Computing Roy's safety-first ratio
    #
    # @param rp portfolio return
    # @param rl threshold level return
    # @param sd standard deviation of portfolio retwns
    # @example
    #   Finrb::Returns.sf_ratio(rp=0.09,rl=0.03,sd=0.12)
    def self.sf_ratio(rp:, rl:, sd:)
      rp = Flt::DecNum(rp.to_s)
      rl = Flt::DecNum(rl.to_s)
      sd = Flt::DecNum(sd.to_s)

      ((rp - rl) / sd)
    end

    # Computing Sharpe Ratio
    #
    # @param rp portfolio return
    # @param rf risk-free return
    # @param sd standard deviation of portfolio retwns
    # @example
    #   Finrb::Returns.sharpe_ratio(rp=0.038,rf=0.015,sd=0.07)
    def self.sharpe_ratio(rp:, rf:, sd:)
      rp = Flt::DecNum(rp.to_s)
      rf = Flt::DecNum(rf.to_s)
      sd = Flt::DecNum(sd.to_s)

      ((rp - rf) / sd)
    end

    # Computing TWRR, the time-weighted rate of return
    #
    # @param ev ordered ending value list
    # @param bv ordered beginning value list
    # @param cfr ordered cash flow received list
    # @example
    #   Finrb::Returns.twrr(ev=[120,260],bv=[100,240],cfr=[2,4])
    def self.twrr(ev:, bv:, cfr:)
      ev = wrap_array(ev).map { |value| Flt::DecNum(value.to_s) }
      bv = wrap_array(bv).map { |value| Flt::DecNum(value.to_s) }
      cfr = wrap_array(cfr).map { |value| Flt::DecNum(value.to_s) }

      r = ev.size
      s = bv.size
      t = cfr.size
      wr = Flt::DecNum(1)
      if r != s || r != t || s != t
        raise(Error, 'Different number of values!')
      else
        (0...r).each do |i|
          wr *= (Finrb::Returns.hpr(ev: ev[i], bv: bv[i], cfr: cfr[i]) + 1)
        end
        ((wr**(Flt::DecNum(1) / r)) - 1)
      end
    end

    # Weighted mean as a portfolio return
    #
    # @param r returns of the individual assets in the portfolio
    # @param w corresponding weights associated with each of the individual assets
    # @example
    #   Finrb::Returns.wpr(r=[0.12, 0.07, 0.03],w=[0.5,0.4,0.1])
    def self.wpr(r:, w:)
      r = wrap_array(r).map { |value| Flt::DecNum(value.to_s) }
      w = wrap_array(w).map { |value| Flt::DecNum(value.to_s) }

      # TODO: need to change
      puts('sum of weights is NOT equal to 1!') if w.sum != 1

      r.zip(w).sum { |arr| arr.reduce(:*) }
    end
  end
end
