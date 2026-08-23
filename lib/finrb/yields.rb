# frozen_string_literal: true

require_relative 'decimal'
require_relative 'errors'

module Finrb
  # Money-market yield and interest-rate conversion calculations.
  module Yields
    # Computing bank discount yield (BDY) for a T-bill
    #
    # @param d the dollar discount, which is equal to the difference between the face value of the bill and the purchase price
    # @param f the face value (par value) of the bill
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Yields.bdy(d=1500,f=100000,t=120)
    def self.bdy(d:, f:, t:)
      d = Flt::DecNum(d.to_s)
      f = Flt::DecNum(f.to_s)
      t = Flt::DecNum(t.to_s)

      (d * 360 / f / t)
    end

    # Computing money market yield (MMY) for a T-bill
    #
    # @param bdy bank discount yield
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Yields.bdy2mmy(bdy=0.045,t=120)
    def self.bdy2mmy(bdy:, t:)
      bdy = Flt::DecNum(bdy.to_s)
      t = Flt::DecNum(t.to_s)

      (bdy * 360 / (360 - (t * bdy)))
    end

    # Convert stated annual rate to the effective annual rate
    #
    # @param r stated annual rate
    # @param m number of compounding periods per year
    # @example
    #   Finrb::Yields.ear(r=0.12,m=12)
    #
    # @example
    #   Finrb::Yields.ear(0.04,365)
    def self.ear(r:, m:)
      r = Flt::DecNum(r.to_s)
      m = Flt::DecNum(m.to_s)

      ((((r / m) + 1)**m) - 1)
    end

    # Convert stated annual rate to the effective annual rate with continuous compounding
    #
    # @param r stated annual rate
    # @example
    #   Finrb::Yields.ear_continuous(r=0.1)
    #
    # @example
    #   Finrb::Yields.ear_continuous(0.03)
    def self.ear_continuous(r:)
      r = Flt::DecNum(r.to_s)

      (r.to_dec.exp - 1)
    end

    # bond-equivalent yield (BEY), 2 x the semiannual discount rate
    #
    # @param ear effective annual rate
    # @example
    #   Finrb::Yields.ear2bey(ear=0.08)
    def self.ear2bey(ear:)
      ear = Flt::DecNum(ear.to_s)

      (((ear + 1).sqrt - 1) * 2)
    end

    # Computing HPR, the holding period return
    #
    # @param ear effective annual rate
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Yields.ear2hpr(ear=0.05039,t=150)
    def self.ear2hpr(ear:, t:)
      ear = Flt::DecNum(ear.to_s)
      t = Flt::DecNum(t.to_s)

      (((ear + 1)**(t / 365)) - 1)
    end

    # Equivalent/proportional Interest Rates
    # @note An interest rate to be applied n times p.a. can be converted to an equivalent rate to be applied p times p.a.
    # @param r interest rate to be applied n times per year (r is annual rate!)
    # @param n times that the interest rate r were compounded per year
    # @param p times that the equivalent rate were compounded per year
    # @param type equivalent interest rates ('e',default) or proportional interest rates ('p')
    # @example
    #   # monthly interest rat equivalent to 5% compounded per year
    #   Finrb::Yields.eir(r=0.05,n=1,p=12)
    #
    # @example
    #   # monthly interest rat equivalent to 5% compounded per half year
    #   Finrb::Yields.eir(r=0.05,n=2,p=12)
    #
    # @example
    #   # monthly interest rat equivalent to 5% compounded per quarter
    #   Finrb::Yields.eir(r=0.05,n=4,p=12)
    #
    # @example
    #   # annual interest rate equivalent to 5% compounded per month
    #   Finrb::Yields.eir(r=0.05,n=12,p=1)
    #   # this is equivalent to
    #   Finrb::Yields.ear(r=0.05,m=12)
    #
    # @example
    #   # quarter interest rate equivalent to 5% compounded per year
    #   Finrb::Yields.eir(r=0.05,n=1,p=4)
    #
    # @example
    #   # quarter interest rate equivalent to 5% compounded per month
    #   Finrb::Yields.eir(r=0.05,n=12,p=4)
    #
    # @example
    #   # monthly proportional interest rate which is equivalent to a simple annual interest
    #   Finrb::Yields.eir(r=0.05,p=12,type='p')
    def self.eir(r:, n: 1, p: 12, type: 'e')
      r = Flt::DecNum(r.to_s)
      n = Flt::DecNum(n.to_s)
      p = Flt::DecNum(p.to_s)
      type = type.to_s

      case type
      when 'e'
        eir = (((r / n) + 1)**(n / p)) - 1
      when 'p'
        eir = r / p
      else
        raise(Error, "type must be 'e' or 'p'")
      end
      eir
    end

    # bond-equivalent yield (BEY), 2 x the semiannual discount rate
    #
    # @param hpr holding period return
    # @param t number of month remaining until maturity
    # @example
    #   Finrb::Yields.hpr2bey(hpr=0.02,t=3)
    def self.hpr2bey(hpr:, t:)
      hpr = Flt::DecNum(hpr.to_s)
      t = Flt::DecNum(t.to_s)

      ((((hpr + 1)**(6 / t)) - 1) * 2)
    end

    # Convert holding period return to the effective annual rate
    #
    # @param hpr holding period return
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Yields.hpr2ear(hpr=0.015228,t=120)
    def self.hpr2ear(hpr:, t:)
      hpr = Flt::DecNum(hpr.to_s)
      t = Flt::DecNum(t.to_s)

      (((hpr + 1)**(365 / t)) - 1)
    end

    # Computing money market yield (MMY) for a T-bill
    #
    # @param hpr holding period return
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Yields.hpr2mmy(hpr=0.01523,t=120)
    def self.hpr2mmy(hpr:, t:)
      hpr = Flt::DecNum(hpr.to_s)
      t = Flt::DecNum(t.to_s)

      (hpr * 360 / t)
    end

    # Computing HPR, the holding period return
    #
    # @param mmy money market yield
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Yields.mmy2hpr(mmy=0.04898,t=150)
    def self.mmy2hpr(mmy:, t:)
      mmy = Flt::DecNum(mmy.to_s)
      t = Flt::DecNum(t.to_s)

      (mmy * t / 360)
    end

    # Convert a given norminal rate to a continuous compounded rate
    #
    # @param r norminal rate
    # @param m number of times compounded each year
    # @example
    #   Finrb::Yields.r_continuous(r=0.03,m=4)
    def self.r_continuous(r:, m:)
      r = Flt::DecNum(r.to_s)
      m = Flt::DecNum(m.to_s)

      (m * ((r / m) + 1).to_dec.log)
    end

    # Convert a given continuous compounded rate to a norminal rate
    #
    # @param rc continuous compounded rate
    # @param m number of desired times compounded each year
    # @example
    #   Finrb::Yields.r_norminal(0.03,1)
    #
    # @example
    #   Finrb::Yields.r_norminal(rc=0.03,m=4)
    def self.r_norminal(rc:, m:)
      rc = Flt::DecNum(rc.to_s)
      m = Flt::DecNum(m.to_s)

      (m * ((rc / m).to_dec.exp - 1))
    end
  end
end
