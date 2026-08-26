# frozen_string_literal: true

require_relative 'decimal'
require_relative 'errors'
require_relative 'validation'

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
      d = Validation.decimal(d, name: 'dollar discount')
      f = Validation.positive_decimal(f, name: 'face value', error: DomainError)
      t = Validation.positive_decimal(t, name: 'time to maturity', error: DomainError)

      (d * 360 / f / t)
    end

    # Computing money market yield (MMY) for a T-bill
    #
    # @param bdy bank discount yield
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Yields.bdy2mmy(bdy=0.045,t=120)
    def self.bdy2mmy(bdy:, t:)
      bdy = Validation.decimal(bdy, name: 'bank discount yield')
      t = Validation.positive_decimal(t, name: 'time to maturity', error: DomainError)
      denominator = 360 - (t * bdy)
      raise(DomainError, 'Bank discount yield and time to maturity must imply a positive purchase price.') unless denominator.positive?

      (bdy * 360 / denominator)
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
      r = Validation.decimal(r, name: 'stated annual rate')
      m = Validation.positive_decimal(m, name: 'compounding periods', error: DomainError)

      ((compounding_base(r, m)**m) - 1)
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
      r = Validation.decimal(r, name: 'stated annual rate')

      (r.exp - 1)
    end

    # bond-equivalent yield (BEY), 2 x the semiannual discount rate
    #
    # @param ear effective annual rate
    # @example
    #   Finrb::Yields.ear2bey(ear=0.08)
    def self.ear2bey(ear:)
      ear = Validation.decimal_at_least(ear, minimum: -1, name: 'effective annual rate', error: DomainError)

      (((ear + 1).sqrt - 1) * 2)
    end

    # Computing HPR, the holding period return
    #
    # @param ear effective annual rate
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Yields.ear2hpr(ear=0.05039,t=150)
    def self.ear2hpr(ear:, t:)
      ear = Validation.decimal_at_least(ear, minimum: -1, name: 'effective annual rate', error: DomainError)
      t = Validation.positive_decimal(t, name: 'time to maturity', error: DomainError)

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
      r = Validation.decimal(r, name: 'annual rate')
      n = Validation.positive_decimal(n, name: 'source compounding periods', error: DomainError)
      p = Validation.positive_decimal(p, name: 'target compounding periods', error: DomainError)
      type = type.to_s

      case type
      when 'e'
        eir = (compounding_base(r, n)**(n / p)) - 1
      when 'p'
        eir = r / p
      else
        raise(ArgumentError, "conversion type must be 'e' (equivalent) or 'p' (proportional)")
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
      hpr = Validation.decimal_at_least(hpr, minimum: -1, name: 'holding period return', error: DomainError)
      t = Validation.positive_decimal(t, name: 'time to maturity', error: DomainError)

      ((((hpr + 1)**(6 / t)) - 1) * 2)
    end

    # Convert holding period return to the effective annual rate
    #
    # @param hpr holding period return
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Yields.hpr2ear(hpr=0.015228,t=120)
    def self.hpr2ear(hpr:, t:)
      hpr = Validation.decimal_at_least(hpr, minimum: -1, name: 'holding period return', error: DomainError)
      t = Validation.positive_decimal(t, name: 'time to maturity', error: DomainError)

      (((hpr + 1)**(365 / t)) - 1)
    end

    # Computing money market yield (MMY) for a T-bill
    #
    # @param hpr holding period return
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Yields.hpr2mmy(hpr=0.01523,t=120)
    def self.hpr2mmy(hpr:, t:)
      hpr = Validation.decimal(hpr, name: 'holding period return')
      t = Validation.positive_decimal(t, name: 'time to maturity', error: DomainError)

      (hpr * 360 / t)
    end

    # Computing HPR, the holding period return
    #
    # @param mmy money market yield
    # @param t number of days remaining until maturity
    # @example
    #   Finrb::Yields.mmy2hpr(mmy=0.04898,t=150)
    def self.mmy2hpr(mmy:, t:)
      mmy = Validation.decimal(mmy, name: 'money market yield')
      t = Validation.positive_decimal(t, name: 'time to maturity', error: DomainError)

      (mmy * t / 360)
    end

    # Convert a given norminal rate to a continuous compounded rate
    #
    # @param r norminal rate
    # @param m number of times compounded each year
    # @example
    #   Finrb::Yields.r_continuous(r=0.03,m=4)
    def self.r_continuous(r:, m:)
      r = Validation.decimal(r, name: 'nominal rate')
      m = Validation.positive_decimal(m, name: 'compounding periods', error: DomainError)

      (m * compounding_base(r, m).log)
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
      rc = Validation.decimal(rc, name: 'continuously compounded rate')
      m = Validation.positive_decimal(m, name: 'compounding periods', error: DomainError)

      (m * ((rc / m).exp - 1))
    end

    def self.compounding_base(rate, periods)
      base = (rate / periods) + 1
      raise(DomainError, 'The rate per compounding period must be greater than -1.') unless base.positive?

      base
    end
    private_class_method :compounding_base
  end
end
