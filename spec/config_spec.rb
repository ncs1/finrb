# frozen_string_literal: true

describe('Cashflows') do
  describe('default values') do
    it('has default values') do
      expect(D(Finrb.config.guess.to_s)).to(eq(D('1.0')))
      expect(D(Finrb.config.eps)).to(eq(D('1.0e-16')))
      expect(Finrb.config.business_days).to(be_falsey)
      expect(Finrb.config.periodic_compound).to(be_falsey)
    end
  end

  describe('overriding defaults') do
    before do
      Finrb.config.guess = 0.25
      Finrb.config.eps = '1.0e-9'
      Finrb.config.business_days = true
      Finrb.config.periodic_compound = true
    end

    after do
      Finrb.config.guess = 1.0
      Finrb.config.eps = '1.0e-16'
      Finrb.config.business_days = false
      Finrb.config.periodic_compound = false
    end

    it('is permanent') do
      expect(0.25).to(be_within(0.001).of(Finrb.config.guess))
      expect(Finrb.config.eps).to(eq('1.0e-9'))
      expect(Finrb.config.business_days).to(be_truthy)
      expect(Finrb.config.periodic_compound).to(be_truthy)
    end
  end
end
