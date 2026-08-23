# frozen_string_literal: true

describe(Finrb) do
  describe('default values') do
    it('has default values') do
      expect(D(described_class.config.guess.to_s)).to(eq(D('1.0')))
      expect(D(described_class.config.eps)).to(eq(D('1.0e-16')))
      expect(described_class.config.business_days).to(be_falsey)
      expect(described_class.config.periodic_compound).to(be_falsey)
    end
  end

  describe('overriding defaults') do
    before do
      described_class.config.guess = 0.25
      described_class.config.eps = '1.0e-9'
      described_class.config.business_days = true
      described_class.config.periodic_compound = true
    end

    after do
      described_class.config.guess = 1.0
      described_class.config.eps = '1.0e-16'
      described_class.config.business_days = false
      described_class.config.periodic_compound = false
    end

    it('is permanent') do
      expect(0.25).to(be_within(0.001).of(described_class.config.guess))
      expect(described_class.config.eps).to(eq('1.0e-9'))
      expect(described_class.config.business_days).to(be_truthy)
      expect(described_class.config.periodic_compound).to(be_truthy)
    end
  end
end
