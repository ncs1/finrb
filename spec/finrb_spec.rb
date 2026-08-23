# frozen_string_literal: true

describe(Finrb) do
  describe('decimal interoperability') do
    it('converts between BigDecimal and Flt::DecNum') do
      decimal = Flt::DecNum(BigDecimal('1.25'))

      expect(decimal).to(eq(D('1.25')))
      expect(decimal.convert_to(BigDecimal)).to(eq(BigDecimal('1.25')))
    end
  end

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
      described_class.configure do |config|
        config.guess = 0.25
        config.eps = 1.0e-9
        config.business_days = true
        config.periodic_compound = true
      end
    end

    after do
      described_class.configure do |config|
        config.guess = 1.0
        config.eps = 1.0e-16
        config.business_days = false
        config.periodic_compound = false
      end
    end

    it('is permanent') do
      expect(0.25).to(be_within(0.001).of(described_class.config.guess))
      expect(described_class.config.eps).to(eq(D('1.0e-9')))
      expect(described_class.config.business_days).to(be_truthy)
      expect(described_class.config.periodic_compound).to(be_truthy)
    end

    it('publishes an immutable configuration') do
      expect(described_class.config).to(be_frozen)
      expect { described_class.config.guess = 0.5 }
        .to(raise_error(NoMethodError))
    end

    it('rejects an invalid replacement without changing the current configuration') do
      current = described_class.config

      expect { described_class.configure { |config| config.eps = 0 } }
        .to(raise_error(ArgumentError, /eps must be positive/))
      expect(described_class.config).to(equal(current))
    end

    it('scopes overrides to the current thread and restores them') do
      configured_guess = described_class.config.guess

      described_class.with_config(guess: 0.75) do
        expect(described_class.config.guess).to(eq(D('0.75')))
        thread = Thread.new { described_class.config.guess }
        other_thread_guess = thread.value
        expect(other_thread_guess).to(eq(configured_guess))
      end

      expect(described_class.config.guess).to(eq(configured_guess))
    end
  end
end
