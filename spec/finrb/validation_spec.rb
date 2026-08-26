# frozen_string_literal: true

describe(Finrb::Validation) do
  describe('decimal bounds') do
    it('returns validated decimals') do
      expect(described_class.positive_decimal(2, name: 'amount')).to(eq(D(2)))
      expect(described_class.non_negative_decimal(0, name: 'amount')).to(eq(D(0)))
      expect(described_class.decimal_greater_than(0, minimum: -1, name: 'rate')).to(eq(D(0)))
      expect(described_class.decimal_at_least(-1, minimum: -1, name: 'rate')).to(eq(D(-1)))
    end

    it('returns decimals validated against zero and ranges') do
      expect(described_class.non_zero_decimal(-1, name: 'balance')).to(eq(D(-1)))
      expect(described_class.decimal_between(1, minimum: 0, maximum: 1, name: 'fraction')).to(eq(D(1)))
    end

    it('rejects values outside each bound') do
      expect { described_class.positive_decimal(0, name: 'amount') }
        .to(raise_error(ArgumentError, /amount must be greater than zero/))
      expect { described_class.non_negative_decimal(-1, name: 'amount') }
        .to(raise_error(ArgumentError, /amount must be greater than or equal to zero/))
      expect { described_class.decimal_greater_than(-1, minimum: -1, name: 'rate') }
        .to(raise_error(ArgumentError, /rate must be greater than -1/))
      expect { described_class.decimal_at_least(-2, minimum: -1, name: 'rate') }
        .to(raise_error(ArgumentError, /rate must be greater than or equal to -1/))
    end

    it('rejects zero and out-of-range decimals') do
      expect { described_class.non_zero_decimal(0, name: 'balance') }
        .to(raise_error(ArgumentError, /balance must be non-zero/))
      expect { described_class.decimal_between(2, minimum: 0, maximum: 1, name: 'fraction') }
        .to(raise_error(ArgumentError, /fraction must be between 0 and 1/))
    end

    it('allows callers to classify financial-domain errors') do
      expect { described_class.positive_decimal(0, name: 'term', error: Finrb::DomainError) }
        .to(raise_error(Finrb::DomainError, /term must be greater than zero/))
    end
  end
end
