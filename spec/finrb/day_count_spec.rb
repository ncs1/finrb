# frozen_string_literal: true

describe(Finrb::DayCount) do
  describe('.year_fraction') do
    it('uses actual elapsed days over a fixed 365-day denominator by default') do
      fraction = described_class.year_fraction(Date.new(2024, 2, 28), Date.new(2024, 3, 1))

      expect(fraction).to(eq(D(2) / D(365)))
    end

    it('uses actual elapsed days over a fixed 360-day denominator when selected') do
      fraction = described_class.year_fraction(Date.new(2025, 1, 15), Date.new(2025, 2, 15), convention: :actual_360)

      expect(fraction).to(eq(D(31) / D(360)))
    end

    it('uses the supplied regular coupon reference period for Actual/Actual ICMA') do
      reference_start = Date.new(2023, 8, 31)
      reference_end = Date.new(2024, 2, 29)
      period_start = Date.new(2023, 11, 30)
      period_end = Date.new(2024, 2, 29)

      fraction = described_class.year_fraction(period_start, period_end, convention: :actual_actual_icma, reference_period_start: reference_start, reference_period_end: reference_end, frequency: 2)

      expect(fraction).to(eq(D(91) / (D(182) * 2)))
      expect(described_class.year_fraction(reference_start, reference_end, convention: :actual_actual_icma, reference_period_start: reference_start, reference_period_end: reference_end, frequency: 2)).to(eq(D('0.5')))
    end

    it('requires the reference-period context for Actual/Actual ICMA') do
      start_date = Date.new(2024, 1, 1)
      end_date = Date.new(2024, 2, 1)

      expect { described_class.year_fraction(start_date, end_date, convention: :actual_actual_icma) }
        .to(raise_error(ArgumentError, /requires reference_period_start/))
    end

    it('validates ICMA reference dates and frequency') do
      start_date = Date.new(2024, 1, 1)
      end_date = Date.new(2024, 2, 1)
      invalid_reference = { convention: :actual_actual_icma, reference_period_start: start_date, reference_period_end: start_date, frequency: 2 }

      expect { described_class.year_fraction(start_date, end_date, **invalid_reference) }
        .to(raise_error(ArgumentError, /reference_period_end must be after/))
    end

    it('requires a positive ICMA coupon frequency and rejects unrelated reference arguments') do
      start_date = Date.new(2024, 1, 1)
      end_date = Date.new(2024, 2, 1)
      invalid_frequency = { convention: :actual_actual_icma, reference_period_start: start_date, reference_period_end: end_date, frequency: 0 }

      expect { described_class.year_fraction(start_date, end_date, **invalid_frequency) }
        .to(raise_error(ArgumentError, /frequency must be a positive integer/))
      expect { described_class.year_fraction(start_date, end_date, convention: :actual_365_fixed, frequency: 2) }
        .to(raise_error(ArgumentError, %r{only valid for Actual/Actual ICMA}))
    end

    it('returns zero for matching dates and a negative fraction for reversed dates') do
      date = Date.new(2025, 1, 1)

      expect(described_class.year_fraction(date, date)).to(be_zero)
      expect(described_class.year_fraction(date + 1, date)).to(eq(-D(1) / D(365)))
    end

    it('requires date-only Date values and a supported convention') do
      expect { described_class.year_fraction('2025-01-01', Date.new(2025, 1, 2)) }
        .to(raise_error(ArgumentError, /start_date must be a Date/))
      date_subclass = Class.new(Date).new(2025, 1, 1)
      expect { described_class.year_fraction(date_subclass, Date.new(2025, 1, 2)) }
        .to(raise_error(ArgumentError, /start_date must be a Date/))
      expect { described_class.year_fraction(Date.new(2025, 1, 1), Date.new(2025, 1, 2), convention: :thirty_360) }
        .to(raise_error(ArgumentError, /day-count convention must be one of/))
    end
  end
end
