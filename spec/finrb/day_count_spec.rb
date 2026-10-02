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
