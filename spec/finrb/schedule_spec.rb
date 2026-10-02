# frozen_string_literal: true

require 'finrb'

describe(Finrb::Schedule) do
  describe('.new') do
    it('generates regular dates from the original end-of-month anchor') do
      schedule = described_class.new(start_date: Date.new(2025, 1, 31), maturity_date: Date.new(2026, 1, 31), frequency: :quarterly)

      expect(schedule.payment_dates).to(eq([Date.new(2025, 4, 30), Date.new(2025, 7, 31), Date.new(2025, 10, 31), Date.new(2026, 1, 31)]))
      expect(schedule.periods.map(&:accrual_start_date)).to(eq([Date.new(2025, 1, 31), Date.new(2025, 4, 30), Date.new(2025, 7, 31), Date.new(2025, 10, 31)]))
      expect(schedule.periods.map(&:index)).to(eq([0, 1, 2, 3]))
      expect(schedule.periods.last.short_final_stub?).to(be(false))
    end

    it('requires explicit short-final convention for a partial final period') do
      arguments = { start_date: Date.new(2025, 1, 31), maturity_date: Date.new(2026, 3, 31), frequency: :quarterly }

      expect { described_class.new(**arguments) }
        .to(raise_error(ArgumentError, /pass stub: :short_final/))

      schedule = described_class.new(**arguments, stub: :short_final)
      expect(schedule.payment_dates.last).to(eq(Date.new(2026, 3, 31)))
      expect(schedule.periods.last.accrual_start_date).to(eq(Date.new(2026, 1, 31)))
      expect(schedule.periods.last.short_final_stub?).to(be(true))
    end

    it('clamps non-month-end anchors without accumulating date drift') do
      schedule = described_class.new(start_date: Date.new(2025, 1, 30), maturity_date: Date.new(2025, 4, 30), frequency: :monthly)

      expect(schedule.payment_dates).to(eq([Date.new(2025, 2, 28), Date.new(2025, 3, 30), Date.new(2025, 4, 30)]))
    end

    it('retains both unadjusted and calendar-adjusted dates') do
      schedule = described_class.new(start_date: Date.new(2026, 1, 31), maturity_date: Date.new(2026, 3, 31), frequency: :monthly, calendar: Finrb::Calendars::USFederalReserve.new, business_day_convention: :modified_following)

      expect(schedule.unadjusted_payment_dates).to(eq([Date.new(2026, 2, 28), Date.new(2026, 3, 31)]))
      expect(schedule.payment_dates).to(eq([Date.new(2026, 2, 27), Date.new(2026, 3, 31)]))
      expect(schedule.periods.last.accrual_start_date).to(eq(Date.new(2026, 2, 27)))
      expect(schedule.periods.last.unadjusted_payment_date).to(eq(Date.new(2026, 3, 31)))
    end

    it('rejects unsupported frequency, stub, and calendar option combinations') do
      valid_dates = { start_date: Date.new(2025, 1, 1), maturity_date: Date.new(2025, 2, 1) }

      expect { described_class.new(**valid_dates, frequency: :weekly) }
        .to(raise_error(ArgumentError, /frequency must be one of/))
      expect { described_class.new(**valid_dates, stub: :long_first) }
        .to(raise_error(ArgumentError, /stub must be one of/))
      expect { described_class.new(**valid_dates, business_day_convention: :following) }
        .to(raise_error(ArgumentError, /requires a calendar/))
    end

    it('requires date-only inputs and maturity after the start date') do
      expect { described_class.new(start_date: Date.today, maturity_date: Date.today) }
        .to(raise_error(ArgumentError, /maturity_date must be after start_date/))
      expect { described_class.new(start_date: '2025-01-01', maturity_date: Date.new(2025, 2, 1)) }
        .to(raise_error(ArgumentError, /start_date must be a Date/))
    end

    it('validates the calendar type and business-day convention') do
      dates = { start_date: Date.new(2025, 1, 1), maturity_date: Date.new(2025, 2, 1) }

      expect { described_class.new(**dates, calendar: Object.new, business_day_convention: :following) }
        .to(raise_error(ArgumentError, /calendar must be a Finrb::Calendars::Base instance/))
      expect { described_class.new(**dates, calendar: Finrb::Calendars::USFederalReserve.new, business_day_convention: :weekly) }
        .to(raise_error(ArgumentError, /business_day_convention must be one of/))
    end

    it('freezes the schedule and its periods') do
      schedule = described_class.from_months(start_date: Date.new(2025, 1, 1), term_months: 3)

      expect(schedule).to(be_frozen)
      expect(schedule.periods).to(be_frozen)
      expect(schedule.payment_dates).to(be_frozen)
      expect(schedule.periods.first).to(be_frozen)
    end
  end

  describe('.from_months') do
    it('derives maturity from the start-date anchor and validates the term') do
      schedule = described_class.from_months(start_date: Date.new(2024, 1, 31), term_months: 14, frequency: :quarterly, stub: :short_final)

      expect(schedule.maturity_date).to(eq(Date.new(2025, 3, 31)))
      expect(schedule.payment_dates).to(eq([Date.new(2024, 4, 30), Date.new(2024, 7, 31), Date.new(2024, 10, 31), Date.new(2025, 1, 31), Date.new(2025, 3, 31)]))
      expect { described_class.from_months(start_date: Date.new(2025, 1, 1), term_months: 0) }
        .to(raise_error(ArgumentError, /term_months must be a positive integer/))
    end
  end
end
