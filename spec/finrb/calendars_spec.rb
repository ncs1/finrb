# frozen_string_literal: true

require 'finrb'
require 'json'

describe(Finrb::Calendars) do
  describe(Finrb::Calendars::USFederalReserve) do
    subject(:calendar) { described_class.new }

    it('follows Federal Reserve bank observance rather than Saturday federal employee observance') do
      expect(calendar.holiday_names(Date.new(2026, 7, 4))).to(include('Independence Day'))
      expect(calendar.business_day?(Date.new(2026, 7, 3))).to(be(true))
      expect(calendar.business_day?(Date.new(2026, 7, 6))).to(be(true))
      expect(calendar.business_day?(Date.new(2023, 1, 2))).to(be(false))
      expect(calendar.holiday_names(Date.new(2023, 1, 2))).to(include("New Year's Day"))
    end

    it('uses effective dates for Juneteenth and the current federal Monday holidays') do
      expect(calendar.business_day?(Date.new(2020, 6, 19))).to(be(true))
      expect(calendar.business_day?(Date.new(2021, 6, 18))).to(be(true))
      expect(calendar.holiday_names(Date.new(2022, 6, 20))).to(include('Juneteenth National Independence Day'))
      expect(calendar.holiday_names(Date.new(1986, 1, 20))).to(include('Birthday of Martin Luther King, Jr.'))
      expect(calendar.business_day?(Date.new(1982, 1, 18))).to(be(true))
    end

    it('matches the Federal Reserve legacy weekday rules and holiday effective years') do
      expect(calendar.holiday_names(Date.new(1953, 2, 23))).to(include("Washington's Birthday"))
      expect(calendar.holiday_names(Date.new(1958, 2, 21))).to(include("Washington's Birthday"))
      expect(calendar.holiday_names(Date.new(1953, 5, 29))).to(include('Memorial Day'))
      expect(calendar.holiday_names(Date.new(1954, 5, 31))).to(include('Memorial Day'))
      expect(calendar.holiday_names(Date.new(1983, 1, 17))).to(include('Birthday of Martin Luther King, Jr.'))
      expect(calendar.business_day?(Date.new(1982, 1, 18))).to(be(true))
      expect(calendar.holiday_names(Date.new(1972, 10, 23))).to(include('Veterans Day'))
      expect(calendar.business_day?(Date.new(1972, 10, 30))).to(be(true))
    end

    it('enforces its documented 1950 through 2065 date range') do
      expect(calendar.business_day?(described_class::SUPPORTED_START_DATE)).to(be(false))
      expect(calendar.business_day?(described_class::SUPPORTED_END_DATE)).to(be(true))
      expect { calendar.business_day?(described_class::SUPPORTED_START_DATE - 1) }
        .to(raise_error(RangeError, /supports dates from 1950-01-01 through 2065-12-31/))
      expect { described_class.new(additional_holidays: [described_class::SUPPORTED_END_DATE + 1]) }
        .to(raise_error(RangeError, /supports dates from 1950-01-01 through 2065-12-31/))
    end

    it('reports names for observed holidays and recognizes ordinary weekends') do
      expect(calendar.holiday_names(Date.new(2026, 11, 26))).to(include('Thanksgiving Day'))
      expect(calendar.business_day?(Date.new(2026, 11, 27))).to(be(true))
      expect(calendar.business_day?(Date.new(2026, 11, 28))).to(be(false))
      expect(calendar.holiday?(Date.new(2026, 11, 28))).to(be(true))
      expect(calendar.holiday_names(Date.new(2026, 11, 28))).to(be_empty)
    end

    it('lists named holidays within an inclusive date range') do
      from = Date.new(2026, 11, 25)
      through = Date.new(2026, 11, 28)
      thanksgiving = Date.new(2026, 11, 26)
      named_holiday = { thanksgiving => ['Thanksgiving Day'] }

      expect(calendar.holidays_between(from, through)).to(eq(named_holiday))
      expect { calendar.holidays_between(through, from) }
        .to(raise_error(ArgumentError, /must not be after/))
    end

    it('can include weekly weekends in holiday lists') do
      from = Date.new(2026, 11, 25)
      through = Date.new(2026, 11, 28)
      closures = { Date.new(2026, 11, 26) => ['Thanksgiving Day'], Date.new(2026, 11, 28) => ['Weekend'] }

      expect(calendar.holidays_between(from, through, include_weekends: true)).to(eq(closures))
    end
  end

  describe(Finrb::Calendars::IsraelTase) do
    subject(:calendar) { described_class.new }

    let(:festival_eve_fixture) do
      fixture_path = File.expand_path('../fixtures/tase_verified_festival_eves.json', __dir__)
      JSON.parse(File.read(fixture_path))
    end

    let(:historical_reference_fixture) do
      fixture_path = File.expand_path('../fixtures/israel_2001_calendar_reference.json', __dir__)
      JSON.parse(File.read(fixture_path))
    end

    it('distinguishes Purim from Shushan Purim') do
      expect(calendar.holiday_names(Date.new(2024, 3, 24))).to(include('Purim'))
      expect(calendar.business_day?(Date.new(2024, 3, 25))).to(be(true))
    end

    it('matches Passover full-day closures while keeping intermediate days open') do
      expect(calendar.holiday_names(Date.new(2024, 4, 22))).to(include('Passover Eve I'))
      expect(calendar.holiday_names(Date.new(2024, 4, 23))).to(include('Passover I'))
      expect(calendar.business_day?(Date.new(2024, 4, 24))).to(be(true))
      expect(calendar.holiday_names(Date.new(2024, 4, 28))).to(include('Passover Eve VII'))
      expect(calendar.holiday_names(Date.new(2024, 4, 29))).to(include('Passover VII'))
    end

    it('matches Independence Day and Shavuot closures') do
      expect(calendar.holiday_names(Date.new(2024, 5, 13))).to(include('Memorial Day'))
      expect(calendar.holiday_names(Date.new(2024, 5, 14))).to(include('Independence Day'))
      expect(calendar.holiday_names(Date.new(2024, 6, 11))).to(include('Shavuot Eve'))
      expect(calendar.holiday_names(Date.new(2024, 6, 12))).to(include('Shavuot'))
      expect(calendar.holiday_names(Date.new(2024, 8, 13))).to(include('Tisha B’Av'))
    end

    it('matches the fall full-day closures in the published TASE schedule') do
      expect(calendar.holiday_names(Date.new(2024, 10, 2))).to(include('Rosh Hashanah Eve'))
      expect(calendar.holiday_names(Date.new(2024, 10, 3))).to(include('Rosh Hashanah I'))
      expect(calendar.holiday_names(Date.new(2024, 10, 4))).to(include('Rosh Hashanah II'))
      expect(calendar.holiday_names(Date.new(2024, 10, 11))).to(include('Yom Kippur Eve'))
      expect(calendar.holiday_names(Date.new(2024, 10, 12))).to(include('Yom Kippur'))
    end

    it('matches festival-eve closures in published TASE annual schedules') do
      expect(festival_eve_fixture.fetch('source_schedules').keys).to(eq(%w[2015 2019 2021 2022 2023 2024]))
      festival_eve_fixture.fetch('closures').each do |closure|
        date = Date.iso8601(closure.fetch('date'))

        expect(calendar.holiday_names(date)).to(include(closure.fetch('holiday')), closure.fetch('date'))
        expect(calendar.business_day?(date)).to(be(false), closure.fetch('date'))
      end
    end

    it('does not adopt the incorrect Shavuot Eve date in TASE’s 2022 schedule') do
      source_error = festival_eve_fixture.fetch('known_source_errors').fetch(0)
      listed_date = Date.iso8601(source_error.fetch('date'))
      actual_holiday_date = Date.iso8601(source_error.fetch('actual_holiday_date'))

      expect(calendar.business_day?(listed_date)).to(be(true), source_error.fetch('date'))
      expect(calendar.holiday_names(listed_date)).not_to(include(source_error.fetch('incorrect_label')))
      expect(calendar.holiday_names(actual_holiday_date)).to(include(source_error.fetch('actual_holiday')))
    end

    it('includes Sukkot and Shemini Atzeret closures') do
      expect(calendar.holiday_names(Date.new(2024, 10, 16))).to(include('Sukkot Eve'))
      expect(calendar.holiday_names(Date.new(2024, 10, 17))).to(include('Sukkot'))
      expect(calendar.holiday_names(Date.new(2024, 10, 23))).to(include('Shemini Atzeret Eve'))
      expect(calendar.holiday_names(Date.new(2024, 10, 24))).to(include('Shemini Atzeret / Simchat Torah'))
    end

    it('uses the historic Sunday-through-Thursday week up to the 2026 transition') do
      expect(calendar.business_day?(Date.new(2025, 12, 28))).to(be(true))
      expect(calendar.business_day?(Date.new(2025, 12, 26))).to(be(false))
      expect(calendar.business_day?(Date.new(2026, 1, 4))).to(be(false))
      expect(calendar.holiday_names(Date.new(2026, 1, 4))).to(include('TASE trading-week transition'))
      expect(calendar.business_day?(Date.new(2026, 1, 5))).to(be(true))
      expect(calendar.business_day?(Date.new(2026, 1, 9))).to(be(true))
      expect(calendar.business_day?(Date.new(2026, 1, 10))).to(be(false))
    end

    it('enforces its documented 2000 through 2050 date range') do
      expect(calendar.business_day?(described_class::SUPPORTED_START_DATE)).to(be(false))
      expect(calendar.holiday_names(described_class::SUPPORTED_END_DATE)).to(be_a(Array))
      expect { calendar.business_day?(described_class::SUPPORTED_START_DATE - 1) }
        .to(raise_error(RangeError, /supports dates from 2000-01-01 through 2050-12-31/))
      expect { calendar.business_day?(described_class::SUPPORTED_END_DATE + 1) }
        .to(raise_error(RangeError, /supports dates from 2000-01-01 through 2050-12-31/))
    end

    it('applies statutory Independence Day weekday adjustments') do
      expect(calendar.holiday_names(Date.new(2025, 4, 30))).to(include('Memorial Day'))
      expect(calendar.holiday_names(Date.new(2025, 5, 1))).to(include('Independence Day'))
      expect(calendar.holiday_names(Date.new(2038, 5, 10))).to(include('Memorial Day'))
      expect(calendar.holiday_names(Date.new(2038, 5, 11))).to(include('Independence Day'))
      expect(calendar.business_day?(Date.new(2038, 5, 10))).to(be(false))
      expect(calendar.business_day?(Date.new(2038, 5, 11))).to(be(false))
    end

    it('uses independently verified 2001 Hebrew dates where QuantLib is one day early') do
      historical_reference_fixture.fetch('closures').each do |closure|
        date = Date.iso8601(closure.fetch('date'))

        expect(calendar.holiday_names(date)).to(include(closure.fetch('holiday')), date.to_s)
        expect(calendar.business_day?(date)).to(be(false), date.to_s)
      end
    end

    it('remains open on the four dates QuantLib closes early in its 2001 table') do
      historical_reference_fixture.fetch('quantlib_only_closures').each do |iso_date|
        date = Date.iso8601(iso_date)

        expect(calendar.business_day?(date)).to(be(true), iso_date)
        expect(calendar.holiday_names(date)).to(be_empty, iso_date)
      end
    end

    it('postpones Tisha B’Av when its Hebrew date falls on Saturday') do
      expect(calendar.business_day?(Date.new(2022, 8, 6))).to(be(false))
      expect(calendar.holiday_names(Date.new(2022, 8, 7))).to(include('Tisha B’Av'))
    end
  end

  describe(Finrb::Calendars::Base) do
    subject(:calendar) { Finrb::Calendars::USFederalReserve.new }

    it('supports following and modified-following conventions') do
      date = Date.new(2026, 1, 31)

      expect(calendar.adjust(date, convention: :following)).to(eq(Date.new(2026, 2, 2)))
      expect(calendar.adjust(date, convention: :modified_following)).to(eq(Date.new(2026, 1, 30)))
    end

    it('supports preceding conventions and half-month modified following') do
      date = Date.new(2026, 1, 31)

      expect(calendar.adjust(date, convention: :preceding)).to(eq(Date.new(2026, 1, 30)))
      expect(calendar.adjust(Date.new(2026, 2, 1), convention: :modified_preceding)).to(eq(Date.new(2026, 2, 2)))
      expect(calendar.adjust(Date.new(2026, 8, 15), convention: :half_month_modified_following)).to(eq(Date.new(2026, 8, 14)))
    end

    it('supports nearest and unadjusted conventions') do
      date = Date.new(2026, 1, 31)

      expect(calendar.adjust(Date.new(2026, 7, 4), convention: :nearest)).to(eq(Date.new(2026, 7, 3)))
      expect(calendar.adjust(date, convention: :unadjusted)).to(eq(date))
    end

    it('advances by business days and adjusts zero-day advances') do
      expect(calendar.advance(Date.new(2026, 10, 9), business_days: 1)).to(eq(Date.new(2026, 10, 13)))
      expect(calendar.advance(Date.new(2026, 10, 13), business_days: -1)).to(eq(Date.new(2026, 10, 9)))
      expect(calendar.advance(Date.new(2026, 7, 4), business_days: 0)).to(eq(Date.new(2026, 7, 6)))
    end

    it('supports immutable per-instance closures and reopenings') do
      closure = Date.new(2026, 7, 3)
      reopen = Date.new(2026, 7, 6)
      customized = Finrb::Calendars::USFederalReserve.new(additional_holidays: [closure], removed_holidays: [reopen])

      expect(customized.business_day?(closure)).to(be(false))
      expect(customized.holiday_names(closure)).to(include('Additional market holiday'))
      expect(customized.business_day?(reopen)).to(be(true))
      expect { customized.additional_holidays << Date.new(2026, 7, 7) }
        .to(raise_error(FrozenError))
      expect(Finrb::Calendars::USFederalReserve.new(removed_holidays: [Date.new(2026, 7, 4)]).business_day?(Date.new(2026, 7, 4))).to(be(false))
    end

    it('validates dates, convention names, and advance counts') do
      expect { calendar.business_day?('2026-01-01') }
        .to(raise_error(ArgumentError, /date must be a Date/))
      date = Date.new(2026, 1, 1)
      expect { calendar.adjust(date, convention: :bad_convention) }
        .to(raise_error(ArgumentError, /business-day convention/))
      expect { calendar.advance(date, business_days: 1.5) }
        .to(raise_error(ArgumentError, /must be an integer/))
      expect { Finrb::Calendars::USFederalReserve.new(additional_holidays: ['2026-01-01']) }
        .to(raise_error(ArgumentError, /array of Dates/))
    end
  end
end
