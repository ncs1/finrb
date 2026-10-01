# frozen_string_literal: true

require 'date'
require 'finrb/core_ext'
require 'json'

# These values are generated independently with QuantLib's Actual/365 Fixed
# day counter and simple-compounding interval factors.
describe(Finrb::Amortization) do
  context('when validating dated reference fixtures') do
    it('agrees with QuantLib-derived schedules under matching conventions') do
      fixture.fetch('cases').each { |test_case| expect_case_to_match_fixture(test_case) }
    end

    def fixture
      JSON.parse(File.read(File.expand_path('../fixtures/quantlib_dated_amortization.json', __dir__)))
    end

    def expect_case_to_match_fixture(test_case)
      rate = Finrb::Rate.new(D(test_case.fetch('apr')), :apr, duration: test_case.fetch('periods').length)
      loan = Finrb::Amortization.new(D(test_case.fetch('principal')), rate, start_date: Date.iso8601(test_case.fetch('start_date')), balloon: D(test_case.fetch('balloon')))
      entries = loan.schedule
      periods = test_case.fetch('periods')
      interval_starts = [loan.start_date, *entries.map(&:date).take(entries.length - 1)]
      actual_days = entries.each_with_index.map { |entry, index| (entry.date - interval_starts.fetch(index)).to_i }

      expect(entries.map { |entry| entry.date.iso8601 }).to(eq(periods.map { |period| period.fetch('date') }))
      expect(actual_days).to(eq(periods.map { |period| period.fetch('actual_days') }))
      expect(entries.map(&:opening_balance)).to(eq(periods.map { |period| D(period.fetch('opening_balance')) }))
      expect(entries.map(&:interest)).to(eq(periods.map { |period| D(period.fetch('interest')) }))
      expect(entries.map(&:payment)).to(eq(periods.map { |period| D(period.fetch('payment')) }))
      expect(entries.map(&:balloon_payment)).to(eq(periods.map { |period| D(period.fetch('balloon_payment')) }))
      expect(entries.map(&:closing_balance)).to(eq(periods.map { |period| D(period.fetch('closing_balance')) }))
      expect(loan.payment).to(eq(D(test_case.fetch('scheduled_payment'))))
      expect(loan.balance).to(be_zero)
    end
  end
end
