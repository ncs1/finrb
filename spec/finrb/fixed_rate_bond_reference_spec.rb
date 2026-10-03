# frozen_string_literal: true

require 'finrb'
require 'json'

describe(Finrb::FixedRateBond) do
  def bond_from_reference(case_data)
    calendar = Finrb::Calendars::USFederalReserve.new if case_data.fetch('calendar') == 'us_federal_reserve'
    options = { face_value: D(case_data.fetch('face_value')), coupon_rate: D(case_data.fetch('coupon_rate')), issue_date: Date.iso8601(case_data.fetch('issue_date')), maturity_date: Date.iso8601(case_data.fetch('maturity_date')), frequency: case_data.fetch('frequency').to_sym, calendar: }
    payment_convention = case_data.fetch('payment_convention')
    options[:business_day_convention] = payment_convention.to_sym unless payment_convention == 'unadjusted'

    described_class.new(**options)
  end

  def expect_reference_decimal(actual, expected)
    expect((actual - D(expected)).abs).to(be < D('1e-10'))
  end

  it('matches QuantLib 1.43 coupons, accrued interest, prices, and yields') do
    fixture = JSON.parse(File.read(File.expand_path('../fixtures/quantlib_fixed_rate_bonds.json', __dir__)))

    expect(fixture.fetch('quantlib_version')).to(eq('1.43'))
    fixture.fetch('cases').each { |case_data| expect_case_matches_reference(case_data) }
  end

  def expect_case_matches_reference(case_data)
    bond = bond_from_reference(case_data)
    settlement_date = Date.iso8601(case_data.fetch('settlement_date'))
    cashflows = bond.cashflows(settlement_date:)

    expect_reference_schedule(bond, case_data)
    expect_reference_cashflows(cashflows, case_data.fetch('future_cashflows'))
    expect_reference_valuation(bond, settlement_date, case_data)
  end

  def expect_reference_schedule(bond, case_data)
    expect(bond.schedule.unadjusted_payment_dates.map(&:iso8601)).to(eq(case_data.fetch('payment_dates')), case_data.fetch('name'))
    expect(bond.schedule.payment_dates.map(&:iso8601)).to(eq(case_data.fetch('adjusted_payment_dates')), case_data.fetch('name'))
  end

  def expect_reference_cashflows(cashflows, expected_cashflows)
    expect(cashflows.map { |cashflow| cashflow.date.iso8601 }).to(eq(expected_cashflows.map { |cashflow| cashflow.fetch('date') }))
    cashflows.zip(expected_cashflows).each { |cashflow, expected| expect_reference_decimal(cashflow.amount, expected.fetch('amount')) }
  end

  def expect_reference_valuation(bond, settlement_date, case_data)
    expect_reference_decimal(bond.accrued_interest(settlement_date:), case_data.fetch('accrued_interest'))
    expect_reference_decimal(bond.dirty_price(settlement_date:, yield_rate: D(case_data.fetch('yield_rate'))), case_data.fetch('dirty_price'))
    expect_reference_decimal(bond.clean_price(settlement_date:, yield_rate: D(case_data.fetch('yield_rate'))), case_data.fetch('clean_price'))
    expect_reference_yield(bond, settlement_date, case_data)
  end

  def expect_reference_yield(bond, settlement_date, case_data)
    expect_reference_decimal(bond.yield_to_maturity(settlement_date:, price: D(case_data.fetch('clean_price'))), case_data.fetch('yield_from_clean_price'))
    expect_reference_decimal(bond.yield_to_maturity(settlement_date:, price: D(case_data.fetch('dirty_price')), price_type: :dirty), case_data.fetch('yield_from_dirty_price'))
  end
end
