# frozen_string_literal: true

require 'date'
require 'json'
require_relative '../../../lib/finrb'

# Evaluates bounded batches for external solver reference verification.
module FinrbReferenceAdapter
  module_function

  def solve(test_case)
    case test_case.fetch('kind')
    when 'irr'
      Finrb::Cashflow.irr(test_case.fetch('amounts'), test_case.fetch('guess'))
    when 'xirr'
      transactions =
        test_case.fetch('transactions').map do |transaction|
          Finrb::Transaction.new(transaction.fetch('amount'), date: Date.iso8601(transaction.fetch('date')))
        end
      Finrb::Cashflow.xirr(transactions, test_case.fetch('guess')).effective
    when 'fixed_rate_bond'
      verify_bond(test_case)
    else
      raise(ArgumentError, "Unknown case kind: #{test_case.fetch('kind')}")
    end
  end

  def verify_bond(test_case)
    calendar = Finrb::Calendars::USFederalReserve.new if test_case.fetch('calendar') == 'us_federal_reserve'
    options = { face_value: Flt::DecNum(test_case.fetch('face_value')), coupon_rate: Flt::DecNum(test_case.fetch('coupon_rate')), issue_date: Date.iso8601(test_case.fetch('issue_date')), maturity_date: Date.iso8601(test_case.fetch('maturity_date')), frequency: test_case.fetch('frequency').to_sym, calendar: }
    convention = test_case.fetch('payment_convention')
    options[:business_day_convention] = convention.to_sym unless calendar.nil?
    bond = Finrb::FixedRateBond.new(**options)
    settlement_date = Date.iso8601(test_case.fetch('settlement_date'))
    yield_rate = Flt::DecNum(test_case.fetch('yield_rate'))
    dirty_price = bond.dirty_price(settlement_date:, yield_rate:)
    clean_price = bond.clean_price(settlement_date:, yield_rate:)

    {
      payment_dates: bond.schedule.unadjusted_payment_dates.map(&:iso8601),
      adjusted_payment_dates: bond.schedule.payment_dates.map(&:iso8601),
      cashflows: bond.cashflows(settlement_date:).map { |cashflow| { date: cashflow.date.iso8601, amount: cashflow.amount.to_s } },
      accrued_interest: bond.accrued_interest(settlement_date:).to_s,
      dirty_price: dirty_price.to_s,
      clean_price: clean_price.to_s,
      yield_from_clean_price: bond.yield_to_maturity(settlement_date:, price: clean_price).to_s,
      yield_from_dirty_price: bond.yield_to_maturity(settlement_date:, price: dirty_price, price_type: :dirty).to_s
    }
  end
end

$stdin.each_line do |line|
  payload = JSON.parse(line)
  results =
    payload.fetch('cases').map do |test_case|
      result = FinrbReferenceAdapter.solve(test_case)
      result.is_a?(Hash) ? result : { value: result.to_s }
    rescue Finrb::Error, ArgumentError => e
      { error: e.class.name, message: e.message }
    end

  puts(JSON.generate(results:))
  $stdout.flush
end
