# frozen_string_literal: true

require 'finrb'

describe(Finrb::FixedRateBond) do
  let(:bond) do
    described_class.new(face_value: 1000, coupon_rate: 0.06, issue_date: Date.new(2024, 1, 31), maturity_date: Date.new(2026, 1, 31), frequency: :semiannual)
  end

  it('creates regular fixed coupons and repays face value with the final coupon') do
    cashflows = bond.cashflows(settlement_date: Date.new(2024, 1, 31))

    expect(bond.day_count).to(eq(:actual_actual_icma))
    expect(cashflows.map(&:date)).to(eq([Date.new(2024, 7, 31), Date.new(2025, 1, 31), Date.new(2025, 7, 31), Date.new(2026, 1, 31)]))
    expect(cashflows.map(&:amount)).to(eq([D(30), D(30), D(30), D(1030)]))
  end

  it('calculates accrued interest against the current unadjusted reference period') do
    settlement_date = Date.new(2024, 4, 30)
    expected = D(1000) * D('0.06') * D(90) / (D(182) * 2)

    expect(bond.accrued_interest(settlement_date:)).to(eq(expected))
  end

  it('prices at par on an accrual boundary when coupon rate equals yield') do
    settlement_date = bond.issue_date

    expect(bond.accrued_interest(settlement_date:)).to(be_zero)
    expect(bond.dirty_price(settlement_date:, yield_rate: 0.06)).to(eq(D(1000)))
    expect(bond.clean_price(settlement_date:, yield_rate: 0.06)).to(eq(D(1000)))
  end

  it('recovers yield from both clean and dirty prices') do
    settlement_date = Date.new(2024, 5, 15)
    clean_price = bond.clean_price(settlement_date:, yield_rate: 0.075)
    dirty_price = bond.dirty_price(settlement_date:, yield_rate: 0.075)

    expect(bond.yield_to_maturity(settlement_date:, price: clean_price, price_type: :clean).round(10)).to(eq(D('0.075').round(10)))
    expect(bond.yield_to_maturity(settlement_date:, price: dirty_price, price_type: :dirty).round(10)).to(eq(D('0.075').round(10)))
  end

  it('excludes a coupon paid on settlement and resets accrued interest') do
    settlement_date = Date.new(2024, 7, 31)

    expect(bond.cashflows(settlement_date:).map(&:date)).to(eq([Date.new(2025, 1, 31), Date.new(2025, 7, 31), Date.new(2026, 1, 31)]))
    expect(bond.accrued_interest(settlement_date:)).to(be_zero)
  end

  it('accrues on unadjusted coupon boundaries when payment dates are adjusted') do
    calendar = Finrb::Calendars::USFederalReserve.new
    adjusted_bond = described_class.new(face_value: 1000, coupon_rate: 0.10, issue_date: Date.new(2025, 8, 31), maturity_date: Date.new(2026, 8, 31), frequency: :semiannual, calendar:, business_day_convention: :modified_following)
    settlement_date = Date.new(2026, 2, 26)

    expect(adjusted_bond.schedule.unadjusted_payment_dates).to(eq([Date.new(2026, 2, 28), Date.new(2026, 8, 31)]))
    expect(adjusted_bond.schedule.payment_dates).to(eq([Date.new(2026, 2, 27), Date.new(2026, 8, 31)]))
    expect(adjusted_bond.cashflows(settlement_date:).first.date).to(eq(Date.new(2026, 2, 27)))
    expect(adjusted_bond.accrued_interest(settlement_date:)).to(eq(D(1000) * D('0.10') * D(179) / (D(181) * 2)))
  end

  it('rejects maturity dates that do not align with coupon frequency') do
    expect { irregular_bond }
      .to(raise_error(ArgumentError, /does not align with frequency/))
  end

  it('rejects invalid amounts and settlement dates') do
    expect { zero_face_bond }
      .to(raise_error(ArgumentError, /face value must be greater than zero/))
    expect { bond.accrued_interest(settlement_date: Date.new(2024, 1, 30)) }
      .to(raise_error(ArgumentError, /on or after issue_date/))
    expect { bond.cashflows(settlement_date: Date.new(2026, 1, 31)) }
      .to(raise_error(ArgumentError, /before maturity_date/))
  end

  it('validates yield, price, and price type domains') do
    settlement_date = Date.new(2024, 2, 15)

    expect { bond.dirty_price(settlement_date:, yield_rate: -1) }
      .to(raise_error(Finrb::DomainError, /bond yield must be greater than -1/))
    expect { bond.yield_to_maturity(settlement_date:, price: 0) }
      .to(raise_error(Finrb::DomainError, /clean bond price must be greater than zero/))
    expect { bond.yield_to_maturity(settlement_date:, price: 1000, price_type: :mid) }
      .to(raise_error(ArgumentError, /price_type must be one of/))
  end

  def irregular_bond
    described_class.new(face_value: 1000, coupon_rate: 0.05, issue_date: Date.new(2025, 1, 1), maturity_date: Date.new(2026, 8, 1), frequency: :semiannual)
  end

  def zero_face_bond
    described_class.new(face_value: 0, coupon_rate: 0.05, issue_date: Date.new(2025, 1, 1), maturity_date: Date.new(2026, 1, 1))
  end
end
