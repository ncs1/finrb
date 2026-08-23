# frozen_string_literal: true

describe(Finrb::Transaction) do
  let(:invalid_numeric) do
    invalid_class = Class.new(Numeric) { define_method(:to_s) { raise(FloatDomainError, 'cannot represent value') } }
    invalid_class.new
  end

  it('stores initial amounts as decimals') do
    transaction = described_class.new(500)

    expect(transaction.amount).to(be_an_instance_of(Flt::DecNum))
  end

  it('stores assigned amounts as decimals') do
    transaction = described_class.new(500)
    transaction.amount = 750.25

    expect(transaction.amount).to(eq(D('750.25')))
  end

  it('stores block-modified amounts as decimals') do
    transaction = described_class.new(500)
    transaction.modify { 650.25 }

    expect(transaction.amount).to(eq(D('650.25')))
    expect(transaction.difference).to(eq(D('150.25')))
  end

  it('rejects non-numeric or non-finite amounts') do
    expect { described_class.new('500') }
      .to(raise_error(ArgumentError, /amount must be numeric/))
    expect { described_class.new(Float::INFINITY) }
      .to(raise_error(ArgumentError, /amount must be finite/))
    expect { described_class.new(invalid_numeric) }
      .to(raise_error(ArgumentError, /finite numeric value/))
  end

  it('rejects invalid dates, periods, and unknown options') do
    expect { described_class.new(500, date: '2026-08-23') }
      .to(raise_error(ArgumentError, /date must respond/))
    expect { described_class.new(500, period: -1) }
      .to(raise_error(ArgumentError, /non-negative integer/))
    expect { described_class.new(500, currency: :usd) }
      .to(raise_error(ArgumentError, /options may only/))
  end

  it('provides inspection strings for each transaction type') do
    date = Date.new(2026, 8, 23)

    expect(described_class.new(10.125, date:).inspect).to(eq('Transaction(10.13, date: 2026-08-23)'))
    expect(Finrb::Interest.new(2.5).inspect).to(eq('Interest(2.5)'))
    expect(Finrb::Payment.new(-12.5).inspect).to(eq('Payment(-12.5)'))
  end
end
