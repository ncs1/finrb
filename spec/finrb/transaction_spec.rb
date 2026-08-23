# frozen_string_literal: true

describe(Finrb::Transaction) do
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
  end

  it('rejects invalid dates, periods, and unknown options') do
    expect { described_class.new(500, date: '2026-08-23') }
      .to(raise_error(ArgumentError, /date must respond/))
    expect { described_class.new(500, period: -1) }
      .to(raise_error(ArgumentError, /non-negative integer/))
    expect { described_class.new(500, currency: :usd) }
      .to(raise_error(ArgumentError, /options may only/))
  end
end
