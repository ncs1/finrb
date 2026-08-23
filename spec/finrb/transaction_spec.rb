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
end
