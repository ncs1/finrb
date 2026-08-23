# frozen_string_literal: true

describe(Finrb::Error) do
  it('is the base class for finrb calculation errors') do
    expect(Finrb::ConvergenceError).to(be < described_class)
    expect(Finrb::DomainError).to(be < described_class)
    expect(Finrb::InvalidCashflowError).to(be < described_class)
  end

  it('preserves the legacy error constant') do
    expect(FinrbError).to(equal(described_class))
  end
end
