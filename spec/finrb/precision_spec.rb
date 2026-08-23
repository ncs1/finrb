# frozen_string_literal: true

describe(Finrb::Precision) do
  it('rounds monetary values to cents using half-up ties') do
    expect(described_class.money(D('1.005'))).to(eq(D('1.01')))
    expect(described_class.money(D('-1.005'))).to(eq(D('-1.01')))
  end

  it('rounds internal periodic rates to fifteen decimal places') do
    expect(described_class.rate(D('0.1234567890123456'))).to(eq(D('0.123456789012346')))
  end

  it('rejects invalid values instead of producing NaN') do
    expect { described_class.money('1.00') }
      .to(raise_error(ArgumentError, /must be numeric/))
  end
end
