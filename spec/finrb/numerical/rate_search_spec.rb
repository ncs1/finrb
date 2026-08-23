# frozen_string_literal: true

describe(Finrb::Numerical::RateSearch) do
  subject(:search) { described_class.new }

  it('finds positive rates') do
    bounds = search.bracket(->(rate) { rate - D('0.123456') }, guess: 0)

    expect(bounds.first).to(be < D('0.123456'))
    expect(bounds.last).to(be > D('0.123456'))
  end

  it('finds rates close to the -100% domain boundary') do
    bounds = search.bracket(->(rate) { rate + D('0.9999') }, guess: -0.9)

    expect(bounds.first).to(be < D('-0.9999'))
    expect(bounds.last).to(be > D('-0.9999'))
  end

  it('returns an exact root at the guess') do
    expect(search.bracket(->(rate) { rate - D('0.25') }, guess: 0.25)).to(eq([D('0.25'), D('0.25')]))
  end

  it('chooses the first sign change reached from the guess') do
    function = ->(rate) { (rate - D('0.1')) * (rate - D('0.2')) }

    low_bounds = search.bracket(function, guess: 0.05)
    high_bounds = search.bracket(function, guess: 0.25)

    expect(low_bounds).to(satisfy { |lower, upper| lower < D('0.1') && upper > D('0.1') })
    expect(high_bounds).to(satisfy { |lower, upper| lower < D('0.2') && upper > D('0.2') })
  end

  it('rejects guesses outside the financial rate domain') do
    expect { search.bracket(->(rate) { rate }, guess: -1) }
      .to(raise_error(Finrb::DomainError, /greater than -1/))
  end

  it('reports when its bounded search cannot find a sign change') do
    bounded_search = described_class.new(max_steps: 3)

    expect { bounded_search.bracket(->(rate) { (rate * rate) + 1 }, guess: 0) }
      .to(raise_error(Finrb::ConvergenceError, /Could not bracket/))
  end

  it('reports domain failures raised while evaluating the rate function') do
    expect { search.bracket(->(_rate) { raise(ZeroDivisionError, 'undefined') }, guess: 0) }
      .to(raise_error(Finrb::DomainError, /undefined at 0/))
  end
end
