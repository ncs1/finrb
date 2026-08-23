# frozen_string_literal: true

describe(Finrb::Numerical::Brent) do
  subject(:solver) { described_class.new(tolerance: '1e-24') }

  it('solves nonlinear roots while preserving decimal precision') do
    root = solver.solve(->(value) { (value * value) - 2 }, lower: 1, upper: 2)

    expect(root).to(be_an_instance_of(Flt::DecNum))
    expect(root).to(be_within(D('1e-23')).of(D('1.414213562373095048801688724')))
  end

  it('returns an exact endpoint root') do
    function = ->(value) { value - 2 }

    expect(solver.solve(function, lower: 2, upper: 5)).to(eq(D('2')))
    expect(solver.solve(function, lower: -1, upper: 2)).to(eq(D('2')))
  end

  it('supports descending endpoint function values') do
    root = solver.solve(->(value) { 7 - (value * value * value) }, lower: 1, upper: 3)

    expect(root).to(be_within(D('1e-23')).of(D('1.912931182772389101199116839')))
  end

  it('rejects an interval without a sign change') do
    expect { solver.solve(->(value) { (value * value) + 1 }, lower: -1, upper: 1) }
      .to(raise_error(Finrb::ConvergenceError, /not bracketed/))
  end

  it('rejects unordered and zero-width intervals') do
    function = ->(value) { value - 1 }

    expect { solver.solve(function, lower: 2, upper: 1) }
      .to(raise_error(ArgumentError, /Lower bound/))
    expect { solver.solve(function, lower: 1, upper: 1) }
      .to(raise_error(ArgumentError, /Lower bound/))
  end

  it('reports function domain failures') do
    expect { solver.solve(->(value) { 1 / value }, lower: -1, upper: 1) }
      .to(raise_error(Finrb::DomainError, /undefined/))
  end

  it('reports exhaustion of the iteration budget') do
    constrained = described_class.new(tolerance: '1e-40', max_iterations: 1)

    expect { constrained.solve(->(value) { (value * value) - 2 }, lower: 1, upper: 2) }
      .to(raise_error(Finrb::ConvergenceError, /1 iterations/))
  end

  it('converges across deterministic scales and root locations') do
    random = Random.new(20_260_823)

    100.times do
      expected = D(random.rand(-100.0..100.0).to_s)
      width = D(random.rand(0.001..10.0).to_s)
      scale = D((10**random.rand(-8.0..8.0)).to_s)
      function = ->(value) { scale * (value - expected) * (((value - expected)**2) + 1) }
      actual = solver.solve(function, lower: expected - width, upper: expected + width)

      expect(actual).to(be_within(D('1e-20')).of(expected))
    end
  end
end
