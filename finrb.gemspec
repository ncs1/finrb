# frozen_string_literal: true

SPEC =
  Gem::Specification.new do |s|
    s.name = 'finrb'
    s.version = '0.0.1'
    s.authors = ['Nadir Cohen', 'Martin Bjeldbak Madsen', 'Bill Kranec']
    s.license = 'LGPL-3.0'
    s.email = ['nadircs11@gmail.com', 'me@martinbjeldbak.com', 'wkranec@gmail.com']
    s.platform = Gem::Platform::RUBY
    s.summary = 'Ruby gem for financial calculations/modeling'

    s.description = <<~EOF
      The finrb library (forked from the finance gem) provides a Ruby interface for financial calculations/modeling.

      - Working with interest rates
      - Mortgage amortization
      - Cashflows (NPV, IRR, etc.)
      - Computing bank discount yield (BDY) for a T-bill
      - Computing money market yield (MMY) for a T-bill
      - Cash ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
      - Computing Coefficient of variation
      - Cost of goods sold and ending inventory under three methods (FIFO,LIFO,Weighted average)
      - Current ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
      - Depreciation Expense Recognition - double-declining balance (DDB), the most common declining balance method, which applies two times the straight-line rate to the declining balance.
      - Debt ratio - Solvency ratios measure the firm's ability to satisfy its long-term obligations.
      - Diluted Earnings Per Share
      - Computing the rate of return for each period
      - Convert stated annual rate to the effective annual rate
      - Convert stated annual rate to the effective annual rate with continuous compounding
      - Bond-equivalent yield (BEY), 2 x the semiannual discount rate
      - Computing HPR, the holding period return
      - Equivalent/proportional Interest Rates
      - Basic Earnings Per Share
      - Financial leverage - Solvency ratios measure the firm's ability to satisfy its long-term obligations.
      - Estimate future value (fv)
      - Estimate future value of an annuity
      - Estimate future value (fv) of a single sum
      - Computing the future value of an uneven cash flow series
      - Geometric mean return
      - Gross profit margin - Evaluate a company's financial performance
      - Harmonic mean, average price
      - Computing HPR, the holding period return
      - Bond-equivalent yield (BEY), 2 x the semiannual discount rate
      - Convert holding period return to the effective annual rate
      - Computing money market yield (MMY) for a T-bill
      - Computing IRR, the internal rate of return
      - Calculate the net increase in common shares from the potential exercise of stock options or warrants
      - Long-term debt-to-equity - Solvency ratios measure the firm's ability to satisfy its long-term obligations.
      - Computing HPR, the holding period return
      - Estimate the number of periods
      - Net profit margin - Evaluate a company's financial performance
      - Computing NPV, the PV of the cash flows less the initial (time = 0) outlay
      - Estimate period payment
      - Estimate present value (pv)
      - Estimate present value (pv) of an annuity
      - Estimate present value of a perpetuity
      - Estimate present value (pv) of a single sum
      - Computing the present value of an uneven cash flow series
      - Quick ratio - Liquidity ratios measure the firm's ability to satisfy its short-term obligations as they come due.
      - Convert a given norminal rate to a continuous compounded rate
      - Convert a given continuous compounded rate to a norminal rate
      - Rate of return for a perpetuity
      - Computing Sampling error
      - Computing Roy's safety-first ratio
      - Computing Sharpe Ratio
      - Depreciation Expense Recognition - Straight-line depreciation (SL) allocates an equal amount of depreciation each year over the asset's useful life
      - Total debt-to-equity - Solvency ratios measure the firm's ability to satisfy its long-term obligations.
      - Computing TWRR, the time-weighted rate of return
      - Calculate weighted average shares - weighted average number of common shares
      - Weighted mean as a portfolio return

    EOF

    s.homepage = 'https://rubygems.org/gems/finrb'

    s.required_ruby_version = '>= 3.0'

    s.add_dependency('activesupport')
    s.add_dependency('business_time')
    s.add_dependency('flt')

    s.add_development_dependency('minitest')
    s.add_development_dependency('pry')
    s.add_development_dependency('rake')
    s.add_development_dependency('rubocop')
    s.add_development_dependency('rubocop-minitest')
    s.add_development_dependency('rubocop-performance')
    s.add_development_dependency('rubocop-rake')
    s.add_development_dependency('solargraph')

    s.files = `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }

    s.extra_rdoc_files = ['README.md', 'COPYING', 'COPYING.LESSER', 'CHANGELOG.md']

    s.metadata['rubygems_mfa_required'] = 'true'
  end
