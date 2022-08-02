# frozen_string_literal: true

SPEC = Gem::Specification.new do |s|
  s.name = 'finrb'
  s.version = '0.0.1'
  s.author = ['Martin Bjeldbak Madsen', 'Bill Kranec']
  s.license = 'LGPL-3.0'
  s.email = ['me@martinbjeldbak.com', 'wkranec@gmail.com']
  s.platform = Gem::Platform::RUBY
  s.summary = 'Ruby gem for simple financial calculations'
  s.description = 'The finrb library (based on finance gem) provides a Ruby interface for working with interest rates, mortgage amortization, and cashflows (NPV, IRR, etc.)'
  s.homepage = 'https://rubygems.org/gems/finrb'

  s.required_ruby_version = '>=3.0'
  s.add_dependency 'flt', '>=1.3.0'
  s.add_dependency 'activesupport', '>= 3.2.0'
  s.add_dependency 'rake'
  s.add_dependency 'business_time', '>= 0.9.2'
  s.add_development_dependency 'minitest', '>= 4.7.5'
  s.add_development_dependency 'pry'
  s.files = `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }

  s.extra_rdoc_files = ['README.md', 'COPYING', 'COPYING.LESSER', 'HISTORY']
end
