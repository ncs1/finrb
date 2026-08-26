# frozen_string_literal: true

require_relative 'lib/finrb/version'

repository_uri = 'https://github.com/ncs1/finrb'

SPEC =
  Gem::Specification.new do |s|
    s.name = 'finrb'
    s.version = Finrb::VERSION
    s.authors = ['Nadir Cohen']
    s.license = 'LGPL-3.0-or-later'
    s.email = ['nadircs11@gmail.com']
    s.platform = Gem::Platform::RUBY
    s.summary = 'Precision-first financial mathematics for Ruby'

    s.description = <<~EOF
      Decimal financial calculations for cashflows, rates, loan amortization,
      time value of money, investment returns, ratios, and accounting.
    EOF

    s.homepage = repository_uri

    s.required_ruby_version = '>= 3.3'

    s.add_dependency('bigdecimal', '>= 3.1.2')
    s.add_dependency('flt')
    s.add_dependency('ostruct')

    s.add_development_dependency('amazing_print')
    s.add_development_dependency('benchmark-ips')
    s.add_development_dependency('bundler-audit')
    s.add_development_dependency('pry')
    s.add_development_dependency('rake')
    s.add_development_dependency('rbs')
    s.add_development_dependency('rspec')
    s.add_development_dependency('rubocop')
    s.add_development_dependency('rubocop-packaging')
    s.add_development_dependency('rubocop-performance')
    s.add_development_dependency('rubocop-rake')
    s.add_development_dependency('rubocop-rspec')
    s.add_development_dependency('semver')
    s.add_development_dependency('simplecov')

    s.files = Dir['CHANGELOG.md', 'CONTRIBUTING.md', 'COPYING*', 'NOTICE.md', 'README.md', 'SECURITY.md', 'lib/**/*', 'sig/**/*'].sort
    s.require_paths = ['lib']

    s.extra_rdoc_files = ['README.md', 'CHANGELOG.md', 'NOTICE.md', 'COPYING', 'COPYING.LESSER']

    # RubyGems requires metadata keys and values to be strings.
    s.metadata = { bug_tracker_uri: "#{repository_uri}/issues", changelog_uri: "#{repository_uri}/blob/main/CHANGELOG.md", documentation_uri: "#{repository_uri}#readme", rubygems_mfa_required: 'true', source_code_uri: repository_uri }.transform_keys(&:to_s)
  end
