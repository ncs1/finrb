# frozen_string_literal: true

SPEC =
  Gem::Specification.new do |s|
    s.name = "finrb"
    s.version = "0.0.1"
    s.authors = ["Nadir Cohen", "Martin Bjeldbak Madsen", "Bill Kranec"]
    s.license = "LGPL-3.0"
    s.email = ["nadircs11@gmail.com", "me@martinbjeldbak.com", "wkranec@gmail.com"]
    s.platform = Gem::Platform::RUBY
    s.summary = "Ruby gem for financial calculations/modeling"

    s.description = <<~EOF
      The finrb library (forked from the finance gem) provides a Ruby interface for financial calculations/modeling. Working with interest rates, Mortgage amortization, Cashflows (NPV, IRR, etc.) and other basic utilities.

    EOF

    s.homepage = "https://rubygems.org/gems/finrb"

    s.required_ruby_version = ">= 3.0"

    s.add_dependency("activesupport")
    s.add_dependency("business_time")
    s.add_dependency("flt")

    s.add_development_dependency("minitest")
    s.add_development_dependency("pry")
    s.add_development_dependency("rake")
    s.add_development_dependency("rubocop")
    s.add_development_dependency("rubocop-minitest")
    s.add_development_dependency("rubocop-performance")
    s.add_development_dependency("rubocop-packaging")
    s.add_development_dependency("rubocop-rake")
    s.add_development_dependency("semver")
    s.add_development_dependency("solargraph")

    s.files = `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }

    s.extra_rdoc_files = ["README.md", "COPYING", "COPYING.LESSER", "CHANGELOG.md"]

    s.metadata["rubygems_mfa_required"] = "true"
  end
