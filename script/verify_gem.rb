# frozen_string_literal: true

require 'rubygems/package'
require_relative '../lib/finrb/version'

package_path = ARGV.fetch(0)
package = Gem::Package.new(package_path)
specification = package.spec

required_files = %w[CHANGELOG.md COPYING COPYING.LESSER NOTICE.md README.md lib/finrb.rb lib/finrb/version.rb sig/finrb.rbs]
missing_files = required_files - specification.files
raise(Gem::InvalidSpecificationException, "Gem is missing packaged files: #{missing_files.join(', ')}") unless missing_files.empty?

required_metadata = %w[bug_tracker_uri changelog_uri documentation_uri rubygems_mfa_required source_code_uri]
missing_metadata =
  required_metadata.reject do |key|
    value = specification.metadata.fetch(key, '')
    key == 'rubygems_mfa_required' ? value == 'true' : value.start_with?('https://')
  end
raise(Gem::InvalidSpecificationException, "Gem has missing or invalid metadata: #{missing_metadata.join(', ')}") unless missing_metadata.empty?

runtime_dependencies = specification.runtime_dependencies.map(&:name)
missing_dependencies = %w[bigdecimal flt ostruct] - runtime_dependencies
raise(Gem::InvalidSpecificationException, "Gem is missing runtime dependencies: #{missing_dependencies.join(', ')}") unless missing_dependencies.empty?

raise(Gem::InvalidSpecificationException, "Gem version #{specification.version} does not match #{Finrb::VERSION}") unless specification.version.to_s == Finrb::VERSION
