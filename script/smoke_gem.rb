# frozen_string_literal: true

require 'finrb'

loaded_version = Gem.loaded_specs.fetch('finrb').version.to_s
abort("Expected finrb #{loaded_version}, loaded #{Finrb::VERSION}") unless loaded_version == Finrb::VERSION
abort('Core extensions loaded by default') if [].respond_to?(:irr)

cashflows = [-4000, 1200, 1410, 1875, 1050]
npv = Finrb::Cashflow.npv(cashflows, 0.10).round(2)
irr = Finrb::Cashflow.irr(cashflows).round(6)
abort("Unexpected NPV: #{npv}") unless npv == Flt::DecNum('382.08')
abort("Unexpected IRR: #{irr}") unless irr == Flt::DecNum('0.142993')

require 'finrb/core_ext'

abort('Core extensions unavailable after explicit require') unless [].respond_to?(:irr)
