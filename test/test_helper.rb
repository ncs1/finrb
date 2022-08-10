# frozen_string_literal: true

require 'minitest/autorun'
require 'minitest/spec'

require 'active_support/all'

require 'pry'

require 'flt'
require 'flt/d'

require_relative '../lib/finrb/config'
require_relative '../lib/finrb/amortization'
require_relative '../lib/finrb/cashflows'
require_relative '../lib/finrb/rates'
require_relative '../lib/finrb/transaction'
include Finrb
