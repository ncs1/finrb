# frozen_string_literal: true

require 'minitest/autorun'
require 'minitest/spec'

require 'active_support/all'

require 'pry'

require 'flt'
require 'flt/d'

require_relative '../lib/finrb/config.rb'
require_relative '../lib/finrb/amortization.rb'
require_relative '../lib/finrb/cashflows.rb'
require_relative '../lib/finrb/rates.rb'
require_relative '../lib/finrb/transaction.rb'
include Finrb
