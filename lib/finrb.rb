# frozen_string_literal: true

require 'finrb/accounting'
require 'finrb/cashflows'
require 'finrb/config'
require 'finrb/decimal'
require 'finrb/errors'
require 'finrb/precision'
require 'finrb/ratios'
require 'finrb/returns'
require 'finrb/tvm'
require 'finrb/version'
require 'finrb/yields'

FinrbError = Finrb::Error

# The *Finrb* module adheres to the following conventions for
# financial calculations:
#
#  * Positive values represent cash inflows (money received); negative
#    values represent cash outflows (payments).
#  * *principal* represents the outstanding balance of a loan or annuity.
#  * *rate* represents the interest rate _per period_.
module Finrb
  autoload :Amortization, 'finrb/amortization'
  autoload :Rate,         'finrb/rates'
  autoload :Transaction,  'finrb/transaction'
end
