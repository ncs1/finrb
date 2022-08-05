# frozen_string_literal: true

require "active_support/configurable"
require "finrb/config"
require "finrb/decimal"
require "finrb/cashflows"
require "finrb/utils"

class FinrbError < StandardError; end

# The *Finrb* module adheres to the following conventions for
# financial calculations:
#
#  * Positive values represent cash inflows (money received); negative
#    values represent cash outflows (payments).
#  * *principal* represents the outstanding balance of a loan or annuity.
#  * *rate* represents the interest rate _per period_.
module Finrb
  autoload :Amortization, "finrb/amortization"
  autoload :Rate,         "finrb/rates"
  autoload :Transaction,  "finrb/transaction"
  autoload :Utils,        "finrb/utils"
end
