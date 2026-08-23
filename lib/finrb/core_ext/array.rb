# frozen_string_literal: true

# Legacy fluent cashflow API, loaded only through +finrb/core_ext+.
class Array
  include Finrb::Cashflow
end
