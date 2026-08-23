# frozen_string_literal: true

# Legacy numeric convenience API, loaded only through +finrb/core_ext+.
class Numeric
  def to_dec
    instance_of?(Flt::DecNum) ? self : Flt::DecNum(to_s)
  end

  def amortize(...)
    Finrb::Amortization.new(self, ...)
  end
end
