# frozen_string_literal: true

require 'flt'
require 'rubygems'

Flt::DecNum.context.define_conversion_from(BigDecimal) do |x, _context|
  Flt::DecNum(x.to_s)
end

Flt::DecNum.context.define_conversion_to(BigDecimal) do |x|
  BigDecimal(x.to_s)
end

class Numeric
  def to_dec
    if instance_of?(Flt::DecNum)
      self
    else
      Flt::DecNum(to_s)
    end
  end
end
