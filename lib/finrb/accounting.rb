# frozen_string_literal: true

require_relative 'decimal'
require_relative 'errors'

module Finrb
  # Inventory costing and depreciation calculations.
  module Accounting
    def self.wrap_array(object)
      if object.nil?
        []
      elsif object.respond_to?(:to_ary)
        object.to_ary || [object]
      else
        [object]
      end
    end
    private_class_method :wrap_array

    # Cost of goods sold and ending inventory under three methods (FIFO,LIFO,Weighted average)
    #
    # @param uinv units of beginning inventory
    # @param pinv price of beginning inventory
    # @param units nx1 vector of inventory units. inventory purchased ordered by time (from first to last)
    # @param price nx1 vector of inventory price. same order as units
    # @param sinv units of sold inventory
    # @param method inventory methods: FIFO (first in first out, permitted under both US and IFRS), LIFO (late in first out, US only), WAC (weighted average cost,US and IFRS)
    # @example
    #   Finrb::Accounting.cogs(uinv=2,pinv=2,units=[3,5],price=[3,5],sinv=7,method="FIFO")
    #
    # @example
    #   Finrb::Accounting.cogs(uinv=2,pinv=2,units=[3,5],price=[3,5],sinv=7,method="LIFO")
    #
    # @example
    #   Finrb::Accounting.cogs(uinv=2,pinv=2,units=[3,5],price=[3,5],sinv=7,method="WAC")
    def self.cogs(uinv:, pinv:, units:, price:, sinv:, method: 'FIFO')
      uinv = Flt::DecNum(uinv.to_s)
      pinv = Flt::DecNum(pinv.to_s)
      units = wrap_array(units).map { |value| Flt::DecNum(value.to_s) }
      price = wrap_array(price).map { |value| Flt::DecNum(value.to_s) }
      sinv = Flt::DecNum(sinv.to_s)
      method = method.to_s

      n = units.size
      m = price.size
      cost_of_goods = 0
      ending_inventory = 0
      if m == n
        case method
        when 'FIFO'
          if sinv <= uinv
            cost_of_goods = sinv * pinv
            ending_inventory = (uinv - sinv) * pinv
            (0...n).each do |i|
              ending_inventory += (units[i] * price[i])
            end
          else
            cost_of_goods = uinv * pinv
            sinv -= uinv
            (0...n).each do |i|
              if sinv <= units[i]
                cost_of_goods += (sinv * price[i])
                ending_inventory = (units[i] - sinv) * price[i]
                if i < n
                  temp = i + 1
                  (temp...n).each do |j|
                    ending_inventory += (units[j] * price[j])
                  end
                end
                sinv = 0
                next
              else
                cost_of_goods += (units[i] * price[i])
                sinv -= units[i]
              end
            end
            raise(Error, "Inventory is not enough to sell\n") if sinv.positive?
          end
        when 'WAC'
          ending_inventory = uinv * pinv
          tu = uinv
          (0...n).each do |i|
            ending_inventory += (units[i] * price[i])
            tu += units[i]
          end
          if tu >= sinv
            cost_of_goods = ending_inventory / tu * sinv
            ending_inventory = ending_inventory / tu * (tu - sinv)
          else
            raise(Error, "Inventory is not enough to sell\n")
          end

        when 'LIFO'
          (n - 1).downto(0).each do |i|
            if sinv <= units[i]
              cost_of_goods += (sinv * price[i])
              ending_inventory = (units[i] - sinv) * price[i]
              if i > 1
                temp = i - 1
                temp.downto(0).each do |j|
                  ending_inventory += (units[j] * price[j])
                end
              end
              ending_inventory += (uinv * pinv)
              sinv = 0
              next
            else
              cost_of_goods += (units[i] * price[i])
              sinv -= units[i]
            end
          end
          if sinv.positive?
            if sinv <= uinv
              cost_of_goods += (sinv * pinv)
              ending_inventory += ((uinv - sinv) * pinv)
            else
              raise(Error, "Inventory is not enough to sell\n")
            end
          end
        end

      else
        raise(Error, "length of units and price are not the same\n")
      end

      {
        cost_of_goods:,
        ending_inventory:
      }
    end

    # Depreciation Expense Recognition -- double-declining balance (DDB), the most common declining balance method, which applies two times the straight-line rate to the declining balance.
    #
    # @param cost cost of long-lived assets
    # @param rv   residual value of the long-lived assets at the end of its useful life. DDB does not explicitly use the asset's residual value in the calculations, but depreciation ends once the estimated residual value has been reached. If the asset is expected to have no residual value, the DB method will never fully depreciate it, so the DB method is typically changed to straight-line at some point in the asset's life.
    # @param t    length of the useful life
    # @example
    #   Finrb::Accounting.ddb(cost=1200,rv=200,t=5)
    def self.ddb(cost:, rv:, t:)
      cost = Flt::DecNum(cost.to_s)
      rv = Flt::DecNum(rv.to_s)
      t = Flt::DecNum(t.to_s)

      raise(Error, 't should be larger than 1') if t < 2

      ddb = [Flt::DecNum(0)] * t
      ddb[0] = cost * 2 / t
      if cost - ddb.first <= rv
        ddb[0] = cost - rv
      else
        cost -= ddb.first
        (1...t).each do |i|
          ddb[i] = cost * 2 / t
          if cost - ddb[i] <= rv
            ddb[i] = cost - rv
            break
          else
            cost -= ddb[i]
          end
        end
      end
      { t: (0...t).to_a, ddb: }
    end

    # Depreciation Expense Recognition -- Straight-line depreciation (SL) allocates an equal amount of depreciation each year over the asset's useful life
    #
    # @param cost cost of long-lived assets
    # @param rv   residual value of the long-lived assets at the end of its useful life
    # @param t    length of the useful life
    # @example
    #   Finrb::Accounting.slde(cost=1200,rv=200,t=5)
    def self.slde(cost:, rv:, t:)
      cost = Flt::DecNum(cost.to_s)
      rv = Flt::DecNum(rv.to_s)
      t = Flt::DecNum(t.to_s)

      ((cost - rv) / t)
    end
  end
end
