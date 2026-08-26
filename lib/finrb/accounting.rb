# frozen_string_literal: true

require_relative 'decimal'
require_relative 'errors'
require_relative 'validation'

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
      uinv, pinv, units, price, sinv, method = inventory_inputs(uinv:, pinv:, units:, price:, sinv:, method:)

      n = units.size
      cost_of_goods = Flt::DecNum(0)
      ending_inventory = Flt::DecNum(0)
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
              break
            else
              cost_of_goods += (units[i] * price[i])
              sinv -= units[i]
            end
          end
          raise(DomainError, 'Available inventory is insufficient for the requested sale.') if sinv.positive?
        end
      when 'WAC'
        ending_inventory = uinv * pinv
        tu = uinv
        (0...n).each do |i|
          ending_inventory += (units[i] * price[i])
          tu += units[i]
        end
        if tu.zero? && sinv.zero?
          cost_of_goods = Flt::DecNum(0)
          ending_inventory = Flt::DecNum(0)
        elsif tu >= sinv
          cost_of_goods = ending_inventory / tu * sinv
          ending_inventory = ending_inventory / tu * (tu - sinv)
        else
          raise(DomainError, 'Available inventory is insufficient for the requested sale.')
        end

      when 'LIFO'
        (n - 1).downto(0).each do |i|
          if sinv <= units[i]
            cost_of_goods += (sinv * price[i])
            ending_inventory = (units[i] - sinv) * price[i]
            if i.positive?
              temp = i - 1
              temp.downto(0).each do |j|
                ending_inventory += (units[j] * price[j])
              end
            end
            ending_inventory += (uinv * pinv)
            sinv = 0
            break
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
            raise(DomainError, 'Available inventory is insufficient for the requested sale.')
          end
        end
      end

      {
        cost_of_goods:,
        ending_inventory:
      }
    end

    def self.inventory_inputs(uinv:, pinv:, units:, price:, sinv:, method:)
      uinv = Validation.non_negative_decimal(uinv, name: 'beginning inventory units')
      pinv = Validation.non_negative_decimal(pinv, name: 'beginning inventory unit cost')
      units = inventory_values(units, name: 'purchase units')
      price = inventory_values(price, name: 'purchase unit cost')
      sinv = Validation.non_negative_decimal(sinv, name: 'units sold')
      method = method.to_s
      raise(ArgumentError, 'Inventory costing method must be FIFO, LIFO, or WAC.') unless %w[FIFO LIFO WAC].include?(method)
      raise(ArgumentError, 'Purchase units and unit costs must have equal lengths.') unless units.size == price.size

      [uinv, pinv, units, price, sinv, method]
    end
    private_class_method :inventory_inputs

    # Depreciation Expense Recognition -- double-declining balance (DDB), the most common declining balance method, which applies two times the straight-line rate to the declining balance.
    #
    # @param cost cost of long-lived assets
    # @param rv   residual value of the long-lived assets at the end of its useful life. DDB does not explicitly use the asset's residual value in the calculations, but depreciation ends once the estimated residual value has been reached. If the asset is expected to have no residual value, the DB method will never fully depreciate it, so the DB method is typically changed to straight-line at some point in the asset's life.
    # @param t    length of the useful life
    # @example
    #   Finrb::Accounting.ddb(cost=1200,rv=200,t=5)
    def self.ddb(cost:, rv:, t:)
      cost = Validation.non_negative_decimal(cost, name: 'asset cost')
      rv = residual_value(rv, cost:)
      t = Validation.positive_integer(t, name: 'useful life')
      raise(DomainError, 'Useful life must be at least 2 periods for double-declining depreciation.') if t < 2

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
      cost = Validation.non_negative_decimal(cost, name: 'asset cost')
      rv = residual_value(rv, cost:)
      t = Validation.positive_decimal(t, name: 'useful life', error: DomainError)

      ((cost - rv) / t)
    end

    def self.inventory_values(values, name:)
      wrap_array(values).map { |value| Validation.non_negative_decimal(value, name:) }
    end
    private_class_method :inventory_values

    def self.residual_value(value, cost:)
      value = Validation.non_negative_decimal(value, name: 'residual value')
      raise(DomainError, 'Residual value must not exceed asset cost.') if value > cost

      value
    end
    private_class_method :residual_value
  end
end
