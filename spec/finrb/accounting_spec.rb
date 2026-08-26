# frozen_string_literal: true

describe(Finrb::Accounting) do
  describe('cogs') do
    it('Example 1') do
      cogs = Accounting.cogs(uinv: 2, pinv: 2, units: [3, 5], price: [3, 5], sinv: 7, method: 'FIFO')
      res = cogs[:cost_of_goods]
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D(23)))
      res = cogs[:ending_inventory]
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D(15)))
    end

    it('Example 2') do
      cogs = Accounting.cogs(uinv: 2, pinv: 2, units: [3, 5], price: [3, 5], sinv: 7, method: 'LIFO')
      res = cogs[:cost_of_goods]
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D(31)))
      res = cogs[:ending_inventory]
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D(7)))
    end

    it('Example 3') do
      cogs = Accounting.cogs(uinv: 2, pinv: 2, units: [3, 5], price: [3, 5], sinv: 7, method: 'WAC')
      res = cogs[:cost_of_goods]
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('26.6')))
      res = cogs[:ending_inventory]
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('11.4')))
    end

    it('uses beginning inventory first under FIFO') do
      result = Accounting.cogs(uinv: 5, pinv: 2, units: 3, price: 4, sinv: 2, method: 'FIFO')

      expect(result).to(eq(cost_of_goods: D(4), ending_inventory: D(18)))
    end

    it('retains later purchase layers after satisfying a FIFO sale') do
      result = Accounting.cogs(uinv: 2, pinv: 2, units: [3, 5], price: [3, 5], sinv: 4, method: 'FIFO')

      expect(result).to(eq(cost_of_goods: D(10), ending_inventory: D(28)))
    end

    it('retains earlier purchase layers under LIFO') do
      result = Accounting.cogs(uinv: 2, pinv: 2, units: [3, 4, 5], price: [3, 4, 5], sinv: 2, method: 'LIFO')

      expect(result).to(eq(cost_of_goods: D(10), ending_inventory: D(44)))
    end

    it('retains every earlier layer when a LIFO sale ends in the second purchase layer') do
      result = Accounting.cogs(uinv: 2, pinv: 2, units: [3, 5], price: [3, 5], sinv: 2, method: 'LIFO')

      expect(result).to(eq(cost_of_goods: D(10), ending_inventory: D(28)))
      expect(result.values.sum).to(eq(D(38)))
    end

    it('uses beginning inventory after exhausting LIFO purchase layers') do
      result = Accounting.cogs(uinv: 2, pinv: 2, units: [3, 5], price: [3, 5], sinv: 9, method: 'LIFO')

      expect(result).to(eq(cost_of_goods: D(36), ending_inventory: D(2)))
    end

    it('accepts no purchase layers when beginning inventory covers the sale') do
      result = Accounting.cogs(uinv: 5, pinv: 2, units: nil, price: nil, sinv: 2, method: 'WAC')

      expect(result).to(eq(cost_of_goods: D(4), ending_inventory: D(6)))
    end

    it('rejects unavailable inventory under every costing method') do
      arguments = { uinv: 1, pinv: 2, units: [1], price: [3], sinv: 3 }

      %w[FIFO LIFO WAC].each do |method|
        expect { Accounting.cogs(**arguments, method:) }
          .to(raise_error(Finrb::DomainError, /inventory is insufficient/i))
      end
    end

    it('rejects mismatched purchase layers') do
      expect { Accounting.cogs(uinv: 1, pinv: 2, units: [1], price: [], sinv: 1) }
        .to(raise_error(ArgumentError, /equal lengths/))
    end

    it('rejects unknown methods and invalid inventory values') do
      expect { Accounting.cogs(uinv: 1, pinv: 2, units: [], price: [], sinv: 0, method: 'AVERAGE') }
        .to(raise_error(ArgumentError, /FIFO, LIFO, or WAC/))
      expect { Accounting.cogs(uinv: -1, pinv: 2, units: [], price: [], sinv: 0) }
        .to(raise_error(ArgumentError, /beginning inventory units/))
      expect { Accounting.cogs(uinv: 0, pinv: 0, units: [], price: [], sinv: 0, method: 'WAC') }
        .not_to(raise_error)
    end
  end

  describe('ddb') do
    it('Example 1') do
      ddb_result_hsh_expect = { t: (0..4).to_a, ddb: [D('480'), D('288'), D('172.8'), D('59.2'), D('0')] }

      ddb_result_hsh = Accounting.ddb(cost: 1200, rv: 200, t: 5)

      expect(ddb_result_hsh.size).to(eq(ddb_result_hsh_expect.size))
      expect(ddb_result_hsh[:t]).to(eq(ddb_result_hsh_expect[:t]))

      ddb_result_hsh[:ddb].each_with_index do |ddb_result, idx|
        res = ddb_result
        expect(res).to(be_an_instance_of(Flt::DecNum))
        expect(res).to(be_within(D('0.00001')).of(ddb_result_hsh_expect[:ddb][idx]))
      end
    end

    it('stops immediately at residual value and rejects short useful lives') do
      expect(Accounting.ddb(cost: 1200, rv: 800, t: 2)[:ddb]).to(eq([D(400), D(0)]))
      expect { Accounting.ddb(cost: 1200, rv: 200, t: 1) }
        .to(raise_error(Finrb::DomainError, /at least 2 periods/))
    end

    it('requires an integer useful life and a valid residual value') do
      expect { Accounting.ddb(cost: 1200, rv: 200, t: 2.5) }
        .to(raise_error(ArgumentError, /useful life must be a positive integer/))
      expect { Accounting.ddb(cost: 1200, rv: 1300, t: 5) }
        .to(raise_error(Finrb::DomainError, /must not exceed asset cost/))
    end
  end

  describe('slde') do
    it('Example 1') do
      res = Accounting.slde(cost: 1200, rv: 200, t: 5)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('200')))
    end

    it('requires a positive useful life and valid finite values') do
      expect { Accounting.slde(cost: 1200, rv: 200, t: 0) }
        .to(raise_error(Finrb::DomainError, /useful life must be greater than zero/))
      expect { Accounting.slde(cost: Float::INFINITY, rv: 200, t: 5) }
        .to(raise_error(ArgumentError, /asset cost must be finite/))
      expect { Accounting.slde(cost: 1200, rv: 1300, t: 5) }
        .to(raise_error(Finrb::DomainError, /must not exceed asset cost/))
    end
  end
end
