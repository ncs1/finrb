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
  end

  describe('slde') do
    it('Example 1') do
      res = Accounting.slde(cost: 1200, rv: 200, t: 5)
      expect(res).to(be_an_instance_of(Flt::DecNum))
      expect(res).to(be_within(D('0.00001')).of(D('200')))
    end
  end
end
