# frozen_string_literal: true

require_relative 'test_helper'

describe 'Cashflows' do
  describe 'an array of numeric cashflows' do
    it 'should have an Internal Rate of Return' do
      assert_equal D('0.143'), [-4000, 1200, 1410, 1875, 1050].irr.round(3)
      assert_raises(ArgumentError) { [10, 20, 30].irr }
    end

    it 'should have a Net Present Value' do
      assert_equal D('49.211'), [-100.0, 60, 60, 60].npv(0.1).round(3)
    end
  end

  describe 'guess with business days' do
    before(:all) do
      Finance.config.business_days = true
      Finance.config.periodic_compound = true
      @transactions = []
      @transactions << Transaction.new(-2_906_071.23, date: Date.new(2017, 8, 31))
      @transactions << Transaction.new(8000.00, date: Date.new(2017, 9, 4))
      @transactions << Transaction.new(2_876_570.16, date: Date.new(2017, 9, 29))
    end

    after(:all) do
      Finance.config.business_days = false
      Finance.config.periodic_compound = false
    end

    it 'should fail to calculate with default guess (1.0)' do
      assert_raises(Flt::Num::InvalidOperation) { @transactions.xirr.apr.to_i }
    end

    it 'should calculate correct rate with new guess (0.5)' do
      assert_equal D('-0.00742'), @transactions.xirr(0.5).effective.round(5)
    end
  end

  describe 'guess' do
    before(:all) do
      @transactions = []
      @transactions << Transaction.new(-1000, date: Time.new(1957, 1, 1))
      @transactions << Transaction.new(390_000, date: Time.new(2013, 1, 1))
    end

    it 'should fail to calculate with default guess (1.0)' do
      assert_equal(-9_999_999_999_998, @transactions.xirr.apr.to_i)
    end

    it 'should calculate correct rate with new guess (0.1)' do
      assert_equal D('0.11234'), @transactions.xirr(0.1).apr.round(5)
    end

    it 'should not allow non-numeric guesses' do
      assert_raises(ArgumentError) { @transactions.xirr('error') }
    end
  end
end
