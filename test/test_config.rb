# frozen_string_literal: true

require_relative 'test_helper'

describe 'Cashflows' do
  describe 'default values' do
    it 'should have default values' do
      assert_equal D('1.0'), D(Finance.config.guess.to_s)
      assert_equal D('1.0e-16'), D(Finance.config.eps)
      assert_equal false, Finance.config.use_business_days
    end
  end

  describe 'overriding defaults' do
    before do
      Finance.config.guess = 0.25
      Finance.config.eps = '1.0e-9'
      Finance.config.use_business_days = true
    end

    after do
      Finance.config.guess = 1.0
      Finance.config.eps = '1.0e-16'
      Finance.config.use_business_days = false
    end

    it 'should be permanent' do
      assert_equal 0.25, Finance.config.guess
      assert_equal '1.0e-9', Finance.config.eps
      assert_equal true, Finance.config.use_business_days
    end
  end
end
