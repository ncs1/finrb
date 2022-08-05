# frozen_string_literal: true

require_relative "test_helper"

describe "Cashflows" do
  describe "default values" do
    it "should have default values" do
      assert_equal D("1.0"), D(Finrb.config.guess.to_s)
      assert_equal D("1.0e-16"), D(Finrb.config.eps)
      refute Finrb.config.business_days
      refute Finrb.config.periodic_compound
    end
  end

  describe "overriding defaults" do
    before do
      Finrb.config.guess = 0.25
      Finrb.config.eps = "1.0e-9"
      Finrb.config.business_days = true
      Finrb.config.periodic_compound = true
    end

    after do
      Finrb.config.guess = 1.0
      Finrb.config.eps = "1.0e-16"
      Finrb.config.business_days = false
      Finrb.config.periodic_compound = false
    end

    it "should be permanent" do
      assert_in_delta(0.25, Finrb.config.guess)
      assert_equal "1.0e-9", Finrb.config.eps
      assert Finrb.config.business_days
      assert Finrb.config.periodic_compound
    end
  end
end
