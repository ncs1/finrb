describe("Rates") do
  describe("an interest rate") do
    describe("can compound with different periods") do
      it("should compound monthly by default") do
        rate = Rate.new(0.15, :nominal)
        expect(rate.effective.round(5)).to(eq(D("0.16075")))
      end
      it("should compound annually") do
        rate = Rate.new(0.15, :nominal, :compounds => :annually)
        expect(rate.effective).to(eq(D("0.15")))
      end
      it("should compound continuously") do
        rate = Rate.new(0.15, :nominal, :compounds => :continuously)
        expect(rate.effective.round(5)).to(eq(D("0.16183")))
      end
      it("should compound daily") do
        rate = Rate.new(0.15, :nominal, :compounds => :daily)
        expect(rate.effective.round(5)).to(eq(D("0.16180")))
      end
      it("should compound quarterly") do
        rate = Rate.new(0.15, :nominal, :compounds => :quarterly)
        expect(rate.effective.round(5)).to(eq(D("0.15865")))
      end
      it("should compound semiannually") do
        rate = Rate.new(0.15, :nominal, :compounds => :semiannually)
        expect(rate.effective.round(5)).to(eq(D("0.15563")))
      end
      it("should accept a numerical value as the compounding frequency per year") do
        rate = Rate.new(0.15, :nominal, :compounds => 7)
        expect(rate.effective.round(5)).to(eq(D("0.15999")))
      end
      it("should raise an exception if an unknown string is given") do
        expect { Rate.new(0.15, :nominal, :compounds => :quickly) }.to(raise_error(ArgumentError))
      end
    end
    it("should accept a duration if given") do
      rate = Rate.new(0.0375, :effective, :duration => 360)
      expect(rate.duration).to(eq(360))
    end
    it("should be comparable to other interest rates") do
      r1 = Rate.new(0.15, :nominal)
      r2 = Rate.new(0.16, :nominal)
      expect((r2 <=> r1)).to(eq(1))
      expect((r1 <=> r2)).to(eq(-1))
    end
    it("should convert to a monthly value") do
      rate = Rate.new(0.0375, :effective)
      expect(rate.monthly).to(eq(D("0.003125")))
    end
    it("should convert effective interest rates to nominal") do
      expect(Rate.to_nominal(D("0.0375"), 12).round(5)).to(eq(D("0.03687")))
      expect(Rate.to_nominal(D("0.0375"), Flt::DecNum.infinity).round(5)).to(eq(D("0.03681")))
    end
    it("should raise an exception if an unknown value is given for :type") do
      expect { Rate.new(0.0375, :foo) }.to(raise_error(ArgumentError))
    end
  end
end
