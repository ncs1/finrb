# frozen_string_literal: true

describe('Rates') do
  describe('an interest rate') do
    describe('can compound with different periods') do
      it('compounds monthly by default') do
        rate = Rate.new(0.15, :nominal)
        expect(rate.effective.round(5)).to(eq(D('0.16075')))
      end

      it('compounds annually') do
        rate = Rate.new(0.15, :nominal, compounds: :annually)
        expect(rate.effective).to(eq(D('0.15')))
      end

      it('compounds continuously') do
        rate = Rate.new(0.15, :nominal, compounds: :continuously)
        expect(rate.effective.round(5)).to(eq(D('0.16183')))
      end

      it('compounds daily') do
        rate = Rate.new(0.15, :nominal, compounds: :daily)
        expect(rate.effective.round(5)).to(eq(D('0.16180')))
      end

      it('compounds quarterly') do
        rate = Rate.new(0.15, :nominal, compounds: :quarterly)
        expect(rate.effective.round(5)).to(eq(D('0.15865')))
      end

      it('compounds semiannually') do
        rate = Rate.new(0.15, :nominal, compounds: :semiannually)
        expect(rate.effective.round(5)).to(eq(D('0.15563')))
      end

      it('accepts a numerical value as the compounding frequency per year') do
        rate = Rate.new(0.15, :nominal, compounds: 7)
        expect(rate.effective.round(5)).to(eq(D('0.15999')))
      end

      it('raises an exception if an unknown string is given') do
        expect { Rate.new(0.15, :nominal, compounds: :quickly) }
          .to(raise_error(ArgumentError))
      end
    end

    it('accepts a duration if given') do
      rate = Rate.new(0.0375, :effective, duration: 360)
      expect(rate.duration).to(eq(360))
    end

    it('is comparable to other interest rates') do
      r1 = Rate.new(0.15, :nominal)
      r2 = Rate.new(0.16, :nominal)
      expect((r2 <=> r1)).to(eq(1))
      expect((r1 <=> r2)).to(eq(-1))
    end

    it('converts to a monthly value') do
      rate = Rate.new(0.0375, :effective)
      expect(rate.monthly).to(eq(D('0.003125')))
    end

    it('converts effective interest rates to nominal') do
      expect(Rate.to_nominal(D('0.0375'), 12).round(5)).to(eq(D('0.03687')))
      expect(Rate.to_nominal(D('0.0375'), Flt::DecNum.infinity).round(5)).to(eq(D('0.03681')))
    end

    it('raises an exception if an unknown value is given for :type') do
      expect { Rate.new(0.0375, :foo) }
        .to(raise_error(ArgumentError))
    end
  end
end
