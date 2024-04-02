# frozen_string_literal: true

describe('Amortization') do
  # frozen_string_literal: true
  # @see https://tinyurl.com/6zroqvd for detailed calculations for the
  #   examples in these unit tests.
  def ipmt(principal, rate, payment, period)
    -(((-rate * principal) * ((rate + 1)**(period - 1))) - (payment * (((rate + 1)**(period - 1)) - 1))).round(2)
  end
  describe('amortization with a 0% rate') do
    it('does not raise a divide-by-zero error') do
      rate = Rate.new(0, :apr, duration: (30 * 12))
      Amortization.new(D(10_000), rate)
    end
  end

  describe('a fixed-rate amortization of 200000 at 3.75% over 30 years') do
    before do
      @rate = Rate.new(0.0375, :apr, duration: (30 * 12))
      @principal = D(200_000)
      @std = Amortization.new(@principal, @rate)
    end

    it('has a principal of $200,000') do
      expect(@std.principal).to(eq(@principal))
    end

    it('has a final balance of zero') do
      expect(@std.balance).to(be_zero)
    end

    it('has a duration of 360 months') do
      expect(@std.duration).to(eq(360))
    end

    it('has a monthly payment of $926.23') do
      expect(@std.payment).to(eq(D('-926.23')))
    end

    it('has a final payment of $926.96 (due to rounding)') do
      expect(@std.payments.last).to(eq(D('-926.96')))
    end

    it('has total payments of $333,443.53') do
      expect(@std.payments.sum).to(eq(D('-333443.53')))
    end

    it('has interest charges which agree with the standard formula') do
      0.upto(359) do |period|
        expect(ipmt(@principal, @rate.monthly, @std.payment, (period + 1))).to(eq(@std.interest[period]))
      end
    end

    it('has total interest charges of $133,433.33') do
      expect(@std.interest.sum).to(eq(D('133443.53')))
    end
  end

  describe('an adjustable rate amortization of 200000 starting at 3.75% and increasing by 1% every 3 years') do
    before do
      @rates = []
      0.upto(9) do |adj|
        (@rates << Rate.new(((D('0.01') * adj) + 0.0375), :apr, duration: (3 * 12)))
      end
      @principal = D(200_000)
      @arm = Amortization.new(@principal, *@rates)
    end

    it('has a principal of $200,000') do
      expect(@arm.principal).to(eq(@principal))
    end

    it('has a final balance of zero') do
      expect(@arm.balance).to(be_zero)
    end

    it('has a duration of 360 months') do
      expect(@arm.duration).to(eq(360))
    end

    it('does not have a fixed monthly payment (since it changes)') do
      expect(@arm.payment).to(be_nil)
    end

    it('has payments which increase every three years') do
      values = ['926.23', '1033.73', '1137.32', '1235.39', '1326.30', '1408.27', '1479.28', '1537.03', '1578.84', '1601.66']
      values.map! { |v| -D(v) }
      payments = []
      values[0, 9].each { |v| 36.times { (payments << v) } }
      35.times { (payments << values[9]) }
      payments[(0..-2)].each_with_index do |payment, index|
        expect(@arm.payments[index]).to(eq(payment))
      end
    end

    it('has a final payment of $1601.78 (due to rounding)') do
      expect(@arm.payments.last).to(eq(D('-1601.78')))
    end

    it('has total payments of $47,505.92') do
      expect(@arm.payments.sum).to(eq(D('-477505.92')))
    end

    it('has total interest charges of $277,505.92') do
      expect(@arm.interest.sum).to(eq(D('277505.92')))
    end
  end

  describe('a fixed-rate amortization of 200000 at 3.75% over 30 years, where an additional 100 is paid each month') do
    before do
      @rate = Rate.new(0.0375, :apr, duration: (30 * 12))
      @principal = D(200_000)
      @exp = Amortization.new(@principal, @rate) { |period| (period.payment - 100) }
    end

    it('has a principal of $200,000') do
      expect(@exp.principal).to(eq(@principal))
    end

    it('has a final balance of zero') do
      expect(@exp.balance).to(be_zero)
    end

    it('has a duration of 301 months') do
      expect(@exp.duration).to(eq(301))
    end

    it('has a monthly payment of $1026.23') do
      expect(@exp.payment).to(eq(D('-1026.23')))
    end

    it('has a final payment of $1011.09') do
      expect(@exp.payments.last).to(eq(D('-1011.09')))
    end

    it('has total payments of $308,880.09') do
      expect(@exp.payments.sum).to(eq(D('-308880.09')))
    end

    it('has total additional payments of $30,084.86') do
      expect(@exp.additional_payments.sum).to(eq(D('-30084.86')))
    end

    it('has total interest charges of $108880.09') do
      expect(@exp.interest.sum).to(eq(D('108880.09')))
    end
  end

  describe('Numeric Method') do
    it('works with simple invocation') do
      rate = Rate.new(0.0375, :apr, duration: (30 * 12))
      amt_method = 300_000.amortize(rate)
      amt_class = Amortization.new(300_000, rate)
      expect(amt_class).to(eq(amt_method))
    end

    it('works with block invocation') do
      rate = Rate.new(0.0375, :apr, duration: (30 * 12))
      amt_method = 300_000.amortize(rate) { |period| (period.payment - 300) }
      amt_class = Amortization.new(300_000, rate) { |period| (period.payment - 300) }
      expect(amt_class).to(eq(amt_method))
    end
  end
end
