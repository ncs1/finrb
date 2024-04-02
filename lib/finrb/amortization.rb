# frozen_string_literal: true

require_relative 'cashflows'
require_relative 'decimal'
require_relative 'transaction'

module Finrb
  # the Amortization class provides an interface for working with loan amortizations.
  # @note There are _two_ ways to create an amortization.  The first
  #   example uses the amortize method for the Numeric class.  The second
  #   calls Amortization.new directly.
  # @example Borrow $250,000 under a 30 year, fixed-rate loan with a 4.25% APR
  #   rate = Rate.new(0.0425, :apr, :duration => (30 * 12))
  #   amortization = 250000.amortize(rate)
  # @example Borrow $250,000 under a 30 year, adjustable rate loan, with an APR starting at 4.25%, and increasing by 1% every five years
  #   values = %w{ 0.0425 0.0525 0.0625 0.0725 0.0825 0.0925 }
  #   rates = values.collect { |value| Rate.new( value, :apr, :duration = (5 * 12) ) }
  #   arm = Amortization.new(250000, *rates)
  # @example Borrow $250,000 under a 30 year, fixed-rate loan with a 4.25% APR, but pay $150 extra each month
  #   rate = Rate.new(0.0425, :apr, :duration => (5 * 12))
  #   extra_payments = 250000.amortize(rate){ |period| period.payment - 150 }
  # @api public
  class Amortization
    # @return [Flt::DecNum] the balance of the loan at the end of the amortization period (usually zero)
    # @api public
    attr_reader :balance
    # @return [Flt::DecNum] the required monthly payment.  For loans with more than one rate, returns nil
    # @api public
    attr_reader :payment
    # @return [Flt::DecNum] the principal amount of the loan
    # @api public
    attr_reader :principal
    # @return [Array] the interest rates used for calculating the amortization
    # @api public
    attr_reader :rates

    # @return [Flt::DecNum] the periodic payment due on a loan
    # @param [Flt::DecNum] principal the initial amount of the loan or investment
    # @param [Rate] rate the applicable interest rate (per period)
    # @param [Integer] periods the number of periods needed for repayment
    # @note in most cases, you will probably want to use rate.monthly when calling this function outside of an Amortization instance.
    # @example
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   rate.duration #=> 360
    #   Amortization.payment(200000, rate.monthly, rate.duration) #=> Flt::DecNum('-926.23')
    # @see https://en.wikipedia.org/wiki/Amortization_calculator
    # @api public
    def self.payment(principal, rate, periods)
      if rate.zero?
        # simplified formula to avoid division-by-zero when interest rate is zero
        -(principal / periods).round(2)
      else
        -(principal * (rate + (rate / (((rate + 1)**periods) - 1)))).round(2)
      end
    end

    # create a new Amortization instance
    # @return [Amortization]
    # @param [Flt::DecNum] principal the initial amount of the loan or investment
    # @param [Rate] rates the applicable interest rates
    # @param [Proc] block
    # @api public
    def initialize(principal, *rates, &block)
      @principal = Flt::DecNum.new(principal.to_s)
      @rates     = rates
      @block     = block

      # compute the total duration from all of the rates.
      @periods = rates.sum(&:duration)
      @period  = 0

      compute
    end

    # compare two Amortization instances
    # @return [Numeric] -1, 0, or +1
    # @param [Amortization] other
    # @api public
    def ==(other)
      (principal == other.principal) && (rates == other.rates) && (payments == other.payments)
    end

    # @return [Array] the amount of any additional payments in each period
    # @example
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = 300000.amortize(rate){ |payment| payment.amount-100}
    #   amt.additional_payments #=> [Flt::DecNum('-100.00'), Flt::DecNum('-100.00'), ... ]
    # @api public
    def additional_payments
      @transactions.filter_map { |trans| trans.difference if trans.payment? }
    end

    # amortize the balance of loan with the given interest rate
    # @return none
    # @param [Rate] rate the interest rate to use in the amortization
    # @api private
    def amortize(rate)
      # For the purposes of calculating a payment, the relevant time
      # period is the remaining number of periods in the loan, not
      # necessarily the duration of the rate itself.
      periods = @periods - @period
      amount = Amortization.payment(@balance, rate.monthly, periods)

      pmt = Payment.new(amount, period: @period)
      pmt.modify(&@block) if @block

      rate.duration.to_i.times do
        # Do this first in case the balance is zero already.
        break if @balance.zero?

        # Compute and record interest on the outstanding balance.
        int = (@balance * rate.monthly).round(2)
        interest = Interest.new(int, period: @period)
        @balance += interest.amount
        @transactions << interest.dup

        # Record payment.  Don't pay more than the outstanding balance.
        pmt.amount = -@balance if pmt.amount.abs > @balance
        @transactions << pmt.dup
        @balance += pmt.amount

        @period += 1
      end
    end

    # compute the amortization of the principal
    # @return none
    # @api private
    def compute
      @balance = @principal
      @transactions = []

      @rates.each do |rate|
        amortize(rate)
      end

      # Add any remaining balance due to rounding error to the last payment.
      if @balance.nonzero?
        @transactions.reverse.find(&:payment?).amount -= @balance
        @balance = 0
      end

      @payment = (payments.first if @rates.length == 1)

      @transactions.freeze
    end

    # @return [Integer] the time required to pay off the loan, in months
    # @example In most cases, the duration is equal to the total duration of all rates
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = 300000.amortize(rate)
    #   amt.duration #=> 360
    # @example Extra payments may reduce the duration
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = 300000.amortize(rate){ |payment| payment.amount-100}
    #   amt.duration #=> 319
    # @api public
    def duration
      payments.length
    end

    # @api public
    def inspect
      "Amortization.new(#{@principal})"
    end

    # @return [Array] the amount of interest charged in each period
    # @example find the total cost of interest for a loan
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = 300000.amortize(rate)
    #   amt.interest.sum #=> Flt::DecNum('200163.94')
    # @example find the total interest charges in the first six months
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = 300000.amortize(rate)
    #   amt.interest[0,6].sum #=> Flt::DecNum('5603.74')
    # @api public
    def interest
      @transactions.filter_map { |trans| trans.amount if trans.interest? }
    end

    # @return [Array] the amount of the payment in each period
    # @example find the total payments for a loan
    #   rate = Rate.new(0.0375, :apr, :duration => (30 * 12))
    #   amt = 300000.amortize(rate)
    #   amt.payments.sum #=> Flt::DecNum('-500163.94')
    # @api public
    def payments
      @transactions.filter_map { |trans| trans.amount if trans.payment? }
    end
  end
end

class Numeric
  # @see Amortization#new
  # @api public
  def amortize(...)
    Finrb::Amortization.new(self, ...)
  end
end
