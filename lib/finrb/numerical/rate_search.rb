# frozen_string_literal: true

require 'flt'
require_relative '../errors'

module Finrb
  module Numerical
    # Locates the nearest sign-changing rate interval around a caller's guess.
    # Search happens in log(1 + rate) space, which covers the entire financial
    # domain rate > -1 without stepping across its singular boundary.
    class RateSearch
      DEFAULT_STEP = '0.125'
      DEFAULT_MAX_STEPS = 256
      private_constant :DEFAULT_STEP, :DEFAULT_MAX_STEPS

      def initialize(step: DEFAULT_STEP, max_steps: DEFAULT_MAX_STEPS)
        @step = decimal(step)
        @max_steps = Integer(max_steps)

        raise(ArgumentError, 'Search step must be positive.') unless @step.positive?
        raise(ArgumentError, 'Maximum search steps must be positive.') unless @max_steps.positive?
      end

      def bracket(function, guess:)
        guess = decimal(guess)
        raise(DomainError, 'Rate guess must be greater than -1.') if guess <= -1

        center_coordinate = (guess + 1).ln
        center = [guess, evaluate(function, guess)]
        return [guess, guess] if center.last.zero?

        left = center
        right = center

        1.upto(@max_steps) do |distance|
          next_left = point(function, center_coordinate - (@step * distance))
          next_right = point(function, center_coordinate + (@step * distance))
          candidates = []
          candidates << [next_left.first, left.first] if opposite_signs?(next_left.last, left.last)
          candidates << [right.first, next_right.first] if opposite_signs?(right.last, next_right.last)
          return nearest(candidates, guess) unless candidates.empty?

          left = next_left
          right = next_right
        end

        raise(ConvergenceError, "Could not bracket a root near guess #{guess}.")
      end

      private

      def point(function, coordinate)
        rate = coordinate.exp - 1
        [rate, evaluate(function, rate)]
      end

      def evaluate(function, rate)
        result = decimal(function.call(rate))
        raise(DomainError, "Rate function returned a non-finite value at #{rate}.") unless result.finite?

        result
      rescue Flt::Num::Exception, FloatDomainError, Math::DomainError, ZeroDivisionError => e
        raise(DomainError, "Rate function is undefined at #{rate}: #{e.message}", e.backtrace)
      end

      def nearest(candidates, guess)
        candidates.min_by { |lower, upper| (((lower + upper) / 2) - guess).abs }
      end

      def opposite_signs?(left, right)
        left.negative? != right.negative?
      end

      def decimal(value)
        Flt::DecNum.new(value.to_s)
      end
    end
  end
end
