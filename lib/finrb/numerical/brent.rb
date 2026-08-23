# frozen_string_literal: true

require 'flt'
require_relative '../errors'

module Finrb
  module Numerical
    # Brent-Dekker root solver for a continuous function on a sign-changing
    # interval. The interpolation steps are safeguarded by bisection.
    #
    # Algorithm: R. P. Brent, Algorithms for Minimization Without Derivatives,
    # Chapter 4 (1973). See also the GNU GSL root-finding documentation:
    # https://www.gnu.org/software/gsl/doc/html/roots.html
    class Brent
      DEFAULT_MAX_ITERATIONS = 256
      private_constant :DEFAULT_MAX_ITERATIONS

      def initialize(tolerance:, relative_tolerance: tolerance, max_iterations: DEFAULT_MAX_ITERATIONS)
        @absolute_tolerance = decimal(tolerance)
        @relative_tolerance = decimal(relative_tolerance)
        @max_iterations = Integer(max_iterations)

        raise(ArgumentError, 'Tolerance must be positive.') unless @absolute_tolerance.positive? && @relative_tolerance.positive?
        raise(ArgumentError, 'Maximum iterations must be positive.') unless @max_iterations.positive?
      end

      def solve(function, lower:, upper:)
        left = decimal(lower)
        right = decimal(upper)
        left_value = evaluate(function, left)
        right_value = evaluate(function, right)

        return left if left_value.zero?
        return right if right_value.zero?
        raise(ConvergenceError, 'Root is not bracketed.') unless opposite_signs?(left_value, right_value)

        left, right, left_value, right_value = best_approximation_last(left, right, left_value, right_value)

        previous = left
        previous_value = left_value
        penultimate = previous
        bisected = true

        @max_iterations.times do
          tolerance = @absolute_tolerance + (@relative_tolerance * right.abs)
          return right if right_value.zero? || (right - left).abs <= tolerance

          candidate =
            if distinct_values?(left_value, right_value, previous_value)
              inverse_quadratic(left, right, previous, left_value, right_value, previous_value)
            else
              right - (right_value * (right - left) / (right_value - left_value))
            end

          bound = ((left * 3) + right) / 4
          outside_safe_interval = candidate <= [bound, right].min || candidate >= [bound, right].max
          insufficient_progress =
            (candidate - right).abs >= if bisected
                                         ((right - previous).abs / 2)
                                       else
                                         ((previous - penultimate).abs / 2)
                                       end
          bracket_too_small =
            if bisected
              (right - previous).abs < tolerance
            else
              (previous - penultimate).abs < tolerance
            end

          if outside_safe_interval || insufficient_progress || bracket_too_small
            candidate = (left + right) / 2
            bisected = true
          else
            bisected = false
          end

          candidate_value = evaluate(function, candidate)
          penultimate = previous
          previous = right
          previous_value = right_value

          if opposite_signs?(left_value, candidate_value)
            right = candidate
            right_value = candidate_value
          else
            left = candidate
            left_value = candidate_value
          end

          left, right, left_value, right_value = best_approximation_last(left, right, left_value, right_value)
        end

        raise(ConvergenceError, "Calculation did not converge after #{@max_iterations} iterations.")
      end

      private

      def inverse_quadratic(left, right, previous, left_value, right_value, previous_value)
        left_term = (left * right_value * previous_value) / ((left_value - right_value) * (left_value - previous_value))
        right_term = (right * left_value * previous_value) / ((right_value - left_value) * (right_value - previous_value))
        previous_term = (previous * left_value * right_value) / ((previous_value - left_value) * (previous_value - right_value))
        left_term + right_term + previous_term
      end

      def best_approximation_last(left, right, left_value, right_value)
        return [right, left, right_value, left_value] if left_value.abs < right_value.abs

        [left, right, left_value, right_value]
      end

      def distinct_values?(*values)
        values.uniq.length == values.length
      end

      def evaluate(function, value)
        result = decimal(function.call(value))
        raise(DomainError, "Solver function returned a non-finite value at #{value}.") unless result.finite?

        result
      rescue Flt::Num::Exception, FloatDomainError, Math::DomainError, ZeroDivisionError => e
        raise(DomainError, "Solver function is undefined at #{value}: #{e.message}", e.backtrace)
      end

      def decimal(value)
        Flt::DecNum.new(value.to_s)
      end

      def opposite_signs?(left, right)
        left.negative? != right.negative?
      end
    end
  end
end
