# frozen_string_literal: true

module Finrb
  class Error < StandardError; end
  class ConvergenceError < Error; end
  class DomainError < Error; end
  class InvalidCashflowError < Error; end
end
