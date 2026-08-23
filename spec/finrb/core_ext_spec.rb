# frozen_string_literal: true

require 'open3'
require 'rbconfig'

describe(Finrb::CoreExt) do
  def ruby_eval(source)
    Open3.capture3(RbConfig.ruby, '-Ilib', '-e', source)
  end

  it('does not modify Array or Numeric when loading finrb') do
    stdout, stderr, status = ruby_eval("require 'finrb'; print [1].respond_to?(:irr), 1.respond_to?(:amortize), 1.respond_to?(:to_dec)")

    expect(status).to(be_success)
    expect(stderr).to(be_empty)
    expect(stdout).to(eq('falsefalsefalse'))
  end

  it('provides the legacy fluent API only when explicitly required') do
    source = "require 'finrb/core_ext'; print [-100, 110].irr.round(1), ',', 1.to_dec"
    stdout, stderr, status = ruby_eval(source)

    expect(status).to(be_success)
    expect(stderr).to(be_empty)
    expect(stdout).to(eq('0.1,1'))
  end
end
