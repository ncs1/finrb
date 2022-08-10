# frozen_string_literal: true

require 'rake'
require 'rspec/core/rake_task'

task(:spec).clear
RSpec::Core::RakeTask.new(:spec) do |t|
  t.verbose = true
end

task default: %i[spec]

namespace :docker do
  desc 'Build docker instance'
  task :build do
    Dir.chdir(__dir__.to_s) do
      system 'docker build --target development -t finrb:1.0 -f Dockerfile .'
    end
  end

  desc 'Run test docker build'
  task :test do
    Dir.chdir(__dir__.to_s) do
      system 'docker build --target testing -t finrb:1.0 -f Dockerfile .'
    end
  end

  desc 'Run dev docker instance'
  task :run do
    system 'docker run --init -it --rm finrb:1.0'
  end
end
