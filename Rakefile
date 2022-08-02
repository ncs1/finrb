# frozen_string_literal: true

require 'rake'
require 'rake/testtask'

task default: [:test_units]

Rake::TestTask.new('test_units') do |t|
  t.pattern = 'test/*.rb'
  t.verbose = true
  t.warning = true
end

namespace :docker do
  desc 'Build docker instance'
  task :build do
    Dir.chdir __dir__.to_s do
      exec 'docker build --target development -t finrb:1.0 -f Dockerfile .'
    end
  end

  task :test do
    Dir.chdir __dir__.to_s do
      exec 'docker build --target testing -t finrb:1.0 -f Dockerfile .'
    end
  end

  desc 'Run dev docker instance'
  task :run do
    exec 'docker run --init -it --rm finrb:1.0'
  end
end
