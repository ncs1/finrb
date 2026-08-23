# frozen_string_literal: true

require 'rake'
require 'rspec/core/rake_task'

task(:spec).clear
RSpec::Core::RakeTask.new(:spec) do |t|
  t.verbose = true
end

task default: %i[spec]

desc 'Run the suite with line and branch coverage'
task :coverage do
  previous_coverage = ENV.fetch('COVERAGE', nil)
  ENV['COVERAGE'] = 'true'
  sh('bundle', 'exec', 'rspec')
ensure
  ENV['COVERAGE'] = previous_coverage
end

desc 'Run all self-contained quality checks'
task quality: %i[coverage rbs:validate]

namespace :rbs do
  desc 'Validate packaged RBS signatures'
  task :validate do
    sh('bundle', 'exec', 'rbs', '-I', 'sig', 'validate')
  end
end

namespace :solver do
  desc 'Verify IRR/XIRR against SciPy and QuantLib references'
  task :verify do
    options = { count: ENV.fetch('COUNT', '100'), seed: ENV.fetch('SEED', '20260825'), workers: ENV.fetch('WORKERS', '4'), batch_size: ENV.fetch('BATCH_SIZE', '50') }
    arguments = options.flat_map { |name, value| ["--#{name.to_s.tr('_', '-')}", value] }
    sh ENV.fetch('PYTHON', 'python3'), File.join(__dir__, 'script', 'verify_solver.py'), *arguments
  end
end

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

  namespace :arm64 do
    desc 'Build an ARM64 development image with Docker Buildx'
    task :build do
      sh('docker', 'buildx', 'build', '--platform', 'linux/arm64', '--target', 'development', '--tag', 'finrb:1.0-arm64', '--load', '--file', 'Dockerfile', '.')
    end

    desc 'Run the quality suite in an emulated ARM64 Docker build'
    task :test do
      sh('docker', 'buildx', 'build', '--platform', 'linux/arm64', '--target', 'testing', '--tag', 'finrb:testing-arm64', '--load', '--file', 'Dockerfile', '.')
    end

    desc 'Run the ARM64 development image on this Docker host'
    task :run do
      sh('docker', 'run', '--platform', 'linux/arm64', '--init', '--interactive', '--tty', '--rm', 'finrb:1.0-arm64')
    end
  end

  desc 'Build and run solver reference verification in Docker'
  task :verify_solver do
    image = 'finrb:solver-verification'
    sh 'docker', 'build', '--target', 'solver-verification', '--tag', image, '--file', 'Dockerfile', '.'

    command = ['docker', 'run', '--rm']
    %w[COUNT SEED WORKERS BATCH_SIZE].each do |name|
      command.push('--env', "#{name}=#{ENV.fetch(name)}") if ENV.key?(name)
    end
    sh(*command, image)
  end

  desc 'Run dev docker instance'
  task :run do
    system 'docker run --init -it --rm finrb:1.0'
  end
end
