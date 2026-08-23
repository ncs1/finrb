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

namespace :security do
  desc 'Update ruby-advisory-db and audit locked dependencies'
  task :audit do
    sh('bundle', 'exec', 'bundle-audit', 'check', '--update')
  end
end

namespace :package do
  desc 'Build, inspect, install, and smoke-test the gem without publishing it'
  task :verify do
    require('bundler')
    require('fileutils')
    require('rbconfig')

    scratch_dir = File.expand_path('tmp/finrb-package-verification', __dir__)
    artifact = File.join(scratch_dir, 'finrb.gem')
    gem_home = File.join(scratch_dir, 'gems')
    gem_environment = { gem_home:, gem_path: gem_home }.transform_keys { |key| key.to_s.upcase }

    FileUtils.rm_rf(scratch_dir)
    FileUtils.mkdir_p(gem_home)

    sh('gem', 'build', 'finrb.gemspec', '--output', artifact)
    sh(RbConfig.ruby, File.join(__dir__, 'script', 'verify_gem.rb'), artifact)
    Bundler.with_unbundled_env do
      sh(gem_environment, 'gem', 'install', artifact, '--no-document')

      Dir.chdir(scratch_dir) do
        sh(gem_environment, RbConfig.ruby, File.join(__dir__, 'script', 'smoke_gem.rb'))
      end
    end
  ensure
    FileUtils.rm_rf(scratch_dir) if defined?(scratch_dir) && scratch_dir
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
  alternative_dockerfile = 'Dockerfile.engines'
  alternative_images = { jruby: ENV.fetch('JRUBY_IMAGE', 'jruby:10-jdk21'), truffleruby: ENV.fetch('TRUFFLERUBY_IMAGE', 'ghcr.io/graalvm/truffleruby-community:latest') }
  build_alternative =
    lambda do |engine, target, tag|
      sh('docker', 'build', '--build-arg', "RUBY_IMAGE=#{alternative_images.fetch(engine)}", '--build-arg', "EXPECTED_RUBY_ENGINE=#{engine}", '--target', target, '--tag', tag, '--file', alternative_dockerfile, '.')
    end

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

  namespace :jruby do
    desc 'Build the JRuby development image'
    task :build do
      build_alternative.call(:jruby, 'development', 'finrb:jruby')
    end

    desc 'Build finrb and run its specs on JRuby'
    task :test do
      build_alternative.call(:jruby, 'testing', 'finrb:jruby-testing')
    end

    desc 'Run the JRuby development image'
    task :run do
      sh('docker', 'run', '--init', '--interactive', '--tty', '--rm', 'finrb:jruby')
    end
  end

  namespace :truffleruby do
    desc 'Build the TruffleRuby development image'
    task :build do
      build_alternative.call(:truffleruby, 'development', 'finrb:truffleruby')
    end

    desc 'Build finrb and run its specs on TruffleRuby'
    task :test do
      build_alternative.call(:truffleruby, 'testing', 'finrb:truffleruby-testing')
    end

    desc 'Run the TruffleRuby development image'
    task :run do
      sh('docker', 'run', '--init', '--interactive', '--tty', '--rm', 'finrb:truffleruby')
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
