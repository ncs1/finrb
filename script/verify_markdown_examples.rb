# frozen_string_literal: true

require 'open3'
require 'rbconfig'

root = File.expand_path('..', __dir__)
paths = ARGV.empty? ? [File.join(root, 'docs', 'api.md')] : ARGV.map { |path| File.expand_path(path, root) }
marker = '<!-- verify-example -->'
pattern = /#{Regexp.escape(marker)}\s*```ruby\s*\n(.*?)^```/m
failures = []
verified = 0

paths.each do |path|
  source = File.read(path)
  examples = source.scan(pattern).flatten
  markers = source.scan(marker).length
  failures << "#{path}: found #{markers} markers but #{examples.length} executable Ruby blocks" if markers != examples.length

  examples.each_with_index do |example, index|
    command = [RbConfig.ruby, "-I#{File.join(root, 'lib')}", '-rfinrb', '-rdate', '-e', example]
    stdout, stderr, status = Open3.capture3(*command, chdir: root)
    verified += 1
    next if status.success?

    failures << <<~MESSAGE
      #{path}: verified example #{index + 1} failed
      #{stdout}#{stderr}
    MESSAGE
  end
end

if failures.empty?
  puts("Verified #{verified} Markdown examples")
else
  warn(failures.join("\n"))
  exit(1)
end
