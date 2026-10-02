# frozen_string_literal: true

require 'date'
require 'finrb'
require 'json'

CALENDARS = { us: Finrb::Calendars::USFederalReserve.new, israel: Finrb::Calendars::IsraelTase.new }.freeze

CALENDARS.each do |profile, calendar|
  range = calendar.class::SUPPORTED_DATE_RANGE
  puts(JSON.generate(type: 'range', profile:, first: range.begin.to_s, last: range.end.to_s))

  (range.begin.year..range.end.year).each do |year|
    dates = Date.new(year, 1, 1)..Date.new(year, 12, 31)
    mask = dates.map { |date| calendar.business_day?(date) ? '1' : '0' }
                .join
    puts(JSON.generate(type: 'mask', profile:, year:, mask:))

    dates.each do |date|
      names = calendar.holiday_names(date)
      next if names.empty?

      puts(JSON.generate(type: 'holiday', profile:, date: date.to_s, names:))
    end
  end
end
