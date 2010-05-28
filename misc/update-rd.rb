#!/usr/bin/env ruby
#
# Copyright (C) 2008-2010  Kouhei Sutou <kou@cozmixng.org>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

$KCODE = "utf8"

require 'nkf'
require 'rwiki/soap/driver'

def convert(src)
  NKF.nkf("-w", src)
end

def update_source(driver, src, name, page_name, log=nil)
  if src == driver.src(page_name)
    puts "#{name} doesn't need update"
    return
  end
  if log.nil?
    ENV["LANG"] = "C"
    if /Last Changed Rev: (\d+)/ =~ `svn info #{name}`
      log = "update to r#{$1}"
    end
  end
  driver.submit(page_name, src, nil, log)
  puts "committed #{name}"
end

def update_rd(driver, name, page_name=nil, prefix=nil)
  page_name ||= name
  prefix ||= "run-test.el::"
  src = convert(File.read(name))
  src = yield(src, page_name, prefix) if block_given?
  page_name = "#{prefix}#{page_name}"
  update_source(driver, src, name, page_name)
end

def update_index(driver, prev_version, current_version)
  page_name = "run-test.el"
  src = driver.src(page_name)
  src.gsub!(/#{prev_version}/, current_version)
  log = "#{prev_version} -> #{current_version}"
  update_source(driver, src, "index", page_name, log)
end

if ARGV.size < 2
  puts "Usage: #{$0} prev_version current_version"
  exit
end

prev_version, current_version = ARGV

end_point = "http://www.cozmixng.org/~rwiki/rw-soap.rb"
driver = RWiki::SOAP::Driver.new(end_point)

%w(ja en).each do |lang|
  %w(README NEWS Tutorial).each do |target|
    rd = "#{target}.#{lang}"
    update_rd(driver, rd) if File.exist?(rd)
  end
end


update_index(driver, prev_version, current_version)
