## NetCrusher.
## Copyright (C) 2011 Bertrand Paquet, David Rousselie All Rights Reserved

## NetCrusher is free software; you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public
## License as published by the Free Software Foundation; either
## version 2.1 of the License, or (at your option) any later version.

## NetCrusher is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## Lesser General Public License for more details.

## You should have received a copy of the GNU Lesser General Public
## License along with this library; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
require 'net/http'
require 'uri'
require 'yaml'
require 'rubygems'
require 'json'
require 'digest/md5'

class NetCrusher

  def set_name (name)
    Thread.current[:name] = name
  end

  def get_name
    Thread.current[:name]
  end

  def log (level, msg)
    if level <= @log_level then
      puts "[#{Thread.current[:name]}] #{msg}"
    end
  end

  def logf (level, format, args)
    if level <= @log_level then
      printf "[#{Thread.current[:name]}] #{format.gsub('~', '%')}\n", *args
    end
  end

  def process_authent (request)
    login = Thread.current[:map]["basic_auth_login"]
    password = Thread.current[:map]["basic_auth_password"]
    if !login.nil? && !password.nil? then
      request.basic_auth login, password
    end
  end

  def process_set_cookie (response)
    if ! response['set-cookie'].nil? then
      found = response['set-cookie'].scan(/(\S+_session=\S+;)/)
      s 'cookie', found[0].first unless found.empty?
    end
  end

  def set_cookie (request)
    if is_defined('cookie') then
      cookie = g 'cookie'
      #log 2, "Use cookie #{cookie}"
      request['cookie'] = cookie
    end
  end

  def http_post_form (url, data)
    uri = URI.parse url
    path = uri.path
    if ! uri.query.nil?
      path = path + "?" + uri.query
    end
    request = Net::HTTP::Post.new(path)
    request.set_form_data data
    send_req uri, request
  end

  def answer (url, data)
    http_post_json url, data
  end

  def http_post_json (url, data)
    uri = URI.parse url
    request = Net::HTTP::Post.new(uri.path)
    request.body = data
    request['Content-Type'] = 'application/json'
    send_req uri, request
  end

  def http_get_with_last_modified (url)
    uri = URI.parse url
    path = uri.path
    if ! uri.query.nil?
      path = path + "?" + uri.query
    end
    request = Net::HTTP::Get.new(path)
    if Thread.current[:map].key?(:last_modified) && Thread.current[:map][:last_modified].key?(url)
      last_modified = Thread.current[:map][:last_modified][url]
      request['If-Modified-Since'] = last_modified
      request['Etag'] = 0
    end
    response = send_req uri, request, 300
    if response.key?('last-modified')
      Thread.current[:map][:last_modified] = {} unless Thread.current[:map].key?(:last_modified)
      Thread.current[:map][:last_modified][url] = response['last-modified']
    end
    response
  end

  def http_get (url)
    uri = URI.parse url
    path = uri.path
    if ! uri.query.nil?
      path = path + "?" + uri.query
    end
    request = Net::HTTP::Get.new(path)
    send_req uri, request
  end

  def send_req (uri, request, timeout=60)
    set_cookie request
    process_authent request
    http = Net::HTTP.new(uri.host, uri.port)
    http.read_timeout = timeout
    response = http.start {|http| http.request(request) }
    process_set_cookie response
    Thread.current[:last_http_response] = response
    response
  end

  def loop_on_http_timeout(&block)
    log 3, "Start loop on timeout"
    while true do
        begin
          block.call
          log 3, "Loop on timeout"
          break
        rescue Timeout::Error
          log 3, "Http timeout, looping"
        end
      end
    end

    def are_assert_enabled
      Thread.current[:map]["no_assert"] != true
    end

    def assert_equal (expected, actual)
      if are_assert_enabled then
        log 3, "Check #{expected} == #{actual}"
        if expected.to_s != actual.to_s then
          raise "[#{get_name}] Assert error : <#{expected}> : <#{actual}>"
        end
        @assert_ok = @assert_ok + 1
      end
    end

    def assert_greater (min, actual)
      if are_assert_enabled then
        log 3, "Check #{actual} > #{min}"
        if actual.to_f < min.to_f then
          raise "[#{get_name}] Assert error : #{actual.to_f} < #{min.to_f}"
        end
      end
    end

    def assert_contains (haystack, needle)
      if are_assert_enabled then
        if haystack.index(needle).nil? then
          raise "[#{get_name}] Assert error : not found <#{needle}> in <#{haystack}>"
        end
        @assert_ok = @assert_ok + 1
      end
    end

    def assert_last_http_response_code (code)
      assert_equal code.to_s, last_http_response.code
    end

    def assert_last_http_response_code_body (code, body)
      assert_last_http_response_code code
      assert_equal body, last_http_response.body
    end

    def assert_last_http_response_body_contains (string)
      assert_contains last_http_response.body, string
    end

    def assert_http_redirect (location)
      assert_last_http_response_code 302
      assert_equal location, last_http_response['location']
    end

    def last_http_response
      Thread.current[:last_http_response]
    end

    def last_http_response_body
      Thread.current[:last_http_response].body
    end

    def last_http_response_header (header_name)
      last_http_response[header_name]
    end

    def extract_last_number (str)
      extract_with_regex(str, "(\\d+)$")
    end

    def extract_with_regex (string, regex)
      string.scan(Regexp.new(regex))[0].first
    end

    def http_parse_json
      unless ['text/json', 'application/json'].include? last_http_response.content_type
        puts "Asking for JSON parsing, but the content type is #{last_http_response.content_type}"
        puts "And the body is #{last_http_response.body}"
        return
      end
      result = JSON.parse last_http_response.body
      result.each do |k, v|
        s k, v.to_s
      end
    end

    def fork (name, filename)
      current_map = Thread.current[:map]
      thread = Thread.new do
        Thread.current[:map] = current_map.clone
        set_name name
        run_file filename
      end
      thread.abort_on_exception = true
      thread.run
      Thread.current[:sub_thread].push thread
    end

    def load_yml (filename)
      log 2, "Load yml file #{filename}"
      data = open(filename) {|f| YAML.load(f) }
      data.each do |k, v|
        Thread.current[:map][k] = v
      end
    end

    def is_defined(key)
      Thread.current[:map].key?(key)
    end

    def g (key)
      value = Thread.current[:map][key]
      raise "Not defined #{key}" if value.nil?
      value
    end

    def s (key, value)
      Thread.current[:map][key] = value
    end

    def gg (table, key)
      g(key)
    end

    def sg(table, key, value)
      s(key, value)
    end

    def run_file (filename)
      Thread.current[:sub_thread] = []
      log 1, "Loading file #{filename}"
      instance_eval File.read(filename), filename
      if ! Thread.current[:event_log].nil? then
        Thread.current[:event_log].close
      end
      Thread.current[:sub_thread].each do |thread|
        log 3, "Waiting for termination #{thread}"
        thread.join
      end
      log 2, 'End of thread'
    end

    def execute (filename)
      log 3, "Execute file #{filename}"
      instance_eval File.read(filename), filename
      log 3, "End of executing #{filename}"
    end

    def set_log_level log_level
      log 0, "Using log_level #{log_level}"
      @log_level = log_level
    end

    def run
      @log_level = 0
      @global_timestamp = {}
      set_name 'master'
      Thread.current[:map] = {}
      puts "Starting test"
      @assert_ok = 0
      (0..(ARGV.length - 1)).each do |k|
        puts "Running #{ARGV[k]}"
        run_file ARGV[k]
      end
      puts "Test ok, #{@assert_ok} asserts"
    end

    def loop (start, stop, &block)
      (start .. stop).each do |k|
        s 'k', k
        block.call
      end
    end

    def sleep_ms (ms)
      sleep ms / 1000
    end

    def internal_log_event(now, length, eventName)
      if ! Thread.current[:map]['event_log_prefix'].nil? then
        File.open(Thread.current[:map]['event_log_prefix'] + Thread.current[:name] + ".log", "a") do |f|
          f.write("#{now.to_f} #{length.to_f} #{eventName}\n")
        end
      end
    end

    def put_timestamp(label)
      @global_timestamp[label] = Time.now
    end

    def chrono_delta(label, eventName)
      timestamp = @global_timestamp.has_key?(label) ? @global_timestamp[label] : Time.now
      internal_log_event(Time.now, Time.now - timestamp, eventName)
    end

    def chrono(eventName, &block)
      start = Time.now
      block.call
      stop = Time.now
      internal_log_event(start, stop - start, eventName)
    end

    def inject_argv
    end

    def http_load_static_resources
    end

    def http_display_static_resources
    end

    def call module_name, method_name
      send(method_name.to_sym)
    end

    def random (min, max)
      min + rand(max - min)
    end

    def md5 (s)
      Digest::MD5.hexdigest(s)
    end

    def read_file(filename)
      File.read(filename)
    end

  end

  NetCrusher.new.run
