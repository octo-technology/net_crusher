
base_url = "http://localhost:#{port}"

log 0, "Base url : #{base_url}"

http_get "#{base_url}/hello"
assert_last_http_response_code_body 200, "42"

http_get "#{base_url}/hello2"
assert_last_http_response_code 404

http_get "#{base_url}/headers/salut"
assert_last_http_response_code_body 418, "42"
assert_equal "salut", last_http_response_header("x-toto")
assert_equal 418, last_http_code

http_get "#{base_url}/read_header/host"
assert_last_http_response_code_body 415, "localhost:#{port}"

# http_get "#{base_url}/read_header/titi", {"Titi" => "myValue"}
# assert_last_http_response_code_body 415, "myValue"

http_get_with_last_modified "#{base_url}/last_modified"
assert_last_http_response_code_body 200, "42"
last_modified = last_http_response_header("last-modified")
etag = last_http_response_header("etag")

http_get_with_last_modified "#{base_url}/last_modified"
assert_last_http_response_code_body 200, "42"
assert_equal last_modified, last_http_response_header("if-modified-since")
# assert_equal etag, last_http_response_header("if-none-match")