
base_url = "http://localhost:#{port}"

log 0, "Base url : #{base_url}"

log 0, "Simple"

http_get "#{base_url}/hello"
assert_last_http_response_code_body 200, "42"
assert_equal 2, last_http_response_size

http_get "#{base_url}/hello2"
assert_last_http_response_code 404

log 0, "HTTP headers"

http_get "#{base_url}/headers/salut"
assert_last_http_response_code_body 418, "42"
assert_equal "salut", last_http_response_header("x-toto")
assert_equal 418, last_http_code

http_get "#{base_url}/read_header/host"
assert_last_http_response_code_body 415, "localhost:#{port}"

add_http_header_on_next_request "Titi", "myValue"
http_get "#{base_url}/read_header/titi"
assert_last_http_response_code_body 415, "myValue"

add_http_header_on_next_request "Host", "myCustomHost"
http_get "#{base_url}/read_header/host"
assert_last_http_response_code_body 415, "myCustomHost"

log 0, "Get with last modified"

http_get_with_last_modified "#{base_url}/last_modified"
assert_last_http_response_code_body 200, "42"
last_modified = last_http_response_header("last-modified")
etag = last_http_response_header("etag")

http_get_with_last_modified "#{base_url}/last_modified"
assert_last_http_response_code_body 200, "42"
assert_equal last_modified, last_http_response_header("if-modified-since")
assert_equal etag, last_http_response_header("if-none-match")

log 0, "Redirect"

http_get "#{base_url}/redirect"
assert_last_http_response_code 302
assert_http_redirect "#{base_url}/end_redirect"
follow_redirect
assert_equal "#{base_url}/end_redirect", last_http_url
assert_last_http_response_code_body 200, "out_of_redirect"

log 0, "Basic auth"

http_get "#{base_url}/protected"
assert_last_http_response_code 401
set_basic_auth "#{base_url}/protected", "titi", "toto"
http_get "#{base_url}/protected"
assert_last_http_response_code_body 200, "Basic dGl0aTp0b3Rv"
