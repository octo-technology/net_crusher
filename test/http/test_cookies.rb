
base_url = "http://localhost:#{port}"

log 0, "Base url : #{base_url}"

http_get "#{base_url}/start_session/myKey/myValue"
assert_last_http_response_code_body 200, "42"

http_get "#{base_url}/get_session/myKey"
assert_last_http_response_code_body 200, "myValue"
