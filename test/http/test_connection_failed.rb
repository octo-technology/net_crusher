
base_url = "http://localhost:#{unused_port}"

catch_http_network_error_into_zero true

log 0, "Test connection failed on get on port #{unused_port}"
http_get "#{base_url}/hello"
assert_last_http_response_code 0

log 0, "Test connection failed on post form on port #{unused_port}"
http_post_form "#{base_url}/post_form", {"p1" => "myP1Value", "p2" => "myP2Value"}
assert_last_http_response_code 0

log 0, "Test connection failed on post json on port #{unused_port}"
http_post_json "#{base_url}/post_json", "{\"a\" :\"1\", \"b\" : \"toto\"}"
assert_last_http_response_code 0
