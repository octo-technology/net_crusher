
base_url = "http://localhost:#{port}"

log 0, "Base url : #{base_url}"

log 0, "Post form"

my_var = 2

http_post_form "#{base_url}/post_form", {"p1" => "myP1Value", "p2" => my_var}
assert_last_http_response_code_body 200, "myP1Value_2"

http_post_form "#{base_url}/post_form_map", {"user" => {"a" => "my_a", "b" => "my_b"}, "titi" => "toto"}
assert_last_http_response_code_body 200, "my_a_my_b_toto"

http_post_json "#{base_url}/post_form", "toto"
assert_last_http_response_code 404

log 0, "Post json with form"

http_post_form "#{base_url}/post_json", {}
assert_last_http_response_code 404

log 0, "Post json"

http_post_json "#{base_url}/post_json", "{\"a\" :\"1\", \"b\" : \"toto\"}"
assert_last_http_response_code 200 
assert_last_http_response_body_matches "{\"a\":\"newValueForA\",\"b\":\"toto\"}"

a = "titi"
assert_equal "titi", a

cmd_http_parse_json
assert_equal "newValueForA", a
assert_equal "toto", b