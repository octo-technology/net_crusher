http_post_form "#{url_base}/#{url_authcanal}", {"User" => {"username" => "dfsdgdsfgdfs@gmail.com", "password" => "motdepasse"}}
assert_last_http_response_code 200