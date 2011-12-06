require 'rubygems'
require 'sinatra'

get '/hello' do
  "42"
end

get '/headers/:value' do
  status 418
  headers "X-Toto" => params[:value]
  body "42"
end

get '/read_header/:name' do
  status 415
  body env["HTTP_#{params[:name].upcase}"]
end

get '/last_modified' do
  headers "Last-Modified" => Time.now.to_s
  headers "Etag" => "6789"
  headers "If-Modified-Since" => env["HTTP_IF_MODIFIED_SINCE"] if env["HTTP_IF_MODIFIED_SINCE"]
  headers "If-None-Match" => env["HTTP_IF_NONE_MATCH"] if env["HTTP_IF_NONE_MATCH"]
  body "42"
end

get '/redirect' do
  redirect '/end_redirect'
end

get '/end_redirect' do
  "out_of_redirect"
end

get '/protected' do
  p env
  if env["HTTP_AUTHORIZATION"]
    body env["HTTP_AUTHORIZATION"]
  else
    status 401
  end
end