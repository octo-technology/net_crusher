require 'rubygems'
require 'sinatra'
require 'json'

enable :sessions

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

get '/start_session/:key/:value' do
  session[params[:key]] = params[:value]
  body "42"
end

get '/get_session/:name' do
  body session[params[:name]]
end

get '/protected' do
  p env
  if env["HTTP_AUTHORIZATION"]
    body env["HTTP_AUTHORIZATION"]
  else
    status 401
  end
end

post '/post_form' do
  if env['CONTENT_TYPE'] == 'application/x-www-form-urlencoded'
    body params[:p1] + "_" + params[:p2]
  else
    status 404
  end
end

post '/post_json' do
  if env['CONTENT_TYPE'] == 'application/json'
    data = JSON.parse request.body.read
    data['a'] = 'newValueForA'
    body JSON.dump(data)
  else
    status 404
  end
end