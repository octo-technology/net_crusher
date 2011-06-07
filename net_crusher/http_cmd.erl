%% NetCrusher.
%% Copyright (C) 2011 Bertrand Paquet, David Rousselie All Rights Reserved

%% NetCrusher is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.

%% NetCrusher is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
-module(http_cmd).

-export([
  cmd_http_get_with_last_modified/1,
  cmd_http_get/1,
  cmd_http_post_form/2,
  cmd_http_post_json/2,
  cmd_assert_last_http_response_code/1,
  cmd_assert_last_http_response_code_body/2,
  cmd_assert_last_http_response_body_contains/1,
  cmd_assert_last_http_response_body_matches/1,
  cmd_assert_http_redirect/1,
  str_last_http_response_header/1,
  str_last_http_response_size/0,
  cmd_http_parse_json/0,
  str_last_http_response_body/0,
  str_last_http_url/0,
  int_last_http_code/0,
  cmd_follow_redirect/0,
  cmd_set_basic_auth/3
]).

cmd_http_get(StrUrl) ->
  cmd_http_get(StrUrl, "").

cmd_http_get(StrUrl, StrHeaders) ->
  put(last_http_url, StrUrl),
  {ok, Protocol, ServerAddr, ServerPort, Path} = tools_http:parse_url(StrUrl),
  Socket = tools_http:open_socket(Protocol, ServerAddr, ServerPort),
  logger:cmd_logf(5, "HTTP GET: ~p", [StrUrl]),
  {ok, Code, Result, Headers, Body} = tools_http:get_http(Socket, Path, StrHeaders ++ generate_headers(ServerAddr, ServerPort, Path)),
  logger:cmd_logf(5, "HTTP GET Result: ~p (~p)", [StrUrl, Code]),
  tools_http:close_socket(Socket),
  process_cookie(ServerAddr, Headers),
  put(last_http_response, {ok, Code, Result, process_headers(Headers), Body}).

cmd_http_post_form(StrUrl, MapParams) ->
  put(last_http_url, StrUrl),
  {ok, Protocol, ServerAddr, ServerPort, Path} = tools_http:parse_url(StrUrl),
  Params = encode_map(MapParams),
  Socket = tools_http:open_socket(Protocol, ServerAddr, ServerPort),
  logger:cmd_logf(5, "HTTP POST: ~p (~p)", [StrUrl, MapParams]),
  {ok, Code, Result, Headers, Body} = tools_http:post_http(Socket, Path, generate_headers(ServerAddr, ServerPort, Path, "Content-Type: application/x-www-form-urlencoded\r\n"), Params),
  logger:cmd_logf(5, "HTTP POST Result: ~p (~p)", [StrUrl, Code]),
  tools_http:close_socket(Socket),
  process_cookie(ServerAddr, Headers),
  put(last_http_response, {ok, Code, Result, process_headers(Headers), Body}).

cmd_http_post_json(StrUrl, StrJson) ->
  put(last_http_url, StrUrl),
  {ok, Protocol, ServerAddr, ServerPort, Path} = tools_http:parse_url(StrUrl),
  Socket = tools_http:open_socket(Protocol, ServerAddr, ServerPort),
  logger:cmd_logf(5, "HTTP POST (JSON): ~p : ~p", [StrUrl, StrJson]),
  {ok, Code, Result, Headers, Body} = tools_http:post_http(Socket, Path, generate_headers(ServerAddr, ServerPort, Path, "Content-Type: application/json\r\n"), StrJson),
  logger:cmd_logf(5, "HTTP POST (JSON) Result: ~p (~p)", [StrUrl, Code]),
  tools_http:close_socket(Socket),
  process_cookie(ServerAddr, Headers),
  put(last_http_response, {ok, Code, Result, process_headers(Headers), Body}).

process_headers([{K, V} | T]) when is_atom(K) -> [{string:to_lower(atom_to_list(K)), V} | process_headers(T)];
process_headers([{K, V} | T]) -> [{string:to_lower(K), V} | process_headers(T)];
process_headers([]) -> [].

set_basic_auth(ServerAddr, Auth) ->
  Map = case get(http_basic_auth) of
    undefined -> dict:new();
    M1 -> M1
  end,
  put(http_basic_auth, dict:store(ServerAddr, Auth, Map)).

get_basic_auth(ServerAddr) ->
  case get(http_basic_auth) of
    undefined -> undefined;
    Map ->
      case dict:find(ServerAddr, Map) of
        error -> undefined;
        {ok, Auth} -> Auth
      end
  end.

cmd_set_basic_auth(StrUrl, StrVarNameLogin, StrVarNamePassword) ->
  case get(StrVarNameLogin) of
    undefined -> noop;
    Login ->
      case get(StrVarNamePassword) of
        undefined -> noop;
        Password ->
          {ok, _, ServerAddr, _, _} = tools_http:parse_url(StrUrl ++ "/"),
          set_basic_auth(ServerAddr, {Login, Password})
      end
  end.

generate_headers(ServerAddr, ServerPort, Path) ->
  generate_headers(ServerAddr, ServerPort, Path, "").

generate_headers(ServerAddr, ServerPort, Path, Header) ->
  "Host: " ++ ServerAddr ++
  case ServerPort of
    80 -> "";
    _ -> ":" ++ integer_to_list(ServerPort)
  end
  ++ "\r\n" ++
  cookies:generate_headers(ServerAddr, Path, get("cookies"))
  ++
  basic_auth(ServerAddr)
  ++
  Header.

basic_auth(ServerAddr) ->
  case get_basic_auth(ServerAddr) of
    undefined -> "";
    {Login, Password} -> "Authorization: Basic " ++ binary_to_list(base64:encode(Login ++ ":" ++ Password)) ++ "\r\n"
  end.

process_cookie(ServerAddr, Headers) ->
  lists:map(
    fun(Cookie) ->
      case cookies:parse(Cookie, ServerAddr) of
        unable_to_parse -> noop;
        {Domain, Path, _, Key, Value} ->
          Map = case get("cookies") of
            undefined -> logger:cmd_log(5, "creating cookies dict"),dict:new();
            M -> M
          end,
          logger:cmd_log(5, "Set cookie : " ++ Key ++ " : " ++ Value ++ " for host " ++ ServerAddr),
          put("cookies", dict:store({Domain, Path, Key}, Value, Map))
      end
    end,
    tools_http:extract_headers(Headers, 'Set-Cookie')).

encode_map(Map) -> encode_map(Map, "").

encode_map([{A, B} | T], Acc) ->
  NewAcc = case Acc of
    "" -> "";
    _ -> Acc ++ "&"
  end
  ++
  edoc_lib:escape_uri(convert(A)) ++ "=" ++ edoc_lib:escape_uri(convert(B)),
  encode_map(T, NewAcc);
encode_map([], Acc) -> Acc.

convert(S) when is_list(S) -> S;
convert(I) when is_integer(I) -> integer_to_list(I).

str_last_http_response_header(StrName) ->
  {ok, _, _, Headers, _} = get(last_http_response),
  case tools_http:extract_header(Headers, StrName) of
    not_found -> not_found;
    {ok, V} -> V
  end.

int_last_http_code() ->
  {ok, Code, _, _, _} = get(last_http_response),
  Code.

cmd_assert_last_http_response_code(IntCode) ->
  assert:cmd_assert_equal(IntCode, int_last_http_code()).

cmd_assert_http_redirect(StrLocation) ->
  {ok, Code, _, Headers, _} = get(last_http_response),
  assert:cmd_assert_equal_one_of([302, 303], Code),
  assert:cmd_assert_equal({ok, StrLocation}, tools_http:extract_header(Headers, "location")).

cmd_follow_redirect() ->
  {ok, Code, _, Headers, _} = get(last_http_response),
  assert:cmd_assert_equal_one_of([302, 303], Code),
  {ok, Url} = tools_http:extract_header(Headers, "location"),
  logger:cmd_logf(2, "Following redirection to ~p", [Url]),
  cmd_http_get(Url).

cmd_assert_last_http_response_code_body(IntCode, StrText) ->
  {ok, _, _, _, Body} = get(last_http_response),
  cmd_assert_last_http_response_code(IntCode),
  assert:cmd_assert_equal(StrText, binary_to_list(Body)).

cmd_assert_last_http_response_body_contains(StrText)  ->
  {ok, _, _, _, Body} = get(last_http_response),
  assert:cmd_assert_contains(binary_to_list(Body), StrText).

cmd_assert_last_http_response_body_matches(StrRegexp) ->
  {ok, _, _, _, Body} = get(last_http_response),
  assert:cmd_assert_matches(binary_to_list(Body), StrRegexp).

cmd_http_parse_json() ->
  {ok, _, _, _, Body} = get(last_http_response),
  JsonToParse = binary_to_list(Body),
  logger:cmd_log(7, "Parsing JSON from last HTTP response: " ++ JsonToParse),
  case catch json_eep:json_to_term(JsonToParse) of
    {badmatch, X} -> logger:cmd_logf(0, "Error parsing JSON : ~p~n~p~n", [X, JsonToParse]);
    X -> process_json(X)
  end.

process_json(List) when is_list(List) -> lists:map(fun process_json/1, List);
process_json({_, {_}}) -> noop;
process_json({K, true}) -> put(binary_to_list(K), "true");
process_json({K, false}) -> put(binary_to_list(K), "false");
process_json({K, V}) when is_binary(V) -> put(binary_to_list(K), binary_to_list(V));
process_json({K, V}) when is_integer(V) -> put(binary_to_list(K), integer_to_list(V));
process_json({K, V}) when is_float(V) -> put(binary_to_list(K), float_to_list(V));
process_json({K, V}) when is_atom(V) -> put(binary_to_list(K), atom_to_list(V));
process_json({K, V}) when is_list(V) -> put(binary_to_list(K), V);
process_json({OtherElement}) -> process_json(OtherElement).

str_last_http_response_body() ->
  {ok, _, _, _, Body} = get(last_http_response),
  binary_to_list(Body).

str_last_http_response_size() ->
  {ok, _, _, _, Body} = get(last_http_response),
    integer_to_list(size(Body)).

get_header_value(StrUrl, HeaderName) ->
  case get(last_headers_received) of
    undefined -> not_found;
    Map ->
      HeaderMap = case dict:find(HeaderName, Map) of
        {ok, M} -> M;
        error -> not_found
      end,
      case dict:find(StrUrl, HeaderMap) of
        {ok, Value} -> Value;
        error -> not_found
      end
  end.

save_header_value(StrUrl, HeaderName) ->
  case str_last_http_response_header(HeaderName) of
      not_found -> noop;
      ToBeStored ->
        Map = case get(last_headers_received) of
          undefined -> dict:new();
          M1 -> M1
        end,
        HeaderMap = case dict:find(HeaderName, Map) of
          {ok, M2} -> M2;
          error -> dict:new()
        end,
        logger:cmd_log(5, "Saving header " ++ HeaderName ++ " for url " ++ StrUrl ++ " with value " ++ ToBeStored),
        NewHeaderMap = dict:store(StrUrl, ToBeStored, HeaderMap),
        put(last_headers_received, dict:store(HeaderName, NewHeaderMap, Map))
  end.

cmd_http_get_with_last_modified(StrUrl) ->
  Headers = lists:foldr(
    fun({HeaderToSearch, HeaderToAdd}, Acc) ->
      case get_header_value(StrUrl, HeaderToSearch) of
        not_found -> Acc;
        Value -> Acc ++ HeaderToAdd ++ ": " ++ Value ++ "\r\n"
      end
    end, "", [{"last-modified", "If-Modified-Since"}, {"etag", "If-None-Match"}]),
  logger:cmd_log(5, "Injecting headers: " ++ Headers),
  cmd_http_get(StrUrl, Headers),
  save_header_value(StrUrl, "last-modified"),
  save_header_value(StrUrl, "etag").

str_last_http_url() ->
  get(last_http_url).

