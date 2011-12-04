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
-module(http_static).

-export([
cmd_http_load_static_resources/0,
cmd_http_display_static_resources/0
]).

cmd_http_display_static_resources() ->
  logger:cmd_log(5, "Current static resources"),
  {A, B, C} = dict:fold(fun(Url, V, {K, Total, Error}) ->
    case V of
      {ok, Size} -> logger:cmd_log(5, "Url : " ++ Url ++ ", size : " ++ integer_to_list(Size)), {K + 1, Total + Size, Error};
      {error, Code} -> logger:cmd_log(5, "Error on url : " ++ Url ++ ", code : " ++ integer_to_list(Code)), {K, Total, Error + 1}
    end
  end, {0, 0, 0}, get_map()),
  logger:cmd_log(5, "End of static resources"),
  logger:cmd_log(4, "Total ok " ++ integer_to_list(A) ++ ", size " ++ integer_to_list(B)),
  logger:cmd_log(4, "Total error " ++ integer_to_list(C)).

filter_comment(S) ->
  case string:str(S, "/*") of
    0 -> S;
    K ->
      Before = string:substr(S, 1, K - 1),
      After = string:substr(S, K + 1),
      RealAfter = case string:str(After, "*/") of
        0 -> throw(css_error);
        KK -> string:substr(After, KK + 2)
      end,
      Before ++ filter_comment(RealAfter)
  end.

cmd_http_load_static_resources() ->
  case get("load_static_resources") of
    "true" ->
      ContentType = extract_type(http_cmd:str_last_http_response_header("content-type")),
      {ok, _, _, _, Body} = get(last_http_response),
      logger:cmd_logf(2, "Loading static resources for ~s", [http_cmd:str_last_http_url()]),
      load_static_resources(ContentType, Body, http_cmd:str_last_http_url());
    _ -> noop
  end.

add_base_url_if_needed(_, []) ->
  [];
add_base_url_if_needed(FromUrl, Urls) ->
  {ok, Protocol, ServerAddr, ServerPort, Query} = tools_http:parse_url(FromUrl),
  BaseUrl = tools_http:make_url(Protocol, ServerAddr, ServerPort),
  LastFolderUrl = BaseUrl ++ string:substr(Query, 1, string:rstr(Query, "/")),
  lists:map(fun(Url) ->
    case re:run(Url, "http:\\/\\/") of
      {match, _} -> Url;
      nomatch ->
        case string:left(Url, 1) of
          "/" -> BaseUrl ++ Url;
          _ -> LastFolderUrl ++ Url
        end
    end
    end, Urls).

load_static_resources(ContentType, Body, Url) ->
  logger:cmd_log(6, "Process " ++ Url ++ ", Content-Type " ++ ContentType),
  Urls = case ContentType of
    "text/html" ->
      BodyStr = binary_to_list(Body),
      add_base_url_if_needed(Url, lists:flatmap(fun(Ext) -> find_link_in_html(BodyStr, Ext) end, ["css", "jpg", "png", "js", "gif", "jpeg", "swf"]));
    "text/css" ->
      BodyStr = filter_comment(binary_to_list(Body)),
      add_base_url_if_needed(Url, lists:flatmap(fun(Ext) -> find_link_in_css(BodyStr, Ext) end, ["jpg", "png", "jpg", "gif", "jpeg"]));
    "image/jpeg" -> [];
    "image/png" -> [];
    "image/gif" -> [];
    "application/javascript" -> [];
    "text/javascript" -> [];
    "application/x-shockwave-flash" -> [];
    "application/x-javascript" -> []
  end,
  %io:fwrite("Resources ~p, for ~s\n", [Urls, Url]),
  lists:map(fun(U) -> download_sub_resources(U) end, Urls).

download_sub_resources(FullUrl) ->
  case has_been_downloaded(FullUrl) of
    true -> noop;
    false ->
      logger:cmd_log(5, "Download " ++ FullUrl),
      http_cmd:cmd_http_get(FullUrl),
      {ok, Code, _, _, Body} = get(last_http_response),
      case Code of
        200 ->
          ContentType = extract_type(http_cmd:str_last_http_response_header("content-type")),
          add_downloaded(FullUrl, {ok, byte_size(Body)}),
          load_static_resources(ContentType, Body, FullUrl);
        _ ->
          logger:cmd_log(5, "Error while getting " ++ FullUrl ++ " : " ++ integer_to_list(Code)),
          add_downloaded(FullUrl, {error, Code})
      end
  end.

extract_type(ContentType) ->
  case string:str(ContentType, ";") of
    0 -> ContentType;
    K -> string:substr(ContentType, 1, K - 1)
  end.

has_been_downloaded(Url) ->
  dict:is_key(Url, get_map()).

get_map() ->
  case get(resource_downloaded) of
    undefined -> dict:new();
    V -> V
  end.

add_downloaded(Url, Result) ->
  put(resource_downloaded, dict:store(Url, Result, get_map())).

find_link_in_html(S, Ext) ->
  case re:run(S, "href=[^\\/A-Za-z0-9-_]?([\\/A-Za-z0-9-_:\\.]+\\." ++ Ext ++ "(\\?[\\/A-Za-z0-9-_=&%]*)?)") of
    {match, [{_, _}, {A1, A2} | _]} -> [string:substr(S, A1 +1 , A2) | find_link_in_html(string:substr(S, A1 + A2 + 1), Ext)];
    nomatch -> []
  end.

find_link_in_css(S, Ext) ->
  case re:run(S, "url\\([^\\/A-Za-z0-9-_]?([\\/A-Za-z0-9-_:\\.]+\\." ++ Ext ++ "(\\?[\\/A-Za-z0-9-_=&%]*)?)\\)") of
    {match, [{_, _}, {A1, A2} | _]} -> [string:substr(S, A1 +1 , A2) | find_link_in_css(string:substr(S, A1 + A2 + 1), Ext)];
    nomatch -> []
  end.
