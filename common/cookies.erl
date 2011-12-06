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
-module(cookies).

-export([
  parse/2,
  generate_headers/3
  ]).

parse([], _) -> unable_to_parse;
parse(Str, DefaultDomain) ->
  logger:cmd_log(7, "Parsing " ++ Str),
  Cookie = parse_string(Str),
  [{Key, Value} | _ ] = Cookie,
  Domain = case lists:keyfind("domain", 1, Cookie) of
    {"domain", D} -> D;
    false -> DefaultDomain
  end,
  Expires = case lists:keyfind("expires", 1, Cookie) of
    {"expires", E} -> E;
    false -> no_expiration
  end,
  Path = case lists:keyfind("path", 1, Cookie) of
    {"path", P} -> P;
    false -> "/"
  end,
  {Domain, Path, Expires, Key, Value}.

parse_string(Str) ->
  case re:run(Str, " *([^ =]+)=([^;]+)", [global]) of
    {match, Data} ->
      lists:foldr(
      fun([{_, _}, {A1, A2}, {B1, B2}], Acc) ->
        Key = string:substr(Str, A1 + 1, A2),
        Value = string:substr(Str, B1 + 1, B2),
        [{Key, Value} | Acc]
      end,
      [],
      Data);
    nomatch -> []
  end.

generate_headers(ServerAddr, Path, Cookies) ->
  CookiesValue = case Cookies of
    undefined -> "";
    _ ->
      dict:fold(
      fun({CookieDomain, CookiePath, Key}, Value, Acc) ->
        logger:cmd_log(6, "Processing cookie " ++ CookieDomain ++ " " ++ CookiePath ++ " : " ++ Key),
        case string:rstr(ServerAddr, CookieDomain) == length(ServerAddr) - length(CookieDomain) + 1 of
          true ->
            case string:str(Path, CookiePath) == 1 of
              true -> Acc ++ Key ++ "=" ++ Value ++ ";";
              false -> Acc
            end;
          false -> Acc
        end
      end,
      "",
      Cookies)
  end,
  case CookiesValue of
    "" ->
      logger:cmd_log(6, "Add cookies headers for " ++ ServerAddr ++ " " ++ Path ++ " : " ++ CookiesValue),
      [];
    V -> [{"Cookie", V}]
  end.
