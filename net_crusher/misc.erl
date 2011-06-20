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
-module(misc).

-export([
  sleep_ms/1,
  md5/1,
  random/2,
  extract_last_number/1,
  inject_argv/0,
  extract_with_regex/2,
  extract_with_regex_or_else/3
]).

sleep_ms(IntMs) ->
  receive
    after IntMs -> noop
  end.

md5(StrStr) ->
  tools:md5_hex(StrStr).

random(IntStart, IntStop) ->
  IntStart + random:uniform(IntStop - IntStart).

inject_argv() ->
  {ok, Re} = re:compile("(.+)=(.+)"),
  lists:map(fun(S) ->
    case re:run(S, Re) of
      nomatch -> logger:logf(1, "Ignoring argument ~p", [S]);
      {match, [{_, _}, {A1, A2}, {B1, B2}]} ->
        Key = string:substr(S, A1 + 1, A2),
        Value = string:substr(S, B1 + 1, B2),
        logger:log(1, "Setting from argv " ++ Key ++ " : " ++ Value),
        vars:s(Key, Value)
    end
  end, init:get_plain_arguments()).

extract_with_regex(StrContent, StrRegex) ->
  case re:run(StrContent, StrRegex) of
    nomatch -> throw({regex_not_found, StrRegex, StrContent});
    {match, [{_, _}, {A1, A2}]} -> string:substr(StrContent, A1 + 1, A2)
  end.

extract_with_regex_or_else(StrContent, StrRegex, StrElse) ->
  case re:run(StrContent, StrRegex) of
    nomatch -> StrElse;
    {match, [{_, _}, {A1, A2}]} -> string:substr(StrContent, A1 + 1, A2)
  end.

extract_last_number(StrK) ->
  extract_with_regex(StrK, "(\\d+)[^\\d]*$").
