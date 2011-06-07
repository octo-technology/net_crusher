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
  cmd_sleep_ms/1,
  str_md5/1,
  int_random/2,
  str_extract_last_number/1,
  cmd_inject_argv/0,
  cmd_load_yml/1,
  str_extract_with_regex/2,
  str_extract_with_regex_or_else/3
]).

cmd_sleep_ms(IntMs) ->
  receive
    after IntMs -> noop
  end.

str_md5(StrStr) ->
  tools:md5_hex(StrStr).

int_random(IntStart, IntStop) ->
  IntStart + random:uniform(IntStop - IntStart).

set_map_in_session([{Key, Value} | T]) -> put(Key, Value), set_map_in_session(T);
set_map_in_session([]) -> ok.

cmd_load_yml(StrFileName) ->
  logger:cmd_log(2, "Load yml file " ++ StrFileName),
  set_map_in_session(yml_loader:load(StrFileName)),
  ok.

cmd_inject_argv() ->
  {ok, Re} = re:compile("(.+)=(.+)"),
  lists:map(fun(S) ->
    case re:run(S, Re) of
      nomatch -> throw({wrong_arg, S});
      {match, [{_, _}, {A1, A2}, {B1, B2}]} ->
        Key = string:substr(S, A1 + 1, A2),
        Value = string:substr(S, B1 + 1, B2),
        logger:cmd_log(1, "Setting from argv " ++ Key ++ " : " ++ Value),
        vars:cmd_s(Key, Value)
    end
  end, get(argv)).

str_extract_with_regex(StrContent, StrRegex) ->
  case re:run(StrContent, StrRegex) of
    nomatch -> throw({regex_not_found, StrRegex, StrContent});
    {match, [{_, _}, {A1, A2}]} -> string:substr(StrContent, A1 + 1, A2)
  end.

str_extract_with_regex_or_else(StrContent, StrRegex, StrElse) ->
  case re:run(StrContent, StrRegex) of
    nomatch -> StrElse;
    {match, [{_, _}, {A1, A2}]} -> string:substr(StrContent, A1 + 1, A2)
  end.

str_extract_last_number(StrK) ->
  str_extract_with_regex(StrK, "(\\d+)[^\\d]*$").
