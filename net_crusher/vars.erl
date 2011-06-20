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
-module(vars).

-export([
  is_defined/1,
  g_or_else/2,
  g/1,
  s/2,
  gg/2,
  gg_or_else/3,
  sg/3,
  set_map_in_session/1,

  get_name/0,
  set_name/1
]).

g_or_else(StrKey, StrDefaultValue) ->
  case get(StrKey) of
    undefined -> StrDefaultValue;
    V -> V
  end.

g(StrKey) ->
  case get(StrKey) of
    undefined -> throw({no_variable_defined, StrKey});
    V -> V
  end.

gg(StrTable, StrKey) ->
  case ets:lookup_element(list_to_atom(StrTable), StrKey, 2) of
    undefined -> throw({no_variable_defined, StrKey});
    V -> V
  end.

gg_or_else(StrTable, StrKey, StrDefaultValue) ->
  case ets:info(list_to_atom(StrTable)) of
    undefined -> StrDefaultValue;
    _ -> case ets:member(list_to_atom(StrTable), StrKey) of
      true -> case ets:lookup_element(list_to_atom(StrTable), StrKey, 2) of
        undefined -> StrDefaultValue;
        V -> V
      end;
      false -> StrDefaultValue
    end
  end.

sg(StrTable, StrKey, StrVal) ->
  case ets:info(list_to_atom(StrTable)) of
    undefined -> ets:new(list_to_atom(StrTable), [named_table, public]);
    _ -> noop
  end,
  ets:insert(list_to_atom(StrTable), {StrKey, StrVal}).

is_defined(StrKey) ->
  get(StrKey) /= undefined.

s(StrKey, StrVal) ->
  case StrVal of
    "" -> erase(StrKey);
    _ -> put(StrKey, StrVal)
  end.

get_name() ->
  g(<<"name">>).

set_name(StrName) ->
  s(<<"name">>, StrName).

set_map_in_session([{Key, Value} | T]) -> vars:s(Key, Value), set_map_in_session(T);
set_map_in_session([]) -> ok.
