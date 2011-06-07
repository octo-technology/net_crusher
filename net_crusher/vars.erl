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
  bool_is_defined/1,
  str_g_or_else/2,
  str_g/1,
  cmd_s/2,
  str_gg/2,
  str_gg_or_else/3,
  cmd_sg/3,

  str_get_name/0,
  cmd_set_name/1
]).

str_g_or_else(StrKey, StrDefaultValue) ->
  case get(StrKey) of
    undefined -> StrDefaultValue;
    V -> V
  end.

str_g(StrKey) ->
  case get(StrKey) of
    undefined -> throw({no_variable_defined, StrKey});
    V -> V
  end.

str_gg(StrTable, StrKey) ->
  case ets:lookup_element(list_to_atom(StrTable), StrKey, 2) of
    undefined -> throw({no_variable_defined, StrKey});
    V -> V
  end.

str_gg_or_else(StrTable, StrKey, StrDefaultValue) ->
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

cmd_sg(StrTable, StrKey, StrVal) ->
  case ets:info(list_to_atom(StrTable)) of
    undefined -> ets:new(list_to_atom(StrTable), [named_table, public]);
    _ -> noop
  end,
  ets:insert(list_to_atom(StrTable), {StrKey, StrVal}).

bool_is_defined(StrKey) ->
  get(StrKey) /= undefined.

cmd_s(StrKey, StrVal) ->
  case StrVal of
    "" -> erase(StrKey);
    _ -> put(StrKey, StrVal)
  end.

str_get_name() ->
  str_g("name").

cmd_set_name(StrName) ->
  cmd_s("name", StrName).
