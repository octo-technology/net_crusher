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
-module(assert).

-export([
  assert_monitor/1,

  cmd_assert_equal/2,
  cmd_assert_contains/2,
  cmd_assert_matches/2,
  cmd_assert_equal_one_of/2,
  cmd_assert_greater/2
]).

cmd_assert_equal(Str1, Str2) when Str1 == Str2 ->
  global:whereis_name(process_assert_monitor) ! ok;
cmd_assert_equal(Str1, Str2) ->
  throw({assert_error, equals, [{expected, Str1}, {actual, Str2}]}).

assert_equal_one_of([], _) ->
  throw({assert_error});
assert_equal_one_of([H | T], Str2) ->
  case catch cmd_assert_equal(H, Str2) of
    {assert_error, equals, _} -> assert_equal_one_of(T, Str2);
    {assert_error} -> throw({assert_error});
    _ -> noop
  end.

cmd_assert_equal_one_of(ArrStr1, Str2) ->
  case catch assert_equal_one_of(ArrStr1, Str2) of
    {assert_error} -> throw({assert_error, equals, [{expected_one_of, ArrStr1}, {actual, Str2}]});
    _ -> noop
  end.

cmd_assert_contains(StrHaystack, StrNeedle) ->
  case string:str(StrHaystack, StrNeedle) of
    0 -> throw({assert_error, contains, [{text, StrNeedle}, {text, StrHaystack}]});
    _ -> global:whereis_name(process_assert_monitor) ! ok
  end.

cmd_assert_matches(Str, StrRegexp) ->
  case re:match(Str, StrRegexp) of
    {match, _, _} -> global:whereis_name(process_assert_monitor) ! ok;
    nomatch -> throw({assert_error, matches, [{regexp, StrRegexp}, {text, Str}]})
  end.

cmd_assert_greater(Str1, Str2) when is_list(Str1) ->
  V1 = case catch(list_to_float(Str1)) of
    {'EXIT', _} -> list_to_integer(Str1);
    VV1 -> VV1
  end,
  V2 = case catch(list_to_float(Str2)) of
    {'EXIT', _} -> list_to_integer(Str2);
    VV2 -> VV2
  end,
  case V2 >= V1 of
    true -> global:whereis_name(process_assert_monitor) ! ok;
    false -> throw({assert_error, greater, [{min, Str1}, {current, Str2}]})
  end.

assert_monitor(K) ->
  receive
    halt -> logger:cmd_log(0, "Assert ok " ++ integer_to_list(K));
    ok -> assert_monitor(K + 1)
  end.
