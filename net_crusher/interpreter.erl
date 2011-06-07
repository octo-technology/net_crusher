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
-module(interpreter).

-export([
  run_commands/1
  ]).

run_commands({commands, L}) ->
   run_commands(L);
run_commands([H | T]) ->
  run_command(H),
  run_commands(T);
run_commands([]) ->
  ok.

run_command({Line, Module, Function, Params}) ->
  try
    erlang:apply(Module, Function, lists:map(fun process_params/1, Params))
  catch
    {line, Line2, E} -> throw({line, Line2, {E, erlang:get_stacktrace()}});
    _:Term -> throw({line, Line, {Term, erlang:get_stacktrace()}})
  end.

exec_function({integer, Command}) -> {integer, run_command(Command)};
exec_function({string, Command}) -> {string, [{string, run_command(Command)}]};
exec_function({bool, Command}) -> {bool, run_command(Command)};
exec_function({map, Command}) -> {map, run_command(Command)}.

process_params({commands, P}) -> fun() -> run_commands({commands, P}) end;
process_params({string, P}) -> get_string(P);
process_params({integer, P}) -> get_integer(P);
process_params({bool, P}) -> get_bool(P);
process_params({evaluable_bool, P}) -> fun() -> get_bool(P) end;
process_params({map, P}) -> get_map(P);
process_params({array, P}) -> get_array({array, P}).

get_bool({bool, B}) when is_boolean(B) -> B;
get_bool({function, Command}) -> get_bool(exec_function(Command)).

get_string({string, [{string, Str} | T]}) -> Str ++ get_string({string, T});
get_string([{string, Str} | T]) -> Str ++ get_string({string, T});
get_string({string, [{interpreted_string, Str} | T]}) -> get_string(Str) ++ get_string({string, T});
get_string({string, []}) -> "";
get_string({array, A}) -> get_array(A);
get_string({integer, Int}) -> integer_to_list(get_integer({integer, Int}));
get_string({bool, B}) when B -> "true";
get_string({bool, B}) when not B -> "false";
get_string({function, Command}) -> get_string(exec_function(Command));
get_string(X) when is_list(X) -> X;
get_string(X) when is_integer(X) -> integer_to_list(X).

get_integer(X) when is_integer(X) -> X;
get_integer({string, String}) ->
  S = get_string({string, String}),
  case catch list_to_integer(S) of
    {'EXIT', _} -> throw({cannot_be_converted_to_integer, S});
    V -> V
  end;
get_integer({integer, Int}) -> Int;
get_integer({function, Command}) -> get_integer(exec_function(Command)).

get_map({map, []}) -> [];
get_map({map, [{A, B} | T]}) -> [{get_string(A), get_string(B)} | get_map({map, T})];
get_map({function, Command}) -> get_map(exec_function(Command)).

get_array({array, A}) -> get_array(A);
get_array([]) -> [];
get_array([H | T]) -> [process_params(H) | get_array(T)];
get_array(E) -> [process_params(E)].
