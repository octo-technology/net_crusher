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
  run_commands/1,
  get_string/1,
  get_integer/1,
  exec_function/1
  ]).

run_commands({commands, L}) ->
   run_commands(L);
run_commands([H | T]) ->
  % io:fwrite("Running line ~p~n", [H]),
  run_command(H),
  run_commands(T);
run_commands([]) ->
  ok.

run_command({ReturnType, {Line, erlang_statment, Module, Function, Params}}) ->
  % io:fwrite("Running builtin function ~p:~p on ~p ~n", [Module, Function, Params]),
  {ReturnType, run_function(Line, {erlang_function, Module, Function}, Params)};
run_command({Line, function_call, FunctionName, Params}) ->
  case lists:keyfind(FunctionName, 1, get(interpreter_funcs)) of
    false -> case length(Params) of
      0 -> try
        {string, vars:str_g(FunctionName)}
        catch
          _:Term -> throw({line, Line, {Term, stacktrace:generate()}})
        end;
      _ -> throw({line, Line, {missing_function, FunctionName, stacktrace:generate()}})
    end;
    {_, {ReturnType, F, TypeOfParams}} -> 
      case length(Params) == length(TypeOfParams) of
        false -> throw({line, Line, {wrong_number_of_args, FunctionName, stacktrace:generate()}});
        true -> noop
      end,
      {[], MixedParams} = lists:foldl(fun(ParamType, {[Param | Tail], Out}) ->
        {Tail, Out ++ [{ParamType, Param}]}
      end,
      {Params, []}, TypeOfParams),
      % io:fwrite("Running function ~p on ~p ~n", [F, MixedParams]),
      {ReturnType, run_function(Line, F, MixedParams)}
  end.

run_function(Line, {erlang_function, Module, Function}, Params) ->
  try
    ProcesedParams = lists:map(fun process_params/1, Params),
    % io:fwrite("Running erlang function ~p:~p (~p)~n", [Module, Function, ProcesedParams]),
    erlang:apply(Module, Function, ProcesedParams)
  catch
    {line, Line2, E} -> throw({line, Line2, {E, stacktrace:generate()}});
    _:Term -> throw({line, Line, {Term, stacktrace:generate()}})
  end.

exec_function(Command) -> 
  Result = run_command(Command),
  % io:fwrite("Return : ~p~n", [Result]),
  process_function_result(Result).

process_function_result({string, Value}) -> {string, [{string, Value}]};
process_function_result({Atom, Value}) -> {Atom, Value}.

process_params({commands, {block, P}}) -> fun() -> run_commands({commands, P}) end;
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
    {'EXIT', _} -> throw({cannot_be_converted_to_integer, S, stacktrace:generate()});
    V -> V
  end;
get_integer({integer, Int}) -> Int;
get_integer({function, Command}) -> get_integer(exec_function(Command)).

get_map({map, []}) -> [];
get_map({map, [{A, B} | T]}) -> [{get_string(A), B} | get_map({map, T})];
get_map({function, Command}) -> get_map(exec_function(Command)).

get_array({array, A}) -> get_array(A);
get_array([]) -> [];
get_array([H | T]) -> [process_params(H) | get_array(T)];
get_array(E) -> [process_params(E)].
