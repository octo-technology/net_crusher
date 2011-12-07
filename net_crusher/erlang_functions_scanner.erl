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
-module(erlang_functions_scanner).

-export([
  scan_module/1,
  scan_modules/1
]).

scan_module(Module) ->
  FileName = atom_to_list(Module) ++ ".erl",
  {ok, Content} = file:read_file(FileName),
  {ok, Tokens, _} = erl_scan:string(binary_to_list(Content)),
  scan_func(Module, Tokens, get_exports(Module:module_info())).

get_exports([{exports, Exports} | _]) -> Exports.

scan_func(_, _, []) -> [];
scan_func(Module, Tokens, [{module_info, _} | T]) -> scan_func(Module, Tokens, T);
scan_func(Module, Tokens, [{Name, _} | T]) -> process_func(Module, seek(Tokens, Name)) ++ scan_func(Module, Tokens, T).

seek([{atom, K, Name}, {'(', K} | T], Name) -> {Name, seek_vars(T)};
seek([_ | T], Name) -> seek(T, Name).

seek_vars([{atom, _, _}, {':', _}, {atom, K, Name} | T]) -> seek([{atom, K, Name} | T], Name);
seek_vars([{var, _, Name} | T]) -> [Name | seek_vars(T)];
seek_vars([{',', _} | T]) -> seek_vars(T);
seek_vars([{'-', _} | T]) -> seek_vars(T);
seek_vars([{')', _} | _]) -> [].

process_func(Module, {Name, Args}) ->
  case parse_name(atom_to_list(Name)) of
    {ReturnType, RealName} -> [{RealName, {ReturnType, {erlang_function, Module, Name}, lists:map(fun process_arg/1, Args)}}];
    {} -> []
  end.

parse_name(Name) ->
  lists:foldr(fun({S, Sym}, Acc) ->
    case string:str(Name, S ++ "_") of
      1 ->
        RealName = string:substr(Name, string:len(S) + 2),
        {Sym, RealName};
      _ -> Acc
    end
  end, {}, [{"cmd", void}, {"str", string}, {"map", map}, {"blk", blk}, {"bool", bool}, {"int", integer}]).

process_arg(Arg) ->
  Name = atom_to_list(Arg),
  Sym = lists:foldr(fun({S, Sym}, Acc) ->
    case string:str(Name, S) of
      1 ->
        Sym;
      _ ->  case string:str(Name, "_" ++ S) of
        1 -> Sym;
        _ -> Acc
      end
    end
  end, {}, [{"Str", string}, {"Int", integer},  {"Map", map}, {"Arr", array}, {"Blk", commands}, {"Bool", bool}]),
  case Sym of
    {} -> throw({wrong_param_name, Name});
    V -> V
  end.
  
scan_modules(Modules) -> 
  lists:foldl(fun(Module, Acc) ->
    io:fwrite("Scan module ~s\n", [Module]),
    scan_module(Module) ++ Acc
  end, [], Modules).
