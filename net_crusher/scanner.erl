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
-module(scanner).

-export([
  scan/1,
  process/2,
  generate_grammar/3,

  make_cmd/4,
  make_blk/4,
  make_str/4,
  make_map/4,
  make_arr/4,
  make_bool/4,
  make_int/4
  ]).

scan(Module) ->
  FileName = atom_to_list(Module) ++ ".erl",
  {ok, Content} = file:read_file(FileName),
  {ok, Tokens, _} = erl_scan:string(binary_to_list(Content)),
  {ok, scan_func(Tokens, get_exports(Module:module_info()))}.

get_exports([{exports, Exports} | _]) -> Exports.

scan_func(_, []) -> [];
scan_func(Tokens, [{module_info, _} | T]) -> scan_func(Tokens, T);
scan_func(Tokens, [{Name, _} | T]) -> [seek(Tokens, Name) | scan_func(Tokens, T)].

seek([{atom, K, Name}, {'(', K} | T], Name) -> {Name, seek_vars(T)};
seek([_ | T], Name) -> seek(T, Name).

seek_vars([{atom, _, ModuleName}, {':', _}, {atom, K, Name} | T]) -> seek([{atom, K, Name} | T], Name);
seek_vars([{var, _, Name} | T]) -> [Name | seek_vars(T)];
seek_vars([{',', _} | T]) -> seek_vars(T);
seek_vars([{'-', _} | T]) -> seek_vars(T);
seek_vars([{')', _} | _]) -> [].

process(Module, [{Name, Args} | T]) -> [process(Module, atom_to_list(Name), Args) | process(Module, T)];
process(_, []) -> [].

process(Module, Name, Args) ->
  lists:foldr(fun(S, Acc) ->
    case string:str(Name, S ++ "_") of
      1 ->
        Prefix = string:substr(Name, string:len(S) + 2),
        {Prefix, erlang:apply(?MODULE, list_to_atom("make_" ++ S), [Module, Name, Prefix, Args])};
      _ -> Acc
    end
  end, {}, ["cmd", "str", "map", "blk", "bool", "int"]).

make_blk(Module, Name, N, Args) ->
  {L1, L2} = parse_args(Args, 2),
  io:fwrite("Adding block ~s:~s\n", [Module, Name]),
  "command -> " ++ N ++ " " ++ L1 ++ " : { element(2, '$1'), " ++ atom_to_list(Module) ++ ", " ++ Name ++ ", [" ++ L2 ++ "]}.".

make_cmd(Module, Name, N, Args) ->
  {L1, L2} = parse_args(Args, 2),
  io:fwrite("Adding command ~s:~s\n", [Module, Name]),
  "command -> " ++ N ++ " " ++ L1 ++ " : { element(2, '$1'), " ++ atom_to_list(Module) ++ ", " ++ Name ++ ", [" ++ L2 ++ "]}.".

make_map(Module, Name, N, Args) ->
  {L1, L2} = parse_args(Args, 2),
  io:fwrite("Adding map function ~s:~s\n", [Module, Name]),
  "expr -> " ++ N ++ " '(' " ++ L1 ++ " ')' : { function, {map, { element(2, '$1'), " ++ atom_to_list(Module) ++ ", " ++ Name ++ ", [" ++ L2 ++ "]}}}.".

make_arr(Module, Name, N, Args) ->
  {L1, L2} = parse_args(Args, 2),
  io:fwrite("Adding arr function ~s:~s\n", [Module, Name]),
  "expr -> " ++ N ++ " '(' " ++ L1 ++ " ')' : { function, {array, { element(2, '$1'), " ++ atom_to_list(Module) ++ ", " ++ Name ++ ", [" ++ L2 ++ "]}}}.".

make_str(Module, Name, N, Args) ->
  {L1, L2} = parse_args(Args, 3),
  io:fwrite("Adding str function ~s:~s\n", [Module, Name]),
  "expr -> " ++ N ++ " '(' " ++ L1 ++ " ')' : { function, {string, { element(2, '$1'), " ++ atom_to_list(Module) ++ ", " ++ Name ++ ", [" ++ L2 ++ "]}}}.".

make_int(Module, Name, N, Args) ->
  {L1, L2} = parse_args(Args, 3),
  io:fwrite("Adding int function ~s:~s\n", [Module, Name]),
  "expr -> " ++ N ++ " '(' " ++ L1 ++ " ')' : { function, {integer, { element(2, '$1'), " ++ atom_to_list(Module) ++ ", " ++ Name ++ ", [" ++ L2 ++ "]}}}.".

make_bool(Module, Name, N, Args) ->
  {L1, L2} = parse_args(Args, 3),
  io:fwrite("Adding bool function ~s:~s\n", [Module, Name]),
  "expr -> " ++ N ++ " '(' " ++ L1 ++ " ')' : { function, {bool, { element(2, '$1'), " ++ atom_to_list(Module) ++ ", " ++ Name ++ ", [" ++ L2 ++ "]}}}.".

remove_underscore(A) when hd(A) == $_ -> string:substr(A, 2);
remove_underscore(A) -> A.

parse_args(A, K) -> parse_args(A, {"", ""}, K).

parse_args([], {A1, A2}, _) -> {A1, A2};
parse_args([H | T], {A1, A2}, K) ->
  Prefix = string:substr(remove_underscore(atom_to_list(H)), 1, 3),
  {B1, B2} = case Prefix of
    "Str" -> {concat(A1, " ',' ", "expr"), concat(A2, " ,", "{ string, '$" ++ integer_to_list(K) ++ "'}")};
    "Int" -> {concat(A1, " ',' ", "expr"), concat(A2, " ,", "{ integer, '$" ++ integer_to_list(K) ++ "'}")};
    "Map" -> {concat(A1, " ',' ", "expr"), concat(A2, " ,", "{ map, '$" ++ integer_to_list(K) ++ "'}")};
    "Arr" -> {concat(A1, " ',' ", "expr"), concat(A2, " ,", "{ array, '$" ++ integer_to_list(K) ++ "'}")};
    "Blk" ->
       KK = case K of
        2 -> 3;
        _ -> K
      end,
      {A1 ++ " do commands end", concat(A2, " ,", "{commands, '$" ++ integer_to_list(KK) ++ "'}")};
    _ -> throw({unknown_type, H, Prefix})
  end,
  parse_args(T, {B1, B2}, K + 2).

concat(A, B, C) ->
  case string:len(A) of
    0 -> C;
    _ -> A ++ B ++ C
  end.

generate_grammar(TplFile, OutputDir, Modules) -> generate_grammar(TplFile, OutputDir, Modules, []).

generate_grammar(TplFile, OutputDir, [], Data) -> generate(TplFile, OutputDir, Data);
generate_grammar(TplFile, OutputDir, [H | T], Data) ->
  io:fwrite("Scan module ~s\n", [H]),
  {ok, Funcs} = scan(H),
  generate_grammar(TplFile, OutputDir, T, Data ++ process(H, Funcs)).

generate(TplFile, OutputDir, Data) ->
  {Tokens, Rules} = assembl(Data, {"", ""}),
  {ok, Orig} = file:read_file(atom_to_list(TplFile) ++ ".yrl.tpl"),
  Content = re:replace(re:replace(binary_to_list(Orig), "##TOKENS##", Tokens), "##RULES##",Rules),
  ok = file:write_file(OutputDir ++ "/" ++ atom_to_list(TplFile) ++ ".yrl", list_to_binary(Content)),
  file:set_cwd(OutputDir),
  {ok, _} = yecc:file(TplFile),
  {ok, _} = c:c(TplFile),
  %file:delete(atom_to_list(TplFile) ++ ".yrl"),
  %file:delete(atom_to_list(TplFile) ++ ".erl"),
  ok.

assembl([{Token, Rule} | T], {A, B}) -> assembl(T, {A ++ "\n" ++ Token, B ++ "\n" ++ Rule});
assembl([{} | T], {A, B}) -> assembl(T, {A, B});
assembl([], {A, B}) -> {A, B}.
