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
-module(yml_loader).

-export([load/1,
         yml_loader/0]).

load(FileName) ->
  tools:sync_msg(global:whereis_name(process_yml_loader), file_loaded, load, {FileName}).

load_file(FileName) ->
  logger:cmd_log(1, "Loading file " ++ FileName),
  {ok, Fd} = file:open(FileName, [read]),
  Result = read(Fd),
  file:close(Fd),
  process(Result).

yml_loader() ->
  receive
    halt -> noop;
    {load, Target, {{Module, Func, Args}}} ->
      Target ! {file_loaded, load_file(apply(Module, Func, Args))},
      yml_loader();
    {load, Target, {FileName}} when is_list(FileName) ->
      Target ! {file_loaded, load_file(FileName)},
      yml_loader()
  end.

read(Fd) ->
  case file:read_line(Fd) of
    eof -> [];
    {ok, Line} -> [string:strip(string:strip(Line, right, $\n)) | read(Fd)]
  end.

process([]) -> [];
process([[] | T]) -> process(T);
process([Str | T]) when erlang:hd(Str) == $# -> process(T);
process([H | T]) -> [parse(H) | process(T)].

parse(Str) ->
  case re:run(Str, "^(\\S+):\s+\"([^\"]+)\"$") of
    {match, [{_, _}, {A1, A2}, {B1, B2}]} -> {string:substr(Str, A1 + 1, A2), string:substr(Str, B1 + 1, B2)};
    nomatch ->
      case re:run(Str, "^(\\S+):\s+(\\S.*)$") of
        {match, [{_, _}, {A1, A2}, {B1, B2}]} -> {string:substr(Str, A1 + 1, A2), string:substr(Str, B1 + 1, B2)};
        nomatch ->
          case re:run(Str, "^(\\S+):\s*$") of
            {match, [{_, _}, {A1, A2}]} -> {string:substr(Str, A1 + 1, A2), ""};
            nomatch -> throw({yml_parse_error, Str})
          end
      end
  end.
