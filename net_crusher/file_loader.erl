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
-module(file_loader).

-export([
  start/0,
  stop/0,
  str_read_file/1,
  load_file/1,
  read_line/2,
  file_loader/0
]).

start() ->
  global:register_name(process_file_loader,
                       runtime:spawn_with_monitor(node(), runtime, spawn_with_name,
                                                  ["FileLoader",
                                                   fun() -> file_loader:file_loader() end])).

stop() ->
  tools:stop_process(process_file_loader).

load_file(FileName) ->
  tools:sync_msg(global:whereis_name(process_file_loader), file_loaded, load_file, {FileName}).

read_line(FileName, Line) ->
  tools:sync_msg(global:whereis_name(process_file_loader), line, read_line, {FileName, Line}).

str_read_file(StrFileName) ->
  tools:sync_msg(global:whereis_name(process_file_loader),
                 file_read, read_file, StrFileName).

file_loader() ->
  vars:cmd_set_name("FileLoader"),
  file_loader(dict:new()).

file_loader(Map) ->
  receive
    halt -> noop;
    {read_line, Target, {FileName, Nb}} ->
      Target ! {line, read_line_(FileName, Nb)},
      file_loader(Map);
    {load_file, Target, {FileName}} ->
      {NewMap, Result} = case dict:is_key(FileName, Map) of
        true ->
          {Map, dict:fetch(FileName, Map)};
        false ->
          {ok, Content} =
          try
            ruby:parse_file(FileName)
          catch
            E1 ->
              throw({unable_to_parse_file, FileName, {E1, stacktrace:generate()}});
            _:E2 ->
              throw({unable_to_parse_file, FileName, {E2, stacktrace:generate()}})
          end,
          logger:cmd_log(2, "File loaded " ++ FileName),
          {dict:store(FileName, Content, Map), Content}
      end,
      Target ! {file_loaded, Result},
      file_loader(NewMap);
    {read_file, Target, FileName} ->
      Target ! {file_read, read_file(FileName)},
      file_loader(Map)
  end.

read_file(FileName) ->
  logger:cmd_logf(1, "Reading file ~p", [FileName]),
  {ok, S} = file:read_file(FileName),
  binary_to_list(S).

read_line_(FileName, Nb) ->
  {ok, Fd} = file:open(FileName, [read]),
  find_line(Fd, Nb).

find_line(Fd, 1) ->
  Line = string:strip(io:get_line(Fd, ""), both, $\n),
  file:close(Fd),
  Line;
find_line(Fd, K) ->
  io:get_line(Fd, ""),
  find_line(Fd, K - 1).
