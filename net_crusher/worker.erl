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
-module(worker).

-export([cmd_start_worker/2,
         cmd_trigger_worker/1,
         cmd_kill_worker/1,
         worker/2
        ]).

cmd_start_worker(StrWorkerName, StrFileName) ->
  global:register_name(list_to_atom(StrWorkerName),
                      runtime:spawn_with_monitor(node(), ?MODULE, worker,
                                                 [StrWorkerName, StrFileName])).

cmd_trigger_worker(StrWorkerName) ->
  global:whereis_name(list_to_atom(StrWorkerName)) ! {trigger, get()}.

cmd_kill_worker(StrWorkerName) ->
  tools:stop_process(list_to_atom(StrWorkerName)).

worker(Name, FileName) ->
  vars:cmd_s("name", Name),
  logger:cmd_log(1, "Starting worker"),
  Commands = file_loader:load_file(FileName),
  worker_loop(FileName, Commands).

worker_loop(FileName, Commands) ->
  receive
    {trigger, EnvMap} ->
      lists:map(fun({K, V}) ->
                  case K of
                    "name" -> noop;
                    _ -> put(K, V)
                  end
                end,
                EnvMap),
      logger:cmd_log(1, "Worker triggered"),
      runtime:execute(FileName, Commands),
      worker_loop(FileName, Commands);
    halt -> logger:cmd_log(1, "Worker killed")
  end.
