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
         cmd_trigger_scheduled/2,
         worker/2,
         worker_scheduled/3
        ]).

cmd_start_worker(StrWorkerName, Blk) ->
  global:register_name(list_to_atom(StrWorkerName),
                      runtime:spawn_with_monitor(node(), ?MODULE, worker,
                                                 [StrWorkerName, Blk])).

cmd_trigger_scheduled(StrWorkerName, IntDelay) ->
  global:register_name(list_to_atom(StrWorkerName ++ "_scheduled"),
                       runtime:spawn_with_monitor(node(), ?MODULE, worker_scheduled,
                                                 [StrWorkerName, IntDelay, get()])).
  

cmd_trigger_worker(StrWorkerName) ->
  global:whereis_name(list_to_atom(StrWorkerName)) ! {trigger_with_map, get()}.

cmd_kill_worker(StrWorkerName) ->
  tools:stop_process(list_to_atom(StrWorkerName)),
  tools:stop_process(list_to_atom(StrWorkerName ++ "_scheduled")).

worker_scheduled(StrWorkerName, IntDelay, EnvMap) ->
  lists:map(fun({K, V}) ->
              case K of
                "name" -> noop;
                _ -> put(K, V)
              end
            end,
            EnvMap),
  worker_scheduled_loop(StrWorkerName, IntDelay).
  
worker_scheduled_loop(StrWorkerName, IntDelay) ->
  receive
    halt -> noop
    after IntDelay ->
      global:whereis_name(list_to_atom(StrWorkerName)) ! {trigger_with_map, get()},
      worker_scheduled_loop(StrWorkerName, IntDelay)
  end.
  
worker(Name, Blk) ->
  vars:cmd_s("name", Name),
  logger:cmd_log(1, "Starting worker"),
  worker_loop(Name, Blk).

worker_loop(Name, Blk) ->
  receive
    {trigger_with_map, EnvMap} ->
      lists:map(fun({K, V}) ->
                  case K of
                    "name" -> noop;
                    _ -> put(K, V)
                  end
                end,
                EnvMap),
      Blk(),
      worker_loop(Name, Blk);
    halt -> logger:cmd_log(1, "Worker killed " ++ Name)
  end.
