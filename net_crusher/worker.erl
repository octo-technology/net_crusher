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

-export([
         start/2,
         trigger/1,
         kill/1,
         worker/2
        ]).

start(StrWorkerName, StrFileName) ->
  global:register_name(binary_to_atom(StrWorkerName, utf8),
                      runtime:spawn_with_monitor(node(), ?MODULE, worker,
                                                 [StrWorkerName, StrFileName])).

trigger(StrWorkerName) ->
  global:whereis_name(binary_to_atom(StrWorkerName, utf8)) ! {trigger, get()}.

kill(StrWorkerName) ->
  tools:stop_process(binary_to_atom(StrWorkerName, utf8)).

worker(Name, FileName) ->
  vars:set_name(Name),
  logger:log(1, "Starting worker"),
  worker_loop(FileName).

worker_loop(FileName) ->
  receive
    {trigger, EnvMap} ->
      lists:map(fun({K, V}) ->
                  case K of
                    <<"name">> -> noop;
                    _ -> put(K, V)
                  end
                end,
                EnvMap),
      logger:log(1, "Worker triggered"),
      runtime:execute(FileName),
      worker_loop(FileName);
    halt -> logger:log(1, "Worker killed")
  end.
