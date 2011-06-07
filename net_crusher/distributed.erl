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
-module(distributed).

-export([
  start/4,
  stop/0,
  fork_handler/2
]).

start(ScriptDir, RemoteNodeSSH, RemoteNodes, UseLocalNode) ->
  case lists:member(process_fork_handler, global:registered_names()) of
    true -> logger:cmd_log(2, "Process fork handler already started");
    _ ->
      logger:cmd_log(1, "Starting process fork handler"),
      LogFun = fun(S) -> logger:cmd_log(4, S) end,
      Result = case UseLocalNode of
              "false" -> [];
              _ -> [node()]
            end
            ++
            connect_by_ssh(split(RemoteNodeSSH), ScriptDir, LogFun)
            ++
            connect(split(RemoteNodes), LogFun),
      logger:cmd_log(3, "Connected nodes : " ++ integer_to_list(length(Result))),
      global:register_name(process_fork_handler,
                           spawn(node(), ?MODULE, fork_handler, [Result, 1]))
  end.

stop() ->
  tools:stop_process(process_fork_handler).

fork_handler(Nodes, CurrentIndex) ->
  receive
    halt -> noop;
    {get_node_for_fork, Target, {Name}} ->
      TargetNode = lists:nth(CurrentIndex, Nodes),
      logger:cmd_logf(5, "Create new process ~p on ~p", [Name, TargetNode]),
      Target ! {node, TargetNode},
      fork_handler(Nodes, CurrentIndex rem length(Nodes) + 1)
  end.

split(undefined) -> undefined;
split(Str) -> [binary_to_list(X) || X <- re:split(Str, ",")].

connect(undefined, _) -> [];
connect(List, LogFun) ->
  pmap:pmap(fun(Hostname) ->
    slave_tools:connect(Hostname, "net_crusher", LogFun)
  end, List).

connect_by_ssh(undefined, _, _) -> [];
connect_by_ssh(List, ScriptDir, LogFun) ->
  pmap:pmap(fun(Hostname) ->
    Node = slave_tools:connect_by_ssh(Hostname, "net_crusher", "+c +K true -setcookie my_beautiful_cookie_qui_tue -pz " ++ ScriptDir, LogFun),
    rpc:call(Node, runtime, init_node, [Hostname]),
    Node
  end, List).
