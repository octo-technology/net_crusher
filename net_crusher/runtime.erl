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
-module(runtime).

-export([
  run/1,
  stop/0,
  sub_process/3,
  sub_process_light/3,

  execute/1,
  execute_and_wait/1,
  fork/2,
  fork_light/2,
  fork_distributed/2,

  spawn_with_monitor_handler/4,
  spawn_with_monitor/4,
  spawn_child_with_monitor/4,

  spawn_with_name/2,

  init_node/1,
  start_spawn_with_monitor/0,
  stop_spawn_with_monitor/0
]).
-include("../common/macros.hrl").

start_spawn_with_monitor() ->
  global:register_name(process_spawn_with_monitor,
                       spawn(node(), ?MODULE, spawn_with_monitor_handler,
                             [[], [], dict:new(), 0])).

stop_spawn_with_monitor() ->
  tools:stop_process(process_spawn_with_monitor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MONITORING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
spawn_child(Target, Node, Module, Function, Args) ->
  ProcessId = spawn(Node, Module, Function, Args),
  erlang:monitor(process, ProcessId),
  case Target of
    undefined -> noop;
    _ -> Target ! {process, ProcessId}
  end,
  ProcessId.

spawn_children([], SubProcesses, ParentProcessDict, _) ->
  logger:log(4, "No more process to spawn"),
  {[], SubProcesses, ParentProcessDict};
spawn_children([{Target, Node, Module, Function, Args} | T] = ProcessesToSpawn, SubProcesses, ParentProcessDict, MaxProcesses) ->
  case length(SubProcesses) of
    N when N > MaxProcesses ->
      logger:log(2, "Too many processes are running, waiting before spawning a new one"),
      {ProcessesToSpawn, SubProcesses, ParentProcessDict};
    N ->
      logger:logf(3, "Spawn child ~p", [N]),
      NewProcess = spawn_child(Target, Node, Module, Function, Args),
      spawn_children(T, [NewProcess | SubProcesses], dict:append(Target, NewProcess, ParentProcessDict), MaxProcesses)
  end.

find_parent_pid_of(ParentProcessDict, Pid) ->
  dict:fold(fun(Key, Value, AccIn) ->
              case AccIn of
                undefined -> case lists:any(fun(E) -> E == Pid end, Value) of
                               true -> Key;
                               _ -> undefined
                             end;
                _ -> AccIn
              end
            end,
            undefined, ParentProcessDict).

remove_pid(ParentProcessDict, Pid) ->
  case find_parent_pid_of(ParentProcessDict, Pid) of
    undefined -> ParentProcessDict;
    ParentPid ->
      SubProcesses = dict:fetch(ParentPid, ParentProcessDict),
      logger:logf(3, "Removing ~p from parent ~p subprocess", [Pid, ParentPid]),
      NewSubProcesses = lists:delete(Pid, SubProcesses),
      case length(NewSubProcesses) of
          0 ->
              logger:logf(3, "Parent ~p has no more subprocess", [ParentPid]),
              ParentPid ! no_more_subprocess;
          _ -> noop
      end,
      dict:store(ParentPid, NewSubProcesses, ParentProcessDict)
  end.

% ProcessesToSpawn is a list of Pid to spawn when resources are available (ie. #{running processes} <= MaxProcesses
% SubProcesses is a list of all children spawned. It is used to be able to limit the number of children
% ParentProcessDict is a dictionnary with the Pid of the parent process as key and
% a list of spawn children as value
spawn_with_monitor_handler(ProcessesToSpawn, SubProcesses,
                           ParentProcessDict, MaxProcesses) ->
  vars:set_name("spawner"),
  receive
    halt -> noop;
    {spawn, Target, {Node, Module, Function, Args}} ->
      ProcessId = spawn(Node, Module, Function, Args),
      erlang:monitor(process, ProcessId),
      Target ! {process, ProcessId},
      spawn_with_monitor_handler(ProcessesToSpawn, SubProcesses,
                                 ParentProcessDict, MaxProcesses);
    {spawn_child, Target, {Node, Module, Function, Args, NewMaxProcesses}} ->
      {NewProcessesToSpawn,
       NewSubProcesses,
       NewParentProcessDict} = spawn_children([{Target, Node, Module,
                                                Function, Args} | ProcessesToSpawn],
                                              SubProcesses, ParentProcessDict,
                                              NewMaxProcesses),
      spawn_with_monitor_handler(NewProcessesToSpawn, NewSubProcesses,
                                 NewParentProcessDict, NewMaxProcesses);
    {get_nb_child_to_wait, Target, {}} ->
      NbChildren = case dict:find(Target, ParentProcessDict) of
        {ok, Value} -> length(Value);
        _ -> 0
      end,
      Target ! {wait_for_child, NbChildren},
      spawn_with_monitor_handler(ProcessesToSpawn, SubProcesses,
                                 ParentProcessDict, MaxProcesses);
    {'DOWN', _, process, Pid, normal} ->
      logger:logf(4, "New process down ~p", [Pid]),
      CleanParentProcessDict = remove_pid(ParentProcessDict, Pid),
      {NewProcessesToSpawn,
       NewSubProcesses,
       NewParentProcessDict} = spawn_children(ProcessesToSpawn,
                                              lists:delete(Pid, SubProcesses),
                                              CleanParentProcessDict, MaxProcesses),
      spawn_with_monitor_handler(NewProcessesToSpawn, NewSubProcesses,
                                 NewParentProcessDict, MaxProcesses);
    {'DOWN', _, process, _, E} ->
      misc:sleep_ms(100),
      io:fwrite("Process crash detected\n~150p\n", [E]),
      misc:sleep_ms(100),
      halt(1)
  end.

get_nb_child_to_wait() ->
  tools:sync_msg(global:whereis_name(process_spawn_with_monitor),
                 wait_for_child, get_nb_child_to_wait,
                 {}).

spawn_with_monitor(Node, Module, Function, Args) ->
  tools:sync_msg(global:whereis_name(process_spawn_with_monitor), process, spawn,
                 {Node, Module, Function, Args}).

spawn_child_with_monitor(Node, Module, Function, Args) ->
  tools:sync_msg(global:whereis_name(process_spawn_with_monitor), process, spawn_child,
                 {Node, Module, Function, Args,
                  list_to_integer(vars:g_or_else("max_player", "10000000"))}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RUNTIME
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute(FileName) ->
  exCode:load_file(".", FileName).

execute(FileName, Commands) ->
  case catch interpreter:run_commands(Commands) of
    {line, Line, E} ->
      LineStr = file_loader:read_line(FileName, Line),
      throw({execution_error,
        {error, E},
        {context,
          {node, node()},
          {process_name, vars:get_name()},
          {file, FileName},
          {line, Line, LineStr},
          {last_http_url, get(last_http_url)}
      }});
    X -> X
  end.

execute_and_wait(StrFileName) ->
  execute(StrFileName),
  wait_for_childs_end().

init_process(Name, Map) ->
  lists:map(fun({K, V}) -> put(K, V) end, Map),
  vars:set_name(Name).

sub_process(Name, FileName, Map) ->
  init_process(Name, Map),
  {ok, Hostname} = inet:gethostname(),
  vars:s("hostname", Hostname),
  tools:init_rand(Name),
  execute(FileName).
  %% wait_for_childs_end().

sub_process_light(Name, FileName, Map) ->
  init_process(Name, Map),
  execute(FileName).
  %% wait_for_childs_end().

fork_(Node, StrName, StrFileName) ->
  spawn_child_with_monitor(Node, ?MODULE, sub_process,
                           [StrName, StrFileName, get()]).

fork_light(StrName, StrFileName) ->
  ?TIME(spawn_child(undefined, node(), ?MODULE, sub_process_light, [StrName, StrFileName, get()]), "fork_light").

fork(StrName, StrFileName) ->
  ?TIME(fork_(node(), StrName, StrFileName), "fork").

fork_distributed(StrName, StrFileName) ->
  fork_(tools:sync_msg(global:whereis_name(process_fork_handler),
                       node, get_node_for_fork, {StrName}),
        StrName, StrFileName).

init_node(FileName) ->
  inets:start(),
  crypto:start(),
  ssl:start(),
  mnesia:start(),
  tools_http:start_void_http_monitor().

run(FileName) ->
  io:setopts([{encoding, utf8}]),
  logger:set_log_level(0),
  init_node(FileName),
  start_spawn_with_monitor(),
  misc:inject_argv(),
  distributed:start(get("script_dir"), get("remote_nodes_ssh"),
                    get("remote_nodes"), get("use_local_node")),
  mnesia:change_config(extra_db_nodes, nodes()),

  global:register_name(process_assert_monitor, spawn_with_monitor(node(), ?MODULE, spawn_with_name, ["AssertMonitor", fun() -> assert:assert_monitor(0) end])),

  stats:start_timestamp_handler(),

  stats:start_stats_monitor(),
  file_loader:start(),

  yml_loader:start(),

  init_process(<<"unknown">>, get()).
  %% ProcessId = spawn_with_monitor(node(), ?MODULE, sub_process,
  %%                                ["unknown", FileName, get()]),
  %% wait_for_process_end(ProcessId),

  %% stop().

stop() ->
  wait_for_childs_end(),
  global:whereis_name(process_assert_monitor) ! halt,
  misc:sleep_ms(100),
  logger:log(0, "End"),
  stats:stop_stats_monitor(),
  stats:stop_timestamp_handler(),
  file_loader:stop(),
  yml_loader:stop(),
  distributed:stop(),
  misc:sleep_ms(100),
  stop_spawn_with_monitor(),
  misc:sleep_ms(100),
  mnesia:stop().

wait_for_childs_end() ->
  case get_nb_child_to_wait() of
    0 -> logger:log(4, "No child to wait");
    _ ->
      logger:log(4, "Wait for child end"),
      receive
        no_more_subprocess -> noop
      end
  end.

wait_for_process_end(P) ->
  case rpc:pinfo(P, [status]) of
    [{status, _}] ->
      misc:sleep_ms(500),
      wait_for_process_end(P);
    _ -> noop
  end.

spawn_with_name(Name, Function) ->
  vars:set_name(Name),
  Function().
