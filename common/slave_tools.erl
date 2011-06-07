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
-module(slave_tools).

-export([
  connect/3,
  connect_by_ssh/4,
  execute_cmd/1,
  kill/2
]).

connect(Hostname, SName, LogFun) ->
  Node = list_to_atom(SName ++ "@" ++ Hostname),
  LogFun("Try to connect to " ++ atom_to_list(Node)),
  case wait(Node, 1) of
    pang -> throw({unable_to_connect, Hostname});
    pong -> noop
  end,
  LogFun("Connection ok to " ++ atom_to_list(Node)),
  Node.
  
connect_by_ssh(Hostname, SName, Opts, LogFun) ->
  Node = list_to_atom(SName ++ "@" ++ Hostname),
  LogFun("Try to connect by ssh to " ++ atom_to_list(Node)),
  kill("ssh " ++ Hostname ++ " ", SName),
  Cmd = "ssh " ++ Hostname ++ " erl " ++ Opts ++ " -noshell -noinput -sname " ++ SName,
  LogFun("Executing: " ++ Cmd),
  spawn_link(?MODULE, execute_cmd, [Cmd]),
  case wait(Node, 8) of
    pang -> throw({unable_to_ssh_connect, Hostname});
    pong -> noop
  end,
  LogFun("Connection ok to " ++ atom_to_list(Node)),
  Node.

kill(Prefix, SName) ->
  Kill = Prefix ++ "\"ps axu | grep beam | grep pz | grep " ++ SName  ++ " | grep -v grep | awk '{print \\$2}' | xargs kill\"",
  %io:format("Execute kill command : ~s\n", [Kill]),
  execute_cmd(Kill).
  
wait(_Node, 0) ->
  pang;
wait(Node, K) ->
  receive
    after 500 -> noop
  end,
  case net_adm:ping(Node) of
    pong -> pong;
    pang -> wait(Node, K - 1)
  end.
  
execute_cmd(Cmd) ->
  %io:format("Try to execute\n~s\n", [Cmd]),
  os:cmd(Cmd).

