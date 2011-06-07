#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp auto +P 250000 +c +K true -rsh ssh -sname master -setcookie mycookie -kernel inet_dist_listen_min 4711 inet_dist_listen_max 4711
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
main([]) ->
    ScriptDir = filename:dirname(escript:script_name()),
    code:add_path(ScriptDir),
    Opts = "-setcookie mycookie -smp auto +P 250000 +c +K true -pz " ++ ScriptDir,
    Target = "data/" ++ integer_to_list(tools:micro_timestamp()) ++ "/",
    io:format("Output directory : ~s\n", [Target]),
    file:make_dir(Target),
    MonitoringIps = read_list("nodes"),
    file:copy("nodes", Target ++ "/nodes"),
    io:format("MonitoringNodes ~p\n", [MonitoringIps]),
    slave_tools:kill("", "slave"),
    MonitoringNodes = slave_tools:connect_by_ssh(MonitoringIps, "monitoring", Opts, 4713),
    FinalMonitoringNodes = lists:append(MonitoringNodes, [node()]),
    io:fwrite("Processes for monitoring : ~p\n", [FinalMonitoringNodes]),
    StatWriter = linux_monitoring:spawn_writer(Target),
    linux_monitoring:start(FinalMonitoringNodes, tools:micro_timestamp(), StatWriter, 1000, 10000),
    linux_monitoring:spawn_latency(tools:micro_timestamp(), Target, 2000, FinalMonitoringNodes),
    io:fwrite("Ready.\n"),
    receive
      stop -> noop
    end.

read_list(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  for_each_lines(Device).

for_each_lines(Device) ->
  case io:fread(Device, "", "~s\n") of
    eof -> file:close(Device), [];
      {ok, [Ip]} -> [ Ip | for_each_lines(Device)]
  end.
