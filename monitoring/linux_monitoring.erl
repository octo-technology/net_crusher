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
-module(linux_monitoring).

-export([
  parse_stat/1,
  parse_net_dev/1,
  parse_meminfo/1,
  parse_net_tcp/1,
  get_hostname/0,
  get_id/1,
  monitoring_loop/4,
  monitoring_tcp_loop/4,
  latency_timer_loop/0,
  latency_monitor/4,
  spawn_writer/1,
  spawn_latency/4,
  write_loop/1,
  start/5]).

-define(PROC, "/proc").
-define(PROC_HOSTNAME, ?PROC ++ "/sys/kernel/hostname").
-define(PROC_STAT, ?PROC ++ "/stat").
-define(PROC_NET_DEV, ?PROC ++ "/net/dev").
-define(PROC_NET_TCP, ?PROC ++ "/net/tcp").
-define(PROC_MEM_INFO, ?PROC ++ "/meminfo").
  
extract_data(Line, List) -> 
  lists:map(fun({S, L}) ->
    string:sub_string(Line, S + 1, S + L)
  end, List).

ignore_line(FileDevice) ->
  io:get_line(FileDevice, "").
  
read_and_parse_line(FileDevice, Regexp) ->
  Line = io:get_line(FileDevice, ""),
  case re:run(Line, Regexp) of
    {match, [{_, _} | Result]} ->
      extract_data(Line, Result)
  end.

parse_net_tcp(FileDevice, Re, Established, Sent, Receive, Fin, Listen) ->
  case io:get_line(FileDevice, "") of
    eof -> [Established, Sent, Receive, Fin, Listen];
    Line -> case re:run(Line, Re) of
      {match, [{_, _} | Result]} ->
        [TcpStatusCode] = extract_data(Line, Result),
        case TcpStatusCode of 
          "01" -> parse_net_tcp(FileDevice, Re, Established + 1, Sent, Receive, Fin, Listen);
          "02" -> parse_net_tcp(FileDevice, Re, Established, Sent + 1, Receive, Fin, Listen);
          "03" -> parse_net_tcp(FileDevice, Re, Established, Sent, Receive + 1, Fin, Listen);
          "04" -> parse_net_tcp(FileDevice, Re, Established, Sent, Receive, Fin + 1, Listen);
          "05" -> parse_net_tcp(FileDevice, Re, Established, Sent, Receive, Fin + 1, Listen);
          "06" -> parse_net_tcp(FileDevice, Re, Established, Sent, Receive, Fin + 1, Listen);
          "07" -> parse_net_tcp(FileDevice, Re, Established, Sent, Receive, Fin + 1, Listen);
          "08" -> parse_net_tcp(FileDevice, Re, Established, Sent, Receive, Fin + 1, Listen);
          "09" -> parse_net_tcp(FileDevice, Re, Established, Sent, Receive, Fin + 1, Listen);
          "0A" -> parse_net_tcp(FileDevice, Re, Established, Sent, Receive, Fin, Listen + 1);
          "0B" -> parse_net_tcp(FileDevice, Re, Established, Sent, Receive, Fin + 1, Listen)
        end
    end
  end.
  
parse_net_tcp(Start) ->
  {ok, FileDevice} = file:open(?PROC_NET_TCP, [read]),
  io:get_line(FileDevice, ""),
  X = parse_net_tcp(FileDevice, "^\\s*\\S+ \\S+ \\S+ (\\S+)", 0, 0, 0, 0, 0),
  file:close(FileDevice),
  {tools:micro_timestamp() - Start, X}.
    
parse_stat(Start) ->
  {ok, FileDevice} = file:open(?PROC_STAT, [read]),
  X = read_and_parse_line(FileDevice, "cpu\\S* +(\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+)"),
  file:close(FileDevice),
  {tools:micro_timestamp() - Start, X}.

parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]) ->
  case io:get_line(FileDevice, "") of
    eof -> [Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive];
    Line ->
      case re:run(Line, "^(\\S+):\\s*(\\d+).*") of
        nomatch -> parse_meminfo(FileDevice, [Slab, SwapCached,SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]);
        {match, [{_, _}, {A1, A2}, {B1, B2}]} ->
          K = string:substr(Line, A1 + 1, A2),
          V = list_to_integer(string:substr(Line, B1 + 1, B2)),
          case K of
            "Slab" -> parse_meminfo(FileDevice, [V * 1024, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]);
            "SwapCached" -> parse_meminfo(FileDevice, [Slab, V * 1024, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]);
            "SwapTotal" -> parse_meminfo(FileDevice, [Slab, SwapCached, V * 1024, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]);
            "SwapFree" -> parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, V * 1024, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]);
            "PageTables" -> parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, SwapFree, V * 1024, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]);
            "VmallocUsed" -> parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, SwapFree, PageTables, V * 1024, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]);
            "MemTotal" -> parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, V * 1024, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]);
            "MemFree" -> parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, V * 1024, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]);
            "Buffers" -> parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, V * 1024, Cached, Committed_AS, Mapped, Active, Inactive]);
            "Cached" -> parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, V * 1024, Committed_AS, Mapped, Active, Inactive]);
            "Committed_AS" -> parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, V * 1024, Mapped, Active, Inactive]);
            "Mapped" -> parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, V * 1024, Active, Inactive]);
            "Active" -> parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, V * 1024, Inactive]);
            "Inactive" -> parse_meminfo(FileDevice, [Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, V * 1024]);
                _ -> parse_meminfo(FileDevice, [Slab, SwapCached,SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive])
          end
      end
  end.
  
parse_meminfo(Start) ->
  {ok, FileDevice} = file:open(?PROC_MEM_INFO, [read]),
  X = parse_meminfo(FileDevice, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
  file:close(FileDevice),
  {tools:micro_timestamp() - Start, X}.
  
parse_net_dev(Start) ->
  {ok, FileDevice} = file:open(?PROC_NET_DEV, [read]),
  % header
  ignore_line(FileDevice),
  % header
  ignore_line(FileDevice),
  % lo
  ignore_line(FileDevice),
  % find first active iface
  find_net_net_dev_not_null(Start, FileDevice).

find_net_net_dev_not_null(Start, FileDevice) ->
  [EthK, Receive, ReceiveError, Emit, EmitError] = read_and_parse_line(FileDevice, " *([^:]+): *(\\d+) +\\d+ +(\\d+) +\\d+ +\\d+ +\\d+ +\\d+ +\\d+ +(\\d+) +\\d+ +(\\d+) +\\d+ +\\d+ +\\d+ +\\d+ +\\d+"),
  case Receive of
    "0" ->
      find_net_net_dev_not_null(Start, FileDevice);
    _ ->
      file:close(FileDevice),
      {tools:micro_timestamp() - Start, [EthK, Receive, ReceiveError, Emit, EmitError]}
  end.

monitoring_loop(Start, NodeName, Target, Delay) ->
  Target ! {proc_stat, NodeName, parse_stat(Start)},
  Target ! {proc_net_dev, NodeName, parse_net_dev(Start)},
  Target ! {proc_meminfo, NodeName, parse_meminfo(Start)},
  receive
    after Delay -> noop
  end,
  monitoring_loop(Start, NodeName, Target, Delay).

monitoring_tcp_loop(Start, NodeName, Target, Delay) ->
  %Target ! {proc_net_tcp, NodeName, parse_net_tcp(Start)},
  receive
    after Delay -> noop
  end,
  monitoring_tcp_loop(Start, NodeName, Target, Delay).

get_hostname() ->
  {ok, FileDevice} = file:open(?PROC_HOSTNAME, [read]),
  {ok, [Line]} = io:fread(FileDevice, "", "~s"),
  file:close(FileDevice),
  Line.

get_id(Target) ->
  {ok, Hostname} = inet:gethostname(),
  Target ! {node(), get_hostname(), Hostname}.

out(Prefix, NodeName, NodeFile, Pattern, Data) ->
  {ok, FileDevice} = file:open(Prefix ++ NodeName ++ "_" ++ NodeFile ++ ".csv", [read, append]),
  io:fwrite(FileDevice, Pattern, Data),
  file:close(FileDevice).
  
write_loop(Prefix) ->
  receive
    {proc_stat, NodeName, {Timestamp, [User, Nice, System, Idle, Iowait, Irq, SoftIrq, Steal]}} ->
      out(Prefix, NodeName, "stat", "~w ~s ~s ~s ~s ~s ~s ~s ~s\n", [Timestamp / 1000000, User, Nice, System, Idle, Iowait, Irq, SoftIrq, Steal]);
    {proc_net_dev, NodeName, {Timestamp, [_IfName, ReceiveOk, ReceiveError, EmitOk, EmitError]}} -> 
      out(Prefix, NodeName, "net", "~w ~s ~s ~s ~s\n", [Timestamp / 1000000, ReceiveOk, ReceiveError, EmitOk, EmitError]);
    {proc_net_tcp, NodeName, {Timestamp, [Established, Sent, Receive, Fin, Listen]}} -> 
      out(Prefix, NodeName, "tcp", "~f ~w ~w ~w ~w ~w\n", [Timestamp / 1000000, Established, Sent, Receive, Fin, Listen]);
    {proc_meminfo, NodeName, {Timestamp, [Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive]}} -> 
      out(Prefix, NodeName, "mem", "~f ~w ~w ~w ~w ~w ~w ~w ~w ~w ~w ~w ~w ~w ~w\n", [Timestamp / 1000000, Slab, SwapCached, SwapTotal, SwapFree, PageTables, VmallocUsed, MemTotal, MemFree, Buffers, Cached, Committed_AS, Mapped, Active, Inactive])
  end,
  write_loop(Prefix).

latency_timer_loop() ->
  receive
    {ping, Target} -> Target ! {pong, tools:micro_timestamp()}
  end,
  latency_timer_loop().

start(List, Start, Target, Delay, DelayTcp) ->
  lists:map(fun(H) ->
    Name = atom_to_list(H),
    SubStr = string:sub_string(Name, string:str(Name, "@") + 1),
    %io:format("Starting monitoring on ~p : ~p\n", [H, SubStr]),
    spawn_link(H, ?MODULE, monitoring_loop, [Start, SubStr, Target, Delay]),
    spawn_link(H, ?MODULE, monitoring_tcp_loop, [Start, SubStr, Target, DelayTcp])
    %io:format("Monitoring started on ~p\n", [H])
  end, List).

latency_monitor(GlobalStart, Prefix, Delay, List) ->
  receive
    after Delay -> noop
  end,
  {ok, FileId} = file:open(Prefix ++ "latency.csv", [write, append]),
  io:fwrite(FileId, "~f ", [(tools:micro_timestamp() - GlobalStart) / 1000000]),
  lists:map(fun(S) ->
    %io:format("Send ping to ~p\n", [S]),
    Start = tools:micro_timestamp(),
    S ! {ping, self()},
    receive 
      {pong, Remote} ->
        Stop = tools:micro_timestamp(),
        %io:format("Pong received\n", []),
        io:fwrite(FileId, "~f ~f ", [(Stop - Start) / 1000, (Start + ((Stop - Start) / 2) - Remote) / 1000])
      after 1000 ->
        io:fwrite(FileId, "~f ~f ", [-1, -1])
    end
  end, List),
  io:fwrite(FileId, "\n", []),
  file:close(FileId),
  latency_monitor(GlobalStart, Prefix, Delay, List).
  
spawn_latency(Start, Prefix, Delay, List) ->
  {ok, FileId} = file:open(Prefix ++ "latency.csv", [write, append]),
  io:fwrite(FileId, "#timestamp ", []),
  Childs = lists:map(fun(S) -> 
    io:fwrite(FileId, "~s ", [S]),
    spawn_link(S, ?MODULE, latency_timer_loop, [])
  end, List),
  io:fwrite(FileId, "\n", []),
  file:close(FileId),
  spawn_link(node(), ?MODULE, latency_monitor, [Start, Prefix, Delay, Childs]).
  
spawn_writer(Prefix) ->
  spawn_link(?MODULE, write_loop, [Prefix]).