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
-module(stats).

-export([
  start_stats_monitor/0,
  stop_stats_monitor/0,
  stats_monitor/0,

  start_timestamp_handler/0,
  stop_timestamp_handler/0,
  put_timestamp/1,
  chrono_delta/2,
  get_timestamp/1,

  chrono/2,
  send_stats/3
]).
-include_lib("stdlib/include/qlc.hrl").
-include("../common/macros.hrl").

-record(timestamp, {label, timestamp}).

start_timestamp_handler() ->
  mnesia:create_table(timestamp,
                      [{attributes, record_info(fields, timestamp)},
                       {index, [timestamp]},
                       {ram_copies, [node()|nodes()]},
                       {type, set}]).

stop_timestamp_handler() ->
  noop.

start_stats_monitor() ->
  global:register_name(process_stats_monitor,
                       runtime:spawn_with_monitor(node(), runtime,
                                                  spawn_with_name,
                                                  ["StatsMonitor",
                                                   fun() -> stats:stats_monitor() end])).

put_timestamp(StrLabel) ->
  ?TIME(put_timestamp_(StrLabel), [put_timestamp, StrLabel]).

put_timestamp_(StrLabel) ->
  mnesia:transaction(fun() ->
                       mnesia:write(#timestamp{ label=StrLabel,
                                                timestamp=tools:micro_timestamp() })
                     end).

get_timestamp(StrLabel) ->
  ?TIME(get_timestamp_(StrLabel), [get_timestamp, StrLabel]).

get_timestamp_(StrLabel) ->
  case mnesia:async_dirty(fun() ->
                            mnesia:read({timestamp, StrLabel})
                          end) of
    [Row] -> Row#timestamp.timestamp;
    _ -> undefined
  end.

chrono_delta(StrLabel, StrEventName) ->
  Now = tools:micro_timestamp(),
  Start = get_timestamp(StrLabel),
  send_stats(Now, Now - Start, StrEventName).

stop_stats_monitor() ->
  tools:stop_process(process_stats_monitor).

chrono(StrEventName, Blk) ->
  Start = tools:micro_timestamp(),
  Blk(),
  Stop = tools:micro_timestamp(),
  Delay = Stop - Start,
  send_stats(Start, Delay, StrEventName).

get_log_filename() ->
  case get("event_log_prefix") of
    undefined -> undefined;
    _ -> case get("event_log_suffix_var") of
      undefined -> get("event_log_prefix");
      SuffixVar -> get("event_log_prefix") ++ "." ++ vars:g_or_else(SuffixVar, "undefined")
    end
  end.

send_stats(Start, Delay, StrEventName) ->
  global:whereis_name(process_stats_monitor) ! {stats, get_log_filename(),
                                                integer_to_list(Start) ++ " "
                                                ++ integer_to_list(Delay) ++ " "
                                                ++ vars:get_name() ++ " "
                                                ++ StrEventName}.

stats_monitor() ->
  stats_monitor(undefined, []).

stats_monitor(FileName, Data) ->
  receive
    {stats, undefined, _} -> stats_monitor(FileName, Data);
    {stats, F, String} ->
      case FileName of
        undefined ->
          logger:log(0, "Using output stat file " ++ F);
        _ -> noop
      end,
      stats_monitor(F, [String | Data]);
    halt -> write_file(FileName, Data)
    after 200 ->
      write_file(FileName, Data),
      stats_monitor(FileName, [])
  end.

write_file(undefined, _) -> noop;
write_file(_, []) -> noop;
write_file(FileName, Data) ->
  {ok, Fd} = file:open(FileName, [write, append]),
  lists:map(fun(S) -> file:write(Fd, list_to_binary(S ++ "\n")) end, Data),
  file:close(Fd).
