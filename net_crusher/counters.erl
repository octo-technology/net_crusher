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
-module(counters).

-export([
  cmd_counter_incr/1,
  int_counter_read/1,
  
  counter_manager/0,
  
  start_counter_manager/0,
  stop_counter_manager/0
]).

cmd_counter_incr(StrCounterName) ->
  global:whereis_name(counter_manager) ! {counter_incr, StrCounterName}.

start_counter_manager() ->
  global:register_name(counter_manager,
                      runtime:spawn_with_monitor(node(), runtime,
                                                  spawn_with_name,
                                                  ["CounterManager",
                                                   fun() -> counters:counter_manager() end])).

int_counter_read(StrCounterName) ->
  tools:sync_msg(global:whereis_name(counter_manager), counter_value, counter_read, StrCounterName).
  
stop_counter_manager() ->
  global:whereis_name(counter_manager) ! halt.
  
counter_manager() ->
  counter_manager(dict:new()).
  
counter_manager(Dict) ->
  receive
    {counter_incr, CounterName} ->
      Value = case dict:find(CounterName, Dict) of
        error -> 0;
        {ok, V} -> V
      end,
      counter_manager(dict:store(CounterName, Value + 1, Dict));
    {counter_read, Target, CounterName} ->
      Result = case dict:find(CounterName, Dict) of
        error -> 0;
        {ok, V} -> V
      end,
      Target ! {counter_value, Result},
      counter_manager(Dict);
    halt ->
      noop
  end.
                                                