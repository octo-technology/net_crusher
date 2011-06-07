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
-module(tools).

-export([
  dehex/1,
  md5_hex/1,
  micro_timestamp/0,
  init_rand/1,
  sync_msg/4,
  stop_process/1
  ]).

-include("macros.hrl").

dehex(S) ->
  {Res, _} = lists:foldr(fun(Char, {A, B}) ->
    {A + B * dehex_char(Char), B * 16}
  end,
  {0, 1},
  S),
  Res.

dehex_char(C) when C >= $0, C =< $9 ->
    C - $0;
dehex_char(C) when C >= $a, C =< $f ->
    C - $a + 10;
dehex_char(C) when C >= $A, C =< $F ->
    C - $A + 10.

md5_hex(S) ->
  Md5_bin =  erlang:md5(S),
  Md5_list = binary_to_list(Md5_bin),
  lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
  lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $a + (N-10).

micro_timestamp() ->
  {MegaSecs, Secs, Micros} = now(),
  MegaSecs * 1000000000000 + Secs * 1000000 + Micros.

init_rand(Str) ->
  {_,A2,A3} = now(),
  random:seed(erlang:phash(list_to_atom(Str), 132735487), A2, A3).

sync_msg(Name, ResponseId, Command, Args) ->
  ?TIME(sync_msg_(Name, ResponseId, Command, Args), [sync_msg, ResponseId, Command]).

sync_msg_(Name, ResponseId, Command, Args) ->
  Name ! {Command, self(), Args},
  receive
    {ResponseId, Data} -> Data
  end.

stop_process(ProcessName) ->
  case lists:member(ProcessName, global:registered_names()) of
    true ->
      global:whereis_name(ProcessName) ! halt,
      global:unregister_name(ProcessName);
    _ -> noop
  end.
