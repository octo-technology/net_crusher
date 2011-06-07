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
-module(sampler_tests).

-include_lib("eunit/include/eunit.hrl").

-export([setup/0,
         cleanup/1,
         test_distribution/0,
         test_two_distribution/0
        ]).

setup() ->
  runtime:start_spawn_with_monitor(),
  sampler:cmd_start_sampler("10", "0,20,40,20,10,10,0").

cleanup(_) ->
  sampler:stop(),
  runtime:stop_spawn_with_monitor().

sampler_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   [fun test_distribution/0,
    fun test_two_distribution/0]}.

test_distribution() ->
  1000 = sampler:int_get_new_player_answer_delay(),
  1000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  3000 = sampler:int_get_new_player_answer_delay(),
  3000 = sampler:int_get_new_player_answer_delay(),
  4000 = sampler:int_get_new_player_answer_delay(),
  5000 = sampler:int_get_new_player_answer_delay().

test_two_distribution() ->
  1000 = sampler:int_get_new_player_answer_delay(),
  1000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  3000 = sampler:int_get_new_player_answer_delay(),
  3000 = sampler:int_get_new_player_answer_delay(),
  4000 = sampler:int_get_new_player_answer_delay(),
  5000 = sampler:int_get_new_player_answer_delay(),
  %% second distribution
  1000 = sampler:int_get_new_player_answer_delay(),
  1000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  2000 = sampler:int_get_new_player_answer_delay(),
  3000 = sampler:int_get_new_player_answer_delay(),
  3000 = sampler:int_get_new_player_answer_delay(),
  4000 = sampler:int_get_new_player_answer_delay(),
  5000 = sampler:int_get_new_player_answer_delay().
