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

-module(stats_tests).

-include_lib("eunit/include/eunit.hrl").

-export([setup/0,
         cleanup/1,
         test_read_write/0
        ]).

setup() ->
  mnesia:start(),
  stats:start_timestamp_handler().

cleanup(_) ->
  stats:stop_timestamp_handler(),
  mnesia:stop().

stats_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   [fun test_read_write/0]}.

test_read_write() ->
  stats:cmd_put_timestamp("Test"),
  case stats:get_timestamp("Test") of
    undefined -> throw({error, "undefined timestamp Test found"});
    _ -> noop
  end.
