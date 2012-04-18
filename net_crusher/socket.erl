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
-module(socket).

-export([
  cmd_open_socket/2,
  cmd_write_socket/1,
  cmd_close_socket/0,
  str_read_socket_string/0
]).

cmd_open_socket(StrHostname, IntPort) ->
  Socket = tools_http:open_socket(http, StrHostname, IntPort),
  put(current_socket, Socket).

cmd_close_socket() ->
  Socket = get(current_socket),
  tools_http:close_socket(Socket).

cmd_write_socket(StrData) ->
  Socket = get(current_socket),
  tools_http:send_socket(Socket, StrData).

str_read_socket_string() ->
  Socket = get(current_socket),
  {ok, Result} = tools_http:read_http_data_binary(Socket),
  binary_to_list(Result).
