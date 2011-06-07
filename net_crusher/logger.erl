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
-module(logger).

-export([
  cmd_log/2,
  cmd_logf/3,
  cmd_set_log_level/1
]).

cmd_logf(IntLogLevel, StrFormat, ArrArgs) ->
  log(IntLogLevel, vars:str_g_or_else("name", "no process name"),
      StrFormat, ArrArgs).

cmd_log(IntLogLevel, StrMessage) ->
  cmd_logf(IntLogLevel, "~s", [StrMessage]).

cmd_set_log_level(IntLogLevel) ->
  log(0, "Logger", "Set log level : ~p", [IntLogLevel]),
  vars:cmd_s("log_level", IntLogLevel).

log(AskedLogLevel, Name, Format, Args) ->
  LogLevel = vars:str_g_or_else("log_level", 0),
  if AskedLogLevel =< LogLevel ->
      io:fwrite("~17.3f [~25s][~20s] " ++ Format ++ "\n", [tools:micro_timestamp() / 1000,
                                                           atom_to_list(node()), Name] ++ Args);
    true -> noop
  end.
