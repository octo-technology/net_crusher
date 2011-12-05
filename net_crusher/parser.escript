#!/usr/bin/env escript
%% -*- erlang -*-
%%! +c +K true
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
main([FileName]) ->
  ScriptName = escript:script_name(),
  RealFileName = case file:read_link(ScriptName) of
    {error, _} -> ScriptName;
    {ok, F} -> F
  end,
  ScriptDir = filename:dirname(RealFileName),
  code:add_path(ScriptDir),
  io:fwrite("Result of lexing and post processing ~s\n~p\n", [FileName, ruby:post_process(ruby:scan_file(FileName))]),
  io:fwrite("Result of parsing ~s\n~p\n", [FileName, ruby:parse_file(FileName)]).
