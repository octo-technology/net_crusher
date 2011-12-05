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

main(Args) ->
  [BinDirectory, OutputFile | Modules] = Args,
  code:add_path(BinDirectory),
  {ok, IoDevice} = file:open(OutputFile, [write]),
  io:format(IoDevice, "~p", [erlang_functions_scanner:scan_modules(to_atoms(Modules))]),
  file:close(IoDevice).
  
  
to_atoms([H | T]) -> [list_to_atom(H) | to_atoms(T)];
to_atoms([]) -> [].
