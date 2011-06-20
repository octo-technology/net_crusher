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
-module(statment).

-export([
  do_while/2,

  make_call/2,

  loop/3
]).

make_call(StrModule, StrFunction) ->
  erlang:apply(list_to_atom(StrModule), list_to_atom("str_" ++ StrFunction), []).

do_while(Cond, Blk) ->
  Blk(),
  case Cond() of
    true -> do_while(Cond, Blk);
    false -> noop
  end.

loop(IntFrom, IntTo, _Blk) when IntFrom > IntTo -> ok;
loop(IntFrom, IntTo, Blk) ->
  Blk(IntFrom),
  loop(IntFrom + 1, IntTo, Blk).
