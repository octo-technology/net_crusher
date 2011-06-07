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
-module(pmap).

-export([
  pmap/2
]).

pmap(Fun, List) ->
  Self = self(),
  Pids = lists:map(fun(I) ->
    spawn(fun() -> 
      pmap_f(Self, Fun, I)
    end
  ) end, List),
  pmap_gather(Pids).

pmap_gather([H|T]) ->
  receive
    {H, Ret} -> [Ret|pmap_gather(T)]
  end;
pmap_gather([]) ->
  [].

pmap_f(Parent, F, I) ->
  Parent ! {self(), (catch F(I))}.
