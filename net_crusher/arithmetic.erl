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
-module(arithmetic).

-export([
  int_add/2,
  int_sub/2,
  int_mul/2,
  int_div/2,
  bool_int_lesser/2,
  bool_int_greater/2,
  bool_str_equal/2,
  bool_str_different/2,
  bool_not/1
  ]).

bool_not(B) -> not B.
bool_int_lesser(I1, I2) -> I1 < I2.
bool_int_greater(I1, I2) -> I1 > I2.
bool_str_equal(S1, S2) -> S1 == S2.
bool_str_different(S1, S2) -> S1 /= S2.
int_add(I1, I2) -> I1 + I2.
int_sub(I1, I2) -> I1 - I2.
int_mul(I1, I2) -> I1 * I2.
int_div(I1, I2) -> erlang:trunc(I1 / I2).
