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
Runtime.run("fake")

Logger.log 0, "testing loop"

Statment.loop(1, 10, do (k)
  Logger.log 0, "k = #{k}"
end)

Logger.log 0, "testing while"

Vars.s 'z, 0

Statment.do_while -> Vars.g('z) != 5, do
  Logger.log 0, "z = #{Vars.g('z)}"
  Vars.s 'z, Vars.g('z) + 1
end

Logger.log 0, "test if"

if 1 < 2
  Logger.log 0, "ok"
end

if 1 > 2
  Logger.log 0, "ko"
end

Runtime.stop
