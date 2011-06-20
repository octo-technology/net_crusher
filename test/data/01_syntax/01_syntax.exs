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

Logger.log(0, "quote and simple quote")

Vars.s "toto", "value"
Logger.log 0, Vars.g("toto")

Vars.s 'toto, "value"
Logger.log 0, Vars.g('toto)

Logger.log 0, "int to string"
Logger.log 0, "1"

Logger.log 0, "inside double quote"
Logger.log 0, "a_#{1 + 2}_b"
Logger.log 0, "a_#{Vars.g('toto)}_b"
Logger.log 0, "#{1 + 2} #{Vars.g('toto)}"

Vars.s 'int, 1234
Logger.log 0, "a_#{Vars.g('int) + 10}_b"

Runtime.stop
