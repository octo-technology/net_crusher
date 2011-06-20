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

Logger.log 0, "here is root #{Vars.get_name()}"

Logger.set_log_level 1

Vars.s "worker_name", "worker#{Vars.get_name()}_1"

Worker.start Vars.g("worker_name"), "a.exs"

Misc.sleep_ms 200

Worker.trigger Vars.g("worker_name")
Worker.kill Vars.g("worker_name")

Misc.sleep_ms 200

Runtime.stop
