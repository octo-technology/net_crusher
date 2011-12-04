## NetCrusher.
## Copyright (C) 2011 Bertrand Paquet, David Rousselie All Rights Reserved

## NetCrusher is free software; you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public
## License as published by the Free Software Foundation; either
## version 2.1 of the License, or (at your option) any later version.

## NetCrusher is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## Lesser General Public License for more details.

## You should have received a copy of the GNU Lesser General Public
## License along with this library; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

log 0, "testing loop"

loop 1, 10 do
  log 0, "k = #{g('k')}"
end

log 0, "testing while"

s 'z', 0

begin
  log 0, "z = #{g('z')}"
  s 'z', g('z') + 1
end while g('z') != 5

log 0, "test if"

if 1 < 2 then
  log 0, "ok"
end

if 1 > 2 then
  log 0, "ko"
end