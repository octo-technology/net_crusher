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

log 0, "quote and simple quote"

s "toto", "value"
log 0, g("toto")

s 'toto', 'value'
log 0, g('toto')

log 0, "int to string"
log 0, "1"

log 0, "inside double quote"
log 0, "a_#{1 + 2}_b"
log 0, "a_#{g('toto')}_b"
log 0, "#{1 + 2} #{g('toto')}"

s "int", 1234
log 0, "a_#{g('int') + 10}_b"

logf 0, "array=~p, int=~p, str=~s.", [["a", "b"], 1, "test"]
