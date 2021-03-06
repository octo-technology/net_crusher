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

log 0, "int"

log 0, 1
log 0, 1 + 2
log 0, 1+2
log 0, 1 - 2
log 0, 0 + 1

log 0, "boolean"

log 0, 0 == 1
log 0, 1 == 1
log 0, 0 != 1
log 0, 1 != 1
log 0, 1 > 1
log 0, 1 > 0

log 0, ! 0 != 1
log 0, ! 1 != 1

log 0, "boolean"

log 0, "a" == "a"
log 0, "a" != "a"
log 0, "a" != "b"

a = "a"

log 0, "a" == a
log 0, a == "a"  

log 0, 3 * 14

log 0, 4 / 3
log 0, 5 / 3
log 0, 4 / 2