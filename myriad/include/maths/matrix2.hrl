% Copyright (C) 2003-2022 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% The classical, canonical 2x2 matrix representation is:
%
% M = | M11 M12 |
%     | M21 M22 |
%
% This corresponds to this record specialised for 2D. Coordinates are listed row
% per row.
%
% A shorthand for these record and corresponding type could be m2.
%
-record( matrix2, {

  m11 :: linear:coordinate(),
  m12 :: linear:coordinate(),

  m21 :: linear:coordinate(),
  m22 :: linear:coordinate() } ).


% No relevant compact_matrix2 to be defined.
