% Copyright (C) 2003-2021 Olivier Boudeville
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


% The classical, canonical 3x3 matrix representation is:
%
% M = [ M11, M12, M13 ]
%     [ M21, M22, M23 ]
%     [ M31, M32, M33 ]
%
-record( mat3, {

  m11 :: linear:coordinate(),
  m12 :: linear:coordinate(),
  m13 :: linear:coordinate(),

  m21 :: linear:coordinate(),
  m22 :: linear:coordinate(),
  m23 :: linear:coordinate(),

  m31 :: linear:coordinate(),
  m32 :: linear:coordinate(),
  m33 :: linear:coordinate()

} ).



% A 3x3 compact matrix is a 2x3 canonical matrix, or, said otherwise, a 2x2
% matrix M2 and a vector T, as:
%
% M2 = [ M11, M12 ]
%      [ M21, M22 ]
%
% and T = [ Tx, Ty ].
%
% This corresponds to, by blocks:
%
% M = [ M2, T ]
%     [  0, 1 ]
%
%   = [ M11, M12, Tx ]
%     [ M21, M22, Ty ]
%     [   0,   0,  1 ]
%
%
% Could have been, with more indirections:
%
% { linear_2D:canonical_matrix(), linear_2D:vector() }
%
-record( cpt_mat3, {

  m11 :: linear:coordinate(),
  m12 :: linear:coordinate(),
  tx  :: linear:coordinate(),

  m21 :: linear:coordinate(),
  m22 :: linear:coordinate(),
  ty  :: linear:coordinate()

} ).
