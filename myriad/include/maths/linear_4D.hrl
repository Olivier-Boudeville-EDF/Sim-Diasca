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


% The classical, canonical 4x4 matrix representation is:
%
% M = [ M11, M12, M13, M14 ]
%     [ M21, M22, M23, M24 ]
%     [ M31, M32, M33, M34 ]
%     [ M41, M42, M43, M44 ]
%
-record( mat4, {

  m11 :: linear:coordinate(),
  m12 :: linear:coordinate(),
  m13 :: linear:coordinate(),
  m14 :: linear:coordinate(),

  m21 :: linear:coordinate(),
  m22 :: linear:coordinate(),
  m23 :: linear:coordinate(),
  m24 :: linear:coordinate(),

  m31 :: linear:coordinate(),
  m32 :: linear:coordinate(),
  m33 :: linear:coordinate(),
  m34 :: linear:coordinate(),

  m41 :: linear:coordinate(),
  m42 :: linear:coordinate(),
  m43 :: linear:coordinate(),
  m44 :: linear:coordinate()

} ).



% A 4x4 compact matrix is a 3x4 canonical matrix, or, said otherwise, a 3x3
% matrix M3 and a vector T, as:
%
% M3 = [ M11, M12, M13 ]
%      [ M21, M22, M23 ]
%      [ M31, M32, M33 ]
%
% and T = [ Tx, Ty, Tz ].
%
% This corresponds to, by blocks:
%
% M = [ M3, T ]
%     [  0, 1 ]
%
%   = [ M11, M12, M13, Tx ]
%     [ M21, M22, M23, Ty ]
%     [ M31, M32, M33, Tz ]
%     [   0,   0,   0,  1 ]
%
%
% Could have been, with more indirections:
%
% { linear_3D:canonical_matrix(), linear_3D:vector() }
%
-record( cpt_mat4, {

  m11 :: linear:coordinate(),
  m12 :: linear:coordinate(),
  m13 :: linear:coordinate(),
  tx  :: linear:coordinate(),

  m21 :: linear:coordinate(),
  m22 :: linear:coordinate(),
  m23 :: linear:coordinate(),
  ty  :: linear:coordinate(),

  m31 :: linear:coordinate(),
  m32 :: linear:coordinate(),
  m33 :: linear:coordinate(),
  tz  :: linear:coordinate()

} ).
