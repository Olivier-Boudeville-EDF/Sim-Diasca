% Copyright (C) 2010-2024 Olivier Boudeville
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
% Creation date: 2010.


% Common definitions about bounding volumes.



% Record declarations:


% Right cuboid-based 3D bounding box (i.e. rectangular parallelepiped).
%
% Each of its faces is either parallel or orthogonal to each of the canonical
% axes.
%
% If base_vertex={X,Y,Z}, abscissa_length is A, ordinate_length is B,
% elevation_length is C, then this cuboid is made of all points {Xp,Yp,Zp}
% where:
%  - X <= Xp < X + A
%  - Y <= Yp < X + B
%  - Z <= Zp < Z + C
%
% See http://en.wikipedia.org/wiki/Cuboid
%
-record( right_cuboid, {

	% The base vertex of the cuboid, its bottom-left-near vertex:
	base_vertex :: point3:point3(),

	% The length along the abscissa axis (X):
	abscissa_length :: linear:distance(),

	% The length along the ordinate axis (Y):
	ordinate_length :: linear:distance(),

	% The length along the elevation axis (Z):
	elevation_length :: linear:distance() } ).



% Sphere-based bounding volume.
-record( sphere, {

	% The center of the sphere:
	center :: point3:point3(),

	% The square of the radius (R^2) of this sphere:
	square_radius :: linear:square_distance() } ).
