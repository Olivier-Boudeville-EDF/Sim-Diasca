% Copyright (C) 2021-2023 Olivier Boudeville
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
% Creation date: Saturday, November 13, 2021.


% Describes a mesh, convex or not.
-record( mesh, {

	% The points defining that mesh:
	vertices = [] :: [ point3:any_vertex3() ],


	% The faces defining that mesh, based on the indices of vertices:
	faces = [] :: [ mesh:indexed_face() ],


	% To which geometric element (ex: per vertex, per face) the next normals
	% correspond:
	%
	normal_type :: mesh:normal_type(),

	% The (unit) normals (if any) of that mesh, defined according to the
	% previous normal type:
	%
	normals = [] :: [ vector3:unit_normal3() ],


	% How this mesh shall be rendered:
	rendering_info = none :: mesh:rendering_info(),


	% Bounding volume information:
	% (can be for example a right-cuboid or a sphere)
	%
	bounding_volume :: maybe( bounding_volume:bounding_volume() ) } ).
