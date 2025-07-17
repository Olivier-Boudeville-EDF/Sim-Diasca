% Copyright (C) 2021-2025 Olivier Boudeville
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
%
% Refer to mesh_render.hrl for rendering-related topics.
%
-record( mesh, {

	% The points defining that mesh, as a repository thereof:
	vertices :: mesh:vertex_repository(),

	% The types of the faces from which this mesh is made (e.g. triangle, quad):
	face_type :: mesh:face_type(),


	% The faces defining that mesh, as a repository thereof, based on the
	% indices of vertices:
	%
	% (all faces of a mesh are expected to have the same number of vertices)
	%
	faces = [] :: [ mesh:indexed_face() ],


	% To which geometric element (e.g. per vertex, per face) the next normals
	% (if any) correspond:
	%
	normal_type :: mesh:normal_type(),

	% The (unit) normals (if any) of that mesh, defined according to the
	% previous normal type (normal indices corresponding thus either to vertex
	% or face ones).
	%
	% (as tessellation induces the sharing of normals of the tessellating
	% triangles, normals should be indexed as well)
	%
	% Normals are optional, as they are mostly used for lighting, and mostly
	% useful if they are per-vertex (per-face normals can be deduced from the
	% vertices of that face)
	%
	% Normals are stored literally (rather than as identifiers pointing to some
	% mesh:normal_repository()), as they are expected to be seldom fetched based
	% on random access, but more iterated through).
	%
	normals = undefined ::
		option( type_utils:tuploid( vector3:unit_normal3() ) ),


	% How this mesh shall be rendered:
	rendering_info = none :: mesh_render:rendering_info(),

	% Any corresponding (OpenGL-level) rendering state:
	rendering_state :: option( mesh_render:rendering_state() ),


	% Bounding volume information:
	%
	% (can be for example a right-cuboid or a sphere)
	%
	bounding_volume :: option( bounding_volume:bounding_volume() ) } ).
