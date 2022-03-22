% Copyright (C) 2021-2022 Olivier Boudeville
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


% @doc Gathering of various facilities for <b>mesh</b> management, to define the
% geometry of 3D objects.
%
% See `mesh_test.erl' for the corresponding test.
%
-module(mesh).

% For the mesh record:
-include("mesh.hrl").


-type mesh() :: #mesh{}.
% Describes a mesh, convex or not.


-type indice() :: linear:indice().
% The indice of an element (ex: vertex, normal, texture coordinate), typically
% in a data container such as a list or a binary buffer.
%
% As always in Myriad, indices start at 1 (ex: as opposed to zero-based indexes
% such as glTF).


-type vertex_indice() :: indice().
% The indice of a vertex in a container thereof.


-type indexed_face() :: [ vertex_indice() ].
% Describes the face of a mesh, based on a list of vertices (for example 3 of
% them, to define a triangle).
%
% Note usually the vertex order matters (regarding culling).


-type indexed_triangle() ::
			{ vertex_indice(), vertex_indice(), vertex_indice() }.
% Made of the corresponding three vertices.
%
% Note that usually the vertex order matters (regarding culling).


-type normal_type() :: 'per_vertex' | 'per_face'.
% Defines to which geometric element a normal corresponds.


-type face_coloring_type() :: 'per_vertex' | 'per_face'.
% Defines how a coloring shall be applied to a face.


-type texture_coordinate2() :: point2:any_point2().
% A (2D) texture coordinate (hence with two U/V components); not a vector per
% se.


-type rendering_info() :: 'none'

		| { 'wireframe', EdgeColor :: render_rgb_color(),
			HiddenFaceRemoval :: boolean() }

		| { 'color', face_coloring_type(), [ render_rgb_color() ] }.
	  % | { 'texture', ...
% Defines how a mesh shall be rendered.

-type vertex_count() :: count().
-type normal_count() :: count().
-type edge_count() :: count().
-type face_count() :: count().


-export_type([ mesh/0, indice/0, vertex_indice/0,
			   indexed_face/0, indexed_triangle/0,
			   normal_type/0,
			   face_coloring_type/0, texture_coordinate2/0,
			   rendering_info/0,
			   vertex_count/0, normal_count/0, edge_count/0, face_count/0 ]).


% For the right_cuboid, sphere records and al:
-include("bounding_volume.hrl").


% Construction-related section.
-export([ create_mesh/5 ]).


% Operations on meshes.
-export([ indexed_face_to_triangle/1,
		  indexed_faces_to_triangles/1,
		  to_string/1, to_compact_string/1 ]).


% Color-related section.
%-export([ set_edge_color/2, get_edge_color/1,
%          set_fill_color/2, get_fill_color/1 ]).


% Bounding volume related section.
%-export([ update_bounding_volume/2 ]).


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().


-type render_rgb_color() :: gui_color:render_rgb_color().
%-type canvas() :: gui:canvas().

%-type distance() :: linear:distance().
%-type square_distance() :: linear:square_distance().

-type vertex3() :: point3:vertex3().

-type unit_normal3() :: vector3:unit_normal3().



% Construction-related section.


% @doc Returns a new mesh whose vertices, faces, normals (of specified type),
% rendering information are the specified ones, with no specific bounding volume
% set.
%
-spec create_mesh( [ vertex3() ], [ indexed_face() ], normal_type(),
				   [ unit_normal3() ], rendering_info() ) -> mesh().
create_mesh( Vertices, Faces, NormalType, Normals, RenderingInfo ) ->

	cond_utils:if_defined( myriad_check_mesh,
						   vector3:check_unit_vectors( Normals ) ),

	#mesh{ vertices=Vertices,
		   faces=Faces,
		   normal_type=NormalType,
		   normals=Normals,
		   rendering_info=RenderingInfo }.




% Exported helpers.


% @doc Returns the indexed triangle corresponding to the specified indexed face
% (thus expected to comprise 3 vertices).
%
-spec indexed_face_to_triangle( indexed_face() ) -> indexed_triangle().
indexed_face_to_triangle( _F=[ V1, V2, V3 ] ) ->
	{ V1, V2, V3 }.



% @doc Returns the indexed triangles corresponding to the specified indexed
% faces (thus expected to comprise 3 vertices each).
%
-spec indexed_faces_to_triangles( [ indexed_face() ] ) ->
												[ indexed_triangle() ].
indexed_faces_to_triangles( Faces ) ->
	[ indexed_face_to_triangle( F ) || F <- Faces ].





% Operations on meshes.



% @doc Returns a (rather full) textual description of the specified mesh.
-spec to_string( mesh() ) -> ustring().
to_string( #mesh{ vertices=Vertices,
				  faces=Faces,
				  normal_type=NormalType,
				  normals=Normals,
				  rendering_info=RenderingInfo,
				  bounding_volume=MaybeBoundingVolume } ) ->

	BVStr = case MaybeBoundingVolume of

		undefined ->
			"none available";

		BV ->
			bounding_volume:to_string( BV )

	end,

	text_utils:format( "mesh defined by:~n"
	  " - ~B vertices: ~w~n"
	  " - ~B faces: ~w~n"
	  " - ~B ~ts normals: ~w~n"
	  " - ~ts"
	  " - bounding volume: ~ts~n",
	  [ length( Vertices ), Vertices, length( Faces ), Faces,
		length( Normals ), normal_type_to_string( NormalType ), Normals,
		rendering_info_to_string( RenderingInfo ), BVStr ] ).



% @doc Returns a compact textual description of the specified mesh.
-spec to_compact_string( mesh() ) -> ustring().
to_compact_string( #mesh{ vertices=Vertices,
						  faces=Faces,
						  normal_type=NormalType,
						  normals=Normals,
						  rendering_info=RenderingInfo,
						  bounding_volume=MaybeBoundingVolume } ) ->

	BVStr = case MaybeBoundingVolume of

		undefined ->
			"none available";

		BV ->
			bounding_volume:to_string( BV )

	end,

	text_utils:format( "mesh with ~B vertices, ~B faces, "
		"~B ~ts normals, with ~ts and ~ts~n",
	  [ length( Vertices ), length( Faces ), length( Normals ),
		normal_type_to_string( NormalType ),
		rendering_info_to_compact_string( RenderingInfo ), BVStr ] ).



% @doc Returns a (rather full) textual description of the specified rendering
% information.
%
-spec rendering_info_to_string( rendering_info() ) -> ustring().
rendering_info_to_string( _RI=none ) ->
	"no rendering set";

rendering_info_to_string( _RI={ wireframe, HiddenFaceRemoval } ) ->
	"wireframe rendering (" ++ case HiddenFaceRemoval of
									true -> "";
									false -> "no "
							   end ++ "hidden-face removal";

rendering_info_to_string( _RI={ color, ColoringType, Colors } ) ->
	case ColoringType of
		per_vertex -> "per-vertex";
		per_face -> "per-face"
	end ++ text_utils:format( " rendering with ~B colors: ~w",
							  [ length( Colors ), Colors ] ).



% @doc Returns a compact textual description of the specified rendering
% information.
-spec rendering_info_to_compact_string( rendering_info() ) -> ustring().
rendering_info_to_compact_string( _RI=none ) ->
	"no rendering set";

rendering_info_to_compact_string(
						_RI={ wireframe, _HiddenFaceRemoval=true } ) ->
	"culled wireframe rendering";

rendering_info_to_compact_string(
						_RI={ wireframe, _HiddenFaceRemoval=false } ) ->
	"unculled wireframe rendering";

rendering_info_to_compact_string( _RI={ color, ColoringType, Colors } ) ->
	case ColoringType of
		per_vertex -> "per-vertex";
		per_face -> "per-face"
	end ++ text_utils:format( " rendering with ~B colors: ~w",
							  [ length( Colors ), Colors ] ).




-spec normal_type_to_string( normal_type() ) -> ustring().
normal_type_to_string( per_vertex ) ->
	"per-vertex";

normal_type_to_string( per_face ) ->
	"per-face".



% Bounding-volume related section.


% at-doc Updates, for the specified mesh, its internal bounding volume, with
% regard to the specified bounding-volume algorithm.
%
% Returns a mesh with updated information.
%
% at-end
%
% The lazy bounding sphere is fast to determine, but not optimal:
%-spec update_bounding_volume( bounding_algorithm(), mesh() ) -> mesh().
%update_bounding_volume( lazy_sphere, Mesh ) ->

%	SphereBVolume = bounding_volume:get_lazy_sphere( Mesh#mesh.vertices ),

%	Mesh#mesh{ bounding_volume=SphereBVolume }.




% Helper functions.
