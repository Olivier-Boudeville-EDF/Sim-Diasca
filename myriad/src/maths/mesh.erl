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

-module(mesh).

-moduledoc """
Gathering of various facilities for **mesh** management, to define the geometry
of 3D objects.

Refer to the `mesh_render` module for the rendering of meshes.

See `mesh_test.erl` for the corresponding test.
""".


% For the mesh record:
-include("mesh.hrl").


-doc "Describes a mesh, convex or not.".
-type mesh() :: #mesh{}.



-doc """
The indice of an element (e.g. vertex, normal, texture coordinate), typically in
a data container such as a list, an array or a binary buffer.

As always in Myriad, indices start at 1 (e.g. as opposed to zero-based indexes
such as glTF).
""".
-type indice() :: linear:indice().



-doc "The index of a vertex (typically a point3()) in an indexed face.".
-type vertex_indice() :: indice().



-doc """
A repository of vertices, typically to be referenced based on their indice.
""".
-type vertex_repository() :: array( vertex3() ).



-doc """
The type of the faces of a mesh.

'arbitrary' could be supported later.
""".
-type face_type() :: 'triangle' | 'quad'.



-doc """
Describes a face of a mesh, based on an (ordered) tuple of vertices (at least
three of them), specified as indexes (for example 3 of them, to define a
triangle).

Note that usually the vertex order matters (regarding culling).
""".
-type indexed_face() :: tuple( vertex_indice() ).



-doc "The index of a face in a container thereof.".
-type face_indice() :: indice().



-doc """
Made of the corresponding three vertices.

Note that usually the vertex order matters (regarding culling).
""".
-type indexed_triangle() ::
	{ vertex_indice(), vertex_indice(), vertex_indice() }.



-doc """
Made of the corresponding four vertices.

Note that usually the vertex order matters (regarding culling).
""".
-type indexed_quad() ::
	{ vertex_indice(), vertex_indice(), vertex_indice(), vertex_indice() }.



-doc "Elements that may be referenced by a mesh.".
-type mesh_element() :: vertex3() | unit_normal3()
					  | mesh_render:render_element().



-doc "Defines to which geometric element a normal corresponds.".
-type normal_type() :: 'per_vertex' | 'per_face'.



-doc "A number of vertices.".
-type vertex_count() :: count().



-doc "A number of normals.".
-type normal_count() :: count().



-doc "A number of edges.".
-type edge_count() :: count().



-doc "A number of faces.".
-type face_count() :: count().



-export_type([ mesh/0, indice/0, vertex_indice/0,
			   face_type/0, indexed_face/0, face_indice/0,
			   indexed_triangle/0, indexed_quad/0,
			   mesh_element/0, normal_type/0,
			   vertex_count/0, normal_count/0, edge_count/0, face_count/0 ]).


% For the right_cuboid, sphere records and al:
-include("bounding_volume.hrl").


% Construction-related section.
-export([ create/2, create/3, create/4, create/6 ]).


% Operations on meshes.
-export([ tessellate/1, triangulate/1,
		  compute_normals/1,
		  indexed_face_to_triangle/1,
		  indexed_faces_to_triangles/1,

		  to_string/1, to_compact_string/1,
		  face_type_to_string/1, normal_type_to_string/1 ]).



% Other operations, lower-level:
-export([ set_vertices/2,
		  compute_normal/2,
		  get_vertex_count_for_face_type/1, get_face_type/1,
		  check_faces/2, check_face/2, check_indice/1,
		  get_vertex_from_id/2, get_vertices_from_ids/2,
		  get_element_from_id/2, get_elements_from_ids/2 ]).


% Bounding volume related section.
%-export([ update_bounding_volume/2 ]).



% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type tuple( T ) :: type_utils:tuple( T ).
-type array( T ) :: array:array( T ).

-type vertex3() :: point3:vertex3().

-type unit_normal3() :: vector3:unit_normal3().

-type rendering_info() :: mesh_render:rendering_info() .



% Implementation notes:

% Regarding data-structures.
%
% Mesh vertices used to be stored as [point3:vertex3()], yet most operations
% (e.g. resolving faces) require random access, so arrays and maps are more
% suitable, at least for large enough geometries. They should have quite similar
% performances, but, keys being positive integers, arrays seem more relevant,
% notably because they should be more compact in memory.



% Construction-related section.


-doc """
Returns a new mesh whose vertices and faces are the specified ones, with no
specific normal, rendering information or bounding volume set.

Determines automatically the face type, assuming it is homogeneous across all
the declared faces.
""".
-spec create( [ vertex3() ], [ indexed_face() ] ) -> mesh().
create( _Vertices, _Faces=[] ) ->
	throw( no_face_declared );

create( Vertices, Faces=[ F | _T ] ) ->
	create( Vertices, get_face_type( F ), Faces, _RenderingInfo=none ).



-doc """
Returns a new mesh whose vertices and faces are the specified ones, with no
specific normal, rendering information or bounding volume set.
""".
-spec create( [ vertex3() ], face_type(), [ indexed_face() ] ) -> mesh().
create( Vertices, FaceType, Faces ) ->
	create( Vertices, FaceType, Faces, _RenderingInfo=none ).



-doc """
Returns a new mesh whose vertices, faces and rendering information are the
specified ones, with no specific normal or bounding volume set.
""".
-spec create( [ vertex3() ], face_type(), [ indexed_face() ],
			  rendering_info() ) -> mesh().
create( Vertices, FaceType, Faces, RenderingInfo ) ->
	create( Vertices, FaceType, Faces, _AnyNormalType=per_face,
			_MaybeNormals=undefined, RenderingInfo ).



-doc """
Returns a new mesh whose vertices, faces, normals (of unit length, and of the
specified type; if any), rendering information are the specified ones, with no
specific bounding volume set.
""".
-spec create( [ vertex3() ], face_type(), [ indexed_face() ],
	normal_type(), option( [ unit_normal3() ] ), rendering_info() ) -> mesh().
create( Vertices, FaceType, Faces, NormalType, MaybeNormals, RenderingInfo ) ->

	cond_utils:if_defined( myriad_check_mesh,
		begin
			check_faces( Faces, get_vertex_count_for_face_type( FaceType ) ),

			MaybeNormals =:= undefined orelse
				begin
					vector3:check_unit_vectors( MaybeNormals ),
					NormalCount = length( MaybeNormals ),
					case NormalType of

						per_vertex ->
							% Assuming all vertices are involved in at least a
							% face:
							%
							VertexCount = array:size( Vertices ),
							basic_utils:assert_equal( NormalCount,
													  VertexCount );

						per_face ->
							FaceCount = length( Faces ),
							basic_utils:assert_equal( NormalCount,
													  FaceCount )

					end
				end
		end ),

	CanonRenderingInfo = mesh_render:canonicalise_rendering_info( RenderingInfo,
																  Faces ),

	#mesh{ vertices=array:from_list( Vertices ),
		   face_type=FaceType,
		   faces=Faces,
		   normal_type=NormalType,
		   normals=MaybeNormals,
		   rendering_info=CanonRenderingInfo }.



-doc """
Returns the number of vertices per face to be expected for the specified face
type.
""".
-spec get_vertex_count_for_face_type( face_type() ) -> count().
get_vertex_count_for_face_type( _FaceType=triangle ) ->
	3;

get_vertex_count_for_face_type( _FaceType=quad ) ->
	4.



-doc "Returns the face type corresponding to the specified (indexed) face.".
-spec get_face_type( indexed_face() ) -> face_type().
get_face_type( IndexedFace ) when is_tuple( IndexedFace )
								  andalso size( IndexedFace ) =:= 3 ->
	triangle;

get_face_type( IndexedFace ) when is_tuple( IndexedFace )
								  andalso size( IndexedFace ) =:= 4 ->
	quad.



-doc """
Checks whether the specified term is a legit list of faces, and returns it.
""".
-spec check_faces( term(), vertex_count() ) -> [ indexed_face() ].
check_faces( Faces, VCount ) when is_list( Faces ) ->
	[ check_face( F, VCount ) || F <- Faces ];

check_faces( Other, _VCount ) ->
	throw( { invalid_faces, Other } ).



-doc "Checks whether the specified term is a legit face, and returns it.".
-spec check_face( term(), vertex_count() ) -> indexed_face().
check_face( Face, VCount )
		when is_tuple( Face ) andalso size( Face ) =:= VCount ->
	[ check_indice( VId ) || VId <- tuple_to_list( Face ) ];

check_face( Other, _VCount ) ->
	throw( { invalid_faces, Other } ).



-doc "Checks whether the specified term is a legit indice, and returns it.".
-spec check_indice( term() ) -> indice().
check_indice( Id ) when is_integer( Id ) andalso Id > 0 ->
	Id;

check_indice( Other ) ->
	throw( { invalid_indice, Other } ).




% Exported helpers.


-doc """
"Tessellates" the specified mesh: returns a version of it whose faces are
triangles (e.g. not quads).
""".
-spec tessellate( mesh() ) -> mesh().
tessellate( Mesh=#mesh{ face_type=triangle } ) ->
	Mesh;

tessellate( Mesh=#mesh{ face_type=quad,
						faces=QuadFaces,
						%normal_type=NormalType,
						%normals=MaybeQuadNormals,
						rendering_info=QuadRendInfo } ) ->

	% (a list comprehension would not suffice)
	TrigFaces = triangulate( QuadFaces ),

	% MaybeTrigNormals = case MaybeQuadNormals of

	%	undefined ->
	%		undefined;

	%	QuadNormals ->
	%		case NormalType of

	%			per_vertex ->
	%				% TODO: convert 4 normals in 2*3 ones:
	%				...( QuadNormals);

	%			per_face ->
	%				% The two triangles have the same normal as their quad:
	%				list_utils:repeat_elements( QuadNormals, _Count=2 )

	%		end

	% end,
	MaybeTrigNormals = undefined,

	TrigRendInfo = mesh_render:tessellate_rendering_info( _FaceType=quad,
														  QuadRendInfo ),

	Mesh#mesh{ face_type=triangle,
			   faces=TrigFaces,
			   normals=MaybeTrigNormals,
			   rendering_info=TrigRendInfo }.



-doc """
Converts each quad face (an indexed_quad(), defined by vertex indices
{V1,V2,V3,V4}) into two triangle faces (indexed_triangle()): {V1,V2,V3} and
{V3,V4,V1} (thus preserving the original conventional order).

So the length of the returned (triangle) list is twice as long as the one of the
specified (quad) one.
""".
-spec triangulate( [ indexed_quad() ] ) -> [ indexed_triangle() ].
triangulate( QuadFaces ) ->
	% (a list comprehension would not suffice; reversing earlier than later is
	% cheaper)
	%
	triangulate( lists:reverse( QuadFaces ), _Acc=[] ).


% (helper)
triangulate( _RevQuadFaces=[], Acc ) ->
	% Reversing already done:
	Acc;

triangulate( _RevQuadFaces=[ _QF={ V1Id, V2Id, V3Id, V4Id } | T ], Acc ) ->
	NewAcc = [ { V1Id, V2Id, V3Id }, { V3Id, V4Id, V1Id } | Acc ],
	triangulate( T, NewAcc ).



-doc """
Returns a mesh whose vertices are the specified ones.

No consistency checked with the other information held by this mesh.
""".
-spec set_vertices( [ vertex3() ], mesh() ) -> mesh().
set_vertices( NewVertices, Mesh ) ->
	Mesh#mesh{ vertices=array:from_list( NewVertices ) }.



-doc """
Updates the specified mesh by computing automatically the "basic" per-face
normals that can be deduced from the vertices of each face of this mesh,
replacing any previously defined normals.

Vertex ordering is supposed to respect Myriad's conventions (CCW from outside),
so that the added normals are pointing outward.
""".
-spec compute_normals( mesh() ) -> mesh().
compute_normals( Mesh=#mesh{ vertices=Vertices,
							 faces=Faces }) ->
	Normals = [ compute_normal( F, Vertices ) || F <- Faces ],
	Mesh#mesh{ normals=Normals }.



-doc """
Updates the specified mesh by computing automatically the "basic" per-face
normals that can be deduced from the vertices of each face of this mesh,
replacing any previously defined normals.

Vertex ordering is supposed to respect Myriad's conventions (CCW from outside),
so that the added normals are pointing outward.
""".
-spec compute_normal( indexed_face(), [ vertex3() ] ) -> unit_normal3().
compute_normal( Face, AllVertices ) ->

	% For that we fetch the first three vertices of that face.

	% A face being a tuple of vertex indices:

	V1Id = element( _FirstIndex=1, Face ),
	V2Id = element( 2, Face ),
	V3Id = element( 3, Face ),

	[ V1, V2, V3 ] = get_elements_from_ids( [ V1Id, V2Id, V3Id ], AllVertices ),

	vector3:compute_normal( V1, V2, V3 ).



-doc """
Returns the indexed triangle corresponding to the specified indexed face (thus
expected to comprise 3 vertices).
""".
-spec indexed_face_to_triangle( indexed_face() ) -> indexed_triangle().
indexed_face_to_triangle( _F=[ V1, V2, V3 ] ) ->
	_IT={ V1, V2, V3 }.



-doc """
Returns the indexed triangles corresponding to the specified indexed faces (thus
expected to comprise 3 vertices each).
""".
-spec indexed_faces_to_triangles( [ indexed_face() ] ) ->
												[ indexed_triangle() ].
indexed_faces_to_triangles( Faces ) ->
	[ indexed_face_to_triangle( F ) || F <- Faces ].




% Basic helpers.


-doc """
Returns the vertex of the specified indice relative to the specified referenced
vertex repository.
""".
-spec get_vertex_from_id( indice(), vertex_repository() ) -> vertex3().
get_vertex_from_id( Id, VRepository ) ->
	% As array indices start at zero:
	array:get( Id-1, VRepository ).



-doc """
Returns the vertices of the specified indices relative to the specified
referenced vertex repository.
""".
-spec get_vertices_from_ids( [ indice() ], vertex_repository() ) ->
											[ vertex3() ].
get_vertices_from_ids( Ids, VRepository ) ->
	[ get_vertex_from_id( Id, VRepository ) || Id <- Ids ].



-doc """
Returns the element (e.g vertex, color, texture coordinates) of the specified
indice relative to the specified referenced mesh elements.
""".
-spec get_element_from_id( indice(), [ mesh_element() ] ) -> mesh_element().
get_element_from_id( Id, Elements ) ->
	lists:nth( Id, Elements ).



-doc """
Returns the elements (typically vertices, colors, texture coordinates) of the
specified indices) relative to the specified referenced mesh elements.
""".
-spec get_elements_from_ids( [ indice() ], [ mesh_element() ] ) ->
											[ mesh_element() ].
get_elements_from_ids( Ids, Elements ) ->
	[ get_element_from_id( Id, Elements ) || Id <- Ids ].




% Operations on meshes.


-doc "Returns a (rather full) textual description of the specified mesh.".
-spec to_string( mesh() ) -> ustring().
to_string( #mesh{ vertices=Vertices,
				  face_type=FaceType,
				  faces=Faces,
				  normal_type=NormalType,
				  normals=MaybeNormals,
				  rendering_info=RenderingInfo,
				  rendering_state=MaybeRenderState,
				  bounding_volume=MaybeBoundingVolume } ) ->

	NormalStr = case MaybeNormals of

		undefined ->
			"no normals";

		Normals ->
			text_utils:format( "~B ~ts normals: ~w", [ length( Normals ),
				normal_type_to_string( NormalType ), Normals ] )

	end,

	RenderStateStr = case MaybeRenderState of

		undefined ->
			"no rendering state available";

		RenderState ->
			mesh_render:rendering_state_to_string( RenderState )

	end,

	BVStr = case MaybeBoundingVolume of

		undefined ->
			"none available";

		BV ->
			bounding_volume:to_string( BV )

	end,

	text_utils:format( "mesh defined by:~n"
		" - ~B vertices: ~w~n"
		" - ~B ~ts faces: ~w~n"
		" - ~ts~n"
		" - ~ts~n"
		" - ~ts~n"
		" - bounding volume: ~ts",
		[ array:size( Vertices ), array:to_list( Vertices ),
		  length( Faces ), face_type_to_string( FaceType ), Faces,
		  NormalStr, mesh_render:rendering_info_to_string( RenderingInfo ),
		  RenderStateStr, BVStr ] ).



-doc "Returns a compact textual description of the specified mesh.".
-spec to_compact_string( mesh() ) -> ustring().
to_compact_string( #mesh{ vertices=Vertices,
						  face_type=FaceType,
						  faces=Faces,
						  normal_type=NormalType,
						  normals=MaybeNormals,
						  rendering_info=RenderingInfo,
						  rendering_state=MaybeRenderState,
						  bounding_volume=MaybeBoundingVolume } ) ->

	NormalStr = case MaybeNormals of

		undefined ->
			"no";

		Normals ->
			text_utils:format( "~B ~ts", [ length( Normals ),
				normal_type_to_string( NormalType ) ] )

	end,

	BVStr = case MaybeBoundingVolume of

		undefined ->
			"no bounding volume available";

		BV ->
			bounding_volume:to_string( BV )

	end,

	text_utils:format( "mesh with ~B vertices, ~B ~ts faces, "
		"~ts normals, with ~ts, ~ts and ~ts",
		[ array:size( Vertices ), length( Faces ),
		  face_type_to_string( FaceType ), NormalStr,
		  mesh_render:rendering_info_to_compact_string( RenderingInfo ),
		  case MaybeRenderState of
				undefined -> "no";
				_ -> "a"
		  end ++ " rendering state",
		  BVStr ] ).



-doc "Returns a textual description of the specified face type.".
-spec face_type_to_string( face_type() ) -> ustring().
face_type_to_string( triangle ) ->
	"triangle";

face_type_to_string( quad ) ->
	"quad".



-doc "Returns a textual description of the specified normal type.".
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

%   SphereBVolume = bounding_volume:get_lazy_sphere( Mesh#mesh.vertices ),

%   Mesh#mesh{ bounding_volume=SphereBVolume }.
