% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Wednesday, April 3, 2024.

-module(gui_opengl_for_testing).

-moduledoc """
A module to concentrate elements for the testing of OpenGL.
""".


-export([ can_be_run/1, has_opengl/1 ]).

-export([ get_test_image_directory/0, get_test_image_path/0,
		  get_logo_image_path/0 ]).


-export([ get_myriad_blue_rgb/0, get_myriad_blue_render/0 ]).

-export([ get_test_tetra_mesh/1,

		  get_test_tetra_vertices/0, get_test_tetra_faces/0,
		  get_test_tetra_normals/0, get_test_tetra_colors/0,


		  get_test_colored_cube_mesh/2,

		  get_test_cube_vertices/1, get_test_cube_faces/0,
		  get_test_cube_normals/0, get_test_cube_colors/1,

		  get_test_textured_cube_mesh/1,

		  get_test_textured_cube_vertices/0, get_test_textured_cube_faces/0,
		  get_test_textured_cube_normals/0, get_test_textured_cube_tex_coords/0
		]).



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type file_path() :: file_utils:file_path().
-type directory_path() :: file_utils:directory_path().

-type distance() :: linear:distance().

-type vertex3() :: point3:vertex3().
-type unit_normal3() :: vector3:unit_normal3().

-type color_by_decimal() :: gui_color:color_by_decimal().
-type render_rgb_color() :: gui_color:render_rgb_color().
-type render_rgba_color() :: gui_color:render_rgba_color().

-type uv_point() :: gui_texture:uv_point().

-type mesh() :: mesh:mesh().
-type indexed_face() :: mesh:indexed_face().
-type indexed_triangle() :: mesh:indexed_triangle().
-type face_granularity() :: mesh:face_granularity().



% Section for general test facilities.


-doc "Tells whether the described OpenGL test can be run.".
-spec can_be_run( ustring() ) -> 'yes' | 'batch' | 'no_opengl'.
can_be_run( TestDescription ) ->

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running this OpenGL test, being in batch mode)" ),
			batch;

		false ->
			has_opengl( TestDescription )

	end.



-doc "Tells whether an OpenGL support seems available.".
-spec has_opengl( ustring() ) -> 'yes' | 'no_opengl'.
has_opengl( TestDescription ) ->

	case gui_opengl:get_glxinfo_strings() of

		undefined ->
			test_facilities:display( "No proper OpenGL support detected on host"
				" (no GLX visual reported), thus no test performed." );

		GlxInfoStr ->
			test_facilities:display( "Checking whether OpenGL hardware "
				"acceleration is available: ~ts.",
				[ gui_opengl:is_hardware_accelerated( GlxInfoStr ) ] ),

			test_facilities:display( "Starting ~ts.", [ TestDescription ] ),

			% May run even if not accelerated:
			yes

	end.




% Path-related section.


-doc "Returns the path to a test image directory.".
-spec get_test_image_directory() -> directory_path().
get_test_image_directory() ->
	% Points to myriad/doc; relative to this test directory:
	file_utils:join( [ "..", "..", "..", "..", "doc" ] ).



-doc """
Returns the path to a basic "material" test image, for example to be mapped on a
rotating cube.
""".
-spec get_test_image_path() -> file_path().
get_test_image_path() ->
	file_utils:join( get_test_image_directory(),
					 %"myriad-space-time-coordinate-system.png" ).
					 "myriad-minimal-enclosing-circle-test.png" ).



-doc """
Returns the path to a logo test image. It may endlessly go up and down on the
screen.
""".
-spec get_logo_image_path() -> file_path().
get_logo_image_path() ->
	file_utils:join( get_test_image_directory(),
		% "myriad-title.png" ).
		% "myriad-minimal-enclosing-circle-test.png" ).
		"myriad-space-time-coordinate-system.png" ).



-doc "Defined for convenience and sharing with other tests.".
-spec get_myriad_blue_rgb() -> color_by_decimal().
get_myriad_blue_rgb() ->
	% #0027a5:
	_RGB={ 0, 39, 165 }.



-doc """
Returns a conventional RGBA value (hence for a vec4).

Defined for convenience and sharing with other tests.
""".
-spec get_myriad_blue_render() -> render_rgba_color().
get_myriad_blue_render() ->
	MyriadRGBA = erlang:insert_element( _PosIndex=4, get_myriad_blue_rgb(),
										_AlphaOpaqueTerm=255 ),
	RC = gui_color:decimal_to_render( MyriadRGBA ),
	%trace_utils:debug_fmt( "Myriad render color is ~w.", [ RC ] ),
	RC.



% Geometry-related section.


% Test tetrahedron.
%
% Logically, 4 vertices, 4 (triangle) faces.


-doc "Returns a mesh corresponding to the Myriad test tetrahedron.".
-spec get_test_tetra_mesh( face_granularity() ) -> mesh().
get_test_tetra_mesh( FaceGranularity ) ->
	Vertices = get_test_tetra_vertices(),
	Faces = get_test_tetra_faces(),
	Normals = get_test_tetra_normals(),

	% Colors can be used for vertices or faces (4 of them are needed):
	RenderingInfo = { colored, FaceGranularity, get_test_tetra_colors() },

	mesh:create( Vertices, _FaceType=triangle, Faces,
				 _NormalType=per_face, Normals, RenderingInfo ).



-doc "Returns the (4) vertices of the Myriad test tetrahedron.".
-spec get_test_tetra_vertices() -> [ vertex3() ].
get_test_tetra_vertices() ->
	[ _V1={ 0.0,  0.0,  0.0 }, % A
	  _V2={ 5.0,  0.0,  0.0 }, % B
	  _V3={ 0.0, 10.0,  0.0 }, % C
	  _V4={ 0.0,  0.0, 15.0 }  % D
	].



-doc """
Returns the (4; as triangles) indexed faces of the test tetrahedron.

Vertex order matters: it respects Myriad's conventions (CCW when seen from
outside).
""".
-spec get_test_tetra_faces() -> [ indexed_face() ].
get_test_tetra_faces() ->
	% Our indices start at 1:
	[ _F1={ 1, 3, 2 }, % ACB
	  _F2={ 1, 2, 4 }, % ABD
	  _F3={ 1, 4, 3 }, % ADC
	  _F4={ 2, 3, 4 }  % BCD
	].



-doc """
Returns the (4) per-face unit normals of the test tetrahedron.
""".
-spec get_test_tetra_normals() -> [ unit_normal3() ].
get_test_tetra_normals() ->
	[ _NF1=[  0.0,  0.0, -1.0 ], % normal of ACB
	  _NF2=[  0.0, -1.0,  0.0 ], % normal of ABD
	  _NF3=[ -1.0,  0.0,  0.0 ], % normal of ADC

	  % NF4, the normal of face BCD, can be obtained with:
	  %  BC = point3:vectorize(B, C).
	  %  BD = point3:vectorize(B, D).
	  %  V = vector3:cross_product(BC, BD).
	  %  NF4 = vector3:normalise(V).
	  %
	  _NF4=[ 0.8571428571428571, 0.42857142857142855, 0.2857142857142857 ] ].



-doc "Returns the (4) per-face colors of the test tetrahedron.".
-spec get_test_tetra_colors( ) -> [ render_rgb_color() ].
get_test_tetra_colors() ->
	[ gui_color:get_color( CName ) || CName <- [ red, green, blue, yellow ] ].




% Test colored cube (regular hexaedron).
%
% Logically, 8 vertices, 6 (square) faces, each face split into 2 triangles,
% hence 12 triangles.
%
% It can be colored per-vertex or per-face.


-doc """
Returns a mesh corresponding to a Myriad test colored cube of the specified edge
length, with the specified face-related granularity for colors.
""".
-spec get_test_colored_cube_mesh( distance(), face_granularity() ) -> mesh().
get_test_colored_cube_mesh( EdgeLength, FaceGranularity ) ->
	Vertices = get_test_cube_vertices( EdgeLength ),
	Faces = get_test_cube_faces(),
	Normals = get_test_cube_normals(),

	RenderingInfo = { colored, FaceGranularity,
					  get_test_cube_colors( FaceGranularity ) },

	%trace_utils:debug_fmt( "For the colored cube: ~ts",
	%   [ mesh_render:rendering_info_to_string( RenderingInfo ) ] ),

	mesh:create( Vertices, _FaceType=quad, Faces,
				 _NormalType=per_face, Normals, RenderingInfo ).



-doc """
Returns the (8) vertices corresponding to a Myriad test cube of the specified
edge length.
""".
-spec get_test_cube_vertices( distance() ) -> [ vertex3() ].
get_test_cube_vertices( _EdgeLength=L ) ->
	[ _V1={ -L,  L,  L }, _V2={  L,  L,  L },
	  _V3={  L, -L,  L }, _V4={ -L, -L,  L },
	  _V5={ -L, -L, -L }, _V6={ -L,  L, -L },
	  _V7={  L,  L, -L }, _V8={  L, -L, -L } ].



-doc """
Returns the (6; as quads) faces of the test cube.

Vertex order matters: it respects Myriad's conventions (CCW when seen from
outside).
""".
-spec get_test_cube_faces() -> [ indexed_face() ].
get_test_cube_faces() ->
	% Our indices start at 1:
	[ _F1={ 1, 4, 3, 2 },
	  _F2={ 6, 7, 8, 5 },
	  _F3={ 1, 6, 5, 4 },
	  _F4={ 2, 3, 8, 7 },
	  _F5={ 1, 2, 7, 6 },
	  _F6={ 3, 4, 5, 8 } ].



-doc """
Returns the (6) per-face unit normals of the test cube.

NFk is the normal of face Fk; they are actually useless, as here just orthogonal
to their respective faces.
""".
-spec get_test_cube_normals() -> [ unit_normal3() ].
get_test_cube_normals() ->
	[ _NF1=[  0.0, 0.0, 1.0 ], _NF2=[ 0.0,  0.0, -1.0 ],
	  _NF3=[ -1.0, 0.0, 0.0 ], _NF4=[ 1.0,  0.0,  0.0 ],
	  _NF5=[  0.0, 1.0, 0.0 ], _NF6=[ 0.0, -1.0,  0.0 ] ].



-doc """
Returns the colors to be used to render the test cube, depending on the
specified face granularity: either 8 per-vertex (not per-face) RGB integer
colors, or 6 per-face ones.
""".
-spec get_test_cube_colors( face_granularity() ) -> [ color_by_decimal() ].
get_test_cube_colors( _FaceGranularity=per_vertex ) ->
	ColQuadruplets = [ _CF1={ red, green, blue, yellow },
					   _CF2={ cyan, pink, gray, maroon },
					   _CF3={ aliceblue, antiquewhite, aqua, aquamarine },
					   _CF4={ azure, beige, bisque, black },
					   _CF5={ blanchedalmond, blue, blueviolet, brown },
					   _CF6={ burlywood, cadetblue, chartreuse, chocolate } ],

	mesh_render:color_to_decimal_tuples( ColQuadruplets );

get_test_cube_colors( _FaceGranularity=per_face ) ->
	[ gui_color:get_color( CName )
		|| CName <- [ red, green, blue, yellow, cyan, pink ] ].




% Test textured cube.
%
% Logically, 8 vertices, 6 faces, each face split into 2 triangles, hence 12
% triangles.
%
% The following content data has been decoded in our higher-level form from the
% Blender default scene.


-doc """
Returns a mesh corresponding to a Myriad test textured cube of the specified
edge length, based on a previous glTF decoding.
""".
-spec get_test_textured_cube_mesh( distance() ) -> mesh().
get_test_textured_cube_mesh( EdgeLength ) ->
	Vertices = get_test_cube_vertices( EdgeLength ),
	Faces = get_test_cube_faces(),
	Normals = get_test_cube_normals(),

	RenderingInfo = { texture, _TextureFaceInfos=[ fixme ] },

	mesh:create( Vertices, _FaceType=quad, Faces,
				 _NormalType=per_face, Normals, RenderingInfo ).



-doc """
Returns the (8 in theory, 24 in practice - each vertex belonging to multiple
faces/triangles) vertices of the test textured, decoded cube.
""".
-spec get_test_textured_cube_vertices() -> [ vertex3() ].
get_test_textured_cube_vertices() ->
	% Stangely enough, in this imported cube, each of the 8 (3D) vertices was
	% listed thrice in a row, so 24 of them were specified (possibly as the same
	% indices are to be used also for normals and texture coordinates, which
	% have per-vertex differences; relying on indices and on an EBO may help):
	%
	list_utils:repeat_elements( [ {  1.0,  1.0, -1.0 }, {  1.0, -1.0, -1.0 },
		{  1.0,  1.0,  1.0 }, {  1.0, -1.0,  1.0 }, { -1.0,  1.0, -1.0 },
		{ -1.0, -1.0, -1.0 }, { -1.0,  1.0,  1.0 }, { -1.0, -1.0,  1.0 } ],
		_Count=3 ).



-doc """
Returns the (2*6=12) face triangles of the test textured, decoded cube.
""".
-spec get_test_textured_cube_faces() -> [ indexed_triangle() ].
get_test_textured_cube_faces() ->

	% 36 indices (each in [0..23] - glTF indices start at zero, listed once or
	% twice):
	%
	Indices = [ 1, 14, 20, 1, 20, 7, 10, 6, 19, 10, 19, 23,
				21, 18, 12, 21, 12, 15, 16, 3, 9, 16, 9, 22,
				5, 2, 8, 5, 8, 11, 17, 13, 0, 17, 0, 4 ],

	% Hence 36/3=12 triangles, 2 on each of the 6 faces:
	gltf_support:indexes_to_triangles( Indices ).



-doc """
Returns the 24 (3D, unitary) unit normals of the test textured, decoded cube.

Possibly 24=3*8 corresponds to 3 normals per vertex of the cube (a given vertex
taking part to 3 faces / 6 triangles).
""".
-spec get_test_textured_cube_normals() -> [ unit_normal3() ].
get_test_textured_cube_normals() ->
	[ [  0.0,  0.0, -1.0 ], [ 0.0,  1.0, -0.0 ], [ 1.0, 0.0, -0.0 ],
	  [  0.0, -1.0, -0.0 ], [ 0.0,  0.0, -1.0 ], [ 1.0, 0.0, -0.0 ],
	  [  0.0,  0.0,  1.0 ], [ 0.0,  1.0, -0.0 ], [ 1.0, 0.0, -0.0 ],
	  [  0.0, -1.0, -0.0 ], [ 0.0,  0.0,  1.0 ], [ 1.0, 0.0, -0.0 ],
	  [ -1.0,  0.0, -0.0 ], [ 0.0,  0.0, -1.0 ], [ 0.0, 1.0, -0.0 ],
	  [ -1.0,  0.0, -0.0 ], [ 0.0, -1.0, -0.0 ], [ 0.0, 0.0, -1.0 ],
	  [ -1.0,  0.0, -0.0 ], [ 0.0,  0.0,  1.0 ], [ 0.0, 1.0, -0.0 ],
	  [ -1.0,  0.0, -0.0 ], [ 0.0, -1.0, -0.0 ], [ 0.0, 0.0,  1.0 ] ].



-doc """
Returns the 24 texture (2D) coordinates (each repeated thrice) of the test
textured, decoded cube.
""".
-spec get_test_textured_cube_tex_coords( ) -> [ uv_point() ].
get_test_textured_cube_tex_coords() ->
	list_utils:repeat_elements( [ { 0.625,0.5 }, { 0.375,0.5 },
		{ 0.625,0.25 }, { 0.375,0.25 }, { 0.625,0.75 }, { 0.375,0.75 },
		{ 0.625,1.0  }, { 0.375,1.0 } ] ).
