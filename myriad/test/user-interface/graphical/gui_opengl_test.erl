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


% @doc Testing the <b>OpenGL support</b>.
%
% See the gui_opengl.erl tested module.
%
-module(gui_opengl_test).


% Implementation notes:
%
% Directly inspired (rewrite) from:
% - wx:demo/0: lib/wx/examples/demo/ex_gl.erl
% - test suite: lib/wx/test/wx_opengl_SUITE.erl
%
% As the OpenGL canvas is not resized when its containers are, we listen to the
% resizing of the parent window and adapt accordingly.


% For GL/GLU defines:
-include("gui_opengl.hrl").
% For user code: -include_lib("myriad/include/gui_opengl.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").


% For re-use in other tests:
-export([ get_test_tetra_info/0, get_test_tetra_mesh/0,

		  get_test_tetra_vertices/0, get_test_tetra_faces/0,
		  get_test_tetra_normals/0, get_test_tetra_colors/0,


		  get_test_colored_cube_info/0, get_test_colored_cube_mesh/0,

		  get_test_colored_cube_vertices/0, get_test_colored_cube_faces/0,
		  get_test_colored_cube_normals/0, get_test_colored_cube_colors/0,


		  get_test_textured_cube_info/0, get_test_textured_cube_mesh/0,

		  get_test_textured_cube_vertices/0, get_test_textured_cube_faces/0,
		  get_test_textured_cube_normals/0, get_test_textured_cube_tex_coords/0,


		  get_test_image_path/0 ]).


% Silencing:
-export([ get_logo_image_path/0, update_clock_texture/2, render/1 ]).


% The duration, in milliseconds, between two updates of the OpenGL rendering:
-define( interframe_duration, 20 ).


% Test-specific overall GUI state:
-record( my_test_state, {

	% The main window of this test:
	parent :: window(),

	% The panel occupying the client area of the main window:
	panel :: panel(),

	% The OpenGL canvas on which rendering will be done:
	canvas :: gl_canvas(),

	% The OpenGL context being used:
	context :: gl_context(),

	% An image used as a material:
	image :: image(),

	% The various OpenGL information kept by this test once initialised:
	opengl_state :: maybe( my_opengl_test_state() ),

	% Records the current time to update the clock texture when relevant:
	time :: time() } ).

-type my_test_state() :: #my_test_state{}.
% Test-specific overall GUI state.



% OpenGL-specific GUI test state:
-record( my_opengl_test_state, {

	% Typically the GL canvas:
	window :: window(),

	mesh :: mesh(),

	angle = 0.0 :: degrees(),

	material_texture :: texture(),

	alpha_texture :: texture(),

	text_texture :: texture(),

	clock_texture :: texture(),

	font :: font(),

	brush :: brush(),

	sphere :: glu_id() } ).

-type my_opengl_test_state() :: #my_opengl_test_state{}.



% Shorthands:

-type file_path() :: file_utils:file_path().

-type time() :: time_utils:time().

-type degrees() :: unit_utils:degrees().

-type vertex3() :: point3:vertex3().
-type unit_normal3() :: vector3:unit_normal3().

-type mesh() :: mesh:mesh().
-type indexed_face() :: mesh:indexed_face().
-type indexed_triangle() :: mesh:indexed_triangle().
-type texture_coordinate2() :: mesh:texture_coordinate2().

-type render_rgb_color() :: gui_color:render_rgb_color().


-type dimensions() :: gui:dimensions().
-type window() :: gui:window().
-type panel() :: gui:panel().
-type image() :: gui:image().
-type font() :: gui:font().
-type brush() :: gui:brush().

-type gl_canvas() :: gui:opengl_canvas().
-type gl_context() :: gui:opengl_context().

-type glu_id() :: gui_opengl:glu_id().
-type texture() :: gui_opengl:texture().




% Test tetrahedron.


% @doc Returns the information needed in order to define a simple test
% tetrahedron.
%
-spec get_test_tetra_info() -> { [ vertex3() ], [ indexed_triangle() ],
								 [ unit_normal3() ], [ render_rgb_color() ] }.
get_test_tetra_info() ->
	% No texture coordinates used:
	{ get_test_tetra_vertices(), get_test_tetra_triangles(),
	  get_test_tetra_normals(), get_test_tetra_colors() }.



% @doc Returns a mesh corresponding to the test tetrahedron.
-spec get_test_tetra_mesh() -> mesh().
get_test_tetra_mesh() ->

	RenderingInfo = { color, per_vertex, get_test_tetra_colors() },

	% Needing faces, not triangles:
	mesh:create_mesh( get_test_tetra_vertices(), get_test_tetra_faces(),
		_NormalType=per_face, get_test_tetra_normals(), RenderingInfo ).



% @doc Returns the (4) vertices of the test tetrahedron.
-spec get_test_tetra_vertices() -> [ vertex3() ].
get_test_tetra_vertices() ->
	[ _V1={ 0.0,  0.0,  0.0 }, % A
	  _V2={ 5.0,  0.0,  0.0 }, % B
	  _V3={ 0.0, 10.0,  0.0 }, % C
	  _V4={ 0.0,  0.0, 15.0 }  % D
	].



% @doc Returns the (4) indexed faces of the test tetrahedron.
%
% Vertex order matters (CCW order when seen from outside)
%
-spec get_test_tetra_faces() -> [ indexed_face() ].
get_test_tetra_faces() ->
	[ _F1=[ 1, 3, 2 ], % ACB
	  _F2=[ 1, 2, 4 ], % ABD
	  _F3=[ 1, 4, 3 ], % ADC
	  _F4=[ 2, 3, 4 ]  % BCD
	].


% @doc Returns the (4) indexed triangles of the test tetrahedron.
%
% Vertex order matters (CCW order when seen from outside)
%
-spec get_test_tetra_triangles() -> [ indexed_triangle() ].
get_test_tetra_triangles() ->
	% Vertex lists to triplets:
	mesh:indexed_faces_to_triangles( get_test_tetra_faces() ).



% @doc Returns the (4) per-face unit normals of the test tetrahedron.
-spec get_test_tetra_normals() -> [ unit_normal3() ].
get_test_tetra_normals() ->
	[ _NF1=[ 0.0,  0.0, -1.0 ], % normal of ACB
	  _NF2=[ 0.0, -1.0,  0.0 ], % normal of ABD
	  _NF3=[ -1.0, 0.0,  0.0 ], % normal of ADC

	  % NF4, the normal of face BCD, can be obtained with:
	  %  BC = point3:vectorize(B, C).
	  %  BD = point3:vectorize(B, D).
	  %  V = vector3:cross_product(BC, BD).
	  %  N = vector3:normalise(V).
	  %
	  _NF4=[ 0.8571428571428571,0.42857142857142855,0.2857142857142857 ] ].



% @doc Returns the (4) per-face colors of the test tetrahedron.
-spec get_test_tetra_colors( ) -> [ render_rgb_color() ].
get_test_tetra_colors() ->
	[ _CF1={ 1.0, 1.0, 1.0 },   % white
	  _CF2={ 1.0, 0.0, 0.0 },   % red
	  _CF3={ 1.0, 1.0, 0.0 },   % yellow
	  _CF4={ 0.0, 0.0, 1.0 } ]. % blue




% Test colored cube (regular hexaedron).

% @doc Returns the information needed in order to define a simple test colored
% cube.
%
-spec get_test_colored_cube_info() ->
	{ [ vertex3() ], [ indexed_face() ], [ unit_normal3() ],
	  [ render_rgb_color() ] }.
get_test_colored_cube_info() ->
	{ get_test_colored_cube_vertices(), get_test_colored_cube_faces(),
	  get_test_colored_cube_normals(), get_test_colored_cube_colors() }.



% @doc Returns a mesh corresponding to the test colored cube.
-spec get_test_colored_cube_mesh() -> mesh().
get_test_colored_cube_mesh() ->
	{ Vertices, Faces, Normals, Colors } = get_test_colored_cube_info(),
	% per_vertex for gradients:
	%RenderingInfo = { color, per_face, Colors },
	RenderingInfo = { color, per_vertex, Colors },
	mesh:create_mesh( Vertices, Faces, _NormalType=per_face, Normals,
					  RenderingInfo ).



% @doc Returns the (8) vertices of the test colored cube.
-spec get_test_colored_cube_vertices() -> [ vertex3() ].
get_test_colored_cube_vertices() ->
	[ _V1={ -0.5, -0.5, -0.5 },
	  _V2={  0.5, -0.5, -0.5 },
	  _V3={  0.5,  0.5, -0.5 },
	  _V4={ -0.5,  0.5, -0.5 },
	  _V5={ -0.5,  0.5,  0.5 },
	  _V6={  0.5,  0.5,  0.5 },
	  _V7={  0.5, -0.5,  0.5 },
	  _V8={ -0.5, -0.5,  0.5 } ].



% @doc Returns the (6) faces of the test colored cube (vertex order matters).
-spec get_test_colored_cube_faces() -> [ indexed_face() ].
get_test_colored_cube_faces() ->
	[ _F1=[ 1, 2, 3, 4 ],
	  _F2=[ 8, 1, 4, 5 ],
	  _F3=[ 2, 7, 6, 3 ],
	  _F4=[ 7, 8, 5, 6 ],
	  _F5=[ 4, 3, 6, 5 ],
	  _F6=[ 1, 2, 7, 8 ] ].



% @doc Returns the (6) per-face unit normals of the test colored cube.
-spec get_test_colored_cube_normals() -> [ unit_normal3() ].
get_test_colored_cube_normals() ->
	[ _NF1=[ 0.0, 0.0,-1.0 ],
	  _NF2=[-1.0, 0.0, 0.0 ],
	  _NF3=[ 1.0, 0.0, 0.0 ],
	  _NF4=[ 0.0, 0.0, 1.0 ],
	  _NF5=[ 0.0, 1.0, 0.0 ],
	  _NF6=[ 0.0,-1.0, 0.0 ] ].



% @doc Returns the (8) per-vertex (not face) colors of the test colored cube.
-spec get_test_colored_cube_colors( ) -> [ render_rgb_color() ].
get_test_colored_cube_colors() ->
	[ _CF1={ 0.0, 0.0, 0.0 },
	  _CF2={ 1.0, 0.0, 0.0 },
	  _CF3={ 1.0, 1.0, 0.0 },
	  _CF4={ 0.0, 1.0, 0.0 },
	  _CF5={ 0.0, 1.0, 1.0 },
	  _CF6={ 1.0, 1.0, 1.0 },
	  _CF7={ 1.0, 0.0, 1.0 },
	  _CF8={ 0.0, 0.0, 1.0 } ].





% Test textured cube.
%
% In theory 8 vertices, 6 faces, 12 triangles.
%
% The following content data has been decoded in our higher-level form from the
% Blender default scene.


% @doc Returns the information needed in order to define a simple test textured
% cube.
%
-spec get_test_textured_cube_info() ->
					{ [ vertex3() ], [ indexed_face() ], [ unit_normal3() ],
					  [ texture_coordinate2() ] }.
get_test_textured_cube_info() ->
	% No texture coordinates used:
	{ get_test_textured_cube_vertices(), get_test_textured_cube_faces(),
	  get_test_textured_cube_normals(), get_test_textured_cube_tex_coords() }.



% @doc Returns a mesh corresponding to the test textured cube.
-spec get_test_textured_cube_mesh() -> mesh().
get_test_textured_cube_mesh() ->
	{ Vertices, Faces, Normals, Colors } = get_test_textured_cube_info(),
	RenderingInfo = { color, per_vertex, Colors },
	mesh:create_mesh( Vertices, Faces, _NormalType=per_face, Normals,
					  RenderingInfo ).



% @doc Returns the (8 in theory, 24 in practice) vertices of the test textured
% cube.
%
-spec get_test_textured_cube_vertices() -> [ vertex3() ].
get_test_textured_cube_vertices() ->
	% Stangely enough, in this cube each of the 8 (3D) vertices is listed thrice
	% in a row, so 24 of them are specified (probably as the same indices are to
	% be used also for normals and texture coordinates, which have per-vertex
	% differences):
	%
	[ {1.0,1.0,-1.0},   {1.0,1.0,-1.0},   {1.0,1.0,-1.0},
	  {1.0,-1.0,-1.0},  {1.0,-1.0,-1.0},  {1.0,-1.0,-1.0},
	  {1.0,1.0,1.0},    {1.0,1.0,1.0},    {1.0,1.0,1.0},
	  {1.0,-1.0,1.0},   {1.0,-1.0,1.0},   {1.0,-1.0,1.0},
	  {-1.0,1.0,-1.0},  {-1.0,1.0,-1.0},  {-1.0,1.0,-1.0},
	  {-1.0,-1.0,-1.0}, {-1.0,-1.0,-1.0}, {-1.0,-1.0,-1.0},
	  {-1.0,1.0,1.0},   {-1.0,1.0,1.0},   {-1.0,1.0,1.0},
	  {-1.0,-1.0,1.0},  {-1.0,-1.0,1.0},  {-1.0,-1.0,1.0} ].



% @doc Returns the (12) face triangles of the test textured cube.
-spec get_test_textured_cube_faces() -> [ indexed_triangle() ].
get_test_textured_cube_faces() ->

	% 36 indices (each in [0..23], listed once or twice):
	Indices = [ 1, 14, 20, 1, 20, 7, 10, 6, 19, 10, 19, 23,
				21, 18, 12, 21, 12, 15, 16, 3, 9, 16, 9, 22,
				5, 2, 8, 5, 8, 11, 17, 13, 0, 17, 0, 4 ],

	% Hence 36/3=12 triangles, 2 on each of the 6 faces:
	gltf_support:indices_to_triangles( Indices ).



% @doc Returns the 24 (3D, unitary) unit normals of the test textured cube.
%
% Possibly 24=3*8 corresponds to 3 normals per vertex of the cube (a given
% vertex taking part to 3 faces / 6 triangles).
%
-spec get_test_textured_cube_normals() -> [ unit_normal3() ].
get_test_textured_cube_normals() ->
	[ [0.0,0.0,-1.0],  [0.0,1.0,-0.0],  [1.0,0.0,-0.0],
	  [0.0,-1.0,-0.0], [0.0,0.0,-1.0],  [1.0,0.0,-0.0],
	  [0.0,0.0,1.0],   [0.0,1.0,-0.0],  [1.0,0.0,-0.0],
	  [0.0,-1.0,-0.0], [0.0,0.0,1.0],   [1.0,0.0,-0.0],
	  [-1.0,0.0,-0.0], [0.0,0.0,-1.0],  [0.0,1.0,-0.0],
	  [-1.0,0.0,-0.0], [0.0,-1.0,-0.0], [0.0,0.0,-1.0],
	  [-1.0,0.0,-0.0], [0.0,0.0,1.0],   [0.0,1.0,-0.0],
	  [-1.0,0.0,-0.0], [0.0,-1.0,-0.0], [0.0,0.0, 1.0] ].



% @doc Returns the 24 texture (2D) coordinates (each repeated thrice) of the
% test textured cube.
%
-spec get_test_textured_cube_tex_coords( ) -> [ texture_coordinate2() ].
get_test_textured_cube_tex_coords() ->
 [ {0.625,0.5},  {0.625,0.5},  {0.625,0.5},
   {0.375,0.5},  {0.375,0.5},  {0.375,0.5},
   {0.625,0.25}, {0.625,0.25}, {0.625,0.25},
   {0.375,0.25}, {0.375,0.25}, {0.375,0.25},
   {0.625,0.75}, {0.625,0.75}, {0.875,0.5},
   {0.375,0.75}, {0.125,0.5},  {0.375,0.75},
   {0.625,1.0},  {0.625,0.0},  {0.875,0.25},
   {0.375,1.0},  {0.125,0.25}, {0.375,0.0} ].



% @doc Returns the path to a test image.
-spec get_test_image_path() -> file_path().
get_test_image_path() ->
	%file_utils:join( gui_image_test:get_test_image_directory(),
	%				 "myriad-space-time-referential.png" ).
	%"image.jpg".
	file_utils:join( gui_image_test:get_test_image_directory(),
					 "myriad-minimal-enclosing-circle-test.png" ).


% @doc Returns the path to a test image.
-spec get_logo_image_path() -> file_path().
get_logo_image_path() ->
	file_utils:join( gui_image_test:get_test_image_directory(),
					 "myriad-title.png" ).
	%file_utils:join( gui_image_test:get_test_image_directory(),
	%				 "myriad-minimal-enclosing-circle-test.png" ).
	%"erlang.png".



% @doc Runs the OpenGL test if possible.
-spec run_test_opengl() -> void().
run_test_opengl() ->

	test_facilities:display( "~nStarting the test of OpenGL support." ),

	case gui_opengl:get_glxinfo_strings() of

		undefined ->
			test_facilities:display( "No proper OpenGL support detected on host"
				" (no GLX visual reported), thus no test performed." );

		GlxInfoStr ->
			test_facilities:display( "Checking whether OpenGL hardware "
				"acceleration is available: ~ts; glxinfo report is: ~ts",
				[ gui_opengl:is_hardware_accelerated( GlxInfoStr ),
				  text_utils:strings_to_string( GlxInfoStr ) ] ),
			run_actual_test()

	end.



% @doc Runs the actual test.
-spec run_actual_test() -> void().
run_actual_test() ->

	gui:start(),

	%gui:set_debug_level( [ calls, life_cycle ] ),

	% Postpone the processing of first events to accelerate initial setup:
	InitialGUIState = gui:batch( fun() -> init_test_gui() end ),

	gui:show( InitialGUIState#my_test_state.parent ),

	% Uncomment to check that a no_gl_context error report is triggered indeed,
	% as expected (as no current GL context yet):
	%
	%gl:viewport( 0, 0, 50, 50 ),

	% OpenGL will be initialised only when the corresponding frame will be ready
	% (that is once first reported as resized):
	%
	gui_main_loop( InitialGUIState ),

	gui:stop().




% @doc Creates the initial test GUI: a main frame containing a panel to which an
% OpenGL canvas is associated, in which an OpenGL context is created.
%
init_test_gui() ->

	MainFrame = gui:create_frame( "MyriadGUI OpenGL Test" ),

	Panel = gui:create_panel( MainFrame ),

	% At least this number of bits per RGB component:
	MinSize = 8,

	GLAttributes = [ rgba, double_buffer, { min_red_size, MinSize },
					 { min_green_size, MinSize }, { min_blue_size, MinSize },
					 { depth_buffer_size, 24 } ],

	GLCanvas = gui_opengl:create_canvas( _Parent=Panel,
		_Opts=[ { style, full_repaint_on_resize },
				{ gl_attributes, GLAttributes } ] ),

	% Created, yet not bound yet:
	GLContext = gui_opengl:create_context( GLCanvas ),

	gui:subscribe_to_events( { [ onShown, onResized, onWindowClosed ],
							   MainFrame } ),

	% (on Apple's Cocoa, subscribing to onRepaintNeeded might be required)
	%gui:subscribe_to_events( { onRepaintNeeded, GLCanvas } ),

	StatusBar = gui:create_status_bar( MainFrame ),

	gui:push_status_text( "Testing OpenGL now.", StatusBar ),

	Image = gui_image:create_from_file( get_test_image_path() ),

	gui_image:scale( Image, _NewWidth=128, _NewHeight=128 ),

	% No OpenGL state yet (GL context not set as current yet):
	#my_test_state{ parent=MainFrame, panel=Panel, canvas=GLCanvas,
					context=GLContext, image=Image }.



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages.
%
-spec gui_main_loop( my_test_state() ) -> void().
gui_main_loop( GUIState=#my_test_state{ parent=ParentWindow,
										opengl_state=MaybeOpenGLState } ) ->

	%trace_utils:debug( "Main loop." ),

	receive

		% For a window, the first resizing event happens (just) before its
		% onShown one, it is a suitable first location to initialise OpenGL:
		%
		{ onResized, [ ParentWindow, NewParentSize, _EventContext ] } ->

			trace_utils:debug_fmt( "Resizing of the parent window to ~w "
				"detected.", [ NewParentSize ] ),

			Panel = GUIState#my_test_state.panel,

			% In main frame:
			gui:maximise_in_parent( Panel ),

			GLCanvas = GUIState#my_test_state.canvas,

			% In panel:
			NewCanvasSize = gui:maximise_in_parent( GLCanvas ),

			NewGUIState =
				on_canvas_resized( GLCanvas, NewCanvasSize, GUIState ),

			gui_main_loop( NewGUIState );


		% Never happens, as onRepaintNeeded not listened to for the canvas:
		%{ onResized, [ GLCanvas, NewSize, _EventContext ] } ->
		%
		%    trace_utils:debug_fmt( "OpenGL canvas resized to ~w.",
		%        [ NewSize ] ),
		%
		%    NewGUIState = on_canvas_resized( GLCanvas, NewSize, GUIState ),
		%    gui_main_loop( NewGUIState );


		% Just to showcase that this event can be intercepted:
		{ onShown, [ ParentWindow, _EventContext ] } ->

			trace_utils:debug( "Test main frame shown." ),

			% A current OpenGL context should be available by design, as a
			% onResized event should have already been received and processed
			% before this onShown one:
			%
			test_facilities:display( "Reported OpenGL settings: "
				"vendor is '~ts', renderer is '~ts'; "
				"OpenGL version is '~ts', and the one of the "
				"shading language is '~ts'.",
				[ gui_opengl:get_vendor_name(), gui_opengl:get_renderer_name(),
				  gui_opengl:get_version(),
				  gui_opengl:get_shading_language_version() ] ),

			gui_main_loop( GUIState );


		% { onRepaintNeeded, [ GLCanvas, _EventContext ] } ->
		%	trace_utils:debug( "Canvas repaint needed." ),
		%	GLContext = GUIState#my_test_state.context,
		%	gui_opengl:set_context( GLCanvas, GLContext ),
		%	gui:enable_repaint( GLCanvas ),
		%	gui_main_loop( GUIState );

		{ onWindowClosed, [ ParentWindow, _EventContext ] } ->
			trace_utils:info( "Main frame closed, test success." ),
			gui:destruct_window( ParentWindow );


		OtherEvent ->
			trace_utils:warning_fmt( "Test ignored following event:~n ~p",
									 [ OtherEvent ] ),

			gui_main_loop( GUIState )

	% As the GUI is to be updated even in the absence of user actions:
	after ?interframe_duration ->

		NewGUIState = case MaybeOpenGLState of

			undefined ->
				trace_utils:debug( "(not ready yet)" ),
				GUIState;

			_ ->
				update_rendering( GUIState )

		end,

		gui_main_loop( NewGUIState )

	end.



% @doc To be called whenever the OpenGL canvas has been resized.
%
% No OpenGL state in this clause:
on_canvas_resized( GLCanvas, _NewCancasSize, GUIState=#my_test_state{
												context=GLContext,
												image=Image,
												opengl_state=undefined } ) ->

	% The first resizing (an event received even before onShown) is a relevant
	% moment in order to setup OpenGL, i.e. to set the created context as the
	% current one for the GL canvas:

	trace_utils:debug( "Initial canvas context setting." ),

	gui_opengl:set_context( GLCanvas, GLContext ),

	InitOpenGLState = setup_opengl( GLCanvas, Image ),

	GUIState#my_test_state{ opengl_state=InitOpenGLState };


% OpenGL state available here:
on_canvas_resized( _GLCanvas, NewCanvasSize, GUIState ) ->
	reset_opengl_on_resize( NewCanvasSize ),
	GUIState.



% @doc Sets up OpenGL, once for all, once a proper current context has been set.
-spec setup_opengl( gl_canvas(), image() ) -> my_opengl_test_state().
setup_opengl( Canvas, Image ) ->

	%trace_utils:debug( "Setting up OpenGL." ),

	Size = gui:get_client_size( Canvas ),
	reset_opengl_on_resize( Size ),

	%trace_utils:debug( "A0" ), %timer:sleep( 500 ),

	gl:enable( ?GL_DEPTH_TEST ),
	gl:depthFunc( ?GL_LESS ),

	% Solid white:
	gl:clearColor( 1.0, 1.0, 1.0, 1.0 ),

	MatTexture = gui_opengl:load_texture_from_image( Image ),

	AlphaTexture = gui_opengl:load_texture_from_file( get_logo_image_path() ),

	Font = gui:create_font( _PointSize=32, _Family=default_font_family,
							_Style=normal, _Weight=bold ),

	Brush = gui:create_brush( _Black={ 0, 0, 0 } ),

	% Myriad dark blue:
	TextColor = { 0, 39, 165 },

	TextTexture = gui_opengl:create_texture_from_text(
		%"This is a MyriadGUI-textured text", Font, Brush, TextColor,
		"MyriadGUI rocks!", Font, Brush, TextColor,
		_Flip=true ),

	ClockTexture =
		get_clock_texture( time_utils:get_local_time(), Font, Brush ),


	TestMesh = get_test_colored_cube_mesh(),
	%TestMesh = get_test_tetra_mesh(),

	SphereId = glu:newQuadric(),

	gl:enable( ?GL_TEXTURE_2D ),

	#my_opengl_test_state{ window=Canvas, mesh=TestMesh, angle=0.0,
		material_texture=MatTexture, alpha_texture=AlphaTexture,
		text_texture=TextTexture, clock_texture=ClockTexture,
		font=Font, brush=Brush, sphere=SphereId }.



% @doc Resets the OpenGL state after a resize of its window.
-spec reset_opengl_on_resize( dimensions() ) -> void().
reset_opengl_on_resize( NewSize={ Width, Height } ) ->

	trace_utils:debug_fmt( "Resetting OpenGL after resizing to ~w.",
						   [ NewSize ] ),

	gl:viewport( 0, 0, Width, Height ),

	gl:matrixMode( ?GL_PROJECTION ),

	gl:loadIdentity(),

	Left = -2.0,
	Bottom = -2.0 * Height / Width,
	Near = -20.00,
	gl:ortho( Left, _Right=-Left, Bottom, _Top=-Bottom, Near, _Far=-Near ),

	gl:matrixMode( ?GL_MODELVIEW ),
	gl:loadIdentity(),

	cond_utils:if_defined( myriad_check_opengl_support,
						   gui_opengl:check_error() ).



% @doc Updates the OpenGL rendering.
%
% Expected to be called periodically.
%
-spec update_rendering( my_test_state() ) -> my_test_state().
update_rendering( TestState=#my_test_state{ opengl_state=GLState,
											time=PreviousTime } ) ->

	%trace_utils:debug( "Updating rendering." ),

	NewAngle = GLState#my_opengl_test_state.angle + 1.0,

	AngleGLState = GLState#my_opengl_test_state{ angle=NewAngle },

	NewTime = time_utils:get_local_time(),

	NewGLState = case NewTime of

		% Still in the same second:
		PreviousTime ->
			AngleGLState;

		_ ->
			update_clock_texture( NewTime, AngleGLState )

	end,

	render( NewGLState ),

	TestState#my_test_state{ opengl_state=NewGLState,
							 time=NewTime }.



% @doc Updates the texture of the clock according to the specified time.
-spec update_clock_texture( time(), my_opengl_test_state() ) ->
												my_opengl_test_state().
update_clock_texture( Time, GLState=#my_opengl_test_state{
		clock_texture=ClockTexture, font=Font, brush=Brush } ) ->

	gui_opengl:delete_texture( ClockTexture ),
	NewClockTexture = get_clock_texture( Time, Font, Brush ),
	GLState#my_opengl_test_state{ clock_texture=NewClockTexture }.



% @doc Returns a texture corresponding to the specified clock time.
-spec get_clock_texture( time(), font(), brush() ) -> texture().
get_clock_texture( Time, Font, Brush ) ->

	TimeStr = time_utils:time_to_string( Time ),

	% Not flipped:
	gui_opengl:create_texture_from_text( TimeStr, Font, Brush,
										 _TextColor={ 255, 40, 40 } ).



% @doc Performs the rendering corresponding to the specified OpenGL state.
-spec render( my_opengl_test_state() ) -> void().
render( #my_opengl_test_state{ window=Window,
							   mesh=CubeMesh,
							   angle=Angle,
							   material_texture=_MatTexture,
							   alpha_texture=AlphaTexture,
							   text_texture=TextTexture,
							   clock_texture=ClockTexture,
							   sphere=SphereId } ) ->

	gl:matrixMode( ?GL_MODELVIEW ),
	gl:loadIdentity(),
	gl:pushMatrix(),
	gl:translatef( 0.0, 0.5, 0.0 ),

	% In degrees, around the first diagonal:
	gl:rotatef( Angle, _X=1.0, _Y=1.0, _Z=1.0 ),

	% See also https://www.khronos.org/opengl/wiki/Common_Mistakes#Swap_Buffers:
	gl:clear( ?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT ),

	%gl:bindTexture( ?GL_TEXTURE_2D, MatTexture#texture.id ),
	gl:disable( ?GL_BLEND ),

	% Specifies a texture environment:
	gl:texEnvi( ?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE ),

	gl:disable( ?GL_CULL_FACE ),

	gui_opengl:render_mesh( CubeMesh ),

	gl:popMatrix(),

	% Modified texture environment:
	gl:texEnvi( ?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE ),

	gui_opengl:enter_2d_mode( Window ),

	{ Width, Height } = gui:get_client_size( Window ),

	Move = abs( 90 - ( trunc( Angle ) rem 180 ) ),

	gui_opengl:render_texture( ClockTexture, _Xc=(Width div 2) - 50,
		_Yc=(Height div 2) - 130 + Move ),

	gui_opengl:render_texture( AlphaTexture, _Xa=(Width div 2) - 80,
		_Ya=(Height div 2) - Move ),

	gui_opengl:leave_2d_mode(),

	gl:pushMatrix(),
	gl:enable( ?GL_CULL_FACE ),
	gl:enable( ?GL_BLEND ),
	gl:blendFunc( ?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA ),
	gl:translatef( 0.0, -0.8, 0.0 ),
	gl:bindTexture( ?GL_TEXTURE_2D, TextTexture#texture.id ),

	% Texture coordinates should be generated:
	glu:quadricTexture( SphereId, ?GLU_TRUE ),
	glu:quadricNormals( SphereId, ?GLU_SMOOTH ),
	glu:quadricDrawStyle( SphereId, ?GLU_FILL ),
	glu:quadricOrientation( SphereId, ?GLU_OUTSIDE ),
	%gl:scalef( 2.0, 0.5, 1.0 ),
	gl:rotatef( -90.0, 1.0, 0.0, 0.0 ),
	gl:rotatef( -Angle, 0.0, 0.0, 1.0 ),
	glu:sphere( SphereId, 0.8, 50,40 ),
	gl:popMatrix(),

	gui_opengl:swap_buffers( Window ).



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the OpenGL test, being in batch mode)" );

		false ->
			run_test_opengl()

	end,

	test_facilities:stop().
