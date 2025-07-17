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
% Creation date: Monday, April 22, 2024.

-module(base_shader_quad_mesh_test).

-moduledoc """
Mesh-based version of the minimal testing of the **OpenGL mesh rendering**:
displays, thanks to the MyriadGUI base shaders, a set of 4 centered rectangles
(quads) of decreasing size, overlaping on a white background; these rectangles
are rendered, from outer to inner, as wireframe, with a face-level solid color,
a vertex-level solid level and textured.

This is one of the simplest shader-based tests: uses NDC coordinates (no
projection).

This test relies on:
- shaders and thus on modern versions of OpenGL (e.g. 3.3), as opposed to the
compatibility mode for OpenGL 1.x
- the MyriadGUI shader conventions and its base shaders
""".


% Implementation notes:
%
% Directly deriving from the base_shader_triangle_mesh_test module.

% For GL/GLU defines; the sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").



% Test-specific overall GUI state:
-record( my_gui_state, {

	% The main frame of this test:
	main_frame :: frame(),

	% The OpenGL canvas on which rendering will be done:
	canvas :: gl_canvas(),

	% The OpenGL context being used:
	context :: gl_context(),

	opengl_state :: option( my_opengl_state() ) } ).


-doc "Test-specific overall GUI state.".
-type my_gui_state() :: #my_gui_state{}.



% In more complex cases, would store the loaded textures, etc.:
-record( my_opengl_state, {

	% The identifier of our GLSL program:
	program_id :: program_id()

						  } ).


-doc "Test-specific overall OpenGL state.".
-type my_opengl_state() :: #my_opengl_state{}.


% Four (mesh-based) quads: one rendered in wireframe, another in a solid
% (per-face) color, a third with a per-vertex color (hence with a gradient), a
% fourth with a texture.
%
-record( my_mv_state, {

	quad_wireframe_mesh :: mesh(),
	quad_solid_mesh :: mesh(),
	quad_gradient_mesh :: mesh(),
	quad_texture_mesh :: mesh(),

	% May be better placed in the my_opengl_state record:
	texture_cache :: texture_cache()

						  } ).

-doc "Test-specific state of the model-view of interest.".
-type my_mv_state() :: #my_mv_state{}.




% Type shorthands:

-type frame() :: gui_frame:frame().

-type width() :: gui:width().
-type height() :: gui:height().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().

-type texture_cache() :: gui_texture:texture_cache().

-type program_id() :: gui_shader:program_id().

-type mesh() :: mesh:mesh().



-doc "Runs the actual test.".
-spec run_actual_test() -> void().
run_actual_test() ->

	test_facilities:display( "This test will display 4 concentric quads "
		"with different renderings, on a white background." ),

	gui:start(),

	% Depends on graphic support for image loading:
	MVState = create_mv_state(),

	% Could be batched (see gui:batch/1) to be more effective:
	InitialGUIState = init_test_gui(),

	gui_frame:show( InitialGUIState#my_gui_state.main_frame ),

	% OpenGL will be initialised only when the corresponding frame will be ready
	% (that is once first reported as resized):
	%
	gui_main_loop( InitialGUIState, MVState ),

	gui:stop().



-doc """
Creates the initial model-view state.

Cannot be done too early as loading images for textures requires the GUI to be
started, and, even more, texture creation requires OpenGL to be initialised.
""".
-spec create_mv_state() -> my_mv_state().
create_mv_state() ->

	% Let's start with a quad (actually a square in NDC, becoming a rectangle
	% due to the viewport aspect ratio), with a wireframe red quad:

	Z = 0.0,

	% Half edge length (each X/Y ranging in [-1;1], so most of the space:
	H = 0.9,

	% Quad defined as [vertex3()], directly in normalized device coordinates
	% here, centered onscreen; CCW order (Q1 bottom left, Q2 bottom right, Q3
	% top right, Q43 top left):
	%
	%         Q4--Q3
	%         |    |
	%         Q1--Q2
	%
	QuadWfVertices = [ _Q1={ -H, -H, Z }, _Q2={  H, -H, Z },
					   _Q3={  H,  H, Z }, _Q4={ -H,  H, Z } ],


	% A single (quad) face that will be tessellated into two triangles; as we
	% rely on vertex indices (i.e. EBO):
	%
	IndexedFaces = [ _F1={ 1, 2, 3, 4 } ], % Hence Q1-Q2-Q3-Q4

	FaceType = quad,

	% Various rendering_info() can be tested:

	%RenderingInfo = none,


	test_facilities:display( "Creating a wireframe quad." ),

	%AreHiddenFacesRemoved = false,
	AreHiddenFacesRemoved = true,

	WfRenderingInfo = { wireframe, _RGBEdgeColor=gui_color:get_color( red ),
						AreHiddenFacesRemoved },

	QuadWfMesh = mesh:create( QuadWfVertices, FaceType, IndexedFaces,
							  WfRenderingInfo ),


	test_facilities:display( "Creating inside a smaller solid yellow quad." ),

	ShrinkFactor = 0.65,

	% Still centered:
	QuadSolidVertices =
		[ point3:scale( P, ShrinkFactor ) || P <- QuadWfVertices ],

	% Still a single face:
	SolidRenderingInfo = { colored, _FaceColoringType=per_face,
		_ElementColors=[ gui_color:get_color( yellow ) ] },


	QuadSolidMesh = mesh:create( QuadSolidVertices, FaceType,
								 IndexedFaces, SolidRenderingInfo ),


	test_facilities:display(
		"Creating inside a smaller gradient-based RGB quad." ),

	QuadGradVertices =
		[ point3:scale( P, ShrinkFactor ) || P <- QuadSolidVertices ],

	GradElemColors = { gui_color:get_color( red ),
					   gui_color:get_color( green ),
					   gui_color:get_color( blue ),
					   gui_color:get_color( black ) },

	GradRenderingInfo = { colored, _FColorType=per_vertex,
						  [ _CF1=GradElemColors ] },

	QuadGradMesh = mesh:create( QuadGradVertices, FaceType, IndexedFaces,
								GradRenderingInfo ),


	test_facilities:display(
		"Creating at its right a texture-based quad." ),

	QuadTexVertices =
		[ point3:scale( P, ShrinkFactor ) || P <- QuadGradVertices ],

	BlankTextureCache = gui_texture:create_cache(),

	{ TextureSpecId, ReadyTextureCache } = gui_texture:declare_texture(
		_UserTexPathSpec=gui_opengl_for_testing:get_test_image_path(),
		BlankTextureCache ),

	test_facilities:display( "Having now a ~ts.",
		[ gui_texture:cache_to_string( ReadyTextureCache ) ] ),

	% As 2D texture coordinates range from 0 to 1 in the X and Y axes:
	UVVertices = { { 0.0, 0.0 }, { 1.0, 0.0 }, { 1.0, 1.0 }, { 0.0, 1.0 } },

	% Single texture, single face here:
	TexRenderingInfo = { textured, TextureSpecId, [ _TexF1=UVVertices ] },

	QuadTexMesh = mesh:create( QuadTexVertices, FaceType, IndexedFaces,
							   TexRenderingInfo ),

	#my_mv_state{ quad_wireframe_mesh=QuadWfMesh,
				  quad_solid_mesh=QuadSolidMesh,
				  quad_gradient_mesh=QuadGradMesh,
				  quad_texture_mesh=QuadTexMesh,
				  texture_cache=ReadyTextureCache }.



-doc """
Creates the initial test GUI: a main frame containing an OpenGL canvas to which
an OpenGL context is associated.
""".
-spec init_test_gui() -> my_gui_state().
init_test_gui() ->

	MainFrame = gui_frame:create(
		"MyriadGUI OpenGL Base Shader as Quad Mesh Test", _Size={ 1024, 768 } ),

	% Using mostly default GL attributes:
	GLCanvasAttrs =
		[ use_core_profile | gui_opengl:get_default_canvas_attributes() ],

	GLCanvas = gui_opengl:create_canvas(
		_CanvasOpts=[ { gl_attributes, GLCanvasAttrs } ], _Parent=MainFrame ),

	% Created, yet not bound yet (must wait for the main frame to be shown):
	GLContext = gui_opengl:create_context( GLCanvas ),

	gui:subscribe_to_events( { [ onResized, onShown, onWindowClosed ],
							   MainFrame } ),

	% Needed, otherwise if that frame is moved out of the screen or if another
	% windows overlaps, the OpenGL canvas gets garbled and thus must be redrawn:
	%
	gui:subscribe_to_events( { onRepaintNeeded, GLCanvas } ),

	% No OpenGL state yet (GL context cannot be set as current yet), actual
	% OpenGL initialisation to happen when available, i.e. when the main frame
	% is shown:
	%
	#my_gui_state{ main_frame=MainFrame, canvas=GLCanvas, context=GLContext }.



-doc """
The main loop of this test, driven by the receiving of MyriadGUI messages.
""".
-spec gui_main_loop( my_gui_state(), my_mv_state() ) -> void().
gui_main_loop( GUIState, MVState ) ->

	%trace_utils:debug( "Main loop." ),

	% Matching the least-often received messages last:
	receive

		{ onRepaintNeeded, [ GLCanvas, _GLCanvasId, _EventContext ] } ->

			%trace_utils:debug_fmt( "Repaint needed for OpenGL canvas ~w.",
			%                       [ GLCanvas ] ),

			case GUIState#my_gui_state.opengl_state of

				% Not ready yet:
				undefined ->
					trace_utils:debug(
						"To be repainted, yet no OpenGL state yet." );

				_GLState ->
					gui_widget:enable_repaint( GLCanvas ),

					% Simpler than storing these at each resize:
					{ CanvasWidth, CanvasHeight } =
						gui_widget:get_size( GLCanvas ),

					render( CanvasWidth, CanvasHeight, MVState ),
					gui_opengl:swap_buffers( GLCanvas )

			end,

			% No state change:
			gui_main_loop( GUIState, MVState );


		% For a window, the first resizing event happens immediately before its
		% onShown one:
		%
		{ onResized, [ _ParentFrame, _ParentFrameId, _NewParentSize,
					   _EventContext ] } ->

			%trace_utils:debug_fmt( "Resizing of the parent window "
			%   "(main frame) to ~w detected.", [ NewParentSize ] ),

			case GUIState#my_gui_state.opengl_state of

				% Not ready yet:
				undefined ->
					trace_utils:debug( "Resized, yet no OpenGL state yet." );

				_ ->
					on_main_frame_resized( GUIState, MVState )

			end,

			gui_main_loop( GUIState, MVState );


		% Less frequent messages looked up last:

		% The most suitable first location to initialise OpenGL, as making a GL
		% context current requires a shown window:
		%
		{ onShown, [ ParentFrame, _ParentFrameId, _EventContext ] } ->

			trace_utils:debug_fmt( "Parent window (main frame) just shown "
				"(initial size of ~w).",
				[ gui_widget:get_size( ParentFrame ) ] ),

			% Optional yet better:
			gui:unsubscribe_from_events( { onShown, ParentFrame } ),

			% Done once for all:
			InitGUIState = initialise_opengl( GUIState ),

			InitMVState = initialise_mv_for_opengl( MVState, InitGUIState ),

			% As the initial onResized was triggered whereas no OpenGL state was
			% already available:
			%
			on_main_frame_resized( InitGUIState, InitMVState ),

			% A onRepaintNeeded event message expected just afterwards.

			gui_main_loop( InitGUIState, InitMVState );


		{ onWindowClosed, [ ParentFrame, _ParentFrameId, _EventContext ] } ->
			cleanup_mv_for_opengl( MVState ),
			cleanup_opengl( GUIState ),

			trace_utils:info( "Main frame closed, test success." ),

			% Very final check, while there is still an OpenGL context:
			gui_opengl:check_error(),

			% No more recursing:
			gui_frame:destruct( ParentFrame );


		OtherEvent ->
			trace_utils:warning_fmt( "Test ignored following event:~n ~p",
									 [ OtherEvent ] ),

			gui_main_loop( GUIState, MVState )

	% No 'after': no spontaneous action taken here, in the absence of events.

	end.



-doc """
Sets up OpenGL, once for all (regardless of next resizings), once a proper
OpenGL context is available.
""".
-spec initialise_opengl( my_gui_state() ) -> my_gui_state().
initialise_opengl( GUIState=#my_gui_state{ canvas=GLCanvas,
										   context=GLContext,
										   % Check:
										   opengl_state=undefined } ) ->

	% Initial size of canvas is typically 20x20 pixels:
	trace_utils:debug_fmt( "Initialising OpenGL (whereas canvas is of initial "
						   "size ~w).", [ gui_widget:get_size( GLCanvas ) ] ),

	% So done only once, with appropriate measures for a first setting:
	gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

	% First possible moment:
	test_facilities:display( "Description of the current OpenGL support: ~ts",
							 [ gui_opengl:get_support_description() ] ),

	% These test shaders are in 3.3 core (cf. their '#version 330 core'):
	MinOpenGLVersion = { 3, 3 },
	%MinOpenGLVersion = { 4, 6 },
	%MinOpenGLVersion = { 99, 0 },

	% Not found available at least in some configurations:
	%TargetProfile = core,

	TargetProfile = compatibility,
	%TargetProfile = non_existing_profile,

	%RequiredExts = [ non_existing_extension ],
	%RequiredExts = [ 'GL_ARB_draw_buffers' ],
	RequiredExts = [],

	gui_opengl:check_requirements( MinOpenGLVersion, TargetProfile,
								   RequiredExts ),


	% These settings will not change afterwards here (hence set once for all):

	% Clears in white (otherwise black background):
	gl:clearColor( _R=1.0, _G=1.0, _B=1.0, ?alpha_fully_opaque ),

	% Creates, compiles, links, installs, prepares our GLSL program based on the
	% MyriadGUI builtin shaders, that are, in the same movement, automatically
	% attached and linked, then detached and deleted:
	%
	ProgramId = gui_shader:deploy_base_program(),

	% Just as an example check; usable as soon as the program is linked; will be
	% found iff declared but also explicitly used in at least one shader:

	VBOLayoutUnifName = ?myriad_gui_vbo_layout_unif_name,

	case gui_shader:get_maybe_uniform_id( VBOLayoutUnifName, ProgramId ) of

		undefined ->
			trace_utils:error_fmt( "No identifier is associated "
				"to the uniform variable named '~ts' within program of "
				"identifier ~B.",
				[ VBOLayoutUnifName, ProgramId ] ),

			throw( { uniform_id_not_found, VBOLayoutUnifName, ProgramId } );

		VBOLayoutUnifId ->
			trace_utils:debug_fmt( "The identifier associated to the uniform "
				"variable named '~ts' within program of identifier ~B has "
				"been found as expected, and is ~B.",
				[ VBOLayoutUnifName, ProgramId, VBOLayoutUnifId ] )

	end,

	% As textures will be used:
	gui_texture:set_basic_general_settings(),

	InitOpenGLState = #my_opengl_state{ program_id=ProgramId },

	GUIState#my_gui_state{ opengl_state=InitOpenGLState }.




-doc "Initialises, OpenGL-wise, the model-view.".
-spec initialise_mv_for_opengl( my_mv_state(), my_gui_state() ) ->
											my_mv_state().
initialise_mv_for_opengl( MVState=#my_mv_state{
							quad_wireframe_mesh=QuadWfMesh,
							quad_solid_mesh=QuadSolidMesh,
							quad_gradient_mesh=QuadGradMesh,
							quad_texture_mesh=QuadTexMesh,
							texture_cache=TexCache },
						  #my_gui_state{ opengl_state=#my_opengl_state{
								program_id=ProgramId } } ) ->

	Meshes = [ QuadWfMesh, QuadSolidMesh, QuadGradMesh,
			   QuadTexMesh ],

	trace_utils:debug_fmt( "Initialising for OpenGL the following meshes: ~ts",
		[ text_utils:strings_to_string(
			[ mesh:to_string( M ) || M <- Meshes ] ) ] ),

	{ [ InitQuadWfMesh, InitQuadSolidMesh, InitQuadGradMesh,
		InitQuadTexMesh ], NewTexCache } =
			mesh_render:initialise_for_opengl( Meshes, ProgramId, TexCache ),

	MVState#my_mv_state{
		quad_wireframe_mesh=InitQuadWfMesh,
		quad_solid_mesh=InitQuadSolidMesh,
		quad_gradient_mesh=InitQuadGradMesh,
		quad_texture_mesh=InitQuadTexMesh,
		texture_cache=NewTexCache }.



-doc "Cleans up the model-view, OpenGL-wise.".
-spec cleanup_mv_for_opengl( my_mv_state() ) -> my_mv_state().
cleanup_mv_for_opengl( MVState=#my_mv_state{
		quad_wireframe_mesh=QuadWfMesh,
		quad_solid_mesh=QuadSolidMesh,
		quad_gradient_mesh=QuadGradMesh,
		quad_texture_mesh=QuadTexMesh
		%square_mesh=SquareMesh
								 } ) ->

	[ CleanedQuadWfMesh, CleanedQuadSolidMesh, CleanedQuadGradMesh,
	  CleanedQuadTexMesh ] = [ mesh_render:cleanup_for_opengl( M )
			|| M <- [ QuadWfMesh, QuadSolidMesh, QuadGradMesh,
					  QuadTexMesh ] ],

	MVState#my_mv_state{ quad_wireframe_mesh=CleanedQuadWfMesh,
						 quad_solid_mesh=CleanedQuadSolidMesh,
						 quad_gradient_mesh=CleanedQuadGradMesh,
						 quad_texture_mesh=CleanedQuadTexMesh }.



-doc "Cleans up OpenGL.".
-spec cleanup_opengl( my_gui_state() ) -> void().
cleanup_opengl( #my_gui_state{ opengl_state=undefined } ) ->
	ok;

cleanup_opengl( #my_gui_state{ opengl_state=#my_opengl_state{
									program_id=ProgramId } } ) ->
	trace_utils:debug( "Cleaning up OpenGL." ),
	gui_shader:delete_program( ProgramId ).



-doc """
Managing a resizing of the main frame.

OpenGL context expected here to have already been set.

Once the rendering is done, the buffers are swapped, and the content is
displayed.
""".
-spec on_main_frame_resized( my_gui_state(), my_mv_state() ) -> void().
on_main_frame_resized( _GUIState=#my_gui_state{ canvas=GLCanvas }, MVState ) ->

	% Maximises the canvas in the main frame:
	{ CanvasWidth, CanvasHeight } = gui_widget:maximise_in_parent( GLCanvas ),

	%trace_utils:debug_fmt( "New client canvas size: {~B,~B}.",
	%                       [ CanvasWidth, CanvasHeight ] ),

	% Lower-left corner and size of the viewport in the current window:
	gl:viewport( 0, 0, CanvasWidth, CanvasHeight ),

	% Apparently, at least on a test setting, a race condition (discovered
	% thanks to the commenting-out of a debug trace) seems to exist between the
	% moment when the canvas is resized and the one when a new OpenGL rendering
	% is triggered afterwards; the cause is probably that maximising involves an
	% (Erlang) asynchronous message to be sent from this user process and to be
	% received and applied by the process of the target window, whereas a GL
	% (NIF-based) operation is immediate; without a sufficient delay, the
	% rendering will thus take place according to the former (e.g. minimised)
	% canvas size, not according to the one that was expected to be already
	% resized.
	%
	gui_widget:sync( GLCanvas ),

	% No specific projection settings enforced.

	% Any OpenGL reset to be done because of the resizing should take place
	% here.
	%
	% Using here normalised coordinates (in [0.0,1.0]), so no need to update the
	% orthographic projection.

	render( CanvasWidth, CanvasHeight, MVState ),

	% Includes a gl:flush/0:
	gui_opengl:swap_buffers( GLCanvas ).



-doc "Performs a (pure OpenGL) rendering.".
-spec render( width(), height(), my_mv_state() ) -> void().
render( _Width, _Height, #my_mv_state{
							quad_wireframe_mesh=QuadWfMesh,
							quad_solid_mesh=QuadSolidMesh,
							quad_gradient_mesh=QuadGradMesh,
							quad_texture_mesh=QuadTexMesh } ) ->

	%trace_utils:debug_fmt( "Rendering now for size {~B,~B}.",
	%                       [ Width, Height ] ),

	gl:clear( ?GL_COLOR_BUFFER_BIT ),

	% Uncomment to switch to wireframe and see how the square decomposes in two
	% quads:
	%
	% (front_and_back_facing not needed, as our vertices are in CCW order)
	%
	%gui_opengl:set_polygon_raster_mode( front_facing, raster_as_lines ),

	[ mesh_render:render_as_opengl( M ) || M <- [ QuadWfMesh,
		QuadSolidMesh, QuadGradMesh, QuadTexMesh ] ],

	% Not swapping buffers here, as would involve GLCanvas, whereas this
	% function is meant to remain pure OpenGL.
	%
	% gl:flush/0 done when swapping buffers.

	ok.



-doc "Runs the test.".
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	gui_opengl_for_testing:can_be_run(
			"the base OpenGL shader test of quad meshes" ) =:= yes
		andalso run_actual_test(),

	test_facilities:stop().
