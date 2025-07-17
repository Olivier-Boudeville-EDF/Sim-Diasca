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
% Creation date: Sunday, March 31, 2024.

-module(mesh_test).

-moduledoc """
Unit tests for the **mesh** services.

See the mesh tested module.

Uses a minimal OpenGL environment (including our base shaders) for its
rendering.
""".



% For GL/GLU defines; the sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").

-include_lib("myriad/include/mesh.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").



% Test-specific overall GUI state:
%
% (no OpenGL-specific state to store, like vertices, textures or alike)
%
-record( my_gui_state, {

	% The main frame of this test:
	main_frame :: frame(),

	% The OpenGL canvas on which rendering will be done:
	canvas :: gl_canvas(),

	% The OpenGL context being used:
	context :: gl_context(),

	% In more complex cases, would store the loaded textures, etc.
	opengl_state :: option( my_opengl_state() ) } ).


-doc "Test-specific overall GUI state.".
-type my_gui_state() :: #my_gui_state{}.



-record( my_opengl_state, {

	% The identifier of our GLSL program:
	program_id :: program_id()

	% The mesh holds its VAO, VBO, EBO by itself.

						  } ).


-doc "Test-specific overall OpenGL state.".
-type my_opengl_state() :: #my_opengl_state{}.



% First and only attribute in the vertex stream that will be passed to our
% shader: the vertices; attribute 0 was chosen, yet no particular reason for
% this index, it just must match the layout (cf. 'location = 0') in the shader.
%
-define( my_vertex_attribute_index, 0 ).



% Type shorthands:

-type mesh() :: mesh:mesh().
-type mesh_rendering_state() :: mesh:rendering_state().

-type frame() :: gui_frame:frame().

-type width() :: gui:width().
-type height() :: gui:height().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().

-type program_id() :: gui_shader:program_id().



-doc """
Continues this test by rendering the specified mesh; however OpenGL must be
already initialised for that.
""".
-spec continue_test_with_opengl( mesh() ) -> void().
continue_test_with_opengl( Mesh ) ->

	gui:start(),

	% Could be batched (see gui:batch/1) to be more effective:
	InitialGUIState = init_test_gui(),

	gui_frame:show( InitialGUIState#my_gui_state.main_frame ),

	% OpenGL will be initialised only when the corresponding frame will be ready
	% (that is once first reported as resized):
	%
	gui_main_loop( InitialGUIState, Mesh ),

	gui:stop().



-doc """
Creates the initial test GUI: a main frame containing an OpenGL canvas to which
an OpenGL context is associated.

Once the rendering is done, the buffers are swapped, and the content is
displayed.
""".
-spec init_test_gui() -> my_gui_state().
init_test_gui() ->

	MainFrame = gui_frame:create( "MyriadGUI OpenGL Mesh Shader Test",
								  _Size={ 1024, 768 } ),

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
-spec gui_main_loop( my_gui_state(), mesh() ) -> void().
gui_main_loop( GUIState, Mesh ) ->

	%trace_utils:debug( "Main loop." ),

	% Matching the least-often received messages last:
	receive

		{ onRepaintNeeded, [ GLCanvas, _GLCanvasId, _EventContext ] } ->

			%trace_utils:debug_fmt( "Repaint needed for OpenGL canvas ~w.",
			%                       [ GLCanvas ] ),

			RepaintedGUIState = case GUIState#my_gui_state.opengl_state of

				% Not ready yet:
				undefined ->
					trace_utils:debug(
						"To be repainted, yet no OpenGL state yet." ),
					GUIState;

				GLState ->
					gui_widget:enable_repaint( GLCanvas ),

					% Simpler than storing these at each resize:
					{ CanvasWidth, CanvasHeight } =
						gui_widget:get_size( GLCanvas ),

					render( CanvasWidth, CanvasHeight, GLState, Mesh ),

					gui_opengl:swap_buffers( GLCanvas ),

					GUIState

			end,
			gui_main_loop( RepaintedGUIState, Mesh );


		% For a window, the first resizing event happens immediately before its
		% onShown one:
		%
		{ onResized, [ _ParentFrame, _ParentFrameId, _NewParentSize,
					   _EventContext ] } ->

			%trace_utils:debug_fmt( "Resizing of the parent window "
			%   "(main frame) to ~w detected.", [ NewParentSize ] ),

			ResizedGUIState = case GUIState#my_gui_state.opengl_state of

				% Not ready yet:
				undefined ->
					trace_utils:debug( "Resized, yet no OpenGL state yet." ),
					GUIState;

				_ ->
					on_main_frame_resized( GUIState, Mesh#mesh.rendering_state )

			end,

			gui_main_loop( ResizedGUIState, Mesh );


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

			InitGLState = InitGUIState#my_gui_state.opengl_state,

			% Now that OpenGL is initialised:
			{ RegMesh, _MaybeTexCache } = mesh_render:initialise_for_opengl(
				Mesh, InitGLState#my_opengl_state.program_id ),

			test_facilities:display( "Registered mesh to OpenGL; its ~ts.",
				[ mesh_render:rendering_state_to_string(
					RegMesh#mesh.rendering_state ) ] ),

			% A onRepaintNeeded event message expected just afterwards.

			gui_main_loop( InitGUIState, RegMesh );


		{ onWindowClosed, [ ParentFrame, _ParentFrameId, _EventContext ] } ->
			CleanedMesh = mesh_render:cleanup_for_opengl( Mesh ),

			cleanup_opengl( GUIState ),

			trace_utils:info_fmt( "Main frame closed, mesh cleaned up (~ts), "
				"test success.", [ mesh:to_compact_string( CleanedMesh ) ] ),

			% Very final check, while there is still an OpenGL context:
			gui_opengl:check_error(),

			% No more recursing:
			gui_frame:destruct( ParentFrame );


		OtherEvent ->
			trace_utils:warning_fmt( "Test ignored following event:~n ~p",
									 [ OtherEvent ] ),

			gui_main_loop( GUIState, Mesh )

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

	% Specifies the location of the vertex attributes, so that the shader will
	% be able to match its input variables with the vertex attributes of the
	% application:
	%
	% ADD color etc.
	UserVertexAttrs = [ { "my_input_vertex", ?my_vertex_attribute_index } ],

	% Creates, compiles and links our GLSL program from the two specified
	% shaders, that are, in the same movement, automatically attached and
	% linked, then detached and deleted:
	%
	ProgramId = gui_shader:generate_program_from(
		"gui_opengl_base_shader.vertex.glsl",
		"gui_opengl_base_shader.fragment.glsl", UserVertexAttrs ),

	% ADD rendering type, color etc.
	SomeVectorUnifName = "some_vector",

	% Usable as soon as the program is linked; will be found iff declared but
	% also explicitly used in at least one shader:
	%
	case gui_shader:get_maybe_uniform_id( SomeVectorUnifName, ProgramId ) of

		% The actual case, as not used in these shaders, at least currently:
		undefined ->
			trace_utils:info_fmt( "As expected, no identifier is associated "
				"to the uniform variable named '~ts' within program of "
				"identifier ~B (as this variable is declared yet not used).",
				[ SomeVectorUnifName, ProgramId ] );

		SomeVectorUnifId ->
			trace_utils:warning_fmt( "The identifier associated to the uniform "
				"variable named '~ts' within program of identifier ~B has "
				"been found (which is unexpected) and is ~B.",
				[ SomeVectorUnifName, ProgramId, SomeVectorUnifId ] )

	end,

	GlobalColorUnifName = "myriad_gui_global_color",

	GlobalColorUnifId =
		gui_shader:get_uniform_id( GlobalColorUnifName, ProgramId ),

	% Rely on our shaders:
	gui_shader:install_program( ProgramId ),

	% Corresponds to a render_rgba_color():
	gui_shader:set_uniform_point4( GlobalColorUnifId,
		gui_opengl_for_testing:get_myriad_blue_render() ),


	% Uncomment to switch to wireframe:
	%
	% (front_and_back_facing not needed, as our vertices are in CCW order)
	%
	%gui_opengl:set_polygon_raster_mode( front_facing, raster_as_lines ),


	InitOpenGLState = #my_opengl_state{ program_id=ProgramId },

	%trace_utils:debug_fmt( "Managing a resize of the main frame to ~w.",
	%                       [ gui:get_size( MainFrame ) ] ),

	InitGUIState = GUIState#my_gui_state{ opengl_state=InitOpenGLState },

	% As the initial onResized was triggered whereas no OpenGL state was
	% already available:
	%
	on_main_frame_resized( InitGUIState, _MaybeMeshRenderState=undefined ).



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
""".
-spec on_main_frame_resized( my_gui_state(), mesh_rendering_state() ) ->
											my_gui_state().
on_main_frame_resized( GUIState=#my_gui_state{ canvas=GLCanvas,
											   opengl_state=GLState },
					   MaybeMeshRenderState ) ->

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

	render( CanvasWidth, CanvasHeight, GLState, MaybeMeshRenderState ),

	% Includes a gl:flush/0:
	gui_opengl:swap_buffers( GLCanvas ),

	% Const here:
	GUIState.



-doc "Performs a (pure OpenGL) rendering.".
-spec render( width(), height(), my_opengl_state(),
			  mesh_rendering_state() ) -> void().
render( _Width, _Height, _OpenGGLState, _MaybeMeshRenderState=undefined ) ->
	ok;

render( _Width, _Height, #my_opengl_state{}, MeshRenderState ) ->

	%trace_utils:debug_fmt( "Rendering now for size {~B,~B}.",
	%                       [ Width, Height ] ),

	gl:clear( ?GL_COLOR_BUFFER_BIT ),

	mesh_render:render_as_opengl( MeshRenderState ),

	% Not swapping buffers here, as would involve GLCanvas, whereas this
	% function is meant to remain pure OpenGL.
	%
	% gl:flush/0 done when swapping buffers.

	ok.



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing mesh services, based on an "
		"origin-centered cube of edges orthogonal to the axes and of length 2 "
		"(refer to myriad-test-cube.png).~n" ),

	%FaceColoringType = per_vertex,
	FaceColoringType = per_face,

	% Based on quads:
	%TestMesh = gui_opengl_for_testing:get_test_colored_cube_mesh(
	%   _EdgeLength=1.0, FaceColoringType ),

	TestMesh = gui_opengl_for_testing:get_test_tetra_mesh( FaceColoringType ),

	test_facilities:display( "The initial test mesh is a ~ts~n",
							 [ mesh:to_string( TestMesh ) ] ),

	TestTrigMesh = mesh:tessellate( TestMesh ),

	test_facilities:display( "The triangle-based test mesh is a ~ts~n",
							 [ mesh:to_string( TestTrigMesh ) ] ),

	gui_opengl_for_testing:can_be_run( "the OpenGL side of this mesh test" )
		=:= yes andalso continue_test_with_opengl( TestTrigMesh ),

	test_facilities:stop().
