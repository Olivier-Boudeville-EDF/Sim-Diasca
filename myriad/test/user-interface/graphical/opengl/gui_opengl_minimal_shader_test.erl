% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Sunday, January 9, 2022.


% @doc Minimal testing of the <b>OpenGL GLSL support</b>; displays, based on
% shaders, a read triangle on a black background.
%
% It is therefore a non-interactive, passive test (no spontaneous/scheduled
% behaviour) whose main interest is to show a simple yet generic, appropriate
% structure in order to properly initialise the GUI and OpenGL, handle
% rendering, resizing and closing.
%
% This test relies on shaders and thus modern versions of OpenGL (ex: 3.3), as
% opposed to the compatibility mode for OpenGL 1.x.
%
% See the gui_opengl.erl tested module.
%
-module(gui_opengl_minimal_shader_test).


% Implementation notes:
%
% Directly inspired from
% https://www.opengl-tutorial.org/beginners-tutorials/tutorial-2-the-first-triangle/;
% see
% https://github.com/opengl-tutorials/ogl/tree/master/tutorial02_red_triangle as
% well.


% For GL/GLU defines:
-include("gui_opengl.hrl").
% For user code: -include_lib("myriad/include/gui_opengl.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").



% Test-specific overall GUI state:
%
% (no OpenGL-specific state to store, like vertices, textures or alike)
%
-record( my_gui_state, {

	% The main frame of this test:
	parent :: frame(),

	% The OpenGL canvas on which rendering will be done:
	canvas :: gl_canvas(),

	% The OpenGL context being used:
	context :: gl_context(),

	% Here just a boolean; in more complex cases, would be a maybe OpenGL state
	% (ex: to store the loaded textures):
	%
	opengl_state :: maybe( my_opengl_state() ) } ).

-type my_gui_state() :: #my_gui_state{}.
% Test-specific overall GUI state.



-record( my_opengl_state, {

	% The identifier of our GLSL program:
	program_id :: program_id(),

	% The identifier of the Vertex Array Object used in this test:
	vao_id :: vao_id(),

	% The identifier of the Vertex Buffer Object for the triangle:
	vbo_id :: vbo_id() } ).

-type my_opengl_state() :: #my_opengl_state{}.
% Test-specific overall OpenGL state.




% Shorthands:

%-type dimensions() :: gui:dimensions().
-type frame() :: gui:frame().

-type width() :: gui:width().
-type height() :: gui:height().

-type gl_canvas() :: gui:opengl_canvas().
-type gl_context() :: gui:opengl_context().

-type vao_id() :: gui_opengl:vao_id().
-type vbo_id() :: gui_opengl:vbo_id().
-type program_id() :: gui_opengl:program_id().



% @doc Runs the OpenGL test if possible.
-spec run_opengl_test() -> void().
run_opengl_test() ->

	test_facilities:display(
		"~nStarting the minimal test of OpenGL shader support." ),

	case gui_opengl:get_glxinfo_strings() of

		undefined ->
			test_facilities:display( "No proper OpenGL support detected on host"
				" (no GLX visual reported), thus no test performed." );

		GlxInfoStr ->
			test_facilities:display( "Checking whether OpenGL hardware "
				"acceleration is available: ~ts",
				[ gui_opengl:is_hardware_accelerated( GlxInfoStr ) ] ),
			run_actual_test()

	end.



% @doc Runs the actual test.
-spec run_actual_test() -> void().
run_actual_test() ->

	test_facilities:display( "This test will display a Myriad-blue triangle "
							 "on a white background." ),

	gui:start(),
	% Could be batched (see gui:batch/1) to be more effective:
	InitialGUIState = init_test_gui(),

	gui:show( InitialGUIState#my_gui_state.parent ),

	% OpenGL will be initialised only when the corresponding frame will be ready
	% (that is once first reported as resized):
	%
	gui_main_loop( InitialGUIState ),

	gui:stop().



% @doc Creates the initial test GUI: a main frame containing an OpenGL canvas to
% which an OpenGL context is associated.
%
% Once the rendering is done, the buffers are swapped, and the content is
% displayed.
%
-spec init_test_gui() -> my_gui_state().
init_test_gui() ->

	MainFrame = gui:create_frame( "MyriadGUI OpenGL Minimal Shader Test",
								  _Size={ 1024, 768 } ),

	% Using default GL attributes:
	GLCanvas = gui_opengl:create_canvas( _Parent=MainFrame,
		[ { gl_attributes, [ use_core_profile ] } ] ),

	% Created, yet not bound yet (must wait for the main frame to be shown):
	GLContext = gui_opengl:create_context( GLCanvas ),

	gui:subscribe_to_events( { [ onResized, onShown, onWindowClosed ],
							   MainFrame } ),

	% Needed, otherwise if that frame is moved out of the screen or if another
	% windows overlaps, the OpenGL canvas gets garbled and thus must be redrawn:
	%
	gui:subscribe_to_events( { onRepaintNeeded, GLCanvas } ),

	% No OpenGL state yet (GL context cannot be set as current yet):
	#my_gui_state{ parent=MainFrame, canvas=GLCanvas, context=GLContext }.



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages.
%
-spec gui_main_loop( my_gui_state() ) -> void().
gui_main_loop( GUIState ) ->

	%trace_utils:debug( "Main loop." ),

	% Matching the least-often received messages last:
	receive


		{ onRepaintNeeded, [ GLCanvas, _EventContext ] } ->

			%trace_utils:debug_fmt( "Repaint needed for OpenGL canvas ~w.",
			%                       [ GLCanvas ] ),

			RepaintedGUIState = case GUIState#my_gui_state.opengl_state of

				% Not ready yet:
				undefined ->
					trace_utils:debug(
						"To be repainted, yet no OpenGL state yet." ),
					GUIState;

				GLState ->
					gui:enable_repaint( GLCanvas ),
					% Simpler than storing these at each resize:
					{ CanvasWidth, CanvasHeight } = gui:get_size( GLCanvas ),
					render( CanvasWidth, CanvasHeight, GLState ),
					gui_opengl:swap_buffers( GLCanvas ),
					GUIState

			end,
			gui_main_loop( RepaintedGUIState );



		% For a window, the first resizing event happens (just) before its
		% onShown one:
		%
		{ onResized, [ _ParentFrame, _NewParentSize, _EventContext ] } ->

			%trace_utils:debug_fmt( "Resizing of the parent window "
			%   "(main frame) to ~w detected.", [ NewParentSize ] ),

			ResizedGUIState = case GUIState#my_gui_state.opengl_state of

				% Not ready yet:
				undefined ->
					trace_utils:debug( "Resized, yet no OpenGL state yet." ),
					GUIState;

				_ ->
					on_main_frame_resized( GUIState )

			end,

			gui_main_loop( ResizedGUIState );



		% The most suitable first location to initialise OpenGL, as making a GL
		% context current requires a shown window:
		%
		{ onShown, [ ParentFrame, _EventContext ] } ->

			trace_utils:debug_fmt( "Parent window (main frame) just shown "
				"(initial size of ~w).", [ gui:get_size( ParentFrame ) ] ),

			% Optional yet better:
			gui:unsubscribe_from_events( { onShown, ParentFrame } ),

			% Done once for all:
			InitGUIState = initialise_opengl( GUIState ),

			gui_main_loop( InitGUIState );


		{ onWindowClosed, [ ParentFrame, _EventContext ] } ->
			cleanup_opengl( GUIState ),
			trace_utils:info( "Main frame closed, test success." ),

			% No more recursing:
			gui:destruct_frame( ParentFrame );


		OtherEvent ->
			trace_utils:warning_fmt( "Test ignored following event:~n ~p",
									 [ OtherEvent ] ),

			gui_main_loop( GUIState )

	% No 'after': no spontaneous action taken, in the absence of events.

	end.



% @doc Sets up OpenGL, once for all (regardless of next resizings), once a
% proper OpenGL context is available.
%
-spec initialise_opengl( my_gui_state() ) -> my_gui_state().
initialise_opengl( GUIState=#my_gui_state{ canvas=GLCanvas,
										   context=GLContext,
										   % Check:
										   opengl_state=undefined } ) ->

	% Initial size of canvas is typically 20x20 pixels:
	trace_utils:debug_fmt( "Initialising OpenGL (whereas canvas is of initial "
						   "size ~w).", [ gui:get_size( GLCanvas ) ] ),

	% So done only once:
	gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

	% First possible moment:
	test_facilities:display( "Description of the current OpenGL support: ~ts",
							 [ gui_opengl:get_support_description() ] ),

	% These test shaders are in 3.3 core (cf. their '#version 330 core'):
	MinOpenGLVersion = { 3, 3 },
	%MinOpenGLVersion = { 4, 6 },
	%MinOpenGLVersion = { 99, 0 },

	TargetProfile = core,
	%TargetProfile = non_existing_profile,

	%RequiredExts = [ non_existing_extension ],
	%RequiredExts = [ 'GL_ARB_draw_buffers' ],
	RequiredExts = [],

	gui_opengl:check_requirements( MinOpenGLVersion, TargetProfile,
								   RequiredExts ),


	% These settings will not change afterwards here (set once for all):

	% Clears in white:
	gl:clearColor( 1.0, 1.0, 1.0, 0.0 ),


	% Creates and compiles our GLSL program from the two specified shaders:
	ProgramId = gui_opengl:generate_program_from(
		"gui_opengl_minimal_shader.vertex.glsl",
		"gui_opengl_minimal_shader.fragment.glsl" ),

	% One (integer) identifier of vertex array wanted:
	[ VertexArrayId ] = gl:genVertexArrays( _Count=1 ),

	gl:bindVertexArray( VertexArrayId ),

	% Rely on our shader:
	gl:useProgram( ProgramId ),

	% Triangle defined by a [ point3:vertex3() ]:
	Vertices = [ { -1.0, -1.0, 0.0 }, { 1.0, -1.0, 0.0 }, { 0.0, 1.0, 0.0 } ],


	% Targeting vertex attributes:
	VertexBufferId = gui_opengl:bind_vertex_buffer_object( Vertices,
												_UsageHint=?GL_STATIC_DRAW ),

	% Could be done once for all here:
	%gl:bindBuffer( ?GL_ARRAY_BUFFER, VertexBufferId ),

	InitOpenGLState = #my_opengl_state{ program_id=ProgramId,
										vbo_id=VertexBufferId },

	%trace_utils:debug_fmt( "Managing a resize of the main frame to ~w.",
	%                       [ gui:get_size( MainFrame ) ] ),

	InitGUIState = GUIState#my_gui_state{ opengl_state=InitOpenGLState },

	% As the initial onResized was triggered whereas no OpenGL state was
	% already available:
	%
	on_main_frame_resized( InitGUIState ).



% @doc Cleans up OpenGL.
-spec cleanup_opengl( my_gui_state() ) -> void().
cleanup_opengl( #my_gui_state{ opengl_state=undefined } ) ->
	ok;

cleanup_opengl( #my_gui_state{ opengl_state=#my_opengl_state{
									program_id=ProgramId,
									vao_id=VaoId,
									vbo_id=VboId } } ) ->

	trace_utils:debug( "Cleaning up OpenGL." ),

	% Cleans up VBO:
	gl:deleteBuffers( [ VboId ] ),
	gl:deleteVertexArrays( [ VaoId ] ),
	gl:deleteProgram( ProgramId ).



% @doc Managing a resizing of the main frame.
%
% OpenGL context expected here to have already been set.
%
-spec on_main_frame_resized( my_gui_state() ) -> my_gui_state().
on_main_frame_resized( GUIState=#my_gui_state{ canvas=GLCanvas,
											   opengl_state=GLState } ) ->

	% Maximises the canvas in the main frame:
	{ CanvasWidth, CanvasHeight } = gui:maximise_in_parent( GLCanvas ),

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
	% rendering will thus take place according to the former (ex: minimised)
	% canvas size, not according to the one that was expected to be already
	% resized.
	%
	gui:sync( GLCanvas ),

	% No specific projection settings enforced.

	% Any OpenGL reset to be done because of the resizing should take place
	% here.
	%
	% Using here normalised coordinates (in [0.0,1.0]), so no need to update the
	% orthographic projection.

	render( CanvasWidth, CanvasHeight, GLState ),

	% Includes a gl:flush/0:
	gui_opengl:swap_buffers( GLCanvas ),

	% Const here:
	GUIState.



% @doc Performs a (pure OpenGL) rendering.
-spec render( width(), height(), my_opengl_state()  ) -> void().
render( _Width, _Height, #my_opengl_state{ vbo_id=VBufferId } ) ->

	%trace_utils:debug_fmt( "Rendering now for size {~B,~B}.",
	%                       [ Width, Height ] ),

	gl:clear( ?GL_COLOR_BUFFER_BIT ),

	% We already rely on our shader program.

	% In this specific simple case, could have been done only once, in OpenGL
	% initialisation:
	%
	gl:bindBuffer( ?GL_ARRAY_BUFFER, VBufferId ),

	% From now, all operations apparently must be performed at each rendering:

	% First and only attribute in buffer: the vertices.
	VertIndex = 0,

	% Uses the currently bound vertex array object;
	gl:enableVertexAttribArray( VertIndex ),

	% Attribute 0; no particular reason for this index, but must match the
	% layout in the shader:
	%
	AttrIndex = 0,

	% Number of components per attribute (3 coordinates per vertex):
	Size = 3,

	% Tells how the currently bound vertex array object shall be interpreted:
	gl:vertexAttribPointer( AttrIndex, Size, _Type=?GL_FLOAT,
		_Normalized=?GL_FALSE, _Stride=0, _ArrayBufferOffset=0 ),

	% Draws our splendid triangle (from 3 indices, starting at 0):
	gl:drawArrays( _Mode=?GL_TRIANGLES, _First=0, _Count=3 ),

	gl:disableVertexAttribArray( VertIndex ),

	% Not swapping buffers here, as would involve GLCanvas, whereas this
	% function is meant to remain pure OpenGL.
	%
	% gl:flush/0 done when swapping buffers.

	ok.



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the OpenGL test, being in batch mode)" );

		false ->
			run_opengl_test()

	end,

	test_facilities:stop().
