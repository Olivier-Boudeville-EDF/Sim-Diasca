% Copyright (C) 2023-2023 Olivier Boudeville
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
% Creation date: Saturday, March 11, 2023.


% @doc Testing of the <b>texture support</b>; displays an image as a texture.
%
% This test relies on the old OpenGL (the one obtained with the "compatibility"
% profile), as opposed to more modern versions of OpenGL (e.g. 3.1) that rely on
% shaders and GLSL. See gui_opengl_texture_shader_test for its more modern
% counterpart.
%
-module(gui_opengl_texture_test).


% Implementation notes:

% Inspired from lib/wx/examples/demo/ex_gl.erl and
% http://www.opengl-tutorial.org/beginners-tutorials/tutorial-5-a-textured-cube/


% For GL/GLU defines; the sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").


% For reuse by other tests:
-export([ get_test_texture_path/0 ]).


% Test-specific overall GUI state:
-record( my_gui_state, {

	% The main frame of this test:
	main_frame :: frame(),

	% The OpenGL canvas on which rendering will be done:
	canvas :: gl_canvas(),

	% The OpenGL context being used:
	context :: gl_context(),

	% The image as loaded from file, to be transformed in a texture:
	image :: image(),

	% Needs an OpenGL context:
	texture :: maybe( texture() ),

	% Here just a boolean; in more complex cases, would be a maybe-(OpenGL
	% state), e.g. to store the loaded textures:
	%
	opengl_initialised = false :: boolean() } ).

-type my_gui_state() :: #my_gui_state{}.
% Test-specific overall GUI state.



% Shorthands:

-type frame() :: gui_frame:frame().

-type image_path() :: gui_image:image_path().
-type image() :: gui_image:image().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().

-type texture() :: gui_texture:texture().



% @doc Runs the OpenGL test if possible.
-spec run_opengl_test() -> void().
run_opengl_test() ->

	test_facilities:display( "~nStarting the test of texture support." ),

	case gui_opengl:get_glxinfo_strings() of

		undefined ->
			test_facilities:display( "No proper OpenGL support detected on host"
				" (no GLX visual reported), thus no test performed." );

		GlxInfoStr ->
			test_facilities:display( "Checking whether OpenGL hardware "
				"acceleration is available: ~ts.",
				[ gui_opengl:is_hardware_accelerated( GlxInfoStr ) ] ),
			run_actual_test()

	end.



% @doc Runs the actual test.
-spec run_actual_test() -> void().
run_actual_test() ->

	test_facilities:display( "This test will display a textured rectangle." ),

	gui:start(),

	% Could be batched (see gui:batch/1) to be more effective:
	InitialGUIState = init_test_gui(),

	gui_frame:show( InitialGUIState#my_gui_state.main_frame ),

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

	MainFrame =
		gui_frame:create( "MyriadGUI OpenGL Texture Test", _Size={ 800, 600 } ),

	% Using default GL attributes:
	GLCanvas = gui_opengl:create_canvas( _Parent=MainFrame ),

	% Created, yet not bound yet (must wait for the main frame to be shown):
	GLContext = gui_opengl:create_context( GLCanvas ),

	gui:subscribe_to_events( { [ onResized, onShown, onWindowClosed ],
							   _Src=MainFrame } ),

	% Needed as well, otherwise if that frame is moved out of the screen or if
	% another window overlaps, the OpenGL canvas gets garbled and thus must be
	% redrawn:
	%
	gui:subscribe_to_events( { onRepaintNeeded, GLCanvas } ),

	% Would be too early (no GL context yet):
	%TestTexture = gui_texture:load_from_file( get_test_texture_path() ),
	TestImage = gui_image:load_from_file( get_test_texture_path() ),

	% No OpenGL state yet (GL context cannot be set as current yet):
	#my_gui_state{ main_frame=MainFrame, canvas=GLCanvas, context=GLContext,
				   image=TestImage }.



-spec get_test_texture_path() -> image_path().
get_test_texture_path() ->
	"../../../../doc/myriad-title.png". % RGBA
	%"../../../../doc/myriad-lorenz-test.png". % RGB
	%"../../../../doc/test_pdf_sampled_function.png". % Color-map
	% From https://learnopengl.com/img/textures/container.jpg:
	%"container.jpg".
	%"wall.jpg".



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages.
%
-spec gui_main_loop( my_gui_state() ) -> void().
gui_main_loop( GUIState ) ->

	%trace_utils:debug( "Main loop." ),

	% Matching the least-often received messages last:
	receive

		{ onRepaintNeeded, [ GLCanvas, _GLCanvasId, _EventContext ] } ->

			%trace_utils:debug_fmt( "Repaint needed for OpenGL canvas ~w.",
			%                       [ GLCanvas ] ),

			RepaintedGUIState = case GUIState#my_gui_state.opengl_initialised of

				true ->
					gui_widget:enable_repaint( GLCanvas ),
					render( GUIState#my_gui_state.texture ),
					gui_opengl:swap_buffers( GLCanvas ),
					GUIState;

				% Not ready yet:
				false ->
					trace_utils:debug(
						"To be repainted, yet no OpenGL state yet." ),
					GUIState

			end,
			gui_main_loop( RepaintedGUIState );


		% For a window, the first resizing event happens immediately before its
		% onShown one:
		%
		{ onResized, [ _ParentWindow, _ParentWindowId, _NewParentSize,
					   _EventContext ] } ->

			%trace_utils:debug_fmt( "Resizing of the parent window "
			%   "(main frame) to ~w detected.", [ NewParentSize ] ),

			ResizedGUIState = case GUIState#my_gui_state.opengl_initialised of

				true ->
					on_main_frame_resized( GUIState );

				% Not ready yet:
				false ->
					trace_utils:debug( "Resized, yet no OpenGL state yet." ),
					GUIState

			end,

			gui_main_loop( ResizedGUIState );


		% The most suitable first location to initialise OpenGL, as making a GL
		% context current requires a shown window:
		%
		{ onShown, [ ParentWindow, _ParentWindowId, _EventContext ] } ->

			trace_utils:debug_fmt( "Parent window (main frame) just shown "
				"(initial size of ~w).",
				[ gui_widget:get_size( ParentWindow ) ] ),

			% Done once for all:
			InitGUIState = initialise_opengl( GUIState ),

			gui_main_loop( InitGUIState );


		{ onWindowClosed, [ ParentWindow, _ParentWindowId, _EventContext ] } ->
			trace_utils:info( "Main frame closed, test success." ),
			% No more recursing:

			% Very final check, while there is still an OpenGL context:
			gui_opengl:check_error(),

			gui_window:destruct( ParentWindow );


		OtherEvent ->
			trace_utils:warning_fmt( "Test ignored following event:~n ~p",
									 [ OtherEvent ] ),

			gui_main_loop( GUIState )

	% No 'after': no spontaneous action taken here, in the absence of events.

	end.



% @doc Sets up OpenGL, once for all (regardless of next resizings), once a
% proper OpenGL context is available.
%
-spec initialise_opengl( my_gui_state() ) -> my_gui_state().
initialise_opengl( GUIState=#my_gui_state{ canvas=GLCanvas,
										   context=GLContext,
										   image=Image,
										   % Checks:
										   texture=undefined,
										   opengl_initialised=false } ) ->

	% Initial size of canvas is typically 20x20 pixels:
	trace_utils:debug_fmt( "Initialising OpenGL (whereas canvas is of initial "
						   "size ~w).", [ gui_widget:get_size( GLCanvas ) ] ),

	% So done only once:
	gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

	% These settings will not change afterwards here (set once for all):

	% No impact: gl:frontFace( ?GL_CW ),

	gui_texture:set_basic_general_settings(),

	% Clears in grey rather than black:
	%gl:clearColor( 0.0, 0.0, 0.0, 0.0 ),
	gl:clearColor( 0.5, 0.5, 0.5, 0.0 ),

	Texture = gui_texture:create_from_image( Image ),

	gui_texture:set_as_current( Texture ),

	trace_utils:debug_fmt( "Prepared ~ts.",
						   [ gui_texture:to_string( Texture ) ] ),

	%trace_utils:debug_fmt( "Managing a resize of the main frame to ~w.",
	%                       [ gui:get_size( MainFrame ) ] ),

	InitGUIState = GUIState#my_gui_state{
		texture=Texture,
		opengl_initialised=true },

	% As the initial onResized was triggered whereas no OpenGL state was
	% already available:
	%
	on_main_frame_resized( InitGUIState ).



% @doc Managing a resizing of the main frame.
%
% OpenGL context expected here to have already been set.
%
-spec on_main_frame_resized( my_gui_state() ) -> my_gui_state().
on_main_frame_resized( GUIState=#my_gui_state{ canvas=GLCanvas,
											   texture=Texture } ) ->

	% Any OpenGL reset to be done because of the resizing should take place
	% here.

	% Maximises the canvas in the main frame:
	{ CanvasWidth, CanvasHeight } = gui_widget:maximise_in_parent( GLCanvas ),

	%trace_utils:debug_fmt( "New client canvas size: {~B,~B}.",
	%                       [ CanvasWidth, CanvasHeight ] ),

	% Lower-left corner and size of the viewport in the current canvas:
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
	% (actually returns {CanvasWidth, CanvasHeight})
	%
	gui_widget:sync( GLCanvas ),

	% The modelview matrix will be multiplied by an orthographic projection
	% matrix, a perspective matrix that produces a parallel projection based on
	% 6 clipping planes.
	%
	% Here coordinates are absolute (based on the size of the viewport - not
	% normalised in [0.0,1.0]), thus resizing the frame implies updating the
	% orthographic projection.

	gl:matrixMode( ?GL_PROJECTION ),
	gl:loadIdentity(),

	% An upside-down viewport was used to compensate for the flipped Y-axis with
	% OpenGL; yet now MyriadGUI automatically flips vertically textures when
	% loaded from images:
	%
	% (like glu:ortho2D/4)
	gl:ortho( _Left=0.0, _Right=float( CanvasWidth ),
			  % So not '_Bottom=float( CanvasHeight ), _Top=0.0,' anymore:
			  _Bottom=0.0, _Top=float( CanvasHeight ),
			  _Near=-1.0, _Far=1.0 ),

	gl:matrixMode( ?GL_MODELVIEW ),

	render( Texture ),

	% Includes a gl:flush/0:
	gui_opengl:swap_buffers( GLCanvas ),

	% Const here:
	GUIState.



% @doc Performs a (pure OpenGL) rendering.
-spec render( texture() ) -> void().
render( #texture{ width=TexWidth,
				  height=TexHeight,
				  min_x=MinX,
				  min_y=MinY,
				  max_x=MaxX,
				  max_y=MaxY } ) ->

	%trace_utils:debug_fmt( "Rendering now for size {~B,~B}.",
	%                       [ Width, Height ] ),

	% Already set: gl:matrixMode(?GL_MODELVIEW),

	gl:clear( ?GL_COLOR_BUFFER_BIT ),

	% The texture of interest is expected to be the one already bound here.

	_TopLeftRenderPoint = { RenderX=15, RenderY=150 },

	W = TexWidth,
	H = TexHeight,

	%trace_utils:debug_fmt( "Min={~f,~f} / Max={~f,~f};  W=~B / H=~B",
	%                       [ MinX, MinY, MaxX, MaxY, W, H ] ),

	% Map the texels to a square made of two upright triangles:
	gl:'begin'( ?GL_TRIANGLE_STRIP ),
		gl:texCoord2f( MinX, MinY ), gl:vertex2i( RenderX,   RenderY   ),
		gl:texCoord2f( MaxX, MinY ), gl:vertex2i( RenderX+W, RenderY   ),
		gl:texCoord2f( MinX, MaxY ), gl:vertex2i( RenderX,   RenderY+H ),
		gl:texCoord2f( MaxX, MaxY ), gl:vertex2i( RenderX+W, RenderY+H ),
	gl:'end'(),

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
