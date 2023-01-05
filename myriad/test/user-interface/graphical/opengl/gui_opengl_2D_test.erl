% Copyright (C) 2022-2023 Olivier Boudeville
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
% Creation date: Wednesday, February 9, 2022.


% @doc 2D testing of the <b>OpenGL support</b>; displays a white rectangle
% on a black background.
%
% It is therefore a non-interactive, passive test (no spontaneous/scheduled
% behaviour) whose main interest is to show a simple yet generic, appropriate
% structure in order to properly initialise the GUI and OpenGL, handle
% rendering, resizing and closing.
%
% This test relies on the OpenGL 1.x compatibility mode, as opposed to more
% modern versions of OpenGL (ex: 3.1) that rely on shaders and GLSL.
%
% See the gui_opengl.erl tested module.
%
% See gui_opengl_minimal_test.erl for a similar 2D test yet operating with
% normalised coordinates (in [0.0,1.0]).
%
-module(gui_opengl_2D_test).


% Implementation notes:
%
% Directly inspired from https://www.glprogramming.com/red/chapter01.html


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

	% The main window of this test:
	parent :: window(),

	% The OpenGL canvas on which rendering will be done:
	canvas :: gl_canvas(),

	% The OpenGL context being used:
	context :: gl_context(),

	% Here just a boolean; in more complex cases, would be a maybe OpenGL state
	% (ex: to store the loaded textures):
	%
	opengl_initialised = false :: boolean() } ).

-type my_gui_state() :: #my_gui_state{}.
% Test-specific overall GUI state.



% Shorthands:

-type window() :: gui:window().

-type width() :: gui:width().
-type height() :: gui:height().

-type gl_canvas() :: gui:opengl_canvas().
-type gl_context() :: gui:opengl_context().



% @doc Runs the OpenGL test if possible.
-spec run_opengl_test() -> void().
run_opengl_test() ->

	test_facilities:display( "~nStarting the test of OpenGL 2D support." ),

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

	test_facilities:display( "This test will display a white rectangle "
		"on a black background, and will adjust to screen resizes." ),

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

	MainFrame =
		gui:create_frame( "MyriadGUI OpenGL 2D Test", _Size={ 500, 250 } ),

	% Using default GL attributes:
	GLCanvas = gui_opengl:create_canvas( _Parent=MainFrame ),

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

		{ onRepaintNeeded, [ GLCanvas, _GLCanvasId, _EventContext ] } ->

			%trace_utils:debug_fmt( "Repaint needed for OpenGL canvas ~w.",
			%                       [ GLCanvas ] ),

			RepaintedGUIState = case GUIState#my_gui_state.opengl_initialised of

				true ->
					gui:enable_repaint( GLCanvas ),
					% Simpler than storing these at each resize:
					{ CanvasWidth, CanvasHeight } = gui:get_size( GLCanvas ),
					render( CanvasWidth, CanvasHeight ),
					gui_opengl:swap_buffers( GLCanvas ),
					GUIState;

				% Not ready yet:
				false ->
					trace_utils:debug(
						"To be repainted, yet no OpenGL state yet." ),
					GUIState

			end,
			gui_main_loop( RepaintedGUIState );


		% For a window, the first resizing event happens (just) before its
		% onShown one:
		%
		{ onResized, [ _ParentWindow, _ParentWindowId, _NewParentSize,
					   _EventContext ] } ->

			%trace_utils:debug_fmt( "Resizing of the parent window "
			%   (main frame) "to ~w detected.", [ NewParentSize ] ),

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
				"(initial size of ~w).", [ gui:get_size( ParentWindow ) ] ),

			% Done once for all:
			InitGUIState = initialise_opengl( GUIState ),

			gui_main_loop( InitGUIState );


		{ onWindowClosed, [ ParentWindow, _ParentWindowId, _EventContext ] } ->
			trace_utils:info( "Main frame closed, test success." ),
			% No more recursing:
			gui:destruct_window( ParentWindow );


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
										   opengl_initialised=false } ) ->

	% Initial size of canvas is typically 20x20 pixels:
	trace_utils:debug_fmt( "Initialising OpenGL (whereas canvas is of initial "
						   "size ~w).", [ gui:get_size( GLCanvas ) ] ),

	% So done only once:
	gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

	% These settings will not change afterwards here (set once for all):

	% Clears in black:
	gl:clearColor( 0.0, 0.0, 0.0, 0.0 ),


	%trace_utils:debug_fmt( "Managing a resize of the main frame to ~w.",
	%                       [ gui:get_size( MainFrame ) ] ),

	InitGUIState = GUIState#my_gui_state{ opengl_initialised=true },

	% As the initial onResized was triggered whereas no OpenGL state was
	% already available:
	%
	on_main_frame_resized( InitGUIState ).



% @doc Managing a resizing of the main frame.
%
% OpenGL context expected here to have already been set.
%
-spec on_main_frame_resized( my_gui_state() ) -> my_gui_state().
on_main_frame_resized( GUIState=#my_gui_state{ canvas=GLCanvas } ) ->

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

	% Multiplies the current modelview matrix by an orthographic matrix, a
	% perspective matrix that produces a parallel projection based on 6 clipping
	% planes.
	%
	% Here coordinates are absolute (based on the size of the viewport - not
	% normalised in [0.0,1.0]), thus resizing the frame implies updating the
	% orthographic projection.

	gl:matrixMode( ?GL_PROJECTION ),
	gl:loadIdentity(),

	% Like glu:ortho2D/4:
	gl:ortho( _Left=0.0, _Right=float( CanvasWidth ),
			  _Bottom=float( CanvasHeight ), _Top=0.0, _Near=-1.0, _Far=1.0 ),

	% Any OpenGL reset to be done because of the resizing should take place
	% here.
	%
	% Using here normalised coordinates (in [0.0,1.0]), so no need to update the
	% orthographic projection.

	render( CanvasWidth, CanvasHeight ),

	% Includes a gl:flush/0:
	gui_opengl:swap_buffers( GLCanvas ),

	% Const here:
	GUIState.



% @doc Performs a (pure OpenGL) rendering.
%
% In this simple case, no specific OpenGL state is needed to pass around.
%
-spec render( width(), height() ) -> void().
render( Width, Height ) ->

	%trace_utils:debug_fmt( "Rendering now for size {~B,~B}.",
	%                       [ Width, Height ] ),

	gl:clear( ?GL_COLOR_BUFFER_BIT ),

	% A white right-angled rectangle in the Z=0 plane (in a black background),
	% whose right angle is at the center of the viewport/frame, and which faces
	% the top-right frame corner:
	%
	% (using MyriadGUI 2D referential)

	% Draws in white:
	gl:color3f( 1.0, 1.0, 1.0 ),

	MidWidth = Width div 2,
	MidHeight = Height div 2,

	% CCW order:
	gl:'begin'( ?GL_TRIANGLES ),

		gl:vertex2i( MidWidth, 0 ),

		% Going towards the bottom of the screen:
		gl:vertex2i( MidWidth, MidHeight ),

		% Going right:
		gl:vertex2i( Width, MidHeight ),

	gl:'end'(),

	% A fix-width blue "F" character:

	% Draws in red now:
	gl:color3f( 1.0, 0.0, 0.0 ),

	XOffset = 10,
	YOffset = 100,

	CharWidth = 20,
	CharHeight = 35,

	% All "F" except its small intermediate horizontal bar, starting from its
	% lowest part:
	%
	gl:'begin'( ?GL_LINE_STRIP ),
		gl:vertex2i( XOffset, YOffset + CharHeight ),
		gl:vertex2i( XOffset, YOffset ),
		gl:vertex2i( XOffset + CharWidth, YOffset ),
	gl:'end'(),

	% Finally its small intermediate horizontal bar:
	gl:'begin'( ?GL_LINES ),

		% Mid-height:
		YBar = YOffset + CharHeight div 2,

		gl:vertex2i( XOffset, YBar ),

		% A little shorter than the top part:
		gl:vertex2i( XOffset + 15, YBar ),

	gl:'end'(),


	% Draws "U" in green now:
	gl:color3f( 0.0, 1.0, 0.0 ),

	XInterletterOffset = 30,

	gl:'begin'( ?GL_LINE_STRIP ),
		gl:vertex2i( XOffset+XInterletterOffset, YOffset ),
		gl:vertex2i( XOffset+XInterletterOffset, YOffset + CharHeight ),
		gl:vertex2i( XOffset+XInterletterOffset+CharWidth,
					 YOffset+CharHeight ),
		gl:vertex2i( XOffset+XInterletterOffset+CharWidth, YOffset ),
	gl:'end'(),

	% Draws "N" in green now:
	gl:color3f( 0.0, 0.0, 1.0 ),

	gl:'begin'( ?GL_LINE_STRIP ),
		gl:vertex2i( XOffset+2*XInterletterOffset, YOffset + CharHeight ),
		gl:vertex2i( XOffset+2*XInterletterOffset, YOffset ),
		gl:vertex2i( XOffset+2*XInterletterOffset+CharWidth,
					 YOffset + CharHeight  ),
		gl:vertex2i( XOffset+2*XInterletterOffset+CharWidth,
					 YOffset ),
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
