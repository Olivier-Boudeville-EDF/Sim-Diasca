% Copyright (C) 2021-2024 Olivier Boudeville
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
% Creation date: Saturday, December 25, 2021.


% @doc Minimal testing of the <b>OpenGL support</b>; displays a white rectangle
% on a black background.
%
% It is therefore a non-interactive, passive test (no spontaneous/scheduled
% behaviour) whose main interest is to show a simple yet generic, appropriate
% structure in order to properly initialise the GUI and OpenGL, handle
% rendering, resizing and closing.
%
% This test relies on the OpenGL 1.x compatibility mode, as opposed to more
% modern versions of OpenGL (e.g. 3.1) that rely on shaders and GLSL.
%
% See the gui_opengl.erl tested module.
%
% See gui_opengl_minimal_test.erl for a similar 2D test yet operating with
% absolute (non-normalised coordinates).
%
-module(gui_opengl_minimal_test).


% Implementation notes:
%
% Directly inspired from https://www.glprogramming.com/red/chapter01.html


% For GL/GLU defines; the sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").


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

	% Here just a boolean; in more complex cases, would be a maybe OpenGL state
	% (e.g. to store the loaded textures):
	%
	opengl_initialised = false :: boolean() } ).

-type my_gui_state() :: #my_gui_state{}.
% Test-specific overall GUI state.


-export([ get_myriad_blue/0 ]).


% Shorthands:

-type frame() :: gui:frame().

-type render_rgb_color() :: gui_color:render_rgb_color().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().



% Defined for convenience and sharing with other tests.
-spec get_myriad_blue() -> render_rgb_color().
get_myriad_blue() ->
	[ 0.05, 0.2, 0.67 ].


% @doc Runs the OpenGL test if possible.
-spec run_opengl_test() -> void().
run_opengl_test() ->

	test_facilities:display( "~nStarting the minimal test of OpenGL support." ),

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
	%InitialGUIState = gui:batch( fun() -> init_test_gui() end ),

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

	MainFrame = gui_frame:create( "MyriadGUI Minimal OpenGL Test",
								  _Size={ 500, 250 } ),

	% This test may request additionally an OpenGL debug context:
	%GLAttrs = gui_opengl:get_default_canvas_attributes(),
	GLAttrs = [ debug_context | gui_opengl:get_default_canvas_attributes() ],

	GLCanvas = gui_opengl:create_canvas(
		_CanvasOpts=[ { gl_attributes, GLAttrs } ], _Parent=MainFrame ),

	% Created, yet not bound yet (must wait for the main frame to be shown):
	GLContext = gui_opengl:create_context( GLCanvas ),

	gui:subscribe_to_events( { [ onResized, onShown, onWindowClosed ],
							   MainFrame } ),

	% Needed, otherwise if that frame is moved out of the screen or if another
	% windows overlaps, the OpenGL canvas gets garbled and thus must be redrawn:
	%
	gui:subscribe_to_events( { onRepaintNeeded, GLCanvas } ),

	% No OpenGL state yet (GL context cannot be set as current yet):
	#my_gui_state{ main_frame=MainFrame, canvas=GLCanvas, context=GLContext }.



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
					render(),
					gui_opengl:swap_buffers( GLCanvas ),
					GUIState;

				% Not ready yet:
				false ->
					trace_utils:debug( "To be repainted, "
									   "yet no OpenGL state yet." ),
					GUIState

			end,
			gui_main_loop( RepaintedGUIState );


		% For a window, the first resizing event happens (just) before its
		% onShown one:
		%
		{ onResized,
		  [ _ParentWindow, _ParentWindowId, _NewParentSize, _EventContext ] } ->

			%trace_utils:debug_fmt( "Resizing of the parent window "
			%  "(main frame) to ~w detected.", [ NewParentSize ] ),

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

			% Optional yet better:
			gui:unsubscribe_from_events( { onShown, ParentWindow } ),

			% Done once for all:
			InitGUIState = initialise_opengl( GUIState ),

			test_facilities:display( "Reported OpenGL support: ~ts",
									 [ gui_opengl:get_support_description() ] ),

			gui_main_loop( InitGUIState );


		{ onWindowClosed, [ ParentWindow, _ParentWindowId, _EventContext ] } ->
			trace_utils:info( "Main frame closed, test success." ),

			% Very final check, while there is still an OpenGL context:
			gui_opengl:check_error(),

			% No more recursing:
			gui_window:destruct( ParentWindow );


		OtherEvent ->
			trace_utils:warning_fmt( "Test ignored following event:~n ~p",
									 [ OtherEvent ] ),

			gui_main_loop( GUIState )

	% No 'after': no spontaneous action taken, in the absence of events.

	end.



% @doc Sets up OpenGL, once for all, once a proper OpenGL context is available.
-spec initialise_opengl( my_gui_state() ) -> my_gui_state().
initialise_opengl( GUIState=#my_gui_state{ canvas=GLCanvas,
										   context=GLContext,
										   % Check:
										   opengl_initialised=false } ) ->

	% Initial size of canvas is typically 20x20 pixels:
	trace_utils:debug_fmt( "Initialising OpenGL (whereas canvas is of initial "
						   "size ~w).", [ gui_widget:get_size( GLCanvas ) ] ),

	% So done only once:
	gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

	test_opengl_debug_context(),

	%Exts = gui_opengl:get_supported_extensions(),
	%trace_utils:info_fmt( "~B OpenGL extensions supported: ~ts",
	%   [ length( Exts ), text_utils:atoms_to_listed_string( Exts ) ] ),

	% These settings will not change afterwards here (set once for all):

	% Clears in black:
	gl:clearColor( 0.0, 0.0, 0.0, 0.0 ),

	% Draws in white:
	gl:color3f( 1.0, 1.0, 1.0 ),

	gl:matrixMode( ?GL_PROJECTION ),
	gl:loadIdentity(),

	% Multiplies the current modelview matrix by an orthographic matrix, a
	% perspective matrix that produces a parallel projection based on 6 clipping
	% planes, implementing the MyriadGUI 2D conventions.
	%
	% Here coordinates are normalised in [0.0,1.0] and as such are
	% definition-independent (resizing the frame and then the viewport will not
	% affect them).
	%
	% Like glu:ortho2D/4:
	%
	gl:ortho( _Left=0.0, _Right=1.0, _Bottom=1.0, _Top=0.0, _Near=-1.0,
			  _Far=1.0 ),

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

	% Any OpenGL reset to be done because of the resizing should take place
	% here.
	%
	% Using normalised coordinates (in [0.0,1.0]), so no need to update the
	% orthographic projection.

	render(),

	% Includes a gl:flush/0:
	gui_opengl:swap_buffers( GLCanvas ),

	% Const here:
	GUIState.



% @doc Performs a (pure OpenGL) rendering.
%
% In this simple case, no specific OpenGL state is needed to pass around.
%
-spec render() -> void().
render() ->

	%trace_utils:debug( "Rendering now." ),

	gl:clear( ?GL_COLOR_BUFFER_BIT ),

	% A white rectangle in the z=0 plane (in a black background):
	gl:'begin'( ?GL_POLYGON ),
		gl:vertex3f( 0.25, 0.25, 0.0 ),
		gl:vertex3f( 0.75, 0.25, 0.0 ),
		gl:vertex3f( 0.75, 0.75, 0.0 ),
		gl:vertex3f( 0.25, 0.75, 0.0 ),
	gl:'end'(),


	% Not swapping buffers here, as would involve GLCanvas, whereas this
	% function is meant to remain pure OpenGL.
	%
	% gl:flush/0 done when swapping buffers.

	ok.



% Tests the OpenGL debug context.
-spec test_opengl_debug_context() -> void().
test_opengl_debug_context() ->

	% Actually already enabled:
	%false = gui_opengl:is_debug_context_enabled(),

	gui_opengl:enable_all_debug_context_reporting(),
	true = gui_opengl:is_debug_context_enabled(),

	[] = gui_opengl:get_debug_context_messages(),

	gui_opengl:insert_debug_context_message( _MsgId=5,
		_Msg="Testing debug context.", _MsgSeverity=high,
		_MsgSource=third_party, _MsgType=other ),

	[ Msg1 ] = gui_opengl:get_debug_context_messages(),

	trace_utils:debug_fmt( "First debug context message: ~ts",
		[ gui_opengl:debug_context_message_to_string( Msg1 ) ] ),

	gui_opengl:insert_debug_context_message( 6, "Second message.", medium,
		third_party, deprecated_behaviour ),

	gui_opengl:insert_debug_context_message( 8, "Third message.", low,
		application, type_error ),

	Msgs = [ _Msg2, _Msg3 ] = gui_opengl:get_debug_context_messages(),

	trace_utils:debug_fmt( "Next two debug context messages: ~ts",
		[ gui_opengl:debug_context_messages_to_string( Msgs ) ] ),

	[] = gui_opengl:get_debug_context_messages(),

	true = gui_opengl:is_debug_context_enabled(),
	gui_opengl:disable_all_debug_context_reporting(),

	% Still enabled:
	%false = gui_opengl:is_debug_context_enabled(),

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
