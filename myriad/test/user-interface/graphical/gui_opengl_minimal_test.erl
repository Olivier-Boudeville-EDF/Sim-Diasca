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
% Creation date: Saturday, December 25, 2021.


% @doc Minimal testing of the <b>OpenGL support</b>; displays a white rectangle
% on a black background.
%
% It is therefore a non-interactive, passive test (no spontaneous/scheduled
% behaviour) whose main interest is to show a simple yet generic, appropriate
% structure in order to properly initialise the GUI and OpenGL, handle
% rendering, resizing and closing.
%
% See the gui_opengl.erl tested module.
%
-module(gui_opengl_minimal_test).


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
-record( my_test_state, {

	% The main window of this test:
	parent :: window(),

	% The OpenGL canvas on which rendering will be done:
	canvas :: gl_canvas(),

	% The OpenGL context being used:
	context :: gl_context(),

	opengl_initialised = false :: boolean() } ).

-type my_test_state() :: #my_test_state{}.
% Test-specific overall GUI state.



% Shorthands:

%-type dimensions() :: gui:dimensions().
-type window() :: gui:window().

-type gl_canvas() :: gui:opengl_canvas().
-type gl_context() :: gui:opengl_context().



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

	InitialGUIState = init_test_gui(),

	gui:show( InitialGUIState#my_test_state.parent ),

	% OpenGL will be initialised only when the corresponding frame will be ready
	% (that is once first reported as resized):
	%
	gui_main_loop( InitialGUIState ),

	gui:stop().



% @doc Creates the initial test GUI: a main frame containing an OpenGL canvas is
% associated, in which an OpenGL context is created.
%
% Once the rendering is done, the buffer are swapped and it is displayed.
%
init_test_gui() ->

	MainFrame = gui:create_frame( "MyriadGUI Minimal OpenGL Test" ),

	% Using default GL attributes:
	GLCanvas = gui_opengl:create_canvas( _Parent=MainFrame ),

	% Created, yet not bound yet:
	GLContext = gui_opengl:create_context( GLCanvas ),

	gui:subscribe_to_events( { [ onResized, onShown, onWindowClosed ],
							   MainFrame } ),

	% No OpenGL state yet (GL context not set as current yet):
	#my_test_state{ parent=MainFrame, canvas=GLCanvas, context=GLContext }.



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages.
%
-spec gui_main_loop( my_test_state() ) -> void().
gui_main_loop( GUIState=#my_test_state{ parent=ParentWindow } ) ->

	%trace_utils:debug( "Main loop." ),

	% Matching the least-often received messages last:
	receive

		% For a window, the first resizing event happens (just) before its
		% onShown one:
		%
		{ onResized, [ ParentWindow, _NewParentSize, _EventContext ] } ->

			%trace_utils:debug_fmt( "Resizing of the parent window to ~w "
			%   "detected.", [ NewParentSize ] ),

			case GUIState#my_test_state.opengl_initialised of

				true ->
					on_main_frame_resized( GUIState );

				% Not ready yet:
				false ->
					trace_utils:debug( "Resized, yet no OpenGL state yet." ),
					ok

			end,

			gui_main_loop( GUIState );


		% The most suitable first location to initialise OpenGL, as making a GL
		% context current requires a shown window:
		%
		{ onShown, [ ParentWindow, _EventContext ] } ->

			trace_utils:debug_fmt( "Parent window just shown "
				"(initial size of ~w).", [ gui:get_size( ParentWindow ) ] ),

			% Done once for all:
			NewGUIState = setup_opengl( GUIState ),

			gui_main_loop( NewGUIState );


		{ onWindowClosed, [ ParentWindow, _EventContext ] } ->
			trace_utils:info( "Main frame closed, test success." ),
			gui:destruct_window( ParentWindow );


		OtherEvent ->
			trace_utils:warning_fmt( "Test ignored following event:~n ~p",
									 [ OtherEvent ] ),

			gui_main_loop( GUIState )

	end.




% @doc Sets up OpenGL, once a proper current context has been set.
-spec setup_opengl( my_test_state() ) ->  my_test_state().
setup_opengl( TestState=#my_test_state{ parent=_MainFrame,
										canvas=GLCanvas,
										context=GLContext,
										% Check:
										opengl_initialised=false } ) ->

	% Initial size of canvas is typically 20x20 pixels:
	trace_utils:debug_fmt( "Setting up OpenGL (whereas canvas is of initial "
						   "size ~w).", [ gui:get_size( GLCanvas ) ] ),

	gui_opengl:set_context( GLCanvas, GLContext ),

	% These settings will not change afterwards (set once for all):

	% Clears in black:
	gl:clearColor( 0.0, 0.0, 0.0, 0.0 ),

	% Draws in white:
	gl:color3f( 1.0, 1.0, 1.0 ),

	gl:matrixMode( ?GL_PROJECTION ),
	gl:loadIdentity(),

	% Multiplies the current modelview matrix by an orthographic matrix, a
	% perspective matrix that produces a parallel projection based on 6 clipping
	% planes:
	%
	gl:ortho( _Left=0.0, _Right=1.0, _Bottom=0.0, _Top=1.0, _Near=-1.0,
			  _Far=1.0 ),

	%trace_utils:debug_fmt( "Managing a resize of the main frame to ~w.",
	%                       [ gui:get_size( MainFrame ) ] ),

	% Used here and when an actual resizing takes place:
	on_main_frame_resized( TestState ),

	TestState#my_test_state{ opengl_initialised=true }.



% @doc Managing a resizing of the main frame.
-spec on_main_frame_resized( my_test_state() ) -> void().
on_main_frame_resized( #my_test_state{ canvas=GLCanvas } ) ->

	% Maximises then canvas in the main frame:
	{ CanvasWidth, CanvasHeight } = gui:maximise_in_parent( GLCanvas ),

	%trace_utils:debug_fmt( "New client canvas size: {~B,~B}.",
	%                       [ CanvasWidth, CanvasHeight ] ),

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

	render(),

	gui_opengl:swap_buffers( GLCanvas ).



% @doc Performs a (pure OpenGL) rendering.
-spec render() -> void().
render() ->

	gl:clear( ?GL_COLOR_BUFFER_BIT ),

	% A white rectangle in the z=0 plane (in a black background):
	gl:'begin'( ?GL_POLYGON ),
		gl:vertex3f( 0.25, 0.25, 0.0 ),
		gl:vertex3f( 0.75, 0.25, 0.0 ),
		gl:vertex3f( 0.75, 0.75, 0.0 ),
		gl:vertex3f( 0.25, 0.75, 0.0 ),
	gl:'end'(),

	% Ensures that the drawing commands are actually executed, rather than
	% stored in a buffer awaiting additional OpenGL commands:
	%
	gl:flush().



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
