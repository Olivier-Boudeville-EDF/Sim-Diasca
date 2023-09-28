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
% Creation date: Wednesday, February 2, 2022.


% @doc Testing of <b>matrix support</b> in relation to OpenGL, to ensure that
% they can be handled as expected.
%
% This test relies on the OpenGL 1.x compatibility mode, as opposed to more
% modern versions of OpenGL (e.g. 3.1) that rely on shaders and GLSL.
%
% See the gui_opengl.erl tested module.
%
-module(gui_opengl_matrix_test).



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



% Shorthands:

-type frame() :: gui_frame:frame().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().



% @doc Runs the OpenGL test if possible.
-spec run_opengl_test() -> void().
run_opengl_test() ->

	test_facilities:display( "~nStarting the test of OpenGL Matrix support." ),

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

	test_facilities:display( "This test will perform basic OpenGL-related "
		"matrix operations; no specific graphical output shall be expected "
		"(but OpenGL must be properly initialised), except a blank, "
		"non-updated window. "
		"Note that finally, at least currently, the matrix support cannot "
		"be tested due to the lack of solution in order to extract a "
		"matrix from OpenGL." ),

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

	MainFrame = gui_frame:create( "MyriadGUI OpenGL Matrix Test",
								  _Size={ 500, 250 } ),

	% Using default GL attributes:
	GLCanvas = gui_opengl:create_canvas( _Parent=MainFrame ),

	% Created, yet not bound yet (must wait for the main frame to be shown):
	GLContext = gui_opengl:create_context( GLCanvas ),

	% Minimal registering:
	gui:subscribe_to_events( { [ onShown, onWindowClosed ], MainFrame } ),

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


		% The most suitable first location to initialise OpenGL, as making a GL
		% context current requires a shown window:
		%
		{ onShown, [ ParentWindow, _ParentWindowId, _EventContext ] } ->

			trace_utils:debug_fmt( "Parent window (main frame) just shown "
				"(initial size of ~w).",
				[ gui_widget:get_size( ParentWindow ) ] ),

			% Done once for all:
			InitGUIState = initialise_opengl( GUIState ),

			test_matrices(),

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
						   "size ~w).", [ gui_widget:get_size( GLCanvas ) ] ),

	% So done only once:
	gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

	% No specific initialisation needed.

	InitGUIState = GUIState#my_gui_state{ opengl_initialised=true },

	% As the initial onResized was triggered whereas no OpenGL state was
	% already available:
	%
	on_main_frame_resized( InitGUIState ).



% @doc Performs the actual matrix test.
-spec test_matrices() -> void().
test_matrices() ->

	% We show that our usual matrices, respecting Myriad conventions, can
	% integrate smoothly with OpenGL:
	%
	% Octave: M = [1, 2, 3, 4; 5, 6, 7, 8; 9, 10, 11, 12; 13, 14, 15, 16]
	M = matrix4:new( [ [ 1,   2,  3,  4 ],
					   [ 5,   6,  7,  8 ],
					   [ 9,  10, 11, 12 ],
					   [ 13, 14, 15, 16 ] ] ),

	% Selects this matrix (must be already the case):
	gl:matrixMode( ?GL_MODELVIEW ),

	gui_opengl:set_matrix( M ),

	% Octave: A = [ 1, 0, -1, 0; 0, -2, 0, 2; 3, 4, 3, 4; 5, 9, 7, 1]
	A = matrix4:new( [ [ 1,  0, -1, 0 ],
					   [ 0, -2,  0, 2 ],
					   [ 3,  4,  3, 4 ],
					   [ 5,  9,  7, 1 ] ] ),

	MpCeylan = matrix4:mult( M, A ),

	% We consider it is post-multiplication (M' = M.A):
	gl:multMatrixf( matrix4:to_tuple( A ) ),

	% Extra verification: Octave: M' = M.A =
	MpOctave = matrix4:new( [ [  30,  44,  36,  20 ],
							  [  66,  88,  72,  48 ],
							  [ 102, 132, 108,  76 ],
							  [ 138, 176, 144, 104 ] ] ),

	true = matrix4:are_equal( MpCeylan, MpOctave ),

	% Problem: apparently no way of extracting the current matrix from OpenGL.
	% Not existing: MpGL = gl:get( ?GL_MODELVIEW_MATRIX)
	%
	%true = matrix4:are_equal( MpCeylan, MpGL ).
	%throw( no_test_performed).
	ok.



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

	% Not changing model-view or projection.

	% Const here:
	GUIState.



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
