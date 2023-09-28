% Copyright (C) 2021-2023 Olivier Boudeville
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
% Creation date: Monday, December 27, 2021.


% @doc Testing of the <b>OpenGL support in a MVC setting</b>, evaluated
% concurrently at fixed, independent frequencies; in practice displays a
% textured rotating square.
%
% It is therefore a non-interactive, active test (the square is scheduled, so
% that it spontaneously rotates) whose main interest is to show a simple yet
% generic, appropriate structure in order to properly initialise the GUI and
% OpenGL, handle rendering, resizing and closing, and integrate a MVC-style
% (Model-View-Controller) parallel logic, with the model scheduling mostly
% uncoupled from the one of the rendering, i.e. a Model running in a different
% process from the View and from the test itself. This test being
% non-interactive, only a very basic controller exists, notably to listen to
% window-related events.
%
% This test relies on the OpenGL 1.x compatibility mode, as opposed to more
% modern versions of OpenGL (e.g. 3.1) that rely on shaders and GLSL.
%
% See the gui_opengl.erl tested module and
% https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller for the
% MVC pattern.
%
-module(gui_opengl_mvc_test).


% Implementation notes:
%
% Inspired from https://www.glprogramming.com/red/chapter01.html


% For GL/GLU defines; the sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").



-record( model_state, {

	% The current angle of the spinning square.
	spin_angle = 0.0 :: unit_utils:degrees(),

	% The (approximate) number of milliseconds between two schedulings of the
	% model.
	%
	scheduling_period :: milliseconds() } ).

-type model_state() :: #model_state{}.
% Stores the current state of the Model, that is its logic, which here mostly
% manages the current rotating angle of the square.
%
% The model does not know any controller or view, nor the test itself.


-record( view_state, {

	% The main frame of this test:
	main_frame :: frame(),

	% The OpenGL canvas on which rendering will be done:
	canvas :: gl_canvas(),

	% The OpenGL context being used:
	context :: gl_context(),

	% The various OpenGL information kept by this test once initialised:
	%
	% (keeping around this texture is not necessary; a mere atom could have
	% sufficed)
	%
	opengl_state :: maybe( texture() ),

	% The model must be known, in order to fetch relevant information from it.
	model_pid :: model_pid(),

	% The controller process is kept, even if not necessary once the view
	% notified it of the main frame (so that the controller can detect its
	% closing).
	%
	controller_pid :: controller_pid(),

	% The (approximate) number of milliseconds between two schedulings of the
	% view.
	%
	scheduling_period :: milliseconds() } ).

-type view_state() :: #view_state{}.
% Stores the current state of the View, that is all relevant rendering
% information.
%
% The view mostly knowns the model.



-record( controller_state, {

	% The model is known, in order to be notified of any relevant information.
	model_pid :: model_pid(),

	% The view is known, in order to notify it of any termination.
	view_pid :: view_pid(),

	% The PID of the test process, to control it (e.g. if user requested to
	% exit).
	%
	test_pid :: test_pid() } ).

-type controller_state() :: #controller_state{}.
% Stores the current state of the Controller, that is its inputs.
%
% The controller knows the view and the test, so that it can drive the
% termination and notify them of it.
%
% It also knows the model, even if it is not necessary for this test (as no user
% input is to impact the model).


-type test_pid() :: pid().
% PID of the main test process.


% Shorthands:

-type frame() :: gui_frame:frame().

-type any_hertz() :: unit_utils:any_hertz().
-type milliseconds() :: unit_utils:milliseconds().

-type model_pid() :: gui:model_pid().
-type view_pid() :: gui:view_pid().
-type controller_pid() :: gui:controller_pid().


-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().
-type texture() :: gui_opengl:texture().



% @doc Runs the OpenGL MVC test if possible.
-spec run_opengl_mvc_test() -> void().
run_opengl_mvc_test() ->

	test_facilities:display( "~nStarting the OpenGL MVC test." ),

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

	test_facilities:display( "Starting the actual OpenGL MyriadGUI "
		"MVC test, from user process ~w.", [ self() ] ),

	trace_utils:notice( "A rotating textured square will be rendered, "
		"managed according to the MVC design pattern, "
		"until the frame is closed by the user." ),

	% Evaluates the model at 40 Hz, as it must be scheduled for its spontaneous
	% behaviour:
	%
	ModelEvalFreq = 40,

	% Evaluates the view at 60 Hz, as rendering must be spontaneously updated:
	ViewEvalFreq = 60,

	% Controller is event-driven (no polling needed).

	% A model is standalone:
	ModelPid = spawn_link( fun() -> run_model( ModelEvalFreq ) end ),

	% Beware, closure:
	TestPid = self(),

	% A controller may (in general) have to send notifications to the model, and
	% is to notify the test of its end as well:
	%
	ControllerPid = spawn_link(
		fun() -> run_controller( ModelPid, TestPid ) end ),

	% A view depends on the model; it also knows the controller, so that it can
	% send it a reference to the main frame (as the controller has to subscribe
	% to it in order to monitor any closing thereof):
	%
	_ViewPid = spawn_link(
		fun() -> run_view( ViewEvalFreq, ModelPid, ControllerPid ) end ),

	% Just block from now:
	trace_utils:debug_fmt( "[~w] Test process waiting for the controller "
		"to report when the test is finished.", [ TestPid ] ),

	receive

		onTestSuccess ->

			trace_utils:debug_fmt( "[~w] Test reported by the controller "
				"as having finished successfully.", [ TestPid ] ),

			% The controller is already driving the (asynchronous) termination
			% of the model and of the view by itself.

			trace_utils:debug_fmt( "[~w] Test finished.", [ TestPid ] ),

			test_facilities:stop()

	end.



% Model section.


% @doc Runs the model; initialises it and runs its main loop.
-spec run_model( any_hertz() ) -> no_return().
run_model( EvalFrequency ) ->

	trace_utils:debug_fmt( "[~w] Model started.", [ self() ] ),

	Period = time_utils:frequency_to_period( EvalFrequency ),

	% Using default initial spin angle:
	InitialModelState = #model_state{ scheduling_period=Period },

	trace_utils:debug_fmt( "[~w] Model entering its main loop "
		"(evaluation frequency: ~B Hz, hence a period of ~B ms).",
		[ self(), EvalFrequency, Period ] ),

	model_main_loop( InitialModelState ).



% @doc Main loop of the model logic.
-spec model_main_loop( model_state() ) -> no_return().
model_main_loop( ModelState=#model_state{ spin_angle=Angle,
										  scheduling_period=MsPeriod } ) ->

	MsStartScheduling = time_utils:get_monotonic_time(),

	%trace_utils:debug_fmt( "[~w] Model main loop.", [ self() ] ),

	NewAngle = case Angle + 2.0 of

		A when A > 360.0 ->
			A - 360.0;

		A ->
			A

	end,

	SpinModelState = ModelState#model_state{ spin_angle=NewAngle },

	%trace_utils:debug_fmt( "[~w] New model spin angle: ~f°.",
	%                       [ self(), NewAngle ] ),

	% Processes any pending requests (typically from the view or the
	% controller), in a non-blocking manner:
	%
	case handle_pending_model_requests( SpinModelState ) of

		model_terminated ->
			% Not recursing anymore, hence terminating:
			trace_utils:debug_fmt( "[~w] Model terminated.", [ self() ] ),
			model_terminated;

		ReqModelState ->
			% Enforce model frequency:
			time_utils:wait_period_ending( MsStartScheduling, MsPeriod ),

			model_main_loop( ReqModelState )

	end.



% @doc Handles any model-level pending requests.
-spec handle_pending_model_requests( model_state() ) ->
				model_state() | 'model_terminated'.
handle_pending_model_requests( ModelState ) ->

	receive

		% As a matter of fact, complies (purposedly) to the WOOPER
		% object-oriented conventions (see http://wooper.esperide.org):
		%
		{ getSpinAngle, _Args=[], SenderPid } ->
			SenderPid ! { notifySpinAngle, ModelState#model_state.spin_angle },
			% As in the general case multiple requests might be pending:
			handle_pending_model_requests( ModelState );


		% Typically sent by the controller:
		{ onModelTermination, SenderPid } ->

			trace_utils:debug_fmt( "[~w] Model notified of termination.",
								   [ self()] ),

			% No specific termination for models:
			SenderPid ! model_terminated;

		OtherEvent ->
			trace_utils:warning_fmt( "[~w] Model ignored the following "
				"event:~n ~w", [ self(), OtherEvent ] ),

			handle_pending_model_requests( ModelState )


	after 0 ->

		ModelState

	end.



% View section.

% @doc Runs the view; initialises it and runs its main loop.
%
% No OpenGL outside of the view!
%
-spec run_view( any_hertz(), model_pid(), controller_pid() ) -> no_return().
run_view( EvalFrequency, ModelPid, ControllerPid ) ->

	trace_utils:debug_fmt( "[~w] View started.", [ self() ] ),

	gui:start(),

	GUIViewState = init_test_gui(),

	MainFrame = GUIViewState#view_state.main_frame,

	ControllerPid ! { notifyViewInformation,
						[ self(), MainFrame, gui:get_backend_environment() ] },

	Period = time_utils:frequency_to_period( EvalFrequency ),

	% Using default initial spin angle:
	InitialViewState = GUIViewState#view_state{ model_pid=ModelPid,
												controller_pid=ControllerPid,
												scheduling_period=Period },

	trace_utils:debug_fmt( "[~w] View entering its main loop "
		"(evaluation frequency: ~B Hz, hence a period of ~B ms).",
		[ self(), EvalFrequency, Period ] ),

	% OpenGL will be initialised only when the corresponding frame will be ready
	% (that is once it is reported as shown):
	%
	view_main_loop( InitialViewState ).



% @doc Main loop of the view logic.



% @doc Creates the initial test GUI: a main frame containing an OpenGL canvas is
% associated, in which an OpenGL context is created.
%
% Once the rendering is done, the buffers are swapped and the current one is
% displayed.
%
-spec init_test_gui() -> view_state().
init_test_gui() ->

	% Initial square size preferred to avoid distortion of the square:
	MainFrame = gui_frame:create( "MyriadGUI MVC OpenGL Test", { 400, 400 } ),

	% Using default GL attributes:
	GLCanvas = gui_opengl:create_canvas( _Parent=MainFrame ),

	% Created, yet not bound yet:
	GLContext = gui_opengl:create_context( GLCanvas ),

	% We consider here that the following events are view-specific, rather than
	% controller-specific:
	%
	gui:subscribe_to_events( { [ onResized, onShown ], MainFrame } ),

	% Needed, otherwise if that frame is moved out of the screen or if another
	% windows overlaps, the OpenGL canvas gets garbled and thus must be redrawn:
	%
	gui:subscribe_to_events( { onRepaintNeeded, GLCanvas } ),

	gui_frame:show( MainFrame ),

	% No OpenGL state yet (GL context not set as current yet):
	#view_state{ main_frame=MainFrame, canvas=GLCanvas, context=GLContext }.



% @doc The main loop of the test view, driven by the receiving of MyriadGUI
% rendering-related messages.
%
-spec view_main_loop( view_state() ) -> void().
view_main_loop( ViewState=#view_state{ scheduling_period=MsPeriod } ) ->

	MsStartScheduling = time_utils:get_monotonic_time(),

	%trace_utils:debug_fmt( "[~w] View main loop.", [ self() ] ),

	case handle_pending_view_events( ViewState ) of

		view_terminated ->
			% Not recursing anymore, hence terminating:
			trace_utils:debug_fmt( "[~w] View terminated.", [ self() ] );

		UpdatedViewState ->

			render_view( UpdatedViewState ),

			% Enforce view frequency:
			time_utils:wait_period_ending( MsStartScheduling, MsPeriod ),

			view_main_loop( UpdatedViewState )

	end.



% @doc Handles any view-level pending events.
-spec handle_pending_view_events( view_state() ) ->
									view_state() | 'view_terminated'.
%handle_pending_view_events( ViewState=#view_state{ main_frame=MainFrame } ) ->
handle_pending_view_events( ViewState ) ->

	% Matching the least-often received messages last:
	receive

		% Not strictly necessary, as anyway a regular redraw is to happen soon
		% afterwards:
		%
		{ onRepaintNeeded, [ GLCanvas, _GLCanvasId, _EventContext ] } ->

			%trace_utils:debug_fmt( "Repaint needed for OpenGL canvas ~w.",
			%                       [ GLCanvas ] ),

			RepaintedViewState = case ViewState#view_state.opengl_state of

				% Not ready yet:
				undefined ->
					trace_utils:debug(
						"To be repainted, yet no OpenGL state yet." ),
					ViewState;

				_GLState ->
					gui_widget:enable_repaint( GLCanvas ),
					% Includes the GL flushing and the buffer swaping:
					render_view( ViewState ),
					ViewState

			end,
			handle_pending_view_events( RepaintedViewState );


		% For a window, the first resizing event happens (just) before its
		% onShown one:
		%
		{ onResized, [ _ParentWindow, _ParentWindowId, NewParentSize,
					   _EventContext ] } ->

			trace_utils:debug_fmt( "Resizing of the parent window to ~w "
				"detected.", [ NewParentSize ] ),

			ResizedViewState = case ViewState#view_state.opengl_state of

				% Not ready yet:
				undefined ->
					trace_utils:debug( "Resized, yet no OpenGL state yet." ),
					ViewState;

				_ ->
					on_main_frame_resized( ViewState )

			end,

			handle_pending_view_events( ResizedViewState );


		% The most suitable first location to initialise OpenGL, as making a GL
		% context current requires a shown window:
		%
		{ onShown, [ ParentWindow, _ParentWindowId, _EventContext ] } ->

			trace_utils:debug_fmt( "Parent window just shown "
				"(initial size of ~w).",
				[ gui_widget:get_size( ParentWindow ) ] ),

			NewViewState = initialise_opengl( ViewState ),

			on_main_frame_resized( NewViewState ),

			handle_pending_view_events( NewViewState );


		% As an example, onWindowClosed is listened to by the controller.


		% Sent by the controller:
		{ onViewTermination, SenderPid } ->

			trace_utils:info_fmt( "[~w] View notified of termination.",
								  [ self() ] ),

			% Very final check, while there is still an OpenGL context:
			gui_opengl:check_error(),

			gui_frame:destruct( ViewState#view_state.main_frame ),

			gui:stop(),
			SenderPid ! view_terminated;


		OtherEvent ->
			trace_utils:warning_fmt( "[~w] View ignored the following "
				"event:~n ~w", [ self(), OtherEvent ] ),

			handle_pending_view_events( ViewState )


	after 0 ->

		ViewState

	end.



% @doc Sets up OpenGL, once for all, once a proper OpenGL context is available.
-spec initialise_opengl( view_state() ) -> view_state().
initialise_opengl( ViewState=#view_state{ canvas=GLCanvas,
										  context=GLContext,
										  % Check:
										  opengl_state=undefined } ) ->

	% Initial size of canvas is typically 20x20 pixels:
	trace_utils:debug_fmt(
		"[~w] Initialising OpenGL (whereas canvas is of initial size ~w).",
		[ self(), gui_widget:get_size( GLCanvas ) ] ),

	gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

	% These settings will not change afterwards (set once for all):

	% Clears in grey rather than black:
	gl:clearColor( 0.5, 0.5, 0.5, 0.0 ),

	% No smooth shading wanted here:
	gl:shadeModel( ?GL_FLAT ),

	% Draws in white:
	gl:color3f( 1.0, 1.0, 1.0 ),

	% These settings apparently still apply after a resize/viewport change,
	% hence are done only once:

	gl:matrixMode( ?GL_PROJECTION ),
	gl:loadIdentity(),

	% Multiplies the current modelview matrix by an orthographic matrix, a
	% perspective matrix that produces a parallel projection based on 6 clipping
	% planes:
	%
	% (here does not depend on viewport size, so can be done once now)
	%
	gl:ortho( _Left=-50.0, _Right=50.0, _Bottom=-50.0, _Top=50.0,
			  _Near=-1.0, _Far=1.0 ),

	%trace_utils:debug_fmt( "Managing a resize of the main frame to ~w.",
	%                       [ gui:get_size( MainFrame ) ] ),

	gl:enable( ?GL_TEXTURE_2D ),

	ImagePath = file_utils:join(
		gui_opengl_direct_integration_test:get_test_image_directory(),
		"myriad-space-time-referential.png" ),

	% Not directly 'Texture = gui_texture:load_from_file(ImgPath)' as we want to
	% flip the image:

	Image = gui_image:load_from_file( ImagePath ),
	MirroredImage = gui_image:mirror( Image, _Orientation=horizontal ),
	Texture = gui_texture:create_from_image( MirroredImage ),
	gui_image:destruct_multiple( [ Image, MirroredImage ] ),

	InitViewState = ViewState#view_state{ opengl_state=Texture },

	% As the initial onResized was triggered whereas no OpenGL state was
	% already available:
	%
	on_main_frame_resized( InitViewState ).



% @doc Managing a resizing of the main frame.
%
% OpenGL context expected here to have already been set.
%
-spec on_main_frame_resized( view_state() ) -> view_state().
on_main_frame_resized( ViewState=#view_state{ canvas=GLCanvas } ) ->

	% OpenGL state expected to be ready here:
	%basic_utils:check_defined( Texture ),

	% Maximises then canvas in the main frame:
	{ CanvasWidth, CanvasHeight } = gui_widget:maximise_in_parent( GLCanvas ),

	trace_utils:debug_fmt( "[~w] View with a new client canvas size: {~B,~B}.",
						   [ self(), CanvasWidth, CanvasHeight ] ),

	% Apparently, at least on a test setting, a race condition (discovered
	% thanks to the commenting-out of a debug trace) seems to exist between the
	% moment when the canvas is resized and the one when a new OpenGL rendering
	% is triggered afterwards; the cause is probably that maximising involves an
	% (Erlang) asynchronous message to be sent from this user process and to be
	% received and applied by the process of the target window, whereas a GL
	% (NIF-based) operation is immediate; without a sufficient delay, the
	% rendering will thus take place according to the former (e.g. minimised)
	% canvas size, not according to the one that was expected to be already
	% resized. Here it does not matter much as the canvas is regularly redrawn
	% (not rendered only once).
	%
	% (actual rendering and buffer swapping to be done just afterwards)
	%
	gui_widget:sync( GLCanvas ),

	% Lower-left corner and size of the viewport in the current window:
	gl:viewport( 0, 0, CanvasWidth, CanvasHeight ),

	% No specific other view-related update.

	% Includes the swapping of buffers:
	render_view( ViewState ),

	% Const:
	ViewState.



% @doc Performs a ("pure OpenGL") rendering of the view.
-spec render_view( view_state() ) -> void().
render_view( #view_state{ opengl_state=undefined } ) ->
	% Not ready yet, no GL context available before the main frame is shown:
	ok;

render_view( #view_state{ canvas=GLCanvas,
						  opengl_state=#texture{ id=TextureId,
												 width=Width,
												 height=Height,
												 min_x=MinXt,
												 min_y=MinYt,
												 max_x=MaxXt,
												 max_y=MaxYt },
						  model_pid=ModelPid } ) ->

	%trace_utils:debug_fmt( "[~w] View rendering now.", [ self() ] ),

	% Trying to interleave as much as possible:

	% (here, by design, even when terminating, the view is stopped synchronously
	% before the model is; otherwise the next call could never receive an
	% answer)
	%
	ModelPid ! { getSpinAngle, _Args=[], self() },

	gl:clear( ?GL_COLOR_BUFFER_BIT ),

	gl:pushMatrix(),

	_MidEdgeLen = 25.0,

	% In answer to getSpinAngle:
	CurrentAngle = receive

		{ notifySpinAngle, Angle } ->
			Angle

	end,

	%trace_utils:debug_fmt( "[~w] View rotation: ~f.",
	%                       [ self(), CurrentAngle ] ),

	% Note that we rotate from scratch directly to the target angle, instead of
	% rotating incrementally from the previous matrix each time (otherwise
	% numercial errors would soon accumulate):
	%
	gl:rotatef( CurrentAngle, 0.0, 0.0, 1.0 ),

	gui_texture:set_as_current_from_id( TextureId ),

	%gl:rectf( -MidEdgeLen, -MidEdgeLen, MidEdgeLen, MidEdgeLen ),

	% Not using 'gui_texture:render(Texture, {0,0}),' as we want smaller
	% dimensions:

	gl:'begin'( ?GL_TRIANGLE_STRIP ),

	Xp = Yp = 0,

	OtherXp = Xp + Width div 4,
	OtherYp = Yp + Height div 4,

	% Associating a (2D) texture coordinate to each vertex:
	gl:texCoord2f( MinXt, MinYt ), gl:vertex2i( Xp,      Yp ),
	gl:texCoord2f( MaxXt, MinYt ), gl:vertex2i( OtherXp, Yp ),
	gl:texCoord2f( MinXt, MaxYt ), gl:vertex2i( Xp,      OtherYp ),
	gl:texCoord2f( MaxXt, MaxYt ), gl:vertex2i( OtherXp, OtherYp ),

	gl:'end'(),

	gl:popMatrix(),

	% Can be done here, as window-related (actually: GLCanvas) information were
	% already necessary anyway; includes a gl:flush/0:
	%
	gui_opengl:swap_buffers( GLCanvas ).





% Controller section.


% @doc Runs the controller; initialises it and runs its main loop.
%
% No specific polling frequency applies.
%
% A more complex controller would manage user entries (mouse, keyboard,
% joystick, etc.).
%
-spec run_controller( model_pid(), test_pid() ) -> no_return().
run_controller( ModelPid, TestPid ) ->

	trace_utils:debug_fmt( "[~w] Controller started.", [ self() ] ),

	receive

		% Expected to be triggered by the view:
		{ notifyViewInformation, [ ViewPid, MainFrame, BackendEnv ] } ->

			trace_utils:debug_fmt( "[~w] Controller received view-related "
								   "information.", [ self() ] ),

			% So that this controller can interact with the backend used by a
			% MyriadGUI:
			%
			gui:set_backend_environment( BackendEnv ),

			% We consider here that the following event is controller-specific,
			% rather than view-specific:
			%
			% (this event type does not propagate, thus the controller will be
			% able to solely control a proper termination)
			%
			gui:subscribe_to_events( { onWindowClosed, MainFrame } ),

			% Not storing specifically the main frame:
			InitialControllerState = #controller_state{ model_pid=ModelPid,
														view_pid=ViewPid,
														test_pid=TestPid },

			controller_main_loop( InitialControllerState )

	end.



% @doc The main loop of the controller process, driven by the receiving of
% MyriadGUI messages relating to user inputs.
%
-spec controller_main_loop( controller_state() ) -> void().
controller_main_loop( ControllerState ) ->

	trace_utils:debug_fmt( "[~w] Controller main loop.", [ self() ] ),

	receive

		{ onWindowClosed, [ _ParentWindow, _ParentWindowId, _EventContext ] } ->
			trace_utils:info_fmt( "[~w] Controller notified of the "
				"closing of the main frame, test success.", [ self() ] ),

			% We stop (synchronously) the view *before* the model, as the view
			% sends (blocking) requests to it (see the sending of a getSpinAngle
			% message in render_view/1) and thus could end up waiting for an
			% answer that would never be received:
			%
			ControllerState#controller_state.view_pid !
				{ onViewTermination, self() },

			receive

				view_terminated ->
					%trace_utils:debug( "Ack received for view termination" ),
					ok

			end,

			ControllerState#controller_state.model_pid !
				{ onModelTermination, self() },

			receive

				model_terminated ->
					%trace_utils:debug( "Ack received for model termination" ),
					ok

			end,

			% No more recursing:
			ControllerState#controller_state.test_pid ! onTestSuccess,

			trace_utils:debug_fmt( "[~w] Controller terminated.", [ self() ] );


		OtherEvent ->
			trace_utils:warning_fmt( "[~w] Controller ignored the following "
				"event:~n ~w", [ self(), OtherEvent ] ),
			controller_main_loop( ControllerState )

	end.



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the OpenGL MVC test, being in batch mode)" );

		false ->
			run_opengl_mvc_test()

	end,

	test_facilities:stop().
