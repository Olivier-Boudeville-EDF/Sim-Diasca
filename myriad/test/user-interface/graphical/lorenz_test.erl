% Copyright (C) 2014-2022 Olivier Boudeville
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


% Unit test mostly for the <b>canvas facility</b>, based on the Lorenz equations
% to show its strange attractor (and also test the `rk4_solver' module).
%
-module(lorenz_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For gui-related defines:
-include("gui.hrl").


-export([ get_name/1, draw_lines/3 ]).



% Rendering section.


% Description of a simple, local, screen coordinate system:
-record( screen, {

   center :: integer_point2(),

   zoom_x :: zoom_factor(),
   zoom_y :: zoom_factor() } ).

-type screen() :: #screen{}.


-type widget_name() :: atom().


-type solver_table() :: table( solver_pid(), { color(), point3() } ).
% The solver table is an associative table whose keys are the PID of each
% solver, and whose values are {Color, LastPoint} pairs.



% Shorthands:

-type message() :: basic_utils:message().

-type zoom_factor() :: math_utils:factor().

-type color() :: gui_color:color().

-type coordinate() :: linear:coordinate().

-type integer_point2() :: point2:integer_point2().

-type point3() :: point3:point3().

-type time() :: rk4_solver:time().

-type time_step() :: time().

-type f3p() :: rk4_solver:f3p().

-type solver_pid() :: pid().



% @doc Resolves the specified equations based on the specified initial
% conditions and derivate function, notifying the specified listener of the new
% computations.
%
% Main loop of a solver instance.
%
% In we are in 3D here:
-spec solver_main_loop( f3p(), point3(), time(), time_step(), screen(),
						pid() ) -> no_return().
solver_main_loop( F, CurrentPoint, CurrentTime, Timestep, Screen,
				  ListenerPid ) ->
	% Initially not started:
	solver_main_loop( F, CurrentPoint, CurrentTime, Timestep, Screen,
					  ListenerPid, _TimeOut=infinity ).

% (helper)
solver_main_loop( F, CurrentPoint, CurrentTime, Timestep, Screen,
				  ListenerPid, TimeOut ) ->


	receive

		{ set_time_step, NewTimestep } ->

			% Whatever the units may be:
			%trace_utils:debug_fmt( "Changing time step from ~p to ~p.",
			%                       [ Timestep, NewTimestep ] ),

			solver_main_loop( F, CurrentPoint, CurrentTime, NewTimestep, Screen,
							  ListenerPid, TimeOut );

		{ set_current_point, NewPoint } ->
			solver_main_loop( F, NewPoint, CurrentTime, Timestep, Screen,
							  ListenerPid, TimeOut );


		start ->
			% Explicit yielding, not wanting to overheat:
			solver_main_loop( F, CurrentPoint, CurrentTime, Timestep, Screen,
							  ListenerPid, _TimeOut=10 );

		stop ->
			solver_main_loop( F, CurrentPoint, CurrentTime, Timestep, Screen,
							  ListenerPid, _TimeOut=infinity)


	after TimeOut ->

		% The basic version relied on one message per point, inducing of course
		% a lot of overhead:

		% New point is yn+1, current point is yn, timestep is h:
		%
		%NewPoint = compute_next_estimate( F, CurrentPoint, CurrentTime,
		%                                  Timestep ),

		%trace_utils:debug_fmt( "- new point computed: ~p", [ NewPoint ] ),

		%ListenerPid ! { draw_point, NewPoint, self() },


		% New version: sending a list of PointCount points at once, and moreover
		% having already projected them on screen coordinates:

		PointCount = 50,

		{ NewProjectedPoints, LastPoint, NewTime } =
			compute_next_estimates( F, CurrentPoint, CurrentTime, Timestep,
									Screen, PointCount ),

		%trace_utils:debug_fmt( "Computed following points: ~w.",
		%                       [ NewProjectedPoints ] ),

		ListenerPid ! { draw_points, NewProjectedPoints, self() },

		solver_main_loop( F, LastPoint, NewTime, Timestep, Screen, ListenerPid,
						  TimeOut )

	end.



% @doc Returns a list of the next PointCount projected points, the last point
% computed and the corresponding next current time.
%
compute_next_estimates( F, Point, Time, Timestep, Screen, PointCount ) ->
	% Clearer than a fold:
	compute_next_estimates( F, Point, Time, Timestep, Screen, PointCount,
							_Acc=[] ).


% (helper)
compute_next_estimates( _F, Point, NextTime, _Timestep, _Screen, _PointCount=0,
						Acc ) ->
	{ lists:reverse( Acc ), Point, NextTime } ;

compute_next_estimates( F, Point, Time, Timestep, Screen, PointCount, Acc ) ->

	NewPoint = rk4_solver:compute_next_estimate3p( F, Point, Time, Timestep ),

	NewProjectedPoint = project_2D( NewPoint, Screen ),

	compute_next_estimates( F, NewPoint, Time+Timestep, Timestep, Screen,
							PointCount-1, [ NewProjectedPoint | Acc ] ).




% @doc Function f(t,v) corresponding to the equations of the Lorenz system.
%
% See http://en.wikipedia.org/wiki/Lorenz_system
%
-spec lorenz_function( time(), point3() ) -> point3().
lorenz_function( _Time, _P={ X0, Y0, Z0 } ) ->

	% These specific equations here happen not to depend on time.

	Sigma = 10.0,
	Rho   = 28.0,
	Beta  = 8.0 / 3.0,

	X1 = Sigma * ( Y0 - X0 ),
	Y1 = X0 * ( Rho - Z0 ) - Y0,
	Z1 = X0 * Y0 - Beta * Z0,

	{ X1, Y1, Z1 }.




% GUI section.


% State of the program, passed between event handlers.
-record( gui_state, { main_frame,
					  start_button,
					  increase_step_button,
					  decrease_step_button,
					  stop_button,
					  clear_button,
					  reset_button,
					  quit_button,
					  status_bar,
					  canvas,
					  screen :: screen(),
					  solver_table :: solver_table(),

					  % The current applicable timestep:
					  timestep :: time_step() } ).



% The left part of the windows gathers the buttons, while the right one shows
% the canvas.


-spec get_main_window_width() -> coordinate().
get_main_window_width() ->
	1920.


-spec get_main_window_height() -> coordinate().
get_main_window_height() ->
	1080.


%-spec get_canvas_width() -> coordinate().
%get_canvas_width() ->
%   640.


%-spec get_canvas_height() -> coordinate().
%get_canvas_height() ->
%   480.



% @doc Lists all the declared names of widget identifiers.
get_all_id_names() ->
	[ 'MainFrame', 'StartButton', 'IncButton', 'DecButton',
	  'StopButton', 'ClearButton', 'ResetButton', 'QuitButton' ].



% @doc Returns the numerical ID corresponding to the specified name.
%
% (a good target for a parse transform, to do it at build-time)
%
-spec get_id( atom() ) -> gui:id().
get_id( Name ) ->
	list_utils:get_index_of( Name, get_all_id_names() ).



% @doc Returns the name (as an atom) of the specified widget (expected to be
% named).
%
% Defines could have been appropriate as well.
%
-spec get_name( gui:id() ) -> widget_name().
get_name( Id ) ->

	Names = get_all_id_names(),

	Len = length( Names ),

	case Id of

		Id when Id < 1 orelse Id > Len ->
			throw( { unregisted_widget_id, Id } );

		_ ->
			lists:nth( Id, Names )

	end.



% @doc Initialises the GUI and associated parts (solver).
-spec start() -> no_return().
start() ->

	gui:start(),

	%observer:start(),

	FrameSize = { get_main_window_width(), get_main_window_height() },

	MainFrame = gui:create_frame( _Title="Lorenz Test", _FramePos=auto,
		FrameSize, _FrameStyle=default, _Id=get_id( 'MainFrame' ),
		_MaybeParent=undefined ),

	gui:subscribe_to_events( { onWindowClosed, MainFrame } ),

	StatusBar = gui:create_status_bar( MainFrame ),

	% Not wanting to overwhelm the test GUI main loop:
	SolverCount = max( 1, system_utils:get_core_count() div 2 ),
	%SolverCount = 2,
	%SolverCount = 0,

	InitialTimestep = 0.005,

	gui:push_status_text( "Initialisation of ~B solvers (not started yet), "
		"with a timestep of ~f.", [ SolverCount, InitialTimestep ], StatusBar ),

	LeftPanel = gui:create_panel( MainFrame ),

	RightPanel = gui:create_panel( MainFrame ),

	%gui:set_background_color( MainFrame, red ),
	%gui:set_background_color( LeftPanel, blue ),
	%gui:set_background_color( RightPanel, green ),

	MainSizer = gui:create_sizer( horizontal ),

	% Constant width:
	gui:add_to_sizer( MainSizer, LeftPanel,
					  [ { proportion, 0 }, { flag, [ expand_fully ] } ] ),

	% Grows with the window:
	gui:add_to_sizer( MainSizer, RightPanel,
					  [ { proportion, 2 }, { flag, [ expand_fully ] } ] ),

	ControlBoxSizer = gui:create_sizer_with_labelled_box( vertical, LeftPanel,
														  "Controls" ),

	% Adding the buttons to the control panel:

	% Common settings:

	Position = auto,
	ButtonSize = auto,
	ButtonStyle = default,
	ParentButton = LeftPanel,

	StartButton = gui:create_button( "Start resolution", Position, ButtonSize,
		ButtonStyle, get_id( 'StartButton' ), ParentButton ),

	IncButton = gui:create_button( "Increase timestep", Position, ButtonSize,
		ButtonStyle, get_id( 'IncButton' ), ParentButton ),

	DecButton = gui:create_button( "Decrease timestep", Position, ButtonSize,
		ButtonStyle, get_id( 'DecButton' ), ParentButton ),

	StopButton = gui:create_button( "Stop resolution", Position, ButtonSize,
		ButtonStyle, get_id( 'StopButton' ), ParentButton ),

	ClearButton = gui:create_button( "Clear phase space", Position, ButtonSize,
		ButtonStyle, get_id( 'ClearButton' ), ParentButton ),

	ResetButton = gui:create_button( "Reset initial conditions", Position,
		ButtonSize, ButtonStyle, get_id( 'ResetButton' ), ParentButton ),

	QuitButton = gui:create_button( "Quit", Position, ButtonSize, ButtonStyle,
									get_id( 'QuitButton' ), ParentButton ),

	Buttons = [ StartButton, IncButton, DecButton, StopButton, ClearButton,
				ResetButton, QuitButton ],

	gui:subscribe_to_events( [ { onButtonClicked, B } || B <- Buttons ] ),


	gui:set_tooltip( LeftPanel, "Controls for the Lorenz test" ),

	ButtonOpt = [ { flag, [ expand_fully ] } ],

	[ gui:add_to_sizer( ControlBoxSizer, B, ButtonOpt ) || B <- Buttons ],

	gui:set_sizer( LeftPanel, ControlBoxSizer ),

	PolyBoxSizer = gui:create_sizer_with_labelled_box( vertical, RightPanel,
		"Phase Space with ~B parallel RK4 solvers", [ SolverCount ] ),

	Canvas = gui:create_canvas( RightPanel ),

	gui:set_background_color( Canvas, red ),

	gui:clear( Canvas ),

	gui:subscribe_to_events( { [ onRepaintNeeded, onResized ], Canvas } ),

	gui:add_to_sizer( PolyBoxSizer, Canvas,
					  [ { proportion, 1 }, { flag, [ expand_fully ] } ] ),

	gui:set_tooltip( Canvas, "Lorenz Attractor." ),

	gui:set_sizer( RightPanel, PolyBoxSizer ),

	gui:set_sizer( MainFrame, MainSizer ),

	% Sets the GUI to visible:
	gui:show( MainFrame ),

	ZoomFactor = 24.0,

	Screen = #screen{ center={ get_main_window_width() / 3 - 550,
							   get_main_window_height() / 2 },
					  zoom_x=ZoomFactor,
					  zoom_y=ZoomFactor },

	Colors = gui_color:get_random_colors( SolverCount ),

	% The function corresponding to the equation system to solve:
	Derivative = fun lorenz_function/2,

	% Initial conditions:
	InitialPoint = get_initial_base_point(),

	InitialTime = 0.0,

	SolverTable = create_solver_table( Derivative, Colors, InitialPoint,
									   InitialTime, InitialTimestep, Screen ),

	InitialState = #gui_state{ main_frame=MainFrame,
							   start_button=StartButton,
							   increase_step_button=IncButton,
							   decrease_step_button=DecButton,
							   stop_button=StopButton,
							   clear_button=ClearButton,
							   reset_button=ResetButton,
							   quit_button=QuitButton,
							   status_bar=StatusBar,
							   canvas=Canvas,
							   screen=Screen,
							   solver_table=SolverTable,
							   timestep=InitialTimestep },

	% Wanting to catch up with the solvers:
	erlang:process_flag( priority, _Level=high ),

	gui_main_loop( InitialState ).



% @doc Returns the initial base point (initial condition) for solvers.
-spec get_initial_base_point() -> point3().
get_initial_base_point() ->
	{ 0.1, 0.0, 0.0 }.


% This table helps the rendering process keeping track of the solvers that feed
% it with new points to plot.
%
create_solver_table( Derivative, Colors, InitialPoint, InitialTime,
					 InitialTimestep, Screen ) ->
	create_solver_table( Derivative, Colors, InitialPoint, InitialTime,
						 InitialTimestep, Screen, _Acc=[] ).


% (helper)
create_solver_table( _Derivative, _Colors=[], _InitialPoint, _InitialTime,
					 _InitialTimestep, _Screen, Acc ) ->
	table:new( Acc );

create_solver_table( Derivative, _Colors=[ C | T ],
					 _PreviousInitialPoint={ X, Y, Z }, InitialTime,
					 InitialTimestep, Screen, Acc ) ->

	NewInitialPoint = { X+5.0, Y+5.0, Z+5.0 },

	% For the closure:
	TestPid = self(),

	NewSolver = spawn_link( fun() -> solver_main_loop( Derivative,
		NewInitialPoint, InitialTime, InitialTimestep, Screen, TestPid ) end ),

	GUIInitialPoint = project_2D( NewInitialPoint, Screen ),

	NewAcc = [ { NewSolver, { C, GUIInitialPoint } } | Acc ],

	create_solver_table( Derivative, T, NewInitialPoint, InitialTime,
						 InitialTimestep, Screen, NewAcc ).


% @doc Resets the solvers based on the specified base point.
-spec reset_solvers( solver_table(), point3() ) -> void().
reset_solvers( SolverTable, InitialP ) ->
	TransVec = [ -4.0, 11.0, 7.0 ],
	reset_solvers( table:keys( SolverTable ), InitialP, TransVec ).


% (helper)
reset_solvers( _Solvers=[], _CurrentP, _TransVec ) ->
	ok;

reset_solvers( _Solvers=[ SolverPid | T ], CurrentP, TransVec ) ->
	NewCurrentP = point3:translate( CurrentP, TransVec ),
	SolverPid ! { set_current_point, NewCurrentP },
	reset_solvers( T, NewCurrentP, TransVec ).



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages.
%
gui_main_loop( State=#gui_state{ main_frame=MainFrame,
								 start_button=StartButton,
								 increase_step_button=IncButton,
								 decrease_step_button=DecButton,
								 stop_button=StopButton,
								 clear_button=ClearButton,
								 reset_button=ResetButton,
								 quit_button=QuitButton,
								 canvas=Canvas,
								 screen=Screen } ) ->

	%test_facilities:display( "Entering main loop." ),

	% 'undefined' if having to quit:
	MaybeNewState = receive

		% Routine messages sent by solvers shall be listed last, otherwise they
		% will eclipse other messages (ex: GUI ones):

		{ onWindowClosed, [ MainFrame, Context ] } ->
			trace_utils:notice_fmt( "Test main frame ~ts has been closed "
				"(~ts), quitting Lorenz test, test success.",
				[ gui:object_to_string( MainFrame ),
				  gui:context_to_string( Context ) ] ),
			undefined;


		{ onButtonClicked, [ StartButton, _Context ] } ->
			%test_facilities:display( "Start button clicked." ),

			SolverTable = State#gui_state.solver_table,

			gui:push_status_text( "Starting ~B solvers (timestep of ~f).",
				[ table:size( SolverTable ), State#gui_state.timestep ],
				State#gui_state.status_bar ),

			send_to_solvers( _Msg=start, SolverTable ),

			State;


		{ onButtonClicked, [ IncButton, _Context ] } ->
			%test_facilities:display( "Increase timestep button clicked." ),

			NewTimestep = 1.05 * State#gui_state.timestep,

			send_to_solvers( _Msg={ set_time_step, NewTimestep },
							 State#gui_state.solver_table ),

			gui:push_status_text( "Timestep increased to ~f.",
								  [ NewTimestep ], State#gui_state.status_bar ),

			State#gui_state{ timestep=NewTimestep };


		{ onButtonClicked, [ DecButton, _Context ] } ->
			%test_facilities:display( "Decrease timestep button clicked." ),

			NewTimestep = 0.95 * State#gui_state.timestep,

			send_to_solvers( _Msg={ set_time_step, NewTimestep },
							 State#gui_state.solver_table ),

			gui:push_status_text( "Timestep decreased to ~f.",
								  [ NewTimestep ], State#gui_state.status_bar ),

			State#gui_state{ timestep=NewTimestep };


		{ onButtonClicked, [ ClearButton, _Context ] } ->
			%test_facilities:display( "Clear button clicked." ),

			gui:push_status_text( "Phase space cleared.",
								  State#gui_state.status_bar ),

			gui:clear( Canvas ),
			gui:blit( Canvas ),
			State;


		{ onButtonClicked, [ StopButton, _Context ] } ->
			%test_facilities:display( "Stop button clicked." ),

			SolverTable = State#gui_state.solver_table,

			gui:push_status_text( "Stopping ~B solvers (timestep was ~f).",
				[ table:size( SolverTable ), State#gui_state.timestep ],
				State#gui_state.status_bar ),

			send_to_solvers( _Msg=stop, SolverTable ),

			State;

		{ onButtonClicked, [ ResetButton, _Context ] } ->
			%test_facilities:display( "Reset button clicked." ),

			reset_solvers( State#gui_state.solver_table,
						   get_initial_base_point() ),

			gui:push_status_text( "Initial conditions of solvers reset.",
								  State#gui_state.status_bar ),
			State;

		{ onButtonClicked, [ QuitButton, _Context ] } ->
			%test_facilities:display( "Quit button clicked." ),
			undefined;


		{ onButtonClicked, [ AnyOtherButton, _Context ] } ->
			test_facilities:display( "Following unexpected button clicked: ~w.",
									 [ AnyOtherButton ] ),
			State;


		{ onRepaintNeeded, [ Canvas, _Context ] } ->

			%trace_utils:notice_fmt( "Test canvas '~ts' needing repaint (~ts).",
			%   [ gui:object_to_string( Canvas ),
			%     gui:context_to_string( Context ) ] ),

			gui:blit( Canvas ),
			State;


		{ onResized, [ Canvas, _NewSize, _Context ] } ->

			%trace_utils:notice_fmt( "Test canvas '~ts' resized to ~p (~ts).",
			%   [ gui:object_to_string( Canvas ), NewSize,
			%     gui:context_to_string( Context ) ] ),

			gui:clear( Canvas ),
			State;


		{ draw_points, NewPoints, SendingSolverPid } ->

			%trace_utils:debug_fmt( "Drawing ~B points from ~w.",
			%   [ length( NewPoints ), SendingSolverPid ] ),

			SolverTable = State#gui_state.solver_table,

			{ Color, LastPoint } =
						table:get_value( SendingSolverPid, SolverTable ),

			NewLastPoint =
						draw_lines( Canvas, [ LastPoint | NewPoints ], Color ),

			gui:blit( Canvas ),

			SolverTable = State#gui_state.solver_table,

			NewSolverTable = table:add_entry( _K=SendingSolverPid,
								_V={ Color, NewLastPoint }, SolverTable ),

			State#gui_state{ solver_table=NewSolverTable };


		{ draw_point, NewPoint, SendingSolverPid } ->

			trace_utils:debug_fmt( " - drawing ~p (from ~p)~n",
								   [ NewPoint, SendingSolverPid ] ),

			SolverTable = State#gui_state.solver_table,

			{ Color, LastPoint } =
						table:get_value( SendingSolverPid, SolverTable ),

			SourceDrawPoint = project_2D( LastPoint, Screen ),

			DestinationDrawPoint = project_2D( NewPoint, Screen ),

			gui:draw_line( Canvas, SourceDrawPoint, DestinationDrawPoint,
						   Color ),

			gui:blit( Canvas ),

			NewSolverTable = table:add_entry( _K=SendingSolverPid,
									_V={ Color, NewPoint }, SolverTable ),

			State#gui_state{ solver_table=NewSolverTable };


		Any ->
			trace_utils:warning_fmt( "The main loop of the Lorenz test "
				"ignored following message:~n ~p.~n", [ Any ] ),
			State

	end,

	case MaybeNewState of

		undefined ->
			send_to_solvers( _StopMsg=stop, State#gui_state.solver_table ),

			% Simply stop recursing:
			gui:destruct_window( MainFrame ),

			gui:stop();

		NewState ->
			gui_main_loop( NewState )

	end.



% @doc Sends specified message to all solvers.
-spec send_to_solvers( message(), solver_table() ) -> void().
send_to_solvers( Msg, SolverTable ) ->
	Solvers = table:keys( SolverTable ),
	[ SolverPid ! Msg || SolverPid <- Solvers ].



% @doc Projects the specified 3D point onto 2D screen system.
-spec project_2D( point3(), screen() ) -> integer_point2().
project_2D( _Point={ X, Y, Z }, #screen{ center={ Xc, Yc },
										 zoom_x=ZoomX,
										 zoom_y=ZoomY } ) ->

	F = 1 / math:sqrt( 2 ),

	{ round( Xc + ZoomX * ( Z - F*X ) ), round( Yc + ZoomY * ( Y - F*X ) ) }.



% @doc Draws lines between all the specified (already projected) points, and
% returns the last of these points.
%
draw_lines( _Canvas, _Points=[ LastPoint ], _Color ) ->
	LastPoint;

draw_lines( Canvas, _Points=[ P1, P2 | T ], Color ) ->

	gui:draw_line( Canvas, P1, P2, Color ),

	draw_lines( Canvas, [ P2 | T ], Color ).



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the GUI test, being in batch mode)" );

		false ->
			start()

	end,
	test_facilities:stop().
