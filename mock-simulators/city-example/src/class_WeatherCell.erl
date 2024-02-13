% Copyright (C) 2014-2024 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]


% @doc Class modelling a <b>weather cell</b>, part of a weather system.
-module(class_WeatherCell).


-define( class_description,
		 "Class modelling a weather cell, part of a weather system." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


-type cell_pid() :: actor_pid().
-type weather_system_pid() :: actor_pid().


% The class-specific attributes of an instance of a weather cell are:
-define( class_attributes, [

	{ weather_state, vector3(), "the state vector of this cell "
	  "(storing local temperature, pressure and hydrometry, supposedly uniform "
	  "in this cell)" },

	{ left_cell, cell_pid(), "the cell on the left of this cell" },

	{ right_cell, cell_pid(), "the cell on the right of this cell" },

	{ top_cell, cell_pid(), "the cell at the top of this cell" },

	{ bottom_cell, cell_pid(), "the cell at the bottom of this cell" },

	{ weather_system_pid, weather_system_pid(),
	  "the PID of the overall weather system" },

	{ period, unit_utils:seconds(),
	  "the number of seconds between two weather updates of this cell" },

	{ period_in_ticks, class_TimeManager:tick_offset(),
	  "the number of ticks between two successive evaluations of this cell" },

	{ spreading_factor, count(),
	  "determines on how many ticks the cells are spread, scheduling-wise" },

	{ solver_iteration_count, count(),
	  "the number of iterations performed to compute an updated weather" },

	{ solver_time_step, unit_utils:milliseconds(), "dictates the length of an "
	  "elementary time-step of the solver (beware to numerical stability if it "
	  "is too high)" } ] ).


% Exported helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.Weather.Cells" ).


% The (uniform) weather in a cell is made of temperature, pressure and
% hydrometry:
%
-type weather_vector() :: vector3().


-export_type([ weather_vector/0 ]).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% For waste_tank() and al:
-include("city_example_types.hrl").


% Number of times that the Lorenz equation is iterated per scheduled time-step:
-define( iteration_count, 50 ).



% Implementation notes:
%
% The state vector of a cell corresponds to: ( temperature, pressure, hydrometry
% ).
%
% All cells share the same equations, but they start each with different initial
% conditions (state vector).


% Shorthands:

%-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type point3() :: point3:point3().
-type vector3() :: vector3:vector3().




% @doc Creates a weather cell.
%
% Construction parameters are:
%
% - ActorSettings is the AAI assigned by the load-balancer to this actor
%
% - Name is the name of this weather cell (as a plain string)
%
% - InitialConditions is the initial (3D) state vector of this cell
%
% - Neighbours corresponds to the four adjacent cells
%
% - WeatherSystemPid is the PID of the overall weather system
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), weather_vector(),
				 { cell_pid(), cell_pid(), cell_pid(), cell_pid() },
				 weather_system_pid() ) -> wooper:state().
construct( State, ActorSettings, Name, InitialConditions,
		   _Neighbours={ LeftCell, RightCell, TopCell, BottomCell },
		   WeatherSystemPid ) ->

	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(Name) ),

	% 4 hours of simulated time between two computational updates:
	Period = 4 * 3600,

	IterationCount = 50,

	% Not used anymore, as the timestep must be fine enough, otherwise
	% computations will diverge and crash:
	%
	%Timestep = Period / IterationCount,
	Timestep = 0.005,

	% Defines on how many ticks the population of cells is spread:
	SpreadingFactor = 5,

	NewState = setAttributes( ActorState, [

		{ weather_state, InitialConditions },

		{ left_cell, LeftCell },
		{ right_cell, RightCell },
		{ top_cell, TopCell },
		{ bottom_cell, BottomCell },

		{ weather_system_pid, WeatherSystemPid },

		{ period, Period },
		{ period_in_ticks, SpreadingFactor },

		{ spreading_factor, SpreadingFactor },

		% The number of solver iterations for the Lorenz system to be
		% computed is also a way of setting the intensity in terms of
		% calculations:
		%
		{ solver_iteration_count, IterationCount },
		{ solver_time_step, Timestep } ] ),


	?send_info_fmt( NewState, "Initialised with conditions ~p.",
					[ InitialConditions ] ),

	NewState.




% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% If ever wanting to compare final states:
	%
	%trace_utils:debug_fmt( "cell stop ~p ~ts ~w",
	%  [ ?getAttr(actor_abstract_id), ?getAttr(name),
	%    ?getAttr(weather_state) ] ),

	% Class-specific actions:
	?debug( "Destructed." ),

	% Then allow chaining:
	State.





% Methods section.


% @doc First scheduling of a weather cell.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
										actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	SentState = class_Actor:send_actor_message( ?getAttr(weather_system_pid),
												register, State ),

	% To enter the never ending loop:
	%
	% (we do not use scheduleNextSpontaneousTick/1 here, as we want to spread
	% evenly the cells over the time step)
	%

	SubsetCount = 5,

	% Must be in a strict future, and subsets are defined to smooth the load
	% over ticks, otherwise non-weather models would be too much silenced:
	%
	InitialTick = class_Actor:get_current_tick_offset( State )
		+ ( ?getAttr(actor_abstract_id) rem SubsetCount ) + 1,

	PlanState = class_Actor:add_spontaneous_tick( InitialTick, SentState ),

	%PeriodInTicks = class_Actor:convert_seconds_to_ticks( ?getAttr(period),
	%                                                      PlanState ),

	?notice_fmt( "Ready, with ~p ticks per period, "
		"and ~B solver iteration count.",
		[ ?getAttr(period_in_ticks), ?getAttr(solver_iteration_count) ] ),

	%trace_utils:debug_fmt( "cell start ~p ~ts ~w",
	%    [ ?getAttr(actor_abstract_id), ?getAttr(name),
	%      ?getAttr(weather_state) ] ),

	actor:return_state( PlanState ).




% @doc The definition of the spontaneous behaviour of this weather cell.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	%?debug( "Acting." ),

	% We iterate on the equation multiple times (we "sample" the period):

	BaseTick = class_Actor:get_current_tick_offset( State ),

	PeriodInTicks = ?getAttr(period_in_ticks),

	PlanState =
		class_Actor:add_spontaneous_tick( BaseTick + PeriodInTicks, State ),

	IterationCount = ?getAttr(solver_iteration_count),

	TimeStep = ?getAttr(solver_time_step),

	CurrentWeatherState = ?getAttr(weather_state),

	% TO-DO: determine here if it is raining or snowing, and if yes notify the
	% relevant model instances (ex: roads) in this weather cell (the idea being
	% to couple them as much as possible).

	NewWeatherState = evaluate_weather( IterationCount, CurrentWeatherState,
										float( BaseTick ), TimeStep ),

	UpdatedState = setAttribute( PlanState, weather_state, NewWeatherState ),

	% Let's notify the neighbours:
	FinalState = lists:foldl(
		fun

			( _NeighbourPid=border, AccState ) ->
				AccState;

			( NeighbourPid, AccState ) ->
				class_Actor:send_actor_message( NeighbourPid,
					{ notifyWeather, [ NewWeatherState ] }, AccState )

		end,

		_Acc0=UpdatedState,

		_List=[ ?getAttr(left_cell), ?getAttr(right_cell),
				?getAttr(top_cell), ?getAttr(bottom_cell) ] ),

	wooper:return_state( FinalState ).



% @doc Called by a neighbouring cell so that this one knows its border
% conditions and can update its own weather accordingly.
%
-spec notifyWeather( wooper:state(), weather_vector(), cell_pid() ) ->
											actor_oneway_return().
notifyWeather( State, NeighbourWeather, _NeighbourCellPid ) ->

	%?debug_fmt( "Received weather ~p from cell ~p.",
	%            [ NeighbourWeather, NeighbourCellPid ] ),

	% Here the adjacent weathers have a moderate impact:
	ImpactFactor = 1.0 / 12,

	ScaledWeatherP = point3:scale( NeighbourWeather, ImpactFactor ),

	ScaledWeatherV = point3:to_vector( ScaledWeatherP ),

	NewWeather = point3:translate( ?getAttr(weather_state), ScaledWeatherV ),

	NewState = setAttribute( State, weather_state, NewWeather ),

	actor:return_state( NewState ).




% @doc Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->
	text_utils:format( "Weather cell '~ts' (AAI: ~B), "
		"whose random state is ~p ",
		[ ?getAttr(name), class_Actor:get_abstract_identifier( State ),
		  random_utils:get_random_state() ] ).




% Helper section.


% @doc Evaluates the next weather based on current one and the number of solver
% iterations requested.
%
% Returns the updated weather.
%
% (helper)
%
evaluate_weather( _IterationCount=0, CurrentWeatherState, _Time, _Timestep ) ->
	CurrentWeatherState;

evaluate_weather( IterationCount, CurrentWeatherState, Time, Timestep ) ->

	NewWeatherState = rk4_solver:compute_next_estimate3p( fun lorenz_function/2,
						CurrentWeatherState, Time, Timestep ),

	evaluate_weather( IterationCount-1, NewWeatherState, Time + Timestep,
					  Timestep ).




% @doc Function f(t,v) corresponding to the equations of the Lorenz system.
%
% See http://en.wikipedia.org/wiki/Lorenz_system
%
-spec lorenz_function( rk4_solver:time(), point3() ) -> point3().
lorenz_function( _Time, _Vector={ X0, Y0, Z0 } ) ->

	%trace_utils:debug_fmt( "Current weather state of ~p: { ~p, ~p, ~p }.",
	%                       [ self(), X0, Y0, Z0 ] ),

	% These equations do not depend directly on time.

	Sigma = 10.0,
	Rho   = 28.0,
	Beta  = 8.0 / 3.0,

	X1 = Sigma * ( Y0 - X0 ),
	Y1 = X0 * ( Rho - Z0 ) - Y0,
	Z1 = X0 * Y0 - Beta * Z0,

	{ X1, Y1, Z1 }.
