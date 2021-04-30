% Copyright (C) 2014-2021 EDF R&D

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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


-module(class_TestSpatialisedActor).

-define( class_description, "Test class for spatialised tests." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_SpatialisedActor ] ).



% Shorthands:

-type tick_offset() :: class_TimeManager:tick_offset().
-type entity_pid() :: class_TwoDimensionalEnvironment:entity_pid().
-type environment_pid() :: class_TwoDimensionalEnvironment: environment_pid().



% The class-specific attributes of a spatialised test actor:
-define( class_attributes, [

  { speed, unit_utils:meters_per_tick(),
	"the speed of this test actor (at least one upper-bound thereof)" },

  { perception_radius, linear:radius(), "the perception radius of this actor" },

  { perception_period, tick_offset(),
	"the period at which this actor will trigger a perception request" },

  { move_period, tick_offset(),
	"the period at which this actor will move (i.e. update its position)" },

  { termination_offset, union( tick_offset(), 'none' ),
	"the tick offset at which this actor will terminate" } ] ).




% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Spatial.TestSpatialisedActor" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_spatialised_actors.hrl").




% Creates a new test spatialised actor, in a 2D environment.
%
% Construction parameters are:
%
% - ActorSettings is the AAI assigned by the load-balancer to this actor
%
% - Name is the name of this actor
%
% - InitialPosition is the initial position of this actor in the specified
% environment
%
% - PerceptionRadius is the perception radius of this actor (in meters)
%
% - MaxSpeed is an upper-bound (if any) of the maximum speed of this actor
% (allows for better environment-level performances)
%
% - TerminationTickOffset is the tick offset at which this test actor is to
% terminate (or 'none')
%
% - EnvironmentPid is the PID of the environment this actor will live in
%

-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_SpatialisedActor:position(),
				 linear:radius(), tick_offset(),
				 class_TwoDimensionalEnvironment:max_speed(),
				 tick_offset(), environment_pid() ) -> wooper:state().
construct( State, ActorSettings, Name, InitialPosition, PerceptionRadius,
		   PerceptionPeriod, MaxSpeed, TerminationTickOffset,
		   EnvironmentPid ) ->

	SpatialState = class_SpatialisedActor:construct( State, ActorSettings,
						?trace_categorize( Name ),
						InitialPosition, MaxSpeed, EnvironmentPid ),

	setAttributes( SpatialState, [

			% We consider that the speed of this actor is constantly its maximum
			% one.
			%
			% Temporarily in meters per second:
			%
			{ speed, MaxSpeed },

			{ perception_radius, PerceptionRadius },
			{ perception_period, PerceptionPeriod },
			{ move_period, 5 },
			{ termination_offset, TerminationTickOffset } ] ).




% Section for actor oneways.



% First scheduling on this test actor.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   actor_oneway_return().
onFirstDiasca( State, SendingActorPid ) ->

	MetersPerSecond = ?getAttr(speed),

	TickDuration = ?getAttr(simulation_tick_duration),

	% In meters per tick:
	VirtualSpeed = case MetersPerSecond of

		undefined ->
			undefined;

		_ ->
			MetersPerSecond * TickDuration

	end,

	?notice_fmt( "Overall speed of ~p meters per second, "
			   "converted to ~p meters per tick "
			   "(duration of a tick: ~p virtual seconds).",
			   [ MetersPerSecond, VirtualSpeed, TickDuration ] ),

	% First, local actions; converting to meters per tick:
	LocalState = setAttribute( State, speed, VirtualSpeed ),

	% Then calling the parent one, to declare ourself to the environment:
	ParentState = executeOnewayAs( LocalState, class_SpatialisedActor,
								   onFirstDiasca, [ SendingActorPid ] ),

	PlanState = class_Actor:scheduleNextSpontaneousTick( ParentState ),

	actor:return_state( PlanState ).



% The definition of the spontaneous behaviour of this test actor.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	TerminationTickOffset = ?getAttr(termination_offset),

	ActState = case ( TerminationTickOffset =/= none )
				   andalso ( CurrentTickOffset >= TerminationTickOffset ) of

		true ->
			trace_utils:info_fmt(
			  "~w decided to terminate (at #~p, diasca O).",
			  [ self(), CurrentTickOffset ] ),

			UndeclaredState = class_Actor:send_actor_message(
					?getAttr(environment_pid), undeclareEntity, State ),

			executeOneway( UndeclaredState, declareTermination );

		false ->
			act_normally( CurrentTickOffset, State )

	end,

	wooper:return_state( ActState ).



% (helper)
act_normally( CurrentTickOffset, State ) ->

	% This actor moves from left to right (increasing abscissa):

	Position = ?getAttr(position),

	MovePeriod = ?getAttr(move_period),

	% Speed in meters per tick:
	NewPosition = case ?getAttr(speed) of

		% Static:
		undefined ->
			Position;

		S ->
			XOffset = S * MovePeriod,
			linear_2D:translate( Position, { XOffset, _YOffset=0 } )

	end,

	?debug_fmt( "Moving to ~p.", [ NewPosition ] ),


	PerceptionPeriod = ?getAttr(perception_period),

	% We decrement the current tick offset so that it is a multiple of 5:p
	RequestState = case ( CurrentTickOffset - 1 ) rem PerceptionPeriod of

		0 ->
			class_Actor:send_actor_message( ?getAttr(environment_pid),
					{ getTypedEntitiesWithin,
					  [ class_TestSpatialisedActor, Position,
						?getAttr(perception_radius) ] }, State );

		_ ->
			State

	end,

	NextActionOffset = CurrentTickOffset + MovePeriod,

	PlannedState = class_Actor:addSpontaneousTick( RequestState,
												   NextActionOffset ),

	setAttribute( PlannedState, position, NewPosition ).



% Called in response to the getEntitiesWithin request.
-spec notifyEntitiesNearby( wooper:state(), [ entity_pid() ],
							environment_pid() ) -> const_actor_oneway_return().
notifyEntitiesNearby( State, _NearbyEntities=[], _EnvironmentPid ) ->

	?debug( "No entity found in perception radius." ),

	actor:const_return();


notifyEntitiesNearby( State, NearbyEntities, _EnvironmentPid ) ->

	?notice_fmt( "~B entities found in perception radius: ~p.",
				 [ length( NearbyEntities ), NearbyEntities ] ),

	actor:const_return().
