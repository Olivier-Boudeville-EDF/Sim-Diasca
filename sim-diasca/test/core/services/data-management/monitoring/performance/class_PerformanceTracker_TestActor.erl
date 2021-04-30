% Copyright (C) 2008-2021 EDF R&D

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

% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


-module(class_PerformanceTracker_TestActor).


-define( class_description,
		 "Defines a simple periodic actor for testing performance tracker "
		 "features (how to trace memory consumption and process count over "
		 "time" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


-type activity_state() :: 'idle' | 'active'.

-type period_count() :: basic_utils:count().

-export_type([ activity_state/0, period_count/0 ]).


-define( class_attributes, [

		{ actor_state, activity_state(),
		  "stores the activity level of this actor" },

		{ periodic, period_count(),
		  "number of periods to wait between actor creations" },

		{ created_actor_count, basic_utils:count(),
		  "number of actors already created" },

		{ termination_tick_offset, class_TimeManager:tick_offset(),
		  "the tick offset at which this test actor will terminate" } ] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.Test" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Constructs a new test actor for the performance tracker:
%
% - ActorSettings is the AAI assigned to this actor by the load
% balancer
%
% - ActorName the name of the actor
%
% - TerminationTickOffset the duration after which this actor should terminate
%
% The Performance Tracker test actor spontaneous behaviours is the following:
% after each elapsed period (according to the predefined 'periodic' attribute),
% every existing test actor creates a new performance tracker actor.
%
% The memory_load_loop/1 function can be activated for an increasing memory
% consumption.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_TimeManager:tick_offset() ) ->
					   wooper:state().
construct( State, ActorSettings, ActorName, TerminationTickOffset ) ->

	% First the direct mother classes, then these class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize( ActorName ) ),

	?send_info( ActorState, "Creating a new performance tracker test actor" ),

	setAttributes( ActorState, [

		{ actor_state, active },

		% Increase the period if wanting to slow down actor creations:
		{ periodic, 8 },

		{ created_actor_count, 0 },
		{ termination_tick_offset, TerminationTickOffset } ] ).



% Methods section.


% The core of the performance tracker test actor behaviour.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	TerminationOffset = ?getAttr(termination_tick_offset),
	CurrentTick = ?getAttr(current_tick_offset),
	CreationTickOffset = ?getAttr(actor_creation_tick_offset),

	UpdatedState = case CurrentTick of

		PastOffset when PastOffset >= CreationTickOffset + TerminationOffset ->

			case ?getAttr(actor_state) of

				active ->
					ActiveState = setAttribute( State, actor_state, idle ),

					executeOneway( ActiveState, scheduleNextSpontaneousTick );
					%AState = setAttribute( State, actor_state, idle ),
					%executeOneway( AState, declareTermination );

				idle ->
					executeOneway( State, declareTermination )

			end;

		_CurrentOffset ->

			UpdatedCreationCounter = ?getAttr(created_actor_count) + 1,

			% Note: as actors will create other actors, many actors are bound to
			% have the same name.
			%
			CreatedActorName = text_utils:format(
						"My Performance Tracker test actor #~B",
						[ UpdatedCreationCounter ] ),

			NewState = class_Actor:create_actor(
					_CreatedClassname=class_PerformanceTracker_TestActor,
					[ CreatedActorName, ?getAttr(termination_tick_offset) ],
					State ),

			CreatedState = setAttribute( NewState, created_actor_count,
										 UpdatedCreationCounter ),

			executeOneway( CreatedState, addSpontaneousTick,
						   CurrentTick + ?getAttr(periodic) )

	end,

	wooper:return_state( UpdatedState ).




% Simply schedules this just created actor at the next tick (diasca 0).
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) -> 
						   actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( ScheduledState ).



% Called once a test actor has been created.
-spec onActorCreated( wooper:state(), created_actor_pid(), class_Actor:tag(),
					  load_balancer_pid() ) -> const_actor_oneway_return().
onActorCreated( State, CreatedActorPid, _CreatedActorTag, _LoadBalancerPid ) ->

	?debug_fmt( "Test actor ~p created.", [ CreatedActorPid ] ),

	actor:const_return().
