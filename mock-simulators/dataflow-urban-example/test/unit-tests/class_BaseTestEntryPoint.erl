% Copyright (C) 2016-2024 EDF R&D

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


% @doc Base experiment entry point, defined for testing.
-module(class_BaseTestEntryPoint).


-define( class_description,
		 "This (programmatic) example of experiment entry point starts each "
		 "step of this test experiment."
		 "It does not rely on changesets to operate." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ExperimentEntryPoint ] ).


% Helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Unit-testing.BaseTestEntryPoint" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


-include("dataflow_unit_test_defines.hrl").



% The attributes that are specific to this base test entry point are:
-define( class_attributes, [

	{ current_step, step_count(),
	  "the current step at which the experiment is" },

	{ max_step, step_count(),
	  "the maximum step that the experiment may reach" },

	{ test_dataflow_objects, [ test_dataflow_object_pid() ],
	  "a list of the test dataflow objects known of this entry point" },

	{ test_processing_units, [ test_processing_unit_pid() ],
	  "a list of the test processing units known of this entry point" },

	{ unit_managers, [ unit_manager_pid() ],
	  "a list of the unit managers driven by this entry point" } ] ).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type step_count() :: class_ExperimentManager:step_count().



% @doc Constructs the base experiment entry point for testing, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Dataflows is a list of the dataflows that this entry point should drive
%
% - ExperimentStepStart is the step at which the experiment shall start
%
% - ExperimentStepStop is the step at which the experiment shall stop
%
% - ExperimentManagerPid is the PID of the experiment manager
%
% - WorldManagerPid is the PID of the world manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
	[ dataflow_pid() ], step_count(), step_count(),
	experiment_manager_pid(), world_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, Dataflows, ExperimentStepStart,
		   ExperimentStepStop, ExperimentManagerPid, WorldManagerPid ) ->

	% First the direct mother class:
	EntryState = class_ExperimentEntryPoint:construct( State, ActorSettings,
						Dataflows, ExperimentManagerPid, WorldManagerPid ),

	% Then the class-specific actions:
	setAttributes( EntryState, [
		{ current_step, ExperimentStepStart },
		{ max_step, ExperimentStepStop },
		{ test_dataflow_objects, [] },
		{ test_processing_units, [] },
		{ unit_managers, [] } ] ).



% @doc Registers specified test dataflow objects to this entry point, so that it
% is able to act upon them (ex: attribute update).
%
-spec registerTestDataflowObjects( wooper:state(),
								   [ test_dataflow_object_pid() ] ) ->
				request_return( 'test_dataflow_objects_registered' ).
registerTestDataflowObjects( State, TestDataflowObjects ) ->

	?info_fmt( "Registering test dataflow objects ~p.",
				[ TestDataflowObjects ] ),

	NewState = concatToAttribute( State, test_dataflow_objects,
								  TestDataflowObjects ),

	wooper:return_state_result( NewState, test_dataflow_objects_registered ).



% @doc Registers specified test processing units to this entry point, so that it
% is able to act upon them.
%
-spec registerTestProcessingUnits( wooper:state(),
								   [ test_processing_unit_pid() ] ) ->
				request_return( 'test_dataflow_units_registered' ).
registerTestProcessingUnits( State, TestProcessingUnits ) ->

	?info_fmt( "Registering test processing units ~p.",
				[ TestProcessingUnits ] ),

	NewState = concatToAttribute( State, test_processing_units,
								  TestProcessingUnits ),

	wooper:return_state_result( NewState, test_dataflow_units_registered ).




% Section for actor oneways.


% @doc Starts the evaluation of the urban experiment for the current tick.
%
% Typically called by the experiment exit point, for synchronisation reasons.
%
-spec startExperimentTick( wooper:state(), sending_actor_pid() ) ->
									actor_oneway_return().
startExperimentTick( State, _SenderActorPid ) ->

	CurrentStep = ?getAttr(current_step),

	MaxStep = ?getAttr(max_step),

	?debug_fmt( "Starting experiment step ~B/~B.", [ CurrentStep, MaxStep ] ),

	% A single one expected here anyway:
	TargetDataflowPid = hd( ?getAttr(dataflows) ),

	Obj1Pid = hd( ?getAttr(test_dataflow_objects) ),

	UpdateState = case CurrentStep of

		5 ->

			% Settings the 'foo' attribute of Obj1 to 42:
			TestUpdateEvent = #update_event{
				object_type=class_BaseTestDataflowObject,
				object_pid=Obj1Pid,
				updates=[ { "foo", 42 } ],
				dataflow_pid=TargetDataflowPid },

			Changeset = [ TestUpdateEvent ],

			WorldManagerPid = ?getAttr(world_manager_pid),

			SentState = class_Actor:send_actor_message( WorldManagerPid,
							{ injectChangeset, [ Changeset ] }, State ),

			class_Actor:send_actor_message( WorldManagerPid,
							notifyAllChangesetsInjected, SentState );


		10 ->

			% Requesting a channel to be created, between Obj1:foo and
			% PU1:my_input_port, thanks to a connection_event:

			PU1Pid = hd( ?getAttr(test_processing_units) ),

			TestConnectionEvent = #connection_event{
				source_block_type=class_BaseTestDataflowObject,
				target_block_type=class_BaseTestProcessingUnit,
				source_block_pid=Obj1Pid,
				target_block_pid=PU1Pid,
				output_port_name="foo",
				input_port_name="my_input_port",
				dataflow_pid=TargetDataflowPid },

			Changeset = [ TestConnectionEvent ],

			WorldManagerPid = ?getAttr(world_manager_pid),

			SentState = class_Actor:send_actor_message( WorldManagerPid,
							{ injectChangeset, [ Changeset ] }, State ),

			class_Actor:send_actor_message( WorldManagerPid,
							notifyAllChangesetsInjected, SentState );

		_ ->
			State

	end,

	FinalState = setAttribute( UpdateState, current_step, CurrentStep+1 ),

	actor:return_state( FinalState ).




% Section for plain methods (ex: not actor oneways).


% @doc Declares the specified unit managers to this entry point.
%
% (request, for synchronicity)
%
-spec addUnitManagers( wooper:state(), [ unit_manager_pid() ] ) ->
								request_return( 'unit_managers_registered' ).
addUnitManagers( State, UnitManagers ) ->

	?info_fmt( "Adding unit managers ~w.", [ UnitManagers ] ),

	NewUnitManagers = ?getAttr(unit_managers) ++ UnitManagers,

	NewState = setAttribute( State, unit_managers, NewUnitManagers ),

	wooper:return_state_result( NewState, unit_managers_registered ).




% Helper functions.


% @doc Returns a textual description of this entry point.
%
% (helper)
%
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	EntryString = class_ExperimentEntryPoint:to_string( State ),

	ObjectString = case ?getAttr(test_dataflow_objects) of

		[] ->
			"not referencing any test dataflow object";

		Objects ->
			 text_utils:format( "referencing ~B test dataflow objects: ~p",
								[ length( Objects ), Objects ] )

	end,

	UnitString = case ?getAttr(test_processing_units) of

		[] ->
			"not referencing any test processing unit";

		Units ->
			 text_utils:format( "referencing ~B test processing units: ~p",
								[ length( Units ), Units ] )

	end,


	UnitManagerString = case ?getAttr(unit_managers) of

		[] ->
			text_utils:format( "not knowing any unit manager" );

		UnitManagers ->
			text_utils:format( "knowing following ~B unit manager(s): ~w",
							   [ length( UnitManagers ), UnitManagers ] )

	end,

	text_utils:format( "Test programmatic ~ts, ~ts, ~ts, ~ts",
		[ EntryString, ObjectString, UnitString, UnitManagerString ] ).
