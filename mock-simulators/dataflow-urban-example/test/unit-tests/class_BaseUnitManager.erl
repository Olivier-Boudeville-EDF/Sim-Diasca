% Copyright (C) 2016-2022 EDF R&D

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


% @doc Unit manager defined for <b>channel testing</b>.
-module(class_BaseUnitManager).


-define( class_description,
		 "This test unit manager creates a channel between a test object and "
		 "a test unit in the course of the simulation." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowUnitManager ] ).



% Helpers:
-export([ to_string/1 ]).



-include("dataflow_unit_test_defines.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Unit-testing.BaseTestUnitManager" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Constructs a test unit manager, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as automatically assigned by the load balancer
%
% - ExperimentManagerPid, the PID of the (parent) experiment manager
%
% - LoadBalancerPid, the PID of the load balancer that may be used by this unit
% manager
%
% - IdentificationServerPid, the PID of the identification server (if any)
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 experiment_manager_pid(), binding_managers(),
				 load_balancer_pid(),
				 maybe( identification_server_pid() ) ) -> wooper:state().
construct( State, ActorSettings, ExperimentManagerPid, BindingManagers,
		   LoadBalancerPid, IdentificationServerPid ) ->

	ManagedUnitSpec = [ class_BaseTestProcessingUnit ],

	ListenedEventMatches = get_listened_event_matches(),

	EmitterName = ?MODULE,

	BaseState = class_DataflowUnitManager:construct( State, ActorSettings,
		?trace_categorize(EmitterName), ManagedUnitSpec,
		ListenedEventMatches, ExperimentManagerPid, BindingManagers,
		LoadBalancerPid, IdentificationServerPid ),

	setAttributes( BaseState, [ { test_objects, [] },
								{ test_units, [] } ] ).




% Methods section.


% @doc Returns the synchronization event matches that this unit manager is
% interested in.
%
-spec get_listened_event_matches() -> static_return( [ event_match() ] ).
get_listened_event_matches() ->

	% We do not listen to this kind of events, as it does not happen anyway in
	% these tests, as these dataflow objects are created initially (hence
	% without world events that could be intercepted):

	%TestObjectCreationMatch = #creation_event_match{
	%   object_type_match=class_BaseTestDataflowObject },

	TestConnectionMatch = #connection_event_match{
		source_block_type_match=class_BaseTestDataflowObject,
		target_block_type_match=class_BaseTestProcessingUnit },

	wooper:return_static( [ TestConnectionMatch ] ).



% @doc Called so that this unit manager can perform domain-specific actions of
% its choice whenever a matching connection happened.
%
% Note: catch-all placeholder implementation, meant to be overridden.
%
-spec onConnectionEventMatched( wooper:state(), connection_event() ) ->
									  oneway_return().
onConnectionEventMatched( State, ConnectionEvent=#connection_event{
							id=EventId,
							source_block_type=class_BaseTestDataflowObject,
							target_block_type=class_BaseTestProcessingUnit,
							source_block_pid=SourceBlockPid,
							target_block_pid=TargetBlockPid,
							output_port_name=OutputPortName,
							input_port_name=InputPortName } ) ->

	?debug_fmt( "Connection event notified: ~ts.",
				[ dataflow_support:world_event_to_string( ConnectionEvent ) ] ),

	ChannelEndpoints = [ { OutputPortName, InputPortName } ],

	ChannelState = class_DataflowUnitManager:create_channels_for( EventId,
					SourceBlockPid, TargetBlockPid, ChannelEndpoints, State ),

	wooper:return_state( ChannelState ).



% @doc Registers specified test blocks.
%
% Typically called by the base entry point.
%
-spec registerTestBlocks( wooper:state(), test_dataflow_object_pid(),
				  test_processing_unit_pid(), sending_actor_pid() ) ->
								actor_oneway_return().
registerTestBlocks( State, TestDataflowObjectPid, TestProcessingUnitPid,
					_SendingActorPid ) ->

	NewTestObjects = [ TestDataflowObjectPid | ?getAttr(test_objects) ],

	NewTestUnits = [ TestProcessingUnitPid | ?getAttr(test_units) ],

	RegisterState = setAttributes( State, [ { test_objects, NewTestObjects },
											{ test_units, NewTestUnits } ] ),

	actor:return_state( RegisterState ).



% @doc To create a channel between Obj1:foo and PU1:my_input_port.
-spec createTestChannel( wooper:state(), sending_actor_pid() ) ->
								actor_oneway_return().
createTestChannel( State, _SendingActorPid ) ->

	SourceObjectPid = hd( ?getAttr(test_objects) ),
	SourceAttributeName = "foo",

	TargetUnitPid = hd( ?getAttr(test_units) ),
	TargetInputPortName = "my_input_port",

	?debug_fmt( "Creating a test channel between attribute '~ts' "
		"of source object ~w, and input port '~ts' of unit ~w.",
		[ SourceAttributeName, SourceObjectPid, TargetInputPortName,
		  TargetUnitPid ] ),

	EventId = 17,

	ConnectedState = class_DataflowUnitManager:create_channels_for( EventId,
		SourceObjectPid, TargetUnitPid,
		[ { SourceAttributeName, TargetInputPortName } ], State ),

	actor:return_state( ConnectedState ).



% @doc Returns a textual description of this unit manager
-spec to_string( wooper:state() ) -> ustring().
to_string( _State ) ->
	"Base test unit manager".
