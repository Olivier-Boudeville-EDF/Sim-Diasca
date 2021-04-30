% Copyright (C) 2016-2021 EDF R&D

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


-module(class_DataflowUnitManager).


-define( class_description,
		 "The dataflow unit manager is the abstract class from which each "
		 "actual manager for a given set of types of dataflow units shall "
		 "inherit. "
		 "Indeed, if relevant, a given unit manager may take care of multiple "
		 "types of units (ex: a urban manager may manage energy demand units, "
		 "pollution units, etc.). "
		 "For example, by design the Foo, Bar and Baz processing units may be "
		 "interlinked, in which case a FooBarBazUnitManager class can be "
		 "defined, inheriting from this DataflowUnitManager class and in "
		 "charge of the life cycle and connectivity of the instances of these "
		 "three kinds of units. "
		 "Each unit manager is registered to the (parent, top-level) "
		 "experiment manager. "
		 "Each unit manager is a singleton and registers itself globally under "
		 "its name, which is, conventionally, its actual classname (ex: "
		 "'class_EnergyDemandManager' or, if needing more clarity, "
		 "'class_EnergyDemandUnitManager'). "
		 "As a unit manager may create dataflow units at runtime (for the "
		 "types of units it is in charge of), it must itself be a (simulation) "
		 "actor." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).



% Design notes:
%
% Dataflow units may be created statically (whereas the simulation is not
% started, i.e. as initial units, typically from the simulation case) or
% dynamically (while the simulation is running, i.e. as runtime units, typically
% from dataflow unit managers).
%
% At runtime, a unit manager will receive from the experiment manager the world
% events matching the event clauses this unit manager declared.
%
% The processing of each of these events (designated by its identifier) will be
% done based on a series of actions (each also designated by identifiers of
% their own).


% Tne class-specific attributes of a unit manager are:
-define( class_attributes, [

	{ unit_table, unit_table(),
	  "a table associating to each supported type of units a list of the "
	  "corresponding instances created by this unit manager" },

	{ event_matches, [ event_match() ],
	  "a list describing the dataflow synchronization events that this unit "
	  "manager is interested in" },

	{ experiment_manager_pid, experiment_manager_pid(),
	  "the PID of the experiment manager" },

	{ binding_managers, binding_managers(),
	  "a record storing the PIDs of all the binding managers corresponding to "
	  "the activated language bindings; useful whenever an instance "
	  "implemented in one of these languages has to be created" },

	{ load_balancer_pid, load_balancer_pid(),
	  "PID of the load balancer, useful to create new units for example" },

	{ identification_server_pid, maybe( identification_server_pid() ),
	  "PID of the identification server, if enabled by the case" },

	{ event_table, event_table(),
	  "records, for each world event (designated by its identifier) to be "
	  "processed, the identifiers of the pending actions still currently in "
	  "progress, as triggered by this unit manager (typically unit or channel "
	  "waited creations or destructions); now that the processing of events "
	  "is serialised (to avoid the pitfalls of event interleaving, i.e. some "
	  "events may need that past ones are fully processed - typically a unit "
	  "being fully created before being updated- for their own processing), "
	  "this table is expected to be either empty or holding one event" },

	{ action_table, action_table(),
	  "allows to keep track of all pending actions, by associating to an "
	  "action identifier a full description to the corresponding action" },

	{ action_count, action_count(),
	  "count of all the actions already declared (and also the identifier of "
	  "the last allocated action)" } ] ).



% Exported helpers:
-export([ create_channels_for/5, create_output_ports/2,
		  event_clauses_to_string/1, event_clause_to_string/1,
		  connection_specs_to_string/1, connection_spec_to_string/1,
		  upstream_spec_to_string/1, downstream_spec_to_string/1 ]).


% Helpers:
-export([ create_runtime_unit/4, to_string/1 ]).


% Records all instances of each managed unit type:
-type unit_table() :: table( dataflow_unit_type(), [ unit_pid() ] ).



% Describes a type of processing unit that is to be managed by a given unit
% manager.
%
% It may be:
%
% - either directly the classname of that unit (ex:
% 'class_TransportationDemandUnit') - implying it is an Erlang-based unit
%
% - or a {Classname, ImplementationLanguage} pair, meaning said unit is defined
% in specified class, and implemented in specified programming language (ex:
% {'class_VehicleTypeUnit', 'python'}, or {'class_EnergyDemandUnit', 'erlang'}
%
-type managed_unit_spec() :: dataflow_unit_type() |
					{ dataflow_unit_type(), language_utils:language() }.



% Section about actions.


% The types of actions that a unit manager may track:
-type action_type() :: 'unit_creation'
					 | 'unit_destruction'
					 | 'unit_connection'
					 | 'unit_disconnection'.


% Any contextual information about an action:
-type context() :: any().


% Allows a unit manager to record a pending action in the context of the
% processing of a given world event:
%
-type action() :: unit_creation_action()
				| unit_destruction_action()
				| unit_connection_action()
				| unit_disconnection_action().


% Action corresponding to the creation of a unit.
-type unit_creation_action() :: { 'unit_creation', dataflow_unit_type(),
	   wooper:construction_parameters(), event_id(), unit_creation_context() }.


% Typically the PID of an upstream dataflow object (i.e dataflow_object_pid()):
-type unit_creation_context() :: context().




% Action corresponding to the destruction of a unit.
-type unit_destruction_action() ::
		{ 'unit_destruction', unit_pid(), event_id() }.



% Information about the (upstream, outgoing, "left") part of a connection:
%
% (if the kind of port is not specified, a standard port is assumed)
%
-type upstream_port_spec() ::
		output_port_string_name()
	  | { 'output_port_name', output_port_string_name() }
	  | { 'output_iteration_name', output_iteration_string_name() }.


% Canonical form of upstream_port_spec/0:
-type canonical_upstream_port_spec() ::
		{ 'output_port_name', output_port_name() }
	  | { 'output_iteration_name', output_iteration_name() }.



% Information about the (downstream, ingoing, "right") part of a connection:
%
% (if the kind of port is not specified, a standard port is assumed)
%
-type downstream_port_spec() ::
		input_port_string_name()
	  | { 'input_port_name', input_port_string_name() }
	  | { 'input_iteration_name', input_iteration_string_name() }.



% Canonical form of downstream_port_spec/0:
-type canonical_downstream_port_spec() ::
		{ 'input_port_name', input_port_name() }
	  | { 'input_iteration_name', input_iteration_name() }.



% Allows to specify, regarding an upstream block and a downstream one, that a
% connection between two ports that shall be made.
%
% Note: such connections are typically aggregated into lists.
%
% (if only a port name is specified, an identically named standard port in both
% ends is assumed)
%
-type connection_spec() :: { upstream_port_spec(), downstream_port_spec() }
						   | port_string_name().


% Canonical form of connection_spec/0:
-type canonical_connection_spec() :: { canonical_upstream_port_spec(),
									   canonical_downstream_port_spec() }.



% Action corresponding to the connection of a unit to the dataflow, i.e. the
% creation of a set of channels, from output ports to input ones, ports being
% standard or iterated ones.
%
-type unit_connection_action() :: { 'unit_connection', event_id(),
		upstream_block_pid(), downstream_block_pid(),
		[ canonical_connection_spec() ], unit_connection_context() }.

-type unit_connection_context() :: context(). % 'undefined'



% Action corresponding to the disconnection of a unit to the dataflow, i.e. the
% removal of a set of channels.
%
-type unit_disconnection_action() :: { 'unit_disconnection', event_id(),
	   block_pid(), unit_disconnection_context() }.

-type unit_disconnection_context() :: context(). % 'undefined'



% Allows to keep track of the actions performed by this unit manager:
-type action_id() :: basic_utils:count().


% Keeps track of the actions in progress:
-type action_table() :: table( action_id(), action() ).


% Allows to set action identifiers:
-type action_count() :: basic_utils:count().


% Records, for a given world event, all the pending actions still currently
% in progress regarding that unit manager:
%
-type event_table() :: table( event_id(), [ action_id() ] ).


% The classname of a unit manager:
-type actor_classname() :: class_Actor:classname().



-export_type([ unit_table/0, managed_unit_spec/0, action_type/0, action/0,
			   canonical_downstream_port_spec/0, canonical_connection_spec/0,
			   action_id/0, action_table/0, action_count/0,
			   event_table/0, actor_classname/0 ]).


% Shorthand:
-type connection_info() :: class_DataflowBlock:connection_info().


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.UnitManager" ).



% For dataflow-related types and names:
-include("dataflow_defines.hrl").


% For all bindings-related types:
-include("bindings.hrl").


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").


% Shorthands:
-type ustring() :: text_utils:ustring().


% Implementation notes:
%
% A unit manager is not linked to any dataflow (as there may exist multiple
% dataflow instances anyway).



% Constructs a unit manager, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Name, an atom designating the classname of this singleton manager (ex:
% 'class_FooBarBazUnitManager')
%
% - ManagedUnitSpecs specifies the types of units that this manager is to take
% care of, together with their implementation language (default being Erlang)
%
% - ListenedEventMatches is a list of the dataflow synchronization events that
% this unit manager is interested in
%
% - ExperimentManagerPid is the PID of the parent manager of this one
%
% - BindingManagers, a record storing the PIDs of all the binding managers
% corresponding to the activated language bindings; useful whenever an instance
% implemented in one of these languages has to be created
%
% - LoadBalancerPid, the PID of the load balancer, useful when instances have to
% be created
%
% - IdentificationServerPid, the PID of the identification server (if any)
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		class_Actor:name(), [ managed_unit_spec() ], [ event_match() ],
		experiment_manager_pid(), binding_managers(), load_balancer_pid(),
		maybe( identification_server_pid() ) ) ->  wooper:state().
construct( State, ActorSettings, Name, ManagedUnitSpecs, ListenedEventMatches,
		   ExperimentManagerPid, BindingManagers, LoadBalancerPid,
		   IdentificationServerPid ) ->

	binding_utils:check_implementation_language( ManagedUnitSpecs,
												 BindingManagers ),

	ManagedUnitTypes =
		dataflow_binding_utils:get_unit_types( ManagedUnitSpecs ),

	% Auto-subscribing, and declaring our own event matches (based on a
	% request):
	%
	ExperimentManagerPid ! { registerUnitManager,
						[ ManagedUnitTypes, ListenedEventMatches ], self() },

	% We expect child classes to pass atom-based names:
	{ RegistrationName, TraceInit } = case Name of

		{ AtomName, TraceCateg } ->
			StringName = text_utils:atom_to_string( AtomName ),
			{ AtomName, { StringName, TraceCateg } };

		% Emitter categorization added later:
		AtomName ->
			StringName = text_utils:atom_to_string( AtomName ),
			{ AtomName, StringName }

	end,

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(TraceInit) ),

	?send_info_fmt( ActorState, "Linked to experiment manager ~w and defining "
		"following ~ts", [ ExperimentManagerPid,
						  event_clauses_to_string( ListenedEventMatches ) ] ),

	% All unit managers register themselves that way:
	% (ensures uniqueness as well)
	%
	naming_utils:register_as( RegistrationName, global_only ),

	PreparedUnitTable = prepare_for_units( ManagedUnitSpecs, ActorState ),

	EmptyTable = table:new(),

	% Then the class-specific actions:
	FinalState = setAttributes( ActorState, [
		{ unit_table, PreparedUnitTable },
		{ event_matches, ListenedEventMatches },
		{ experiment_manager_pid, ExperimentManagerPid },
		{ binding_managers, BindingManagers },
		{ load_balancer_pid, LoadBalancerPid },
		{ identification_server_pid, IdentificationServerPid },
		{ event_table, EmptyTable },
		{ action_table, EmptyTable },

		% One may prefer starting counting the actions from an easily-spotted
		% offset (ex: to better discriminate actions from event identifiers):
		%
		%{ action_count, 100 } ] ),
		{ action_count, 0 } ] ),

	% Interleaving of registerUnitManager/2 is over:
	receive

		{ wooper_result, unit_manager_registered } ->
			ok

	end,

	FinalState.



% Prepares the management of the specified types of units.
-spec prepare_for_units( [ managed_unit_spec() ], wooper:state() ) ->
								unit_table().
prepare_for_units( UnitSpecs, State ) ->

	% Gets the list of unit (Erlang) classnames:
	UnitTypes = dataflow_binding_utils:get_unit_types( UnitSpecs ),

	case class_DataflowBlock:declare_static_information_for( UnitSpecs ) of

		ok ->
			?notice_fmt( "All semantics and types for units ~p successfully "
						 "declared statically.", [ UnitTypes ] );

		{ error, Reason } ->
			?error_fmt( "Static declaration of semantics and types failed for "
				"unit data ~p. Reason: ~p", [ UnitSpecs, Reason ] ),
			throw( { static_declaration_failed, UnitTypes, Reason } )

	end,

	% Initially all unit types know none of their instances:
	EmptyEntries = [ { Type, [] } || Type <- UnitTypes ],

	table:add_entries( EmptyEntries, table:new() ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?info_fmt( "Being deleted, while still ~ts",
			   [ unit_table_to_string( State ) ] ),

	State.




% Methods section.


% Callback executed on the first diasca of existence of this unit manager.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							const_actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?info_fmt( "Created a ~ts", [ to_string( State ) ] ),

	actor:const_return().




% Event processing section.


% Called (most probably by the experiment manager) to notify this unit manager
% (having requested to be notified of all events) that a world event has been
% received (and successfully matched).
%
-spec processAnyEventMatched( wooper:state(), world_event(),
							  sending_actor_pid() ) -> actor_oneway_return().
processAnyEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onAnyEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	actor:return_state( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a world event happened.
%
% Note: catch-all placeholder implementation, meant to be overridden (probably
% by a non-const oneway).
%
-spec onAnyEventMatched( wooper:state(), world_event() ) ->
								const_oneway_return().
onAnyEventMatched( State, Event ) ->

	?warning_fmt( "Default onAnyEventMatched/2 implementation ignoring ~ts.",
				  [ dataflow_support:world_event_to_string( Event ) ] ),

	wooper:const_return().




% Called (most probably by the experiment manager) to notify this unit manager
% that a creation event has been received and successfully matched against a
% clause specified by this unit manager.
%
-spec processCreationEventMatched( wooper:state(), creation_event(),
							sending_actor_pid() ) -> actor_oneway_return().
processCreationEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onCreationEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	actor:return_state( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching creation happened.
%
% Note: catch-all placeholder implementation, meant to be overridden (probably
% by a non-const oneway).
%
-spec onCreationEventMatched( wooper:state(), creation_event() ) ->
									const_oneway_return().
onCreationEventMatched( State, CreationEvent ) ->

	?warning_fmt( "Default onCreationEventMatched/2 implementation "
		"ignoring ~ts.",
		[ dataflow_support:world_event_to_string( CreationEvent ) ] ),

	wooper:const_return().




% Called (most probably by the experiment manager) to notify this unit manager
% that a destruction event has been received and successfully matched against a
% clause specified by this unit manager.
%
-spec processDestructionEventMatched( wooper:state(), destruction_event(),
								sending_actor_pid() ) -> actor_oneway_return().
processDestructionEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onDestructionEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	actor:return_state( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching destruction happened.
%
% Note: catch-all placeholder implementation, meant to be overridden (probably
% by a non-const oneway).
%
-spec onDestructionEventMatched( wooper:state(), destruction_event() ) ->
										const_actor_oneway_return().
onDestructionEventMatched( State, DestructionEvent ) ->

	?warning_fmt( "Default onDestructionEventMatched/2 implementation "
		"ignoring ~ts.",
		[ dataflow_support:world_event_to_string( DestructionEvent ) ] ),

	actor:const_return().




% Called (most probably by the experiment manager) to notify this unit manager
% that an association event has been received and successfully matched against a
% clause specified by this unit manager.
%
-spec processAssociationEventMatched( wooper:state(), association_event(),
								sending_actor_pid() ) -> actor_oneway_return().
processAssociationEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onAssociationEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	actor:return_state( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching association happened.
%
% Note: catch-all placeholder implementation, meant to be overridden (probably
% by a non-const oneway).
%
-spec onAssociationEventMatched( wooper:state(), association_event() ) ->
										const_oneway_return().
onAssociationEventMatched( State, AssociationEvent ) ->

	?warning_fmt( "Default onAssociationEventMatched/2 implementation "
		"ignoring ~ts.",
		[ dataflow_support:world_event_to_string( AssociationEvent ) ] ),

	wooper:const_return().



% Called (most probably by the experiment manager) to notify this unit manager
% that a binary association event has been received and successfully matched
% against a clause specified by this unit manager.
%
-spec processBinaryAssociationEventMatched( wooper:state(),
	binary_association_event(), sending_actor_pid() ) -> actor_oneway_return().
processBinaryAssociationEventMatched( State, Event, _SendingActorPid ) ->

	CalledState =
		executeOneway( State, onBinaryAssociationEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	actor:return_state( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching binary association happened.
%
% Note: catch-all placeholder implementation, meant to be overridden (probably
% by a non-const oneway).
%
-spec onBinaryAssociationEventMatched( wooper:state(),
			binary_association_event() ) -> const_oneway_return().
onBinaryAssociationEventMatched( State, BinaryAssociationEvent ) ->

	?warning_fmt( "Default onBinaryAssociationEventMatched/2 implementation "
		"ignoring ~ts.",
		[ dataflow_support:world_event_to_string( BinaryAssociationEvent ) ] ),

	wooper:const_return().



% Called (most probably by the experiment manager) to notify this unit manager
% that a disassociation event has been received and successfully matched against
% a clause specified by this unit manager.
%
% (actor oneway)
%
-spec processDisassociationEventMatched( wooper:state(), disassociation_event(),
								sending_actor_pid() ) -> actor_oneway_return().
processDisassociationEventMatched( State, Event, _SendingActorPid ) ->

	CalledState =
		executeOneway( State, onDisassociationEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	actor:return_state( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching disassociation happened.
%
% Note: catch-all placeholder implementation, meant to be overridden (probably
% by a non-const oneway).
%
-spec onDisassociationEventMatched( wooper:state(), disassociation_event() ) ->
											const_oneway_return().
onDisassociationEventMatched( State, DisassociationEvent ) ->

	?warning_fmt( "Default onDisassociationEventMatched/2 implementation "
		"ignoring ~ts.",
		[ dataflow_support:world_event_to_string( DisassociationEvent ) ] ),

	wooper:const_return().




% Called (most probably by the experiment manager) to notify this unit manager
% that a connection event has been received and successfully matched against a
% clause specified by this unit manager.
%
-spec processConnectionEventMatched( wooper:state(), connection_event(),
							sending_actor_pid() ) -> actor_oneway_return().
processConnectionEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onConnectionEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	actor:return_state( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching connection happened.
%
% Note: catch-all placeholder implementation, meant to be overridden (probably
% by a non-const oneway).
%
-spec onConnectionEventMatched( wooper:state(), connection_event() ) ->
										const_oneway_return().
onConnectionEventMatched( State, ConnectionEvent ) ->

	?warning_fmt( "Default onConnectionEventMatched/2 implementation "
		"ignoring ~ts.",
		[ dataflow_support:world_event_to_string( ConnectionEvent ) ] ),

	wooper:const_return().




% Called (most probably by the experiment manager) to notify this unit manager
% that a disconnection event has been received and successfully matched against
% a clause specified by this unit manager.
%
-spec processDisconnectionEventMatched( wooper:state(), connection_event(),
								sending_actor_pid() ) -> actor_oneway_return().
processDisconnectionEventMatched( State, Event, _SendingActorPid ) ->

	CalledState =
		executeOneway( State, onDisconnectionEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	actor:return_state( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching disconnection happened.
%
% Note: catch-all placeholder implementation, meant to be overridden (probably
% by a non-const oneway).
%
-spec onDisconnectionEventMatched( wooper:state(), disconnection_event() ) ->
											const_oneway_return().
onDisconnectionEventMatched( State, DisconnectionEvent ) ->

	?warning_fmt( "Default onDisconnectionEventMatched/2 implementation "
		"ignoring ~ts.",
		[ dataflow_support:world_event_to_string( DisconnectionEvent ) ] ),

	wooper:const_return().



% Called (most probably by the experiment manager) to notify this unit manager
% that an update event has been received and successfully matched against a
% clause specified by this unit manager.
%
-spec processUpdateEventMatched( wooper:state(), disassociation_event(),
								 sending_actor_pid() ) -> actor_oneway_return().
processUpdateEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onUpdateEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	actor:return_state( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching update happened.
%
% Note: catch-all placeholder implementation, meant to be overridden (probably
% by a non-const oneway).
%
-spec onUpdateEventMatched( wooper:state(), update_event() ) ->
									const_oneway_return().
onUpdateEventMatched( State, UpdateEvent ) ->

	?warning_fmt( "Default onUpdateEventMatched/2 implementation "
		"ignoring ~ts.",
		[ dataflow_support:world_event_to_string( UpdateEvent ) ] ),

	wooper:const_return().





% Creates, synchronously and while the simulation is not running, an (initial)
% instance of the specified unit type, associated to specified dataflow, using
% specified core construction parameters for that, and returning the
% corresponding instance PID.
%
-spec createInitialUnitInstance( wooper:state(), managed_unit_spec(),
				dataflow_pid(), construction_parameters() ) ->
										request_return( unit_pid() ).
createInitialUnitInstance( State, _UnitSpec={ UnitType, _Language=erlang },
						   DataflowPid, CoreConstructionParameters ) ->

	% Clause for standard (Erlang-based) units.

	% Building the full construction parameters for the new unit:
	?debug_fmt( "Creating an initial instance of unit type '~ts', associated "
		"to dataflow ~w, and based on following core construction "
		"parameters:~n~p",
		[ UnitType, DataflowPid, CoreConstructionParameters ] ),

	FullConstructParams = list_utils:append_at_end( DataflowPid,
												CoreConstructionParameters ),

	% Creating the unit with these parameters:
	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPid = class_Actor:create_initial_actor( UnitType, FullConstructParams,
												LoadBalancerPid ),

	% Will register itself to its dataflow at the first diasca of this unit.

	% May create a new entry for this unit type:
	NewUnitTable = table:append_to_entry( _K=UnitType, UnitPid,
										  ?getAttr(unit_table) ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	wooper:return_state_result( NewState, UnitPid );


% Created through a binding:
createInitialUnitInstance( State, _UnitSpec={ UnitType, Language },
						   DataflowPid, CoreConstructionParameters ) ->

	% Building the full construction parameters for the new unit:
	?debug_fmt( "Creating an initial instance of unit type '~ts', relying on "
		"the ~ts binding, associated to dataflow ~w, and based on "
		"following core construction parameters:~n~p",
		[ UnitType, language_utils:language_to_string( Language ),
		  DataflowPid, CoreConstructionParameters ] ),

	% Per-binding generic unit type (ex: class_DataflowPythonProcessingUnit):
	ActualUnitType = dataflow_binding_utils:get_erlang_unit_type( Language ),

	% Binding manager in charge of that language (ex: the PythonBindingManager):
	BindingManagerPid = binding_utils:get_binding_manager( Language,
											?getAttr(binding_managers) ),

	FullConstructParams = [ UnitType, CoreConstructionParameters, DataflowPid,
							BindingManagerPid ],

	% Creating the unit with these parameters:
	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPid = class_Actor:create_initial_actor( ActualUnitType,
								FullConstructParams, LoadBalancerPid ),

	% Will register itself to its dataflow at the first diasca of this unit.

	% May create a new entry for this unit type:
	NewUnitTable = table:append_to_entry( _K=UnitType, UnitPid,
										  ?getAttr(unit_table) ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	wooper:return_state_result( NewState, UnitPid );


createInitialUnitInstance( State, _UnitSpec=UnitType, DataflowPid,
		CoreConstructionParameters ) when is_atom( UnitType ) ->

	FullUnitSpec={ UnitType, _Language=erlang },

	{ NewState, UnitPid } = createInitialUnitInstance( State, FullUnitSpec,
					DataflowPid, CoreConstructionParameters ),

	wooper:return_state_result( NewState, UnitPid );


createInitialUnitInstance( State, UnitSpec, _DataflowPid,
						   _CoreConstructParameters ) ->

	?error_fmt( "Cannot instantiate a processing unit according to the "
		"following spec:~n   ~p~n"
		"The specification of a processing unit must be either an "
		"atom (WOOPER classname) or a pair whose first element is "
		"such an atom while the second element is an atom indicating "
		"its implementation language.", [ UnitSpec ] ),

	throw( { invalid_initial_unit_spec, UnitSpec } ).



% Creates, synchronously and while the simulation is not running, an (initial)
% instance of the specified modk-up unit type, using specified core construction
% parameters for that, and returning the corresponding instance PID.
%
-spec createInitialMockupUnitInstance( wooper:state(), mockup_unit_spec(),
		dataflow_pid(), class_DataflowProcessingUnit:unit_name() ) ->
											request_return( unit_pid() ).
createInitialMockupUnitInstance( State, MockupUnitSpec, DataflowPid,
								 UnitName ) ->

	UnitType = MockupUnitSpec#mockup_unit_spec.unit_type,

	?debug_fmt( "Creating an initial instance of mockup unit type '~ts', named "
		"~ts and based on the following specification record: ~p.",
		[ UnitType, UnitName, MockupUnitSpec ] ),

	% Building the full construction parameters for the new unit:
	FullConstructParams = [ UnitName, MockupUnitSpec, DataflowPid ],

	% Creating the unit with these parameters:
	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPid = class_Actor:create_initial_actor( class_DataflowMockupUnit,
								FullConstructParams, LoadBalancerPid ),

	% Will register itself to its dataflow at the first diasca of this unit.

	% May create a new entry for this unit type:
	NewUnitTable = table:append_to_entry( _K=UnitType, UnitPid,
										  ?getAttr(unit_table) ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	wooper:return_state_result( NewState, UnitPid ).



% Creates, synchronously and while the simulation is not running, a set of
% (initial) instances of the specified unit type, using the specified list of
% core construction parameters for that, and returning the corresponding
% instance PIDs, in the same order.
%
-spec createInitialUnitInstances( wooper:state(), managed_unit_spec(),
							dataflow_pid(), [ construction_parameters() ] ) ->
										request_return( [ unit_pid() ] ).
createInitialUnitInstances( State, _UnitSpec={ UnitType, _Language=erlang },
							DataflowPid, CoreConstructParamLists ) ->

	ParamStrings = [ text_utils:format( "~p", [ CPL ] )
					 || CPL <- CoreConstructParamLists ],

	?debug_fmt( "Creating ~B initial instances of unit type '~ts', associated "
		"to dataflow ~w, based on following list of core construction "
		"parameters: ~ts",
		[ length( CoreConstructParamLists ), UnitType, DataflowPid,
		  text_utils:strings_to_string( ParamStrings ) ] ),

	% Prepares a list of { Classname, FullConstructParams }:
	ConstructEntries = [ { UnitType,
						   list_utils:append_at_end( DataflowPid, CPL ) }
						 || CPL <- CoreConstructParamLists ],

	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPidList = class_Actor:create_initial_actors( ConstructEntries,
													 LoadBalancerPid ),

	UnitTable = ?getAttr(unit_table),

	UnitList = table:get_value( _K=UnitType, UnitTable ),

	NewUnitTable =
		table:add_entry( UnitType, UnitPidList ++ UnitList,	UnitTable ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	wooper:return_state_result( NewState, UnitPidList );


createInitialUnitInstances( State, _UnitSpec={ UnitType, Language },
							DataflowPid, CoreConstructParamLists ) ->

	ParamStrings = [ text_utils:format( "~p", [ CPL ] )
					 || CPL <- CoreConstructParamLists ],

	?debug_fmt( "Creating ~B initial instances of unit type '~ts', relying on "
		"the ~ts binding, associated to dataflow ~w, based on following "
		"list of core construction parameters: ~ts",
		[ length( CoreConstructParamLists ), UnitType,
		  language_utils:language_to_string( Language ), DataflowPid,
		  text_utils:strings_to_string( ParamStrings ) ] ),

	ActualUnitType = dataflow_binding_utils:get_erlang_unit_type( Language ),

	BindingManagerPid = binding_utils:get_binding_manager( Language,
												?getAttr(binding_managers) ),

	% Prepares a list of { Classname, FullConstructParams }:
	ConstructEntries = [ { ActualUnitType,
						   [ UnitType, CPL, DataflowPid, BindingManagerPid ] }
						 || CPL <- CoreConstructParamLists ],

	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPidList = class_Actor:create_initial_actors( ConstructEntries,
													 LoadBalancerPid ),

	UnitTable = ?getAttr(unit_table),

	UnitList = table:get_value( _K=UnitType, UnitTable ),

	NewUnitTable = table:add_entry( UnitType, UnitPidList ++ UnitList,
									UnitTable ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	wooper:return_state_result( NewState, UnitPidList );


createInitialUnitInstances( State, _UnitSpec=UnitType, DataflowPid,
		CoreConstructParamLists ) when is_atom( UnitType ) ->

	FullUnitSpec={ UnitType, _Language=erlang },

	{ NewState, UnitPidList } = createInitialUnitInstances( State, FullUnitSpec,
								DataflowPid, CoreConstructParamLists ),

	wooper:return_state_result( NewState, UnitPidList );



createInitialUnitInstances( State, UnitSpec, _DataflowPid,
							_CoreConstructParamLists ) ->

	?error_fmt( "Cannot instantiate processing units according to the "
		"following spec:~n   ~p~n"
		"The specification of a processing unit must be either an "
		"atom (WOOPER classname) or a pair whose first element is "
		"such an atom while the second element is an atom indicating "
		"its implementation language.", [ UnitSpec ] ),

	throw( { invalid_initial_unit_spec, UnitSpec } ).



% Creates, synchronously and while the simulation is not running, a set of
% (initial) instances of the specified mockup unit type, using a specification
% parameters record and the unit names for that, and returning the corresponding
% instance PIDs, in the same order.
%
-spec createInitialMockupUnitInstances( wooper:state(), mockup_unit_spec(),
			dataflow_pid(), [ class_DataflowProcessingUnit:unit_name() ] ) ->
											request_return( [ unit_pid() ] ).
createInitialMockupUnitInstances( State, MockupUnitSpec, DataflowPid,
								  UnitNames ) ->

	UnitType = MockupUnitSpec#mockup_unit_spec.unit_type,

	?debug_fmt( "Creating ~B initial instances of mockup unit type '~ts' based "
		"on the following specification parameters: ~ts",
		[ length( UnitNames ), UnitType,
		  text_utils:format( "~p", [ MockupUnitSpec ] ) ] ),

	% Prepares a list of { Classname, FullConstructParams }:
	CoreConstructParamList = [ [ UN, MockupUnitSpec ] || UN <- UnitNames ],

	ConstructEntries = [ { class_DataflowMockupUnit,
						   list_utils:append_at_end( DataflowPid, CP ) }
						 || CP <- CoreConstructParamList ],

	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPidList = class_Actor:create_initial_actors( ConstructEntries,
													 LoadBalancerPid ),

	UnitTable = ?getAttr(unit_table),

	UnitList = table:get_value( _K=UnitType, UnitTable ),

	NewUnitTable = table:add_entry( UnitType, UnitPidList ++ UnitList,
									UnitTable ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	wooper:return_state_result( NewState, UnitPidList ).



% Creates specified unit.
%
% Will trigger back a call to onUnitCreated/6.
%
% Like create_runtime_unit/4, except operating with the changeset system (rather
% than in a programmatic setting).
%
-spec createUnit( wooper:state(), managed_unit_spec(),
	wooper:construction_parameters(), event_id(), unit_creation_context() ) ->
						oneway_return().
createUnit( State, _UnitSpec={ UnitType, erlang }, UnitConstructParams,
			EventId, Context ) ->

	NewActionId = ?getAttr(action_count) + 1,

	?debug_fmt( "Creating a '~ts' unit, implemented in ~ts, with construction "
		"parameters ~p for event #~B (action #~B; context: ~p).",
		[ UnitType, language_utils:language_to_string( erlang ),
		  UnitConstructParams, EventId, NewActionId, Context ] ),

	CreatedState = class_Actor:create_actor( UnitType, UnitConstructParams,
											 _Tag=NewActionId, State ),

	% Registers the pending creation (corresponding to this new action), so that
	% its completion makes the processing of the overall event progress:
	%
	NewAction = { unit_creation, UnitType, UnitConstructParams, EventId,
				  Context },

	NewEventTable = register_action_for_event( NewActionId, EventId,
											   CreatedState ),

	NewActionTable = table:add_new_entry( NewActionId, NewAction,
										  ?getAttr(action_table) ),

	FinalState = setAttributes( CreatedState, [
					{ action_count, NewActionId },
					{ event_table, NewEventTable },
					{ action_table, NewActionTable } ] ),

	wooper:return_state( FinalState );


createUnit( State, _UnitSpec={ UnitType, Language }, UnitConstructParams,
			EventId, Context ) ->

	NewActionId = ?getAttr(action_count) + 1,

	?info_fmt( "Creating a '~ts' unit, implemented in ~ts, with construction "
		"parameters ~p for event #~B (action #~B; context: ~p).",
		[ UnitType, language_utils:language_to_string( Language ),
		  UnitConstructParams, EventId, NewActionId, Context ] ),

	% Per-binding generic unit type (ex: class_DataflowPythonProcessingUnit):
	ActualUnitType = dataflow_binding_utils:get_erlang_unit_type( Language ),

	% Binding manager in charge of that language (ex: the PythonBindingManager):
	BindingManagerPid = binding_utils:get_binding_manager( Language,
												?getAttr(binding_managers) ),

	%UnitConstructParams = [ UnitName, _Year=2020, 0.5, 1.0, DataflowPid ],

	% By convention, DataflowPid is the last element of the construction
	% parameters:
	%
	{ DataflowPid, OtherParams } =
		list_utils:extract_last_element( UnitConstructParams ),

	FullUnitConstructParams = [ UnitType, OtherParams ]
		++ [ DataflowPid, BindingManagerPid ],

	%trace_utils:debug_fmt( "ActualUnitType: '~p', "
	%					   "FullUnitConstructParams: '~p'.",
	%					   [ ActualUnitType, FullUnitConstructParams ] ),

	CreatedState = class_Actor:create_actor( ActualUnitType,
					   FullUnitConstructParams, _Tag=NewActionId, State ),

	% Registers the pending creation (corresponding to this new action), so that
	% its completion makes the processing of the overall event progress:
	%
	NewAction = { unit_creation, UnitType, UnitConstructParams, EventId,
				  Context },

	NewEventTable =
		register_action_for_event( NewActionId, EventId, CreatedState ),

	NewActionTable = table:add_new_entry( NewActionId, NewAction,
										  ?getAttr(action_table) ),

	FinalState = setAttributes( CreatedState, [
					{ action_count, NewActionId },
					{ event_table, NewEventTable },
					{ action_table, NewActionTable } ] ),

	wooper:return_state( FinalState );


createUnit( State, _UnitSpec=UnitType, UnitConstructParams, EventId,
			Context ) ->

	FullUnitSpec = { UnitType, _Language=erlang },

	CreatedState = createUnit( State, FullUnitSpec, UnitConstructParams,
							   EventId, Context ),

	wooper:return_state( CreatedState ).




% Destructs specified unit.
%
% Will trigger back a call to onUnitDestructed/4.
%
-spec destructUnit( wooper:state(), unit_pid(), event_id() ) -> oneway_return().
destructUnit( State, UnitPid, EventId ) ->

	NewActionId = ?getAttr(action_count) + 1,

	?debug_fmt( "Destructing unit ~p for event #~B (action #~B).",
				[ UnitPid, EventId, NewActionId ] ),

	% Will trigger a onUnitDestructed/4 callback:
	DestructedState = class_Actor:send_actor_message( UnitPid,
		{ triggerDestruction, [ NewActionId ] }, State ),

	NewAction = { unit_destruction, UnitPid, EventId },

	NewEventTable = register_action_for_event( NewActionId, EventId,
											   DestructedState ),

	NewActionTable = table:add_new_entry( NewActionId, NewAction,
										  ?getAttr(action_table) ),

	FinalState = setAttributes( DestructedState, [
					{ action_count, NewActionId },
					{ event_table, NewEventTable },
					{ action_table, NewActionTable } ] ),

	wooper:return_state( FinalState ).



% Connects directly (i.e. thanks to an unsynchronised request - thus to be done
% initially) the named output port of each of the specified upstream blocks to
% the specified iteration of the specified downstream unit, creating the
% corresponding iterated input ports for that.
%
-spec connectToIteratedInitially( wooper:state(),
	{ [ upstream_block_pid() ], output_port_name() }, iteration_port_target() )
					   -> const_request_return( 'connected_to_iterated' ).
connectToIteratedInitially( State, { UpstreamBlocks, OutputPortName },
							{ DownstreamUnitPid, InputIterationName } )
  when is_binary( OutputPortName ) andalso is_binary( InputIterationName ) ->

	ChannelCount = length( UpstreamBlocks ),

	?debug_fmt( "Creating ~B channels, from the output port '~ts' of each of "
		"the upstream units ~p to the input port iteration '~ts' of "
		"the downstream unit ~p.",
		[ ChannelCount, OutputPortName, UpstreamBlocks,
		  InputIterationName, DownstreamUnitPid ] ),

	% For that we have to request from the iteration the right number of input
	% iterated ports:
	%
	DownstreamUnitPid ! { createInputIteratedPorts,
						  [ InputIterationName, ChannelCount ], self() },

	InputPortNames = receive

		{ wooper_result, PortNames } ->
			PortNames

	end,

	% By design the waited units are exactly the upstream ones:
	request_initial_connections_to_iterated( UpstreamBlocks, OutputPortName,
										DownstreamUnitPid, InputPortNames ),

	% Answers to the connectOutputPortInitially/4 requests:
	wooper:wait_for_request_acknowledgements( ChannelCount,
											  output_port_connected ),

	wooper:const_return_result( connected_to_iterated ).



% Called automatically after (generally after two diascas) this manager created
% a unit instance.
%
% Parameters are:
%
% - CreatedUnitPid the PID of the just created unit
%
% - CreatedActorTag the tag used for this actor creation so that it is able to
% discriminate among the multiple creations it might have requested; this is
% here the identifier of the corresponding action
%
-spec onActorCreated( wooper:state(), unit_pid(), action_id(),
					  load_balancer_pid() ) -> actor_oneway_return().
onActorCreated( State, CreatedUnitPid, _CreatedActorTag=ActionId,
				_LoadBalancerPid ) ->

	% This is the generic part of any runtime unit creation, dispatching
	% relevant information to the (most probably overridden) onUnitCreated/6
	% oneway.

	ActionTable = ?getAttr(action_table),

	{ _Action={ unit_creation, UnitType, ConstructParams, EventId, Context },
	  ShrunkActionTable } = table:extract_entry( ActionId, ActionTable ),

	?void_fmt( "Recording newly created unit instance ~p of type ~ts "
		"for event #~B (created from ~p, with context ~p, "
		"through action #~B, in the course of the simulation).",
		[ CreatedUnitPid, UnitType, EventId, ConstructParams, Context,
		  ActionId ] ),

	ShrunkState = setAttribute( State, action_table, ShrunkActionTable ),

	% Domain-specific actions done there:
	UnitState = executeOneway( ShrunkState, onUnitCreated, [ UnitType,
		ConstructParams, CreatedUnitPid, EventId, Context ] ),

	% The onUnitCreated/6 method might have decided for more actions:
	DeclaredState = declare_action_performed( ActionId, EventId, UnitState ),

	UnitTable = getAttribute( DeclaredState, unit_table ),

	% Updates already-existing unit-type entry:

	case table:has_entry( UnitType, UnitTable ) of

		true ->
			ok;

		false ->

			KnownTypes = table:keys( UnitTable ),

			?error_fmt( "The '~ts' unit type is not known. It shall have been "
				"declared (with possibly a binding language) at the creation "
				"of this unit manager. Indeed the ~B only known types are: ~ts",
				[ UnitType, length( KnownTypes ),
				  text_utils:atoms_to_sorted_string( KnownTypes ) ] ),

			throw( { undeclared_unit_type, UnitType } )

	end,

	NewUnitTable = table:append_to_existing_entry( _K=UnitType, CreatedUnitPid,
												   UnitTable ),

	FinalState = setAttribute( DeclaredState, unit_table, NewUnitTable ),

	actor:return_state( FinalState ).



% Called whenever a unit has been created; meant to be overridden with any
% action needed (typically creating channels between this new unit and the rest
% of the dataflow).
%
% Parameters are:
%
% - CreatedUnitType is the type (classname) of the just created unit
%
% - CreatedUnitConstructionParameters is the construction parameters
% corresponding to this new creation
%
% - CreatedUnitPid is the PID of the just created unit
%
% - EventId is the identifier of the corresponding overall world event
%
% - CreationContext is the context of this creation
%
-spec onUnitCreated( wooper:state(), dataflow_unit_type(),
		wooper:construction_parameters(), unit_pid(), event_id(),
		unit_creation_context() ) -> const_oneway_return().
onUnitCreated( State, _CreatedUnitType, _CreatedUnitConstructionParameters,
			   _CreatedUnitPid, _EventId, _CreationContext ) ->

	?warning( "Default onUnitCreated/6 oneway not overridden." ),

	wooper:const_return().



% Called whenever a unit has been destructed (typically from the
% triggerDestruction/3 actor oneway of that unit).
%
% May be overridden if needed (in that case this base implementation shall be
% called from there).
%
% Parameters are:
%
% - DestructedUnitPid is the PID of the just destructd unit
%
% - ActionId is the identifier of the corresponding action
%
% - CreationContext is the context of this creation
%
% Note: it is an actor oneway, not a mere oneway like for onUnitCreated/6.
%
-spec onUnitDestructed( wooper:state(), action_id(), dataflow_unit_type(),
						unit_pid() ) -> actor_oneway_return().
onUnitDestructed( State, ActionId, UnitType,
				  _SendingActorPid=DestructedUnitPid ) ->

	% This is the generic part of any runtime unit destruction.
	% Note: very much like onConnectionsCreated/6.

	ActionTable = ?getAttr(action_table),

	% Includes a match-based check on unit PID:
	{ _Action={ unit_destruction, DestructedUnitPid, EventId },
	  ShrunkActionTable } = table:extract_entry( ActionId, ActionTable ),

	?debug_fmt( "Recording the destruction of unit ~w "
		"(in the context of action #~B of event #~B)",
		[ DestructedUnitPid, ActionId, EventId ] ),

	% Generally nothing domain-specific to be done here, thus no call to a
	% oneway in the spirit of onUnitCreated/6.

	DeclaredState = declare_action_performed( ActionId, EventId, State ),

	% Removes already-existing entry:
	NewUnitTable = table:delete_existing_from_entry( _K=UnitType,
							DestructedUnitPid, ?getAttr(unit_table) ),

	FinalState = setAttributes( DeclaredState, [
						{ unit_table, NewUnitTable },
						{ action_table, ShrunkActionTable } ] ),

	actor:return_state( FinalState ).





% Helper (unexported) functions.



% Requests the specified upstream units to connect their output port named as
% requested to a specific one among the provided iterated input ports of the
% downstream unit.
%
-spec request_initial_connections_to_iterated( [ unit_pid() ],
			output_port_name(), unit_pid(), [ input_port_name() ] ) -> void().
% Exhausted:
request_initial_connections_to_iterated( _UpstreamUnits=[], _OutputPortName,
								 _DownstreamUnitPid, _InputPortNames=[] ) ->
	ok;

request_initial_connections_to_iterated( _UpstreamUnits=[ UpUnitPid | TUnit ],
								 OutputPortName, DownstreamUnitPid,
								 _InputPortNames=[ InputPortName | TName ] ) ->

	UpUnitPid ! { connectOutputPortInitially,
				 [ OutputPortName, DownstreamUnitPid, InputPortName ], self() },

	request_initial_connections_to_iterated( TUnit, OutputPortName,
											 DownstreamUnitPid, TName );

% Both lists expected to be exhausted simultaneously:
request_initial_connections_to_iterated( UpstreamUnits, OutputPortName,
										 DownstreamUnitPid, InputPortNames ) ->
	throw( { inconsistent_internal_state, { UpstreamUnits, OutputPortName },
			 { DownstreamUnitPid, InputPortNames } } ).



% Returns a textual description of the unit instances currently managed.
-spec unit_table_to_string( wooper:state() ) -> string().
unit_table_to_string( State ) ->

	case table:enumerate( ?getAttr(unit_table) ) of

		[] ->
			 "not managing any unit type";

		Types ->

			StringEntries = [
				case IList of

					[] ->
						text_utils:format( "no instance of unit type '~ts'",
										   [ UName ] );

					_ ->
						text_utils:format(
						  "~B instance(s) of unit type '~ts': ~w",
						  [ length( IList ), UName, IList ] )

				end || { UName, IList } <- Types ],

			text_utils:format( "managing ~B unit types: ~ts",
				[ length( Types ),
				  text_utils:strings_to_string( StringEntries ) ] )

	end.



% Declares that specified action, in the context of specified event, has been
% performed.
%
% Possibly reports that this event is fully processed by this unit manager.
%
-spec declare_action_performed( action_id(), event_id(), wooper:state() ) ->
										wooper:state().
declare_action_performed( ActionId, EventId, State ) ->

	EventTable = ?getAttr(event_table),

	case table:lookup_entry( EventId, EventTable ) of

		{ value, ActionList } ->
			ShrunkActionList = list_utils:delete_existing( ActionId,
														   ActionList ),

			manage_possible_event_completion( ShrunkActionList, EventId,
											  EventTable, State );

		key_not_found ->
			erlang:error( { event_not_known, EventId } )

	end.




% Here, all actions (if any) for said event have been processed:
manage_possible_event_completion( _ActionList=[], EventId, EventTable,
								  State ) ->

	% Event may not be in table if called from a match clause not declaring any
	% action:
	%
	NewEventTable = table:remove_entry( EventId, EventTable ),

	% Just for traces here:
	case table:keys( NewEventTable ) of

		[] ->
			?void_fmt( "Reporting that event #~B has been fully "
				"processed; no more pending event.", [ EventId ] );

		EventList ->
			?void_fmt( "Reporting that event #~B has been fully "
				"processed; still ~B pending events: ~w.",
				[ EventId, length( EventList ), EventList ] )

	end,

	SentState = class_Actor:send_actor_message(
				  ?getAttr(experiment_manager_pid),
				  { onEventProcessed, [ EventId ] }, State ),

	setAttribute( SentState, event_table, NewEventTable );


% Here, at least one action is remaining:
manage_possible_event_completion( ActionList, EventId, EventTable, State ) ->
	NewEventTable = table:add_entry( EventId, ActionList, EventTable ),
	setAttribute( State, event_table, NewEventTable ).



% To be called typically from one of the process*Matched/3 actor oneways, to
% determine automatically whether the specified event is fully processed.
%
-spec manage_possible_event_completion( world_event(), wooper:state() ) ->
												wooper:state().
manage_possible_event_completion( Event, State ) ->

	EventId = dataflow_support:get_event_id( Event ),

	EventTable = ?getAttr(event_table),

	ActionList = get_actions_for_event( EventId, EventTable ),

	manage_possible_event_completion( ActionList, EventId, EventTable,
									  State ).




% Section for exported helpers.



% Creates a set of channels, in the context of the processing of the specified
% event, between specified upstream and downstream blocks, based on the
% specified port names (be they the same on both sides or not, be there standard
% or iterated ones), and returns an updated state.
%
% If just a name PortName is specified, then it is assumed that both endpoints
% are standard ports, and that they bear that same port name.
%
% Otherwise the complete form is to be used, a pair describing the output port
% and the input one. Not specifying the kind of port (standard or iteration)
% defaults to standard.
%
% Will ultimately trigger back a call to onConnectionsCreated/5.
%
% Note: to be used even if a single channel is to be created.
%
% (exported helper)
%
-spec create_channels_for( event_id(), upstream_block_pid(),
		downstream_block_pid(), [ connection_spec() ], wooper:state() ) ->
								 wooper:state().
create_channels_for( EventId, UpstreamBlockPid, DownstreamBlockPid,
					 ConnectionSpecs, State ) ->

	% Block endpoints and state specified to report clearer errors:
	%
	CanonicalConnectionSpecs = canonicalize_connection_specs( ConnectionSpecs,
								UpstreamBlockPid, DownstreamBlockPid, State ),

	?info_fmt( "Creating ~B channels in the context of event #~B, from "
		"upstream block ~w to downstream one ~w, using ~ts",
		[ length( CanonicalConnectionSpecs ), EventId, UpstreamBlockPid,
		  DownstreamBlockPid,
		  connection_specs_to_string( CanonicalConnectionSpecs ) ] ),

	NewActionId = ?getAttr(action_count) + 1,

	% Requests the upstream block to create these downstream channels:
	Oneway = { connectToDownstreamBlock,
			   [ CanonicalConnectionSpecs, DownstreamBlockPid, NewActionId ] },

	SentState =
		class_Actor:send_actor_message( UpstreamBlockPid, Oneway, State ),

	NewEventTable =
		register_action_for_event( NewActionId, EventId, SentState ),

	% Records that action for a later acknowledgement thereof:
	NewAction = { unit_connection, EventId, UpstreamBlockPid,
		DownstreamBlockPid, CanonicalConnectionSpecs, _Context=undefined },

	NewActionTable = table:add_new_entry( NewActionId, NewAction,
										  ?getAttr(action_table) ),

	setAttributes( SentState, [ { event_table, NewEventTable },
								{ action_table, NewActionTable },
								{ action_count, NewActionId } ] ).



% Called (by an upsteam block) once a set of channels from this upstream block
% to a downstream one has been created, as requested by the
% create_channels_for/5 helper of this unit manager.
%
% May be overridden if needed (in that case this base implementation shall be
% called from there).
%
% Parameters are:
%
% - PortPairs is a list of the actual names of the output and input ports
% created for the requested channels; these names correspond to standard ports,
% possibly created from any outut or input iteration
%
% - DownstreamBlockPid is the PID of the target block to which all channels are
% drawn
%
% - ActionId is the identifier of the corresponding action, as known by the unit
% manager that triggered it
%
% - UpstreamBlockPid is the PID of the source block from which all channels are
% drawn; it happens also to be the sending actor
%
% Note: it is an actor oneway, not a mere oneway like for onUnitCreated/6.
%
-spec onConnectionsCreated( wooper:state(), [ connection_info() ], actor_pid(),
					action_id(), sending_actor_pid() ) -> actor_oneway_return().
onConnectionsCreated( State, PortPairs, DownstreamBlockPid, ActionId,
					  _SenderPid=UpstreamBlockPid ) ->

	ActionTable = ?getAttr(action_table),

	% Includes a match-based check on block PIDs:
	{ _Action={ unit_connection, EventId, UpstreamBlockPid, DownstreamBlockPid,
				CanonicalConnectionSpecs, _Context=undefined },
	  ShrunkActionTable } = table:extract_entry( ActionId, ActionTable ),

	?void_fmt( "Recording ~B channel connections through action #~B, from "
		"upstream block ~w to downstream one ~w, involving following "
		"ports: ~ts~n(canonical connection specs were: ~ts)",
		[ length( PortPairs ), ActionId, UpstreamBlockPid,
		  DownstreamBlockPid, text_utils:strings_to_string(
			[ text_utils:format( "from output port '~ts' to input one '~ts'",
								 [ OutputPortName, InputPortName ] )
			  || { OutputPortName, InputPortName } <- PortPairs ] ),
		  connection_specs_to_string( CanonicalConnectionSpecs ) ] ),

	% Generally nothing domain-specific to be done here, thus no call to a
	% oneway in the spirit of onUnitCreated/6.

	DeclaredState = declare_action_performed( ActionId, EventId, State ),

	FinalState = setAttribute( DeclaredState, action_table, ShrunkActionTable ),

	actor:return_state( FinalState ).



% Creates a set of output ports on the specified block.
%
% This is a synchronous call: ports are already created when it returns.
%
-spec create_output_ports( block_pid(), [ output_port_spec() ] ) -> void().
create_output_ports( BlockPid, OutputPortSpecs ) ->
	wooper:execute_request( BlockPid, createOutputPorts, [ OutputPortSpecs ],
							_ExpectedResult=output_ports_created ).




% Stringification section.



% Returns a textual description of the specified synchronization event matches.
-spec event_clauses_to_string( [ event_match() ] ) -> ustring().
event_clauses_to_string( EventMatches ) ->

	EventString = text_utils:strings_to_string(
					[ event_clause_to_string( E ) || E <- EventMatches ] ),

	text_utils:format( "~B synchronization event matches: ~ts",
					   [ length( EventMatches ), EventString ] ).



% Returns a textual description of the specified synchronization event clause.
-spec event_clause_to_string( event_match() ) -> ustring().
event_clause_to_string( EventMatch=#creation_event_match{} ) ->
	text_utils:format( "creation clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#destruction_event_match{} ) ->
	text_utils:format( "destruction clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#association_event_match{} ) ->
	text_utils:format( "association clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#binary_association_event_match{} ) ->
	text_utils:format( "binary association clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#disassociation_event_match{} ) ->
	text_utils:format( "disassociation clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#connection_event_match{} ) ->
	text_utils:format( "connection clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#disconnection_event_match{} ) ->
	text_utils:format( "disconnection clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#update_event_match{} ) ->
	text_utils:format( "update clause ~p", [ EventMatch ] );

event_clause_to_string( _EventMatch=any_event_type ) ->
	"clause corresponding to any type of event".




% Returns a textual description of the specified action.
-spec action_to_string( action() ) -> ustring().
action_to_string( { unit_creation, EventId, UnitType, ConstructParams,
					Context } ) ->
	text_utils:format( "unit creation for event #~B, for unit type ~ts, "
		"construction parameters ~p and context ~p",
		[ EventId, UnitType, ConstructParams, Context ] );

action_to_string( { unit_connection, EventId, OutputPortId, InputPortId,
					Context } ) ->
	text_utils:format( "channel creation for event #~B, from ~ts to ~ts, "
		"context ~p", [ EventId,
		dataflow_support:port_id_to_string( OutputPortId ),
		dataflow_support:port_id_to_string( InputPortId ), Context ] ).



% Returns a textual description of this unit manager.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	IdString = case ?getAttr(identification_server_pid) of

		undefined ->
			"no identification server";

		IdPid ->
			text_utils:format( "identification server ~w", [ IdPid ] )

	end,

	UnitTypeString = unit_table_to_string( State ),

	ClauseString = event_clauses_to_string( ?getAttr(event_matches) ),

	EventString = case table:enumerate( ?getAttr(event_table) ) of

		[] ->
			"no pending event";

		EventPairs ->
			EvStrings = [ text_utils:format( "the processing of event #~B "
							"involves following ~B actions: ~w",
							[ EvId, length( Actions ), Actions ] )
						  || { EvId, Actions } <- EventPairs ],

			text_utils:format( "following ~B events pending: ~ts",
				[ length( EventPairs ),
				  text_utils:strings_to_string( EvStrings ) ] )

	end,

	ActionString = case table:enumerate( ?getAttr(action_table) ) of

		[] ->
			"no pending action";

		ActionPairs ->

			AcStrings = [ text_utils:format( "action #~B: ~ts",
								[ AcId, action_to_string( Action ) ] )
						  || { AcId, Action } <- ActionPairs ],

			text_utils:format( "following ~B actions pending: ~ts",
				[ length( ActionPairs ),
				  text_utils:strings_to_string( AcStrings ) ] )

	end,

	text_utils:format( "unit manager linked to experiment manager ~p, "
		"knowing ~ts, ~ts~nListening to ~ts; having ~ts and having ~ts",
		[ ?getAttr(experiment_manager_pid), IdString, UnitTypeString,
		  ClauseString, EventString, ActionString ] ).




% Static section.


% Creates (initially, i.e. before the simulation is started) the specified unit
% managers, supposing here that they accept exactly four construction
% parameters, i.e.:
%
% - the PID of their parent manager
% - the PID of the load balancer
% - a binding_managers record referencing, for each of the supported programming
% languages, the PID of the associated binding manager (if any)
% - the PID of any identification server in use (here: none)
%
% Returns the list of their PID in the same order as the one of their names.
%
-spec create_managers( [ wooper:classname() ], experiment_manager_pid(),
					   binding_managers(), load_balancer_pid() ) ->
							static_return( [ unit_manager_pid() ] ).
create_managers( UnitManagerNames, ExperimentManagerPid, BindingManagers,
				 LoadBalancerPid ) ->

	IdentificationServerPid = undefined,

	UnitManagerPids = create_managers( UnitManagerNames, ExperimentManagerPid,
				   BindingManagers, LoadBalancerPid, IdentificationServerPid ),

	wooper:return_static( UnitManagerPids ).



% Creates (initially, i.e. before the simulation is started) the specified unit
% managers, supposing here that they accept exactly four construction
% parameters, i.e.:
%
% - the PID of their parent manager
% - the PID of the load balancer
% - a binding_managers record referencing, for each of the supported programming
% languages, the PID of the associated binding manager (if any)
% - the PID of the identification server in use
%
% Returns the list of their PID in the same order as the one of their names.
%
-spec create_managers( [ wooper:classname() ], experiment_manager_pid(),
		binding_managers(), load_balancer_pid(),
		maybe( identification_server_pid() ) ) ->
			static_return( [ unit_manager_pid() ] ).
create_managers( UnitManagerNames, ExperimentManagerPid, BindingManagers,
				 LoadBalancerPid, IdentificationServerPid ) ->

	ConstructionParameters = [ ExperimentManagerPid, BindingManagers,
							   LoadBalancerPid, IdentificationServerPid ],

	% By convention the name of a unit manager is its classname:
	UnitManagerPids =
		[ class_Actor:create_initial_actor( Classname, ConstructionParameters )
		  || Classname <- UnitManagerNames ],

	wooper:return_static( UnitManagerPids ).



% Requests the synchronous creation by specified unit manager of an initial
% (i.e. not dynamic, at runtime) instance of specified unit type, using
% specified core construction parameters for that, and returns the PID of the
% created unit instance.
%
% Note: only the core, unit-specific construction parameters shall be
% specified; the others (actor-specific ones, dataflow PID, etc.) will be added
% automatically.
%
% For example, for a call to class_Foobar:construct(State, ActorSettings, A, B,
% C, DataflowPid) to happen, [A, B, C] shall be specified as core construction
% parameters.
%
% Defined for convenience, typically when implementing a simulation case.
%
-spec create_initial_unit( unit_manager_pid(), managed_unit_spec(),
	dataflow_pid(), construction_parameters() ) -> static_return( unit_pid() ).
create_initial_unit( UnitManagerPid, UnitSpec, DataflowPid,
					 CoreConstructionParameters ) ->

	UnitManagerPid ! { createInitialUnitInstance,
		[ UnitSpec, DataflowPid, CoreConstructionParameters ], self() },

	receive

		{ wooper_result, UnitPid } when is_pid( UnitPid ) ->
			wooper:return_static( UnitPid )

	end.



% Requests the synchronous creation by specified unit manager of an initial
% (i.e. not dynamic, at runtime) instance of specified mockup unit type, using
% specified core construction parameters for that, and returns the PID of the
% created mockup unit instance.
%
% Note: only the core, unit-specific construction parameters shall be
% specified; the others (actor-specific ones, dataflow PID, etc.) will be added
% automatically.
%
% For example, for a call to class_Foobar:construct(State, ActorSettings, A, B,
% C, DataflowPid) to happen, [A, B, C] shall be specified as core construction
% parameters.
%
% Defined for convenience, typically when implementing a simulation case.
%
-spec create_initial_mockup_unit( unit_manager_pid(), mockup_unit_spec(),
				dataflow_pid(), class_DataflowProcessingUnit:unit_name() ) ->
										static_return( [ unit_pid() ] ).
create_initial_mockup_unit( UnitManagerPid, MockupUnitSpec, DataflowPid,
							UnitName ) ->

	UnitManagerPid ! { createInitialMockupUnitInstance,
					   [ MockupUnitSpec, DataflowPid, UnitName ], self() },

	receive

		{ wooper_result, UnitPid } when is_pid( UnitPid ) ->
			wooper:return_static( UnitPid )

	end.



% Requests the synchronous creations by specified unit manager of a set of
% initial (i.e. not dynamic, at runtime) instances of the specified unit type,
% associated to specified dataflow, using specified list of core construction
% parameters for that, and returns the list of the PIDs of the created unit
% instances, in the order of their construction parameters.
%
% Note: only the core, unit-specific construction parameters shall be specified
% (actor-specific ones, dataflow PID, etc.) will be added automatically.
%
% For example, for a call to class_Foobar:construct(State, ActorSettings, A, B,
% C, DataflowPid) to happen, [A, B, C] shall be specified as core construction
% parameters.
%
% Defined for convenience, typically when implementing a simulation case.
%
-spec create_initial_units( unit_manager_pid(), managed_unit_spec(),
							dataflow_pid(), [ construction_parameters() ] ) ->
								  static_return( [ unit_pid() ] ).
create_initial_units( UnitManagerPid, UnitSpec, DataflowPid,
					  CoreConstructionParamLists ) ->

	UnitManagerPid ! { createInitialUnitInstances,
		[ UnitSpec, DataflowPid, CoreConstructionParamLists ], self() },

	receive

		{ wooper_result, UnitPidList } when is_list( UnitPidList ) ->
			wooper:return_static( UnitPidList )

	end.



% Requests the synchronous creations by specified unit manager of a set of
% initial (i.e. not dynamic, at runtime) mockup instances of the specified unit
% type, associated to specified dataflow, using specified list of core
% construction parameters for that, and returns the list of the PIDs of the
% created unit instances, in the order of their construction parameters.
%
% Note: only the core, unit-specific construction parameters shall be specified
% (actor-specific ones, dataflow PID, etc.) will be added automatically.
%
% For example, for a call to class_Foobar:construct(State, ActorSettings, A, B,
% C, DataflowPid) to happen, [A, B, C] shall be specified as core construction
% parameters.
%
% Defined for convenience, typically when implementing a simulation case.
%
-spec create_initial_mockup_units( unit_manager_pid(), mockup_unit_spec(),
			dataflow_pid(), [ class_DataflowProcessingUnit:unit_name() ] ) ->
									static_return( [ unit_pid() ] ).
create_initial_mockup_units( UnitManagerPid, MockupUnitSpec, DataflowPid,
							 UnitNames ) ->

	UnitManagerPid ! { createInitialMockupUnitInstances,
					   [ MockupUnitSpec, DataflowPid, UnitNames ], self() },

	receive

		{ wooper_result, UnitPidList } when is_list( UnitPidList ) ->
			wooper:return_static( UnitPidList )

	end.




% Creation helpers (they rely on a state).


% Creates, at runtime (i.e. in the course of the simulation), a unit of
% specified type (classname), associated with specified dataflow, based on
% specified list of core construction parameters, and returns an updated state.
%
% To be called from an actor, typically a specialised unit manager.
%
% (helper)
%
-spec create_runtime_unit( managed_unit_spec(), dataflow_pid(),
			[ construction_parameters() ], wooper:state() ) -> wooper:state().
create_runtime_unit( _UnitSpec={ UnitType, erlang }, DataflowPid,
					 CoreConstructionParameters, State ) ->

	?debug_fmt( "Creating a runtime instance of unit type '~ts', "
		"implemented in ~ts, associated to dataflow ~w, based on "
		"following core construction parameters: ~p",
		[ UnitType, language_utils:language_to_string( erlang ),
		  DataflowPid, CoreConstructionParameters ] ),

	% Building the full construction parameters for the new unit:

	FullConstructParams =
		list_utils:append_at_end( DataflowPid, CoreConstructionParameters ),

	% Returns an updated state; the PID of the created actor will be recorded in
	% onActorCreated/4.
	%
	class_Actor:create_actor( UnitType, FullConstructParams, State );


create_runtime_unit( _UnitSpec={ UnitType, Language }, DataflowPid,
					 CoreConstructionParameters, State ) ->

	?debug_fmt( "Creating a runtime instance of unit type '~ts', "
		"implemented in ~ts, associated to dataflow ~w, based on "
		"following core construction parameters: ~p",
		[ UnitType, language_utils:language_to_string( Language ),
		  DataflowPid, CoreConstructionParameters ] ),

	% Per-binding generic unit type (ex: class_DataflowPythonProcessingUnit):
	ActualUnitType = dataflow_binding_utils:get_erlang_unit_type( Language ),

	% Binding manager in charge of that language (ex: the PythonBindingManager):
	BindingManagerPid = binding_utils:get_binding_manager( Language,
												?getAttr(binding_managers) ),

	% Building the full construction parameters for the new unit:

	FullConstructParams = [ UnitType, CoreConstructionParameters, DataflowPid,
							BindingManagerPid ],

	% Returns an updated state; the PID of the created actor will be recorded in
	% onActorCreated/4.
	%
	class_Actor:create_actor( ActualUnitType, FullConstructParams, State );


create_runtime_unit( _UnitSpec=UnitType, DataflowPid,
					 CoreConstructionParameters, State ) ->

	FullUnitSpec = { UnitType, _Language=erlang },

	create_runtime_unit( FullUnitSpec, DataflowPid, CoreConstructionParameters,
						 State ).



% Connects and directly (i.e. based on a direct request, not on an actor message
% - thus to be done initially), thanks to the specified unit manager, the named
% output port of the listed upstream blocks to a target port iteration,
% specified thanks to the target (downstream) unit and the name of its
% iteration.
%
% Defined for convenience.
%
-spec connect_to_iterated_initially( unit_manager_pid(),
	{ [ upstream_block_pid() ], output_port_string_name() },
	iteration_port_string_target() ) -> static_void_return().
connect_to_iterated_initially( UnitManagerPid,
		_SourcePorts={ UpstreamBlocks, OutputPortName },
		_TargetIteration={ DownstreamUnitPid, InputIterationName } ) ->

	BinSourcePorts = { UpstreamBlocks,
					   text_utils:string_to_binary( OutputPortName ) },

	BinTargetIteration = { DownstreamUnitPid,
						   text_utils:string_to_binary( InputIterationName ) },

	UnitManagerPid ! { connectToIteratedInitially,
					   [ BinSourcePorts, BinTargetIteration ], self() },

	receive

		{ wooper_result, connected_to_iterated } ->
			wooper:return_static_void()

	end.





% Canonicalization section.



% Canonicalizes specified connection specs.
-spec canonicalize_connection_specs( [ connection_spec() ],
			upstream_block_pid(), downstream_block_pid(), wooper:state() ) ->
											[ canonical_connection_spec() ].
canonicalize_connection_specs( ConnectionSpecs, UpstreamBlockPid,
							   DownstreamBlockPid, State )
  when is_list( ConnectionSpecs ) ->
	[ canonicalize_connection_spec( Spec, UpstreamBlockPid, DownstreamBlockPid,
									State ) || Spec <- ConnectionSpecs ];

canonicalize_connection_specs( Other, UpstreamBlockPid, DownstreamBlockPid,
							   State ) ->

	?error_fmt( "Invalid connection specification: ~p (not a list), "
		"from upstream block ~w to downstream one ~w.",
		[ Other, UpstreamBlockPid, DownstreamBlockPid ] ),

	throw( { invalid_connection_specs, Other, UpstreamBlockPid,
			 DownstreamBlockPid } ).



% Canonicalizes specified connection spec.
-spec canonicalize_connection_spec( connection_spec(), upstream_block_pid(),
		 downstream_block_pid(), wooper:state()) -> canonical_connection_spec().
canonicalize_connection_spec( { UpstreamSpec, DownstreamSpec },
							  UpstreamBlockPid, DownstreamBlockPid, State ) ->

	{ canonicalize_upstream_connection_spec( UpstreamSpec, UpstreamBlockPid,
											 DownstreamBlockPid, State  ),
	  canonicalize_downstream_connection_spec( DownstreamSpec, UpstreamBlockPid,
											   DownstreamBlockPid, State  ) };

% A single name means it is to apply to both endpoints:
canonicalize_connection_spec( PortStringName, UpstreamBlockPid,
				  DownstreamBlockPid, State ) when is_list( PortStringName ) ->
	canonicalize_connection_spec( { PortStringName, PortStringName },
								  UpstreamBlockPid, DownstreamBlockPid, State );

canonicalize_connection_spec( Other, UpstreamBlockPid, DownstreamBlockPid,
							  State ) ->

	?error_fmt( "Invalid connection specification: port name '~p' is not a "
		"string (upstream block ~w, downstream one ~w).",
		[ Other, UpstreamBlockPid, DownstreamBlockPid ] ),

	throw( { invalid_connection_spec, Other } ).




% Canonicalizes specified upstream connection spec.
canonicalize_upstream_connection_spec(
  { output_port_name, OutputPortStringName }, _UpstreamBlockPid,
  _DownstreamBlockPid, _State ) when is_list( OutputPortStringName ) ->
	{ output_port_name, text_utils:string_to_binary( OutputPortStringName ) };

canonicalize_upstream_connection_spec( { output_iteration_name,
		   OutputIterationStringName }, _UpstreamBlockPid, _DownstreamBlockPid,
		   _State ) when is_list( OutputIterationStringName ) ->
	{ output_iteration_name,
	  text_utils:string_to_binary( OutputIterationStringName ) };

% Not specified means standard port:
canonicalize_upstream_connection_spec( OutputPortStringName, UpstreamBlockPid,
	   DownstreamBlockPid, State ) when is_list( OutputPortStringName ) ->
	canonicalize_upstream_connection_spec(
	  { output_port_name, OutputPortStringName }, UpstreamBlockPid,
	  DownstreamBlockPid, State );

canonicalize_upstream_connection_spec( Other, UpstreamBlockPid,
									   DownstreamBlockPid, State ) ->

	?error_fmt( "Invalid upstream connection specification: port name '~p' is "
		"not a string (upstream block ~w, downstream one ~w).",
		[ Other, UpstreamBlockPid, DownstreamBlockPid ] ),

	throw( { invalid_upstream_connection_spec, Other } ).



% Canonicalizes specified downstream connection spec.
canonicalize_downstream_connection_spec(
  { input_port_name, InputPortStringName }, _UpstreamBlockPid,
  _DownstreamBlockPid, _State ) when is_list( InputPortStringName ) ->
	{ input_port_name, text_utils:string_to_binary( InputPortStringName ) };

canonicalize_downstream_connection_spec( { input_iteration_name,
		InputIterationStringName }, _UpstreamBlockPid, _DownstreamBlockPid,
		_State ) when is_list( InputIterationStringName ) ->
	{ input_iteration_name,
	  text_utils:string_to_binary( InputIterationStringName ) };

% Not specified means standard port:
canonicalize_downstream_connection_spec( InputPortStringName, UpstreamBlockPid,
										 DownstreamBlockPid, State )
  when is_list( InputPortStringName )->
	canonicalize_downstream_connection_spec(
	  { input_port_name, InputPortStringName }, UpstreamBlockPid,
	  DownstreamBlockPid, State );

canonicalize_downstream_connection_spec( Other, UpstreamBlockPid,
										 DownstreamBlockPid, State ) ->

	?error_fmt( "Invalid downstream connection specification: port name "
		"'~p' is not a string (upstream block ~w, downstream one ~w).",
		[ Other, UpstreamBlockPid, DownstreamBlockPid ] ),

	throw( { invalid_downstream_connection_spec, Other } ).



% Associates specified action identifier to specified event being processed, by
% returning an updated event table.
%
-spec register_action_for_event( action_id(), event_id(), wooper:state() ) ->
										event_table().
register_action_for_event( ActionId, EventId, State ) ->

	EventTable = ?getAttr(event_table),

	case table:lookup_entry( EventId, EventTable ) of

		{ value, ActionList } ->
			case lists:member( ActionId, ActionList ) of

				false ->
					ok;

				true ->
					throw( { duplicated_action_id, ActionId, EventId,
							 ActionList } )

			end,

			NewActionList = [ ActionId | ActionList ],
			table:add_entry( EventId, NewActionList, EventTable );

		key_not_found ->
			table:add_entry( EventId, _NewActionList=[ ActionId ], EventTable )

	end.



% Returns a (possibly empty) list of the actions associated to specified event.
-spec get_actions_for_event( event_id(), event_table() ) -> [ action_id() ].
get_actions_for_event( EventId, EventTable ) ->

	% As long as no related action has been declared for an event, that event is
	% not known in the event table:

	case table:lookup_entry( EventId, EventTable ) of

		{ value, ActionList } ->
			ActionList;

		key_not_found ->
			[]

	end.



% to_string section.


% (helper)
-spec connection_specs_to_string( [ canonical_connection_spec() ] ) ->
										ustring().
connection_specs_to_string( _ConnectionSpecs=[] ) ->
	"empty connection specification";

connection_specs_to_string( ConnectionSpecs ) ->

	ConnectionString = text_utils:strings_to_string(
		  [ connection_spec_to_string( Spec ) || Spec <- ConnectionSpecs ] ),

	text_utils:format( "following ~B connection specifications: ~ts",
					   [ length( ConnectionSpecs ), ConnectionString ] ).



% Note: applies to all versions of connection specifications (user-defined or
% canonical ones).
%
-spec connection_spec_to_string(
		connection_spec() | canonical_connection_spec() ) -> ustring().
connection_spec_to_string( UniquePortSpec ) when is_list( UniquePortSpec ) ->
	text_utils:format( "connection between two standard ports, "
		"both named '~ts'", [ UniquePortSpec ] );

connection_spec_to_string( { UpstreamPortSpec, DownstreamPortSpec } ) ->
	text_utils:format( "connection from ~ts to ~ts", [
		upstream_spec_to_string( UpstreamPortSpec ),
		downstream_spec_to_string( DownstreamPortSpec ) ] );

connection_spec_to_string( Unexpected ) ->
	throw( { unexpected_connection_spec, Unexpected } ).



% (helper)
upstream_spec_to_string( { output_port_name, PortName } ) ->
	text_utils:format( "standard output port named '~ts'", [ PortName ] );

upstream_spec_to_string( { output_iteration_name, IterName } ) ->
	text_utils:format( "output port iteration named '~ts'", [ IterName ] );

upstream_spec_to_string( PortName ) when is_list( PortName ) ->
	text_utils:format( "standard output port named '~ts'", [ PortName ] );

upstream_spec_to_string( Other ) ->
	throw( { unexpected_upstream_connection_spec, Other } ).



% (helper)
downstream_spec_to_string( { input_port_name, PortName } ) ->
	text_utils:format( "standard input port named '~ts'", [ PortName ] );

downstream_spec_to_string( { input_iteration_name, IterName } ) ->
	text_utils:format( "input port iteration named '~ts'", [ IterName ] );

downstream_spec_to_string( PortName ) when is_list( PortName ) ->
	text_utils:format( "standard input port named '~ts'", [ PortName ] );

downstream_spec_to_string( Other ) ->
	throw( { unexpected_downstream_connection_spec, Other } ).
