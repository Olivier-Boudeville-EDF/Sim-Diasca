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

% Authors:  Olivier Boudeville (olivier.boudeville@edf.fr)
%           Samuel Thiriot     (samuel.thiriot@edf.fr)


-module(class_DataflowObjectManager).


-define( class_description,
		 "The dataflow object manager is a base (possibly overridden) class, "
		 "common to all actual managers for a given set of types of dataflow "
		 "objects (they are instances either directly of that class, or from "
		 "child classes thereof). "
		 "For example, if a simulation involves buildings in the form of "
		 "dataflow objects, these instances could be managed by a "
		 "BuildingManager that may inherit (probably directly) from the "
		 "current class, if some domain-specific handling of dataflow objects "
		 "is needed (ex: to manage specific associations); "
		 "otherwise this BuildingManager may simply be directly an instance of "
		 "the current class. "
		 "If relevant, a given object manager may take care of multiple types "
		 "of objects (ex: a manager may be in charge of the building "
		 "instances, but also the ones of dwellings, households, lifts, etc.). "
		 "Each object manager is directly linked to the world manager. "
		 "Each object manager is a singleton and registers itself globally "
		 "under its name - which is, conventionally, its actual classname "
		 "(ex: 'class_BuildingManager' or, if needing more clarity, "
		 "'class_BuildingObjectManager'). It registers as well to the world "
		 "manager. "
		 "As an object manager may have to create dataflow objects at runtime "
		 "(for the types of objects it is in charge of), it must itself be a "
		 "(simulation) actor." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).



% Design notes:

% Managing object types.
%
% An object manager may be dedicated to a given object type, or may handle
% multiple object types (typically if they are tightly linked, and thus to be
% managed in a single operation).
%
% We want ultimately that the world manager (i.e. the root, top-level object
% manager) stores the full table allowing to convert an object type (ex:
% 'Building' or 'Dwelling') into the PID of the object manager in charge of that
% type.
%
% Object managers are to declare themselves to the world manager.

% Dataflow objects may be created statically (whereas the simulation is not
% started, i.e. as initial objects, typically from the simulation case) or
% dynamically (while the simulation is running, i.e. as runtime objects,
% typically from the experiment entry point or, as expected, from dataflow
% object managers).



% Managing events and induced ones.

% When applying a changeset, all first level events are applied in turn (in the
% same diasca). Then they are stored (in the triggered_events attribute, with a
% tick-based timestamp) so that, on completion, their induced events can be
% applied in turn (this is true for creations and destructions, that may last
% for an unbounded number of diascas, and also for "shorter", constant-time,
% oneway operations like associations or updates, to preserve uniformity and
% induction order).
%
% A given object manager is expected to be able to handle by itself all induced
% events stemming from the events it handles.
%
% A changeset possibly modifies the structure of the dataflow and the state of
% at least some of its blocks.
%
% Blocks that have at least one of their input ports assigned shall be put in
% the suspended state, to prevent that get activated while the dataflow is still
% being modified.
%
% When all changesets have been applied, these blocks shall be explicitly
% resumed so that the actual evaluation of the dataflow(s) can take place. As a
% result, the set of blocks (most probably dataflow objects) that will have to
% be resumed must be kept (in addition to calling their *Suspending* methods
% such as setSuspendingInputPortValue/4); this suspended list is managed
% by the dataflow itself (see the declareSuspendedBlock* methods).



% The attributes that are specific to an object manager are:
-define( class_attributes, [

	{ object_table, object_table(), "an associative table whose keys are the "
	  "types of objects that are managed by this instance and whose values are "
	  "(supposedly exhaustive) lists of instances of that type (these "
	  "instances are only referenced, not owned, as their life cycle is to be "
	  "managed by their parent dataflow)" },

	{ world_manager_pid, world_manager_pid(), "PID of the world manager" },

	{ dispatch_table, table( object_manager_pid(), [ world_event() ] ),
	  "a table associating to an object manager a list of the world events "
	  "whose application was dispatched to it" },

	{ triggered_events, [ world_event() ], "a list of the (time-stamped, to "
	  "detect unhandled events) world events whose application is triggered "
	  "yet not completed; once done, their completion will be reported to the "
	  "world manager" },

	{ completed_event_infos, [ completed_event_info() ], "a list of the "
	  "information about the world events that have been completed, yet not "
	  "reported yet to the world manager" },

	{ injected_events, [ world_event() ], "a list of the world events that "
	  "the processing of another event led to inject (ex: if a child class of "
	  "this class processed the creation of a car, it may inject 4 creation "
	  "events for each of the associated wheels)" },

	{ load_balancer_pid, load_balancer_pid(), "PID of the load balancer, "
	  "useful to create new objects for example" },

	{ identification_server_pid, maybe( identification_server_pid() ),
	  "if enabled by the case, the PID of the identification server" } ] ).



% Helpers exported for convenience:
-export([ apply_world_events/3, to_string/1, get_object_pids/2,
		  create_runtime_object/4 ]).


% Records all instances of a managed object type:
-type object_table() :: table( dataflow_object_type(), [ object_pid() ] ).



% Records the identifier of a completed event and any additional relevant
% information (such as the PID of the created object should it refers to a
% creation event)
%
% (exported to avoid unused warning)
%
-type completed_event_info() ::
		{ event_id(), class_WorldManager:completion_extra_info() }.


-export_type([ object_table/0, completed_event_info/0 ]).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.World.ObjectManager" ).


% For input_port_spec and all:
-include("dataflow_defines.hrl").


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").


% Definition of any object manager that is defined by default (i.e. with no
% specific construction parameter), based only on its name and on a list of the
% types of dataflow objects it is to manage.
%
-type object_manager_def() ::
		{ object_manager_name(), [ dataflow_object_type() ] }.


% Shorthands:

-type ustring() :: text_utils:ustring().

-type classname() :: wooper:classname().



% Implementation notes:
%
% An object manager is not linked to any particular dataflow (as there may exist
% multiple dataflow instances).



% Constructs an object manager, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Name, an atom designating the classname of this singleton manager (ex:
% 'class_FooObjectManager')
%
% - ManagedObjectTypes is a (non-empty) list of the dataflow object types
% (classnames) that this object manager is to take care of (ex:
% ['class_Building','class_Dwelling'])
%
% - WorldManagerPid is the PID of the world manager
%
% - LoadBalancerPid, the PID of the load balancer, useful when instances have to
% be created
%
% - IdentificationServerPid, the PID of the identification server (if any)
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		classname()
			| { classname(), class_TraceEmitter:emitter_categorization() },
		[ dataflow_object_type() ], world_manager_pid(), load_balancer_pid(),
		maybe( identification_server_pid() ) ) -> wooper:state().
construct( _State, _ActorSettings, Name, _ManagedObjectTypes=[],
		   _WorldManagerPid, _LoadBalancerPid, _IdentificationServerPid ) ->
	throw( { no_object_type_specified_for_manager, Name } );

construct( State, ActorSettings, Name, ManagedObjectTypes, WorldManagerPid,
		   LoadBalancerPid, IdentificationServerPid ) ->

	% Auto-subscribing, and declaring our own types (based on a plain request):
	WorldManagerPid ! { registerObjectManager, [ ManagedObjectTypes ], self() },

	% We expect child classes to pass atom-based names (typically their own
	% module name), not strings:
	%
	{ _RegistrationName, TraceInit } = case Name of

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

	?send_info_fmt( ActorState, "Managing from now the objects of classes ~p.",
					[ ManagedObjectTypes ] ),

	% All object managers used to register themselves that way (ensured
	% uniqueness as well), yet now for load-balancing purposes we might have
	% multiple instances thereof:
	%
	%naming_utils:register_as( RegistrationName, global_only ),

	PreparedObjectTable = prepare_for_objects( ManagedObjectTypes, ActorState ),

	% Then the class-specific actions:
	FinalState = setAttributes( ActorState, [
		{ object_table, PreparedObjectTable },
		{ world_manager_pid, WorldManagerPid },
		{ triggered_events, [] },
		{ completed_event_infos, [] },
		{ injected_events, [] },
		{ load_balancer_pid, LoadBalancerPid },
		{ identification_server_pid, IdentificationServerPid } ] ),

	% Interleaving of registerObjectManager/2 is over:
	receive

		{ wooper_result, object_manager_registered } ->
			ok

	end,

	FinalState.



% Prepares the management of the specified types of objects.
%
% (helper)
%
-spec prepare_for_objects( [ dataflow_object_type() ], wooper:state() ) ->
									object_table().
prepare_for_objects( ObjectTypes, State ) ->

	case class_DataflowBlock:declare_static_information_for( ObjectTypes )
			of

		ok ->
			?notice_fmt( "All semantics and types for objects ~p successfully "
						 "declared statically.", [ ObjectTypes ] );

		{ error, Reason } ->
			?error_fmt( "Static declaration of semantics and types failed "
						"for objects ~p: ~p", [ ObjectTypes, Reason ] ),
			throw( { static_declaration_failed, ObjectTypes, Reason } )

	end,

	% Initially all object types know none of their instances:
	EmptyEntries = [ { Type, [] } || Type <- ObjectTypes ],

	table:new( EmptyEntries ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?info( "Being deleted." ),

	State.




% Methods section.


% Callback executed on the first diasca of existence of this object manager.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							const_actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?void_fmt( "Created ~ts.", [ to_string( State ) ] ),

	actor:const_return().




% Creates, synchronously and while the simulation is not running, an (initial)
% instance of the specified object type, associated to specified dataflow, using
% specified core construction parameters for that, and returning the
% corresponding instance PID.
%
-spec createInitialObjectInstance( wooper:state(), dataflow_object_type(),
		dataflow_pid(), construction_parameters() ) ->
										request_return( object_pid() ).
createInitialObjectInstance( State, ObjectType, DataflowPid,
							 CoreConstructionParameters ) ->

	?void_fmt( "Creating an initial instance of object type '~ts', associated "
		"to dataflow ~w, and based on following core construction "
		"parameters:~n  ~p",
		[ ObjectType, DataflowPid, CoreConstructionParameters ] ),

	% Building the full construction parameters for the new object:

	FullConstructParams =
		list_utils:append_at_end( DataflowPid, CoreConstructionParameters ),

	LoadBalancerPid = ?getAttr(load_balancer_pid),

	ObjectPid = class_Actor:create_initial_actor( ObjectType,
							  FullConstructParams, LoadBalancerPid ),

	% Will register itself to its dataflow at the first diasca of this object.

	% May create a new entry for this object type:
	NewObjectTable = table:append_to_entry( _K=ObjectType, ObjectPid,
											?getAttr(object_table) ),

	NewState = setAttribute( State, object_table, NewObjectTable ),

	wooper:return_state_result( NewState, ObjectPid ).



% Creates, synchronously and while the simulation is not running, a set of
% (initial) instances of the specified object type, using the specified list of
% core construction parameters for that, and returning the corresponding
% instance PIDs, in the same order.
%
-spec createInitialObjectInstances( wooper:state(), dataflow_object_type(),
		dataflow_pid(), [ construction_parameters() ] ) ->
										request_return( [ object_pid() ] ).
createInitialObjectInstances( State, ObjectType, DataflowPid,
							  CoreConstructParamList ) ->

	ParamStrings = [ text_utils:format( "~p", [ CP ] )
					 || CP <- CoreConstructParamList ],

	?void_fmt( "Creating ~B initial instances of object type '~ts', associated "
		"to dataflow ~w, based on following list of core construction "
		"parameters: ~ts",
		[ length( CoreConstructParamList ), ObjectType, DataflowPid,
		  text_utils:strings_to_string( ParamStrings ) ] ),

	% Prepares a list of { Classname, FullConstructParams }:
	ConstructEntries = [ { ObjectType,
						   list_utils:append_at_end( DataflowPid, CP ) }
						 || CP <- CoreConstructParamList ],

	LoadBalancerPid = ?getAttr(load_balancer_pid),

	ObjectPidList = class_Actor:create_initial_actors( ConstructEntries,
													   LoadBalancerPid ),

	ObjectTable = ?getAttr(object_table),

	ObjectList = table:get_value( _K=ObjectType, ObjectTable ),

	NewObjectTable = table:add_entry( ObjectType, ObjectPidList ++ ObjectList,
									  ObjectTable ),

	NewState = setAttribute( State, object_table, NewObjectTable ),

	wooper:return_state_result( NewState, ObjectPidList ).



% Applies the specified changeset (a list of world events), expected to be
% emitted by the object manager.
%
% (actor oneway, that can possibly be overridden)
%
-spec applyChangeset( wooper:state(), changeset(), actor_pid() ) ->
							actor_oneway_return().
applyChangeset( State, Changeset, SendingActorPid ) ->

	check_no_pending_triggered_event( State ),

	%?void_fmt( "Applying (generically) ~ts (received from ~w)",
	% [ dataflow_support:changeset_to_string( Changeset ), SendingActorPid ] ),

	?void_fmt( "Applying (generically) a changeset of ~B elements "
		"(received from ~w)", [ length( Changeset ), SendingActorPid ] ),

	ObjectTable = ?getAttr(object_table),

	ManagedTypes = table:keys( ObjectTable ),

	% This changeset will be processed, and recorded in triggered_events:
	AppliedState = apply_world_events( Changeset, ManagedTypes, State ),

	actor:return_state( AppliedState ).



% Ensures that there is no unacknowledged, past triggered event.
%
% (helper)
%
check_no_pending_triggered_event( State ) ->

	CurrentTick = class_Actor:get_current_tick_offset( State ),

	TriggeredEvents = ?getAttr(triggered_events),

	[ check_triggered( Event, CurrentTick ) || Event <- TriggeredEvents ].


% (helper)
check_triggered( Event, CurrentTick ) ->

	% By design the timestamp is stored in the second field of all event types:
	case erlang:element( _N=3, Event ) of

		undefined ->
			throw( { triggered_event_not_timestamped, Event } );

		{ CurrentTick, _ } ->
			ok;

		{ EventTimestamp, _ } when EventTimestamp > CurrentTick ->
			throw( { future_triggered_event, EventTimestamp, CurrentTick,
					 Event } );

		% Thus in the past:
		{ EventTimestamp, _ } ->
			throw( { unacknowledged_triggered_event, EventTimestamp,
					 CurrentTick, Event } )

	end.



% Applies in turn the specified list of world events (which is a changeset).
% Only events that can be generically managed are expected to be found there;
% the other events are expected to have been filtered out and processed by a
% dedicated object manager (inheriting from this class).
%
% Note: induced events (if any) originally registered in an event will be
% triggered when the callback associated to this parent event will be executed
% (ex: onActorCreated/4, for a creation event, will trigger in turn its
% top-level induced events); as a result they are not handled there.
%
% (exported helper)
%
-spec apply_world_events( [ world_event() ], [ dataflow_object_type() ],
						  wooper:state() ) -> wooper:state().
apply_world_events( _WorldEvents=[], _ManagedTypes, State ) ->
	% Nothing more to do, just waiting for the callbacks to be triggered:
	State;


% Creations events can be managed by default, as model-agnostic generic rules
% can be defined:
%
apply_world_events( _WorldEvents=[ CreationEvent=#creation_event{
	  object_type=ObjectType,
	  external_id=ExternalID,
	  object_pid=undefined,
	  construction_parameters=ConstructParams,
	  dataflow_pid=DataflowPid } | T ], ManagedTypes, State ) ->

	% We do not have to specifically suspend just created dataflow objects, as
	% they start as such.

	% The external identifier is conventionally the name of the created actor:
	ActorName = case ExternalID of

		undefined ->

			?error_fmt( "Received a creation event with no external identifier:"
				" ~ts.",
				[ dataflow_support:world_event_to_string( CreationEvent ) ] ),

			% We might use
			% class_IdentificationServer:forge_external_identifier/1, yet we
			% have no new PID yet.
			%
			throw( { creation_with_no_external_id, CreationEvent } );

		_ ->
			ExternalID

	end,

	case lists:member( ObjectType, ManagedTypes ) of

		true ->

			ActualConstructParams = [ ActorName, ConstructParams, DataflowPid ],

			?void_fmt( "Object manager ~p creating a '~ts' instance from "
				"following construction parameters:~n  ~p",
				[ self(), ObjectType, ActualConstructParams ] ),

			%?void_fmt( "Object manager ~p creating a '~ts' instance.",
			%			[ self(), ObjectType ] ),

			% Will trigger back onActorCreated/4:
			CreationState = class_Actor:create_actor( ObjectType,
												ActualConstructParams, State ),

			TrigState = appendToAttribute( CreationState, triggered_events,
										   CreationEvent ),

			apply_world_events( T, ManagedTypes, TrigState );

		false ->
			throw( { unsupported_object_type, ObjectType, ManagedTypes } )

	end;



% Destruction events can also be generically managed, yet for that the PID of
% the target dataflow object must be known:
%
apply_world_events( _WorldEvents=[ DestructionEvent=#destruction_event{
	  external_id=ExternalID,
	  object_pid=undefined } | T ], ManagedTypes, State ) ->

	% Here we have a destruction event, yet not the corresponding PID, that must
	% be obtained first (object expected to be already existing; direct message
	% used as this is an access to an immutable value):
	%
	?getAttr(identification_server_pid) !
		{ getBlockPID, [ ExternalID ], self() },

	receive

		{ wooper_result, ObjectPid } when is_pid( ObjectPid ) ->

			% Reinjecting a now complete event for next clause:
			DestructiondEvents = [ DestructionEvent#destruction_event{
									object_pid=ObjectPid } | T ],

			apply_world_events( DestructiondEvents, ManagedTypes, State )

	end;



% Sufficiently complete destruction event, processing it for good:
apply_world_events( _WorldEvents=[ DestructionEvent=#destruction_event{
	  object_type=ObjectType,
	  id=EventId,
	  %object_type=ObjectType,
	  %external_id=ExternalID,
	  object_pid=ObjectPid } | T ], ManagedTypes, State ) ->

	case lists:member( ObjectType, ManagedTypes ) of

		true ->
			ok;

		false ->
			throw( { unsupported_object_type, ObjectType, ManagedTypes } )

	end,

	?info_fmt( "Requesting the destruction of dataflow object ~p.",
			   [ ObjectPid ] ),

	% Will result into a onAttributeDestructionPerformed/3 callback:
	SentState = class_Actor:send_actor_message( ObjectPid,
					% Not to collide with built-in destruct/1:
					{ triggerDestruction, [ EventId ] }, State ),

	TrigState = appendToAttribute( SentState, triggered_events,
								   DestructionEvent ),

	apply_world_events( T, ManagedTypes, TrigState );



% Update events can also be generically managed, yet for that the PID of the
% target dataflow object must be known:
%
apply_world_events( _WorldEvents=[ UpdateEvent=#update_event{
	  external_id=ExternalID,
	  object_pid=undefined } | T ], ManagedTypes, State ) ->

	% Here we have an update event, yet not the corresponding PID, that must be
	% obtained first (object expected to be already existing; direct message
	% used as it is an access to an immutable value):
	%
	?getAttr(identification_server_pid) !
		{ getBlockPID, [ ExternalID ], self() },

	receive

		{ wooper_result, ObjectPid } when is_pid( ObjectPid ) ->

			% Reinjecting a now complete event for next clause:
			UpdatedEvents = [ UpdateEvent#update_event{
									object_pid=ObjectPid } | T ],

			apply_world_events( UpdatedEvents, ManagedTypes, State )

	end;


% Sufficiently complete update event, processing it for good:
apply_world_events( _WorldEvents=[ UpdateEvent=#update_event{
	  object_type=ObjectType,
	  id=EventId,
	  %object_type=ObjectType,
	  %external_id=ExternalID,
	  object_pid=ObjectPid,
	  updates=Updates } | T ], ManagedTypes, State ) ->

	case lists:member( ObjectType, ManagedTypes ) of

		true ->
			ok;

		false ->
			throw( { unsupported_object_type, ObjectType, ManagedTypes } )

	end,

	% Remove empty events from the updates:
	UpdatesFiltered = [ Update || Update <- Updates, Update =/= {} ],

	?debug_fmt( "Managing an update event of a '~p' with ~p ",
				[ ObjectType, Updates ] ),

	BinUpdates = [ { text_utils:ensure_binary( AttrName ), AttrValue }
				   || { AttrName, AttrValue } <- UpdatesFiltered ],

	% We could have skipped an explicit acknowledgement notification (thanks to
	% the onAttributeUpdatePerformed/3 callback), as we know that the update
	% operation is to be applied at next diasca, yet this would have made
	% updates a special case and we would have had to take care immediately of
	% the update-induced events.
	%
	% Due to message reordering, this could have led an induced event to be
	% processed before its parent one, which of course is incorrect.

	?void_fmt( "Requesting dataflow object ~p to perform following "
			   "updates: ~p", [ ObjectPid, UpdatesFiltered ] ),

	% Will result into a onAttributeUpdatePerformed/3 callback:
	SentState = class_Actor:send_actor_message( ObjectPid,
					{ updateAttributes, [ BinUpdates, EventId ] }, State ),

	TrigState = appendToAttribute( SentState, triggered_events,
								   UpdateEvent ),

	apply_world_events( T, ManagedTypes, TrigState );



% Manage associations with the default procedure regarding unique or multiple
% peer associations between dataflow objects:
%
apply_world_events(_WorldEvents=[ BinAssocEvent=#binary_association_event{
		% Extraneous checkings performed:
		id=EventId,
		association_type=peer_type_association,
		source_object_type=SourceType,
		target_object_type=TargetType,
		source_external_id=SourceExternalId,
		target_external_id=TargetExternalId,
		source_object_pid=undefined,
		target_object_pid=undefined } | T ], ManagedTypes, State ) ->

	case lists:member( SourceType, ManagedTypes ) andalso
		 lists:member( TargetType, ManagedTypes ) of

		true ->

			% This manager is in charge of this source object type, so let's
			% register that event:

			?void_fmt( "Object manager ~p taking in charge the association of "
				"the ~ts instance named '~ts' to the ~ts named '~ts'.",
				[ self(), SourceType, SourceExternalId, TargetType,
				  TargetExternalId ] ),


			% Get the PID of these two objects:
			[ SourcePid, TargetPid ] =
				class_DataflowObjectManager:get_object_pids(
				  [ SourceExternalId, TargetExternalId ], State ),

			% Requests to both of these objects to declare these peers:
			SourceSentState = class_Actor:send_actor_message( SourcePid,
				{ registerPeerAs, [ TargetPid, TargetType ] }, State ),

			TargetSentState = class_Actor:send_actor_message( TargetPid,
				{ registerPeerAs, [ SourcePid, SourceType ] },
				SourceSentState ),

			% Hence handled next diasca, as wanted; probably better than having
			% the source or the target report all these information by itself:
			%
			SelfSentState = class_Actor:send_actor_message( self(),
				{ onBinaryAssociationEstablished, [ EventId ] },
				  TargetSentState ),

			UpdatedBinAssocEvent = BinAssocEvent#binary_association_event{
				source_object_pid=SourcePid, target_object_pid=TargetPid },

			% To prepare upcoming completion:
			RegisterState = appendToAttribute( SelfSentState, triggered_events,
											   UpdatedBinAssocEvent ),

			apply_world_events( T, ManagedTypes, RegisterState );


		false ->
			throw( { unsupported_object_type, SourceType, TargetType,
					 ManagedTypes } )

	end;


apply_world_events( _WorldEvents=[ Event | _T ], _ManagedTypes, State ) ->

	?error_fmt( "Error, world event '~p' not handled by the generic object "
				"manager.", [ Event ] ),

	throw( { event_not_handled, Event } ).




% Called automatically after (generally after two diascas) this manager created
% a requested object instance.
%
% Parameters are:
%
% - CreatedActorPid the PID of the just created object
%
% - CreatedActorTag the tag used for this actor creation so that it is able to
% discriminate among the multiple creations it might have requested; this is {
% ActorClassname, ActorConstructionParameters }, i.e. a pair made of the
% classname of that created actor and of the parameters that were specified for
% its creation
%
-spec onActorCreated( wooper:state(), object_pid(), class_Actor:tag(),
					  load_balancer_pid() ) -> actor_oneway_return().
onActorCreated( State, CreatedObjectPid,
	  CreatedActorTag={ ObjectType, [ ActorName | OtherConstructParameters ] },
	 _LoadBalancerPid ) ->

	?void_fmt( "Recording the completion of the creation of the dataflow object"
		" instance named '~ts' (PID: ~w) of type '~ts', created at "
		"runtime from ~B core construction parameters.",
		[ ActorName, CreatedObjectPid, ObjectType,
		  length( OtherConstructParameters ) ] ),

	% Performing creation-specific housekeeping:
	case ?getAttr(identification_server_pid) of

		undefined ->
			ok;

		IdPid ->

			BinActorName = text_utils:ensure_binary( ActorName ),

			wooper:execute_request( IdPid, declareIdentifierAssociation,
				[ CreatedObjectPid, BinActorName ],
				_ExpectedRes=identifier_association_declared )

	end,

	% May create a new entry for this object type:
	NewObjectTable = table:append_to_entry( _K=ObjectType, CreatedObjectPid,
											?getAttr(object_table) ),

	% We have now to remove the corresponding creation event from the triggered
	% (waited) ones:

	TriggeredEvents = ?getAttr(triggered_events),

	% We do not have here the event id to rely on:
	{ CreationEvent, OtherTriggeredEvents } =
		find_creation_event( CreatedActorTag, TriggeredEvents ),

	% Rather than reporting each completed event as it occurs, we gather them
	% (simpler, less messages involved).

	CreationInfo = { CreationEvent#creation_event.id, CreatedObjectPid },

	CompletedEventInfos = [ CreationInfo | ?getAttr(completed_event_infos) ],

	PostState = manage_post_event( OtherTriggeredEvents, CompletedEventInfos,
								   State ),

	FinalState = setAttribute( PostState, object_table, NewObjectTable ),

	actor:return_state( FinalState ).



% (helper)
-spec find_creation_event( class_Actor:tag(), [ world_event() ] ) ->
					{ world_event(), [ world_event() ] }.
find_creation_event( _CreatedActorTag={ ObjectType, [ ObjectName | _ ] },
					 Events ) ->

	% We have to filter the specified events; we only want the (supposedly
	% unique) creation event that applies to the expected object type and
	% external identifier:
	%
	extract_creation_event( Events, ObjectType, ObjectName ).



% Extracts the specified creation event from specified events.
%
% (helper)
%
-spec extract_creation_event( [ world_event() ], dataflow_object_type(),
				external_id() ) -> { creation_event(), [ world_event() ] }.
extract_creation_event( Events, ObjectType, ObjectName ) ->
	extract_creation_event( Events, ObjectType, ObjectName, _Acc=[] ).


extract_creation_event( _Events=[], ObjectType, ObjectName, _Acc ) ->
	throw( { creation_event_not_found, ObjectType, ObjectName } );


extract_creation_event( _Events=[ E=#creation_event{ object_type=ObjectType,
													 external_id=ObjectName }
								  | T ], ObjectType, ObjectName, Acc ) ->

	% Event found, we nevertheless check that no other event matches:
	check_no_creation_matching( T, ObjectType, ObjectName ),

	% Preserving event order (preferable):
	{ E, lists:reverse( Acc ) ++ T };


extract_creation_event( _Events=[ E | T ], ObjectType, ObjectName, Acc ) ->
	% Non-matching event:
	extract_creation_event( T, ObjectType, ObjectName, [ E | Acc ] ).



% Checks that no creation event matches specified parameters.
%
% (helper)
%
check_no_creation_matching( _Events=[], _ObjectType, _ObjectName ) ->
	ok;

check_no_creation_matching( _Events=[ #creation_event{ object_type=ObjectType,
			  external_id=ObjectName } | _T ],  ObjectType, ObjectName ) ->
	throw( { unexpected_creation_match, ObjectType, ObjectName } );

check_no_creation_matching( _Events=[ _E | T ],  ObjectType, ObjectName ) ->
	check_no_creation_matching( T, ObjectType, ObjectName ).




% Called automatically after a requested object (non-binary) association is
% performed.
%
% Parameter is EventId, the identifier of the corresponding completed
% association event.
%
-spec onAssociationEstablished( wooper:state(), event_id(),
								sending_actor_pid() ) -> actor_oneway_return().
onAssociationEstablished( State, EventId, _SendingActorPid ) ->

	?void_fmt( "Recording the completion of the non-binary association "
			   "whose event identifier is ~p", [ EventId ] ),

	% No particular association-related housekeeping to perform.

	% We have now to remove the corresponding association event from the
	% triggered (waited) ones:

	TriggeredEvents = ?getAttr(triggered_events),

	{ BinAssocEvent, OtherTriggeredEvents } =
		dataflow_support:find_event_by_id( EventId, TriggeredEvents ),

	% Rather than reporting each completed event as it occurs, we accumulate
	% them (simpler, and less messages involved).

	% To complement the reference event, held by the world manager:
	BinAssocExtraInfo = {
	  BinAssocEvent#binary_association_event.source_object_pid,
	  BinAssocEvent#binary_association_event.target_object_pid },

	CompletionInfo = { EventId, BinAssocExtraInfo },

	CompletedEventInfos = [ CompletionInfo | ?getAttr(completed_event_infos) ],

	PostState = manage_post_event( OtherTriggeredEvents, CompletedEventInfos,
								   State ),

	actor:return_state( PostState ).



% Called automatically after a requested binary object association is
% performed.
%
% Parameter is EventId, the identifier of the corresponding completed binary
% association event.
%
-spec onBinaryAssociationEstablished( wooper:state(), event_id(),
			sending_actor_pid() ) -> actor_oneway_return().
onBinaryAssociationEstablished( State, EventId, _SendingActorPid ) ->

	?void_fmt( "Recording the completion of the binary association "
			   "whose event identifier is ~p", [ EventId ] ),

	% No particular association-related housekeeping to perform.

	% We have now to remove the corresponding event from the triggered (waited)
	% ones:

	TriggeredEvents = ?getAttr(triggered_events),

	{ BinAssocEvent, OtherTriggeredEvents } =
		dataflow_support:find_event_by_id( EventId, TriggeredEvents ),

	% Rather than reporting each completed event as it occurs, we accumulate
	% them (simpler, and less messages involved).

	% To complement the reference event, held by the world manager:
	BinAssocExtraInfo = {
	  BinAssocEvent#binary_association_event.source_object_pid,
	  BinAssocEvent#binary_association_event.target_object_pid },

	CompletionInfo = { EventId, BinAssocExtraInfo },

	CompletedEventInfos = [ CompletionInfo | ?getAttr(completed_event_infos) ],

	PostState = manage_post_event( OtherTriggeredEvents, CompletedEventInfos,
								   State ),

	actor:return_state( PostState ).



% Called automatically after a requested disassociation is performed.
%
% Parameter is EventId, the identifier of the corresponding completed
% disassociation event.
%
-spec onDisassociationPerformed( wooper:state(), event_id(),
		   sending_actor_pid() ) -> actor_oneway_return().
onDisassociationPerformed( State, EventId, _SendingActorPid ) ->

	?void_fmt( "Recording the completion of the disassociation whose "
			   "event identifier is ~p", [ EventId ] ),

	% No particular disassociation-related housekeeping to perform.

	% We have now to remove the corresponding event from the triggered (waited)
	% ones:

	TriggeredEvents = ?getAttr(triggered_events),

	{ DisassocEvent, OtherTriggeredEvents } =
		dataflow_support:find_event_by_id( EventId, TriggeredEvents ),

	% Rather than reporting each completed event as it occurs, we accumulate
	% them (simpler, and less messages involved).

	% To complement the reference event, held by the world manager:
	DisassocExtraInfo = {
	  DisassocEvent#disassociation_event.object_pid,
	  DisassocEvent#disassociation_event.disassociation_information },

	CompletionInfo = { EventId, DisassocExtraInfo },

	CompletedEventInfos = [ CompletionInfo | ?getAttr(completed_event_infos) ],

	PostState = manage_post_event( OtherTriggeredEvents, CompletedEventInfos,
								   State ),

	actor:return_state( PostState ).



% Called automatically after a request update of attributes of a dataflow object
% is performed.
%
% Parameter is EventId, the identifier of the corresponding completed attribute
% update event.
%
-spec onAttributeUpdatePerformed( wooper:state(), event_id(),
							sending_actor_pid() ) -> actor_oneway_return().
onAttributeUpdatePerformed( State, EventId, _SendingActorPid ) ->

	?void_fmt( "Recording the completion of the attribute update "
			   "whose event identifier is ~p", [ EventId ] ),

	% No particular update-related housekeeping to perform.

	% We have now to remove the corresponding event from the triggered (waited)
	% ones:

	TriggeredEvents = ?getAttr(triggered_events),

	{ #update_event{ object_pid=UpdatedObjectPid }, OtherTriggeredEvents } =
		dataflow_support:find_event_by_id( EventId, TriggeredEvents ),

	% Rather than reporting each completed event as it occurs, we accumulate
	% them (simpler, and less messages involved).

	% To complement the reference event, held by the world manager (same as
	% SendingActorPid):
	%
	UpdateExtraInfo = UpdatedObjectPid,

	CompletionInfo = { EventId, UpdateExtraInfo },

	CompletedEventInfos = [ CompletionInfo | ?getAttr(completed_event_infos) ],

	PostState = manage_post_event( OtherTriggeredEvents, CompletedEventInfos,
								   State ),

	actor:return_state( PostState ).




% Called automatically after a requested destruction of a dataflow object is
% performed (the sender being this destructed object).
%
% Parameter is EventId, the identifier of the corresponding completed
% destruction event.
%
% (actor oneway)
%
-spec onDestructionTriggered( wooper:state(), event_id(),
							sending_actor_pid() ) -> actor_oneway_return().
onDestructionTriggered( State, EventId, DestructedObjectPid ) ->

	?void_fmt( "Recording the completion of the dataflow object destruction "
			   "whose event identifier is ~p", [ EventId ] ),

	% Performing destruction-specific housekeeping:

	case ?getAttr(identification_server_pid) of

		undefined ->
			ok;

		IdPid ->
			wooper:execute_request( IdPid, removeIdentifierAssociation,
				[ DestructedObjectPid ],
				_ExpectedRes=identifier_associations_removed )

	end,

	% We have now to remove the corresponding event from the triggered (waited)
	% ones:

	TriggeredEvents = ?getAttr(triggered_events),

	{ #destruction_event{
		 object_type=DestructedObjectType,
		 object_pid=DestructedObjectPid },
	  OtherTriggeredEvents } =
		dataflow_support:find_event_by_id( EventId, TriggeredEvents ),

	% Let's remove that destructed object from internal table:

	ObjectTable = ?getAttr(object_table),

	SameTypeObjects = table:get_value( DestructedObjectType, ObjectTable ),

	ShrunkTypeObjects =
		list_utils:delete_existing( DestructedObjectPid, SameTypeObjects ),

	NewObjectTable = table:add_entry( DestructedObjectType, ShrunkTypeObjects,
									  ObjectTable ),

	% Rather than reporting each completed event as it occurs, we accumulate
	% them (simpler, and less messages involved).

	% To complement the reference event, held by the world manager (same as
	% SendingActorPid):
	%
	DestructionExtraInfo = DestructedObjectPid,

	CompletionInfo = { EventId, DestructionExtraInfo },

	CompletedEventInfos = [ CompletionInfo | ?getAttr(completed_event_infos) ],

	TableState = setAttribute( State, object_table, NewObjectTable ),

	PostState = manage_post_event( OtherTriggeredEvents, CompletedEventInfos,
								   TableState ),

	actor:return_state( PostState ).




% Manages the completion of an event, when it has been reported.
%
% (helper)
%
-spec manage_post_event( changeset(),
		[ { event_id(), class_WorldManager:completion_extra_info() } ],
		wooper:state() ) -> wooper:state().
manage_post_event( _RemainingTriggeredEvents=[], CompletedEventInfos, State ) ->

	InjectedEvents = ?getAttr(injected_events),

	% All known events completed, reporting completed ones then:
	?void_fmt( "Reporting ~B completed events (identifiers: ~w) "
		"to the world manager, and injecting following changeset: ~ts",
		[ length( CompletedEventInfos ),
		  [ Id || { Id, _ExtraInfo } <- CompletedEventInfos ],
		  dataflow_support:changeset_to_string( InjectedEvents ) ] ),

	SentState = class_Actor:send_actor_message( ?getAttr(world_manager_pid),
		  { reportChangesetCompletion,
			[ CompletedEventInfos, InjectedEvents ] }, State ),

	% Reset for next tick:
	setAttributes( SentState, [ { triggered_events, [] },
								{ completed_event_infos, [] },
								{ injected_events, [] } ] );

manage_post_event( RemainingTriggeredEvents, CompletedEventInfos, State ) ->
	% Still triggered events to wait:

	?void_fmt( "Still ~B triggered events waited: ~ts",
			   [ length( RemainingTriggeredEvents ),
				 text_utils:strings_to_string( [
		   dataflow_support:world_event_to_string( E )
							  || E <- RemainingTriggeredEvents ] ) ] ),

	setAttributes( State, [ { triggered_events, RemainingTriggeredEvents },
							{ completed_event_infos, CompletedEventInfos } ] ).



% Helper functions.


% Returns a textual description of the object instances currently managed.
-spec object_table_to_string( wooper:state() ) -> string().
object_table_to_string( State ) ->

	case table:enumerate( ?getAttr(object_table) ) of

		[] ->
			 "not managing any object type";

		Types ->

			StringEntries = [ case IList of

				[] ->
					text_utils:format( "no instance managed for object "
									   "type '~ts'", [ BName ] );

				_ ->
					text_utils:format( "~B instance(s) managed for object "
						"type '~ts': ~w",
						[ length( IList ), BName, IList ] )

							  end || { BName, IList } <- Types ],

			text_utils:format( "managing ~B object types: ",
							   [ length( Types ) ] )
				++ text_utils:strings_to_string( StringEntries )

	end.



% Returns a textual description of this object manager.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	IdString = case ?getAttr(identification_server_pid) of

		undefined ->
			"no identification server";

		IdPid ->
			text_utils:format( "identification server ~w", [ IdPid ] )

	end,

	EventString = case ?getAttr(triggered_events) of

		[] ->
			"not waiting for the completion of any world event";

		Events ->
			ListString = text_utils:strings_to_string(
			  [ dataflow_support:world_event_to_string( E ) || E <- Events ] ),

			text_utils:format( "waiting for the completion of ~B world "
							   "events: ~ts", [ length( Events ), ListString ] )

	end,

	ObjectTypeString = object_table_to_string( State ),

	text_utils:format( "Object manager knowing ~ts, ~ts and ~ts",
					   [ IdString, EventString, ObjectTypeString ] ).




% Static section.


% Creates (initially, i.e. before the simulation is started) the specified
% default (i.e. not specifically defined in a class of their own) object
% managers, from their respective name and a list of the types of dataflow
% objects they are each to manage, with no identification server specified.
%
% Returns the list of their PIDs in the same order as the one of their names.
%
-spec create_default_managers( [ object_manager_def() ], world_manager_pid(),
			load_balancer_pid() ) -> static_return( [ object_manager_pid() ] ).
create_default_managers( ObjectManagerDefs, WorldManagerPid,
						 LoadBalancerPid ) ->

	Managers = create_default_managers( ObjectManagerDefs, WorldManagerPid,
			 LoadBalancerPid, _IdentificationServerPid=undefined ),

	wooper:return_static( Managers ).



% Creates (initially, i.e. before the simulation is started) multiple default
% (i.e. not specifically defined in a class of their own) object managers, based
% on their respective names and managed object types, and also on the number of
% computing cores (thus on the user host).
%
% A default manager is supposed to be stateless.
%
% One of the several managers will be chosen randomly (yet consistently) to
% process a given event, thus providing a simple load balancing.
%
% Returns the list of their PIDs in the same order as the one of their names.
%
-spec create_default_managers( [ object_manager_def() ], world_manager_pid(),
			load_balancer_pid(), identification_server_pid() ) ->
									static_return( [ object_manager_pid() ] ).
create_default_managers( ObjectManagerDefs, WorldManagerPid, LoadBalancerPid,
						 IdentificationServerPid ) ->

	% The more available cores, the more redundant default managers
	% instantiated:
	%
	ManagerCount = math_utils:floor( system_utils:get_core_count() * 1.6 ),

	ManagerCreationSpecs = [
		{ class_DataflowObjectManager, [ ManagerName, ManagedObjectTypes,
										 WorldManagerPid, LoadBalancerPid,
										 IdentificationServerPid ] }
				 || { ManagerName, ManagedObjectTypes } <- ObjectManagerDefs,
					_Several <- lists:seq( 1, ManagerCount ) ],

	Managers = class_Actor:create_initial_actors( ManagerCreationSpecs,
												  LoadBalancerPid ),

	wooper:return_static( Managers ).



% Creates (initially, i.e. before the simulation is started) the specified
% specific (i.e. defined in a class of their own, as opposed to default ones,
% direct instances of this class) object managers, supposing here that they each
% accept exactly three construction parameters, i.e. the PID of the world
% manager, the one of the dataflow and the one of the load balancer. No
% identification server is specified here.
%
% No need to specify the types of dataflow objects that they manage, as this is
% directly defined in their respective implementations.
%
% Returns the list of their PIDs in the same order as the one of their names.
%
-spec create_specific_managers( [ classname() ], world_manager_pid(),
			load_balancer_pid() ) -> static_return( [ object_manager_pid() ] ).
create_specific_managers( ObjectManagerNames, WorldManagerPid,
						  LoadBalancerPid ) ->

	Managers = create_specific_managers( ObjectManagerNames, WorldManagerPid,
					LoadBalancerPid, _IdentificationServerPid=undefined ),

	wooper:return_static( Managers ).



% Creates (initially, i.e. before the simulation is started) the specified
% specific (i.e. defined in a class of their own, as opposed to default ones,
% direct instances of this class) object managers, supposing here that they each
% accept exactly three construction parameters, i.e. the PID of the world
% manager, the one of the dataflow and the one of the load balancer.
%
% No need to specify the types of dataflow objects that they manage, as this is
% defined in their respective implementations.
%
% Returns the list of their PIDs in the same order as the one of their names.
%
-spec create_specific_managers( [ classname() ], world_manager_pid(),
					   load_balancer_pid(), identification_server_pid()  ) ->
		static_return( [ object_manager_pid() ] ).
create_specific_managers( ObjectManagerNames, WorldManagerPid, LoadBalancerPid,
						  IdentificationServerPid ) ->

	ConstructionParameters = [ WorldManagerPid, LoadBalancerPid,
							   IdentificationServerPid ],

	% By convention the name of a object manager is its classname:
	Managers = [ class_Actor:create_initial_actor( Classname,
												   ConstructionParameters )
				 || Classname <- ObjectManagerNames ],

	wooper:return_static( Managers ).



% Requests the synchronous creation by specified object manager of an initial
% (i.e. not dynamic, at runtime) instance of specified object type, using
% specified core construction parameters for that, and returns the PID of the
% created object instance.
%
% Note: only the core, object-specific construction parameters shall be
% specified; the others (actor-specific ones, dataflow PID, etc.) will be added
% automatically.
%
% For example, for a call to class_Foobar:construct(State, ActorSettings, A, B,
% C, DataflowPid) to happen, only [A, B, C] shall be specified as core
% construction parameters.
%
% Defined for convenience, typically when implementing a simulation case.
%
-spec create_initial_object( object_manager_pid(), dataflow_object_type(),
   dataflow_pid(), construction_parameters() ) -> static_return( object_pid() ).
create_initial_object( ObjectManagerPid, ObjectClassname, DataflowPid,
					   CoreConstructionParameters ) ->

	ObjectManagerPid ! { createInitialObjectInstance, [ ObjectClassname,
						   DataflowPid, CoreConstructionParameters ], self() },

	receive

		{ wooper_result, ObjectPid } when is_pid( ObjectPid ) ->
			wooper:return_static( ObjectPid )

	end.



% Requests the synchronous creations by specified object manager of a set of
% initial (i.e. not dynamic, at runtime) instances of the specified object type,
% associated to specified dataflow, using specified list of core construction
% parameters for that, and returns the list of the PIDs of the created object
% instances, in the order of their construction parameters.
%
% Note: only the core, object-specific construction parameters shall be
% specified (actor-specific ones, dataflow PID, etc.) will be added
% automatically.
%
% For example, for a call to class_Foobar:construct(State, ActorSettings, A, B,
% C, DataflowPid) to happen, only [A, B, C] shall be specified as core
% construction parameters.
%
% Defined for convenience, typically when implementing a simulation case.
%
-spec create_initial_objects( object_manager_pid(), dataflow_object_type(),
			dataflow_pid(), [ construction_parameters() ] ) ->
									static_return( [ object_pid() ] ).
create_initial_objects( ObjectManagerPid, ObjectClassname, DataflowPid,
						CoreConstructionParamList ) ->

	ObjectManagerPid ! { createInitialObjectInstances,
		[ ObjectClassname, DataflowPid, CoreConstructionParamList ], self() },

	receive

		{ wooper_result, ObjectPidList } when is_list( ObjectPidList ) ->
			wooper:return_static( ObjectPidList )

	end.



% Creates, at runtime (i.e. in the course of the simulation), an object of
% specified type (classname), associated to specified dataflow, based on
% specified list of core construction parameters, and returns an updated state.
%
% To be called from an actor, typically from a specialised object manager.
%
% (exported helper)
%
-spec create_runtime_object( dataflow_object_type(), dataflow_pid(),
			[ construction_parameters() ], wooper:state() ) -> wooper:state().
create_runtime_object( ObjectType, DataflowPid, CoreConstructionParameters,
					   State ) ->

	?void_fmt( "Creating a runtime instance of object type '~ts', associated "
		"to dataflow ~w, based on following core construction parameters: ~p",
		[ ObjectType, DataflowPid, CoreConstructionParameters ] ),

	% Building the full construction parameters for the new object:

	FullConstructParams = list_utils:append_at_end( DataflowPid,
											   CoreConstructionParameters ),

	% Returns an updated state; the PID of the created actor will be recorded in
	% onActorCreated/4.
	%
	class_Actor:create_actor( ObjectType, FullConstructParams, State ).



% Returns the PID of the specified dataflow objects, as designated by their
% external identifier.
%
-spec get_object_pids( [ external_id() ], wooper:state() ) -> [ object_pid() ].
get_object_pids( ExternalIdentifiers, State ) ->

	% Internally, any external_id defined as a plain string is expected to have
	% already been converted into a binary.

	% Returns the corresponding list of PIDs, in the right order:
	wooper:execute_request( ?getAttr(identification_server_pid), getBlockPIDs,
							[ ExternalIdentifiers ] ).
