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


-module(class_ExperimentManager).


-define( class_description,
		 "The experiment manager (EM) is a singleton instance in charge of "
		 "driving the computations that shall be operated on the simulated "
		 "world, in a dataflow context." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).



% Design notes:
%
% The experiment manager is in charge of organising all the dataflow
% computations that are to happen, i.e. in practice of all the instances of
% dataflows and all dataflow units that have been defined.
%
% Currently each dataflow declares itself to the experiment manager, even if it
% is unsure whether it is useful.

% This singleton additionally interacts with its descriptive counterpart, the
% world manager.


% Note that initial creations (i.e. typically dataflow objects that are created
% from the simulation case before the simulation is started) do not rely on
% world events, and as such cannot trigger a unit manager.


% Helpers:
-export([ to_string/1 ]).


% Count of experiment steps:
-type step_count() :: basic_utils:count().


% The various phases at which an experiment can be:
-type phase() :: 'initialisation' | 'simulation' | 'termination'.


-export_type([ step_count/0, phase/0 ]).


% We define tables, where simple lists of { event_match(), unit_manager_pid() }
% pairs could have sufficed, as the overhead of factorising matches is low, and
% very generic matches may happen to be used more than once.


% To determine which unit manager(s) shall be triggered whenever any kind of
% synchronization event occurs:
%
-type any_match_table() :: [ unit_manager_pid() ].


% To determine, for a given creation event match, which unit manager(s) shall be
% triggered:
%
-type creation_match_table() ::
		table( creation_event_match(), [ unit_manager_pid() ] ).


% To determine, for a given destruction event match, which unit manager(s) shall
% be triggered:
%
-type destruction_match_table() ::
		table( destruction_event_match(), [ unit_manager_pid() ] ).


% To determine, for a given (non-binary) association event match, which unit
% manager(s) shall be triggered:
%
-type association_match_table() ::
		table( association_event_match(), [ unit_manager_pid() ] ).


% To determine, for a given binary association event match, which unit
% manager(s) shall be triggered:
%
-type binary_association_match_table() ::
		table( binary_association_event_match(), [ unit_manager_pid() ] ).


% To determine, for a given disassociation event match, which unit manager(s)
% shall be triggered:
%
-type disassociation_match_table() ::
		table( disassociation_event_match(), [ unit_manager_pid() ] ).


% To determine, for a given connection event match, which unit manager(s) shall
% be triggered:
%
-type connection_match_table() ::
		table( connection_event_match(), [ unit_manager_pid() ] ).


% To determine, for a given disconnection event match, which unit manager(s)
% shall be triggered:
%
-type disconnection_match_table() ::
		table( disconnection_event_match(), [ unit_manager_pid() ] ).


% To determine, for a given update event match, which unit manager(s) shall be
% triggered:
%
-type update_match_table() ::
		table( update_event_match(), [ unit_manager_pid() ] ).



% To hold and update all match tables at once (defined for convenience):
-record( match_tables, {
		   any_match_table                :: any_match_table(),
		   creation_match_table           :: creation_match_table(),
		   destruction_match_table        :: destruction_match_table(),
		   association_match_table        :: association_match_table(),
		   binary_association_match_table :: binary_association_match_table(),
		   disassociation_match_table     :: disassociation_match_table(),
		   connection_match_table         :: connection_match_table(),
		   disconnection_match_table      :: disconnection_match_table(),
		   update_match_table             :: update_match_table() } ).


-type match_tables() :: #match_tables{}.



% Silencing:
-export_type([ any_match_table/0,
			   creation_match_table/0, destruction_match_table/0,
			   association_match_table/0, binary_association_match_table/0,
			   disassociation_match_table/0,
			   connection_match_table/0, disconnection_match_table/0,
			   update_match_table/0 ]).


% Waited acknowledgement table:
-type ack_table() :: table( unit_manager_pid(), [ event_id() ] ).


% The attributes that are specific to an experiment manager are:
-define( class_attributes, [

	{ unit_managers, table( unit_manager_pid(), [ dataflow_unit_type() ] ),
	  "a table whose keys are the PID of the known unit managers, and whose "
	  "associated values are a list of types of dataflow units that each "
	  "manages" },

	{ world_manager_pid, world_manager_pid(), "PID of the world manager" },

	{ dataflows, [ dataflow_pid() ], "a list of the dataflow instances known "
	  "of this experiment manager" },

	{ entry_point_pid, maybe( dataflow_entry_point_pid() ),
	  "the PID of the entry point (if any) for all dataflows managed by this "
	  "experiment manager" },

	{ exit_point_pid, maybe( dataflow_exit_point_pid() ),
	  "the PID of the exit point (if any) for all dataflows managed by this "
	  "experiment manager" },

	{ match_tables, match_tables(), "a record holding the various per-type of "
	  "events match tables in use, namely with the following fields:~n"
	  " - any_match_table :: any_match_table() is a list of the unit managers "
	  "that subscribed to any type of dataflow synchronization event~n"
	  " - creation_match_table :: creation_match_table() allows to determine, "
	  "for an event clause regarding the creation of a dataflow object that "
	  "matches, the corresponding list of unit managers that shall be "
	  "triggered~n"
	  " - destruction_match_table :: destruction_match_table() allows to "
	  "determine, for an event clause regarding the destruction of a dataflow "
	  "object that matches, the corresponding list of unit managers that shall "
	  "be triggered~n"
	  " - association_match_table :: association_match_table() allows to "
	  "determine, for an event clause regarding the (non-binary) association "
	  "of a dataflow object that matches, the corresponding list of unit "
	  "managers that shall be triggered~n"
	  " - binary_association_match_table :: binary_association_match_table() "
	  "allows to determine, for an event clause regarding the binary "
	  "association of a dataflow object that matches, the corresponding list "
	  "of unit managers that shall be triggered~n"
	  " - disassociation_match_table :: disassociation_match_table() allows to "
	  "determine, for an event clause regarding the disassociation of a "
	  "dataflow object that matches, the corresponding list of unit managers "
	  "that shall be triggered~n"
	  " - connection_match_table :: connection_match_table() allows to "
	  "determine, for an event clause regarding the connection of a dataflow "
	  "object that matches, the corresponding list of unit managers that shall "
	  "be triggered~n"
	  " - disconnection_match_table :: disconnection_match_table() allows to "
	  "determine, for an event clause regarding the disconnection of a "
	  "dataflow object that matches, the corresponding list of unit managers "
	  "that shall be triggered~n"
	  " - update_match_table :: update_match_table() allows to determine, for "
	  "an event clause regarding the update of a dataflow object that matches, "
	  "the corresponding list of unit managers that shall be triggered~n" },

	{ pending_events, [ world_event() ],
	  "the (ordered) list of the next events that will be dispatched once "
	  "the current one will be fully processed" },

	{ waited_event_acks, ack_table(), "a table associating to each unit "
	  "manager a list of the identifiers of the events that have been notified "
	  "to this manager and that have not been acknowledged yet" } ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Experiment.ExperimentManager" ).


% For world_manager_pid() and all:
-include("dataflow_defines.hrl").


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").



% Implementation notes:

% The experiment manager is the direct parent of all unit managers, yet is not
% one itself, as the root of this hierarchy has a sufficiently specific purpose
% and role.


% Event dispatching and processing.
%
% We now enforce a stricter processing of events, in which a given event must be
% fully processed until the next one is dispatched.
%
% Before, events were dispatched one after the other in the same diasca (and of
% course waited for in parallel), which allowed some level of interleaving (as
% they were processed in parallel).
%
% Yet this approach resulted in unspecified ordering, and could not support the
% (frequent) cases where the processing of a given event typically resulted in
% the connection of a unit created because of the previous event. Even in the
% most favorable reordering case, the creation of this unit would not be
% completed when the connection coming next would be attempted.
%
% Now we ensure that the current event is fully processed until the next one is
% dispatched.



% Synchronization event match tables.
%
% Note that a unit manager will be triggered with a given synchronization event
% as many times as this manager defined a clause that matches that event; the
% unit manager may record the ID of the processed events to handle each of them
% only once.


% Shorthand:

-type ustring() :: text_utils:ustring().



% Constructs the experiment manager, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - WorldManagerPid is the PID of the world manager that may interact with this
% experiment manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 world_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, WorldManagerPid ) ->

	% Auto-subscribing:
	WorldManagerPid ! { setExperimentManager, self() },

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
								?trace_categorize("ExperimentManager") ),

	naming_utils:register_as( ?experiment_manager_name, global_only ),

	MatchTables = init_match_tables(),

	EmptyTable = table:new(),

	% Then the class-specific actions:
	setAttributes( ActorState, [

		{ unit_managers, EmptyTable },
		{ world_manager_pid, WorldManagerPid },
		{ dataflows, [] },

		{ entry_point_pid, undefined },
		{ exit_point_pid, undefined },

		{ match_tables, MatchTables },
		{ pending_events, [] },
		{ waited_event_acks, EmptyTable } ] ).



% Initializes the match tables, stored in a single record for convenience.
-spec init_match_tables() -> match_tables().
init_match_tables() ->

	EmptyTable = table:new(),

	#match_tables{ any_match_table=[],
				   creation_match_table=EmptyTable,
				   destruction_match_table=EmptyTable,
				   association_match_table=EmptyTable,
				   binary_association_match_table=EmptyTable,
				   disassociation_match_table=EmptyTable,
				   connection_match_table=EmptyTable,
				   disconnection_match_table=EmptyTable,
				   update_match_table=EmptyTable }.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?info_fmt( "Being deleted (the ~B registered unit managers will be "
				"automatically be deleted as well).",
				[ table:size( ?getAttr(unit_managers) ) ] ),

	% No need to do that explicitly, as they are actors (hence will be removed
	% automatically at simulation end):
	%
	%[ UnitManagerPid ! delete
	%    || UnitManagerPid <- table:size( ?getAttr(unit_managers) ) ],
	%
	%setAttribute( State, unit_managers, table:new() ).

	State.



% Methods section.


% Registers (synchronously) specified dataflow.
-spec registerDataflow( wooper:state() ) ->
								request_return( 'dataflow_registered' ).
registerDataflow( State ) ->

	DataflowPid = ?getSender(),

	KnownDataflows = ?getAttr(dataflows),

	case lists:member( DataflowPid, KnownDataflows ) of

		true ->
			throw( { dataflow_already_registered, DataflowPid } );

		false ->
			?info_fmt( "Registered dataflow ~p.", [ DataflowPid ] ),

			NewState = setAttribute( State, dataflows,
									 [ DataflowPid | KnownDataflows ] ),

			wooper:return_state_result( NewState, dataflow_registered )

	end.



% Registers specified unit manager, in charge of specified types of units, and
% relying on the specified event match clauses.
%
% (synchronous request, to run prior to simulation start)
%
-spec registerUnitManager( wooper:state(), [ dataflow_unit_type() ],
			event_matches() ) -> request_return( 'unit_manager_registered' ).
registerUnitManager( State, ManagedUnitTypes, EventMatches ) ->

	% Federating all unit managers, storing all relevant information:

	UnitManagerPid = ?getSender(),

	?debug_fmt( "Registering unit manager ~w, in charge of following ~B "
		"types of units: ~w, and having declared ~ts.",
		[ UnitManagerPid, length( ManagedUnitTypes ),
		  ManagedUnitTypes,
		  class_DataflowUnitManager:event_clauses_to_string( EventMatches ) ] ),

	UnitManagers = ?getAttr(unit_managers),

	NewUnitManagers = case lists:member( UnitManagerPid,
										 table:keys( UnitManagers ) ) of

		true ->
			throw( { unit_unit_manager_already_registered, UnitManagerPid } );

		false ->
			table:add_entry( UnitManagerPid, ManagedUnitTypes, UnitManagers )

	end,

	% Starts with no event waited:
	NewAckTable = table:add_new_entry( UnitManagerPid, [],
									   ?getAttr(waited_event_acks) ),

	EventState = register_event_matches( EventMatches, UnitManagerPid, State ),

	NewState = setAttributes( EventState, [
						{ unit_managers, NewUnitManagers },
						{ waited_event_acks, NewAckTable } ] ),

	wooper:return_state_result( NewState, unit_manager_registered ).



% Registers the unit manager associations regarding specified unit types.
%
% Note that if 'any_event_type' is listed among the match clauses, it will not
% prevent this unit manager to be also triggered for any other listed clause.
%
-spec register_event_matches( [ event_match() ], unit_manager_pid(),
							  wooper:state() ) -> wooper:state().
register_event_matches( _EventMatches=[], _UnitManagerPid, State ) ->
	State;


register_event_matches( _EventMatches=[ any_event_type | T ], UnitManagerPid,
						State ) ->

	MatchTables = ?getAttr(match_tables),

	NewAnyTable = [ UnitManagerPid | MatchTables#match_tables.any_match_table ],

	NewState = setAttribute( State, match_tables,
					 MatchTables#match_tables{ any_match_table=NewAnyTable } ),

	register_event_matches( T, UnitManagerPid, NewState );


register_event_matches( _EventMatches=[ Clause | T ], UnitManagerPid, State )
  when is_record( Clause, creation_event_match ) ->

	MatchTables = ?getAttr(match_tables),

	NewCreationTable = table:append_to_entry( _K=Clause, UnitManagerPid,
							MatchTables#match_tables.creation_match_table ),

	NewState = setAttribute( State, match_tables,
		 MatchTables#match_tables{ creation_match_table=NewCreationTable } ),

	register_event_matches( T, UnitManagerPid, NewState );


register_event_matches( _EventMatches=[ Clause | T ], UnitManagerPid, State )
  when is_record( Clause, destruction_event_match ) ->

	MatchTables = ?getAttr(match_tables),

	NewDestructionTable = table:append_to_entry( _K=Clause, UnitManagerPid,
				MatchTables#match_tables.destruction_match_table ),

	NewState = setAttribute( State, match_tables,
	  MatchTables#match_tables{ destruction_match_table=NewDestructionTable } ),

	register_event_matches( T, UnitManagerPid, NewState );


register_event_matches( _EventMatches=[ Clause | T ], UnitManagerPid, State )
  when is_record( Clause, association_event_match ) ->

	MatchTables = ?getAttr(match_tables),

	NewAssocTable = table:append_to_entry( _K=Clause, UnitManagerPid,
						MatchTables#match_tables.association_match_table ),

	NewState = setAttribute( State, match_tables,
	  MatchTables#match_tables{ association_match_table=NewAssocTable } ),

	register_event_matches( T, UnitManagerPid, NewState );


register_event_matches( _EventMatches=[ Clause | T ], UnitManagerPid, State )
  when is_record( Clause, binary_association_event_match ) ->

	MatchTables = ?getAttr(match_tables),

	NewBinAssocTable = table:append_to_entry( _K=Clause, UnitManagerPid,
				MatchTables#match_tables.binary_association_match_table ),

	NewState = setAttribute( State, match_tables, MatchTables#match_tables{
						binary_association_match_table=NewBinAssocTable } ),

	register_event_matches( T, UnitManagerPid, NewState );


register_event_matches( _EventMatches=[ Clause | T ], UnitManagerPid, State )
  when is_record( Clause, disassociation_event_match ) ->

	MatchTables = ?getAttr(match_tables),

	NewDisassocTable = table:append_to_entry( _K=Clause, UnitManagerPid,
						MatchTables#match_tables.disassociation_match_table ),

	NewState = setAttribute( State, match_tables,
	  MatchTables#match_tables{ disassociation_match_table=NewDisassocTable } ),

	register_event_matches( T, UnitManagerPid, NewState );


register_event_matches( _EventMatches=[ Clause | T ], UnitManagerPid, State )
  when is_record( Clause, connection_event_match ) ->

	MatchTables = ?getAttr(match_tables),

	NewConnTable = table:append_to_entry( _K=Clause, UnitManagerPid,
						MatchTables#match_tables.connection_match_table ),

	NewState = setAttribute( State, match_tables,
	  MatchTables#match_tables{ connection_match_table=NewConnTable } ),

	register_event_matches( T, UnitManagerPid, NewState );


register_event_matches( _EventMatches=[ Clause | T ], UnitManagerPid, State )
  when is_record( Clause, disconnection_event_match ) ->

	MatchTables = ?getAttr(match_tables),

	NewDisconnTable = table:append_to_entry( _K=Clause, UnitManagerPid,
						MatchTables#match_tables.disconnection_match_table ),

	NewState = setAttribute( State, match_tables,
	  MatchTables#match_tables{ disconnection_match_table=NewDisconnTable } ),

	register_event_matches( T, UnitManagerPid, NewState );


register_event_matches( _EventMatches=[ Clause | T ], UnitManagerPid, State )
  when is_record( Clause, update_event_match ) ->

	MatchTables = ?getAttr(match_tables),

	NewUpdateTable = table:append_to_entry( _K=Clause, UnitManagerPid,
						MatchTables#match_tables.update_match_table ),

	NewState = setAttribute( State, match_tables,
				MatchTables#match_tables{ update_match_table=NewUpdateTable } ),

	register_event_matches( T, UnitManagerPid, NewState ).




% Registers (synchronously) specified (optional) experiment entry point.
-spec registerExperimentEntryPoint( wooper:state() ) ->
						request_return( 'experiment_entry_point_registered' ).
registerExperimentEntryPoint( State ) ->

	EntryPointPid = ?getSender(),

	% Check:
	undefined = ?getAttr(entry_point_pid),

	NewState = setAttribute( State, entry_point_pid, EntryPointPid ),

	wooper:return_state_result( NewState, experiment_entry_point_registered ).



% Registers (synchronously) specified (optional) experiment exit point.
-spec registerExperimentExitPoint( wooper:state() ) ->
						request_return( 'experiment_exit_point_registered' ).
registerExperimentExitPoint( State ) ->

	ExitPointPid = ?getSender(),

	% Check:
	undefined = ?getAttr(exit_point_pid),

	NewState = setAttribute( State, exit_point_pid, ExitPointPid ),

	wooper:return_state_result( NewState, experiment_exit_point_registered ).



% Callback executed on the first diasca of existence of this manager.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							const_actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?debug_fmt( "Created ~ts.", [ to_string( State ) ] ),

	actor:const_return().



% Notifies (most probably sent by the world manager) this experiment manager
% that a fully completed changeset has been applied on the state-of-the-world
% side, and thus is ready to be taken into account by unit managers, on the
% computation side - once the corresponding events will be dispatched
% adequately, by this actor oneway.
%
-spec notifyFullyCompletedChangeset( wooper:state(), changeset(),
							sending_actor_pid() ) -> actor_oneway_return().
notifyFullyCompletedChangeset( State, Changeset, _SendingActorPid ) ->

	% First, let's check that no previous timestep left unprocessed change
	% events:
	%
	check_no_past_event_waited( State ),

	% Then that we have no pending changeset:
	[] = ?getAttr(pending_events),

	% The changeset could be reordered by increasing event identifiers, to
	% further ensure that the causality order is preserved (this should already
	% be the case; only the order in each batch of events - i.e. the order of
	% the events having the same height in the various trees of the changeset -
	% may not be respected)

	?notice_fmt( "Dispatching following changeset to the known "
		"unit managers: ~ts",
		[ dataflow_support:changeset_to_string( Changeset ) ] ),

	% Updates the pending_events attribute:
	case dispatch_next_event( Changeset, State ) of

		{ DispatchedState, _IsWaiting=true } ->

			% Here at least one event matched (now: exactly one, other being
			% pending), we therefore wait for its completion. The dataflows will
			% be resumed when all waited event(s) (now, this single one) will be
			% reported as processed.
			%
			?debug_fmt( "Processing of this changeset initiated; "
				"new state: ~ts", [ to_string( DispatchedState ) ] ),

			actor:return_state( DispatchedState );


		{ DispatchedState, _IsWaiting=false } ->

			% Then we have to resume dataflows now:

			?notice( "Registered unit managers did not subscribe to any world "
				"event in this changeset, so resuming dataflows directly." ),

			actor:return_state( resume_dataflows( DispatchedState ) )

	end.



% Checks that no past change event is still recorded as being waited for.
-spec check_no_past_event_waited( wooper:state()  ) -> void().
check_no_past_event_waited( State ) ->

	AckPairs = table:enumerate( ?getAttr(waited_event_acks) ),

	case get_waited_events( AckPairs, _Acc=[] ) of

		[] ->
			ok;

		EventPairs ->

			?error_fmt( "~B unit managers still had unacknowledged events "
				"from a previous timestep: ~ts",[ length( EventPairs ),
					text_utils:strings_to_string( EventPairs ) ] ),

			WaitedEvents = lists:flatten( [ Events
							|| { _UnitManagerPid, Events } <- AckPairs ] ),

			throw( { unacknowledged_change_events, WaitedEvents } )

	end.



% Returns a list of strings describing the waited events stored in the specified
% {WaitingUnitManager, WaitedEventIdList} pairs.
%
-spec get_waited_events( [ { unit_manager_pid(), [ event_id() ] } ],
						 [ ustring() ] ) -> [ ustring() ].
get_waited_events( _AckPairs=[], Acc ) ->
	Acc;

get_waited_events( _AckPairs=[ { _UnitManagerPid, _WaitedEventIds=[] } | T ],
				   Acc ) ->
	get_waited_events( T, Acc );

get_waited_events( _AckPairs=[ { UnitManagerPid, WaitedEventIds } | T ],
				   Acc ) ->
	Str = text_utils:format( "unit manager ~w still waiting for ~B events, "
		"whose identifiers are: ~w",
		[ UnitManagerPid, length( WaitedEventIds ), WaitedEventIds ] ),
	get_waited_events( T, [ Str | Acc ] ).



% Callback triggered by a unit manager to notify the experiment manager that it
% processed specified event.
%
-spec onEventProcessed( wooper:state(), event_id(), unit_manager_pid() ) ->
								actor_oneway_return().
onEventProcessed( State, EventId, UnitManagerPid ) ->

	?void_fmt( "Event ID #~B reported as completed by unit manager ~w.",
			   [ EventId, UnitManagerPid ] ),

	AckTable = ?getAttr(waited_event_acks),

	WaitedEvents = table:get_value( _K=UnitManagerPid, AckTable ),

	% Only removes the first corresponding PID from the currently remaining
	% events:
	%
	RemainingEvents = list_utils:delete_existing( EventId, WaitedEvents ),

	NewAckTable = table:add_entry( UnitManagerPid, RemainingEvents, AckTable ),

	AckState = setAttribute( State, waited_event_acks, NewAckTable ),

	case all_current_events_processed( NewAckTable ) of

		true ->

			% Then here the (now single) current event processing is done,
			% looking for any next pending one:
			%
			% (updates the pending_event attribute)
			%
			case dispatch_next_event( ?getAttr(pending_events), AckState ) of


				{ DispatchedState, _IsWaiting=true } ->
					?void( "A new event has been dispatched, hence waiting "
						   "for the completion of its processing." ),

					actor:return_state( DispatchedState );


				{ DispatchedState, _IsWaiting=false } ->

					?notice( "All current events have been processed by their "
						"respective unit managers, and there is no more "
						"pending event, the processing of the current "
						"changeset is thus over. Considering that the "
						"dataflows reached a new stable state, resuming "
						"thus their evaluation now." ),

					ResumeState = resume_dataflows( DispatchedState ),

					actor:return_state( ResumeState )

			end;


		false ->

			?debug_fmt( "Still waiting for events to be processed by their "
				"respective unit managers: ~ts",
				[ ack_table_to_string( NewAckTable ) ] ),

			actor:return_state( AckState )

	end.




% Helper section.


% Resumes known dataflows, once the experiment manager did its job, i.e. when
% the calculation side of the dataflow is updated and stable as well.
%
-spec resume_dataflows( wooper:state() ) -> wooper:state().
resume_dataflows( State ) ->

	Dataflows = ?getAttr(dataflows),

	?debug_fmt( "Resuming now ~B dataflows: ~w.",
				[ length( Dataflows ), Dataflows ] ),

	class_Actor:send_actor_messages( Dataflows, resumeSuspendedBlocks, State ).



% Tells whether all pending events have been processed by their respective unit
% managers.
%
-spec all_current_events_processed( ack_table() ) -> boolean().
all_current_events_processed( AckTable ) ->

	EventLists = table:values( AckTable ),

	check_events_processed( EventLists ).


 % (helper)
check_events_processed( _EventLists=[] ) ->
	true;

check_events_processed( _EventLists=[ [] | T ] ) ->
	check_events_processed( T );

% Here, we must have encountered a non-empty list:
check_events_processed( _EventLists ) ->
	false.




% Dispatches next matching event (if any) from the specified changeset to the
% relevant unit managers, based on the match clause that they declared.
%
% Returns an updated state, and tells whether at least one event has been sent
% to a unit manager (and thus is waited).
%
-spec dispatch_next_event( changeset(), wooper:state() ) ->
								{ wooper:state(), boolean() }.
dispatch_next_event( Changeset, State ) ->
	MatchTables = ?getAttr(match_tables),
	dispatch_next_event( Changeset, MatchTables, State ).



% Drops all unmatching events, triggers the first matching one (if any), stores
% the remaining ones.
%
-spec dispatch_next_event( changeset(), match_tables(), wooper:state() ) ->
									{ wooper:state(), boolean() }.
dispatch_next_event( _Changeset=[], _MatchTables, State ) ->
	% Here, no event matched:
	PendingState = setAttribute( State, pending_events, [] ),
	{ PendingState, _IsWaiting=false };


dispatch_next_event( _Changeset=[ CreationEvent=#creation_event{} | T ],
					 MatchTables, State ) ->

	{ AnyEventState, AnyWaiting } = dispatch_any_event( CreationEvent,
							MatchTables#match_tables.any_match_table, State ),

	{ CreationState, CreationWaiting } = dispatch_creation_event( CreationEvent,
			 MatchTables#match_tables.creation_match_table, AnyEventState ),

	case AnyWaiting or CreationWaiting of

		true ->
			% Here this creation event has been dispatched to at least one unit
			% manager, we thus stop here and wait for the completion of its
			% processing to be reported, before dispatching the next matching
			% events (no interleaving in the processing of events allowed
			% anymore):
			%
			?void_fmt( "Dispatched ~ts.",
				[ dataflow_support:world_event_to_string( CreationEvent ) ] ),

			DispatchedState = setAttribute( CreationState, pending_events, T ),
			{ DispatchedState, _IsWaiting=true };

		false ->
			?void_fmt( "Dropped ~ts.",
				[ dataflow_support:world_event_to_string( CreationEvent ) ] ),

			% Let's see if the rest of the changeset an event may match:
			dispatch_next_event( T, MatchTables, CreationState )

	end;


dispatch_next_event( _Changeset=[ DestructionEvent=#destruction_event{} | T ],
					 MatchTables, State ) ->

	{ AnyEventState, AnyWaiting } = dispatch_any_event( DestructionEvent,
							MatchTables#match_tables.any_match_table, State ),

	{ DestructionState, DestructionWaiting } = dispatch_destruction_event(
		DestructionEvent, MatchTables#match_tables.destruction_match_table,
		AnyEventState ),

	% See comments in clause above:
	%
	case AnyWaiting or DestructionWaiting of

		true ->
			?void_fmt( "Dispatched ~ts.",
				[ dataflow_support:world_event_to_string(
					DestructionEvent ) ] ),
			DispatchedState =
				setAttribute( DestructionState, pending_events, T ),
			{ DispatchedState, _IsWaiting=true };

		false ->
			?void_fmt( "Dropped ~ts.",
				[ dataflow_support:world_event_to_string(
					DestructionEvent ) ] ),
			dispatch_next_event( T, MatchTables, DestructionState )

	end;


dispatch_next_event( _Changeset=[ AssociationEvent=#association_event{} | T ],
					 MatchTables, State ) ->

	{ AnyEventState, AnyWaiting } = dispatch_any_event( AssociationEvent,
							MatchTables#match_tables.any_match_table, State ),

	{ AssociationState, AssocWaiting } = dispatch_association_event(
			AssociationEvent,
			MatchTables#match_tables.association_match_table, AnyEventState ),

	case AnyWaiting or AssocWaiting of

		true ->
			?void_fmt( "Dispatched ~ts.",
				[ dataflow_support:world_event_to_string(
					AssociationEvent ) ] ),
			DispatchedState =
				setAttribute( AssociationState, pending_events, T ),
			{ DispatchedState, _IsWaiting=true };

		false ->
			?void_fmt( "Dropped ~ts.",
				[ dataflow_support:world_event_to_string(
					AssociationEvent ) ] ),
			dispatch_next_event( T, MatchTables, AssociationState )

	end;


dispatch_next_event(
  _Changeset=[ BinAssociationEvent=#binary_association_event{} | T ],
  MatchTables, State ) ->

	{ AnyEventState, AnyWaiting } = dispatch_any_event( BinAssociationEvent,
							MatchTables#match_tables.any_match_table, State ),

	{ BinAssocState, AssocWaiting } = dispatch_binary_association_event(
			BinAssociationEvent,
			MatchTables#match_tables.binary_association_match_table,
			AnyEventState ),

	case AnyWaiting or AssocWaiting of

		true ->
			?void_fmt( "Dispatched ~ts.",
				[ dataflow_support:world_event_to_string(
					BinAssociationEvent ) ] ),
			DispatchedState =
				setAttribute( BinAssocState, pending_events, T ),
			{ DispatchedState, _IsWaiting=true };

		false ->
			?void_fmt( "Dropped ~ts.",
				[ dataflow_support:world_event_to_string(
					BinAssociationEvent ) ] ),
			dispatch_next_event( T, MatchTables, BinAssocState )

	end;


dispatch_next_event(
  _Changeset=[ DisassociationEvent=#disassociation_event{} | T ], MatchTables,
  State ) ->

	{ AnyEventState, AnyWaiting } = dispatch_any_event( DisassociationEvent,
							MatchTables#match_tables.any_match_table, State ),

	{ DisassociationState, DisassocWaiting } = dispatch_disassociation_event(
		  DisassociationEvent,
		  MatchTables#match_tables.disassociation_match_table, AnyEventState ),

	case AnyWaiting or DisassocWaiting of

		true ->
			?void_fmt( "Dispatched ~ts.",
				[ dataflow_support:world_event_to_string(
					DisassociationEvent ) ] ),
			DispatchedState =
				setAttribute( DisassociationState, pending_events, T ),
			{ DispatchedState, _IsWaiting=true };

		false ->
			?void_fmt( "Dropped ~ts.",
				[ dataflow_support:world_event_to_string(
					DisassociationEvent ) ] ),
			dispatch_next_event( T, MatchTables, DisassociationState )

	end;


dispatch_next_event( _Changeset=[ ConnectionEvent=#connection_event{} | T ],
					 MatchTables, State ) ->

	{ AnyEventState, AnyWaiting } = dispatch_any_event( ConnectionEvent,
							MatchTables#match_tables.any_match_table, State ),

	{ ConnectionState, ConnWaiting } = dispatch_connection_event(
			ConnectionEvent,
			MatchTables#match_tables.connection_match_table, AnyEventState ),

	case AnyWaiting or ConnWaiting of

		true ->
			?void_fmt( "Dispatched ~ts.",
				[ dataflow_support:world_event_to_string( ConnectionEvent ) ] ),
			DispatchedState =
				setAttribute( ConnectionState, pending_events, T ),
			{ DispatchedState, _IsWaiting=true };

		false ->
			?void_fmt( "Dropped ~ts.",
				[ dataflow_support:world_event_to_string( ConnectionEvent ) ] ),
			dispatch_next_event( T, MatchTables, ConnectionState )

	end;


dispatch_next_event(
  _Changeset=[ DisconnectionEvent=#disconnection_event{} | T ], MatchTables,
  State ) ->

	{ AnyEventState, AnyWaiting } = dispatch_any_event( DisconnectionEvent,
							MatchTables#match_tables.any_match_table, State ),

	{ DisconnectionState, DisconnWaiting } = dispatch_disconnection_event(
		 DisconnectionEvent,
		 MatchTables#match_tables.disconnection_match_table, AnyEventState ),

	case AnyWaiting or DisconnWaiting of

		true ->
			?void_fmt( "Dispatched ~ts.",
				[ dataflow_support:world_event_to_string(
					DisconnectionEvent ) ] ),
			DispatchedState =
				setAttribute( DisconnectionState, pending_events, T ),
			{ DispatchedState, _IsWaiting=true };

		false ->
			?void_fmt( "Dropped ~ts.", [ dataflow_support:world_event_to_string(
										   DisconnectionEvent ) ] ),
			dispatch_next_event( T, MatchTables, DisconnectionState )

	end;


dispatch_next_event( _Changeset=[ UpdateEvent=#update_event{} | T ],
					 MatchTables, State ) ->

	{ AnyEventState, AnyWaiting } = dispatch_any_event( UpdateEvent,
							MatchTables#match_tables.any_match_table, State ),

	{ UpdateState, UpWaiting } = dispatch_update_event( UpdateEvent,
		  MatchTables#match_tables.update_match_table, AnyEventState ),

	case AnyWaiting or UpWaiting of

		true ->
			?void_fmt( "Dispatched ~ts.",
				[ dataflow_support:world_event_to_string( UpdateEvent ) ] ),
			DispatchedState = setAttribute( UpdateState, pending_events, T ),
			{ DispatchedState, _IsWaiting=true };

		false ->
			?void_fmt( "Dropped ~ts.",
				[ dataflow_support:world_event_to_string( UpdateEvent ) ] ),
			dispatch_next_event( T, MatchTables, UpdateState )

	end.



% Dispatches specified event to the unit managers that declared an interest for
% all (any) events.
%
-spec dispatch_any_event( world_event(), any_match_table(), wooper:state() ) ->
								{ wooper:state(), boolean() }.
dispatch_any_event( Event, UnitManagers, State ) ->
	dispatch_any_event( Event, UnitManagers, State, _IsWaiting=false ).


dispatch_any_event( _Event, _UnitManagers=[], State, IsWaiting ) ->
	{ State, IsWaiting };

dispatch_any_event( Event, [ UnitManagerPid | T ], State, _IsWaiting ) ->

	SentState = class_Actor:send_actor_message( UnitManagerPid,
					{ processAnyEventMatched, [ Event ] }, State ),

	AckState = register_waited_ack( dataflow_support:get_event_id( Event ),
									UnitManagerPid, SentState ),

	dispatch_any_event( Event, T, AckState, true ).



% Dispatches specified creation event to the unit managers (if any) that
% declared an interest for (at least a subset) of the creation events.
%
-spec dispatch_creation_event( creation_event(), creation_match_table(),
					   wooper:state() ) -> { wooper:state(), boolean() }.
dispatch_creation_event( CreationEvent, CreationTable, State ) ->

	MatchPairs = table:enumerate( CreationTable ),

	?void_fmt( "Creation matches: ~p.", [ MatchPairs ] ),

	lists:foldl( fun( { Clause, Managers }, AccState ) ->
						 dispatch_creation_event( CreationEvent, Clause,
												  Managers, AccState )
				 end,
				 _InitalAcc={ State, _IsWaiting=false },
				 _List=MatchPairs ).




% Dispatches specified creation event to the unit managers (if any) that
% declared an interest for (at least a subset) of the creation events.
%
-spec dispatch_creation_event( creation_event(), creation_event_match(),
				[ unit_manager_pid() ], { wooper:state(), boolean() } ) ->
									{ wooper:state(), boolean() }.
dispatch_creation_event( CreationEvent=#creation_event{
							 id=Id,
							 object_type=ObjectType,
							 external_id=ExternalId,
							 % Non-matched: object_pid=ObjectPid,
							 construction_parameters=ConstructParams,
							 dataflow_pid=DataflowPid },
						 Match=#creation_event_match{
							 object_type_match=ObjectTypeMatch,
							 external_id_match=ExternalIdMatch,
							 % Non-matched: object_pid_match=ObjectPidMatch,
							 construction_parameters_match=ConstructParamsMatch,
							 dataflow_pid_match=DataflowPidMatch },
						 UnitManagers, Param={ State, IsWaiting } ) ->

	?void_fmt( "Confronting creation event ~p to match ~p.",
			   [ CreationEvent, Match ] ),

	case object_type_match( ObjectType, ObjectTypeMatch )
		   andalso external_id_match( ExternalId, ExternalIdMatch )
		   andalso construction_parameters_match( ConstructParams,
												  ConstructParamsMatch )
		   andalso dataflow_pid_match( DataflowPid, DataflowPidMatch ) of

		true ->
			{ lists:foldl( fun( UnitManagerPid, AccState ) ->
							SentState = class_Actor:send_actor_message(
								UnitManagerPid,
								{ processCreationEventMatched,
								  [ CreationEvent ] },
								AccState ),
							register_waited_ack( Id, UnitManagerPid, SentState )
						   end,
						   _InitialAcc=State,
						   _List=UnitManagers ),
			  _IsWaiting= IsWaiting or ( UnitManagers =/= [] ) };

		false ->
			%?debug_fmt( "Creation event not matched: ~ts.",
			%			[ dataflow_support:world_event_to_string(
			%				CreationEvent ) ] ),
			Param

	end.



% Dispatches specified destruction event to the unit managers (if any) that
% declared an interest for (at least a subset) of the destruction events.
%
-spec dispatch_destruction_event( destruction_event(),
		destruction_match_table(), wooper:state() ) ->
										{ wooper:state(), boolean() }.
dispatch_destruction_event( DestructionEvent, DestructionTable, State ) ->

	MatchPairs = table:enumerate( DestructionTable ),

	lists:foldl( fun( { Clause, Managers }, AccState ) ->
						 dispatch_destruction_event( DestructionEvent, Clause,
													 Managers, AccState )
				 end,
				 _InitalAcc={ State, _IsWaiting=false },
				 _List=MatchPairs ).



% Dispatches specified destruction event to the unit managers (if any) that
% declared an interest for (at least a subset) of the destruction events.
%
-spec dispatch_destruction_event( destruction_event(),
		destruction_event_match(), [ unit_manager_pid() ],
		{ wooper:state(), boolean() } ) -> { wooper:state(), boolean() }.
dispatch_destruction_event( DestructionEvent=#destruction_event{
							   id=Id,
							   object_type=ObjectType,
							   external_id=ExternalId,
							   % Non-matched: object_pid=ObjectPid,
							   dataflow_pid=DataflowPid },
							#destruction_event_match{
							   object_type_match=ObjectTypeMatch,
							   external_id_match=ExternalIdMatch,
							   % Non-matched: object_pid_match=ObjectPidMatch,
							   dataflow_pid_match=DataflowPidMatch },
							UnitManagers, Param={ State, IsWaiting } ) ->
	case object_type_match( ObjectType, ObjectTypeMatch )
		   andalso external_id_match( ExternalId, ExternalIdMatch )
		   andalso dataflow_pid_match( DataflowPid, DataflowPidMatch ) of

		true ->
			{ lists:foldl( fun( UnitManagerPid, AccState ) ->
							SentState = class_Actor:send_actor_message(
								UnitManagerPid,
								{ processDestructionEventMatched,
								  [ DestructionEvent ] },
								AccState ),
							register_waited_ack( Id, UnitManagerPid, SentState )

						   end,
						   _InitialAcc=State,
						   _List=UnitManagers ),
			  _IsWaiting= IsWaiting or ( UnitManagers =/= [] ) };

		false ->
			%?debug_fmt( "Destruction event not matched: ~ts.",
			%			[ dataflow_support:world_event_to_string(
			%				DestructionEvent ) ] ),
			Param


	end.



% Dispatches specified (non-binary) association event to the unit managers (if
% any) that declared an interest for (at least a subset) of the association
% events.
%
-spec dispatch_association_event( association_event(),
			association_match_table(), wooper:state() ) ->
										{ wooper:state(), boolean() }.
dispatch_association_event( AssociationEvent, AssociationTable, State ) ->

	MatchPairs = table:enumerate( AssociationTable ),

	lists:foldl( fun( { Clause, Managers }, AccState ) ->
						 dispatch_association_event( AssociationEvent, Clause,
													 Managers, AccState )
				 end,
				 _InitalAcc={ State, _IsWaiting=false },
				 _List=MatchPairs ).



% Dispatches specified association event to the unit managers (if any) that
% declared an interest for (at least a subset) of the association events.
%
-spec dispatch_association_event( association_event(),
		association_event_match(), [ unit_manager_pid() ],
			{ wooper:state(), boolean() } ) -> { wooper:state(), boolean() }.
dispatch_association_event( AssociationEvent=#association_event{
							  id=Id,
							  object_type=ObjectType,
							  external_id=ExternalId,
							  % Non-matched: object_pid=ObjectPid,
							  association_information=AssocInfos,
							  dataflow_pid=DataflowPid },
							#association_event_match{
							  object_type_match=ObjectTypeMatch,
							  external_id_match=ExternalIdMatch,
							  % Non-matched: object_pid_match=ObjectPidMatch,
							  association_info_match=AssocInfosMatch,
							  dataflow_pid_match=DataflowPidMatch },
							UnitManagers, Param={ State, IsWaiting } ) ->
	case object_type_match( ObjectType, ObjectTypeMatch )
		   andalso external_id_match( ExternalId, ExternalIdMatch )
		   andalso association_information_match( AssocInfos, AssocInfosMatch )
		   andalso dataflow_pid_match( DataflowPid, DataflowPidMatch ) of

		true ->
			{ lists:foldl( fun( UnitManagerPid, AccState ) ->
							SentState = class_Actor:send_actor_message(
								UnitManagerPid,
								{ processAssociationEventMatched,
								  [ AssociationEvent ] },
								AccState ),
							register_waited_ack( Id, UnitManagerPid, SentState )
						   end,
						   _InitialAcc=State,
						   _List=UnitManagers ),
			  _IsWaiting= IsWaiting or ( UnitManagers =/= [] ) };

		false ->
			%?debug_fmt( "Association event not matched: ~ts.",
			%			[ dataflow_support:world_event_to_string(
			%				AssociationEvent ) ] ),
			Param

	end.



% Dispatches specified binary association event to the unit managers (if any)
% that declared an interest for (at least a subset) of the association events.
%
-spec dispatch_binary_association_event( binary_association_event(),
		binary_association_match_table(), wooper:state() ) ->
											{ wooper:state(), boolean() }.
dispatch_binary_association_event( BinAssocEvent, BinAssocTable, State ) ->

	MatchPairs = table:enumerate( BinAssocTable ),

	lists:foldl( fun( { Clause, Managers }, AccState ) ->
					 dispatch_binary_association_event( BinAssocEvent, Clause,
														Managers, AccState )
				 end,
				 _InitalAcc={ State, _IsWaiting=false },
				 _List=MatchPairs ).



% Dispatches specified association event to the unit managers (if any) that
% declared an interest for (at least a subset) of the association events.
%
% (helper)
%
-spec dispatch_binary_association_event( binary_association_event(),
		binary_association_event_match(), [ unit_manager_pid() ],
			{ wooper:state(), boolean() } ) -> { wooper:state(), boolean() }.
dispatch_binary_association_event( BinAssocEvent=#binary_association_event{
							  id=Id,
							  association_type=AssocType,
							  source_object_type=SourceObjectType,
							  target_object_type=TargetObjectType,
							  source_external_id=SourceExternalId,
							  target_external_id=TargetExternalId,
							  % Non-matched:
							  % source_object_pid=SourceObjectPid,
							  % target_object_pid=TargetObjectPid,
							  association_information=AssocInfos,
							  dataflow_pid=DataflowPid },
								   #binary_association_event_match{
							   association_type_match=AssocTypeMatch,
							   source_object_type_match=SourceObjectTypeMatch,
							   target_object_type_match=TargetObjectTypeMatch,
							   source_external_id_match=SourceExternalIdMatch,
							   target_external_id_match=TargetExternalIdMatch,
							   % Non-matched:
							   % source_object_pid_match=SourceObjectPidMatch,
							   % target_object_pid_match=TargetObjectPidMatch,
							   association_info_match=AssocInfosMatch,
							   dataflow_pid_match=DataflowPidMatch },
								   UnitManagers, Param={ State, IsWaiting } ) ->
	case association_type_match( AssocType, AssocTypeMatch )
		andalso object_type_match( SourceObjectType, SourceObjectTypeMatch )
		andalso object_type_match( TargetObjectType, TargetObjectTypeMatch )
		andalso external_id_match( SourceExternalId, SourceExternalIdMatch )
		andalso external_id_match( TargetExternalId, TargetExternalIdMatch )
		andalso association_information_match( AssocInfos, AssocInfosMatch )
		andalso dataflow_pid_match( DataflowPid, DataflowPidMatch ) of

		true ->
			{ lists:foldl( fun( UnitManagerPid, AccState ) ->
							SentState = class_Actor:send_actor_message(
								UnitManagerPid,
								{ processBinaryAssociationEventMatched,
								  [ BinAssocEvent ] },
								AccState ),
							register_waited_ack( Id, UnitManagerPid, SentState )
						   end,
						   _InitialAcc=State,
						   _List=UnitManagers ),
			  _IsWaiting= IsWaiting or ( UnitManagers =/= [] ) };

		false ->
			%?debug_fmt( "Association event not matched: ~ts.",
			%			[ dataflow_support:world_event_to_string(
			%				AssociationEvent ) ] ),
			Param

	end.



% Dispatches specified disassociation event to the unit managers (if any) that
% declared an interest for (at least a subset) of the disassociation events.
%
-spec dispatch_disassociation_event( disassociation_event(),
			disassociation_match_table(), wooper:state() ) ->
										{ wooper:state(), boolean() }.
dispatch_disassociation_event( DisassociationEvent, DisassociationTable,
							   State ) ->

	MatchPairs = table:enumerate( DisassociationTable ),

	lists:foldl( fun( { Clause, Managers }, AccState ) ->
						 dispatch_disassociation_event( DisassociationEvent,
										Clause, Managers, AccState )
				 end,
				 _InitalAcc={ State, _IsWaiting=false },
				 _List=MatchPairs ).



% Dispatches specified disassociation event to the unit managers (if any) that
% declared an interest for (at least a subset) of the disassociation events.
%
% (helper)
%
-spec dispatch_disassociation_event( disassociation_event(),
	disassociation_event_match(), [ unit_manager_pid() ],
		{ wooper:state(), boolean() } ) -> { wooper:state(), boolean() }.
dispatch_disassociation_event( DisassociationEvent=#disassociation_event{
							 id=Id,
							 object_type=ObjectType,
							 external_id=ExternalId,
							 % Non-matched: object_pid=ObjectPid,
							 disassociation_information=DisassociationInfo,
							 dataflow_pid=DataflowPid },
							   #disassociation_event_match{
							 object_type_match=ObjectTypeMatch,
							 external_id_match=ExternalIdMatch,
							 % Non-matched: object_pid_match=ObjectPidMatch,
							 disassociation_info_match=DisassociationInfoMatch,
							 dataflow_pid_match=DataflowPidMatch },
							   UnitManagers, Param={ State, IsWaiting } ) ->
	case object_type_match( ObjectType, ObjectTypeMatch )
		   andalso external_id_match( ExternalId, ExternalIdMatch )
		   andalso disassociation_info_match( DisassociationInfo,
											  DisassociationInfoMatch )
		   andalso dataflow_pid_match( DataflowPid, DataflowPidMatch ) of

		true ->
			{ lists:foldl( fun( UnitManagerPid, AccState ) ->
							SentState = class_Actor:send_actor_message(
								UnitManagerPid,
								{ processDisassociationEventMatched,
								  [ DisassociationEvent ] },
								AccState ),
							register_waited_ack( Id, UnitManagerPid, SentState )
						   end,
						   _InitialAcc=State,
						   _List=UnitManagers ),
			  _IsWaiting= IsWaiting or ( UnitManagers =/= [] ) };

		false ->
			%?debug_fmt( "Disassociation event not matched: ~ts.",
			%			[ dataflow_support:world_event_to_string(
			%				DisassociationEvent ) ] ),
			Param

	end.



% Dispatches specified connection event to the unit managers (if any) that
% declared an interest for (at least a subset) of the connection events.
%
-spec dispatch_connection_event( connection_event(), connection_match_table(),
				wooper:state() ) -> { wooper:state(), boolean() }.
dispatch_connection_event( ConnectionEvent, ConnectionTable, State ) ->

	MatchPairs = table:enumerate( ConnectionTable ),

	%?debug_fmt( "Connection matches: ~p.", [ MatchPairs ] ),

	lists:foldl( fun( { Clause, Managers }, AccState ) ->
					 dispatch_connection_event( ConnectionEvent, Clause,
												Managers, AccState )
				 end,
				 _InitalAcc={ State, _IsWaiting=false },
				 _List=MatchPairs ).



% Dispatches specified connection event to the unit managers (if any) that
% declared an interest for (at least a subset) of the connection events.
%
-spec dispatch_connection_event( connection_event(), connection_event_match(),
				[ unit_manager_pid() ], { wooper:state(), boolean() } ) ->
									{ wooper:state(), boolean() }.
dispatch_connection_event( ConnectionEvent=#connection_event{
							 id=Id,
							 source_block_type=SourceBlockType,
							 target_block_type=TargetBlockType,
							 source_external_id=SourceExternalId,
							 target_external_id=TargetExternalId,
							 % Non-matched: source_block_pid=BlockPid,
							 % Non-matched: target_block_pid=BlockPid,
							 output_port_name=SourceAttrName,
							 input_port_name=TargetAttrName,
							 dataflow_pid=DataflowPid },
						   _Match=#connection_event_match{
							 source_block_type_match=SourceBlockTypeMatch,
							 target_block_type_match=TargetBlockTypeMatch,
							 source_external_id_match=SourceExternalIdMatch,
							 target_external_id_match=TargetExternalIdMatch,
							 output_port_name_match=OutputPortNameMatch,
							 input_port_name_match=InputPortNameMatch,
							 dataflow_pid_match=DataflowPidMatch },
						   UnitManagers, Param={ State, IsWaiting } ) ->

	%?debug_fmt( "Confronting connection event ~p to match ~p.",
	%			  [ ConnectionEvent, Match ] ),

	case        block_type_match( SourceBlockType, SourceBlockTypeMatch )
		andalso block_type_match( TargetBlockType, TargetBlockTypeMatch )
		andalso external_id_match( SourceExternalId, SourceExternalIdMatch )
		andalso external_id_match( TargetExternalId, TargetExternalIdMatch )
		andalso port_name_match( SourceAttrName, OutputPortNameMatch )
		andalso port_name_match( TargetAttrName, InputPortNameMatch )
		andalso dataflow_pid_match( DataflowPid, DataflowPidMatch ) of

		true ->
			?void_fmt( "Connection event matched: ~ts.",
				[ dataflow_support:world_event_to_string( ConnectionEvent ) ] ),

			{ lists:foldl( fun( UnitManagerPid, AccState ) ->
							SentState = class_Actor:send_actor_message(
								UnitManagerPid,
								{ processConnectionEventMatched,
								  [ ConnectionEvent ] },
								AccState ),
							register_waited_ack( Id, UnitManagerPid, SentState )
						   end,
						   _InitialAcc=State,
						   _List=UnitManagers ),
			  _IsWaiting= IsWaiting or ( UnitManagers =/= [] ) };

		false ->
			?void_fmt( "Connection event not matched: ~ts.",
					   [ dataflow_support:world_event_to_string(
						   ConnectionEvent ) ] ),
			Param

	end.



% Dispatches specified disconnection event to the unit managers (if any) that
% declared an interest for (at least a subset) of the disconnection events.
%
-spec dispatch_disconnection_event( disconnection_event(),
		disconnection_match_table(), wooper:state() ) ->
										{ wooper:state(), boolean() }.
dispatch_disconnection_event( DisconnectionEvent, DisconnectionTable, State ) ->

	MatchPairs = table:enumerate( DisconnectionTable ),

	lists:foldl( fun( { Clause, Managers }, AccState ) ->
						 dispatch_disconnection_event( DisconnectionEvent,
											Clause, Managers, AccState )
				 end,
				 _InitalAcc={ State, _IsWaiting=false },
				 _List=MatchPairs ).



% Dispatches specified disconnection event to the unit managers (if any) that
% declared an interest for (at least a subset) of the disconnection events.
%
-spec dispatch_disconnection_event( disconnection_event(),
		disconnection_event_match(), [ unit_manager_pid() ],
		{ wooper:state(), boolean() } ) -> { wooper:state(), boolean() }.
dispatch_disconnection_event( DisconnectionEvent=#disconnection_event{
							 id=Id,
							 source_block_type=SourceBlockType,
							 target_block_type=TargetBlockType,
							 source_external_id=SourceExternalId,
							 target_external_id=TargetExternalId,
							 % Non-matched: source_block_pid=BlockPid,
							 % Non-matched: target_block_pid=BlockPid,
							 output_port_name=OutputPortName,
							 input_port_name=InputPortName,
							 dataflow_pid=DataflowPid },
							  #disconnection_event_match{
							 source_block_type_match=SourceBlockTypeMatch,
							 target_block_type_match=TargetBlockTypeMatch,
							 source_external_id_match=SourceExternalIdMatch,
							 target_external_id_match=TargetExternalIdMatch,
							 output_port_name_match=OutputPortNameMatch,
							 input_port_name_match=InputPortNameMatch,
							 dataflow_pid_match=DataflowPidMatch },
							  UnitManagers, Param={ State, IsWaiting } ) ->
	case        block_type_match( SourceBlockType, SourceBlockTypeMatch )
		andalso block_type_match( TargetBlockType, TargetBlockTypeMatch )
		andalso external_id_match( SourceExternalId, SourceExternalIdMatch )
		andalso external_id_match( TargetExternalId, TargetExternalIdMatch )
		andalso port_name_match( OutputPortName, OutputPortNameMatch )
		andalso port_name_match( InputPortName, InputPortNameMatch )
		andalso dataflow_pid_match( DataflowPid, DataflowPidMatch ) of

		true ->
			{ lists:foldl( fun( UnitManagerPid, AccState ) ->
							SentState = class_Actor:send_actor_message(
								UnitManagerPid,
								{ processDisconnectionEventMatched,
								  [ DisconnectionEvent ] },
								AccState ),
							register_waited_ack( Id, UnitManagerPid, SentState )
						   end,
						   _InitialAcc=State,
						   _List=UnitManagers ),
			  _IsWaiting= IsWaiting or ( UnitManagers =/= [] ) };

		false ->
			%?debug_fmt( "Disconnection event not matched: ~ts.",
			%			[ dataflow_support:world_event_to_string(
			%				DisconnectionEvent ) ] ),
			Param

	end.



% Dispatches specified update event to the unit managers (if any) that declared
% an interest for (at least a subset) of the update events.
%
-spec dispatch_update_event( update_event(), update_match_table(),
							 wooper:state() ) -> { wooper:state(), boolean() }.
dispatch_update_event( UpdateEvent, UpdateTable, State ) ->

	MatchPairs = table:enumerate( UpdateTable ),

	lists:foldl( fun( { Clause, Managers }, AccState ) ->
						 dispatch_update_event( UpdateEvent, Clause, Managers,
												AccState )
				 end,
				 _InitalAcc={ State, _IsWaiting=false },
				 _List=MatchPairs ).



% Dispatches specified update event to the unit managers (if any) that
% declared an interest for (at least a subset) of the update events.
%
-spec dispatch_update_event( update_event(), update_event_match(),
   [ unit_manager_pid() ], { wooper:state(), boolean() } ) -> wooper:state().
dispatch_update_event( UpdateEvent=#update_event{
							 id=Id,
							 object_type=ObjectType,
							 external_id=ExternalId,
							 % Non-matched: object_pid=ObjectPid,
							 updates=AttrUpdates,
							 dataflow_pid=DataflowPid },
						 #update_event_match{
							 object_type_match=ObjectTypeMatch,
							 external_id_match=ExternalIdMatch,
							 % Non-matched: object_pid_match=ObjectPidMatch,
							 attribute_update_match=AttrUpdateMatch,
							 dataflow_pid_match=DataflowPidMatch },
						 UnitManagers, Param={ State, IsWaiting } ) ->
	case object_type_match( ObjectType, ObjectTypeMatch )
		   andalso external_id_match( ExternalId, ExternalIdMatch )
		   andalso update_event_match( AttrUpdates, AttrUpdateMatch )
		   andalso dataflow_pid_match( DataflowPid, DataflowPidMatch ) of

		true ->
			{ lists:foldl( fun( UnitManagerPid, AccState ) ->
							SentState = class_Actor:send_actor_message(
								UnitManagerPid,
								{ processUpdateEventMatched, [ UpdateEvent ] },
								AccState ),
							register_waited_ack( Id, UnitManagerPid, SentState )
						   end,
						   _InitialAcc=State,
						   _List=UnitManagers ),
			  _IsWaiting= IsWaiting or ( UnitManagers =/= [] ) };

		false ->
			%?debug_fmt( "Update event not matched: ~ts.",
			%			[ dataflow_support:world_event_to_string(
			%				UpdateEvent ) ] ),
			Param

	end.





% Registers specified event (through its identifier) among the ones whose
% processing acknowledgment is waited for (from specified unit manager).
%
-spec register_waited_ack( event_id(), unit_manager_pid(), wooper:state() ) ->
								wooper:state().
register_waited_ack( EventId, UnitManagerPid, State ) ->
	% All unit managers already known:
	NewAcks = table:append_to_existing_entry( _K=UnitManagerPid, _V=EventId,
											  ?getAttr(waited_event_acks) ),
	setAttribute( State, waited_event_acks, NewAcks ).



% Match helpers.


% Tells whether specified block type matches the specified clause.
%
% (match helper)
%
-spec block_type_match( block_type(), block_type_match() ) -> boolean().
block_type_match( _BlockType, _BlockTypeMatch=any_block_type ) ->
	true;

block_type_match( BlockType, _BlockTypeMatch=BlockType ) ->
	true;

block_type_match( _BlockType, _BlockTypeMatch ) ->
	false.



% Tells whether specified object type matches the specified clause.
%
% (match helper)
%
-spec object_type_match( dataflow_object_type(), object_type_match() ) ->
								boolean().
object_type_match( _ObjectType, _ObjectTypeMatch=any_object_type ) ->
	true;

object_type_match( ObjectType, _ObjectTypeMatch=ObjectType ) ->
	true;

object_type_match( _ObjectType, _ObjectTypeMatch ) ->
	false.



% Tells whether specified external identifier matches the specified clause.
%
% (match helper)
%
-spec external_id_match( external_id(), external_id_match() ) -> boolean().
external_id_match( _ExternalId, _ExternalIdMatch=any_external_id ) ->
	true;

external_id_match( ExternalId, _ExternalIdMatch=ExternalId ) ->
	true;

external_id_match( _ExternalId, _ExternalIdMatch ) ->
	false.



% Tells whether specified construction parameters match the specified clause.
%
% (match helper)
%
-spec construction_parameters_match( wooper:construction_parameters(),
				 construction_parameters_match() ) -> boolean().
construction_parameters_match( _ConstructParams,
						_ConstructParamsMatch=any_construction_parameters ) ->
	true;

construction_parameters_match( ConstructParams,
							   _ConstructParamsMatch=ConstructParams ) ->
	true;

construction_parameters_match( _ConstructParams, _ConstructParamsMatch ) ->
	false.



% Tells whether specified association type matches the specified clause.
%
% (match helper)
%
-spec association_type_match( association_type(), association_type_match() ) ->
									boolean().
association_type_match( _AssociationType,
						_AssociationTypeMatch=any_association_type ) ->
	true;

association_type_match( AssociationType,
						_AssociationTypeMatch=AssociationType ) ->
	true;

association_type_match( _AssociationType, _AssociationTypeMatch ) ->
	false.



% Tells whether specified association information matches the specified clause.
%
% (match helper)
%
-spec association_information_match( association_info(),
									 association_info_match() ) -> boolean().
association_information_match( _AssociationInfo,
							   _AssociationInfoMatch=any_association_info ) ->
	true;

association_information_match( AssociationInfo,
							   _AssociationInfoMatch=AssociationInfo ) ->
	true;

association_information_match( _AssociationInfo, _AssociationInfoMatch ) ->
	false.



% Tells whether specified disassociation information matches the specified
% clause.
%
% (match helper)
%
-spec disassociation_info_match( disassociation_info(),
								 disassociation_info_match() ) -> boolean().
disassociation_info_match( _DisassociationInfo,
						   _DisassociationInfoMatch=any_disassociation_info ) ->
	true;

disassociation_info_match( DisassociationInfo,
						   _DisassociationInfoMatch=DisassociationInfo ) ->
	true;

disassociation_info_match( _DisassociationInfo, _DisassociationInfoMatch ) ->
	false.



% Tells whether specified attribute update matches the specified clause.
%
% (match helper)
%
-spec update_event_match( [ class_DataflowObject:attribute_update() ],
						  attribute_update_match() ) -> boolean().
update_event_match( _AttrUpdates, _AttrUpdateMatch=any_attribute_update ) ->
	true;

% Expected to be sorted the same:
update_event_match( AttrUpdates, _AttrUpdateMatch=AttrUpdates ) ->
	true;

update_event_match( _AttrUpdates, _AttrUpdateMatch ) ->
	false.



% Tells whether specified port information matches the specified clause.
%
% (match helper)
%
-spec port_name_match( port_string_name(), port_name_match() ) -> boolean().
port_name_match( _PortName, _PortNameMatch=any_port_name ) ->
	true;

port_name_match( PortName, _PortNameMatch=PortName ) ->
	true;

port_name_match( _PortName, _PortNameMatch ) ->
	false.



% Tells whether specified dataflow PID matches with the specified clause.
%
% (match helper)
%
-spec dataflow_pid_match( dataflow_pid(), dataflow_pid_match() ) -> boolean().
dataflow_pid_match( _DataflowPid, _DataflowPidMatch=any_dataflow_pid ) ->
	true;

dataflow_pid_match( DataflowPid, _DataflowPidMatch=DataflowPid ) ->
	true;

dataflow_pid_match( _DataflowPid, _DataflowPidMatch ) ->
	false.




% Helpers for matches.



% Returns a textual description of this experiment manager.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	WorldString = case ?getAttr(world_manager_pid) of

		undefined ->
			"not associated to the world manager";

		WorldPid ->
			text_utils:format( "associated to the world manager ~w",
							   [ WorldPid ] )

	end,

	DataflowString = case ?getAttr(dataflows) of

		[] ->
			"not referencing any dataflow";

		[ Dataflow ] ->
			text_utils:format( "referencing a single dataflow instance ~w",
							   [ Dataflow ] );

		Dataflows ->
			text_utils:format( "referencing ~B dataflow instances: ~w",
							   [ length( Dataflows ), Dataflows ] )

	end,

	EntryPointString = case ?getAttr(entry_point_pid) of

		undefined ->
			"not referencing an experiment entry point";

		EntryPointPid ->
			text_utils:format( "referencing the experiment entry point ~w",
							   [ EntryPointPid ] )

	end,

	ExitPointString = case ?getAttr(exit_point_pid) of

		undefined ->
			"not referencing an experiment exit point";

		ExitPointPid ->
			text_utils:format( "referencing the experiment exit point ~w",
							   [ ExitPointPid ] )

	end,

	UnitManagers = table:keys( ?getAttr(unit_managers) ),

	UnitString = case length( UnitManagers ) of

		0 ->
			"not having any unit manager";

		UnitManagerCount ->
			text_utils:format( "registering ~B unit managers: ~w",
							   [ UnitManagerCount, UnitManagers ] )

	end,

	PendingString = case ?getAttr(pending_events) of

		[] ->
			"with no pending event";

		PendingEvents ->
			PendingIds = [ dataflow_support:get_event_id( E )
						   || E <- PendingEvents ],
			text_utils:format( "with ~B pending events (of identifiers ~w)",
							   [ length( PendingEvents ), PendingIds ] )

	end,

	AckString = ack_table_to_string( ?getAttr(waited_event_acks) ),

	MatchString = match_tables_to_string( State ),

	text_utils:format( "experiment manager ~ts, ~ts, ~ts, ~ts, ~ts, ~ts, ~ts "
		"and ~ts", [ WorldString, DataflowString, EntryPointString,
					 ExitPointString, UnitString, PendingString, AckString,
					 MatchString ] ).



% Returns a textual description of the acknowledgement waited table.
-spec ack_table_to_string( ack_table() ) -> string().
ack_table_to_string( AckTable ) ->

	case lists:foldl( fun

		( { _UnitManagerPid, _WaitedEvents=[] }, AccStrings )  ->
			AccStrings;

		( { UnitManagerPid, WaitedEvents }, AccStrings )  ->
			ManagerString = text_utils:format( "~B events waited from unit "
				"manager ~w: event identifiers are ~w",
				[ length( WaitedEvents ), UnitManagerPid, WaitedEvents ] ),
			[ ManagerString | AccStrings ]

					  end,
					  _Acc0=[],
					  _List=table:enumerate( AckTable ) ) of

		[] ->
			"not waiting for any unit manager to process any event";

		AckStrings ->
			text_utils:format( "waiting for following event processing "
				"acknowledgements: ~ts",
				[ text_utils:strings_to_string( AckStrings ) ] )

	end.



% Returns a textual description of the event match tables.
-spec match_tables_to_string( wooper:state() ) -> string().
match_tables_to_string( State ) ->

	MatchTables = ?getAttr(match_tables),

	AnyList = MatchTables#match_tables.any_match_table,

	AnyString = case length( AnyList ) of

		0 ->
			"no unit manager listens for any kind of event";

		AnyLen ->
			text_utils:format( "following ~B unit managers listen for any "
							   "kind of event: ~w", [ AnyLen, AnyList ] )

	end,


	CreationList = table:enumerate(
					MatchTables#match_tables.creation_match_table ),

	CreationString = case length( CreationList ) of

		0 ->
			"no unit manager listens for creation events";

		CreationLen ->

			CreationStrings = [ event_clause_to_string( CM, Managers )
								|| { CM, Managers } <- CreationList ],

			CreationBulletString = text_utils:strings_to_string(
				CreationStrings, text_utils:get_bullet_for_level( 1 ) ),

			text_utils:format( "following ~B creation clauses registered: ~ts",
							   [ CreationLen, CreationBulletString ] )

	end,


	DestructionList = table:enumerate(
						MatchTables#match_tables.destruction_match_table ),

	DestructionString = case length( DestructionList ) of

		0 ->
			"no unit manager listens for destruction events";

		DestructionLen ->

			DestructionStrings = [ event_clause_to_string( CM, Managers )
									|| { CM, Managers } <- DestructionList ],

			DestructionBulletString = text_utils:strings_to_string(
				DestructionStrings, text_utils:get_bullet_for_level( 1 ) ),

			text_utils:format(
			  "following ~B destruction clauses registered: ~ts",
			  [ DestructionLen, DestructionBulletString ] )

	end,


	AssociationList = table:enumerate(
						MatchTables#match_tables.association_match_table ),

	AssociationString = case length( AssociationList ) of

		0 ->
			"no unit manager listens for (general) association events";

		AssociationLen ->

			AssociationStrings = [ event_clause_to_string( CM, Managers )
								   || { CM, Managers } <- AssociationList ],

			AssociationBulletString = text_utils:strings_to_string(
				AssociationStrings, text_utils:get_bullet_for_level( 1 ) ),

			text_utils:format( "following ~B association clauses "
				"registered: ~ts", [ AssociationLen, AssociationBulletString ] )

	end,


	BinAssocList = table:enumerate(
					MatchTables#match_tables.binary_association_match_table ),

	BinAssocString = case length( BinAssocList ) of

		0 ->
			"no unit manager listens for binary association events";

		BinAssocLen ->

			BinAssocStrings = [ event_clause_to_string( CM, Managers )
								|| { CM, Managers } <- BinAssocList ],

			BinAssocBulletString = text_utils:strings_to_string(
				BinAssocStrings, text_utils:get_bullet_for_level( 1 ) ),

			text_utils:format(
			  "following ~B binary association clauses registered: ~ts",
			  [ BinAssocLen, BinAssocBulletString ] )

	end,


	DisassociationList = table:enumerate(
					 MatchTables#match_tables.disassociation_match_table ),

	DisassociationString = case length( DisassociationList ) of

		0 ->
			"no unit manager listens for disassociation events";

		DisassociationLen ->

			DisassociationStrings = [ event_clause_to_string( CM, Managers )
								|| { CM, Managers } <- DisassociationList ],

			DisassociationBulletString = text_utils:strings_to_string(
				DisassociationStrings, text_utils:get_bullet_for_level( 1 ) ),

			text_utils:format( "following ~B disassociation clauses "
				"registered: ~ts",
				[ DisassociationLen, DisassociationBulletString ] )

	end,


	ConnectionList = table:enumerate(
						MatchTables#match_tables.connection_match_table ),

	ConnectionString = case length( ConnectionList ) of

		0 ->
			"no unit manager listens for connection events";

		ConnectionLen ->

			ConnectionStrings = [ event_clause_to_string( CM, Managers )
								  || { CM, Managers } <- ConnectionList ],

			ConnectionBulletString = text_utils:strings_to_string(
				ConnectionStrings, text_utils:get_bullet_for_level( 1 ) ),

			text_utils:format( "following ~B connection clauses "
				"registered: ~ts", [ ConnectionLen, ConnectionBulletString ] )

	end,


	DisconnectionList = table:enumerate(
						MatchTables#match_tables.disconnection_match_table ),

	DisconnectionString = case length( DisconnectionList ) of

		0 ->
			"no unit manager listens for disconnection events";

		DisconnectionLen ->

			DisconnectionStrings = [ event_clause_to_string( CM, Managers )
									 || { CM, Managers } <- DisconnectionList ],

			DisconnectionBulletString = text_utils:strings_to_string(
				DisconnectionStrings, text_utils:get_bullet_for_level( 1 ) ),

			text_utils:format( "following ~B disconnection clauses "
				"registered: ~ts",
				[ DisconnectionLen, DisconnectionBulletString ] )

	end,


	UpdateList = table:enumerate(
					MatchTables#match_tables.update_match_table ),

	UpdateString = case length( UpdateList ) of

		0 ->
			"no unit manager listens for update events";

		UpdateLen ->
			UpdateStrings = [ event_clause_to_string( CM, Managers )
							  || { CM, Managers } <- UpdateList ],

			UpdateBulletString = text_utils:strings_to_string(
				UpdateStrings, text_utils:get_bullet_for_level( 1 ) ),

			text_utils:format( "following ~B update clauses registered: ~ts",
							   [ UpdateLen, UpdateBulletString ] )

	end,

	StringList = [ AnyString, CreationString, DestructionString,
				   AssociationString, BinAssocString, DisassociationString,
				   ConnectionString, DisconnectionString, UpdateString ],

	text_utils:format( "regarding synchronization events: ~ts",
					   [ text_utils:strings_to_string( StringList ) ] ).



% Returns a textual description of the specified event clause.
-spec event_clause_to_string( event_match(), [ unit_manager_pid() ] ) ->
									ustring().
% At least one unit manager expected to be listed:
event_clause_to_string( EventMatch, UnitManagers ) ->
	text_utils:format( "synchronization events matching ~ts will be routed "
		"to following ~B unit manager(s): ~w",
		[ class_DataflowUnitManager:event_clause_to_string( EventMatch ),
		  length( UnitManagers ), UnitManagers ] ).
