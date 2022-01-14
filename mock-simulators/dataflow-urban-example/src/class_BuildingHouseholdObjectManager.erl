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


% @doc Example of <b>object manager</b>.
-module(class_BuildingHouseholdObjectManager).

-define( class_description,
		 "This object manager is in charge both of the buildings and of the "
		 "households involved in this simulation."
		 "Such a specialization of class_DataflowObjectManager has been "
		 "introduced in order to showcase how domain-specific associations "
		 "('located_in_district' and 'living_in_building' in this example) "
		 "can be introduced." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowObjectManager ] ).



% Design notes:
%
% This object manager has been specifically defined in a class of its own (as
% opposed to be a direct instance of class_DataflowObjectManager), to showcase
% how associations can be handled in a domain-specific way of choice.
%
% The initial value of the disassociation_information field of the corresponding
% disassociation_event is expected here to be { 'living_in_building',
% BuildingExternalId } (typically specified from the dataflow entry point), the
% associated household being designated by the external_id field of that event.
%
% Once this event will have been processed, at the level of the world manager
% the disassociation_information field will be augmented in:
% { 'living_in_building', BuildingExternalId, BuildingPid }, while the
% object_pid field will hold the PID of said household.


-include("urban_example_defines.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Urban-Example.BuildingHouseholdManager" ).


% For dataflow-related types and names:
-include("sim_diasca_for_actors.hrl").



% No attribute is specific to this object manager.


% @doc Constructs a building and household manager, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as automatically assigned by the load balancer
%
% - WorldManagerPid, the PID of the parent object manager
%
% - LoadBalancerPid, the PID of the load balancer that may be used by this
% object manager
%
% - IdentificationServerPid, the PID of the identification server (if any)
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_DataflowObjectManager:parent_pid(), load_balancer_pid(),
				 maybe( identification_server_pid() ) ) -> wooper:state().
construct( State, ActorSettings, WorldManagerPid, LoadBalancerPid,
		   IdentificationServerPid ) ->

	ManagedObjectTypes = [ class_Building, class_Household ],

	class_DataflowObjectManager:construct( State, ActorSettings,
		?trace_categorize(?MODULE), ManagedObjectTypes,
		WorldManagerPid, LoadBalancerPid, IdentificationServerPid ).



% Methods section.


% @doc Applies the specified changeset (a list of world events).
-spec applyChangeset( wooper:state(), changeset(), actor_pid() ) ->
								actor_oneway_return().
applyChangeset( State, Changeset, SendingActorPid ) ->

	% This oneway has been overridden so that the domain-specific
	% 'located_in_district' and 'living_in_building' associations can be
	% managed:
	%
	?debug_fmt( "Applying ~ts (received from ~w)",
		[ dataflow_support:changeset_to_string( Changeset ),
		  SendingActorPid ] ),

	% Delegates all other events to the mother, generic class (always done even
	% if no event remains, as this base implementation performs additional
	% checks):
	%
	{ RemainingEvents, FilteredState } =
		filter_world_events( Changeset, State ),

	DelegatedState = executeOnewayAs( FilteredState,
		class_DataflowObjectManager, applyChangeset,
		[ RemainingEvents, self() ] ),

	actor:return_state( DelegatedState ).



% @doc Filters the specified events: selects a subset of them (the ones managed
% specifically by this child object manager) and process them, and returns the
% other, unmanaged events (expected to be managed generically by the object
% manager class).
%
% Anything can be done here: adding, modifying, reordering, removing events.
%
% (helper)
%
-spec filter_world_events( [ world_event() ], [ dataflow_object_type() ],
						   wooper:state() ) -> { changeset(), wooper:state() }.
filter_world_events( Events, State ) ->
	filter_world_events( Events, _Acc=[], State ).


filter_world_events( _WorldEvents=[], Acc, State ) ->
	{ Acc, State };

% Here we associate a building to the district it will be located in:
filter_world_events(
  _WorldEvents=[ BinAssocEvent=#binary_association_event{
		% Extraneous checkings performed:
		id=EventId,
		association_type=located_in_district,
		source_object_type=class_Building,
		target_object_type=class_District,
		source_external_id=BuildingExternalId,
		target_external_id=DistrictExternalId,
		source_object_pid=undefined,
		target_object_pid=undefined }
				 | T ], Acc, State ) ->

	?debug_fmt( "Object manager ~p associating (located-in) the building "
		"instance named '~ts' to the district named '~ts'.",
		[ self(), BuildingExternalId, DistrictExternalId ] ),

	[ BuildingPid, DistrictPid ] = class_DataflowObjectManager:get_object_pids(
					[ BuildingExternalId, DistrictExternalId ], State ),

	BuildingSentState = class_Actor:send_actor_message( BuildingPid,
		{ setDistrict, [ DistrictPid ] }, State ),

	DistrictSentState = class_Actor:send_actor_message( DistrictPid,
		{ registerBuilding, [ BuildingPid ] }, BuildingSentState ),

	% Hence handled next diasca, as wanted; probably better than having the
	% building or the district report all these information by itself:
	%
	SelfSentState = class_Actor:send_actor_message( self(),
		{ onBinaryAssociationEstablished, [ EventId ] }, DistrictSentState ),

	UpdatedBinAssocEvent = BinAssocEvent#binary_association_event{
								source_object_pid=BuildingPid,
								target_object_pid=DistrictPid },

	% To prepare upcoming completion:
	RegisterState = appendToAttribute( SelfSentState, triggered_events,
									   UpdatedBinAssocEvent ),

	filter_world_events( T, Acc, RegisterState );


% Here we associate an household to the building it will live in:
filter_world_events(
  _WorldEvents=[ BinAssocEvent=#binary_association_event{
		% Extraneous checkings performed:
		id=EventId,
		association_type=living_in_building,
		source_object_type=class_Household,
		target_object_type=class_Building,
		source_external_id=HouseholdExternalId,
		target_external_id=BuildingExternalId,
		source_object_pid=undefined,
		target_object_pid=undefined } | T ], Acc, State ) ->

	?debug_fmt( "Object manager ~p associating (living-in) the household "
		"instance named '~ts' to the building named '~ts'.",
		[ self(), HouseholdExternalId, BuildingExternalId ] ),

	[ HouseholdPid, BuildingPid ] = class_DataflowObjectManager:get_object_pids(
					[ HouseholdExternalId, BuildingExternalId ], State ),

	 HouseholdSentState = class_Actor:send_actor_message( HouseholdPid,
				{ setBuilding, [ BuildingPid ] }, State ),

	 BuildingSentState = class_Actor:send_actor_message( BuildingPid,
				{ registerHousehold, [ HouseholdPid ] }, HouseholdSentState ),

	% Hence handled next diasca, as wanted:
	SelfSentState = class_Actor:send_actor_message( self(),
		{ onBinaryAssociationEstablished, [ EventId ] },
		BuildingSentState ),

	UpdatedBinAssocEvent = BinAssocEvent#binary_association_event{
							 source_object_pid=HouseholdPid,
							 target_object_pid=BuildingPid },

	RegisterState = appendToAttribute( SelfSentState, triggered_events,
									   UpdatedBinAssocEvent ),

	filter_world_events( T, Acc, RegisterState );


% Here we disassociate an household from the building it used to live in:
filter_world_events(
  _WorldEvents=[ DisassocEvent=#disassociation_event{
		% Extraneous checkings performed:
		id=EventId,
		object_type=class_Household,
		external_id=HouseholdExternalId,
		object_pid=undefined,
		disassociation_information={ living_in_building, BuildingExtId } }
					| T ],
  Acc, State ) ->

	% May be specified by the user as a string or a binary:
	BuildingExternalId = text_utils:ensure_binary( BuildingExtId ),

	?debug_fmt( "Object manager ~p disassociating (was: living-in) the "
		"household instance named '~ts' from the building named '~ts'.",
		[ self(), HouseholdExternalId, BuildingExternalId ] ),

	[ HouseholdPid, BuildingPid ] = class_DataflowObjectManager:get_object_pids(
					[ HouseholdExternalId, BuildingExternalId ], State ),

	 HouseholdSentState = class_Actor:send_actor_message( HouseholdPid,
		{ unsetBuilding, [ BuildingPid ] }, State ),

	 BuildingSentState = class_Actor:send_actor_message( BuildingPid,
		{ unregisterHousehold, [ HouseholdPid ] }, HouseholdSentState ),

	% Hence handled next diasca (by the mother class), as wanted:
	SelfSentState = class_Actor:send_actor_message( self(),
		{ onDisassociationPerformed, [ EventId ] }, BuildingSentState ),

	% We update in-place the disassociation information (for completeness), from
	% a pair to a triplet so that we can also record the PID of that building:
	%
	% So now the type of this information is:
	% { 'living_in_building', text_utils:bin_string(), object_pid() }.
	%
	UpdatedDisassocEvent = DisassocEvent#disassociation_event{
		object_pid=HouseholdPid,
		disassociation_information={ living_in_building,
									 BuildingExternalId, BuildingPid } },

	RegisterState = appendToAttribute( SelfSentState, triggered_events,
									   UpdatedDisassocEvent ),

	filter_world_events( T, Acc, RegisterState );


filter_world_events( _WorldEvents=[ Event | T ], Acc, State ) ->

	?debug_fmt( "World event '~p' not specifically handled by this object "
		"manager, its processing will be delegated to the generic, "
		"parent one (i.e. defined in the DataflowObjectManager mother class).",
		[ Event ] ),

	filter_world_events( T, [ Event | Acc ], State ).
