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


% @doc A joint <b>test unit manager</b>.
-module(class_UrbanUnitManager).


-define( class_description,
		 "This test unit manager is in charge of the demand-related units both "
		 "for transportation and energy; it is thus a joint unit manager." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowUnitManager ] ).


% Attributes that are specific to this unit manager singleton are:
-define( class_attributes, [

	{ transport_table, table( household_pid(), transportation_unit_pid() ),
	  "a table telling, for an household PID, the PID of its associated "
	  "transportation unit (if any)" },

	{ energy_table, table( building_pid(), energy_unit_pid() ),
	  "a table telling, for a building PID, the PID of its associated energy "
	  "unit (if any)" } ] ).


% Helpers:
-export([ to_string/1 ]).


% Design notes:
%
% This unit manager does not define specific types.


-include("urban_example_defines.hrl").


% For binding_managers():
-include("bindings.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Urban-Example.UrbanUnitManager" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Constructs a urban unit manager, from:
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
	experiment_manager_pid(), binding_managers(), load_balancer_pid(),
	maybe( identification_server_pid() ) ) -> wooper:state().
construct( State, ActorSettings, ExperimentManagerPid, BindingManagers,
		   LoadBalancerPid, IdentificationServerPid ) ->

	% The first unit is implicitly implemented in Erlang:
	ManagedUnitSpec = [ class_TransportationDemandUnit,
						{ class_VehicleTypeUnit, erlang },
						{ class_EnergyDemandUnit, erlang } ],

	ListenedEventMatches = get_listened_event_matches(),

	EmitterName = ?MODULE,

	BaseState = class_DataflowUnitManager:construct( State, ActorSettings,
		?trace_categorize(EmitterName), ManagedUnitSpec,
		ListenedEventMatches, ExperimentManagerPid, BindingManagers,
		LoadBalancerPid, IdentificationServerPid ),

	EmptyTable = table:new(),

	setAttributes( BaseState, [ { transport_table, EmptyTable },
								{ energy_table, EmptyTable } ] ).




% Methods section.


% @doc Returns the synchronization event matches that this unit manager is
% interested in.
%
-spec get_listened_event_matches() -> static_return( [ event_match() ] ).
get_listened_event_matches() ->

	% We want to be notified iff:
	%
	% - an household is created, to create its corresponding
	% TransportationDemand unit
	%
	% - an household is associated to a building, to showcase this feature
	%
	% - a building is created, to create its corresponding EnergyDemand unit
	%
	% - a building is associated to a district, also to showcase this feature

	HouseholdCreationMatch = #creation_event_match{
			  object_type_match=class_Household },

	HouseholdAssociationMatch = #binary_association_event_match{
		association_type_match=living_in_building,
		source_object_type_match=class_Household
		% Implied: target_object_type_match=class_Building
								  },

	HouseholdDestructionMatch = #destruction_event_match{
		object_type_match=class_Household },

	BuildingCreationMatch = #creation_event_match{
		object_type_match=class_Building },

	BuildingAssociationMatch = #binary_association_event_match{
		association_type_match=located_in_district,
		source_object_type_match=class_Building
		% Implied: target_object_type_match=class_District
								 },

	% A lot more precise than a simple 'any_event_type':
	wooper:return_static( [ HouseholdCreationMatch, HouseholdAssociationMatch,
		HouseholdDestructionMatch, BuildingCreationMatch,
		BuildingAssociationMatch ] ).



% @doc Called so that this unit manager can perform domain-specific actions of
% its choice whenever a building or a household has just been created.
%
% As a result, if the created dataflow object is:
%
% - a building, then ultimately (over diascas) a EnergyDemandUnit is created and
% connected appropriately
%
% - a household, then ultimately (over diascas) a TransportationDemandUnit is
% created and connected appropriately
%
-spec onCreationEventMatched( wooper:state(), creation_event() ) ->
									oneway_return().
onCreationEventMatched( State, #creation_event{
								id=EventId,
								object_type=class_Building,
								object_pid=BuildingPid,
								construction_parameters=ConstructParams,
								dataflow_pid=DataflowPid } ) ->

	% Here we create with relevant parameters an energy demand unit
	% corresponding to this building:

	BuildingName = hd( ConstructParams ),

	UnitName = text_utils:format(
		"Energy Demand unit for the '~ts' building", [ BuildingName ] ),

	?debug_fmt( "Reacting to the creation of a building (~w; event #~B) "
		"by creating in turn its associated energy unit, to be named '~ts'.",
		[ BuildingPid, EventId, UnitName ] ),

	% In the same dataflow:
	UnitConstructParams = [ UnitName, DataflowPid ],

	% Will trigger back a call to onUnitCreated/6:
	CreatedState = executeOneway( State, createUnit,
		[ class_EnergyDemandUnit, UnitConstructParams, EventId,
		  _Context=BuildingPid ] ),

	wooper:return_state( CreatedState );


onCreationEventMatched( State, #creation_event{
									id=EventId,
									object_type=class_Household,
									object_pid=HouseholdPid,
									construction_parameters=ConstructParams,
									dataflow_pid=DataflowPid } ) ->

	% Here we create with relevant parameters a transportation demand unit
	% corresponding to this household:

	HouseholdName = hd( ConstructParams ),

	UnitName = text_utils:format( "Transportation Demand unit for the '~ts' "
								  "household", [ HouseholdName ] ),

	?debug_fmt( "Reacting to the creation of an household (~w; event #~B) "
		"by creating in turn its associated transportation unit, "
		"to be named '~ts'.", [ HouseholdPid, EventId, UnitName ] ),

	% Constant 10% here:
	LevelOfVehicleSharing = 0.1,

	% In the same dataflow:
	UnitConstructParams = [ UnitName, LevelOfVehicleSharing, DataflowPid ],

	% The household -> transportation unit association will be recorded in that
	% callback, as we need the PID of that unit.

	% Will trigger back a call to onUnitCreated/6:
	CreatedState = executeOneway( State, createUnit,
		[ class_TransportationDemandUnit, UnitConstructParams, EventId,
		  _Context=HouseholdPid ] ),

	wooper:return_state( CreatedState ).




% @doc Called so that this unit manager can perform domain-specific actions of
% its choice whenever a household has just been destructed.
%
% As a result, its corresponding transportation unit shall be removed as well,
% once having been disconnected.
%
-spec onDestructionEventMatched( wooper:state(), destruction_event() ) ->
										oneway_return().
onDestructionEventMatched( State, #destruction_event{
									id=EventId,
									% Just a check:
									object_type=class_Household,
									external_id=HouseholdExtId,
									object_pid=HouseholdPid,
									dataflow_pid=DataflowPid } ) ->

	?debug_fmt( "Reacting to the destruction of an household, named "
		"'~ts', of PID ~w, in dataflow ~w, in the context of event #~B.",
		[ HouseholdExtId, HouseholdPid, DataflowPid, EventId ] ),

	% We have to destruct in turn the (transportation demand) unit associated to
	% this household (we can already forget that unit):
	%
	{ TransportUnitPid, ShrunkTransportTable } =
		table:extract_entry( HouseholdPid, ?getAttr(transport_table) ),

	ForgetState = setAttribute( State, transport_table, ShrunkTransportTable ),

	% Will trigger a onUnitDestructed/3 callback:
	DestructedState = executeOneway( ForgetState, destructUnit,
									 [ TransportUnitPid, EventId ] ),

	wooper:return_state( DestructedState ).



% @doc Called so that this unit manager can perform domain-specific actions of
% its choice whenever a matching binary association happened.
%
-spec onBinaryAssociationEventMatched( wooper:state(),
						binary_association_event() ) -> const_oneway_return().
onBinaryAssociationEventMatched( State,
								 BinAssociationEvent=#binary_association_event{
									id=EventId,
									association_type=living_in_building,
									source_object_type=class_Household,
									%target_object_type=class_Building,
									source_object_pid=_HouseholdPid,
									target_object_pid=_BuildingPid,
									association_information=_AssocInfos } ) ->

	%trace_utils:debug_fmt( "association: household ~p living in building "
	%  "~p: ~p", [ HouseholdPid, BuildingPid, BinAssociationEvent ] ),

	?debug_fmt( "Reacting to the association of an household (event #~B): ~ts",
		[ EventId,
		  dataflow_support:world_event_to_string( BinAssociationEvent ) ] ),

	wooper:const_return();


onBinaryAssociationEventMatched( State,
								 BinAssociationEvent=#binary_association_event{
									id=EventId,
									association_type=located_in_district,
									source_object_type=class_Building,
									%target_object_type=class_District,
									source_object_pid=_BuildingPid,
									target_object_pid=_DistrictPid,
									association_information=_AssocInfos } ) ->

	%trace_utils:debug_fmt( "association: building ~p located in district ~p: "
	% "~p", [ BuildingPid, DistrictPid, BinAssociationEvent ] ),

	?debug_fmt( "Reacting to the association of an household (event #~B): ~ts",
		[ EventId,
		  dataflow_support:world_event_to_string( BinAssociationEvent ) ] ),

	wooper:const_return().



% @doc Notifies this unit manager how a dataflow-level event shall be handled,
% not relying on the changeset system for that (just useful in the context of a
% programmatic case).
%
-spec notifyEvent( wooper:state(), urban_dataflow_event(), event_data(),
				   sending_actor_pid() ) -> actor_oneway_return().
notifyEvent( State, _Event=new_energy_demand_unit_needed, UnitName,
			 _SendingActorPid ) ->

	?debug_fmt( "Creating an energy demand unit, named '~ts'.",
				[ UnitName ] ),

	CreatedState = class_DataflowUnitManager:create_runtime_unit(
		_UnitClassname={ erlang, class_EnergyDemandUnit },
		_CoreConstructionParameters=[ UnitName ], State ),

	actor:return_state( CreatedState ).




% @doc Called whenever a unit has been created, so that channels between this
% new unit and the rest of the dataflow can be created.
%
% Parameters are:
%
% - CreatedUnitType is the type of the just created unit
%
% - CreatedUnitConstructionParameters is the construction parameters
% corresponding to this new creation
%
% - CreatedUnitPid the PID of the just created unit
%
% - EventId is the identifier of the corresponding overall world event
%
% - CreationContext is the context of this creation
%
-spec onUnitCreated( wooper:state(), dataflow_unit_type(),
					 wooper:construction_parameters(), unit_pid(),
					 event_id(), building_pid() ) -> oneway_return().
onUnitCreated( State, _CreatedUnitType=class_EnergyDemandUnit,
			   CreatedUnitConstructParams, CreatedEnergyUnitPid,
			   EventId, _CreationContext=BuildingPid ) ->

	% We now link this new energy demand unit with first its downstream
	% building; the channel source (here, the unit) drives the channel creation,
	% based on actor messages (the channel target, here the building, will
	% ultimately call back the onChannelCreated/4 oneway of this unit manager):

	% A single channel from this (upstream) new unit to this (downstream)
	% building:
	%
	BuildingChannelEndpointNames =
		[ { "energy_needed", "total_energy_demand" } ],

	?debug_fmt( "Energy Demand Unit ~w created, from construction "
		"parameters '~p'; creating now channels towards its related "
		"building (~w), namely: ~p",
		[ CreatedEnergyUnitPid, CreatedUnitConstructParams,
		  BuildingPid, hd( BuildingChannelEndpointNames ) ] ),

	BuildingChannelState = class_DataflowUnitManager:create_channels_for(
								EventId, CreatedEnergyUnitPid, BuildingPid,
								BuildingChannelEndpointNames, State ),

	% We do not create the upstream connections of this energy demand unit
	% (created after a building is created) here, as they will done later, when
	% the households corresponding to this building will be in turn created.
	%
	% However, so that it can happen later, we record, for a given building, its
	% associated energy unit:

	NewTable = table:add_new_entry( BuildingPid, CreatedEnergyUnitPid,
									?getAttr(energy_table) ),

	FinalState = setAttribute( BuildingChannelState, energy_table, NewTable ),

	wooper:return_state( FinalState );


onUnitCreated( State, _CreatedUnitType=class_TransportationDemandUnit,
			   CreatedUnitConstructParams, CreatedTransportUnitPid,
			   EventId, _CreationContext=HouseholdPid ) ->

	% Here we do for an household and its transportation unit roughly what we do
	% for a building and its energy unit, except that the unit here is
	% downstream (compared to its related dataflow object) rather than upstream.

	% From the upstream household to this (downstream) new unit, port names
	% happen to be the same:
	%
	HouseholdChannelEndpointNames = [ "adult_count", "child_count" ],

	?debug_fmt( "Transportation Demand Unit ~w created, from construction "
		"parameters '~p'; creating now channels from its related "
		"household (~w), namely: ~ts",
		[ CreatedTransportUnitPid, CreatedUnitConstructParams,
		  HouseholdPid,
		  text_utils:strings_to_string( HouseholdChannelEndpointNames ) ] ),

	HouseholdChannelState = class_DataflowUnitManager:create_channels_for(
		EventId, HouseholdPid, CreatedTransportUnitPid,
		HouseholdChannelEndpointNames, State ),

	% We need also to create the upstream channel 'area_type' from the relevant
	% district to this unit.
	%
	% This district shall be the one in which the household of interest is
	% located.
	%
	% One way could have been to have this unit manager listen to the various
	% changes happening to the city from the start, in order to create its own
	% view of it (and thus, for example, to be able to find the right district).
	%
	% A more effective approach for this need is to have this unit manager
	% directly ask to this household what its district is.
	%
	% This could/should be done based on actor messages (the sole means for
	% actors to interact in the course of the simulation). However, for the sake
	% of simplicity, and knowing that by design the state of the disaggregated
	% city is stable during the phase driven by the experiment manager, we just
	% rely on basic, direct WOOPER messages here; we use an encapsulated,
	% household-level primitive, defined for re-use (among all other general
	% primitives defined by the disaggregated city in order to discover its
	% actual structure).
	%
	% Finally, the household could have directly known its district (which is
	% not a direct parent thereof), however it is more desirable not to
	% duplicate information, so the household will actually ask its building
	% (which then directly knows its parent district) and thus will be able to
	% answer.

	{ parent_district, DistrictPid } =
		wooper:execute_request( HouseholdPid, getParentDistrict ),

	% Here a single channel is to be created, with different names for its port
	% endpoints (from district to transportation unit):
	%
	DistrictChannelEndpointNames = [ { "type", "area_type" } ],

	?debug_fmt( "Creating now a channel from the parent district (~w) of this "
		"household (~w) to this transport unit (~w), the names of "
		"its port endpoints being ~p.",
		[ DistrictPid, HouseholdPid, CreatedTransportUnitPid,
		  hd( DistrictChannelEndpointNames ) ] ),

	% These creations will be managed transparently from then by the base class:
	DistrictChannelState = class_DataflowUnitManager:create_channels_for(
		EventId, DistrictPid, CreatedTransportUnitPid,
		DistrictChannelEndpointNames, HouseholdChannelState ),

	% Now it is time for the last connectivity-related action: to create the
	% downstream connections of this transportation unit.
	%
	% This example showcases a more complex case than the usual ones, as it
	% deviates from the recommended approach consisting on strictly alternating
	% dataflow objects and units: here the downstream block of this
	% transportation demand unit happens to be another unit, namely a energy
	% demand one.
	%
	% The question is: how this unit manager may obtain a reference (a PID) on
	% the targeted energy unit, from this point.
	%
	% This will be done by first locating its associated dataflow object (the
	% building linked to the target energy unit), and from that information
	% determining that energy unit (as it is this unit manager that created it).
	%
	% To do so, we chose to look-up first the building corresponding to this
	% household of interest, by requesting the disaggregated city for that (as
	% done with getParentDistrict/1 above).
	%
	{ parent_building, BuildingPid } =
		wooper:execute_request( HouseholdPid, getParentBuilding ),

	% So we now know the building corresponding to this household; last
	% operation needed is to obtain from that building its associated energy
	% unit.
	%
	% Of course an element of the disaggregated city (here, the household) is
	% not directly aware of the unit(s) operating on it (here the energy
	% unit).
	%
	% However this information can be known by this unit manager, since it is
	% the one that created this unit at the first place; we have just to ensure
	% it remembers it (when it created such unit) and return it here:
	%
	EnergyUnitPid = get_energy_unit_for( BuildingPid, DistrictChannelState ),

	% Here a single channel is to be created, from the transportation unit to
	% energy one; note that the second endpoint is not a standard port, but a
	% port iteration:
	%
	TransportChannelEndpointNames = [ { "energy_needed",
					{ input_iteration_name, "energy_demand" } } ],

	?debug_fmt( "Creating now a channel from this just created transportation "
		"unit (~w) to the energy demand unit (~w) corresponding to the parent "
		"building (~w), the names of its port endpoints being ~p.",
		[ CreatedTransportUnitPid, EnergyUnitPid, BuildingPid,
		  TransportChannelEndpointNames ] ),

	TransportChannelState = class_DataflowUnitManager:create_channels_for(
		EventId, CreatedTransportUnitPid, EnergyUnitPid,
		TransportChannelEndpointNames, DistrictChannelState ),

	% We finally record the household -> transportation unit association, so
	% that if the household is destructed we are able to destruct in turn its
	% associated transportation unit:
	%
	NewTransportTable = table:add_new_entry( HouseholdPid,
		CreatedTransportUnitPid, ?getAttr(transport_table) ),

	% Maybe clearer than using addKeyValueToAttribute/4:
	RecordedState = setAttribute( TransportChannelState,
								  transport_table, NewTransportTable ),

	wooper:return_state( RecordedState );


% Defined for convenience:
onUnitCreated( State, CreatedUnitType, CreatedUnitConstructParams,
			   CreatedEnergyUnitPid, EventId, CreationContext ) ->

	?error_fmt( "Unhandled unit creation, for type '~ts', for construction "
		"parameters ~p; the unit for event #~B was ~w "
		"(associated context being ~p).",
		[ CreatedUnitType, CreatedUnitConstructParams, EventId,
		  CreatedEnergyUnitPid, CreationContext ] ),

	throw( { unhandled_unit_creation, CreatedUnitType } ).



% @doc Returns a textual description of this unit manager.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	EnergyString = case table:enumerate( ?getAttr(energy_table) ) of

		[] ->
			"no known building/energy unit association";

		Pairs ->
			Strings = [ text_utils:format(
							"to building ~w is associated the energy unit ~w",
							[ BuildingPid, EnergyUnitPid ] )
						|| { BuildingPid, EnergyUnitPid } <- Pairs ],
			text_utils:format( "~B building/energy unit associations "
							   "are known: ~ts", [ length( Pairs ),
				text_utils:strings_to_string( Strings ) ] )

	end,

	text_utils:format( "Urban unit manager knowing ~ts", [ EnergyString ] ).



% @doc Returns the PID of the energy unit associated to specified building.
%
% (helper)
%
-spec get_energy_unit_for( building_pid(), wooper:state() ) ->
									energy_unit_pid().
get_energy_unit_for( BuildingPid, State ) ->
	table:get_value( BuildingPid, ?getAttr(energy_table) ).
