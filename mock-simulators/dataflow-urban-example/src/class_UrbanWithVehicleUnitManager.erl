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


-module(class_UrbanWithVehicleUnitManager).


-define( class_description,
		 "This test unit manager is in charge of the demand-related units both "
		 "for transportation and energy; it is thus a joint unit manager." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowUnitManager ] ).


% Attributes that are specific to this unit manager singleton are:
-define( class_attributes, [


  { transport_table, table:table( household_pid(), transportation_unit_pid() ),
	"table telling, for an household PID, the PID of its associated "
	"transportation unit (if any)" },

  { energy_table, table:table( building_pid(), energy_unit_pid() ),
	"table telling, for a building PID, the PID of its associated energy unit "
	"(if any)" } ] ).


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



% Constructs an urban unit manager, from:
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

	% The first unit is implicitly implemented in Erlang:
	ManagedUnitSpec = [ class_TransportationDemandUnit,
						{ class_VehicleTypeUnit, python },
						{ class_EnergyDemandUnit, erlang } ],

	ListenedEventMatches = get_listened_event_matches(),

	EmitterName = ?MODULE,

	BaseState = class_DataflowUnitManager:construct( State, ActorSettings,
			?trace_categorize( EmitterName ), ManagedUnitSpec,
			ListenedEventMatches, ExperimentManagerPid, BindingManagers,
			LoadBalancerPid, IdentificationServerPid ),

	EmptyTable = table:new(),

	setAttributes( BaseState, [ { transport_table, EmptyTable },
								{ energy_table, EmptyTable } ] ).




% Methods section.


% Returns the synchronization event matches that this unit manager is interested
% in.
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

	BuildingCreationMatch = #creation_event_match{
			  object_type_match=class_Building },

	BuildingAssociationMatch = #binary_association_event_match{
			  association_type_match=located_in_district,
			  source_object_type_match=class_Building
			  % Implied: target_object_type_match=class_District
								 },

	% A lot more precise than a simple 'any_event_type':
	wooper:return_static( [ HouseholdCreationMatch, HouseholdAssociationMatch,
							BuildingCreationMatch, BuildingAssociationMatch ] ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a building or a household has just been created.
%
% As a result, if the created dataflow object is:
%
% - a building, then ultimately (over diascas) a EnergyDemandUnit is created and
% connected appropriately
%
% - a household, then ultimately (over diascas) a TransportationDemandUnit is
% created and connected appropriately
%
% (overridden oneway)
%
-spec onCreationEventMatched( wooper:state(), creation_event() ) ->
									oneway_return().
onCreationEventMatched( State, #creation_event{
								  id=EventId,
								  object_type=class_Building,
								  object_pid=BuildingPid,
								  construction_parameters=ConstructParams,
								  dataflow_pid=DataflowPid } ) ->

	% Here we create with relevant parameters a vehicle-type unit corresponding
	% to this building:

	BuildingName = hd( ConstructParams ),

	UnitName = text_utils:format( "Vehicle-type unit for the '~s' building",
								  [ BuildingName ] ),

	?debug_fmt( "Reacting to the creation of a building (~w; event #~B) "
				"by creating in turn an associated vehicle type unit, "
				"to be named '~s'.", [ BuildingPid, EventId, UnitName ] ),

	% In the same dataflow:
	UnitConstructParams = [ UnitName, _Year=2020, 0.5, 1.0, DataflowPid ],

	% Will trigger back a call to onUnitCreated/6:
	CreatedState = executeOneway( State, createUnit,
			 [ { class_VehicleTypeUnit, python }, UnitConstructParams, EventId,
			   _Context=undefined ] ),

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

	UnitName = text_utils:format( "Transportation Demand unit for the '~s' "
								  "household", [ HouseholdName ] ),

	?debug_fmt( "Reacting to the creation of an household (~w; event #~B) "
				"by creating in turn its associated transportation unit, "
				"to be named '~s'.", [ HouseholdPid, EventId, UnitName ] ),

	% Constant 10% here:
	LevelOfVehicleSharing = 0.1,

	% In the same dataflow:
	UnitConstructParams = [ UnitName, LevelOfVehicleSharing, DataflowPid ],

	% Will trigger back a call to onUnitCreated/6:
	CreatedState = executeOneway( State, createUnit,
			 [ class_TransportationDemandUnit, UnitConstructParams, EventId,
			   _Context=HouseholdPid ] ),

	% The household -> transportation unit association will be recorded in that
	% callback, as we need the PID of that unit.

	wooper:return_state( CreatedState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a binary association event has been received and successfully
% matched against a clause specified by this unit manager.
%
% (overridden oneway)
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

	%trace_utils:debug_fmt( "association: household ~p living in "
	%  "building ~p: ~p", [ HouseholdPid, BuildingPid, BinAssociationEvent ] ),

	?debug_fmt( "Reacting to the association of an household (event #~B): ~s",
				[ EventId, dataflow_support:world_event_to_string(
					BinAssociationEvent ) ] ),

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

	%trace_utils:debug_fmt( "association: building ~p located in "
	%  "district ~p: ~p~n", [ BuildingPid, DistrictPid, BinAssociationEvent ] ),

	?debug_fmt( "Reacting to the association of an household (event #~B): ~s",
				[ EventId, dataflow_support:world_event_to_string(
					BinAssociationEvent ) ] ),

	wooper:const_return().



% Notifies this unit manager how a dataflow-level event shall be handled, not
% relying on the changeset system for that (just useful in the context of a
% programmatic case).
%
-spec notifyEvent( wooper:state(), urban_dataflow_event(), event_data(),
				   sending_actor_pid() ) -> actor_oneway_return().
notifyEvent( State, _Event=new_energy_demand_unit_needed, UnitName,
			 _SendingActorPid ) ->

	?debug_fmt( "Creating a new energy demand unit, named '~s'.",
				[ UnitName ] ),

	CreatedState = class_DataflowUnitManager:create_runtime_unit(
					 _UnitClassname={ erlang, class_EnergyDemandUnit },
					 _CoreConstructionParameters=[ UnitName ], State ),

	actor:return_state( CreatedState ).




% Called whenever a unit has been created, so that channels between this new
% unit and the rest of the dataflow can be created.
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
				"household (~w), namely: ~s",
				[ CreatedTransportUnitPid, CreatedUnitConstructParams,
				  HouseholdPid, text_utils:strings_to_string(
								  HouseholdChannelEndpointNames ) ] ),

	HouseholdChannelState = class_DataflowUnitManager:create_channels_for(
							  EventId, HouseholdPid, CreatedTransportUnitPid,
							  HouseholdChannelEndpointNames, State ),

	wooper:return_state( HouseholdChannelState );



onUnitCreated( State, _CreatedUnitType=class_VehicleTypeUnit,
			   CreatedUnitConstructParams, CreatedVehicleTypeUnitPid,
			   EventId, _CreationContext ) ->

	?debug_fmt( "VehicleType Unit ~w created, from construction "
				"parameters '~p', for event #~B.",
				[ CreatedVehicleTypeUnitPid, CreatedUnitConstructParams,
				  EventId ] ),

	wooper:const_return();



% Defined for convenience:
onUnitCreated( State, CreatedUnitType, CreatedUnitConstructParams,
			   CreatedEnergyUnitPid, EventId, CreationContext ) ->

	?error_fmt( "Unhandled unit creation, for type '~s', "
				"for construction parameters ~p; the unit for event #~B "
				"was ~w (associated context being ~p).",
				[ CreatedUnitType, CreatedUnitConstructParams, EventId,
				  CreatedEnergyUnitPid, CreationContext ] ),

	throw( { unhandled_unit_creation, CreatedUnitType } ).




% Returns a textual description of this unit manager
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	EnergyString = case table:enumerate( ?getAttr(energy_table) ) of

		[] ->
			"no known building/energy unit association";

		Pairs ->
			Strings = [ text_utils:format( "to building ~w is associated "
										   "the energy unit ~w",
										   [ BuildingPid, EnergyUnitPid ] )
						|| { BuildingPid, EnergyUnitPid } <- Pairs ],
			text_utils:format( "~B building/energy unit associations "
							   "are known: ~s", [ length( Pairs ),
				text_utils:strings_to_string( Strings ) ] )

	end,

	text_utils:format( "Urban unit manager knowing ~s", [ EnergyString ] ).
