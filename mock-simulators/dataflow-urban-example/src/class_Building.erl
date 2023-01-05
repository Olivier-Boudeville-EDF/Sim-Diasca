% Copyright (C) 2016-2023 EDF R&D

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


% @doc Example of a <b>dataflow object</b>.
-module(class_Building).


-define( class_description,
		 "Example dataflow object corresponding to a building, in the context "
		 "of the 'Dataflow Urban Example' case." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowObject ] ).



% Plain (standard) attributes specific to a building object are:
-define( class_attributes, [

	{ district_pid, maybe( district_pid() ), "the PID of the parent  district "
	  "of this building (may not be set initially)" },

	{ households, [ household_pid() ],
	  "a list of the households that this building hosts (they live in it)" }

] ).



% Helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Urban-Example.Building" ).


% For types and shorthands:
-include("sim_diasca_for_actors.hrl").


% For energy_demand_semantics and all:
-include("urban_example_defines.hrl").


-type building_name() :: ustring().

-type postal_address() :: ustring().



% Dataflow attributes specific to a building object are defined in the
% get_dataflow_attributes_specs/0 static method.


% Note that the 'district_pid' and 'households' attributes could/should have
% been defined as, respectively, unique and multiple peers instead.


% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Constructs a dataflow building object instance, in charge of modelling
% the state of a building:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - BuildingName is the name of this building
%
% - PostalAddress is the postal address of this building
%
% - DistrictPid is the PID of the parent distruct of this building
%
% - DataflowPid is the PID of the dataflow instance
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 building_name(), [ postal_address() | district_pid() ],
				 dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, BuildingName,
		   [ PostalAddress, DistrictPid ], DataflowPid ) ->

	AttributeSpecs = get_dataflow_attribute_specs(),

	InitialAttributeValues = [ _TotalEnergyDemand=0.0,
							   _TotalPollutionExhausted=0.0,
							   PostalAddress,
							   _FloorCount=1 ],

	% First the direct mother class:
	ObjectState = class_DataflowObject:construct( State, ActorSettings,
		?trace_categorize(BuildingName), AttributeSpecs,
		InitialAttributeValues, _SpecForUniquePeers=[],
		_SpecForMultiplePeers=[], DataflowPid ),

	% Then the class-specific actions:
	setAttributes( ObjectState, [ { district_pid, DistrictPid },
								  { households, [] } ] ).



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	case ?getAttr(district_pid) of

		undefined ->
			ok;

		DistrictPid ->
			% Normal if no disassociation event was specified:
			?info_fmt( "Destructed, yet still referencing the parent "
						"district ~w.", [ DistrictPid ] )

	end,


	case ?getAttr(households) of

		[] ->
			ok;

		Households ->
			% Normal if no disassociation event was specified:
			?info_fmt( "Destructed, yet still referencing ~B households: ~w.",
						[ length( Households ), Households ] )

	end,

	% Then allow chaining:
	State.




% Member methods section.


% @doc Sets the parent district of this building: this building will be
% located-in the specified district.
%
-spec setDistrict( wooper:state(), district_pid(), sending_actor_pid() ) ->
							actor_oneway_return().
setDistrict( State, DistrictPid, _SendingActorPid )
								when is_pid( DistrictPid ) ->

	% No reassignment permitted:
	undefined = ?getAttr(district_pid),

	?debug_fmt( "Setting district to ~p.", [ DistrictPid ] ),

	NewState = setAttribute( State, district_pid, DistrictPid ),

	actor:return_state( NewState ).



% @doc Registers specified household to this building: the specified household
% will live-in this building.
%
-spec registerHousehold( wooper:state(), household_pid(),
						 sending_actor_pid() ) -> actor_oneway_return().
registerHousehold( State, HouseholdPid, _SendingActorPid )
  when is_pid( HouseholdPid ) ->

	% Check that registered up to once:
	false = lists:member( HouseholdPid, ?getAttr(households) ),

	?debug_fmt( "Registering household ~p.", [ HouseholdPid ] ),

	NewState = appendToAttribute( State, households, HouseholdPid ),

	actor:return_state( NewState ).



% @doc Unregisters specified household from this building: the specified
% household will no longer live-in this building.
%
-spec unregisterHousehold( wooper:state(), household_pid(),
						   sending_actor_pid() ) -> actor_oneway_return().
unregisterHousehold( State, HouseholdPid, _SendingActorPid )
  when is_pid( HouseholdPid ) ->

	?info_fmt( "Unregistering household ~p.", [ HouseholdPid ] ),

	NewHouseholds = list_utils:delete_existing( HouseholdPid,
												?getAttr(households) ),

	NewState = setAttribute( State, households, NewHouseholds ),

	actor:return_state( NewState ).



% @doc Returns the (indirect) parent district of this building.
-spec getParentDistrict( wooper:state() ) ->
				const_request_return( { 'parent_district', district_pid() } ).
getParentDistrict( State ) ->
	ParentDistrict = ?getAttr(district_pid),
	wooper:const_return_result( { parent_district, ParentDistrict } ).




% Static section.


% @doc Allows to fully specify the dataflow attributes of this object.
-spec get_dataflow_attribute_specs() ->
								static_return( [ dataflow_attribute_spec() ] ).
get_dataflow_attribute_specs() ->
	wooper:return_static( [

	 #dataflow_attribute_spec{
		attribute_name="total_energy_demand",
		semantics=[ ?energy_demand_semantics ],
		unit="kW.h",
		type_description="float",
		constraints=[ positive ] },

	 #dataflow_attribute_spec{
		attribute_name="total_pollution_exhausted",
		% To test whether a value lacking a semantics is indeed rejected:
		%semantics=[ ?pollution_emission_semantics ],
		semantics=[ ?pollution_emission_semantics, ?extra_semantics ],
		%unit="g.cm^-3",
		% To test how unknown units are managed:
		%unit="teqCO2/year",
		%unit="teqCO2 ff ff /year",
		unit="IAmTheStrangestUnit/year",
		type_description="float",
		constraints=[ positive ] },

	 % Default empty constraints apply:
	 #dataflow_attribute_spec{
		attribute_name="postal_address",
		semantics=[ ?address_semantics ],
		unit="dimensionless",
		type_description="string" },

	 #dataflow_attribute_spec{
		attribute_name="number_of_floors",
		% Specifying the same semantics multiple times is permitted:
		semantics=[ ?floor_count_semantics, ?floor_count_semantics ],
		unit="dimensionless",
		type_description="integer",
		constraints=[] }

	] ).




% Helper section.


% @doc Returns a textual description of this building dataflow object.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	HouseholdString = case ?getAttr(households) of

		[] ->
			"no household";

		Households ->
			text_utils:format( "~B households (~w)",
							   [ length( Households ), Households ] )

	end,

	ParentString = case ?getAttr(district_pid) of

		undefined ->
			"no parent district";

		DistrictPid ->
			text_utils:format( "for parent district ~w", [ DistrictPid ] )

	end,

	text_utils:format( "Building object named '~ts', hosting ~ts, "
		"having ~ts, and having ~ts",
		[ ?getAttr(name), HouseholdString, ParentString,
		  class_DataflowObject:attributes_to_string( State ) ] ).
