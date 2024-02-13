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


% @doc Example <b>dataflow object</b>.
-module(class_Household).


-define( class_description,
		 "Example dataflow object corresponding to a household, in the context "
		 "of the 'Dataflow Urban Example' case." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowObject ] ).


% The plain (standard) attributes specific to a household object are:
-define( class_attributes, [

	{ building_pid, maybe( building_pid() ), "the PID of the parent building "
	  "hosting that household (i.e. comprising its dwelling)" } ] ).


% Helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Urban-Example.Household" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% For energy_demand_semantics and all:
-include("urban_example_defines.hrl").


-type household_name() :: ustring().
-type family_name() :: ustring().
-type income() :: float().
-type distance() :: float().



% Dataflow attributes specific to a household object are defined in the
% get_dataflow_attributes_specs/0 static method.

% Note that the 'building_pid' attribute could/should have been defined as a
% unique peer instead.


% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Constructs a dataflow household object instance, in charge of modelling
% the state of a household:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - HouseholdName is the name of this household
%
% - FamilyName is the name of the family corresponding to this household
%
% - AdultCount is the number of adults in this household
%
% - ChildCount is the number of children in this household
%
% - DisposableIncome is the disponible income of this household
%
% - MeanDistanceCovered is the mean distance covered by the members of this
% household
%
% - BuildingPid is the PID of the building that hosts this household
%
% - DataflowPid is the PID of the dataflow instance
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
	household_name(),
	[ family_name() | adult_count() | child_count() | income()
	  | distance() | building_pid() ],
	dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, HouseholdName,
		   [ FamilyName, AdultCount, ChildCount, DisposableIncome,
			 MeanDistanceCovered, BuildingPid ], DataflowPid ) ->

	AttributeSpecs = get_dataflow_attribute_specs(),

	InitialAttributeValues = [ FamilyName, AdultCount, ChildCount,
							   DisposableIncome, MeanDistanceCovered ],

	% First the direct mother class:
	ObjectState = class_DataflowObject:construct( State, ActorSettings,
		?trace_categorize(HouseholdName), AttributeSpecs,
		InitialAttributeValues, _SpecForUniquePeers=[],
		_SpecForMultiplePeers=[], DataflowPid ),

	% Then the class-specific actions:
	setAttribute( ObjectState, building_pid, BuildingPid ).



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	case ?getAttr(building_pid) of

		undefined ->
			ok;

		BuildingPid ->
			% Normal if no disassociation event was specified:
			?info_fmt( "Destructed, yet still referencing the parent "
					   "building ~w.", [ BuildingPid ] )

	end,

	% Then allow chaining:
	State.



% Member methods section.


% @doc Sets the parent building of this household: this household will live-in
% this building.
%
-spec setBuilding( wooper:state(), building_pid(), sending_actor_pid() ) ->
						 actor_oneway_return().
setBuilding( State, BuildingPid, _SendingActorPid )
						when is_pid( BuildingPid ) ->

	% No reassignment permitted:
	undefined = ?getAttr(building_pid),

	?info_fmt( "Setting parent building to ~w.", [ BuildingPid ] ),

	NewState = setAttribute( State, building_pid, BuildingPid ),

	actor:return_state( NewState ).



% @doc Unsets the parent building of this household: this household will no more
% live-in in this building.
%
-spec unsetBuilding( wooper:state(), building_pid(), sending_actor_pid() ) ->
							actor_oneway_return().
unsetBuilding( State, BuildingPid, _SendingActorPid )
  when is_pid( BuildingPid ) ->

	% Check:
	BuildingPid = ?getAttr(building_pid),

	?info_fmt( "Unsetting parent building (was ~w).", [ BuildingPid ] ),

	NewState = setAttribute( State, building_pid, undefined ),

	actor:return_state( NewState ).



% Static section.


% @doc Allows to fully specify the dataflow attributes of this object.
-spec get_dataflow_attribute_specs() ->
						  static_return( [ dataflow_attribute_spec() ] ).
get_dataflow_attribute_specs() ->

	wooper:return_static( [

	 #dataflow_attribute_spec{
		attribute_name="family_name",
		semantics=[ ?name_semantics ],
		unit="dimensionless",
		type_description="string" },

	 #dataflow_attribute_spec{
		attribute_name="adult_count",
		semantics=[ ?adult_count_semantics ],
		unit="dimensionless",
		type_description="integer",
		constraints = [ positive ] },

	 #dataflow_attribute_spec{
		attribute_name="child_count",
		semantics=[ ?child_count_semantics ],
		unit="dimensionless",
		type_description="integer",
		constraints = [ positive ] },

	 #dataflow_attribute_spec{
		attribute_name="disposable_income",
		semantics=[ ?income_semantics ],
		unit="dimensionless",
		type_description="float" },

	 #dataflow_attribute_spec{
		attribute_name="mean_distance_covered",
		semantics=[ ?path_length_semantics ],
		unit="km",
		type_description="float" } ] ).




% Helper section.


% @doc Returns the (indirect) parent district of this household.
-spec getParentDistrict( wooper:state() ) ->
				const_request_return( { 'parent_district', district_pid() } ).
getParentDistrict( State ) ->

	Res = case ?getAttr(building_pid) of

		undefined ->
			throw( no_parent_building );

		ParentBuildingPid ->
			{ parent_district, _DistrictPid } =
				wooper:execute_request( ParentBuildingPid, getParentDistrict )

	end,

	wooper:const_return_result( Res ).



% @doc Returns the (direct) parent building of this household.
-spec getParentBuilding( wooper:state() ) ->
		const_request_return( { 'parent_building', maybe( building_pid() ) } ).
getParentBuilding( State ) ->

	% Possibly 'undefined':
	ParentBuildingPid = ?getAttr(building_pid),

	wooper:const_return_result( { parent_building, ParentBuildingPid } ).



% @doc Returns a textual description of this household dataflow object.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	BuildingString = case ?getAttr(building_pid) of

		undefined ->
			"not living in any building";

		BuildingPid ->
			text_utils:format( "living in building ~w", [ BuildingPid ] )

	end,

	text_utils:format( "Household object named '~ts', ~ts and having ~ts",
		[ ?getAttr(name), BuildingString,
		  class_DataflowObject:attributes_to_string( State ) ] ).
