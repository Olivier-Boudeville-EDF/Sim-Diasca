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


-module(class_District).


-define( class_description,
		 "Example dataflow object corresponding to a district, in the context "
		 "of the 'Dataflow Urban Example' case." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowObject ] ).


-type administrative_name() :: string().
-type surface() :: float().

% Plain (standard) attributes specific to a district object are:
-define( class_attributes, [

	{ districts,  [ building_pid() ], "a list of the buildings that this "
	  "district contains (not owning them)" } ] ).



% Helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Urban-Example.District" ).


% For types and shorthands:
-include("sim_diasca_for_actors.hrl").


% For energy_demand_semantics and all:
-include("urban_example_defines.hrl").



% Dataflow attributes specific to a district object are defined in the
% get_dataflow_attributes_specs/0 static method.

% Note that the 'buildings' attribute could/should have been defined as a
% multiple peer instead.




% Constructs a new dataflow district object instance, in charge of modelling the
% state of a district:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - DistrictName is the name of this district
%
% - AdministrativeName is the administrative name of this district
%
% - GroundSurface is the total ground surface of this district
%
% - Type is the rural/urban typology of this district
%
% - DataflowPid is the PID of the dataflow instance
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(),
				 [ administrative_name() | surface() | area_type() ],
				 dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, DistrictName,
		   [ AdministrativeName, GroundSurface, Type ], DataflowPid ) ->

	AttributeSpecs = get_dataflow_attribute_specs(),

	InitialAttributeValues = [ AdministrativeName, GroundSurface, Type ],

	% First the direct mother class:
	ObjectState = class_DataflowObject:construct( State, ActorSettings,
		?trace_categorize( DistrictName ), AttributeSpecs,
		InitialAttributeValues, _SpecForUniquePeers=[],
		_SpecForMultiplePeers=[], DataflowPid ),

	% Then the class-specific actions:
	setAttribute( ObjectState, buildings, [] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	case ?getAttr(buildings) of

		[] ->
			ok;

		Buildings ->
			% Normal if no disassociation event was specified:
			?info_fmt( "Destructed, yet still referencing ~B buildings: ~w.",
					   [ length( Buildings ), Buildings ] )

	end,

	% Then allow chaining:
	State.




% Member methods section.



% Registers specified building to this district: the specified building is
% located-in this district.
%
-spec registerBuilding( wooper:state(), building_pid(), sending_actor_pid() ) ->
						 actor_oneway_return().
registerBuilding( State, BuildingPid, _SendingActorPid )
  when is_pid( BuildingPid ) ->

	% Check that registered up to once:
	false = lists:member( BuildingPid, ?getAttr(buildings) ),

	?debug_fmt( "Registering building ~p.", [ BuildingPid ] ),

	NewState = appendToAttribute( State, buildings, BuildingPid ),

	actor:return_state( NewState ).




% Static section.


% Allows to fully specify the dataflow attributes of this object.
-spec get_dataflow_attribute_specs() ->
						  static_return( [ dataflow_attribute_spec() ] ).
get_dataflow_attribute_specs() ->
	wooper:return_static( [

	 #dataflow_attribute_spec{
		attribute_name="administrative_name",
		semantics=[ ?name_semantics ],
		unit="dimensionless",
		type_description="string" },

	 #dataflow_attribute_spec{
		attribute_name="ground_surface",
		semantics=[ ?surface_semantics ],
		unit="m^2",
		type_description="float",
		constraints = [ positive ] },

	 #dataflow_attribute_spec{
		attribute_name="type",
		semantics=[ ?area_type_semantics ],
		unit="dimensionless",
		type_description="area_type",
		constraints = [ positive ] } ] ).




% Helper section.


% Returns a textual description of this district dataflow object.
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	BuildingString = case ?getAttr(buildings) of

		[] ->
			"no building";

		Buildings ->
			text_utils:format( "~B buildings (~w)",
							   [ length( Buildings ), Buildings ] )

	end,

	text_utils:format( "District object named '~s', containing ~s and "
					   "having ~s",
					   [ ?getAttr(name), BuildingString,
						 class_DataflowObject:attributes_to_string( State ) ] ).
