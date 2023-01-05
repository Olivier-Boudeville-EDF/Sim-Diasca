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


% @doc Example of transportation-related <b>dataflow unit</b>.
-module(class_TransportationDemandUnit).


-define( class_description,
		 "Example dataflow unit in terms of transportation demand, in the "
		 "context of the 'Dataflow Urban Example' case." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowProcessingUnit ] ).





% The attributes specific to this processing unit are:
-define( class_attributes, [

	{ vehicle_sharing_probability, math_utils:percent(), "is the likeliness of "
	  "resorting to vehicle sharing for a transportation need" },

	{ ambient_pollution, float(), "describes the ambient, overall pollution in "
	  "the city, in g.cm^-3 (mainly a way of showing that processing units may "
	  "be stateful)" } ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Urban-Example.Transportation" ).


% For types and shorthands:
-include("sim_diasca_for_actors.hrl").


% For types like energy_demand():
-include("urban_example_defines.hrl").



% Design notes:

% This unit is ruled by the 'activate_on_new_set' activation policy, hence it
% will be activated each time that one of its input ports is set.

% In order to illustrate the different ways of setting, reading or extracting a
% value, we consider here that at least the 'average_journey' input port must be
% set from outside that unit (typically through a channel, or possibly
% explicitly by the entry point). As a result, the value of this port will be
% consumed by this unit once read, i.e. each activation will result in having
% this port be in the unset status.
%
% The other input ports may or may not be set through the dataflow. If not, we
% prefer here that they rely on default values, that are to be assigned
% initially.
%
% Therefore the constructor of this unit sets these defaults that activations
% will not unset.


% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Constructs a dataflow unit instance, in charge of evaluating the need for
% transport:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - UnitName is a human-readable name for that unit instance (as a plain,
% non-empty string)
%
% - LevelOfVehicleSharing is a percentage describing the likeliness of resorting
% to vehicle sharing for a transportation need (useful to show that units are
% generally parametrised, i.e. that their instances may accept at least some
% degree of customisation)
%
% - DataflowPid is the PID of the dataflow instance
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_DataflowProcessingUnit:unit_name(), math_utils:percent(),
				 dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, UnitName, LevelOfVehicleSharing,
		   DataflowPid ) ->

	% All instances of that unit type at least have these three input ports:
	{ InputPortSpecs, OutputPortSpecs } = get_port_specifications(),

	% First the direct mother class:
	UnitState = class_DataflowProcessingUnit:construct( State, ActorSettings,
		?trace_categorize(UnitName), _ActivationPolicy=activate_on_new_set,
		InputPortSpecs, OutputPortSpecs, DataflowPid ),

	DefaultState = set_input_port_defaults( UnitState ),

	% Then the class-specific actions:
	setAttributes( DefaultState, [
		{ ambient_pollution, 0.115 },
		{ vehicle_sharing_probability, LevelOfVehicleSharing } ] ).



% @doc Sets the defaults for the input ports that may or may not be connected.
%
% (helper)
%
-spec set_input_port_defaults( wooper:state() ) -> wooper:state().
set_input_port_defaults( State ) ->

	DefaultAdultCountValue = class_Dataflow:create_channel_value( _AdultCount=1,
		[ ?adult_count_semantics ], "dimensionless", "integer" ),

	DefaultChildCountValue = class_Dataflow:create_channel_value( _ChildCount=0,
		[ ?child_count_semantics ], "dimensionless", "integer" ),

	% Previously, no defaults for 'average_journey' existed, was to be set from
	% outside; now:
	%
	DefaultAverageJourneyValue = class_Dataflow:create_channel_value(
		_AverageJourney=5.8, [ ?path_length_semantics ], "km", "float" ),

	DefaultAreaTypeValue = class_Dataflow:create_channel_value( _AreaType=urban,
		[ ?area_type_semantics ], "dimensionless", "area_type" ),

	PortPairs = [ { "adult_count",     DefaultAdultCountValue },
				  { "child_count",     DefaultChildCountValue },
				  { "average_journey", DefaultAverageJourneyValue },
				  { "area_type",       DefaultAreaTypeValue   } ],

	% Updated state returned:
	class_DataflowBlock:set_input_port_values( PortPairs, State ).




% Methods section.



% @doc Callback executed automatically whenever the unit is activated.
%
% Meant to be overridden.
%
-spec activate( wooper:state() ) -> oneway_return().
activate( State ) ->

	?info_fmt( "Evaluating now the transportation demand for ~ts.",
			   [ to_string( State ) ] ),

	% We are activated, hence we must have at least one input port set.
	%
	% So we iterate through all input ports (in their declaration order) in
	% order to have their respective values.
	%
	% We had defaults here; now that they are set at construction-time and read
	% (not extracted - hence never reset), not having a value for them is an
	% error:
	%
	AdultCount = case class_DataflowBlock:get_input_port_status(
						"adult_count", State ) of

		unset ->
			throw( { unset_input_port, "adult_count" } );

		{ set, ReadAdultCount } ->
			%?debug_fmt( "Read value ~B for the set 'adult_count' input port.",
			%            [ ReadAdultCount ] ),
			ReadAdultCount

	end,

	ChildCount = case
			class_DataflowBlock:get_input_port_status( "child_count", State ) of

		unset ->
			throw( { unset_input_port, "child_count" } );

		{ set, ReadChildCount } ->
			%?debug_fmt( "Read value ~B for the set 'child_count' input port.",
			%   [ ReadChildCount ] ),
			ReadChildCount

	end,


	% Previously, was the only input port whose value was extracted, i.e. that
	% shall be unset after having been read; now, for the sake of simplicity (of
	% test cases), it is simply read, like the other input ports:

	%InputPortTable = ?getAttr(input_ports),
	%
	%{ AverageJourney, JourneyPortTable } = case
	%       class_DataflowBlock:extract_input_port_value(
	%           "average_journey", InputPortTable, State ) of
	%
	%	port_already_unset ->
	%		throw( { unset_input_port, "average_journey" } );
	%
	%	ReadJourney={ _ReadAverageJourney, _ReadJourneyTable } ->
	%		%?debug_fmt( "Read value ~f for the set 'average_journey' input "
	%		%            "port.", [ ReadAverageJourney ] ),
	%		ReadJourney
	%
	%end,

	% Could be deferred, yet clearer that way:
	%ExtractState = setAttribute( State, input_ports, JourneyPortTable ),

	% Set initially and never extracted/reset, hence always set:

	AverageJourney = case class_DataflowBlock:get_input_port_status(
							"average_journey", State ) of

		unset ->
			throw( { unset_input_port, "average_journey" } );

		{ set, ReadAverageJourney } ->
			%?debug_fmt( "Read value ~B for the set 'average_journey' "
			%   "input port.", [ ReadAverageJourney ] ),
			ReadAverageJourney

	end,

	% Note that we read this value from a now obsolete version of the table
	% (does not matter here):
	%
	AreaType = case
			class_DataflowBlock:get_input_port_status( "area_type", State ) of

		unset ->
			throw( { unset_input_port, "area_type" } );

		{ set, ReadAreaType } ->
			%?debug_fmt( "Read value ~ts for the set 'area_type' input port.",
			%            [ ReadAreaType ] ),
			ReadAreaType

	end,

	AmbientPollution = ?getAttr(ambient_pollution),

	VehicleSharingProbability = ?getAttr(vehicle_sharing_probability),

	% Performing the actual domain-specific computations with no specific link
	% to the dataflow:
	%
	{ EnergyNeeded, PollutionExhausted } = compute_transportation_metrics(
		AdultCount, ChildCount, AverageJourney, AreaType,
		AmbientPollution, VehicleSharingProbability, State ),

	% Updating correspondingly the relevant output ports:

	EnergyValue = class_Dataflow:create_channel_value( EnergyNeeded,
						[ ?energy_demand_semantics ], "kW.h", "float" ),

	PollutionValue = class_Dataflow:create_channel_value( PollutionExhausted,
						[ ?pollution_emission_semantics ], "g.cm^-3", "float" ),

	SetState = class_DataflowBlock:set_output_port_values( [
		{ "energy_needed", EnergyValue },
		{ "pollution_exhausted", PollutionValue } ], State ),


	%FinalState = setAttribute( SetState, input_ports, JourneyPortTable ),
	FinalState = SetState,

	wooper:return_state( FinalState ).



% @doc The core of this transportation pseudo-model, i.e. the place where its
% actual domain-specific computations are done, from the dataflow-originating
% values.
%
% Note: this logic is pure, has strictly no link with anything related to a
% dataflow or even to the internal state of this unit.
%
% (helper)
%
-spec compute_transportation_metrics( adult_count(), child_count(),
		average_journey(), area_type(), pollution_level(),
		math_utils:probability(), wooper:state() ) ->
						{ energy_demand(), pollution_level() }.
compute_transportation_metrics( AdultCount, ChildCount, AverageJourney,
			   AreaType, AmbientPollution, VehicleSharingProbability, State ) ->

	EnergyNeeded = compute_energy_needed( AdultCount, ChildCount,
						  AverageJourney, AreaType, VehicleSharingProbability ),

	PollutionExhausted = compute_pollution_exhausted( AverageJourney, AreaType,
													  AmbientPollution ),

	?info_fmt( "Transportation demand evaluated for ~B adult(s), "
		"~B child(ren), an average journey of ~f kilometers and an area of "
		"type ~ts: energy needed is ~f kW.h, pollution exhausted is ~f g.cm^-3",
		[ AdultCount, ChildCount, AverageJourney, AreaType,
		  EnergyNeeded, PollutionExhausted ] ),

	{ EnergyNeeded, PollutionExhausted }.




% @doc Computes the energy needed for specified transportation.
%
% (helper)
%
-spec compute_energy_needed( adult_count(), child_count(), average_journey(),
		 area_type(), math_utils:probability() ) -> energy_demand().
compute_energy_needed( AdultCount, ChildCount, AverageJourney, _AreaType=rural,
					   _VehicleSharingProbability=0.0 ) ->
	( AdultCount + ChildCount ) * 1.12 * AverageJourney;

compute_energy_needed( AdultCount, ChildCount, AverageJourney, _AreaType=urban,
					   _VehicleSharingProbability=0.0 ) ->
	( AdultCount + ChildCount ) * 0.7 * AverageJourney;

compute_energy_needed( AdultCount, ChildCount, AverageJourney, _AreaType=rural,
					   VehicleSharingProbability ) ->
	( 1.4 * AdultCount + 1.2 * ChildCount ) * AverageJourney /
		( VehicleSharingProbability * ( AdultCount + ChildCount + 2 ) );

compute_energy_needed( AdultCount, ChildCount, AverageJourney, _AreaType=urban,
					   VehicleSharingProbability ) ->
	0.65 * compute_energy_needed( AdultCount, ChildCount, AverageJourney, rural,
								  VehicleSharingProbability ).



% @doc Computes the pollution exhausted because of specified transportation.
%
% (helper)
%
-spec compute_pollution_exhausted( average_journey(), area_type(),
								   pollution_level() ) -> pollution_level().
compute_pollution_exhausted( AverageJourney, _AreaType=rural,
							 AmbientPollution ) ->
	AverageJourney * 117.5 * AmbientPollution / ( 1.1 + AmbientPollution );

compute_pollution_exhausted( AverageJourney, _AreaType=urban,
							 AmbientPollution ) ->
	AverageJourney * 142.1 * AmbientPollution / ( 1.08 + AmbientPollution ).



% Static section.


% @doc Returns the specifications for the input and output ports of that
% dataflow block.
%
-spec get_port_specifications() ->
			static_return( { [ input_port_spec() ], [ output_port_spec() ] } ).
get_port_specifications() ->
	wooper:return_static(
		{ get_input_port_specs(), get_output_port_specs() } ).



% Returns a list of the specifications of the (initial) input ports for that
% dataflow block.
%
-spec get_input_port_specs() -> static_return( [ input_port_spec() ] ).
get_input_port_specs() ->

	AdultCountIPort = #input_port_spec{
		name="adult_count",
		comment="Adults have special transportation needs",
		value_semantics=[ ?adult_count_semantics ],
		value_unit="dimensionless",
		value_type_description="integer",
		value_constraints=[ positive ] },

	ChildCountIPort = #input_port_spec{
		name="child_count",
		comment="Children have special transportation needs",
		value_semantics=[ ?child_count_semantics ],
		value_unit="dimensionless",
		value_type_description="integer",
		value_constraints=[ positive ] },

	AverageJourneyIPort = #input_port_spec{
		name="average_journey",
		comment="The length of an average local journey",
		value_semantics=[ ?path_length_semantics ],
		value_unit="km",
		value_type_description="float",
		% A journey is deemed local iff it remains below 25 km:
		value_constraints=[ { lower_than, 25.0 } ] },

	AreaTypeIPort = #input_port_spec{
		name="area_type",
		comment="The type of area impacts the transportation needs",
		value_semantics=[ ?area_type_semantics ],
		value_unit="dimensionless",
		% A type directly defined by this unit:
		value_type_description="area_type" },

	wooper:return_static( [ AdultCountIPort, ChildCountIPort,
							AverageJourneyIPort, AreaTypeIPort ] ).



% @doc Returns a list of the specifications of the (initial) output ports for
% that unit.
%
-spec get_output_port_specs() -> static_return( [ output_port_spec() ] ).
get_output_port_specs() ->

	EnergyDemandOPort = #output_port_spec{
		name="energy_needed",
		comment="Energy needed for transportation with a reference vehicle",
		%value_semantics='Energy demand',
		value_semantics=[ ?energy_demand_semantics ],
		value_unit="kW.h",
		value_type_description="float",
		value_constraints=[ positive ] },

	PollutionOPort = #output_port_spec{
		name="pollution_exhausted",
		comment="Pollution exhausted by transportations with a "
				"reference vehicle",
		%value_semantics='Pollution emitted',
		value_semantics=[ ?pollution_emission_semantics ],
		value_unit="g.cm^-3",
		value_type_description="float",
		value_constraints=[ positive ] },

	wooper:return_static( [ EnergyDemandOPort, PollutionOPort ] ).



% @doc Returns the semantics statically declared by this processing unit.
%
% Defining this method allows to ensure that all the ports ever created by this
% processing unit will rely on user-level semantics among this explicitly stated
% list.
%
% Otherwise the list would be deduced from the initial port specifications, with
% no specific control.
%
-spec get_declared_semantics() -> static_return( user_vocabulary() ).
get_declared_semantics() ->
	wooper:return_static( [ ?adult_count_semantics, ?child_count_semantics,
		?path_length_semantics, ?area_type_semantics,
		?energy_demand_semantics, ?pollution_emission_semantics ] ).



% @doc Returns the types statically declared by this unit.
-spec get_declared_types() -> static_return( class_TypeServer:type_entries() ).
get_declared_types() ->
	wooper:return_static( [ { 'area_type', "'rural'|'urban'" } ] ).




% Helper functions.


% @doc Returns a textual description of this unit.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->
	text_utils:format( "Transportation demand unit with a probability of "
		"vehicle sharing of ~f% and an ambient pollution "
		"of ~f g.cm^-3; this is a ~ts",
		[ ?getAttr(vehicle_sharing_probability), ?getAttr(ambient_pollution),
		  class_DataflowProcessingUnit:to_string( State ) ] ).
