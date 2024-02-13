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

% Author: Robin Huart [robin-externe (dot) huart (at) edf (dot) fr]


% @doc Example of a <b>dataflow unit</b> regarding vehicle types.
-module(class_VehicleTypeUnit).


-define( class_description,
		 "Example of dataflow unit simulating the effects of the choice of "
		 "a particular vehicle by some households on their overall energy "
		 "demand and pollution emissions, in the context of the 'Dataflow "
		 "Urban Example' case. Instead of multiplying the units, whenever "
		 "2 households own the same vehicle (or rather the same vehicle type), "
		 "we aggregate their consumptions and emissions inside a same unit "
		 "thanks to port iterations." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowProcessingUnit ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Urban-Example.VehicleType" ).


% For types and shorthands:
-include("sim_diasca_for_actors.hrl").


% For types like energy_demand():
-include("urban_example_defines.hrl").


-type efficiency() :: float().


% The class-specific attributes of a vehicle type unit are:
-define( class_attributes, [

	{ age, years(), "the age of the vehicle" },

	{ origin_year, year(), "the year at which the car was made " },

	{ energy_efficiency, efficiency(), "a constant coefficient modifying the "
	  "energy consumption of households per vehicle" },

	{ pollution_ref_efficiency, efficiency(), "another coefficient "
	  "(an efficiency of reference) modifying per vehicle the quantity of "
	  "pollution emitted by households" } ] ).



% Design notes:

% This unit is ruled by the 'activate_when_all_set' activation policy, hence it
% will be activated each time that all of its input ports are set.

% This unit defines two port iterations, to accommodate any number of upstream
% transportation processing units (each one corresponding to one household).
% The number of input iterations for energy and pollution are the same, since
% they must be equal to the number of households.
%
% In this pseudo-model, every household is supposed to own exactly one
% vehicle. Since all vehicles of a same type are supposed to share the same
% behaviour, all households owning the same type of vehicle are to connect to
% the same instance of this unit. Thus, the output ports store computational
% results (still the energy demand and the pollution emissions) aggregated from
% all the vehicles of a same type.

% The iteration starts with no specific iterated port created; they are added
% on demand, typically by their unit manager.


% The input pollution emitted is modified by a coefficient (pollution
% efficiency) that depends on this last attribute and on the age of the
% vehicle, hence the 'ref' in 'pollution_ref_efficiency'.


% Shorthands:

-type ustring() :: text_utils:ustring().

-type year() :: unit_utils:year().



% @doc Constructs a dataflow unit instance, in charge of evaluating the impacts
% of the vehicle type on the need for energy and on the pollution emissions:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - UnitName is a human-readable name for that unit instance (as a plain,
% non-empty string)
%
% - YearOfOrigin, EnergyEfficiency and PollutionRefEfficiency correspond
% respectively to the attributes origin_year, energy_efficiency and
% pollution_ref_efficiency presented above
%
% - DataflowPid is the PID of the dataflow instance
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		class_DataflowProcessingUnit:unit_name(), year(),
		efficiency(), efficiency(), dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, UnitName, YearOfOrigin, EnergyEfficiency,
		   PollutionRefEfficiency, DataflowPid ) ->

	% We start with two input iterations and two outputs:
	{ InputPortSpecs, OutputPortSpecs } = get_port_specifications(),

	% First the direct mother class:
	UnitState = class_DataflowProcessingUnit:construct(
		State, ActorSettings, ?trace_categorize(UnitName),
		_ActivationPolicy=activate_when_all_set, InputPortSpecs,
		OutputPortSpecs, DataflowPid ),

	% Then the class-specific actions:
	setAttributes( UnitState, [
		{ age, 0 },
		{ origin_year, YearOfOrigin },
		{ energy_efficiency, EnergyEfficiency },
		{ pollution_ref_efficiency, PollutionRefEfficiency } ] ).




% Methods section.


% @doc Callback executed automatically whenever this unit gets activated.
%
% Meant to be overridden.
%
-spec activate( wooper:state() ) -> oneway_return().
activate( State ) ->

	% First update the age of the vehicle:
	OriginYear = ?getAttr(origin_year),

	{ _State, { { CurrentYear, _Month, _Day }, { _Hour, _Minute, _Second} } } =
		executeRequest( State, getSimulationDate ),

	AgedState = setAttribute( State, age, CurrentYear - OriginYear ),

	?info_fmt( "Evaluating the effects of a vehicle type on the energy "
		"consumptions and the pollution emissions for ~ts.",
		[ to_string( AgedState ) ] ),

	% Here we enter the actual unit; currently, internally we manage values
	% directly, bypassing any provision in terms of type, unit and all:
	%
	% (this computation could be done in any other module or programming
	% language)

	% Let's aggregate all the estimated energy demands:
	EnergyEstimates = class_DataflowBlock:get_all_input_iteration_values(
						  "energy_demand_estimates", AgedState ),

	% And also all the estimated pollution emitted:
	PollutionEstimates = class_DataflowBlock:get_all_input_iteration_values(
							"pollution_estimates", AgedState ),

	% Finally, we can determine the actual impacts of the vehicle(s) in terms of
	% energy demand and pollution:
	%
	{ ActualEnergyDemand, ActualPollutionEmission } = compute_vehicle_effects(
				EnergyEstimates, PollutionEstimates, AgedState ),

	% Updating correspondingly the relevant output ports:
	%
	EnergyOut = class_Dataflow:create_channel_value( ActualEnergyDemand,
					[ ?energy_demand_semantics ], "kW.h", "float" ),

	PollutionOut = class_Dataflow:create_channel_value( ActualPollutionEmission,
					[ ?pollution_emission_semantics ], "g.cm^-3", "float" ),

	SetState = class_DataflowBlock:set_output_port_values( [
				{ "actual_energy_need", EnergyOut },
				{ "actual_pollution", PollutionOut } ], AgedState ),

	wooper:return_state( SetState ).



% @doc This is the core of this "vehicle efficiency" pseudo-model, the function
% where its actual domain-specific computations are performed from the
% dataflow-originating values.
%
% Note: this logic is pure, has strictly no link with anything related to a
% dataflow or even to the internal state of this unit.
%
% (helper)
%
-spec compute_vehicle_effects( energy_demand(), pollution_level(),
			wooper:state() ) -> { energy_demand(), pollution_level() }.
compute_vehicle_effects( EnergyEstimates, PollutionEstimates, State ) ->

	% Effect 1: energy efficiency
	% (rule using a flat, constant coefficient)

	EnergyEfficiency = ?getAttr(energy_efficiency),
	ActualEnergyDemand = lists:sum( EnergyEstimates ) * EnergyEfficiency,


	% Effect 2: pollution emission
	% (rule using a coefficient increasing with age)

	PollutionRefEfficiency = ?getAttr(pollution_ref_efficiency),
	CurrentAge = ?getAttr(age),

	ActualPollution = lists:sum( PollutionEstimates ) *
						  PollutionRefEfficiency * ( 1 + 0.03*CurrentAge ),

	?info_fmt( "Impacts of all the vehicle(s) of type '~ts': energy needed is "
		"~f kW.h, pollution emitted is ~f g.cm^-3",
		[ ?getAttr(name), ActualEnergyDemand, ActualPollution ] ),

	{ ActualEnergyDemand, ActualPollution }.



% @doc Returns the specifications for the input and output ports of that
% dataflow processing unit.
%
-spec get_port_specifications() ->
		static_return( { [ input_port_spec() ], [ output_port_spec() ] } ).
get_port_specifications() ->
	wooper:return_static(
	  { get_input_port_specs(), get_output_port_specs() } ).



% @doc Returns a list of the specifications of the (initial) input ports for
% that dataflow block.
%
-spec get_input_port_specs() -> static_return( [ input_port_spec() ] ).
get_input_port_specs() ->

	% These are static information (defined prior to knowing anything about the
	% case), so we start with an empty iteration (no initial instance known
	% yet):
	%
	EnergyIterationIPort = #input_port_spec{
		name="energy_demand_estimates",
		comment="Each of these iterated ports tracks a source of (estimated) "
				"energy demand",
		% Min and current instance count: 0, max is unbounded:
		is_iteration=0,
		value_semantics=[ ?energy_demand_semantics ],
		value_unit="kW.h",
		value_type_description="float",
		value_constraints=[ positive ] },

	PollutionIterationIPort = #input_port_spec{
		name="pollution_estimates",
		comment="Each of these iterated ports tracks a source of (estimated) "
				"pollution",
		% Min and current instance count: 0, max is unbounded:
		is_iteration=0,
		value_semantics=[ ?pollution_emission_semantics ],
		value_unit="g.cm^-3",
		value_type_description="float",
		value_constraints=[ positive ] },

	wooper:return_static(
	    [ EnergyIterationIPort, PollutionIterationIPort ] ).



% @doc Returns a list of the specifications of the (initial) output ports for
% that unit.
%
-spec get_output_port_specs() -> static_return( [ output_port_spec() ] ).
get_output_port_specs() ->

	ActualEnergyOPort = #output_port_spec{
		name="actual_energy_need",
		comment="Aggregated energy need of households sharing the same "
				"vehicle type",
		value_semantics= [ ?energy_demand_semantics ],
		value_unit="kW.h",
		value_type_description="float",
		value_constraints=[ positive ] },

	ActualPollutionOPort = #output_port_spec{
		name="actual_pollution",
		comment="Aggregated pollution emitted by households sharing the "
				"same vehicle type",
		value_semantics= [ ?pollution_emission_semantics ],
		value_unit="g.cm^-3",
		value_type_description="float",
		value_constraints=[ positive ] },

	wooper:return_static( [ ActualEnergyOPort, ActualPollutionOPort ] ).



% @doc Returns the semantics statically declared by this processing unit.
%
% Defining this method allows to ensure that all the ports ever created by this
% processing unit will rely on user-level semantics among this explicitly stated
% list.
%
% Otherwise the list would be deduced from the initial port specifications, with
% no specific control.
%
-spec get_declared_semantics( term() ) -> static_return( user_vocabulary() ).
get_declared_semantics( X ) ->

	trace_utils:warning_fmt( "Ignoring '~p' for semantics.", [ X ] ),

	wooper:return_static(
	  [ ?energy_demand_semantics, ?pollution_emission_semantics ] ).



% @doc Returns the types statically declared by this unit.
-spec get_declared_types( term() ) ->
				   static_return( class_TypeServer:type_entries() ).
get_declared_types( X ) ->

	trace_utils:warning_fmt( "Ignoring '~p' for types.", [ X ] ),

	wooper:return_static( [] ). % Only built-in types



% Helper functions.


% @doc Returns a textual description of this unit.
-spec to_string( wooper:state() ) -> static_return( ustring() ).
to_string( State ) ->
	wooper:return_static(
		text_utils:format( "Vehicle type unit; this is a ~ts",
			[ class_DataflowProcessingUnit:to_string( State ) ] ) ).
