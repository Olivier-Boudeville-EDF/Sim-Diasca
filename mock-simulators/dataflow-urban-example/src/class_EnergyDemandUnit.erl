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


% @doc Example <b>dataflow unit</b>.
-module(class_EnergyDemandUnit).


-define( class_description,
		 "Example dataflow unit in terms of energy demand, in the context of "
		 "the 'Dataflow Urban Example' case." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowProcessingUnit ] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Urban-Example.Energy" ).

% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% For types like energy_demand():
-include("urban_example_defines.hrl").



% Design notes:

% This unit is ruled by the 'activate_when_all_set' activation policy, hence it
% will be activated each time that all of its input ports are set.

% This unit defines a port iteration, to accommodate any number of upstream
% transportation units.

% The iteration starts with no specific iterated port created; they are added
% on demand, typically by their unit manager.



% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Constructs a dataflow unit instance in charge of evaluating the need for
% energy:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - UnitName is a human-readable name for that unit instance (as a plain,
% non-empty string)
%
% - DataflowPid is the PID of the dataflow instance
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_DataflowProcessingUnit:unit_name(), dataflow_pid() ) ->
					   wooper:state().
construct( State, ActorSettings, UnitName, DataflowPid ) ->

	% We start with an input iteration:

	{ InputPortSpecs, OutputPortSpecs } = get_port_specifications(),

	% First the direct mother class:
	class_DataflowProcessingUnit:construct( State, ActorSettings,
		?trace_categorize(UnitName), _ActivationPolicy=activate_when_all_set,
		InputPortSpecs, OutputPortSpecs, DataflowPid ).




% Methods section.




% @doc Callback executed automatically whenever this unit gets activated.
%
% Meant to be overridden.
%
-spec activate( wooper:state() ) -> oneway_return().
activate( State ) ->

	?info_fmt( "Evaluating now the energy demand for ~ts.",
			   [ to_string( State ) ] ),

	% Here we enter the actual unit; currently we manage values directly,
	% bypassing any provision in terms of type, unit and all:
	%
	% (this computation could be done in any other module or programming
	% language)

	% Let's aggregate all energy demands:
	EnergyDemands = class_DataflowBlock:get_all_input_iteration_values(
					    "energy_demand", State ),

	% And fetch the current efficiency:
	TransformerEfficiency = class_DataflowBlock:get_input_port_value(
							    "transformer_efficiency", State ),

	% Finally we can determine the aggregated, building-level energy demand:
	AggregatedEnergyDemand = lists:sum( EnergyDemands ) / TransformerEfficiency,

	DemandStrings = [ text_utils:format( "value : ~p", [ D ] )
					    || D <- EnergyDemands ],

	?debug_fmt( "The aggregated energy demand is ~p, sum of the following ones,"
		" once weighted by a transformation efficiency of ~f: ~ts",
		[ AggregatedEnergyDemand, TransformerEfficiency,
		  text_utils:strings_to_string( DemandStrings ) ] ),

	% We now reenter the dataflow: we feed the corresponding output port with
	% the channel-ready version of this computation:

	TotalEnergyDemand = class_Dataflow:create_channel_value(
		AggregatedEnergyDemand, [ ?energy_demand_semantics ], "kW.h", "float" ),

	FirstOutputState = class_DataflowBlock:set_output_port_value(
						    "energy_needed", TotalEnergyDemand, State ),

	FinalState = FirstOutputState,

	wooper:return_state( FinalState ).



% Static section.


% @doc Returns the specifications for the input and output ports of that
% dataflow block.
%
-spec get_port_specifications() ->
		    static_return( { [ input_port_spec() ], [ output_port_spec() ] } ).
get_port_specifications() ->
	wooper:return_static( { get_input_port_specs(), get_output_port_specs() } ).



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
		name="energy_demand",
		comment="Each of these iterated ports tracks a source of energy demand",
		% Min and current instance count: 0, max is unbounded:
		is_iteration=true,
		value_semantics=[ ?energy_demand_semantics ],
		value_unit="kW.h",
		value_type_description="float",
		value_constraints=[ positive ] },

	% Implicitly (by default) not an iteration:
	TransformerIPort = #input_port_spec{
		name="transformer_efficiency",
		comment="The efficiency of a building voltage transformer",
		value_semantics=[ ?transformation_efficiency_semantics ],
		value_unit="dimensionless",
		value_type_description="float",
		value_constraints=[ strictly_positive ] },

	wooper:return_static( [ EnergyIterationIPort, TransformerIPort ] ).



% @doc Returns a list of the specifications of the (initial) output ports for
% that unit.
%
-spec get_output_port_specs() -> static_return( [ output_port_spec() ] ).
get_output_port_specs() ->

	AggregatedEnergyDemandOPort = #output_port_spec{
		name="energy_needed",
		comment="Aggregation of the energy_demand iteration",
		%value_semantics='Energy demand',
		value_semantics=[ ?energy_demand_semantics ],
		value_unit="kW.h",
		value_type_description="float",
		value_constraints=[ positive ] },

	AggregatedPollutionExhaustedOPort = #output_port_spec{
		name="pollution_exhausted",
		comment="Total pollution exhausted",
		%value_semantics='Pollution emitted',
		value_semantics=[ ?pollution_emission_semantics ],
		value_unit="g.cm^-3",
		value_type_description="float",
		value_constraints=[ positive ] },

	wooper:return_static( [ AggregatedEnergyDemandOPort,
							AggregatedPollutionExhaustedOPort ] ).



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
	wooper:return_static( [ ?energy_demand_semantics,
							?transformation_efficiency_semantics,
							?pollution_emission_semantics ] ).



% Helper functions.


% @doc Returns a textual description of this unit.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->
	text_utils:format( "Energy demand unit; this is a ~ts",
					   [ class_DataflowProcessingUnit:to_string( State ) ] ).
