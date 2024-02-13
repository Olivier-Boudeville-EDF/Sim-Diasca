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


% @doc Example of a programmatic experiment entry point.
-module(class_UrbanExperimentProgrammaticEntryPoint).


-define( class_description,
		 "This (programmatic) example of experiment entry point starts each "
		 "step of this urban case experiment."
		 "This entry point does not rely on changesets to operate (see "
		 "class_UrbanExperimentPlatformEmulatingEntryPoint.erl for that)." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ExperimentEntryPoint ] ).


% Attributes that are specific to this urban experiment entry point are:
-define( class_attributes, [

	{ current_step, step_count(),
	  "the current step at which the experiment is" },

	{ max_step, step_count(),
	  "the maximum step that the experiment may reach" },

	{ transport_units, [ transport_unit_pid() ],
	  "the transportation units known of this entry point" },

	{ energy_demand_units, [ energy_demand_unit_pid() ],
	  "the energy demand units known of this entry point" },

	{ district_objects, [ district_pid() ],
	  "a list of the district dataflow objects known of this entry point" },

	{ household_objects, [ household_pid() ],
	  "a list of the household dataflow objects known of this entry point" },

	{ base_distance_covered, unit_utils:kilometers(), "the mean total distance "
	  "covered by all the persons of a given household at simulation start "
	  "(meant to vary over time)" },

	{ base_transformer_efficiency, math_utils:percent(), "the transformer "
	  "performance at simulation start (meant to vary over time)" },

	{ unit_managers, [ unit_manager_pid() ],
	  "a list of the unit managers driven by this entry point" },

	{ entry_probe_ref, class_Probe:probe_ref(), "a basic probe (if any) "
	  "allowing to monitor the data injected by this entry point into the "
	  "dataflow" } ] ).


% Helpers:
-export([ update_dataflow_structure/2, to_string/1 ]).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Urban-Example.EntryPoint" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% For transport_unit_pid/0 and all:
-include("urban_example_defines.hrl").



% Shorthands:

-type ustring() :: text_utils:ustring().

-type step_count() :: class_ExperimentManager:step_count().



% @doc Constructs the urban-example experiment entry point, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Dataflows is a list of the dataflows that this entry point should drive
%
% - ExperimentStepStart is the step at which the experiment shall start
%
% - ExperimentStepStop is the step at which the experiment shall stop
%
% - ExperimentManagerPid is the PID of the experiment manager
%
% - WorldManagerPid is the PID of the world manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
	[ dataflow_pid() ], step_count(), step_count(),
	experiment_manager_pid(), world_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, Dataflows, ExperimentStepStart,
		   ExperimentStepStop, ExperimentManagerPid, WorldManagerPid ) ->

	% First the direct mother class:
	EntryState = class_ExperimentEntryPoint:construct( State, ActorSettings,
						Dataflows, ExperimentManagerPid, WorldManagerPid ),

	ProbeName = "Monitoring the setting of input ports over the dataflow, "
		"thanks to a probe attached to the urban experiment entry point",

	CurveNames = [
		"'average\_journey' input port of the first transportation unit",
		"'average\_journey' input port of the second transportation unit",
		"'average\_journey' input port of the third transportation unit",
		"'transformer\_efficiency' input port of the aggregating energy unit"
				 ],

	EntryProbeRef = class_Probe:declare_result_probe( ProbeName, CurveNames,
		_Zones=[], _Title=ProbeName, _XLabel="Simulated Year",
		_YLabel="Three inputs set as distance (in km), "
				"one set as efficiency (in %)" ),

	% Then the class-specific actions:
	setAttributes( EntryState, [ { current_step, ExperimentStepStart },
								 { max_step, ExperimentStepStop },
								 { district_objects, [] },
								 { household_objects, [] },
								 { transport_units, [] },
								 { energy_demand_units, [] },
								 { base_transformer_efficiency, 2.7 },
								 { unit_managers, [] },
								 { entry_probe_ref, EntryProbeRef } ] ).



% @doc Registers specified district dataflow objects to this entry point, so
% that it is able to act upon them (ex: attribute update).
%
-spec registerDistrictObjects( wooper:state(), [ district_pid() ] ) ->
				request_return( 'district_objects_registered' ).
registerDistrictObjects( State, Districts ) ->

	?info_fmt( "Registering districts ~p.", [ Districts ] ),

	NewState = appendToAttribute( State, district_objects, Districts ),

	wooper:return_state_result( NewState, district_objects_registered ).



% @doc Registers specified household dataflow objects to this entry point, so
% that it is able to act upon them (ex: attribute update).
%
-spec registerHouseholdObjects( wooper:state(), [ household_pid() ] ) ->
				request_return( 'household_objects_registered' ).
registerHouseholdObjects( State, Households ) ->

	?info_fmt( "Registering households ~p.", [ Households ] ),

	NewState = appendToAttribute( State, household_objects, Households ),

	wooper:return_state_result( NewState, household_objects_registered ).




% Section for actor oneways.


% @doc Starts the evaluation of the urban experiment for the current tick.
%
% Typically called by the experiment exit point, for synchronisation reasons.
%
-spec startExperimentTick( wooper:state(), sending_actor_pid() ) ->
								 actor_oneway_return().
startExperimentTick( State, _SenderActorPid ) ->

	CurrentStep = ?getAttr(current_step),

	% This example implementation just impulses updates in the transport units:

	TransportUnits = ?getAttr(transport_units),

	MaxStep = ?getAttr(max_step),

	?debug_fmt( "Starting experiment step ~B/~B, first updating ~B "
		"transport units: ~w.",
		[ CurrentStep, MaxStep, length( TransportUnits ), TransportUnits ] ),


	% For this example, we emulate the use of an external source of information
	% that is to:
	%
	% - change the structure of the dataflow, by creating new energy demand
	% units, thanks to the known unit managers
	%
	% - update the state of the dataflow, by updating (thanks to
	% setInputPortValue/4) the state of our target system before we simulate it
	% for this step; here we chose to perform these updates partly on the base
	% of the current step

	% First the unit creations:

	% Temporarily disabled, to be replaced by changesets and all:
	% StructureState = update_dataflow_structure( CurrentStep, State ),
	StructureState = State,

	% Then the unit updates:
	UpdateState = assign_input_ports( TransportUnits, CurrentStep, MaxStep,
									  StructureState ),

	FinalState = setAttribute( UpdateState, current_step, CurrentStep+1 ),

	actor:return_state( FinalState ).



% @doc Creates new (hence dynamic, runtime) units, to demonstrate how structural
% dataflow changes may be done.
%
% (helper)
%
-spec update_dataflow_structure( wooper:state(), step_count() ) ->
						wooper:state().
update_dataflow_structure( CurrentStep, State ) ->

	UnitManagers = [ UrbanUnitManagerPid ] = ?getAttr(unit_managers),

	?debug_fmt( "Requesting the unit managers ~w to update the dataflow.",
				[ UnitManagers ] ),

	% Let's suppose we fetch from outside the following creation information:
	case CurrentStep of

		2025 ->
			?info_fmt( "Determined that a new energy demand unit is needed, "
				"notifying the unit manager ~w.", [ UrbanUnitManagerPid ] ),

			class_Actor:send_actor_message( UrbanUnitManagerPid,
				{ notifyEvent, [ new_energy_demand_unit_needed,
								 "My 2025 Energy Demand Unit" ] }, State );

		_ ->
			State

	end.



% @doc Assigns the input ports of specified transport units to demonstrate unit
% updates (i.e. state changes of dataflow elements).
%
-spec assign_input_ports( [ transport_unit_pid() ], step_count(), step_count(),
						  wooper:state() ) -> wooper:state().
assign_input_ports( TransportUnits, CurrentStep, MaxStep, State ) ->

	%trace_utils:debug_fmt( "CurrentStep=~p, MaxStep=~p.",
	%                       [ CurrentStep, MaxStep ] ),

	BaseAverageJourney = 0.29 * ( MaxStep - CurrentStep ),

	{ TransportState, AverageJourney } =
		update_transport_units( TransportUnits, BaseAverageJourney, State ),

	% Note: we select here the past energy demand units, not any newly created
	% one:
	%
	EnergyUnits = ?getAttr(energy_demand_units),

	% 6% improvement per year!
	NewEfficiency = 1.06 * ?getAttr(base_transformer_efficiency),

	?debug_fmt( "Setting the base transformer efficiency of the energy "
				"demand units (~w) to ~f.", [ EnergyUnits, NewEfficiency ] ),

	{ EnergyState, TransformerEfficiencies } = update_energy_demand_units(
							EnergyUnits, NewEfficiency, TransportState ),

	Sample = list_to_tuple( AverageJourney ++ TransformerEfficiencies ),

	class_Probe:send_data( ?getAttr(entry_probe_ref), CurrentStep, Sample ),

	setAttribute( EnergyState, base_transformer_efficiency, NewEfficiency ).



% @doc Triggers the update of specified transport units, prior to evaluating the
% experiment for the corresponding new step.
%
% (helper)
%
-spec update_transport_units( [ transport_unit_pid() ], average_journey(),
				wooper:state() ) -> { wooper:state(), [ average_journey() ] }.
update_transport_units( TransportUnits, BaseAverageJourney, State ) ->

	% Here we feed the 'average_journey' input port of each of these
	% transportation units with a value that changes (from a unit to another,
	% and from a step to another).
	%
	% This value is determined from the specified base one (e.g. the current
	% experiment step) on which the same affine function is applied one more
	% time per new transportation unit that is examined (so that their
	% corresponding input port is updated with a value different from the
	% others, all values changing at each experiment step):
	%
	update_average_journey( TransportUnits,
		_SpecificDistance=BaseAverageJourney, State, _Acc=[] ).




% (helper)
update_average_journey( _TransportUnits=[], _SpecificDistance, State, Acc ) ->
	{ State, lists:reverse( Acc ) };

update_average_journey( _TransportUnits=[ TransportUnitPid | T ],
						SpecificDistance, State, Acc ) ->

	% The affine function to have a different value for each unit, yet the port
	% constraint ({lower_than,4.0}, in km) shall still be accommodated:
	%
	NewSpecificDistance = SpecificDistance * 0.85 + 1,

	% Uncomment elements to test some error cases:
	FullValue = class_Dataflow:create_channel_value(
		_Value=NewSpecificDistance,
		%_Semantics="http://foobar.org/urban/1.1/faulty",
		% As a value may comprise semantics not known of a port:
		_Semantics=[ ?path_length_semantics, ?extra_semantics ],
		%_Unit="m",
		_Unit="km",
		_Type="float" ),

	Message = { setInputPortValue,
				[ _InputPortName="average_journey", FullValue ] },

	SentState = class_Actor:send_actor_message( TransportUnitPid, Message,
												State ),

	update_average_journey( T, NewSpecificDistance, SentState ,
							[ NewSpecificDistance | Acc ] ).



% @doc Triggers the update of specified energy demand units, prior to evaluating
% the experiment for the corresponding new step.
%
-spec update_energy_demand_units( [ energy_unit_pid() ],
		transformer_efficiency(), wooper:state() ) ->
						{ wooper:state(), [ transformer_efficiency() ] }.
update_energy_demand_units( EnergyUnits, Efficiency, State ) ->

	% Here all these units are notified at the common, uniform transformer
	% efficiency for the current simulation time:

	update_transformer_efficiency( EnergyUnits, Efficiency, State, _Acc=[] ).


% (helper)
update_transformer_efficiency( _EnergyUnits=[], _Efficiency, State, Acc ) ->
	{ State, lists:reverse( Acc ) };

update_transformer_efficiency( _EnergyUnits=[ EnergyDemandUnitPid | T ],
							   Efficiency, State, Acc ) ->

	% Uncomment elements to test some error cases:
	FullValue = class_Dataflow:create_channel_value(
		_Value=Efficiency,
		%_Semantics="http://foobar.org/urban/1.1/faulty",
		_Semantics=[ ?transformation_efficiency_semantics ],
		_Unit="dimensionless",
		_Type="float" ),

	Message = { setInputPortValue,
				[ _InputPortName="transformer_efficiency", FullValue ] },

	SentState = class_Actor:send_actor_message( EnergyDemandUnitPid, Message,
												State ),

	update_transformer_efficiency( T, Efficiency, SentState,
								   [ Efficiency | Acc ] ).




% Section for plain methods (ex: not actor oneways).


% @doc Declares the specified unit managers to this entry point.
%
% (request, for synchronicity)
%
-spec addUnitManagers( wooper:state(), [ unit_manager_pid() ] ) ->
			request_return( 'unit_managers_registered' ).
addUnitManagers( State, UnitManagers ) ->

	?info_fmt( "Adding unit managers ~w.", [ UnitManagers ] ),

	NewUnitManagers = ?getAttr(unit_managers) ++ UnitManagers,

	NewState = setAttribute( State, unit_managers, NewUnitManagers ),

	wooper:return_state_result( NewState, unit_managers_registered ).



% @doc Sets the transport units known of this entry point.
-spec setTransportUnits( wooper:state(), [ transport_unit_pid() ] ) ->
				request_return( 'transport_units_registered' ).
setTransportUnits( State, TransportUnits ) ->

	?info_fmt( "Setting transport units to ~p.", [ TransportUnits ] ),

	NewState = setAttribute( State, transport_units, TransportUnits ),

	wooper:return_state_result( NewState, transport_units_registered ).



% @doc Sets the energy demand units known of this entry point.
-spec setEnergyDemandUnits( wooper:state(), [ energy_demand_unit_pid() ] ) ->
				request_return( 'energy_demand_units_registered' ).
setEnergyDemandUnits( State, EnergyDemandUnits ) ->

	?info_fmt( "Setting energy demand units to ~p.", [ EnergyDemandUnits ] ),

	NewState = setAttribute( State, energy_demand_units, EnergyDemandUnits ),

	wooper:return_state_result( NewState, energy_demand_units_registered ).




% Helper functions.


% @doc Returns a textual description of this entry point.
%
% (helper)
%
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	EntryString = class_ExperimentEntryPoint:to_string( State ),

	TransportString = case ?getAttr(transport_units) of

		[] ->
			"not referencing any transport unit";

		TransportUnits ->
			 text_utils:format( "referencing ~B transport units: ~p",
								[ length( TransportUnits ), TransportUnits ] )

	end,

	EnergyDemandString = case ?getAttr(energy_demand_units) of

		[] ->
			"not referencing any energy demand unit";

		EnergyDemandUnits ->
			 text_utils:format( "referencing ~B energy demand units: ~p",
				[ length( EnergyDemandUnits ), EnergyDemandUnits ] )

	end,

	UnitManagerString = case ?getAttr(unit_managers) of

		[] ->
			text_utils:format( "not knowing any unit manager" );

		UnitManagers ->
			text_utils:format( "knowing following ~B unit manager(s): ~w",
							   [ length( UnitManagers ), UnitManagers ] )

	end,

	ProbeString = case ?getAttr(entry_probe_ref) of

		non_wanted_probe ->
			"not using a probe";

		ProbePid ->
			text_utils:format( "using probe ~p", [ ProbePid ] )

	end,

	text_utils:format( "Urban programmatic ~ts, ~ts, ~ts, ~ts, ~ts",
		[ EntryString, TransportString, EnergyDemandString, UnitManagerString,
		  ProbeString ] ).
