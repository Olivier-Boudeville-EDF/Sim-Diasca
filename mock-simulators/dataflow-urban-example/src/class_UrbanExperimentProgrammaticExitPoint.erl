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


% @doc A programmatic example of experiment exit point.
-module(class_UrbanExperimentProgrammaticExitPoint).


-define( class_description,
		 "This (programmatic) example of experiment exit point starts, as at "
		 "least most exit points, each step of this urban case experiment."
		 "This entry point does not rely on changesets to operate (see "
		 "class_UrbanExperimentPlatformEmulatingEntryPoint.erl for that)." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ExperimentExitPoint ] ).


% Helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Urban-Example.ExitPoint" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% For transport_unit_pid/0 and all:
-include("urban_example_defines.hrl").


% Implementation notes:
%
% Currently the only termination criterion is reaching the final step. At this
% positional parameter other criteria may be supported in the future.


% Attributes that are specific to the urban experiment exit point are:
-define( class_attributes, [

	{ current_step, step_count(),
	  "the current step at which the experiment is" },

	{ max_step, step_count(),
	  "the maximum step that the experiment may reach" },

	{ energy_demand_units, [ energy_demand_unit_pid() ],
	  "the energy demand units known of this exit point" },

	{ exit_probe_ref, class_Probe:probe_ref(), "a basic probe (if any) "
	  "allowing to monitor the data extracted by this exit point from the "
	  "dataflow" } ] ).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type step_count() :: class_ExperimentManager:step_count().


% @doc Constructs the urban-example experiment exit point, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Dataflows is a list of the dataflows that this exit point should drive
%
% - ExperimentStepStart is the number of steps the overall experiment shall
% start from (ex: first year)
%
% - ExperimentStepStop is the number of steps the overall experiment shall stop
% at (ex: last year)
%
% - ExperimentEntryPointPid is the PID of the entry point of the experiment
%
% - ExperimentManagerPid is the PID of the experiment manager
%
% - WorldManagerPid is the PID of the world manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
	[ dataflow_pid() ], step_count(), step_count(),
	experiment_entry_point_pid(), experiment_manager_pid(),
	world_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, Dataflows, ExperimentStepStart,
		   ExperimentStepStop, ExperimentEntryPointPid,
		   ExperimentManagerPid, WorldManagerPid ) ->

	% First the direct mother class:
	ExitState = class_ExperimentExitPoint:construct( State, ActorSettings,
		Dataflows, ExperimentEntryPointPid, ExperimentManagerPid,
		WorldManagerPid ),

	ProbeName = "Monitoring the setting of output ports over the dataflow, "
		"thanks to a probe attached to the urban experiment exit point",

	CurveNames = [
		"'total\_energy\_demand' output port of the energy demand unit" ],

	ExitProbeRef = class_Probe:declare_result_probe( ProbeName, CurveNames,
		_Zones=[], _Title=ProbeName, _XLabel="Simulated Year",
		_YLabel="Output as energy (in kW.h)" ),

	% Then the class-specific actions:
	setAttributes( ExitState, [
		{ current_step, ExperimentStepStart },
		{ max_step, ExperimentStepStop },
		{ energy_demand_units, [] },
		{ exit_probe_ref, ExitProbeRef } ] ).



% Methods section.


% @doc Sets (registers) the energy demand units that are known of this exit
% point.
%
-spec setEnergyDemandUnits( wooper:state(), [ energy_demand_unit_pid() ] ) ->
				request_return( 'energy_demand_units_registered' ).
setEnergyDemandUnits( State, EnergyDemandUnits ) ->

	?info_fmt( "Setting energy demand units to ~p.", [ EnergyDemandUnits ] ),

	NewState = setAttribute( State, energy_demand_units, EnergyDemandUnits ),

	wooper:return_state_result( NewState, energy_demand_units_registered ).



% @doc The core of the behaviour of this exit point.
%
% Overrides the default behaviour, yet reuses it.
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentStep = ?getAttr(current_step) + 1,

	MaxStep = ?getAttr(max_step),

	UpdatedState = case CurrentStep of

		S when S >= MaxStep ->

			?notice_fmt( "Maximum step (~B) reached, terminating the "
						 "simulation.", [ S ] ),

			setAttribute( State, phase, termination );

		_ ->

			% Simulation still in progress, we add our own behaviour, which
			% mostly consists on feeding a relevant probe:

			EnergyUnits = [ EnergyUnitPid ] = ?getAttr(energy_demand_units),


			OutputPortName = text_utils:string_to_binary( "energy_needed" ),

			?debug_fmt( "Reading the total energy demand from "
						"following units: ~w.", [ EnergyUnits ] ),

			% Will trigger back a notifyOutputPortStatus/3 call:
			class_Actor:send_actor_message( EnergyUnitPid,
				{ requestOutputPortStatus, [ OutputPortName ] }, State )

	end,

	% We call our direct mother class (ex: so that the *entry* point is
	% triggered at the next diasca):
	%
	ScheduleState = executeOnewayAs( UpdatedState, class_ExperimentExitPoint,
									 actSpontaneous ),

	FinalState = setAttribute( ScheduleState, current_step, CurrentStep ),

	wooper:return_state( FinalState ).



% @doc Called in answer to a requestOutputPortStatus/3 inquiry.
-spec notifyOutputPortStatus( wooper:state(), value_status(),
					sending_actor_pid() ) -> const_actor_oneway_return().
notifyOutputPortStatus( State, OutputPortStatus, SendingActorPid ) ->

	?debug_fmt( "Received output port status ~p from ~w.",
				[ OutputPortStatus, SendingActorPid ] ),

	case OutputPortStatus of

		{ set, TotalEnergy } ->
			CurrentStep = ?getAttr(current_step),
			class_Probe:send_data( ?getAttr(exit_probe_ref), CurrentStep,
								   { TotalEnergy } );

		unset ->
			ok

	end,

	actor:const_return().



% Helper functions.


% @doc Returns a textual description of this exit point.
%
% (helper)
%
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	ExitString = class_ExperimentExitPoint:to_string( State ),

	EnergyDemandString = case ?getAttr(energy_demand_units) of

		[] ->
			"not referencing any energy demand unit";

		EnergyDemandUnits ->
			text_utils:format( "referencing ~B energy demand units: ~p",
				[ length( EnergyDemandUnits ), EnergyDemandUnits ] )

	end,

	ProbeString = case ?getAttr(exit_probe_ref) of

		non_wanted_probe ->
			"not using a probe";

		ProbePid ->
			text_utils:format( "using probe ~p", [ ProbePid ] )

	end,

	text_utils:format( "Urban programmatic ~ts, ~ts, ~ts",
					   [ ExitString, EnergyDemandString, ProbeString ] ).
