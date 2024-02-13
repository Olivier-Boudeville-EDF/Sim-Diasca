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


% @doc Integration test for the 'Dataflow Urban Example' case, based on the
% <b>management of changesets like if the simulation was driven by an overall
% platform</b> (that is emulated here).
%
% The purpose of this module is to perform an overall, integrated testing of the
% dataflow support.
%
% See also the 'Sim-Diasca Dataflow HOWTO'.
%
-module(dataflow_urban_example_platform_emulating_case).



% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").


% For the common semantics:
-include("urban_example_defines.hrl").



% Overall scenario:
%
% - 2020: initial year, nothing special
%
% - 2021: same thing
%
% - 2022: beginning of the real action:
%    - set-up of an actual simulation world (dataflow objects):
%      * creation of the "District-9" dataflow object
%      * creation of the "First Building" and of "Second Building" dataflow
%        objects, both registered in "District-9"
%      * creation of the "Household #1" and "Household #2" dataflow objects,
%        both included in "First Building"
%
%    - set-up of the corresponding processing (dataflow units)
%      * creation of the "First Household Transportation Demand" unit,
%        associated to "Household #1"
%      * creation of the "Second Household Transportation Demand" unit,
%        associated to "Household #2"
%      * creation of the "Third Household Transportation Demand" unit, not
%        associated to any dataflow block
%      * creation of the "First Per-Building Energy Demand" unit
%
%    - create channels (links from an output port to an input one)
%      * between "Household #1" and "First Household Transportation Demand",
%        etc.
%
% - 2023:
%    - update of various dataflow attributes of various dataflow objects
%
% - 2024:
%    - another update of various dataflow attributes of various dataflow objects
%
% - 2025: nothing special performed
%
% - 2026:
%      * creation of the "Household #3" dataflow object, included in
%        "Second Building"
%      * creation of the "Second Per-Building Energy Demand" unit
%      * update of various dataflow attributes of various dataflow objects
%
% - 2027: destruction of the "Household #2" dataflow object, to induce in turn
%   the destruction of the associated "Second Household Transportation Demand"
%   unit
%
% - from 2028 onward: update of various dataflow attributes of various dataflow
%   objects with some constant values, some other not
%
% As we emulate the use of a third-party platform, this scenario is expressed
% thanks to changesets that have been directly specified (in an hardcoded form)
% in the entry point (namely
% class_UrbanExperimentPlatformEmulatingEntryPoint.erl).



% @doc Runs the case.
-spec run() -> no_return().
run() ->

	?case_start,

	% The duration (in floating-point virtual seconds) of a fundamental tick in
	% the simulation corresponds to one year:
	%
	% (the various case-specific information used here could come from the
	% platform, for example thanks to REST calls that would be issued from this
	% test case)
	%
	TickDuration = time_utils:years_to_seconds( 1 ),

	SimulationDurationInYears = 30,

	% Just for the sake of checking it on the display:
	ExpectedTimestepCount = round( time_utils:years_to_seconds(
								SimulationDurationInYears ) / TickDuration ),

	SimulationDurationInYears = ExpectedTimestepCount,

	TimestepString = time_utils:duration_to_string(
						_Microseconds=1000*TickDuration ),

	test_facilities:display( "Running a 'Dataflow Urban Example' simulation "
		"case with a timestep of ~ts and a duration of "
		"~B years (hence corresponding to ~p expected "
		"timesteps), in a changeset-based way.",
		[ TimestepString, SimulationDurationInYears, ExpectedTimestepCount ] ),

	test_facilities:display( "Of course this is merely a technical example "
		"being happily meaningless in terms of urban matters." ),

	SimulationSettings = #simulation_settings{
		simulation_name="Urban Dataflow Platform-Emulating Example",
		tick_duration=TickDuration },


	DeploymentSettings = #deployment_settings{

		% We want to embed additionally this test and its specific
		% prerequisites, defined in the Mock Simulators:
		%
		additional_elements_to_deploy = [ { ".", code } ] },


	% We will initialise first the engine:
	DeploymentManagerPid =
		sim_diasca:init( SimulationSettings, DeploymentSettings ),

	LoadBalancerPid = class_LoadBalancer:get_balancer(),


	% Then the dataflow support:
	DataflowState = dataflow_support:start(),

	WorldManagerPid = DataflowState#dataflow_state.world_manager_pid,

	ExperimentManagerPid = DataflowState#dataflow_state.experiment_manager_pid,

	IdentificationServerPid =
		DataflowState#dataflow_state.identification_manager_pid,

	% Let's create our own dataflow, directly from this case, programmatically:

	% Creating first the (here, single, overall) dataflow of interest:
	DataflowPid = class_Actor:create_initial_actor( class_Dataflow,
		[ "Urban Platform-Emulating Dataflow", ExperimentManagerPid ],
		LoadBalancerPid ),

	% A single dataflow here:
	Dataflows = [ DataflowPid ],

	% Let's start on following year, January 1st at 00:00:00...
	StartYear = 2020,

	% Here the experiment step correspond to the year of interest:
	%
	FirstStep = StartYear,
	LastStep = FirstStep + SimulationDurationInYears,


	% For this experiment, we rely on these specific, optional entry and exit
	% points (they are the main components linked to any remote platform; here
	% they emulate the presence of such a platform):
	%
	UrbanExperimentEntryPointPid = class_Actor:create_initial_actor(
		class_UrbanExperimentPlatformEmulatingEntryPoint,
		[ Dataflows, FirstStep, LastStep, ExperimentManagerPid,
		  WorldManagerPid ], LoadBalancerPid ),


	_UrbanExperimentExitPointPid = class_Actor:create_initial_actor(
		class_UrbanExperimentPlatformEmulatingExitPoint,
		[ Dataflows, FirstStep, LastStep, UrbanExperimentEntryPointPid,
		  ExperimentManagerPid, WorldManagerPid ], LoadBalancerPid ),


	% Now that the engine and the dataflow are initialised, no specific, actual
	% initial object or unit instance is created here: they will be created by
	% the entry and exit points, and while the simulation is already running.
	%
	% So we create just the overall object and unit managers, and start the
	% simulation immediately afterwards.


	% For this example we could have considered up to three different dataflow
	% object managers: DistrictManager, BuildingManager and HouseholdManager.
	%
	% To showcase that an object manager may handle more than one type of
	% objects, here we have:
	%
	% - a DistrictManager, managing, well, districts (and only them)
	%
	% - a BuildingHouseholdManager, taking care of both the buildings and of the
	% households that they are hosting (considering that these urban objects are
	% tightly linked)


	% Moreover there are two ways of creating an object manager:
	%
	% - either we create a generic one by instantiating it directly from the
	% class_DataflowObjectManager (in which case it will rely on the default
	% implementation and correspond to no specific child class thereof); this
	% will be the case for class_DistrictManager
	%
	% - or we define specifically a child class of class_DataflowObjectManager,
	% and create a singleton instance of it (should domain-specific code be
	% needed for this particular object manager); this will be the case for
	% class_UrbanUnitManager

	% We also consider here that the tree of the object managers is flat (all of
	% them are directly linked to the world manager).

	% Only one default object manager, in charge only of districts:
	DefaultObjectManagerDefs =
		[ { class_DistrictObjectManager, [ class_District ] } ],

	?test_info_fmt( "Creating directly following default dataflow "
		"object managers, taking in charge following types of "
		"dataflow objects: ~p.", [ DefaultObjectManagerDefs ] ),

	_DefaultObjectManagers =
		class_DataflowObjectManager:create_default_managers(
			DefaultObjectManagerDefs, WorldManagerPid, LoadBalancerPid,
			IdentificationServerPid ),

	% Only one specifically-defined object manager:
	%
	SpecificObjectManagers = [ class_BuildingHouseholdObjectManager ],

	?test_info_fmt( "Creating now the specifically defined dataflow "
					"object managers: ~p", [ SpecificObjectManagers ] ),

	_SpecificObjectManagers = [ _BuildingHouseholdManagerPid ] =
		class_DataflowObjectManager:create_specific_managers(
			SpecificObjectManagers, WorldManagerPid, LoadBalancerPid,
			IdentificationServerPid ),


	% In a rather similar manner as for object managers, we define dataflow unit
	% managers; there is here only one of them, driven by the experiment
	% manager.
	%
	UnitManagerNames = [ class_UrbanUnitManager ],

	% Gets the (enabled) binding managers:
	DeploymentManagerPid ! { getBindingManagers, [], self() },
	ActualBindingManagers = test_receive(),


	?test_info_fmt( "Creating following dataflow unit managers: ~p, using .",
					[ UnitManagerNames ] ),

	_UnitManagers = [ _UrbanUnitManagerPid ] =
		class_DataflowUnitManager:create_managers( UnitManagerNames,
			ExperimentManagerPid, ActualBindingManagers, LoadBalancerPid ),



	% To show that semantics and types can be introduced from the simulation
	% case as well:

	CaseSemantics = [ ?average_population_gain, ?average_savings ],

	dataflow_support:declare_vocabulary( CaseSemantics, DataflowState ),


	% Definition of a type, and of another one directly depending on the first:
	CaseTypes = [ { 'a_type_from_case', "[ float ]" },
				  { 'my_other_type', "{ integer, a_type_from_case }" } ],

	dataflow_support:declare_types( CaseTypes, DataflowState ),


	% No creation of any actual dataflow object or unit; we just start the
	% simulation, the entry point will take care of creating all domain-specific
	% landscape directly at runtime:

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	StartDate = { StartYear, 1, 1 },
	StartTime = { 0, 0, 0 },

	RootTimeManagerPid !
		{ setInitialSimulationTimestamp, [ StartDate, StartTime ] },

	% ...and end specified years later:
	EndDate = { StartYear + SimulationDurationInYears, 1, 1 },
	EndTime = StartTime,

	% No need to specifically set the end date, as it will be induced by the
	% termination of the experiment entry point when it will have exhausted its
	% steps (and thus will not request to be scheduled anymore):
	%
	%RootTimeManagerPid ! { setFinalSimulationTimestamp, [ EndDate, EndTime ] },

	StartTimestamp = { StartDate, StartTime },
	EndTimestamp = { EndDate, EndTime },

	?test_info_fmt( "Starting simulation at ~ts, "
		"for an expected stop at ending timestamp ~ts.",
		[ time_utils:get_textual_timestamp( StartTimestamp ),
		  time_utils:get_textual_timestamp( EndTimestamp ) ] ),


	% Termination decided by the exit point, by not triggering the entry point
	% anymore, leading to an automatic stop:
	%
	RootTimeManagerPid ! { start, self() },

	?test_info( "Waiting for the simulation to end, since having been "
				"declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously, specified stop tick "
						"must have been reached." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	class_IdentificationServer:stop(),

	dataflow_support:stop(),

	sim_diasca:shutdown(),

	?case_stop.
