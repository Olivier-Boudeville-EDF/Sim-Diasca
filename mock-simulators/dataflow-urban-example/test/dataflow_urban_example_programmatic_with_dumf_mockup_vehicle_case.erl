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



% @doc Programmatic test for the 'Dataflow Urban Example' case, illustrating the
% use of a <b>processing unit specified as a DUMF mockup</b>.
%
% The purpose of this module is to perform an overall, integrated testing of the
% dataflow support taking into account the DUMF format.
%
% See also the 'Sim-Diasca Dataflow HOWTO'.
%
-module(dataflow_urban_example_programmatic_with_dumf_mockup_vehicle_case).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").


% For the common semantics:
-include("urban_example_defines.hrl").


% In this simulation case, we have no third-party (ex: a remote platform) to
% deal with, hence we do not need any specific identification server.



% @doc Runs the case.
-spec run() -> no_return().
run() ->

	?case_start,

	% The duration (in floating-point virtual seconds) of a fundamental tick in
	% the simulation corresponds to one year:
	%
	TickDuration = time_utils:years_to_seconds( 1 ),

	SimulationDurationInYears = 30,

	% Just for the sake of checking it on the display:
	ExpectedTimestepCount = round( time_utils:years_to_seconds(
								SimulationDurationInYears ) / TickDuration ),

	SimulationDurationInYears = ExpectedTimestepCount,

	TimestepString =
		time_utils:duration_to_string( _Microseconds=1000*TickDuration ),

	DUMFFilename = "VehicleTypeMockup.dumf",

	test_facilities:display( "Running a 'Dataflow Urban Example' simulation "
		"case with a timestep of ~ts and a duration of "
		"~B years (hence corresponding to ~p expected "
		"timesteps), in a programmatic way, involving "
		"a processing unit based on the '~ts' DUMF file.",
		[ TimestepString, SimulationDurationInYears,
		  ExpectedTimestepCount, DUMFFilename ] ),

	test_facilities:display( "Of course this is merely a technical example "
		"being happily meaningless in terms of urban matters." ),

	SimulationSettings = #simulation_settings{
		simulation_name="Urban Dataflow Programmatic DUMF Example",
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


	% Let's create our own dataflow, directly from this case, programmatically:

	% Creating first the (here, single, overall) dataflow of interest:
	DataflowPid = class_Actor:create_initial_actor( class_Dataflow,
		[ "Urban Programmatic DUMF Example Dataflow", ExperimentManagerPid ],
		LoadBalancerPid ),

	% A single dataflow here:
	Dataflows = [ DataflowPid ],

	% Let's start on following year, January 1st at 00:00:00...
	StartYear = 2020,

	% Here the experiment step corresponds to the year of interest:
	%
	FirstStep = StartYear,
	LastStep = FirstStep + SimulationDurationInYears,



	% For this experiment, we rely on these specific, optional, programmatic
	% entry and exit points:
	%
	UrbanExperimentEntryPointPid = class_Actor:create_initial_actor(
		class_UrbanExperimentProgrammaticEntryPoint,
		[ Dataflows, FirstStep, LastStep, ExperimentManagerPid,
		  WorldManagerPid ], LoadBalancerPid ),


	UrbanExperimentExitPointPid = class_Actor:create_initial_actor(
		class_UrbanExperimentProgrammaticExitPoint,
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

	DefaultObjectManagers = class_DataflowObjectManager:create_default_managers(
		DefaultObjectManagerDefs, WorldManagerPid, LoadBalancerPid ),

	% For load-balancing purposes, multiple object managers may be created, we
	% just select one of them here:
	%
	DistrictManagerPid = hd( DefaultObjectManagers ),

	% Only one specifically-defined object manager:
	%
	SpecificObjectManagers = [ class_BuildingHouseholdObjectManager ],

	?test_info_fmt( "Creating now the specifically defined dataflow "
					"object managers: ~p", [ SpecificObjectManagers ] ),

	_SpecificObjectManagers = [ BuildingHouseholdManagerPid ] =
		class_DataflowObjectManager:create_specific_managers(
			SpecificObjectManagers, WorldManagerPid, LoadBalancerPid ),


	% In a very similar manner as for object managers, we define dataflow unit
	% managers; there is here only one of them. It is also directly linked to
	% its root, the experiment manager:
	%
	UnitManagerNames = [ class_UrbanUnitManager ],

	% Gets the (enabled) binding managers:
	DeploymentManagerPid ! { getBindingManagers, [], self() },
	ActualBindingManagers = test_receive(),


	?test_info_fmt( "Creating following dataflow unit managers: ~p.",
					[ UnitManagerNames ] ),

	_UnitManagers = [ UrbanUnitManagerPid ] =
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


	% In terms of dataflow object dependencies, we have a hierarchy here (an
	% abstracted-out city comprising districts that comprise buildings that
	% comprise households) that we prefer building top-to-bottom.
	%
	% As a consequence, when creating an urban object, its parent (if any) has
	% to be specified to it; implicitly this object will ensure that, on the
	% other way round, its parent is aware of its creation, by notifying it
	% accordingly, at the first diasca of this created object.
	%
	% Note: the connectivity information - for example which household is in
	% which building - does not pertain to the values exchanged over the
	% dataflow, so it is stored thanks to standard (actor) attributes, not
	% dataflow ones.

	% So we prefer creating first the districts (here, a single one), knowing
	% that the city is abstracted out.
	%
	% The construction parameters to do so are: the name of the district, its
	% administrative name, its ground surface (in square kilometers) and its
	% type (rural or urban).
	%
	DistrictPid = class_DataflowObjectManager:create_initial_object(
		DistrictManagerPid, class_District, DataflowPid,
		[ "District-9", [ "Columbia District", 1210.0, urban ] ] ),

	% A single district of interest here:
	Districts = [ DistrictPid ],

	% For the sake of this testing, this experiment entry point directly knows
	% the district involved, so that it can assign their input ports:
	%
	UrbanExperimentEntryPointPid !
		{ registerDistrictObjects, [ Districts ], self() },

	district_objects_registered = test_receive(),


	% Then the buildings associated to each district:
	%
	_Buildings = [ FirstBuildingPid, _SecondBuildingPid ] =
		class_DataflowObjectManager:create_initial_objects(
			BuildingHouseholdManagerPid, class_Building, DataflowPid,
			[

				% The construction parameters to create a building are its name,
				% its postal address and its parent district:
				%
				[ "First Building",
					[ "4 Main Street, Forest Hills, NY", DistrictPid ] ],

				[ "Second Building",
					[ "12 Walnut Avenue, Denver, Colorado", DistrictPid ] ] ] ),


	% Finally the households associated to each building are created, as
	% mentioned thanks to the building manager here:
	%
	_Households = [ _FirstHouseholdPid, _SecondHouseholdPid,
					_ThirdHouseholdPid ] =
		class_DataflowObjectManager:create_initial_objects(
			BuildingHouseholdManagerPid, class_Household, DataflowPid,
			[

				% The construction parameters to create an household are the
				% household name, last name, number of adults, number of
				% children, disposable income (in euros per year), mean distance
				% covered (in km) and the parent building:
				%
				[ "Household-1", [ "The Stallones", 0, 1, 951000, 1.15,
								   FirstBuildingPid ] ],

				[ "Household-2", [ "The Ramones", 4, 0, 210000, 1.71,
								   FirstBuildingPid ] ],

				[ "Household-3", [ "The Evans", 2, 2, 280000, 2.18,
								   FirstBuildingPid ] ] ] ),



	% Now that the initial dataflow objects are created, let's do the same of
	% the initial dataflow units, in practise thanks to the single unit manager
	% in charge of them here.
	%
	% Here we want to create three transportation demand unit instances, and two
	% energy demand unit instances, and have the 'energy_needed' output port of
	% each transport unit be linked to an 'energy_demand' input port iteration
	% (hence to as many iterated ports) of the relevant energy demand unit.
	%
	% We thus send these creation requests to the right unit manager.

	% First, the creation of the three transportation units (as always, we must
	% specify to which dataflow these units will be associated):
	%
	TransportUnits = class_DataflowUnitManager:create_initial_units(
		UrbanUnitManagerPid, class_TransportationDemandUnit,
		DataflowPid,
		% A transport unit takes here two specific construction
		% parameters, its name and its level of car sharing (as a
		% percentage):
		%
		[ [ "My First Initial Transport Unit",  0.22 ],
		  [ "My Second Initial Transport Unit", 0.17 ],
		  [ "My Third Initial Transport Unit",  0.24 ] ] ),

	[ FirstTransportUnit, SecondTransportUnit, ThirdTransportUnit ] =
		TransportUnits,


	% For the sake of this testing, this experiment entry point directly knows
	% the transportation units:
	%
	UrbanExperimentEntryPointPid !
		{ setTransportUnits, [ TransportUnits ], self() },

	transport_units_registered = test_receive(),



	% Second, we consider the creation of two vehicle units, both based on
	% a mock-up unit whose spec is obtained from a DUMF file:
	%
	VehicleTypeSpec =
		class_DataflowMockupUnit:read_mockup_unit_spec(	DUMFFilename ),

	VehicleUnits = [ FirstVehicleUnit, SecondVehicleUnit ] =
		class_DataflowUnitManager:create_initial_mockup_units(
			UrbanUnitManagerPid, VehicleTypeSpec, DataflowPid,
			[ "Zohe", "Yarix" ] ),

	% Then the creation of the single initial energy demand unit (a second one
	% will be created in the course of the simulation).
	%
	% The only construction parameter used here is the name of the unit.
	%
	EnergyUnit = class_DataflowUnitManager:create_initial_unit(
					UrbanUnitManagerPid, class_EnergyDemandUnit,
					DataflowPid, [ "My Single Initial Energy Demand Unit" ] ),

	EnergyUnits = [ EnergyUnit ],

	% The entry point directly knows here the energy units as well, in order to
	% feed them:
	%
	UrbanExperimentEntryPointPid !
		{ setEnergyDemandUnits, [ EnergyUnits ], self() },

	energy_demand_units_registered = test_receive(),

	% The exit point needs to know the energy units as well, so that it can
	% fetch output values from them:
	%
	UrbanExperimentExitPointPid !
		{ setEnergyDemandUnits, [ EnergyUnits ], self() },

	energy_demand_units_registered = test_receive(),


	% Then we connect the output ports of transport units to the input port
	% iterations of vehicle units and connect similarly the output ports of
	% vehicle units to the input port iterations of the energy unit:

	% For energy:

	class_DataflowUnitManager:connect_to_iterated_initially(
		UrbanUnitManagerPid,
		{ [ FirstTransportUnit, SecondTransportUnit ],
		  "energy_needed" },
		{ FirstVehicleUnit, "energy_demand_estimates" } ),

	class_DataflowUnitManager:connect_to_iterated_initially(
		UrbanUnitManagerPid,
		{ [ ThirdTransportUnit ], "energy_needed" },
		{ SecondVehicleUnit, "energy_demand_estimates" } ),

	class_DataflowUnitManager:connect_to_iterated_initially(
		UrbanUnitManagerPid,
		{ VehicleUnits, "actual_energy_need" },
		{ EnergyUnit, "energy_demand" } ),

	% For pollution:

	class_DataflowUnitManager:connect_to_iterated_initially(
		UrbanUnitManagerPid,
		{ [ FirstTransportUnit, SecondTransportUnit ],
		  "pollution_exhausted" },
		{ FirstVehicleUnit, "pollution_estimates" } ),

	class_DataflowUnitManager:connect_to_iterated_initially(
		UrbanUnitManagerPid,
		{ [ ThirdTransportUnit ], "pollution_exhausted" },
		{ SecondVehicleUnit, "pollution_estimates" } ),


	% Now the dataflow description is complete and we can specify
	% the time settings to the simulation engine before starting.
	%
	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	% The experiment will start in year StartYear...
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

	dataflow_support:stop(),

	sim_diasca:shutdown(),

	?case_stop.
