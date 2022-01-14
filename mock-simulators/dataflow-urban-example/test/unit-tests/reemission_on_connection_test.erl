% Copyright (C) 2018-2022 EDF R&D

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


% @doc Test regarding the reemission of past values sent by an output port
% whenever it gets connected again.
%
% See also its descrition in the 'Unit Tests Overview'
% (cf. unit-tests-overview.rst).
%
-module(reemission_on_connection_test).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").


% For the test semantics:
-include("dataflow_unit_test_defines.hrl").


% @doc Runs the case.
-spec run() -> no_return().
run() ->

	?case_start,

	% The duration (in floating-point virtual seconds) of a fundamental tick:
	%TickDuration = 1.0,

	TickCount = 50,

	test_facilities:display( "Running a ~ts test, in a programmatic way, "
							 "for ~B ticks.", [ ?MODULE, TickCount ] ),

	SimulationSettings = #simulation_settings{},

	DeploymentSettings = #deployment_settings{

		% We want to embed additionally just this unit test:
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
			[ "Unit test Dataflow", ExperimentManagerPid ], LoadBalancerPid ),

	% A single dataflow here:
	Dataflows = [ DataflowPid ],

	StartTick = 1,

	% Here the experiment step correspond to the tick of interest:
	FirstStep = StartTick,
	LastStep = FirstStep + TickCount,


	% For this experiment, we rely on these specific, optional, programmatic
	% entry and exit points:
	%
	TestExperimentEntryPointPid = class_Actor:create_initial_actor(
		class_BaseTestEntryPoint,
		[ Dataflows, FirstStep, LastStep, ExperimentManagerPid,
		  WorldManagerPid ], LoadBalancerPid ),


	_TestExperimentExitPointPid = class_Actor:create_initial_actor(
		class_BaseTestExitPoint,
		[ Dataflows, FirstStep, LastStep, TestExperimentEntryPointPid,
		  ExperimentManagerPid, WorldManagerPid ], LoadBalancerPid ),

	% Default object managers to handle a single type of dataflow object:
	DefaultObjectManagerDefs =
		[ { class_DataflowObjectManager, [ class_BaseTestDataflowObject ] } ],

	?test_info_fmt( "Creating directly following default dataflow "
		"object managers, taking in charge following types of "
		"dataflow objects: ~p.", [ DefaultObjectManagerDefs ] ),

	DefaultObjectManagers = class_DataflowObjectManager:create_default_managers(
		  DefaultObjectManagerDefs, WorldManagerPid, LoadBalancerPid ),

	% For load-balancing purposes, multiple default object managers may be
	% created, we just select one of them here:
	%
	BaseTestDataflowObjectManagerPid = hd( DefaultObjectManagers ),


	% Gets the (enabled) binding managers:
	DeploymentManagerPid ! { getBindingManagers, [], self() },
	ActualBindingManagers = test_receive(),


	% A single unit manager:
	UnitManagerNames = [ class_BaseUnitManager ],

	?test_info_fmt( "Creating following dataflow unit managers: ~p.",
					[ UnitManagerNames ] ),

	UnitManagers = [ BaseUnitManagerPid ] =
		class_DataflowUnitManager:create_managers( UnitManagerNames,
			ExperimentManagerPid, ActualBindingManagers, LoadBalancerPid ),

	TestExperimentEntryPointPid ! { addUnitManagers, [ UnitManagers ], self() },

	unit_managers_registered = test_receive(),

	% We create initially the base, upstream dataflow object of interest:
	BaseTestDataflowObjectPid =
		class_DataflowObjectManager:create_initial_object(
			BaseTestDataflowObjectManagerPid, class_BaseTestDataflowObject,
			DataflowPid, [ "MyBaseTestDataflowObject",
						   [ _FooValue=1, _BarValue=3.141 ] ] ),

	BaseTestDataflowObjects = [ BaseTestDataflowObjectPid ],

	% For the sake of this testing, this experiment entry point directly knows
	% the objects involved, so that it can assign directly their attribute at
	% appropriate times:
	%
	TestExperimentEntryPointPid ! { registerTestDataflowObjects,
									[ BaseTestDataflowObjects ], self() },

	test_dataflow_objects_registered = test_receive(),

	% We also create initially the unit that, once connected, will be
	% downstream:
	%
	BaseTestProcessingUnitPid = class_DataflowUnitManager:create_initial_unit(
		BaseUnitManagerPid, class_BaseTestProcessingUnit, DataflowPid,
		_ConstructParams=[ "MyBaseTestProcessingUnit" ] ),

	BaseTestDataflowUnits = [ BaseTestProcessingUnitPid ],

	% For the sake of this testing, this experiment entry point directly knows
	% this test unit as well:
	%
	TestExperimentEntryPointPid !
		{ registerTestProcessingUnits, [ BaseTestDataflowUnits ], self() },

	test_dataflow_units_registered = test_receive(),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	StartDate = { 0, 1, 1 },
	StartTime = { 0, 0, StartTick },

	RootTimeManagerPid !
		{ setInitialSimulationTimestamp, [ StartDate, StartTime ] },

	% ...and end specified ticks later:
	EndDate = StartDate,
	EndTime = { 0,0, StartTick + TickCount },

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
