% Copyright (C) 2008-2023 EDF R&D

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


% @doc Overall unit test of the Sim-Diasca deployment and scheduling framework.
%
% Three coupled erratic actors will be created prior to starting the simulation,
% planning to terminate at tick offsets #8000, #10000 and #15000, whereas the
% simulation stops at #12000.
%
-module(scheduling_multiple_coupled_erratic_actors_longer_test).


% To check that the three erratic actors are correctly scheduled, here is the
% overall behaviour they are expected to respect:


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").



% @doc Runs a distributed simulation (of course if relevant computing hosts are
% specified).
%
-spec run() -> no_return().
run() ->

	?case_start,

	% Default simulation settings (50Hz, batch reproducible) are used, except
	% for the name:
	%
	SimulationSettings = #simulation_settings{

		simulation_name =
			"Scheduling multiple coupled erratic actor longer test" },


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested), but computing
	% hosts are specified (to be updated depending on your environment):
	%
	% (note that localhost is implied)
	%
	DeploymentSettings = #deployment_settings{

		computing_hosts =
			{ use_host_file_otherwise_local, "sim-diasca-host-candidates.txt" }

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	?test_notice_fmt( "This test will deploy a distributed simulation"
		" based on computing hosts specified as ~p.",
		[ DeploymentSettings#deployment_settings.computing_hosts ] ),


	% Directly created on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								DeploymentSettings, LoadBalancingSettings ),


	?test_info( "Deployment manager created, retrieving the load balancer." ),

	DeploymentManagerPid ! { getLoadBalancer, [], self() },
	LoadBalancerPid = test_receive(),


	?test_info( "Requesting to the load balancer the creation of "
				"a first initial test actor." ),

	FirstActorPid = class_Actor:create_initial_actor( class_TestActor,
		[ "First test actor", { erratic, _FirstMinRange=5 }, no_creation,
		  _FirstTerminationTickOffset=8000 ], LoadBalancerPid ),

	FirstActorPid ! { getAAI, [], self() },
	2 = test_receive(),


	?test_notice_fmt( "First actor has for PID ~w and for AAI 2.",
					  [ FirstActorPid ] ),


	?test_info( "First actor has a correct AAI." ),

	SecondActorPid = class_Actor:create_initial_actor( class_TestActor,
		[ "Second test actor", { erratic, _SecondMinRange=7 }, no_creation,
		  _SecondTerminationTickOffset=10000 ],
		LoadBalancerPid ),

	SecondActorPid ! { getAAI, [], self() },
	3 = test_receive(),


	ThirdActorPid = class_Actor:create_initial_actor( class_TestActor,
		[ "Third test actor", { erratic, _ThirdMinRange=10 },
		  no_creation, _ThirdTerminationTickOffset=15000 ], LoadBalancerPid ),

	ThirdActorPid ! { getAAI, [], self() },
	4 = test_receive(),

	?test_info( "First three actors have correct AAI." ),

	AllActors = [ FirstActorPid, SecondActorPid, ThirdActorPid ],

	[ A ! lowerTraceIntensity || A <- AllActors ],

	?test_info( "Linking actors." ),

	class_TestActor:add_initial_peers( FirstActorPid,
									   [ SecondActorPid, ThirdActorPid ] ),

	class_TestActor:add_initial_peers( SecondActorPid,
									   [ FirstActorPid, ThirdActorPid ] ),

	class_TestActor:add_initial_peers( ThirdActorPid,
									   [ FirstActorPid, SecondActorPid ] ),


	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),


	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=12000, self() ] },


	?test_info( "Requesting textual timings (first)." ),

	RootTimeManagerPid ! { getTextualTimings, [], self() },
	FirstTimingString = test_receive(),

	?test_notice_fmt( "Received first time: ~ts.", [ FirstTimingString ] ),


	% Waits until simulation is finished:
	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,


	?test_info( "Requesting textual timings (second)." ),

	RootTimeManagerPid ! { getTextualTimings, [], self() },
	SecondTimingString = test_receive(),

	?test_notice_fmt( "Received second time: ~ts.", [ SecondTimingString ] ),

	sim_diasca:shutdown(),

	?case_stop.
