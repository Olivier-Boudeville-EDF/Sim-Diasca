% Copyright (C) 2008-2021 EDF R&D

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



% Overall unit test of the Sim-Diasca actor creation, dealing with more complex
% runtime actor creation: multiple initial actors, creating recursively other
% actors, etc.
%
-module(runtime_actor_complex_creation_case).



% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").



% Runs the local test simulation.
%
-spec run() -> no_return().
run() ->

	?case_start,

	% Default simulation settings (50Hz, batch reproducible) are used, except
	% for the name:
	SimulationSettings = #simulation_settings{

		simulation_name = "Runtime actor complex creation"

	},


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested), but computing hosts are
	% specified (to be updated depending on your environment):
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

	?test_info( "Deployment manager created." ),

	% No need to retrieve explicitly the load balancer, to create initial
	% actors, yet useful for this test to monitor its state:

	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	LoadBalancerPid ! { traceState, [ "at start-up" ] },

	?test_info( "Requesting the creation of a first programmatic initial "
				"test actor, itself creating actors." ),

	% Settings for the created actors:

	CoreCreationPolicy = { 3, { { erratic, 5 }, no_creation } },

	InnerCreationPolicy = { 8, { { periodic, 4 }, CoreCreationPolicy } },

	% Number of spontaneous schedulings before an actor is created:
	FirstInterCount = 5,
	FirstCreatedSchedulingPolicy = { periodic, 27 },

	FirstKindOfCreatedActor = { FirstCreatedSchedulingPolicy,
								InnerCreationPolicy },

	FirstCreationPolicy = { FirstInterCount, FirstKindOfCreatedActor },

	_FirstActorPid = class_Actor:create_initial_actor( class_TestActor,
			[ _FirstName="First test actor",
			  _FirstSchedulingSettings={ erratic, 7 },
			  FirstCreationPolicy,
			  _FirstTerminationTickOffset=180 ] ),

	LoadBalancerPid ! { traceState,
						[ "after the first (programmatic) initial creation" ] },

	SecondInterCount = 8,
	SecondCreatedSchedulingPolicy = { erratic, 13 },
	SecondKindOfCreatedActor = { SecondCreatedSchedulingPolicy,
								 InnerCreationPolicy },
	SecondCreationPolicy = { SecondInterCount, SecondKindOfCreatedActor },

	_SecondActorPid = class_Actor:create_initial_actor( class_TestActor,
			[ _SecondName="Second test actor",
			  _SecondSchedulingSettings={ erratic, 9 },
			  SecondCreationPolicy,
			  _SecondTerminationTickOffset=280 ] ),

	LoadBalancerPid ! { traceState,
					[ "after the second (programmatic) initial creation" ] },

	% Not supported, as an actor must be synchronised beforehand:
	%ThirdCreationPolicy = creation_from_constructor,

	%_ThirdActorPid = class_Actor:create_initial_actor( class_TestActor,
	%		[ _ThirdName="Third test actor",
	%		  _ThirdSchedulingSettings={ erratic, 2 },
	%		  ThirdCreationPolicy,
	%		  _ThirdTerminationTickOffset=800 ] ),

	%LoadBalancerPid ! { traceState,
	%				[ "after the third (programmatic) initial creation" ] },


	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	LoadBalancerPid ! { traceState, [ "just before starting simulation" ] },

	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=10000, self() ] },


	?test_info( "Requesting textual timings (first)." ),

	RootTimeManagerPid ! { getTextualTimings, [], self() },
	FirstTimingString = test_receive(),

	?test_notice_fmt( "Received first time: ~s.", [ FirstTimingString ] ),

	LoadBalancerPid ! { traceState, [ "possibly during simulation" ] },

	% Waits until simulation is finished:
	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,

	LoadBalancerPid ! { traceState, [ "just after stopping simulation" ] },

	?test_info( "Requesting textual timings (second)." ),

	RootTimeManagerPid ! { getTextualTimings, [], self() },
	SecondTimingString = test_receive(),

	?test_notice_fmt( "Received second time: ~s.", [ SecondTimingString ] ),

	LoadBalancerPid ! { traceState, [ "at shutdown" ] },

	sim_diasca:shutdown(),

	?case_stop.
