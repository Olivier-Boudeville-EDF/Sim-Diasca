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



% Unit test of the Sim-Diasca actor creation, dealing with the (programmatic)
% creation of an initial actor that itself performs an initial creation.
%
-module(initial_actor_performing_nested_initial_creation_case).



% For facilities common to all tests:
-include("sim_diasca_for_cases.hrl").



% Runs the local test simulation.
%
-spec run() -> no_return().
run() ->

	?case_start,

	% Default simulation settings (50Hz, batch reproducible) are used, except
	% for the name:
	SimulationSettings = #simulation_settings{

		simulation_name = "Initial actor performing nested initial creation"

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

	LoadBalancerPid ! { traceState, [ "at start-up, once the initialisation "
									  "file has been read" ] },

	?test_info( "Requesting the creation of a first programmatic initial "
				"test actor." ),

	_FirstActorPid = class_Actor:create_initial_actor( class_TestActor,
			[ _FirstName="First programmatic test actor",
			  _FirstSchedulingSettings={ erratic, 7 },
			  _FirstCreationSettings=initial_creation,
			  _FirstTerminationTickOffset=80 ] ),

	LoadBalancerPid ! { traceState,
						[ "after first programmatic initial creation" ] },


	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	LoadBalancerPid ! { traceState, [ "just before starting simulation" ] },

	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=120, self() ] },


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
