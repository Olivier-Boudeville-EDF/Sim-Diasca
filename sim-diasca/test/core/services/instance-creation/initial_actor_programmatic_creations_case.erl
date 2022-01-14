% Copyright (C) 2008-2022 EDF R&D

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


% @doc Overall unit test of the Sim-Diasca actor creation, dealing only with the
% programmatic creation of initial actors.
%
% The test case will run until tick offset #120, however the only actor expects
% to terminate at tick offset #80, thus the simulation will finish before the
% specified user duration, since having no more actor to schedule.
%
-module(initial_actor_programmatic_creations_case).



% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").



% @doc Runs the local test simulation.
-spec run() -> no_return().
run() ->

	?case_start,

	% Default simulation settings (50Hz, batch reproducible) are used, except
	% for the name:
	SimulationSettings = #simulation_settings{
		simulation_name="Initial actor programmatic creations test" },


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested), but computing hosts are
	% specified (to be updated depending on your environment):
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

	?test_info( "Deployment manager created." ),

	% No need to retrieve explicitly the load balancer, to create initial
	% actors, yet useful for this test to monitor its state:

	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	LoadBalancerPid ! { traceState, [ "at start-up" ] },

	?test_info( "Requesting the creation of a first initial test actor." ),

	FirstActorPid = class_Actor:create_initial_actor( class_TestActor,
		[ _FirstName="First programmatic test actor",
		  _FirstSchedulingSettings={ erratic, 7 },
		  _FirstCreationSettings=no_creation,
		  _FirstTerminationTickOffset=80 ] ),

	LoadBalancerPid ! { traceState, [ "after first initial creation" ] },


	_SecondActorPid = class_Actor:create_initial_placed_actor( class_TestActor,
		[ _SecondName="Second programmatic test actor",
		  _SecondchedulingSettings={ erratic, 6 },
		  _SecondCreationSettings=no_creation,
		  _SecondTerminationTickOffset=100 ], my_second_placement_hint ),

	LoadBalancerPid ! { traceState, [ "after second initial creation" ] },


	ActorList = [ _ThirdActorPid, _FourthActorPid, _FifthActorPid ] =
		class_Actor:create_initial_actors( [

			{ class_TestActor, [ "Third programmatic actor", {erratic,5},
								 no_creation, 120 ] },

			{ class_TestActor, [ "Fourth programmatic actor", {erratic,5},
								 no_creation, 150 ],
			  my_fourth_placement_hint },

			{ class_TestActor, [ "Fifth programmatic actor", {erratic,5},
								 no_creation, 200 ] } ] ),

	LoadBalancerPid ! { traceState,
						[ "after third (multiple) initial creation" ] },

	FirstActorPid ! { getAAI, [], self() },
	Id = test_receive(),

	?test_notice_fmt( "Actor list created: ~p.", [ ActorList ] ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),


	?test_notice_fmt( "The actor identifier for first actor is ~w.", [ Id ] ),


	LoadBalancerPid ! { traceState, [ "just before starting simulation" ] },

	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=120, self() ] },


	?test_info( "Requesting textual timings (first)." ),

	RootTimeManagerPid ! { getTextualTimings, [], self() },
	FirstTimingString = test_receive(),

	?test_notice_fmt( "Received first time: ~ts.", [ FirstTimingString ] ),

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

	?test_notice_fmt( "Received second time: ~ts.", [ SecondTimingString ] ),

	LoadBalancerPid ! { traceState, [ "at shutdown" ] },

	sim_diasca:shutdown(),

	?case_stop.
