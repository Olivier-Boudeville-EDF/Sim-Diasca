% Copyright (C) 2008-2023 EDF R&D
%
% This file is part of Sim-Diasca.
%
% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: 2008.


% @doc Unit tests for the interaction between the <b>random manager and
% stochastic actors</b>.
%
% See the class_RandomManager.erl and class_TestStochasticActor.erl tested
% modules.
%
-module(random_laws_and_stochastic_actor_test).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	?case_start,

	% Prefer reusing most default settings:
	SimulationSettings = #simulation_settings{
		simulation_name="Stochastic Actor Test" },

	DeploymentSettings = #deployment_settings{},

	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
		DeploymentSettings, LoadBalancingSettings ),

	% The random laws the test actor will rely on:
	% ({RandomLawName, RandomSpec})
	%
	LawDescs = [ { my_first_uniform,  { uniform, 5, 15 } },
				 { my_second_uniform, { integer_uniform, 0, 100 } },
				 { my_exponential,    { exponential_1p, 80 } },
				 { my_gaussian,       { gaussian, 50, 2 } } ],


	% Creates an actor that will automatically subscribe itself to the manager
	% and that will terminate on specified tick:
	%
	class_Actor:create_initial_actor( class_TestStochasticActor,
		[ "Cartman", LawDescs, _CartmanTerminationProbability=20 ] ),

	% Other actors:
	class_Actor:create_initial_actor( class_TestStochasticActor,
		[ "Kenny", LawDescs, _KennyTerminationProbability=99 ] ),

	class_Actor:create_initial_actor( class_TestStochasticActor,
		[ "Kyle", LawDescs, _KyleTerminationProbability=10 ] ),

	class_Actor:create_initial_actor( class_TestStochasticActor,
		[ "Stan", LawDescs, _StanTerminationProbability=0 ] ),


	% A TestStochasticActor requesting - and consuming - no law was successfully
	% tested as well.


	% We want this test to end once a specified number of ticks are elapsed:
	StopTick = 30,

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_notice_fmt( "Starting simulation, "
					  "for a stop at tick offset #~B.", [ StopTick ] ),

	RootTimeManagerPid ! { start, [ StopTick, self() ] },

	?test_info( "Waiting for the simulation to end, "
		"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously, "
						"specified stop tick must have been reached." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	?case_stop.
