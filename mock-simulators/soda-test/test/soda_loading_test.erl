% Copyright (C) 2014-2022 EDF R&D

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


% @doc Test for the <b>loading of the initial state</b> of a soda simulation
% from files.
%
-module(soda_loading_test).



% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	?case_start,

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

		simulation_name="Initial State Loading Test",

		% Using 100Hz here:
		tick_duration=0.01,

		% To load from file a part of the initial state of the simulation:
		%initialisation_files = []
		initialisation_files=[ "soda-instances.init" ] },


	DeploymentSettings = #deployment_settings{

		% Note that the configuration file below has not to be declared above as
		% well:
		%
		enable_data_exchanger={ true, [ "soda_parameters.cfg" ] },

		enable_performance_tracker=true },


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								DeploymentSettings, LoadBalancingSettings ),


	% We loaded instances from file, but of course we can still create others
	% programmatically as well:

	% First machine starts with 100 cans, 2 euros each:
	SVM1 = class_Actor:create_initial_actor( class_SodaVendingMachine,
		[ _FirstMachineName="First soda machine", _FirstInitialCanCount=100,
		  _FirstCanCost=1.0 ] ),

	% Second machine starts with 8 cans, 1.15 euro each:
	SVM2 = class_Actor:create_initial_placed_actor( class_SodaVendingMachine,
		[ _SecondMachineName="Second soda machine", _SecondInitialCanCount=8,
		  _SecondCanCost=1.15 ], _PlacementHint=gimme_some_shelter ),


	% First customer is deterministic, uses SVM1, is thirsty 2 minutes after
	% having drunk, and has 35 euros in his pockets:
	%
	_TC1 = class_Actor:create_initial_actor( class_DeterministicThirstyCustomer,
		[ _FirstCustomerName="John", _FirstKnownMachine=SVM1,
		  _FirstRepletionDuration=2, _FirstInitialBudget=35.0 ] ),


	% Second customer uses SVM1 too, yet is stochastic: he will be thirsty again
	% between 1 and 7 minutes after having drunk, and has 40 euros in his
	% pockets initially:
	%
	_TC2 = class_Actor:create_initial_actor( class_StochasticThirstyCustomer,
		[ _SecondCustomerName="Terry", _SecondKnownMachine=SVM1,
		  _SecondRepletionLaw={ uniform, 7 }, _SecondInitialBudget=40.0 ] ),


	% Third customer uses SVM2, is deterministic and thirsty 2 minutes after
	% having drunk, and has 77 euros in his pockets:
	%
	_TC3 = class_Actor:create_initial_actor( class_DeterministicThirstyCustomer,
		[ _ThirdCustomerName="Michael", _ThirdKnownMachine=SVM2,
		  _ThirdRepletionDuration=2, _ThirdInitialBudget=77.0 ] ),


	% We want this test to end once a specified virtual duration elapsed, in
	% seconds:
	%
	SimulationDuration = 150,

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, for a stop after a duration "
					"in virtual time of ~Bms.", [ SimulationDuration ] ),

	RootTimeManagerPid ! { startFor, [ SimulationDuration, self() ] },

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
