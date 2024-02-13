% Copyright (C) 2008-2024 EDF R&D

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


% @doc Example of simulation case for <b>platform integration</b>.
%
% Mostly used to:
%
% - check that we can indeed run it thanks to:
%  'make soda_platform_integration_run'
%
% - check the support of SII (see the --simulation-instance-id option)
%
% - have a longer simulation, so that elements like UNIX processes or resource
% consumption can be monitored and compared
%
-module(soda_platform_integration_case).



% For all facilities common to all cases:
-include("sim_diasca_for_cases.hrl").



% @doc Runs the case.
-spec run() -> no_return().
run() ->

	?case_start,

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

		simulation_name="Soda Platform Integration Case",

		% Using 100Hz here:
		tick_duration=0.01

		% We leave it to the default specification (all_outputs):
		% result_specification =
		%  [ { targeted_patterns, [ {".*",[data_and_rendering]} ] },
		%    { blacklisted_patterns, ["^Second" ] } ]

		%result_specification = [ { targeted_patterns, [ {".*",data_only} ] } ]

	},


	DeploymentSettings = #deployment_settings{

		computing_hosts={ use_host_file_otherwise_local,
						  "sim-diasca-host-candidates.txt" },

		%node_availability_tolerance = fail_on_unavailable_node,

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


	% Now that the engine is initialised, let's create the initial instances:

	% First machine starts with 10 cans, 2 euros each:
	SVM1 = class_Actor:create_initial_actor( class_SodaVendingMachine,
		[ _FirstMachineName="First soda machine", _FirstInitialCanCount=10,
		  _FirstCanCost=0.001 ] ),

	% Second machine starts with 8 cans, 1.5 euro each:
	SVM2 = class_Actor:create_initial_placed_actor( class_SodaVendingMachine,
		[ _SecondMachineName="Second soda machine", _SecondInitialCanCount=80,
		  _SecondCanCost=0.001 ], _PlacementHint=gimme_some_shelter ),


	% First customer uses SVM1, is thirsty 1 minute after having drunk, and has
	% 6 euros in his pockets:
	%
	_TC1 = class_Actor:create_initial_actor( class_DeterministicThirstyCustomer,
		[ _FirstCustomerName="John", _FirstKnownMachine=SVM1,
		  _FirstRepletionDuration=2, _FirstInitialBudget=35.0 ] ),


	% Second customer uses SVM1 too, is thirsty 3 minutes after having drunk,
	% and has 8 euros in his pockets:
	%
	_TC2 = class_Actor:create_initial_actor( class_DeterministicThirstyCustomer,
		[ _SecondCustomerName="Terry", _SecondKnownMachine=SVM1,
		  _SecondRepletionDuration=7, _SecondInitialBudget=40.0 ] ),


	% Third customer uses SVM2, is thirsty 2 minutes after having drunk, and has
	% 15 euros in his pockets:
	%
	_TC3 = class_Actor:create_initial_actor( class_DeterministicThirstyCustomer,
		[ _ThirdCustomerName="Michael", _ThirdKnownMachine=SVM2,
		  _ThirdRepletionDuration=2, _ThirdInitialBudget=77.0 ] ),


	% We want this case to end once a specified virtual duration elapsed, in
	% seconds:
	%
	SimulationDuration = 15000,

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = case_receive(),

	?case_info_fmt( "Starting simulation, for a stop after a duration "
					"in virtual time of ~Bms.", [ SimulationDuration ] ),

	RootTimeManagerPid ! { startFor, [ SimulationDuration, self() ] },

	?case_info( "Waiting for the simulation to end, "
				"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?case_info( "Simulation stopped spontaneously, "
						"specified stop tick must have been reached." )

	end,

	?case_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	?case_stop.
