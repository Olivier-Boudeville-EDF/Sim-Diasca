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


% @doc <b>Integration test</b> for the soda stochastic example case.
%
% See also:
%  - class_SodaVendingMachine.erl
%  - class_StochasticThirstyCustomer.erl
%
-module(soda_stochastic_interactive_integration_test).


% To silence a never-match Dialyzer warning:
-export([ run/1 ]).


% For all facilities common to all simulation cases:
-include("sim_diasca_for_cases.hrl").



% Probes are retrieved in this test, so that they can either output their graphs
% or not, depending on the test being run in batch or not.


% @doc Returns the settings for a test required to be more or less long.
%
% Returns { {FirstInitialCanCount, FirstInitialBudget},
% {SecondInitialCanCount, SecondInitialBudget}, StopTick }
%
get_settings_for( base_test ) ->

	FirstInitialCanCount = 150,
	FirstInitialBudget = 6.0,

	SecondInitialCanCount = 60,
	SecondInitialBudget = 80.0,

	% Already long enough (40 seconds):
	StopTick = 2000,

	{ { FirstInitialCanCount, FirstInitialBudget },
	  { SecondInitialCanCount, SecondInitialBudget }, StopTick };


get_settings_for( longer_test ) ->

	FirstInitialCanCount = 10000,
	FirstInitialBudget = 60000000.0,

	SecondInitialCanCount = 8000,
	SecondInitialBudget = 8000000.0,

	% This test will end when the last actor will vanish, actually sooner than:
	StopTick = 100000,

	{ { FirstInitialCanCount, FirstInitialBudget },
		{ SecondInitialCanCount, SecondInitialBudget }, StopTick }.





% @doc Runs the test.
-spec run() -> no_return().
run() ->
	run( base_test ).


-spec run( 'base_test' | 'longer_test' ) -> no_return().
run( TestType ) ->

	?case_start,

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

		simulation_name="Soda Stochastic Interactive Integration Test",

		simulation_interactivity_mode=interactive

		% Using default simulation frequency (50Hz, period of 20ms).

		% We leave it to the default specification (all_outputs):
		%result_specification =
		% [ { targeted_patterns, [ {".*",[data_and_rendering] } ] },
		%   { blacklisted_patterns, ["^Second" ] } ]

		%result_specification = no_output

	},


	% Specifies the list of computing hosts that can be used:
	%
	% (see the sim-diasca-host-candidates-sample.txt example in the
	% sim-diasca/conf directory)
	DeploymentSettings = #deployment_settings{

		% We want to embed additionally this test and its specific
		% prerequisites, defined in the Mock Simulators:
		%
		additional_elements_to_deploy=[ { ".", code } ],

		% Not needed here:
		enable_data_exchanger=false },


	% A deployment manager is created directly on the user node:
	DeploymentManagerPid =
		sim_diasca:init( SimulationSettings, DeploymentSettings ),

	{ { FirstInitialCanCount, FirstInitialBudget },
	 { SecondInitialCanCount, SecondInitialBudget }, StopTick } =
		get_settings_for( TestType ),


	% First machine starts with 10 cans, 2 euros each:
	SVM1 = class_Actor:create_initial_actor( class_SodaVendingMachine,
		[ _FirstMachineName="First soda machine", FirstInitialCanCount,
		  _FirstCanCost=2.0 ] ),


	% Testing the parallel creation now:

	% Second machine starts with 8 cans, 1.5 euro each, while first customer
	% uses SVM1, is thirsty after a duration between 1 and 10 minutes (with all
	% durations in-between having equal probability) after having drunk, and has
	% some euros in his pockets:
	%
	[ SVM2, _TC1 ] = class_Actor:create_initial_actors( [

		{ class_SodaVendingMachine, [ _SecondMachineName="Second soda machine",
			SecondInitialCanCount, _SecondCanCost=1.5 ] },

		{ class_StochasticThirstyCustomer, [ _FirstCustomerName="John",
			_FirstKnownMachine=SVM1, _FirstRepletionDurationLaw={uniform,10},
			FirstInitialBudget ] } ] ),

	% Second customer uses SVM1 too, is thirsty on average 3 minutes after
	% having drunk with a standard deviation of 1, and has some euros in his
	% pockets:
	%
	_TC2 = class_Actor:create_initial_actor( class_StochasticThirstyCustomer,
		[ _SecondCustomerName="Terry", _SecondKnownMachine=SVM1,
		  _SecondRepletionDurationLaw={gaussian,3,1}, SecondInitialBudget ] ),

	% Third customer uses SVM2, is thirsty 2 minutes after having drunk
	% (deterministically), and has 15 euros in his pockets:
	%
	_TC3 = class_Actor:create_initial_actor( class_DeterministicThirstyCustomer,
		[ _ThirdCustomerName="Michael", _ThirdKnownMachine=SVM2,
		  _ThirdRepletionDuration=2, _ThirdInitialBudget=15.0 ] ),


	TC4Name = "George",
	TC5Name = "Paul",
	TC6Name = "Carrie",

	ActorPids = [ _TC4, _TC5, _TC6 ] = class_Actor:create_initial_actors( [

		% Now commented-out, as will not be used afterwards, and would trigger
		% warnings about empty plots:
		%
		%{ class_SodaVendingMachine, [ "Other soda machine", 5, 2.0 ] },

		{ class_StochasticThirstyCustomer,
			[ TC4Name, SVM2, {uniform,10}, 100.0 ], "George's Hint!" },

		{ class_StochasticThirstyCustomer,
			[ TC5Name, SVM2, {gaussian,4,2}, 250.0 ] },

		{ class_StochasticThirstyCustomer,
			[ TC6Name, SVM1, {gaussian,7,3}, 120.0 ] } ] ),

	% Ensures that creations are in-order indeed by checking the names of the
	% customers:
	%
	ExpectedNames = [ text_utils:string_to_binary( N )
						|| N <- [ TC4Name, TC5Name, TC6Name ] ],

	% Pattern-matches:
	ExpectedNames =
		[ begin
			TC ! { sayName, [], self() }, Name = test_receive(),
			%io:format( "Name of ~w is ~ts.~n", [ TC, Name ] ),
			Name
		  end || TC <- ActorPids ],


	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, for a stop at tick offset ~B.",
					[ StopTick ] ),

	RootTimeManagerPid ! { start, [ StopTick, self() ] },


	?test_info( "Waiting for the simulation to end, "
				"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	?case_stop.
