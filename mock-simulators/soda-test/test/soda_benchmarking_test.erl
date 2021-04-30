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


% Benchmarking case obtained from the soda deterministic example case.
%
% See also:
%
% - class_SodaVendingMachine.erl
%
% - class_DeterministicThirstyCustomer.erl
%
% - soda_full_loading_test.erl for the same simulated case using instance
% loading from file instead of instances created programmatically
%
-module(soda_benchmarking_test).


% To silence a Dialyzer warning:
-export([ run/1 ]).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").


% Shorthands:

-type count() :: basic_utils:count().

-type actor_pid() :: class_Actor:actor_pid().
-type tick_offset() :: class_Actor:tick_offset().



% Returns the main settings to choose the size of this test.
-spec get_benchmark_settings( 'minimal' | 'normal' ) ->
								{ count(), count(), tick_offset() }.
get_benchmark_settings( minimal ) ->

	MachineCount = 4,
	CustomerCount = 12,

	StopTick = 2000,

	{ MachineCount, CustomerCount, StopTick };

get_benchmark_settings( normal ) ->

	% Sets these parameters according to how numerous and powerful your
	% computing hosts are:
	%
	% (prefer having many more machines than customers, to ensure that no
	% machine will remain idle, leading to warnings when generating empty
	% plots):

	MachineCount = 60,
	CustomerCount = 360,

	% We want this test to end once a specified number of ticks are elapsed:
	%
	% (note that the simulation may end sooner, if no machine has a can anymore
	% and/or if no customer has enough money to buy a can in his machine of
	% choice)

	StopTick = 10000,

	{ MachineCount, CustomerCount, StopTick }.




% Creates the specified number of soda vending machines, and returns a list of
% their PID.
%
-spec create_vending_machines( count() ) -> [ actor_pid() ].
create_vending_machines( Count ) ->
	create_vending_machines( Count, _Acc=[] ).


% (helper)
create_vending_machines( _Count=0, Acc ) ->
	Acc;

create_vending_machines( Count, Acc ) ->

	MachineName = text_utils:format( "Soda machine #~B", [ Count ] ),

	% On average, a machine will hold 200 cans initially:
	InitialCanCount = 120
		+ class_RandomManager:get_positive_integer_gaussian_value(
			_Mu=80, _Sigma=5.0 ),

	% Any can of this machine will cost anything between 1 euro and 6 euros
	% (bounds included):
	%
	CanCost = float( class_RandomManager:get_uniform_value( 6 ) ),

	SVMPid = class_Actor:create_initial_actor( class_SodaVendingMachine,
					[ MachineName, InitialCanCount, CanCost ] ),

	create_vending_machines( Count-1, [ SVMPid | Acc ] ).



% Creates the specified number of thirsty customers, knowing each one soda
% vending machine among the specified ones, and returns a list of the PID of
% these customers.
%
-spec create_thirsty_customers( count(), [ actor_pid() ] ) -> [ actor_pid() ].
create_thirsty_customers( CustomerCount, VendingMachines ) ->
	create_thirsty_customers( CustomerCount, VendingMachines, _Acc=[] ).


create_thirsty_customers( _CustomerCount=0, _VendingMachines, Acc ) ->
	Acc;

create_thirsty_customers( CustomerCount, VendingMachines, Acc ) ->

	% 2/3 of them will be stochastic on average:
	CustomerPid = case class_RandomManager:get_uniform_value( 3 ) of

		1 ->
			create_deterministic_customer( CustomerCount, VendingMachines );

		_ ->
			create_stochastic_customer( CustomerCount, VendingMachines )

	end,

	create_thirsty_customers( CustomerCount-1, VendingMachines,
							  [ CustomerPid | Acc ] ).



% Creates a new deterministic thirsty customer, knowing one of the specified
% vending machines.
%
-spec create_deterministic_customer( count(), [ actor_pid() ] ) -> actor_pid().
create_deterministic_customer( CustomerCount, VendingMachines ) ->

	CustomerName = text_utils:format( "Customer #~B - deterministic",
								  [ CustomerCount ] ),

	ElectedMachineIndex = class_RandomManager:get_uniform_value(
							length( VendingMachines ) ),

	ElectedMachine = list_utils:get_element_at( VendingMachines,
												ElectedMachineIndex ),

	RepletionDuration = 250 + round( class_RandomManager:get_exponential_value(
									   _Lamba=0.05 ) ),

	InitialBudget = 15.0 + class_RandomManager:get_uniform_value( 200 ),

	class_Actor:create_initial_actor( class_DeterministicThirstyCustomer,
		  [ CustomerName, ElectedMachine, RepletionDuration, InitialBudget ] ).




% Creates a new stochastic thirsty customer, knowing one of the specified
% vending machines.
%
-spec create_stochastic_customer( count(), [ actor_pid() ] ) -> actor_pid().
create_stochastic_customer( CustomerCount, VendingMachines ) ->

	CustomerName = text_utils:format( "Customer #~B - stochastic",
									  [ CustomerCount ] ),

	ElectedMachineIndex =
		class_RandomManager:get_uniform_value( length( VendingMachines ) ),

	ElectedMachine =
		list_utils:get_element_at( VendingMachines,	ElectedMachineIndex ),

	MaxDuration = 250 + class_RandomManager:get_positive_integer_gaussian_value(
							_Mu=5, _Sigma=1.0 ),

	RepletionDuration = { uniform, MaxDuration },

	InitialBudget = 10.0 + class_RandomManager:get_uniform_value( 200 ),

	class_Actor:create_initial_actor( class_StochasticThirstyCustomer,
		  [ CustomerName, ElectedMachine, RepletionDuration, InitialBudget ] ).




% Runs the test.
-spec run() -> no_return().
run() ->
	run( _Settings=minimal ).
	%run( _Settings=normal ).


-spec run( 'minimal' | 'normal' ) -> no_return().
run( Settings ) ->

	?case_start,

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name="Soda Benchmarking Test"

	  % We leave it to the default specification (all_outputs):
	  % result_specification =
	  %  [ { targeted_patterns, [ {".*",[data_and_rendering] } ] },
	  %    { blacklisted_patterns, ["^Second" ] } ]

	  %result_specification = [ { targeted_patterns, [ {".*",data_only} ] } ]

	},


	DeploymentSettings = #deployment_settings{

		% Note that the configuration file below does not have to be declared
		% above as well:
		%
		enable_data_exchanger={ true, [ "soda_parameters.cfg" ] },

		enable_performance_tracker=true },


	% A deployment manager is created directly on the user node:
	DeploymentManagerPid =
		sim_diasca:init( SimulationSettings, DeploymentSettings ),

	{ MachineCount, CustomerCount, StopTick } =
		get_benchmark_settings( Settings ),

	% Accounts for next actors as well:
	?test_info_fmt( "This benchmark case will involve "
		"~B soda vending machines, ~B customers "
		"and will stop no later than tick offset #~B.",
		[ MachineCount + 2, CustomerCount + 3, StopTick ] ),


	% First machine starts with 10 cans, 2 euros each:
	SVM1 = class_Actor:create_initial_actor( class_SodaVendingMachine,
		[ _FirstMachineName="First soda machine", _FirstInitialCanCount=1000,
		  _FirstCanCost=2.0 ] ),

	% Second machine starts with 8 cans, 1.5 euro each:
	SVM2 = class_Actor:create_initial_placed_actor( class_SodaVendingMachine,
		[ _SecondMachineName="Second soda machine", _SecondInitialCanCount=8,
		  _SecondCanCost=1.5 ], _PlacementHint=gimme_some_shelter ),


	% First customer uses SVM1, is thirsty 1 minute after having drunk, and has
	% 6 euros in his pockets:
	_TC1 = class_Actor:create_initial_actor( class_DeterministicThirstyCustomer,
		[ _FirstCustomerName="John", _FirstKnownMachine=SVM1,
		  _FirstRepletionDuration=1, _FirstInitialBudget=6.0 ] ),


	% Second customer uses SVM1 too, is thirsty 3 minutes after having drunk,
	% and has 8 euros in his pockets:
	_TC2 = class_Actor:create_initial_actor( class_DeterministicThirstyCustomer,
		[ _SecondCustomerName="Terry", _SecondKnownMachine=SVM1,
		  _SecondRepletionDuration=3, _SecondInitialBudget=80.0 ] ),


	% Third customer uses SVM2, is thirsty 2 minutes after having drunk, and has
	% 15 euros in his pockets:
	_TC3 = class_Actor:create_initial_actor( class_DeterministicThirstyCustomer,
		[ _ThirdCustomerName="Michael", _ThirdKnownMachine=SVM2,
		  _ThirdRepletionDuration=2, _ThirdInitialBudget=15.0 ] ),

	% Now some batch creations for this test:

	VendingMachines = create_vending_machines( MachineCount ),

	_Customers = create_thirsty_customers( CustomerCount, VendingMachines ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, "
					"for a stop at tick offset ~B.", [ StopTick ] ),

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
