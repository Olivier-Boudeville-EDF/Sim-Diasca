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


% @doc <b>Benchmarking case</b> obtained from the soda deterministic example
% case.
%
% See also:
%
% - class_SodaVendingMachine.erl
% - class_DeterministicThirstyCustomer.erl
% - soda_benchmarking_test for a counterpart test case, exactly the same except
% that it creates its initial instances programmatically
%
-module(soda_integral_loading_test).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").


% @doc Runs the test.
-spec run() -> no_return().
run() ->

	?case_start,

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

		simulation_name="Soda Integral Loading Test",

		initialisation_files=[ "soda-integral-loading-test.init" ] },


	DeploymentSettings = #deployment_settings{

		% Note that the configuration file below has not to be declared above as
		% well:
		%
		enable_data_exchanger = { true, [ "soda_parameters.cfg" ] } },


	% A deployment manager is created directly on the user node:
	DeploymentManagerPid =
		sim_diasca:init( SimulationSettings, DeploymentSettings ),

	StopTick = 50000,

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
