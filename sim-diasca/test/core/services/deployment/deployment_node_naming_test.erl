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


% @doc Overall unit test of the Sim-Diasca management of the various <b>node
% naming modes</b>.
%
-module(deployment_node_naming_test).


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
		simulation_name="Test of the management of node naming modes" },


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested), but computing
	% hosts are specified (to be updated depending on your environment):
	% (note that localhost is implied)

	% Differs from the defaults hence interesting to test:
	PreferredNodeNamingModes = [ short_name, long_name ],

	% Other values that can be tested:
	%PreferredNodeNamingModes = [ long_name, short_name ],
	%PreferredNodeNamingModes = [ short_name ],
	%PreferredNodeNamingModes = [ long_name ],
	%PreferredNodeNamingModes = [ bogus_value ],
	%PreferredNodeNamingModes = [],

	DeploymentSettings = #deployment_settings{

		computing_hosts =
			{ use_host_file_otherwise_local, "sim-diasca-host-candidates.txt" },

		% Reversed conventions compared to defaults:
		preferred_node_naming_modes = PreferredNodeNamingModes },

	?test_warning( "By default this test will not use an alternate EPMD port, "
		"as the overall engine settings have to be changed "
		"accordingly for this test to succeed." ),

	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	?test_notice_fmt( "This test will deploy a distributed simulation"
		" based on computing hosts specified as ~p, with following node "
		"naming conventions: ~p.",
		[ DeploymentSettings#deployment_settings.computing_hosts,
		  PreferredNodeNamingModes ] ),


	% Directly created on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								DeploymentSettings, LoadBalancingSettings ),


	?test_info( "Here we do not create any actor, "
				"thus the simulation will stop immediately." ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),


	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=120, self() ] },


	% Waits until simulation is finished:
	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,


	?test_info( "Requesting textual timings (second)." ),

	sim_diasca:shutdown(),

	?case_stop.
