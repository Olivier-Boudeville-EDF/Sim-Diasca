% Copyright (C) 2024-2024 EDF R&D
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
% Creation date: Monday, January 15, 2024.


% @doc <b>Integration test</b> for the <b>testing of graph stream probes</b>.
%
% See the class_GraphStreamProbe.erl module.
%
-module(graph_stream_probe_test).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").


% @doc Runs the tests.
-spec run() -> no_return().
run() ->

	?case_start,

	?test_info( "Testing the use of graph stream probes." ),

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

		simulation_name="Graph Stream Probe Integration Test"

		% Using default simulation frequency (50Hz, period of 20ms).

		% We leave it to the default specification (all_outputs):
		%result_specification =
		% [ { targeted_patterns, [ {".*",[data_and_rendering] } ] },
		%   { blacklisted_patterns, ["^Second" ] } ]

		%result_specification = no_output

	},


	%_MaybeProjectPath=undefined,
	MaybeProjectPath=
		"../../../../../../../myriad/test/data-management/test-project.gephi",

	% Specifies the list of computing hosts that can be used:
	%
	% (see the sim-diasca-host-candidates-sample.txt example in the
	% sim-diasca/conf directory)
	%
	DeploymentSettings = #deployment_settings{
		%enable_graph_streaming=false,
		enable_graph_streaming={ true,

			MaybeProjectPath,

			%_WorkspaceName="Test workspace" } },
			_WorkspaceName="siclone" } },


	% A deployment manager is created directly on the user node:
	DeploymentManagerPid =
		sim_diasca:init( SimulationSettings, DeploymentSettings ),


	_GSTA1 = class_Actor:create_initial_actor( class_TestGraphStreamActor,
											   [ "First test actor" ] ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	StopTick = 2000,

	?test_info_fmt( "Starting simulation, "
					"for a stop at tick offset ~B.", [ StopTick ] ),

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
