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



% Overall unit test of the Sim-Diasca web-based result management facilities.
-module(web_result_management_test).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").



% Runs the test.
-spec run() -> no_return().
run() ->

	?case_start,

	% Default simulation settings (50Hz, batch reproducible) are used, except
	% for the name:
	%
	SimulationSettings = #simulation_settings{
		simulation_name="Web result management test" },


	% For this test of course we have to enable a local webserver; we rely here
	% on the default path to the root of the webserver installation (none is
	% specified here), namely ~/Software/sim_diasca_webserver_install_root
	% (possibly a symlink).
	%
	DeploymentSettings = #deployment_settings{ enable_webmanager=true },


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	?test_notice_fmt( "This test will deploy a distributed simulation"
		" based on computing hosts specified as ~p.",
		[ DeploymentSettings#deployment_settings.computing_hosts ] ),


	% Directly created on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								  DeploymentSettings, LoadBalancingSettings ),


	?test_info( "Creating directly (from case, not from an actor) "
				"some web probes." ),

	_MyTestActorPid = class_Actor:create_initial_actor( class_TestWebActor,
												   [ "My test web actor" ] ),

	FacilityProbeName = "My test facility web probe",

	% We do not want this test to fail because of a prior one:
	ProbeOutputFilename = "web-probe-My_test_facility_web_probe.html",

	file_utils:remove_file_if_existing( ProbeOutputFilename ),

	% Second is a facility one, meant to be generically accessed:
	%
	% (note that this test used to automatically remove at its end the file
	% produced by this probe - but it is now left behind)
	%
	MyFacilityProbePid = class_WebProbe:create_facility_probe(
						   FacilityProbeName ),

	MyFacilityProbePid ! { setMainContent,
			   [ "<p>Content of my test facility web probe.</p>" ], self() },
	content_set = test_receive(),


	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=120, self() ] },

	% Waits until simulation is finished:
	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	class_WebProbe:delete_facility_probe( MyFacilityProbePid ),

	sim_diasca:shutdown(),

	% Finally we prefer leaving behind the test, typically, a file named
	% web-probe-My_test_facility_web_probe.html (clearer):
	%
	% FacilityProbeFilename = class_WebProbe:get_filename_for( FacilityProbeName
	% ),
	%
	%file_utils:remove_file_if_existing( FacilityProbeFilename ),

	?case_stop.
