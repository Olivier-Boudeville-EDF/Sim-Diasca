% Copyright (C) 2012-2021 EDF R&D

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


% This is the root Sim-Diasca module, to be called from most simulation cases,
% hiding the details of the simulation services being used underneath, and
% offering opportunities for single-place updates with no impact on existing
% cases.
%
-module(sim_diasca).


-export([ init/1, init/2, init/3, get_simulation_name/1, is_running/0,
		  create_initial_instances/1, notify_hint/1,
		  run_simulation/2, run_simulation_and_browse_results/2, shutdown/0,
		  start_for_test/0 ]).


% Simulation UUID (ex: for node cookies):
-type simulation_uuid() :: id_utils:uuid().


% Simulation Instance Identifier (possibly user-defined):
-type sii() :: ustring().


-type simulation_identifiers() :: { simulation_uuid(), sii() }.


% PID of an agent of the engine (an engine base object):
-type agent_pid() :: pid().


-export_type([ simulation_uuid/0, sii/0, simulation_identifiers/0,
			   agent_pid/0 ]).


% For case_main_process_name:
-include("engine_common_defines.hrl").

% For load_balancing_settings() and all:
-include("class_LoadBalancer.hrl").

% For deployment_settings and all:
-include("class_DeploymentManager.hrl").

% For simulation_settings:
-include("class_TimeManager.hrl").

% For case_main_process_name:
-include("case_defines.hrl").


% For notify_warning_fmt and al:
-include_lib("traces/include/traces.hrl").

% For ?trace_aggregator_name:
-include_lib("traces/include/class_TraceAggregator.hrl").

-define( registration_scope, global_only ).


% Shorthand:
-type ustring() :: text_utils:ustring().



% Initialises the engine according to specified settings, using default
% deployment and load-balancing settings.
%
% Returns the PID of the deployment manager.
%
-spec init( simulation_settings() ) -> deployment_manager_pid().
init( SimulationSettings ) ->
	init( SimulationSettings, #deployment_settings{} ).



% Initialises the engine according to specified settings, using default
% load-balancing settings.
%
% Returns the PID of the deployment manager.
%
-spec init( simulation_settings(), deployment_settings() ) ->
					deployment_manager_pid().
init( SimulationSettings, DeploymentSettings ) ->
	init( SimulationSettings, DeploymentSettings, #load_balancing_settings{} ).



% Initialises the engine according to specified settings.
%
% Returns the PID of the deployment manager.
%
-spec init( simulation_settings(), deployment_settings(),
			load_balancing_settings() ) -> deployment_manager_pid().
init( SimulationSettings, DeploymentSettings, LoadBalancingSettings )
  when is_record( SimulationSettings, simulation_settings ) andalso
	   is_record( DeploymentSettings, deployment_settings ) andalso
	   is_record( LoadBalancingSettings, load_balancing_settings ) ->

	% We explicitly force the use of the Unicode encoding, as apparently a
	% side-effect of running the VM with the -noinput option (which is usually
	% the case here) is to switch the current encoding to latin1 (then for
	% example terminal outputs become scrambled):
	%
	system_utils:force_unicode_support(),

	% Takes care of the simulation UUID and SII:
	{ SimUUID, SII } = SimIdentifiers = get_simulation_identifiers(),

	trace_utils:notice_fmt( "Simulation instance identifier is '~ts'.",
							[ SII ] ),

	% We need to rename this user node so that it bears a conventional name
	% (deriving from the simulation case, and including the SII) - yet this can
	% only be done by starting from a non-distributed node (done by the
	% Sim-Diasca make rules, relying on the --nn option):

	SimulationName = get_simulation_name( SimulationSettings ),

	% We wanted the trace system to be autonomous (ex: so that it can be used
	% before the initialisation of the engine and after its shutdown); as a
	% result, from ?case_start a default trace filename was created and used
	% (ex: 'my_foobar_case.traces').
	%
	% Now that the engine is being initialised, we request the trace file to be
	% renamed, so that it also includes the user and the SII (ex:
	% 'my_foobar_case-by-boudevil-94.traces'):
	%
	NewTraceFilename = file_utils:convert_to_filename( SimulationName ++ "-by-"
								++ system_utils:get_user_name() ++ "-" ++ SII
								++ ?TraceExtension ),

	TraceAggregatorPid = naming_utils:get_registered_pid_for(
							?trace_aggregator_name, global ),

	% We have to rename the trace file (ex: to include the SII):
	TraceAggregatorPid ! { renameTraceFile, [ NewTraceFilename ] },

	TraceAggregatorPid ! { getTraceType, [], self() },

	TraceType = receive

		{ wooper_result, { notify_trace_type, Type } } ->
			Type

	end,

	%trace_utils:debug( "Initialising the trace supervisor." ),

	% Now is the first time at which we can run the trace supervisor, since the
	% trace filename is not expected to change anymore:
	%
	% (there used to be a possible, slight race condition here, if ever the
	% renameTraceFile message arrived too late, yet a side-effect of the - now
	% used - call to the getTraceType/1 request is to make the renaming
	% synchronous as well)
	%
	class_TraceSupervisor:init( NewTraceFilename, TraceType,
								TraceAggregatorPid, self() ),

	notify_conditional_settings(),


	% EPMD must be running prior to having a node go distributed:
	%
	% We select here at which port this launched EPMD will run, yet the current
	% VM apparently got the EPMD port that it will contact at start-up, and it
	% does not seem possible to change it.
	%
	% As a result, the EPMD ports in the launch command (see the EPMD_PORT
	% variable in myriad/GNUmakevars.inc) and in the deployment settings (see
	% its firewall_restrictions field) must match.
	%
	case class_DeploymentManager:interpret_firewall_options(
		   DeploymentSettings ) of

		{ _EPMDPort=undefined, _TcpRangeOption } ->
			% We use here the default Sim-Diasca EPMD port:
			net_utils:launch_epmd();

		{ EPMDPort, _TcpRangeOption } ->
			system_utils:set_environment_variable( "ERL_EPMD_PORT",
									text_utils:format( "~B", [ EPMDPort ] ) ),
			net_utils:launch_epmd( EPMDPort )

	end,

	NodePrefix = class_DeploymentManager:get_node_name_prefix_from(
					SimulationName, SII ),

	% We rename this user node accordingly:
	UserNodeName = NodePrefix ++ "-user-node",

	net_utils:enable_distribution( UserNodeName, long_name ),

	% Simulations will never step over others (previous ones):
	Cookie = text_utils:string_to_atom( SimUUID ),

	% All spawned nodes will be given later the current cookie of this node:
	erlang:set_cookie( node(), Cookie ),

	% Detailed checking of this field done later, by the deployment manager:
	case DeploymentSettings#deployment_settings.crash_resilience of

		K when is_integer( K ) andalso K > 0 ->
			% Here, an actual resilience is wanted. As a result, this current
			% process (i.e. the one of the simulation case) shall resist to any
			% node loss, thus must trap exits (ex: for initial - linked - actors
			% that were running on a crashed node).  However, process crashes
			% should not remain silent, thus EXIT messages will be searched for
			% later.
			%
			process_flag( trap_exit, _ResistExitMsg=true );

		_ ->
			ok

	end,

	% We register this process (the one of the simulation case), so that I can
	% be found by others, like the resilience manager:
	%
	naming_utils:register_as( ?case_main_process_name, ?registration_scope ),

	% Simply returns this PID, for later use:
	%
	% (we kept the link with the user process corresponding to the simulation
	% case, as if no resilience had been requested we want to stop whenever a
	% node crashed, and with resilience enabled we trap exits, and are thus able
	% to detect crashes nevertheless)
	%
	class_DeploymentManager:synchronous_new_link( SimulationSettings,
			DeploymentSettings, LoadBalancingSettings, SimIdentifiers,
			deploy_from_scratch );


% One set of settings is invalid:
init( SimulationSettings, DeploymentSettings, LoadBalancingSettings )
  when is_record( DeploymentSettings, deployment_settings ) andalso
	   is_record( LoadBalancingSettings, load_balancing_settings ) ->
	throw( { invalid_simulation_settings, SimulationSettings } );

init( SimulationSettings, DeploymentSettings, LoadBalancingSettings )
  when is_record( SimulationSettings, simulation_settings ) andalso
	   is_record( LoadBalancingSettings, load_balancing_settings ) ->
	throw( { invalid_deployment_settings, DeploymentSettings } );

init( SimulationSettings, DeploymentSettings, LoadBalancingSettings )
  when is_record( SimulationSettings, simulation_settings ) andalso
	   is_record( DeploymentSettings, deployment_settings ) ->
	throw( { invalid_load_balancing_settings, LoadBalancingSettings } );

% Even worse, at least two are invalid:
init( SimulationSettings, DeploymentSettings, LoadBalancingSettings ) ->
	throw( { invalid_settings, SimulationSettings, DeploymentSettings,
			 LoadBalancingSettings } ).


% Notifies the user about the currently-enforced condition settings, in order to
% be able to be able to remain aware of what is currently compiled (note that
% this applies only to the current module, yet compilation options are expected
% to be uniform across modules).
%
-spec notify_conditional_settings() -> void().
notify_conditional_settings() ->

	% There is no more reliable monitoring of these settings:

	ExecStr = "Running in following execution target: "
		++ cond_utils:if_defined( exec_target_is_production, "production",
								  "development" ) ++ " mode.",

	% These are compile-time constructs so one shall not try here to factorize
	% these calls:
	%
	DebugTopicStrs = [

		"model behaviours " ++ cond_utils:if_defined(
			simdiasca_debug_model_behaviours, "enabled", "disabled" ),

		"user calls to engine API " ++ cond_utils:if_defined(
			simdiasca_debug_user_api_calls, "enabled", "disabled" ),

		"time-management " ++ cond_utils:if_defined(
			simdiasca_debug_time_management, "enabled", "disabled" ),

		"initial actor creations " ++ cond_utils:if_defined(
			simdiasca_debug_initial_creations, "enabled", "disabled" ),

		"runtime actor creations " ++ cond_utils:if_defined(
			simdiasca_debug_runtime_creations, "enabled", "disabled" ),

		"actor life cycles " ++ cond_utils:if_defined(
			simdiasca_debug_life_cycles, "enabled", "disabled" ) ],

	CheckTopicStrs = [

		"model behaviours " ++ cond_utils:if_defined(
			simdiasca_check_model_behaviours, "enabled", "disabled" ),

		"user calls to engine API " ++ cond_utils:if_defined(
			simdiasca_check_user_api_calls, "enabled", "disabled" ),

		"time-management " ++ cond_utils:if_defined(
			simdiasca_check_time_management, "enabled", "disabled" ),

		"initial actor creations " ++ cond_utils:if_defined(
			simdiasca_check_initial_creations, "enabled", "disabled" ),

		"runtime actor creations " ++ cond_utils:if_defined(
			simdiasca_check_runtime_creations, "enabled", "disabled" ),

		"actor life cycles " ++ cond_utils:if_defined(
			simdiasca_check_life_cycles, "enabled", "disabled" ) ],

	class_TraceEmitter:send_standalone( info, ExecStr ++ text_utils:format(
		"~nRegarding the activation of engine-level conditional debug "
		"topics: ~ts~nRegarding the conditional check topics: ~ts",
		[ text_utils:strings_to_string( DebugTopicStrs ),
		  text_utils:strings_to_string( CheckTopicStrs ) ] ),
		  _EmitterCateg="Core" ).



% Tells whether the engine is running (i.e. has been initialised and not been
% stopped yet).
%
-spec is_running() -> boolean().
is_running() ->

	% We rely on the registration of the case process for that:
	case naming_utils:is_registered( ?case_main_process_name,
									 ?registration_scope ) of

		not_registered ->
			false;

		_ ->
			true

	end.



% Creates (synchronously) the initial instances, from specified file.
-spec create_initial_instances( file_utils:file_path() ) -> void().
create_initial_instances( _FilePath ) ->

	% Currently one shall use the initialisation_files field of the
	% simulation_settings instead:
	%
	throw( not_implemented_yet ).



% Notifies the user of an engine-level hint.
%
% This is typically used to suggest that some kind of model-level defect is the
% culprit for a detected error.
%
-spec notify_hint( ustring() ) -> void().
notify_hint( Message ) ->
	io:format( "[hint] ~ts.~n", [ Message ] ).



% Runs the actual simulation, until reaching the stop tick or any prior
% termination criterion.
%
-spec run_simulation( class_TimeManager:tick(), pid() ) -> void().
run_simulation( StopTick, DeploymentManagerPid ) ->

	% As some processes (ex: the time manager) have the PID of this simulation
	% case process in their state, it must be declared too to the corresponding
	% instance tracker (now that it has been deployed):
	%
	class_InstanceTracker:register_agent( ?case_main_process_name ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },

	RootTimeManagerPid = traces:receive_applicative_message(),

	?notify_notice_fmt( "Starting simulation, "
						"for a stop at tick offset ~B.", [ StopTick ] ),

	RootTimeManagerPid ! { start, [ StopTick, self() ] },

	?notify_info( "Waiting for the simulation to end, "
				  "since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?notify_info( "Simulation stopped spontaneously, "
						  "specified stop tick must have been reached." )

	end.



% Runs the actual simulation, until reaching the stop tick, and allows the user
% to browse the corresponding results, if it succeeded.
%
-spec run_simulation_and_browse_results( class_TimeManager:tick(), pid() ) ->
												void().
run_simulation_and_browse_results( StopTick, DeploymentManagerPid ) ->

	run_simulation( StopTick, DeploymentManagerPid ),

	?notify_info( "Browsing the report results, if in batch mode." ),

	class_ResultManager:browse_reports().



% Shutdowns the engine.
-spec shutdown() -> void().
shutdown() ->

	% Stateless, hence resilience-friendly.

	case naming_utils:is_registered( ?deployment_manager_name, global ) of

		not_registered ->
			ok;

		DeployPid ->
			class_DeploymentManager:shutdown( DeployPid )

	end,

	naming_utils:unregister( ?case_main_process_name, global_only ),

	check_exit_messages().



% Allows to support both OTP conventions and ad hoc, automatic ones.
-spec start_for_test() -> void().
start_for_test() ->
	trace_utils:info( "Starting Sim-Diasca test environment." ),
	wooper_utils:start_for_test().



% Lists any EXIT messages that would linger in mailbox.
-spec check_exit_messages() -> void().
check_exit_messages() ->

	receive

		{ 'EXIT', _From, _Reason=normal } ->
			% Ignored:
			check_exit_messages();

		{ 'EXIT', From, Reason } ->
			?notify_warning_fmt( "process whose PID was ~w had exited "
								 "with reason '~p'.~n", [ From, Reason ] ),
			check_exit_messages()

	after 0 ->

		% Stop recursing:
		ok

	end.




% Returns the simulation identifiers: determines both the simulation UUID and
% the SII (potentially derived from it, if not specified by the user).
%
-spec get_simulation_identifiers() -> simulation_identifiers().
get_simulation_identifiers() ->

	% In all cases an UUID will be needed:
	UUID = id_utils:generate_uuid(),

	EmitterName = "Case" ,

	EmitterCategorization = "Core.Deployment",

	MessageCategorization = uncategorized,

	SII = case shell_utils:get_command_arguments_for_option(
				 '-simulation-instance-id' ) of

		undefined ->
			% No SII defined by the user, hence deducing it from UUID.

			% Will lead to a shorter, more human-tractable string (up to 9
			% numerical characters, instead of 36 alphanumerical ones for the
			% UUID):
			%
			DeducedSII = text_utils:integer_to_string( erlang:phash2( UUID ) ),

			?notify_fmt_em( "Using simulation UUID '~ts', and, no SII having "
				"been specified by the user, a UUID-derived one, SII '~ts'.",
				[ UUID, DeducedSII ], EmitterName, EmitterCategorization,
				MessageCategorization ),

			DeducedSII;


		[ [ UserSpecifiedSII ] ] when is_list( UserSpecifiedSII )
									  andalso UserSpecifiedSII =/= "" ->
			?notify_fmt_em( "Using simulation UUID '~ts', and "
				"user-specified SII '~ts'.", [ UUID, UserSpecifiedSII ],
				EmitterName, EmitterCategorization, MessageCategorization ),
			UserSpecifiedSII;

		[ [ UserSpecifiedSII ] ] ->
			throw( { invalid_user_specified_simulation_instance_id,
					 UserSpecifiedSII } );

		OtherSIIArg ->
			throw( { multiple_user_specified_simulation_instance_id,
					 OtherSIIArg } )

	end,

	{ UUID, SII }.



% Returns the (string) simulation name obtained from the specified simulation
% settings.
%
-spec get_simulation_name( simulation_settings() ) -> ustring().
get_simulation_name( #simulation_settings{ simulation_name=AtomName } )
  when is_atom( AtomName ) ->
	text_utils:atom_to_string( AtomName );

get_simulation_name( #simulation_settings{ simulation_name=StringName } )
  when is_list( StringName ) ->
	StringName;

get_simulation_name( #simulation_settings{ simulation_name=UnexpectedName } ) ->
	throw( { invalid_simulation_name, UnexpectedName } ).
