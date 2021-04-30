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


-module(class_DeploymentManager).


-define( class_description,
		 "Its role is to manage the deployment of a full simulation over the "
		 "distributed resources. "
		 "To do so, it uses one specific manager per computing host, in charge "
		 "of its setting-up, then deployment. "
		 "More precisely, the deployment manager is a simulation agent whose "
		 "main role is:"
		 " - to centralize, on the user behalf, most simulation information "
		 "and settings"
		 " - to determine on which host and node all other simulation agents "
		 "(ex: the load balancer, the root time manager, etc.) should run, and "
		 "then to launch them accordingly, with the help of managers of "
		 "computing hosts" ).


% See also class_DeploymentManager.hrl and class_ComputingHostManager.erl.


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).


% The class-specific attributes of a deployment manager:
-define( class_attributes, [

	{ simulation_settings, simulation_settings(),
	  "stores the simulation settings" },

	{ deployment_settings, deployment_settings(),
	  "stores the deployment settings" },

	{ load_balancing_settings, load_balancing_settings(),
	  "stores the load-balancing settings" },

	{ simulation_uuid, sim_diasca:simulation_uuid(), "stores the (unique) UUID "
	  "corresponding to this simulation instance" },

	{ simulation_instance_id, sim_diasca:sii(), "stores the (possibly "
	  "user-defined) SII (Simulation Instance Identifier)" },

	{ engine_root_dir, directory_path(), "corresponds to the absolute root "
	  "directory of the sources of a Sim-Diasca installation or clone, "
	  "i.e. to the directory that contains directly 'myriad', 'wooper',
	  'traces', the 'sim-diasca' directory itself, etc." },

	{ deployment_base_dir, file_utils:path(), "corresponds to the absolute "
	  "base directory of the deployment to be done on each computing node" },

	{ database_running, boolean(),
	  "tells whether the database is currently available" },

	{ host_infos, [ computing_host_info() ], "records information about the "
	  "computing hosts involved in the simulation" },

	{ compute_scheduler_count, maybe( count() ), "tells if a "
	  "specific number of sequencers shall be created on each computing node" },

	{ epmd_port, maybe( net_utils:tcp_port() ), "stores any non-default TCP "
	  "port to be used for the EPMD daemon" },

	{ tcp_port_range, net_utils:tcp_port_restriction(), "records any "
	  "restriction to apply regarding the range of used TCP ports" },

	{ ping_allowed, boolean(), "tells whether ping (ICMP) messages may be used "
	  "to test host availability" },

	{ deploy_time_out, milliseconds(), "is the maximum number of "
	  "milliseconds for an host to be deployed successfully" },

	{ shutdown_initiated, boolean(),
	  "tells whether an (emergency) shutdown is in progress" },

	{ troubleshooting_mode, troubleshooting_mode(), "tells whether the "
	  "troubleshooting mode is currently enabled" },

	{ resilience_level, count(), "the maximum number of computing "
	  "nodes whose loss should be recoverable in the course of the "
	  "simulation" },

	{ node_naming_mode, net_utils:node_naming_mode(), "is either 'short_name' "
	  "or 'long_name', depending on how the user node was launched (the "
	  "naming mode of the user node can be set by overriding the NODE_NAMING "
	  "make variable, in the GNUmakevars.inc file of Myriad)" },

	{ load_balancer_pid, load_balancer_pid(), "the PID of the load balancer" },

	{ root_time_manager_pid, time_manager_pid(),
	  "the PID of the root time manager" },

	{ local_time_managers, [ time_manager_pid() ], "a list of the PID of all "
	  "local (non-root) time managers" },

	{ data_logger_pid, data_logger_pid(), "the PID of the (main) data-logger" },

	{ result_manager_pid, result_manager_pid(),
	  "the PID of the result manager" },

	{ web_manager_pid, maybe( web_manager_pid() ),
	  "the PID of the web manager (if any)" },

	{ root_data_exchanger_pid, maybe( data_exchanger_pid() ),
	  "PID of the root data exchanger (if any)" },

	{ root_instance_tracker_pid, instance_tracker_pid(),
	  "PID of the root instance tracker" },

	{ local_instance_trackers, [ instance_tracker_pid() ],
	  "a list of the PID of all local (non-root) instance trackers" },

	{ plugin_manager_pid, plugin_manager_pid(), "PID of the plugin manager" },

	{ performance_tracker_pid, maybe( performance_tracker_pid() ),
	  "PID of the performance tracker (if any)" },

	{ resilience_manager_pid, resilience_manager_pid(),
	  "PID of the resilience manager" },

	{ resilience_manager_ref, maybe( reference() ),
	  "a monitor reference onto the resilience manager (if any)" } ] ).


% Exported as called from a fun:
-export([ inspect_archive/2 ]).


% Helpers, currently non-used, exported to avoid warning:
-export([ get_hostname_for/2, get_username_for/2, halt_on_error/1,
		  halt_on_error/2 ]).


-type manager_pid() :: sim_diasca:agent_pid().


% The various types of nodes involved:
-type node_type() :: 'computing_node' | 'user_node'.


% Specification of the context of a simulation:
-type simulation_context() :: 'deploy_from_scratch' | tuple().


-export_type([ manager_pid/0, node_type/0, simulation_context/0,
			   elements_to_deploy/0 ]).


% Local types:

-type host_user_list() :: [ { net_utils:string_host_name(),
							  basic_utils:user_name() } ].

-type simulation_name() :: ustring().


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Deployment.Manager" ).


% To retrieve the versions of most prerequisites:
%
% (header generated automatically by the 'all-pre-hook' make target of the root
% makefile)
%
-include("package-versions.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% For static outputs:
-include_lib("traces/include/traces.hrl").

% For trace_aggregator_name:
-include_lib("traces/include/class_TraceAggregator.hrl").


% For simulation_settings:
-include("class_TimeManager.hrl").


% For deployment_settings:
-include("class_DeploymentManager.hrl").


% For load_balancing_settings:
-include("class_LoadBalancer.hrl").


% For performance tracker settings:
-include("class_PerformanceTracker.hrl").


% For the technical_settings record:
-include("sim_diasca_plugin.hrl").


% For resilience_manager_name:
-include("class_ResilienceManager.hrl").


% For host_static_info:
-include_lib("myriad/include/system_utils.hrl").

% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").

% For the #binding_manager record:
-include("bindings.hrl").


% Including for engine_arg_root_key:
-include("engine_common_defines.hrl").


% Implementation notes.

% Note that we preferred to avoid any direct communication between this
% deployment manager and a (remote) deployment agent: there is a process in the
% middle, the computing host manager.



% For services based on singletons:
%
% (node_name() defined in class_ResilienceManager.hrl)
%
-type centralised_placement() :: node_name().

% For services with one manager and multiple (distributed) agents:
-type distributed_placement() :: { node_name(), [ node_name() ] }.



% Describes a dispatching of the agents of the simulation services onto a set of
% nodes.
%
-record( service_placement, {

	% LoadBalancerNode:
	load_balancing :: centralised_placement(),

	% { RootTimeManagerNode, LocalTimeManagerNodes }:
	time_management :: distributed_placement(),

	% DataLoggerNode:
	data_logging :: centralised_placement(),

	% { RootDataExchangerNode, LocalDataExchangerNodes }:
	data_exchanging :: distributed_placement(),

	% ResultManagerNode:
	result_management :: centralised_placement(),

	% { RootInstanceTrackerNode, LocalInstanceTrackerNodes }:
	instance_tracking :: distributed_placement(),

	% PerformanceTrackerNode:
	performance_tracking :: distributed_placement(),

	% { ResilienceManagerNode, ResilienceAgentNodes }:
	resilience_management :: distributed_placement(),

	% { BindingManagersNode, BindingResourcesNodes }:
	bindings_management :: distributed_placement() } ).


-type service_placement() :: #service_placement{}.



% Typical BEAM file included in the build tree of an Erlang/OTP version (used in
% order to detect when such an Erlang build tree is included by mistake in a
% simulation archive):
%
-define( typical_erlang_otp_beam_file, "gen_statem.beam" ).


% Shorthands:

%-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type atom_node_name() :: net_utils:atom_node_name().

%-type milliseconds() :: unit_utils:milliseconds().

-type file_path() :: file_utils:file_path().
-type file_name() :: file_utils:file_name().

%-type directory_path() :: file_utils:directory_path().
-type directory_name() :: file_utils:directory_name().
-type bin_directory_name() :: file_utils:bin_directory_name().


-type host_manager_pid() :: class_ComputingHostManager:host_manager_pid().


% For purely local (non-distributed) simulations, note that the mode of
% operation relies on the fact that the current node is run as a potentially
% networked one (with the short or long names interpreter option), which is
% enforced by the Sim-Diasca defaults.

% Once the load balancer is created, the local host is not managed differently
% from the other hosts, as we prefer relying on a clean and separated second
% runtime environment, for an increased homogeneity and reliability.
%
% Moreover the name of the initial user node would most probably not correspond
% to the simulation name.

% If a simulation is interrupted, then still running nodes could be left behind
% and, at the next simulation run, they could be reused whereas they are still
% using modules which were updated since then. This is why we use a
% node-cleaner.sh script each time the simulation is run. Morever an UUID-based
% unique cookie system has been integrated to ensure no agent mismatch can
% happen between two simulations.


% Warning: this class is expected to be executed in the context of the user
% node, not on any of the deployed computing nodes, thus it will use the test
% code path (BEAM directories), *not* the one set by the deployment agent.
%
% This is useful to remember whenever having crashes due to 'undef' functions
% (actually their module is not found at all).





% Constructs a new deployment manager, from following parameters:
%
% - SimulationSettings: see the simulation_settings record defined in
% class_TimeManager.hrl
%
% - DeploymentSettings: see the deployment_settings record defined in
% class_DeploymentManager.hrl
%
% - LoadBalancingSettings: see the load_balancing_settings record defined in
% class_LoadBalancer.hrl
%
% - Context: tells whether it is a deployment from scratch (with no prior
% deployment), or a redeployment (with a pre-existing context, that must be
% specified)
%
% See class_TimeManager and class_LoadBalancer for further details.
%
-spec construct( wooper:state(), simulation_settings(), deployment_settings(),
			load_balancing_settings(), sim_diasca:simulation_identifiers(),
			simulation_context() ) -> wooper:state().
construct( State,
		   SimulationSettings=#simulation_settings{},
		   DeploymentSettings=#deployment_settings{},
		   LoadBalancingSettings=#load_balancing_settings{},
		   _SimIdentifiers={ SimUUID, SII },
		   Context=deploy_from_scratch ) ->

	% Corresponds to an initial deployment.

	% May be useful, but in some cases almost freezes everything:
	%etop:start(),

	InterNodeSeconds = get_inter_node_tick_time_out(),

	% This deployment manager will run directly on the user node, which thus
	% will adopt a longer time-out. As launched (computing) nodes will do the
	% same thanks to the deployment agent, all involved nodes will respect the
	% same convention:
	%
	% (typically the first run returns 'change_initiated')
	%
	TickChange = net_kernel:set_net_ticktime( InterNodeSeconds ),

	% To catch any failing computing node:
	monitor_utils:monitor_nodes( _NewSubscription=true ),

	% So that the deployment manager is able to react to the failure of
	% processes implementing simulation services (ex: time managers):
	%
	erlang:process_flag( trap_exit, true ),

	% First, keep track of launching time:
	StartTimestamp = time_utils:get_timestamp(),

	% We want to catch the start ASAP:
	PluginManagerPid = set_up_plugin_management( DeploymentSettings ),

	PluginConfChanges = class_PluginManager:notify_simulator_start(),


	SimulationName = sim_diasca:get_simulation_name( SimulationSettings ),

	Name = "Deployment manager for " ++ SimulationName,

	% First the direct mother classes:
	InitialState = class_EngineBaseObject:construct( State,
									?trace_categorize(Name) ),

	TraceState = setAttribute( InitialState, compute_scheduler_count,
							   undefined ),

	% Plugins may have requested configuration changes:
	ChangedState = apply_configuration_changes( PluginConfChanges, TraceState ),

	{ EpmdPortOption, TcpRangeOption } =
		interpret_firewall_options( DeploymentSettings ),

	PingAllowed = interpret_ping_option( DeploymentSettings ),

	TmpDir = determine_temporary_directory( DeploymentSettings ),

	DeploymentBaseDir = get_deployment_base_directory_for( SimulationName,
										TmpDir, StartTimestamp, SII ),

	EngineRootDir = determine_root_directory(),

	CleanUpSettings = get_clean_up_settings( DeploymentSettings, EngineRootDir,
											 ChangedState ),

	DeployTimeOut = get_deploy_time_out( DeploymentSettings, ChangedState ),

	NodeNamingMode = net_utils:get_node_naming_mode(),

	UserSettings = { _SimulationName, _SimInteractivityMode,
		_UIInteractivityMode, _TickDuration, _EvaluationMode,
		TroubleShootingMode, NodeAvailabilityTolerance, _DataLoggerWanted,
		_WebManagerInfos, _DataExchangerSettings, _LanguageBindings,
		_PlacementPolicy, _InitialisationFiles, _ResultSpecification,
	   ResilienceLevel, _FullSettings={ NewSimulationSettings,
		  NewDeploymentSettings, NewLoadBalancingSettings } } =
		determine_user_settings( SimulationSettings, DeploymentSettings,
								 LoadBalancingSettings ),

	% The 'make' executable is expected to be found in the path, knowing that
	% its location depends on the distribution (ex: /bin/make or /usr/bin/make).
	%
	% (needed for the purpose of rebuilding the deployed content)
	%
	PathOfMake = executable_utils:find_executable( "make" ),

	% trace_utils:debug_fmt( "Engine root directory: ~s.", [ EngineRootDir ] ),

	StartingState = setAttributes( ChangedState, [
		{ simulation_settings, NewSimulationSettings },
		{ deployment_settings, NewDeploymentSettings },
		{ load_balancing_settings, NewLoadBalancingSettings },
		{ simulation_uuid, SimUUID },
		{ simulation_instance_id, SII },
		{ engine_root_dir, EngineRootDir },
		{ make_path, PathOfMake },
		{ node_naming_mode, NodeNamingMode },
		{ node_cleanup, CleanUpSettings },
		{ deployment_base_dir, DeploymentBaseDir },
		{ database_running, false },
		{ plugin_manager_pid, PluginManagerPid },
		{ epmd_port, EpmdPortOption },
		{ tcp_port_range, TcpRangeOption },
		{ ping_allowed, PingAllowed },
		{ deploy_time_out, DeployTimeOut },
		{ shutdown_initiated, false },
		{ troubleshooting_mode, TroubleShootingMode },
		{ resilience_level, ResilienceLevel } ] ),

	% Read from the generated package-versions.hrl file:
	VersionStrings = { MyriadVersionString, WOOPERVersionString,
		TracesVersionString, SimDiascaVersionString } = get_version_string(),

	Cookie = erlang:get_cookie(),

	% Use ~ts to protect from Unicode strings:
	?send_info_fmt( StartingState,
			 "Creating a new deployment manager with simulation settings:~ts "
			 "~ncompleted with deployment settings:~ts "
			 "~nand with load balancing settings:~ts"
			 "~nVersions are:~n"
			 " - for the Erlang user virtual machine: ~s\n"
			 " - for the Myriad layer: ~s\n"
			 " - for the WOOPER layer: ~s\n"
			 " - for the Traces layer: ~s\n"
			 " - for Sim-Diasca engine: ~s\n"
			 "~nSystem settings are: ~ts\n"
			 "~nNetwork settings are: ~ts\n"
			 "Deployment time-out is set to ~s.\n"
			 "Simulation instance identifier (SII) is '~s'.\n"
			 "Simulation UUID is '~s' (and so cookie is '~s').\n"
			 "Current tick change policy is: ~p.",
			 [ class_TimeManager:settings_to_string( SimulationSettings ),
			   settings_to_string( DeploymentSettings ),
			   class_LoadBalancer:settings_to_string( LoadBalancingSettings ),
			   system_utils:get_interpreter_version(),
			   MyriadVersionString,
			   WOOPERVersionString,
			   TracesVersionString,
			   SimDiascaVersionString,
			   system_utils:get_system_description(),
			   get_network_description(),
			   time_utils:duration_to_string( DeployTimeOut ),
			   SII,
			   SimUUID,
			   Cookie,
			   TickChange ] ),

	% Ensures also it is a singleton indeed:
	naming_utils:register_as( ?deployment_manager_name, global_only ),

	% HostUserList is a list of { Hostname, Username } string pairs:
	HostUserList = get_host_user_list( DeploymentSettings, StartingState ),

	BaseNodeName = get_computing_node_prefix_from( SimulationName, SII ),

	class_PluginManager:notify( on_deployment_start ),

	AdditionalBEAMDirs = get_additional_beam_dirs( DeploymentSettings ),

	AdditionalBEAMBinDirs =
		[ text_utils:string_to_binary( D ) || D <- AdditionalBEAMDirs ],

	% Initiates (triggers) the parallel setting-up of the various hosts involved
	% as soon as possible:
	%
	SetUpState = set_up_computing_nodes( BaseNodeName, HostUserList,
					InterNodeSeconds, AdditionalBEAMBinDirs, StartingState ),

	% Declared on this (user) node as well:
	code_utils:declare_beam_directories( AdditionalBEAMDirs ),

	% In the meantime, prepare (in parallel) the next step, the longest task to
	% come being generally, by far, the creation (if any) of the full deployment
	% package:
	%
	{ SimulationPackageFilename, ArchiveSelectedFiles } =
		manage_simulation_package( SetUpState ),

	% In parallel, out of the critical path, we check that no two files selected
	% have the same name (albeit of course in different directories):
	%
	% (answer managed in interpret_archive_inspection/1)
	%
	inspect_archive_content( ArchiveSelectedFiles ),

	% Collects each set-up notification and then triggers its deployment:
	%
	% (AvailableHosts is a list of computing_host_info records corresponding to
	% elected computing hosts)
	%
	{ AvailableHosts, FailedHosts, CollectState } =
		process_setup_outcome( SimulationPackageFilename, SetUpState ),


	% List of atoms (note that all failed computing hosts have been removed):
	SelectedNodes = interpret_setup_outcome( AvailableHosts, FailedHosts,
									NodeAvailabilityTolerance, CollectState ),

	trace_utils:notice_fmt( "Use cookie '~s' to connect to the nodes "
							"(user or computing ones).", [ Cookie ] ),

	class_PluginManager:notify( on_technical_settings_available,
				#technical_settings{ computing_nodes=SelectedNodes,
									 cookie=Cookie } ),

	% Now that the BEAM files are available, we can plan where agents will be:
	ServicePlacement = dispatch_agents( SelectedNodes, NodeNamingMode ),

	?send_info( CollectState, service_placement_to_string( ServicePlacement ) ),

	RuntimeSettings = { EngineRootDir, StartTimestamp, VersionStrings,
				AvailableHosts, SelectedNodes, BaseNodeName, ServicePlacement },

	% And then we actually create the right, well-placed simulation agents:
	ServiceState = set_up_simulation_services( UserSettings, RuntimeSettings,
											   Context, CollectState ),

	% Now the instance tracking service is available:
	class_InstanceTracker:register_agent( class_PluginManager,
										  PluginManagerPid ),

	% Receives information from inspect_archive_content/1:
	interpret_archive_inspection( ServiceState ),

	class_PluginManager:notify( on_deployment_stop ),

	% All simulation services are available now.

	class_PluginManager:notify( on_case_initialisation_start ),


	% We force a synchronous creation (less concurrent, yet a lot simpler for
	% case developers):
	%
	% (hopefully many actions will have been anticipated)
	%
	LoadBalancerPid = getAttribute( ServiceState, load_balancer_pid ),

	% Oneway sent, as we need the load balancer not to be blocked in a request
	% call, as it will delegate its own work (to the instance loader) while
	% remaining responsive during the process (to place newly read instances):
	%
	LoadBalancerPid ! { createInitialInstancesFromFiles,
						[ self(), EngineRootDir ] },

	receive

		instances_created_from_files ->
			ok

	end,

	setAttribute( ServiceState, host_infos, AvailableHosts );





% This clause allows for a redeployment (onto an already deployed system), most
% probably after node crash(es). In that case, the nodes specified in the
% context are the ones that survived.
%
% Note: deserves an update, not currently functional.
%
construct( State, SimulationSettings, DeploymentSettings, LoadBalancingSettings,
		   _SimIdentifiers={ SimUUID, SII },
		   Context={ Nodes, StartTimestamp, RootDir, AvailableHosts } ) ->

	% Note: this clause (and, more generally, the whole resilience system),
	% shall be updated w.r.t. to many new features. Consider it as being
	% currently non-functional as a whole.

	% Redeployment, hence many different steps (ex: no simulation package, no
	% resilience infrastructure to launch, etc.) are to be taken:

	UserSettings = { SimulationName, _SimInteractivityMode,
					 _UIInteractivityMode, _TickDuration,
					 _EvaluationMode, TroubleShootingMode,
					 _NodeAvailabilityTolerance, DataLoggerWanted,
					 _WebManagerInfos, DataExchangerSettings, _LanguageBindings,
					 _PlacementPolicy, _InitialisationFiles,
					 _ResultSpecification, ResilienceLevel, _FullSettings } =
		determine_user_settings( SimulationSettings, DeploymentSettings,
								 LoadBalancingSettings ),

	% StartTimestamp obtained here from context.

	Name = "Deployment manager for " ++ SimulationName,

	% First the direct mother classes:
	TraceState = class_EngineBaseObject:construct( State,
												   ?trace_categorize(Name) ),

	?send_notice_fmt( TraceState, "Redeploying on following nodes: ~s",
					  [ text_utils:atoms_to_string( Nodes ) ] ),


	% No need to perform again global operations, like the setting of VM net
	% tick time, cookie, firewall options, SSH clean-up and launches, etc.

	% Let's recreate the state of this new deployment manager:

	{ EpmdPort, TcpRange } =
		interpret_firewall_options( DeploymentSettings ),

	TmpDir = determine_temporary_directory( DeploymentSettings ),


	DeploymentBaseDir = get_deployment_base_directory_for( SimulationName,
							TmpDir, StartTimestamp, SII ),

	% RootDir obtained here from context:
	CleanUpSettings = get_clean_up_settings( DeploymentSettings, RootDir,
											 TraceState ),

	DeployTimeOut = get_deploy_time_out( DeploymentSettings, TraceState ),

	DataLoggerWanted = check_data_logger_wanted( DeploymentSettings ),

	DataExchangerSettings = check_data_exchanger_settings( DeploymentSettings ),

	PathOfMake = executable_utils:find_executable( "make" ),

	SetState = setAttributes( TraceState, [
		{ simulation_settings, SimulationSettings },
		{ deployment_settings, DeploymentSettings },
		{ load_balancing_settings, LoadBalancingSettings },
		{ simulation_uuid, SimUUID },
		{ simulation_instance_id, SII },
		{ engine_root_dir, RootDir },
		{ make_path, PathOfMake },
		{ node_naming_mode, net_utils:get_node_naming_mode() },
		{ node_cleanup, CleanUpSettings },
		{ deployment_base_dir, DeploymentBaseDir },

		% Behaviour in case of crash to be determined:
		{ database_running, false },
		{ epmd_port, EpmdPort },
		{ tcp_port_range, TcpRange },
		{ deploy_time_out, DeployTimeOut },
		{ troubleshooting_mode, TroubleShootingMode },
		{ resilience_level, ResilienceLevel },
		{ host_infos, AvailableHosts } ] ),

	% Ensures also it is a singleton indeed, despite re-launching:
	naming_utils:register_as( ?deployment_manager_name, global_only ),

	% No host set-up, deployment of a simulation package, etc., just:
	ServicePlacement =
		dispatch_agents( Nodes, net_utils:get_node_naming_mode() ),

	?send_info_fmt( SetState, "Redeployed service placement: ~s~n",
					[ service_placement_to_string( ServicePlacement ) ] ),

	VersionStrings = get_version_string(),

	SelectedNodes = get_node_names( AvailableHosts ),

	BaseNodeName = get_node_name_prefix_from( SimulationName, SII ),

	RuntimeSettings = { RootDir, StartTimestamp, VersionStrings, AvailableHosts,
						SelectedNodes, BaseNodeName, ServicePlacement },

	set_up_simulation_services( UserSettings, RuntimeSettings, Context,
								SetState ).




% Returns a textual description of the network settings on the current (user)
% host.
%
% (helper)
%
-spec get_network_description() -> ustring().
get_network_description() ->

	NetworkFilenames = [ "/etc/hosts", "/etc/resolv.conf" ],

	FileStrings = [ case file_utils:is_existing_file( Filename ) of

		true ->
			FileContent = file_utils:read_whole( Filename ),
			text_utils:format( "content of '~s' is:~n"
				"\"\"\"~n~s\"\"\"~n", [ Filename, FileContent ] );

		false ->
			text_utils:format( "(no '~s' file found)", [ Filename ] )

					end || Filename <- NetworkFilenames ],

	HostnameCmd = executable_utils:find_executable( "hostname" ),

	OptPairs = [ { "alias names are", "aliases" },
				 { "DNS domain name is", "domain" },
				 { "DNS host name or FQDN is", "fqdn" },
				 { "IP addresses for the host name are", "ip-addresses" },
				 { "short host name is", "short" },
				 { "NIS/YP domain name is", "nis" } ],

	HostStrings = lists:foldl(
		fun( { Label, Opt }, AccStrings ) ->
			Cmd = text_utils:format( "~s --~s", [ HostnameCmd, Opt ] ),
			{ _ReturnCode, CmdOutput } = system_utils:run_command( Cmd ),
			Optstring = text_utils:format( "the ~s: ~s",
										   [ Label, CmdOutput ] ),
			[ Optstring | AccStrings ]
		end,
		_Acc0=[],
		% To respect finally the original order:
		lists:reverse( OptPairs ) ),

	text_utils:strings_to_string( FileStrings ++ HostStrings ).



% Returns a list of the (absolute, normalised) additional BEAM directories.
%
% (helper)
%
-spec get_additional_beam_dirs( deployment_settings() ) -> [ directory_name() ].
get_additional_beam_dirs( #deployment_settings{
							 additional_beam_directories=InitialBEAMDirs,
							 enable_language_bindings=Languages } ) ->

	LanguagesWithoutCodePaths = [ case Elem of

		{ Lang, _Path } ->
			Lang;

		Lang ->
			Lang

								  end || Elem <- Languages ],

	% Adds the BEAM paths required by the dependencies of the language bindings
	% that are going to be used by the simulation:
	%
	Dirs = InitialBEAMDirs
		++ language_utils:get_additional_beam_directories_for(
			 LanguagesWithoutCodePaths ),

	% The existence of listed directories will be checked when setting the local
	% code path afterwards (on each node):
	%
	lists:foldl(

	  fun( Dir, AccDirList ) ->

		NormDir = file_utils:normalise_path( Dir ),

		case file_utils:is_absolute_path( NormDir ) of

			true ->
				[ NormDir | AccDirList ];

			false ->
				throw( { non_absolute_additional_beam_dir, NormDir } )

		end

	  end,
	  _Acc0=[],
	  % To preserve final order:
	  _List=lists:reverse( Dirs ) ).



% Checks asynchronously that, in the specified list of file paths, no two of
% them designate the same filename, and that no filename is the sign of a
% problem (typically that Erlang/OTP modules might be erroneously selected)
%
-spec inspect_archive_content( [ file_path() ] ) -> void().
inspect_archive_content( ArchiveSelectedFiles ) ->

	Self = self(),

	?myriad_spawn_link( ?MODULE, inspect_archive,
						[ ArchiveSelectedFiles, Self ] ).



% Checks that, in the specified list of file paths, no two of them designate the
% same filename, and that no filename is the sign of a problem (typically that
% Erlang/OTP modules might be erroneously selected).
%
-spec inspect_archive( [ file_path() ], pid() ) -> no_return().
inspect_archive( ArchiveSelectedFiles, TargetPid ) ->

	% First, registers and checks all full paths, per basename:
	{ FilenameTable, ProblemFiles } = lists:foldl(
		fun( FullPath, _Acc={ TableAcc, PbFiles } ) ->

			%trace_utils:debug_fmt( " - inspecting '~s'", [ FullPath ] ),

			Basename = filename:basename( FullPath ),

			NewTableAcc = table:append_to_entry( Basename, FullPath, TableAcc ),

			NewPbFiles = case Basename of

				?typical_erlang_otp_beam_file ->
					[ FullPath | PbFiles ];

				_ ->
					PbFiles

			end,

			{ NewTableAcc, NewPbFiles }

		end,
		_Acc0={ table:new(), _ProblemFiles=[] },
		_List=ArchiveSelectedFiles ),

	% Then returns the per-basename path duplicates:
	DuplicateList = lists:foldl(
		fun

			( { _Basename, _PathList=[ _P ] }, Acc ) ->
				% Only present once, ok then:
				Acc;

			% Entry is { Basename, PathList } with length( PathList ) > 1:
			( Entry, Acc ) ->
				[ Entry | Acc ]

		end,
		_OtherAcc0=[],
		_OtherList=table:enumerate( FilenameTable ) ),

	% We nevertheless filter-out the possible duplicates that are known not to
	% be problematic:
	%
	NonProblematicsFiles = [ ".gitignore", "GNUmakefile" ],

	FilteredDuplicateList  = lists:foldl(
		fun( E={ Basename, _PathList }, Acc ) ->

			case lists:member( Basename, NonProblematicsFiles ) of

				true ->
					% Not a problem then, drop that entry:
					Acc;

				false ->
					% Problem, thus kept:
					[ E | Acc ]

			end

		end,
		[],
		DuplicateList ),

	TargetPid ! { notify_archive_inspection, FilteredDuplicateList,
				  ProblemFiles }.



% Blocks until the message sent from inspect_archive_content/1 is received, and
% examines the corresponding outcome.
%
-spec interpret_archive_inspection( wooper:state() ) -> void().
interpret_archive_inspection( State ) ->

	% Waits for the answer from inspect_archive_content/1:
	receive


		{ notify_archive_inspection, _DuplicateList=[], _ProblemFiles=[] } ->
			ok;


		% First ensure that there is no problem file:
		{ notify_archive_inspection, _DuplicateList=[], ProblemFiles } ->

			case file_utils:is_leaf_among( ?typical_erlang_otp_beam_file,
										   ProblemFiles ) of

				false ->
					?warning_fmt( "Following problematic files have been "
						"detected in your simulation archive: ~s",
						[ text_utils:strings_to_string( ProblemFiles ) ] );


				OTPFullPath ->
					?warning_fmt( "It looks like that at least a part "
						"of Erlang/OTP itself is included in your "
						"simulation archive (the '~s' file has "
						"been spotted, in '~s'). "
						"Please ensure that no foreign element "
						"(typically an Erlang build tree) "
						"has been included by mistake in the "
						"directories listed for your deployment archive.",
						[ ?typical_erlang_otp_beam_file,
						  filename:dirname( OTPFullPath ) ] )

			end;


		% Then there must be here at least some duplicates:

		% A single duplicate:
		{ notify_archive_inspection, _DuplicateList=[ { Basename, FullPaths } ],
		  _ProblemFiles } ->

			?error_fmt( "The '~s' file appears ~B times in the "
				"simulation archive, as: ~s", [ Basename, length( FullPaths ),
				text_utils:strings_to_string( FullPaths,
											  _IndentationLevel=1 ) ] ),

			throw( { duplicated_filename_in_archive, Basename } );


		% Multiple duplicates:
		{ notify_archive_inspection, DuplicateList, _ProblemFiles } ->

			DupStrings = [ text_utils:format(
							 "~B duplicates found for filename '~s', in: ~s",
							 [ length( FullPaths ), Basename,
							   text_utils:strings_to_string( FullPaths,
												 _IndentationLevel=1 ) ] )
						   || { Basename, FullPaths } <- DuplicateList ],

			?error_fmt( "~B different filenames appear more than once in the "
				"simulation archive: ~s", [ length( DupStrings ),
				  text_utils:strings_to_string( DupStrings ) ] ),

			throw( { duplicated_filenames_in_archive,
				[ Basename || { Basename, _FullPaths } <- DuplicateList ] } )

	end.



% Sets up all simulation services one by one, and in-order, and returns an
% updated state referencing them.
%
% (helper)
%
set_up_simulation_services(

		_UserSettings={ SimulationName, SimInteractivityMode,
			UIInteractivityMode, TickDuration, EvaluationMode,
			TroubleShootingMode, NodeAvailabilityTolerance, DataLoggerWanted,
			WebManagerInfos, DataExchangeSettings, LanguageBindings,
			PlacementPolicy, InitialisationFiles, ResultSpecification,
			_ResilienceLevel, FullSettings={ _SimulationSettings,
				DeploymentSettings, _LoadBalancingSettings } },

		_RuntimeSettings={ EngineRootDir, StartTimestamp, VersionStrings,
			AvailableHosts, SelectedNodes, BaseNodeName, ServicePlacement },

		Context,

		State ) ->

	?debug( "Setting up simulation services." ),

	% We now set-up simulation services one by one, and in-order:

	{ RootInstanceTrackerPid, LocalInstanceTrackers } =
		set_up_instance_tracking( TroubleShootingMode, ServicePlacement ),


	{ RootTimeManagerPid, LocalTimeManagers } = set_up_time_management(
	   TroubleShootingMode, SimInteractivityMode, TickDuration,
	   RootInstanceTrackerPid, ServicePlacement, Context ),

	SII = ?getAttr(simulation_instance_id),

	{ ResultManagerPid, DataLoggerPid, ResultDir } =
		set_up_result_management_and_datalogging( SimulationName,
			StartTimestamp, SII, EngineRootDir, VersionStrings, TickDuration,
			ResultSpecification, AvailableHosts, RootTimeManagerPid,
			DataLoggerWanted, ServicePlacement, Context ),

	MaybeWebManagerPid =
		set_up_web_management( SII, EngineRootDir, UIInteractivityMode,
							   ResultManagerPid, ResultDir, WebManagerInfos ),

	AllInstanceTrackers = [ RootInstanceTrackerPid | LocalInstanceTrackers ],

	LoadBalancerPid = set_up_load_balancing( PlacementPolicy,
		SelectedNodes, NodeAvailabilityTolerance, EvaluationMode,
		TroubleShootingMode, RootTimeManagerPid, AllInstanceTrackers,
		EngineRootDir, ServicePlacement, InitialisationFiles ),

	RootDataExchangerPid = set_up_data_exchanging( DataExchangeSettings,
		RootTimeManagerPid, BaseNodeName, ?getAttr(engine_root_dir),
		ServicePlacement ),

	BindingManagers = set_up_binding_managers( LanguageBindings, EngineRootDir,
							?getAttr(epmd_port), ServicePlacement, State ),

	% All settings shall be transmitted to the resilience manager, so that they
	% are not lost in case of rollback:
	%
	{ ResilienceManagerPid, ResilienceManagerRef } = case Context of

		deploy_from_scratch ->

			set_up_resilience_management( FullSettings, RootTimeManagerPid,
				ResultManagerPid, StartTimestamp, SII, EngineRootDir,
				AvailableHosts, ServicePlacement );


		_OtherContext ->

			% This is the sole manager expected to be still running then:
			ResilManagerPid =
				naming_utils:get_registered_pid_for( ?resilience_manager_name ),

			% We are a new instance:
			ResilManagerRef = erlang:monitor( process, ResilManagerPid ),

			{ ResilManagerPid, ResilManagerRef }

	end,

	% Last one to be created (if wanted), as depends on most services:
	PerformanceTrackerPid = set_up_performance_tracking( DeploymentSettings,
		ResultDir, RootTimeManagerPid, SelectedNodes, AllInstanceTrackers,
		LoadBalancerPid, Context ),


	setAttributes( State, [

		{ root_instance_tracker_pid, RootInstanceTrackerPid },
		{ local_instance_trackers, LocalInstanceTrackers },

		{ root_time_manager_pid, RootTimeManagerPid },
		{ local_time_managers, LocalTimeManagers },

		{ result_manager_pid, ResultManagerPid },
		{ data_logger_pid, DataLoggerPid },
		{ web_manager_pid, MaybeWebManagerPid },

		{ load_balancer_pid, LoadBalancerPid },
		{ root_data_exchanger_pid, RootDataExchangerPid },

		{ binding_managers, BindingManagers },

		{ resilience_manager_pid, ResilienceManagerPid },
		{ resilience_manager_ref, ResilienceManagerRef },

		{ performance_tracker_pid, PerformanceTrackerPid } ] ).




% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%trace_utils:debug_fmt( "Destructor of deployment manager ~p called.",
	%                        [ self() ] ),

	% Class-specific actions:
	?info( "Deleting deployment manager." ),

	ShutdownState = shutdown_services( State ),

	?debug( "Deployment manager deleted." ),

	% Then allows chaining:
	ShutdownState.



% Performs a global teardown of the simulation services: orderly shutdown.
-spec shutdown_services( wooper:state() ) -> wooper:state().
shutdown_services( State ) ->

	% We shutdown services step-by-step, from the top-level (ex: time manager)
	% to the bottom-ones (ex: data exchanger), avoiding to have in the same
	% level (thus in parallel deletion) coupled services (ex: time manager and
	% load balancer - which is an actor).
	%
	% (distributed services will remove by themselves all their agents, based on
	% their respective root agents)

	case ?getAttr(resilience_manager_ref) of

		undefined ->
			ok;

		MonitorRef ->
			erlang:demonitor( MonitorRef )

	end,

	% We use here safe_* deletions services, as if a runtime error occurred,
	% because of the links, some services may already be dead, and we want to
	% avoid waiting a synchronous time-out for each of them.

	% First, deletes the performance tracker and unrelated higher-level
	% services, if running:
	%
	FirstDeletedState =
		wooper:safe_delete_synchronously_any_instance_referenced_in(
		[ performance_tracker_pid, root_data_exchanger_pid,
		  resilience_manager_pid, web_manager_pid ], State ),

	SecondDeletedState =
		wooper:safe_delete_synchronously_any_instance_referenced_in(
		[ root_time_manager_pid, result_manager_pid ], FirstDeletedState ),


	class_PluginManager:notify_if_registered( on_simulator_stop ),

	ThirdDeletedState =
		wooper:safe_delete_synchronously_any_instance_referenced_in(
		[ data_logger_pid, load_balancer_pid, plugin_manager_pid ],
		SecondDeletedState ),

	case class_InstanceTracker:is_local_tracker_registered() of

		not_registered ->
			ok;

		_ ->

			class_InstanceTracker:unregister_agent(
			  class_TraceAggregator:get_aggregator( _CreateIfNotFound=false ) ),

			% Unregisters this deployment manager as well:
			class_InstanceTracker:unregister_agent()

	end,

	% First internal service to run, last to be removed; triggers a recursive
	% removal of all child instance trackers:
	%
	FourthDeletedState =
		wooper:safe_delete_synchronously_any_instance_referenced_in(
		root_instance_tracker_pid, ThirdDeletedState ),

	% Ensures that in all cases the database is shut down (even though it is not
	% started directly by the deployment manager):
	%
	{ FifthDeletedState, _Res } =
		executeRequest( FourthDeletedState, deactivateDatabase ),

	wooper:safe_delete_synchronously_instances(
	  get_host_managers( FifthDeletedState ) ),

	setAttribute( FifthDeletedState, host_infos, [] ).




% Now we try to collect the setup outcome for each and every host manager: some
% may succeed (then they anticipate on the next phase and are sent their
% simulation package), some may fail, some may never answer or answer too late.
%
% This allows also to desynchronise the sendings of the simulation archive.
%
% Returns {AvailableHosts, FailedHosts, NewState}, where:
%
% - AvailableHosts is a list of computing_host_info records that correspond to
% hosts on which the deployment succeeded
%
% - FailedHosts is a list of {FailComputingHostPid, FailReason} pairs, where
% FailComputingHostPid is the PID of the computing host which corresponds to a
% failed host, and FailReason is an atom describing the failure reason
%
% - NewState is an updated state
%
% (helper)
%
-spec process_setup_outcome( file_name(), wooper:state() ) ->
	{ [ computing_host_info() ],
	  [ { host_manager_pid(), basic_utils:error_type() } ], wooper:state() }.
process_setup_outcome( SimulationPackageFilename, State ) ->

	% We will wait for the specified duration for non-responding managers after
	% the last valid manager answered:

	% Margin, in milliseconds, so that in the worst case all time-outs at the
	% computing host level will fire before this one (otherwise this manager
	% might receive notifications from them whereas it already timed-out and
	% went through the next steps, which should be avoided):
	%
	Margin = 1500,

	% Milliseconds:
	CollectTimeOut = ?getAttr(deploy_time_out) + Margin,

	% We start by waiting for all managers to set-up:
	Waited = get_host_managers( State ),

	?debug_fmt( "Waiting (as ~s, with cookie '~s') the deployment report "
		"from ~B host managers (~p), using a time-out of ~s.",
		[ node(), net_utils:get_cookie(), length( Waited ), Waited,
		  time_utils:duration_to_string( CollectTimeOut ) ] ),

	wait_setup_outcome( Waited, _Available=[], _Failed=[], CollectTimeOut,
			text_utils:string_to_binary( SimulationPackageFilename ), State ).



% We maintain lists of the PID of computing host managers that are initially
% waited, then over time migrate to available or failed, explicitly or
% implicitly (on time-out).
%
wait_setup_outcome( _Waited=[], Available, Failed, _CollectTimeOut,
					_BinPackageFilename, State ) ->
	% All answered, or time-out was triggered:
	{ Available, Failed, State };

wait_setup_outcome( Waited, Available, Failed, CollectTimeOut,
					BinPackageFilename, State ) ->

	% Default case; we hijack the WOOPER main loop:
	receive

		{ getPackage, [], HostManagerPid } ->

			% Side-effect: resets the overall time-out.
			HostManagerPid ! { wooper_result, BinPackageFilename },

			wait_setup_outcome( Waited, Available, Failed, CollectTimeOut,
								BinPackageFilename, State );


		{ onHostDeploymentSuccess, [ HostManagerPid, HostInfos ] } ->

			% The full list was already set up in set_up_computing_nodes/5, but
			% the host_info subentries were not available then:
			%
			InitialComputingHostInfo = get_host_info( HostManagerPid, State ),

			NewComputingHostInfo = InitialComputingHostInfo#computing_host_info{
									   host_infos=HostInfos },

			?info_fmt( "Received notification of set-up success from "
				"manager ~w: deployment on host '~s' starts now, "
				"with user ~s, for a target node named ~s.",
				[ HostManagerPid,
				  NewComputingHostInfo#computing_host_info.host_name,
				  NewComputingHostInfo#computing_host_info.user_name,
				  NewComputingHostInfo#computing_host_info.node_name ] ),

			NewWaited = lists:delete( HostManagerPid, Waited ),

			NewAvailable = [ NewComputingHostInfo | Available ],

			wait_setup_outcome( NewWaited, NewAvailable, Failed, CollectTimeOut,
								BinPackageFilename, State );


		{ onHostDeploymentFailure, [ HostManagerPid, Reason ] } ->

			#computing_host_info{ host_name=Hostname,
								  user_name=Username,
								  node_name=Nodename } =
				get_host_info( HostManagerPid, State ),

			?warning_fmt( "Received notification of deployment failure "
				"from manager ~w: host '~s' with user '~s' "
				"will not take part to this simulation "
				"(reason: ~s), its node (~s) has been shut down.",
				[ HostManagerPid, Hostname, Username,
				  interpret_host_failure( Reason ), Nodename ] ),

			NewWaited = lists:delete( HostManagerPid, Waited ),

			NewFailed = [ { HostManagerPid, Reason } | Failed ],

			wait_setup_outcome( NewWaited, Available, NewFailed,
								CollectTimeOut, BinPackageFilename, State )


	after CollectTimeOut ->

			% Note that each host manager that answered previously reset this
			% timer.

			% Notifies waited host managers as early as possible, to minimise
			% the risk they answer in the meantime.

			% All the managers that did not answer are supposed faulty, we thus
			% give up using them and will delete them later, which in turn will
			% shutdown their nodes:
			FailedHostsByTimeOut =
				[ { W, deployment_time_out } || W <- Waited ],

			% Here all pending managers are rejected:
			FailedHostInfos = [ begin

				 #computing_host_info{
					host_name=Host,
					user_name=User,
					node_name=Node } = get_host_info( W, State ),

				 text_utils:format( "host '~s' for user '~s', using computing "
					"host manager ~w for target node '~s'",
					[ Host, User, W, Node ] )

								end || W <- Waited ],

			?error_fmt( "Deployment failed for following hosts (they failed to "
				"report their deployment status on time; "
				"reset collection time-out: ~s), therefore "
				"giving up deploying the simulation on them: ~s",
				[ time_utils:duration_to_string( CollectTimeOut ),
				  text_utils:strings_to_string( FailedHostInfos ) ] ),

			NewFailed = FailedHostsByTimeOut ++ Failed,

			wait_setup_outcome( _Waited=[], Available, NewFailed,
								CollectTimeOut, BinPackageFilename, State )

	end.




% Member methods section.


% A computing host manager that has been removed because of a time-out may send
% a onHostDeploymentFailure message too late to be intercepted by the loop
% above, which will trigger a method call (thus this void body has to defined).
%
% (const pseudo oneway)
%
-spec onHostDeploymentFailure( wooper:state(), host_manager_pid(), term() ) ->
									const_oneway_return().
onHostDeploymentFailure ( State, _HostManagerPid, _Reason ) ->

	% Just ignore that now useless message:
	wooper:const_return().



% Returns the PID of the load balancer.
-spec getLoadBalancer( wooper:state() ) ->
							 const_request_return( load_balancer_pid() ).
getLoadBalancer( State ) ->
	wooper:const_return_result( ?getAttr(load_balancer_pid) ).



% Returns the PIDs of the binding managers, through an appropriate record.
-spec getBindingManagers( wooper:state() ) ->
								const_request_return( binding_managers() ).
getBindingManagers( State ) ->
	wooper:const_return_result( ?getAttr(binding_managers) ).



% Returns the PID of the root time manager.
-spec getRootTimeManager( wooper:state() ) ->
								const_request_return( time_manager_pid() ).
getRootTimeManager( State ) ->
	wooper:const_return_result( ?getAttr(root_time_manager_pid) ).



% Returns the PID of the result manager.
-spec getResultManager( wooper:state() ) ->
							const_request_return( result_manager_pid() ).
getResultManager( State ) ->
	wooper:const_return_result( ?getAttr(result_manager_pid) ).



% Returns the PID of the web manager.
-spec getWebManager( wooper:state() ) ->
							const_request_return( web_manager_pid() ).
getWebManager( State ) ->
	wooper:const_return_result( ?getAttr(web_manager_pid) ).




% Oneway triggered by the nodeup messages enabled by (option-less) node
% monitoring.
%
-spec nodeup( wooper:state(), atom_node_name() ) ->
					const_oneway_return().
nodeup( State, NewlyConnectedNodeName ) ->

	% Presumably a computing node:
	?debug_fmt( "A new node connected: '~s'.", [ NewlyConnectedNodeName ] ),

	wooper:const_return().



% Oneway triggered by the nodeup messages enabled by (option-less) node
% monitoring.
%
-spec nodedown( wooper:state(), atom_node_name() ) -> oneway_return().
nodedown( State, DisconnectedNodeName ) ->

	DownState = case ?getAttr(shutdown_initiated) of

		true ->
			State;

		false ->
			?emergency_fmt( "The '~s' node disconnected, performing an "
							"emergency shutdown.", [ DisconnectedNodeName ] ),
			trigger_emergency_shutdown( _ExitCode=2, State )

	end,

	wooper:return_state( DownState ).



% Callback triggered whenever a new node connected to this one (should node
% monitoring with options be enabled).
%
-spec onWOOPERNodeConnection( wooper:state(), atom_node_name(),
			  monitor_utils:monitor_info() ) -> const_oneway_return().
onWOOPERNodeConnection( State, NewlyConnectedNodeName, MonitorNodeInfo ) ->

	% (silences the default WOOPER handler)

	% Presumably a computing node:
	?debug_fmt( "A new node connected: '~s' (monitor info: ~p).",
				[ NewlyConnectedNodeName, MonitorNodeInfo ] ),

	wooper:const_return().



% Callback triggered whenever a node disconnected from this one (should node
% monitoring with options be enabled).
%
-spec onWOOPERNodeDisconnection( wooper:state(), atom_node_name(),
			  monitor_utils:monitor_info() ) -> oneway_return().
onWOOPERNodeDisconnection( State, DisconnectedNodeName, MonitorNodeInfo ) ->

	% (silences the default WOOPER handler)

	DownState = case ?getAttr(shutdown_initiated) of

		true ->
			State;

		false ->
			?emergency_fmt( "The '~s' node disconnected (monitor info: ~p), "
							"performing an emergency shutdown.",
							[ DisconnectedNodeName, MonitorNodeInfo ] ),
			trigger_emergency_shutdown( _ExitCode=2, State )

	end,

	wooper:return_state( DownState ).



% Called whenever a linked process exits (ex: a time manager on a computing node
% exiting because one of its local actors itself exited).
%
-spec onWOOPERExitReceived( wooper:state(), pid() | port(), term() ) ->
								  const_oneway_return().
onWOOPERExitReceived( State, _PidOrPort, _ExitType=normal ) ->

	% Normal exits are to be ignored.

	%trace_utils:debug_fmt( "(ignoring normal exit for process or port ~p)",
	%						[ PidOrPort ] ),

	wooper:const_return();


onWOOPERExitReceived( State, PidOrPort, ExitType ) ->

	case ?getAttr(shutdown_initiated) of

		true ->
			% Multiple links to the deployment manager are generally triggered
			% in case of runtime issues, only taking into account the first:
			%

			%trace_utils:debug_fmt(
			%          "(exit signal '~p' received from ~p ignored, as "
			%		   "already performing an emergency shutdown)",
			%		   [ ExitType, PidOrPort ] ),

			wooper:const_return();

		false ->
			% Short message wanted:
			?emergency_fmt( "Error exit signal received ('~p'), performing "
						"now an emergency simulation shutdown.", [ ExitType ] ),

			% Sent here to avoid too much detail on the console:
			?notice_fmt( "(the exit signal '~p' was sent by ~p)",
				[ ExitType, PidOrPort ] ),

			% Would not be a good idea, as (exactly as the associated computing
			% host manager) would attempt to interact with the now defunct
			% deployment agent:
			%
			%ShutdownState = trigger_emergency_shutdown( _ExitCode=1, State ),

			% Note: probably that the associated link should not be created, as
			% the corresponding computing host manager has also its own
			% onWOOPERExitReceived/3 triggered for the same reason.

			basic_utils:stop_on_failure( 10 ),

			wooper:const_return()

	end.



% Triggers an (asynchronous) emergency shutdown.
%
% (helper)
%
-spec trigger_emergency_shutdown( system_utils:return_code(),
								  wooper:state() ) -> wooper:state().
trigger_emergency_shutdown( ExitCode, State ) ->

	false = ?getAttr(shutdown_initiated),

	% We need here to perform a shutdown as clean as possible, while still
	% halting this user node as well (hence 'self() ! delete' would not be
	% sufficient by itself)
	%
	ShutdownState = shutdown_services( State ),

	% Actually returns (asynchronous stopping):
	init:stop( ExitCode ),

	setAttribute( ShutdownState, shutdown_initiated, true ).



% Returns a list of the names of all the selected computing nodes.
-spec getComputingNodes( wooper:state() ) ->
				const_request_return( [ atom_node_name() ] ).
getComputingNodes( State ) ->
	wooper:const_return_result( get_computing_nodes( State ) ).



% Activates the support of the Mnesia database on all known nodes.
%
% This is a request (not a oneway) to force synchronisation, not to collect a
% particular result.
%
-spec activateDatabase( wooper:state() ) ->
		request_return( 'database_already_running' | 'database_started' ).
activateDatabase( State ) ->

	%trace_utils:debug( "Deployment: requesting the activation of the "
	%                   "Mnesia database." ),

	case ?getAttr(database_running) of

		true ->
			%trace_utils:debug(
			%  "(Deployment: database already activated)" ),
			wooper:const_return_result( database_already_running );

		false ->

			%trace_utils:debug(
			%   "Deployment: activating the Mnesia database." ),

			MnesiaDir = filename:join( ?getAttr(deployment_base_dir),
									   "mnesia-storage" ),

			% Better than using the command-line -mnesia dir XXX:
			ok = application:set_env( mnesia, dir, MnesiaDir ),

			% With the data-logger, we are using disc_only_copies, thus
			% apparently these settings would not be useful (there is no memory
			% dump as such):

			% We want tables to be dumped less frequently from memory to disc,
			% in order to buffer writings (default value is 4):
			%ok = application:set_env( mnesia, dc_dump_limit, 1 ),

			% Increases a lot (default value is 100) the maximum number of
			% writes to the transaction log before a new dump is performed:
			%ok = application:set_env( mnesia, dump_log_write_threshold, 50000
			%   ),

			% Here, the deployment manager is already created, thus nodes are
			% already interconnected (thus fully meshed).

			% List of NodeName atoms:
			DatabaseNodes = get_computing_nodes( State ),

			% To be created before starting Mnesia; all nodes enabled:
			%
			% (apparently we must add this user node (the current node seems to
			% have to be included when creating a schema), otherwise we have:
			% {error,{"Cannot install fallback",
			%   {"No disc resident schema on local node",
			%      [MY_COMPUTING_NODE]}}}})
			%
			DatabaseEnabledNodes = [ net_utils:localnode() | DatabaseNodes ],


			% Here we remove any pre-existing database, thus any previous result
			% will be lost.

			% Ignore failure if no schema was already existing:
			case mnesia:delete_schema( DatabaseEnabledNodes ) of

				ok ->
					?info( "A previous database has been deleted." );

				{ error, { Reason, Arg } } ->
					?info_fmt( "No previous database deleted: ~s: ~w.",
							   [ Reason, lists:flatten( Arg ) ] )

			end,

			%trace_utils:debug_fmt( "(creating database schema on nodes ~p, "
			%		   "with Mnesia directory ~p)",
			%		   [ DatabaseEnabledNodes, MnesiaDir ] ),

			?info_fmt( "Creating database schema on nodes ~p.",
					   [ DatabaseEnabledNodes ] ),

			ok = mnesia:create_schema( DatabaseEnabledNodes ),

			DatabaseAgents = get_host_managers( State ),

			?info_fmt( "Starting database, notifying agents ~p.",
						[ DatabaseAgents ] ),

			% Oneway:
			[ HostManagerPid ! { startDatabase, self() }
			  || HostManagerPid <- DatabaseAgents ],

			% Allows to use the database directly from this user node as well
			% (ex: for a test case which would need a virtual probe)
			%
			ok = application:start( mnesia ),

			wait_for_database_event( DatabaseAgents, onDatabaseStarted ),

			%trace_utils:debug( "Deployment: Mnesia database activated." ),
			%mnesia:info(),

			wooper:return_state_result(
			   setAttribute( State, database_running, true ),
			   database_started )

	end.



% Deactivates the support of the Mnesia database on all known nodes.
%
% This is a request to force synchronisation, not to collect a particular
% result.
%
-spec deactivateDatabase( wooper:state() ) ->
			request_return( 'database_stopped' | 'database_was_not_running' ).
deactivateDatabase( State ) ->

	case ?getAttr(database_running) of

		false ->
			wooper:const_return_result( database_was_not_running );

		true ->

			% Host managers drive the database:
			DatabaseAgents = get_host_managers( State ),

			% Oneway:
			[ HostManagerPid ! { stopDatabase, self() } ||
				HostManagerPid <- DatabaseAgents ],

			wait_for_database_event( DatabaseAgents, onDatabaseStopped ),

			wooper:return_state_result(
					setAttribute( State, database_running, false ),
					database_stopped )

	end.



% Notification (expected to be sent by the resilience manager) of the PID of all
% resilience agents.
%
% Note: cannot be done the other way round (with getAllResilienceAgents/1), as
% it would create a deadlock.
%
-spec notifyResilienceAgents( wooper:state(), [ resilience_agent_pid() ] ) ->
									const_oneway_return().
notifyResilienceAgents( State, ResilienceAgentPidList ) ->

	% We do not like silent agent failures, but we do not want to be affected by
	% them and/or to affect them in return:
	%
	% (this manager may thus receive { 'DOWN', ... } messages)
	%
	[ erlang:monitor( process, AgentPid )
	  || AgentPid <- ResilienceAgentPidList ],

	wooper:const_return().



% Static methods section.



% Returns the atom corresponding to the name the load balancer should be
% registered as.
%
% Note: executed on the caller node.
%
-spec get_registration_name() ->
				 static_return( naming_utils:registration_name() ).
get_registration_name() ->
	% Ex: sim_diasca_deployment_manager
	wooper:return_static( ?deployment_manager_name ).



% Returns the string prefix to be used in order to name the Erlang nodes that
% correspond to the specified simulation name and SII, with the current user.
%
-spec get_node_name_prefix_from( simulation_name(), sim_diasca:sii() ) ->
							static_return( net_utils:string_node_name() ).
get_node_name_prefix_from( SimulationName, SII ) ->

	% Example: 'Sim-Diasca-Soda_Stochastic_Integration_Test-boudevil-55925800'.

	% We select the name of the user on the user node:
	Prefix = lists:flatten( "Sim-Diasca-"
				++ net_utils:generate_valid_node_name_from( SimulationName )
				++ "-" ++ system_utils:get_user_name() ++ "-" ++ SII ),

	wooper:return_static( Prefix ).




% Returns the name (as an atom) of the user node corresponding to the specified
% simulation name and SII, with the current user.
%
-spec get_user_node_name_from( simulation_name(), sim_diasca:sii() ) ->
								 static_return( atom_node_name() ).
get_user_node_name_from( SimulationName, SII ) ->
	NodePrefix = get_node_name_prefix_from( SimulationName, SII ),
	Name = text_utils:string_to_atom( NodePrefix ++ "-user-node" ),
	wooper:return_static( Name ).




% Returns the name (as a string) of any computing node corresponding to the
% specified simulation name and SII, with the current user.
%
-spec get_computing_node_prefix_from( simulation_name(), sim_diasca:sii() ) ->
								static_return( net_utils:string_node_name() ).
get_computing_node_prefix_from( SimulationName, SII ) ->
	Name = get_node_name_prefix_from( SimulationName, SII )
		++ "-computing-node",
	wooper:return_static( Name ).



% Returns the PID of the (unique) deployment manager.
-spec get_deployment_manager() -> static_return( manager_pid() ).
get_deployment_manager() ->
	Pid = naming_utils:wait_for_global_registration_of(
			get_registration_name() ),
	wooper:return_static( Pid ).




% Shutdowns a full deployment, hence the whole simulation, based on the
% specified PID of the deployment manager.
%
-spec shutdown( manager_pid() ) -> static_void_return().
shutdown( DeploymentManagerPid ) ->

	StaticEmitterCategorization = "Core.Deployment.class_DeploymentManager",

	?notify_info_cat( "Removing deployment manager, which will in turn "
		"take care of the deletion of all actors and agents "
		"on all nodes.", StaticEmitterCategorization ),

	DeploymentManagerPid ! { synchronous_delete, self() },
	receive

		{ deleted, DeploymentManagerPid } ->
			?notify_info_cat( "Overall shutdown succeeded.",
							  StaticEmitterCategorization )

	% We disabled the time-out for deployment shutdown (thus we finally kept it
	% as a synchronous operation), as otherwise not only some processings could
	% be interrupted (ex: trace aggregation), but also compute nodes would thus
	% linger (not stopping then, even if their work was over), blocking next
	% runs of the same case.

	% To let any lingering deletion be notified beforehand:
	%after ?synchronous_time_out + 1000 -> %+ 100000 ->
	%
	%		?notify_warning( "Time-out waiting for the deployment shutdown, "
	%						  "giving up." )

	end,

	wooper:return_static_void().




% To avoid 'The variable NonDir can never match since previous clauses
% completely covered the type ustring()':
%
-dialyzer( { no_match, determine_temporary_directory/1 } ).



% Determines and checks the temporary directory that should be used here.
-spec determine_temporary_directory( deployment_settings() ) ->
							static_return( directory_name() ).
determine_temporary_directory( DeploymentSettings ) ->

	case DeploymentSettings#deployment_settings.temporary_directory of

		Dir when is_list( Dir ) ->
			% Checked first on the user node:
			case file_utils:is_existing_directory( Dir ) of

				true ->
					wooper:return_static( Dir );

				false ->
					throw( { non_existing_temporary_directory_specified, Dir } )

			end;

		NonDir ->
			throw( { invalid_temporary_directory_specification, NonDir } )

	end.



% Returns the base deployment directory, corresponding to the specified
% simulation name.
%
-spec get_deployment_base_directory_for( simulation_name(),
		directory_name(), time_utils:timestamp(),
		sim_diasca:sii() ) -> static_return( file_utils:path() ).
get_deployment_base_directory_for( SimulationName, TmpDir, Timestamp, SII ) ->

	% We want to end up with a directory name (relatively to /tmp - or any
	% other user-specified base directory) like:
	% sim-diasca-My_Simulation_Case-boudevil-2012-12-7-at-13h-56m-03s-842497

	% The objective is to avoid having simulations step on each other:
	%
	SimDir = file_utils:convert_to_filename( "sim-diasca-" ++ SimulationName
			++ "-" ++ system_utils:get_user_name() ++ "-"
			++ time_utils:get_textual_timestamp_for_path( Timestamp )
			++ "-" ++ SII ),

	Path = filename:join( TmpDir, SimDir ),

	wooper:return_static( Path ).



% Returns a textual description of the specified deployment settings record.
-spec settings_to_string( #deployment_settings{} ) ->
								static_return( ustring() ).
settings_to_string( _DeploymentSettings=#deployment_settings{
						computing_hosts=Hosts,
						node_availability_tolerance=Tolerance,
						ping_available=PingAvailable,
						maximum_allowed_deployment_duration=MaxDepDuration,
						package_manager=PackageManager,
						rebuild_on_deployment_package_generation=Rebuild,
						additional_elements_to_deploy=AdditionalElements,
						plugin_directories=PluginDirectories,
						additional_beam_directories=AddBeamDirectories,
						perform_initial_node_cleanup=Cleanup,
						firewall_restrictions=FirewallRestrictions,
						temporary_directory=TmpDir,
						enable_data_logger=EnableDatalogger,
						enable_data_exchanger=EnableDataExchanger,
						enable_performance_tracker=EnablePerfTracker,
						enable_language_bindings=LanguageBindings,
						crash_resilience=ResilienceSettings,
						serialisation_period=SerialPeriod } ) ->

	IndentationLevel = 0,


	HostString = text_utils:format(
			"eligible computing hosts were described with ~p", [ Hosts ] ),


	ToleranceString = case Tolerance of

		allow_unavailable_nodes ->
			"unavailable nodes will be tolerated" ;

		fail_on_unavailable_node ->
			"no unavailable node will be tolerated"

	end,


	PingString = case PingAvailable of

		true ->
			"ping use enabled";

		false ->
			"ping use disabled"

	end,


	DepDurationString = case MaxDepDuration of

		undefined ->
			"using default deployment time-out";

		DepSec ->
			text_utils:format( "using a deployment time-out of ~B seconds",
							   [ DepSec ] )

	end,


	RebuildString = case Rebuild of

		true ->
			"after the simulation engine will have been rebuilt" ;

		false ->
			"with no prior rebuild of the simulation engine"

	end,


	PackageString = case PackageManager of

		generate_deployment_package ->
			"the simulation deployment package will be generated on-the-fly "
			"and saved in a deployment archive file, " ++ RebuildString;

		{ generate_and_save_deployment_package, TargetFilename } ->
			text_utils:format(
				"the simulation deployment package will be generated on-the-fly"
				" and saved in a deployment archive file named '~s', ",
				[ TargetFilename ] ) ++ RebuildString ;

		{ use_deployment_package, Filename } ->
			text_utils:format(
				"the simulation deployment package in file '~s' will be used.",
				[ Filename ] )

	end,


	AdditionalElementsString = case AdditionalElements of

		[] ->
			"no additional element will be deployed";

		Elements->
			text_utils:format(
				"following additional elements to deploy were specified:~n~p",
				[ Elements ] )

	end,


	PluginString = case PluginDirectories of

		[] ->
			"no plugin directory specified";

		_ ->
			text_utils:format( "~B plugin directories specified: ~s",
				[ length( PluginDirectories ),
				  text_utils:strings_to_string( PluginDirectories,
												IndentationLevel + 1 ) ] )

	end,


	AddBeamString = case AddBeamDirectories of

		[] ->
			"no additional BEAM directory specified";

		_ ->
			text_utils:format( "~B additional BEAM directories specified: ~s",
				[ length( AddBeamDirectories ),
				  text_utils:strings_to_string( AddBeamDirectories,
												IndentationLevel + 1 ) ] )

	end,


	CleanupString = case Cleanup of

		true ->
			"a clean-up of any Erlang node whose name matches the target one "
			"will be performed on all listed computing nodes";

		false ->
			"no specific initial clean-up of Erlang nodes will be performed"

	end,


	FirewallString = case FirewallRestrictions of

		none ->
			 "no firewall restriction specified";

		[] ->
			 "no firewall restriction specified";

		Options ->
			 text_utils:format( "firewall restrictions are: ~w", [ Options ] )

	end,


	TmpDirString = text_utils:format( "temporary directory used: '~s'",
									  [ TmpDir ] ),


	DataloggerString = case EnableDatalogger of

		true ->
			"datalogger service enabled";

		false ->
			"datalogger service disabled"

	end,


	DataExchangerString = case EnableDataExchanger of

		true ->
			"data-exchanger service enabled (with no specific "
			"configuration file)";

		{ true, ConfigurationFiles } ->
			text_utils:format( "data-exchanger service enabled, with ~B "
				"configuration files specified: ~s",
				[ length( ConfigurationFiles ),
				  text_utils:strings_to_string( ConfigurationFiles ) ] );

		false ->
			"datalogger service disabled"

	end,


	PerfTrackerString = case EnablePerfTracker of

		true ->
			"performance tracker service enabled";

		false ->
			"performance tracker service disabled"

	end,


	BindingString = case LanguageBindings of

		[] ->
			"no language binding support enabled";

		_NonEmptyList ->

			% Languages may include a code path:
			LangString = text_utils:strings_to_string(
			   [ language_utils:language_to_string( L,
								_IndentationLevel=1 )
				 || L <- LanguageBindings ] ),

			text_utils:format( "binding support enabled for following "
							   "languages: ~s", [ LangString ] )


	end,


	ResilienceString = case ResilienceSettings of

		none ->
			"no crash resilience required";

		0 ->
			"no crash resilience required";

		K when is_integer( K ) andalso K > 0 ->
			text_utils:format( "~B-crash resilience required, with a "
				"serialisation period set to ~p", [ K, SerialPeriod ] );

		Other ->
			text_utils:format( "invalid resilience specification (~p)",
							   [ Other ] )

	end,

	wooper:return_static( text_utils:strings_to_string( [
		HostString, ToleranceString, PingString, DepDurationString,
		RebuildString, PackageString, AdditionalElementsString, PluginString,
		AddBeamString, CleanupString, FirewallString, TmpDirString,
		DataloggerString, DataExchangerString, PerfTrackerString, BindingString,
		ResilienceString ] ) ).



% Determines from the command-line parameters where is the root directory of the
% sources of the simulation engine (useful to locate specific files).
%
-spec determine_root_directory() -> static_return( file_utils:path() ).
determine_root_directory() ->

	CurrentDir = file_utils:get_current_directory(),

	CmdLineRootOpt = ?engine_arg_root_key,

	RelativeRootDir = case
			shell_utils:get_command_arguments_for_option( CmdLineRootOpt ) of

		[ [ D ] ] ->
			D;

		Other ->
			trace_utils:error_fmt( "Unable to retrieve engine root directory "
				"from the command line: the '~s' option is associated to ~p; "
				"had ~s", [ CmdLineRootOpt, Other,
							shell_utils:argument_table_to_string(
								shell_utils:get_argument_table() ) ] ),

			throw( { lacking_command_line_option, CmdLineRootOpt, Other } )

	end,

	% RelativeRootDir corresponds to the SIM_DIASCA_TOP makefile variable, which
	% leads to the 'sim-diasca' directory, whereas we want its *parent*
	% directory:
	%
	EngineRootDir = file_utils:join( [ CurrentDir, RelativeRootDir, ".." ] ),

	wooper:return_static( file_utils:normalise_path( EngineRootDir ) ).



% Returns the most usual file suffixes that are generally to exclude when
% creating a deployment archive.
%
-spec get_basic_blacklisted_suffixes() -> static_return( [ ustring() ] ).
get_basic_blacklisted_suffixes() ->

	% Thus now accepted: ".py", ".class", since:
	Suffixes = [ ".erl", ".hrl", ".sdar", "_test.beam", ".java", ".sh",
		".escript", "GNUmakefile", "top-GNUmakefile-for-releases", ".inc",
		".properties", ".png", ".rst", ".odg", ".dia", ".graph", ".txt",
		".html", ".css", ".traces", ".sample", ".rc", ".pyc" ],

	wooper:return_static( Suffixes ).




% Helper functions section.


% Returns the version string for the layers used here.
%
% (helper)
%
-spec get_version_string() -> { ustring(), ustring(), ustring(), ustring() }.
get_version_string() ->

	MyriadVersionString = ?myriad_version,
	WOOPERVersionString = ?wooper_version,
	TracesVersionString = ?traces_version,
	SimDiascaVersionString = ?sim_diasca_version,

	{ MyriadVersionString, WOOPERVersionString, TracesVersionString,
	  SimDiascaVersionString }.



% Returns adequate meta-data for result producers.
%
% (helper)
%
get_result_metadata( _VersionStrings={ MyriadVersionString, WOOPERVersionString,
							TracesVersionString, SimDiascaVersionString },
					 SimulationName, TickDuration ) ->

	Duration = time_utils:duration_to_string( _Milliseconds=1000*TickDuration ),

	option_list:new( [

		% No need to interpret version strings with basic_utils:parse_version/1,
		% text is fine here as well:

		{ myriad_version, text_utils:string_to_binary(
			"version of the Myriad layer: " ++ MyriadVersionString ) },

		{ wooper_version, text_utils:string_to_binary(
			"version of the WOOPER layer: " ++ WOOPERVersionString ) },

		{ traces_version, text_utils:string_to_binary(
			"version of the Traces layer: " ++ TracesVersionString ) },

		{ sim_diasca_version, text_utils:string_to_binary(
			 "version of the Sim-Diasca engine: " ++ SimDiascaVersionString ) },

		{ simulation_name, text_utils:string_to_binary(
			 "name of the simulation case: " ++ SimulationName ) },

		{ tick_duration, text_utils:string_to_binary( text_utils:format(
			 "tick duration (in virtual seconds): ~f (corresponding to ~s)",
			 [ TickDuration, Duration ] ) ) } ] ).



% Creates the root data exchanger, returns its PID, and creates and links as
% well all the local exchangers.
%
% (helper)
%
-spec create_data_exchangers( atom_node_name(), [ file_utils:path() ],
		  time_manager_pid(), [ atom_node_name() ],
		  net_utils:string_node_name() ) -> data_exchanger_pid().
create_data_exchangers( RootDataExchangerNode, ConfigurationFileList,
		RootTimeManagerPid, LocalDataExchangerNodes, BaseNodeName ) ->

	% Actual paths will be checked later, in class_DataExchanger constructor:
	check_configuration_file_list( ConfigurationFileList ),

	%trace_utils:debug_fmt( "Creating the root data exchanger on ~p, "
	% "local ones on ~p.", [ RootDataExchangerNode, LocalDataExchangerNodes ] ),

	RootExchangerPid = class_DataExchanger:remote_synchronous_timed_new_link(
		_Node=RootDataExchangerNode, _Name="Root Data Exchanger",
		{ ConfigurationFileList, RootTimeManagerPid } ),


	% Currently we create only a simplistic exchanger tree: all non-root
	% exchangers are direct children of the root, they are thus leaves of a tree
	% whose height is 1. We could imagine deeper trees.

	% However we need a data-exchanger to be available on the user host (not
	% necessarily node) for the simulation case to be able to exchange data as
	% well.

	% If there is a computing node on the user host, the simulation case is to
	% use the data-exchanger of that node, thanks to an appropriate
	% registering. Otherwise a dedicated local data-exchanger will be created
	% directly on that user node.


	% We do not keep the PID of the local exchangers as we will manage them
	% exclusively from the root.
	%
	% Note that we do not specify directly the configuration files to the child
	% exchangers, as this is the role of the root exchanger to do so, and it may
	% for example check their existence and content beforehand; moreover this
	% results in their creation being quick, no need to create them
	% asynchronously in a row and then wait for their parallel setting-up:
	%
	_LocalExchangers = [ class_DataExchanger:remote_synchronous_timed_new_link(
		  Node,
		  _LocalName=text_utils:format( "Local exchanger for ~s", [ Node ] ),
		  { _ParentExchangerPid=RootExchangerPid, _NodeType=computing_node } )
						 || Node <- LocalDataExchangerNodes ],

	% Determining now if there is a suitable computing node on the user host:

	% Here we determine the name of the *computing node* that *may* exist on the
	% user host, not the name of the user node itself:
	%
	UserComputingNodeName = net_utils:get_fully_qualified_node_name(
		BaseNodeName, net_utils:localhost(), net_utils:get_node_naming_mode() ),

	%trace_utils:debug_fmt( "Local exchanger nodes = ~p, "
	%  "user computing node: ~p",
	%  [ LocalDataExchangerNodes, UserComputingNodeName ] ),

	% The root of the problem is that when no PID of a local exchanger was
	% specified to data-exchange calls within the simulation case, the PID had
	% to be retrieved from the local registering service. In the case of the
	% user node, we wanted to use instead the PID of the host-local
	% data-exchanger instantiated on the computing node. However, from the user
	% node it is a remote PID and cannot be registered locally.

	% We thus used to rely on a WOOPER instance proxy beforehand:

	%trace_utils:debug_fmt( "The user node will rely on the data exchanger "
	%			"~w, on host-local node ~p, through a proxy.",
	%			[ Pid, TargetNodeName ] ),

	% We cannot register this exchanger PID locally on the user
	% node, as it is a non-local PID, we thus used a proxy for
	% that:
	%ProxyPid = wooper_instance_proxy:start_link( Pid ),

	%naming_utils:register_as( ProxyPid, LocalExchangerName, local_only ),

	% Now, instead of a proxy, we register (globally) the data-exchanger local
	% to the computing node on the user host.

	CaseExchangerName =
		class_DataExchanger:get_global_name_of_exchanger_for_case(),

	AllExchangerNodes = [ RootDataExchangerNode | LocalDataExchangerNodes ],

	RootDataExchangerHost = node_to_host( RootDataExchangerNode ),

	UserNode = node(),
	UserHost = node_to_host( UserNode ),

	case lists:member( UserComputingNodeName, AllExchangerNodes ) of

		true ->

			% Here, the user host was included, thus the current node (i.e. the
			% user one) is to use the data-exchanger of that computing node on
			% the same host (that is already available, thanks to synchronous
			% operations).

			% In the general case this would involve registering globally that
			% data-exchanger under the name under which it will be looked-up
			% from a test case (i.e. get_global_name_of_exchanger_for_case/0).

			% However this may be not directly possible on all configurations,
			% as this computing node might have been chosen to host the *root*
			% data exchanger: in that case, it will have already registered
			% itself globally (as the root one), therefore preventing to be
			% registered globally again, with the aforementioned name for case.

			% So: if the root exchanger is local to the user host (case #1), we
			% do nothing (as the "name for case" look-up will fail and then the
			% case will fall back to the look-up of the root exchanger).
			% Otherwise (case #2), we simply register globally the
			% data-exchanger on the computing node local to the user host under
			% the name for case.

			case UserHost of


				RootDataExchangerHost ->

					% This is case #1, nothing to do.
					%trace_utils:debug_fmt( "The root data-exchanger was "
					%  "created on the user host (~s), nothing to do.",
					%			[ UserHost ] ),
					ok;


				_ ->

					% This is case #2. First, look-up the target exchanger:

					%trace_utils:debug_fmt( "Registering pre-existing "
					%  "data-exchanger located on the user host (node ~s).",
					%  [ UserComputingNodeName ] ),

					TargetPid = case rpc:call( UserComputingNodeName,
						_Mod=erlang,
						_Fun=whereis,
						_Args=[ class_DataExchanger:get_local_exchanger_name() ]
											  ) of

						{ badrpc, Reason } ->
							throw( { remote_data_exchanger_not_found, Reason,
									 UserComputingNodeName } );

						Pid ->
							Pid

					end,

					% Here we retrieved the PID of the local exchanger already
					% available on the user host. We register it globally now:
					%trace_utils:debug_fmt( "Registering globally ~w (on ~s) "
					%  "as ~s.", [ TargetPid, UserComputingNodeName,
					% CaseExchangerName ] ),

					naming_utils:register_as( TargetPid, CaseExchangerName,
											  global_only )

			end;


		false ->

			% Here, the user host was not included in the simulation, so we have
			% to create a new local data-exchanger specific to the user node:
			%trace_utils:debug_fmt( "A data-exchanger dedicated to the user "
			%   "node (~s) "is created now, on ~s.", [ UserNode, UserHost ] ),

			% Note however that this data-exchanger will be the only one not to
			% be created on a computing host, thus may behave differently (ex:
			% the local path of configuration files is specifically managed):
			Pid = class_DataExchanger:synchronous_timed_new_link(
				_LocalName="Local exchanger dedicated to user node",
				{ _ParentExchangerPid=RootExchangerPid, _NodeType=user_node } ),

			% We will register this user-specific exchanger globally as well, so
			% that a global look-up can be performed regardless of the choice in
			% the user-related exchangers (node-local or only host-local).
			naming_utils:register_as( Pid, CaseExchangerName, global_only )

	end,

	RootExchangerPid.



% To avoid 'The pattern _FileList = [Elem | _T] can never match since previous
% clauses completely covered the type [string()]':
%
-dialyzer( { no_match, check_configuration_file_list/1 } ).


% Ensures that all configuration files are specified as strings.
%
% (helper)
%
check_configuration_file_list( _FileList=[] ) ->
	ok;

check_configuration_file_list( _FileList=[ Path | T ] ) when is_list( Path ) ->

	case text_utils:is_string( Path ) of

		true ->
			check_configuration_file_list( T );

		false ->
			throw( { invalid_configuration_file, Path } )

	end;

check_configuration_file_list( _FileList=[ Elem | _T ] ) ->
		throw( { invalid_configuration_filename, Elem } ).



% Returns the hostname (as a plain string) which corresponds to the specified
% node (as an atom).
%
% (helper)
%
-spec node_to_host( atom_node_name() ) ->
						net_utils:string_host_name().
node_to_host( NodeName ) ->

	% Ex: returns "Data_Exchange_test-john@foobar":
	StringNodeName = text_utils:atom_to_string( NodeName ),

	% Returns "foobar":
	string:sub_word( StringNodeName, _WordIndex=2, _SplittingChar=$@ ).




% Returns all the information recorded for specified computing host manager.
%
% Returns { Hostname, Username, Nodename, }, i.e. all fields of the
% computing_host record except the first), on success.
%
% (helper)
%
-spec get_host_info( host_manager_pid(), wooper:state() ) ->
						   computing_host_info().
get_host_info( ComputingHostManagerPid, State ) ->

	HostInfos = ?getAttr(host_infos),

	% Relies on the record-to-tuple syntax, knowing the first tuple element is
	% the record tag (i.e. 'computing_host_info'):
	%
	case lists:keyfind( ComputingHostManagerPid, _Index=2, HostInfos ) of

		false ->
			throw( { host_manager_not_found, ComputingHostManagerPid } );

		HostInfo ->
			HostInfo

	end.



% Returns the host name, as a binary, which corresponds to specified computing
% host manager.
%
% (helper)
%
-spec get_hostname_for( host_manager_pid(), wooper:state() ) ->
							  text_utils:bin_string().
get_hostname_for( ComputingHostManagerPid, State ) ->

	HostInfo = get_host_info( ComputingHostManagerPid, State ),

	HostInfo#computing_host_info.host_name.



% Returns the user name, as a binary, which corresponds to specified computing
% host manager.
%
% (helper)
%
-spec get_username_for( host_manager_pid(), wooper:state() ) ->
							  text_utils:bin_string().
get_username_for( ComputingHostManagerPid, State ) ->

	HostInfo = get_host_info( ComputingHostManagerPid, State ),

	HostInfo#computing_host_info.user_name.



% Returns a list of all the (fully qualified) node names (as atoms)
% corresponding to the known computing nodes.
%
% (helper)
%
get_computing_nodes( State ) ->
	[ H#computing_host_info.node_name || H <- ?getAttr(host_infos) ].



% Returns a {HostCoreList, UserNodeInfo} pair, where:
%
% - HostCoreList is a list of { HostName, NodeName, CoreCount } where CoreCount
% is the number of cores of specified host HostName, and NodeName is the name of
% the computing node on that host (both are atoms); we ensure that even if the
% host on which the user node runs is not running a computing node, it is still
% listed (once), so that a result queue can be created as well for it
%
% - UserNodeInfo={UserNodeName, UserHostName} , i.e. a pair made of the names of
% the user node and host (both as atoms)
%
% (helper)
%
get_host_information( HostInfos ) ->

	% Unable to make it compile:
	%[ { H#computing_host_info.node_name,
	%	( H#computing_host_info.host_infos )#host_static_info.core_count }
	%		|| H <- HostInfos ].

	Hostnames = [ H#computing_host_info.host_name || H <- HostInfos ],

	% This deployment manager runs on the user node:
	UserHostnameString = net_utils:localhost(),

	UserHostnameBin = text_utils:string_to_binary( UserHostnameString ),

	UserHostnameAtom = text_utils:string_to_atom( UserHostnameString ),

	UserNodeAtom = node(),

	HostCoreList = case lists:member( UserHostnameBin, Hostnames ) of

		true ->
			% User host already taken into account:
			generate_host_core_list( HostInfos );

		false ->
			% No computing node must be running on the user host, adding it:
			UserCores = system_utils:get_core_count(),

			UserHostEntry = { UserHostnameAtom, UserNodeAtom, UserCores },
				[ UserHostEntry | generate_host_core_list( HostInfos ) ]

	end,

	UserNodeInfo = { UserNodeAtom, UserHostnameAtom },

	{ HostCoreList, UserNodeInfo }.



% (helper)
generate_host_core_list( HostInfos ) ->
	generate_host_core_list( HostInfos, _Acc=[] ).


generate_host_core_list( _HostInfos=[], Acc ) ->
	Acc;

generate_host_core_list( _HostInfos=[ H | T ], Acc ) ->

	HostNameAtom = text_utils:binary_to_atom( H#computing_host_info.host_name ),

	NodeNameAtom = H#computing_host_info.node_name,

	HostInfo = H#computing_host_info.host_infos,
	CoreCount = HostInfo#host_static_info.core_count,

	generate_host_core_list( T,
		[ { HostNameAtom, NodeNameAtom, CoreCount } | Acc ] ).



% Returns a list of the PID of all known host managers.
%
% (helper)
%
-spec get_host_managers( wooper:state() ) -> [ host_manager_pid() ].
get_host_managers( State ) ->
	[ H#computing_host_info.host_manager_pid || H <- ?getAttr(host_infos) ].




% Returns a list of node names (as atoms) corresponding to specified computing
% host manager entries.
%
% (helper)
%
-spec get_node_names( [ computing_host_info() ] ) ->
							[ atom_node_name() ].
get_node_names( HostManagerEntryList ) ->
	[ H#computing_host_info.node_name || H <- HostManagerEntryList ].



% Returns whether an initial clean-up of any previously existing node with that
% name is wanted: it is either false, or the full path of the clean-up script to
% be used, as a binary.
%
% (helper)
%
get_clean_up_settings( DeploySettings, RootDir, State ) ->

	case DeploySettings#deployment_settings.perform_initial_node_cleanup of

		true ->

			% First, check that the script is available:
			CleanScriptName = "node-cleaner.sh",

			CleanScriptFullPath = filename:join( [ RootDir, "sim-diasca", "src",
					  "core", "services", "deployment", CleanScriptName ] ),

			case file_utils:is_existing_file( CleanScriptFullPath ) of

				true ->
					text_utils:string_to_binary( CleanScriptFullPath );

				false ->
					?emergency_fmt( "Script '~s', used to remove any pending "
						"Erlang node, could not be found, aborting.",
						[ CleanScriptFullPath ] ),
					throw( { clean_up_script_not_found, CleanScriptFullPath } )

			end;

		false ->
			false

	end.


% To avoid 'The variable FaultyDuration can never match since previous clauses
% completely covered the type 'undefined' | integer()':
%
-dialyzer( { no_match, get_deploy_time_out/2 } ).


% Returns the number of milliseconds that should be used as deployment time-out.
%
% (helper)
%
get_deploy_time_out( DeploymentSettings, State ) ->

	% In milliseconds:
	DeployTimeOut = case
	 DeploymentSettings#deployment_settings.maximum_allowed_deployment_duration
						of

		undefined ->
		 class_ComputingHostManager:get_host_deployment_duration_upper_bound();

		S when is_integer( S ) ->
		 % Was specified in seconds:
		 1000 * S;

		FaultyDuration ->
		 throw( { invalid_maximum_allowed_deployment_duration,
				  FaultyDuration } )

	end,

	DMin = 3000,

	% Prefer higher values if you happen to be in a context where SSH
	% connections are unusually long to establish:
	%
	% (note: in the general case, three SSH connections per node will be
	% attempted; scp and ssh of the node cleaner script, and ssh of the actual
	% remote Erlang VM)
	%
	%DMin = 30000,

	case DeployTimeOut of

		U when U < DMin ->
			?warning_fmt( "The determined deployment duration (~B ms) "
						  "was too short, it has been set back to a "
						  "more conservative value, ~B ms.", [ U, DMin ] ),
			DMin;

		U ->
			U

	end.



% Applies the specified configuration changes.
%
% (helper)
%
apply_configuration_changes( _ConfChanges=#configuration_changes{
				compute_scheduler_count=ComputeSchedulerCount }, State ) ->

	case ComputeSchedulerCount of

		undefined ->
			State;

		C when is_integer( C ) andalso C =< 1024 ->
			?info_fmt( "Scheduler count of computing nodes set to ~B.",
					   [ C ] ),
			setAttribute( State, compute_scheduler_count, C );

		Other ->
			throw( { invalid_compute_scheduler_count, Other } )

	end.



% Interprets the outcome of the set-up phase, as returned by the host managers,
% and on success returns a list of selected node names (as atoms).
%
% (helper)
%
interpret_setup_outcome( _Available=[], _Failed=[], _NodeAvailabilityTolerance,
						 State ) ->

	Message = "Error, no computing host specified, hence no node selected, "
			  "and no simulation can be run.",

	?emergency( Message ),

	throw( no_computing_node );


interpret_setup_outcome( _Available=[], Failed, _NodeAvailabilityTolerance,
						 State ) ->

	% None available here.


	Message = case Failed of

		[ { UniqueFailed, Reason } ] ->

			UniqueFailed ! delete,

			UniqueFailedName = get_hostname_for( UniqueFailed, State ),

			text_utils:format( "Error, the deployment on the single computing "
				"host specified, ~s, failed (reason: ~s), "
				"no simulation can be run.~n~s",
				[ UniqueFailedName, interpret_host_failure( Reason ),
				  get_context_information( ?getAttr(epmd_port) ) ] );

		_ ->
			text_utils:format( "Error, no available computing node found "
				"on following ~B host candidates: ~s~n~s",
				[ length( Failed ), interpret_failed_hosts( Failed, State ),
				  get_context_information( ?getAttr(epmd_port) ) ] )

	end,

	?emergency( Message ),

	throw( no_available_computing_node );


interpret_setup_outcome( Available, _Failed=[], _NodeAvailabilityTolerance,
						 State ) ->

	AtomNodeNames = get_node_names( Available ),

	Message = case AtomNodeNames of

		[ UniqueAvailable ] ->
			text_utils:format( "The single specified computing host is "
				"available, using corresponding node: '~s'.~n",
				[ UniqueAvailable ] );

		_ ->
			text_utils:format( "On all specified computing hosts, "
				"a corresponding node is available.~n~n"
				"The ~B validated computing nodes are: ~s",
				[ length( AtomNodeNames ),
				  text_utils:atoms_to_string( AtomNodeNames ) ] )

	end,

	basic_utils:display( "~n~s", [ Message ] ),
	?notice( Message ),

	AtomNodeNames;


interpret_setup_outcome( Available, Failed, NodeAvailabilityTolerance,
						 State ) ->

	% At least one node must be lacking, yet the simulation can be run
	% (the Available and Failed lists contain at least one PID each):
	%
	FailMessage = notify_failed_node( Failed, State ),

	case NodeAvailabilityTolerance of

		fail_on_unavailable_node ->

			Message = FailMessage ++ "As the selected policy is to fail should "
				"a computing host be unavailable, no simulation will be run.",

			?emergency( Message ),

			[ ComputingHostPid ! delete
			  || { ComputingHostPid, _Reason } <- Failed ],

			% Not wanting a stacktrace here:
			%throw( { unavailable_nodes, Failed } );
			halt_on_error( "there is at least one unavailable node" ,
						   _ErrorCode=70 );

		allow_unavailable_nodes ->

			AtomNodeNames = get_node_names( Available ),

			ContinueMessage = case AtomNodeNames of

				  [ UniqueAvailable ] ->
					  text_utils:format( "Continuing on a single node: ~s.",
										 [ UniqueAvailable ] );

				  _ ->
					  text_utils:format(
						"Continuing with following ~B nodes: ~s",
						[ length( AtomNodeNames ),
						  text_utils:atoms_to_string( AtomNodeNames ) ] )

			end,

			% Useless, as shown by shown on the console by next warning:
			%trace_utils:debug_fmt( "~n~s~n~s",
			%   [ FailMessage, ContinueMessage ] ),

			?warning_fmt( "~s~n~s", [ FailMessage, ContinueMessage ] ),

			[ ComputingHostPid ! delete
			 || { ComputingHostPid, _Reason } <- Failed ],

			AtomNodeNames

	end.



% Returns runtime, contextual information that may help any troubleshooting.
-spec get_context_information( net_utils:tcp_port() ) -> ustring().
get_context_information( EpmdPort ) ->

	DefaultEpmdPort = 4369,

	EpmdStrings = case EpmdPort of

		DefaultEpmdPort ->
			% Thus a single diagnosis will suffice:
			[ get_epmd_diagnosis( EpmdPort ) ];

		_ ->
			[ get_epmd_diagnosis( EpmdPort ),
			  % For an increased safety, we also collect any names that would be
			  % known of a default, potentially unexpected EPMD:
			  %
			  get_epmd_diagnosis( DefaultEpmdPort ) ]

	end,

	% For the order:
	DiagStrings = EpmdStrings
		++ [ get_epmd_local_diagnosis(), get_beam_local_diagnosis() ],

	text_utils:format( "Additional context follows: ~s",
					   [ text_utils:strings_to_string( DiagStrings ) ] ).



% Returns any diagnosis obtained thanks to EPMD.
-spec get_epmd_diagnosis( net_utils:tcp_port() ) -> ustring().
get_epmd_diagnosis( EpmdPort ) ->

	EpmdExecName = "epmd",

	case executable_utils:lookup_executable( EpmdExecName ) of

		false ->
			text_utils:format( "(no EPMD diagnosis available, "
				"as no '~s' executable found)", [ EpmdExecName ] );

		EpmdPath ->

			Cmd = text_utils:format( "~s -names", [ EpmdPath ] ),

			Env = [ { "ERL_EPMD_PORT",
					  text_utils:integer_to_string( EpmdPort ) } ],

			{ ReturnCode, CmdOutput } = system_utils:run_command( Cmd, Env ),

			text_utils:format( "epmd, for port ~B, returned (with exit code ~B)"
							   ":~n~s", [ EpmdPort, ReturnCode, CmdOutput ] )

	end.



% Returns any local diagnosis obtained thanks to the look-up of EPMD processes.
-spec get_epmd_local_diagnosis() -> ustring().
get_epmd_local_diagnosis() ->

	Expr = "ps -eF | grep epmd | grep -v grep | grep -v launch-erl.sh"
		   "| grep -v beam.smp",

	ShellOutput = system_utils:evaluate_shell_expression( Expr ),

	text_utils:format( "local EPMD daemons found:~n~s", [ ShellOutput ] ).



% Returns any local diagnosis obtained thanks to the look-up of BEAM-based
% processes.
%
-spec get_beam_local_diagnosis() -> ustring().
get_beam_local_diagnosis() ->

	ShellOutput = system_utils:evaluate_shell_expression(
					"ps -eF | grep beam.smp | grep -v grep" ),

	text_utils:format( "local BEAM processes found:~n~s", [ ShellOutput ] ).



% Returns a notification message (plain string) corresponding to the specified
% failed nodes.
%
% (helper)
%
notify_failed_node( [ { UniqueFailed, Reason } ], State ) ->

	UniqueFailedName = get_hostname_for( UniqueFailed, State ),

	text_utils:format( "No computing node could be created "
		"on host candidate ~s: ~s.~n~s",
		[ UniqueFailedName, interpret_host_failure( Reason ),
		  get_context_information( ?getAttr(epmd_port) ) ] );


notify_failed_node( FailedList, State ) when length( FailedList ) > 1 ->

	text_utils:format( "Following ~B host candidates were specified "
		"but no computing node could be created on them: ~s~n~s",
		[ length( FailedList ),
		  interpret_failed_hosts( FailedList, State ),
		  get_context_information( ?getAttr(epmd_port) ) ] ).



% Returns a list of { Hostname, Username } pairs (a list of pair of plain
% strings) corresponding to the deployment information entered in the
% computing_hosts field.
%
% UserName corresponds to the user login name on the user host, specified as a
% a plain string.
%
% Note that if the current host is to be included automatically, it will be in
% the first position.
%
% (helper)
%
determine_host_list_from( { use_host_file, HostFile }, UserName, State ) ->
	determine_host_list_from( { use_host_file, HostFile, include_localhost },
							  UserName, State );

determine_host_list_from( { use_host_file, HostFile, include_localhost },
						  UserName, State ) ->
	ensure_localhost_included( determine_host_list_from(
		{ use_host_file, HostFile, exclude_localhost }, UserName, State ),
							   UserName  );

determine_host_list_from( { use_host_file, HostFile, exclude_localhost },
						  UserName, _State ) ->

	case file_utils:is_existing_file( HostFile ) of

		true ->
			get_hosts_from_file( HostFile, UserName );

		false ->
			throw( { host_file_not_found, HostFile } )

	end;

determine_host_list_from( { use_host_file_otherwise_local, HostFile },
						  UserName, State ) ->
	determine_host_list_from( { use_host_file_otherwise_local, HostFile,
								include_localhost }, UserName, State );

determine_host_list_from( { use_host_file_otherwise_local, HostFile,
							include_localhost }, UserName, State ) ->

	ensure_localhost_included( determine_host_list_from(
		  { use_host_file_otherwise_local, HostFile, exclude_localhost },
								 UserName, State ), UserName );

determine_host_list_from( { use_host_file_otherwise_local, HostFile,
							exclude_localhost }, UserName, State ) ->

	case file_utils:is_existing_file_or_link( HostFile ) of

		true ->
			?notice_fmt( "Host file '~s' found, using it.", [ HostFile ] ),
			get_hosts_from_file( HostFile, UserName );

		false ->
			?notice_fmt( "No host file '~s' found, defaulting to "
				"a simulation running on local host only.", [ HostFile ] ),
			determine_host_list_from( localhost_only, UserName, State )

	end;

determine_host_list_from( localhost_only, UserName, State ) ->
	determine_host_list_from( { [], include_localhost }, UserName, State );

determine_host_list_from( HostList, UserName, State ) when is_list(HostList) ->
	determine_host_list_from( { HostList, include_localhost }, UserName,
							  State );

determine_host_list_from( { HostList, include_localhost }, UserName, State ) ->
	ensure_localhost_included( determine_host_list_from(
		{ HostList, exclude_localhost }, UserName, State ), UserName );

determine_host_list_from( { HostList, exclude_localhost }, UserName, _State ) ->
	ensure_username_specified( HostList, UserName, _Acc=[] );

determine_host_list_from( UnexpectedHostInfo, _UserName, _State ) ->
	throw( { invalid_computing_host_specification, UnexpectedHostInfo } ).



% Ensures that the local host is listed once, and only once.
% Hence it will be added (in first position) iff was lacking.
%
% (helper)
%
ensure_localhost_included( HostList, UserName ) ->

	% Username must match too:
	HostEntry = { net_utils:localhost(), UserName },

	%trace_utils:debug_fmt( "HostEntry = ~p, HostList = ~p.",
	%                       [ HostEntry, HostList ] ),

	case lists:member( HostEntry, HostList ) of

		true ->
			HostList;

		false ->
			[ HostEntry | HostList ]

	end.


% Performs two actions: validates and converts entries, and adds default
% username if none was specified.
%
% (helper)
%
ensure_username_specified( _HostList=[], _DefaultUserName, Acc ) ->
	Acc;

ensure_username_specified( _HostList=[ { Host, UserName } | T ],
						   DefaultUserName, Acc )
  when is_atom( Host ) andalso is_atom( UserName ) ->

	Entry = { atom_to_list( Host ), atom_to_list( UserName ) },
	ensure_username_specified( T, DefaultUserName, [ Entry | Acc ] );

ensure_username_specified( [ H | _T ], _DefaultUserName, _Acc ) ->
	throw( { invalid_host_list_entry, H } ).



% Returns the list of {Hostnames,Username} pairs, as specified in the host file.
%
% (helper)
%
get_hosts_from_file( HostFile, DefaultUsername ) ->

	case file:consult( HostFile ) of

		{ ok, LineElements } ->
			filter_line_elements( LineElements, DefaultUsername, _Acc=[] );

		{ error, Reason } ->
			throw( { invalid_host_file_content, HostFile, Reason } )

	end.



% Filters the content of a host candidate file.
%
% (helper)
%
filter_line_elements( [], _DefaultUsername, Acc ) ->
	Acc;

filter_line_elements( [ { Hostname, Login, Comment } | T ], DefaultUsername,
		 Acc ) when is_atom( Hostname ) andalso is_atom( Login )
					andalso is_list( Comment ) ->

	% Comments are just dropped:
	HostPair = { atom_to_list( Hostname ), atom_to_list( Login ) },
	filter_line_elements( T, DefaultUsername, [ HostPair | Acc ] );

filter_line_elements( [ { Hostname, Comment } | T ], DefaultUsername, Acc )
  when is_atom( Hostname ) andalso is_list( Comment ) ->

	% Comments are just dropped; using default username:
	HostPair = { atom_to_list( Hostname ), DefaultUsername },
	filter_line_elements( T, DefaultUsername, [ HostPair | Acc ] );

filter_line_elements( [ Hostname | T ], DefaultUsername, Acc )
  when is_atom( Hostname ) ->

	HostPair = { atom_to_list( Hostname ), DefaultUsername },
	filter_line_elements( T, DefaultUsername, [ HostPair | Acc ] );

filter_line_elements( [ H |_T ], _DefaultUsername, _Acc ) ->
	throw( { unexpected_host_entry, H } ).



% Triggers the setting-up of each computing host, so that we end up with an
% appropriate Erlang node on each of the valid hosts.
%
% - BaseNodeName is a plain string describing the prefix common to all names of
% computing nodes
%
% - HostUserList is a list of target {Hostname, Username}, as plain strings
%
% This operation is now performed in parallel across all registered computing
% nodes, as, if having hundreds nodes, taking care of one after the other would
% be uselessly long.
%
% Returns an updated state.
%
% (helper)
%
-spec set_up_computing_nodes( net_utils:string_node_name(), host_user_list(),
					unit_utils:seconds(), [ bin_directory_name() ],
					wooper:state() ) -> wooper:state().
set_up_computing_nodes( BaseNodeName, HostUserList, InterNodeSeconds,
						AdditionalBEAMBinDirs, State ) ->

	% Creates here one manager per computing host. They will automatically start
	% their setup work as soon as created:
	%
	NodeOpts = { BaseNodeName, NamingMode=?getAttr(node_naming_mode),
				 ?getAttr(node_cleanup), erlang:get_cookie(),
				 ?getAttr(compute_scheduler_count) },

	NetworkOpts = { ?getAttr(epmd_port), ?getAttr(tcp_port_range),
					?getAttr(ping_allowed) },

	DeployBaseDir = text_utils:string_to_binary(
					  ?getAttr(deployment_base_dir) ),

	DeployOptions = { self(), ?getAttr(deploy_time_out), InterNodeSeconds,
					  DeployBaseDir, AdditionalBEAMBinDirs,
					  ?getAttr(simulation_instance_id) },

	LocalHost = net_utils:localhost(),

	% The computing host for the local host should know it is local, otherwise
	% it would attempt local-to-local SSH connections, which may fail depending
	% on the SSH settings (typically, by default, a password will be
	% interactively requested), whereas a simulation should be able to run
	% purely locally:
	%
	HostFun = fun( Hostname ) ->

		case Hostname of

			LocalHost ->
				%trace_utils:debug_fmt( "Localhost detected ('~s').",
				%					   [ LocalHost ] ),
				localhost;

			H ->
				%trace_utils:debug_fmt( "Remote host detected ('~s').",
				%					   [ H ] ),
				H

		end

	end,

	HostInfos = [ #computing_host_info{

		host_manager_pid=class_ComputingHostManager:new_link(
			{ HostFun( Host ), User }, NodeOpts, NetworkOpts, DeployOptions ),

		host_name=text_utils:string_to_binary( Host ),

		user_name=text_utils:string_to_binary( User ),

		node_name=net_utils:get_fully_qualified_node_name( BaseNodeName, Host,
														   NamingMode ),

		host_infos=undefined } || { Host, User } <- HostUserList ],


	% Host managers are now being launched asynchronously and be working in the
	% background, we can in the meantime prepare all possible next steps before
	% collecting their results.

	setAttribute( State, host_infos, HostInfos ).



% Selects the most appropriate nodes on which the deployment manager, the
% data-logger, the root time manager and all local (i.e. non-root) time
% managers, the root data-exchanger and all local (i.e. non-root)
% data-exchangers should run.
%
% Placement constraints are:
%
% - C1: the deployment manager must be on the user node (as will deploy all
% other nodes from there)
%
% - C2: the root time manager and root data-exchanger must be on the same
% (computing) node (to minimise the inter-tick latency they induce)
%
% - C3: the user host must be further loaded as little as possible (as, if it is
% included in the simulation, it will include the user node *and* a computing
% node, and possibly the trace supervisor and other user applications; it may
% also be less powerful than the computing hosts, ex: it could be the laptop of
% the user)
%
% - C4: the rest of the agents should be spread as evenly as possible on all
% other hosts
%
% Note that a simulation case (ex: a test case) may need as well to interact
% directly with the data-exchanger service. Instead of replicating a possibly
% already-existing data-exchanger local to the user node (if that node was
% requested to be included), we will use that host-local data-exchanger if
% available, otherwise we will create a local exchanger just for the user node.
%
% Returns { LoadBalancerNode, { RootTimeManagerNode, LocalTimeManagerNodes },
%  DataLoggerNode, { RootDataExchangerNode, LocalDataExchangerNodes },
%  { RootInstanceTrackerNode, LocalInstanceTrackerNodes },
%  PerformanceTrackerNode }, where:
%
%  - LoadBalancerNode is the node where the load balancer should be spawned
%
%  - RootTimeManagerNode is the node where the root time manager should be
%  spawned
%
%  - LocalTimeManagerNodes corresponds to the list of all nodes except
%  RootTimeManagerNode
%
%  - DataLoggerNode is the node where the data-logger should be spawned
%
%  - RootDataExchangerNode is the node where the root data-exchanger should be
%  spawned
%
%  - LocalDataExchangerNodes corresponds to the list of all nodes except
% RootDataExchangerNode
%
%  - RootInstanceTrackerNode is the node where the root instance tracker should
%  be spawned
%
%  - LocalInstanceTrackerNodes corresponds to the list of all nodes except
%  RootInstanceTrackerNode
%
%  - PerformanceTrackerNode is the node where the performance tracker should be
%  spawned (if any)
%
% (all nodes are specified as atoms, 'undefined' is used if the corresponding
% service is disabled)
%
% (helper)
%
-spec dispatch_agents( [ atom_node_name() ], net_utils:node_naming_mode() ) ->
							 service_placement().
dispatch_agents( NodeList, NodeNamingMode ) ->

	%trace_utils:debug_fmt( "dispatch_agents: NodeList is ~p.", [ NodeList ] ),

	% We usually want a short hostname (ex: 'foobar', not a FQDN like
	% 'foobar.baz.org').
	%
	% Returns a string:
	UserHost = net_utils:get_naming_compliant_hostname( net_utils:localhost(),
														NodeNamingMode ),

	% The initial node list contains a list of Node@Host atoms (ex:
	% 'Data_Exchange_test-john@foobar'), we transform it into a list of
	% { Node@Host, HostString } pairs for easier and more efficient reorderings:
	% (ex: { 'Data_Exchange_test-john@foobar', "foobar" }).
	NodePairList = [ { N, node_to_host( N ) } || N <- NodeList ],

	% We want here to put any local (user) computing node at the end:
	ReorderedNodes = reorder_nodes( NodePairList, _LastHost=UserHost ),

	% Taking the last element of that list, while the user node has not been
	% added yet, results in getting the computation node running on the user
	% host:
	%
	{ UserComputingNode, _UserHost } =
		list_utils:get_last_element( ReorderedNodes ),

	% C1 already true by design.

	% C2 and C3: first position is not the user node, unless there is only one
	% node involved.
	%
	% There is at least one node (the user one):
	[ { FirstNode, FirstHost } | NonFirstNodePairs ] = ReorderedNodes,

	RootTimeManagerNode = FirstNode,

	% Minimising the overall inter-tick latency is crucial, thus we want the two
	% root agents (for time management and data exchange) to communicate as fast
	% as possible, thus they will created on the same node:
	%
	RootDataExchangerNode = RootTimeManagerNode,

	NonFirstNodes = [ N || { N, _Host } <- NonFirstNodePairs ],

	LocalTimeManagerNodes = NonFirstNodes,
	LocalDataExchangerNodes = LocalTimeManagerNodes,

	% Now we have to dispatch "evenly" load-balancer, data-logger and possibly
	% performance-tracker. For that we update again the "preferred order" of
	% nodes. We want to possibly end up with a list terminating by: FirstNode
	% then UserNode.
	%
	FirstReorderedList  = reorder_nodes( NodePairList, FirstHost ),
	SecondReorderedList = reorder_nodes( FirstReorderedList, UserHost ),

	UserNode = node(),

	AllComputingNodes = [ N || { N, _H } <- SecondReorderedList,
							   N =/= UserNode ],

	% For the moment, the performance tracker must be running from the user node
	% (later, as it is in some way coupled to the load balancer, they should be
	% created on the same node):
	%
	% Now we allocate the load balancer on the user host (not node) as well,
	% otherwise we had to send the full initialisation files to all other hosts,
	% involving much slow-down (and time-out) because of the compressing,
	% sending over the network and decompressing; we must target the user host,
	% but not the user node (which for example does not have the required time
	% manager)
	%
	%
	{ LoadBalancerNode, DataLoggerNode, RootInstanceTrackerNode,
	  PerformanceTrackerNode } =

		case SecondReorderedList of

			[ { _N1, _ }, { N2, _ }, { N3, _ } | _ ] ->
				% Here at least three nodes.
				% Each agent on a different node:
				%{ N1, N2, N3, UserNode };
				{ UserComputingNode, N2, N3, UserNode };

			[ { _N1, _ }, { N2, _ } ] ->
				% Each agent on a different node except the datalogger and root
				% instance tracker; now we also force the load balancer to be
				% placed on the user node, so that it can directly read any
				% initialisation file (which is potentially huge), instead of
				% including this file in the archive (thus requiring longer
				% compressing, sending and decompressing):

				%{ N1, N2, N2, UserNode };
				{ UserComputingNode, N2, N2, UserNode };

			[ { N, _ } ] ->
				% Only one node, no placement choice can be performed:
				%{ N, N, N, UserNode }
				{ UserComputingNode, N, N, UserNode }

	end,

	% May be changed in the future:
	ResultManagerNode = UserNode,

	% We add the user node to the list of local computing nodes on which an
	% instance tracker should be created (so that the performance tracker can
	% monitor the resources of the user node as well):
	%
	InstanceTrackerComputingNodes =
		[ N || { N, _ } <- NodePairList, N =/= RootInstanceTrackerNode ],

	LocalInstanceTrackerNodes = [ UserNode | InstanceTrackerComputingNodes ],

	% We want the manager to resist as much as possible:
	ResilienceManagerNode = UserNode,
	ResilienceAgentNodes = AllComputingNodes,

	% We place the binding (runtime) managers on the user node, since they have
	% a role similar to this deployment manager (attributing resources), and are
	% to create, each, one runtime container per computing node:
	%
	BindingManagersNode = UserNode,
	BindingResourcesNodes = AllComputingNodes,

	#service_placement{
		load_balancing=LoadBalancerNode,
		time_management={ RootTimeManagerNode, LocalTimeManagerNodes },
		data_logging=DataLoggerNode,
		data_exchanging={ RootDataExchangerNode, LocalDataExchangerNodes },
		result_management=ResultManagerNode,
		instance_tracking={ RootInstanceTrackerNode,
							LocalInstanceTrackerNodes },
		performance_tracking=PerformanceTrackerNode,
		resilience_management={ ResilienceManagerNode, ResilienceAgentNodes },
		bindings_management={ BindingManagersNode, BindingResourcesNodes } }.



% Determines and checks the parameters that the user specified.
%
% (helper)
%
determine_user_settings( SimulationSettings, DeploymentSettings,
						 LoadBalancingSettings ) ->

	SimulationName = sim_diasca:get_simulation_name( SimulationSettings ),

	SimInteractivityMode =
		interpret_simulation_interactivity_mode( SimulationSettings ),

	UIInteractivityMode = interpret_user_interface_interactivity_mode(),

	TickDuration = check_tick_duration( SimulationSettings ),

	EvaluationMode = check_evaluation_mode( SimulationSettings ),

	TroubleShootingMode = check_troubleshooting_mode( SimulationSettings ),

	NodeAvailabilityTolerance =
		check_node_availability_tolerance( DeploymentSettings ),

	DataLoggerWanted = check_data_logger_wanted( DeploymentSettings ),

	WebManagerInfos = check_webmanager_wanted( DeploymentSettings ),

	DataExchangerSettings = check_data_exchanger_settings( DeploymentSettings ),

	LanguageBindings = check_language_bindings_settings( DeploymentSettings ),

	PlacementPolicy = check_placement_policy( LoadBalancingSettings ),

	InitialisationFiles = check_initialisation_files( SimulationSettings ),

	ResultSpecification = check_result_specification( SimulationSettings ),

	ResilienceLevel =
		check_resilience_level( DeploymentSettings, SimInteractivityMode ),

	% Adopts a simpler, more tractable, canonical form:
	NewDeploymentSettings = DeploymentSettings#deployment_settings{
							  enable_webmanager=WebManagerInfos },

	FullSettings = { SimulationSettings, NewDeploymentSettings,
					 LoadBalancingSettings },

	{ SimulationName, SimInteractivityMode, UIInteractivityMode,
	  TickDuration, EvaluationMode, TroubleShootingMode,
	  NodeAvailabilityTolerance, DataLoggerWanted,
	  WebManagerInfos, DataExchangerSettings, LanguageBindings, PlacementPolicy,
	  InitialisationFiles, ResultSpecification, ResilienceLevel, FullSettings }.




% Section for some checkings:



% To avoid "The pattern {'simulation_settings', _, T, _, _, _, _, _} can never
% match since previous clauses completely covered the type
% #simulation_settings":
%
-dialyzer( { no_match, check_tick_duration/1 } ).


% Early check of user-specified tick duration, which is returned.
%
% (helper)
%
check_tick_duration( #simulation_settings{ tick_duration=T } )
  when is_float( T ) ->
	T;

check_tick_duration( #simulation_settings{ tick_duration=T } )
  when is_integer( T ) ->
	float( T );

check_tick_duration( #simulation_settings{ tick_duration=Other } ) ->
	throw( { invalid_tick_duration, Other } ).



% To avoid "The pattern {'simulation_settings'...} can never match since
% previous clauses completely covered the type #simulation_settings":
%
-dialyzer( { no_match, check_evaluation_mode/1 } ).


% Early check of user-specified evaluation mode, which is returned.
%
% (helper)
%
check_evaluation_mode( #simulation_settings{ evaluation_mode=M } )
  when M =:= fastest orelse M =:= reproducible orelse M =:= ergodic ->
	M;

check_evaluation_mode( #simulation_settings{
			   evaluation_mode= E = { reproducible, Seed } } ) ->
	random_utils:check_random_seed( Seed ),
	E;

check_evaluation_mode( #simulation_settings{ evaluation_mode=Other } ) ->
	throw( { invalid_evaluation_mode, Other } ).



% To avoid "The pattern {'simulation_settings'...} can never match since
% previous clauses completely covered the type #simulation_settings":
%
-dialyzer( { no_match, check_troubleshooting_mode/1 } ).


% Early check the user-specified troubleshooting mode.
%
% (helper)
%
check_troubleshooting_mode( #simulation_settings{
									   troubleshooting_mode=enabled } ) ->
	true;

check_troubleshooting_mode( #simulation_settings{
									   troubleshooting_mode=disabled } ) ->
	false;

check_troubleshooting_mode( #simulation_settings{
									   troubleshooting_mode=Other } ) ->

	throw( { invalid_troubleshooting_specification, Other  } ).



% To avoid "The pattern {'deployment_settings'...} can never match since
% previous clauses completely covered the type #simulation_settings":
%
-dialyzer( { no_match, check_node_availability_tolerance/1 } ).


% Early check of user-specified tolerance with regard to unavailable nodes,
% which is returned.
%
% (helper)
%
check_node_availability_tolerance( #deployment_settings{
			node_availability_tolerance=T } )
  when T =:= allow_unavailable_nodes orelse T =:= fail_on_unavailable_node ->
	T;

check_node_availability_tolerance( Other ) ->
	throw( { invalid_node_availability_tolerance, Other } ).



% To avoid "The pattern {'deployment_settings'...} can never match since
% previous clauses completely covered the type #simulation_settings":
%
-dialyzer( { no_match, check_data_logger_wanted/1 } ).


% Early check of the user-specified datalogger options.
%
% (helper)
%
check_data_logger_wanted( #deployment_settings{ enable_data_logger=true } ) ->
	true;

check_data_logger_wanted( #deployment_settings{ enable_data_logger=false } ) ->
	false;

check_data_logger_wanted( #deployment_settings{ enable_data_logger=Other } ) ->
	throw( { invalid_data_logger_setting, Other } ).




% To avoid "The pattern {'deployment_settings'...} can never match since
% previous clauses completely covered the type #simulation_settings":
%
-dialyzer( { no_match, check_webmanager_wanted/1 } ).


% Early check of the user-specified webmanager-related options (detailed
% checking done later).
%
% Returns a more canonical form as these settings.
%
% (helper)
%
check_webmanager_wanted( #deployment_settings{ enable_webmanager=false } ) ->
	false;

check_webmanager_wanted( #deployment_settings{ enable_webmanager=true } ) ->
	{ true, _WebProbeClassnames=[], undefined, undefined };

check_webmanager_wanted( #deployment_settings{
			enable_webmanager={ true, WebProbeClassnames } } ) ->
	{ true, WebProbeClassnames, undefined, undefined };

check_webmanager_wanted( #deployment_settings{
			enable_webmanager={ true, WebProbeClassnames, TCPPort } } ) ->
	{ true, WebProbeClassnames, TCPPort, undefined };

check_webmanager_wanted( #deployment_settings{
			enable_webmanager=Settings={ true, _WebProbeClassnames, _TCPPort,
										 _WebserverInstallRoot } } ) ->
	Settings;

check_webmanager_wanted( #deployment_settings{ enable_webmanager=Other } ) ->
	throw( { invalid_webmanager_setting, Other } ).




% To avoid "The pattern {'deployment_settings'...} can never match since
% previous clauses completely covered the type #simulation_settings":
%
-dialyzer( { no_match, check_data_exchanger_settings/1 } ).


% Early check of user-specified data-exchanger options.
%
% (helper)
%
check_data_exchanger_settings( S=#deployment_settings{
			enable_data_exchanger=true } ) ->
	S;

check_data_exchanger_settings( S=#deployment_settings{
			enable_data_exchanger={ true, SetConfigurationFileList } } )
	   when is_list( SetConfigurationFileList ) ->
	% File list will be checked later (in create_data_exchangers/5):
	S;

check_data_exchanger_settings( S=#deployment_settings{
			enable_data_exchanger=false } ) ->
	S;

check_data_exchanger_settings( #deployment_settings{
			enable_data_exchanger=Other } ) ->
	throw( { invalid_data_exchanger_setting, Other } ).




% To avoid "The pattern {'deployment_settings'...} can never match since
% previous clauses completely covered the type #simulation_settings":
%
-dialyzer( { no_match, check_language_bindings_settings/1 } ).


% Early check of user-specified language binding options.
%
% (helper)
%
check_language_bindings_settings( #deployment_settings{
									 enable_language_bindings=[] } ) ->
	[];

check_language_bindings_settings( #deployment_settings{
			enable_language_bindings=Languages } ) when is_list( Languages ) ->

	% Erlang not expected to be listed, yet should not trigger an error:
	%
	ActualLanguages = lists:delete( erlang, Languages ),

	SupportedLanguages = language_utils:get_supported_languages(),

	% Checks that each member of the list is a supported language indeed:
	[ check_language_binding_setting( L, SupportedLanguages )
	  || L <- ActualLanguages ],

	% Uniquifies the list of requested language bindings:
	list_utils:uniquify( ActualLanguages );

check_language_bindings_settings( #deployment_settings{
			enable_language_bindings=OtherSpec } ) ->
	throw( { invalid_language_bindings_specification, OtherSpec } ).




% (helper)
check_language_binding_setting( Language, SupportedLanguages )
  when is_atom( Language ) ->

	case lists:member( Language, SupportedLanguages ) of

		true ->
			ok;

		false ->
			throw( { language_not_supported, Language } )

	end;


check_language_binding_setting( { Language, CodePath }, SupportedLanguages )
  when is_atom( Language ) ->

	check_language_binding_setting( Language, SupportedLanguages ),

	case text_utils:are_strings( CodePath ) of

		true ->
			ok;

		false ->
			throw( { invalid_code_path_for, Language, CodePath } )

	end;


check_language_binding_setting( InvalidLanguageSpec, _SupportedLanguages ) ->
	throw( { invalid_language_binding_specification, InvalidLanguageSpec } ).




% To avoid "The pattern {'simulation_settings'...} can never match since
% previous clauses completely covered the type #simulation_settings":
%
-dialyzer( { no_match, check_placement_policy/1 } ).


% Early check of user-specified placement policy for load-balancing.
%
% (helper)
%
check_placement_policy( #load_balancing_settings{
						   placement_policy=round_robin } ) ->
	round_robin;

check_placement_policy( #load_balancing_settings{
						   placement_policy=Other } ) ->

	throw( { invalid_placement_policy, Other } ).



% To avoid "The pattern {'simulation_settings'...} can never match since
% previous clauses completely covered the type #simulation_settings":
%
-dialyzer( { no_match, check_initialisation_files/1 } ).


% Early check of user-specified placement policy for load-balancing.
%
% (helper)
%
check_initialisation_files( #simulation_settings{
						initialisation_files=InitialisationFiles } )
  when is_list( InitialisationFiles ) ->
	InitialisationFiles;

check_initialisation_files(
  #simulation_settings{ initialisation_files=Other } ) ->
	throw( { invalid_initialisation_files, Other } ).



% Early check of user-specified interactivity mode, which is returned.
%
% (helper)
%
check_result_specification( #simulation_settings{
							   result_specification=ResultSpecification } ) ->
	% Checked later, when creating the result manager:
	ResultSpecification.



% To avoid "The pattern {'deployment_settings'...} can never match since
% previous clauses completely covered the type #simulation_settings":
%
-dialyzer( { no_match, check_resilience_level/2 } ).


% Early check of the user-specified resilience level, which is returned.
%
% (helper)
%
check_resilience_level( #deployment_settings{ crash_resilience=none },
						_InteractivityMode ) ->
	0;

check_resilience_level( #deployment_settings{ crash_resilience=K },
		_InteractivityMode=batch ) when is_integer( K ) andalso K >= 0 ->
	check_stochastic_resilience(),
	K;

check_resilience_level( #deployment_settings{ crash_resilience=K },
		InteractivityMode=interactive ) when is_integer( K ) andalso K >= 0 ->
	throw( { unmatching_resilience_and_interactivity, K, InteractivityMode } );

check_resilience_level( #deployment_settings{ crash_resilience=Other },
		_InteractivityMode ) ->
	throw( { invalid_crash_resilience_specification, Other } ).




% To avoid "The pattern 'random' can never match the type 'rand'" and "The
% variable Other can never match since previous clauses completely covered the
% type 'rand'":
%
-dialyzer( { no_match, check_stochastic_resilience/0 } ).


% Early check for compatibility between user-specified settings.
%
-spec check_stochastic_resilience() -> void().
check_stochastic_resilience() ->

	case random_utils:get_random_module_name() of

		'random' ->
			ok;

		'rand' ->
			ok;

		Other ->
			% Not yet supported: the distributed seeds shall be serialised
			throw( { no_stochastic_resilience_for, Other } )

	end.



% Returns a list of { Hostname, Username } string pairs corresponding to the
% potential computing hosts the simulation might use.
%
% (helper)
%
-spec get_host_user_list( deployment_settings(), wooper:state() ) ->
								host_user_list().
get_host_user_list( #deployment_settings{ computing_hosts=ComputingHosts },
					State ) ->

	% Initial host list, as determined from the user-specified configuration, if
	% not overridden on the command-line:
	%
	HostInformation = case shell_utils:get_command_arguments_for_option(
							 '-sim-diasca-host-file' ) of

		undefined ->
			% Just read the settings defined in the simulation case:
			ComputingHosts;

		[ [ HostFilename ] ] when is_list( HostFilename ) ->
			% Overridden on the command-line, thus has priority:
			{ use_host_file, HostFilename, exclude_localhost };

		OtherHostArg ->
			throw( { invalid_host_file_specification, OtherHostArg } )

	end,

	%
	% Note that any { Host, User } duplicate pair will be removed; however { H,
	% U1 } and { H, U2 } may coexist.
	%
	% We remove duplicates, as otherwise the recursive file removal in the node
	% cleaner script may fail, as two processes would traverse the same tree to
	% perform a /bin/rm, leading to errors being returned as they step over each
	% other.
	%
	HostUserList = list_utils:uniquify(
			  determine_host_list_from( HostInformation,
										system_utils:get_user_name(), State ) ),

	HostStrings = [ Username ++ "@" ++ Hostname
				   || { Hostname, Username } <- HostUserList ],

	?debug_fmt( "The following ~B host candidate(s) (with users) "
		"were specified: ~s", [ length( HostStrings ),
								text_utils:strings_to_string( HostStrings ) ] ),

	% No need to check for clashing nodes (prevented by design).

	HostUserList.



% Sets up the instance tracking service.
%
% (helper)
%
set_up_instance_tracking( TroubleShootingMode, #service_placement{
  instance_tracking={ RootInstanceTrackerNode, LocalInstanceTrackerNodes } }
						 ) ->

	% Instance trackers must be created before the load balancer and the time
	% manager, so that they can know them (hence the instance tracking service
	% is the first one to exist, and all other services can rely on its
	% pre-existing):
	%
	RootInstanceTrackerPid =
		class_InstanceTracker:remote_synchronous_timed_new_link(
		  RootInstanceTrackerNode, _ParentInstanceTracker=none,
		  TroubleShootingMode ),

	% List of { TrackerPid, TrackerNode } pairs:
	%
	% (this includes the user node)
	%
	LocalInstanceTrackers = [
		   class_InstanceTracker:remote_synchronous_timed_new_link(
				Node,
				_ParentlInstanceTrackerPid=RootInstanceTrackerPid,
				TroubleShootingMode ) || Node <- LocalInstanceTrackerNodes ],


	% For some uses (like serialisation), we want to be able to contact as
	% directly as possible the instance tracker in charge of a PID. For that, we
	% start by requesting the local tracker, which may in turn request the one
	% of the PID node. As a result, each of the instance trackers should know
	% all of them:
	%
	AllInstanceTrackers = [ RootInstanceTrackerPid | LocalInstanceTrackers ],

	TrackerDeclarationMessage = { declareTrackers, [ AllInstanceTrackers ],
								  self() },

	[ Tracker ! TrackerDeclarationMessage || Tracker <- AllInstanceTrackers ],

	% While waiting, performing some registering:

	% We have to register all services, including this manager, and any service
	% from lower layers we want to be available through instance trackers:

	% First registers itself:
	ThisClassname = ?MODULE,
	class_InstanceTracker:register_agent( ThisClassname ),

	% Then lower-layer services:
	AggregatorPid = class_TraceAggregator:get_aggregator(
													_CreateIfNotFound=false ),

	class_InstanceTracker:register_agent( class_TraceAggregator,
										  AggregatorPid ),

	basic_utils:wait_for( { wooper_result, trackers_declared },
						  length( AllInstanceTrackers ) ),

	{ RootInstanceTrackerPid, LocalInstanceTrackers }.



% Sets up the plugin management service.
%
% (helper)
%
set_up_plugin_management(
  #deployment_settings{ plugin_directories=PluginDirs } ) ->

	% Needed as synchronous, otherwise a race condition exists with the start
	% notification (w.r.t. registering):
	%
	class_PluginManager:synchronous_new_link( PluginDirs ).



% Sets up the time management service.
%
% (helper)
%
set_up_time_management( TroubleShootingMode, InteractivityMode, TickDuration,
	   RootInstanceTrackerPid, #service_placement{
  time_management={ RootTimeManagerNode, LocalTimeManagerNodes } }, Context ) ->

	% The root time manager is directly linked to the deployment manager.

	% Hack to avoid that the local PID of the root time manager is the same as
	% the ones of the child managers: as the same series of simulation processes
	% (including the time manager) may be created on each node, their PID may be
	% difficult to discriminate (ex: all time managers may end up as <XXX.86.0>,
	% so a local <0.86.0> may actually mean <6919.86.0>, <6811.86.0>, etc.,
	% additionally the first part of the PID, for a given node, depends on the
	% node that examine it, i.e. the same PID will be displayed differently by
	% two remote nodes, as it is actually the atom slot integer for the node
	% name, with generally varies from a node to another.
	%
	% So this spawn allows the root time manager to be <*.87.0> while all child
	% time managers remain <*.86.0>:
	%
	% (useful to debugging time management)
	%
	%spawn( RootTimeManagerNode, fun() -> ok end ),

	% Creating now the time managers:

	RootTimeManagerPid = class_TimeManager:remote_synchronous_timed_new_link(
		_RootSpawnNode=RootTimeManagerNode,
		_RootTickDuration=TickDuration,
		_RootInteractivityMode=InteractivityMode,
		_RootTimeManagerPid=none,
		RootInstanceTrackerPid,
		TroubleShootingMode,
		Context ),


	% We should create local time managers only at start-up: during a resilience
	% rollback, they will be deserialised and managed accordingly.

	LocalTimeManagers = case Context of

		deploy_from_scratch ->

			% Simulation mode information will be given by the root time
			% manager:
			%
			% (we link non-root time managers too to the deployment manager for
			% an increased safety - knowing that the full time manager hierarchy
			% is linked, directly or not, to the root one - as a race condition
			% could exist if a non-root time manager was to crash before it
			% managed to link to its parent one)
			%
			[ class_TimeManager:remote_synchronous_timed_new( Node,
				  TickDuration, InteractivityMode, _ParentTM=RootTimeManagerPid,
				  RootInstanceTrackerPid, TroubleShootingMode,
				  _Context=deploy_from_scratch )
						 || Node <- LocalTimeManagerNodes ];

		_OtherContext ->
			% Created later (from serialised ones, during resilience rollback)
			undefined

	end,

	{ RootTimeManagerPid, LocalTimeManagers }.



% Sets up the result management service, and the data-logging one.
%
% (helper)
%
set_up_result_management_and_datalogging( SimulationName, StartTimestamp, SII,
	RootDir, VersionStrings, TickDuration, ResultSpecification, AvailableHosts,
	RootTimeManagerPid, DataLoggerWanted,
	#service_placement{ result_management=ResultManagerNode,
						data_logging=DataLoggerNode }, Context ) ->

	ResultDirName = get_result_directory_name( SimulationName, StartTimestamp,
											   SII ),

	RunDir = file_utils:get_current_directory(),

	ResultDir = file_utils:join( RunDir, ResultDirName ),

	case Context of

		deploy_from_scratch ->

			% Must be done only initially:
			file_utils:create_directory( ResultDirName ),

			% Copying the post-mortem generation script to the simulation result
			% directory as soon as the result directory is created, so that it
			% is available in all cases:

			ScriptName = "generate-results-post-mortem.sh",

			PostMortemScript = file_utils:join( [ "src", "core", "services",
				"data-management", "result-management", ScriptName ] ),

			PostMortemScriptPath = file_utils:join( RootDir, PostMortemScript ),

			DestScriptFile = file_utils:join( ResultDir, ScriptName ),

			file_utils:copy_file_if_existing( PostMortemScriptPath,
											  DestScriptFile );


		_OtherContext ->
			ok


	end,

	% The meta-data that shall be available from all result producers:
	ResultMetadata = get_result_metadata( VersionStrings, SimulationName,
										  TickDuration ),

	% This deployment manager is on the user node, the result manager must be
	% there also, thus no remote creation:
	%
	% (we must create it prior to the data-logger, as the latter is a result
	% producer that, when created, will look-up the former)
	%
	ResultManagerPid = class_ResultManager:remote_synchronous_timed_new_link(
		ResultManagerNode, ResultSpecification, DataLoggerWanted,
		RootTimeManagerPid, RunDir, ResultDir, ResultMetadata ),

	% UserNodeInfos = { UserNodeName, UserHostAtom }:
	{ HostCoreList, UserNodeInfos } = get_host_information( AvailableHosts ),

	ResultManagerPid ! { setResourceMapping, [ HostCoreList, UserNodeInfos  ] },

	% We used to create it unconditionally (regardless of DataLoggerWanted), as
	% the actual database operations are triggered only when actually used,
	% however gnuplot is then always sought after (even if not needed), making
	% thus some deployments fail whereas they should not:
	%
	DataLoggerPid = case DataLoggerWanted of

		true ->
			class_DataLogger:create_main_datalogger( DataLoggerNode,
													 ResultMetadata );

		false ->
			undefined

	end,

	{ ResultManagerPid, DataLoggerPid, ResultDir }.



% Sets up the web management service.
%
% (helper)
%
set_up_web_management( SII, EngineRootDir, InteractivityMode, ResultManagerPid,
	 ResultDir, _WebManagerInfos={ true, _WebProbeClassnames, MaybeTCPPort,
								   MaybeWebserverInstallRoot } ) ->

	% Already partly in a canonical form; only created on the user node (as
	% deemed in this case able to launch a local webserver); linked, timed and
	% synchronous, yet not waiting for the launched webserver to be up and
	% ready, as this would be a bit complex (HTTP connection polling), may be
	% long, and at worse the user of the browser may poll by reloading until
	% ready (the creation of the web manager itself *is* synchronous, and web
	% producers rely on it):
	%

	% No need to involve SII, as the parent directory (ResultDir) already
	% includes it.

	WebserverContentRoot = class_WebManager:get_web_content_root( ResultDir ),

	class_WebManager:create_manager( SII, EngineRootDir, InteractivityMode,
			WebserverContentRoot, ResultManagerPid,
			MaybeWebserverInstallRoot, MaybeTCPPort );


set_up_web_management( _SII, _EngineRootDir, _InteractivityMode,
				   _ResultManagerPid, _ResultDir, _WebManagerInfos=false ) ->
	undefined.





% Sets up the load-balancing service.
%
% (helper)
%
set_up_load_balancing( PlacementPolicy, SelectedNodes,
		NodeAvailabilityTolerance, EvaluationMode, TroubleShootingMode,
		RootTimeManagerPid, AllInstanceTrackers, RootDirectory,
		#service_placement{ load_balancing=LoadBalancerNode },
		InitialisationFiles ) ->

	CurrentDir = file_utils:get_current_directory(),

	% We will change the current working directory afterwards:
	ActualInitialisationFiles = [ translate_path( F, RootDirectory, CurrentDir )
								  || F <- InitialisationFiles ],


	% I knew that some day this incredible operator would be needed:
	LoadBalancerPid = class_LoadBalancer:remote_synchronous_timed_new_link(
		LoadBalancerNode, PlacementPolicy, SelectedNodes,
		NodeAvailabilityTolerance, EvaluationMode, TroubleShootingMode,
		ActualInitialisationFiles ),

	% Instance trackers and the root time manager were created beforehand:

	% wooper:send_requests_and_wait_acks(
	%   _RequestName=setLoadBalancerPid,
	%   _RequestArgs=LoadBalancerPid,
	%   _TargetInstancePIDs=[ RootTimeManagerPid | AllInstanceTrackers ]
	%   _AckAtom=load_balancer_set ),

	% We must notify the time manager in charge of the load balancer that it
	% must manage this special actor (ex: it is the only one to be created with
	% a non-empty agenda for bootstrapping purposes).
	%
	% We must also notify the root time manager of the PID of this load
	% balancer, whether or not this is the time manager of the load balancer:
	%
	TimeManagerOfLoadBalancerPid = naming_utils:get_locally_registered_pid_for(
				_Name=?time_manager_name, _TargetNode=LoadBalancerNode ),


	LoadBalancerMessage = { setLoadBalancerPid, LoadBalancerPid, self() },

	% Here we notify these agents of the PID of the load balancer:
	[ T ! LoadBalancerMessage ||
		T <- [ RootTimeManagerPid | AllInstanceTrackers ] ],

	% We use a different request as the load balancer may or may not be on the
	% same host as the root time manager:
	%
	TimeManagerOfLoadBalancerPid ! { registerBootstrapScheduling,
									 LoadBalancerPid, self() },

	% All previous requests are to return the same acknowledge message:
	basic_utils:wait_for( _Message={ wooper_result, load_balancer_set },
						  _Count=length( AllInstanceTrackers ) + 2 ),

	LoadBalancerPid.




% Sets up the language binding support services.
%
% (helper)
%
set_up_binding_managers( LanguageBindings, RootDir, EpmdPort,
		#service_placement{ bindings_management={ BindingManagersNode,
												  BindingResourcesNodes } },
						 State ) ->

	% The binding managers are given a BindingManagersNode during the service
	% placement, but we want it to be equal to UserNode and it is thus unused,
	% at least currently:

	BindingManagersNode = node(),

	BindingLangManagerPairs = [ begin

		{ Lang, CodePath } = case LangSpec of

			Pair={ _Lg, _Cp } ->
				Pair;

			JustLg ->
				% If just the language is specified, no extra code path applies:
				{ JustLg, _Cp=[] }

		end,

		BindingManagerClass = binding_utils:get_binding_manager_class( Lang ),

		% Local by design; returns the PID of the corresponding binding manager:
		BdManagerPid = BindingManagerClass:synchronous_new_link(
				 BindingResourcesNodes, RootDir, EpmdPort, CodePath, self() ),

		{ Lang, BdManagerPid }

						   end || LangSpec <- LanguageBindings ],

	{ Languages, LangManagers } = lists:unzip( BindingLangManagerPairs ),

	case Languages of

		[] ->
			ok;

		_ ->
			?info_fmt( "Waiting for the managers of following ~B bindings: ~s",
				[ length( Languages ),
				  text_utils:atoms_to_string( Languages ) ] ),
			% To ensure synchronous operations:
			wait_for_binding_managers( LangManagers )

	end,

	binding_utils:set_binding_managers_record( BindingLangManagerPairs ).



% (helper)
wait_for_binding_managers( _LangManagers=[] ) ->
	ok;

wait_for_binding_managers( LangManagers ) ->

	receive

		{ notifyBindingManagerReady, LangManagerPid } ->
			NewWaited = list_utils:delete_existing( LangManagerPid,
													LangManagers ),
			wait_for_binding_managers( NewWaited )

	end.



% Sets up the data-exchanging service.
%
% Creates a data-exchanger, if needed: as this service is optional, returns
% RootDataExchangerPid or undefined.
%
% (helper)
%
set_up_data_exchanging(
		#deployment_settings{ enable_data_exchanger=true },
		RootTimeManagerPid,
		BaseNodeName,
		_RootDirectory,
		#service_placement{ data_exchanging={ RootDataExchangerNode,
											  LocalDataExchangerNodes } } ) ->

	create_data_exchangers( RootDataExchangerNode, _ConfigurationFileList=[],
			RootTimeManagerPid, LocalDataExchangerNodes, BaseNodeName );

set_up_data_exchanging(
		#deployment_settings{
				   enable_data_exchanger={ true, ConfigurationFileList } },
		RootTimeManagerPid,
		BaseNodeName,
		RootDirectory,
		#service_placement{ data_exchanging={ RootDataExchangerNode,
											  LocalDataExchangerNodes } } ) ->

	% A configuration file may be defined relatively to the case directory:

	CurrentDir = file_utils:get_current_directory(),

	NewConfigFiles = [ translate_path( F, RootDirectory, CurrentDir )
					   || F <- ConfigurationFileList ],

	create_data_exchangers( RootDataExchangerNode, NewConfigFiles,
			RootTimeManagerPid, LocalDataExchangerNodes, BaseNodeName );

set_up_data_exchanging( #deployment_settings{ enable_data_exchanger=false },
						_RootTimeManagerPid, _BaseNodeName, _RootDirectory,
						_ServicePlacement ) ->
	undefined.




% Sets up the resilience management service.
%
% (helper)
%
set_up_resilience_management( FullSettings, RootTimeManagerPid,
		ResultManagerPid, StartTimestamp, SII, RootDir, AvailableHosts,
		#service_placement{ resilience_management={
					 ResilienceManagerNode, ResilienceAgentNodes } } ) ->

	% Created on the user node (shall be the last and and only process standing
	% in case of crash).
	%
	% Will create in turn the local resilience agents on all computing nodes:
	%
	% (no link created here - otherwise the resilience manager will die with all
	% other managers; instead we create non-atomically a monitor, so that if the
	% resilience manager crashes - it may happen - this deployment manager will
	% know, while if the latter crashes, the former will not be directly
	% impacted)
	%
	ResilienceManagerPid = class_ResilienceManager:remote_synchronous_timed_new(
		ResilienceManagerNode, FullSettings, ResilienceAgentNodes,
		RootTimeManagerPid, ResultManagerPid, StartTimestamp, SII, RootDir,
		AvailableHosts ),

	ResilienceManagerRef = erlang:monitor( process, ResilienceManagerPid ),

	{ ResilienceManagerPid, ResilienceManagerRef }.



% Sets up the performance tracking service.
%
% Creates a performance tracker, if needed.
%
% (helper)
%
set_up_performance_tracking(
		#deployment_settings{ enable_performance_tracker=true }, ResultDir,
		RootTimeManagerPid, SelectedNodes, AllInstanceTrackers,
		LoadBalancerPid, Context ) ->

	ResultDirInfo = case Context of

		deploy_from_scratch ->
			ResultDir;

		_OtherContext ->
			{ ResultDir, do_not_create }

	end,

	PerfTrackPid = class_PerformanceTracker:synchronous_timed_new_link(
					% Created on user node: PerformanceTrackerNode,
					?performance_tracker_name,
					?performance_tracker_registration_scope,
					ResultDirInfo ),

	% Asynchronous is not a problem here:
	PerfTrackPid ! { start, [ RootTimeManagerPid, [ node() | SelectedNodes ],
							  AllInstanceTrackers, LoadBalancerPid ] },

	PerfTrackPid;


set_up_performance_tracking(
		#deployment_settings{ enable_performance_tracker=false }, _ResultDir,
		_RootTimeManagerPid, _SelectedNodes, _AllInstanceTrackers,
		_LoadBalancerPid, _Context ) ->

	undefined.



% Returns the actual, overall simulation interactivity setting, as set in the
% simulation settings.
%
-spec interpret_simulation_interactivity_mode( simulation_settings() ) ->
							 class_TimeManager:simulation_interactivity_mode().
interpret_simulation_interactivity_mode(
		#simulation_settings{ simulation_interactivity_mode=interactive } ) ->
	interactive;

interpret_simulation_interactivity_mode(
		#simulation_settings{ simulation_interactivity_mode=batch } ) ->
	batch;

interpret_simulation_interactivity_mode(
		#simulation_settings{ simulation_interactivity_mode=Other } ) ->
	throw( { invalid_simulation_interactivity_mode, Other } ).



% Returns the actual, overall user-interface interactivity setting, as possibly
% set by the command-line options.
%
-spec interpret_user_interface_interactivity_mode() -> ui:interactivity_mode().
interpret_user_interface_interactivity_mode() ->

	% Note that the next tested batch status regards only the interactivity of
	% the user-interface, not the one of the simulation:
	%
	case executable_utils:is_batch() of

		true ->
			batch;

		false ->
			interactive
	end.



% Returns the firewall-related options, as determined from the deployment
% settings.
%
-spec interpret_firewall_options( deployment_settings() ) -> static_return(
		{ maybe( net_utils:tcp_port() ), net_utils:tcp_port_restriction() } ).
interpret_firewall_options( DeploymentSettings ) ->

	Options = DeploymentSettings#deployment_settings.firewall_restrictions,

	% Returns { EpmdPortOption, TcpRangeOption }:
	FinalOptions = interpret_firewall_options( Options,
							_Defaults={ undefined, no_restriction } ),

	wooper:return_static( FinalOptions ).



% Clearer than proplists:
interpret_firewall_options( none, Acc ) ->
	Acc;

interpret_firewall_options( _Opts=[], Acc ) ->
	Acc;

interpret_firewall_options( _Opts=[ { epmd_port, Port } | T ],
							{ undefined, Range } ) when is_integer( Port ) ->
	interpret_firewall_options( T, { Port, Range } );

interpret_firewall_options(_Opts= [ { epmd_port, Port } | _T ],
						   { undefined, _Range } ) ->
	throw( { incorrect_firewall_restriction, invalid_epmd_port, Port } );

interpret_firewall_options( _Opts=[ { epmd_port, _Port } | _T ],
							{ _Port, _Range } ) ->
	throw( { incorrect_firewall_restriction,
			 epmd_port_defined_more_than_once } );

interpret_firewall_options(
  _Opts=[ { tcp_restricted_range, R={ Min, Max } } | T ],
  { Port, no_restriction } )
  when is_integer( Min ) andalso is_integer( Max ) andalso Min < Max ->
	interpret_firewall_options( T, { Port, R } );

interpret_firewall_options(
  _Opts=[ { tcp_restricted_range, R={ Min, Max } } | _T ],
  { _Port, no_restriction } )
  when is_integer( Min ) andalso is_integer( Max ) ->
	throw( { incorrect_firewall_restriction, invalid_tcp_range, R } );

interpret_firewall_options(
  _Opts=[ { tcp_restricted_range, R={ _Min, _Max } } | _T ],
  { _Port, no_restriction } ) ->
	throw( { incorrect_firewall_restriction, invalid_tcp_bounds, R } );

interpret_firewall_options( _Opts=[ { tcp_restricted_range, _ARange } | _T ],
							{ _Port, _AnotherRange } ) ->
	throw( { incorrect_firewall_restriction,
			tcp_range_defined_more_than_once } );

interpret_firewall_options( _Opts=[ Other | _T ], _Acc ) ->
	throw( { incorrect_firewall_restriction, incorrect_option, Other } ).



% Interpret the 'ping_available' field of the deployment settings.
-spec interpret_ping_option( deployment_settings() ) -> boolean().
interpret_ping_option( #deployment_settings{ ping_available=true } ) ->
	true;

interpret_ping_option( #deployment_settings{ ping_available=false } ) ->
	false;

interpret_ping_option( #deployment_settings{ ping_available=Other } ) ->
	throw( { invalid_ping_available_option, Other } ).




% Returns a string describing the specified list of failed hosts and the reason
% for their unavailability.
%
% (helper)
%
interpret_failed_hosts( _Failed=[], _State ) ->
	"no failed host";

interpret_failed_hosts( Failed, State ) ->
	interpret_failed_hosts( Failed, _Acc=[], State ).



interpret_failed_hosts( _FailedHost=[], Acc, _State ) ->
	text_utils:strings_to_string( Acc );

interpret_failed_hosts( [ { FailedHost, Reason } | H ], Acc, State ) ->

	#computing_host_info{ host_name=Hostname, user_name=Username } =
		get_host_info( FailedHost, State ),

	interpret_failed_hosts( H,
		[ text_utils:format( "~s for user ~s: ~s", [ Hostname, Username,
					 interpret_host_failure( Reason ) ] ) | Acc ], State ).



% Interprets the reason for an host failure.
-spec interpret_host_failure( class_ComputingHostManager:host_failure_reason() )
							-> static_return( ustring() ).
interpret_host_failure( host_not_available ) ->
	wooper:return_static( "host does not answer to ping requests" );

interpret_host_failure( deployment_time_out ) ->
	wooper:return_static( "deployment failed on time-out" );

interpret_host_failure( vm_detection_abnormal ) ->
	wooper:return_static( "abnormal local computing node detection" );

interpret_host_failure( vm_detection_none ) ->
	wooper:return_static( "no local node found (abnormal)" );

interpret_host_failure( launched_vm_not_found ) ->
	wooper:return_static( "launched local computing node not found" );

interpret_host_failure( multiple_vms_detected ) ->
	wooper:return_static( "launched local computing node not responding, "
						  "whereas multiple nodes were found" );

interpret_host_failure( vm_detection_failed ) ->
	wooper:return_static( "local computing node detection failed" );

interpret_host_failure( vm_remote_detection_abnormal ) ->
	wooper:return_static( "abnormal remote computing node detection" );

interpret_host_failure( remote_launched_vm_not_found ) ->
	wooper:return_static( "launched remote computing node not found" );

interpret_host_failure( one_remote_vm_detected ) ->
	wooper:return_static( "launched remote computing node not responding, "
						  "exactly one remote node found" );

interpret_host_failure( multiple_remote_vms_detected ) ->
	wooper:return_static( "launched remote computing node not responding, "
						  "multiple remote nodes found" );

interpret_host_failure( vm_remote_detection_failed ) ->
	wooper:return_static( "remote computing node detection failed" ).



% Takes care of the simulation package: ensures it is available as a file, to be
% sent to all computing nodes that are to take part to the simulation.
%
% Returns the corresponding filename, as a string, and a list of the
% corresponding selected files for the archive, in order that they can be
% checked.
%
% (helper)
%
-spec manage_simulation_package( wooper:state() ) ->
				   { file_name(), [ file_name() ] }.
manage_simulation_package( State ) ->

	% Previously a binary was returned, for a sending as a message, but now we
	% use a far better choice, sendfile, hence we need to rely on a file.

	DeploySettings = ?getAttr(deployment_settings),

	case DeploySettings#deployment_settings.package_manager of

		% The default setting:
		generate_deployment_package ->
			{ Bin, SelectedFiles } = build_simulation_package( State ),
			TargetFilename = get_default_deployment_package_name(),
			save_simulation_package( Bin, TargetFilename, State ),
			{ TargetFilename, SelectedFiles };

		{ generate_and_save_deployment_package, TargetFilename } ->
			{ Bin, SelectedFiles } = build_simulation_package( State ),
			save_simulation_package( Bin, TargetFilename, State ),
			{ TargetFilename, SelectedFiles };

		{ use_deployment_package, SourceFilename } ->
			% Pre-existing packages supposed already checked for duplicates:
			case file_utils:is_existing_file( SourceFilename ) of

				true ->
					{ SourceFilename, _SelectedFiles=[] };

				false ->
					throw( { specified_deployment_package_not_found,
							 SourceFilename } )

			end

	end.



% Saves the specified (binary) simulation package, in specified file.
%
% (helper)
%
save_simulation_package( BinaryPackage, TargetFilename, State ) ->

	% delayed_write would not be terribly useful here, if not
	% counter-productive:
	%
	% (maybe could also have been opened in binary mode)
	%
	File = file_utils:open( TargetFilename, [ write, raw ] ),
	file_utils:write( File, BinaryPackage ),
	file_utils:close( File ),
	?info_fmt( "Deployment package has been successfully written "
			   "to the '~s' file.", [ TargetFilename ] ).



% Builds the simulation package, and returns it as a binary.
%
% (helper)
%
-spec build_simulation_package( wooper:state() ) ->
									  { binary(), [ file_name() ] }.
build_simulation_package( State ) ->

	% We have to ensure first that all filesystem entries that are relative (not
	% absolute) are from now on defined relatively to the root of the engine.

	DeploySettings = ?getAttr(deployment_settings),

	% The root directory of the engine is the one all relative paths should be
	% relative to:
	%
	RootDir = ?getAttr(engine_root_dir),

	BinRootDir = text_utils:string_to_binary( RootDir ),

	WebDeploySettings =
		case DeploySettings#deployment_settings.enable_webmanager of

			false ->
			  DeploySettings;

			{ true, WebProbeClassnames, _TCPPort, _WebserverInstallRoot } ->

			  CurrentDir = file_utils:get_current_directory(),
			  AddedElems = lists:foldl(
					fun( Classname, Elems ) ->
						Classname:get_elements_to_deploy( BinRootDir ) ++ Elems
					end,
					_Acc0=[],
					_List=WebProbeClassnames ),

			  % As these elements are added on a per-service, general basis,
			  % their path could not be relative to the current run directory
			  % (which depends on the simulation case being run), so:
			  %
			  RelativeAddedElems = [ case Elem of

				{ ElemPath, ElemType, ElemOpts } ->
					{ file_utils:make_relative( ElemPath, _RefDir=CurrentDir ),
					  ElemType, ElemOpts } ;

				{ ElemPath, ElemType } ->
					{ file_utils:make_relative( ElemPath, _RefDir=CurrentDir ),
					  ElemType }

									end || Elem <- AddedElems ],
			  BaseElems =
			   DeploySettings#deployment_settings.additional_elements_to_deploy,

			  AllElems = RelativeAddedElems ++ BaseElems,

			  DeploySettings#deployment_settings{
				additional_elements_to_deploy=AllElems }

	end,


	% We add a default code entry pointing to the 'src' directory, as presumably
	% found from a 'test' directory typically (otherwise all test cases would
	% have to specify such an entry - whereas it is mostly harmless otherwise):
	%
	RelatedSrcDir = "../src",

	UserElems =
		WebDeploySettings#deployment_settings.additional_elements_to_deploy,

	% As non-existing directories are not allowed:
	ComplementedElems =
		case file_utils:is_existing_directory( RelatedSrcDir ) of

			true ->
				[ { RelatedSrcDir, code } | UserElems ];

			false ->
				UserElems

	end,

	ComplementedDeploySettings = WebDeploySettings#deployment_settings{
					additional_elements_to_deploy=ComplementedElems },

	% Starts from the current directory for relative paths:
	InitialDir = file_utils:get_current_directory(),


	AdditionalElemList = make_paths_root_relative( ComplementedElems, RootDir,
												   InitialDir ),

	%trace_utils:debug_fmt( "AdditionalElemList: ~p.", [ AdditionalElemList ] ),

	ConfigurationFiles = case
		ComplementedDeploySettings#deployment_settings.enable_data_exchanger of

		{ true, ConfFiles } when is_list( ConfFiles ) ->
			[ { translate_path( F, RootDir, InitialDir ), data }
			  || F <- ConfFiles ];

		_ ->
			% Ignores invalid entries here, they will be checked by the data
			% exchanger anyway:
			[]

	end,


	% We use to include initialisation files in the archives but, even if these
	% files were compressed beforehand, their size was leading to too long
	% sending and uncompressing phases. Now we ensure the load balancer is
	% created on the user node and that it reads directly the initialisation
	% files, so initialisation shall not be included anymore in the simulation
	% archive:

	%SimulationSettings = ?getAttr(simulation_settings),

	% They may have to be read from another node as well:
	%InitialisationFiles = [ { translate_path( F, RootDir, InitialDir ), data }
	%	|| F <- SimulationSettings#simulation_settings.initialisation_files ],

	% We start from the engine - which has obviously to be deployed - and we add
	% any user-supplied content:
	%
	MustRebuild = ComplementedDeploySettings#deployment_settings.rebuild_on_deployment_package_generation,

	ActualAdditions = get_engine_deployment_settings( MustRebuild )
		++ AdditionalElemList ++ ConfigurationFiles, % ++ InitialisationFiles,

	CanonizedAdditions = [ standardise_deploy_element( Addition )
						   || Addition <- ActualAdditions ],

	%trace_utils:debug_fmt( "CanonizedAdditions: ~p.", [ CanonizedAdditions ] ),

	RootDir = ?getAttr(engine_root_dir),

	file_utils:set_current_directory( RootDir ),

	% The selected paths will be inspected later to detect any duplicated
	% filename:
	%
	Selected = manage_rebuild_and_select( CanonizedAdditions, State ),

	SortedSelected = lists:sort( Selected ),

	%trace_utils:debug_fmt( "Archive elements: ~p", [ SortedSelected ] ),

	RuleString = text_utils:format( "Following ~B deployment selection rules "
		"have been applied: ~s", [ length( CanonizedAdditions ),
		  text_utils:strings_to_string( [ text_utils:format( "rule ~p", [ R ] )
			  || R <- CanonizedAdditions ] ) ] ),

	FileString = text_utils:format( "Following ~B files have been selected "
	  "(ordered alphabetically): ~s", [ length( SortedSelected ),
		text_utils:strings_to_string(
		  [ text_utils:format( "~s", [ F ] ) || F <- SortedSelected ] ) ] ),


	?info_fmt( "Building simulation package from the base root simulator "
		"directory '~s'.~n~s~n~n~s~n", [ RootDir, RuleString, FileString ] ),

	%trace_utils:debug_fmt( "Building simulation package from "
	%			"the base root simulator directory '~s'."
	%			"~nFollowing files have been selected:~n~p.~n"
	%			"~nFollowing deployment selection rules have been "
	%			"applied:~n~p.",
	%			[ RootDir, Selected, CanonizedAdditions ] ),

	% We are still in root directory here.

	PackageBin = file_utils:files_to_zipped_term( Selected ),

	?info_fmt( "Built package contains ~B files and has for size ~s.",
			   [ length( Selected ),
				 system_utils:interpret_byte_size( size( PackageBin ) ) ] ),

	% Restores current (VM-wide) working directory, otherwise for example the
	% deployment BEAM will not be found anymore:
	%
	file_utils:set_current_directory( InitialDir ),

	{ PackageBin, Selected }.



% Returns the specified paths (see element_spec()) transformed so that relative
% ones are defined relatively to RootDir (some checks already performed).
%
make_paths_root_relative( Elems, RootDir, CurrentDir ) ->

	%trace_utils:debug_fmt( "Elems = ~p", [ Elems ] ),

	RootDirLen = length( RootDir ),

	make_paths_root_relative( Elems, RootDir, RootDirLen, CurrentDir, _Acc=[] ).


make_paths_root_relative( _Elems=[], _RootDir, _RootDirLen, _CurrentDir,
						  Acc ) ->
	Acc;

make_paths_root_relative( _Elems=[ { Path, Type, Opt } | T ], RootDir,
						  RootDirLen, CurrentDir, Acc ) ->

	check_elem_type( Type ),

	NewPath = translate_path( Path, RootDir, RootDirLen, CurrentDir ),

	make_paths_root_relative( T, RootDir, RootDirLen, CurrentDir,
							  [ { NewPath, Type, Opt } | Acc ] );


make_paths_root_relative( _Elems=[ { Path, Type } | T ], RootDir, RootDirLen,
						  CurrentDir, Acc ) ->

	check_elem_type( Type ),

	NewPath = translate_path( Path, RootDir, RootDirLen, CurrentDir ),

	make_paths_root_relative( T, RootDir, RootDirLen, CurrentDir,
							  [ { NewPath, Type } | Acc ] ).



% Checks the specified type for the element to deploy.
check_elem_type( data ) ->
	ok;

check_elem_type( code ) ->
	ok;

check_elem_type( Other ) ->
	trace_utils:error_fmt( "User-specified type ('~p') for element to "
		"deploy is invalid (expecting 'data' or 'code').", [ Other ] ),
	throw( { invalid_element_type, Other } ).



% Translates specified full path so that it becomes relative to the root
% directory, rather than the current directory.
%
translate_path( Path, RootDir, CurrentDir ) ->
	translate_path( Path, RootDir, length( RootDir ), CurrentDir ).


translate_path( Path, RootDir, RootDirLen, CurrentDir )
  when is_list( Path ) ->

	case file_utils:is_absolute_path( Path ) of

		true ->
			Path;

		false ->
			% We must transform this path relative to the current directory into
			% a path relative to the root one; for that we check that, once made
			% absolute, the beginning of the specified path matches the root
			% directory, and then we keep only its second part, thus made
			% relative to this root directory:
			%
			RelativeToCurrent = file_utils:normalise_path(
								  file_utils:join( CurrentDir, Path ) ),

			case string:sub_string( RelativeToCurrent, _Start=1,
									_Stop=RootDirLen ) of

				RootDir ->
					% Yes, the prefix matches, let's remove it:
					%
					% (we have to jump over the last separator of the prefix)
					%
					string:sub_string( RelativeToCurrent,
									   _SuffixStart=RootDirLen + 2 );

				_ ->
					throw( { path_not_relative_to_engine_dir, Path, RootDir } )

			end

	end;

translate_path( Path, _RootDir, _RootDirLen, _CurrentDir ) ->
	trace_utils:error_fmt( "The path '~p', specified among the additional "
		"elements to deploy, is invalid (not a string).", [ Path ] ),
	throw( { non_string_path, Path } ).



% Returns the settings appropriate to the deployment of the Sim-Diasca engine
% itself.
%
% (helper)
%
get_engine_deployment_settings( MustRebuild ) ->

	RebuildOpt = case MustRebuild of

		true ->
			rebuild;

		false ->
			no_rebuild

	end,

	% Here we just happen to select all BEAM files (*.beam), except the test
	% ones (*_test.beam), and only them:
	%
	% (we do not use '{exclude_suffixes,get_basic_blacklisted_suffixes()}' as
	% anyway we will keep only the remaining .beam files):
	%
	DeployOptions = [ { exclude_suffixes, ["_test.beam"] },
					  %{ exclude_directories, [ ".svn" ] },
					  { keep_only_suffixes, [ ".beam", ".class" ] },
					  RebuildOpt ],

	% Our base layers are special-cased, as applying OTP conventions results in
	% having each BEAM being duplicated, with one copy in the original build
	% directory and one (for OTP compliance) in the overall ebin directory of
	% that layer.

	% Doing so avoid that any Erlang/OTP build tree resulting from a run of
	% install-erlang.sh directly from myriad/conf results in trying to include
	% in the simulation archive the full Erlang compiled code base:
	%
	% (current directory: base simulation root, common to all layers, yet all
	% filesystem elements such as excluded directories should be defined
	% relatively to the root of their own layer)
	%
	MyriadOptions =
		[ { exclude_directories, [ "conf", "ebin" ] } | DeployOptions ],

	WOOPERExclDirs = [ file_utils:join( "priv", "examples" ), "ebin" ],
	WOOPEROptions = [ { exclude_directories, WOOPERExclDirs } | DeployOptions ],

	TracesOptions = [ { exclude_directories, [ "ebin" ] } | DeployOptions ],

	% Commented-out, as would make the corresponding model tests fail:
	%SimDiascaOptions = [ {exclude_directories,["src/models"]} |DeployOptions],

	SimDiascaOptions = DeployOptions,

	[ { "myriad",     code, MyriadOptions },
	  { "wooper",     code, WOOPEROptions },
	  { "traces",     code, TracesOptions },
	  { "sim-diasca", code, SimDiascaOptions } ].



% Ensures that all deploy options are known, and that if multiple lists are
% specified, they are correctly merged.
%
% (helper)
%
standardise_deploy_element( { ElementPath, ElementType } ) ->
	standardise_deploy_element( { ElementPath, ElementType, _Option=[] } );

% Encloses any standalone option into a list:
standardise_deploy_element( { ElementPath, ElementType, Option } )
  when not is_list( Option ) ->
	standardise_deploy_element( { ElementPath, ElementType, [ Option ] } );

% Actual returning of the standardised version:
% Code implies BEAM-only:
%
standardise_deploy_element( { ElementPath, code, OptionList } ) ->

	% We do not add ".py" next to ".beam" here as, even in the engine,
	% duplicates (ex: __init__.py, from bindings/python/api) would be detected:
	%
	StandardOpts = standardise_deploy_options( OptionList,
								_Acc={ [], [], [ ".beam" ], [] } ),
	{ ElementPath, code, StandardOpts };

standardise_deploy_element( { ElementPath, ElementType, OptionList } ) ->
	StandardOpts = standardise_deploy_options( OptionList,
								_Acc={ [], [], [], [] } ),
	{ ElementPath, ElementType, StandardOpts }.



% Merges relevant deployment options.
standardise_deploy_options( _Opts=[],
							{ ExDirs, ExSufs, KeepSufs, OtherOpts } ) ->
	% keep_only options are to be specified, thus processed, last:
	[ { exclude_directories, list_utils:uniquify( ExDirs ) },
	  { exclude_suffixes,    list_utils:uniquify( ExSufs ) },
	  { keep_only_suffixes,  list_utils:uniquify( KeepSufs ) } | OtherOpts ];

standardise_deploy_options( [ { exclude_directories, D } | T ],
							{ ExDirs, ExSufs, KeepSufs, OtherOpts } ) ->
	case text_utils:are_strings( D ) of

		true ->
			standardise_deploy_options( T,
				{ ExDirs ++ D, ExSufs, KeepSufs, OtherOpts } );

		false ->
			trace_utils:error_fmt( "The specified term to designate excluded "
				"directories is ~p, whereas a list of paths is expected.",
				[ D ] ),
			throw( { not_list_of_paths, exclude_directories, D } )

	end;

standardise_deploy_options( [ { exclude_suffixes, S } | T ],
							{ ExDirs, ExSufs, KeepSufs, OtherOpts } ) ->
	standardise_deploy_options( T, { ExDirs, ExSufs++S, KeepSufs, OtherOpts } );

standardise_deploy_options( [ { keep_only_suffixes, K } | T ],
							{ ExDirs, ExSufs, KeepSufs, OtherOpts } ) ->
	standardise_deploy_options( T, { ExDirs, ExSufs, KeepSufs++K, OtherOpts } );

standardise_deploy_options( [ OtherOpt | T ],
							{ ExDirs, ExSufs, KeepSufs, OtherOpts } ) ->
	standardise_deploy_options( T,
		   { ExDirs, ExSufs, KeepSufs, [ OtherOpt | OtherOpts ] } ).




% Determines what are the element paths that should be selected. The ones that
% are to be rebuilt are rebuilt.
%
% Returns the list of selected (and possibly rebuilt) files.
%
% We suppose we are already in the root directory, so that both absolute and
% relative paths can be managed the same. Next operations are not supposed then
% to change the current directory.
%
% (helper)
%
manage_rebuild_and_select( Additions, State ) ->
	% One-level flatten:
	list_utils:uniquify( lists:append(
				 [ process_element( Elem, State ) || Elem <- Additions ] ) ).




% Processing of the path elements.


% Encloses any standalone option into a list:
%
% (helper)



% To avoid "The pattern <AnyElement, State> can never match since previous
% clauses completely covered the type", should the user enter an incorrect
% element type:
%
-dialyzer( { no_match, process_element/2 }).



% ElementOptions is necessarily already a list:
%process_element( { ElementPath, ElementType, ElementOptions }, State ) when
%	  not is_list( ElementOptions ) ->
%	process_element( { ElementPath, ElementType, [ ElementOptions ] }, State );

% Now that we have necessarily a list, managing the type 'data':
process_element( { ElementPath, _ElementType=data, ElementOptions }, State ) ->

	%trace_utils:debug_fmt( "Processing data in ~s with options ~p.",
	%					   [ ElementPath, ElementOptions ] ),

	% Pure data, thus nothing to rebuild by default.

	% First action is to determine whether it exists, then whether it is a file
	% or a directory:

	check_element_path_exists( ElementPath, State ),

	MustRebuild = lists:member( rebuild, ElementOptions ),

	case file_utils:get_type_of( ElementPath ) of

		regular ->
			case MustRebuild of

				true ->
					rebuild_file( ElementPath, State );

				false ->
					ok

			end,
			[ ElementPath ];


		directory ->
			case MustRebuild of

				true ->
					rebuild_directory( ElementPath, State );

				false ->
					ok

			end,

			% Needing to recurse here, maybe excluded directories and/or
			% suffixes have been defined:
			%
			DataContent = select_content_from( ElementPath, ElementOptions ),

			%trace_utils:debug_fmt( "Data content: ~p.", [ DataContent ] ),

			DataContent;


		_Other ->
			% device, other, etc.:
			?error_fmt( "An additional data element path to deploy (~p) was "
				"specified, however it is neither a file nor a "
				"directory, aborting deployment.", [ ElementPath ] ),
			throw( { unexpected_type_for_deployed_data, ElementPath } )

	end;


% Manage the type 'code':
process_element( { ElementPath, _ElementType=code, ElementOptions }, State ) ->

	% First, rebuild if appropriate (note that the rebuild may involve excluded
	% directories; note also that 'rebuild' is the default, so we just look for
	% any 'no_rebuild' specification), then select:

	check_element_path_exists( ElementPath, State ),

	MustRebuild = not lists:member( no_rebuild, ElementOptions ),

	case file_utils:get_type_of( ElementPath ) of


		regular ->
			case MustRebuild of

				true ->
					rebuild_file( ElementPath, State );

				false ->
					ok

			end,
			[ ElementPath ];


		directory ->
			case MustRebuild of

				true ->
					rebuild_directory( ElementPath, State );

				false ->
					ok

			end,

			% Needing to recurse here, maybe excluded directories and/or
			% suffixes have been defined:
			%
			CodeContent = select_content_from( ElementPath, ElementOptions ),

			%trace_utils:debug_fmt( "CodeContent for directory '~s': ~s",
			%    [ ElementPath, text_utils:strings_to_string( CodeContent ) ] ),

			CodeContent;


		_Other ->
			% device, other, etc.:
			?error_fmt( "An additional code element path to deploy (~p) was "
				"specified, however it is neither a file nor a "
				"directory, aborting deployment.", [ ElementPath ] ),
			throw( { unexpected_type_for_deployed_code, ElementPath } )

	end;


process_element( AnyElement, State ) ->

	?error_fmt( "An incorrect additional element to deploy was specified: ~p, "
				"aborting deployment.", [ AnyElement ] ),

	throw( { incorrect_additional_element_specified, AnyElement } ).



% Just ensures there exists a filesystem element corresponding to that path:
%
% (helper)
%
check_element_path_exists( ElementPath, State ) ->

	%trace_utils:debug_fmt( "Checking element path '~p'.", [ ElementPath ] ),

	case file_utils:exists( ElementPath ) of

		true ->
			ok;

		false ->
			?error_fmt( "The element path '~s' was requested to be deployed, "
				"but it could not be found in the filesystem.",
				[ ElementPath ] ),

			throw( { deployment_failed, element_path_not_found, ElementPath } )

	end.



% Selects content from directory with options.
%
% (helper)
%
select_content_from( Directory, Options ) ->

	%trace_utils:debug_fmt( "Selecting content from directory '~s', "
	%					   "with options: ~p.", [ Directory, Options ] ),

	% Ensure that we are not including an Erlang/OTP build tree by mistake:
	%
	[ begin

		  case filename:basename( D ) of

			  SubDir="otp_src_" ++ _ ->
				  trace_utils:error_fmt(
					"While selecting content from directory '~s', apparently "
					"an Erlang/OTP build tree was found included ('~s'); "
					"please remove it first so that no Erlang internal BEAM "
					"file may be deployed by mistake.", [ D, SubDir ] ),
				  throw( { erlang_otp_build_tree_in_deployed_dir, Directory,
						   D } );

			  _ ->
				  ok

		  end

	  end
	  || D <- file_utils:find_directories_from( Directory ) ],


	% First, blacklists content:
	BlackFileList = case proplists:get_value( exclude_directories, Options ) of

		ExcludeOpt when ExcludeOpt =:= undefined orelse ExcludeOpt =:= [] ->

			%trace_utils:debug( "No excluded directory." ),

			case proplists:get_value( exclude_suffixes, Options ) of

				undefined ->
					% No dir or suffix exclusion here:
					file_utils:find_files_from( Directory );

				ExcludedSuffixes ->
					% Just suffix exclusion:
					file_utils:find_files_with_excluded_suffixes( Directory,
														ExcludedSuffixes )

			end;

		ExcludedDirectories ->

			%trace_utils:debug_fmt( "Excluded directories: ~p",
			%					   [ ExcludedDirectories ] ),

			% To avoid that faulty exclusions remain unnoticed:
			[ begin

				  AbsDir = file_utils:join( Directory, D ),

				  case file_utils:is_existing_directory_or_link( AbsDir ) of

					  true ->
						  ok;

					  false ->
						  trace_utils:error_fmt( "Excluded directory '~s' "
							"does not exist.", [ AbsDir ] ),

						  throw( { non_existing_excluded_directory, AbsDir } )
				  end

			  end || D <- ExcludedDirectories ],

			case proplists:get_value( exclude_suffixes, Options ) of

				undefined ->
					% Just dir exclusion:
					file_utils:find_files_with_excluded_dirs( Directory,
													ExcludedDirectories );

				ExcludedSuffixes ->
					% Both dir and suffix exclusion:
					file_utils:find_files_with_excluded_dirs_and_suffixes(
							Directory, ExcludedDirectories, ExcludedSuffixes )

			end

		end,

	% Then, whitelists:
	WhiteFileList = case proplists:get_value( keep_only_suffixes, Options ) of

		KeepOpt when KeepOpt =:= undefined orelse KeepOpt =:= [] ->
			BlackFileList;

		KeptOnlySuffixes ->
			file_utils:filter_by_included_suffixes( BlackFileList,
													KeptOnlySuffixes )

	end,

	%trace_utils:debug_fmt( "Files after black-listing:~n~p~n~n"
	%	"Files after white-listing:~n~p", [ BlackFileList, WhiteFileList ] ),

	% Finally: restores back the directory prefix:
	[ file_utils:join( Directory, F ) || F <- WhiteFileList ].



% Rebuilds the specified directory, supposedly known to be existing and to be a
% directory: runs 'make all' from it, throws an exception on failure.
%
% Does not change the current directory, supposed to be the root one.
%
% (helper)
%
rebuild_directory( Directory, State ) ->

	%trace_utils:debug_fmt( "Rebuilding all in directory '~s'.",
	%					   [ Directory ] ),

	CurrentDir = file_utils:get_current_directory(),

	file_utils:set_current_directory( Directory ),

	% Note that it seems that, at least in some cases, for currently unknown
	% reasons, a rebuild may be triggered whereas all BEAMs are available and
	% valid.

	% This may become a problem especially if the local Erlang environment (ex:
	% on a server) is different (typically older) than the build Erlang
	% environment (typically in a cutting-edge developer machine).
	%
	% One may deploy prebuilt packages and disable the automatic rebuild in the
	% deployment settings (see their 'rebuild_on_deployment_package_generation'
	% field).

	% We could as well keep all standard output; here only the error output is
	% kept:
	%
	% (now, with these current more conservative settings, we collect all
	% outputs to have them in the traces)

	%Command = ?getAttr(make_path) ++ " -s all 1>/dev/null",
	Command = ?getAttr(make_path) ++ " -s all",

	case system_utils:run_command( Command ) of

		%{ _ReturnCode=0, _CmdOutput=[] } ->
		%	?debug_fmt( "Directory ~s successfully rebuilt.", [ Directory ] );

		%{ _ReturnCode=0, CmdOutput } ->
		%	?warning_fmt( "Directory ~s successfully rebuilt, yet a message "
		%				  "was output: '~s'.", [ Directory, CmdOutput ] );

		{ _ReturnCode=0, CmdOutput } ->
			?info_fmt( "Directory '~s' successfully rebuilt, based on "
				"following command: '~s', from following directory: "
				"'~s' and with following output:~n~s",
				% To have a more precise directory:
				[ Directory, Command, file_utils:get_current_directory(),
				  CmdOutput ] );

		% Often code is 2:
		{ ReturnCode, CmdOutput } ->

			DisplayedMessage = text_utils:format(
				"Rebuild of directory '~s' failed (error code: ~B). "
				"Build error message was:~n~n\"\"\"~n~s~n\"\"\"",
				[ Directory, ReturnCode, CmdOutput ] ),
			?error( DisplayedMessage ),
			throw( { rebuild_failed, make_failed_for_directory, Directory } )

	end,

	file_utils:set_current_directory( CurrentDir ).



% Rebuilds the specified file FILE, supposedly known to be existing and to be a
% file: runs 'make FILE' from its parent directory, throws an exception on
% failure.
%
% Does not change the current directory, supposed to be the root one.
%
% (helper)
%
rebuild_file( Filename, State ) ->

	ParentDir = filename:dirname( Filename ),
	BaseFilename = filename:basename( Filename ),

	CurrentDir = file_utils:get_current_directory(),

	file_utils:set_current_directory( ParentDir ),

	Command = ?getAttr(make_path) ++ " " ++ BaseFilename ++ " 1>/dev/null",

	case system_utils:run_command( Command ) of

		{ _ReturnCode=0, _CmdOutput=[] } ->
			?debug_fmt( "File ~s successfully rebuilt.", [ Filename ] );

		{ _ReturnCode=0, CmdOutput } ->
			?warning_fmt( "File ~s successfully rebuilt, yet a message "
						  "was output: '~s'.", [ Filename, CmdOutput ] );

		% Often code is 2:
		{ ReturnCode, CmdOutput } ->
			DisplayedMessage = text_utils:format( "Rebuild of file ~s failed "
				"(error code: ~B). Build error message was:~n~n'~s'.",
				[ Filename, ReturnCode, CmdOutput ] ),
			?error( DisplayedMessage ),
			%trace_utils:debug_fmt( "~n~s", [ DisplayedMessage ] ),
			throw( { rebuild_failed, make_failed_for_file, Filename } )

	end,

	file_utils:set_current_directory( CurrentDir ).



% Waits for the database event (ex: onDatabaseStarted) to be reported by all
% specified processes (generally deployment agents). No time-out managed here.
%
% (helper)
%
wait_for_database_event( _Agents=[], _Event ) ->
	ok;

wait_for_database_event( Agents, Event ) ->

	%trace_utils:debug_fmt( "Waiting for database event '~p' from ~p.",
	%					   [ Event, Agents ] ),

	receive

		{ Event, AgentPid } ->
			%trace_utils:debug_fmt( "Received event '~p' from agent ~p.",
			%						[ Event, AgentPid ] ),
			wait_for_database_event( lists:delete( AgentPid, Agents ), Event )

	end.



% Reorders specified list of { Node@Host, HostString } pairs so that all pairs
% whose HostString matches LastHost are put at the end of the returned list
% (i.e. at the last position).
%
% (helper)
%
reorder_nodes( NodeList, LastHost ) ->
	reorder_nodes( NodeList, LastHost, _Acc=[] ).


reorder_nodes( _NodeList=[], _LastHost, Acc ) ->
	Acc;

reorder_nodes( _NodeList=[ E={ _Node, LastHost } | T ], LastHost, Acc ) ->
	% Put any LastHost-based pair at tail:
	reorder_nodes( T, LastHost, Acc ++ [ E ] );

reorder_nodes( _NodeList=[ E | T ], LastHost, Acc ) ->
	% Appends normally all other pairs at head:
	reorder_nodes( T, LastHost, [ E | Acc ] ).



% Returns the inter-node time-out, depending on the execution target: the number
% of seconds for the Erlang kernel tick time, so that Erlang nodes can monitor
% others.
%
-spec get_inter_node_tick_time_out() -> static_return( unit_utils:seconds() ).


-ifdef(exec_target_is_production).



% In production mode, we want to overcome situations where a few nodes might be
% especially unresponsive (yes, this happens).
%
% The default, 60s, is too small for some HPC clusters, we want to avoid:
% ** Node XXX not responding **
% ** Removing (timedout) connection **
%
% Note that all connected nodes must rely on the same duration.
%
get_inter_node_tick_time_out() ->

	Minutes = 30,

	%trace_utils:debug_fmt( "(in production mode, thus ~w on node '~s' will "
	%   "be using the extended inter-node time-out: ~B minutes)",
	%	[ self(), node(), Minutes ] ),

	trace_utils:debug_fmt( "(in production mode, thus using the extended "
						   "inter-node time-out: ~B minutes)", [ Minutes ] ),

	wooper:return_static( Minutes * 60 ).


-else. % exec_target_is_production


% In development mode, we can rely on the default value:
get_inter_node_tick_time_out() ->

	Minutes = 5,

	% Not wanted in development mode:
	%trace_utils:debug_fmt( "(in development mode, thus ~w on node ~s will "
	%    "be using default inter-node time-out: ~B minutes)",
	%	 [ self(), node(), Minutes ] ),

	wooper:return_static( Minutes * 60 ).


-endif. % exec_target_is_production



% Returns the default filename of the deployment package archive.
%
% (helper)
%
get_default_deployment_package_name() ->
	"Sim-Diasca-deployment-archive.sdar".




% Returns the name of the result directory that should be used.
-spec get_result_directory_name( simulation_name(), time_utils:timestamp(),
		sim_diasca:sii() ) -> static_return( directory_name() ).
get_result_directory_name( SimulationName, StartTimestamp, SII ) ->

	% We add the SII (formerly was a part of the cookie that is an UUID) to
	% ensure uniqueness even in the case of a series of simulations launched by
	% a script at the same second:
	%
	wooper:return_static( text_utils:format( "~s-on-~s-by-~s-~s", [
		file_utils:convert_to_filename( SimulationName ),
		time_utils:get_textual_timestamp_for_path( StartTimestamp ),
		file_utils:convert_to_filename( system_utils:get_user_name() ),
		SII ] ) ).



% Returns a textual description of specified service placement record.
%
% (helper)
%
-spec service_placement_to_string( service_placement() ) -> ustring().
service_placement_to_string( #service_placement{
	load_balancing=LoadBalancing,
	time_management=TimeManagement,
	data_logging=DataLogging,
	data_exchanging=DataExchanging,
	result_management=ResultManagement,
	instance_tracking=InstanceTracking,
	performance_tracking=PerformanceTracking,
	resilience_management=ResilienceManagement,
	bindings_management=BindingsManagement } ) ->

	"Simulation services dispatched as: " ++ text_utils:strings_to_string(
		[ placement_description( Name, NodeInfo ) || { Name, NodeInfo } <-
			[ { "load-balancing", LoadBalancing },
			  { "time management", TimeManagement },
			  { "data-logging", DataLogging },
			  { "data-exchanging", DataExchanging },
			  { "result management", ResultManagement },
			  { "instance tracking", InstanceTracking },
			  { "performance tracking", PerformanceTracking },
			  { "resilience management", ResilienceManagement },
			  { "bindings management", BindingsManagement } ] ] ).



% Helper:
placement_description( ServiceName, _Nodes={ RootNode, LocalNodes } ) ->
	text_utils:format( "~s: root on ~s, ~B agents on ~p",
		[ ServiceName, RootNode, length( LocalNodes ), LocalNodes ] );

placement_description( ServiceName, SingletonNode ) ->
	text_utils:format( "~s: ~s", [ ServiceName, SingletonNode ] ).


% Halts on error, rather than throwing an uncaught exception that will trigger
% the display of a stacktrace and all.
%
% Here we just want to (cleanly) halt.
%
% (helper)
%
-spec halt_on_error( ustring() ) -> no_return().
halt_on_error( Message ) ->
	halt_on_error( Message, _ErrorCode=1 ).


halt_on_error( Message, ErrorCode ) when ErrorCode > 0 ->

	basic_utils:display( "~nHalting on error (code: ~B): ~s",
						 [ ErrorCode, Message ] ),

	basic_utils:stop( ErrorCode ).
