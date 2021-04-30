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


-module(class_ComputingHostManager).


-define( class_description,
		 "Manager of a computing host, notably for deployment purpose. "
		 "All computing host managers run on the user node. "
		 "Such a manager will in turn, for the computing host it is in "
		 "charge of:"
		 " - set-up the host: check liveliness (with ping), perform a "
		 "node-cleanup if requested, then launch a dedicated Erlang node"
		 " - deploy the simulation on that host: send the pioneer modules, "
		 "run the deployment agent, provide the resources it requests, ensure "
		 "it reports that its deployment is ready"
		 " - report to the deployment manager that this host is finally ready "
		 "to take part to the simulation" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).


% The class-specific attributes of a computing host manager:
-define( class_attributes, [

		{ managed_host, union( 'localhost', string_host_name() ),
		  "designates the host that is managed by this instance" },

		{ user_name, basic_utils:user_name(), "the name of the "
		  "(operating-system) user with which we should connect to that host" },

		{ node_name, string_node_name(),
		  "the managed, short node name (ex: 'my_node'), i.e. it is not "
		  "fully-qualified" },

		{ full_node_name, atom_node_name(),
		  "the fully-qualified node name (ex: 'my_node@foo.org') to be used "
		  "by other nodes in order to target the corresponding node" },

		{ node_naming_mode, net_utils:node_naming_mode(), "the node naming "
		  "mode in use (i.e. short or long names)" },

		{ node_cleanup, union( 'false', file_utils:bin_script_path() ),
		  "tells whether an initial clean-up of any previously existing node "
		  "with that name is wanted (either false or the full path of the "
		  "clean-up script to be used, as a binary)" },

		{ node_cookie, net_utils:cookie(),
		  "the cookie that must be used to launch any new node" },

		{ scheduler_count, basic_utils:count(),
		  "the number of schedulers to be used to launch any new node" },

		{ epmd_port, maybe( tcp_port() ),
		  "the EPMD (TCP) port number (if any)" },

		{ tcp_port_range, net_utils:tcp_port_restriction(), "Any TCP port "
		  "restriction to respect, either the 'no_restriction' atom or a pair "
		  "of integers { MinTCPPort, MaxTCPPort }" },

		{ simulation_instance_id, sim_diasca:sii(),
		  "the identifier of the current simulation" },

		{ ping_allowed, boolean(), "tells whether ping (ICMP) messages may be "
		  "used to test host availability" },

		{ deployment_manager_pid, deployment_manager_pid(),
		  "the PID of the (parent) deployment manager" },

		{ deployment_agent_pid, maybe( deployment_agent_pid() ),
		  "the PID of the associated deployment agent" },

		{ deployment_agent_monitor_ref, maybe( reference() ),
		  "the monitoring reference (if any) towards the associated deployment "
		   "agent" },

		{ deployed_node, maybe( atom_node_name() ),
		  "the name of the deployed node, as an atom" },

		{ deploy_time_out, time_out(), "time-out, in milliseconds, "
		  "for this host to be fully deployed" },

		{ deploy_base_dir, bin_directory_path(), "the deployment base "
		  "directory for all computing nodes (as a binary)" },

		{ additional_beam_bin_dirs, [ bin_directory_path() ],
		  "a list of additional BEAM directories (as binaries), for the "
		  "deployment agent" },

		{ start_time, time_utils:get_timestamp(), "start time, so that we can "
		  "give up if a blocking operation made us wait past the deployment "
		  "time-out" },

		{ tick_time_out, time_out(), "inter-tick time-out, in "
		  "milliseconds, to configure the deployed node (thanks to the "
		  "deployment agent)" } ] ).



% The various reasons why a VM launch may fail:
-type host_failure_reason() :: 'host_not_available'
							 | 'deployment_time_out'
							 | 'vm_detection_abnormal'
							 | 'vm_detection_none'
							 | 'launched_vm_not_found'
							 | 'multiple_vms_detected'
							 | 'vm_detection_failed'
							 | 'vm_remote_detection_abnormal'
							 | 'remote_launched_vm_not_found'
							 | 'one_remote_vm_detected'
							 | 'multiple_remote_vms_detected'
							 | 'vm_remote_detection_failed'.


-type host_manager_pid() :: sim_diasca:agent_pid().

-export_type([ host_failure_reason/0, host_manager_pid/0 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Deployment.ComputingHostManager" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% For deployment_manager_pid() and all:
-include("engine_common_defines.hrl").



% Shorthands:

-type count() :: basic_utils:count().
-type user_name() :: basic_utils:user_name().
-type exit_reason() :: basic_utils:exit_reason().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type milliseconds() :: unit_utils:milliseconds().

-type bin_file_name() :: file_utils:bin_file_name().
-type bin_file_path() :: file_utils:bin_file_path().
-type bin_directory_path() :: file_utils:bin_directory_path().


-type time_out() :: time_utils:time_out().

-type atom_node_name() :: net_utils:atom_node_name().
-type string_node_name() :: net_utils:string_node_name().
-type string_host_name() :: net_utils:string_host_name().
-type tcp_port() :: net_utils:tcp_port().



% Implementation notes.


% If connecting from X@a to Y@b, the connection may fail or wait for a user
% input (ex: modal window popped through SSH ask pass, if password-less
% authentication failed) and thus stay stuck in system_utils:run_command/1,
% indefinitively or long enough to be rejected (no way of stopping this command
% by message or time-out).

% As a series of potentially lenghty blocking operations are performed here, we
% try not to communicate with the deployment manager once we are already past
% the deployment time-out, as we do not want to interact with it whereas there
% is a 'delete' message already waiting for this manager to be read: the
% deployment manager would then not expect to receive such messages, having gone
% through the next steps.

% To start a remote node (via SSH), instead of using system_utils:run_command/1,
% a slave node could be used, see:
% support.process-one.net/doc/display/ERL/Starting+a+set+of+Erlang+cluster+nodes
%
% However using system_utils:run_command/1 seems to work great, and relying on a
% slave node seems to imply some consequences that may not be wanted, as
% discussed in: http://www.erlang.org/doc/man/slave.html. For example:
%
% - all TTY output produced at the slave will be sent back to the master node
%
% - file I/O is done via the master
%
% - the slave node should use the same file system at the master; at least,
% Erlang/OTP should be installed in the same place on both computers and the
% same version of Erlang should be used


% A problem with code_loader may happen due to the boot sequence of an Erlang
% node. Basically, net_adm:ping may reply positively *before* the code server
% can accept requests.





% Constructs a new manager for a given computing host, from following
% parameters:
%
% - HostnameOptions={Hostname, Username}, Hostname being either the name (as a
% plain string) of the remote computing host to manage, or the 'localhost' atom,
% Username being the name of the user to rely on for that host
%
% - NodeOptions={NodeBaseName, NodeNamingNode, NodeCleanupWanted, NodeCookie,
% NodeSchedulerCount}, a tuple made of:
%
%  - NodeName, the base node name (a plain string, ex: "my_node"), without any
%   host name
%
%   - NodeNamingNode, a node naming mode (i.e. short or long names)
%
%   - NodeCleanupWanted, which tells whether an initial clean-up of any
%   previously existing node with that name is wanted: it is either false, or
%   the full path of the clean-up script to be used, as a binary (note: this
%   path must have been already validated once for all by the caller, it is
%   considered here as reliable)
%
%   - NodeCookie, the cookie that must be used to launch any new node
%
%   - NodeSchedulerCount :: maybe( count() ) the number of
%   schedulers to create on the associated node
%
% - NetworkOptions={EpmdPort, TCPPortRestriction}, a pair made of:
%
%   - EpmdPort is the EPMD port specification, with can be either the
%   'undefined' atom or the port number; note that if a non-default EPMD port is
%   specified for a new node, this implies that the current node usually has to
%   itself respect the same non-standard convention (ex: see the FIREWALL_OPT
%   make option in myriad/GNUmakevars.inc), otherwise available nodes will not
%   be found
%
%   - TCPPortRestriction is the TCP port restriction, with can be either the
%   'no_restriction' atom or a pair of integers {MinTCPPort,MaxTCPPort}; note
%   that if using a specific TCP/IP port range for a new node, the current node
%   may have to respect this constraint as well (see the FIREWALL_OPT make
%   option in myriad/GNUmakevars.inc), otherwise inter-node communication could
%   fail
%
% - DeployOptions is a {DeploymentManagerPid, DeployTimeOut,
% InterNodeTickTimeOut, AdditionalBEAMDirs} tuple, where:
%
%   - DeploymentManagerPid: the PID of the deployment manager, which created
%   this manager, in order to be able to interact with it later
%
%   - DeployTimeOut is the maximum number of milliseconds which will be left to
%   this host to be deployed
%
%   - InterNodeTickTimeOut is the time-out for inter-node ticks, to be
%   transmitted to the deployment agent later
%
%   - BinDeployBaseDir is the deployment base directory (as a binary)
%
%   - AdditionalBEAMBinDirs is a list of directories (as binaries) containing
%   BEAM files that shall be added to the code path of each computing node
%
%   - SII is the identifier of the current simulation
%
-spec construct( wooper:state(),

		{ 'localhost' | string_host_name(), user_name() },

		{ atom_node_name(), net_utils:node_naming_mode(),
		  'false' | bin_file_path(), net_utils:cookie(), maybe( count() ) },

		{ maybe( tcp_port() ), 'no_restriction' | net_utils:tcp_port_range() },

		{ deployment_manager_pid(), milliseconds(),
		  milliseconds(), bin_string(),
		  [ bin_directory_path() ], sim_diasca:sii() } )

				-> wooper:state().
construct( State,

		   _HostnameOptions={ Hostname, Username },

		   _NodeOptions={ NodeName, NodeNamingNode, NodeCleanupWanted,
						  NodeCookie, NodeSchedulerCount },

		   _NetworkOptions={ EpmdPort, TCPPortRestriction, PingAllowed },

		   _DeployOptions={ DeploymentManagerPid, DeployTimeOut,
							InterNodeTickTimeOut, BinDeployBaseDir,
							AdditionalBEAMBinDirs, SII } ) ->

	{ MessageHostname, ActualHostname } = case Hostname of

		localhost ->
			% We have had our deal of problems regarding systems whose local
			% hostnames resolved in varied, sometimes variable, potentially
			% unresolvable values (see Sim-Diasca issues #3 and #4 for more
			% information).
			%
			% So:
			%LocalHostname = net_utils:localhost(),
			% We reuse the actual host of this local node:
			%
			[ _ThisNodeName, LocalHostname ] = text_utils:split(
				text_utils:atom_to_string( node() ), [ $@ ] ),

			{ "the user host", LocalHostname };

		_ ->
			{ Hostname, Hostname }

	end,

	%trace_utils:debug_fmt( "Hostname as specified by the deployment manager: "
	%					   "'~s', resulting in an actual hostname of '~s'.",
	%					   [ Hostname, ActualHostname ] ),

	% First the direct mother classes:
	%
	% (we replace dots by semi-colons, otherwise LogMX would create branches in
	% the message tree)
	%
	NonDottedMessageHostname = re:replace( _Subject=MessageHostname,
		_RegExp="\\.", _Replacement=":", _Opts=[ {return,list}, global ] ),

	EmitterName = "Host manager for " ++ NonDottedMessageHostname,

	TraceState = class_EngineBaseObject:construct( State,
								?trace_categorize(EmitterName) ),

	% As it may explain many connection issues (we waited to be able to send
	% traces):
	%
	case Hostname of

		localhost ->

			case net_utils:localhost() of

				ActualHostname ->
					ok;

				OtherHostname ->
					?send_warning_fmt( TraceState,
						"Apparently the local hostname can resolve in different"
						" versions: '~s' as determined internally, and '~s' as "
						"deduced from the node name. Such inconsistencies may "
						"prevent the node interconnection.",
						[ OtherHostname, ActualHostname ] )

			end;

		_ ->
			ok

	end,

	FullyQualifiedNodeName = net_utils:get_fully_qualified_node_name( NodeName,
									ActualHostname, NodeNamingNode ),

	% EPMD possibly undefined:
	?send_debug_fmt( TraceState, "Creating a computing host manager for "
		"host '~s' (user: '~s', ~s node: '~s', cookie: '~s', EPMD port: ~w, "
		"TCP port restriction: ~w, SII: ~s), "
		"resulting in following full node name: '~s'.",
		[ Hostname, Username, NodeNamingNode, NodeName, NodeCookie, EpmdPort,
		  TCPPortRestriction, SII, FullyQualifiedNodeName ] ),

	StartingState = setAttributes( TraceState, [

		% Either the name (as a plain string) of the remote computing host to
		% manage, or the 'localhost' atom:
		%
		{ managed_host, Hostname },

		% The name of the user the with which we should connect to that node:
		{ user_name, Username },

		% Node name (a plain string); this is just the node name, ex: "my_node",
		% i.e. it is not fully-qualified.
		%
		{ node_name, NodeName },

		% Fully-qualified node name (stored as an atom), ex: 'my_node@foo.org',
		% to be used by other nodes, to target the corresponding node.
		%
		{ full_node_name, FullyQualifiedNodeName },

		% Node naming mode (i.e. short or long names):
		{ node_naming_mode, NodeNamingNode },

		% Tells whether an initial clean-up of any previously existing node with
		% that name is wanted (either false or the full path of the clean-up
		% script to be used, as a binary):
		%
		{ node_cleanup, NodeCleanupWanted },

		% The cookie that must be used to launch any new node:
		{ node_cookie, NodeCookie },

		% The number of schedulers to be used to launch any new node:
		{ scheduler_count, NodeSchedulerCount },

		% The EPMD port specification, either the 'undefined' atom or the port
		% number:
		%
		{ epmd_port, EpmdPort },

		% The TCP port restriction, either the 'no_restriction' atom or a pair
		% of integers { MinTCPPort, MaxTCPPort }
		%
		{ tcp_port_range, TCPPortRestriction },

		{ simulation_instance_id, SII },

		% Tells whether ping (ICMP) messages may be used to test host
		% availability:
		%
		{ ping_allowed, PingAllowed },

		% The PID of the deployment manager:
		{ deployment_manager_pid, DeploymentManagerPid },

		% The PID of the deployment agent:
		{ deployment_agent_pid, undefined },

		% The monitor reference of the deployment agent (if any):
		{ deployment_agent_monitor_ref, undefined },

		% The name of the deployed node, as an atom:
		{ deployed_node, undefined },

		% Time-out, in milliseconds, for this host to be fully deployed:
		{ deploy_time_out, DeployTimeOut },

		% The deployment base directory for all computing nodes (as a binary):
		{ deploy_base_dir, BinDeployBaseDir },

		{ additional_beam_bin_dirs, AdditionalBEAMBinDirs },

		% Start time, so that we can give up if a blocking operation made us
		% wait past the deployment time-out:
		%
		{ start_time, time_utils:get_timestamp() },

		% Time-out, in milliseconds, to configure the deployed node:
		{ tick_time_out, InterNodeTickTimeOut } ] ),


	?send_info_fmt( StartingState,
		"Created a new manager for computing host '~s', "
		"with node name '~s'.", [ Hostname, NodeName ] ),

	% Direct asynchronous auto-activation after this constructor:
	self() ! setUpHost,

	StartingState.




% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%trace_utils:debug_fmt( "Deleting computing host manager ~w.",
	%                       [ self() ] ),

	% Class-specific actions:
	?info( "Deleting computing-host manager." ),

	case ?getAttr(deployment_agent_monitor_ref) of

		undefined ->
			ok;

		MonitorRef ->
			% Probably optional:
			erlang:demonitor( MonitorRef )

	end,

	% Halts corresponding node (safe remote shutdown):
	case ?getAttr(deployment_agent_pid) of

		undefined ->
			ok;

		AgentPid ->
			%trace_utils:debug_fmt( "Requesting deployment agent ~p to "
			%                       "terminate.", [ Pid ] ),
			%timer:sleep( 1000 ),

			AgentPid ! terminate

	end,

	% Useless:
	%?getAttr(deployment_manager_pid) ! { notifyHostRemoval, self() },

	% Previous solution to halt corresponding node, used to trigger
	% 'noconnection' errors:

	%TargetNode = ?getAttr(full_node_name),

	%try

		%net_utils:shutdown_node( TargetNode )

	%catch

	%	E ->
	%		trace_utils:debug_fmt( "Exception caught when shutting down node "
	%                              "'~s': ~p.", [ TargetNode, E ] )

	%end,

	?debug( "Computing-host manager deleted." ),

	% "Then" allow chaining:
	State.





% Methods section.



% Performs an initial set-up of the managed host, to prepare for deployment.
%
% (oneway, as it is a long-running task)
%
-spec setUpHost( wooper:state() ) -> oneway_return().
setUpHost( State ) ->

	ManagedHostname = ?getAttr(managed_host),

	NewState = case check_availability( ManagedHostname, State ) of

		true ->
			case is_already_too_late( State ) of

				false ->
					connect_to_host( ManagedHostname, State );

				TimeOutString ->
					?debug_fmt( "No connection set-up attempted, "
						"as already too late (~s).", [ TimeOutString ] ),
					State

			end;

		false ->
			declare_deployment_failure( host_not_available, State )

	end,

	wooper:return_state( NewState ).



% Requests the simulation package to be sent to the caller, expected to be the
% deployment agent.
%
% DeployedNode is the node on which the calling agent runs, specified as an
% atom.
%
-spec requestPackage( wooper:state(), atom_node_name() ) ->
		request_return( 'deploy_time_out' | bin_file_name() ).
requestPackage( State, DeployedNode ) ->

	% This call is also a way of discovering the PID of the remote agent that
	% was created thanks to a rpc:cast.

	% We want to know if ever a deployment agent crashed, however, should a
	% computing host manager crash, we do not want its associated deployment
	% agent to crash in turn (otherwise it would not be able to perform its
	% teardown operations, such as halting its node).
	%
	% So this manager *monitors* its agent, instead of linking to it (one-way
	% instead of two-way dependency)
	%
	% (there is a race condition though, as the agent *may* crash before the
	% monitor is made, but apparently we cannot do better with rpc operations)
	%
	AgentPid = ?getSender(),

	% Anyway now we trap exits (does not seem to change much, the agent may
	% still crash on throw yet its host manager is not aware of it):
	%
	erlang:process_flag( trap_exit, true ),

	erlang:link( AgentPid ),

	% Not exactly a one-way link; results in receiving a message like: {'DOWN',
	% MonitorRef, Type, Object, Info}, itself intercepted by WOOPER, triggering
	% here the onWOOPERDownNotified/5 method of this manager:
	%
	AgentMonitorRef = erlang:monitor( process, AgentPid ),

	% Checkings:
	undefined = ?getAttr(deployment_agent_pid),
	undefined = ?getAttr(deployed_node),

	case is_already_too_late( State ) of

		false ->

			?info_fmt( "Node ~p, corresponding to deployment agent ~p, "
				"requested the package, which is being sent to it.",
				[ DeployedNode, AgentPid ] ),

			% This request is necessary to ensure that we wait for the package
			% to be built by the deployment manager:
			%
			?getAttr(deployment_manager_pid) ! { getPackage, [], self() },

			?info( "Waiting for the package to be sent locally by "
					"the deployment manager." ),

			% Luckily sent big binaries are not copied when in the same node:
			{ PackageFilename, PackageFilenameBin } = receive

				{ wooper_result, FilenameBin } when is_binary( FilenameBin ) ->
					{ text_utils:binary_to_string( FilenameBin ), FilenameBin }

			end,

			% We must tell the agent that it is not already too late:
			AgentPid ! { wooper_result, send_starting },

			?info_fmt( "Sending the package '~s' through sendfile.",
					   [ PackageFilename ] ),

			% Now we send the package through sendfile (rather than using a too
			% large Erlang message for that); the deployment agent already
			% triggered its receive_file call:
			%
			net_utils:send_file( PackageFilename, AgentPid ),

			?info( "Package sent." ),

			% No reference kept on the package:
			wooper:return_state_result( setAttributes( State, [
					{ deployment_agent_pid, AgentPid },
					{ deployment_agent_monitor_ref, AgentMonitorRef },
					{ deployed_node, DeployedNode } ] ),
										PackageFilenameBin );


		TimeOutString ->

			?info_fmt( "(already too late even to request "
					   "the deployment package: ~s)", [ TimeOutString ] ),

			wooper:const_return_result( deploy_time_out )

	end.



% Notifies this manager that the deployment agent finished its deployment.
-spec onDeploymentReady( wooper:state(), system_utils:host_static_info() ) ->
							const_oneway_return().
onDeploymentReady( State, HostInfo ) ->

	% Declare success if on time:
	case is_already_too_late( State ) of

		false ->
			?getAttr(deployment_manager_pid) !
				{ onHostDeploymentSuccess, [ self(), HostInfo ] };

		TimeOutString ->
			?info_fmt( "(already too late even to report that the "
					   "deployment succeeded: ~s)", [ TimeOutString ] ),
			ok

	end,

	wooper:const_return().



% Starts a database agent, on the corresponding computing host, generally on
% behalf of the deployment manager.
%
-spec startDatabase( wooper:state(), pid() ) -> const_oneway_return().
startDatabase( State, CallerPid ) ->

	DeployAgentPid = ?getAttr(deployment_agent_pid),

	DeployAgentPid ! { start_database, self() },

	receive

		onDatabaseStarted ->
			CallerPid ! { onDatabaseStarted, self() }

	end,

	wooper:const_return().



% Stops a database agent, on the corresponding computing host, generally on
% behalf of the deployment manager.
%
-spec stopDatabase( wooper:state(), pid() ) -> const_oneway_return().
stopDatabase( State, CallerPid ) ->

	DeployAgentPid = ?getAttr(deployment_agent_pid),

	%trace_utils:debug_fmt( "Stopping database, requesting agent ~w.",
	%						[ DeployAgentPid ] ),

	DeployAgentPid ! { stop_database, self() },

	receive

		onDatabaseStopped ->
			%trace_utils:debug_fmt( "Database stopped by ~w.",
			%						[ DeployAgentPid ] ),
			CallerPid ! { onDatabaseStopped, self() }

	end,

	wooper:const_return().



% Terminates the computing node managed, typically on error-related teardowns in
% order to avoid that the corresponding UNIX processes remain as zombis.
%
-spec terminateComputingNode( wooper:state() ) -> const_oneway_return().
terminateComputingNode( State ) ->

	DeployAgentPid = ?getAttr(deployment_agent_pid),

	%trace_utils:debug_fmt( "Requesting deployment agent ~p to terminate "
	%						"its node now.", [ DeployAgentPid ] ),

	DeployAgentPid ! terminate,

	wooper:const_return().



% Section for static methods.


% Returns an upper bound to the duration, in milliseconds, of a host-level
% deployment.
%
-spec get_host_deployment_duration_upper_bound() ->
										static_return( milliseconds() ).
get_host_deployment_duration_upper_bound() ->

	% If an error was returned, chances are that the host will never answer, no
	% waiting is less useful:
	%
	LaunchDuration = max( get_time_out_for( launch_success ),
						  get_time_out_for( launch_error ) ),

	% We must take into account the duration of the operations that are needed
	% beyond node-level deployment; and another margin will be added by the
	% deployment manager later:
	%
	wooper:return_static( LaunchDuration + get_other_operations_duration() ).



% Returns the estimated maximum duration, in milliseconds, of all operations
% beyond node setup, for a host deployment.
%
-spec get_other_operations_duration() ->
							 static_return( milliseconds() ).
get_other_operations_duration() ->
	wooper:return_static( 1000 ).




% Section for helper functions.


% Tells whether, at this point in time, this manager is already too late to
% respect its deployment time-out. If yes, the deployment manager already
% ignored it and possibly went through next steps, and is not listening anymore.
%
% (helper function)
%
-spec is_already_too_late( wooper:state() ) -> 'false' | ustring().
is_already_too_late( State ) ->

	Now = time_utils:get_timestamp(),

	% Milliseconds:
	TimeOut = ?getAttr(deploy_time_out),

	% get_duration returns seconds:
	case 1000 * time_utils:get_duration( ?getAttr(start_time), Now ) of

		D when D > TimeOut ->
			io_lib:format( "already waited for ~s, "
				"longer than time-out of ~s", [
				time_utils:duration_to_string( D ),
				time_utils:duration_to_string( TimeOut ) ] );

		_ ->
			false

	end.




% Connects to specified host, performs any required clean-up, and launch a
% corresponding node.
%
% (helper function)
%
-spec connect_to_host( string_host_name(), wooper:state() ) -> wooper:state().
connect_to_host( Hostname, State ) ->

	FullyQualifiedNodeName = ?getAttr(full_node_name),

	?debug_fmt( "Starting the creation of node '~s' on host '~s'.",
				[ FullyQualifiedNodeName, Hostname ] ),

	case manage_node_cleanup( State ) of

		false ->
			?warning_fmt( "Deployment for host '~s' already too long after "
				"clean-up, thus this host has already been "
				"considered as unavailable.", [  Hostname ] ),
			State;

		true ->

			% Let's continue then:

			case launch_erlang_node( State ) of

				success ->

					?info_fmt( "Deployment succeeded for host '~s', notifying "
							   "the deployment manager.", [ Hostname ] ),

					% Another thing that can be done in parallel; returns an
					% udpated state:
					%
					case is_already_too_late( State ) of

						false ->
							send_deployment_agent( State );


						TimeOutString ->

							?warning_fmt( "Deployment succeeded for host '~s', "
								"however it took too long compared to the "
								"deployment time-out (~s), and thus this host "
								"has already been considered as unavailable.",
								[  Hostname, TimeOutString ] ),

							State

					end;


				{ failure, Reason } ->
					declare_deployment_failure( Reason, State )


			end

	end.



% Declares to the deployment manager that on this host the set-up failed, then
% triggers the deletion of this manager.
%
% Returns an updated state.
%
declare_deployment_failure( Reason, State ) ->

	ManagedHostname = ?getAttr(managed_host),
	LocalUsername = ?getAttr(user_name),

	ReasonString = class_DeploymentManager:interpret_host_failure( Reason ),

	case is_already_too_late( State ) of

		false ->

			?warning_fmt( "Deployment failed for host '~s' with user '~s' "
				"(reason: ~s) and anyway it had already taken too long "
				"compared to the deployment time-out; notifying the deployment "
				"manager and terminating.",
				[ ManagedHostname, LocalUsername, ReasonString ] ),

			?getAttr(deployment_manager_pid) !
				{ onHostDeploymentFailure, [ self(), Reason ] };


		TimeOutString ->

			?warning_fmt( "Deployment failed for host '~s' with user '~s' "
				"(reason: ~s; ~s), notifying the deployment manager and "
				"terminating.", [ ManagedHostname, LocalUsername, ReasonString,
								  TimeOutString ] ),
			ok

	end,

	self() ! delete,

	State.



% Checks that the specified host is available and that no previous node is on
% the way.
%
% Returns whether the specified host is valid.
%
% (helper function)
%
check_availability( Hostname, State ) ->

	case check_host_availability( Hostname, State ) of

		true ->
			ensure_no_lingering_node( ?getAttr(full_node_name),
									  ?getAttr(node_name), Hostname, State ),

			true;

		false ->
			false

	end.



% Returns whether specified host seems to be reachable from the network.
%
% Checks with a ping that the specified host is available.
%
% (helper function)
%
check_host_availability( localhost, _State ) ->
	true;

check_host_availability( Hostname, State ) ->

	case ?getAttr(ping_allowed) of

		true ->
			?debug_fmt( "Will ping now host '~s'. Depending on the DNS "
				"settings, if the host is not available, the operation "
				"may last for some time.", [ Hostname ] ),

			% Note: pinging a non-existing host may block this process for a few
			% seconds.
			case net_utils:ping( Hostname ) of

				true ->
					?debug_fmt( "Host '~s' is available (ping success).",
								[ Hostname ] ),
					true;

				false ->
					?warning_fmt( "Host '~s' not available, no node checked "
								  "nor launched.", [ Hostname ] ),
					false

			end;

		false ->
			?debug_fmt( "The use of ping is disallowed, so supposing that "
						"host '~s' is available.", [ Hostname ] ),
			true

	end.



% Ensures that no lingering node with specified name exists on the target host.
%
% The usefulness of this function is quite hypothetical now, as cookies should
% not match on purpose (new UUID already used here), and anyway a node cleaner
% script might be run afterwards.
%
% (helper function)
%
ensure_no_lingering_node( FullyQualifiedNodeName, NodeName, Hostname, State ) ->

	% 'Immediate', as it is not being launched here:
	%
	% (however this operation is long - typically 8-10 seconds on some contexts,
	% probably especially if the node is actually not available, which is by far
	% the most common case, so this checking alone might be responsible for a
	% time-out failure)
	%
	case net_utils:check_node_availability( FullyQualifiedNodeName,
											immediate ) of

		{ true, _Duration } ->
			?warning_fmt( "Node ~s on host ~s was already available, stopping"
				" (and, later relaunching) it to ensure it runs the "
				"correct code version.", [ NodeName, Hostname ] ),

			% Preferred to unloading-purging/reloading our modules:
			% (this is a blocking operation)
			%
			net_utils:shutdown_node( FullyQualifiedNodeName );

		{ false, _Duration } ->
			?debug_fmt( "Node ~s on host ~s is not available, it will be "
						"launched from scratch.", [ NodeName, Hostname ] )

	end.




% Performs a node-cleanup, if requested to do so.
%
% As we use at each simulation run, on purpose, unique (generated) cookies to
% avoid any possibility of connecting by mistake to previously running instances
% of the same simulation case, we are not able to connect to such a pre-existing
% node to shutdown it.
%
% As a consequence it could remain on the way and prevent its host to take part
% to the simulation (until the node performs its automatic shutdown on idle
% time-out, which had to be set-up to a high value in order to support any
% possible cluster slow-down).
%
% Therefore the cookie system ensures no connection mismatch can ever happen,
% and the cleaner script allows to avoid at all that any such nodes gets ever in
% the way: a (normally successful) attempt to destroy them preventively can be
% performed.
%
% Returns whether the deployment shall continue afterwards.
%
-spec manage_node_cleanup( wooper:state() ) -> boolean().
manage_node_cleanup( State ) ->

	case ?getAttr(node_cleanup) of

		false ->
			% Ready to continue directly:
			true;

		ScriptFullPathAsBin ->

			ScriptFullPath = text_utils:binary_to_string( ScriptFullPathAsBin ),

			ManagedHost = ?getAttr(managed_host),

			CleanCommand = case ManagedHost of

				localhost ->
					get_clean_up_command_for_localhost( ScriptFullPath, State );

				Hostname ->
					get_clean_up_command_for_host( Hostname, ScriptFullPath,
												   State )

			end,

			?debug_fmt( "Cleaning up was requested, with clean-up script '~s', "
				"resulting in full command '~s'.~n",
				[ ScriptFullPath, CleanCommand ] ),

			%trace_utils:debug_fmt( "Clean-up command:~n~s",
			%						[ CleanCommand ] ),

			% We noticed that the full-blown SSH command was actually *not*
			% executed if using system_utils:run_command/1 (i.e. an Erlang
			% port); hence for remote nodes we use
			% system_utils:evaluate_shell_expression/1 (that is indeed
			% executed):
			%
			case ManagedHost of

				localhost ->
					case system_utils:run_command( CleanCommand ) of

						% Our script is designed to output messages, and the
						% sole guide is to rely on its return code:
						{ _ReturnCode=0, CmdOutput } ->
							?notice_fmt( "Local clean-up command succeeded and "
								"resulted in following output: '~s'.",
								[ CmdOutput ] );

						{ ReturnCode, CmdOutput } ->
							?error_fmt( "Local clean-up command failed (error "
							  "code: ~B) and returned following output: '~s'.",
							  [ ReturnCode, CmdOutput ] )

					end;

				_Hostname ->
					CmdOutput = system_utils:evaluate_shell_expression(
								  CleanCommand ),
					?notice_fmt( "Remote clean-up command resulted in "
								 "following output: '~s'.", [ CmdOutput ] )

			end,

			% Tells whether we shall continue afterwards:
			case is_already_too_late( State ) of

				false ->
					true;

				_TimeOutString ->
					false

			end

	end.



% Clean-up the local computing node.
%
% We try to avoid a SSH connection from this node to itself, as it may not be
% already in its own known hosts.
%
get_clean_up_command_for_localhost( ScriptFullPath, State ) ->
	ScriptFullPath ++ " " ++ ?getAttr(node_name).



% Clean-up specified remote computing node.
%
% First, copies the script, then executes it there, then removes it.
get_clean_up_command_for_host( Hostname, ScriptFullPath, State ) ->

	% We suppose here we do not have anything to do, firewall-wise:
	SSHOption = executable_utils:get_ssh_mute_option(),

	% Note that this is the user name on the user node, not necessarily the user
	% name on the current host of interest, however this is not a problem as
	% long as it is consistently done:
	%
	Username = ?getAttr(user_name),

	% We used to suppose that the user home directory on this remote host was
	% the same as on this one, however the user name can change depending on
	% host. Instead of forging one with /home/USER, we just swap the user names,
	% not depending on the underlying path structure, so that it is preserved:

	%UserHomeDirectory = system_utils:get_user_home_directory(),
	%UserHomeDirectory = io_lib:format( "/home/~s", [Username] ),
	UserHomeDirectory = re:replace(
						  _Subject=system_utils:get_user_home_directory(),
						  _RegExp=system_utils:get_user_name(),
						  _Replacement=Username,
						  _Opts=[ { return, list } ] ),

	% Previously we attempted to use a one-liner with SSH but could not succeed,
	% so we had to write a specific script.
	%
	% Including a selection based on $USER, supposedly correctly set when
	% connected:
	%
	%"\"for p in $(/bin/ps -o pid,cmd -u $USER|"
	%	"grep beam|grep -v grep|grep -v " ++ StringCookie
	%	++ "| grep " ++ NodeName
	%	++ " | cut -f 1 -d ' ') ; do kill $p ; done ; "
	%	++ BasicCommand ++ "\"";

	% Hidden yet being still clearly related to Sim-Diasca:
	RemoteCleanScriptName = ".sim-diasca-node-cleaner.sh",

	TargetScriptName =
		filename:join( UserHomeDirectory, RemoteCleanScriptName ),

	% Like 'scp xx.sh joe@foo.org:/home/joe/yy.sh &&
	% ssh joe@foo.org "/home/joe/yy.sh NODE ; /bin/rm -f /home/joe/yy.sh"':
	RemoteCommand = "\"" ++ TargetScriptName ++ " " ++ ?getAttr(node_name)
		++ " ; /bin/rm -f " ++ TargetScriptName ++ "\"",

	text_utils:join( _Separator=" ", [
		executable_utils:get_default_scp_executable_path(),
		SSHOption,
		ScriptFullPath,
		Username ++ "@" ++ Hostname ++ ":" ++ TargetScriptName,
		"&&",
		executable_utils:get_default_ssh_client_path(),
		SSHOption,
		Username ++ "@" ++ Hostname,
		RemoteCommand ] ).



% To silence the spurious warning "The pattern {'failure', Reason} can never
% match the type 'success'" (indeed we *can* have it):
%
-dialyzer( { no_match, launch_erlang_node/1 }).



% Launches on the specfied host (remote or not, i.e. local) an appropriately
% configured Erlang node, on which first the deployment agent will be run.
%
% Returns either 'success' or { failure, Reason }.
%
-spec launch_erlang_node( wooper:state() ) ->
								'success' | { 'failure', ustring() }.
launch_erlang_node( State ) ->

	NodeName = ?getAttr(node_name),

	% To test how deployment failures are managed:
	%NodeName = list_to_atom( "non_matching_node_name_for_test" ),

	UserName = ?getAttr(user_name),
	Hostname = ?getAttr(managed_host),

	% For local launches, command explicitly launched with a dedicated blocked
	% process (previously: using os:cmd/1 with '&') in the background, hence no
	% return code nor command output actually expected - except for some basic
	% (ex: syntax) errors.
	%
	% For remote launches, the ssh option for background launches is used, yet
	% we still have some return code and a command output, hence we do not
	% consider this launch as a background one.
	%
	{ Command, Env, IsBackground } =
		get_erlang_launch_command( NodeName, UserName, Hostname, State ),

	?info_fmt( "Trying to launch computing node ~s on host ~s with user ~s "
		"(in the background: ~s) based on following "
		"command: '~s' and following environment: ~s",
		[ NodeName, Hostname, UserName, IsBackground, Command,
		  system_utils:environment_to_string( Env ) ] ),

	%trace_utils:debug_fmt( "### Launching: '~s', in the background: ~s, "
	%                       "env: ~s.",
	%						Command, IsBackground,
	%						system_utils:environment_to_string( Env ) ] ),

	% We will try to ensure that host managers will not answer after the
	% deployment manager times-out:
	%
	MaxWaitingBudget =
		?getAttr(deploy_time_out) - get_other_operations_duration(),

	% We will have to answer before the deployment manager times-out, yet we
	% want to let a node at the very least 2s for set-up, otherwise waiting for
	% node is pointless:
	%
	SuccessTimeout = max( 2000, MaxWaitingBudget ),

	% First we execute the command, and gather any feedback that would be
	% available (i.e. if not run in the background), then we check whether the
	% launched VM is available indeed.
	%
	{ ActualTimeOut, InfoString } = case IsBackground of

		true ->
			%trace_utils:debug_fmt( "Launching node in background with "
			%  "command = ~p and env = ~p.", [ Command, Env ] ),
			% No return code or output available here:
			system_utils:run_background_command( Command, Env ),
			{ SuccessTimeout, "launched in the background" };

		false ->
			%trace_utils:debug_fmt( "Launching node NOT in background with "
			%						"command = ~p and env = ~p.",
			%						[ Command, Env ] ),

			% Direct launch (to go in the background by itself
			% afterwards, hence with no relevant output or exit status):

			% 'system_utils:evaluate_shell_expression( Command, Env )' could be
			% used instead, yet no output is transmitted (empty string).

			case system_utils:run_command( Command, Env ) of

				% Best possible case:
				{ _ReturnCode=0, _CmdOutput=[] } ->
					{ SuccessTimeout,
					  "launched directly with no issue reported" };

				% Here we have either a non-zero return code and/or an output:
				ExecOutcome ->

				% Node apparently had trouble being launched, checking it:

					ErrorCauseString = case ExecOutcome of

							 { C, _Output=[] } ->
								 text_utils:format( "error code ~B", [ C ] );

							 { _C=0, Output } ->
								 text_utils:format( "output '~s'", [ Output ] );

							 { C, Output } ->
								 text_utils:format(
								   "error code ~B and output '~s'",
								   [ C, Output ] )

					end,


					% Here we consider not using the full waiting budget, as
					% apparently something probably went wrong (otherwise we
					% would wait for the maximum duration regardless of command
					% outcome):

					% (note that the previous launch command might have last a
					% long time, to the point that the simulation might already
					% be finished)
					%
					IssueTimeout = max( 2000,
										min( get_time_out_for(launch_error),
											 MaxWaitingBudget ) ),

					{ IssueTimeout, "launched, despite " ++ ErrorCauseString }

			end

	end,

	%trace_utils:debug_fmt( "ActualTimeOut = ~B ms.", [ ActualTimeOut ] ),

	FullyQualifiedNodeName = ?getAttr(full_node_name),

	% We will try to ensure that host managers will not answer after the
	% deployment manager times-out:
	%
	MaxWaitingBudget =
		?getAttr(deploy_time_out) - get_other_operations_duration(),

	% Node availability will be determined based on Erlang-level ping:
	%
	%trace_utils:debug_fmt( "Ping of '~s' with time-out ~p.",
	%					   [ FullyQualifiedNodeName, ActualTimeOut ] ),

	case net_utils:check_node_availability( FullyQualifiedNodeName,
											ActualTimeOut ) of

		{ true, Duration } ->
			%trace_utils:debug_fmt( "Ping success for '~s'.",
			%                      [ FullyQualifiedNodeName ] ),
			?info_fmt( "Node ~s on host ~s (~s) successfully launched and "
				"checked (which took ~B ms on a time-out of ~B ms, i.e. ~s).",
				[ NodeName, Hostname, InfoString, Duration, ActualTimeOut,
				  time_utils:duration_to_string( ActualTimeOut ) ] ),
			success;


		{ false, Duration } ->

			%trace_utils:debug_fmt( "Ping failure for '~s'.",
			%					   [ FullyQualifiedNodeName ] ),

			?error_fmt( "Node '~s' on host '~s' apparently failed to launch "
				"properly (reported as ~s) and is not responding "
				"after a measured duration of ~s "
				"(time-out was set to ~B milliseconds). "
				"Are you using indeed a proper SSH password-less "
				"account for that host, and is Erlang available on it? "
				"One may try executing: 'ssh USER@HOST erl' to check, "
				"i.e. typically 'ssh ~s@~s erl'; an Erlang prompt shall"
				" then be displayed (use CTR-C twice to exit it)." ,
				[ NodeName, Hostname, InfoString,
				  time_utils:duration_to_string( Duration ), ActualTimeOut,
				  UserName, Hostname ] ),

			Reason = interpret_launch_failure( ActualTimeOut, Duration,
						NodeName, UserName, Hostname, Command, State ),

			{ failure, Reason }

	end.



% Helper, to try to diagnose why no answer (Erlang-level ping) from a launched
% VM was obtained, based on the look-up of relevant UNIX processes.
%
% Sends a trace message and returns a reason atom.
%
-spec interpret_launch_failure( time_out(), milliseconds(), string_node_name(),
		user_name(), string_host_name(),
		system_utils:command(), wooper:state() ) -> atom().
interpret_launch_failure( ActualTimeOut, Duration, NodeName, UserName,
						  Hostname, Command, State ) ->

	% We will try to count the number of live Erlang VMs on the host of
	% interest.

	%BaseCountCmd = "/bin/ps -u " ++ UserName ++ " | /bin/grep beam | wc -l",

	% As the VM executable is either beam or beamp.smp, and that it can be
	% followed either by arguments (then first by a whitespace) or by an end of
	% line:
	%
	BaseCountCmd = "/bin/ps -u " ++ UserName
		++ " | /bin/grep -E '^.*(beam|beam.smp)(\s|$)'| wc -l",

	case ?getAttr(managed_host) of

		localhost ->

			% On the user host, there is of course at least this user node (VM):

			case system_utils:run_command( BaseCountCmd ) of

				% No output (strange, unexpected)
				{ _ReturnCode=0, _CmdOutput="" } ->
					?error_fmt( "Local computing node not responding to Erlang "
						"ping after ~B milliseconds (time-out duration: ~B), "
						"not able either to detect any VM process. "
						"Launch command was: '~s'.",
						[ Duration, ActualTimeOut, Command ] ),
					vm_detection_abnormal;

				% No VM found:
				{ _ReturnCode=0, _CmdOutput="0" } ->
					?error_fmt( "Local computing node not responding to Erlang "
						"ping after ~B milliseconds (time-out duration: ~B), "
						"detecting zero VM process (whereas at least one "
						"should have been found). Launch command was: '~s'.",
						[ Duration, ActualTimeOut, Command ] ),
					vm_detection_none;


				% Exactly one VM found:
				{ _ReturnCode=0, _CmdOutput="1" } ->
					?error_fmt( "Local computing node not responding to Erlang "
						"ping after ~B milliseconds (time-out duration: ~B) "
						"nor found as a live process, its VM must not have "
						"been launched properly. Launch command was: '~s'.",
						[ Duration, ActualTimeOut, Command ] ),
					launched_vm_not_found;

				% More than one VM found (not a problem per se):
				{ _ReturnCode=0, _CmdOutput=AtLeastTwo } ->
					?error_fmt( "Local computing node not responding to Erlang "
						"ping after ~B milliseconds (time-out duration: ~B) "
						"whereas multiple VMs (~s) are found running. "
						"Launch command was: '~s'.",
						[ Duration, ActualTimeOut, AtLeastTwo, Command ] ),
					multiple_vms_detected;

				% Unexpected error:
				{ ReturnCode, CmdOutput } ->
					?error_fmt( "Error (code: ~B, message: '~s') when "
						"looking-up local computing nodes (VMs). "
						"Launch command was: '~s'.",
						[ ReturnCode, CmdOutput, Command ] ),
					vm_detection_failed

			end;


		 _Hostname ->

			% Let's connect to that host to check whether a VM is running there:
			SSHOption = executable_utils:get_ssh_mute_option(),

			CountCmd = text_utils:join( _Separator=" ", [
				executable_utils:get_default_ssh_client_path(),
				SSHOption, UserName ++ "@" ++ Hostname, BaseCountCmd ] ),

			case system_utils:run_command( CountCmd ) of

				{ _ReturnCode=0, _CmdOutput="" } ->
					?error_fmt(
					   "Node ~s on host ~s apparently successfully "
					   "launched, but not responding (to Erlang ping) "
					   "after ~B milliseconds (time-out duration: ~B), "
					   "and the counting of VM processes on that host failed "
					   "(no count could be obtained). "
					   "The VM may have crashed soon or may have not properly "
					   "been launched; launch command was: '~s'.",
					   [ NodeName, Hostname, Duration, ActualTimeOut,
						 Command ] ),
					vm_remote_detection_abnormal;

				{ _ReturnCode=0, _CmdOutput="0" } ->
					?error_fmt(
					   "Node ~s on host ~s apparently successfully "
					   "launched, but not responding (to Erlang ping) "
					   "after ~B milliseconds (time-out duration: ~B), "
					   "and the counting of VM processes on that host reported "
					   "that none is running. "
					   "The VM may have crashed soon or may have not properly "
					   "been launched; launch command was: '~s'.",
					   [ NodeName, Hostname, Duration, ActualTimeOut,
						 Command ] ),
					remote_launched_vm_not_found;


				{ _ReturnCode=0, _CmdOutput="1" } ->
					?error_fmt(
					   "Node ~s on host ~s apparently successfully "
					   "launched, but not responding (to Erlang ping) "
					   "after ~B milliseconds (time-out duration: ~B), "
					   "whereas exactly one VM process was found on "
					   "that host; launch command was: '~s'.",
					   [ NodeName, Hostname, Duration, ActualTimeOut,
						 Command ] ),
					one_remote_vm_detected;

				{ _ReturnCode=0, _CmdOutput=AtLeastTwo } ->
					?error_fmt(
					   "Node ~s on host ~s apparently successfully "
					   "launched, but not responding (to Erlang ping) "
					   "after ~B milliseconds (time-out duration: ~B). "
					   "Apparently multiple VMs (~s) were found there; "
					   "launch command was: '~s'.",
					   [ NodeName, Hostname, Duration, ActualTimeOut,
						 AtLeastTwo, Command ] ),
					multiple_remote_vms_detected;

				{ ReturnCode, CmdOutput } ->
					?error_fmt(
					   "Not able to establish whether node ~s on host ~s "
					   "has been successfully launched (code: ~B, message:'~s')"
					   "Launch command was: '~s'.",
					   [ NodeName, Hostname, ReturnCode, CmdOutput, Command ] ),
					vm_remote_detection_failed

			end

	end.



% Called whenever an 'EXIT' message is received, typically from the associated
% deployment agent.
%
-spec onWOOPERExitReceived( wooper:state(), pid(), exit_reason() ) ->
								const_oneway_return().
onWOOPERExitReceived( State, _Pid, _ExitReason=normal ) ->
	% Just ignored:
	wooper:const_return();

onWOOPERExitReceived( State, Pid, ExitReason ) ->

	?emergency_fmt( "EXIT message received for ~w, whose exit reason was: ~p, "
					"terminating now.", [ Pid, ExitReason ] ),

	% Shall be avoided, as would attempt to interact with the now defunct
	% deployment agent:
	%
	% self() ! delete,

	basic_utils:stop_on_failure( 10 ),

	wooper:const_return().



% Called whenever a 'DOWN' message is received, typically from the associated
% deployment agent.
%
-spec onWOOPERDownNotified( wooper:state(), monitor_utils:monitor_reference(),
	monitor_utils:monitored_element_type(), monitor_utils:monitored_element(),
							exit_reason() ) -> const_oneway_return().
onWOOPERDownNotified( State, MonitorReference, MonitoredType, MonitoredElement,
					  ExitReason ) ->

	% Not an error trace anymore, as at least most of the time an error signal
	% has already been received and handled:
	%
	?notice_fmt( "DOWN message received (reference: ~p) for monitored element "
		"'~p' (of type ~p), whose exit reason was: ~p, "
		"terminating now.", [ MonitorReference, MonitoredElement,
									 MonitoredType, ExitReason ] ),

	% Shall be avoided, as would attempt to interact with the now defunct
	% deployment agent:
	%
	% self() ! delete,

	basic_utils:stop_on_failure( 11 ),

	wooper:const_return().




% Returns a command suitable to the launching of the corresponding Erlang node,
% with a relevant environment and telling whether this shall be a background
% launch.
%
% (helper)
%
-spec get_erlang_launch_command( string_node_name(), user_name(),
								 string_host_name(), wooper:state() ) ->
	   { system_utils:command(), system_utils:environment(), boolean() }.
get_erlang_launch_command( NodeName, Username, Hostname, State ) ->

	% We replicate the settings of the user node on all computer nodes:
	%
	% (see the --max-process-count and --async-thread-count options of
	% myriad/src/scripts/launch-erl.sh)
	%
	AsynchThreadsCount = erlang:system_info( thread_pool_size ),
	MaxProcesses = erlang:system_info( process_limit ),

	SeqOption = case ?getAttr(scheduler_count) of

		undefined ->
			"";

		Count ->
			text_utils:format( "+S ~B", [ Count ] )

	end,

	% tnnps: thread_no_node_processor_spread - a combination of thread_spread,
	% and no_node_processor_spread. Schedulers will be spread over hardware
	% threads across NUMA nodes, but schedulers will only be spread over
	% processors internally in one NUMA node at a time.
	%
	% Apparently decreases the duration of at least some simulation runs, and
	% makes the scalability curves considerably smoother.

	AdditionalOptions = text_utils:format(
		" -noshell -smp auto +sbt tnnps ~s +K true +A ~B +P ~B ",
		[ SeqOption, AsynchThreadsCount, MaxProcesses ] ),

	EpmdPort = ?getAttr(epmd_port),


	% At least on some hosts, the domain name as resolved by Erlang is not the
	% one included in the FQDN; for example:
	%
	%  - 'hostname -f' will return 'hurricane.foo.org'
	%
	%  - 'erl -name hello' will result in a prompt like:
	% (hello@hurricane.localdomain)1>
	%
	% (culprit: /etc/resolv.conf having still a 'domain localdomain', whereas
	% /etc/hosts has something like:
	% 127.0.1.1  hurricane.foo.org hurricane
	% (/etc/resolv.conf shall be fixed, but we have to overcome it anyway...)
	%
	% So, instead of specifying 'my_user_node_name' (at, implicitly, the local
	% host), we have to specify 'my_user_node_name@ hurricane.foo.org':

	% Not wanting the 'localhost' atom:
	ActualHostname = case Hostname of

		localhost ->
			net_utils:localhost();

		_ ->
			Hostname

	end,

	FullNodeName = text_utils:format( "~s@~s", [ NodeName, ActualHostname ] ),

	% The next command propagates the cookie of the user node to this newly
	% launched computing node (using -setcookie); however there seems to be a
	% short time window for a race condition, as (quite infrequently) we can see
	% a computing node reporting "** Connection attempt from disallowed node",
	% (that node being the user node); however this seems to be only a transient
	% error and the simulation overcomes it, as we saw it.
	%
	{ BasicCommand, BasicEnv } = net_utils:get_basic_node_launching_command(
		FullNodeName, ?getAttr(node_naming_mode), EpmdPort,
		?getAttr(tcp_port_range), AdditionalOptions ),

	%trace_utils:debug_fmt( "Basic command = '~s'.", [ BasicCommand ] ),

	case ?getAttr(managed_host) of


		localhost ->

			% We are on the current host, no need to perform a SSH login (it
			% should even be avoided, as most accounts are not configured for a
			% password-less authentication to self), as already logged here:
			%
			% (checking we are using indeed the same user)
			%
			Username = system_utils:get_user_name(),

			{ BasicCommand ++ " & ", BasicEnv, _IsBackground=true };


		_ ->

			% We target a remote host here:

			EpmdPrefix = case EpmdPort of

			   undefined ->
					  "";

			   Port when is_integer( Port ) ->
				  text_utils:format( "export ERL_EPMD_PORT=~B && ", [ Port ] )

			end,

			% -f: Requests ssh to go to background just before command execution
			%
			% We suppose here we do not have anything to do, firewall-wise:
			%
			Command = executable_utils:get_default_ssh_client_path() ++ " "
				++ executable_utils:get_ssh_mute_option() ++ " -f "
				++ Username ++ "@" ++ Hostname ++ " '" ++ EpmdPrefix
				++ BasicCommand ++ "'",

			% Now in the background as well, as no output or exit status can be
			% expected:
			%
			{ Command, BasicEnv, _IsBackground=true }

	end.



% Sends pioneer modules (e.g. the deployment agent with its prerequisites), that
% will then organise the deployment, based on the simulation archive that is
% expected to be received from the deployment manager afterwards (see the
% deploy/5 function).
%
% Returns an udpated state.
%
-spec send_deployment_agent( wooper:state() ) -> wooper:state().
send_deployment_agent( State ) ->

	TargetNode = ?getAttr(full_node_name),

	% This system_info call may not work on ancient Erlang versions, see
	% system_utils:get_interpreter_version/0:
	%
	% (more generally, depending on the version clashes, a worker VM may be
	% unable to report at all its version, or to connect to the user node, thus
	% these checkings may not be triggered in all cases; rule of thumb: use the
	% latest stable Erlang version everywhere, see our install-erlang.sh script
	% for that, and that's it!)

	{ IsVersionObtained, VersionInfo } = case rpc:call( TargetNode,
								_FirstModule=erlang,
								_FirstFunction=system_info,
								_FirstArgs=[ otp_release ] ) of

		{ badrpc, FirstReason } ->

			case rpc:call( TargetNode, init, script_id, [] ) of

				{ badrpc, SecondReason } ->
					Comment = text_utils:format( "the remote Erlang version "
						"could not be determined (reasons: first ~p, then ~p)",
						[ FirstReason, SecondReason ] ),
					{ false, Comment };

				{ _OTPInfos, V } ->
					{ true, V }

			end;

		Version ->
			{ true, Version }

	end,

	case IsVersionObtained of

		true ->
			?info_fmt( "Sending Sim-Diasca deployment agent and "
				"its prerequisites to node ~s, which runs the "
				"following Erlang version: '~s'.",
				[ TargetNode, VersionInfo ] );

		false ->
			?error_fmt( "Sending Sim-Diasca deployment agent and its "
				"prerequisites to node ~s, whereas ~s.",
				[ TargetNode, VersionInfo ] )

	end,

	% These essential modules are badly needed by the deployment agent, so
	% sending them beforehand as well.
	%
	% They are not very heavy (less than 200 kB in total, as shown by: 'du -ch
	% myriad/src/utils/{basic,file,net,system,text}_utils.beam
	% sim-diasca/src/core/src/deployment/deployment_agent.beam', so sending them
	% through mere Erlang messages should not be a real problem (as opposed to
	% the simulation archive, whose size can be considerably higher depending on
	% the simulation at hand)
	%
	% We found that if ever text_utils.beam is sometimes lacking during its
	% deployment (whereas found present), or if a strange bad_fun exception is
	% triggered, this may be the result of a faulty automatic rebuild pared with
	% the use of an erlc in a different version than the initial build)
	%
	% Such pioneer modules may be reported as lacking should, for example, a
	% computing host be specified incorrectly (ex: based on its IP address,
	% rather than on its expected FQDN).
	%
	ModulesToDeploy = [ text_utils, basic_utils, file_utils, net_utils,
						system_utils, deployment_agent ],

	% We inserted this delay as we suspect a potential race condition when the
	% VM launches.
	%
	% Indeed, in some cases, on some very specific machines, we got a
	% {not_a_string,'CosEventChannelAdmin_AlreadyConnected'} exception raised,
	% suggesting that we may receive an unexpected message - probably because of
	% a race condition triggered if we try to deploy modules while the start-up
	% of the VM has not completed yet:
	%
	%timer:sleep( 500 ),

	try

		% Only a single target node here:
		code_utils:deploy_modules( ModulesToDeploy, [ TargetNode ] )

	catch

		{ module_deployment_failed, _FileUtils, [ { error, badfile } ] } ->

			LocalVersion = system_utils:get_interpreter_version(),

			{ RemoteVersionString, RemoteVersionAtom } =
				case IsVersionObtained of

				true ->
					{ text_utils:format( "version ~s", [ VersionInfo ] ),
					  VersionInfo };

				false ->
					{ "a version that could not be determined",
					  unknown_version }

			end,

			Message = text_utils:format(
				"Deployment failed, possibly due to a version mistmatch "
				"between the Erlang environments in the user node "
				"(~s, which relies on ~s) and the node ~s "
				"(which relies on ~s).",
				[ net_utils:localhost(), LocalVersion, TargetNode,
				  RemoteVersionString ] ),

			trace_utils:emergency( Message ),

			?emergency( Message ),

			throw( { module_deployment_failed,
					 possibly_incompatible_erlang_versions,
					 { { local, LocalVersion },
					   { TargetNode, RemoteVersionAtom } } } );

		Type:Exception:StackTrace ->

			StackString = code_utils:interpret_stacktrace( StackTrace ),

			?alert_fmt( "Error (~p) while deploying ~p on '~s': ~p.~n"
				"Stack trace is:~n~s",
				[ Type, ModulesToDeploy, TargetNode, Exception, StackString ] ),

			throw( { module_deployment_failed, Type, Exception,
					 StackString } )

	end,

	% Of course we do not want to wait for this deploy/5 function to finish, as
	% it is itself waiting for the simulation to finish, so this is a
	% non-blocking call (obviously without a result being returned):

	GroupLeaderPid = group_leader(),

	% 'Oneway' call of the deployment_agent:deploy/5 on the target computing
	% node, corresponding to the creating of that agent:
	%
	rpc:cast( TargetNode,
			  _SecondModule=deployment_agent,
			  _SecondFunction=deploy,
			  _SecondArgs=[ self(), GroupLeaderPid, ?getAttr(tick_time_out),
							?getAttr(deploy_base_dir),
							?getAttr(additional_beam_bin_dirs),
							?getAttr(simulation_instance_id) ] ),

	State.



-spec get_time_out_for( 'launch_success' | 'launch_error' ) -> milliseconds().


-ifdef(exec_target_is_production).


% Returns the duration, in milliseconds, that shall be waited until deciding a
% non-responding launched node is unavailable, depending on the value returned
% by its launch command.


% In production mode, we want to overcome situations where a few nodes might be
% especially long to set-up:
%
get_time_out_for( launch_success ) ->
	% 5 minutes:
	5 * 60 * 1000;

get_time_out_for( launch_error ) ->
	% 2 minutes:
	2 * 60 * 1000.


-else. % exec_target_is_production


% In development mode, we want to be reactive, thus we rely on shorter
% durations:
%
get_time_out_for( launch_success ) ->
	% 400 seconds:
	400 * 1000;

get_time_out_for( launch_error ) ->
	% 5 seconds:
	5 * 1000.


-endif. % exec_target_is_production
