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



% Agent to be sent, thanks to deployment workers, on all computing nodes, so
% that it can deploy automatically everything that is needed there in order to
% run a simulation.
%
% Not using WOOPER here, in order to avoid needing extra dependencies and
% environment during this bootstrap phase.
%
-module(deployment_agent).


-export([ deploy/6 ]).



% Implementation notes:
%
% Includes are not a problem (they are seen at build time, as any other module);
% however making use of other modules is a problem, as this pioneer module
% should be as self-contained as reasonably possible.
%
% Thus some functions defined in other modules were duplicated verbatim
% from other base modules (ex: system_utils), to avoid having to rely on too
% many prerequisite modules (the pioneer list must be lean and mean).
%
% Finally, we included more pioneer modules, as verbatim duplication is
% error-prone.
%
% Pioneer modules are listed in
% class_ComputingHostManager:send_deployment_agent/1.
%
% Ensuring a proper termination in all cases is difficult for such an agent, see
% log_on_file/{1,2} to check/investigate.


% For computing_host_manager_pid():
-include("engine_common_defines.hrl").


% For trace_aggregator_name:
-include_lib("traces/include/class_TraceAggregator.hrl").


% For tracing_activated:
-include_lib("traces/include/class_TraceEmitter.hrl").

% For host_static_info record:
-include_lib("myriad/include/system_utils.hrl").

% For the file_info record:
-include_lib("kernel/include/file.hrl").


% Time-out in milliseconds before this node considers that it could not connect
% back to the user one:
%
-define( connection_time_out, 600000 ).


% Comment-out to disable:
-define( enable_log_to_file, ).


% The format string to establish the log filename where extra debug information
% regarding deployment will be written:
%
% (path expected to be host-specific - not expecting to be on a shared
% filesystem where deployment agents would overwrite their respective log files)
%
-define( deploy_log_file_fmt,
		 "/tmp/sim-diasca-deployment-debug-for-sii-~s.txt" ).


% Silencing:
-compile({ nowarn_unused_function, [ set_handler/1, get_handler_config/1 ] }).


% Shorthands:

-type directory_name() :: file_utils:directory_name().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type trace_severity() :: traces:trace_severity().

-type sii() :: sim_diasca:sii().




% Performs the actual deployment; triggered by a rpc:cast/4 called by the
% associated computing host manager.
%
% io:format print-outs will end up in the user console.
%
% ComputerHostManagerPid is sent, as it is the main user-side interlocutor for
% remote deployment agents.
%
-spec deploy( computing_host_manager_pid(), pid(), unit_utils:seconds(),
			  bin_string(), [ file_utils:bin_directory_name() ], sii() ) ->
					'onDatabaseStarted' | 'onDatabaseStopped'.
deploy( ComputerHostManagerPid, GroupLeaderPid, InterNodeTickTimeOut,
		BinDeployBaseDir, AdditionalBEAMBinDirs, SII ) ->

	% Not expected to exist, but 'ok' not matched for a better robustness:
	file:delete( io_lib:format( ?deploy_log_file_fmt, [ SII ] ) ),

	log_on_file( SII, "Deployment started." ),

	% Linking a deployment agent to the rest of the simulation (e.g. the agents
	% of the user node) must be associated to trapping exits, otherwise a mere
	% link may crash this agent whereas it would have performed teardown
	% operations (such as terminating its computing node).

	erlang:process_flag( trap_exit, true ),

	erlang:link( ComputerHostManagerPid ),

	% All nodes must behave the same:
	change_initiated = net_kernel:set_net_ticktime( InterNodeTickTimeOut ),

	% Should a computing node fail (ex: bug in an actor), not only the relevant
	% processes shall be stopped, but also (all) computing nodes and the user
	% one. So we monitor nodes from that agent (basically we monitor the user
	% node):
	%
	ok = net_kernel:monitor_nodes( _NewSubscription=true ),

	% Reports I/O to the user node:
	group_leader( GroupLeaderPid, self() ),

	% Declares the additional BEAM directories (in a consistent order):
	Dirs = [ binary_to_list( D ) || D <- AdditionalBEAMBinDirs ],

	ok = code:add_pathsa( lists:reverse( Dirs ) ),

	% Not wanting to deploy an extra dependency (onto naming_utils), hence not
	% using its wait_for_global_registration_of/1 function:
	%
	%TraceAggregatorPid = naming_utils:wait_for_global_registration_of(
	%					   ?info_aggregator_name ),

	TraceAggregatorPid =
		wait_for_global_registration_of( ?trace_aggregator_name ),


	send_trace_fmt( TraceAggregatorPid, "Deployment agent running on node ~p, "
		"with version ~s of the virtual machine, requesting the "
		"simulation package from ~w. Current scheduler count: ~B.",
		[ node(), system_utils:get_interpreter_version(),
		  ComputerHostManagerPid, erlang:system_info( schedulers ) ], info ),

	ComputerHostManagerPid ! { requestPackage, node(), self() },

	% Prepare some operations in the meantime:
	{ DeployBaseDir, DeployBeamDir } =
		prepare_package( BinDeployBaseDir, TraceAggregatorPid ),

	receive


		{ wooper_result, deploy_time_out } ->
			Message = "overall deployment time-out reached",
			log_on_file( SII, "Error: ~s.", [ Message ] ),
			terminate( error, Message, TraceAggregatorPid, SII );


		{ wooper_result, send_starting } ->

			log_on_file( SII, "Package receiving started." ),

			send_trace( TraceAggregatorPid,
					"Receiving of the simulation package started.", debug ),

			% The current directory is already correct:
			PackageFilename = net_utils:receive_file( ComputerHostManagerPid ),

			send_trace( TraceAggregatorPid,
						"Simulation package fully received.", debug ),

			PackageBin = file_utils:read_whole( PackageFilename ),

			manage_package( PackageBin, DeployBaseDir, DeployBeamDir,
							TraceAggregatorPid ),

			{ _UsedSwap, TotalSwap } = system_utils:get_swap_status(),

			HostInfo = #host_static_info{
						total_ram=system_utils:get_total_physical_memory(),
						total_swap=TotalSwap,
						core_count=system_utils:get_core_count(),
						erlang_version=system_utils:get_interpreter_version() },

			ComputerHostManagerPid ! { onDeploymentReady, HostInfo },

			log_on_file( SII, "Deployment done, entering final loop." ),

			% Kept running, as could be useful later:
			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir, SII );


		{ nodedown, NodeDown } ->
			on_node_down( NodeDown );


		% Ignored in the mailbox, managed later:
		%{ nodeup, NodeUp } ->
		%	io:format( "(received a notification about node '~s' "
		%			   "becoming up)~n", [ NodeUp ] ),...


		% Not read yet: { 'EXIT', SourcePid, _ExitReason=normal } ->

		{ 'EXIT', SourcePid, ExitReason } when ExitReason =/= normal ->

			Reason = io_lib:format( "EXIT signal received from ~w "
				"with reason '~p', terminating", [ SourcePid, ExitReason ] ),

			log_on_file( SII, "Error: ~s.", [ Reason ] ),

			terminate( error, Reason, TraceAggregatorPid, SII );


		terminate ->
			Message = "termination request received while waiting "
				"for deployment start",

			log_on_file( SII, "Info: ~s.", [ Message ] ),

			terminate( error, Message, TraceAggregatorPid, SII )


	after ?connection_time_out->

			Message = io_lib:format( "no answer received on time (after ~w ms) "
				"from the user node regarding the package request",
				[ ?connection_time_out ]  ),

			log_on_file( SII, "Error: ~s.", [ Message ] ),

			terminate( error, Message, TraceAggregatorPid, SII )

	end.



% Prepares to receive the deployment package.
-spec prepare_package( bin_string(), pid() ) ->
							{ directory_name(), directory_name() }.
prepare_package( BinDeployBaseDir, TraceAggregatorPid ) ->

	DeployBaseDir = binary_to_list( BinDeployBaseDir ),

	case file_utils:exists( DeployBaseDir ) of

		true ->

			send_trace_fmt( TraceAggregatorPid,
				"Deployment directory '~s' already existing "
				"as a filesystem element, removing it fully first.",
				[ DeployBaseDir ], info ),

			% No higher-level code available yet:
			Command = "/bin/rm -rf '" ++ DeployBaseDir ++ "' 1>/dev/null",

			case system_utils:run_command( Command ) of

				{ _ErrorCode=0, _CmdOutput=[] } ->
					ok;

				{ _ErrorCode=0, CmdOutput } ->
					send_trace_fmt( TraceAggregatorPid,
						"Removal of deployment directory '~s' succeeded, yet "
						"output following message: '~s'.",
						[ DeployBaseDir, CmdOutput ], warning );

				{ ErrorCode, CmdOutput } ->
					send_trace_fmt( TraceAggregatorPid, "Error, removal of "
						"deployment directory '~s' failed (error code: ~B, "
						"output: '~s').",
						[ DeployBaseDir, ErrorCode, CmdOutput ], error )

			end;


		false ->
			send_trace_fmt( TraceAggregatorPid,
				"Deployment directory '~s' not already existing, creating it.",
				[ DeployBaseDir ], debug )

	end,

	DeployBeamDir = filename:join( DeployBaseDir, "deployed-elements" ),

	file_utils:create_directory( DeployBeamDir, create_parents ),

	file_utils:set_current_directory( DeployBeamDir ),

	{ DeployBaseDir, DeployBeamDir }.



% Manages the received deployment package.
-spec manage_package( binary(), directory_name(), directory_name(), pid() ) ->
							void().
manage_package( PackageBin, DeployBaseDir, DeployBeamDir,
				TraceAggregatorPid ) ->

	send_trace_fmt( TraceAggregatorPid,
		"Received simulation package, whose size is ~B bytes, "
		"will extract it in deployment directory '~s'.~n",
		[ size( PackageBin ), DeployBeamDir ], debug ),

	FileNames = file_utils:zipped_term_to_unzipped_files( PackageBin ),

	send_trace_fmt( TraceAggregatorPid,
		"Following ~B files were extracted in '~s':~n~p.~n",
		[ length( FileNames ), DeployBeamDir, lists:sort( FileNames ) ],
		debug ),

	% Now updating the code path according to the layout of the deployed tree:

	% Some directories not containing BEAMs could be removed:
	BeamDirs = file_utils:find_directories_from( "." ),

	% Dealing with absolute directories is easier to debug:
	AbsoluteBeamDirs = [ filename:join( DeployBeamDir, D ) || D <- BeamDirs ],

	%io:format( "Added BEAM dirs: ~p.~n", [ AbsoluteBeamDirs ] ),

	ok = code:add_paths( AbsoluteBeamDirs ),

	% Works, yet is actually fully useless, as visibly the Erlang logger already
	% takes care of forwarding properly any message to its counterpart on the
	% user node; so setting our handler here would just result in such messages
	% to be duplicated verbatim in the traces.
	%
	% Integrates to the Erlang logger (first time it is possible, as logger will
	% look up traces:log/2 in the code path directly):
	%
	%set_handler( TraceAggregatorPid ),

	send_trace_fmt( TraceAggregatorPid, "Following BEAM directories were "
					"added to code path:~n~p.~n", [ BeamDirs ], info ),

	%io:format( "Updated code path:~n~p.~n", [ code:get_path() ] ),

	OutputDir = filename:join( DeployBaseDir, "outputs" ),

	file_utils:create_directory( OutputDir, create_parents ),

	% We prefer that the VM of the computing nodes remain at the root of the
	% temporary directory created for deployment:
	%
	% (ex: '/tmp/sim-diasca-My_case-My_User-2017-11-21-at-16h-14m-33s-1007882/')
	%
	% file_utils:set_current_directory( OutputDir ).

	ok.



% Final loop of this deploy agent.
final_main_loop( TraceAggregatorPid, ComputerHostManagerPid, BinDeployBaseDir,
				 SII ) ->

	% To test the proper simulation teardown should this agent fail:
	%erlang:halt( abort ),

	receive

		% Answer from the class_ComputingHostManager:requestPackage/2 request,
		% that was ignored in the previous receiving:
		%
		{ wooper_result, DeployFilenameBin }
		  when is_binary( DeployFilenameBin ) ->

			log_on_file( SII, "(package archive was '~s')",
						 [ DeployFilenameBin ] ),

			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir, SII );

		{ start_database, CallerPid } ->

			% The mnesia directory must already have been set here (no
			% application:set_env( mnesia, dir, ... ) taken into account here).

			send_trace_fmt( TraceAggregatorPid,
							"Deployment agent starting database on node ~s.",
							[ node() ], info ),

			%io:format( "Deployment agent starting database on node ~s.~n",
			%		  [ node() ] ),

			% No prior loading accepted:

			%%case application:load(mnesia) of

			%%	ok ->
			%%		ok;

			%%	LoadError ->
			%%		throw( { mnesia_load_failed, node(), LoadError } )

			%%end,

			case application:start( mnesia ) of

				ok ->
					ok;

				StartError ->
					throw( { mnesia_start_failed, node(), StartError } )

			end,

			%io:format( "Database started on node ~s.~n", [ node() ] ),

			CallerPid ! onDatabaseStarted,

			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir, SII );


		{ stop_database, CallerPid } ->

			%io:format( "~w stopping database.~n", [ self() ] ),

			ok = application:stop( mnesia ),
			ok = application:unload( mnesia ),

			%io:format( "~w stopped database.~n", [ self() ] ),

			CallerPid ! onDatabaseStopped,

			% We must recurse, as we still want to properly terminate, otherwise
			% there would be lingering computing nodes:
			%
			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir, SII );


		{ nodeup, NewNode } ->

			Message = io_lib:format( "A new node connected (to ~p): ~p.",
									 [ node(), NewNode ] ),

			%io:format( "Warning: ~s~n", [ Message ] ),
			log_on_file( SII, Message ),

			send_trace( TraceAggregatorPid, Message, info ),

			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir, SII );


		{ nodedown, NodeDown } ->
			on_node_down( NodeDown );


		% Ignored (ex: coming from port):
		{ 'EXIT', SourcePid, _ExitReason=normal } ->

			log_on_file( SII, "Normal EXIT received for ~w.", [ SourcePid ] ),

			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir, SII );


		{ 'EXIT', SourcePid, ExitReason } ->
			Reason = io_lib:format( "EXIT signal received from ~w "
									"with reason '~p', terminating",
									[ SourcePid, ExitReason ] ),


			log_on_file( SII, Reason ),

			terminate( error, Reason, TraceAggregatorPid, SII );


		terminate ->

			Message = io_lib:format( "Requested to terminate, removing "
				"deployment directory '~s' and terminating now.",
				[ BinDeployBaseDir ] ),

			log_on_file( SII, Message ),

			send_trace( TraceAggregatorPid, Message, info ),

			% Set to false if wanting to inspect the deployed directory:
			RemoveDeployedDirectory = true,
			%RemoveDeployedDirectory = false,

			DeployDirString = binary_to_list( BinDeployBaseDir ),

			case RemoveDeployedDirectory of

				true ->

					RemoveCommand = "/bin/rm -rf '" ++ DeployDirString ++ "'",

					case system_utils:run_command( RemoveCommand ) of

						{ _ReturnCode=0, _CmdOutput=[] } ->
							ok;

						{ ReturnCode, CmdOutput } ->
							send_trace_fmt( TraceAggregatorPid,
								"Problem while removing deployment directory "
								"'~s' (error code: ~B, message '~s').",
								[ BinDeployBaseDir, ReturnCode, CmdOutput ],
								error )

					end;

				false ->
					send_trace_fmt( TraceAggregatorPid,
						"Not removing the deployment directory '~s'.",
						[ DeployDirString ], warning )

			end,

			terminate( SII );


		Unexpected ->

			log_on_file( SII,
						 "Received unexpected, hence ignored, message:~n~p",
						 [ Unexpected ] ),

			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir, SII )


		% No more 'after' clause to perform an automatic shutdown after a
		% time-out; at this point this computing node should be connected to the
		% user node (at least), and, should a disconnection happen, it will
		% detected thanks to the net tick time or the node monitoring.

	end.



% Called whenever a node (most probably the user node, the only other node
% known) is detected as down.
%
% (helper)
%
-spec on_node_down( net_utils:atom_node_name() ) -> no_return().
on_node_down( NodeDown ) ->

	log_on_file( "Deployment agent detected the 's' node is down, stopping.",
				 [ NodeDown ] ),

	% We must have lost the user node, hence both the trace aggregator and our
	% group leader as well.
	%
	% As a result we should not attempt to emit a trace or any console I/O
	% (otherwise we are bound to block there), we just terminate (on error)
	% directly, to ensure this computing node will not linger (as a zombi UNIX
	% process) whereas that deployment failed:
	%
	init:stop( _ExitCode=16 ).



% Reports the specified termination reason on the console and as a trace, and
% terminate.
%
-spec terminate( atom(), ustring(), trace_aggregator_pid(), sii() ) ->
						no_return().
terminate( _TraceLevel, Reason, _TraceAggregatorPid, SII ) ->

	% Apparently the deployment agent, at least often, is able to record on file
	% the next trace (reason: EXIT signal received from <XXX.92.0> with reason
	% 'noconnection') but not the one in terminate/1; as it is trapping EXITs,
	% the reason we see could be the caller to either send_trace/3 and/or
	% io:format/2. At least for the sake of testing we disabled both below, and
	% it seems to work.

	Message = io_lib:format( "Deployment agent ~w halting now the computing "
		"node '~s'; reason: ~s.", [ self(), node(), Reason ] ),

	log_on_file( SII, Message ),

	%send_trace( TraceAggregatorPid, Message, TraceLevel ),

	%io:format( Message ++ "~n", [] ),

	terminate( SII ).



-spec terminate( sii() ) -> no_return().
terminate( SII ) ->

	% We do not want nodes to wait any longer, otherwise old code of modules
	% could linger:
	%
	log_on_file( SII, "Deployment agent terminating immediately." ),

	%io:format( "~n(deployment agent ~p terminating immediately)~n",
	%		   [ self() ] ),
	%timer:sleep( 1000 ),

	% Remote shutdown directly done by computing host manager:
	init:stop( _Success=0 ).



-ifdef( enable_log_to_file ).


% Just for debugging/testing:
log_on_file( SII, FormatString, Values ) ->

	Message = io_lib:format( FormatString, Values ),

	log_on_file( SII, Message ).


log_on_file( SII, String ) ->

	TimestampString = io_lib:format( "~B/~B/~B at ~B:~B:~B",
					tuple_to_list( date() ) ++ tuple_to_list( time() ) ),

	Message = io_lib:format( "~n[~s] ~s", [ TimestampString, String ] ),

	LogFilename = io_lib:format( ?deploy_log_file_fmt, [ SII ] ),

	ok = file:write_file( LogFilename, Message, _Modes=[ append ] ).


-else.


log_on_file( _SII, _FormatString, _Values ) ->
	ok.

log_on_file( _SII, _String ) ->
	ok.


-endif.


% Section to help sending traces from the deployment agent, which is not a trace
% emitter.


-ifdef(tracing_activated).


% Here all trace types are sent:

-spec send_trace( pid(), ustring(), trace_severity() ) -> void().
send_trace( TraceAggregatorPid, Message, TraceSeverity ) ->
	send_trace_helper( TraceAggregatorPid, Message, TraceSeverity ).


-spec send_trace_fmt( pid(), text_utils:format_string(), [ any() ],
					  trace_severity() ) -> void().
send_trace_fmt( TraceAggregatorPid, MessageFormat, FormatValues,
				TraceSeverity ) ->
	Message = io_lib:format( MessageFormat, FormatValues ),
	send_trace_helper( TraceAggregatorPid, Message, TraceSeverity ).


-else. % not tracing_activated:


% Avoids warnings:


-spec send_trace( pid(), ustring(), trace_severity() ) -> void().
send_trace( TraceAggregatorPid, Message, TraceSeverity ) ->
	deploy_trace( TraceAggregatorPid, Message, TraceSeverity ).


-spec send_trace_fmt( pid(), ustring(), text_utils:format_string(),
					  trace_severity() ) -> void().
send_trace_fmt( TraceAggregatorPid, MessageFormat, FormatValues,
				TraceSeverity ) ->
	Message = io_lib:format( MessageFormat, FormatValues ),
	deploy_trace( TraceAggregatorPid, Message, TraceSeverity ).


% Even when tracing is not activated, the most severe priorities are not
% filtered out:
%
deploy_trace( TraceAggregatorPid, Message, TraceSeverity=emergency ) ->
	send_trace_helper( TraceAggregatorPid, Message, TraceSeverity );

deploy_trace( TraceAggregatorPid, Message, TraceSeverity=alert ) ->
	send_trace_helper( TraceAggregatorPid, Message, TraceSeverity );

deploy_trace( TraceAggregatorPid, Message, TraceSeverity=critical ) ->
	send_trace_helper( TraceAggregatorPid, Message, TraceSeverity );

deploy_trace( TraceAggregatorPid, Message, TraceSeverity=error ) ->
	send_trace_helper( TraceAggregatorPid, Message, TraceSeverity );

deploy_trace( TraceAggregatorPid, Message, TraceSeverity=warning ) ->
	send_trace_helper( TraceAggregatorPid, Message, TraceSeverity );

deploy_trace( _TraceAggregatorPid, _Message, _TraceSeverity ) ->
	trace_disabled.


-endif. % tracing_activated



% Helper to actually send a trace.
send_trace_helper( TraceAggregatorPid, Message, TraceSeverity ) ->

	% Conditional echoing (even if generally already filtered beforehand):
	ErrorLikeSeverities = [ emergency, alert, critical, error, warning ],

	case lists:member( TraceSeverity, ErrorLikeSeverities ) of

		true ->
			io:format( "[deployment ~s] ~s~n", [ TraceSeverity, Message ] );

		false ->
			ok

	end,

	% We keep only the hostname, not the FQDN, otherwise the (last) dot in the
	% name would be interpreted as subcategory in the traces:
	TraceAggregatorPid ! { send,
		[ self(), "Deployment agent on "
			++ hd( string:tokens( net_adm:localhost(), "." ) ),
		  "Core.Deployment", _Tick=undefined, current_time_to_string(),
		  node(), "Standalone.Deployment", get_priority_for( TraceSeverity ),
		  Message ] }.


% Corresponds to time_utims:get_textual_timestamp/0:
%
% Returns the current time and date as a string, with correct format.
%
% Example: "14/04/2008 04:41:24".
%
current_time_to_string() ->
	{ { Year, Month, Day }, { Hour, Minute, Second } } = erlang:localtime(),
	lists:flatten( io_lib:format( "~B/~B/~B ~B:~B:~B",
		[ Day, Month, Year, Hour, Minute, Second ] ) ).




% Duplication section: the deployment_agent is the only one that is to be run
% standalone (pioneer module with almost no allowed prerequisite).

% Duplicated verbatim from class_TraceEmitter.erl:

% Returns the (numerical) priority associated to specified trace severity
% (i.e. emergency, alert, etc.).
%
% See also: its reciprocal get_severity_for/1.
%
-spec get_priority_for( trace_utils:trace_severity() ) ->
							  trace_utils:trace_priority().
% From most common to least:
get_priority_for( debug ) ->
	7;

get_priority_for( info ) ->
	6;

get_priority_for( notice ) ->
	5;

get_priority_for( warning ) ->
	4;

get_priority_for( error ) ->
	3;

get_priority_for( critical ) ->
	2;

get_priority_for( alert ) ->
	1;

get_priority_for( emergency ) ->
	0;

get_priority_for( Other ) ->
	throw( { unexpected_trace_priority, Other } ).

% 'void' not expected here.


% Duplicated almost verbatim (cf. module) from traces.erl:

% Replaces the current (probably default) logger handler with this Traces one
% (registered as 'default'), based on the specified trace aggregator.
%
-spec set_handler( aggregator_pid() ) -> void().
set_handler( AggregatorPid ) ->

	% Note: if set before actual deployment, trace_utils and all not ready yet:
	%trace_utils:debug_fmt( "Setting from deployment manager the default "
	%	"handler for logger to aggregator ~w, on node: ~s.",
	%	[ AggregatorPid, node() ] ),

	%trace_utils:debug_fmt( "Previous handler: ~p",
	%					   [ logger:get_handler_config( default ) ] ),

	TargetHandler = default,

	case logger:remove_handler( TargetHandler ) of

		ok ->
			ok;

		{ error, RemoveErrReason } ->
			throw( { unable_to_remove_log_handler, RemoveErrReason,
					 TargetHandler } )

	end,

	case logger:add_handler( _HandlerId=default, _Module=traces,
							 get_handler_config( AggregatorPid ) ) of

		ok ->
			ok;

		{ error, AddErrReason, TargetHandler } ->
			throw( { unable_to_set_traces_log_handler, AddErrReason,
					 TargetHandler } )

	end.



% Returns the (initial) configuration of the Traces logger handler, branching to
% the specified trace aggregator.
%
-spec get_handler_config( aggregator_pid() ) -> logger:handler_config().
get_handler_config( AggregatorPid ) ->

	#{ config => AggregatorPid
	   % Defaults:
	   % level => all,
	   % filter_default => log | stop,
	   % filters => [],
	   % formatter => {logger_formatter, DefaultFormatterConfig}

	   % Set by logger:
	   %id => HandlerId
	   %module => Module
	 }.



% Duplicated verbatim from naming_utils:


% Waits (up to 10 seconds) until specified name is globally registered.
%
% Returns the resolved PID, or throws {global_registration_waiting_timeout,
% Name}.
%
%-spec wait_for_global_registration_of( registration_name() ) -> pid().
wait_for_global_registration_of( Name ) ->
	wait_for_global_registration_of( Name, _Seconds=10 ).


wait_for_global_registration_of( Name, _Seconds=0 ) ->
	throw( { global_registration_waiting_timeout, Name } );

wait_for_global_registration_of( Name, SecondsToWait ) ->
	case global:whereis_name( Name ) of

		undefined ->
			timer:sleep( 1000 ),
			wait_for_global_registration_of( Name, SecondsToWait-1 );

		Pid ->
			Pid

	end.
