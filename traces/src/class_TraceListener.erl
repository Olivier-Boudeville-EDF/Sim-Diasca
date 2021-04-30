% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-Traces library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: July 1, 2007.
%
-module(class_TraceListener).


-define( class_description, "Trace listener, similar to a remote trace "
		 "supervisor. "
		 "This version just uses LogMX (http://logmx.com) to track the default "
		 "execution trace file, which will be synchronized automatically: "
		 "history will be retrieved under a zipped form from the aggregator, "
		 "and next traces will be sent directly to this listener as well as to "
		 "the aggregator."
		 "So the corresponding trace aggregator must have been run with the "
		 "LogMX-compliant trace type beforehand, i.e. advanced_traces.").


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).


% Describes the class-specific attributes:
-define( class_attributes, [

	{ trace_filename, file_utils:file_path(),
	  "the name of the file where traces are to be stored (ex: *.traces)" },

	{ trace_file, file_utils:file_path(),
	  "the actual file where traces are written" },

	{ trace_aggregator_pid, aggregator_pid(),
	  "the PID of the supervised trace aggregator" },

	{ temp_dir, file_utils:directory_name(), "the name of the directory where "
	  "the compressed trace archive will be stored" },

	{ supervision_waiter_pid, maybe( pid() ),
	  "the PID of the helper process (if any) in charge of waiting for the "
	  "trace interface to be closed" },

	{ close_listener_pid, maybe( pid() ),
	  "the PID of the process (if any) to notify whenever this listener is "
	  "to terminate (typically so that the calling application can itself "
	  "terminate afterwards)" } ] ).


% The PID of a trace listener:
-type listener_pid() :: pid().

-export_type([ listener_pid/0 ]).


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


-define( LogPrefix, "[Trace Listener]" ).


%-define( LogOutput( Message, Format ), io:format( Message, Format ) ).
-define( LogOutput( Message, Format ), void ).


% Shorthand:
-type aggregator_pid() :: class_TraceAggregator:aggregator_pid().



% Implementation notes:
%
% In some (presumably rare) cases, LogMX may display a message twice.
%
% To compare traces for equality, one should preferably compare directly
% *.traces files.

% We cannot block the listener process with a synchronous execution of LogMX, as
% this would prevent the listener to receive new trace messages. So we shall
% launch LogMX in the background. However then the closing of LogMX cannot be
% detected anymore, and the listener will not shutdown automatically once local
% supervision is over. To solve this, LogMX is now launched on a separate
% process, waiting (synchronously) for LogMX, sending a message back to the
% listener when closed. Then the listener can unregister from the aggregator
% (important) and terminate.


% We want the trace listeners to have the *exact* same traces as the aggregator.



% Constructs a new trace listener.
%
% TraceAggregatorPid is the PID of the trace aggregator to which this listener
% will be synchronized.
%
-spec construct( wooper:state(), aggregator_pid(), pid() ) -> wooper:state().
construct( State, TraceAggregatorPid, CloseListenerPid ) ->

	trace_utils:notice_fmt( "~ts Creating a trace listener whose PID is ~w, "
		"synchronized on trace aggregator ~w.",
		[ ?LogPrefix, self(), TraceAggregatorPid ] ),

	trace_utils:debug_fmt( "~ts Requesting from aggregator a trace "
						   "synchronization.", [ ?LogPrefix ] ),

	TraceAggregatorPid ! { addTraceListener, self() },

	% We used to rely on basic ZIP sent over Erlang messages:
	%receive
	%
	%	 { trace_sending, Bin, TraceFilename } ->
	%
	%			% Allows to run for the same directory as aggregator:
	%			ListenerTraceFilename = "Listener-" ++ TraceFilename,
	%
	%           file_utils:zipped_term_to_unzipped_file( Bin,
	%										 ListenerTraceFilename ),
	%	{ trace_sending, ErrorReason } ->
	%
	%		trace_utils:error_fmt(
	%           "~ts Trace listener cannot listen to current trace "
	%			"aggregator, as this aggregator does not use "
	%			"LogMX-based traces.", [ ?LogPrefix ] ),
	%
	%		throw( { cannot_listen_aggregator, TraceAggregatorPid,
	%				 ErrorReason } )

	% Now we prefer XZ + sendFile:

	% Currently we prefer using a temporary directory (it also allows to avoid
	% stepping on the source compressed file if running the listener from the
	% same directory as the aggregator - as it is the case for tests):
	%
	TempDir = file_utils:create_temporary_directory(),

	CompressedFilename = net_utils:receive_file( TraceAggregatorPid, TempDir ),

	ManagedState = manage_send_traces( CompressedFilename, State ),

	SetState = setAttributes( ManagedState, [
				{ trace_aggregator_pid, TraceAggregatorPid },
				{ temp_dir, TempDir },
				{ supervision_waiter_pid, undefined },
				{ close_listener_pid, CloseListenerPid } ] ),

	EndState = executeOneway( SetState, monitor ),

	%trace_utils:info_fmt( "~ts Trace listener created.", [ ?LogPrefix ] ),

	EndState.



% Constructs a new trace listener, whose listening sockets will have to be
% elected within the specified range of TCP ports.
%
% TraceAggregatorPid is the PID of the trace aggregator to which this listener
% will be synchronized.
%
-spec construct( wooper:state(), aggregator_pid(), net_utils:tcp_port(),
				 net_utils:tcp_port(), pid() ) -> wooper:state().
construct( State, TraceAggregatorPid, MinTCPPort, MaxTCPPort,
		   CloseListenerPid ) ->

	trace_utils:notice_fmt( "~ts Creating a trace listener whose PID is ~w, "
		"synchronized on trace aggregator ~w, using a TCP listening port "
		"in the [~B,~B[ range.",
		[ ?LogPrefix, self(), TraceAggregatorPid, MinTCPPort, MaxTCPPort ] ),

	trace_utils:debug_fmt(
	  "~ts Requesting from aggregator a trace synchronization.",
	  [ ?LogPrefix ] ),

	TraceAggregatorPid ! { addTraceListener, self() },

	% See comments in construct/3/

	TempDir = file_utils:create_temporary_directory(),

	CompressedFilename = net_utils:receive_file( TraceAggregatorPid, TempDir,
												 MinTCPPort, MaxTCPPort ),

	ManagedState = manage_send_traces( CompressedFilename, State ),

	SetState = setAttributes( ManagedState, [
				{ trace_aggregator_pid, TraceAggregatorPid },
				{ temp_dir, TempDir },
				{ supervision_waiter_pid, undefined },
				{ close_listener_pid, CloseListenerPid } ] ),

	EndState = executeOneway( SetState, monitor ),

	%trace_utils:info_fmt( "~ts Trace listener created.", [ ?LogPrefix ] ),

	EndState.




% (construction helper)
manage_send_traces( CompressedFilename, State ) ->

	TraceFilename = file_utils:decompress( CompressedFilename,
										   _CompressionFormat=xz ),

	file_utils:remove_file( CompressedFilename ),

	%trace_utils:info_fmt( "~ts Received from aggregator a trace "
	%					   "synchronization for file '~ts', reused for "
	%					   "later traces.", [ ?LogPrefix, TraceFilename ] ),

	% Will write in it newly received traces (sent through messages); now
	% preferring the (more efficient) raw mode:
	%
	File = file_utils:open( TraceFilename,
				[ append, raw, { delayed_write, _Size=1024, _Delay=200 } ] ),

	setAttributes( State, [ { trace_filename, TraceFilename },
							{ trace_file, File } ] ).




% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	trace_utils:notice_fmt( "~ts Deleting trace listener.", [ ?LogPrefix ] ),

	% Important message, to avoid loading the aggregator with sendings to
	% defunct listeners:
	%
	?getAttr(trace_aggregator_pid) ! { removeTraceListener, self() },

	file_utils:close( ?getAttr(trace_file) ),

	file_utils:remove_file( ?getAttr(trace_filename) ),

	file_utils:remove_directory( ?getAttr(temp_dir) ),

	case ?getAttr(close_listener_pid) of

		Pid when is_pid( Pid ) ->
			%trace_utils:notice( "Notifying close listener of deletion." ),
			Pid ! { trace_listening_finished, self() };

		_ ->
			ok

	end,

	trace_utils:notice_fmt( "~ts Trace listener deleted.", [ ?LogPrefix ] ),

	% Allow chaining:
	State.



% Methods section.



% Triggers an asynchronous supervision (trace monitoring).
%
% Will return immediately.
%
% Note: directly inspired from class_TraceSupervisor.erl, for monitor/1 and
% blocking_monitor/1.
%
-spec monitor( wooper:state() ) -> oneway_return().
monitor( State ) ->

	Filename = ?getAttr(trace_filename),

	case file_utils:is_existing_file( Filename ) of

		true ->
			ok;

		false ->
			trace_utils:error_fmt( "class_TraceListener:monitor/1 "
				"unable to find trace file '~ts'.", [ Filename ] ),
			trace_file_not_found

	end,

	trace_utils:notice_fmt( "~ts Trace listener will monitor file '~ts' "
							"with LogMX now.", [ ?LogPrefix, Filename ] ),

	Self = self(),

	WaiterPid = ?myriad_spawn_link( fun() ->

		% Blocking this waiter process (logmx.sh must be found in the PATH):
		case system_utils:run_command(
				executable_utils:get_default_trace_viewer_path() ++ " '"
				++ Filename ++ "'" ) of

			{ _ExitCode=0, _Output } ->
				trace_utils:notice_fmt(
					"~ts Trace listener ended the monitoring of '~ts'.",
					[ ?LogPrefix, Filename ] );

			{ ExitCode, ErrorOutput } ->
				trace_utils:error_fmt( "The trace listening failed "
					"(error code: ~B): ~ts.", [ ExitCode, ErrorOutput ] )

		end,

		% Unblock the listener:
		Self ! { onMonitoringOver, self() }

									end ),

	SupState = setAttribute( State, supervision_waiter_pid, WaiterPid ),

	wooper:return_state( SupState ).



% Registers a new pre-formatted trace in trace file.
%
% To be called by the trace aggregator.
%
-spec addTrace( wooper:state(), text_utils:bin_string() ) ->
						const_oneway_return().
addTrace( State, NewTrace ) ->

	% Write to file:

	% We used to rely on:

	%io:format( ?getAttr(trace_file), "~ts", [
	%					text_utils:binary_to_string( NewTrace ) ] ),

	% yet now the internal trace file is opened in raw mode (so there is no
	% intermediate process handling the I/O protocol), so:

	Content = text_utils:format( "~ts",
				[ text_utils:binary_to_string( NewTrace ) ] ),

	file:write( ?getAttr(trace_file), Content ),

	wooper:const_return().



% Callback triggered when the waiter process detected that the supervision tool
% has been closed.
%
-spec onMonitoringOver( wooper:state(), pid() ) -> const_oneway_return().
onMonitoringOver( State, WaiterPid ) ->

	% Check:
	WaiterPid = ?getAttr(supervision_waiter_pid),

	self() ! delete,

	wooper:const_return().



% Static section:


% Creates the trace listener that will synchronize itself to the specified
% aggregator.
%
-spec create( aggregator_pid() ) -> static_return( listener_pid() ).
create( AggregatorPid ) ->

	% No link here, not wanting to take down the whole system because of a
	% listener:
	%
	ListenerPid = new( AggregatorPid, _CloseListenerPid=undefined ),

	wooper:return_static( ListenerPid ).
