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
-module(class_TraceSupervisor).


-define( class_description, "Trace supervisor; this version relies on the "
		 "advanced traces, often monitored thanks to LogMX (http://logmx.com) "
		 "to track the default execution trace file, expected to be locally "
		 "available on disk." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).


% Describes the class-specific attributes:
-define( class_attributes, [

	{ trace_filename, file_utils:bin_file_path(),
	  "the name of the file in which traces are to be read "
	  "(ex: <<\"foobar.traces\">>" },

	{ trace_type, trace_type(),
	  "the type of traces to be written (ex: advanced_traces)" },

	% Needed if wanting to wait for the trace file to exist before launching the
	% supervision tool:
	%
	{ trace_aggregator_pid, aggregator_pid(),
	  "the PID of the supervised trace aggregator" } ] ).


-type supervisor_pid() :: pid().

-type supervisor_outcome() :: 'no_trace_supervisor_wanted' | supervisor_pid().

-export_type([ supervisor_pid/0, supervisor_outcome/0 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% For TraceExtension:
-include("traces.hrl").


% For the default trace filename:
-include("class_TraceAggregator.hrl").


-define( LogPrefix, "[Trace Supervisor]" ).


% Use global:registered_names() to check supervisor presence.


%-define( LogOutput( Message, Format ),
%  trace_utils:debug_fmt( Message, Format ) ).

-define( LogOutput(Message,Format), void ).



% Total width (expressed as a number of characters) of a line of log, in text
% mode (text_traces).
%
-define( TextWidth, 110 ).


% Shorthands:

-type file_name() :: file_utils:file_name().
-type bin_file_name() :: file_utils:bin_file_name().
-type any_file_path() :: file_utils:any_file_path().

-type aggregator_pid() :: class_TraceAggregator:aggregator_pid().

-type trace_supervision_type() :: traces:trace_supervision_type().



% Constructs a new trace supervisor:
%
% - {TraceFilename, TraceType, MaybeTraceAggregatorPid}:
%
%   - TraceFilename is the name of the file whence traces should be read
%
%   - TraceType the type of traces to expect (ex: advanced_traces, text_traces)
%
%   - MaybeTraceAggregatorPid is the PID of the trace aggregator to wait for (if
%   any)
%
% - MonitorNow tells whether the supervision should begin immediately (if true)
% or only when the monitor method is called (if false)
%
% - MaybeWaitingPid tells whether the monitoring should be blocking: if set to a
% PID, this supervisor will notify the corresponding process whenever the
% monitoring will be done; otherwise (if set to 'undefined') nothing will be
% done then
%
-spec construct( wooper:state(), { bin_file_name(), trace_supervision_type(),
								   maybe( aggregator_pid() ) },
				 boolean(), maybe( pid() ) ) -> wooper:state().
construct( State, { TraceFilename, TraceType, MaybeTraceAggregatorPid },
		   MonitorNow, MaybeWaitingPid ) ->

	trace_utils:debug_fmt( "~ts Creating a trace supervisor, whose PID is ~w "
		"(trace filename: '~ts', trace type: '~ts', monitor now: ~w, "
		"blocking: ~w).",
		[ ?LogPrefix, self(), TraceFilename, TraceType, MonitorNow,
		  MaybeWaitingPid ] ),

	NewState = setAttributes( State, [
		{ trace_filename, TraceFilename },
		{ trace_type, TraceType },
		{ trace_aggregator_pid, MaybeTraceAggregatorPid } ] ),

	case MaybeTraceAggregatorPid of

		AggPid when is_pid( AggPid ) ->

			% We have a PID; avoid the race condition that could happen if the
			% trace viewer (ex: LogMX) was launched before a first trace was
			% written by the aggregator in the trace file:

			%trace_utils:debug(
			%	   "(trace supervisor waiting for the trace aggregator)" ),

			AggPid ! { requestReadyNotification, [], self() },

			receive

				{ wooper_result, trace_file_ready } ->
					%trace_utils:debug( "Trace aggregator answered." ),
					ok

			end;

		undefined ->

			%trace_utils:debug(
			%	   "(trace supervisor not waiting any trace aggregator)" ),

			% Supposedly no race condition is to be feared here:
			ok

	end,

	%trace_utils:debug_fmt( "Supervisor associated to aggregator ~w.",
	%					   [ MaybeTraceAggregatorPid ] ),

	EndState = case MonitorNow of

		true ->

			case MaybeWaitingPid of

				WaitingPid when is_pid( WaitingPid ) ->

					% Pattern-match the result of in-place invocation:
					%
					% ('monitor_ok' used to be temporarily replaced by '_' due
					% to the LogMX issue with java_security_PrivilegedAction)
					%
					case executeRequest( NewState, blocking_monitor ) of

						{ RequestState, monitor_ok } ->

							%trace_utils:debug(
							%  "Blocking supervisor received monitor_ok." ),

							% Sends back to the caller:
							WaitingPid ! { wooper_result, monitor_ok },
							self() ! delete,
							RequestState;

						{ AnyState, monitor_failed } ->

							trace_utils:warning(
							  "Blocking supervisor received monitor_failed." ),

							% If needing to ignore a non-significant error from
							% the supervision tool:
							%
							WaitingPid ! { wooper_result, monitor_ok },
							self() ! delete,
							AnyState

							%throw( blocking_monitoring_failed )

					end;

				undefined ->
					% Non-blocking, handled after the constructor:
					%trace_utils:debug( "Non-blocking supervisor." ),
					self() ! monitor,
					NewState

			end;

		false ->
			%trace_utils:debug( "Supervisor not monitoring now." ),
			NewState

	end,

	%trace_utils:debug_fmt( "~ts Supervisor created.", [ ?LogPrefix ] ),

	EndState.




% Methods section.


% Triggers a non-blocking supervision (trace monitoring).
% Will return immediately.
%
-spec monitor( wooper:state() ) -> const_oneway_return().
monitor( State ) ->

	case ?getAttr(trace_type) of

		{ text_traces, pdf } ->
			trace_utils:notice_fmt( "~ts Supervisor has nothing to monitor, "
				"as the PDF trace report will be generated only on "
				"execution termination.", [ ?LogPrefix ] ),
			wooper:const_return();


		_Other ->

			{ Command, ActualFilename } = get_viewer_settings( State ),

			case file_utils:is_existing_file( ActualFilename ) of

				true ->
					ok;

				false ->
					trace_utils:error_fmt( "class_TraceSupervisor:monitor "
					  "unable to find trace file '~ts'.", [ ActualFilename ] ),
					throw( { trace_file_not_found, ActualFilename } )

			end,

			trace_utils:notice_fmt(
			  "~ts Supervisor will monitor file '~ts' now, "
			  "with '~ts'.", [ ?LogPrefix, ActualFilename, Command ] ),

			Cmd = Command ++ " '" ++ ActualFilename ++ "'",

			% Non-blocking (command must be found in the PATH):
			system_utils:run_background_command( Cmd ),

			wooper:const_return()

	end.



% Triggers a blocking supervision (trace monitoring).
%
% Will block until the viewer window is closed by the user.
%
-spec blocking_monitor( wooper:state() ) ->
							const_request_return( 'monitor_ok' ).
blocking_monitor( State ) ->

	case ?getAttr(trace_type) of

		{ text_traces, pdf } ->
			trace_utils:notice_fmt( "~ts Supervisor has nothing to monitor, "
				"as the PDF trace report will be generated only on "
				"execution termination.", [ ?LogPrefix ] ),
			wooper:const_return_result( monitor_ok );

		_Other ->

			{ Command, ActualFilename } = get_viewer_settings( State ),

			CurrentDir = file_utils:get_current_directory(),

			case file_utils:is_existing_file( ActualFilename ) of

				true ->
					%trace_utils:debug_fmt(
					%  "Found actual trace filename '~ts' from '~ts'.",
					%  [ ActualFilename, CurrentDir ] ),
					ok;

				false ->
					trace_utils:error_fmt(
					  "class_TraceSupervisor:blocking_monitor unable to find "
					  "trace file '~ts' (while current directory is '~ts').",
					  [ ActualFilename, CurrentDir ] ),
					throw( { trace_file_not_found, ActualFilename } )

			end,

			trace_utils:notice_fmt( "~ts Supervisor will monitor file '~ts' "
				"now with '~ts', blocking until the user closes the viewer "
				"window.", [ ?LogPrefix, ActualFilename, Command ] ),

			% Blocking:
			case system_utils:run_command(
				   Command ++ " '" ++ ActualFilename ++ "'",
				   system_utils:get_standard_environment(),
				   _WorkingDir=CurrentDir ) of

				{ _ExitStatus=0, _Output } ->
					trace_utils:notice_fmt(
					  "~ts Supervisor ended monitoring of '~ts'.",
					  [ ?LogPrefix, ActualFilename ] ),
					wooper:const_return_result( monitor_ok );

				{ ExitStatus, _ErrorOutput="" } ->
					trace_utils:error_fmt( "The monitoring of trace supervisor "
						"failed (error ~B).", [ ExitStatus ] ),

					% Must not be a blocking error:
					%wooper:const_return_result( monitor_failed )
					%throw( trace_supervision_failed )
					wooper:const_return_result( monitor_ok );

				{ ExitStatus, ErrorOutput } ->
					trace_utils:error_fmt(
						"The monitoring of trace supervisor failed "
						"(error ~B): '~ts'.", [ ExitStatus, ErrorOutput ] ),

					% Must not be a blocking error:
					%wooper:const_return_result( monitor_failed )
					%throw( trace_supervision_failed )
					wooper:const_return_result( monitor_ok )

			end

	end.




% Static section.


% Creates the trace supervisor with default settings regarding trace filename,
% start mode (immediate here, not deferred) and trace type (advanced ones here,
% not text based), with no PID specified for the trace aggregator, and blocks
% until closed.
%
% See create/5 for a more in-depth explanation of the parameters.
%
-spec create() -> static_return( supervisor_pid() ).
create() ->

	SupervisorPid = create( _MaybeWaitingPid=undefined ),

	wooper:return_static( SupervisorPid ).



% Creates the trace supervisor with default settings regarding trace
% filename, start mode (immediate here, not deferred) and trace type (advanced
% ones here, not text based), with no PID specified for the trace aggregator.
%
% Once the trace monitoring is over, will notify any specified waiting process.
%
% See create/5 for a more in-depth explanation of the parameters.
%
-spec create( maybe( pid() ) ) -> static_return( supervisor_pid() ).
create( MaybeWaitingPid ) ->

	SupervisorPid = create( MaybeWaitingPid, ?trace_aggregator_filename ),

	wooper:return_static( SupervisorPid ).



% Creates the trace supervisor with default settings regarding start mode
% (immediate here, not deferred) and trace type (advanced ones here, not text
% based), with no PID specified for the trace aggregator.
%
% Once the trace monitoring is over, will notify any specified waiting process.
%
% See create/5 for a more in-depth explanation of the parameters.
%
-spec create( maybe( pid() ), any_file_path() ) ->
					static_return( supervisor_pid() ).
create( MaybeWaitingPid, TraceFilename ) ->

	SupervisorPid = create( MaybeWaitingPid, TraceFilename,
		 _TraceType=advanced_traces, _TraceAggregatorPid=undefined ),

	wooper:return_static( SupervisorPid ).




% Creates the trace supervisor, with default settings regarding start mode
% (immediate here, not deferred).
%
% Once the trace monitoring is over, will notify any specified waiting process.
%
% See create/5 for a more in-depth explanation of the parameters.
%
-spec create( maybe( pid() ), any_file_path(), trace_supervision_type(),
			  maybe( aggregator_pid() ) ) -> static_return( supervisor_pid() ).
create( MaybeWaitingPid, TraceFilename, TraceType, TraceAggregatorPid ) ->

	SupervisorPid = create( MaybeWaitingPid, _MonitorNow=true, TraceFilename,
							TraceType, TraceAggregatorPid ),

	wooper:return_static( SupervisorPid ).




% Creates a trace supervisor:
%
% - MaybeWaitingPid, if set to a PID, will notify the corresponding process once
% the trace monitoring is over
%
% - MonitorNow tells whether the monitoring should start immediately or only
% when a monitor/blocking_monitor method is called
%
% - TraceFilename the trace file to monitor
%
% - TraceType the expected type of the traces (ex: advanced_traces, text_traces)
%
% - MaybeTraceAggregatorPid is either the PID of the trace aggregator, or the
% 'undefined' atom
%
% Returns either the PID of the created supervisor or, if blocking (hence the
% supervisor being dead by design when this creation returns), 'undefined'.
%
-spec create( maybe( pid() ), boolean(), any_file_path(),
			  trace_supervision_type(), maybe( aggregator_pid() ) ) ->
					static_return( maybe( supervisor_pid() ) ).
create( MaybeWaitingPid, MonitorNow, TraceFilename, TraceType,
		MaybeTraceAggregatorPid ) ->

	BinTraceFilename = text_utils:ensure_binary( TraceFilename ),

	SupervisorPid = new_link(
		{ BinTraceFilename, TraceType, MaybeTraceAggregatorPid },
		MonitorNow, MaybeWaitingPid ),

	MaybeSupervisorPid = case MaybeWaitingPid of

		undefined ->
			SupervisorPid;

		% Then by design the process is terminated:
		_Pid ->
			undefined

	end,

	wooper:return_static( MaybeSupervisorPid ).



% Inits a trace supervisor; especially useful when the trace supervisor cannot
% be created at the same time as the trace aggregator (ex: if the trace filename
% is to change at runtime).
%
% Use the --batch option (ex: erl --batch, or with the make system 'make
% MY_TARGET CMD_LINE_OPT="--batch") to disable the use of the trace supervisor.
%
-spec init( file_name(), trace_supervision_type(), aggregator_pid() ) ->
				static_return( supervisor_outcome() ).
init( TraceFilename, TraceType, TraceAggregatorPid ) ->

	SupOutcome = init( TraceFilename, TraceType, TraceAggregatorPid,
					   _MaybeWaitingPid=undefined ),

	wooper:return_static( SupOutcome ).



% Inits a trace supervisor; especially useful when the trace supervisor cannot
% be created at the same time as the trace aggregator (ex: if the trace filename
% is to change at runtime).
%
% Use the --batch option (ex: erl --batch, or with the make system 'make
% MY_TARGET CMD_LINE_OPT="--batch") to disable the use of the trace supervisor.
%
-spec init( file_name(), trace_supervision_type(), aggregator_pid(),
			maybe( pid() ) ) -> static_return( supervisor_outcome() ).
init( TraceFilename, TraceType, TraceAggregatorPid, MaybeWaitingPid ) ->

	%trace_utils:info_fmt( "Initializing trace supervisor for file '~ts' and "
	%						"trace type ~p.", [ TraceFilename, TraceType ] ),

	% By default (with no specific option) a synchronous supervisor is wanted
	% (wait for its launch to complete):

	case executable_utils:is_batch() of

		true ->
			% Option specified to disable the supervisor:
			trace_utils:notice_fmt( "Application trace file is '~ts'; no "
				"interactive supervision requested.", [ TraceFilename ] ),
			wooper:return_static( no_trace_supervisor_wanted );

		false ->
			% Default: a trace supervisor is used.
			%trace_utils:notice( "Supervisor enabled." ),

			SupervisorPid = create( MaybeWaitingPid, TraceFilename,
									TraceType, TraceAggregatorPid ),

			%trace_utils:debug( "Waiting for trace supervisor to be closed." )

			wooper:return_static( SupervisorPid )

	end.



% Waits, usually at the end of a test, for any trace supervisor to be closed by
% the user.
%
-spec wait_for() -> static_void_return().
wait_for() ->

	case executable_utils:is_batch() of

		true ->
			% No supervisor was launched.
			% Let live the system for some time instead:
			system_utils:await_output_completion();

		false ->
			actual_wait_for()

	end,

	wooper:return_static_void().



% (helper)
-spec actual_wait_for() -> void().
actual_wait_for() ->

	% A supervisor must be waited for here:
	trace_utils:notice(
	  "(waiting for the user to stop the trace supervision)" ),

	receive

		{ wooper_result, monitor_ok } ->

			%trace_utils:notice(
			%    "Notification received from supervisor." ),

			% Not {test,app}_notice, as this function is used in both contexts:
			class_TraceEmitter:send_standalone( notice,
				"Traces successfully monitored." )

	end.



% Helper section.


% Returns the path of the tool and the corresponding file that should be used to
% monitor traces.
%
% (helper)
%
-spec get_viewer_settings( wooper:state() ) ->
								{ file_utils:path(), file_name() }.
get_viewer_settings( State ) ->

	Filename = text_utils:binary_to_string( ?getAttr(trace_filename) ),

	case ?getAttr(trace_type) of

		advanced_traces ->
			{ executable_utils:get_default_trace_viewer_path(), Filename };

		{ text_traces, text_only } ->
			{ executable_utils:get_default_wide_text_viewer_path( ?TextWidth ),
			  Filename };

		{ text_traces, pdf } ->

			PdfTargetFilename = file_utils:replace_extension( Filename,
													?TraceExtension, ".pdf" ),

			{ executable_utils:get_default_pdf_viewer_path(),
			  PdfTargetFilename }

	end.
