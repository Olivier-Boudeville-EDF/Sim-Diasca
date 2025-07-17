% Copyright (C) 2023-2025 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
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
% Creation date: Thursday, May 18, 2023.

-module(process_utils).

-moduledoc """
Gathering of various convenient facilities regarding **(Erlang) processes**.

See process_utils_test.erl for the corresponding test.
""".



-export([ spawn_message_queue_monitor/1, spawn_message_queue_monitor/2,
		  spawn_message_queue_monitor/4,

		  set_label/1, get_label/1 ]).



-doc "The PID of a monitoring process.".
-type monitor_pid() :: pid().



% Mostly defined here to remember it:
%
% (stored with the '$process_label' key in the process dictionary)
%
-doc """
A label set on a given process.

Helps the debugging of unregistered processes, notably when they are not able
anymore to process messages.

Many tools (observer, logger, crash reporter, etc.) will exploit this
information afterwards.
""".
-type process_label() :: term().



-export_type([ monitor_pid/0, process_label/0 ]).


% Implementation notes:
%
% The proc_lib module is of interest here.



% Type shorthands:

-type any_string() :: text_utils:any_string().
-type bin_string() :: text_utils:bin_string().

-type milliseconds() :: time_utils:milliseconds().

-type count() :: basic_utils:count().



% For myriad_spawn_link/1:
-include("spawn_utils.hrl").



-doc """
Spawns a process monitoring the length of the message queue of the specified
process: returns the PID of an helper process that displays a warning message
if, during a periodic sampling each two seconds, this length is above 1000.

The 'terminate' atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_message_queue_monitor( pid() ) -> monitor_pid().
spawn_message_queue_monitor( MonitoredPid ) ->
	spawn_message_queue_monitor( MonitoredPid,
		_MaybeMonitoredProcessDescStr=undefined ).



-doc """
Spawns a process monitoring the length of the message queue of the specified
process: returns the PID of an helper process that displays a warning message
(with any description thereof supplied) if, during a periodic sampling each two
seconds, this length is above 1000.

The 'terminate' atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_message_queue_monitor( pid(), option( any_string() ) ) ->
											monitor_pid().
spawn_message_queue_monitor( MonitoredPid, MaybeMonitoredProcessDesc ) ->
	spawn_message_queue_monitor( MonitoredPid, MaybeMonitoredProcessDesc,
		_MsgThreshold=1000, _SamplingPeriod=2000 ).



-doc """
Spawns a process monitoring the length of the message queue of the specified
process: returns the PID of an helper process that displays a warning message
(with any description thereof supplied) if, during a periodic sampling, this
length is above the specified threshold.

The 'terminate' atom shall be sent to the returned PID in order to terminate the
corresponding monitoring process.
""".
-spec spawn_message_queue_monitor( pid(), option( any_string() ),
			count(), milliseconds() ) -> monitor_pid().
spawn_message_queue_monitor( MonitoredPid, MaybeMonitoredProcessDesc,
		MsgThreshold, SamplingPeriod ) ->

	BinProcDesc = case MaybeMonitoredProcessDesc of

		undefined ->
			text_utils:bin_format( "process ~w", [ MonitoredPid ] );

		AnyDesc ->
			text_utils:bin_format( "the '~ts' process (~w)",
								   [ AnyDesc, MonitoredPid ] )

	end,

	MonitorPid = ?myriad_spawn_link(
		fun() ->
			message_queue_monitor_main_loop( MonitoredPid, BinProcDesc,
											 MsgThreshold, SamplingPeriod )
		end ),

	trace_utils:debug_fmt( "Spawned ~w as message-queue monitor of "
		"~ts; threshold for message-queue length: ~B; sampling period: ~ts.",
		[ MonitorPid, BinProcDesc, MsgThreshold,
		  time_utils:duration_to_string( SamplingPeriod ) ] ),

	MonitorPid.



-spec message_queue_monitor_main_loop ( pid(), bin_string(), count(),
										milliseconds() ) -> no_return().
message_queue_monitor_main_loop( MonitoredPid, BinProcDesc, MsgThreshold,
								 SamplingPeriod ) ->

	receive

		terminate ->
			trace_utils:debug_fmt( "(message-queue monitor for ~ts (~w) "
				"terminated)", [ BinProcDesc, MonitoredPid ] ),

			terminated;


		UnexpectedMsg ->
			trace_utils:warning_fmt( "Unexpected message received by message "
				"queue monitor, thus ignored: ~p", [ UnexpectedMsg ] ),

			message_queue_monitor_main_loop( MonitoredPid, BinProcDesc,
											 MsgThreshold, SamplingPeriod )

	after SamplingPeriod ->

			{ message_queue_len, QueueLen } =
				erlang:process_info( MonitoredPid, message_queue_len ),

			QueueLen > MsgThreshold andalso
				trace_utils:warning_fmt( "The length of the message queue "
					"of ~ts is ~B (thus exceeds the ~w threshold).",
					[ BinProcDesc, QueueLen, MsgThreshold ] ),

			message_queue_monitor_main_loop( MonitoredPid, BinProcDesc,
											 MsgThreshold, SamplingPeriod )

	end.



-doc "Sets the label of the current process.".
-spec set_label( process_label() ) -> void().
set_label( ProcessLabel ) ->
	proc_lib:set_label( ProcessLabel ).



-doc "Gets the label (if any) of the specified process.".
-spec get_label( pid() ) -> option( process_label() ).
get_label( Pid ) ->
	proc_lib:set_label( Pid ).
