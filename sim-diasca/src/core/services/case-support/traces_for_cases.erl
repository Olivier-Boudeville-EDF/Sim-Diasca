% Copyright (C) 2012-2021 EDF R&D
%
% This file is part of Sim-Diasca.
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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: July 1, 2007.


% Directly obtained from traces_for_tests.erl.


% This module gathers all code that allows to lighten the trace macros for
% cases.
%
-module(traces_for_cases).


-export([ case_start/2, case_start/3,
		  case_stop/3, case_immediate_stop/2,
		  case_stop_on_shell/2 ]).


-define( trace_emitter_categorization, "case.life-cycle" ).


% For case_notice_fmt and al:
-include("traces_case_header.hrl").


% For trace_aggregator_pid():
-include("engine_common_defines.hrl").


% For TraceType:
-include_lib("traces/include/traces.hrl").

-include_lib("traces/include/class_TraceSupervisor.hrl").


-include("traces_case_footer.hrl").



% Shorthands:

-type module_name() :: basic_utils:module_name().

-type initialize_supervision() ::
		class_TraceAggregator:initialize_supervision().



% To be called from the counterpart macro.
%
% Here we disable explicitly the trapping of EXIT events, as a function run
% through "erl -eval" (like our cases) or through "erl -run" will be executed in
% a process which will silently trap EXIT events, which would mean that the
% crash of any process created from the case, even thanks to spawn_link, would
% most probably remain unnoticed (just leading to an EXIT message happily
% sitting in the mailbox of the case process).
%
-spec case_start( module_name(), initialize_supervision() ) -> aggregator_pid().
case_start( ModuleName, InitTraceSupervisor ) ->

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	%sim_diasca:start_for_test(),

	% Here, no trace type has been specified, looking for any option specified
	% on the command-line:

	% The actual option is: "--trace-type XXX":
	TraceType =
		case shell_utils:get_command_arguments_for_option( '-trace-type' ) of

		undefined ->
			trace_utils:info( "No trace type specified, defaulting "
							  "to advanced type." ),
			advanced_traces;

		% Single option expected:
		[ [ SpecifiedTraceType ] ] ->
			case SpecifiedTraceType of

				"advanced" ->
					advanced_traces;

				"text" ->
					{ text_traces, text_only };

				"pdf" ->
					{ text_traces, pdf };

				Other ->
					trace_utils:error_fmt(
						"Unexpected trace type specified: '~p'.", [ Other ] ),
					throw( { unexpected_trace_type_specified, Other } )

			end;

		OtherTraceTypeArg ->
			trace_utils:error_fmt( "Invalid trace type option specified: '~p'.",
								   [ OtherTraceTypeArg ] ),
			throw( { invalid_trace_type_option, OtherTraceTypeArg } )

	end,

	case_start( ModuleName, InitTraceSupervisor, TraceType ).



% To be called from the counterpart macro.
%
% Here we disable explicitly the trapping of EXIT events, as a function run
% through "erl -eval" (like our cases) or through "erl -run" will be executed in
% a process that will silently trap EXIT events, which would mean that the crash
% of any process created from the case, even thanks to spawn_link, would most
% probably remain unnoticed (just leading to an EXIT message happily sitting in
% the mailbox of the case process).
%
% Returns TraceAggregatorPid.
%
-spec case_start( module_name(), initialize_supervision(),
				  traces:trace_supervision_type() ) -> aggregator_pid().
% Clause generally not used by simulation cases:
case_start( ModuleName, _InitTraceSupervisor=true, TraceType ) ->

	% First jump to the other clause:
	TraceAggregatorPid = case_start( ModuleName, _InitTraceSuperv=false,
									 TraceType ),

	% For a simulation case, we do not consider to launch the trace supervisor
	% this early, as we would need to change its trace filename, which is not
	% supported on some back-ends (ex: LogMX).

	?case_notice_fmt( "Starting case ~ts.", [ ModuleName ] ),

	% So we trigger the supervisor launch by ourselves:
	case executable_utils:is_batch() of

		true ->
			%trace_utils:debug(
			%  "In batch mode, so no trace supervisor launched." ),
			ok;

		false ->
			%trace_utils:debug(
			%  "Not in batch mode, so launching trace supervisor." ),

			TraceAggregatorPid ! { launchTraceSupervisor, [], self() },
			receive

				{ wooper_result, _SupervisorPid } ->
					ok

			end

	end,

	TraceAggregatorPid;


% This is the clause typically directly called by actual simulation cases, as
% the trace supervisor is to be launched later, from sim_diasca:init/{1,2,3},
% when the simulation name (hence the one of the trace file as well) is known.
%
case_start( ModuleName, _InitTraceSupervisor=false, TraceType ) ->

	% See comments above about:
	erlang:process_flag( trap_exit, false ),

	% Create first, synchronously (to avoid race conditions), a trace aggregator
	% (false is to specify a non-private i.e. global aggregator).
	%
	% Race conditions could occur at least with trace emitters (they would
	% create their own aggregator, should none by found) and with trace
	% supervisor (which expects a trace file to be already created at start-up).

	CaseIsBatch = executable_utils:is_batch(),

	TraceFilename = traces:get_trace_filename( ModuleName ),

	% Not wanting the trace aggregator to initialize the trace supervisor, as
	% otherwise the latter would notify that its monitoring is over to the
	% former, whereas we want instead the calling process (i.e. the case) to be
	% notified of it (see case_stop/2):
	%
	TraceAggregatorPid = class_TraceAggregator:synchronous_new_link(
		TraceFilename, TraceType, ?TraceTitle,
		_MaybeRegistrationScope=global_only, CaseIsBatch,
		_AggInitTraceSupervisor=false ),

	?case_notice_fmt( "Starting case ~ts.", [ ModuleName ] ),

	TraceAggregatorPid.





% Stopping of cases.


% To be called from the counterpart macro.
-spec case_stop( module_name(), trace_aggregator_pid(), boolean() ) ->
						no_return().
case_stop( ModuleName, TraceAggregatorPid, WaitForTraceSupervisor ) ->

	%trace_utils:info_fmt( "Case stopping (aggregator: ~w, wait supervisor: "
	%    "~ts).", [ TraceAggregatorPid, WaitForTraceSupervisor] ),

	case WaitForTraceSupervisor of

		true ->
			class_TraceSupervisor:wait_for();

		false ->
			ok

	end,

	%trace_utils:info( "Going for immediate stop." ),

	% Stop trace sent there:
	case_immediate_stop( ModuleName, TraceAggregatorPid ).



% To be called from the counterpart macro.
-spec case_immediate_stop( module_name(), trace_aggregator_pid() ) ->
								no_return().
case_immediate_stop( ModuleName, TraceAggregatorPid ) ->

	case_stop_on_shell( ModuleName, TraceAggregatorPid ),

	case_facilities:finished().



% To be called from the counterpart macro.
-spec case_stop_on_shell( module_name(), trace_aggregator_pid() ) ->
								no_return().
case_stop_on_shell( ModuleName, TraceAggregatorPid ) ->

	?case_notice_fmt( "Stopping case ~ts.", [ ModuleName ] ),

	% Variable shared through macro use:
	TraceAggregatorPid ! { synchronous_delete, self() },

	receive

		{ deleted, TraceAggregatorPid } ->
			ok

	end,

	traces:check_pending_wooper_results(),

	class_TraceAggregator:remove(),

	case_facilities:display( "End of case ~ts", [ ModuleName ] ).
