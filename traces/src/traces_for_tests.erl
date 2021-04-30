% Copyright (C) 2007-2021 Olivier Boudeville
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


% This module gathers all the code that allows to lighten the trace macros for
% tests.
%
-module(traces_for_tests).


-export([ test_start/2, test_stop/3, test_immediate_stop/2,
		  test_stop_on_shell/2 ]).


-define( trace_emitter_categorization, "test.life-cycle" ).


% For test_info_fmt and al:
-include("traces_test_header.hrl").


% For TraceType:
-include("traces.hrl").


-include("class_TraceSupervisor.hrl").


-include("traces_test_footer.hrl").



% Shorthands:

-type module_name() :: basic_utils:module_name().

-type aggregator_pid() :: class_TraceAggregator:aggregator_pid().



% To be called from the counterpart macro.
%
% The trace supervisor can be requested to be initialized now or not at all, or
% later (typically only once the desired filename for the traces file will be
% known for good, i.e. at its first renaming).
%
% Here we disable explicitly the trapping of EXIT signals, as a function run
% through "erl -eval" (like our apps) or through "erl -run" will be executed in
% a process that will silently trap EXIT signals, which would mean that the
% crash of any process created from the app, even thanks to spawn_link, would
% most probably remain unnoticed (just leading to an EXIT message happily
% sitting in the mailbox of the app process).
%
-spec test_start( module_name(),
		class_TraceAggregator:initialize_supervision() ) -> aggregator_pid().
% All values possible for InitTraceSupervisor here:
test_start( ModuleName, InitTraceSupervisor ) ->

	% See comments above about:
	erlang:process_flag( trap_exit, false ),

	% Create first, synchronously (to avoid race conditions), a trace
	% aggregator.
	%
	% Race conditions could occur at least with trace emitters (they would
	% create their own aggregator, should none by found) and with trace
	% supervisor (which expects a trace file to be already created at start-up).

	TestIsBatch = executable_utils:is_batch(),

	%trace_utils:debug_fmt( "At test_start/2: TestIsBatch=~ts, "
	%	"InitTraceSupervisor=~ts.", [ TestIsBatch, InitTraceSupervisor ] ),

	TraceFilename = traces:get_trace_filename( ModuleName ),

	% Not wanting the trace aggregator to initialize the trace supervisor, as
	% otherwise the latter would notify that its monitoring is over to the
	% former, whereas we want instead the calling process (i.e. the test) to be
	% notified of it (see test_stop/2):
	%
	TraceAggregatorPid = class_TraceAggregator:synchronous_new_link(
		TraceFilename, ?TraceType, ?TraceTitle,
		_MaybeRegistrationScope=global_only, TestIsBatch,
		_AggInitTraceSupervisor=false ),

	case ModuleName of

		traces_via_otp ->
			?test_info( "Starting the Ceylan-Traces test from an "
						"OTP context." );

		_ ->
			?test_info_fmt( "Starting test ~ts.", [ ModuleName ] )

	end,

	% So we trigger the supervisor launch by ourselves:
	%
	% (ex: InitTraceSupervisor could have been set to 'later')
	case ( not TestIsBatch ) andalso ( InitTraceSupervisor =:= true ) of

		true ->
			TraceAggregatorPid ! { launchTraceSupervisor, [], self() },
			receive

				{ wooper_result, _SupervisorPid } ->
					ok

			end;

		false ->
			ok

	end,

	TraceAggregatorPid.



% To be called from the counterpart macro.
-spec test_stop( module_name(), aggregator_pid(), boolean() ) -> no_return().
test_stop( ModuleName, TraceAggregatorPid, WaitForTraceSupervisor ) ->

	% As test_start might have been called with InitTraceSupervisor=false.

	%trace_utils:info_fmt( "Test stopping (aggregator: ~w, wait supervisor: "
	%    "~ts).", [ TraceAggregatorPid, WaitForTraceSupervisor] ),

	case WaitForTraceSupervisor of

		true ->
			class_TraceSupervisor:wait_for();

		false ->
			ok

	end,

	%trace_utils:info( "Going for immediate stop." ),

	% Stop trace sent there:
	test_immediate_stop( ModuleName, TraceAggregatorPid ).



% To be called from the counterpart macro.
-spec test_immediate_stop( module_name(), aggregator_pid() ) -> no_return().
test_immediate_stop( ModuleName, TraceAggregatorPid ) ->

	%trace_utils:info( "Immediate stop." ),

	test_stop_on_shell( ModuleName, TraceAggregatorPid ),

	%trace_utils:info( "Finishing." ),

	test_facilities:finished().



% To be called from the counterpart macro, directly or not.
-spec test_stop_on_shell( module_name(), aggregator_pid() ) -> no_return().
test_stop_on_shell( ModuleName, TraceAggregatorPid ) ->

	?test_info_fmt( "Stopping test ~ts.", [ ModuleName ] ),

	% Also possible: class_TraceAggregator:remove(),

	% Variable shared through macro use:
	TraceAggregatorPid ! { synchronous_delete, self() },

	receive

		{ deleted, TraceAggregatorPid } ->
			ok

	end,

	traces:check_pending_wooper_results(),

	test_facilities:display( "End of test ~ts.", [ ModuleName ] ).
