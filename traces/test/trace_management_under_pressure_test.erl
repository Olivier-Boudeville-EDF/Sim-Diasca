% Copyright (C) 2007-2024 Olivier Boudeville
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


% @doc Unit tests for the implementation of <b>trace management</b>.
%
% See the following modules:
% - class_TraceAggregator
% - class_TraceSupervisor
%
% Note: trace services are among the most generic services offered, they are
% used in the vast majority of tests but this one, as the purpose of this test
% is actually to test traces by themselves (we cannot use the trace system to
% test the trace system!).
%
-module(trace_management_under_pressure_test).



% For trace facilities:
-include("traces_for_tests.hrl").



send_traces( _TraceEmitters, _SequenceCount=0 ) ->
	ok;

send_traces( TraceEmitters, SequenceCount ) ->

	%test_facilities:display( "Pressure test sending set of traces, "
	%                         "remaining: ~B.", [ SequenceCount ] ),

	% We do not want 'ok' answers on purpose, to speed up the sending:
	[ TE ! sendAsyncTraces || TE <- TraceEmitters ],

	send_traces( TraceEmitters, SequenceCount-1 ).



% @doc Run the tests.
%
% Note: this test is among the only ones that do not use the trace
% functionalities for their own behaviours (since it is the subject of these
% tests).
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	test_facilities:display( "Starting Trace system, with a trace aggregator "
							 "and, if requested, a trace supervisor." ),
	?test_start,


	case executable_utils:is_batch() of

		true ->
			test_facilities:display( "Running in batch mode." );

		false ->
			test_facilities:display( "Running in interactive mode." )

	end,

	Name = "I am a test emitter of traces",

	%EmitterCount = 100,
	%EmitterCount = 20,
	EmitterCount = 2,

	test_facilities:display( "Creating ~B instances of TestTraceEmitters.",
							 [ EmitterCount ] ),

	% Should not trigger the launch of another global aggregator:
	% (as test_start triggers a *synchronous* aggregator):
	%
	MyTraceEmitters = [
		begin
			EmitterName = text_utils:format( "~ts #~B", [ Name, C ] ),
			class_TestTraceEmitter:synchronous_new_link( EmitterName )
		end || C <- lists:seq( 1, EmitterCount ) ],

	% No console output wanted, as biasing:
	%?test_emergency( "This is a test of the emergency severity for tests." ),
	%?test_alert(     "This is a test of the alert severity for tests." ),
	%?test_critical(  "This is a test of the critical severity for tests." ),
	%?test_error(     "This is a test of the error severity for tests." ),
	%?test_warning(   "This is a test of the warning severity for tests." ),
	%?test_notice(    "This is a test of the notice severity for tests." ),
	?test_info(      "This is a test of the info severity for tests." ),
	?test_debug(     "This is a test of the debug severity for tests." ),
	?test_void(      "This is a test of the void severity for tests." ),

	?test_debug(     "This is an additional test with some special characters: "
					 "àéèïîôùû." ),

	test_facilities:display(
		"Requesting the TestTraceEmitter to send some traces." ),

	% Wait until there is an answer for this trace emitter:
	%
	% (count was set to 500 previously, but synchronized console outputs are too
	% slow for that now)
	%
	SequenceCount = 5,
	%SequenceCount = 15,
	%SequenceCount = 100,

	send_traces( MyTraceEmitters, SequenceCount ),

	test_facilities:display( "All traces sent." ),

	ExpectedFirstBinaryName = <<"I am a test emitter of traces #1">>,

	FirstEmitterPid = hd( MyTraceEmitters ),

	FirstEmitterPid ! { getName, [], self() },
	ExpectedFirstBinaryName = test_receive(),
	?test_info( "Correct name returned." ),

	NewName = "This is my new name",

	FirstEmitterPid ! { setName, [ NewName ] },

	ExpectedSecondBinaryName = text_utils:string_to_binary( NewName ),

	FirstEmitterPid ! { getName, [], self() },
	ExpectedSecondBinaryName = test_receive(),

	?test_info( "Correct new name returned." ),

	test_facilities:display( "Deleting this TestTraceEmitter." ),

	[ TE ! delete || TE <- MyTraceEmitters ],

	% Test target here:
	?test_stop,

	test_facilities:stop().
