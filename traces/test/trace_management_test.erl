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


% Unit tests for the implementation of trace management.
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
-module(trace_management_test).



% Test target:
-include("traces_for_tests.hrl").



% Runs the tests.
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

	case executable_utils:is_batch() of

		true ->
			test_facilities:display( "Running in batch mode." );

		false ->
			test_facilities:display( "Running in interactive mode." )

	end,


	test_facilities:display( "Starting Trace system, with a trace aggregator "
							 "and, if requested, a trace supervisor." ),

	% Test target here:
	?test_start,

	test_facilities:display( "Creating a new TestTraceEmitter." ),

	Name = "I am a test emitter of traces",

	% Should not trigger the launch of another global aggregator:
	% (as test_start triggers a *synchronous* aggregator):
	%
	MyTraceEmitter = class_TestTraceEmitter:synchronous_new_link( Name ),

	% Test target here:
	?test_emergency(   "This is a test of the emergency severity for tests." ),
	?test_alert(   "This is a test of the alert severity for tests." ),
	?test_critical(   "This is a test of the critical severity for tests." ),
	?test_error(   "This is a test of the error severity for tests." ),
	?test_warning( "This is a test of the warning severity for tests." ),
	?test_notice(   "This is a test of the notice severity for tests." ),
	?test_info(    "This is a test of the info severity for tests."  ),
	?test_debug(   "This is a test of the debug severity for tests." ),
	?test_void(    "This is a test of the void severity for tests." ),

	% Useful to test the trace listener:
	?test_debug(   "This is an additional test with some non-Latin1 "
				   "characters: àâäéèêëîïôöùûü" ),


	?test_emergency_fmt( "This is a test of the ~w severity for tests.",
						 [ emergency ] ),

	?test_alert_fmt( "This is a test of the ~w severity for tests.",
					 [ alert ] ),

	?test_critical_fmt( "This is a test of the ~w severity for tests.",
						[ critical ] ),

	?test_error_fmt( "This is a test of the ~w severity for tests.",
					 [ error ] ),

	?test_warning_fmt( "This is a test of the ~w severity for tests.",
					   [ warning ] ),

	?test_notice_fmt( "This is a test of the ~w severity for tests.",
					  [ notice ] ),

	?test_info_fmt( "This is a test of the ~w severity for tests.",
					[ info ] ),

	?test_debug_fmt( "This is a test of the ~w severity for tests.",
					 [ debug ] ),

	?test_void_fmt( "This is a test of the ~w severity for tests.",
					 [ void ] ),


	test_facilities:display(
		"Requesting the TestTraceEmitter to send some traces." ),

	% Waits until there is an answer for this trace emitter:
	MyTraceEmitter ! { sendTraces, [], self() },

	receive

		{ wooper_result, ok } ->
			test_facilities:display( "Traces sent." )

	end,

	ExpectedFirstBinaryName = text_utils:string_to_binary( Name ),

	MyTraceEmitter ! { getName, [], self() },
	ExpectedFirstBinaryName = test_receive(),
	?test_info( "Correct name returned." ),


	NewName = "This is my new name",

	MyTraceEmitter ! { setName, [ NewName ] },

	ExpectedSecondBinaryName = text_utils:string_to_binary( NewName ),

	MyTraceEmitter ! { getName, [], self() },
	ExpectedSecondBinaryName = test_receive(),

	?test_info( "Correct name returned." ),


	test_facilities:display( "Deleting this TestTraceEmitter." ),

	MyTraceEmitter ! delete,

	% Test target here:
	?test_stop,

	test_facilities:stop().
