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


% Defines some macros and functions useful for trace-using tests.
% This is thus the main/only header file such tests should include.


% We have kept macros for all the traces (including the ones for tests, and
% start/stop) for the sake of consistency. Moreover doing so allows to
% communicate more easily with agents like the trace aggregator (as we can then
% share discretly variables like TraceAggregatorPid).


% Defines everything regarding application traces:
-include("traces_test_header.hrl").


% For the export of run/0:
-include_lib("myriad/include/test_facilities.hrl").


% To avoid warnings if not used:
-export([ test_receive/0, test_receive/1, test_failed/1, test_failed/2 ]).


% For notify_* and al:
-include("traces.hrl").




% Start/stop section.
%
% Any test that is not using (directly on not) traces_for_tests:test_start/2
% (like with the macros below) should then execute by itself:
%
%    erlang:process_flag( trap_exit, false )
%
% otherwise the test will silently trap EXIT signals, typically resulting in
% having linked instances failing without notice.
%
% See the comment of traces_for_tests:test_start/2 for more details.


-ifdef(tracing_activated).


% TraceAggregatorPid voluntarily exported from test_start, for test_stop:

-define( test_start,

		 % true is for InitTraceSupervisor (not even binding a mute variable for
		 % that); test_stop/2 to be consistent with it.
		 %
		 TraceAggregatorPid = traces_for_tests:test_start( ?MODULE, true )
).


-define( test_stop,
		 % true is for WaitForTraceSupervisor, in accordance with test_start/2.
		 traces_for_tests:test_stop( ?MODULE, TraceAggregatorPid, true )
).


-else. % tracing_activated


-define( test_start,
		 % Here, even if the trace sending is deactivated, a trace aggregator is
		 % created, as some processes nevertheless expect to find one at
		 % start-up, or some of them may have been recompiled to be
		 % trace-enabled.
		 %
		 % However no trace supervisor is needed here.
		 %
		 % false is for InitTraceSupervisor (not even binding a mute variable
		 % for that); test_stop/2 to be consistent with it.
		 %
		 TraceAggregatorPid = traces_for_tests:test_start( ?MODULE, false )
).


-define( test_stop,
		 % false is for WaitForTraceSupervisor, in accordance with test_start/2.
		 traces_for_tests:test_stop( ?MODULE, TraceAggregatorPid, false )
).


-endif. % tracing_activated



% Valid whether or not tracing is activated:

-define( test_stop_without_waiting_for_trace_supervisor,
		 traces_for_tests:test_immediate_stop( ?MODULE, TraceAggregatorPid )
).


-define( test_stop_on_shell,
		 traces_for_tests:test_stop_on_shell( ?MODULE, TraceAggregatorPid )
).





%%%%%%%%%%%%%%%%%%%%%%%%% Between header and footer %%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Defines everything regarding application traces:
-include("traces_test_footer.hrl").



% Helper function to write receive clauses in tests which cannot interfere with
% trace supervision, as a test may also receive trace control message the test
% code should remain unware of.
%
% Returns the received value.
%
% Ex: Pid ! {getBaz, [], self()}, MyBaz = test_receive(), ...
%
% to be used instead of:
%
% Pid ! {getBaz, [], self()},
% receive
%
%   {wooper_result, V} ->
%			V
%
% end,
% ...
%
-spec test_receive() -> any().
test_receive() ->
	traces:receive_applicative_message().



% Helper function to write receive clauses for specific messages in tests while
% not interfering with trace supervision.
%
-spec test_receive( any() ) -> void().
test_receive( Message ) ->
	traces:receive_applicative_message( Message ).




% Helper macro for those who would not know they could have called the
% corresponding function directly:
%
-define( test_failed, test_failed() ).



% Handles a test failure, using specified string as advertised reason.
-spec test_failed( text_utils:ustring() ) -> no_return().
test_failed( Reason ) ->

	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	Message = text_utils:format( "Test ~ts failed, reason: ~ts.",
								 [ ?MODULE, Reason ] ),

	trace_utils:error( Message ),

	?test_emergency( Message ),

	% Needed, otherwise error_logger may not display anything:
	system_utils:await_output_completion(),

	erlang:error( "Test ~ts failed.", [ ?MODULE ] ).



% Handles a test failure, using specified first string as an advertised reason
% with format characters (ex: '~w') and specified list as actual values to be
% formatted.
%
-spec test_failed( text_utils:format_string(), text_utils:format_values() ) ->
						no_return().
test_failed( FormatReason, FormatValues ) ->
	test_failed( text_utils:format( FormatReason, FormatValues ) ).
