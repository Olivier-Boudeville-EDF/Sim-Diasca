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


% Defines some macros and functions useful for trace-using cases.
% This is thus the main/only header file such cases should include.

% Note: directly obtained from traces_for_tests.hrl.

% We have kept macros for all the traces (including the ones for cases, and
% start/stop) for the sake of consistency. Moreover doing so allows to
% communicate more easily with agents like the trace aggregator (as we can then
% share discretly variables like TraceAggregatorPid).


% Defines everything regarding application traces:
-include("traces_case_header.hrl").


% For the export of run/0:
-include("case_facilities.hrl").


% To avoid warnings if not used:
-export([ case_failed/1, case_failed/2, test_failed/1, test_failed/2 ]).


% To perform receives from cases:
-include("receive_helpers_header.hrl").


% For notify_* and al:
-include_lib("traces/include/traces.hrl").





% Start/stop section.




% From here the trace supervisor is not started, due to a renaming to happen
% later, in sim_diasca:init/3.
%
% As a consequence, if a given test/case is not to call sim_diasca:init/3, it
% should not use the case_* macros (prefer the test_* ones in
% traces_for_tests.hrl), otherwise no trace supervisor will be created.


-ifdef(tracing_activated).


% TraceAggregatorPid voluntarily exported from case_start, for case_stop:

-define( case_start,
	% No supervisor wanted from scratch, as their trace file will have to be
	% renamed and the LogMX supervisor cannot change the file it is tracking:
	%
	% (not binding even a mute variable, would have been:
	% _InitTraceSupervisor=false)
	%
	TraceAggregatorPid = traces_for_cases:case_start( ?MODULE, false )
).


-define( case_start(TraceType),
	% No supervisor wanted from scratch, as their trace file will have to be
	% renamed and the LogMX supervisor cannot change the file it is tracking:
	%
	% (not binding even a mute variable, would have been:
	% _InitTraceSupervisor=false)
	%
	TraceAggregatorPid = traces_for_cases:case_start( ?MODULE, false,
													  TraceType )
).


-define( case_stop,
	% Consistent with case_start above:
	%
	% (not binding even a mute variable, would have been:
	% _WaitForTraceSupervisor=true)
	%
	traces_for_cases:case_stop( ?MODULE, TraceAggregatorPid, true )
).


-else. % tracing_activated



-define( case_start,
	% Here, even if the trace sending is deactivated, a trace aggregator is
	% created, as some processes nevertheless expect to find one at start-up, or
	% some of them may have been recompiled to be trace-enabled.
	%
	% However no trace supervisor is triggered here.
	%
	% (not binding even a mute variable, would have been:
	% _InitTraceSupervisor=false)
	%
	TraceAggregatorPid = traces_for_cases:case_start( ?MODULE, false )
).


-define( case_start(TraceType),
	% Here, even if the trace sending is deactivated, a trace aggregator is
	% created, as some processes nevertheless expect to find one at start-up, or
	% some of them may have been recompiled to be trace-enabled.
	%
	% However no trace supervisor is triggered here.
	%
	% false is for InitTraceSupervisor (not even binding a mute variable
	% for that); test_stop/2 to be consistent with it.
	%
	TraceAggregatorPid = traces_for_cases:case_start( ?MODULE, false,
													  TraceType )
).


-define( case_stop,

	% Consistent with case_start above:
	%
	% (not binding even a mute variable, would have been:
	% _WaitForTraceSupervisor=false; not a call to case_immediate_stop/3 either)
	%
	traces_for_cases:case_immediate_stop( ?MODULE, TraceAggregatorPid )
).


-endif. % tracing_activated



% Valid whether or not tracing is activated:

-define( case_stop_without_waiting_for_trace_supervisor,
	traces_for_cases:case_immediate_stop( ?MODULE, TraceAggregatorPid ) ).


-define( case_stop_on_shell,
	traces_for_cases:case_stop_on_shell( ?MODULE, TraceAggregatorPid ) ).







%%%%%%%%%%%%%%%%%%%%%%%%% Between header and footer %%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Defines everything regarding application traces:
-include("traces_case_footer.hrl").


% To perform receives from cases:
-include("receive_helpers_footer.hrl").


% Helper macro for those who would not know they could have called the
% corresponding function directly:
%
-define( case_failed, case_failed() ).



% Handles a case failure, using specified string as advertised reason.
-spec case_failed( string() ) -> no_return().
case_failed( Reason ) ->

	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	Message = io_lib:format( "Case ~ts failed, reason: ~ts.",
							 [ ?MODULE, Reason ] ),

	trace_utils:error( Message ),

	?case_emergency( Message ),

	% Needed, otherwise the standard logger may not display anything:
	system_utils:await_output_completion(),

	erlang:error( "Case ~ts failed.", [ ?MODULE ] ).



% Handles a case failure, using specified first string as an advertised reason
% with format characters (ex: '~w') and specified list as actual values to be
% formatted.
%
-spec case_failed( text_utils:format_string(), [ any() ] ) ->
						 no_return().
case_failed( Reason, FormattedValue ) ->
	case_failed( io_lib:format( Reason, FormattedValue ) ).



% Test support:

-spec test_failed( string() ) -> no_return().
test_failed( Reason ) ->
	case_failed( Reason ).


-spec test_failed( text_utils:format_string(), text_utils:format_values() ) ->
						 no_return().
test_failed( Reason, FormatValues ) ->
	case_failed( Reason, FormatValues ).
