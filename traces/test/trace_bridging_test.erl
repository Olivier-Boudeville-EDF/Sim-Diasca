% Copyright (C) 2020-2021 Olivier Boudeville
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
% Creation date: Wednesday, October 21, 2020.


% Testing the actual use of a trace bridge, in the context of Ceylan-Traces.
%
% See also, in Ceylan-Myriad, trace_bridge_test.erl for a similar test when no
% specific bridge is registered.
%
-module(trace_bridging_test).


% Test target:
-include("traces_for_tests.hrl").


% Runs the test.
-spec run() -> no_return().
run() ->

	?test_start,

	?test_info( "Testing first when no trace bridge is registered." ),

	% Note that we rely on the same test sending as the one done in Myriad, to
	% better showcase it can be transparently switched:
	%
	trace_bridge_test:emit_traces(),

	?test_info( "Then testing the trace bridge with a Ceylan-Traces bridge "
				"registered for this test process." ),

	% We define our own bridge spec and apply it to ourself:

	BridgeSpec = trace_bridge:get_bridge_spec( _MyEmitterName="MyBridgeTester",
		_MyCateg="MyTraceCategory", class_TraceAggregator:get_aggregator() ),

	trace_bridge:register( BridgeSpec ),

	trace_bridge:register_if_not_already( BridgeSpec ),

	trace_bridge_test:emit_traces(),

	trace_bridge:unregister(),

	?test_debug_fmt( "End of test for ~ts.", [ ?MODULE ] ),

	?test_stop.
