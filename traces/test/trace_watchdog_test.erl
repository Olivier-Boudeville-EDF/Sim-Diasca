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
% Creation date: Friday, December 25, 2020.


% Testing the watchdog for the trace aggregator.
-module(trace_watchdog_test).



% Test target:
-include("traces_for_tests.hrl").



% Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	% Test target here:
	?test_start,

	AggregatorPid = case class_TraceAggregator:get_aggregator() of

		trace_aggregator_not_found ->
			throw( trace_aggregator_not_found );

		AgPid ->
			AgPid

	end,

	% Will run as long as the test is not stopped; 1 check per second:
	AggregatorPid ! { enableWatchdog, [ _Period=1 ] },

	timer:sleep( 1000 ),

	% Test target here:
	?test_stop,

	test_facilities:stop().
