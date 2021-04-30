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
% Creation date: Sunday, December 6, 2020.


% Test of the integration of the standard Erlang logs into the Traces subsystem.
-module(standard_trace_integration_test).



% Test target:
-include("traces_for_tests.hrl").



% Runs the test.
%
% Sort of a counterpart of Myriad's trace_utils_test.erl.
%
-spec run() -> no_return().
run() ->

	?test_start,

	case executable_utils:is_batch() of

		true ->
			?test_info( "Running in batch mode." );

		false ->
			?test_info( "Running in interactive mode." )

	end,

	?test_notice( "Testing the integration of the standard Erlang logs "
		"by generating a runtime error (crash) and seeing whether it "
		"is intercepted as expected." ),

	_NonCrasherPid = spawn(
		fun() ->
			?test_info_fmt( "Hello from non-crasher test process ~w.",
							[ self() ] )
		end ),

	?test_warning( "The next badarith error is intentional." ),

	_CrasherPid = spawn(

		fun() ->
			?test_notice_fmt( "Hello from crasher test process ~w.",
							  [ self() ] ),
			basic_utils:crash()
		end ),

	?test_info( "(test waiting a bit; as mentioned, the next badarith error "
				"is intentional)" ),

	timer:sleep( 2000 ),

	?test_info( "(test ended successfully)" ),

	?test_stop.
