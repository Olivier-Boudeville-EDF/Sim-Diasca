% Copyright (C) 2022-2023 Olivier Boudeville
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
% Creation date: Sunday, February 27, 2022.


% @doc Testing of the <b>environment</b> service.
%
% See the environment.erl tested module.
%
-module(environment_test).



% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Environment while the service is not running: "
							 "~ts", [ environment:to_string() ] ),

	ETFFilePath = "test-environment.etf",

	file_utils:remove_file_if_existing( ETFFilePath ),

	FirstEnvRegName = first_env,

	% May not be called (automatic launching of the service whenever needed):
	% (test of file loading done in preferences_test.erl)
	%
	environment:start_link( FirstEnvRegName ),

	FirstEnvPid = environment:get_server( FirstEnvRegName ),

	test_facilities:display( "Environment after the service is just started: "
		"~ts (server PID for '~ts': ~w).", [ environment:to_string(),
			FirstEnvRegName, FirstEnvPid ] ),


	FirstTargetKey = first_test_key,

	test_facilities:display( "Value associated to ~ts before it is set "
		"from test: ~p", [ FirstTargetKey,
			environment:get( FirstTargetKey, FirstEnvRegName ) ] ),

	FirstTargetValue = "This is the first test value!",

	environment:set( FirstTargetKey, FirstTargetValue, FirstEnvRegName ),

	test_facilities:display( "Value associated to ~ts after it is set "
		"from test: ~p", [ FirstTargetKey,
			environment:get( FirstTargetKey, FirstEnvRegName ) ] ),


	SecondTargetKey = second_test_key,
	SecondTargetValue = "This is the second test value!",
	environment:set( SecondTargetKey, SecondTargetValue, FirstEnvRegName ),

	AnotherTargetValue = "This is another test value!",

	ThirdTargetKey = third_test_key,
	FourthTargetValue = "This is a fourth value!",

	environment:set( [ { FirstTargetKey, AnotherTargetValue },
					   { ThirdTargetKey, FourthTargetValue } ],
					 FirstEnvRegName ),

	test_facilities:display( "Value associated to ~ts after it is set "
		"again from test: ~p", [ FirstTargetKey,
			environment:get( FirstTargetKey, FirstEnvRegName ) ] ),


	[ AnotherTargetValue, FourthTargetValue, SecondTargetValue ] =
		environment:get( [ FirstTargetKey, ThirdTargetKey, SecondTargetKey ],
						 FirstEnvRegName ),

	test_facilities:display( "Environment while the service is running: "
							 ++ environment:to_string() ),

	TestEntries = [ { FirstTargetKey, 1 }, { SecondTargetKey, 2 } ],

	environment:set( TestEntries, FirstEnvRegName ),

	[ 2, 1 ] =
		environment:get( [ SecondTargetKey, FirstTargetKey ], FirstEnvPid ),

	test_facilities:display( "Environment after the second setting: "
							 ++ environment:to_string() ),

	environment:cache(
		[ first_test_key, { third_test_key, "Third cache value!" } ],
		FirstEnvRegName ),


	% For this test we update the environment server stealthly, bypassing the
	% cache; better than:
	%
	%FirstEnvRegName ! { set_environment, [ { first_test_key, 7 } ] },
	%
	% is to use a separate client process for that:
	spawn_link( fun() ->

		environment:set( first_test_key, 7, FirstEnvPid ),

		% Both a check and a way of synchronising the server update to avoid any
		% race condition with the next read from the test process:
		%
		7 = environment:get( first_test_key, FirstEnvPid ),

		test_facilities:display( "Auxiliary test process ~w succeeded.",
								 [ self() ] )

				end ),


	% This test process should be still unaware of the update because of its
	% caching:
	%
	1 = environment:get( FirstTargetKey, FirstEnvRegName ),

	environment:sync( FirstEnvRegName ),

	% Now aware:
	7 = environment:get( FirstTargetKey, FirstEnvRegName ),

	test_facilities:display( "Environment after the cache enabling: "
								++ environment:to_string() ),

	% New cache request:
	environment:cache( { FirstTargetKey, 8 }, FirstEnvRegName ),

	8 = environment:get( FirstTargetKey, FirstEnvRegName ),

	% Note that first_test_key will still be at 7 in:
	environment:store( FirstEnvRegName, ETFFilePath ),


	test_facilities:display( "Now testing conditional setting." ),

	% With a non-cached key:
	NonCachedKey = non_cached_key,
	Hello = hello,

	environment:set_cond( NonCachedKey, Hello, FirstEnvPid ),

	Hello = environment:get( NonCachedKey, FirstEnvPid ),


	% With a cached key already set to the specified value:
	environment:set_cond( FirstTargetKey, 8, FirstEnvPid ),

	8 = environment:get( FirstTargetKey, FirstEnvRegName ),


	% With a cached key already not set to the specified value:
	Goodbye = goodbye,

	environment:set_cond( FirstTargetKey, Goodbye, FirstEnvPid ),

	Goodbye = environment:get( FirstTargetKey, FirstEnvRegName ),


	% Useless in the general case (permanent service):
	environment:stop( FirstEnvRegName ),

	test_facilities:stop().
