% Copyright (C) 2014-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER library.
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
% Creation date: Wednesday, October 31, 2018.



% This module allows to test the management of the methods of a class.
-module(method_management_test).


-export([ run/0 ]).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	test_facilities:display( "Running method test." ),

	TestedPid = class_MethodTester:new_link(),

	test_facilities:display( "Instance ~p created, calling first request.",
							 [ TestedPid ] ),

	TestedPid ! { getName, [], self() },

	test_facilities:display( "Waiting for first request answer." ),

	receive

		{ wooper_result, "Terry" } ->
			ok;

		Other ->
			test_facilities:fail( "First getName/2 test failed: ~p.",
								  [ Other ] )

	end,

	NewName = "John",

	test_facilities:display( "Sending oneway." ),
	TestedPid ! { setName, [ NewName ] },


	test_facilities:display( "Calling second request." ),
	TestedPid ! { getName, [], self() },

	test_facilities:display( "Waiting for second request answer." ),

	receive

		{ wooper_result, NewName } ->
			ok;

		Another ->
			test_facilities:fail( "Second getName/2 test failed: ~p.",
								  [ Another ] )

	end,

	A = 1,
	B = 5,
	Expected = A + B + 10,


	test_facilities:display( "Testing indirectly executeConstRequestAs/4." ),

	ChildTestPid = class_ChildTestClass:new_link( 14, female ),

	ChildTestPid ! test_of_const_req_as,


	test_facilities:display( "Calling static method." ),

	Expected = class_MethodTester:get_static_info( A, B ),

	wooper_void_return = class_MethodTester:test_static_void(),

	test_facilities:display( "Test success." ),

	wooper:delete_synchronously_instance( TestedPid ),

	test_facilities:stop().
