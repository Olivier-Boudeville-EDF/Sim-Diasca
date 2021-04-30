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
% Creation date: Wednesday, October 31, 2018



% This module allows to test the management of passive instances.
-module(passive_instance_test).


-export([ run/0 ]).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	test_facilities:display( "Running passive instance test." ),

	TestInstance = class_ChildTestClass:new_passive( _Age=74, _Gender=male ),

	test_facilities:display( "Instance created, calling first request." ),

	{ FirstReqTestInstance, "Bob" } =
		wooper:execute_request( TestInstance, getName ),


	test_facilities:display( "Calling first oneway." ),

	NewName = "John",

	FirstOnwTestInstance =
		wooper:execute_oneway( FirstReqTestInstance, setName, [ NewName ] ),

	test_facilities:display( "Calling second request." ),

	{ SecondReqTestInstance, NewName } =
		wooper:execute_request( FirstOnwTestInstance, getName ),

	A = 1,
	B = 5,
	Expected = A + B + 10,

	test_facilities:display( "Calling static method." ),

	Expected = class_ChildTestClass:get_static_info( A, B ),

	wooper:delete_passive( SecondReqTestInstance ),

	test_facilities:display( "Test success." ),

	test_facilities:stop().
