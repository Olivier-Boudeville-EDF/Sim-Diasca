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


% This module allows to test the support for multiple constructors.
-module(multiple_constructors_test).


-export([ run/0 ]).


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	TestedClass = class_MultipleConstructors,

	I1Name = "Ian",

	test_facilities:display( "Creating a new instance of ~ts, named '~ts'.",
							 [ TestedClass, I1Name ] ),

	I1Pid = TestedClass:new_link( I1Name, male ),

	I1Pid ! { getName, [], self() },
	receive

		{ wooper_result, I1Name } ->
			ok

	end,

	I1Pid ! { getGender, [], self() },
	receive

		{ wooper_result, male } ->
			ok

	end,

	I1Pid ! { synchronous_delete, self() },
	receive

		{ deleted, I1Pid } ->
			ok

	end,


	I2Name = "Murdock",

	test_facilities:display( "Creating a new instance of ~ts, named '~ts'.",
							 [ TestedClass, I2Name ] ),

	I2Pid = TestedClass:new_link( I2Name ),

	I2Pid ! { getName, [], self() },
	receive

		{ wooper_result, I2Name } ->
			ok

	end,

	I2Pid ! { getGender, [], self() },
	receive

		{ wooper_result, undefined } ->
			ok

	end,

	I2Pid ! { synchronous_delete, self() },
	receive

		{ deleted, I2Pid } ->
			ok

	end,


	test_facilities:display( "Creating a new, anonymous instance of ~ts.",
							 [ TestedClass ] ),

	I3Pid = TestedClass:new_link(),

	I3Pid ! { getName, [], self() },
	receive

		{ wooper_result, "Terry" } ->
			ok

	end,

	I3Pid ! { getGender, [], self() },
	receive

		{ wooper_result, unknown } ->
			ok

	end,

	I3Pid ! { synchronous_delete, self() },
	receive

		{ deleted, I3Pid } ->
			ok

	end,

	test_facilities:stop().
