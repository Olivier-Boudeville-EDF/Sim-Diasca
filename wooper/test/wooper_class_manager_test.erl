% Copyright (C) 2003-2021 Olivier Boudeville
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


% Unit tests for the WOOPER class manager implementation.
%
% See the wooper_class_manager.erl tested module.
%
-module(wooper_class_manager_test).


-include_lib("myriad/include/test_facilities.hrl").


% For wooper_class_manager_name:
-include("wooper_class_manager.hrl").


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").


-define( requested_class, class_BaseTestClass ).



% Testing the OTP-based class manager.
-spec test_with_otp() -> void().
test_with_otp() ->

	test_facilities:display( "Testing the OTP-based mode of operation." ),

	ManagerPid = wooper_class_manager:start_link(),

	wooper_class_manager:display(),

	TableKey = wooper_class_manager:get_table_key( ?requested_class ),

	test_facilities:display( "Table obtained from ~w for '~ts' "
		"(key: '~p'):~n~ts",
		[ ManagerPid, ?requested_class, TableKey,
		  table:to_string( persistent_term:get( TableKey ) ) ] ),

	wooper_class_manager:display(),

	wooper_class_manager:stop(),

	test_facilities:display( "End of the OTP-based test." ).




% Testing the non-OTP class manager.
-spec test_without_otp() -> void().
test_without_otp() ->

	test_facilities:display( "Testing the base (non-OTP) mode of operation." ),

	ManagerPid = wooper_class_manager:get_manager(),

	ManagerPid ! display,

	ManagerPid ! { get_table_key, ?requested_class, self() },

	receive

		{ wooper_virtual_table_key, TableKey } ->
			test_facilities:display( "Table obtained from ~w for '~ts' "
				"(key: '~p'):~n~ts",
				[ ManagerPid, ?requested_class, TableKey,
				  table:to_string( persistent_term:get( TableKey ) ) ] )

	end,

	ManagerPid ! display,

	ManagerPid ! stop,

	test_facilities:display( "End of the non-OTP test." ).




-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	% Not to be done here: wooper_utils:start_for_test(),

	test_with_otp(),

	test_without_otp(),

	% Probably that, if exit_after_test is set (the default), the test will stop
	% before the manager itself:
	%
	test_facilities:stop().
