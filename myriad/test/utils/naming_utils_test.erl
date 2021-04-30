% Copyright (C) 2003-2021 Olivier Boudeville
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


% Unit tests for the basic utils toolbox.
%
% See the naming_utils.erl tested module.
%
-module(naming_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	UnregisteredName = test_non_registered,

	try naming_utils:get_registered_pid_for( UnregisteredName ) of

		_Anything ->
			throw( test_should_have_failed )

	catch

		{ neither_registered_locally_nor_globally, UnregisteredName } ->
			ok

	end,

	not_registered = naming_utils:is_registered( UnregisteredName ),

	RegisteredName = test_registered,
	PidToRegister = self(),
	naming_utils:register_as( PidToRegister, RegisteredName, global_only ),

	try naming_utils:get_registered_pid_for( RegisteredName ) of

		PidToRegister ->
			ok

	catch

		Exception ->
			throw( { test_should_have_succeeded, Exception } )

	end,


	case naming_utils:is_registered( RegisteredName ) of

		not_registered ->
			throw( { neither_registered_locally_nor_globally,
					 RegisteredName } );

		Pid when is_pid(Pid) ->
			ok

	end,

	test_facilities:stop().
