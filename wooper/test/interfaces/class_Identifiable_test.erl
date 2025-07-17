% Copyright (C) 2022-2025 Olivier Boudeville
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
% Creation date: Sunday, July 24, 2022.

-module(class_Identifiable_test).

-moduledoc """
Unit tests for the **Identifiable** class implementation.

See the class_Identifiable module.
""".


-export([ run/0 ]).



-doc "Runs the tests.".
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Creating a test Identifiable." ),

	% We use here sortable identifiers:
	FirstIdentifier = id_utils:get_initial_sortable_id(),

	FirstIdentifiable = class_Identifiable:new_link( FirstIdentifier ),

	FirstIdentifiable ! { getIdentifier, [], self() },

	% Check:
	FirstIdentifier = receive

		{ wooper_result, FirstId } ->
			FirstId

	end,

	test_facilities:display( "Correct identifier returned: '~ts'.",
		[ id_utils:sortable_id_to_string( FirstIdentifier ) ] ),


	SecondIdentifier = id_utils:get_next_sortable_id( FirstIdentifier ),

	FirstIdentifiable ! { setIdentifier, [ SecondIdentifier ] },

	FirstIdentifiable ! { getIdentifier, [], self() },

	% Check:
	SecondIdentifier = receive

		{ wooper_result, SecondId } ->
			SecondId

	end,

	test_facilities:display( "Correct new identifier returned: '~ts'.",
		[ id_utils:sortable_id_to_string( SecondIdentifier ) ] ),


	ThirdIdentifier =
		id_utils:get_sortable_id_between( FirstIdentifier, SecondIdentifier ),

	SecondIdentifiable = class_Identifiable:new_link( ThirdIdentifier ),


	FirstIdentifiable ! { compareWith, [ SecondIdentifiable ], self() },

	receive

		{ wooper_result, false } ->
			ok;

		{ wooper_result, true } ->
			throw( { incorrect_id_comparison, FirstIdentifiable,
					 SecondIdentifiable } )

	end,

	true = class_Identifiable:compare( FirstIdentifiable, FirstIdentifiable ),
	true = class_Identifiable:compare( SecondIdentifiable, SecondIdentifiable ),
	false = class_Identifiable:compare( FirstIdentifiable, SecondIdentifiable ),

	wooper:delete_synchronously_instances(
		[ FirstIdentifiable, SecondIdentifiable ] ),

	test_facilities:stop().
