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


% Unit tests for the management of identifiers.
%
% See the id_utils.erl tested module.
%
-module(id_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Generating a new UUID: '~ts'.",
							 [ id_utils:generate_uuid() ] ),


	FirstId = id_utils:get_initial_sortable_id(),

	SecondId = id_utils:get_next_sortable_id( FirstId ),

	ThirdId = id_utils:get_next_sortable_id( SecondId ),

	BetweenTwoAndThirdId =
		id_utils:get_sortable_id_between( SecondId, ThirdId ),

	Ids = [ FirstId, SecondId, BetweenTwoAndThirdId, ThirdId ],

	FirstSortStrings = [ id_utils:sortable_id_to_string( Id ) || Id <- Ids ],

	test_facilities:display( "Test first sortable identifiers are: ~ts",
			[ text_utils:strings_to_enumerated_string( FirstSortStrings ) ] ),

	LowerBound = id_utils:get_sortable_id_lower_bound(),
	UpperBound = id_utils:get_sortable_id_upper_bound(),

	BoundString = id_utils:sortable_ids_to_string( [ LowerBound, UpperBound ] ),

	test_facilities:display( "Lower and upper bounds in terms of sortable "
							 "identifiers are: ~ts.", [ BoundString ] ),

	ElementsToIdentify=[ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i' ],

	%KnownIds = [],
	%KnownIds = [ { a, [ 5 ] } ],
	KnownIds = [ { b, [ 5 ] } ],

	IdentifierTable = table:new( KnownIds ),

	NewIdentifierTable = id_utils:assign_sorted_identifiers( ElementsToIdentify,
															 IdentifierTable ),

	test_facilities:display( "Identifier table ~ts, once updated for elements "
		"~w, with known identifiers ~w, is: ~ts",
	  [ id_utils:identifier_table_to_string( IdentifierTable ),
		ElementsToIdentify, KnownIds,
		id_utils:identifier_table_to_string( NewIdentifierTable ) ] ),

	test_facilities:stop().
