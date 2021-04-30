% Copyright (C) 2014-2021 Olivier Boudeville
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

% Creation date: Tuesday, December 2, 2014
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Unit tests for the map-based hashtable implementation.
%
% See the map_hashtable.erl tested module.
%
-module(map_hashtable_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-define(MyFirstKey,  'MyFirstKey').
-define(MySecondKey, 'MySecondKey').
-define(MyThirdKey,  'MyThirdKey').
-define(MyFourthKey, 'MyFourthKey').
-define(MyFifthKey,  'MyFifthKey').



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	MyH1 = map_hashtable:new( 10 ),

	true = map_hashtable:is_empty( MyH1 ),

	map_hashtable:display( "Vanilla map hashtable", MyH1 ),

	%UniqueEntries = [ {a,1}, {b,1}, {a,666} ],
	UniqueEntries = [ {a,1}, {b,1}, {c,666} ],
	map_hashtable:new_from_unique_entries( UniqueEntries ),

	%map_hashtable:display( MyH1 ),
	test_facilities:display( "Adding entries in map hashtable." ),
	MyH2 = map_hashtable:new( 4 ),

	MyFirstValue = "MyFirstValue",
	MyH3 = map_hashtable:add_entry( ?MyFirstKey, MyFirstValue, MyH2 ),
	false = map_hashtable:is_empty( MyH3 ),

	_MyUpdatedH3 = 
		map_hashtable:update_entry( ?MyFirstKey, MyFirstValue, MyH3 ),

	MySecondValue = [ 1, 2, 3 ],
	MyH4 = map_hashtable:add_entry( ?MySecondKey, MySecondValue, MyH3 ),
	false = map_hashtable:is_empty( MyH4 ),

	MyUpdatedH4 = map_hashtable:update_entry( ?MySecondKey, MyFirstValue, 
											  MyH4 ),

	MyFirstValue = map_hashtable:get_value( ?MySecondKey, MyUpdatedH4 ),


	map_hashtable:display( "The map hashtable", MyH4 ),

	MyH4Size = map_hashtable:size( MyH4 ),
	test_facilities:display( "Size of table '~ts': ~B entries",
							 [ map_hashtable:to_string( MyH4 ), MyH4Size ] ),

	test_facilities:display( "Looking up for ~ts: ~p", [ ?MyFirstKey,
			map_hashtable:lookup_entry( ?MyFirstKey, MyH4 ) ] ),

	{ value, MyFirstValue } = map_hashtable:lookup_entry( ?MyFirstKey, MyH4 ),

	test_facilities:display( "Removing that entry." ),
	MyH5 = map_hashtable:remove_entry( ?MyFirstKey, MyH4 ),
	false = map_hashtable:is_empty( MyH5 ),

	test_facilities:display( "Extracting the same entry from "
							 "the same initial table." ),
	{ MyFirstValue, MyH5 } = map_hashtable:extract_entry( ?MyFirstKey, MyH4 ),

	test_facilities:display( "Looking up for ~ts: ~p", [ ?MyFirstKey,
		map_hashtable:lookup_entry( ?MyFirstKey, MyH5 ) ] ),

	key_not_found = map_hashtable:lookup_entry( ?MyFirstKey, MyH5 ),

	[ MySecondValue, MyFirstValue ] = map_hashtable:get_all_values(
										[ ?MySecondKey, ?MyFirstKey ], MyH4 ),

	EmptyTable = map_hashtable:new(),

	{ [ MyFirstValue, MySecondValue ], EmptyTable } =
		map_hashtable:extract_entries( [ ?MyFirstKey, ?MySecondKey ], MyH4 ),

	% remove_entry can also be used if the specified key is not here, will
	% return an identical table.

	MyHRemoved = map_hashtable:remove_existing_entries(
				    [ ?MyFirstKey, ?MySecondKey ], MyH4 ),

	true = map_hashtable:is_empty( MyHRemoved ),

	% Should not fail:
	_ = map_hashtable:remove_entries( [ ?MyFirstKey, ?MyThirdKey ], MyH4 ),

	map_hashtable:display( MyH5 ),
	test_facilities:display( "Testing double key registering." ),

	MyH6 = map_hashtable:add_entry( ?MySecondKey, anything, MyH5 ),
	map_hashtable:display( MyH6 ),

	test_facilities:display( "Enumerating the hashtable: ~p.",
		[ map_hashtable:enumerate( MyH4 ) ] ),

	test_facilities:display( "Listing the hashtable keys: ~p.",
		[ map_hashtable:keys( MyH4 ) ] ),

	test_facilities:display( "Listing the hashtable values: ~p",
		[ map_hashtable:values( MyH4 ) ] ),


	test_facilities:display( "Appending values to elements" ),


	MyH7 = map_hashtable:append_to_entry( ?MyFourthKey, first_element, MyH5 ),

	MyH8 = map_hashtable:append_to_existing_entry( ?MyFourthKey, second_element,
												   MyH7 ),

	MyH9 = map_hashtable:append_list_to_existing_entry( ?MyFourthKey,
								[ third_element, fourth_element ], MyH8 ),

	map_hashtable:display( MyH9 ),


	MyH10 = map_hashtable:concat_list_to_entries(
				[ { ?MyFourthKey, [ last_element ] },
				  { ?MyFifthKey, [ some_element ] } ], MyH9 ),

	test_facilities:display( "Listing a concatenated table: ~ts",
							 [ map_hashtable:to_string( MyH10 ) ] ),

	test_facilities:display( "Applying a fun to all values of "
							 "previous hashtable" ),

	FunValue = fun( V ) ->
				io:format( " - hello value '~p'!~n", [ V ] ),
				% Unchanged here:
				V
	end,

	map_hashtable:map_on_values( FunValue, MyH4 ),


	test_facilities:display( "Applying a fun to all entries of "
							 "previous hashtable:" ),

	FunEntry = fun( E={ K, V } ) ->
				io:format( " - hello, key '~p' associated to value '~p'!~n",
						   [ K, V ] ),
				% Unchanged here:
				E
	end,

	map_hashtable:map_on_entries( FunEntry, MyH4 ),


	test_facilities:display( "Folding on the same initial hashtable to "
							 "count the number of entries." ),

	FunCount = fun( _Entry, AccCount ) ->
					AccCount + 1
			   end,

	2 = map_hashtable:fold_on_entries( FunCount, _InitialCount=0, MyH4 ),

	0 = map_hashtable:fold_on_entries( FunCount, _InitialCount=0, MyH1 ),


	true = list_utils:unordered_compare( [ ?MyFirstKey, ?MySecondKey ],
										 map_hashtable:keys( MyH4 ) ),

	MyH11 = map_hashtable:add_entry( ?MyThirdKey, 3, MyH6 ),

	% MyH8 should have {AnotherKey, [1,2,3]} and {?MyThirdKey, 3}:
	MyH12 = map_hashtable:merge( MyH4, MyH11 ),

	% Any optimisation would be automatic:
	test_facilities:display( "Merged table: ~ts.",
							 [ map_hashtable:to_string( MyH12 ) ] ),

	MyH13 = map_hashtable:new(),

	MyH14 = map_hashtable:new( [ { ?MyFourthKey, x }, { ?MyFifthKey, y } ] ),

	MyMergeResTable = map_hashtable:merge_unique( MyH4, MyH14 ),

	MyMergeResTable = map_hashtable:merge_unique( [ MyH4, MyH13, MyH14 ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys ~p:~n ~p",
					[ Keys, map_hashtable:select_entries( Keys, MyH12 ) ] ),

	test_facilities:stop().
