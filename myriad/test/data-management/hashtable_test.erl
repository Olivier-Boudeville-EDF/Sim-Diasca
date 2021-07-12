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


% Unit tests for the generic hashtable implementation.
%
% See the hashtable.erl tested module.
%
-module(hashtable_test).


% Directly depends on the hashtable module.


% For run/0 export and al:
-include("test_facilities.hrl").


-define(MyFirstKey,  'MyFirstKey' ).
-define(MySecondKey, 'MySecondKey').
-define(MyThirdKey,  'MyThirdKey' ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	MyH1 = hashtable:new( 10 ),

	true = hashtable:is_empty( MyH1 ),

	hashtable:display( "Vanilla table", MyH1 ),
	MyH1Optimised = hashtable:optimise( MyH1 ),
	hashtable:display( "Optimised table", MyH1Optimised ),

	hashtable:display( MyH1 ),
	MyH2 = hashtable:new( 4 ),

	MyFirstValue = "MyFirstValue",
	MyH3 = hashtable:add_entry( ?MyFirstKey, MyFirstValue, MyH2 ),
	false = hashtable:is_empty( MyH3 ),

	MySecondValue = [ 1, 2, 3 ],
	MyH4 = hashtable:add_entry( ?MySecondKey, MySecondValue, MyH3 ),
	false = hashtable:is_empty( MyH4 ),

	hashtable:display( MyH4 ),

	MyH4Size = hashtable:size( MyH4 ),
	test_facilities:display( "Size of table '~ts': ~B entries",
							 [ hashtable:to_string( MyH4 ), MyH4Size ] ),

	test_facilities:display( "Looking up for ~ts: ~p", [ ?MyFirstKey,
		hashtable:lookup_entry( ?MyFirstKey, MyH4 ) ] ),
	{ value, MyFirstValue } = hashtable:lookup_entry( ?MyFirstKey, MyH4 ),

	test_facilities:display( "Removing that entry." ),
	MyH5 = hashtable:remove_entry( ?MyFirstKey, MyH4 ),
	false = hashtable:is_empty( MyH5 ),

	test_facilities:display( "Extracting the same entry from "
							 "the same initial table." ),
	{ MyFirstValue, MyH5 } = hashtable:extract_entry( ?MyFirstKey, MyH4 ),

	test_facilities:display( "Looking up for ~ts: ~p", [ ?MyFirstKey,
		hashtable:lookup_entry( ?MyFirstKey, MyH5 ) ] ),

	key_not_found  = hashtable:lookup_entry( ?MyFirstKey, MyH5 ),

	[ MySecondValue, MyFirstValue ] =
		hashtable:get_all_values( [ ?MySecondKey, ?MyFirstKey ], MyH4 ),

	% remove_entry can also be used if the specified key is not here, will
	% return an identical table.

	hashtable:display( MyH5 ),
	test_facilities:display( "Testing double key registering." ),
	MyH6 = hashtable:add_entry( ?MySecondKey, anything, MyH5 ),
	hashtable:display( MyH6 ),

	test_facilities:display( "Enumerating the hashtable: ~p",
		[ hashtable:enumerate( MyH4 ) ] ),

	test_facilities:display( "Listing the hashtable keys: ~p",
		[ hashtable:keys( MyH4 ) ] ),

	test_facilities:display( "Listing the hashtable values: ~p",
		[ hashtable:values( MyH4 ) ] ),


	test_facilities:display( "Applying a fun to all values of "
							 "previous hashtable:" ),

	FunValue = fun( V ) ->
				io:format( " - hello value '~p'!~n", [ V ] ),
				% Unchanged here:
				V
	end,

	hashtable:map_on_values( FunValue, MyH4 ),


	test_facilities:display( "Applying a fun to all entries of "
							 "previous hashtable:" ),

	FunEntry = fun( E={ K, V } ) ->
				io:format( " - hello, key '~p' associated to value '~p'!~n",
						   [ K, V ] ),
				% Unchanged here:
				E
	end,

	hashtable:map_on_entries( FunEntry, MyH4 ),

	test_facilities:display( "Folding on the same initial hashtable to "
							 "count the number of entries." ),

	FunCount = fun( _Entry, AccCount ) ->
					   AccCount + 1
			   end,

	InitialCount = 0,

	2 = hashtable:fold_on_entries( FunCount, InitialCount, MyH4 ),

	0 = hashtable:fold_on_entries( FunCount, InitialCount, MyH1 ),

	true = list_utils:unordered_compare( [ ?MyFirstKey, ?MySecondKey ],
										 hashtable:keys( MyH4 ) ),

	MyH7 = hashtable:add_entry( ?MyThirdKey, 3, MyH6 ),

	% MyH8 should have { MySecondKey, [1,2,3] } and { ?MyThirdKey, 3 }:
	MyH8 = hashtable:merge( MyH4, MyH7 ),
	test_facilities:display( "Merged table: ~ts",
							[ hashtable:to_string( MyH8 ) ] ),

	MyH9 = hashtable:optimise( MyH8 ),
	hashtable:display( "Optimised merged table", MyH9 ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys ~p:~n ~p",
							[ Keys, hashtable:select_entries( Keys, MyH9 ) ] ),

	test_facilities:stop().
