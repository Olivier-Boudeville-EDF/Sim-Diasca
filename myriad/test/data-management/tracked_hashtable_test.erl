% Copyright (C) 2003-2023 Olivier Boudeville
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
% Author: Jingxuan Ma [jingxuan (dot) ma (at) edf (dot) fr]


% Unit tests for the tracked hashtable implementation.
% See the tracked_hashtable.erl module.
%
-module(tracked_hashtable_test).

% Directly depends on tracked_hashtable module.


-define(MyFirstKey,  'MyFirstKey').
-define(MySecondKey, 'MySecondKey').
-define(MyThirdKey,  'MyThirdKey').



% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	MyH1 = tracked_hashtable:new( 10 ),

	true = tracked_hashtable:is_empty( MyH1 ),
	tracked_hashtable:display( "Vanilla tracked table ", MyH1 ),

	MyH2 = tracked_hashtable:new( 4 ),

	MyFirstValue = "MyFirstValue",
	MyH3 = tracked_hashtable:add_entry( ?MyFirstKey, MyFirstValue, MyH2 ),
	false = tracked_hashtable:is_empty( MyH3 ),
	tracked_hashtable:display("The tracked hashtable", MyH3 ),

	MySecondValue = [ 1, 2, 3 ],
	MyH4 = tracked_hashtable:add_entry( ?MySecondKey, MySecondValue, MyH3 ),
	false = tracked_hashtable:is_empty( MyH4 ),

	tracked_hashtable:display( "The tracked hashtable", MyH4 ),

	MyH4Size = tracked_hashtable:size( MyH4 ),
	test_facilities:display( "Size of table '~ts': ~B entries",
		[ tracked_hashtable:to_string( MyH4 ), MyH4Size ] ),

	test_facilities:display( "Looking up for ~ts: ~p", [ ?MyFirstKey,
		tracked_hashtable:lookup_entry( ?MyFirstKey, MyH4 ) ] ),

	{ value, "MyFirstValue" } =
		tracked_hashtable:lookup_entry( ?MyFirstKey, MyH4 ),

	test_facilities:display( "Removing that entry." ),

	MyH5 = tracked_hashtable:remove_entry( ?MyFirstKey, MyH4 ),
	false = tracked_hashtable:is_empty( MyH5 ),

	test_facilities:display( "Extracting the same entry from "
							 "the same initial table." ),

	{ "MyFirstValue", MyH5 } =
		tracked_hashtable:extract_entry( ?MyFirstKey, MyH4 ),

	test_facilities:display( "Looking up for ~ts: ~p", [ ?MySecondKey,
			tracked_hashtable:lookup_entry( ?MySecondKey, MyH5 ) ] ),

	{ value, [1,2,3] } = tracked_hashtable:lookup_entry( ?MySecondKey, MyH5 ),

	test_facilities:display( "Removing the last entry." ),
	MyH55 = tracked_hashtable:remove_entry( ?MySecondKey, MyH5 ),
	true = tracked_hashtable:is_empty( MyH55 ),
	tracked_hashtable:display("The tracked hashtable ", MyH55 ),

	test_facilities:display( "Looking up for ~ts: ~p", [ ?MyFirstKey,
		tracked_hashtable:lookup_entry( ?MyFirstKey, MyH5 ) ] ),

	key_not_found = tracked_hashtable:lookup_entry( ?MyFirstKey, MyH5 ),

	[ MySecondValue, MyFirstValue ] =
		tracked_hashtable:get_all_values( [ ?MySecondKey, ?MyFirstKey ], MyH4 ),

	% remove_entry can also be used if the specified key is not here, will
	% return an identical table.

	tracked_hashtable:display( "The tracked hashtable ", MyH5 ),

	test_facilities:display( "Testing double key registering." ),
	MyH6 = tracked_hashtable:add_entry( ?MySecondKey, anything, MyH5 ),
	tracked_hashtable:display( "MyH6", MyH6 ),

	test_facilities:display( "Enumerating the hashtable : ~p",
		[ tracked_hashtable:enumerate( MyH6 ) ] ),

	test_facilities:display( "Listing the hashtable keys: ~p",
		[ tracked_hashtable:keys( MyH4 ) ] ),

	test_facilities:display( "Listing the hashtable values: ~p",
		[ tracked_hashtable:values( MyH4 ) ] ),


	test_facilities:display( "Applying a fun to all values of "
							 "previous hashtable:" ),

	FunValue = fun( V ) ->
				io:format( " - hello value '~p'!~n", [ V ] ),
				% Unchanged here:
				V
	end,

	tracked_hashtable:map_on_values( FunValue, MyH4 ),


	test_facilities:display( "Applying a fun to all entries of "
							 "previous hashtable:" ),

	FunEntry = fun( E={ K, V } ) ->
				io:format( " - hello, key '~p' associated to value '~p'!~n",
						   [ K, V ] ),
				% Unchanged here:
				E
	end,

	tracked_hashtable:map_on_entries( FunEntry, MyH4 ),

	test_facilities:display( "Folding on the same initial hashtable to "
							 "count the number of entries." ),

	FunCount = fun( _Entry, AccCount ) ->
					   AccCount + 1
			   end,

	InitialCount = 0,

	2 = tracked_hashtable:fold_on_entries( FunCount, InitialCount, MyH4 ),

	0 = tracked_hashtable:fold_on_entries( FunCount, InitialCount, MyH1 ),


	true = list_utils:unordered_compare( [ ?MyFirstKey, ?MySecondKey ],
										 tracked_hashtable:keys( MyH4 ) ),

	MyH7 = tracked_hashtable:add_entry( ?MyThirdKey, 3, MyH6 ),

	% MyH8 should have { AnotherKey, [1,2,3] } and { ?MyThirdKey, 3 }:
	MyH8 = tracked_hashtable:merge( MyH4, MyH7 ),

	test_facilities:display( "Merged table: ~ts~n",
		[ tracked_hashtable:to_string( MyH8 ) ] ),

	tracked_hashtable:display( "The Merged Hashtable of tracked hashtable",
							MyH8 ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys ~p:~n ~p",
					[ Keys, tracked_hashtable:select_entries( Keys, MyH8 ) ] ),

	test_facilities:stop().
