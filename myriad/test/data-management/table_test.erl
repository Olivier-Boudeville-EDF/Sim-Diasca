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
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Test for the 'table' pseudo-module.
%
% The 'Myriad' parse transform is expected to replace at compilation time calls
% to this table by calls to the current reference implementation.
%
% See the myriad_parse_transform.erl module.
%
-module(table_test).


% Directly depends on the hashtable module.


% For run/0 export and al:
-include("test_facilities.hrl").


-define(MyFirstKey,  'MyFirstKey' ).
-define(MySecondKey, 'MySecondKey').
-define(MyThirdKey,  'MyThirdKey' ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	MyH1 = table:new( 10 ),

	true = table:is_empty( MyH1 ),

	table:display( "Vanilla table", MyH1 ),
	MyH1Optimised = table:optimise( MyH1 ),
	table:display( "Optimised table", MyH1Optimised ),

	table:display( MyH1 ),
	MyH2 = table:new( 4 ),

	MyFirstValue = "MyFirstValue",
	MyH3 = table:add_entry( ?MyFirstKey, MyFirstValue, MyH2 ),
	false = table:is_empty( MyH3 ),

	MySecondValue = [ 1, 2, 3 ],
	MyH4 = table:add_entry( ?MySecondKey, MySecondValue, MyH3 ),
	false = table:is_empty( MyH4 ),

	table:display( MyH4 ),

	MyH4Size = table:size( MyH4 ),
	test_facilities:display( "Size of table '~ts': ~B entries",
							 [ table:to_string( MyH4 ), MyH4Size ] ),

	test_facilities:display( "Looking up for ~ts: ~p", [ ?MyFirstKey,
		table:lookup_entry( ?MyFirstKey, MyH4 ) ] ),
	{ value, MyFirstValue } = table:lookup_entry( ?MyFirstKey, MyH4 ),

	test_facilities:display( "Removing that entry." ),
	MyH5 = table:remove_entry( ?MyFirstKey, MyH4 ),
	false = table:is_empty( MyH5 ),

	test_facilities:display( "Extracting the same entry from "
							 "the same initial table." ),
	{ MyFirstValue, MyH5 } = table:extract_entry( ?MyFirstKey, MyH4 ),

	test_facilities:display( "Looking up for ~ts: ~p",
		[ ?MyFirstKey, table:lookup_entry( ?MyFirstKey, MyH5 ) ] ),

	key_not_found  = table:lookup_entry( ?MyFirstKey, MyH5 ),

	[ MySecondValue, MyFirstValue ] = table:get_all_values(
										[ ?MySecondKey, ?MyFirstKey ], MyH4 ),

	% remove_entry can also be used if the specified key is not here, will
	% return an identical table.

	table:display( MyH5 ),
	test_facilities:display( "Testing double key registering." ),
	MyH6 = table:add_entry( ?MySecondKey, anything, MyH5 ),
	table:display( MyH6 ),

	test_facilities:display( "Enumerating the table: ~p",
		[ table:enumerate( MyH4 ) ] ),

	test_facilities:display( "Listing the table keys: ~p",
		[ table:keys( MyH4 ) ] ),

	test_facilities:display( "Listing the table values: ~p",
		[ table:values( MyH4 ) ] ),


	test_facilities:display( "Applying a fun to all values of "
							 "previous table:" ),

	FunValue = fun( V ) ->
					test_facilities:display( " - hello value '~p'!~n", [ V ] ),
					% Unchanged here:
					 V
			   end,

	table:map_on_values( FunValue, MyH4 ),


	test_facilities:display( "Applying a fun to all entries of "
							 "previous table:" ),

	FunEntry = fun( E={ K, V } ) ->
					test_facilities:display(
						 " - hello, key '~p' associated to value '~p'!~n",
						 [ K, V ] ),
					% Unchanged here:
					E
			   end,

	table:map_on_entries( FunEntry, MyH4 ),

	test_facilities:display( "Folding on the same initial table to "
							 "count the number of entries." ),

	FunCount = fun( _Entry, AccCount ) ->
					   AccCount + 1
			   end,

	2 = table:fold_on_entries( FunCount, _InitialCount=0, MyH4 ),

	0 = table:fold_on_entries( FunCount, _InitialCount=0, MyH1 ),

	true = list_utils:unordered_compare( [ ?MyFirstKey, ?MySecondKey ],
										 table:keys( MyH4 ) ),

	MyH7 = table:add_entry( ?MyThirdKey, 3, MyH6 ),

	% MyH8 should have {MySecondKey, [1,2,3]} and {?MyThirdKey, 3}:
	MyH8 = table:merge( MyH4, MyH7 ),
	test_facilities:display( "Merged table: ~ts", [ table:to_string( MyH8 ) ] ),

	MyH9 = table:optimise( MyH8 ),
	table:display( "Optimised merged table", MyH9 ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys ~p:~n ~p",
							 [ Keys, table:select_entries( Keys, MyH9 ) ] ),

	test_facilities:stop().
