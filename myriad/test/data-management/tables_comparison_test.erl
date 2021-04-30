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
% Authors: Jingxuan Ma [jingxuan (dot) ma (at) edf (dot) fr]
%          Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Tests for comparing and illustrating the differences of following types of
% hashtables:
%
% - hashtable
% - tracked hashtable
% - lazy hashtable
% - map hashtable
% - list table
%
% See also hashtable.erl, tracked_hashtable.erl, lazy_hashtable.erl,
% map_hashtable.erl, list_table.erl and their respective test modules.
%
% Directly depends on the following modules: hashtable, tracked_hashtable,
% lazy_hashtable, map_hashtable, list_table.
%
% Note that the 'table' pseudo-module is not tested here: not only it is
% actually one of the previous implementations, but also modules are applied
% dynamically here, hence the parse transform will not replace anything.
%
-module(tables_comparison_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-define(MyFirstKey,  'MyFirstKey').
-define(MySecondKey, 'MySecondKey').
-define(MyThirdKey,  'MyThirdKey').
-define(MyFourthKey, 'MyFourthKey').


-export([ run_basic_tests/0, run_performance_tests/0 ]).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	%run_basic_tests(),

	run_performance_tests(),

	test_facilities:stop().



display_separator() ->
	test_facilities:display( "  -----------------------------~n~n" ).



% Performs the same set of operations of an instance of each type of hashtable.
run_basic_tests() ->

	% Tests for each kind of table should be separated.

	display_separator(),

	test_facilities:display( " Comparison of tables creation: " ),
	MyH1 = hashtable:new( 10 ),
	true = hashtable:is_empty( MyH1 ),
	hashtable:display( "Vanilla hashtable", MyH1 ),
	MyH1Optimised = hashtable:optimise( MyH1 ),
	hashtable:display( "Optimized hash table", MyH1Optimised ),


	MyTH1 = tracked_hashtable:new( 10 ),
	true = tracked_hashtable:is_empty( MyTH1 ),
	tracked_hashtable:display( "Vanilla tracked table ", MyTH1 ),

	MyLH1 = lazy_hashtable:new( 10 ),
	true = lazy_hashtable:is_empty( MyLH1 ),
	lazy_hashtable:display( "Vanilla lazy hashtable", MyLH1 ),

	display_separator(),
	MyM1 = map_hashtable:new( 10 ),
	true = map_hashtable:is_empty( MyM1 ),
	map_hashtable:display( "Vanilla map hashtable", MyM1 ),

	display_separator(),
	MyL1 = list_table:new( 10 ),
	true = list_table:is_empty( MyL1 ),
	list_table:display( "Vanilla list hashtable", MyL1 ),


	test_facilities:display( "End of the comparison of tables creation." ),
	display_separator(),



	test_facilities:display( "Comparison of tables' state after "
							 "adding entries:" ),


	test_facilities:display( "Adding entries in hash table:" ),
	MyH2 = hashtable:new( 4 ),
	MyH3 = hashtable:add_entry( ?MyFirstKey, "MyFirstValue", MyH2 ),
	false = hashtable:is_empty( MyH3 ),

	MyH4 = hashtable:add_entry( ?MySecondKey, "MySecondValue", MyH3 ),
	false = hashtable:is_empty( MyH4 ),

	MyH5 = hashtable:add_entry( ?MyThirdKey, [1,2,3], MyH4 ),
	false = hashtable:is_empty( MyH5 ),
	hashtable:display( MyH5 ),

	MyH5Optimised = hashtable:optimise( MyH5 ),
	hashtable:display( "Optimised hashtable", MyH5Optimised ),


	test_facilities:display( "Adding entries in tracked hashtable:"),
	MyTH2 = tracked_hashtable:new(4 ),
	MyTH3 = tracked_hashtable:add_entry( ?MyFirstKey, "MyFirstValue", MyTH2 ),
	false = tracked_hashtable:is_empty( MyTH3 ),

	MyTH4 = tracked_hashtable:add_entry( ?MySecondKey, "MySecondValue", MyTH3 ),
	false = tracked_hashtable:is_empty( MyTH4 ),

	MyTH5 = tracked_hashtable:add_entry( ?MyThirdKey, [1,2,3], MyTH4 ),
	false = tracked_hashtable:is_empty( MyTH5 ),
	tracked_hashtable:display( "Tracked hashtable: ", MyTH5 ),

	MyTH5Optimised = tracked_hashtable:optimise( MyTH5 ),
	tracked_hashtable:display( "Optimised tracked hashtable", MyTH5Optimised ),


	test_facilities:display( "Adding entries in lazy hashtable:" ),
	MyLH2 = lazy_hashtable:new( 4 ),
	MyLH3 = lazy_hashtable:add_entry( ?MyFirstKey, "MyFirstValue", MyLH2 ),
	false = lazy_hashtable:is_empty( MyLH3 ),

	MyLH4 = lazy_hashtable:add_entry( ?MySecondKey, "MySecondValue", MyLH3 ),
	false = lazy_hashtable:is_empty( MyLH4 ),

	MyLH5 = lazy_hashtable:add_entry( ?MyThirdKey, [1,2,3], MyLH4 ),
	false = lazy_hashtable:is_empty( MyLH5 ),
	lazy_hashtable:display( "Lazy hashtable: ", MyLH5 ),

	MyLH5Optimised = lazy_hashtable:optimise( MyLH5 ),
	lazy_hashtable:display( "Optimised lazy hashtable", MyLH5Optimised ),


	test_facilities:display( "Adding entries in map hashtable:" ),
	MyMH2 = map_hashtable:new( 4 ),
	MyMH3 = map_hashtable:add_entry( ?MyFirstKey, "MyFirstValue", MyMH2 ),
	false = map_hashtable:is_empty( MyMH3 ),

	MyMH4 = map_hashtable:add_entry( ?MySecondKey, "MySecondValue", MyMH3 ),
	false = map_hashtable:is_empty( MyMH4 ),

	MyMH5 = map_hashtable:add_entry( ?MyThirdKey, [1,2,3], MyMH4 ),
	false = map_hashtable:is_empty( MyMH5 ),
	map_hashtable:display( "Map hashtable: ", MyMH5 ),


	test_facilities:display( "Adding entries in list hashtable:" ),
	MyL2 = list_table:new( 4 ),
	MyL3 = list_table:add_entry( ?MyFirstKey, "MyFirstValue", MyL2 ),
	false = list_table:is_empty( MyL3 ),

	MyL4 = list_table:add_entry( ?MySecondKey, "MySecondValue", MyL3 ),
	false = list_table:is_empty( MyL4 ),

	MyL5 = list_table:add_entry( ?MyThirdKey, [1,2,3], MyL4 ),
	false = list_table:is_empty( MyL5 ),
	list_table:display( "List hashtable: ", MyL5 ),


	display_separator(),


	test_facilities:display( "Looking up for ~ts in hashtable: ~p",
		   [ ?MyFirstKey, hashtable:lookup_entry( ?MyFirstKey, MyH5 ) ] ),

	{ value, "MyFirstValue" } = hashtable:lookup_entry( ?MyFirstKey, MyH5 ),

	test_facilities:display( "Removing that entry." ),
	MyH6 = hashtable:remove_entry( ?MyFirstKey, MyH5 ),
	false = hashtable:is_empty( MyH6 ),
	test_facilities:display( "Looking up for ~ts hashtable: ~p", [ ?MyFirstKey,
		hashtable:lookup_entry( ?MyFirstKey, MyH6 ) ] ),

	key_not_found = hashtable:lookup_entry( ?MyFirstKey, MyH6 ),

	% remove_entry can also be used if the specified key is not here, will
	% return an identical table.

	hashtable:display( "Hashtable", MyH6 ),


	test_facilities:display( "Looking up for ~ts in tracked hashtable: ~p",
		[ ?MyFirstKey,
		  tracked_hashtable:lookup_entry( ?MyFirstKey, MyTH5 ) ] ),

	{ value, "MyFirstValue" } =
		tracked_hashtable:lookup_entry( ?MyFirstKey, MyTH5 ),

	test_facilities:display( "Removing that entry." ),
	MyTH6 = tracked_hashtable:remove_entry( ?MyFirstKey, MyTH5 ),
	false = tracked_hashtable:is_empty( MyTH6 ),

	test_facilities:display( "Looking up for ~ts in tracked hashtable: ~p",
		[ ?MyFirstKey,
		  tracked_hashtable:lookup_entry( ?MyFirstKey, MyTH6 ) ] ),

	key_not_found = tracked_hashtable:lookup_entry( ?MyFirstKey, MyTH6 ),

	% remove_entry can also be used if the specified key is not here, will
	% return an identical table.

	tracked_hashtable:display( "Tracked hashtable", MyTH6 ),


	test_facilities:display( "Looking up for ~ts in lazy hashtable: ~p",
		[ ?MyFirstKey, lazy_hashtable:lookup_entry( ?MyFirstKey, MyLH5 ) ] ),

	{ value, "MyFirstValue" } = lazy_hashtable:lookup_entry( ?MyFirstKey,
														   MyLH5 ),

	test_facilities:display( "Removing that entry." ),
	MyLH6 = lazy_hashtable:remove_entry( ?MyFirstKey, MyLH5 ),
	false = lazy_hashtable:is_empty( MyLH6 ),
	test_facilities:display( "Looking up for ~ts in lazy hashtable: ~p",
		[ ?MyFirstKey, lazy_hashtable:lookup_entry( ?MyFirstKey, MyLH6 ) ] ),

	key_not_found = lazy_hashtable:lookup_entry( ?MyFirstKey, MyLH6 ),

	% remove_entry can also be used if the specified key is not here, will
	% return an identical table.

	lazy_hashtable:display( "Lazy hashtable", MyLH6 ),



	test_facilities:display( "Looking up for ~ts in map hashtable: ~p",
		[ ?MyFirstKey, map_hashtable:lookup_entry( ?MyFirstKey, MyMH5 ) ] ),

	{ value, "MyFirstValue" } =
		map_hashtable:lookup_entry( ?MyFirstKey, MyMH5 ),

	test_facilities:display( "Removing that entry." ),
	MyMH6 = map_hashtable:remove_entry( ?MyFirstKey, MyMH5 ),
	false = map_hashtable:is_empty( MyMH6 ),
	test_facilities:display( "Looking up for ~ts in map hashtable: ~p",
		[ ?MyFirstKey, map_hashtable:lookup_entry( ?MyFirstKey, MyMH6 ) ] ),

	key_not_found = map_hashtable:lookup_entry( ?MyFirstKey, MyMH6 ),


	test_facilities:display( "Looking up for ~ts in list hashtable: ~p",
		[ ?MyFirstKey, list_table:lookup_entry( ?MyFirstKey, MyL5 ) ] ),

	{ value, "MyFirstValue" } = list_table:lookup_entry( ?MyFirstKey,
															MyL5 ),

	test_facilities:display( "Removing that entry." ),
	MyL6 = list_table:remove_entry( ?MyFirstKey, MyL5 ),
	false = list_table:is_empty( MyL6 )
		,
	test_facilities:display( "Looking up for ~ts in list hashtable: ~p",
		[ ?MyFirstKey, list_table:lookup_entry( ?MyFirstKey, MyL6 ) ] ),

	key_not_found = list_table:lookup_entry( ?MyFirstKey, MyL6 ),

	% remove_entry can also be used if the specified key is not here, will
	% return an identical table.
	%
	list_table:display( "List hashtable", MyL6 ),


	display_separator(),


	test_facilities:display( "Testing double key registering." ),

	MyH7 = hashtable:add_entry( ?MyThirdKey, anything, MyH6 ),
	hashtable:display( MyH7 ),

	test_facilities:display( "Enumerating the hash table: ~p",
		[ hashtable:enumerate( MyH6 ) ] ),

	test_facilities:display( "Listing the hash table keys: ~p",
		[ hashtable:keys( MyH6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey, ?MyThirdKey ],
										 hashtable:keys( MyH6 ) ),

	MyH8 = hashtable:add_entries( [ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyH7 ),

	MyH9 = hashtable:merge( MyH4, MyH8 ),
	test_facilities:display( "Merged table: ~ts, size of buckets is ~B.",
		[ hashtable:to_string( MyH9 ), hashtable:get_bucket_count( MyH9 ) ] ),

	MyH10 = hashtable:optimise( MyH9 ),
	test_facilities:display( "The optimised table: ~ts size of buckets is ~B.",
		[ hashtable:to_string( MyH10 ), hashtable:get_bucket_count( MyH10 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys in table ~p:"
		"~n ~p", [ Keys, hashtable:select_entries( Keys, MyH10 ) ] ),



	MyTH7 = tracked_hashtable:add_entry( ?MyThirdKey, anything, MyTH6 ),
	tracked_hashtable:display( MyTH7 ),

	test_facilities:display( "Enumerating the tracked hash table: ~p.",
		[ tracked_hashtable:enumerate( MyTH6 ) ] ),

	test_facilities:display( "Listing the tracked table keys: ~p.",
		[ tracked_hashtable:keys( MyTH6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey,?MyThirdKey ],
										 tracked_hashtable:keys( MyTH6 ) ),

	MyTH8 = tracked_hashtable:add_entries(
							[ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyTH7 ),

	MyTH9 = tracked_hashtable:merge( MyTH4, MyTH8 ),

	test_facilities:display( "Merged tracked table: ~ts",
				[ tracked_hashtable:to_string( MyTH9 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys ~p in tracked table:"
		" ~n ~p", [ Keys, tracked_hashtable:select_entries( Keys, MyTH9 ) ] ),



	MyLH7 = lazy_hashtable:add_entry( ?MyThirdKey, anything, MyLH6 ),
	lazy_hashtable:display( MyLH7 ),

	test_facilities:display( "Enumerating the lazy table: ~p.",
		[ lazy_hashtable:enumerate( MyLH6 ) ] ),

	test_facilities:display( "Listing the lazy table keys: ~p.",
		[ lazy_hashtable:keys( MyLH6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey, ?MyThirdKey ],
										 lazy_hashtable:keys( MyLH6 ) ),

	MyLH8 = lazy_hashtable:add_entries(
			[ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyLH7 ),

	MyLH9 = lazy_hashtable:merge( MyLH4, MyLH8 ),

	test_facilities:display( "Merged lazy table: ~ts",
			[ lazy_hashtable:to_string( MyLH9 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys in lazy table ~p:"
		"~n ~p", [ Keys, lazy_hashtable:select_entries( Keys, MyLH9 ) ] ),




	MyMH7 = map_hashtable:add_entry( ?MyThirdKey, anything, MyMH6 ),
	map_hashtable:display( MyMH7 ),

	test_facilities:display( "Enumerating the map table: ~p.",
		[ map_hashtable:enumerate( MyMH6 ) ] ),

	test_facilities:display( "Listing the map table keys: ~p.",
		[ map_hashtable:keys( MyMH6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey, ?MyThirdKey ],
										 map_hashtable:keys( MyMH6 ) ),

	MyMH8 = map_hashtable:add_entries(
			[ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyMH7 ),

	MyMH9 = map_hashtable:merge( MyMH4, MyMH8 ),


	test_facilities:display( "Merged map table: ~ts",
							 [ map_hashtable:to_string( MyMH9 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys in map table ~p:"
		"~n ~p", [ Keys, map_hashtable:select_entries( Keys, MyMH9 ) ] ),



	MyL7 = list_table:add_entry( ?MyThirdKey, anything, MyL6 ),
	list_table:display( MyL7 ),

	test_facilities:display( "Enumerating the list table: ~p.",
		[ list_table:enumerate( MyL6 ) ] ),

	test_facilities:display( "Listing the list table keys: ~p.",
		[ list_table:keys( MyL6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey, ?MyThirdKey ],
										 list_table:keys( MyL6 ) ),

	MyL8 = list_table:add_entries(
			[ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyL7 ),

	MyL9 = list_table:merge( MyL4, MyL8 ),


	test_facilities:display( "Merged list table: ~ts",
			[ list_table:to_string( MyL9 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys in list table ~p:"
		"~n ~p", [ Keys, list_table:select_entries( Keys, MyL9 ) ] ).



-define( series_count, 12 ).
%-define( series_count, 120 ).


% Returns a suitable set of 12*7=84 (presumably the typical size of a table of
% interest) key/value pairs (respectively an atom and a value of various types)
% to test tables.
%
get_pairs() ->
	get_pairs( _Series=?series_count, _Count=1, _Acc=[] ).


get_pairs( _Series=0, _Count, Acc ) ->
	Acc;

get_pairs( Series, Count, Acc ) ->

	ToAddStrings = [

			 { io_lib:format( "key-~B", [ Count + 1 ] ) , self() },
			 { io_lib:format( "key-~B", [ Count + 2 ] ) , "hello world!" },
			 { io_lib:format( "key-~B", [ Count + 3 ] ) , an_atom },
			 { io_lib:format( "key-~B", [ Count + 4 ] ) , [ "a", 123, list ] },
			 { io_lib:format( "key-~B", [ Count + 5 ] ) , { 23, 45, 67, 90 } },
			 { io_lib:format( "key-~B", [ Count + 6 ] ) , 1.0 },
			 { io_lib:format( "key-~B", [ Count + 7 ] ) , << "A binary" >> }

	],

	ToAdd = [ { text_utils:string_to_atom( lists:flatten( K ) ), V }
			  || { K, V } <- ToAddStrings ],

	get_pairs( Series - 1, Count + length( ToAdd ), ToAdd ++ Acc ).



% Returns different key/value pairs (the same keys associated to different
% values, and as many new keys as well).
%
get_other_pairs( Pairs ) ->
	get_other_pairs( Pairs, _Acc=[] ).

get_other_pairs( _Pairs=[], Acc ) ->

	NewPairs = get_pairs( _Series=?series_count, _Count=length( Acc ) + 1,
						  [] ),

	list_utils:random_permute( Acc ++ NewPairs );

get_other_pairs( _Pairs=[ { K, _V } | T ], Acc ) ->

	% Same key, different value (should not matter much):
	NewAcc = [ { K, io_lib:format( "Value for ~p", [ K ] ) } | Acc ],

	get_other_pairs( T, NewAcc ).




-define( run_count, 100 ).


% Feeds specified table and returns the corresponding average duration, in
% milliseconds (on average over Count runs).
%
feed_table( Table, Module, Pairs ) ->

	Count = ?run_count,

	Start = time_utils:get_precise_timestamp(),

	NewTables = [ lists:foldl( fun( { K, V }, T ) ->
									Module:add_entry( K, V, T )
							   end,
							   _Acc0=Table,
							   _List=Pairs ) || _C <- lists:seq( 1, Count ) ],

	Stop = time_utils:get_precise_timestamp(),

	Timing = time_utils:get_precise_duration( Start, Stop ) / Count,

	[ First | Others ] = NewTables,

	% Checking:
	[ First = OtherTable || OtherTable <- Others ],

	{ Module, First, Timing }.



% Updates specified table and returns the corresponding average duration, in
% milliseconds (on average over Count runs).
%
update_table( Table, Module, Pairs ) ->

	Count = ?run_count,

	Start = time_utils:get_precise_timestamp(),

	NewTables = [ lists:foldl( fun( { K, V }, T ) ->
									Module:add_entry( K, V, T )
							   end,
							   _Acc0=Table,
							   _List=Pairs ) || _C <- lists:seq( 1, Count ) ],

	Stop = time_utils:get_precise_timestamp(),

	Timing = time_utils:get_precise_duration( Start, Stop ) / Count,

	[ First | Others ] = NewTables,

	% Checking:
	[ First = OtherTable || OtherTable <- Others ],

	{ Module, First, Timing }.



% Benchmarks look-up durations.
benchmark_look_ups( Table, Module, Pairs ) ->

	%test_facilities:display( "Benchmarking ~ts on:~n~ts",
	%		   [ Module, Module:to_string( Table ) ] ),

	Count = 100 * ?run_count,

	Start = time_utils:get_precise_timestamp(),

	_Values = [ lists:foldl( fun( { K, V }, T ) ->
								V = Module:get_value( K, T ),
								T
							 end,
							 _Acc0=Table,
							 _List=Pairs ) || _C <- lists:seq( 1, Count ) ],

	Stop = time_utils:get_precise_timestamp(),

	Timing = time_utils:get_precise_duration( Start, Stop ) / Count,

	{ Module, Timing }.



% Runs performance tests, to compare the various table implementations.
run_performance_tests() ->

	Implementations = [ hashtable, tracked_hashtable, lazy_hashtable,
						map_hashtable, list_table ],

	% Change the order to see whether result variy:
	ActualImplementations = lists:reverse( Implementations ),

	EmptyTables = [ { M, M:new() } || M <- ActualImplementations ],

	Pairs = get_pairs(),

	test_facilities:display(
	  "Feeding empty tables with ~B initial key/value pairs.",
	  [ length( Pairs ) ] ),

	% Do it 5 times at blank to avoid transition effects:
	_FedTablesWithTimings = [ [ feed_table( T, M, Pairs )
					 || { M, T } <- EmptyTables ] || _C <- lists:seq( 1, 5 ) ],

	FedTablesWithTimings = [ feed_table( T, M, Pairs )
							 || { M, T } <- EmptyTables ],

	FedTimeStrings = [ text_utils:format( "for ~ts: ~.3f ms", [ M, Timing ] )
				   || { M, _T, Timing } <- FedTablesWithTimings ],

	test_facilities:display( "~nFeed durations: ~ts",
				[ text_utils:strings_to_string( FedTimeStrings ) ] ),



	FedSizeStrings = [ text_utils:format( "for ~ts: ~ts", [ M,
		 system_utils:interpret_byte_size_with_unit( basic_utils:size( T ) ) ] )
					   || { M, T, _Timing } <- FedTablesWithTimings ],

	test_facilities:display( "~nSizes: ~ts",
			   [ text_utils:strings_to_string( FedSizeStrings ) ] ),


	OtherPairs = get_other_pairs( Pairs ),

	test_facilities:display(
	  "~n~nUpdating these tables with ~B key/value pairs "
	  "(equal mix of updated and new keys).", [ length( OtherPairs ) ] ),

	FedTables = [ { M, T } || { M, T, _Timing } <- FedTablesWithTimings ],

	UpdatedTablesWithTimings = [ update_table( T, M, OtherPairs )
							   || { M, T } <- FedTables ],


	UpTimeStrings = [ text_utils:format( "for ~ts: ~.3f ms", [ M, Timing ] )
					  || { M, _T, Timing } <- UpdatedTablesWithTimings ],

	test_facilities:display( "~nUpdate durations: ~ts",
			   [ text_utils:strings_to_string( UpTimeStrings ) ] ),


	UpSizeStrings = [ text_utils:format( "for ~ts: ~ts", [ M,
		system_utils:interpret_byte_size_with_unit( basic_utils:size( T ) ) ] )
					  || { M, T, _Timing } <- UpdatedTablesWithTimings ],

	test_facilities:display( "~nSizes: ~ts",
				[ text_utils:strings_to_string( UpSizeStrings ) ] ),


	test_facilities:display(
	  "~nBenchmarking look-ups (with no optimisation)." ),

	ShuffledPairs = list_utils:random_permute( OtherPairs ),

	LookedUpTimings = [ benchmark_look_ups( T, M, ShuffledPairs )
						|| { M, T, _Timings } <- UpdatedTablesWithTimings ],

	LookedUpStrings = [ text_utils:format( "for ~ts: ~.1f microsec",
										   [ M, 1000 * Timing ] )
						|| { M, Timing } <- LookedUpTimings ],

	test_facilities:display( "~nLook-up durations: ~ts",
				[ text_utils:strings_to_string( LookedUpStrings ) ] ),


	OptimisedTables = [ { M, M:optimise( T ), undefined }
						|| { M, T, _Timing } <- UpdatedTablesWithTimings ],


	test_facilities:display(
	  "~nBenchmarking look-ups (after optimisation)." ),

	NewLookedUpTimings = [ benchmark_look_ups( T, M, ShuffledPairs )
						   || { M, T, _Timings } <- OptimisedTables ],

	NewLookedUpStrings = [ text_utils:format( "for ~ts: ~.1f microsec",
											  [ M, 1000 * Timing ] )
						   || { M, Timing } <- NewLookedUpTimings ],

	test_facilities:display( "~nLook-up durations: ~ts",
			   [ text_utils:strings_to_string( NewLookedUpStrings ) ] ),


	FinalTablesWithTimings = UpdatedTablesWithTimings,

	AllListedTables = [ { _, FirstList } | Others ] =
		[ { M, lists:sort( M:enumerate( T ) ) }
		  || { M, T, _Timing } <- FinalTablesWithTimings ],

	Hashes = [ { M, erlang:phash2( L ) }
			   || { M, L } <- AllListedTables ],

	%test_facilities:display( "Checking content: ~ts",
	%    [ text_utils:strings_to_string(
	%									  [ io_lib:format( "for ~ts: ~ts",
	%												 [ M, M:to_string( T ) ] )
	%			   || { M, T, _Timing } <- FinalTablesWithTimings ] ) ] ),

	test_facilities:display( "~nChecking hashes: ~ts",
		[ text_utils:strings_to_string(
			[ io_lib:format( "for ~ts: ~p", [ M, H ] )
			  || { M, H } <- Hashes ] ) ] ),

	test_facilities:display( "~nChecking final states: ok" ),

	%test_facilities:display( "Reference: ~ts",
	%                         [ hashtable:to_string( First ) ] ),

	[ FirstList= OtherListedTable || { _, OtherListedTable } <- Others ].
