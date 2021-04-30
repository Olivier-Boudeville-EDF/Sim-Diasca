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
% Creation date: July 2, 2007.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Generic hash table implementation.
% See hashtable_test.erl for the corresponding test.


% An hashtable is basically a tuple whose size (number of elements) is the
% number of buckets in the hashtable. Each element of the tuple is a list
% containing key/value pairs.
%
%
% We provide different multiple types of hashtables, including:
%
% - 'hashtable' (this module), the most basic, safest, reference implementation
% - and quite efficient as well
%
% - 'tracked_hashtable', an attempt of optimisation of it (not necessarily the
% best)
%
% - 'lazy_hashtable', deciding to optimise in a less costly way
% than 'tracked_hashtable'
%
% - 'map_hashtable', which is probably the most efficient implementation
% (speed/size compromise)
%
% - 'list_table', a list-based implementation, efficient for smaller tables (and
% only them)
%
% They are to provide the same API (signatures and contracts).
%
-module(hashtable).


% To avoid code duplication yet having fastest speed:
-compile( { inline, [ get_bucket_index/2 ] } ).


% We want to be able to use our size/1 from here as well:
-compile( { no_auto_import, [ size/1 ] } ).



% Directly depends on the text_utils module.

% Heavily inspired of the tupleStore example from 'Concurrent Programming in
% Erlang' (Joe Armstrong), section 9.8.



% The hashtable is implemented thanks to a tuple whose size is the number of
% buckets specified at the hashtable creation.
%
% Each tuple element (hence each bucket) is a list of key/value pairs.

% Maybe the ETS module, proplists, dict, etc. could/should be used instead.

% When the table holds less than 50 elements, probably that using functions like
% lists:keystore/4 and lists:keymember/3 would be faster.


% There is no function that is specific to this implementation, to enforce
% substitutability.


% The standard hashtable API:
-export([ new/0, new/1,
		  add_entry/3, add_diagnosed_entry/3,
		  add_entries/2, add_diagnosed_entries/2,
		  remove_entry/2, remove_diagnosed_entry/2,
		  lookup_entry/2, has_entry/2, get_value/2, extract_entry/2,
		  get_value_with_defaults/3, get_values/2, get_all_values/2,
		  add_to_entry/3, subtract_from_entry/3, toggle_entry/2,
		  append_to_entry/3, delete_from_entry/3, pop_from_entry/2,
		  enumerate/1, select_entries/2, keys/1, values/1,
		  is_empty/1, size/1,
		  map_on_entries/2, map_on_values/2,
		  fold_on_entries/3,
		  merge/2, optimise/1, to_string/1, to_string/2,
		  display/1, display/2 ]).



% These functions are exported only to ease the alternate *_hashtable
% implementation, so that we can switch implementations.
%
% Not intended to be used in user code.
%
-export([ new_with_buckets/1, delete_bucket/3, delete_bucket_verbose/3,
		  replace_bucket/4,
		  get_bucket_index_for/2, get_bucket_count/1, get_ideal_bucket_count/1,
		  must_optimise/2, optimise_unconditionally/4 ]).


-define( default_bullet, " + " ).

% The default expected number of entries:
-define(default_entry_count,32).


% Not necessarily an atom, but surely not a string (as lists are interpreted as
% lists of keys):
%
-type key() :: number() | atom() | binary() | pid() | tuple().

-type value() :: term().

-type entry() :: { key(), value() }.
-type entry( K, V ) :: { K, V }.

-type entries() :: [ entry() ].
-type entries( K, V ) :: [ { K, V } ].

-type entry_count() :: basic_utils:count().

-type bucket_count() :: pos_integer().

-type bucket() :: [ entries() ].
-type bucket( K, V ) :: [ entries( K, V ) ].


% A problem is that the number of buckets (hence the size of the tuple) is
% determined at runtime:
%
-opaque hashtable() :: tuple().

% Since 18.0, type tuple/1 does not seem to exist anymore:
%-opaque hashtable( K, V ) :: tuple( bucket( K, V ) ).

% A type of bullet (ex: " * "):
-type bullet() :: ustring().

-type description_type() :: bullet() | 'user_friendly' | 'full' | 'internal'.


-export_type([ key/0, value/0, entry/0, entry/2, entries/0, entries/2,
			   entry_count/0, bucket/0, bucket/2, bucket_count/0,
			   hashtable/0, bullet/0, description_type/0 ]).


% Shorthands:
-type ustring() :: text_utils:ustring().



% Returns a new empty hashtable dimensioned for the default number of entries.
-spec new() -> hashtable().
new() ->
	new( ?default_entry_count ).



% Returns a new empty hashtable dimensioned for the specified expected number of
% entries.
%
-spec new( entry_count() | entries() ) -> hashtable().
new( ExpectedNumberOfEntries ) when is_integer( ExpectedNumberOfEntries ) ->

	NumberOfBuckets = get_ideal_bucket_count( ExpectedNumberOfEntries ),

	create_tuple( NumberOfBuckets, _DefaultValue=[] );


new( InitialEntries ) when is_list( InitialEntries ) ->

	BlankTable = new(),

	add_entries( InitialEntries, BlankTable ).



% Returns a new empty hashtable dimensioned with the specified number of
% buckets.
%
% (helper)
%
-spec new_with_buckets( bucket_count() ) -> hashtable().
new_with_buckets( NumberOfBuckets ) ->
	create_tuple( NumberOfBuckets, _DefaultValue=[] ).




% Adds specified key/value pair into the specified hashtable.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec add_entry( key(), value(), hashtable() ) -> hashtable().
add_entry( Key, Value, Hashtable ) ->

	KeyIndex = get_bucket_index( Key, Hashtable ),

	% Retrieve appropriate bucket:
	PreviousBucket = element( KeyIndex, Hashtable ),

	NewBucket = replace_bucket( Key, Value, PreviousBucket, [] ),

	setelement( KeyIndex, Hashtable, NewBucket ).



% Adds specified key/value pair into the specified hashtable, and returns an
% update diagnosis.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec add_diagnosed_entry( key(), value(), hashtable() ) ->
								{ hashtable(), 'added' | 'updated' }.
add_diagnosed_entry( Key, Value, Hashtable ) ->

	KeyIndex = get_bucket_index( Key, Hashtable ),

	% Retrieve appropriate bucket:
	PreviousBucket = element( KeyIndex, Hashtable ),

	{ Diagnosis, NewBucket } = replace_bucket_diagnose( Key, Value,
														PreviousBucket, [] ),

	NewTable = setelement( KeyIndex, Hashtable, NewBucket ),

	{ NewTable, Diagnosis }.



% Adds specified list of key/value pairs into the specified hashtable.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec add_entries( entries(), hashtable() ) -> hashtable().
add_entries( _EntryList=[], Hashtable ) ->
	Hashtable;

add_entries( [ { EntryName, EntryValue } | Rest ], Hashtable ) ->
	add_entries( Rest, add_entry( EntryName, EntryValue, Hashtable ) ).



% Adds specified list of key/value pairs into the specified hashtable, and
% returns an update diagnosis.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec add_diagnosed_entries( entries(), hashtable() ) ->
									{ hashtable(), 'added' | 'updated' }.
add_diagnosed_entries( Entries, Hashtable ) ->
	lists:foldl( fun( _Entry={K,V}, _Acc={ Table, _Diag='added' } ) ->
						 NewTable = add_entry( K, V, Table ),
						 { NewTable, added };

					% Implicitly, Diag is 'updated' here:
					( _Entry={K,V}, _Acc={ Table, _Diag } ) ->
						 % Returns directly { NewTable, NewDiagnosis }:
						 add_diagnosed_entry( K, V, Table )

					end,
					_InitialAcc={ Hashtable, _InitialDiag=updated },
					_List=Entries ).



% Removes specified key/value pair, as designated by the key, from the specified
% hashtable.
%
% Does nothing if the key is not found.
%
% Returns an updated table.
%
-spec remove_entry( key(), hashtable() ) -> hashtable().
remove_entry( Key, Hashtable ) ->

	KeyIndex = get_bucket_index( Key, Hashtable ),

	PreviousBucket = element( KeyIndex, Hashtable ),

	NewBucket = delete_bucket( Key, PreviousBucket, _Acc=[] ),

	setelement( KeyIndex, Hashtable, NewBucket ).



% Removes specified key/value pair, as designated by the key, from the specified
% hashtable.
%
% Does nothing if the key is not found.
%
% Returns a diagnosis and an updated table.
%
-spec remove_diagnosed_entry( key(), hashtable() ) ->
						{ 'deleted', hashtable() } | 'unchanged'.
remove_diagnosed_entry( Key, Hashtable ) ->

	KeyIndex = get_bucket_index( Key, Hashtable ),

	PreviousBucket = element( KeyIndex, Hashtable ),

	case delete_bucket_verbose( Key, PreviousBucket, _Acc=[] ) of

		% Diagnosis is either 'deleted' or 'unchanged'
		{ deleted, NewBucket } ->
			NewTable = setelement( KeyIndex, Hashtable, NewBucket ),
			{ deleted, NewTable };

		unchanged ->
			unchanged

	end.



% Looks-up specified entry (designated by its key) in specified hashtable.
%
% Returns either 'key_not_found' if no such key is registered in the
% table, or { value, Value }, with Value being the value associated to the
% specified key.
%
-spec lookup_entry( key(), hashtable() ) ->
						'key_not_found' | { 'value', value() }.
lookup_entry( Key, Hashtable ) ->
	lookup_in_list( Key, element( get_bucket_index( Key, Hashtable ),
		Hashtable ) ).



% Tells whether the specified key exists in the table: returns true or false.
-spec has_entry( key(), hashtable() ) -> boolean().
has_entry( Key, Hashtable ) ->

	case lookup_in_list( Key,
			element( get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, _Value } ->
			true;

		% key_not_found ->
		_ ->
			false

	end.



% Retrieves the value corresponding to specified (existing) key and returns it
% directly.
%
% The key/value pair is expected to exist already, otherwise an exception is
% raised.
%
-spec get_value( key(), hashtable() ) -> value().
get_value( Key, Hashtable ) ->

	case lookup_in_list( Key, element( get_bucket_index( Key, Hashtable ),
									   Hashtable ) ) of

		% Most likely case first:
		{ value, Value } ->
			Value;

		%key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { key_not_found, Key } )

	end.



% Extracts specified entry from specified hashtable, i.e. returns the associated
% value and removes that entry from the table.
%
% The key/value pair is expected to exist already, otherwise an exception is
% raised.
%
-spec extract_entry( key(), hashtable() ) -> { value(), hashtable() }.
extract_entry( Key, Hashtable ) ->

	BucketIndex = get_bucket_index( Key, Hashtable ),

	case extractFromList( Key, element( BucketIndex, Hashtable ) ) of

		key_not_found ->

			% Badmatches are not informative enough:
			throw( { key_not_found, Key } );


		{ Value, ShortenBucket } ->

			NewTable = erlang:setelement( BucketIndex, _Tuple=Hashtable,
										  _Value=ShortenBucket ),

			{ Value, NewTable }

	end.



% Looks for specified entry in specified table and, if found, returns the
% associated value; otherwise returns the specified default value.
%
-spec get_value_with_defaults( key(), value(), hashtable() ) -> value().
get_value_with_defaults( Key, DefaultValue, Hashtable ) ->

	case lookup_in_list( Key, element( get_bucket_index( Key, Hashtable ),
									 Hashtable ) ) of

		% Most likely case first:
		{ value, Value } ->
			Value;

		%key_not_found ->
		_ ->
			DefaultValue

	end.



% Returns the (ordered) list of values that correspond to the specified
% (ordered) list of keys of this table.
%
% The key/value pairs are expected to exist already, otherwise an exception is
% raised.
%
% Ex: [ Color, Age, Mass ] = hashtable:get_values( [ color, age, mass ],
%   MyTable ] )
%
-spec get_values( [ key() ], hashtable() ) -> [ value() ].
get_values( Keys, Hashtable ) ->

	{ RevValues, _FinalTable } = lists:foldl(

				fun( _Elem=Key, _Acc={ Values, Table } ) ->

					   { Value, ShrunkTable } = extract_entry( Key, Table ),
					   { [ Value | Values ], ShrunkTable }

				end,
				_Acc0={ [], Hashtable },
				_List=Keys ),

	lists:reverse( RevValues ).



% Returns the (ordered) list of values that correspond to the specified
% (ordered) list of keys of this table, ensuring all entries have been read,
% otherwise throwing an exception.
%
% The key/value pairs are expected to exist already, otherwise an exception is
% raised.
%
% Ex: [ Color=red, Age=23, Mass=51 ] = hashtable:get_all_values( [ color, age,
%         mass ], [ {color, red}, {mass, 51}, {age, 23} ] )
%
-spec get_all_values( [ key() ], hashtable() ) -> [ value() ].
get_all_values( Keys, Hashtable ) ->

	{ RevValues, FinalTable } = lists:foldl(
		   fun( _Elem=Key, _Acc={ Values, Table } ) ->

				{ Value, ShrunkTable } = extract_entry( Key, Table ),
				{ [ Value | Values ], ShrunkTable }

		   end,
		   _Acc0={ [], Hashtable },
		   _List=Keys ),

	case is_empty( FinalTable ) of

		true ->
			lists:reverse( RevValues );

		false ->
			throw( { remaining_keys, keys( FinalTable ) } )

	end.



% Applies (maps) the specified anonymous function to each of the key-value
% entries contained in this hashtable.
%
% Allows to apply "in-place" an operation on all entries without having to
% enumerate the content of the hashtable and iterate on it (hence without having
% to duplicate the whole content in memory).
%
% Note: as the fun may return modified keys, the whole structure of the
% hashtable may change (ex: different buckets used for replaced entries,
% colliding keys resulting in having less entries afterwards, etc.).
%
% One may request the returned hashtable to be optimised after this call.
%
-spec map_on_entries( fun( ( entry() ) -> entry() ), hashtable() ) ->
							hashtable().
map_on_entries( Fun, Hashtable ) ->

	BucketList = tuple_to_list( Hashtable ),

	BlankHashtable = new(),

	% We have to rebuild the table, as entries might be modified by the
	% function, hence their hash may change:
	%
	map_on_entries( Fun, BucketList, BlankHashtable ).



% Returns a new hashtable, with the entries from specified bucket list
% transformed.
%
% (helper)
%
map_on_entries( _Fun, _BucketList=[], Hashtable ) ->
	Hashtable;

map_on_entries( Fun, _BucketList=[ Bucket | T ], Hashtable ) ->

	NewHashtable = lists:foldl(
		fun( Entry, AccTable ) ->

			{ NewKey, NewValue } = Fun( Entry ),

			% NewKey may not be in the same bucket as Key:
			add_entry( NewKey, NewValue, AccTable )

		end,
		_InitialAcc=Hashtable,
		_List=Bucket ),

	map_on_entries( Fun, T, NewHashtable ).



% Applies (maps) the specified anonymous function to each of the values
% contained in this hashtable.
%
% Allows to apply "in-place" an operation on all values without having to
% enumerate the content of the hashtable and iterate on it (hence without having
% to duplicate the whole content in memory).
%
% Note: the keys are left as are, hence the structure of the hashtable does not
% change.
%
-spec map_on_values( fun( ( value() ) -> value() ), hashtable() ) ->
						   hashtable().
map_on_values( Fun, Hashtable ) ->

	BucketList = tuple_to_list( Hashtable ),

	NewBucketList = [ map_bucket_for_values( Fun, Bucket )
					  || Bucket <- BucketList ],

	list_to_tuple( NewBucketList ).



% Maps specified function to all values of specified bucket.
%
% (helper)
%
map_bucket_for_values( Fun, Bucket ) ->
	[ { K, Fun( V ) } || { K, V } <- Bucket ].



% Folds specified anonymous function on all entries of the specified hashtable.
%
% The order of transformation for entries is not specified.
%
% Returns the final accumulator.
%
-spec fold_on_entries( fun( ( entry(), basic_utils:accumulator() )
						  -> basic_utils:accumulator() ),
					 basic_utils:accumulator(),
					 hashtable() ) -> basic_utils:accumulator().
fold_on_entries( Fun, InitialAcc, Hashtable ) ->

	BucketList = tuple_to_list( Hashtable ),

	fold_on_entries_helper( Fun, BucketList, InitialAcc ).



% (helper)
%
% Could be itself a fold!
%
fold_on_entries_helper( _Fun, _BucketList=[], Acc ) ->
	Acc;

fold_on_entries_helper( Fun, _BucketList=[ Bucket | T ], Acc ) ->

	NewAcc = lists:foldl( Fun, Acc, _List=Bucket ),

	fold_on_entries_helper( Fun, T, NewAcc ).




% Adds specified value to the value, supposed to be numerical, associated to
% specified key.
%
% An exception is thrown if the key does not exist, a bad arithm is triggered if
% no addition can be performed on the associated value.
%
-spec add_to_entry( key(), number(), hashtable() ) -> hashtable().
add_to_entry( Key, Value, Hashtable ) ->

	case lookup_in_list( Key,
		element( get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, Number } ->
			add_entry( Key, Number + Value, Hashtable );

		%key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { key_not_found, Key } )

	end.



% Subtracts specified value to the value, supposed to be numerical, associated
% to specified key.
%
% An exception is thrown if the key does not exist, a bad arithm is triggered if
% no subtraction can be performed on the associated value.
%
-spec subtract_from_entry( key(), number(), hashtable() ) -> hashtable().
subtract_from_entry( Key, Value, Hashtable ) ->

	case lookup_in_list( Key,
		element( get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, Number } ->
			add_entry( Key, Number - Value, Hashtable );

		%key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { key_not_found, Key } )

	end.




% Toggles the boolean value associated with specified key: if true will be
% false, if false will be true.
%
% An exception is thrown if the key does not exist or if its associated value is
% not a boolean.
%
-spec toggle_entry( key(), hashtable() ) -> hashtable().
toggle_entry( Key, Hashtable ) ->

	case lookup_in_list( Key,
			element( get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, true } ->
			add_entry( Key, false, Hashtable );

		{ value, false } ->
			add_entry( Key, true, Hashtable );

		{ value, Other } ->
			throw( { non_boolean_value, Other } );

		%key_not_found ->
		_ ->
			throw( { key_not_found, Key } )

	end.



% Returns a new hashtable, which started from HashtableBase and was enriched
% with the HashtableAdd entries whose keys where not already in HashtableBase
% (if a key is in both tables, the one from HashtableBase will be kept).
%
-spec merge( hashtable(), hashtable() ) -> hashtable().
merge( HashtableBase, HashtableAdd ) ->

	% Uses the fact that when two entries with the same key are added, the final
	% associated value is the one of the latest to be added.

	lists:foldl(
		fun( { Key, Value }, Acc ) -> add_entry( Key, Value, Acc ) end,
		_InitialAcc=HashtableAdd,
		_List=enumerate( HashtableBase ) ).



% Appends specified element to the value, supposed to be a list, associated to
% specified key.
%
% An exception is thrown if the key does not exist.
%
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
%
-spec append_to_entry( key(), term(), hashtable() ) -> hashtable().
append_to_entry( Key, Element, Hashtable ) ->

	case lookup_in_list( Key,
		element( get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, List } ->
			add_entry( Key, [ Element | List ], Hashtable );

		%key_not_found ->
		_ ->
			throw( { key_not_found, Key } )

	end.



% Deletes the first match of the specified element in the value associated to
% specified key, this value being assumed to be a list.
%
% An exception is thrown if the key does not exist.
%
% If the element is not in the specified list, the list will not be modified.
%
-spec delete_from_entry( key(), term(), hashtable() ) -> hashtable().
delete_from_entry( Key, Element, Hashtable ) ->

	case lookup_in_list( Key,
		element( get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, List } ->
			add_entry( Key, lists:delete( Element, List ), Hashtable );

		%key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { key_not_found, Key } )

	end.



% Pops the head of the value (supposed to be a list) associated to specified
% key, and returns a pair made of the popped head and of the new hashtable.
%
-spec pop_from_entry( key(), hashtable() ) -> { term(), hashtable() }.
pop_from_entry( Key, Hashtable ) ->

	case lookup_entry( Key, Hashtable ) of

		{ value, [ H | T ] } ->
			{ H, add_entry( Key, T, Hashtable ) };

		%key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { key_not_found, Key } )

	end.




% Returns a flat list whose elements are all the key/value pairs of the
% hashtable, in no particular order.
%
% Ex: [{K1,V1}, {K2,V2}, ...].
%
-spec enumerate( hashtable() ) -> entries().
enumerate( Hashtable ) ->
	lists:flatten( tuple_to_list( Hashtable ) ).



% Returns a list of key/value pairs corresponding to the list of specified keys,
% or throws a badmatch is at least one key is not found.
%
-spec select_entries( [ key() ], hashtable() ) -> entries().
select_entries( Keys, Hashtable ) ->
	select_entries( Keys, Hashtable, _Acc=[] ).

select_entries( _Keys=[], _Hashtable, Acc ) ->
	Acc;

select_entries( _Keys=[ K | T ], Hashtable, Acc ) ->

	case lookup_entry( K, Hashtable ) of

		{ value, V } ->
			select_entries( T, Hashtable, [ { K, V } | Acc ] );

		%key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { key_not_found, K } )

	end.



% Returns a list containing all the keys of this hashtable.
-spec keys( hashtable() ) -> [ key() ].
keys( Hashtable ) ->
	get_keys_from_buckets( tuple_to_list( Hashtable ), _Acc=[] ).



% Returns a list containing all the values of this hashtable.
%
% Ex: useful if the key was used as an index to generate this table first.
%
-spec values( hashtable() ) -> [ value() ].
values( Hashtable ) ->
	get_values_from_buckets( tuple_to_list( Hashtable ), _Acc=[] ).



% Returns whether the specified hashtable is empty (not storing any key/value
% pair).
%
-spec is_empty( hashtable() ) -> boolean().
is_empty( Hashtable ) ->
	BucketList = tuple_to_list( Hashtable ),
	is_empty_helper( BucketList ).


% Tells whether the specified list of lists is empty.
is_empty_helper( [] ) ->
	true;

is_empty_helper( [ _L=[] | T ] ) ->
	is_empty_helper( T );

is_empty_helper( _Any ) ->
	% Here we have an overall list which is not empty, whose first element is
	% itself not an empty list, thus there is at least one entry and we can stop
	% here:
	%
	false.


% Returns the size (number of entries, i.e. of key/value pairs) of the specified
% table.
%
-spec size( hashtable() ) -> entry_count().
size( Hashtable ) ->

	lists:foldl( fun( Bucket, Sum ) ->
						 Sum + length( Bucket )
				 end,
				 _InitialAcc=0,
				 _List=tuple_to_list( Hashtable ) ).



% Optimises this hashtable with regard to its load factor (see
% http://en.wikipedia.org/wiki/Hash_table#Load_factor).
%
% To be called whenever the size of a given hashtable is not expected to change
% substantially. The principle is to determine the optimal number of buckets for
% the current number of stored entries, allowing to perform fast look-ups and to
% use the right amount of memory for that (i.e. to rely on the best CPU vs RAM
% trade-off).
%
-spec optimise( hashtable() ) -> hashtable().
optimise( Hashtable ) ->

	% Like size/1, but allows to re-use Entries:
	Entries = enumerate( Hashtable ),
	EntryCount = length( Entries ),

	% Number of elements of the underlying tuple:
	BucketCount = tuple_size( Hashtable ),

	case must_optimise( EntryCount, BucketCount ) of

		% Outside bounds, re-hash:
		true ->
			optimise_unconditionally( EntryCount, BucketCount, Entries,
									  Hashtable );

		false ->
			Hashtable

	end.



% Returns whether an optimisation ought to be triggered.
%
% Too high a load factor (more than 0.75) induces slow look-ups, too small (less
% than 0.5) wastes memory:
%
-spec must_optimise( entry_count(), bucket_count() ) -> boolean().
must_optimise( EntryCount, BucketCount ) ->

	% BucketCount is expected to be never null:
	LoadFactor = ( EntryCount + 1 ) / BucketCount,

	( LoadFactor < 0.5 ) orelse ( LoadFactor > 0.75 ).



% Performs an optimisation of the specified hashtable.
-spec optimise_unconditionally( entry_count(), bucket_count(), entries(),
								hashtable() ) -> hashtable().
optimise_unconditionally( EntryCount, CurrentBucketCount, Entries,
						  Hashtable ) ->

	IdealBucketCount = get_ideal_bucket_count( EntryCount ),

	% Avoids useless reshuffles (ex: due to rounding errors):
	case IdealBucketCount of

		CurrentBucketCount ->
			Hashtable;

		_ ->
			NewTable = new_with_buckets( IdealBucketCount ),
			add_entries( Entries, NewTable )

	end.



% Returns a textual description of the specified hashtable.
-spec to_string( hashtable() ) -> ustring().
to_string( Hashtable ) ->
	to_string( Hashtable, user_friendly ).



% Returns a textual description of the specified table.
%
% Either a bullet is specified, or the returned string is ellipsed if needed (if
% using 'user_friendly'), or quite raw and non-ellipsed (if using 'full'), or
% even completly raw ('internal').
%
-spec to_string( hashtable(), description_type() ) -> ustring().
to_string( Hashtable, DescriptionType ) ->

	case enumerate( Hashtable ) of

		[] ->
			"empty table";

		[ { K, V } ] ->
			case DescriptionType of

				user_friendly ->
					text_utils:format_ellipsed( "table with a single entry, "
						"key being ~p, value being ~p", [ K, V ] );

				_ ->
					text_utils:format( "table with a single entry, "
						"key being ~p, value being ~p", [ K, V ] )

			end;


		L ->

			% Enforces a consistent order; flatten below is needed, in order to
			% use the result with ~ts:
			%
			case DescriptionType of

				user_friendly ->
					Strs = [ text_utils:format_ellipsed( "~p: ~p", [ K, V ] )
							 || { K, V } <- lists:sort( L ) ],

					lists:flatten( io_lib:format( "table with ~B entries: ~ts",
						[ length( L ), text_utils:strings_to_string( Strs,
												   ?default_bullet ) ] ) );

				full ->
					Strs = [ text_utils:format( "~p: ~p", [ K, V ] )
							 || { K, V } <- lists:sort( L ) ],

					lists:flatten( io_lib:format( "table with ~B entries: ~ts",
						[ length( L ),
						  text_utils:strings_to_string( Strs,
														?default_bullet ) ] ) );

				internal ->
					lists:foldl(

					  fun( Bucket, Acc ) ->
							Acc ++ io_lib:format( "  + ~ts~n",
								[ bucket_to_string( Bucket ) ] )
					  end,

					  _Acc0=io_lib:format( "table with ~B bucket(s) and ~B "
						  "entry(ies): ~n",
						  [ tuple_size( Hashtable ), size( Hashtable ) ] ),

					  _List=tuple_to_list( Hashtable ) );


				% Here, ellipsed and with specified bullet:
				Bullet ->
					Strs = [ text_utils:format_ellipsed( "~p: ~p", [ K, V ] )
							 || { K, V } <- lists:sort( L ) ],

					lists:flatten( io_lib:format( "table with ~B entries: ~ts",
						[ length( L ),
						  text_utils:strings_to_string( Strs, Bullet ) ] ) )

			end

	end.



% Displays the specified hashtable on the standard output.
-spec display( hashtable() ) -> void().
display( Hashtable ) ->
	io:format( "~ts~n", [ to_string( Hashtable ) ] ).



% Displays the specified hashtable on the standard output, with the specified
% title on top.
%
-spec display( ustring(), hashtable() ) -> void().
display( Title, Hashtable ) ->
	io:format( "~ts:~n~ts~n", [ Title, to_string( Hashtable ) ] ).




% Section for helper functions.



% Returns the ideal number of buckets needed for specified number of entries.
-spec get_ideal_bucket_count( entry_count() ) -> bucket_count().
get_ideal_bucket_count( EntryCount ) ->

	IdealLoadFactor = 0.65,

	% To avoid requesting zero bucket:
	erlang:max( round( EntryCount / IdealLoadFactor ), 1 ).


% Returns a new tuple, whose size is the specified length and whose elements are
% all set to specified default value.
%
create_tuple( _Length=0, _DefaultValue ) ->
	throw( at_least_one_bucket_per_hashtable );

create_tuple( Length, DefaultValue ) ->
	create_tuple( Length, DefaultValue, _Acc=[] ).


% Final step:
create_tuple( _N=0, _DefaultValue, Acc ) ->
	list_to_tuple( Acc );


% Building from n-1 to n elements:
create_tuple( N, DefaultValue, Acc ) ->
	create_tuple( N-1, DefaultValue, [ DefaultValue | Acc ] ).



% Removes the (first) entry pair whose key matches the specified one, if any.
%
% (returns an identical list if the key is not found)
%
delete_bucket( Key, [ { Key, _Value } | T ], Acc ) ->
	% Forget the pair if the key if matching, and just stop:
	lists:append( T, Acc );

delete_bucket( Key, [ H | T ], Acc ) ->
	% Keeps everything else (non-matching entries):
	delete_bucket( Key, T, [ H | Acc ] );

delete_bucket( _Key, [], Acc ) ->
	% Nothing at all was deleted in this call:
	Acc.



% Returns, if an entry with the specified key was found, { 'deleted', NewBucket
% }, i.e. a pair made of an atom telling whether a deletion was done, and a list
% whose first entry having a matching key is removed, otherwise 'unchanged'.
%
% (like delete_bucket/3, but gives more information, used for example by
% tracked_hashtable)
%
-spec delete_bucket_verbose( key(), entries(), entries() ) ->
				{ 'deleted', entries() } | 'unchanged'.
delete_bucket_verbose( Key, _Entries=[ { Key, _Value } | T ], Acc ) ->
	% Forget the pair if the key if matching, and stops:
	{ deleted, lists:append( T, Acc ) };

delete_bucket_verbose( Key, _Entries=[ H | T ], Acc ) ->
	% Keeps everything else (non-matching entries):
	delete_bucket_verbose( Key, T, [ H | Acc ] );

delete_bucket_verbose( _Key, _Entries=[], _Acc ) ->
	% Nothing was deleted in this call:
	unchanged.



% Replaces, in specified list, a key/value pair by another.
%
% Updates the pair if this key was already declared, otherwise add the new
% entry.
%
% Note: order does not matter.
%
-spec replace_bucket( key(), value(), entries(), entries() ) -> entries().
replace_bucket( Key, Value, _Entries=[], Acc ) ->
	% Key was not there previously, just adding it:
	[ { Key, Value } | Acc ];

replace_bucket( Key, Value, _Entries=[ { Key, _ } | T ], Acc ) ->
	% Add the key, join the two parts of the list and return it:
	[ { Key, Value } | lists:append( T, Acc ) ];

replace_bucket( Key, Value, _Entries=[ H | T ], Acc ) ->
	% Another key, continue iterating:
	replace_bucket( Key, Value, T, [ H | Acc ] ).



% Replaces in specified list a key/value pair by another, and tells whether it
% is an addition or an update.
%
% Updates the pair if this key was already declared, otherwise add the new
% entry.
%
% (like replace_bucket/4, but gives more information, used for example by
% tracked_hashtable)
%
% Note: order does not matter.
%
% Returns { Diagnosis, NewBucket }.
%
replace_bucket_diagnose( Key, Value, _RestOfBucket=[], Acc ) ->
	% Key was not there previously, just adding it:
	{ added, [ { Key, Value } | Acc ] };

replace_bucket_diagnose( Key, Value, [ { Key, _ } | T ], Acc ) ->
	% Add the key, join the two parts of the list and return it:
	{ updated,[ { Key, Value } | lists:append( T, Acc ) ] };

replace_bucket_diagnose( Key, Value, [ H | T ], Acc ) ->
	% Another key, continue iterating:
	replace_bucket_diagnose( Key, Value, T, [ H | Acc ] ).



% Returns the number of buckets in this hashtable.
%
% Not intended to be used by user code.
%
% (helper)
%
-spec get_bucket_count( hashtable() ) -> bucket_count().
get_bucket_count( Hashtable ) ->
	tuple_size( Hashtable ).



% Returns a string describing a hashtable bucket (list of key/value pairs):
bucket_to_string( _EmptyBucket=[] ) ->
	"empty bucket";

bucket_to_string( Bucket ) ->
	lists:foldl(

		fun( { Key, Value }, Acc ) ->
			Acc ++ io_lib:format( "     * ~w -> ~ts~n",
				[ text_utils:term_to_string( Key ),
				  text_utils:term_to_string( Value ) ] )
		end,

		io_lib:format( "bucket with ~B element(s):~n", [ length( Bucket ) ] ),

		Bucket ).



% Returns the value corresponding to the key in the specified list.
lookup_in_list( _Key, _TargetList=[] ) ->

	% We hesitated and considered returning the key since, if this function is
	% used like '{value,V} = hashtable:lookup_in_list( K,L)', if the key is not
	% found, the raised 'badmatch' will directly specify the offending key
	% instead of a mere {badmatch,key_not_found}.
	%
	% However now get_value/2 throws an exception and should be used instead.

	%{key_not_found, Key};
	key_not_found;

lookup_in_list( Key, _TargetList=[ { Key, Value } | _T ] ) ->
	{ value, Value };

lookup_in_list( Key, _TargetList=[ _H | T ] ) ->
	lookup_in_list( Key, T ).



% Returns the value corresponding to the key in the specified list, and the list
% without this entry: { Value, ShortenList }, or 'key_not_found'.
%
extractFromList( Key, TargetList ) ->
	extractFromList( Key, TargetList, _AccList=[] ).


extractFromList( _Key, _TargetList=[], _AccList ) ->
	%{ key_not_found, Key };
	key_not_found;

extractFromList( Key, _TargetList=[ { Key, Value } | T ], AccList ) ->
	% Entry order does not matter:
	{ Value, T ++ AccList };

extractFromList( Key, _TargetList=[ H | T ], AccList ) ->
	extractFromList( Key, T, [ H | AccList ] ).




% Iterates over buckets and fetches the keys.
get_keys_from_buckets( _Buckets=[], Acc ) ->
	Acc;

get_keys_from_buckets(  _Buckets=[ H | T ], Acc ) ->
	get_keys_from_buckets( T, [ Key || { Key, _Value } <- H ] ++ Acc ).



% Iterates over buckets and fetches the values.
get_values_from_buckets( _Buckets=[], Acc ) ->
	Acc;

get_values_from_buckets( _Buckets=[ H | T ], Acc ) ->
	get_values_from_buckets( T, [ Value || { _Key, Value } <- H ] ++ Acc ).



% Returns the number of the bucket associated to specified key in specified
% hashtable.
%
% If having N buckets, returns a value in [1,N].
%
% Typically defined to avoid code duplication, but meant to be inlined.
%
-spec get_bucket_index( key(), hashtable() ) -> bucket_count().
get_bucket_index( Key, Hashtable ) ->
	erlang:phash2( Key, tuple_size( Hashtable ) ) + 1.



% Returns the number of the bucket associated to specified key in specified
% hashtable.
%
% If having N buckets, returns a value in [1,N].
%
% Defined (exactly as get_bucket_index/2, which is defined for inlining) and
% exported only for opaqueness purposes.
%
-spec get_bucket_index_for( key(), hashtable() ) -> bucket_count().
get_bucket_index_for( Key, Hashtable ) ->
	erlang:phash2( Key, tuple_size( Hashtable ) ) + 1.
