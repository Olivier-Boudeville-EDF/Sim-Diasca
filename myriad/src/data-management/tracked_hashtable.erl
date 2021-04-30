% Copyright (C) 2011-2021 Olivier Boudeville
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
% Creation date: November 10, 2011.
% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


% Tracked hashtable implementation.
%
% See tracked_hashtable_test.erl for the corresponding test.
% See hashtable.erl
%
%
% We provide different multiple types of hashtables, including:
%
% - 'hashtable', the most basic, safest, reference implementation - and quite
% efficient as well
%
% - 'tracked_hashtable' (this module), an attempt of optimisation of it (not
% necessarily the best)
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



% However this tracked version is deemed less effective than the lazy version,
% and thus is not updated/tested as much as the others (for example: error cases
% have not been uniformised, insofar that they can still issue badmatches while
% other implementations raise more proper exceptions).



% A tracked_hashtable is a { Hashtable, NumberOfEntries, NumberOfBuckets }
% triplet where:
%
% - Hashtable is a hashtable(), refer to the hashtable module for more detail
%
% - NumberOfEntries represents the number of entries in the hashtable; it is
% zero when a new hashtable is created
%
% - NumberOfBuckets is the number of buckets of the internal hashtable; a
% default number of buckets is chosen at the creation of a new hashtable
%
% Directly depends on the hashtable module.
%
-module(tracked_hashtable).


% Same as hashtable:
-export([ new/0, new/1, new_with_buckets/1, add_entry/3, add_entries/2,
		  remove_entry/2, lookup_entry/2, has_entry/2, get_value/2,
		  extract_entry/2, get_value_with_defaults/3, get_values/2,
		  get_all_values/2,
		  add_to_entry/3, subtract_from_entry/3, toggle_entry/2,
		  append_to_entry/3, delete_from_entry/3, pop_from_entry/2,
		  enumerate/1, select_entries/2, keys/1, values/1,
		  is_empty/1, size/1,
		  map_on_entries/2, map_on_values/2,
		  fold_on_entries/3,
		  merge/2, optimise/1, to_string/1, to_string/2,
		  display/1, display/2 ]).


-type key() :: hashtable:key().

-type value() :: hashtable:value().

-type entry() :: hashtable:entry().

-type entries() :: [ entry() ].

-type entry_count() :: basic_utils:count().


% Not supported since Erlang 18.0:
%
%-opaque tracked_hashtable( K, V ) ::
%		  { hashtable:hashtable( K, V ), hashtable:entry_count(),
%			hashtable:bucket_count() }.

-opaque tracked_hashtable() :: { hashtable:hashtable(), hashtable:entry_count(),
								 hashtable:bucket_count() }.


-export_type([ key/0, value/0, entry/0, entries/0, entry_count/0,
			   tracked_hashtable/0 ]).


% We want to be able to use our size/1 from here as well:
-compile( { no_auto_import, [ size/1 ] } ).


% Shorthands:

-type accumulator() :: basic_utils:accumulator().

-type ustring() :: text_utils:ustring().
-type entries() :: hashtable:entries().



% Implementation notes:
%
% Each time the content of the internal hashtable is modified, the meta-data
% must be updated; if the number of entries stored in the table changed, the
% load factor of this hashtable is updated; if it is not anymore between the
% accepted bounds, the hashtable is then be optimised




% Creates a new empty tracker table.
%
% A new empty tracked hashtable is returned.
%
-spec new() -> tracked_hashtable().
new() ->
	% Starts at minimal size, otherwise will soon be shrunk anyway:
	NewHashtable = hashtable:new_with_buckets( 1 ),
	{ NewHashtable, _EntryCount=0, _BucketCount=1 }.



% As tracked hashtables manage by themselves their size, no need to specify any
% target size. This function is only defined so that we can transparently switch
% APIs with the hashtable module.
%
new( _ExpectedNumberOfEntries ) ->
	new().



% Defined also to allow seamless change of hashtable modules:
%
new_with_buckets( _NumberOfBuckets ) ->
	new().


% Adds specified key/value pair into the specified tracked hash table.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
% As the load factor of the tracked hashtable is verified at each additional
% entry, the tracked hashtable is optimised as soon as possible.
%
-spec add_entry( key(), value(), tracked_hashtable() ) -> tracked_hashtable().
add_entry( Key, Value,
		 _TrackedHashtable={ Hashtable, EntryCount, NumberOfBuckets } ) ->

	% We pay inter-module calls to preserve hashtable opaqueness.

	% A problem is that we must distinguish between a new key being added
	% (impacting the load factor) or being updated (load factor unchanged).

	{ Newhashtable, Diagnosis } = hashtable:add_diagnosed_entry( Key, Value,
			Hashtable ),

	% Should a new element be added, we verify and optimise the hashtable if
	% necessary:
	%
	case Diagnosis of

		updated ->
			% Bucket size did not change, so we just updated an existing entry,
			% and there is no need to optimise:
			{ Newhashtable, EntryCount, NumberOfBuckets };

		added ->

			% Here a new key has been added, we might have to optimise:
			case hashtable:must_optimise( EntryCount, NumberOfBuckets ) of

				true ->

					Entries = hashtable:enumerate( Newhashtable ),

					OptimisedTable = hashtable:optimise_unconditionally(
						 EntryCount+1, NumberOfBuckets, Entries,
						 Newhashtable ),

					{ OptimisedTable, EntryCount+1,
					  hashtable:get_bucket_count( OptimisedTable ) };

				false ->
					{ Newhashtable, EntryCount+1, NumberOfBuckets }

			end

	end.



% Adds specified list of key/value pairs into the specified hashtable.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec add_entries( entries(), tracked_hashtable() )	-> tracked_hashtable().
add_entries( EntryList,
		_TrackedHashtable={ Hashtable, _EntryCount, NumberOfBuckets } ) ->

	% We want to optimise only at end (to avoid useless reshuffles with longer
	% entry lists) yet counting the total number of entries correctly
	% (w.r.t. duplicated keys):
	%
	AugmentedTable = hashtable:add_entries( EntryList, Hashtable ),

	Entries = hashtable:enumerate( AugmentedTable ),

	% Depends on key collisions (cannot be predicted):
	NewEntryCount = length( Entries ),

	case hashtable:must_optimise( NewEntryCount, NumberOfBuckets ) of

		true ->

			OptimisedTable = hashtable:optimise_unconditionally( NewEntryCount,
						NumberOfBuckets, Entries, AugmentedTable ),
			{ OptimisedTable, NewEntryCount,
			  hashtable:get_bucket_count( OptimisedTable ) };

		false ->

			{ AugmentedTable, NewEntryCount, NumberOfBuckets }

	end.




% Removes specified key/value pair from the specified hash table.
%
% Does nothing if the key is not found.
%
-spec remove_entry( key(), tracked_hashtable() ) -> tracked_hashtable().
remove_entry( Key, TrackedHashtable={ Hashtable, EntryCount, BucketCount } ) ->

	case hashtable:remove_diagnosed_entry( Key, Hashtable ) of

		{ deleted, NewTable } ->

			NewEntryCount = EntryCount - 1,

			% Here an entry has been removed, we might have to optimise:
			case hashtable:must_optimise( NewEntryCount, BucketCount ) of

				true ->

					Entries = hashtable:enumerate( NewTable ),

					OptimisedTable = hashtable:optimise_unconditionally(
							NewEntryCount, BucketCount, Entries, NewTable ),

					{ OptimisedTable, NewEntryCount,
					  hashtable:get_bucket_count( OptimisedTable ) };

				false ->
					{ NewTable, NewEntryCount, BucketCount }

			end;

		unchanged ->
			TrackedHashtable

	end.



% Looks-up specified entry (designated by its key) in specified tracked
% hashtable.
%
% Returns either 'key_not_found' if no such key is registered in the
% table, or {value,Value}, with Value being the value associated to the
% specified key.
%
-spec lookup_entry( key(), tracked_hashtable() ) ->
							'key_not_found' | { 'value', value() }.
lookup_entry( Key, _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:lookup_entry( Key, Hashtable ).



% Looks-up specified entry (designated by its key) in specified tracked
% hashtable: returns eigher true or false.
%
-spec has_entry( key(), tracked_hashtable() ) -> boolean().
has_entry( Key, _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:has_entry( Key, Hashtable ).



% Retrieves the value corresponding to specified key and returns it directly.
%
% The key/value pair is expected to exist already, otherwise a bad match is
% triggered.
%
-spec get_value( key(), tracked_hashtable() ) -> value().
get_value( Key, _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:get_value( Key, Hashtable ).



% Extracts specified entry from specified hashtable, i.e. returns the associated
% value and removes that entry from the table.
%
% The key/value pair is expected to exist already, otherwise an exception is
% raised.
%
-spec extract_entry( key(), tracked_hashtable() ) ->
							{ value(), tracked_hashtable() }.
extract_entry( Key, _TrackedHashtable={ Hashtable, NEnt, NBuck } ) ->

	{ Value, NewHashtable } = hashtable:extract_entry( Key, Hashtable ),

	NewTrackedTable = { NewHashtable, NEnt - 1, NBuck },

	{ Value, NewTrackedTable }.



% Looks for specified entry in specified table and, if found, returns the
% associated value; otherwise returns the specified default value.
%
-spec get_value_with_defaults( key(), value(), tracked_hashtable() ) -> value().
get_value_with_defaults( Key, DefaultValue,
					  _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:get_value_with_defaults( Key, DefaultValue, Hashtable ).



% Returns the (ordered) list of values that correspond to the specified
% (ordered) list of keys of this table.
%
% The key/value pairs are expected to exist already, otherwise an exception is
% raised.
%
% Ex: [Color, Age, Mass] = tracked_hashtable:get_values([color, age, mass],
% MyTable])
%
-spec get_values( [ key() ], tracked_hashtable() ) -> [ value() ].
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
% Ex: [Color=red, Age=23, Mass=51 ] = tracked_hashtable:get_all_values(
%                 [color, age, mass], [{color, red}, {mass, 51}, {age, 23}])
%
-spec get_all_values( [ key() ], tracked_hashtable() ) -> [ value() ].
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
-spec map_on_entries( fun( ( entry() ) -> entry() ), tracked_hashtable() ) ->
						  tracked_hashtable().
map_on_entries( Fun, _TrackedHashtable={ Hashtable, _NEnt, _NBuck }  ) ->

	NewHashtable = hashtable:map_on_entries( Fun, Hashtable ),

	% Might have changed as well:
	NEnt = hashtable:size( NewHashtable ),
	NBuck = hashtable:get_bucket_count( NewHashtable ),

	{ NewHashtable, NEnt, NBuck }.



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
-spec map_on_values( fun( ( value() ) -> value() ), tracked_hashtable() ) ->
							tracked_hashtable().
map_on_values( Fun, _TrackedHashtable={ Hashtable, NEnt, NBuck }  ) ->

	NewHashtable = hashtable:map_on_values( Fun, Hashtable ),

	{ NewHashtable, NEnt, NBuck }.



% Folds specified anonymous function on all entries of the specified tracked
% hashtable.
%
% The order of transformation for entries is not specified.
%
% Returns the final accumulator.
%
-spec fold_on_entries( fun( ( entry(), accumulator() ) -> accumulator() ),
					   accumulator(), tracked_hashtable() ) -> accumulator().
fold_on_entries( Fun, InitialAcc, _TrackedHashtable={ Hashtable, _NEnt, _NBuck }
			 ) ->
	hashtable:fold_on_entries( Fun, InitialAcc, Hashtable ).



% Returns a new tracked hashtable, which started from TrackedHashtableBase and
% was enriched with the TrackedHashtableAdd entries whose keys where not
% already in TrackedHashtableBase (if a key is in both tables, the one from
% TrackedHashtableBase will be kept).
%
-spec merge( tracked_hashtable(), tracked_hashtable() ) -> tracked_hashtable().
merge( _TrackedHashtableBase={ HashtableBase, _NEntB, _NBuckB },
	 _TrackedHashtableAdd={ HashtableAdd, _NEntA, _NBuckA } ) ->

	UpdatedHashtable = hashtable:merge( HashtableBase, HashtableAdd ),
	Entries = hashtable:enumerate( UpdatedHashtable ),

	% Depends on key collisions (cannot be predicted):
	NewEntryCount = length( Entries ),

	UpdatedTableSize = hashtable:get_bucket_count( UpdatedHashtable ),

	case hashtable:must_optimise( NewEntryCount, UpdatedTableSize ) of

		true ->
			OptimisedTable = hashtable:optimise_unconditionally( NewEntryCount,
					UpdatedTableSize, Entries, UpdatedHashtable ),

			{ OptimisedTable, NewEntryCount,
			  hashtable:get_bucket_count( OptimisedTable ) };

		false ->
			{ UpdatedHashtable, NewEntryCount, UpdatedTableSize }

	end.




% Optimises this hashtable.
%
% A no-operation for tracked hashtables.
%
-spec optimise( tracked_hashtable() ) -> tracked_hashtable().
optimise( Hashtable ) ->
	Hashtable.



% Adds a specified value to the value of specified key, supposed the existing
% one to be numerical,
%
% A case clause is triggered if the key did not exist; a bad arithm is triggered
% if no addition can be performed on the associated value.
%
-spec add_to_entry( key(), number(), tracked_hashtable() ) ->
							tracked_hashtable().
add_to_entry( Key, Value, TrackedHashtable ) ->
	{ value, Number } = lookup_entry( Key, TrackedHashtable ),
	add_entry( Key, Number+Value, TrackedHashtable ).



% Subtracts specified value to the value, supposed to be numerical, associated
% to specified key.
%
% A case clause is triggered if the key did not exist, a bad arithm is triggered
% if no subtraction can be performed on the associated value.
%
-spec subtract_from_entry( key(), number(), tracked_hashtable() ) ->
									tracked_hashtable().
subtract_from_entry( Key, Value, TrackedHashtable ) ->
	{ value, Number } = lookup_entry( Key, TrackedHashtable ),
	add_entry( Key, Number-Value, TrackedHashtable ).



% Toggles the boolean value associated with specified key: if true will be
% false, if false will be true.
%
% A case clause is triggered if the entry does not exist or it is not a boolean
% value.
%
-spec toggle_entry( key(), tracked_hashtable() ) -> tracked_hashtable().
toggle_entry( Key,
			  _TrackedHashtable={ Hashtable, EntryCount, NumberOfBuckets } ) ->

	{ hashtable:toggle_entry( Key, Hashtable ), EntryCount, NumberOfBuckets }.



% Appends specified element to the value of specified key, supposing the value
% to be a list.
%
% A case clause is triggered if the entry does not exist.
%
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
%
-spec append_to_entry( key(), term(), tracked_hashtable() ) ->
								tracked_hashtable().
append_to_entry( Key, Element, TrackedHashtable ) ->
	{ value, List } = lookup_entry( Key, TrackedHashtable ),
	add_entry( Key, [ Element | List ], TrackedHashtable ).



% Deletes the first match of specified element from the value associated to
% specified key, that value being supposed to be a list.
%
% A case clause is triggered if the entry did not exist.
% If the element is not in the specified list, the list will not be modified.
%
-spec delete_from_entry( key(), term(), tracked_hashtable() ) ->
								tracked_hashtable().
delete_from_entry( Key, Element, TrackedHashtable ) ->
	{ value, List } = lookup_entry( Key, TrackedHashtable ),
	add_entry( Key, lists:delete( Element, List ), TrackedHashtable ).



% Pops the head of the value (supposed to be a list) associated to specified
% key, and returns a pair made of the popped head and the new hashtable.
%
-spec pop_from_entry( key(), tracked_hashtable() ) ->
							{ term(), tracked_hashtable() }.
pop_from_entry( Key, TrackedHashtable ) ->
	{ value, [ H | T ] } = lookup_entry( Key, TrackedHashtable ),
	{ H, add_entry( Key, T, TrackedHashtable ) }.



% Returns a flat list whose elements are all the key/value pairs of the
% hashtable.
%
% Ex: [ {K1,V1}, {K2,V2}, ... ].
%
-spec enumerate( tracked_hashtable() ) -> entries().
enumerate( _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	lists:flatten( tuple_to_list( Hashtable ) ).



% Returns a list of key/value pairs corresponding to the list of specified keys,
% or throws a badmatch is at least one key is not found.
%
-spec select_entries( [ key() ], tracked_hashtable() ) -> entries().
select_entries( Keys, _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->

	hashtable:select_entries( Keys, Hashtable ).


% Returns a list containing all the keys of this hashtable.
-spec keys( tracked_hashtable() ) -> [ key() ].
keys( _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:keys( Hashtable ).


% Returns a list containing all the values of this hashtable.
%
% Ex: useful if the key was used as an index to generate this table first.
%
-spec values( tracked_hashtable() ) -> [ value() ].
values( _TrackedHashtable={ Hashtable, _NEnt, _NBuck }  ) ->
	hashtable:values( Hashtable ).



% Returns whether the specified hashtable is empty (not storing any key/value
% pair).
%
-spec is_empty( tracked_hashtable() ) -> boolean().
is_empty( _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:is_empty( Hashtable ).



% Returns the size (number of entries, i.e. of key/value pairs) of the specified
% table.
%
-spec size( tracked_hashtable() ) -> hashtable:entry_count().
size( _TrackedHashTable={ _Hashtable, NEntries, _NBuckets } ) ->
	NEntries.



% Returns a textual description of the specified hashtable.
-spec to_string( tracked_hashtable() ) -> ustring().
to_string( _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:to_string( Hashtable ).



% Returns a textual description of the specified hashtable, with specified
% display setting.
%
-spec to_string( tracked_hashtable(), 'internal' | 'user_friendly' ) ->
						ustring().
to_string( _TrackedHashtable={ Hashtable, _NEnt, _NBuck }, DescriptionType ) ->
	hashtable:to_string( Hashtable, DescriptionType ).


% Displays the specified hashtable on the standard output.
-spec display( tracked_hashtable() ) -> void().
display( _TrackedHashtable={ Hashtable, _ElementCount, NumberOfBuckets } ) ->

	hashtable:display( Hashtable ),
	io:format( " and its bucket size is ~B.~n", [ NumberOfBuckets ] ).



% Displays the specified hashtable on the standard output, with the specified
% title on top.
%
-spec display( ustring(), tracked_hashtable() ) -> void().
display( Title,
		 _TrackedHashtable={ Hashtable, _ElementCount, NumberOfBuckets } ) ->

	hashtable:display( Title, Hashtable ),
	io:format( " and its bucket size is ~B.~n", [ NumberOfBuckets ] ).
