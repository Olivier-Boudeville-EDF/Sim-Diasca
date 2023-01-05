% Copyright (C) 2019-2023 Olivier Boudeville
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
% Creation date: Saturday, May 4, 2019.


% @doc Datastructure allowing to perform <b>bidirectional conversions between
% two sets</b>.
%
% One can see it as a `[{first_type(), second_type()}]' associative table
% allowing to transform any element of a set into its (unique) corresponding
% element in the other one.
%
% See also const_bijective_table.erl for constant bijective tables that can be
% requested from any number (potentially extremely large) of callers very
% efficiently thanks to the generation (only in memory, or in file) of a
% corresponding module.
%
% Refer to:
% - bijective_table_test.erl for an usage example and testing thereof
% - const_bijective_table.erl for a constant, compile-time bijective table
%
-module(bijective_table).


-export([ new/0, new/1,

		  add_entry/2, add_entry/3, add_entries/2,
		  add_new_entry/2, add_new_entry/3, add_new_entries/2,

		  get_first_for/2, get_maybe_first_for/2, get_first_elements_for/2,
		  get_second_for/2, get_maybe_second_for/2, get_second_elements_for/2,

		  remove_entry_by_first/2, remove_entry_by_second/2,

		  to_string/1 ]).


-opaque bijective_table() :: bijective_table( any(), any() ).


-opaque bijective_table( F, S ) :: { table:table( F, S ), table:table( S, F ) }.
% Internally, two tables used, one for each direction of conversion.


-type first_type() :: any().
-type second_type() :: any().

-type entry() :: { first_type(), second_type() }.

-type entries() :: [ entry() ].
% Entries that can be fed to a bijective table.

-export_type([ bijective_table/0, bijective_table/2 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().


% @doc Returns a new, empty bijective table.
-spec new() -> bijective_table().
new() ->
	EmptyTable = table:new(),
	{ EmptyTable, EmptyTable }.


% @doc Returns a new bijective table allowing a two-way conversion between
% the specified (initial) entries.
%
-spec new( entries() ) -> bijective_table().
new( InitialEntries ) -> % list type tested by table:new/1:

	FirstToSecondTable = table:new( InitialEntries ),

	Reversed = [ { Second, First } || { First, Second } <- InitialEntries ],

	SecondToFirstTable = table:new( Reversed ),

	% Detect any unexpected duplicate:
	case { table:size( FirstToSecondTable ),
		   table:size( SecondToFirstTable ) } of

		{ S, S } ->
			{ FirstToSecondTable, SecondToFirstTable };

		%P={ S1, S2 } ->
		P ->
			throw( { non_bijective_sets, P,
					 table:enumerate( FirstToSecondTable ),
					 table:enumerate( SecondToFirstTable ) } )

	end.



% @doc Adds the specified (pair of) corresponding elements in the specified
% bijective table, possibly overwriting any already-existing entry, and returns
% this enriched table.
%
-spec add_entry( first_type(), second_type(), bijective_table() ) ->
												bijective_table().
add_entry( First, Second,
		   _BijTable={ FirstToSecondTable, SecondToFirstTable } ) ->
	NewFirstTable = table:add_entry( First, Second, FirstToSecondTable ),
	NewSecondTable = table:add_entry( Second, First, SecondToFirstTable ),
	{ NewFirstTable, NewSecondTable }.


% @doc Adds the specified entry in the specified bijective table, possibly
% overwriting any already-existing entry, and returns this enriched table.
%
-spec add_entry( entry(), bijective_table() ) -> bijective_table().
add_entry( _Entry={ First, Second }, BijTable ) ->
	add_entry( First, Second, BijTable ).


% @doc Adds the specified entries in the specified bijective table, possibly
% overwriting any already-existing entries, and returns this enriched table.
%
-spec add_entries( entries(), bijective_table() ) -> bijective_table().
add_entries( Entries, BijTable ) ->
	lists:foldl( fun( E, BijTableAcc ) ->
					add_entry( E, BijTableAcc )
				 end,
				 _List=Entries,
				 _InitialAcc=BijTable ).



% @doc Adds the specified (pair of) corresponding elements in the specified
% bijective table, none expected to be already registered, and returns this
% enriched table.
%
% Throws an exception should an element happened to be already registered.
%
-spec add_new_entry( first_type(), second_type(), bijective_table() ) ->
												bijective_table().
add_new_entry( First, Second,
			   _BijTable={ FirstToSecondTable, SecondToFirstTable } ) ->
	NewFirstTable = table:add_new_entry( First, Second, FirstToSecondTable ),
	NewSecondTable = table:add_new_entry( Second, First, SecondToFirstTable ),
	{ NewFirstTable, NewSecondTable }.


% @doc Adds the specified entry in the specified bijective table, none of the
% pair element expected to be already registered, and returns this enriched
% table.
%
-spec add_new_entry( entry(), bijective_table() ) -> bijective_table().
add_new_entry( _Entry={ First, Second }, BijTable ) ->
	add_new_entry( First, Second, BijTable ).


% @doc Adds the specified entries in the specified bijective table, none of the
% element of the pairs expected to be already registered, and returns this
% enriched table.
%
-spec add_new_entries( entries(), bijective_table() ) -> bijective_table().
add_new_entries( Entries, BijTable ) ->
	lists:foldl( fun( E, BijTableAcc ) ->
					add_new_entry( E, BijTableAcc )
				 end,
				 _List=Entries,
				 _InitialAcc=BijTable ).



% @doc Returns the element of the first type that corresponds to the specified
% element of the second type.
%
-spec get_first_for( second_type(), bijective_table() ) -> first_type().
get_first_for( Second,
			   _BijTable={ _FirstToSecondTable, SecondToFirstTable } ) ->
	table:get_value( Second, SecondToFirstTable ).


% @doc Returns the element of the first type (if any) that corresponds to the
% specified element of the second type.
%
-spec get_maybe_first_for( second_type(), bijective_table() ) ->
											maybe( first_type() ).
get_maybe_first_for( Second,
				_BijTable={ _FirstToSecondTable, SecondToFirstTable } ) ->
	case table:lookup_entry( Second, SecondToFirstTable ) of

		key_not_found ->
			undefined;

		{ value, First } ->
			First

	end.



% @doc Returns the elements of the first type that correspond to the specified
% elements of the second type.
%
-spec get_first_elements_for( [ second_type() ], bijective_table() ) ->
											[ first_type() ].
get_first_elements_for( SecondElems,
				_BijTable={ _FirstToSecondTable, SecondToFirstTable } ) ->
	table:get_values( SecondElems, SecondToFirstTable ).



% @doc Returns the element of the second type that corresponds to the specified
% element of the first type.
%
-spec get_second_for( first_type(), bijective_table() ) -> second_type().
get_second_for( First,
				_BijTable={ FirstToSecondTable, _SecondToFirstTable } ) ->
	table:get_value( First, FirstToSecondTable ).


% @doc Returns the element of the second type (if any) that corresponds to the
% specified element of the first type.
%
-spec get_maybe_second_for( first_type(), bijective_table() ) ->
											maybe( second_type() ).
get_maybe_second_for( First,
					  _BijTable={ FirstToSecondTable, _SecondToFirstTable } ) ->
	case table:lookup_entry( First, FirstToSecondTable ) of

		key_not_found ->
			undefined;

		{ value, Second } ->
			Second

	end.



% @doc Returns the elements of the first type that correspond to the specified
% elements of the first type.
%
-spec get_second_elements_for( [ first_type() ], bijective_table() ) ->
											[ second_type() ].
get_second_elements_for( FirstElems,
				_BijTable={ _SecondToFirstTable, FirstSecondtoTable } ) ->
	table:get_values( FirstElems, FirstSecondtoTable ).



% @doc Removes the specified entry, as designated by its first element, from the
% specified bijective table.
%
% Throws an exception if the corresponding entry is not found.
%
% Returns an updated table.
%
-spec remove_entry_by_first( first_type(), bijective_table() ) ->
											bijective_table().
remove_entry_by_first( First,
				_BijTable={ FirstToSecondTable, SecondToFirstTable } ) ->
	Second = table:get_value( First, FirstToSecondTable ),
	ShrunkFirstTable = table:remove_entry( First, FirstToSecondTable ),
	ShrunkSecondTable = table:remove_entry( Second, SecondToFirstTable ),
	{ ShrunkFirstTable, ShrunkSecondTable }.


% @doc Removes the specified entry, as designated by its second element, from
% the specified bijective table.
%
% Throws an exception if the corresponding entry is not found.
%
% Returns an updated table.
%
-spec remove_entry_by_second( second_type(), bijective_table() ) ->
												bijective_table().
remove_entry_by_second( Second,
				_BijTable={ FirstToSecondTable, SecondToFirstTable } ) ->
	First = table:get_value( Second, SecondToFirstTable ),
	ShrunkFirstTable = table:remove_entry( First, FirstToSecondTable ),
	ShrunkSecondTable = table:remove_entry( Second, SecondToFirstTable ),
	{ ShrunkFirstTable, ShrunkSecondTable }.



% @doc Returns a textual description of the specified bijective table.
-spec to_string( bijective_table() ) -> ustring().
to_string( _BijTable={ FirstToSecondTable, _SecondToFirstTable } ) ->

	case lists:sort( table:enumerate( FirstToSecondTable ) ) of

		[] ->
			"empty bijective table";

		Elems ->
			text_utils:format( "bijective table containing ~B element(s): ~ts",
				[ table:size( FirstToSecondTable ),
				  text_utils:strings_to_string(
					[ text_utils:format( "~p <-> ~p", [ F, S ] )
									|| { F, S } <- Elems ] ) ] )


	end.
