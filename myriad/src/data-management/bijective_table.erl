% Copyright (C) 2019-2022 Olivier Boudeville
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
% Creation date: Saturday, May 4, 2019.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% @doc Datastructure allowing to perform <b>bidirectional conversions between
% two sets</b>.
%
% One can see it as a `[{first_type(), second_type()}]' associative table
% allowing to transform any element of a set into a corresponding element of the
% other one.
%
-module(bijective_table).


-export([ new/1, get_first_for/2, get_second_for/2, to_string/1 ]).


-opaque bijective_table() :: bijective_table( any(), any() ).


-opaque bijective_table( F, S ) :: { table:table( F, S ), table:table( S, F ) }.
% Internally, two tables used, one for each direction of conversion.


-type first_type() :: any().
-type second_type() :: any().

-type entry() :: { first_type(), second_type() }.

-type entries() :: [ entry() ].


-export_type([ bijective_table/0, bijective_table/2 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Returns a new bijective table allowing a two-way conversion between
% specified (initial) entries.
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



% @doc Returns the element of the first type that corresponds to the specified
% element of the second type.
%
-spec get_first_for( second_type(), bijective_table() ) -> first_type().
get_first_for( Second,
			   _BijTable={ _FirstToSecondTable, SecondToFirstTable } ) ->
	table:get_value( Second, SecondToFirstTable ).



% @doc Returns the element of the second type that corresponds to the specified
% element of the first type.
%
-spec get_second_for( first_type(), bijective_table() ) -> second_type().
get_second_for( First,
				_BijTable={ FirstToSecondTable, _SecondToFirstTable } ) ->
	table:get_value( First, FirstToSecondTable ).



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
