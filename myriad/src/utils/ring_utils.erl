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
% Creation date: July 1, 2007.


% Gathering of various facilities about rings (infinite lists, looping on
% themselves).
%
% See ring_utils_test.erl for the corresponding test.
%
-module(ring_utils).



% A ring behaves as an (infinite) list whose next element after its last is its
% first again.
%
% Internally, the first list is the working one (from which elements may be
% extracted), while the second is a copy of the full reference one.
%
-opaque ring() :: { list(), list() }.

-opaque ring( T ) :: { [ T ], [ T ] }.


% Ring-related operations:
-export([ from_list/1, to_list/1, head/1, get_next/2, get_reference_list/1,
		  size/1, to_string/1 ]).



-export_type([ ring/0, ring/1 ]).


% Shorthands:

-type count() :: basic_utils:count().
-type ustring() :: text_utils:ustring().



% Ring-related section (infinite, circular buffer whose end is connected to its
% beginning).



% Returns a ring corresponding to the specified list.
-spec from_list( list() ) -> ring().
from_list( InputList ) ->
	{ InputList, InputList }.



% Returns a list corresponding to the current state of specified ring.
-spec to_list( ring() ) -> list().
to_list( Ring={ _WorkingList, ReferenceList } ) ->
	{ List, _NewRing } = get_next( _Count=length( ReferenceList ), Ring ),
	List.


% Pops the head of specified ring: returns {Head,UpdatedRing}.
-spec head( ring() ) -> { term(), ring() }.
head( _Ring={ _WorkingList=[], ReferenceList } ) ->
	% Replenish:
	%
	% Dialyzer does not want an opaque argument to be used:
	%head( { ReferenceList, ReferenceList } );
	head( from_list( ReferenceList ) );

head( _Ring={ _WorkingList=[ H | T ], ReferenceList } ) ->
	{ H, { T, ReferenceList } }.



% Returns a list of the Count popped elements (in their order in the ring), and
% the corresponding updated ring.
%
% Ex: for a new ring based on [a, b, c, d], if Count=6 then
% [a, b, c, d, a, b] will be returned.
%
-spec get_next( count(), ring() ) -> { [ term() ], ring() }.
get_next( Count, Ring ) ->
	% Quite similar to a map:foldl/3:
	get_next_helper( Count, Ring, _Acc=[] ).


get_next_helper( _Count=0, Ring, Acc ) ->
	{ lists:reverse( Acc ), Ring };

get_next_helper( Count, Ring, Acc ) ->
	{ H, NewRing } = head( Ring ),
	get_next_helper( Count-1, NewRing, [ H | Acc ] ).



% Returns the list from which the ring was created (in its original order).
-spec get_reference_list( ring() ) -> [ term() ].
get_reference_list( _Ring={ _WorkingList, ReferenceList } ) ->
	ReferenceList.


% Returns the number of elements in the specified ring.
-spec size( ring() ) -> count().
size( _Ring={ _WorkingList, ReferenceList } ) ->
	length( ReferenceList ).



% Returns a textual representation of the specified ring.
-spec to_string( ring() ) -> ustring().
to_string( Ring ) ->

	case to_list( Ring ) of

		[] ->
			"empty ring";

		[ Element ] ->
			text_utils:format( "ring with a single element, ~p",
							   [ Element ] );

		Elements ->

			ElemString = text_utils:strings_to_string( [
				   text_utils:format( "~p", [ E ] ) || E <- Elements ] ),

			text_utils:format( "ring with following ~B elements: ~ts",
							   [ length( Elements ), ElemString ] )

	end.
