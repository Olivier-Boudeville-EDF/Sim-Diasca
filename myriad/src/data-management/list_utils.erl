% Copyright (C) 2007-2023 Olivier Boudeville
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


% @doc Gathering of various facilities about <b>lists</b>.
%
% See list_utils_test.erl for the corresponding test.
%
% See also: set_utils.erl and list_table.erl.
%
-module(list_utils).


% Note: if having an (at least mostly) constant list, possibly containing a
% large number of elements, and on which the main operation that shall be fast
% is element look-up (telling whether or not a given element is in the list),
% then the set type shall be preferred (see set_utils.erl for that).



% For list_impl:
-include("data_types.hrl").


% Checks regarding lists:
%
% (see also basic_utils:{check,are}_all_{,un}defined/1).
%
-export([ ensure_list/1, ensure_atoms/1, ensure_tuples/1, ensure_pids/1,
		  ensure_proplist/1,
		  are_integers/1, check_integers/1, are_pids/1, are_atoms/1,
		  check_strictly_ascending/1 ]).


% Basic list operations:
-export([ get_element_at/2, set_element_at/3, insert_element_at/3,
		  extract_element_at/2, extract_element_if_existing/2,
		  remove_first_elements/2, remove_element_at/2, remove_last_element/1,
		  heads/2,
		  get_last_element/1, extract_last_element/1,
		  get_index_of/2, get_maybe_index_of/2, split_at/2, group_by/2,
		  uniquify/1, uniquify_ordered/1,
		  ensure_is_once_in/2,
		  duplicate/2, has_duplicates/1, count_occurrences/1, get_duplicates/1,
		  union/2, intersection/2,
		  difference/2, differences/2,
		  cartesian_product/1,
		  subtract_all_duplicates/2,
		  get_all_permutations/1,
		  delete_existing/2, delete_existing_elements/2,
		  delete_if_existing/2,
		  remove_element_from/2, remove_elements_from/2,
		  remove_first_occurrence/2, remove_first_occurrences/2,
		  delete_all_in/2,
		  intercalate/2, append_at_end/2,
		  unordered_compare/2, flatten_once/1, filter_out_undefined/1 ]).


% Less common list operations:
-export([ dispatch_in/2, add_as_heads/2, insert_at_all_places/2 ]).


% For list of tuples (ex: typically used by the HDF5 binding), extended flatten
% and al:
%
-export([ determine_tuple_info/1, flatten_tuples/1, reconstruct_tuples/2 ]).



% Random operations on lists:
-export([ random_permute/1, random_permute_reciprocal/1,
		  draw_element/1, draw_element/2, draw_element_weighted/1,
		  draw_elements_from/2, extract_elements_from/2 ]).


-type element() :: term().
% An element of a list.


-type maybe_list( T ) :: [ T ] | T.
% Either a list of terms, or a term by itself.
%
% Note: different from maybe(list()).


-export_type([ maybe_list/1 ]).



% Shorthands:

-type proplist() :: proplists:proplist().

-type count() :: basic_utils:count().

-type positive_index() :: basic_utils:positive_index().
% These indexes start at 1.



% Section for the checking of lists.


% @doc Ensures that the specified argument is a list: encloses it in a list of
% its own if not already a list; respects any original order.
%
% Note: not to be applied on strings for example.
%
-spec ensure_list( maybe_list( T ) ) -> [ T ].
ensure_list( List ) when is_list( List ) ->
	List;

ensure_list( Term ) ->
	[ Term ].



% @doc Ensures that the specified argument is a list of atoms: encloses any atom
% in a list of its own if not already a list, or check that this list is only
% populated of atoms; respects any original order.
%
ensure_atoms( Atom ) when is_atom( Atom ) ->
	[ Atom ];

ensure_atoms( List ) when is_list( List ) ->
	case type_utils:is_homogeneous( List, _CommonType=atom ) of

		true ->
			List;

		false ->
			throw( { not_list_of_atoms, List } )

	end;

ensure_atoms( Other ) ->
	throw( { neither_list_nor_atom, Other } ).



% @doc Ensures that the specified argument is a list of tuples: encloses any
% tuple in a list of its own if not already a list, or check that this list is
% only populated of tuples; respects any original order.
%
ensure_tuples( Tuple ) when is_tuple( Tuple ) ->
	[ Tuple ];

ensure_tuples( List ) when is_list( List ) ->
	case type_utils:is_homogeneous( List, _CommonType=tuple ) of

		true ->
			List;

		false ->
			throw( { not_list_of_tuples, List } )

	end;

ensure_tuples( Other ) ->
	throw( { neither_list_nor_tuple, Other } ).



% @doc Ensures that the specified argument is a list of PIDs: encloses any PID
% in a list of its own if not already a list, or check that this list is only
% populated of PIDs; respects any original order.
%
ensure_pids( Pid ) when is_pid( Pid ) ->
	[ Pid ];

ensure_pids( List ) when is_list( List ) ->
	case type_utils:is_homogeneous( List, _CommonType=pid ) of

		true ->
			List;

		false ->
			throw( { not_list_of_pids, List } )

	end;

ensure_pids( Other ) ->
	throw( { neither_list_nor_pid, Other } ).



% @doc Ensures that the specified argument is a proplist, that is a list of
% atoms or of pairs whose first argument is an atom: encloses any of such
% element in a list of its own if not already a list, or checks that this list
% is only populated as expected; respects any original order.
%
-spec ensure_proplist( maybe_list( atom() | { atom(), any() } ) ) -> proplist().
ensure_proplist( Atom ) when is_atom( Atom ) ->
	[ Atom ];

ensure_proplist( P={ Atom, _Any } ) when is_atom( Atom ) ->
	[ P ];

ensure_proplist( L ) when is_list( L ) ->
	ensure_proplist_helper( L, _Acc=[] );

ensure_proplist( Other ) ->
	throw( { not_proplistable, Other } ).


% Not using here ensure_proplist/1, as nested lists are not permitted:
ensure_proplist_helper( _L=[], Acc ) ->
	lists:reverse( Acc );

ensure_proplist_helper( _L=[ Atom | T ], Acc ) when is_atom( Atom ) ->
	ensure_proplist_helper( T, [ Atom | Acc ] );

ensure_proplist_helper( _L=[ P={ Atom, _Any } | T ], Acc )
										when is_atom( Atom ) ->
	ensure_proplist_helper( T, [ P | Acc ] );

ensure_proplist_helper( [ Other | _T ], _Acc ) ->
	throw( { invalid_proplist_element, Other } ).



% Section for basic list operations.


% As usual in Myriad, indices start at position #1, not #0.


% @doc Returns the element in the list at the specified index, in
% `[1..length(List)]'.
%
% If the index is out of bounds, a function_clause is raised.
%
% Note: usually these kinds of functions should not be used, recursive
% algorithms are a lot more effective, when applicable.
%
% Not tail recursive version:
%
% get_element_at( List, 1 ) ->
%   hd(List);
%
% get_element_at( [ _H | T ], Index ) ->
%   get_element_at( T, Index-1 ).
%
-spec get_element_at( list(), positive_index() ) -> element().
get_element_at( List, Index ) ->
	%trace_utils:debug_fmt( " - getting element #~B of ~w", [ Index, List ] ),
	lists:nth( Index, List ).



% @doc Returns the specified list with the element at specified index replaced
% by the specified element.
%
-spec set_element_at( element(), list(), positive_index() ) -> list().
set_element_at( Element, List, Index ) ->
	set_element_at( Element, List, Index, _Acc=[] ).


% (helper)
set_element_at( Element, _List=[ _H | T ], _Index=1, Acc ) ->
	lists:reverse( Acc ) ++ [ Element | T ];

set_element_at( Element, _List=[ H | T ], Index, Acc ) ->
	set_element_at( Element, T, Index-1, [ H | Acc ] ).



% @doc Inserts specified element at specified position in the specified list.
%
% For example: insert_element_at(foo, [a, b, c, d], 3) will return
% [a, b, foo, c, d].
%
-spec insert_element_at( element(), list(), positive_index() ) -> list().
insert_element_at( Element, List, Index ) ->

	%io:format( " - inserting element ~p at #~B in ~w~n",
	%           [ Element, Index, List ] ),

	insert_element_at( Element, List, Index, _Acc=[] ).


insert_element_at( Element, _List=[], _Index=1, Acc ) ->
	lists:reverse( [ Element | Acc ] );

insert_element_at( _Element, _List=[], Index, Acc ) ->
	% Rebuilds input parameters:
	throw( { invalid_index, Index + length( Acc ), lists:reverse( Acc ) } );

insert_element_at( Element, List, _Index=1, Acc ) ->
	lists:reverse( [ Element | Acc ] ) ++ List;

insert_element_at( Element, _List=[ H | T ], Index, Acc ) ->
	insert_element_at( Element, T, Index-1, [ H | Acc ] ).



% @doc Extracts element from the specified list, at the specified index.
%
% Returns that element and the resulting, shrunk list.
%
% For example: {b, [a, c]} = extract_element_at([ a, b, c], 2).
%
-spec extract_element_at( list(), positive_index() ) -> { element(), list() }.
extract_element_at( List, Index ) ->
	% Relying now on lists:split/2:
	{ Left, [ Elem | Right ] } = lists:split( Index-1, List ),
	{ Elem, Left ++ Right }.

	% Before: nothing relevant found in the lists module, so:
	%extract_element_at( List, Index, _Acc=[] ).


% (helper)
%extract_element_at( _List=[ H | T ], _Index=1, Acc ) ->
%   { H, lists:reverse( Acc ) ++ T };

%extract_element_at( _List=[], Index, Acc ) ->
%   throw( { index_out_of_range, Index + length( Acc ),
%            lists:reverse( Acc ) } );

%extract_element_at( _List=[ H | T ], Index, Acc ) ->
%   extract_element_at( T, Index-1, [ H | Acc ] ).



% @doc Extracts the (first occurrence of the) specified element from the
% specified list, if existing there, then returning the shrunk list (with that
% element removed, and in its original order), otherwise returning false.
%
-spec extract_element_if_existing( element(), list() ) -> 'false' | list().
extract_element_if_existing( Elem, List ) ->
	extract_element_if_existing( Elem, List, _Acc=[] ).


% (helper)
extract_element_if_existing( _Elem, _List=[], _Acc ) ->
	false;

extract_element_if_existing( Elem, _List=[ Elem | T ], Acc ) ->
	lists:reverse( Acc ) ++ T;

extract_element_if_existing( Elem, _List=[ H | T ], Acc ) ->
	extract_element_if_existing( Elem, T, [ H | Acc ] ).



% @doc Removes the specified number first elements.
%
% For example: [c, d, e] = list_utils:remove_first_elements([a, b, c, d, e], 2).
%
-spec remove_first_elements( list(), count() ) -> list().
remove_first_elements( List, _Count=0 ) ->
	List;

remove_first_elements( List, Count ) ->
	remove_first_elements( tl( List ), Count-1 ).



% @doc Returns a list corresponding to the specified one with the element at
% specified index removed.
%
% If the index is out of bounds, a function_clause like
% '[{list_utils, remove_element_at, ...}]' is triggered.
%
% Note: usually these kinds of functions should not be used, recursive
% algorithms are a lot more effective, when applicable.
%
% Curiously lists:nth exists, but no function to remove an element specified by
% its index seems to be available in the lists module.
%
% Not tail recursive version:
%remove_element_at( [ _H | T ], _LastIndex=1 ) ->
%   T;
%
%remove_element_at( [ H | T ], _Index=N ) ->
%   [ H | remove_element_at( T, _NextIndex=N-1 ) ].
%
% Tail recursive version.
%
-spec remove_element_at( list(), positive_index() ) -> list().
remove_element_at( List, Index ) ->
	remove_element_at( List, Index, _Result=[] ).

remove_element_at( [ _H | RemainingList ], 1, Result ) ->
	lists:reverse( Result ) ++ RemainingList;

remove_element_at( [ H | RemainingList ], Index, Result ) ->
	remove_element_at( RemainingList, Index-1, [ H | Result ] ).



% @doc Removes the last element of the specified list.
%
% Crashes (with 'no function clause') if the input list is empty.
%
% Note: not computationnally efficient, usually removing the last element
% suggests a bad code design.
%
-spec remove_last_element( list() ) -> list().
remove_last_element( List ) ->
	lists:droplast( List ).

% remove_last_element( List, _Acc=[] ).


%remove_last_element( _List=[ _Last ], Acc ) ->
%   lists:reverse( Acc );

%remove_last_element( _List=[ H | T ], Acc ) ->
%   remove_last_element( T, [ H | Acc ] ).



% @doc Returns a pair made of the N first elements ("heads") of the specified
% list, and of its remainder (tail).
%
% For example: heads([a,b,c,d,e], _N=3) = {[a,b,c],[d,e]}.
%
% Like list:sublist/1 yet returning the tail (list:nthtail/1) as well.
%
-spec heads( list(), count() ) -> { list(), list() }.
heads( List, N ) ->
	heads( List, N, _Acc=[] ).


% (helper)
heads( List, _N=0, Acc ) ->
	{ lists:reverse( Acc ), List };

heads( _List=[ H | T ], N, Acc ) ->
	heads( T, N-1, [ H | Acc ] ).




% @doc Returns the last element of the specified list.
%
% Note: not computationnally efficient, usually having to retrieve the last
% element suggests a bad code design.
%
% Crashes (with 'no function clause') if the input list is empty.
%
-spec get_last_element( list() ) -> element().
get_last_element( _List=[ SingleElement ] ) ->
	SingleElement;

get_last_element( _List=[ _H | T ] ) ->
	get_last_element( T ).



% @doc Extracts the last element of the specified (non-empty) list, returning a
% pair made of that element and of the remainder of the list (in its original
% order).
%
% Note: not computationnally efficient, usually having to retrieve the last
% element suggests a bad code design.
%
-spec extract_last_element( list() ) -> { element(), list() }.
extract_last_element( _List=[] ) ->
	throw( cannot_extract_from_empty_list );


extract_last_element( List ) ->

	% Probably the most efficient variant:
	[ LastElement | RevRest ] = lists:reverse( List ),

	{ LastElement, lists:reverse( RevRest ) }.


% Variant:
%extract_last_element( List ) ->
%  extract_last_element( List, _Acc=[] ).


% (helper)
%extract_last_element( _List=[ LastElement ], Acc ) ->
%  { LastElement, lists:reverse( Acc ) };
%
%extract_last_element( _List=[ H | T ], Acc ) ->
%  extract_last_element( T, [ H | Acc ] ).



% @doc Returns the index, in `[1..length(List)]', of the (first occurrence of
% the) specified element in the specified list.
%
% Throws an exception if the element is not found.
%
% For example: 3 = get_index_of(bar, [foo, ugh, bar, baz])
%
-spec get_index_of( element(), list() ) -> count().
get_index_of( Element, List ) ->
	case get_maybe_index_of( Element, List ) of

		undefined ->
			throw( { non_existing_element, Element } );

		I ->
			I

	end.



% @doc Returns the index, in `[1..length(List)]', of the (first occurrence of
% the) specified element in the specified list, or 'undefined' if the element is
% not found.
%
% For example:
%   3 = get_maybe_index_of(bar, [foo, ugh, bar, baz])
%   undefined = get_maybe_index_of(xxx, [foo, ugh, bar, baz])
%
-spec get_maybe_index_of( element(), list() ) -> maybe( count() ).
get_maybe_index_of( Element, List ) ->
	get_maybe_index_of( Element, List, _Count=1 ).


get_maybe_index_of( _Element, _List=[], _Count ) ->
	undefined;

get_maybe_index_of( Element, _List=[ Element | _T ], Count  ) ->
	Count;

get_maybe_index_of( Element, _List=[ _H | T ], Count ) ->
	get_maybe_index_of( Element, T, Count+1 ).



% @doc Splits the specified (plain) list in two parts (two plain lists, that are
% returned):
%
% - the first contains the first elements, up to MaxLen included, and in
%   reverse order
% - the second contains the remaining elements (if any)
%
% For example: split_at(3, [a, b, c, d, e]) = {[c, b, a], [d, e]}
%
-spec split_at( count(), list() ) -> { list(), list() }.
split_at( MaxLen, List ) ->
	%trace_utils:debug_fmt( "Splitting ~p at position #~B.", [ List, MaxLen ] ),
	split_at( List, _Count=0, MaxLen, _Acc=[] ).


% (helper)
split_at( InputList, _Count=MaxLen, MaxLen, AccList ) ->
	% Max len reached, stopping here:
	{ AccList, InputList };

split_at( InputList=[], _Count, _MaxLen, AccList ) ->
	% Input list exhausted:
	{ AccList, InputList };

split_at( _List=[ E | T ], Count, MaxLen, Acc ) ->
	split_at( T, Count+1, MaxLen, [ E | Acc ] ).



% @doc Splits the specified list by groups of Count elements.
%
% The last group may have less than Count elements.
%
% For example: [[a,b], [c,d], [e]] = group_by(_Count=2, [a,b,c,d,e])
%
-spec group_by( count(), list() ) -> [ list() ].
group_by( Count, List ) ->
	group_by( Count, List, _Acc=[] ).


% (helper)
group_by( _Count, _List=[], Acc ) ->
	lists:reverse( Acc );

group_by( Count, List, Acc ) ->
	{ RevFirst, RemainingList } = split_at( Count, List ),
	NewAcc = [ lists:reverse( RevFirst ) | Acc ],
	group_by( Count, RemainingList, NewAcc ).



% @doc Returns a list whose elements are the ones of the specified list, except
% that they are unique (all their next duplicates have been removed).
%
% No specific order is respected in the returned list.
%
% For example: if L = [1,2,3,2,2,4,5,5,4,6,6,5], then uniquify(L) is:
% [1,2,3,4,5,6].
%
-spec uniquify( list() ) -> list().
uniquify( List ) ->
	% There is probably a more efficient way of doing the same:
	% (previously order was not respected in the returned list)
	%sets:to_list( sets:from_list( List ) ).

	% Now the following is readily available, and preserves order:
	lists:uniq( List ).



% @doc Returns a list whose elements are the ones of the specified list, except
% that they are unique (all their duplicates have been removed), while the order
% in the kept elements is preserved.
%
% Expected to be a bit slower than uniquify/1.
%
% For example: if L = [1,3,2,3,2,2,4,5,5,4,6,6,5], then uniquify_ordered(L) is:
% [1,3,2,4,5,6].
%
-spec uniquify_ordered( list() ) -> list().
uniquify_ordered( List ) ->
	uniquify_ordered( List, _Acc=[], _KnownSet=set_utils:new() ).


% (helper)
uniquify_ordered( [], Acc, _KnownSet ) ->
	lists:reverse( Acc );

uniquify_ordered( [ H | T ], Acc, KnownSet ) ->
	case set_utils:member( H, KnownSet ) of

		true ->
			uniquify_ordered( T, Acc, KnownSet );

		false ->
			uniquify_ordered( T, [ H | Acc ], set_utils:add( H, KnownSet ) )

	end.



% @doc Ensures that the specified element is included once in the specified
% unordered list (supposed to contain it already up to once).
%
% Note: refer to set_utils for a more proper implementation of sets.
%
-spec ensure_is_once_in( term(), list() ) -> list().
ensure_is_once_in( Elem, List ) ->

	case lists:member( Elem, List ) of

		true ->
			List;

		false ->
			[ Elem | List ]

	end.



% @doc Returns a list made of the specified number of occurrences of the
% specified element.
%
-spec duplicate( element(), count() ) -> [ element() ].
duplicate( Elem, Count ) ->
	%[ Elem || _ <- lists:seq( 1, Count ) ].
	lists:duplicate( Count, Elem ).


% @doc Tells whether there are in the specified list elements that are present
% more than once.
%
has_duplicates( List ) ->
	length( uniquify( List ) ) =/= length( List ).



% @doc Counts the number of occurences of all elements in the specified list:
% returns an (unordered) list of {Term,Count} pairs, where each term is
% associated to the total number of its occurrences (1 or above) in the
% specified list.
%
-spec count_occurrences( list() ) -> [ { element(), count() } ].
count_occurrences( List ) ->
	count_occurrences( List, _Acc=[] ).

count_occurrences( _List=[], Acc ) ->
	Acc;

count_occurrences( _List=[ Term | T ], Acc ) ->

	% trace_utils:debug_fmt( "Inquiring about term '~p' into ~p.",
	%                        [ Term, T ] ),

	case count_and_filter_term( Term, _InitialList=T, _FilteredList=[],
								_InitialCount=0 ) of

		not_found ->
			% No a duplicated element, just iterating on the next term:
			count_occurrences( T, [ { Term, 1 } | Acc ] );

		{ TermCount, FilteredList } ->
			% We already extracted the first Term:
			count_occurrences( FilteredList, [ { Term, TermCount+1 } | Acc ] )

   end.



% @doc Returns the duplicates in the specified list: returns an (unordered) list
% of {DuplicatedTerm,DuplicationCount} pairs, where each duplicated term (that
% is a term present more than once) is specified, alongside the total number of
% occurrences of that term in the specified list.
%
% Note: as a consequence, a term that is not in the specified list, or that is
% present there only once, will not be referenced in the returned list; use
% count_occurrences/1 if wanting to include the terms that are listed only once
% each.
%
% For example: L = [a,a,b,b,b,c,d,d],
%              [{b,3},{d,2},{a,2}] = list_utils:get_duplicates(L)
%
% Use lists:keysort(2, list_utils:get_duplicates(L)) to sort duplicates by
% increasing number of occurrences (e.g. [{d,2},{a,2},{b,3}] here).
%
-spec get_duplicates( list() ) -> [ { element(), count() } ].
get_duplicates( List ) ->
	get_duplicates( List, _Acc=[] ).

get_duplicates( _List=[], Acc ) ->
	Acc;

get_duplicates( _List=[ Term | T ], Acc ) ->

	% trace_utils:debug_fmt( "Inquiring about term '~p' into ~p.",
	%                        [ Term, T ] ),

	case count_and_filter_term( Term, _InitialList=T, _FilteredList=[],
								_InitialCount=0 ) of

		not_found ->
			% No a duplicated element, just iterating on the next term:
			get_duplicates( T, Acc );

		{ TermCount, FilteredList } ->
			% We already extracted the first Term:
			get_duplicates( FilteredList, [ { Term, TermCount+1 } | Acc ] )

   end.


% (helper)
count_and_filter_term( _Term, _List=[], _FilteredList, _CurrentCount=0 ) ->
	not_found;

count_and_filter_term( _Term, _List=[], FilteredList, CurrentCount ) ->
	{ CurrentCount, FilteredList };

% Term found:
count_and_filter_term( Term, _List=[ Term | H ], FilteredList, CurrentCount ) ->
	count_and_filter_term( Term, H, FilteredList, CurrentCount+1 );

% Other term:
count_and_filter_term( Term, _List=[ OtherTerm | H ], FilteredList,
					   CurrentCount ) ->
	count_and_filter_term( Term, H, [ OtherTerm | FilteredList ],
						   CurrentCount ).



% @doc Returns the union of the two specified lists, that is the list of all
% elements that are in either list.
%
-spec union( list(), list() ) -> list().
union( L1, L2 ) ->
	%uniquify( L1 ++ L2 ).
	set_utils:to_list( set_utils:union( set_utils:from_list( L1 ),
										set_utils:from_list( L2 ) ) ).



% @doc Returns the intersection of the two specified lists, that is the list of
% all elements that are in both lists.
%
% See also: subtract_all_duplicates/2.
%
-spec intersection( list(), list() ) -> list().
intersection( L1, L2 ) ->
	%set_utils:to_list( set_utils:intersection( set_utils:from_list( L1 ),
	%											set_utils:from_list( L2 ) ) ).
	lists:filter( fun( E ) -> lists:member( E, L2 ) end, L1 ).



% @doc Returns the difference between the first specified list and the second,
% that is the elements of the first list that are not in the second one.
%
-spec difference( list(), list() ) -> list().
difference( L1, L2 ) ->
	set_utils:to_list( set_utils:difference( set_utils:from_list( L1 ),
											 set_utils:from_list( L2 ) ) ).



% @doc Returns the differences between the first specified list and the second,
% as a pair, whose first element corresponds to the elements of the first list
% that are not in the second one, and whose second element corresponds to the
% elements of the second list that are not in the first one.
%
-spec differences( list(), list() ) -> { list(), list() }.
differences( L1, L2 ) ->
	{ S1, S2 } = set_utils:differences( set_utils:from_list( L1 ),
										set_utils:from_list( L2 ) ),
	{ set_utils:to_list( S1 ), set_utils:to_list( S2 ) }.



% @doc Returns the cartesian product of the specified lists (collected in a
% top-level list).
%
% For example: cartesian_product([[a,b,c], [d,e], [f]]) =
%     [[a,d,f], [a,e,f], [b,d,f], [b,e,f], [c,d,f], [c,e,f]]
%
-spec cartesian_product( [ [ T ] ] ) -> [ [ T ] ].
cartesian_product( [ SingleList ] ) ->
	[ [ E ] || E <- SingleList ];

cartesian_product( [ List | OtherLists ] ) ->
	[ [ E | SubList ]
		|| E <- List, SubList <- cartesian_product( OtherLists ) ].



% @doc Returns a list equal to L1 except that all elements found in L2 have been
% removed, even if in L1 they were duplicated.
%
% Note: like lists:subtract/2, except that *all* occurences from L2 in L1 (not
% only the first one) are removed. See also: the '--' operator.
%
% Example: [1,4] = list_utils:subtract_all_duplicates( [1,2,3,4,2], [2,3] )
%
% Taken from
% http://www.trapexit.org/Finding_Elements_in_One_Array_but_Not_Another
%
-spec subtract_all_duplicates( list(), list() ) -> list().
subtract_all_duplicates( L1, L2 ) ->
	lists:filter( fun( E ) -> not lists:member( E, L2 ) end, L1 ).



% @doc Returns a list of all the permutations of the specified list.
%
% For example: get_all_permutations([a,b,c]) =
%    [ [c,b,a], [c,a,b], [a,c,b], [b,c,a], [b,a,c], [a,b,c] ]
%
-spec get_all_permutations( list() ) -> [ list() ].
get_all_permutations( L=[ _E ] ) ->
	[ L ];

get_all_permutations( _L=[ H | T ] ) ->
	% Recurse from length N to N-1:
	TPerms = get_all_permutations( T ),
	flatten_once( [ insert_at_all_places( H, APermL ) || APermL <- TPerms ] ).



% @doc Returns all the lists obtained from the specified one L, when inserting
% the specified element at each possible place in L (including before and
% after).
%
% For example: insert_at_all_places(a, [b,c,d]) =
%                  [ [b,c,d,a], [b,c,a,d], [b,a,c,d], [a,b,c,d] ]
%
-spec insert_at_all_places( element(), list() ) -> [ list() ].
insert_at_all_places( E, L ) ->
	insert_at_all_places( E, L, _RevL=[], _Acc=[] ).


insert_at_all_places( E, _L=[], RevL, Acc ) ->
	% Last possibility is when ending with E:
	[ lists:reverse( [ E | RevL ] ) | Acc ];

insert_at_all_places( E, _L=[ H | T ], RevL, Acc ) ->
	NewList = lists:reverse( [ H, E | RevL ] ) ++ T,
	NewAcc = [ NewList | Acc ],
	insert_at_all_places( E, T, [ H | RevL ], NewAcc ).



% @doc Returns a copy of the specified list where the first element matching
% Elem is deleted, ensuring that at least one of these elements exists (as
% opposed to lists:delete/2). The order of the specified list is preserved.
%
-spec delete_existing( element(), list() ) -> list().
delete_existing( Elem, List ) ->

	% We keep a copy of the original list to be able to generate better error
	% messages:
	%
	delete_existing( Elem, List, _OriginalList=List, _Acc=[] ).


% (helper)
delete_existing( Elem, _List=[], OriginalList, _Acc ) ->
	throw( { element_to_delete_not_found, Elem, OriginalList } );

delete_existing( Elem, _List=[ Elem | T ], _OriginalList, Acc ) ->
	% The first element found stops the iteration:
	lists:reverse( Acc ) ++ T;

delete_existing( Elem, _List=[ H | T ], OriginalList, Acc ) ->
	delete_existing( Elem, T, OriginalList, [ H | Acc ] ).



% @doc Returns a copy of the specified list where the first element matching
% each of the specified elements is deleted, ensuring that at least one of these
% elements exists (as opposed to lists:delete/2). The order of the specified
% list is preserved.
%
-spec delete_existing_elements( [ element() ], list() ) -> list().
delete_existing_elements( _Elems=[ E | T ], List ) ->
	NewList = delete_existing( E, List ),
	delete_existing_elements( T, NewList );

delete_existing_elements( _Elems=[], List ) ->
	List.



% @doc Deletes up to one occurrence (the first found) of the specified element
% in the specified list, whose order is preserved.
%
-spec remove_first_occurrence( element(), list() ) -> list().
remove_first_occurrence( Element, List ) ->
	lists:delete( Element, List ).


% @doc Deletes up to one occurrence (the first found) of each of the specified
% elements, from the specified list, whose order is preserved.
%
-spec remove_first_occurrences( [ element() ], list() ) -> list().
remove_first_occurrences( _ElementsToRemove=[], List ) ->
	List;

remove_first_occurrences( _ElementsToRemove=[ E | T ], List ) ->
	NewList = lists:delete( E, List ),
	remove_first_occurrences( T, NewList ).



% @doc Deletes the first matching of specified element from specified list,
% returning whether an element has been removed: either the 'not_found' atom (in
% which case the list remained the same) or the corresponding new list (same
% order and content, except first occurrence removed).
%
% Note: allows to perform only one traversal of the list (compared for example
% to a lists:member/2 then a lists:delete/2).
%
-spec delete_if_existing( element(), list() ) -> 'not_found' | list().
delete_if_existing( Elem, List ) ->
	delete_if_existing( Elem, List, _Acc=[] ).


delete_if_existing( _Elem, _List=[], _Acc ) ->
	not_found;

delete_if_existing( Elem, _List=[ Elem | T ], Acc ) ->
	lists:reverse( Acc ) ++ T;

delete_if_existing( Elem, _List=[ H | T ], Acc ) ->
	delete_if_existing( Elem, T, [ H | Acc ] ).



% @doc Removes all occurrences (not the first one only: possibly zero, possibly
% more than one) of the specified element from the specified list, and returns
% the result (in the original order).
%
-spec remove_element_from( element(), list() ) -> list().
remove_element_from( Elem, List ) ->
	remove_element_from( Elem, List, _Acc=[] ).


% (helper)
remove_element_from( _Elem, _List=[], Acc ) ->
	lists:reverse( Acc );

remove_element_from( Elem, _List=[ Elem | T ], Acc ) ->
	% Dropped:
	remove_element_from( Elem, T, Acc );

remove_element_from( Elem, _List=[ H | T ], Acc ) ->
	remove_element_from( Elem, T, [ H | Acc ] ).



% @doc Removes all occurrences (not the first one only: possibly zero, possibly
% more than one) of the specified elements from the specified list, and returns
% the result (in the original order).
%
-spec remove_elements_from( [ element() ], list() ) -> list().
remove_elements_from( Elems, List ) ->
	remove_elements_from( Elems, List, _Acc=[] ).


% (helper)
remove_elements_from( _Elems, _List=[], Acc ) ->
	lists:reverse( Acc );

remove_elements_from( Elems, _List=[ E | T ], Acc ) ->
	case lists:member( E, Elems ) of

		true ->
			% Dropped:
			remove_elements_from( Elems, T, Acc );

		_False ->
			remove_elements_from( Elems, T, [ E | Acc ] )

	end.



% @doc Returns a copy of the specified list where all elements matching Elem are
% deleted, whether or not there is any.
%
% The element order of the specified list is preserved.
%
% Note: kept only for backward compatibility; prefer remove_element_from/2.
%
-spec delete_all_in( element(), list() ) -> list().
delete_all_in( Elem, List ) ->
	remove_element_from( Elem, List ).



% @doc Intercalates the specified term between all elements of the specified
% list.
%
% For example:
%  [] = list_utils:intercalate('a', []),
%  [1] = list_utils:intercalate('a', [1]),
%  [1,a,2] = list_utils:intercalate('a', [1,2]),
%  [1,a,2,a,3] = list_utils:intercalate('a', [1,2,3]).
%
-spec intercalate( element(), list() ) -> list().
intercalate( _Elem, _TargetList=[] ) ->
	[];

intercalate( Elem, TargetList ) ->
	intercalate( Elem, TargetList, _Acc=[] ).


% (helper)
intercalate( _Elem, _TargetList=[ H | [] ], Acc ) ->
	lists:reverse( [ H | Acc ] );

intercalate( Elem, _TargetList=[ H | T ], Acc ) ->
	intercalate( Elem, T, [ Elem, H | Acc ] ).



% @doc Appends specified element at the end of specified list, without changing
% the order of the list.
%
% For example: append_at_end(d, [a,b,c]) returns [a,b,c,d].
%
% Note: usually adding elements at the end of a list should be avoided, as it is
% costlier than adding them at head.
%
-spec append_at_end( element(), list() ) -> nonempty_list().
% This clause was not a bright idea:
% - beware of not using a string (hence a list) as first parameter, if not
% expecting it to be managed as a list
%
%append_at_end( ElemList, L ) when is_list( ElemList ) andalso is_list( L ) ->
%   L ++ ElemList;
%
append_at_end( Elem, L ) when is_list( L ) ->
	% Should be more efficient than:
	%lists:reverse( [ Elem | lists:reverse( L ) ] ):
	L ++ [ Elem ].



% @doc Returns whether the specified list contains only integers.
%
% Considers that an empty list complies.
%
-spec are_integers( term() ) -> boolean().
are_integers( [] ) ->
	true;

are_integers( [ H | T ] ) when is_integer( H ) ->
	are_integers( T );

are_integers( _ ) ->
	false.


% @doc Checks that specified argument is a list of integers.
%
% See also type_utils:check_integers/1.
%
-spec check_integers( term() ) -> void().
check_integers( Any ) ->
	true = are_integers( Any ).


% @doc Returns whether the specified list contains only PIDs.
%
% Considers that an empty list complies.
%
-spec are_pids( term() ) -> boolean().
are_pids( [] ) ->
	true;

are_pids( [ H | T ] ) when is_pid( H ) ->
	are_pids( T );

are_pids( _ ) ->
	false.



% @doc Returns whether the specified list contains only atoms.
%
% Considers that an empty list complies.
%
-spec are_atoms( term() ) -> boolean().
are_atoms( [] ) ->
	true;

are_atoms( [ H | T ] ) when is_atom( H ) ->
	are_atoms( T );

are_atoms( _ ) ->
	false.



% @doc Checks that the terms in the specified list are in strict (no duplicates)
% ascending (Erlang) term order.
%
% In many cases, the actual type of these elements shall be checked beforehand
% (ex: see type_utils:check_{integers,floats}/1) to ensure that comparisons make
% sense (ex: float versus atom).
%
-spec check_strictly_ascending( list() ) -> boolean().
check_strictly_ascending( _List=[] ) ->
	true;

check_strictly_ascending( _List=[ H | T ] ) ->
	check_strictly_ascending( T, _LastSeen=H ).


% (helper)
check_strictly_ascending( _List=[], _LastSeen ) ->
	true;

check_strictly_ascending( _List=[ H | T ], LastSeen ) when H > LastSeen ->
	check_strictly_ascending( T, H );

check_strictly_ascending( _List, _LastSeen ) ->
	false.



% @doc Compares the two specified lists with no regard to the order of their
% elements: returns true iff they have the exact same elements (differentiating
% between 1 and 1.0 for example), possibly in a different order.
%
-spec unordered_compare( list(), list() ) -> boolean().
unordered_compare( L1, L2 ) ->
	lists:sort( L1 ) =:= lists:sort( L2 ).



% @doc Flattens specified list of lists only once (that is on a single level),
% as opposed to indefinitively (as done recursively by lists:flatten/1);
% provides more control than a recursive counterpart.
%
% Element order is preserved.
%
% For example: if L=[[1], [2,[3,4]]], lists:flatten(L) yields [1,2,3,4] whereas
% list_utils:flatten_once(L) should yield [1,2,[3,4]].
%
% See text_utils:concatenate/1 for string-related operations.
%
-spec flatten_once( [ list() ] ) -> list().
flatten_once( List ) ->
	flatten_once( List, _Acc=[] ).


% (helper)
%
% Note: not using simply 'lists:reverse( Acc );' and a (more efficient) 'L ++
% Acc', as we would end up with [1,[3,4],2] - whereas we want to preserve order.
%
flatten_once( [], Acc ) ->
	Acc;

flatten_once( [ L | T ], Acc ) when is_list( L ) ->
	flatten_once( T, Acc ++ L );

flatten_once( [ Unexpected | _T ], _Acc ) ->
	throw( { not_a_list, Unexpected } ).



% @doc Removes (filters out) all elements equal to 'undefined'; preserves order.
-spec filter_out_undefined( list() ) -> list().
filter_out_undefined( L ) ->
	% Or: delete_all_in( undefined, L ).
	[ E || E <- L, E =/= undefined ].



% @doc Dispatches the specified list on the specified number of sublists, each
% element being in turn placed in the next sublist, until going back at the
% first (like a ring).
%
% Note that the number of elements of the input list must be a multiple of the
% specified number of sublists (hence all sublists will have the same length).
%
% The order in each sublist is preserved.
%
% For example: dispatch_in(3, [a, b, c, d, e, f]) = [[a,d], [b,e], [c,f]].
%
-spec dispatch_in( count(), list() ) -> [ list() ].
dispatch_in( SublistCount, List ) ->
	AccSubLists = lists:duplicate( SublistCount, _InitSublist=[] ),
	Res = dispatch_in( SublistCount, lists:reverse( List ), AccSubLists ),
	lists:reverse( Res ).


% (helper)
dispatch_in( _SublistCount, _List=[], AccSubLists ) ->
	AccSubLists;

dispatch_in( SublistCount, List, AccSubLists ) ->
	{ Heads, TailList } = heads( List, SublistCount ),
	NewAccSubLists = add_as_heads( Heads, AccSubLists ),
	dispatch_in( SublistCount, TailList, NewAccSubLists ).



% @doc Adds the specified elements as heads of the specified lists.
%
% For example: add_as_heads([a,b,c], [[u,v], [], [w]]) = [[a,u,v], [b], [c,w]].
%
% Of course the two lists shall have the same length.
%
add_as_heads( Heads, Lists ) ->
	add_as_heads( Heads, Lists, _Acc=[] ).


% (helper)
add_as_heads( _Heads=[], _Lists=[], Acc ) ->
	% Restores the order of augmented lists:
	lists:reverse( Acc );

add_as_heads( _Heads=[ H | TH ], _Lists=[ L | TL ], Acc ) ->
	NewL = [ H | L ],
	add_as_heads( TH, TL, [ NewL | Acc ] ).



% @doc Determines tuple-related information about specified datastructure:
% returns {TupleCount, TupleSize}, supposing the list is made of tuples of
% uniform sizes.
%
-spec determine_tuple_info( [ tuple() ] ) -> { count(), count() }.
determine_tuple_info( _TupleList=[] ) ->
	throw( empty_list );

determine_tuple_info( _TupleList=[ FirstTuple | T ] )
								when is_tuple( FirstTuple ) ->
	TupleSize = size( FirstTuple ),
	Count = check_tuple_length( T, TupleSize, _AccCount=1 ),
	{ Count, TupleSize }.


% (helper)
check_tuple_length( _TupleList=[], _TupleSize, AccCount ) ->
	AccCount;

check_tuple_length( _TupleList=[ Tuple | T ], TupleSize, AccCount ) ->

	case size( Tuple ) of

		TupleSize ->
			check_tuple_length( T, TupleSize, AccCount+1 );

		OtherSize ->
			throw( { heterogeneous_tuple_size, { Tuple, OtherSize },
						{ expected, TupleSize } } )

	end.



% @doc Flattens a list of tuples into a simple list of their elements, without
% tuples and in the same order.
%
% For example: flatten_tuples([{1, 2, 3}, {4, 5, 6}]) = [1, 2, 3, 4, 5, 6])
%
-spec flatten_tuples( [ tuple() ] ) -> list().
flatten_tuples( List ) ->
	flatten_tuples( List, _Acc=[] ).


flatten_tuples( _List=[], Acc ) ->
	lists:reverse( Acc );

flatten_tuples( [ H | T ], Acc ) ->
	NewAcc = lists:reverse( tuple_to_list( H ) ) ++ Acc,
	flatten_tuples( T, NewAcc ).



% @doc Reconstructs a list of tuples of specified size from the specified flat
% list.
%
% For example: reconstruct_tuples([1, 2, 3, 4, 5, 6], 3) =
%                            [{1, 2, 3}, {4, 5, 6}]
%
-spec reconstruct_tuples( list(), count() ) -> [ tuple() ].
reconstruct_tuples( List, _TupleSize=1 ) ->
	% Atomic elements do not need to be wrapped in a single-element tuple:
	List;

reconstruct_tuples( List, TupleSize ) ->
	reconstruct_tuples( List, TupleSize, _Acc=[] ).


reconstruct_tuples( _List=[], _TupleSize, Acc ) ->
	lists:reverse( Acc );

reconstruct_tuples( List, TupleSize, Acc ) ->
	{ TupleAsList, T } = lists:split( _Count=TupleSize, List ),
	reconstruct_tuples( T, TupleSize, [ list_to_tuple( TupleAsList ) | Acc ] ).



% Section to perform random operations on lists.


% @doc Returns a random uniform permutation of the specified list.
%
% Inspired from http://paste.lisp.org/display/74804.
%
% All these algorithms would need random access to a list, which is not readily
% possible here, hence must be emulated.
%
% See also the 'Speedy unsort:shuffle/1,2' thread in the erlang-questions
% mailing list for counterparts.
%
-spec random_permute( list() ) -> list().
random_permute( List ) ->
	random_permute( List, length( List ) ).


random_permute( _List, _RemainingLen=0 ) ->
	[];

random_permute( List, RemainingLen ) ->

	% Checking is commented-out:
	%RemainingLen = length( List ),

	% (using remove_element_at/2 should be quicker than using
	% proplists:delete/2, as we stop at the first matching element found)
	%
	Index = random_utils:get_uniform_value( RemainingLen ),

	%io:format( "Index=~p, ", [ Index ] ),

	% We put the drawn element at head, and recurse in the remaining list:
	[ get_element_at( List, Index )
		| random_permute( remove_element_at( List, Index ),
						  RemainingLen-1 ) ].



% @doc Returns a reciprocal random uniform permutation of the specified list,
% compared to random_permute/1.
%
% Consists on the reciprocal operation, so that, if starting from a random state
% S (see set_random_state/1) and if L2 = random_permute( L1 ), then, if starting
% again from S, L1 = random_permute_reciprocal( L2 ).
%
-spec random_permute_reciprocal( list() ) -> list().
random_permute_reciprocal( List ) ->

	% This is a little trickier than random_permute/1; we have to reverse
	% operations from latest to first, hence we must start with the latest drawn
	% value. So we draw them all first, and start by the end of that list,
	% taking into account that the range is decremented at each draw:
	%
	ReciprocalIndex = lists:reverse( [ random_utils:get_uniform_value( L )
			|| L <- lists:reverse( lists:seq( 1, length( List ) ) ) ] ),

	%io:format( "Reciprocal index = ~p~n", [ ReciprocalIndex ] ),

	random_permute_reciprocal( lists:reverse( List ), ReciprocalIndex,
							   _Acc=[] ).


random_permute_reciprocal( _List=[], _ReciprocalIndex=[], Acc ) ->
	Acc;

random_permute_reciprocal( _List=[ H | T ], _ReciprocalIndex=[ I | Is ],
						   Acc ) ->

	NewAcc = insert_element_at( _Elem=H, Acc, _Index=I ),

	random_permute_reciprocal( T, Is, NewAcc ).



% @doc Draws one element at random of the specified list, knowing that they all
% have the same probability of being drawn (uniform probability).
%
-spec draw_element( list() ) -> element().
draw_element( _ElementList=[] ) ->
	throw( cannot_draw_from_empty_list );

draw_element( ElementList ) ->
	Len = length( ElementList ),
	draw_element( ElementList, Len ).



% @doc Draws one element at random of the specified list, whose length must be
% the specified one (allows to precompute it once for multiple drawings),
% knowing all elements have the same probability of being drawn (uniform
% probability).
%
-spec draw_element( list(), count() ) -> element().
draw_element( ElementList, Length ) ->

	DrawnIndex = random_utils:get_uniform_value( Length ),

	get_element_at( ElementList, DrawnIndex ).

	% Alternate, less efficient, implementation:

	% Same probability:
	%UniformList = [ { Elem, 1 } || Elem <- ElementList ],
	%draw_element_weighted( UniformList ).



% @doc Draws one element at random of the specified list, which is a list of
% {Element, Probability} pairs: returns the drawn element, knowing that it will
% be chosen according to the probability associated to each element.
%
% Probabilities are managed as positive (possibly null) numbers (integer or
% floating-point values) defined relatively to each other (they do not have to
% sum up to 1.0).
%
% For example: ElementList = [{first,1}, {second,2}, {third,1}] is expected to
% return on average 'second' twice as frequently as 'first' or 'third'.
%
% Using [{first,1}, {second,0}, {third,1}] instead would mean that 'second'
% would never be drawn.
%
% See also random_utils:generate_random_state_from/1 and
% random_utils:get_sample[s]_from/1.
%
-spec draw_element_weighted( [ { element(), number() } ] ) -> element().
draw_element_weighted( ElementList ) ->
	draw_element_weighted( ElementList, sum_probabilities( ElementList ) ).


-spec sum_probabilities( [ { element(), number() } ] ) -> number().
sum_probabilities( ElementList ) ->
	sum_probabilities( ElementList, _Acc=0 ).


sum_probabilities( _ElementList=[], Acc ) ->
	Acc;

sum_probabilities( _ElementList=[ { _Element, Probability } | T ], Acc ) ->
	sum_probabilities( T, Acc + Probability ).



% Sum must be equal to the sum of all probabilities in ElementList.
draw_element_weighted( _ElementList, 0 ) ->
	throw( null_total_probability );

draw_element_weighted( ElementList, Sum ) ->
	DrawnValue = random_utils:get_uniform_value( Sum ),
	%io:format( "draw_element: drawn ~B.~n", [ DrawnValue ] ),
	select_element( ElementList, DrawnValue, _CurrentSum=0 ).


% (helper)
select_element( [ { Element, Probability } | _T ], DrawnValue, CurrentSum )
		when Probability + CurrentSum >= DrawnValue ->
	% Just gone past the probability range:
	Element;

select_element( [ { _Element, Probability } | T ], DrawnValue, CurrentSum ) ->
	% Drawn value still not reached, continuing:
	select_element( T, DrawnValue, CurrentSum + Probability ).



% @doc Draws the specified number of elements at random of the specified list,
% knowing that they all have the same probability of being drawn initially, but
% when an element is drawn, it is removed from the candidate list so that the
% next drawing operates on the resulting shortened list, so that any element is
% drawn up to once.
%
% Note that the specified list must contain at least the specified count of
% elements.
%
-spec draw_elements_from( list(), count() ) -> list().
draw_elements_from( ElementList, Count ) ->

	{ DrawnElems, _RemainingElems } =
		extract_elements_from( ElementList, Count ),

	DrawnElems.



% @doc Extracts the specified number of elements at random from the specified
% list, knowing that they all have the same probability of being extracted
% initially, but when an element is extracted, it is removed from the candidate
% list so that the next extracting operates on the resulting shortened list
% (that is a subset with no duplicates is returned).
%
% Returns the list of extracted elements, and the list of the remaining
% elements.
%
% Note that the specified list must contain at least the specified count of
% elements.
%
-spec extract_elements_from( list(), count() ) ->
				{ ExtractedElems :: list(), RemainingElems :: list() }.
extract_elements_from( ElementList, Count ) ->
	extract_elements_from( ElementList, Count, _AccExtract=[] ).


% (helper)
extract_elements_from( RemainingElems, _Count=0, AccExtract ) ->
	{ AccExtract, RemainingElems };

extract_elements_from( RemainingElems, Count, AccExtract ) ->

	DrawnElem = draw_element( RemainingElems ),

	ShrunkRemainingElems = lists:delete( DrawnElem, RemainingElems ),

	extract_elements_from( ShrunkRemainingElems, Count-1,
						   [ DrawnElem | AccExtract ] ).
