% Copyright (C) 2007-2021 Olivier Boudeville
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


% Gathering of various facilities about lists.
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
-export([ ensure_list/1, ensure_atoms/1, ensure_tuples/1, ensure_pids/1,
		  are_integers/1, check_integers/1, are_pids/1 ]).


% Basic list operations:
-export([ get_element_at/2, insert_element_at/3, extract_element_at/2,
		  remove_first_elements/2, remove_element_at/2, remove_last_element/1,
		  get_last_element/1, extract_last_element/1,
		  get_index_of/2, get_maybe_index_of/2, split_at/2,
		  uniquify/1, uniquify_ordered/1,
		  ensure_is_once_in/2,
		  has_duplicates/1, count_occurrences/1, get_duplicates/1,
		  union/2, intersection/2,
		  difference/2, differences/2,
		  cartesian_product/1,
		  subtract_all_duplicates/2, delete_existing/2, delete_if_existing/2,
		  delete_all_in/2, append_at_end/2,
		  unordered_compare/2, flatten_once/1, filter_out_undefined/1 ]).


% For list of tuples (ex: typically used by the HDF5 binding), extended flatten
% and al:
%
-export([ determine_tuple_info/1, flatten_tuples/1, reconstruct_tuples/2 ]).



% Random operations on lists:
-export([ random_permute/1, random_permute_reciprocal/1,
		  draw_element/1, draw_element/2, draw_element_weighted/1,
		  draw_elements_from/2, extract_elements_from/2 ]).


% An element of a list:
-type element() :: term().


% Either a list of terms, or a term by itself.
%
% Note: different from maybe( list() ).
%
-type maybe_list( T ) :: [ T ] | T.


-export_type([ maybe_list/1 ]).


% Shorthands:

-type count() :: basic_utils:count().
-type positive_index() :: basic_utils:positive_index().


% Section for the checking of lists.


% Ensures that the specified argument is a list: encloses it in a list of its
% own if not already a list.
%
% Note: not to be applied on strings for example.
%
-spec ensure_list( maybe_list( T ) ) -> [ T ].
ensure_list( List ) when is_list( List ) ->
	List;

ensure_list( Term ) ->
	[ Term ].



% Ensures that the specified argument is a list of atoms: encloses any atom in
% a list of its own if not already a list, or check that this list is only
% populated of atoms.
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



% Ensures that the specified argument is a list of tuples: encloses any tuple in
% a list of its own if not already a list, or check that this list is only
% populated of tuples.
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



% Ensures that the specified argument is a list of PIDs: encloses any PID in a
% list of its own if not already a list, or check that this list is only
% populated of PIDs.
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



% Section for basic list operations.


% Index start at position #1, not #0.

% Returns the element in the list at the specified index, in [1..length(List)].
%
% If the index is out of bounds, a function_clause is raised.
%
% Note: usually these kinds of functions should not be used, recursive
% algorithms are a lot more effective, when applicable.
%
% Not tail recursive version:
%
%% get_element_at( List, 1 ) ->
%%	hd(List);
%
%% get_element_at( [ _H | T ], Index ) ->
%%	get_element_at( T, Index-1 ).
%
-spec get_element_at( list(), positive_index() ) -> element().
get_element_at( List, Index ) ->
	%io:format( " - getting element #~B of ~w~n", [ Index, List ] ),
	lists:nth( Index, List ).



% Inserts specified element at specified position in specified list.
%
% For example, insert_element_at(foo, [a,b,c,d], 3) will return
% [a, b, foo, c, d].
%
-spec insert_element_at( element(), list(), positive_index() ) -> list().
insert_element_at( Element, List, Index ) ->

	%io:format( " - inserting element ~p at #~B in ~w~n",
	%		   [ Element, Index, List ] ),

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



% Extracts element from the specified list, at the specified index.
%
% Returns that element and the resulting, shrunk list.
%
% Ex: {b, [a, c]} = extract_element_at([ a, b, c], 2).
%
-spec extract_element_at( list(), positive_index() ) -> { element(), list() }.
extract_element_at( List, Index ) ->
	% Nothing relevant found in the lists module, so:
	extract_element_at( List, Index, _Acc=[] ).


% (helper)
extract_element_at( _List=[ H | T ], _Index=1, Acc ) ->
	{ H, lists:reverse( Acc ) ++ T };

extract_element_at( _List=[], Index, Acc ) ->
	throw( { index_out_of_range, Index + length( Acc ),
			 lists:reverse( Acc ) } );

extract_element_at( _List=[ H | T ], Index, Acc ) ->
	extract_element_at( T, Index-1, [ H | Acc ] ).



% Removes the specified number first elements.
%
% Ex: [c, d , e  = list_utils:remove_first_elements([a, b, c, d, e], 2).
%
-spec remove_first_elements( list(), count() ) -> list().
remove_first_elements( List, _Count=0 ) ->
	List;

remove_first_elements( List, Count ) ->
	remove_first_elements( tl( List ), Count-1 ).



% Returns a list corresponding to the specified one with the element at
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
% Tail recursive version:
%
-spec remove_element_at( list(), positive_index() ) -> list().
remove_element_at( List, Index ) ->
	remove_element_at( List, Index, _Result=[] ).

remove_element_at( [ _H | RemainingList ], 1, Result ) ->
	lists:reverse( Result ) ++ RemainingList;

remove_element_at( [ H | RemainingList ], Index, Result ) ->
	remove_element_at( RemainingList, Index-1, [ H | Result ] ).



% Removes the last element of the specified list.
%
% Crashes (with 'no function clause') if the input list is empty.
%
% Note: not computationnally efficient, usually removing the last element
% suggests a bad code design.
%
-spec remove_last_element( list() ) -> list().
remove_last_element( List ) ->
	lists:droplast( List ).

%	remove_last_element( List, _Acc=[] ).


%remove_last_element( _List=[ _Last ], Acc ) ->
%	lists:reverse( Acc );

%remove_last_element( _List=[ H | T ], Acc ) ->
%	remove_last_element( T, [ H | Acc ] ).



% Returns the last element of the specified list.
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



% Extracts the last element of the specified (non-empty) list, returning a pair
% made of that element and of the remainder of the list (in its original order).
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
%	extract_last_element( List, _Acc=[] ).


% (helper)
%extract_last_element( _List=[ LastElement ], Acc ) ->
%	{ LastElement, lists:reverse( Acc ) };
%
%extract_last_element( _List=[ H | T ], Acc ) ->
%	extract_last_element( T, [ H | Acc ] ).



% Returns the index, in [1..length(List)], of the (first occurrence of the)
% specified element in the specified list.
%
% Throws an exception if the element is not found.
%
% Ex: 3 = get_index_of( bar, [ foo, ugh, bar, baz ] )
%
-spec get_index_of( element(), list() ) -> count().
get_index_of( Element, List ) ->
	case get_maybe_index_of( Element, List ) of

		undefined ->
			throw( { non_existing_element, Element } );

		I ->
			I

	end.



% Returns the index, in [1..length(List)], of the (first occurrence of the)
% specified element in the specified list, or 'undefined' if the element is not
% found.
%
% Ex:
%   3 = get_maybe_index_of( bar, [ foo, ugh, bar, baz ] )
%   undefined = get_maybe_index_of( xxx, [ foo, ugh, bar, baz ] )
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



% Splits the specified (plain) list in two parts (two plain lists, that are
% returned): the first contains the first elements, up to MaxLen included (in
% reverse order), and the second the others (if any).
%
% Ex: split_at(3, [a, b, c, d, e]) = {[c, b, a], [d, e]}
%
-spec split_at( count(), list() ) -> { list(), list() }.
split_at( MaxLen, List ) ->
	split_at( List, _Count=0, MaxLen, _Acc=[] ).


% (helper)
split_at( InputList, _Count=MaxLen, MaxLen, AccList ) ->
	% Max len reached, stopping here:
	{ AccList, InputList };

split_at( _InputList=[], _Count, _MaxLen, AccList ) ->
	% Input list exhausted:
	{ AccList, _InputList=[] };


split_at( _List=[ H | T ], Count, MaxLen, Acc ) ->
	split_at( T, Count+1, MaxLen, [ H | Acc ] ).




% Returns a list whose elements are the ones of the specified list, except that
% they are unique (all their duplicates have been removed).
%
% No specific order is respected in the returned list.
%
% Ex: if L = [1,2,3,2,2,4,5,5,4,6,6,5], then uniquify(L) is:
% [3,6,2,5,1,4].
%
-spec uniquify( list() ) -> list().
uniquify( List ) ->
	% There is probably a more efficient way of doing the same:
	sets:to_list( sets:from_list( List ) ).


% Returns a list whose elements are the ones of the specified list, except that
% they are unique (all their duplicates have been removed), while the order in
% the kept elements is preserved.
%
% Expected to be a bit slower than uniquify/1.
%
% Ex: if L = [1,3,2,3,2,2,4,5,5,4,6,6,5], then uniquify_ordered(L) is:
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



% Ensures that specified element is included once in specified unordered list
% (supposed to contain it already up to once).
%
% Note: refer to set_utils for a more proper implementation of set.
%
-spec ensure_is_once_in( term(), list() ) -> list().
ensure_is_once_in( Elem, List ) ->

	case lists:member( Elem, List ) of

		true ->
			List;

		false ->
			[ Elem | List ]

	end.



% Tells whether there are in the specified list elements that are present more
% than once.
%
has_duplicates( List ) ->
	length( uniquify( List ) ) =/= length( List ).



% Counts the number of occurences of all elements in the specified list: returns
% an (unordered) list of {Term,Count} pairs, where each term is associated to
% the total number of its occurrences (1 or above) in the specified list.
%
-spec count_occurrences( list() ) -> [ { element(), count() } ].
count_occurrences( List ) ->
	count_occurrences( List, _Acc=[] ).

count_occurrences( _List=[], Acc ) ->
	Acc;

count_occurrences( _List=[ Term | T ], Acc ) ->

	% trace_utils:debug_fmt( "Inquiring about term '~p' into ~p.",
	% [ Term, T ] ),

	case count_and_filter_term( Term, _InitialList=T, _FilteredList=[],
								_InitialCount=0 ) of

		not_found ->
			% No a duplicated element, just iterating on the next term:
			count_occurrences( T, [ { Term, 1 } | Acc ] );

		{ TermCount, FilteredList } ->
			% We already extracted the first Term:
			count_occurrences( FilteredList, [ { Term, TermCount+1 } | Acc ] )

   end.



% Returns the duplicates in the specified list: returns an (unordered) list of
% {DuplicatedTerm,DuplicationCount} pairs, where each duplicated term (i.e. a
% term present more than once) is specified, alongside the total number of
% occurrences of that term in the specified list.
%
% Note: as a consequence, a term that is not in the specified list, or that is
% present there only once, will not be referenced in the returned list; use
% count_occurrences/1 if wanting to include the terms that are listed only once
% each.
%
% Ex: list_utils:get_duplicates([a,a,b,b,b,c,d,d]) = [{b,3},{d,2},{a,2}]
%
-spec get_duplicates( list() ) -> [ { element(), count() } ].
get_duplicates( List ) ->
	get_duplicates( List, _Acc=[] ).

get_duplicates( _List=[], Acc ) ->
	Acc;

get_duplicates( _List=[ Term | T ], Acc ) ->

	% trace_utils:debug_fmt( "Inquiring about term '~p' into ~p.",
	% [ Term, T ] ),

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



% Returns the union of the two specified lists, i.e. the list of all elements
% that are in either list.
%
-spec union( list(), list() ) -> list().
union( L1, L2 ) ->
	%uniquify( L1 ++ L2 ).
	set_utils:to_list( set_utils:union( set_utils:from_list( L1 ),
										set_utils:from_list( L2 ) ) ).



% Returns the intersection of the two specified lists, i.e. the list of all
% elements that are in both lists.
%
% See also: subtract_all_duplicates/2.
%
-spec intersection( list(), list() ) -> list().
intersection( L1, L2 ) ->
	%set_utils:to_list( set_utils:intersection( set_utils:from_list( L1 ),
	%											set_utils:from_list( L2 ) ) ).
	lists:filter( fun( E ) -> lists:member( E, L2 ) end, L1 ).



% Returns the difference between the first specified list and the second,
% i.e. the elements of the first list that are not in the second one.
%
-spec difference( list(), list() ) -> list().
difference( L1, L2 ) ->
	set_utils:to_list( set_utils:difference( set_utils:from_list( L1 ),
											 set_utils:from_list( L2 ) ) ).



% Returns the differences between the first specified list and the second, as a
% pair, whose first element corresponds to the elements of the first list that
% are not in the second one, and whose second element corresponds to the
% elements of the second list that are not in the first one.
%
-spec differences( list(), list() ) -> { list(), list() }.
differences( L1, L2 ) ->
	{ S1, S2 } = set_utils:differences( set_utils:from_list( L1 ),
										set_utils:from_list( L2 ) ),
	{ set_utils:to_list( S1 ), set_utils:to_list( S2 ) }.



% Returns the cartesian product of the specified lists (collected in a top-level
% list).
%
% Ex: cartesian_product( [ [a,b,c], [d,e], [f] ] ) =
% [ [a,d,f], [a,e,f], [b,d,f], [b,e,f], [c,d,f], [c,e,f] ]
%
-spec cartesian_product( [ [ T ] ] ) -> [ [ T ] ].
cartesian_product( [ SingleList ] ) ->
	[ [ E ] || E <- SingleList ];

cartesian_product( [ List | OtherLists ] ) ->
	[ [ E | SubList ]
	  || E <- List, SubList <- cartesian_product( OtherLists ) ].



% Returns a list equal to L1 except that all elements found in L2 have been
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



% Returns a copy of the specified list where the first element matching Elem is
% deleted, ensuring at least one of these elements exists (as opposed to
% lists:delete/2). The order of the specified list is preserved.
%
-spec delete_existing( element(), list() ) -> list().
delete_existing( Elem, List ) ->

	% We keep a copy of the original list to be able to generate better error
	% messages:
	%
	delete_existing( Elem, List, _OriginalList=List, _Acc=[] ).


delete_existing( Elem, _List=[], OriginalList, _Acc ) ->
	throw( { element_to_delete_not_found, Elem, OriginalList } );

delete_existing( Elem, _List=[ Elem | T ], _OriginalList, Acc ) ->
	% The first element found stops the iteration:
	lists:reverse( Acc ) ++ T;

delete_existing( Elem, _List=[ H | T ], OriginalList, Acc ) ->
	delete_existing( Elem, T, OriginalList, [ H | Acc ] ).



% Deletes the first matching of specified element from specified list, returning
% whether an element has been removed: either the 'not_found' atom (in which
% case the list remained the same) or the corresponding new list (same order and
% content, except first occurrence removed).
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
	lists:reverse( Acc) ++ T;

delete_if_existing( Elem, _List=[ H | T ], Acc ) ->
	delete_if_existing( Elem, T, [ H | Acc ] ).



% Returns a copy of the specified list where all elements matching Elem are
% deleted, whether or not there is any.
%
% The element order of the specified list is preserved.
%
-spec delete_all_in( element(), list() ) -> list().
delete_all_in( Elem, List ) ->
	% A list comprehension would have sufficed:
	delete_all_in( Elem, List, _Acc=[] ).


delete_all_in( _Elem, _List=[], Acc ) ->
	lists:reverse( Acc );

delete_all_in( Elem, _List=[ Elem | T ], Acc ) ->
	% An element not to retain:
	delete_all_in( Elem, T, Acc );

delete_all_in( Elem, _List=[ H | T ], Acc ) ->
	% Non-matching, keep it:
	delete_all_in( Elem, T, [ H | Acc ] ).




% Appends specified element at the end of specified list, without changing the
% order of the list.
%
% Ex: append_at_end(d, [a,b,c]) returns [a,b,c,d].
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
%	L ++ ElemList;
%
append_at_end( Elem, L ) when is_list( L ) ->
	% Should be more efficient than:
	%lists:reverse( [ Elem | lists:reverse( L ) ] ):
	L ++ [ Elem ].



% Returns whether the specified list contains only integers.
-spec are_integers( term() ) -> boolean().
are_integers( [] ) ->
	true;

are_integers( [ H | T ] ) when is_integer( H ) ->
	are_integers( T );

are_integers( _ ) ->
	false.


% Checks that specified argument is a list of integers.
-spec check_integers( term() ) -> void().
check_integers( Any ) ->
	true = are_integers( Any ).


% Returns whether the specified list contains only PIDs.
-spec are_pids( term() ) -> boolean().
are_pids( [] ) ->
	true;

are_pids( [ H | T ] ) when is_pid( H ) ->
	are_pids( T );

are_pids( _ ) ->
	false.



% Compares the two specified lists with no regard to the order of their
% elements: returns true iff they have the exact same elements (differentiating
% between 1 and 1.0 for example), possibly in a different order.
%
-spec unordered_compare( list(), list() ) -> boolean().
unordered_compare( L1, L2 ) ->
	lists:sort( L1 ) =:= lists:sort( L2 ).



% Flattens specified list of lists only once (i.e. on a single level), as
% opposed to indefinitively (as done recursively by lists:flatten/1); provides
% more control than a recursive counterpart.
%
% Element order is preserved.
%
% Ex: if L=[ [1], [2,[3,4]] ], lists:flatten(L) yields [1,2,3,4] whereas
% list_utils:flatten_once(L) should yield [1,2,[3,4]].
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



% Removes (filters out) all elements equal to 'undefined'; preserves order.
-spec filter_out_undefined( list() ) -> list().
filter_out_undefined( L ) ->
	% Or: delete_all_in( undefined, L ).
	[ E || E <- L, E =/= undefined ].



% Determines tuple-related information about specified datastructure: returns
% {TupleCount, TupleSize}, supposing the list is made of tuples of uniform
% sizes.
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



% Flattens a list of tuples into a simple list of their elements, without
% tuples and in the same order.
%
% Ex: flatten_tuples([{1, 2, 3}, {4, 5, 6}]) = [1, 2, 3, 4, 5, 6])
%
-spec flatten_tuples( [ tuple() ] ) -> list().
flatten_tuples( List ) ->
	flatten_tuples( List, _Acc=[] ).


flatten_tuples( _List=[], Acc ) ->
	lists:reverse( Acc );

flatten_tuples( [ H | T ], Acc ) ->
	NewAcc = lists:reverse( tuple_to_list( H ) ) ++ Acc,
	flatten_tuples( T, NewAcc ).



% Reconstructs a list of tuples of specified size from the specified flat list.
%
% Ex: reconstruct_tuples([ 1, 2, 3, 4, 5, 6 ], 3) = [{1, 2, 3}, {4, 5, 6}]
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


% Returns a random uniform permutation of the specified list.
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
	Index = random_utils:get_random_value( RemainingLen ),

	%io:format( "Index=~p, ", [ Index ] ),

	% We put the drawn element at head, and recurse in the remaining list:
	[ get_element_at( List, Index )
		| random_permute( remove_element_at( List, Index ),
						  RemainingLen-1 ) ].



% Returns a reciprocal random uniform permutation of the specified list,
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
	ReciprocalIndex = lists:reverse( [ random_utils:get_random_value( L )
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



% Draws one element at random of the specified list, knowing that they all have
% the same probability of being drawn (uniform probability).
%
-spec draw_element( list() ) -> element().
draw_element( _ElementList=[] ) ->
	throw( cannot_draw_from_empty_list );

draw_element( ElementList ) ->
	Len = length( ElementList ),
	draw_element( ElementList, Len ).



% Draws one element at random of the specified list, whose length must be the
% specified one (allows to precompute it once for multiple drawings), knowing
% all elements have the same probability of being drawn (uniform probability).
%
-spec draw_element( list(), count() ) -> element().
draw_element( ElementList, Length ) ->

	DrawnIndex = random_utils:get_random_value( Length ),

	get_element_at( ElementList, DrawnIndex ).

	% Alternate, less efficient, implementation:

	% Same probability:
	%UniformList = [ { Elem, 1 } || Elem <- ElementList ],
	%draw_element_weighted( UniformList ).



% Draws one element at random of the specified list, which is a list of {
% Element, Probability } pairs: returns the drawn element, knowing that it will
% be chosen according to the probability associated to each element.
%
% Probabilities are managed as positive (possibly null) numbers (integer or
% floating-point values) defined relatively to each other (they do not have to
% sum up to 1.0).
%
% Ex: ElementList = [ {first,1}, {second,2}, {third,1} ] is excepted to return
% on average 'second' twice as frequently as 'first' or 'third'.
%
% Using [ {first,1}, {second,0}, {third,1} ] instead would mean that 'second'
% would never be drawn.
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
	DrawnValue = random_utils:get_random_value( Sum ),
	%io:format( "draw_element: drawn ~B.~n", [ DrawnValue ] ),
	select_element( ElementList, DrawnValue, _CurrentSum=0 ).



select_element( [ { Element, Probability } | _T ], DrawnValue, CurrentSum )
		when Probability + CurrentSum >= DrawnValue ->
	% Just gone past the probability range:
	Element;

select_element( [ { _Element, Probability } | T ], DrawnValue, CurrentSum ) ->
	% Drawn value still not reached, continuing:
	select_element( T, DrawnValue, CurrentSum + Probability ).



% Draws the specified number of elements at random of the specified list,
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



% Extracts the specified number of elements at random from the specified list,
% knowing that they all have the same probability of being extracted initially,
% but when an element is extracted, it is removed from the candidate list so
% that the next extracting operates on the resulting shortened list (i.e. a
% subset with no duplicates is returned).
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


extract_elements_from( RemainingElems, _Count=0, AccExtract ) ->
	{ AccExtract, RemainingElems };

extract_elements_from( RemainingElems, Count, AccExtract ) ->
	DrawnElem = draw_element( RemainingElems ),
	ShrunkRemainingElems = lists:delete( DrawnElem, RemainingElems ),
	extract_elements_from( ShrunkRemainingElems, Count-1,
						   [ DrawnElem | AccExtract ] ).
