% Copyright (C) 2016-2021 Olivier Boudeville
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
% Creation date: July 1, 2016.


% Gathering of various facilities about sets.
%
% A set is a container that:
%
% - does not allow duplicates (adding an element more than once is like adding
% it only once)
%
% - has no intrinsic order (yet can be iterated over)
%
% Notes:
%
% - we provide here a basic, general-purpose set support, and do not rely on any
% lighter, alternate level of indirection
%
% - if you feel the need for an associative table whose values do no matter,
% your actual need is a set!
%
% See set_utils_test.erl for the corresponding test.
%
% See also: list_utils.erl and set_utils_test.erl.
%
-module(set_utils).



% Set-related operations are:
-export([ new/0, singleton/1, new/1, are_equal/2,
		  add/2, add_as_new/2, add_element_list/2,
		  union/2, union/1, intersection/2, intersection/1,
		  difference/2, differences/2, is_set/1, check_set/1, is_subset/2,
		  from_list/1, to_list/1,
		  member/2, is_empty/1, size/1,
		  iterator/1, next/1, extract_if_existing/2,
		  delete/2, delete_existing/2, to_string/1 ]).


% Our default elected type of set:
%
% (ordsets may, perhaps in many cases, perform better, yet provide less
% features; ex: no iterators)
%
% Apparently gb_sets are stored as tuples, whose first, top-level element is
% their size (hence fetching the size of a set should be inexpensive)
%
-define( set_impl, gb_sets ).

%-type set() :: gb_sets:set().
-type set() :: ?set_impl:set().


% For homogeneous sets:
%-type set( T ) :: gb_sets:set( T ).
-type set( T ) :: ?set_impl:set( T ).


% Element of a set:
-type element() :: term().


% Internally, a kind of enumeration (list) of the elements in the set:
-type iterator() :: ?set_impl:iter().


-export_type([ set/0, set/1, element/0, iterator/0 ]).


% Shorthands:

-type count() :: basic_utils:count().
-type ustring() :: text_utils:ustring().



% Design notes:
%
% The purpose of this module is to provide a set-like container, iterable yet
% *not preserving order*, able to perform some operations (typically: element
% look-up) more efficiently than plain lists, especially when the number of
% elements becomes significant.



% Returns a new, empty, set.
-spec new() -> set().
new() ->
	?set_impl:new().


% Returns a set comprising only specified element.
%
% More elegant than set_utils:add( Foo, set_utils:new() ).
%
-spec singleton( element() ) -> set().
singleton( Element ) ->
	% Not defined for ordsets:
	%?set_impl:singleton( Element ).
	?set_impl:add_element( Element, ?set_impl:new() ).


% Returns a new set, containing the elements of specified list (possibly
% unordered and containing duplicates).
%
% See singleton/1 if wanting to create a set with one element.
%
-spec new( [ element() ] ) -> set().
new( ElementList ) ->
	?set_impl:from_list( ElementList ).




% Tells whether the two specified sets are equal (i.e. contain exactly the same
% elements).
%
% Note: depending on set_impl, this function may or may not be useless, as
% using the basic '==' term-level operator may be sufficient to compare some
% types of sets (ex: ordsets).
%
-spec are_equal( set(), set() ) -> boolean().
are_equal( Set1, Set2 ) ->
	% Shall be correct (albeit expensive) in all cases:
	?set_impl:is_subset( Set1, Set2 )
		andalso ?set_impl:is_subset( Set2, Set1 ).



% Returns a new set formed from the specified one with specified element
% inserted. If this element is already in the specified set, the returned set is
% the same.
%
-spec add( element(), set() ) -> set().
add( Element, Set ) ->
	?set_impl:add_element( Element, Set ).



% Returns a new set formed from the specified one with specified element
% inserted, checking that this element was not already in the original set
% (otherwise a batmatch exception is thrown).
%
-spec add_as_new( element(), set() ) -> set().
add_as_new( Element, Set ) ->
	case ?set_impl:is_member( Element, Set ) of

		false ->
			?set_impl:add_element( Element, Set );

		true ->
			throw( { already_in_set, Element, ?set_impl:to_list( Set ) } )

	end.



% Returns a set made of the specified set to which the elements of the specified
% plain list have been added.
%
-spec add_element_list( [ element() ], set() ) -> set().
%add_element_list( _PlainList=[], Set ) ->
%  Set;

%add_element_list( _PlainList=[ H | T ], Set ) ->
%NewSet = ?set_impl:add_element( H, SetImplSet ),
%add_element_list( T, NewSet ).
add_element_list( Elements, Set ) ->
	AddSet = ?set_impl:from_list( Elements ),
	?set_impl:union( AddSet, Set ).



% Returns the union of the two specified sets.
-spec union( set(), set() ) -> set().
union( FirstSet, SecondSet ) ->
	?set_impl:union( FirstSet, SecondSet ).


% Returns the union of the specified sets.
-spec union( [ set() ] ) -> set().
union( ListOfSets ) ->
	?set_impl:union( ListOfSets ).



% Returns the intersection of the two specified sets.
-spec intersection( set(), set() ) -> set().
intersection( FirstSet, SecondSet ) ->
	?set_impl:intersection( FirstSet, SecondSet ).


% Returns the intersection of the specified sets.
-spec intersection( [ set() ] ) -> set().
intersection( ListOfSets ) ->
	?set_impl:intersection( ListOfSets ).



% Returns the difference between the first specified set and the second,
% i.e. the elements of the first set that are not in the second one.
%
-spec difference( set(), set() ) -> set().
difference( FirstSet, SecondSet ) ->
	?set_impl:difference( FirstSet, SecondSet ).


% Returns the differences between the first specified set and the second, as a
% pair, whose first element corresponds to the elements of the first set that
% are not in the second one, and whose second element corresponds to the
% elements of the second set that are not in the first one.
%
-spec differences( set(), set() ) -> { set(), set() }.
differences( FirstSet, SecondSet ) ->
	{ ?set_impl:difference( FirstSet, SecondSet ),
	  ?set_impl:difference( SecondSet, FirstSet ) }.



% Returns whether the specified term appears to be a legit set.
-spec is_set( term() ) -> boolean().
is_set( Term ) ->
	?set_impl:is_set( Term ).



% Ensures that the specified term is a set, throws an exception if not.
-spec check_set( term() ) -> void().
check_set( Term ) ->
	case is_set( Term ) of

		true ->
			ok;

		false ->
			throw( { not_a_set, Term } )

	end.



% Tells whether the first set is a subset of the second, i.e. if each element of
% the first is also in the second.
%
-spec is_subset( set(), set() ) -> boolean().
is_subset( FirstSet, SecondSet ) ->
	?set_impl:is_subset( FirstSet, SecondSet ).



% Returns a set created from specified list of elements.
-spec from_list( [ element() ] ) -> set().
from_list( List ) ->
	?set_impl:from_list( List ).



% Returns a list created from the elements of specified set.
-spec to_list( set() ) -> [ element() ].
to_list( Set ) ->
	?set_impl:to_list( Set ).



% Returns true iff specified element is an element of specified set.
-spec member( element(), set() ) -> boolean().
member( Element, Set ) ->
	?set_impl:is_member( Element, Set ).



% Returns whether the specified set is empty.
-spec is_empty( set() ) -> boolean().
is_empty( Set ) ->
	% Not defined for ordsets:
	%?set_impl:is_empty( Set ).
	0 =:= ?set_impl:size( Set ).



% Returns the number of elements in specified set.
-spec size( set() ) -> count().
size( Set ) ->
	?set_impl:size( Set ).



% Note: iterating could be done with a fold as well (ordsets).


% Returns an iterator that can be used for traversing the entries of the
% specified set.
%
% Note: the iterator is *not* the first iterated element of a set: next/1 shall
% be used even for the very first element.
%
-spec iterator( set() ) -> iterator().
iterator( Set ) ->
	?set_impl:iterator( Set ).



%-spec iterator_from( element(), set() ) ->
%iterator_from( Element, Set ) ->

% Allows the iterators to be gone through.
-spec next( iterator() ) -> { element(), iterator() } | 'none'.
next( Iterator ) ->
	?set_impl:next( Iterator ).



% Extracts specified element (if any) from specified set, i.e. removes it from
% the returned set.
%
% Otherwise, i.e. if that element does not exist in the specified set, returns
% false.
%
-spec extract_if_existing( element(), set() ) -> 'false' | set().
extract_if_existing( Element, Set ) ->

	case ?set_impl:is_member( Element, Set ) of

		true ->
			?set_impl:del_element( Element, Set );

		false ->
			false

	end.



% Removes the specified element (if any) from the specified set, and returns the
% resulting set.
%
% Note: does not fail if the element was not in the set; use delete_existing/2
% to ensure that the element was present.
%
-spec delete( element(), set() ) -> set().
delete( Element, Set ) ->
	?set_impl:del_element( Element, Set ).



% Ensures that the specified element was indeed in the specified set before
% removing it, and returning the resulting set.
%
% Note: use delete/2 to delete an element without checking whether the element
% was already present in the set.
%
-spec delete_existing( element(), set() ) -> set().
delete_existing( Element, Set ) ->

	case ?set_impl:is_element( Element, Set ) of

		true ->
			?set_impl:del_element( Element, Set );

		false ->
			throw( { non_existing_element_to_delete, Element,
					 ?set_impl:to_list( Set ) } )

	end.



% Returns a textual representation of the specified set.
-spec to_string( set() ) -> ustring().
to_string( Set ) ->

	case ?set_impl:size( Set ) of

		0 ->
			"empty set";

		1 ->
			[ Elem ] = ?set_impl:to_list( Set ),
			text_utils:format( "set containing a single element: ~p",
							   [ Elem ] );

		S ->
			ElemStrings = [ text_utils:format( "~p", [ E ] )
							|| E <- ?set_impl:to_list( Set ) ],

			text_utils:format( "set containing following ~B elements: ~ts",
					[ S, text_utils:strings_to_string( ElemStrings ) ] )

	end.
