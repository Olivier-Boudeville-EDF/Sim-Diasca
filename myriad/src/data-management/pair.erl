% Copyright (C) 2015-2025 Olivier Boudeville
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
% Creation date: Thursday, April 30, 2015.

-module(pair).

-moduledoc """
Minor utilities to manage **pairs** (that is: all kinds of 2-element tuples).

For lists of tagged pairs, refer to the tagged_list module.
""".


-type element() :: any().

-type pair() :: { element(), element() }.


-doc """
A pair whose first element is of type F (first), and second of type S (second).
""".
-type pair( _F, _S ) :: pair().



-doc """
A pair whose first element is an atom, and second is of the specified type.

For lists of tagged pairs, refer to the tagged_list module.
""".
-type tagged_pair( T ) :: { atom(), T }.


-doc """
A pair whose first element is an atom.

For lists of tagged pairs, refer to the tagged_list module.
""".
-type tagged_pair() :: { atom(), element() }.


-export_type([ pair/0, pair/2, tagged_pair/0, tagged_pair/1 ]).


-export([ first/1, firsts/1, second/1, seconds/1,
		  unzip/1, swap/1, check_list/1, to_list/1, to_string/1 ]).

-compile( { inline, [ first/1, second/1, swap/1 ] } ).



-doc "Returns the first element of the specified pair.".
-spec first( pair() ) -> element().
first( _P={ X, _Y } ) ->
	X.



-doc """
Returns the first elements of the specified list of pairs, in-order.

Does not check whether non-pairs exist in the input list.
""".
-spec firsts( [ pair() ] ) -> [ element() ].
firsts( Pairs ) ->
	cond_utils:if_defined( myriad_debug_datastructures, check_list( Pairs ) ),
	[ X || _P={ X, _Y } <- Pairs ].



-doc "Returns the second element of the specified pair.".
-spec second( pair() ) -> element().
second( _P={ _X, Y } ) ->
	Y.



-doc """
Returns the second elements of the specified list of pairs, in-order.

Does not check whether non-pairs exist in the input list.
""".
-spec seconds( [ pair() ] ) -> [ element() ].
seconds( Pairs ) ->
	cond_utils:if_defined( myriad_debug_datastructures, check_list( Pairs ) ),
	[ Y || _P={ _X, Y } <- Pairs ].



-doc """
Unzips the specified list of pairs.

For example, unzip([{a,1}, {b,2}, {c,3}]) = {[a,b,c], [1,2,3]}.
""".
-spec unzip( [ pair( F, S ) ] ) -> pair( [ F ], [ S ] ).
unzip( Pairs ) ->
	% Mostly to remember that it exists:
	lists:unzip( Pairs ).



-doc """
Returns a pair whose elements have been swapped compared to the specified one.
""".
-spec swap( pair() ) -> pair().
swap( _P={ X, Y } ) ->
	{ Y, X }.



-doc """
Throws an exception if the specified list is not a list of pairs, otherwise
returns this exact list (for chaining).
""".
-spec check_list( term() ) -> [ pair() ].
check_list( Term ) ->
	check_list( Term, Term ).


% (helper)
check_list( [], Term ) ->
	Term;

check_list( [ _P={ _X, _Y } | T ], Term ) ->
	check_list( T, Term );

check_list( Other, Term ) ->
	throw( { not_list_of_pairs, Other, Term } ).



-doc "Returns a list of two elements corresponding to the specified pair.".
-spec to_list( pair() ) -> [ element() ].
to_list( _P={ F, S } ) ->
	[ F, S ].



-doc "Returns a textual description of the specified pair.".
-spec to_string( pair() ) -> text_utils:ustring().
to_string( _P={ X, Y } ) ->
	text_utils:format( "{ ~p, ~p }", [ X, Y ] ).
