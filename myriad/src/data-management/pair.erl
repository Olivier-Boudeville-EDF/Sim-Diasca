% Copyright (C) 2015-2023 Olivier Boudeville
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


% @doc Minor utilities to manage <b>pairs</b> (that is 2-element tuples).
-module(pair).

-export([ first/1, firsts/1, second/1, seconds/1,
		  swap/1, check_list/1, to_list/1, to_string/1 ]).

-compile( { inline, [ first/1, second/1, swap/1 ] } ).


-type element() :: any().

-type pair() :: { element(), element() }.

-export_type([ pair/0 ]).


% @doc Returns the first element of the specified pair.
-spec first( pair() ) -> element().
first( { X, _Y } ) ->
	X.


% @doc Returns the first elements of the specified list of pairs, in-order.
%
% Does not check whether non-pairs exist in the input list.
%
-spec firsts( [ pair() ] ) -> [ element() ].
firsts( Pairs ) ->
	cond_utils:if_defined( myriad_debug_datastructures, check_list( Pairs ) ),
	[ X || { X, _Y } <- Pairs ].



% @doc Returns the second element of the specified pair.
-spec second( pair() ) -> element().
second( { _X, Y } ) ->
	Y.


% @doc Returns the second elements of the specified list of pairs, in-order.
%
% Does not check whether non-pairs exist in the input list.
%
-spec seconds( [ pair() ] ) -> [ element() ].
seconds( Pairs ) ->
	cond_utils:if_defined( myriad_debug_datastructures, check_list( Pairs ) ),
	[ Y || { _X, Y } <- Pairs ].



% @doc Returns a pair whose elements have been swapped compared to the specified
% one.
%
-spec swap( pair() ) -> pair().
swap( { X, Y } ) ->
	{ Y, X }.



% @doc Throws an exception if the specified list is not a list of pairs.
-spec check_list( term() ) -> void().
check_list( Term ) ->
	check_list( Term, Term ).


% (helper)
check_list( [], _Term ) ->
	true;

check_list( [ { _X, _Y } | T  ], Term ) ->
	check_list( T, Term );

check_list( Other, Term ) ->
	throw( { not_list_of_pairs, Other, Term } ).



% @doc Returns a list of two elements corresponding to the specified pair.
-spec to_list( pair() ) -> [ element() ].
to_list( { F, S } ) ->
	[ F, S ].


% @doc Returns a textual description of the specified pair.
-spec to_string( pair() ) -> text_utils:ustring().
to_string( { X, Y } ) ->
	text_utils:format( "{ ~p, ~p }", [ X, Y ] ).
