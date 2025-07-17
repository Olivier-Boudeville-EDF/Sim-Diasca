% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Wednesday, September 18, 2024.

-module(tagged_list).

-moduledoc """
Utilities to manage **tagged lists** (strict or not), i.e. lists of tagged pairs
(pairs whose first element is an atom) and possibly mere atoms (if the tagged
list is not strict).

Tagged lists may have entries with the same keys (a key being either a
standalone one or the first element of a pair); use check_duplicate_keys*/1 to
detect it.

See also the pair module.
""".


-type tagged_element( T ) :: atom() | tagged_pair( T ).

-type tagged_element() :: atom() | tagged_pair().


-doc """
A specific case of proplist(), whose tuples are only pairs made of an atom and a
value of the specified type (and may comprise mere atoms as well).

Not equivalent to list_table() either (which does not support mere atoms, whose
keys may not be atoms, and cannot be duplicated).
""".
-type tagged_list( T ) :: [ tagged_element( T ) ].


-doc """
A specific case of proplist, whose tuples are only pairs made of an atom and of
any value, and may comprise mere atoms as well.
""".
-type tagged_list() :: tagged_list( element() ).


-type invalid_tagged_list_reason() :: 'non_atom_tag'
									| 'not_atom_or_tagged_pair'
									| 'not_list'.


-doc """
A specific case of tagged list, whose tuples are only pairs made of an atom and
a value of the specified type (mere atoms not supported).

Not equivalent to list_table() either (whose keys may not be atoms, and cannot
be duplicated).
""".
-type strict_tagged_list( T ) :: [ tagged_pair( T ) ].


-doc """
A specific case of tagged list, whose tuples are only pairs made of an atom and
of any value (mere atoms not supported).
""".
-type strict_tagged_list() :: [ tagged_pair( element() ) ].


-type invalid_strict_tagged_list_reason() :: 'non_atom_tag'
										   | 'not_tagged_pair'
										   | 'not_list'.



-export_type([ tagged_element/0, tagged_element/1,
			   tagged_list/0, tagged_list/1,
			   strict_tagged_list/0, strict_tagged_list/1,
			   invalid_tagged_list_reason/0,
			   invalid_strict_tagged_list_reason/0 ]).



% For tagged lists:
-export([ is_tagged_list/1, is_strict_tagged_list/1,
		  check_tagged_list/1, ensure_tagged_list/1, check_duplicate_keys/1,
		  extract_atom_if_existing/2, extract_atom_with_default/3,
		  extract_pair_if_existing/2, extract_pair_with_default/3 ]).


% For strict tagged lists:
-export([ check_strict_tagged_list/1, ensure_strict_tagged_list/1,
		  check_strict_duplicate_keys/1,
		  extract_pair_if_existing_strict/2,
		  extract_pair_with_default_strict/3 ]).



% Implementation notes:


% Regarding tagged lists:

% Proplists can have any type as keys (not just atoms), and may include tuples
% of any size (not just pairs).

% Note: if having an (at least mostly) constant list, possibly containing a
% large number of elements, and on which the main operation that shall be fast
% is element look-up (telling whether or not a given element is in the list),
% then the 'set' type shall be preferred (see set_utils.erl for that).


% Type shorthands:

-type maybe_list( T ) :: list_utils:maybe_list( T ).
-type duplicate_info() :: list_utils:duplicate_info().

-type element() :: pair:element().

-type tagged_pair( T ) :: pair:tagged_pair( T ).
-type tagged_pair() :: pair:tagged_pair().



% Listing implementations for both types of tagged lists:



-doc """
Confirms that the specified term is a tagged list, otherwise returns an
explanation pair.
""".
-spec is_tagged_list( term() ) ->
		'true'
	  | { invalid_tagged_list_reason(), FirstFaultyEntry :: term() }.
is_tagged_list( _Term=[] ) ->
	true;

is_tagged_list( [ { Atom, _Y } | T ] ) when is_atom( Atom ) ->
	is_tagged_list( T );

is_tagged_list( [ P={ _X, _Y } | _T ] ) ->
	{ non_atom_tag, P };

is_tagged_list( [ Atom | T ] ) when is_atom( Atom ) ->
	is_tagged_list( T );

is_tagged_list( [ Other | _T ] ) ->
	{ not_atom_or_tagged_pair, Other };

is_tagged_list( Other ) ->
	{ not_list, Other }.



-doc """
Throws an exception if the specified term is not a tagged list, otherwise
returns this exact list (for chaining).
""".
-spec check_tagged_list( term() ) -> tagged_list().
check_tagged_list( Term ) ->
	case is_tagged_list( Term ) of

		true ->
			Term;

		P -> % { Reason, FirstFaultyEntry } ->
			throw( { not_tagged_list, P, Term } )

	end.




-doc """
Confirms that the specified term is a strict tagged list, otherwise returns an
explanation pair.
""".
-spec is_strict_tagged_list( term() ) ->
		'true'
	  | { invalid_strict_tagged_list_reason(), FirstFaultyEntry :: term() }.
is_strict_tagged_list( _Term=[] ) ->
	true;

is_strict_tagged_list( [ { X, _Y } | T ] ) when is_atom( X ) ->
	is_strict_tagged_list( T );

is_strict_tagged_list( [ P={ _X, _Y } | _T ] ) ->
	{ non_atom_tag, P };

is_strict_tagged_list( [ Other | _T ] ) ->
	{ not_tagged_pair, Other };

is_strict_tagged_list( Other ) ->
	{ not_list, Other }.



-doc """
Throws an exception if the specified term is not a strict tagged list, otherwise
returns this exact list (for chaining).
""".
-spec check_strict_tagged_list( term() ) -> strict_tagged_list().
check_strict_tagged_list( Term ) ->
	case is_strict_tagged_list( Term ) of

		true ->
			Term;

		P -> % { Reason, FirstFaultyEntry } ->
			throw( { not_strict_tagged_list, P, Term } )

	end.




-doc """
Ensures that the specified term is a tagged list, that is a list of atoms or of
pairs whose first element is an atom: encloses any of such element in a list of
its own if not already a list, or checks that this list is only populated as
expected; returns a corresponding tagged list, in the original order.
""".
-spec ensure_tagged_list( maybe_list( atom() | tagged_pair() ) ) ->
											tagged_list().
ensure_tagged_list( Atom ) when is_atom( Atom ) ->
	[ Atom ];

ensure_tagged_list( P={ Atom, _Any } ) when is_atom( Atom ) ->
	[ P ];

ensure_tagged_list( L ) when is_list( L ) ->
	ensure_tagged_list_helper( L, _Acc=[] );

ensure_tagged_list( Other ) ->
	throw( { not_tagged_listable, Other } ).


% Not reusing here ensure_tagged_list/1, as nested lists are not permitted:
ensure_tagged_list_helper( _L=[], Acc ) ->
	lists:reverse( Acc );

ensure_tagged_list_helper( _L=[ Atom | T ], Acc ) when is_atom( Atom ) ->
	ensure_tagged_list_helper( T, [ Atom | Acc ] );

ensure_tagged_list_helper( _L=[ P={ Atom, _Any } | T ], Acc )
										when is_atom( Atom ) ->
	ensure_tagged_list_helper( T, [ P | Acc ] );

ensure_tagged_list_helper( _L=[ _P={ Other, _Any } | _T ], _Acc ) ->
	throw( { not_an_atom_tag, Other } );

ensure_tagged_list_helper( [ Other | _T ], _Acc ) ->
	throw( { invalid_tagged_list_element, Other } ).



-doc """
Returns an (unordered) list of the duplicated keys found in the specified tagged
list.
""".
-spec check_duplicate_keys( tagged_list() ) -> duplicate_info().
check_duplicate_keys( TaggedList ) ->
	Ks = [ case E of

				{ K, _V } ->
					K;

				A when is_atom( A ) ->
					A

		   end || E <- TaggedList ],

	list_utils:get_duplicates( Ks ).



-doc """
Returns an (unordered) list of the duplicated keys found in the specified strict
tagged list.
""".
-spec check_strict_duplicate_keys( strict_tagged_list() ) -> duplicate_info().
check_strict_duplicate_keys( StrictTaggedList ) ->
	Ks = [ K || { K, _V } <- StrictTaggedList ],
	list_utils:get_duplicates( Ks ).



-doc """
Ensures that the specified term is a strict tagged list, that is a list of pairs
whose first element is an atom: encloses any of such element in a list of its
own if not already a list, or checks that this list is only populated as
expected; returns a corresponding strict tagged list, in the original order.
""".
-spec ensure_strict_tagged_list( maybe_list( tagged_pair() ) ) ->
											strict_tagged_list().
ensure_strict_tagged_list( P={ Atom, _Any } ) when is_atom( Atom ) ->
	[ P ];

ensure_strict_tagged_list( L ) when is_list( L ) ->
	ensure_strict_tagged_list_helper( L, _Acc=[] );

ensure_strict_tagged_list( Other ) ->
	throw( { not_strict_tagged_listable, Other } ).


% Not reusing here ensure_strict_tagged_list/1, as nested lists are not
% permitted:
%
ensure_strict_tagged_list_helper( _L=[], Acc ) ->
	lists:reverse( Acc );

ensure_strict_tagged_list_helper( _L=[ P={ Atom, _Any } | T ], Acc )
										when is_atom( Atom ) ->
	ensure_strict_tagged_list_helper( T, [ P | Acc ] );

ensure_strict_tagged_list_helper( _L=[ _P={ Other, _Any } | _T ], _Acc ) ->
	throw( { not_an_atom_tag, Other } );

ensure_strict_tagged_list_helper( [ Other | _T ], _Acc ) ->
	throw( { invalid_strict_tagged_list_element, Other } ).




-doc """
Extracts, from the specified tagged list, the (first occurrence of the)
specified atom: either returns 'not_found' if such an atom was not found, or the
tagged list obtained when the first occurrence of that atom has been removed.
""".
-spec extract_atom_if_existing( atom(), tagged_list() ) ->
										'not_found' | tagged_list().
extract_atom_if_existing( Atom, TaggedList ) ->
	extract_atom_if_existing_helper( Atom, TaggedList, _Acc=[] ).


% Not found at all:
extract_atom_if_existing_helper( _Atom, _TaggedList=[], _Acc ) ->
	not_found;

% First atom found:
extract_atom_if_existing_helper( Atom, _TaggedList=[ Atom | T ], Acc ) ->
	lists:reverse( Acc ) ++ T;

% Other is another atom or a pair:
extract_atom_if_existing_helper( Atom, _TaggedList=[ Other | T ], Acc ) ->
	extract_atom_if_existing_helper( Atom, T, [ Other | Acc ] ).


% No extract_atom_if_existing/2 makes sense for strict tagged lists.



-doc """
Extracts, from the specified tagged list, the (first occurrence of the)
specified atom if found, otherwise returns the specified default; in all cases
returns an element (extracted or default) and a corresponding tagged list.

So either returns the specified atom if found, or the specified default,
together with the resulting tagged list (which is either the original tagged
list if the default is returned, or a shrunk tagged list if an actual extraction
could be done).
""".
-spec extract_atom_with_default( atom(), element(), tagged_list() ) ->
										{ element(), tagged_list() }.
extract_atom_with_default( Atom, DefaultValue, TaggedList ) ->
	extract_atom_with_default_helper( Atom, DefaultValue, TaggedList,
									  _Acc=[] ).


% Not found at all:
extract_atom_with_default_helper( _Atom, DefaultValue, _TaggedList=[], Acc ) ->
	{ DefaultValue, lists:reverse( Acc ) };

% First atom found (no is_atom/1 guard needed):
extract_atom_with_default_helper( Atom, _DefaultValue,
								  _TaggedList=[ Atom | T ], Acc ) ->
	{ Atom, lists:reverse( Acc ) ++ T };

% Other is another atom or a pair:
extract_atom_with_default_helper( Atom, DefaultValue,
								  _TaggedList=[ Other | T ], Acc ) ->
	extract_atom_with_default_helper( Atom, DefaultValue, T, [ Other | Acc ] ).


% No extract_atom_with_default/3 makes sense for strict tagged lists.




-doc """
Extracts, from the specified tagged list, the (first occurrence of the)
key/value pair specified based on its first atom element (its key): either
returns false if no such pair was found, or returns the value associated to the
specified atom (thus in second position of the corresponding pair), together
with the rest of the specified tagged list.
""".
-spec extract_pair_if_existing( atom(), tagged_list() ) ->
									'false' | { element(), tagged_list() }.
extract_pair_if_existing( KeyAtom, TaggedList ) ->
	extract_pair_if_existing_helper( KeyAtom, TaggedList, _Acc=[] ).


% Not found at all:
extract_pair_if_existing_helper( _KeyAtom, _TaggedList=[], _Acc ) ->
	false;

% First pair found:
extract_pair_if_existing_helper( KeyAtom, _TaggedList=[ { KeyAtom, V } | T ],
								 Acc ) ->
	{ V, lists:reverse( Acc ) ++ T };

% Other is atom or pair:
extract_pair_if_existing_helper( KeyAtom, _TaggedList=[ Other | T ], Acc ) ->
	extract_pair_if_existing_helper( KeyAtom, T, [ Other | Acc ] ).



-doc """
Extracts, from the specified strict tagged list, the (first occurrence of the)
key/value pair specified based on its first atom element (its key): either
returns false if no such pair was found, or returns the value associated to the
specified atom (thus in second position of the corresponding pair), together
with the rest of the specified tagged list.
""".
-spec extract_pair_if_existing_strict( atom(), strict_tagged_list() ) ->
							'false' | { element(), strict_tagged_list() }.
extract_pair_if_existing_strict( KeyAtom, StrictTaggedList ) ->
	% Special case of:
	extract_pair_if_existing( KeyAtom, StrictTaggedList ).




-doc """
Extracts, from the specified tagged list, the (first occurrence of the)
key/value pair specified based on its first atom element (its key) if found,
otherwise returns for this element the specified default; in all cases returns
an element (extracted or default) and a corresponding tagged list.

So either returns that default if no such pair was found, or returns the value
associated to the specified key atom (thus in second position of the
corresponding pair), together with the resulting tagged list (which is either
the original tagged list if the default is returned, or a shrunk tagged list if
an actual extraction could be done).
""".
-spec extract_pair_with_default( atom(), element(), tagged_list() ) ->
										{ element(), tagged_list() }.
extract_pair_with_default( KeyAtom, DefaultValue, TaggedList ) ->
	extract_pair_with_default_helper( KeyAtom, DefaultValue, TaggedList,
									  _Acc=[] ).


% Not found at all:
extract_pair_with_default_helper( _KeyAtom, DefaultValue,
								  _TaggedList=[], Acc ) ->
	{ DefaultValue, lists:reverse( Acc ) };

% First pair found:
extract_pair_with_default_helper( KeyAtom, _DefaultValue,
								  _TaggedList=[ { KeyAtom, V } | T ], Acc ) ->
	{ V, lists:reverse( Acc ) ++ T };

% Other is atom or pair:
extract_pair_with_default_helper( KeyAtom, DefaultValue,
								  _TaggedList=[ Other | T ], Acc ) ->
	extract_pair_with_default_helper( KeyAtom, DefaultValue, T,
									  [ Other | Acc ] ).



-doc """
Extracts, from the specified strict tagged list, the (first occurrence of the)
key/value pair specified based on its first atom element (its key) if found,
otherwise returns for this element the specified default; in all cases returns
an element (extracted or default) and a corresponding strict tagged list.

So either returns that default if no such pair was found, or returns the value
associated to the specified key atom (thus in second position of the
corresponding pair), together with the resulting strict tagged list (which is
either the original strict tagged list if the default is returned, or a shrunk
strict tagged list if an actual extraction could be done).
""".
-spec extract_pair_with_default_strict( atom(), element(),
		strict_tagged_list() ) -> { element(), strict_tagged_list() }.
extract_pair_with_default_strict( KeyAtom, DefaultValue, StrictTaggedList ) ->
	% Special case of:
	extract_pair_with_default( KeyAtom, DefaultValue, StrictTaggedList ).
