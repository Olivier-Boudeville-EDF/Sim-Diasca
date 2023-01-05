% Copyright (C) 2021-2023 Olivier Boudeville
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
% Creation date: Sunday, September 26, 2021.


% @doc Module implementing the support for vectors of <b>arbitrary
% dimension</b>.
%
% See also:
% - the corresponding arbitrary-dimensioned points (in point.erl) and matrices
% (in matrix.erl)
% - the specialised vectors, such as vector{2,3,4}.erl
%
-module(vector).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For the epsilon define:
-include("math_utils.hrl").



% Implementation notes:
%
% No dependent types, not able to declare a vector(D) type.
%
% Arbitrary vectors and specialised ones have the same form (they are list of
% floats) and therefore do not need conversion primitives between these two
% kinds of types.


-type user_vector() :: [ any_coordinate() ].
% A user-specified vector, as a list (hence not a tuple) with integer or
% floating-point coordinates.



-type vector() :: [ coordinate() ].
% An (internal) vector of arbitrary dimension (with floating-point coordinates).


-type integer_vector() :: [ integer_coordinate() ].
% A vector of arbitrary dimension, with integer coordinates.


-type any_vector() :: vector() | integer_vector().
% A vector of any dimension, with any types of numerical coordinates.


-type unit_vector() :: vector().
% A unit vector, that is a vector of magnitude 1.0.
%
% Defined for documentation purpose.


-type normal() :: vector().
% A vector orthogonal to a plane.
%
% Defined for documentation purpose.


-type unit_normal() :: unit_vector().
% A unit vector orthogonal to a plane.
%
% Defined for documentation purpose.


-type specialised_vector() :: vector2:vector2()
							| vector3:vector3()
							| vector4:vector4().
% A specialised vector that is of one of the specifically supported dimensions.


-export_type([ user_vector/0, vector/0, integer_vector/0, any_vector/0,
			   unit_vector/0, normal/0, unit_normal/0, specialised_vector/0 ]).


-export([ new/1, null/1,
		  from_point/1, to_point/1,
		  dimension/1,
		  add/2, add/1, sub/2, mult/2,
		  are_equal/2,
		  square_magnitude/1, magnitude/1, negate/1, scale/2, normalise/1,
		  dot_product/2,
		  is_unitary/1,
		  check/1, check_integer/1,
		  to_string/1, to_compact_string/1, to_basic_string/1,
		  to_user_string/1 ] ).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type factor() :: math_utils:factor().

-type dimension() :: linear:dimension().
-type coordinate() :: linear:coordinate().
-type integer_coordinate() :: linear:integer_coordinate().
-type any_coordinate() :: linear:any_coordinate().

-type distance() :: linear:distance().
-type square_distance() :: linear:square_distance().

-type point() :: point:point().
-type any_point() :: point:any_point().



% @doc Returns an (arbitrary) vector corresponding to the user-specified one.
-spec new( user_vector() ) -> vector().
%new( UserVector ) when is_tuple( UserVector ) ->
%   new( tuple_to_list( UserVector ) );
% Throws bad_generator anyway if a tuple:
new( UserVector ) -> %when is_list( UserVector ) ->
	[ type_utils:ensure_float( UC ) || UC <- UserVector ].



% @doc Returns an (arbitrary) vector of specified dimension whose coordinates
% are all null.
%
-spec null( dimension() ) -> vector().
null( Dim ) ->
	lists:duplicate( Dim, 0.0 ).


% @doc Returns an (arbitrary) vector corresponding to the specified point.
-spec from_point( any_point() ) -> vector().
from_point( P ) ->
	[ type_utils:ensure_float( C ) || C <- tuple_to_list( P ) ].


% @doc Returns an (arbitrary, and with floating-point coordinates) point
% corresponding to the specified vector.
%
-spec to_point( vector() ) -> point().
to_point( V ) ->
	point:from_vector( V ).



% @doc Returns the dimension of the specified vector.
-spec dimension( any_vector() ) -> dimension().
dimension( V ) ->
	length( V ).



% @doc Returns the sum of the two specified vectors (supposedly of the same
% dimension).
%
-spec add( vector(), vector() ) -> vector().
add( V1, V2 ) ->
	lists:zipwith( fun( C1, C2 ) ->
						C1 + C2
				   end,
				   V1, V2 ).


% @doc Returns the sum of all vectors (supposedly of the same dimension) in the
% specified (supposedly non-empty) list.
%
-spec add( [ vector() ] ) -> vector().
% Just to avoid using null() as Acc0 and thus having to compute the dimension:
add( _Vectors=[ VFirst | VOthers ] ) ->
	lists:foldl( fun( V, AccV ) ->
					add( V, AccV )
				 end,
				 _InitialAcc=VFirst,
				 _List=VOthers ).



% @doc Returns the subtraction of the two specified vectors (supposedly of the
% same dimension): V = V1 - V2.
%
-spec sub( vector(), vector() ) -> vector().
sub( V1, V2 ) ->
	lists:zipwith( fun( C1, C2 ) ->
						C1 - C2
				   end,
				   V1, V2 ).



% @doc Returns the Hadamard product of the two specified vectors.
%
% Each coordinate of the result vector is the product of the coordinates of the
% same ranks in the two input vectors.
%
-spec mult( vector(), vector() ) -> vector().
mult( V1, V2 ) ->
	mult( V1, V2, _Acc=[] ).


mult( _V1=[], _V2=[], Acc ) ->
	lists:reverse( Acc );

mult( _V1=[ C1 | T1 ], _V2=[ C2 | T2 ], Acc ) ->
	mult( T1, T2, [ C1*C2 | Acc ] ).



% No need for an are_close/2 alias.


% @doc Returns true iff the two specified vectors are considered equal.
-spec are_equal( vector(), vector() ) -> boolean().
are_equal( _V1=[], _V2=[] ) ->
	true;

are_equal( _V1=[ C1 | T1 ], _V2=[ C2 | T2 ] ) ->
	case math_utils:are_close( C1, C2 ) of

		true ->
			are_equal( T1, T2 );

		false ->
			false

	end.



% @doc Returns the square of the magnitude of the specified vector.
-spec square_magnitude( vector() ) -> square_distance().
square_magnitude( V ) ->
	square_magnitude( V, _Acc=0.0 ).


% (helper)
square_magnitude( _V=[], Acc ) ->
	Acc;

square_magnitude( _V=[ C | T ], Acc ) ->
	square_magnitude( T, Acc + C*C ).



% @doc Returns the magnitude of the specified vector.
-spec magnitude( vector() ) -> distance().
magnitude( V ) ->
	math:sqrt( square_magnitude( V ) ).



% @doc Negates the specified vector: returns the opposite one (of the same
% magnitude).
%
-spec negate( vector() ) -> vector().
negate( V ) ->
	[ -C || C <- V ].



% @doc Scales the specified vector of the specified factor.
-spec scale( vector(), factor() ) -> vector().
scale( V, Factor ) ->
	[ Factor*C || C <- V ].



% @doc Normalises the specified non-null vector, that is returns it once scaled
% to an unit length (whose magnitude is thus 1.0).
%
-spec normalise( vector() ) -> unit_vector().
normalise( V ) ->
	case magnitude( V ) of

		M when M < ?epsilon ->
			throw( cannot_normalise_null_vector );

		M ->
			scale( V, 1 / M )

	end.



% @doc Returns the dot-product of the two specified vectors.
-spec dot_product( vector(), vector() ) -> float().
dot_product( V1, V2 ) ->
	dot_product( V1, V2, _Acc=0.0 ).


% (helper)
dot_product( _V1=[], _V2=[], Acc ) ->
	Acc;

dot_product( _V1=[ H1 | T1 ], _V2=[ H2 | T2 ], Acc ) ->
	dot_product( T1, T2, Acc + H1*H2 ).



% @doc Returns whether the specified vector is unitary, that is whether it is of
% magnitude 1.0.
%
-spec is_unitary( vector() ) -> boolean().
is_unitary( V ) ->
	% No specific need of computing the square root thereof:
	math_utils:are_equal( 1.0, square_magnitude( V ) ).



% @doc Checks that the specified vector is legit, and returns it.
-spec check( vector() ) -> vector().
check( V ) ->
	type_utils:check_floats( V ).


% @doc Checks that the specified integer vector is legit, and returns it.
-spec check_integer( integer_vector() ) -> integer_vector().
check_integer( V ) ->
	type_utils:check_integers( V ).



% @doc Returns a textual representation of the specified vector; full float
% precision is shown.
%
-spec to_string( vector() ) -> ustring().
to_string( Vector ) ->
	to_user_string( Vector ).



% @doc Returns a compact, textual, informal representation of the specified
% vector.
%
-spec to_compact_string( vector() ) -> ustring().
to_compact_string( Vector ) ->

	%Ws = [ "~w" || _ <- Vector ],
	%FormatStr = "[ " ++ text_utils:join( _Sep=", ", Ws ) ++ " ]",
	%text_utils:format( FormatStr, Vector ).

	text_utils:format( "~w", [ Vector ] ).



% @doc Returns a basic, not even fixed-width for floating-vector coordinates
% (see linear.hrl for width and precision) representation of the specified
% vector.
%
-spec to_basic_string( any_vector() ) -> ustring().
to_basic_string( Vector ) ->

	% Vectors supposed to be lists of floats:
	ElemFormatStr = "[ " ++ ?coord_float_format ++ " ]~n",

	FormatStr = "~n" ++ text_utils:duplicate( length( Vector ), ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; CoordList: ~w.",
	%                       [ FormatStr, CoordList ] ),

	text_utils:format( FormatStr, Vector ).



% @doc Returns a textual, more user-friendly representation of the specified
% vector; full float precision is shown.
%
% This is the recommended representation.
%
-spec to_user_string( vector() ) -> ustring().
to_user_string( Vector ) ->

	Strs = linear:coords_to_best_width_strings( Vector ),

	% No need for ~ts here:
	ElemFormatStr = "[ ~s ]~n",

	FormatStr = "~n" ++ text_utils:duplicate( length( Vector ), ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
	%                       [ FormatStr, Strs ] ),

	text_utils:format( FormatStr, Strs ).
