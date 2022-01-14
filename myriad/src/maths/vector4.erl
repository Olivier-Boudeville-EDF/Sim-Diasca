% Copyright (C) 2021-2022 Olivier Boudeville
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
% Creation date: Sunday, October 10, 2021.



% @doc Module implementing the support for <b>4D vectors</b>.
%
% See also:
% - the corresponding points (in point4.erl) and matrices (in matrix4.erl)
% - the (unspecialised) vectors of arbitrary dimensions, in vector.erl
% - the various 4D extra services in linear_4D.erl
%
-module(vector4).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For the epsilon define:
-include("math_utils.hrl").


-type user_vector4() :: [ any_coordinate() ].
% A user-specified 4D vector, as a list (hence not a tuple) with 4 integer or
% floating-point coordinates.


-type vector4() :: [ coordinate() ].
% An (internal) 4D vector, with (exactly) 4 floating-point coordinates.
% They are typically referenced as [X, Y, Z, W].


-type integer_vector4() :: [ integer_coordinate() ].
% A 4D vector, with (exactly) 4 integer coordinates.
% They are typically referenced as [X, Y, Z, W].


-type any_vector4() :: vector4() | integer_vector4().
% A 4D vector, with any types of numerical coordinates.
% They are typically referenced as [X, Y, Z, W].


-type unit_vector4() :: vector4().
% A unit 4D vector, that is a vector of magnitude 1.0.
%
% Defined for documentation purpose.


-type normal4() :: vector4().
% A 4D vector orthogonal to a plane.
%
% Defined for documentation purpose.


-type unit_normal4() :: unit_vector4().
% A unit 4D vector orthogonal to a plane.
%
% Defined for documentation purpose.


-export_type([ user_vector4/0, vector4/0, integer_vector4/0, any_vector4/0,
			   unit_vector4/0, normal4/0, unit_normal4/0 ]).


-export([ new/1, new/4, new_integer/4, null/0,
		  from_point/1, to_point/1,
		  add/2, add/1,
		  are_close/2, are_equal/2,
		  square_magnitude/1, magnitude/1, negate/1, scale/2, normalise/1,
		  dot_product/2,
		  is_unitary/1,
		  check/1, check_integer/1, check_unit_vector/1, check_unit_vectors/1,
		  to_string/1, to_compact_string/1, to_basic_string/1,
		  to_user_string/1 ] ).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type factor() :: math_utils:factor().

-type coordinate() :: linear:coordinate().
-type integer_coordinate() :: linear:integer_coordinate().
-type any_coordinate() :: linear:any_coordinate().
-type user_coordinate() :: linear:user_coordinate().

-type distance() :: linear:distance().
-type square_distance() :: linear:square_distance().

-type point4() :: point4().
-type any_point4() :: any_point4().



% @doc Returns a 4D vector corresponding to the user-specified one.
-spec new( user_vector4() ) -> vector4().
%new( UserVector ) when is_tuple( UserVector ) ->
%	new( tuple_to_list( UserVector ) );
% Throws bad_generator anyway if a tuple:
new( UserVector ) -> %when is_list( UserVector ) ->
	[ type_utils:ensure_float( UC ) || UC <- UserVector ].



% @doc Returns a 4D vector corresponding to the user-specified one.
-spec new( user_coordinate(), user_coordinate(), user_coordinate(),
		   user_coordinate() ) -> vector4().
new( X, Y, Z, W ) ->
	[ type_utils:ensure_float( X ), type_utils:ensure_float( Y ),
	  type_utils:ensure_float( Z ), type_utils:ensure_float( W ) ].



% @doc Returns an integer 4D vector corresponding to the user-specified one.
-spec new_integer( integer_coordinate(), integer_coordinate(),
			integer_coordinate(), integer_coordinate() ) -> integer_vector4().
new_integer( X, Y, Z, W ) when is_integer( X ) andalso is_integer( Y )
							andalso is_integer( Z ) andalso is_integer( W ) ->
	[ X, Y, Z, W ].



% @doc Returns the null (4D) vector, that is a 4D vector whose coordinates are
% all null.
%
-spec null() -> vector4().
null() ->
	Zero = 0.0,
	[ Zero, Zero, Zero, Zero ].



% @doc Returns a 4D vector corresponding to the specified 4D point.
-spec from_point( any_point4() ) -> vector4().
from_point( P4 ) ->
	[ type_utils:ensure_float( C ) || C <- tuple_to_list( P4 ) ].


% @doc Returns a 4D point corresponding to the specified 4D vector.
-spec to_point( any_vector4() ) -> any_point4().
to_point( V4 ) ->
	point4:from_vector( V4 ).



% @doc Returns the sum of the two specified 4D vectors: V = V1 + V2.
-spec add( vector4(), vector4() ) -> vector4().
add( _V1=[ X1, Y1, Z1, W1 ], _V2=[ X2, Y2, Z2, W2 ] ) ->
	[ X1+X2, Y1+Y2, Z1+Z2, W1+W2 ].


% @doc Returns the sum of all 4D vectors in the specified (supposedly non-empty)
% list.
%
-spec add( [ vector4() ] ) -> vector4().
add( _Vectors=[ VFirst | VOthers ]  ) ->
	lists:foldl( fun( [ X, Y, Z, W ], _AccVec=[ Xa, Ya, Za, Wa ] ) ->
					[ X+Xa, Y+Ya, Z+Za, W+Wa ]
				 end,
				 _InitialAcc=VFirst,
				 _List=VOthers ).



% @doc Returns whether the two specified 4D vectors are close, that is if they
% could be considered as representing the same vector (equality operator on
% vectors).
%
-spec are_close( vector4(), vector4() ) -> boolean().
are_close( V1, V2 ) ->
	are_equal( V1, V2 ).


% @doc Returns whether the two specified 4D vectors are equal, that is if they
% could be considered as representing the same vector (equality operator on
% vectors).
%
-spec are_equal( vector4(), vector4() ) -> boolean().
are_equal( _V1=[X1,Y1,Z1,W1], _V2=[X2,Y2,Z2,W2] ) ->
	math_utils:are_close( X1, X2 ) andalso math_utils:are_close( Y1, Y2 )
		andalso math_utils:are_close( Z1, Z2 )
		andalso math_utils:are_close( W1, W2 ).



% No cross_product/2 for dimensions different from 3.


% @doc Returns the square of the magnitude of the specified 4D vector.
-spec square_magnitude( vector4() ) -> square_distance().
square_magnitude( _V=[X,Y,Z,W] ) ->
	X*X + Y*Y + Z*Z + W*W.


% @doc Returns the magnitude of the specified 4D vector.
-spec magnitude( vector4() ) -> distance().
magnitude( V ) ->
	math:sqrt( square_magnitude( V ) ).



% @doc Negates the specified vector: returns the opposite one (of the same
% magnitude).
%
-spec negate( vector4() ) -> vector4().
negate( _V=[X,Y,Z,W] ) ->
	[ -X, -Y, -Z, -W ].



% @doc Scales the specified 4D vector of the specified scalar factor.
-spec scale( vector4(), factor() ) -> vector4().
scale( _V=[X,Y,Z,W], Factor ) ->
	[ Factor*X, Factor*Y, Factor*Z, Factor*W ].



% @doc Normalises the specified non-null 4D vector, that is returns it once
% scaled to an unit length (whose magnitude is thus 1.0).
%
-spec normalise( vector4() ) -> unit_vector4().
normalise( V ) ->
	case magnitude( V ) of

		M when M < ?epsilon ->
			throw( cannot_normalise_null_vector );

		M ->
			scale( V, 1.0 / M )

	end.



% @doc Returns the dot-product of the two specified 4D vectors: D = V1.V2.
-spec dot_product( vector4(), vector4() ) -> float().
dot_product( _V1=[ X1, Y1, Z1, W1 ], _V2=[ X2, Y2, Z2, W2 ] ) ->
	X1*X2 + Y1*Y2 + Z1*Z2 + W1*W2.



% @doc Returns whether the specified vector is unitary, that is whether it is of
% magnitude 1.0.
%
-spec is_unitary( vector4() ) -> boolean().
is_unitary( V ) ->
	% No specific need of computing the square root thereof:
	math_utils:are_equal( 1.0, square_magnitude( V ) ).



% @doc Checks that the specified 4D vector is legit, and returns it.
-spec check( vector4() ) -> vector4().
check( V ) ->
	4 = length( V ),
	type_utils:check_floats( V ).


% @doc Checks that the specified 4D integer vector is legit, and returns it.
-spec check_integer( integer_vector4() ) -> integer_vector4().
check_integer( V ) ->
	4 = length( V ),
	type_utils:check_integers( V ).



% @doc Checks that the specified 4D vector is normalised, and returns it.
-spec check_unit_vector( vector4() ) -> unit_vector4().
check_unit_vector( V ) ->
	true = is_unitary( V ),
	V.


% @doc Checks that the specified 4D vectors are normalised, and returns them.
-spec check_unit_vectors( vector4() ) -> unit_vector4().
check_unit_vectors( Vs ) ->
	[ true = is_unitary( V ) || V <- Vs ],
	Vs.



% @doc Returns a textual representation of the specified 4D vector; full float
% precision is shown.
%
-spec to_string( vector4() ) -> ustring().
to_string( Vector ) ->
	to_user_string( Vector ).



% @doc Returns a compact, textual, informal representation of the specified 4D
% vector.
%
-spec to_compact_string( vector4() ) -> ustring().
to_compact_string( Vector ) ->

	%Ws = [ "~w" || _ <- Vector ],
	%FormatStr = "[ " ++ text_utils:join( _Sep=", ", Ws ) ++ " ]",
	%text_utils:format( FormatStr, Vector ).

	text_utils:format( "~w", [ Vector ] ).



% @doc Returns a basic, not even fixed-width for floating-vector coordinates
% (see linear.hrl for width and precision) representation of the specified 4D
% vector.
%
-spec to_basic_string( vector4() ) -> ustring().
to_basic_string( Vector ) ->

	% Vectors supposed to be lists of floats:
	ElemFormatStr = "[ " ++ ?coord_float_format ++ " ]~n",

	FormatStr = "~n" ++ text_utils:duplicate( 4, ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; CoordList: ~w.",
	%                       [ FormatStr, CoordList ] ),

	text_utils:format( FormatStr, Vector ).



% @doc Returns a textual, more user-friendly representation of the specified 4D
% vector; full float precision is shown.
%
% This is the recommended representation.
%
-spec to_user_string( vector4() ) -> ustring().
to_user_string( Vector ) ->

	Strs = linear:coords_to_best_width_strings( Vector ),

	% No need for ~ts here:
	ElemFormatStr = "[ ~s ]~n",

	FormatStr = "~n" ++ text_utils:duplicate( 4, ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
	%                       [ FormatStr, Strs ] ),

	text_utils:format( FormatStr, Strs ).
