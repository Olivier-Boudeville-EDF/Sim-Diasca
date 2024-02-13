% Copyright (C) 2021-2024 Olivier Boudeville
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
% Creation date: Saturday, October 9, 2021.


% @doc Module implementing the support for <b>2D vectors</b>.
%
% See also:
% - the corresponding points (in point2.erl) and matrices (in matrix2.erl)
% - the (unspecialised) vectors of arbitrary dimensions, in vector.erl
% - the various 2D extra services in linear_2D.erl
%
-module(vector2).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For the epsilon define:
-include("math_utils.hrl").


-type user_vector2() :: [ any_coordinate() ].
% A user-specified 2D vector, as a list (hence not a tuple) with 2 integer or
% floating-point coordinates.


-type vector2() :: [ coordinate() ].
% An (internal) 2D vector, with (exactly) 2 floating-point coordinates.
% They are typically referenced as [X, Y, Z].


-type integer_vector2() :: [ integer_coordinate() ].
% A 2D vector, with (exactly) 2 integer coordinates.
% They are typically referenced as [X, Y, Z].


-type any_vector2() :: vector2() | integer_vector2().
% A 2D vector, with any types of numerical coordinates.
% They are typically referenced as [X, Y, Z].


-type unit_vector2() :: vector2().
% A unit 2D vector, that is a vector of magnitude 1.0.
%
% Defined for documentation purpose.


-type normal2() :: vector2().
% A 2D vector orthogonal to a plane.
%
% Defined for documentation purpose.


-type unit_normal2() :: unit_vector2().
% A unit 2D vector orthogonal to a plane.
%
% Defined for documentation purpose.


-type texture_vector2() :: vector2().
% A 2D vector containing texture coordinates.


-export_type([ user_vector2/0, vector2/0, integer_vector2/0, any_vector2/0,
			   unit_vector2/0, normal2/0, unit_normal2/0, texture_vector2/0 ]).


-export([ new/1, new/2, new_integer/2, null/0,
		  x_axis/0, y_axis/0,
		  from_point/1, to_point/1,
		  add/2, add/1, cross_product/2,
		  are_close/2, are_equal/2,
		  square_magnitude/1, magnitude/1, negate/1, scale/2, normalise/1,
		  normal_left/1, normal_right/1,
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
-type any_square_distance() :: linear:any_square_distance().

-type any_point2() :: point2:any_point2().



% @doc Returns a 2D vector corresponding to the user-specified one.
-spec new( user_vector2() ) -> vector2().
%new( UserVector ) when is_tuple( UserVector ) ->
%   new( tuple_to_list( UserVector ) );
% Throws bad_generator anyway if a tuple:
new( _UserVector=[ X, Y ] ) ->
	[ type_utils:ensure_float( X ), type_utils:ensure_float( Y ) ].



% @doc Returns a 2D vector corresponding to the user-specified one.
-spec new( user_coordinate(), user_coordinate() ) -> vector2().
new( X, Y ) ->
	[ type_utils:ensure_float( X ), type_utils:ensure_float( Y ) ].



% @doc Returns an integer 2D vector corresponding to the user-specified one.
-spec new_integer( integer_coordinate(), integer_coordinate() ) ->
							integer_vector2().
new_integer( X, Y ) when is_integer( X ) andalso is_integer( Y ) ->
	[ X, Y ].



% @doc Returns the null (2D) vector, that is a 2D vector whose coordinates are
% all null.
%
-spec null() -> vector2().
null() ->
	Zero = 0.0,
	[ Zero, Zero ].




% @doc Returns a 2D vector corresponding to the X axis of the current
% referential.
%
-spec x_axis() -> vector2().
x_axis() ->
	[ 1.0, 0.0 ].


% @doc Returns a 2D vector corresponding to the Y axis of the current
% referential.
%
-spec y_axis() -> vector2().
y_axis() ->
	[  0.0, 1.0 ].



% @doc Returns a 2D vector corresponding to the specified 2D point.
-spec from_point( any_point2() ) -> vector2().
from_point( _P={ X, Y } ) ->
	[ type_utils:ensure_float( X ), type_utils:ensure_float( Y ) ].


% @doc Returns a 2D point corresponding to the specified 2D vector.
-spec to_point( any_vector2() ) -> any_point2().
to_point( V2 ) ->
	point2:from_vector( V2 ).



% @doc Returns the sum of the two specified 2D vectors: V = V1 + V2.
-spec add( vector2(), vector2() ) -> vector2().
add( _V1=[ X1, Y1 ], _V2=[ X2, Y2 ] ) ->
	[ X1+X2, Y1+Y2 ].


% @doc Returns the sum of all 2D vectors in the specified (supposedly non-empty)
% list.
%
-spec add( [ vector2() ] ) -> vector2().
add( _Vectors=[ VFirst | VOthers ]  ) ->
	lists:foldl( fun( [ X, Y ], _AccVec=[ Xa, Ya ] ) ->
					[ X+Xa, Y+Ya ]
				 end,
				 _InitialAcc=VFirst,
				 _List=VOthers ).



% @doc Returns the cross-product of the two specified 2D points, that is the
% square magnitude of the vector that would result from a regular 3D cross
% product of the input vectors, taking their Z values implicitly as 0.
%
-spec cross_product( vector2(), vector2() ) -> any_square_distance().
cross_product( [X1,Y1], [X2,Y2] ) ->
	abs( X1*Y2 - Y1*X2 ).



% @doc Returns whether the two specified 2D vectors are close, that is if they
% could be considered as representing the same vector (equality operator on
% vectors).
%
-spec are_close( vector2(), vector2() ) -> boolean().
are_close( V1, V2 ) ->
	are_equal( V1, V2 ).


% @doc Returns whether the two specified 2D vectors are equal, that is if they
% could be considered as representing the same vector (equality operator on
% vectors).
%
-spec are_equal( vector2(), vector2() ) -> boolean().
are_equal( _V1=[X1,Y1], _V2=[X2,Y2] ) ->
	math_utils:are_close( X1, X2 ) andalso math_utils:are_close( Y1, Y2 ).



% @doc Returns the square of the magnitude of the 2D specified vector.
-spec square_magnitude( any_vector2() ) -> any_square_distance().
square_magnitude( _V=[X,Y] ) ->
	X*X + Y*Y.


% @doc Returns the magnitude of the specified 2D vector.
-spec magnitude( any_vector2() ) -> distance().
magnitude( V ) ->
	math:sqrt( square_magnitude( V ) ).



% @doc Negates the specified vector: returns the opposite one (of the same
% magnitude).
%
-spec negate( vector2() ) -> vector2().
negate( _V=[X,Y] ) ->
	[ -X, -Y ].



% @doc Scales the specified 2D vector of the specified scalar factor.
-spec scale( any_vector2(), factor() ) -> vector2().
scale( _V=[X,Y], Factor ) ->
	[ Factor*X, Factor*Y ].



% @doc Normalises the specified non-null 2D vector, that is returns it once
% scaled to an unit length (whose magnitude is thus 1.0).
%
-spec normalise( vector2() ) -> unit_vector2().
normalise( V ) ->
	case magnitude( V ) of

		M when M < ?epsilon ->
			throw( cannot_normalise_null_vector );

		M ->
			scale( V, 1.0 / M )

	end.



% @doc Returns a (non-unit) vector that is normal to the specified vector V, and
% is on the left of V in the standard basis.
%
-spec normal_left( vector2() ) -> vector2();
				 ( integer_vector2() ) -> integer_vector2().
normal_left( _V=[X,Y] ) ->
	[ -Y, X ].


% @doc Returns a (non-unit) vector that is normal to the specified vector V, and
% is on the right of V in the standard basis.
%
-spec normal_right( vector2() ) -> vector2();
				  ( integer_vector2() ) -> integer_vector2().
normal_right( _V=[X,Y] ) ->
	[ Y, -X ].



% @doc Returns the dot-product of the two specified 2D vectors: D = V1.V2.
-spec dot_product( vector2(), vector2() ) -> float();
				 ( integer_vector2(), integer_vector2() ) -> integer().
 dot_product( _V1=[ X1, Y1 ], _V2=[ X2, Y2 ] ) ->
	X1*X2 + Y1*Y2.



% @doc Returns whether the specified vector is unitary, that is whether it is of
% magnitude 1.0.
%
-spec is_unitary( vector2() ) -> boolean().
is_unitary( V ) ->
	% No specific need of computing the square root thereof:
	math_utils:are_equal( 1.0, square_magnitude( V ) ).



% @doc Checks that the specified 2D vector is legit, and returns it.
-spec check( vector2() ) -> vector2().
check( V=[_X,_Y] ) ->
	type_utils:check_floats( V ).


% @doc Checks that the specified 2D integer vector is legit, and returns it.
-spec check_integer( integer_vector2() ) -> integer_vector2().
check_integer( V=[_X,_Y] ) ->
	type_utils:check_integers( V ).



% @doc Checks that the specified 2D vector is normalised, and returns it.
-spec check_unit_vector( vector2() ) -> unit_vector2().
check_unit_vector( V ) ->
	true = is_unitary( V ),
	V.


% @doc Checks that the specified 2D vectors are normalised, and returns them.
-spec check_unit_vectors( [ vector2() ] ) -> [ unit_vector2() ].
check_unit_vectors( Vs ) ->
	[ true = is_unitary( V ) || V <- Vs ],
	Vs.



% @doc Returns a textual representation of the specified 2D vector; full float
% precision is shown.
%
-spec to_string( vector2() ) -> ustring().
to_string( Vector ) ->
	to_user_string( Vector ).



% @doc Returns a compact, textual, informal representation of the specified 2D
% vector.
%
-spec to_compact_string( vector2() ) -> ustring().
to_compact_string( Vector ) ->

	%Ws = [ "~w" || _ <- Vector ],
	%FormatStr = "[ " ++ text_utils:join( _Sep=", ", Ws ) ++ " ]",
	%text_utils:format( FormatStr, Vector ).

	text_utils:format( "~w", [ Vector ] ).



% @doc Returns a basic, not even fixed-width for floating-vector coordinates
% (see linear.hrl for width and precision) representation of the specified 2D
% vector.
%
-spec to_basic_string( vector2() ) -> ustring().
to_basic_string( Vector ) ->

	% Vectors supposed to be lists of floats:
	ElemFormatStr = "[ " ++ ?coord_float_format ++ " ]~n",

	FormatStr = "~n" ++ ElemFormatStr ++ ElemFormatStr,

	%trace_utils:debug_fmt( "FormatStr: ~ts; CoordList: ~w.",
	%                       [ FormatStr, CoordList ] ),

	text_utils:format( FormatStr, Vector ).



% @doc Returns a textual, more user-friendly representation of the specified 2D
% vector; full float precision is shown.
%
% This is the recommended representation.
%
-spec to_user_string( vector2() ) -> ustring().
to_user_string( Vector ) ->

	Strs = linear:coords_to_best_width_strings( Vector ),

	% No need for ~ts here:
	ElemFormatStr = "[ ~s ]~n",

	FormatStr = "~n" ++ ElemFormatStr ++ ElemFormatStr,

	%trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
	%                       [ FormatStr, Strs ] ),

	text_utils:format( FormatStr, Strs ).
