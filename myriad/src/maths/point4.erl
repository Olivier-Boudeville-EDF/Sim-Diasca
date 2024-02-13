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



% @doc Module implementing the support for <b>4D points</b>.
%
% See also:
% - the corresponding vectors (in vector4.erl) and matrices (in matrix4.erl)
% - the (unspecialised) points of arbitrary dimensions, in point.erl
% - the various 4D services in linear_4D.erl
%
-module(point4).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For the epsilon define:
-include("math_utils.hrl").


-type user_point4() :: any_point4() | [ any_coordinate() ].
% A user-specified point, preferably as a tuple, otherwise as a list (hence as a
% vector), with 4 integer or floating-point coordinates.


-type point4() :: { X :: coordinate(), Y :: coordinate(), Z :: coordinate(),
					W :: coordinate() }.
% A point in a 4D space, with (exactly) 4 floating-point coordinates.


-type integer_point4() :: { X :: integer_coordinate(),
							Y :: integer_coordinate(),
							Z :: integer_coordinate(),
							W :: integer_coordinate() }.
 % A point in a 4D space, with (exactly) 4 integer coordinates.


-type any_point4() :: point4() | integer_point4().
% A point in a 4D space, with any numerical coordinates (4 of them).


-type vertex4() :: point4().
% A 4D (floating-point) vertex of a mesh.

-type integer_vertex4() :: point4().
% A 4D integer vertex of a mesh.


-export_type([ user_point4/0, point4/0, integer_point4/0, any_point4/0,
			   vertex4/0, integer_vertex4/0 ]).


-export([ new/1, new/4, new_integer/4, null/0,
		  from_vector/1, to_vector/1, to_any_vector/1,
		  roundify/1,
		  get_center/2, get_integer_center/2,
		  translate/2, scale/2, vectorize/2,
		  are_close/2, are_equal/2, is_within/3, is_within_square/3,
		  square_distance/2, distance/2,
		  check/1,
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

-type vector4() :: vector4:vector4().
-type any_vector4() :: vector4:any_vector4().



% @doc Returns a 4D point corresponding to the user-specified one (preferably a
% tuple rather than a list).
%
% We do not check whether all coordinates are either integer or floating-point
% ones.
%
-spec new( user_point4() ) -> any_point4().
new( UserPoint4 ) when is_list( UserPoint4 ) ->
	list_to_tuple( UserPoint4 );

new( UserPoint4 ) when is_tuple( UserPoint4 ) ->
	UserPoint4.



% @doc Returns a 4D point corresponding to the user-specified one.
-spec new( user_coordinate(), user_coordinate(), user_coordinate(),
		   user_coordinate() ) -> point4().
new( X, Y, Z, W ) ->
	{ type_utils:ensure_float( X ), type_utils:ensure_float( Y ),
	  type_utils:ensure_float( Z ), type_utils:ensure_float( W ) }.



% @doc Returns an integer 4D point corresponding to the user-specified one.
-spec new_integer( integer_coordinate(), integer_coordinate(),
				   integer_coordinate(), integer_coordinate() ) ->
						 integer_point4().
new_integer( X, Y, Z, W ) when is_integer( X ) andalso is_integer( Y )
							andalso is_integer( Z ) andalso is_integer( W ) ->
	{ X, Y, Z, W }.



% @doc Returns a 4D point whose coordinates are all null, that is the origin of
% the local referential.
%
-spec null() -> point4().
null() ->
	Zero = 0.0,
	{ Zero, Zero, Zero, Zero }.



% @doc Returns a 4D point corresponding to the specified vector (expected to be
% type-homogeneous).
%
-spec from_vector( any_vector4() ) -> any_point4().
from_vector( V4 ) ->
	list_to_tuple( V4 ).



% @doc Returns a 4D vector (with floating-vector coordinates) corresponding to
% the specified 4D point.
%
-spec to_vector( any_point4() ) -> vector4().
to_vector( P4 ) ->
	vector4:from_point( P4 ).


% @doc Returns a 4D vector (with coordinates of the same type) corresponding to
% the specified 4D point.
%
-spec to_any_vector( any_point4() ) -> any_vector4().
to_any_vector( P4 ) ->
	tuple_to_list( P4 ).



% @doc Returns a point whose floating-point coordinates have been rounded to the
% respective nearest integers.
%
-spec roundify( point4() ) -> integer_point4().
roundify( _P={X,Y,Z,W} ) ->
	{ erlang:round(X), erlang:round(Y), erlang:round(Z), erlang:round(W) }.



% @doc Returns a point corresponding the midpoint (middle) of the two specified
% points.
%
-spec get_center( point4(), point4() ) -> point4().
get_center( _P1={X1,Y1,Z1,W1}, _P2={X2,Y2,Z2,W2} ) ->
	{ (X1+X2)/2, (Y1+Y2)/2, (Z1+Z2)/2, (W1+W2)/2 }.



% @doc Returns a point corresponding the middle of the two specified points,
% returned with integer coordinates.
%
-spec get_integer_center( point4(), point4() ) -> integer_point4().
get_integer_center( P1, P2 ) ->
	roundify( get_center( P1, P2 ) ).



% @doc Returns a point corresponding to the specified point translated of the
% specified vector.
%
-spec translate( point4(), vector4() ) -> point4().
translate( _P={X,Y,Z,W}, _V=[Vx,Vy,Vz,Vw] ) ->
	{ X+Vx, Y+Vy, Z+Vz, W+Vw }.



% @doc Scales the specified 4D point of the specified scalar factor.
-spec scale( any_point4(), factor() ) -> point4().
scale( _P={X,Y,Z,W}, Factor ) ->
	{ Factor*X, Factor*Y, Factor*Z, Factor*W }.



% @doc Returns a vector V made from the specified two points: V=P2-P1.
-spec vectorize( point4(), point4() ) -> vector4().
vectorize( _P1={X1,Y1,Z1,W1}, _P2={X2,Y2,Z2,W2} ) ->
	[ X2-X1, Y2-Y1, Z2-Z1, W2-W1 ].



% @doc Returns whether the two specified 4D points are close, that is if they
% could be considered as representing the same point (equality operator on
% points).
%
-spec are_close( point4(), point4() ) -> boolean().
are_close( P1, P2 ) ->
	are_equal( P1, P2 ).


% @doc Returns whether the two specified 3D points are equal, that is if they
% could be considered as representing the same point (equality operator on
% points).
%
-spec are_equal( point4(), point4() ) -> boolean().
are_equal( _P1={X1,Y1,Z1,W1}, _P2={X2,Y2,Z2,W2} ) ->
	math_utils:are_close( X1, X2 ) andalso math_utils:are_close( Y1, Y2 )
		andalso math_utils:are_close( Z1, Z2 )
		andalso math_utils:are_close( W1, W2 ).



% @doc Tells whether the specified 4D point P1 is within a distance D from 4D
% point P2, using some margin to overcome numerical errors.
%
-spec is_within( point4(), point4(), distance() ) -> boolean().
is_within( P1, P2, D ) ->
	% "Taylor series", square(epsilon) is negligible here:
	square_distance( P1, P2 ) < D * ( D + ?epsilon ).



% @doc Tells whether the specified 4D point P1 is within a square distance
% SquareD from 4D point P2.
%
-spec is_within_square( point4(), point4(), square_distance() ) -> boolean().
is_within_square( P1, P2, SquareD ) ->
	square_distance( P1, P2 ) < SquareD.



% @doc Returns the square of the distance between the two specified 4D points.
%
% For comparison purposes, computing the square root is useless.
%
% Could rely on vectorize and square_magnitude as well.
%
-spec square_distance( point4(), point4() ) -> square_distance().
square_distance( _P1={X1,Y1,Z1,W1}, _P2={X2,Y2,Z2,W2} ) ->

	XDiff = X2 - X1,
	YDiff = Y2 - Y1,
	ZDiff = Z2 - Z1,
	WDiff = W2 - W1,

	XDiff*XDiff + YDiff*YDiff + ZDiff*ZDiff + WDiff*WDiff.



% @doc Returns the distance between the two specified 4D points.
%
% Note: just for comparison purposes, computing the square root is useless.
%
-spec distance( point4(), point4() ) -> distance().
distance( P1, P2 ) ->
	math:sqrt( square_distance( P1, P2 ) ).



% @doc Checks that the specified 4D point is legit, and returns it.
-spec check( point4() ) -> point4().
check( P ) ->
	point:check( P ).



% @doc Returns a textual representation of the specified 4D point; full float
% precision is shown.
%
-spec to_string( any_point4() ) -> ustring().
to_string( Point4 ) ->
	to_user_string( Point4 ).



% @doc Returns a compact, textual, informal representation of the specified
% 4D point.
%
-spec to_compact_string( any_point4() ) -> ustring().
to_compact_string( Point4 ) ->
	text_utils:format( "~w", [ Point4 ] ).



% @doc Returns a basic, not even fixed-width for floating-point coordinates (see
% linear.hrl for width and precision) representation of the specified 4D point.
%
-spec to_basic_string( any_point4() ) -> ustring().
to_basic_string( Point4 ) ->

	CoordList = tuple_to_list( Point4 ),

	% Points supposed to be homogeneous tuples:
	CoordFmt = case is_integer( hd( CoordList ) ) of

		true ->
			?coord_integer_format;

		false ->
			?coord_float_format

	end,

	ElemFormatStr = "{ " ++ CoordFmt ++ " }~n",

	FormatStr = "~n" ++ text_utils:duplicate( 4, ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; CoordList: ~w.",
	%                       [ FormatStr, CoordList ] ),

	text_utils:format( FormatStr, CoordList ).



% @doc Returns a textual, more user-friendly representation of the specified
% 4D point; full float precision is shown.
%
% This is the recommended representation.
%
-spec to_user_string( any_point4() ) -> ustring().
to_user_string( Point4 ) ->

	CoordList = tuple_to_list( Point4 ),

	Strs = linear:coords_to_best_width_strings( CoordList ),

	% No need for ~ts here:
	ElemFormatStr = "{ ~s }~n",

	FormatStr = "~n" ++ text_utils:duplicate( 4, ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
	%                       [ FormatStr, Strs ] ),

	text_utils:format( FormatStr, Strs ).
