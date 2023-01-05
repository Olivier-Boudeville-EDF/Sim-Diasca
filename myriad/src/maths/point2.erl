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
% Creation date: Saturday, October 9, 2021.


% @doc Module implementing the support for <b>2D points</b>.
%
% See also:
% - the corresponding vectors (in vector2.erl) and matrices (in matrix3.erl)
% - the (unspecialised) points of arbitrary dimensions, in point.erl
% - the various 2D services in linear_2D.erl
%
-module(point2).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For the epsilon define:
-include("math_utils.hrl").


-type user_point2() :: any_point2()
					 | [ coordinate() ] | [ integer_coordinate() ].
% A user-specified point, preferably as a tuple, otherwise as a list (hence as a
% vector), with two integer or floating-point coordinates.


-type point2() :: { X :: coordinate(), Y :: coordinate() }.
% A point in a 2D space, with (exactly) 2 floating-point coordinates.


-type integer_point2() :: { X :: integer_coordinate(),
							Y :: integer_coordinate() }.
 % A point in a 2D space, with (exactly) 2 integer coordinates.


-type any_point2() :: point2() | integer_point2().
% A point in a 2D space, with any numerical coordinates (2 of them).


-type vertex2() :: point2().
% A 2D (floating-point) vertex of a polygon.

-type integer_vertex2() :: point2().
% A 2D integer vertex of a polygon.

-type any_vertex2() :: any_point2().
% A 2D vertex of a polygon, with any numerical coordinates (2 of them).


-export_type([ user_point2/0, point2/0, integer_point2/0, any_point2/0,
			   vertex2/0, integer_vertex2/0, any_vertex2/0 ]).


-export([ new/1, new/2, new_integer/2, null/0,
		  from_vector/1, to_vector/1, to_any_vector/1,
		  roundify/1,
		  get_center/2, get_integer_center/2,
		  translate/2, scale/2, vectorize/2,
		  are_close/2, are_equal/2, is_within/3, is_within_square/3,
		  square_distance/2, distance/2,
		  draw_integer_random/2, draw_integer_random/3,
		  check/1,
		  to_string/1, to_compact_string/1, to_basic_string/1,
		  to_user_string/1, to_string/2 ] ).



% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type factor() :: math_utils:factor().

-type coordinate() :: linear:coordinate().
-type integer_coordinate() :: linear:integer_coordinate().
-type user_coordinate() :: linear:user_coordinate().

-type distance() :: linear:distance().
-type square_distance() :: linear:square_distance().

-type vector2() :: vector2:vector2().
-type integer_vector2() :: vector2:integer_vector2().
-type any_vector2() :: vector2:any_vector2().



% @doc Returns a 2D point corresponding to the user-specified one (preferably a
% tuple rather than a list).
%
% We do not check whether all coordinates are either integer or floating-point
% ones.
%
-spec new( user_point2() ) -> any_point2().
new( [ P1, P2 ] ) ->
	{ P1, P2 };

new( UserPoint2 ) when is_tuple( UserPoint2 ) ->
	UserPoint2.



% @doc Returns a 2D point corresponding to the user-specified one.
-spec new( user_coordinate(), user_coordinate() ) -> point2().
new( X, Y ) ->
	{ type_utils:ensure_float( X ), type_utils:ensure_float( Y ) }.



% @doc Returns an integer 2D point corresponding to the user-specified one.
-spec new_integer( integer_coordinate(), integer_coordinate() ) ->
			integer_point2().
new_integer( X, Y ) when is_integer( X ) andalso is_integer( Y ) ->
	{ X, Y }.



% @doc Returns a 2D point whose coordinates are all null, that is the origin of
% the local referential.
%
-spec null() -> point2().
null() ->
	Zero = 0.0,
	{ Zero, Zero }.



% @doc Returns a 2D point corresponding to the specified vector (expected to be
% type-homogeneous).
%
-spec from_vector( any_vector2() ) -> any_point2().
from_vector( [ X, Y ] ) ->
	{ X, Y }.



% @doc Returns a 2D vector (with floating-vector coordinates) corresponding to
% the specified 2D point.
%
-spec to_vector( any_point2() ) -> vector2().
to_vector( { X, Y } ) ->
	[ type_utils:ensure_float( X ), type_utils:ensure_float( Y ) ].


% @doc Returns a 2D vector (with coordinates of the same type) corresponding to
% the specified 2D point.
%
-spec to_any_vector( any_point2() ) -> any_vector2().
to_any_vector( { X, Y } ) ->
	[ X, Y ].



% @doc Returns a point whose floating-point coordinates have been rounded to the
% respective nearest integers.
%
-spec roundify( point2() ) -> integer_point2().
roundify( _P={X,Y} ) ->
	{ erlang:round(X), erlang:round(Y) }.



% @doc Returns a point corresponding the midpoint (middle) of the two specified
% points.
%
-spec get_center( any_point2(), any_point2() ) -> point2().
get_center( _P1={X1,Y1}, _P2={X2,Y2} ) ->
	{ (X1+X2)/2, (Y1+Y2)/2 }.



% @doc Returns a point corresponding the middle of the two specified points,
% returned with integer coordinates.
%
-spec get_integer_center( point2(), point2() ) -> integer_point2().
get_integer_center( P1, P2 ) ->
	roundify( get_center( P1, P2 ) ).



% @doc Returns a point corresponding to the specified point translated of the
% specified vector.
%
-spec translate( any_point2(), any_vector2() ) -> any_point2().
translate( _P={X,Y}, _V=[Vx,Vy] ) ->
	{ X+Vx, Y+Vy }.



% @doc Scales the specified 2D point of the specified scalar factor.
-spec scale( any_point2(), factor() ) -> point2().
scale( _P={X,Y}, Factor ) ->
	{ Factor*X, Factor*Y }.



% @doc Returns a vector V made from the specified two points P1 and P2: V=P2-P1.
-spec vectorize( point2(), point2() ) -> vector2();
			   ( integer_point2(), integer_point2() ) -> integer_vector2().
vectorize( _P1={X1,Y1}, _P2={X2,Y2} ) ->
	[ X2-X1, Y2-Y1 ].



% @doc Returns whether the two specified 2D points are close, that is if they
% could be considered as representing the same point (equality operator on
% points).
%
-spec are_close( point2(), point2() ) -> boolean().
are_close( P1, P2 ) ->
	are_equal( P1, P2 ).


% @doc Returns whether the two specified 2D points are equal, that is if they
% could be considered as representing the same point (equality operator on
% points).
%
-spec are_equal( point2(), point2() ) -> boolean().
are_equal( _P1={X1,Y1}, _P2={X2,Y2} ) ->
	math_utils:are_close( X1, X2 ) andalso math_utils:are_close( Y1, Y2 ).



% @doc Tells whether the specified 2D point P1 is within a distance D from 2D
% point P2, using some margin to overcome numerical errors.
%
-spec is_within( point2(), point2(), distance() ) -> boolean().
is_within( P1, P2, D ) ->
	% "Taylor series", square(epsilon) is negligible here:
	square_distance( P1, P2 ) < D * ( D + ?epsilon ).



% @doc Tells whether the specified 2D point P1 is within a square distance
% SquareD from 2D point P2.
%
-spec is_within_square( point2(), point2(), square_distance() ) -> boolean().
is_within_square( P1, P2, SquareD ) ->
	square_distance( P1, P2 ) < SquareD.



% @doc Returns the square of the distance between the two specified 2D points.
%
% For comparison purposes, computing the square root is useless.
%
% Could rely on vectorize and square_magnitude as well.
%
-spec square_distance( point2(), point2() ) -> square_distance().
square_distance( _P1={X1,Y1}, _P2={X2,Y2} ) ->

	XDiff = X2 - X1,
	YDiff = Y2 - Y1,

	XDiff*XDiff + YDiff*YDiff.



% @doc Returns the distance between the two specified 2D points.
%
% Note: just for comparison purposes, computing the square root is useless.
%
-spec distance( point2(), point2() ) -> distance().
distance( P1, P2 ) ->
	math:sqrt( square_distance( P1, P2 ) ).



% @doc Draws a random point between specified (included) bounds (for both
% dimensions).
%
-spec draw_integer_random( integer_coordinate(), integer_coordinate() ) ->
											integer_point2().
draw_integer_random( Min, Max ) ->
	[ X, Y ] = random_utils:get_uniform_values( Min, Max, _Count=2 ),
	{ X, Y }.


% @doc Draws the specified number of random points between specified (included)
% bounds (for both dimensions).
%
-spec draw_integer_random( integer_coordinate(), integer_coordinate(),
						   count() ) -> [ integer_point2() ].
draw_integer_random( Min, Max, Count ) ->
	Coords = random_utils:get_uniform_values( Min, Max, 2*Count ),
	gather_as_points( Coords, _Acc=[] ).


% (helper)
gather_as_points( _Coords=[], Acc ) ->
	Acc;

gather_as_points( _Coords=[ X, Y | T ], Acc ) ->
	gather_as_points( T, [ { X, Y } | Acc ] ).



% @doc Checks that the specified 2D point is legit, and returns it.
-spec check( point2() ) -> point2().
check( P ) ->
	point:check( P ).



% @doc Returns a textual representation of the specified 2D point; full float
% precision is shown.
%
-spec to_string( any_point2() ) -> ustring().
to_string( Point2 ) ->
	to_user_string( Point2 ).



% @doc Returns a compact, textual, informal representation of the specified
% 2D point.
%
-spec to_compact_string( any_point2() ) -> ustring().
to_compact_string( Point2 ) ->
	text_utils:format( "~w", [ Point2 ] ).



% @doc Returns a basic, not even fixed-width for floating-point coordinates (see
% linear.hrl for width and precision) representation of the specified 2D point.
%
-spec to_basic_string( any_point2() ) -> ustring().
to_basic_string( _Point={ X, Y } ) ->

	% Points supposed to be homogeneous tuples:
	CoordFmt = case is_integer( X ) of

		true ->
			?coord_integer_format;

		false ->
			?coord_float_format

	end,

	ElemFormatStr = "{ " ++ CoordFmt ++ " }~n",

	FormatStr = "~n" ++ ElemFormatStr ++ ElemFormatStr,

	CoordList = [ X, Y ],

	%trace_utils:debug_fmt( "FormatStr: ~ts; CoordList: ~w.",
	%                       [ FormatStr, CoordList ] ),

	text_utils:format( FormatStr, CoordList ).



% @doc Returns a textual, more user-friendly representation of the specified
% 2D point; full float precision is shown.
%
% This is the recommended representation.
%
-spec to_user_string( any_point2() ) -> ustring().
to_user_string( _Point={ X, Y } ) ->

	Strs = linear:coords_to_best_width_strings( [ X, Y ] ),

	% No need for ~ts here:
	ElemFormatStr = "{ ~s }~n",

	FormatStr = "~n" ++ ElemFormatStr ++ ElemFormatStr,

	%trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
	%                       [ FormatStr, Strs ] ),

	text_utils:format( FormatStr, Strs ).




% @doc Returns a human-friendly, approximated textual representation of
% specified point, based on specified print-out precision (number of digits
% after the comma).
%
-spec to_string( point2(), basic_utils:count() ) -> ustring().
to_string( { X, Y }, DigitCountAfterComma ) ->

	% We want to avoid displaying larger texts for coordinates, like
	% 0.10000000000000009:

	%XRounded = math_utils:round_after( X, DigitCountAfterComma ),

	%YRounded = math_utils:round_after( Y, DigitCountAfterComma ),

	%text_utils:format( "{ ~.*w, ~.*w }", [ Precision, X, Precision, Y ] ).

	XString = case is_float( X ) of

		true ->
			text_utils:format( "~.*f", [ DigitCountAfterComma, X ] );

		false ->
			% Integer:
			text_utils:format( "~B", [ X ] )

	end,

	YString = case is_float( Y ) of

		true ->
			text_utils:format( "~.*f", [ DigitCountAfterComma, Y ] );

		false ->
			% Integer:
			text_utils:format( "~B", [ Y ] )

	end,

	text_utils:format( "~n{ ~ts, ~ts }", [ XString, YString ] ).
