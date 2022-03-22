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
% Creation date: Saturday, October 9, 2021.


% @doc Module implementing the support for <b>3D points</b>.
%
% See also:
% - the corresponding vectors (in vector3.erl) and matrices (in matrix3.erl)
% - the (unspecialised) points of arbitrary dimensions, in point.erl
% - the various 3D services in linear_3D.erl
%
-module(point3).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For the epsilon define:
-include("math_utils.hrl").


-type user_point3() :: any_point3() | [ any_coordinate() ].
% A user-specified point, preferably as a tuple, otherwise as a list (hence as a
% vector), with 3 integer or floating-point coordinates.


-type point3() :: { X :: coordinate(), Y :: coordinate(), Z :: coordinate() }.
% A point in a 3D space, with (exactly) 3 floating-point coordinates.


-type integer_point3() :: { X :: integer_coordinate(),
							Y :: integer_coordinate(),
							Z :: integer_coordinate() }.
 % A point in a 3D space, with (exactly) 3 integer coordinates.


-type any_point3() :: point3() | integer_point3().
% A point in a 3D space, with any numerical coordinates (3 of them).


-type vertex3() :: point3().
% A 3D (floating-point) vertex of a mesh.

-type integer_vertex3() :: point3().
% A 3D integer vertex of a mesh.

-type any_vertex3() :: any_point3().
% A 3D vertex of a polygon, with any numerical coordinates (3 of them).


-type yup_point3() :: point3().
% A point in a 3D space, yet with Y-UP conventions (as opposed to Myriad's Z-UP
% ones). Refer to the design notes in linear_3D.erl for further details.



-export_type([ user_point3/0, point3/0, integer_point3/0, any_point3/0,
			   vertex3/0, integer_vertex3/0, any_vertex3/0, yup_point3/0 ]).


-export([ new/1, new/3, new_integer/3, null/0,
		  from_vector/1, to_vector/1, to_any_vector/1,
		  point3_to_yup/1, yup_to_point3/1, point3_to_yups/1, yup_to_point3s/1,
		  to_buffer/1,

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

-type vector3() :: vector3:vector3().
-type any_vector3() :: vector3:any_vector3().



% @doc Returns a 3D point corresponding to the user-specified one (preferably a
% tuple rather than a list).
%
% We do not check whether all coordinates are either integer or floating-point
% ones.
%
-spec new( user_point3() ) -> any_point3().
new( UserPoint3 ) when is_list( UserPoint3 ) ->
	list_to_tuple( UserPoint3 );

new( UserPoint3 ) when is_tuple( UserPoint3 ) ->
	UserPoint3.



% @doc Returns a 3D point corresponding to the user-specified one.
-spec new( user_coordinate(), user_coordinate(), user_coordinate() ) ->
			point3().
new( X, Y, Z ) ->
	{ type_utils:ensure_float( X ), type_utils:ensure_float( Y ),
	  type_utils:ensure_float( Z ) }.



% @doc Returns an integer 3D point corresponding to the user-specified one.
-spec new_integer( integer_coordinate(), integer_coordinate(),
				   integer_coordinate() ) -> integer_point3().
new_integer( X, Y, Z ) when is_integer( X ) andalso is_integer( Y )
							andalso is_integer( Z ) ->
	{ X, Y, Z }.



% @doc Returns a 3D point whose coordinates are all null, that is the origin of
% the local referential.
%
% See also linear_3D:get_origin/0.
%
-spec null() -> point3().
null() ->
	Zero = 0.0,
	{ Zero, Zero, Zero }.



% @doc Returns a 3D point corresponding to the specified vector (expected to be
% type-homogeneous).
%
-spec from_vector( any_vector3() ) -> any_point3().
from_vector( V3 ) ->
	list_to_tuple( V3 ).



% @doc Returns a 3D vector (with floating-vector coordinates) corresponding to
% the specified 3D point.
%
-spec to_vector( any_point3() ) -> vector3().
to_vector( P3 ) ->
	vector3:from_point( P3 ).


% @doc Returns a 3D vector (with coordinates of the same type) corresponding to
% the specified 3D point.
%
-spec to_any_vector( any_point3() ) -> any_vector3().
to_any_vector( P3 ) ->
	tuple_to_list( P3 ).



% @doc Converts a usual 3D point into one that follows the Y-UP convention (as
% opposed to the Myriad Z-UP one, see the design notes in linear_3D).
%
-spec point3_to_yup( point3() ) -> yup_point3().
point3_to_yup( _P={X,Y,Z} ) ->
	{X,Z,-Y}.


% @doc Converts usual 3D points into ones that follow the Y-UP convention (as
% opposed to the Myriad Z-UP one, see the design notes in linear_3D).
%
-spec point3_to_yups( [ point3() ] ) -> [ yup_point3() ].
point3_to_yups( Points ) ->
	[ point3_to_yup( P ) || P <- Points ].



% @doc Converts a 3D following the Y-UP convention (as opposed to the Myriad
% Z-UP one, see the design notes in linear_3D) into a usual one.
%
-spec yup_to_point3( yup_point3() ) -> point3().
yup_to_point3( _P={X,Y,Z} ) ->
	{X,-Z,Y}.


% @doc Converts a 3D following the Y-UP convention (as opposed to the Myriad
% Z-UP one, see the design notes in linear_3D) into a usual one.
%
-spec yup_to_point3s( [ yup_point3() ] ) -> [ point3() ].
yup_to_point3s( Points ) ->
	[ yup_to_point3( P ) || P <- Points ].



% @doc Returns a binary buffer of floats, corresponding to the specified points.
%
% Typically suitable for OpenGL.
%
-spec to_buffer( [ point3() ] ) -> binary().
to_buffer( Points ) ->
	<< <<X:?F32, Y:?F32, Z:?F32>> || { X, Y, Z } <- Points >>.



% @doc Returns a point whose floating-point coordinates have been rounded to the
% respective nearest integers.
%
-spec roundify( point3() ) -> integer_point3().
roundify( _P={X,Y,Z} ) ->
	{ erlang:round(X), erlang:round(Y), erlang:round(Z) }.



% @doc Returns a point corresponding the midpoint (middle) of the two specified
% points.
%
-spec get_center( point3(), point3() ) -> point3().
get_center( _P1={X1,Y1,Z1}, _P2={X2,Y2,Z2} ) ->
	{ (X1+X2)/2, (Y1+Y2)/2, (Z1+Z2)/2 }.



% @doc Returns a point corresponding the middle of the two specified points,
% returned with integer coordinates.
%
-spec get_integer_center( point3(), point3() ) -> integer_point3().
get_integer_center( P1, P2 ) ->
	roundify( get_center( P1, P2 ) ).



% @doc Returns a point corresponding to the specified point translated of the
% specified vector.
%
-spec translate( point3(), vector3() ) -> point3().
translate( _P={X,Y,Z}, _V=[Vx,Vy,Vz] ) ->
	{ X+Vx, Y+Vy, Z+Vz }.



% @doc Scales the specified 3D point of the specified scalar factor.
-spec scale( any_point3(), factor() ) -> point3().
scale( _P={X,Y,Z}, Factor ) ->
	{ Factor*X, Factor*Y, Factor*Z }.



% @doc Returns a vector V made from the specified two points: V=P2-P1.
-spec vectorize( point3(), point3() ) -> vector3().
vectorize( _P1={X1,Y1,Z1}, _P2={X2,Y2,Z2} ) ->
	[ X2-X1, Y2-Y1, Z2-Z1 ].



% @doc Returns whether the two specified 3D points are close, that is if they
% could be considered as representing the same point (equality operator on
% points).
%
-spec are_close( point3(), point3() ) -> boolean().
are_close( P1, P2 ) ->
	are_equal( P1, P2 ).


% @doc Returns whether the two specified 3D points are equal, that is if they
% could be considered as representing the same point (equality operator on
% points).
%
-spec are_equal( point3(), point3() ) -> boolean().
are_equal( _P1={X1,Y1,Z1}, _P2={X2,Y2,Z2} ) ->
	math_utils:are_close( X1, X2 ) andalso math_utils:are_close( Y1, Y2 )
		andalso math_utils:are_close( Z1, Z2 ).



% @doc Tells whether the specified 3D point P1 is within a distance D from 3D
% point P2, using some margin to overcome numerical errors.
%
-spec is_within( point3(), point3(), distance() ) -> boolean().
is_within( P1, P2, D ) ->
	% "Taylor series", square(epsilon) is negligible here:
	square_distance( P1, P2 ) < D * ( D + ?epsilon ).



% @doc Tells whether the specified 3D point P1 is within a square distance
% SquareD from 3D point P2.
%
-spec is_within_square( point3(), point3(), square_distance() ) -> boolean().
is_within_square( P1, P2, SquareD ) ->
	square_distance( P1, P2 ) < SquareD.



% @doc Returns the square of the distance between the two specified 3D points.
%
% For comparison purposes, computing the square root is useless.
%
% Could rely on vectorize and square_magnitude as well.
%
-spec square_distance( point3(), point3() ) -> square_distance().
square_distance( _P1={X1,Y1,Z1}, _P2={X2,Y2,Z2} ) ->

	XDiff = X2 - X1,
	YDiff = Y2 - Y1,
	ZDiff = Z2 - Z1,

	XDiff*XDiff + YDiff*YDiff + ZDiff*ZDiff.



% @doc Returns the distance between the two specified 3D points.
%
% Note: just for comparison purposes, computing the square root is useless.
%
-spec distance( point3(), point3() ) -> distance().
distance( P1, P2 ) ->
	math:sqrt( square_distance( P1, P2 ) ).



% @doc Checks that the specified 3D point is legit, and returns it.
-spec check( point3() ) -> point3().
check( P ) ->
	point:check( P ).



% @doc Returns a textual representation of the specified 3D point; full float
% precision is shown.
%
-spec to_string( any_point3() ) -> ustring().
to_string( Point3 ) ->
	to_user_string( Point3 ).



% @doc Returns a compact, textual, informal representation of the specified
% 3D point.
%
-spec to_compact_string( any_point3() ) -> ustring().
to_compact_string( Point3 ) ->
	text_utils:format( "~w", [ Point3 ] ).



% @doc Returns a basic, not even fixed-width for floating-point coordinates (see
% linear.hrl for width and precision) representation of the specified 3D point.
%
-spec to_basic_string( any_point3() ) -> ustring().
to_basic_string( Point3 ) ->

	CoordList = tuple_to_list( Point3 ),

	% Points supposed to be homogeneous tuples:
	CoordFmt = case is_integer( hd( CoordList ) ) of

		true ->
			?coord_integer_format;

		false ->
			?coord_float_format

	end,

	ElemFormatStr = "{ " ++ CoordFmt ++ " }~n",

	FormatStr = "~n" ++ text_utils:duplicate( 3, ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; CoordList: ~w.",
	%                       [ FormatStr, CoordList ] ),

	text_utils:format( FormatStr, CoordList ).



% @doc Returns a textual, more user-friendly representation of the specified
% 3D point; full float precision is shown.
%
% This is the recommended representation.
%
-spec to_user_string( any_point3() ) -> ustring().
to_user_string( Point3 ) ->

	CoordList = tuple_to_list( Point3 ),

	Strs = linear:coords_to_best_width_strings( CoordList ),

	% No need for ~ts here:
	ElemFormatStr = "{ ~s }~n",

	FormatStr = "~n" ++ text_utils:duplicate( 3, ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
	%                       [ FormatStr, Strs ] ),

	text_utils:format( FormatStr, Strs ).
