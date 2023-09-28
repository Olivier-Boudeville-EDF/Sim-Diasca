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
% Creation date: Sunday, October 3, 2021.


% @doc Module implementing the support for points in <b>arbitrary dimension</b>.
%
% See also:
% - the corresponding arbitrary-dimensioned vectors (in vector.erl) and matrices
% (in matrix.erl)
% - the specialised points, such as points{2,3,4}.erl
%
-module(point).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For the epsilon define:
-include("math_utils.hrl").


% Implementation notes:
%
% No dependent types, not able to declare a point(D) type.
%
% We call a container type-homogeneous if all the coordinates that it gathers
% are all either integer or floating-point ones.



-type user_point() :: any_point() | [ any_coordinate() ].
% A user-specified point, preferably as a tuple, otherwise as a list (hence as a
% vector), with integer or floating-point coordinates.


%-type point() :: tuple( coordinate() ).
-type point() :: tuple().
% A point in a space of arbitrary dimension, with floating-point coordinates.


%-type integer_point() :: tuple( integer_coordinate() ).
-type integer_point() :: tuple().
% A point of any dimension, with integer coordinates (e.g. on-screen ones).


-type any_point() :: point() | integer_point().
% A point of any dimension, with any numerical coordinates.


-type specialised_point() :: point2:point2()
						   | point3:point3()
						   | point4:point4().
% A specialised point that is of one of the specifically-supported dimensions.


-export_type([ user_point/0, point/0, integer_point/0, any_point/0,
			   specialised_point/0 ]).


-export([ new/1, null/1, from_vector/1, to_vector/1, to_any_vector/1,
		  roundify/1,
		  translate/2, vectorize/2,
		  dimension/1, are_close/2, are_equal/2,
		  is_within/3, is_within_square/3,
		  square_distance/2, distance/2,
		  check/1,
		  to_string/1, to_compact_string/1, to_basic_string/1,
		  to_user_string/1 ] ).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type dimension() :: linear:dimension().
%-type coordinate() :: linear:coordinate().
%-type integer_coordinate() :: linear:integer_coordinate().
-type any_coordinate() :: linear:any_coordinate().

-type distance() :: linear:distance().
-type square_distance() :: linear:square_distance().

-type vector() :: vector:vector().
-type any_vector() :: vector:any_vector().



% @doc Returns an (arbitrary) point corresponding to the user-specified one
% (preferably a tuple rather than a list).
%
% We do not check whether all coordinates are either integer or floating-point
% ones.
%
-spec new( user_point() ) -> any_point().
new( UserPoint ) when is_list( UserPoint ) ->
	list_to_tuple( UserPoint );

new( UserPoint ) when is_tuple( UserPoint ) ->
	UserPoint.



% @doc Returns an (arbitrary) point of specified dimension whose coordinates
% are all null.
%
-spec null( dimension() ) -> point().
null( Dim ) ->
	list_to_tuple( lists:duplicate( Dim, 0.0 ) ).



% @doc Returns an (arbitrary) point corresponding to the specified vector
% (expected to be type-homogeneous).
%
-spec from_vector( any_vector() ) -> any_point().
from_vector( V ) ->
	list_to_tuple( V ).



% @doc Returns an (arbitrary, and with floating-vector coordinates) vector
% corresponding to the specified point.
%
-spec to_vector( any_point() ) -> vector().
to_vector( P ) ->
	vector:from_point( P ).


% @doc Returns an (arbitrary, and with coordinates of the same type) vector
% corresponding to the specified point.
%
-spec to_any_vector( any_point() ) -> any_vector().
to_any_vector( P ) ->
	tuple_to_list( P ).



% @doc Returns a point whose floating-point coordinates have been rounded to the
% respective nearest integers.
%
-spec roundify( point() ) -> integer_point().
roundify( {X,Y,Z} ) ->
	{ erlang:round(X), erlang:round(Y), erlang:round(Z) }.



% @doc Returns a point corresponding to the specified point translated by the
% specified vector.
%
-spec translate( point(), vector() ) -> point().
translate( P, V ) ->
	translate( tuple_to_list( P ), V, _AccP=[] ).


% (helper)
translate( _P=[], _V=[], AccP ) ->
	list_to_tuple( lists:reverse( AccP ) );

translate( _P=[ CP | TP ], _V=[ CV | TV ], AccP ) ->
	translate( TP, TV, [ CP+CV | AccP ] ).



% @doc Returns a vector V made from the specified two points: V=P2-P1.
-spec vectorize( point(), point() ) -> vector().
vectorize( P1, P2 ) ->
	vectorize( tuple_to_list( P1 ), tuple_to_list( P2 ), _AccV=[] ).


% (helper)
vectorize( _P1=[], _P2=[], AccV ) ->
	lists:reverse( AccV );

vectorize( _P1=[ C1 | T1 ], _V=[ C2 | T2 ], AccV ) ->
	vectorize( T1, T2, [ C2-C1 | AccV ] ).



% @doc Returns the dimension of the specified point.
-spec dimension( any_point() ) -> dimension().
dimension( P ) ->
	size( P ).



% @doc Returns whether the two specified points are close, that if they could be
% considered as representing the same point (equality operator on points).
%
% Alias for are_equal/2.
%
-spec are_close( point(), point() ) -> boolean().
are_close( P1, P2 ) ->
	are_equal( P1, P2 ).


% @doc Returns whether the two specified points are equal, that if they could be
% considered as representing the same point (equality operator on points).
%
-spec are_equal( point(), point() ) -> boolean().
are_equal( P1, P2 ) ->
	vector:are_equal( tuple_to_list( P1 ), tuple_to_list( P2 ) ).



% @doc Tells whether point P1 is within a distance D from point P2, using some
% margin to overcome numerical errors.
%
-spec is_within( point(), point(), distance() ) -> boolean().
is_within( P1, P2, D ) ->
	% "Taylor series", square(epsilon) is negligible here:
	square_distance( P1, P2 ) < D * ( D + ?epsilon ).


% @doc Tells whether point P1 is within a square distance SquareD from point P2.
-spec is_within_square( point(), point(), square_distance() ) -> boolean().
is_within_square( P1, P2, SquareD ) ->
	square_distance( P1, P2 ) < SquareD.



% @doc Returns the square of the distance between the two specified points.
%
% For comparison purposes, computing the square root is useless.
%
% Could rely on vectorize and square_magnitude as well.
%
-spec square_distance( point(), point() ) -> square_distance().
square_distance( P1, P2 ) ->
	square_distance( tuple_to_list( P1 ), tuple_to_list( P2 ), _Acc=0.0 ).


% (helper)
square_distance( _P1=[], _P2=[], Acc ) ->
	Acc;

square_distance( _P1=[ C1 | T1 ], _P2=[ C2 | T2 ], Acc ) ->
	CDiff = C2 - C1,
	NewAcc = Acc + CDiff*CDiff,
	square_distance( T1, T2, NewAcc ).



% @doc Returns the distance between the two specified points.
%
% Note: just for comparison purposes, computing the square root is useless.
%
-spec distance( point(), point() ) -> distance().
distance( P1, P2 ) ->
	math:sqrt( square_distance( P1, P2 ) ).



% @doc Checks that the specified point is legit, and returns it.
-spec check( any_point() ) -> any_point().
check( P ) ->
	[ C | T ] = tuple_to_list( P ),
	case is_integer( C ) of

		true ->
			vector:check_integer( T );

		false ->
			vector:check( T )

	end,
	P.



% @doc Returns a textual representation of the specified point; full float
% precision is shown.
%
-spec to_string( any_point() ) -> ustring().
to_string( Point ) ->
	to_user_string( Point ).



% @doc Returns a compact, textual, informal representation of the specified
% point.
%
-spec to_compact_string( any_point() ) -> ustring().
to_compact_string( Point ) ->
	text_utils:format( "~w", [ Point ] ).



% @doc Returns a basic, not even fixed-width for floating-point coordinates (see
% linear.hrl for width and precision) representation of the specified point.
%
-spec to_basic_string( any_point() ) -> ustring().
to_basic_string( Point ) ->

	CoordList = tuple_to_list( Point ),

	% Points supposed to be homogeneous tuples:
	CoordFmt = case is_integer( hd( CoordList ) ) of

		true ->
			?coord_integer_format;

		false ->
			?coord_float_format

	end,

	ElemFormatStr = "{ " ++ CoordFmt ++ " }~n",

	FormatStr = text_utils:duplicate( length( CoordList ), ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; CoordList: ~w.",
	%                       [ FormatStr, CoordList ] ),

	text_utils:format( FormatStr, CoordList ).



% @doc Returns a textual, more user-friendly representation of the specified
% point; full float precision is shown.
%
% This is the recommended representation.
%
-spec to_user_string( any_point() ) -> ustring().
to_user_string( Point ) ->

	CoordList = tuple_to_list( Point ),

	Strs = linear:coords_to_best_width_strings( CoordList ),

	% No need for ~ts here:
	ElemFormatStr = "{ ~s }~n",

	FormatStr = text_utils:duplicate( length( CoordList ), ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
	%                       [ FormatStr, Strs ] ),

	text_utils:format( FormatStr, Strs ).
