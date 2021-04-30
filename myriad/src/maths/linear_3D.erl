% Copyright (C) 2003-2021 Olivier Boudeville
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
% Creation date: Monday, February 15, 2010.


% Gathering of various three dimensional linear facilities.
%
% See linear_3D_test.erl for the corresponding test.
%
-module(linear_3D).


% Relatively aggressive inlining for basic operations:
-compile( inline ).
-compile( { inline_size, 48 } ).


% For the mat3 record:
-include("linear_3D.hrl").


% Operations on points:
-export([ get_origin/0, are_close/2, is_within/3, is_within_square/3,
		  square_distance/2, distance/2, cross_product/2, roundify/1,
		  get_integer_center/2, get_center/2, translate/2 ]).


% Operations on vectors:
-export([ vectorize/2, square_magnitude/1, magnitude/1, scale/2, make_unit/1,
		  dot_product/2 ]).


% Operations common to points and vectors:
-export([ add/1, add/2 ]).


% Textual conversions:
-export([ to_string/1 ]).


% For epsilon:
-include("math_utils.hrl").


% Shorthands:

-type coordinate() :: linear:coordinate().
-type integer_coordinate() :: linear:integer_coordinate().
-type factor() :: linear:factor().
-type distance() :: linear:distance().
-type square_distance() :: linear:square_distance().
-type ustring() :: text_utils:ustring().



% Section about points and vectors.


% A 3D vector, with floating-point coordinates.
%
% They are typically referenced as [ X, Y, Z ].
%
-type point() :: { coordinate(), coordinate(), coordinate() }.


% A 3D point, with integer coordinates.
%
-type integer_point() ::
		{ integer_coordinate(), integer_coordinate(), integer_coordinate() }.


% Vectors could/should be aliased to points.


% A 3D vector, with floating-point coordinates:
-type vector() :: { coordinate(), coordinate(), coordinate() }.


% A 3D unit vector, i.e. a vector of magnitude 1.0.
%
% For documentation purpose.
%
-type unit_vector() :: vector().


% A 3D vector normal to a plane.
%
% For documentation purpose.
%
-type normal() :: vector().


% A 3D unit vector normal to a plane.
%
% For documentation purpose.
%
-type unit_normal() :: unit_vector().


% 3D vector, with integer coordinates:
-type integer_vector() ::
		{ integer_coordinate(),	integer_coordinate(), integer_coordinate() }.


-export_type([ point/0, integer_point/0,
			   vector/0, unit_vector/0, normal/0, unit_normal/0,
			   integer_vector/0 ]).



% Section about lines and planes.


% A 3D line, whose equation A.x+B.y+C.z+D=0, can be defined from these four
% factors {A,B,C,D}.
%
-type line() :: { factor(), factor(), factor(), factor() }.


% A plane, whose general equation is: A.x + B.y + C.z + D = 0, where:
%
% - P=(x,y,z) is a point belonging to this plane
%
% - N=(A,B,C) is a (non-necessarily unit) normal vector to this plane
%
% - P0=(x0,y0,z0) is a point of that plane
%
% - D= -A.x0 - B.y0 - C.z0
%
% See http://mathworld.wolfram.com/Plane.html
%
% So a plane may be described as (N,D):
%
-type plane() :: { normal(), factor() }.


% A plane in Hessian normal form.
%
% See http://mathworld.wolfram.com/HessianNormalForm.html
%
-type hessian_plane() :: { unit_normal(), factor() }.


-export_type([ line/0, plane/0, hessian_plane/0 ]).



% Section about matrices.

% Alias for 3x3 canonical matrices:
-type mat3() :: #mat3{}.
-type canonical_matrix() :: mat3().


% Aliases for 3x3 compact matrices:
-type cpt_mat3() :: #cpt_mat3{}.
-type compact_matrix() :: cpt_mat3().


-type matrix() :: 'identity_3' | canonical_matrix() | compact_matrix().


-export_type([ mat3/0, canonical_matrix/0,
			   cpt_mat3/0, compact_matrix/0, matrix/0 ]).



% Section about shapes.


% Various types of known 3D shapes (basic geometries):
-type shape() :: 'sphere' | 'right_cuboid'.


-export_type([ shape/0 ]).




% Implementations now.


% Point section.


% Returns the origin of this referential.
-spec get_origin() -> point().
get_origin() ->
	{ 0, 0, 0 }.



% Returns whether the two specified points are close, i.e. if they could be
% considered as representing the same point (equality operator on points).
%
-spec are_close( point(), point() ) -> boolean().
are_close( _P1={X1,Y1,Z1}, _P2={X2,Y2,Z2} ) ->
	math_utils:are_close( X1, X2 ) andalso math_utils:are_close( Y1, Y2 )
		andalso math_utils:are_close( Z1, Z2 ).



% Tells whether point P is within a distance D from point C, using some margin
% to overcome numerical errors.
%
-spec is_within( point(), point(), distance() ) -> boolean().
is_within( P, C, D ) ->
	% "Taylor series", square(epsilon) is negligible here:
	square_distance( P, C ) < D * ( D + ?epsilon ).



% Tells whether point P is within a square distance SquareD from point C.
-spec is_within_square( point(), point(), square_distance() ) -> boolean().
is_within_square( P, C, SquareD ) ->
	square_distance( P, C ) < SquareD.



% Returns the square of the distance between the two specified points.
%
% For comparison purposes, computing the square root is useless.
%
% Could rely on vectorize and square_magnitude as well.
%
-spec square_distance( point(), point() ) -> square_distance().
square_distance( {X1,Y1,Z1}, {X2,Y2,Z2} ) ->

	XDiff = X2 - X1,
	YDiff = Y2 - Y1,
	ZDiff = Z2 - Z1,

	XDiff*XDiff + YDiff*YDiff + ZDiff*ZDiff.



% Returns the distance between the two specified points.
%
% Note: just for comparison purposes, computing the square root is useless.
%
% Could rely on vectorize and magnitude as well.
%
-spec distance( point(), point() ) -> distance().
distance( P1, P2 ) ->
	math:sqrt( square_distance( P1, P2 ) ).



% Returns the cross-product of the two specified vectors, i.e. the results of a
% regular 3D cross product of the input vectors.
%
-spec cross_product( vector(), vector() ) -> vector().
cross_product( {X1,Y1,Z1}, {X2,Y2,Z2} ) ->
	{ Y1*Z2 - Z1*Y2, Z1*X2 - X1*Z2, X1*Y2 - Y1*X2 }.




% Returns a point (or vector) whose coordinates have been rounded to the
% respective nearest integers.
%
-spec roundify( point() ) -> integer_point().
roundify( {X,Y,Z} ) ->
	{ erlang:round(X), erlang:round(Y), erlang:round(Z) }.



% Returns a vertex corresponding the middle of the two specified vertices.
-spec get_center( point(), point() ) -> point().
get_center( {X1,Y1,Z1}, {X2,Y2,Z2} ) ->
	{ (X1+X2)/2, (Y1+Y2)/2, (Z1+Z2)/2 }.



% Returns a vertex corresponding the middle of the two specified vertices,
% returned with integer coordinates.
%
-spec get_integer_center( point(), point() ) -> integer_point().
get_integer_center( P1, P2 ) ->
	roundify( get_center( P1, P2 ) ).



% Returns a point corresponding to the specified point P translated by the
% specified vector V.
%
-spec translate( point(), vector() ) -> point().
translate( _P={X,Y,Z}, _V={Vx,Vy,Vz} ) ->
	{ X+Vx, Y+Vy, Z+Vz }.




% Section for sets of points.


% Vector section.


% Returns a vector V made from the specified two points: V=P2-P1.
-spec vectorize( point(), point() ) -> vector().
vectorize( _P1={X1,Y1,Z1}, _P2={X2,Y2,Z2} ) ->
	{ X2-X1, Y2-Y1, Z2-Z1 }.



% Returns the square of the magnitude of the specified vector.
-spec square_magnitude( vector() ) -> square_distance().
square_magnitude( _V={X,Y,Z} ) ->
	X*X + Y*Y + Z*Z.


% Returns the magnitude of the specified vector.
-spec magnitude( vector() ) -> distance().
magnitude( V ) ->
	math:sqrt( square_magnitude( V ) ).


% Scales specified vector of specified factor.
%
-spec scale( vector(), factor() ) -> vector().
scale( _V={X,Y,Z}, Factor ) ->
	{ Factor*X, Factor*Y, Factor*Z }.



% Returns the specified vector with an unit length (whose magnitude is thus 1).
-spec make_unit( vector() ) -> vector().
make_unit( V ) ->
	case magnitude( V ) of

		M when M < ?epsilon ->
			throw( cannot_make_null_vector_unit );

		M ->
			scale( V, 1.0 / M )

	end.



% Returns the dot product of the two specified vectors: D = V1.V2.
-spec dot_product( vector(), vector() ) -> float().
dot_product( _V1={X1,Y1,Z1}, _V2={X2,Y2,Z2} ) ->
	X1*X2 + Y1*Y2 + Z1*Z2.



% Returns the sum of the two specified vectors: C = A + B.
-spec add( vector(), vector() ) -> vector().
add( { X1, Y1, Z1 }, { X2, Y2, Z2 } ) ->
	{ X1 + X2, Y1 + Y2, Z1 + Z2 }.



% Returns the sum of all vectors in the specified list.
-spec add( [ vector() ] ) -> vector().
add( Vectors ) ->

	lists:foldl( fun( { X, Y, Z }, _AccVec={ Xa, Ya, Za } ) ->
					{ X + Xa, Y + Ya, Z + Za }
				 end,
				 _InitialAcc={ 0, 0, 0 },
				 _List=Vectors ).





% Textual conversion section.


% Returns a stringified representation of specified point or vector.
-spec to_string( point() | vector() ) -> ustring().
to_string( { X, Y, Z } ) ->
	io_lib:format( "{ ~w, ~w, ~w }", [ X, Y, Z ] ).
