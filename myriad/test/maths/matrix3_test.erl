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
% Creation date: Sunday, October 17, 2021.


% @doc Unit tests for the <b>specialised 3x3 matrix</b> facilities.
%
% See the matrix3 tested module.
%
-module(matrix3_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing 3x3 matrices." ),

	NullMatrix = matrix3:null(),

	test_facilities:display( "Null matrix is: ~ts",
							 [ matrix3:to_string( NullMatrix ) ] ),

	Dim = 3,

	Id = matrix3:identity(),
	true = matrix:are_equal( matrix:unspecialise( Id ),
							 matrix:identity( Dim ) ),

	% Compact matrix:
	true = matrix3:are_equal( Id, matrix3:new( [ 1.0, 0.0, 0.0 ],
											   [ 0.0, 1.0, 0.0 ] ) ),

	test_facilities:display( "Identity matrix is: ~ts",
							 [ matrix3:to_string( Id ) ] ),


	% Performing a counterclockwise rotation of 90° around the X axis:

	UnitRotVec = [ 1.0, 0.0, 0.0 ],

	RotAngle = math:pi() / 2,

	RotM = matrix3:rotation( UnitRotVec, RotAngle ),
	test_facilities:display( "Rotation matrix of angle ~f radians "
		"around the following axis (unitary vector) ~ts is ~ts",
		[ RotAngle, vector3:to_string( UnitRotVec ),
		  matrix3:to_string( RotM ) ] ),

	XAxis = [ 1.0, 0.0, 0.0 ],
	YAxis = [ 0.0, 1.0, 0.0 ],
	ZAxis = [ 0.0, 0.0, 1.0 ],

	% Transforms axes that way (check):

	% X unchanged:
	true = vector3:are_equal( XAxis, matrix3:apply( RotM, XAxis ) ),

	% Y to become Z:
	true = vector3:are_equal( ZAxis, matrix3:apply( RotM, YAxis ) ),

	% Z to become -Y:
	MinusYAxis = vector3:negate( YAxis ),

	true = vector3:are_equal( MinusYAxis, matrix3:apply( RotM, ZAxis ) ),


	V1 = [ 10.0, 25.0, -7.0 ],
	V2 = [ 1.0, 2.0, 4.0 ],
	V3 = [ 0.0, 0.0, 5.0 ],

	% Octave: ColMatrix = [ 10, 1, 0; 25, 2, 0; -7, 4, 5 ]
	ColMatrix = matrix3:from_columns( V1, V2, V3 ),
	V2 = matrix3:column( 2, ColMatrix ),

	test_facilities:display( "Matrix whose columns are V1, V2 and V3 "
							 "is: ~ts", [ matrix3:to_string( ColMatrix ) ] ),

	RowMatrix = matrix3:from_rows( V1, V2, V3 ),
	V3 = matrix3:row( 3, RowMatrix ),

	test_facilities:display( "Matrix whose rows are V1, V2 and V3 "
							 "is: ~ts", [ matrix3:to_string( RowMatrix ) ] ),

	% Octave: CoordMatrix = [ 1, 2, 3; 5, 6, 7; 9, 10, 11 ]
	CoordMatrix = matrix3:from_coordinates( 1.0,   2.0,  3.0,
											5.0,   6.0,  7.0,
											9.0,  10.0, 11.0 ),

	test_facilities:display( "Matrix explicitly set from coordinates is: ~ts",
							 [ matrix3:to_string( CoordMatrix ) ] ),


	CompactMatrix = matrix3:from_compact_coordinates( 1.0,  2.0,  3.0,
													  5.0,  6.0,  7.0 ),

	test_facilities:display( "Compact matrix explicitly set from coordinates "
		"is: ~ts", [ matrix3:to_string( CompactMatrix ) ] ),

	Matrix2 = matrix2:new( [ [ 1.0,  2.0 ],
							 [ 5.0,  6.0 ] ] ),

	Vec2 = [ 3.0, 7.0 ],

	CompactMatrix = matrix3:from_2D( Matrix2, Vec2 ),


	ScaleFactor = 2.0,

	ScaledMatrix = matrix3:scale( CoordMatrix, ScaleFactor ),

	test_facilities:display( "The previous matrix scaled by ~p is: ~ts",
						[ ScaleFactor, matrix3:to_string( ScaledMatrix ) ] ),

	ScaledMatrix = matrix3:to_canonical( ScaledMatrix ),

	CompactIdMatrix = matrix3:to_compact( Id ),

	test_facilities:display( "Compact version of identity is: ~ts",
							 [ matrix3:to_string( CompactIdMatrix ) ] ),

	SubMatrix = matrix3:new( [ [ 0.0,   0.0,  0.0 ],
							   [ 0.0,   0.0,  0.0 ],
							   [ 9.0,  10.0, 10.0 ] ] ),

	% Tests scale/2 and add/2 as well:
	true = matrix3:are_equal( matrix3:sub( CoordMatrix, CompactMatrix ),
							  SubMatrix ),


	10.0 = matrix3:get_element( _RowC=3, _ColC=2, SubMatrix ),
	SubSetMatrix = matrix3:set_element( RwC=3, ClC=1, 21.0, SubMatrix ),
	21.0 = matrix3:get_element( RwC, ClC, SubSetMatrix ),

	TransposedCoordMatrix = matrix3:from_coordinates( 1.0, 5.0,  9.0,
													  2.0, 6.0, 10.0,
													  3.0, 7.0, 11.0 ),

	TransposedCoordMatrix = matrix3:transpose( CoordMatrix ),
	true = matrix3:are_equal( TransposedCoordMatrix, TransposedCoordMatrix ),


	% Checked with octave, for example:
	%
	% CoordMatrix = [ 1, 2, 3 ; 5, 6, 7 ; 9, 10, 11 ]
	% ScaledMatrix = 2.0 * CoordMatrix
	% RowMatrix = [ 10, 25, -7; 1, 2 4; 0, 0, 5 ]

	MultCanMatrix = matrix3:from_coordinates(  24.0,  58.0, 32.0,
												 112.0, 274.0, 48.0,
												 200.0, 490.0, 64.0 ),

	MultCanMatrix = matrix3:mult( ScaledMatrix, RowMatrix ),

	[ ArbitraryScaledMatrix, ArbitraryRowMatrix, ArbitraryMultCanMatrix ] =
		[ matrix:unspecialise( M )
			|| M <- [ ScaledMatrix, RowMatrix, MultCanMatrix ] ],

	ArbitraryMultCanMatrix =
		matrix:mult( ArbitraryScaledMatrix, ArbitraryRowMatrix ),

	test_facilities:display( "The multiplication of matrix ~ts "
		"by matrix ~ts yields: ~ts",
		[ matrix3:to_string( ScaledMatrix ),
		  matrix3:to_string( RowMatrix ),
		  matrix3:to_string( MultCanMatrix ) ] ),


	% Octave:
	% FirstCompactMatrix = [ 30, 12, 15; 62, 42, 50; 0, 0, 1 ]
	FirstCompactMatrix = matrix3:from_compact_coordinates(
						   30.0, 12.0, 15.0,
						   62.0, 42.0, 50.0 ),

	%test_facilities:display( "FirstCompactMatrix = ~ts",
	%                         [ matrix3:to_string( FirstCompactMatrix ) ] ),

	% SecondCompactMatrix = [ 59, 44, 24; 97, 4, 56; 0, 0, 1 ]
	SecondCompactMatrix = matrix3:from_compact_coordinates(
							59.0,  44.0, 24.0,
							97.0,   4.0, 56.0 ),

	% MultCptMatrix = [ 2934, 1368, 1407; 7732, 2896, 3890; 0, 0, 1 ]
	MultCptMatrix = matrix3:from_compact_coordinates(
							2934.0, 1368.0, 1407.0,
							7732.0, 2896.0, 3890.0 ),

	true = matrix3:are_equal( MultCptMatrix,
					matrix3:mult( FirstCompactMatrix, SecondCompactMatrix ) ),

	% Octave:
	% MultCptCanMatrix = [ 450, 564, 678; 1444, 1752, 2060; 18, 20, 22 ]
	MultCptCanMatrix = matrix3:from_coordinates(
							450.0, 564.0,   678.0,
						   1444.0, 1752.0, 2060.0,
							 18.0,   20.0,   22.0 ),

	%test_facilities:display( "ScaledMatrix = ~ts",
	%						  [ matrix3:to_string( ScaledMatrix ) ] ),

	FirstMult = matrix3:mult( FirstCompactMatrix, ScaledMatrix ),

	test_facilities:display( "Comparing matrix ~ts with matrix ~ts",
		[ matrix3:to_string( MultCptCanMatrix ),
		  matrix3:to_string( FirstMult ) ] ),

	true = matrix3:are_equal( MultCptCanMatrix, FirstMult ),


	% MultCanCptMatrix = [ 308, 192, 236; 1044, 624, 764; 1780, 1056, 1292 ]
	MultCanCptMatrix = matrix3:from_coordinates(
						 308.0,  192.0,  236.0,
						1044.0,  624.0,  764.0,
						1780.0, 1056.0, 1292.0 ),

	SecondMult = matrix3:mult( ScaledMatrix, FirstCompactMatrix ),

	test_facilities:display( "Comparing matrix ~ts with matrix ~ts",
		[ matrix3:to_string( MultCanCptMatrix ),
		  matrix3:to_string( SecondMult ) ] ),

	true = matrix3:are_equal( MultCanCptMatrix, SecondMult ),

	% Knowing that CoordMatrix is not invertible (its determinant is epsilon):
	-25.0 = matrix3:determinant( ColMatrix ),

	InvColMatrix = matrix3:inverse( ColMatrix ),

	test_facilities:display( "The inverse of canonical matrix ~ts is: ~ts",
		[ matrix3:to_string( ColMatrix ),
		  matrix3:to_string( InvColMatrix ) ] ),

	true = matrix3:are_equal( Id,
		matrix3:mult( ColMatrix, InvColMatrix ) ),

	true = matrix3:are_equal( Id,
		matrix3:mult( InvColMatrix, ColMatrix ) ),

	% From octave: inv(ColMatrix)
	InvColMatrixOctave = matrix3:new( [ [ -0.4,   0.2, 0.0 ],
										[  5.0,  -2.0, 0.0 ],
										[ -4.56, 1.88, 0.2 ] ] ),

	true = matrix3:are_equal( InvColMatrix, InvColMatrixOctave ),



	516.0 = matrix3:determinant( FirstCompactMatrix ),

	InvFirstCompactMatrix = matrix3:inverse( FirstCompactMatrix ),

	test_facilities:display( "The inverse of compact matrix ~ts is: ~ts",
		[ matrix3:to_string( FirstCompactMatrix ),
		  matrix3:to_string( InvFirstCompactMatrix ) ] ),

	true = matrix3:are_equal( Id,
		matrix3:mult( FirstCompactMatrix, InvFirstCompactMatrix ) ),

	true = matrix3:are_equal( Id,
		matrix3:mult( InvFirstCompactMatrix, FirstCompactMatrix ) ),

	InvFirstCompactMatrixOctave = matrix3:from_compact_coordinates(
		 0.081395348837209, -0.023255813953488, -0.058139534883721,
		-0.120155038759690,  0.058139534883721, -1.104651162790698 ),

	true = matrix3:are_equal( InvFirstCompactMatrix,
							  InvFirstCompactMatrixOctave ),

	test_facilities:stop().
