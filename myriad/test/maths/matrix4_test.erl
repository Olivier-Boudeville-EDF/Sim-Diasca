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
% Creation date: Wednesday, October 13, 2021.


% @doc Unit tests for the <b>specialised 4x4 matrix</b> facilities.
%
% See the matrix4 tested module.
%
-module(matrix4_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing 4x4 matrices." ),

	NullMatrix = matrix4:null(),

	test_facilities:display( "Null matrix is: ~ts",
							 [ matrix4:to_string( NullMatrix ) ] ),

	Dim = 4,

	Id = matrix4:identity(),
	true = matrix:are_equal( matrix:unspecialise( Id ),
							 matrix:identity( Dim ) ),

	% Compact matrix:
	true = matrix4:are_equal( Id, matrix4:new(
									[ 1.0, 0.0, 0.0, 0.0 ],
									[ 0.0, 1.0, 0.0, 0.0 ],
									[ 0.0, 0.0, 1.0, 0.0 ] ) ),

	test_facilities:display( "Identity matrix is: ~ts",
							 [ matrix4:to_string( Id ) ] ),

	V1 = [ 10.0, 25.0, -7.0, 2.0 ],
	V2 = [ 1.0, 2.0, 4.0, 8.71 ],
	V3 = [ 0.0, 0.0, 0.0, 3.0 ],
	V4 = [ 11.0, 27.0, -3.0, 10.71 ],

	% Octave: ColMatrix = [ 10, 1, 0, 11; 25, 2, 0, 27;
	%                       -7, 4, 0, -3; 2, 8.71, 3, 10.71 ]
	%
	ColMatrix = matrix4:from_columns( V1, V2, V3, V4 ),
	V2 = matrix4:column( 2, ColMatrix ),

	test_facilities:display( "Matrix whose columns are V1, V2, V3 and V4 "
							 "is: ~ts", [ matrix4:to_string( ColMatrix ) ] ),

	RowMatrix = matrix4:from_rows( V1, V2, V3, V4 ),
	V3 = matrix4:row( 3, RowMatrix ),

	test_facilities:display( "Matrix whose rows are V1, V2, V3 and V4 "
							 "is: ~ts", [ matrix4:to_string( RowMatrix ) ] ),


	% Octave: CoordMatrix = [ 1, 2, 3, 4; 5, 6, 7, 8; 9, 10, 11, 12;
	% 13, 14, 15, 16 ]
	%
	CoordMatrix = matrix4:from_coordinates( 1.0,   2.0,  3.0,  4.0,
											5.0,   6.0,  7.0,  8.0,
											9.0,  10.0, 11.0, 12.0,
											13.0, 14.0, 15.0, 16.0 ),

	test_facilities:display( "Matrix explicitly set from coordinates is: ~ts",
							 [ matrix4:to_string( CoordMatrix ) ] ),


	CompactMatrix = matrix4:from_compact_coordinates( 1.0,  2.0,  3.0,  4.0,
													  5.0,  6.0,  7.0,  8.0,
													  9.0, 10.0, 11.0, 12.0 ),

	test_facilities:display( "Compact matrix explicitly set from coordinates "
		"is: ~ts", [ matrix4:to_string( CompactMatrix ) ] ),

	Matrix3 = matrix3:new( [ [ 1.0,  2.0,  3.0 ],
							 [ 5.0,  6.0,  7.0 ],
							 [ 9.0, 10.0, 11.0 ] ] ),

	Vec3 = [ 4.0, 8.0, 12.0 ],

	CompactMatrix = matrix4:from_3D( Matrix3, Vec3 ),


	ScaleFactor = 2.0,

	ScaledMatrix = matrix4:scale( CoordMatrix, ScaleFactor ),

	test_facilities:display( "The previous matrix scaled by ~p is: ~ts",
						[ ScaleFactor, matrix4:to_string( ScaledMatrix ) ] ),

	ScaledMatrix = matrix4:to_canonical( ScaledMatrix ),

	CompactIdMatrix = matrix4:to_compact( Id ),

	test_facilities:display( "Compact version of identity is: ~ts",
							 [ matrix4:to_string( CompactIdMatrix ) ] ),

	SubMatrix = matrix4:new( [ [ 0.0,   0.0,  0.0,  0.0 ],
							   [ 0.0,   0.0,  0.0,  0.0 ],
							   [ 0.0,   0.0,  0.0,  0.0 ],
							   [ 13.0, 14.0, 15.0, 15.0 ] ] ),

	% Tests scale/2 and add/2 as well:
	true = matrix4:are_equal( matrix4:sub( CoordMatrix, CompactMatrix ),
							  SubMatrix ),


	14.0 = matrix4:get_element( _RowC=4, _ColC=2, SubMatrix ),
	SubSetMatrix = matrix4:set_element( RwC=4, ClC=3, 21.0, SubMatrix ),
	21.0 = matrix4:get_element( RwC, ClC, SubSetMatrix ),

	% Octave: TransposedCoordMatrix = [ 1, 5, 9, 13; 2, 6, 10, 14;
	%                                   3, 7, 11, 15; 4, 8, 12, 16 ]
	%
	TransposedCoordMatrix = matrix4:from_coordinates( 1.0, 5.0,  9.0, 13.0,
													  2.0, 6.0, 10.0, 14.0,
													  3.0, 7.0, 11.0, 15.0,
													  4.0, 8.0, 12.0, 16.0 ),

	TransposedCoordMatrix = matrix4:transpose( CoordMatrix ),
	true = matrix4:are_equal( TransposedCoordMatrix, TransposedCoordMatrix ),


	% Checked with octave, for example:
	%
	% CoordMatrix = [ 1, 2, 3, 4 ; 5, 6, 7, 8 ; 9, 10, 11, 12 ; 13, 14, 15, 16 ]
	% ScaledMatrix = 2.0 * CoordMatrix
	% RowMatrix = [ 10, 25, -7, 2; 1, 2, 4, 8.71; 0, 0, 0, 0;
	%  11, 27, -3, 10.71 ]

	MultCanCanMatrix = matrix4:from_coordinates( 112.0,  274.0,  -22.0, 142.52,
												 288.0,  706.0,  -70.0, 337.88,
												 464.0, 1138.0, -118.0, 533.24,
												 640.0, 1570.0, -166.0, 728.6 ),

	MultCanCanMatrix = matrix4:mult( ScaledMatrix, RowMatrix ),

	[ ArbitraryScaledMatrix, ArbitraryRowMatrix, ArbitraryMultCanCanMatrix ] =
		[ matrix:unspecialise( M )
			|| M <- [ ScaledMatrix, RowMatrix, MultCanCanMatrix ] ],

	ArbitraryMultCanCanMatrix =
		matrix:mult( ArbitraryScaledMatrix, ArbitraryRowMatrix ),

	test_facilities:display( "The multiplication of matrix ~ts "
		"by matrix ~ts yields: ~ts",
		[ matrix4:to_string( ScaledMatrix ),
		  matrix4:to_string( RowMatrix ),
		  matrix4:to_string( MultCanCanMatrix ) ] ),


	% Octave:
	% FirstCompactMatrix = [ 30, 12, 15, 55; 62, 42, 50, 11;
	%                        11, 39, 21, 19; 0, 0, 0, 1 ]
	FirstCompactMatrix = matrix4:from_compact_coordinates(
							30.0, 12.0, 15.0, 55.0,
							62.0, 42.0, 50.0, 11.0,
							11.0, 39.0, 21.0, 19.0 ),

	%test_facilities:display( "FirstCompactMatrix = ~ts",
	%                         [ matrix4:to_string( FirstCompactMatrix ) ] ),

	% Octave: SecondCompactMatrix = [ 59, 44, 24, 12; 97, 4, 56, 1;
	%                                 110, -4, 23, 9; 0, 0, 0, 1 ]
	SecondCompactMatrix = matrix4:from_compact_coordinates(
							59.0,  44.0, 24.0, 12.0,
							97.0,   4.0, 56.0,  1.0,
							110.0, -4.0, 23.0,  9.0 ),

	MultCptCptMatrix = matrix4:from_compact_coordinates(
							4584.0,  1308.0, 1737.0,  562.0,
						   13232.0,  2696.0, 4990.0, 1247.0,
							6742.0,   556.0, 2931.0,  379.0 ),

	true = matrix4:are_equal( MultCptCptMatrix,
					matrix4:mult( FirstCompactMatrix, SecondCompactMatrix ) ),

	% Octave:
	% MultCptCanMatrix = [ 1880, 2104, 2328, 2552; 1730, 2060, 2390, 2720;
	%                      1284, 1464, 1644, 1824; 26, 28, 30, 32 ]
	MultCptCanMatrix = matrix4:from_coordinates(
							1880.0, 2104.0, 2328.0, 2552.0,
							1730.0, 2060.0, 2390.0, 2720.0,
							1284.0, 1464.0, 1644.0, 1824.0,
							  26.0,   28.0,   30.0,   32.0 ),

	%test_facilities:display( "ScaledMatrix = ~ts",
	%						  [ matrix4:to_string( ScaledMatrix ) ] ),

	FirstMult = matrix4:mult( FirstCompactMatrix, ScaledMatrix ),

	test_facilities:display( "Comparing matrix ~ts with matrix ~ts",
		[ matrix4:to_string( MultCptCanMatrix ),
		  matrix4:to_string( FirstMult ) ] ),

	true = matrix4:are_equal( MultCptCanMatrix, FirstMult ),


	% MultCanCptMatrix = [ 374, 426, 356, 276; 1198, 1170, 1044, 964;
	%                      2022, 1914, 1732, 1652; 2846, 2658, 2420, 2340 ]
	MultCanCptMatrix = matrix4:from_coordinates(
							 374.0,  426.0,  356.0,  276.0,
							1198.0, 1170.0, 1044.0,  964.0,
							2022.0, 1914.0, 1732.0, 1652.0,
							2846.0, 2658.0, 2420.0, 2340.0 ),

	SecondMult = matrix4:mult( ScaledMatrix, FirstCompactMatrix ),

	test_facilities:display( "Comparing matrix ~ts with matrix ~ts",
		[ matrix4:to_string( MultCanCptMatrix ),
		  matrix4:to_string( SecondMult ) ] ),

	true = matrix4:are_equal( MultCanCptMatrix, SecondMult ),

	% As FirstCompactMatrix, ColMatrix, TransposedCoordMatrix and
	% MultCanCptMatrix are not invertible either (!):
	%
	% InvertibleCanMatrix4 = [4,3,7,4;1,9,7,3;17,5,19,77;16,6,19,83]
	InvertibleCanMatrix4 = matrix4:from_coordinates( 4.0,  3.0,  7.0,  4.0,
													 1.0,  9.0,  7.0,  3.0,
													 17.0, 5.0, 19.0, 77.0,
													 16.0, 6.0, 19.0, 83.0 ),

	-2562.0 = matrix4:determinant( InvertibleCanMatrix4 ),

	InversedCanMatrix4 = matrix4:inverse( InvertibleCanMatrix4 ),

	test_facilities:display( "The inverse of canonical matrix ~ts is: ~ts",
		[ matrix4:to_string( InvertibleCanMatrix4 ),
		  matrix4:to_string( InversedCanMatrix4 ) ] ),

	true = matrix4:are_equal( Id,
		matrix4:mult( InvertibleCanMatrix4, InversedCanMatrix4 ) ),

	true = matrix4:are_equal( Id,
		matrix4:mult( InversedCanMatrix4, InvertibleCanMatrix4 ) ),

	% From octave: inv(InvertibleCanMatrix4)
	InversedCanMatrix4Octave = matrix4:new( [
						   [ -0.5066354410616709, 0.2322404371584700,
							 1.176814988290398,  -1.075722092115535 ],
						   [ -0.4223263075722095, 0.2814207650273225,
							 0.5772833723653399, -0.5253708040593289 ],
						   [ 0.6213895394223266, -0.2486338797814208,
							 -0.8817330210772836, 0.7970335675253709 ],
						   [ -0.01405152224824354, -0.008196721311475415,
							 -0.06674473067915693, 0.07494145199063233 ] ] ),

	true = matrix4:are_equal( InversedCanMatrix4, InversedCanMatrix4Octave ),



	% InvertibleCptMatrix4 = [ 7, 11, 13, 17; 25, 2, 0, 27;
	%                         -7, 4, 0, -3; 0, 0, 0, 1 ]
	InvertibleCptMatrix4 = matrix4:from_compact_coordinates(
								 7.0, 11.0, 13.0, 17.0,
								25.0,  2.0,  0.0, 27.0,
								-7.0,  4.0,  0.0, -3.0 ),

	1482.0 = matrix4:determinant( InvertibleCptMatrix4 ),

	InversedCptMatrix4 = matrix4:inverse( InvertibleCptMatrix4 ),

	test_facilities:display( "The inverse of compact matrix ~ts is: ~ts",
		[ matrix4:to_string( InvertibleCptMatrix4 ),
		  matrix4:to_string( InversedCptMatrix4 ) ] ),

	true = matrix4:are_equal( Id,
		matrix4:mult( InvertibleCptMatrix4, InversedCptMatrix4 ) ),

	true = matrix4:are_equal( Id,
		matrix4:mult( InversedCptMatrix4, InvertibleCptMatrix4 ) ),

	% From octave: inv(InvertibleCptMatrix4)
	InversedCptMatrix4Octave = matrix4:from_compact_coordinates(
		0.0, 0.035087719298246, -0.017543859649123, -1.0,
		0.0, 0.061403508771930,  0.219298245614035, -1.0,
		0.076923076923077,      -0.070850202429150, -0.176113360323887,
														0.076923076923077 ),

	true = matrix4:are_equal( InversedCptMatrix4, InversedCptMatrix4Octave ),


	test_facilities:stop().
