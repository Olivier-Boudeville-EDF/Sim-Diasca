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


% @doc Unit tests for the <b>specialised 2x2 matrix</b> facilities.
%
% See the matrix2 tested module.
%
-module(matrix2_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing 2x2 matrices." ),

	NullMatrix = matrix2:null(),

	test_facilities:display( "Null matrix is: ~ts",
							 [ matrix2:to_string( NullMatrix ) ] ),

	Dim = 2,

	Id = matrix2:identity(),
	true = matrix:are_equal( matrix:unspecialise( Id ),
							 matrix:identity( Dim ) ),

	test_facilities:display( "Identity matrix is: ~ts",
							 [ matrix2:to_string( Id ) ] ),



	RotAngle = math:pi() / 2,
	Deg90Rot = matrix2:rotation( RotAngle ),

	test_facilities:display( "Rotation matrix of angle ~f radians is: ~ts",
							 [ RotAngle, matrix2:to_string( Id ) ] ),

	% Transforms the abscissa axis into the ordinate one:
	X = [ 1.0, 0.0 ],
	Y = [ 0.0, 1.0 ],

	% Check:
	true = vector2:are_equal( Y, matrix2:apply( Deg90Rot, X ) ),



	V1 = [ 10.0, 25.0 ],
	V2 = [ 1.0, 2.0 ],

	ColMatrix = matrix2:from_columns( V1, V2 ),
	V2 = matrix2:column( 2, ColMatrix ),

	test_facilities:display( "Matrix whose columns are V1 and V2 "
							 "is: ~ts", [ matrix2:to_string( ColMatrix ) ] ),

	% Octave: RowMatrix = [ 10, 25; 1, 2 ]
	RowMatrix = matrix2:from_rows( V1, V2 ),

	V2 = matrix2:row( 2, RowMatrix ),

	test_facilities:display( "Matrix whose rows are V1 and V2 "
							 "is: ~ts", [ matrix2:to_string( RowMatrix ) ] ),


	% Octave: CoordMatrix = [ 10, 20; 30, 40 ]
	CoordMatrix = matrix2:from_coordinates( 10.0, 20.0,
											30.0, 40.0 ),

	test_facilities:display( "Matrix explicitly set from coordinates is: ~ts",
							 [ matrix2:to_string( CoordMatrix ) ] ),

	% Octave: Matrix2 = [ 1, 2; 5, 6 ]
	Matrix2 = matrix2:new( [ [ 1.0,  2.0 ],
							 [ 5.0,  6.0 ] ] ),

	ScaleFactor = 2.0,

	ScaledMatrix = matrix2:scale( CoordMatrix, ScaleFactor ),

	test_facilities:display( "The previous matrix scaled by ~p is: ~ts",
						[ ScaleFactor, matrix2:to_string( ScaledMatrix ) ] ),

	ScaledMatrix = matrix2:to_canonical( ScaledMatrix ),


	SubMatrix = matrix2:new( [ [  9.0, 18.0 ],
							   [ 25.0, 34.0 ] ] ),

	% Tests scale/2 and add/2 as well:
	true = matrix2:are_equal( matrix2:sub( CoordMatrix, Matrix2 ),
							  SubMatrix ),


	18.0 = matrix2:get_element( _RowC=1, _ColC=2, SubMatrix ),
	SubSetMatrix = matrix2:set_element( RwC=2, ClC=1, 21.0, SubMatrix ),
	21.0 = matrix2:get_element( RwC, ClC, SubSetMatrix ),

	TransposedCoordMatrix = matrix2:from_coordinates(  10.0, 30.0,
													   20.0, 40.0 ),

	TransposedCoordMatrix = matrix2:transpose( CoordMatrix ),
	true = matrix2:are_equal( TransposedCoordMatrix, TransposedCoordMatrix ),


	% Checked with octave, for example:
	%
	% CoordMatrix = [ 1, 2 ; 5, 6 ]
	% ScaledMatrix = 2.0 * CoordMatrix
	% RowMatrix = [ 10, 25, -7; 1, 2 4; 0, 0, 5 ]

	MultCanMatrix = matrix2:from_coordinates(  240.0,  580.0,
											   680.0, 1660.0 ),

	MultCanMatrix = matrix2:mult( ScaledMatrix, RowMatrix ),

	test_facilities:display( "The multiplication of matrix ~ts "
		"by matrix ~ts yields: ~ts",
		[ matrix2:to_string( ScaledMatrix ),
		  matrix2:to_string( RowMatrix ),
		  matrix2:to_string( MultCanMatrix ) ] ),

	[ ArbitraryScaledMatrix, ArbitraryRowMatrix, ArbitraryMultCanMatrix ] =
		[ matrix:unspecialise( M )
			|| M <- [ ScaledMatrix, RowMatrix, MultCanMatrix ] ],

	ArbitraryMultCanMatrix =
		matrix:mult( ArbitraryScaledMatrix, ArbitraryRowMatrix ),

	-200.0 = matrix2:determinant( CoordMatrix ),

	InvCoordMatrix = matrix2:inverse( CoordMatrix ),

	test_facilities:display( "The inverse of matrix ~ts is: ~ts",
		[ matrix2:to_string( CoordMatrix ),
		  matrix2:to_string( InvCoordMatrix ) ] ),

	true = matrix2:are_equal( Id,
		matrix2:mult( CoordMatrix, InvCoordMatrix ) ),

	true = matrix2:are_equal( Id,
		matrix2:mult( InvCoordMatrix, CoordMatrix ) ),

	% Octave: inv(CoordMatrix) = [ 1, 2 ; 5, 6 ]
	InvCoordMatrixOctave = matrix2:from_coordinates( -0.2, 0.1, 0.15, -0.05 ),

	true = matrix2:are_equal( InvCoordMatrix, InvCoordMatrixOctave ),

	test_facilities:stop().
