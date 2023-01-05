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


% @doc Unit tests for the <b>arbitrary matrix</b> facilities.
%
% See the matrix tested module.
%
-module(matrix_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	M = matrix:new( [ [ 0.0, 1.0, 2.0 ], [ 7777.0, 0.0, 1/3 ] ] ),

	test_facilities:display( "Base textual representation for ~w: ~ts",
							 [ M, matrix:to_string( M ) ] ),

	test_facilities:display( "Basic textual representation for ~w: ~ts",
							 [ M, matrix:to_basic_string( M ) ] ),

	test_facilities:display( "User-friendly textual representation "
		"for ~w: ~ts", [ M, matrix:to_user_string( M ) ] ),

	Null3x2 = [ [ 0.0, 0.0 ], [ 0.0, 0.0 ], [ 0.0, 0.0 ] ],

	Null3x2 = matrix:null( _RowCount=3, _ColumnCount=2 ),

	[ [ 1.0, 0.0 ], [ 0.0, 1.0 ] ] = matrix:identity( 2 ),

	{ 3, 2 } = matrix:dimensions( Null3x2 ),

	Dim = 5,

	Id = matrix:identity( Dim ),

	test_facilities:display( "Id(~B) = ~ts", [ Dim, matrix:to_string( Id ) ] ),


	% Like in matrix3_test.erl, but adapted:

	UnitRotVec = [ 1.0, 0.0, 0.0 ],

	RotAngle = math:pi() / 2,

	RotM = matrix:rotation( UnitRotVec, RotAngle ),
	test_facilities:display( "Rotation matrix of angle ~f radians "
		"around the following axis (unitary vector) ~ts is ~ts",
		[ RotAngle, vector:to_string( UnitRotVec ),
		  matrix:to_string( RotM ) ] ),

	X3DAxis = [ 1.0, 0.0, 0.0 ],
	Y3DAxis = [ 0.0, 1.0, 0.0 ],
	Z3DAxis = [ 0.0, 0.0, 1.0 ],

	% Transforms axes that way (check):

	% X unchanged:
	true = vector:are_equal( X3DAxis, matrix:apply( RotM, X3DAxis ) ),

	% Y to become Z:
	true = vector:are_equal( Z3DAxis, matrix:apply( RotM, Y3DAxis ) ),

	% Z to become -Y:
	MinusY3DAxis = vector:negate( Y3DAxis ),

	true = vector:are_equal( MinusY3DAxis, matrix:apply( RotM, Z3DAxis ) ),



	% Octave: M1 = [ 1, 2, 3; 4, 5, 6; 7, 8, 9 ]

	M1 = matrix:new( [ [ 1.0, 2.0, 3.0 ], [ 4.0, 5.0, 6.0 ],
					   [ 7.0, 8.0, 9.0 ] ] ),

	Columns = [ [ 1.0, 4.0, 7.0 ], [ 2.0, 5.0, 8.0 ], [ 3.0, 6.0, 9.0 ] ],
	M1 = matrix:from_columns( Columns ),

	Rows = M1,
	M1 = matrix:from_rows( Rows ),

	Coords = list_utils:flatten_once( Rows ),
	M1 = matrix:from_coordinates( Coords, _ColCount=3 ),
	Coords = matrix:to_coordinates( M1 ),

	Row = hd( Rows ),
	Row = matrix:row( 1, M1 ),

	Col = hd( Columns ),
	Col = matrix:column( 1, M1 ),

	8.0 = matrix:get_element( 3, 2, M1 ),

	NewM1 = matrix:set_element( 3, 2, 10.0, M1 ),
	test_facilities:display( "M1 = ~ts", [ matrix:to_string( M1 ) ] ),

	10.0 = matrix:get_element( 3, 2, NewM1 ),

	test_facilities:display( "NewM1 = ~ts", [ matrix:to_string( NewM1 ) ] ),

	TransposeM = [ [ 0.0, 7777.0 ], [ 1.0, 0.0 ], [2.0, 1/3 ] ],
	TransposeM = matrix:transpose( M ),

	test_facilities:display( "Transpose of M = ~ts is: ~ts",
		[ matrix:to_string( M ), matrix:to_string( TransposeM )] ),

	TransposeM = [ [ 0.0, 7777.0 ], [ 1.0, 0.0 ], [2.0, 1/3 ] ],
	TransposeM = matrix:transpose( M ),

	test_facilities:display( "Transpose of M = ~ts is: ~ts",
		[ matrix:to_string( M ), matrix:to_string( TransposeM )] ),

	DoubleM1 = matrix:new( [ [ 2.0, 4.0, 6.0 ], [ 8.0, 10.0, 12.0 ],
							 [ 14, 16, 18 ] ] ),

	DoubleM1 = matrix:scale( M1, 2.0 ),

	% Octave: M2 = [ 1, 0, 0; 0, 1, 0; 0, 0, 1 ]
	M2 = matrix:identity( 3 ),

	M3 = matrix:add( M1, M2 ),

	[ 7.0, 8.0, 10.0 ] = matrix:row( 3, M3 ),

	[ 2.0, 6.0, 8.0 ] = matrix:column( 2, M3 ),

	M1Minus6M2 = matrix:sub( M1, matrix:scale( M2, 6.0 ) ),
	M1Minus6M2 = [ [ -5.0, 2.0, 3.0 ], [ 4.0, -1.0, 6.0 ],
				   [ 7.0, 8.0, 3.0 ] ],

	test_facilities:display( "M1 = ~tsM2 = ~tsM3 = M1 + M2 = ~ts"
							 "M1 - 6.M2 = ~ts",
		[ matrix:to_string( M1 ), matrix:to_string( M2 ),
		  matrix:to_string( M3 ), matrix:to_string( M1Minus6M2 ) ] ),


	M1M2 = matrix:mult( M1, M2 ),

	M1 = M1M2,
	true = matrix:are_equal( M1, M1M2 ),

	% Octave: M4 = [ 11, -2, 5; 4, 8, 0; -7, 10, 1 ]
	M4 = matrix:new( [ [ 11.0, -2.0, 5.0 ], [ 4.0, 8.0, 0.0 ],
					   [ -7.0, 10.0, 1.0 ] ] ),
	test_facilities:display( "M4 = ~ts", [ matrix:to_string( M4 ) ] ),

	M5 = matrix:new( [ [ -2.0, 44.0, 8.0 ], [ 22.0, 92.0, 26.0 ],
					   [ 46.0, 140.0, 44.0 ] ] ),

	M5 = matrix:mult( M1, M4 ),
	test_facilities:display( "M5 = M1*M4 = ~ts", [ matrix:to_string( M5 ) ] ),

	UserCoords = [ [ 1.0, 9.0, 4.0, 5.0 ], [ 1.0, 8.0, 2.0, 7.0 ],
				   [ 4.0, 3.0, 7.0, 5.0 ], [ 6.0, 8.0, 2.0, 8.0 ] ],

	M6 = matrix:new( UserCoords ),
	matrix4 = matrix:get_specialised_module_for( M6 ),

	M7 = matrix4:new( UserCoords ),
	matrix4 = matrix:get_specialised_module_of( M7 ),

	M7 = matrix:specialise( M6 ),
	M6 = matrix:unspecialise( M7 ),

	matrix:check( M6 ),

	% Octave: InversibleCanMatrix4 = [  4, 3,  7,  4; 1,  9,  7,  3;
	%                                  17, 5, 19, 77; 16, 6, 19, 83 ]
	InversibleCanMatrix4 = matrix:new( [ [ 4.0,  3.0,  7.0,  4.0 ],
										 [ 1.0,  9.0,  7.0,  3.0 ],
										 [ 17.0, 5.0, 19.0, 77.0 ],
										 [ 16.0, 6.0, 19.0, 83.0 ] ] ),

	-2562.0 = matrix:determinant( InversibleCanMatrix4 ),

	InversedCanMatrix4 = matrix:inverse( InversibleCanMatrix4 ),

	test_facilities:display( "Inverse of ~ts is: ~ts",
		[ matrix:to_string( InversibleCanMatrix4),
		  matrix:to_string( InversedCanMatrix4 ) ] ),

	Id4 = matrix:identity( 4 ),

	true = matrix:are_equal( Id4,
		matrix:mult( InversibleCanMatrix4, InversedCanMatrix4 ) ),

	true = matrix:are_equal( Id4,
		matrix:mult( InversedCanMatrix4, InversibleCanMatrix4 ) ),

	% Octave: inv( InversibleCanMatrix4 ) returns (provided 'format long' has
	% been selected, otherwise matrices will not be equal):
	%
	OctaveInversedCanMatrix4 = [
		[ -5.066354410616709e-01, 2.322404371584700e-01,
		  1.176814988290398e+00, -1.075722092115535e+00 ],
		[ -4.223263075722095e-01, 2.814207650273225e-01,
		  5.772833723653399e-01, -5.253708040593289e-01 ],
		[ 6.213895394223266e-01, -2.486338797814208e-01,
		  -8.817330210772836e-01, 7.970335675253709e-01 ],
		[ -1.405152224824354e-02, -8.196721311475415e-03,
		  -6.674473067915693e-02, 7.494145199063233e-02 ] ],

	DiffInv = matrix:sub( InversedCanMatrix4, OctaveInversedCanMatrix4 ),

	test_facilities:display( "Differences between inverses is: ~ts",
							 [ matrix:to_string( DiffInv ) ] ),

	true = matrix:are_equal( InversedCanMatrix4, OctaveInversedCanMatrix4 ),

	test_facilities:stop().
