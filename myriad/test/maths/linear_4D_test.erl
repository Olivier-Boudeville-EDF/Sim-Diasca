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


% Unit tests for the linear 4D facilities.
%
% See the linear_4D tested module.
%
-module(linear_4D_test).



% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),


	test_facilities:display( "~nTesting vectors first." ),

	NullVector = linear_4D:null_vector(),

	test_facilities:display( " Null vector is: ~ts",
							 [ linear_4D:to_string( NullVector ) ] ),

	V1 = { 9.0, 1.0, 0.0, 1.0 },

	test_facilities:display( " V1 is: ~ts", [ linear_4D:to_string( V1 ) ] ),

	ScaleFactor = 2.0,
	Vscale = { 18.0, 2.0, 0.0, 2.0 } = linear_4D:scale( V1, ScaleFactor ),

	test_facilities:display( " V1 scaled by ~p is: ~ts",
							 [ ScaleFactor, linear_4D:to_string( Vscale ) ] ),

	V1 = linear_4D:add( V1, NullVector ),

	V2 = { 10.0, 10.0, 5.0, 2.0 },
	test_facilities:display( " V2 is: ~ts", [ linear_4D:to_string( V2 ) ] ),

	Vsum = { 19.0, 11.0, 5.0, 3.0 } = linear_4D:add( V1, V2 ),

	test_facilities:display( " V1+V2 is: ~ts",
							 [ linear_4D:to_string( Vsum ) ] ),


	V3 = { 0.0, 0.0, 0.0, 3.0 },
	test_facilities:display( " V3 is: ~ts", [ linear_4D:to_string( V3 ) ] ),

	V4 = { 1.0, 2.0, 3.0, 4.0 },
	test_facilities:display( " V4 is: ~ts", [ linear_4D:to_string( V4 ) ] ),

	Vectors = [ V1, V2, V3, V4 ],

	{ 20.0, 13.0, 8.0, 10.0 } = Sum = linear_4D:add( Vectors ),

	test_facilities:display( " Sum of vectors V1, V2, V3 and V4 is ~ts.",
							 [ linear_4D:to_string( Sum ) ] ),


	test_facilities:display( "Testing matrices." ),

	NullMatrix = linear_4D:null_matrix(),

	test_facilities:display( " Null matrix is:~n~ts",
							 [ linear_4D:to_string( NullMatrix ) ] ),

	Id = linear_4D:identity(),

	test_facilities:display( " Identity matrix is:~n~ts",
							 [ linear_4D:to_string( Id ) ] ),

	ColMatrix = linear_4D:from_columns( V1, V2, V3, V4 ),

	test_facilities:display( " Matrix whose columns are V1, V2, V3 and V4 "
							 "is:~n~ts", [ linear_4D:to_string( ColMatrix ) ] ),

	RowMatrix = linear_4D:from_rows( V1, V2, V3, V4 ),

	test_facilities:display( " Matrix whose rows are V1, V2, V3 and V4 "
							 "is:~n~ts", [ linear_4D:to_string( RowMatrix ) ] ),


	CoordMatrix = linear_4D:from_coordinates( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
											  11, 12, 13, 14, 15, 16 ),

	test_facilities:display( " Matrix explicitly set from coordinates is:~n~ts",
							 [ linear_4D:to_string( CoordMatrix ) ] ),

	ScaledMatrix = linear_4D:scale( CoordMatrix, ScaleFactor ),

	test_facilities:display( " The previous matrix scaled by ~p is:~n~ts",
					 [ ScaleFactor, linear_4D:to_string( ScaledMatrix  ) ] ),

	ScaledMatrix = linear_4D:to_canonical( ScaledMatrix ),

	CompactIdMatrix = linear_4D:to_compact( Id ),

	test_facilities:display( " Compact version of identity is:~n~ts",
							 [ linear_4D:to_string( CompactIdMatrix ) ] ),

	% Checked with octave, for example:
	%
	% CoordMatrix = [ 1, 2, 3, 4 ; 5, 6, 7, 8 ; 9, 10, 11, 12 ; 13, 14, 15, 16 ]
	% ScaledMatrix = 2.0 * CoordMatrix
	% RowMatrix = [ 9, 1, 0, 1 ; 10, 10, 5, 2 ; 0, 0, 0, 3 ; 1, 2, 3, 4 ]
	% ScaledMatrix*RowMatrix
	% - [66,58,44,60;226,162,108,140;386,266,172,220;546,370,236,300]

	MultCanCanMatrix = linear_4D:mult( ScaledMatrix, RowMatrix ),

	test_facilities:display( " The multiplication of matrix~n~ts "
		"by matrix~n~ts yields:~n~ts",
		[ linear_4D:to_string( ScaledMatrix ),
		  linear_4D:to_string( RowMatrix ),
		  linear_4D:to_string( MultCanCanMatrix ) ] ),

	FirstCompactMatrix = linear_4D:from_coordinates( 30, 12, 15, 55,
								62, 42, 50, 11, 11, 39, 21, 19 ),

	SecondCompactMatrix = linear_4D:from_coordinates( 59, 44, 24, 12,
								97, 4, 56, 1, 110, -4, 23, 9 ),

	MultCptCptMatrix = linear_4D:from_coordinates( 4584, 1308, 1737, 562,
							13232, 2696, 4990, 1247, 6742, 556, 2931, 379 ),

	true = linear_4D:are_equal( MultCptCptMatrix,
					linear_4D:mult( FirstCompactMatrix, SecondCompactMatrix ) ),

	MultCptCanMatrix = linear_4D:from_coordinates( 1880, 2104, 2328, 2552,
		1730, 2060, 2390, 2720, 1284, 1464, 1644, 1824, 26, 28, 30, 32 ),

	true = linear_4D:are_equal( MultCptCanMatrix,
					linear_4D:mult( FirstCompactMatrix, ScaledMatrix ) ),

	MultCanCptMatrix = linear_4D:from_coordinates( 374, 426, 356, 276,
		1198, 1170, 1044, 964, 2022, 1914, 1732, 1652, 2846, 2658, 2420, 2340 ),

	true = linear_4D:are_equal( MultCanCptMatrix,
					linear_4D:mult( ScaledMatrix, FirstCompactMatrix ) ),

	test_facilities:stop().
