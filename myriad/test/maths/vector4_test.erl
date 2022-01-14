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
% Creation date: Sunday, October 10, 2021.


% @doc Unit tests for the <b>4D vector</b> facilities.
%
% See the vector4 tested module.
%
-module(vector4_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Comparisons are made of this specialised vector4 implementation with the one
% for arbitrary vectors (see vector.erl).
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	Null4D = [ 0.0, 0.0, 0.0, 0.0 ],

	Null4D = vector4:null(),

	X1 = [ 1.0, 0.0, 0.0, 0.0 ],
	X2 = [ 1, 0.0, 0, 0 ],

	V4D = vector4:new( X1 ),

	[ V4D = vector4:new( X ) || X <- [ X1, X2 ] ],


	IntP = { 10, 25, -7, 2 },

	V1 = [ 10.0, 25.0, -7.0, 2.0 ],
	V1 = vector4:from_point( IntP ),

	FloatP = { 1/4, 17.0, 5.0, 0.0 },
	VecP = tuple_to_list( FloatP ),

	FloatP = vector4:to_point( VecP ),

	test_facilities:display( "Base textual representation for V4D = ~w: ~ts",
							 [ V4D, vector4:to_string( V4D ) ] ),

	test_facilities:display( "Compact textual representation for V4D = ~w: ~ts",
							 [ V4D, vector4:to_compact_string( V4D ) ] ),

	test_facilities:display( "User-friendly textual representation "
		"for V4D = ~w: ~ts", [ V4D, vector4:to_user_string( V4D ) ] ),

	test_facilities:display( "User-friendly textual representation "
		"for VecP = ~w: ~ts", [ VecP, vector4:to_user_string( VecP ) ] ),

	test_facilities:display( "V1 = ~ts", [ vector4:to_string( V1 ) ] ),

	V2 = [ 1.0, 2.0, 4.0, 8.71 ],
	test_facilities:display( "V2 = ~ts", [ vector4:to_string( V2 ) ] ),

	V3 = [ 0.0, 0.0, 0.0, 3.0 ],

	V4 = [ 11.0, 27.0, -3.0, 10.71 ],

	V4 = vector4:add( V1, V2 ),

	test_facilities:display( "V4 = V1 + V2 = ~ts",
							 [ vector4:to_string( V4 ) ] ),

	Sum = vector4:add( [ V1, V2, V4 ] ),
	Sum = vector:add( [ V1, V2, V4 ] ),
	Sum = vector4:scale( V4, 2.0 ),

	Vectors = [ V1, V2, V3, V4 ],

	[ 22.0, 54.0, -6.0, 24.42 ] = Sum = vector4:add( Vectors ),

	Y1 = [ 0.0, 1.0, 0.0, 0.0 ],
	vector4:check( Y1 ),

	Z1 = vector4:new_integer( 0, 0, 1, 0 ),
	vector4:check_integer( Z1 ),

	% Not defined for 4D: Y1 = vector4:cross_product( Z1, X1 ),

	MinusV4 = [ -11.0, -27.0, 3.0, -10.71 ],
	MinusV4 = vector4:scale( V4, -1.0 ),

	true = math_utils:are_close( 973.7041, vector4:square_magnitude( V4 ) ),

	% Expected to be exact:
	1.0 = vector4:magnitude( X1 ),
	true = vector4:is_unitary( X1 ),

	UnitV4 = vector4:normalise( V4 ),

	test_facilities:display( "Unit V4 = ~ts", [ vector4:to_string( V4 ) ] ),

	true = math_utils:are_close( 1.0, vector4:magnitude( UnitV4 ) ),

	DP = vector4:dot_product( V1, V2 ),
	DP = vector:dot_product( V1, V2 ),
	test_facilities:display( "V1.V2 = ~w", [ DP ] ),
	DP = 49.42,

	test_facilities:stop().
