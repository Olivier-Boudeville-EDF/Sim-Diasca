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
% Creation date: Sunday, October 10, 2021.


% @doc Unit tests for the <b>2D vector</b> facilities.
%
% See the vector2 tested module.
%
-module(vector2_test).


% For run/0 export and al:
-include("test_facilities.hrl").



% Comparisons are made of this specialised vector2 implementation with the one
% for arbitrary vectors (see vector.erl).
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	Null2D = [ 0.0, 0.0 ],

	Null2D = vector2:null(),

	X1 = [ 1.0, 0.0 ],
	X2 = [ 1, 0.0 ],

	V2D = vector2:new( X1 ),

	[ V2D = vector2:new( X ) || X <- [ X1, X2 ] ],


	IntP = { 10, 25 },

	V1 = [ 10.0, 25.0 ],
	V1 = vector2:from_point( IntP ),

	FloatP = { 1/3, 17.0 },
	VecP = tuple_to_list( FloatP ),

	FloatP = vector2:to_point( VecP ),

	test_facilities:display( "Base textual representation for V2D = ~w: ~ts",
							 [ V2D, vector2:to_string( V2D ) ] ),

	test_facilities:display( "Compact textual representation for V2D = ~w: ~ts",
							 [ V2D, vector2:to_compact_string( V2D ) ] ),

	test_facilities:display( "User-friendly textual representation "
		"for V2D = ~w: ~ts", [ V2D, vector2:to_user_string( V2D ) ] ),

	test_facilities:display( "User-friendly textual representation "
		"for VecP = ~w: ~ts", [ VecP, vector2:to_user_string( VecP ) ] ),

	test_facilities:display( "V1 = ~ts", [ vector2:to_string( V1 ) ] ),

	V2 = [ 1.0, 2.0 ],
	test_facilities:display( "V2 = ~ts", [ vector2:to_string( V2 ) ] ),

	V3 = [ 11.0, 27.0 ],

	V3 = vector2:add( V1, V2 ),

	test_facilities:display( "V3 = V1 + V2 = ~ts",
							 [ vector2:to_string( V3 ) ] ),

	Sum = vector2:add( [ V1, V2, V3 ] ),
	Sum = vector:add( [ V1, V2, V3 ] ),
	Sum = vector2:scale( V3, 2.0 ),

	Y1 = [ 0.0, 1.0 ],
	vector2:check( Y1 ),

	X1int = vector2:new_integer( 1, 0 ),
	vector2:check_integer( X1int ),

	MinusV3 = [ -11.0, -27.0 ],
	MinusV3 = vector2:scale( V3, -1.0 ),

	true = math_utils:are_close( 850.0, vector2:square_magnitude( V3 ) ),

	1.0 = vector2:magnitude( X1 ),
	true = vector2:is_unitary( X1 ),

	UnitV3 = vector2:normalise( V3 ),

	test_facilities:display( "Unit V3 = ~ts", [ vector2:to_string( V3 ) ] ),

	true = math_utils:are_close( 1.0, vector2:magnitude( UnitV3 ) ),

	DP = vector2:dot_product( V1, V2 ),
	DP = vector:dot_product( V1, V2 ),
	test_facilities:display( "V1.V2 = ~w", [ DP ] ),
	DP = 60.0,

	V = [9,1],

	NL = vector2:normal_left( V ),
	NR = vector2:normal_right( V ),

	0 = vector2:dot_product( V, NL ),
	0 = vector2:dot_product( V, NR ),

	test_facilities:display( "The vector ~ts is a (non-unit) left normal "
		"for vector~ts, and ~ts is a (non-unit) right normal.",
		[ vector2:to_string( NL ), vector2:to_string( V ),
		  vector2:to_string( NR ) ] ),

	% See also linear_2D_test.erl for a longer test.

	test_facilities:stop().
