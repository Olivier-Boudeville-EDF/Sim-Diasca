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
% Creation date: Sunday, September 26, 2021.


% @doc Unit tests for the <b>arbitrary vector</b> facilities.
%
% See the vector tested module.
%
-module(vector_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	Null2D = [ 0.0, 0.0 ],

	Null2D = vector:null( _Dim=2 ),

	X1 = [ 1.0, 0.0, 0.0 ],
	X2 = [ 1, 0.0, 0 ],

	V3D = vector:new( X1 ),

	[ V3D = vector:new( X ) || X <- [ X1, X2 ] ],

	IntP = { 10, 25, -7 },

	V1 = [ 10.0, 25.0, -7.0 ],
	V1 = vector:from_point( IntP ),
	3 = vector:dimension( V1 ),

	FloatP = { 1/3, 17.0 },
	VecP = tuple_to_list( FloatP ),
	FloatP = vector:to_point( VecP ),

	test_facilities:display( "Base textual representation for V3D = ~w: ~ts",
							 [ V3D, vector:to_string( V3D ) ] ),

	test_facilities:display( "Compact textual representation for V3D = ~w: ~ts",
							 [ V3D, vector:to_compact_string( V3D ) ] ),

	test_facilities:display( "User-friendly textual representation "
		"for V3D = ~w: ~ts", [ V3D, vector:to_user_string( V3D ) ] ),

	test_facilities:display( "User-friendly textual representation "
		"for VecP = ~w: ~ts", [ VecP, vector:to_user_string( VecP ) ] ),

	test_facilities:display( "V1 = ~ts", [ vector:to_string( V1 ) ] ),

	V2 = [ 1.0, 2.0, 3.0 ],
	test_facilities:display( "V2 = ~ts", [ vector:to_string( V2 ) ] ),

	V3 = [ 11.0, 27.0, -4.0 ],

	V3 = vector:add( V1, V2 ),

	test_facilities:display( "V3 = V1 + V2 = ~ts",
							 [ vector:to_string( V3 ) ] ),

	MinusV3 = [ -11.0, -27.0, 4.0 ],
	MinusV3 = vector:scale( V3, -1.0 ),

	true = math_utils:are_close( 866.0, vector:square_magnitude( V3 ) ),

	% Expected to be exact:
	1.0 = vector:magnitude( X1 ),
	true = vector:is_unitary( X1 ),

	UnitV3 = vector:normalise( V3 ),

	test_facilities:display( "Unit V3 = ~ts", [ vector:to_string( V3 ) ] ),

	true = math_utils:are_close( 1.0, vector:magnitude( UnitV3 ) ),


	[ 11.0, 54.0, -12.0 ] = vector:mult( V2, V3 ),

	DP = vector:dot_product( V1, V2 ),
	test_facilities:display( "V1.V2 = ~w", [ DP ] ),
	DP = 39.0,

	test_facilities:stop().
