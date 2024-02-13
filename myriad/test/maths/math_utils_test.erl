% Copyright (C) 2010-2024 Olivier Boudeville
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
% Creation date: 2010.


% @doc Unit tests for the <b>math-related basic toolbox</b> facilities.
%
% See the math_utils tested module.
%
-module(math_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Silencing:
-export([ test_basics/0, test_sampling/0, test_function_support/0,
		  test_rectangular_level_function_support/0,
		  test_amortised_sinus_function_support/0 ]).



test_basics() ->

	Roundings = [ -1.1, -1.0, -0.9, 0.0, 0.9, 1.0, 1.1 ],

	% Not used anymore, as erlang:floor/1 has been introduced since then:
	%[ test_facilities:display( "Floor for ~p is ~p.",
	%  [ V, math_utils:floor(V) ] ) || V <- Roundings ],

	[ test_facilities:display( "Ceiling for ~p is ~p.",
		[ V, math_utils:ceiling(V) ] ) || V <- Roundings ],

	TruncateTargets = [ -12345.6789, -1.23456789, 0.0, 12.3456789, 123.4568 ],

	[
	 [ test_facilities:display( "Rounding ~p after ~B digit(s) is ~p.",
		[ V, D,
		  math_utils:round_after( V, D ) ] ) || V <- TruncateTargets ]
					|| D <- [ 0, 1, 2, 3 ] ],


	Modulo = 3,
	[ test_facilities:display( "~p modulo ~p is ~p.", [ X, Modulo,
		math_utils:modulo( X, Modulo ) ] ) || X <- lists:seq(-7,7) ],

	2 = math_utils:clamp( 1, 3, 2 ),
	1 = math_utils:clamp( 1, 3, 0 ),
	3 = math_utils:clamp( 1, 3, 6 ),

	2.0 = math_utils:clamp( 1, 3, 2.0 ),
	1   = math_utils:clamp( 1, 3, 0.0 ),
	3   = math_utils:clamp( 1, 3, 6.0 ),

	2   = math_utils:clamp( 1.0, 3.0, 2 ),
	1.0 = math_utils:clamp( 1.0, 3.0, 0 ),
	3.0 = math_utils:clamp( 1.0, 3.0, 6 ),

	8 = math_utils:get_next_power_of_two( 5 ),
	8 = math_utils:get_next_power_of_two( 8 ),

	[ test_facilities:display( "Canonical form for ~p° is ~p°.",
							   [ A, math_utils:canonify(A) ] )
		|| A <- [ -721, -721.0, -720, -720.0, -719, -719.0, -100, -100.0,
				  0, 0.0, 100, 100.0, 359, 359.0, 360, 360.0, 361, 361.0,
				  400, 400.0 ] ],


	X1 = 300000.0,
	X2 = 300000.1,
	X3 = 300000.0000000000001,
	Y  = 300000.0,


	% Only the result of the fourth test should make a difference between
	% absolute/relative comparisons:

	true  = math_utils:are_close( X1, Y ),
	false = math_utils:are_close( X1, 0 ),
	true  = math_utils:are_close( X3, Y ),
	false = math_utils:are_close( X2, Y ),

	true  = math_utils:are_relatively_close( X1, Y ),
	false = math_utils:are_relatively_close( X1, 0 ),
	true  = math_utils:are_relatively_close( X3, Y ),
	true  = math_utils:are_relatively_close( X2, Y ),

	[ test_facilities:display( "Angle ~p rad is ~f°.", [ Angle,
			math_utils:radians_to_degrees( Angle ) ] )
		|| Angle <- [ 0, math:pi()/2, 1.0, math:pi(), 2*math:pi() ] ].



test_sampling() ->

	AffinFun = fun( X ) -> 2*X + 5 end,

	Pairs = math_utils:sample_as_pairs( AffinFun, _Start=10.0, _Stop=16.0,
										_Inc=3.0 ),

	test_facilities:display( "Sampled pairs for affin test function: ~p",
							 [ Pairs ] ),

	DataFilePath = "test-affin.dat",

	WriteDataFile = false,

	WriteDataFile andalso
		begin
			file_utils:remove_file_if_existing( DataFilePath ),
			csv_utils:write_file( Pairs, DataFilePath )
		end,

	Samples = math_utils:sample( AffinFun, _From=1.0, _To=20.0, _Incr=2.0 ),

	test_facilities:display( "Samples for affin test function: ~p",
							 [ Samples ] ).



test_function_support() ->
	%test_rectangular_level_function_support(),
	test_amortised_sinus_function_support().


% A rather easy case:
test_rectangular_level_function_support() ->

	test_facilities:display( "Testing a rectangular level function." ),

	LevelFun = fun( X ) when X >= 0.0 andalso X < 1.0 -> 1.0;
				  ( _X ) -> 0.0
			   end,

	LevelBounds = math_utils:compute_support( LevelFun ),

	test_facilities:display( "Support of level function: ~ts",
							 [ math_utils:bounds_to_string( LevelBounds ) ] ).



% A somewhat worst-case scenario case:
test_amortised_sinus_function_support() ->

	test_facilities:display( "Testing an amortised sinus function." ),

	% With Octave:

	% function retval = amort_sin(x)
	%   retval = sin(x) .* exp(-abs(x/50))
	% endfunction
	% xs = -500:500
	% ys = amort_sin(xs)
	% plot(xs,ys)

	AmortisedSinFun = fun( X ) ->
						math:sin( X ) * math:exp( -abs( X/50.0 ) )
					  end,

	ASinBounds = math_utils:compute_support( AmortisedSinFun ),

	test_facilities:display( "Support of amortised sinus function: ~ts",
							 [ math_utils:bounds_to_string( ASinBounds ) ] ).


test_gamma() ->

	test_facilities:display( "Testing the Gamma function." ),

	% Checked for example with:
	%
	% octave:1> gamma(10)
	% ans = 362880

	1      = math_utils:gamma( 1 ),
	24     = math_utils:gamma( 5 ),
	362880 = math_utils:gamma( 10 ),

	math_utils:are_equal( math_utils:gamma( 2.1 ), 1.0465 ),
	math_utils:are_equal( math_utils:gamma( 4.8 ), 17.838 ),
	math_utils:are_equal( math_utils:gamma( 10.7 ), 1.7998e+06 ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_basics(),

	test_sampling(),

	test_function_support(),

	test_gamma(),

	test_facilities:stop().
