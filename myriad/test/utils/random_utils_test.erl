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
% Creation date: 2003


% Unit tests for the random utils toolbox.
%
% See the random_utils.erl tested module.
%
-module(random_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	random_utils:start_random_source( default_seed ),

	{ MinBound, MaxBound } = { 1, 15 },

	FirstList = [ random_utils:get_random_value( X )
				  || X <- lists:seq( MinBound, MaxBound ) ],

	test_facilities:display( "First random, reproducible list: ~w",
							 [ FirstList ] ),

	SecondList = [ random_utils:get_random_value( X, Y )
				   || X <- lists:seq( MinBound, 5 ),
					  Y <- lists:seq( X, MaxBound )  ],

	test_facilities:display( "Second random, reproducible list: ~w",
							 [ SecondList ] ),


	Min = 5,
	Max = 12,
	Count = 500,

	AnotherList = random_utils:get_random_values( Min, Max, Count ),

	case length( AnotherList ) of

		Count ->
			% All values (including bounds) should have been drawn at least
			% once (probably):
			[ true = lists:member( X, AnotherList )
			  || X <- lists:seq( Min, Max ) ];

		_ ->
			throw( probable_faulty_batched_ranged_generation )

	end,

	random_utils:stop_random_source(),

	test_facilities:display(
		"Current module being used as random source: ~w.",
		[ random_utils:get_random_module_name() ] ),

	test_facilities:display( "Restarting random source, this time with a "
							 "non-reproducible seeding." ),


	random_utils:start_random_source( time_based_seed ),

	ThirdList = random_utils:get_random_values( MaxBound, _Count=35 ),

	test_facilities:display( "Random, non-reproducible list between 1 and ~B "
							 "(both included): ~w", [ MaxBound, ThirdList ] ),

	random_utils:stop_random_source(),

	test_facilities:stop().
