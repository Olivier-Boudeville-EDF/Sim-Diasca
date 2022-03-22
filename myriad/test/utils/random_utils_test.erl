% Copyright (C) 2003-2022 Olivier Boudeville
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
% Creation date: 2003.


% @doc Unit tests for the <b>random-related</b> toolbox.
%
% See the random_utils.erl tested module.
%
-module(random_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing first uniform random sampling." ),

	random_utils:start_random_source( default_seed ),

	{ MinBound, MaxBound } = { 1, 15 },

	FirstList = [ random_utils:get_random_value( X )
					|| X <- lists:seq( MinBound, MaxBound ) ],

	test_facilities:display( "First random, uniform reproducible list: ~w",
							 [ FirstList ] ),

	SecondList = [ random_utils:get_random_value( X, Y )
					 || X <- lists:seq( MinBound, 5 ),
						Y <- lists:seq( X, MaxBound )  ],

	test_facilities:display( "Second random, uniform reproducible list: ~w",
							 [ SecondList ] ),


	Min = 5,
	Max = 12,
	Count = 500,

	AnotherList = random_utils:get_random_values( Min, Max, Count ),

	case length( AnotherList ) of

		Count ->
			% All values (including bounds) should have been drawn at least
			% once (probably):
			%
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

	test_facilities:display( "Random, uniform, non-reproducible list "
		"between 1 and ~B (both included): ~w", [ MaxBound, ThirdList ] ),

	random_utils:stop_random_source(),



	test_facilities:display( "Testing now non-uniform random sampling." ),

	BiasedCoinState = random_utils:generate_random_state_from(
		[ { obverse, 30 }, { reverse, 50 } ] ),

	SampleCount = 20,

	 test_facilities:display( "Generated ~B samples by flipping "
		"a biased coin: ~p",
		[ SampleCount,
		  random_utils:get_samples_from( SampleCount, BiasedCoinState ) ] ),


	test_facilities:display( "Creating a half-Gaussian half-affin "
		"probability density function and sampling from it." ),

	Mean = 50.0,
	Variance = 120.0,

	% Refer to https://en.wikipedia.org/wiki/Normal_distribution for more
	% details:
	%
	MyPDF = fun( X ) when X =< Mean ->
		80 *math:exp( - 1/2*math:pow( ( X - Mean ), 2 ) / Variance )
			/ ( math:sqrt( 2*math:pi() * Variance ) );

			   ( X ) ->
					erlang:max( 2.5 - 0.01*X, 1 )

			end,

	PDFPairs = math_utils:sample_as_pairs( MyPDF, 0, 100, 1 ),

	NormalisedPDFPairs = math_utils:normalise( PDFPairs, _ProbIndex=2 ),

	%trace_utils:debug_fmt( "Normalised PDF pairs: ~p",
	%                       [ NormalisedPDFPairs ] ),

	PDFDataFilename = "test_pdf_sampled_function.dat",
	SampleDataFilename = "test_pdf_actual_samples.dat",
	ComparisonOutputFilename = "test_pdf_sampled_function.png",

	CheckWithDataFile = not executable_utils:is_batch(),

	MaybeGnuplotPath = executable_utils:lookup_executable( "gnuplot" ),

	case CheckWithDataFile andalso MaybeGnuplotPath =/= false of

		true ->

			PDFSampleCount = 1000000,

			test_facilities:display( "Comparing the test PDF with the "
				"frequencies of ~B samplings.", [ PDFSampleCount ] ),

			file_utils:remove_files_if_existing( [ PDFDataFilename,
				SampleDataFilename, ComparisonOutputFilename ] ),

			csv_utils:write_file( NormalisedPDFPairs, PDFDataFilename ),

			% Already as a list of {X,Fun(X)}, i.e. {SampleValue,ProbOfValue}:
			AliasState = random_utils:generate_random_state_from( PDFPairs ),

			Samples = random_utils:get_samples_from( PDFSampleCount,
													  AliasState ),

			%trace_utils:debug_fmt( "Sample values: ~p", [ Samples ] ),

			ValueCountPairs = list_utils:get_duplicates( Samples ),

			%trace_utils:debug_fmt( "Value-count pairs: ~p",
			%                       [ ValueCountPairs ] ),

			NormalisedValueCountPairs = lists:sort(
				math_utils:normalise( ValueCountPairs, _FreqIndex=2 ) ),

			csv_utils:write_file( NormalisedValueCountPairs,
								  SampleDataFilename ),

			Args = [ "compare_test_pdf_sampling.p" ],

			case system_utils:run_executable( MaybeGnuplotPath, Args ) of

				{ _ReturnCode=0, Output } ->
					case Output of

						"" ->
							ok;

						_ ->
							trace_utils:warning_fmt( "Following output was "
								"returned by gnuplot: ~ts", [ Output ] )

					end,

					executable_utils:display_png_file(
						"test_pdf_sampled_function.png" );


				{ ErrorCode, Output } ->
					trace_utils:error_fmt( "Error ~B reported by gnuplot: ~ts",
										   [ ErrorCode, Output ] ),

					throw( plot_generation_failed )

			end;

		false ->
			ok

	end,

	test_facilities:stop().
