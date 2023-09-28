% Copyright (C) 2007-2023 Olivier Boudeville
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
% Creation date: 2007.


% @doc Unit tests for the <b>random-related</b> toolbox.
%
% See the random_utils.erl tested module.
%
-module(random_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For epsilon:
-include_lib("myriad/include/math_utils.hrl").


% For testing:
-export([ gather_samples_in_buckets/2 ]).


% Silencing:
-export([ test_basic_random/0, test_biased_coin/0,
		  test_uniform/0, test_exponential/0, test_gamma/0, test_gumbel/0,
		  test_loglogistic/0, test_lognormal/0,
		  test_gaussian/0, test_weibull/0, test_beta/0,
		  test_custom_pdf/0 ]).


% Refer to the design notes in random_utils to understand why a gathering of
% samples in a limited number of buckets is required, especially for
% distributions that can be directly sampled.
%
% Note also that the two sampling rates (discretisation of the PDF / gathering
% in buckets) must be the same, otherwise the sampling count will differ and a
% scaling will exist between the two curves.
%
% Finally, as the default sampling count for uniform distributions is fixed
% (see, in random_utils, the default_pdf_sample_count define), to avoid again an
% undesirable scaling, they shall match as well, so:
%
%-define( test_sample_count, 512 ).
-define( test_sample_count, 1024 ).
%-define( test_sample_count, 4096 ).



% Shorthands:

-type count() :: basic_utils:count().

-type random_law_spec() :: random_utils:random_law_spec().
-type random_law_data() :: random_utils:random_law_data().

-type sample() :: random_utils:sample().



test_basic_random() ->

	test_facilities:display( "Testing first uniform random sampling." ),

	% Reproducible (deterministic seeding):
	random_utils:start_random_source( default_seed ),

	{ MinBound, MaxBound } = { 1, 15 },

	FirstList = [ random_utils:get_uniform_value( X )
					|| X <- lists:seq( MinBound, MaxBound ) ],

	test_facilities:display( "First random, uniform reproducible list: ~w",
							 [ FirstList ] ),

	SecondList = [ random_utils:get_uniform_value( X, Y )
					|| X <- lists:seq( MinBound, 5 ),
					   Y <- lists:seq( X, MaxBound ) ],

	test_facilities:display( "Second random, uniform reproducible list: ~w",
							 [ SecondList ] ),


	Min = 5,
	Max = 12,
	Count = 500,

	AnotherList = random_utils:get_uniform_values( Min, Max, Count ),

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

	ThirdList = random_utils:get_uniform_values( MaxBound, _Count=35 ),

	test_facilities:display( "Random, uniform, non-reproducible list "
		"between 1 and ~B (both included): ~w", [ MaxBound, ThirdList ] ),

	random_utils:stop_random_source().



% This is a simple yet interesting test of an intrinsically discrete
% distribution:
%
test_biased_coin() ->

	% Front/back sides of coins:
	BiasedCoinLawSpec = { arbitrary, "My law based on an unfair coin",
		% Wanting to graph it easily, so xtics shall better be numerical:
		%
		% (otherwise: 'set xtics ("obverse" 1, "inverse" 2)', and 1 and 2 shall
		% be used in datafiles)
		%
		%[ { obverse, 30 }, { reverse, 50 } ] },
		[ { _Obverse=1, 30 }, { _Reverse=2, 50 } ] },

	test_law( BiasedCoinLawSpec ).



test_uniform() ->
	UniformSpec = { uniform, _Minf=3, _Maxf=9 },
	test_law( UniformSpec ),

	IntegerUniformSpec = { integer_uniform, _Min=-10, _Max=5 },
	test_law( IntegerUniformSpec ).



test_exponential() ->
	Exp1pLawSpec = { exponential_1p, _Lambda1p=0.2 },
	test_law( Exp1pLawSpec ),

	IntegerExp1pLawSpec = { positive_integer_exponential_1p, _IntLambda1p=0.1 },
	test_law( IntegerExp1pLawSpec ),

	Exp2pLawSpec = { exponential_2p, _Lambda2p=0.8, _Gamma2p=0.1 },
	test_law( Exp2pLawSpec ).



test_gamma() ->

	% Reasonable values:
	Gam2pLawSpec = { gamma_2p, _K=5, _Theta=1 },
	test_law( Gam2pLawSpec ).

	% More problematic ones:
	%Gam3pLawSpec = { gamma_3p, _AlphaH=650.0, _Beta=0.51, _Theta=0.06 },
	%test_law( Gam3pLawSpec ).



test_gaussian() ->
	GausLawSpec = { gaussian, _Muf=2, _Sigmaf=0.4 },
	test_law( GausLawSpec ),

	IntegerGausLawSpec = { positive_integer_gaussian, _Mu=70, _Sigma=8 },
	test_law( IntegerGausLawSpec ).



test_gumbel() ->
	GumLawSpec = { gumbel_2p, _Mu=3, _Beta=4 },
	test_law( GumLawSpec ).



test_loglogistic() ->
	Log2pLawSpec = { loglogistic_2p, _Alpha2p=3, _Beta2p=4 },
	test_law( Log2pLawSpec ),

	Log3pLawSpec = { loglogistic_3p, _Alpha3p=3, _Beta3p=4, _Theta3p=1 },
	test_law( Log3pLawSpec ).



test_lognormal() ->
	Log2pLawSpec = { lognormal_2p, _Mu2p=0, _Sigma2p=0.25 },
	test_law( Log2pLawSpec ),

	Log3pLawSpec = { lognormal_3p, _Mu3p=0.5, _Sigma3p=0.4, _Theta3p=1.2 },
	test_law( Log3pLawSpec ).



test_weibull() ->

	% Default sample count and support:
	WbLaw2pSpec = { weibull_2p, _K2p=1.5, _Lambda2p=1 },
	test_law( WbLaw2pSpec ),

	WbLaw3pSpec = { weibull_3p, _K3p=2.1, _Lambda3p=2, _Gamma3p=10 },
	test_law( WbLaw3pSpec ),

	WbCrLawSpec = { weibull_cr, _LambdaCr=1.1, _KCr=1.2, _ThetaCr=4 },
	test_law( WbCrLawSpec ),

	WbDsLawSpec = { weibull_ds, _LambdaDs=2.1, _KDs=0.2, _SigmaDs=3 },
	test_law( WbDsLawSpec ),

	% Parameters could be also taken from
	% https://reliability.readthedocs.io/en/latest/DSZI%20models.html?highlight=DSZI#example-1;
	% then the approximation seems a lot less good:
	%
	%WbDsziLawSpec = { weibull_dszi, _LambdaDszi=50, _KDszi=2,
	%				  _SigmaDszi=0.8, _ThetaDszi=0.3 },

	WbDsziLawSpec = { weibull_dszi, _LambdaDszi=10, _KDszi=1,
					  _SigmaDszi=0.8, _ThetaDszi=0.3 },
	test_law( WbDsziLawSpec ),

	WbMixLawSpec = { weibull_mixture, _PMix=0.3, _LambdaMix1=1.0, _KMix1=5.0,
					 _LambdaMix2=1.7, _KMix2=4.2 },
	test_law( WbMixLawSpec ),

	WbZiLawSpec = { weibull_zi, _LambdaZi=0.4, _KZi=1.2, _PZi=4.3 },
	test_law( WbZiLawSpec ).


test_beta() ->
	Beta2pSpec = { beta_2p, _Alpha=2, _Beta=5 },
	test_law( Beta2pSpec ).



test_custom_pdf() ->

	test_facilities:display( "Creating a half-Gaussian half-affin "
		"probability density function and sampling from it." ),

	Mean = 50.0,
	Variance = 120.0,

	% We define an example PDF here.
	%
	% Refer to https://en.wikipedia.org/wiki/Normal_distribution for more
	% details:
	%
	MyPDF = fun( X ) when X =< Mean ->

		80 *math:exp( - 1/2*math:pow( ( X - Mean ), 2 ) / Variance )
			/ ( math:sqrt( 2*math:pi() * Variance ) );

			   ( X ) ->
					erlang:max( 2.5 - 0.01*X, 0.0 )

			end,

	% We could specify in the law spec no bounds and let the corresponding
	% support be automatically determined (this would be [-9.769263,
	% 249.999901]), however in some cases it may be convenient to force some
	% bounds (e.g. if a PDF represents durations, we may not want to be able to
	% draw negative ones):

	% Non-negative:
	OurMinBound = 0.0,

	% Hence support is truncated:
	OurMaxBound = 240.0,

	OurPDFBounds = { OurMinBound, OurMaxBound },

	% We can set explicitly here the number of samples to be done to a low
	% value, for testing:
	%
	SampleCount = ?test_sample_count,

	MyPDFInfo = { MyPDF, SampleCount, OurPDFBounds },

	MyLawSpec = { arbitrary, "My PDF-based law spec", MyPDFInfo },

	test_law( MyLawSpec ).



% @doc Tests the specified random law, based on its specification.
-spec test_law( random_law_spec() ) -> void().
test_law( LawSpec ) ->

	test_facilities:display( "Testing a(n) ~ts.",
		[ random_utils:law_spec_to_string( LawSpec ) ] ),

	LawData = random_utils:initialise_law( LawSpec ),

	SampleCount = 20,

	test_facilities:display( "Initialised as ~ts, generating ~B samples "
		"from it:~n  ~w",
		[ random_utils:law_data_to_string( LawData ), SampleCount,
		  random_utils:get_samples_from( SampleCount, LawData ) ] ),

	executable_utils:is_batch() orelse graph_law( LawSpec, LawData ).



% @doc Graphs the specified random law.
-spec graph_law( random_law_spec(), random_law_data() ) -> void().
graph_law( LawSpec, LawData ) ->

	test_facilities:display( "Creating a graph for ~ts.",
							 [ random_utils:law_data_to_string( LawData ) ] ),

	% With Octave, refer to LAW_NAME.m as an example, to be run thanks to:
	% 'octave LAW_NAME.m'.

	LawDesc = random_utils:law_data_to_string( LawData ),

	LawSuffixStr = file_utils:convert_to_filename( LawDesc ),

	PDFDataFilename = text_utils:format( "test_~ts_pdf_sampled_function.dat",
										 [ LawSuffixStr ] ),

	SampleDataFilename = text_utils:format( "test_~ts_pdf_actual_samples.dat",
											[ LawSuffixStr ] ),

	PNGOutputFilename = text_utils:format(
		"test_~ts_pdf_sampled_function_comparison.png", [ LawSuffixStr ] ),

	CmdFilename = text_utils:format(
		"compare_test_pdf_sampling_for_~ts.p", [ LawSuffixStr ] ),

	MaybeGnuplotPath = executable_utils:lookup_executable( "gnuplot" ),

	MaybeGnuplotPath =/= false andalso
		begin

			file_utils:remove_files_if_existing( [ PDFDataFilename,
				SampleDataFilename, PNGOutputFilename, CmdFilename ] ),

			% First, just capture the ideal law as it is:
			PDFPairs = random_utils:get_all_sample_pairs( LawSpec ),

			% Very theoretical, as depends on the discretisation step:
			NormalisedPDFPairs = math_utils:normalise( PDFPairs, _Index=2 ),

			csv_utils:write_file( NormalisedPDFPairs, PDFDataFilename ),


			% Now seeing whether drawings converge to it:
			PDFSamplingCount = 1000000,
			%PDFSamplingCount =   500000,
			%PDFSamplingCount =    10000,

			test_facilities:display( "Comparing the test PDF with the "
				"frequencies of ~B samplings.", [ PDFSamplingCount ] ),

			GnuplotCmdFileTemplate = "compare_test_pdf_sampling.p.template",

			WordWrappedLawDesc = text_utils:join( _Sep="\\n",
				text_utils:format_text_for_width( LawDesc, _Width=80,
												  _DoPad=false ) ),

			TranslationTable = table:new( [
				{ "LAW_DESC", WordWrappedLawDesc },
				{ "PDF_DATA_FILENAME", PDFDataFilename },
				{ "SAMPLED_DATA_FILENAME", SampleDataFilename },
				{ "GENERATED_PNG", PNGOutputFilename } ] ),

			% A copy and a transformation:
			file_utils:update_with_keywords(
				_OriginalFilePath=GnuplotCmdFileTemplate,
				_TargetFilePath=CmdFilename, TranslationTable ),

			RawSamples =
				random_utils:get_samples_from( PDFSamplingCount, LawData ),

			% To check that any artifact in the drawn data (brief returns to
			% zero) are due to the bucket aggregation, not to the sample
			% generation per se:
			%
			%TestRawFilename = "test-raw.dat",

			%file_utils:remove_file_if_existing( TestRawFilename ),

			%csv_utils:write_file( lists:sort(
			%   list_utils:count_occurrences( RawSamples ) ), TestRawFilename ),

			% We force aggregation in a sufficiently low number of buckets,
			% otherwise direct (non-discretised) distributions yielding
			% floating-point values would be seen as uniform (as any
			% floating-point value would never be drawn more than once):
			%
			AggSamples = gather_samples_in_buckets( RawSamples,
													?test_sample_count ),


			% Already as a list of {X,Fun(X)}, i.e. {SampleValue,ProbOfValue}:
			%trace_utils:debug_fmt( "Aggregated Sample values: ~p",
			%                       [ AggSamples ] ),

			% Previously no discretised gathering was done (zero-width buckets),
			% leading to all distributions being rendered as uniform:
			% (hence to be used if not gathering samples)
			%ValueCountPairs = list_utils:count_occurrences( RawSamples ),

			ValueCountPairs = AggSamples,

			%trace_utils:debug_fmt( "Value-count pairs: ~p",
			%                       [ ValueCountPairs ] ),

			% Normalising is useful so that the two curves are of the same
			% scale:
			%
			NormalisedValueCountPairs = lists:sort(
				math_utils:normalise( ValueCountPairs, _FreqIndex=2 ) ),

			%NormalisedValueCountPairs = lists:sort( ValueCountPairs ),

			csv_utils:write_file( NormalisedValueCountPairs,
								  SampleDataFilename ),

			Args = [ CmdFilename ],

			case system_utils:run_executable( MaybeGnuplotPath, Args ) of

				{ _ReturnCode=0, Output } ->
					Output =:= "" orelse
						trace_utils:warning_fmt( "Following output was "
							"returned by gnuplot: ~ts", [ Output ] ),

					executable_utils:display_png_file( PNGOutputFilename ),

					trace_utils:debug_fmt( "(displaying '~ts')",
										   [ PNGOutputFilename ] );

				{ ErrorCode, Output } ->
					trace_utils:error_fmt( "Error ~B reported by gnuplot: ~ts",
										   [ ErrorCode, Output ] ),

					throw( plot_generation_failed )

			end

	end.



% @doc Gathers the specified samples in evenly-spaced buckets: aggregates them
% in coarser "counter" slots.
%
% Otherwise each individual sample would be produced once, resulting in a
% falsely-uniform probability density function.
%
-spec gather_samples_in_buckets( [ sample() ], count() ) ->
			[ { sample(), count() } ].
gather_samples_in_buckets( Samples, BucketCount ) ->

	% Probably not really efficient:

	% Both bounds included, hence Max-Min buckets:
	{ Min, Max } = list_utils:get_min_max( Samples ),

	% There will be at least one sample exactly equal to Max, which must thus be
	% assigned to the last slot, so we must have: Min+ BucketWidth*BucketCount >
	% Max; so BucketWidth must be greater than (Max-Min)/BucketCount, and we
	% take:
	%
	Offset = ?epsilon,
	%Offset = 0.0,

	BucketWidth = (Max-Min) / BucketCount + Offset,

	Counters = type_utils:initialise_counters( BucketCount ),

	Buckets = count_in_buckets( Samples, Counters, Min, BucketWidth ),

	add_mid_points( Min, BucketWidth, BucketCount, Buckets ).


% Registers each sample in its corresponding bucket:
count_in_buckets( _Samples=[], Counters, _Min, _BucketWidth ) ->
	%trace_utils:debug_fmt( "Final buckets: ~p.", [ Counters ] ),
	tuple_to_list( Counters );

count_in_buckets( _Samples=[ S | T ], Counters, Min, BucketWidth ) ->
	% Index starts at 1; no rounding up, not to go past last bucket:
	BucketIndex = floor( ( S - Min ) / BucketWidth ) + 1,
	NewCounters = type_utils:increment_counter( BucketIndex, Counters ),
	count_in_buckets( T, NewCounters, Min, BucketWidth ).


% Associates to the count in each bucket the middle of the bucket range; returns
% a list of {MidPointValue,Count}.
%
add_mid_points( Min, BucketWidth, BucketCount, Buckets ) ->
	% Pre-offset all values of half bucket width:
	%Midpoints = get_midpoints( Min + BucketWidth / 2, BucketCount,
	Midpoints = get_midpoints( Min, BucketCount,
							   BucketWidth, _Acc=[] ),
	lists:zip( Midpoints, Buckets ).


get_midpoints( _Current, _BucketCount=0, _BucketWidth, Acc ) ->
	lists:reverse( Acc );

get_midpoints( Current, BucketCount, BucketWidth, Acc ) ->
	NewAcc = [ Current | Acc ],
	get_midpoints( Current+BucketWidth, BucketCount-1, BucketWidth, NewAcc ).




-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_basic_random(),

	test_facilities:display( "Testing now non-uniform random sampling." ),

	% Defined as discrete options:
	%test_biased_coin(),

	%test_uniform(),
	%test_exponential(),
	test_gamma(),
	%test_gumbel(),
	%test_loglogistic(),
	%test_lognormal(),
	%test_gaussian(),

	%test_weibull(),

	%test_beta(),

	%test_custom_pdf(),

	test_facilities:stop().
