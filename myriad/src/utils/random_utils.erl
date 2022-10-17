% Copyright (C) 2007-2022 Olivier Boudeville
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
% Creation date: July 1, 2007.


% @doc Gathering of various <b>random-related</b> facilities, based on
% probability distributions, either built-in (uniform, exponential or gaussian)
% or user-defined, arbitrary ones.
%
% See random_utils_test.erl for the corresponding test.
%
-module(random_utils).



% Usage notes.

% There are three basic classes of built-in random distributions:
% - uniform (a.k.a. white noise)
% - exponential
% - Gaussian (a.k.a. normal)
%
% Often distributions are generalised with parameters that allow to specialise
% them into well-known distributions (which are thus special cases thereof).
%
% Upcoming additions could be:
%
% - the Gamma distribution (see
% https://en.wikipedia.org/wiki/Gamma_distribution), which includes the
% exponential distribution, the (unrelated) Erlang distribution, and the
% chi-square distribution
%
% - the Dirac delta distribution (a.k.a the unit impulse)
%
% - some instantaneous distribution (to be determined)
%
% - the Weibull_distribution (see
% https://en.wikipedia.org/wiki/Weibull_distribution); being quite flexible, its
% proper parametrisation can cover many laws, including the exponential law
% (k=1) and the Rayleigh law (k=2)



% Functions for random management.

% As a general rule of thumb, if generating random values in an interval that
% are:
%  - integer: then both bounds are included
%  - floating point: then the lower bound is included and the uppoer one is
%  excluded

% In this module, "positive integer" must be understood as comprising zero.


% Service usage, state and seeding:
-export([ start_random_source/3, start_random_source/1, can_be_seeded/0,
		  reset_random_source/1, stop_random_source/0,

		  get_random_module_name/0,
		  get_random_state/0, set_random_state/1,

		  get_random_seed/0, check_random_seed/1 ]).


% Functions related to uniform sampling:
-export([ get_uniform_value/0, get_uniform_value/1, get_uniform_value/2,
		  get_uniform_values/2, get_uniform_values/3,

		  get_uniform_floating_point_value/1,
		  get_uniform_floating_point_value/2,

		  get_boolean/0, one_of/1, get_random_subset/2 ]).


% Functions related to exponential sampling:
-export([ get_exponential_value/1, get_positive_integer_exponential_value/1,
		  get_exponential_values/2, get_positive_integer_exponential_values/2
		]).


% Functions related to gaussian sampling:
-export([ get_gaussian_value/2, get_positive_integer_gaussian_value/2,
		  get_gaussian_values/3, get_positive_integer_gaussian_values/3 ]).


% Functions related to arbitrary, non-uniform sampling:
-export([ generate_random_state_from/1,
		  get_sample_from/1, get_samples_from/2 ]).





-type seed_element() :: integer().
% Not future-proof enough (ex: not compliant with other solutions like
% SIMD-oriented Fast Mersenne Twister).

-type seed() :: { seed_element(), seed_element(), seed_element() }.


% random:ran/0 does not seem exported, replaced by seed/0:
-type random_state() :: seed()
					  | rand:state()
					  | alias_state() % for non-uniform random samples
					  | any().
% For simpler generators, the state is just a seed, for all the others the state
% may be much larger/more complex.


-record( alias_state, {

	% Total number of sample entries:
	entry_count :: count(),

	% Array referencing all declared samples:
	sample_values :: array( sample() ),

	% Array keeping track of the sample corresponding to each category:
	indexes :: array( positive_index() ),

	% Array of probability-like for each category:
	prob_likes :: array( probability_like() ) } ).


-opaque alias_state() :: #alias_state{}.
% The state of a random generator in charge of producing samples according to
% the specified discrete probability distribution, based on the alias method.



-type sample( T ) :: T.
% The type of a sample that can be drawn from a probability distribution; a
% probability may be indeed associated to any kind of samples (integer ones,
% strings, vectors, etc.).

-type sample() :: sample( any() ).
% Designates a sample value of unknown type.


-type sample_entry( T ) :: { sample( T ), probability_like() }.
% An entry corresponding to a sample of specified type in a probability
% distribution.

-type sample_entry() :: sample_entry( any() ).
% An entry corresponding to a sample of unspecified type in a probability
% distribution.


-type rate() :: number().
% A rate parameter, typically for the Lambda parameter of the exponential law.


-type mean() :: number().
% An arithmetic mean of a list of numbers, that is the sum of all of the numbers
% divided by the number of numbers.
%
% See https://en.wikipedia.org/wiki/Mean#Arithmetic_mean_(AM)


-type standard_deviation() :: math_utils:standard_deviation().
% A measure of the amount of dispersion of a set of values.
%
% It is the square root of its variance.
%
% See https://en.wikipedia.org/wiki/Standard_deviation




% Section for the description of random laws (probability distributions).


-type uniform_law() :: { 'uniform', non_neg_integer() }.
% A probability distribution with which all declared samples have the same
% probability of being drawn.



-type exponential_law() :: { 'exponential', Lambda :: rate() }.
% An exponential law is fully determined when its single, "rate" parameter
% (Lambda) is given.
%
% The probability density function is p(x) = Lambda.exp(-Lambda.x), whose
% integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% Refer to https://en.wikipedia.org/wiki/Exponential_distribution.


-type positive_integer_exponential_law() ::
						{ 'positive_integer_exponential', Lambda :: rate() }.
% An exponential law yielding only positive integer samples.
%
% Refer to exponential_law/0 for further details.



-type gaussian_law() ::
		{ 'gaussian', Mu :: mean(), Sigma :: standard_deviation() }.
% A Gaussian (a.k.a. normal, bell curve) law is fully determined when its two
% parameters are given:
%
% - its mean (Mu), the average value of the samples
%
% - its standard deviation (Sigma), being expressed in the same unit as the
% samples (its square being the variance)
%
% About 68% of the samples are in [Mu-Sigma;Mu+Sigma].
% About 95.4% of the samples (i.e. almost all) are in [Mu-2.Sigma;Mu+2.Sigma].
%
% See also: http://en.wikipedia.org/wiki/Standard_deviation


-type positive_integer_gaussian_law() ::
		{ 'positive_integer_gaussian', Mu :: mean(),
		  Sigma :: standard_deviation() }.
% A Gaussian (a.k.a. normal, bell curve) law yielding only positive integer
% samples.
%
% Refer to gaussian_law/0 for further details.



-type random_law() :: uniform_law()

					| exponential_law()
					| positive_integer_exponential_law()

					| gaussian_law()
					| positive_integer_gaussian_law().


-type discrete_probability_distribution( T ) :: [ sample_entry( T ) ].
% The specification of a discrete probability distribution (a.k.a. frequency
% distribution) whose samples are of the specified type.
%
% Ex: discrete_probability_distribution(integer()) or
% discrete_probability_distribution(vector3:vector3()).
%
% Samples of null probability are usually omitted, as such a probability is
% implicit and results in the corresponding sample never to be drawn.
%
% At least an entry with a non-null probability must be defined.
%
% Preferably a given sample value is specified only once, i.e. is declared in a
% single entry (otherwise the distribution will behave as if the probabilities
% for that sample were summed - yet the distribution will be less compact).
%
% Such a distribution can be obtained either directly or by sampling a fun( T ->
% probability_like() function over a subset of its domain.


-type discrete_probability_distribution() ::
		discrete_probability_distribution( any() ).
% The specification of a discrete probability distribution of unknown sample
% type.


-type pdf( S ) :: fun( ( S ) -> probability_like() ).
% A Probability Density Function telling, for a given sample of type S, its
% corresponding probability-like value.
%
% See also the math_utils:sample* functions and get_samples_from/2.


-type pdf() :: pdf( any() ).
% A Probability Density Function for samples of unspecified type.


-export_type([ seed_element/0, seed/0, random_state/0, alias_state/0,
			   sample/0, sample/1, sample_entry/0, sample_entry/1,
			   rate/0, mean/0, standard_deviation/0,

			   uniform_law/0,
			   exponential_law/0, positive_integer_exponential_law/0,
			   gaussian_law/0, positive_integer_gaussian_law/0,
			   random_law/0,

			   discrete_probability_distribution/0,
			   discrete_probability_distribution/1,

			   pdf/0, pdf/1 ]).


% Uncomment to enable the logging of random operations:
%-define(log_random,).

-ifdef(log_random).

  -define( trace_random(FS,FV), trace_utils:info_fmt( FS, FV ) ).

-else. % log_random

  -define( trace_random(FS,FV), no_trace ).

-endif. % log_random




% Implementation notes.

% About pseudorandom number generator (PRNG):
%
% If use_crypto_module is defined, the (now deprecated here, for this use)
% crypto module will be used, otherwise (which is the default now) the rand
% module will be used instead (the random module being now deprecated).
%
% Currently the crypto module is not used by default, as:
%
% - not all Erlang VMs can be built with the proper SSH support
%
% - it is unclear whether the crypto module can be seeded like the random module
% can be (probably it cannot be)
%
% - there is no crypto function returning a random float uniformly distributed
% between 0.0 and 1.0, and it may not be easy to implement it from what is
% available
%
% - we do not know whether the seed is per-node (most likely), or per-process
%
% Therefore crypto cannot be easily swapped with other random generators.
%
% Finally, the requirements of a Cryptographically-secure PRNG (CSPRNG) exceed
% the general PRNGs, and may be useless / of higher costs in other
% contexts. Refer to our hash_utils module for more information on that topic.
%
% The current module considered using TinyMT and/or SFMT, yet now they have been
% superseded by the xorshift, xoroshiro, and xoshiro algorithms by Sebastiano
% Vigna, and the rand module offers variations of the xorshift and xoroshiro
% algorithms, called exsss and exro928ss.
%
% Of course, switching random engines will generate different random series.
%
% They may also have different behaviours (ex: with regards to processes not
% being explicitly seeded, inheriting from a seed that is constant or not - the
% shortest path to break reproducibility).
%
% Note that modules relying on an implicit state (e.g. for seeding) generally
% use the process dictionary to store it (e.g. 'rand').

%-define(use_crypto_module,).


% Shorthands:

-type array( T ) :: array:array( T ).

-type count() :: basic_utils:count().
-type positive_index() :: basic_utils:positive_index().


-type probability_like() :: math_utils:probability_like().
% Any number that can be interpreted ultimately as a probability.



% Apparently, as soon as functions are defined within preprocessor guards, their
% definition is ignored by edoc, resulting in edoc failing because of multiple
% 'doc' tags at the first function defined outsmart of these guards.
%
% As if we copied their documentation there this would not work either (the
% corresponding functions not being found), we just disabled their 'doc' tags
% ('at' doc being replaced with 'doc:').


% Specs gathered here, because of macro guards.
-spec start_random_source( seed_element(), seed_element(), seed_element() ) ->
								random_state().

-spec start_random_source( 'default_seed' | 'time_based_seed' | seed() ) ->
								void().

-spec can_be_seeded() -> boolean().

-spec reset_random_source( 'default_seed' | 'time_based_seed' | seed() ) ->
								void().

-spec stop_random_source() -> void().


-spec get_uniform_value() -> float().

-spec get_uniform_value( pos_integer() ) -> pos_integer().

-spec get_uniform_value( integer(), integer() ) -> integer().

-spec get_uniform_floating_point_value( number() ) -> float().
-spec get_uniform_floating_point_value( number(), number() ) -> float().

-spec get_random_state() -> maybe( random_state() ).
-spec set_random_state( random_state() ) -> void().



% @doc Generates a list of Count elements uniformly drawn in [1,N].
-spec get_uniform_values( pos_integer(), count() ) -> [ pos_integer() ].
get_uniform_values( N, Count ) ->
	get_uniform_values_helper( N, Count, _Acc=[] ).


get_uniform_values_helper( _N, _Count=0, Acc ) ->
	Acc;

get_uniform_values_helper( N, Count, Acc ) ->
	get_uniform_values_helper( N, Count-1, [ get_uniform_value( N ) | Acc ] ).



% @doc Generates a list of Count elements uniformly drawn in [Nmin,Nmax].
-spec get_uniform_values( integer(), integer(), count() ) -> [ integer() ].
get_uniform_values( Nmin, Nmax, Count ) ->
	get_uniform_values_helper( Nmin, Nmax, Count, _Acc=[] ).


% (helper)
get_uniform_values_helper( _Nmin, _Nmax, _Count=0, Acc ) ->
	Acc;

get_uniform_values_helper( Nmin, Nmax, Count, Acc ) ->
	get_uniform_values_helper( Nmin, Nmax, Count - 1,
							  [ get_uniform_value( Nmin, Nmax ) | Acc ] ).


% To test compilation:
%-define(use_crypto_module,).


% Now crypto is hardly used anymore; however these are the doc tags that edoc
% read.
%
-ifdef(use_crypto_module).


% crypto module used here.
%
% Warning: the seed and state management are presumably global (not
% per-process).


% @doc Starts the random source with specified seeding.
start_random_source( _A, _B, _C ) ->
	throw( crypto_module_cannot_be_seeded ).



% @doc Starts the random source with specified seeding.
start_random_source( default_seed ) ->
	?trace_random( "~w starting random source with crypto.", [ self() ] ),
	ok = crypto:start();

start_random_source( time_based_seed ) ->
	throw( crypto_module_cannot_be_seeded ).


% @doc Tells whether this random source can be seeded.
%
% crypto cannot be seeded, but rand can.
%
can_be_seeded() ->
	false.


% @doc Resets the random source with a new seed.
reset_random_source( _Seed ) ->
	throw( crypto_module_cannot_be_reset ).


% @doc Stops the random source.
stop_random_source() ->
	ok = crypto:stop().



% @doc Returns a random float uniformly distributed between 0.0 (included) and
% 1.0 (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value() ->
	% Not available: crypto:rand_uniform( 0.0, 1.0 ).
	throw( not_available ).



% @doc Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( N ) ->
	crypto:rand_uniform( 1, N+1 ).



% @doc Returns an integer random value generated from an uniform distribution in
% [Nmin,Nmax] (thus with both bounds included), updating the random state in the
% process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( Nmin, Nmax ) when Nmin =< Nmax ->
	crypto:rand_uniform( Nmin, Nmax+1 ).


% @doc Returns a floating-point random value in [0.0,N[ generated from an
% uniform distribution.
%
% Given a number (integer or float) N (positive or not), returns a random
% floating-point value uniformly distributed between 0.0 (included) and N
% (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( N ) ->
	throw( not_available ).


% @doc Returns a floating-point random value in [Nmin, Nmax[ generated from an
% uniform distribution.
%
% Given two numbers (integer or float) Nmin and Nmax (each being positive or
% not), returns a random floating-point value uniformly distributed between Nmin
% (included) and Nmax (excluded), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( Nmin, Nmax ) ->
	throw( not_available ).



% @doc Returns the name of the module managing the random generation.
%
% Spec already specified, for all random settings.
%
-spec get_random_module_name() -> 'crypto'.
get_random_module_name() ->
	crypto.



% @doc Returns the random state of this process (it is useful for example for
% process serialisations).
%
% Spec already specified, for all random settings.
%
get_random_state() ->
	% At least: not implemented yet.
	throw( not_available ).



% @doc Sets the random state of this process (it is useful for example for
% process serialisations).
%
% Spec already specified, for all random settings.
%
set_random_state( _NewState ) ->
	% At least: not implemented yet.
	throw( not_available ).





-else. % use_crypto_module not defined below:


% Here we do not use the 'crypto' module; we use the 'rand' one (replacing the
% deprecated 'random' module), and this is the default, most common setting now.


% For the 'random' module, according to
% http://osdir.com/ml/erlang-questions-programming/2013-10/msg00235.html one
% must ensure that the initial values are large and different enough.
%
% Anyway, starting from OTP 18.0, we switched from the 'random' module to the
% new 'rand' module, offering better service and control.
%
% They have different semantics though: if a process is not explicitly seeded,
% 'random' will assign a constant seed while 'rand' will assign a non-constant
% one.

% Default rand module used here.
%
% The seed and state management is per-process. We prefer that. The process
% dictionary (based on the 'rand_seed' key) is used to store this random state
% (otherwise we stay away from any use of this impure dictionary).



% Refer to http://www.erlang.org/doc/man/rand.html for more information about
% algorithms:

% Xorshift116+, 58 bits precision and period of 2^116-1, 320 bytes per state, on
% 64-bit:
%
% (corrected version of exsplus, yet now superseded by exrop, see below)
%
%-define( rand_algorithm, exsp ).



% Xoroshiro116+, 58 bits precision and period of 2^116-1
%
% Jump function: equivalent to 2^64 calls.
%
% Default since OTP 20, to be used in most cases:
%
-define( rand_algorithm, exrop ).



% Xorshift64*, 64 bits precision and a period of 2^64-1, 336 bytes per state on
% 64-bit:
%
%-define( rand_algorithm, exs64 ).


% Xorshift1024*, 64 bits precision and a period of 2^1024-1 (most expensive of
% the built-in random algorithms, 856 bytes per state on 64-bit):
%
% (corrected version, to be used instead of exsplus)
%
%-define( rand_algorithm, exs1024s ).



% doc: Starts the random source with specified seeding.
%
% Note: if a process does not explicitly select a seed, with 'rand' a
% non-constant seed will be assigned. For reproducibility, start your random
% sources with a seed of your own.
%
start_random_source( A, B, C ) ->

	Seed = { A, B, C },

	?trace_random( "~w starting random source with rand (~p), "
				   "seeded with ~w.", [ self(), ?rand_algorithm, Seed ] ),

	%random:seed( A, B, C ).
	rand:seed( ?rand_algorithm, Seed ).



% doc: Seeds the random number generator, with specified seeding, or with a
% default seed (if wanting to obtain the same random series at each run) or with
% current time (if wanting "real", non-reproducible randomness).
%
% Spec already specified, for all random settings.
%
% Note: if a process does not explicitly select a seed, with 'rand' a
% non-constant seed will be assigned. For reproducibility, start explicitly your
% random sources.
%
start_random_source( _Seed={ A, B, C } ) ->
	start_random_source( A, B, C );


start_random_source( default_seed ) ->

	% Use default (fixed) values in the process dictionary:
	%random:seed();

	% random:seed/0 was using a constant seed, not rand:seed/1, so we have to
	% provide a constant seed by ourselves:
	%
	ConstantSeed = { _A=17, _B=79, _C=1111 },

	?trace_random( "~w starting random source with rand (~p), "
		"using default constant seed ~w.",
		[ self(), ?rand_algorithm, ConstantSeed ] ),

	rand:seed( ?rand_algorithm, ConstantSeed );


start_random_source( time_based_seed ) ->

	% Each run will result in different random series (erlang:now/0 was
	% previously used):
	%
	% (refer to:
	% http://erlang.org/doc/apps/erts/time_correction.html#Erlang_System_Time)
	%
	{ A, B, C } = { erlang:monotonic_time(), erlang:unique_integer(),
					erlang:time_offset() },

	?trace_random( "~w forging time-based seed ~p.", [ self(), { A, B, C } ] ),

	% Directly inspired from third example in
	% http://osdir.com/ml/erlang-questions-programming/2013-10/msg00244.html:
	%
	start_random_source( A + erlang:phash2( C ), B, 690123 + 16384 * C ).



% doc rand can be seeded.
can_be_seeded() ->
	true.



% doc: Resets the random source with a new seed.
reset_random_source( Seed ) ->
	% New seeding (stored in the process dictionary), as opposed to the setting
	% of a previously defined state:
	%
	rand:seed( ?rand_algorithm, Seed ).



% doc: Stops the random source.
stop_random_source() ->
	ok.



% doc: Returns a random float uniformly distributed between 0.0 (included) and
% 1.0 (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value() ->
	%random:uniform().
	rand:uniform().



% doc: Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( N ) when is_integer( N ) ->
	%random:uniform( N ).
	rand:uniform( N );

get_uniform_value( N ) ->
	throw( { not_integer, N } ).



% doc: Returns an integer random value generated from an uniform distribution in
% [Nmin,Nmax] (i.e. both bounds included), updating the random state in the
% process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_value( Nmin, Nmax ) when is_integer( Nmin )
					andalso is_integer( Nmax ) andalso Nmin =< Nmax ->

	% Ex: if Nmin = 3, Nmax = 5, we can draw value in [3, 4, 5], hence:
	%
	% N = 5 - 3 + 1 = 3.
	%
	N = Nmax - Nmin + 1,

	% Drawn in [1,N]:
	get_uniform_value( N ) + Nmin - 1;

get_uniform_value( Nmin, Nmax ) ->
	throw( { not_integer_bounds, { Nmin, Nmax } } ).



% doc: Returns a floating-point random value in [0.0,N[ generated from an
% uniform distribution.
%
% Given a number (integer or float) N (positive or not), returns a random
% floating-point value uniformly distributed between 0.0 (included) and N
% (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( N ) ->
	% Generated float in [0.0,1.0[:
	N * rand:uniform().


% doc: Returns a floating-point random value in [Nmin, Nmax[ generated from an
% uniform distribution.
%
% Given two numbers (integer or float) Nmin and Nmax (each being positive or
% not), returns a random floating-point value uniformly distributed between Nmin
% (included) and Nmax (excluded), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( Nmin, Nmax ) ->
	% Generated float in [0.0,1.0[:
	Nmin + ( Nmax - Nmin ) * rand:uniform().



% doc: Returns the name of the module managing the random generation.
%
% Spec already specified, for all random settings.
%
-spec get_random_module_name() -> 'rand'. %'random'.
get_random_module_name() ->
	%random.
	rand.


% doc: Returns the random state of the current process (it is useful for example
% for process serialisations).
%
% Spec already specified, for all random settings.
%
get_random_state() ->

	% Read from the process dictionary:

	% Actually, no state should not be considered as an error:
	%case erlang:get( random_seed ) of
	%
	%   undefined ->
	%       % Probably that there has been not prior seeding:
	%       throw( random_state_not_available );
	%
	%   S ->
	%       S
	%
	%end.

	% May return 'undefined', if not seeded yet:
	%erlang:get( random_seed ).
	%erlang:get( rand_seed ).
	% Best:
	rand:export_seed().



% doc: Sets the random state of this process (it is useful for example for
% process serialisations).
%
% Spec already specified, for all random settings.
%
set_random_state( RandomState ) ->

	% All are in the process dictionary (beware!):
	%erlang:put( random_seed, NewState ).
	%erlang:put( rand_seed, NewState ).
	rand:seed( RandomState ).


-endif. % use_crypto_module not defined






% Now sections that do not depend on defines.


% Seeding section.


% The upper bound for a seed element.
-define( seed_upper_bound, 65500 ).


% @doc Returns a seed obtained from the random source in use.
%
% This is a randomly-determined seed, meant to be used to create another random
% generator.
%
-spec get_random_seed() -> seed().
get_random_seed() ->
	{ get_uniform_value( ?seed_upper_bound ),
	  get_uniform_value( ?seed_upper_bound ),
	  get_uniform_value( ?seed_upper_bound ) }.



% @doc Checks that the specified seed is valid.
%
% Ex: at least with some algorithms, {0, 0, 0} does not yield a correct random
% series.
%
-spec check_random_seed( seed() ) -> void().
check_random_seed( { A, B, C } ) when is_integer( A ) andalso is_integer( B )
		andalso is_integer( C ) andalso A > 0 andalso B > 0 andalso C > 0 ->
	ok;

check_random_seed( S ) ->
	throw( { invalid_random_seed, S } ).





% Section for the generation of random samples according to a uniform law.


% @doc Returns a boolean random value generated from an uniform distribution.
%
% Therefore true and false are equally likely to be returned.
%
-spec get_boolean() -> boolean().
get_boolean() ->
	get_uniform_value( 0, 100 ) >= 49.



% @doc Returns a random element of the specified list, selected according to a
% uniform distribution.
%
-spec one_of( [ any() ] ) -> any().
one_of( ListOfThings ) ->
	Index = get_uniform_value( length( ListOfThings ) ),
	lists:nth( Index, ListOfThings ).



% @doc Returns a list of the specified number of unique elements drawn from the
% specified input list (so that there is no duplicate in the returned list).
%
% Note: defined to ease interface look-up, use directly
% list_utils:draw_elements_from/2 instead.
%
-spec get_random_subset( count(), list() ) -> list().
get_random_subset( ValueCount, InputList ) ->
	list_utils:draw_elements_from( InputList, ValueCount ).





% Section for the generation of random samples according to an exponential law.
%
% Note: each of the three forms comes in two versions, with floating-point or
% (positive) integer values being returned.



% @doc Returns an exponential floating-point random value, with Lambda being the
% rate parameter.
%
% As get_uniform_value/1 never returns 1.0, a strictly positive value is always
% returned.
%
% See exponential_law() for further details.
%
% Using ad-hoc inverse transform sampling here.
%
-spec get_exponential_value( rate() ) -> float().
get_exponential_value( Lambda ) ->

	%trace_utils:debug_fmt( "Lambda=~p", [ Lambda ] ),

	% Note: with Erlang, math:log(x) is ln(x):
	- math:log( get_uniform_value() ) / Lambda.



% @doc Returns an exponential (positive) integer random value, with Lambda being
% the rate parameter.
%
% See get_exponential_value/1 for further details.
%
-spec get_positive_integer_exponential_value( rate() ) -> non_neg_integer().
get_positive_integer_exponential_value( Lambda ) ->
	round( get_exponential_value( Lambda ) ).



% @doc Returns a list of Count exponential values according to the specified
% Lambda setting.
%
% See get_exponential_value/1 for further details.
%
-spec get_exponential_values( rate(), count() ) -> [ float() ].
get_exponential_values( Lambda, Count ) ->
	generate_exponential_list( Lambda, Count, _Acc=[] ).



% The generate_*_list could rely on higher-order functions.


% @doc Generates a list of Count exponential random values.
%
% (helper)
-spec generate_exponential_list( rate(), count(), [ float() ] ) ->
												[ float() ].
generate_exponential_list( _Lambda, _Count=0, Acc ) ->
	Acc;

generate_exponential_list( Lambda, Count, Acc ) ->
	generate_exponential_list( Lambda, Count-1,
							   [ get_exponential_value( Lambda ) | Acc ] ).



% @doc Returns a list of Count positive integer exponential values according to
% the specified Lambda setting.
%
% See get_exponential_value/1 for further details.
%
-spec get_positive_integer_exponential_values( rate(), count() ) ->
											[ non_neg_integer() ].
get_positive_integer_exponential_values( Lambda, Count ) ->
	generate_positive_integer_exponential_list( Lambda, Count, _Acc=[] ).


% (helper)
generate_positive_integer_exponential_list( _Lambda, _Count=0, Acc ) ->
	Acc;

generate_positive_integer_exponential_list( Lambda, Count, Acc ) ->
	generate_positive_integer_exponential_list( Lambda, Count-1,
		[ get_positive_integer_exponential_value( Lambda ) | Acc ] ).






% Section for the generation of random samples according to a gaussian law.



% @doc Returns a random value generated from the normal (Gaussian) distribution
% with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns a random
% floating-point value drawn according to the corresponding Gaussian law,
% updating the state in the process dictionary.
%
-spec get_gaussian_value( mean(), standard_deviation() ) -> float().
get_gaussian_value( Mu, Sigma ) ->
	sigma_loop( Mu, Sigma ).



% @doc Returns a non-negative integer random value generated from the
% normal (Gaussian) distribution with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
% The result is a non-negative integer (not a float). Values will be drawn until
% they are non-negative.
%
-spec get_positive_integer_gaussian_value( mean(), standard_deviation() ) ->
											non_neg_integer().
get_positive_integer_gaussian_value( Mu, Sigma ) ->
	sigma_loop_positive_integer( Mu, Sigma ).




% @doc Generates a new gaussian value and updates the state.
%
% Mu is the mean, Sigma is the standard deviation (variance being its square).
%
% Returns the computed value.
%
% See also
% https://en.wikipedia.org/wiki/Normal_distribution#Computational_methods
%
-spec sigma_loop( mean(), standard_deviation() ) -> float().
sigma_loop( Mu, Sigma ) ->

	% Best (most efficient) implementation that could be used in the future:
	% https://en.wikipedia.org/wiki/Ziggurat_algorithm

	% Note: at least for (Mu=0, Sigma=1), rand:normal/0 could be used.

	% Using here the second best approach, the Marsaglia polar method (see
	% https://en.wikipedia.org/wiki/Marsaglia_polar_method); used for example by
	% C++11 GNU GCC libstdc++.

	% So V1 and V2 are in [-1.0;1.0[:
	V1 = 2.0 * get_uniform_value() - 1.0,

	% Supposedly independent from V1:
	V2 = 2.0 * get_uniform_value() - 1.0,

	S  = (V1 * V1) + (V2 * V2),

	% Loop until S in ]0,1[:
	case S >= 1.0 orelse S =:= 0.0 of

		% Rejected:
		true ->
			sigma_loop( Mu, Sigma );

		_False ->

			% Here S in ]0;1.0[ (note that 1.0 should be included, possibly by
			% transforming any 0.0 in a 1.0):
			%
			%trace_utils:debug_fmt( "Mu = ~p, Sigma = ~p, S = ~p.",
			%                       [ Mu, Sigma, S ] ),

			% math:log/1 is the Natural Log (base e log):
			Scale = math:sqrt( ( -2.0 * math:log( S ) ) / S ),

			% Adjust for standard deviation and mean:
			Mu + Sigma * Scale * V1

	end.



% @doc Generates a new integer non-negative gaussian value and updates the
% state.
%
% Returns the computed value.
%
-spec sigma_loop_positive_integer( mean(), standard_deviation() ) ->
												non_neg_integer().
sigma_loop_positive_integer( Mu, Sigma ) ->

	% Loops until a positive integer is found:
	case round( sigma_loop( Mu, Sigma ) ) of

		TriedValue when TriedValue < 0 ->
			sigma_loop_positive_integer( Mu, Sigma );

		NonNegativeValue ->
			NonNegativeValue

	end.



% @doc Returns a list of Count Gaussian values.
%
% Given a mean Mu and a standard deviation Sigma, returns random floating-point
% values drawn according the corresponding Gaussian law, updating the state in
% the process dictionary.
%
-spec get_gaussian_values( mean(), standard_deviation(), count() ) ->
													[ float() ].
get_gaussian_values( Mu, Sigma, Count ) ->
	generate_gaussian_list( Mu, Sigma, Count, _Acc=[] ).



% @doc Generates a list of Count Gaussian random values.
%
% (helper)
generate_gaussian_list( _Mu, _Sigma, _Count=0, Acc ) ->
	Acc;

generate_gaussian_list( Mu, Sigma, Count, Acc ) ->
	generate_gaussian_list( Mu, Sigma, Count-1,
							[ sigma_loop( Mu, Sigma ) | Acc ] ).



% @doc Returns a list of Count positive integer Gaussian values.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
-spec get_positive_integer_gaussian_values( mean(), standard_deviation(),
											count() ) -> [ non_neg_integer() ].
get_positive_integer_gaussian_values( Mu, Sigma, Count ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count ).



% @doc Generates a list of Count positive integer Gaussian random values.
-spec generate_positive_integer_gaussian_list( mean(), standard_deviation(),
									count() ) -> [ non_neg_integer() ].
generate_positive_integer_gaussian_list( Mu, Sigma, Count ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count, [] ).


% (helper)
generate_positive_integer_gaussian_list( _Mu, _Sigma, _Count=0, Acc ) ->
	Acc;

generate_positive_integer_gaussian_list( Mu, Sigma, Count, Acc ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count-1,
		[ erlang:round( sigma_loop_positive_integer( Mu, Sigma ) ) | Acc ] ).





% Section for the generation of arbitrary, non-uniform random samples.
%
% Drawing from user-specified random laws (probability distributions / PDFs,
% i.e. Probability Density Functions) corresponds to operating inverse transform
% sampling (see https://en.wikipedia.org/wiki/Inverse_transform_sampling).

% Here, such sampling is done on discrete (as opposed to continuous)
% probabilities, which can be done efficiently thanks to the alias method (see
% https://en.wikipedia.org/wiki/Alias_method), also known as the Walker method.
%
% The current implementation derives directly notably from the one kindly shared
% (Apache License Version 2.0) by Sergey Prokhorov (email: me at seriyps dot
% ru), refer to https://gist.github.com/seriyps/5593193, which itself derived
% from a Python implementation.



% @doc Returns the state of a random generator in charge of producing samples
% according to the specified discrete probability distribution.
%
% Ex: MyDistributionState = random_utils:generate_random_state_from(
%                              [{a,10}, {b,20}, {c,40}, {d,30}]).
%
% or MyDistributionState = random_utils:generate_random_state_from(
%                              [{"hello",0.6}, {"goodbye",0.4}]).
%
% From this (const) state, computed once for all and whose construction does not
% depend on any other random state (it is deterministic), any number of samples
% respecting said distribution can be drawn, like in:
%  ```
%  S1 = random_utils:generate_random_state_from(MyDistributionState),
%  Samples = [random_utils:get_sample_from(MyDistributionState)
%                        || _ <- lists:seq(1, Count)]
%  '''
%
% This preprocessing uses O(N) time, where N is the number of declared samples
% of the corresponding distribution, in order to generate its returned state.
%
-spec generate_random_state_from( discrete_probability_distribution() ) ->
											alias_state().
generate_random_state_from( DiscreteProbDist ) ->
	{ SampleValues, ProbLikes } = lists:unzip( DiscreteProbDist ),
	generate_random_state_from( SampleValues, ProbLikes ).


% (helper)
-spec generate_random_state_from( [ sample() ], [ probability_like() ] ) ->
										alias_state().
generate_random_state_from( SampleValues, ProbLikes ) ->

	EntryCount = length( ProbLikes ),

	% Notations from https://en.wikipedia.org/wiki/Alias_method#Table_generation

	% Ui = n.pi:

	% Sum expected to be non-null (at least one non-null probability requested):
	Factor = EntryCount / lists:sum( ProbLikes ),

	ScaledProbs = [ Factor * PL || PL <- ProbLikes ],

	{ UnderFulls, OverFulls } = split_by_index_fullness( ScaledProbs ),

	%trace_utils:debug_fmt( "EntryCount = ~p, UnderFulls = ~p, OverFulls = ~p",
	%                       [ EntryCount, UnderFulls, OverFulls ] ),

	{ Indexes, UpdatedProbLikes } = fill_entries( UnderFulls, OverFulls,
		array:new( EntryCount ), array:from_list( ScaledProbs ) ),

	%trace_utils:debug_fmt( "Sample value array: ~p~nIndex array: ~p~n"
	%   "Prob-like array: ~p",
	%   [ SampleValues, array:to_list( Indexes ),
	%     array:to_list( UpdatedProbLikes ) ] ),

	SampleArray = array:from_list( SampleValues ),

	#alias_state{ entry_count=EntryCount,
				  sample_values=SampleArray,
				  indexes=Indexes,
				  prob_likes=UpdatedProbLikes }.



% Returns two index lists, the second corresponding to the "overfull" group
% (where Ui > 1), the first to the "underfull" group (where (Ui < 1 and Ki has
% not been initialized) or (where Ui = 1 or Ki has been initialized) - that is,
% respectively, the "strict underfull" subgroup and the "exactly full" one).
%
% (helper)
%
-spec split_by_index_fullness( [ probability_like() ] ) ->
					{ UnderFulls :: [ positive_index() ],
					  OverFulls  :: [ positive_index() ]  }.
split_by_index_fullness( ScaledProbLikes ) ->
	split_by_index_fullness( ScaledProbLikes, _UnderFulls=[],
							 _OverFulls=[], _Idx=0 ).


% (helper)
split_by_index_fullness( _ScaledProbLikes=[], UnderFulls, OverFulls, _Idx ) ->
	{ UnderFulls, OverFulls };

split_by_index_fullness( _ScaledProbLikes=[ PL | T ], UnderFulls, OverFulls,
						 Idx ) when PL < 1 ->
	split_by_index_fullness( T, [ Idx | UnderFulls ], OverFulls, Idx+1 );

split_by_index_fullness( _ScaledProbLikes=[ _PL | T ], UnderFulls, OverFulls,
						 Idx ) -> % Implicit: when PL >= 1 ->
	split_by_index_fullness( T,  UnderFulls, [ Idx | OverFulls ], Idx+1 ).



% Allocates the unused space in an underfull entry (U) to an overfull one (O),
% so that U becomes exactly full.
%
% (helper)
%
fill_entries( _UnderFulls=[ UIdx | TU ], _OverFulls=[ OIdx | TO ], IdxArray,
			  ProbLikeArray ) ->
	% Allocate the unused space in entry O to outcome U:
	NewIdxArray = array:set( _Idx=UIdx, _Value=OIdx, IdxArray ),
	UPL = array:get( UIdx, ProbLikeArray ),
	OPL = array:get( OIdx, ProbLikeArray ),

	% Remove the allocated space from entry U:
	NewOPL = OPL + UPL - 1,

	NewProbLikeArray = array:set( OIdx, NewOPL, ProbLikeArray ),

	% Place O in the right list:
	{ NewUnderFulls, NewOverFulls } = case NewOPL < 1 of

		true ->
			{ [ OIdx | TU ], TO };

		_ ->
			{ TU, [ OIdx | TO ] }

	end,

	fill_entries( NewUnderFulls, NewOverFulls, NewIdxArray, NewProbLikeArray );


% Implicitly UnderFulls and/or OverFulls is empty here:
fill_entries( _UnderFulls, _OverFulls, IdxArray, ProbLikeArray ) ->
	{ IdxArray, ProbLikeArray }.



% @doc Returns a new sample drawn from the discrete probability distribution
% specified through its (constant) state (which is thus not returned), this
% state having been obtained initially (and once for all) from
% generate_random_state_from/1.
%
% Each sample is generated in constant time O(1) time with regard to the number
% of samples declared in the corresponding distribution.
%
% Such a generation depends (and modifies) the state of the underlying uniform
% random generator (ex: see start_random_source/0); precisely each non-uniform
% sampling results in two uniform samples to be drawn.
%
-spec get_sample_from( alias_state() ) -> sample().
get_sample_from( #alias_state{ entry_count=EntryCount,
							   sample_values=SampleValueArray,
							   indexes=IndexArray,
							   prob_likes=ProbLikeArray } ) ->

	% Thus uniform in [0, EntryCount-1]:
	PLIdx = get_uniform_value( EntryCount ) - 1,

	% Thus uniform in [0.0, 1.0[:
	P = get_uniform_value(),

	SampleIdx = case P =< array:get( PLIdx, ProbLikeArray ) of

		true ->
			PLIdx;

		_ ->
			array:get( PLIdx, IndexArray )

	end,

	array:get( SampleIdx, SampleValueArray ).



% @doc Returns the specified number of samples drawn from the discrete
% probability distribution specified through its (constant) state (which is thus
% not returned), as obtained from generate_random_state_from/1.
%
% Each sample is generated in constant time, that is O(1) time with regard to
% the number of samples declared in the corresponding distribution.
%
% Such a generation depends (and modifies) the state of the underlying uniform
% random generator (ex: see start_random_source/0); precisely each non-uniform
% sampling results in two uniform samples to be drawn.
%
-spec get_samples_from( count(), alias_state() ) -> [ sample() ].
get_samples_from( Count, AliasState ) ->
	[ get_sample_from( AliasState ) || _ <- lists:seq( 1, Count ) ].
