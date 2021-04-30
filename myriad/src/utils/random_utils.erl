% Copyright (C) 2007-2021 Olivier Boudeville
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


% Gathering of various random-related facilities.
%
% See random_utils_test.erl for the corresponding test.
%
-module(random_utils).



% Random-related functions.
-export([ start_random_source/3, start_random_source/1, can_be_seeded/0,
		  reset_random_source/1, stop_random_source/0,
		  get_random_value/0, get_random_value/1, get_random_value/2,
		  get_random_values/2, get_random_values/3,
		  get_uniform_floating_point_value/1,
		  get_uniform_floating_point_value/2,
		  get_random_subset/2,
		  get_random_module_name/0,
		  get_random_state/0, set_random_state/1,
		  get_random_seed/0, check_random_seed/1 ]).


% Not future-proof enough (ex: not compliant with other solutions like
% SIMD-oriented Fast Mersenne Twister):
%
-type seed_element() :: integer().

-type seed() :: { seed_element(), seed_element(), seed_element() }.


% For simpler generators, the state is just a seed, for all the others the state
% is much more complex:
%
% random:ran/0 does not seem exported, replaced by seed/0:
-type random_state() :: seed() | rand:state() | any().



-export_type([ seed_element/0, seed/0, random_state/0 ]).


% Uncomment to enable the logging of random operations:
%-define(log_random,).

-ifdef(log_random).

  -define( trace_random(FS,FV), trace_utils:info_fmt( FS, FV ) ).

-else. % log_random

  -define( trace_random(FS,FV), no_trace ).

-endif. % log_random



% Functions for random management.

% General rule of thumb: if generating random values in an interval that are:
%  - integer: then both bounds are included
%  - floating point: then the lower bound is included and the uppoer one is
%  excluded

% If use_crypto_module is defined, the (now deprecated) crypto module will be
% used, otherwise (which is the default now) the rand module will be used
% instead (the random module being now deprecated).
%
% Currently the crypto module is not used by default, as:
%
% - not all Erlang VM can be built with the proper SSH support
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
% The current module may support in the future the use of TinyMT and/or SFMT.
%
% Of course, switching random engines will generate different random series.
%
% They may also have different behaviours (ex: with regards to processes not
% being explictly seeded, inheriting from a seed that is constant or not - the
% shortest path to break reproducibility)
%
%-define(use_crypto_module,).


% Shorthands:

-type count() :: basic_utils:count().



% Specs gathered here, because of macro guards.
-spec start_random_source( seed_element(), seed_element(), seed_element() ) ->
								random_state().

-spec start_random_source( 'default_seed' | 'time_based_seed' | seed() ) ->
								void().


-spec can_be_seeded() -> boolean().

-spec reset_random_source( 'default_seed' | 'time_based_seed' | seed() ) ->
								void().

-spec stop_random_source() -> void().


-spec get_random_value() -> float().

-spec get_random_value( pos_integer() ) -> pos_integer().

-spec get_random_value( integer(), integer() ) -> integer().

-spec get_uniform_floating_point_value( number() ) -> float().
-spec get_uniform_floating_point_value( number(), number() ) -> float().

-spec get_random_state() -> maybe( random_state() ).
-spec set_random_state( random_state() ) -> void().


% Generates a list of Count elements uniformly drawn in [1,N].
-spec get_random_values( pos_integer(), count() ) ->
								[ pos_integer() ].
get_random_values( N, Count ) ->
	get_random_values_helper( N, Count, _Acc=[] ).


get_random_values_helper( _N, _Count=0, Acc ) ->
	Acc;

get_random_values_helper( N, Count, Acc ) ->
	get_random_values_helper( N, Count-1, [ get_random_value( N ) | Acc ] ).



% Generates a list of Count elements uniformly drawn in [Nmin;Nmax].
-spec get_random_values( integer(), integer(), count() ) ->
								[ integer() ].
get_random_values( Nmin, Nmax, Count ) ->
	get_random_values_helper( Nmin, Nmax, Count, _Acc=[] ).


% (helper)
get_random_values_helper( _Nmin, _Nmax, _Count=0, Acc ) ->
	Acc;

get_random_values_helper( Nmin, Nmax, Count, Acc ) ->
	get_random_values_helper( Nmin, Nmax, Count - 1,
							  [ get_random_value( Nmin, Nmax ) | Acc ] ).


% To test compilation:
%-define(use_crypto_module,).


% Now crypto is hardly used anymore:
-ifdef(use_crypto_module).


% crypto module used here.
%
% Warning: the seed and state management are presumably global (not
% per-process).


% Starts the random source with specified seeding.
start_random_source( _A, _B, _C ) ->
	throw( crypto_module_cannot_be_seeded ).



% Starts the random source with specified seeding.
start_random_source( default_seed ) ->

	?trace_random( "~w starting random source with crypto.", [ self() ] ),

	ok = crypto:start();

start_random_source( time_based_seed ) ->
	throw( crypto_module_cannot_be_seeded ).


% crypto cannot be seeded:
can_be_seeded() ->
	false.


% Resets the random source with a new seed.
reset_random_source( _Seed ) ->
	throw( crypto_module_cannot_be_reset ).


% Stops the random source.
stop_random_source() ->
	ok = crypto:stop().



% Returns a random float uniformly distributed between 0.0 and 1.0, updating the
% random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_random_value() ->
	% Not available: crypto:rand_uniform( 0.0, 1.0 ).
	throw( not_available ).



% Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_random_value( N ) ->
	crypto:rand_uniform( 1, N+1 ).



% Returns an integer random value generated from an uniform distribution in
% [Nmin;Nmax] (i.e. both bounds included), updating the random state in the
% process dictionary.
%
% Spec already specified, for all random settings.
%
get_random_value( Nmin, Nmax ) when Nmin =< Nmax ->
	crypto:rand_uniform( Nmin, Nmax+1 ).


% Returns a floating-point random value in [0.0;N[ generated from an uniform
% distribution.
%
% Given a number (integer or float) N (positive or not), returns a random
% floating-point value uniformly distributed between 0.0 (included) and N
% (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_uniform_floating_point_value( N ) ->
	throw( not_available ).


% Returns a floating-point random value in [Nmin, Nmax[ generated from an
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



% Returns the name of the module managing the random generation.
%
% Spec already specified, for all random settings.
%
-spec get_random_module_name() -> 'crypto'.
get_random_module_name() ->
	crypto.



% Returns the random state of this process (it is useful for example for process
% serialisations).
%
% Spec already specified, for all random settings.
%
get_random_state() ->
	% At least: not implemented yet.
	throw( not_available ).



% Sets the random state of this process (it is useful for example for process
% serialisations).
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



% Starts the random source with specified seeding.
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



% Seeds the random number generator, with specified seeding., or with a
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



% rand can be seeded:
can_be_seeded() ->
	true.



% Resets the random source with a new seed.
reset_random_source( Seed ) ->
	% New seeding, as opposed to the setting of a previously defined state:
	rand:seed( ?rand_algorithm, Seed ).



% Stops the random source.
stop_random_source() ->
	ok.



% Returns a random float uniformly distributed between 0.0 (included) and 1.0
% (excluded), updating the random state in the process dictionary.
%
% Spec already specified, for all random settings.
%
get_random_value() ->
	%random:uniform().
	rand:uniform().



% Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
% Spec already specified, for all random settings.
%
get_random_value( N ) when is_integer( N ) ->
	%random:uniform( N ).
	rand:uniform( N );

get_random_value( N ) ->
	throw( { not_integer, N } ).



% Returns an integer random value generated from an uniform distribution in
% [Nmin;Nmax] (i.e. both bounds included), updating the random state in the
% process dictionary.
%
% Spec already specified, for all random settings.
%
get_random_value( Nmin, Nmax ) when is_integer( Nmin )
					andalso is_integer( Nmax ) andalso Nmin =< Nmax ->

	% Ex: if Nmin = 3, Nmax = 5, we can draw value in [3, 4, 5], hence:
	%
	% N = 5 - 3 + 1 = 3.
	%
	N = Nmax - Nmin + 1,

	% Drawn in [1;N]:
	get_random_value( N ) + Nmin - 1;

get_random_value( Nmin, Nmax ) ->
	throw( { not_integer_bounds, { Nmin, Nmax } } ).




% Returns a floating-point random value in [0.0;N[ generated from an uniform
% distribution.
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


% Returns a floating-point random value in [Nmin, Nmax[ generated from an
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



% Returns the name of the module managing the random generation.
%
% Spec already specified, for all random settings.
%
-spec get_random_module_name() -> 'rand'. %'random'.
get_random_module_name() ->
	%random.
	rand.


% Returns the random state of the current process (it is useful for example for
% process serialisations).
%
% Spec already specified, for all random settings.
%
get_random_state() ->

	% Read from the process dictionary:

	% Actually, no state should not be considered as an error:
	%case erlang:get( random_seed ) of
	%
	%		undefined ->
	%		% Probably that there has been not prior seeding:
	%		throw( random_state_not_available );
	%
	%	S ->
	%		S
	%
	%end.

	% May return 'undefined', if not seeded yet:
	%erlang:get( random_seed ).
	%erlang:get( rand_seed ).
	% Best:
	rand:export_seed().



% Sets the random state of this process (it is useful for example for process
% serialisations).
%
% Spec already specified, for all random settings.
%
set_random_state( RandomState ) ->

	% Process dictionary:
	%erlang:put( random_seed, NewState ).
	%erlang:put( rand_seed, NewState ).
	rand:seed( RandomState ).


-endif. % use_crypto_module not defined




% Section which does not depend on defines.



% Returns a list of the specified number of unique elements drawn from input
% list (i.e. so that there is no duplicate in the returned list).
%
% Note: defined to ease interface look-up, use directly
% list_utils:draw_elements_from/2 instead.
%
-spec get_random_subset( count(), list() ) -> list().
get_random_subset( ValueCount, InputList ) ->
	list_utils:draw_elements_from( InputList, ValueCount ).



% The upper bound for a seed element.
-define( seed_upper_bound, 65500 ).


% Returns a seed obtained from the random source in use.
%
% This is a randomly-determined seed, meant to be used to create another random
% generator.
%
-spec get_random_seed() -> seed().
get_random_seed() ->
	{ get_random_value( ?seed_upper_bound ),
	  get_random_value( ?seed_upper_bound ),
	  get_random_value( ?seed_upper_bound ) }.



% Checks that the specified seed is valid.
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
