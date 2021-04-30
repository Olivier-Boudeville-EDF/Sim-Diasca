% Copyright (C) 2008-2021 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Authors: Olivier Boudeville  (olivier.boudeville@edf.fr)
%          Samuel Thiriot      (samuel.thiriot@edf.fr)


-module(class_RandomManager).


-define( class_description,
		 "Management of random number generation. "
		 "Now all actors (of course including stochastic ones) have their "
		 "own (private, well-initialised) random generator, while still "
		 "being able to preserve reproducibility. "
		 "As a consequence, the random manager module is now mostly useful "
		 "for the (static) functions for stochastic laws that it exports."
		 "Inspired from http://www.trapexit.org/Random_Numbers_Biased. "
		 "Note: it is possible to request only a seed from a RandomManager, "
		 "and to generate afterwards one's own random series. That is what the "
		 "class_Actor instances do, indirectly." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).


% The class-specific attributes of a random manager:
-define( class_attributes, [

	{ is_private, boolean(),
	  "tells whether this manager is private to an actor" } ] ).



% Section for the description of random laws.

-type uniform_law() :: { 'uniform', pos_integer() }.


% Parameter is Lambda:
%
% (refer to https://en.wikipedia.org/wiki/Exponential_distribution)
%
-type exponential_law() :: { 'exponential', number() }.

-type positive_integer_exponential_law() ::
						{ 'positive_integer_exponential', number() }.


-type mean() :: number().

-type standard_deviation() :: math_utils:standard_deviation().

% The parameters of a gaussian/normal law are respectively Mu and Sigma:
-type gaussian_law() ::
		{ 'gaussian', Mu :: mean(), Sigma :: standard_deviation() }.


-type positive_integer_gaussian_law() ::
		{ 'positive_integer_gaussian', float(), float() }.


-type random_law() :: uniform_law()
					| exponential_law()
					| positive_integer_exponential_law()
					| gaussian_law()
					| positive_integer_gaussian_law().

-type manager_pid() :: sim_diasca:agent_pid().

-export_type([ random_law/0, manager_pid/0 ]).




% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "RandomManagement" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% For random constants:
-include("class_RandomManager.hrl").



% Implementation notes:
%
% There are three basic classes of built-in random distributions:
% - uniform (a.k.a. white noise)
% - exponential
% - Gaussian (a.k.a. normal)
%
%
% For each distribution law, following variations are available:
%
% - a simple non-synchronized method (ex: getUniformValue/2), returning one
% value to the caller PID (not necessarily an actor)
%
% - a simple synchronized method (ex: getUniformValue/3), returning one value to
% the caller actor
%
% - a more complex synchronized method (ex: getUniformValues/3) [note the final
% 's'), returning a series of values and an identifier to the caller actor so
% that the caller is able to track multiple distributions simulatenously
%
% The last two cases use actor oneways, they send their answer through a oneway
% actor call to the caller actor, calling corresponding set*Value/ set*Values
% actor method (ex: setUniformValue).
%
% The uniform law can be based either on the random module (random:uniform/1) or
% on the crypto module (crypto:rand_uniform/2). The two forms yield different
% but quite similar results. See use_crypto_module below.
%
% We rely on random_utils:get_random_value which allows to swap implementations.
%
% Exponential and Gaussian laws generate by default floating-point numbers.
%
% For convenience, counterparts returning positive integer values have been
% defined (ex: getGaussianValue/getPositiveIntegerGaussianValue).


% Regarding inverse transform sampling, refer to
% https://en.wikipedia.org/wiki/Inverse_transform_sampling.


% A Gaussian (a.k.a. normal, bell curve) law is fully determined when two
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



% Where a random manager should be registered.
%
% Could be local_only or global_only as well:
%
-define( registration_scope, local_and_global ).


-type seed_info() :: random_utils:random_state()
				   | 'default_seed'
				   | 'time_based_seed'.


% Shorthand:
-type count() :: basic_utils:count().


% Constructs a new random manager, from:
%
% - SeedInformations allows to choose the random seed to be used, it can be:
%
%   - a triplet {A,B,C}, to set explicitly the seed, to be tailored for
%   reproducibility or for ergodic mode
%
%   - default_seed, to use default (fixed) values in the process dictionary
%
%   - time_based_seed, as they are based on current time, each run will result
%   in different random series
%
% - IsPrivate tells whether this random manager will be privately held (hence
% should not be registered in naming service) or if it is a (registered)
% singleton (can be the atom true of false)
%
-spec construct( wooper:state(), seed_info(), boolean() ) -> wooper:state().
construct( State, SeedInformations, IsPrivate ) ->

	% First the direct mother classes:
	TraceState = class_EngineBaseObject:construct( State,
										?trace_categorize("RandomManager") ),

	class_InstanceTracker:register_agent( State ),

	% Then the class-specific actions:
	StartingState = setAttribute( TraceState, is_private, IsPrivate ),

	case IsPrivate of

		true ->
			?send_info( StartingState, "Creating a private random manager." );

		false ->
			?send_info( StartingState, "Creating a public random manager." ),

			try

				naming_utils:register_as( ?random_manager_name,
										  ?registration_scope )

			catch

				Exception ->
					?send_error( StartingState,
								 "Random manager could not be registered." ),
					throw( { random_manager_could_not_register, Exception } )

			end,

			?send_debug_fmt( StartingState, "Random manager registered as ~w.",
							 [ ?registration_scope ] )

	end,

	?send_info_fmt( StartingState,
		"Random manager will use following seed information: ~w.",
		[ SeedInformations ] ),

	random_utils:start_random_source( SeedInformations ),

	?send_debug( StartingState, "Random manager constructed." ),

	StartingState.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?notice( "Deleting random manager." ),

	% Private random managers not owned.
	random_utils:stop_random_source(),

	case ?getAttr(is_private) of

		true ->
			ok;

		false ->
			naming_utils:unregister( ?random_manager_name, ?registration_scope )

	end,

	class_InstanceTracker:unregister_agent(),

	?debug( "Random manager deleted." ),

	% Then allow chaining:
	State.




% Methods section.


% For each random distribution, there is at least:
% - a request-based method
% - a static method, which thus relies on the state of random generation of the
% caller process


% All these functions are doubled, to support the request of one random value or
% a given number of values.
%
% Finally, distributions that may return floating-point values (ex: exponential,
% Gaussian) have also versions that return positive integer values.





% Uniform section.


% Returns a boolean random value generated from an uniform distribution.
% Therefore true and false are equally likely to be returned.
%
-spec get_boolean() -> static_return( boolean() ).
get_boolean() ->
	wooper:return_static( random_utils:get_random_value( 0, 100 ) >= 49 ).



% Returns a randomly-selected element of specified list.
-spec one_of( [ any() ] ) -> static_return( any() ).
one_of( ListOfThings ) ->

	Index = random_utils:get_random_value( length( ListOfThings ) ),

	wooper:return_static( lists:nth( Index, ListOfThings ) ).



% Returns an integer random value generated from an uniform distribution, in
% specified range.
%
% Given two integers Nmin and Nmax, returns a random integer uniformly
% distributed between these two bounds (both included), updating the random
% state in the process dictionary.
%
-spec getUniformValue( wooper:state(), integer(), integer() ) ->
				const_request_return( { 'uniform_value', integer() } ).
getUniformValue( State, Nmin, Nmax ) ->

	Value = random_utils:get_random_value( Nmin, Nmax ),

	%?debug_fmt( "Returning uniform value ~w.", [ Value ] ),

	wooper:const_return_result( { uniform_value, Value } ).



% Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
-spec getUniformValue( wooper:state(), pos_integer() ) ->
				const_request_return( { 'uniform_value', pos_integer() } ).
getUniformValue( State, N ) ->

	Value = random_utils:get_random_value( N ),

	%?debug_fmt( "Returning uniform value ~w.", [ Value ] ),

	wooper:const_return_result( { uniform_value, Value } ).



% Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
-spec get_uniform_value( pos_integer() ) -> static_return( pos_integer() ).
get_uniform_value( N ) ->
	wooper:return_static( random_utils:get_random_value( N ) ).



% Returns an integer random value generated from an uniform distribution, in
% specified range.
%
% Given two integers Nmin and Nmax, returns a random integer uniformly
% distributed between these two bounds (both included), updating the random
% state in the process dictionary.
%
-spec get_uniform_value( integer(), integer() ) -> static_return( integer() ).
get_uniform_value( Nmin, Nmax ) ->
	wooper:return_static( random_utils:get_random_value( Nmin, Nmax ) ).



% Returns a list of Count integer uniform values in [ 1, N ] (both included).
%
% Given an integer N >= 1, returns random integers uniformly distributed between
% 1 and N, updating the random state in the process dictionary.
%
-spec get_uniform_values( pos_integer(), count() ) ->
								static_return( [ pos_integer() ] ).
get_uniform_values( N, Count ) ->
	wooper:return_static( random_utils:get_random_values( N, Count ) ).



% Returns a list of Count integer uniform values in [Nmin,Nmax] (both included),
% updating the random state in the process dictionary.
%
-spec get_uniform_values( integer(), integer(), count() ) ->
							static_return( [ integer() ] ).
get_uniform_values( Nmin, Nmax, Count ) ->
	wooper:return_static( random_utils:get_random_values( Nmin, Nmax, Count ) ).



% Returns a floating-point random value in [0.0;N[ generated from an uniform
% distribution.
%
% Given a number (integer or float) N (positive or not), returns a random
% floating-point value uniformly distributed between 0.0 (included) and N
% (excluded), updating the random state in the process dictionary.
%
-spec get_uniform_floating_point_value( number() ) -> static_return( float() ).
get_uniform_floating_point_value( N ) ->
	V = random_utils:get_uniform_floating_point_value( N ),
	wooper:return_static( V ).



% Returns a floating-point random value in [Nmin, Nmax[ generated from an
% uniform distribution.
%
% Given two numbers (integer or float) Nmin and Nmax (each being positive or
% not), returns a random floating-point value uniformly distributed between Nmin
% (included) and Nmax (excluded), updating the random state in the process
% dictionary.
%
-spec get_uniform_floating_point_value( number(), number() ) ->
											  static_return( float() ).
get_uniform_floating_point_value( Nmin, Nmax ) ->
	V = random_utils:get_uniform_floating_point_value( Nmin, Nmax ),
	wooper:return_static( V ).



% Exponential section.
%
% Note: each of the three forms comes in two versions, with floating-point or
% (positive) integer values being returned.




% Returns an exponential floating-point random value with Lambda being the rate
% parameter.
%
% The probability density function is p(x) = Lambda.exp(-Lambda.x), whose
% integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
-spec getExponentialValue( wooper:state(), number() ) ->
				const_request_return( { 'exponential_value', float() } ).
getExponentialValue( State, Lambda ) ->

	Value = get_exponential_value( Lambda ),

	%?debug_fmt( "Returning exponential value ~w.", [ Value ] ),

	wooper:const_return_result( { exponential_value, Value } ).



% Returns an exponential floating-point random value with Lambda being the rate
% parameter.
%
% The probability density function is p(x) = Lambda.exp(-Lambda.x), whose
% integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
-spec get_exponential_value( number() ) -> static_return( float() ).
get_exponential_value( Lambda ) ->

	% Note: with Erlang, math:log(x) is ln(x):
	V = - math:log( random_utils:get_random_value() ) / Lambda,

	wooper:return_static( V ).




% Returns an exponential (positive) integer random value with Lambda being the
% rate parameter.
%
% The probability density function is p(x) = Lambda.exp(-Lambda.x), whose
% integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
-spec getPositiveIntegerExponentialValue( wooper:state(), number() ) ->
	const_request_return( { 'positive_integer_exponential_value', integer() } ).
getPositiveIntegerExponentialValue( State, Lambda ) ->

	Value = round( get_exponential_value( Lambda ) ),

	%?debug_fmt( "Returning positive integer exponential value ~w.",
	%			[ Value ] ),

	wooper:const_return_result(
		{ positive_integer_exponential_value, Value } ).



% Returns an exponential (positive) integer random value with Lambda being the
% rate parameter.
%
% The probability density function is p(x) = Lambda.exp(-Lambda.x), whose
% integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
-spec get_positive_integer_exponential_value( number() ) ->
													static_return( integer() ).
get_positive_integer_exponential_value( Lambda ) ->
	wooper:return_static( round( get_exponential_value( Lambda ) ) ).



% Returns a list of Count exponential values according to the specified Lambda
% setting.
%
% Lambda is the rate parameter: the probability density function is
% p(x) = Lambda.exp(-Lambda.x), whose integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
-spec get_exponential_values( number(), count() ) ->
									static_return( [ float() ] ).
get_exponential_values( Lambda, Count ) ->
	V = generate_exponential_list( Lambda, Count ),
	wooper:return_static( V ).



% Returns a list of Count (positive) integer exponential values according to the
% specified Lambda setting.
%
% Lambda is the rate parameter: the probability density function is
% p(x) = Lambda.exp(-Lambda.x), whose integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
-spec get_positive_integer_exponential_values( number(), count() ) ->
										static_return( [ pos_integer() ] ).
get_positive_integer_exponential_values( Lambda, Count ) ->
	V = generate_positive_integer_exponential_list( Lambda, Count ),
	wooper:return_static( V ).




% Gaussian section.
%
% Note: each of the three forms comes in two versions, with floating-point or
% positive integer values being returned.
%
% Note also that the function order matters, and some share some arities.




% Reordering was needed, as there are two getGaussianValue/3 (one request/one
% actor oneway).



% Returns a random value generated from the normal (Gaussian) distribution with
% specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns a random
% floating-point value drawn according to the corresponding Gaussian law,
% updating the state in the process dictionary.
%
-spec getGaussianValue( wooper:state(), number(), number() ) ->
							const_request_return( { gaussian_value, float() } ).
getGaussianValue( State, Mu, Sigma ) ->

	Value = sigma_loop( Mu, Sigma ),

	%?debug_fmt( "Returning Gaussian value ~w.", [ Value ] ),

	wooper:const_return_result( { gaussian_value, Value } ).



% Returns a random value generated from the normal (Gaussian) distribution with
% specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns a random
% floating-point value drawn according to the corresponding Gaussian law,
% updating the state in the process dictionary.
%
-spec get_gaussian_value( number(), number() ) -> static_return( float() ).
get_gaussian_value( Mu, Sigma ) ->
	wooper:return_static( sigma_loop( Mu, Sigma ) ).





% Returns a positive integer random value generated from the normal (Gaussian)
% distribution with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
% The result is a non-negative integer (not a float). Values will be drawn until
% they are non-negative.
%
-spec getPositiveIntegerGaussianValue( wooper:state(), number(), number() ) ->
	const_request_return( { positive_integer_gaussian_value, pos_integer() } ).
getPositiveIntegerGaussianValue( State, Mu, Sigma ) ->

	Value = sigma_loop_positive_integer( Mu, Sigma ),

	%?debug_fmt( "Returning positive integer Gaussian value ~w.", [ Value ] ),

	wooper:const_return_result(
		{ positive_integer_gaussian_value, Value } ).



% Returns a positive integer random value generated from the normal (Gaussian)
% distribution with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
% The result is a non-negative integer (not a float). Values will be drawn until
% they are non-negative.
%
-spec get_positive_integer_gaussian_value( number(), number() ) ->
												static_return( pos_integer() ).
get_positive_integer_gaussian_value( Mu, Sigma ) ->
	V = sigma_loop_positive_integer( Mu, Sigma ),
	wooper:return_static( V ).



% Returns a list of Count Gaussian values.
%
% Given a mean Mu and a standard deviation Sigma, returns random floating-point
% values drawn according the corresponding Gaussian law, updating the state in
% the process dictionary.
%
-spec get_gaussian_values( number(), number(), count() ) ->
									static_return( [ float() ] ).
get_gaussian_values( Mu, Sigma, Count ) ->
	Values = generate_gaussian_list( Mu, Sigma, Count ),
	wooper:return_static( Values ).



% Returns a list of Count positive integer Gaussian values.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
-spec get_positive_integer_gaussian_values( number(), number(), count() ) ->
										static_return( [ pos_integer() ] ).
get_positive_integer_gaussian_values( Mu, Sigma, Count ) ->
	Values = generate_positive_integer_gaussian_list( Mu, Sigma, Count ),
	wooper:return_static( Values ).



% Returns a new seed triplet.
-spec get_new_seed() -> static_return( random_utils:seed() ).
get_new_seed() ->
	wooper:return_static( random_utils:get_random_seed() ).



% Creates the random manager asynchronously, with default settings (mean of
% zero, sigma of 1).
%
-spec create() -> static_return( manager_pid() ).
create() ->

	% Not created here as an actor:
	ManagerPid = new_link( _SeedInformations=default_seed, _IsPrivate=false ),

	wooper:return_static( ManagerPid ).



% Returns the PID of the current random manager if it exists, otherwise
% random_manager_not_found.
%
% Waits a bit before giving up: useful when client and manager processes may be
% launched almost simultaneously.
%
-spec getManager() -> static_return( manager_pid() ).
getManager() ->

	% Waits gracefully for the random manager to exist:
	ManagerPid = naming_utils:wait_for_global_registration_of(
				   ?random_manager_name ),

	wooper:return_static( ManagerPid ).



% Deletes (asynchronously) any global random manager.
-spec remove() -> static_return( 'ok' | 'random_manager_not_found' ).
remove() ->

	case global:whereis_name( ?random_manager_name ) of

		undefined ->
			wooper:return_static( random_manager_not_found );

		RandomManagerPid ->
			RandomManagerPid ! delete,
			% It will unregister itself.
			wooper:return_static( ok )

	end.



% Section for helper functions (not methods).


% generate_*_list could use higher-order functions.



% Generates a list of Count exponential random values.
-spec generate_exponential_list( number(), count() ) -> [ float() ].
generate_exponential_list( Lambda, Count ) ->
	generate_exponential_list( Lambda, Count, [] ).


generate_exponential_list( _Lambda, _Count=0, Acc ) ->
	Acc;

generate_exponential_list( Lambda, Count, Acc ) ->
	generate_exponential_list( Lambda, Count-1,
							   [ get_exponential_value( Lambda )  | Acc ] ).



% Generates a list of Count positive integer exponential random values.
-spec generate_positive_integer_exponential_list( number(),
						   count() ) -> [ pos_integer() ].
generate_positive_integer_exponential_list( Lambda, Count ) ->
	generate_positive_integer_exponential_list( Lambda, Count, [] ).


generate_positive_integer_exponential_list( _Lambda, _Count=0, Acc ) ->
	Acc;

generate_positive_integer_exponential_list( Lambda, Count, Acc ) ->
	generate_positive_integer_exponential_list( Lambda, Count-1,
		[ erlang:round( get_exponential_value( Lambda ) ) | Acc ] ).



% Generates a list of Count Gaussian random values.
-spec generate_gaussian_list( number(), number(), count() ) ->
									[ float() ].
generate_gaussian_list( Mu, Sigma, Count ) ->
	generate_gaussian_list( Mu, Sigma, Count, [] ).


generate_gaussian_list( _Mu, _Sigma, _Count=0, Acc ) ->
	Acc;

generate_gaussian_list( Mu, Sigma, Count, Acc ) ->
	generate_gaussian_list( Mu, Sigma, Count-1,
							[ sigma_loop( Mu, Sigma )  | Acc ] ).



% Generates a list of Count positive integer Gaussian random values.
-spec generate_positive_integer_gaussian_list( number(), number(), count() ) ->
													 [ pos_integer() ].
generate_positive_integer_gaussian_list( Mu, Sigma, Count ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count, [] ).


generate_positive_integer_gaussian_list( _Mu, _Sigma, _Count=0, Acc ) ->
	Acc;

generate_positive_integer_gaussian_list( Mu, Sigma, Count, Acc ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count-1,
		[ erlang:round( sigma_loop_positive_integer( Mu, Sigma ) ) | Acc ] ).



% Generates a new normal value and updates the state.
%
% Mu is the mean, Sigma is the standard deviation (variance being its square).
%
% Returns the computed value.
%
% See also
% https://en.wikipedia.org/wiki/Normal_distribution#Computational_methods
%
-spec sigma_loop( number(), number() ) -> float().
sigma_loop( Mu, Sigma ) ->

	% Best (most efficient) implementation that could be used in the future:
	% https://en.wikipedia.org/wiki/Ziggurat_algorithm

	% Note: at least for Mu=0, Sigma=1, rand:normal/0 could be used.

	% Using here the second best approach, the Marsaglia polar method (see
	% https://en.wikipedia.org/wiki/Marsaglia_polar_method); used for example by
	% C++11 GNU GCC libstdc++.

	% random_utils:get_random_value/0 returns a random float uniformly
	% distributed between 0.0 (included) and 1.0 (excluded), updating the random
	% state in the process dictionary.

	% So V1 and V2 are in [-1.0;1.0[:
	V1 = 2.0 * random_utils:get_random_value() - 1.0,

	% Supposedly independent from V1:
	V2 = 2.0 * random_utils:get_random_value() - 1.0,

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
			%					   [ Mu, Sigma, S ] ),

			% math:log/1 is the Natural Log (base e log):
			Scale = math:sqrt( ( -2.0 * math:log( S ) ) / S ),

			% Adjust for standard deviation and mean:
			Mu + ( Sigma * Scale * V1)

	end.



% Generates a new integer non-negative normal value and updates the state.
%
% Returns the computed value.
%
-spec sigma_loop_positive_integer( number(), number() ) -> pos_integer().
sigma_loop_positive_integer( Mu, Sigma ) ->

	% Loops until a positive integer is found:
	case round( sigma_loop( Mu, Sigma ) ) of

		TriedValue when TriedValue < 0 ->
			sigma_loop_positive_integer( Mu, Sigma );

		NonNegativeValue ->
			NonNegativeValue

	end.
