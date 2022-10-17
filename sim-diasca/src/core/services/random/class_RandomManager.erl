% Copyright (C) 2008-2022 EDF R&D

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
% Creation date: 2008.


% @doc Class offering services in terms of <b>random number generation</b>.
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
		 "class_Actor instances do, indirectly. "
		 "See also: the random_utils module." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).


% The class-specific attributes of a random manager:
-define( class_attributes, [

	{ is_private, boolean(),
	  "tells whether this manager is private to an actor" } ] ).



-type manager_pid() :: sim_diasca:agent_pid().

-export_type([ manager_pid/0 ]).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "RandomManagement" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% For random constants:
-include("class_RandomManager.hrl").


% Note: for an increased genericity, all general-purpose lower-level elements
% are defined in / have been moved to Myriad's random_utils module. As the
% management of an explicit state and the offering of a centralised random
% service for actors is not necessary anymore, the usefullness of this module
% considerably decreased.



% Implementation notes:
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
% We rely on random_utils:get_uniform_value/1, which allows to swap
% implementations.
%
% Exponential and Gaussian laws generate by default floating-point numbers.
%
% For convenience, counterparts returning positive integer values have been
% defined (ex: getGaussianValue/getPositiveIntegerGaussianValue).


% Where a random manager should be registered.
%
% Could be local_only or global_only as well:
%
-define( registration_scope, local_and_global ).


-type seed_info() :: random_utils:random_state()
				   | 'default_seed'
				   | 'time_based_seed'.


% Shorthands:

-type count() :: basic_utils:count().

-type rate() :: random_utils:rate().
-type mean() :: random_utils:mean().
-type standard_deviation() :: random_utils:standard_deviation().



% @doc Constructs a random manager.
%
% Construction parameters:
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



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?notice( "Deleting random manager." ),

	% Private random managers not owned.
	random_utils:stop_random_source(),

	?getAttr(is_private) orelse
		naming_utils:unregister( ?random_manager_name, ?registration_scope ),

	class_InstanceTracker:unregister_agent(),

	?debug( "Random manager deleted." ),

	% Then allow chaining:
	State.




% Methods section.


% For each random distribution, there is at least:
%  - a request-based method
%  - a static method, which thus relies on the state of random generation of the
%  caller process


% All these functions are doubled, to support the request of one random value or
% a given number of values.
%
% Finally, distributions that may return floating-point values (ex: exponential,
% Gaussian) have also versions that return positive integer values.




% Uniform section.


% @doc Returns a boolean random value generated from an uniform distribution.
%
% Therefore true and false are equally likely to be returned.
%
-spec get_boolean() -> static_return( boolean() ).
get_boolean() ->
	wooper:return_static( random_utils:get_boolean() ).



% @doc Returns a randomly-selected element of the specified list.
-spec one_of( [ any() ] ) -> static_return( any() ).
one_of( ListOfThings ) ->
	wooper:return_static( random_utils:one_of( ListOfThings ) ).



% @doc Returns an integer random value generated from an uniform distribution,
% in specified range.
%
% Given two integers Nmin and Nmax, returns a random integer uniformly
% distributed between these two bounds (both included), updating the random
% state in the process dictionary.
%
-spec getUniformValue( wooper:state(), integer(), integer() ) ->
					const_request_return( { 'uniform_value', integer() } ).
getUniformValue( State, Nmin, Nmax ) ->

	Value = random_utils:get_uniform_value( Nmin, Nmax ),

	%?debug_fmt( "Returning uniform value ~w.", [ Value ] ),

	wooper:const_return_result( { uniform_value, Value } ).



% @doc Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
-spec getUniformValue( wooper:state(), pos_integer() ) ->
				const_request_return( { 'uniform_value', pos_integer() } ).
getUniformValue( State, N ) ->

	Value = random_utils:get_uniform_value( N ),

	%?debug_fmt( "Returning uniform value ~w.", [ Value ] ),

	wooper:const_return_result( { uniform_value, Value } ).



% @doc Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
-spec get_uniform_value( pos_integer() ) -> static_return( pos_integer() ).
get_uniform_value( N ) ->
	wooper:return_static( random_utils:get_uniform_value( N ) ).



% @doc Returns an integer random value generated from an uniform distribution,
% in specified range.
%
% Given two integers Nmin and Nmax, returns a random integer uniformly
% distributed between these two bounds (both included), updating the random
% state in the process dictionary.
%
-spec get_uniform_value( integer(), integer() ) -> static_return( integer() ).
get_uniform_value( Nmin, Nmax ) ->
	wooper:return_static( random_utils:get_uniform_value( Nmin, Nmax ) ).



% @doc Returns a list of Count integer uniform values in [1, N] (both included).
%
% Given an integer N >= 1, returns random integers uniformly distributed between
% 1 and N, updating the random state in the process dictionary.
%
-spec get_uniform_values( pos_integer(), count() ) ->
								static_return( [ pos_integer() ] ).
get_uniform_values( N, Count ) ->
	wooper:return_static( random_utils:get_uniform_values( N, Count ) ).



% @doc Returns a list of Count integer uniform values in [Nmin,Nmax] (both
% included), updating the random state in the process dictionary.
%
-spec get_uniform_values( integer(), integer(), count() ) ->
								static_return( [ integer() ] ).
get_uniform_values( Nmin, Nmax, Count ) ->
	wooper:return_static(
		random_utils:get_uniform_values( Nmin, Nmax, Count ) ).



% @doc Returns a floating-point random value in [0.0;N[ generated from an
% uniform distribution.
%
% Given a number (integer or float) N (positive or not), returns a random
% floating-point value uniformly distributed between 0.0 (included) and N
% (excluded), updating the random state in the process dictionary.
%
-spec get_uniform_floating_point_value( number() ) -> static_return( float() ).
get_uniform_floating_point_value( N ) ->
	V = random_utils:get_uniform_floating_point_value( N ),
	wooper:return_static( V ).



% @doc Returns a floating-point random value in [Nmin, Nmax[ generated from an
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



% @doc Returns an exponential floating-point random value with Lambda being the
% rate parameter.
%
% See the random_utils module for further details.
%
-spec getExponentialValue( wooper:state(), rate() ) ->
				const_request_return( { 'exponential_value', float() } ).
getExponentialValue( State, Lambda ) ->

	Value = random_utils:get_exponential_value( Lambda ),

	%?debug_fmt( "Returning exponential value ~w.", [ Value ] ),

	wooper:const_return_result( { exponential_value, Value } ).



% @doc Returns an exponential random value according to the specified Lambda
% rate parameter.
%
% See the random_utils module for further details.
%
-spec get_exponential_value( rate() ) ->
									static_return( non_neg_integer() ).
get_exponential_value( Lambda ) ->
	Value = random_utils:get_exponential_value( Lambda ),
	wooper:return_static( Value ).



% @doc Returns an exponential (positive) integer random value with Lambda being
% the rate parameter.
%
% See the random_utils module for further details.
%
-spec getPositiveIntegerExponentialValue( wooper:state(), rate() ) ->
	const_request_return( { 'positive_integer_exponential_value',
							non_neg_integer() } ).
getPositiveIntegerExponentialValue( State, Lambda ) ->

	Value = random_utils:get_positive_integer_exponential_value( Lambda ),

	%?debug_fmt( "Returning positive integer exponential value ~w.",
	%            [ Value ] ),

	wooper:const_return_result(
		{ positive_integer_exponential_value, Value } ).



% @doc Returns an exponential (positive) integer random value with Lambda being
% the rate parameter.
%
% See the random_utils module for further details.
%
-spec get_positive_integer_exponential_value( rate() ) ->
									static_return( non_neg_integer() ).
get_positive_integer_exponential_value( Lambda ) ->
	Value = random_utils:get_positive_integer_exponential_value( Lambda ),
	wooper:return_static( Value ).



% @doc Returns a list of Count exponential values according to the specified
% Lambda rate parameter.
%
% See the random_utils module for further details.
%
-spec get_exponential_values( rate(), count() ) ->
											static_return( [ float() ] ).
get_exponential_values( Lambda, Count ) ->
	V = random_utils:get_exponential_values( Lambda, Count ),
	wooper:return_static( V ).



% @doc Returns a list of Count (positive) integer exponential values according
% to the specified Lambda rate parameter.
%
-spec get_positive_integer_exponential_values( rate(), count() ) ->
										static_return( [ pos_integer() ] ).
get_positive_integer_exponential_values( Lambda, Count ) ->
	V = random_utils:generate_positive_integer_exponential_list( Lambda,
																 Count ),
	wooper:return_static( V ).




% Gaussian section.
%
% Note: each of the three forms comes in two versions, with floating-point or
% positive integer values being returned.
%
% Note also that the function order matters, and some share some arities.




% Reordering was needed, as there are two getGaussianValue/3 (one request/one
% actor oneway).



% @doc Returns a random value generated from the normal (Gaussian) distribution
% with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns a random
% floating-point value drawn according to the corresponding Gaussian law,
% updating the state in the process dictionary.
%
-spec getGaussianValue( wooper:state(), mean(), standard_deviation() ) ->
							const_request_return( { gaussian_value, float() } ).
getGaussianValue( State, Mu, Sigma ) ->

	Value = random_utils:get_gaussian_value( Mu, Sigma ),

	%?debug_fmt( "Returning Gaussian value ~w.", [ Value ] ),

	wooper:const_return_result( { gaussian_value, Value } ).



% @doc Returns a random value generated from the normal (Gaussian) distribution
% with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns a random
% floating-point value drawn according to the corresponding Gaussian law,
% updating the state in the process dictionary.
%
-spec get_gaussian_value( mean(), standard_deviation() ) ->
									static_return( float() ).
get_gaussian_value( Mu, Sigma ) ->
	Value = random_utils:get_gaussian_value( Mu, Sigma ),
	wooper:return_static( Value ).




% @doc Returns a positive integer random value generated from the normal
% (Gaussian) distribution with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
% The result is a non-negative integer (not a float). Values will be drawn until
% they are non-negative.
%
-spec getPositiveIntegerGaussianValue( wooper:state(), mean(),
									   standard_deviation() ) ->
	const_request_return( { positive_integer_gaussian_value,
							non_neg_integer() } ).
getPositiveIntegerGaussianValue( State, Mu, Sigma ) ->

	Value = random_utils:get_positive_integer_gaussian_value( Mu, Sigma ),

	%?debug_fmt( "Returning positive integer Gaussian value ~w.", [ Value ] ),

	wooper:const_return_result(
		{ positive_integer_gaussian_value, Value } ).



% @doc Returns a positive integer random value generated from the normal
% (Gaussian) distribution with specified settings.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
% The result is a non-negative integer (not a float). Values will be drawn until
% they are non-negative.
%
-spec get_positive_integer_gaussian_value( mean(), standard_deviation() ) ->
												static_return( pos_integer() ).
get_positive_integer_gaussian_value( Mu, Sigma ) ->
	V = random_utils:get_positive_integer_gaussian_value( Mu, Sigma ),
	wooper:return_static( V ).



% @doc Returns a list of Count Gaussian values.
%
% Given a mean Mu and a standard deviation Sigma, returns random floating-point
% values drawn according the corresponding Gaussian law, updating the state in
% the process dictionary.
%
-spec get_gaussian_values( mean(), standard_deviation(), count() ) ->
									static_return( [ float() ] ).
get_gaussian_values( Mu, Sigma, Count ) ->
	Values = random_utils:get_gaussian_values( Mu, Sigma, Count ),
	wooper:return_static( Values ).



% @doc Returns a list of Count positive integer Gaussian values.
%
% Given a mean Mu and a standard deviation Sigma, returns random integers drawn
% according the corresponding Gaussian law, updating the state in the process
% dictionary.
%
-spec get_positive_integer_gaussian_values( mean(), standard_deviation(),
						count() ) -> static_return( [ non_neg_integer() ] ).
get_positive_integer_gaussian_values( Mu, Sigma, Count ) ->

	Values = random_utils:get_positive_integer_gaussian_values( Mu, Sigma,
																Count ),

	wooper:return_static( Values ).



% @doc Returns a new seed triplet.
-spec get_new_seed() -> static_return( random_utils:seed() ).
get_new_seed() ->
	wooper:return_static( random_utils:get_random_seed() ).



% @doc Creates the random manager asynchronously, with default settings (mean of
% zero, sigma of 1).
%
-spec create() -> static_return( manager_pid() ).
create() ->

	% Not created here as an actor:
	ManagerPid = new_link( _SeedInformations=default_seed, _IsPrivate=false ),

	wooper:return_static( ManagerPid ).



% @doc Returns the PID of the current random manager if it exists, otherwise
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



% @doc Deletes (asynchronously) any global random manager.
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
