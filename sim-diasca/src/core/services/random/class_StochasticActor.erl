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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)
% Creation date: 2008.


% @doc This child class offers basic services in order to easily <b>obtain
% values from random laws</b>.
%
% Basic actors may also directly generate their random values (see
% class_RandomManager.erl for that).
%
% Important note: this way of managing random values is deprecated, since a full
% stochastic support is now available to all instances of class_Actor. As a
% consequence, this class is itself scheduled for deprecation and removal.
%
-module(class_StochasticActor).


-define( class_description,
		 "Stochastic actor class, for actors whose behaviour is at least "
		 "partly ruled by random laws. "
		 "See class_StochasticActor_test.erl." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


% The attributes that are specific to a stochastic actor are:
-define( class_attributes, [

	{ random_laws, [ law_entry() ],
	  "a list of all the available random laws for that stochastic actor" } ]).



% Helper functions:
-export([ get_random_value_from/2, add_law/3, remove_law/2 ]).




% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.StochasticActor" ).


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").



% Implementation notes:
%
% Each stochastic actor now *is* (implicitly) now a random manager of its own,
% so that it can rely on any number of random values with zero latency.
%
% Previously, two approaches could be used:
%
% - either use the previous stochastic actor class, which included a generic
% automatic mechanism to buffer appropriately random values retrieved from a
% singleton public random manager
%
% - or spawn a private random manager that would be used by this instance only
%
% First approach was complex and needed to set an upper bound to the number of
% stochastic values that could be requested per tick, second approach induced
% one extra process per actor and thus hindered scalability.
%
% The new approach is better, and more efficient and flexible than both of them
% (see class_Actor).


% Request identifiers allow to designate a particular law of a stochastic actor.


-type law_identifier() :: term().
% Designates a model-specific random law (usually an atom), so that can once a
% law is defined we can request random values out of it:


-type law_entry() :: { law_identifier(), class_RandomManager:random_law() }.
% Describes a law entry set in this stochastic actor.



% @doc Constructs a stochastic actor.
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - StochasticActorName the name of this stochastic actor
%
% - ListOfRandomLaws is a list of pairs, each pair being in the form of:
% {attribute_name_of_law, RandomType} where:
%
%  - attribute_name_of_law is a random law identifier (preferably as an atom),
%  it can be chosen freely (avoid collision, though!); ex:
%  'my_first_uniform_law'
%
%  - RandomType is a tuple which describes the corresponding random law; its
%  first element is the name of the random law (ex: uniform, exponential,
%  gaussian, etc.) and the following elements are the corresponding settings for
%  that law
%
% For example, ListOfRandomLaws may be [ { test_first_uniform, {uniform,5} } ].
%
% See also: get_random_value_from/2 for more details
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), [ law_entry() ] ) -> wooper:state().
construct( State, ActorSettings, StochasticActorName, ListOfRandomLaws ) ->

	% First the direct mother classes:
	ActorState = class_Actor:construct( State, ActorSettings,
								?trace_categorize(StochasticActorName) ),

	% Then the class-specific actions:

	% Now we have an initialized actor, hence properly seeded.

	[ check_law_definition( LawDef )
			|| { _LawId, LawDef } <- ListOfRandomLaws ],

	% random_laws is the list of random laws, each element being like
	% {attribute_name_of_law, RandomType}:
	%
	StartingState = setAttribute( ActorState, random_laws, ListOfRandomLaws ),

	% ?send_info_fmt( StartingState, "Creating a stochastic actor "
	%                 "with random laws ~p.", [ ListOfRandomLaws ] ),

	StartingState.



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	%?info( "Deleting stochastic actor." ),

	%?debug( "Stochastic actor deleted." ),

	% Then allow chaining:
	State.




% Methods section.


% Management section of the actor.


% actSpontaneous/1 not overridden here.



% Helper section.


% @doc Returns a new random value from the random law which specified by its
% identifier (as declared at creation).
%
% State is unchanged hence not returned.
%
% Returns the random value.
%
% (helper)
%
-spec get_random_value_from( law_identifier(), wooper:state() ) -> number().
get_random_value_from( LawIdentifier, State ) ->

	case get_law_settings_for( LawIdentifier, ?getAttr(random_laws) ) of

		{ uniform, N } ->
			class_RandomManager:get_uniform_value( N );

		{ exponential, Lambda } ->
			class_RandomManager:get_exponential_value( Lambda );

		{ positive_integer_exponential, Lambda } ->
			class_RandomManager:get_positive_integer_exponential_value(
																	   Lambda );

		{ gaussian, Mu, Sigma } ->
			class_RandomManager:get_gaussian_value( Mu, Sigma );

		{ positive_integer_gaussian, Mu, Sigma } ->
			class_RandomManager:get_positive_integer_gaussian_value( Mu,
																	Sigma );

		OtherLaw ->
			throw( { unexpected_random_law, OtherLaw, LawIdentifier } )

	end.



% @doc Declares the specified random law in the returned state, for a later
% possible reuse.
%
% Any law declared with the same identifier will be overridden by this one.
%
% (helper)
%
-spec add_law( law_identifier(), class_RandomManager:random_law(),
			   wooper:state() ) -> wooper:state().
add_law( LawIdentifier, LawDefinition, State ) ->

	check_law_definition( LawDefinition ),

	CurrentLaws = ?getAttr(random_laws),

	% Removes any previous entry to avoid lists that may grow too much:
	ExpurgedLaws = lists:keydelete( _Key=LawIdentifier, _Index=1, CurrentLaws ),

	NewLaws = [ { LawIdentifier, LawDefinition } | ExpurgedLaws ],

	setAttribute( State, random_laws, NewLaws ).



% @doc Removes specified random law from the known laws.
%
% (helper)
%
-spec remove_law( law_identifier(), wooper:state() ) -> wooper:state().
remove_law( LawIdentifier, State ) ->

	CurrentLaws = ?getAttr(random_laws),

	Key = LawIdentifier,

	Index = 1,

	case lists:keymember( Key, Index, CurrentLaws ) of

		true ->
			NewLaws = lists:keydelete( Key, Index, CurrentLaws ),
			setAttribute( State, random_laws, NewLaws );

		false ->
			throw( { unknown_random_law_to_remove, LawIdentifier,
					 CurrentLaws } )

	end.



% @doc Returns the settings of the random law specified by its identifier.
%
% (helper)
%
get_law_settings_for( LawIdentifier, [] ) ->
	throw( { unknown_random_law_identifier, LawIdentifier } );

get_law_settings_for( LawIdentifier,
		[ { LawIdentifier, Settings } | _OtherLaws ] ) ->
	Settings;

get_law_settings_for( LawIdentifier,
		[ { _OtherLawIdentifier, _Settings } | OtherLaws ] ) ->
	get_law_settings_for( LawIdentifier, OtherLaws ).



% @doc Checks that specified law definition is correct.
%
% Note: usually this checking can also be done statically by Dialyzer.
%
check_law_definition( { uniform, N } ) when is_integer( N ) andalso N > 0 ->
	ok;

check_law_definition( { exponential, Lambda } )
								when is_number( Lambda ) andalso Lambda > 0 ->
	ok;

check_law_definition( { positive_integer_exponential, Lambda } )
								when is_number( Lambda) andalso Lambda > 0 ->
	ok;

check_law_definition( { gaussian, Mu, Sigma } )
		when is_number( Mu ) andalso is_number( Sigma ) andalso Sigma >= 0 ->
	ok;

check_law_definition( { positive_integer_gaussian, Mu, Sigma } )
		when is_number( Mu ) andalso is_number( Sigma ) andalso Sigma >= 0 ->
	ok;

check_law_definition( UnexpectedLaw ) ->
	throw( { invalid_random_law, UnexpectedLaw } ).
