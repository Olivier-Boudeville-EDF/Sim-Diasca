% Copyright (C) 2008-2023 EDF R&D
%
% This file is part of Sim-Diasca.
%
% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: 2008.


% @doc This child class offers basic services in order to easily <b>obtain
% values from random laws</b>.
%
% Basic actors may also directly generate their random values (see
% class_RandomManager.erl for that).
%
% Important note: this way of managing random values through inheritance is
% mostly deprecated, since a full stochastic support is now available to all
% instances of class_Actor. This class is now mostly useful for its static
% methods.
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

	% 'sd_' prefix, as considered as a Sim-Diasca builtin:
	{ sd_random_laws, list_table( law_identifier(), random_law_data() ),
	  "a table of all random laws available to this stochastic actor" } ]).



% Helper functions:
-export([ get_random_value_from/2, add_law/3, remove_law/2 ]).




% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.StochasticActor" ).



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
% Designates, as an identifier, a model-specific random law (usually an atom),
% so that can once a law has been defined the model can readily obtain random
% samples from it.
%
% Law identifiers are themselves model-specific.


-type law_description() :: { law_identifier(), random_law_spec() }.
% Describes a random law to be used by this stochastic actor.


-type law_entry() :: { law_identifier(), random_law_data() }.
% Describes a law entry set in this stochastic actor.
%
% Entries are stored in a table, registered in an conventional attribute of this
% actor.


-export_type([ law_entry/0 ]).



% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").


% Shorthands:

%-type list_table( K, V ) :: list_table:list_table( K, V).

-type random_law_spec() :: random_utils:random_law_spec().
-type random_law_data() :: random_utils:random_law_data().
-type sample() :: random_utils:sample().



% @doc Constructs a stochastic actor.
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - StochasticActorName is the name of this stochastic actor
%
% - RandomLawDescs is a list of pairs, each pair being in the form of
% {AttributeNameOfLaw, RandomSpec}, where:
%
%  - AttributeNameOfLaw is a random law identifier (preferably as an atom),
%  it can be chosen freely (avoid collision, though!); e.g.
%  'my_first_uniform_law'
%
%  - RandomSpec is a tuple that specifies the corresponding random law; its
%  first element is the name of the random law (e.g. uniform, exponential,
%  gaussian, etc.) and the following elements are the corresponding settings for
%  that law
%
% For example, RandomLawDescs may be [{test_first_uniform, {uniform,5,15}}].
%
% See also: get_random_value_from/2 for more details.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), [ law_description() ] ) -> wooper:state().
construct( State, ActorSettings, StochasticActorName, RandomLawDescs ) ->

	% First the direct mother classes:
	ActorState = class_Actor:construct( State, ActorSettings,
		?trace_categorize(StochasticActorName) ),

	% Then the class-specific actions:

	% Now we have an initialised actor, hence properly seeded.

	RandomLawEntries = list_table:new(
		[ { LawId, random_utils:initialise_law( LawSpec ) }
			|| { LawId, LawSpec } <- RandomLawDescs ] ),

	% random_laws is a list of initialised random law data, each element being
	% like {attribute_name_of_law, RandomData}:
	%
	StartingState =
		setAttribute( ActorState, sd_random_laws, RandomLawEntries ),

	% ?send_info_fmt( StartingState, "Creating a stochastic actor "
	%                 "with random laws ~p.", [ RandomLawEntries ] ),

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


% @doc Returns a new random value from the random law that is specified by its
% identifier (as declared at creation).
%
% State is unchanged, hence not returned.
%
% Returns the random value.
%
% (helper)
%
-spec get_random_value_from( law_identifier(), wooper:state() ) -> sample().
get_random_value_from( LawIdentifier, State ) ->

	LawData = list_table:get_value( LawIdentifier, ?getAttr(sd_random_laws) ),

	random_utils:get_sample_from( LawData ).



% @doc Declares the specified random law in the returned state, for a later
% possible reuse.
%
% Any law declared with the same identifier will be overridden by this one.
%
% (helper)
%
-spec add_law( law_identifier(), random_law_spec(), wooper:state() ) ->
											wooper:state().
add_law( LawIdentifier, LawSpec, State ) ->

	LawData = random_utils:initialise_law( LawSpec ),

	NewLaws = list_table:add_entry( _K=LawIdentifier, _V=LawData,
									?getAttr(sd_random_laws) ),

	setAttribute( State, sd_random_laws, NewLaws ).



% @doc Removes the specified random law from the known laws.
%
% (helper)
%
-spec remove_law( law_identifier(), wooper:state() ) -> wooper:state().
remove_law( LawIdentifier, State ) ->

	CurrentLaws = ?getAttr(sd_random_laws),

	Key = LawIdentifier,

	Index = 1,

	case lists:keymember( Key, Index, CurrentLaws ) of

		true ->
			NewLaws = lists:keydelete( Key, Index, CurrentLaws ),
			setAttribute( State, sd_random_laws, NewLaws );

		false ->
			throw( { unknown_random_law_to_remove, LawIdentifier,
					 CurrentLaws } )

	end.
