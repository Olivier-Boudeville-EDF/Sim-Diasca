% Copyright (C) 2008-2024 EDF R&D
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


% @doc Test of the Stochastic Actor class, regarding <b>random management</b>.
%
% Note: to be used with random_laws_and_stochastic_actor_test.erl.
%
-module(class_TestStochasticActor).

-define( class_description,
		 "Test of the Stochastic Actor class, regarding random management."
		 "To be used with random_laws_and_stochastic_actor_test.erl." ).


% The attributes that are specific to this class are:
-define( class_attributes, [

	{ behaviour_table, behaviour_table(),
	  "the probabilities of all behaviour modes" } ] ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_StochasticActor ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "StochasticActor.Test" ).


-type behaviour() :: atom().

-type behaviour_table() :: list_table:list_table( behaviour(), pos_integer() ).


% Silencing:

-export_type([ behaviour/0, behaviour_table/0 ]).



% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% Shorthands:

-type random_law_spec() :: random_utils:random_law_spec().



% @doc Constructs a test stochastic actor.
%
% TerminationProbability models the probability that this actor terminates, when
% deciding what to do next.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), [ random_law_spec() ],
				 math_utils:probability() ) -> wooper:state().
construct( State, ActorSettings, ActorName, RandomLawSpecs,
		   TerminationProbability ) ->

	% First the direct mother classes, then this class-specific actions:
	StochasticState = class_StochasticActor:construct( State, ActorSettings,
		?trace_categorize(ActorName), RandomLawSpecs ),

	% Probabilities of all possible behaviours:
	Behaviours = [ { terminate, round( 100 * TerminationProbability ) },
				   { be_passive, 10 },
				   { talk, 30 } ],

	?send_info_fmt( StochasticState, "Creating the '~ts' test stochastic "
		"actor, whose specifications of randow laws are:~n  ~p~n"
		"and whose behaviours are:~n ~p.",
		[ ActorName, RandomLawSpecs, Behaviours ] ),

	setAttribute( StochasticState, behaviour_table, Behaviours ).



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?notice( "Deleting test stochastic actor." ),

	% erlang:unlink() not used, as done manager-side.
	?debug( "Test stochastic actor deleted." ),

	% Then allow chaining:
	State.




% Methods section.


% Management section of the actor.



% @doc The core of the test stochastic actor behaviour.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	?notice( "Test stochastic actor acting." ),

	% Let's draw for fun some random values:
	FirstRandomValue = class_StochasticActor:get_random_value_from(
		my_first_uniform, State ),

	SecondRandomValue = class_StochasticActor:get_random_value_from(
		my_first_uniform, State ),

	ThirdRandomValue = class_StochasticActor:get_random_value_from(
		my_second_uniform, State ),

	FourthRandomValue = class_StochasticActor:get_random_value_from(
		my_second_uniform, State ),

	FifthRandomValue = class_StochasticActor:get_random_value_from(
		my_gaussian, State ),

	SixthRandomValue = class_StochasticActor:get_random_value_from(
		my_exponential, State ),

	?notice_fmt( "Test Actor drew ~p from first uniform list, ~p from second, "
		"~p from gaussian one and ~p from exponential one.",
		[ { FirstRandomValue,SecondRandomValue },
		  { ThirdRandomValue,FourthRandomValue },
		  FifthRandomValue, SixthRandomValue ] ),

	% Now let's decide what we do next:
	NextState = case list_utils:draw_element_weighted(
			?getAttr(behaviour_table) ) of

		terminate ->
			?notice( "Test Stochastic Actor preparing termination." ),
			executeOneway( State, declareTermination );

		be_passive ->
			?notice( "Test Stochastic Actor will be passive next." ),
			State;

		talk ->
			% As my_second_uniform is an integer law:
			NextTickOffset = ?getAttr(current_tick_offset)
				+ class_StochasticActor:get_random_value_from(
					my_second_uniform, State ) + 1,

			?notice_fmt( "Test Stochastic Actor talked and "
				"now requests to be spontaneously scheduled "
				"at tick offset #~B.", [ NextTickOffset ] ),

			executeOneway( State, addSpontaneousTick, NextTickOffset )

	end,

	wooper:return_state( NextState ).



% @doc Called at first diasca only.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% Just schedule once the first next tick:
	NewState = executeOneway( State, addSpontaneousTick,
							  ?getAttr(current_tick_offset) + 1 ),

	actor:return_state( NewState ).
