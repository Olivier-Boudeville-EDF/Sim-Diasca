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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


-module(class_TestStochasticActor).

-define( class_description,
		 "Test of the Stochastic Actor class, regarding random management."
		 "Note: to be used with "
		 "randomManagerAndStochasticActorPair_test.erl." ).


% The attributes that are specific to this class are:
-define( class_attributes, [

	{ behaviour_table, list_table:list_table(),
	  "Probabilities of all behaviour modes" } ] ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_StochasticActor ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "StochasticActor.Test" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Constructs a new test stochastic actor.
%
% TerminationProbability is a probability (here in [0,0.6], which gives the
% probability that this actor terminates, when deciding what to do next.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), [ class_RandomManager:random_law() ],
				math_utils:probability() ) -> wooper:state().
construct( State, ActorSettings, ActorName, ListOfRandomLaws,
		   TerminationProbability ) ->

	% First the direct mother classes, then this class-specific actions:
	StochasticState = class_StochasticActor:construct( State, ActorSettings,
							?trace_categorize( ActorName ), ListOfRandomLaws ),

	?send_info( StochasticState, "Creating a new test stochastic actor." ),

	% Probabilities of all possible behaviours:
	Behaviours = [ { terminate, round( 100 * TerminationProbability ) },
				   { be_passive, 10 },
				   { talk, 30 } ],

	% Prepare different lists of random values:
	setAttribute( StochasticState, behaviour_table, Behaviours ).



% Overridden destructor.
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



% The core of the test stochastic actor behaviour.
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
			NextTickOffset = ?getAttr(current_tick_offset)
				+ class_StochasticActor:get_random_value_from( my_first_uniform,
															   State ) + 1,

			?notice_fmt( "Test Stochastic Actor talked and "
				"now requests to be spontaneously scheduled "
				"at tick offset #~B.", [ NextTickOffset ] ),

			executeOneway( State, addSpontaneousTick, NextTickOffset )

	end,

	wooper:return_state( NextState ).



% Called at first diasca only.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% Just schedule the first next tick:
	NewState = executeOneway( State, addSpontaneousTick,
							  ?getAttr(current_tick_offset) + 1 ),

	actor:return_state( NewState ).
