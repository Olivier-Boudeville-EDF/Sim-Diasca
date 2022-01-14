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

% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


% This file is part of forest ecosystem test case, which is a Sim-Diasca
% integration test example.


% @doc The objective of this module is to show the significant features of a
% Sim-Diasca actor in multiple scheduling modes.
%
% This means that it has a periodic schedule and that it can be also triggered
% by messages.
%
-module(class_FemaleRedSquirrel).


-define( class_description,
		 "Class modelling a female red squirrel: it defines the specific "
		 "female squirrel actor attributes and its spontaneous behaviour." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Squirrel ] ).


-include("ssi_test_types.hrl").



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "SSI-Test.class_FemaleRedSquirrel" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% @doc Constructs a female red squirrel actor:
%
% - number_of_offsprings: the total number of offsprings of this female
%
% - breeding_age: a female is capable to breed at this age
%
% - current_agenda: is a tick list at which the female will breed
%
% - offsprings_per_litter: the number of offsprings per litter
%
% - state can be available, weak (when a newborn is less than 10 weeks),
% gestation, nursing
%
% Reference to class_ForestDweller.erl for other attributes
%
construct( State, ActorSettings, SquirrelName, GivenAge, ForestPid ) ->

	% Firstly, the mother class:
	SquirrelState = class_Squirrel:construct( State, ActorSettings,
		?trace_categorize(SquirrelName), GivenAge, ForestPid ),

	StartingState = setAttributes( SquirrelState, [
		{ gender, female },
		{ number_of_offsprings, 0 },

		% A female can breed at 60 weeks of age and once per 60-week cycle:
		{ breeding_age, 6 },

		{ breeding_agenda, [] },
		{ offsprings_per_litter, 3 },
		{ gestation_period, 5 },
		{ nursing_period, 10 } ] ),

	?send_info( StartingState, "Creating a female squirrel." ),

	StartingState.



% Methods implementation session



% @doc Simply schedules this just created actor at the next tick (diasca 0).
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
										actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( ScheduledState ).



% @doc The spontaneous behaviour of a squirrel instance.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	% A squirrel is terminating when it comes to the end of its natural
	% lifespan:
	%
	TerminationOffset = ?getAttr(lifespan),
	WaitTicks = ?getAttr(termination_waiting_ticks),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case ?getAttr(current_tick_offset) of

		PastOffset when PastOffset >= TerminationOffset ->

			case ?getAttr(termination_initiated) of

				false ->

					?notice( "I am preparing a deferred termination." ),
					executeOneway( State, notifyTermination );

				true when WaitTicks > 0 ->

					NewWaitTick = ?getAttr(termination_waiting_ticks) - 1,

					TermState = setAttribute( State, termination_waiting_ticks,
											  NewWaitTick ),

					executeOneway( TermState, scheduleNextSpontaneousTick );


				_Other ->

					% Following two calls could also have been grouped into an
					% overloading of the default declareTermination/1
					% implementation:
					%
					?notice_fmt( "I am terminating at #~B.", [ PastOffset ] ),

					executeOneway( State, declareTermination )

			end;

		_CurrentOffset ->
			trigger_spontaneous_activities( State )

	end,

	wooper:return_state( NewState ).




% @doc To be called when an alert is received from another actor.
-spec beAlert( wooper:state(), alert(), sending_actor_pid() ) ->
						actor_oneway_return().
beAlert( State, Alert, _SendingActorPID ) ->

	NewState = case Alert of

		savage ->
			?notice_fmt( "A ~ts alert is received, I am preparing my deferred "
						 "termination.", [ Alert ] ),
			executeOneway( State, notifyTermination );

		famine ->
			case ?getAttr(state) of

				AState when (AState=:=weak) or (AState=:=gestation) ->

					?notice_fmt( "I am in ~ts state and a ~ts alert "
						"is received, I am preparing my deferred termination.",
						[ AState, Alert ] ),

					executeOneway( State, notifyTermination );

				_otherState ->

					?notice_fmt( "I received a ~ts alert, but I am surviving.",
								 [ Alert ] ),
					State
			end;

		reproduction ->
			case ?getAttr(state) of

				AState when (AState=:=available) ->

					class_Actor:send_actor_message( ?getAttr(forest_pid),
													beginCompetition, State ) ;

				_OtherState ->
					?notice_fmt( "I received a ~ts alert, but I am not "
								 "available.", [ Alert ] ),
					State

			end;

		_Others ->
			?notice_fmt( "I received a ~ts alert, but I am surviving.",
						 [ Alert ] ),
			State

	end,

	actor:return_state( NewState ).



% @doc Called by the forest for informing the winner of the competition; then
% the actor sends a "youAreWinner" message to the winner.
%
-spec theWinner( wooper:state(), pid(), pid() ) -> actor_oneway_return().
theWinner( State, WinnerPid, _SendingActorPid ) when is_pid( WinnerPid ) ->

	?notice_fmt( "The winner is ~w.", [ WinnerPid ] ),

	NState = class_Actor:send_actor_message( WinnerPid, youAreWinner, State ),

	?notice( "I am in gestation." ),

	NewAvailableTick = ?getAttr(current_tick_offset)
		+ ?getAttr(gestation_period),

	UpdatedState = setAttributes( NState, [
		{ state, gestation },
		{ available_tick, NewAvailableTick } ] ),

	FinalState = executeOneway( UpdatedState, addSpontaneousTick,
								NewAvailableTick ),

	actor:return_state( FinalState );


theWinner( State, _WinnerPid, _Sender )  ->
	?notice("I am so sad that no one won the competition." ),
	actor:const_return().



% @doc Returned the breeding tick list of this female squirrel.
get_breeding_tick_list( State ) ->
	BreedingNumber = ?getAttr(lifespan) div ?getAttr(breeding_age),
	get_list( State, _BreedingTickList=[], BreedingNumber ).


get_list( _State, TickList, 0 ) ->
	TickList;

get_list( State, TickList, BreedingNumber ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),
	BreedingPeriod = ?getAttr(breeding_age),

	BreedingTick = CurrentTickOffset + BreedingNumber*BreedingPeriod,
	NewTickList = TickList ++ [ BreedingTick ],
	get_list( State, NewTickList, BreedingNumber-1 ).



% (helper)
give_birth( State, 0 ) ->

	UpdatedNumberOfChildren = ?getAttr(number_of_offsprings) +
									?getAttr(offsprings_per_litter),

	NewAvailableTick = ?getAttr(current_tick_offset) + ?getAttr(nursing_period),

	UpdateState = setAttributes( State, [
		{ number_of_offsprings, UpdatedNumberOfChildren },
		{ state, nursing },
		{ available_tick, NewAvailableTick } ] ),

	executeOneway( UpdateState, addSpontaneousTick, NewAvailableTick );


give_birth( State, NbBirth ) ->

	NbthOfNewChild = ?getAttr(number_of_offsprings) + 1,
	%MotherName = lists:substract("class_", )
	ChildName = ?getAttr(name) ++ "." ++ integer_to_list( NbthOfNewChild ),

	ConstructParameters = [ ChildName, _defaultAge=0, ?getAttr(forest_pid) ],

	CreateNewActorState = case get_gender( State ) of

		male ->
			% The squirrel actor creates a male actor at runtime:
			class_Actor:create_actor( class_MaleRedSquirrel,
					ConstructParameters, State );

		female ->
			% The squirrel actor creates a female actor at runtime:
			class_Actor:create_actor( class_FemaleRedSquirrel,
					ConstructParameters, State )

	end,

	UpdatedState = setAttribute( CreateNewActorState, number_of_offsprings,
					 ?getAttr(number_of_offsprings) + 1 ),

	?notice_fmt( "~ts has a new child, its name is ~ts.",
				 [ ?getAttr(name), ChildName ] ),

	give_birth( UpdatedState, NbBirth-1 ).



% @doc Decides a gender for a newborn.
%
% For reproductivity reasons, this function assures that the first newborn is
% male, the second one is female and so on.
%
get_gender( State ) ->

	case ?getAttr(number_of_offsprings) rem 2 of

		0 ->
			male;

		1 ->
			female

	end.



% @doc This helper function groups all spontaneous activities of this actor.
%
% (helper)
%
trigger_spontaneous_activities( State ) ->

	case ?getAttr(is_registered) of

		false ->
			BreedingList = get_breeding_tick_list( State ),
			SetState = setAttribute( State, breeding_agenda, BreedingList ),

			% Apparently fakes an actor oneway (strange):
			executeOneway( SetState, tryToRegister, [ ?MODULE, self() ] );

		true ->
			CurrentOffset = ?getAttr(current_tick_offset),
			AvailableTick = ?getAttr(available_tick),

			case ?getAttr(state) of

				weak when CurrentOffset >= AvailableTick ->
					?notice( "I am proud of becoming adult." ),
					AvailState = setAttribute( State, state, available ),
					executeOneway( AvailState, scheduleNextSpontaneousTick );

				gestation when CurrentOffset >= AvailableTick ->
					?notice( "I am so happy, I will have a baby." ),
					give_birth( State, ?getAttr(offsprings_per_litter) );

				nursing when CurrentOffset > AvailableTick ->
					?notice( "I am so happy, all my babies are growing up." ),
					AvailState = setAttribute( State, state, available ),
					executeOneway( AvailState, scheduleNextSpontaneousTick );

				_OtherState ->
					State

			end

	end.
