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

% Author: Jingxuan Ma (jingxuan.ma@edf.fr)

% This file is part of forest ecosystem integration test case.


% The objective of this module is to show the main features of an actor in
% multiple scheduling modes. These instances have indeed a periodic schedule and
% can be also triggered by messages.


-module(class_MaleRedSquirrel).


-define( class_description, "Class modelling a male red squirrel." ).


% TO-DO (FIXME):
% - what is "savage" expected to mean?
% - weak should be replaced by young?


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Squirrel ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "SSI-Test.class_MaleRedSquirrel" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


-include("ssi_test_types.hrl").



% Constructs a new male red squirrel actor:
%
% - ActorSettings corresponds to the engine settings for this actor
%
% - SquirrelName is the name of the squirrel, as a plain string
%
% - GivenAge is the initial age of this squirrel
%
% - ForestPid is the PID of the forest this squirrel is in
%
-spec construct( wooper:state(), class_Actor:actor_settings(), string(), age(),
				 actor_pid() ) -> wooper:state().
construct( State, ActorSettings, SquirrelName, GivenAge, ForestPid ) ->

	% First, the mother class:
	SquirrelState = class_Squirrel:construct( State, ActorSettings,
					  ?trace_categorize( SquirrelName ), GivenAge, ForestPid ),

	% Attribute descriptions:
	% - frame_of_mind: can be 'available' or 'weak'

	StartingState = setAttributes( SquirrelState, [
		{ frame_of_mind, undefined },
		{ tail_length, get_initial_tail_length() } ] ),

	?send_info( StartingState, "Creating a new male red squirrel." ),

	StartingState.




% Methods section.



% Simply schedules this just created actor at the next tick (diasca 0).
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( ScheduledState ).



% The spontaneous behaviour of a male red squirrel instance.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	TerminationOffset = ?getAttr(termination_tick_offset),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case ?getAttr(current_tick_offset) of

		PastOffset when PastOffset >= TerminationOffset ->
			termination_relative_activities( State );

		_CurrentOffset ->
			spontaneous_activities( State )

	end,

	wooper:return_state( NewState ).




% Called when this squirrel succeeded in a contest.
%
% When a youWon message is received, the squirrel switches for an arrogant
% frame_of_mind for a defined period.
%
-spec youWon( wooper:state(), sending_actor_pid() ) -> actor_oneway_return().
youWon( State, _SendingActorPid ) ->

	?notice( "I am the winner, I am the most beautiful squirrel." ),

	FrameState = setAttribute( State, frame_of_mind, arrogant ),

	NextAvailableTick = ?getAttr(current_tick_offset)
		+ ?getAttr(arrogant_period),

	FinalState = executeOneway( FrameState, addSpontaneousTick,
								NextAvailableTick ),

	actor:return_state( FinalState ).



% Called when this squirrel failed in a contest.
-spec youLose( wooper:state(), sending_actor_pid() ) -> actor_oneway_return().
youLose( State, _SendingActorPid ) ->

	?notice( "I am so sad, I am not the winner." ),

	NextState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( setAttribute( NextState, frame_of_mind, available ) ).




% Called when an alert message is received from the forest.
-spec beAlert( wooper:state(), alert(), sending_actor_pid() ) ->
					 actor_oneway_return().
beAlert( State, Alert, _SendingActorPid ) ->

	NewState = case Alert of

		savage ->
			?notice( "A savage alert is received, I am preparing my deferred "
				   "termination." ),
			executeOneway( State, notifyTermination );

		famine ->
			case ?getAttr(frame_of_mind) of

				Mindset when (Mindset=:=weak) orelse (Mindset=:=gestation) ->
					?notice_fmt( "I am in ~p frame of mind and "
							   "a famine alert is received, I am preparing "
							   "my deferred termination.", [ Mindset ] ),
					executeOneway( State, notifyTermination );

				_OtherMindset ->
					?notice_fmt( "I received a ~p alert, but I am surviving.",
							   [ Alert ] ),
					State

			end;

		_OtherAlert ->
			?notice_fmt( "I received a ~p alert, but I am surviving.", [ Alert ] ),
			State

	end,

	actor:return_state( NewState ).



% A competition invitation is received, the squirrel replies with its tail
% length.
%
-spec beInvited( wooper:state(), actor_pid(), sending_actor_pid() ) ->
					   actor_oneway_return().
beInvited( State, LauncherPid, SendingActorPid ) ->

	?notice_fmt( "I, male red squirrel ~w, receive a invitation.", [ self() ] ),

	TailLength = case ?getAttr(frame_of_mind) of

		weak ->
			 ?notice( "I cannot participate to the competition, I am to young." ),
			 refused;

		_Others ->
			?notice( "I received a competition invitation, I believe that I will win." ),
			?getAttr(tail_length)

	end,

	SentState = class_Actor:send_actor_message( SendingActorPid,
				 { informedParticipation, [ LauncherPid, TailLength ] },
				 State ),

  actor:return_state( SentState ).





% Helper functions.



% Once a squirrel eats, its tail gains 0.1cm.
eat_nuts( State ) ->

	NewLength = math_utils:round_after( ?getAttr(tail_length) + 0.1,
										_Digits=1 ),

	?notice( "How delicious the nuts are, my tail is more beautiful." ),

	setAttribute( State, tail_length, NewLength ).



% Returns a random tail length, between 3 - 6 for a newborn.
%
% This function is called only one time when a new male is created.
%
get_initial_tail_length()->
	3 + random_utils:get_random_value( 3 ).



% This helper function groups all termination related activities.
%
% Returns an updated state.
%
termination_relative_activities( State ) ->

	WaitTicks = ?getAttr(termination_waiting_ticks),
	CurrentOffset = ?getAttr(current_tick_offset),

	case ?getAttr(termination_initiated) of

		false ->
			?notice( "I am preparing deferred termination." ),
			executeOneway( State, notifyTermination );

		true when WaitTicks > 0 ->

			NextState = executeOneway( State, scheduleNextSpontaneousTick ),

			NewWaitTick = ?getAttr(termination_waiting_ticks) - 1,

			setAttribute( NextState, termination_waiting_ticks, NewWaitTick );

		_Other ->
			?notice_fmt( "I am terminating at tick #~B.", [ CurrentOffset ] ),
			executeOneway( State, declareTermination )

	end.




% This helper function groups all spontaneous activities of this actor.
%
% Returns an updated state.
%
spontaneous_activities( State ) ->

	CurrentOffset = ?getAttr(current_tick_offset),
	AvailableTick = ?getAttr(available_tick),

	case ?getAttr(is_registered) of

		false ->
			% Apparently fakes an actor oneway (strange):
			executeOneway( State, tryToRegister, [ ?MODULE, self() ] );

		true ->
			case ?getAttr(frame_of_mind) of

				weak when CurrentOffset >= AvailableTick ->

					?notice( "I am an adult squirrel now!" ),

					FrameState = setAttribute( State, frame_of_mind,
											   available ),

					executeOneway( FrameState, scheduleNextSpontaneousTick );

				arrogant when CurrentOffset < AvailableTick ->
						?notice( "I am the most beautiful squirrel." ),
						State;

				_Others ->
						eat_nuts( State )
			end

	end.
