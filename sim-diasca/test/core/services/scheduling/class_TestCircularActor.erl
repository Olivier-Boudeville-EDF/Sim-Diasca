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


-module(class_TestCircularActor).


-define( class_description,
		 "Basic test of Actor class, regarding time management." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


% Exported helpers:
-export([ output/2, output/3 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.CircularTest" ).

% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Constructs a new test actor.
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), string() ) -> wooper:state().
construct( State, ActorSettings, ActorName, Message ) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize( ActorName ) ),

	?send_info( ActorState, "Creating a new test circular actor." ),

	%trace_utils:debug_fmt(
	%  "- creating a new class_TestCircularActor: PID: ~w, AAI: ~B, seed: ~w",
	%		   [ self(), getAttribute( ActorState, actor_abstract_id ),
	%			 getAttribute( ActorState, random_seed ) ] ),

	%ReverseInitialCreation = true,
	ReverseInitialCreation = false,

	case ReverseInitialCreation of

		true ->
			_TestActorPid = class_Actor:create_initial_actor( class_TestActor,
				[ _Rev="Reverse test actor",
				  _FirstSchedulingSettings={ erratic, 3 },
				  _FirstCreationSettings=no_creation,
				  _FirstTerminationTickOffset=107 ] );

		false ->
			ok

	end,

	setAttributes( ActorState, [
		{ message, Message },
		{ initialization_status, in_progress },

		%{ talkative, true },
		{ talkative, false } ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?debug( "Test circular actor deleted." ),

	% Then allow chaining:
	State.




% Management section of the actor.



% This actor oneway is automatically called the next diasca after an actor is
% created or, if the simulation was not running, on diasca 1 (i.e. just after
% the spontaneous behaviours) of tick offset #0.
%
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->
	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),
	actor:return_state( ScheduledState ).



% The core of the test actor behaviour.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	?notice( "Test Circular Actor acting." ),

	output( "Test Circular Actor acting.", State ),

	ActedState = case ?getAttr(initialization_status) of

		completed ->
			say_something( State );

		_OtherStatus ->
			State

	end,

	NextScheduleOffset = class_Actor:get_current_tick_offset( ActedState ) + 10,

	PlanState = executeOneway( ActedState, addSpontaneousTick,
							   NextScheduleOffset ),

	wooper:return_state( PlanState ).



% Adds specified peer to known peers.
-spec addPeer( wooper:state(), actor_pid() ) -> oneway_return().
addPeer( State, PeerPid ) ->

	?notice_fmt( "Chaining to ~w.", [ PeerPid ] ),

	wooper:return_state( setAttribute( State, peer, PeerPid ) ).



% Receives a hello message.
-spec receiveMessage( wooper:state(), class_Actor:name(), string(),
					  sending_actor_pid() ) -> actor_oneway_return().
receiveMessage( State, SenderName, Message, SenderPid ) ->

	?notice_fmt( "Received following message from ~s (~w): '~s', "
			   "using this message from now on.",
			   [ SenderName, SenderPid, Message ] ),

	actor:return_state( setAttribute( State, message, Message ) ).




% Section for helper functions (not methods).


% Says hello to all peers.
% Returns an updated state.
%
% (helper function)
%
say_something( State ) ->

	Peer = ?getAttr(peer),

	?notice_fmt( "Sending '~s' to ~w.", [ ?getAttr(message), Peer ] ),

	case Peer of

		Peer when is_pid( Peer ) ->
			class_Actor:send_actor_message( Peer,
			 { receiveMessage, [ ?getAttr(name), ?getAttr(message) ] }, State );

		undefined ->
			State

	end.



% Outputs specified message in console, iff talkative.
%
% (helper)
%
output( Message, State ) ->

	case ?getAttr(talkative) of

		true ->
			TickOffset = class_Actor:get_current_tick_offset( State ),
			trace_utils:debug_fmt( " [~s (~w) at ~p] " ++ Message,
					   [ ?getAttr(name), self(), TickOffset ] );

		false ->
			ok

	end.



% Outputs specified formatted message in console, iff talkative.
%
% (helper)
%
output( Format, Values, State ) ->
	Message = text_utils:format( Format, Values ),
	output( Message, State ).
