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


% This file is part of forest ecosystem test case, which is a Sim-Diasca
% integration test example.

% The purpose of this class is to show the significant features of a Sim-Diasca
% actor scheduled totally in passive mode.
%
% The passive mode scheduling means that activities are only triggered by
% received messages (no specific spontaneous behaviour).


-module(class_Oak).


-define( class_description, "Class modelling an oak" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ForestDweller ] ).


-include("ssi_test_types.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "SSI-Test.Oak" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Constructs a new Oak actor:
%
% - local_squirrel: is a list of squirrel PIDs living in this oak
%
% - max_inhabitant: the maximal inhabitants that can live in this oak; when the
% number of local squirrels is higher, the Oak actor is terminated because of
% overload
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), age(), pid() ) -> wooper:state().
construct( State, ActorSettings, OakName, GivenAge,
		 ForestPid ) ->

	% Firstly, the mother class:
	DwellerState = class_ForestDweller:construct( State, ActorSettings,
					  ?trace_categorize( OakName ), GivenAge, ForestPid ),

	% Then the class-specific attributes:
	UpdatedState = setAttributes( DwellerState, [
		{ local_squirrel, [] },
		{ max_inhabitant, 5 },
		{ termination_waiting_ticks, 3 } ] ),

	?send_info( UpdatedState, "Creating a new Oak." ),

	UpdatedState.




% Methods implementation section.



% Simply schedules this just created actor at the next tick (diasca 0).
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) -> 
						   actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( ScheduledState ).



% The spontaneous behaviour of this Oak actor, which is mostly passive.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	TerminationOffset = ?getAttr(termination_tick_offset),
	WaitTicks = ?getAttr(termination_waiting_ticks),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case ?getAttr(current_tick_offset) of

		PastOffset when PastOffset >= TerminationOffset ->

			case ?getAttr(termination_initiated) of

				false ->

					?notice( "I am preparing a deferred termination." ),

					% Source and target peers must be notified here, otherwise,
					% next time they will send a message to this actor, they
					% will hang forever:
					%
					% (this returns a new state)
					%
					notify_termination( State );

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
					?notice_fmt( "I am terminating at ~B.", [ PastOffset ] ),

					UpdatedState = reset_termination_waiting_ticks( State ),

					executeOneway( UpdatedState, declareTermination )

			end;

		_CurrentOffset ->

			case ?getAttr(is_registered) of

				false ->
					% When the actor is not registered to a forest, it will try
					% to register:
					%
					try_to_register( State );

				true ->
					State

			end

	end,

	wooper:return_state( NewState ).




% Following section deals with the messages received from other actors.


% An Alert is received from other actor.
%
% When it is a fire alert, the oak actor will inform its forest of its
% termination.
%
-spec beAlert( wooper:state(), alert(), sending_actor_pid() ) ->
					 actor_oneway_return().
beAlert( State, Alert, _SendingActorPID ) ->

	NewStated = case Alert of

		fire ->
			?notice( "I am terminating of fire." ),
			notify_termination( State );

		_Others ->
			?notice_fmt( "I received a ~p alert, but I am surviving.",
					   [ Alert ] ),
			State

	end,

	actor:return_state( NewStated ).



% Called by forest with the new affected squirrel PID.
-spec beAffected( wooper:state(), sending_actor_pid() ) ->
						actor_oneway_return().
beAffected( State, AffectedSquirrelPid ) ->

	SquirrelList = ?getAttr(local_squirrel),
	UpdatedList = [ AffectedSquirrelPid | SquirrelList ],
	MaxInhabitant = ?getAttr(max_inhabitant),

	UpdatedState = case length( UpdatedList ) of

		Length when Length > MaxInhabitant ->

			?notice( "I will terminate, due to the overload from dwellers." ),

			CurrentOffset = ?getAttr(current_tick_offset),

			TermState = setAttributes( State, [
						{ local_squirrel, UpdatedList },
						{ termination_tick_offset, CurrentOffset } ] ),

			NextState = executeOneway( TermState, scheduleNextSpontaneousTick ),

			notify_termination( NextState );

		_OtherLength ->
			setAttribute( State, local_squirrel, UpdatedList )

	end,

	actor:return_state( UpdatedState ).



% Removes specified squirrel from the known ones.
-spec deleteFromPeers( wooper:state(), actor_pid(), sending_actor_pid() ) ->
							 actor_oneway_return().
deleteFromPeers( State, SquirrelPid, _SendingActorPid ) ->

	?notice_fmt( "Squirrel ~w is deleted from Oak ~w.",
			   [ SquirrelPid, self() ] ),

	SquirrelList = ?getAttr(local_squirrel),

	UpdatedSquirrelList = lists:delete( SquirrelPid, SquirrelList ),

	actor:return_state(
	  setAttribute( State, local_squirrel, UpdatedSquirrelList ) ).



% Registers specified squirrel.
-spec addInPeers( wooper:state(), actor_pid(), sending_actor_pid() ) ->
						actor_oneway_return().
addInPeers( State, SquirrelPid, _SendingActorPid ) ->

	?notice_fmt( "Squirrel ~w is added in Oak ~w.", [ SquirrelPid, self() ] ),

	actor:return_state(
	  appendToAttribute( State, local_squirrel, SquirrelPid ) ).



% Notification that the forest is destroyed.
-spec forestDestroyed( wooper:state(), sending_actor_pid() ) ->
							 actor_oneway_return().
forestDestroyed( State, SendingActorPid )->

	?notice_fmt( "~w ~w will terminate, as its forest is destroyed.",
			   [ self(), ?getAttr(name) ] ),

	TargetPeers = ?getAttr(target_peers),

	UpdatedList = lists:delete( SendingActorPid, TargetPeers ),

	NewState = setAttributes( State, [ { forest_pid, undefined },
									   { target_peers, UpdatedList } ] ),

	actor:return_state( notify_termination( NewState ) ).




% Helper functions


% Registers to its forest, if any.
%
% Returns an updated state.
%
% (helper)
%
try_to_register( State ) ->

	case ?getAttr(forest_pid) of

		undefined ->
			State;

		ForestPid ->

			NewState = class_Actor:send_actor_message( ForestPid,
						{ addInPeers, ?MODULE }, State ),

			TargetPeers = ?getAttr(target_peers),

			setAttributes( NewState, [
						 { is_registered, true },
						 { target_peers, [ ForestPid | TargetPeers ] } ] )

	end.



% Sending message to its forest for notifying its termination
%
% (helper)
%
notify_termination( State ) ->

	CurrentOffset = ?getAttr(current_tick_offset),

	?notice_fmt( "I inform my relative actors of my termination at tick #~B.",
				 [ CurrentOffset ] ),

	NewState = case ?getAttr(forest_pid) of

		undefined ->
			State;

		ForestPid ->
			% Informs the forest of its termination:
			class_Actor:send_actor_message( ForestPid, deleteFromPeers, State )

	end,

	LocalSquirrels = ?getAttr(local_squirrel),

	UpdatedState = case LocalSquirrels of

		[] ->
			NewState;

		_Others ->

			% Informs its dwellers of its termination:
			SendFun = fun( InhabitantPid, FunState ) ->
				% Returns an updated state:
				class_Actor:send_actor_message( InhabitantPid,
												beMoved, FunState )
			end,

			% Returns an updated state:
			lists:foldl( SendFun, NewState, LocalSquirrels )

	end,

	NextState = executeOneway( UpdatedState, scheduleNextSpontaneousTick ),

	executeOneway( NextState, prepareTermination ).



% Resets the waiting.
-spec reset_termination_waiting_ticks( wooper:state() ) -> oneway_return().
reset_termination_waiting_ticks( State ) ->
	wooper:return_state( setAttribute( State, termination_waiting_ticks, 3 ) ).
