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

% The objective of this module is to show the significant features of a
% Sim-Diasca actor in multiple scheduling modes.
%
% This means that it has a periodic schedule and that it can be also triggered
% by messages.


-module(class_Squirrel).


-define( class_description,
		 "Class modelling a squirrel actor. It defines the common squirrel "
		 "actor attributes and spontaneous bheaviour." ).

% NB: In SSI-Test, each simulation tick corresponds to a week.


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ForestDweller ] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "SSI-Test.Squirrel" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Constructs a new squirrel actor:
%
% - oak_pid: records the PID of the tree where this squirrel lives
%
% - lifespan: is the nature longevity of a squirrel. In this test case, it is
% defined as a static value: 208 weeks
%
% - state: can be
%
%  - beNursed: if it is a newborn
%  - available
%  - gestation: if the female squirrel is waiting for baby
%  - nursing: if the female is nursing its offsprings
%
construct( State, ActorSettings, SquirrelName, GivenAge, ForestPid ) ->

	% Firstly, the mother class
	DwellerState = class_ForestDweller:construct( State, ActorSettings,
					?trace_categorize( SquirrelName ), GivenAge, ForestPid ),

	% For an initial created squirrel, a default age is given to make difference
	% between the initially created squirrels.
	%
	% For the squirrel created during the simulation, the given age is 0 and age
	% will change over simulation time.
	%
	% And the lifespan of a squirrel is defined as 4 years, i.e. 208 weeks.
	%
	{ InitialSquirrelState, AvailableTick } = case GivenAge of

		0 ->
			{ weak, 10 };

		_Others ->
			{ available, 0 }

	end,

	setAttributes( DwellerState, [
		{ gender, undefined },
		{ oak_pid, undefined },
		{ lifespan, 208 },

		% All newborn squirrels must be nursed for 10 weeks:
		{ be_nursed_period, 10 },

		% At squirrel actor creation, the termination_tick_offset is initiated
		% as its lifespan; it can anyway be modified during the simulation:
		%
		{ termination_tick_offset, 208 },

		{ termination_waiting_ticks, 3 },
		{ state, InitialSquirrelState },
		{ available_tick, AvailableTick } ] ).


% Simply schedules this just created actor at the next tick (diasca 0).
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( ScheduledState ).



% Called by a relative oak for notifying this squirrel that it is removed from
% the oak.
%
% When receiving this message, the squirrel asks its forest for a relocation.
%
-spec beMoved( wooper:state(), sending_actor_pid() ) -> actor_oneway_return().
beMoved( State, SendingActorPid ) ->

	NewState = case ?getAttr(oak_pid) of

		undefined ->
			?notice_fmt( "I receive a beMoved from ~w, but it is not my oak.",
					   [ SendingActorPid ] ),
			State;

		% Note the matching to this already-bound variable:
		SendingActorPid ->

			?notice( "I am moved from my oak, I have no home and I am requiring "
				   "to be relocated." ),

			UpdatedTargetPeers = lists:delete( SendingActorPid,
											   ?getAttr(target_peers) ),

			NState = setAttribute( State, target_peers, UpdatedTargetPeers ),

			class_Actor:send_actor_message( ?getAttr(forest_pid),
											requiredLocation, NState );

		_OtherPid ->
			?notice_fmt( "I receive a beMoved from ~w, but it is not my oak.",
					   [ SendingActorPid ] ),
			State

	end,

	actor:return_state( NewState ).



% Called by the forest for informing this squirrel of a new oak.
-spec beAllocated( wooper:state(), actor_pid(), sending_actor_pid() ) ->
						 actor_oneway_return().
beAllocated( State, OakPid, _SendingActorPid ) ->

	TargetPeers = ?getAttr(target_peers),

	UpdatedState = case OakPid of

		undefined ->

				?notice( "I am moved from my tree and I am homeless." ),

				NState = setAttributes( State, [
										 { oak_pid, undefined },
										 { target_peers, TargetPeers } ] ),

				executeOneway( NState, notifyTermination );

		_ ->
				?notice_fmt( "I am relocated to ~w.", [ OakPid ] ),
				setAttributes( State, [
					{ oak_pid, OakPid },
					{ target_peers,[ OakPid | TargetPeers ] } ] )

	end,

	actor:return_state( UpdatedState ).



% Deletes a specified squirrel PID from the target peers.
-spec deleteFromPeers( wooper:state(), sending_actor_pid() ) ->
							 actor_oneway_return().
deleteFromPeers( State, SendingActorPid ) ->

	?notice_fmt( "~w is deleted from the target peers of ~w.",
			   [ SendingActorPid, self() ] ),

	TargetPeers = ?getAttr(target_peers),
	UpdatedList = lists:delete( SendingActorPid, TargetPeers ),

	actor:return_state( setAttribute( State, target_peers, UpdatedList ) ).



% Received from the forest.
-spec forestDestroyed( wooper:state(), sending_actor_pid() ) ->
							 actor_oneway_return().
forestDestroyed( State, SendingActorPid )->

	?notice_fmt( "~w ~w will terminate because of destroyed forest.",
			   [ self(), ?getAttr(name) ] ),

	TargetPeers = ?getAttr(target_peers),
	UpdatedList = lists:delete( SendingActorPid, TargetPeers ),
	NewState = setAttributes( State, [ { forest_pid, undefined },
									   { target_peers, UpdatedList } ] ),

	NotifState = executeOneway( NewState, notifyTermination ),

	actor:return_state( NotifState ).



% The squirrel actor informs its target peers about its termination.
-spec notifyTermination( wooper:state() ) -> actor_oneway_return().
notifyTermination( State ) ->

	% Source and target peers must be notified here, otherwise, next time they
	% will send a message to this actor, they will hang forever:
	%
	% (this returns a new state)

	CurrentOffset = ?getAttr(current_tick_offset),

	?notice_fmt( "I inform my relative actors for my termination at tick #~B.",
				 [ CurrentOffset ] ),

	NewState = case ?getAttr(target_peers) of

		[] ->
			State;

		TargetPeers ->

			SendFun = fun( TargetPid, FunState ) ->

				%Returns an updated state:
				class_Actor:send_actor_message( TargetPid, deleteFromPeers,
												FunState )
			end,

			% Returns an updated state:
			lists:foldl( SendFun, State, TargetPeers )

		end,

	FinalState = executeOneway( NewState, prepareTermination ),

	actor:return_state( FinalState ).





% Called when is_registered is false.
%
% The actor sends a addInPeers message when the forest PID exists and then an
% updated state is returned; otherwise, the original state is returned.
%
-spec tryToRegister( wooper:state(), classname(), sending_actor_pid() ) ->
						   actor_oneway_return().
tryToRegister( State, Classname, _SendingActorPid ) ->

	UpdatedState = case ?getAttr(forest_pid) of

		undefined ->
			State;

		ForestPid  ->

			NewState = class_Actor:send_actor_message( ForestPid,
								{ addInPeers, Classname }, State ),

			TargetPeers = ?getAttr(target_peers),

			UpdatedTargetPeers = [ ForestPid | TargetPeers ],

			PeerState = setAttributes( NewState, [
						   { is_registered, true },
						   { target_peers, UpdatedTargetPeers } ] ),

			executeOneway( PeerState, scheduleNextSpontaneousTick )

	end,

	actor:return_state( UpdatedState ).
