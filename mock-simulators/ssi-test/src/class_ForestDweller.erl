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


-module(class_ForestDweller).


-define( class_description,
		 "Class modeling a forest dweller. This module is an abstract one "
		 "derived from class_Actor.erl and defining some common forest dweller "
		 "attributes and spontaneous behaviours." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


-export([ prepareTermination/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "SSI-Test.ForestDweller" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


-include("ssi_test_types.hrl").



% Constructs a new forest dweller actor:
%
% - is_registered: shows if the actor is registed to a forest
%
% - givenAge: for an initially created actor,an given age can be other than 0
% for making difference between all initially created ones; it is 0 for an actor
% created during simulation
%
% - termination_tick_off_set: duration after which this actor should terminate
%
% - termination_waiting_ticks: the ticks between the actor informs its
% termination and execute its termination. This attribute is defined for
% avoiding termination_but_trigger error. The value of this attribute should be
% defined according to the specific actor behavior
%
% - termination_initiated: when it is true, means the forest instance is ready
% to be terminated. By default, its value is false
%
% - target_peers: records a list of pids to which the actor sends messages
%
% Reference to class_Actor.erl for other attributes
%
construct( State, ActorSettings, DwellerName, GivenAge, ForestPid ) ->

	% Firstly, the mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize( DwellerName ) ),

	% Then the class-specific attributes:
	setAttributes( ActorState, [
		{ is_registered, false },
		{ givenAge, GivenAge },
		{ forest_pid, ForestPid },
		{ termination_tick_offset, undefined },
		{ termination_waiting_ticks, undefined },
		{ termination_initiated, false },
		{ target_peers, [] } ] ).




% Method implementation section.



% Message received from the forest with the forest PID in parameter.
-spec beRegistered( wooper:state(), actor_pid() ) -> actor_oneway_return().
beRegistered( State, ForestPid ) ->

	?notice_fmt( "~w has been registed to ~w.", [ self(), ForestPid ] ),

	UpdatedState = setAttributes( State, [ { is_registered, true },
										   { forest_pid, ForestPid } ] ),

	actor:return_state( UpdatedState ).



% Called whenever an Alert message is received.
%
% This method will be overridden by the specific dweller.
%
-spec beAlert( wooper:state(), alert(), sending_actor_pid() ) -> const_actor_oneway_return().
beAlert( State, _Alert, _SendingActorPID ) ->
	actor:const_return().



% Allows to prepare the termination of a dweller.
%
% (helper)
%
-spec prepareTermination( wooper:state() ) -> wooper:state().
prepareTermination( State ) ->

	CurrentOffset = ?getAttr(current_tick_offset),

	setAttributes( State, [
		 { termination_initiated, true },
		 { termination_tick_offset, CurrentOffset } ] ).
