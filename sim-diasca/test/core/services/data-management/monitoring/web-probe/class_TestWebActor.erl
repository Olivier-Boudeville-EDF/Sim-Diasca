% Copyright (C) 2019-2021 EDF R&D

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
% Creation date: Wednesday, June 19, 2019.


-module(class_TestWebActor).


-define( class_description, "Actor test class regarding web probes" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).



% The class-specific attributes of this test actor are:
-define( class_attributes, [

	{ web_probe_ref, class_WebProbe:probe_ref(),
	  "the tested web probe (if any)" } ] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.Test" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


-type tick_offset() :: class_TimeManager:tick_offset().



% Constructs a new test actor:
%
% - ActorSettings corresponds to the various information (ex: AAI, seeding,
% ordering mode, etc.) that the load-balancer sets for each newly created actor
%
% - ActorName the name of the actor
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name() ) -> wooper:state().
construct( State, ActorSettings, ActorName ) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize( ActorName ) ),

	ProbeName = text_utils:format( "Test probe for ~s", [ ActorName ] ),

	ProbeRef = class_TestWebProbe:declare_result_probe( ProbeName ),

	setAttributes( ActorState, [
			{ web_probe_ref, ProbeRef },
			{ termination_tick_offset, 150 } ] ).




% Methods section.


% Management section of the actor.


% The core of the test actor behaviour.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),
	TerminationOffset = ?getAttr(termination_tick_offset),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case CurrentTickOffset of

		PastOffset when PastOffset >= TerminationOffset ->
			executeOneway( State, declareTermination );

		CurrentOffset ->
			% Non-termination behaviour:
			behave_normally( CurrentOffset, State )

	end,

	wooper:return_state( NewState ).



% Overridden, in order to synchronise correctly the internal planning that this
% test actor maintains, and to start its behaviour.
%
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	UpdatedState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( UpdatedState ).


% Manages the normal, non-termination behaviour of this actor.
%
% Returns an updated state.
%
% (helper)
%
-spec behave_normally( tick_offset(), wooper:state() ) -> void().
behave_normally( CurrentOffset, State ) ->

	?debug( "Acting spontaneously." ),

	case ?getAttr(web_probe_ref) of

		non_wanted_probe ->
			ok;

		ProbeRef ->
			ProbeRef ! { update, CurrentOffset, self() },
			receive

				{ wooper_result, probe_updated } ->
					ok

			end

	end,

	executeOneway( State, addSpontaneousTick, CurrentOffset + 5 ).
