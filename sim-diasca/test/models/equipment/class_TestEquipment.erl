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


-module(class_TestEquipment).


-define( class_description, "Test Equipment class. Used by the integration "
		 "test defined in equipment_integration_test.erl" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Equipment ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.Equipment.TestEquipment" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Constructs a new test equipment.
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_TimeManager:tick_offset(),
				 class_FailureModel:model_pid(),
				 class_RepairModel:model_pid() ) -> wooper:state().
construct( State, ActorSettings, EquipmentName, TerminationTickOffset,
		   FailureModelPid, RepairModelPid ) ->

	% First the direct mother classes:
	EquipmentState = class_Equipment:construct( State, ActorSettings,
		?trace_categorize(EquipmentName), FailureModelPid, RepairModelPid ),

	% Then the class-specific actions:
	% Failure state can be 'nominal' or 'dysfunction'.
	%
	% Equipments are supposed tested before being installed, thus start in
	% nominal condition:
	%
	% (cannot set next_*_tick, as no knowledge of current scheduling here)
	%
	StartingState = setAttribute( EquipmentState, termination_tick_offset,
								  TerminationTickOffset ),

	?send_info( StartingState, "Creating a new test equipment." ),

	StartingState.





% Methods section.


% Behaviour when being in nominal state.
%
% Note: tick termination will be handled by act_common/1.
%
-spec actNominal( wooper:state() ) -> oneway_return().
actNominal( State ) ->

	?notice( "Acting normally (test-overridden actNominal/1 called)." ),

	wooper:return_state( act_common( State ) ).



% Behaviour when being in dysfunction state.
%
% Note: tick termination will be handled by act_common/1.
%
-spec actInDysfunction( wooper:state() ) -> oneway_return().
actInDysfunction( State ) ->

	?notice( "Test-overridden actInDysfunction/1 called." ),

	wooper:return_state( act_common( State ) ).




% Section for helper functions (not methods).


% Manage the termination of this test equipment.
%
% Common to nominal and dysfunction.
%
% Returns an updated state.
%
-spec act_common( wooper:state() ) -> wooper:state().
act_common( State ) ->

	TerminationOffset = ?getAttr(termination_tick_offset),

	% We must prevent this equipment to further interact with other actors when
	% its end is near:
	%
	FreezeOffset = ?getAttr(termination_tick_offset) - 2,

	% Terminates if the termination offset is reached:
	case ?getAttr(current_tick_offset) of

		Offset when Offset >= FreezeOffset  ->

			% No more interactions wanted:
			TermState = executeOneway( State, scheduleNextSpontaneousTick ),

			setAttributes( TermState, [
				{ next_failure_tick, termination_triggered },
				{ next_repair_tick, termination_triggered },
				{ current_failure_state, terminating } ] );

		Offset when Offset >= TerminationOffset ->

			?notice( "Test Equipment preparing termination." ),

			% Returns an updated state:
			executeOneway( State, declareTermination );

		_ ->
			State

	end.



% Simply schedules this just created actor at the next tick (diasca 0).
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( ScheduledState ).



-spec onFailure( wooper:state() ) -> const_oneway_return().
onFailure( State ) ->

	?notice( "Failure occurred! "
			 "(test-overridden onFailure oneway method called)." ),

	wooper:const_return().



-spec onReparation( wooper:state() ) -> const_oneway_return().
onReparation( State ) ->

	?notice( "Reparation occurred! "
			 "(test-overridden onReparation oneway method called)." ),

	wooper:const_return().
