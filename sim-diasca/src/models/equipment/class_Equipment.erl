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


-module(class_Equipment).


-define( class_description,
		 "Equipment class, models any equipment, mostly electronic ones. "
		 "Equipments are affected by failures and can be repaired, thus make "
		 "use of a failure model and a repair model. "
		 "An equipment can define any specific behavior to happen in case of "
		 "failure or reparation, either on state transitions "
		 "(onFailure/onReparation) or on persistent state "
		 "(actNominal/actInDysfunction). "
		 "See class_TestEquipment.erl for an example of equipment and "
		 "equipment_integration_test.erl for a global test." ).



% Determines what are the direct mother classes of this class (if any):
% (the stochastic behaviour is obtained indirectly, from failure/repair models).
%
-define( superclasses, [ class_Actor ] ).


% Exported to factor behaviours when overriding onFailure/onReparation:
-export([ notify_failure/1, notify_reparation/1 ]).


-type reliability_status() :: 'nominal' | 'dysfunction'.


-type reliability_duration() :: time_utils:dhms_duration().


-type random_profile() :: { 'uniform', pos_integer() }
						| { 'exponential', float() }
						| { 'positive_integer_exponential', pos_integer() }
						| { 'gaussian', float(), float() }
						| { 'positive_integer_gaussian', number(), number() }.


-type reliability_tick() :: class_TimeManager:tick_offset()
						  | 'uninitialized' | 'waiting'.


-type reliability_listener_pid() :: pid().


% Silences as well unused warning:
-export_type([ reliability_status/0, reliability_duration/0,
			   random_profile/0,
			   reliability_tick/0, reliability_listener_pid/0 ]).


% Tne class-specific attributes of an equipment are:
-define( class_attributes, [

	{ failure_model_pid, maybe( class_FailureModel:model_pid() ),
	  "PID of the failure model in use (if any)" },

	{ repair_model_pid, maybe( class_RepairModel:model_pid() ),
	  "PID of the repair model in use (if any)" },

	{ next_failure_tick, reliability_tick(),
	  "records the time of next failure (if any is planned)" },

	{ next_repair_tick, reliability_tick(),
	  "records the time of next reparation (if any is planned)" },

	{ current_failure_state, union( 'nominal', 'dysfunction' ),
	  "tells about the current reliability state of this equipment" },

	{ reliability_listener, maybe( reliability_listener_pid() ),
	  "records the PID on the reliability listener (if any)" },

	{ reliability_probe, maybe( class_Probe:probe_ref() ),
	  "the probe monitoring reliability (if any)" } ] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.Equipment" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% For nominal/failure status:
-include("class_ReliabilityProbe.hrl").



% Implementation notes:
%
% An equipment could also be modelled as a standalone stochastic actor, with no
% need for separate failure and reparation models.
%
% When making a reliability transition (ex: from nominal to dysfunction), we
% send two consecutive samples to the reliability probes, otherwise, as we are
% jumping from a transition to another, the probe would show triangles instead
% of solid, plain rectangles, i.e. without representing clearly at each tick
% what is the current reliability status.



% Constructs a new equipment actor, regarding notably failure and reparation.
%
% Parameters are:
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - EquipmentName is the name of this equipment (as a string)
%
% - FailureModelPid is the PID of the failure model to be used by this equipment
%
% - RepairModelPid is the PID of the repair model to be used by this equipment
%
% Child classes of this Equipment class only have to override if needed the
% following two oneway methods:
%
% - the actNominal(State) oneway, called at each tick where the equipment is in
% nominal conditions (including the ticks during when it has just been repaired)
%
% - the actInDysfunction(State) oneway, called at each tick where the equipment
% is in dysfunction (including the ticks when a dysfunction just occurred)
%
% Besides, the onFailure(State) and onReparation(State) oneways will be called
% at each transition, from nominal to dysfunction, and the other way round.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_FailureModel:model_pid(),
				 class_RepairModel:model_pid() ) -> wooper:state().
construct( State, ActorSettings, EquipmentName, FailureModelPid,
		   RepairModelPid ) ->

	% First the direct mother classes:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(EquipmentName) ),

	% Then the class-specific actions:
	% Failure state can be 'nominal' or 'dysfunction'.
	%
	% Equipments are supposed tested before being installed, thus start in
	% nominal condition:
	%
	% (cannot set next_*_tick, as no knowledge of current scheduling here)
	%
	StartingState = setAttributes( ActorState, [
		{ failure_model_pid, FailureModelPid },
		{ repair_model_pid, RepairModelPid },
		{ next_failure_tick, uninitialized },
		{ next_repair_tick, uninitialized },
		{ current_failure_state, nominal },
		{ reliability_listener, undefined },
		{ reliability_probe, undefined } ] ),

	?send_info_fmt( StartingState,
		"Creating a new equipment whose failure model is ~w and "
		"whose repair model is ~w.", [ FailureModelPid, RepairModelPid ] ),

	StartingState.




% Methods section.


% Management section of the equipment.


% Called by the failure model, in answer to a getNextFailure call.
%
% Third parameter of the request (sender PID, the failure model) is ignored.
%
-spec setNextFailure( wooper:state(), class_TimeManager:tick_offset(),
					  sending_actor_pid() ) -> actor_oneway_return().
setNextFailure( State, FailureTick, _SendingActorPid ) ->

	%trace_utils:debug_fmt( "setNextFailure, for tick #~B.", [ FailureTick ] ),

	% Consistency check:
	NewState = case ?getAttr(next_failure_tick) of

		waiting ->

		   % FailureTick is already a time (a tick), not a duration:
		   ?debug_fmt( "Equipment planned future failure at tick ~B.",
					   [ FailureTick ] ),

			PlannedState = executeOneway( State, addSpontaneousTick,
										  FailureTick ),

			setAttributes( PlannedState, [
								{ next_failure_tick, FailureTick },
								{ next_repair_tick, uninitialized } ] );

		termination_triggered ->
			State

	 end,

	actor:return_state( NewState ).



% Called by the repair model, in answer to a getNextRepair call.
%
% Third parameter (sender Pid, the repair model) is ignored.
%
-spec setNextRepair( wooper:state(), class_TimeManager:tick_offset(),
					 sending_actor_pid() ) -> actor_oneway_return().
setNextRepair( State, RepairTick, _SendingActorPid ) ->

	%trace_utils:debug_fmt( "setNextRepair, for tick #~B.", [ RepairTick ] ),

	% Consistency check:
	NewState = case ?getAttr(next_repair_tick) of

		waiting ->

		   % RepairTick is already a time (a tick), not a duration:
		   ?debug_fmt( "Equipment planned future repair at tick ~B.",
					   [ RepairTick ] ),

			PlannedState = executeOneway( State, addSpontaneousTick,
										  RepairTick ),

			setAttributes( PlannedState, [
						{ next_failure_tick, uninitialized },
						{ next_repair_tick, RepairTick } ] ) ;

		termination_triggered ->
			State

	 end,

	actor:return_state( NewState ).




% Management section of the equipment actor.


% The core of the equipment generic behaviour.
%
% Manages transition between failure and repair, and triggers actions associated
% for both of these states.
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	%?debug( "Equipment acting." ),

	%trace_utils:debug_fmt( "actSpontaneous at ~B: state is ~p.",
	% [ class_Actor:get_current_tick( State ),
	%  ?getAttr(current_failure_state) ] ),

	% Reliability is probed at tick begin:
	NewState = case ?getAttr(current_failure_state) of

		nominal ->
			handle_nominal( State );

		dysfunction ->
			handle_dysfunction( State );

		terminating ->
			executeOneway( State, scheduleNextSpontaneousTick )

	end,

	%?debug( "Equipment acted." ),

	% No need to schedule each tick, here we can jump to the next transition:
	%PlannedState = executeOneway( NewState, scheduleNextSpontaneousTick ),
	PlannedState = NewState,

	wooper:return_state( PlannedState ).




% Default implementation of the actNominal oneway.
%
% Note: made to be overridden for actual equipments.
%
-spec actNominal( wooper:state() ) -> const_oneway_return().
actNominal( State ) ->

	?warning( "Equipment actNominal/1 oneway method called." ),

	wooper:const_return().



% Default implementation of the actInDysfunction oneway.

% Note: made to be overridden for actual equipments.
%
-spec actInDysfunction( wooper:state() ) -> const_oneway_return().
actInDysfunction( State ) ->

	?warning( "Non-overridden actInDysfunction/1 oneway method called." ),

	wooper:const_return().



% Simply schedules this just created actor at the next tick (diasca 0).
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( ScheduledState ).



% Default implementation of the onFailure oneway.
%
% Note: made to be overridden for actual equipments.
%
-spec onFailure( wooper:state() ) -> oneway_return().
onFailure( State ) ->

	?warning( "Non-overridden onFailure/1 oneway method called." ),

	wooper:return_state( notify_failure( State ) ).



% Default implementation of the onReparation oneway.
%
% Note: made to be overridden for actual equipments.
%
-spec onReparation( wooper:state() ) -> oneway_return().
onReparation( State ) ->

	?warning( "Non-overridden onReparation/1 oneway method called." ),

	wooper:return_state( notify_reparation( State ) ).



% Returns the current status of this equipment regarding reliability, i.e.
% either nominal or dysfunction.
%
-spec getReliabilityStatus( wooper:state()) ->
								  const_request_return( reliability_status() ).
getReliabilityStatus( State ) ->
	wooper:const_return_result( ?getAttr(current_failure_state) ).



% Links specified reliability probe to this equipment.
%
% (request, for synchronisation purpose)
%
-spec setReliabilityProbe( wooper:state(), probe_ref() ) ->
								 request_return( 'probe_set' ).
setReliabilityProbe( State, ProbePid ) ->

	%?info( "setReliabilityProbe called." ),

	% We do not want the probe to crash without having the simulation halt:
	erlang:link( ProbePid ),

	wooper:return_state_result(
		setAttribute( State, reliability_probe, ProbePid ), probe_set ).




% Section for helper functions (not methods).


% Called whenever a failure happens.
%
% Returns an updated state.
%
% (helper)
%
-spec trigger_failure( wooper:state() ) -> wooper:state().
trigger_failure( State ) ->

	% Consistency check:
	nominal = ?getAttr(current_failure_state),

	FailureState = setAttribute( State, current_failure_state, dysfunction ),

	% Next repair tick not set yet, let's request it:
	RequestState = class_Actor:send_actor_message(
		?getAttr(repair_model_pid), getNextRepair, FailureState ),

	setAttribute( RequestState, next_repair_tick, waiting ).



% Called whenever a repair happens.
%
% Returns an updated state.
%
% (helper)
%
-spec trigger_repair( wooper:state() ) -> wooper:state().
trigger_repair( State ) ->

	% Consistency check:
	dysfunction = ?getAttr(current_failure_state),

	NominalState = setAttribute( State, current_failure_state, nominal ),

	% Next failure tick not set yet, let's request it:
	RequestState = class_Actor:send_actor_message(
		?getAttr(failure_model_pid), getNextFailure, NominalState ),

	setAttribute( RequestState, next_failure_tick, waiting ).




% Helper function for the actSpontaneous/1 oneway.
%
% Returns an updated state.
%
% (helper)
%
-spec handle_nominal( wooper:state() ) -> wooper:state().
handle_nominal( State ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	% Working correctly, thus watching for next failure:
	case ?getAttr(next_failure_tick) of

		 uninitialized->

			%trace_utils:debug_fmt(
			%   "handle_nominal at #~B: no next failure set.",
			%	[ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset, nominal, State ),

			% Failure tick not set yet, let's request it:
			RequestState = class_Actor:send_actor_message(
				?getAttr(failure_model_pid), getNextFailure, State ),

			WaitingState = setAttribute( RequestState, next_failure_tick,
										 waiting ),

			% Acts nevertheless, in a nominal way here.

			?debug( "Equipment acting normally, until knowing "
					"when the next failure will occur." ),

			% Calls directly the overridden actNominal/1 oneway:
			executeOneway( WaitingState, actNominal );


		CurrentTickOffset ->

			%trace_utils:debug_fmt( "handle_nominal at #~B: failing!~n",
			%	[ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset-1, nominal, State ),
			send_probe( CurrentTickOffset, dysfunction, State ),

			?notice( "Equipment failure." ),

			% Failure happened!
			FailedState = trigger_failure( State ),

			% Calls directly overridden onFailure oneway:

			% Notifies the transition:
			FirstFailedState = executeOneway( FailedState, onFailure ),

			% And acts accordingly to this newly failed state:
			executeOneway( FirstFailedState, actInDysfunction );


		_ ->

			% Includes any other failure tick and the 'waiting' atom:

			%trace_utils:debug_fmt( "handle_nominal at #~B: acting normally.~n",
			%	[ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset, nominal, State ),

			?debug( "Equipment acting normally." ),
			executeOneway( State, actNominal )

	end.



% Helper function for the actSpontaneous/1 oneway.
%
% Returns an updated state.
%
% (helper)
%
-spec handle_dysfunction( wooper:state() ) -> wooper:state().
handle_dysfunction( State ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	send_probe( CurrentTickOffset, dysfunction, State ),

	% If out of order, watch for repair.
	% Repair tick was already set when last failure was triggered.
	case ?getAttr(next_repair_tick) of

		 uninitialized->

			%trace_utils:debug_fmt(
			%   "handle_dysfunction at #~B: no next repair set.",
			%   [ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset, dysfunction, State ),

			% Repair tick not set yet, let's request it:
			RequestState = class_Actor:send_actor_message(
				?getAttr(repair_model_pid), getNextRepair, State ),

			WaitingState = setAttribute( RequestState, next_repair_tick,
										 waiting ),

			% Acts nevertheless, in a nominal way here.

			?debug( "Equipment acting in dysfunction, until knowing "
					"when the next reparation will occur." ),

			% Calls directly overridden actNominal oneway:
			executeOneway( WaitingState, actInDysfunction );


		CurrentTickOffset ->

			?notice( "Equipment reparation is over." ),

			%trace_utils:debug_fmt(
			%   "handle_dysfunction at #~B: being repaired!",
			%	[ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset-1, dysfunction, State ),
			send_probe( CurrentTickOffset, nominal, State ),

			% Repairing over!
			RepairState = trigger_repair( State ),

			% Calls directly overridden onReparation oneway:

			% Notifies the transition:
			FirstRepairedState = executeOneway( RepairState, onReparation ),

			% And acts accordingly to this newly repaired state, returns an
			% updated state:
			%
			executeOneway( FirstRepairedState, actNominal );


		_OtherTick ->

			%trace_utils:debug_fmt(
			%   "handle_dysfunction at #~B: being out of order.",
			%	[ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset, dysfunction, State ),

			% Includes any repair tick and the 'waiting' atom:

			?debug( "Equipment still out of order." ),

			% Returns an updated state:
			executeOneway( State, actInDysfunction )

	end.



% Notifies any reliability listener that this equipment failed.
%
% Returns an updated state.
%
% (helper)
%
-spec notify_failure( wooper:state() ) -> wooper:state().
notify_failure( State ) ->

	case ?getAttr(reliability_listener) of

		undefined ->
			State;

		ListenerPid ->
			class_Actor:send_actor_message( ListenerPid, notifyFailure, State )

	end.



% Notifies any reliability listener that this equipment was repaired.
%
% Returns an updated state.
%
% (helper)
%
-spec notify_reparation( wooper:state() ) -> wooper:state().
notify_reparation( State ) ->

	case ?getAttr(reliability_listener) of

		undefined ->
			State;

		ListenerPid ->
			class_Actor:send_actor_message( ListenerPid, notifyReparation,
											State )

	end.


% Sends reliability information to the probe.
%
% (helper)
%
send_probe( CurrentTickOffset, Status, State ) ->

	case ?getAttr(reliability_probe) of

		undefined ->
			ok;

		ProbePid ->

			SampleData = case Status of

				nominal ->
					?nominal_status;

				dysfunction ->
					?failed_status

			end,

			ProbePid ! { setData, [ CurrentTickOffset, { SampleData } ] }

	end.
