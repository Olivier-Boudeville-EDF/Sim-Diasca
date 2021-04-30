% Copyright (C) 2016-2021 EDF R&D

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


-module(class_ExperimentExitPoint).


-define( class_description,
		 "The experiment exit point is a singleton instance in charge of being "
		 "the (logical) stopping point that terminates the evaluation of the "
		 "registered dataflows, possibly at each timestep; technically it is "
		 "run first (spontaneously), and once done triggers the experiment "
		 "entry point." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).



% Design notes:
%
% The Dataflow Exit Point knows the various dataflows involved, but this may not
% be useful (and thus may be removed in the future).
%
% Please refer to the 'Dataflow Exit & Exit Points' and 'Scheduling Cycle of
% Experiments' sections of the Dataflow HOWTO in order to understand why this
% actor is spontaneously scheduled and triggers its ExperimentEntryPoint
% counterpart component.



% Attributes that are specific to an experiment exit point are:
-define( class_attributes, [

	{ entry_point_pid, experiment_entry_point_pid(), "the PID of the entry "
	  "point that this exit point will trigger" },

	{ experiment_manager_pid, experiment_manager_pid(),
	  "the PID of the experiment manager" },

	{ world_manager_pid, world_manager_pid(), "the PID of the world manager" },

	{ dataflows, [ dataflow_pid() ], "a list of the dataflow instances known "
	  "by this experiment manager" },

	{ phase, class_ExperimentManager:phase(), "tells at which step of its "
	  "behaviour this exit point is" } ] ).



% Helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Experiment.ExitPoint" ).


% For world_manager_pid() and all:
-include("dataflow_defines.hrl").


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").


% Implementation notes:
%
% This base class does not define by itself a criterion for the termination of
% the simulation, it is up to its child classes to define at least one.


% Shorthands:
-type ustring() :: text_utils:ustring().



% Constructs the experiment exit point, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Dataflows is a list of the dataflows that this exit point should drive
%
% - ExperimentEntryPointPid is the PID of the entry point of the experiment
%
% - ExperimentManagerPid is the PID of the experiment manager
%
% - WorldManagerPid is the PID of the world manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		[ dataflow_pid() ], experiment_entry_point_pid(),
		experiment_manager_pid(), world_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, Dataflows, ExperimentEntryPointPid,
		   ExperimentManagerPid, WorldManagerPid ) ->

	% Auto-subscribing:
	RegistrationMessage = { registerExperimentExitPoint, [], self() },

	ExperimentManagerPid ! RegistrationMessage,
	WorldManagerPid ! RegistrationMessage,

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
								?trace_categorize("ExperimentExitPoint") ),

	% Then the class-specific actions:
	FinalState = setAttributes( ActorState, [
				{ entry_point_pid, ExperimentEntryPointPid },
				{ experiment_manager_pid, ExperimentManagerPid },
				{ world_manager_pid, WorldManagerPid },
				{ dataflows, Dataflows },
				{ phase, initialisation } ] ),

	%?send_info( FinalState, "Registering now." ),

	% From both registerExperimentExitPoint requests:
	wooper:wait_for_request_acknowledgements( _Count=2,
				_AckAtom=experiment_exit_point_registered ),

	%?send_info( FinalState, "Registered, and initialised." ),

	FinalState.



% Methods section.


% Callback executed on the first diasca of existence of this exit point.
-spec onFirstDiasca( wooper:state(), pid() ) -> actor_oneway_return().
onFirstDiasca( State, _CallerPid ) ->

	?debug_fmt( "Created ~ts.", [ to_string( State ) ] ),

	% Before we start the actual dataflow evaluation (based on actSpontaneous/1
	% and typically the injection of events), there may have been initial blocks
	% that have been created (and linked) yet that are still suspended.
	%
	% They shall be explicitly resumed, otherwise, typically from a programmatic
	% case, no scheduling would happen at the first tick, and either the
	% simulation would terminate or jump at any next planned tick, which would
	% then detect that there are suspended blocks that should have been resumed,
	% but have not.

	% So we first resume these suspended blocks, and once any resulting
	% evaluation will be over, we will start for real and act spontaneously.
	%
	% However, at this very first diasca, initial blocks (notably dataflow
	% objects) are also in the process of registering themselves (from their
	% onFirstDiasca/2 oneway) to their dataflow.
	%
	% This exit point should thus initiate the dataflow evaluation by sending to
	% the known dataflows a resume order only at the next diasca (as otherwise
	% the dataflows would receive at the same, next diasca, both the
	% registrations of the dataflow objects and the request to resume the
	% currently known ones), so that all initial blocks (and not only those that
	% happened to be registered by dataflows before this resume order) are
	% actually taken into account for that resume.
	% So:


	% Plan to resume initial blocks at next diasca by auto-scheduling this exit
	% point:
	%
	AutoSentState = class_Actor:send_actor_message( self(),
										initiateDataflowEvaluation, State ),

	actor:return_state( AutoSentState ).



% Initiates the evaluation of the known dataflows, supposing all initial blocks
% have already registered themselves to their respective dataflows.
%
-spec initiateDataflowEvaluation( wooper:state(), sending_actor_pid() ) ->
										actor_oneway_return().
initiateDataflowEvaluation( State, _SendingActorPid ) ->

	?debug( "Initiating dataflow evaluation." ),

	Dataflows = ?getAttr(dataflows),

	% Wake up known dataflows:
	SentState = class_Actor:send_actor_messages( Dataflows,
												 resumeSuspendedBlocks, State ),

	% And operate as well, notably to plan next actions:
	ActState = executeOneway( SentState, actSpontaneous ),

	actor:return_state( ActState ).



% The core of the behaviour of this exit point.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	Phase = ?getAttr(phase),

	?debug_fmt( "Acting spontaneously, in ~ts phase, on behalf of (any) "
				"previous one.", [ Phase ] ),

	FinalState = case Phase of

		initialisation ->
			EntryPointPid = ?getAttr(entry_point_pid),

			?notice_fmt( "Initialisation over; performing first trigger "
						 "of entry point ~p.", [ EntryPointPid ] ),

			SentState = class_Actor:send_actor_message( EntryPointPid,
								startExperimentTick, State ),

			NextState = executeOneway( SentState,
									   scheduleNextSpontaneousTick ),

			setAttribute( NextState, phase, simulation );

		simulation ->
			EntryPointPid = ?getAttr(entry_point_pid),

			%?debug_fmt( "Normal trigger of entry point ~p.",
			%			 [ EntryPointPid ] ),

			SentState = class_Actor:send_actor_message( EntryPointPid,
									startExperimentTick, State ),

			executeOneway( SentState, scheduleNextSpontaneousTick );


		termination ->
			?info( "Termination reached, no more trigger of entry "
					"point nor additional scheduling." ),
			State

	end,

	wooper:return_state( FinalState ).



% Declares the termination of the experiment.
%
% Note: usually this is determined internally.
%
-spec declareExperimentTermination( wooper:state() ) -> oneway_return().
declareExperimentTermination( State ) ->

	NewState = case ?getAttr(phase) of

		termination ->
			throw( already_terminated );

		_ ->
			?info( "Experiment terminating now." ),
			setAttribute( State, phase, termination )

	end,

	wooper:return_state( NewState ).





% Helper functions.


% Returns a textual description of this exit point.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	DataflowString = case ?getAttr(dataflows) of

		[] ->
			"not referencing any dataflow";

		[ Dataflow ] ->
			text_utils:format( "referencing a single dataflow instance: ~p",
							   [ Dataflow ] );

		Dataflows ->
			text_utils:format( "referencing ~B dataflow instances: ~p",
							   [ length( Dataflows ), Dataflows ] )

	end,

	text_utils:format( "Experiment exit point in ~ts phase referencing "
		"its entry point counterpart ~p, associated to the experiment "
		"manager ~w, to the world manager ~w and ~ts",
		[ ?getAttr(phase), ?getAttr(entry_point_pid),
		  ?getAttr(experiment_manager_pid), ?getAttr(world_manager_pid),
		  DataflowString ] ).
