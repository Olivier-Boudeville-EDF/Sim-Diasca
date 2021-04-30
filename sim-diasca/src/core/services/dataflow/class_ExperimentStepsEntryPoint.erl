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


-module(class_ExperimentStepsEntryPoint).

-define( class_description,
		 "The experiment entry point is a singleton instance in charge of "
		 "being the (logical) starting point that impulses the evaluation of "
		 "the registered dataflows, possibly at each timestep; technically it "
		 "is run in second position, just after the experiment exit point that "
		 "triggers it. "
		 "This specialised version of the ExperimentEntryPoint has been "
		 "introduced in order to manage simulation steps."
		 "Note: no class_ExperimentStepsExitPoint has been defined, as the "
		 "base implementation (class_ExperimentExitPoint) is sufficient "
		 "here." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ExperimentEntryPoint ] ).



% Design notes:
%
% The Dataflow Entry Point knows the various dataflows involved, which is useful
% whenever having to create, update, delete, etc. dataflow blocks (which each
% pertains to a given dataflow).
%
% This (experiment) entry point knows the experiment manager (as it may have to
% perform experiment-level operations) but also the world manager (as it may
% typically impulse changes in the simulation world).


% Please refer to the 'Dataflow Entry & Exit Points' and 'Scheduling Cycle of
% Experiments' sections of the Dataflow HOWTO in order to understand why this
% actor is purely passive and (only) triggered by its ExperimentExitPoint
% counterpart component.



% Attributes that are specific to such an experiment entry point are:
-define( class_attributes, [

	{ current_step, step_count(),
	  "the current step at which the experiment is" },

	{ max_step, step_count(),
	  "the maximum step that the experiment may reach" } ] ).



% Helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Experiment.EntryPoint" ).


% For types and shorthands:
-include("dataflow_defines.hrl").


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").


% Shorthands:
-type ustring() :: text_utils:ustring().
-type step_count() :: class_ExperimentManager:step_count().




% Constructs this experiment entry point, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Dataflows is a list of the dataflows that this entry point should drive
%
% - ExperimentStepStart is the step at which the experiment shall start
%
% - ExperimentStepStop is the step at which the experiment shall stop
%
% - ExperimentManagerPid is the PID of the experiment manager
%
% - WorldManagerPid is the PID of the world manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		[ dataflow_pid() ], step_count(), step_count(),
		experiment_manager_pid(), world_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, Dataflows, ExperimentStepStart,
		   ExperimentStepStop, ExperimentManagerPid, WorldManagerPid ) ->

	% First the direct mother class:
	ActorState = class_ExperimentEntryPoint:construct( State, ActorSettings,
							Dataflows, ExperimentManagerPid, WorldManagerPid ),

	% Then the class-specific actions:
	setAttributes( ActorState, [ { current_step, ExperimentStepStart },
								 { max_step, ExperimentStepStop } ] ).



% Methods section.


% Callback executed on the first diasca of existence of this entry point.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							const_actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?debug_fmt( "Created ~ts.", [ to_string( State ) ] ),

	actor:const_return().



% Starts the evaluation of the experiment for the current tick.
%
% Typically called by the experiment exit point.
%
% Mostly empty implementation, meant to be overridden.
%
-spec startExperimentTick( wooper:state(), sending_actor_pid() ) ->
								actor_oneway_return().
startExperimentTick( State, _SenderActorPid ) ->

	CurrentStep = ?getAttr(current_step),


	% This is an empty implementation.
	%
	% Actual ones may fetch information from any source (ex: thanks to a REST
	% call), and may update accordingly the corresponding dataflow elements
	% (typically dataflow actors), possibly directly or through the various
	% registered dataflows.
	%
	% Then corresponding blocks may be activated, and the dataflow evaluated.

	%SentState = class_Actor:send_actor_messages( ?getAttr(dataflows),
	%						{ startExperimentTick, [...], State },

	%actor:return_state( SentState ).

	NewState = setAttribute( State, current_step, CurrentStep+1 ),

	?debug_fmt( "Shifting to step ~B/~B.",
				[ CurrentStep+1, ?getAttr(max_step) ] ),

	actor:return_state( NewState ).




% Helper functions.


% Returns a textual description of this entry point.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	DataflowString = case ?getAttr(dataflows) of

		[] ->
			"not referencing any dataflow";

		[ Dataflow ] ->
			text_utils:format( "referencing a single dataflow instance: ~p",
							   [ Dataflow ] );

		Dataflows ->
			text_utils:format( "referencing ~B dataflow instances: ~w",
							   [ length( Dataflows ), Dataflows ] )

	end,

	text_utils:format( "Experiment entry point (in step ~B/~B), associated to "
		"the experiment manager ~w, to the world manager ~w, and ~ts",
		[ ?getAttr(current_step), ?getAttr(max_step),
		  ?getAttr(experiment_manager_pid),
		  ?getAttr(world_manager_pid), DataflowString ] ).
