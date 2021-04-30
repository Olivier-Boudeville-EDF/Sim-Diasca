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


-module(class_ExperimentStepsExitPoint).

-define( class_description,
		 "The experiment exit point is a singleton instance in charge of being "
		 "the (logical) stopping point that terminates the evaluation of the "
		 "registered dataflows, possibly at each timestep; technically it is "
		 "run first (spontaneously), and once done triggers the experiment "
		 "entry point."
		 "Note: see also the class_BaseTestExitPoint.erl in the Dataflow Urban "
		 "Example" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ExperimentExitPoint ] ).



% Design notes:
%
% The Dataflow Exit Point knows the various dataflows involved, but this may not
% be useful (and thus may be removed in the future).
%
% Please refer to the 'Dataflow Exit & Exit Points' and 'Scheduling Cycle of
% Experiments' sections of the Dataflow HOWTO in order to understand why this
% actor is spontaneously scheduled and triggers its ExperimentEntryPoint
% counterpart component.



% Attributes that are specific to such an experiment exit point are:
-define( class_attributes, [

	{ current_step, class_ExperimentManager:step_count(),
	  "current step at which the experiment is" },

	{ max_step, class_ExperimentManager:step_count(),
	  "the maximum step that the experiment may reach" } ] ).



% Helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Experiment.ExitPoint" ).


% For types and shorthands:
-include("dataflow_defines.hrl").


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").


% Implementation notes:
%
% Currently the only termination criterion is reaching the final step. At this
% positional parameter other criteria may be added in the future.


% Shorthands:
-type ustring() :: text_utils:ustring().


% Constructs such experiment exit point, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Dataflows is a list of the dataflows that this exit point should drive
%
% - ExperimentStepStart is the number of steps the overall experiment shall
% start from (ex: first year)
%
% - ExperimentStepStop is the number of steps the overall experiment shall stop
% at (ex: last year)
%
% - ExperimentEntryPointPid is the PID of the entry point of the experiment
%
% - ExperimentManagerPid is the PID of the experiment manager
%
% - WorldManagerPid is the PID of the world manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		[ dataflow_pid() ], class_ExperimentManager:step_count(),
		class_ExperimentManager:step_count(),
		experiment_entry_point_pid(), experiment_manager_pid(),
		world_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, Dataflows, ExperimentStepStart,
		ExperimentStepStop, ExperimentEntryPointPid, ExperimentManagerPid, 
        WorldManagerPid ) ->

	% First the direct mother class:
	ActorState = class_ExperimentExitPoint:construct( State, ActorSettings,
					Dataflows, ExperimentEntryPointPid,
					ExperimentManagerPid, WorldManagerPid ),

	% Then the class-specific actions:
	setAttributes( ActorState, [ { current_step, ExperimentStepStart },
								 { max_step, ExperimentStepStop } ] ).



% Methods section.


% Callback executed on the first diasca of existence of this exit point.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						    actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?debug_fmt( "Created ~ts.", [ to_string( State ) ] ),

	% Start from this very first diasca:
	ActState = executeOneway( State, actSpontaneous ),

	actor:return_state( ActState ).



% The core of the behaviour of this exit point.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	Phase = ?getAttr(phase),

	CurrentStep = ?getAttr(current_step),
	MaxStep = ?getAttr(max_step),

	?debug_fmt( "Acting spontaneously, in ~ts phase, at step ~B/~B, "
		"on behalf of (any) previous one.", [ Phase, CurrentStep, MaxStep ] ),

	% Update our state (detect termination) depending on the steps:
	StepState = case CurrentStep >= MaxStep of

		true ->
			?notice_fmt( "Reached step ~B (maximum one being ~B), terminating.",
						 [ CurrentStep, MaxStep ] ),
			setAttribute( State, phase, termination );

		false ->
			State

	end,

	SpontaneousState = executeOnewayAs( StepState, class_ExperimentExitPoint,
										actSpontaneous ),

	NewStep = CurrentStep + 1,

	YearState = setAttribute( SpontaneousState, current_step, NewStep ),

	?debug_fmt( "Shifting to step ~B/~B.", [ NewStep, MaxStep ] ),

	wooper:return_state( YearState ).



% Declares the termination of the experiment.
%
% Note: usually this is determined internally.
%
% -spec declareExperimentTermination( wooper:state() ) -> oneway_return().
% declareExperimentTermination( State ) ->

%	NewState = case ?getAttr(phase) of

%		termination ->
%			throw( already_terminated);

%		_ ->
%			?info( "Experiment terminating now." ),
%			setAttribute( State, phase, termination )

%	end,

%	wooper:return_state( NewState ).





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

	text_utils:format( "Experiment exit point in ~ts phase (in step ~B/~B), "
		"referencing its entry point counterpart ~p, associated to the "
		"experiment manager ~w, to the world manager ~w and ~ts",
		[ ?getAttr(phase), ?getAttr(current_step),
		  ?getAttr(max_step), ?getAttr(entry_point_pid),
		  ?getAttr(experiment_manager_pid),
		  ?getAttr(world_manager_pid), DataflowString ] ).
