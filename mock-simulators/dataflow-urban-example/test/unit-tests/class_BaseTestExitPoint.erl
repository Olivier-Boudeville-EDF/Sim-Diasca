% Copyright (C) 2016-2022 EDF R&D

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


% @doc Experiment exit point, defined for testing.
-module(class_BaseTestExitPoint).


-define( class_description,
		 "This (programmatic) example of experiment exit point starts, as "
		 "at least most exit points, each step of this test experiment."
		 "This exit point does not rely on changesets to operate." ).


% See also class_ExperimentExitPoint.erl.
% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ExperimentExitPoint ] ).


% Helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Unit-testing.BaseTestExitPoint" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


-include("dataflow_unit_test_defines.hrl").


% Implementation notes:
%
% Currently the only termination criterion is reaching the final step. At this
% positional parameter other criteria may be supported in the future.


% Attributes that are specific to this test experiment exit point are:
-define( class_attributes, [

	{ current_step, step_count(), 
	  "the current step at which the experiment is" },

	{ max_step, step_count(),
	  "the maximum step that the experiment may reach" } ] ).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type step_count() :: class_ExperimentManager:step_count().



% @doc Constructs the urban-example experiment exit point, from:
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
	[ dataflow_pid() ], step_count(), step_count(),
	experiment_entry_point_pid(), experiment_manager_pid(),
	world_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, Dataflows, ExperimentStepStart,
		   ExperimentStepStop, ExperimentEntryPointPid,
		   ExperimentManagerPid, WorldManagerPid ) ->

	% First the direct mother class:
	ExitState = class_ExperimentExitPoint:construct( State, ActorSettings,
		Dataflows, ExperimentEntryPointPid,
		ExperimentManagerPid, WorldManagerPid ),

	% Then the class-specific actions:
	setAttributes( ExitState, [ { current_step, ExperimentStepStart },
								{ max_step, ExperimentStepStop } ] ).



% Methods section.


% @doc The core of the behaviour of this base exit point, mostly adding to the
% default exit point a fixed termination step.
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentStep = ?getAttr(current_step),
	MaxStep = ?getAttr(max_step),

	?debug_fmt( "Base exit point at step ~B/~B, in ~ts phase.",
				[ CurrentStep, MaxStep, ?getAttr(phase) ] ),

	NewState = case CurrentStep of

		Step when Step >= MaxStep ->
			setAttribute( State, phase, termination );

		_ ->
			setAttribute( State, current_step, CurrentStep+1 )

	end,

	ActState = executeOnewayAs( NewState, class_ExperimentExitPoint,
								actSpontaneous ),

	wooper:return_state( ActState ).




% Helper functions.


% Returns a textual description of this exit point.
%
% (helper)
%
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	ExitString = class_ExperimentExitPoint:to_string( State ),

	text_utils:format( "Test programmatic ~ts", [ ExitString ] ).
