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


-module(class_FailureModel).


-define( class_description,
		 "Class modelling failure behaviours, typically of equipments. "
		 "Most failure models rely on underlying random generators. "
		 "Their instances must be simulation actors, so that the total "
		 "ordering of their incoming messages is recreated, otherwise the "
		 "generation of the random numbers will break reproducibility. "
		 "Thus a failure model is a stochastic actor. "
		 "See equipment_integration_test.erl for an integration test." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_StochasticActor ] ).


-type mttf() :: class_Equipment:reliability_duration().

-type model_pid() :: actor_pid().

-export_type([ mttf/0, model_pid/0 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.StochasticActor.FailureModel" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Constructs a new failure model:
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - FailureModelName is its name
%
% - RandomProfile is a tuple describing the kind of randomness needed to compute
% failures (if any)
%
%
% RandomProfile can be among:
%
% - {uniform, N} for uniform laws (positive integer)
%
% - {exponential, Lambda} for exponential laws (floating-point)
%
% - {positive_integer_exponential, Lambda} for exponential laws (positive
% integer)
%
% - {gaussian, Mu, Sigma} for gaussian laws
%
% - {positive_integer_gaussian, Mu, Sigma} for gaussian laws (positive integer)
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
	  class_Actor:name(), class_Equipment:random_profile() ) -> wooper:state().
construct( State, ActorSettings, FailureModelName, RandomProfile ) ->

	% First the direct mother classes:
	% (this particular random list stores only one profile, the failure one)
	StochasticState = class_StochasticActor:construct( State, ActorSettings,
			 ?trace_categorize(FailureModelName),
			 [ { failure_profile, RandomProfile } ] ),

	% Then the class-specific actions:

	?send_info_fmt( StochasticState, "Creating a new failure model "
		"whose failure profile is ~w, with, "
		"as default upper-bound of random consumption.", [ RandomProfile ] ),

	StochasticState.



% Methods section.


% Management section of the actor.


% Defined simply to avoid a useless warning to be issued / an exception to be
% thrown.
%
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   const_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->
	wooper:const_return().


% A failure model is purely passive, it only answers to requests, and it does
% not even override its 'actSpontaneous' method.



% Requests this model to determine (asynchronously) the tick offset of next
% failure, i.e. computes the next tick offset at which the caller equipment will
% fail (assuming it is just created or repaired).
%
% (actor oneway, but triggers back a oneway on the caller)
%
-spec getNextFailure( wooper:state(), sending_actor_pid() ) ->
							actor_oneway_return().
getNextFailure( State, EquipmentPid ) ->

	% Uses directly stochastic mother class (automatic background refill):
	FailureDurationInSeconds =
		class_StochasticActor:get_random_value_from( failure_profile, State ),

	% FailureDuration is in seconds, converting to ticks:
	%
	% (results in at least two ticks, so that the caller is never supposed to
	% fail at the same tick it receives that information)
	%
	% We also relax the maximum relative error, so that such a test is unlikely
	% to fail.
	%
	FailureDurationInTicks = erlang:max( 2,
	  class_Actor:convert_seconds_to_ticks( FailureDurationInSeconds,
					 _MaxRelativeError=0.5, State ) ),

	%trace_utils:debug_fmt( "Failure duration: ~w seconds, i.e. ~B ticks.",
	%				  [ FailureDurationInSeconds, FailureDurationInTicks ] ),

	FailureTickOffset = class_Actor:get_current_tick_offset( State )
		+ FailureDurationInTicks,

	?info_fmt( "Determined next failure at tick offset #~B for ~w.",
			   [ FailureTickOffset, EquipmentPid ] ),

	% Answer can be returned directly, as this method is triggered by reordered
	% actor messages, like its answer will be (at next tick):
	%
	SentState = class_Actor:send_actor_message( EquipmentPid,
		{ setNextFailure, FailureTickOffset }, State ),

	actor:return_state( SentState ).
