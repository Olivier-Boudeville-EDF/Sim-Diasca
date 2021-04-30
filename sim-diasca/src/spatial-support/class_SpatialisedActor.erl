% Copyright (C) 2014-2021 EDF R&D

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

-module(class_SpatialisedActor).


-define( class_description,
		 "Class modelling an actor taking part to a spatial environment, "
		 "i.e. being at a given position in this environment." ).


% Determines what are the direct mother classes of this class (if any):
% (SpatialisedTraceEmitter to supersede TraceEmitter, inherited from Actor)
-define( superclasses, [ class_Actor ] ).


-define( class_attributes,[

	{ position, position(),
	  "current position of this actor; note that between two calls this value "
	  "must always be up to date, as it might be requested directly "
	  "(bypassing actor messages) by the environment in some cases" },

	{ max_speed, maybe( class_TwoDimensionalEnvironment:max_speed() ),
	  "upper-bound (if any) of the maximum speed of this actor" },

	{ environment_pid, environment_pid(),
	  "PID of the environment this actor will live in" },

	{ env_width, class_TwoDimensionalEnvironment:border_extent(),
	  "abscissa extent of the environment, from its origin" },

	{ env_height, class_TwoDimensionalEnvironment:border_extent(),
	  "ordinate extent of the environment, from its origin" },

	{ border_settings, class_TwoDimensionalEnvironment:border_description(),
	  "describes how border crossing shall be managed" } ] ).


-type position() :: class_TwoDimensionalEnvironment:position().

-export_type([ position/0 ]).


-export([ get_spatial_message/2 ]).




% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Spatial.SpatialisedActor" ).


-include("sim_diasca_for_spatialised_actors.hrl").


% Shorthands:

-type environment_pid() :: class_TwoDimensionalEnvironment:environment_pid().
-type position() :: class_TwoDimensionalEnvironment:position().



% Creates a new spatialised actor, in a 2D environment.
%
% Construction parameters are:
%
% - ActorSettings is the AAI assigned by the load-balancer to this actor
%
% - Name is the name of this actor
%
% - InitialPosition is the initial position of this actor in the specified
% environment
%
% - MaxSpeed is an upper-bound (if any) of the maximum speed of this actor
% (allows for better environment-level performances)
%
% - EnvironmentPid is the PID of the environment this actor will live in
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		class_Actor:name(), position(),
		class_TwoDimensionalEnvironment:max_speed(), environment_pid() ) ->
					   wooper:state().
construct( State, ActorSettings, Name, InitialPosition, MaxSpeed,
		   EnvironmentPid ) ->

	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize( Name ) ),

	setAttributes( ActorState, [

			{ position, InitialPosition },
			{ max_speed, MaxSpeed },
			{ environment_pid, EnvironmentPid },

			% Will serve as cached environment information:
			{ env_width, undefined },
			{ env_height, undefined },
			{ border_settings, undefined } ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	% We do not undeclare this spatialised actor automatically from this
	% environment, as this must be done in the course of the simulation.

	% This can be done easily thanks to:
	%SentState = class_Actor:send_actor_message( ?getAttr(environment_pid),
	%											undeclare, State ),

	% Then allow chaining:
	State.





% Section for actor oneways.



% First scheduling on this spatialised actor.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% Declaring ourself to the environment:
	SentState = class_Actor:send_actor_message(
				  ?getAttr(environment_pid),
				  { declareEntity, [ ?getAttr(position), ?getAttr(max_speed),
									 wooper:get_classname( State ) ] },
				  State ),

	% Creates an initial deadline at the tick, to trigger the burners:
	actor:return_state( SentState ).




% The definition of the spontaneous behaviour of this incinerator.
%
-spec actSpontaneous( wooper:state() ) -> const_actor_oneway_return().
actSpontaneous( State ) ->
	% Nothing specific here, no futur planned spontaneous action.
	actor:const_return().



% Requests this actor to return back its current position.
%
% Notably called by the environment.
%
-spec getPosition( wooper:state(), sending_actor_pid() ) ->
						 actor_oneway_return().
getPosition( State, SenderPid ) ->

	SentState = class_Actor:send_actor_message( SenderPid,
					{ notifyPosition, ?getAttr(position) }, State ),

	actor:return_state( SentState ).



-spec notifyEnvironmentSettings( wooper:state(),
			class_TwoDimensionalEnvironment:border_extent(),
			class_TwoDimensionalEnvironment:border_extent(),
			class_TwoDimensionalEnvironment:border_description(),
						sending_actor_pid() ) -> actor_oneway_return().
notifyEnvironmentSettings( State, Width, Height, BorderSettings, _EnvPid ) ->

	actor:return_state( setAttributes( State, [
			{ env_width, Width },
			{ env_height, Height },
			{ border_settings, BorderSettings } ] ) ).



% Returns a textual description of this instance.
-spec toString( wooper:state() ) -> const_request_return( string() ).
toString( State ) ->
	 wooper:const_return_result( to_string( State ) ).



% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->
	text_utils:format( "Spatialised actor ~w whose position is ~p (max speed: "
					   "~p meters per second), using environment ~w",
					   [ self(), ?getAttr(position), ?getAttr(max_speed),
						 ?getAttr(environment_pid) ] ).
