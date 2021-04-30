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


-module(class_DataLoggingActor).


-define( class_description,
		 "Test of the data-logger facilities, from a simulation actor." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


% The class-specific attributes of a datalogging actor instance:
-define( class_attributes, [

	 { first_probe_ref, class_DataLogger:virtual_probe_reference(),
	   "reference onto the first virtual probe created" },

	 { second_probe_ref, class_DataLogger:virtual_probe_reference(),
	   "reference onto the second virtual probe created" },

	{ listener_pid, maybe( pid() ),
	  "the PID of any process (ex: the test case) listening to this test "
	  "actor; allows to notify it that the report generation is over" },

	 { talkative, boolean(), "tells whether this actor is talkative" },

	 { termination_tick_offset, class_TimeManager:tick_offset(),
	   "the tick offset at which this test actor will terminate" } ] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.Test.Datalogging" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").




% Constructs a new test actor for data-logging:
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - ActorName the name of the actor
%
% - TerminationTickOffset the duration after which this actor should terminate
%
% - ListenerPid for any listener of this actor
%
% This test actor creates two (virtual) probes.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		class_Actor:name(), class_TimeManager:tick_offset(), pid() ) ->
						wooper:state().
construct( State, ActorSettings, ActorName, TerminationTickOffset,
		   ListenerPid ) ->

	% Cannot use 'output' yet (no talkative attribute):
	%trace_utils:debug_fmt( "Creating a test actor named '~ts'.",
	%	[ ActorName ] ),

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(ActorName) ),

	% Will be used as probe name as well:
	FirstTitle = text_utils:format( "Curves A for actor ~w", [ self() ] ),

	FirstVirtualProbePair = class_DataLogger:create_virtual_probe(
			 _FirstProbeName=FirstTitle,
			 _FirstCurveNames=[ "Curve A1", "Curve A2" ],
			 _FirstZones=[],
			 _FirstTitle=FirstTitle,
			 _FirstXLabel="Simulation tick",
			 _FirstYLabel="Curve A values" ),

	SecondTitle = text_utils:format( "Curves B for actor ~w", [ self() ] ),

	SecondVirtualProbePair = class_DataLogger:create_virtual_probe(
			 _SecondProbeName=SecondTitle,
			 _SecondCurveNames=[ "Curve B1", "Curve B2", "Curve B3" ],
			 _SecondZones=[],
			 _SecondTitle=SecondTitle,
			 _SecondXLabel="Simulation tick",
			 _SecondYLabel="Curve B values" ),

	?send_notice_fmt( ActorState, "Creating a new datalogging test actor, "
		"terminating no sooner than tick offset #~w.",
		[ TerminationTickOffset ] ),

	setAttributes( ActorState, [

		{ first_probe_ref, FirstVirtualProbePair },
		{ second_probe_ref, SecondVirtualProbePair },
		{ listener_pid, ListenerPid },

		% Useful to select console verbosity:
		{ talkative, false },
		%{ talkative, true},

		{ termination_tick_offset, TerminationTickOffset } ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?notice( "Deleting datalogging test actor." ),

	% No specific probe deletion.

	% Then allow chaining:
	State.




% Methods section.


% Management section of the actor.



% The core of the test actor behaviour.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	TerminationOffset = ?getAttr(termination_tick_offset),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case ?getAttr(current_tick_offset) of

		PastOffset when PastOffset >= TerminationOffset ->

			?notice( "Test Actor preparing termination." ),

			TerminatingState = executeOneway( State, declareTermination ),

			output( "~w terminating at #~B", [ self(), PastOffset ], State ),

			TerminatingState;

		CurrentOffset ->

			output( "~w acting spontaneously at #~B on ~ts",
					[ self(), CurrentOffset, net_utils:localnode() ], State ),

			send_probe_data( CurrentOffset, State ),

			executeOneway( State, addSpontaneousTick, CurrentOffset + 7 )

	end,
	wooper:return_state( NewState ).



% Section for actor oneways.


-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% Let's simply schedule this actor for the next tick:
	NewState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( NewState ).




% Section for helper functions (not methods).


% Outputs specified message in console, iff talkative.
%
% (helper)
%
-spec output( text_utils:format_string(), text_utils:format_values(),
			  wooper:state() ) -> void().
output( MessageFormat, FormatValues, State ) ->

	case ?getAttr(talkative) of

		true ->
			TickOffset = class_Actor:get_current_tick_offset( State ),
			trace_utils:debug_fmt( "[~ts (~w) at ~p] " ++ MessageFormat,
				[ ?getAttr(name), self(), TickOffset ] ++ FormatValues );

		false ->
			ok

	end.



% Sends data to the virtual probes (if any was selected).
send_probe_data( CurrentOffset, State ) ->

	% Depending on the result specification, one probe may be wanted while the
	% other not.
	%
	% In both cases, should a (virtual) probe be not wanted, automatically no
	% sample will be sent to it:

	class_DataLogger:send_data( ?getAttr(first_probe_ref), CurrentOffset,
					_FirstSample={ random_utils:get_random_value( 50 ),
								   random_utils:get_random_value( 30 ) } ),

	class_DataLogger:send_data( ?getAttr(second_probe_ref), CurrentOffset,
					_SecondSample={ random_utils:get_random_value( 100 ),
									random_utils:get_random_value( 30 ),
									random_utils:get_random_value( 150 ) } ).
