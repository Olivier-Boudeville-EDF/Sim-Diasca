% Copyright (C) 2011-2021 EDF R&D

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


-module(class_DataExchangeTestActor).


-define( class_description,
		 "Test of the data-exchange facilities, from a simulation actor." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


-define( class_attributes, [

	 { data_key, class_DataExchanger:key(),
	   "designates a value of interest read and possibly modified by this "
	   "test actor interacting with the data-exchanger" },

	 { termination_tick_offset, class_TimeManager:tick_offset(),
	   "the tick offset at which this test actor will terminate" },

	 { talkative, boolean(), "tells whether this actor is talkative" },

	 { expected_value, integer(), "records the test expected value" } ] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.Test.DataExchange" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").




% Implementation notes:
%
% This test actor will perform data-exchanges (read/modify/write). The point is
% that it does not even know whether the simulation is started or not (in this
% case it is an initial actor).



% Constructs a new test actor for data-exchange:
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - ActorName the name of the actor
%
% - DataKey is an atom corresponding to the key of a data entry of interest for
% that actor
%
% - TerminationTickOffset the duration after which this actor should terminate
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), class_DataExchanger:key(),
				class_TimeManager:tick_offset() ) -> wooper:state().
construct( State, ActorSettings, ActorName, DataKey, TerminationTickOffset ) ->

	% Cannot use 'output' yet (no talkative attribute):
	%trace_utils:debug_fmt( "Creating a test actor named '~s', with key '~p'.",
	%	[ ActorName, DataKey ] ),

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(ActorName) ),

	?send_notice_fmt( ActorState, "Creating a new data-exchange test actor, "
		"terminating no sooner than tick offset #~w.",
		[ TerminationTickOffset ] ),

	% This actor will make use of the data-exchange service:
	ExchangeState = class_Actor:enable_data_exchange( ActorState ),

	% Common to all actors:
	{ 3, mutable } = class_Actor:read_qualified_data( example_key_for_actors,
													  ExchangeState ),

	1 = class_Actor:read_data( other_example_key_for_actors, ExchangeState ),


	% Read-modify-write its "own" specified key:
	V = class_Actor:read_data( DataKey, ExchangeState ),
	class_Actor:modify_data( DataKey, V + 1, mutable, ExchangeState ),

	setAttributes( ExchangeState, [

		{ data_key, DataKey },
		{ termination_tick_offset, TerminationTickOffset },

		% Useful to select console verbosity:
		{ talkative, false },
		%{ talkative, true },

		% Allows to check that the value read from the data-exchange service is
		% correct:
		%
		{ expected_value, V + 1 } ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%trace_utils:debug_fmt( "Test actor ~w deleted, while was still at tick "
	%                       "offset #~p.",
	%						[ self(), ?getAttr(current_tick_offset) ] ),

	% Class-specific actions:
	?notice( "Deleting data-exchange test actor." ),

	% Then allow chaining:
	State.




% Methods section.


% Management section of the actor.


% The core of the test actor behaviour.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	%trace_utils:debug_fmt( "--> tick offset #~p for ~w.~n",
	%	[ ?getAttr(current_tick_offset), self() ] ),

	TerminationOffset = ?getAttr(termination_tick_offset),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case ?getAttr(current_tick_offset) of


		PastOffset when PastOffset >= TerminationOffset ->

			?notice( "Test Actor preparing termination." ),

			%trace_utils:debug_fmt( "Test Actor ~p preparing termination at #~p.~n",
			%		  [ self(), PastOffset ] ),

			% Following two calls could also have been grouped into an
			% overloading of the default declareTermination/2 implementation:
			%
			% (we request an immediate termination here, as we should not have
			% to wait for anyone)
			%
			TerminatingState = executeOneway( State, declareTermination,
											  _IntercalaryDiasca=0 ),

			output( "~w terminating at #~B.", [ self(), PastOffset ],
					TerminatingState ),

			TerminatingState;


		CurrentOffset ->

			output( "~w acting spontaneously at #~B on ~s",
					[ self(), CurrentOffset, net_utils:localnode() ], State ),

			Key = ?getAttr(data_key),

			V = class_Actor:read_data( Key, State ),

			% Checking:
			V = ?getAttr(expected_value),

			NewV = V + CurrentOffset rem (?getAttr(actor_abstract_id) + 2 ),

			class_Actor:modify_data( Key, NewV, mutable, State ),

			ExpectedState = setAttribute( State, expected_value,NewV ),

			executeOneway( ExpectedState, addSpontaneousTick, CurrentOffset+7 )


	end,

	wooper:return_state( NewState ).




% Section for actor oneways.


-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% We choose here not to do anything until the next tick:
	NewState = executeOneway( State, addSpontaneousTick,
							  ?getAttr(current_tick_offset) + 1 ),

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
			trace_utils:debug_fmt( "[~s (~w) at ~p] " ++ MessageFormat,
			   [ ?getAttr(name), self(), TickOffset ] ++ FormatValues );

		false ->
			ok

	end.
