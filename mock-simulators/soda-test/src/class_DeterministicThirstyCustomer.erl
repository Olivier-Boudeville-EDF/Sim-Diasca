% Copyright (C) 2008-2024 EDF R&D

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

% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]


% @doc Class modelling a <b>deterministic thirsty customer</b>.
-module(class_DeterministicThirstyCustomer).


-define( class_description,
		 "Class modelling a deterministic thirsty customer." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


% Design notes:
%
% A generic ThirstyCustomer class could have been introduced to factor at least
% a part of the state and behaviour between deterministic and stochastic
% clients.


-type customer_pid() :: actor_pid().

-export_type([ customer_pid/0 ]).


% Shorthands:

-type machine_pid() :: class_SodaVendingMachine:machine_pid().



% The class-specific attributes of a deterministic thirsty customer are:
-define( class_attributes, [

	{ known_machine_pid, machine_pid(),
	  "the PID of the soda-vending machine that this customer knows" },

	{ can_cost, maybe( amount() ), "the cost of a can from this machine (as a "
	  "floating-point number of euros)" },

	{ repletion_duration, maybe( class_TimeManager:tick_offset() ),
	  "the duration before, once having drunk, this customer will be thirsty "
	  "again" },

	{ next_thirsty_tick, class_TimeManager:tick_offset(),
	  "the next tick offset at which this customer will be thirsty again" },

	{ current_money, amount(),
	  "the (floating-point) number of euros this customer has in pocket" },

	{ transaction_in_progress, boolean(),
	  "tells whether a transaction with its machine is in progress" } ] ).


% For common types defined in this Soda-Test example:
-include("soda_test_types.hrl").



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Soda-test.DeterministicThirstyCustomer" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% Implementation notes:
%
% As usual, tick offsets are preferred to absolute ticks.



% @doc Creates a deterministic thirsty customer.
%
% Parameters are:
% - ActorSettings corresponds to the engine settings for this actor
% - CustomerName is the name of this customer (as a plain string)
% - KnownMachinePid is the PID of the soda vending machine this customer may
% know
% - RepletionDuration is the duration, expressed in integer virtual minutes, of
% the period during which, once having drunk a can, this customer is not thirsty
% anymore
% - InitialBudget is the amount of money this actor has in his pocket initially
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), machine_pid(), duration(), amount() ) ->
						wooper:state().
construct( State, ActorSettings, CustomerName, KnownMachinePid,
		   RepletionDuration, InitialBudget ) ->

	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(CustomerName) ),

	?send_info_fmt( ActorState,
		"Creating a deterministic thirsty customer named '~ts', "
		"having initially ~.2f euro(s), knowing the following vending machine: "
		"~w and being thirsty ~B minutes after having drunk.",
		[ CustomerName, InitialBudget, KnownMachinePid, RepletionDuration ] ),

	% From minutes to ticks:
	TickRepletionDuration = class_Actor:convert_seconds_to_ticks(
									60*RepletionDuration, ActorState ),

	setAttributes( ActorState, [ { known_machine_pid, KnownMachinePid },
								 { can_cost, undefined },
								 { repletion_duration, TickRepletionDuration },
								 { next_thirsty_tick, undefined },
								 { current_money, InitialBudget },
								 { transaction_in_progress, false } ] ).



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	?notice_fmt( "Deleting deterministic thirsty customer named '~ts', "
		"who had finally ~.2f euros left in pocket.",
		[ ?getAttr(name), ?getAttr(current_money) ] ),

	% Then allow chaining:
	State.





% Methods section.


% Management section of the actor.


% @doc Schedules this just created actor at the next tick (diasca 0).
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?debug( "Just deciding at first diasca to schedule our next "
			"spontaneous tick." ),

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( ScheduledState ).



% @doc The core of the customer behaviour.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	NewState = case ?getAttr(can_cost) of

		undefined ->
			request_cost( State );

		% Could happen if the vending machine answered after a delay:
		requested ->
			?notice( "Price quote from the machine being requested." ),

			% We already know we will be scheduled next, to know then the cost
			% of a can; nothing to do here, just wait:
			%
			State;

		_ ->
			manage_thirst( State )

	end,

	wooper:return_state( NewState ).



% @doc Called by the known machine, in return to a getCanCost/2 call.
-spec setCanCost( wooper:state(), amount(), sending_actor_pid() ) ->
						actor_oneway_return().
setCanCost( State, CanCost, MachinePid ) ->

	?debug_fmt( "The vending machine answered that a can costs ~w euros.",
				[ CanCost ] ),

	% Sanity checks:
	MachinePid = ?getAttr(known_machine_pid),
	requested = ?getAttr(can_cost),

	% Now, we are able to enter the thirsty/not thirsty loop, starting as if not
	% thirsty at all:
	%
	ThirstyState = set_next_thirsty_tick( State ),

	actor:return_state( setAttribute( ThirstyState, can_cost, CanCost ) ).



% @doc Called by the machine in return to a orderSoda/3 call, when a can was
% available.
%
-spec getCan( wooper:state(), sending_actor_pid() ) -> actor_oneway_return().
getCan( State, _SendingMachinePid ) ->

	% To test the proper simulation teardown should an actor fail:
	%basic_utils:crash(),

	% To test how a method taking a longer duration is reported:
	%timer:sleep( 5 * 60 * 1000 ),
	%basic_utils:freeze(),

	?notice( "Received a can, drank it, no more thirsty for a while." ),

	MoneyState = subtractFromAttribute( State, current_money,
										?getAttr(can_cost) ),

	FinishState = setAttribute( MoneyState, transaction_in_progress, false ),

	% Plan the next thirsty moment:
	actor:return_state( set_next_thirsty_tick( FinishState ) ).



% @doc Called whenever a can was requested to a machine, whereas none is
% available.
%
-spec onNoCanAvailable( wooper:state(), sending_actor_pid() ) ->
								actor_oneway_return().
onNoCanAvailable( State, _SendingMachinePid ) ->

	?notice( "Could not have soda, the machine had no can left." ),

	% Will retry next:
	actor:return_state( setAttribute( State, transaction_in_progress, false ) ).



% @doc Called whenever the customer requested a new can but actually cannot
% afford it (this should never happen).
%
-spec onNotEnoughMoney( wooper:state(), sending_actor_pid() ) ->
								actor_oneway_return().
onNotEnoughMoney( State, MachinePid ) ->

	?error_fmt( "Still having ~.2f euros but unable to buy a can from ~w that "
		"should cost ~.2f euros, this soda vending machine is a crook.",
		[ ?getAttr(current_money), MachinePid, ?getAttr(can_cost) ] ),

	actor:return_state( setAttribute( State, transaction_in_progress, false ) ).




% Helper functions.


% @doc Requests the known machine to return the cost of one of its cans.
%
% Triggers back a setCanCost/3 call.
%
% Returns an updated state.
%
% (helper)
%
-spec request_cost( wooper:state() ) -> wooper:state().
request_cost( State ) ->

	?notice( "Investigating how much a soda costs; requesting the machine." ),

	% Expect our setCanCost/3 oneway to be called back by the vending machine:
	class_Actor:send_actor_message( ?getAttr(known_machine_pid), getCanCost,
						setAttribute( State, can_cost, requested ) ).



% @doc Determines the behaviour of the customer with regard to his thirst.
%
% Returns an updated state.
%
% (helper)
%
-spec manage_thirst( wooper:state() ) -> wooper:state().
manage_thirst( State ) ->

	case is_thirsty( State ) of

		true ->
			case ?getAttr(transaction_in_progress) of

				true ->
					% Do nothing until it is over, just wait:
					?debug( "Cost transaction in progress, waiting..."),
					State;

				false ->
					% Here we need to drink, let's try to do so by ordering a
					% soda:
					%
					% (we specify our budget, but the machine has the final
					% word)
					%
					Budget = ?getAttr(current_money),
					CanCost = ?getAttr(can_cost),

					case CanCost of

						Cost when Cost > Budget ->

							?notice_fmt( "Thirsty, but not having enough money:"
								" a can costs ~.2f euros, whereas having only "
								"~.2f euro(s).", [ CanCost, Budget ] ),

							% No scheduling planned here, implies being passive
							% from now on:
							%
							State;


						_ ->
							% We should be able to afford the can; instead of
							% disclosing its budget to the machine, a customer
							% could just order a can based on its (known by
							% design) exact cost:

							?notice_fmt(
								"Thirsty and having enough money (~.2f euros), "
								"trying to buy a can.", [ Budget ] ),

							class_Actor:send_actor_message(
								?getAttr(known_machine_pid),
								{ orderSoda, Budget },
								setAttribute( State, transaction_in_progress,
											  true ) )

					end

			end;

		false ->

			%?notice( "Feeling fine, not thirsty currently." ),

			% We could jump directly to the moment this customer will be thirsty
			% again:
			%
			executeOneway( State, scheduleNextSpontaneousTick )

	end.



% @doc Returns whether this customer is thirsty.
%
% (helper)
%
-spec is_thirsty( wooper:state() ) -> boolean().
is_thirsty( State ) ->

	CurrentTick = class_Actor:get_current_tick_offset( State ),

	case ?getAttr(next_thirsty_tick) of

		ThirstTick when CurrentTick >= ThirstTick ->
			true;

		_ ->
			false

	end.



% @doc Computes the next thirsty tick and records it.
%
% Returns an updated state.
%
% (helper)
%
-spec set_next_thirsty_tick( wooper:state() ) -> wooper:state().
set_next_thirsty_tick( State ) ->

	DurationInTicks = ?getAttr(repletion_duration),

	NextThirstyTick = class_Actor:get_current_tick_offset( State )
		+ DurationInTicks,

	ThirstyState = setAttribute( State, next_thirsty_tick, NextThirstyTick ),

	?info_fmt( "Determined that will be thirsty again in ~B ticks.",
			   [ DurationInTicks ] ),

	executeOneway( ThirstyState, scheduleNextSpontaneousTick ).
