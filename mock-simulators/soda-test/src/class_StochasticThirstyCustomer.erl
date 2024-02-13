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
% Creation date: 2008.


% @doc Class modelling a <b>stochastic thirsty customer</b>.
-module(class_StochasticThirstyCustomer).


-define( class_description, "Class modelling a stochastic thirsty customer." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_StochasticActor ] ).


% Design notes:
%
% A generic ThirstyCustomer class could have been introduced to factor at least
% a part of the state and behaviour between deterministic and stochastic
% clients.


-type customer_pid() :: actor_pid().

-export_type([ customer_pid/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type random_law() :: random_utils:random_law().

-type machine_pid() :: class_SodaVendingMachine:machine_pid().




% The class-specific attributes of a stochastic thirsty customer are:
-define( class_attributes, [

	{ known_machine_pid, machine_pid(),
	  "the PID of the soda-vending machine that this customer knows" },

	{ can_cost, maybe( amount() ), "the cost of a can from this machine (as a "
	  "floating-point number of euros)" },

	{ repletion_law, random_law(),
	  "the random law describing the duration, in minutes, before a customer "
	  "becomes thirsty again, once he has just drunk a can" },

	{ next_thirsty_tick, class_TimeManager:tick_offset(),
	  "the next tick offset at which this customer will be thirsty again" },

	{ current_money, amount(),
	  "the (floating-point) number of euros this customer has in pocket" },

	{ transaction_in_progress, boolean(),
	  "records whether a transaction with its machine is in progress" } ] ).


% For common types defined in this Soda-Test example:
-include("soda_test_types.hrl").



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Soda-test.StochasticThirstyCustomer" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% Implementation notes:
%
% As usual, tick offsets are preferred to absolute ticks.



% @doc Creates a stochastic thirsty customer.
%
% Parameters are:
% - ActorSettings corresponds to the engine settings for this actor
% - CustomerName is the name of this customer (as a plain string)
% - KnownMachinePid is the PID of the soda vending machine this customer may
% know
% - RepletionDurationLaw is a random law (expressed in minutes), like for
% example: '{gaussian, 10, 2}'
% - InitialBudget is the amount of money this actor has in his pocket initially
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		class_Actor:name(), machine_pid(), random_law(), amount() ) ->
			wooper:state().
construct( State, ActorSettings, CustomerName, KnownMachinePid,
		   RepletionDurationLaw, InitialBudget ) ->

	StochasticState = class_StochasticActor:construct( State,
		ActorSettings, ?trace_categorize(CustomerName),
		[ { repletion_law, RepletionDurationLaw } ] ),

	?send_info_fmt( StochasticState,
		"Creating a stochastic thirsty customer named '~ts', having "
		"initially ~.2f euro(s), knowing the following vending machine: ~w "
		"and being thirsty, once having drunk, after a duration respecting "
		"the following random law (in minutes): ~w.",
		[ CustomerName, InitialBudget, KnownMachinePid,
		  RepletionDurationLaw ] ),

	setAttributes( StochasticState, [ { known_machine_pid, KnownMachinePid },
									  { can_cost, undefined },
									  { next_thirsty_tick, undefined },
									  { current_money, InitialBudget },
									  { transaction_in_progress, false } ] ).



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	?notice_fmt( "Deleting stochastic thirsty customer named '~ts', "
		"who had finally ~.2f euros left in pocket.",
		[ ?getAttr(name), ?getAttr(current_money) ] ),

	% Then allow chaining:
	State.





% Methods section.


% Management section of the actor.


% @doc Simply schedules this just created actor at the next tick (diasca 0).
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
								actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( ScheduledState ).



% @doc The core of the customer behaviour.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	% To test simulation stalls due to busy actors:

	%MillisecondDuration = 6*1000,
	%trace_utils:info_fmt( "Stochastic thirsty customer ~w sleeping for ~ts...",
	%   [ self(), time_utils:duration_to_string( MillisecondDuration ) ] ),
	%timer:sleep( MillisecondDuration ),
	%trace_utils:info_fmt( "Stochastic thirsty customer ~w slept.",
	%                     [ self() ] ),

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

	?notice( "Received a can, drank it, no more thirsty for a while." ),

	MoneyState =
		subtractFromAttribute( State, current_money, ?getAttr(can_cost) ),

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



% Section for standard methods.
%
% Note: such methods are not actor oneways, and thus must not be called whereas
% the simulation is running. Only WOOPER requests (not mere oneways, for
% synchronicity) are allowed to be used in such context (typically from the
% simulation case, whereas the simulation is not started yet).


% @doc Requests this customer to return its name.
-spec sayName( wooper:state() ) -> const_request_return( ustring() ).
sayName( State ) ->

   Name = ?getAttr(name),

   %trace_utils:debug_fmt(
   %   "I am a stochastic thirsty customer named ~ts (PID: ~w).",
   %   [ Name, self() ] ),

   wooper:const_return_result( Name ).




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

	CurrentTick = class_Actor:get_current_tick_offset( State ),

	% Depending on the law settings, we could end up with strictly negative
	% (minute-based) durations; we ensure that they remain positive:
	%
	DurationInSeconds = 60 * erlang:abs(
		class_StochasticActor:get_random_value_from( repletion_law, State ) ),

	% Ensures we always are thirsty in the future (and relaxes a lot the maximum
	% relative error to avoid any test failure):
	%
	DurationInTicks = class_Actor:convert_seconds_to_non_null_ticks(
					DurationInSeconds, _MaxRelativeErrorForTest=0.50, State ),

	ThirstyState = setAttribute( State, next_thirsty_tick,
								 CurrentTick + DurationInTicks ),

	?info_fmt( "Determined that will be thirsty again in ~w seconds "
		"(i.e. ~ts), corresponding to ~B ticks.",
		[ DurationInSeconds,
		  time_utils:duration_to_string( 1000 * DurationInSeconds ),
		  DurationInTicks ] ),

	executeOneway( ThirstyState, scheduleNextSpontaneousTick ).
