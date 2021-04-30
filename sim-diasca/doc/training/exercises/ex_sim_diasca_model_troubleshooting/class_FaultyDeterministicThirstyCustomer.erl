% Copyright (C) 2008-2021 EDF R&D
%
% This file is part of the Sim-Diasca training material.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


-module(class_FaultyDeterministicThirstyCustomer).


-define( class_description,
		 "Class modeling a deterministic thirsty customer." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.DeterministicThirstyCustomer" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Constructs a new faulty deterministic thirsty customer.
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_SodaVendingMachine:machine_pid(),
				 duration(), amount() ) -> wooper:state().
construct( State, ActorSettings, CustomerName, KnownMachine,
		   RepletionDuration, InitialBudget ) ->

	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize( CustomerName ) ),

	?send_notice_fmt( ActorState,
		"Creating a new deterministic thirsty customer named '~ts', "
		"having initially ~B euro(s), knowing the following vending machine: ~w"
		" and being thirsty ~B minutes after having drunk.",
		[ CustomerName, InitialBudget, KnownMachine, RepletionDuration ] ),

	setAttributes( ActorState, [
		{ known_machine, KnownMachine },
		{ can_cost, undefined },
		{ repletion_duration, RepletionDuration },
		{ next_thirsty_tick, undefined },
		{ current_money, InitialBudget },
		{ transaction_in_progress, false } ] ).



% Overridden destructor.
-spec delete( wooper:state() ) -> wooper:state().
delete( State ) ->

	% Class-specific actions:

	?notice_fmt( "Deleting thirsty customer named '~ts', who had finally ~B "
		"euros left in pocket.", [ ?getAttr(name), ?getAttr(current_money) ] ),

	% Then allow chaining:
	State.





% Methods section.


% Management section of the actor.


% The core of the customer behaviour.
-spec actSpontaneous( wooper:state() ) -> const_oneway_return().
actSpontaneous( State ) ->

	NewState = case ?getAttr(can_cost) of

		undefined ->
			request_cost( State );

		requested ->
			?notice( "Price quote from the machine being requested." ),
			State;

		_ ->
			manage_thirst( State )

	end,

	wooper:const_return().



% Called by the known machine, in return to a getCanCost call.
-spec setCanCost( wooper:state(), amount(), sending_actor_pid() ) ->
						actor_oneway_return().
setCanCost( State, CanCost, MachinePid ) ->

	% Sanity checks:
	MachinePid = ?getAttr(known_machine),
	requested = ?getAttr(can_cost),

	% Now, we enter the thirsty/not thirsty loop, starting as if not
	% thirsty at all:
	%
	ThirstyState = set_next_thirsty_tick( State , MachinePid ),

	actor:return_state( setAttribute( ThirstyState, can_cost, CanCost ) ).



% Called by the machine in return to a orderSoda call, when a can was available.
-spec getCan( wooper:state(), sending_actor_pid() ) -> actor_oneway_return().
getCan( State, _MachinePid ) ->

	?notice( "Received a can, drank it, no more thirsty for a while." ),

	MoneyState = subtractFromAttribute( State, current_money,
										?getAttr(can_cost) ),

	FinishState = setAttribute( MoneyState, transaction_in_progress, false ),

	actor:return_state( set_next_thirsty_tick( FinishState ) ).



% Called by the machine in return to a orderSoda call, when no can is available.
-spec onNoCanAvailable( wooper:state(), sending_actor_pid() ) ->
							actor_oneway_return().
onNoCanAvailable( State, _MachinePid ) ->

	?notice( "Could not have soda, the machine had no can left." ),

	actor:return_state( setAttribute( State, transaction_in_progress, false ) ).



% Called by the machine in return to a orderSoda call, when not enough money was
% inserted for a can.
%
-spec onNotEnoughMoney( wooper:state(), sending_actor_pid() ) ->
							  actor_oneway_return().
onNotEnoughMoney( State, MachinePid ) ->

	?error_fmt( "Still having ~B euros but unable to buy a can from ~w "
		"that should cost ~B euros, this soda vending machine is a crook.",
		[ ?getAttr(current_money), MachinePid, ?getAttr(can_cost) ] ) ] ),

	?wooper_return_state_only( setAttribute( State,
		transaction_in_progress, false ) ).



% Helper functions.


% Request the known machine to return the cost of one of its cans.
%
% Returns an updated state.
%
% (helper)
%
-spec request_cost( wooper:state() ) -> wooper:state().
request_cost( State ) ->

	?notice( "Investigating how much costs a soda, requesting the machine." ),

	class_Actor:send_actor_message( ?getAttr(known_machine),
		getCostOfCan, setAttribute( State, can_cost, requested ) ).




% Returns an updated state.
%
% (helper)
%
-spec request_cost( wooper:state() ) -> wooper:state().
manage_thirst( State ) ->

	case is_thirsty( State ) of

		true ->
			case ?getAttr(transaction_in_progress) of

				true ->
					% Do nothing until it is over, just wait:
					State;

				false ->
					% Here we need to drink, let's try to do so by ordering a
					% soda: (we specify our budget, but the machine has the
					% final word)
					%
					Budget = ?getAttr(current_money),
					CanCost = ?getAttr(can_cost),

					case CanCost of

						Cost when Cost > Budget ->
							?notice_fmt( "Thirsty, but not having enough money: "
								"a can costs ~B euros, "
								"whereas having only ~B euro(s).",
								[ CanCost, Budget ] ),
							State;

						_ ->

							% We should be able to afford the can:
							?notice_fmt( "Thirsty and having enough money "
								"(~B euros), trying to buy a can.", [ Budget ] ),

							class_Actor:send_actor_message(
								?getAttr(known_machine),
								{ orderSoda, Budget },
								setAttribute( State,
											  transaction_in_progress,true ) )

					end

			end;

		false ->
			?notice( "Feeling fine, not thirsty currently." ),
			State

	end.



% Returns whether this customer is thirsty.
%
% (helper)
%
-spec is_thirsty( wooper:state() ) -> wooper:state().
is_thirsty( State ) ->

	CurrentTick = class_Actor:get_current_tick( State ),

	case ?getAttr(next_thirsty_tick) of

		ThirstTick when CurrentTick >= ThirstTick ->
			true;

		_ ->
			false

	end.



% Computes the next thirsty tick and records it.
%
% Returns an updated state.
%
% (helper)
%
-spec set_next_thirsty_tick( wooper:state() ) -> wooper:state().
set_next_thirsty_tick( State ) ->

	% Relying on tick offsets is generally more convenient:
	NextThirstyTick = class_Actor:get_current_tick( State )
		+ ?getAttr(repletion_duration),

	setAttribute( State, next_thirsty_tick, NextThirstyTick ).
