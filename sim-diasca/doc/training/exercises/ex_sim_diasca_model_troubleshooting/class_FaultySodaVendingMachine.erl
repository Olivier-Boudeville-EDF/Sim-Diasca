% Copyright (C) 2008-2021 EDF R&D
%
% This file is part of the Sim-Diasca training material.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


-module(class_FaultySodaVendingMachine).


-define( class_description, "Class modeling a soda vending machine." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.SodaVendingMachine" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% Constructs a faulty soda-vending machine.
construct( State, ActorSettings, MachineName, InitialCanCount, CanCost )
  when InitialCanCount >= 0 ->

	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(MachineName) ),

	?send_notice_fmt( ActorState,
					"Creating a new soda vending machine named '~ts', "
					"having initially ~B can(s), costing each ~B euro(s).",
					[ MachineName, InitialCanCount, CanCost ] ),

	StockProbeRef = class_Probe:new(
		text_utils:format( "~ts Soda Stock Probe", [ MachineName ] ),
		{ text_utils:format( "~ts can stock", [ MachineName ] ) },
		"Monitoring the soda consumption",
		"Simulation tick",
		"Number of cans still available in the machine" ),

	setAttributes( ActorState, [ { can_count, InitialCanCount },
								 { can_cost, CanCost },
								 { probe_ref, StockProbeRef } ] ).



% Overridden destructor.
-spec delete( wooper:state() ) -> wooper:state().
delete( State ) ->

	% Class-specific actions:
	?notice_fmt( "Deleting soda vending machine named '~ts', "
			   "whose final can stock was ~B.",
			   [ ?getAttr(name), ?getAttr(can_count) ] ),

	% Then allow chaining:
	State.




% Methods section.


% Management section of the actor.


% The core of the soda vending machine behaviour.
-spec actSpontaneous( wooper:state() ) -> const_oneway_return().
actSpontaneous( State ) ->

	% Here a machine as no spontaneous behaviour, so it does not do anything
	% special, except collecting some data:

	CurrentTick = class_Actor:get_current_tick( State ),

	?getAttr(probe_ref) ! { setData, [ CurrentTick, { ?getAttr(can_count) } ] },

	wooper:const_return().



% Called by a customer wanting to know the cost of a can for this machine.
-spec getCanCost( wooper:state(), sending_actor_pid() ) ->
						const_actor_oneway_return().
getCanCost( State, CustomerPid ) ->

	?notice_fmt( "Telling to customer ~w the cost of a can.", [ CustomerPid ] ),

	CustomerPid ! { setCanCost, ?getAttr(can_cost) },

	actor:const_return().



% Called by a customer wanting to purchase a can.
-spec orderSoda( wooper:state(), amount(), sending_actor_pid() ) ->
					   actor_oneway_return().
orderSoda( State, CustomerBudget, CustomerPid ) ->

	NewState = case ?getAttr(can_count) of

		CanCount ->

			% We have a can, so where is the cash?
			case ?getAttr(can_cost) of

				CanCost when CanCost > CustomerBudget ->
					?notice( "Order failed, as customer is not rich enough." ),
					class_Actor:send_actor_message( CustomerPid,
													onNotEnoughMoney, State );

				_ ->
					SentState = class_Actor:send_actor_message( CustomerPid,
																getCan, State ),
					setAttribute( SentState, can_count, CanCount-1 )

			end,

		0 ->
			info( "Order failed, as no soda can left." ),
			class_Actor:send_actor_message( CustomerPid,
											onNoCanAvailable, State )

	end,

	wooper:return_state_result( NewState, CustomerBudget ).



% Returns the probe in use by this machine.
%
% Useful for the calling test, so that it can control by itself the probe, as
% depending on whether it is run in batch mode or not, probe displaying is
% wanted or not.
%
-spec getProbe( wooper:state() ) -> const_request_return( probe_ref() ).
getProbe( State ) ->
	const_return_result( ?getAttr(probe_ref) ).
