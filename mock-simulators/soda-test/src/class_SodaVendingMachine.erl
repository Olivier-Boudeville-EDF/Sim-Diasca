% Copyright (C) 2008-2024 EDF R&D
%
% This file is part of Sim-Diasca.
%
% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: 2008.


% @doc Class modelling a <b>soda vending machine</b>.
-module(class_SodaVendingMachine).


-define( class_description, "Class modelling a soda vending machine." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


% For common types defined in this Soda-Test example:
-include("soda_test_types.hrl").


-type machine_pid() :: actor_pid().


-export_type([ machine_pid/0 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Soda-test.SodaVendingMachine" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% The class-specific attributes of a soda vending machine are:
-define( class_attributes, [

	{ can_count, can_count(),
	  "the (positive integer) number of cans this machine currently holds" },

	{ can_cost, amount(), "the cost of a can from this machine (as a "
	  "floating-point number of euros)" },

	{ probe_ref, class_Probe:probe_ref(),
	  "the PID of the probe (if any) declared by this vending machine" } ] ).




% @doc Creates a soda vending machine.
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), can_count(), amount() ) -> wooper:state().
construct( State, ActorSettings, MachineName, InitialCanCount, CanCost )
									when InitialCanCount >= 0 ->

	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(MachineName) ),

	?send_info_fmt( ActorState,
		"Creating a soda vending machine named '~ts', "
		"having initially ~B can(s), costing each ~.2f euro(s).",
		[ MachineName, InitialCanCount, CanCost ] ),

	ExtraSettingsTable = table:new( [ { global_plot_style, boxes } ] ),

	% Depending on the choice of the result manager, this reference will be
	% either a PID (if the corresponding result is wanted) or a
	% 'non_wanted_probe' atom:
	%
	StockProbeRef = class_Actor:declare_probe(
		_Name=text_utils:format( "~ts Soda Stock Probe", [ MachineName ] ),
		_Curves=[ text_utils:format( "~ts can stock", [ MachineName ] ) ],
		_Zones=[],
		_Title="Monitoring the soda consumption",
		_XLabel=undefined,
		_YLabel="Number of cans still available in the machine",
		ExtraSettingsTable, ActorState ),

	setAttributes( ActorState, [
		{ can_count, InitialCanCount },
		{ can_cost, CanCost },
		{ probe_ref, StockProbeRef } ] ).



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?notice_fmt( "Deleting soda vending machine named '~ts', whose final can "
				 "stock was ~B.", [ ?getAttr(name), ?getAttr(can_count) ] ),

	% Any internal probe to be deleted by the result manager.

	% Then allow chaining:
	State.




% Methods section.



% Management section of the actor.


% @doc We want to synchronise here our probes starting from the very first
% diasca of the simulation.
%
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							const_actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	SimulationInitialTick = ?getAttr(initial_tick),

	% Checking:
	true = ( SimulationInitialTick =/= undefined ),

	% Pseudo-const method:
	actor:const_return().



% A soda vending machine behaviour has not even its actSpontaneous/1 defined, as
% it is by design a purely passive actor.



% @doc Called by a customer wanting to know the cost of a can for this machine.
-spec getCanCost( wooper:state(), sending_actor_pid() ) ->
												actor_oneway_return().
getCanCost( State, CustomerPid ) ->

	?notice_fmt( "Telling to customer ~w the cost of a can.", [ CustomerPid ] ),

	SentState = class_Actor:send_actor_message( CustomerPid,
		{ setCanCost, ?getAttr(can_cost) }, State ),

	actor:return_state( SentState ).



% @doc Called by a customer wanting to purchase a can.
-spec orderSoda( wooper:state(), amount(), sending_actor_pid() ) ->
												actor_oneway_return().
orderSoda( State, CustomerBudget, CustomerPid ) ->

	% To test simulation stalls due to actors (here, thirsty customers) blocking
	% the simulation not because they are busy, but because they are blocked by
	% others (this machine):

	%trace_utils:debug_fmt( "Soda vending machine ~w sleeping...", [ self() ] ),
	%timer:sleep( _Milliseconds=11*1000 ),
	%trace_utils:debug_fmt( "Soda vending machine ~w slept.", [ self() ] ),

	NewState = case ?getAttr(can_count) of

		0 ->
			?notice( "Order failed, as no soda can left." ),
			class_Actor:send_actor_message( CustomerPid,
											onNoCanAvailable, State );

		CanCount ->

			% We have a can, so where is the cash?
			case ?getAttr(can_cost) of

				% Note that, like customers are currently modelled, this should
				% never happen as they request the price of a can before
				% ordering it, and thus never try to purchase a can with less
				% than its cost:
				%
				CanCost when CanCost > CustomerBudget ->

					?notice( "Order failed, as customer is not rich enough." ),

					class_Actor:send_actor_message( CustomerPid,
													onNotEnoughMoney, State );

				_ ->
					SentState = class_Actor:send_actor_message( CustomerPid,
																getCan, State ),

					setAttribute( SentState, can_count, CanCount-1 )

			end

	end,

	% Manages automatically the fact that the creation of this probe may have
	% been rejected by the result manager:
	%
	class_Probe:send_data( ?getAttr(probe_ref),
		class_Actor:get_current_tick( State ),
		{ getAttribute( NewState, can_count ) } ),

	actor:return_state( NewState ).
