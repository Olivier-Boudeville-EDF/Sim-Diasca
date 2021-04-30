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


% This module gathers some facilities for all core classes and for all
% test/simulation cases.
%
-module(core_utils).



-export([ get_case_arguments/0, get_title/0, wait_ready/0,
		  suspend_simulation_until_enter_pressed/1,
		  draw_item_from/2, draw_items_from/3 ]).


% For notify_debug_fmt and al:
-include_lib("traces/include/traces.hrl").

% Including for engine_arg_version_key:
-include("engine_common_defines.hrl").


% Returns a case-specific argument table, i.e. whose engine-specified arguments
% have been removed so that only the ones interesting the current case remain.
%
% Allows to remove all engine-specific options besides the Erlang ones.
%
-spec get_case_arguments() -> shell_utils:argument_table().
get_case_arguments() ->

	AllArgs = shell_utils:get_argument_table(),

	KeysToIgnore = [ ?myriad_verbatim_key, ?trace_batch_key,
					 ?engine_arg_root_key, ?engine_arg_version_key ],

	% Not using shell_utils:argument_table_to_string/1 to avoid the
	% interpretation of argument names:
	%
	%trace_utils:debug_fmt( "Got, regarding arguments, ~p, whereas keys to "
	%					   "ignore are ~p.", [ AllArgs, KeysToIgnore ] ),

	list_table:remove_entries( KeysToIgnore, AllArgs ).



% Returns a textual description of the version of Sim-Diasca being used.
-spec get_title() -> string().
get_title() ->

	case shell_utils:get_command_arguments_for_option(
		   ?engine_arg_version_key ) of

		undefined ->
			"Sim-Diasca distributed branch (unspecified version)";

		[ [ VersionString ] ] ->
			"Sim-Diasca distributed branch (v" ++ VersionString ++ ")";

		OtherArg ->
			throw( { invalid_engine_version, OtherArg } )

	end.



% Waits until an actor is ready, and acknowledges its notification.
-spec wait_ready() -> void().
wait_ready() ->

	receive

		{ actorMessage, [ _ATick, notifyReady, ActorPid ] } ->

			?notify_debug_fmt( "Actor ~w ready.", [ ActorPid ] ),

			% Acknowledges the actor message, otherwise the actor will be
			% frozen:
			%
			ActorPid ! { acknowledgeMessage, self() }

	end.



% Suspends the simulation until the Enter key is pressed.
-spec suspend_simulation_until_enter_pressed( time_manager_pid() ) -> void().
suspend_simulation_until_enter_pressed( TimeManagerPid ) ->

	case executable_utils:is_batch() of

		true ->
			nothing_done;

		false ->

			%trace_utils:debug("Requesting the simulation to be suspended."),
			TimeManagerPid ! suspend,

			io:get_line( "Simulation requested to be suspended, "
						 "press Enter to resume it." ),

			TimeManagerPid ! resume,

			io:format( "Simulation requested to be resumed.~n" )

	end.



% Draws one item from specified list using a random law and the specified random
% manager, and returns a pair made of the drawn item and of the resulting list,
% which is the specified one with the first instance of that drawn item removed:
% { DrawnItem, RemainingList }.
%
% Expects the specified list to be non-empty.
%
% Note: this function is mostly deprecated, as now stochastic values can
% generally (ex: for actors) be obtained without direct exchange with a random
% manager).
%
-spec draw_item_from( [ T ], random_manager_pid() ) -> { T, [ T ] }.
draw_item_from( DrawableList, RandomManagerPid ) when DrawableList =/= [] ->

	% getUniformValue returns a number in 1..N:
	RandomManagerPid ! { getUniformValue, length( DrawableList ), self() },

	DrawnPosition = receive

		{ wooper_result, { uniform_value, Value } } ->
			Value

	end,

	% No item should be drawn twice:
	DrawnItem = lists:nth( DrawnPosition, DrawableList ),

	{ DrawnItem, lists:delete( DrawnItem, DrawableList ) }.



% Draws ItemCount items from specified list using a random law and the specified
% random manager, and returns either the 'too_many_drawn_items' atom is the
% specified list is too short, or a pair made of the list of drawn items and of
% the resulting list, which is the specified one with the first instance of all
% drawn items removed: { DrawnItemList, RemainingList }.
%
% Note: this function is mostly deprecated, as now stochastic values can
% generally (ex: for actors) be obtained without direct exchange with a random
% manager).
%
-spec draw_items_from( [ T ], basic_utils:count(), random_manager_pid() ) ->
							 'too_many_drawn_items' | { [ T ], [ T ] }.
draw_items_from( DrawableList, ItemCount, RandomManagerPid ) ->
	draw_items_from( DrawableList, ItemCount, RandomManagerPid, _Acc=[] ).


draw_items_from( DrawableList, 0, _RandomManagerPid, Acc ) ->
	{ Acc, DrawableList };

draw_items_from( _DrawableList=[], _ItemCount, _RandomManagerPid, _Acc ) ->
	too_many_drawn_items;

draw_items_from( DrawableList, ItemCount, RandomManagerPid, Acc ) ->

	{ DrawnItem, RemainingList } =
		draw_item_from( DrawableList, RandomManagerPid ),

	draw_items_from( RemainingList, ItemCount - 1, RandomManagerPid,
					 [ DrawnItem | Acc ] ).
