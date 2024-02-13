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


% @doc Unit tests for the <b>core_utils</b> module.
%
% See core_utils.erl module.
%
-module(core_utils_test).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").


% For random_manager_name:
-include("class_RandomManager.hrl").



% @doc Runs the tests.
-spec run() -> no_return().
run() ->

	?case_start,

	class_InstanceTracker:create_mockup_environment(),

	?test_info( "Creating a random manager." ),
	class_RandomManager:create(),

	RandomManagerPid = naming_utils:wait_for_global_registration_of(
							?random_manager_name ),

	?test_info( "Testing the random drawing of items in lists." ),

	% Unordered and with a duplicate:
	DrawableList = [ 1, 2, 4, 3, 5, 7, 6, 13, 2 ],

	{ DrawnItem, FirstRemainingList } =
		core_utils:draw_item_from( DrawableList, RandomManagerPid ),

	?test_notice_fmt( "From list ~p, extracted ~B, remaining: ~p.",
					  [ DrawableList, DrawnItem, FirstRemainingList ] ),

	FirstItemCount = 4,

	{ FirstDrawnItemList, SecondRemainingList } = core_utils:draw_items_from(
		DrawableList, FirstItemCount, RandomManagerPid ),

	?test_notice_fmt( "From list ~p, extracted ~B items: ~p, remaining: ~p.",
		[ DrawableList, FirstItemCount, FirstDrawnItemList,
		  SecondRemainingList ] ),

	SecondItemCount = length( DrawableList ),

	{ SecondDrawnItemList, ThirdRemainingList } = core_utils:draw_items_from(
		DrawableList, SecondItemCount, RandomManagerPid ),

	?test_notice_fmt( "From list ~p, extracted all ~B items: ~p, "
		"remaining: ~p.",
		[ DrawableList, SecondItemCount, SecondDrawnItemList,
		  ThirdRemainingList ] ),

	ThirdItemCount = length( DrawableList ) + 1,

	too_many_drawn_items = core_utils:draw_items_from( DrawableList,
											ThirdItemCount, RandomManagerPid ),

	?test_info( "Extracting too many items from a list is correctly detected."),

	?test_info( "Removing random manager." ),
	wooper:delete_synchronously_instance( RandomManagerPid ),

	?case_stop.
