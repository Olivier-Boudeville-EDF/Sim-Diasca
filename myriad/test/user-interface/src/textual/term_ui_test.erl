% Copyright (C) 2018-2021 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Unit tests for the term_ui toolbox.
%
% See the term_ui.erl tested module.
%
-module(term_ui_test).


% For run/0 export and al:
-include("test_facilities.hrl").



% The actual test:
run_test_ui() ->

	% All calls use an implicit state.

	test_facilities:display( "Testing the term_ui services." ),

	term_ui:start(),

	term_ui:set( [ { backtitle, "Test of term_ui" }, { title, "A title" } ] ),

	term_ui:display( "My text to display! (featuring 'single quotes' and "
					 "also \"double quotes\"!)" ),

	term_ui:display( "My second text to display!" ),

	case term_ui:choose_designated_item_with_default(
		   "Please choose a color among:",
		   [ { red, "Reddish" }, { blue, "Blueish" }, { green, "Greenish" } ],
		   blue ) of

		red ->
			term_ui:display( "Red was chosen!" );

		blue ->
			term_ui:display( "Blue was chosen!" );

		green ->
			term_ui:display( "Green was chosen!" );

		ui_cancel ->
			term_ui:display( "Choice cancelled by the user!" )

	end,

	case term_ui:choose_numbered_item_with_default(
		   "Please choose a race among:",
		   [ "Elves", "Orcs", "Humans" ], "Humans" ) of

		1 ->
			term_ui:display( "Elves were chosen!" );

		2 ->
			term_ui:display( "Orcs were chosen!" );

		3 ->
			term_ui:display( "Humans were chosen!" );

		ui_cancel ->
			term_ui:display( "Choice cancelled by the user!" )

	end,

	trace_utils:debug_fmt( "UI state: ~ts", [ term_ui:to_string() ] ),

	term_ui:stop().



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the term_ui test, being in batch mode)" );

		false ->
			run_test_ui()

	end,

	test_facilities:stop().
