% Copyright (C) 2016-2021 Olivier Boudeville
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


% Unit tests for the ui toolbox.
%
% See the ui.erl tested module.
%
-module(ui_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For an easier disabling:
-export([ test_basic_message_display/0, test_user_entry/0 ]).


% Test of the display of a basic display.
-spec test_basic_message_display() -> void().
test_basic_message_display() ->

	ui:set_settings( [ { title,
		"This is a title about the display of a basic message." },
					   { backtitle,
		"This is a back-title about the display of a basic message." } ] ),

	Message = "It has been a long time, dear master.",

	ui:display( "The UI service greets you.~n~ts", [ Message ] ),

	ui:unset_settings( [ title, backtitle ] ).



% Test of the facilities regarding user entry.
-spec test_user_entry() -> void().
test_user_entry() ->

	ui:set_settings( [ { title,
		"This is a title about the support of user input." },
					   { backtitle,
		"This is a back-title about the support of user input." } ] ),

	Lower = 10,
	Upper = 42,

	Prompt = text_utils:format( "Please enter an integer between ~B and ~B "
								"(bounds included): ", [ Lower, Upper ] ),

	case ui:read_text_as_maybe_integer( Prompt ) of

		undefined ->
			ui:display_error( "A non-empty value must be entered, "
							  "please retry." ),
			test_user_entry();

		V when V >= Lower andalso V =< Upper ->
			ui:display( "Congratulation, you entered a correct value: ~B",
						[ V ] );

		V ->
			ui:display_error( "Sorry, ~B is not between ~B and ~B.",
							  [ V, Lower, Upper ] ),
			test_user_entry()

	end.


% The actual test:
run_test_ui() ->

	test_facilities:display( "Testing the ui services." ),

	ui:start(),

	test_basic_message_display(),

	test_user_entry(),

	ui:stop().



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
			  "(not running the ui test, being in batch mode)" );

		false ->
			run_test_ui()

	end,

	test_facilities:stop().
