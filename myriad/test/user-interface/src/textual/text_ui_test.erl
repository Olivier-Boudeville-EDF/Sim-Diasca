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


% Unit tests for the text_ui toolbox.
%
% See the text_ui.erl tested module.
%
-module(text_ui_test).


% For run/0 export and al:
-include("test_facilities.hrl").



% The actual test:
run_test_ui() ->

	test_facilities:display( "Testing the text_ui services.~n" ),

	text_ui:start(),

	text_ui:set( [ { backtitle, "Test of text_ui" }, { title, "A title" } ] ),

	text_ui:display( "My text to display!" ),

	text_ui:display_error( "My error to display!" ),


	text_ui:unset( [ backtitle, title ] ),

	text_ui:display_numbered_list( "My numbered list is:",
								   [ "Foo", "Bar", "Baz" ] ),

	FirstChoice = text_ui:choose_designated_item(
					[ { choice_1, "Choice 1" },
					  { choice_2, "Choice 2" },
					  { choice_3, "Choice 3" },
					  { choice_4, "Choice 4" } ] ),

	text_ui:display( "Choice has been ~p", [ FirstChoice ] ),

	text_ui:add_separation(),

	SecondChoice = text_ui:choose_numbered_item_with_default(
					 "This is my label; just choose below:",
					 [ "Choice 1", "Choice 2", "Choice 3", "Choice 4" ], 2 ),

	text_ui:display( "Second choice has been ~p", [ SecondChoice ] ),

	text_ui:trace( "My UI trace" ),

	text_ui:display( "Text UI state: ~ts", [ text_ui:to_string() ] ),

	text_ui:stop().



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
			  "(not running the text_ui test, being in batch mode)" );

		false ->
			run_test_ui()

	end,

	test_facilities:stop().
