% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Wednesday, July 17, 2024.

-module(gui_shell_test).

-moduledoc """
Unit tests for the management of the **shell components**, providing access
graphically to an Erlang interpreter.
""".


% For run/0 export and al:
-include("test_facilities.hrl").


-doc """
Here the main loop just has to remember the frame whose closing is awaited for.
""".
-type my_test_state() :: shell().



% Type shorthands:

-type shell() :: gui_shell:shell().



-doc "Executes the actual test.".
-spec run_gui_test() -> void().
run_gui_test() ->

	test_facilities:display( "~nStarting the GUI shell test." ),

	gui:start(),

	Frame = gui_frame:create( "This is the overall frame for GUI shell testing",
							  _Size={ 1024, 768 } ),

	%ShellOpts = [],
	ShellOpts = [ timestamp, log, focused, auto_parenthesis,
				  auto_add_trailing_dot,
				  { histories, _MaxCmdDepth=15, _MaxResDepth=20 }
				  %persistent_command_history
				  %{ callback_module, foo }
				],

	gui_shell:create( _FontSize=10, ShellOpts, _ParentWin=Frame ),

	gui:subscribe_to_events( { onWindowClosed, Frame } ),

	gui_frame:show( Frame ),

	test_main_loop( _InitialState=Frame ).



-doc """
A very simple main loop, whose actual state is simply the GUI object
corresponding to the frame that shall be closed to stop the test
(i.e. CloseFrame).
""".
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( State=Frame ) ->

	trace_utils:info( "Test main loop running..." ),

	receive

		{ onWindowClosed, [ Frame, _FrameId, _EventContext ] } ->
			trace_utils:info( "Main frame has been closed; test success." ),
			gui_frame:destruct( Frame ),
			gui:stop();

		Other ->
			trace_utils:warning_fmt( "Test main loop ignored the following "
									 "message:~n ~p.", [ Other ] ),
			test_main_loop( State )

	end.



-doc "Runs the test.".
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the MyriadGUI test, being in batch mode)" );

		false ->
			run_gui_test()

	end,

	test_facilities:stop().
