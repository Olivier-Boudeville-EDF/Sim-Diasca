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
% Creation date: Sunday, December 8, 2024.

-module(gui_select_box_test).

-moduledoc """
Unit tests for the management of **selection boxes**.
""".


% For run/0 export and al:
-include("test_facilities.hrl").


-doc """
Here the main loop just has to remember the frame whose closing is awaited for,
and the test selection box.
""".
-type my_test_state() :: { frame(), select_box() }.



% Type shorthands:

-type frame() :: gui_frame:frame().
-type select_box() :: gui_select_box:select_box().



-doc "Executes the actual test.".
-spec run_gui_test() -> void().
run_gui_test() ->

	test_facilities:display( "~nStarting the selection box test." ),

	gui:start(),


	Frame = gui_frame:create(
		"This is the overall frame for selection box testing",
		_Size={ 1280, 1024 } ),

	Panel = gui_panel:create( Frame ),

	Items = [ "First item", "Second item", "Third item" ],

	SelBox = gui_select_box:create( Items, _Parent=Panel ),
	% No specific need to call gui:layout/1 or gui:{refresh,update}/1.

	gui:subscribe_to_events( [ { onWindowClosed, Frame } ] ),
	%						   { onButtonToggled, ToggleButton } ),

	gui_frame:show( Frame ),

	test_main_loop( _InitialState={ Frame, SelBox } ).



-doc """
A very simple main loop, whose actual state is simply the GUI object
corresponding to the frame that shall be closed to stop the test
(i.e. CloseFrame).
""".
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( State={ Frame, _SelBox } ) ->

	trace_utils:info( "Test main loop running..." ),

	receive

		{ onWindowClosed, [ Frame, _FrameId, _EventContext ] } ->
			trace_utils:info( "Main frame has been closed." ),
			stop( State );

		Other ->
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message: ~p.", [ Other ] ),
			test_main_loop( State )

	end.


stop( _State={ Frame, SelBox } ) ->
	trace_utils:info( "Test success, destructing widgets." ),

	gui_select_box:destruct( SelBox ),

	gui_frame:destruct( Frame ),

	trace_utils:info( "Stopping." ),
	gui:stop().



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
