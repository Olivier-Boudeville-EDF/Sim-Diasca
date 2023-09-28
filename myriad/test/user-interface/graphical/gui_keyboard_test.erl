% Copyright (C) 2022-2023 Olivier Boudeville
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
% Creation date: Sunday, March 6, 2022.


% @doc Testing of the <b>MyriadGUI keyboard support</b>.
%
% See the gui_keyboard.erl tested module.
%
-module(gui_keyboard_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-type my_test_state() :: { gui:frame(), CheckScanCode :: boolean() }.
% Here the main loop just has to remember the frame whose closing is awaited
% for and whether we are checking scan codes.


% Shorthands:

-type event_context() :: gui:event_context().


% @doc Actual execution of the test.
-spec run_test_gui() -> void().
run_test_gui() ->

	test_facilities:display(
		"~nStarting the actual keyboard test of MyriadGUI, from ~w. ",
		[ self() ] ),

	trace_utils:notice( "An empty, resizable test frame shall appear; pressing "
		"keys while this frame has the focus should display the corresponding "
		"generated keyboard events." ),

	gui:start(),

	TestFrame = gui_frame:create( "This is the single and only test frame, "
								  "for keyboard testing" ),

	gui:subscribe_to_events( { onWindowClosed, TestFrame } ),


	% A frame cannot handle key events, so we create a panel within it:
	TestPanel = gui_panel:create( _Parent=TestFrame ),

	% Set to true to focus (only) on the scan codes:
	CheckScanCode = false,
	%CheckScanCode = true,

	EventTypes = [ onWindowClosed |
		case CheckScanCode of

			true ->
				[ onKeyPressed ];

			false ->
				[ onKeyPressed, onKeyReleased, onCharEntered ]

		end ],

	gui:subscribe_to_events( { EventTypes, TestPanel } ),

	% Focus needed to receive events:
	gui_widget:set_focus( TestPanel ),

	trace_utils:notice( "Please close the frame to end this test." ),

	gui_frame:show( TestFrame ),

	test_main_loop( { TestFrame, CheckScanCode } ).



% @doc A very simple main loop, whose actual state is simply the GUI object
% corresponding to the frame that shall be closed to stop the test.
%
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( TestState={ TestFrame, CheckScanCode } ) ->

	receive

		{ onCharEntered, [ _TestFrame, _TestFrameId, Context ] } ->
			interpret_event_context( Context, CheckScanCode ),
			test_main_loop( TestState );


		{ onKeyPressed, [ _TestFrame, _TestFrameId, Context ] } ->
			interpret_event_context( Context, CheckScanCode ),
			test_main_loop( TestState );


		{ onKeyReleased, [ _TestFrame, _TestFrameId, Context ] } ->
			interpret_event_context( Context, CheckScanCode ),
			test_main_loop( TestState );


		{ onWindowClosed, [ _TestFrame, _TestFrameId, Context ] } ->
			trace_utils:info_fmt( "Test frame '~ts' closed (~ts).",
				[ gui:object_to_string( TestFrame ),
				  gui_event:context_to_string( Context ) ] ),

			gui_frame:destruct( TestFrame ),

			trace_utils:info( "Test frame closed, test success." ),

			gui:stop();


		Other ->
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message: ~p.", [ Other ] ),
			test_main_loop( TestState )

	end.



% (helper)
-spec interpret_event_context( event_context(), boolean() ) -> void().
interpret_event_context( Context, _CheckScanCode=true ) ->

	WxKeyEvent = gui_keyboard:get_backend_event( Context ),

	MaybeUChar = gui_keyboard:get_maybe_uchar( WxKeyEvent ),
	Keycode = gui_keyboard:get_keycode( WxKeyEvent ),
	Scancode = gui_keyboard:get_scancode( WxKeyEvent ),

	%trace_utils:debug_fmt( "Maybe unicode char=~w, Keycode=~w, Scancode=~w.",
	%                       [ MaybeUChar, Keycode, Scancode ] ),

	case MaybeUChar of

		undefined ->
			trace_utils:info_fmt( "keycode ~B, scancode ~B",
								  [ Keycode, Scancode ] );

		UChar ->
			trace_utils:info_fmt(
				"Unicode character '~ts' of scancode ~B (keycode ~B).",
				[ [ UChar ], Scancode, Keycode ] )

	end;


interpret_event_context( Context, _CheckScanCode=false ) ->
	WxKeyEvent = gui_keyboard:get_backend_event( Context ),
	trace_utils:info_fmt( "Received ~ts.~n",
						  [ gui_keyboard:key_event_to_string( WxKeyEvent ) ] ).



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display( "(not running the MyriadGUI keyboard "
									 "test, being in batch mode)" );

		false ->
			run_test_gui()

	end,

	test_facilities:stop().
