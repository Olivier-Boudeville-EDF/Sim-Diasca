% Copyright (C) 2013-2024 Olivier Boudeville
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
% Creation date: 2013.


% <b>Simple unit tests for the MyriadGUI toolbox</b>: creates a few frames,
% enters a main loop, and exits when the fourth frame is closed by the user.
%
% Note: this test showcases also how an (explicit) GUI state can be kept and
% used, if ever needed.
%
% See the gui.erl tested module.
%
-module(gui_frame_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-type my_test_state() :: gui_frame:frame().
% Here the main loop just has to remember the frame whose closing is awaited
% for.



% @doc Executes the actual test.
-spec run_gui_test() -> void().
run_gui_test() ->

	test_facilities:display( "~nStarting the actual simple MyriadGUI test, "
							 "from ~w.", [ self() ] ),

	trace_utils:notice( "Please close the fourth frame to end this test "
		"(note that most frames may be one of top of the others)." ),

	% We used to choose here to carry around the GUI state, whereas in general
	% it is not necessary at all.

	gui:start(),

	%gui:set_debug_level( [ calls, life_cycle ] ),

	FirstFrame = gui_frame:create( "This is the first frame",
								   _Id=my_test_first_frame, _Parent=undefined ),

	SecondFrame = gui_frame:create( "This is the second frame" ),

	ThirdFrame = gui_frame:create( "This is the third frame",
		_Position={ 50, 10 }, _Size={ 150, 200 }, _Style=[ default ] ),

	FourthFrame = gui_frame:create( "This is the fourth frame" ),


	Frames = [ FirstFrame, SecondFrame, ThirdFrame, FourthFrame ],

	gui_frame:show( Frames ),

	% As a result, closing the third frame will not be known from here:
	TrackedFrames = [ FirstFrame, SecondFrame, FourthFrame ],

	gui:subscribe_to_events( { onWindowClosed, TrackedFrames } ),

	test_main_loop( FourthFrame ).




% @doc A very simple main loop, whose actual state is simply the GUI object
% corresponding to the frame that shall be closed to stop the test
% (i.e. CloseFrame).
%
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( CloseFrame ) ->

	trace_utils:info( "Test main loop running..." ),

	receive

		{ onWindowClosed, [ CloseFrame, _FrameId, Context ] } ->
			trace_utils:info_fmt( "The closing frame ~ts has been, well, "
				"closed (~ts); test success.",
				[ gui:object_to_string( CloseFrame ),
				  gui_event:context_to_string( Context ) ] ),

			gui_frame:destruct( CloseFrame ),

			gui:stop();


		{ onWindowClosed, [ AnyFrame, AnyFrameId, Context ] } ->
			trace_utils:info_fmt( "Frame ~ts (id: ~w) closed (~ts).",
				[ gui:object_to_string( AnyFrame ), AnyFrameId,
				  gui_event:context_to_string( Context ) ] ),

			gui_frame:destruct( AnyFrame ),
			test_main_loop( CloseFrame );


		Other ->
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message: ~p.", [ Other ] ),
			test_main_loop( CloseFrame )

	end.



% @doc Runs the test.
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


