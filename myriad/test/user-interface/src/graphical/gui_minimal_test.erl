% Copyright (C) 2003-2021 Olivier Boudeville
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


% Minimal test for the MyriadGUI toolbox: draws a few frames and exits.
%
% See the gui.erl tested module.
%
-module(gui_minimal_test).


% For run/0 export and al:
-include("test_facilities.hrl").



run_test_gui() ->

	test_facilities:display(
	  "~nStarting the actual minimal test of MyriadGUI, from ~w.", [ self() ] ),

	gui:start(),


	TestFrame = gui:create_frame( "This is the single and only frame" ),


	EventOfInterest = { onWindowClosed, TestFrame },

	gui:subscribe_to_events( EventOfInterest ),


	trace_utils:notice( "Please close the frame to end this test." ),

	gui:show( TestFrame ),


	% Not even a real main loop here, just a one-shot event waited:
	receive

		{ onWindowClosed, [ TestFrame, Context ] } ->

			trace_utils:info_fmt( "Frame '~ts' closed (~ts).",
				[ gui:object_to_string( TestFrame ),
				  gui:context_to_string( Context ) ] ),

			% A frame is a window:
			gui:destruct_window( TestFrame ),

			trace_utils:info( "Test frame closed, test success." ),

			gui:stop()

	end.



% Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
			  "(not running the MyriadGUI test, being in batch mode)" );

		false ->
			run_test_gui()

	end,

	test_facilities:stop().
