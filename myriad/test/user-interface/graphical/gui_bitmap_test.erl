% Copyright (C) 2023-2023 Olivier Boudeville
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
% Creation date: Saturday, August 19, 2023.


% @doc Testing the <b>support for the management of (mostly built-in)
% bitmaps</b>.
%
-module(gui_bitmap_test).


% Implementation notes:
%
% We favor mostly the PNG and JPEG formats.
%
% Here, rather than using our canvas, we directly paint of the panel defined
% within the main frame.


% For run/0 export and al:
-include("test_facilities.hrl").




% Shorthands:

-type frame() :: gui_frame:frame().



% @doc Runs the actual test.
-spec run_bitmap_test() -> void().
run_bitmap_test() ->

	test_facilities:display( "Starting the bitmap test, "
		"simply by displaying the built-in bitmaps in a resizable window." ),

	trace_utils:notice( "A resizable frame displaying the bitmaps shall "
		"appear. The test will end as soon as this frame is closed." ),

	gui:start(),

	MainFrame = gui_frame:create( _Title="MyriadGUI Bitmap Test",
								  gui_overall_test:get_main_window_size() ),

	% No need to add _Opts=[{style, full_repaint_on_resize}]:
	Panel = gui_panel:create( MainFrame ),

	ColumnCount = 8,

	% Will auto-adjust the number of rows:
	GridSizer = gui_sizer:create_grid( _RowCount=0, ColumnCount,
									   _HorizGap=3, _VertGap=3 ),

	{ bitmap_id, BitmapEntries, _ElemLookup } =
		gui_constants:get_bitmap_id_topic_spec(),

	AllBitmapIds = pair:firsts( BitmapEntries ),

	BitmapIdss = list_utils:group_by( ColumnCount, AllBitmapIds ),

	BitmapStrs = [ text_utils:atoms_to_listed_string( Ids )
						|| Ids <- BitmapIdss ],

	trace_utils:debug_fmt(
		"All ~B standard bitmap identifiers, listed per row: ~ts",
		[ length( AllBitmapIds ),
		  text_utils:strings_to_enumerated_string( BitmapStrs ) ] ),

	BitmapParent = Panel,

	Dims = { 32, 32 },

	AllBitmaps = [ gui_bitmap:get_standard( BId, Dims )
					|| BId <- AllBitmapIds ],

	AllBitmapDisplays = [ gui_bitmap:create_static_display( B, BitmapParent )
								|| B <- AllBitmaps ],

	gui_sizer:add_elements( GridSizer, AllBitmapDisplays, _BitmapFlags=[] ),

	gui_widget:set_sizer( Panel, GridSizer ),

	% No need to subscribe to 'onRepaintNeeded' for the panel:
	gui:subscribe_to_events( [ { onWindowClosed, MainFrame } ] ),

	% Renders the GUI:
	gui_frame:show( MainFrame ),

	test_main_loop( MainFrame ),

	gui:stop().



% The main loop of this test.
-spec test_main_loop( frame() ) -> void().
test_main_loop( MainFrame ) ->

	receive

		{ onWindowClosed, [ MainFrame, _MainFrameId, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt( "Test main frame ~ts has been closed "
					"(~ts), test success.",
					[ gui:object_to_string( MainFrame ),
					  gui_event:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( Context ) ),

			gui_frame:destruct( MainFrame );


		Other ->
			% Extra newline for better separation:
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message:~n ~p.~n", [ Other ] ),
			test_main_loop( MainFrame )

	end.



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the bitmap test, being in batch mode)" );

		false ->
			run_bitmap_test()

	end,

	test_facilities:stop().
