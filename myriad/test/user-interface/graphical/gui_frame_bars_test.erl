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
% Creation date: Wednesday, April 27, 2022.


% @doc Unit tests for the management of <b>toolbars and status bars</b> in
% frames.
%
-module(gui_frame_bars_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Shorthands:

-type frame() :: gui_frame:frame().
-type toolbar() :: gui_toolbar:toolbar().


-type my_test_state() :: { frame(), toolbar() }.
% Here the main loop just has to remember the created toolbar, and this frame
% whose closing is awaited for.




% @doc Executes the actual test.
-spec run_gui_test() -> void().
run_gui_test() ->

	test_facilities:display(
		"~nStarting the bar test: toolbar and status bar. "
		"Close the frame to stop." ),

	gui:start(),

	Frame = gui_frame:create( "This is the overall frame for bar testing" ),

	StatusBar = gui_statusbar:create( Frame, _Styles=[ sunken ] ),

	% Thus 1/3 for the first field, 2/3 for the second:
	gui_statusbar:set_field_widths( StatusBar, [ -1, -2 ] ),

	gui_statusbar:push_field_text( StatusBar, "First field", _FieldIndex=1 ),

	gui_statusbar:push_field_text( StatusBar, "Second, wider field",
								   _FIndex=2 ),

	ToolbarStyle = [ top, dockable, flat, text ],
	%ToolbarStyle = bottom,
	%ToolbarStyle = left,
	%ToolbarStyle = right,

	Toolbar = gui_toolbar:create( Frame, _ToobarId=undefined, ToolbarStyle ),

	NewBitmap = gui_bitmap:get_standard( new_bitmap ),
	gui_toolbar:add_tool( Toolbar, _Id=my_new_id, _Label="New", NewBitmap,
						  "New short help" ),

	QuestionBitmap = gui_bitmap:get_standard( question_bitmap ),
	gui_toolbar:add_tool( Toolbar, my_question_id, "Question", QuestionBitmap,
						  "Question short help" ),

	WarningBitmap = gui_bitmap:get_standard( warning_bitmap ),
	gui_toolbar:add_tool( Toolbar, my_warning_id, "Warning", WarningBitmap,
						  "Warning short help" ),

	InformationBitmap = gui_bitmap:get_standard( information_bitmap ),
	gui_toolbar:add_tool( Toolbar, my_information_id, "Information",
						  InformationBitmap, "Information short help" ),

	AddBookmarkBitmap = gui_bitmap:get_standard( add_bookmark_bitmap ),
	gui_toolbar:add_tool( Toolbar, my_add_bookmark_id, "Add Bookmark",
						  AddBookmarkBitmap, "Add bookmark short help" ),

	DelBookmarkBitmap = gui_bitmap:get_standard( delete_bookmark_bitmap ),
	gui_toolbar:add_tool( Toolbar, my_del_bookmark_id, "Delete Bookmark",
						  DelBookmarkBitmap, "Delete bookmark short help" ),


	% Some bitmaps may be unavailable (yielding a bitmap.IsOk() wxWidgets Assert
	% failure):
	%

	OtherNames = [ help_side_panel_bitmap, help_settings_bitmap,
		help_book_bitmap, help_folder_bitmap, help_page_bitmap, go_back_bitmap,
		go_forward_bitmap, go_up_bitmap, go_down_bitmap, go_to_parent_bitmap,
		go_home_bitmap, goto_first_bitmap, goto_last_bitmap, print_bitmap,
		help_bitmap, tip_bitmap, report_view_bitmap, list_view_bitmap,
		new_folder_bitmap, folder_bitmap, open_folder_bitmap,
		go_folder_up_bitmap, executable_file_bitmap, normal_file_bitmap,
		tick_mark_bitmap, cross_mark_bitmap, missing_image_bitmap, new_bitmap,
		file_open_bitmap, file_save_bitmap, file_save_as_bitmap,
		file_delete_bitmap, copy_bitmap, cut_bitmap, paste_bitmap, undo_bitmap,
		redo_bitmap, plus_bitmap, minus_bitmap, close_bitmap, quit_bitmap,
		find_bitmap, find_and_replace_bitmap, full_screen_bitmap, edit_bitmap,
		hard_disk_bitmap, floppy_bitmap, cdrom_bitmap, removable_bitmap,
		backend_logo_bitmap ],

	[ gui_toolbar:add_tool( Toolbar, _ToolId=N,
		text_utils:atom_to_string( N ),
		gui_bitmap:get_standard( N ), "Other short help" ) || N <- OtherNames ],

	% May not be necessary in this case:
	gui_toolbar:update_tools( Toolbar ),

	gui:subscribe_to_events( [
		{ [ onMouseRightButtonReleased, onWindowClosed ], Frame },
		{ [ onToolbarEntered, onItemSelected, onToolRightClicked ],
		  Toolbar } ] ),

	gui_frame:show( Frame ),

	test_main_loop( _InitialState={ Frame, Toolbar } ).




% @doc A very simple main loop, whose actual state is simply the GUI object
% corresponding to the frame that shall be closed to stop the test
% (i.e. CloseFrame).
%
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( State={ Frame, Toolbar } ) ->

	trace_utils:info( "Test main loop running..." ),

	receive

		{ onToolbarEntered, [ Toolbar, _ToolbarId, _EventContext ] } ->
			trace_utils:debug_fmt( "Entering toolbar ~w.", [ Toolbar ] ),
			test_main_loop( State );

		{ onItemSelected, [ Toolbar, quit_bitmap, _EventContext ] } ->
			trace_utils:debug(
				"Toolbar quit item selected, quitting; test success." ),
			gui:destruct_window( Frame ),
			gui:stop();

		{ onItemSelected, [ Toolbar, ToolId, _EventContext ] } ->
			trace_utils:debug_fmt( "Toolbar item ~p selected.", [ ToolId ] ),
			test_main_loop( State );

		{ onToolRightClicked, [ Toolbar, ToolId, _EventContext ] } ->
			trace_utils:debug_fmt( "Toolbar item ~p right-clicked.",
								   [ ToolId ]  ),
			test_main_loop( State );

		{ onWindowClosed, [ Frame, _FrameId, _Context ] } ->
			trace_utils:info( "Main frame has been closed; test success." ),
			gui_frame:destruct( Frame ),
			gui:stop();

		Other ->
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message: ~w.", [ Other ] ),
			test_main_loop( State )

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
