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
% Creation date: Monday, August 21, 2023.


% @doc Unit tests for the management of <b>dialogs</b>, that are various
% standard modal windows.
%
-module(gui_dialog_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-type my_test_state() :: frame().
% Here the main loop just has to remember the frame whose closing is awaited
% for.


% Shorthands:

-type frame() :: gui_frame:frame().



% @doc Executes the actual test.
-spec run_gui_test() -> void().
run_gui_test() ->

	test_facilities:display( "~nStarting the dialog test." ),

	gui:start(),

	Frame = gui_frame:create( "This is the overall frame for dialog testing" ),

	gui:subscribe_to_events( { onWindowClosed, Frame } ),

	Panel = gui_panel:create( Frame ),

	Sizer = gui_sizer:create( _Orientation=vertical ),

	gui_widget:set_sizer( Panel, Sizer ),

	% Common settings:

	Position = auto,
	ButtonSize = auto,
	ButtonStyle = [],

	Parent = Panel,

	% Label / name identifier pairs for each dialog-specific button:
	DlgPairs = [ { "message", message_dialog_button_id },
				 { "single-choice", single_choice_dialog_button_id },
				 { "multi-choice", multi_choice_dialog_button_id },
				 { "text-entry", text_entry_dialog_button_id },
				 { "file-selection", file_selection_dialog_button_id },
				 { "directory-selection",
				   directory_selection_dialog_button_id },
				 { "color-selection", color_selection_dialog_button_id },
				 { "font-selection", font_selection_dialog_button_id } ],

	Buttons = [ gui_button:create( "Show " ++ Lbl ++ " dialog", Position,
		ButtonSize, ButtonStyle, Nid, Parent ) || { Lbl, Nid } <- DlgPairs ],

	gui_sizer:add_elements( Sizer, Buttons ),
	gui:subscribe_to_events( [ { onButtonClicked, B } || B <- Buttons ] ),

	gui_frame:show( Frame ),

	% Order matters (bottom-up):
	[ gui_widget:fit( W  ) || W <- [ Panel, Frame ] ],

	test_main_loop( _InitialState=Frame ).




% @doc A very simple main loop, whose actual state is simply the GUI object
% corresponding to the frame that shall be closed to stop the test
% (i.e. CloseFrame).
%
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( State=Frame ) ->

	trace_utils:info( "Test main loop running..." ),

	receive

		{ onButtonClicked,
				[ _Button, _ButtonId=message_dialog_button_id, _Context ] } ->

			MsgDialog = gui_dialog:create_for_message(
				"This is a message dialog",
				{ style, [ yes_no_buttons, information_icon, center ] },
				_Parent=Frame ),

			case gui_dialog:show_modal( MsgDialog ) of

				yes_returned ->
					trace_utils:debug( "Message dialog returned 'yes'." );

				no_returned ->
					trace_utils:debug( "Message dialog returned 'no'." );

				cancel_returned ->
					trace_utils:debug( "Message dialog cancelled." )

			end,

			gui_dialog:destruct_for_message( MsgDialog ),

			test_main_loop( State );


		{ onButtonClicked, [ _Button, _ButtonId=single_choice_dialog_button_id,
							 _Context ] } ->

			ChoiceSpec = [ { choice_a, "Choice A" }, { choice_b, "Choice B" },
						   { choice_c, "Choice C" } ],

			SingChDialog = gui_dialog:create_for_single_choice(
				"This is a single-choice dialog.", "This is a caption",
				ChoiceSpec, _InitialChoiceDesignator=choice_c, _DialogOpts=[],
				_Parent=Frame ),

			case gui_dialog:show_modal( SingChDialog ) of

				ok_returned ->
					Designator = gui_dialog:get_choice_designator( SingChDialog,
																   ChoiceSpec ),

					trace_utils:debug_fmt( "Single-choice dialog returned "
						"'ok', selected designator being '~ts'.",
						[ Designator ] );

				cancel_returned ->
					trace_utils:debug( "Single-choice dialog cancelled." )

			end,

			gui_dialog:destruct_for_single_choice( SingChDialog ),

			test_main_loop( State );


		{ onButtonClicked, [ _Button, _ButtonId=multi_choice_dialog_button_id,
							 _Context ] } ->

			ChoiceSpec = [ { choice_a, "Choice A" }, { choice_b, "Choice B" },
						   { choice_c, "Choice C" }, { choice_d, "Choice D" },
						   { choice_e, "Choice E" } ],

			MultChDialog = gui_dialog:create_for_multi_choice(
				"This is a multi-choice dialog.", "This is a caption",
				ChoiceSpec,
				_InitialChoiceDesignators=[ choice_b, choice_c ],
				 { style, [ ok_button, cancel_button, center ] },
				_Parent=Frame ),

			case gui_dialog:show_modal( MultChDialog ) of

				ok_returned ->
					Designators = gui_dialog:get_choice_designators(
						MultChDialog, ChoiceSpec ),

					trace_utils:debug_fmt( "Multi-choice dialog returned "
						"'ok', selected designators being ~w.",
						[ Designators ] );

				cancel_returned ->
					trace_utils:debug( "Multi-choice dialog cancelled." )

			end,

			gui_dialog:destruct_for_multi_choice( MultChDialog ),

			test_main_loop( State );


		{ onButtonClicked, [ _Button, _ButtonId=text_entry_dialog_button_id,
							 _Context ] } ->

			TextEntDialog = gui_dialog:create_for_text_entry(
				"This is a text-entry dialog.", _DlgOpts=[],
				_InitialText="This is my initial text.", _Parent=Frame ),

			case gui_dialog:show_modal( TextEntDialog ) of

				ok_returned ->
					Text = gui_dialog:get_entered_text( TextEntDialog ),

					trace_utils:debug_fmt( "Text entry dialog returned "
						"text '~ts'.", [ Text ] );

				cancel_returned ->
					trace_utils:debug( "Text-entry dialog cancelled." )

			end,

			gui_dialog:destruct_for_text_entry( TextEntDialog ),

			test_main_loop( State );


		{ onButtonClicked, [ _Button, _ButtonId=file_selection_dialog_button_id,
							 _Context ] } ->

			MatchFilter = "All kinds of Erlang files|*.?rl;*.beam",

			FileSelDialog = gui_dialog:create_for_file_selection(
				_DlgOpts=[ { style, [ multiple_files, show_hidden_files ] },
						   { match_filter, MatchFilter } ],
				_Parent=Frame ),

			case gui_dialog:show_modal( FileSelDialog ) of

				ok_returned ->
					SelectedPaths =
						gui_dialog:get_selected_files( FileSelDialog ),

					trace_utils:debug_fmt( "File selection dialog returned "
						"following ~B paths: ~ts.", [ length( SelectedPaths ),
							text_utils:strings_to_listed_string(
								SelectedPaths ) ] );

				cancel_returned ->
					trace_utils:debug( "File selection dialog cancelled." )

			end,

			gui_dialog:destruct_for_file_selection( FileSelDialog ),

			test_main_loop( State );


		{ onButtonClicked,
				[ _Button, _ButtonId=directory_selection_dialog_button_id,
				  _Context ] } ->

			DirectorySelDialog = gui_dialog:create_for_directory_selection(
				_DlgOpts=[], _Parent=Frame ),

			case gui_dialog:show_modal( DirectorySelDialog ) of

				ok_returned ->
					SelectedPath =
						gui_dialog:get_selected_directory( DirectorySelDialog ),

					trace_utils:debug_fmt( "Directory selection dialog returned"
						" path '~ts'.", [ SelectedPath ] );

				cancel_returned ->
					trace_utils:debug( "Directory selection dialog cancelled." )

			end,

			gui_dialog:destruct_for_directory_selection( DirectorySelDialog ),

			test_main_loop( State );


		{ onButtonClicked, [ _Button,
							 _ButtonId=color_selection_dialog_button_id,
							 _Context ] } ->

			ColorSelDialog =
				gui_dialog:create_for_color_selection( _Parent=Frame ),

			case gui_dialog:show_modal( ColorSelDialog ) of

				ok_returned ->
					SelectedColorPath =
						gui_dialog:get_selected_color( ColorSelDialog ),

					trace_utils:debug_fmt( "Color selection dialog returned "
						"RGBA color ~w.", [ SelectedColorPath ] );

				cancel_returned ->
					trace_utils:debug( "Color selection dialog cancelled." )

			end,

			gui_dialog:destruct_for_color_selection( ColorSelDialog ),

			test_main_loop( State );


		{ onButtonClicked, [ _Button,
							 _ButtonId=font_selection_dialog_button_id,
							 _Context ] } ->

			FontSelDialog =
				gui_dialog:create_for_font_selection( _Parent=Frame ),

			case gui_dialog:show_modal( FontSelDialog ) of

				ok_returned ->
					SelectedFont =
						gui_dialog:get_selected_font( FontSelDialog ),

					trace_utils:debug_fmt( "Font selection dialog returned "
						"font ~w, described in a platform-dependent way "
						"as '~ts', in a user-friendly one as '~ts'.",
						[ SelectedFont,
						  gui_font:get_platform_dependent_description(
							SelectedFont ),
						  gui_font:get_user_friendly_description(
							SelectedFont ) ] );

				cancel_returned ->
					trace_utils:debug( "Font selection dialog cancelled." )

			end,

			gui_dialog:destruct_for_font_selection( FontSelDialog ),

			test_main_loop( State );


		{ onWindowClosed, [ Frame, _FrameId, _Context ] } ->
			trace_utils:info( "Main frame has been closed; test success." ),
			gui_frame:destruct( Frame ),
			gui:stop();

		Other ->
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message: ~p.", [ Other ] ),
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
