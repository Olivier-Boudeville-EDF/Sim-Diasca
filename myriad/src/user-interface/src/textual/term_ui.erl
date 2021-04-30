% Copyright (C) 2018-2021 Olivier Boudeville
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
% Creation date: Wednesday, May 2, 2018.



% This is the second most basic, terminal-based textual interface, with colors,
% dialog boxes, etc., based on the 'dialog' or 'whiptail' tools.
%
% See:
% - term_ui_test.erl for the corresponding test
% - text_ui.erl for a more basic text interface
% - gui.erl for a graphical counterpart
%
% See also: trace_utils.erl for another kind of output and test-dialog.sh for an
% autonomous, standalone test.
%
-module(term_ui).



% Implementation notes:
%
% In this very specific case, we use the process dictionary to avoid having to
% keep around a UI-state variable in all calls.
%
% We generally do not provide here counterpart functions dealing with an
% explicit state instead (too long, for too little interest, and too many arity
% clashes).


% Note that using this service from within an escript seems to raise issues:
% {display_error_reported,255,[]} is returned whenever trying to display a modal
% window.



% Dialog-specific section.
%
% These dialogs will take advantage of the current locale (ex: 'OK' vs
% 'Accepter').
%
% See also: test-dialog.sh for a live demo.



% List of known types of dialogs:
%
% (see also: https://invisible-island.net/dialog/dialog-figures.html)


% 'msgbox' dialog: a modal window to be dismissed by hitting Enter
%
% - ex: LANG= dialog --title "Hello" --msgbox 'Hello world!' 6 20
%   (validated after Enter is pressed)


% 'yesno' (yes/no) dialog: a modal window offering two possibilities
%
% - ex: LANG= dialog --title "Message" --yesno "Are you having\n fun?" 6 25
%   (exit status: 0 if Yes, 1 otherwise (No or interrupted))


% 'infobox' dialog: a window displayed once
%
% - ex: LANG= dialog --infobox "Please wait" 10 30 ; sleep 4
%   (disappears after sleep)


% 'pause' dialog: pauses for a number of seconds
%
% - ex: LANG= dialog --pause "Pausing" 10 30 4
%   (disappears once pause is over)


% 'inputbox' dialog: a request for the user to type an echoed string
%
% - ex: LANG= dialog --inputbox "Enter your name:" 8 40
%   (input written to standard error, possibly redirected)


% 'passwordbox' dialog: a request for the user to type a non-echoed string
%
% - ex: LANG= dialog  --passwordbox "Enter some password:" 8 40
%	(no input echoed, but written to standard error, possibly redirected)


% 'textbox' dialog: displays the content of a file
%
% - ex: LANG= dialog --textbox /etc/profile 22 70


% 'menu' dialog: allows to select one option among a set (quite similar to
% radiolist, yet no default can be specified)
%
% - ex: LANG= dialog --menu "Choose one:" 10 30 3 1 red 2 green 3 blue
%   (choice index written to standard error, possibly redirected)


% 'radiolist' dialog: allows to select one option among a set (quite similar to
% menu); potentially a bit misleading as, if hitting the Enter key, the selected
% item will be the starred one, not necessarily the currently selected one...
%
% - ex: LANG= dialog --radiolist "Select CPU type:" 10 40 4 1 386SX off 2 386DX
% on 3 486SX off 4 486DX off 2>${result_file}
%   (choice index written to standard error, possibly redirected)


% 'treeview' dialog: allows to select a tree element
%
% - ex: LANG= dialog --treeview "Select tree element:" 10 40 5 1 a on 1 2 b on 2
% 3 c off 1 4 d on 3
%   (element written to standard error, possibly redirected)


% 'checklist' dialog: allows to select non-exclusive options, thanks to a set of
% radio buttons with defaults
%
% - ex: LANG= dialog --checklist "Choose toppings:" 10 40 3 1 Cheese on 2
% "Tomato Sauce" on 3 Anchovies off
%   (choice indexes written to standard error, possibly redirected)


% 'calendar' dialog: allows to select a date
%
% - ex: LANG= dialog --calendar "Select a date:"  10 40 17 5 1977
%   (date written to standard error, possibly redirected)


% 'timebox' dialog: allows to select a time
%
% - ex: LANG= dialog --timebox "Select a time:"  10 40
%   (time written to standard error, possibly redirected)


% 'fselect' dialog: allows to select a file
%
% - ex: LANG= dialog --fselect / 10 40
%   (file path written to standard error, possibly redirected)


% 'dselect' dialog: allows to select a directory
%
% - ex: LANG= dialog --dselect / 10 40
%   (directory path written to standard error, possibly redirected)


% 'gauge' dialog: display percentage values
%
% - ex: LANG= dialog --gauge "My gauge:" 10 20 12



% 'tailbox' dialog:

% 'tailboxbg' dialog:

% 'progressbox' dialog:


% 'buildlist' dialog:

% 'editbox' dialog:

% 'form' dialog:

% 'inputmenu' dialog:

% 'mixedform' dialog:

% 'mixedgauge' dialog:

% 'passwordform' dialog:


% 'prgbox' dialog:

% 'programbox' dialog:


% 'rangebox' dialog:


% Also useful:
%
% - 'dialog --print-maxsize' (ex: MaxSize: 35, 123)
% - 'dialog --clear'


% The temporary file needed to collect the outputs of some backend operations:
-define( temp_file, "/tmp/.myriad.term_ui.tmp" ).


% Basic UI operations.
-export([ is_available/0,

		  start/0, start/1,

		  set/1, set/2, unset/1,

		  display/1, display/2,

		  display_numbered_list/2,

		  display_warning/1, display_warning/2,

		  display_error/1, display_error/2,
		  display_error_numbered_list/2,

		  add_separation/0,

		  get_text/1, get_text/2,
		  get_text_as_integer/1, get_text_as_integer/2,
		  get_text_as_maybe_integer/1, get_text_as_maybe_integer/2,
		  read_text_as_integer/1, read_text_as_integer/2,
		  read_text_as_maybe_integer/1, read_text_as_maybe_integer/2,

		  ask_yes_no/2, ask_yes_no/3,

		  choose_designated_item/1, choose_designated_item/2,
		  choose_designated_item/3,

		  choose_designated_item_with_default/2,
		  choose_designated_item_with_default/3,
		  choose_designated_item_with_default/4,

		  choose_numbered_item/1, choose_numbered_item/2,
		  choose_numbered_item/3,

		  choose_numbered_item_with_default/2,
		  choose_numbered_item_with_default/3,
		  choose_numbered_item_with_default/4,

		  set_setting/2, set_setting/3,
		  set_settings/1, set_settings/2,

		  unset_setting/1, unset_setting/2,
		  unset_settings/1, unset_settings/2,

		  get_setting/1,

		  trace/1, trace/2,

		  clear/0, clear/1,

		  stop/0, stop/1,

		  to_string/0, to_string/1 ]).



% The default place where file-based communications are to occur:
% (finally not needed)
%-define( default_state_path, "/tmp/.myriad-term_ui.state" ).


-type dialog_tool() :: 'dialog' | 'whiptail'.


% The locale to be used by dialogs:
% - default: the current user one
% - none: no locale (defaulting to "C")
% - a user-specified one
%
-type dialog_locale() :: 'default' | 'none' | ustring().


-record( term_ui_state, {

		   %state_filename = ?default_state_path :: file_path(),
		   dialog_tool :: dialog_tool(),
		   dialog_tool_path :: file_path(),
		   locale = default :: dialog_locale(),

		   % Generally little use of console outputs for this backend:
		   log_console = false :: boolean(),

		   log_file = undefined :: maybe( file_utils:file() ),
		   settings :: setting_table() } ).


-type ui_state() :: #term_ui_state{}.


% For common, transverse defines:
-include("ui.hrl").


% For control codes:
-include("term_ui.hrl").


-export_type([ ui_state/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type format_string() :: text_utils:format_string().

-type file_path() :: file_utils:file_path().


% The key used by this module to store its state in the process dictionaty:
-define( state_key, term_ui_state ).



% Tells whether this user-interface backend is available.
-spec is_available() -> boolean().
is_available() ->

	case lookup_dialog_tool() of

		undefined ->
			false;

		% { T, TPath }:
		_ ->
			true

	end.



% Starts the UI with default settings.
%
% Stores the corresponding state in the process dictionary, yet returns as well
% that state, for any explicit later operation.
%
-spec start() -> ui_state().
start() ->
	start( _Opts=[] ).



% Starts the UI with specified settings.
%
% Stores the corresponding state in the process dictionary, yet returns as well
% that state, for any explicit later operation.
%
-spec start( ui_options() ) -> ui_state().
start( Options ) ->

	DialogUIState = case lookup_dialog_tool() of

		undefined ->
			throw( no_dialog_tool_available );

		{ Tool, ToolPath } ->
			init_state_with_dimensions( Tool, ToolPath )

	end,

	start_helper( Options, DialogUIState ).



% (helper)
start_helper( _Options=[], UIState ) ->

	case process_dictionary:put( ?ui_name_key, ?MODULE ) of

		undefined ->
			ok;

		UIName ->
			throw( { ui_already_started, UIName } )

	end,

	%trace_utils:debug_fmt( "Storing following initial UI state: ~ts",
	%					   [ to_string( UIState ) ] ),

	% No prior state expected:
	case process_dictionary:put( ?ui_state_key, UIState ) of

		undefined ->
			ok;

		_ ->
			throw( term_ui_already_started )

	end,

	UIState;

start_helper( _Options=[ log_file | T ], UIState ) ->
	start_helper( [ { log_file, "ui.log" } | T ], UIState );

start_helper( _Options=[ { log_file, Filename } | T ], UIState ) ->

	LogFile = file_utils:open( Filename, [ write, exclusive, raw ] ),

	file_utils:write_ustring( LogFile, "Starting term UI.\n" ),

	NewUIState = UIState#term_ui_state{ log_file=LogFile },

	start_helper( T, NewUIState );

start_helper( _Options=[ log_console | T ], UIState ) ->
	NewUIState = UIState#term_ui_state{ log_console=true },
	start_helper( T, NewUIState );

start_helper( UnexpectedList, _UIState ) when is_list( UnexpectedList ) ->
	throw( { unexpected_options, UnexpectedList } );

start_helper( SingleElem, UIState ) ->
	start_helper( [ SingleElem ], UIState ).



% (helper)
-spec init_state_with_dimensions( dialog_tool(), file_path() ) -> ui_state().
init_state_with_dimensions( Tool=dialog, DialogPath ) ->

	Cmd = text_utils:join( _Sep=" ",
		[ DialogPath, "--print-maxsize", get_redirect_string_for_code() ] ),

	%trace_utils:debug_fmt( "Command: '~ts'.", [ Cmd ] ),

	{ Env, PortOpts } = get_execution_settings(),

	% By default we will be using the full terminal space:
	case system_utils:run_command( Cmd, Env, _WorkingDir=undefined,
								   PortOpts ) of

		% Ex: Result="MaxSize: 28, 107"
		{ _ExitStatus=0, _Result="MaxSize: " ++ SizeString } ->

			% Here, SizeString="28, 107".
			[ HeightString, " " ++ WidthString ] =
				text_utils:split( SizeString, [ $, ] ),

			Height = text_utils:string_to_integer( HeightString ),

			Width = text_utils:string_to_integer( WidthString ),

			DimSettings = ?ui_table:new(
							 [ { max_height, Height }, { max_width, Width } ] ),

			#term_ui_state{ dialog_tool=Tool,
							dialog_tool_path=DialogPath,
							settings=DimSettings };

		{ ExitStatus, Result } ->
			throw( { max_size_lookup_failure, ExitStatus, Result } )

	end.



% Sets specified UI setting.
-spec set( ui_setting_key(), ui_setting_value() ) -> void().
set( SettingKey, SettingValue ) ->
	set( [ { SettingKey, SettingValue } ] ).


% Sets specified UI settings.
-spec set( [ ui_setting_entry() ] ) -> void().
set( SettingEntries ) ->

	UIState = #term_ui_state{ settings=SettingTable } = get_state(),

	NewSettingTable = ?ui_table:add_entries( SettingEntries, SettingTable ),

	set_state( UIState#term_ui_state{ settings=NewSettingTable } ).



% Unsets specified UI setting.
-spec unset( [ ui_setting_key() ] | ui_setting_key() ) -> void().
unset( SettingKeys ) when is_list( SettingKeys ) ->

	UIState = #term_ui_state{ settings=SettingTable } = get_state(),

	NewSettingTable = ?ui_table:remove_entries( SettingKeys, SettingTable ),

	set_state( UIState#term_ui_state{ settings=NewSettingTable } );

unset( SettingKey ) ->

	UIState = #term_ui_state{ settings=SettingTable } = get_state(),

	NewSettingTable = ?ui_table:remove_entry( SettingKey, SettingTable ),

	set_state( UIState#term_ui_state{ settings=NewSettingTable } ).




% Displays specified text, as a normal message.
-spec display( text() ) -> void().
display( Text ) ->

	% Note: 'dialog' will eat the leading spaces of a line; this will be visible
	% typically in bullet lists.

	% Simplified example:
	%Cmd = "dialog --msgbox 'Hello!' 8 40",

	% Single quotes induce no specific issues (as are enclosed in double ones)
	EscapedText = text_utils:escape_double_quotes( Text ),

	%trace_utils:debug_fmt( "Original text: '~ts'; once escaped: '~ts'.",
	%					   [ Text, EscapedText ] ),

	#term_ui_state{ dialog_tool_path=ToolPath,
					settings=SettingTable } = get_state(),

	%trace_utils:debug_fmt( "Dialog path: '~ts'.", [ ToolPath ] ),


	{ SettingString, SuffixString } =
		get_dialog_settings_for_return_code( SettingTable ),

	%trace_utils:debug_fmt( "Setting string: '~ts'.", [ SettingString ] ),
	%trace_utils:debug_fmt( "Suffix string: '~ts'.", [ SuffixString ] ),

	DialogString = text_utils:format( "--msgbox \"~ts\" ~ts",
									  [ EscapedText, SuffixString ] ),

	Cmd = text_utils:join( _Sep=" ",
						   [ ToolPath, SettingString, DialogString ] ),

	%trace_utils:debug_fmt( "term_ui display command: '~ts'.", [ Cmd ] ),

	{ Env, PortOpts } = get_execution_settings(),

	case system_utils:run_command( Cmd, Env, _WorkingDir=undefined,
								   PortOpts ) of

		{ _ExitStatus=0, _Output="" } ->
			ok;

		{ _ExitStatus=0, Output } ->
			trace_utils:warning_fmt( "Unexpected output: '~ts'.", [ Output ] );

		{ ExitStatus, Output } ->
			trace_utils:error_fmt( "Display error reported for '~ts' "
				"(exit status: ~B):~n~ts", [ EscapedText, ExitStatus, Output ] )
			%throw( { display_error_reported, ExitStatus, Output } )

	end.



% Displays specified formatted text, as a normal message.
-spec display( format_string(), [ term() ] ) -> void().
display( FormatString, Values ) ->
	display( text_utils:format( FormatString, Values ) ).



% Displays in-order the items of specified list, as a normal message.
-spec display_numbered_list( label(), [ text() ] ) -> void().
display_numbered_list( Label, Lines ) ->
	LineStrings = text_utils:strings_to_enumerated_string( Lines ),
	display( Label ++ LineStrings ).



% Displays specified text, as a warning message.
-spec display_warning( text() ) -> void().
display_warning( Text ) ->

	% Simplified example:
	%Cmd = "dialog --infobox 'Warning!' 8 40",

	% Single quotes induce no specific issues (as are enclosed in double ones)
	EscapedText = text_utils:escape_double_quotes( Text ),

	%trace_utils:debug_fmt( "Original text: '~ts'; once escaped: '~ts'.",
	%					   [ Text, EscapedText ] ),

	#term_ui_state{ dialog_tool_path=ToolPath,
					settings=SettingTable } = get_state(),

	%trace_utils:debug_fmt( "Dialog path: '~ts'.", [ ToolPath ] ),

	WarningSettingTable = ?ui_table:add_entry( 'title', ?yellow"Warning"?normal,
											   SettingTable ),

	{ SettingString, SuffixString } =
		get_dialog_settings_for_return_code( WarningSettingTable ),

	%trace_utils:debug_fmt( "Setting string: '~ts'.", [ SettingString ] ),
	%trace_utils:debug_fmt( "Suffix string: '~ts'.", [ SuffixString ] ),

	% Apparently button colors are ignored:
	%OKLabel = "--ok-label '"?red" Abort "?normal"'",
	OKLabel = "--ok-label 'Abort'",

	DialogString = "--colors " ++ OKLabel
		++ text_utils:format( " --msgbox \"~ts\" ~ts",
							  [ EscapedText, SuffixString ] ),

	Cmd = text_utils:join( _Sep=" ",
						   [ ToolPath, SettingString, DialogString ] ),

	%trace_utils:debug_fmt( "term_ui display command: '~ts'.", [ Cmd ] ),

	{ Env, PortOpts } = get_execution_settings(),

	case system_utils:run_command( Cmd, Env, _WorkingDir=undefined,
								   PortOpts ) of

		{ _ExitStatus=0, _Output="" } ->
			ok;

		%{ _ExitStatus=0, Output } ->
			%trace_utils:debug_fmt( "Display output: '~ts'.", [ Output ] );

		{ ExitStatus, Output } ->
			throw( { display_warning_reported, ExitStatus, Output } )

	end.



% Displays specified formatted text, as a warning message.
-spec display_warning( format_string(), [ term() ] ) -> void().
display_warning( FormatString, Values ) ->
	display_warning( text_utils:format( FormatString, Values ) ).



% Displays specified text, as an error message.
-spec display_error( text() ) -> void().
display_error( Text ) ->

	% Simplified example:
	%Cmd = "dialog --infobox 'Error!' 8 40",

	% Single quotes induce no specific issues (as are enclosed in double ones)
	EscapedText = text_utils:escape_double_quotes( Text ),

	%trace_utils:debug_fmt( "Original text: '~ts'; once escaped: '~ts'.",
	%					   [ Text, EscapedText ] ),

	#term_ui_state{ dialog_tool_path=ToolPath,
					settings=SettingTable } = get_state(),

	%trace_utils:debug_fmt( "Dialog path: '~ts'.", [ ToolPath ] ),

	ErrorSettingTable = ?ui_table:add_entry( 'title', ?red"Error"?normal,
											 SettingTable ),

	{ SettingString, SuffixString } =
		get_dialog_settings_for_return_code( ErrorSettingTable ),

	%trace_utils:debug_fmt( "Setting string: '~ts'.", [ SettingString ] ),
	%trace_utils:debug_fmt( "Suffix string: '~ts'.", [ SuffixString ] ),

	% Apparently button colors are ignored:
	%OKLabel = "--ok-label '"?red" Abort "?normal"'",
	OKLabel = "--ok-label 'Abort'",

	DialogString = "--colors " ++ OKLabel
		++ text_utils:format( " --msgbox \"~ts\" ~ts",
							  [ EscapedText, SuffixString ] ),

	Cmd = text_utils:join( _Sep=" ",
						   [ ToolPath, SettingString, DialogString ] ),

	%trace_utils:debug_fmt( "term_ui display command: '~ts'.", [ Cmd ] ),

	{ Env, PortOpts } = get_execution_settings(),

	case system_utils:run_command( Cmd, Env, _WorkingDir=undefined,
								   PortOpts ) of

		{ _ExitStatus=0, _Output="" } ->
			ok;

		%{ _ExitStatus=0, Output } ->
			%trace_utils:debug_fmt( "Display output: '~ts'.", [ Output ] );

		{ ExitStatus, Output } ->
			throw( { display_error_reported, ExitStatus, Output } )

	end.



% Displays specified formatted text, as an error message.
-spec display_error( format_string(), [ term() ] ) -> void().
display_error( FormatString, Values ) ->
	display_error( text_utils:format( FormatString, Values ) ).


% Displays in-order the items of specified list, as an error message.
-spec display_error_numbered_list( label(), [ text() ] ) -> void().
display_error_numbered_list( Label, Lines ) ->
	LineStrings = text_utils:strings_to_enumerated_string( Lines ),
	display_error( Label ++ LineStrings ).


% Adds a default separation between previous and next content.
%
% Note: meaningless for this backend.
%
-spec add_separation() -> void().
add_separation() ->
	% Could be a clear.
	ok.



% Returns the user-entered text, based on an implicit state.
%
% (const)
%
-spec get_text( prompt() ) -> text().
get_text( Prompt ) ->
	get_text( Prompt, _UIState=get_state() ).


% Returns the user-entered text, based on an explicit state, or throws
% operation_cancelled if the user preferred the cancel button.
%
% (const)
%
-spec get_text( prompt(), ui_state() ) -> text().
get_text( Prompt,
		  %_UIState ) ->
		  #term_ui_state{ dialog_tool_path=ToolPath,
						  settings=SettingTable } ) ->

	% Ex: dialog --backtitle "AA" --inputbox "Enter your name:" 8 40 2>
	% foobar.txt

	% Single quotes induce no specific issues (as are enclosed in double ones)
	EscapedPrompt = text_utils:escape_double_quotes( Prompt ),

	{ SettingString, SuffixString } =
		get_dialog_settings_for_file_return( SettingTable ),

	DialogString = text_utils:format( "--inputbox \"~ts\" ~ts",
									  [ EscapedPrompt, SuffixString ] ),

	CmdStrings = [ ToolPath, SettingString, DialogString ],

	%trace_utils:debug_fmt( "CmdStrings = ~p", [ CmdStrings ] ),

	Cmd = text_utils:join( _Sep=" ", CmdStrings ),

	{ Env, PortOpts } = get_execution_settings(),

	Read = case system_utils:run_command( Cmd, Env, _WorkingDir=undefined,
										  PortOpts ) of

		{ _ExitStatus=0, _Result="" } ->
			TmpFilename = ?temp_file,
			case file_utils:is_existing_file( TmpFilename ) of

				true ->
					BinContent = file_utils:read_whole( TmpFilename ),
					%trace_utils:debug_fmt( "Content = ~p", [ BinContent ] ),
					file_utils:remove_file(TmpFilename ),
					text_utils:binary_to_string( BinContent );

				false ->
					throw( { term_ui_temp_file_not_found, TmpFilename } )

			end;

		{ _ExitStatus=0, Result } ->
			throw( { unexpected_result, Result } );

		{ _ExitStatus=1, _Output=[] } ->
			throw( operation_cancelled );

		{ ExitStatus, Output } ->
			throw( { get_text_failed, ExitStatus, Output } )

	end,

	%trace_utils:debug_fmt( "get_text/2 read: '~p'.", [ Read ] ),

	text_utils:remove_ending_carriage_return( Read ).



% Returns the user-entered text, once translated to an integer, based on an
% implicit state.
%
% (const)
%
-spec get_text_as_integer( prompt(), ui_state() ) -> text().
get_text_as_integer( Prompt ) ->
	get_text_as_integer( Prompt, _UIState=get_state() ).


% Returns the user-entered text, once translated to an integer, based on an
% explicit state.
%
% (const)
%
-spec get_text_as_integer( prompt(), ui_state() ) -> text().
get_text_as_integer( Prompt, UIState ) ->

	Text = get_text( Prompt, UIState ),

	text_utils:string_to_integer( Text ).



% Returns the user-entered text, once translated to an integer, based on an
% implicit state, prompting the user until a valid input is obtained.
%
% (const)
%
-spec read_text_as_integer( prompt() ) -> text().
read_text_as_integer( Prompt ) ->
	read_text_as_integer( Prompt, _UIState=get_state() ).


% Returns the user-entered text, once translated to an integer, based on an
% explicit state, prompting the user until a valid input is obtained.
%
% (const)
%
-spec read_text_as_integer( prompt(), ui_state() ) -> text().
read_text_as_integer( Prompt, UIState ) ->

	Text = get_text( Prompt, UIState ),

	case text_utils:try_string_to_integer( Text ) of

		undefined ->
			%trace_utils:debug_fmt( "(rejected: '~ts')", [ Text ] ),
			display_error( "Invalid value specified (~p).", [ Text ] ),
			read_text_as_integer( Prompt, UIState );

		I ->
			I

	end.



% Returns the user-entered text (if any) after specified prompt, once translated
% to (possibly) an integer, based on an implicit state.
%
% (const)
%
-spec get_text_as_maybe_integer( prompt() ) -> maybe( text() ).
get_text_as_maybe_integer( Prompt ) ->
	get_text_as_maybe_integer( Prompt, _UIState=get_state() ).


% Returns the user-entered text (if any) after specified prompt, once translated
% to (possibly) an integer, based on an explicit state.
%
% (const)
%
-spec get_text_as_maybe_integer( prompt(), ui_state() ) -> maybe( text() ).
get_text_as_maybe_integer( Prompt, UIState ) ->

	case get_text( Prompt, UIState ) of

		"" ->
			undefined;

		Text ->
			text_utils:string_to_integer( Text )

	end.



% Returns the user-entered text after specified prompt, once translated to an
% integer, prompting the user until a valid input is obtained: either a string
% that resolves to an integer (then returned), or an empty string (then
% returning 'undefined'), based on an implicit state.
%
% (const)
%
-spec read_text_as_maybe_integer( prompt() ) -> maybe( text() ).
read_text_as_maybe_integer( Prompt ) ->
	read_text_as_maybe_integer( Prompt, _UIState=get_state() ).


% Returns the user-entered text after specified prompt, once translated to an
% integer, prompting the user until a valid input is obtained: either a string
% that resolves to an integer (then returned), or an empty string (then
% returning 'undefined'), based on an explicit state.
%
% (const)
%
-spec read_text_as_maybe_integer( prompt(), ui_state() ) -> maybe( text() ).
read_text_as_maybe_integer( Prompt, UIState ) ->

	case get_text( Prompt, UIState ) of

		"" ->
			undefined;

		Text ->
			case text_utils:try_string_to_integer( Text ) of

				undefined ->
					read_text_as_maybe_integer( Prompt, UIState );

				I ->
					I

			end

	end.



% Displays specified prompt, let the user choose between two options, "yes" and
% "no" (with specified default option), and returns that choice.
%
-spec ask_yes_no( prompt(), binary_choice() ) -> binary_choice().
ask_yes_no( Prompt, BinaryDefault ) ->
	ask_yes_no( Prompt, BinaryDefault, get_state() ).



% Displays specified prompt, let the user choose between two options, "yes" and
% "no" (with specified default option), and returns that choice.
%
-spec ask_yes_no( prompt(), binary_choice(), ui_state() ) -> binary_choice().
ask_yes_no( Prompt, BinaryDefault, #term_ui_state{ dialog_tool_path=ToolPath,
												   settings=SettingTable } ) ->

	% Ex: dialog --backtitle "AA" --title "BB" --defaultno --yesno "Having\n
	% fun?" 6 25

	% Single quotes induce no specific issues (as are enclosed in double ones)
	EscapedPrompt = text_utils:escape_double_quotes( Prompt ),

	DefaultChoiceOpt = case BinaryDefault of

		yes ->
			"";

		no ->
			"--defaultno"

	end,

	{ SettingString, SuffixString } =
		get_dialog_settings_for_return_code( SettingTable ),

	DialogString = text_utils:format( "~ts --yesno \"~ts\" ~ts",
						[ DefaultChoiceOpt, EscapedPrompt, SuffixString ] ),

	CmdStrings = [ ToolPath, SettingString, DialogString ],

	%trace_utils:debug_fmt( "CmdStrings = ~p", [ CmdStrings ] ),

	Cmd = text_utils:join( _Sep=" ", CmdStrings ),

	{ Env, PortOpts } = get_execution_settings(),

	case system_utils:run_command( Cmd, Env, _WorkingDir=undefined,
								   PortOpts ) of

		{ _ExitStatus=0, _Result=[] } ->
			yes;

		{ _ExitStatus=1, _Result=[] } ->
			no;

		{ ExitStatus, Output } ->
			throw( { yes_no_choice_failed, ExitStatus, Output } )

	end.



% Selects, using a default prompt, an item among the specified ones (comprising,
% for each, a user-specified, internal, designator and a text), and returns its
% designator.
%
% Note that the 'ui_cancel' designator atom can also be returned, should the
% user prefer to cancel that operation.
%
% (const)
%
-spec choose_designated_item( [ choice_element() ] ) -> choice_designator().
choose_designated_item( Choices ) ->

	Prompt = text_utils:format( "Select among these ~B choices:",
							   [ length( Choices ) + 1 ] ),

	choose_designated_item( Prompt, Choices ).



% Selects, using specified prompt, an item among the specified ones (comprising,
% for each, a user-specified, internal, designator and a text), and returns its
% designator.
%
% Note that the 'ui_cancel' designator atom can also be returned, should the
% user prefer to cancel that operation.
%
% (const)
%
-spec choose_designated_item( prompt(), [ choice_element() ] ) ->
									choice_designator().
choose_designated_item( Prompt, Choices ) ->
	choose_designated_item( Prompt, Choices, get_state() ).



% Selects, based on an explicit state, using the specified prompt, an item among
% the specified ones (comprising, for each, a user-specified, internal,
% designator and a text), and returns its designator.
%
% Note that the 'ui_cancel' designator atom can also be returned, should the
% user prefer to cancel that operation.
%
% (const)
%
-spec choose_designated_item( prompt(), [ choice_element() ], ui_state() ) ->
									choice_designator().
choose_designated_item( Prompt, Choices,
						#term_ui_state{ dialog_tool_path=ToolPath,
										settings=SettingTable } ) ->

	% Command: dialog --menu <text> <height> <width> <menu height> <tag1>
	% <item1>...
	%
	% Ex: dialog --menu "Hello" 0 0 0 1 One 2 Two 3 Three

	{ Designators, Texts } = lists:unzip( Choices ),

	case lists:member( ui_cancel, Designators ) of

		true ->
			throw( { disallowed_choice_designator, ui_cancel } );

		false ->
			ok

	end,

	ChoiceCount = length( Choices ),

	% We simply tag the choices with a counter (rather than using the designator
	% atoms):
	%
	NumChoices = lists:zip( lists:seq( 1, ChoiceCount ), Texts ),

	NumStrings = lists:foldl( fun( { Num, Text }, AccStrings ) ->
									 [ text_utils:format( " ~B \"~ts\"",
											[ Num, Text ] ) | AccStrings ]
							  end,
							  _Acc0=[],
							  _List=NumChoices ),

	{ SettingString, _SuffixString } =
		get_dialog_settings_for_return_code( SettingTable ),

	AutoSizeString = "0 0",

	DialogStrings = [ "--menu", "\"" ++ Prompt ++ "\"", AutoSizeString,
	  _MenuHeight=text_utils:integer_to_string( ChoiceCount )
		 | lists:reverse( [ get_redirect_string_for_code() | NumStrings ] ) ],

	CmdStrings = [ ToolPath, SettingString | DialogStrings ],

	%trace_utils:debug_fmt( "CmdStrings = ~p", [ CmdStrings ] ),

	Cmd = text_utils:join( _Sep=" ", CmdStrings ),

	{ Env, PortOpts } = get_execution_settings(),

	case system_utils:run_command( Cmd, Env, _WorkingDir=undefined,
								   PortOpts ) of

		{ _ExitStatus=0, Result } ->
			ChosenNum = text_utils:string_to_integer( Result ),
			list_utils:get_element_at( Designators, ChosenNum );

		{ _ExitStatus=1, _Output=[] } ->
			ui_cancel;

		{ ExitStatus, Output } ->
			trace_utils:error_fmt( "Exit code ~B and error output '~s' "
				"for prompt '~s' and choices:~n~p "
				"corresponding to command:~n~s",
				[ ExitStatus, Output, Prompt, Choices, Cmd ] ),
			throw( { choice_failed, ExitStatus, Output } )

	end.



% Selects, based on an implicit state, using a default prompt, an item among the
% specified ones (comprising, for each, a user-specified, internal, designator
% and a text), with a default choix designator being specified, and returns its
% designator.
%
% Note that the 'ui_cancel' designator atom can also be returned, should the
% user prefer to cancel that operation.
%
% (const)
%
-spec choose_designated_item_with_default( [ choice_element() ],
				choice_designator() ) -> choice_designator().
choose_designated_item_with_default( Choices, DefaultChoiceDesignator ) ->

	Prompt = text_utils:format( "Select among these ~B choices:",
							   [ length( Choices ) + 1 ] ),

	choose_designated_item_with_default( Prompt, Choices,
						DefaultChoiceDesignator, get_state() ).



% Selects, based on an implicit state, using the specified prompt, an item among
% the specified ones (comprising, for each, a user-specified, internal,
% designator and a text), with a default choix designator being specified, and
% returns its designator.
%
% Note that the 'ui_cancel' designator atom can also be returned, should the
% user prefer to cancel that operation.
%
% (const)
%
-spec choose_designated_item_with_default( prompt(), [ choice_element() ],
				choice_designator() ) -> choice_designator().
choose_designated_item_with_default( Prompt, Choices,
									 DefaultChoiceDesignator ) ->
	choose_designated_item_with_default( Prompt, Choices,
						DefaultChoiceDesignator, get_state() ).



% Selects, based on an explicit state, using the specified prompt, an item among
% the specified ones (comprising, for each, a user-specified, internal,
% designator and a text), with a default choix designator being specified, and
% returns its designator.
%
% Note that the 'ui_cancel' designator atom can also be returned, should the
% user prefer to cancel that operation.
%
% (const)
%
-spec choose_designated_item_with_default( prompt(), [ choice_element() ],
				choice_designator(), ui_state() ) -> choice_designator().
choose_designated_item_with_default( Prompt, Choices, DefaultChoiceDesignator,
								#term_ui_state{ dialog_tool_path=ToolPath,
												settings=SettingTable } ) ->

	% Using 'radio list' rather than 'menu', for the selectable initial, default
	% choice.

	% Command: dialog --radiolist <text> <height> <width> <list height> <tag1>
	% <item1> <status1>...
	%
	% Ex: dialog --radiolist "Select CPU type:" 10 40 10 1 386SX off 2 386DX on
	% 3 486SX off 4 486DX off

	% (very much inspired from choose_designated_item/3)

	{ Designators, Texts } = lists:unzip( Choices ),

	case lists:member( ui_cancel, Designators ) of

		true ->
			throw( { disallowed_choice_designator, ui_cancel } );

		false ->
			ok

	end,

	DefaultChoiceIndex = case list_utils:get_maybe_index_of(
								DefaultChoiceDesignator, Designators ) of

		undefined ->
			throw( { default_designator_not_among_choices,
					 DefaultChoiceDesignator, Designators } );

		I ->
			I

	end,

	ChoiceCount = length( Choices ),

	% We simply tag the choices with a counter (rather than using the designator
	% atoms):
	%
	NumChoices = lists:zip( lists:seq( 1, ChoiceCount ), Texts ),

	% Cannot directly match DefaultChoiceIndex in the head of a clause (not
	% understood by the compiler):
	%
	NumStrings = lists:foldl(
		   fun( { Num, Text }, AccStrings ) ->
				case Num of

					DefaultChoiceIndex ->
						[ text_utils:format( " ~B \"~ts\" on",
							[ DefaultChoiceIndex, Text ] ) | AccStrings ];

					_ ->
						[ text_utils:format( " ~B \"~ts\" off",
							[ Num, Text ] ) | AccStrings ]

				end
		end,
		_Acc0=[],
		_List=NumChoices ),

	{ SettingString, _SuffixString } =
		get_dialog_settings_for_return_code( SettingTable ),

	AutoSizeString = "0 0",

	DialogStrings = [ "--radiolist", "\"" ++ Prompt ++ "\"", AutoSizeString,
	  _MenuHeight=text_utils:integer_to_string( ChoiceCount )
		 | lists:reverse( [ get_redirect_string_for_code() | NumStrings ] ) ],

	CmdStrings = [ ToolPath, SettingString | DialogStrings ],

	%trace_utils:debug_fmt( "CmdStrings = ~p", [ CmdStrings ] ),

	Cmd = text_utils:join( _Sep=" ", CmdStrings ),

	{ Env, PortOpts } = get_execution_settings(),

	case system_utils:run_command( Cmd, Env, _WorkingDir=undefined,
								   PortOpts ) of

		{ _ExitStatus=0, Result } ->
			ChosenNum = text_utils:string_to_integer( Result ),
			list_utils:get_element_at( Designators, ChosenNum );

		{ _ExitStatus=1, _Output=[] } ->
			ui_cancel;

		{ ExitStatus, Output } ->
			throw( { choice_failed, ExitStatus, Output } )

	end.



% Selects, based on an implicit state, using a default prompt, an item among the
% specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item( [ choice_text() ] ) -> choice_index().
choose_numbered_item( Choices ) ->
	choose_numbered_item( Choices, get_state() ).


% Selects, based on an explicit state, using a default prompt, an item among the
% specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
% Selects, based on an implicit state, using the specified prompt, an item among
% the specified ones, and returns its index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item( [ choice_text() ], ui_state() ) ->
									choice_index();
						  ( prompt(), [ choice_element() ] ) -> choice_index().
choose_numbered_item( Choices, UIState )
  when is_record( UIState, term_ui_state ) ->

	Prompt = text_utils:format( "Select among these ~B choices:",
							   [ length( Choices ) ] ),

	choose_numbered_item( Prompt, Choices, UIState );

choose_numbered_item( Prompt, Choices ) ->
	choose_numbered_item( Prompt, Choices, get_state() ).



% Selects, based on an explicit state, using the specified prompt, an item among
% the specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item( prompt(), [ choice_text() ], ui_state() ) ->
									choice_index().
choose_numbered_item( Prompt, Choices, UIState ) ->

	%trace_utils:debug_fmt( "Prompt = ~ts, Choices = ~p", [ Prompt, Choices ] ),

	% We could as well have used a radio list, yet a menu is probably a tad
	% clearer (and selecting the default, initial entry would have no real use
	% here).
	%
	% We reuse choose_designated_item/3 in a hackhish yet very simple way, based
	% on integer indexes:
	%
	ChoiceElements = lists:zip( lists:seq( 1, length( Choices ) ), Choices ),

	case choose_designated_item( Prompt, ChoiceElements, UIState ) of

		ui_cancel ->
			0;

		D ->
			D

	end.




% Selects, based on an implicit state, using a default prompt, an item among the
% specified ones, with a default item being specified, and returns its
% index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item_with_default( [ choice_text() ], choice_index() ) ->
												choice_index().
choose_numbered_item_with_default( Choices, DefaultChoiceText ) ->
	choose_numbered_item_with_default( Choices, DefaultChoiceText,
									   get_state() ).



% Selects, based on an explicit state, using a default prompt, an item among the
% specified ones (specified as direct text, with no specific designator
% provided) and returns its index.
%
% Selects, based on an implicit state, using the specified prompt and default
% item, an item among the specified ones, and returns its index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item_with_default( [ choice_text() ], choice_index(),
										 ui_state() ) -> choice_index();
									   ( prompt(), [ choice_text() ],
										 choice_index() ) -> choice_index().
choose_numbered_item_with_default( Choices, DefaultChoiceText, UIState )
  when is_record( UIState, term_ui_state ) ->

	Prompt = text_utils:format( "Select among these ~B choices:",
							   [ length( Choices ) ] ),

	choose_numbered_item_with_default( Prompt, Choices, DefaultChoiceText,
									   UIState );

choose_numbered_item_with_default( Prompt, Choices, DefaultChoiceText ) ->
	choose_numbered_item_with_default( Prompt, Choices, DefaultChoiceText,
									   get_state() ).



% Selects, based on an explicit state, using the specified prompt and default
% item, an item among the specified ones (specified as direct text, with no
% specific designator provided), and returns its index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item_with_default( prompt(), [ choice_text() ],
			choice_text(), ui_state() ) -> choice_index().
choose_numbered_item_with_default( Prompt, Choices, DefaultChoiceText,
								   UIState ) ->

	% We use a radio list here, to benefit from a default.

	% (very much inspired from choose_numbered_item/3)

	% We reuse choose_designated_item_with_default/4 in a hackhish yet very
	% simple way, based on integer indexes:
	%
	ChoiceElements = lists:zip( lists:seq( 1, length( Choices ) ), Choices ),

	DefaultChoiceIndex =
		case list_utils:get_maybe_index_of( DefaultChoiceText, Choices ) of

		undefined ->
			throw( { default_not_among_choices, DefaultChoiceText, Choices } );

		I ->
			I

	end,

	% Choice designators are simply integers here:
	case choose_designated_item_with_default( Prompt, ChoiceElements,
										DefaultChoiceIndex, UIState ) of

		ui_cancel ->
			0;

		D ->
			D

	end.





% For traces, we attempt to do the same as text_ui, yet with a different
% ui_state() (hence with no code reuse).


% Traces specified message, by displaying it, and possibly logging it, based on
% an implicit state.
%
-spec trace( message() ) -> void().
trace( Message ) ->
	trace( Message, get_state() ).



% Traces specified message, by displaying it, and possibly logging it.
-spec trace( message(), ui_state() ) -> void();
		   ( format_string(), [ term() ] ) -> void().
trace( Message, UIState ) when is_record( UIState, term_ui_state ) ->

	TraceMessage = "[trace] " ++ Message ++ "\n",

	case UIState#term_ui_state.log_console of

		true ->
			trace_utils:info( TraceMessage );

		false ->
			ok

	end,

	case UIState#term_ui_state.log_file of

		undefined ->
			ok;

		LogFile ->
			text_ui:display( LogFile, TraceMessage )

end;

trace( FormatString, Values ) ->
	trace( text_utils:format( FormatString, Values ) ).



% Clears the interface.
-spec clear() -> void().
clear() ->
	clear( get_state() ).


% Clears the interface.
-spec clear( ui_state() ) -> void().
clear( #term_ui_state{ dialog_tool_path=ToolPath } ) ->

	% Simplified example:
	%Cmd = "dialog --clear",

	DialogString = "--clear",

	Cmd = text_utils:join( _Sep=" ", [ ToolPath, DialogString ] ),

	{ Env, PortOpts } = get_execution_settings(),

	case system_utils:run_command( Cmd, Env, _WorkingDir=undefined,
								   PortOpts ) of

		{ _ExitStatus=0, _Output="" } ->
			%trace_utils:debug( "Cleared." ),
			ok;

		{ _ExitStatus=0, Output } ->
			trace_utils:debug_fmt( "Display output: '~ts'.", [ Output ] );

		{ ExitStatus, Output } ->
			throw( { display_error_reported, ExitStatus, Output } )

	end.



% Stops the UI.
-spec stop() -> void().
stop() ->
	stop( get_state() ).



% Stops the UI.
-spec stop( ui_state() ) -> void().
stop( UIState=#term_ui_state{ log_file=undefined } ) ->
	stop_helper( UIState );

stop( UIState=#term_ui_state{ log_file=LogFile } ) ->
	file_utils:write_ustring( LogFile, "Stopping UI.\n" ),
	file_utils:close( LogFile ),
	stop_helper( UIState ).


% (helper)
%stop_helper( #term_ui_state{ state_filename=StateFilename } ) ->
stop_helper( _UIState ) ->

	clear(),

	%file_utils:remove_file_if_existing( StateFilename ),

	process_dictionary:remove( ?ui_state_key ).




% Helper section.


% Tries to find a suitable dialog tool.
-spec lookup_dialog_tool() -> maybe( { dialog_tool(), file_path() } ).
lookup_dialog_tool() ->

	case executable_utils:lookup_executable( "dialog" ) of

		false ->
			% Maybe in the future:
			%AcceptWhiptail = true,
			AcceptWhiptail = false,

			case AcceptWhiptail andalso
				executable_utils:lookup_executable( "whiptail" ) of

				false ->
					undefined;

				WPath ->
					{ whiptail, WPath }

			end;

		DPath ->
			%trace_utils:debug_fmt( "Dialog path: '~ts'.", [ DPath ] ),
			{ dialog, DPath }

	end.



% Sets the current UI state.
%
% (helper)
%
-spec set_state( ui_state() ) -> void().
set_state( UIState ) ->

	%trace_utils:debug_fmt( "Setting as '~ts': ~ts.",
	%					   [ ?ui_state_key, to_string( UIState ) ] ),

	process_dictionary:put( ?ui_state_key, UIState ).



% Returns the current UI state.
%
% (helper)
%
-spec get_state() -> ui_state().
get_state() ->

	case process_dictionary:get( ?ui_state_key ) of

		undefined ->
			throw( term_ui_not_started );

		UIState ->
			UIState

	end.


% Returns the command-line options corresponding to specified table: a settings
% string, a suffix string (dealing with size and redirection for an output
% returned as a return code).
%
-spec get_dialog_settings_for_return_code( setting_table() ) ->
												{ ustring(), ustring() }.
get_dialog_settings_for_return_code( SettingTable ) ->

	{ SettingsString, SuffixString } = get_dialog_base_settings( SettingTable ),

	{ SettingsString, text_utils:format( "~ts ~ts",
					   [ SuffixString, get_redirect_string_for_code() ] ) }.



% Returns the command-line options corresponding to specified table: a settings
% string, a suffix string (dealing with size and redirection for an output
% returned as a temporary file).
%
-spec get_dialog_settings_for_file_return( setting_table() ) ->
					{ ustring(), ustring() }.
get_dialog_settings_for_file_return( SettingTable ) ->

	{ SettingsString, SuffixString } = get_dialog_base_settings( SettingTable ),

	{ SettingsString, text_utils:format( "~ts ~ts",
					   [ SuffixString, get_redirect_string_for_file() ] ) }.



% Returns the base settings for dialog, expected redirection.
get_dialog_base_settings( SettingTable ) ->

	TitleOpt = case ?ui_table:get_value_with_defaults( 'title',
								_Default=undefined, SettingTable ) of

		undefined ->
			"";

		Title ->
			% We prefer having the title surrounded by spaces:
			text_utils:format( "--title ' ~ts '", [ Title ] )

	end,

	BacktitleOpt = case ?ui_table:get_value_with_defaults( 'backtitle',
								   _Default=undefined, SettingTable ) of

		undefined ->
			"";

		Backtitle ->
			text_utils:format( "--backtitle '~ts'", [ Backtitle ] )

	end,

	SettingsOpts = [ TitleOpt, BacktitleOpt ],

	SettingsString = text_utils:join( _Separator=" ", SettingsOpts ),


	% Dialogs look a lot better if not using the maximum dimensions but
	% requesting auto sizing:

	%Height = ?ui_table:get_value( 'max_height', SettingTable ),
	%Width = ?ui_table:get_value( 'max_width', SettingTable ),

	% Auto:
	Height = 0,
	Width = 0,

	SuffixString = text_utils:format( "~B ~B", [ Height, Width ] ),

	{ SettingsString, SuffixString }.



% Returns a string to be used for I/O redirection in an execution command
% relying on exit statuses (hence only for a single positive integer output).
%
-spec get_redirect_string_for_code() -> ustring().
get_redirect_string_for_code() ->
	% As 'nouse_stdio' will be needed:
	"2>&4".


% Returns a string to be used for I/O redirection in an execution command
% relying on a temporary file (hence for any kind of outputs).
%
get_redirect_string_for_file() ->
	"2> " ++ ?temp_file.



% Returns the settings suitable for an execution of the backend.
-spec get_execution_settings() ->
			{ system_utils:environment(), [ system_utils:port_option() ] }.
get_execution_settings() ->

	% Results in having LANG=C:
	Env = system_utils:get_standard_environment(),

	% Finding this combination was really not obvious:
	% (and the VM must be run with -noinput only)
	%
	PortOpts = [ stream, nouse_stdio, exit_status, eof ],

	{ Env, PortOpts }.



% Sets the specified setting to specified value, in the (implicit) UI state.
-spec set_setting( ui_setting_key(), ui_setting_value() ) -> void().
set_setting( SettingKey, SettingValue ) ->
	NewUIState = set_setting( SettingKey, SettingValue, get_state() ),
	set_state( NewUIState ).



% Sets the specified setting to specified value, in the specified UI state.
-spec set_setting( ui_setting_key(), ui_setting_value(), ui_state() ) ->
							ui_state().
set_setting( SettingKey, SettingValue,
			 UIState=#term_ui_state{ settings=SettingTable } ) ->

	NewSettingTable = ?ui_table:add_entry( SettingKey, SettingValue,
										   SettingTable ),

	UIState#term_ui_state{ settings=NewSettingTable }.



% Sets the specified settings to specified values, in the (implicit) UI state.
-spec set_settings( [ ui_setting_entry() ] ) -> void().
set_settings( SettingEntries ) ->
	NewUIState = set_settings( SettingEntries, get_state() ),
	set_state( NewUIState ).



% Sets the specified settings to specified values, in the specified UI state.
-spec set_settings( [ ui_setting_entry() ], ui_state() ) -> ui_state().
set_settings( SettingEntries,
			  UIState=#term_ui_state{ settings=SettingTable } ) ->

	NewSettingTable = ?ui_table:add_entries( SettingEntries, SettingTable ),

	UIState#term_ui_state{ settings=NewSettingTable }.



% Unsets specified setting, in the (implicit) UI state.
-spec unset_setting( ui_setting_key() ) -> void().
unset_setting( SettingKey ) ->
	NewUIState = unset_setting( SettingKey, get_state() ),
	set_state( NewUIState ).


% Unsets specified settings, in the (implicit) UI state.
-spec unset_settings( [ ui_setting_key() ] ) -> void().
unset_settings( SettingKeys ) ->
	NewUIState = unset_settings( SettingKeys, get_state() ),
	set_state( NewUIState ).



% Unsets specified setting, in the specified UI state.
-spec unset_setting( ui_setting_key(), ui_state() ) -> void().
unset_setting( SettingKey,
			   UIState=#term_ui_state{ settings=SettingTable } ) ->

	NewSettingTable = ?ui_table:add_entry( SettingKey, _SettingValue=undefined,
										   SettingTable ),

	UIState#term_ui_state{ settings=NewSettingTable }.


% Unsets specified settings, in the specified UI state.
-spec unset_settings( [ ui_setting_key() ], ui_state() ) -> void().
unset_settings( SettingKeys,
				UIState=#term_ui_state{ settings=SettingTable } ) ->

	NewEntries = [ { S, undefined } || S <- SettingKeys ],
	NewSettingTable = ?ui_table:add_entries( NewEntries, SettingTable ),

	UIState#term_ui_state{ settings=NewSettingTable }.



% Returns the value (if any) associated, in the (implicit) UI state, to the
% specified setting.
%
-spec get_setting( ui_setting_key() ) -> maybe( ui_setting_value() ).
get_setting( SettingKey ) ->
	get_setting( SettingKey, get_state() ).


% Returns the value (if any) associated, in the specified UI state, to the
% specified setting.
%
-spec get_setting( ui_setting_key(), ui_state() ) ->
							maybe( ui_setting_value() ).
get_setting( SettingKey, #term_ui_state{ settings=SettingTable } ) ->
	?ui_table:get_value_with_defaults( SettingKey, _Default=undefined,
									SettingTable ).





% Returns a textual description of the (implicit) UI state.
-spec to_string() -> ustring().
to_string() ->
	to_string( get_state() ).


% Returns a textual description of the specified UI state.
-spec to_string( ui_state() ) -> ustring().
to_string( #term_ui_state{ %state_filename=StateFilename,
						   dialog_tool=DialogTool,
						   dialog_tool_path=DialogToolPath,
						   locale=Locale,
						   log_console=LogConsole,
						   log_file=LogFile,
						   settings=SettingTable } ) ->

	DialogString = text_utils:format( "~ts (found in '~ts')",
									  [ DialogTool, DialogToolPath ] ),

	LocaleString = text_utils:format( "using the ~ts locale", [ Locale ] ),

	ConsoleString = case LogConsole of

		true ->
			"";

		false ->
			"not"

	end,

	FileString = case LogFile of

		undefined ->
			"not using a log file";

		_ ->
			text_utils:format( "using log file '~ts'", [ LogFile ] )

	end,

	SettingString = ui:settings_to_string( SettingTable ),

	%text_utils:format( "term_ui interface, using state file '~ts' "
	%    "for tool ~ts, ~ts, ~ts writing logs on console, ~ts and ~ts",
	text_utils:format( "term_ui interface, using tool ~ts, "
		"~ts, ~ts writing logs on console, ~ts and ~ts",
		[ DialogString, LocaleString, ConsoleString, FileString,
		  SettingString ] ).
