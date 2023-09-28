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
% Creation date: Friday, August 25, 2023.


% @doc Gathering of various facilities for <b>dialog management</b>, for all
% base dialogs.
%
% Covers the 8 following dialog needs:
%  - displaying a message: message_dialog()
%  - offering to select a single choice among a set: single_choice_dialog()
%  - offering to select multiple choices among a set: multi_choice_dialog()
%  - collecting a text from the user: text_entry_dialog()
%  - selecting one or more files: file_selection_dialog()
%  - selecting one or more directories: directory_selection_dialog()
%  - selecting a color: color_selection_dialog()
%  - selecting a font: font_selection_dialog()
%
-module(gui_dialog).


% Dialog-general subsection.

-opaque dialog() :: wxDialog:wxDialog().
% Any type of dialog.
%
% This is a window that can be shown in a modal way (GUI-blocking), or modeless.


-type dialog_return_code() ::
	'ok_returned'
  | 'cancel_returned'
  | 'yes_returned'
  | 'no_returned'.
% A return code obtained when showing a dialog in a modal way.

-export_type([ dialog/0, dialog_return_code/0 ]).



% Dialog-specific subsection.


-opaque message_dialog() :: wxMessageDialog:wxMessageDialog().
% A dialog displaying a message and a few buttons.


-type message_dialog_option() ::
	{ 'caption', caption() }
  | { 'style', [ message_dialog_style() ] }
  | { 'position', point() }.
% An option for the creation of a message dialog.


-type message_dialog_style() ::
	'ok_button'
  | 'cancel_button'
  | 'yes_no_buttons'
  | 'help_button'
  | 'default_is_no'
  | 'default_is_cancel'
  % (alias of default_is_ok) | 'default_is_yes'
  | 'default_is_ok'
  | 'no_icon'
  | 'error_icon'
  | 'warning_icon'
  | 'question_icon'
  | 'information_icon'
  | 'security_icon'
  | 'stay_on_top'
  | 'center'.
% A style element of a message dialog.


-export_type([ message_dialog/0, message_dialog_option/0,
			   message_dialog_style/0 ]).



-opaque single_choice_dialog() :: wxSingleChoiceDialog:wxSingleChoiceDialog().
% A dialog allowing the user to select a single option among a set thereof.
%
% Double-clicking on a list item is equivalent to single-clicking and then
% pressing OK.


-type single_choice_dialog_option() ::
	{ 'style', [ single_choice_dialog_style() ] }
  | { 'position', point() }.
% An option for the creation of a single-choice dialog.


-type single_choice_dialog_style() ::
	'ok_button'
  | 'cancel_button'
  | 'center'.
% A style element of a single-choice dialog.


-export_type([ single_choice_dialog/0, single_choice_dialog_option/0,
			   single_choice_dialog_style/0 ]).



-opaque multi_choice_dialog() :: wxMultiChoiceDialog:wxMultiChoiceDialog().
% A dialog allowing the user to select multiple options from a set thereof.


-type multi_choice_dialog_option() ::
	{ 'style', [ multi_choice_dialog_style() ] }
  | { 'position', point() }.
% An option for the creation of a multiple-choice dialog.


-type multi_choice_dialog_style() ::
	'ok_button'
  | 'cancel_button'
  | 'center'.
% A style element of a multiple-choice dialog.


-export_type([ multi_choice_dialog/0, multi_choice_dialog_option/0,
			   multi_choice_dialog_style/0 ]).



-opaque text_entry_dialog() :: wxTextEntryDialog:wxTextEntryDialog().
% A dialog allowing the user to enter a (line of) text.


-type text_entry_dialog_option() ::
	{ 'caption', caption() }
  | { 'style', [ text_entry_dialog_style() ] }
  | { 'initial_text', text() }
  | { 'position', point() }.
% An option for the creation of a text entry dialog.


-type text_entry_dialog_style() ::
	'ok_button'
  | 'cancel_button'
  | 'center'.
% A style element of a text-entry dialog.


-export_type([ text_entry_dialog/0, text_entry_dialog_option/0,
			   text_entry_dialog_style/0 ]).



-opaque file_selection_dialog() :: wxDirDialog:wxDirDialog().
% A dialog allowing to select a local file.


-type file_selection_dialog_option() ::
	{ 'message', message() }
  | { 'style', [ file_selection_dialog_style() ] }
  | { 'default_dir', any_directory_path() }
  | { 'default_file', any_file_path() }
  | { 'match_filter', match_filter() }
  | { 'position', point() }
  | { 'size', dimensions() }.
% An option for the creation of a file-selection dialog.


-type file_selection_dialog_style() ::
	'open_file'
  | 'save_file'
  | 'confirm_overwrite' % Only with 'save_file'
  | 'follow_no_link'
  | 'only_existing_file' % Only with 'open_file'
  | 'multiple_files' % Only with 'open_file'
  | 'change_working_dir'
  | 'preview_selected'
  | 'show_hidden_files'.
% A style element of a file-selection dialog.

% Regarding selections made with filesystem-related dialogs: typing a filename
% containing wildcards (*, ?) in the filename text item and clicking on OK will
% result in only the files matching the pattern being displayed.


-type match_filter() :: any_string().
% A specification of matching expressions, based on wildcards (*, ?).
%
% Entries are separated with semicolons (";"), and if a pipe ("|") is specified,
% it is ignored together with all preceding characters in this entry.
%
% For example:
% "BMP and GIF files (*.bmp;*.gif)|*.bmp;*.gif|PNG files (*.png)|*.png;*.jpeg".
%
% Typically useful to select which filesystem entries could be candidates for a
% selection.


-export_type([ file_selection_dialog/0, file_selection_dialog_option/0,
			   file_selection_dialog_style/0, match_filter/0 ]).



-opaque directory_selection_dialog() :: wxDirDialog:wxDirDialog().
% A dialog allowing to select a local directory.


-type directory_selection_dialog_option() ::
	{ 'caption', caption() }
  | { 'style', [ message_dialog_style() ] }
  | { 'default_dir', any_directory_path() }
  | { 'position', point() }
  | { 'size', dimensions() }.
% An option for the creation of a directory-selection dialog.


-type directory_selection_dialog_style() ::
	'only_existing_directory' % Reciprocal of enable_directory_creation
  | 'enable_directory_creation' % (the default)
  | 'change_working_dir' % Cannot be used with multiple_directories
  % Not supported currently by wx apparently:
  %| 'multiple_directories'
  | 'show_hidden_directories'.
% A style element of a directory-selection dialog.


-export_type([ directory_selection_dialog/0,
			   directory_selection_dialog_option/0,
			   directory_selection_dialog_style/0 ]).



-opaque color_selection_dialog() :: wxColorDialog:wxColorDialog().
% A dialog allowing to select a color.

% No color_selection_dialog_option() or color_selection_dialog_style() applies.

-export_type([ color_selection_dialog/0 ]).


-opaque font_selection_dialog() :: wxFontDialog:wxFontDialog().
% A dialog allowing to select a font installed on the system, and its size.

% No font_selection_dialog_option() or font_selection_dialog_style() applies.

-export_type([ font_selection_dialog/0 ]).


% For dialogs in general:
-export([ show/1, show_modal/1 ]).

% Message dialogs:
-export([ create_for_message/2, create_for_message/3, destruct_for_message/1 ]).

% Single-choice dialogs:
-export([ create_for_single_choice/4, create_for_single_choice/5,
		  create_for_single_choice/6,
		  set_selected_choice/3, get_choice_designator/2,
		  destruct_for_single_choice/1 ]).

% Multiple-choice dialogs:
-export([ create_for_multi_choice/4, create_for_multi_choice/5,
		  create_for_multi_choice/6,
		  set_selected_choices/3, get_choice_designators/2,
		  destruct_for_multi_choice/1 ]).

% Text-entry dialogs:
-export([ create_for_text_entry/2, create_for_text_entry/3,
		  create_for_text_entry/4,
		  set_default_text/2, get_entered_text/1,
		  destruct_for_text_entry/1 ]).

% File-selection dialogs:
-export([ create_for_file_selection/1, create_for_file_selection/2,
		  get_selected_file/1, get_selected_files/1,
		  destruct_for_file_selection/1 ]).

% Directory-selection dialogs:
-export([ create_for_directory_selection/1, create_for_directory_selection/2,
		  get_selected_directory/1, %get_selected_directories/1,
		  destruct_for_directory_selection/1 ]).

% Color-selection dialogs:
-export([ create_for_color_selection/1, %create_for_color_selection/2,
		  get_selected_color/1,
		  destruct_for_color_selection/1 ]).

% Font-selection dialogs:
-export([ create_for_font_selection/1, get_selected_font/1,
		  destruct_for_font_selection/1 ]).



% Shorthands:

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type file_path() :: file_utils:file_path().
-type any_file_path() :: file_utils:any_file_path().

-type directory_path() :: file_utils:directory_path().
-type any_directory_path() :: file_utils:any_directory_path().

-type any_string() :: text_utils:any_string().

-type text() :: gui_text:text().

-type caption() :: ui:caption().
-type label() :: ui:label().
-type message() :: ui:message().
-type choice_spec() :: ui:choice_spec().
-type choice_designator() :: ui:choice_designator().

-type point() :: gui:point().
-type dimensions() :: gui:dimensions().
-type parent() :: gui:parent().

-type color_by_decimal_with_alpha() :: gui_color:color_by_decimal_with_alpha().

-type font() :: gui_font:font().





% Subsection transverse to all dialogs.


% @doc Shows the specified dialog in a modeless way; returns true if the dialog
% has been shown or hidden, or false if nothing was done because it already was
% in the requested state.
%
% Use show_modal/1 to show it in a modal way.
%
-spec show( dialog() ) -> boolean().
show( Dialog ) ->
	wxDialog:show( Dialog ).


% @doc Shows the specified dialog in a modal way.
%
% Use show/1 to show it in a modeless way.
%
-spec show_modal( dialog() ) -> dialog_return_code().
show_modal( Dialog ) ->
	WxReturnCode = wxDialog:showModal( Dialog ),
	gui_generated:get_first_for_dialog_return( WxReturnCode ).



% Message dialog subsection.


% @doc Creates a dialog displaying the specified message.
-spec create_for_message( message(), parent() ) -> message_dialog().
create_for_message( Message, Parent ) ->
	create_for_message( Message, _Opts=[], Parent ).


% @doc Creates a dialog displaying the specified message, with specified
% options.
%
-spec create_for_message( message(), maybe_list( message_dialog_option() ),
						  parent() ) -> message_dialog().
create_for_message( Message, Opts, Parent ) when is_list( Opts ) ->
	wxMessageDialog:new( Parent, Message, to_wx_message_dialog_opts( Opts ) );

create_for_message( Message, Opt, Parent ) ->
	create_for_message( Message, [ Opt ], Parent ).



% @doc Destructs the specified message dialog.
-spec destruct_for_message( message_dialog() ) -> void().
destruct_for_message( MsgDialog ) ->
	wxMessageDialog:destroy( MsgDialog ).



% Choice-related dialogs.
%
% For selections, rather than relying on basic strings and indexes thereof, we
% prefer relying on choice elements, that are application-friendly logical
% elements (atoms) associated to arbitrary texts (e.g. _ChoiceElement={
% _ChoiceDesignator=my_foobar_elem, _ChoiceText="This is my foo bar element"}).
%
% Then the developer has simply to handle atoms like my_foobar_elem, regardless
% of the content of the associated (possibly translated) text.
%
% Refer to gui_dialog_test.erl for a full example thereof.



% Single-choice dialog subsection.


% @doc Creates a dialog that will offer the user to select a single choice
% option among the specified ones.
%
% No entry is selected initially.
%
% A bit similar in spirit to the {,text_,term_}ui:choose_designated_item/*
% functions.
%
-spec create_for_single_choice( message(), caption(), choice_spec(),
								parent() ) -> single_choice_dialog().
create_for_single_choice( Message, Caption, ChoiceSpec, Parent ) ->
	ChoiceTexts = pair:seconds( ChoiceSpec ),
	wxSingleChoiceDialog:new( Parent, Message, Caption, ChoiceTexts ).


% @doc Creates a dialog that will offer the user to select a single choice
% option among the specified ones.
%
% Noentry  is selected initially.
%
% A bit similar in spirit to the {,text_,term_}ui:choose_designated_item/*
% functions.
%
-spec create_for_single_choice( message(), caption(), choice_spec(),
		maybe_list( single_choice_dialog_option() ), parent() ) ->
			single_choice_dialog().
create_for_single_choice( Message, Caption, ChoiceSpec, DialogOpts, Parent ) ->
	ChoiceTexts = pair:seconds( ChoiceSpec ),
	WxOpts = to_wx_single_choice_dialog_opts( DialogOpts ),
	wxSingleChoiceDialog:new( Parent, Message, Caption, ChoiceTexts, WxOpts ).


% @doc Creates a dialog that will offer the user to select a single choice
% option among the specified ones, with the specified one being selected
% initially.
%
% A bit similar in spirit to the {,text_,term_}ui:choose_designated_item/*
% functions.
%
-spec create_for_single_choice( message(), caption(), choice_spec(),
	choice_designator(), maybe_list( single_choice_dialog_option() ),
	parent() ) -> single_choice_dialog().
create_for_single_choice( Message, Caption, ChoiceSpec,
						  InitialChoiceDesignator, DialogOpts, Parent ) ->
	ChoiceTexts = pair:seconds( ChoiceSpec ),
	WxOpts = to_wx_single_choice_dialog_opts( DialogOpts ),

	Dlg = wxSingleChoiceDialog:new( Parent, Message, Caption, ChoiceTexts,
									WxOpts ),

	set_selected_choice( Dlg, InitialChoiceDesignator, ChoiceSpec ),

	Dlg.


% @doc Sets the initially selected choice option of the specified single-choice
% dialog.
%
-spec set_selected_choice( single_choice_dialog(), choice_designator(),
						   choice_spec() ) -> void().
set_selected_choice( SingleChoiceDialog, ChoiceDesignator, ChoiceSpec ) ->
	Designators = pair:firsts( ChoiceSpec ),
	Index = list_utils:get_index_of( ChoiceDesignator, Designators ),
	wxSingleChoiceDialog:setSelection( SingleChoiceDialog, Index-1 ).


% @doc Returns the designator of the option choice made based on the specified
% single-choice dialog.
%
-spec get_choice_designator( single_choice_dialog(), choice_spec() ) ->
			choice_designator().
get_choice_designator( SingleChoiceDialog, ChoiceSpec ) ->
	Index = wxSingleChoiceDialog:getSelection( SingleChoiceDialog ),
	Designators = pair:firsts( ChoiceSpec ),
	lists:nth( Index+1, Designators ).


% @doc Destructs the specified single-choice dialog.
-spec destruct_for_single_choice( single_choice_dialog() ) -> void().
destruct_for_single_choice( SingleChoiceDialog ) ->
	wxSingleChoiceDialog:destroy( SingleChoiceDialog ).



% Multiple-choice dialog subsection.


% @doc Creates a dialog that will offer the user to select multiple-choice
% options among the specified ones.
%
% None is selected initially.
%
-spec create_for_multi_choice( message(), caption(), choice_spec(),
							   parent() ) -> multi_choice_dialog().
create_for_multi_choice( Message, Caption, ChoiceSpec, Parent ) ->
	ChoiceTexts = pair:seconds( ChoiceSpec ),
	wxMultiChoiceDialog:new( Parent, Message, Caption, ChoiceTexts ).


% @doc Creates a dialog that will offer the user to select multiple-choice
% options among the specified ones.
%
% None is selected initially.
%
-spec create_for_multi_choice( message(), caption(), choice_spec(),
		maybe_list( multi_choice_dialog_option() ), parent() ) ->
										multi_choice_dialog().
create_for_multi_choice( Message, Caption, ChoiceSpec, DialogOpts,
						 Parent ) ->
	ChoiceTexts = pair:seconds( ChoiceSpec ),
	WxOpts = to_wx_multi_choice_dialog_opts( DialogOpts ),
	wxMultiChoiceDialog:new( Parent, Message, Caption, ChoiceTexts, WxOpts ).


% @doc Creates a dialog that will offer the user to select multiple-choice
% options among the specified ones, with the designated ones already selected.
%
-spec create_for_multi_choice( message(), caption(), choice_spec(),
		[ choice_designator() ], maybe_list( multi_choice_dialog_option() ),
		parent() ) -> multi_choice_dialog().
create_for_multi_choice( Message, Caption, ChoiceSpec,
						 InitialChoiceDesignators, DialogOpts, Parent ) ->
	ChoiceTexts = pair:seconds( ChoiceSpec ),
	WxOpts = to_wx_multi_choice_dialog_opts( DialogOpts ),

	Dlg = wxMultiChoiceDialog:new( Parent, Message, Caption, ChoiceTexts,
								   WxOpts ),

	set_selected_choices( Dlg, InitialChoiceDesignators, ChoiceSpec ),

	Dlg.


% @doc Sets the initially selected option choices of the specified
% multiple-choice dialog.
%
-spec set_selected_choices( multi_choice_dialog(), [ choice_designator() ],
							choice_spec() ) -> void().
set_selected_choices( MultiChoiceDialog, ChoiceDesignators, ChoiceSpec ) ->
	Designators = pair:firsts( ChoiceSpec ),
	Indexes = [ list_utils:get_index_of( CD, Designators ) - 1
					|| CD <- ChoiceDesignators ],
	wxMultiChoiceDialog:setSelections( MultiChoiceDialog, Indexes ).


% @doc Returns the designators (in unspecified order) of the choices made based
% on the specified multi-choice dialog.
%
-spec get_choice_designators( multi_choice_dialog(), choice_spec() ) ->
										[ choice_designator() ].
get_choice_designators( MultiChoiceDialog, ChoiceSpec ) ->
	Indexes = wxMultiChoiceDialog:getSelections( MultiChoiceDialog ),
	Designators = pair:firsts( ChoiceSpec ),
	[ lists:nth( Id+1, Designators ) || Id <- Indexes ].


% @doc Destructs the specified multi-choice dialog.
-spec destruct_for_multi_choice( multi_choice_dialog() ) -> void().
destruct_for_multi_choice( MultiChoiceDialog ) ->
	wxMultiChoiceDialog:destroy( MultiChoiceDialog ).



% Text-entry dialog subsection.


% @doc Creates a dialog that will offer the user to enter a text, while
% displaying the specified label.
%
-spec create_for_text_entry( label(), parent() ) -> text_entry_dialog().
create_for_text_entry( Label, Parent ) ->
	wxTextEntryDialog:new( Parent, Label ).


% @doc Creates a dialog that will offer the user to enter a text with the
% specified options, while displaying the specified label.
%
-spec create_for_text_entry( label(), maybe_list( text_entry_dialog_option() ),
							 parent() ) -> text_entry_dialog().
create_for_text_entry( Label, DialogOpts, Parent ) ->
	WxOpts = to_wx_text_entry_dialog_opts( DialogOpts ),
	wxTextEntryDialog:new( Parent, Label, WxOpts ).


% @doc Creates a dialog that will offer the user to enter a text with the
% specified options, while displaying the specified label, and once having set
% the specified initial text.
%
-spec create_for_text_entry( label(), maybe_list( text_entry_dialog_option() ),
							 text(), parent() ) -> text_entry_dialog().
create_for_text_entry( Label, DialogOpts, InitialText, Parent ) ->
	WxOpts = to_wx_text_entry_dialog_opts( DialogOpts ),
	Dlg = wxTextEntryDialog:new( Parent, Label, WxOpts ),
	set_default_text( Dlg, InitialText ),
	Dlg.


% @doc Sets the default, initially present, text of the specified text-entry
% dialog.
%
-spec set_default_text( text_entry_dialog(), text() ) -> void().
set_default_text( TextEntryDialog, Text ) ->
	wxTextEntryDialog:setValue( TextEntryDialog, Text ).


% @doc Returns the text present in the specified text-entry dialog.
-spec get_entered_text( text_entry_dialog() ) -> text().
get_entered_text( TextEntryDialog ) ->
	wxTextEntryDialog:getValue( TextEntryDialog ).


% @doc Destructs the specified text-entry dialog.
-spec destruct_for_text_entry( text_entry_dialog() ) -> void().
destruct_for_text_entry( TextEntryDialog ) ->
	wxTextEntryDialog:destroy( TextEntryDialog ).




% File-selection dialog subsection.


% @doc Creates a dialog that will offer to select a file.
-spec create_for_file_selection( parent() ) -> file_selection_dialog().
create_for_file_selection( Parent ) ->
	wxFileDialog:new( Parent ).


% @doc Creates a dialog that will offer to select a file, based on the specified
% options.
%
-spec create_for_file_selection( maybe_list( file_selection_dialog_option() ),
								 parent() ) -> file_selection_dialog().
create_for_file_selection( DialogOpts, Parent ) ->
	WxOpts = to_wx_file_selection_dialog_opts( DialogOpts ),
	wxFileDialog:new( Parent, WxOpts ).


% getDirectory/1, getFilename/1 and others found of little interest.


% @doc Returns the full path (directory and filename) of the selected file.
%
% The 'multiple_files' option should not have been specified for this dialog.
%
-spec get_selected_file( file_selection_dialog() ) -> file_path().
get_selected_file( FileSelectionDialog ) ->
	wxFileDialog:getPath( FileSelectionDialog ).


% @doc Returns the full path (directory and filename) of the selected files.
%
% The 'multiple_files' option should have been specified for this dialog.
%
-spec get_selected_files( file_selection_dialog() ) -> [ file_path() ].
get_selected_files( FileSelectionDialog ) ->
	wxFileDialog:getPaths( FileSelectionDialog ).


% @doc Destructs the specified file-selection dialog.
-spec destruct_for_file_selection( file_selection_dialog() ) -> void().
destruct_for_file_selection( FileSelectionDialog ) ->
	wxFileDialog:destroy( FileSelectionDialog ).



% Directory-selection dialog subsection.


% @doc Creates a dialog that will offer to select a directory.
-spec create_for_directory_selection( parent() ) ->
										directory_selection_dialog().
create_for_directory_selection( Parent ) ->
	wxDirDialog:new( Parent ).


% @doc Creates a dialog that will offer to select a directory, based on the
% specified options.
%
-spec create_for_directory_selection(
			maybe_list( directory_selection_dialog_option() ), parent() ) ->
		directory_selection_dialog().
create_for_directory_selection( DialogOpts, Parent ) ->
	WxOpts = to_wx_directory_selection_dialog_opts( DialogOpts ),
	wxDirDialog:new( Parent, WxOpts ).


% getDirectory/1 and getDirectoryname/1 and others found of little interest.


% @doc Returns the full path of the selected directory.
%
% The 'multiple_directorys' option should not have been specified for this
% dialog.
%
-spec get_selected_directory( directory_selection_dialog() ) ->
											directory_path().
get_selected_directory( DirSelDialog ) ->
	wxDirDialog:getPath( DirSelDialog ).


% Apparently not currently supported by wx:

% at-doc Returns the full path of the selected directories.
%
% The 'multiple_directories' option should have been specified for this dialog.
%
%-spec get_selected_paths( directory_selection_dialog() ) ->
%           [ directory_path() ].
%get_selected_paths( DirSelDialog ) ->
%   wxDirDialog:getPaths( DirSelDialog ).


% @doc Destructs the specified directory-selection dialog.
-spec destruct_for_directory_selection( directory_selection_dialog() ) ->
											void().
destruct_for_directory_selection( DirSelDialog ) ->
	wxDirDialog:destroy( DirSelDialog ).



% Color-selection dialog subsection.


% @doc Creates a dialog that will offer to select a color.
-spec create_for_color_selection( parent() ) -> color_selection_dialog().
create_for_color_selection( Parent ) ->
	wxColourDialog:new( Parent ).


% At least currently not supporting wxColorData (not useful enough).

% at-doc Creates a dialog that will offer to select a color, based on the
% specified options.
%
%-spec create_for_color_selection(
%           maybe_list( color_selection_dialog_option() ), parent() ) ->
%       color_selection_dialog().
%create_for_color_selection( DialogOpts, Parent ) ->
%   WxOpts = to_wx_color_selection_dialog_opts( DialogOpts ),
%   wxColourDialog:new( Parent, WxOpts ).


% @doc Returns the RGBA definition of the selected color.
-spec get_selected_color( color_selection_dialog() ) ->
								color_by_decimal_with_alpha().
get_selected_color( ColSelDialog ) ->
	ColourData = wxColourDialog:getColourData( ColSelDialog ),
	wxColourData:getColour( ColourData ).


% @doc Destructs the specified color-selection dialog.
-spec destruct_for_color_selection( color_selection_dialog() ) ->
											void().
destruct_for_color_selection( ColSelDialog ) ->
	wxColourDialog:destroy( ColSelDialog ).



% Font-selection dialog subsection.


% @doc Creates a dialog that will offer to select one of the known fonts,
% together with its size.
%
% For example a font described as "Sans Bold 10" may be selected.
%
-spec create_for_font_selection( parent() ) -> font_selection_dialog().
create_for_font_selection( Parent ) ->
	wxFontDialog:new( Parent, _Data=wxFontData:new() ).


% @doc Returns the selected font.
-spec get_selected_font( font_selection_dialog() ) -> font().
get_selected_font( FontSelDialog ) ->
	FontData = wxFontDialog:getFontData( FontSelDialog ),
	wxFontData:getChosenFont( FontData ).


% @doc Destructs the specified font-selection dialog.
-spec destruct_for_font_selection( font_selection_dialog() ) -> void().
destruct_for_font_selection( FontSelDialog ) ->
	wxFontDialog:destroy( FontSelDialog ).





% Dialog helpers.
%
% Defined here to clear the API above.


% @doc Converts the specified option(s) for message dialogs into wx-specific
% ones.
%
-spec to_wx_message_dialog_opts( maybe_list( message_dialog_option() ) ) ->
											list().
to_wx_message_dialog_opts( MsgOpts ) when is_list( MsgOpts ) ->
	Res = [ to_wx_message_dialog_opt( MO ) || MO <- MsgOpts ],
	%trace_utils:debug_fmt( "Wx message dialog options: ~p", [ Res ] ),
	Res;

to_wx_message_dialog_opts( MsgOpt ) ->
	to_wx_message_dialog_opts( [ MsgOpt ] ).



% @doc Converts the specified option for message dialogs into a wx-specific one.
-spec to_wx_message_dialog_opt( message_dialog_option() ) -> tuple().
to_wx_message_dialog_opt( MsgOpt={ caption, _CaptionStr } ) ->
	MsgOpt;

to_wx_message_dialog_opt( _MsgOpt={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		gui_generated:get_second_for_message_dialog_style( S ) bor Acc end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle };

to_wx_message_dialog_opt( _MsgOpt={ position, Pos } ) ->
	{ pos, Pos }.



% @doc Converts the specified option(s) for single-choice dialogs into
% wx-specific ones.
%
-spec to_wx_single_choice_dialog_opts(
						maybe_list( single_choice_dialog_option() ) ) -> list().
to_wx_single_choice_dialog_opts( ScdOpts ) when is_list( ScdOpts ) ->
	Res = [ to_wx_single_choice_dialog_option( SO ) || SO <- ScdOpts ],
	%trace_utils:debug_fmt( "Wx single-choice dialog options: ~p", [ Res ] ),
	Res;

to_wx_single_choice_dialog_opts( ScdOpt ) ->
	to_wx_single_choice_dialog_opts( [ ScdOpt ] ).


% @doc Converts the specified option for single-choice dialogs into a
% wx-specific one.
%
-spec to_wx_single_choice_dialog_option( single_choice_dialog_option() ) ->
											tuple().
to_wx_single_choice_dialog_option( _ScdOpt={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		gui_generated:get_second_for_single_choice_dialog_style( S ) bor Acc
						   end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle };

to_wx_single_choice_dialog_option( _ScdOpt={ position, Pos } ) ->
	{ pos, Pos }.



% @doc Converts the specified option(s) for multiple-choice dialogs into
% wx-specific ones.
%
-spec to_wx_multi_choice_dialog_opts(
						maybe_list( multi_choice_dialog_option() ) ) -> list().
to_wx_multi_choice_dialog_opts( MultOpts ) when is_list( MultOpts ) ->
	[ to_wx_multi_choice_dialog_option( MO ) || MO <- MultOpts ];

to_wx_multi_choice_dialog_opts( MultOpt ) ->
	to_wx_multi_choice_dialog_opts( [ MultOpt ] ).


% @doc Converts the specified option for multi-choice dialogs into a
% wx-specific one.
%
-spec to_wx_multi_choice_dialog_option( multi_choice_dialog_option() ) ->
											tuple().
to_wx_multi_choice_dialog_option( _MultOpt={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		gui_generated:get_second_for_multi_choice_dialog_style( S ) bor Acc
						   end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle };

to_wx_multi_choice_dialog_option( _MultOpt={ position, Pos } ) ->
	{ pos, Pos }.




% @doc Converts the specified option(s) for text-entry dialogs into wx-specific
% ones.
%
-spec to_wx_text_entry_dialog_opts(
			maybe_list( text_entry_dialog_option() ) ) -> list().
to_wx_text_entry_dialog_opts( TextEntryOpts ) when is_list( TextEntryOpts ) ->
	[ to_wx_text_entry_dialog_option( TEO ) || TEO <- TextEntryOpts ];

to_wx_text_entry_dialog_opts( TextEntryOpt ) ->
	to_wx_text_entry_dialog_opts( [ TextEntryOpt ] ).


% @doc Converts the specified option for text-entry dialogs into a wx-specific
% one.
%
-spec to_wx_text_entry_dialog_option( text_entry_dialog_option() ) -> tuple().
to_wx_text_entry_dialog_option( TextEntryOpt={ caption, _CaptionStr } ) ->
	TextEntryOpt;

to_wx_text_entry_dialog_option( _TextEntryOpt={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		gui_generated:get_second_for_text_entry_dialog_style( S ) bor Acc end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle };

to_wx_text_entry_dialog_option( _TextEntryOpt={ initial_text, InitialText } ) ->
	{ value, InitialText };

to_wx_text_entry_dialog_option( _TextEntryOpt={ position, Pos } ) ->
	{ pos, Pos }.



% @doc Converts the specified option(s) for file-selection dialogs into
% wx-specific ones.
%
-spec to_wx_file_selection_dialog_opts(
				maybe_list( file_selection_dialog_option() ) ) -> list().
to_wx_file_selection_dialog_opts( FileSelOpts ) when is_list( FileSelOpts ) ->
	[ to_wx_file_selection_dialog_option( FSO ) || FSO <- FileSelOpts ];

to_wx_file_selection_dialog_opts( FileSelOpt ) ->
	to_wx_file_selection_dialog_opts( [ FileSelOpt ] ).


% @doc Converts the specified option for file-selection dialogs into a
% wx-specific one.
%
-spec to_wx_file_selection_dialog_option( file_selection_dialog_option() ) ->
											tuple().
to_wx_file_selection_dialog_option( FileSelOpt={ message, _CaptionStr } ) ->
	FileSelOpt;

to_wx_file_selection_dialog_option( _FileSelOpt={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		gui_generated:get_second_for_file_selection_dialog_style( S )
			bor Acc end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle };

to_wx_file_selection_dialog_option( _FileSelOpt={ default_dir, DefDir } ) ->
	{ defaultDir, DefDir };

to_wx_file_selection_dialog_option( _FileSelOpt={ default_file, DefFile } ) ->
	{ defaultFile, DefFile };

to_wx_file_selection_dialog_option(
		_FileSelOpt={ match_filter, MatchFilter } ) ->
	{ wildCard, MatchFilter };

to_wx_file_selection_dialog_option( _FileSelOpt={ position, Pos } ) ->
	{ pos, Pos };

to_wx_file_selection_dialog_option( _FileSelOpt={ size, Size } ) ->
	{ sz, Size }.



% @doc Converts the specified option(s) for directory-selection dialogs into
% wx-specific ones.
%
-spec to_wx_directory_selection_dialog_opts(
				maybe_list( directory_selection_dialog_option() ) ) -> list().
to_wx_directory_selection_dialog_opts( DirSelOpts )
											when is_list( DirSelOpts ) ->
	[ to_wx_directory_selection_dialog_option( DSO ) || DSO <- DirSelOpts ];

to_wx_directory_selection_dialog_opts( DirSelOpt ) ->
	to_wx_directory_selection_dialog_opts( [ DirSelOpt ] ).


% @doc Converts the specified option for directory-selection dialogs into a
% wx-specific one.
%
-spec to_wx_directory_selection_dialog_option(
						directory_selection_dialog_option() ) -> tuple().
to_wx_directory_selection_dialog_option( _DirSelOpt={ caption, CaptionStr } ) ->
	{ title, CaptionStr };

to_wx_directory_selection_dialog_option( _DirSelOpt={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		gui_generated:get_second_for_directory_selection_dialog_style( S )
			bor Acc end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle };

to_wx_directory_selection_dialog_option(
		_FileSelOpt={ default_dir, DefDir } ) ->
	{ defaultPath, DefDir };

to_wx_directory_selection_dialog_option( _DirSelOpt={ position, Pos } ) ->
	{ pos, Pos };

to_wx_directory_selection_dialog_option( _DirSelOpt={ size, Size } ) ->
	{ sz, Size }.
