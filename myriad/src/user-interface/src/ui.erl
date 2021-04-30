% Copyright (C) 2016-2021 Olivier Boudeville
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
% Creation date: Saturday, May 5, 2018.


% Aggregates all code transverse to the various actual UI backends.
%
% See:
% - text_ui_test.erl for the test of the most basic text interface
% - term_ui.erl for a more advanced text interface (ncurses-based)
% - gui.erl for a graphical counterpart thereof
%
% See also: trace_utils.erl for another kind of output.
%
-module(ui).



% Usage notes:
%
% By default, this module will do its best to select the most suitable backend,
% i.e. the most advanced among the available ones.
%
% One may instead select, from the command-line, a specific backend thanks to
% the following option: --use-ui-backend BACKEND_NAME, where BACKEND_NAME is the
% name of the associated backend (ex: text_ui, term_ui or gui).
%
% For example: make ui_run CMD_LINE_OPT="--use-ui-backend text_ui"
%
% See examples like merge-tree.escript (merge_utils.erl) allow the user to
% override the actual backend.


% Note that the actual option starts with one extra dash (thus two of them):
-define( ui_backend_opt, '-use-ui-backend' ).


% Usage conventions:

% Now all user interfaces proceed based on screens.

% read_* primitives will loop until having a satisfactory entry specified,
% whereas their get_* counterparts will try only once.

% When offering the user a choice within a range of options, choice designators
% (possibly atoms, like 'do_save' or 'do_exit') can be used (see for example
% choose_designated_item/*) to designate and handle a logical choice made by the
% user regardless of how it was introduced (ex: possibly in different languages,
% according to the locale).
%
% Another possibility is to designate choices based on their index (numerical
% order), see for example choose_numbered_item/*).


% Implementation notes:
%
% Each backend is to store its current state into a specific state record (ex:
% of type term_ui_state() ), kept under a separate, backend-specific key (see
% ui_name_key) in the process dictionary.
%
% Among the fields of these backend records, one is the settings table (see the
% setting_table() type). It allows the developer to specify all kinds of
% settings (ex: default window size), which may or may not be accommodated by a
% given backend.

% There are in the interfaces very similar primitives to deal with internal
% settings, like set/{1,2} and set_setting/{2,3}. They all have a purpose, some
% variations relying on an explicit state, others not.

-include("ui.hrl").


% Describes the interactivity mode of a user interface:
-type interactivity_mode() :: 'batch' | 'interactive'.

-export_type([ interactivity_mode/0 ]).


-export([ start/0, start/1, start/2 ]).


% Directly forwarded section:
-export([ set/1, set/2, unset/1,

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

		  ask_yes_no/1, ask_yes_no/2,

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

		  get_setting/1 ]).


-export([ trace/1, trace/2,
		  clear/0,
		  stop/0,
		  settings_to_string/1 ]).


% Typically text_ui_state() | term_ui_state() | gui_state() | ...
-type ui_state() :: any().


% Shorthands:

-type argument_table() :: shell_utils:argument_table().

-type ustring() :: text_utils:ustring().
-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().



% Starts the UI with default settings.
-spec start() -> void().
start() ->
	start( _Opts=[] ).



% Starts the UI with specified settings, and returns the command-line arguments
% once expurged of any of the known UI-related option (as an argument table).
%
% Stores the corresponding state in the process dictionary.
%
-spec start( ui_options() ) -> argument_table().
start( Options ) ->

	% Here, no argument table is specified, fetching it (thus supposedly not
	% running as an escript):
	%
	start( Options, shell_utils:get_argument_table() ).



% Starts the UI with specified table-based settings, and returns the
% command-line arguments, still as a table, once expurged of any UI-related
% option (as an argument table).
%
% Stores the corresponding state in the process dictionary.
%
-spec start( ui_options(), argument_table() ) -> argument_table().
start( Options, ArgumentTable ) ->

	%trace_utils:debug_fmt( "UI got following full argument(s): ~ts",
	%	   [ shell_utils:argument_table_to_string( ArgumentTable ) ] ),

	% Just a check:
	case process_dictionary:get( ?ui_name_key ) of

		undefined ->
			ok;

		AlreadyUI ->
			throw( { ui_already_started, AlreadyUI } )

	end,

	% With the leading dash removed:
	OptName = ?ui_backend_opt,

	{ BackendModuleName, RemainingArgTable } =
		case shell_utils:extract_command_arguments_for_option( OptName,
												ArgumentTable ) of

		{ undefined, ArgTable } ->
			%trace_utils:debug( "No backend specified, determining it." ),
			{ get_best_ui_backend(), ArgTable };


		{ [ [ BackendName ] ], OtherArgTable } ->

			%trace_utils:debug_fmt( "Following backend was specified: '~ts'.",
			%					   [ BackendName ] ),

			BackendModName = text_utils:string_to_atom( BackendName ),

			case code_utils:is_beam_in_path( BackendModName ) of

				not_found ->
					trace_utils:error_fmt( "No BEAM file found in code path "
						"for user-specified UI backend module '~ts'.",
						[ BackendModName ] ),
					throw( { ui_backend_module_not_found, BackendModName } );

				[ _SinglePath ] ->
					%trace_utils:debug_fmt( "UI backend module found as '~ts'.",
					%					   [ SinglePath ] ),
					{ BackendModName, OtherArgTable };

				MultiplePaths ->
					throw( { multiple_ui_backend_modules_found,
							 MultiplePaths } )

			end;

		{ OtherValues, _OtherArgTable } ->
			throw( { invalid_ui_options, OtherValues } )

	end,

	%trace_utils:debug_fmt( "The '~ts' backend module has been selected.",
	%					   [ BackendModuleName ] ),

	% Expects this backend to register its name and state in the process
	% dictionary:
	%
	BackendModuleName:start( Options ),

	% Pass along the unexploited command-line arguments:
	RemainingArgTable.




% Directly forwarded to the backend section:
% (a parse transform could help)


% Sets specified UI setting.
-spec set( ui_setting_key(), ui_setting_value() ) -> void().
set( SettingKey, SettingValue ) ->
	UIModule = get_backend_name(),
	UIModule:set( SettingKey, SettingValue ).


% Sets specified UI settings.
-spec set( [ ui_setting_entry() ] ) -> void().
set( SettingEntries ) ->
	UIModule = get_backend_name(),
	UIModule:set( SettingEntries ).



% Unsets specified UI setting(s).
-spec unset( [ ui_setting_key() ] | ui_setting_key() ) -> void().
unset( SettingElement ) ->
	UIModule = get_backend_name(),
	UIModule:unset( SettingElement ).



% Displays specified text, as a normal message.
%
% Note: all types of quotes are allowed in the specified text.
%
-spec display( text() ) -> void().
display( Text ) ->
	%trace_utils:debug_fmt( "Displaying '~ts'.", [ Text ] ),
	UIModule = get_backend_name(),
	UIModule:display( Text ).



% Displays specified formatted text, as a normal message.
-spec display( format_string(), format_values() ) -> void().
display( FormatString, Values ) ->
	UIModule = get_backend_name(),
	UIModule:display( FormatString, Values ).


% Displays in-order the items of the specified list, as a normal message.
-spec display_numbered_list( label(), [ text() ] ) -> void().
display_numbered_list( Label, Lines ) ->
	UIModule = get_backend_name(),
	UIModule:display_numbered_list( Label, Lines ).



% Displays specified text, as a warning message.
%
% Note: all types of quotes are allowed in the specified text.
%
-spec display_warning( text() ) -> void().
display_warning( Text ) ->
	UIModule = get_backend_name(),
	UIModule:display( "Warning: " ++ Text ).


% Displays specified formatted text, as a warning message.
-spec display_warning( format_string(), format_values() ) -> void().
display_warning( FormatString, Values ) ->
	UIModule = get_backend_name(),
	UIModule:display( "Warning: " ++ FormatString, Values ).



% Displays specified text, as an error message.
-spec display_error( text() ) -> void().
display_error( Text ) ->
	UIModule = get_backend_name(),
	UIModule:display_error( Text ).



% Displays specified formatted text, as an error message.
-spec display_error( format_string(), format_values() ) -> void().
display_error( FormatString, Values ) ->
	UIModule = get_backend_name(),
	UIModule:display_error( FormatString, Values ).



% Displays in-order the items of the specified list, as an error message.
-spec display_error_numbered_list( label(), [ text() ] ) -> void().
display_error_numbered_list( Label, Lines ) ->
	UIModule = get_backend_name(),
	UIModule:display_error_numbered_list( Label, Lines ).



% Adds a default separation between previous and next content.
-spec add_separation() -> void().
add_separation() ->
	UIModule = get_backend_name(),
	UIModule:add_separation().



% Returns the user-entered text after specified prompt, based on an implicit
% state.
%
% (const)
%
-spec get_text( prompt() ) -> text().
get_text( Prompt ) ->
	UIModule = get_backend_name(),
	UIModule:get_text( Prompt ).


% Returns the user-entered text after specified prompt, based on an explicit
% state.
%
% (const)
%
-spec get_text( prompt(), ui_state() ) -> text().
get_text( Prompt, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:get_text( Prompt, UIState ).



% Returns the user-entered text after specified prompt, once translated to an
% integer, based on an implicit state.
%
% (const)
%
-spec get_text_as_integer( prompt() ) -> text().
get_text_as_integer( Prompt ) ->
	UIModule = get_backend_name(),
	UIModule:get_text_as_integer( Prompt ).


% Returns the user-entered text after specified prompt, once translated to an
% integer, based on an explicit state.
%
% (const)
%
-spec get_text_as_integer( prompt(), ui_state() ) -> text().
get_text_as_integer( Prompt, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:get_text_as_integer( Prompt, UIState ).



% Returns the user-entered text after specified prompt, once translated to an
% integer, based on an implicit state, prompting the user until a valid input is
% obtained.
%
% (const)
%
-spec read_text_as_integer( prompt() ) -> text().
read_text_as_integer( Prompt ) ->
	UIModule = get_backend_name(),
	UIModule:read_text_as_integer( Prompt ).


% Returns the user-entered text after specified prompt, once translated to an
% integer, based on an explicit state, prompting the user until a valid input is
% obtained.
%
% (const)
%
-spec read_text_as_integer( prompt(), ui_state() ) -> text().
read_text_as_integer( Prompt, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:read_text_as_integer( Prompt, UIState ).



% Returns the user-entered text (if any) after specified prompt, once translated
% to (possibly) an integer, based on an implicit state.
%
% (const)
%
-spec get_text_as_maybe_integer( prompt() ) -> maybe( text() ).
get_text_as_maybe_integer( Prompt ) ->
	UIModule = get_backend_name(),
	UIModule:get_text_as_maybe_integer( Prompt ).



% Returns the user-entered text (if any) after specified prompt, once translated
% to (possibly) an integer, based on an explicit state.
%
% (const)
%
-spec get_text_as_maybe_integer( prompt(), ui_state() ) -> maybe( text() ).
get_text_as_maybe_integer( Prompt, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:get_text_as_maybe_integer( Prompt, UIState ).



% Returns the user-entered text after specified prompt, once translated to an
% integer, prompting the user until a valid input is obtained: either a string
% that resolves to an integer (then returned), or an empty string (then
% returning 'undefined'), based on an implicit state.
%
% (const)
%
-spec read_text_as_maybe_integer( prompt() ) -> maybe( text() ).
read_text_as_maybe_integer( Prompt ) ->
	UIModule = get_backend_name(),
	UIModule:read_text_as_maybe_integer( Prompt ).


% Returns the user-entered text after specified prompt, once translated to an
% integer, prompting the user until a valid input is obtained: either a string
% that resolves to an integer (then returned), or an empty string (then
% returning 'undefined'), based on an explicit state.
%
% (const)
%
-spec read_text_as_maybe_integer( prompt(), ui_state() ) -> maybe( text() ).
read_text_as_maybe_integer( Prompt, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:read_text_as_maybe_integer( Prompt, UIState ).



% Displays specified prompt, lets the user choose between two options, 'yes' and
% 'no' (the default option here), and returns that choice.
%
-spec ask_yes_no( prompt() ) -> binary_choice().
ask_yes_no( Prompt ) ->
	ask_yes_no( Prompt, _BinaryDefault=no ).


% Displays specified prompt, lets the user choose between two options, "yes" and
% "no" (with specified default option), and returns that choice.
%
-spec ask_yes_no( prompt(), binary_choice() ) -> binary_choice().
ask_yes_no( Prompt, BinaryDefault ) ->
	UIModule = get_backend_name(),
	UIModule:ask_yes_no( Prompt, BinaryDefault ).



% Selects, using a default prompt, an item among the specified ones (comprising,
% for each, an internal designator and a text), and returns its designator.
%
% Note that the 'ui_cancel' designator atom can also be returned, should the
% user prefer to cancel that operation.
%
% (const)
%
-spec choose_designated_item( [ choice_element() ] ) -> choice_designator().
choose_designated_item( Choices ) ->
	UIModule = get_backend_name(),
	UIModule:choose_designated_item( Choices ).



% Selects, using specified prompt, an item among the specified ones (comprising,
% for each, an internal designator and a text), and returns its designator.
%
% Note that the 'ui_cancel' designator atom can also be returned, should the
% user prefer to cancel that operation.
%
% (const)
%
-spec choose_designated_item( prompt(), [ choice_element() ] ) ->
									choice_designator().
choose_designated_item( Prompt, Choices ) ->
	UIModule = get_backend_name(),
	UIModule:choose_designated_item( Prompt, Choices ).



% Selects, based on an explicit state, using the specified prompt, an item among
% the specified ones (comprising, for each, an internal designator and a text),
% and returns its designator.
%
% Note that the 'ui_cancel' designator atom can also be returned, should the
% user prefer to cancel that operation.
%
% (const)
%
-spec choose_designated_item( prompt(), [ choice_element() ], ui_state() ) ->
									choice_designator().
choose_designated_item( Prompt, Choices, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:choose_designated_item( Prompt, Choices, UIState ).



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
	UIModule = get_backend_name(),
	UIModule:choose_designated_item( Choices, DefaultChoiceDesignator ).



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
	UIModule = get_backend_name(),
	UIModule:choose_designated_item_with_default( Prompt, Choices,
												  DefaultChoiceDesignator ).



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
choose_designated_item_with_default( Prompt, Choices,
								DefaultChoiceDesignator, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:choose_designated_item_with_default( Prompt, Choices,
								DefaultChoiceDesignator, UIState ).




% Selects, based on an implicit state, using a default prompt, an item among the
% specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item( [ choice_element() ] ) -> choice_index().
choose_numbered_item( Choices ) ->
	UIModule = get_backend_name(),
	UIModule:choose_numbered_item( Choices ).



% Selects, based on an explicit state, using a default prompt, an item among the
% specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item( [ choice_element() ], ui_state() ) ->
									choice_index();
						  ( prompt(), [ choice_element() ] ) -> choice_index().
choose_numbered_item( Choices, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:choose_numbered_item( Choices, UIState ).



% Selects, based on an explicit state, using the specified prompt, an item among
% the specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item( prompt(), [ choice_element() ], ui_state() ) ->
									choice_index().
choose_numbered_item( Prompt, Choices, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:choose_numbered_item( Prompt, Choices, UIState ).



% Selects, based on an implicit state, using a default prompt, an item among the
% specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item_with_default( [ choice_element() ],
										 choice_index() ) -> choice_index().
choose_numbered_item_with_default( Choices, DefaultChoiceIndex ) ->
	UIModule = get_backend_name(),
	UIModule:choose_numbered_item_with_default( Choices, DefaultChoiceIndex ).



% Selects, based on an explicit state, using a default prompt, an item among the
% specified ones (specified as direct text, with no specific designator
% provided), and returns its index.
%
% Selects, based on an implicit state, using the specified prompt and default
% item, an item among the specified ones, and returns its index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item_with_default( [ choice_element() ], choice_index(),
										 ui_state() ) -> choice_index();
									   ( prompt(), [ choice_element() ],
										 maybe( choice_index() ) ) ->
												choice_index().
choose_numbered_item_with_default( Choices, DefaultChoiceIndex, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:choose_numbered_item_with_default( Choices, DefaultChoiceIndex,
												UIState ).



% Selects, based on an explicit state, using the specified prompt and default
% item, an item among the specified ones (specified as direct text, with no
% specific designator provided), and returns its index.
%
% Note that index zero can also be returned, corresponding to the 'ui_cancel'
% atom, should the user prefer to cancel that operation.
%
-spec choose_numbered_item_with_default( prompt(), [ choice_element() ],
			maybe( choice_index() ), ui_state() ) -> choice_index().
choose_numbered_item_with_default( Prompt, Choices, DefaultChoiceIndex,
								   UIState ) ->
	UIModule = get_backend_name(),

	UIModule:choose_numbered_item_with_default( Prompt, Choices,
												DefaultChoiceIndex, UIState ).



% Sets the specified setting to the specified value, in the (implicit) UI state.
-spec set_setting( ui_setting_key(), ui_setting_value() ) -> void().
set_setting( SettingKey, SettingValue ) ->
	UIModule = get_backend_name(),
	UIModule:set_setting( SettingKey, SettingValue ).



% Sets the specified setting to the specified value, in the specified UI state.
-spec set_setting( ui_setting_key(), ui_setting_value(), ui_state() ) ->
							ui_state().
set_setting( SettingKey, SettingValue, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:set_setting( SettingKey, SettingValue, UIState ).



% Sets the specified settings to the specified values, in the (implicit) UI
% state.
%
-spec set_settings( [ ui_setting_entry() ] ) -> void().
set_settings( SettingEntries ) ->
	UIModule = get_backend_name(),
	UIModule:set_settings( SettingEntries ).



% Sets the specified settings to the specified values, in the specified UI
% state.
%
-spec set_settings( [ ui_setting_entry() ], ui_state() ) -> ui_state().
set_settings( SettingEntries, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:set_settings( SettingEntries, UIState ).



% Unsets the specified setting, in the (implicit) UI state.
-spec unset_setting( ui_setting_key() ) -> void().
unset_setting( SettingKey ) ->
	UIModule = get_backend_name(),
	UIModule:unset_setting( SettingKey ).



% Unsets the specified settings, in the (implicit) UI state.
-spec unset_settings( [ ui_setting_key() ] ) -> void().
unset_settings( SettingKeys ) ->
	UIModule = get_backend_name(),
	UIModule:unset_settings( SettingKeys ).



% Unsets the specified setting, in the specified UI state.
-spec unset_setting( ui_setting_key(), ui_state() ) -> void().
unset_setting( SettingKey, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:unset_setting( SettingKey, UIState ).


% Unsets the specified settings, in the specified UI state.
-spec unset_settings( [ ui_setting_key() ], ui_state() ) -> void().
unset_settings( SettingKeys, UIState ) ->
	UIModule = get_backend_name(),
	UIModule:unset_settings( SettingKeys, UIState ).


% Returns the value (if any) associated, in the (implicit) UI state, to the
% specified setting.
%
-spec get_setting( ui_setting_key() ) -> maybe( ui_setting_value() ).
get_setting( SettingKey ) ->
	UIModule = get_backend_name(),
	UIModule:get_setting( SettingKey ).



% End of forward section.



% Traces specified status string, by displaying it, and possibly logging it.
-spec trace( ustring() ) -> void().
trace( Message ) ->
	UIModule = get_backend_name(),
	UIModule:trace( Message ).



% Displays and logs specified formatted text.
-spec trace( format_string(), [ term() ] ) -> void().
trace( FormatString, Values ) ->
	UIModule = get_backend_name(),
	UIModule:trace( FormatString, Values ).



% Clears the interface.
-spec clear() -> void().
clear() ->
	UIModule = get_backend_name(),
	UIModule:clear().



% Stops the UI.
-spec stop() -> void().
stop() ->
	UIModuleName = get_backend_name(),
	UIModuleName:stop().



% Returns the module name of the current UI backend.
%
% (helper)
%
-spec get_backend_name() -> basic_utils:module_name().
get_backend_name() ->

	case process_dictionary:get( ?ui_name_key ) of

		undefined ->
			throw( no_backend_started );

		ModName ->
			ModName

	end.



% Returns the most suitable UI backend found, as automatically determined.
%
% By decreasing order of preference: gui, term_ui and text_ui.
%
-spec get_best_ui_backend() -> basic_utils:module_name().
get_best_ui_backend() ->

	_RightResult = case gui:is_available() of

		true ->
			gui;

		false ->
			case term_ui:is_available() of

				true ->
					term_ui;

				false ->
					text_ui

			end

	end,

	% For testing:
	%RiggedResult = text_ui,
	RiggedResult = term_ui,

	%trace_utils:warning_fmt( "Selecting '~ts', as currently hardcoded "
	%	"(should have been '~ts').", [ RiggedResult, RightResult ] ),

	RiggedResult.



% Returns a textual description of the specified setting table.
-spec settings_to_string( setting_table() ) -> ustring().
settings_to_string( SettingTable ) ->
	case ?ui_table:size( SettingTable ) of

		0 ->
			"having no setting recorded";

		Count ->

			SetPairs = lists:sort( ?ui_table:enumerate( SettingTable ) ),

			SetStrings = [ text_utils:format( "'~ts' set to ~p",
									  [ K, V ] ) || { K, V } <- SetPairs ],

			text_utils:format( "with ~B settings recorded: ~ts",
					   [ Count, text_utils:strings_to_string( SetStrings ) ] )

	end.
