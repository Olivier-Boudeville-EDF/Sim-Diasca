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
% Creation date: Wednesday, May 2, 2018.



% This is the most basic, line-based monochrome textual interface, directly in
% raw text, with no cursor control.
%
% See:
% - text_ui_test.erl for the corresponding test
% - gui.erl for a graphical counterpart
%
% See also: trace_utils.erl for another kind of output.
%
-module(text_ui).



% Implementation notes:
%
% In this very specific case, we use the process dictionary to avoid having to
% keep around a UI-state variable in all calls.
%
% So now the UI state is fully implicit; counterpart functions with an explicit
% state used to be also provided (ex: if having a large number of UI operations
% to perform in a row), in which case they were to return an updated state,
% however it proved too cumbersome to define and use.


% Basic UI operations.
-export([ % No is_available/0, as text_ui is deemed always available.

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

		  choose_designated_item/1, choose_designated_item/2,
		  choose_designated_item/3,

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

		  stop/0, stop/1,

		  to_string/0, to_string/1 ]).



-record( text_ui_state, {

		   % Path to our work-around script:
		   get_line_script = file_utils:executable_path(),

		   log_console = false :: boolean(),
		   log_file = undefined :: maybe( file_utils:file() ),

		   settings :: setting_table() } ).


-type ui_state() :: #text_ui_state{}.



% For common, transverse defines:
-include("ui.hrl").


-export_type([ ui_state/0 ]).



% An I/O device, either standard_io, standard_error, a registered name, or a pid
% handling I/O protocols (returned from file:open/2):
%
-type channel() :: io:device().

-define( warning_prefix, "~n[warning] " ).
-define( warning_suffix, "~n" ).

-define( error_prefix, "~n[error] " ).
-define( error_suffix, "~n" ).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type format_string() :: text_utils:format_string().



% Starts the UI with default settings.
%
% Stores the corresponding state in the process dictionary.
%
-spec start() -> void().
start() ->
	start( _Opts=[] ).



% Starts the UI with specified settings.
%
% Stores the corresponding state in the process dictionary.
%
-spec start( ui_options() ) -> void().
start( Options ) ->

	% Cached, as the look-up is a bit demanding:
	GetLineScript = system_utils:get_line_helper_script(),

	BlankUIState = #text_ui_state{ get_line_script=GetLineScript,
								   settings=?ui_table:new() },

	start( Options, BlankUIState ).


% (non-exported helper)
start( _Options=[], UIState ) ->

	% Also a check:
	undefined = process_dictionary:put( ?ui_name_key, ?MODULE ),

	% No prior state expected:
	case process_dictionary:put( ?ui_state_key, UIState ) of

		undefined ->
			ok;

		_ ->
			throw( text_ui_already_started )

	end;

start( _Options=[ log_file | T ], UIState ) ->
	start( [ { log_file, "ui.log" } | T ], UIState );

start( _Options=[ { log_file, Filename } | T ], UIState ) ->

	LogFile = file_utils:open( Filename, [ write, exclusive, raw ] ),

	file_utils:write_ustring( LogFile, "Starting text UI.\n" ),
	NewUIState = UIState#text_ui_state{ log_file=LogFile },
	start( T, NewUIState );

start( _Options=[ log_console | T ], UIState ) ->
	NewUIState = UIState#text_ui_state{ log_console=true },
	start( T, NewUIState );

start( SingleElem, UIState ) ->
	start( [ SingleElem ], UIState ).



% Sets specified UI setting.
-spec set( ui_setting_key(), ui_setting_value() ) -> void().
set( SettingKey, SettingValue ) ->
	set( [ { SettingKey, SettingValue } ] ).



% Sets specified UI settings.
-spec set( [ ui_setting_entry() ] ) -> void().
set( SettingEntries ) ->

	UIState = #text_ui_state{ settings=SettingTable } = get_state(),

	NewSettingTable = ?ui_table:add_entries( SettingEntries, SettingTable ),

	set_state( UIState#text_ui_state{ settings=NewSettingTable } ).



% Unsets specified UI setting.
-spec unset( [ ui_setting_key() ] | ui_setting_key() ) -> void().
unset( SettingKeys ) when is_list( SettingKeys ) ->

	UIState = #text_ui_state{ settings=SettingTable } = get_state(),

	NewSettingTable = ?ui_table:remove_entries( SettingKeys, SettingTable ),

	set_state( UIState#text_ui_state{ settings=NewSettingTable } );

unset( SettingKey ) ->

	UIState = #text_ui_state{ settings=SettingTable } = get_state(),

	NewSettingTable = ?ui_table:remove_entry( SettingKey, SettingTable ),

	set_state( UIState#text_ui_state{ settings=NewSettingTable } ).



% Displays specified text, as a normal message.
-spec display( text() ) -> void().
display( Text ) ->
	display_helper( _Channel=standard_io, Text ).



% Displays specified formatted text, as a normal message.
-spec display( format_string(), [ term() ] ) -> void().
display( FormatString, Values ) ->
	display_helper( _Channel=standard_io, FormatString, Values ).


% Displays in-order the items of specified list, as a normal message.
-spec display_numbered_list( label(), [ text() ] ) -> void().
display_numbered_list( Label, Lines ) ->
	LineStrings = text_utils:strings_to_enumerated_string( Lines ),
	display_helper( _Channel=standard_io, "~ts~ts", [ Label, LineStrings ] ).



% Displays specified text, as a warning message.
-spec display_warning( text() ) -> void().
display_warning( Text ) ->
	display_helper( standard_error,
					?warning_prefix ++ Text ++ ?warning_suffix ).


% Displays specified formatted text, as a warning message.
-spec display_warning( format_string(), [ term() ] ) -> void().
display_warning( FormatString, Values ) ->
	display_helper( standard_error,
		?warning_prefix ++ FormatString ++ ?warning_suffix, Values ).



% Displays specified text, as an error message.
-spec display_error( text() ) -> void().
display_error( Text ) ->
	display_helper( standard_error, ?error_prefix ++ Text ++ ?error_suffix ).


% Displays specified formatted text, as an error message.
-spec display_error( format_string(), [ term() ] ) -> void().
display_error( FormatString, Values ) ->
	display_helper( standard_error,
					?error_prefix ++ FormatString ++ ?error_suffix, Values ).



% Displays in-order the items of specified list, as an error message.
-spec display_error_numbered_list( label(), [ text() ] ) -> void().
display_error_numbered_list( Label, Lines ) ->
	LineStrings = text_utils:strings_to_enumerated_string( Lines ),
	display_helper( _Channel=standard_error,
					?error_prefix ++ "~ts~ts" ++ ?error_suffix,
					[ Label, LineStrings ] ).



% Adds a default separation between previous and next content.
%
% Note: shall not be used, as cannot apply to all backends.
%
-spec add_separation() -> void().
add_separation() ->
	display( _Text="" ).



% Returns the user-entered text, based on an implicit state.
%
% (const)
%
-spec get_text( prompt() ) -> text().
get_text( Prompt ) ->
	get_text( Prompt, _UIState=get_state() ).


% Returns the user-entered text, based on an explicit state.
%
% (const)
%
-spec get_text( prompt(), ui_state() ) -> text().
get_text( Prompt,
		  %_UIState ) ->
		  #text_ui_state{ get_line_script=GetLineScript } ) ->

	%trace_utils:debug( "get_text/2 called." ),

	% If not using our script, will freeze:
	%Read = io:get_line( Prompt ),

	Read = system_utils:get_line( Prompt, GetLineScript ),

	%trace_utils:debug( "get_text/2 read: '~p'.", [ Read ] ),

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

	display( _Text="" ),

	Text = get_text( Prompt, UIState ),

	case text_utils:try_string_to_integer( Text ) of

		undefined ->
			%trace_utils:debug_fmt( "(rejected: '~ts')", [ Text ] ),
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
					read_text_as_integer( Prompt, UIState );

				I ->
					I

			end

	end.



% Selects, using a default prompt, an item among the specified ones, and returns
% its designator.
%
% Note that the 'ui_cancel' atom can also be returned, should the user prefer to
% cancel that operation.
%
% (const)
%
-spec choose_designated_item( [ choice_element() ] ) -> choice_designator().
choose_designated_item( Choices ) ->

	Prompt = text_utils:format( "Select among these ~B choices:",
								[ length( Choices ) ] ),

	choose_designated_item( Prompt, Choices ).



% Selects, using specified prompt, an item among the specified ones, and returns
% its designator.
%
% Note that the 'ui_cancel' atom can also be returned, should the user prefer to
% cancel that operation.
%
% (const)
%
choose_designated_item( Label, Choices ) ->
	choose_designated_item( Label, Choices, get_state() ).



% Selects, based on an explicit state, using the specified label, an item among
% the specified ones, and returns its designator.
%
% Note that the 'ui_cancel' atom can also be returned, should the user prefer to
% cancel that operation.
%
% (const)
%
-spec choose_designated_item( label(), [ choice_element() ], ui_state() ) ->
									choice_designator().
choose_designated_item( Label, Choices, UIState ) ->

	%trace_utils:debug_fmt( "Choices = ~p", [ Choices ] ),

	{ Designators, Texts } = lists:unzip( Choices ),

	case lists:member( ui_cancel, Designators ) of

		true ->
			throw( { disallowed_choice_designator, ui_cancel } );

		false ->
			ok

	end,

	ChoiceCount = length( Choices ),

	{ _FinalCount, NumberedText } = lists:foldl(
					 fun( Text, { Count, AccText } ) ->

						NewText = text_utils:format( "[~B] ~ts",
													 [ Count, Text ] ),

						NewAccText = [ NewText | AccText ],

						{ Count+1, NewAccText }

					 end,
					 _Acc0= { 1, [] },
					 _List=Texts ),

	Text = text_utils:strings_to_string(
			 lists:reverse( NumberedText ), _Bullet=" " ),

	FullLabel = text_utils:format( "~ts~ts~nChoice> ", [ Label, Text ] ),

	case read_text_as_integer( FullLabel, UIState ) of

		{ parsing_failed, Input } ->
			display_error( "Input shall be an integer (not ~ts).",
						   [ Input ] ),
			choose_designated_item( Label, Choices, UIState );


		N when N < 1 ->
			display_error( "Specified choice shall be at least 1 (not ~B).",
						   [ N ] ),
			%throw( { invalid_choice, too_low, N } );
			choose_designated_item( Label, Choices, UIState );

		N when N > ChoiceCount ->
			display_error(
			  "Specified choice shall not be greater than ~B (not ~B).",
			  [ ChoiceCount, N ] ),
			%throw( { invalid_choice, too_high, N } );
			choose_designated_item( Label, Choices, UIState );

		N ->
			lists:nth( N, Designators )

	end.



% Selects, based on an implicit state, using a default label, an item among the
% specified ones, and returns its index.
%
% Note that the 'ui_cancel' atom can also be returned, should the user prefer to
% cancel that operation.
%
-spec choose_numbered_item( [ choice_element() ] ) ->  choice_index().
choose_numbered_item( Choices ) ->
	choose_numbered_item( Choices, get_state() ).


% Selects, based on an explicit state, using a default label, an item among the
% specified ones, and returns its index.
%
% Selects, based on an implicit state, using the specified label, an item among
% the specified ones, and returns its index.
%
% Note that the 'ui_cancel' atom can also be returned, should the user prefer to
% cancel that operation.
%
-spec choose_numbered_item( [ choice_element() ], ui_state() ) ->
									choice_index();
						  ( label(), [ choice_element() ] ) -> choice_index().
choose_numbered_item( Choices, UIState )
  when is_record( UIState, text_ui_state ) ->

	Label = text_utils:format( "Select among these ~B choices:",
								[ length( Choices ) ] ),

	choose_numbered_item( Label, Choices, UIState );

choose_numbered_item( Label, Choices ) ->
	choose_numbered_item( Label, Choices, get_state() ).



% Selects, based on an explicit state, using the specified label, an item among
% the specified ones, and returns its index.
%
% Note that the 'ui_cancel' atom can also be returned, should the user prefer to
% cancel that operation.
%
-spec choose_numbered_item( label(), [ choice_element() ], ui_state() ) ->
									choice_index().
choose_numbered_item( Label, Choices, UIState ) ->

	ChoiceCount = length( Choices ),

	{ _FinalCount, NumberedText } = lists:foldl(
					 fun( Text, { Count, AccText } ) ->

						NewText = text_utils:format( "[~B] ~ts",
													 [ Count, Text ] ),

						NewAccText = [ NewText | AccText ],

						{ Count+1, NewAccText }

					 end,
					 _Acc0= { 1, [] },
					 _List=Choices ),

	Text = text_utils:strings_to_string(
			 lists:reverse( NumberedText ), _Bullet=" " ),

	FullLabel = text_utils:format( "~ts~ts~nChoice> ", [ Label, Text ] ),

	SelectedNumber = get_text_as_integer( FullLabel, UIState ),

	%trace_utils:format( "Selected: ~B", [ SelectedNumber ] ),

	case SelectedNumber of

		N when N < 1 ->
			display_error( "Specified choice shall be at least 1 (not ~B).",
						   [ N ] ),
			%throw( { invalid_choice, too_low, N } );
			choose_numbered_item( Label, Choices, UIState );

		N when N > ChoiceCount ->
			display_error(
			  "Specified choice shall not be greater than ~B (not ~B).",
			  [ ChoiceCount, N ] ),
			%throw( { invalid_choice, too_high, N } );
			choose_numbered_item( Label, Choices, UIState );

		N ->
			N

	end.



% Selects, based on an implicit state, using a default label, an item among the
% specified ones, and returns its index.
%
% Note that the 'ui_cancel' atom can also be returned, should the user prefer to
% cancel that operation.
%
-spec choose_numbered_item_with_default( [ choice_element() ],
										 choice_index() ) -> choice_index().
choose_numbered_item_with_default( Choices, DefaultChoiceIndex ) ->
	choose_numbered_item_with_default( Choices, DefaultChoiceIndex,
									   get_state() ).



% Selects, based on an explicit state, using a default label, an item among the
% specified ones, and returns its index.
%
% Selects, based on an implicit state, using the specified label and default
% item, an item among the specified ones, and returns its index.
%
% Note that the 'ui_cancel' atom can also be returned, should the user prefer to
% cancel that operation.
%
-spec choose_numbered_item_with_default( [ choice_element() ], choice_index(),
										 ui_state() ) -> choice_index();
									   ( label(), [ choice_element() ],
										 maybe( choice_index() ) ) ->
											   choice_index().
choose_numbered_item_with_default( Choices, DefaultChoiceIndex, UIState )
  when is_record( UIState, text_ui_state ) ->

	Label = text_utils:format( "Select among these ~B choices:",
							   [ length( Choices ) ] ),

	choose_numbered_item_with_default( Label, Choices, DefaultChoiceIndex,
									   UIState );

choose_numbered_item_with_default( Label, Choices, DefaultChoiceIndex ) ->
	choose_numbered_item_with_default( Label, Choices, DefaultChoiceIndex,
									   get_state() ).



% Selects, based on an explicit state, using the specified label and default
% item, an item among the specified ones, and returns its index.
%
% Note that the 'ui_cancel' atom can also be returned, should the user prefer to
% cancel that operation.
%
-spec choose_numbered_item_with_default( label(), [ choice_element() ],
			maybe( choice_index() ), ui_state() ) -> choice_index().
choose_numbered_item_with_default( Label, Choices, DefaultChoiceIndex,
								   UIState ) ->

	ChoiceCount = length( Choices ),

	case DefaultChoiceIndex =/= undefined andalso DefaultChoiceIndex > 0
		andalso DefaultChoiceIndex =< ChoiceCount of

		true ->
			ok;

		false ->
			throw( { invalid_default_index, DefaultChoiceIndex } )

	end,

	{ _FinalCount, NumberedText } = lists:foldl(
					 fun( Text, { Count, AccText } ) ->

						NewText = text_utils:format( "[~B] ~ts",
													 [ Count, Text ] ),

						NewAccText = [ NewText | AccText ],

						{ Count+1, NewAccText }

					 end,
					 _Acc0= { 1, [] },
					 _List=Choices ),

	Text = text_utils:strings_to_string(
			 lists:reverse( NumberedText ), _Bullet=" " ),

	FullLabel = text_utils:format( "~ts~ts~nChoice [default: ~B]> ",
								   [ Label, Text, DefaultChoiceIndex ] ),

	case read_text_as_maybe_integer( FullLabel, UIState ) of

		% Default:
		undefined ->
			DefaultChoiceIndex;

		N when N < 1 ->
			display_error( "Specified choice shall be at least 1 (not ~B).",
						   [ N ] ),
			%throw( { invalid_choice, too_low, N } );
			choose_numbered_item_with_default( Label, Choices,
											   DefaultChoiceIndex, UIState );

		N when N > ChoiceCount ->
			display_error( "Specified choice shall not be greater than ~B "
						   "(not ~B).", [ ChoiceCount, N ] ),
			%throw( { invalid_choice, too_high, N } );
			choose_numbered_item_with_default( Label, Choices,
											   DefaultChoiceIndex, UIState );

		N ->
			N

	end.



% Traces specified message, by displaying it, and possibly logging it, based on
% an implicit state.
%
-spec trace( message() ) -> void().
trace( Message ) ->
	trace( Message, get_state() ).



% Traces specified message, by displaying it, and possibly logging it.
-spec trace( message(), ui_state() ) -> void();
		   ( format_string(), [ term() ] ) -> void().
trace( Message, UIState ) when is_record( UIState, text_ui_state ) ->

	%trace_utils:debug_fmt( "UIState: ~p", [ UIState ] ),

	TraceMessage = "[trace] " ++ Message ++ "\n",

	case UIState#text_ui_state.log_console of

		true ->
			display( TraceMessage );

		false ->
			ok

	end,

	case UIState#text_ui_state.log_file of

		undefined ->
			ok;

		LogFile ->
			display( LogFile, TraceMessage )

end;

trace( FormatString, Values ) ->
	trace( text_utils:format( FormatString, Values ) ).




% Stops the UI.
-spec stop() -> void().
stop() ->
	stop( get_state() ).



% Stops the UI.
-spec stop( ui_state() ) -> void().
stop( #text_ui_state{ log_file=undefined } ) ->
	stop_helper();


stop( #text_ui_state{ log_file=LogFile } ) ->
	file_utils:write_ustring( LogFile, "Stopping UI.\n" ),
	file_utils:close( LogFile ),
	stop_helper().


stop_helper() ->
	[ process_dictionary:remove( Key )
	  || Key <- [ ?ui_name_key, ?ui_state_key ] ].



% Helper section.


% Sets the current UI state.
%
% (helper)
%
-spec set_state( ui_state() ) -> void().
set_state( UIState ) ->
	%trace_utils:debug_fmt( "Setting ~p in ", [ UIState, ?ui_state_key ] ),
	process_dictionary:put( ?ui_state_key, UIState ).



% Returns the current UI state.
%
% (helper)
%
-spec get_state() -> ui_state().
get_state() ->

	case process_dictionary:get( ?ui_state_key ) of

		undefined ->
			throw( text_ui_not_started );

		UIState ->
			UIState

	end.


% Displays specified text, on specified channel.
%
% (helper)
%
-spec display_helper( channel(), text() ) -> void().
display_helper( Channel, Text ) ->
	display_helper( Channel, Text, _Values=[] ).



% Displays specified formatted text, on specified channel.
-spec display_helper( channel(), format_string(), [ term() ] ) -> void().
display_helper( Channel, FormatString, Values ) ->

	%trace_utils:debug_fmt( "Displaying, on channel '~p', '~p', with '~p'.",
	%					   [ Channel, FormatString, Values ] ),

	UIState = get_state(),

	case get_setting( backtitle, UIState ) of

		undefined ->
			ok;

		Backtitle ->
			io:format( Channel, "~n~n [Backtitle: ~ts]~n", [ Backtitle ] )

	end,

	case get_setting( title, UIState ) of

		undefined ->
			ok;

		Title ->
			io:format( Channel, "Title:     ~ts~n", [ Title ] )

	end,

	% Otherwise, at least in some cases, '~n' can be literally output:
	String = text_utils:format( FormatString ++ "~n", Values ),
	io:format( Channel, String, [] ).



% Sets the specified setting to specified value, in the (implicit) UI state.
-spec set_setting( ui_setting_key(), ui_setting_value() ) -> void().
set_setting( SettingKey, SettingValue ) ->
	NewUIState = set_setting( SettingKey, SettingValue, get_state() ),
	set_state( NewUIState ).



% Sets the specified setting to specified value, in the specified UI state.
-spec set_setting( ui_setting_key(), ui_setting_value(), ui_state() ) ->
						 ui_state().
set_setting( SettingKey, SettingValue,
			 UIState=#text_ui_state{ settings=SettingTable } ) ->

	NewSettingTable = ?ui_table:add_entry( SettingKey, SettingValue,
										   SettingTable ),

	UIState#text_ui_state{ settings=NewSettingTable }.



% Sets the specified settings to specified values, in the (implicit) UI state.
-spec set_settings( [ ui_setting_entry() ] ) -> void().
set_settings( SettingEntries ) ->
	NewUIState = set_settings( SettingEntries, get_state() ),
	set_state( NewUIState ).



% Sets the specified settings to specified values, in the specified UI state.
-spec set_settings( [ ui_setting_entry() ], ui_state() ) -> ui_state().
set_settings( SettingEntries,
			  UIState=#text_ui_state{ settings=SettingTable } ) ->

	%trace_utils:debug_fmt( "Settings: ~p", [ SettingEntries ] ),

	NewSettingTable = ?ui_table:add_entries( SettingEntries, SettingTable ),

	UIState#text_ui_state{ settings=NewSettingTable }.



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
			   UIState=#text_ui_state{ settings=SettingTable } ) ->

	NewSettingTable = ?ui_table:add_entry( SettingKey, _SettingValue=undefined,
										   SettingTable ),

	UIState#text_ui_state{ settings=NewSettingTable }.



% Unsets specified settings, in the specified UI state.
-spec unset_settings( [ ui_setting_key() ], ui_state() ) -> void().
unset_settings( SettingKeys,
				UIState=#text_ui_state{ settings=SettingTable } ) ->

	NewEntries = [ { S, undefined } || S <- SettingKeys ],
	NewSettingTable = ?ui_table:add_entries( NewEntries, SettingTable ),

	UIState#text_ui_state{ settings=NewSettingTable }.



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
get_setting( SettingKey, #text_ui_state{ settings=SettingTable } ) ->
	?ui_table:get_value_with_defaults( SettingKey, _Default=undefined,
									SettingTable ).



% Returns a textual description of the (implicit) UI state.
-spec to_string() -> ustring().
to_string() ->
	to_string( get_state() ).


% Returns a textual description of the specified UI state.
-spec to_string( ui_state() ) -> ustring().
to_string( #text_ui_state{ get_line_script=GetLineScript,
						   log_console=LogConsole,
						   log_file=LogFile,
						   settings=SettingTable }) ->

	ScriptString = text_utils:format( "relying on helper script '~ts'",
									  [ GetLineScript ] ),

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

	text_utils:format( "text_ui interface, ~ts, ~ts writing logs on console, "
		"~ts and ~ts",
		[ ScriptString, ConsoleString, FileString, SettingString ] ).
