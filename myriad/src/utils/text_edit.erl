% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Friday, November 22, 2024.

-module(text_edit).

-moduledoc """
Provides facilities to edit any kind of text made of a series of entries
generically, with cursor control, shortcuts, etc.

At least currently, this consists in managing a (single) entry (logical line)
thanks to higher-level operations.

This may be applied for example to shell commands or chat clients (like IRC).

Allows typically to uncouple an input/output source (from the command-line, a
dedicated GUI, a web page, etc.; typically, in MVC parlance, a component in
charge of a View and of a Controller) from the management of the text editing by
itself (this module) and the processing of that text (the Model, e.g. the
processing of the corresponding entries, like done by the shell_utils
module for shell commands).

See the gui_shell and shell_utils modules for an example of use thereof.
""".


% For the text_edit record:
-include("text_edit.hrl").


-doc "The state of a general-purpose text edition facility.".
-type text_edit() :: #text_edit{}.


-doc """
The available options when creating a text edit.
""".
-type text_edit_option() ::

	% Whether a trailing dot should be automatically added if lacking in an
	% command (convenient in the context of single-line edition):
	%
	'auto_add_trailing_dot'

	% Whether the entry cursor should wrap around:
 | 'wrap_cursor'.



-doc """
An index of a position in the text; starts at 1.
""".
-type char_pos() :: pos_integer().


-doc """
A (signed) offset in terms of character position.
""".
-type offset_char_pos() :: integer().


-type prefix_info() :: { ustring(), length() }.


% Entries are to be understood here as a generalisation of commands:

-doc "Any kind of entry (e.g. a shell command).".
-type entry() :: bin_string().

-doc "Any kind of entry (e.g. a shell command), as a string.".
-type entry_str() :: ustring().


-doc """
The number of an entry (in their history), which is an identifier thereof.
""".
-type entry_id() :: count().


-doc "A text that could be suffixed to the current entry.".
-type completion() :: bin_string().


-doc "The PID of a process in charge of managing each validated entry.".
-type processor_pid() :: pid().


-doc "The result of the processing of an entry, as evaluated by a processor.".
-type process_result() :: any().


-doc "An error message generated when a processor evaluates a submitted entry.".
-type process_error_reason() :: bin_string().


-doc "The information returned once an entry has been processed.".
-type process_outcome() ::

	{ 'processing_success', process_result(), NewCurrentEntryId :: entry_id(),
	  MaybeTimestampBinStr :: option( timestamp_binstring() ) }

  | { 'processing_error', process_error_reason(),
	  NewCurrentEntryId :: entry_id(),
	  MaybeTimestampBinStr :: option( timestamp_binstring() ) }.




-export_type([ text_edit/0, text_edit_option/0,

			   char_pos/0, offset_char_pos/0, prefix_info/0,
			   entry/0, entry_str/0, entry_id/0,

			   processor_pid/0, process_result/0, process_error_reason/0,
			   process_outcome/0 ]).



-export([ create/4, create/5, filter_options/1,
		  recall_previous_entry/1, recall_next_entry/1,
		  delete_current_char/1, delete_previous_char/1,
		  get_completions/1,
		  to_string/1,
		  destruct/1 ]).



% Read-only accessors.
-export([ get_entry_id/1, get_prefix/1, get_prefix_length/1,
		  get_cursor_position/1,
		  get_entry/1, get_bin_entry/1,
		  get_full_text/1, get_full_text_with_cursor/1,
		  get_entry_for_submission/1  ]).


% Operations.
-export([ set_prefix/2, set_entry/2, add_char/2, add_char_after/2,
		  append_string/2, append_string_truncate/2,
		  move_cursor_left/1, move_cursor_right/1,
		  set_cursor_to_start_of_line/1, set_cursor_to_end_of_line/1,
		  kill_from_cursor/1, restore_previous_line/1, clear/1, process/1 ]).



% Type shorthands:

-type count() :: basic_utils:count().
-type any_option() :: basic_utils:any_option().

-type length() :: text_utils:length().
-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type uchar() :: text_utils:uchar().

-type timestamp_binstring() :: time_utils:timestamp_binstring().

-type text() :: ui:text().



-doc """
Creates a text edit, with its processor, next entry identifier, specific options
and no prefix.
""".
-spec create( processor_pid(), entry_id(), boolean(), boolean() ) ->
											text_edit().
create( ProcessorPid, NextEntryId, AutoAddTrailingDot, WrapCursor  ) ->
	create( ProcessorPid, NextEntryId, AutoAddTrailingDot, WrapCursor,
			_Prefix="" ).



-doc """
Creates a text edit, with its processor, next entry identifier, specific options
and entry prefix.
""".
% Creation is a bit convoluted as the processor cannot be created here (as is
% arbitrary):
-spec create( processor_pid(), entry_id(), boolean(), boolean(), ustring() ) ->
											text_edit().
create( ProcessorPid, NextEntryId, AutoAddTrailingDot, WrapCursor, Prefix ) ->
	#text_edit{ entry_id=NextEntryId,
				prefix=Prefix,
				prefix_len=length( Prefix ),
				precursor_chars=[],
				postcursor_chars=[],
				auto_add_trailing_dot=AutoAddTrailingDot,
				wrap_cursor=WrapCursor,
				hist_entry_id=undefined,
				current_entry=undefined,
				processor_pid=ProcessorPid }.



-doc """
Returns the text edit settings found in the specified options, and returns them
together with the remaining options (possibly processor-specific ones).
""".
-spec filter_options( [ any_option() ] ) ->
	{ AutoAddTrailingDot :: boolean(), WrapCursor :: boolean(),
	  [ any_option() ] }.
filter_options( FullOpts ) ->

	{ AutoAddTrailingDot, FirstOpts } =
			case list_utils:extract_element_if_existing(
				_AutoElem=auto_add_trailing_dot, FullOpts ) of

		false ->
			{ false, FullOpts };

		OtherAutoOpts ->
			{ true,  OtherAutoOpts }

	end,

	{ WrapCursor, SecondOpts } =
			case list_utils:extract_element_if_existing(
				_WrapElem=wrap_cursor, FirstOpts ) of

		false ->
			{ false, FirstOpts };

		OtherWrapOpts ->
			{ true, OtherWrapOpts }

	end,

	{ AutoAddTrailingDot, WrapCursor, SecondOpts }.



-doc "Destructs the specified text edit.".
-spec destruct( text_edit() ) -> void().
destruct( #text_edit{ processor_pid=ProcessorPid } ) ->
	ProcessorPid ! terminate.



% Read-only accessors.


-doc """
Returns the identifier of the entry being currently edited (hence that will be
the "next" one.
""".
-spec get_entry_id( text_edit() ) -> entry_id().
get_entry_id( #text_edit{ entry_id=EntryId } ) ->
	EntryId.


-doc "Returns the entry prefix.".
-spec get_prefix( text_edit() ) -> ustring().
get_prefix( #text_edit{ prefix=Pfx } ) ->
	Pfx.


-doc "Returns the number of characters of the entry prefix.".
-spec get_prefix_length( text_edit() ) -> length().
get_prefix_length( #text_edit{ prefix_len=PfxLen } ) ->
	PfxLen.


-doc "Returns the current cursor position.".
-spec get_cursor_position( text_edit() ) -> char_pos().
get_cursor_position( #text_edit{ prefix_len=PfxLen,
								 precursor_chars=PreChars }  ) ->
	%trace_utils:debug_fmt( "PfxLen = ~p.", [ PfxLen ] ),
	% As positions start at 1:
	PfxLen + length( PreChars ) + 1.


-doc "Returns the currently edited entry (with no prefix taken into account).".
-spec get_entry( text_edit() ) -> text().
get_entry( #text_edit{ precursor_chars=PreChars,
					   postcursor_chars=PostChars } ) ->
	lists:reverse( PreChars ) ++ PostChars.


-doc "Returns the currently edited entry (with no prefix), as a binary string.".
-spec get_bin_entry( text_edit() ) -> bin_string().
get_bin_entry( TE ) ->
	text_utils:ensure_binary( get_entry( TE ) ).



-doc "Returns the current, full (prefix included) edited text.".
-spec get_full_text( text_edit() ) -> text().
get_full_text( TE=#text_edit{ prefix=Pfx } ) ->
	Pfx ++ get_entry( TE ).


-doc """
Returns the current, full (prefix included) edited text, together with the
cursor position.
""".
-spec get_full_text_with_cursor( text_edit() ) -> { text(), char_pos() }.
get_full_text_with_cursor( TE=#text_edit{ prefix_len=PfxLen,
										  precursor_chars=PreChars } ) ->
	{ get_full_text( TE ), PfxLen + length( PreChars ) + 1 }.



-doc """
Returns the currently edited entry in a final form, ready to be submitted (hence
with no prefix, as a binary string, with all options applied.
""".
-spec get_entry_for_submission( text_edit() ) -> bin_string().
get_entry_for_submission( #text_edit{ precursor_chars=PreChars,
									  postcursor_chars=PostChars,
									  auto_add_trailing_dot=AutoAddTrailingDot
									} ) ->

	InitCmdStr = lists:reverse( PreChars ) ++ PostChars,

	% Adding a last '.' iff enabled and needed:
	CmdStr = case AutoAddTrailingDot of

		true ->
			case lists:reverse( InitCmdStr ) of

				[ $. | _T ] ->
					InitCmdStr;

				RevCmdStr ->
					lists:reverse( [ $. | RevCmdStr ] )

			end;

		false ->
			InitCmdStr

	end,

	text_utils:string_to_binary( CmdStr ).






% Operations.
%
% Some of them may return 'unchanged' so that no update is triggered in the
% caller.
%
% Note that operations that do not modify the edited text (e.g. moving the
% cursor) are not cannot be restored (Ctrl-z). The others have to backup in all
% cases both the pre *and* post chars (even if only one of them is modified),
% otherwise, as any cursor movement is possible in-between, the restore would be
% faulty in the general case.


-doc "Sets the specified prefix of the specified text edit.".
-spec set_prefix( text_edit(), prefix_info() ) -> text_edit().
set_prefix( TE, _PrefixInfo={ Pfx, PfxLen } ) ->
	TE#text_edit{ prefix=Pfx, prefix_len=PfxLen }.


-doc "Sets the currently edited entry (with no prefix taken into account).".
-spec set_entry( text_edit(), text() ) -> text_edit().
set_entry( TE=#text_edit{ precursor_chars=PreChars,
						  postcursor_chars=PostChars }, Text ) ->

	TextStr = text_utils:ensure_string( Text ),

	TE#text_edit{ precursor_chars=lists:reverse( TextStr ),
				  prev_precursor_chars=PreChars,
				  prev_postcursor_chars=PostChars }.



-doc """
Adds the specified character to the current text, shifting the cursor
accordingly.
""".
-spec add_char( uchar(), text_edit() ) -> text_edit().
add_char( NewChar, TE=#text_edit{ precursor_chars=PreChars,
								  postcursor_chars=PostChars } ) ->

	%trace_utils:debug_fmt( "(adding '~ts')", [ [ NewChar ] ] ),

	NewPreChars = [ NewChar | PreChars ],

	TE#text_edit{ precursor_chars=NewPreChars,
				  prev_precursor_chars=PreChars,
				  prev_postcursor_chars=PostChars }.


-doc """
Adds the specified character to the current text, leaving the cursor as was.
""".
-spec add_char_after( uchar(), text_edit() ) -> text_edit().
add_char_after( NewChar, TE=#text_edit{ precursor_chars=PreChars,
										postcursor_chars=PostChars } ) ->

	%trace_utils:debug_fmt( "(adding '~ts' after)", [ [ NewChar ] ] ),

	TE#text_edit{ postcursor_chars=[ NewChar | PostChars ],
				  prev_precursor_chars=PreChars,
				  prev_postcursor_chars=PostChars }.



-doc """
Appends the specified string to the text at the current cursor position,
shifting the cursor to the end of this addition, and preserving after the text
of that was on the right of that cursor.
""".
-spec append_string( ustring(), text_edit() ) -> text_edit().
append_string( SuffixStr, TE=#text_edit{ precursor_chars=PreChars,
										 postcursor_chars=PostChars } ) ->

	NewPreChars = lists:reverse( SuffixStr ) ++ PreChars,

	TE#text_edit{ precursor_chars=NewPreChars,
				  prev_precursor_chars=PreChars,
				  prev_postcursor_chars=PostChars }.


-doc """
Appends the specified string to the text at the current cursor position,
shifting the cursor to the end of this addition, and removing all the text that
was on the right of that cursor.
""".
-spec append_string_truncate( ustring(), text_edit() ) -> text_edit().
append_string_truncate( SuffixStr, TE=#text_edit{ precursor_chars=PreChars,
										postcursor_chars=PostChars } ) ->

	NewPreChars = lists:reverse( SuffixStr ) ++ PreChars,

	TE#text_edit{ precursor_chars=NewPreChars,
				  postcursor_chars=[],
				  prev_precursor_chars=PreChars,
				  prev_postcursor_chars=PostChars }.



-doc "Moves the cursor one character on the left, if possible.".
-spec move_cursor_left( text_edit() ) -> 'unchanged' | text_edit().
% Here already at the leftmost position, and cursor wrap wanted:
move_cursor_left( TE=#text_edit{ precursor_chars=[],
								 postcursor_chars=PostChars,
								 wrap_cursor=true } ) ->
	TE#text_edit{ precursor_chars=lists:reverse( PostChars ),
				  postcursor_chars=[] };

% Same, but no wrapping enabled, so no change:
move_cursor_left( #text_edit{ precursor_chars=[] } ) ->
	unchanged;

% General case, not at end:
move_cursor_left( TE=#text_edit{ precursor_chars=[ PrevChar | T ],
								 postcursor_chars=PostChars } ) ->

	NewPreChars = T,
	NewPostChars = [ PrevChar | PostChars ],

	TE#text_edit{ precursor_chars=NewPreChars,
				  postcursor_chars=NewPostChars }.




-doc "Moves the cursor one character on the right, if possible.".
-spec move_cursor_right( text_edit() ) -> 'unchanged' | text_edit().
% Here already at the rightmost position, and cursor wrap wanted:
move_cursor_right( TE=#text_edit{ precursor_chars=PreChars,
								  postcursor_chars=[],
								  wrap_cursor=true } ) ->
	%trace_utils:debug( "To right but already at end; wrapping." ),
	TE#text_edit{ precursor_chars=[],
				  postcursor_chars=lists:reverse( PreChars ) };

% Same, but no wrapping enabled, so no change:
move_cursor_right( #text_edit{ postcursor_chars=[] } ) ->
	%trace_utils:debug( "To right but already at end; not wrapping." ),
	unchanged;

% General case, not at end:
move_cursor_right( TE=#text_edit{ precursor_chars=PreChars,
								  postcursor_chars=[ NextChar | T ] } ) ->

	NewPreChars = [ NextChar | PreChars ],
	NewPostChars = T,

	TE#text_edit{ precursor_chars=NewPreChars,
				  postcursor_chars=NewPostChars }.



-doc "Moves the cursor to the leftmost editable position.".
-spec set_cursor_to_start_of_line( text_edit() ) -> text_edit().
set_cursor_to_start_of_line( TE=#text_edit{ precursor_chars=PreChars,
											postcursor_chars=PostChars } ) ->

	NewPreChars = [],
	NewPostChars = lists:reverse( PreChars ) ++ PostChars,

	TE#text_edit{ precursor_chars=NewPreChars,
				  postcursor_chars=NewPostChars }.



-doc "Moves the cursor to the rightmost editable position.".
-spec set_cursor_to_end_of_line( text_edit() ) -> text_edit().
set_cursor_to_end_of_line( TE=#text_edit{ precursor_chars=PreChars,
										  postcursor_chars=PostChars } ) ->

	NewPreChars = lists:reverse( PostChars ) ++ PreChars,
	NewPostChars = [],

	TE#text_edit{ precursor_chars=NewPreChars,
				  postcursor_chars=NewPostChars }.



-doc "Kills all characters from the cursor to the end of line.".
-spec kill_from_cursor( text_edit() ) -> text_edit().
kill_from_cursor( TE=#text_edit{ precursor_chars=PreChars,
								 postcursor_chars=PostChars } ) ->
	TE#text_edit{ postcursor_chars=[],
				  prev_precursor_chars=PreChars,
				  prev_postcursor_chars=PostChars }.



-doc "Restores the previous edited line.".
-spec restore_previous_line( text_edit() ) -> text_edit().
restore_previous_line( TE=#text_edit{ precursor_chars=PreChars,
									  postcursor_chars=PostChars,
									  prev_precursor_chars=PrevPreChars,
									  prev_postcursor_chars=PrevPostChars } ) ->
	% Swaps:
	TE#text_edit{ precursor_chars=PrevPreChars,
				  postcursor_chars=PrevPostChars,
				  prev_precursor_chars=PreChars,
				  prev_postcursor_chars=PostChars }.



-doc "Clears the current text.".
-spec clear( text_edit() ) -> text_edit().
clear( TE=#text_edit{ precursor_chars=PreChars,
					  postcursor_chars=PostChars } ) ->
	TE#text_edit{ precursor_chars=[],
				  postcursor_chars=[],
				  prev_precursor_chars=PreChars,
				  prev_postcursor_chars=PostChars }.



-doc "Processes the current text.".
-spec process( text_edit() ) -> process_outcome().
process( TE=#text_edit{ processor_pid=ProcessorPid } ) ->

	EntryBinStr = get_entry_for_submission( TE ),

	cond_utils:if_defined( myriad_debug_text_edit,
		trace_utils:debug_fmt( "Validation triggered for entry '~ts' (#~B) "
			"on processor ~w.",
			[ EntryBinStr, TE#text_edit.entry_id, ProcessorPid ] ) ),

	ProcessorPid ! { processEntry, EntryBinStr, self() },

	ResetTE = clear( TE ),

	receive

		% Receiving ProcessOutcome:
		{ processing_success, ProcessResult, NewCurrentEntryId,
		  MaybeTimestampBinStr } ->
			NewTE = ResetTE#text_edit{ entry_id=NewCurrentEntryId,
									   hist_entry_id=undefined },

			cond_utils:if_defined( myriad_debug_text_edit,
				trace_utils:debug_fmt( "Next entry: #~B.",
									   [ NewCurrentEntryId ] ) ),

			% A different atom, to be clearer:
			{ success, NewTE, EntryBinStr, ProcessResult,
			  MaybeTimestampBinStr };


		{ processing_error, ReasonBinStr, NewCurrentEntryId,
		  MaybeTimestampBinStr } ->
			NewTE = ResetTE#text_edit{ entry_id=NewCurrentEntryId,
									   hist_entry_id=undefined },

			cond_utils:if_defined( myriad_debug_text_edit,
				trace_utils:debug_fmt( "Next entry: #~B.",
									   [ NewCurrentEntryId ] ) ),

			% A different atom, to be clearer:
			{ error, NewTE, EntryBinStr, ReasonBinStr, MaybeTimestampBinStr };


		{ entry_update, NewBinEntry } ->
			NewEntry = text_utils:binary_to_string( NewBinEntry ),
			NewTE = TE#text_edit{ precursor_chars=lists:reverse( NewEntry ),
								  postcursor_chars=[] },

			{ update_entry, NewTE }

	end.



-doc "Recalls the previous entry in history, if any.".
-spec recall_previous_entry( text_edit() ) -> 'unchanged' | text_edit().
% Starting history navigation:
recall_previous_entry( TE=#text_edit{ entry_id=CurrentEntryId,
									  hist_entry_id=undefined,
									  processor_pid=ProcessorPid } ) ->
	PrevHistEntryId = CurrentEntryId - 1,
	ProcessorPid ! { getMaybeEntryFromId, PrevHistEntryId, self() },

	receive

		% Exceeded history, not changing anything:
		{ target_entry, _MaybeEntryBinStr=undefined } ->
			cond_utils:if_defined( myriad_debug_text_edit,
				trace_utils:debug( "Recall previous: unchanged." ) ),
			unchanged;

		{ target_entry, EntryBinStr } ->

			EntryStr = text_utils:binary_to_string( EntryBinStr ),

			cond_utils:if_defined( myriad_debug_text_edit,
				trace_utils:debug_fmt( "Recall previous (A): '~p' (~p).",
									   [ EntryStr, PrevHistEntryId ] ) ),

			% Cursor will be just after this recalled entry:
			TE#text_edit{ precursor_chars=lists:reverse( EntryStr ),
						  postcursor_chars=[],
						  hist_entry_id=PrevHistEntryId,
						  % Backup of the entry that was in edition:
						  current_entry=get_entry( TE ) }

	end;

% Already navigating in history:
recall_previous_entry( TE=#text_edit{ hist_entry_id=HistEntryId,
									  processor_pid=ProcessorPid } ) ->
	PrevHistEntryId = HistEntryId - 1,
	ProcessorPid ! { getMaybeEntryFromId, PrevHistEntryId, self() },

	receive

		% Exceeded history, not changing anything:
		{ target_entry, _MaybeEntryBinStr=undefined } ->
			unchanged;

		{ target_entry, EntryBinStr } ->

			EntryStr = text_utils:binary_to_string( EntryBinStr ),

			cond_utils:if_defined( myriad_debug_text_edit,
				trace_utils:debug_fmt( "Recall previous (B): '~p' (~p).",
									   [ EntryStr, PrevHistEntryId ] ) ),

			% Skipping over duplicated commands in history:

			RevEntryStr = lists:reverse( EntryStr ),

			% Expected: TE#text_edit.postcursor_chars =:= [].
			case TE#text_edit.precursor_chars =:= RevEntryStr of

				true ->
					% Skipping duplicate:
					recall_previous_entry( TE#text_edit{
											hist_entry_id=PrevHistEntryId } );

				false ->
					% Cursor will be just after this new recalled entry:
					TE#text_edit{ precursor_chars=RevEntryStr,
								  postcursor_chars=[],
								  hist_entry_id=PrevHistEntryId }

			end

	end.



-doc "Recalls the next entry in history, if any.".
-spec recall_next_entry( text_edit() ) -> 'unchanged' | text_edit().
% Then nothing to do (already at end, no navigation):
recall_next_entry( #text_edit{ hist_entry_id=undefined } ) ->
	cond_utils:if_defined( myriad_debug_text_edit,
						   trace_utils:debug( "Recall next: unchanged." ) ),
	unchanged;

recall_next_entry( TE=#text_edit{ entry_id=CurrentEntryId,
								  hist_entry_id=HistEntryId,
								  processor_pid=ProcessorPid } ) ->

	%trace_utils:debug_fmt( "Recall next: ~B.", [ HistEntryId+1 ] ),

	case HistEntryId+1 of

		% So leaving history navigation:
		CurrentEntryId ->
			% To restore current entry:
			RevCurrentEntry = lists:reverse( TE#text_edit.current_entry ),
			TE#text_edit{ precursor_chars=RevCurrentEntry,
						  hist_entry_id=undefined };

		% Still navigating towards current:
		NextHistEntryId ->
			ProcessorPid ! { getMaybeEntryFromId, NextHistEntryId, self() },
			receive

				% MaybeEntryBinStr=undefined not possible:
				{ target_entry, EntryBinStr } ->

					RevEntryStr = lists:reverse(
						text_utils:binary_to_string( EntryBinStr ) ),

					case RevEntryStr =:= TE#text_edit.precursor_chars of

						true ->
							% Then skipping:
							recall_next_entry( TE#text_edit{
								hist_entry_id=NextHistEntryId } );

						false ->

							cond_utils:if_defined( myriad_debug_text_edit,
								trace_utils:debug_fmt(
									"Recall next: '~p' (~p).",
									[ EntryBinStr, NextHistEntryId ] ) ),

							TE#text_edit{ precursor_chars=RevEntryStr,
										  postcursor_chars=[],
										  hist_entry_id=NextHistEntryId }

					end

			end

	end.



-doc """
Deletes any character at the current cursor position (like with the DELETE key).
""".
-spec delete_current_char( text_edit() ) -> 'unchanged' | text_edit().
delete_current_char( #text_edit{ postcursor_chars=[] } ) ->
	unchanged;

delete_current_char( TE=#text_edit{ precursor_chars=PreChars,
									postcursor_chars=S=[ _Next | Others ] } ) ->
	TE#text_edit{ postcursor_chars=Others,
				  prev_precursor_chars=PreChars,
				  prev_postcursor_chars=S }.



-doc """
Deletes any character just before the current cursor position (like with the
BACKSPACE key).
""".
-spec delete_previous_char( text_edit() ) -> 'unchanged' | text_edit().
delete_previous_char( #text_edit{ precursor_chars=[] } ) ->
	unchanged;

delete_previous_char( TE=#text_edit{ precursor_chars=S=[ _Prev | Others ],
									 postcursor_chars=PostChars } ) ->
	TE#text_edit{ precursor_chars=Others,
				  prev_precursor_chars=S,
				  prev_postcursor_chars=PostChars }.



-doc """
Returns an updated text edit, completed as much as possible, together with any
list of extra completions spotted.
""".
-spec get_completions( text_edit() ) ->
				{ text_edit(), option( [ completion() ] ) }.
get_completions( TextEdit ) ->
	{ TextEdit, [ "aa", "bb", "cc" ] }.
	%{ TextEdit, [ "ssssssss" ] }.



-doc "Returns a textual representation of the specified text edit.".
-spec to_string( text_edit() ) -> ustring().
to_string( #text_edit{ entry_id=EntryId,
					   precursor_chars=PreChars,
					   postcursor_chars=PostChars } ) ->
	text_utils:format( "text edit #~B whose text with cursor is '~ts|~ts'",
					   [ EntryId, lists:reverse( PreChars ), PostChars ] ).
