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
% Creation date: Saturday, November 23, 2024.


% The state of a general-purpose text edition facility.
%
% Note that at least most users of text edits shall rely on the functions of the
% text_edit module, rather than including and using this record.
%
-record( text_edit, {

	% Identifier (number) of the entry being currently edited (hence that will
	% be the "next" one).
	%
	% So corresponds to the number of entries already submitted (as obtained
	% from the corresponding processor) plus one.
	%
	entry_id :: text_edit:entry_id(),

	% Any text before the leftmost position of the cursor:
	prefix :: text_utils:ustring(),

	% The number of characters of the prefix:
	prefix_len :: text_utils:length(),

	% The characters (Unicode codepoints) that are strictly before the current
	% cursor, in reverse order:
	%
	precursor_chars = [] :: [ text_utils:uchar() ],

	% The characters (Unicode codepoints) that are at or after the current
	% cursor (in normal order):
	%
	postcursor_chars = [] :: [ text_utils:uchar() ],


	% Backup to support (one-level of) Ctrl-z in command editing:
	prev_precursor_chars = [] :: [ text_utils:uchar() ],

	% Backup to support (one-level of) Ctrl-z in command editing:
	prev_postcursor_chars = [] :: [ text_utils:uchar() ],


	% Tells whether a trailing dot should be automatically added if lacking in a
	% entry (mostly useful for shells; default: false):
	%
	auto_add_trailing_dot = false :: boolean(),

	% Tells whether the entry cursor should wrap around:
	wrap_cursor = false :: boolean(),


	% The identifier of any entry currently selected in their history:
	hist_entry_id :: option( text_edit:entry_id() ),

	% To backup the currently-edited entry while navigating in their history:
	current_entry :: option( text_edit:entry() ),

	% The PID of the underlying actual processing logic:
	processor_pid :: text_edit:processor_pid() } ).
