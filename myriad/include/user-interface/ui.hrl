% Copyright (C) 2018-2024 Olivier Boudeville
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
% Creation date: 2018.


-type ui_options() :: [ any() ].
% Options to initialise a user interface.


-export_type([ ui_options/0 ]).


% Defines that are common, transverse to all user interface backends.

-type text() :: text_utils:ustring().

-type label() :: text().
-type prompt() :: text().
-type title() :: text().


-type binary_choice() :: 'yes' | 'no'.


-type message() :: text().
% For traces.


-type choice_text() :: text().
% The text of a choice.


-type choice_designator() :: term() | 'ui_cancel'.
% Designator of a choice (e.g. regardless of the choice labels, locales, etc.):
%
% (usually an atom, but possibly an integer or anything else)
%
% Note that there is a reserved designator (i.e. that shall never be specified
% among the user ones) that is the 'ui_cancel' atom, which is returned by a
% backend whenever the user chose to cancel the operation rather than selecting
% one of the available options.


-type choice_index() :: basic_utils:count().
% The index of a choice (starting at 1).


-type choice_element() :: { choice_designator(), choice_text() }.


-export_type([ text/0, label/0, prompt/0, title/0, message/0,
			   choice_text/0, choice_designator/0, choice_index/0,
			   choice_element/0 ]).



% As soon as an interface (thanks to a given backend) gets rich enough, many
% optional settings may be specified.
%
% Rather than having them all listed as separate function parameters or than
% using records (knowing that different kinds of dialogs call for different
% settings), we chose to rely on a more flexible parameter table, in which
% parameters will be looked-up (their values will apply, otherwise default
% behaviour will be retained).


-type ui_setting_key() :: 'backtitle' | 'title'.
% The known per-setting keys.


-type ui_setting_value() :: term().
% The setting-specific values.


-type ui_setting_entry() :: { ui_setting_key(), ui_setting_value() }.
% For setting-specific entries.


-type common_entry() :: backtitle_entry() | title_entry().
% Parameter keys that are common to all dialogs.


-type backtitle_entry() :: { 'backtitle', title() }.
% For back-titles.


-type title_entry() :: { 'title', title() }.
% For (front) titles.



% The type of associated table used for the settings of user interfaces:
-define( ui_table, list_table ).


-type setting_table() :: ?ui_table:?ui_table( ui_setting_key(),
											  ui_setting_value() ).
% A table storing UI settings (either top-level or backend-specific).



-export_type([ ui_setting_key/0, ui_setting_value/0, ui_setting_entry/0,
			   common_entry/0, backtitle_entry/0, title_entry/0,
			   setting_table/0 ]).



% The key used by UI modules to store their name in the process dictionary:
-define( ui_name_key, myriad_ui_name ).

% The key used by UI modules to store their state in the process dictionary:
-define( ui_state_key, myriad_ui_state ).
