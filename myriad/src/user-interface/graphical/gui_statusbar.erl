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
% Creation date: Saturday, August 26, 2023.


% @doc Gathering of various facilities for the management of <b>status bars</b>,
% a thin space at the bottom of a frame where texts are typically displayed.
%
-module(gui_statusbar).


-opaque status_bar() :: wxStatusBar:wxStatusBar().
% A thin space at the bottom of a frame where texts are typically displayed.
%
% They are very useful to trace events, and can be split in separate fields.


-type status_bar_style() ::
	'normal'
  | 'flat'
  | 'raised'
  | 'sunken'.
% A style element of a status bar.


-type field_count() :: count().
% A number of fields, typically in a status bar.

-type field_index() :: positive_index().
% The index (1 or more) of a field in a status bar.

-type field_width() :: integer().
% The width of a field in a status bar.
%
% If positive, designates a fixed width, in pixels.
% If negative, designates a width ratio among all variable-width fields.



-export_type([ status_bar/0, status_bar_style/0,
			   field_count/0, field_index/0, field_width/0 ]).


-export([ create/1, create/2, destruct/1,
		  set_field_count/2, set_field_widths/2,
		  push_text/2, push_text/3,
		  push_field_text/3, push_field_text/4 ]).



% Shorthands:

-type count() :: basic_utils:count().
-type positive_index() :: basic_utils:positive_index().
-type bit_mask() :: basic_utils:bit_mask().

-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type text() :: gui_text:text().

-type frame() :: gui_window:frame().



% @doc Creates and attaches a status bar at the bottom of the specified frame.
-spec create( frame() ) -> status_bar().
create( Frame ) ->
	% No interesting option (winid):
	wxFrame:createStatusBar( Frame ).



% @doc Creates and attaches a status bar of the specified style at the bottom of
% the specified frame.
%
-spec create( frame(), [ status_bar_style() ] ) -> status_bar().
create( Frame, StatusBarStyles ) ->
	% No interesting option (winid):
	wxFrame:createStatusBar( Frame,
		[ { style, statusbar_styles_to_bitmask( StatusBarStyles ) } ] ).



% @doc Destructs the specified status bar.
-spec destruct( status_bar() ) -> void().
destruct( StatusBar ) ->
	wxStatusBar:destroy( StatusBar ).



% @doc Sets the number of fields in which the specified status bar is to be
% (evenly) split.
%
-spec set_field_count( status_bar(), field_count() ) -> void().
set_field_count( StatusBar, FieldCount ) ->
	wxStatusBar:setFieldsCount( StatusBar, FieldCount ).


% @doc Sets the width of the fields of the specified status bar (and thus
% determines their number as well).
%
-spec set_field_widths( status_bar(), [ field_width() ] ) -> void().
set_field_widths( StatusBar, FieldWidths ) ->
	wxStatusBar:setFieldsCount( StatusBar, length( FieldWidths ),
								[ { widths, FieldWidths } ] ).



% @doc Pushes the specified text in the (first field of the) specified status
% bar.
%
-spec push_text( status_bar(), text() ) -> void().
push_text( StatusBar, Text ) ->
	wxStatusBar:pushStatusText( StatusBar, Text ).


% @doc Pushes the specified formatted text in the (first field of the) specified
% status bar.
%
-spec push_text( status_bar(), format_string(), format_values() ) -> void().
push_text( StatusBar, FormatString, FormatValues ) ->
	Text = text_utils:format( FormatString, FormatValues ),
	push_text( StatusBar, Text ).



% @doc Pushes the specified text in the specified field of the specified status
% bar.
%
-spec push_field_text( status_bar(), text(), field_index() ) -> void().
push_field_text( StatusBar, Text, FieldIndex ) ->
	% Wx starts at zero:
	wxStatusBar:pushStatusText( StatusBar, Text, [ { number, FieldIndex-1 } ] ).


% @doc Pushes the specified formatted text in the specified status bar.
-spec push_field_text( status_bar(), format_string(), format_values(),
						 field_index() ) -> void().
push_field_text( StatusBar, FormatString, FormatValues, FieldIndex ) ->
	Text = text_utils:format( FormatString, FormatValues ),
	push_field_text( StatusBar, Text, FieldIndex ).


% @doc Converts the specified MyriadGUI status bar style elements into the
% appropriate wx-specific bit mask.
%
% (helper)
%
-spec statusbar_styles_to_bitmask( [ status_bar_style() ] ) -> bit_mask().
statusbar_styles_to_bitmask( Styles ) ->
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_status_bar_style( S ) bor Acc
				 end,
				 _InitialAcc=0,
				 _List=Styles ).
