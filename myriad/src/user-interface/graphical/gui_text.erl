% Copyright (C) 2010-2023 Olivier Boudeville
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
% Creation date: Monday, February 15, 2010.


% @doc Gathering of various facilities for <b>text input and display</b>.
%
% See also the corresponding dialogs: gui_dialog:text_entry_dialog() and
% gui_dialog:message_dialog().
%
-module(gui_text).


-opaque static_text_display() :: wxStaticText:wxStaticText().
% A widget displaying a text; a text display behaves like a panel dedicated
% to the rendering of a text.


-type static_display_option() ::
	{ 'position', point() }
  | { 'size', size() }
  | { 'style', [ static_display_style() ] }.
% An option for the creation of a static text display.


-type static_display_style() ::
	'align_left'   % Align the text to the left.
  | 'align_right'  % Align the text to the right.
  | 'center'       % Center the text (horizontally).
  | 'fixed_size'   % No auto-resize.
  | 'ellipsize_begin'     % Any shrinking done from the start of the text.
  | 'ellipsize_middle'    % Any shrinking done at the middle of the text.
  | 'ellipsize_end'.      % Any shrinking done at the end of the text.
% A style element of a static text display.
%
% See also [http://docs.wxwidgets.org/stable/classwx_static_text.html].


-export_type([ static_text_display/0,
			   static_display_option/0, static_display_style/0 ]).


-type text() :: ui:text().
% Any kind of GUI-related text.


-type help_info() :: text().
% A text for help information.


% Other text-related types:
-export_type([ text/0, help_info/0 ]).


% Operations related to static texts to display:
-export([ create_static_display/2, create_static_display/3,
		  create_static_display/4,
		  destruct_static_display/1 ]).




% For related defines:
-include("gui_base.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").


% Shorthands:

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type label() :: gui:label().
-type parent() :: gui:parent().
-type point() :: gui:point().
-type size() :: gui:size().

-type id() :: gui_id:id().

-type wx_opt_pair() :: gui_wx_backend:wx_opt_pair().



% Text display section.


% @doc Creates a static text display, based on the specified label.
-spec create_static_display( label(), parent() ) -> static_text_display().
create_static_display( Label, Parent ) ->
	create_static_display( Label, gui_id:get_any_id(), Parent ).



% @doc Creates a static text display, based on the specified label and
% identifier.
%
-spec create_static_display( label(), id(), parent() ) -> static_text_display().
create_static_display( Label, Id, Parent ) ->
	create_static_display( Label, Id, _Options=[], Parent ).



% @doc Creates a static text display, based on the specified label, identifier
% and option(s).
%
-spec create_static_display( label(), id(),
	maybe_list( static_display_option() ), parent() ) -> static_text_display().
create_static_display( Label, Id, Options, Parent ) ->
	WxOpts = to_wx_static_display_opts( Options ),
	wxStaticText:new( Parent, Id, Label, WxOpts ).


% @doc Destructs the specified static text display.
-spec destruct_static_display( static_text_display() ) -> void().
destruct_static_display( StaticTextDisplay ) ->
	wxStaticText:destroy( StaticTextDisplay ).




% @doc Converts the specified static text option(s) into the appropriate
% back-end specific options.
%
% (helper)
%
-spec to_wx_static_display_opts( maybe_list( static_display_option() ) ) ->
											[ wx_opt_pair() ].
to_wx_static_display_opts( Options ) when is_list( Options )->
	[ to_wx_static_text_opt( O ) || O <- Options ];

% Probably a pair:
to_wx_static_display_opts( Opt ) ->
	to_wx_static_display_opts( [ Opt ] ).


% (helper)
-spec to_wx_static_text_opt( static_display_option() ) -> wx_opt_pair().
to_wx_static_text_opt( _Opt={ position, Pos } ) ->
	{ pos, Pos };

to_wx_static_text_opt( Opt={ size, _S } ) ->
	Opt;

to_wx_static_text_opt( _Opts={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		gui_generated:get_second_for_static_text_display_style( S ) bor Acc end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle }.
