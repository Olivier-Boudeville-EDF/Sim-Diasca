% Copyright (C) 2023-2024 Olivier Boudeville
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
% Creation date: Thursday, August 31, 2023.


% @doc Gathering of various facilities for <b>windows</b>.
%
% A window may be a top-level of not, a frame, a splitter window, etc.
%
% A window is a special case of widget, which is the most general form of
% graphical component.
%
% See also:
% - the gui_window_manager module regarding the insertion of windows in their
% environment
% - the gui_frame module
%
-module(gui_window).



-opaque window() :: gui_widget:widget().
% Any kind of window.
%
% Base, most general class for all windows (for example, a frame is a window
% whose size and position can usually be changed by the user).
%
% This corresponds to "real" windows - not to any widget (that wx/WxWidgets call
% "windows").
%
% Note that this class is mostly a mother, abstract one, and various creation
% settings will lead to no window being displayed at all. For most practical
% purposes, a frame (a concrete, special case thereof) may/should be created
% instead.



-type window_option() :: { 'position', point() }
					   | { 'size', size() }
					   | { 'style', [ window_style() ] }.
% Window-specific options (quite common).


-type window_style() ::
	'default_border'
  | 'simple_border'
  | 'sunken_border'
  | 'raised_border'
  | 'static_border'
  | 'theme_border'
  | 'no_border'
  | 'double_border'
  | 'transparent' % Windows-only
  | 'tab_traversable'
  | 'grab_all_keys'

  % Note that, in most ports, scrollbars cannot be used with native controls
  % that do not support scrollbars (i.e. that do not derive from
  % wxScrolledWindow) nor with top-level windows; see our gui_scrollable module
  % instead. Refer to https://docs.wxwidgets.org/latest/overview_scrolling.html
  % for more details.
  %
  | 'with_vertical_scrollbar'
  | 'with_horizontal_scrollbar'

  | 'never_hide_scrollbars'
  | 'clip_children'
  | 'full_repaint_on_resize'.
% A style element of a window.
%
% See also [http://docs.wxwidgets.org/stable/classwx_window.html]


-export_type([ window/0, window_option/0, window_style/0 ]).



-type icon_name_id() :: standard_icon_name_id() | id().
% The identifier of an icon.


-type standard_icon_name_id() ::
	'asterisk_icon' | 'stop_icon' | 'information_icon'
  | 'question_icon' | 'error_icon' | 'warning_icon' | 'hand_icon'
  | 'exclamation_icon'.
% The name identifiers of the standard icons.


-export_type([ icon_name_id/0, standard_icon_name_id/0 ]).



-opaque top_level_window() :: wxTopLevelWindow:wxTopLevelWindow().
% A top-level (application-wide) window.
%
% The top-level window is a base class common to frames and dialogs; so such a
% window is typically any frame (including any main one) or any dialog.


-export_type([ top_level_window/0 ]).



% At least for the splitter record:
-include("gui_base.hrl").

-type splitter() :: #splitter{}.
% Represents a window able to be split into two panes.
%
% Information regarding the (fixed, static) horizontal or vertical splitting of
% a window into two ones.


-opaque splitter_window() :: wxSplitterWindow:wxSplitterWindow().
% A window able to be split into two panes; it may thus manage up to two
% subwindows.


-type sash_gravity() :: number().
% Tells how much the first pane of a splitter window is to grow while resizing
% it:
%  - 0.0: only the bottom/right window is automatically resized
%  - 0.5: both windows grow by equal size
%  - 1.0: only left/top window grows
%
% Gravity should be a value between 0.0 and 1.0; its default value is 0.0.


-export_type([ splitter/0, splitter_window/0, sash_gravity/0 ]).


% Local types:

-type wx_art_id() :: unicode:chardata().
% For example "wxART_NEW".



% For standard, basic windows:
-export([ create/0, create/1, create/2, create/5,
		  destruct/1 ]).


% For any kind of window:
-export([ show/1, hide/1, record_as_top_level/1, set_menu_bar/2 ]).


% For top-level windows:
-export([ set_title/2, get_title/1,
		  set_icon/2,
		  center_on_screen/1, center_on_screen/2,
		  is_maximised/1, maximize/1,
		  is_fullscreen/1, set_fullscreen/2,
		  is_active/1 ]).


% For splitters:
-export([ create_splitter/4, create_splitter/5, set_unique_pane/2 ]).


% Wx-level:
-export([ window_styles_to_bitmask/1, to_wx_window_options/1,
		  to_wx_icon_id/1 ]).



% Implementation notes:
%
% The wx backend names the concept of widget "window", which is a bit
% restrictive (a window is seen at least here in MyriadGUI as a special case of
% widget).


% For ?gui_any_id:
-include("gui_internal_defines.hrl").

-type wx_window_option() :: term().


% Shorthands:

-type bit_mask() :: basic_utils:bit_mask().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type os_type() :: system_utils:os_type().

-type any_file_path() :: file_utils:any_file_path().

-type point() :: gui:point().
-type size() :: gui:size().
-type sizing() :: gui:sizing().

-type position() :: gui:position().
-type orientation() :: orientation().
-type parent() :: gui:parent().
-type title() :: gui:title().

-type menu_bar() :: gui_menu:menu_bar().

-type id() :: gui_id:id().



% Subsection for standard, basic windows.
%
% Parent-less windows (widgets) are edge cases; frames shall be preferred.


% @doc Creates a basic window.
%
% @hidden (internal use only)
%
-spec create() -> window().
create() ->
	wxWindow:new().



% @doc Creates a basic window having the specified identifier.
%
% @hidden (internal use only)
%
-spec create( id(), parent() ) -> window().
create( Id, Parent ) ->

	ActualId = gui_id:declare_any_id( Id ),

	% Should not be 'undefined', otherwise: "wxWidgets Assert failure:
	% ./src/gtk/window.cpp(2586): \"parent\" in PreCreation() : Must have
	% non-NULL parent"}
	%
	ActualParent = gui_wx_backend:to_wx_parent( Parent ),

	wxWindow:new( ActualParent, ActualId ).



% @doc Creates a basic window of the specified size.
%
% @hidden (internal use only)
%
-spec create( sizing() ) -> window().
create( Size ) ->

	ActualId = gui_id:declare_any_id( undefined ),
	ActualParent = gui_wx_backend:to_wx_parent( undefined ),

	WxOpts = [ gui_wx_backend:to_wx_size( Size ) ],

	wxWindow:new( ActualParent, ActualId, WxOpts ).



% @doc Creates a basic window from the specified settings.
%
-spec create( position(), sizing(), [ window_style() ], id(), parent() ) ->
											window().
create( Position, Sizing, Styles, Id, Parent ) ->

	WxOpts = [ gui_wx_backend:to_wx_position( Position ),
			   gui_wx_backend:to_wx_size( Sizing ),
			   { style, window_styles_to_bitmask( Styles ) } ],

	ActualId = gui_id:declare_any_id( Id ),
	ActualParent = gui_wx_backend:to_wx_parent( Parent ),

	%trace_utils:debug_fmt( "Creating a window with backend options ~w, "
	%   "identifier ~w and parent ~w.", [ WxOpts, ActualId, ActualParent ] ),

	wxWindow:new( ActualParent, ActualId, WxOpts ).



% @doc Destructs the specified window.
-spec destruct( window() ) -> void().
destruct( Window ) ->
	wxWindow:destroy( Window ).



% @doc Shows (renders) the specified window (or subclass thereof).
%
% Returns whether anything had to be done.
%
% This is the place where all widgets resolve their positions, sizes and
% contents.
%
-spec show( window() | [ window() ] ) -> boolean().
show( Windows ) when is_list( Windows )->

	% Note: onShown used to be sent to the MyriadGUI loop, as some widgets had
	% to be adjusted then, but it is no longer useful.

	%trace_utils:debug_fmt( "Showing windows ~p.", [ Windows ] ),
	Res = show_helper( Windows, _Acc=false ),
	%get_main_loop_pid() ! { onShown, [ Windows ] },

	show_fix(),

	Res;

show( Window ) ->
	%trace_utils:debug_fmt( "Showing window ~p.", [ Window ] ),
	Res = wxWindow:show( Window ),
	%get_main_loop_pid() ! { onShown, [ [ Window ] ] },
	show_fix(),

	Res.


% (helper)
show_helper( _Windows=[], Acc ) ->
	show_fix(),
	Acc;

show_helper( _Windows=[ W | T ], Acc ) ->
	NewAcc = wxWindow:show( W ) orelse Acc,
	show_helper( T, NewAcc ).


% This is certainly a strange fix. It was observed with gui_image_test.erl that
% the image was initially displayed iff such a sleep was following
% 'gui_window:show(MainFrame)' - although there was no next rendering operation
% (a receive blocking until the user closes the window). Otherwise the panel
% remained blank until the frame was redrawn for any reason (e.g. resize).
%
show_fix() ->
	timer:sleep( 10 ).
	%ok.


% @doc Hides the specified window.
%
% Returns whether anything had to be done.
%
-spec hide( window() ) -> boolean().
hide( Window ) ->
	wxWindow:show( Window, [ { show, false } ] ).




% Top-level window subsection.


% @doc Sets the title of the specified top-level window.
-spec set_title( top_level_window(), title() ) -> void().
set_title( TopLevelWindow, Title ) ->
	wxTopLevelWindow:setTitle( TopLevelWindow, Title ).


% @doc Returns the title of the specified top-level window.
-spec get_title( top_level_window() ) -> title().
get_title( TopLevelWindow ) ->
	wxTopLevelWindow:getTitle( TopLevelWindow ).


% @doc Sets the icon of the specified top-level window.
-spec set_icon( top_level_window(), any_file_path() ) -> void().
set_icon( TopLvlWin, IconPath ) ->

	%trace_utils:debug_fmt( "Setting icon of ~w to '~ts'.",
	%                       [ TopLvlWin, IconPath ] ),

	% Supported image formats documented as being only BMP by default, yet test
	% on PNG succeeded.

	% Current no wx_image:initAllImageHandlers/* (for other formats than BMP),
	% just wx_image:initStandardHandlers/0.

	cond_utils:if_defined( myriad_debug_resources,
						   file_utils:check_existing_file_or_link( IconPath ) ),

	% Apparently 'Icon = wxIcon:new(IconPath),' could have sufficed:
	Img = wxImage:new( IconPath ),
	Bitmap = wxBitmap:new( Img ),
	Icon = wxIcon:new(),
	wxIcon:copyFromBitmap( Icon, Bitmap ),

	wxTopLevelWindow:setIcon( TopLvlWin, Icon ).


% @doc Centers the specified top-level window on screen.
-spec center_on_screen( top_level_window() ) -> void().
center_on_screen( TopLvlWin ) ->
	wxTopLevelWindow:centerOnScreen( TopLvlWin ).


% @doc Centers the specified top-level window on screen, along the specified
% orientation(s).
%
-spec center_on_screen( top_level_window(), orientation() ) -> void().
center_on_screen( TopLvlWin, Orientation ) ->
	wxTopLevelWindow:centerOnScreen( TopLvlWin,
		gui_wx_backend:to_wx_orientation( Orientation ) ).


% @doc Tells whether the specified top-level window is maximised.
-spec is_maximised( top_level_window() ) -> boolean().
is_maximised( TopLevelWindow ) ->
	wxTopLevelWindow:isMaximized( TopLevelWindow ).


% @doc Maximises the specified top-level window.
-spec maximize( top_level_window() ) -> void().
maximize( TopLevelWindow ) ->
	wxTopLevelWindow:maximize( TopLevelWindow ).



% @doc Returns whether the specified top-level window is fullscreen.
-spec is_fullscreen( top_level_window() ) -> boolean().
is_fullscreen( TopLvlWin ) ->
	wxTopLevelWindow:isFullScreen( TopLvlWin ).


% @doc Shows the specified top-level window to fullscreen (if true) or restores
% it to its normal state (if false).
%
% Showing a window full screen also actually shows the window if it is not
% already shown. Any menu bar is then hidden.
%
% Returns (supposedly) whether the operation succeeded.
%
-spec set_fullscreen( top_level_window(), boolean() ) -> void().
set_fullscreen( TopLvlWin, ForceFullscreen ) ->
	wxTopLevelWindow:showFullScreen( TopLvlWin, ForceFullscreen ).


% @doc Returns whether the specified top-level window is currently active, that
% is if the user is currently interacting with it.
%
-spec is_active( top_level_window() ) -> boolean().
is_active( TopLvlWin ) ->
	wxTopLevelWindow:isActive( TopLvlWin ).



% Splitter subsection.
%
% Note that these functions handle splitter() instances - not any form of
% splitter_window().


% @doc Creates a splitter of the specified orientation, in the specified window,
% based on the specified sash gravity and pane size, returning a corresponding
% splitter record so that the up to two subwindows can be declared afterwards.
%
-spec create_splitter( window(), orientation(), sash_gravity(), size() ) ->
								splitter().
create_splitter( ParentWindow, Orientation, SashGravity, PaneSize ) ->
	create_splitter( ParentWindow, Orientation, SashGravity, PaneSize,
					 system_utils:get_operating_system_type() ).


% @doc Creates a splitter of the specified orientation, in the specified window,
% based on the specified sash gravity and pane size, according to the specified
% OS, returning a corresponding splitter record so that the up to two subwindows
% can be declared afterwards.
%
-spec create_splitter( window(), orientation(), sash_gravity(), size(),
					   os_type() ) -> splitter().
create_splitter( ParentWindow, Orientation, SashGravity, PaneSize, OSType ) ->

	WxStyle = case OSType of

		{ _OSFamily=unix, _OSName=darwin } ->
			?wxSP_LIVE_UPDATE bor ?wxSP_3DSASH;

		{ win32, _ } ->
			?wxSP_LIVE_UPDATE bor ?wxSP_BORDER;

		_ ->
			?wxSP_LIVE_UPDATE bor ?wxSP_3D

		end,

	SplitterWin = wxSplitterWindow:new( ParentWindow, [ { style, WxStyle } ] ),

	wxSplitterWindow:setSashGravity( SplitterWin, SashGravity ),

	wxSplitterWindow:setMinimumPaneSize( SplitterWin, PaneSize ),

	#splitter{ splitter_window=SplitterWin,
			   orientation=gui:check_orientation( Orientation ) }.


% @doc Sets the specified splitter in a single pane configuration, using for
% that the specified window.
%
-spec set_unique_pane( splitter(), parent() ) -> void().
set_unique_pane( #splitter{ splitter_window=SplitterWin }, WindowPane ) ->
	wxSplitterWindow:initialize( SplitterWin, WindowPane ).




% @doc Records, in the MyriadGUI environment, the specified window (typically a
% frame) as the application top-level window.
%
% Note that the specified window is expected to be already, in terms of type, a
% top-level one (e.g. a frame or a dialog); the purpose of this function is only
% to have it recorded as such by MyriadGUI.
%
-spec record_as_top_level( top_level_window() ) -> void().
record_as_top_level( Window ) ->
	environment:set( _K=top_level_window, _V=Window,
					 _Designator=?gui_env_reg_name ).


% @doc Assigns the specified menu bar to the specified window.
-spec set_menu_bar( window(), menu_bar() ) -> void().
set_menu_bar( Window, MenuBar ) ->
	wxWindow:setMenuBar( Window, MenuBar ).



% @doc Converts the specified MyriadGUI window style elements into the
% appropriate wx-specific bit mask.
%
% (helper)
%
-spec window_styles_to_bitmask( [ window_style() ] ) -> bit_mask().
window_styles_to_bitmask( StyleOpts ) when is_list( StyleOpts ) ->
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_window_style( S ) bor Acc
				 end,
				 _InitialAcc=0,
				 _List=StyleOpts );

window_styles_to_bitmask( StyleOpt ) ->
	gui_generated:get_second_for_window_style( StyleOpt ).



% @doc Converts the specified MyriadGUI window option(s) into the appropriate
% wx-specific options.
%
% (exported helper)
%
-spec to_wx_window_options( maybe_list( window_option() ) ) ->
								[ wx_window_option() ].
to_wx_window_options( Options ) when is_list( Options ) ->
	to_wx_window_options( Options, _Acc=[] );

to_wx_window_options( Option ) ->
	to_wx_window_options( [ Option ] ).



to_wx_window_options( _Options=[], Acc ) ->
	Acc;

to_wx_window_options( _Options=[ { style, Style } | T ], Acc ) ->
	to_wx_window_options( T,
		[ { style, window_styles_to_bitmask( Style ) } | Acc ] );

% Unchanged:
to_wx_window_options( _Options=[ H | T ], Acc ) ->
	to_wx_window_options( T, [ H | Acc ] ).


% @doc Converts the specified icon identifier into a wx-specific one.
-spec to_wx_icon_id( icon_name_id() ) -> wx_art_id().
to_wx_icon_id( IconId ) ->
	case gui_generated:get_maybe_second_for_icon_name_id( IconId ) of

		undefined ->
			throw( { unknown_icon_id, IconId } );

		WxIconId ->
			WxIconId

	end.
