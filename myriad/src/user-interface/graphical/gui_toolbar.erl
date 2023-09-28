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
% Creation date: Saturday, September 2, 2023.


% @doc Gathering of various facilities for <b>toolbar management</b>.
%
% A tool bar is a bar of buttons and/or other controls usually placed below the
% menu bar in a frame.
%
% Use our gui_frame_bars_test.erl test in order to display all known tools.
%
-module(gui_toolbar).


-opaque toolbar() :: wxToolBar:wxToolBar().
% A bar of image-based buttons and/or other controls usually placed below the
% menu bar of a frame.
%
% A toolbar emits menu commands in the same way that a frame menubar does.


-type toolbar_style() ::
	'top'
  | 'bottom'
  | 'left'
  | 'right'
  | 'flat'
  | 'dockable'
  | 'no_icons'
  | 'text'
  | 'no_divider'
  | 'no_align'
  | 'horizontal_layout'
  | 'no_tooltips'
  | 'default'.
% A style element of a toolbar.


-type tool() :: wx_object().
% A tool, as an element of a toolbar.


% Corresponds to menu items:
-type tool_kind() :: gui_menu:menu_item_kind().
% The kind of a tool in a toolbar.

-type separator() :: wx_object().
% A separator for spacing groups of tools.


-export_type([ toolbar/0, toolbar_style/0, tool/0, tool_kind/0,
			   separator/0 ]).


-export([ create/1, create/3, set/2,
		  add_control/2, add_tool/5, add_tool/7, add_separator/1,
		  update_tools/1 ]).


% Exported helpers:
-export([ to_wx_tool_kind/1 ]).


% Shorthands:

-type wx_object() :: gui:wx_object().
-type label() :: gui:label().

-type frame() :: gui_window:frame().

-type control() :: gui_widget:control().

-type bitmap() :: gui_bitmap:bitmap().

-type help_info() :: gui_text:help_info().

-type id() :: gui_id:id().

-type wx_enum() :: gui_wx_backend:wx_enum().


% @doc Creates a toolbar in the specified frame.
-spec create( frame() ) -> toolbar().
create( Frame ) ->
	wxFrame:createToolBar( Frame ).


% @doc Creates a toolbar in the specified frame, with the specified identifier
% (if any) and style.
%
% Apparently up to one toolbar can be associated to a frame (e.g. no top and
% left toolbars allowed simultaneously).
%
-spec create( frame(), id(), [ toolbar_style() ] ) -> toolbar().
create( Frame, Id, ToolbarStyles ) ->
	wxFrame:createToolBar( Frame, [ { id, gui_id:declare_any_id( Id ) },
		{ style, toolbar_styles_to_bitmask( ToolbarStyles ) } ] ).



% @doc Sets the specified toolbar in the specified frame.
-spec set( frame(), toolbar() ) -> void().
set( Frame, Toolbar ) ->
	wxFrame:setToolBar( Frame, Toolbar ).



% @doc Adds the specified control to the specified toolbar.
-spec add_control( toolbar(), control() ) -> void().
add_control( Toolbar, Control ) ->
	wxToolBar:addControl( Toolbar, Control ).



% @doc Adds the specified tool, represented by the specified bitmap, with the
% specified identifier (if any) and any short help, to the specified toolbar.
%
% update_tools/1 should be called once additions have been done, so that they
% are taken into account.
%
-spec add_tool( toolbar(), id(), label(), bitmap(), maybe( help_info() ) ) ->
											void().
add_tool( Toolbar, Id, Label, Bitmap, MaybeShortHelp ) ->

	WxOpts = case MaybeShortHelp of

		undefined ->
			[];

		ShortHelp ->
			[ { shortHelp, ShortHelp } ]

	end,

	wxToolBar:addTool( Toolbar, gui_id:declare_any_id( Id ), Label, Bitmap,
					   WxOpts ).


% @doc Adds the specified tool, represented by the specified enabled/disabled
% bitmaps, with the specified identifier (if any) and any short/long helps, to
% the specified toolbar.
%
% update_tools/1 should be called once additions have been done, so that they
% are taken into account.
%
-spec add_tool( toolbar(), id(), label(), bitmap(), bitmap(),
				maybe( help_info() ), maybe( help_info() ) ) -> void().
add_tool( Toolbar, Id, Label, BitmapIfEnabled, BitmapIfDisabled,
		  MaybeShortHelp, MaybeLongHelp ) ->

	WxOpts = case MaybeShortHelp of
		undefined -> [];
		ShortHelp -> [ { shortHelp, ShortHelp } ]
	end ++ case MaybeLongHelp of
		undefined -> [];
		LongHelp -> [ { longHelp, LongHelp } ]
	end,

	wxToolBar:addTool( Toolbar, gui_id:declare_any_id( Id ), Label,
					   BitmapIfEnabled, BitmapIfDisabled, WxOpts ).


% @doc Adds a separator to the specified toolbar, and returns that separator.
%
% update_tools/1 should be called once additions have been done, so that they
% are taken into account.
%
-spec add_separator( toolbar() ) -> separator().
add_separator( Toolbar ) ->
	wxToolBar:addSeparator( Toolbar ).



% @doc Updates the specified toolbar so that it takes into account any new
% tools; returns whether an update had to be done.
%
-spec update_tools( toolbar() ) -> boolean().
update_tools( Toolbar ) ->
	wxToolBar:realize( Toolbar ).



% Wx support.


% @doc Converts the specified MyriadGUI toolbar style elements into the
% appropriate wx-specific bit mask.
%
% (helper)
%
-spec toolbar_styles_to_bitmask( [ toolbar_style() ] ) -> wx_enum().
toolbar_styles_to_bitmask( Styles ) ->
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_toolbar_style( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=Styles ).



% @doc Converts the specified kind of tool into a wx-specific one.
-spec to_wx_tool_kind( tool_kind() ) -> wx_enum().
to_wx_tool_kind( ToolKind ) ->
	% Same:
	gui_generated:get_second_for_menu_item_kind( ToolKind ).
