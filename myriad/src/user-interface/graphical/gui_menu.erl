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
% Creation date: Saturday, September 2, 2023.


% @doc Gathering of various facilities for <b>menu management</b>, covering both
% menu bars and popup menus.
%
-module(gui_menu).


-opaque menu() :: wxMenu:wxMenu().
% The definition of a menu, to be assigned either to a menu bar or to a popup
% menu.
%
% A menu can be generically designed, before being assigned either to a menu bar
% or to a popup menu.
%
% A submenu may be attached up to once in a menu.
%
% At least for menus meant to be assigned to a menu bar, specifying a title for
% them is of little interest, as when assigning them (at least with add_menu/3),
% a title has to be specified and will take precedence.


-type menu_option() :: 'detachable'.
% An option when creating a menu.


-type menu_style() :: 'detachable'.
% A style element of a menu, see
% [http://docs.wxwidgets.org/stable/classwx_menu.html].


-opaque menu_item() :: wxMenuItem:wxMenuItem().
% An entry registered in a menu; possibly a basic item, a submenu or a
% separator.
%
% Items are added at the current bottom of a menu.


-type menu_title() :: label().
% The title associated to a menu element.
%
% Use ampersand to denote a shortcut/accelerator character, e.g. `"&File"' or
% `"&Print code"'.


-type menu_label() :: menu_title().
% The text to display for a menu item (typically of a menu bar).


-type menu_item_label() :: menu_title().
% The text to display for a menu item (typically of a menu).


-type menu_item_kind() :: 'normal'    % Basic menu item
						| 'check'     % Menu item that can be checked/toggled
						| 'radio'     % Menu item with a radio element
						| 'separator' % Separator between menu items
						| 'dropdown'. % A normal menu item with a dropdown arrow
									  % next to it.
% A kind of menu item.


-type menu_item_status() :: 'enabled' | 'disabled'.
% Tells whether a menu item is enabled or disabled (greyed out).


% See gui_menu_test.erl to inspect them; we listed here only a subset of the
% standard menu items, which are quite numerous:
%
-type standard_menu_item_name_id() ::
	  'new_menu_item'
	| 'open_menu_item'
	| 'close_menu_item'
	| 'save_menu_item'
	| 'save_as_menu_item'
	| 'revert_to_saved_menu_item'
	| 'undelete_menu_item'
	| 'print_menu_item'

	% Print preview:
	| 'preview_menu_item'

	| 'revert_menu_item'
	| 'edit_menu_item'
	| 'file_menu_item'
	| 'properties_menu_item'
	| 'cut_menu_item'
	| 'copy_menu_item'
	| 'paste_menu_item'
	| 'delete_menu_item'
	| 'clear_menu_item'
	| 'find_menu_item'
	| 'select_all_menu_item'

	% Find and replace:
	| 'replace_menu_item'

	| 'replace_all_menu_item'
	| 'clear_menu_item'
	| 'ok_menu_item'
	| 'cancel_menu_item'
	| 'apply_menu_item'
	| 'yes_menu_item'
	| 'no_menu_item'
	| 'add_menu_item'
	| 'remove_menu_item'
	| 'convert_menu_item'
	| 'execute_menu_item'
	| 'home_menu_item'
	| 'refresh_menu_item'
	| 'stop_menu_item'
	| 'index_menu_item'
	| 'select_color_menu_item'
	| 'select_font_menu_item'
	| 'forward_menu_item'
	| 'backward_menu_item'
	| 'up_menu_item'
	| 'down_menu_item'
	| 'top_menu_item'
	| 'bottom_menu_item'
	| 'first_menu_item'
	| 'last_menu_item'
	| 'jump_to_menu_item'
	| 'info_menu_item'

	| 'zoom_factor_one'
	| 'zoom_factor_fit'
	| 'zoom_factor_in'
	| 'zoom_factor_out'

	| 'undo_menu_item'
	| 'redo_menu_item'
	| 'help_menu_item'
	| 'preferences_menu_item'
	| 'about_menu_item'
	| 'floppy_menu_item'
	| 'hard_disk_menu_item'
	| 'network_menu_item'

	| 'bold_menu_item'
	| 'cdrom_menu_item'
	| 'indent_menu_item'
	| 'italic_menu_item'

	| 'justify_center_menu_item'
	| 'justify_fill_menu_item'
	| 'justify_left_menu_item'
	| 'justify_right_menu_item'

	| 'sort_ascending_menu_item'
	| 'sort_descending_menu_item'

	| 'spell_check_menu_item'

	| 'strikethrough_menu_item'

	| 'underline_menu_item'
	| 'unindent_menu_item'

	| 'exit_menu_item'.
% The name identifiers of the standard menu items.
%
% Such standard items are specifically managed (e.g. they have a corresponding
% icon automatically associated).
%
% See also gui_constants:get_menu_item_id_topic_spec/0.
%
% The atoms listed here are reserved name identifiers, as 'undefined' is.


-type menu_item_id() :: standard_menu_item_name_id() | id().
% The identifier of a menu item, possibly having a specific, named (standard or
% not) meaning and graphical representation.


-opaque menu_bar() :: wxMenuBar:wxMenuBar().
% A menu bar is a series of menus in a row, accessible from the top of a frame.
%
% When a frame goes fullscreen, any menu bar it has is hidden.

% Not existing? -type menu_bar_option() :: ''.


-export_type([ menu/0, menu_option/0, menu_style/0,  menu_item/0, menu_title/0,
			   menu_label/0, menu_item_label/0, menu_item_kind/0,
			   menu_item_status/0, standard_menu_item_name_id/0, menu_item_id/0,
			   menu_bar/0 ]).



% Menu life-cycle, to be used as a menu bar or a popup menu.
-export([ create/0, create/1, create/2, destruct/1 ]).


% Functions related to menus in general.
-export([ get_standard_item_names/0, add_item/2, add_item/3, append_item/2,
		  append_submenu/4, append_submenu/5,
		  add_checkable_item/2, add_checkable_item/3, add_checkable_item/4,
		  set_checkable_item/3,
		  add_radio_item/3, add_radio_item/4,
		  add_separator/1, set_item_status/3,
		  remove_menu_item/2 ]).


% Functions for menu bars.
-export([ create_bar/0, create_bar/1, add_menu/3, set_menu_bar/2 ]).


% Functions for popup menus.
-export([ activate_as_popup/2 ]).


% Exported helpers:
-export([ to_wx_menu_item_id/1, to_new_wx_menu_item_id/1,
		  to_wx_menu_item_kind/1 ]).



% Shorthands:

-type bit_mask() :: basic_utils:bit_mask().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type label() :: gui:label().
-type title() :: gui:title().
-type widget() :: gui:widget().

-type window() :: gui_window:window().

-type help_info() :: gui_text:help_info().

-type id() :: gui_id:id().
-type name_id() :: gui_id:name_id().

-type wx_id() :: gui_wx_backend:wx_id().
-type wx_enum() :: gui_wx_backend:wx_enum().



% @doc Creates a menu, either to be attached to a menu bar, or to be used as a
% popup menu.
%
% This is the most recommended and useful function to create a menu.
%
-spec create() -> menu().
create() ->
	wxMenu:new().


% @doc Creates a menu with the specified options, either to be attached to a
% menu bar, or to be used as a popup menu.
%
-spec create( maybe_list( menu_option() ) ) -> menu().
create( MaybeOptions ) ->
	WxOpts = to_wx_menu_options( MaybeOptions ),

	%trace_utils:debug_fmt( "Creating a menu with options ~p.",
	%                       [ WxOpts ] ),

	wxMenu:new( WxOpts ).


% @doc Creates a menu with the specified title and options, either to be
% attached to a menu bar, or to be used as a popup menu.
%
-spec create( title(), maybe_list( menu_option() ) ) -> menu().
create( Title, MaybeOptions ) ->
	WxOpts = to_wx_menu_options( MaybeOptions ),

	%trace_utils:debug_fmt( "Creating a menu '~ts' with options ~p.",
	%                       [ Title, WxOpts ] ),

	wxMenu:new( Title, WxOpts ).


% @doc Destructs the specified menu.
-spec destruct( menu() ) -> void().
destruct( Menu ) ->
	wxMenu:destroy( Menu ).




% Functions related to menus in general.


% @doc Returns a list of the names of the standard menu item identifiers.
-spec get_standard_item_names() -> [ name_id() ].
get_standard_item_names() ->
	% Must correspond to standard_menu_item_name_id() (could be obtained from
	% standard buttons as well):
	%
	 { menu_item_id, MenuItemEntries, _ElemLookup } =
		gui_constants:get_menu_item_id_topic_spec(),

	pair:firsts( MenuItemEntries ).


% @doc Creates a menu item based on the specified label, adds it to the
% specified menu, and returns that menu item.
%
-spec add_item( menu(), menu_item_label() ) -> menu_item().
add_item( Menu, MenuItemLabel ) ->
	add_item( Menu, _MenuItemId=undefined, MenuItemLabel ).



% @doc Creates a menu item based on the specified identifier and label, adds it
% to the specified menu, and returns that menu item.
%
% If using a standard (stock) menu item identifier (e.g. help_menu_item), an
% empty label can/should be specified, in which case it will be automatically
% set (e.g. as "Help").
%
-spec add_item( menu(), menu_item_id(), menu_item_label() ) -> menu_item().
% Now applies to all sorts of identifiers:
add_item( Menu, MenuItemId, MenuItemLabel ) -> %when is_integer( MenuItemId ) ->
	wxMenu:append( Menu, gui_id:declare_any_id( MenuItemId ), MenuItemLabel ).

%add_item( Menu, MenuItemId, MenuItemLabel ) when is_atom( MenuItemId ) ->
	% We must not declare a standard name identifier (as it already is):
	% BackendId = case lists:member( MenuItemId, get_standard_item_names() ) of

	%	true ->
	%		gui_id:resolve_any_id( MenuItemId );

	%	false ->
	%		gui_id:declare_any_id( MenuItemId )

	% end,

	% Now not predeclared anymore, was a bad idea:
%	BackendId = gui_id:declare_any_id( MenuItemId ),
%	wxMenu:append( Menu, BackendId, MenuItemLabel ).



% @doc Appends the specified already-created menu item (not a mere label) to the
% specified menu, and returns a possibly updated version of that menu item.
%
-spec append_item( menu(), menu_item() ) -> menu_item().
append_item( Menu, MenuItem ) ->
	wxMenu:append( Menu, MenuItem ).


% @doc Adds the specified labelled submenu, associated to the specified
% identifier, to the specified menu, and returns the corresponding menu item.
%
% If using a standard (stock) menu item identifier (e.g. help_menu_item), an
% empty label can/should be specified, in which case it will be automatically
% set (e.g. as "Help").
%
-spec append_submenu( menu(), menu_item_id(), menu_item_label(), menu() ) ->
													menu_item().
append_submenu( Menu, MenuItemId, MenuItemLabel, SubMenu ) ->
	% Not gui_id:resolve_any_id( MenuItemId ):
	wxMenu:append( Menu, gui_id:declare_any_id( MenuItemId ), MenuItemLabel,
				   SubMenu ).


% @doc Adds the specified labelled submenu, associated to the specified
% identifier and help information, to the specified menu, and returns that item.
%
% If using a standard (stock) menu item identifier (e.g. help_menu_item), an
% empty label can/should be specified, in which case it will be automatically
% set (e.g. as "Help").
%
-spec append_submenu( menu(), menu_item_id(), menu_item_label(), menu(),
					  help_info() ) -> menu_item().
append_submenu( Menu, MenuItemId, MenuItemLabel, SubMenu, HelpInfoStr ) ->
	wxMenu:append( Menu, gui_id:declare_any_id( MenuItemId ), MenuItemLabel,
				   SubMenu, [ { help, HelpInfoStr } ] ).


% @doc Creates a menu item that can be toggled/checked, based on the specified
% label, adds it to the specified menu, and returns that menu item.
%
-spec add_checkable_item( menu(), menu_item_label() ) -> menu_item().
add_checkable_item( Menu, MenuItemLabel ) ->
	add_checkable_item( Menu, _MenuItemId=undefined, MenuItemLabel ).


% @doc Creates a menu item that can be toggled/checked, based on the specified
% identifier and label, adds it to the specified menu, and returns that menu
% item.
%
% If using a standard (stock) menu item identifier (e.g. help_menu_item), an
% empty label can/should be specified, in which case it will be automatically
% set (e.g. as "Help").
%
-spec add_checkable_item( menu(), menu_item_id(), menu_item_label() ) ->
													menu_item().
add_checkable_item( Menu, MenuItemId, MenuItemLabel ) ->
	wxMenu:appendCheckItem( Menu, gui_id:declare_any_id( MenuItemId ),
							MenuItemLabel ).


% @doc Creates a menu item that can be toggled/checked, based on the specified
% identifier, label and help information, adds it to the specified menu, and
% returns that menu item.
%
% If using a standard (stock) menu item identifier (e.g. help_menu_item), an
% empty label can/should be specified, in which case it will be automatically
% set (e.g. as "Help").
%
-spec add_checkable_item( menu(), menu_item_id(), menu_item_label(),
						  help_info() ) -> menu_item().
add_checkable_item( Menu, MenuItemId, MenuItemLabel, HelpInfoStr ) ->
	wxMenu:appendCheckItem( Menu, gui_id:declare_any_id( MenuItemId ),
							MenuItemLabel, [ { help, HelpInfoStr } ] ).


% @doc Checks/unchecks the (checkable) menu item specified by its identifier
% (not by its menu item reference).
%
-spec set_checkable_item( menu(), menu_item_id(), boolean() ) -> void().
set_checkable_item( Menu, MenuItemId, SetAsChecked ) ->
	wxMenu:check( Menu, gui_id:resolve_any_id( MenuItemId ), SetAsChecked ).


% @doc Adds the specified radio item to the specified menu, and returns that
% item.
%
% All consequent radio items form a group and when an item in the group is
% checked, all the others are automatically unchecked.
%
% If using a standard (stock) menu item identifier (e.g. help_menu_item), an
% empty label can/should be specified, in which case it will be automatically
% set (e.g. as "Help").
%
-spec add_radio_item( menu(), menu_item_id(), menu_item_label() ) ->
													menu_item().
add_radio_item( Menu, MenuItemId, MenuItemLabel ) ->
	wxMenu:appendRadioItem( Menu, gui_id:resolve_any_id( MenuItemId ),
							MenuItemLabel ).


% @doc Adds the specified labelled item that can be checked to the specified
% menu, and returns that item.
%
% If using a standard (stock) menu item identifier (e.g. help_menu_item), an
% empty label can/should be specified, in which case it will be automatically
% set (e.g. as "Help").
%
-spec add_radio_item( menu(), menu_item_id(), menu_item_label(),
					  help_info() ) -> menu_item().
add_radio_item( Menu, MenuItemId, MenuItemLabel, HelpInfoStr ) ->
	wxMenu:appendRadioItem( Menu, gui_id:resolve_any_id( MenuItemId ),
							MenuItemLabel, [ { help, HelpInfoStr } ] ).


% @doc Adds a separator to the specified menu, and returns that separator.
-spec add_separator( menu() ) -> menu_item().
add_separator( Menu ) ->
	wxMenu:appendSeparator( Menu ).


% @doc Sets the enabled/disabled status of the specified menu item.
-spec set_item_status( menu(), menu_item_id(), menu_item_status() ) ->
														void().
set_item_status( Menu, MenuItemId, _NewEnableStatus=enabled ) ->
	wxMenu:enable( Menu, gui_id:resolve_any_id( MenuItemId ), _Check=true );

set_item_status( Menu, MenuItemId, _NewEnableStatus=disabled ) ->
	wxMenu:enable( Menu, gui_id:resolve_any_id( MenuItemId ), _Check=false ).


% @doc Removes the specified item from the specified menu.
%
% If the item is a submenu, it is removed yet not deallocated.
%
-spec remove_menu_item( menu(), menu_item_id() ) -> void().
remove_menu_item( Menu, MenuItemId ) ->
	wxMenu:delete( Menu, gui_id:resolve_any_id( MenuItemId ) ).



% Menu bar subsection.

% @doc Creates an empty menu bar, not yet specifically associated to a window.
-spec create_bar() -> menu_bar().
create_bar() ->
	wxMenuBar:new().


% @doc Creates an empty menu bar, associated to the specified window.
-spec create_bar( window() ) -> menu_bar().
create_bar( Window ) ->
	MenuBar = wxMenuBar:new(),
	gui_window:set_menu_bar( Window, MenuBar ),
	MenuBar.


% @doc Adds the specified menu to the specified menu bar.
-spec add_menu( menu_bar(), menu(), title() ) -> void().
add_menu( MenuBar, Menu, MenuTitle ) ->
	true = wxMenuBar:append( MenuBar, Menu, MenuTitle ).


% @doc Assigns the specified menu bar to the specified window.
%
% Note: to be deprecated soon; use gui_window:set_menu_bar/2 instead.
%
-spec set_menu_bar( menu_bar(), window() ) -> void().
set_menu_bar( MenuBar, Window ) ->
	% More logical that way:
	gui_window:set_menu_bar( Window, MenuBar ).



% Popup menu subsection.


% @doc Activates the specified menu as a popup one on the specified widget.
%
% Typically called on receiving of a onMouseRightButtonReleased event.
%
-spec activate_as_popup( menu(), widget() ) -> void().
activate_as_popup( Menu, Widget ) ->
	% Probably more logical to place it in gui_widget.

	% Meaning of returned boolean unclear:
	wxWindow:popupMenu( Widget, Menu ).



% Wx support.


% @doc Converts the specified menu option(s) into wx-specific ones.
-spec to_wx_menu_options( maybe_list( menu_option() ) ) -> list().
to_wx_menu_options( Options ) when is_list( Options ) ->
	[ to_wx_menu_option( O ) || O <- Options ];

to_wx_menu_options( Opt ) ->
	to_wx_menu_options( [ Opt ] ).


% (helper)
to_wx_menu_option( Opt=detachable ) ->
	{ style, menu_styles_to_bitmask( _MenuStyle=Opt ) }.



% @doc Converts the specified menu item identifier (for an already-existing menu
% item) into a wx-specific one.
%
-spec to_wx_menu_item_id( menu_item_id() ) -> wx_id().
to_wx_menu_item_id( MenuItemId ) ->

	%trace_utils:debug_fmt( "Testing availability: ~ts",
	%   [ code_utils:study_function_availability( gui_generated,
	%       get_maybe_second_for_menu_item_id, 1 ) ] ),

	case gui_generated:get_maybe_second_for_menu_item_id( MenuItemId ) of

		undefined ->
			gui_id:resolve_id( MenuItemId );

		WxId ->
			WxId

	end.


% @doc Converts the specified menu identifier for a new menu item into a
% wx-specific one.
%
-spec to_new_wx_menu_item_id( menu_item_id() ) -> wx_id().
to_new_wx_menu_item_id( MenuItemId ) ->
	case gui_generated:get_maybe_second_for_menu_item_id( MenuItemId ) of

		undefined ->
			gui_id:declare_id( MenuItemId );

		WxId ->
			WxId

	end.


% @doc Converts the specified kind of menu identifier into a wx-specific one.
-spec to_wx_menu_item_kind( menu_item_kind() ) -> wx_enum().
to_wx_menu_item_kind( Kind ) ->
	% Same:
	gui_generated:get_second_for_menu_item_kind( Kind ).



% @doc Converts the specified MyriadGUI menu style elements into the
% appropriate wx-specific bit mask.
%
% (helper)
%
-spec menu_styles_to_bitmask( [ menu_style() ] ) -> bit_mask().
menu_styles_to_bitmask( Styles ) ->
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_menu_style( S ) bor Acc
				 end,
				 _InitialAcc=0,
				 _List=Styles ).
