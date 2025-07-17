% Copyright (C) 2023-2025 Olivier Boudeville
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

-module(gui_button).

-moduledoc """
Gathering of various facilities for **button management**.

Usage notes:
- the parent of a button must be a widget (not a sizer for example)
- for a corresponding stock image/icon to be displayed within a button, its
  stock identifier shall of course be specified (e.g. 'about_button'), but, if
  specified, its label must also exactly match a built-in one (e.g. "About" or
  "&About") - otherwise no image will be added; for a stock button it is thus
  recommended to specify an empty label
- a button must be destructed according to its actual class: basic buttons by
  destruct/1, toggle buttons by destruct_toggle/1, bitmap ones by
  destruct_bitmap/1 (otherwise a {badtype,ActualButtonClass} exception will be
  thrown)
""".



-doc """
Designates an actual basic button instance.

When such a button is clicked, it emits an onButtonClicked event.
""".
-opaque button() :: wxButton:wxButton().



-doc "Any kind of reference onto a button.".
-type button_ref() :: button() | button_id().


-doc """
Designates a toggle button, that is a button that, when clicked by the user,
stays pressed.

It is therefore similar to a checkbox in functionality, but looks like a button.

When such a toggle button is clicked, it emits an onButtonToggled event.

It should be destructed thanks to destruct_toggle/1, not destruct/1.
""".
-opaque toggle_button() :: wxToggleButton:wxToggleButton().


-doc """
Designates an actual button instance displaying a bitmap instead of the usual
label.

Note that the button will adapt to any kind of bitmap size.

We recommend using SVG images, exported as PNG ones of the desired size.

It should be destructed thanks to destruct_bitmap/1, not destruct/1.
""".
-opaque bitmap_button() :: wxBitmapButton:wxBitmapButton().



-doc "Any kind of button.".
-opaque any_button() :: button() | toggle_button() | bitmap_button().


-export_type([ button/0, button_ref/0, toggle_button/0, bitmap_button/0,
			   any_button/0 ]).


-doc "An option for the creation of a button.".
-type button_option() ::
	{ 'label', label() }
  | { 'style', [ button_style() ] }
  | { 'position', point() }
  | { 'size', dimensions() }.



-doc """
A style element of a button, see
<http://docs.wxwidgets.org/stable/classwx_button.html>.
""".
-type button_style() ::
	'left_justified'
  | 'right_justified'
  | 'top_justified'
  | 'bottom_justified'
  | 'exact_fit'
  | 'flat'.



-doc "The identifiers of the buttons with icons added by MyriadGUI.".
-type myriadgui_button_id() ::
	'left_chevron_green_button' | 'right_chevron_green_button'
  | 'up_chevron_green_button' | 'down_chevron_green_button'.



-export_type([ button_option/0, button_style/0 ]).


-export([ create/2, create/3, create/4, create/6,
		  create_multiple/2,
		  create_toggle/3, create_toggle/4,
		  create_bitmap/3,
		  set_label/2, destruct/1, destruct_toggle/1, destruct_bitmap/1 ]).


% Internal use:
-export([ get_myriadgui_identifiers/0, is_myriadgui_identifier/1 ]).


% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").



% Implementation notes:

% Some function names defined here may seem a bit unclear, for example
% create_bitmap/* is defined instead of create_bitmap_button/*. However user
% code will call gui_button:create_bitmap/*, which is clear in itself.


% MyriadGUI emulates stock buttons by using ad-hoc bitmap buttons with icons
% stored in its own resources.
%
% In some cases we could see that a specialised button could not be be
% destructed with wxButton:destroy/1, however we could see that at least for the
% bitmap buttons that MyriadGUI uses, this works, probably in link with the fact
% that since wxWidgets 2.9.1 bitmap display is supported by the base wxButton
% class itself.


% The default number of pixels of the larger dimension of the (rectangular) icon
% of a MyriadGUI button is determined through a symlink (e.g. in priv/resources,
% move-blue.png may point to move-blue-24.png, for 24 pixels; it is generally
% 16.



% Type shorthands:

-type bit_mask() :: type_utils:bit_mask().

-type filename() :: file_utils:filename().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type parent() :: gui:parent().

% Note that an empty label ("") may be specified in order to force the use of a
% stock labels (if specifying a relevant identifier):
%
-type label() :: gui:label().

-type point() :: gui:point().
-type position() :: gui:position().
-type dimensions() :: gui:dimensions().
-type size() :: gui:size().

-type id() :: gui_id:id().

% Includes myriadgui_button_id():
-type button_id() :: gui_id:button_id().

-type bitmap() :: gui_image:bitmap().



-doc "Creates a (labelled) button, with the specified parent.".
-spec create( label(), parent() ) -> button().
create( Label, Parent ) ->

	Id = ?gui_any_id,

	Options = [ { label, Label } ],

	%trace_utils:info_fmt( "Button options (for any ID): ~p.",
	%                      [ Id, Options ] ),

	wxButton:new( Parent, Id, Options ).



-doc "Creates a (labelled) button, with the specified identifier and parent.".
-spec create( label(), id(), parent() ) -> button().
create( Label, Id, Parent ) ->

	Options = [ { label, Label } ],

	BackendId = gui_id:declare_any_id( Id ),

	cond_utils:if_defined( myriad_debug_gui_buttons,
		trace_utils:debug_fmt( "Button options for ~ts (backend ~ts): ~p.",
			[ gui_id:id_to_string( Id ), gui_id:id_to_string( BackendId ),
			  Options ] ) ),

	wxButton:new( Parent, BackendId, Options ).



-doc """
Creates a (labelled) button, with the specified identifier, option(s) and
parent.

No 'label' option is to be specified among the options.
""".
-spec create( label(), id(), maybe_list( button_option() ), parent() ) ->
											button().
create( Label, Id, Opts, Parent ) ->

	WxOpts = [ { label, Label } | to_wx_button_opts( Opts ) ],

	wxButton:new( Parent, gui_id:declare_any_id( Id ), WxOpts ).



-doc """
Creates a button, with the parent and most settings specified.

Specifying a standard button identifier (e.g. ok_button or select_color_button;
see gui_constants:get_button_id_topic_spec/1 for a complete list) will create a
corresponding button (with its standard stock icon).

Additionally, specifying a MyriadGUI button identifier
(e.g. left_chevron_button) will create a corresponding button (with its standard
MyriadGUI icon).

(mostly for internal use)
""".
% any_button(), as a standard button identifier will return a button(), whereas
% a MyriadGUI will return a bitmap_button():
%
-spec create( label(), position(), size(), [ button_style() ], id(),
			  parent() ) -> any_button().
create( Label, Position, Size, Styles, Id, Parent ) ->

	Options = [ { label, Label }, gui_wx_backend:to_wx_position( Position ),
				gui_wx_backend:to_wx_size( Size ),
				{ style, button_styles_to_bitmask( Styles ) } ],

	case is_myriadgui_identifier( Id ) of

		true ->
			% Here we have to emulate a stock button with one of our icons:

			ResDir = resource:get_builtin_directory(),

			IconPath = file_utils:join( ResDir, get_icon_filename_for( Id ) ),

			cond_utils:if_defined( myriad_debug_gui_buttons,
				trace_utils:debug_fmt( "For MyriadGUI button labelled '~ts' "
					"(~ts), icon path: '~ts'. Options: ~n ~p.",
					[ Label, gui_id:id_to_string( Id ), IconPath, Options ] ) ),

			IconBmp = gui_bitmap:create_from( IconPath ),

			% Identifier declared there:
			create_bitmap( IconBmp, Id, Parent );


		false ->
			BackendId = gui_id:declare_any_id( Id ),

			cond_utils:if_defined( myriad_debug_gui_buttons,
				trace_utils:debug_fmt( "For stock button labelled '~ts' "
					"(~ts), got ~ts. Options: ~n ~p.",
					[ Label, gui_id:id_to_string( Id ),
					  gui_id:id_to_string( BackendId ), Options ] ) ),

			wxButton:new( Parent, BackendId, Options )

	end.



-doc """
Creates (labelled) buttons, with their (single, common) parent specified.
""".
-spec create_multiple( [ label() ], parent() ) -> [ button() ].
% Not merged with create/2, as would not be clear enough.
create_multiple( Labels, Parent ) ->
	create_multiple_helper( Labels, Parent, _Acc=[] ).


% (helper)
create_multiple_helper( _Labels=[], _Parent, Acc ) ->
	lists:reverse( Acc );

create_multiple_helper( [ Label | T ], Parent, Acc ) ->
	NewButton = create( Label, Parent ),
	create_multiple_helper( T, Parent, [ NewButton | Acc ] ).



-doc """
Creates a (labelled) toggle button with the specified identifier and parent.
""".
-spec create_toggle( label(), id(), parent() ) -> toggle_button().
create_toggle( Label, Id, Parent ) ->
	wxToggleButton:new( Parent, gui_id:declare_any_id( Id ), Label ).



-doc """
Creates a (labelled) toggle button, with the specified identifier, option(s) and
parent.

No 'label' option is to be specified among the options.
""".
-spec create_toggle( label(), id(), maybe_list( button_option() ), parent() ) ->
											toggle_button().
create_toggle( Label, Id, Opts, Parent ) ->
	wxToggleButton:new( Parent, gui_id:declare_any_id( Id ), Label,
						to_wx_button_opts( Opts ) ).



-doc """
Creates a button with the specified identifier and that displays the specified
bitmap.

The button does not take ownership of the specified bitmap (it just increments
its reference count). As a result, the caller may destruct that bitmap just
afterwards.
""".
-spec create_bitmap( bitmap(), id(), parent() ) -> bitmap_button().
create_bitmap( Bitmap, Id, Parent ) ->
	% The button does not take the ownership of the bitmap:
	B = wxBitmapButton:new( Parent, gui_id:declare_any_id( Id ), Bitmap ),

	% Would still return a functional button:
	%gui_bitmap:destruct( Bitmap ),

	B.



-doc "Sets the label of the specified button.".
-spec set_label( button(), label() ) -> void().
set_label( Button, Label ) ->
	wxButton:setLabel( Button, Label ).



-doc """
Destructs the specified basic button.

Apparently is able to deallocate bitmap_button() instances as well.
""".
-spec destruct( button() ) -> void().
destruct( Button ) ->

	cond_utils:if_defined( myriad_debug_gui_buttons,
		trace_utils:debug_fmt( "Destructing basic button '~p'.", [ Button ] ) ),

	wxButton:destroy( Button ).



-doc "Destructs the specified toggle button.".
-spec destruct_toggle( toggle_button() ) -> void().
destruct_toggle( Button ) ->
	wxToggleButton:destroy( Button ).



-doc "Destructs the specified bitmap button.".
-spec destruct_bitmap( bitmap_button() ) -> void().
destruct_bitmap( Button ) ->
	wxBitmapButton:destroy( Button ).




% Helper section.


-doc "Returns a list of all known MyriadGUI standard button identifiers.".
-spec get_myriadgui_identifiers() -> [ myriadgui_button_id() ].
get_myriadgui_identifiers() ->
	[ left_chevron_green_button, right_chevron_green_button,
	  up_chevron_green_button, down_chevron_green_button,

	  left_double_chevron_green_button, right_double_chevron_green_button,
	  up_double_chevron_green_button, down_double_chevron_green_button,

	  left_arrow_green_button, right_arrow_green_button,
	  up_arrow_green_button, down_arrow_green_button,

	  left_arrow_red_button, right_arrow_red_button,
	  up_arrow_red_button, down_arrow_red_button,

	  left_arrow_black_button, right_arrow_black_button,
	  up_arrow_black_button, down_arrow_black_button,

	  move_blue_button, move_black_button,
	  rotate_black_button, rotate_ccw_blue_button, rotate_cw_blue_button,

	  scale_black_button,

	  jump_to_black_button, outbound_black_button, inbound_black_button,
	  orthographic_black_button, perspective_black_button,
	  add_bookmark_black_button, go_to_bookmark_black_button,

	  prompt_black_button ].



-doc """
Tells whether the specified button identifier (e.g. down_chevron_button) is a
MyriadGUI one.
""".
-spec is_myriadgui_identifier( button_id() ) -> boolean().
is_myriadgui_identifier( Id ) ->
	lists:member( Id, get_myriadgui_identifiers() ).



-doc """
Returns the filename of the icon corresponding to the specified MyriadGUI button
identifier.

For example, for an 'left_chevron_green_button' argument, may return
"left-chevron-green-16.png".
""".
-spec get_icon_filename_for( myriadgui_button_id() ) -> filename().
get_icon_filename_for( Id ) ->
	IdStr = text_utils:atom_to_string( Id ),
	Elems = text_utils:split_per_element( IdStr, _Separators=[ $_ ] ),

	% With a check:
	{ "button", FirstElems } = list_utils:extract_last_element( Elems ),

	DashStr = text_utils:join( _Sep=$-, FirstElems ),

	% Now relying on symlinks to select the default size:
	%text_utils:format( "~ts-~B.png", [ DashStr, ?button_icon_size ] ).
	DashStr ++ ".png".



-doc "Converts the specified button options into wx-specific ones.".
-spec to_wx_button_opts( maybe_list( button_option() ) ) -> list().
to_wx_button_opts( ButtonOpts ) when is_list( ButtonOpts ) ->
	[ to_wx_button_opt( BO ) || BO <- ButtonOpts ];

to_wx_button_opts( ButtonOpt ) ->
	to_wx_button_opts( [ ButtonOpt ] ).


-doc "Converts the specified button option into the wx-specific one.".
-spec to_wx_button_opt( button_option() ) -> tuple().
to_wx_button_opt( ButtonOpt={ label, _Label } ) ->
	ButtonOpt;

to_wx_button_opt( _ButtonOpt={ style, ButtonStyle } ) ->
	{ style, button_styles_to_bitmask( ButtonStyle ) };

to_wx_button_opt( _ButtonOpt={ position, Pos } ) ->
	{ pos, Pos };

to_wx_button_opt( _ButtonOpt={ size, Size } ) ->
	{ sz, Size }.

% validator not supported apparently.



-doc """
Converts the specified MyriadGUI button style elements into the appropriate
wx-specific bit mask.

(helper)
""".
-spec button_styles_to_bitmask( [ button_style() ] ) -> bit_mask().
button_styles_to_bitmask( Styles ) ->
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_button_style( S ) bor Acc
				 end,
				 _InitialAcc=0,
				 _List=Styles ).
