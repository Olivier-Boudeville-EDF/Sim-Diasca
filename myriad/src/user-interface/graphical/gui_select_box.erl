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
% Creation date: Sunday, December 8, 2024.

-module(gui_select_box).

-moduledoc """
Gathering of various facilities for **selection boxes**, which are widgets
displaying options that can be selected.

See also https://docs.wxwidgets.org/stable3.2/classwx_list_box.html

""".



-doc """
Designates an actual basic selection box instance.

When an item of such a selection box is selected, it emits an onItemSelected
event.

When an item is double-clicked, it emits an onItemDoubleClicked event.
""".
-opaque select_box() :: wxListBox:wxListBox().


% An element that can be selected with such a box.
-type item() :: text().


-export_type([ select_box/0, item/0 ]).


-doc "An option for the creation of a selection box.".
-type select_box_option() ::
	{ 'position', position() }
  | { 'size', size() }
  | { 'style', [ select_box_style() ] }.
  % No validator.


-doc """
A style element for the creation of a selection box.
""".
-type select_box_style() ::

	% These 3 are mutually exclusive:
	'single_selection' % Up to one item selected.
  | 'multiple_selection' % Multiple items can be selected.
  | 'extendable_selection' % Multiple items can be selected, included as a
						   % range.

  | 'horizontal_scroll_if_needed' % Creates a horizontal scrollbar if the items
								  % are too wide.
  | 'vertical_scroll_always' % Always shows a vertical scrollbar.
  | 'vertical_scroll_if_needed' % Only creates a vertical scrollbar if needed.
  | 'vertical_no_scroll' % Never creates a vertical scrollbar.

  | 'sorted'. % Sorts items in alphabetical order.



-export([ create/2, create/3, create/4, destruct/1 ]).


% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").


% Implementation notes:



% Type shorthands:


-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type bit_mask() :: type_utils:bit_mask().

-type text() :: ui:text().

-type parent() :: gui:parent().

-type position() :: gui:position().

-type size() :: gui:size().

-type id() :: gui_id:id().



-doc "Creates a selection box, with the specified items and parent.".
-spec create( [ item() ], parent() ) -> select_box().
create( Items, Parent ) ->

	Id = ?gui_any_id,

	Options = [ { choices, Items } ],

	trace_utils:info_fmt( "Selection box options (for any ID): ~p, "
						  "with parent ~w.", [ Options, Parent ] ),

	wxListBox:new( Parent, Id, Options ).



-doc """
Creates a selection box, with the specified items, identifier and parent.
""".
-spec create( [ item() ], id(), parent() ) -> select_box().
create( Items, Id, Parent ) ->

	Options = [ { choices, Items } ],

	BackendId = gui_id:declare_any_id( Id ),

	cond_utils:if_defined( myriad_debug_gui_select_boxes,
		trace_utils:debug_fmt( "Selection box options for ~ts "
			"(backend ~ts): ~p.",
			[ gui_id:id_to_string( Id ), gui_id:id_to_string( BackendId ),
			  Options ] ) ),

	%trace_utils:info_fmt( "Selection box options (for any ID): ~p.",
	%                      [ Id, Options ] ),

	wxListBox:new( Parent, BackendId, Options ).



-doc """
Creates a selection box, with the specified items, identifier, option(s) and
parent.
""".
-spec create( [ item() ], id(), maybe_list( select_box_option() ),
			  parent() ) -> select_box().
create( Items, Id, Opts, Parent ) ->

	WxOpts = [ { choices, Items } | to_wx_list_box_opts( Opts ) ],

	wxListBox:new( Parent, gui_id:declare_any_id( Id ), WxOpts ).



-doc """
Destructs the specified selection box.
""".
-spec destruct( select_box() ) -> void().
destruct( SelBox ) ->

	cond_utils:if_defined( myriad_debug_gui_select_boxes,
		trace_utils:debug_fmt( "Destructing selection box '~p'.",
							   [ SelBox ] ) ),

	wxListBox:destroy( SelBox ).



-doc "Converts the specified selection box options into wx-specific ones.".
-spec to_wx_list_box_opts( maybe_list( select_box_option() ) ) -> list().
to_wx_list_box_opts( SelBoxOpts ) when is_list( SelBoxOpts ) ->
	[ to_wx_list_box_opt( SBO ) || SBO <- SelBoxOpts ];

to_wx_list_box_opts( SelBoxOpt ) ->
	to_wx_list_box_opts( [ SelBoxOpt ] ).


-doc "Converts the specified selection box option into the wx-specific one.".
-spec to_wx_list_box_opt( select_box_option() ) -> tuple().
to_wx_list_box_opt( _SelBoxOpt={ position, Pos } ) ->
	{ pos, Pos };

to_wx_list_box_opt( _SelBoxOpt={ size, Size } ) ->
	{ sz, Size };

to_wx_list_box_opt( _SelBoxOpt={ style, SelBoxStyle } ) ->
	{ style, select_box_styles_to_bitmask( SelBoxStyle ) }.


% No validator supported.



-doc """
Converts the specified MyriadGUI selection box style elements into the
appropriate wx-specific bit mask.

(helper)
""".
-spec select_box_styles_to_bitmask( [ select_box_style() ] ) -> bit_mask().
select_box_styles_to_bitmask( Styles ) ->
	lists:foldl( fun( S, Acc ) ->
					gui_generated:get_second_for_select_box_style( S ) bor Acc
				 end,
				 _InitialAcc=0,
				 _List=Styles ).
