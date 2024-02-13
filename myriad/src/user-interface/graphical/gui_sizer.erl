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


% @doc Gathering of various facilities for <b>sizer management</b>.
%
% The purpose of a sizer (which is not a widget) is to drive the layout of the
% child widgets of a given widget (window, panel, etc.), by dynamically resizing
% them.
%
% Various types of sizers are available:
%  - normal sizers (one-dimension), that are either horizontal or vertical
%  virtual containers
%  - variations thereof, rendered in a box and possibly with a label
%  - grid sizers that are two-dimension sizers, either normal or compact
%
% Once created, a sizer is to be associated to a widget, typically thanks to
% gui_widget:set_sizer/2. A widget can be resized to best fit its sizer, see
% gui_widget:fit_to_sizer/2.
%
% Beyond child elements, a sizer may include spacers, which are empty areas used
% to define better layouts.
%
-module(gui_sizer).



-opaque sizer() :: wxSizer:wxSizer().
% A vertical and/or horizontal virtual (not a widget) container whose elements
% are dynamically resized.
%
% These elements will be rendered in their addition order.
%
% A sizer should not be considered as a widget (e.g. it does not handle events),
% it is just an helper component to compute the respective sizes of the widgets
% that it registered.


-opaque grid_sizer() :: wxGridSizer:wxGridSizer().
% A grid sizer is a sizer that lays out its children in a two-dimensional table
% with all table fields having the same size, i.e. the width of each field is
% the width of the widest child, the height of each field is the height of the
% tallest child.


-opaque compact_grid_sizer() :: wxFlexGridSizer:wxFlexGridSizer().
% A grid sizer trying to be as compact as possible.
%
% Indeed the width of each column and the height of each row are calculated
% individually, according to the minimal requirements from the respectively
% biggest child.
%
% Additionally, columns and rows can be declared to be stretchable if the sizer
% is assigned a size different from the one it requested.


-type sizer_child() :: widget() | sizer().
% An element that can be included in a sizer.
%
% Sizers can thus be included in sizers.


-opaque sizer_item() :: wxSizerItem:wxSizerItem().
% An object in charge of tracking the position, size and other attributes of an
% element of a sizer.


-type sizer_flag_opt() ::
	'default'
  | 'top_border'
  | 'bottom_border'
  | 'left_border'
  | 'right_border'
  | 'all_borders'
  | 'expand_fully'
  | 'expand_shaped'
  | 'fixed_size'
  | 'counted_even_if_hidden'
  | 'align_center'
  | 'align_left'
  | 'align_right'
  | 'align_top'
  | 'align_bottom'
  | 'align_center_vertical'
  | 'align_center_horizontal'.
% A configuration option flag for a sizer.
%
% See [https://docs.wxwidgets.org/stable/classwx_sizer.html].


% Simplification for the user: no specific flag needed.
-type sizer_option() :: { 'proportion', weight_factor() }

						% Applies iff at least one border is requested:
					  | { 'border_width', width() }

					  | { 'user_data', term() }
					  | sizer_flag_opt().
% An option when configuring a sizer.



-type weight_factor() :: non_neg_integer().
% A weight factor akin to a proportion, typically of an element (widget or
% sizer) in a sizer.
%
% 0 means that the corresponding element is fixed-sized (in the direction of the
% sizer), whereas strictly positive values specify the weight (expansion factor)
% of that element relative to the other elements.



-export_type([ sizer/0, grid_sizer/0, compact_grid_sizer/0,
			   sizer_child/0, sizer_item/0,
			   sizer_flag_opt/0,
			   sizer_option/0, weight_factor/0 ]).


-export([ create/1, create_with_box/2,
		  create_with_labelled_box/3, create_with_labelled_box/4,
		  create_grid/4, create_compact_grid/4,

		  add_element/2, add_element/3,
		  add_elements/2, add_elements/3,
		  add_spacer/2, add_spacer/4,
		  add_stretchable_spacer/1,

		  clear/1, clear/2 ]).


% To improve:
-type wx_sizer_options() :: [ sizer_option() ].


% Shorthands:

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type bit_mask() :: basic_utils:bit_mask().

-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type length() :: gui:length().
-type height() :: gui:height().
-type width() :: gui:width().
-type orientation() :: gui:orientation().
-type label() :: gui:label().
-type widget() :: gui:widget().
-type parent() :: gui:parent().
-type row_count() :: gui:row_count().
-type column_count() :: gui:column_count().



% Implementation notes:
%
% wxSizer is an abstract class.
%
% Base sizers correspond actually to wxBoxSizer.
%
% Sizers surrounded by a box (possibly with a label) are wxStaticBoxSizer
% instances.
%
% Null size (zero width/height) may mean that a sizer element (e.g. a spacer) is
% stretchable; conversely, a zero proportion will mean non-stretchable.
%
% Refer to https://docs.wxwidgets.org/stable/overview_sizer.html#overview_sizer
% for details.



% Creation section.


% @doc Creates a sizer operating along the specified orientation.
-spec create( orientation() ) -> sizer().
create( Orientation ) ->
	ActualOrientation = gui_wx_backend:to_wx_orientation( Orientation ),
	wxBoxSizer:new( ActualOrientation ).



% To have a box created, a parent must be specified.

% @doc Creates a sizer operating along the specified orientation, within
% specified parent, with a box drawn around.
%
-spec create_with_box( orientation(), parent() ) -> sizer().
create_with_box( Orientation, Parent ) ->
	ActualOrientation = gui_wx_backend:to_wx_orientation( Orientation ),
	wxStaticBoxSizer:new( ActualOrientation, Parent, _Opts=[] ).



% @doc Creates a sizer operating along the specified orientation, within the
% specified parent, with a box drawn around bearing the specified label.
%
-spec create_with_labelled_box( orientation(), label(), parent() ) -> sizer().
create_with_labelled_box( Orientation, Label, Parent ) ->
	ActualOrientation = gui_wx_backend:to_wx_orientation( Orientation ),
	wxStaticBoxSizer:new( ActualOrientation, Parent, [ { label, Label } ] ).


% @doc Creates a sizer operating along the specified orientation, within the
% specified parent, with a box drawn around bearing the specified formatted
% label.
%
-spec create_with_labelled_box( orientation(), format_string(), format_values(),
								parent() ) -> sizer().
create_with_labelled_box( Orientation, FormatString, FormatValues, Parent ) ->
	Label = text_utils:format( FormatString, FormatValues ),
	create_with_labelled_box( Orientation, Label, Parent ).


% @doc Creates a (basic) grid sizer.
%
% A zero count requires that the sizer is automatically adjusting in the
% corresponding dimension its number of elements, depending on the number of its
% widget children.
%
-spec create_grid( row_count(), column_count(), width(), height() ) ->
											grid_sizer().
create_grid( RowCount, ColumnCount, HorizGap, VertGap ) ->
	% Note the gap swap:
	wxGridSizer:new( RowCount, ColumnCount, VertGap, HorizGap ).


% @doc Creates a compact grid sizer.
%
% A zero count requires that the sizer is automatically adjusting in the
% corresponding dimension its number of elements, depending on the number of its
% widget children.
%
-spec create_compact_grid( row_count(), column_count(), width(), height() ) ->
											compact_grid_sizer().
create_compact_grid( RowCount, ColumnCount, HorizGap, VertGap ) ->
	% Note the gap swap:
	wxFlexGridSizer:new( RowCount, ColumnCount, VertGap, HorizGap ).



% @doc Adds the specified element to the specified sizer, and returns the
% corresponding item.
%
-spec add_element( sizer(), sizer_child() ) -> sizer_item().
add_element( Sizer, _Element={ myriad_object_ref, myr_canvas, CanvasId } ) ->
	gui:get_main_loop_pid() ! { getPanelForCanvas, CanvasId, self() },
	receive

		{ notifyCanvasPanel, AssociatedPanel } ->
			add_element( Sizer, AssociatedPanel )

	end;

add_element( Sizer, Element ) ->
	%trace_utils:debug_fmt( "Adding ~w to sizer ~w.", [ Element, Sizer ] ),
	wxSizer:add( Sizer, Element ).


% @doc Adds the specified element to the specified sizer with the specified
% option(s), and returns the corresponding item.
%
-spec add_element( sizer(), sizer_child(), maybe_list( sizer_option() ) ) ->
											sizer_item().
add_element( Sizer, _Element={ myriad_object_ref, myr_canvas, CanvasId },
			 Options ) ->
	gui:get_main_loop_pid() ! { getPanelForCanvas, CanvasId, self() },
	receive

		{ notifyCanvasPanel, AssociatedPanel } ->
			add_element( Sizer, AssociatedPanel, Options )

	end;

add_element( Sizer, Element, Options ) ->
	ActualOptions = to_wx_sizer_options( Options ),

	%trace_utils:debug_fmt( "Adding ~w to sizer ~w with options ~w "
	%    "(i.e. ~w).", [ Element, Sizer, Options, ActualOptions ] ),

	wxSizer:add( Sizer, Element, ActualOptions ).



% @doc Adds the specified elements, possibly specified with option(s), to the
% specified sizer, and returns the corresponding items.
%
-spec add_elements( sizer(),
		[ sizer_child() | { sizer_child(), maybe_list( sizer_option() ) } ] ) ->
											[ sizer_item() ].
add_elements( _Sizer, _Elements=[] ) ->
	[];

add_elements( Sizer, _Elements=[ { Elem, Opts } | T ] ) ->
	Item = add_element( Sizer, Elem, Opts ),
	[ Item | add_elements( Sizer, T ) ];

add_elements( Sizer, _Elements=[ Elem | T ] ) ->
	Item = add_element( Sizer, Elem ),
	[ Item | add_elements( Sizer, T ) ].



% @doc Adds the specified elements, with the specified common option(s), to the
% specified sizer.
%
-spec add_elements( sizer(), [ sizer_child() ],
					maybe_list( sizer_option() ) ) -> [ sizer_item() ].
add_elements( _Sizer, _Elements=[], _Options ) ->
	[];

add_elements( Sizer, _Elements=[ Elem | T ], Options ) ->
	Item = add_element( Sizer, Elem, Options ),
	[ Item | add_elements( Sizer, T, Options ) ].



% @doc Adds to the specified sizer a fixed-size (non-stretchable) spacer child
% of the specified length in both dimensions (hence is square), and returns the
% corresponding sizer item.
%
-spec add_spacer( sizer(), length() ) -> sizer_item().
add_spacer( Sizer, Length ) ->
	wxSizer:addSpacer( Sizer, _Size=Length ).


% @doc Adds to the specified sizer a spacer child of the specified dimensions,
% and returns the corresponding item.
%
-spec add_spacer( sizer(), width(), height(), maybe_list( sizer_option() ) ) ->
											sizer_item().
add_spacer( Sizer, Width, Height, Options ) ->
	ActualOptions = to_wx_sizer_options( Options ),
	% Not addSpacer:
	wxSizer:add( Sizer, Width, Height, ActualOptions ).


% @doc Adds to the specified sizer a stretchable spacer child, and returns the
% corresponding sizer item.
%
-spec add_stretchable_spacer( sizer() ) -> sizer_item().
add_stretchable_spacer( Sizer ) ->
	wxSizer:addStretchSpacer( Sizer ).



% @doc Clears the specified sizer, detaching and deleting all its child widgets.
-spec clear( sizer() ) -> void().
clear( Sizer ) ->
	clear( Sizer, _DeleteChildWidgets=true ).


% @doc Clears the specified sizer, detaching all its child widgets, and deleting
% them iff requested.
%
-spec clear( sizer(), boolean() ) -> void().
clear( Sizer, DeleteChildWidgets ) ->
	wxSizer:clear( Sizer, [ { delete_windows, DeleteChildWidgets } ] ).



% @doc Converts the specified sizer options into wx-specific ones.
%
% (helper)
%
-spec to_wx_sizer_options( maybe_list( sizer_option() ) ) -> wx_sizer_options().
to_wx_sizer_options( Options ) when is_list( Options ) ->
	to_wx_sizer_options( Options, _AccOpts=[], _AccFlags=[] );

to_wx_sizer_options( Option ) ->
	to_wx_sizer_options( [ Option ] ).



% (helper)
to_wx_sizer_options( _Options=[], AccOpts, _AccFlags=[]  ) ->
	AccOpts;

to_wx_sizer_options( _Options=[], AccOpts, AccFlags ) ->
	[ { flag, sizer_flags_to_bitmask( AccFlags ) } | AccOpts ];

to_wx_sizer_options( _Options=[ { userData, Data } | T ], AccOpts, AccFlags ) ->
	to_wx_sizer_options( T, [ { user_data, Data } | AccOpts ], AccFlags );

to_wx_sizer_options( _Options=[ { border_width, W } | T ], AccOpts,
					 AccFlags ) ->
	to_wx_sizer_options( T, [ { border, W } | AccOpts ], AccFlags );

% Letting {proportion, integer()} go through:
to_wx_sizer_options( _Options=[ P | T ], AccOpts, AccFlags )
											when is_tuple( P ) ->
	to_wx_sizer_options( T, [ P | AccOpts ], AccFlags );

to_wx_sizer_options( _Options=[ F | T ], AccOpts, AccFlags ) ->
	to_wx_sizer_options( T, AccOpts, [ F | AccFlags ] ).



% @doc Converts the specified MyriadGUI sizer flag options into the appropriate
% wx-specific bit mask.
%
% (helper)
%
-spec sizer_flags_to_bitmask( maybe_list( sizer_option() ) ) -> bit_mask().
sizer_flags_to_bitmask( FlagOpts ) when is_list( FlagOpts ) ->
	lists:foldl( fun( F, Acc ) ->
					gui_generated:get_second_for_sizer_flag( F ) bor Acc end,
				 _InitialAcc=0,
				 _List=FlagOpts );

sizer_flags_to_bitmask( FlagOpt ) ->
	gui_generated:get_second_for_sizer_flag( FlagOpt ).
