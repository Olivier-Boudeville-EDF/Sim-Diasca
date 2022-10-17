% Copyright (C) 2017-2022 Olivier Boudeville
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
% Creation date: Wednesday, October 4, 2017.


% @doc Gathers all elements relative to the (Erlang) <b>wx backend</b> version
% 2.1 (itself based on [wxWidgets](https://www.wxwidgets.org/)).
%
-module(gui_wx_backend).



% Usually a class of wxWidgets is represented as a module in Erlang.
%
% GUI objects (e.g. widgets) correspond to (Erlang) processes. They are
% designated here with gui_object() values, which are references, either to wx
% objects or, in some cases, to MyriadGUI-defined ones.

% In wx, the user code handles wx:wx_object() instances, which are actually just
% references onto the actual instances that are stored internally by wx.
%
% We replicate this behaviour with the myriad_object_ref() type, referencing a
% MyriadGUI-object (most probably made from wx ones, like in the case of the
% canvas).
%
% MyriadGUI objects are created with {new,create}/*, and, when appropriate,
% deleted with destruct/1.
%
% For information regarding events, refer to gui_event.erl.



% Understanding the wx base widgets hierarchy:
%
% (offsets meaning "inheriting from", corresponding local types specified
% between brackets; a hierarchy is a truncated view as a wx class may inherit
% from more than one parent one):
%
% .
% ├── wxObject
% │   ├── wxEvtHandler
% │   │   └── wxWindow
% │   │       ├── wxControl
% │   │       │   └── wxAnyButton
% │   │       │       └── wxButton
% │   │       ├── wxPanel
% │   │       ├── wxStatusBar
% │   │       └── wxTopLevelWindow
% │   │           ├── wxDialog
% │   │           └── wxFrame
% │   └── wxSizer
% └── wxTrackable


% In a more detailed view, with the direct parent classes listed between
% parentheses, and with the corresponding MyriadGUI types being specified
% between brackets:
%
% - wxObject() [gui_object]: root class of all wxWidgets classes
%
%   - wxEvtHandler(wxObject) [event_handler]: a class that can handle events
%   from the windowing system
%
%      - wxWindow(wxEvtHandler) [window]: the base class for all windows and
%      represents any visible object on screen For colors, refer to the color
%      module
%
%        - wxControl(wxWindow): base class for a control or
%        "widget"; generally a small window which processes user input and/or
%        displays one or more item of data
%
%          - wxButton(wxControl): a control that contains a text string, and is
%          one of the most common elements of a GUI; may be placed on any
%          window, notably dialog boxes or panels
%
%        - wxPanel(wxWindow) [panel]: a window on which controls are placed;
%        usually placed within a frame
%
%        - wxTopLevelWindow(wxWindow): abstract base class for wxDialog and
%        wxFrame
%
%           - wxDialog(wxTopLevelWindow): a window with a title bar and
%           sometimes a system menu, which can be moved around the screen; often
%           used to allow the user to make some choice or to answer a question
%
%           - wxFrame(wxTopLevelWindow) [frame]: a window whose size and
%           position can (usually) be changed by the user. It usually has thick
%           borders and a title bar, and can optionally contain a menu bar,
%           toolbar and status bar. A frame can contain any window that is not a
%           frame or dialog
%
%      - wxStatusBar(wxEvtHandler): narrow window that can be placed along the
%      bottom of a frame
%
%   - wxSizer(wxClientDataContainer): abstract base class used for laying out
%   subwindows in a window


% See also all the classes of wxwidgets:
% https://docs.wxwidgets.org/3.0/page_class_cat.html and
% https://docs.wxwidgets.org/3.0/classes.html


% Additional widgets
%
% Some types of widgets seem to be lacking to wxWidgets, such as canvases that
% would be first-level citizens (ex: able to emit and receive events, when
% needing repaint or being resized).
%
% To support them, we defined the myriad_object_ref record to complement the
% wx_object() type (all widgets are thus gui_object(), meaning either
% wx:wx_object() or myriad_object_ref()), and we maintain our own instance table
% of the instances of the additional gui_object types we defined.
%
% For the actual mode of operation, we mimic the mode of operation of wx; to
% find the GUI server, no naming service is used, instead the process dictionary
% stores a (MyriadGUI) environment (like the wx one).


% Function export section.


-export([ get_wx_version/0 ]).


% Conversions from MyriadGUI to wx:
-export([ to_wx_object_type/1,
		  to_wx_event_type/2, from_wx_event_type/2,
		  to_wx_connect_options/3,
		  to_wx_debug_level/1,

		  window_style_to_bitmask/1, get_window_options/1,
		  frame_style_to_bitmask/1,
		  get_panel_options/1,
		  button_style_to_bitmask/1,

		  to_wx_sizer_options/1, sizer_flag_to_bitmask/1,

		  to_wx_menu_options/1,
		  to_wx_menu_item_id/1, to_wx_menu_item_maybe_id/1,
		  to_new_wx_menu_item_id/1,
		  to_wx_menu_item_kind/1,
		  %to_wx_menu_item_options/1,

		  to_wx_bitmap_id/1, to_wx_icon_id/1,

		  to_wx_status_bar_style/1, to_wx_toolbar_style/1,
		  to_wx_tool_kind/1,


		  to_wx_id/1, to_wx_parent/1, to_wx_position/1, to_wx_size/1,
		  to_wx_orientation/1, wx_id_to_window/1, wx_id_to_string/1,

		  to_wx_device_context_attributes/1,

		  connect/2, connect/4, connect/5, disconnect/1, disconnect/3 ]).


% Conversions from wx to MyriadGUI:
-export([ from_wx_object_type/1 ]).


% For wx defines:
-include("gui_internal_defines.hrl").


% Type section.


% Default position, chosen by either the windowing system or wxWidgets,
% depending on platform:
%
-define( wx_default_position, { -1, -1 } ).

-type wx_position() :: { 'pos', gui:point() }.


% Default size, chosen by either the windowing system or wxWidgets,
% depending on platform:
%
-define( wx_default_size, { -1, -1 } ).

-type wx_size() :: { 'size', gui:size() }.


-type wx_orientation() :: ?wxVERTICAL | ?wxHORIZONTAL.


-type wx_id() :: maybe( integer() ).
% The identifier (ID) of a wx element is an integer (positive or not).
%
% This identifier (ex: 63) is relative to a given type, like in:
% {wx_ref,63,wxFrame,[]}.
%
% This type allows to specify a 'void' (null) ID of a GUI element.
%
% Sometimes the ID may be directly provided by the user or have a predefined
% value, such as wxID_OPEN; see
% [http://docs.wxwidgets.org/2.8.12/wx_stockitems.html#stockitems] for a list
% thereof.
%
% Often, however, the value of the ID is unimportant and in this case it is
% enough to use wxID_ANY as the ID of an object which tells wxWidgets to assign
% an ID automatically.
%
% All such automatically-assigned IDs are negative, so the IDs predefined in the
% user code should always be positive to avoid clashes with them.
%
% More generally, wx identifiers (rather than wx references) should be used only
% internally.
%
% Note: this type is defined and exported, yet reported unknown by Dialyzer.


% See any_id, no_parent, etc. as defined in gui.hrl.



-type wx_native_object_type() :: atom().
% Native wx object types (ex: 'wxFrame').
%
% No enumeration like 'wxWindow' | 'wxFrame' | ... found in wx.



-type wx_window_option() :: term().

-type wx_event_handler_option() :: { 'id', integer() }
								 | { 'lastId', integer() }
								 | { 'skip', boolean() }
								 | 'callback'
								 | {'callback', function() }
								 | {'userData', term() }.
% Refer to https://erlang.org/doc/man/wxEvtHandler.html
% See the corresponding gui:connect_opt().


-type wx_panel_option() :: wx_window_option() | wx_event_handler_option().

% Precisely:
%    {id, integer()} |
%    {pos, {X :: integer(), Y :: integer()}} |
%    {size, {W :: integer(), H :: integer()}} |
%    {style, integer()} |
%    {name, unicode:chardata()} |
%    {palette, wxPalette:wxPalette()}
%
-type other_wx_device_context_attribute() :: atom_entry().


-type wx_device_context_attribute() ::
		{ 'attribList', integer() } | other_wx_device_context_attribute().
% Refer to wxGLCanvas: https://www.erlang.org/doc/man/wxglcanvas#new-2.


-type wx_enum() :: wx:wx_enum().
% A wxWidgets enumerated value.


-export_type([ wx_native_object_type/0, wx_window_option/0,
			   wx_event_handler_option/0, wx_panel_option/0,
			   other_wx_device_context_attribute/0,
			   wx_device_context_attribute/0, wx_enum/0 ]).

-type wx_art_id() :: unicode:chardata().
% Ex: "wxART_NEW".


% Preferably no '-export_type' here to avoid leakage of backend conventions.


% Shorthands:

-type bit_mask() :: basic_utils:bit_mask().

-type maybe_list(T) :: list_utils:maybe_list( T ).

-type atom_entry() :: hashtable:atom_entry().

-type ustring() :: text_utils:ustring().


-type wx_object_type() :: gui:wx_object_type().
-type myriad_object_type() :: gui:myriad_object_type().
-type window() :: gui:window().
-type window_style() :: gui:window_style().
-type window_option() :: gui:window_option().
-type frame_style() :: gui:frame_style().
-type panel_option() :: gui:panel_option().
-type button_style() :: gui:button_style().

-type bitmap_name_id() :: gui:bitmap_name_id().
-type icon_name_id() :: gui:icon_name_id().

-type sizer_options() :: gui:sizer_options().
-type sizer_flag_opt() :: gui:sizer_flag_opt().

-type menu_option() :: gui:menu_option().
-type menu_item_id() :: gui:menu_item_id().
-type menu_item_kind() :: gui:menu_item_kind().

-type status_bar_style() :: gui:status_bar_style().

-type toolbar_style() :: gui:toolbar_style().
-type tool_kind() :: gui:tool_kind().

-type position() :: gui:position().
-type size() :: gui:size().
-type orientation() :: gui:orientation().
-type connect_opt() :: gui:connect_opt().
-type connect_options() :: gui:connect_options().

-type myriad_instance_id() :: gui_id:myriad_instance_id().

-type wx_event_type() :: gui_event:wx_event_type().
-type event_type() :: gui_event:event_type().
-type event_source() :: gui_event:event_source().
-type trap_set() :: gui_event:trap_set().
-type event_translation_table() :: gui_event:event_translation_table().

-type device_context_attribute() :: gui_opengl:device_context_attribute().

% To improve:
-type wx_sizer_options() :: sizer_options().


% To avoid unused warnings:
-export_type([ wx_id/0 ]).


% For canvas_state():
-include("gui_canvas.hrl").



% Implementation section.
%
% Generally conversions are listed by themes, and in each theme first from
% MyriadGUI to wx (to_wx_*), then from wx to MyriadGUI (from_wx_*), to
% facilitate the consistency of the two-way definitions.



% Object type section.


% @doc Converts a MyriadGUI type of object into a wx one.
-spec to_wx_object_type( myriad_object_type() ) -> wx_object_type().
to_wx_object_type( object ) ->
	wxObject;

to_wx_object_type( event_handler ) ->
	wxEvtHandler;

to_wx_object_type( window ) ->
	wxWindow;

to_wx_object_type( control ) ->
	wxControl;

to_wx_object_type( button ) ->
	wxButton;

to_wx_object_type( panel ) ->
	wxPanel;

to_wx_object_type( status_bar ) ->
	wxStatusBar;

to_wx_object_type( top_level_window ) ->
	wxTopLevelWindow;

to_wx_object_type( dialog ) ->
	wxDialog;

to_wx_object_type( frame ) ->
	wxFrame;

to_wx_object_type( sizer ) ->
	wxSizer;

to_wx_object_type( bitmap ) ->
	wxBitmap;

to_wx_object_type( memory_device_context ) ->
	wxMemoryDC;

to_wx_object_type( _Other ) ->
	%throw( { unsupported_object_type, Other } ).
	unknown_wx_object_type.




% @doc Converts a wx type of object into a MyriadGUI one.
-spec from_wx_object_type( wx_object_type() ) -> myriad_object_type().
from_wx_object_type( wxObject ) ->
	object;

from_wx_object_type( wxEvtHandler ) ->
	event_handler;

from_wx_object_type( wxWindow ) ->
	window;

from_wx_object_type( wxControl ) ->
	control;

from_wx_object_type( wxButton ) ->
	button;

from_wx_object_type( wxPanel ) ->
	panel;

from_wx_object_type( wxStatusBar ) ->
	status_bar;

from_wx_object_type( wxTopLevelWindow ) ->
	top_level_window;

from_wx_object_type( wxDialog ) ->
	dialog;

from_wx_object_type( wxFrame ) ->
	frame;

from_wx_object_type( wxSizer ) ->
	sizer;

from_wx_object_type( wxBitmap ) ->
	bitmap;

from_wx_object_type( wxMemoryDC ) ->
	memory_device_context;

from_wx_object_type( _Other ) ->
%	throw( { unsupported_wx_object_type, Other } ).
	unknown_wx_object_type.


% @doc Returns the build-time version of wx (wxWidgets).
-spec get_wx_version() -> basic_utils:four_digit_version().
get_wx_version() ->
	{ ?wxMAJOR_VERSION, ?wxMINOR_VERSION, ?wxRELEASE_NUMBER,
	  ?wxSUBRELEASE_NUMBER }.




% Event type section.


% @doc Converts a MyriadGUI type of event into a wx one.
-spec to_wx_event_type( event_type(), event_translation_table() ) ->
											wx_event_type().
to_wx_event_type( EventType, EventTranslationTable ) ->
	bijective_table:get_first_for( EventType, EventTranslationTable ).


% @doc Converts a wx type of event into a MyriadGUI one.
-spec from_wx_event_type( wx_event_type(), event_translation_table() ) ->
														event_type().
from_wx_event_type( WxEventType, EventTranslationTable ) ->
	bijective_table:get_second_for( WxEventType, EventTranslationTable ).


%
% Section for the conversions from MyriadGUI to wx.
%


% Debug section.


% @doc Converts the debug level from MyriadGUI to the one of wx.
%
% (helper)
%
to_wx_debug_level( _DebugLevel=none ) ->
	none;

to_wx_debug_level( _DebugLevel=calls ) ->
	trace;

to_wx_debug_level( _DebugLevel=life_cycle ) ->
	driver.




% Windows section.


% @doc Converts specified MyriadGUI window style into the appropriate
% wx-specific bit mask.
%
% (helper)
%
-spec window_style_to_bitmask( window_style() ) -> bit_mask().
window_style_to_bitmask( StyleList ) when is_list( StyleList ) ->
	lists:foldl( fun( S, Acc ) -> window_style_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

window_style_to_bitmask( _Style=default_border ) ->
	?wxBORDER_SIMPLE;

window_style_to_bitmask( _Style=simple_border ) ->
	?wxBORDER_SIMPLE;

window_style_to_bitmask( _Style=sunken_border ) ->
	?wxBORDER_SUNKEN;

window_style_to_bitmask( _Style=raised_border ) ->
	?wxBORDER_RAISED;

window_style_to_bitmask( _Style=static_border ) ->
	?wxSTATIC_BORDER;

window_style_to_bitmask( _Style=theme_border ) ->
	?wxBORDER_THEME;

window_style_to_bitmask( _Style=no_border ) ->
	?wxBORDER_NONE;

window_style_to_bitmask( _Style=double_border ) ->
	?wxBORDER_DOUBLE;


window_style_to_bitmask( _Style=transparent ) ->
	?wxTRANSPARENT_WINDOW;

window_style_to_bitmask( _Style=tab_traversable ) ->
	?wxTAB_TRAVERSAL;

window_style_to_bitmask( _Style=grab_all_keys ) ->
	?wxWANTS_CHARS;

window_style_to_bitmask( _Style=with_vertical_scrollbar ) ->
	?wxVSCROLL;

window_style_to_bitmask( _Style=with_horizontal_scrollbar ) ->
	?wxHSCROLL;

window_style_to_bitmask( _Style=never_hide_scrollbars ) ->
	?wxALWAYS_SHOW_SB;

window_style_to_bitmask( _Style=clip_children ) ->
	?wxCLIP_CHILDREN;

window_style_to_bitmask( _Style=full_repaint_on_resize ) ->
	% Forces a complete redraw of the window whenever it is resized instead of
	% redrawing just the part of the window affected by resizing:
	% (see https://docs.wxwidgets.org/3.0/classwx_window.html)
	%
	?wxFULL_REPAINT_ON_RESIZE.



% @doc Converts the specified MyriadGUI window option(s) into the appropriate
% wx-specific options.
%
% (exported helper)
%
-spec get_window_options( maybe_list( window_option() ) ) ->
								[ wx_window_option() ].
get_window_options( Options ) when is_list( Options ) ->
	get_window_options( Options, _Acc=[] );

get_window_options( Option ) ->
	get_window_options( [ Option ] ).



get_window_options( _Options=[], Acc ) ->
	Acc;

get_window_options( _Options=[ { style, Style } | T ], Acc ) ->
	get_window_options( T,
					[ { style, window_style_to_bitmask( Style ) } | Acc ] );

% Unchanged:
get_window_options( _Options=[ H | T ], Acc ) ->
	get_window_options( T, [ H | Acc ] ).



% Frames section.


% @doc Converts specified MyriadGUI frame style into the appropriate wx-specific
% bit mask.
%
% (helper)
%
-spec frame_style_to_bitmask( frame_style() ) -> bit_mask().
frame_style_to_bitmask( StyleList ) when is_list( StyleList ) ->

	% 'bor ?wxWANTS_CHARS' not desirable a priori:
	lists:foldl( fun( S, Acc ) -> frame_style_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

frame_style_to_bitmask( _Style=default ) ->
	?wxDEFAULT_FRAME_STYLE;

frame_style_to_bitmask( _Style=caption ) ->
	?wxCAPTION;

% Useless 'minimize' (Windows-only);
%frame_style_to_bitmask( _Style=minimize ) ->
%	?wxMINIMIZE;

frame_style_to_bitmask( _Style=minimize_icon ) ->
	?wxMINIMIZE_BOX;

% Useless 'maximize' (Windows-only);
%frame_style_to_bitmask( _Style=maximize ) ->
%	?wxMAXIMIZE;

frame_style_to_bitmask( _Style=maximize_icon ) ->
	?wxMAXIMIZE_BOX;

frame_style_to_bitmask( _Style=close_icon ) ->
	?wxCLOSE_BOX;

frame_style_to_bitmask( _Style=stay_on_top ) ->
	?wxSTAY_ON_TOP;

frame_style_to_bitmask( _Style=system_menu ) ->
	?wxSYSTEM_MENU;

frame_style_to_bitmask( _Style=resize_border ) ->
	?wxRESIZE_BORDER;

frame_style_to_bitmask( _Style=tool_window ) ->
	?wxFRAME_TOOL_WINDOW;

frame_style_to_bitmask( _Style=no_taskbar ) ->
	?wxFRAME_NO_TASKBAR;

frame_style_to_bitmask( _Style=float_on_parent ) ->
	?wxFRAME_FLOAT_ON_PARENT;

frame_style_to_bitmask( _Style=shaped ) ->
	?wxFRAME_SHAPED.




% Panels section.


% @doc Converts specified MyriadGUI panel option(s) into the appropriate
% wx-specific options.
%
% (exported helper)
%
-spec get_panel_options( maybe_list( panel_option() ) ) ->
											[ wx_panel_option() ].
get_panel_options( Options ) ->
	get_window_options( Options ).




% Buttons section.


% @doc Converts specified MyriadGUI button style into the appropriate
% wx-specific bit mask.
%
% (helper)
%
-spec button_style_to_bitmask( button_style() ) -> bit_mask().
button_style_to_bitmask( StyleList ) when is_list( StyleList ) ->

	lists:foldl( fun( S, Acc ) -> button_style_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

button_style_to_bitmask( _Style=default ) ->
	0;

button_style_to_bitmask( _Style=left_justified ) ->
	?wxBU_LEFT;

button_style_to_bitmask( _Style=right_justified ) ->
	?wxBU_RIGHT;

button_style_to_bitmask( _Style=top_justified ) ->
	?wxBU_TOP;

button_style_to_bitmask( _Style=bottom_justified ) ->
	?wxBU_BOTTOM;

button_style_to_bitmask( _Style=exact_fit ) ->
	?wxBU_EXACTFIT;

button_style_to_bitmask( _Style=flat ) ->
	throw( not_implemented ).




% Sizers section.



% @doc Converts specified sizer options into wx-specific ones.
%
% (helper)
%
-spec to_wx_sizer_options( maybe_list( sizer_options() ) ) ->
											wx_sizer_options().
to_wx_sizer_options( Options ) when is_list( Options ) ->
	to_wx_sizer_options( Options, _AccOpts=[], _AccFlags=[] );

to_wx_sizer_options( Option ) ->
	to_wx_sizer_options( [ Option ] ).



% (helper)
to_wx_sizer_options( _Options=[], AccOpts, _AccFlags=[]  ) ->
	AccOpts;

to_wx_sizer_options( _Options=[], AccOpts, AccFlags ) ->
	[ { flag, sizer_flag_to_bitmask( AccFlags ) } | AccOpts ];

to_wx_sizer_options( _Options=[ { userData, Data } | T ], AccOpts, AccFlags ) ->
	to_wx_sizer_options( T, [ { user_data, Data } | AccOpts ], AccFlags );

% Letting {proportion, integer()} and {border, integer()} go through:
to_wx_sizer_options( _Options=[ P | T ], AccOpts, AccFlags )
											when is_tuple( P ) ->
	to_wx_sizer_options( T, [ P | AccOpts ], AccFlags );

to_wx_sizer_options( _Options=[ F | T ], AccOpts, AccFlags ) ->
	to_wx_sizer_options( T, AccOpts, [ F | AccFlags ] ).


% @doc Converts specified MyriadGUI sizer flag option(s) into the appropriate
% wx-specific bit mask.
%
% (helper)
%
-spec sizer_flag_to_bitmask( sizer_flag_opt() | [ sizer_flag_opt() ] ) ->
											bit_mask().
sizer_flag_to_bitmask( FlagList ) when is_list( FlagList ) ->

	lists:foldl( fun( F, Acc ) -> sizer_flag_to_bitmask( F ) bor Acc end,
				 _InitialAcc=0,
				 _List=FlagList );

sizer_flag_to_bitmask( _Flag=default ) ->
	0;

sizer_flag_to_bitmask( _Flag=top_border ) ->
	?wxTOP;

sizer_flag_to_bitmask( _Flag=bottom_border ) ->
	?wxBOTTOM;

sizer_flag_to_bitmask( _Flag=left_border ) ->
	?wxLEFT;

sizer_flag_to_bitmask( _Flag=right_border ) ->
	?wxRIGHT;

sizer_flag_to_bitmask( _Flag=all_borders ) ->
	?wxALL;

sizer_flag_to_bitmask( _Flag=expand_fully ) ->
	?wxEXPAND;

sizer_flag_to_bitmask( _Flag=expand_shaped ) ->
	?wxSHAPED;

sizer_flag_to_bitmask( _Flag=fixed_size ) ->
	?wxFIXED_MINSIZE;

sizer_flag_to_bitmask( _Flag=counted_even_if_hidden ) ->
	?wxRESERVE_SPACE_EVEN_IF_HIDDEN;

sizer_flag_to_bitmask( _Flag=align_center ) ->
	?wxALIGN_CENTER;

sizer_flag_to_bitmask( _Flag=align_left ) ->
	?wxALIGN_LEFT;

sizer_flag_to_bitmask( _Flag=align_right ) ->
	?wxALIGN_RIGHT;

sizer_flag_to_bitmask( _Flag=align_top ) ->
	?wxALIGN_TOP;

sizer_flag_to_bitmask( _Flag=align_bottom ) ->
	?wxALIGN_BOTTOM;

sizer_flag_to_bitmask( _Flag=align_center_vertical ) ->
	?wxALIGN_CENTER_VERTICAL;

sizer_flag_to_bitmask( _Flag=align_center_horizontal ) ->
	?wxALIGN_CENTER_HORIZONTAL.



% @doc Converts specified menu option(s) into wx-specific ones.
-spec to_wx_menu_options( maybe_list( menu_option() ) ) -> list().
to_wx_menu_options( Options ) when is_list( Options ) ->
	[ to_wx_menu_option( O ) || O <- Options ];

to_wx_menu_options( Opt ) ->
	[ to_wx_menu_option( Opt ) ].


% (helper)
to_wx_menu_option( _Opt=detachable ) ->
	?wxMENU_TEAROFF.



% @doc Converts the specified menu item identifier (for an already-existing menu
% item) into a wx-specific one.
%
-spec to_wx_menu_item_id( menu_item_id() ) -> wx_id().
to_wx_menu_item_id( MenuId ) ->
	case to_wx_menu_item_maybe_id( MenuId ) of

		undefined ->
			gui_id:resolve_id( MenuId );

		WxId ->
			WxId

	end.


% @doc Converts the specified menu identifier for a new menu item into a
% wx-specific one.
%
-spec to_new_wx_menu_item_id( menu_item_id() ) -> wx_id().
to_new_wx_menu_item_id( MenuId ) ->
	case to_wx_menu_item_maybe_id( MenuId ) of

		undefined ->
			gui_id:declare_id( MenuId );

		WxId ->
			WxId

	end.


% Converts wx standard menu item identifiers.
%
% (helper)
-spec to_wx_menu_item_maybe_id( menu_item_id() ) -> maybe( wx_id() ).
to_wx_menu_item_maybe_id( _MenuId=new_menu_item ) ->
	?wxID_NEW;

to_wx_menu_item_maybe_id( _MenuId=open_menu_item ) ->
	?wxID_OPEN;

to_wx_menu_item_maybe_id( _MenuId=close_menu_item ) ->
	?wxID_CLOSE;

to_wx_menu_item_maybe_id( _MenuId=save_menu_item ) ->
	?wxID_SAVE;

to_wx_menu_item_maybe_id( _MenuId=save_as_menu_item ) ->
	?wxID_SAVEAS;

to_wx_menu_item_maybe_id( _MenuId=revert_to_saved_menu_item ) ->
	?wxID_REVERT_TO_SAVED;

to_wx_menu_item_maybe_id( _MenuId=undelete_menu_item ) ->
	?wxID_UNDELETE;

to_wx_menu_item_maybe_id( _MenuId=print_menu_item ) ->
	?wxID_PRINT;

to_wx_menu_item_maybe_id( _MenuId=preview_menu_item ) ->
	?wxID_PREVIEW;

to_wx_menu_item_maybe_id( _MenuId=revert_menu_item ) ->
	?wxID_REVERT;

to_wx_menu_item_maybe_id( _MenuId=edit_menu_item ) ->
	?wxID_EDIT;

to_wx_menu_item_maybe_id( _MenuId=file_menu_item ) ->
	?wxID_FILE;

to_wx_menu_item_maybe_id( _MenuId=properties_menu_item ) ->
	?wxID_PROPERTIES;

to_wx_menu_item_maybe_id( _MenuId=cut_menu_item ) ->
	?wxID_CUT;

to_wx_menu_item_maybe_id( _MenuId=copy_menu_item ) ->
	?wxID_COPY;

to_wx_menu_item_maybe_id( _MenuId=paste_menu_item ) ->
	?wxID_PASTE;

to_wx_menu_item_maybe_id( _MenuId=delete_menu_item ) ->
	?wxID_DELETE;

to_wx_menu_item_maybe_id( _MenuId=find_menu_item ) ->
	?wxID_FIND;

to_wx_menu_item_maybe_id( _MenuId=select_all_menu_item ) ->
	?wxID_SELECTALL;

to_wx_menu_item_maybe_id( _MenuId=replace_menu_item ) ->
	?wxID_REPLACE;

to_wx_menu_item_maybe_id( _MenuId=replace_all_menu_item ) ->
	?wxID_REPLACE_ALL;

to_wx_menu_item_maybe_id( _MenuId=clear_menu_item ) ->
	?wxID_CLEAR;

to_wx_menu_item_maybe_id( _MenuId=ok_menu_item ) ->
	?wxID_OK;

to_wx_menu_item_maybe_id( _MenuId=cancel_menu_item ) ->
	?wxID_CANCEL;

to_wx_menu_item_maybe_id( _MenuId=apply_menu_item ) ->
	?wxID_APPLY;

to_wx_menu_item_maybe_id( _MenuId=yes_menu_item ) ->
	?wxID_YES;

to_wx_menu_item_maybe_id( _MenuId=no_menu_item ) ->
	?wxID_NO;

to_wx_menu_item_maybe_id( _MenuId=add_menu_item ) ->
	?wxID_ADD;

to_wx_menu_item_maybe_id( _MenuId=convert_menu_item ) ->
	?wxID_CONVERT;

to_wx_menu_item_maybe_id( _MenuId=execute_menu_item ) ->
	?wxID_EXECUTE;

to_wx_menu_item_maybe_id( _MenuId=remove_menu_item ) ->
	?wxID_REMOVE;

to_wx_menu_item_maybe_id( _MenuId=home_menu_item ) ->
	?wxID_HOME;

to_wx_menu_item_maybe_id( _MenuId=refresh_menu_item ) ->
	?wxID_REFRESH;

to_wx_menu_item_maybe_id( _MenuId=stop_menu_item ) ->
	?wxID_STOP;

to_wx_menu_item_maybe_id( _MenuId=index_menu_item ) ->
	?wxID_INDEX;

to_wx_menu_item_maybe_id( _MenuId=select_color_menu_item ) ->
	?wxID_SELECT_COLOR;

to_wx_menu_item_maybe_id( _MenuId=select_font_menu_item ) ->
	?wxID_SELECT_FONT;

to_wx_menu_item_maybe_id( _MenuId=forward_menu_item ) ->
	?wxID_FORWARD;

to_wx_menu_item_maybe_id( _MenuId=backward_menu_item ) ->
	?wxID_BACKWARD;

to_wx_menu_item_maybe_id( _MenuId=up_menu_item ) ->
	?wxID_UP;

to_wx_menu_item_maybe_id( _MenuId=down_menu_item ) ->
	?wxID_DOWN;

to_wx_menu_item_maybe_id( _MenuId=top_menu_item ) ->
	?wxID_TOP;

to_wx_menu_item_maybe_id( _MenuId=bottom_menu_item ) ->
	?wxID_BOTTOM;

to_wx_menu_item_maybe_id( _MenuId=first_menu_item ) ->
	?wxID_FIRST;

to_wx_menu_item_maybe_id( _MenuId=last_menu_item ) ->
	?wxID_LAST;

to_wx_menu_item_maybe_id( _MenuId=jump_to_menu_item ) ->
	?wxID_JUMP_TO;

to_wx_menu_item_maybe_id( _MenuId=info_menu_item ) ->
	?wxID_INFO;

to_wx_menu_item_maybe_id( _MenuId=zoom_factor_one ) ->
	?wxID_ZOOM_100;

to_wx_menu_item_maybe_id( _MenuId=zoom_factor_fit ) ->
	?wxID_ZOOM_FIT;

to_wx_menu_item_maybe_id( _MenuId=zoom_factor_in ) ->
	?wxID_ZOOM_IN;

to_wx_menu_item_maybe_id( _MenuId=zoom_factor_out ) ->
	?wxID_ZOOM_OUT;

to_wx_menu_item_maybe_id( _MenuId=undo_menu_item ) ->
	?wxID_UNDO;

to_wx_menu_item_maybe_id( _MenuId=redo_menu_item ) ->
	?wxID_REDO;

to_wx_menu_item_maybe_id( _MenuId=help_menu_item ) ->
	?wxID_HELP;

to_wx_menu_item_maybe_id( _MenuId=preferences_menu_item ) ->
	?wxID_PREFERENCES;

to_wx_menu_item_maybe_id( _MenuId=about_menu_item ) ->
	?wxID_ABOUT;

to_wx_menu_item_maybe_id( _MenuId=floppy_menu_item ) ->
	?wxID_FLOPPY;

to_wx_menu_item_maybe_id( _MenuId=hard_disk_menu_item ) ->
	?wxID_HARDDISK;

to_wx_menu_item_maybe_id( _MenuId=network_menu_item ) ->
	?wxID_NETWORK;

to_wx_menu_item_maybe_id( _MenuId=exit_menu_item ) ->
	?wxID_EXIT;

to_wx_menu_item_maybe_id( _MenuId=undefined ) ->
	?wxID_ANY;

to_wx_menu_item_maybe_id( MenuId ) when is_integer( MenuId ) ->
	MenuId;

to_wx_menu_item_maybe_id( _Other )  ->
	undefined.



% @doc Converts the specified bitmap identifier (for an already-existing menu
% item) into a wx-specific one.
%
-spec to_wx_bitmap_id( bitmap_name_id() ) -> wx_art_id().
to_wx_bitmap_id( BitmapId ) ->
	case to_wx_bitmap_maybe_id( BitmapId ) of

		undefined ->
			throw( { unknown_bitmap_id, BitmapId } );

		WxArtId ->
			WxArtId

	end.


% Converts wx standard bitmap identifiers.
%
% (helper)
-spec to_wx_bitmap_maybe_id( bitmap_name_id() ) -> maybe( wx_art_id() ).
to_wx_bitmap_maybe_id( _BitmapId=error_bitmap ) ->
	"wxART_ERROR";

to_wx_bitmap_maybe_id( _BitmapId=question_bitmap ) ->
	"wxART_QUESTION";

to_wx_bitmap_maybe_id( _BitmapId=warning_bitmap ) ->
	"wxART_WARNING";

to_wx_bitmap_maybe_id( _BitmapId=information_bitmap ) ->
	"wxART_INFORMATION";

to_wx_bitmap_maybe_id( _BitmapId=add_bookmark_bitmap ) ->
	"wxART_ADD_BOOKMARK";

to_wx_bitmap_maybe_id( _BitmapId=delete_bookmark_bitmap ) ->
	"wxART_DEL_BOOKMARK";

to_wx_bitmap_maybe_id( _BitmapId=help_side_panel_bitmap ) ->
	"wxART_HELP_SIDE_PANEL";

to_wx_bitmap_maybe_id( _BitmapId=help_settings_bitmap ) ->
	"wxART_HELP_SETTINGS";

to_wx_bitmap_maybe_id( _BitmapId=help_book_bitmap ) ->
	"wxART_HELP_BOOK";

to_wx_bitmap_maybe_id( _BitmapId=help_folder_bitmap ) ->
	"wxART_HELP_FOLDER";

to_wx_bitmap_maybe_id( _BitmapId=help_page_bitmap ) ->
	"wxART_HELP_PAGE";

to_wx_bitmap_maybe_id( _BitmapId=go_back_bitmap ) ->
	"wxART_GO_BACK";

to_wx_bitmap_maybe_id( _BitmapId=go_forward_bitmap ) ->
	"wxART_GO_FORWARD";

to_wx_bitmap_maybe_id( _BitmapId=go_up_bitmap ) ->
	"wxART_GO_UP";

to_wx_bitmap_maybe_id( _BitmapId=go_down_bitmap ) ->
	"wxART_GO_DOWN";

to_wx_bitmap_maybe_id( _BitmapId=go_to_parent_bitmap ) ->
	"wxART_GO_TO_PARENT";

to_wx_bitmap_maybe_id( _BitmapId=go_home_bitmap ) ->
	"wxART_GO_HOME";

to_wx_bitmap_maybe_id( _BitmapId=goto_first_bitmap ) ->
	"wxART_GOTO_FIRST";

to_wx_bitmap_maybe_id( _BitmapId=goto_last_bitmap ) ->
	"wxART_GOTO_LAST";

to_wx_bitmap_maybe_id( _BitmapId=print_bitmap ) ->
	"wxART_PRINT";

to_wx_bitmap_maybe_id( _BitmapId=help_bitmap ) ->
	"wxART_HELP";

to_wx_bitmap_maybe_id( _BitmapId=tip_bitmap ) ->
	"wxART_TIP";

to_wx_bitmap_maybe_id( _BitmapId=report_view_bitmap ) ->
	"wxART_REPORT_VIEW";

to_wx_bitmap_maybe_id( _BitmapId=list_view_bitmap ) ->
	"wxART_LIST_VIEW";

to_wx_bitmap_maybe_id( _BitmapId=new_folder_bitmap ) ->
	"wxART_NEW_DIR";

to_wx_bitmap_maybe_id( _BitmapId=folder_bitmap ) ->
	"wxART_FOLDER";

to_wx_bitmap_maybe_id( _BitmapId=open_folder_bitmap ) ->
	"wxART_FOLDER_OPEN";

to_wx_bitmap_maybe_id( _BitmapId=go_folder_up_bitmap ) ->
	"wxART_GO_DIR_UP";

to_wx_bitmap_maybe_id( _BitmapId=executable_file_bitmap ) ->
	"wxART_EXECUTABLE_FILE";

to_wx_bitmap_maybe_id( _BitmapId=normal_file_bitmap ) ->
	"wxART_NORMAL_FILE";

to_wx_bitmap_maybe_id( _BitmapId=tick_mark_bitmap ) ->
	"wxART_TICK_MARK";

to_wx_bitmap_maybe_id( _BitmapId=cross_mark_bitmap ) ->
	"wxART_CROSS_MARK";

to_wx_bitmap_maybe_id( _BitmapId=missing_image_bitmap ) ->
	"wxART_MISSING_IMAGE";

to_wx_bitmap_maybe_id( _BitmapId=new_bitmap ) ->
	"wxART_NEW";

to_wx_bitmap_maybe_id( _BitmapId=file_open_bitmap ) ->
	"wxART_FILE_OPEN";

to_wx_bitmap_maybe_id( _BitmapId=file_save_bitmap ) ->
	"wxART_FILE_SAVE";

to_wx_bitmap_maybe_id( _BitmapId=file_save_as_bitmap ) ->
	"wxART_FILE_SAVE_AS";

to_wx_bitmap_maybe_id( _BitmapId=file_delete_bitmap ) ->
	"wxART_DELETE";

to_wx_bitmap_maybe_id( _BitmapId=copy_bitmap ) ->
	"wxART_COPY";

to_wx_bitmap_maybe_id( _BitmapId=cut_bitmap ) ->
	"wxART_CUT";

to_wx_bitmap_maybe_id( _BitmapId=paste_bitmap ) ->
	"wxART_PASTE";

to_wx_bitmap_maybe_id( _BitmapId=undo_bitmap ) ->
	"wxART_UNDO";

to_wx_bitmap_maybe_id( _BitmapId=redo_bitmap ) ->
	"wxART_REDO";

to_wx_bitmap_maybe_id( _BitmapId=plus_bitmap ) ->
	"wxART_PLUS";

to_wx_bitmap_maybe_id( _BitmapId=minus_bitmap ) ->
	"wxART_MINUS";

to_wx_bitmap_maybe_id( _BitmapId=close_bitmap ) ->
	"wxART_CLOSE";

to_wx_bitmap_maybe_id( _BitmapId=quit_bitmap ) ->
	"wxART_QUIT";

to_wx_bitmap_maybe_id( _BitmapId=find_bitmap ) ->
	"wxART_FIND";

to_wx_bitmap_maybe_id( _BitmapId=find_and_replace_bitmap ) ->
	"wxART_FIND_AND_REPLACE";

to_wx_bitmap_maybe_id( _BitmapId=full_screen_bitmap ) ->
	"wxART_FULL_SCREEN";

to_wx_bitmap_maybe_id( _BitmapId=edit_bitmap ) ->
	"wxART_EDIT";

to_wx_bitmap_maybe_id( _BitmapId=hard_disk_bitmap ) ->
	"wxART_HARDDISK";

to_wx_bitmap_maybe_id( _BitmapId=floppy_bitmap ) ->
	"wxART_FLOPPY";

to_wx_bitmap_maybe_id( _BitmapId=cdrom_bitmap ) ->
	"wxART_CDROM";

to_wx_bitmap_maybe_id( _BitmapId=removable_bitmap ) ->
	"wxART_REMOVABLE";

to_wx_bitmap_maybe_id( _BitmapId=backend_logo_bitmap ) ->
	"wxART_WX_LOGO";

to_wx_bitmap_maybe_id( _OtherBitmapId ) ->
	undefined.



% @doc Converts the specified icon identifier into a wx-specific one.
-spec to_wx_icon_id( icon_name_id() ) -> wx_art_id().
to_wx_icon_id( IconId ) ->
	case to_wx_icon_maybe_id( IconId ) of

		undefined ->
			throw( { unknown_icon_id, IconId } );

		WxIconId ->
			WxIconId

	end.


% Converts wx standard bitmap identifiers.
%
% (helper)
-spec to_wx_icon_maybe_id( icon_name_id() ) -> maybe( wx_art_id() ).
to_wx_icon_maybe_id( _IconId=asterisk_icon ) ->
	?wxICON_ASTERISK;

to_wx_icon_maybe_id( _IconId=stop_icon ) ->
	?wxICON_STOP;

to_wx_icon_maybe_id( _IconId=information_icon ) ->
	?wxICON_INFORMATION;

to_wx_icon_maybe_id( _IconId=question_icon ) ->
	?wxICON_QUESTION;

to_wx_icon_maybe_id( _IconId=error_icon ) ->
	?wxICON_ERROR;

to_wx_icon_maybe_id( _IconId=warning_icon ) ->
	?wxICON_WARNING;

to_wx_icon_maybe_id( _IconId=hand_icon ) ->
	?wxICON_HAND;

to_wx_icon_maybe_id( _IconId=exclamation_icon ) ->
	?wxICON_EXCLAMATION;

to_wx_icon_maybe_id( _OtherIconId ) ->
	undefined.


%to_wx_menu_item_options( _Opt=detachable ) ->


% @doc Converts the specified kind of menu identifier into a wx-specific one.
-spec to_wx_menu_item_kind( menu_item_kind() ) -> wx_enum().
to_wx_menu_item_kind( _Kind=normal ) ->
	?wxITEM_NORMAL;

to_wx_menu_item_kind( _Kind=toggle ) ->
	?wxITEM_CHECK;

to_wx_menu_item_kind( _Kind=radio ) ->
	?wxITEM_RADIO;

to_wx_menu_item_kind( _Kind=separator ) ->
	?wxITEM_SEPARATOR;

to_wx_menu_item_kind( _Kind=dropdown ) ->
	?wxITEM_DROPDOWN.

% No ?wxITEM_MAX




% @doc Converts the specified style of status bar into a wx-specific one.
-spec to_wx_status_bar_style( status_bar_style() ) -> wx_enum().
to_wx_status_bar_style( _StatusBarStyle=normal ) ->
	?wxSB_NORMAL;

to_wx_status_bar_style( _StatusBarStyle=flat ) ->
	?wxSB_FLAT;

to_wx_status_bar_style( _StatusBarStyle=raised ) ->
	?wxSB_RAISED;

to_wx_status_bar_style( _StatusBarStyle=sunken ) ->
	?wxSB_SUNKEN.



% @doc Converts the specified style of toolbar into a wx-specific one.
-spec to_wx_toolbar_style( maybe_list( toolbar_style() ) ) -> wx_enum().
to_wx_toolbar_style( ToolbarStyles ) when is_list( ToolbarStyles ) ->
	lists:foldl( fun( S, Acc ) -> to_wx_toolbar_style( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=ToolbarStyles );

to_wx_toolbar_style( _ToolbarStyle=top ) ->
	?wxTB_TOP;

to_wx_toolbar_style( _ToolbarStyle=bottom ) ->
	?wxTB_BOTTOM;

to_wx_toolbar_style( _ToolbarStyle=left ) ->
	?wxTB_VERTICAL;

to_wx_toolbar_style( _ToolbarStyle=right ) ->
	?wxTB_RIGHT;

to_wx_toolbar_style( _ToolbarStyle=flat ) ->
	?wxTB_FLAT;

to_wx_toolbar_style( _ToolbarStyle=dockable ) ->
	?wxTB_DOCKABLE;

to_wx_toolbar_style( _ToolbarStyle=no_icons ) ->
	?wxTB_NOICONS;

to_wx_toolbar_style( _ToolbarStyle=text ) ->
	?wxTB_TEXT;

to_wx_toolbar_style( _ToolbarStyle=no_divider ) ->
	?wxTB_NODIVIDER;

to_wx_toolbar_style( _ToolbarStyle=no_align ) ->
	?wxTB_NOALIGN;

to_wx_toolbar_style( _ToolbarStyle=horizontal_layout ) ->
	?wxTB_HORZ_LAYOUT;

to_wx_toolbar_style( _ToolbarStyle=no_tooltips ) ->
	?wxTB_NO_TOOLTIPS;

to_wx_toolbar_style( _ToolbarStyle=default ) ->
	?wxTB_DEFAULT_STYLE.


% @doc Converts the specified kind of tool into a wx-specific one.
-spec to_wx_tool_kind( tool_kind() ) -> wx_enum().
to_wx_tool_kind( ToolKind ) ->
	to_wx_menu_item_kind( ToolKind ).



% @doc Converts specified MyriadGUI identifier in a wx-specific widget
% identifier.
%
% (helper)
%
-spec to_wx_id( maybe( myriad_instance_id() ) ) -> wx_id().
to_wx_id( undefined ) ->
	?any_id;

to_wx_id( Other ) ->
	Other.



% @doc Converts the specified MyriadGUI identifier into a wx-specific parent
% widget identifier.
%
% (helper)
%
-spec to_wx_parent( maybe( myriad_instance_id() ) ) -> wx_id().
to_wx_parent( undefined ) ->
	?no_parent;

to_wx_parent( Other ) ->
	Other.



% @doc Converts specified MyriadGUI position in a wx-specific position (with
% defaults).
%
% (helper)
%
-spec to_wx_position( position() ) -> wx_position().
to_wx_position( _Position=auto ) ->
	{ pos, ?wx_default_position };

to_wx_position( Position ) ->
	{ pos, Position }.



% @doc Converts specified MyriadGUI size in a wx-specific size (with defaults).
%
% (helper)
%
-spec to_wx_size( size() ) -> wx_size().
to_wx_size( _Size=auto ) ->
	{ size, ?wx_default_size };

%to_wx_size( Size={ _X, _Y } ) ->
to_wx_size( Size ) ->
	{ size, Size }.



% @doc Converts to back-end orientation.
%
% (helper)
%
-spec to_wx_orientation( orientation() ) -> wx_orientation().
to_wx_orientation( vertical ) ->
	?wxVERTICAL;

to_wx_orientation( horizontal ) ->
	?wxHORIZONTAL.



% @doc Converts the specified MyriadGUI device context attributes to wx
% conventions.
%
-spec to_wx_device_context_attributes( [ device_context_attribute() ] ) ->
											[ wx_device_context_attribute() ].
to_wx_device_context_attributes( Attrs ) ->
	to_wx_device_context_attributes( Attrs, _Acc=[] ).


% (helper)
%
% Adding in a reverse form:
to_wx_device_context_attributes( _Attrs=[], Acc ) ->
	lists:reverse( [ 0 | Acc ] );

to_wx_device_context_attributes( _Attrs=[ rgba | T ], Acc ) ->
	to_wx_device_context_attributes( T, [ ?WX_GL_RGBA | Acc ] );

% Not existing:
%to_wx_device_context_attributes( _Attrs=[ bgra | T ], Acc ) ->
%   to_wx_device_context_attributes( T, [ ?WX_GL_BGRA | Acc ] );

to_wx_device_context_attributes( _Attrs=[ double_buffer | T ], Acc ) ->
	to_wx_device_context_attributes( T, [ ?WX_GL_DOUBLEBUFFER | Acc ] );

to_wx_device_context_attributes( _Attrs=[ { min_red_size, S } | T ], Acc ) ->
	to_wx_device_context_attributes( T, [ S, ?WX_GL_MIN_RED | Acc ] );

to_wx_device_context_attributes( _Attrs=[ { min_green_size, S } | T ],
								 Acc ) ->
	to_wx_device_context_attributes( T, [ S, ?WX_GL_MIN_GREEN | Acc ] );

to_wx_device_context_attributes( _Attrs=[ { min_blue_size, S } | T ], Acc ) ->
	to_wx_device_context_attributes( T, [ S, ?WX_GL_MIN_BLUE | Acc ] );

to_wx_device_context_attributes( _Attrs=[ { depth_buffer_size, S } | T ],
								 Acc ) ->
	to_wx_device_context_attributes( T, [ S, ?WX_GL_DEPTH_SIZE | Acc ] );

to_wx_device_context_attributes( _Attrs=[ use_core_profile | T ], Acc ) ->

	% Currently ignored, as leading to a Segmentation fault (and working
	% without):
	%
	%to_wx_device_context_attributes( T, [ ?WX_GL_CORE_PROFILE | Acc ] );
	to_wx_device_context_attributes( T, Acc );

to_wx_device_context_attributes( _Attrs=[ Other | _T ], _Acc ) ->
	throw( { unsupported_device_context_attribute, Other } ).



%
% Section for the conversions from wx to MyriadGUI:
%




%
% Section for wx-related facilities.
%


% @doc Returns the widget corresponding to the specified wx identifier.
%
% (internal use only)
%
-spec wx_id_to_window( wx_id() ) -> window().
wx_id_to_window( Id ) ->
	wxWindow:findWindowById( Id ).


% @doc Returns a textual representation of the specified GUI object wx
% identifier.
%
-spec wx_id_to_string( wx_id() ) -> ustring().
wx_id_to_string( _Id=undefined ) ->
	"no id defined";

wx_id_to_string( _Id=?any_id ) ->
	"'any id' defined";

wx_id_to_string( Id ) ->
	text_utils:format( "ID #~B", [ Id ] ).






% Connection-related section.


% @doc Subscribes the current process to the specified type(s) of events
% regarding the specified object (receiving for that a message).
%
% Only useful for context-less calls; the versions of that function specifying a
% "trap set" parameter shall be preferred, as they are more efficient.
%
-spec connect( event_source(), maybe_list( event_type() ) ) -> void().
connect( EventSource, EventTypeOrTypes ) ->

	% Here no trap set specified, trying to secure it:
	GUIEnvPid = gui:get_environment_server(),

	[ TrapSet, EventTranslationTable ] = environment:get(
		[ trap_set, event_translation_table ], GUIEnvPid ),

	connect( EventSource, EventTypeOrTypes, TrapSet, EventTranslationTable ).



% @doc Subscribes the current process to the specified type(s) of events
% regarding the specified object (receiving for that a message).
%
% Said otherwise: requests the specified widget to send to the current process a
% message-based event when the specified kind of event happens, knowing that by
% default, depending on its type, this event may also be propagated upward in
% the widget hierarchy, through the corresponding event handlers (the trap_event
% option allows not to propagate this event).
%
% Note:
%  - apparently registering more than once a given type has no effect (not N
%  messages of that type sent afterwards)
%  - only useful internally or when bypassing the default main loop
%
-spec connect( event_source(), maybe_list( event_type() ), trap_set(),
			   event_translation_table() ) -> void().
connect( EventSource, EventTypeOrTypes, TrapSet, EventTranslationTable ) ->
	connect( EventSource, EventTypeOrTypes, _Options=[], TrapSet,
			 EventTranslationTable ).



% @doc Subscribes the current process to the specified type(s) of events
% regarding the specified object, with the specified options; this process will
% thus receive a gui_event() message whenever a corresponding event occurs.
%
% The {trap,propagate}_event options (or the corresponding
% {trap,propagate}_event/1 functions) can override these defaults.
%
% Refer to connect/3 for all details.
%
-spec connect( event_source(), maybe_list( event_type() ),
		connect_options(), trap_set(), event_translation_table() ) -> void().
% Was not used apparently:
%connect( #canvas_state{ panel=Panel }, EventTypeOrTypes, Options, TrapSet ) ->
%   connect( Panel, EventTypeOrTypes, Options, TrapSet );

connect( SourceGUIObject, EventTypes, Options, TrapSet, EventTranslationTable )
										when is_list( EventTypes ) ->

	%trace_utils:debug_fmt( "Connecting ~p for event types ~w with options ~p.",
	%                       [ SourceObject, EventTypes, Options ] ),

	[ connect( SourceGUIObject, ET, Options, TrapSet, EventTranslationTable )
								|| ET <- EventTypes ];

connect( SourceGUIObject, EventType, Options, TrapSet,
		 EventTranslationTable ) ->

	% Events to be processed through messages, not callbacks:
	WxEventType = to_wx_event_type( EventType, EventTranslationTable ),

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( " - connecting event source '~ts' to ~w "
			"for ~p (i.e. ~p), with options ~p.",
			[ gui:object_to_string( SourceGUIObject ), self(), EventType,
			  WxEventType, Options ] ) ),

	WxConnOpts = to_wx_connect_options( Options, EventType, TrapSet ),

	wxEvtHandler:connect( SourceGUIObject, WxEventType, WxConnOpts ).



% @doc Converts MyriadGUI connect options into wx ones.
%
% The corresponding event type must be specified in order to apply per-type
% defaults.
%
-spec to_wx_connect_options( [ connect_opt() ], event_type(), trap_set() ) ->
								[ wx_event_handler_option() ].
to_wx_connect_options( Opts, EventType, TrapSet ) ->
	to_wx_connect_options( Opts, EventType, TrapSet,
						   _PropagationSetting=undefined, _Acc=[] ).


% (helper)
-spec to_wx_connect_options( [ connect_opt() ], event_type(), trap_set(),
							 maybe( 'propagate' | 'trap' ), event_type() ) ->
										[ wx_event_handler_option() ].
% End of recursion, propagation explicitly requested by the user:
to_wx_connect_options( _Opts=[], _EventType, _TrapSet,
					   _PropagationSetting=propagate, Acc ) ->
	[ _Propagate={ skip, true } | Acc ];

% End of recursion, propagation explicitly denied by the user:
to_wx_connect_options( _Opts=[], _EventType, _TrapSet,
					   _PropagationSetting=trap, Acc ) ->
	% As skip=False is the default:
	%[ _Trap={ skip, false } | Acc ];
	Acc;

% End of recursion, no user-defined propagation setting, applying thus per-type
% defaults:
%
to_wx_connect_options( _Opts=[], EventType, TrapSet,
					   _PropagationSetting=undefined, Acc ) ->

	case set_utils:member( EventType, TrapSet ) of

		true ->
			% As skip=False is the default:
			%[ _Trap={ skip, false } | Acc ];
			Acc;

		false ->
			[ _Propagate={ skip, true } | Acc ]

	end;

to_wx_connect_options( _Opts=[ P={ id, _I } | T ], EventType, TrapSet,
					   PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet, PropagationSetting,
						   [ P | Acc ] );

to_wx_connect_options( _Opts=[ { last_id, I } | T ], EventType, TrapSet,
					   PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet, PropagationSetting,
						   [ { lastId, I } | Acc ] );

to_wx_connect_options( _Opts=[ trap_event | T ], EventType, TrapSet,
					   _PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet,
						   _ForcedPropagationSetting=trap, Acc );

to_wx_connect_options( _Opts=[ propagate_event | T ], EventType, TrapSet,
					   _PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet,
						   _ForcedPropagationSetting=propagate, Acc );

to_wx_connect_options( _Opts=[ callback | T ], EventType, TrapSet,
					   PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet, PropagationSetting,
						   [ callback | Acc ] );

to_wx_connect_options( _Opts=[ P={ callback, _F } | T ], EventType, TrapSet,
					   PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet, PropagationSetting,
						   [ P | Acc ] );

to_wx_connect_options( _Opts=[ { user_data, D } | T ], EventType, TrapSet,
					   PropagationSetting, Acc ) ->
	to_wx_connect_options( T, EventType, TrapSet, PropagationSetting,
						   [ { userData, D } | Acc ] );

to_wx_connect_options( _Opts=[ Other | _T ], _EventType, _TrapSet,
					   _PropagationSetting, _Acc ) ->
	throw( { invalid_event_subscription_option, Other } ).



% @doc Unsubscribes the current process from the specified object, for all event
% types.
%
% The meaning of the returned boolean is not specified, presumably whether the
% operation went well.
%
-spec disconnect( event_source() ) -> boolean().
disconnect( _SourceObject=#canvas_state{ panel=Panel } ) ->
	disconnect( Panel );

disconnect( SourceObject ) ->
	wxEvtHandler:disconnect( SourceObject ).



% @doc Unsubscribes the current process from the specified object, for the
% specified event type(s).
%
% The meaning of the returned boolean is not specified, presumably whether the
% operation went well.
%
-spec disconnect( event_source(), maybe_list( event_type() ),
				  event_translation_table() ) -> boolean().
disconnect( SourceObject, EventTypes, EventTranslationTable )
								when is_list( EventTypes ) ->
	[ disconnect( SourceObject, ET, EventTranslationTable )
											|| ET <- EventTypes ];

% Single event type now:
disconnect( #canvas_state{ panel=Panel }, EventType, EventTranslationTable ) ->
	disconnect( Panel, EventType, EventTranslationTable );

disconnect( SourceObject, EventType, EventTranslationTable ) ->

	WxEventType = to_wx_event_type( EventType, EventTranslationTable ),

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( " - disconnecting event source '~ts' from ~w "
			"for ~p (i.e. ~p).",
			[ gui:object_to_string( SourceObject ), self(), EventType,
			  WxEventType ] ) ),

	wxEvtHandler:disconnect( SourceObject, WxEventType ).
