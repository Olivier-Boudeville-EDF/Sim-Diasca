% Copyright (C) 2010-2021 Olivier Boudeville
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



% Gathers all elements relative to the wx backend (itself based on WxWidgets).
-module(gui_wx_backend).



% Usually a class of WxWidgets is represented as a module in Erlang.
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
% GUI objects are created with new/*, and deleted with destroy/1.
%
% For information regarding events, refer to gui_event.erl.



% Understanding the wx base widgets hierarchy:
%
% (offsets meaning "inheriting from", corresponding local types specified
% between brackets):
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


% In a more detailed view, with the corresponding MyriadGUI types being
% specified between brackets:
%
% - wxObject [gui_object]: root class of all wxWidgets classes
%
%   - wxEvtHandler [event_handler]: a class that can handle events from the
%   windowing system
%
%      - wxWindow [window]: the base class for all windows and represents any
%      visible object on screen For colors, refer to the color module
%
%        - wxControl: base class for a control or "widget"; generally a small
%        window which processes user input and/or displays one or more item of
%        data
%
%          - wxButton: a control that contains a text string, and is one of the
%          most common elements of a GUI; may be placed on any window, notably
%          dialog boxes or panels
%
%        - wxPanel [panel]: a window on which controls are placed; usually
%        placed within a frame
%
%        - wxTopLevelWindow: abstract base class for wxDialog and wxFrame
%
%           - wxDialog: a window with a title bar and sometimes a system menu,
%           which can be moved around the screen; often used to allow the user
%           to make some choice or to answer a question
%
%           - wxFrame [frame]: a window whose size and position can (usually) be
%           changed by the user. It usually has thick borders and a title bar,
%           and can optionally contain a menu bar, toolbar and status bar. A
%           frame can contain any window that is not a frame or dialog
%
%        - wxStatusBar: narrow window that can be placed along the bottom of a
%        frame
%
%   - wxSizer (also derives from wxClientDataContainer): abstract base class
%   used for laying out subwindows in a window


% Additional widgets
%
% Some types of widgets seem to be lacking to WxWidgets, such as canvases that
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


% Conversions from MyriadGUI to wx:
-export([ to_wx_object_type/1,
		  to_wx_event_type/1, from_wx_event_type/1,
		  to_wx_debug_level/1,
		  window_style_to_bitmask/1,
		  get_window_options/1, get_window_options/2,
		  frame_style_to_bitmask/1,
		  get_panel_options/1,
		  button_style_to_bitmask/1,
		  to_wx_sizer_options/1, to_wx_sizer_options/2,
		  sizer_flag_to_bitmask/1,
		  to_wx_id/1, to_wx_parent/1, to_wx_position/1, to_wx_size/1,
		  to_wx_orientation/1, wx_id_to_window/1, wx_id_to_string/1,
		  connect/2, connect/3, disconnect/1 ]).


% Conversions from wx to MyriadGUI:
-export([ from_wx_object_type/1 ]).


% For wx defines:
-include("gui_internal_defines.hrl").


% Type section.


% Default position, chosen by either the windowing system or wxWidgets,
% depending on platform:
%
-define( wx_default_position, { -1, -1 } ).

-type wx_position() :: { 'pos', linear_2D:point() }.


% Default size, chosen by either the windowing system or wxWidgets,
% depending on platform:
%
-define( wx_default_size, { -1, -1 } ).

-type wx_size() :: { 'size', linear_2D:dimensions() }.


-type wx_orientation() :: ?wxVERTICAL | ?wxHORIZONTAL.


% The identifier (ID) of a wx element is an integer (positive or not).
%
% It allows to specify a 'void' (null) ID of a GUI element.
%
% Sometimes the ID may be directly provided by the user or have a predefined
% value, such as wxID_OPEN; see
% http://docs.wxwidgets.org/2.8.12/wx_stockitems.html#stockitems for a list
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
% (note: this type is defined and exported, yet reported unknown by Dialyzer)
%
-type wx_id() :: maybe( integer() ).


% See any_id, no_parent, etc. as defined in gui.hrl.



% Native wx object types.
%
% No enumeration like 'wxWindow' | 'wxFrame' | ... found in wx, so:
%
-type wx_native_object_type() :: atom().


% Shorthands:

-type bit_mask() :: basic_utils:bit_mask().

-type ustring() :: text_utils:ustring().

-type event_type() :: gui_event:event_type().
-type event_source() :: gui_event:event_source().

-type wx_object_type() :: gui:wx_object_type().
-type myriad_object_type() :: gui:myriad_object_type().


% To avoid unused warnings:
-export_type([ wx_id/0, wx_native_object_type/0 ]).


% For canvas_state():
-include("gui_canvas.hrl").



% Implementation section.
%
% Generally conversions are listed by themes, and in each theme first from
% MyriadGUI to wx (to_wx_*), then from wx to MyriadGUI (from_wx_*), to
% facilitate the consistency of the two-way definitions.



% Object type section.


% Converts a MyriadGUI type of object into a wx one.
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

to_wx_object_type( Other ) ->
	throw( { unsupported_object_type, Other } ).




% Converts a wx type of object into a MyriadGUI one.
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

from_wx_object_type( Other ) ->
	throw( { unsupported_wx_object_type, Other } ).






% Event type section.


% Converts a MyriadGUI type of event into a wx one.
-spec to_wx_event_type( event_type() ) -> gui_event:wx_event_type().
to_wx_event_type( onWindowClosed ) ->
	close_window;

to_wx_event_type( onButtonClicked ) ->
	command_button_clicked;

to_wx_event_type( onRepaintNeeded ) ->
	paint;

to_wx_event_type( onResized ) ->
	size;

to_wx_event_type( Other ) ->
	throw( { unsupported_wx_event_type, Other } ).




% Converts a wx type of event into a MyriadGUI one.
-spec from_wx_event_type( gui_event:wx_event_type() ) -> event_type().
from_wx_event_type( close_window ) ->
	onWindowClosed;

from_wx_event_type( command_button_clicked ) ->
	onButtonClicked;

from_wx_event_type( paint ) ->
	onRepaintNeeded;

from_wx_event_type( size ) ->
	onResized;

from_wx_event_type( Other ) ->
	throw( { unsupported_wx_event_type, Other } ).





%
% Section for the conversions from MyriadGUI to wx.
%


% Debug section.


% Converts the debug level from MyriadGUI to the one of wx.
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


% Converts specified MyriadGUI window style into the appropriate wx-specific bit
% mask.
%
% (helper)
%
-spec window_style_to_bitmask( gui:window_style() ) -> bit_mask().
window_style_to_bitmask( StyleList ) when is_list( StyleList ) ->

	lists:foldl( fun( S, Acc ) -> window_style_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

window_style_to_bitmask( _Style=default ) ->
	?wxBORDER_SIMPLE;

window_style_to_bitmask( _Style=simple_border ) ->
	?wxBORDER_SIMPLE;

window_style_to_bitmask( _Style=double_border ) ->
	?wxBORDER_DOUBLE;

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
	?wxFULL_REPAINT_ON_RESIZE.



% Converts specified MyriadGUI window options into the appropriate wx-specific
% options.
%
% (exported helper)
%
get_window_options( Options ) ->
	get_window_options( Options, _Acc=[] ).


get_window_options( _Options=[], Acc ) ->
	Acc;

get_window_options( _Options=[ { style, Style } | T ], Acc ) ->
	get_window_options( T,
					[ { style, window_style_to_bitmask( Style ) } | Acc ] );

get_window_options( _Options=[ H | T ], Acc ) ->
	get_window_options( T, [ H | Acc ] ).



% Frames section.


% Converts specified MyriadGUI frame style into the appropriate wx-specific bit
% mask.
%
% (helper)
%
-spec frame_style_to_bitmask( gui:frame_style() ) -> bit_mask().
frame_style_to_bitmask( StyleList ) when is_list( StyleList ) ->

	lists:foldl( fun( S, Acc ) -> frame_style_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

frame_style_to_bitmask( _Style=default ) ->
	?wxDEFAULT_FRAME_STYLE;

frame_style_to_bitmask( _Style=caption ) ->
	?wxCAPTION;

frame_style_to_bitmask( _Style=minimize ) ->
	?wxMINIMIZE;

frame_style_to_bitmask( _Style=minimize_box ) ->
	?wxMINIMIZE_BOX;

frame_style_to_bitmask( _Style=maximize ) ->
	?wxMAXIMIZE;

frame_style_to_bitmask( _Style=maximize_box ) ->
	?wxMAXIMIZE_BOX;

frame_style_to_bitmask( _Style=close_box ) ->
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
	?wxFRAME_NO_TASKBAR.



% Panels section.


% Converts specified MyriadGUI panel options into the appropriate wx-specific
% options.
%
% (exported helper)
%
get_panel_options( Options ) ->
	get_window_options( Options ).




% Buttons section.


% Converts specified MyriadGUI button style into the appropriate wx-specific bit
% mask.
%
% (helper)
%
-spec button_style_to_bitmask( gui:button_style() ) -> bit_mask().
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



% Converts specified sizer options into wx-specific ones.
%
% (helper)
%
-spec to_wx_sizer_options( gui:sizer_options() ) -> gui:sizer_options().
to_wx_sizer_options( Options ) ->
	to_wx_sizer_options( Options, _Acc=[] ).

to_wx_sizer_options( _Options=[], Acc ) ->
	Acc;

to_wx_sizer_options( _Options=[ { flag, Flag } | T ], Acc ) ->
	to_wx_sizer_options( T, [ { flag, sizer_flag_to_bitmask( Flag ) } | Acc ] );

to_wx_sizer_options(_Options=[ H | T ], Acc ) ->
	to_wx_sizer_options( T, [ H | Acc ] ).



% Converts specified MyriadGUI sizer flag into the appropriate wx-specific bit
% mask.
%
% (helper)
%
-spec sizer_flag_to_bitmask( gui:sizer_flag() ) -> bit_mask().
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



% Converts specified MyriadGUI identifier in a wx-specific widget identifier.
%
% (helper)
%
-spec to_wx_id( maybe( gui:myriad_instance_pid() ) ) -> wx_id().
to_wx_id( undefined ) ->
	?any_id;

to_wx_id( Other ) ->
	Other.


% Converts specified MyriadGUI identifier in a wx-specific parent widget
% identifier.
%
% (helper)
%
-spec to_wx_parent( maybe( gui:myriad_instance_pid() ) ) -> wx_id().
to_wx_parent( undefined ) ->
	?no_parent;

to_wx_parent( Other ) ->
	Other.



% Converts specified MyriadGUI position in a wx-specific position (with
% defaults).
%
% (helper)
%
-spec to_wx_position( gui:position() ) -> wx_position().
to_wx_position( _Position=auto ) ->
	{ pos, ?wx_default_position };

to_wx_position( Position ) ->
	{ pos, Position }.




% Converts specified MyriadGUI size in a wx-specific size (with defaults).
%
% (helper)
%
-spec to_wx_size( gui:size() ) -> wx_size().
to_wx_size( _Size=auto ) ->
	{ size, ?wx_default_size };

%to_wx_size( Size={ _X, _Y } ) ->
to_wx_size( Size ) ->
	{ size, Size }.





% Converts to back-end orientation.
%
% (helper)
%
-spec to_wx_orientation( gui:orientation() ) -> wx_orientation().
to_wx_orientation( vertical ) ->
	?wxVERTICAL;

to_wx_orientation( horizontal ) ->
	?wxHORIZONTAL.



%
% Section for the conversions from wx to MyriadGUI:
%




%
% Section for wx-related facilities.
%


% Returns the widget corresponding to the specified wx identifier.
%
% (internal use only)
%
-spec wx_id_to_window( wx_id() ) -> gui:window().
wx_id_to_window( Id ) ->
	wxWindow:findWindowById( Id ).


% Returns a textual representation of the specified GUI object wx identifier.
-spec wx_id_to_string( wx_id() ) -> ustring().
wx_id_to_string( _Id=undefined ) ->
	"no id defined";

wx_id_to_string( _Id=?any_id ) ->
	"'any id' defined";

wx_id_to_string( Id ) ->
	text_utils:format( "ID #~B", [ Id ] ).




% Subscribes the current process to the specified type of event of the specified
% object (receiving for that a message).
%
% Said otherwise: requests the specified widget to send a message-based event
% when the specified kind of event happens, overriding its default behaviour.
%
% Note: only useful internally or when bypasssing the default main loop.
%
-spec connect( event_source(), event_type() ) -> void().
connect( #canvas_state{ panel=Panel }, EventType ) ->
	connect( Panel, EventType );

connect( EventSource, EventType ) ->
	connect( EventSource, EventType, _Options=[] ).



% Subscribes the current process to the specified type of event of the specified
% object (receiving for that a message).
%
% Said otherwise: requests the specified widget to send a message-based event
% when the specified kind of event happens, overriding its default behaviour
% based on specified options.
%
% Note: only useful internally or when bypasssing the default main loop.
%
-spec connect( event_source(), event_type(), gui:connect_options() ) -> void().
connect( #canvas_state{ panel=Panel }, EventType, Options ) ->
	connect( Panel, EventType, Options );

connect( SourceObject, EventType, Options ) ->

	% Events to be processed through messages, not callbacks:
	WxEventType = to_wx_event_type( EventType ),

	trace_utils:debug_fmt( "Connecting event source '~ts' to ~w for ~p.",
		[ gui:object_to_string( SourceObject ), self(), EventType ] ),

	wxEvtHandler:connect( SourceObject, WxEventType, Options ).



% Unsubscribes the event source, for all event types.
-spec disconnect( event_source() ) -> boolean().
disconnect( #canvas_state{ panel=Panel } ) ->
	disconnect( Panel );

disconnect( EventSource ) ->
		wxEvtHandler:disconnect( EventSource ).
