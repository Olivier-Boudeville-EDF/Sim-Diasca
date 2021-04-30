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
% Creation date: Monday, February 15, 2010.



% Gathering of various facilities for Graphical User Interfaces.
%
% We name this library MyriadGUI (shortened here, whenever it is not ambiguous,
% in 'gui').
%
% The purpose of MyriadGUI is to wrap, complement and improve what we consider
% the best set of gui backends available (previously: gs alone; now: wx+esdl,
% with OpenGL), for classical applications and multimedia ones (ex: games).
%
% See gui_test.erl for the corresponding test.
%
-module(gui).


% For the canvas_state record:
-include("gui_canvas.hrl").


-include("polygon.hrl").




% Rendering of GUI elements.
%
% Formerly based on the gs backend, now on the wx one.
%
% Providing improved and enriched APIs for all kinds of GUI.
%
% Relying optionally on:
%
% - OpenGL, for efficient 3D rendering
%
% - esdl, for adequate lower-level primitives (ex: management of input devices)



% Implementation notes:

% This module used to rely on the gs module, whose API was quite simple and
% elegant.
%
% As 'gs' was replaced (quite quickly, with short notice unfortunately) by 'wx'
% (an Erlang binding to wxWidgets), now we rely on the latter and wrap it
% accordingly, to be future-proof.
%
% The general convention is still to put as first argument the object on which
% the operation is to be applied (ex: the window).
%
% See also:
% - the gui_wx_backend module for our use of wx as a backend
% - the gui_canvas module for all canvas-related operations.



% Event loops.
%
% There is generally two event loops involved here:
%
% - a mandatory, generic, MyriadGUI-internal one (defined in the gui_event
% module), looping over process_event_messages/1 that relies on an (opaque)
% gui_event:loop_state(), running on a dedicated process spawned by the
% start/n functions
%
% - a user-defined, application-specific one, fed by the former internal loop
% (ex: with onWindowClosed messages), possibly using any client-side state of
% interest (whose definition is fully free)




% Environments.


% Environment kept for MyriadGUI, usually in the process dictionary (like wx
% does):
%
-record( gui_env, {

	% Reference to the current top-level wx server:
	wx_server :: wx:wx_object(),

	% PID of the main loop:
	loop_pid :: pid() }).


% Stores the current, user-side (client) state (merely references) of the GUI.
%
% Like wx:wx_env(); kept in the process dictionary for easier sharing that if
% using a naming service or having to keep around a bound variable.
%
-type gui_env() :: #gui_env{}.


% Current backend is wx (WxWidgets).
%
% (useful to avoid including the header of wx in our own public ones)
%
-type backend_event() :: gui_event:wx_event().


% To be used directly from the user code:
-type canvas() :: gui_canvas:canvas().


% Basic GUI operations.
-export([ is_available/0, start/0, start/1, set_debug_level/1, stop/0 ]).



% Event-related operations.
-export([ subscribe_to_events/1, propagate_event/1 ]).



% Stringification section.
%
% (mostly internal purpose)
%
-export([ object_to_string/1, context_to_string/1 ]).



% Widget-related section.


% General-purpose:
-export([ set_tooltip/2 ]).




% Windows:
-export([ create_window/0, create_window/1, create_window/2, create_window/5,
		  set_sizer/2, show/1, hide/1, get_size/1, destruct_window/1 ]).


% Frames:
%
% Note that a frame is a top_level_window(), a window() and an event_handler(),
% and thus can use their methods.
%
-export([ create_frame/0, create_frame/1, create_frame/2, create_frame/3,
		  create_frame/4, create_frame/6 ]).


% Panels:
-export([ create_panel/0, create_panel/1, create_panel/2, create_panel/4,
		  create_panel/5, create_panel/6 ]).


% Buttons:
-export([ create_button/2, create_button/6, create_buttons/2 ]).


% Sizers:
-export([ create_sizer/1, create_sizer_with_box/2,
		  create_sizer_with_labelled_box/3, add_to_sizer/2, add_to_sizer/3,
		  clear_sizer/1, clear_sizer/2 ]).


% Status bars:
-export([ create_status_bar/1, push_status_text/2 ]).


% Canvas support (forwarded to gui_canvas).
-export([ create_canvas/1, set_draw_color/2, set_fill_color/2,
		  set_background_color/2, get_rgb/2, set_rgb/2,
		  draw_line/3, draw_line/4, draw_lines/2, draw_lines/3,
		  draw_segment/4, draw_polygon/2,
		  draw_label/3,
		  draw_cross/2, draw_cross/3, draw_cross/4, draw_labelled_cross/4,
		  draw_labelled_cross/5, draw_circle/3, draw_circle/4,
		  draw_numbered_points/2,
		  load_image/2, load_image/3, blit/1, clear/1 ]).



% For related, public defines:
-include("gui.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").


% For myriad_spawn*:
-include("spawn_utils.hrl").



% Type declarations:

-type length() :: linear:distance().
-type coordinate() :: linear:integer_coordinate().


% linear_2D:point() would allow for floating-point coordinates:
-type point() :: linear_2D:integer_point().

-type position() :: point() | 'auto'.


% Size, typically of a widget:
-type size() :: linear_2D:dimensions() | 'auto'.


% A vertical orientation means piling elements top to bottom for example, while
% an horizontal means left to right, for example.
%
-type orientation() :: 'vertical' | 'horizontal'.



% Widget types.


% Internal, overall types for all GUI objects:
-type object_type() :: wx_object_type() | myriad_object_type().



% MyriadGUI-translated version of a native wx type, i.e. of the
% wx_native_object_type().
%
% (ex: 'window', instead of 'wxWindow'):
%
-type wx_object_type() :: 'object'
						| 'event_handler'
						| 'window'
						| 'control'
						| 'button'
						| 'panel'
						| 'status_bar'
						| 'top_level_window'
						| 'dialog'
						| 'frame'
						| 'sizer'
						| 'server'.


% The additional widget types introduced by Myriad
-type myriad_object_type() :: 'canvas'.


% Records the actual state of a MyriadGUI object:
-type myriad_object_state() :: gui_canvas:canvas_state().
						   % | all other *_state() that may be introduced



% The construction parameters of a MyriadGUI object:
-type construction_parameters() :: [ term() ].


% Myriad-specific instance identifier, a PID:
-type myriad_instance_pid() :: pid().


% wx-specific object identifier:
%
% (defined so that the gui_event_context (public) record has no trace of the
% backend)
%
% May not be defined if the actual event comes from MyriadGUI itself (and thus
% not wx).
%
-type id() :: maybe( wx_id() ).


% Reference to a GUI object (often designated as "widget" here), somewhat akin
% to a PID.
%
% (ex: {wx_ref,35,wxFrame,[]} or {myriad_object_ref,canvas,12})
%
-type gui_object() :: wx:wx_object() | myriad_object_ref().


% Alias to designate more clearly the wx server:
-type wx_server() :: gui_object().


% Defining the actual widget types corresponding to wx_object_type():


-type window() :: maybe( wxWindow:wxWindow() | gui_canvas:canvas() ).

-type frame() :: wxFrame:wxFrame().

-type panel() :: wxPanel:wxPanel().

-type button() :: wxButton:wxButton().

-type sizer() :: wxSizer:wxSizer().


% Elements that can be included in a sizer:
-type sizer_child() :: window() | sizer().

-type sizer_item() :: wxSizerItem:wxSizerItem().

-type status_bar() :: wxStatusBar:wxStatusBar().

-type bitmap() :: wxBitmap:wxBitmap().

-type back_buffer() :: wxMemoryDC:wxMemoryDC().



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type text() :: ustring().

-type wx_id() :: gui_wx_backend:wx_id().

-type integer_distance() :: linear:integer_distance().

-type color() :: gui_color:color().


% Aliases:

-type title() :: text().
-type label() :: text().


% User data, as specified in the connect call:
-type user_data() :: any().




% Options for windows, see:
% http://docs.wxwidgets.org/stable/wx_wxwindow.html and
% http://docs.wxwidgets.org/stable/wx_windowstyles.html#windowstyles
%
-type window_style_opt() :: 'default'
						  | 'simple_border'
						  | 'double_border'
						  | 'sunken_border'
						  | 'raised_border'
						  | 'static_border'
						  | 'theme_border'
						  | 'no_border'
						  | 'transparent'
						  | 'tab_traversable'
						  | 'grab_all_keys'
						  | 'with_vertical_scrollbar'
						  | 'with_horizontal_scrollbar'
						  | 'never_hide_scrollbars'
						  | 'clip_children'
						  | 'grab_all_keys'
						  | 'full_repaint_on_resize'.


-type window_style() :: window_style_opt() | [ window_style_opt() ].

-type window_option() :: { pos, point() }
					   | { size, size() }
					   | { style, [ window_style_opt() ] }.

% Unused: -type window_options() :: [ window_option() ].



% Options for frames, see:
% http://docs.wxwidgets.org/stable/wx_wxframe.html#wxframewxframe
%
-type frame_style_opt() :: 'default'
						 | 'caption'
						 | 'minimize'
						 | 'minimize_box'
						 | 'maximize'
						 | 'maximize_box'
						 | 'close_box'
						 | 'stay_on_top'
						 | 'system_menu'
						 | 'resize_border'
						 | 'tool_window'
						 | 'no_taskbar'.


-type frame_style() :: frame_style_opt() | [ frame_style_opt() ].


% Options for panels, see:
% http://docs.wxwidgets.org/stable/wx_wxpanel.html#wxpanelwxpanel
%
-type panel_option() :: window_option().

-type panel_options() :: [ panel_option() ].



% Options for button style, see:
% http://docs.wxwidgets.org/stable/wx_wxbutton.html#wxbuttonwxbutton
%
-type button_style_opt() :: 'default'
						  | 'left_justified'
						  | 'right_justified'
						  | 'top_justified'
						  | 'bottom_justified'
						  | 'exact_fit'
						  | 'flat'.


-type button_style() :: button_style_opt() | [ button_style_opt() ].



% Options for sizers, see:
% http://docs.wxwidgets.org/stable/wx_wxsizer.html
%
-type sizer_flag_opt() :: 'default'
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


-type sizer_flag() :: sizer_flag_opt() | [ sizer_flag_opt() ].

-type sizer_option() :: { 'proportion', integer() }
					  | { 'flag', sizer_flag() }
					  | { 'border', integer() }
					  | { 'userData', gui_object() }.


-type sizer_options() :: [ sizer_option() ].


% Options for event management connections:
-type connect_opt() ::   { 'id', integer() }
					   | { lastId, integer() }
					   | { skip, boolean() }
					   |   callback
					   | { callback, function() }
					   | { userData, term() }.


-type connect_options() :: connect_opt() | [ connect_opt() ].


% Mapped internally to 'none', 'verbose', 'trace', etc.; see
% convert_debug_level/1:
%
-type debug_level_opt() :: 'none' | 'calls' | 'life_cycle'.


-type debug_level() :: debug_level_opt() | [ debug_level_opt() ].

-type error_message() :: term().



-export_type([ length/0, coordinate/0, point/0, position/0, size/0,
			   orientation/0, object_type/0, wx_object_type/0,
			   myriad_object_type/0, myriad_instance_pid/0,
			   title/0, label/0, user_data/0,
			   id/0, gui_object/0, wx_server/0,
			   window/0, frame/0, panel/0, button/0,
			   sizer/0, sizer_child/0, sizer_item/0, status_bar/0,
			   bitmap/0, back_buffer/0, canvas/0,
			   construction_parameters/0, backend_event/0, connect_options/0,
			   window_style/0, frame_style/0, button_style/0,
			   sizer_flag_opt/0, sizer_flag/0, sizer_option/0, sizer_options/0,
			   connect_opt/0, debug_level_opt/0, debug_level/0,
			   error_message/0 ]).


% To avoid unused warnings:
-export_type([ myriad_object_state/0 ]).


% Function shorthands:
-import( gui_wx_backend, [ to_wx_parent/1, to_wx_id/1, to_wx_position/1,
						   to_wx_size/1, to_wx_orientation/1,
						   frame_style_to_bitmask/1, get_panel_options/1 ]).


% GUI-specific defines:


% Key of the MyriadGUI environment, in the process dictionary:
-define( gui_env_process_key, myriad_gui_env ).



% Section for basic GUI overall operations.


% Tells whether this user-interface backend is available.
-spec is_available() -> boolean().
is_available() ->
	% As simple as:
	system_utils:has_graphical_output().





% Starts the MyriadGUI subsystem.
-spec start() -> void().
start() ->

	% Initialises the wx backend (no option relevant here):
	WxServer = wx:new(),

	% The wx environment will be exported to the internal main loop process, so
	% that both the user code (i.e. the current process) and that loop can make
	% use of wx:
	%
	WxEnv = wx:get_env(),

	% The event table must be initialised in the spawned process, so that
	% connect/n can use the right actual, first-level subscriber PID, which is
	% the internal main loop in charge of the message routing and conversion.

	LoopPid = ?myriad_spawn_link( gui_event, start_main_event_loop,
								  [ WxServer, WxEnv ] ),

	trace_utils:info_fmt( "Main loop running on ~w (created from ~w).",
						  [ LoopPid, self() ] ),

	GUIEnv = #gui_env{ wx_server=WxServer, loop_pid=LoopPid },

	% Stored in the process dictionary of the user process, like for wx:
	put( ?gui_env_process_key, GUIEnv ).



% Starts the GUI subsystem, with specified debug level.
-spec start( debug_level() ) -> void().
start( DebugLevel ) ->
	start(),
	set_debug_level( DebugLevel ).



% Sets the debug level(s) of the GUI.
-spec set_debug_level( debug_level() ) -> void().
set_debug_level( DebugLevels ) when is_list( DebugLevels ) ->
	wx:debug( [ gui_wx_backend:to_wx_debug_level( L ) || L <- DebugLevels ] );

set_debug_level( DebugLevel ) ->
	set_debug_level( [ DebugLevel ] ).



% Subscribes the current, calling process to the specified kind of events, like
% { onWindowClosed, MyFrame }.
%
% This process will then receive MyriadGUI callback messages whenever events
% that match happen, such as: { onWindowClosed, [ MyFrame, Context ] }.
%
% By default the corresponding event will not be transmitted upward in the
% widget hierarchy (as this event will be expected to be processed for good by
% the subscriber(s) it has been dispatched to), unless the propagate_event/1
% function is called from one of them.
%
-spec subscribe_to_events( gui_event:event_subscription_spec() ) -> void().
subscribe_to_events( SubscribedEvents ) when is_list( SubscribedEvents ) ->

	trace_utils:info_fmt( "Subscribing to following events: ~p.",
						  [ SubscribedEvents ] ),

	GUIEnv = get_gui_env(),

	LoopPid = GUIEnv#gui_env.loop_pid,

	% Oneway:
	LoopPid ! { subscribeToEvents, [ SubscribedEvents, self() ] };


subscribe_to_events( SubscribedEvent ) when is_tuple( SubscribedEvent ) ->
	subscribe_to_events( [ SubscribedEvent ] ).



% Propagates the event designated by the specified context upward in the widget
% hierarchy (instead of the default, which is considering that it has been
% processed once for all, and thus shall not be propagated further).
%
% Events are handled in order, from bottom to top in the widgets hierarchy, by
% the last subscribed handler first. Most of the events have default event
% handler(s) set.
%
% As a result, calling this function results in having the corresponding event
% handled by the other handler(s) afterwards.
%
% In general, it is recommended to propagate all non-command events to allow the
% default handling to take place. The command events are, however, normally not
% propagated as usually a single command such as a button click or menu item
% selection must only be processed by one handler.
%
% Note: to be called from an event handler, i.e. at least from a process which
% set the wx environment.
%
-spec propagate_event( gui_event_context() ) -> void().
propagate_event( EventContext ) ->
	gui_event:propagate_event( EventContext ).



% Stops the GUI subsystem.
-spec stop() -> void().
stop() ->

	% No wx_server needed:
	ok = wx:destroy(),

	% Remove from process dictionary:
	put( ?gui_env_process_key, _Value=undefined ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Widget section.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Common section.



% Attaches a tooltip to specified widget.
-spec set_tooltip( window(), label() ) -> void().
set_tooltip( _Canvas={ myriad_object_ref, canvas, CanvasId }, Label ) ->
	get_main_loop_pid() ! { setTooltip, [ CanvasId, Label ] };

set_tooltip( Window, Label ) ->

	%trace_utils:debug_fmt( "Setting tooltip '~ts' to ~ts.",
	%					   [ Label, object_to_string( Window ) ] ),

	% For an unknown reason, works on panels but never on buttons:
	wxWindow:setToolTip( Window, Label ).



% Window section.
%
% Base class for all windows and represents any visible object on screen. All
% controls, top level windows and so on are windows. Sizers and device contexts
% are not, however, as they do not appear on screen themselves.


% Creates a new window.
-spec create_window() -> window().
create_window() ->
	wxWindow:new().


% (internal use only)
-spec create_window( wx_id(), window() ) -> window().
create_window( Id, Parent ) ->

	ActualId = to_wx_id( Id ),
	ActualParent = to_wx_parent( Parent ),

	wxWindow:new( ActualParent, ActualId ).


-spec create_window( size() ) -> window().
create_window( Size ) ->

	ActualId = to_wx_id( undefined ),
	ActualParent = to_wx_parent( undefined ),

	Options =  [ to_wx_size( Size ) ],

	wxWindow:new( ActualParent, ActualId, Options ).



% (internal use only)
-spec create_window( position(), size(), window_style(), wx_id(), window() ) ->
							window().
create_window( Position, Size, Style, Id, Parent ) ->

	Options = [ to_wx_position( Position ), to_wx_size( Size ),
				 { style, gui_wx_backend:window_style_to_bitmask( Style ) } ],

	ActualId = to_wx_id( Id ),
	ActualParent = to_wx_parent( Parent ),

	wxWindow:new( ActualParent, ActualId, Options ).




% Canvas section.


% Creates a canvas, attached to specified parent window.
-spec create_canvas( window() ) -> canvas().
create_canvas( Parent ) ->

	% Returns the corresponding myriad_object_ref:
	execute_instance_creation( canvas, [ Parent ] ).



% Sets the color to be used for the drawing of the outline of shapes.
-spec set_draw_color( canvas(), color() ) -> void().
set_draw_color( _Canvas={ myriad_object_ref, canvas, CanvasId }, Color ) ->
	get_main_loop_pid() ! { setCanvasDrawColor, [ CanvasId, Color ] }.



% Sets the color to be using for filling surfaces.
-spec set_fill_color( canvas(), color() ) -> void().
set_fill_color( _Canvas={ myriad_object_ref, canvas, CanvasId }, Color ) ->
	get_main_loop_pid() ! { setCanvasFillColor, [ CanvasId, Color ] }.



% Sets the background color of the specified window.
-spec set_background_color( window(), color() ) -> void().
set_background_color( _Canvas={ myriad_object_ref, canvas, CanvasId },
					  Color ) ->

	%trace_utils:debug_fmt( "Setting background color of canvas ~w to ~p.",
	%					   [ Canvas, Color ] ),

	get_main_loop_pid() ! { setCanvasBackgroundColor, [ CanvasId, Color ] };

set_background_color( Window, Color ) ->

	ActualColor = gui_color:get_color( Color ),

	wxWindow:setBackgroundColour( Window, ActualColor ).



% Returns the RGB value of the pixel at specified position.
-spec get_rgb( canvas(), point() ) ->
						gui_color:color_by_decimal_with_alpha().
get_rgb( _Canvas={ myriad_object_ref, canvas, CanvasId }, Point ) ->

	get_main_loop_pid() ! { getCanvasRGB, [ CanvasId, Point ], self() },

	receive

		{ notifyCanvasRGB, Color } ->
			Color

	end.



% Sets the pixel at specified position to the current RGB point value.
-spec set_rgb( canvas(), point() ) -> void().
set_rgb( _Canvas={ myriad_object_ref, canvas, CanvasId }, Point ) ->
	get_main_loop_pid() ! { setCanvasRGB, [ CanvasId, Point ] }.



% Draws a line between the specified two points in the back-buffer of the
% specified canvas, using current draw color.
%
-spec draw_line( canvas(), point(), point() ) -> void().
draw_line( _Canvas={ myriad_object_ref, canvas, CanvasId }, P1, P2 ) ->
	get_main_loop_pid() ! { drawCanvasLine, [ CanvasId, P1, P2 ] }.



% Draws a line between the specified two points in specified canvas, with
% specified color.
%
-spec draw_line( canvas(), point(), point(),
				 color() ) -> void().
draw_line( _Canvas={ myriad_object_ref, canvas, CanvasId }, P1, P2, Color ) ->
	get_main_loop_pid() ! { drawCanvasLine, [ CanvasId, P1, P2, Color ] }.



% Draws lines between the specified list of points, in specified canvas, using
% current draw color.
%
-spec draw_lines( canvas(), [ point() ] ) -> void().
draw_lines( _Canvas={ myriad_object_ref, canvas, CanvasId }, Points ) ->
	get_main_loop_pid() ! { drawCanvasLines, [ CanvasId, Points ] }.



% Draws lines between specified list of points in specified canvas, with
% specified color.
%
-spec draw_lines( canvas(), [ point() ], color() ) ->
						void().
draw_lines( _Canvas={ myriad_object_ref, canvas, CanvasId }, Points, Color ) ->
	get_main_loop_pid() ! { drawCanvasLines, [ CanvasId, Points, Color ] }.



% Draws a segment of the line L between the two specified ordinates.
%
% Line L must not have for equation Y=constant (i.e. its A parameter must not be
% null).
%
-spec draw_segment( canvas(), linear_2D:line(), coordinate(), coordinate() ) ->
							void().
draw_segment( _Canvas={ myriad_object_ref, canvas, CanvasId }, L, Y1, Y2 ) ->
	get_main_loop_pid() ! { drawCanvasSegment, [ CanvasId, L, Y1, Y2 ] }.



% Draws the specified polygon, closing the lines and filling them.
-spec draw_polygon( canvas(), [ point() ] ) -> void().
draw_polygon( _Canvas={ myriad_object_ref, canvas, CanvasId }, Points ) ->
	get_main_loop_pid() ! { drawCanvasPolygon, [ CanvasId, Points ] }.



% Draws the specified label (a plain string) at specified position, on specified
% canvas, using the current draw color.
%
-spec draw_label( canvas(), point(), label() ) -> void().
draw_label( _Canvas={ myriad_object_ref, canvas, CanvasId }, Point, Label ) ->
	get_main_loop_pid() ! { drawCanvasLabel, [ CanvasId, Point, Label  ] }.



% Draws an upright cross at specified location (2D point), with default edge
% length.
%
-spec draw_cross( canvas(), point() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, canvas, CanvasId }, Location ) ->
	get_main_loop_pid() ! { drawCanvasCross, [ CanvasId, Location ] }.



% Draws an upright cross at specified location (2D point), with specified edge
% length.
%
-spec draw_cross( canvas(), point(), integer_distance() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, canvas, CanvasId }, Location,
			EdgeLength ) ->
	get_main_loop_pid() ! { drawCanvasCross,
							[ CanvasId, Location, EdgeLength ] }.



% Draws an upright cross at specified location (2D point), with specified edge
% length and color.
%
-spec draw_cross( canvas(), point(), integer_distance(), color() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, canvas, CanvasId }, Location,
			EdgeLength, Color ) ->
	get_main_loop_pid() ! { drawCanvasCross,
							[ CanvasId, Location, EdgeLength, Color ] }.



% Draws an upright cross at specified location (2D point), with specified edge
% length and companion label.
%
-spec draw_labelled_cross( canvas(), point(), integer_distance(), label()  ) ->
								 void().
draw_labelled_cross( _Canvas={ myriad_object_ref, canvas, CanvasId }, Location,
					 EdgeLength, LabelText ) ->
	get_main_loop_pid() ! { drawCanvasLabelledCross,
							[ CanvasId, Location, EdgeLength, LabelText  ] }.



% Draws an upright cross at specified location (2D point), with specified edge
% length and companion label, and with specified color.
%
-spec draw_labelled_cross( canvas(), point(), integer_distance(), color(),
						   label() ) -> void().
draw_labelled_cross( _Canvas={ myriad_object_ref, canvas, CanvasId }, Location,
					 EdgeLength, Color, LabelText ) ->
	get_main_loop_pid() ! { drawCanvasLabelledCross,
						[ CanvasId, Location, EdgeLength, Color, LabelText  ] }.



% Renders specified circle (actually, depending on the fill color, it may be a
% disc) in specified canvas.
%
-spec draw_circle( canvas(), point(), integer_distance() ) -> void().
draw_circle( _Canvas={ myriad_object_ref, canvas, CanvasId }, Center,
			 Radius ) ->
	get_main_loop_pid() ! { drawCanvasCircle, [ CanvasId, Center, Radius ] }.



% Renders specified circle (actually, depending on the fill color, it may be a
% disc) in specified canvas.
%
-spec draw_circle( canvas(), point(), integer_distance(), color() ) -> void().
draw_circle( _Canvas={ myriad_object_ref, canvas, CanvasId }, Center, Radius,
			 Color ) ->
	get_main_loop_pid() ! { drawCanvasCircle,
							[ CanvasId, Center, Radius, Color ] }.



% Draws specified list of points, each point being identified in turn with one
% cross and a label, at the same rank order (L1 for the first point of the list,
% L2 for the next, etc.).
%
-spec draw_numbered_points( canvas(), [ point() ] ) ->  void().
draw_numbered_points( _Canvas={ myriad_object_ref, canvas, CanvasId },
					  Points ) ->
	get_main_loop_pid() ! { drawCanvasNumberedPoints, [ CanvasId, Points ] }.



% Loads image from specified path into specified canvas, pasting it at its upper
% left corner.
%
-spec load_image( canvas(), file_utils:file_name() ) -> void().
load_image( _Canvas={ myriad_object_ref, canvas, CanvasId }, Filename ) ->
	get_main_loop_pid() ! { loadCanvasImage, [ CanvasId, Filename ] }.



% Loads image from specified path into specified canvas, pasting it at specified
% location.
%
-spec load_image( canvas(), point(), file_utils:file_path() ) ->
						void().
load_image( _Canvas={ myriad_object_ref, canvas, CanvasId }, Position,
			FilePath ) ->
	get_main_loop_pid() ! { loadCanvasImage, [ CanvasId, Position, FilePath ] }.



% Blits the back-buffer of this canvas onto its visible area.
%
% The back-buffer remains as it was before this call.
%
-spec blit( canvas() ) -> void().
blit( _Canvas={ myriad_object_ref, canvas, CanvasId } ) ->
	get_main_loop_pid() ! { blitCanvas, CanvasId }.


% Clears the specified canvas.
-spec clear( canvas() ) -> void().
clear( _Canvas={ myriad_object_ref, canvas, CanvasId } ) ->
	get_main_loop_pid() ! { clearCanvas, CanvasId }.




% Associates specified sizer to specified window.
-spec set_sizer( window(), sizer() ) -> void().
set_sizer( _Canvas={ myriad_object_ref, canvas, CanvasId }, Sizer ) ->
	get_main_loop_pid() ! { getCanvasPanel, [ CanvasId ], self() },

	receive

		{ notifyCanvasPanel, Panel } ->
			set_sizer( Panel, Sizer )

	end;

set_sizer( Window, Sizer ) ->
	wxWindow:setSizer( Window, Sizer ).



% Shows (renders) specified window (or subclass thereof).
%
% Returns whether anything had to be done.
%
% This is the place where all widgets resolve their positions, sizes and
% contents.
%
-spec show( window() | [ window() ] ) -> boolean().
show( Windows ) when is_list( Windows )->
	Res = show_helper( Windows, _Acc=false ),
	get_main_loop_pid() ! { onShow, [ Windows ] },
	Res;

show( Window ) ->
	Res = wxWindow:show( Window ),
	get_main_loop_pid() ! { onShow, [ [ Window ] ] },
	Res.


show_helper( _Windows=[], Acc ) ->
	Acc;

show_helper( _Windows=[ W | T ], Acc ) ->
	NewAcc = wxWindow:show( W ) orelse Acc,
	show_helper( T, NewAcc ).



% Hides specified window.
%
% Returns whether anything had to be done.
%
-spec hide( window() ) -> boolean().
hide( Window ) ->
	wxWindow:show( Window, [ { show, false } ] ).



% Returns the size (as a 2D vector, i.e. {Width,Height}) of specified window.
-spec get_size( window() ) -> linear_2D:vector().
get_size( _Canvas={ myriad_object_ref, canvas, CanvasId } ) ->

	%trace_utils:debug_fmt( "Getting size of canvas #~B.", [ CanvasId ] ),

	get_main_loop_pid() ! { getCanvasSize, CanvasId, self() },
	receive

		{ notifyCanvasSize, Size } ->
			Size

	end;

get_size( Window ) ->
	wxWindow:getSize( Window ).



% Destructs specified window.
-spec destruct_window( window() ) -> void().
destruct_window( Window ) ->
	wxWindow:destroy( Window ).



% Frame section.
%
% A frame is a window whose size and position can (usually) be changed by the
% user. It usually has thick borders and a title bar, and can optionally contain
% a menu bar, toolbar and status bar. A frame can contain any window that is not
% a frame or dialog.
%
% Source: http://docs.wxwidgets.org/stable/wx_wxframe.html#wxframewxframe


% Creates a new frame, with default title, ID, parent, position, size and style.
%
% Note: this version apparently does not correctly initialise the frame;
% following error is indeed reported:
% "wxWidgets Assert failure: ./src/gtk/toplevel.cpp(988): \"m_widget\" in Show()
% : invalid frame".
%
-spec create_frame() -> frame().
create_frame() ->
	wxFrame:new().


% Creates a new frame, with default position, size, style, ID and parent.
-spec create_frame( title() ) -> frame().
create_frame( Title ) ->
	wxFrame:new( to_wx_parent( undefined ), to_wx_id( undefined ), Title ).



% Creates a new frame, with specified size, and default ID and parent.
-spec create_frame( title(), size() ) -> frame().
create_frame( Title, Size ) ->

	Options =  [ to_wx_size( Size ) ],

	%trace_utils:debug_fmt( "create_frame options: ~p.", [ Options ] ),

	wxFrame:new( to_wx_parent( undefined ), to_wx_id( undefined ), Title,
				 Options ).



% Creates a new frame, with default position, size and style.
%
% (internal use only)
%
-spec create_frame( title(), wx_id(), window() ) -> frame().
create_frame( Title, Id, Parent ) ->
	wxFrame:new( to_wx_parent( Parent ), to_wx_id( Id ), Title ).


% Creates a new frame, with default parent.
-spec create_frame( title(), position(), size(), frame_style() ) -> frame().
create_frame( Title, Position, Size, Style ) ->

	Options =  [ to_wx_position( Position ), to_wx_size( Size ),
				 { style, frame_style_to_bitmask( Style ) } ],

	%trace_utils:debug_fmt( "create_frame options: ~p.", [ Options ] ),

	wxFrame:new( to_wx_parent( undefined ), to_wx_id( undefined ), Title,
				 Options ).



% Creates a new frame.
%
% (internal use only)
%
-spec create_frame( title(), position(), size(), frame_style(), wx_id(),
					window() ) -> frame().
create_frame( Title, Position, Size, Style, Id, Parent ) ->

	Options =  [ to_wx_position( Position ), to_wx_size( Size ),
				 { style, frame_style_to_bitmask( Style ) } ],

	ActualId = to_wx_id( Id ),

	ActualParent = to_wx_parent( Parent ),

	wxFrame:new( ActualParent, ActualId, Title, Options ).




% Panel section.


% Creates a new panel.
-spec create_panel() -> panel().
create_panel() ->
	wxPanel:new().



% Creates a new panel, associated to specified parent.
-spec create_panel( window() ) -> panel().
create_panel( Parent ) ->
	wxPanel:new( Parent ).



% Creates a new panel, associated to specified parent and with specified
% options.
%
-spec create_panel( window(), panel_options() ) -> panel().
create_panel( Parent, Options ) ->

	ActualOptions = get_panel_options( Options ),

	wxPanel:new( Parent, ActualOptions ).



% Creates a new panel, associated to specified parent and with specified
% position and dimensions.
%
-spec create_panel( window(), coordinate(), coordinate(), length(),
					length() ) -> panel().
create_panel( Parent, X, Y, Width, Height ) ->
	create_panel( Parent, _Pos={ X, Y }, _Size={ Width, Height } ).



% Creates a new panel, associated to specified parent and with specified
% position and dimensions.
%
-spec create_panel( window(), position(), size() ) -> panel().
create_panel( Parent, Position, Size ) ->
	wxPanel:new( Parent, _Opts=[ { pos, to_wx_position( Position ) },
								 { size, to_wx_size( Size ) } ] ).


% Creates a new panel, associated to specified parent and with specified
% position and dimensions.
%
-spec create_panel( window(), position(), size(), panel_options() ) -> panel().
create_panel( Parent, Position, Size, Options ) ->

	FullOptions = get_panel_options( Options )
		++ [ to_wx_position( Position ), to_wx_size( Size ) ],

	%trace_utils:debug_fmt( "Creating panel: parent: ~w, position: ~w, "
	%					   "size: ~w, options: ~w, full options: ~w.",
	%					   [ Parent, Position, Size, Options, FullOptions ] ),

	wxPanel:new( Parent, FullOptions ).



% Creates a new panel, associated to specified parent, with specified
% position, dimensions and options.
%
-spec create_panel( window(), coordinate(), coordinate(),
					length(), length(), panel_options() ) -> panel().
create_panel( Parent, X, Y, Width, Height, Options ) ->

	ActualOptions = get_panel_options( Options ),

	wxPanel:new( Parent, X, Y, Width, Height, ActualOptions ).




% Button section.



% Creates a new (labelled) button, with parent specified.
-spec create_button( label(), window() ) -> button().
create_button( Label, Parent ) ->

	Id = ?wxID_ANY,

	Options = [ { label, Label } ],

	%trace_utils:info_fmt( "Button options (for any ID): ~p.",
	%                      [ Id, Options ] ),

	wxButton:new( Parent, Id, Options ).



% Creates new (labelled) buttons, with their (single, common) parent specified.
-spec create_buttons( [ label() ], window() ) -> [ button() ].
create_buttons( Labels, Parent ) ->
	create_buttons_helper( Labels, Parent, _Acc=[] ).


create_buttons_helper( _Labels=[], _Parent, Acc ) ->
	lists:reverse( Acc );

create_buttons_helper( [ Label | T ], Parent, Acc ) ->
	NewButton = create_button( Label, Parent ),
	create_buttons_helper( T, Parent, [ NewButton | Acc ] ).




% Creates a new button, with parent and most settings specified.
%
% (internal use only)
%
-spec create_button( label(), position(), size(), button_style(), wx_id(),
					 window() ) -> button().
create_button( Label, Position, Size, Style, Id, Parent ) ->

	Options = [ { label, Label }, to_wx_position( Position ),
				to_wx_size( Size ),
				{ style, gui_wx_backend:button_style_to_bitmask( Style ) } ],

	%trace_utils:info_fmt( "Button options for ID #~B: ~p.", [ Id, Options ] ),

	wxButton:new( Parent, Id, Options ).




% Sizer section.
%
% Sizers correspond actually to wxBoxSizer (wxSizer is an abstract class).


% Creates a sizer operating on specified orientation.
-spec create_sizer( orientation() ) -> sizer().
create_sizer( Orientation ) ->
	ActualOrientation = to_wx_orientation( Orientation ),
	wxBoxSizer:new( ActualOrientation ).



% Creates a sizer operating on specified orientation, within specified parent,
% with a box drawn around.
%
-spec create_sizer_with_box( orientation(), window() ) -> sizer().
create_sizer_with_box( Orientation, Parent ) ->

	ActualOrientation = to_wx_orientation( Orientation ),

	wxStaticBoxSizer:new( ActualOrientation, Parent ).



% Creates a sizer operating on specified orientation, within specified parent,
% with a box drawn around bearing specified label.
%
-spec create_sizer_with_labelled_box( orientation(), window(), label() ) ->
											sizer().
create_sizer_with_labelled_box( Orientation, Parent, Label ) ->

	ActualOrientation = to_wx_orientation( Orientation ),

	wxStaticBoxSizer:new( ActualOrientation, Parent, [ { label, Label } ] ).



% Adds specified element, or elements with options, to the specified sizer.
-spec add_to_sizer( sizer(), sizer_child() ) -> sizer_item();
				  ( sizer(), [ { sizer_child(), sizer_options() } ] ) -> void().
add_to_sizer( Sizer, _Element={ myriad_object_ref, canvas, CanvasId } ) ->
	get_main_loop_pid() ! { getPanelForCanvas, CanvasId, self() },
	io:format( "SEND"),
	receive

		{ notifyCanvasPanel, AssociatedPanel } ->
	io:format( "RECEIVED"),
			add_to_sizer( Sizer, AssociatedPanel )

	end;

% List version:
add_to_sizer( _Sizer, _Elements=[] ) ->
	ok;

add_to_sizer( Sizer, _Elements=[ { Elem, Opts } | T ] ) ->
	add_to_sizer( Sizer, Elem, Opts ),
	add_to_sizer( Sizer, T );

add_to_sizer( Sizer, Element ) ->
	trace_utils:debug_fmt( "Adding ~w to sizer ~w.", [ Element, Sizer ] ),
	wxSizer:add( Sizer, Element ).



% Adds specified element (or elements), with (common) options, to the specified
% sizer.
%
-spec add_to_sizer( sizer(), sizer_child(), sizer_options() ) -> sizer_item();
				  ( sizer(), [ sizer_child() ], sizer_options() ) -> void().
add_to_sizer( Sizer, _Element={ myriad_object_ref, canvas, CanvasId },
			  Options ) ->
	get_main_loop_pid() ! { getPanelForCanvas, CanvasId, self() },
	receive

		{ notifyCanvasPanel, AssociatedPanel } ->
			add_to_sizer( Sizer, AssociatedPanel, Options )

	end;

add_to_sizer( _Sizer, _Elements=[], _Options ) ->
	ok;

add_to_sizer( Sizer, _Elements=[ Elem | T ], Options ) ->
	add_to_sizer( Sizer, Elem, Options ),
	add_to_sizer( Sizer, T, Options );

add_to_sizer( Sizer, Element, Options ) ->

	ActualOptions = gui_wx_backend:to_wx_sizer_options( Options ),

	wxSizer:add( Sizer, Element, ActualOptions ).




% Clears specified sizer, detaching and deleting all its child windows.
-spec clear_sizer( sizer() ) -> void().
clear_sizer( Sizer ) ->
	clear_sizer( Sizer, _DeleteWindows=true ).



% Clears specified sizer, detaching all its child windows, and deleting them iff
% requested.
%
-spec clear_sizer( sizer(), boolean() ) -> void().
clear_sizer( Sizer, DeleteWindows ) ->
	wxSizer:clear( Sizer, [ { delete_windows, DeleteWindows } ] ).



% Status bar section.
%
% Status bars are very useful to trace events.


% Creates and attaches a status bar to the specified frame.
-spec create_status_bar( frame() ) -> status_bar().
create_status_bar( Frame ) ->
	% No interesting option:
	wxFrame:createStatusBar( Frame ).



% Pushes specified text in the specified status bar.
-spec push_status_text( text(), status_bar() ) -> void().
push_status_text( Text, StatusBar ) ->
	wxStatusBar:pushStatusText( StatusBar, Text ).




% General MyriadGUI helpers.


% Requests the creation of the specified instance (to be done from the MyriadGUI
% main loop), and returns the corresponding GUI object reference.
%
-spec execute_instance_creation( myriad_object_type(),
						construction_parameters() ) -> myriad_object_ref().
execute_instance_creation( ObjectType, ConstructionParams ) ->

	trace_utils:debug_fmt( "Requesting the creation of a '~ts' instance, "
		"based on following construction parameters:~n~w.",
		[ ObjectType, ConstructionParams ] ),

	LoopPid = get_main_loop_pid(),

	LoopPid ! { createInstance, [ ObjectType, ConstructionParams ], self() },

	receive

		% Match on the object type:
		{ instance_created, ObjectType, ObjectRef } ->

			trace_utils:debug_fmt( "'~ts' instance created, now referenced "
				"as ~w.", [ ObjectType, ObjectRef ] ),

			ObjectRef

	end.



% Fetches (from the MyriadGUI environment) the PID of the process in charge of
% running the main GUI loop.
%
-spec get_main_loop_pid() -> pid().
get_main_loop_pid() ->

	GUIEnv = get_gui_env(),

	GUIEnv#gui_env.loop_pid.



% Fetches (from the process dictionary) the MyriadGUI environment.
-spec get_gui_env() -> gui_env().
get_gui_env() ->

	case get( ?gui_env_process_key ) of

		undefined ->
			trace_utils:error_fmt( "No MyriadGUI environment available for "
				"process ~w.", [ self() ] ),
			throw( { no_myriad_gui_env, self() } );

		Env ->
			Env

	end.




%
% General-purpose section.
%



% Returns a textual representation of the specified GUI object.
-spec object_to_string( gui_object() ) -> ustring().
object_to_string( #myriad_object_ref{ object_type=ObjectType,
									  myriad_instance_pid=InstancePid } ) ->
	text_utils:format( "~ts-~B", [ ObjectType, InstancePid ] );

object_to_string( { wx_ref, InstanceRef, WxObjectType, _State=[] } ) ->
	% Ex: {wx_ref,35,wxFrame,[]}
	ObjectType = gui_wx_backend:from_wx_object_type( WxObjectType ),
	text_utils:format( "~ts-~B", [ ObjectType, InstanceRef ] );

object_to_string( { wx_ref, InstanceRef, WxObjectType, State } ) ->
	ObjectType = gui_wx_backend:from_wx_object_type( WxObjectType ),
	text_utils:format( "~ts-~B whose state is ~p",
					   [ ObjectType, InstanceRef, State ] ).


% Returns a textual representation of the specified GUI event context.
-spec context_to_string( gui_event_context() ) -> ustring().
context_to_string( #gui_event_context{ id=Id, user_data=UserData,
									   backend_event=WxEvent } ) ->

	IdString = gui_wx_backend:wx_id_to_string( Id ),

	UserDataString = case UserData of

		[] ->
			"no user data";

		_ ->
			text_utils:format( "following user data: ~p", [ UserData ] )

	end,

	EventString = text_utils:format( "~p", [ WxEvent ] ),

	text_utils:format( "context for event ~ts: ~ts and ~ts",
					   [ EventString, IdString, UserDataString ] ).
