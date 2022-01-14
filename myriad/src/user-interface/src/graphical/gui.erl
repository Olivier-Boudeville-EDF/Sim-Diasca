% Copyright (C) 2010-2022 Olivier Boudeville
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


% @doc Gathering of various <b>facilities for Graphical User Interfaces</b>
% (GUI).
%
% We name this library MyriadGUI (shortened here, whenever it is not ambiguous,
% in 'gui').
%
% The purpose of MyriadGUI is to wrap, complement and improve what we consider
% the best set of gui backends available (previously: gs alone; now: wx+esdl,
% with OpenGL), for classical applications and multimedia ones (ex: games).
%
% See `gui_test.erl' for the corresponding test.
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
% Relying optionally:
% - on OpenGL, for efficient 2D/3D rendering (see gui_opengl.erl)
% - possibly, some day, on esdl (https://github.com/dgud/esdl), for adequate
% lower-level primitives (ex: management of input devices)



% General use:
%
% From a user process (a test, an application, etc.) the GUI support is first
% started (gui:start/0), then widgets (windows, frames, panels, buttons, etc.)
% are created (ex: gui:create_frame/6) and the user process subscribes to the
% events it is interested in (as a combination of an event type and a
% widget-as-an-event-emitter; for example:
% gui:subscribe_to_events({onWindowClosed, MainFrame})). It triggers also any
% relevant operation (ex: clearing widgets, setting various parameters),
% generally shows at least a main frame and records the GUI state that it needs
% (typically containing at least the MyriadGUI references of the widgets that it
% created).
%
% Then the user process enters its (GUI) specific main loop, from which it will
% receive the events that it subscribed to, to which it will react by performing
% application-specific operations and/or GUI-related operations (creating,
% modifying, deleting widgets).
%
% Beware of asynchronous operations (i.e. the sending of oneways) to the GUI
% main loop process, as they may introduce nasty race conditions. For example,
% if the subscribing to events (ex: to the main frame being shown) was left
% asynchronous, then the operations coming next from the user code may be fast
% enough so that a wx instance (ex: the main frame) processes these events (ex:
% requesting the main frame to be shown) before even that the GUI main loop had
% a chance to declare its connection to them. This would result in an expected
% event (onShown here) never to be emitted and thus never to be received.
%
% Another race condition can happen when destructing a resource (ex: when
% issuing 'gui:destruct_window(MainFrame)' whereas some previous operations may
% not have been processed yet (ex: gui_canvas:set_draw_color/2), resulting in
% access errors (ex: {unknown_env,{wxPen,new,2}}) due to an
% use-after-destroy. Short of making most operations synchronous, no real
% solution seems to exist. The issue exists maybe even without MyriadGUI, but
% having a middle process increases the likeliness of such a problem due to the
% extra induced latency.
%
% Generally at least one condition is defined in order to leave that main loop
% and stop the GUI (gui:stop/0).
%
% See lorenz_test.erl as a full, executable usage example thereof.


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
	wx_server ::wx_object(),

	% PID of the main loop:
	loop_pid :: pid() } ).



-type gui_env() :: #gui_env{}.
% Stores the current, user-side (client) state (merely references) of the GUI.
%
% Like wx:wx_env(); kept in the process dictionary for easier sharing that if
% using a naming service or having to keep around a bound variable.


% Current backend is wx (WxWidgets).
%
% (useful to avoid including the header of wx in our own public ones)
-opaque backend_event() :: gui_event:wx_event().
% An (opaque) backend GUI event.



% With wx, device contexts (ex: obtained from wxMemoryDC:new/1) must be
% explicitly managed (ex: wxMemoryDC:destroy/1 must be called when finished with
% them), which is inconvenient and error-prone.
%
-type device_context() :: wx_object().
% Designates an abstract device where rendering can take place, and which can be
% the source or target of a blit. Akin to a surface in SDL (libsdl).



-type canvas() :: gui_canvas:canvas().
% A basic canvas (not to be mixed with an OpenGL one, opengl_canvas/0).



-type opengl_canvas() :: gui_opengl:gl_canvas().
% An OpenGL canvas (not to be mixed with a basic one, canvas/0).

-type opengl_context() :: gui_opengl:gl_context().
% An OpenGL context.



% Basic GUI operations.
-export([ is_available/0, start/0, start/1, set_debug_level/1, stop/0 ]).


% Extra overall operations.
-export([ batch/1 ]).


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
		  set_sizer/2, show/1, hide/1, get_size/1, get_client_size/1,
		  maximise_in_parent/1, sync/1, enable_repaint/1,
		  lock_window/1, unlock_window/1, destruct_window/1 ]).


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
		  create_sizer_with_labelled_box/3, create_sizer_with_labelled_box/4,
		  add_to_sizer/2, add_to_sizer/3,
		  clear_sizer/1, clear_sizer/2 ]).


% Status bars:
-export([ create_status_bar/1, push_status_text/2, push_status_text/3 ]).


% Brushes:
-export([ create_brush/1, destruct_brush/1 ]).


% Canvas support (forwarded to gui_canvas).
-export([ create_canvas/1, set_draw_color/2, set_fill_color/2,
		  set_background_color/2, get_rgb/2, set_rgb/2,
		  draw_line/3, draw_line/4, draw_lines/2, draw_lines/3,
		  draw_segment/4, draw_polygon/2,
		  draw_label/3,
		  draw_cross/2, draw_cross/3, draw_cross/4, draw_labelled_cross/4,
		  draw_labelled_cross/5, draw_circle/3, draw_circle/4,
		  draw_numbered_points/2,
		  load_image/2, load_image/3,
		  resize/2, blit/1, clear/1 ]).


% Bitmaps:
-export([ create_bitmap/1, create_blank_bitmap/1, create_blank_bitmap/2,
		  create_blank_bitmap_for/1,
		  lock_bitmap/1, draw_bitmap/3, unlock_bitmap/1, destruct_bitmap/1 ]).


% Device contexts:
-export([ clear_device_context/1, blit/5, blit/6 ]).


% Fonts:
-export([ create_font/4, create_font/5 ]).


% For related, public defines:
-include("gui.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").


% For myriad_spawn*:
-include("spawn_utils.hrl").



% Type declarations:

-type length() :: linear:integer_distance().
% A length, as a number of pixels.

-type width() :: length().
% A width, as a number of pixels.

-type height() :: length().
% An height, as a number of pixels.


-type coordinate() :: linear:integer_coordinate().
% For a GUI, coordinates are an integer number of pixels.


-type point() :: point2:integer_point2().
% A pixel-wise GUI point (as point2:point2() would allow for floating-point
% coordinates).

-type position() :: point() | 'auto'.
% Position, in pixel coordinates, typically of a widget.


-type dimensions() :: linear_2D:integer_rect_dimensions().
% Dimensions in pixels, as {IntegerWidth,IntegerHeight}.

-type size() :: dimensions() | 'auto'.
% Size, typically of a widget.


-type orientation() :: 'vertical' | 'horizontal'.
% A vertical orientation means piling elements top to bottom for example, while
% an horizontal means left to right, for example.


-type fps() :: count().
% Number of frames per second.
%
% The old Charlie Chaplin movies were shot at 16 frames per second and are
% noticeably jerky.
%
% 60 frames per second is smoother than 30, and 120 is marginally better than
% 60; beyond 120 fps has no real interest as it exceeds eye perception.



% MVC (Model-View-Controller) section.
%
% Generally the view knows (i.e. has the PID of) the model, the controller knows
% the model and the model does not know specifically either of them.

% See https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller for
% further information.

-type model_pid() :: pid().
% The PID of a process whose role is to be a Model in the sense of the MVC
% pattern.


-type view_pid() :: pid().
% The PID of a process whose role is to be a View in the sense of the MVC
% pattern.


-type controller_pid() :: pid().
% The PID of a process whose role is to be a Controller in the sense of the MVC
% pattern.



% Widget types.


-type object_type() :: wx_object_type() | myriad_object_type().
% Internal, overall types for all GUI objects.


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
% MyriadGUI-translated version of a native wx type, i.e. of the
% wx_native_object_type().
%
% (ex: 'window', instead of 'wxWindow').


-type myriad_object_type() :: 'canvas'.
% The additional widget types introduced by Myriad.


% Also: | all other *_state() that may be introduced
-type myriad_object_state() :: gui_canvas:canvas_state().
% Records the actual state of a MyriadGUI object.


-type construction_parameters() :: [ term() ].
% The construction parameters of a MyriadGUI object.


-type myriad_instance_id() :: count().
% Myriad-specific instance identifier, corresponding a reference in the internal
% MyriadGUI type table.


-type id() :: maybe( wx_id() ).
% wx-specific object identifier (defined so that the gui_event_context (public)
% record has no trace of the backend).
%
% May not be defined if the actual event comes from MyriadGUI itself (and thus
% not wx).


-type gui_object() :: wx:wx_object() | myriad_object_ref().
% Reference to a GUI object (often designated as "widget" here), somewhat akin
% to a PID.
%
% (ex: {wx_ref,35,wxFrame,[]} or {myriad_object_ref,canvas,12}).


-type wx_server() :: gui_object().
% Alias to designate more clearly the wx server.


% Defining the actual widget types corresponding to wx_object_type():


-opaque window() :: maybe( wxWindow:wxWindow() | gui_canvas:canvas() ).
% Any kind of windows, that is widget (ex: any canvas is a window).

-opaque frame() :: wxFrame:wxFrame().

-opaque panel() :: wxPanel:wxPanel().

-opaque button() :: wxButton:wxButton().

-opaque sizer() :: wxSizer:wxSizer().


-opaque sizer_child() :: window() | sizer().
% Elements that can be included in a sizer.


-opaque sizer_item() :: wxSizerItem:wxSizerItem().

-opaque status_bar() :: wxStatusBar:wxStatusBar().


-opaque bitmap() :: gui_image:bitmap().
% Platform-dependent bitmap, either monochrome or colour (with or without alpha
% channel).
%
% Intended to be a wrapper of whatever is the native image format, which is
% quickest/easiest to draw to a display context.


-opaque brush() :: wxBrush:wxBrush().

-opaque back_buffer() :: wxMemoryDC:wxMemoryDC().


-type font() :: gui_font:font().

-type font_size() :: gui_font:font_size().

-type point_size() :: gui_font:point_size().

-type font_family() :: gui_font:font_family().

-type font_style() :: gui_font:font_style().

-type font_weight() :: gui_font:font_weight().

-type font_option() :: gui_font:font_option().

-type title() :: text().
-type label() :: text().


-type user_data() :: any().
% User data, as specified in the connect call.


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
% Options for windows, see
% [http://docs.wxwidgets.org/stable/classwx_window.html]


-type window_style() :: window_style_opt() | [ window_style_opt() ].

-type window_option() :: { pos, point() }
					   | { size, size() }
					   | { style, [ window_style_opt() ] }.

% Unused: -type window_options() :: [ window_option() ].


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
% Options for frames, see
% [http://docs.wxwidgets.org/stable/classwx_frame.html].



-type frame_style() :: frame_style_opt() | [ frame_style_opt() ].


-type panel_option() :: window_option().
% Options for panels, see
% [http://docs.wxwidgets.org/stable/classwx_panel.html].


-type panel_options() :: [ panel_option() ].


-type button_style_opt() :: 'default'
						  | 'left_justified'
						  | 'right_justified'
						  | 'top_justified'
						  | 'bottom_justified'
						  | 'exact_fit'
						  | 'flat'.
% Options for button style, see
% [http://docs.wxwidgets.org/stable/classwx_button.html].


-type button_style() :: button_style_opt() | [ button_style_opt() ].


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
% Options for sizers, see [https://docs.wxwidgets.org/stable/classwx_sizer.html]


-type sizer_flag() :: sizer_flag_opt() | [ sizer_flag_opt() ].

-type sizer_option() :: { 'proportion', integer() }
					  | { 'flag', sizer_flag() }
					  | { 'border', integer() }
					  | { 'userData', gui_object() }.


-type sizer_options() :: [ sizer_option() ].


-type image() :: gui_image:image().
% An image is a bitmap buffer of RGB bytes with an optional buffer for the alpha
% bytes.
%
% It is thus generic, independent from platforms and image file formats.


-type connect_opt() ::   { 'id', integer() }
					   | { lastId, integer() }
					   | { skip, boolean() }
						% Triggers handle_sync_event/3, see the wx_object
						% behaviour:
						%
					   |   callback
					   | { callback, function() }
					   | { userData, term() }.
% Options for event management connections.


-type connect_options() :: connect_opt() | [ connect_opt() ].


-type debug_level_opt() :: 'none' | 'calls' | 'life_cycle'.
% Mapped internally to 'none', 'verbose', 'trace', etc.; see
% convert_debug_level/1.



-type debug_level() :: debug_level_opt() | [ debug_level_opt() ].

-type error_message() :: term().



-export_type([ length/0, width/0, height/0,
			   coordinate/0, point/0, position/0, size/0,
			   orientation/0, fps/0,
			   model_pid/0, view_pid/0, controller_pid/0,
			   object_type/0, wx_object_type/0,
			   myriad_object_type/0, myriad_instance_id/0,
			   title/0, label/0, user_data/0,
			   id/0, gui_object/0, wx_server/0,
			   window/0, frame/0, panel/0, button/0,
			   sizer/0, sizer_child/0, sizer_item/0, status_bar/0,

			   font/0, font_size/0, point_size/0, font_family/0, font_style/0,
			   font_weight/0,

			   bitmap/0, brush/0, back_buffer/0, device_context/0, canvas/0,
			   opengl_canvas/0, opengl_context/0,
			   construction_parameters/0, backend_event/0, connect_options/0,
			   window_style/0, frame_style/0, button_style/0,

			   window_style_opt/0, window_option/0,
			   frame_style_opt/0,
			   panel_option/0, panel_options/0,
			   button_style_opt/0,
			   sizer_flag_opt/0, sizer_flag/0, sizer_option/0, sizer_options/0,
			   image/0,
			   connect_opt/0,
			   debug_level_opt/0, debug_level/0, error_message/0 ]).


% To avoid unused warnings:
-export_type([ myriad_object_state/0 ]).


% Function shorthands:
-import( gui_wx_backend, [ to_wx_parent/1, to_wx_id/1, to_wx_position/1,
						   to_wx_size/1, to_wx_orientation/1,
						   frame_style_to_bitmask/1, get_panel_options/1 ]).


% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type text() :: ustring().

-type any_file_path() :: file_utils:any_file_path().

-type line2() :: linear_2D:line2().

-type color() :: gui_color:color().
-type color_by_decimal() :: gui_color:color_by_decimal().
-type color_by_decimal_with_alpha() :: gui_color:color_by_decimal_with_alpha().

-type event_subscription_spec() :: gui_event:event_subscription_spec().

-type wx_id() :: gui_wx_backend:wx_id().

-type wx_object() :: wx:wx_object().


% GUI-specific defines:


% Key of the MyriadGUI environment, in the process dictionary:
-define( gui_env_process_key, myriad_gui_env ).




% Section for basic GUI overall operations.


% @doc Tells whether this user-interface backend is available.
-spec is_available() -> boolean().
is_available() ->
	% As simple as:
	system_utils:has_graphical_output().




% @doc Starts the MyriadGUI subsystem.
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

	cond_utils:if_defined( myriad_debug_user_interface, trace_utils:info_fmt(
		"Main loop running on GUI process ~w (created from user process ~w).",
		[ LoopPid, self() ] ) ),

	GUIEnv = #gui_env{ wx_server=WxServer, loop_pid=LoopPid },

	% Stored in the process dictionary of the user process, like for wx:
	put( ?gui_env_process_key, GUIEnv ).



% @doc Starts the GUI subsystem, with specified debug level.
-spec start( debug_level() ) -> void().
start( DebugLevel ) ->
	start(),
	set_debug_level( DebugLevel ).



% @doc Sets the debug level(s) of the GUI.
-spec set_debug_level( debug_level() ) -> void().
set_debug_level( DebugLevels ) when is_list( DebugLevels ) ->
	wx:debug( [ gui_wx_backend:to_wx_debug_level( L ) || L <- DebugLevels ] );

set_debug_level( DebugLevel ) ->
	set_debug_level( [ DebugLevel ] ).



% @doc Subscribes the current, calling process to the specified kind of events
% (event type and emitter), like {onWindowClosed, MyFrame}.
%
% This process will then receive MyriadGUI callback messages whenever events
% that match happen, such as: {onWindowClosed, [MyFrame, EventContext]}.
%
% By default the corresponding event will not be transmitted upward in the
% widget hierarchy (as this event will be expected to be processed for good by
% the subscriber(s) it has been dispatched to), unless the propagate_event/1
% function is called from one of them.
%
% Note that, at least when creating the main frame, if having subscribed to
% onShown and onResized, on creation first a onResized event will be received by
% the subscriber (typically for a 20x20 size), then a onShow event.
%
-spec subscribe_to_events( event_subscription_spec() ) -> void().
subscribe_to_events( SubscribedEvents ) when is_list( SubscribedEvents ) ->

	GUIEnv = get_gui_env(),

	LoopPid = GUIEnv#gui_env.loop_pid,

	% This is, in logical terms, a oneway (received in
	% gui_event:process_event_message/2), yet it must be a request (i.e. it must
	% be synchronous), otherwise a race condition exists (ex: the user
	% subscribes to 'onShown' for the main frame, and just after executes
	% 'gui:show(MainFrame)'. If subscribing is non-blocking, then the main frame
	% may be shown before being connected to the main loop, and thus it will not
	% notify the GUI main loop it is shown...

	LoopPid ! { subscribeToEvents, [ SubscribedEvents, self() ] },

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:info_fmt( "User process subscribing as ~w to ~w about "
			"following events:~n~p.", [ self(), LoopPid, SubscribedEvents ] ) ),

	receive

		onEventSubscriptionProcessed ->
		  ok

	end;

subscribe_to_events( SubscribedEvent ) when is_tuple( SubscribedEvent ) ->
	subscribe_to_events( [ SubscribedEvent ] ).



% @doc Propagates the event designated by the specified context upward in the
% widget hierarchy (instead of the default, which is considering that it has
% been processed once for all, and thus shall not be propagated further).
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



% @doc Stops the GUI subsystem.
-spec stop() -> void().
stop() ->

	% No wx_server needed:
	ok = wx:destroy(),

	% Remove from process dictionary:
	put( ?gui_env_process_key, _Value=undefined ).



% @doc Batches the sequence of GUI operations encasulated on the specified
% function, and returns the value this function returned.
%
% May improve performance of the command processing, by grabbing the backend
% thread so that no event processing will be done before the complete batch of
% commands is invoked.
%
% Example: Result = gui:batch(fun() -> do_init(Config) end).
%
-spec batch( function() ) -> term().
batch( GUIFun ) ->
	wx:batch( GUIFun ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Widget section.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Common section.



% @doc Attaches a tooltip to specified widget.
-spec set_tooltip( window(), label() ) -> void().
set_tooltip( _Canvas={ myriad_object_ref, canvas, CanvasId }, Label ) ->
	get_main_loop_pid() ! { setTooltip, [ CanvasId, Label ] };

set_tooltip( Window, Label ) ->

	%trace_utils:debug_fmt( "Setting tooltip '~ts' to ~ts.",
	%                       [ Label, object_to_string( Window ) ] ),

	% For an unknown reason, works on panels but never on buttons:
	wxWindow:setToolTip( Window, Label ).



% Window section.
%
% Base class for all windows and represents any visible object on screen. All
% controls, top level windows and so on are windows. Sizers and device contexts
% are not, however, as they do not appear on screen themselves.


% @doc Creates a window.
-spec create_window() -> window().
create_window() ->
	wxWindow:new().


% @doc Creates a window of specified identifier.
%
% @hidden (internal use only)
%
-spec create_window( wx_id(), window() ) -> window().
create_window( Id, Parent ) ->

	ActualId = to_wx_id( Id ),
	ActualParent = to_wx_parent( Parent ),

	wxWindow:new( ActualParent, ActualId ).


% @doc Creates a window of specified size.
-spec create_window( size() ) -> window().
create_window( Size ) ->

	ActualId = to_wx_id( undefined ),
	ActualParent = to_wx_parent( undefined ),

	Options = [ to_wx_size( Size ) ],

	wxWindow:new( ActualParent, ActualId, Options ).


% @doc Creates a window of specified settings.
%
% @hidden (internal use only)
%
-spec create_window( position(), size(), window_style(), wx_id(), window() ) ->
												window().
create_window( Position, Size, Style, Id, Parent ) ->

	Options = [ to_wx_position( Position ), to_wx_size( Size ),
				{ style, gui_wx_backend:window_style_to_bitmask( Style ) } ],

	ActualId = to_wx_id( Id ),
	ActualParent = to_wx_parent( Parent ),

	wxWindow:new( ActualParent, ActualId, Options ).




% Canvas section.


% @doc Creates a (basic) canvas, attached to the specified parent window.
%
% Note: not to be mixed up with gui_opengl:create_canvas/{1,2}.
%
-spec create_canvas( window() ) -> canvas().
create_canvas( Parent ) ->
	% Returns the corresponding myriad_object_ref:
	execute_instance_creation( canvas, [ Parent ] ).



% @doc Sets the color to be used for the drawing of the outline of shapes.
-spec set_draw_color( canvas(), color() ) -> void().
set_draw_color( _Canvas={ myriad_object_ref, canvas, CanvasId }, Color ) ->
	get_main_loop_pid() ! { setCanvasDrawColor, [ CanvasId, Color ] }.



% @doc Sets the color to be using for filling surfaces.
%
% An undefined color corresponds to a fully transparent one.
%
-spec set_fill_color( canvas(), maybe( color() ) ) -> void().
set_fill_color( _Canvas={ myriad_object_ref, canvas, CanvasId }, Color ) ->
	get_main_loop_pid() ! { setCanvasFillColor, [ CanvasId, Color ] }.



% @doc Sets the background color of the specified window.
-spec set_background_color( window(), color() ) -> void().
set_background_color( _Canvas={ myriad_object_ref, canvas, CanvasId },
					  Color ) ->

	%trace_utils:debug_fmt( "Setting background color of canvas ~w to ~p.",
	%                       [ Canvas, Color ] ),

	get_main_loop_pid() ! { setCanvasBackgroundColor, [ CanvasId, Color ] };

set_background_color( Window, Color ) ->

	ActualColor = gui_color:get_color( Color ),

	wxWindow:setBackgroundColour( Window, ActualColor ).



% @doc Returns the RGB value of the pixel at specified position.
-spec get_rgb( canvas(), point() ) -> color_by_decimal_with_alpha().
get_rgb( _Canvas={ myriad_object_ref, canvas, CanvasId }, Point ) ->

	get_main_loop_pid() ! { getCanvasRGB, [ CanvasId, Point ], self() },

	receive

		{ notifyCanvasRGB, Color } ->
			Color

	end.



% @doc Sets the pixel at specified position to the current RGB point value.
-spec set_rgb( canvas(), point() ) -> void().
set_rgb( _Canvas={ myriad_object_ref, canvas, CanvasId }, Point ) ->
	get_main_loop_pid() ! { setCanvasRGB, [ CanvasId, Point ] }.



% @doc Draws a line between the specified two points in the back-buffer of the
% specified canvas, using current draw color.
%
-spec draw_line( canvas(), point(), point() ) -> void().
draw_line( _Canvas={ myriad_object_ref, canvas, CanvasId }, P1, P2 ) ->
	get_main_loop_pid() ! { drawCanvasLine, [ CanvasId, P1, P2 ] }.



% @doc Draws a line between the specified two points in specified canvas, with
% specified color.
%
-spec draw_line( canvas(), point(), point(), color() ) -> void().
draw_line( _Canvas={ myriad_object_ref, canvas, CanvasId }, P1, P2, Color ) ->
	get_main_loop_pid() ! { drawCanvasLine, [ CanvasId, P1, P2, Color ] }.



% @doc Draws lines between the specified list of points, in specified canvas,
% using current draw color.
%
-spec draw_lines( canvas(), [ point() ] ) -> void().
draw_lines( _Canvas={ myriad_object_ref, canvas, CanvasId }, Points ) ->
	get_main_loop_pid() ! { drawCanvasLines, [ CanvasId, Points ] }.



% @doc Draws lines between specified list of points in specified canvas, with
% specified color.
%
-spec draw_lines( canvas(), [ point() ], color() ) -> void().
draw_lines( _Canvas={ myriad_object_ref, canvas, CanvasId }, Points, Color ) ->
	get_main_loop_pid() ! { drawCanvasLines, [ CanvasId, Points, Color ] }.



% @doc Draws a segment of the line L between the two specified ordinates.
%
% Line L must not have for equation Y=constant (i.e. its A parameter must not be
% null).
%
-spec draw_segment( canvas(), line2(), coordinate(), coordinate() ) -> void().
draw_segment( _Canvas={ myriad_object_ref, canvas, CanvasId }, L, Y1, Y2 ) ->
	get_main_loop_pid() ! { drawCanvasSegment, [ CanvasId, L, Y1, Y2 ] }.



% @doc Draws the specified polygon, closing the lines and filling them.
-spec draw_polygon( canvas(), [ point() ] ) -> void().
draw_polygon( _Canvas={ myriad_object_ref, canvas, CanvasId }, Points ) ->
	get_main_loop_pid() ! { drawCanvasPolygon, [ CanvasId, Points ] }.



% @doc Draws the specified label (a plain string) at specified position, on
% specified canvas, using the current draw color.
%
-spec draw_label( canvas(), point(), label() ) -> void().
draw_label( _Canvas={ myriad_object_ref, canvas, CanvasId }, Point, Label ) ->
	get_main_loop_pid() ! { drawCanvasLabel, [ CanvasId, Point, Label  ] }.



% @doc Draws an upright cross at specified location (2D point), with default
% edge length.
%
-spec draw_cross( canvas(), point() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, canvas, CanvasId }, Location ) ->
	get_main_loop_pid() ! { drawCanvasCross, [ CanvasId, Location ] }.



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length.
%
-spec draw_cross( canvas(), point(), length() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, canvas, CanvasId }, Location,
			EdgeLength ) ->
	get_main_loop_pid() ! { drawCanvasCross,
							[ CanvasId, Location, EdgeLength ] }.



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length and color.
%
-spec draw_cross( canvas(), point(), length(), color() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, canvas, CanvasId }, Location,
			EdgeLength, Color ) ->
	get_main_loop_pid() ! { drawCanvasCross,
							[ CanvasId, Location, EdgeLength, Color ] }.



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length and companion label.
%
-spec draw_labelled_cross( canvas(), point(), length(), label()  ) -> void().
draw_labelled_cross( _Canvas={ myriad_object_ref, canvas, CanvasId }, Location,
					 EdgeLength, LabelText ) ->
	get_main_loop_pid() ! { drawCanvasLabelledCross,
							[ CanvasId, Location, EdgeLength, LabelText  ] }.



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length and companion label, and with specified color.
%
-spec draw_labelled_cross( canvas(), point(), length(), color(), label() ) ->
									 void().
draw_labelled_cross( _Canvas={ myriad_object_ref, canvas, CanvasId }, Location,
					 EdgeLength, Color, LabelText ) ->
	get_main_loop_pid() ! { drawCanvasLabelledCross,
						[ CanvasId, Location, EdgeLength, Color, LabelText  ] }.



% @doc Renders specified circle (actually, depending on the fill color, it may
% be a disc) in specified canvas.
%
-spec draw_circle( canvas(), point(), length() ) -> void().
draw_circle( _Canvas={ myriad_object_ref, canvas, CanvasId }, Center,
			 Radius ) ->
	%trace_utils:debug_fmt( "Drawing circle centered at ~p.", [ Center ] ),
	get_main_loop_pid() ! { drawCanvasCircle, [ CanvasId, Center, Radius ] }.



% @doc Renders specified circle (actually, depending on the fill color, it may
% be a disc) in specified canvas.
%
-spec draw_circle( canvas(), point(), length(), color() ) -> void().
draw_circle( _Canvas={ myriad_object_ref, canvas, CanvasId }, Center, Radius,
			 Color ) ->

	%trace_utils:debug_fmt( "Drawing circle centered at ~p, of colour ~p.",
	%                       [ Center, Color ] ),

	get_main_loop_pid() !
		{ drawCanvasCircle, [ CanvasId, Center, Radius, Color ] }.



% @doc Draws specified list of points, each point being identified in turn with
% one cross and a label, at the same rank order (L1 for the first point of the
% list, L2 for the next, etc.).
%
-spec draw_numbered_points( canvas(), [ point() ] ) -> void().
draw_numbered_points( _Canvas={ myriad_object_ref, canvas, CanvasId },
					  Points ) ->
	get_main_loop_pid() ! { drawCanvasNumberedPoints, [ CanvasId, Points ] }.



% @doc Loads image from the specified path into specified canvas, pasting it at
% its upper left corner.
%
-spec load_image( canvas(), any_file_path() ) -> void().
load_image( _Canvas={ myriad_object_ref, canvas, CanvasId }, Filename ) ->
	get_main_loop_pid() ! { loadCanvasImage, [ CanvasId, Filename ] }.



% @doc Loads image from the specified path into specified canvas, pasting it at
% specified location.
%
-spec load_image( canvas(), point(), any_file_path() ) -> void().
load_image( _Canvas={ myriad_object_ref, canvas, CanvasId }, Position,
			FilePath ) ->
	get_main_loop_pid() ! { loadCanvasImage, [ CanvasId, Position, FilePath ] }.



% @doc Resizes the specified canvas according to the specified new size.
-spec resize( canvas(), size() ) -> void().
resize( _Canvas={ myriad_object_ref, canvas, CanvasId }, NewSize ) ->
	get_main_loop_pid() ! { resizeCanvas, [ CanvasId, NewSize ] }.



% @doc Blits the back-buffer of this canvas onto its visible area.
%
% The back-buffer remains as it was before this call.
%
-spec blit( canvas() ) -> void().
blit( _Canvas={ myriad_object_ref, canvas, CanvasId } ) ->
	get_main_loop_pid() ! { blitCanvas, CanvasId }.


% @doc Clears the specified canvas.
%
% Note: the result will not be visible until the canvas is blitted.
%
-spec clear( canvas() ) -> void().
clear( _Canvas={ myriad_object_ref, canvas, CanvasId } ) ->
	get_main_loop_pid() ! { clearCanvas, CanvasId }.




% @doc Associates specified sizer to specified window.
-spec set_sizer( window(), sizer() ) -> void().
set_sizer( _Canvas={ myriad_object_ref, canvas, CanvasId }, Sizer ) ->
	get_main_loop_pid() ! { getCanvasPanel, [ CanvasId ], self() },

	receive

		{ notifyCanvasPanel, Panel } ->
			set_sizer( Panel, Sizer )

	end;

set_sizer( Window, Sizer ) ->
	wxWindow:setSizer( Window, Sizer ).



% @doc Shows (renders) specified window (or subclass thereof).
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
	Res;

show( Window ) ->
	%trace_utils:debug_fmt( "Showing window ~p.", [ Window ] ),
	Res = wxWindow:show( Window ),
	%get_main_loop_pid() ! { onShown, [ [ Window ] ] },
	Res.


% (helper)
show_helper( _Windows=[], Acc ) ->
	Acc;

show_helper( _Windows=[ W | T ], Acc ) ->
	NewAcc = wxWindow:show( W ) orelse Acc,
	show_helper( T, NewAcc ).



% @doc Hides the specified window.
%
% Returns whether anything had to be done.
%
-spec hide( window() ) -> boolean().
hide( Window ) ->
	wxWindow:show( Window, [ { show, false } ] ).



% @doc Returns the size (as {Width,Height}) of the specified window or bitmap.
-spec get_size( window() | bitmap() ) -> dimensions().
get_size( _Canvas={ myriad_object_ref, canvas, CanvasId } ) ->

	%trace_utils:debug_fmt( "Getting size of canvas #~B.", [ CanvasId ] ),

	get_main_loop_pid() ! { getCanvasSize, CanvasId, self() },
	receive

		{ notifyCanvasSize, Size } ->
			Size

	end;

get_size( Bitmap={ wx_ref, _Id, wxBitmap, _List } ) ->
	{ wxBitmap:getWidth( Bitmap ), wxBitmap:getHeight( Bitmap ) };

get_size( Window ) ->
	wxWindow:getSize( Window ).



% @doc Returns the client size (as {Width,Height}) of the specified window,
% i.e. the actual size of the area that can be drawn upon (excluded menu, bars,
% etc.)
%
-spec get_client_size( window() ) -> dimensions().
get_client_size( _Canvas={ myriad_object_ref, canvas, CanvasId } ) ->

	get_main_loop_pid() ! { getCanvasClientSize, CanvasId, self() },
	receive

		{ notifyCanvasClientSize, Size } ->
			Size

	end;

get_client_size( Window ) ->
	wxWindow:getClientSize( Window ).



% @doc Maximises the specified widget (a specialised window; ex: a panel) in its
% parent (adopting its maximum client size), and returns the new size of this
% widget.
%
-spec maximise_in_parent( window() ) -> dimensions().
maximise_in_parent( Widget ) ->
	ParentWindow = wxWindow:getParent( Widget ),
	ParentWindowClientSize = wxWindow:getClientSize( ParentWindow ),
	wxWindow:setSize( Widget, ParentWindowClientSize ),
	ParentWindowClientSize.



% @doc Synchronises the specified window, to ensure that no past operation is
% still pending at its level.
%
% Useful if there exists some means of interacting with it directly (ex: thanks
% to an OpenGL NIF) that could create a race condition (ex: presumably
% message-based resizing immediately followed by a direct OpenGL rendering).
%
% See gui_opengl_minimal_test:on_main_frame_resize/1 for further details.
%
-spec sync( window() ) -> dimensions().
sync( Window ) ->
	% The result in itself may be of no use, the point here is just, through a
	% synchronous operation (a request), to ensure that the specified window is
	% "ready" (that it has processed its previous messages) with a sufficient
	% probability (with certainty if past operations were triggered by the same
	% process as this calling one):
	%
	wxWindow:getSize( Window ).



% @doc Updates the specified window-like object (ex: a canvas) so that its
% client area can be painted.
%
% To be called from a repaint event handler.
%
% See [https://www.erlang.org/doc/man/wxpaintdc#description] for more details.
%
-spec enable_repaint( window() ) -> void().
enable_repaint( Window ) ->
	DC= wxPaintDC:new( Window ),
	wxPaintDC:destroy( DC ).



% @doc Locks the specified window, so that direct access to its content can be
% done, through the returned device context.
%
% Once the desired changes will have been made, this window must be unlocked.
%
-spec lock_window( window() ) -> device_context().
lock_window( Window ) ->
	gui_image:lock_window( Window ).



% @doc Unlocks the specified window, based on the specified device context
% obtained from a previous locking.
%
-spec unlock_window( device_context() ) -> void().
unlock_window( DC ) ->
	gui_image:unlock_window( DC ).



% @doc Destructs the specified window.
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
% Source: http://docs.wxwidgets.org/stable/classwx_frame.html


% @doc Creates a frame, with default title, ID, parent, position, size and
% style.
%
% Note: this version apparently does not correctly initialise the frame;
% following error is indeed reported:
% "wxWidgets Assert failure: ./src/gtk/toplevel.cpp(988): \"m_widget\" in Show()
% : invalid frame".
%
-spec create_frame() -> frame().
create_frame() ->
	wxFrame:new().


% @doc Creates a frame, with default position, size, style, ID and parent.
-spec create_frame( title() ) -> frame().
create_frame( Title ) ->
	wxFrame:new( to_wx_parent( undefined ), to_wx_id( undefined ), Title ).



% @doc Creates a frame, with specified size, and default ID and parent.
-spec create_frame( title(), size() ) -> frame().
create_frame( Title, Size ) ->

	Options = [ to_wx_size( Size ) ],

	%trace_utils:debug_fmt( "create_frame options: ~p.", [ Options ] ),

	wxFrame:new( to_wx_parent( undefined ), to_wx_id( undefined ), Title,
				 Options ).



% @doc Creates a frame, with default position, size and style.
%
% (internal use only)
%
-spec create_frame( title(), wx_id(), window() ) -> frame().
create_frame( Title, Id, Parent ) ->
	wxFrame:new( to_wx_parent( Parent ), to_wx_id( Id ), Title ).


% @doc Creates a frame, with default parent.
-spec create_frame( title(), position(), size(), frame_style() ) -> frame().
create_frame( Title, Position, Size, Style ) ->

	Options =  [ to_wx_position( Position ), to_wx_size( Size ),
				 { style, frame_style_to_bitmask( Style ) } ],

	%trace_utils:debug_fmt( "create_frame options: ~p.", [ Options ] ),

	wxFrame:new( to_wx_parent( undefined ), to_wx_id( undefined ), Title,
				 Options ).



% @doc Creates a frame.
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


% @doc Creates a panel.
-spec create_panel() -> panel().
create_panel() ->
	wxPanel:new().



% @doc Creates a panel, associated to specified parent.
-spec create_panel( window() ) -> panel().
create_panel( Parent ) ->
	wxPanel:new( Parent ).



% @doc Creates a panel, associated to specified parent and with specified
% options.
%
-spec create_panel( window(), panel_options() ) -> panel().
create_panel( Parent, Options ) ->

	ActualOptions = get_panel_options( Options ),

	wxPanel:new( Parent, ActualOptions ).



% @doc Creates a panel, associated to specified parent and with specified
% position and dimensions.
%
-spec create_panel( window(), coordinate(), coordinate(), length(),
					length() ) -> panel().
create_panel( Parent, X, Y, Width, Height ) ->
	create_panel( Parent, _Pos={ X, Y }, _Size={ Width, Height } ).



% @doc Creates a panel, associated to specified parent and with specified
% position and dimensions.
%
-spec create_panel( window(), position(), size() ) -> panel().
create_panel( Parent, Position, Size ) ->
	wxPanel:new( Parent, _Opts=[ { pos, to_wx_position( Position ) },
								 { size, to_wx_size( Size ) } ] ).


% @doc Creates a panel, associated to specified parent and with specified
% position and dimensions.
%
-spec create_panel( window(), position(), size(), panel_options() ) -> panel().
create_panel( Parent, Position, Size, Options ) ->

	FullOptions = get_panel_options( Options )
		++ [ to_wx_position( Position ), to_wx_size( Size ) ],

	%trace_utils:debug_fmt( "Creating panel: parent: ~w, position: ~w, "
	%    "size: ~w, options: ~w, full options: ~w.",
	%    [ Parent, Position, Size, Options, FullOptions ] ),

	wxPanel:new( Parent, FullOptions ).



% @doc Creates a panel, associated to specified parent, with specified
% position, dimensions and options.
%
-spec create_panel( window(), coordinate(), coordinate(), width(), height(),
					panel_options() ) -> panel().
create_panel( Parent, X, Y, Width, Height, Options ) ->

	ActualOptions = get_panel_options( Options ),

	wxPanel:new( Parent, X, Y, Width, Height, ActualOptions ).




% Button section.



% @doc Creates a (labelled) button, with parent specified.
-spec create_button( label(), window() ) -> button().
create_button( Label, Parent ) ->

	Id = ?wxID_ANY,

	Options = [ { label, Label } ],

	%trace_utils:info_fmt( "Button options (for any ID): ~p.",
	%                      [ Id, Options ] ),

	wxButton:new( Parent, Id, Options ).



% @doc Creates (labelled) buttons, with their (single, common) parent specified.
-spec create_buttons( [ label() ], window() ) -> [ button() ].
create_buttons( Labels, Parent ) ->
	create_buttons_helper( Labels, Parent, _Acc=[] ).


% (helper)
create_buttons_helper( _Labels=[], _Parent, Acc ) ->
	lists:reverse( Acc );

create_buttons_helper( [ Label | T ], Parent, Acc ) ->
	NewButton = create_button( Label, Parent ),
	create_buttons_helper( T, Parent, [ NewButton | Acc ] ).




% @doc Creates a button, with parent and most settings specified.
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


% @doc Creates a sizer operating on specified orientation.
-spec create_sizer( orientation() ) -> sizer().
create_sizer( Orientation ) ->
	ActualOrientation = to_wx_orientation( Orientation ),
	wxBoxSizer:new( ActualOrientation ).



% @doc Creates a sizer operating on specified orientation, within specified
% parent, with a box drawn around.
%
-spec create_sizer_with_box( orientation(), window() ) -> sizer().
create_sizer_with_box( Orientation, Parent ) ->

	ActualOrientation = to_wx_orientation( Orientation ),

	wxStaticBoxSizer:new( ActualOrientation, Parent ).



% @doc Creates a sizer operating on specified orientation, within specified
% parent, with a box drawn around bearing specified label.
%
-spec create_sizer_with_labelled_box( orientation(), window(), label() ) ->
											sizer().
create_sizer_with_labelled_box( Orientation, Parent, Label ) ->

	ActualOrientation = to_wx_orientation( Orientation ),

	wxStaticBoxSizer:new( ActualOrientation, Parent, [ { label, Label } ] ).


% @doc Creates a sizer operating on specified orientation, within specified
% parent, with a box drawn around bearing specified label.
%
-spec create_sizer_with_labelled_box( orientation(), window(),
			format_string(), format_values() ) -> sizer().
create_sizer_with_labelled_box( Orientation, Parent, FormatString,
								FormatValues ) ->
	Label = text_utils:format( FormatString, FormatValues ),
	create_sizer_with_labelled_box( Orientation, Parent, Label ).


% @doc Adds specified element, or elements with options, to the specified sizer.
-spec add_to_sizer( sizer(), sizer_child() ) -> sizer_item();
				  ( sizer(), [ { sizer_child(), sizer_options() } ] ) -> void().
add_to_sizer( Sizer, _Element={ myriad_object_ref, canvas, CanvasId } ) ->
	get_main_loop_pid() ! { getPanelForCanvas, CanvasId, self() },
	%io:format( "SEND"),
	receive

		{ notifyCanvasPanel, AssociatedPanel } ->
			%io:format( "RECEIVED"),
			add_to_sizer( Sizer, AssociatedPanel )

	end;

% List version:
add_to_sizer( _Sizer, _Elements=[] ) ->
	ok;

add_to_sizer( Sizer, _Elements=[ { Elem, Opts } | T ] ) ->
	add_to_sizer( Sizer, Elem, Opts ),
	add_to_sizer( Sizer, T );

add_to_sizer( Sizer, Element ) ->
	%trace_utils:debug_fmt( "Adding ~w to sizer ~w.", [ Element, Sizer ] ),
	wxSizer:add( Sizer, Element ).



% @doc Adds specified element (or elements), with (common) options, to the
% specified sizer.
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




% @doc Clears specified sizer, detaching and deleting all its child windows.
-spec clear_sizer( sizer() ) -> void().
clear_sizer( Sizer ) ->
	clear_sizer( Sizer, _DeleteWindows=true ).



% @doc Clears specified sizer, detaching all its child windows, and deleting
% them iff requested.
%
-spec clear_sizer( sizer(), boolean() ) -> void().
clear_sizer( Sizer, DeleteWindows ) ->
	wxSizer:clear( Sizer, [ { delete_windows, DeleteWindows } ] ).



% Status bar section.
%
% Status bars are very useful to trace events.


% @doc Creates and attaches a status bar to the specified frame.
-spec create_status_bar( frame() ) -> status_bar().
create_status_bar( Frame ) ->
	% No interesting option:
	wxFrame:createStatusBar( Frame ).



% @doc Pushes the specified text in the specified status bar.
-spec push_status_text( text(), status_bar() ) -> void().
push_status_text( Text, StatusBar ) ->
	wxStatusBar:pushStatusText( StatusBar, Text ).


% @doc Pushes the specified text in the specified status bar.
-spec push_status_text( format_string(), format_values(), status_bar() ) ->
											void().
push_status_text( FormatString, FormatValues, StatusBar ) ->
	Text = text_utils:format( FormatString, FormatValues ),
	push_status_text( Text, StatusBar ).



% Brush section.


% @doc Returns a brush of the specified color.
-spec create_brush( color_by_decimal() ) -> brush().
create_brush( RGBColor ) ->
	wxBrush:new( RGBColor ).


% @doc Tells that the specified (reference-counted) brush may be deleted.
-spec destruct_brush( brush() ) -> void().
destruct_brush( Brush ) ->
	wxBrush:destroy( Brush ).



% Bitmap section.


% @doc Returns a bitmap created from the specified image path.
-spec create_bitmap( any_file_path() ) -> bitmap().
create_bitmap( ImagePath ) ->
	gui_image:create_bitmap( ImagePath ).



% @doc Returns a blank bitmap of the specified size.
-spec create_blank_bitmap( dimensions() ) -> bitmap().
create_blank_bitmap( Dimensions ) ->
	gui_image:create_blank_bitmap( Dimensions ).



% @doc Returns a blank bitmap of the specified size.
-spec create_blank_bitmap( width(), height() ) -> bitmap().
create_blank_bitmap( Width, Height ) ->
	gui_image:create_blank_bitmap( Width, Height ).



% @doc Returns a blank bitmap whose size is the client one of the specified
% window.
%
-spec create_blank_bitmap_for( window() ) -> bitmap().
create_blank_bitmap_for( Window ) ->
	ClientSize = wxWindow:getClientSize( Window ),
	create_blank_bitmap( ClientSize ).



% @doc Locks the specified bitmap, so that direct access to its content can be
% done, through the returned device context.
%
% Once the desired changes will have been made, this bitmap must be unlocked.
%
-spec lock_bitmap( bitmap() ) -> device_context().
lock_bitmap( Bitmap ) ->
	gui_image:lock_bitmap( Bitmap ).



% @doc Draws the specified bitmap in the specified device context, at the
% specified position.
%
-spec draw_bitmap( bitmap(), device_context(), point() ) -> void().
draw_bitmap( SourceBitmap, TargetDC, PosInTarget ) ->
	gui_image:draw_bitmap( SourceBitmap, TargetDC, PosInTarget ).



% @doc Unlocks the specified bitmap, based on the specified device context
% obtained from a previous locking.
%
-spec unlock_bitmap( device_context() ) -> void().
unlock_bitmap( DC ) ->
	gui_image:unlock_bitmap( DC ).



% @doc Destructs the specified bitmap (which must not be locked).
-spec destruct_bitmap( bitmap() ) -> void().
destruct_bitmap( Bitmap ) ->
	gui_image:destruct_bitmap( Bitmap ).



% Device context section.


% @doc Clears the specified device context, using the current background brush.
% If none was set, a solid white brush is used.
%
-spec clear_device_context( device_context() ) -> void().
clear_device_context( DC ) ->
	gui_image:clear_device_context( DC ).



% @doc Blits (copies) the specified area of the source device context at the
% specified position in the target device context.
%
% Returns a boolean of unspecified meaning.
%
-spec blit( device_context(), point(), dimensions() , device_context(),
			point() ) -> boolean().
blit( SourceDC, SrcTopLeft, Size, TargetDC, TgtTopLeft ) ->
	gui_image:blit( SourceDC, SrcTopLeft, Size, TargetDC, TgtTopLeft ).



% @doc Blits (copies) the specified area of the source device context at the
% specified position in the target device context.
%
% Returns a boolean of unspecified meaning.
%
-spec blit( device_context(), point(), width(), height(), device_context(),
			point() ) -> boolean().
blit( SourceDC, SrcTopLeft, Width, Height, TargetDC, TgtTopLeft ) ->
	gui_image:blit( SourceDC, SrcTopLeft, Width, Height, TargetDC, TgtTopLeft ).




% Font section.


% @doc Creates a corresponding font object, to determine the appearance of
% rendered text.
%
-spec create_font( font_size() | point_size(), font_family(), font_style(),
				   font_weight() ) -> font().
create_font( FontSize, FontFamily, FontStyle, FontWeight ) ->
	gui_font:create( FontSize, FontFamily, FontStyle, FontWeight ).


% @doc Creates a corresponding font object, to determine the appearance of
% rendered text.
%
-spec create_font( font_size() | point_size(), font_family(), font_style(),
				   font_weight(), [ font_option() ] ) -> font().
create_font( FontSize, FontFamily, FontStyle, FontWeight, FontOpts ) ->
	gui_font:create( FontSize, FontFamily, FontStyle, FontWeight, FontOpts ).



% General MyriadGUI helpers.


% @doc Requests the creation of the specified instance (to be done from the
% MyriadGUI main loop), and returns the corresponding GUI object reference.
%
-spec execute_instance_creation( myriad_object_type(),
						construction_parameters() ) -> myriad_object_ref().
execute_instance_creation( ObjectType, ConstructionParams ) ->

	cond_utils:if_defined( myriad_debug_gui_instances,
		trace_utils:debug_fmt( "Requesting the creation of a '~ts' instance, "
			"based on following construction parameters:~n~w.",
			[ ObjectType, ConstructionParams ] ) ),

	LoopPid = get_main_loop_pid(),

	LoopPid ! { createInstance, [ ObjectType, ConstructionParams ], self() },

	receive

		% Match on the object type:
		{ instance_created, ObjectType, ObjectRef } ->

			cond_utils:if_defined( myriad_debug_gui_instances,
				trace_utils:debug_fmt( "'~ts' instance created, now referenced "
									   "as ~w.", [ ObjectType, ObjectRef ] ) ),

			ObjectRef

	end.



% @doc Fetches (from the MyriadGUI environment) the PID of the process in charge
% of running the main GUI loop.
%
-spec get_main_loop_pid() -> pid().
get_main_loop_pid() ->

	GUIEnv = get_gui_env(),

	GUIEnv#gui_env.loop_pid.



% @doc Fetches (from the process dictionary) the MyriadGUI environment.
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



% @doc Returns a textual representation of the specified GUI object.
-spec object_to_string( gui_object() ) -> ustring().
object_to_string( #myriad_object_ref{ object_type=ObjectType,
									  myriad_instance_id=InstanceId } ) ->
	text_utils:format( "~ts-~B", [ ObjectType, InstanceId ] );

object_to_string( { wx_ref, InstanceRef, WxObjectType, _State=[] } ) ->
	% Ex: {wx_ref,35,wxFrame,[]}
	ObjectType = gui_wx_backend:from_wx_object_type( WxObjectType ),
	text_utils:format( "~ts-~B", [ ObjectType, InstanceRef ] );

object_to_string( { wx_ref, InstanceRef, WxObjectType, State } ) ->
	ObjectType = gui_wx_backend:from_wx_object_type( WxObjectType ),
	text_utils:format( "~ts-~B whose state is ~p",
					   [ ObjectType, InstanceRef, State ] ).



% @doc Returns a textual representation of the specified GUI event context.
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
