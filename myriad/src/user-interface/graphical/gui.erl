% Copyright (C) 2010-2023 Olivier Boudeville
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
% the best set of gui backends available (previously: gs alone; now: wx, with
% OpenGL), for classical applications and multimedia ones (ex: games).
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
% - the gui_canvas module for all canvas-related operations

% The gui module uses its own environment server to record its defaults and also
% elements about its current state.
%
% In general, the opaqueness of types is too difficult to preserve here.


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



% Environment-related storage of the current state of the GUI (merely
% references):
%
-define( gui_env_entries, [

	% GUI-level entries:

	% The family of the current operating system, typically to adapt to OS
	% GUI specificities:
	%
	{ 'os_family', os_family() },

	% A more precise name of the current operating system, for finer control:
	{ 'os_name', os_name() },


	% The PID of the allocator of unique backend identifiers:
	{ 'id_allocator_pid', id_allocator_pid() },

	% The main, top-level window (if any; generally a frame) of the application:
	{ 'top_level_window', maybe( window() ) },

	% PID of the MyriadGUI main event loop:
	{ 'loop_pid', pid() },

	% The event types that are trapped by default:
	{ 'trap_set', trap_set() },

	% A bijective table to translate wx and MyriadGUI event types back and
	% forth:
	%
	{ 'event_translation_table', event_translation_table() },

	% Any backend-specific top-level server used for the GUI (here wx):
	{ 'backend_server', wx_object() },

	% Any backend-specific environment term used for the GUI (here wx):
	{ 'backend_env', wx_environment() },


	% OpenGL-related entries:

	% The current OpenGL canvas (if any):
	{ 'gl_canvas', maybe( opengl_canvas() ) },

	% The current OpenGL context (if any):
	{ 'gl_context', maybe( opengl_context() ) },


	% Mouse-related entries:

	% A table keeping track of the various mouse cursors available:
	{ 'cursor_table', gui_mouse:cursor_table() },

	% The current type of cursor (if any):
	{ 'current_cursor_type', maybe( gui_mouse:cursor_type() ) },

	% The stack (as a list) of the windows that grabbed the mouse cursor:
	{ 'grab_stack', [ window() ] },

	% Tells whether we are in key-released event-handling mode:
	{ 'key_released', boolean() },

	% The coordinates at which the mouse cursor shall warp:
	{ 'warp_coordinates', maybe( point() ) },


	% Window manager related entries:

	% The currently active window (if any), i.e. the one handling current
	% events:
	%
	{ 'active_window', maybe( window_name() ) },

	% The window (if any) currently having the focus (implicitly or because
	% having grabbed the mouse):
	%
	{ 'focused_window', maybe( window_name() ) } ] ).
% These keys, associated to values of the associated types, are used (and
% reserved) by MyriadGUI in order to record application-level information, made
% available to its processes through its environment server.
%
% At least a subset of these entries may be cached from the environment, for
% easier/faster lookups and updates.


-type service() :: 'mouse'.
% The various MyriadGUI services that may or may not be enabled.


-type gui_env_pid() :: environment:env_pid().

-type gui_env_info() :: environment:env_info().


-type gui_env_designator() :: environment:env_designator().

-type backend_identifier() :: 'gs' % Now obsolete
							| 'wx' % Based on wxWidgets
							| atom().
% Identifier of a graphical backend.


-type backend_information() ::
		{ backend_identifier(), basic_utils:any_version() }.
% Information regarding a graphical backend.


% Current backend is wx (based on WxWidgets).
%
% (useful to avoid including the header of wx in our own public ones)
%
-type backend_event() :: gui_event:wx_event().
% A (supposedly opaque) backend GUI event.


-opaque backend_environment() :: wx_environment().
% An (opaque) environment used by a GUI backend.


-type wx_environment() :: term().
% An (opaque) wx process environment.


% With wx, device contexts (ex: obtained from wxMemoryDC:new/1) must be
% explicitly managed (ex: wxMemoryDC:destroy/1 must be called when finished with
% them), which is inconvenient and error-prone.
%
-type device_context() :: wxDC:wxDC().
% Designates an abstract device where rendering can take place, and which can be
% the source or target of a blit. Akin to a surface in SDL (libsdl).

-type paint_device_context() :: wxPaintDC:wxPaintDC().
% Designates a device context allowing to paint on the client area of a window
% from within an onRepaintNeeded event handler.

-type memory_device_context() :: wxMemoryDC:wxMemoryDC().
% Provides a means of drawing graphics onto a bitmap.


-type window_device_context() :: wxWindowDC:wxWindowDC().
% Allows to paint on the whole area of a window (client and decorations). This
% should normally be constructed as a temporary stack object, and shall never be
% stored.
%
% To draw on a window from inside an onRepaintNeeded event handler, create a
% paint_device_context() instance instead.



-type any_window_device_context() :: window_device_context()
								   | window()
								   | memory_device_context()
								   | image().
% Any window-related device context.


-type graphic_context() :: wxGraphicsContext:wxGraphicsContext().
% Corresponds to a GUI object that is drawn upon. It is created by a renderer.


-type canvas() :: gui_canvas:canvas().
% A basic canvas (not to be mixed with an OpenGL one, opengl_canvas/0).


-type opengl_canvas() :: gui_opengl:gl_canvas().
% An OpenGL canvas (not to be mixed with a basic one, canvas/0).

-type opengl_context() :: gui_opengl:gl_context().
% An OpenGL context.



% Basic GUI operations.
-export([ is_available/0, get_backend_information/0,
		  start/0, start/1, set_debug_level/1, stop/0 ]).


% Extra overall operations.
-export([ batch/1, get_environment_server/0 ]).


% Event-related operations.
-export([ subscribe_to_events/1, subscribe_to_events/2,
		  unsubscribe_from_events/1, unsubscribe_from_events/2,
		  register_event_callback/3, register_event_callback/4,
		  trap_event/1, propagate_event/1 ]).



% Stringification section.
%
% (mostly internal purpose)
%
-export([ object_to_string/1, object_key_to_string/1, context_to_string/1 ]).



% Widget-related section.


% General-purpose:
-export([ set_tooltip/2, set_as_controller/1, set_controller/2 ]).


% Miscellaneous:
-export([ get_backend_environment/0, set_backend_environment/1 ]).


% Windows (see also the gui_window_manager module regarding the insertion of
% windows in their environment), the base, most generic kind of widget.
%
-export([ create_window/0, create_window/1, create_window/2, create_window/5,
		  set_foreground_color/2, set_background_color/2,
		  set_font/2, set_font/3,
		  set_sizer/2, add_stretch_spacer/1, layout/1, fit_to_sizer/2,
		  create_splitter/4, create_splitter/5, set_unique_pane/2,
		  show/1, hide/1, is_maximised/1, maximize/1, set_title/2,
		  get_focused/0, set_focus/1,
		  get_size/1, get_client_size/1,
		  get_best_size/1, set_client_size/2, fit/1,
		  maximise_in_parent/1, sync/1, enable_repaint/1,
		  lock_window/1, unlock_window/1, destruct_window/1 ]).


% Frames:
%
% A frame is a window whose size and position can usually be changed by the
% user.
%
% Note that a frame is a top_level_window(), a window() and an event_handler(),
% and thus can use their methods.
%
-export([ create_top_level_frame/1, create_top_level_frame/2,
		  create_top_level_frame/4,
		  create_frame/0, create_frame/1, create_frame/2, create_frame/3,
		  create_frame/4, create_frame/6,
		  set_icon/2, create_toolbar/1, create_toolbar/3, set_toolbar/2,
		  destruct_frame/1 ]).


% Panels:
-export([ create_panel/0, create_panel/1, create_panel/2, create_panel/4,
		  create_panel/5, create_panel/6, destruct_panel/1 ]).


% Buttons:
-export([ create_button/2, create_button/6, create_buttons/2 ]).


% Sizers:
-export([ create_sizer/1, create_sizer_with_box/2,
		  create_sizer_with_labelled_box/3, create_sizer_with_labelled_box/4,
		  add_to_sizer/2, add_to_sizer/3, add_spacer_to_sizer/4,
		  clear_sizer/1, clear_sizer/2 ]).



% Status bars:
-export([ create_status_bar/1, create_status_bar/2,
		  destruct_status_bar/1,
		  set_field_count/2, set_field_widths/2,
		  push_status_text/2, push_status_text/3,
		  push_field_status_text/3, push_field_status_text/4 ]).


% Toolbars:
%
% Notable emitted events: command_event_type(), that is onToolbarEntered,
% onItemSelected, onToolRightClicked.
%
% Note that wx name them 'wxToolBar', not 'wxToolbar'.
%
-export([ add_control/2, add_tool/5, add_tool/7, update_tools/1 ]).


% Brushes:
-export([ create_brush/1, create_transparent_brush/1, destruct_brush/1 ]).


% Canvas support (forwarded to gui_canvas).
-export([ create_canvas/1, set_draw_color/2, set_fill_color/2,
		  get_rgb/2, set_rgb/2,
		  draw_line/3, draw_line/4, draw_lines/2, draw_lines/3,
		  draw_segment/4, draw_polygon/2,
		  draw_label/3,
		  draw_cross/2, draw_cross/3, draw_cross/4, draw_labelled_cross/4,
		  draw_labelled_cross/5, draw_circle/3, draw_circle/4,
		  draw_numbered_points/2,
		  load_image/2, load_image/3,
		  resize/2, blit/1, clear/1 ]).



% Bitmaps correspond to the native image format that is the quickest/easiest to
% operate with - as opposed to images, which are generic elements, independent
% from platform and image file format (they are just buffers of RGB bytes, with
% an optional buffer for the alpha bytes).
%
-export([ create_bitmap/1, create_blank_bitmap/1, create_blank_bitmap/2,
		  create_blank_bitmap_for/1,

		  create_bitmap_display/2, create_bitmap_display/3,
		  destruct_bitmap_display/1,

		  create_text_display/2, create_text_display/3,
		  destruct_text_display/1,

		  lock_bitmap/1, draw_bitmap/3, unlock_bitmap/1, destruct_bitmap/1 ]).



% Device contexts:
-export([ get_flickerfree_paint_device_context/1,
		  get_flickerfree_paint_device_context/2,
		  destruct_paint_device_context/1,
		  clear_device_context/1, blit/5, blit/6 ]).


% Graphic contexts:
-export([ create_graphic_context/1, % already exported: set_font/3,
		  set_brush/2,
		  set_font/4,
		  get_text_extent/2, get_text_width/2, get_text_height/2,
		  get_text_dimensions/2,
		  draw_text/4,
		  draw_bitmap/4, draw_bitmap/5, draw_bitmap/6 ]).


% Menus:
-export([ create_menu/0, create_menu/1, create_menu/2,
		  destruct_menu/1, get_standard_item_names/0,
		  add_item/2, add_item/3, append_item/2,
		  append_submenu/4, append_submenu/5,
		  add_checkable_item/2, add_checkable_item/3, add_checkable_item/4,
		  set_checkable_menu_item/3,
		  add_radio_item/3, add_radio_item/4,
		  add_separator/1,
		  set_menu_item_status/3, remove_menu_item/2,
		  create_menu_bar/0, add_menu/3, set_menu_bar/2,
		  activate_popup_menu/2 ]).


% Input support:

% Keyboard:
%-export([ getKey/0 ]).



% Fonts:
-export([ create_font/1, create_font/2, create_font/3, create_font/4,
		  create_font/5, destruct_font/1 ]).



% Internal, silencing exports:
-export([ create_gui_environment/1,
		  destruct_gui_environment/0, destruct_gui_environment/1,
		  event_interception_callback/3,
		  resolve_id/1 ]).


% For related, public defines:
-include("gui.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").


% For myriad_spawn*:
-include("spawn_utils.hrl").



% Type declarations:

-type length() :: linear:integer_distance().
% A length, as an integer number of pixels.

-type width() :: length().
% A width, as an integer number of pixels.

-type height() :: length().
% An height, as an integer number of pixels.


-type any_length() :: linear:any_distance().
% A length, as a number (integer or floating-point) of pixels.

-type any_width() :: any_length().
% A width, as a number (integer or floating-point) of pixels.

-type any_height() :: any_length().
% An height, as a number (integer or floating-point) of pixels.


-type coordinate() :: linear:integer_coordinate().
% For a GUI, coordinates are an integer number of pixels.


-type point() :: point2:integer_point2().
% A pixel-wise (tuple-based) GUI point (as point2:point2() would allow for
% floating-point coordinates).

-type position() :: point() | 'auto'.
% Position, in pixel coordinates, typically of a widget.


-type dimensions() :: linear_2D:integer_rect_dimensions().
% Dimensions in pixels, as {IntegerWidth,IntegerHeight}.

-type size() :: dimensions() | 'auto'.
% Size, typically of a widget.


-type orientation() :: 'vertical' | 'horizontal'.
% A vertical orientation means piling elements top to bottom for example, while
% an horizontal one means left to right, for example.


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


-type myriad_object_type() :: 'myr_canvas'.
% The additional widget types introduced by Myriad.


% Also: | all other *_state() that may be introduced
-type myriad_object_state() :: gui_canvas:canvas_state().
% Records the actual state of a MyriadGUI object.


-type construction_parameters() :: [ term() ].
% The construction parameters of a MyriadGUI object.


-type gui_object() :: wx_object() | myriad_object_ref().
% Reference to a GUI object (often designated as "widget" here), somewhat akin
% to a PID.
%
% (ex: {wx_ref,35,wxFrame,[]} or {myriad_object_ref,myr_canvas,12}).


-type wx_server() :: gui_object().
% Alias to designate more clearly the wx server.



% Defining the actual widget types corresponding to wx_object_type():


-type widget() :: window().
% Any kind of widget (graphical component).
%
% Clearer naming than window, as not necessarily directly akin to a regular
% window.


% Includes wx:null(), i.e. a #wx_ref{ref=0, type=wx}.
-type window() :: maybe( wxWindow:wxWindow() | gui_canvas:canvas() ).
% Any kind of window, that is widget (ex: any canvas is a window).



-type top_level_window() :: window().
% A top-level (application-wide) window.


-type splitter_window() :: wxSplitterWindow:wxSplitterWindow().
% A window able to be split into two panes.


-opaque frame() :: wxFrame:wxFrame().

-type top_level_frame() :: frame().

-type panel() :: wxPanel:wxPanel().

-type button() :: wxButton:wxButton().


-type sizer() :: wxSizer:wxSizer().
% A vertical or horizontal container whose elements are dynamically resized.

-type sizer_child() :: window() | sizer().
% Elements that can be included in a sizer.


-type sizer_item() :: wxSizerItem:wxSizerItem().
% An element of a sizer.


-type splitter() :: #splitter{}.
% Information regarding the (fixed, static) horizonta or vertical splitting of a
% window into two ones.


-type sash_gravity() :: number().
% Tells how much the first pane of a splitter window is to grow while resizing
% it:
%  - 0.0: only the bottom/right window is automatically resized
%  - 0.5: both windows grow by equal size
%  - 1.0: only left/top window grows
%
% Gravity should be a value between 0.0 and 1.0; its default value is 0.0.


-opaque status_bar() :: wxStatusBar:wxStatusBar().
% A thin space at the bottom of a frame where texts are typically displayed.

-type status_bar_style() :: 'normal'
						  | 'flat'
						  | 'raised'
						  | 'sunken'.
% A style of status bar.


-type toolbar() :: wxToolBar:wxToolBar().
% A bar of buttons and/or other controls usually placed below the menu bar of a
% frame.
%
% A toolbar emits menu commands in the same way that a frame menubar does.


-type toolbar_style() :: 'top' | 'bottom' | 'left' | 'right'
					   | 'flat'
					   | 'dockable'
					   | 'no_icons'
					   | 'text'
					   | 'no_divider'
					   | 'no_align'
					   | 'horizontal_layout'
					   | 'no_tooltips'
					   | 'default'.
% A style of toolbar.


-type tool() :: wx_object().
% A tool, as an element of a toolbar.


% Corresponds to menu items:
-type tool_kind() :: menu_item_kind().
% The kind of a tool in a toolbar.


-type control() :: wxControl:wxControl().
% A control is generally a small window which processes user input and/or
% displays one or more item of data.


-type field_count() :: count().
% A number of fields in a status bar.

-type field_index() :: positive_index().
% The index (1 or more) of a field in a status bar.

-type field_width() :: integer().
% The width of a field in a status bar.
%
% If positive, designates a fixed width, in pixels.
% If negative, designates a width ratio among all variable-width fields.

-type bitmap() :: gui_image:bitmap().
% Platform-dependent bitmap, either monochrome or colour (with or without alpha
% channel).
%
% Intended to be a wrapper of whatever is the native image format, which is
% quickest/easiest to draw to a display context.


-opaque bitmap_display() :: gui_image:bitmap_display().
% A widget displaying a bitmap.


-opaque icon() :: gui_image:icon().
% A small rectangular bitmap usually used for denoting a minimised application.


-opaque text_display() :: gui_image:text_display().
% A widget displaying a text.


-type brush() :: wxBrush:wxBrush().
% A brush, used to draw elements.

-type back_buffer() :: wxMemoryDC:wxMemoryDC().
% A non-displayed buffer to which rendering shall be done, before being made
% visible as a whole, to avoid flicker.


-opaque menu() :: wxMenu:wxMenu().
% The definition of a menu, to be assigned to a menu bar or a popup menu.


-type menu_option() :: 'detachable'.
% An option when creating a menu.


-opaque menu_item() :: wxMenuItem:wxMenuItem().
% An entry registered in a menu; possibly a basic item, a submenu or a
% separator.


-type menu_title() :: label().
% The title associated to a menu element.
%
% Use ampersand to denote a shortcut character, e.g. `"&File"' or `"&Print
% code"'.


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
-type standard_menu_item_name_id() :: 'new_menu_item'
									| 'open_menu_item'
									| 'close_menu_item'
									| 'save_menu_item'
									| 'save_as_menu_item'
									| 'revert_to_saved_menu_item'
									| 'undelete_menu_item'
									| 'print_menu_item'
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
									| 'exit_menu_item'.
% The name identifiers of the standard menu items.
%
% Such standard items are specifically managed (ex: they have a corresponding
% icon automatically associated).


-type menu_item_id() :: standard_menu_item_name_id() | id().
% The identifier of a menu item, possibly having a specific, named (standard or
% not) meaning and graphical representation.
%
% The atoms listed here are reserved name identifiers, as 'undefined' is.


-opaque menu_bar() :: wxMenuBar:wxMenuBar().
% A menu bar is a series of menus in a row, accessible from the top of a frame.

% Not existing? -type menu_bar_option() :: ''.


-type standard_icon_name_id() ::
		'asterisk_icon' | 'stop_icon' | 'information_icon'
	  | 'question_icon' | 'error_icon' | 'warning_icon' | 'hand_icon'
	  | 'exclamation_icon'.
% The name identifiers of the standard icons.


-type icon_name_id() :: standard_icon_name_id() | id().


-type standard_bitmap_name_id() ::
	  'error_bitmap' | 'question_bitmap' | 'warning_bitmap'
	| 'information_bitmap' | 'add_bookmark_bitmap'
	| 'delete_bookmark_bitmap' | 'help_side_panel_bitmap'
	| 'help_settings_bitmap' | 'help_book_bitmap' | 'help_folder_bitmap'
	| 'help_page_bitmap' | 'go_back_bitmap' | 'go_forward_bitmap'
	| 'go_up_bitmap' | 'go_down_bitmap' | 'go_to_parent_bitmap'
	| 'go_home_bitmap' | 'goto_first_bitmap' | 'goto_last_bitmap'
	| 'print_bitmap' | 'help_bitmap' | 'tip_bitmap' | 'report_view_bitmap'
	| 'list_view_bitmap' | 'new_folder_bitmap' | 'folder_bitmap'
	| 'open_folder_bitmap' | 'go_folder_up_bitmap' | 'executable_file_bitmap'
	| 'normal_file_bitmap' | 'tick_mark_bitmap' | 'cross_mark_bitmap'
	| 'missing_image_bitmap' | 'new_bitmap' | 'file_open_bitmap'
	| 'file_save_bitmap' | 'file_save_as_bitmap' | 'file_delete_bitmap'
	| 'copy_bitmap' | 'cut_bitmap' | 'paste_bitmap' | 'undo_bitmap'
	| 'redo_bitmap' | 'plus_bitmap' | 'minus_bitmap' | 'close_bitmap'
	| 'quit_bitmap' | 'find_bitmap' | 'find_and_replace_bitmap'
	| 'full_screen_bitmap' | 'edit_bitmap' | 'hard_disk_bitmap'
	| 'floppy_bitmap' | 'cdrom_bitmap' | 'removable_bitmap'
	| 'backend_logo_bitmap'.
% The name identifiers of the standard bitmaps.


-type bitmap_name_id() :: standard_bitmap_name_id() | id().

-type font() :: gui_font:font().

-type font_size() :: gui_font:font_size().

-type point_size() :: gui_font:point_size().

-type font_family() :: gui_font:font_family().

-type font_style() :: gui_font:font_style().

-type font_weight() :: gui_font:font_weight().

-type font_option() :: gui_font:font_option().

-type title() :: text().
-type label() :: text().
-type help_info() :: text().


-type event_callback() :: gui_event:event_callback().
% Shorthand type.

-type user_data() :: any().
% User data, as specified in an event subscription/callback.


-type window_style_opt() :: 'default_border'
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
						  | 'with_vertical_scrollbar'
						  | 'with_horizontal_scrollbar'
						  | 'never_hide_scrollbars'
						  | 'clip_children'
						  | 'full_repaint_on_resize'.
% Options for windows. See also
% [http://docs.wxwidgets.org/stable/classwx_window.html]


-type window_style() :: window_style_opt() | [ window_style_opt() ].

-type window_option() :: { pos, point() }
					   | { size, size() }
					   | { style, [ window_style_opt() ] }.
% Window-specific options (quite common).

% Unused: -type window_options() :: [ window_option() ].


% 'iconized' not kept (duplicate of 'minimize').
-type frame_style_opt() ::

		% Corresponds to the following options: minimize_icon, maximize_icon,
		% resize_border, system_menu, caption, close_icon and clip_children.
		%
		'default'

		% Displays a caption on the title bar of this frame (needed for icons).
	  | 'caption'

		% Displays a minimize icon on the title bar of this frame.
	  | 'minimize_icon'

		% Displays a maximize icon on the title bar of this frame.
	  | 'maximize_icon'

		% Displays a close icon on the title bar of this frame.
	  | 'close_icon'

		% Stays on top of all other windows.
	  | 'stay_on_top'

		% Displays a system menu containing the list of various windows commands
		% in the window title bar.
		%
	  | 'system_menu'

		% Displays a resizable border around the window.
	  | 'resize_border'

		% This frame will have a small title bar:
	  | 'tool_window'

		% Requests that this frame does not appear in the taskbar:
	  | 'no_taskbar'

		 % Stays on top of (only) its parent:
	  | 'float_on_parent'

		 % Allows this frame to have its shape changed:
	  | 'shaped'.
% Options for frames.
%
% Note that specifying an empty option list does not enable any option.
%
% See also [http://docs.wxwidgets.org/stable/classwx_frame.html].



-type frame_style() :: frame_style_opt() | [ frame_style_opt() ].


-type panel_option() :: window_option().
% Options for panels, see
% [http://docs.wxwidgets.org/stable/classwx_panel.html].


-type panel_options() :: maybe_list( panel_option() ).


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


-type weight_factor() :: non_neg_integer().
% A weight factor akin to a proportion, typically of an element (widget or
% sizer) in a sizer.
%
% 0 means that the corresponding element is fixed-sized (in the direction of the
% sizer), whereas strictly positive values specify the weight (expansion factor)
% of that element relative to the other elements.


% Simplification for the user: no specific flag needed.
-type sizer_option() :: { 'proportion', weight_factor() }
					  | { 'border', integer() }
					  | { 'user_data', term() }
					  | sizer_flag_opt().
% An option when creating a sizer.


-type sizer_options() :: [ sizer_option() ].


-type image() :: gui_image:image().
% An image is a bitmap buffer of RGB bytes with an optional buffer for the alpha
% bytes.
%
% It is thus generic, independent from platforms and image file formats.


-type event_subscription_opt() ::

	{ 'id', id() }

  | { 'last_id', id() }

	% Processes the event but does not propagate it upward in the widget
	% hierarchy afterwards (which is the default for most event types).
	%
	% See trap_event/1 and https://howtos.esperide.org/Erlang.html#using-wx for
	% a clarification of the use of this option.
	%
  | 'trap_event'

	% Processes the event and propagates it upward in the widget hierarchy
	% afterwards (some event types by default trap events).
	%
	% See propagate_event/1 and https://howtos.esperide.org/Erlang.html#using-wx
	% for a clarification of the use of this option.
	%
  | 'propagate_event'

	% Triggers handle_sync_event/3, see the wx_object behaviour:
  | 'callback'

  | { 'callback', event_callback() }
  | { 'user_data', user_data() }.
% Options for the subscription to events.
%
% Note that with MyriadGUI by default most types of events are propagated to
% parent handlers; it allows notably the GUI backend to update the state of the
% widget hierarchy accordingly (otherwise for example the user code subscribing
% to onResized events would prevent the corresponding resizes to be properly
% taken into account by the backend).
%
% However it may result in race conditions for example when shutting the
% application after the receiving of a onWindowClosed message, as the backend
% would be destroying the application GUI resources concurrently to any
% corresponding user-defined event handler; this is why the subscription to some
% event types implies that by default their events are trapped.
%
% Use the 'trap_event' option or the trap_event/1 function to prevent any
% default event propagation to happen, so that the user code is the sole manager
% of such events (ex: of the application termination).
%
% Conversely, use the 'propagate_event' option or the propagate_event/1 function
% to force event propagation despite an event type implying that by default
% these events are trapped.


-type event_subscription_options() ::
		event_subscription_opt() | [ event_subscription_opt() ].


-type debug_level_opt() :: 'none' | 'calls' | 'life_cycle'.
% Mapped internally to 'none', 'verbose', 'trace', etc.; see
% convert_debug_level/1.



-type debug_level() :: debug_level_opt() | [ debug_level_opt() ].

-type error_message() :: term().



-export_type([ service/0,
			   gui_env_pid/0, gui_env_info/0, gui_env_designator/0,
			   backend_identifier/0, backend_information/0,

			   length/0, width/0, height/0, dimensions/0,
			   any_length/0, any_width/0, any_height/0,
			   coordinate/0, point/0, position/0, size/0,
			   orientation/0, fps/0, id/0,

			   model_pid/0, view_pid/0, controller_pid/0,
			   object_type/0, wx_object_type/0,
			   myriad_object_type/0,
			   title/0, label/0, event_callback/0, user_data/0,
			   gui_object/0, wx_server/0,
			   widget/0,
			   window/0, top_level_window/0, splitter_window/0,
			   frame/0, top_level_frame/0,
			   panel/0, button/0,
			   sizer/0, sizer_child/0, sizer_item/0,
			   splitter/0, sash_gravity/0,
			   status_bar/0, status_bar_style/0,

			   toolbar/0, toolbar_style/0, tool/0, tool_kind/0,
			   control/0,

			   menu/0, menu_option/0,
			   menu_item/0, menu_title/0, menu_label/0,
			   menu_item_label/0, menu_item_kind/0,
			   standard_menu_item_name_id/0, menu_item_id/0, menu_bar/0,

			   standard_icon_name_id/0, icon_name_id/0,
			   standard_bitmap_name_id/0, bitmap_name_id/0,

			   font/0, font_size/0, point_size/0, font_family/0, font_style/0,
			   font_weight/0,

			   bitmap/0, bitmap_display/0, icon/0, text_display/0,
			   brush/0, back_buffer/0,

			   device_context/0, paint_device_context/0,
			   memory_device_context/0, window_device_context/0,
			   any_window_device_context/0,

			   graphic_context/0,

			   canvas/0,
			   opengl_canvas/0, opengl_context/0,
			   construction_parameters/0, backend_event/0,
			   event_subscription_options/0,
			   window_style/0, frame_style/0, button_style/0,

			   window_style_opt/0, window_option/0,
			   frame_style_opt/0,
			   panel_option/0, panel_options/0,
			   button_style_opt/0,
			   sizer_flag_opt/0, sizer_option/0, sizer_options/0,
			   image/0,
			   event_subscription_opt/0,
			   debug_level_opt/0, debug_level/0, error_message/0 ]).


% To avoid unused warnings:
-export_type([ myriad_object_state/0, backend_environment/0 ]).


% Function shorthands:
-import( gui_wx_backend, [ to_wx_parent/1, to_wx_position/1,
						   to_wx_size/1, to_wx_orientation/1,
						   frame_style_to_bitmask/1, get_panel_options/1 ]).

-type text() :: ustring().


% Type shorthands:

-type count() :: basic_utils:count().
-type positive_index() :: basic_utils:positive_index().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type os_type() :: system_utils:os_type().
-type os_family() :: system_utils:os_family().
%-type os_name() :: system_utils:os_name().

-type ustring() :: text_utils:ustring().

-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().


-type file_path() :: file_utils:file_path().
-type any_file_path() :: file_utils:any_file_path().

-type line2() :: linear_2D:line2().

-type color() :: gui_color:color().
-type color_by_decimal() :: gui_color:color_by_decimal().
-type color_by_decimal_with_alpha() :: gui_color:color_by_decimal_with_alpha().

-type event_subscription_spec() :: gui_event:event_subscription_spec().
-type event_unsubscription_spec() :: gui_event:event_unsubscription_spec().
-type event_context() :: gui_event:event_context().
-type gui_object_key() :: gui_event:gui_object_key().
-type event_type() :: gui_event:event_type().
-type gui_event_object() :: gui_event:gui_event_object().
-type event_subscriber() :: gui_event:event_subscriber().
-type event_translation_table() :: gui_event:event_translation_table().

-type text_display_option() :: gui_image:text_display_option().

%-type window_name() :: gui_window_manager:window_name().

-type id() :: gui_id:id().
-type name_id() :: gui_id:name_id().

-type backend_id() :: gui_id:backend_id().

-type wx_object() :: wx:wx_object().
% Shorthand for a wx_object(), that is a #wx_ref record.


% GUI-specific defines:

% Key of the MyriadGUI environment, in the process dictionary:
-define( gui_env_process_key, myriad_gui_env ).



% Section for basic GUI overall operations.


% @doc Tells whether this user-interface backend is available.
-spec is_available() -> boolean().
is_available() ->
	% As simple as:
	system_utils:has_graphical_output().


% @doc Returns information regarding the graphical backend in use.
-spec get_backend_information() -> backend_information().
get_backend_information() ->
	{ wx, gui_wx_backend:get_wx_version() }.



% @doc Starts the MyriadGUI subsystem, with all optional services; returns the
% information regarding its environment.
%
% Note that OpenGL-related options are to be specified when creating a GL canvas
% (see gui_opengl:create_canvas{1,2}).
%
-spec start() -> gui_env_info().
start() ->
	start( [ mouse ] ).



% @doc Starts the MyriadGUI subsystem, with the specified services, or with all
% services while setting specified debug level; returns the information
% regarding its environment.
%
% Note that OpenGL-related options are to be specified when creating a GL canvas
% (see gui_opengl:create_canvas{1,2}).
%
-spec start( [ service() ] | debug_level() ) -> gui_env_info().
start( Services ) when is_list( Services ) ->

	% Now the identifier allocator is directly integrated in the gui_event main
	% loop:
	%
	%IdAllocPid = ?myriad_spawn_link( fun gui_id:create_id_allocator/0 ),

	% Starting the MyriadGUI environment:
	%create_gui_environment( Services, IdAllocPid );
	create_gui_environment( Services );

start( DebugLevel ) ->
	EnvInfo = start(),
	set_debug_level( DebugLevel ),
	EnvInfo.




% @doc Creates and initialises the MyriadGUI environment server; returns the
% information of the just created MyriadGUI environment.
%
% Some services must be specifically declared here, as they require
% initialisation (ex: for the loading of mouse cursors).
%
%-spec create_gui_environment( [ service() ], id_allocator_pid() ) ->
%													gui_env_info().
-spec create_gui_environment( [ service() ] ) -> gui_env_info().
%create_gui_environment( Services, IdAllocPid ) ->
create_gui_environment( Services ) ->

	cond_utils:if_defined( myriad_debug_user_interface,
		trace_utils:info_fmt( "Starting GUI, with following services: ~p",
							  [ Services ] ) ),

	GUIEnvRegName = ?gui_env_reg_name,

	{ OSFamily, OSName } = system_utils:get_operating_system_type(),

	% Initialises the wx backend (no option relevant here):
	WxServer = wx:new(),

	% The wx environment will be exported to the internal main loop process, so
	% that both the user code (i.e. the current process) and that loop can make
	% use of wx:
	%
	WxEnv = wx:get_env(),

	% Needed both directly in the main event loop and in the GUI environment
	% (ex: for when creating a plain canvas with no specific context):
	%
	TrapSet = gui_event:get_trapped_event_types( Services ),

	EventTranslationTable = gui_event:get_event_translation_table(),

	% The event table must be initialised in the spawned process, so that
	% connect/n can use the right actual, first-level subscriber PID/name, which
	% is the internal main loop in charge of the message routing and conversion:

	LoopPid = ?myriad_spawn_link( gui_event, start_main_event_loop,
		[ WxServer, WxEnv, TrapSet, EventTranslationTable ] ),

	% Caches in the calling process and initialises some GUI-related entries
	% (refer to the gui_env_entries define):
	%
	GUIEnvPid = environment:start_link_cached( GUIEnvRegName, [

		{ os_family, OSFamily },
		{ os_name, OSName },

		%{ id_allocator_pid, IdAllocPid },

		{ top_level_window, undefined },

		{ loop_pid, LoopPid },
		{ trap_set, TrapSet },
		{ event_translation_table, EventTranslationTable },

		{ backend_server, WxServer },
		{ backend_env, WxEnv },

		{ gl_canvas, undefined },
		{ gl_context, undefined } ] ),


	cond_utils:if_defined( myriad_debug_user_interface, trace_utils:info_fmt(
		"Main loop running on GUI process ~w (created from user process ~w), "
		"using environment server ~w.", [ LoopPid, self(), GUIEnvPid ] ) ),

	NonMouseServices =
			case list_utils:extract_element_if_existing( mouse, Services ) of

		false ->
			Services;

		MouseShrunkSvces ->
			gui_mouse:register_in_environment( GUIEnvPid ),
			MouseShrunkSvces

	end,

	case NonMouseServices of

		[] ->
			ok;

		_ ->
			throw( { unknown_services, NonMouseServices } )

	end,

	{ GUIEnvRegName, GUIEnvPid }.



% @doc Destructs the MyriadGUI environment server.
-spec destruct_gui_environment() -> void().
destruct_gui_environment() ->
	destruct_gui_environment( get_environment_server() ).


% @doc Destructs the specified environment server.
-spec destruct_gui_environment( gui_env_pid() ) -> void().
destruct_gui_environment( GUIEnvPid ) ->
	LoopPid = environment:get( loop_pid, GUIEnvPid ),
	LoopPid ! terminate_gui_loop,
	gui_mouse:unregister_from_environment( GUIEnvPid ),
	GUIEnvPid ! stop,

	% No wx_server needed:
	ok = wx:destroy().




% @doc Sets the debug level(s) of the GUI.
-spec set_debug_level( debug_level() ) -> void().
set_debug_level( DebugLevels ) when is_list( DebugLevels ) ->
	wx:debug( [ gui_wx_backend:to_wx_debug_level( L ) || L <- DebugLevels ] );

set_debug_level( DebugLevel ) ->
	set_debug_level( [ DebugLevel ] ).



% @doc Subscribes the current, calling process to the specified kind of events,
% resulting in corresponding MyriadGUI callback messages being received whenever
% such events occur, typically like:
%    {onWindowClosed, [WindowGUIObject, WindowId, EventContext]}
%
% The MyriadGUI general convention is to send to the subscribers a message as a
% tuple whose:
%
% - first element corresponds to the type of event having happened, as an atom
% (ex: 'onWindowClosed', 'onResized', 'onShown', etc.)
%
% - second is a list whose elements depend on the type of this event
%
% In any case, this list respects the following structure:
% [EmitterGUIObject, EmitterId, ..., EventContext]; indeed it begins with:
%   - first the reference onto the actual event emitter, as a gui_object()
%   - second its identifier (any user-specified name, otherwise the lower-level
% backend one), as a gui_id:id()
%   - ends with an event context record (see gui_event:event_context())
% concentrating all available information, should it be needed
%
% In-between, there may be additional key information for that type of event
% inserted (like the new dimensions for a onResized event, to have readily
% available instead of having to peek in the associated event context).
%
% So typical messages may be:
%  - {onWindowClosed, [WindowGUIObject, WindowId, EventContext]}
%  - {onResized, [WidgetGUIObject, WidgetId, NewSize, EventContext]}
%
% By default, subscribing to an event type implies that the corresponding events
% will still be transmitted upward in the widget hierarchy, so that other event
% handlers can apply; if wanting to disable this propagation - so that this
% event is considered to be processed for good by the current handler - either
% specify here the 'trap_event' subscription option or, later, in the
% corresponding event handler, call the trap_event/1 function.
%
% Note that trapping non-command events may prevent GUI updates done by the
% backend.
%
% Note also that, at least when creating the main frame, if having subscribed to
% onShown and onResized, on creation first a onResized event will be received by
% the subscriber (typically for a 20x20 size), then a onShown event.
%
-spec subscribe_to_events( event_subscription_spec() ) -> void().
subscribe_to_events( SubscribedEvents ) ->
	subscribe_to_events( SubscribedEvents, _SubscriberDesignator=self() ).



% @doc Subscribes the specified process to the specified kind of events,
% resulting in corresponding MyriadGUI callback messages being received whenever
% such events occur, like:
% {onWindowClosed, [WindowGUIObject, WindowId, EventContext]}
%
% Refer to subscribe_to_events/1 for further information.
%
-spec subscribe_to_events( event_subscription_spec(), event_subscriber() ) ->
											void().
subscribe_to_events( SubscribedEvents, SubscriberDesignator )
										when is_list( SubscribedEvents ) ->

	LoopPid = get_main_loop_pid(),

	% This is, in logical terms, a oneway (received in
	% gui_event:process_event_message/2), yet it must be a request (i.e. it must
	% be synchronous), otherwise a race condition exists (ex: the user
	% subscribes to 'onShown' for the main frame, and just after executes
	% 'gui:show(MainFrame)'. If subscribing is non-blocking, then the main frame
	% may be shown before being connected to the main loop, and thus it will not
	% notify the GUI main loop it is shown...

	LoopPid ! { subscribeToEvents, [ SubscribedEvents, SubscriberDesignator ],
				self() },

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:info_fmt( "User process ~w subscribing process ~w to ~w "
			"regarding following events:~n~p.",
			[ self(), SubscriberDesignator, LoopPid, SubscribedEvents ] ) ),

	receive

		onEventSubscriptionProcessed ->
			ok

	end;

subscribe_to_events( SubscribedEvent, SubscriberDesignator )
								when is_tuple( SubscribedEvent ) ->
	subscribe_to_events( [ SubscribedEvent ], SubscriberDesignator ).



% @doc Unsubscribes the current, calling process from the specified kind of
% events (event type and emitter), like {onWindowClosed, MyFrame}.
%
-spec unsubscribe_from_events( event_unsubscription_spec() ) -> void().
unsubscribe_from_events( UnsubscribedEvents ) ->
	unsubscribe_from_events( UnsubscribedEvents, _SubscriberDesignator=self() ).


% @doc Subscribes the specified process from the specified kind of events (event
% type and emitter), like {onWindowClosed, MyFrame}.
%
-spec unsubscribe_from_events( event_unsubscription_spec(),
							   event_subscriber() ) -> void().
unsubscribe_from_events( UnsubscribedEvents, SubscribedDesignator )
										when is_list( UnsubscribedEvents ) ->

	LoopPid = get_main_loop_pid(),

	% This is, in logical terms, a oneway (received in
	% gui_event:process_event_message/2), yet it must be a request as well
	% (refer to subscribe_to_events/2 for an explanation)

	LoopPid !
		{ unsubscribeFromEvents, [ UnsubscribedEvents, SubscribedDesignator ],
		  self() },

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:info_fmt( "User process ~w unsubscribing process ~w to ~w "
			"regarding following events:~n~p.",
			[ self(), SubscribedDesignator, LoopPid, SubscribedEvents ] ) ),

	receive

		onEventUnsubscriptionProcessed ->
			ok

	end;

unsubscribe_from_events( UnsubscribedEvents, SubscribedDesignator )
									when is_tuple( UnsubscribedEvents ) ->
	unsubscribe_from_events( [ UnsubscribedEvents ], SubscribedDesignator ).



% @doc Registers the specified event callback, so that, when the specified event
% type(s) are generated by the specified (source) object, a transient process is
% spawned and executes the specified event callback function, before
% terminating.
%
-spec register_event_callback( gui_object(), maybe_list( event_type() ),
							   event_callback() ) -> void().
register_event_callback( SourceGUIObject, MaybeListEventType,
						 EventCallbackFun ) ->
	register_event_callback( SourceGUIObject, MaybeListEventType,
							 EventCallbackFun, _MaybeUserData=undefined ).



% @doc Registers the specified event callback, so that, when the specified event
% type(s) are generated by the specified object, a transient process is spawned
% and executes the specified event callback function, before terminating.
%
-spec register_event_callback( gui_object(), maybe_list( event_type() ),
					event_callback(), maybe( user_data() ) ) -> void().
register_event_callback( SourceGUIObject, EventType, EventCallbackFun,
						 MaybeUserData ) when is_atom( EventType ) ->
	register_event_callback( SourceGUIObject, [ EventType ], EventCallbackFun,
							 MaybeUserData );

register_event_callback( SourceGUIObject, EventTypes, EventCallbackFun,
						 MaybeUserData ) ->

	%trace_utils:debug_fmt( "Registering event callback for ~w: events of "
	%   "types ~w will trigger ~w with user data ~w.",
	%   [ SourceGUIObject, EventTypes, EventCallbackFun, MaybeUserData ] ),

	LoopPid = get_main_loop_pid(),

	LoopPid ! { getEventTranslationTable, [], self() },

	WxUserData = case MaybeUserData of

		undefined ->
			% wx default:
			[];

		UserData ->
			UserData

	end,

	% Recording for later used when a corresponding event is fired:
	CallbackData = { EventCallbackFun, WxUserData },

	% Interleaving:
	EventTranslationTable = receive

		{ notifyEventTranslationTable, EvTransTable } ->
			EvTransTable

	end,

	% As we will have to convert back the received wx event into a MyriadGUI
	% one:

	% Closure needed to embed the translation table:
	WxCallback = fun( WxEventRecord, WxEventObject ) ->
		event_interception_callback( WxEventRecord, WxEventObject,
									 EventTranslationTable )
				 end,

	% No other option found interesting ('skip' would be ignored here):
	WxOptions = [ { callback, WxCallback },
				  { userData, CallbackData } ],

	[ begin

		WxEventType = gui_wx_backend:to_wx_event_type( ET,
									EventTranslationTable ),

		%trace_utils:debug_fmt( "Callback-connecting object ~w "
		%   "for event type ~w with options ~w.",
		%   [ SourceGUIObject, WxEventType, WxOptions ] ),

		wxEvtHandler:connect( SourceGUIObject, WxEventType, WxOptions )

	  end || ET <- EventTypes ].



% Internal function defined so that a wx callback can be converted to a
% MyriadGUI one before calling the user-specified callback with it.
%
-spec event_interception_callback( gui_event:wx_event(), wxEvent:wxEvent(),
								   event_translation_table() ) -> void().
event_interception_callback( WxEventRecord=#wx{
			userData={ EventCallbackFun, ActualUserData } },
							 WxEventObject, EventTranslationTable ) ->

	% Ex: WxEventObject={ wx_ref, 92, wxPaintEvent, [] }:
	%trace_utils:debug_fmt( "Event interception callback: WxEventObject is ~p",
	%                       [ WxEventObject ] ),

	MyriadGUIEvent = gui_event:wx_to_myriad_event(
		WxEventRecord#wx{ userData=ActualUserData }, EventTranslationTable ),

	EventCallbackFun( MyriadGUIEvent, WxEventObject ).



% @doc Traps the specified event: does not propagate it upward in the widget
% hierarchy, thus considering that it has been processed once for all by the
% current handler.
%
% Events are handled in order, from bottom to top in the widget hierarchy, by
% the last subscribed handler first. Most of the events have default event
% handler(s) set.
%
% As a result, calling this function results in having the corresponding event
% not be handled by the other handler(s) afterwards.
%
% In general, it is recommended to let all non-command events propagate, in
% order to allow the default handling of the backend GUI to take place. The
% command events are, however, normally not propagated, as usually a single
% command such as a button click or menu item selection must only be processed
% by one handler; this trap_event/1 function may then be useful, if the
% corresponding event type does not already imply trapping and if the
% 'trap_event' option was not already specified when subscribing to this event
% type. See also https://howtos.esperide.org/Erlang.html#using-wx for more
% clarifications about when trapping events is of use.
%
% Note: to be called from an event handler, i.e. at least from a process which
% set the wx environment.
%
% See propagate_event/1 for the opposition operation (forcing propagation of an
% event).
%
-spec trap_event( gui_event_object() ) -> void().
trap_event( GUIEventObject ) ->
	gui_event:trap_event( GUIEventObject ).


% @doc Propagates the specified event upward in the widget hierarchy, so that it
% can be processed by parent handlers knowing that, for some event types, by
% default no event propagation is enabled.
%
% Calling this function (from a user event handler) may be a way to trigger the
% backend built-in operations for this event only when prerequisite operations
% have been done (thus sequentially).
%
% Otherwise, if the propagation is simply enabled once for all at subscription
% time, then the user code is triggered asynchronously (through the receiving of
% a message), resulting in the built-in handlers to operate in parallel to this
% user handler. This may be a problem for example when terminating, as the
% onWindowClosed event would be propagated in the backend, leading to resources
% being deallocated, whereas the user handler is still performing its tasks -
% that may rely on these resources. A better option is having the user handler
% perform its shutdown operations, then unblock the closing mechanisms by
% propagating it in the backend thanks to this function.
%
% Refer to trap_event/1, the opposite operation, for more propagation-related
% information.
%
-spec propagate_event( gui_event_object() ) -> void().
propagate_event( GUIEventObject ) ->
	gui_event:propagate_event( GUIEventObject ).



% @doc Stops the GUI subsystem.
-spec stop() -> void().
stop() ->
	destruct_gui_environment().




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



% @doc Returns the PID of the supposedly already-running MyriadGUI environment
% server.
%
-spec get_environment_server() -> gui_env_pid().
get_environment_server() ->
	environment:get_server( ?gui_env_reg_name ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Widget section.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Common section.



% @doc Attaches a tooltip to specified widget.
-spec set_tooltip( window(), label() ) -> void().
set_tooltip( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Label ) ->
	get_main_loop_pid() ! { setTooltip, [ CanvasId, Label ] };

set_tooltip( Window, Label ) ->

	%trace_utils:debug_fmt( "Setting tooltip '~ts' to ~ts.",
	%                       [ Label, object_to_string( Window ) ] ),

	% For an unknown reason, works on panels but never on buttons:
	wxWindow:setToolTip( Window, Label ).



% @doc Sets the current process as the controller of the specified GUI object.
-spec set_as_controller( gui_object() ) -> gui_object().
set_as_controller( Object ) ->
	set_controller( Object, _ControllerPid=self() ).



% @doc Sets the process of specified PID as the controller of the specified GUI
% object.
%
-spec set_controller( gui_object(), pid() ) -> gui_object().
set_controller( Object, ControllerPid ) ->

	% Typically takes { wx_ref, Id, WxObjectType, _State=[] }, and returns:
	% { wx_ref, Id, WxObjectTypewxFrame, ControllerPid }

	wx_object:set_pid( Object, ControllerPid ).



% Window section.
%
% Base, most general class for all windows (for example a frame is a window
% whose size and position can usually be changed by the user). Represents any
% visible object on screen. All controls, top level windows and so on are
% windows. Sizers and device contexts are not, however, as they do not appear on
% screen themselves.


% @doc Creates a window.
-spec create_window() -> window().
create_window() ->
	wxWindow:new().


% @doc Creates a window of specified identifier.
%
% @hidden (internal use only)
%
-spec create_window( id(), window() ) -> window().
create_window( Id, Parent ) ->

	ActualId = declare_id( Id ),

	% Should not be 'undefined', otherwise: "wxWidgets Assert failure:
	% ./src/gtk/window.cpp(2586): \"parent\" in PreCreation() : Must have
	% non-NULL parent"}
	%
	ActualParent = to_wx_parent( Parent ),

	wxWindow:new( ActualParent, ActualId ).


% @doc Creates a window of specified size.
-spec create_window( size() ) -> window().
create_window( Size ) ->

	ActualId = declare_id( undefined ),
	ActualParent = to_wx_parent( undefined ),

	Options = [ to_wx_size( Size ) ],

	wxWindow:new( ActualParent, ActualId, Options ).


% @doc Creates a window of specified settings.
%
% @hidden (internal use only)
%
-spec create_window( position(), size(), window_style(), id(), window() ) ->
													window().
create_window( Position, Size, Style, Id, Parent ) ->

	Options = [ to_wx_position( Position ), to_wx_size( Size ),
				{ style, gui_wx_backend:window_style_to_bitmask( Style ) } ],

	ActualId = declare_id( Id ),
	ActualParent = to_wx_parent( Parent ),

	wxWindow:new( ActualParent, ActualId, Options ).


% Records the specified frame as the application top-level window, in the
% MyriadGUI environment.
%
% (helper)
%
-spec record_top_level_window( window() ) -> void().
record_top_level_window( Window ) ->
	environment:set( _K=top_level_window, _V=Window,
					 _Designator=?gui_env_reg_name ).



% @doc Sets the foreground color of the specified window.
-spec set_foreground_color( window(), color() ) -> void().
set_foreground_color( _Canvas={ myriad_object_ref, myr_canvas, CanvasId },
					  Color ) ->

	%trace_utils:debug_fmt( "Setting foreground color of canvas ~w to ~p.",
	%                       [ Canvas, Color ] ),

	get_main_loop_pid() ! { setCanvasForegroundColor, [ CanvasId, Color ] };

set_foreground_color( Window, Color ) ->

	ActualColor = gui_color:get_color( Color ),

	wxWindow:setForegroundColour( Window, ActualColor ).



% @doc Sets the background color of the specified window.
-spec set_background_color( window(), color() ) -> void().
set_background_color( _Canvas={ myriad_object_ref, myr_canvas, CanvasId },
					  Color ) ->

	%trace_utils:debug_fmt( "Setting background color of canvas ~w to ~p.",
	%                       [ Canvas, Color ] ),

	get_main_loop_pid() ! { setCanvasBackgroundColor, [ CanvasId, Color ] };

set_background_color( Window, Color ) ->

	ActualColor = gui_color:get_color( Color ),

	wxWindow:setBackgroundColour( Window, ActualColor ).



% @doc Sets the font to be used by the specified window and its children.
-spec set_font( window(), font() ) -> void().
set_font( Window, Font ) ->
	set_font( Window, Font, _DestructFont=false ).


% @doc Sets the font to be used by the specified:
% - window and its children, then, if requested, destructs that font
% - graphic context, with the specified color
%
-spec set_font( window(), font(), boolean() ) -> void();
			  ( graphic_context(), font(), color() ) -> void().
set_font( Window, Font, _DestructFont=true ) ->
	set_font( Window, Font, _DoDestructFont=false ),
	gui_font:destruct( Font );

set_font( Window, Font, _DestructFont=false ) ->
	wxWindow:setFont( Window, Font );

set_font( GraphicContext, Font, Color ) ->
	set_font( GraphicContext, Font, Color, _DoDestructFont=false ).



% @doc Sets the font and color to be used by the specified window (and its
% children) or graphic context, then, if requested, destructs that font.
%
% For windows, a side-effect is setting the foreground color.
%
-spec set_font( window() | graphic_context(), font(), color(), boolean() ) ->
																void().
set_font( GraphicContext, Font, Color, _DestructFont=true ) ->
	set_font( GraphicContext, Font, Color, _DoDestructFont=false ),
	gui_font:destruct( Font );

set_font( GraphicContext={ wx_ref, _Id, wxGraphicsContext, _State }, Font,
		  Color, _DestructFont=false ) ->
	wxGraphicsContext:setFont( GraphicContext, Font,
							   gui_color:get_color( Color ) );

% wxWindow, wxPanel, etc.:
set_font( Window={ wx_ref, _Id, _AnyWxWindowLike, _State }, Font, Color,
		  _DestructFont=false ) ->
	wxWindow:setFont( Window, Font ),
	wxWindow:setForegroundColour( Window, gui_color:get_color( Color ) ).




% Canvas section.


% @doc Creates a (basic) canvas, attached to the specified parent window.
%
% Note: not to be mixed up with gui_opengl:create_canvas/{1,2}.
%
-spec create_canvas( window() ) -> canvas().
create_canvas( Parent ) ->
	% Returns the corresponding myriad_object_ref:
	execute_instance_creation( myr_canvas, [ Parent ] ).



% @doc Sets the color to be used for the drawing of the outline of shapes.
-spec set_draw_color( canvas(), color() ) -> void().
set_draw_color( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Color ) ->
	get_main_loop_pid() ! { setCanvasDrawColor, [ CanvasId, Color ] }.



% @doc Sets the color to be using for filling surfaces.
%
% An undefined color corresponds to a fully transparent one.
%
-spec set_fill_color( canvas(), maybe( color() ) ) -> void().
set_fill_color( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Color ) ->
	get_main_loop_pid() ! { setCanvasFillColor, [ CanvasId, Color ] }.



% @doc Returns the RGB value of the pixel at specified position.
-spec get_rgb( canvas(), point() ) -> color_by_decimal_with_alpha().
get_rgb( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Point ) ->

	get_main_loop_pid() ! { getCanvasRGB, [ CanvasId, Point ], self() },

	receive

		{ notifyCanvasRGB, Color } ->
			Color

	end.



% @doc Sets the pixel at the specified position to the current RGB point value.
-spec set_rgb( canvas(), point() ) -> void().
set_rgb( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Point ) ->
	get_main_loop_pid() ! { setCanvasRGB, [ CanvasId, Point ] }.



% @doc Draws a line between the specified two points in the back-buffer of the
% specified canvas, using current draw color.
%
-spec draw_line( canvas(), point(), point() ) -> void().
draw_line( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, P1, P2 ) ->
	get_main_loop_pid() ! { drawCanvasLine, [ CanvasId, P1, P2 ] }.



% @doc Draws a line between the specified two points in specified canvas, with
% specified color.
%
-spec draw_line( canvas(), point(), point(), color() ) -> void().
draw_line( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, P1, P2,
		   Color ) ->
	get_main_loop_pid() ! { drawCanvasLine, [ CanvasId, P1, P2, Color ] }.



% @doc Draws lines between the specified list of points, in specified canvas,
% using current draw color.
%
-spec draw_lines( canvas(), [ point() ] ) -> void().
draw_lines( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Points ) ->
	get_main_loop_pid() ! { drawCanvasLines, [ CanvasId, Points ] }.



% @doc Draws lines between specified list of points in specified canvas, with
% specified color.
%
-spec draw_lines( canvas(), [ point() ], color() ) -> void().
draw_lines( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Points,
			Color ) ->
	get_main_loop_pid() ! { drawCanvasLines, [ CanvasId, Points, Color ] }.



% @doc Draws a segment of the line L between the two specified ordinates.
%
% Line L must not have for equation Y=constant (i.e. its A parameter must not be
% null).
%
-spec draw_segment( canvas(), line2(), coordinate(), coordinate() ) -> void().
draw_segment( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, L,
			  Y1, Y2 ) ->
	get_main_loop_pid() ! { drawCanvasSegment, [ CanvasId, L, Y1, Y2 ] }.



% @doc Draws the specified polygon, closing the lines and filling them.
-spec draw_polygon( canvas(), [ point() ] ) -> void().
draw_polygon( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Points ) ->
	get_main_loop_pid() ! { drawCanvasPolygon, [ CanvasId, Points ] }.



% @doc Draws the specified label (a plain string) at specified position, on
% specified canvas, using the current draw color.
%
-spec draw_label( canvas(), point(), label() ) -> void().
draw_label( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Point,
			Label ) ->
	get_main_loop_pid() ! { drawCanvasLabel, [ CanvasId, Point, Label  ] }.



% @doc Draws an upright cross at specified location (2D point), with default
% edge length.
%
-spec draw_cross( canvas(), point() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Location ) ->
	get_main_loop_pid() ! { drawCanvasCross, [ CanvasId, Location ] }.



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length.
%
-spec draw_cross( canvas(), point(), length() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Location,
			EdgeLength ) ->
	get_main_loop_pid() !
		{ drawCanvasCross, [ CanvasId, Location, EdgeLength ] }.



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length and color.
%
-spec draw_cross( canvas(), point(), length(), color() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Location,
			EdgeLength, Color ) ->
	get_main_loop_pid() !
		{ drawCanvasCross, [ CanvasId, Location, EdgeLength, Color ] }.



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length and companion label.
%
-spec draw_labelled_cross( canvas(), point(), length(), label()  ) -> void().
draw_labelled_cross( _Canvas={ myriad_object_ref, myr_canvas, CanvasId },
					 Location, EdgeLength, LabelText ) ->
	get_main_loop_pid() ! { drawCanvasLabelledCross,
							[ CanvasId, Location, EdgeLength, LabelText  ] }.



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length and companion label, and with specified color.
%
-spec draw_labelled_cross( canvas(), point(), length(), color(), label() ) ->
																void().
draw_labelled_cross( _Canvas={ myriad_object_ref, myr_canvas, CanvasId },
					 Location, EdgeLength, Color, LabelText ) ->
	get_main_loop_pid() ! { drawCanvasLabelledCross,
						[ CanvasId, Location, EdgeLength, Color, LabelText  ] }.



% @doc Renders specified circle (actually, depending on the fill color, it may
% be a disc) in specified canvas.
%
-spec draw_circle( canvas(), point(), length() ) -> void().
draw_circle( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Center,
			 Radius ) ->
	%trace_utils:debug_fmt( "Drawing circle centered at ~p.", [ Center ] ),
	get_main_loop_pid() ! { drawCanvasCircle, [ CanvasId, Center, Radius ] }.



% @doc Renders specified circle (actually, depending on the fill color, it may
% be a disc) in specified canvas.
%
-spec draw_circle( canvas(), point(), length(), color() ) -> void().
draw_circle( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Center,
			 Radius, Color ) ->

	%trace_utils:debug_fmt( "Drawing circle centered at ~p, of colour ~p.",
	%                       [ Center, Color ] ),

	get_main_loop_pid() !
		{ drawCanvasCircle, [ CanvasId, Center, Radius, Color ] }.



% @doc Draws specified list of points, each point being identified in turn with
% one cross and a label, at the same rank order (L1 for the first point of the
% list, L2 for the next, etc.).
%
-spec draw_numbered_points( canvas(), [ point() ] ) -> void().
draw_numbered_points( _Canvas={ myriad_object_ref, myr_canvas, CanvasId },
					  Points ) ->
	get_main_loop_pid() ! { drawCanvasNumberedPoints, [ CanvasId, Points ] }.



% @doc Loads image from the specified path into specified canvas, pasting it at
% its upper left corner.
%
-spec load_image( canvas(), any_file_path() ) -> void().
load_image( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Filename ) ->
	get_main_loop_pid() ! { loadCanvasImage, [ CanvasId, Filename ] }.



% @doc Loads image from the specified path into specified canvas, pasting it at
% specified location.
%
-spec load_image( canvas(), point(), any_file_path() ) -> void().
load_image( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Position,
			FilePath ) ->
	get_main_loop_pid() ! { loadCanvasImage, [ CanvasId, Position, FilePath ] }.



% @doc Resizes the specified canvas according to the specified new size.
-spec resize( canvas(), size() ) -> void().
resize( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, NewSize ) ->
	get_main_loop_pid() ! { resizeCanvas, [ CanvasId, NewSize ] }.



% @doc Blits the back-buffer of this canvas onto its visible area.
%
% The back-buffer remains as it was before this call.
%
-spec blit( canvas() ) -> void().
blit( _Canvas={ myriad_object_ref, myr_canvas, CanvasId } ) ->
	get_main_loop_pid() ! { blitCanvas, CanvasId }.


% @doc Clears the specified canvas.
%
% Note: the result will not be visible until the canvas is blitted.
%
-spec clear( canvas() ) -> void().
clear( _Canvas={ myriad_object_ref, myr_canvas, CanvasId } ) ->
	get_main_loop_pid() ! { clearCanvas, CanvasId }.




% @doc Associates specified sizer to the specified window.
-spec set_sizer( window(), sizer() ) -> void().
set_sizer( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Sizer ) ->
	get_main_loop_pid() ! { getCanvasPanel, [ CanvasId ], self() },

	receive

		{ notifyCanvasPanel, Panel } ->
			set_sizer( Panel, Sizer )

	end;

set_sizer( Window, Sizer ) ->
	wxWindow:setSizer( Window, Sizer ).



% @doc Adds to the specified sizer a stretch spacer, that is stretchable space,
% and returns it.
%
-spec add_stretch_spacer( sizer() ) -> sizer_item().
add_stretch_spacer( Sizer ) ->
	wxSizer:addStretchSpacer( Sizer ).



% @doc Lays out the children of this window using any associated sizer,
% otherwise does nothing (except if it is a top level window).
%
-spec layout( window() ) -> void().
layout( Window ) ->
	wxWindow:layout( Window ).


% @doc Resizes the specified window so that its client area matches the minimal
% size of the specified sizer.
%
-spec fit_to_sizer( window(), sizer() ) -> void().
fit_to_sizer( Window, Sizer ) ->
	wxSizer:fit( Sizer, Window ).




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

	check_orientation( Orientation ),
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
			   orientation=Orientation }.


% (helper)
-spec check_orientation( term() ) -> void().
check_orientation( vertical ) ->
	ok;

check_orientation( horizontal ) ->
	ok;

check_orientation( Other ) ->
	throw( { invalid_orientation, Other } ).



% @doc Sets the specified splitter in a single pane configuration, using for
% that specified window.
%
-spec set_unique_pane( splitter(), window() ) -> void().
set_unique_pane( #splitter{ splitter_window=SplitterWin }, WindowPane ) ->
	wxSplitterWindow:initialize( SplitterWin, WindowPane ).



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


% @doc Tells whether the specified top-level window is maximised.
-spec is_maximised( top_level_window() ) -> boolean().
is_maximised( TopLevelWindow ) ->
	wxTopLevelWindow:isMaximized( TopLevelWindow ).


% @doc Maximises the specified top-level window.
-spec maximize( top_level_window() ) -> void().
maximize( TopLevelWindow ) ->
	wxTopLevelWindow:maximize( TopLevelWindow ).


% @doc Sets the title of the specified top-level window.
-spec set_title( top_level_window(), title() ) -> void().
set_title( TopLevelWindow, Title ) ->
	wxTopLevelWindow:setTitle( TopLevelWindow, Title ).



% @doc Returns the widget (if any) that has the current keyboard focus.
-spec get_focused() -> maybe( window() ).
get_focused() ->
	wxWindow:findFocus().


% @doc Sets the specified widget to receive keyboard input.
-spec set_focus( window() ) -> void().
set_focus( Window ) ->
	wxWindow:setFocus( Window ).



% @doc Returns the size (as {Width,Height}) of the specified window or bitmap.
-spec get_size( window() | bitmap() ) -> dimensions().
get_size( _Canvas={ myriad_object_ref, myr_canvas, CanvasId } ) ->

	%trace_utils:debug_fmt( "Getting size of canvas #~B.", [ CanvasId ] ),

	get_main_loop_pid() ! { getCanvasSize, CanvasId, self() },
	receive

		{ notifyCanvasSize, Size } ->
			Size

	end;

get_size( Bitmap={ wx_ref, _Id, wxBitmap, _List } ) ->
	% No wxBitmap:getSize/1 implemented in wx:
	{ wxBitmap:getWidth( Bitmap ), wxBitmap:getHeight( Bitmap ) };

get_size( Window ) ->
	%trace_utils:debug_fmt( "get_size for ~w.", [ Window ] ),
	wxWindow:getSize( Window ).



% @doc Returns the client size (as {Width,Height}) of the specified widget,
% i.e. the actual size of the area that can be drawn upon (excluded menu, bars,
% etc.)
%
-spec get_client_size( window() ) -> dimensions().
get_client_size( _Canvas={ myriad_object_ref, myr_canvas, CanvasId } ) ->

	get_main_loop_pid() ! { getCanvasClientSize, CanvasId, self() },
	receive

		{ notifyCanvasClientSize, Size } ->
			Size

	end;

get_client_size( Window ) ->
	wxWindow:getClientSize( Window ).



% @doc Returns the best size (as {Width,Height}) of the specified widget, that
% is its best acceptable minimal size.
%
-spec get_best_size( window() ) -> dimensions().
get_best_size( _Canvas={ myriad_object_ref, myr_canvas, CanvasId } ) ->

	get_main_loop_pid() ! { getCanvasClientSize, CanvasId, self() },
	receive

		{ notifyCanvasClientSize, Size } ->
			Size

	end;

get_best_size( Window ) ->
	wxWindow:getBestSize( Window ).



% @doc Sets the size of the client area of the specified widget
-spec set_client_size( window(), dimensions() ) -> void().
set_client_size( _Canvas={ myriad_object_ref, myr_canvas, _CanvasId },
				 _Size ) ->
	throw( not_implemented );

set_client_size( Window, Size ) ->
	wxWindow:setClientSize( Window, Size ).


% @doc Fits the specified widget to its best size.
%
% Corresponds to setting its client size to its best one.
%
-spec fit( window() ) -> void().
fit( _Canvas={ myriad_object_ref, myr_canvas, _CanvasId } ) ->
	throw( not_implemented );

fit( Window ) ->
	wxWindow:fit( Window).



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



% @doc Synchronises the specified window with the MyriadGUI loop, to ensure that
% no past operation is still pending at its level.
%
% Useful if there exists some means of interacting with it directly (ex: thanks
% to an OpenGL NIF) that could create a race condition (ex: presumably
% message-based resizing immediately followed by a direct OpenGL rendering).
%
% See gui_opengl_minimal_test:on_main_frame_resize/1 for further details.
%
-spec sync( window() ) -> dimensions().
sync( Window ) ->
	% The result in itself may be of no use; the point here is just, through a
	% synchronous operation (a request), to ensure that the specified window is
	% "ready" (that it has processed all its previous messages) with a
	% sufficient probability (and with certainty if past operations were
	% triggered by the same process as this calling one):
	%
	wxWindow:getSize( Window ).



% @doc Updates the specified window-like object (ex: a canvas) so that its
% client area can be painted.
%
% To be called from a repaint event handler.
%
% See [https://www.erlang.org/doc/man/wxpaintdc#description] for more details.
%
% Based on our tests, does not seem strictly necessary.
%
-spec enable_repaint( window() ) -> void().
enable_repaint( Window ) ->
	DC = wxPaintDC:new( Window ),
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

% An application has generally exactly one top-level frame. Creating such kind
% of frame allows to record it, and then the window management services are able
% to tell whether for example the application as a whole shall be considered as
% maximised.


% @doc Creates a top-level frame, with default position, size, style and ID.
-spec create_top_level_frame( title() ) -> frame().
create_top_level_frame( Title ) ->
	Frame = create_frame( Title ),
	record_top_level_window( Frame ),
	Frame.



% @doc Creates a top-level frame, with specified size, and default ID.
-spec create_top_level_frame( title(), size() ) -> frame().
create_top_level_frame( Title, Size ) ->
	Frame = create_frame( Title, Size ),
	record_top_level_window( Frame ),
	Frame.



% @doc Creates a top-level frame, with specified title, position, size and
% style.
%
-spec create_top_level_frame( title(), position(), size(), frame_style() ) ->
												frame().
create_top_level_frame( Title, Position, Size, Style ) ->
	Frame = create_frame( Title, Position, Size, Style ),
	record_top_level_window( Frame ),
	Frame.



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
	% We could see a case where a call to wxFrame:new/0 issued by an helper
	% spawned process (having set its environment) would trigger a segmentation
	% fault, whereas wxFrame:new(wx:null(), ?wxID_ANY, "Hello") worked
	% flawlessly:
	%
	wxFrame:new().


% @doc Creates a titled frame, with default position, size, style, ID and
% parent.
%
-spec create_frame( title() ) -> frame().
create_frame( Title ) ->
	wxFrame:new( to_wx_parent( undefined ), declare_id( undefined ), Title ).



% @doc Creates a frame, with specified title and size, and default ID and
% parent.
%
-spec create_frame( title(), size() ) -> frame().
create_frame( Title, Size ) ->

	Options = [ to_wx_size( Size ) ],

	%trace_utils:debug_fmt( "create_frame options: ~p.", [ Options ] ),

	wxFrame:new( to_wx_parent( undefined ), declare_id( undefined ), Title,
				 Options ).



% @doc Creates a frame, with default position, size and style.
%
% (internal use only)
%
-spec create_frame( title(), id(), maybe( window() ) ) -> frame().
create_frame( Title, Id, Parent ) ->
	wxFrame:new( to_wx_parent( Parent ), declare_id( Id ), Title ).


% @doc Creates a frame, with specified title, position, size and style, and with
% a default parent.
%
-spec create_frame( title(), position(), size(), frame_style() ) -> frame().
create_frame( Title, Position, Size, Style ) ->

	Options = [ to_wx_position( Position ), to_wx_size( Size ),
				{ style, frame_style_to_bitmask( Style ) } ],

	%trace_utils:debug_fmt( "create_frame options: ~p.", [ Options ] ),

	wxFrame:new( to_wx_parent( undefined ), declare_id( undefined ), Title,
				 Options ).



% @doc Creates a frame, with specified title, position, size and style, and with
% a default parent.
%
% (internal use only: wx exposed)
%
-spec create_frame( title(), position(), size(), frame_style(), id(),
					window() ) -> frame().
create_frame( Title, Position, Size, Style, Id, Parent ) ->

	Options = [ to_wx_position( Position ), to_wx_size( Size ),
				{ style, frame_style_to_bitmask( Style ) } ],

	ActualId = declare_id( Id ),

	ActualParent = to_wx_parent( Parent ),

	wxFrame:new( ActualParent, ActualId, Title, Options ).



% @doc Sets the icon of the specified (main) frame.
-spec set_icon( frame(), file_path() ) -> void().
set_icon( Frame, IconPath ) ->

	% Supported image formats documented as being only BMP by default, yet test
	% on PNG succeeded.

	% Current no wx_image:initAllImageHandlers/* (for other formats than BMP),
	% just wx_image:initStandardHandlers/0.

	% Apparently 'Icon = wxIcon:new( IconPath ),' could have sufficed:
	Img = wxImage:new( IconPath ),
	Bitmap = wxBitmap:new( Img ),
	Icon = wxIcon:new(),
	wxIcon:copyFromBitmap( Icon, Bitmap ),

	wxTopLevelWindow:setIcon( Frame, Icon ).



% @doc Creates a toolbar in the specified frame.
-spec create_toolbar( frame() ) -> toolbar().
create_toolbar( Frame ) ->
	wxFrame:createToolBar( Frame ).


% @doc Creates a toolbar in the specified frame, with the specified identifier
% (if any) and style.
%
% Apparently up to one toolbar can be associated to a frame (ex: no top and left
% toolbars allowed simultaneously).
%
-spec create_toolbar( frame(), id(), maybe_list( toolbar_style() ) ) ->
											toolbar().
create_toolbar( Frame, Id, MaybeToolbarStyles ) ->
	wxFrame:createToolBar( Frame, [ { id, declare_id( Id ) },
		{ style, gui_wx_backend:to_wx_toolbar_style( MaybeToolbarStyles ) } ] ).



% @doc Sets the specified toolbar in the specified frame.
-spec set_toolbar( frame(), toolbar() ) -> void().
set_toolbar( Frame, Toolbar ) ->
	wxFrame:setToolBar( Frame, Toolbar ).



% @doc Destructs the specified frame.
-spec destruct_frame( frame() ) -> void().
destruct_frame( Frame  ) ->
	wxFrame:destroy( Frame ).



% Panel section.


% @doc Creates a panel.
-spec create_panel() -> panel().
create_panel() ->
	wxPanel:new().



% @doc Creates a panel, associated to the specified parent.
-spec create_panel( window() | splitter() ) -> panel().
create_panel( _Parent=#splitter{ splitter_window=Win } ) ->
	wxPanel:new( Win );

create_panel( Parent ) ->
	wxPanel:new( Parent ).



% @doc Creates a panel, associated to specified parent and with specified
% options.
%
-spec create_panel( window() | splitter(), panel_options() ) -> panel().
create_panel( _Parent=#splitter{ splitter_window=Win }, Options ) ->
	create_panel( Win, Options );

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

	FullOptions = [ to_wx_position( Position ), to_wx_size( Size )
						| get_panel_options( Options ) ],

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

	FullOptions = [ { pos, { X, Y } }, { size, { Width, Height } }
						| get_panel_options( Options ) ],

	wxPanel:new( Parent, FullOptions ).



% @doc Destructs the specified panel.
-spec destruct_panel( panel() ) -> void().
destruct_panel( Panel ) ->
	wxPanel:destroy( Panel ).



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
-spec create_button( label(), position(), size(), button_style(), id(),
					 window() ) -> button().
create_button( Label, Position, Size, Style, Id, Parent ) ->

	Options = [ { label, Label }, to_wx_position( Position ),
				to_wx_size( Size ),
				{ style, gui_wx_backend:button_style_to_bitmask( Style ) } ],

	%trace_utils:info_fmt( "Button options for ID #~B: ~p.", [ Id, Options ] ),

	wxButton:new( Parent, declare_id( Id ), Options ).




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
add_to_sizer( Sizer, _Element={ myriad_object_ref, myr_canvas, CanvasId } ) ->
	get_main_loop_pid() ! { getPanelForCanvas, CanvasId, self() },
	receive

		{ notifyCanvasPanel, AssociatedPanel } ->
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
-spec add_to_sizer( sizer(), sizer_child(), maybe_list( sizer_options() ) ) ->
											sizer_item();
				  ( sizer(), [ sizer_child() ],
								maybe_list( sizer_options() ) ) -> void().
add_to_sizer( Sizer, _Element={ myriad_object_ref, myr_canvas, CanvasId },
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



% @doc Adds a spacer child to the specified sizer.
-spec add_spacer_to_sizer( sizer(), width(), height(),
						   maybe_list( sizer_options() ) ) -> sizer_item().
add_spacer_to_sizer( Sizer, Width, Height, Options ) ->
	ActualOptions = gui_wx_backend:to_wx_sizer_options( Options ),
	wxSizer:add( Sizer, Width, Height, ActualOptions ).



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
% They can be split in separate fields.


% @doc Creates and attaches a status bar at the bottom of the specified frame.
-spec create_status_bar( frame() ) -> status_bar().
create_status_bar( Frame ) ->
	% No interesting option (winid):
	wxFrame:createStatusBar( Frame ).


% @doc Creates and attaches a status bar of the specified style at the bottom of
% the specified frame.
%
-spec create_status_bar( frame(), status_bar_style() ) -> status_bar().
create_status_bar( Frame, StatusBarStyle ) ->
	% No interesting option (winid):
	wxFrame:createStatusBar( Frame, [ { style,
			gui_wx_backend:to_wx_status_bar_style( StatusBarStyle ) } ] ).


% @doc Destructs the specified status bar.
-spec destruct_status_bar( status_bar() ) -> void().
destruct_status_bar( StatusBar ) ->
	wxStatusBar:destroy( StatusBar ).


% @doc Sets the number of fields in which the specified status bar is to be
% (evenly) split.
%
-spec set_field_count( status_bar(), field_count() ) -> void().
set_field_count( StatusBar, FieldCount ) ->
	wxStatusBar:setFieldsCount( StatusBar, FieldCount ).


% @doc Sets the width of the fields of the specified status bar (and thus
% determines their number as well).
%
-spec set_field_widths( status_bar(), [ field_width() ] ) -> void().
set_field_widths( StatusBar, FieldWidths ) ->
	wxStatusBar:setFieldsCount( StatusBar, length( FieldWidths ),
								[ { widths, FieldWidths } ] ).



% @doc Pushes the specified text in the (first field of the) specified status
% bar.
%
-spec push_status_text( text(), status_bar() ) -> void().
push_status_text( Text, StatusBar ) ->
	wxStatusBar:pushStatusText( StatusBar, Text ).


% @doc Pushes the specified text in the (first field of the) specified status
% bar.
%
-spec push_status_text( format_string(), format_values(), status_bar() ) ->
											void().
push_status_text( FormatString, FormatValues, StatusBar ) ->
	Text = text_utils:format( FormatString, FormatValues ),
	push_status_text( Text, StatusBar ).



% @doc Pushes the specified text in the specified field of the specified status
% bar.
%
-spec push_field_status_text( text(), status_bar(), field_index() ) -> void().
push_field_status_text( Text, StatusBar, FieldIndex ) ->
	% Wx starts at zero:
	wxStatusBar:pushStatusText( StatusBar, Text, [ { number, FieldIndex-1 } ] ).


% @doc Pushes the specified text in the specified status bar.
-spec push_field_status_text( format_string(), format_values(), status_bar(),
							  field_index() ) -> void().
push_field_status_text( FormatString, FormatValues, StatusBar, FieldIndex ) ->
	Text = text_utils:format( FormatString, FormatValues ),
	push_field_status_text( Text, StatusBar, FieldIndex ).




% Toolbar section.
%
% Refer to create_toolbar/1 in order to obtain a toolbar object.


% @doc Adds the specified control to the specified toolbar.
-spec add_control( toolbar(), control() ) -> void().
add_control( Toolbar, Control ) ->
	wxToolBar:addControl( Toolbar, Control ).


% For add_separator/1, refer to the menu section.


% @doc Adds the specified tool, represented by the specified bitmap, with the
% specified identifier (if any) and short help, to the specified toolbar.
%
% update_tools/1 should be called once additions have been done, so that they
% are taken into account.
%
-spec add_tool( toolbar(), id(), label(), bitmap(), help_info() ) -> void().
add_tool( Toolbar, Id, Label, Bitmap, ShortHelp ) ->

	Opts = case ShortHelp of

		undefined ->
			[];

		_ ->
			[ { shortHelp, ShortHelp } ]

	end,

	wxToolBar:addTool( Toolbar, declare_id( Id ), Label, Bitmap, Opts ).


% @doc Adds the specified tool, represented by the specified enabled/disabled
% bitmaps, with the specified identifier (if any) and short/long helps, to the
% specified toolbar.
%
% update_tools/1 should be called once additions have been done, so that they
% are taken into account.
%
% Use our gui_frame_bars_test.erl test in order to display all known tools.
%
-spec add_tool( toolbar(), id(), label(), bitmap(), bitmap(), help_info(),
				help_info() ) -> void().
add_tool( Toolbar, Id, Label, BitmapIfEnabled, BitmapIfDisabled,
		  ShortHelp, LongHelp ) ->

	Opts = case ShortHelp of
		undefined -> [];
		_ -> [ { shortHelp, ShortHelp } ]
	end ++ case LongHelp  of
		undefined -> [];
		_ -> [ { longHelp, LongHelp } ]
	end,

	wxToolBar:addTool( Toolbar, declare_id( Id ), Label,
					   BitmapIfEnabled, BitmapIfDisabled, Opts ).



% @doc Updates the specified toolbar so that it takes into account any new
% tools; returns whether an update had to be done.
%
-spec update_tools( toolbar() ) -> boolean().
update_tools( Toolbar ) ->
	wxToolBar:realize( Toolbar ).



% Brush section.


% @doc Returns a brush of the specified color.
-spec create_brush( color_by_decimal() ) -> brush().
create_brush( RGBColor ) ->
	wxBrush:new( RGBColor ).


% @doc Returns a brush of the specified color, whose background is transparent.
-spec create_transparent_brush( color_by_decimal() ) -> brush().
create_transparent_brush( RGBColor ) ->
	wxBrush:new( RGBColor, [ { style, ?wxTRANSPARENT } ] ).


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




% Bitmap display section.


% @doc Creates a bitmap display from the specified bitmap.
-spec create_bitmap_display( window(), bitmap() ) -> bitmap_display().
create_bitmap_display( Parent, Bitmap ) ->
	gui_image:create_bitmap_display( Parent, Bitmap ).


% @doc Creates a bitmap display from the specified bitmap and with the specified
% options.
%
-spec create_bitmap_display( window(), bitmap(), [ window_option() ] ) ->
												bitmap_display().
create_bitmap_display( Parent, Bitmap, Options ) ->
	gui_image:create_bitmap_display( Parent, Bitmap, Options ).


% @doc Destructs the specified bitmap display.
-spec destruct_bitmap_display( bitmap_display() ) -> void().
destruct_bitmap_display( BitmapDisplay ) ->
	gui_image:destruct_bitmap_display( BitmapDisplay ).



% @doc Creates a text display from the specified text.
-spec create_text_display( window(), label() ) -> text_display().
create_text_display( Parent, Label ) ->
	gui_image:create_text_display( Parent, Label ).


% @doc Creates a text display from the specified label and with the specified
% options.
%
-spec create_text_display( window(), label(), [ text_display_option() ] ) ->
													text_display().
create_text_display( Parent, Label, Options ) ->
	gui_image:create_text_display( Parent, Label, Options ).


% @doc Destructs the specified text display.
-spec destruct_text_display( text_display() ) -> void().
destruct_text_display( TextDisplay ) ->
	gui_image:text( TextDisplay ).



% Device context section.


% @doc Returns a flicker-free paint device context corresponding to the
% specified widget, so that its client area can be modified (from an
% onRepaintNeeded event handler).
%
-spec get_flickerfree_paint_device_context( window() ) ->
												paint_device_context().
get_flickerfree_paint_device_context( Widget ) ->
	get_flickerfree_paint_device_context( Widget,
							system_utils:get_operating_system_family() ).


% @doc Returns a flicker-free device context corresponding to the specified
% widget (on specified OS family), so that its client area can be modified (from
% an onRepaintNeeded event handler).
%
-spec get_flickerfree_paint_device_context( window(), os_family() ) ->
													paint_device_context().
get_flickerfree_paint_device_context( Widget, _OSFamily=win32 ) ->
	% Otherwise flickers on Windows:
	wx:typeCast( wxBufferedPaintDC:new( Widget ), _NewType=wxPaintDC );

get_flickerfree_paint_device_context( Widget, _OSFamily ) ->
	wxPaintDC:new( Widget ).


% @doc Destructs the specified paint device context.
-spec destruct_paint_device_context( paint_device_context() ) -> void().
destruct_paint_device_context( PaintDeviceContext ) ->
	wxPaintDC:destroy( PaintDeviceContext ).



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




% Graphic context section.


% @doc Returns a graphic context created from the specified window device
% context.
%
-spec create_graphic_context( window_device_context() ) -> graphic_context().
create_graphic_context( DC ) ->
	wxGraphicsContext:create( DC ).


-spec set_brush( graphic_context(), brush() ) -> void().
set_brush( GraphicContext, Brush ) ->
	wxGraphicsContext:setBrush( GraphicContext, Brush ).



% set_font/{3,4} are already defined in the window() section.



% @doc Returns the extent corresponding to the rendering of the specified
% string, using the currently selected font.
%
% Note that the returned dimensions may be integer or floating-point numbers.
%
-spec get_text_extent( graphic_context(), ustring() ) ->
				{ any_width(), any_height(),
				  Descent :: any_length(), ExternalLeading :: any_length() }.
get_text_extent( GraphicContext, Text ) ->
	wxGraphicsContext:getTextExtent( GraphicContext, Text ).


% @doc Returns the (integer) width corresponding to the rendering of the
% specified string, using the currently selected font.
%
-spec get_text_width( graphic_context(), ustring() ) -> width().
get_text_width( GraphicContext, Text ) ->

	{ W, _H, _D, _EL } =
		wxGraphicsContext:getTextExtent( GraphicContext, Text ),

	type_utils:ensure_rounded_integer( W ).


% @doc Returns the (integer) height corresponding to the rendering of the
% specified string, using the currently selected font.
%
-spec get_text_height( graphic_context(), ustring() ) -> height().
get_text_height( GraphicContext, Text ) ->

	{ _W, H, _D, _EL } =
		wxGraphicsContext:getTextExtent( GraphicContext, Text ),

	type_utils:ensure_rounded_integer( H ).


% @doc Returns the (integer) dimensions corresponding to the rendering of the
% specified string, using the currently selected font.
%
-spec get_text_dimensions( graphic_context(), ustring() ) -> dimensions().
get_text_dimensions( GraphicContext, Text ) ->

	{ W, H, _D, _EL } =
		wxGraphicsContext:getTextExtent( GraphicContext, Text ),

	{ type_utils:ensure_rounded_integer( W ),
	  type_utils:ensure_rounded_integer( H ) }.




% @doc Renders the specified text on the specified graphic context at the
% specified position.
%
-spec draw_text( graphic_context(), ustring(), coordinate(), coordinate() ) ->
										void().
draw_text( GraphicContext, Text, X, Y ) ->
	wxGraphicsContext:drawText( GraphicContext, Text, X, Y ).



% @doc Draws the specified bitmap onto the specified graphic context, at the
% specified location.
%
-spec draw_bitmap( graphic_context(), bitmap(), coordinate(), coordinate() ) ->
																void().
draw_bitmap( GraphicContext, Bitmap, X, Y ) ->
	{ W, H } = get_size( Bitmap ),
	wxGraphicsContext:drawBitmap( GraphicContext, Bitmap, X, Y, W, H ).


% @doc Draws the specified bitmap onto the specified graphic context, at the
% specified location with the specified dimensions.
%
-spec draw_bitmap( graphic_context(), bitmap(), coordinate(), coordinate(),
				   dimensions() ) -> void().
draw_bitmap( GraphicContext, Bitmap, X, Y, _Dims={ Width, Height } ) ->
	wxGraphicsContext:drawBitmap( GraphicContext, Bitmap, X, Y, Width, Height ).



% @doc Draws the specified bitmap onto the specified graphic context, at
% specified location with specified dimensions.
%
-spec draw_bitmap( graphic_context(), bitmap(), coordinate(), coordinate(),
				   width(), height() ) -> void().
draw_bitmap( GraphicContext, Bitmap, X, Y, Width, Height ) ->
	wxGraphicsContext:drawBitmap( GraphicContext, Bitmap, X, Y, Width, Height ).




% Menu section.
%
% A menu can be generically designed, before being assigned to a menu bar or a
% popup menu.
%
% Items are added at the current bottom of a menu.
%
% A submenu may be attached up to once in a menu.


% @doc Creates a menu, either to be attached to a menu bar, or to be used as a
% popup menu.
%
-spec create_menu() -> menu().
create_menu() ->
	wxMenu:new().


% @doc Creates a menu with the specified options, either to be attached to a
% menu bar, or to be used as a popup menu.
%
-spec create_menu( maybe_list( menu_option() ) ) -> menu().
create_menu( MaybeOptions ) ->
	WxStyleOpts = gui_wx_backend:to_wx_menu_options( MaybeOptions ),
	wxMenu:new( [ { style, WxStyleOpts } ] ).


% @doc Creates a menu with the specified title and options, either to be
% attached to a menu bar, or to be used as a popup menu.
%
-spec create_menu( title(), maybe_list( menu_option() ) ) -> menu().
create_menu( Title, MaybeOptions ) ->
	WxStyleOpts = gui_wx_backend:to_wx_menu_options( MaybeOptions ),
	wxMenu:new( Title, [ { style, WxStyleOpts } ] ).


% @doc Destructs the specified menu.
-spec destruct_menu( menu() ) -> void().
destruct_menu( Menu ) ->
	wxMenu:destroy( Menu ).


% @doc Returns a list of the names of the standard menu item identifiers.
-spec get_standard_item_names() -> [ name_id() ].
get_standard_item_names() ->
	% Must correspond to standard_menu_item_name_id():
	[ new_menu_item, open_menu_item, close_menu_item,
	  save_menu_item, save_as_menu_item,
	  revert_to_saved_menu_item,
	  undelete_menu_item,
	  print_menu_item, preview_menu_item,
	  revert_menu_item, edit_menu_item, file_menu_item, properties_menu_item,
	  cut_menu_item, copy_menu_item, paste_menu_item, delete_menu_item,
	  find_menu_item, select_all_menu_item,
	  replace_menu_item, replace_all_menu_item,
	  clear_menu_item,
	  ok_menu_item, cancel_menu_item, apply_menu_item,
	  yes_menu_item, no_menu_item,
	  add_menu_item, remove_menu_item,
	  convert_menu_item, execute_menu_item,
	  home_menu_item, refresh_menu_item, stop_menu_item, index_menu_item,
	  select_color_menu_item, select_font_menu_item,
	  forward_menu_item, backward_menu_item,
	  up_menu_item, down_menu_item,
	  top_menu_item, bottom_menu_item, first_menu_item, last_menu_item,
	  jump_to_menu_item, info_menu_item,
	  zoom_factor_one, zoom_factor_fit, zoom_factor_in, zoom_factor_out,
	  undo_menu_item, redo_menu_item, help_menu_item, preferences_menu_item,
	  about_menu_item,
	  floppy_menu_item, hard_disk_menu_item, network_menu_item,
	  exit_menu_item ].


% @doc Creates a menu item based on the specified label, adds it to the
% specified menu, and returns that menu item.
%
-spec add_item( menu(), menu_item_label() ) -> menu_item().
add_item( Menu, MenuItemLabel ) ->
	add_item( Menu, _MenuItemId=undefined, MenuItemLabel ).



% @doc Creates a menu item based on the specified identifier and label, adds it
% to the specified menu, and returns that menu item.
%
-spec add_item( menu(), menu_item_id(), menu_item_label() ) -> menu_item().
add_item( Menu, MenuItemId, MenuItemLabel ) when is_integer( MenuItemId ) ->
	wxMenu:append( Menu, declare_id( MenuItemId ), MenuItemLabel );

add_item( Menu, MenuItemId, MenuItemLabel ) when is_atom( MenuItemId ) ->
	% We must not declare a standard name identifier (as it already is):
	BackendId = case lists:member( MenuItemId, get_standard_item_names() ) of

		true ->
			resolve_id( MenuItemId );

		false ->
			declare_id( MenuItemId )

	end,
	wxMenu:append( Menu, BackendId, MenuItemLabel ).



% @doc Appends the specified already-created menu item (not a mere label) to the
% specified menu, and returns a possibly updated version of that menu item.
%
-spec append_item( menu(), menu_item() ) -> menu_item().
append_item( Menu, MenuItem ) ->
	wxMenu:append( Menu, MenuItem ).


% @doc Adds the specified labelled submenu, associated to the specified
% identifier, to the specified menu, and returns the corresponding menu item.
%
-spec append_submenu( menu(), menu_item_id(), menu_item_label(), menu() ) ->
													menu_item().
append_submenu( Menu, MenuItemId, MenuItemLabel, SubMenu ) ->
	% Not resolve_id( MenuItemId ):
	wxMenu:append( Menu, declare_id( MenuItemId ), MenuItemLabel, SubMenu ).


% @doc Adds the specified labelled submenu, associated to the specified
% identifier and help information, to the specified menu, and returns that item.
%
-spec append_submenu( menu(), menu_item_id(), menu_item_label(), menu(),
					  help_info() ) -> menu_item().
append_submenu( Menu, MenuItemId, MenuItemLabel, SubMenu, HelpInfoStr ) ->
	wxMenu:append( Menu, declare_id( MenuItemId ), MenuItemLabel, SubMenu,
				   [ { help, HelpInfoStr } ] ).


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
-spec add_checkable_item( menu(), menu_item_id(), menu_item_label() ) ->
													menu_item().
add_checkable_item( Menu, MenuItemId, MenuItemLabel ) ->
	wxMenu:appendCheckItem( Menu, declare_id( MenuItemId ), MenuItemLabel ).


% @doc Creates a menu item that can be toggled/checked, based on the specified
% identifier, label and help information, adds it to the specified menu, and
% returns that menu item.
%
-spec add_checkable_item( menu(), menu_item_id(), menu_item_label(),
						  help_info() ) -> menu_item().
add_checkable_item( Menu, MenuItemId, MenuItemLabel, HelpInfoStr ) ->
	wxMenu:appendCheckItem( Menu, declare_id( MenuItemId ), MenuItemLabel,
							[ { help, HelpInfoStr } ] ).


% @doc Checks/unchecks the (checkable) menu item specified by its identifier
% (not by its menu item reference).
%
-spec set_checkable_menu_item( menu(), menu_item_id(), boolean() ) -> void().
set_checkable_menu_item( Menu, MenuItemId, SetAsChecked ) ->
	wxMenu:check( Menu, resolve_id( MenuItemId ), SetAsChecked ).


% @doc Adds the specified radio item to the specified menu, and returns that
% item.
%
% All consequent radio items form a group and when an item in the group is
% checked, all the others are automatically unchecked.
%
-spec add_radio_item( menu(), menu_item_id(), menu_item_label() ) ->
													menu_item().
add_radio_item( Menu, MenuItemId, MenuItemLabel ) ->
	wxMenu:appendRadioItem( Menu, resolve_id( MenuItemId ), MenuItemLabel ).


% @doc Adds the specified labelled item that can be checkd/checked to the
% specified menu, and returns that item.
%
-spec add_radio_item( menu(), menu_item_id(), menu_item_label(),
					  help_info() ) -> menu_item().
add_radio_item( Menu, MenuItemId, MenuItemLabel, HelpInfoStr ) ->
	wxMenu:appendRadioItem( Menu, resolve_id( MenuItemId ), MenuItemLabel,
							[ { help, HelpInfoStr } ] ).


% @doc Adds a separator to the specified menu or toolbar, and returns that
% separator.
%
-spec add_separator( menu() | toolbar() ) -> menu_item().
add_separator( Menu={ wx_ref, _InstanceRef, _WxObjectType=wxMenu, _State } ) ->
	wxMenu:appendSeparator( Menu );

add_separator(
		Toolbar={ wx_ref, _InstanceRef, _WxObjectType=wxToolBar, _State } ) ->
	wxToolBar:addSeparator( Toolbar ).




% @doc Sets the enabled/disabled status of the specified menu item.
-spec set_menu_item_status( menu(), menu_item_id(), menu_item_status() ) ->
														void().
set_menu_item_status( Menu, MenuItemId, _NewEnableStatus=enabled ) ->
	wxMenu:enable( Menu, resolve_id( MenuItemId ), _Check=true );

set_menu_item_status( Menu, MenuItemId, _NewEnableStatus=disabled ) ->
	wxMenu:enable( Menu, resolve_id( MenuItemId ), _Check=false ).



% @doc Removes the specified item from the specified menu.
%
% If the item is a submenu, it is removed yet not deallocated.
%
-spec remove_menu_item( menu(), menu_item_id() ) -> void().
remove_menu_item( Menu, MenuItemId ) ->
	wxMenu:delete( Menu, resolve_id( MenuItemId ) ).



% Menu bar subsection.


% @doc Creates an empty menu bar, at the top of the specified parent window.
-spec create_menu_bar() -> menu_bar().
create_menu_bar() ->
	wxMenuBar:new().


% @doc Adds the specified menu to the specified menu bar.
-spec add_menu( menu_bar(), menu(), menu_label() ) -> void().
add_menu( MenuBar, Menu, MenuTitle ) ->
	true = wxMenuBar:append( MenuBar, Menu, MenuTitle ).


% @doc Assigns the specified menu bar to the specified frame.
-spec set_menu_bar( frame(), menu_bar() ) -> void().
set_menu_bar( Frame, MenuBar ) ->
	wxFrame:setMenuBar( Frame, MenuBar ).


% Popup menu subsection.


% @doc Activates the specified menu as a popup one on the specified widget.
%
% Typically called on receiving of a onMouseRightButtonReleased event.
%
-spec activate_popup_menu( widget(), menu() ) -> void().
activate_popup_menu( Widget, Menu ) ->
	% Meaning on returned boolean unclear:
	wxWindow:popupMenu( Widget, Menu ).



% Font section.


% @doc Creates a corresponding font object, to determine the appearance of
% rendered text.
%
-spec create_font( font_size() | point_size() ) -> font().
create_font( FontSize ) ->
	gui_font:create( FontSize ).


% @doc Creates a corresponding font object, to determine the appearance of
% rendered text.
%
-spec create_font( font_size() | point_size(), font_family() ) -> font().
create_font( FontSize, FontFamily ) ->
	gui_font:create( FontSize, FontFamily ).


% @doc Creates a corresponding font object, to determine the appearance of
% rendered text.
%
-spec create_font( font_size() | point_size(), font_family(), font_style() ) ->
															font().
create_font( FontSize, FontFamily, FontStyle ) ->
	gui_font:create( FontSize, FontFamily, FontStyle ).


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


% @doc Destructs the specified font object.
-spec destruct_font( font() ) -> void().
destruct_font( Font ) ->
	wxFont:destroy( Font ).



% Miscellaneous.


% @doc Gets the backend environment currently used by the calling process,
% typically to transmit it to any other process in order to enable it to use
% that backend.
%
-spec get_backend_environment() -> backend_environment().
get_backend_environment() ->
	wx:get_env().


% @doc Sets the specified backend environment for the calling process, so that
% it can make use of the corresponding backend.
%
-spec set_backend_environment( backend_environment() ) -> void().
set_backend_environment( WxEnv ) ->
	wx:set_env( WxEnv ).



% General MyriadGUI helpers.


% @doc Requests the creation of the specified instance (to be done from the
% MyriadGUI main loop), and returns the corresponding GUI object reference.
%
-spec execute_instance_creation( myriad_object_type(),
						construction_parameters() ) -> myriad_object_ref().
execute_instance_creation( ObjectType, ConstructionParams ) ->

	cond_utils:if_defined( myriad_debug_gui_instances,
		trace_utils:debug_fmt( "Requesting the creation of a '~ts' instance, "
			"based on the following construction parameters:~n~w.",
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
% of running the main MyriadGUI loop.
%
-spec get_main_loop_pid() -> pid().
get_main_loop_pid() ->
	environment:get( loop_pid, ?gui_env_process_key ).


% @doc Returns a backend-specific widget identifier associated to the specified
% new identifier, expected not to have already been declared.
%
-spec declare_id( id() ) -> backend_id().
% Module-local, meant to declare quickly most cases.
declare_id( undefined ) ->
	?wxID_ANY;

declare_id( Id ) when is_integer( Id ) ->
	Id;

declare_id( NameId ) ->
	get_main_loop_pid() ! { declareNameId, NameId, self() },
	receive

		{ notifyingAssignedId, BackendId } ->
			BackendId

	end.



% @doc Returns the backend-specific widget identifier supposed to be already
% associated to the specified identifier, expected not to have already been
% resolved.
%
-spec resolve_id( id() ) -> backend_id().
% Module-local, meant to resolve quickly most cases.
resolve_id( undefined ) ->
	?wxID_ANY;

resolve_id( Id ) when is_integer( Id ) ->
	Id;

resolve_id( NameId ) ->
	get_main_loop_pid() ! { resolveNameId, NameId, self() },
	receive

		{ notifyResolvedIdentifier, BackendId } ->
			BackendId

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



% @doc Returns a textual representation of the specified GUI object key.
-spec object_key_to_string( gui_object_key() ) -> ustring().
object_key_to_string( { AnyObjectType, AnyInstanceId } ) ->
	text_utils:format( "~ts-~B", [ AnyObjectType, AnyInstanceId ] ).




% @doc Returns a textual representation of the specified GUI event context.
-spec context_to_string( event_context() ) -> ustring().
context_to_string( #event_context{ id=Id, user_data=UserData,
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
