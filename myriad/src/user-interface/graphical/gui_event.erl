% Copyright (C) 2010-2024 Olivier Boudeville
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


% @doc Gathers all elements relative to the <b>MyriadGUI</b> events, including
% the event loop.
%
-module(gui_event).


% Usage notes:

% MyriadGUI can be used (with or without OpenGL) in two modes:
%
%  - either a "direct" mode, where the application has to define by itself a
%  main loop performing a selective receive of MyriadGUI user, lower-level
%  messages (for example {onWindowClosed, [ParentFrame, ParentWindowId,
%  EventContext]})
%
%  - or an "applicative" mode based on an application GUI state integrating
%  event drivers, whose role is to process the previous lower-level messages
%  into, possibly, higher-level, applicative ones (e.g. quit_requested, which
%  can be issued whenever the main frame is closed through the window manager,
%  when a Quit button is clicked, when specific keys like Escape of 'q' are
%  pressed, etc.); this applicative mode allows to support once for all services
%  that most applications may require, like the remapping of keys; it defines
%  its own GUI event loop, to be customised thanks to event drivers



% For the event_context record:
-include("gui_base.hrl").

% For canvas_state():
-include("gui_canvas.hrl").


% For wx headers:
-include("gui_internal_defines.hrl").


% For key/scan codes:
-include("ui_keyboard_keycodes.hrl").
-include("ui_keyboard_scancodes.hrl").


-type event_driver_table() :: table( event_type(), event_driver() ).
% A table associating an event type (like onWindowClosed) to an event driver, to
% which the full events of that type, with their elements (like {onWindowClosed,
% [Window, CloseContext]}), will be fed for processing.
%
% This allows MyriadGUI to provide default drivers, while letting the user code
% override them as needed.


-type event_element() :: gui_object() | backend_id() | event_context().
% One of the elements that could be sent by MyriadGUI when an event happened
% (together with the type of this event).
%
% For example the list in the {onWindowClosed, [WindowGUIObject, WindowId,
% EventContext]} event pair includes such event elements.


-type event_elements() :: [ event_element() ].
% Elements that could be sent by MyriadGUI when an event happened (together with
% the type of this event).
%
% For example the list in the {onWindowClosed, [WindowGUIObject, WindowId,
% EventContext]} event pair.


-type event_driver() :: fun( ( event_elements(), app_gui_state() ) ->
									app_event_return() ).
% A driver function in charge of processing a given type of MyriadGUI user
% event, returning out of it possibly an application event pair, and an updated
% GUI state.
%
% An event driver is thus relative to an (implicit) event_type(), such as
% onButtonClicked or onWindowClosed; example of a driver signature:
% my_onWindowClosed_driver(_EventElements=[WindowGUIObject, WindowId,
% EventContext], AppGUIState) -> ....
%
% It can be either a built-in, default event driver, or one defined by the
% application.


-type basic_event_table() :: table( basic_user_event(), application_event() ).
% A table allowing to translate a basic user event into an higher level
% application event.


% If found useful, in the future any button reference may be held by such a
% table: the match can be based either on a button() or, preferably, on a
% button_backend_id(); this last match is the first searched. No named_id() is
% expected there.
%
-type button_table() ::
	%table( button_backend_id() | button(), application_event() ).
	table( button_backend_id(), application_event() ).
% A table allowing to translate a widget button press event into an higher level
% application event.


-type scancode_table() :: table( scancode(), application_event() ).
% A table allowing to translate a key-as-scancode press event into an higher
% level application event.

-type keycode_table() :: table( keycode(), application_event() ).
% A table allowing to translate a key-as-keycode press event into an higher
% level application event.



-type gl_init_status() ::
	'uninitialised' % OpenGL enabled, GL canvas and context registered, but
					% the latter has not been bound on the former (typically
					% because the corresponding frame was not shown yet).
  | 'initialised'.  % OpenGL enabled and ready (GL context bound to GL canvas).
% The current OpenGL runtime status.


-type opengl_base_info() :: { gl_canvas(), gl_context() }.
% Base OpenGL information, typically needed when creating a GUI-related
% applicative state.


-type opengl_base_state() ::
	'disabled' % For non-OpenGL applications.
  | { gl_init_status(), gl_canvas(), gl_context() }.
% An OpenGL-related base state kept by the registry of event drivers.
%
% Stores, if available, the OpenGL canvas on which rendering will be done, and
% the associated OpenGL context.


-type app_specific_info() :: any().
% An arbitrary application-specific GUI information (typically a record) to be
% kept around, notably so that it can be used by the application-specific event
% drivers.
%
% Contains generally references to the widgets instantiated by the application
% (e.g. the main frame, buttons, etc.), and possibly OpenGL elements.


% For the app_gui_state and user_event_registry records:
-include("gui_event.hrl").


-type app_gui_state() :: #app_gui_state{}.
% A full, GUI-related applicative state to be kept around, notably so
% that it can be used by the event drivers.
%
% Tables translating lower-level user events into higher-level application
% events are stored there, among other information.
%
% For example, whether the user closes the main window, clicks on a 'Quit'
% button or presses a key with a specific scancode, a 'quit_requested'
% application event may have to be generated and processed.


-export_type([ event_driver_table/0, basic_event_table/0, button_table/0,
			   scancode_table/0, keycode_table/0,
			   gl_init_status/0, opengl_base_info/0,
			   app_specific_info/0, app_gui_state/0 ]).




% Type section.


% Event messages.
%
% Lower-level, backend-specific events are translated into MyriadGUI event
% messages, to be received by their respective event subscribers.



-type gui_event() :: { event_type(), event_elements() }.
% A (MyriadGUI) event is a pair whose first element is the event type, as an
% atom (e.g. 'onWindowClosed'), and whose second element is a list, whose first
% element is the GUI object that generated that event (the closed window, here),
% and whose last element is the event context (intermediary elements carrying
% event-specific information):
%
% {event_type(), [gui_object(), ..., event_context()]}
%
% For example {onWindowClosed, [Window, CloseContext]}, {onButtonClicked,
% [Button, ButtonId, Context]} etc.
%
%
% So the event context can be fetched with:
% EventContext = list_utils:get_last_element( Elements ),
%
% These values are sent as messages to the processes having subscribed to this
% type of event.
%
% Note: these messages respect the WOOPER conventions, and this is done on
% purpose, to facilitate any integration with upper layers.



% Thus an actual (non-opaque) wx:wx_object(), i.e. a #wx_ref record, for example
% {wx_ref,131,wxPaintEvent,[]}:
%
-type gui_event_object() :: wxEvent:wxEvent().
% A MyriadGUI object (therefore the reference to a full-blown backend process -
% not a mere datastructure like an event record received as a message) holding
% information about an event passed to a callback or member function.
%
% Unless explicitly trapped by such a function (see the 'trap_event'
% subscription option, or the gui:trap_event/1 function), most event types are
% propagated upward in the widget hierarchy.


-type instance_count() :: count().
% A count of instances of a given object type.


% Event management.
%
% In general we promote managing events thanks to messages rather than thanks to
% callbacks, as the former can access easily to the full context of the program
% whereas the latter is only executed into a transient process (less convenient,
% probably less efficient).

-type event_source() :: wx_event_handler() | myriad_event_handler().


-type event_context() :: #event_context{}.
% Context sent to corresponding subscribers together with an event.
%
% This context can be ignored in most cases.



-opaque wx_event_handler() :: wxEvtHandler:wxEvtHandler().



-type myriad_event_handler() :: gui_canvas:canvas().
% Only one currently.


% | ...
-type wx_event_type() ::
	% For windows:
	wx_repaint_event_type()
  | wx_click_event_type()
  | wx_resize_event_type()
  | wx_close_event_type()
  | wx_show_event_type()

	% For I/O:
  | wx_mouse_event_type()
  | wx_keyboard_event_type().
% Using the wx-event type, leaked by wx.hrl (enrich this union whenever needed).


-type wx_repaint_event_type() :: 'paint'.

% Toggle may be added:
-type wx_click_event_type() :: 'command_button_clicked'.

-type wx_resize_event_type() :: 'size'.

-type wx_close_event_type() :: 'close_window'
							 | 'end_session'
							 | 'query_end_session'.
% Associated to wxCloseEvent.

-type wx_show_event_type() :: 'show'.


-type wx_mouse_event_type() :: wxMouseEvent:wxMouseEventType().
% For left_down | left_up | middle_down, etc.

-type wx_keyboard_event_type() :: wxKeyEvent:wxKeyEventType().
% For char | char_hook | key_down | key_up.


-type event_type() :: command_event_type()
					| basic_event_type().
% A type of MyriadGUI event, independent from any backend.
%
% Unless specified otherwise, by default the events (actually: mostly the
% command ones) of a given type will propagate: subscribing to them does not
% preclude them from being sent also to the parent event handlers in the widget
% hierarchy.
%
% For some other, more basic, event types (e.g. onWindowClosed), they will be by
% default trapped (their events will not be propagated, so they will be
% processed only by the user event handler).
%
% For the event types that propagate by default, specifying the 'trap_event'
% subscription option, or calling the trap_event/1 function in one's event
% handler, will disable that propagation.
%
% Conversely, for the event types that are trapped by default, specifying the
% 'propagate_event' subscription option or calling the propagate_event/1
% function in one's event handler will enable that propagation.
%
% Note: if adding event types, consider updating get_trapped_event_types/0 as
% well.


-type command_event_type() ::

	% Typically when the mouse cursor enters a toolbar (hovering):
	'onToolbarEntered'

	% Typically when selecting (e.g. left-clicking) an item of a menu or a tool
	% of a toolbar:
	%
  | 'onItemSelected'

	% Typically when right-clicking on a tool of a toolbar:
  | 'onToolRightClicked'.
% A type of events emitted by commands, a variety of simple controls
% (e.g. buttons, menus, toolbars) or an actual window.
%
% By default these higher-level command events are propagated upward in the
% widget hierarchy, so that multiple handlers may manage them - unless a given
% handler chooses to trap them.
%
% See https://docs.wxwidgets.org/stable/classwx_command_event.html to better
% picture them.


-type basic_event_type() :: window_event_type()
						  | mouse_event_type()
						  | keyboard_event_type().
						  % | many_other_event_types()
% Basically the type of all non-command events.
%
% These lower-level events may be triggered by using the input devices (such as
% keyboard, mouse, joystick) directly.
%
% By default these lower-level events are not propagated in the widget
% hierarchy, as a single, user-defined handler usually suffices - unless a given
% handler chooses to propagate them explicitly.
%
% See https://docs.wxwidgets.org/stable/classwx_event.html to better picture
% them.


-type window_event_type() ::
	'onShown'
  | 'onRepaintNeeded'
  | 'onResized'
  | 'onButtonClicked'
  | 'onKeyPressed'

	% Trapped by default, to avoid race condition in termination procedures
	% between user handlers and backend ones:
	%
  | 'onWindowClosed'.
% A type of event possibly emitted by a window.
%
% Note that resizing a widget (typically a canvas) implies receiving also a
% onRepaintNeeded event; so a canvas may subscribe only to onRepaintNeeded (not
% necessarily to onResized).



-type event_subscription_option() ::

	% Disables any automatic event propagation:
	'trap_event'

	% Enables automatic event propagation:
  | 'propagate_event'.
% The options that can be specified whenever subscribing to a type of events.
%
% For most event types, by default, once an event has been processed by a
% user-defined handler, it is propagated upward in the widget hierarchy, so that
% further event handlers (including the built-in backend ones) can be triggered.
% It is usually necessary so that the GUI backend can update the other widgets
% accordingly (e.g. for proper resizes). Specifying the 'trap_event' option
% disable this automatic propagation; note that this may disable in turn key GUI
% update behaviours (such as the automatic resizing of widgets).
%
% Conversely, for the fewer event types for which by default no event
% propagation occurs, such a propagation to parent event handlers may be enabled
% by specifying the 'propagate_event' option.


-type user_pid() :: pid().
% The PID of a user calling process.


-type event_subscriber() :: naming_utils:local_designator().
% The PID or locally-registered name of an event subscriber.


-type event_subscription() ::

	{ maybe_list( event_type() ), maybe_list( gui_object() ) }
	% Default options and subscriber.

  | { maybe_list( event_type() ), maybe_list( gui_object() ),
	  maybe_list( event_subscription_opt() ) }
	% Default subscriber.

  | { maybe_list( event_type() ), maybe_list( gui_object() ),
	  maybe_list( event_subscription_opt() ),
	  maybe_list( event_subscriber() ) }.
% Describes, in the context of an event subscription, the type(s) of events
% generated by specific GUI object(s) to be listened to, with any relevant
% options, by which subscribers.
%
% To be specified so that user process(es) can subscribe to GUI events of
% interest.


-type event_subscription_spec() :: maybe_list( event_subscription() ).
% Specifies, for event subscribers (by default: the calling process), any
% combination of types of events and GUI objects that shall be listened to.


-type event_unsubscription() ::
	{ maybe_list( event_type() ), maybe_list( gui_object() ) }.
% So that user process(es) can unsubscribe from GUI events.

-type event_unsubscription_spec() :: maybe_list( event_unsubscription() ).
% Specifies, for an event subscriber (by default: the calling process), any
% combination of types of events and GUI objects to which it was subscribed yet
% that shall not be listened to anymore.


-type event_callback() ::
	fun( ( gui_event(), gui_event_object() ) -> void() ).
% A user-defined function to be called whenever an event occurred that
% corresponds to an already-registered GUI callback.
%
% It takes two parameters, an event tuple (whose content is typically used by
% the callback in order to process this event and act accordingly), and (the
% reference onto) an actual MyriadGUI object that corresponds to this event
% (that will typically be propagated upward in the widget hierarchy; see
% trap_event/1) to prevent this).


-type gui_wx_object_key() ::
	{ gui_wx_backend:wx_native_object_type(), gui_wx_backend:wx_id() }.
% A suitable stable key corresponding to a gui_object() (notably ignoring the
% last, 'state' element of this quadruplet).
%
% For example the gui_object() {wx_ref,63,wxFrame,AnyState} results in the
% {wxFrame,63} gui_wx_object_key() key.


-type myriad_object_key() :: { myriad_object_type(), myriad_instance_id() }.
% The MyriadGUI type corresponding to gui_wx_object_key/0.
%
% For example the myriad_object_ref() {myriad_object_ref,myr_canvas,12} results
% in the {myr_canvas,12} key.


-type gui_object_key() :: gui_wx_object_key() | myriad_object_key().
% Stable reference to a widget instance.
%
% This type has been introduced in order to benefit from more relevant keys for
% event tables: previously these keys were gui_object(), until more complex GUI
% uses shown that, after using wx_object:set_pid/2, a frame now known as
% `{wx_ref,63,wxFrame,<0.119.0>}' still generated events as
% `{wx_ref,63,wxFrame,[]}', so a better, stable identifier thereof is
% `{wxFrame,63}'.



-export_type([ event_source/0,
			   wx_event_handler/0, myriad_event_handler/0,

			   wx_event_type/0,
			   wx_repaint_event_type/0, wx_click_event_type/0,
			   wx_resize_event_type/0, wx_close_event_type/0,

			   event_type/0, event_subscription_option/0, user_pid/0,
			   event_subscriber/0,
			   event_subscription/0, event_subscription_spec/0,
			   event_unsubscription/0, event_unsubscription_spec/0,
			   event_callback/0,
			   gui_event_object/0, event_context/0,

			   gui_wx_object_key/0, myriad_object_key/0, gui_object_key/0 ]).



-type event_table() :: table( gui_object_key(), event_dispatch_table() ).
% An indirection table dispatching events according to subscription
% specifications.
%
% For an incoming event, we see this type (virtually, logically) as:
% table({gui_object(), event_type()}, set_utils:set(event_subscriber())):
%
% - the first key is the key corresponding to the GUI object (e.g. widget) from
% which the event emanates (e.g. a frame)
%
% - the second key is its corresponding (internal) event type (e.g.
% 'onWindowClosed')
%
% - the associated value is a list/set of the PID/name of the subscribers
% regarding this (object,event) combination
%
% Note: two nested tables (one table(), one list_table()) are used also in
% order to ensure that there is up to one entry per GUI object and per event
% type stored.


-type event_dispatch_table() ::
		list_table:list_table( event_type(), [ event_subscriber() ] ).
% Tells, for a given event type (e.g. in the context of a specific GUI object),
% to which event subscribers the corresponding GUI messages shall be sent.


-type reassign_table() ::
		table( SourceObject :: gui_object(), TargetObject :: gui_object() ).
% To replace actual source events objects (e.g. a panel) by others (e.g. its
% associated canvas, if any).
%
% For a given actual target object, a single source one must exist.
%
% Using a bijective_table could speed up the look-ups done when an instance is
% destructed.


-type myriad_type_table() ::
		table( myriad_object_type(), instance_referential() ).
% To store the MyriadGUI instances (sorted by types) and manage them like wx
% native objects.
%
% Keys are like 'myr_canvas'.



-type application_event() :: 'quit_requested'
						   | 'toggle_fullscreen'
						   | term().
% The higher-level, application events.


-type basic_user_event() :: 'window_closed'.
% A basic, atom-based user event.


-type user_event_spec() :: { 'button_clicked', button_id() }
						 | { 'scancode_pressed', scancode() }
						 | { 'keycode_pressed', keycode() }
						 | basic_user_event() .
% A specification of the various user-level events that should trigger
% application-level events.
%
% The snake_case (e.g. 'window_closed') is used rather than CamelCase
% (e.g. 'onWindowClosed') so that user-level events can be more easily
% distinguished from actual MyriadGUI events.
%
% The received event about key presses will be managed regardless of the
% (focused) widget that reports them.


-type application_event_spec() ::
	{ application_event(), [ user_event_spec() ] }.
% The specification of a conversion from any of the listed user events to the
% specified application event.


-type application_event_pair() :: { application_event(), gui_event() }.
% A pair made of a higher-level application event and, for extra information,
% the low-level GUI event that originated it.


-type app_event_return() ::
	{ maybe( application_event_pair() ), app_gui_state() }.
% Pair, together with an updated application GUI state, returned whenever a user
% event has been processed by a corresponding event driver and possibly been
% converted into an application event.


-export_type([ application_event/0, basic_user_event/0, user_event_spec/0,
			   application_event_spec/0, application_event_pair/0,
			   app_event_return/0 ]).


-record( instance_referential, {

	% Total count of the instances already created for that type:
	instance_count :: instance_count(),

	instance_table :: table( myriad_instance_id(), myriad_object_state() ) } ).


-type instance_referential() :: #instance_referential{}.
% To store, for a given MyriadGUI type (e. g. 'canvas'), all information about
% all instances.
%
% - a total count of the instances already created for that type
%
% - a table whose keys are the identifiers of the objects of that type, and
% whose values are the actual state of these instances.
%
% Note: the total count is not the same as the size of the table, as instances
% may be deleted.



% Stores the current MyriadGUI state, as managed by its main event loop.
-record( loop_state, {

	% Identifier of the current top-level wx server:
	wx_server :: wx_server(),


	% To dispatch appropriately the backend-originating events:
	event_table :: event_table(),


	% Allows to replace an event source by another.
	%
	% For example useful when having defined a canvas (which thus embeds a wx
	% panel): when the internal event loop receives a 'paint' wx event for that
	% wx panel, the actual object referred to by the GUI message that we will
	% send to the user code shall not be that panel, but the canvas that owns it
	% (for example so that other elements of that canvas can then be used when
	% the user code processes this event - like the bitmap or the back-buffer of
	% this canvas).
	%
	reassign_table :: reassign_table(),


	% Stores, by types, the current widget instances that have been introduced
	% by MyriadGUI to complement the backend (e.g. canvas instances).
	%
	type_table :: myriad_type_table(),


	% The precomputed set of event types that shall be trapped by default:
	trap_set :: trap_set(),


	id_next :: backend_id(),
	% The next backend identifier that will be allocated.

	id_name_alloc_table :: id_name_alloc_table()
	% A bijective table to convert between name identifiers and backend ones
	% (stored directly here, as the name allocation is directly managed by the
	% main loop, rather than by a dedicated gui_id allocator process).


	% List of the MyriadGUI objects that shall be adjusted after a show:
	%
	% (actually not found necessary, hence at least currently disabled)
	%
	%objects_to_adjust=[] :: [ myriad_object_ref() ]

} ).


-type loop_state() :: #loop_state{}.


-type backend_event() :: wx_event().
% A (supposedly opaque) backend GUI event.


-type wx_event() ::
	{ 'wx', wx_id(), wx_object(), gui:user_data(), wx_event_info() }.
% A wx_event record comprises:
%
% - (the 'wx' record tag, if the record instance is seen as a tuple)
%
% - id :: wx_id() the (integer) identifier of the object (e.g. widget) that
% received the event (event source)
%
% - obj :: wx_object() is the reference of the wx object that was specified
% in the connect/n call, i.e. on which connect/n was called (e.g.
% {wx_ref,35,wxFrame,[]})
%
% - userData :: user_data() is the user-specified data that was specified in the
% connect/n call (typically [], as not very useful)
%
% - event :: wx_event_info() is the description of the event itself
%
% As always, same as: -record( wx,...
%
% Note: not to be mixed up with wx:wxEvent(), which is a full-blown wx_object().


-type wx_event_info() :: wxClose() | wxCommand() | wxKey() | tuple().
% A wx-defined record describing an actual event.
%
% A WxFoobar-like record whose first field is its 'type', and which may have
% other fields, whose number and types depend on the event.
%
% Examples of descriptions, as tuples:
% - {wxClose, close_window}
% - {wxCommand, command_button_clicked, CmdString, CmdInt, ...}
% - {wxKey, char, 227, 139, 97,false, ...}


-type received_event() :: wx_event() | gui_event().
% A received event is either a backend one or a MyriadGUI one.


-type trap_set() :: set( [ event_type() ] ).
% A set of the event types that shall be trapped by default.


-export_type([ backend_event/0, wx_event/0, wx_event_info/0, trap_set/0 ]).





% Function export section.


% Main event primitives:
-export([ start_main_event_loop/3,
		  get_trapped_event_types/1, trap_event/1, propagate_event/1,
		  wx_to_myriad_event/1, get_event_info/1,
		  set_instance_state/3, match/2 ]).


% User events:
-export([ create_app_gui_state/1, create_app_gui_state/2,
		  create_app_gui_state/3,

		  set_event_driver/3, set_event_drivers/2,

		  default_onShown_driver/2, default_onRepaintNeeded_driver/2,
		  default_onResized_driver/2, default_onButtonClicked_driver/2,
		  default_onKeyPressed_driver/2, default_onWindowClosed_driver/2,

		  get_base_application_event_specs/0,

		  enable_opengl/3,
		  get_application_event/1,
		  get_maybe_application_event/1, get_maybe_application_event/2,
		  app_gui_state_to_string/1 ]).


% Helpers:
-export([ get_backend_event/1 ]).

% Stringification:
-export([ event_table_to_string/1, gui_event_to_string/1, context_to_string/1,
		  application_event_to_string/1 ]).

% To silence unused warnings:
-export([ get_subscribers_for/3, adjust_objects/4,
		  process_only_latest_repaint_event/4, reassign_table_to_string/1,
		  get_instance_state/2, type_table_to_string/1,
		  instance_referential_to_string/1, set_canvas_instance_state/3 ]).


% Wx-level:
-export([ to_wx_event_type/1, from_wx_event_type/1 ]).



% Implementation notes.

% So for at least most of the functions here are executed in the context of the
% process of the MyriadGUI main loop.


% Event propagation: this is presumably the most complex element to understand
% in a GUI.
%
% Events originate from a widget and are managed by any event handler found
% while climbing the widget hierarchy; generally the first handler triggered
% will handle the event (especially if it is an higher-level command event) and
% trap it (i.e. not propagate it further). Otherwise, typically if it is a basic
% event, it may propagate it (handler "skipped", the search for any handler
% continuing then), resulting in multiple handlers being possible triggered
% (useful for example to trigger a resize or a repaint of each).

% Refer to:
%  - https://docs.wxwidgets.org/stable/overview_events.html#overview_events_propagation
% for more details (see notably "How Events are Processed")
%  - https://docs.wxwidgets.org/stable/classwx_event.html to better discriminate
%  between event types (command or basic events)


% Events can be managed as messages or callbacks. We generally prefer the former
% (as messages can be selectively received, any context can be kept in the
% receive loop, no temporary process is created, no wx include is needed hence
% the backend can be well encapsulated, etc.).
%
% Whether an event shall be also dispatched to subsequent handlers may be
% decided by using trap_event/1.
%
% Event messages are internally converted, in order to hide the wx backend, to
% augment it with other primitives (e.g. canvas widget) and to make them
% compliant with the MyriadGUI conventions, as seen by the user code (hint:
% these conventions comply with the WOOPER ones, should the GUI be used in an
% OOP context).
%
% Regarding events, see also:
% https://wiki.wxwidgets.org/Events#Event.Skip_and_Event.Veto

% Please refer to wx.pdf (available on http://www.erlang.org/doc/apps/wx/wx.pdf)
% for more architecture/implementation details about wx.

% We used to rely on a separate process created for
% gui_id:embody_as_id_allocator/0 to manage identifiers, yet it is certainly
% more efficient to have them managed directly by this gui_event main loop (less
% messages involved for declarations/resolutions).


% Identifier/reference memento:
%  - an object type (gui:object_type()) is either a wx one (wx_object_type(),
%  like 'frame') or a Myriad one (myriad_object_type(), like 'myr_canvas'
%  - a Myriad instance identifier is just a positive integer (e.g. 12)
%  - GUI object keys are typically {wxFrame,63} or {myr_canvas,12} pairs
%  - Myriad object references are myriad_object_ref records, such as
%  {myriad_object_ref, myr_canvas, 12}


% Panel issues:
%
% There is a problem at least with panels: when they are just by themselves
% (created with no child widgets), when subscribing to key presses (e.g. as
% {onKeyPressed, TestPanel}), key press events are indeed received by the user
% event loop; yet, as soon as a button is created as a child of this panel (even
% without changing any event subscription), for some reason the panel will not
% send any key press event, and even by fiddling with event propagation /
% skipping / trapping, we could not change it.


% Shorthands:

-type count() :: basic_utils:count().
-type time_out() :: time_utils:time_out().

-type ustring() :: text_utils:ustring().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type set( T ) :: set_utils:set( T ).

-type gui_object() :: gui:gui_object().
-type myriad_object_type() :: gui:myriad_object_type().
-type myriad_object_state() :: gui:myriad_object_state().
-type construction_parameters() :: gui:construction_parameters().
-type wx_server() :: gui:wx_server().
-type event_subscription_opt() :: gui:event_subscription_opt().
-type service() :: gui:service().
-type button_backend_id() :: gui:button_backend_id().


-type button_id() :: gui_id:button_id().
-type backend_id() :: gui_id:backend_id().
-type wx_id() :: gui_id:wx_id().
-type id_name_alloc_table() :: gui_id:id_name_alloc_table().
-type instance_id() :: gui_id:instance_id().
-type myriad_instance_id() :: gui_id:myriad_instance_id().


-type keyboard_event_type() :: gui_keyboard:keyboard_event_type().
-type scancode() :: gui_keyboard:scancode().
-type keycode() :: gui_keyboard:keycode().

-type mouse_event_type() :: gui_mouse:mouse_event_type().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().


-type wx_object() :: gui:wx_object().
-type wx_env() :: wx:wx_env().





% Implementation section.



% @doc Starts the internal, main event loop of MyriadGUI.
%
% The backend events received will result in callbacks to be triggered on their
% respective subscribers.
%
% The goal is to devise a generic event loop, while still being able to be
% notified of all relevant information (and only them).
%
-spec start_main_event_loop( wx_server(), wx_env(), trap_set() ) -> no_return().
start_main_event_loop( WxServer, WxEnv, TrapSet ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( "[event] Will start the main MyriadGUI loop "
			"with wx server ~w, wx environment ~w, trap set ~p.",
			[ WxServer, WxEnv, TrapSet ] ) ),

	% Yet it can be, often preferably, reached through an environment:
	naming_utils:register_as( ?gui_event_loop_reg_name, _Scope=local_only ),

	% To be done first, so that we are able to use wx from that process from now
	% on:
	%
	gui:set_backend_environment( WxEnv ),

	EmptyTable = table:new(),

	InitialLoopState = #loop_state{
		wx_server=WxServer,
		event_table=EmptyTable,
		reassign_table=EmptyTable,
		type_table=EmptyTable,
		trap_set=TrapSet,
		id_next=gui_id:get_first_allocatable_id(),
		id_name_alloc_table=gui_id:get_initial_allocation_table() },

	% Increases the chances that this MyriadGUI main loop is not overwhelmed by
	% client calls:
	%
	erlang:process_flag( priority, _Level=high ),

	% To monitor overloading (will never be terminated):
	cond_utils:if_defined( myriad_debug_gui_performance,
		process_utils:spawn_message_queue_monitor( _MonitoredPid=self(),
			_MonitoredProcessDesc="MyriadGUI main loop" ) ),

	%trace_utils:debug_fmt( "[event] Starting main MyriadGUI loop." ] ),

	% Enter the infinite event loop:
	process_event_messages( InitialLoopState ).



% @doc Returns the set of event types that shall be trapped by default.
-spec get_trapped_event_types( [ service() ] ) -> trap_set().
get_trapped_event_types( Services ) ->

	% Precomputation, once for all, of the subset of event types that shall be
	% trapped (otherwise they may trigger parent-level handlers spuriously);
	% refer to the event_type() specification to understand why these specific
	% event types are trapped by default:

	WindowEventTypes = [ onButtonClicked, onWindowClosed ],

	CommandEventTypes =
		[ onToolbarEntered, onItemSelected, onToolRightClicked ],

	% Could/should be added: keyboard presses (see keyboard_event_type()).

	AllEventTypesToTrap = WindowEventTypes ++ CommandEventTypes
		++ case lists:member( mouse, Services ) of

			true ->
				gui_mouse:get_event_types_to_trap();

			false ->
				[]

		   end,

	% For faster lookup:
	TrapSet = set_utils:new( AllEventTypesToTrap ),

	%trace_utils:debug_fmt( "Trap set is: ~ts",
	%                       [ set_utils:to_string( TrapSet ) ] ),

	TrapSet.




% @doc Receives and process all messages (this is the actual MyriadGUI main
% event loop), coming:
%
% - either from controlling processes (typically from application processes
% subscribing to some events)
%
% - or from the (here, wx) backend, that notifies this loop of the actual,
% lower-level events
%
-spec process_event_messages( loop_state() | 'terminated' ) -> no_return().
process_event_messages( terminated ) ->
	trace_utils:debug( "Main MyriadGUI loop terminated." ),
	terminated;

process_event_messages( LoopState ) ->
% To check sooner:
%process_event_messages( LoopState=#loop_state{} ) ->

	cond_utils:if_defined( myriad_debug_gui_repaint_logic,
		trace_utils:debug_fmt( "[event] GUI main loop ~w waiting "
							   "for event messages...", [ self() ] ) ),

	cond_utils:if_defined( myriad_debug_gui_id,
		trace_utils:debug_fmt( "Name table: ~ts", [ bijective_table:to_string(
			LoopState#loop_state.id_name_alloc_table ) ] ) ),

	% Special management of repaint requests, to avoid useless repaintings.
	%
	% Indeed, even if having registered (with wxEvtHandler:connect/3) a panel
	% only once, at least in some cases, when resizing, we notice that we
	% receive the following event *twice*:
	% {wx,-2017,{wx_ref,56,wxPanel,[]},[],{wxPaint,paint}}.
	%
	% Our dropping logic allows to repaint only once in that case.
	%
	NewLoopState = cond_utils:if_defined( myriad_gui_skip_extra_repaints,

		% If skipping is allowed:
		receive

			% So that no large series of repaint requests for the same object
			% pile up:
			%
			FirstWxRepaintEvent=#wx{ obj=SourceObject,
									 event={ wxPaint, paint } } ->

				cond_utils:if_defined( myriad_debug_gui_repaint_logic,
					trace_utils:debug_fmt(
						"[event] Received first repaint event:~n ~w.",
						[ FirstWxRepaintEvent ] ) ),

				process_only_latest_repaint_event( FirstWxRepaintEvent,
					SourceObject, _DropCount=0, LoopState );

			OtherEvent ->
				cond_utils:if_defined( myriad_debug_gui_repaint_logic,
					trace_utils:debug_fmt( "[event] Received other event: ~w.",
										   [ OtherEvent ] ) ),
				process_event_message( OtherEvent, LoopState )

		end,

		% If skipping is not allowed, bypasses the "smarter" management above,
		% for test/comparison purpose:
		%
		receive

			AnyEvent ->
				cond_utils:if_defined( myriad_debug_gui_repaint_logic,
					trace_utils:debug_fmt( "[event] Received any event:~n ~w.",
										   [ AnyEvent ] ) ),
				process_event_message( AnyEvent, LoopState )

		end ),

	process_event_messages( NewLoopState ).




% Event types roughly sorted in clauses by decreasing frequency of appearance:
%
% (defined in lib/wx/include/wx.hrl)
%
% Note: using a (complete) wx_to_myriad_event/1 function would be better than
% relying on functions, as the event callbacks would use the same code (a wx
% event is passed to callbacks, we want them to be feed a MyriadGUI event
% instead).



% @doc Processes the specified GUI event from the current (wx) backend.
%
% A *wx* (backend) event has been received here, in this first clause:
%
% Structure: {wx, EventSourceId, Obj, UserData, EventInfo }, with EventInfo:
% {WxEventName, EventType, ...}
%
% For example {wx, -2006, {wx_ref,35,wxFrame,[]}, [], {wxClose,close_window}}.
%
-spec process_event_message( received_event(), loop_state() ) -> loop_state().
process_event_message( WxEvent=#wx{ id=EventSourceId, obj=GUIObject,
									userData=UserData, event=WxEventInfo },
					   LoopState ) ->

	cond_utils:if_defined( myriad_debug_gui_repaint_logic,
		trace_utils:debug_fmt( "[event] Received wx event ~w.", [ WxEvent ] ) ),

	process_wx_event( EventSourceId, GUIObject, UserData, WxEventInfo,
					  WxEvent, LoopState );


% Here the MyriadGUI main process impersonates a standalone gui_id server:

% Special case: when creating a widget instance (e.g. a frame) while specifying
% its name, the GUI loop must be notified so that it stores this name,
% associates it to a new backend identifier (wx_id()) and returns it to the
% sender for its upcoming, corresponding wx creation call.
%
% Note that, at least currently, this MyriadGUI main loop supports only a subset
% of the message types that a real identifier allocator (see gui_id) would
% support.
%
process_event_message( { declareNameIdentifier, NameId, SenderPid },
		LoopState=#loop_state{ id_next=NextId,
							   id_name_alloc_table=NameTable } ) ->

	{ AllocatedId, NewNextId, NewNameTable } =
		gui_id:declare_name_id_internal( NameId, NextId, NameTable ),

	%trace_utils:debug_fmt( "Allocated to name '~ts': ~ts.",
	%                       [ NameId, gui_id:id_to_string( AllocatedId ) ] ),

	SenderPid ! { notifyDeclaredNameIdentifier, AllocatedId },

	LoopState#loop_state{ id_next=NewNextId,
						  id_name_alloc_table=NewNameTable };


process_event_message( { resolveNameIdentifier, NameId, SenderPid },
		LoopState=#loop_state{ id_name_alloc_table=NameTable } ) ->

	BackendId = gui_id:resolve_named_id_internal( NameId, NameTable ),

	SenderPid ! { notifyResolvedNameIdentifier, BackendId },

	LoopState;


process_event_message( { resolveBackendIdentifier, BackendId, SenderPid },
		LoopState=#loop_state{ id_name_alloc_table=NameTable } ) ->

	MaybeNameId =
			case gui_generated:get_maybe_first_for_button_id( BackendId ) of

		undefined ->
			case gui_generated:get_maybe_first_for_menu_item_id(
					BackendId ) of

				undefined ->
					bijective_table:get_maybe_first_for( BackendId, NameTable );

				NameIdFromMenu ->
					NameIdFromMenu

			end;

		NameIdFromButton ->
			NameIdFromButton

	end,

	SenderPid ! { notifyResolvedBackendIdentifier, MaybeNameId },

	LoopState;


% From now, the event messages received are *MyriadGUI* ones, i.e. gui_event()
% (either internal or user-emanating), roughly sorted by decreasing expected
% calling frequency:
%
% (some operations directly impact the canvas state as seen from MyriadGUI,
% others, like draw operations, not; they impact only the state of backend
% objects, thus references on them in CanvasState and thus LoopState can be kept
% as are)
%
% So, from here, CanvasId is in the form of {myriad_object_ref,myr_canvas,12}
% (and thus no name_id() conversion makes sense):
%
process_event_message( { setCanvasDrawColor, [ CanvasId, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->

	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:set_draw_color_impl( CanvasState, Color ),
	LoopState;


process_event_message( { setCanvasFillColor, [ CanvasId, MaybeColor ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:set_fill_color_impl( CanvasState, MaybeColor ),
	LoopState;

process_event_message( { setCanvasBackgroundColor, [ CanvasId, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->

	%trace_utils:debug_fmt( "Canvas: ~p", [ Canvas ] ),
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),

	%trace_utils:debug_fmt( "CanvasState: ~p", [ CanvasState ] ),
	gui_canvas:set_background_color_impl( CanvasState, Color ),
	LoopState;

process_event_message( { getCanvasRGBA, [ CanvasId, Point2 ], CallerPid },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	Color = gui_canvas:get_rgba_impl( CanvasState, Point2 ),
	CallerPid ! { notifyCanvasRGBA, Color },
	LoopState;

process_event_message( { setCanvasRGBA, [ CanvasId, Point ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:set_rgba_impl( CanvasState, Point ),
	LoopState;

process_event_message( { drawCanvasLine, [ CanvasId, P1, P2 ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->

	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_line_impl( CanvasState, P1, P2 ),
	LoopState;

process_event_message( { drawCanvasLine, [ CanvasId, P1, P2, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_line_impl( CanvasState, P1, P2, Color ),
	LoopState;

process_event_message( { drawCanvasLines, [ CanvasId, Points ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_lines_impl( CanvasState, Points ),
	LoopState;

process_event_message( { drawCanvasLines, [ CanvasId, Points, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_lines_impl( CanvasState, Points, Color ),
	LoopState;

process_event_message( { drawCanvasSegment, [ CanvasId, _Points ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	_CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	throw( not_implemented ),
	% 2 missing arguments in:
	%gui_canvas:draw_segment_impl( CanvasState, Points ),
	%gui_canvas:draw_segment_impl(canvas_state(), line2(), coordinate(),
	% coordinate()),

	LoopState;

process_event_message( { drawCanvasPolygon, [ CanvasId, Points ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_polygon_impl( CanvasState, Points ),
	LoopState;

process_event_message( { drawCanvasLabel, [ CanvasId, Point, Label ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_label_impl( CanvasState, Point, Label ),
	LoopState;

process_event_message( { drawCanvasCross, [ CanvasId, Location ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_cross_impl( CanvasState, Location ),
	LoopState;

process_event_message( { drawCanvasCross, [ CanvasId, Location, EdgeLength ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_cross_impl( CanvasState, Location, EdgeLength ),
	LoopState;

process_event_message( { drawCanvasCross,
							[ CanvasId, Location, EdgeLength, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_cross_impl( CanvasState, Location, EdgeLength, Color ),
	LoopState;

process_event_message( { drawCanvasLabelledCross,
							[ CanvasId, Location, EdgeLength, LabelText ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_labelled_cross_impl( CanvasState, Location, EdgeLength,
										 LabelText ),
	LoopState;

process_event_message( { drawCanvasLabelledCross,
		[ CanvasId, Location, EdgeLength, Color, LabelText ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_labelled_cross_impl( CanvasState, Location, EdgeLength,
										 Color, LabelText ),
	LoopState;

process_event_message( { drawCanvasCircle, [ CanvasId, Center, Radius ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_circle_impl( CanvasState, Center, Radius ),
	LoopState;

process_event_message( { drawCanvasCircle,
		[ CanvasId, Center, Radius, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_circle_impl( CanvasState, Center, Radius, Color ),
	LoopState;

process_event_message( { drawCanvasNumberedPoints, [ CanvasId, Points ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_numbered_points_impl( CanvasState, Points ),
	LoopState;

process_event_message( { loadCanvasImage, [ CanvasId, Filename ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	% Canvas state is const (just an update of the back-buffer):
	gui_canvas:load_image_impl( CanvasState, Filename ),
	LoopState;

process_event_message( { loadCanvasImage, [ CanvasId, Position, Filename ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:load_image_impl( CanvasState, Position, Filename ),
	LoopState;

process_event_message( { resizeCanvas, [ CanvasId, NewSize ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->

	TypeTable = LoopState#loop_state.type_table,

	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),

	NewCanvasState = gui_canvas:resize_impl( CanvasState, NewSize ),

	%trace_utils:debug_fmt( "NewCanvasState = ~p", [ NewCanvasState ] ),

	NewTypeTable = set_canvas_instance_state( CanvasId, NewCanvasState,
											  TypeTable ),

	LoopState#loop_state{ type_table=NewTypeTable };

process_event_message( { blitCanvas, CanvasId },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:blit_impl( CanvasState ),
	LoopState;

process_event_message( { clearCanvas, CanvasId },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:clear_impl( CanvasState ),
	LoopState;

process_event_message( { setTooltip, [ CanvasId, Label ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_widget:set_tooltip( CanvasState#canvas_state.panel, Label ),
	LoopState;

process_event_message( { getPanelForCanvas, CanvasId, CallerPid },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	CallerPid ! { notifyCanvasPanel, CanvasState#canvas_state.panel },
	LoopState;

process_event_message( { getCanvasSize, CanvasId, CallerPid },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	Size = gui_canvas:get_size_impl( CanvasState ),
	CallerPid ! { notifyCanvasSize, Size },
	LoopState;

process_event_message( { getCanvasClientSize, CanvasId, CallerPid },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	Size = gui_canvas:get_client_size_impl( CanvasState ),
	CallerPid ! { notifyCanvasClientSize, Size },
	LoopState;


% MyriadGUI user requests (e.g. emanating from gui_canvas:create/1):

process_event_message( { createInstance, [ ObjectType, ConstructionParams ],
						 CallerPid }, LoopState ) ->
	process_myriad_creation( ObjectType, ConstructionParams, CallerPid,
							 LoopState );


process_event_message( { destructInstance, [ ObjectType, InstanceId ] },
					   LoopState ) ->
	process_myriad_destruction( ObjectType, InstanceId, LoopState );


process_event_message( { subscribeToEvents,
		[ SubscribedEvents, SubscriberDesignator ], SenderPid }, LoopState ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( "[event] Subscribing by ~w of process ~w "
			"to events ~w.",
			[ SenderPid, SubscriberDesignator, SubscribedEvents ] ) ),

	NewLoopState = register_in_event_loop_tables( SubscribedEvents,
		SubscriberDesignator, LoopState ),

	% Now synchronous to avoid race conditions:
	SenderPid ! onEventSubscriptionProcessed,
	NewLoopState;


process_event_message( { unsubscribeFromEvents,
		[ UnsubscribedEvents, SubscribedPid ], SenderPid }, LoopState ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( "[event] Unsubscribing by ~w of process ~w "
			"from events ~w.",
			[ SenderPid, SubscribedPid, UnsubscribedEvents ] ) ),

	NewLoopState = unregister_from_event_loop_tables( UnsubscribedEvents,
		SubscribedPid, LoopState ),

	% Now synchronous to avoid race conditions:
	SenderPid ! onEventUnsubscriptionProcessed,
	NewLoopState;


% Synchronous no-op action, allowing the caller to know that all pending
% operations that may have been received by this main loop prior to this message
% have been processed (e.g. useful for safe, coordinated shutdown/widget
% destruction, to avoid any operation to try to access to resources that are not
% available anymore):
%
% (see also gui:sync/1)
%
process_event_message( { synchroniseWithCaller, [ ], SenderPid }, LoopState ) ->
	SenderPid ! onSynchronisedWithCaller,
	LoopState;


% To account for example for a silently-resized inner panel of a canvas:
%
% (done only once, initially; finally useless):
%
%process_event_message( { adjustObject, ObjectRef }, LoopState ) ->
%
%   trace_utils:debug_fmt( "Recording object to adjust: ~w.", [ ObjectRef ] ),
%
%   NewAdjustList = [ ObjectRef | LoopState#loop_state.objects_to_adjust ],
%
%   % To test the bypass of this mechanism:
%   %NewAdjustList = LoopState#loop_state.objects_to_adjust,
%
%   LoopState#loop_state{ objects_to_adjust=NewAdjustList };


% Currently we update widgets regardless of whether one of their parent windows
% is reported here as shown:
%
%process_event_message( { onShown, [ _Windows ] }, LoopState ) ->

	%ObjectsToAdjust = LoopState#loop_state.objects_to_adjust,

	%trace_utils:debug_fmt( "Adjusting after show: ~p.", [ ObjectsToAdjust ] ),

	%EventTable = LoopState#loop_state.event_table,

	%NewTypeTable = adjust_objects( ObjectsToAdjust, EventTable,
	%                               LoopState#loop_state.type_table ),

	% Purged:
	%LoopState#loop_state{ type_table=NewTypeTable, objects_to_adjust=[] };

process_event_message( terminate_gui_loop, _LoopState ) ->
	%trace_utils:debug( "Main MyriadGUI loop terminating." ),
	terminated;

process_event_message( UnmatchedEvent, LoopState ) ->
	trace_utils:warning_fmt( "Ignored following unmatched event "
							 "message:~n~p", [ UnmatchedEvent ] ),
	LoopState.



% @doc Drops all intermediate repaint events, and processes the last one, and
% then the next non-repaint event.
%
-spec process_only_latest_repaint_event( wx_event(), wx_object(), count(),
										 loop_state() ) -> loop_state().
process_only_latest_repaint_event( CurrentWxRepaintEvent, SourceObject,
								   DropCount, LoopState ) ->

	receive

		% Ignores all repaints applying to specified object of a series, except
		% the last:
		%
		NewWxRepaintEvent=#wx{ obj=SourceObject, event={wxPaint,paint} } ->

			cond_utils:if_defined( myriad_debug_gui_repaint_logic,
				trace_utils:debug_fmt( "[event] Dropping last repaint event "
					"received in favor of newer one:~n ~p.",
					[ NewWxRepaintEvent ] ) ),

			process_only_latest_repaint_event( NewWxRepaintEvent, SourceObject,
											   DropCount+1, LoopState );


		OtherEvent ->

			% End of a series of repaints; thus process the last one we got:

			#wx{ id=EventSourceId, obj=GUIObject, userData=UserData,
				 event=WxEventInfo } = CurrentWxRepaintEvent,

			% By design this is a wx (repaint) event:
			PostRepaintLoopState = process_wx_event( EventSourceId, GUIObject,
				UserData, WxEventInfo, CurrentWxRepaintEvent, LoopState ),

			PostRepaintLoopState =:= undefined
				andalso throw( faulty_loop_state_1 ),

			cond_utils:if_defined( myriad_debug_gui_repaint_logic,
				case DropCount of

					0 ->
						trace_utils:debug( "[event] (no drop)" );
						%throw( no_drop );

					1 ->
						trace_utils:debug( "[event](single drop)" );

					_ ->
						trace_utils:debug_fmt( "[event] Received post-repaint "
							"event after ~B drops:~n ~w.",
							[ DropCount, OtherEvent ] )
						%throw( { drop_count, DropCount } )

				end ),

			% And then process the first non-repaint event that was just
			% received:
			%
			process_event_message( OtherEvent, PostRepaintLoopState )

	% We should not delay arbitrarily the processing of a unique repaint event,
	% so we time-out shortly:
	%
	%after 5 ->
	after 0 ->

		cond_utils:if_defined( myriad_debug_gui_repaint_logic,
			case DropCount of

				0 ->
					trace_utils:debug( "[event] (no time-out drop)" );
					%throw( no_drop_time_out );

				1 ->
					trace_utils:debug( "[event] (single time-out drop)" );

				_ ->
					trace_utils:debug_fmt( "[event] Timed-out after ~B drops.",
										   [ DropCount ] )
					%throw( { drop_count, DropCount } )

			end ),


		#wx{ id=EventSourceId, obj=GUIObject, userData=UserData,
			 event=WxEventInfo } = CurrentWxRepaintEvent,

		% By design this is a wx (repaint) event:
		process_wx_event( EventSourceId, GUIObject, UserData, WxEventInfo,
						  CurrentWxRepaintEvent, LoopState )

	end.



% @doc Processes the specified wx event message.
-spec process_wx_event( wx_id(), wx_object(), gui:user_data(),
		wx_event_info(), wx_event(), loop_state() ) -> loop_state().
process_wx_event( EventSourceId, GUIObject, UserData, WxEventInfo, WxEvent,
				  LoopState=#loop_state{
					event_table=EventTable,
					reassign_table=ReassignTable,
					type_table=TypeTable,
					id_name_alloc_table=NameTable } ) ->

	% The backend indentifier EventSourceId is kept as is: an attempt of
	% promoting to a named identifier will be done in send_event/7.

	%trace_utils:debug_fmt(
	%   "Processing wx event from ~w:~n  ~p.", [ EventSourceId, WxEvent ] ),

	% Reassigns this event and possibly updates its actual target:
	{ ActualGUIObject, NewTypeTable } =
			case table:lookup_entry( GUIObject, ReassignTable ) of

		key_not_found ->
			%trace_utils:debug_fmt( "Wx event received about '~ts' "
			%   "(no reassignment needed):~n~p.",
			%   [ gui:object_to_string( GUIObject ), WxEventInfo ] ),
			{ GUIObject, TypeTable };

		{ value, TargetGUIObject } ->
			cond_utils:if_defined( myriad_debug_gui_events,
				trace_utils:debug_fmt( "Wx event received about '~ts', "
					"reassigned to '~ts':~n~p.",
					[ gui:object_to_string( GUIObject ),
					  gui:object_to_string( TargetGUIObject ),
					  WxEventInfo ] ) ),

			% Before notifying the event subscribers below, some special actions
			% may be needed to update that target reassigned object first (e.g.
			% for a canvas, an onResized event shall trigger first an update of
			% the canvas back-buffer):
			%
			UpdatedTypeTable = update_instance_on_event( TargetGUIObject,
				WxEventInfo, TypeTable ),

			{ TargetGUIObject, UpdatedTypeTable }

	end,

	ObjectKey = get_key_from_object( ActualGUIObject ),

	NewLoopState = LoopState#loop_state{ type_table=NewTypeTable },

	% Then notify the user-defined subscribers, if any (could use
	% get_subscribers_for/3):
	%
	case table:lookup_entry( ObjectKey, EventTable ) of

		key_not_found ->
			% At least one subscriber would be expected:
			trace_utils:warning_fmt( "No event subscription for GUI "
				"object '~ts' (i.e. ~w, whose key is ~w), which is abnormal, "
				"thus ignoring event; event table is ~ts.",
				[ gui:object_to_string( ActualGUIObject ), ActualGUIObject,
				  ObjectKey, table:to_string( EventTable ) ] );

		{ value, DispatchTable } ->

			% Example: WxEventType=close_window (the first element being the
			% record name, such as 'wxClose').
			%
			WxEventType = element( 2, WxEventInfo ),

			% Converting to a MyriadGUI event to look-up any subscribers:
			EventType = from_wx_event_type( WxEventType ),

			% We could try here to convert any backend identifier in the event
			% term into a MyriadGUI (atom) identifier.

			case list_table:lookup_entry( EventType, DispatchTable ) of

				key_not_found ->
					trace_utils:warning_fmt( "Received event of type '~ts' "
						"(i.e. ~w) from ~w (originally ~w, i.e. ~w) that has "
						"no dispatch entry.",
						[ EventType, WxEventType, ActualGUIObject, GUIObject,
						  EventSourceId ] );

				{ value, _Subscribers=[] } ->
					% Such entry should have been removed as a whole instead:
					trace_utils:error_fmt( "For GUI object '~ts', event type "
						"'~ts' had an empty list of subscribers).",
						[ gui:object_to_string( ActualGUIObject ),
						  EventType ] );

				{ value, Subscribers } ->
					cond_utils:if_defined( myriad_debug_gui_events,
						trace_utils:debug_fmt( "Dispatching ~p event to "
							"subscribers ~w.", [ EventType, Subscribers ] ) ),

					send_event( Subscribers, EventType, EventSourceId,
								ActualGUIObject, UserData, WxEvent, NameTable )

			end

	end,

	NewLoopState.



% @doc Updates specified GUI object (probably a MyriadGUI one, like a canvas)
% after specified event (e.g. an onResized one) has been received.
%
-spec update_instance_on_event( gui_object(), wx_event_info(),
								myriad_type_table() ) -> myriad_type_table().
update_instance_on_event(
		_GuiObject={ myriad_object_ref, myr_canvas, CanvasId },
		WxEventInfo, TypeTable ) ->

	case _WxEventType=element( 2, WxEventInfo ) of

		% A canvas must be updated internally when resized:
		size ->
			NewSize = WxEventInfo#wxSize.size,

			CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),

			NewCanvasState = gui_canvas:resize_impl( CanvasState, NewSize ),

			set_canvas_instance_state( CanvasId, NewCanvasState, TypeTable );

		OtherWxEventType ->
			cond_utils:if_defined( myriad_debug_gui_canvas,
				trace_utils:debug_fmt(
					"No canvas update to be done for event '~ts'.",
					[ OtherWxEventType ] ),
				basic_utils:ignore_unused( OtherWxEventType ) ),
			TypeTable

	end;

update_instance_on_event( GuiObject, WxEventInfo, TypeTable ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( "No specific update needed for GUI object ~w "
			"regarding event information ~p.", [ GuiObject, WxEventInfo ] ),
		basic_utils:ignore_unused( [ GuiObject, WxEventInfo ] ) ),

	TypeTable.



% @doc Returns the subscribers (if any) to the specified GUI object, for the
% specified event type.
%
-spec get_subscribers_for( gui_object(), event_type(), event_table() ) ->
									[ event_subscriber() ].
get_subscribers_for( GUIObject, EventType, EventTable ) ->

	ObjectKey = get_key_from_object( GUIObject ),

	case table:lookup_entry( ObjectKey, EventTable ) of

		key_not_found ->
			[];

		{ value, DispatchTable } ->

			case list_table:lookup_entry( EventType, DispatchTable ) of

				key_not_found ->
					[];

				{ value, Subscribers } ->
					Subscribers

			end

	end.



% @doc Creates the specified MyriadGUI object.
-spec process_myriad_creation( myriad_object_type(),
	construction_parameters(), user_pid(), loop_state() ) -> loop_state().
process_myriad_creation( ObjectType, ConstructionParams, CallerPid,
						 LoopState=#loop_state{ reassign_table=ReassignTable,
												type_table=TypeTable } ) ->

	%trace_utils:debug_fmt( "Myriad instance creation request received from ~w,"
	%    " for type '~ts', with construction parameters ~w.",
	%    [ CallerPid, ObjectType, ConstructionParams ] ),

	case ObjectType of

		myr_canvas ->

			{ CanvasInitialState, PanelRef } =
				gui_canvas:create_instance( ConstructionParams ),

			{ CanvasRef, NewTypeTable } =
				register_instance( ObjectType, CanvasInitialState, TypeTable ),

			CallerPid ! { instance_created, ObjectType, CanvasRef },

			% An event about its (already connected by
			% gui_canvas:create_instance/1) parent panel must be reassigned to
			% its canvas:
			%
			NewReassignTable =
				table:add_new_entry( PanelRef, CanvasRef, ReassignTable ),

			%trace_utils:debug_fmt( "Events sent to panel ~w will be from now "
			%    "reassigned to canvas ~w.", [ PanelRef, CanvasRef ] ),

			% Last step is to have the canvas subscribe to these events:

			% After this canvas is created, its panel, when finally shown, will
			% be resized, with no specific notification being propagated; to
			% adapt that canvas (otherwise it will be never be set at the right
			% initial size), we post ourselves an event, the first one to be
			% handled by our event loop:
			%
			%self() ! { adjustObject, CanvasRef },

			LoopState#loop_state{ reassign_table=NewReassignTable,
								  type_table=NewTypeTable };


		UnexpectedType ->
			trace_utils:error_fmt( "'~ts' is not a known MyriadGUI type.",
								   [ UnexpectedType ] ),

			throw( { unexpected_myriad_type, UnexpectedType } )

	end.



% @doc Destructs the specified MyriadGUI object.
-spec process_myriad_destruction( myriad_object_type(), instance_id(),
								  loop_state() ) -> loop_state().
process_myriad_destruction( ObjectType, InstanceId,
							LoopState=#loop_state{ reassign_table=ReassignTable,
												   type_table=TypeTable } ) ->

	%trace_utils:debug_fmt( "Myriad instance destruction request received, "
	%    "for type '~ts', instance identifier #~B.",
	%    [ ObjectType, InstanceId ] ),

	{ InstanceState, ShrunkTypeTable } =
		unregister_instance( ObjectType, InstanceId, TypeTable ),

	ShrunkReassignTable = case ObjectType of

		myr_canvas ->
			#canvas_state{ panel=Panel } = InstanceState,

			 gui_canvas:destruct_instance( InstanceState ),

			% This panel is associated to this canvas:
			table:remove_entry( Panel, ReassignTable );

		UnexpectedType ->
			trace_utils:error_fmt( "'~ts' is not a known MyriadGUI type.",
								   [ UnexpectedType ] ),

			throw( { unexpected_myriad_type, UnexpectedType } )

	end,

	% No interest found in making it synchronous, no message sent.

	LoopState#loop_state{ reassign_table=ShrunkReassignTable,
						  type_table=ShrunkTypeTable }.



% @doc Registers the creation of a MyriadGUI instance of the specified type and
% initial state, in the specified instance table.
%
-spec register_instance( myriad_object_type(), myriad_object_state(),
		myriad_type_table() ) -> { myriad_object_ref(), myriad_type_table() }.
register_instance( ObjectType, ObjectInitialState, TypeTable ) ->

	%trace_utils:info_fmt( "Registering a MyriadGUI instance of type '~ts', "
	%    "of following state:~n~p.", [ ObjectType, ObjectInitialState ] ),

	{ NewInstanceId, NewInstanceReferential } =
			case table:lookup_entry( ObjectType, TypeTable ) of

		key_not_found ->

			% First instance of its type:
			FirstInstanceId = 1,

			FirstInstanceTable =
				table:singleton( FirstInstanceId, ObjectInitialState ),

			FirstInstanceReferential = #instance_referential{
				instance_count=1,
				instance_table=FirstInstanceTable },

			{ FirstInstanceId, FirstInstanceReferential };


		{ value, InstanceReferential=#instance_referential{
				instance_count=InstanceCount,
				instance_table=InstanceTable } } ->

			NextInstanceId = InstanceCount + 1,

			NextInstanceTable = table:add_entry( NextInstanceId,
				ObjectInitialState, InstanceTable ),

			NextInstanceReferential = InstanceReferential#instance_referential{
				instance_count=NextInstanceId,
				instance_table=NextInstanceTable },

			{ NextInstanceId, NextInstanceReferential }


	end,

	% Prepares the reference onto this new instance:
	MyriadRef = #myriad_object_ref{ object_type=ObjectType,
									myriad_instance_id=NewInstanceId },

	NewTypeTable =
		table:add_entry( ObjectType, NewInstanceReferential, TypeTable ),

	{ MyriadRef, NewTypeTable }.



% @doc Unregisters the specified MyriadGUI instance of the specified type from
% the specified instance table.
%
-spec unregister_instance( myriad_object_type(), instance_id(),
		myriad_type_table() ) -> { myriad_object_state(), myriad_type_table() }.
unregister_instance( ObjectType, InstanceId, TypeTable ) ->

	%trace_utils:info_fmt( "Unregistering the MyriadGUI instance of "
	%   "id #~B and type '~ts'.", [ InstanceId, ObjectType ] ),

	{ InstanceState, NewInstReferential } =
			case table:lookup_entry( ObjectType, TypeTable ) of

		key_not_found ->
			throw( { invalid_object_type_to_unregister, ObjectType } );

		{ value, InstanceReferential=#instance_referential{
				instance_count=InstanceCount,
				instance_table=InstanceTable } } ->

			case table:extract_entry_if_existing( _K=InstanceId,
												  InstanceTable ) of

				{ InstState, ShrunkInstTable } ->
					{ InstState,
					  InstanceReferential#instance_referential{
						instance_count=InstanceCount-1,
						instance_table=ShrunkInstTable } };

				false ->
					throw( { instance_to_unregister_not_found, InstanceId,
							 ObjectType } )

		end

	end,

	NewTypeTable = table:add_entry( ObjectType, NewInstReferential, TypeTable ),

	{ InstanceState, NewTypeTable }.



% @doc Sends the specified MyriadGUI event to the relevant subscribers.
%
% Refer to gui:subscribe_to_events/1 for a description of the messages to be
% sent to subscribers.
%
% In all cases we keep a raw event context (hence with backend identifiers), but
% update the event source identifier so that if possible it becomes a named
% identifier.
%
% (helper)
%
-spec send_event( [ event_subscriber() ], event_type(), backend_id(),
			gui_object(), gui:user_data(), gui:backend_event(),
			id_name_alloc_table() ) -> void().
send_event( _Subscribers=[], _EventType, _EventSourceId, _GUIObject, _UserData,
			_Event, _NameTable ) ->
	ok;

% Special cases first, when the information to return to the user process is to
% be specifically adapted.
%
% Window-specific clause; for resizing, wanting to have the new size directly
% available in the message sent back, so that there is no need to search the
% backend event for that information:
%
send_event( Subscribers, EventType=onResized, EventSourceId, GUIObject,
			UserData, Event, NameTable ) ->

	BestSrcId = gui_id:get_best_id_internal( EventSourceId, NameTable ),

	Context = #event_context{ id=EventSourceId, user_data=UserData,
							  backend_event=Event },

	% Making the new size readily available:

	WxEventInfo = Event#wx.event,

	% Defined in wx.hrl:
	NewSize = WxEventInfo#wxSize.size,

	%trace_utils:debug_fmt( "onResized event: new size is ~p.", [ NewSize ] ),

	% Same structure as for OpenGL canvases:
	Msg = { EventType, [ GUIObject, BestSrcId, NewSize, Context ] },

	%trace_utils:debug_fmt( "Sending back following resize event "
	%   "to subscriber(s) ~w:~n~p.", [ Subscribers, Msg ] ),

	% PID or name:
	[ SubDesignator ! Msg || SubDesignator <- Subscribers ];


% Button-specific clause:
send_event( Subscribers, EventType=onButtonClicked, EventSourceId, GUIObject,
			UserData, Event, NameTable ) ->

	BestSrcId = gui_id:get_best_button_id_internal( EventSourceId, NameTable ),

	%trace_utils:debug_fmt( "onButtonClicked: using ~w for ~w.",
	%                       [ BestSrcId, EventSourceId ] ),

	send_event_for_id( BestSrcId, Subscribers, EventType, EventSourceId,
					   GUIObject, UserData, Event );

% Menu item specific clause (although applies to toolbar tools as well):
send_event( Subscribers, EventType=onItemSelected, EventSourceId, GUIObject,
			UserData, Event, NameTable ) ->

	BestSrcId = gui_id:get_best_menu_item_id_internal( EventSourceId,
													   NameTable ),

	%trace_utils:debug_fmt( "onItemSelected: using ~w for ~w.",
	%                       [ BestSrcId, EventSourceId ] ),

	send_event_for_id( BestSrcId, Subscribers, EventType, EventSourceId,
					   GUIObject, UserData, Event );

% Base case, for all events that do not require specific treatments:
send_event( Subscribers, EventType, EventSourceId, GUIObject, UserData, Event,
			NameTable ) ->

	BestSrcId = gui_id:get_best_id_internal( EventSourceId, NameTable ),

	send_event_for_id( BestSrcId, Subscribers, EventType, EventSourceId,
					   GUIObject, UserData, Event ).


% Sends the specified event using the specified best event source identifier.
%
% Factoring helper.
%
-spec send_event_for_id( gui_id:id(), [ event_subscriber() ], event_type(),
			backend_id(), gui_object(),
			gui:user_data(), gui:backend_event() ) -> void().
send_event_for_id( BestEventSourceId, Subscribers, EventType, EventSourceId,
				   GUIObject, UserData, Event ) ->

	%trace_utils:debug_fmt( "Best identifier for source ~w: ~w.",
	%                        [ EventSourceId, BestId ] ),

	Context = #event_context{ id=EventSourceId, user_data=UserData,
							  backend_event=Event },

	Msg = { EventType, [ GUIObject, BestEventSourceId, Context ] },

	%trace_utils:debug_fmt( "Sending back following event "
	%   "to subscriber(s) ~w:~n~p.", [ Subscribers, Msg ] ),

	[ SubPid ! Msg || SubPid <- Subscribers ].



% @doc Enriches the specified event table with the specified event subscription
% information.
%
% (helper)
%
-spec register_in_event_loop_tables( event_subscription_spec(),
		event_subscriber(), loop_state() ) -> loop_state().
register_in_event_loop_tables( _SubscribedEvents=[],
							   _DefaultSubscriberDesignator, LoopState ) ->
	LoopState;

% Subscription pair:
register_in_event_loop_tables( _SubscribedEvents=[
			{ EventTypeMaybeList, GUIObjectMaybeList } | T ],
							   DefaultSubscriberDesignator, LoopState ) ->
	% Just adding the default option and subscriber to have a subscription
	% quadruplet:
	%
	register_in_event_loop_tables( [ { EventTypeMaybeList, GUIObjectMaybeList,
		_SubscriptionOpts=[], [ DefaultSubscriberDesignator ] } | T ],
		DefaultSubscriberDesignator, LoopState );

% Subscription triplet:
register_in_event_loop_tables( _SubscribedEvents=[
		{ EventTypeMaybeList, GUIObjectMaybeList, SubscriptionMaybeOpts } | T ],
		DefaultSubscriberDesignator, LoopState ) ->
	% Just adding the default subscriber to have a subscription quadruplet:
	register_in_event_loop_tables( [ { EventTypeMaybeList, GUIObjectMaybeList,
		SubscriptionMaybeOpts, [ DefaultSubscriberDesignator ] } | T ],
		DefaultSubscriberDesignator, LoopState );

% Full, canonical subscription quadruplet:
register_in_event_loop_tables( _SubscribedEvents=[
		{ EventTypeMaybeList, GUIObjectMaybeList, SubscriptionMaybeOpts,
		  SubscriberMaybeList } | T ],
		DefaultSubscriberDesignator, LoopState ) ->

	EventTypes = list_utils:ensure_atoms( EventTypeMaybeList ),

	% Objects, not identifiers for example:
	GUIObjects = list_utils:ensure_tuples( GUIObjectMaybeList ),

	SubOpts = list_utils:ensure_proplist( SubscriptionMaybeOpts ),
	Subscribers = list_utils:ensure_pids( SubscriberMaybeList ),

	NewLoopState = lists:foldl(
		fun( Obj, AccState ) ->
			register_event_types_for( Obj, EventTypes, SubOpts, Subscribers,
									  AccState )
		end,
		_Acc0=LoopState,
		_List=GUIObjects ),

	register_in_event_loop_tables( T, DefaultSubscriberDesignator,
								   NewLoopState );

register_in_event_loop_tables( _SubscribedEvents=[ Invalid | _T ],
							   _DefaultSubscriberDesignator, _LoopState ) ->
	throw( { invalid_event_subscription_spec, Invalid } ).



% (helper)
-spec register_event_types_for( gui_object(), [ event_type() ],
		[ event_subscription_opt() ], [ event_subscriber() ], loop_state() ) ->
											loop_state().
register_event_types_for( Canvas={ myriad_object_ref, myr_canvas, CanvasId },
						  EventTypes, SubOpts, Subscribers,
						  LoopState=#loop_state{
							event_table=EventTable,
							type_table=TypeTable,
							trap_set=TrapSet } ) ->

	%trace_utils:debug_fmt( "Registering subscribers ~w for event types ~p "
	%    "regarding canvas '~ts'.", [ Subscribers, EventTypes,
	%                                 gui:object_to_string( Canvas ) ] ),

	NewEventTable =
		record_subscriptions( Canvas, EventTypes, Subscribers, EventTable ),

	% We used to connect here the panel of a canvas to the MyriadGUI main loop
	% directly based on the user-specified types, however a panel must be
	% connected in all cases (whether or not the user subscribed to events
	% regarding their canvas) for onRepaintNeeded and onResized (for example to
	% manage properly resizings), and this has already been done by
	% gui_canvas:create_instance/1.
	%
	% We must thus avoid here to connect such panel again (thus more than once)
	% for a given event type (e.g. onRepaintNeeded), otherwise a logic (e.g. the
	% repaint one) could be triggered multiple times (as multiple identical
	% messages could then be received - however in practice this is not the
	% case, exactly one message per event type is received).

	BaseEventTypes = gui_canvas:get_base_panel_events_of_interest(),

	% Exactly one occurrence of these types to remove:
	NewEventTypes =
		list_utils:remove_first_occurrences( BaseEventTypes, EventTypes ),

	NewEventTypes =:= [] orelse
		begin

			% As a canvas is registered in wx as a panel (as wx will send events
			% about it) that will be reassigned as a canvas:

			CanvasState = get_instance_state( canvas, CanvasId, TypeTable ),

			Panel = CanvasState#canvas_state.panel,

			% Will defer these extra types of events of the underlying panel to
			% the canvas:
			%
			gui_wx_backend:connect( Panel, NewEventTypes, SubOpts, TrapSet )

		end,

	LoopState#loop_state{ event_table=NewEventTable };


register_event_types_for( GUIObject, EventTypes, SubOpts, Subscribers,
						  LoopState=#loop_state{
							event_table=EventTable,
							trap_set=TrapSet } ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( "Registering subscribers ~w for event types ~p "
			"regarding object '~ts' with options ~w.",
			[ Subscribers, EventTypes, gui:object_to_string( GUIObject ),
			  SubOpts ] ) ),

	% Auto-connection to the current PID (i.e. the one of the internal, main
	% event loop), so that it receives these events for their upcoming
	% dispatching to the actual subscribers:
	%
	[ gui_wx_backend:connect( GUIObject, EvType, SubOpts, TrapSet )
		|| EvType <- EventTypes ],

	% Now prepare the upcoming routing to the right subscriber:
	NewEventTable = record_subscriptions( GUIObject, EventTypes, Subscribers,
										  EventTable ),

	LoopState#loop_state{ event_table=NewEventTable }.



% @doc Removes from the specified event table the specified event subscription
% information.
%
% (helper)
%
-spec unregister_from_event_loop_tables( event_unsubscription_spec(),
		event_subscriber(), loop_state() ) -> loop_state().
unregister_from_event_loop_tables( _SubscribedEvents=[], _DefaultSubscribedPid,
								   LoopState ) ->
	LoopState;

unregister_from_event_loop_tables( _SubscribedEvents=[
		{ EventTypeMaybeList, GUIObjectMaybeList, SubscribedMaybeList } | T ],
		DefaultSubscribedPid, LoopState ) ->

	EventTypes = list_utils:ensure_atoms( EventTypeMaybeList ),
	GUIObjects = list_utils:ensure_tuples( GUIObjectMaybeList ),
	SubscribedList = list_utils:ensure_pids( SubscribedMaybeList ),

	NewLoopState = lists:foldl(
		fun( Obj, AccState ) ->
			unregister_event_types_from( Obj, EventTypes, SubscribedList,
										 AccState )
		end,
		_Acc0=LoopState,
		_List=GUIObjects ),

	unregister_from_event_loop_tables( T, DefaultSubscribedPid, NewLoopState );

unregister_from_event_loop_tables( _SubscribedEvents=[
			{ EventTypeMaybeList, GUIObjectMaybeList } | T ],
								   DefaultSubscribedPid, LoopState ) ->

	unregister_from_event_loop_tables( [ { EventTypeMaybeList,
		GUIObjectMaybeList, [ DefaultSubscribedPid ] } | T ],
									   DefaultSubscribedPid, LoopState ).



% (helper)
-spec unregister_event_types_from( gui_object(), [ event_type() ],
				[ event_subscriber() ], loop_state() ) -> loop_state().
unregister_event_types_from( Canvas={ myriad_object_ref, myr_canvas, CanvasId },
		EventTypes, Unsubscribers, LoopState=#loop_state{
			event_table=EventTable,
			type_table=TypeTable } ) ->

	%trace_utils:debug_fmt( "Unregistering subscribers ~w for event types ~p "
	%    "regarding canvas '~ts'.", [ Unsubscribers, EventTypes,
	%                                 gui:object_to_string( Canvas ) ] ),

	NewEventTable =
		record_unsubscriptions( Canvas, EventTypes, Unsubscribers, EventTable ),

	BaseEventTypes = gui_canvas:get_base_panel_events_of_interest(),

	% Exactly one occurrence of these types to remove:
	NewEventTypes =
		list_utils:remove_first_occurrences( BaseEventTypes, EventTypes ),

	NewEventTypes =:= [] orelse
		begin

			% As a canvas is registered in wx as a panel (as wx will send events
			% about it) that will be reassigned as a canvas:

			CanvasState = get_instance_state( canvas, CanvasId, TypeTable ),

			Panel = CanvasState#canvas_state.panel,

			% Will defer these extra types of events of the underlying panel to
			% the canvas:
			%
			gui_wx_backend:disconnect( Panel, NewEventTypes )

		end,

	LoopState#loop_state{ event_table=NewEventTable };

unregister_event_types_from( GUIObject, EventTypes, Unsubscribers,
		LoopState=#loop_state{ event_table=EventTable } ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( "Unregistering subscribers ~w "
			"for event types ~p regarding object '~ts'.",
			[ Unsubscribers, EventTypes,
			  gui:object_to_string( GUIObject ) ] ) ),

	[ gui_wx_backend:disconnect( GUIObject, EvType ) || EvType <- EventTypes ],

	% Remove the routing to the right subscriber:
	NewEventTable = record_unsubscriptions( GUIObject, EventTypes,
											Unsubscribers, EventTable ),

	LoopState#loop_state{ event_table=NewEventTable }.



% @doc Records the specified subscribers for each of the specified event types
% for the specified GUI object.
%
% (helper)
%
-spec record_subscriptions( gui_object(), [ event_type() ],
			[ event_subscriber() ], event_table() ) -> event_table().
record_subscriptions( GUIObject, EventTypes, Subscribers, EventTable ) ->

	ObjectKey = get_key_from_object( GUIObject ),

	NewDispatchTable = case table:lookup_entry( ObjectKey, EventTable ) of

		key_not_found ->
			UniqueSubscribers = list_utils:uniquify( Subscribers ),
			Entries = [ { EvType, UniqueSubscribers } || EvType <- EventTypes ],
			list_table:new( Entries );

		{ value, DispatchTable } ->
			enrich_event_table( EventTypes, Subscribers, DispatchTable )

	end,

	table:add_entry( ObjectKey, NewDispatchTable, EventTable ).



% @doc Records the removal of the specified subscribers for each of the
% specified event types for the specified GUI object.
%
% (helper)
%
-spec record_unsubscriptions( gui_object(), [ event_type() ],
			[ event_subscriber() ], event_table() ) -> event_table().
record_unsubscriptions( GUIObject, EventTypes, Unsubscribers, EventTable ) ->

	ObjectKey = get_key_from_object( GUIObject ),

	NewDispatchTable= case table:lookup_entry( ObjectKey, EventTable ) of

		key_not_found ->
			trace_utils:error_fmt( "Unable to unsubscribe from GUI object "
				"~w as it is not referenced (unsubscribers: ~w; "
				"event types: ~w); unsubscription ignored.",
				[ GUIObject, Unsubscribers, EventTypes ] ),
			EventTable;

		{ value, DispatchTable } ->
			shrink_event_table( EventTypes, Unsubscribers, DispatchTable )

	end,

	table:add_entry( ObjectKey, NewDispatchTable, EventTable ).



% @doc Returns an event dispatch table recording specified event type /
% subscriber associations.
%
-spec enrich_event_table( [ event_type() ], [ event_subscriber() ],
						  event_dispatch_table() ) -> event_dispatch_table().
enrich_event_table( _EventTypes=[], _Subscribers, DispatchTable ) ->
	DispatchTable;

enrich_event_table( _EventTypes=[ EventType | T ], Subscribers,
					DispatchTable ) ->

	NewSubscribers =
			case list_table:lookup_entry( EventType, DispatchTable ) of

		key_not_found ->
			list_utils:uniquify( Subscribers );

		{ value, CurrentSubscribers } ->
			list_utils:union( CurrentSubscribers, Subscribers )

	end,

	NewDispatchTable =
		list_table:add_entry( EventType, NewSubscribers, DispatchTable ),

	enrich_event_table( T, Subscribers, NewDispatchTable ).



% @doc Returns an event dispatch table from which the specified event type /
% subscriber associations have been removed.
%
-spec shrink_event_table( [ event_type() ], [ event_subscriber() ],
						  event_dispatch_table() ) -> event_dispatch_table().
shrink_event_table( _EventTypes=[], _Subscribers, DispatchTable ) ->
	DispatchTable;

shrink_event_table( _EventTypes=[ EventType | T ], Subscribers,
					DispatchTable ) ->

	UpdatedDispatchTable = case
			list_table:lookup_entry( EventType, DispatchTable ) of

		key_not_found ->
			trace_utils:error_fmt( "Unable to remove subscribers (~w) "
				"for event type '~ts' as it is not referenced in dispatch "
				"table; removal ignored.", [ Subscribers, EventType ] ),
			DispatchTable;

		{ value, CurrentSubscribers } ->
			% Stronger check:
			case list_utils:delete_existing_elements( Subscribers,
													  CurrentSubscribers ) of
			%case list_utils:remove_first_occurrences( Subscribers,
			%                                          CurrentSubscribers ) of

				[] ->
					list_table:remove_entry( EventType, DispatchTable );

				RemainingSubscribers ->
					list_table:add_entry( EventType, RemainingSubscribers,
										  DispatchTable )

			end

	end,

	shrink_event_table( T, Subscribers, UpdatedDispatchTable ).



% Returns a suitable, stable key corresponding to the specified object
% reference.
%
% (helper)
%
-spec get_key_from_object( gui_object() ) -> gui_object_key().
get_key_from_object( _WxObject={ wx_ref, WxId, WxType, _AnyState } ) ->
	{ WxType, WxId };

get_key_from_object( #myriad_object_ref{ object_type=ObjectType,
										 myriad_instance_id=InstanceId } ) ->
	{ ObjectType, InstanceId }.



% @doc Tells whether the two specified GUI objects match (equality operator).
-spec match( gui_object(), gui_object() ) -> boolean().
% The point is to ignore the included state (last element), which should not
% matter here, for reference comparisons:
%
match( _FirstWxObject={wx_ref, WxId, WxType, _AnyFirstState },
	   _SecondWxObject={wx_ref, WxId, WxType, _AnySecondState } ) ->
	true;

match( #myriad_object_ref{ object_type=ObjectType,
						   myriad_instance_id=InstanceId },
	   #myriad_object_ref{ object_type=ObjectType,
						   myriad_instance_id=InstanceId } ) ->
	true;

match( _FirstGUIObject, _SecondGUIObject ) ->
	false.



% @doc Returns a full, GUI-related applicative state to be kept around, notably
% so that it can be used by the event drivers.
%
% Here no specific OpenGL support is enabled, and no specific application data
% is registered.
%
% Refer to create_app_gui_state/3 for further details.
%
-spec create_app_gui_state( [ application_event_spec() ] ) -> app_gui_state().
create_app_gui_state( AppEventSpecs ) ->
	create_app_gui_state( AppEventSpecs, _MaybeOpenGLBaseInfo=undefined ).


% @doc Returns a full, GUI-related applicative state to be kept around, notably
% so that it can be used by the event drivers.
%
% Here an OpenGL support is enabled iff a base state is specified, and no
% specific application data is registered.
%
% Refer to create_app_gui_state/3 for further details.
%
-spec create_app_gui_state( [ application_event_spec() ],
							maybe( opengl_base_info() ) ) -> app_gui_state().
create_app_gui_state( AppEventSpecs, MaybeOpenGLBaseInfo ) ->
	create_app_gui_state( AppEventSpecs, MaybeOpenGLBaseInfo,
						  _MaybeAppSpecificInfo=undefined ).



% @doc Returns a full, GUI-related applicative state to be kept around, notably
% so that it can be used by the event drivers.
%
% Here an OpenGL support is enabled iff a base state is specified, and any
% (arbitrary) application- specific data is registered.
%
% Note that:
%  - buttons will be searched first by ID (recommended designator), otherwise by
%  GUI object reference
%  - keys will be searched first by scancodes, then keycodes, and regardless of
%  the (focused) widget that reports them
%
-spec create_app_gui_state( [ application_event_spec() ],
			maybe( opengl_base_info() ), maybe( any() ) ) -> app_gui_state().
create_app_gui_state( AppEventSpecs, MaybeOpenGLBaseInfo,
					  MaybeAppSpecificInfo ) ->

	trace_utils:debug_fmt( "Creating an application GUI state from:~n"
		" - specs: ~p~n - OpenGL base information: ~p~n"
		" - application-specific information: ~p",
		[ AppEventSpecs, MaybeOpenGLBaseInfo, MaybeAppSpecificInfo ] ),

	EventDriverTable = get_default_event_driver_table(),

	BlankTable = table:new(),

	GLBaseState = case MaybeOpenGLBaseInfo of

		undefined ->
			disabled;

		_GLBaseInfo={ GLCanvas, GLContext } ->
			{ uninitialised, GLCanvas, GLContext }

	end,

	register_app_event_spec( AppEventSpecs,
		#app_gui_state{ event_driver_table=EventDriverTable,
						basic_event_table=BlankTable,
						button_table=BlankTable,
						scancode_table=BlankTable,
						keycode_table=BlankTable,
						opengl_base_state=GLBaseState,
						app_specific_info=MaybeAppSpecificInfo } ).


% (helper)
register_app_event_spec( _AppEventSpecs=[], AppGUIState ) ->
	AppGUIState;

register_app_event_spec( _AppEventSpecs=[ { AppEvent, UserEvents } | T ],
						 AppGUIState ) ->
	NewAppGUIState = register_user_events( UserEvents, AppEvent, AppGUIState ),
	register_app_event_spec( T, NewAppGUIState ).



% (helper)
register_user_events( _UserEvents=[], _AppEvent, AppGUIState ) ->
	AppGUIState;

register_user_events( _UserEvents=[ { button_clicked, ButtonId } | T ],
		AppEvent, AppGUIState=#app_gui_state{ button_table=ButtonTable } ) ->

	% Not wanting names in the table keys:
	BackendButtonId = gui_id:resolve_any_id( ButtonId ),

	% Overwrites any previous association for that button:
	NewButtonTable = table:add_entry( BackendButtonId, AppEvent, ButtonTable ),

	NewAppGUIState = AppGUIState#app_gui_state{ button_table=NewButtonTable },

	register_user_events( T, AppEvent, NewAppGUIState );

register_user_events( _UserEvents=[ { scancode_pressed, Scancode } | T ],
		AppEvent, AppGUIState=#app_gui_state{
									scancode_table=ScancodeTable } ) ->
	% Overwrites any previous association for that scancode:
	NewScancodeTable = table:add_entry( Scancode, AppEvent, ScancodeTable ),

	NewAppGUIState = AppGUIState#app_gui_state{
		scancode_table=NewScancodeTable },

	register_user_events( T, AppEvent, NewAppGUIState );

register_user_events( _UserEvents=[ { keycode_pressed, Keycode } | T ],
		AppEvent, AppGUIState=#app_gui_state{
			keycode_table=KeycodeTable } ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( "Associating keycode ~w to ~ts.",
			[ Keycode, application_event_to_string( AppEvent ) ] ) ),

	% Overwrites any previous association for that keycode:
	NewKeycodeTable = table:add_entry( Keycode, AppEvent, KeycodeTable ),

	NewAppGUIState =
		AppGUIState#app_gui_state{ keycode_table=NewKeycodeTable },

	register_user_events( T, AppEvent, NewAppGUIState );

% For example EventAtom=window_closed:
register_user_events( _UserEvents=[ EventAtom | T ], AppEvent,
		AppGUIState=#app_gui_state{ basic_event_table=EventTable } )
										when is_atom( EventAtom ) ->
	% Overwrites any previous association for that event:
	NewEventTable = table:add_entry( EventAtom, AppEvent, EventTable ),

	NewAppGUIState =
		AppGUIState#app_gui_state{ basic_event_table=NewEventTable },

	register_user_events( T, AppEvent, NewAppGUIState );

register_user_events( _UserEvents=[ Other | _T ], AppEvent, _AppGUIState ) ->
	throw( { invalid_user_event, Other, AppEvent } ).



% @doc Returns the table of the default event drivers.
%
% The user application may override them as necessary.
%
-spec get_default_event_driver_table() -> event_driver_table().
get_default_event_driver_table() ->
	table:new( [ { onShown,         fun default_onShown_driver/2         },
				 { onRepaintNeeded, fun default_onRepaintNeeded_driver/2 },
				 { onResized,       fun default_onResized_driver/2       },
				 { onButtonClicked, fun default_onButtonClicked_driver/2 },
				 { onKeyPressed,    fun default_onKeyPressed_driver/2    },
				 { onWindowClosed,  fun default_onWindowClosed_driver/2  } ] ).



% @doc Associates the specified event driver to the specified event type,
% instead of the previous driver.
%
-spec set_event_driver( event_type(), event_driver(), app_gui_state() ) ->
								app_gui_state().
set_event_driver( EventType, EventDriver, AppGUIState=#app_gui_state{
						event_driver_table=DriverTable } ) ->

	NewDriverTable = table:add_entry( _K=EventType, _V=EventDriver,
									  DriverTable ),

	AppGUIState#app_gui_state{ event_driver_table=NewDriverTable }.



% @doc Associates the specified event drivers to the corresponding specified
% event types, instead of the previous drivers.
%
-spec set_event_drivers( [ { event_type(), event_driver() } ],
						 app_gui_state() ) -> app_gui_state().
set_event_drivers( EventTypeDriverPairs, AppGUIState=#app_gui_state{
						event_driver_table=DriverTable } ) ->

	NewDriverTable = table:add_entries( _Entries=EventTypeDriverPairs,
										DriverTable ),

	AppGUIState#app_gui_state{ event_driver_table=NewDriverTable }.





% Section for the implementation of default event drivers.
%
% Examples of direct implementations for non-OpenGL programs can be
% gui_overall_test.erl, whereas for OpenGL ones gui_opengl_integration_test.erl
% can be used.



% @doc The default event driver for the onShown (user) event type.
%
% Its type is event_driver().
%
-spec default_onShown_driver( event_elements(), app_gui_state() ) ->
								app_event_return().
% This default non-OpenGL implementation does not have much to do:
default_onShown_driver( _Elements=[ Frame, FrameId, EventContext ],
		AppGUIState=#app_gui_state{ opengl_base_state=disabled } ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt(
			"Frame ~ts (ID: ~ts) is shown (~ts); not using OpenGL.",
			[ gui:object_to_string( Frame ), gui_id:id_to_string( FrameId ),
			  context_to_string( EventContext ) ] ),
			  basic_utils:ignore_unused( [ FrameId, EventContext ] ) ),

	% Optional yet better:
	gui:unsubscribe_from_events( { onShown, Frame } ),

	{ _MaybeAppEventPair=undefined, AppGUIState };

% Here OpenGL is to be used, but is not initialised yet:
default_onShown_driver( _Elements=[ Frame, FrameId, EventContext ],
		AppGUIState=#app_gui_state{ opengl_base_state={ _GLStatus=uninitialised,
			_GLCanvas, _GLContext } } ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt(
			"Frame ~ts (ID: ~ts) is shown (~ts); using OpenGL, "
			"which is not initialised yet.",
			[ gui:object_to_string( Frame ), gui_id:id_to_string( FrameId ),
			  context_to_string( EventContext ) ] ),
			  basic_utils:ignore_unused( [ FrameId, EventContext ] ) ),

	trace_utils:warning(
		"onShown driver not overridden by the OpenGL application." ),

	% Most OpenGL applications are to override this driver, as it is the most
	% suitable first location to initialise OpenGL, since making a GL context
	% current (that is binding it) requires a shown window.

	% For example:

	% Optional yet better:
	gui:unsubscribe_from_events( { onShown, Frame } ),

	% Done once for all:
	%InitGUIState = initialise_opengl( AppGUIState ),

	% A onRepaintNeeded event message is expected to be received just
	% afterwards.

	{ _MaybeAppEvPair=undefined, AppGUIState }.



% @doc The default event driver for the onRepaintNeeded (user) event type.
%
% This default implementation is mostly a boilerplate, as such a driver should
% trigger, at least with OpenGL, a new (application-specific) rendering for the
% needed repaint. It is thus generally expected to be overridden.
%
% Its type is event_driver().
%
-spec default_onRepaintNeeded_driver( event_elements(), app_gui_state() ) ->
											app_event_return().
% Default for non-OpenGL rendering:
default_onRepaintNeeded_driver(
		_Elements=[ Canvas, CanvasId, EventContext ],
		AppGUIState=#app_gui_state{ opengl_base_state=disabled } ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt(
			"Non-OpenGL canvas '~ts' (~ts) needing repaint (~ts).",
			[ gui:object_to_string( Canvas ),
			  gui_id:id_to_string( CanvasId ),
			  context_to_string( EventContext ) ] ),
		basic_utils:ignore_unused( [ CanvasId, EventContext ] ) ),

	gui:blit( Canvas ),

	{ _MaybeAppEvPair=undefined, AppGUIState };


% Here OpenGL is to be used, but is not initialised yet:
default_onRepaintNeeded_driver(
		_Elements, % These are [GLCanvas, GLCanvasId, EventContext],
		AppGUIState=#app_gui_state{ opengl_base_state={ _GLStatus=uninitialised,
			_GLCanvas, _GLContext } } ) ->

	% OpenGL not ready yet (main window not shown yet):
	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug( "To be repainted, yet no OpenGL state yet." ) ),

	{ _MaybeAppEventPair=undefined, AppGUIState };


% Here using OpenGL and having it already initialised; rather
% application-specific:
%
default_onRepaintNeeded_driver(
		_Elements=[ GLCanvas, _GLCanvasId, _EventContext ],
		AppGUIState=#app_gui_state{
			opengl_base_state={ initialised, _GLCanvas, _GLContext } } ) ->

	gui:enable_repaint( GLCanvas ),

	trace_utils:warning(
		"onRepaintNeeded driver not overridden by the OpenGL application." ),

	% Most applications are to override this driver in order to perform a new
	% rendering (see gui_overall_test.erl for an example).

	% Not strictly necessary, as anyway a regular redraw is to happen soon
	% afterwards:

	% Should include the GL flushing and the buffer swapping:
	% NewAppGUIState = render( AppGUIState ),
	%
	% { _MaybeAppEventPair=undefined, NewAppGUIState }

	{ _MaybeAppEventPair=undefined, AppGUIState }.



% @doc The default event driver for the onResized (user) event type.
%
% This default implementation is mostly a boilerplate, as such a driver should
% trigger, at least with OpenGL, a new rendering to account for the resizing.
%
% Its type is event_driver().
%
-spec default_onResized_driver( event_elements(), app_gui_state() ) ->
											app_event_return().
% Default for non-OpenGL rendering:
default_onResized_driver(
		_Elements=[ Canvas, CanvasId, NewSize, EventContext ],
		AppGUIState=#app_gui_state{ opengl_base_state=disabled } ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt(
			"Non-OpenGL canvas '~ts' (~ts) resized to ~p (~ts).",
			[ gui:object_to_string( Canvas ), gui_id:id_to_string( CanvasId ),
			  NewSize, context_to_string( EventContext ) ] ),
		basic_utils:ignore_unused(
			[ Canvas, CanvasId, NewSize, EventContext ] ) ),

	% A rendering based on a (standard) canvas should take place here.

	{ _MaybeAppEvPair=undefined, AppGUIState };


% Here OpenGL is to be used, but is not initialised yet:
default_onResized_driver(
		_Elements, % These are [GLCanvas, GLCanvasId, NewSize, EventContext],
		AppGUIState=#app_gui_state{ opengl_base_state={ _GLStatus=uninitialised,
			_GLCanvas, _GLContext } } ) ->

	% OpenGL not ready yet (main window not shown yet):
	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug( "To be resized, yet no OpenGL state yet." ) ),

	{ _MaybeAppEventPair=undefined, AppGUIState };


% Here using OpenGL and having it already initialised; rather
% application-specific:
%
default_onResized_driver(
		_Elements, % These are [GLCanvas, GLCanvasId, NewSize, EventContext],
		AppGUIState=#app_gui_state{
			opengl_base_state={ initialised, _GLCanvas, _GLContext } } ) ->

	% A full rendering based on the resized canvas should take place here.

	trace_utils:warning(
		"onResized driver not overridden by the OpenGL application." ),

	% Most applications are to override this driver in order to perform a new
	% rendering (see gui_overall_test.erl for an example).

	{ _MaybeAppEventPair=undefined, AppGUIState }.



% @doc The default event driver for the onButtonClicked (user) event type.
%
% Its type is event_driver().
%
-spec default_onButtonClicked_driver( event_elements(),
									  app_gui_state() ) -> app_event_return().
default_onButtonClicked_driver( Elements=[ Button, ButtonId, EventContext ],
		AppGUIState=#app_gui_state{ button_table=ButtonTable } ) ->

	%cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt(
			"Button ~ts (ID: ~ts) has been clicked (~ts).",
			[ gui:object_to_string( Button ), gui_id:id_to_string( ButtonId ),
			  context_to_string( EventContext ) ] ),
			 % basic_utils:ignore_unused( EventContext ) ),

	ButtonBackendId = gui_id:resolve_any_id( ButtonId ),

	%trace_utils:debug_fmt( "Button table: ~ts",
	%                       [ table:to_string( ButtonTable ) ] ),

	% First looking up the button ID, then its GUI object reference:
	MaybeAppEvPair = case table:lookup_entry( ButtonBackendId, ButtonTable ) of

		key_not_found ->
			% As any button reference can be used:
			case table:lookup_entry( Button, ButtonTable ) of

				key_not_found ->
					undefined;

				% Typically quit_requested:
				{ value, AppEvent } ->
					BaseGUIEvent = { onButtonClicked, Elements },
					_AppEvPair={ AppEvent, BaseGUIEvent }

			end;

		{ value, AppEvent } ->
			BaseGUIEvent = { onButtonClicked, Elements },
			_AppEvPair={ AppEvent, BaseGUIEvent }

	end,

	% With this default implementation, application GUI state is constant:
	{ MaybeAppEvPair, AppGUIState }.



% @doc The default event driver for the onKeyPressed (user) event type.
%
% It looks up the scancode and keycode tables in order to map key events to any
% user-defined application event.
%
% Its type is event_driver().
%
-spec default_onKeyPressed_driver( event_elements(),
								   app_gui_state() ) -> app_event_return().
default_onKeyPressed_driver( Elements=[ Frame, FrameId, EventContext ],
		AppGUIState=#app_gui_state{ scancode_table=ScancodeTable,
									keycode_table=KeycodeTable } ) ->

	{ Scancode, Keycode } =
		gui_keyboard:event_context_to_code_pair( EventContext ),

	%cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt(
			"Key (scancode: ~w, keycode: ~w) has been pressed "
			"in frame ~ts (~ts), with ~ts.",
			[ Scancode, Keycode, gui:object_to_string( Frame ),
			  gui_id:id_to_string( FrameId ),
			  context_to_string( EventContext ) ] ),
		basic_utils:ignore_unused( [ Frame, FrameId ] ), % ),

	MaybeAppEvPair = case table:lookup_entry( Scancode, ScancodeTable ) of

		key_not_found ->
			case table:lookup_entry( Keycode, KeycodeTable ) of

				key_not_found ->
					undefined;

				{ value, AppEvent } ->
					BaseGUIEvent = { onKeyPressed, Elements },
					_AppEvPair={ AppEvent, BaseGUIEvent }

			end;

		{ value, AppEvent } ->
			BaseGUIEvent = { onKeyPressed, Elements },
			_AppEvPair={ AppEvent, BaseGUIEvent }

	end,

	% With this default implementation, application GUI state is constant:
	{ MaybeAppEvPair, AppGUIState }.




% @doc The default event driver for the onWindowClosed (user) event type.
%
% Its type is event_driver().
%
-spec default_onWindowClosed_driver( event_elements(),
				app_gui_state() ) -> app_event_return().
default_onWindowClosed_driver(
		Elements=[ MainFrame, MainFrameId, EventContext ],
		AppGUIState=#app_gui_state{ basic_event_table=BasicEventTable } ) ->

	% This default driver assumes that a single frame is tracked, the main one.

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( "Frame ~ts (~ts) has been closed (~ts).",
			[ gui:object_to_string( MainFrame ),
			  gui_id:id_to_string( MainFrameId ),
			  context_to_string( EventContext ) ] ),
		basic_utils:ignore_unused( [ MainFrame, MainFrameId, EventContext ] ) ),

	% For example a returned application event could lead to
	% gui:destruct_window(MainFrame) or gui:stop().

	case table:lookup_entry( window_closed, BasicEventTable ) of

		{ value, AppEvent } ->
			BaseGUIEvent = { onWindowClosed, Elements },
			AppEventPair = { AppEvent, BaseGUIEvent },
			{ AppEventPair, AppGUIState };

		key_not_found ->
			% Another default could be 'quit_requested':
			{ _MaybeAppEventPair=undefined, AppGUIState }

	end.



% @doc Returns usual, basic defaults in terms of application event
% specification, that is how user events shall be abstracted out in terms of
% (higher-level) application events.
%
% Corresponds to reasonable defaults for the first parameter of the
% create_app_gui_state/* functions.
%
-spec get_base_application_event_specs() -> [ application_event_spec() ].
get_base_application_event_specs() ->

	% - key "f" shall toggle fullscreen
	% - key "q" or Escape, or closing the (main) window shall quit the
	% application

	[ % Trigger the following app-level event...
		{ toggle_fullscreen, [ { keycode_pressed, ?MYR_K_f } ] },
		{ quit_requested,
		  % ... whenever any of these user-level events happens:
		  [ { keycode_pressed, ?MYR_K_q },
			{ scancode_pressed, ?MYR_SCANCODE_ESCAPE },
			window_closed ] } ].


% @doc Enables OpenGL in the specified application GUI state.
%
% Note that OpenGL actual initialisation is bound to happen no sooner than the
% corresponding window is shown.
%
% Note also that this function is of little use, as generally the GL canvas and
% context are directly set thanks to create_app_gui_state/3.
%
-spec enable_opengl( gl_canvas(), gl_context(), app_gui_state() ) ->
									app_gui_state().
enable_opengl( GLCanvas, GLContext, AppGUIState ) ->

	cond_utils:assert( myriad_debug_gui_events,
		AppGUIState#app_gui_state.opengl_base_state =:= disabled ),

	AppGUIState#app_gui_state{
		opengl_base_state={ uninitialised, GLCanvas, GLContext } }.



% @doc Waits (blocks) for the next user event that can be converted into an
% application event, which is then returned with its corresponding user event,
% together with a possibly updated application GUI state.
%
% Processes all user events (even those that do not result in an application
% event), returning only on the first one that can be converted into an
% application event.
%
% Meant to be called by the user code, instead of having to define its own
% lower-level event loop. Receives all messages that are collected by the
% calling process.
%
-spec get_application_event( app_gui_state() ) -> app_event_return().
get_application_event( AppGUIState ) ->
	case get_maybe_application_event( AppGUIState, _Timeout=infinity ) of

		{ _MaybeAppEventPair=undefined, NewAppGUIState } ->

			% Would be pointless, as cannot be a tight loop, due
			% to get_maybe_application_event/2:

			% Possibly better than yield/1:
			% timer:sleep( 10 ),

			get_application_event( NewAppGUIState );


		% Not expected to ever happen because of the (infinite) timeout, or
		% should an unexpected message be received:
		%
		%undefined ->
		%	get_application_event( AppGUIState );


		% Thus {DefinedAppEventPair, NewAppGUIState} ->
		AppEventReturn ->
			AppEventReturn

	end.



% @doc Reads any pending (lower-level) user event, returning it with any
% resulting application event, and an updated GUI state
%
% More precisely, tries to read any pending UserEvent:
%  - if none is found, returns just 'undefined'
%  - if a user event is found, returns {{MaybeApplicationEvent, UserEvent},
%  UpdatedGUIState}: if UserEvent can be converted into an application event,
%  returns this application event as first element of the pair, otherwise puts
%  'undefined' there
%
% Processes all user events (even those that do not result in an application
% event).
%
% Meant to be called by the user code, instead of having to define its own
% lower-level event loop. Receives all messages that are collected by the
% calling process.
%
-spec get_maybe_application_event( app_gui_state() ) ->
									maybe( app_event_return() ).
get_maybe_application_event( AppGUIState ) ->
	get_maybe_application_event( AppGUIState, _Timeout=0 ).



% @doc Reads any (lower-level) user event received during the specified (finite
% or not) time-out that can be converted into an application event, which is
% then returned with its corresponding user event, together with a possibly
% updated application GUI state.
%
% If a user event is received during the specified time-out yet cannot be
% converted into an application event, 'undefined' is returned instead of said
% application event, and an updated application GUI state is still returned.
%
% If no user event is available during said time-out, returns just 'undefined'
% (with no application GUI state), thus never blocks longer.
%
% Processes up to one pending user event, whether or not it results in an
% application event.
%
% Main, most flexible form.
%
% Meant to be called by the user code, instead of having to define its own
% lower-level event loop. Receives all messages that are collected by the
% calling process.
%
-spec get_maybe_application_event( app_gui_state(), time_out() ) ->
									maybe( app_event_return() ).
get_maybe_application_event( AppGUIState=#app_gui_state{
		event_driver_table=EventDriverTable }, Timeout ) ->

	%trace_utils:debug( "Entering GUI receive for user events." ),

	% Relying on an unique function like the present one is convenient, as it
	% allows to still define a selective receive.
	%
	receive

		{ EventType, EventElements } ->

			% Uncomment to ensure that a given event of interest is indeed
			% received:
			%
			%trace_utils:debug_fmt(
			%   "Received event of type '~ts', with elements: ~w",
			%   [ EventType, EventElements ] ),

			% As a bonus, avoids to have to write selective receives matching
			% the least-often received messages last:
			%
			%EventDriverFun = table:get_value( _K=EventType, EventDriverTable ),
			EventDriverFun = case table:lookup_entry( _K=EventType,
													  EventDriverTable ) of

				{ value, EvDrivFun } ->
					EvDrivFun;

				key_not_found ->
					throw( { no_event_driver_registered_for, EventType } )

			end,

			% Returns app_event_return(), that is {MaybeAppEventPair,
			% UpdatedAppGUIState}:
			%
			R = EventDriverFun( EventElements, AppGUIState ),

			%trace_utils:debug_fmt( "Driver for event type '~ts' returned:~n~w",
			%                       [ EventType, R ] ),

			R;

		Other ->
			trace_utils:eror_fmt( "The user-level GUI receiving logic "
				"received the following unexpected message:~n ~p.", [ Other ] ),
			% See below for semantics:
			%undefined
			throw( { unexpected_event_message, Other } )


	after Timeout ->

		% Not {undefined, AppGUIState}, to convey a different semantics (no user
		% event received here - as opposed to the receiving of a user event that
		% does not convert to an application one)
		%
		undefined

	end.



% @doc Returns a textual representation of the specified GUI-related applicative
% state.
%
-spec app_gui_state_to_string( app_gui_state() ) -> ustring().
app_gui_state_to_string( #app_gui_state{
		event_driver_table=EventDriverTable,
		basic_event_table=BasicEventTable,
		button_table=ButtonTable,
		scancode_table=ScancodeTable,
		keycode_table=KeycodeTable,
		opengl_base_state=OpenGLBaseState,
		app_specific_info=MaybeAppSpecificInfo } ) ->

	EventDriverStr = text_utils:format( "~B event drivers",
		[ table:size( EventDriverTable ) ] ),

	BasicEventStr = text_utils:format( "~B basic, user-level events",
		[ table:size( BasicEventTable ) ] ),

	ButtonStr = text_utils:format( "~B widget buttons",
		[ table:size( ButtonTable ) ] ),

	ScancodeStr = text_utils:format( "~B scancodes",
		[ table:size( ScancodeTable ) ] ),

	KeycodeStr = text_utils:format( "~B keycodes",
		[ table:size( KeycodeTable ) ] ),

	OpenGLStr = case OpenGLBaseState of

		disabled ->
			"not using OpenGL";

		{ uninitialised, _GLCanvas, _GLContext } ->
			"using OpenGL, which is not initialised yet";

		{ initialised, _GLCanvas, _GLContext } ->
			"using OpenGL, which is already initialised"

	end,

	AppStr = case MaybeAppSpecificInfo of

		undefined ->
			"no application-specific information";

		AppSpecificInfo ->
			text_utils:format( "application-specific information ~p",
							   [ AppSpecificInfo ] )

	end,


	text_utils:format( "user event registry with ~ts, supporting ~ts, "
		"tracking ~ts, ~ts and ~ts, ~ts and ~ts",
		[ EventDriverStr, BasicEventStr, ButtonStr, ScancodeStr, KeycodeStr,
		  OpenGLStr, AppStr ] ).



% @doc Returns the backend event included in the specified event context.
-spec get_backend_event( event_context() ) -> backend_event().
get_backend_event( #event_context{ backend_event=BackendEvent } ) ->
	BackendEvent.


% @doc Adjusts the specified MyriadGUI instances.
-spec adjust_objects( [ myriad_object_ref() ], event_table(),
			myriad_type_table(), id_name_alloc_table() ) -> myriad_type_table().
adjust_objects( _ObjectsToAdjust=[], _EventTable, TypeTable, _NameTable ) ->
	TypeTable;

adjust_objects( _ObjectsToAdjust=[ CanvasRef=#myriad_object_ref{
										object_type=myr_canvas,
										myriad_instance_id=CanvasId } | T ],
				EventTable, TypeTable, NameTable ) ->

	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),

	NewCanvasState = case gui_canvas:adjust_size_impl( CanvasState ) of

		{ _NeedsRepaint=true, AdjCanvasState } ->

			EventType = onRepaintNeeded,

			Subscribers =
				get_subscribers_for( CanvasRef, EventType, EventTable ),

			send_event( Subscribers, EventType, _EventSourceId=undefined,
						CanvasRef, _UserData=[], _Event=undefined, NameTable ),

			AdjCanvasState;

		{ _NeedsRepaint=false, AdjCanvasState } ->
			AdjCanvasState

	end,

	NewTypeTable =
		set_canvas_instance_state( CanvasId, NewCanvasState, TypeTable ),

	adjust_objects( T, EventTable, NewTypeTable, NameTable ).



% @doc Returns the internal state of the specified canvas instance.
-spec get_canvas_instance_state( myriad_instance_id(),
								 myriad_type_table() ) -> myriad_object_state().
get_canvas_instance_state( CanvasId, TypeTable ) ->
	get_instance_state( _MyriadObjectType=myr_canvas, CanvasId, TypeTable ).



% @doc Returns the internal state of the specified MyriadGUI instance.
-spec get_instance_state( myriad_object_ref(), myriad_type_table() ) ->
								myriad_object_state().
get_instance_state( { myriad_object_ref, MyriadObjectType, InstanceId },
					TypeTable ) ->
	get_instance_state( MyriadObjectType, InstanceId, TypeTable ).



% @doc Returns the internal state of the specified MyriadGUI instance.
-spec get_instance_state( myriad_object_ref(), myriad_instance_id(),
						  myriad_type_table() ) -> myriad_object_state().
get_instance_state( MyriadObjectType, InstanceId, TypeTable ) ->

	%trace_utils:debug_fmt( "~ts", [ type_table_to_string( TypeTable ) ] ),

	case table:lookup_entry( MyriadObjectType, TypeTable ) of

		{ value, #instance_referential{ instance_table=InstanceTable } } ->

			case table:lookup_entry( InstanceId, InstanceTable ) of

				{ value, InstanceState } ->
					InstanceState;

				key_not_found ->
					trace_utils:error_fmt( "Non-existing instance ~w "
						"of type ~ts; ~ts",
						[ InstanceId, MyriadObjectType,
						  type_table_to_string( TypeTable ) ] ),

					throw( { non_existing_instance, InstanceId,
							 MyriadObjectType } )

			end;

		key_not_found ->
			throw( { invalid_myriad_object_type, MyriadObjectType } )

	end.



% @doc Sets the internal state of the specified canvas instance.
-spec set_canvas_instance_state( myriad_instance_id(), myriad_object_state(),
								 myriad_type_table() ) -> myriad_type_table().
set_canvas_instance_state( CanvasId, CanvasState, TypeTable ) ->
	set_instance_state( _MyriadObjectType=myr_canvas, CanvasId, CanvasState,
						TypeTable ).



% @doc Returns the internal state of the specified MyriadGUI instance.
-spec set_instance_state( myriad_object_ref(), myriad_object_state(),
						  myriad_type_table() ) -> myriad_type_table().
set_instance_state( { myriad_object_ref, MyriadObjectType, InstanceId },
					InstanceState, TypeTable ) ->
	set_instance_state( MyriadObjectType, InstanceId, InstanceState,
						TypeTable ).



% @doc Returns the internal state of the specified, already-existing MyriadGUI
% instance.
%
-spec set_instance_state( gui:myriad_object_type(), myriad_instance_id(),
			myriad_object_state(), myriad_type_table() ) -> myriad_type_table().
set_instance_state( MyriadObjectType, InstanceId, InstanceState, TypeTable ) ->

	%trace_utils:debug_fmt( "Setting state of instance ~p of type ~ts to ~p",
	%   [ InstanceId, MyriadObjectType, InstanceState ] ),

	%trace_utils:debug_fmt( "~ts", [ type_table_to_string( TypeTable ) ] ),

	case table:lookup_entry( MyriadObjectType, TypeTable ) of

		{ value, Referential=#instance_referential{
								instance_table=InstanceTable } } ->

			% Already existing, hence no change in instance count:
			NewInstanceTable = table:update_entry( InstanceId, InstanceState,
												   InstanceTable ),

			NewReferential = Referential#instance_referential{
				instance_table=NewInstanceTable },

			% An update actually:
			table:add_entry( MyriadObjectType, NewReferential, TypeTable );


		key_not_found ->
			throw( { invalid_myriad_object_type, MyriadObjectType } )

	end.



% @doc Traps the specified event: does not propagate it upward in the widget
% hierarchy, thus considering that it has been processed once for all by the
% current handler. May typically apply to command events.
%
% Refer to gui:trap_event/1 for all details.
%
-spec trap_event( gui_event_object() ) -> void().
trap_event( GUIEventObject ) ->

	trace_utils:debug_fmt( "Trapping event ~w.", [ GUIEventObject ] ),

	% As various unrelated terms may apparently be accepted:
	cond_utils:assert( myriad_debug_gui_events,
					   gui_wx_backend:is_wx_event( GUIEventObject ) ),

	% The skip semantics is a bit unclear.
	% 'skip' is strangely here a synonymous of 'propagate'.

	% If no trap_event connection option was used when subscribing, the
	% corresponding events will be propagated - unless in the corresponding
	% event handler the skip/2 function is used with the {skip, false} option;
	% so here, as we want to trap this event:

	% Default of skip/1 is having skip=true.
	wxEvent:skip( GUIEventObject, _Opts=[ { skip, false } ] ).



% @doc Propagates the specified event upward in the widget hierarchy, so that it
% can be processed by parent handlers knowing that, for some event types (basic
% events, i.e. non-command ones like onRepaintNeeded), by default no event
% propagation is enabled.
%
% Refer to gui:propagate_event/1 for all details.
%
-spec propagate_event( gui_event_object() ) -> void().
propagate_event( GUIEventObject ) ->

	trace_utils:debug_fmt( "Propagating event ~w.", [ GUIEventObject ] ),

	% As various unrelated terms may apparently be accepted:
	cond_utils:assert( myriad_debug_gui_events,
					   gui_wx_backend:is_wx_event( GUIEventObject ) ),

	% Default of skip/1 is having skip=true:
	wxEvent:skip( GUIEventObject ).



% @doc Converts the specified wx event into a MyriadGUI one.
-spec wx_to_myriad_event( wx_event() ) -> gui_event().
wx_to_myriad_event( WxEvent={ wx, WxId, WxObject, UserData, WxEventInfo } ) ->

	% Example: WxEventType=close_window (the first element being the record
	% name, such as 'wxClose').
	%
	WxEventType = element( 2, WxEventInfo ),

	MyriadEventType = from_wx_event_type( WxEventType ),

	EventContext = #event_context{ id=WxId,
								   user_data=UserData,
								   backend_event=WxEvent },

	{ MyriadEventType, [ WxObject, EventContext ] }.



% @doc Returns the low-level wx record describing its full actual event, a
% record whose structure depends on that event.
%
-spec get_event_info( event_context() ) -> wx_event_info().
get_event_info( #event_context{ backend_event=#wx{ event=WxEventInfo } } ) ->
	WxEventInfo.



% Helper section.


% Stringification subsection.


% @doc Returns a textual representation of the specified event table.
-spec event_table_to_string( event_table() ) -> ustring().
event_table_to_string( EventTable ) ->

	case table:enumerate( EventTable ) of

		[] ->
			"empty event table";

		DispatchPairs ->

			DispatchStrings = [ dispatch_table_to_string( Object, Table )
									|| { Object, Table } <- DispatchPairs ],

			DispatchString = text_utils:strings_to_string( DispatchStrings ),

			text_utils:format( "event table with ~B GUI objects registered: "
				"~ts", [ length( DispatchPairs ), DispatchString ] )

	end.



% @doc Returns a textual representation of specified dispatch table.
-spec dispatch_table_to_string( gui_object_key(), event_dispatch_table() ) ->
										ustring().
dispatch_table_to_string( GUIObjectKey, DispatchTable ) ->

	EventPairs = list_table:enumerate( DispatchTable ),

	EventStrings = [ text_utils:format( "subscribers for event '~ts': ~w",
										[ EvType, EvSubscribers ] )
						|| { EvType, EvSubscribers } <- EventPairs ],

	EventString = text_utils:strings_to_string( EventStrings,
												_IndentationLevel=1 ),

	text_utils:format( "for GUI object whose key is '~ts': ~ts",
		[ gui:object_key_to_string( GUIObjectKey ), EventString ] ).



% @doc Returns a textual representation of the specified reassign table.
-spec reassign_table_to_string( reassign_table() ) -> ustring().
reassign_table_to_string( ReassignTable ) ->

	case table:enumerate( ReassignTable ) of

		[] ->
			"no GUI object reassignment defined";

		ObjectPairs ->
			Strings = [ text_utils:format( "events sent to '~ts' will be "
				"reassigned to '~ts'",
				[ gui:object_to_string( From ), gui:object_to_string( To ) ] )
						|| { From, To } <- ObjectPairs ],
			text_utils:format( "~B GUI object reassignments defined: ~ts",
				[ length( Strings), text_utils:strings_to_string( Strings ) ] )

	end.



% @doc Returns a textual representation of the specified type table.
-spec type_table_to_string( myriad_type_table() ) -> ustring().
type_table_to_string( Table ) ->

	case table:enumerate( Table ) of

		[] ->
			"empty type table";

		Pairs ->
			Strings = [ text_utils:format( "for type '~ts', ~ts", [ Type,
				instance_referential_to_string( Referential ) ] )
					|| { Type, Referential } <- Pairs ],

			text_utils:format( "Type table with ~B object types registered: "
				"~ts",
				[ length( Strings ), text_utils:strings_to_string( Strings ) ] )

	end.



% @doc Returns a textual representation of the specified type table.
-spec instance_referential_to_string( instance_referential() ) -> ustring().
instance_referential_to_string( #instance_referential{
									instance_count=Count,
									instance_table=InstanceTable } ) ->

	case table:enumerate( InstanceTable ) of

		[] ->
			Count = 0,
			"no instance recorded";

		Pairs ->
			Count = length( Pairs ),
			Strings = [ text_utils:format(
				"ID ~ts for instance whose state is: ~w",
				[ gui_id:id_to_string( Id ), State ] )
						|| { Id, State } <- Pairs ],
			text_utils:strings_to_string( Strings, _IndentLevel=1 )

	end.



% @doc Returns a textual representation of the specified GUI event.
-spec gui_event_to_string( gui_event() ) -> ustring().
gui_event_to_string( { EventType, Elements } ) ->
	% ~w clearer than ~p here:
	text_utils:format( "event of type '~ts', parametrised by elements ~w",
					   [ EventType, Elements ] ).



% @doc Returns a textual representation of the specified GUI event context.
%
% Typically obtained from an event-triggered message.
%
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

	% Preferred to ~p:
	EventString = text_utils:format( "~w", [ WxEvent ] ),

	text_utils:format( "context for event ~ts: ~ts and ~ts",
					   [ EventString, IdString, UserDataString ] ).




% @doc Returns a textual representation of the specified application event.
-spec application_event_to_string( application_event() ) -> ustring().
% For example 'quit_requested':
application_event_to_string( AE ) when is_atom( AE ) ->
	text_utils:atom_to_string( AE );

application_event_to_string( AE ) ->
	text_utils:format( "~p", [ AE ] ).



% Event type section.


% @doc Converts a MyriadGUI type of event into a wx one.
-spec to_wx_event_type( event_type() ) -> wx_event_type().
to_wx_event_type( EventType ) ->
	gui_generated:get_second_for_event_type( EventType ).


% @doc Converts a wx type of event into a MyriadGUI one.
-spec from_wx_event_type( wx_event_type() ) -> event_type().
from_wx_event_type( WxEventType ) ->
	gui_generated:get_first_for_event_type( WxEventType ).
