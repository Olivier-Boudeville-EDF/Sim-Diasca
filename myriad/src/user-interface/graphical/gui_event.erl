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


% @doc Gathers all elements relative to the <b>MyriadGUI</b> events, including
% the event loop.
%
-module(gui_event).


% Implementation notes.


% So for at least most of the functions here are executed in the context of the
% process of the MyriadGUI main loop.


% Events can be managed as messages or callbacks. We generally prefer the former
% (as messages can be selectively received, any context can be kept in the
% receive loop, no temporary process is created, no wx include is needed hence
% the backend can be well encapsulated, etc.).
%
% Whether an event shall be also dispatched to subsequent handlers may be
% decided by using trap_event/1.
%
% Event messages are internally converted, in order to hide the wx backend,
% augment it with other primitives (ex: canvas widget) and make them compliant
% with the MyriadGUI conventions, as seen by the user code (hint: these
% conventions comply with the WOOPER ones, should the GUI be used in an OOP
% context).
%
% Regarding events, see also:
% https://wiki.wxwidgets.org/Events#Event.Skip_and_Event.Veto

% Please refer to wx.pdf (available on http://www.erlang.org/doc/apps/wx/wx.pdf)
% for more architecture/implementation details about wx.

% We used to rely on a separate process obtained from
% gui_id:create_id_allocator/0 to manage identifiers, yet it is certainly more
% efficient to have them managed directly by this gui_event main loop.


% Function export section.


% Main event primitives:
-export([ start_main_event_loop/4,
		  get_trapped_event_types/1, trap_event/1, propagate_event/1,
		  get_event_translation_table/0,
		  wx_to_myriad_event/2, set_instance_state/3, match/2 ]).


% Stringification:
-export([ event_table_to_string/1 ]).


% To silence unused warnings:
-export([ get_subscribers_for/3, adjust_objects/4,
		  process_only_latest_repaint_event/4, reassign_table_to_string/1,
		  get_instance_state/2, type_table_to_string/1,
		  instance_referential_to_string/1, set_canvas_instance_state/3 ]).



% For the event_context record:
-include("gui.hrl").

% For canvas_state():
-include("gui_canvas.hrl").


% For wx headers:
-include("gui_internal_defines.hrl").



% Type section.


% Event messages.
%
% Lower-level, backend-specific events are translated into MyriadGUI event
% messages, to be received by their respective event subscribers.



-type gui_event() :: { event_type(), Elements :: [ any() ] }.
% A (MyriadGUI) event is a pair whose first element is the event type, as an
% atom (ex: 'onWindowClosed'), and whose second element is a list, whose first
% element is the GUI object that generated that event (the closed window, here),
% and whose last element is the event context (intermediary elements carrying
% event-specific information):
%
% {event_type(), [gui_object(), ..., event_context()]}
%
% Ex: {onWindowClosed, [Window, CloseContext]}.
%
% So the event context can be fetched with:
% EventContext = list_utils:get_last_element( Elements ),
%
% These values are sent as messages to the processes having subscribed to this
% type of event.
%
% Note: these messages respect the WOOPER conventions, and this is done on
% purpose, to facilitate any integration with upper layers.


% Thus an actual wx:wx_object(), i.e. a #wx_ref record:
-type gui_event_object() :: wxEvent:wxEvent().
% A Myriad GUI object (therefore the reference to a full-blown backend process -
% not a mere datastructure like an event record received as a message) holding
% information about an event passed to a callback or member function.
%
% Unless explicitly trapped by such a function (see the 'trap_event'
% subscription option, or the gui:trap_event/1 function), most event types are
% propagated upward in the widget hierarchy.



-type instance_count() :: basic_utils:count().
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



-type wx_event_handler() :: wxEvtHandler:wxEvtHandler().



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


-type event_type() :: window_event_type()
					| command_event_type()
					| mouse_event_type()
					| keyboard_event_type().
% A type of MyriadGUI event, independent from any backend.
%
% Unless specified otherwise, by default the events of a given type will
% propagate: subscribing to them does not preclude them from being sent also to
% the parent event handlers in the widget hierarchy.
%
% For some other, less numerous event types (ex: onWindowClosed), they will be
% by default trapped (their events will not be propagated, so they will be
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
% Note: if adding event types, consider updating gui:get_event_types_to_trap/0
% as well.


-type window_event_type() ::
		'onRepaintNeeded'
	  | 'onButtonClicked'
	  | 'onResized'

		% Trapped by default, to avoid race condition in termination procedures
		% between user handlers and backend ones:
		%
	  | 'onWindowClosed'

	  | 'onShown'.
% A type of event possibly emitted by a window.
%
% Note that resizing a widget (typically a canvas) implies receiving also a
% onRepaintNeeded event; so a canvas may subscribe only to onRepaintNeeded (not
% necessarily to onResized).


-type command_event_type() ::

		% Typically when the mouse cursor enters a toolbar (hovering):
		'onToolbarEntered'

		% Typically when selecting (e.g. left-clicking) an item of a menu or a
		% tool of a toolbar:
		%
	  | 'onItemSelected'

		% Typically when right-clicking on a tool of a toolbar:
	  | 'onToolRightClicked'.
% Types of events emitted by commands, a variety of simple controls (ex: menus,
% toolbars).
%
% Generally these command events are meant to be trapped (not propagated in the
% widget hierarchy), as a single, user-defined handler suffice; as a result, by
% default, all of them are trapped.




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
% accordingly (ex: for proper resizes). Specifying the 'trap_event' option
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
		{ gui_wx_backend:wx_widget_type(), gui_wx_backend:wx_id() }.
% A suitable stable key corresponding to a gui_object() (notably ignoring the
% last, 'state' element of this quadruplet).
%
% Ex: the gui_object() {wx_ref,63,wxFrame,AnyState} results in the {wxFrame,63}
% key.


-type myriad_object_key() :: { myriad_object_type(), myriad_instance_id() }.
% The MyriadGUI type corresponding to gui_wx_object_key/0.
%
% Ex: the myriad_object_ref() {myriad_object_ref,myr_canvas,12} results in the
% {myr_canvas,12} key.


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



-type event_table() :: table:table( gui_object_key(), event_dispatch_table() ).
% An indirection table dispatching events according to subscription
% specifications.
%
% For an incoming event, we see this type (virtually, logically) as:
% table({gui_object(), event_type()}, set_utils:set(event_subscriber())):
%
% - the first key is the key corresponding to the GUI object (e.g. widget) from
% which the event emanates (ex: a frame)
%
% - the second key is its corresponding (internal) event type (ex:
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


-type reassign_table() :: table:table( gui_object(), gui_object() ).
% To replace source events objects (ex: a panel) by others (ex: its associated
% canvas, if any).


-type myriad_type_table() ::
		table:table( myriad_object_type(), instance_referential() ).
% To store the MyriadGUI instances (sorted by types) and manage them like wx
% native objects.
%
% Keys are like 'canvas'.



-record( instance_referential, {

	% Total count of the instances already created for that type:
	instance_count :: instance_count(),

	instance_table :: table( myriad_instance_id(), myriad_object_state() ) } ).


-type instance_referential() :: #instance_referential{}.
% To store, for a given MyriadGUI type (ex: 'canvas'), all information about all
% instances.
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
	% by MyriadGUI to complement the backend (ex: canvas instances).
	%
	type_table :: myriad_type_table(),


	% The precomputed set of event types that shall be trapped by default:
	trap_set :: trap_set(),


	% A bijective table to translate wx and MyriadGUI event types back and
	% forth.
	%
	event_translation_table :: event_translation_table(),


	id_next :: backend_id(),
	% The next backend identifier that will be allocated.

	id_name_alloc_table :: id_name_alloc_table()
	% A bijective table to convert between name identifiers and backend ones.


	% List of the MyriadGUI objects that shall be adjusted after a show:
	%
	% (actually not found necessary, hence at least currently disabled)
	%
	%objects_to_adjust=[] :: [ myriad_object_ref() ]

} ).


-type loop_state() :: #loop_state{}.


-type wx_event() ::
		{ 'wx', wx_id(), wx:wx_object(), gui:user_data(), wx_event_info() }.
% A wx_event record comprises:
%
% - (the 'wx' record tag, if the record instance is seen as a tuple)
%
% - id :: wx_id() the (integer) identifier of the object (e.g. widget) that
% received the event (event source)
%
% - obj :: wx:wx_object() is the reference of the wx object that was specified
% in the connect/n call, i.e. on which connect/n was called (ex:
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


-type wx_event_info() :: tuple().
% A wx-defined record describing an actual event.
%
% A WxFoobar-like record whose first field is its 'type', and which may have
% other fields.
%
% Examples of descriptions, as tuples: {wxClose,close_window}, or
% {wxCommand,command_button_clicked,CmdString,CmdInt,...}


-type received_event() :: wx_event() | gui_event().
% A received event is either a backend one or a MyriadGUI one.


-type trap_set() :: set( [ event_type() ] ).
% A set of the event types that shall be trapped by default.


-type event_translation_table() ::
   bijective_table:bijective_table( wx_event_type(), event_type() ).
% A table to implement an effecient two-way translation between the event types
% of wx and of MyriadGUI.


-export_type([ wx_event/0, wx_event_info/0,
			   trap_set/0, event_translation_table/0 ]).


% Shorthands:

-type count() :: basic_utils:count().

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

-type backend_id() :: gui_id:backend_id().
-type wx_id() :: gui_id:wx_id().
-type id_name_alloc_table() :: gui_id:id_name_alloc_table().
-type myriad_instance_id() :: gui_id:myriad_instance_id().

-type keyboard_event_type() :: gui_keyboard:keyboard_event_type().
-type mouse_event_type() :: gui_mouse:mouse_event_type().

-type wx_object() :: wx:wx_object().
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
-spec start_main_event_loop( wx_server(), wx_env(), trap_set(),
							 event_translation_table() ) -> no_return().
start_main_event_loop( WxServer, WxEnv, TrapSet, EventTranslationTable ) ->

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
		event_translation_table=EventTranslationTable,
		id_next=gui_id:get_first_allocatable_id(),
		id_name_alloc_table=gui_id:get_initial_allocation_table() },

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

	CommandEventTypes = [ onToolbarEntered, onItemSelected,
						  onToolRightClicked ],

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



% @doc Returns a bijective table to translate wx and MyriadGUI event types back
% and forth.
%
-spec get_event_translation_table() -> event_translation_table().
get_event_translation_table() ->

	% {wx_event_type(), event_type()} entries:

	% Mouse section:
	MouseEntries = [
		{ motion, onMouseMoved },

		{ left_down,   onMouseLeftButtonPressed },
		{ left_up,     onMouseLeftButtonReleased },
		{ left_dclick, onMouseLeftButtonDoubleClicked },

		{ middle_down,   onMouseMiddleButtonPressed },
		{ middle_up,     onMouseMiddleButtonReleased },
		{ middle_dclick, onMouseMiddleButtonDoubleClicked },

		{ right_down,   onMouseRightButtonPressed },
		{ right_up,     onMouseRightButtonReleased },
		{ right_dclick, onMouseRightButtonDoubleClicked },

		{ aux1_down,   onMouseFourthButtonPressed },
		{ aux1_up,     onMouseFourthButtonReleased },
		{ aux1_dclick, onMouseFourthButtonDoubleClicked },

		{ aux2_down,   onMouseFifthButtonPressed },
		{ aux2_up,     onMouseFifthButtonReleased },
		{ aux2_dclick, onMouseFifthButtonDoubleClicked },

		{ mousewheel, onMouseWheelScrolled },

		{ enter_window, onMouseEnteredWindow },
		{ leave_window, onMouseLeftWindow } ],

	% Keyboard section:
	KeyboardEntries = [
		{ char,      onCharEntered },
		{ char_hook, onCharEnteredHook },
		{ key_down,  onKeyPressed },
		{ key_up,    onKeyReleased } ],

	% Menu section/tool(bar) section:
	MenuToolEntries = [
		{ command_menu_selected, onItemSelected },
		{ command_tool_enter,    onToolbarEntered },
		{ command_tool_rclicked, onToolRightClicked } ],

	% Window section:
	WindowEntries = [
		{ show,                   onShown },
		{ size,                   onResized },
		{ paint,                  onRepaintNeeded },
		{ command_button_clicked, onButtonClicked },
		{ close_window,           onWindowClosed } ],

	AllEventEntries = MouseEntries ++ KeyboardEntries ++ MenuToolEntries
											++ WindowEntries,

	bijective_table:new( AllEventEntries ).



% @doc Receives and process all messages (this is the actual MyriadGUI main
% event loop), coming:
%
% - either from controlling processes (typically from application processes
% subscribing to some events)
%
% - or from the (here, wx) backend, that notifies this loop of the actual,
% lower-level events
%
-spec process_event_messages( loop_state() ) -> no_return().
process_event_messages( LoopState ) ->

	cond_utils:if_defined( myriad_debug_gui_repaint_logic,
		trace_utils:debug_fmt( "[event] GUI main loop ~w waiting "
							   "for event messages...", [ self() ] ) ),

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



% @doc Processes specified GUI event from the current (wx) backend.
%
% A *wx* (backend) event has been received here, in this first clause:
%
% Structure: {wx, EventSourceId, Obj, UserData, EventInfo }, with EventInfo:
% {WxEventName, EventType, ...}
%
% Ex: {wx, -2006, {wx_ref,35,wxFrame,[]}, [], {wxClose,close_window}}.
%
-spec process_event_message( received_event(), loop_state() ) -> loop_state().
process_event_message( WxEvent=#wx{ id=EventSourceId, obj=GUIObject,
									userData=UserData, event=WxEventInfo },
					   LoopState ) ->

	cond_utils:if_defined( myriad_debug_gui_repaint_logic,
		trace_utils:debug_fmt( "[event] Received wx event ~w.", [ WxEvent ] ) ),

	process_wx_event( EventSourceId, GUIObject, UserData, WxEventInfo,
					  WxEvent, LoopState );


% Special case: when creating a widget instance (ex: a frame) while specifying
% its name, the GUI loop must be notified so that it stores this name,
% associates it to a new backend identifier (wx_id()) and returns it to the
% sender for its upcoming, corresponding wx creation call.
%
process_event_message( { declareNameId, NameId, SenderPid },
			LoopState=#loop_state{ id_next=NextId,
								   id_name_alloc_table=NameTable } ) ->

	SenderPid ! { notifyingAssignedId, NextId },

	NewNameTable = bijective_table:add_new_entry( NameId, NextId, NameTable ),

	LoopState#loop_state{ id_next=NextId+1,
						  id_name_alloc_table=NewNameTable };


process_event_message( { resolveNameId, NameId, SenderPid },
			LoopState=#loop_state{ id_name_alloc_table=NameTable } ) ->

	Id = bijective_table:get_second_for( NameId, NameTable ),

	SenderPid ! { notifyResolvedIdentifier, Id },

	LoopState;


% From now, the event messages received are *MyriadGUI* ones, i.e. gui_event()
% (either internal or user-emanating):
%
% (some operations directly impact the canvas state as seen from MyriadGUI,
% others, like draw operations, not;they impact only the state of backend
% objects, thus references on them in CanvasState and thus LoopState can be kept
% as are)
%
% So, from here, CanvasId is in the form of {myriad_object_ref,myr_canvas,12}
% (and thus no name_id() conversion makes sense):
%
process_event_message( { setCanvasDrawColor, [ CanvasId, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->

	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),

	gui_canvas:set_draw_color( CanvasState, Color ),

	LoopState;


process_event_message( { setCanvasFillColor, [ CanvasId, MaybeColor ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:set_fill_color( CanvasState, MaybeColor ),
	LoopState;

process_event_message( { setCanvasBackgroundColor, [ CanvasId, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->

	%trace_utils:debug_fmt( "Canvas: ~p", [ Canvas ] ),
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),

	%trace_utils:debug_fmt( "CanvasState: ~p", [ CanvasState ] ),
	gui_canvas:set_background_color( CanvasState, Color ),
	LoopState;

process_event_message( { getCanvasRGB, [ CanvasId, Point2 ], CallerPid },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	Color = gui_canvas:get_rgb( CanvasState, Point2 ),
	CallerPid ! { notifyCanvasRGB, Color },
	LoopState;

process_event_message( { setCanvasRGB, [ CanvasId, Point ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:set_rgb( CanvasState, Point ),
	LoopState;

process_event_message( { drawCanvasLine, [ CanvasId, P1, P2 ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->

	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_line( CanvasState, P1, P2 ),
	LoopState;

process_event_message( { drawCanvasLine, [ CanvasId, P1, P2, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_line( CanvasState, P1, P2, Color ),
	LoopState;

process_event_message( { drawCanvasLines, [ CanvasId, Points ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_lines( CanvasState, Points ),
	LoopState;

process_event_message( { drawCanvasLines, [ CanvasId, Points, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_lines( CanvasState, Points, Color ),
	LoopState;

process_event_message( { drawCanvasSegment, [ CanvasId, _Points ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	_CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	throw( not_implemented ),
	% 2 missing arguments in:
	%gui_canvas:draw_segment( CanvasState, Points ),
	%gui_canvas:draw_segment(canvas_state(), line2(), coordinate(),
	% coordinate()),

	LoopState;

process_event_message( { drawCanvasPolygon, [ CanvasId, Points ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_polygon( CanvasState, Points ),
	LoopState;

process_event_message( { drawCanvasLabel, [ CanvasId, Point, Label ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_label( CanvasState, Point, Label ),
	LoopState;

process_event_message( { drawCanvasCross, [ CanvasId, Location ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_cross( CanvasState, Location ),
	LoopState;

process_event_message( { drawCanvasCross, [ CanvasId, Location, EdgeLength ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_cross( CanvasState, Location, EdgeLength ),
	LoopState;

process_event_message( { drawCanvasCross,
							[ CanvasId, Location, EdgeLength, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_cross( CanvasState, Location, EdgeLength, Color ),
	LoopState;

process_event_message( { drawCanvasLabelledCross,
							[ CanvasId, Location, EdgeLength, LabelText ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_labelled_cross( CanvasState, Location, EdgeLength,
									LabelText ),
	LoopState;

process_event_message( { drawCanvasLabelledCross,
		[ CanvasId, Location, EdgeLength, Color, LabelText ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_labelled_cross( CanvasState, Location, EdgeLength,
									Color, LabelText ),
	LoopState;

process_event_message( { drawCanvasCircle, [ CanvasId, Center, Radius ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_circle( CanvasState, Center, Radius ),
	LoopState;

process_event_message( { drawCanvasCircle,
		[ CanvasId, Center, Radius, Color ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_circle( CanvasState, Center, Radius, Color ),
	LoopState;

process_event_message( { drawCanvasNumberedPoints, [ CanvasId, Points ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:draw_numbered_points( CanvasState, Points ),
	LoopState;

process_event_message( { loadCanvasImage, [ CanvasId, Filename ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	% Canvas state is const (just an update of the back-buffer):
	gui_canvas:load_image( CanvasState, Filename ),
	LoopState;

process_event_message( { loadCanvasImage, [ CanvasId, Position, Filename ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:load_image( CanvasState, Position, Filename ),
	LoopState;

process_event_message( { resizeCanvas, [ CanvasId, NewSize ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->

	TypeTable = LoopState#loop_state.type_table,

	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),

	NewCanvasState = gui_canvas:resize( CanvasState, NewSize ),

	%trace_utils:debug_fmt( "NewCanvasState = ~p", [ NewCanvasState ] ),

	NewTypeTable = set_canvas_instance_state( CanvasId, NewCanvasState,
											  TypeTable ),

	LoopState#loop_state{ type_table=NewTypeTable };

process_event_message( { blitCanvas, CanvasId },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:blit( CanvasState ),
	LoopState;

process_event_message( { clearCanvas, CanvasId },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui_canvas:clear( CanvasState ),
	LoopState;

process_event_message( { setTooltip, [ CanvasId, Label ] },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	gui:set_tooltip( CanvasState#canvas_state.panel, Label ),
	LoopState;

process_event_message( { getPanelForCanvas, CanvasId, CallerPid },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	CallerPid ! { notifyCanvasPanel, CanvasState#canvas_state.panel },
	LoopState;

process_event_message( { getCanvasSize, CanvasId, CallerPid },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	Size = gui_canvas:get_size( CanvasState ),
	CallerPid ! { notifyCanvasSize, Size },
	LoopState;

process_event_message( { getCanvasClientSize, CanvasId, CallerPid },
					   LoopState=#loop_state{ type_table=TypeTable } ) ->
	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),
	Size = gui_canvas:get_client_size( CanvasState ),
	CallerPid ! { notifyCanvasClientSize, Size },
	LoopState;


% MyriadGUI user request (ex: emanating from gui:create_canvas/1):
process_event_message( { createInstance, [ ObjectType, ConstructionParams ],
						 CallerPid }, LoopState ) ->
	process_myriad_creation( ObjectType, ConstructionParams, CallerPid,
							 LoopState );


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
			"from events ~p.",
			[ SenderPid, SubscribedPid, UnsubscribedEvents ] ) ),

	NewLoopState = unregister_from_event_loop_tables( UnsubscribedEvents,
												SubscribedPid, LoopState ),

	% Now synchronous to avoid race conditions:
	SenderPid ! onEventUnsubscriptionProcessed,
	NewLoopState;

process_event_message( { getEventTranslationTable, [], SenderPid },
					   LoopState=#loop_state{
							event_translation_table=EventTranslationTable } ) ->
	SenderPid ! { notifyEventTranslationTable, EventTranslationTable },
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
	ok;

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



% @doc Processes specified wx event message.
-spec process_wx_event( wx_id(), wx_object(), gui:user_data(),
					wx_event_info(), wx_event(), loop_state() ) -> loop_state().
process_wx_event( EventSourceId, GUIObject, UserData, WxEventInfo, WxEvent,
				  LoopState=#loop_state{
							   event_table=EventTable,
							   reassign_table=ReassignTable,
							   type_table=TypeTable,
							   event_translation_table=EventTranslationTable,
							   id_name_alloc_table=NameTable } ) ->

	%trace_utils:debug_fmt( "Processing wx event~n  ~p.", [ WxEvent ] ),

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
			% may be needed to update that target reassigned object first (ex:
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
			EventType = gui_wx_backend:from_wx_event_type( WxEventType,
								EventTranslationTable ),

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
% after specified event (ex: an onResized one) has been received.
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

			NewCanvasState = gui_canvas:resize( CanvasState, NewSize ),

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
				table:new( [ { FirstInstanceId, ObjectInitialState } ] ),

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

	NewTypeTable = table:add_entry( ObjectType, NewInstanceReferential,
									TypeTable ),

	{ MyriadRef, NewTypeTable }.




% @doc Sends the specified MyriadGUI event to the relevant subscribers.
%
% Refer to gui:subscribe_to_events/1 for a description of the messages to be
% sent to subscribers.
%
% (helper)
%
-spec send_event( [ event_subscriber() ], event_type(), gui:id(),
			gui_object(), gui:user_data(), gui:backend_event(),
			id_name_alloc_table() ) -> void().
send_event( _Subscribers=[], _EventType, _EventSourceId, _GUIObject, _UserData,
			_Event, _NameTable ) ->
	ok;

% Special cases first, when the information to return to the user process is to
% be specifically adapted.
%
% Wanting to have the new size directly available in the message sent back:
send_event( Subscribers, EventType=onResized, EventSourceId, GUIObject,
			UserData, Event, NameTable ) ->

	BestId = gui_id:try_resolve_id( EventSourceId, NameTable ),

	Context = #event_context{ id=EventSourceId, user_data=UserData,
							  backend_event=Event },

	% Making the new size readily available:

	WxEventInfo = Event#wx.event,

	% Defined in wx.hrl:
	NewSize = WxEventInfo#wxSize.size,

	%trace_utils:debug_fmt( "onResized event: new size is ~p.", [ NewSize ] ),

	% Same structure as for OpenGL canvases:
	Msg = { EventType, [ GUIObject, BestId, NewSize, Context ] },

	%trace_utils:debug_fmt( "Sending back following resize event "
	%   "to subscriber(s) ~w:~n~p.", [ Subscribers, Msg ] ),

	% PID or name:
	[ SubDesignator ! Msg || SubDesignator <- Subscribers ];


% Base case, for all events that do not require specific treatments:
send_event( Subscribers, EventType, EventSourceId, GUIObject, UserData,
			Event, NameTable ) ->

	BestId = gui_id:try_resolve_id( EventSourceId, NameTable ),

	Context = #event_context{ id=EventSourceId, user_data=UserData,
							  backend_event=Event },

	Msg = { EventType, [ GUIObject, BestId, Context ] },

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
							trap_set=TrapSet,
							event_translation_table=EventTranslationTable} ) ->

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
	% for a given event type (ex: onRepaintNeeded), otherwise a logic (ex: the
	% repaint one) could be triggered multiple times (as multiple identical
	% messages could then be received - however in practice this is not the
	% case, exactly one message per event type is received).

	BaseEventTypes = gui_canvas:get_base_panel_events_of_interest(),

	% Exactly one occurrence of these types to remove:
	NewEventTypes =
		list_utils:remove_first_occurrences( BaseEventTypes, EventTypes ),

	case NewEventTypes of

		% Shortcut:
		[] ->
			ok;

		_ ->
			% As a canvas is registered in wx as a panel (as wx will send events
			% about it) that will be reassigned as a canvas:

			CanvasState = get_instance_state( canvas, CanvasId, TypeTable ),

			Panel = CanvasState#canvas_state.panel,

			% Will defer these extra types of events of the underlying panel to
			% the canvas:
			%
			gui_wx_backend:connect( Panel, NewEventTypes, SubOpts, TrapSet,
									EventTranslationTable )

	end,

	LoopState#loop_state{ event_table=NewEventTable };


register_event_types_for( GUIObject, EventTypes, SubOpts, Subscribers,
						  LoopState=#loop_state{
						   event_table=EventTable,
						   trap_set=TrapSet,
						   event_translation_table=EventTranslationTable } ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( "Registering subscribers ~w for event types ~p "
			"regarding object '~ts' with options ~w.",
			[ Subscribers, EventTypes, gui:object_to_string( GUIObject ),
			  SubOpts ] ) ),

	% Auto-connection to the current PID (i.e. the one of the internal, main
	% event loop), so that it receives these events for their upcoming
	% dispatching to the actual subscribers:
	%
	[ gui_wx_backend:connect( GUIObject, EvType, SubOpts, TrapSet,
							  EventTranslationTable ) || EvType <- EventTypes ],

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
			event_translation_table=EventTranslationTable,
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

	case NewEventTypes of

		% Shortcut:
		[] ->
			ok;

		_ ->
			% As a canvas is registered in wx as a panel (as wx will send events
			% about it) that will be reassigned as a canvas:

			CanvasState = get_instance_state( canvas, CanvasId, TypeTable ),

			Panel = CanvasState#canvas_state.panel,

			% Will defer these extra types of events of the underlying panel to
			% the canvas:
			%
			gui_wx_backend:disconnect( Panel, NewEventTypes,
									   EventTranslationTable )

	end,

	LoopState#loop_state{ event_table=NewEventTable };

unregister_event_types_from( GUIObject, EventTypes, Unsubscribers,
						LoopState=#loop_state{
							event_table=EventTable,
							event_translation_table=EventTranslationTable } ) ->

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:debug_fmt( "Unregistering subscribers ~w "
			"for event types ~p regarding object '~ts'.",
			[ Unsubscribers, EventTypes,
			  gui:object_to_string( GUIObject ) ] ) ),

	[ gui_wx_backend:disconnect( GUIObject, EvType, EventTranslationTable )
											|| EvType <- EventTypes ],

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
get_key_from_object( _WxObject={wx_ref, WxId, WxType, _AnyState } ) ->
	{ WxType, WxId };

get_key_from_object( #myriad_object_ref{ object_type=ObjectType,
										 myriad_instance_id=InstanceId } ) ->
	{ ObjectType, InstanceId }.



% @doc Tells whether the two specified GUI objects match (equality operator).
-spec match( gui_object(), gui_object() ) -> boolean().
% The point is to ignore the included state (last element), which should not
% matter:
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

	NewCanvasState = case gui_canvas:adjust_size( CanvasState ) of

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
-spec get_instance_state( myriad_object_ref(), myriad_type_table(),
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
% current handler.
%
% Refer to gui:trap_event/1 for all details.
%
-spec trap_event( gui_event_object() ) -> void().
trap_event( GUIEventObject ) ->

	% The skip semantics is a bit unclear.
	% 'skip' is strangely here a synonymous of 'propagate'.

	% If no trap_event connection option was used when subscribing, the
	% corresponding events will be propagated - unless in the corresponding
	% event handler the skip/2 function is used with the {skip, false} option;
	% so here, as we want to trap this event:

	% Default of skip/1 is having skip=true.
	wxEvent:skip( GUIEventObject, _Opts=[ { skip, false } ] ).



% @doc Propagates the specified event upward in the widget hierarchy, so that it
% can be processed by parent handlers knowing that, for some event types, by
% default no event propagation is enabled.
%
% Refer to gui:propagate_event/1 for all details.
%
-spec propagate_event( gui_event_object() ) -> void().
propagate_event( GUIEventObject ) ->
	% Default of skip/1 is having skip=true:
	wxEvent:skip( GUIEventObject ).



% @doc Converts the specified wx event into a MyriadGUI one.
-spec wx_to_myriad_event( wx_event(), event_translation_table() ) ->
											gui_event().
wx_to_myriad_event( WxEvent={ wx, WxId, WxObject, UserData, WxEventInfo },
					EventTranslationTable ) ->

	% Example: WxEventType=close_window (the first element being the record
	% name, such as 'wxClose').
	%
	WxEventType = element( 2, WxEventInfo ),

	MyriadEventType =
		gui_wx_backend:from_wx_event_type( WxEventType, EventTranslationTable ),

	EventContext = #event_context{ id=WxId, user_data=UserData,
								   backend_event=WxEvent },

	{ MyriadEventType, [ WxObject, EventContext ] }.



% Helper section.


% Stringification subsection.


% @doc Returns a textual representation of specified event table.
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
			Strings = [ text__utils:format( "for type '~ts', ~ts", [ Type,
					instance_referential_to_string( Referential ) ] )
						|| { Type, Referential } <- Pairs ],
			text_utils:format( "Type table with ~B object types registered: "
				"~ts",
				[ length( Strings ), text_utils:strings_to_string( Strings ) ] )

	end.



% @doc Returns a textual representation of the specified type table.
-spec instance_referential_to_string( instance_referential() ) -> ustring().
instance_referential_to_string( #instance_referential{ instance_count=Count,
										instance_table=InstanceTable } ) ->

	case table:enumerate( InstanceTable ) of

		[] ->
			Count = 0,
			"no instance recorded";

		Pairs ->
			Count = length( Pairs),
			Strings = [ text_utils:format(
							"ID #~B for instance whose state is: ~w",
							[ Id, State ] ) || { Id, State } <- Pairs ],
			text_utils:strings_to_string( Strings, _IndentLevel=1 )

	end.
