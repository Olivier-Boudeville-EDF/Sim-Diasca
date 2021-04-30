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



% Gathers all elements relative to the MyriadGUI events, including the event
% loop.
%
-module(gui_event).


% Implementation notes.


% Events can be managed as messages or callbacks. We generally prefer the former
% (as messages can be selectively received, any context can be kept in the
% receive loop, no temporary process is created, no wx include is needed hence
% the backend can be well encapsulated, etc.).
%
% Whether an event shall be also dispatched to subsequent handlers may be
% decided by using propagate_event/1.
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


% Function export section.


% Main event primitives:
-export([ start_main_event_loop/2, propagate_event/1, set_instance_state/3 ]).


% Stringification:
-export([ event_table_to_string/1 ]).


% To silence unused warnings:
-export([ reassign_table_to_string/1, get_instance_state/2,
		  type_table_to_string/1, instance_referential_to_string/1 ]).



% Type section.


% Event messages.
%
% Lower-level, backend-specific events are translated into MyriadGUI event
% messages, to be received by their respective event subscribers.



% An event message is a pair whose first element is the event type, as an atom
% (ex: 'onWindowClosed'), and whose second element is a list, whose first
% element is the GUI object that generated that event (the closed window, here),
% and whose last element is the event context (intermediary elements carrying
% event-specific information):
%
% { event_type(), [ gui_object(), ..., event_context() ] }
%
% Ex: { onWindowClosed, [ Window, CloseContext ] }.
%
% Note: these messages respect the WOOPER conventions, and this is done on
% purpose, to facilitate any integration with upper layers.
%
-type event_message() :: { event_type(), [ any() ] }.



% A count of instances of a given object type:
-type instance_count() :: basic_utils:count().


% Event management.
%
% In general we promote managing events thanks to messages rather than thanks to
% callbacks, as the former can access easily to the full context of the program
% whereas the latter is only executed into a transient process (less convenient,
% probably less efficient).

-type event_source() :: wx_event_handler() | myriad_event_handler().


-type wx_event_handler() :: wxEvtHandler:wxEvtHandler().


% Only one currently:
-type myriad_event_handler() :: gui_canvas:canvas().



% Using the wx-event type, leaked by wx.hrl:
%
% (enrich this union whenever needed)
%
-type wx_event_type() :: wx_close_event_type(). % | ...


% Associated to wxCloseEvent:
-type wx_close_event_type() :: 'close_window'
							 | 'end_session'
							 | 'query_end_session'.


% Our own event types, independent from any backend:
-type event_type() :: 'onWindowClosed'.


% The PID of a user calling process:
-type user_pid() :: pid().


% The PID of an event subscriber:
-type event_subscriber_pid() :: pid().


% So that user process(es) can subscribe to GUI events:
-type event_subscription() ::
		{ list_utils:maybe_list( event_type() ),
		  list_utils:maybe_list( gui_object() ),
		  list_utils:maybe_list( event_subscriber_pid() ) }.



% Specifies, for an event subscriber (by default: the calling process), any
% combination of type of events and GUI objects that shall be listened to.
%
-type event_subscription_spec() ::
		list_utils:maybe_list( event_subscription() ).


-export_type([ event_message/0, event_source/0,
			   wx_event_handler/0, myriad_event_handler/0,
			   wx_event_type/0, wx_close_event_type/0,
			   event_type/0, user_pid/0,
			   event_subscriber_pid/0, event_subscription/0,
			   event_subscription_spec/0 ]).



% An indirection table dispatching events according to subscription
% specifications.
%
% For an incoming event, we see this type (virtually, logically) as:
% table( { gui_object(), event_type() },
%            set_utils:set( event_subscriber_pid() ) ):
%
% - the first key is the GUI object (e.g. widget) from which the event emanates
% (ex: a frame)
%
% - the second key is its corresponding (internal) event type (ex:
% 'onWindowClosed')
%
% - the associated value is a list/set of the PID of the subscribers regarding
% this (object,event) combination
%
% Note: two nested tables (one table(), one list_table()) are used also in
% order to ensure that there is up to one entry per GUI object and per event
% type stored.
%
-type event_table() :: table:table( gui_object(), event_dispatch_table() ).


% Tells, for a given event type (e.g. in the context of a specific GUI object),
% to which event subscribers the corresponding GUI messages shall be sent.
%
-type event_dispatch_table() :: list_table:table( event_type(),
												  [ event_subscriber_pid() ] ).


% To replace source events objects (ex: a panel) by others (ex: its associated
% canvas, if any):
%
-type reassign_table() :: table:table( gui_object(), gui_object() ).


% To store the MyriadGUI instances (sorted by types) and manage them like wx
% native objects.
%
% Keys are like 'canvas'.
%
-type myriad_type_table() :: table:table( gui:myriad_object_type(),
										  instance_referential() ).


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
%
-record( instance_referential, {

		% Total count of the instances already created for that type:
		instance_count :: instance_count(),

		instance_table :: table:table( gui:myriad_instance_pid(),
									   gui:myriad_object_state() ) }).

-type instance_referential() :: #instance_referential{}.



% Stores the current MyriadGUI state, as managed by its main event loop.
-record( loop_state, {


		   % Identifier of the current top-level wx server:
		   wx_server :: gui:wx_server(),


		   % To dispatch appropriately the backend-originating events:
		   event_table :: event_table(),


		   % Allows to replace an event source by another.
		   %
		   % For example useful when having defined a canvas (which thus embeds
		   % a wx panel): when the internal event loop receives a 'paint' wx
		   % event for that wx panel, the actual object referred to by the GUI
		   % message that we will send to the user code shall not be that panel,
		   % but the canvas that owns it (for example so that other elements of
		   % that canvas can then be used when the user code processes this
		   % event - like the bitmap or the back-buffer of this canvas).
		   %
		   reassign_table :: reassign_table(),


		   % Stores, by types, the current widget instances that have been
		   % introduced by MyriadGUI to complement the backend (ex: canvas
		   % instances).
		   %
		   type_table :: myriad_type_table(),


		   % List of the MyriadGUI objects that shall be adjusted after a show:
		   %
		   objects_to_adjust=[] :: [ myriad_object_ref() ] }).


-type loop_state() :: #loop_state{}.



% A #wx event record comprises:
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
-type wx_event() :: { 'wx', wx_id(), wx:wx_object(), gui:user_data(),
					  wx_event_info() }.


% A wx-defined record describing an actual event.
%
% WxFoobar record whose first field is 'type', and which may have other
% fields.
%
% Examples of descriptions, as tuples: {wxClose,close_window}, or
% {wxCommand,command_button_clicked,CmdString,CmdInt,...}
%
-type wx_event_info() :: tuple().


-export_type([ wx_event/0, wx_event_info/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type gui_object() :: gui:gui_object().

-type wx_id() :: gui_wx_backend:wx_id().

-type wx_object() :: wx:wx_object().




% For gui_event_context():
-include("gui.hrl").

% For canvas_state():
-include("gui_canvas.hrl").


% For wx headers:
-include("gui_internal_defines.hrl").




% Implementation section.



% Starts the internal, main event loop of MyriadGUI.
%
% The backend events received will result in callbacks to be triggered on their
% respective subscribers.
%
% The goal is to devise a generic event loop, while still being able to be
% notified of all relevant information (and only them).
%
-spec start_main_event_loop( gui:wx_server(), wx:wx_env() ) -> no_return().
start_main_event_loop( WxServer, WxEnv ) ->

	% To be done first, so that we are able to use wx from that process from now
	% on:
	%
	wx:set_env( WxEnv ),

	EmptyTable = table:new(),

	InitialLoopState = #loop_state{ wx_server=WxServer,
									event_table=EmptyTable,
									reassign_table=EmptyTable,
									type_table=EmptyTable },

	%trace_utils:debug_fmt( "Starting main MyriadGUI loop." ] ),

	% Enter the infinite event loop:
	process_event_messages( InitialLoopState ).



% Receives and process all messages (actual main event loop), coming:
%
% - either from controlling processes (typically from application processes
% subscribing to some events)
%
% - or from the backend, that notifies this loop of the actual, lower-level
% events
%
-spec process_event_messages( loop_state() ) -> no_return().
process_event_messages( LoopState ) ->

	%trace_utils:info( "Waiting for event messages..." ),

	NewLoopState = receive

		% So that no large series of repaint requests for the same object pile
		% up:
		FirstWxRepaintEvent=#wx{ obj=SourceObject, event={wxPaint,paint} } ->

			trace_utils:debug_fmt( "Received first repaint event: ~p.",
								   [ FirstWxRepaintEvent ] ),

			process_only_latest_repaint_event( FirstWxRepaintEvent,
											   SourceObject, LoopState );

		OtherEvent ->
			trace_utils:debug_fmt( "Received other event: ~p.",
								   [ OtherEvent ] ),
			process_event_message( OtherEvent, LoopState )

	end,

	process_event_messages( NewLoopState ).




% Event types roughly sorted in clauses by decreasing frequency of appearance:
%
% (defined in lib/wx/include/wx.hrl)
%


% A wx event has been received here:
%
% Structure: { wx, EventSourceId, Obj, UserData, EventInfo }, with EventInfo:
% { WxEventName, EventType, ...}
%
% Ex: { wx, -2006, {wx_ref,35,wxFrame,[]}, [], {wxClose,close_window} }.
%
process_event_message( WxEvent=#wx{ id=EventSourceId, obj=GUIObject,
									userData=UserData, event=WxEventInfo },
					   LoopState ) ->
	process_wx_event( EventSourceId, GUIObject, UserData, WxEventInfo,
					  WxEvent, LoopState );


process_event_message( { setCanvasDrawColor, [ CanvasId, Color ] },
					   LoopState ) ->
	CanvasState = get_canvas_instance_state( CanvasId,
											 LoopState#loop_state.type_table ),
	gui_canvas:set_draw_color( CanvasState, Color ),
	LoopState;


process_event_message( { setCanvasFillColor, [ CanvasId, Color ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:set_fill_color( CanvasState, Color ),
			LoopState;

process_event_message( { setCanvasBackgroundColor, [ CanvasId, Color ] },
					   LoopState ) ->
			%trace_utils:debug_fmt( "Canvas: ~p", [ Canvas ] ),
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			%trace_utils:debug_fmt( "CanvasState: ~p", [ CanvasState ] ),
			gui_canvas:set_background_color( CanvasState, Color ),
			LoopState;

process_event_message( { getCanvasRGB, [ CanvasId, Point ], CallerPid },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			Color = gui_canvas:get_rgb( CanvasState, Point ),
			CallerPid ! { notifyCanvasRGB, Color },
			LoopState;

process_event_message( { setCanvasRGB, [ CanvasId, Point ] }, LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:set_rgb( CanvasState, Point ),
			LoopState;

process_event_message( { drawCanvasLine, [ CanvasId, P1, P2 ] }, LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_line( CanvasState, P1, P2 ),
			LoopState;

process_event_message( { drawCanvasLine, [ CanvasId, P1, P2, Color ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_line( CanvasState, P1, P2, Color ),
			LoopState;

process_event_message( { drawCanvasLines, [ CanvasId, Points ] }, LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_lines( CanvasState, Points ),
			LoopState;

process_event_message( { drawCanvasLines, [ CanvasId, Points, Color ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_lines( CanvasState, Points, Color ),
			LoopState;

process_event_message( { drawCanvasSegment, [ CanvasId, Points ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_segment( CanvasState, Points ),
			LoopState;

process_event_message( { drawCanvasPolygon, [ CanvasId, Points ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_polygon( CanvasState, Points ),
			LoopState;

process_event_message( { drawCanvasLabel, [ CanvasId, Point, Label ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_label( CanvasState, Point, Label ),
			LoopState;

process_event_message( { drawCanvasCross, [ CanvasId, Location ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_cross( CanvasState, Location ),
			LoopState;

process_event_message( { drawCanvasCross, [ CanvasId, Location, EdgeLength ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_cross( CanvasState, Location, EdgeLength ),
			LoopState;

process_event_message( { drawCanvasCross,
						 [ CanvasId, Location, EdgeLength, Color ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_cross( CanvasState, Location, EdgeLength, Color ),
			LoopState;

process_event_message( { drawCanvasLabelledCross,
						 [ CanvasId, Location, EdgeLength, LabelText ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_labelled_cross( CanvasState, Location, EdgeLength,
											LabelText ),
			LoopState;

process_event_message( { drawCanvasLabelledCross,
						 [ CanvasId, Location, EdgeLength, Color, LabelText ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_labelled_cross( CanvasState, Location, EdgeLength,
											Color, LabelText ),
			LoopState;

process_event_message( { drawCanvasCircle, [ CanvasId, Center, Radius ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_circle( CanvasState, Center, Radius ),
			LoopState;

process_event_message( { drawCanvasCircle,
						 [ CanvasId, Center, Radius, Color ] }, LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									   LoopState#loop_state.type_table ),
			gui_canvas:draw_circle( CanvasState, Center, Radius, Color ),
			LoopState;

process_event_message( { drawCanvasNumberedPoints, [ CanvasId, Points ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:draw_numbered_points( CanvasState, Points ),
			LoopState;

process_event_message( { loadCanvasImage, [ CanvasId, Filename ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:load_image( CanvasState, Filename ),
			LoopState;

process_event_message( { loadCanvasImage, [ CanvasId, Position, Filename ] },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:load_image( CanvasState, Position, Filename ),
			LoopState;

process_event_message( { blitCanvas, CanvasId }, LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:blit( CanvasState ),
			LoopState;

process_event_message( { clearCanvas, CanvasId }, LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui_canvas:clear( CanvasState ),
			LoopState;

process_event_message( { setTooltip, [ CanvasId, Label ] }, LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			gui:set_tooltip( CanvasState#canvas_state.panel, Label ),
			LoopState;

process_event_message( { getPanelForCanvas, CanvasId, CallerPid },
					   LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			CallerPid ! { notifyCanvasPanel, CanvasState#canvas_state.panel },
			LoopState;

process_event_message( { getCanvasSize, CanvasId, CallerPid }, LoopState ) ->
			CanvasState = get_canvas_instance_state( CanvasId,
									LoopState#loop_state.type_table ),
			Size = gui_canvas:get_size( CanvasState ),
			CallerPid ! { notifyCanvasSize, Size },
			LoopState;


% MyriadGUI user request (ex: emanating from gui:create_canvas/1):
process_event_message( { createInstance, [ ObjectType, ConstructionParams ],
						 CallerPid }, LoopState ) ->
			process_myriad_creation( ObjectType, ConstructionParams,
									 CallerPid, LoopState );


process_event_message( { subscribeToEvents,
						 [ SubscribedEvents, SubscriberPid ] }, LoopState ) ->

			trace_utils:debug_fmt( "Subscribing process ~w to events ~p.",
								   [ SubscriberPid, SubscribedEvents ] ),

			update_event_loop_tables( SubscribedEvents, SubscriberPid,
									  LoopState );


% To account for example for a silently resized inner panel of a canvas:
%
% (done only once, initially):
%
process_event_message( { adjustObject, ObjectRef }, LoopState ) ->

			trace_utils:debug_fmt( "Recording object to adjust: ~w.",
								   [ ObjectRef ] ),

			NewAdjustList =
				[ ObjectRef | LoopState#loop_state.objects_to_adjust ],

			LoopState#loop_state{ objects_to_adjust=NewAdjustList };


% Currently we update widgets regardless of whether one of their parent windows
% is reported here as shown:
%
process_event_message( { onShow, [ _Windows ] }, LoopState ) ->

			ObjectsToAdjust = LoopState#loop_state.objects_to_adjust,

			trace_utils:debug_fmt( "Adjusting after show: ~p.",
								   [ ObjectsToAdjust ] ),

			EventTable = LoopState#loop_state.event_table,

			NewTypeTable = adjust_objects( ObjectsToAdjust, EventTable,
										   LoopState#loop_state.type_table ),

			LoopState#loop_state{ type_table=NewTypeTable,
								  objects_to_adjust=[] };


process_event_message( UnmatchedEvent, LoopState ) ->
			trace_utils:warning_fmt( "Ignored following unmatched event "
									 "message:~n~p", [ UnmatchedEvent ] ),
			LoopState.



% Drops all intermediate repaint events, and processes the last one, and the
% next non-repaint event.
%
-spec process_only_latest_repaint_event( wx_event(), wx_object(),
										 loop_state() ) -> loop_state().
process_only_latest_repaint_event( CurrentWxRepaintEvent, SourceObject,
								   LoopState ) ->

	receive


		% Ignores all repaints applying to specified object of a series, except
		% the last:
		%
		NewWxRepaintEvent=#wx{ obj=SourceObject, event={wxPaint,paint} } ->

			trace_utils:debug_fmt( "Received next repaint event: ~p.",
								   [ NewWxRepaintEvent ] ),

			process_only_latest_repaint_event( NewWxRepaintEvent, SourceObject,
											   LoopState );


		OtherEvent ->

			% End of a series of repaints; thus process the last one we got:

			#wx{ id=EventSourceId, obj=GUIObject, userData=UserData,
				 event=WxEventInfo } = CurrentWxRepaintEvent,

			% By design this is a wx (repaint) event:
			PostRepaintLoopState = process_wx_event( EventSourceId, GUIObject,
					UserData, WxEventInfo, CurrentWxRepaintEvent, LoopState ),

			trace_utils:debug_fmt( "Received post-repaint event: ~p.",
								   [ OtherEvent ] ),

			% And then process the first non-repaint event that was just
			% received:
			%
			process_event_message( OtherEvent, PostRepaintLoopState )

	% We should not delay arbitrarily the processing of a unique repaint event:
	after 5 ->

		#wx{ id=EventSourceId, obj=GUIObject, userData=UserData,
			 event=WxEventInfo } = CurrentWxRepaintEvent,

		% By design this is a wx (repaint) event:
		process_wx_event( EventSourceId, GUIObject, UserData, WxEventInfo,
						  CurrentWxRepaintEvent, LoopState )

	end.



% Processes specified wx event message.
-spec process_wx_event( wx_id(), wx_object(), gui:user_data(),
					wx_event_info(), wx_event(), loop_state() ) -> loop_state().
process_wx_event( EventSourceId, GUIObject, UserData, WxEventInfo, WxEvent,
				  LoopState=#loop_state{ event_table=EventTable,
										 reassign_table=ReassignTable } ) ->

	ActualGUIObject = case table:lookup_entry( GUIObject, ReassignTable ) of

		key_not_found ->
			trace_utils:info_fmt( "Wx event received about '~ts':~n~p.",
				[ gui:object_to_string( GUIObject ), WxEventInfo ] ),
			GUIObject;


		{ value, TargetGUIObject } ->
			trace_utils:info_fmt( "Wx event received about '~ts', "
				"reassigned to '~ts':~n~p.",
				[ gui:object_to_string( GUIObject ),
				  gui:object_to_string( TargetGUIObject ), WxEventInfo ] ),

			TargetGUIObject

	end,

	% Here some events shall be intercepted and specific processing actions
	% shall be triggered: if a canvas-owned panel is resized, then that canvas
	% shall itself by resized, see gui_canvas:update/2.

	% Then notify the subscribers (could use get_subscribers_for/3):
	case table:lookup_entry( ActualGUIObject, EventTable ) of

		key_not_found ->
			% At least one subscriber would be expected:
			trace_utils:warning_fmt( "No event subscription for GUI "
				"object '~ts', hence ignoring event.",
				[ gui:object_to_string( ActualGUIObject ) ] ),
			LoopState;

		{ value, DispatchTable } ->

			% Example: WxEventType = close_window (first element is the record
			% name, such as wxClose).
			%
			WxEventType = element( 2, WxEventInfo ),

			EventType = gui_wx_backend:from_wx_event_type( WxEventType ),

			case list_table:lookup_entry( EventType, DispatchTable ) of

				{ value, _Subscribers=[] } ->
					trace_utils:error_fmt( "For GUI object '~ts', event type "
					   "'~ts' not registered whereas notified (abnormal).",
					   [ gui:object_to_string( ActualGUIObject ), EventType ] );

				{ value, Subscribers } ->

					trace_utils:debug_fmt( "Sending ~p event to subscribers "
										   "~w.", [ EventType, Subscribers ] ),

					send_event( Subscribers, EventType, EventSourceId,
								ActualGUIObject, UserData, WxEvent )

			end,

			LoopState

	end.



% Returns the subscribers (if any) to the specified GUI object, for the
% specified event type.
%
-spec get_subscribers_for( gui_object(), event_type(), event_table() ) ->
									[ event_subscriber_pid() ].
get_subscribers_for( GUIObject, EventType, EventTable ) ->

	case table:lookup_entry( GUIObject, EventTable ) of

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



% Creates specified MyriadGUI object.
-spec process_myriad_creation( gui:myriad_object_type(),
	gui:construction_parameters(), user_pid(), loop_state() ) -> loop_state().
process_myriad_creation( ObjectType, ConstructionParams, CallerPid,
						 LoopState=#loop_state{ reassign_table=ReassignTable,
												type_table=TypeTable } ) ->

	trace_utils:debug_fmt( "Myriad instance creation request received from ~w, "
		"for type '~ts', with construction parameters ~w.",
		[ CallerPid, ObjectType, ConstructionParams ] ),

	case ObjectType of

		canvas ->

			{ CanvasInitialState, PanelRef } =
				gui_canvas:create_instance( ConstructionParams ),

			{ CanvasRef, NewTypeTable } =
				register_instance( ObjectType, CanvasInitialState, TypeTable ),

			CallerPid ! { instance_created, ObjectType, CanvasRef },

			% An event about its internal panel will be reassigned to its canvas
			% owner:
			%
			NewReassignTable = table:add_new_entry( PanelRef, CanvasRef,
													ReassignTable ),

			% After this canvas is created, its panel, when finally shown, will
			% be sized, with no specific notification; to adapt that canvas
			% (otherwise it will be never be set at the right initial size), we
			% post ourselves an event, the first one to be handled by our event
			% loop:
			%
			self() ! { adjustObject, CanvasRef },

			LoopState#loop_state{ reassign_table=NewReassignTable,
								  type_table=NewTypeTable };


		UnexpectedType ->
			trace_utils:error_fmt( "'~ts' is not a known MyriadGUI type.",
								   [ UnexpectedType ] ),

			throw( { unexpected_myriad_type, UnexpectedType } )


	end.



% Registers the creation of a MyriadGUI instance of specified type and initial
% state, in specified instance table.
%
-spec register_instance( gui:myriad_object_type(), gui:myriad_object_state(),
		 myriad_type_table() ) -> { myriad_object_ref(), myriad_type_table() }.
register_instance( ObjectType, ObjectInitialState, TypeTable ) ->

	trace_utils:info_fmt( "Registering a MyriadGUI instance of type '~ts', "
		"of following state:~n~p.", [ ObjectType, ObjectInitialState ] ),

	{ NewInstanceId, NewInstanceReferential } =
		case table:lookup_entry( ObjectType, TypeTable ) of

		key_not_found ->

			% First instance of its type:
			FirstInstanceId = 1,

			FirstInstanceTable = table:new(
								  [ { FirstInstanceId, ObjectInitialState } ] ),

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
									myriad_instance_pid=NewInstanceId },

	NewTypeTable = table:add_entry( ObjectType, NewInstanceReferential,
								   TypeTable ),

	{ MyriadRef, NewTypeTable }.




% Sends the specified MyriadGUI event to the relevant subscribers.
%
% (helper)
%
-spec send_event( [ event_subscriber_pid() ], event_type(), gui:id(),
		  gui_object(), gui:user_data(), gui:backend_event() ) -> void().
send_event( _Subscribers=[], _EventType, _EventSourceId, _GUIObject, _UserData,
			_Event ) ->
	ok;

send_event( Subscribers, _EventType=onResized, EventSourceId, GUIObject,
			UserData, Event ) ->

	Context = #gui_event_context{ id=EventSourceId, user_data=UserData,
								  backend_event=Event },

	WxEventInfo = Event#wx.event,

	% Defined in wx.hrl:
	NewSize = WxEventInfo#wxSize.size,

	%trace_utils:debug_fmt( "onResized event: new size is ~p.", [ NewSize ] ),

	Msg = { onResized, [ GUIObject, NewSize, Context ] },

	trace_utils:debug_fmt( "Sending event '~p' to ~w.", [ Msg, Subscribers ] ),

	[ SubPid ! Msg || SubPid <- Subscribers ];


send_event( Subscribers, EventType, Id, GUIObject, UserData, Event ) ->

	Context = #gui_event_context{ id=Id, user_data=UserData,
								  backend_event=Event },

	Msg = { EventType, [ GUIObject, Context ] },

	trace_utils:debug_fmt( "Sending event '~p' to ~w.", [ Msg, Subscribers ] ),

	[ SubPid ! Msg || SubPid <- Subscribers ].



% Enriches the specified event table with specified subscription information.
%
% (helper)
%
-spec update_event_loop_tables( event_subscription_spec(),
		event_subscriber_pid(), loop_state() ) -> loop_state().
update_event_loop_tables( _SubscribedEvents=[], _DefaultSubscriberPid,
						  LoopState ) ->
	LoopState;

update_event_loop_tables( _SubscribedEvents=[
	   { EventTypeMaybeList, GUIObjectMaybeList, SubscriberMaybeList } | T ],
	   DefaultSubscriberPid, LoopState ) ->

	EventTypeList = list_utils:ensure_atoms( EventTypeMaybeList ),
	GUIObjectList = list_utils:ensure_tuples( GUIObjectMaybeList ),
	SubscriberList= list_utils:ensure_pids( SubscriberMaybeList ),

	NewLoopState = lists:foldl(
					 fun( Obj, AccState ) ->
						register_event_types_for( Obj, EventTypeList,
												  SubscriberList, AccState )
					 end,
					 _Acc0=LoopState,
					 _List=GUIObjectList ),

	update_event_loop_tables( T, DefaultSubscriberPid, NewLoopState );

update_event_loop_tables( _SubscribedEvents=[
	   { EventTypeMaybeList, GUIObjectMaybeList } | T ],
	   DefaultSubscriberPid, LoopState ) ->
	update_event_loop_tables( [ { EventTypeMaybeList, GUIObjectMaybeList,
		[ DefaultSubscriberPid ] } | T ], DefaultSubscriberPid, LoopState ).


% (helper)
-spec register_event_types_for( gui_object(), [ event_type() ],
				[ event_subscriber_pid() ], loop_state() ) -> loop_state().
register_event_types_for( Canvas={ myriad_object_ref, canvas, CanvasId },
						  EventTypes, Subscribers, LoopState=#loop_state{
											event_table=EventTable,
											type_table=TypeTable } ) ->

	%trace_utils:debug_fmt( "Registering subscribers ~w for event types ~p "
	%					   "regarding canvas '~ts'.", [ Subscribers, EventTypes,
	%						   gui:object_to_string( Canvas ) ] ),

	NewEventTable = record_subscriptions( Canvas, EventTypes, Subscribers,
										  EventTable ),


	% A canvas is registered in wx as a panel (as wx will send events about it)
	% that will be reassigned as a canvas:

	CanvasState = get_instance_state( canvas, CanvasId, TypeTable ),

	Panel = CanvasState#canvas_state.panel,

	% Will defer all events (paint, size) of the underlying panel to the canvas:

	[ gui_wx_backend:connect( Panel, EvType ) || EvType <- EventTypes ],

	LoopState#loop_state{ event_table=NewEventTable };


register_event_types_for( GUIObject, EventTypes, Subscribers,
						  LoopState=#loop_state{ event_table=EventTable } ) ->

	trace_utils:debug_fmt( "Registering subscribers ~w for event types ~p "
		"regarding object '~ts'.",
		[ Subscribers, EventTypes, gui:object_to_string( GUIObject ) ] ),

	% Auto-connection to the current PID (i.e. the one of the internal, main
	% event loop), so that it receives these events for their upcoming
	% dispatching to the actual subscribers:
	%
	[ gui_wx_backend:connect( GUIObject, EvType ) || EvType <- EventTypes ],

	% Now prepare the upcoming routing to the right subscriber:
	NewEventTable = record_subscriptions( GUIObject, EventTypes, Subscribers,
										  EventTable ),

	LoopState#loop_state{ event_table=NewEventTable }.



% Records the specified subscribers for each of the specified event types for
% the specified GUI object.
%
% (helper)
%
-spec record_subscriptions( gui_object(), [ event_type() ],
			[ event_subscriber_pid() ], event_table() ) -> event_table().
record_subscriptions( GUIObject, EventTypes, Subscribers, EventTable ) ->

	NewDispatchTable= case table:lookup_entry( GUIObject, EventTable ) of

		key_not_found ->
			UniqueSubscribers = list_utils:uniquify( Subscribers ),
			Entries = [ { EvType, UniqueSubscribers } || EvType <- EventTypes ],
			list_table:new( Entries );

		{ value, DispatchTable } ->
			update_event_table( EventTypes, Subscribers, DispatchTable )

	end,

	table:add_entry( GUIObject, NewDispatchTable, EventTable ).



% Returns an event dispatch table recording specified event type / subscriber
% associations.
%
-spec update_event_table( [ event_type() ], [ event_subscriber_pid() ],
						  event_dispatch_table() ) -> event_dispatch_table().
update_event_table( _EventTypes=[], _Subscribers, DispatchTable ) ->
	DispatchTable;

update_event_table( _EventTypes=[ EventType | T ], Subscribers,
					DispatchTable ) ->

	NewSubscribers = case list_table:lookup_entry( EventType,
												  DispatchTable ) of

		key_not_found ->
			list_utils:uniquify( Subscribers );

		{ value, CurrentSubscribers } ->
			list_utils:union( CurrentSubscribers, Subscribers )

	end,

	NewDispatchTable = list_table:add_entry( EventType, NewSubscribers,
											 DispatchTable ),

	update_event_table( T, Subscribers, NewDispatchTable ).



% Adjusts the specified MyriadGUI instances.
-spec adjust_objects( [ myriad_object_ref() ], event_table(),
					  myriad_type_table() ) -> myriad_type_table().
adjust_objects( _ObjectsToAdjust=[], _EventTable, TypeTable ) ->
	TypeTable;

adjust_objects( _ObjectsToAdjust=[ CanvasRef=#myriad_object_ref{
									  object_type=canvas,
									  myriad_instance_pid=CanvasId } | T ],
				EventTable, TypeTable ) ->

	CanvasState = get_canvas_instance_state( CanvasId, TypeTable ),

	NewCanvasState = case gui_canvas:adjust_size( CanvasState ) of

		{ _NeedsRepaint=true, AdjCanvasState } ->

			EventType = onRepaintNeeded,

			Subscribers = get_subscribers_for( CanvasRef, EventType,
											   EventTable ),

			send_event( Subscribers, EventType, _EventSourceId=undefined,
						CanvasRef, _UserData=[], _Event=undefined ),

			AdjCanvasState;

		{ _NeedsRepaint=false, AdjCanvasState } ->
			AdjCanvasState

	end,

	NewTypeTable = set_canvas_instance_state( CanvasId, NewCanvasState,
											  TypeTable ),

	adjust_objects( T, EventTable, NewTypeTable ).



% Returns the internal state of the specified canvas instance.
-spec get_canvas_instance_state( gui:myriad_instance_pid(),
						myriad_type_table() ) -> gui:myriad_object_state().
get_canvas_instance_state( CanvasId, TypeTable ) ->
	get_instance_state( _MyriadObjectType=canvas, CanvasId, TypeTable ).



% Returns the internal state of the specified MyriadGUI instance.
-spec get_instance_state( myriad_object_ref(), myriad_type_table() ) ->
								gui:myriad_object_state().
get_instance_state( { myriad_object_ref, MyriadObjectType, InstanceId },
					TypeTable ) ->
	get_instance_state( MyriadObjectType, InstanceId, TypeTable ).



% Returns the internal state of the specified MyriadGUI instance.
-spec get_instance_state( myriad_object_ref(), myriad_type_table(),
						  myriad_type_table() ) -> gui:myriad_object_state().
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



% Sets the internal state of the specified canvas instance.
-spec set_canvas_instance_state( gui:myriad_instance_pid(),
	gui:myriad_object_state(), myriad_type_table() ) -> myriad_type_table().
set_canvas_instance_state( CanvasId, CanvasState, TypeTable ) ->
	set_instance_state( _MyriadObjectType=canvas, CanvasId, CanvasState,
						TypeTable ).



% Returns the internal state of the specified MyriadGUI instance.
-spec set_instance_state( myriad_object_ref(), gui:myriad_object_state(),
						  myriad_type_table() ) -> myriad_type_table().
set_instance_state( { myriad_object_ref, MyriadObjectType, InstanceId },
					InstanceState, TypeTable ) ->
	set_instance_state( MyriadObjectType, InstanceId, InstanceState,
						TypeTable ).



% Returns the internal state of the specified, already-existing MyriadGUI
% instance.
%
-spec set_instance_state( gui:myriad_object_type(), gui:myriad_instance_pid(),
						  gui:myriad_object_state(), myriad_type_table() ) ->
								myriad_type_table().
set_instance_state( MyriadObjectType, InstanceId, InstanceState, TypeTable ) ->

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
propagate_event( #gui_event_context{ backend_event=WxEvent } ) ->

	% Honestly the skip semantics looks a bit unclear.
	% 'skip' is here a synonymous of 'propagate'.

	% Default is having skip=true, so same as:
	% wxEvent:skip( WxEvent, _Opts=[ { skip, true } ] ):
	wxEvent:skip( WxEvent ).





% Helper section.


% Stringification subsection.



% Returns a textual representation of specified event table.
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


% (helper)
-spec dispatch_table_to_string( gui_object(), event_dispatch_table() ) ->
										ustring().
dispatch_table_to_string( GUIObject, DispatchTable ) ->

	EventPairs = list_table:enumerate( DispatchTable ),

	EventStrings = [ text_utils:format( "subscribers for event '~ts': ~w",
										[ EvType, EvSubscribers ] )
					 || { EvType, EvSubscribers } <- EventPairs ],

	EventString = text_utils:strings_to_string( EventStrings,
												_IndentationLevel=1 ),

	text_utils:format( "for GUI object '~ts': ~ts",
					   [ gui:object_to_string( GUIObject ), EventString ] ).



% Returns a textual representation of the specified reassign table.
-spec reassign_table_to_string( reassign_table() ) -> ustring().
reassign_table_to_string( ReassignTable ) ->

	case table:enumerate( ReassignTable ) of

		[] ->
			"no GUI object reassignment defined";

		ObjectPairs ->
			Strings = [ text_utils:format( "events sent to '~ts' will be "
				"reassigned to '~ts'", [ gui:object_to_string( From ),
										 gui:object_to_string( To ) ] )
						|| { From, To } <- ObjectPairs ],
			text_utils:format( "~B GUI object reassignments defined: ~ts",
				[ length( Strings), text_utils:strings_to_string( Strings ) ] )

	end.



% Returns a textual representation of the specified type table.
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



% Returns a textual representation of the specified type table.
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
