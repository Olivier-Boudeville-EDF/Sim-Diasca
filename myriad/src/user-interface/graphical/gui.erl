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


% @doc Main module of MyriadGUI, a gathering of various <b>facilities for
% Graphical User Interfaces</b> (GUI).
%
% We name this library MyriadGUI (shortened here, whenever it is not ambiguous,
% in 'gui'), a part of Ceylan-Myriad.
%
% The purpose of MyriadGUI is to wrap, complement and improve what we consider
% the best set of gui backends available (previously: gs alone; now: wx, with
% OpenGL), for classical applications and interactive multimedia ones
% (e.g. video games).
%
% wx is the standard Erlang binding to WxWidgets (https://www.wxwidgets.org/).
%
% See `gui_test.erl' for the corresponding test.
%
-module(gui).


% For the canvas_state record:
-include("gui_canvas.hrl").


-include("polygon.hrl").


% Conventions in terms of API definition:
%
% We promote a modular design, where GUI elements (e.g. dialogs) are defined in
% a module of their own (e.g. gui_dialog). We prefer nevertheless a coarser
% granularity that the one of wx/WxWidgets (which define many "classes"). For
% example, with MyriadGUI a single gui_dialog module concentrates the 8
% different types of standard dialogs (rather than introducing at least 8
% different modules).
%
% So the name of the MyriadGUI modules shall be clear and short
% (e.g. gui_button) - we do not recommend importing functions, and the name of
% their functions will generally not include the theme already specified in
% their modules (e.g. gui_button:create/N will be defined, not
% gui_button:create_button/N).
%
% For creation functions, the convention is:
% - to create an instance of X from a module gui_X, create/* functions shall be
% defined and used (not create_X/*) ones; for Y in gui_X, create_Y/* is the way
% to go
% - to start with the essential construction parameters, followed by any
% identifier specification
% - to finish with the parent widget
%
% Options (typed as *_option(), not *_opt()) tend to be placed just before this
% parent last parameter, and are generally to be managed as maybe-lists
% (i.e. either a single option can be directly specified by the user, or a list
% thereof; refer to list_utils:maybe_list/1).
%
% So a general form could be: gui_X:create(A, B, Id, Opts, Parent) -> ...
%
% We prefer:
%  - not representing styles as maybe-lists, to emphasize that multiple style
% elements should always be considered
%  - not defining specific types corresponding to only a list of instances a
%  given of a given type; for example sizer_options() :: [sizer_option()] is not
%  deemed worthwhile as its direct actual type is clearer; moreover in this case
%  a maybe_list/1 may be more relevant
%
% Note that, for a widget type X (e.g. X being 'button'), a X_option() type will
% generally be defined (rather than X_opt()), and that if a style is supported,
% the various style elements will be of the X_style() type (not an
% "opt"-including name, to prevent mix-up).
%
% For method-like functions, the first parameter shall correspond to the
% MyriadGUI instance at hand; for example gui_button:set_label(button(), ...
%
% Many conversions (especially for options, styles, etc.) were to be done in a
% dedicated backend module (e.g. gui_wx_backend), which could have been the sole
% one to include the backend headers. However all MyriadGUI modules depend at
% least on backend (wx) modules (not headers); for example, in gui_font, calls
% to the wxFont module have of course to be made, and wxFont types have to be
% referenced. So the backend cannot be fully hidden in the gui_* widget modules
% anyway.
%
% Moreover a dedicated backend module would quickly concentrate way too many
% elements: it would become too big and would be a single-point of definition
% (hence be anti-modular).
%
% This is hardly a problem now that most conversions are done thanks to the
% gui_generated module, which is obtained from the gui_constants one:
% conversions can be done directly from the widget module (e.g. gui_font) rather
% than a too centralised backend module (e.g. gui_wx_backend). It cuts as well a
% few cross-module calls, requires less type declaration and allows to load
% modules more selectively.
%
% These conversions are generally two-way, between MyriadGUI and the backend of
% choice (see the const_bijective_topics for their implementation).
%
% Links to the WxWidgets documentation shall point to the latest stable version
% rather than a specific one (e.g. https://docs.wxwidgets.org/stable/xxx rather
% than https://docs.wxwidgets.org/3.0/xxx).



% Rendering of GUI elements.
%
% Formerly based on the gs backend, now on the wx one.
%
% Providing improved and enriched APIs for all kinds of GUI.
%
% Relying optionally:
%
% - on (legacy and modern - i.e. shader-based) OpenGL, for efficient 2D/3D
% rendering (see gui_opengl.erl)
%
% - possibly, some day, on esdl (https://github.com/dgud/esdl), for adequate
% lower-level primitives (e.g. management of input devices), if not on an
% hypothetic binding to the Godot engine (https://godotengine.org/)



% General use:
%
% From a user process (a test, an application, etc.) the GUI support is first
% started (gui:start/0), then widgets (windows, frames, panels, buttons, etc.)
% are created (e.g. gui_button:create/4) and the user process subscribes to the
% events it is interested in (as a combination of an event type and a
% widget-as-an-event-emitter; for example:
% gui:subscribe_to_events({onWindowClosed, MainFrame})). It triggers also any
% relevant operation (e.g. clearing widgets, setting various parameters),
% generally shows at least a main frame and records the GUI state that it needs
% (typically containing at least the MyriadGUI references on the widgets that it
% created and that it may need in the future).
%
% Then, when relying on the "direct" mode (as opposed to the "applicative" one),
% the user process enters its own (GUI) specific main loop, from which it will
% receive the events that it subscribed to, to which it will react by performing
% application-specific operations and/or GUI-related operations (creating,
% modifying, deleting widgets).
%
% Beware of asynchronous operations (i.e. the sending of oneways) to the GUI
% main loop process, as they may introduce tricky race conditions. For example,
% if the subscribing to events (e.g. to the main frame being shown) was left
% asynchronous, then the operations coming next from the user code may be fast
% enough so that a wx instance (e.g. the main frame) processes these events
% (e.g.  requesting the main frame to be shown) before even that the GUI main
% loop had a chance to declare its connection to them. This would result in an
% expected event (onShown here) never to be emitted, and thus never to be
% received and processed.
%
% In addition to the race conditions that may occur between message sendings,
% some operations (e.g. at least some OpenGL-related ones) may be done directly,
% thanks to a NIF - thus immediately, beating any message that would still be on
% its way. This could be a message-based resizing immediately followed by a
% direct (NIF-based) OpenGL rendering, which would be done based on the previous
% size.
%
% Another race condition can happen when destructing a resource (e.g. when
% issuing 'gui:destruct_window(MainFrame)' whereas some previous operations may
% not have been processed yet (e.g. gui_canvas:set_draw_color/2), resulting in
% access errors (e.g. {unknown_env,{wxPen,new,2}}) due to an
% use-after-destroy. Short of making most operations synchronous, no real
% solution seems to exist, except some careful design based on series of
% compliant operations. The issue exists maybe even without MyriadGUI, but
% having to rely on a middle process increases the likeliness of such a problem
% due to the extra induced latency.
%
% Generally at least one condition is defined in order to leave that main loop
% and to stop the GUI (gui:stop/0).
%
% See lorenz_test.erl as a full, executable usage example thereof.
%
% The user main loop may also be abstracted out thanks to the applicative
% mode. See gui_event:app_gui_state() for more information.


% Regarding accelerators:
%
% These are key shortcuts (mnemonics) associated to widgets such as buttons or
% menu items.
%
% They are to be prepared by prefixing the chosen accelerator character with an
% ampersand ("&"), typically in the label of the widget of interest.
%
% At runtime:
%  - holding Alt will underline all accelerator keys
%  - holding Alt while pressing the (lowercase) key corresponding to an
%  accelerator will activate the corresponding widget (e.g. the corresponding
%  button will be clicked), provided that this accelerator key is bound exactly
%  once in that window (otherwise no event will be generated); note that
%  many standard accelerator mappings collide (e.g. for "Find" and "Font")
%
% See also https://www.wxwidgets.org/docs/tutorials/using-mnemonics/.




% Type declarations:

-type length() :: integer_distance().
% A length, as an integer number of pixels.

-type width() :: length().
% A width, as an integer number of pixels.

-type height() :: length().
% An height, as an integer number of pixels.


-type aspect_ratio() :: ratio().
% An aspect ratio, typically of a screen, equal to Width/Height.


-type any_length() :: any_distance().
% A length, as a number (integer or floating-point) of pixels.

-type any_width() :: any_length().
% A width, as a number (integer or floating-point) of pixels.

-type any_height() :: any_length().
% An height, as a number (integer or floating-point) of pixels.


-type coordinate() :: linear:integer_coordinate().
% Here, for a GUI, coordinates are an integer number of pixels.


-type point() :: point2:integer_point2().
% A pixel-wise (tuple-based) GUI point (as point2:point2() would allow for
% floating-point coordinates).

-type position() :: point() | 'auto'.
% Position, in pixel coordinates, typically of a widget.


-type dimensions() :: linear_2D:integer_rect_dimensions().
% Dimensions in pixels, as {IntegerWidth,IntegerHeight}.
%
% Note that in general size() shall be preferred to this type, notably to
% designate an attribute of a graphical element - except for example for
% textures or to express dimensions in general.


-type size() :: dimensions().
% Size, typically of a widget.

-type sizing() :: size() | 'auto'.
% Sizing information, typically of a widget.


-type orientation() :: 'vertical' | 'horizontal'.
% A vertical orientation means piling elements top to bottom for example, while
% an horizontal one means left to right, for example.


-type direction() :: orientation() | 'both'.
% A direction, for example to maximise a widget in a container.

-type row_count() :: count().
% A number of rows.

-type column_count() :: count().
% A number of columns.



-type fps() :: count().
% A number of frames per second.
%
% The old Charlie Chaplin movies were shot at 16 frames per second and are
% noticeably jerky.
%
% 60 frames per second is smoother than 30, and 120 is marginally better than
% 60; beyond 120 fps has no real interest, as it exceeds eye perception.



% MVC (Model-View-Controller) section.
%
% Generally the view knows (i.e. has the PID of) the model, the controller knows
% the model, and the model does not know specifically either of them.
%
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
						| 'toggle_button'
						| 'bitmap_button'
						| 'panel'
						| 'gl_canvas'
						| 'status_bar'
						| 'top_level_window'
						| 'dialog'
						| 'frame'
						| 'sizer'
						| 'bitmap'
						| 'memory_device_context'.
% MyriadGUI-translated version of a native wx type, that is of the
% wx_native_object_type(); for example 'window', instead of 'wxWindow'.


-type myriad_object_type() :: 'myr_canvas'.
% The additional widget types introduced by Myriad.


% Also: | all other *_state() that may be introduced in the future.
-type myriad_object_state() :: gui_canvas:canvas_state().
% Records the actual state of a MyriadGUI object.


-type construction_parameters() :: [ term() ].
% The construction parameters of a MyriadGUI object.


-type gui_object() :: wx_object() | myriad_object_ref().
% Reference to a GUI object (often designated as "widget" here), somewhat akin
% to a PID (e.g. {wx_ref, 35, wxFrame, []} or {myriad_object_ref, myr_canvas,
% 12}).


-type wx_server() :: gui_object().
% Alias to designate more clearly the wx server.



% Defining the actual widget types corresponding to wx_object_type():


-type parent() :: widget().
% The parent (widget) of a widget, as they tend to form a hierarchy.


-type label() :: ui:label().
% A label, typically of a widget.
%
% Control characters can be used (e.g. `"\n"', `"\t"'), and
% shortcuts/accelerators (with a `"&"' prefix): all `"&"' characters in the
% label are special and indicate that the following character is a mnemonic for
% this control and can be used to activate it from the keyboard (typically by
% using Alt key in combination with it). To insert a literal ampersand
% character, you need to double it, i.e. to use `"&&"'.


-type event_callback() :: gui_event:event_callback().
% Shorthand type.

-type user_data() :: any().
% User data, as specified in an event subscription/callback.



-type event_subscription_opt() ::

	{ 'id', id() }

  | { 'last_id', id() }

	% Processes the event, but does not propagate it upward in the widget
	% hierarchy afterwards (which is the default for most event types).
	%
	% See trap_event/1 and https://howtos.esperide.org/Erlang.html#using-wx for
	% a clarification of the use of this option.
	%
	% Opposite of 'propagate_event'.
	%
  | 'trap_event'

	% Processes the event and propagates it upward in the widget hierarchy
	% afterwards (some event types by default trap events).
	%
	% See propagate_event/1 and https://howtos.esperide.org/Erlang.html#using-wx
	% for a clarification of the use of this option.
	%
	% Opposite of 'trap_event'.
	%
  | 'propagate_event'

	% Triggers handle_sync_event/3; see the wx_object behaviour:
  | 'callback'

  | { 'callback', event_callback() }
  | { 'user_data', user_data() }.
% Options for the subscription to events.
%
% Note that, with MyriadGUI, by default most types of events are propagated to
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
% of such events (e.g. of the application termination). Once finished it may
% propagate the event, to force its sequential processing.
%
% Conversely, use the 'propagate_event' option or the propagate_event/1 function
% to force event propagation despite an event type implying that by default
% these events are trapped.
%
% See
% https://docs.wxwidgets.org/stable/overview_events.html#overview_events_propagation
% for further information regarding event propagation.


-type event_subscription_options() ::
		event_subscription_opt() | [ event_subscription_opt() ].


-type debug_level_opt() :: 'none' | 'calls' | 'life_cycle'.
% Mapped internally to 'none', 'verbose', 'trace', etc.
%
% See convert_debug_level/1.



-type debug_level() :: debug_level_opt() | [ debug_level_opt() ].

-type error_message() :: term().

-opaque wx_object() :: wx:wx_object().
% MyriadGUI-level type for a wx_object(), that is a #wx_ref record.


-export_type([ service/0,
			   gui_env_pid/0, gui_env_info/0, gui_env_designator/0,
			   backend_identifier/0, backend_information/0,

			   length/0, width/0, height/0, aspect_ratio/0, dimensions/0,
			   any_length/0, any_width/0, any_height/0,
			   coordinate/0, point/0, position/0, size/0, sizing/0,
			   orientation/0, direction/0,
			   row_count/0, column_count/0,
			   fps/0, id/0,

			   model_pid/0, view_pid/0, controller_pid/0,
			   object_type/0, wx_object_type/0,
			   myriad_object_type/0,
			   title/0, label/0, event_callback/0, user_data/0,
			   gui_object/0, wx_server/0,
			   widget/0, parent/0 ]).


-export_type([ loop_pid/0,
			   construction_parameters/0, backend_event/0,
			   event_subscription_options/0,
			   event_subscription_opt/0,
			   debug_level_opt/0, debug_level/0, error_message/0,
			   wx_object/0 ]).


% To avoid unused warnings:
-export_type([ myriad_object_state/0, backend_environment/0 ]).


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
	%
	% (now corresponds directly to the MyriadGUI gui_event main event-loop
	% process, which acts as a simplified gui_id identifier allocator; see the
	% loop_pid entry)
	%
	{ 'id_allocator_pid', id_allocator_pid() },

	% The main, top-level window (if any; generally a frame) of the application:
	{ 'top_level_window', maybe( top_level_window() ) },

	% PID of the MyriadGUI main event loop:
	{ 'loop_pid', loop_pid() },

	% The event types that are trapped by default:
	{ 'trap_set', trap_set() },

	% Any backend-specific top-level server used for the GUI (here wx):
	{ 'backend_server', wx_object() },

	% Any backend-specific, opaque environment term used for the GUI (here wx):
	{ 'backend_env', wx_environment() },


	% OpenGL-related entries:

	% The current OpenGL canvas (if any):
	{ 'gl_canvas', maybe( gl_canvas() ) },

	% The current OpenGL context (if any):
	{ 'gl_context', maybe( gl_context() ) },


	% Mouse-related entries:

	% A table keeping track of the various mouse cursors available:
	{ 'cursor_table', gui_mouse:cursor_table() },

	% The current type of cursor (if any):
	{ 'current_cursor_type', maybe( gui_mouse:cursor_type() ) },

	% The stack (as a list) of the windows that grabbed the mouse cursor:
	{ 'grab_stack', [ window() ] },

	% Tells whether we are in key-released event-handling mode:
	{ 'key_released', boolean() },

	% The coordinates (if any) at which the mouse cursor shall warp:
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
% At least a subset of these entries may be cached from the environment by a
% given process, for easier/faster lookups and updates.
%
% One or multiple entries can be fetched in one go; for example:
%
% GUIEnvPid = gui:get_environment_server(),
%
% GLCanvas = environment:get(gl_canvas, GUIEnvPid), ...
%
% or
%
% [GLCanvas, Context] = environment:get([gl_canvas, gl_context], GUIEnvPid), ...


-type service() :: 'mouse'.
% The various MyriadGUI services that may or may not be enabled.


-type backend_identifier() :: 'gs' % Now obsolete
							| 'wx' % Based on WxWidgets
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


-type loop_pid() :: pid().
% The PID of the MyriadGUI main loop.



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




% Widget-related section.

% Types of GUI elements defined in a dedicated module:
% - gui_button
% - gui_canvas
% - gui_color
% - gui_dialog
% - gui_font
% - gui_image
% - gui_keyboard
% - gui_mouse
% - gui_opengl
% - gui_shader
% - gui_sizer
% - gui_text
% - gui_texture
% - gui_window_manager
%
% Most elements are to be placed in a separate gui_* module - yet not
% necessarily one dedicated to them.


% Related to GUI objects in general:
-export([ set_as_controller/1, set_controller/2 ]).


% Stringification section.
%
% (mostly internal purpose)
%
-export([ object_to_string/1, object_key_to_string/1 ]).


% Regarding MyriadGUI own conventions:
-export([ check_orientation/1 ]).


% To be used by widgets introduced by MyriadGUI:
-export([ execute_instance_creation/2, execute_instance_destruction/2 ]).


% Miscellaneous:
-export([ get_backend_environment/0, set_backend_environment/1,
		  get_main_loop_pid/0, get_main_loop_pid/1,
		  get_id_allocator_pid/0, get_id_allocator_pid/1 ]).



% Internal, silencing exports:
-export([ create_gui_environment/1, create_gui_environment/2,
		  destruct_gui_environment/0, destruct_gui_environment/1,
		  event_interception_callback/2 ]).


% API for module generation:
-export([ generate_support_modules/0 ]).

% As possibly used much by the gui_* modules:
-compile({ inline, [ get_environment_server/0, get_main_loop_pid/0,
					 get_id_allocator_pid/0, get_id_allocator_pid/1 ]}).


% For related, public defines:
-include("gui_base.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").


% For myriad_spawn*:
-include("spawn_utils.hrl").


% Implementation notes:

% This module used to rely on the gs module, whose API was quite simple and
% elegant.
%
% As 'gs' was replaced (quite quickly, with short notice unfortunately) by 'wx'
% (a very interesting Erlang binding to WxWidgets), now we rely on the latter
% and wrap it accordingly, to be future-proof (i.e. to have a fair chance of
% sheltering already-written applications from any backend change - should wx be
% replaced in turn by another backend).
%
% The general convention is still to put as first argument the object on which
% the operation is to be applied (e.g. the window).
%
% See also:
% - the gui_wx_backend module for our use of wx as a backend
% - the gui_canvas module for all canvas-related operations

% The gui module uses its own environment server to record its defaults and also
% elements about its current state.
%
% In general, the opaqueness of types is too difficult to preserve here.


% We tried to make so that this gui module shares as much as possible
% conventions with the more general 'ui' one (for all user interfaces, be them
% graphical or not), at least through base types.


% Event loops.
%
% There are generally two event loops involved here:
%
% - a mandatory, generic, MyriadGUI-internal one (defined in the gui_event
% module), looping over process_event_messages/1 that relies on an (opaque)
% gui_event:loop_state(), running on a dedicated process spawned by the
% start/* functions
%
% - a user-defined, application-specific one, fed by the former internal loop
% (e.g. with onWindowClosed messages), possibly using any client-side state of
% interest (whose definition is fully free); a predefined, integrated
% app_gui_state() may be used instead




% Type shorthands:

-type count() :: basic_utils:count().

-type maybe_list( T ) :: list_utils:maybe_list( T ).


-type ustring() :: text_utils:ustring().


-type ratio() :: math_utils:ratio().

-type integer_distance() :: linear:integer_distance().
-type any_distance() :: linear:any_distance().


-type gui_env_pid() :: environment:env_pid().
-type gui_env_info() :: environment:env_info().
-type gui_env_designator() :: environment:env_designator().


-type title() :: ui:title().

-type widget() :: gui_widget:widget().


-type event_subscription_spec() :: gui_event:event_subscription_spec().
-type event_unsubscription_spec() :: gui_event:event_unsubscription_spec().
-type gui_object_key() :: gui_event:gui_object_key().
-type event_type() :: gui_event:event_type().
-type gui_event_object() :: gui_event:gui_event_object().
-type event_subscriber() :: gui_event:event_subscriber().


-type id() :: gui_id:id().
-type myriad_instance_id() :: gui_id:myriad_instance_id().
-type id_allocator_pid() :: gui_id:id_allocator_pid().



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
% Note that OpenGL-related options are to be specified if wanting to create a GL
% canvas afterwards (see gui_opengl:create_canvas{1,2}).
%
-spec start() -> gui_env_info().
start() ->
	start( [ mouse ] ).



% @doc Starts the MyriadGUI subsystem, with the specified services, or with all
% services while setting specified debug level; returns the information
% regarding its environment.
%
% Note that OpenGL-related options are to be specified if wanting to create a GL
% canvas afterwards (see gui_opengl:create_canvas{1,2}).
%
-spec start( [ service() ] | debug_level() ) -> gui_env_info().
start( Services ) when is_list( Services ) ->

	% Now the identifier allocator is directly integrated in the MyriadGUI
	% (gui_event) main loop:
	%
	%IdAllocPid = ?myriad_spawn_link( fun gui_id:embody_as_id_allocator/0 ),

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
% initialisation (e.g. for the loading of mouse cursors).
%
-spec create_gui_environment( [ service() ] ) -> gui_env_info().
create_gui_environment( Services ) ->
	% Now, at least currently, the MyriadGUI main loop process directly hosts
	% the identifier allocation table (to avoid more messages having to be
	% exchanged between the two); so no standalone id allocator is wanted:
	%
	create_gui_environment( Services, _MaybeIdAllocPid=undefined ).



% @doc Creates and initialises the MyriadGUI environment server; returns the
% information of the just created MyriadGUI environment.
%
% Some services must be specifically declared here, as they require
% initialisation (e.g. for the loading of mouse cursors).
%
-spec create_gui_environment( [ service() ], maybe( id_allocator_pid() ) ) ->
											gui_env_info().
create_gui_environment( Services, MaybeIdAllocPid ) ->

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
	% (e.g. for when creating a plain canvas with no specific context):
	%
	TrapSet = gui_event:get_trapped_event_types( Services ),

	% The event table must be initialised in the spawned process, so that
	% connect/N can use the right actual, first-level subscriber PID/name, which
	% is the internal main loop in charge of the message routing and conversion:

	LoopPid = ?myriad_spawn_link( gui_event, start_main_event_loop,
								  [ WxServer, WxEnv, TrapSet ] ),

	IdAllocPid = case MaybeIdAllocPid of

		undefined ->
			% Then the main loop acts as a simplified id allocator:
			LoopPid;

		IdAllcPid ->
			IdAllcPid

	end,

	% Caches in the calling process and initialises some GUI-related entries
	% (refer to the gui_env_entries define):
	%
	GUIEnvPid = environment:start_link_cached( GUIEnvRegName, [

		{ os_family, OSFamily },
		{ os_name, OSName },

		{ id_allocator_pid, IdAllocPid },

		{ top_level_window, undefined },

		{ loop_pid, LoopPid },
		{ trap_set, TrapSet },

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

	NonMouseServices =:= [] orelse
		throw( { unknown_services, NonMouseServices } ),

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


% @doc Returns the PID of the supposedly already-running MyriadGUI environment
% server.
%
-spec get_environment_server() -> gui_env_pid().
get_environment_server() ->
	environment:get_server( ?gui_env_reg_name ).




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
% (e.g. 'onWindowClosed', 'onResized', 'onShown', etc.; refer to
% gui_event:event_type())
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
% that are inserted (like the new dimensions for a onResized event, to have it
% readily available instead of having to peek in the associated event context).
%
% So typical messages may be:
%  - {onWindowClosed, [WindowGUIObject, WindowId, EventContext]}
%  - {onResized, [WidgetGUIObject, WidgetId, NewSize, EventContext]}
%
% By default (especially for non-command events), subscribing to an event type
% implies that the corresponding events will still be transmitted upward in the
% widget hierarchy, so that other event handlers can apply; if wanting to
% disable this propagation - so that this event is considered to be processed
% for good by the current handler - either specify here the 'trap_event'
% subscription option or, later, in the corresponding event handler, call the
% trap_event/1 function.
%
% Note that trapping non-command events may prevent GUI updates that are to be
% done by the backend.
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
	% be synchronous), otherwise a race condition exists (e.g. the user
	% subscribes to 'onShown' for the main frame, and just after executes
	% 'gui:show(MainFrame)'. If subscribing is non-blocking, then the main frame
	% may (with great probability due to the MyriadGUI process-in-the-middle),
	% be shown before being connected to the main loop, and thus it will not
	% notify the GUI main loop it is shown...

	LoopPid ! { subscribeToEvents, [ SubscribedEvents, SubscriberDesignator ],
				self() },

	cond_utils:if_defined( myriad_debug_gui_events,
		trace_utils:info_fmt( "User process ~w subscribing process ~w to ~w "
			"regarding following events:~n~p.",
			[ self(), SubscriberDesignator, LoopPid, SubscribedEvents ] ) ),

	% Thus synchronous:
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
			[ self(), SubscribedDesignator, LoopPid, UnsubscribedEvents ] ) ),

	% Thus synchronous:
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
% type(s) are generated by the specified (source) object, a transient process is
% spawned and executes the specified event callback function, before
% terminating.
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

	WxUserData = case MaybeUserData of

		undefined ->
			% wx default:
			[];

		UserData ->
			UserData

	end,

	% Recording for later use, when a corresponding event is fired:
	CallbackData = { EventCallbackFun, WxUserData },

	% As we will have to convert back the received wx event into a MyriadGUI
	% one:

	WxCallback = fun event_interception_callback/2,

	% No other option found interesting ('skip' would be ignored here):
	WxOptions = [ { callback, WxCallback }, { userData, CallbackData } ],

	[ begin

		WxEventType = gui_event:to_wx_event_type( ET ),

		%trace_utils:debug_fmt( "Callback-connecting object ~w "
		%   "for event type ~w with options ~w.",
		%   [ SourceGUIObject, WxEventType, WxOptions ] ),

		wxEvtHandler:connect( SourceGUIObject, WxEventType, WxOptions )

	  end || ET <- EventTypes ].



% Internal function defined so that a wx callback can be converted to a
% MyriadGUI one before calling the user-specified callback with it.
%
-spec event_interception_callback( gui_event:wx_event(), wxEvent:wxEvent() ) ->
						void().
event_interception_callback( WxEventRecord=#wx{
			userData={ EventCallbackFun, ActualUserData } },
							 WxEventObject ) ->

	% For example WxEventObject={wx_ref, 92, wxPaintEvent, []}:
	%trace_utils:debug_fmt( "Event interception callback: WxEventObject is ~p",
	%                       [ WxEventObject ] ),

	MyriadGUIEvent = gui_event:wx_to_myriad_event(
		WxEventRecord#wx{ userData=ActualUserData } ),

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
% See propagate_event/1 for the opposite operation (forcing the propagation of
% an event).
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
% that may rely on these resources. A better option is to have the user handler
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




% @doc Batches the sequence of GUI operations encasulated by the specified
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




% Section regarding MyriadGUI own conventions.


% @doc Checks the specified orientation, returns it if it is a legit one,
% otherwise throws an exception.
%
-spec check_orientation( term() ) -> orientation().
check_orientation( vertical ) ->
	vertical;

check_orientation( horizontal ) ->
	horizontal;

check_orientation( Other ) ->
	throw( { invalid_orientation, Other } ).




% Miscellaneous.


% General MyriadGUI helpers.


% @doc Requests the creation of the specified instance (that will be done from
% the MyriadGUI main loop), and returns the corresponding GUI object reference.
%
-spec execute_instance_creation( myriad_object_type(),
		construction_parameters() ) -> myriad_object_ref().
execute_instance_creation( ObjectType, ConstructionParams ) ->

	cond_utils:if_defined( myriad_debug_gui_instances,
		trace_utils:debug_fmt( "Requesting the creation of a '~ts' instance, "
			"based on the following construction parameters:~n~w.",
			[ ObjectType, ConstructionParams ] ) ),

	LoopPid = get_main_loop_pid(),

	% See gui_event:
	LoopPid ! { createInstance, [ ObjectType, ConstructionParams ], self() },

	receive

		% Match on the object type:
		{ instance_created, ObjectType, ObjectRef } ->

			cond_utils:if_defined( myriad_debug_gui_instances,
				trace_utils:debug_fmt(
				   "'~ts' instance created, now referenced as ~w.",
				   [ ObjectType, ObjectRef ] ) ),

			ObjectRef

	end.



% @doc Requests the destruction of the specified instance (that will be done
% from the MyriadGUI main loop).
%
% At least currently, does not return anything and remains asynchronous.
%
-spec execute_instance_destruction( myriad_object_type(),
									myriad_instance_id() ) -> void().
execute_instance_destruction( ObjectType, InstanceId ) ->

	cond_utils:if_defined( myriad_debug_gui_instances,
		trace_utils:debug_fmt( "Requesting the destruction of "
			"the #~B ~ts instance.", [ InstanceId, ObjectType ] ) ),

	get_main_loop_pid() ! { destructInstance, [ ObjectType, InstanceId ] }.



% Section related to GUI objects in general.


% @doc Sets the current process as the controller of the specified GUI object.
-spec set_as_controller( gui_object() ) -> gui_object().
set_as_controller( Object ) ->
	set_controller( Object, _ControllerPid=self() ).


% @doc Sets the process of specified PID as the controller of the specified GUI
% object.
%
-spec set_controller( gui_object(), pid() ) -> gui_object().
set_controller( Object, ControllerPid ) ->

	% Typically takes {wx_ref, Id, WxObjectType, _State=[]}, and returns:
	% {wx_ref, Id, WxObjectTypewxFrame, ControllerPid}.

	wx_object:set_pid( Object, ControllerPid ).



% @doc Returns a textual representation of the specified GUI object.
-spec object_to_string( gui_object() ) -> ustring().
object_to_string( #myriad_object_ref{ object_type=ObjectType,
									  myriad_instance_id=InstanceId } ) ->
	text_utils:format( "~ts-~B", [ ObjectType, InstanceId ] );

object_to_string( { wx_ref, InstanceRef, WxObjectType, _State=[] } ) ->
	% For example {wx_ref,35,wxFrame,[]}
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




% Miscellaneous section.


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
% Typically obtained from get_backend_environment/0 once called from a main
% process.
%
-spec set_backend_environment( backend_environment() ) -> void().
set_backend_environment( WxEnv ) ->
	wx:set_env( WxEnv ).



% @doc Fetches (from the MyriadGUI environment) the PID of the process in charge
% of running the main MyriadGUI loop.
%
% Note that it is sometimes inlined in other gui_* modules (e.g. gui_canvas).
%
-spec get_main_loop_pid() -> loop_pid().
get_main_loop_pid() ->
	environment:get( loop_pid, ?gui_env_reg_name ).


% @doc Fetches from the specified environment the PID of the process in charge
% of running the main MyriadGUI loop.
%
% Note that it is sometimes inlined in other gui_* modules (e.g. gui_canvas).
%
-spec get_main_loop_pid( gui_env_designator() ) -> loop_pid().
get_main_loop_pid( GUIEnvDesignator ) ->
	environment:get( loop_pid, GUIEnvDesignator ).



% @doc Fetches (from the MyriadGUI environment) the PID of the process in charge
% of running the MyriadGUI identifier allocation process.
%
% Allows, as much as possible, to resolve this PID locally, without any message
% sending.
%
% Note that it is sometimes inlined in other gui_* modules (e.g. gui_canvas,
% gui_id).
%
-spec get_id_allocator_pid() -> id_allocator_pid().
get_id_allocator_pid() ->
	environment:get( id_allocator_pid, ?gui_env_reg_name ).


% @doc Fetches from the specified environment the PID of the process in charge
% of running the MyriadGUI identifier allocation process.
%
% Allows, as much as possible, to resolve this PID locally, without any message
% sending.
%
% Note that it is sometimes inlined in other gui_* modules (e.g. gui_canvas,
% gui_id).
%
-spec get_id_allocator_pid( gui_env_designator() ) -> id_allocator_pid().
get_id_allocator_pid( GUIEnvDesignator ) ->
	environment:get( id_allocator_pid, GUIEnvDesignator ).






% Section for the build-time generation of support modules.


% @doc To be called by the 'gui_generated.beam' automatic make target in order
% to generate, here, a (single) module to share the MyriadGUI base constants.
%
-spec generate_support_modules() -> no_return().
generate_support_modules() ->

	TargetModName = gui_generated,

	%trace_bridge:info_fmt( "Generating module '~ts'...", [ TargetModName ] ),

	TopicSpecs =
		[ gui_constants:F() || F <- gui_constants:list_topic_spec_functions() ],

	_ModFilename =
		const_bijective_topics:generate_in_file( TargetModName, TopicSpecs ),

	%trace_bridge:info_fmt( "File '~ts' generated.", [ ModFilename ] ),

	erlang:halt().
