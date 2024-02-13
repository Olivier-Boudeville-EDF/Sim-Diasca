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


% @doc Gathering of various facilities for <b>canvas management</b>, for the
% basic, plain ones that MyriadGUI introduced (as opposed to the OpenGL ones,
% which are handled exclusively in our gui_opengl module).
%
% See `gui_canvas_test.erl' for the corresponding test.
%
% See `gui.erl' for more general rendering topics.
%
-module(gui_canvas).



% Implementation notes:
%
% Apparently there is actually no such thing as a plain (non-OpenGL) canvas in
% wx: here, they are actually panels with bitmaps; note however that 'Canvas =
% wxGraphicsContext:create(Win)' and/or wxglcanvas (see gui_opengl) could be
% options apparently, refer to the 'graphicsContext' menu entry of wx:demo() for
% an example, and to https://www.erlang.org/doc/man/wxgraphicscontext.
%
% Another option could be to use a panel as a canvas, like in: 'Canvas =
% wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}])' and/or to create a
% new "class" of widget, with the wx_object behaviour (refer to
% https://www.erlang.org/doc/man/wx_object).
%
% In any case we emulate a basic canvas here, resulting notably in the fact that
% a canvas object is not here a reference onto a wx object, but a stateful
% instance that shall, as a result, be kept from a call to another: as its state
% may change, the result of functions returning a canvas must not be ignored. So
% the MyriadGUI main loop keeps track of the state of all its instantiated
% canvases (there is not a per-canvas process).
%
% Due to their number, canvas operations have been defined here, separately from
% the gui module. For a given action (e.g. resize/2), two functions are defined:
%  - one belonging to the user API (e.g. directly named resize/2), taking as
%  argument a canvas reference, a canvas()), consisting in sending a
%  corresponding message to the MyriadGUI main loop
%  - one implementing the corresponding action, whose name is suffixed by
%  "_impl" (e.g. resize_impl/2), taking as argument a canvas_state(), being
%  executed by the MyriadGUI main loop
%
% A canvas is double-buffered, in the sense that it performs its rendering in an
% in-memory surface (not onscreen), a back-buffer: operations that are performed
% on it will not be visible as long as no blitting of it on the visible buffer
% is done (see blit/1).

% The main event of interest for a canvas is the onRepaintNeeded one: if a user
% code registered this event (see gui:subscribe_to_events/1) for a canvas, this
% code will receive a {onRepaintNeeded, [Canvas, Context]} message whenever
% appropriate.
%
% Knowing that in all cases the user code shall subscribe for each canvas to its
% onRepaintNeeded event, two options can be considered:
%
%  - either (recommended approach) to subscribe also to its onResized event, so
% that only a resizing triggers a new rendering (in its back-buffer); then
% onRepaintNeeded is just a matter of blitting this back-buffer back on the
% visible buffer
%
% - or (less efficient) not to specifically subscribe to its onResized event;
% then onRepaintNeeded shall, prior to blitting, perform a new rendering; this
% is bound to lead to potentially many useless renderings, as repainting happens
% for more reasons that just resizing (e.g. overlapping window, application
% tooltip being displayed, etc.) that may just reuse its previously rendered
% content
%
% See gui_overall_test.erl for an example of use.


% Strangely enough, at least on some configurations (e.g. Arch Linux, XFCE) when
% launching the same program twice (e.g.  gui_overall_test), the initial
% backbuffer will happen to be the final one of the former instance
% (i.e. showing the exact same content, although these OS processes should be
% totally unrelated). This can be detected with this test by clicking the 'Paste
% image' button on its first instance to make the previous buffer different from
% the next one. The second test instance will start with the same buffer
% content.

% Note: these functions are meant to be executed in the context of the MyriadGUI
% main process (not in a user process).



% Rendering of canvas elements.


% Canvas user API:
-export([ create/1, destruct/1,
		  set_draw_color/2, set_fill_color/2, set_background_color/2,
		  get_rgba/2, set_rgba/2,
		  draw_line/3, draw_line/4, draw_lines/2, draw_lines/3,
		  draw_segment/4, draw_polygon/2, draw_label/3,
		  draw_cross/2, draw_cross/3, draw_cross/4,
		  draw_labelled_cross/4, draw_labelled_cross/5,
		  draw_circle/3, draw_circle/4,
		  draw_numbered_points/2,
		  load_image/2, load_image/3,
		  resize/2, blit/1, clear/1 ]).


% Canvas implementation functions (typically called by the MyriadGUI main loop):

-export([ create_instance/1, destruct_instance/1,
		  adjust_size_impl/1, resize_impl/2, clear_impl/1, blit_impl/1,
		  get_size_impl/1, get_client_size_impl/1 ]).



% Color-related rendering, to draw the outline of shapes (with a "pen") and
% possibly fill them (with a "brush").
%
% As for us, we tend to rely on a state machine.
%
% Note: as stated in
% http://docs.wxwidgets.org/stable/wx_wxpenlist.html#wxpenlist pen can be
% created on the fly with no real concern apparently.
%
-export([ set_draw_color_impl/2, set_fill_color_impl/2,
		  set_background_color_impl/2 ]).


% Pixel-level operations.
-export([ get_rgba_impl/2, set_rgba_impl/2 ]).


% Line-related rendering.
-export([ draw_line_impl/3, draw_line_impl/4,
		  draw_lines_impl/2, draw_lines_impl/3,
		  draw_segment_impl/4, draw_polygon_impl/2 ]).



% Rendering of other elements.
-export([ draw_label_impl/3,
		  draw_cross_impl/2, draw_cross_impl/3, draw_cross_impl/4,
		  draw_labelled_cross_impl/4, draw_labelled_cross_impl/5,
		  draw_circle_impl/3, draw_circle_impl/4,
		  draw_numbered_points_impl/2 ]).



% Image loading.
-export([ load_image_impl/2, load_image_impl/3 ]).


% For internal use:
-export([ get_base_panel_events_of_interest/0 ]).



% For related defines:
-include("gui_canvas.hrl").

-type canvas_state() :: #canvas_state{}.
% The actual canvas type we are to use.

-type canvas() :: { 'myriad_object_ref', 'myr_canvas', myriad_instance_id() }.
% A basic canvas (not to be mixed with an OpenGL one, gui_opengl:gl_canvas/0).
%
% This is a specific kind for canvases of the myriad_object_ref() record, thus
% relating to a gui_object().

-export_type([ canvas_state/0, canvas/0 ]).


% For all basic declarations (including distance() and al):
-include("gui_base.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").



% Shorthands:

-type file_path() :: file_utils:file_path().
-type any_file_path() :: file_utils:any_file_path().

-type coordinate() :: linear:coordinate().
-type integer_distance() :: linear:integer_distance().

-type line2() :: linear_2D:line2().

-type point() :: point2:integer_point2().

-type color() :: gui_color:color().
-type color_by_decimal_with_alpha() :: gui_color:color_by_decimal_with_alpha().


-type myriad_instance_id() :: gui_id:myriad_instance_id().

-type dimensions() :: gui:dimensions().
-type size() :: gui:size().
-type length() :: gui:length().
-type panel() :: gui:panel().
-type parent() :: gui:parent().
-type label() :: gui:label().

-type event_type() :: gui_event:event_type().



% Section for the user (canvas) API.


% @doc Creates a (basic, plain) canvas, attached to the specified parent window.
%
% Note: not to be mixed up with gui_opengl:create_canvas/{1,2}.
%
-spec create( parent() ) -> canvas().
create( Parent ) ->
	% Returns the corresponding myriad_object_ref:
	gui:execute_instance_creation( myr_canvas, [ Parent ] ).


% @doc Destructs the specified canvas.
-spec destruct( canvas() ) -> void().
destruct( _Canvas={ myriad_object_ref, myr_canvas, CanvasId } ) ->
	gui:execute_instance_destruction( myr_canvas, CanvasId ).


% @doc Sets the color to be used on the specified canvas for the drawing of the
% outline of shapes.
%
-spec set_draw_color( canvas(), color() ) -> void().
set_draw_color( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Color ) ->
	gui:get_main_loop_pid() ! { setCanvasDrawColor, [ CanvasId, Color ] }.



% @doc Sets the color to be using on the specified canvas for filling surfaces.
%
% An undefined color corresponds to a fully transparent one.
%
-spec set_fill_color( canvas(), maybe( color() ) ) -> void().
set_fill_color( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Color ) ->
	gui:get_main_loop_pid() ! { setCanvasFillColor, [ CanvasId, Color ] }.



% @doc Sets the background color to be using on the specified canvas.
%
% An undefined color corresponds to a fully transparent one.
%
-spec set_background_color( canvas(), maybe( color() ) ) -> void().
set_background_color( _Canvas={ myriad_object_ref, myr_canvas, CanvasId },
					  Color ) ->
	gui:get_main_loop_pid() ! { setCanvasBackgroundColor, [ CanvasId, Color ] }.



% @doc Returns the RGBA value of the pixel at the specified position on the
% specified canvas.
%
-spec get_rgba( canvas(), point() ) -> color_by_decimal_with_alpha().
get_rgba( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Point ) ->
	gui:get_main_loop_pid() ! { getCanvasRGBA, [ CanvasId, Point ], self() },

	receive

		{ notifyCanvasRGBA, Color } ->
			Color

	end.



% @doc Sets the pixel at the specified position on the specified canvas to the
% current RGBA point value.
%
-spec set_rgba( canvas(), point() ) -> void().
set_rgba( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Point ) ->
	gui:get_main_loop_pid() ! { setCanvasRGBA, [ CanvasId, Point ] }.



% @doc Draws a line between the specified two points on the back-buffer of the
% specified canvas, using the current draw color.
%
-spec draw_line( canvas(), point(), point() ) -> void().
draw_line( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, P1, P2 ) ->
	gui:get_main_loop_pid() ! { drawCanvasLine, [ CanvasId, P1, P2 ] }.



% @doc Draws a line between the specified two points on the specified canvas,
% with the specified color.
%
-spec draw_line( canvas(), point(), point(), color() ) -> void().
draw_line( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, P1, P2,
		   Color ) ->
	gui:get_main_loop_pid() ! { drawCanvasLine, [ CanvasId, P1, P2, Color ] }.



% @doc Draws lines between the specified points, on the specified canvas, using
% the current draw color.
%
-spec draw_lines( canvas(), [ point() ] ) -> void().
draw_lines( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Points ) ->
	gui:get_main_loop_pid() ! { drawCanvasLines, [ CanvasId, Points ] }.



% @doc Draws lines between the specified points, on the specified canvas, with
% the specified color.
%
-spec draw_lines( canvas(), [ point() ], color() ) -> void().
draw_lines( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Points,
			Color ) ->
	gui:get_main_loop_pid() ! { drawCanvasLines, [ CanvasId, Points, Color ] }.



% @doc Draws a segment of the line L between the two specified ordinates on the
% specified canvas.
%
% Line L must not have for equation Y=constant (i.e. its A parameter must not be
% null).
%
-spec draw_segment( canvas(), line2(), coordinate(), coordinate() ) -> void().
draw_segment( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, L,
			  Y1, Y2 ) ->
	gui:get_main_loop_pid() ! { drawCanvasSegment, [ CanvasId, L, Y1, Y2 ] }.



% @doc Draws the specified polygon on the specified canvas, closing the lines
% and filling them.
%
-spec draw_polygon( canvas(), [ point() ] ) -> void().
draw_polygon( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Points ) ->
	gui:get_main_loop_pid() ! { drawCanvasPolygon, [ CanvasId, Points ] }.



% @doc Draws the specified label (a plain string) at the specified position, on
% the specified canvas, using the current draw color.
%
-spec draw_label( canvas(), point(), label() ) -> void().
draw_label( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Point,
			Label ) ->
	gui:get_main_loop_pid() ! { drawCanvasLabel, [ CanvasId, Point, Label  ] }.



% @doc Draws an upright cross at the specified location (2D point), with default
% edge length.
%
-spec draw_cross( canvas(), point() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Location ) ->
	gui:get_main_loop_pid() ! { drawCanvasCross, [ CanvasId, Location ] }.



% @doc Draws an upright cross at the specified location (2D point), with the
% specified edge length.
%
-spec draw_cross( canvas(), point(), length() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Location,
			EdgeLength ) ->
	gui:get_main_loop_pid() !
		{ drawCanvasCross, [ CanvasId, Location, EdgeLength ] }.



% @doc Draws an upright cross at the specified location (2D point), with the
% specified edge length and color.
%
-spec draw_cross( canvas(), point(), length(), color() ) -> void().
draw_cross( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Location,
			EdgeLength, Color ) ->
	gui:get_main_loop_pid() !
		{ drawCanvasCross, [ CanvasId, Location, EdgeLength, Color ] }.



% @doc Draws an upright cross at the specified location (2D point), with the
% specified edge length and companion label.
%
-spec draw_labelled_cross( canvas(), point(), length(), label()  ) -> void().
draw_labelled_cross( _Canvas={ myriad_object_ref, myr_canvas, CanvasId },
					 Location, EdgeLength, LabelText ) ->
	gui:get_main_loop_pid() ! { drawCanvasLabelledCross,
							[ CanvasId, Location, EdgeLength, LabelText  ] }.



% @doc Draws an upright cross at the specified location (2D point), with the
% specified edge length and companion label, and with the specified color.
%
-spec draw_labelled_cross( canvas(), point(), length(), color(), label() ) ->
																void().
draw_labelled_cross( _Canvas={ myriad_object_ref, myr_canvas, CanvasId },
					 Location, EdgeLength, Color, LabelText ) ->
	gui:get_main_loop_pid() ! { drawCanvasLabelledCross,
		[ CanvasId, Location, EdgeLength, Color, LabelText  ] }.



% @doc Renders the specified circle (actually, depending on the fill color, it
% may be a disc) on the specified canvas.
%
-spec draw_circle( canvas(), point(), length() ) -> void().
draw_circle( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Center,
			 Radius ) ->
	%trace_utils:debug_fmt( "Drawing circle centered at ~p.", [ Center ] ),
	gui:get_main_loop_pid() !
		{ drawCanvasCircle, [ CanvasId, Center, Radius ] }.



% @doc Renders the specified circle (actually, depending on the fill color, it
% may be a disc) on the specified canvas.
%
-spec draw_circle( canvas(), point(), length(), color() ) -> void().
draw_circle( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Center,
			 Radius, Color ) ->

	%trace_utils:debug_fmt( "Drawing circle centered at ~p, of colour ~p.",
	%                       [ Center, Color ] ),

	gui:get_main_loop_pid() !
		{ drawCanvasCircle, [ CanvasId, Center, Radius, Color ] }.



% @doc Draws the specified list of points, each point being identified in turn
% with one cross and a label, at the same rank order (L1 for the first point of
% the list, L2 for the next, etc.).
%
-spec draw_numbered_points( canvas(), [ point() ] ) -> void().
draw_numbered_points( _Canvas={ myriad_object_ref, myr_canvas, CanvasId },
					  Points ) ->
	gui:get_main_loop_pid() !
		{ drawCanvasNumberedPoints, [ CanvasId, Points ] }.



% @doc Loads the image from the specified path into specified canvas, pasting it
% at its upper left corner.
%
-spec load_image( canvas(), any_file_path() ) -> void().
load_image( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Filename ) ->
	gui:get_main_loop_pid() ! { loadCanvasImage, [ CanvasId, Filename ] }.



% @doc Loads the image from the specified path, pasting it on the specified
% canvas at the specified location.
%
-spec load_image( canvas(), point(), any_file_path() ) -> void().
load_image( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Position,
			FilePath ) ->
	gui:get_main_loop_pid() !
		{ loadCanvasImage, [ CanvasId, Position, FilePath ] }.



% @doc Resizes the specified canvas according to the specified new size.
-spec resize( canvas(), size() ) -> void().
resize( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, NewSize ) ->
	gui:get_main_loop_pid() ! { resizeCanvas, [ CanvasId, NewSize ] }.



% @doc Blits the back-buffer of this canvas onto its visible area.
%
% The back-buffer remains as it was before this call.
%
-spec blit( canvas() ) -> void().
blit( _Canvas={ myriad_object_ref, myr_canvas, CanvasId } ) ->
	gui:get_main_loop_pid() ! { blitCanvas, CanvasId }.


% @doc Clears the specified canvas.
%
% Note: the result will not be visible until the canvas is blitted.
%
-spec clear( canvas() ) -> void().
clear( _Canvas={ myriad_object_ref, myr_canvas, CanvasId } ) ->
	gui:get_main_loop_pid() ! { clearCanvas, CanvasId }.








% Section regarding canvas implementation.
%
% All (method-like) functions operating on a canvas state are called by the
% gui_event main loop; their name shall be suffixed with "_impl" to differ from
% their counterparts belonging to the user API.



% @doc Returns the types of events regarding its panel that a canvas will
% register to by default.
%
-spec get_base_panel_events_of_interest() -> [ event_type() ].
get_base_panel_events_of_interest() ->
	% Both are strictly needed:
	[ onRepaintNeeded, onResized ].



% Section for internal management functions.
%
% These functions are called from the MyriadGUI main loop.


% @doc Creates a canvas, whose parent is the specified widget.
%
% Typically called from gui:execute_instance_creation/2 and thus
% gui_event:process_myriad_creation/4.
%
-spec create_instance( [ parent() ] ) -> { canvas_state(), panel() }.
create_instance( [ Parent ] ) ->

	cond_utils:if_defined( myriad_debug_gui_canvas,
		trace_utils:debug_fmt( "Creating an instance of MyriadGUI canvas, "
			"whose parent is ~w.", [ Parent ] ) ),

	% Could have been: Size = auto,
	Size = { W, H } = gui_widget:get_size( Parent ),

	% Internally, a canvas is mostly an association between a dedicated panel,
	% bitmap and back-buffer:

	% A canvas is to be fully repainted when resized:
	Panel = gui_panel:create( _Pos=auto, Size,
		_Opt=[ { style, [ full_repaint_on_resize ] } ], Parent ),

	% Creates an actual bitmap with the screen color path:
	Bitmap = gui_bitmap:create( W, H ),

	BackBuffer = wxMemoryDC:new( Bitmap ),

	InitialCanvasState = #canvas_state{ panel=Panel, bitmap=Bitmap,
										back_buffer=BackBuffer, size=Size },

	% The current MyriadGUI loop process must be notified whenever the
	% associated panel has to be repainted or is resized, so that this canvas
	% can be notified in turn (to update its internal state, for example its
	% size). The reassign_table mechanism will take care of that:
	%
	gui_wx_backend:connect( Panel, get_base_panel_events_of_interest() ),

	% So when the panel will be resized, a wx 'size' event will be received by
	% our main loop and reassigned to the canvas.

	% Last step is having this canvas manage this resizing.

	{ InitialCanvasState, Panel }.



% @doc Destructs the specified canvas.
%
% Typically called from gui:execute_instance_destruction/2 and thus
% gui_event:process_myriad_destruction/3.
%
-spec destruct_instance( canvas_state() ) -> void().
destruct_instance( CanvasState=#canvas_state{ panel=Panel,
											  bitmap=Bitmap,
											  back_buffer=BackBuffer } ) ->

	cond_utils:if_defined( myriad_debug_gui_canvas,
		trace_utils:debug_fmt( "Destructing an instance of MyriadGUI canvas, "
			"whose state was ~w.", [ CanvasState ] ) ),

	% Reciprocal of create_instance/1:

	% Underlying panel disconnected from loop for all event types:
	% (probably useless as the panel is destructed next)
	%
	gui_wx_backend:disconnect( CanvasState ),

	gui_render:destruct_memory_device_context( BackBuffer ),

	gui_bitmap:destruct( Bitmap ),

	gui_panel:destruct( Panel ).



% @doc Updates the specified canvas state so that it matches any change in size
% of its panel, and tells whether that canvas shall be repainted.
%
% Typically used to be called initially (onShown); now called whenever the
% parent container is resized.
%
-spec adjust_size_impl( canvas_state() ) -> { boolean(), canvas_state() }.
adjust_size_impl( CanvasState=#canvas_state{ panel=Panel, size=Size } ) ->

	cond_utils:if_defined( myriad_debug_gui_canvas,
		trace_utils:debug_fmt( "Adjusting size of canvas '~p': currently ~w, "
			"while panel's is ~w.",
			[ CanvasState, Size, gui_widget:get_size( Panel ) ] ) ),

	case gui_widget:get_size( Panel ) of

		% Nothing to do if the size of the panel already matches the one of its
		% canvas:
		%
		Size ->
			{ _NeedsRepaint=false, CanvasState };

		% Panel was then resized, so canvas should be as well:
		NewSize ->
			{ _NeedsRepaint=true, resize( CanvasState, NewSize ) }

	end.



% @doc Resizes the specified canvas (which, in most cases, should be cleared and
% repainted then).
%
-spec resize_impl( canvas_state(), size() ) -> canvas_state().
resize_impl( CanvasState=#canvas_state{ bitmap=Bitmap,
										back_buffer=BackBuffer },
			 NewSize={ W, H } ) ->

	cond_utils:if_defined( myriad_debug_gui_canvas,
		trace_utils:debug_fmt( "Resizing canvas to ~w.", [ NewSize ] ) ),

	% Regardless of call order and whether either one or both of the next calls
	% are enabled, if an error like {'_wxe_error_',710, {wxDC,setPen,2},
	% {badarg,"This"}} is triggered, then probably some operation replaced these
	% elements yet did not update their reference in the canvas/loop states.
	%
	wxMemoryDC:destroy( BackBuffer ),
	wxBitmap:destroy( Bitmap ),

	NewBitmap = wxBitmap:new( W, H ),

	wxBitmap:create( NewBitmap, W, H,
					 [ { depth, ?wxBITMAP_SCREEN_DEPTH } ] )
		orelse throw( { bitmap_recreation_failed, NewBitmap } ),

	NewBackBuffer = wxMemoryDC:new( NewBitmap ),

	CanvasState#canvas_state{ bitmap=NewBitmap,
							  back_buffer=NewBackBuffer,
							  size=NewSize }.



% @doc Clears the back-buffer of the specified canvas.
-spec clear_impl( canvas_state() ) -> void().
clear_impl( #canvas_state{ back_buffer=BackBuffer } ) ->
	wxMemoryDC:clear( BackBuffer ).



% @doc Blits the back-buffer of this canvas onto its visible area.
%
% Returns the (same) canvas object, for convenience.
%
% The back-buffer remains as it was before this call.
%
-spec blit_impl( canvas_state() ) -> canvas_state().
blit_impl( Canvas=#canvas_state{ panel=Panel,
								 bitmap=Bitmap,
								 back_buffer=BackBuffer,
								 size=Size } ) ->

	VisibleBuffer = wxWindowDC:new( Panel ),

	cond_utils:if_defined( myriad_check_user_interface,
		Size = { wxBitmap:getWidth( Bitmap ), wxBitmap:getHeight( Bitmap ) },
		basic_utils:ignore_unused( Bitmap ) ),

	TopLeft = {0,0},

	wxDC:blit( VisibleBuffer, _Dest=TopLeft, Size, _Source=BackBuffer,
			   _Src=TopLeft ),

	wxWindowDC:destroy( VisibleBuffer ),

	Canvas.



% @doc Returns the size of this canvas, as {IntegerWidth, IntegerHeight}.
-spec get_size_impl( canvas_state() ) -> dimensions().
get_size_impl( #canvas_state{ back_buffer=BackBuffer, size=Size } ) ->

	cond_utils:if_defined( myriad_check_user_interface,
		Size = wxDC:getSize( BackBuffer ),
		basic_utils:ignore_unused( BackBuffer ) ),

	Size.


% @doc Returns the client size of this canvas, as {IntegerWidth, IntegerHeight}.
-spec get_client_size_impl( canvas_state() ) -> dimensions().
get_client_size_impl( CanvasState ) ->
	% For a canvas, we expect the client size to be the size:
	get_size_impl( CanvasState ).





% Color rendering section.


% @doc Sets the color to be used for the drawing of the outline of shapes.
-spec set_draw_color_impl( canvas_state(), color() ) -> void().
set_draw_color_impl( Canvas, Color ) when is_atom( Color ) ->
	set_draw_color_impl( Canvas, gui_color:get_color( Color ) );

set_draw_color_impl( Canvas, Color ) ->
	NewPen = wxPen:new( Color ),
	BackBuffer = Canvas#canvas_state.back_buffer,

	%trace_utils:debug_fmt( "For canvas ~n~p, setting backbuffer ~w "
	%                       "to new pen ~w.", [ Canvas, BackBuffer, NewPen ] ),

	wxDC:setPen( BackBuffer, NewPen ),
	wxPen:destroy( NewPen ).



% @doc Sets the color to be using for filling surfaces.
%
% An 'undefined' color corresponds to a fully transparent one.
%
-spec set_fill_color_impl( canvas_state(), maybe( color() ) ) -> void().
set_fill_color_impl( #canvas_state{ back_buffer=BackBuffer },
					 _MaybeColor=undefined ) ->
	% We want transparency here:
	wxDC:setBrush( BackBuffer, ?transparent_color );

set_fill_color_impl( Canvas, Color ) when is_atom( Color ) ->
	set_fill_color_impl( Canvas, gui_color:get_color( Color ) );

set_fill_color_impl( #canvas_state{ back_buffer=BackBuffer }, Color ) ->
	NewBrush = wxBrush:new( Color ),
	wxDC:setBrush( BackBuffer, NewBrush ),
	wxBrush:destroy( NewBrush ).



% @doc Sets the background color of the specified canvas.
-spec set_background_color_impl( canvas_state(), color() ) -> void().
set_background_color_impl( #canvas_state{ back_buffer=BackBuffer },
						   Color ) ->

	%trace_utils:debug_fmt( "Setting background color of canvas to ~p.",
	%                       [ Color ] ),

	% Must not be used, otherwise double-deallocation core dump:
	%_PreviousBrush = wxMemoryDC:getBrush( BackBuffer ),
	%wxBrush:destroy( PreviousBrush ),

	ActualColor = gui_color:get_color( Color ),

	NewBrush = wxBrush:new( ActualColor ),

	wxMemoryDC:setBackground( BackBuffer, NewBrush ).



% @doc Returns the RGBA value of the pixel at the specified position.
-spec get_rgba_impl( canvas_state(), point() ) -> color_by_decimal_with_alpha().
get_rgba_impl( #canvas_state{ back_buffer=BackBuffer }, Point ) ->

	case wxDC:getPixel( BackBuffer, Point ) of

		{ true, Color } ->
			Color;

		_ ->
			throw( { get_rgba_failed, Point, BackBuffer } )

	end.



% @doc Sets the pixel at the specified position to the current RGBA point value.
-spec set_rgba_impl( canvas_state(), point() ) -> void().
set_rgba_impl( #canvas_state{ back_buffer=BackBuffer }, Point ) ->
	% Uses the color of the current pen:
	wxDC:drawPoint( BackBuffer, Point ).



% Line section.


% @doc Draws a line between the specified two points on the back-buffer of the
% specified canvas, using current draw color.
%
-spec draw_line_impl( canvas_state(), point(), point() ) -> void().
draw_line_impl( #canvas_state{ back_buffer=BackBuffer }, P1, P2 ) ->
	wxDC:drawLine( BackBuffer, P1, P2 ).


% @doc Draws a line between the specified two points on the specified canvas,
% with the specified color.
%
-spec draw_line_impl( canvas_state(), point(), point(), color() ) -> void().
draw_line_impl( Canvas, P1, P2, Color ) ->

	%trace_utils:debug_fmt( "draw_line from ~p to ~p with color ~p.",
	%                       [ P1, P2, Color ] ),

	set_draw_color_impl( Canvas, Color ),
	draw_line_impl( Canvas, P1, P2 ).



% @doc Draws lines between the specified list of points, on the specified
% canvas, using the current draw color.
%
-spec draw_lines_impl( canvas_state(), [ point() ] ) -> void().
draw_lines_impl( #canvas_state{ back_buffer=BackBuffer }, Points ) ->
	wxDC:drawLines( BackBuffer, Points ).


% @doc Draws lines between the specified list of points on the specified canvas,
% with the specified color.
%
-spec draw_lines_impl( canvas_state(), [ point() ], color() ) -> void().
draw_lines_impl( Canvas, Points, Color ) ->
	set_draw_color_impl( Canvas, Color),
	draw_lines_impl( Canvas, Points ).



% @doc Draws a segment of the line L between the two specified ordinates.
%
% Line L must not have for equation Y=constant (i.e. its A parameter must not be
% null).
%
-spec draw_segment_impl( canvas_state(), line2(),
						 coordinate(), coordinate() ) -> void().
draw_segment_impl( Canvas, L, Y1, Y2 ) ->
	draw_line_impl( Canvas,
		{ round( linear_2D:get_abscissa_for_ordinate( L, Y1 ) ), Y1 },
		{ round( linear_2D:get_abscissa_for_ordinate( L, Y2 ) ), Y2 } ).



% @doc Draws the specified polygon, closing the lines and filling them.
-spec draw_polygon_impl( canvas_state(), [ point() ] ) -> void().
draw_polygon_impl( #canvas_state{ back_buffer=BackBuffer }, Points ) ->
	wxDC:drawPolygon( BackBuffer, Points ).



% Section for other elements.


% @doc Draws the specified label (a plain string) at the specified position, on
% the specified canvas, using the current draw color.
%
-spec draw_label_impl( canvas_state(), point(), label() ) -> void().
draw_label_impl( #canvas_state{ back_buffer=BackBuffer }, Point, LabelText ) ->

	% Not needed:
	%wxDC:setBackgroundMode( BackBuffer, ?wxPENSTYLE_TRANSPARENT ),
	wxDC:drawText( BackBuffer, LabelText, Point ).



% @doc Draws an upright cross at the specified location (2D point), with default
% edge length.
%
-spec draw_cross_impl( canvas_state(), point() ) -> void().
draw_cross_impl( Canvas, Location ) ->
	draw_cross_impl( Canvas, Location, _DefaultEdgeLength=4 ).



% @doc Draws an upright cross at the specified location (2D point), with
% the specified edge length.
%
-spec draw_cross_impl( canvas_state(), point(), integer_distance() ) ->
											void().
draw_cross_impl( Canvas, _Location={X,Y}, EdgeLength ) ->
	Offset = EdgeLength div 2,
	% The last pixel of a line is not drawn, hence the +1:
	draw_line_impl( Canvas, { X-Offset, Y }, { X+Offset+1, Y } ),
	draw_line_impl( Canvas, { X, Y-Offset }, { X, Y+Offset+1 } ).



% @doc Draws an upright cross at the specified location (2D point), with the
% specified edge length and color.
%
-spec draw_cross_impl( canvas_state(), point(), integer_distance(), color() ) ->
											void().
draw_cross_impl( Canvas, _Location={X,Y}, EdgeLength, Color ) ->
	Offset = EdgeLength div 2,
	% The last pixel of a line is not drawn, hence the +1:
	draw_line_impl( Canvas, { X-Offset, Y }, { X+Offset+1, Y }, Color ),
	draw_line_impl( Canvas, { X, Y-Offset }, { X, Y+Offset+1 }, Color ).



% @doc Draws an upright cross at the specified location (2D point), with the
% specified edge length and companion label.
%
-spec draw_labelled_cross_impl( canvas_state(), point(), integer_distance(),
								label() ) -> void().
draw_labelled_cross_impl( Canvas, Location={X,Y}, EdgeLength, LabelText ) ->

	draw_cross_impl( Canvas, Location, EdgeLength ),

	% Text a little above and on the right:
	draw_label_impl( Canvas, { X+4, Y-12 }, LabelText ).



% @doc Draws an upright cross at the specified location (2D point), with the
% specified edge length and companion label, and with specified color.
%
-spec draw_labelled_cross_impl( canvas_state(), point(), integer_distance(),
								color(), label() ) -> void().
draw_labelled_cross_impl( Canvas, Location, EdgeLength, Color,
							  LabelText ) ->
	set_draw_color_impl( Canvas, Color ),
	draw_labelled_cross_impl( Canvas, Location, EdgeLength, LabelText ).



% @doc Renders the specified circle (actually, depending on the fill color, it
% may be a disc) on the specified canvas.
%
-spec draw_circle_impl( canvas_state(), point(), integer_distance() ) -> void().
draw_circle_impl( #canvas_state{ back_buffer=BackBuffer }, Center, Radius ) ->
	wxDC:drawCircle( BackBuffer, Center, Radius ).



% @doc Renders the specified circle (actually, depending on the specified fill
% color, it may be a disc) on the specified canvas.
%
-spec draw_circle_impl( canvas_state(), point(), integer_distance(),
						color() ) -> void().
draw_circle_impl( Canvas, Center, Radius, Color ) ->
	set_draw_color_impl( Canvas, Color ),
	draw_circle_impl( Canvas, Center, Radius ).



% @doc Draws the specified list of points, each point being identified in turn
% with one cross and a label (the n-th point will have for label "Pn").
%
-spec draw_numbered_points_impl( canvas_state(), [ point() ] ) -> void().
draw_numbered_points_impl( Canvas, Points ) ->

	LabelledPoints = label_points( Points, _Acc=[], _InitialCount=1 ),

	%trace_utils:debug_fmt( "Labelled points: ~p.", [ LabelledPoints ] ),

	[ draw_labelled_cross_impl( Canvas, Location, _EdgeLength=6, Label )
			|| { Label, Location } <- LabelledPoints ].



% @doc Loads an image from the specified path into specified canvas, pasting it
% at its upper left corner.
%
-spec load_image_impl( canvas_state(), file_path() ) -> void().
load_image_impl( Canvas, Filename ) ->
	load_image_impl( Canvas, _Pos={0,0}, Filename ).



% @doc Loads an image from the specified path into the specified canvas, pasting
% it at the specified location.
%
-spec load_image_impl( canvas_state(), point(), file_path() ) -> void().
load_image_impl( #canvas_state{ back_buffer=BackBuffer }, Position,
				 FilePath ) ->

	case file_utils:is_existing_file( FilePath ) of

		true ->
			Image = wxImage:new( FilePath ),
			Bitmap = wxBitmap:new( Image ),
			wxImage:destroy( Image ),
			wxDC:drawBitmap( BackBuffer, Bitmap, Position ),
			wxBitmap:destroy( Bitmap );

		false ->
			throw( { image_file_not_found, FilePath } )

	end.



% Helper functions.


% @doc Adds a numbered label to each point in list.
%
% Transforms a list of points into a list of {PointLabel,Point} pairs while
% keeping its order.
%
% (helper)
%
label_points( _Points=[], Acc, _Count ) ->
	% Removes the reverse operation induced by iterating below in this function:
	lists:reverse( Acc );

label_points( _Points=[ P | T ], Acc, Count ) ->
	Label = text_utils:format( "P~B", [ Count ] ),
	label_points( T, [ { Label, P } | Acc ], Count+1 ).
