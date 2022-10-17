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


% @doc Gathering of various facilities for <b>canvas management</b>, for the
% basic ones that MyriadGUI introduced (as opposed to the OpenGL ones, which are
% handled exclusively in our gui_opengl module).
%
% See `gui_canvas_test.erl' for the corresponding test.
%
% See `gui.erl' for more general rendering topics.
%
-module(gui_canvas).



% Implementation notes:
%
% Apparently there is actually no such thing as a plain canvas in wx: here, they
% are actually panels with bitmaps; note however that 'Canvas =
% wxGraphicsContext:create(Win)' and/or wxglcanvas (see gui_opengl) could be
% options apparently, refer to the 'graphicsContext' menu entry of wx:demo() for
% an example, and to https://www.erlang.org/doc/man/wxgraphicscontext).
%
% Another option could be to use a panel as a canvas, like in: 'Canvas =
% wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}])' and/or to create a
% new "class" of widget, with the wx_object behaviour.
%
% In any case we emulate a basic canvas here, resulting notably in the fact that
% a canvas object is not here a reference onto a wx object, but a stateful
% instance that shall as a result be kept from a call to another: as its state
% may change, the result of functions returning a canvas must not be ignored.
%
% Due to their number, canvas operations have been defined separately from the
% gui module.
%
% A canvas is double-buffered, in the sens that it performs its rendering in an
% in-memory surface (not onscreen), a back-buffer: operations that are performed
% on it will not be visible as long as that no blitting of it on the visible
% buffer is done (see blit/1).

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
% for more reasons that just resizing (ex: overlapping window, application
% tooltip being displayed, etc.)
%
% See gui_overall_test.erl for an example of use.


% Strangely enough, when launching the same program twice (ex:
% gui_overall_test), the initial backbuffer will happen to be the final one of
% the former instance (i.e. showing the exact same content, although these OS
% processes should be totally unrelated). This can be detected with this test by
% clicking the 'Paste image' button on its first instance to make the previous
% buffer different from the next one. The second test instance will start with
% the same buffer content.

% Note: these functions are meant to be executed in the context of the MyriadGUI
% main process (not in a user process).



% Rendering of canvas elements.


% Canvas general operations:
-export([ create_instance/1, adjust_size/1, resize/2, clear/1, blit/1,
		  get_size/1, get_client_size/1, destruct/1 ]).



% Color-related rendering, to draw the outline of shapes (with a "pen") and
% possibly fill them (with a "brush").
%
% As for us, we tend to rely on a state machine.
%
% Note: as stated in
% http://docs.wxwidgets.org/stable/wx_wxpenlist.html#wxpenlist pen can be
% created on the fly with no real concern apparently.
%
-export([ set_draw_color/2, set_fill_color/2, set_background_color/2 ]).


% Pixel-level operations.
-export([ get_rgb/2, set_rgb/2 ]).


% Line-related rendering.
-export([ draw_line/3, draw_line/4, draw_lines/2, draw_lines/3, draw_segment/4,
		  draw_polygon/2 ]).



% Rendering of other elements.
-export([ draw_label/3,
		  draw_cross/2, draw_cross/3, draw_cross/4, draw_labelled_cross/4,
		  draw_labelled_cross/5, draw_circle/3, draw_circle/4,
		  draw_numbered_points/2 ]).



% Image loading.
-export([ load_image/2, load_image/3 ]).


% For internal use:
-export([ get_base_panel_events_of_interest/0 ]).



% For related defines:
-include("gui_canvas.hrl").


-type canvas() :: { gui_object_ref, 'canvas', myriad_instance_id() }.
% A specific kind of gui_object_ref().


-export_type([ canvas_state/0, canvas/0 ]).


% For all basic declarations (including distance() and al):
-include("gui.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").



% Shorthands:

-type file_path() :: file_utils:file_path().

-type coordinate() :: linear:coordinate().
-type integer_distance() :: linear:integer_distance().

-type line2() :: linear_2D:line2().

-type point2() :: point2:point2().

-type color() :: gui_color:color().
-type color_by_decimal_with_alpha() :: gui_color:color_by_decimal_with_alpha().


-type myriad_instance_id() :: gui_id:myriad_instance_id().

-type dimensions() :: gui:dimensions().
-type window() :: gui:window().
-type panel() :: gui:panel().
-type size() :: gui:size().
-type label() :: gui:label().

-type event_type() :: gui_event:event_type().



% @doc Creates a canvas, whose parent is the specified window.
%
% Typically called from gui:execute_instance_creation/2.
%
-spec create_instance( [ window() ] ) -> { canvas_state(), panel() }.
create_instance( [ Parent ] ) ->

	cond_utils:if_defined( myriad_debug_gui_canvas,
		trace_utils:debug_fmt( "Creating an instance of MyriadGUI canvas, "
			"whose parent is ~w.", [ Parent ] ) ),

	% Could have been: Size = auto,
	Size = { W, H } = gui:get_size( Parent ),

	% Internally, a canvas is mostly an association between a dedicated panel,
	% bitmap and back-buffer:

	% A canvas is to be fully repainted when resized:
	Panel = gui:create_panel( Parent, _Pos=auto, Size,
							  _Opt=[ { style, [ full_repaint_on_resize ] } ] ),

	% Created with no data:
	Bitmap = wxBitmap:new( W, H ),

	% Creates an actual bitmap:
	case wxBitmap:create( Bitmap, W, H,
						  [ { depth, ?wxBITMAP_SCREEN_DEPTH } ] ) of

		true ->
			ok;

		false ->
			throw( { bitmap_creation_failed, Bitmap } )

	end,

	BackBuffer = wxMemoryDC:new( Bitmap ),

	InitialCanvasState = #canvas_state{ panel=Panel, bitmap=Bitmap,
										back_buffer=BackBuffer, size=Size },

	% The current MyriadGUI loop process must be notified whenever the
	% associated panel has to be repainted or is resized, so that this canvas
	% can be notified in turn (to update its internal state, for example its
	% size). The reassign_table mechanism will take care of that:
	%
	gui_wx_backend:connect( Panel, get_base_panel_events_of_interest() ),

	% process_myriad_creation/4
	% So when the panel will be resized, a wx 'size' event will be received by
	% our main loop and reassigned to the canvas. Last step is having this
	% canvas manage this resizing:

	{ InitialCanvasState, Panel }.



% @doc Returns the types of events regarding its panel that a canvas will
% register to by default.
%
-spec get_base_panel_events_of_interest() -> [ event_type() ].
get_base_panel_events_of_interest() ->
	% Both are strictly needed:
	[ onRepaintNeeded, onResized ].



% @doc Updates the specified canvas state so that it matches any change in size
% of its panel, and tells whether that canvas shall be repainted.
%
% Typically used to be called initially (onShown); now called whenever the
% parent container is resized.
%
-spec adjust_size( canvas_state() ) -> { boolean(), canvas_state() }.
adjust_size( CanvasState=#canvas_state{ panel=Panel, size=Size } ) ->

	cond_utils:if_defined( myriad_debug_gui_canvas,
		trace_utils:debug_fmt( "Adjusting size of canvas '~p': currently ~w, "
			"while panel's is ~w.",
			[ CanvasState, Size, gui:get_size( Panel ) ] ) ),

	case gui:get_size( Panel ) of

		% Nothing to do if the size of the panel already matches the one of its
		% canvas:
		%
		Size ->
			{ _NeedsRepaint=false, CanvasState };

		% Panel was then resized, so canvas should be as well:
		NewSize ->
			{ _NeedsRepaint=true, resize( CanvasState, NewSize ) }

	end.



% @doc Resizes specified canvas (which, in most cases, should be cleared and
% repainted then).
%
-spec resize( canvas_state(), size() ) -> canvas_state().
resize( CanvasState=#canvas_state{ bitmap=Bitmap, back_buffer=BackBuffer },
		NewSize={ W, H } ) ->

	cond_utils:if_defined( myriad_debug_gui_canvas,
		trace_utils:debug_fmt( "Resizing canvas to ~w.", [ NewSize ] ) ),

	% Regardless of call order and wheter either one or both of the next calls
	% are enabled, if an error like {'_wxe_error_',710, {wxDC,setPen,2},
	% {badarg,"This"}} is triggered, then probably some operation replaced these
	% elements yet did not update their reference in the canvas/loop states.
	%
	wxMemoryDC:destroy( BackBuffer ),
	wxBitmap:destroy( Bitmap ),

	NewBitmap = wxBitmap:new( W, H ),

	case wxBitmap:create( NewBitmap, W, H,
						  [ { depth, ?wxBITMAP_SCREEN_DEPTH } ] ) of

		true ->
			ok;

		false ->
			throw( { bitmap_recreation_failed, NewBitmap } )

	end,

	NewBackBuffer = wxMemoryDC:new( NewBitmap ),

	CanvasState#canvas_state{ bitmap=NewBitmap,
							  back_buffer=NewBackBuffer,
							  size=NewSize }.



% @doc Clears the back-buffer of the specified canvas.
-spec clear( canvas_state() ) -> void().
clear( #canvas_state{ back_buffer=BackBuffer } ) ->
	wxMemoryDC:clear( BackBuffer ).



% @doc Blits the back-buffer of this canvas onto its visible area.
%
% Returns the (same) canvas object, for convenience.
%
% The back-buffer remains as it was before this call.
%
-spec blit( canvas_state() ) -> canvas_state().
blit( Canvas=#canvas_state{ panel=Panel,
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
-spec get_size( canvas_state() ) -> dimensions().
get_size( #canvas_state{ back_buffer=BackBuffer,
						 size=Size } ) ->

	cond_utils:if_defined( myriad_check_user_interface,
		Size = wxDC:getSize( BackBuffer ),
		basic_utils:ignore_unused( BackBuffer ) ),

	Size.


% @doc Returns the client size of this canvas, as {IntegerWidth, IntegerHeight}.
-spec get_client_size( canvas_state() ) -> dimensions().
get_client_size( #canvas_state{ back_buffer=BackBuffer,
								size=Size } ) ->

	% For a canvas, we expect the client size to be the size:

	cond_utils:if_defined( myriad_check_user_interface,
		Size = wxDC:getSize( BackBuffer ),
		basic_utils:ignore_unused( BackBuffer ) ),

	Size.



% @doc Destructs the specified canvas.
-spec destruct( canvas_state() ) -> void().
destruct( #canvas_state{ bitmap=Bitmap, back_buffer=BackBuffer } ) ->
	% Bitmap used to be not destroyed:
	wxBitmap:destroy( Bitmap ),
	wxMemoryDC:destroy( BackBuffer ).



% Color rendering section.


% @doc Sets the color to be used for the drawing of the outline of shapes.
-spec set_draw_color( canvas_state(), color() ) -> void().
set_draw_color( Canvas, Color ) when is_atom( Color ) ->
	set_draw_color( Canvas, gui_color:get_color( Color ) );

set_draw_color( Canvas, Color ) ->
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
-spec set_fill_color( canvas_state(), maybe( color() ) ) -> void().
set_fill_color( #canvas_state{ back_buffer=BackBuffer },
				_MaybeColor=undefined ) ->
	% We want transparency here:
	wxDC:setBrush( BackBuffer, ?transparent_color );

set_fill_color( Canvas, Color ) when is_atom( Color ) ->
	set_fill_color( Canvas, gui_color:get_color( Color ) );

set_fill_color( #canvas_state{ back_buffer=BackBuffer }, Color ) ->
	NewBrush = wxBrush:new( Color ),
	wxDC:setBrush( BackBuffer, NewBrush ),
	wxBrush:destroy( NewBrush ).



% @doc Sets the background color of the specified canvas.
-spec set_background_color( canvas_state(), color() ) -> void().
set_background_color( #canvas_state{ back_buffer=BackBuffer }, Color ) ->

	%trace_utils:debug_fmt( "Setting background color of canvas to ~p.",
	%					   [ Color ] ),

	% Must not be used, otherwise double-deallocation core dump:
	%_PreviousBrush = wxMemoryDC:getBrush( BackBuffer ),
	%wxBrush:destroy( PreviousBrush ),

	ActualColor = gui_color:get_color( Color ),

	NewBrush = wxBrush:new( ActualColor ),

	wxMemoryDC:setBackground( BackBuffer, NewBrush ).



% @doc Returns the RGB value of the pixel at the specified position.
-spec get_rgb( canvas_state(), point2() ) -> color_by_decimal_with_alpha().
get_rgb( #canvas_state{ back_buffer=BackBuffer }, Point ) ->

	case wxDC:getPixel( BackBuffer, Point ) of

		{ true, Color } ->
			Color;

		_ ->
			throw( { get_rgb_failed, Point, BackBuffer } )

	end.



% @doc Sets the pixel at specified position to the current RGB point value.
-spec set_rgb( canvas_state(), point2() ) -> void().
set_rgb( #canvas_state{ back_buffer=BackBuffer }, Point ) ->
	% Uses the color of the current pen:
	wxDC:drawPoint( BackBuffer, Point ).



% Line section.


% @doc Draws a line between the specified two points in the back-buffer of the
% specified canvas, using current draw color.
%
-spec draw_line( canvas_state(), point2(), point2() ) -> void().
draw_line( #canvas_state{ back_buffer=BackBuffer }, P1, P2 ) ->
	wxDC:drawLine( BackBuffer, P1, P2 ).


% @doc Draws a line between the specified two points in specified canvas, with
% specified color.
%
-spec draw_line( canvas_state(), point2(), point2(), color() ) -> void().
draw_line( Canvas, P1, P2, Color ) ->

	%trace_utils:debug_fmt( "draw_line from ~p to ~p with color ~p.",
	%                       [ P1, P2, Color ] ),

	set_draw_color( Canvas, Color ),
	draw_line( Canvas, P1, P2 ).



% @doc Draws lines between the specified list of points, in specified canvas,
% using current draw color.
%
-spec draw_lines( canvas_state(), [ point2() ] ) -> void().
draw_lines( #canvas_state{ back_buffer=BackBuffer }, Points ) ->
	wxDC:drawLines( BackBuffer, Points ).


% @doc Draws lines between the specified list of points in specified canvas,
% with specified color.
%
-spec draw_lines( canvas_state(), [ point2() ], color() ) -> void().
draw_lines( Canvas, Points, Color ) ->
	set_draw_color( Canvas, Color),
	draw_lines( Canvas, Points ).



% @doc Draws a segment of the line L between the two specified ordinates.
%
% Line L must not have for equation Y=constant (i.e. its A parameter must not be
% null).
%
-spec draw_segment( canvas_state(), line2(), coordinate(), coordinate() ) ->
										void().
draw_segment( Canvas, L, Y1, Y2 ) ->
	draw_line( Canvas,
		{ round( linear_2D:get_abscissa_for_ordinate( L, Y1 ) ), Y1 },
		{ round( linear_2D:get_abscissa_for_ordinate( L, Y2 ) ), Y2 } ).



% @doc Draws the specified polygon, closing the lines and filling them.
-spec draw_polygon( canvas_state(), [ point2() ] ) -> void().
draw_polygon( #canvas_state{ back_buffer=BackBuffer }, Points ) ->
	wxDC:drawPolygon( BackBuffer, Points ).



% Section for other elements.


% @doc Draws the specified label (a plain string) at specified position, on
% specified canvas, using the current draw color.
%
-spec draw_label( canvas_state(), point2(), label() ) -> void().
draw_label( #canvas_state{ back_buffer=BackBuffer }, Point, LabelText ) ->

	% Not needed:
	%wxDC:setBackgroundMode( BackBuffer, ?wxPENSTYLE_TRANSPARENT ),
	wxDC:drawText( BackBuffer, LabelText, Point ).



% @doc Draws an upright cross at specified location (2D point), with default
% edge length.
%
-spec draw_cross( canvas_state(), point2() ) -> void().
draw_cross( Canvas, Location ) ->
	draw_cross( Canvas, Location, _DefaultEdgeLength=4 ).



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length.
%
-spec draw_cross( canvas_state(), point2(), integer_distance() ) -> void().
draw_cross( Canvas, _Location={X,Y}, EdgeLength ) ->
	Offset = EdgeLength div 2,
	% The last pixel of a line is not drawn, hence the +1:
	draw_line( Canvas, { X-Offset, Y }, { X+Offset+1, Y } ),
	draw_line( Canvas, { X, Y-Offset }, { X, Y+Offset+1 } ).



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length and color.
%
-spec draw_cross( canvas_state(), point2(), integer_distance(), color() ) ->
														void().
draw_cross( Canvas, _Location={X,Y}, EdgeLength, Color ) ->
	Offset = EdgeLength div 2,
	% The last pixel of a line is not drawn, hence the +1:
	draw_line( Canvas, { X-Offset, Y }, { X+Offset+1, Y }, Color ),
	draw_line( Canvas, { X, Y-Offset }, { X, Y+Offset+1 }, Color ).



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length and companion label.
%
-spec draw_labelled_cross( canvas_state(), point2(), integer_distance(),
						   label() ) -> void().
draw_labelled_cross( Canvas, Location={X,Y}, EdgeLength, LabelText ) ->

	draw_cross( Canvas, Location, EdgeLength ),

	% Text a little above and on the right:
	draw_label( Canvas, { X+4, Y-12 }, LabelText ).



% @doc Draws an upright cross at specified location (2D point), with specified
% edge length and companion label, and with specified color.
%
-spec draw_labelled_cross( canvas_state(), point2(), integer_distance(),
						   color(), label() ) -> void().
draw_labelled_cross( Canvas, Location, EdgeLength, Color, LabelText ) ->
	set_draw_color( Canvas, Color ),
	draw_labelled_cross( Canvas, Location, EdgeLength, LabelText ).



% @doc Renders specified circle (actually, depending on the fill color, it may
% be a disc) in specified canvas.
%
-spec draw_circle( canvas_state(), point2(), integer_distance() ) -> void().
draw_circle( #canvas_state{ back_buffer=BackBuffer }, Center, Radius ) ->
	wxDC:drawCircle( BackBuffer, Center, Radius ).



% @doc Renders specified circle (actually, depending on the specified fill
% color, it may be a disc) in specified canvas.
%
-spec draw_circle( canvas_state(), point2(), integer_distance(), color() ) ->
				void().
draw_circle( Canvas, Center, Radius, Color ) ->
	set_draw_color( Canvas, Color ),
	draw_circle( Canvas, Center, Radius ).



% @doc Draws specified list of points, each point being identified in turn with
% one cross and a label (the n-th point will have for label "Pn").
%
-spec draw_numbered_points( canvas_state(), [ point2() ] ) -> void().
draw_numbered_points( Canvas, Points ) ->

	LabelledPoints = label_points( Points, _Acc=[], _InitialCount=1 ),

	%trace_utils:debug_fmt( "Labelled points: ~p.", [ LabelledPoints ] ),

	[ draw_labelled_cross( Canvas, Location, _EdgeLength=6, Label )
			|| { Label, Location } <- LabelledPoints  ].



% @doc Loads image from specified path into specified canvas, pasting it at its
% upper left corner.
%
-spec load_image( canvas_state(), file_path() ) -> void().
load_image( Canvas, Filename ) ->
	load_image( Canvas, _Pos={0,0}, Filename ).



% @doc Loads image from specified path into the specified canvas, pasting it at
% specified location.
%
-spec load_image( canvas_state(), point2(), file_path() ) -> void().
load_image( #canvas_state{ back_buffer=BackBuffer }, Position, FilePath ) ->

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
