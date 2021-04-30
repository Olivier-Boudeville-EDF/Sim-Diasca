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



% Gathering of various facilities for canvas management.
%
% See gui_canvas_test.erl for the corresponding test.
%
% See gui.erl for more general rendering topics.
%
-module(gui_canvas).




% Rendering of canvas elements.


% Implementation notes:


% Canvas general operations:
%
-export([ create_instance/1, adjust_size/1, resize/2, clear/1, blit/1,
		  get_size/1, destroy/1 ]).



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


% Implementation notes:
%
% There is actually no such thing as a plain canvas in wx: here, they are
% actually panels with bitmaps.
%
% So we emulate a canvas here, resulting notably in the fact that a canvas
% object is not a reference onto a wx object, but a stateful instance that shall
% as a result be kept from a call to another: as its state may change, the
% result of functions returning a canvas must not be ignored.

% Due to their number, canvas operations have been defined separately from the
% gui module.



% Implementation notes:
%
% There is actually no such thing as a plain canvas in wx: here, they are
% actually panels with bitmaps.
%
% So we emulate a canvas here, resulting notably in the fact that a canvas
% object is not a reference onto a wx object, but a stateful instance that shall
% as a result be kept from a call to another: as its state may change, the
% result of functions returning a canvas must not be ignored.

% Due to their number, canvas operations have been defined separately from the
% gui module.



% For related defines:
-include("gui_canvas.hrl").


% A specific kind of gui_object_ref():
-type canvas() :: { gui_object_ref, 'canvas', gui:myriad_instance_pid() }.


-export_type([ canvas_state/0, gl_canvas/0, canvas/0 ]).


% For all basic declarations (including distance() and al):
-include("gui.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").



% Creates a canvas, whose parent is the specified window.
%
% Typically called from gui:process_myriad_creation/4.
%
-spec create_instance( [ gui:window() ] ) -> { canvas_state(), gui:panel() }.
create_instance( [ Parent ] ) ->

	% Could have been: Size = auto,
	Size = { W, H } = gui:get_size( Parent ),

	% Internally, a canvas is mostly an association between a dedicated panel,
	% bitmap and back-buffer:

	Panel = gui:create_panel( Parent, _Pos=auto, Size,
						  _Opt=[ { style, [ full_repaint_on_resize ] } ] ),

	Bitmap = wxBitmap:new( W, H ),

	BackBuffer = wxMemoryDC:new( Bitmap ),

	InitialCanvasState = #canvas_state{ panel=Panel, bitmap=Bitmap,
										back_buffer=BackBuffer, size=Size },

	{ InitialCanvasState, Panel }.



% Updates the specified canvas state so that it matches any change in size of
% its panel, and tells whether that canvas shall be repainted.
%
-spec adjust_size( canvas_state() ) -> { boolean(), canvas_state() }.
adjust_size( Canvas=#canvas_state{ panel=Panel, size=Size } ) ->

	trace_utils:debug_fmt( "Adjusting size of canvas '~p': currently ~w, "
						   "while panel: ~w.",
						   [ Canvas, Size, gui:get_size( Panel ) ] ),

	case gui:get_size( Panel ) of

		Size ->
			{ _NeedsRepaint=false, Canvas };

		% Panel was then resized, so canvas should be as well:
		NewSize ->
			{ _NeedsRepaint=true, resize( Canvas, NewSize ) }

	end.



% Resizes specified canvas (which in most cases should be cleared and repainted
% then).
%
-spec resize( canvas_state(), gui:size() ) -> canvas_state().
resize( Canvas=#canvas_state{ bitmap=Bitmap, back_buffer=BackBuffer },
		NewSize={ W, H } ) ->

	trace_utils:debug_fmt( "Resizing canvas to ~w.", [ NewSize ] ),

	wxBitmap:destroy( Bitmap ),
	wxMemoryDC:destroy( BackBuffer ),

	NewBitmap = wxBitmap:new( W, H ),
	NewBackBuffer = wxMemoryDC:new( NewBitmap ),

	Canvas#canvas_state{ bitmap=NewBitmap,
						 back_buffer=NewBackBuffer,
						 size=NewSize }.




% Clears the back-buffer of the specified canvas.
-spec clear( canvas_state() ) -> void().
clear( #canvas_state{ back_buffer=BackBuffer } ) ->
	wxMemoryDC:clear( BackBuffer ).



% Blits the back-buffer of this canvas onto its visible area.
%
% Returns the (same) canvas object, for convenience.
%
% The back-buffer remains as it was before this call.
%
-spec blit( canvas_state() ) -> canvas_state().
blit( Canvas=#canvas_state{ panel=Panel,
							bitmap=Bitmap,
							back_buffer=BackBuffer } ) ->

	VisibleBuffer = wxWindowDC:new( Panel ),

	wxDC:blit( VisibleBuffer, {0,0},
		  { wxBitmap:getWidth( Bitmap ), wxBitmap:getHeight( Bitmap ) },
		  BackBuffer, {0,0} ),

	wxWindowDC:destroy( VisibleBuffer ),

	Canvas.



% Returns the size of this canvas, as { IntegerWidth, IntegerHeight }.
-spec get_size( canvas_state() ) -> linear_2D:dimensions().
get_size( #canvas_state{ back_buffer=BackBuffer } ) ->
	wxDC:getSize( BackBuffer ).



% Destroys specified canvas.
-spec destroy( canvas_state() ) -> void().
destroy( #canvas_state{ back_buffer=BackBuffer } ) ->
	wxMemoryDC:destroy( BackBuffer ).



% Color rendering section.


% Sets the color to be used for the drawing of the outline of shapes.
-spec set_draw_color( canvas_state(), gui_color:color() ) -> void().
set_draw_color( Canvas, Color ) when is_atom( Color ) ->
	set_draw_color( Canvas, gui_color:get_color( Color ) );

set_draw_color( Canvas, Color ) ->
	NewPen = wxPen:new( Color ),
	wxDC:setPen( Canvas#canvas_state.back_buffer, NewPen ),
	wxPen:destroy( NewPen ).



% Sets the color to be using for filling surfaces.
-spec set_fill_color( canvas_state(), gui_color:color() ) -> void().
set_fill_color( #canvas_state{ back_buffer=BackBuffer }, _Color=none ) ->
	% We want transparency here:
	wxDC:setBrush( BackBuffer, ?transparent_color );

set_fill_color( Canvas, Color ) when is_atom( Color ) ->
	set_fill_color( Canvas, gui_color:get_color( Color ) );

set_fill_color( #canvas_state{ back_buffer=BackBuffer }, Color ) ->
	NewBrush = wxBrush:new( Color ),
	wxDC:setBrush( BackBuffer, NewBrush ),
	wxBrush:destroy( NewBrush ).



% Sets the background color of the specified canvas.
-spec set_background_color( canvas_state(), gui_color:color() ) -> void().
set_background_color( #canvas_state{ back_buffer=BackBuffer }, Color ) ->

	trace_utils:debug_fmt( "Setting background color of canvas to ~p.",
						   [ Color ] ),

	% Must not be used, other double-deallocation core dump:
	%_PreviousBrush = wxMemoryDC:getBrush( BackBuffer ),
	%wxBrush:destroy( PreviousBrush ),

	ActualColor = gui_color:get_color( Color ),

	NewBrush = wxBrush:new( ActualColor ),

	wxMemoryDC:setBackground( BackBuffer, NewBrush ).



% Returns the RGB value of the pixel at specified position.
-spec get_rgb( canvas_state(), linear_2D:point() ) ->
					 gui_color:color_by_decimal_with_alpha().
get_rgb( #canvas_state{ back_buffer=BackBuffer }, Point ) ->

	case wxDC:getPixel( BackBuffer, Point ) of

		{ true, Color } ->
			Color;

		_ ->
			throw( { get_rgb_failed, Point } )

	end.



% Sets the pixel at specified position to the current RGB point value.
-spec set_rgb( canvas_state(), linear_2D:point() ) -> void().
set_rgb( #canvas_state{ back_buffer=BackBuffer }, Point ) ->

	% Uses the color of the current pen:
	wxDC:drawPoint( BackBuffer, Point ).



% Line section.


% Draws a line between the specified two points in the back-buffer of the
% specified canvas, using current draw color.
%
-spec draw_line( canvas_state(), linear_2D:point(), linear_2D:point() ) ->
					   void().
draw_line( #canvas_state{ back_buffer=BackBuffer }, P1, P2 ) ->
	wxDC:drawLine( BackBuffer, P1, P2 ).


% Draws a line between the specified two points in specified canvas, with
% specified color.
%
-spec draw_line( canvas_state(), linear_2D:point(), linear_2D:point(),
				 gui_color:color() ) -> void().
draw_line( Canvas, P1, P2, Color ) ->

	%trace_utils:debug_fmt( "draw_line from ~p to ~p with color ~p.",
	%  [ P1, P2, Color ] ),

	set_draw_color( Canvas, Color ),

	draw_line( Canvas, P1, P2 ).



% Draws lines between the specified list of points, in specified canvas, using
% current draw color.
%
-spec draw_lines( canvas_state(), [ linear_2D:point() ] ) -> void().
draw_lines( #canvas_state{ back_buffer=BackBuffer }, Points ) ->
	wxDC:drawLines( BackBuffer, Points ).


% Draws lines between the specified list of points in specified canvas, with
% specified color.
%
-spec draw_lines( canvas_state(), [ linear_2D:point() ], gui_color:color() ) ->
						void().
draw_lines( Canvas, Points, Color ) ->
	set_draw_color( Canvas, Color),
	draw_lines( Canvas, Points ).



% Draws a segment of the line L between the two specified ordinates.
%
% Line L must not have for equation Y=constant (i.e. its A parameter must not be
% null).
%
-spec draw_segment( canvas_state(), linear_2D:line(), linear:coordinate(),
					linear:coordinate() ) -> void().
draw_segment( Canvas, L, Y1, Y2 ) ->
	draw_line( Canvas,
			{ round( linear_2D:get_abscissa_for_ordinate( L, Y1 ) ), Y1 },
			{ round( linear_2D:get_abscissa_for_ordinate( L, Y2 ) ), Y2 } ).



% Draws the specified polygon, closing the lines and filling them.
-spec draw_polygon( canvas_state(), [ linear_2D:point() ] ) -> void().
draw_polygon( #canvas_state{ back_buffer=BackBuffer }, Points ) ->
	wxDC:drawPolygon( BackBuffer, Points ).



% Section for other elements.


% Draws the specified label (a plain string) at specified position, on specified
% canvas, using the current draw color.
%
-spec draw_label( canvas_state(), linear_2D:point(), gui:label() ) -> void().
draw_label( #canvas_state{ back_buffer=BackBuffer }, Point, LabelText ) ->
	wxDC:drawText( BackBuffer, LabelText, Point ).



% Draws an upright cross at specified location (2D point), with default edge
% length.
%
-spec draw_cross( canvas_state(), linear_2D:point() ) -> void().
draw_cross( Canvas, Location ) ->
	draw_cross( Canvas, Location, _DefaultEdgeLength=4 ).



% Draws an upright cross at specified location (2D point), with specified edge
% length.
%
-spec draw_cross( canvas_state(), linear_2D:point(),
				  linear:integer_distance() ) -> void().
draw_cross( Canvas, _Location={X,Y}, EdgeLength ) ->
	Offset = EdgeLength div 2,
	% The last pixel of a line is not drawn, hence the +1:
	draw_line( Canvas, { X-Offset, Y }, { X+Offset+1, Y } ),
	draw_line( Canvas, { X, Y-Offset }, { X, Y+Offset+1 } ).



% Draws an upright cross at specified location (2D point), with specified edge
% length and color.
%
-spec draw_cross( canvas_state(), linear_2D:point(), linear:integer_distance(),
				  gui_color:color() ) -> void().
draw_cross( Canvas, _Location={X,Y}, EdgeLength, Color ) ->
	Offset = EdgeLength div 2,
	% The last pixel of a line is not drawn, hence the +1:
	draw_line( Canvas, { X-Offset, Y }, { X+Offset+1, Y }, Color ),
	draw_line( Canvas, { X, Y-Offset }, { X, Y+Offset+1 }, Color ).



% Draws an upright cross at specified location (2D point), with specified edge
% length and companion label.
%
-spec draw_labelled_cross( canvas_state(), linear_2D:point(),
	   linear:integer_distance(), gui:label() ) -> void().
draw_labelled_cross( Canvas, Location={X,Y}, EdgeLength, LabelText ) ->

	draw_cross( Canvas, Location, EdgeLength ),

	% Text a little above and on the right:
	draw_label( Canvas, { X+4, Y-12 }, LabelText ).



% Draws an upright cross at specified location (2D point), with specified edge
% length and companion label, and with specified color.
%
-spec draw_labelled_cross( canvas_state(), linear_2D:point(),
	   linear:integer_distance(), gui_color:color(), gui:label() ) -> void().
draw_labelled_cross( Canvas, Location, EdgeLength, Color, LabelText ) ->
	set_draw_color( Canvas, Color ),
	draw_labelled_cross( Canvas, Location, EdgeLength, LabelText ).



% Renders specified circle (actually, depending on the fill color, it may be a
% disc) in specified canvas.
%
-spec draw_circle( canvas_state(), linear_2D:point(),
				   linear:integer_distance() ) -> void().
draw_circle( #canvas_state{ back_buffer=BackBuffer }, Center, Radius ) ->
	wxDC:drawCircle( BackBuffer, Center, Radius ).



% Renders specified circle (actually, depending on the specified fill color, it
% may be a disc) in specified canvas.
%
-spec draw_circle( canvas_state(), linear_2D:point(), linear:integer_distance(),
				   gui_color:color() ) -> void().
draw_circle( Canvas, Center, Radius, Color ) ->
	set_draw_color( Canvas, Color ),
	draw_circle( Canvas, Center, Radius ).



% Draws specified list of points, each point being identified in turn with one
% cross and a label (the n-th point will have for label "Pn").
%
-spec draw_numbered_points( canvas_state(), [ linear_2D:point() ] ) -> void().
draw_numbered_points( Canvas, Points ) ->

	LabelledPoints = label_points( Points, _Acc=[], _InitialCount=1 ),

	%trace_utils:debug_fmt( "Labelled points: ~p.~n", [ LabelledPoints ] ),
	[ draw_labelled_cross( Canvas, Location, _EdgeLength=6, Label )
	  || { Label, Location } <- LabelledPoints  ].



% Loads image from specified path into specified canvas, pasting it at its upper
% left corner.
%
-spec load_image( canvas_state(), file_utils:file_name() ) -> void().
load_image( Canvas, Filename ) ->
	load_image( Canvas, _Pos={0,0}, Filename ).



% Loads image from specified path into specified canvas, pasting it at specified
% location.
%
-spec load_image( canvas_state(), linear_2D:point(), file_utils:file_name() ) ->
						void().
load_image( #canvas_state{ back_buffer=BackBuffer }, Position, Filename ) ->

	case file_utils:is_existing_file( Filename ) of

		true ->
			Image = wxImage:new( Filename ),

			Bitmap = wxBitmap:new( Image ),

			wxImage:destroy( Image ),

			wxDC:drawBitmap( BackBuffer, Bitmap, Position );

		false ->

			throw( { image_file_not_found, Filename } )

	end.



% Helper functions.


% Adds a numbered label to each point in list.
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
	label_points( T, [ { Label, P } | Acc ], Count + 1 ).
