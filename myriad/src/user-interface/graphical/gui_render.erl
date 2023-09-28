% Copyright (C) 2023-2023 Olivier Boudeville
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
% Creation date: Saturday, September 2, 2023.


% @doc Gathering of various facilities related to <b>rendering</b>. This
% comprises the management of brushes for drawing, of graphic contexts, of the
% various kinds of device contexts.
%
% See also the gui_canvas and gui_bitmap modules.
%
-module(gui_render).


-opaque brush() :: wxBrush:wxBrush().
% A brush, used to draw elements.

-opaque graphic_context() :: wxGraphicsContext:wxGraphicsContext().
% Corresponds to a GUI object that is drawn upon.
%
% It is created by a renderer; some of them (like Direct2D or Cairo) defer their
% drawing operations; they may be forced by destroying the context.


% With wx, device contexts (e.g. obtained from wxMemoryDC:new/1) must be
% explicitly managed (e.g. wxMemoryDC:destroy/1 must be called when finished
% with them), which is inconvenient and error-prone.
%
-opaque device_context() :: wxDC:wxDC().
% Designates an abstract device where rendering can take place, and which can be
% the source or target of a blit. Akin to a surface in SDL (libsdl).


-opaque paint_device_context() :: wxPaintDC:wxPaintDC().
% Designates a device context allowing to paint on the client area of a window
% from within an onRepaintNeeded event handler.


-opaque memory_device_context() :: wxMemoryDC:wxMemoryDC().
% Provides a means of drawing graphics onto a bitmap.
%
% https://docs.wxwidgets.org/stable/classwx_memory_d_c.html


-type back_buffer() :: memory_device_context().
% A non-displayed buffer to which rendering shall be done, before being made
% visible as a whole, to avoid flicker.


-opaque window_device_context() :: wxWindowDC:wxWindowDC().
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


-type buffer() :: bin_utils:buffer().
% Any type of GUI-related buffer.


-export_type([ brush/0, graphic_context/0,
			   device_context/0, paint_device_context/0,
			   memory_device_context/0, back_buffer/0,
			   window_device_context/0,
			   any_window_device_context/0,
			   buffer/0 ]).



% For brushes:
-export([ create_brush/1, create_transparent_brush/1, destruct_brush/1 ]).


% For graphic contexts:
-export([ create_graphic_context/1,
		  set_brush/2,
		  set_font/3, set_font/4,
		  get_text_extent/2, get_text_width/2, get_text_height/2,
		  get_text_dimensions/2,
		  draw_text/4,
		  draw_bitmap/4, draw_bitmap/5, draw_bitmap/6 ]).


% For device contexts in general:
-export([ clear_device_context/1, blit/5, blit/6 ]).


% For paint device contexts:
-export([ get_flickerfree_paint_device_context/1,
		  get_flickerfree_paint_device_context/2,
		  destruct_paint_device_context/1 ]).


% For memory device contexts:
-export([ destruct_memory_device_context/1 ]).


% For wx defines:
-include_lib("wx/include/wx.hrl").



% Shorthands:

-type ustring() :: text_utils:ustring().

-type os_family() :: system_utils:os_family().

-type dimensions() :: gui:dimensions().
-type coordinate() :: gui:coordinate().

-type any_width() :: gui:any_width().
-type any_height() :: gui:any_height().

-type width() :: gui:width().
-type height() :: gui:height().

-type any_length() :: gui:any_length().
-type point() :: gui:point().
-type widget() :: gui:widget().

-type window() :: gui_window:window().

-type image() :: gui_image:image().
-type bitmap() :: gui_bitmap:bitmap().

-type color() :: gui_color:color().
-type color_by_decimal() :: gui_color:color_by_decimal().

-type font() :: gui_font:font().



% Brush section.


% @doc Returns a brush of the specified color.
-spec create_brush( color_by_decimal() ) -> brush().
create_brush( RGBColor ) ->
	wxBrush:new( RGBColor ).


% @doc Returns a brush of the specified color, and whose background is
% transparent.
%
-spec create_transparent_brush( color_by_decimal() ) -> brush().
create_transparent_brush( RGBColor ) ->
	wxBrush:new( RGBColor, [ { style, ?wxTRANSPARENT } ] ).


% @doc Tells that the specified (reference-counted) brush may be deleted.
-spec destruct_brush( brush() ) -> void().
destruct_brush( Brush ) ->
	wxBrush:destroy( Brush ).




% Graphic context section.


% @doc Returns a graphic context created from the specified window device
% context.
%
-spec create_graphic_context( window_device_context() ) -> graphic_context().
create_graphic_context( DC ) ->
	wxGraphicsContext:create( DC ).


% @doc Sets the specified brush for the specified graphic context.
-spec set_brush( graphic_context(), brush() ) -> void().
set_brush( GraphicContext, Brush ) ->
	wxGraphicsContext:setBrush( GraphicContext, Brush ).


% @doc Sets the font to be used by the specified graphic context, with the
% specified color.
%
-spec set_font( graphic_context(), font(), color() ) -> void().
set_font( GraphicContext, Font, Color ) ->
	set_font( GraphicContext, Font, Color, _DoDestructFont=false ).


% @doc Sets the font and color to be used by the specified graphic context,
% then, if requested, destructs that font.
%
-spec set_font( graphic_context(), font(), color(), boolean() ) -> void().
set_font( GraphicContext, Font, Color, _DestructFont=true ) ->
	set_font( GraphicContext, Font, Color, _DoDestructFont=false ),
	gui_font:destruct( Font );

set_font( GraphicContext, Font, Color, _DestructFont=false ) ->
	wxGraphicsContext:setFont( GraphicContext, Font,
							   gui_color:get_color( Color ) ).



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

	% Ceil could be used:
	type_utils:ensure_ceiled_integer( W ).


% @doc Returns the (integer) height corresponding to the rendering of the
% specified string, using the currently selected font.
%
-spec get_text_height( graphic_context(), ustring() ) -> height().
get_text_height( GraphicContext, Text ) ->

	{ _W, H, _D, _EL } =
		wxGraphicsContext:getTextExtent( GraphicContext, Text ),

	type_utils:ensure_ceiled_integer( H ).


% @doc Returns the (integer) dimensions corresponding to the rendering of the
% specified string, using the currently selected font.
%
-spec get_text_dimensions( graphic_context(), ustring() ) -> dimensions().
get_text_dimensions( GraphicContext, Text ) ->

	{ W, H, _D, _EL } =
		wxGraphicsContext:getTextExtent( GraphicContext, Text ),

	{ type_utils:ensure_ceiled_integer( W ),
	  type_utils:ensure_ceiled_integer( H ) }.




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
	{ W, H } = gui_bitmap:get_size( Bitmap ),
	wxGraphicsContext:drawBitmap( GraphicContext, Bitmap, X, Y, W, H ).


% @doc Draws the specified bitmap onto the specified graphic context, at the
% specified location with the specified dimensions.
%
-spec draw_bitmap( graphic_context(), bitmap(), coordinate(), coordinate(),
				   dimensions() ) -> void().
draw_bitmap( GraphicContext, Bitmap, X, Y, _Dims={ Width, Height } ) ->
	wxGraphicsContext:drawBitmap( GraphicContext, Bitmap, X, Y, Width, Height ).


% @doc Draws the specified bitmap onto the specified graphic context, at the
% specified location with the specified dimensions.
%
-spec draw_bitmap( graphic_context(), bitmap(), coordinate(), coordinate(),
				   width(), height() ) -> void().
draw_bitmap( GraphicContext, Bitmap, X, Y, Width, Height ) ->
	wxGraphicsContext:drawBitmap( GraphicContext, Bitmap, X, Y, Width, Height ).





% Device context section.


% @doc Clears the specified device context, using the current background brush.
% If none was set, a solid white brush is used.
%
-spec clear_device_context( device_context() ) -> void().
clear_device_context( DC ) ->
	wxDC:clear( DC ).



% @doc Blits (copies) the specified area of the source device context at the
% specified position in the target device context.
%
% Returns a boolean of unspecified meaning.
%
-spec blit( device_context(), point(), dimensions() , device_context(),
			point() ) -> boolean().
blit( SourceDC, SrcTopLeft, Size, TargetDC, TgtTopLeft ) ->
	wxDC:blit( TargetDC, TgtTopLeft, Size, SourceDC, SrcTopLeft ).


% @doc Blits (copies) the specified area of the source device context at the
% specified position in the target device context.
%
% Returns a boolean of unspecified meaning.
%
-spec blit( device_context(), point(), width(), height(), device_context(),
			point() ) -> boolean().
blit( SourceDC, SrcTopLeft, Width, Height, TargetDC, TgtTopLeft ) ->
	blit( SourceDC, SrcTopLeft, _Size={ Width, Height }, TargetDC, TgtTopLeft ).





% Paint device context section.


% @doc Returns a flicker-free paint device context corresponding to the
% specified widget, so that its client area can be modified (from an
% onRepaintNeeded event handler).
%
-spec get_flickerfree_paint_device_context( widget() ) ->
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



% Memory device context section.


% @doc Destructs the specified memory device context.
-spec destruct_memory_device_context( memory_device_context() ) -> void().
destruct_memory_device_context( MemDeviceContext ) ->
	wxMemoryDC:destroy( MemDeviceContext ).
