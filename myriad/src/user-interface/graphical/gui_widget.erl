% Copyright (C) 2023-2024 Olivier Boudeville
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
% Creation date: Thursday, August 31, 2023.


% @doc Gathering of various facilities for <b>widgets</b>.
%
% A widget is the most general form of graphical component.
%
% Some very general functions are gathered here as well.
%
-module(gui_widget).



% Could include wx:null(), i.e. a #wx_ref{ref=0, type=wx}:
% (probably cannot be opaque because of the union)
-type widget() :: wxWindow:wxWindow() | gui_canvas:canvas().
% Any kind of widget, that is graphical component; for example a button or a
% canvas is a widget.
%
% Represents any visible object on screen. All controls, top level windows,
% buttons, etc. are widgets.
%
% Sizers and device contexts are not, however, as they do not appear on screen
% themselves.
%
% Widget is a clearer naming than "window" (chosen by wx/WxWidgets), as not
% necessarily directly akin to a regular window.


-opaque control() :: wxControl:wxControl().
% A control is generally a small window that processes user input and/or
% displays one or more items of data.


-export_type([ widget/0, control/0 ]).



-export([ destruct/1, destruct/2, destruct_direct/1,

		  set_sizer/2, fit_to_sizer/2, set_and_fit_to_sizer/2,
		  layout/1,

		  set_foreground_color/2, set_background_color/2,

		  set_font/2, set_font/4,

		  set_tooltip/2,

		  sync/1, enable_repaint/1, refresh/1, update/1,
		  lock/1, unlock/1,

		  get_focused/0, set_focus/1,

		  get_size/1, get_client_size/1, get_best_size/1, set_client_size/2,
		  fit/1, maximise_in_parent/1 ]).



% Implementation notes:
%
% The wx backend names the concept of widget "window", which is a bit
% restrictive (a window is seen at least here as a special case of widget).


% For related, internal, wx-related defines:
%-include("gui_internal_defines.hrl").


% Shorthands:

-type label() :: gui:label().
-type size() :: gui:size().

-type sizer() :: gui_sizer:sizer().

-type gui_env_pid() :: gui:gui_env_pid().

-type color() :: gui_color:color().

-type font() :: gui_font:font().

-type device_context() :: gui_render:device_context().


% @doc Destructs the specified widget.
-spec destruct( widget() ) -> void().
destruct( Widget ) ->
	destruct( Widget, gui:get_environment_server() ).


% @doc Destructs the specified widget, using the specified environment server.
-spec destruct( widget(), gui_env_pid() ) -> void().
destruct( Widget, GUIEnvPid ) ->

	% Note that the resource destruction done by wxWindow:destroy/1 (which is
	% possibly instantaneous) may happen whereas concurrent, message-based
	% operations (typically drawings) may still be on the fly; in this case,
	% when their processing will happen, they may find out that their backend
	% resources do not exist (anymore), for example with
	% {unknown_env,{wxPen,new,2}},[{wxe_util,rec,1,[...).
	%
	% A solution is to stop synchronously all GUI-using callers, then have the
	% GUI loop be waited for to flush any pending operations, before triggering
	% the requested destruction:

	gui:get_main_loop_pid( GUIEnvPid ) ! { synchroniseWithCaller, [], self() },

	receive

		onSynchronisedWithCaller ->
			ok

	end,

	destruct_direct( Widget ).



% @doc Destructs the specified widget directly and basically, with no
% synchronisation involved.
%
% Note however that this corresponds to the decrementing of its reference count,
% so an actual destruction may not happen immediately.
%
-spec destruct_direct( widget() ) -> void().
destruct_direct( Widget ) ->
	wxWindow:destroy( Widget ).



% @doc Associates the specified sizer to the specified widget.
%
% A sizer will not be taken into account as long as it is not associated to a
% widget (typically prior to showing that widget).
%
-spec set_sizer( widget(), sizer() ) -> void().
set_sizer( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Sizer ) ->
	gui:get_main_loop_pid() ! { getCanvasPanel, [ CanvasId ], self() },

	receive

		{ notifyCanvasPanel, Panel } ->
			set_sizer( Panel, Sizer )

	end;

set_sizer( Widget, Sizer ) ->
	wxWindow:setSizer( Widget, Sizer ).


% @doc Resizes the specified widget so that its client area matches the minimal
% size of the specified sizer.
%
-spec fit_to_sizer( widget(), sizer() ) -> void().
fit_to_sizer( Widget, Sizer ) ->
	wxSizer:fit( Sizer, Widget ).


% @doc Associates the specified sizer to the specified window, and sets the size
% and minimal size of the window accordingly.
%
-spec set_and_fit_to_sizer( widget(), sizer() ) -> void().
set_and_fit_to_sizer( Canvas={ myriad_object_ref, myr_canvas, _CanvasId },
					  Sizer ) ->
	set_sizer( Canvas, Sizer ),
	fit_to_sizer( Canvas, Sizer );

set_and_fit_to_sizer( Widget, Sizer ) ->
	wxWindow:setSizerAndFit( Widget, Sizer ).



% @doc Lays out the children of this widget, using any associated sizer,
% otherwise does nothing (except if it is a top level window).
%
-spec layout( widget() ) -> void().
layout( Widget ) ->
	wxWindow:layout( Widget ).



% @doc Sets the foreground color of the specified widget.
-spec set_foreground_color( widget(), color() ) -> void().
set_foreground_color( _Canvas={ myriad_object_ref, myr_canvas, CanvasId },
					  Color ) ->

	%trace_utils:debug_fmt( "Setting foreground color of canvas ~w to ~p.",
	%                       [ Canvas, Color ] ),

	gui:get_main_loop_pid() ! { setCanvasForegroundColor, [ CanvasId, Color ] };

set_foreground_color( Widget, Color ) ->
	ActualColor = gui_color:get_color( Color ),
	wxWindow:setForegroundColour( Widget, ActualColor ).



% @doc Sets the background color of the specified widget.
-spec set_background_color( widget(), color() ) -> void().
set_background_color( _Canvas={ myriad_object_ref, myr_canvas, CanvasId },
					  Color ) ->

	%trace_utils:debug_fmt( "Setting background color of canvas ~w to ~p.",
	%                       [ Canvas, Color ] ),

	gui:get_main_loop_pid() ! { setCanvasBackgroundColor, [ CanvasId, Color ] };

set_background_color( Widget, Color ) ->
	ActualColor = gui_color:get_color( Color ),
	wxWindow:setBackgroundColour( Widget, ActualColor ).



% @doc Sets the font to be used by the specified widget and its children.
-spec set_font( widget(), font() ) -> void().
set_font( Widget, Font ) ->
	set_font( Widget, Font, _DestructFont=false ).


% @doc Sets the font to be used by the specified widget and its children, then,
% if requested, destructs that font.
%
-spec set_font( widget(), font(), boolean() ) -> void().
set_font( Widget, Font, _DestructFont=true ) ->
	set_font( Widget, Font, _DoDestructFont=false ),
	gui_font:destruct( Font );

set_font( Widget, Font, _DestructFont=false ) ->
	wxWindow:setFont( Widget, Font ).


% @doc Sets the font and color to be used by the specified widget (and its
% children), then, if requested, destructs that font.
%
% A side-effect is that the foreground color gets set.
%
-spec set_font( widget(), font(), color(), boolean() ) -> void().
% wxWindow, wxPanel, etc.:
set_font( Widget={ wx_ref, _Id, _AnyWxWidgetLike, _State }, Font, Color,
		  _DestructFont=false ) ->

	%trace_utils:debug_fmt( "Setting for widget ~w font to ~w (color: ~w).",
	%                       [ Widget, Font, Color ] ),

	wxWindow:setFont( Widget, Font ),
	wxWindow:setForegroundColour( Widget, gui_color:get_color( Color ) );

set_font( Widget, Font, Color, _DestructFont=true ) ->
	set_font( Widget, Font, Color, _DestructFnt=false ),
	gui_font:destruct( Font ).


% @doc Attaches a tooltip to the specified widget.
%
% For an unknown reason, works on panels but never on buttons (this is even the
% case for ex_button.erl).
%
-spec set_tooltip( widget(), label() ) -> void().
set_tooltip( _Canvas={ myriad_object_ref, myr_canvas, CanvasId }, Label ) ->
	gui:get_main_loop_pid() ! { setTooltip, [ CanvasId, Label ] };

set_tooltip( Widget, Label ) ->

	%trace_utils:debug_fmt( "Setting tooltip '~ts' to ~ts.",
	%                       [ Label, object_to_string( Widget ) ] ),

	wxWindow:setToolTip( Widget, Label ).





% @doc Synchronises the specified widget to the MyriadGUI loop, to ensure that
% no past operation is still pending at its level.
%
% Useful if there exists some means of interacting with the widget directly
% (e.g. an OpenGL canvas, thanks to an OpenGL NIF) that could create a race
% condition (e.g. presumably a message-based resizing immediately followed by a
% direct OpenGL rendering: the rendering may actually happen before the
% resizing).
%
% See gui_opengl_{minimal,2D}_test:on_main_frame_resize/1 for further details;
% see also the synchroniseWithCaller message supported by the MyriadGUI loop.
%
-spec sync( widget() ) -> size().
sync( Widget ) ->
	% The result in itself may be of no use; the point here is just, through a
	% synchronous operation (a request), to ensure that the specified widget is
	% "ready" (that it has processed all its previous messages) with a
	% sufficient probability (and with certainty if past operations were
	% triggered by the same process as this calling one):
	%
	wxWindow:getSize( Widget ).



% @doc Updates the specified window-like object (e.g. a canvas) so that its
% client area can be painted.
%
% To be called from a repaint event handler.
%
% See [https://www.erlang.org/doc/man/wxpaintdc#description] for more details.
%
% Based on our tests, does not seem strictly necessary.
%
-spec enable_repaint( widget() ) -> void().
enable_repaint( Widget ) ->
	DC = wxPaintDC:new( Widget ),
	wxPaintDC:destroy( DC ).



% @doc Causes this widget, and all of its children recursively, to be repainted
% during the next event loop iteration.
%
% If you need to update the widget immediately, use update/1 instead.
%
-spec refresh( widget() ) -> void().
refresh( Widget ) ->
	wxWindow:refresh( Widget ).



% @doc Repaints the invalidated area of the window and all of its children
% recursively (this normally only happens when the flow of control returns to
% the event loop).
%
% Note that this function does not invalidate any area of the window, so nothing
% happens if nothing has been invalidated (i.e. marked as requiring a redraw).
%
% Use refresh/1 first if you want to "immediately" redraw the window
% unconditionally.
%
-spec update( widget() ) -> void().
update( Widget ) ->
	wxWindow:update( Widget ).



% @doc Locks the specified widget, so that direct access to its content can be
% done, through the returned device context.
%
% Once the desired changes will have been made, this widget must be unlocked.
%
-spec lock( widget() ) -> device_context().
lock( Widget ) ->
	DC = wxWindowDC:new( Widget ),
	case wxDC:isOk( DC ) of

		true ->
			DC;

		false ->
			throw( { lock_widget_failed, Widget } )

	end.



% @doc Unlocks the specified widget, based on the specified device context
% obtained from a previous locking.
%
-spec unlock( device_context() ) -> void().
unlock( DC ) ->
	wxWindowDC:destroy( DC ).




% @doc Returns the widget (if any) that has the current keyboard focus.
%
% Refer to the 'Keyboard-related events' section of the gui_keyboard module for
% further information.
%
-spec get_focused() -> maybe( widget() ).
get_focused() ->
	wxWindow:findFocus().


% @doc Sets the specified widget to receive keyboard input, provided notably
% that this type of widget can have the focus (e.g. frame() cannot).
%
% Refer to the 'Keyboard-related events' section of the gui_keyboard module for
% further information.
%
-spec set_focus( widget() ) -> void().
set_focus( Widget ) ->
	wxWindow:setFocus( Widget ).



% @doc Returns the size (as {Width,Height}) of the specified widget.
-spec get_size( widget() ) -> size().
get_size( _Canvas={ myriad_object_ref, myr_canvas, CanvasId } ) ->

	%trace_utils:debug_fmt( "Getting size of canvas #~B.", [ CanvasId ] ),

	gui:get_main_loop_pid() ! { getCanvasSize, CanvasId, self() },
	receive

		{ notifyCanvasSize, Size } ->
			Size

	end;

get_size( Widget ) ->
	%trace_utils:debug_fmt( "get_size for ~w.", [ Widget ] ),
	wxWindow:getSize( Widget ).



% @doc Returns the client size (as {Width,Height}) of the specified widget, that
% is the actual size of the area that can be drawn upon (excluded menu, bars,
% etc.).
%
-spec get_client_size( widget() ) -> size().
get_client_size( _Canvas={ myriad_object_ref, myr_canvas, CanvasId } ) ->

	gui:get_main_loop_pid() ! { getCanvasClientSize, CanvasId, self() },
	receive

		{ notifyCanvasClientSize, Size } ->
			Size

	end;

get_client_size( Widget ) ->
	wxWindow:getClientSize( Widget ).



% @doc Returns the best size (as {Width,Height}) of the specified widget, that
% is its best acceptable minimal size.
%
-spec get_best_size( widget() ) -> size().
get_best_size( _Canvas={ myriad_object_ref, myr_canvas, CanvasId } ) ->

	gui:get_main_loop_pid() ! { getCanvasClientSize, CanvasId, self() },
	receive

		{ notifyCanvasClientSize, Size } ->
			Size

	end;

get_best_size( Widget ) ->
	wxWindow:getBestSize( Widget ).



% @doc Sets the size of the client area of the specified widget.
-spec set_client_size( widget(), size() ) -> void().
set_client_size( _Canvas={ myriad_object_ref, myr_canvas, _CanvasId },
				 _Size ) ->
	throw( not_implemented );

set_client_size( Widget, Size ) ->
	wxWindow:setClientSize( Widget, Size ).



% @doc Fits the specified widget to its best size.
%
% Corresponds to setting its client size to its best one.
%
-spec fit( widget() ) -> void().
fit( _Canvas={ myriad_object_ref, myr_canvas, _CanvasId } ) ->
	throw( not_implemented );

fit( Widget ) ->
	wxWindow:fit( Widget ).



% @doc Maximises the specified widget (a specialised window, for example a
% panel) in its parent (adopting its maximum client size), and returns the new
% size of this widget.
%
-spec maximise_in_parent( widget() ) -> size().
maximise_in_parent( Widget ) ->
	ParentWindow = wxWindow:getParent( Widget ),
	ParentWindowClientSize = wxWindow:getClientSize( ParentWindow ),
	wxWindow:setSize( Widget, ParentWindowClientSize ),
	ParentWindowClientSize.
