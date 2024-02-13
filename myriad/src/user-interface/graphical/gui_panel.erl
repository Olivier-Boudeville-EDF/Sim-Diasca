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


% @doc Gathering of various facilities for <b>panels</b>.
-module(gui_panel).


-opaque panel() :: wxPanel:wxPanel().
% A panel, able to host child widgets and to catch events such as key presses
% (unlike windows/frames).
%
% Note though that, as soon as a widget declares that its parent is a panel
% (e.g. if a button is declared in a panel, or if a sizer including a box
% designates the panel as its parent), this panel will not receive anymore key
% events (even if the focus is set to the panel), presumably to support
% accelerator keys.
%
% Refer to "Panel issues" in gui_event for further information.


-type panel_option() :: window_option().
% Options for panels.
%
% See [http://docs.wxwidgets.org/stable/classwx_panel.html].


-type panel_options() :: maybe_list( panel_option() ).


-export_type([ panel/0, panel_option/0, panel_options/0 ]).


-export([ create/0, create/1, create/2, create/3, create/4, create/5, create/6,
		  destruct/1,
		  get_size/1 ]).


-type wx_panel_option() :: gui_window:wx_window_option()
						 | gui_wx_backend:wx_event_handler_option().


% Implementation notes:
%
% The parent of a panel can be widgets like windows, including splitter windows
% that may have to be special-cased.


% At least for the splitter record:
-include("gui_base.hrl").


% Shorthands:

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type parent() :: gui:parent().
-type width() :: gui:width().
-type height() :: gui:height().
-type size() :: gui:size().
-type dimensions() :: gui:dimensions().
-type position() :: gui:position().
-type coordinate() :: gui:coordinate().

-type window_option() :: gui_window:window_option().


% @doc Creates a panel.
-spec create() -> panel().
create() ->
	wxPanel:new().



% @doc Creates a panel, associated to the specified parent.
-spec create( parent() ) -> panel().
create( _Parent=#splitter{ splitter_window=Win } ) ->
	%trace_utils:debug_fmt( "Creating panel from splitter window ~w.",
	%                       [ Win ] ),
	wxPanel:new( Win );

create( Parent ) ->
	wxPanel:new( Parent ).



% @doc Creates a panel, associated to the specified parent and with the
% specified options.
%
-spec create( panel_options(), parent() ) -> panel().
create( Options, _Parent=#splitter{ splitter_window=Win } ) ->
	create( Options, Win );

create( Options, Parent ) ->
	wxPanel:new( Parent, to_wx_panel_options( Options ) ).



% @doc Creates a panel, associated to the specified parent and with the
% specified position and dimensions.
%
-spec create( coordinate(), coordinate(), width(), height(), parent() ) ->
											panel().
create( X, Y, Width, Height, Parent ) ->
	create( _Pos={ X, Y }, _Size={ Width, Height }, Parent ).



% @doc Creates a panel, associated to the specified parent and with the
% specified position and dimensions.
%
-spec create( position(), size(), parent() ) -> panel().
create( Position, Size, Parent ) ->

	WxOpts = [ gui_wx_backend:to_wx_position( Position ),
			   gui_wx_backend:to_wx_size( Size ) ],

	wxPanel:new( Parent, WxOpts ).


% @doc Creates a panel, associated to the specified parent and with the
% specified position and dimensions.
%
-spec create( position(), size(), panel_options(), parent() ) -> panel().
create( Position, Size, Options, Parent ) ->

	WxOpts = [ gui_wx_backend:to_wx_position( Position ),
			   gui_wx_backend:to_wx_size( Size )
					| to_wx_panel_options( Options ) ],

	%trace_utils:debug_fmt( "Creating panel: parent: ~w, position: ~w, "
	%    "size: ~w, options: ~w, full options: ~w.",
	%    [ Parent, Position, Size, Options, WxOpts ] ),

	wxPanel:new( Parent, WxOpts ).



% @doc Creates a panel, associated to the specified parent, with the specified
% position, dimensions and options.
%
-spec create( coordinate(), coordinate(), width(), height(), panel_options(),
			  parent() ) -> panel().
create( X, Y, Width, Height, Options, Parent ) ->

	WxOpts = [ { pos, { X, Y } }, { size, { Width, Height } }
					| to_wx_panel_options( Options ) ],

	wxPanel:new( Parent, WxOpts ).



% @doc Destructs the specified panel.
-spec destruct( panel() ) -> void().
destruct( Panel ) ->
	wxPanel:destroy( Panel ).


% @doc Returns the size of the specified panel.
%
% Defined here only for convenience (as gui_widget provides it).
%
-spec get_size( panel() ) -> dimensions().
get_size( Panel ) ->
	wxWindow:getSize( Panel ).


% @doc Converts the specified MyriadGUI panel option(s) into the appropriate
% wx-specific options.
%
% (exported helper)
%
-spec to_wx_panel_options( maybe_list( panel_option() ) ) ->
											[ wx_panel_option() ].
to_wx_panel_options( Options ) ->
	gui_window:to_wx_window_options( Options ).
