% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Saturday, February 26, 2022.


% @doc Gathering of various facilities for <b>mouse management</b>.
-module(gui_mouse).


% We consider that each mouse can have up to 5 buttons, designated as:
%  - left (a.k.a. first)
%  - middle (a.k.a. second)
%  - right (a.k.a. third)
%  - fourth
%  - fifth


% Implementation notes:
%
% The goal is to offer a stable interface, as independent as possible from any
% backend.
%
% Currently the actual backend is wx; in the future it may be any other (ex:
% esdl2).

% Largely inspired from: Wings3D, SDL and esdl2.

% Custom cursors may be defined, see
% https://docs.wxwidgets.org/3.1/classwx_cursor.html.

% Internally we rely directly on wxMouseState records (no translation to
% SDL-like conventions).

-include_lib("wx/include/wx.hrl").


% For gui_env_reg_name:
-include("gui.hrl").


% To be kept updated with list_cursor_types/0:
-type cursor_type() ::
		'default'
	  | 'none' % Unsupported type
	  | 'blank' % An invisible cursor, when the mouse is grabbed
	  | 'stop' % "no entry", typically to denote a disallowed operation
	  | 'arrow' % Typically to designate an element
	  | 'right_arrow'
	  | 'question_arrow'
	  | 'wait_arrow'
	  | 'wait' % Hourglass, tTypically to denote an ongoing operation
	  | 'watch'
	  | 'hand' % "pointing hand"
	  | 'closed_hand' % "bull's eye"
	  | 'point_left'
	  | 'point_right'
	  | 'char'
	  | 'cross'
	  | 'ibeam'
	  | 'left_button'
	  | 'middle_button'
	  | 'right_button'
	  | 'magnifier' % Typically to zoom
	  | 'paintbrush'
	  | 'pencil'
	  | 'spraycan'
	  %| 'eyedropper' % Typically to pick a color
	  | 'size_ne_sw'
	  | 'size_ns'
	  | 'size_nw_se'
	  | 'size_we'
	  | 'sizing'.
% A cursor is a small bitmap usually used for denoting where the mouse pointer
% is, with a picture that might indicate the interpretation of a mouse click.
%
% Refer to the 'cursor' example of wx:demo/0 that showcases them all.


-type cursor_table() :: table( cursor_type(), wx_cursor_type() ).
% A table storing the correspondance between MyriadGUI and backend cursor types.

-type grab_status() :: 'no_grab' | 'still_grabbed'.


% To be kept in line with get_{all_mouse_event_types,event_types_to_trap}/0.
-type mouse_event_type() ::

	% Button 1:
	  'onMouseLeftButtonPressed' | 'onMouseLeftButtonReleased'
	| 'onMouseLeftButtonDoubleClicked'

	% Button 2:
	| 'onMouseMiddleButtonPressed' | 'onMouseMiddleButtonReleased'
	| 'onMouseMiddleButtonDoubleClicked'

	% Button 3:
	| 'onMouseRightButtonPressed' | 'onMouseRightButtonReleased'
	| 'onMouseRightButtonDoubleClicked'

	% Button 4:
	| 'onMouseFourthButtonPressed' | 'onMouseFourthButtonReleased'
	| 'onMouseFourthButtonDoubleClicked'

	% Button 5:
	| 'onMouseFifthButtonPressed' | 'onMouseFifthtButtonReleased'
	| 'onMouseFifthButtonDoubleClicked'

	% Wheel:
	| 'onMouseWheelScrolled'

	| 'onMouseEnteredWindow' | 'onMouseLeftWindow'
	| 'onMouseMoved'.
% A type of event possibly emitted by a mouse.

-export_type([ cursor_type/0, cursor_table/0, grab_status/0,
			   mouse_event_type/0 ]).


-export([ register_in_environment/1, set_cursor_types/2,
		  unregister_from_environment/1,
		  get_all_mouse_event_types/0, get_event_types_to_trap/0,
		  list_cursor_types/0, set_cursor/1,

		  reset_grab/0, grab/1, ungrab/2, is_grabbed/0 ,warp/3 ]).



% Internals:

-type wx_cursor_type() :: non_neg_integer().
% See include/wx.hrl.

-export([ cursor_type_to_wx/1 ]).


% Shorthands:

-type coordinate() :: gui:coordinate().
-type window() :: gui:window().
-type gui_env_pid() :: gui:gui_env_pid().

-type gui_env_designator() :: gui:gui_env_designator().

-type wxCursor() :: wxCursor:wxCursor().



% @doc Registers in the MyriadGUI environment server the mouse-related settings.
%
% This server is expected to already exist.
%
-spec register_in_environment( gui_env_pid() ) -> void().
register_in_environment( GUIEnvPid ) ->
	AllCursorTypes = list_cursor_types(),
	set_cursor_types( AllCursorTypes, GUIEnvPid ),
	set_cursor( _Default=arrow, GUIEnvPid ),
	environment:cache( { grab_stack, [] }, GUIEnvPid ).


% @doc Sets in the MyriadGUI environment server the specified standard mouse
% cursors.
%
% The GUI environment is assumed to be already known of the caller cache.
%
-spec set_cursor_types( [ cursor_type() ], gui_env_pid() ) -> void().
set_cursor_types( CursorTypes, GUIEnvPid ) ->
	CursorEntries = [ { CT,
		case CT of

			blank ->
				blank( wxCursor:new( ?wxCURSOR_BLANK ) );

			_ ->
				WxCursorType = cursor_type_to_wx( CT ),

				% Uncomment below to detect an unsupported cursor type:
				%trace_utils:debug_fmt( "Declaring cursor type ~ts (in wx: ~p)",
				%                       [ CT, WxCursorType ] ),
				WxCursor = wxCursor:new( WxCursorType ),
				%timer:sleep( 500 ),
				%trace_utils:debug_fmt( "(end for cursor type ~ts)", [ CT ] ),

				% Still an '"Assert failure" in InitFromStock() : unsupported
				% cursor type' can be reported despite no error seen:
				%
				case wxCursor:isOk( WxCursor ) of

					true ->
						WxCursor;

					false ->
						throw( { unsupported_cursor_type, CT, WxCursorType } )

				end

		end } || CT <- CursorTypes ],
	CursorTable = table:new( CursorEntries ),
	environment:cache( { _K=cursor_table, _Value=CursorTable }, GUIEnvPid ).



% @doc Unregisters the mouse-related settings from the MyriadGUI environment.
-spec unregister_from_environment( gui_env_pid() ) -> void().
unregister_from_environment( GUIEnvPid ) ->

	[ CursorTable, _CurrentCursorType, _GrabStack ] = environment:extract(
		_Ks=[ cursor_table, current_cursor_type, grab_stack ], GUIEnvPid ),

	[ wxCursor:destroy( WxCT ) || WxCT <- table:values( CursorTable ) ].



% @doc Returns a list of all types of mouse-related events.
-spec get_all_mouse_event_types() -> [ mouse_event_type() ].
get_all_mouse_event_types() ->

	% To be kept in line with mouse_event_type().

	[ % Button 1:
	  onMouseLeftButtonPressed,
	  onMouseLeftButtonReleased, onMouseLeftButtonDoubleClicked,


	  % Button 2:
	  onMouseMiddleButtonPressed, onMouseMiddleButtonReleased,
	  onMouseMiddleButtonDoubleClicked,

	  % Button 3:
	  onMouseRightButtonPressed, onMouseRightButtonReleased,
	  onMouseRightButtonDoubleClicked,

	  % Button 4:
	  onMouseFourthButtonPressed, onMouseFourthButtonReleased,
	  onMouseFourthButtonDoubleClicked,

	  % Button 5:
	  onMouseFifthButtonPressed, onMouseFifthtButtonReleased,
	  onMouseFifthButtonDoubleClicked,

	  % Wheel:
	  onMouseWheelScrolled,

	  onMouseEnteredWindow, onMouseLeftWindow,
	  onMouseMoved ].



% @doc Returns a list of the types of mouse-related events that shall be trapped
% by default.
%
-spec get_event_types_to_trap() -> [ mouse_event_type() ].
get_event_types_to_trap() ->
	get_all_mouse_event_types().



% @doc Returns a list of all the standard mouse cursor types.
-spec list_cursor_types() -> [ cursor_type() ].
list_cursor_types() ->
	% To be kept updated with the cursor_type/0 type:
	[ 'default', % 'none': unsupported,
	  'blank', 'stop', 'arrow', 'right_arrow',
	  'question_arrow', 'wait_arrow', 'wait', 'watch', 'hand',
	  'closed_hand', 'point_left', 'point_right', 'char', 'cross', 'ibeam',
	  'left_button', 'middle_button', 'right_button', 'magnifier', 'paintbrush',
	  'pencil', 'spraycan', 'size_ne_sw', 'size_ns', 'size_nw_se', 'size_we',
	  'sizing' ].



% @doc Sets the type of the current mouse cursor.
-spec set_cursor( cursor_type() ) -> void().
set_cursor( CursorType ) ->
	set_cursor( CursorType, ?gui_env_reg_name ).


% @doc Sets the type of the current mouse cursor.
-spec set_cursor( cursor_type(), gui_env_designator() ) -> void().
set_cursor( CursorType, GUIEnvDesignator ) ->

	% Probably cached:
	[ CursorTable, MaybeCurrentCursorType, OSFamily, MaybeWindow ] =
		environment:get( [ cursor_table, current_cursor_type, os_family,
						   top_level_window ], GUIEnvDesignator ),

	MaybeCurrentCursorType =:= CursorType orelse
		% Includes undefined:
		begin

			WxCursorType = table:get_value( CursorType, CursorTable ),

			case OSFamily of

				win32 ->
					case MaybeWindow of

						undefined ->
							trace_utils:warning( "No top-level window when "
								"changing mouse cursor." );

						Win ->
							wxWindow:setCursor( Win, WxCursorType )

					end;

				_ ->
					wx_misc:setCursor( WxCursorType )

			end,

			environment:cache( { _K=current_cursor_type, _Value=CursorType },
							   GUIEnvDesignator )

		end.



% Section about mouse grabbing.

% @doc Resets the mouse grabbing, releasing it.
-spec reset_grab() -> void().
reset_grab() ->
	reset_grab( _ReleaseGrab=true ).


% @doc Resets the mouse grabbing, releasing it if requested.
-spec reset_grab( boolean() ) -> void().
reset_grab( ReleaseGrab ) ->
	GUIEnvPid = environment:get_server( ?gui_env_reg_name ),
	reset_grab( ReleaseGrab, GUIEnvPid ).


% @doc Resets the mouse grabbing, releasing it if requested.
-spec reset_grab( boolean(), gui_env_pid() ) -> void().
reset_grab( ReleaseGrab, GUIEnvPid ) ->
	case environment:get( grab_stack, GUIEnvPid ) of

		[] ->
			ignore;

		% Flush:
		_Stack=[ Window | _T ] ->
			environment:set( grab_stack, _EmptyStack=[], GUIEnvPid ),
			ReleaseGrab andalso wxWindow:releaseMouse( Window )

	end,
	set_cursor( _CursorType=arrow, GUIEnvPid ).



% @doc Have the specified window grab the mouse cursor.
-spec grab( window() ) -> void().
grab( Window ) ->
	GUIEnvPid = environment:get_server( ?gui_env_reg_name ),
	grab( Window, GUIEnvPid ).


% @doc Have the specified window grab the mouse cursor.
-spec grab( window(), gui_env_pid() ) -> void().
grab( Window, GUIEnvPid ) ->
	case environment:get( grab_stack, GUIEnvPid ) of

		[] ->
			set_cursor( blank, GUIEnvPid ),
			wxWindow:captureMouse( Window ),
			environment:set( grab_stack, [ Window ], GUIEnvPid );

		% Regrab:
		GrabStack=[ Window | _ ] ->
			environment:set( grab_stack, [ Window | GrabStack ], GUIEnvPid );

		[ OtherWindow | _T ] ->
			trace_utils:error_fmt( "Mouse requested to be grabbed by windows "
				"~w whereas already grabbed by ~w.", [ Window, OtherWindow ] ),
			throw( { mouse_already_grabbed, OtherWindow, Window } )

	end.



% @doc Ungrabs the mouse cursor, having it appear (warp) at specified
% coordinates, returns whether it is still grabbed.
%
-spec ungrab( coordinate(), coordinate() ) -> grab_status().
ungrab( X, Y ) ->
	GUIEnvPid = environment:get_server( ?gui_env_reg_name ),
	ungrab( X, Y, GUIEnvPid ).


% @doc Ungrabs the mouse cursor, having it appear (warp) at specified
% coordinates, returns whether it is still grabbed.
%
-spec ungrab( coordinate(), coordinate(), gui_env_pid() ) -> grab_status().
ungrab( X, Y, GUIEnvPid ) ->

	case environment:get( grab_stack, GUIEnvPid ) of

		[] ->
			no_grab;

		[ Window ] ->
			wxWindow:releaseMouse( Window ),
			warp( Window, X, Y ),
			set_cursor( arrow ),
			environment:set( grab_stack, [], GUIEnvPid ),
			no_grab;

		[ _Window | RestOfStack ] ->
			environment:set( grab_stack, RestOfStack, GUIEnvPid ),
			still_grabbed

	end.



% @doc Tells whether the mouse cursor is grabbed.
-spec is_grabbed() -> boolean().
is_grabbed() ->
	GUIEnvPid = environment:get_server( ?gui_env_reg_name ),
	is_grabbed( GUIEnvPid ).


% @doc Tells whether the mouse cursor is grabbed.
-spec is_grabbed( gui_env_pid() ) -> boolean().
is_grabbed( GUIEnvPid ) ->

	case environment:get( grab_stack, GUIEnvPid ) of

		[] ->
			false;

		_ ->
			true

	end.



% @doc Warps (moves) the mouse cursor at the specified location on the specified
% window.
%
-spec warp( window(), coordinate(), coordinate() ) -> void().
warp( Window, X, Y ) ->
	GUIEnvPid = environment:get_server( ?gui_env_reg_name ),
	warp( Window, X, Y, GUIEnvPid ).


% @doc Warps (moves) the mouse cursor at the specified location on the specified
% window.
%
-spec warp( window(), coordinate(), coordinate(), gui_env_pid() ) -> void().
warp( Window, X, Y, GUIEnvPid ) ->
	environment:set( warp_coordinates, _Point={X,Y}, GUIEnvPid ),
	wxWindow:warpPointer( Window, X, Y ).



% @doc Returns a blank cursor, either the specified predefined one if legit or,
% if it has no data associated, a new one.
%
-spec blank( wxCursor() ) -> wxCursor().
blank( WxCursor ) ->
	case wxCursor:ok( WxCursor ) of

		true ->
			WxCursor;

		% Then rebuilds a correct blank cursor:
		false ->
			wxCursor:destroy( WxCursor ),
			Image = wxImage:new( _W=16, _H=16 ),
			AllBlack = <<0:(16*16*3*8)>>,
			wxImage:setData( Image, AllBlack ),

			% Black mask:
			wxImage:setMaskColour( Image, _R=0, _G=0, _B=0 ),

			% Therefore fully masked:
			wxImage:setMask( Image ),

			BlankWxCursor = wxCursor:new( Image ),
			wxImage:destroy( Image ),
			BlankWxCursor

	end.



% Helpers related to wx:


% @doc Returns the wx cursor type corresponding to the MyriadGUI specified one.
-spec cursor_type_to_wx( cursor_type() ) -> wx_cursor_type().
% From wx.hrl:
cursor_type_to_wx( default ) -> ?wxCURSOR_DEFAULT;
cursor_type_to_wx( none ) -> ?wxCURSOR_NONE;

% Deactivated, as must be special-cased:
%cursor_type_to_wx( blank ) -> ?wxCURSOR_BLANK;

cursor_type_to_wx( stop ) -> ?wxCURSOR_NO_ENTRY;
cursor_type_to_wx( arrow ) -> ?wxCURSOR_ARROW;
cursor_type_to_wx( right_arrow ) -> ?wxCURSOR_RIGHT_ARROW;
cursor_type_to_wx( question_arrow ) -> ?wxCURSOR_QUESTION_ARROW;
cursor_type_to_wx( wait_arrow ) -> ?wxCURSOR_ARROWWAIT;
cursor_type_to_wx( wait ) -> ?wxCURSOR_WAIT;
cursor_type_to_wx( watch ) -> ?wxCURSOR_WATCH;
cursor_type_to_wx( hand ) -> ?wxCURSOR_HAND;
cursor_type_to_wx( closed_hand ) -> ?wxCURSOR_BULLSEYE;

cursor_type_to_wx( point_left ) -> ?wxCURSOR_POINT_LEFT;
cursor_type_to_wx( point_right ) -> ?wxCURSOR_POINT_RIGHT;

cursor_type_to_wx( char ) -> ?wxCURSOR_CHAR;
cursor_type_to_wx( cross ) -> ?wxCURSOR_CROSS;
cursor_type_to_wx( ibeam ) -> ?wxCURSOR_IBEAM;

cursor_type_to_wx( left_button ) -> ?wxCURSOR_LEFT_BUTTON;
cursor_type_to_wx( middle_button ) -> ?wxCURSOR_MIDDLE_BUTTON;
cursor_type_to_wx( right_button ) -> ?wxCURSOR_RIGHT_BUTTON;

cursor_type_to_wx( magnifier ) -> ?wxCURSOR_MAGNIFIER;
cursor_type_to_wx( paintbrush ) -> ?wxCURSOR_PAINT_BRUSH;
cursor_type_to_wx( pencil ) -> ?wxCURSOR_PENCIL;
cursor_type_to_wx( spraycan ) -> ?wxCURSOR_SPRAYCAN;
% Non-existing: cursor_type_to_wx( eyedropper ) -> ?wxCURSOR_;

cursor_type_to_wx( size_ne_sw ) -> ?wxCURSOR_SIZENESW;
cursor_type_to_wx( size_ns ) -> ?wxCURSOR_SIZENS;
cursor_type_to_wx( size_nw_se ) -> ?wxCURSOR_SIZENWSE;
cursor_type_to_wx( size_we ) -> ?wxCURSOR_SIZEWE;
cursor_type_to_wx( sizing ) -> ?wxCURSOR_SIZING.
