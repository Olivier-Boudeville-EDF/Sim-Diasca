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


% @doc Gathering of various facilities for <b>keyboard management</b>.
%
% See also the gui_keyboard_test module.
%
-module(gui_keyboard).


% Implementation notes:
%
% The goal is to offer a stable interface, as independent as possible from any
% backend.
%
% Currently the actual backend is wx; in the future it may be any other (ex:
% esdl2).

% Largely inspired from: Wings3D, SDL and esdl2.


% There are two ways to consider a keyboard:
%  - a ~104-button joystick, for which only the buttons locations and
%  press/release statuses matter
%  - a device that produces text (Unicode) inputs
%
% In the first case, we are dealing with scancodes ("button codes"), whereas in
% the second one we are dealing with keycodes ("character codes"). Both are
% 32-bit values, yet have different semantics.

% Scancodes relate only to actual button locations (not on any label on top of
% the keys), hence are meant not to depend on any current specific keyboard
% layout; think of this as "the user pressed the Q key as it would be on a US
% QWERTY keyboard", regardless of whether this is actually an European keyboard,
% a Dvorak one, or any other. Even if they return the character code for Latin-1
% keys corresponding to an hypothetical, canonical US keyboard for compatibility
% (so testing the returned value with a different keyboard layout may be of
% use), they should be used to handle special/location-based characters (such as
% cursor arrows keys, the Home or Insert keys, etc.). They correspond to the
% gui_keyboard:scancode/0 type.
%
% So the scancode is always the same key position, it basically designates a
% button at a given location of the aforementioned 104-button joystick.
%
% As for keycodes, they are meant to be layout-dependent (and
% location-independent). Think of this as "the user pressed the key that is
% labelled 'Q' on a specific keyboard, wherever it is."; keycodes include
% non-Latin-1 characters that can be entered when using national keyboard
% layouts. They correspond to the gui_keyboard:keycode/0 type.


% Keyboard-related events
%
% Key events are only sent to the widget that has the focus (see
% gui:set_focus/1, gui:get_focused/0), provided that this type of widget *can*
% have the focus at all (e.g. frame() cannot); by default, only the last
% rendered widget has the focus.
%
% Moreover key events are not command events, so they are *not* propagated
% automatically to the parent widgets.
%
% Key events record a position in client coordinates, corresponding to the
% unclipped position of the mouse cursor: should the focus be kept by the widget
% whereas the mouse cursor is outside of its client area, coordinates beyond its
% range - i.e. negative or larger than its dimensions - will be reported.


% For the key defines:
-include_lib("wx/include/wx.hrl").


% For the actual scancode definitions:
-include("ui_keyboard_scancodes.hrl").

% For the actual keycode definitions:
-include("ui_keyboard_keycodes.hrl").


-type scancode() :: uint32().
% Designates a button at a given location of the keyboard when it is considered
% as a ~104-button joystick.
%
% A scancode is a "button code", it designates a location of a button on any
% keyboard, regardless of the character labelled on the user's actual keyboard.
%
% These locations are designated according to the characters that would be
% printed on a virtual, canonical US QWERTY keyboard, taken as a reference, or
% to non-printable special keys (ex: the Home key, the Insert one).
%
% A modifier generates a scancode just by itself.
%
% Refer to the corresponding MYR_SCANCODE_* defines.



-type keycode() :: uint32().
% Designates a layout-dependent (Unicode) character code, that is the key
% corresponding to a given character, wherever that key may be on the user's
% actual keyboard (which notably depends on its layout of choice).
%
% A modifier does not generate a keycode just by itself.
%
% Refer to the corresponding MYR_K_* defines.


-type modifier() :: keycode().
% Designates a modifier, like Shift, Control, Alt or Meta.


-type key_transition() :: 'key_down'
						| 'key_up'.
% Corresponds to a (punctual) state transition of a key.
%
% Note that key down/up transitions are not paired (ex: if a key is maintained
% in a pressed state, many key down events will be generated, but only one key
% up will be reported at the end, when the key is released).


-type key_status() :: 'pressed'
					| 'released'.
% Corresponds to the (potentially durable) status of a key.


-type keyboard_event_type() ::

	% Event taking into account any modifier (ex: Control, Shift, Caps Lock) for
	% the returned logical haracter (ex: returning 'A' instead of 'a'):
	%
	'onCharEntered'

	% So that parent windows can intercept keys received by focused (child)
	% windows:
	%
	| 'onCharEnteredHook'

	% Event just about the physical key of interest (regardless of any
	% modifier):
	%
	| 'onKeyPressed'
	| 'onKeyReleased'.



-export_type([ scancode/0, keycode/0, modifier/0,
			   key_transition/0, key_status/0,
			   keyboard_event_type/0 ]).


-export([ is_modkey_pressed/1, is_key_pressed/1, to_lower/2,
		  key_event_to_string/1, key_event_to_string/2 ]).

% Internals:

-export([ wx_keycode_to_myr/1, myr_keycode_to_wx/1 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type uint32() :: type_utils:uint32().

-type wx_keycode() :: integer().

-type event_translation_table() :: gui_event:event_translation_table().


% @doc Tells whether the specified key, designated as a scancode comprising a
% modifier, is pressed.
%
-spec is_modkey_pressed( scancode() ) -> boolean().
is_modkey_pressed( Scancode ) when ( Scancode band ?MYR_SCANCODE_LCTRL ) > 0 ->
	wx_misc:getKeyState( ?WXK_CONTROL );

is_modkey_pressed( Scancode ) when ( Scancode band ?MYR_K_LALT )  > 0 ->
	wx_misc:getKeyState( ?WXK_ALT );

is_modkey_pressed( Scancode ) when ( Scancode band ?MYR_K_LSHIFT ) > 0 ->
	wx_misc:getKeyState( ?WXK_SHIFT );

is_modkey_pressed( Scancode ) when ( Scancode band ?MYR_K_LSUPER ) > 0 ->
	wx_misc:getKeyState( ?WXK_WINDOWS_LEFT )
		orelse wx_misc:getKeyState( ?WXK_WINDOWS_RIGHT ).



% @doc Tells whether the specified key, designated as a scancode, is pressed.
-spec is_key_pressed( keycode() ) -> boolean().
is_key_pressed( Keycode ) ->
	wx_misc:getKeyState( myr_keycode_to_wx( Keycode ) ).



% @doc Returns the specified key, once the specified pressed modifier has been
% taken into account.
%
-spec to_lower( modifier(), keycode() ) -> keycode().
to_lower( ?MYR_K_ANY_SHIFT, Char ) ->
	Char;

to_lower( _Mod, Char ) ->
	string:to_lower( Char ).




% Helpers related to wx:


% doc Returns the MyriadGUI scancode corresponding to the specified wx one.
%-spec wx_scancode_to_myr( wx_scancode() ) -> scancode().


% doc Returns the wx scancode corresponding to the specified MyriadGUI one.
%-spec myr_scancode_to_wx( scancode() ) -> wx_scancode().
%myr_scancode_to_wx( MyrScanCode ) ->



% @doc Returns the MyriadGUI keycode corresponding to the specified wx one.
-spec wx_keycode_to_myr( wx_keycode() ) -> maybe( keycode() ).
% Directly obtained from wings_io_wx:wx_key_map/1:
% (could be substituted with a corresponding edsl2 NIF)
wx_keycode_to_myr( ?WXK_SHIFT )   -> ?MYR_K_LSHIFT;
wx_keycode_to_myr( ?WXK_ALT )     -> ?MYR_K_LALT;
wx_keycode_to_myr( ?WXK_CONTROL ) -> ?MYR_K_LCTRL;

wx_keycode_to_myr( ?WXK_F1 )  -> ?MYR_K_F1;
wx_keycode_to_myr( ?WXK_F2 )  -> ?MYR_K_F2;
wx_keycode_to_myr( ?WXK_F3 )  -> ?MYR_K_F3;
wx_keycode_to_myr( ?WXK_F4 )  -> ?MYR_K_F4;
wx_keycode_to_myr( ?WXK_F5 )  -> ?MYR_K_F5;
wx_keycode_to_myr( ?WXK_F6 )  -> ?MYR_K_F6;
wx_keycode_to_myr( ?WXK_F7 )  -> ?MYR_K_F7;
wx_keycode_to_myr( ?WXK_F8 )  -> ?MYR_K_F8;
wx_keycode_to_myr( ?WXK_F9 )  -> ?MYR_K_F9;
wx_keycode_to_myr( ?WXK_F10 ) -> ?MYR_K_F10;
wx_keycode_to_myr( ?WXK_F11 ) -> ?MYR_K_F11;
wx_keycode_to_myr( ?WXK_F12 ) -> ?MYR_K_F12;
wx_keycode_to_myr( ?WXK_F13 ) -> ?MYR_K_F13;
wx_keycode_to_myr( ?WXK_F14 ) -> ?MYR_K_F14;
wx_keycode_to_myr( ?WXK_F15 ) -> ?MYR_K_F15;

wx_keycode_to_myr( ?WXK_UP )    -> ?MYR_K_UP;
wx_keycode_to_myr( ?WXK_LEFT )  -> ?MYR_K_LEFT;
wx_keycode_to_myr( ?WXK_DOWN )  -> ?MYR_K_DOWN;
wx_keycode_to_myr( ?WXK_RIGHT ) -> ?MYR_K_RIGHT;

wx_keycode_to_myr( ?WXK_DELETE )   -> ?MYR_K_DELETE; %% same
wx_keycode_to_myr( ?WXK_INSERT )   -> ?MYR_K_INSERT;
wx_keycode_to_myr( ?WXK_END )      -> ?MYR_K_END;
wx_keycode_to_myr( ?WXK_HOME )     -> ?MYR_K_HOME;
wx_keycode_to_myr( ?WXK_PAGEUP )   -> ?MYR_K_PAGEUP;
wx_keycode_to_myr( ?WXK_PAGEDOWN ) -> ?MYR_K_PAGEDOWN;

% In SDL2, '_KPn' became '_KP_n':
wx_keycode_to_myr( ?WXK_NUMPAD0 ) -> ?MYR_K_KP_0;
wx_keycode_to_myr( ?WXK_NUMPAD1 ) -> ?MYR_K_KP_1;
wx_keycode_to_myr( ?WXK_NUMPAD2 ) -> ?MYR_K_KP_2;
wx_keycode_to_myr( ?WXK_NUMPAD3 ) -> ?MYR_K_KP_3;
wx_keycode_to_myr( ?WXK_NUMPAD4 ) -> ?MYR_K_KP_4;
wx_keycode_to_myr( ?WXK_NUMPAD5 ) -> ?MYR_K_KP_5;
wx_keycode_to_myr( ?WXK_NUMPAD6 ) -> ?MYR_K_KP_6;
wx_keycode_to_myr( ?WXK_NUMPAD7 ) -> ?MYR_K_KP_7;
wx_keycode_to_myr( ?WXK_NUMPAD8 ) -> ?MYR_K_KP_8;
wx_keycode_to_myr( ?WXK_NUMPAD9 ) -> ?MYR_K_KP_9;

wx_keycode_to_myr( ?WXK_NUMPAD_MULTIPLY ) -> ?MYR_K_KP_MULTIPLY;
wx_keycode_to_myr( ?WXK_NUMPAD_ADD )      -> ?MYR_K_KP_PLUS;
wx_keycode_to_myr( ?WXK_NUMPAD_SUBTRACT ) -> ?MYR_K_KP_MINUS;
wx_keycode_to_myr( ?WXK_NUMPAD_DECIMAL )  -> ?MYR_K_KP_PERIOD;
wx_keycode_to_myr( ?WXK_NUMPAD_DIVIDE )   -> ?MYR_K_KP_DIVIDE;
wx_keycode_to_myr( ?WXK_NUMPAD_ENTER )    -> ?MYR_K_KP_ENTER;

% In SDL2, '_{L,R}LSUPER' became '_{L,R}GUI', we prefer the former convention:
wx_keycode_to_myr( ?WXK_WINDOWS_LEFT )  -> ?MYR_K_LSUPER;
wx_keycode_to_myr( ?WXK_WINDOWS_RIGHT ) -> ?MYR_K_RSUPER;

wx_keycode_to_myr( WxKeycode ) ->
	trace_utils:warning_fmt( "Unknown wx keycode: '~ts' (i.e. ~B).",
							 [ [ WxKeycode ], WxKeycode ] ),
	undefined.


% @doc Returns the wx keycode corresponding to the specified MyriadGUI one.
-spec myr_keycode_to_wx( keycode() ) -> wx_keycode().
% Directly obtained from wings_io_wx:sdl_key_map/1:
% ( could be substituted with a corresponding edsl2 NIF)
myr_keycode_to_wx( ?MYR_K_LSHIFT ) -> ?WXK_SHIFT;
myr_keycode_to_wx( ?MYR_K_LALT )   -> ?WXK_ALT;
myr_keycode_to_wx( ?MYR_K_LCTRL )  -> ?WXK_CONTROL;

myr_keycode_to_wx( ?MYR_K_F1 )   -> ?WXK_F1;
myr_keycode_to_wx( ?MYR_K_F2 )   -> ?WXK_F2;
myr_keycode_to_wx( ?MYR_K_F3 )   -> ?WXK_F3;
myr_keycode_to_wx( ?MYR_K_F4 )   -> ?WXK_F4;
myr_keycode_to_wx( ?MYR_K_F5 )   -> ?WXK_F5;
myr_keycode_to_wx( ?MYR_K_F6 )   -> ?WXK_F6;
myr_keycode_to_wx( ?MYR_K_F7 )   -> ?WXK_F7;
myr_keycode_to_wx( ?MYR_K_F8 )   -> ?WXK_F8;
myr_keycode_to_wx( ?MYR_K_F9 )   -> ?WXK_F9;
myr_keycode_to_wx( ?MYR_K_F10 )  -> ?WXK_F10;
myr_keycode_to_wx( ?MYR_K_F11 )  -> ?WXK_F11;
myr_keycode_to_wx( ?MYR_K_F12 )  -> ?WXK_F12;
myr_keycode_to_wx( ?MYR_K_F13 )  -> ?WXK_F13;
myr_keycode_to_wx( ?MYR_K_F14 )  -> ?WXK_F14;
myr_keycode_to_wx( ?MYR_K_F15 )  -> ?WXK_F15;

myr_keycode_to_wx( ?MYR_K_UP )    -> ?WXK_UP;
myr_keycode_to_wx( ?MYR_K_LEFT )  -> ?WXK_LEFT;
myr_keycode_to_wx( ?MYR_K_DOWN )  -> ?WXK_DOWN;
myr_keycode_to_wx( ?MYR_K_RIGHT ) -> ?WXK_RIGHT;

myr_keycode_to_wx( ?MYR_K_DELETE )   -> ?WXK_DELETE; %% same
myr_keycode_to_wx( ?MYR_K_INSERT )   -> ?WXK_INSERT;
myr_keycode_to_wx( ?MYR_K_END )      -> ?WXK_END;
myr_keycode_to_wx( ?MYR_K_HOME )     -> ?WXK_HOME;
myr_keycode_to_wx( ?MYR_K_PAGEUP )   -> ?WXK_PAGEUP;
myr_keycode_to_wx( ?MYR_K_PAGEDOWN ) -> ?WXK_PAGEDOWN;

% In SDL2, '_KPn' became '_KP_n':
myr_keycode_to_wx( ?MYR_K_KP_0 ) -> ?WXK_NUMPAD0;
myr_keycode_to_wx( ?MYR_K_KP_1 ) -> ?WXK_NUMPAD1;
myr_keycode_to_wx( ?MYR_K_KP_2 ) -> ?WXK_NUMPAD2;
myr_keycode_to_wx( ?MYR_K_KP_3 ) -> ?WXK_NUMPAD3;
myr_keycode_to_wx( ?MYR_K_KP_4 ) -> ?WXK_NUMPAD4;
myr_keycode_to_wx( ?MYR_K_KP_5 ) -> ?WXK_NUMPAD5;
myr_keycode_to_wx( ?MYR_K_KP_6 ) -> ?WXK_NUMPAD6;
myr_keycode_to_wx( ?MYR_K_KP_7 ) -> ?WXK_NUMPAD7;
myr_keycode_to_wx( ?MYR_K_KP_8 ) -> ?WXK_NUMPAD8;
myr_keycode_to_wx( ?MYR_K_KP_9 ) -> ?WXK_NUMPAD9;

myr_keycode_to_wx( ?MYR_K_KP_MULTIPLY ) -> ?WXK_NUMPAD_MULTIPLY;
myr_keycode_to_wx( ?MYR_K_KP_PLUS )     -> ?WXK_NUMPAD_ADD;
myr_keycode_to_wx( ?MYR_K_KP_MINUS )    -> ?WXK_NUMPAD_SUBTRACT;
myr_keycode_to_wx( ?MYR_K_KP_PERIOD )   -> ?WXK_NUMPAD_DECIMAL;
myr_keycode_to_wx( ?MYR_K_KP_DIVIDE )   -> ?WXK_NUMPAD_DIVIDE;
myr_keycode_to_wx( ?MYR_K_KP_ENTER )    -> ?WXK_NUMPAD_ENTER;

% In SDL2, '_{L,R}LSUPER' became '_{L,R}GUI', we prefer the former convention:
myr_keycode_to_wx( ?MYR_K_LSUPER ) -> ?WXK_WINDOWS_LEFT;
myr_keycode_to_wx( ?MYR_K_RSUPER ) -> ?WXK_WINDOWS_RIGHT;

myr_keycode_to_wx( MyrKeycode ) ->
	trace_utils:warning_fmt( "MyriadGUI keycode '~ts' (i.e. ~B) passed "
		"verbatim to wx.", [ [ MyrKeycode ], MyrKeycode ] ),
	MyrKeycode.



% @doc Returns a textual description of the specified key event, of type
% gui_wx_event_info().
%
-spec key_event_to_string( wxKey() ) -> ustring().
key_event_to_string( WxKeyEvent ) ->

	GUIEnvPid = gui:get_environment_server(),

	EventTranslationTable =
		environment:get( event_translation_table, GUIEnvPid ),

	key_event_to_string( WxKeyEvent, EventTranslationTable ).



% @doc Returns a textual description of the specified key event.
-spec key_event_to_string( wxKey(), event_translation_table() ) -> ustring().
key_event_to_string( #wxKey{ type=WxKeyEventType, x=X, y=Y, keyCode=KeyCode,
		controlDown=CtrlDown, shiftDown=ShiftDown, altDown=AltDown,
		metaDown=MetaDown,
		uniChar=Unichar, rawCode=RawCode, rawFlags=RawFlags },
					 EventTranslationTable ) ->

	KeyEventType = bijective_table:get_second_for( WxKeyEventType,
												   EventTranslationTable ),

	Mods = case CtrlDown of
				true -> [ "control" ];
				false -> []
		   end
		++ case ShiftDown of
				true -> [ "shift" ];
				false -> []
		   end
		++ case AltDown of
				true -> [ "alt" ];
				false -> []
		   end
		++ case MetaDown of
				true -> [ "meta" ];
				false -> []
		   end,

	ModStr = case Mods of

		[] ->
			"no modifier";

		[ Mod ] ->
			text_utils:format( "the ~ts modifier", [ Mod ] );

		_ ->
			text_utils:format( "the ~ts modifiers",
				[ text_utils:strings_to_listed_string( Mods ) ] )

	end,

	text_utils:format( "~ts event at client-coordinate position {~B,~B}, "
		"whose key code is '~w' with ~ts, Unicode char is '~w', "
		"raw code being '~w' and raw flags being ~w",
		[ KeyEventType, X, Y, KeyCode, ModStr, Unichar, RawCode, RawFlags ] ).
