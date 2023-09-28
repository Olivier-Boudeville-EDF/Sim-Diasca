% Copyright (C) 2022-2023 Olivier Boudeville
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
% Currently the actual backend is wx; in the future it may be any other (e.g.
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

% Scancodes relate only to actual button *locations* (not on any label on top of
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


% As an example, pressing, on a French keyboard with the French (AZERTY) layout
% enabled, the key labelled 'A' (leftmost of the top letter row), reports:
% - a key code ('keyCode' field of the wxKey record) of 65 (with no modifier)
% - a Unicode char ('uniChar' field) of 65
% - a raw code ('rawCode' field) of 97
% - raw flags ('rawFlags') of 24
%
% (let's note that the ASCII code of 'a' is 97, and the one of 'A' is 65)
%
% If now switching to the English (QWERTY) layout, pressing the same key now
% reports:
% - a key code of 81 (with no modifier)
% - a Unicode char of 81
% - a raw code of 113
% - raw flags of 24
%
% So the scan code should correspond to the 'raw flags' field, even if its value
% does not match the SDL-originating MYR_SCANCODE_*.
%
% Our gui_keyboard_test module can be used in order to check scan codes (then
% set CheckScanCode to true).
%
% See also https://docs.wxwidgets.org/stable/classwx_key_event.html for guidance
% regarding key management.


% Description of the wxKey record (defined in lib/wx/include/wx.hrl):
%
% - type :: wxKeyEvent:wxKeyEventType(): for example key_down
%
% - x :: integer(): abscissa of event in focused client area
%
% - y :: integer(): ordinate of event in focused client area
%
% - keyCode :: integer(): should be used to handle special characters
% (non-printable special keys such as cursor arrows keys, HOME, INS, etc.); it
% also returns the character code for Latin-1 keys for compatibility, yet it
% does not work for Unicode characters in general and will return a non-matching
% value for any non-Latin-1 ones; the keycode of non-special keys is an ASCII
% code, which means it will depend on the current keyboard layout
%
% - controlDown :: boolean(): tells whether Ctrl is pressed
%
% - shiftDown :: boolean(): tells whether Shift is pressed
%
% - altDown :: boolean(): tells whether Alt is pressed
%
% - metaDown :: boolean():tells whether Meta ("Super/Windows/Command key") is
% pressed
%
% - uniChar :: integer(): should be used for the printable characters, as it
% works for any of these keys, including non-Latin-1 characters that can be
% entered when using national keyboard layouts; returns 0 for other keys
% (e.g. left arrow)
%
% - rawCode :: integer(): they are platform specific, yet allow to discriminate
% between keys - should uniChar and keyCode report no mapping
%
% - rawFlags :: integer(): correspond to the scan code (layout-independent)


% Example value when hitting the key labelled 'A' on a French keyboard (with a
% French AZERTY layout) on GNU/Linux (with no modifier):
% {wxKey,key_down,601,372,65,false,false,false,false,65,97,24}
% The same when a US QWERTY layout:
% {wxKey,key_down,413,411,81,false,false,false,false,81,113,24}


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


% For the event_context record:
-include("gui_base.hrl").


-type scancode() :: uint32().
% Designates a button at a given location of the keyboard when it is considered
% as a ~104-button joystick.
%
% A scancode is a "button code", it designates a location of a button on any
% keyboard, regardless of the character labelled on the user's actual keyboard.
%
% These locations are designated according to the characters that would be
% printed on a virtual, canonical US QWERTY keyboard, taken as a reference, or
% to non-printable special keys (e.g. the Home key, the Insert one).
%
% A modifier generates a scancode just by itself, as it is a button by itself.
%
% Refer to the corresponding MYR_SCANCODE_* defines.



-type keycode() :: uint32().
% Designates a layout-dependent (Unicode) character code, that is the key
% corresponding to a given character, wherever that key may be on the user's
% actual keyboard (which notably depends on its layout of choice).
%
% A modifier does not generate a keycode just by itself, as it is taken into
% account whenever generating a keycode.
%
% Refer to the corresponding MYR_K_* defines.


-type code_pair() :: { scancode(), keycode() }.
% A pair of codes corresponding to an event regarding a given key.


-type modifier() :: keycode().
% Designates a modifier, like Shift, Control, Alt or Meta.


-type key_transition() :: 'key_down' | 'key_up'.
% Corresponds to a (punctual) state transition of a key.
%
% Note that key down/up transitions are not paired (e.g. if a key is maintained
% in a pressed state, many key down events will be generated, but only one key
% up will be reported at the end, when the key is released).


-type key_status() :: 'pressed'
					| 'released'.
% Corresponds to the (potentially durable) status of a key.


-type keyboard_event_type() ::

	% Event taking into account any modifier (e.g. Control, Shift, Caps Lock)
	% for the returned logical character (e.g. returning 'A' instead of 'a' iff
	% a corresponding modifier applies):
	%
	'onCharEntered'

	% So that parent windows can intercept keys received by focused (child)
	% windows:
	%
	| 'onCharEnteredHook'

	% Event just about the physical key of interest (regardless of any
	% modifier):
	%
	| 'onKeyPressed' % Does not take into account modifiers; for some reason,
					 % associated Unicode characters are uppercased; so hitting
					 % the 'a' key returns the 'A' uchar (not the 'a' one).

	| 'onKeyReleased'.  % See onKeyPressed regarding modifiers and case.



% May integrate modifiers in the future:
%-type key_match() :: 'scancode' | 'keycode'.
% Describes how a key shall be matched.



-type backend_keyboard_event() :: wxKey().


-export_type([ scancode/0, keycode/0, code_pair/0, modifier/0,
			   key_transition/0, key_status/0,
			   keyboard_event_type/0, backend_keyboard_event/0 ]).


-export([ is_modkey_pressed/1, is_key_pressed/1, to_lower/2,
		  get_backend_event/1,

		  get_maybe_uchar/1,

		  get_scancode/1, get_keycode/1, get_code_pair/1,

		  event_context_to_maybe_uchar/1, event_context_to_keycode/1,
		  event_context_to_scancode/1, event_context_to_code_pair/1,

		  key_event_to_string/1 ]).


% Internals:

-export([ wx_keycode_to_myr/1, myr_keycode_to_wx/1 ]).


% Shorthands:

-type uint32() :: type_utils:uint32().

-type ustring() :: text_utils:ustring().
-type uchar() :: text_utils:uchar().

-type event_context() :: gui:event_context().

-type wx_keycode() :: integer().



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



% @doc Tells whether the specified key, designated as a keycode, is pressed.
-spec is_key_pressed( keycode() ) -> boolean().
is_key_pressed( Keycode ) ->
	% A small doubt remains about whether getKeyState/1 expected key or scan
	% codes:
	%
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



% Note: such conversions should be made based on a constant bijective table.


% @doc Returns the MyriadGUI keycode corresponding to the specified wx one.
-spec wx_keycode_to_myr( wx_keycode() ) -> maybe( keycode() ).
% Directly obtained from wings_io_wx:wx_key_map/1:
% (could be substituted with a corresponding edsl2 NIF)

% For some reason defines like MYR_K_KP_MULTIPLY are not found whereas they
% should.

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
% As the MYR_SCANCODE_F* counterparts are currently not defined:
%wx_keycode_to_myr( ?WXK_F13 ) -> ?MYR_K_F13;
%wx_keycode_to_myr( ?WXK_F14 ) -> ?MYR_K_F14;
%wx_keycode_to_myr( ?WXK_F15 ) -> ?MYR_K_F15;

wx_keycode_to_myr( ?WXK_UP )    -> ?MYR_K_UP;
wx_keycode_to_myr( ?WXK_LEFT )  -> ?MYR_K_LEFT;
wx_keycode_to_myr( ?WXK_DOWN )  -> ?MYR_K_DOWN;
wx_keycode_to_myr( ?WXK_RIGHT ) -> ?MYR_K_RIGHT;

wx_keycode_to_myr( ?WXK_DELETE )   -> ?MYR_K_DELETE;
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

%wx_keycode_to_myr( ?WXK_NUMPAD_MULTIPLY ) -> ?MYR_K_KP_MULTIPLY;
wx_keycode_to_myr( ?WXK_NUMPAD_ADD )      -> ?MYR_K_KP_PLUS;
wx_keycode_to_myr( ?WXK_NUMPAD_SUBTRACT ) -> ?MYR_K_KP_MINUS;
wx_keycode_to_myr( ?WXK_NUMPAD_DECIMAL )  -> ?MYR_K_KP_PERIOD;
%wx_keycode_to_myr( ?WXK_NUMPAD_DIVIDE )   -> ?MYR_K_KP_DIVIDE;
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
%myr_keycode_to_wx( ?MYR_K_F13 )  -> ?WXK_F13;
%myr_keycode_to_wx( ?MYR_K_F14 )  -> ?WXK_F14;
%myr_keycode_to_wx( ?MYR_K_F15 )  -> ?WXK_F15;

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

%myr_keycode_to_wx( ?MYR_K_KP_MULTIPLY ) -> ?WXK_NUMPAD_MULTIPLY;
myr_keycode_to_wx( ?MYR_K_KP_PLUS )     -> ?WXK_NUMPAD_ADD;
myr_keycode_to_wx( ?MYR_K_KP_MINUS )    -> ?WXK_NUMPAD_SUBTRACT;
myr_keycode_to_wx( ?MYR_K_KP_PERIOD )   -> ?WXK_NUMPAD_DECIMAL;
%myr_keycode_to_wx( ?MYR_K_KP_DIVIDE )   -> ?WXK_NUMPAD_DIVIDE;
myr_keycode_to_wx( ?MYR_K_KP_ENTER )    -> ?WXK_NUMPAD_ENTER;

% In SDL2, '_{L,R}LSUPER' became '_{L,R}GUI', we prefer the former convention:
myr_keycode_to_wx( ?MYR_K_LSUPER ) -> ?WXK_WINDOWS_LEFT;
myr_keycode_to_wx( ?MYR_K_RSUPER ) -> ?WXK_WINDOWS_RIGHT;

myr_keycode_to_wx( MyrKeycode ) ->
	trace_utils:warning_fmt( "MyriadGUI keycode '~ts' (i.e. ~B) passed "
		"verbatim to wx.", [ [ MyrKeycode ], MyrKeycode ] ),
	MyrKeycode.



% doc Returns the backend keyboard event included in the specified
% (keyboard-related) event context.
%
-spec get_backend_event( event_context() ) -> backend_keyboard_event().
get_backend_event( #event_context{
		backend_event={ 'wx', _WxSrcId, _WxConnectedObj, _UserData,
						WxKeyEvent } } ) ->
	WxKeyEvent.



% @doc Returns the Unicode char corresponding to the printable character (if
% any) referenced in the specified backend keyboard event.
%
% Works for any printable key, including non-Latin-1 characters that can be
% entered when using national keyboard layouts.
%
% Returns 'undefined' when the key corresponds to a non-printable character
% (e.g. PRINTSCREEN or SCROLLLOCK).
%
-spec get_maybe_uchar( backend_keyboard_event() ) -> maybe( uchar() ).
get_maybe_uchar( _WxKey=#wxKey{ uniChar=0 } ) ->
	undefined;

get_maybe_uchar( _WxKey=#wxKey{ uniChar=Unichar } ) ->
	Unichar.



% @doc Returns the scancode corresponding to the key (interpreted as a "button
% code" rather than as any character) referenced in the specified backend
% keyboard event.
%
% So scancodes are location-dependent, do not depend on the current keyboard
% layout and do not specifically correspond to a given character.
%
% See also the corresponding MYR_SCANCODE_* scancode defines.
%
-spec get_scancode( backend_keyboard_event() ) -> scancode().
get_scancode( _WxKey=#wxKey{ rawFlags=Scancode } ) ->
	Scancode.



% @doc Returns the keycode corresponding to the character referenced in the
% specified backend keyboard event.
%
% Keycodes depend on the current keyboard layout and are location-independent
% (they designate a logical key, wherever it actually is on the keyboard); they
% should be used to handle special characters (non-printable special keys such
% as cursor arrows keys, HOME, INS, etc.) when no relevant Unicode character
% applies.
%
% For Latin-1 keys, these values happen to match, for compatibility reasons, the
% ones returned by get_unicode_char/1 (the keycode of non-special keys is
% an ASCII code, which means it will depend on the current keyboard layout); yet
% this does not work for Unicode characters in general.
%
% See also the corresponding MYR_K_* keycode defines.
%
-spec get_keycode( backend_keyboard_event() ) -> keycode().
get_keycode( _WxKey=#wxKey{ keyCode=Keycode } ) ->
	Keycode.


% @doc Returns the scancode and keycode corresponding to the key referenced in
% the specified backend keyboard event.
%
-spec get_code_pair( backend_keyboard_event() ) -> code_pair().
get_code_pair( _WxKey=#wxKey{ rawFlags=Scancode,
							  keyCode=Keycode } ) ->
	{ Scancode, Keycode }.



% @doc Returns the Unicode char corresponding to the printable character (if
% any) referenced in the specified event context corresponding to the receiving
% of a keyboard-related event message (onCharEntered, onKeyPressed, etc.), thus
% expected to include a backend keyboard event.
%
% Refer to get_maybe_uchar/1 for further details.
%
-spec event_context_to_maybe_uchar( event_context() ) -> maybe( uchar() ).
event_context_to_maybe_uchar( EventContext ) ->
	BackendKeyboardEvent = get_backend_event( EventContext ),
	get_maybe_uchar( BackendKeyboardEvent ).


% @doc Returns the keycode corresponding to the character referenced in the
% specified event context corresponding to the receiving of a keyboard-related
% event message (onCharEntered, onKeyPressed, etc.), thus expected to include a
% backend keyboard event.
%
% Refer to get_keycode/1 for further details.
%
-spec event_context_to_keycode( event_context() ) -> keycode().
event_context_to_keycode( EventContext ) ->
	BackendKeyboardEvent = get_backend_event( EventContext ),
	get_keycode( BackendKeyboardEvent ).


% @doc Returns the scancode corresponding to the key (interpreted as a "button
% code" rather than as any character) referenced in the specified event context
% corresponding to the receiving of a keyboard-related event message
% (onCharEntered, onKeyPressed, etc.), thus expected to include a backend
% keyboard event.
%
% Refer to get_scancode/1 for further details.
%
-spec event_context_to_scancode( event_context() ) -> scancode().
event_context_to_scancode( EventContext ) ->
	BackendKeyboardEvent = get_backend_event( EventContext ),
	get_scancode( BackendKeyboardEvent ).



% @doc Returns the scancode/keycode pair corresponding to the key referenced in
% the specified event context corresponding to the receiving of a
% keyboard-related event message (onCharEntered, onKeyPressed, etc.), thus
% expected to include a backend keyboard event.
%
-spec event_context_to_code_pair( event_context() ) -> code_pair().
event_context_to_code_pair( EventContext ) ->
	BackendKeyboardEvent = get_backend_event( EventContext ),
	get_code_pair( BackendKeyboardEvent ).



% @doc Returns a textual description of the specified key event, of type
% gui_wx_event_info().
%
-spec key_event_to_string( wxKey() ) -> ustring().
key_event_to_string( _WxKey=#wxKey{ type=WxKeyEventType,
		x=X, y=Y, keyCode=KeyCode,
		controlDown=CtrlDown, shiftDown=ShiftDown, altDown=AltDown,
		metaDown=MetaDown,
		uniChar=Uchar, rawCode=RawCode, rawFlags=RawFlags } ) ->

	% To check that all fields are interpreted:
	%trace_utils:debug_fmt( "wxKey: ~p", [ WxKey ] ),

	KeyEventType = gui_event:from_wx_event_type( WxKeyEventType ),

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

	UcharStr = case Uchar of

		0 ->
			"undefined";

		_ ->
			text_utils:format( "'~ts' (value: ~B)", [ [ Uchar ], Uchar ] )

	end,

	text_utils:format( "~ts event at client-coordinate position {~B,~B}, "
		"whose keycode is ~w with ~ts, Unicode character is ~ts, "
		"raw code being ~w and scancode being ~w",
		[ KeyEventType, X, Y, KeyCode, ModStr, UcharStr, RawCode, RawFlags ] );


key_event_to_string( Other ) ->
	throw( { invalid_key_event, Other } ).
