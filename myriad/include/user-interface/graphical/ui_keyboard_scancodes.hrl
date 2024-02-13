% Copyright (C) 2022-2024 Olivier Boudeville
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


% These keyboard scancodes (see gui_keyboard:scancode()) were initially
% directly coming from Loic Hoguin's esdl2 (see [1]:
% https://github.com/ninenines/esdl2/blob/master/include/sdl_scancode.hrl),
% which were themselves deriving in turn from Sam Lantinga's libSDL2 (see [2]:
% https://github.com/libsdl-org/SDL/blob/main/src/events/SDL_keyboard.c).
%
% However, through wx, if layout-independent key codes could indeed be found in
% the wxKey records (in the 'rawFlags' field), their value did not match at all
% the SDL-related ones. So we remapped them appropriately, based on the actual
% scancodes wx is returning with our (French, basic) keyboard once set in a US
% layout.
%
% Procedure to check / correct / update these scancodes:
%  $ cd test/user-interface/graphical
%  # Ensure that, in gui_keyboard_test.erl, CheckScanCode is set to true
%  $ /bin/setxkbmap us
%  $ make gui_keyboard_run
%  # Hit a key (e.g. the one labelled "A" on a French keyboard)
%  # Following is output: "[info] Scan code for key 'Q': 24".
%  # So the MYR_SCANCODE_Q define shall be equal to 24 below.

% Design of the reference keyboard used for the MYR_SCANCODE_* scan codes:
% https://en.wikipedia.org/wiki/QWERTY#/media/File:Qwerty.svg or
% https://en.wikipedia.org/wiki/QWERTY#/media/File:KB_United_States-NoAltGr.svg

% Refer to the implementation notes in gui_keyboard.erl for all related
% information, notably for the differences between scancodes and keycodes.


% We prefer exposing uniform prefixes, and not pieces of some backends; so below
% is just [1], with the 'SDL' prefix replaced with the 'MYR' one (standing for
% MyriadGUI) - and revamped values.

% Note that, with SDL, 'GUI' designates the former "Windows" key, we prefer
% naming it 'Super' (see https://en.wikipedia.org/wiki/Windows_key).

% Preferably not to be included directly; include at least the more general
% 'myriad_ui.hrl' instead.

% Include guard:
-ifndef(__MYRIAD_UI_KEYBOARD_SCANCODES_HRL__).
-define(__MYRIAD_UI_KEYBOARD_SCANCODES_HRL__, 1).

% Definitions of all known scancodes - a.k.a. button locations of a standard
% (US) keyboard seen as a ~104-button keypad:
%
% (the values that are commented-out were the SDL ones; in ours there are
% probably mistakes, we have no reference keyboard at hand, any checking/update
% appreciated)


% Structure of a scancode define, using 'MYR_SCANCODE_Q' as an example:
% - 'MYR' for Myriad, to avoid any define clash with third-party
% - 'SCANCODE' (as opposed to keycode)
% - 'q' designating the location of the 'Q' key in a standard keyboard


% Typewriter keys:

% For example with an AZERTY layout, the key returning this scancode is labelled
% 'Q', not 'A':
%
-define(MYR_SCANCODE_A, 38 ).  % 4).

-define(MYR_SCANCODE_B, 56 ).  % 5).
-define(MYR_SCANCODE_C, 54 ).  % 6).
-define(MYR_SCANCODE_D, 40 ).  % 7).
-define(MYR_SCANCODE_E, 26 ).  % 8).
-define(MYR_SCANCODE_F, 41 ).  % 9).
-define(MYR_SCANCODE_G, 42 ).  % 10).
-define(MYR_SCANCODE_H, 43 ).  % 11).
-define(MYR_SCANCODE_I, 31 ).  % 12).
-define(MYR_SCANCODE_J, 44 ).  % 13).
-define(MYR_SCANCODE_K, 45 ).  % 14).
-define(MYR_SCANCODE_L, 46 ).  % 15).
-define(MYR_SCANCODE_M, 58 ).  % 16).
-define(MYR_SCANCODE_N, 57 ).  % 17).
-define(MYR_SCANCODE_O, 32 ).  % 18).
-define(MYR_SCANCODE_P, 33 ).  % 19).
-define(MYR_SCANCODE_Q, 24 ).  % 20).
-define(MYR_SCANCODE_R, 27 ).  % 21).
-define(MYR_SCANCODE_S, 39 ).  % 22).
-define(MYR_SCANCODE_T, 28 ).  % 23).
-define(MYR_SCANCODE_U, 30 ).  % 24).
-define(MYR_SCANCODE_V, 55 ).  % 25).
-define(MYR_SCANCODE_W, 25 ).  % 26).
-define(MYR_SCANCODE_X, 53 ).  % 27).
-define(MYR_SCANCODE_Y, 29 ).  % 28).
-define(MYR_SCANCODE_Z, 52 ).  % 29).

% Top row (the keypad numbers are defined later):
-define(MYR_SCANCODE_1, 10 ).  % 30).
-define(MYR_SCANCODE_2, 11 ).  % 31).
-define(MYR_SCANCODE_3, 12 ).  % 32).
-define(MYR_SCANCODE_4, 13 ).  % 33).
-define(MYR_SCANCODE_5, 14 ).  % 34).
-define(MYR_SCANCODE_6, 15 ).  % 35).
-define(MYR_SCANCODE_7, 16 ).  % 36).
-define(MYR_SCANCODE_8, 17 ).  % 37).
-define(MYR_SCANCODE_9, 18 ).  % 38).
-define(MYR_SCANCODE_0, 19 ).  % 39).

% A.k.a. Enter:
-define(MYR_SCANCODE_RETURN,       36 ).  % 40).

-define(MYR_SCANCODE_ESCAPE,        9 ).  % 41).
-define(MYR_SCANCODE_BACKSPACE,    22 ).  % 42).
-define(MYR_SCANCODE_TAB,          23 ).  % 43).
-define(MYR_SCANCODE_SPACE,        65 ).  % 44).
-define(MYR_SCANCODE_MINUS,        20 ).  % 45).
-define(MYR_SCANCODE_EQUALS,       21 ).  % 46).
-define(MYR_SCANCODE_LEFTBRACKET,  34 ).  % 47).
-define(MYR_SCANCODE_RIGHTBRACKET, 35 ).  % 48).
-define(MYR_SCANCODE_BACKSLASH,    51 ).  % 49).

% "Non-US #" not on my keyboard: -define(MYR_SCANCODE_NONUSHASH,  ).  % 50).

% ";" not on my keyboard: -define(MYR_SCANCODE_SEMICOLON,  ).  % 51).

-define(MYR_SCANCODE_APOSTROPHE, 48 ).  % 52).

% "è" not on my keyboard: -define(MYR_SCANCODE_GRAVE,  ).  % 53).

% ",":
-define(MYR_SCANCODE_COMMA, 59 ).  % 54).

% ".":
-define(MYR_SCANCODE_PERIOD, 60 ).  % 55).

% "/":
-define(MYR_SCANCODE_SLASH, 61 ).  % 56).

-define(MYR_SCANCODE_CAPSLOCK, 66 ).  % 57).


% Function keys:

-define(MYR_SCANCODE_F1,  67 ).  % 58).
-define(MYR_SCANCODE_F2,  68 ).  % 59).
-define(MYR_SCANCODE_F3,  69 ).  % 60).
-define(MYR_SCANCODE_F4,  70 ).  % 61).
-define(MYR_SCANCODE_F5,  71 ).  % 62).
-define(MYR_SCANCODE_F6,  72 ).  % 63).
-define(MYR_SCANCODE_F7,  73 ).  % 64).
-define(MYR_SCANCODE_F8,  74 ).  % 65).
-define(MYR_SCANCODE_F9,  75 ).  % 66).
-define(MYR_SCANCODE_F10, 76 ).  % 67).
-define(MYR_SCANCODE_F11, 95 ).  % 68).
-define(MYR_SCANCODE_F12, 96 ).  % 69).

-define(MYR_SCANCODE_PRINTSCREEN, 107 ).  % 70).
-define(MYR_SCANCODE_SCROLLLOCK,   78 ).  % 71).
-define(MYR_SCANCODE_PAUSE,       127 ).  % 72).

% Rather unsure (unmatching places on my keyboard; possibly permuted):
-define(MYR_SCANCODE_INSERT,   118 ).  % 73).
-define(MYR_SCANCODE_HOME,     110 ).  % 74).
-define(MYR_SCANCODE_PAGEUP,   112 ).  % 75).
-define(MYR_SCANCODE_DELETE,   119 ).  % 76).
-define(MYR_SCANCODE_END,      115 ).  % 77).
-define(MYR_SCANCODE_PAGEDOWN, 117 ).  % 78).


% The arrows, especially useful if not having a keypad:
-define(MYR_SCANCODE_RIGHT, 114 ).  % 79).
-define(MYR_SCANCODE_LEFT,  113 ).  % 80).
-define(MYR_SCANCODE_DOWN,  116 ).  % 81).
-define(MYR_SCANCODE_UP,    111 ).  % 82).


% Numeric keypad:

-define(MYR_SCANCODE_NUMLOCKCLEAR, 77 ).  % 83).
-define(MYR_SCANCODE_KP_DIVIDE,    106 ). % 84).
-define(MYR_SCANCODE_KP_MULTIPLY,  63 ).  % 85).
-define(MYR_SCANCODE_KP_MINUS,     82 ).  % 86).
-define(MYR_SCANCODE_KP_PLUS,      86 ).  % 87).
-define(MYR_SCANCODE_KP_ENTER,     104 ). % 88).

-define(MYR_SCANCODE_KP_1, 87 ).  % 89).
-define(MYR_SCANCODE_KP_2, 88 ).  % 90).
-define(MYR_SCANCODE_KP_3, 89 ).  % 91).
-define(MYR_SCANCODE_KP_4, 83 ).  % 92).
-define(MYR_SCANCODE_KP_5, 84 ).  % 93).
-define(MYR_SCANCODE_KP_6, 85 ).  % 94).
-define(MYR_SCANCODE_KP_7, 79 ).  % 95).
-define(MYR_SCANCODE_KP_8, 80 ).  % 96).
-define(MYR_SCANCODE_KP_9, 81 ).  % 97).
-define(MYR_SCANCODE_KP_0, 90 ).  % 98).

-define(MYR_SCANCODE_KP_PERIOD, 91 ).  % 99).

% Not on my keyboard: -define(MYR_SCANCODE_NONUSBACKSLASH,  ).  % 100).
% Not on my keyboard: -define(MYR_SCANCODE_APPLICATION,  ).  % 101).
% Not on my keyboard: -define(MYR_SCANCODE_POWER,  ).  % 102).
% Not on my keyboard: -define(MYR_SCANCODE_KP_EQUALS,  ).  % 103).
% Not on my keyboard: -define(MYR_SCANCODE_F13,  ).  % 104).
% Not on my keyboard: -define(MYR_SCANCODE_F14,  ).  % 105).
% Not on my keyboard: -define(MYR_SCANCODE_F15,  ).  % 106).
% Not on my keyboard: -define(MYR_SCANCODE_F16,  ).  % 107).
% Not on my keyboard: -define(MYR_SCANCODE_F17,  ).  % 108).
% Not on my keyboard: -define(MYR_SCANCODE_F18,  ).  % 109).
% Not on my keyboard: -define(MYR_SCANCODE_F19,  ).  % 110).
% Not on my keyboard: -define(MYR_SCANCODE_F20,  ).  % 111).
% Not on my keyboard: -define(MYR_SCANCODE_F21,  ).  % 112).
% Not on my keyboard: -define(MYR_SCANCODE_F22,  ).  % 113).
% Not on my keyboard: -define(MYR_SCANCODE_F23,  ).  % 114).
% Not on my keyboard: -define(MYR_SCANCODE_F24,  ).  % 115).
% Not on my keyboard: -define(MYR_SCANCODE_EXECUTE,  ).  % 116).
% Not on my keyboard: -define(MYR_SCANCODE_HELP,  ).  % 117).
% Not on my keyboard: -define(MYR_SCANCODE_MENU,  ).  % 118).
% Not on my keyboard: -define(MYR_SCANCODE_SELECT,  ).  % 119).
% Not on my keyboard: -define(MYR_SCANCODE_STOP,  ).  % 120).
% Not on my keyboard: -define(MYR_SCANCODE_AGAIN,  ).  % 121).
% Not on my keyboard: -define(MYR_SCANCODE_UNDO,  ).  % 122).
% Not on my keyboard: -define(MYR_SCANCODE_CUT,  ).  % 123).
% Not on my keyboard: -define(MYR_SCANCODE_COPY,  ).  % 124).
% Not on my keyboard: -define(MYR_SCANCODE_PASTE,  ).  % 125).
% Not on my keyboard: -define(MYR_SCANCODE_FIND,  ).  % 126).
% Not on my keyboard: -define(MYR_SCANCODE_MUTE,  ).  % 127).
% Not on my keyboard: -define(MYR_SCANCODE_VOLUMEUP,  ).  % 128).
% Not on my keyboard: -define(MYR_SCANCODE_VOLUMEDOWN,  ).  % 129).
% Not on my keyboard: -define(MYR_SCANCODE_KP_COMMA,  ).  % 133).
% Not on my keyboard: -define(MYR_SCANCODE_KP_EQUALSAS400,  ).  % 134).
% Not on my keyboard: -define(MYR_SCANCODE_INTERNATIONAL1,  ).  % 135).
% Not on my keyboard: -define(MYR_SCANCODE_INTERNATIONAL2,  ).  % 136).
% Not on my keyboard: -define(MYR_SCANCODE_INTERNATIONAL3,  ).  % 137).
% Not on my keyboard: -define(MYR_SCANCODE_INTERNATIONAL4,  ).  % 138).
% Not on my keyboard: -define(MYR_SCANCODE_INTERNATIONAL5,  ).  % 139).
% Not on my keyboard: -define(MYR_SCANCODE_INTERNATIONAL6,  ).  % 140).
% Not on my keyboard: -define(MYR_SCANCODE_INTERNATIONAL7,  ).  % 141).
% Not on my keyboard: -define(MYR_SCANCODE_INTERNATIONAL8,  ).  % 142).
% Not on my keyboard: -define(MYR_SCANCODE_INTERNATIONAL9,  ).  % 143).
% Not on my keyboard: -define(MYR_SCANCODE_LANG1,  ).  % 144).
% Not on my keyboard: -define(MYR_SCANCODE_LANG2,  ).  % 145).
% Not on my keyboard: -define(MYR_SCANCODE_LANG3,  ).  % 146).
% Not on my keyboard: -define(MYR_SCANCODE_LANG4,  ).  % 147).
% Not on my keyboard: -define(MYR_SCANCODE_LANG5,  ).  % 148).
% Not on my keyboard: -define(MYR_SCANCODE_LANG6,  ).  % 149).
% Not on my keyboard: -define(MYR_SCANCODE_LANG7,  ).  % 150).
% Not on my keyboard: -define(MYR_SCANCODE_LANG8,  ).  % 151).
% Not on my keyboard: -define(MYR_SCANCODE_LANG9,  ).  % 152).
% Not on my keyboard: -define(MYR_SCANCODE_ALTERASE,  ).  % 153).
% Not on my keyboard: -define(MYR_SCANCODE_SYSREQ,  ).  % 154).
% Not on my keyboard: -define(MYR_SCANCODE_CANCEL,  ).  % 155).
% Not on my keyboard: -define(MYR_SCANCODE_CLEAR,  ).  % 156).
% Not on my keyboard: -define(MYR_SCANCODE_PRIOR,  ).  % 157).
% Not on my keyboard: -define(MYR_SCANCODE_RETURN2,  ).  % 158).
% Not on my keyboard: -define(MYR_SCANCODE_SEPARATOR,  ).  % 159).
% Not on my keyboard: -define(MYR_SCANCODE_OUT,  ).  % 160).
% Not on my keyboard: -define(MYR_SCANCODE_OPER,  ).  % 161).
% Not on my keyboard: -define(MYR_SCANCODE_CLEARAGAIN,  ).  % 162).
% Not on my keyboard: -define(MYR_SCANCODE_CRSEL,  ).  % 163).
% Not on my keyboard: -define(MYR_SCANCODE_EXSEL,  ).  % 164).
% Not on my keyboard: -define(MYR_SCANCODE_KP_00,  ).  % 176).
% Not on my keyboard: -define(MYR_SCANCODE_KP_000,  ).  % 177).
% Not on my keyboard: -define(MYR_SCANCODE_THOUSANDSSEPARATOR,  ).  % 178).
% Not on my keyboard: -define(MYR_SCANCODE_DECIMALSEPARATOR,  ).  % 179).
% Not on my keyboard: -define(MYR_SCANCODE_CURRENCYUNIT,  ).  % 180).
% Not on my keyboard: -define(MYR_SCANCODE_CURRENCYSUBUNIT,  ).  % 181).
% Not on my keyboard: -define(MYR_SCANCODE_KP_LEFTPAREN,  ).  % 182).
% Not on my keyboard: -define(MYR_SCANCODE_KP_RIGHTPAREN,  ).  % 183).
% Not on my keyboard: -define(MYR_SCANCODE_KP_LEFTBRACE,  ).  % 184).
% Not on my keyboard: -define(MYR_SCANCODE_KP_RIGHTBRACE,  ).  % 185).
% Not on my keyboard: -define(MYR_SCANCODE_KP_TAB,  ).  % 186).
% Not on my keyboard: -define(MYR_SCANCODE_KP_BACKSPACE,  ).  % 187).
% Not on my keyboard: -define(MYR_SCANCODE_KP_A,  ).  % 188).
% Not on my keyboard: -define(MYR_SCANCODE_KP_B,  ).  % 189).
% Not on my keyboard: -define(MYR_SCANCODE_KP_C,  ).  % 190).
% Not on my keyboard: -define(MYR_SCANCODE_KP_D,  ).  % 191).
% Not on my keyboard: -define(MYR_SCANCODE_KP_E,  ).  % 192).
% Not on my keyboard: -define(MYR_SCANCODE_KP_F,  ).  % 193).
% Not on my keyboard: -define(MYR_SCANCODE_KP_XOR,  ).  % 194).
% Not on my keyboard: -define(MYR_SCANCODE_KP_POWER,  ).  % 195).
% Not on my keyboard: -define(MYR_SCANCODE_KP_PERCENT,  ).  % 196).
% Not on my keyboard: -define(MYR_SCANCODE_KP_LESS,  ).  % 197).
% Not on my keyboard: -define(MYR_SCANCODE_KP_GREATER,  ).  % 198).
% Not on my keyboard: -define(MYR_SCANCODE_KP_AMPERSAND,  ).  % 199).
% Not on my keyboard: -define(MYR_SCANCODE_KP_DBLAMPERSAND,  ).  % 200).
% Not on my keyboard: -define(MYR_SCANCODE_KP_VERTICALBAR,  ).  % 201).
% Not on my keyboard: -define(MYR_SCANCODE_KP_DBLVERTICALBAR,  ).  % 202).
% Not on my keyboard: -define(MYR_SCANCODE_KP_COLON,  ).  % 203).
% Not on my keyboard: -define(MYR_SCANCODE_KP_HASH,  ).  % 204).
% Not on my keyboard: -define(MYR_SCANCODE_KP_SPACE,  ).  % 205).
% Not on my keyboard: -define(MYR_SCANCODE_KP_AT,  ).  % 206).
% Not on my keyboard: -define(MYR_SCANCODE_KP_EXCLAM,  ).  % 207).
% Not on my keyboard: -define(MYR_SCANCODE_KP_MEMSTORE,  ).  % 208).
% Not on my keyboard: -define(MYR_SCANCODE_KP_MEMRECALL,  ).  % 209).
% Not on my keyboard: -define(MYR_SCANCODE_KP_MEMCLEAR,  ).  % 210).
% Not on my keyboard: -define(MYR_SCANCODE_KP_MEMADD,  ).  % 211).
% Not on my keyboard: -define(MYR_SCANCODE_KP_MEMSUBTRACT,  ).  % 212).
% Not on my keyboard: -define(MYR_SCANCODE_KP_MEMMULTIPLY,  ).  % 213).
% Not on my keyboard: -define(MYR_SCANCODE_KP_MEMDIVIDE,  ).  % 214).
% Not on my keyboard: -define(MYR_SCANCODE_KP_PLUSMINUS,  ).  % 215).
% Not on my keyboard: -define(MYR_SCANCODE_KP_CLEAR,  ).  % 216).
% Not on my keyboard: -define(MYR_SCANCODE_KP_CLEARENTRY,  ).  % 217).
% Not on my keyboard: -define(MYR_SCANCODE_KP_BINARY,  ).  % 218).
% Not on my keyboard: -define(MYR_SCANCODE_KP_OCTAL,  ).  % 219).
% Not on my keyboard: -define(MYR_SCANCODE_KP_DECIMAL,  ).  % 220).
% Not on my keyboard: -define(MYR_SCANCODE_KP_HEXADECIMAL,  ).  % 221).

-define(MYR_SCANCODE_LCTRL,  37 ).  % 224).
-define(MYR_SCANCODE_LSHIFT, 50 ).  % 225).
-define(MYR_SCANCODE_LALT,   64 ).  % 226).

% Preferred to LGUI:
-define(MYR_SCANCODE_LSUPER, 133 ).  % 227).

-define(MYR_SCANCODE_RCTRL,  105 ).  % 228).
-define(MYR_SCANCODE_RSHIFT, 62 ).   % 229).
-define(MYR_SCANCODE_RALT,   108 ).  % 230).

% Preferred to RGUI:
-define(MYR_SCANCODE_RSUPER, 135 ).  % 231).

% Not on my keyboard: -define(MYR_SCANCODE_MODE,  ).  % 257).
% Not on my keyboard: -define(MYR_SCANCODE_AUDIONEXT,  ).  % 258).
% Not on my keyboard: -define(MYR_SCANCODE_AUDIOPREV,  ).  % 259).
% Not on my keyboard: -define(MYR_SCANCODE_AUDIOSTOP,  ).  % 260).
% Not on my keyboard: -define(MYR_SCANCODE_AUDIOPLAY,  ).  % 261).
% Not on my keyboard: -define(MYR_SCANCODE_AUDIOMUTE,  ).  % 262).
% Not on my keyboard: -define(MYR_SCANCODE_MEDIASELECT,  ).  % 263).
% Not on my keyboard: -define(MYR_SCANCODE_WWW,  ).  % 264).
% Not on my keyboard: -define(MYR_SCANCODE_MAIL,  ).  % 265).
% Not on my keyboard: -define(MYR_SCANCODE_CALCULATOR,  ).  % 266).
% Not on my keyboard: -define(MYR_SCANCODE_COMPUTER,  ).  % 267).
% Not on my keyboard: -define(MYR_SCANCODE_AC_SEARCH,  ).  % 268).
% Not on my keyboard: -define(MYR_SCANCODE_AC_HOME,  ).  % 269).
% Not on my keyboard: -define(MYR_SCANCODE_AC_BACK,  ).  % 270).
% Not on my keyboard: -define(MYR_SCANCODE_AC_FORWARD,  ).  % 271).
% Not on my keyboard: -define(MYR_SCANCODE_AC_STOP,  ).  % 272).
% Not on my keyboard: -define(MYR_SCANCODE_AC_REFRESH,  ).  % 273).
% Not on my keyboard: -define(MYR_SCANCODE_AC_BOOKMARKS,  ).  % 274).
% Not on my keyboard: -define(MYR_SCANCODE_BRIGHTNESSDOWN,  ).  % 275).
% Not on my keyboard: -define(MYR_SCANCODE_BRIGHTNESSUP,  ).  % 276).
% Not on my keyboard: -define(MYR_SCANCODE_DISPLAYSWITCH,  ).  % 277).
% Not on my keyboard: -define(MYR_SCANCODE_KBDILLUMTOGGLE,  ).  % 278).
% Not on my keyboard: -define(MYR_SCANCODE_KBDILLUMDOWN,  ).  % 279).
% Not on my keyboard: -define(MYR_SCANCODE_KBDILLUMUP,  ).  % 280).
% Not on my keyboard: -define(MYR_SCANCODE_EJECT,  ).  % 281).
% Not on my keyboard: -define(MYR_SCANCODE_SLEEP,  ).  % 282).
% Not on my keyboard: -define(MYR_SCANCODE_APP1,  ).  % 283).
% Not on my keyboard: -define(MYR_SCANCODE_APP2,  ).  % 284).
% Not on my keyboard: -define(MYR_SCANCODE_AUDIOREWIND,  ).  % 285).
% Not on my keyboard: -define(MYR_SCANCODE_AUDIOFASTFORWARD,  ).  % 286).

-endif. % __MYRIAD_UI_KEYBOARD_SCANCODES_HRL__
