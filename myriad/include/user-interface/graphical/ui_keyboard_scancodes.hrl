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


% These keyboard scancodes (i.e. gui_keyboard:scancode()) are directly coming
% from Loic Hoguin's esdl2 (see [1]:
% https://github.com/ninenines/esdl2/blob/master/include/sdl_scancode.hrl),
% which are themselves deriving in turn from Sam Lantinga's libSDL2 (see [2]:
% https://github.com/libsdl-org/SDL/blob/main/src/events/SDL_keyboard.c).

% Many thanks to both projects.

% Refer to the implementation notes in gui_keyboard.erl for all related
% information, notably for the differences between scancodes and keycodes.


% We prefer exposing uniform prefixes, and not pieces of some backends; so below
% is just [1], with the 'SDL' prefix replaced with the 'MYR' one (for
% MyriadGUI).

% Note that, with SDL, 'GUI' designates the former "Windows" key, we prefer
% naming it 'SUPER' (see https://en.wikipedia.org/wiki/Windows_key).


% Include guard:
-ifndef(__MYRIAD_UI_KEYBOARD_SCANCODES_HRL__).
-define(__MYRIAD_UI_KEYBOARD_SCANCODES_HRL__, 1).



% Definitions of all known scancodes (a.k.a. button locations):

-define(MYR_SCANCODE_A, 4).
-define(MYR_SCANCODE_B, 5).
-define(MYR_SCANCODE_C, 6).
-define(MYR_SCANCODE_D, 7).
-define(MYR_SCANCODE_E, 8).
-define(MYR_SCANCODE_F, 9).
-define(MYR_SCANCODE_G, 10).
-define(MYR_SCANCODE_H, 11).
-define(MYR_SCANCODE_I, 12).
-define(MYR_SCANCODE_J, 13).
-define(MYR_SCANCODE_K, 14).
-define(MYR_SCANCODE_L, 15).
-define(MYR_SCANCODE_M, 16).
-define(MYR_SCANCODE_N, 17).
-define(MYR_SCANCODE_O, 18).
-define(MYR_SCANCODE_P, 19).
-define(MYR_SCANCODE_Q, 20).
-define(MYR_SCANCODE_R, 21).
-define(MYR_SCANCODE_S, 22).
-define(MYR_SCANCODE_T, 23).
-define(MYR_SCANCODE_U, 24).
-define(MYR_SCANCODE_V, 25).
-define(MYR_SCANCODE_W, 26).
-define(MYR_SCANCODE_X, 27).
-define(MYR_SCANCODE_Y, 28).
-define(MYR_SCANCODE_Z, 29).
-define(MYR_SCANCODE_1, 30).
-define(MYR_SCANCODE_2, 31).
-define(MYR_SCANCODE_3, 32).
-define(MYR_SCANCODE_4, 33).
-define(MYR_SCANCODE_5, 34).
-define(MYR_SCANCODE_6, 35).
-define(MYR_SCANCODE_7, 36).
-define(MYR_SCANCODE_8, 37).
-define(MYR_SCANCODE_9, 38).
-define(MYR_SCANCODE_0, 39).
-define(MYR_SCANCODE_RETURN, 40).
-define(MYR_SCANCODE_ESCAPE, 41).
-define(MYR_SCANCODE_BACKSPACE, 42).
-define(MYR_SCANCODE_TAB, 43).
-define(MYR_SCANCODE_SPACE, 44).
-define(MYR_SCANCODE_MINUS, 45).
-define(MYR_SCANCODE_EQUALS, 46).
-define(MYR_SCANCODE_LEFTBRACKET, 47).
-define(MYR_SCANCODE_RIGHTBRACKET, 48).
-define(MYR_SCANCODE_BACKSLASH, 49).
-define(MYR_SCANCODE_NONUSHASH, 50).
-define(MYR_SCANCODE_SEMICOLON, 51).
-define(MYR_SCANCODE_APOSTROPHE, 52).
-define(MYR_SCANCODE_GRAVE, 53).
-define(MYR_SCANCODE_COMMA, 54).
-define(MYR_SCANCODE_PERIOD, 55).
-define(MYR_SCANCODE_SLASH, 56).
-define(MYR_SCANCODE_CAPSLOCK, 57).
-define(MYR_SCANCODE_F1, 58).
-define(MYR_SCANCODE_F2, 59).
-define(MYR_SCANCODE_F3, 60).
-define(MYR_SCANCODE_F4, 61).
-define(MYR_SCANCODE_F5, 62).
-define(MYR_SCANCODE_F6, 63).
-define(MYR_SCANCODE_F7, 64).
-define(MYR_SCANCODE_F8, 65).
-define(MYR_SCANCODE_F9, 66).
-define(MYR_SCANCODE_F10, 67).
-define(MYR_SCANCODE_F11, 68).
-define(MYR_SCANCODE_F12, 69).
-define(MYR_SCANCODE_PRINTSCREEN, 70).
-define(MYR_SCANCODE_SCROLLLOCK, 71).
-define(MYR_SCANCODE_PAUSE, 72).
-define(MYR_SCANCODE_INSERT, 73).
-define(MYR_SCANCODE_HOME, 74).
-define(MYR_SCANCODE_PAGEUP, 75).
-define(MYR_SCANCODE_DELETE, 76).
-define(MYR_SCANCODE_END, 77).
-define(MYR_SCANCODE_PAGEDOWN, 78).
-define(MYR_SCANCODE_RIGHT, 79).
-define(MYR_SCANCODE_LEFT, 80).
-define(MYR_SCANCODE_DOWN, 81).
-define(MYR_SCANCODE_UP, 82).
-define(MYR_SCANCODE_NUMLOCKCLEAR, 83).
-define(MYR_SCANCODE_KP_DIVIDE, 84).
-define(MYR_SCANCODE_KP_MULTIPLY, 85).
-define(MYR_SCANCODE_KP_MINUS, 86).
-define(MYR_SCANCODE_KP_PLUS, 87).
-define(MYR_SCANCODE_KP_ENTER, 88).
-define(MYR_SCANCODE_KP_1, 89).
-define(MYR_SCANCODE_KP_2, 90).
-define(MYR_SCANCODE_KP_3, 91).
-define(MYR_SCANCODE_KP_4, 92).
-define(MYR_SCANCODE_KP_5, 93).
-define(MYR_SCANCODE_KP_6, 94).
-define(MYR_SCANCODE_KP_7, 95).
-define(MYR_SCANCODE_KP_8, 96).
-define(MYR_SCANCODE_KP_9, 97).
-define(MYR_SCANCODE_KP_0, 98).
-define(MYR_SCANCODE_KP_PERIOD, 99).
-define(MYR_SCANCODE_NONUSBACKSLASH, 100).
-define(MYR_SCANCODE_APPLICATION, 101).
-define(MYR_SCANCODE_POWER, 102).
-define(MYR_SCANCODE_KP_EQUALS, 103).
-define(MYR_SCANCODE_F13, 104).
-define(MYR_SCANCODE_F14, 105).
-define(MYR_SCANCODE_F15, 106).
-define(MYR_SCANCODE_F16, 107).
-define(MYR_SCANCODE_F17, 108).
-define(MYR_SCANCODE_F18, 109).
-define(MYR_SCANCODE_F19, 110).
-define(MYR_SCANCODE_F20, 111).
-define(MYR_SCANCODE_F21, 112).
-define(MYR_SCANCODE_F22, 113).
-define(MYR_SCANCODE_F23, 114).
-define(MYR_SCANCODE_F24, 115).
-define(MYR_SCANCODE_EXECUTE, 116).
-define(MYR_SCANCODE_HELP, 117).
-define(MYR_SCANCODE_MENU, 118).
-define(MYR_SCANCODE_SELECT, 119).
-define(MYR_SCANCODE_STOP, 120).
-define(MYR_SCANCODE_AGAIN, 121).
-define(MYR_SCANCODE_UNDO, 122).
-define(MYR_SCANCODE_CUT, 123).
-define(MYR_SCANCODE_COPY, 124).
-define(MYR_SCANCODE_PASTE, 125).
-define(MYR_SCANCODE_FIND, 126).
-define(MYR_SCANCODE_MUTE, 127).
-define(MYR_SCANCODE_VOLUMEUP, 128).
-define(MYR_SCANCODE_VOLUMEDOWN, 129).
-define(MYR_SCANCODE_KP_COMMA, 133).
-define(MYR_SCANCODE_KP_EQUALSAS400, 134).
-define(MYR_SCANCODE_INTERNATIONAL1, 135).
-define(MYR_SCANCODE_INTERNATIONAL2, 136).
-define(MYR_SCANCODE_INTERNATIONAL3, 137).
-define(MYR_SCANCODE_INTERNATIONAL4, 138).
-define(MYR_SCANCODE_INTERNATIONAL5, 139).
-define(MYR_SCANCODE_INTERNATIONAL6, 140).
-define(MYR_SCANCODE_INTERNATIONAL7, 141).
-define(MYR_SCANCODE_INTERNATIONAL8, 142).
-define(MYR_SCANCODE_INTERNATIONAL9, 143).
-define(MYR_SCANCODE_LANG1, 144).
-define(MYR_SCANCODE_LANG2, 145).
-define(MYR_SCANCODE_LANG3, 146).
-define(MYR_SCANCODE_LANG4, 147).
-define(MYR_SCANCODE_LANG5, 148).
-define(MYR_SCANCODE_LANG6, 149).
-define(MYR_SCANCODE_LANG7, 150).
-define(MYR_SCANCODE_LANG8, 151).
-define(MYR_SCANCODE_LANG9, 152).
-define(MYR_SCANCODE_ALTERASE, 153).
-define(MYR_SCANCODE_SYSREQ, 154).
-define(MYR_SCANCODE_CANCEL, 155).
-define(MYR_SCANCODE_CLEAR, 156).
-define(MYR_SCANCODE_PRIOR, 157).
-define(MYR_SCANCODE_RETURN2, 158).
-define(MYR_SCANCODE_SEPARATOR, 159).
-define(MYR_SCANCODE_OUT, 160).
-define(MYR_SCANCODE_OPER, 161).
-define(MYR_SCANCODE_CLEARAGAIN, 162).
-define(MYR_SCANCODE_CRSEL, 163).
-define(MYR_SCANCODE_EXSEL, 164).
-define(MYR_SCANCODE_KP_00, 176).
-define(MYR_SCANCODE_KP_000, 177).
-define(MYR_SCANCODE_THOUSANDSSEPARATOR, 178).
-define(MYR_SCANCODE_DECIMALSEPARATOR, 179).
-define(MYR_SCANCODE_CURRENCYUNIT, 180).
-define(MYR_SCANCODE_CURRENCYSUBUNIT, 181).
-define(MYR_SCANCODE_KP_LEFTPAREN, 182).
-define(MYR_SCANCODE_KP_RIGHTPAREN, 183).
-define(MYR_SCANCODE_KP_LEFTBRACE, 184).
-define(MYR_SCANCODE_KP_RIGHTBRACE, 185).
-define(MYR_SCANCODE_KP_TAB, 186).
-define(MYR_SCANCODE_KP_BACKSPACE, 187).
-define(MYR_SCANCODE_KP_A, 188).
-define(MYR_SCANCODE_KP_B, 189).
-define(MYR_SCANCODE_KP_C, 190).
-define(MYR_SCANCODE_KP_D, 191).
-define(MYR_SCANCODE_KP_E, 192).
-define(MYR_SCANCODE_KP_F, 193).
-define(MYR_SCANCODE_KP_XOR, 194).
-define(MYR_SCANCODE_KP_POWER, 195).
-define(MYR_SCANCODE_KP_PERCENT, 196).
-define(MYR_SCANCODE_KP_LESS, 197).
-define(MYR_SCANCODE_KP_GREATER, 198).
-define(MYR_SCANCODE_KP_AMPERSAND, 199).
-define(MYR_SCANCODE_KP_DBLAMPERSAND, 200).
-define(MYR_SCANCODE_KP_VERTICALBAR, 201).
-define(MYR_SCANCODE_KP_DBLVERTICALBAR, 202).
-define(MYR_SCANCODE_KP_COLON, 203).
-define(MYR_SCANCODE_KP_HASH, 204).
-define(MYR_SCANCODE_KP_SPACE, 205).
-define(MYR_SCANCODE_KP_AT, 206).
-define(MYR_SCANCODE_KP_EXCLAM, 207).
-define(MYR_SCANCODE_KP_MEMSTORE, 208).
-define(MYR_SCANCODE_KP_MEMRECALL, 209).
-define(MYR_SCANCODE_KP_MEMCLEAR, 210).
-define(MYR_SCANCODE_KP_MEMADD, 211).
-define(MYR_SCANCODE_KP_MEMSUBTRACT, 212).
-define(MYR_SCANCODE_KP_MEMMULTIPLY, 213).
-define(MYR_SCANCODE_KP_MEMDIVIDE, 214).
-define(MYR_SCANCODE_KP_PLUSMINUS, 215).
-define(MYR_SCANCODE_KP_CLEAR, 216).
-define(MYR_SCANCODE_KP_CLEARENTRY, 217).
-define(MYR_SCANCODE_KP_BINARY, 218).
-define(MYR_SCANCODE_KP_OCTAL, 219).
-define(MYR_SCANCODE_KP_DECIMAL, 220).
-define(MYR_SCANCODE_KP_HEXADECIMAL, 221).
-define(MYR_SCANCODE_LCTRL, 224).
-define(MYR_SCANCODE_LSHIFT, 225).
-define(MYR_SCANCODE_LALT, 226).

% Preferred to LGUI:
-define(MYR_SCANCODE_LSUPER, 227).

-define(MYR_SCANCODE_RCTRL, 228).
-define(MYR_SCANCODE_RSHIFT, 229).
-define(MYR_SCANCODE_RALT, 230).

% Preferred to RGUI:
-define(MYR_SCANCODE_RSUPER, 231).

-define(MYR_SCANCODE_MODE, 257).
-define(MYR_SCANCODE_AUDIONEXT, 258).
-define(MYR_SCANCODE_AUDIOPREV, 259).
-define(MYR_SCANCODE_AUDIOSTOP, 260).
-define(MYR_SCANCODE_AUDIOPLAY, 261).
-define(MYR_SCANCODE_AUDIOMUTE, 262).
-define(MYR_SCANCODE_MEDIASELECT, 263).
-define(MYR_SCANCODE_WWW, 264).
-define(MYR_SCANCODE_MAIL, 265).
-define(MYR_SCANCODE_CALCULATOR, 266).
-define(MYR_SCANCODE_COMPUTER, 267).
-define(MYR_SCANCODE_AC_SEARCH, 268).
-define(MYR_SCANCODE_AC_HOME, 269).
-define(MYR_SCANCODE_AC_BACK, 270).
-define(MYR_SCANCODE_AC_FORWARD, 271).
-define(MYR_SCANCODE_AC_STOP, 272).
-define(MYR_SCANCODE_AC_REFRESH, 273).
-define(MYR_SCANCODE_AC_BOOKMARKS, 274).
-define(MYR_SCANCODE_BRIGHTNESSDOWN, 275).
-define(MYR_SCANCODE_BRIGHTNESSUP, 276).
-define(MYR_SCANCODE_DISPLAYSWITCH, 277).
-define(MYR_SCANCODE_KBDILLUMTOGGLE, 278).
-define(MYR_SCANCODE_KBDILLUMDOWN, 279).
-define(MYR_SCANCODE_KBDILLUMUP, 280).
-define(MYR_SCANCODE_EJECT, 281).
-define(MYR_SCANCODE_SLEEP, 282).
-define(MYR_SCANCODE_APP1, 283).
-define(MYR_SCANCODE_APP2, 284).
-define(MYR_SCANCODE_AUDIOREWIND, 285).
-define(MYR_SCANCODE_AUDIOFASTFORWARD, 286).

-endif. % __MYRIAD_UI_KEYBOARD_SCANCODES_HRL__
