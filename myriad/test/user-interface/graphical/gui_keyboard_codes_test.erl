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
% Creation date: Sunday, July 9, 2023.


% @doc Testing of the <b>MyriadGUI keyboard scancodes and keycodes</b>.
%
% See the gui_keyboard.erl tested module.
%
-module(gui_keyboard_codes_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-include_lib("myriad/include/myriad_ui.hrl").


% Shorthands:

-type ustring() :: text_utils:ustring().

-type scancode() :: gui_keyboard:scancode().
-type keycode() :: gui_keyboard:keycode().



% @doc Lists all known scancodes: name of their define, then (decimal) value.
-spec list_scancode_pairs() -> { ustring(), scancode() }.
list_scancode_pairs() ->
	[	{ "MYR_SCANCODE_A", ?MYR_SCANCODE_A },
		{ "MYR_SCANCODE_B", ?MYR_SCANCODE_B },
		{ "MYR_SCANCODE_C", ?MYR_SCANCODE_C },
		{ "MYR_SCANCODE_D", ?MYR_SCANCODE_D },
		{ "MYR_SCANCODE_E", ?MYR_SCANCODE_E },
		{ "MYR_SCANCODE_F", ?MYR_SCANCODE_F },
		{ "MYR_SCANCODE_G", ?MYR_SCANCODE_G },
		{ "MYR_SCANCODE_H", ?MYR_SCANCODE_H },
		{ "MYR_SCANCODE_I", ?MYR_SCANCODE_I },
		{ "MYR_SCANCODE_J", ?MYR_SCANCODE_J },
		{ "MYR_SCANCODE_K", ?MYR_SCANCODE_K },
		{ "MYR_SCANCODE_L", ?MYR_SCANCODE_L },
		{ "MYR_SCANCODE_M", ?MYR_SCANCODE_M },
		{ "MYR_SCANCODE_N", ?MYR_SCANCODE_N },
		{ "MYR_SCANCODE_O", ?MYR_SCANCODE_O },
		{ "MYR_SCANCODE_P", ?MYR_SCANCODE_P },
		{ "MYR_SCANCODE_Q", ?MYR_SCANCODE_Q },
		{ "MYR_SCANCODE_R", ?MYR_SCANCODE_R },
		{ "MYR_SCANCODE_S", ?MYR_SCANCODE_S },
		{ "MYR_SCANCODE_T", ?MYR_SCANCODE_T },
		{ "MYR_SCANCODE_U", ?MYR_SCANCODE_U },
		{ "MYR_SCANCODE_V", ?MYR_SCANCODE_V },
		{ "MYR_SCANCODE_W", ?MYR_SCANCODE_W },
		{ "MYR_SCANCODE_X", ?MYR_SCANCODE_X },
		{ "MYR_SCANCODE_Y", ?MYR_SCANCODE_Y },
		{ "MYR_SCANCODE_Z", ?MYR_SCANCODE_Z },

		{ "MYR_SCANCODE_1", ?MYR_SCANCODE_1 },
		{ "MYR_SCANCODE_2", ?MYR_SCANCODE_2 },
		{ "MYR_SCANCODE_3", ?MYR_SCANCODE_3 },
		{ "MYR_SCANCODE_4", ?MYR_SCANCODE_4 },
		{ "MYR_SCANCODE_5", ?MYR_SCANCODE_5 },
		{ "MYR_SCANCODE_6", ?MYR_SCANCODE_6 },
		{ "MYR_SCANCODE_7", ?MYR_SCANCODE_7 },
		{ "MYR_SCANCODE_8", ?MYR_SCANCODE_8 },
		{ "MYR_SCANCODE_9", ?MYR_SCANCODE_9 },
		{ "MYR_SCANCODE_0", ?MYR_SCANCODE_0 },


		{ "MYR_SCANCODE_RETURN",       ?MYR_SCANCODE_RETURN },

		{ "MYR_SCANCODE_ESCAPE",       ?MYR_SCANCODE_ESCAPE      },
		{ "MYR_SCANCODE_BACKSPACE",    ?MYR_SCANCODE_BACKSPACE    },
		{ "MYR_SCANCODE_TAB",          ?MYR_SCANCODE_TAB          },
		{ "MYR_SCANCODE_SPACE",        ?MYR_SCANCODE_SPACE        },
		{ "MYR_SCANCODE_MINUS",        ?MYR_SCANCODE_MINUS        },
		{ "MYR_SCANCODE_EQUALS",       ?MYR_SCANCODE_EQUALS       },
		{ "MYR_SCANCODE_LEFTBRACKET",  ?MYR_SCANCODE_LEFTBRACKET  },
		{ "MYR_SCANCODE_RIGHTBRACKET", ?MYR_SCANCODE_RIGHTBRACKET },
		{ "MYR_SCANCODE_BACKSLASH",    ?MYR_SCANCODE_BACKSLASH    },


		%{ "MYR_SCANCODE_NONUSHASH",  ?MYR_SCANCODE_NONUSHASH  },
		%{ "MYR_SCANCODE_SEMICOLON",  ?MYR_SCANCODE_SEMICOLON  },
		%{ "MYR_SCANCODE_APOSTROPHE", ?MYR_SCANCODE_APOSTROPHE },
		%{ "MYR_SCANCODE_GRAVE",      ?MYR_SCANCODE_GRAVE      },
		%{ "MYR_SCANCODE_COMMA",      ?MYR_SCANCODE_COMMA      },
		%{ "MYR_SCANCODE_PERIOD"      ?MYR_SCANCODE_PERIOD     },

		%{ "MYR_SCANCODE_SLASH",    ?MYR_SCANCODE_SLASH    },
		%{ "MYR_SCANCODE_CAPSLOCK", ?MYR_SCANCODE_CAPSLOCK },

		%{ "MYR_SCANCODE_F1",  ?MYR_SCANCODE_F1  },
		%{ "MYR_SCANCODE_F2",  ?MYR_SCANCODE_F2  },
		%{ "MYR_SCANCODE_F3",  ?MYR_SCANCODE_F3  },
		%{ "MYR_SCANCODE_F4",  ?MYR_SCANCODE_F4  },
		%{ "MYR_SCANCODE_F5",  ?MYR_SCANCODE_F5  },
		%{ "MYR_SCANCODE_F6",  ?MYR_SCANCODE_F6  },
		%{ "MYR_SCANCODE_F7",  ?MYR_SCANCODE_F7  },
		%{ "MYR_SCANCODE_F8",  ?MYR_SCANCODE_F8  },
		%{ "MYR_SCANCODE_F9",  ?MYR_SCANCODE_F9  },
		%{ "MYR_SCANCODE_F10", ?MYR_SCANCODE_F10 },
		%{ "MYR_SCANCODE_F11", ?MYR_SCANCODE_F11 },
		%{ "MYR_SCANCODE_F12", ?MYR_SCANCODE_F12 },

		%{ "MYR_SCANCODE_PRINTSCREEN", ?MYR_SCANCODE_PRINTSCREEN },
		%{ "MYR_SCANCODE_SCROLLLOCK",  ?MYR_SCANCODE_SCROLLLOCK  },
		%{ "MYR_SCANCODE_PAUSE",       ?MYR_SCANCODE_PAUSE       },

		%{ "MYR_SCANCODE_INSERT",   ?MYR_SCANCODE_INSERT   },
		%{ "MYR_SCANCODE_HOME",     ?MYR_SCANCODE_HOME     },
		%{ "MYR_SCANCODE_PAGEUP",   ?MYR_SCANCODE_PAGEUP   },
		%{ "MYR_SCANCODE_DELETE",   ?MYR_SCANCODE_DELETE   },
		%{ "MYR_SCANCODE_END",      ?MYR_SCANCODE_END      },
		%{ "MYR_SCANCODE_PAGEDOWN", ?MYR_SCANCODE_PAGEDOWN },

		%{ "MYR_SCANCODE_RIGHT", ?MYR_SCANCODE_RIGHT },
		%{ "MYR_SCANCODE_LEFT",  ?MYR_SCANCODE_LEFT  },
		%{ "MYR_SCANCODE_DOWN",  ?MYR_SCANCODE_DOWN  },
		%{ "MYR_SCANCODE_UP",    ?MYR_SCANCODE_UP    },

		%{ "MYR_SCANCODE_NUMLOCKCLEAR", ?MYR_SCANCODE_NUMLOCKCLEAR },
		%{ "MYR_SCANCODE_KP_DIVIDE",    ?MYR_SCANCODE_KP_DIVIDE    },
		%{ "MYR_SCANCODE_KP_MULTIPLY",  ?MYR_SCANCODE_KP_MULTIPLY  },
		%{ "MYR_SCANCODE_KP_MINUS",     ?MYR_SCANCODE_KP_MINUS     },
		%{ "MYR_SCANCODE_KP_PLUS",      ?MYR_SCANCODE_KP_PLUS      },
		%{ "MYR_SCANCODE_KP_ENTER",     ?MYR_SCANCODE_KP_ENTER     },

		%{ "MYR_SCANCODE_KP_1", ?MYR_SCANCODE_KP_1 },
		%{ "MYR_SCANCODE_KP_2", ?MYR_SCANCODE_KP_2 },
		%{ "MYR_SCANCODE_KP_3", ?MYR_SCANCODE_KP_3 },
		%{ "MYR_SCANCODE_KP_4", ?MYR_SCANCODE_KP_4 },
		%{ "MYR_SCANCODE_KP_5", ?MYR_SCANCODE_KP_5 },
		%{ "MYR_SCANCODE_KP_6", ?MYR_SCANCODE_KP_6 },
		%{ "MYR_SCANCODE_KP_7", ?MYR_SCANCODE_KP_7 },
		%{ "MYR_SCANCODE_KP_8", ?MYR_SCANCODE_KP_8 },
		%{ "MYR_SCANCODE_KP_9", ?MYR_SCANCODE_KP_9 },
		%{ "MYR_SCANCODE_KP_0", ?MYR_SCANCODE_KP_0 },

		%{ "MYR_SCANCODE_KP_PERIOD", ?MYR_SCANCODE_KP_PERIOD },

		%{ "MYR_SCANCODE_NONUSBACKSLASH",     ?MYR_SCANCODE_NONUSBACKSLASH },
		%{ "MYR_SCANCODE_APPLICATION",        ?MYR_SCANCODE_APPLICATION },
		%{ "MYR_SCANCODE_POWER",              ?MYR_SCANCODE_POWER },
		%{ "MYR_SCANCODE_KP_EQUALS",          ?MYR_SCANCODE_KP_EQUALS },
		%{ "MYR_SCANCODE_F13",                ?MYR_SCANCODE_F13 },
		%{ "MYR_SCANCODE_F14",                ?MYR_SCANCODE_F14 },
		%{ "MYR_SCANCODE_F15",                ?MYR_SCANCODE_F15 },
		%{ "MYR_SCANCODE_F16",                ?MYR_SCANCODE_F16 },
		%{ "MYR_SCANCODE_F17",                ?MYR_SCANCODE_F17 },
		%{ "MYR_SCANCODE_F18",                ?MYR_SCANCODE_F18 },
		%{ "MYR_SCANCODE_F19",                ?MYR_SCANCODE_F19 },
		%{ "MYR_SCANCODE_F20",                ?MYR_SCANCODE_F20 },
		%{ "MYR_SCANCODE_F21",                ?MYR_SCANCODE_F21 },
		%{ "MYR_SCANCODE_F22",                ?MYR_SCANCODE_F22 },
		%{ "MYR_SCANCODE_F23",                ?MYR_SCANCODE_F23 },
		%{ "MYR_SCANCODE_F24",                ?MYR_SCANCODE_F24 },
		%{ "MYR_SCANCODE_EXECUTE",            ?MYR_SCANCODE_EXECUTE },
		%{ "MYR_SCANCODE_HELP",               ?MYR_SCANCODE_HELP },
		%{ "MYR_SCANCODE_MENU",               ?MYR_SCANCODE_MENU },
		%{ "MYR_SCANCODE_SELECT",             ?MYR_SCANCODE_SELECT },
		%{ "MYR_SCANCODE_STOP",               ?MYR_SCANCODE_STOP },
		%{ "MYR_SCANCODE_AGAIN",              ?MYR_SCANCODE_AGAIN },
		%{ "MYR_SCANCODE_UNDO",               ?MYR_SCANCODE_UNDO },
		%{ "MYR_SCANCODE_CUT",                ?MYR_SCANCODE_CUT },
		%{ "MYR_SCANCODE_COPY",               ?MYR_SCANCODE_COPY },
		%{ "MYR_SCANCODE_PASTE",              ?MYR_SCANCODE_PASTE },
		%{ "MYR_SCANCODE_FIND",               ?MYR_SCANCODE_FIND },
		%{ "MYR_SCANCODE_MUTE",               ?MYR_SCANCODE_MUTE },
		%{ "MYR_SCANCODE_VOLUMEUP",           ?MYR_SCANCODE_VOLUMEUP },
		%{ "MYR_SCANCODE_VOLUMEDOWN",         ?MYR_SCANCODE_VOLUMEDOWN },
		%{ "MYR_SCANCODE_KP_COMMA",           ?MYR_SCANCODE_KP_COMMA },
		%{ "MYR_SCANCODE_KP_EQUALSAS400",     ?MYR_SCANCODE_KP_EQUALSAS400 },
		%{ "MYR_SCANCODE_INTERNATIONAL1",     ?MYR_SCANCODE_INTERNATIONAL1 },
		%{ "MYR_SCANCODE_INTERNATIONAL2",     ?MYR_SCANCODE_INTERNATIONAL2 },
		%{ "MYR_SCANCODE_INTERNATIONAL3",     ?MYR_SCANCODE_INTERNATIONAL3 },
		%{ "MYR_SCANCODE_INTERNATIONAL4",     ?MYR_SCANCODE_INTERNATIONAL4 },
		%{ "MYR_SCANCODE_INTERNATIONAL5",     ?MYR_SCANCODE_INTERNATIONAL5 },
		%{ "MYR_SCANCODE_INTERNATIONAL6",     ?MYR_SCANCODE_INTERNATIONAL6 },
		%{ "MYR_SCANCODE_INTERNATIONAL7",     ?MYR_SCANCODE_INTERNATIONAL7 },
		%{ "MYR_SCANCODE_INTERNATIONAL8",     ?MYR_SCANCODE_INTERNATIONAL8 },
		%{ "MYR_SCANCODE_INTERNATIONAL9",     ?MYR_SCANCODE_INTERNATIONAL9 },
		%{ "MYR_SCANCODE_LANG1",              ?MYR_SCANCODE_LANG1 },
		%{ "MYR_SCANCODE_LANG2",              ?MYR_SCANCODE_LANG2 },
		%{ "MYR_SCANCODE_LANG3",              ?MYR_SCANCODE_LANG3 },
		%{ "MYR_SCANCODE_LANG4",              ?MYR_SCANCODE_LANG4 },
		%{ "MYR_SCANCODE_LANG5",              ?MYR_SCANCODE_LANG5 },
		%{ "MYR_SCANCODE_LANG6",              ?MYR_SCANCODE_LANG6 },
		%{ "MYR_SCANCODE_LANG7",              ?MYR_SCANCODE_LANG7 },
		%{ "MYR_SCANCODE_LANG8",              ?MYR_SCANCODE_LANG8 },
		%{ "MYR_SCANCODE_LANG9",              ?MYR_SCANCODE_LANG9 },
		%{ "MYR_SCANCODE_ALTERASE",           ?MYR_SCANCODE_ALTERASE },
		%{ "MYR_SCANCODE_SYSREQ",             ?MYR_SCANCODE_SYSREQ },
		%{ "MYR_SCANCODE_CANCEL",             ?MYR_SCANCODE_CANCEL },
		%{ "MYR_SCANCODE_CLEAR",              ?MYR_SCANCODE_CLEAR },
		%{ "MYR_SCANCODE_PRIOR",              ?MYR_SCANCODE_PRIOR },
		%{ "MYR_SCANCODE_RETURN2",            ?MYR_SCANCODE_RETURN2 },
		%{ "MYR_SCANCODE_SEPARATOR",          ?MYR_SCANCODE_SEPARATOR },
		%{ "MYR_SCANCODE_OUT",                ?MYR_SCANCODE_OUT },
		%{ "MYR_SCANCODE_OPER",               ?MYR_SCANCODE_OPER },
		%{ "MYR_SCANCODE_CLEARAGAIN",         ?MYR_SCANCODE_CLEARAGAIN },
		%{ "MYR_SCANCODE_CRSEL",              ?MYR_SCANCODE_CRSEL },
		%{ "MYR_SCANCODE_EXSEL",              ?MYR_SCANCODE_EXSEL },
		%{ "MYR_SCANCODE_KP_00",              ?MYR_SCANCODE_KP_00 },
		%{ "MYR_SCANCODE_KP_000",             ?MYR_SCANCODE_KP_000 },
		%{ "MYR_SCANCODE_THOUSANDSSEPARATOR", ?MYR_SCANCODE_THOUSANDSSEPARATOR },
		%{ "MYR_SCANCODE_DECIMALSEPARATOR",   ?MYR_SCANCODE_DECIMALSEPARATOR },
		%{ "MYR_SCANCODE_CURRENCYUNIT",       ?MYR_SCANCODE_CURRENCYUNIT },
		%{ "MYR_SCANCODE_CURRENCYSUBUNIT",    ?MYR_SCANCODE_CURRENCYSUBUNIT },
		%{ "MYR_SCANCODE_KP_LEFTPAREN",       ?MYR_SCANCODE_KP_LEFTPAREN },
		%{ "MYR_SCANCODE_KP_RIGHTPAREN",      ?MYR_SCANCODE_KP_RIGHTPAREN },
		%{ "MYR_SCANCODE_KP_LEFTBRACE",       ?MYR_SCANCODE_KP_LEFTBRACE },
		%{ "MYR_SCANCODE_KP_RIGHTBRACE",      ?MYR_SCANCODE_KP_RIGHTBRACE },
		%{ "MYR_SCANCODE_KP_TAB",             ?MYR_SCANCODE_KP_TAB },
		%{ "MYR_SCANCODE_KP_BACKSPACE",       ?MYR_SCANCODE_KP_BACKSPACE },
		%{ "MYR_SCANCODE_KP_A",               ?MYR_SCANCODE_KP_A },
		%{ "MYR_SCANCODE_KP_B",               ?MYR_SCANCODE_KP_B },
		%{ "MYR_SCANCODE_KP_C",               ?MYR_SCANCODE_KP_C },
		%{ "MYR_SCANCODE_KP_D",               ?MYR_SCANCODE_KP_D },
		%{ "MYR_SCANCODE_KP_E",               ?MYR_SCANCODE_KP_E },
		%{ "MYR_SCANCODE_KP_F",               ?MYR_SCANCODE_KP_F },
		%{ "MYR_SCANCODE_KP_XOR",             ?MYR_SCANCODE_KP_XOR },
		%{ "MYR_SCANCODE_KP_POWER",           ?MYR_SCANCODE_KP_POWER },
		%{ "MYR_SCANCODE_KP_PERCENT",         ?MYR_SCANCODE_KP_PERCENT },
		%{ "MYR_SCANCODE_KP_LESS",            ?MYR_SCANCODE_KP_LESS },
		%{ "MYR_SCANCODE_KP_GREATER",         ?MYR_SCANCODE_KP_GREATER },
		%{ "MYR_SCANCODE_KP_AMPERSAND",       ?MYR_SCANCODE_KP_AMPERSAND },
		%{ "MYR_SCANCODE_KP_DBLAMPERSAND",    ?MYR_SCANCODE_KP_DBLAMPERSAND },
		%{ "MYR_SCANCODE_KP_VERTICALBAR",     ?MYR_SCANCODE_KP_VERTICALBAR },
		%{ "MYR_SCANCODE_KP_DBLVERTICALBAR",  ?MYR_SCANCODE_KP_DBLVERTICALBAR },
		%{ "MYR_SCANCODE_KP_COLON",           ?MYR_SCANCODE_KP_COLON },
		%{ "MYR_SCANCODE_KP_HASH",            ?MYR_SCANCODE_KP_HASH },
		%{ "MYR_SCANCODE_KP_SPACE",           ?MYR_SCANCODE_KP_SPACE },
		%{ "MYR_SCANCODE_KP_AT",              ?MYR_SCANCODE_KP_AT },
		%{ "MYR_SCANCODE_KP_EXCLAM",          ?MYR_SCANCODE_KP_EXCLAM },
		%{ "MYR_SCANCODE_KP_MEMSTORE",        ?MYR_SCANCODE_KP_MEMSTORE },
		%{ "MYR_SCANCODE_KP_MEMRECALL",       ?MYR_SCANCODE_KP_MEMRECALL },
		%{ "MYR_SCANCODE_KP_MEMCLEAR",        ?MYR_SCANCODE_KP_MEMCLEAR },
		%{ "MYR_SCANCODE_KP_MEMADD",          ?MYR_SCANCODE_KP_MEMADD },
		%{ "MYR_SCANCODE_KP_MEMSUBTRACT",     ?MYR_SCANCODE_KP_MEMSUBTRACT },
		%{ "MYR_SCANCODE_KP_MEMMULTIPLY",     ?MYR_SCANCODE_KP_MEMMULTIPLY },
		%{ "MYR_SCANCODE_KP_MEMDIVIDE",       ?MYR_SCANCODE_KP_MEMDIVIDE },
		%{ "MYR_SCANCODE_KP_PLUSMINUS",       ?MYR_SCANCODE_KP_PLUSMINUS },
		%{ "MYR_SCANCODE_KP_CLEAR",           ?MYR_SCANCODE_KP_CLEAR },
		%{ "MYR_SCANCODE_KP_CLEARENTRY",      ?MYR_SCANCODE_KP_CLEARENTRY },
		%{ "MYR_SCANCODE_KP_BINARY",          ?MYR_SCANCODE_KP_BINARY },
		%{ "MYR_SCANCODE_KP_OCTAL",           ?MYR_SCANCODE_KP_OCTAL },
		%{ "MYR_SCANCODE_KP_DECIMAL",         ?MYR_SCANCODE_KP_DECIMAL },
		%{ "MYR_SCANCODE_KP_HEXADECIMAL",     ?MYR_SCANCODE_KP_HEXADECIMAL },

		{ "MYR_SCANCODE_LCTRL",  ?MYR_SCANCODE_LCTRL  },
		{ "MYR_SCANCODE_LSHIFT", ?MYR_SCANCODE_LSHIFT },
		{ "MYR_SCANCODE_LALT",   ?MYR_SCANCODE_LALT   },

		{ "MYR_SCANCODE_LSUPER", ?MYR_SCANCODE_LSUPER },

		{ "MYR_SCANCODE_RCTRL",  ?MYR_SCANCODE_RCTRL  },
		{ "MYR_SCANCODE_RSHIFT", ?MYR_SCANCODE_RSHIFT },
		{ "MYR_SCANCODE_RALT",   ?MYR_SCANCODE_RALT   },

		{ "MYR_SCANCODE_RSUPER", ?MYR_SCANCODE_RSUPER }

		%{ "MYR_SCANCODE_MODE",             ?MYR_SCANCODE_MODE },
		%{ "MYR_SCANCODE_AUDIONEXT",        ?MYR_SCANCODE_AUDIONEXT },
		%{ "MYR_SCANCODE_AUDIOPREV",        ?MYR_SCANCODE_AUDIOPREV },
		%{ "MYR_SCANCODE_AUDIOSTOP",        ?MYR_SCANCODE_AUDIOSTOP },
		%{ "MYR_SCANCODE_AUDIOPLAY",        ?MYR_SCANCODE_AUDIOPLAY },
		%{ "MYR_SCANCODE_AUDIOMUTE",        ?MYR_SCANCODE_AUDIOMUTE},
		%{ "MYR_SCANCODE_MEDIASELECT",      ?MYR_SCANCODE_MEDIASELECT },
		%{ "MYR_SCANCODE_WWW",              ?MYR_SCANCODE_WWW },
		%{ "MYR_SCANCODE_MAIL",             ?MYR_SCANCODE_MAIL },
		%{ "MYR_SCANCODE_CALCULATOR",       ?MYR_SCANCODE_CALCULATOR },
		%{ "MYR_SCANCODE_COMPUTER",         ?MYR_SCANCODE_COMPUTER },
		%{ "MYR_SCANCODE_AC_SEARCH",        ?MYR_SCANCODE_AC_SEARCH },
		%{ "MYR_SCANCODE_AC_HOME",          ?MYR_SCANCODE_AC_HOME },
		%{ "MYR_SCANCODE_AC_BACK",          ?MYR_SCANCODE_AC_BACK },
		%{ "MYR_SCANCODE_AC_FORWARD",       ?MYR_SCANCODE_AC_FORWARD },
		%{ "MYR_SCANCODE_AC_STOP",          ?MYR_SCANCODE_AC_STOP },
		%{ "MYR_SCANCODE_AC_REFRESH",       ?MYR_SCANCODE_AC_REFRESH },
		%{ "MYR_SCANCODE_AC_BOOKMARKS",     ?MYR_SCANCODE_AC_BOOKMARKS },
		%{ "MYR_SCANCODE_BRIGHTNESSDOWN",   ?MYR_SCANCODE_BRIGHTNESSDOWN },
		%{ "MYR_SCANCODE_BRIGHTNESSUP",     ?MYR_SCANCODE_BRIGHTNESSUP },
		%{ "MYR_SCANCODE_DISPLAYSWITCH",    ?MYR_SCANCODE_DISPLAYSWITCH },
		%{ "MYR_SCANCODE_KBDILLUMTOGGLE",   ?MYR_SCANCODE_KBDILLUMTOGGLE },
		%{ "MYR_SCANCODE_KBDILLUMDOWN",     ?MYR_SCANCODE_KBDILLUMDOWN },
		%{ "MYR_SCANCODE_KBDILLUMUP",       ?MYR_SCANCODE_KBDILLUMUP },
		%{ "MYR_SCANCODE_EJECT",            ?MYR_SCANCODE_EJECT },
		%{ "MYR_SCANCODE_SLEEP",            ?MYR_SCANCODE_SLEEP },
		%{ "MYR_SCANCODE_APP1",             ?MYR_SCANCODE_APP1 },
		%{ "MYR_SCANCODE_APP2",             ?MYR_SCANCODE_APP2 },
		%{ "MYR_SCANCODE_AUDIOREWIND",      ?MYR_SCANCODE_AUDIOREWIND },
		%{ "MYR_SCANCODE_AUDIOFASTFORWARD", ?MYR_SCANCODE_AUDIOFASTFORWARD }

	].



% @doc Lists all known keycodes: name of their define, then (decimal) value.
-spec list_keycode_pairs() -> { ustring(), keycode() }.
list_keycode_pairs() ->
	[
	  { "MYR_K_RETURN", ?MYR_K_RETURN },
	  { "MYR_K_ESCAPE", ?MYR_K_ESCAPE },
	  { "MYR_K_BACKSPACE", ?MYR_K_BACKSPACE },
	  { "MYR_K_TAB", ?MYR_K_TAB },
	  { "MYR_K_SPACE", ?MYR_K_SPACE },
	  { "MYR_K_EXCLAIM", ?MYR_K_EXCLAIM },
	  { "MYR_K_QUOTEDBL", ?MYR_K_QUOTEDBL },
	  { "MYR_K_HASH", ?MYR_K_HASH },
	  { "MYR_K_PERCENT", ?MYR_K_PERCENT },
	  { "MYR_K_DOLLAR", ?MYR_K_DOLLAR },
	  { "MYR_K_AMPERSAND", ?MYR_K_AMPERSAND },
	  { "MYR_K_QUOTE", ?MYR_K_QUOTE },
	  { "MYR_K_LEFTPAREN", ?MYR_K_LEFTPAREN },
	  { "MYR_K_RIGHTPAREN", ?MYR_K_RIGHTPAREN },
	  { "MYR_K_ASTERISK", ?MYR_K_ASTERISK },
	  { "MYR_K_PLUS", ?MYR_K_PLUS },
	  { "MYR_K_COMMA", ?MYR_K_COMMA },
	  { "MYR_K_MINUS", ?MYR_K_MINUS },
	  { "MYR_K_PERIOD", ?MYR_K_PERIOD },
	  { "MYR_K_SLASH", ?MYR_K_SLASH },
	  { "MYR_K_0", ?MYR_K_0 },
	  { "MYR_K_1", ?MYR_K_1 },
	  { "MYR_K_2", ?MYR_K_2 },
	  { "MYR_K_3", ?MYR_K_3 },
	  { "MYR_K_4", ?MYR_K_4 },
	  { "MYR_K_5", ?MYR_K_5 },
	  { "MYR_K_6", ?MYR_K_6 },
	  { "MYR_K_7", ?MYR_K_7 },
	  { "MYR_K_8", ?MYR_K_8 },
	  { "MYR_K_9", ?MYR_K_9 },
	  { "MYR_K_COLON", ?MYR_K_9 },
	  { "MYR_K_SEMICOLON", ?MYR_K_SEMICOLON },
	  { "MYR_K_LESS", ?MYR_K_LESS },
	  { "MYR_K_EQUALS", ?MYR_K_EQUALS },
	  { "MYR_K_GREATER", ?MYR_K_GREATER },
	  { "MYR_K_QUESTION", ?MYR_K_QUESTION },
	  { "MYR_K_AT", ?MYR_K_AT },
	  { "MYR_K_LEFTBRACKET", ?MYR_K_LEFTBRACKET },
	  { "MYR_K_BACKSLASH", ?MYR_K_BACKSLASH },
	  { "MYR_K_RIGHTBRACKET", ?MYR_K_RIGHTBRACKET },
	  { "MYR_K_CARET", ?MYR_K_CARET },
	  { "MYR_K_UNDERSCORE", ?MYR_K_UNDERSCORE },
	  { "MYR_K_BACKQUOTE", ?MYR_K_BACKQUOTE },
	  { "MYR_K_a", ?MYR_K_a },
	  { "MYR_K_b", ?MYR_K_b },
	  { "MYR_K_c", ?MYR_K_c },
	  { "MYR_K_d", ?MYR_K_d },
	  { "MYR_K_e", ?MYR_K_e },
	  { "MYR_K_f", ?MYR_K_f },
	  { "MYR_K_g", ?MYR_K_g },
	  { "MYR_K_h", ?MYR_K_h },
	  { "MYR_K_i", ?MYR_K_i },
	  { "MYR_K_j", ?MYR_K_j },
	  { "MYR_K_k", ?MYR_K_k },
	  { "MYR_K_l", ?MYR_K_l },
	  { "MYR_K_m", ?MYR_K_m },
	  { "MYR_K_n", ?MYR_K_n },
	  { "MYR_K_o", ?MYR_K_o },
	  { "MYR_K_p", ?MYR_K_p },
	  { "MYR_K_q", ?MYR_K_q },
	  { "MYR_K_r", ?MYR_K_r },
	  { "MYR_K_s", ?MYR_K_s },
	  { "MYR_K_t", ?MYR_K_t },
	  { "MYR_K_u", ?MYR_K_u },
	  { "MYR_K_v", ?MYR_K_v },
	  { "MYR_K_w", ?MYR_K_w },
	  { "MYR_K_x", ?MYR_K_x },
	  { "MYR_K_y", ?MYR_K_y },
	  { "MYR_K_z", ?MYR_K_z },
	  { "MYR_K_CAPSLOCK", ?MYR_K_CAPSLOCK },
	  { "MYR_K_F1", ?MYR_K_F1 },
	  { "MYR_K_F2", ?MYR_K_F2 },
	  { "MYR_K_F3", ?MYR_K_F3 },
	  { "MYR_K_F4", ?MYR_K_F4 },
	  { "MYR_K_F5", ?MYR_K_F5 },
	  { "MYR_K_F6", ?MYR_K_F6 },
	  { "MYR_K_F7", ?MYR_K_F7 },
	  { "MYR_K_F8", ?MYR_K_F8 },
	  { "MYR_K_F9", ?MYR_K_F9 },
	  { "MYR_K_F10", ?MYR_K_F10 },
	  { "MYR_K_F11", ?MYR_K_F11 },
	  { "MYR_K_F12", ?MYR_K_F12 },
	  { "MYR_K_PRINTSCREEN", ?MYR_K_PRINTSCREEN },
	  { "MYR_K_SCROLLLOCK", ?MYR_K_SCROLLLOCK },
	  { "MYR_K_PAUSE", ?MYR_K_PAUSE },
	  { "MYR_K_INSERT", ?MYR_K_INSERT },
	  { "MYR_K_HOME", ?MYR_K_HOME },
	  { "MYR_K_PAGEUP", ?MYR_K_PAGEUP },
	  { "MYR_K_DELETE", ?MYR_K_DELETE },
	  { "MYR_K_END", ?MYR_K_END },
	  { "MYR_K_PAGEDOWN", ?MYR_K_PAGEDOWN },
	  { "MYR_K_RIGHT", ?MYR_K_RIGHT },
	  { "MYR_K_LEFT", ?MYR_K_LEFT },
	  { "MYR_K_DOWN", ?MYR_K_DOWN },
	  { "MYR_K_UP", ?MYR_K_UP },
	  { "MYR_K_NUMLOCKCLEAR", ?MYR_K_NUMLOCKCLEAR },
	  { "MYR_K_KP_DIVIDE",    ?MYR_K_KP_DIVIDE },
	  { "MYR_K_KP_MULTIPLY",  ?MYR_K_KP_MULTIPLY },
	  { "MYR_K_KP_MINUS",     ?MYR_K_KP_MINUS },
	  { "MYR_K_KP_PLUS",      ?MYR_K_KP_PLUS },
	  { "MYR_K_KP_ENTER",     ?MYR_K_KP_ENTER },
	  { "MYR_K_KP_1", ?MYR_K_KP_1 },
	  { "MYR_K_KP_2", ?MYR_K_KP_2 },
	  { "MYR_K_KP_3", ?MYR_K_KP_3 },
	  { "MYR_K_KP_4", ?MYR_K_KP_4 },
	  { "MYR_K_KP_5", ?MYR_K_KP_5 },
	  { "MYR_K_KP_6", ?MYR_K_KP_6 },
	  { "MYR_K_KP_7", ?MYR_K_KP_7 },
	  { "MYR_K_KP_8", ?MYR_K_KP_8 },
	  { "MYR_K_KP_9", ?MYR_K_KP_9 },
	  { "MYR_K_KP_0", ?MYR_K_KP_0 },
	  { "MYR_K_KP_PERIOD", ?MYR_K_KP_PERIOD }

	  % No MYR_SCANCODE_APPLICATION set:
	  %{ "MYR_K_APPLICATION", ?MYR_K_APPLICATION },

	  %{ "MYR_K_POWER", ?MYR_K_POWER },
	  %{ "MYR_K_KP_EQUALS", ?MYR_K_KP_EQUALS },
	  %{ "MYR_K_F13", ?MYR_K_F13 },
	  %{ "MYR_K_F14", ?MYR_K_F14 },
	  %{ "MYR_K_F15", ?MYR_K_F15 },
	  %{ "MYR_K_F16", ?MYR_K_F16 },
	  %{ "MYR_K_F17", ?MYR_K_F17 },
	  %{ "MYR_K_F18", ?MYR_K_F18 },
	  %{ "MYR_K_F19", ?MYR_K_F19 },
	  %{ "MYR_K_F20", ?MYR_K_F20 },
	  %{ "MYR_K_F21", ?MYR_K_F21 },
	  %{ "MYR_K_F22", ?MYR_K_F22 },
	  %{ "MYR_K_F23", ?MYR_K_F23 },
	  %{ "MYR_K_F24", ?MYR_K_F24 },
	  %{ "MYR_K_EXECUTE", ?MYR_K_EXECUTE },
	  %{ "MYR_K_HELP", ?MYR_K_HELP },
	  %{ "MYR_K_MENU", ?MYR_K_MENU },
	  %{ "MYR_K_SELECT", ?MYR_K_SELECT },
	  %{ "MYR_K_STOP", ?MYR_K_STOP },
	  %{ "MYR_K_AGAIN", ?MYR_K_AGAIN },
	  %{ "MYR_K_UNDO", ?MYR_K_UNDO },
	  %{ "MYR_K_CUT", ?MYR_K_CUT },
	  %{ "MYR_K_COPY", ?MYR_K_COPY },
	  %{ "MYR_K_PASTE", ?MYR_K_PASTE },
	  %{ "MYR_K_FIND", ?MYR_K_FIND },
	  %{ "MYR_K_MUTE", ?MYR_K_MUTE },
	  %{ "MYR_K_VOLUMEUP", ?MYR_K_VOLUMEUP },
	  %{ "MYR_K_VOLUMEDOWN", ?MYR_K_VOLUMEDOWN },
	  %{ "MYR_K_KP_COMMA", ?MYR_K_KP_COMMA },
	  %{ "MYR_K_KP_EQUALSAS400", ?MYR_K_KP_EQUALSAS400 },
	  %{ "MYR_K_ALTERASE", ?MYR_K_ALTERASE },
	  %{ "MYR_K_SYSREQ", ?MYR_K_SYSREQ },
	  %{ "MYR_K_CANCEL", ?MYR_K_CANCEL },
	  %{ "MYR_K_CLEAR", ?MYR_K_CLEAR },
	  %{ "MYR_K_PRIOR", ?MYR_K_PRIOR },
	  %{ "MYR_K_RETURN2", ?MYR_K_RETURN2 },
	  %{ "MYR_K_SEPARATOR", ?MYR_K_SEPARATOR },
	  %{ "MYR_K_OUT", ?MYR_K_OUT },
	  %{ "MYR_K_OPER", ?MYR_K_OPER },
	  %{ "MYR_K_CLEARAGAIN", ?MYR_K_CLEARAGAIN },
	  %{ "MYR_K_CRSEL", ?MYR_K_CRSEL },
	  %{ "MYR_K_EXSEL", ?MYR_K_EXSEL },
	  %{ "MYR_K_KP_00", ?MYR_K_KP_00 },
	  %{ "MYR_K_KP_000", ?MYR_K_KP_000 },
	  %{ "MYR_K_THOUSANDSSEPARATOR", ?MYR_K_THOUSANDSSEPARATOR },
	  %{ "MYR_K_DECIMALSEPARATOR", ?MYR_K_DECIMALSEPARATOR },
	  %{ "MYR_K_CURRENCYUNIT", ?MYR_K_CURRENCYUNIT },
	  %{ "MYR_K_CURRENCYSUBUNIT", ?MYR_K_CURRENCYSUBUNIT },
	  %{ "MYR_K_KP_LEFTPAREN", ?MYR_K_KP_LEFTPAREN },
	  %{ "MYR_K_KP_RIGHTPAREN", ?MYR_K_KP_RIGHTPAREN },
	  %{ "MYR_K_KP_LEFTBRACE", ?MYR_K_KP_LEFTBRACE },
	  %{ "MYR_K_KP_RIGHTBRACE", ?MYR_K_KP_RIGHTBRACE },
	  %{ "MYR_K_KP_TAB", ?MYR_K_KP_TAB },
	  %{ "MYR_K_KP_BACKSPACE", ?MYR_K_KP_BACKSPACE },
	  %{ "MYR_K_KP_A", ?MYR_K_KP_A },
	  %{ "MYR_K_KP_B", ?MYR_K_KP_B },
	  %{ "MYR_K_KP_C", ?MYR_K_KP_C },
	  %{ "MYR_K_KP_D", ?MYR_K_KP_D },
	  %{ "MYR_K_KP_E", ?MYR_K_KP_E },
	  %{ "MYR_K_KP_F", ?MYR_K_KP_F },
	  %{ "MYR_K_KP_XOR", ?MYR_K_KP_XOR },
	  %{ "MYR_K_KP_POWER", ?MYR_K_KP_POWER },
	  %{ "MYR_K_KP_PERCENT", ?MYR_K_KP_PERCENT },
	  %{ "MYR_K_KP_LESS", ?MYR_K_KP_LESS },
	  %{ "MYR_K_KP_GREATER", ?MYR_K_KP_GREATER },
	  %{ "MYR_K_KP_AMPERSAND", ?MYR_K_KP_AMPERSAND },
	  %{ "MYR_K_KP_DBLAMPERSAND", ?MYR_K_KP_DBLAMPERSAND },
	  %{ "MYR_K_KP_VERTICALBAR", ?MYR_K_KP_VERTICALBAR },
	  %{ "MYR_K_KP_DBLVERTICALBAR", ?MYR_K_KP_DBLVERTICALBAR },
	  %{ "MYR_K_KP_COLON", ?MYR_K_KP_COLON },
	  %{ "MYR_K_KP_HASH", ?MYR_K_KP_HASH },
	  %{ "MYR_K_KP_SPACE", ?MYR_K_KP_SPACE },
	  %{ "MYR_K_KP_AT", ?MYR_K_KP_AT },
	  %{ "MYR_K_KP_EXCLAM", ?MYR_K_KP_EXCLAM },
	  %{ "MYR_K_KP_MEMSTORE", ?MYR_K_KP_MEMSTORE },
	  %{ "MYR_K_KP_MEMRECALL", ?MYR_K_KP_MEMRECALL },
	  %{ "MYR_K_KP_MEMCLEAR", ?MYR_K_KP_MEMCLEAR },
	  %{ "MYR_K_KP_MEMADD", ?MYR_K_KP_MEMADD },
	  %{ "MYR_K_KP_MEMSUBTRACT", ?MYR_K_KP_MEMSUBTRACT },
	  %{ "MYR_K_KP_MEMMULTIPLY", ?MYR_K_KP_MEMMULTIPLY },
	  %{ "MYR_K_KP_MEMDIVIDE", ?MYR_K_KP_MEMDIVIDE },
	  %{ "MYR_K_KP_PLUSMINUS", ?MYR_K_KP_PLUSMINUS },
	  %{ "MYR_K_KP_CLEAR", ?MYR_K_KP_CLEAR },
	  %{ "MYR_K_KP_CLEARENTRY", ?MYR_K_KP_CLEARENTRY },
	  %{ "MYR_K_KP_BINARY", ?MYR_K_KP_BINARY },
	  %{ "MYR_K_KP_OCTAL", ?MYR_K_KP_OCTAL },
	  %{ "MYR_K_KP_DECIMAL", ?MYR_K_KP_DECIMAL },
	  %{ "MYR_K_KP_HEXADECIMAL", ?MYR_K_KP_HEXADECIMAL },
	  %{ "MYR_K_LCTRL", ?MYR_K_LCTRL },
	  %{ "MYR_K_LSHIFT", ?MYR_K_LSHIFT },
	  %{ "MYR_K_LALT", ?MYR_K_LALT },

% Preferred to LGUI:
	  %{ "MYR_K_LSUPER", ?MYR_K_LSUPER },

	  %{ "MYR_K_RCTRL", ?MYR_K_RCTRL },
	  %{ "MYR_K_RSHIFT", ?MYR_K_RSHIFT },

% Any shift:
	  %{ "MYR_K_ANY_SHIFT", ?MYR_K_ANY_SHIFT },

	  %{ "MYR_K_RALT", ?MYR_K_RALT },



% Preferred to RGUI:
	  %{ "MYR_K_RSUPER", ?MYR_K_RSUPER },

% "Alt Gr" key:
	  %{ "MYR_K_MODE", ?MYR_K_MODE },

	  %{ "MYR_K_AUDIONEXT", ?MYR_K_AUDIONEXT },
	  %{ "MYR_K_AUDIOPREV", ?MYR_K_AUDIOPREV },
	  %{ "MYR_K_AUDIOSTOP", ?MYR_K_AUDIOSTOP },
	  %{ "MYR_K_AUDIOPLAY", ?MYR_K_AUDIOPLAY },
	  %{ "MYR_K_AUDIOMUTE", ?MYR_K_AUDIOMUTE },
	  %{ "MYR_K_MEDIASELECT", ?MYR_K_MEDIASELECT },
	  %{ "MYR_K_WWW", ?MYR_K_WWW },
	  %{ "MYR_K_MAIL", ?MYR_K_MAIL },
	  %{ "MYR_K_CALCULATOR", ?MYR_K_CALCULATOR },
	  %{ "MYR_K_COMPUTER", ?MYR_K_COMPUTER },
	  %{ "MYR_K_AC_SEARCH", ?MYR_K_AC_SEARCH },
	  %{ "MYR_K_AC_HOME", ?MYR_K_AC_HOME },
	  %{ "MYR_K_AC_BACK", ?MYR_K_AC_BACK },
	  %{ "MYR_K_AC_FORWARD", ?MYR_K_AC_FORWARD },
	  %{ "MYR_K_AC_STOP", ?MYR_K_AC_STOP },
	  %{ "MYR_K_AC_REFRESH", ?MYR_K_AC_REFRESH },
	  %{ "MYR_K_AC_BOOKMARKS", ?MYR_K_AC_BOOKMARKS },
	  %{ "MYR_K_BRIGHTNESSDOWN", ?MYR_K_BRIGHTNESSDOWN },
	  %{ "MYR_K_BRIGHTNESSUP", ?MYR_K_BRIGHTNESSUP },
	  %{ "MYR_K_DISPLAYSWITCH", ?MYR_K_DISPLAYSWITCH },
	  %{ "MYR_K_KBDILLUMTOGGLE", ?MYR_K_KBDILLUMTOGGLE },
	  %{ "MYR_K_KBDILLUMDOWN", ?MYR_K_KBDILLUMDOWN },
	  %{ "MYR_K_KBDILLUMUP", ?MYR_K_KBDILLUMUP },
	  %{ "MYR_K_EJECT", ?MYR_K_EJECT },
	  %{ "MYR_K_SLEEP", ?MYR_K_SLEEP },
	  %{ "MYR_K_APP1", ?MYR_K_APP1 },
	  %{ "MYR_K_APP2", ?MYR_K_APP2 },
	  %{ "MYR_K_AUDIOREWIND", ?MYR_K_AUDIOREWIND },
	  %{ "MYR_K_AUDIOFASTFORWARD", ?MYR_K_AUDIOFASTFORWARD }

].


% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	trace_utils:info( "Just listing alphabetically all known "
		"scancodes and keycodes, typically so that they can be "
		"grep'ed for checking them (values are decimal, not hexadecimal)." ),

	ScancodePairs = list_scancode_pairs(),

	trace_utils:info_fmt( "~B scancodes: ~ts",
		[ length( ScancodePairs ),
		  text_utils:strings_to_string( lists:sort( [
			text_utils:format( "  ~ts: ~B", [ ScanDesc, Scancode ] )
				|| { ScanDesc, Scancode } <- ScancodePairs ] ) ) ] ),


	KeycodePairs = list_keycode_pairs(),

	trace_utils:info_fmt( "~B keycodes: ~ts",
		[ length( KeycodePairs ),
		  text_utils:strings_to_string( lists:sort( [
			text_utils:format( "  ~ts: ~B", [ KeyDesc, Keycode ] )
				|| { KeyDesc, Keycode } <- list_keycode_pairs() ] ) ) ] ),

	test_facilities:stop().
