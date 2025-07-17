% Copyright (C) 2010-2025 Olivier Boudeville
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

-module(gui_color).

-moduledoc """
Gathering of various facilities for **color management**.
""".


% Colors.
%
% Colors are designated here generally by {R,G,B} triplets, or as one of the
% predefined (atom-based) names, like red, green, etc.
%
% For example {0,0,0} is black and {255,255,255} is white.


-include("gui_color.hrl").


% For related defines:
-include("gui_base.hrl").


% For wx color defines:
-include_lib("wx/include/wx.hrl").



-doc """
A color, as designated by an atom (e.g. 'aliceblue'); possibly a logical
color. See get_colors/0 for a list thereof.
""".
-type color_by_name() :: atom().



-doc "A logical color, like 'window_frame_color'.".
-type logical_color() :: color_by_name().


-doc "An integer coordinate, in [0;255].".
-type decimal_coordinate() :: byte().



-doc """
RGB (integer coordinates, in [0;255]) color; no alpha coordinate here.

Corresponds also to the wx_colour/0 triplets.
""".
-type color_by_decimal() ::
		{ Red :: decimal_coordinate(), Green :: decimal_coordinate(),
		  Blue :: decimal_coordinate() }.


-doc """
A RGB color that is encoded based on 6 hexadecimal digits in a string, with a
"#" prefix (hence is not a text_utils:hexastring/0).

For example "#3ab001" corresponds to lightgreen.

Possibly used for HTML content, by gnuplot, etc.
""".
-type rgb_hexastring() :: ustring().



-doc """
A RGBA color that is encoded based on 8 hexadecimal digits in a string, with a
"#" prefix (hence is not a text_utils:hexastring/0).

For example "#3ab001ee" corresponds to a mostly solid lightgreen.
""".
-type rgba_hexastring() :: ustring().



-doc """
RGBA (integer coordinates, in [0;255]) color.

Corresponds also to wx_colour4/0.
""".
-type color_by_decimal_with_alpha() ::
		{ Red :: decimal_coordinate(), Green :: decimal_coordinate(),
		  Blue :: decimal_coordinate(), Alpha :: decimal_coordinate() }.



-doc """
Any RGB or RGBA color with integer coordinates, in [0;255].

Corresponds also to wx_colour/0.
""".
-type any_color_by_decimal() ::
		color_by_decimal() | color_by_decimal_with_alpha().



-doc "Any kind of RGB color.".
-type color() :: color_by_name() | color_by_decimal() | rgb_hexastring().


-doc "Any kind of RGB or RGBA color.".
-type any_color() :: color_by_name()
				   | color_by_decimal()
				   | color_by_decimal_with_alpha()
				   | rgb_hexastring().


-doc """
A color depth, as a positive number of bits.

For example the color depth of a monochrome display is 1.
""".
-type color_depth() :: system_utils:bit_size().



-doc """
Color coordinate, floating-point in [0.0, 1.0].

For example as used and clamped by OpenGL.
""".
-type color_coordinate() :: float().



-doc """
An alpha-transparency color coordinate.

From 0.0 (full transparent) to 1.0 (solid); same as OpenGL conventions.
""".
-type alpha_coordinate() :: color_coordinate().



-doc """
A floating-point RGB color (whose coordinates are typically in [0.0,1.0]).

The three components shall be encoded with the sRGB transfer function.

For example useful with OpenGL. Used to be a triplet (a tuple), yet, for example
for uniform variable, a (3D) vector is expected instead.
""".
-type render_rgb_color() ::
		{ Red :: color_coordinate(), Green :: color_coordinate(),
		  Blue :: color_coordinate() }.
		%[ color_coordinate() ]. % (a list of 3 elements)



-doc """
A floating-point RGBA color.

The first three components (RGB) shall be encoded with the sRGB transfer
function.

For example useful with OpenGL.
""".
-type render_rgba_color() :: { Red ::color_coordinate(),
		Green :: color_coordinate(), Blue ::color_coordinate(),
		Alpha :: alpha_coordinate() }.



-doc """
A floating-point RGB or RGBA color.

The first three components (RGB) shall be encoded with the sRGB transfer
function.

For example useful with OpenGL.
""".
-type render_color() :: render_rgb_color() | render_rgba_color().



-doc """
A buffer of pixel colors coded as a sequence of RGB or RGBA binary elements
(e.g. RGBRGBRGB..., or RGBARGBARGBA...), from the top-left pixel to bottom-right
one, row per row.

Useful for direct image manipulation.
""".
-type color_buffer() :: rgb_color_buffer() | rgba_color_buffer().



-doc """
A buffer of pixel colors coded as a sequence of RGB binary elements (RGBRGBRGB),
from the top-left pixel to bottom-right one, row per row.

Useful for direct image manipulation.
""".
-type rgb_color_buffer() :: buffer().



-doc """
A buffer of pixel colors coded as a sequence of RGBA binary elements
(RGBARGBARGBA), from the top-left pixel to bottom-right one, row per row.

Useful for direct image manipulation.
""".
-type rgba_color_buffer() :: buffer().



-doc """
A buffer of (only) pixel alpha coordinates, from the top-left pixel to
bottom-right one, row per row.

Useful for direct image manipulation.
""".
-type alpha_buffer() :: buffer().


-doc "A specification of a pixel format.".
-type pixel_format() :: 'rgb' | 'rgba'.



-doc """
A wx object representing information regarding a color.

For example {wx_ref,92,wxColourData,[]}.
""".
-opaque color_data() :: wxColourData:wxColourData().



-export_type([ color_by_name/0, logical_color/0,

			   color_by_decimal/0, color_by_decimal_with_alpha/0,
			   any_color_by_decimal/0,
			   rgb_hexastring/0, rgba_hexastring/0,

			   color/0, any_color/0,

			   color_depth/0,
			   color_coordinate/0, alpha_coordinate/0,
			   render_rgb_color/0, render_rgba_color/0, render_color/0,

			   color_buffer/0, rgb_color_buffer/0, rgba_color_buffer/0,
			   alpha_buffer/0, pixel_format/0,
			   color_data/0 ]).


% Color definition related operations.
-export([ get_colors/0, get_color/1, get_any_color/1,
		  get_render_rgb_color/1, get_render_rgba_color/1,
		  get_logical_colors/0, get_logical_color/1,
		  get_color_for_gnuplot/1, get_random_colors/1 ]).


% Descriptions:
-export([ to_string/1, color_by_decimal_to_string/1,
		  color_by_decimal_with_alpha_to_string/1 ]).


% Color conversions.
-export([ add_alpha_opaque/1, add_alpha/2, decimal_to_render/1 ]).


% Other operations:
-export([ check_color_by_decimal/1,
		  get_pixel_size/1, pixel_format_to_string/1 ]).



% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type byte_size() :: system_utils:byte_size().

-type buffer() :: bin_utils:buffer().



% Here colors are defined as a triplet of color components: {R,G,B}, see the
% color_by_decimal() type.
%
% For example {0,0,0} is black and {255,255,255} is white.



% Section for color definition.


-doc """
Returns a list of known {color_name, ColorDefinition} associations.
""".
-spec get_colors() -> [ { color_by_name(), any_color_by_decimal() } ].
get_colors() ->
	[

	  % Initially, first, "functional" (RGBA) colors were listed (from wx.hrl),
	  % yet this must be avoided, as the next call would require wx to be
	  % initialised and its environment to be available, which is not the case
	  % generally, like in:
	  %
	  %{ window_frame_color,
	  %  wxSystemSettings:getColour( ?wxSYS_COLOUR_WINDOWFRAME ) }, ...
	  %
	  % See now get_logical_color/1.

	  % No less than 141 RGB color definitions follow, based on
	  %www.uni-hamburg.de/Wiss/FB/15/Sustainability/schneider/gnuplot/colors.htm

	  { aliceblue,            { 240, 248, 255 } },
	  { antiquewhite,         { 250, 235, 215 } },
	  { aqua,                 {   0, 255, 255 } },
	  { aquamarine,           { 127, 255, 212 } },
	  { azure,                { 240, 255, 255 } },
	  { beige,                { 245, 245, 220 } },
	  { bisque,               { 255, 228, 196 } },
	  { black,                {   0,   0,   0 } },
	  { blanchedalmond,       { 255, 235, 205 } },
	  { blue,                 {   0,   0, 255 } },
	  { blueviolet,           { 138,  43, 226 } },
	  { brown,                { 165,  42,  42 } },
	  { burlywood,            { 222, 184, 135 } },
	  { cadetblue,            {  95, 158, 160 } },
	  { chartreuse,           { 127, 255,   0 } },
	  { chocolate,            { 210, 105,  30 } },
	  { coral,                { 255, 127,  80 } },
	  { cornflowerblue,       { 100, 149, 237 } },
	  { cornsilk,             { 255, 248, 220 } },
	  { crimson,              { 220,  20,  60 } },
	  { cyan,                 {   0, 255, 255 } },
	  { darkblue,             {   0,   0, 139 } },
	  { darkcyan,             {   0, 139, 139 } },
	  { darkgoldenrod,        { 184, 134,  11 } },
	  { darkgray,             { 169, 169, 169 } },
	  { darkgreen,            {   0, 100,   0 } },
	  { darkkhaki,            { 189, 183, 107 } },
	  { darkmagenta,          { 139,   0, 139 } },
	  { darkolivegreen,       {  85, 107, 47  } },
	  { darkorange,           { 255, 140,  0  } },
	  { darkorchid,           { 153,  50, 204 } },
	  { darkred,              { 139,   0,   0 } },
	  { darksalmon,           { 233, 150, 122 } },
	  { darkseagreen,         { 143, 188, 143 } },
	  { darkslateblue,        {  72,  61, 139 } },
	  { darkslategray,        {  47,  79,  79 } },
	  { darkturquoise,        {   0, 206, 209 } },
	  { darkviolet,           { 148,   0, 211 } },
	  { deeppink,             { 255,  20, 147 } },
	  { deepskyblue,          {   0, 191, 255 } },
	  { dimgray,              { 105, 105, 105 } },
	  { dodgerblue,           {  30, 144, 255 } },
	  { firebrick,            { 178,  34, 34  } },
	  { floralwhite,          { 255, 250, 240 } },
	  { forestgreen,          {  34, 139,  34 } },
	  { fuchsia,              { 255,   0, 255 } },
	  { gainsboro,            { 220, 220, 220 } },
	  { ghostwhite,           { 248, 248, 255 } },
	  { gold,                 { 255, 215,   0 } },
	  { goldenrod,            { 218, 165,  32 } },
	  { gray,                 { 127, 127, 127 } },
	  { green,                {   0, 128,   0 } },
	  { greenyellow,          { 173, 255,  47 } },
	  { honeydew,             { 240, 255, 240 } },
	  { hotpink,              { 255, 105, 180 } },
	  { indianred,            { 205,  92,  92 } },
	  { indigo,               {  75,   0, 130 } },
	  { ivory,                { 255, 255, 240 } },
	  { khaki,                { 240, 230, 140 } },
	  { lavender,             { 230, 230, 250 } },
	  { lavenderblush,        { 255, 240, 245 } },
	  { lawngreen,            { 124, 252,   0 } },
	  { lemonchiffon,         { 255, 250, 205 } },
	  { lightblue,            { 173, 216, 230 } },
	  { lightcoral,           { 240, 128, 128 } },
	  { lightcyan,            { 224, 255, 255 } },
	  { lightgoldenrodyellow, { 250, 250, 210 } },
	  { lightgreen,           { 144, 238, 144 } },
	  { lightgrey,            { 211, 211, 211 } },
	  { lightpink,            { 255, 182, 193 } },
	  { lightsalmon,          { 255, 160, 122 } },
	  { lightseagreen,        {  32, 178, 170 } },
	  { lightskyblue,         { 135, 206, 250 } },
	  { lightslategray,       { 119, 136, 153 } },
	  { lightsteelblue,       { 176, 196, 222 } },
	  { lightyellow,          { 255, 255, 224 } },
	  { lime,                 {   0, 255,   0 } },
	  { limegreen,            {  50, 205,  50 } },
	  { linen,                { 250, 240, 230 } },
	  { magenta,              { 255,   0, 255 } },
	  { maroon,               { 128,   0,   0 } },
	  { mediumaquamarine,     { 102, 205, 170 } },
	  { mediumblue,           {   0,   0, 205 } },
	  { mediumorchid,         { 186,  85, 211 } },
	  { mediumpurple,         { 147, 112, 219 } },
	  { mediumseagreen,       {  60, 179, 113 } },
	  { mediumslateblue,      { 123, 104, 238 } },
	  { mediumspringgreen,    {   0, 250, 154 } },
	  { mediumturquoise,      {  72, 209, 204 } },
	  { mediumvioletred,      { 199,  21, 133 } },
	  { midnightblue,         {  25,  25, 112 } },
	  { mintcream,            { 245, 255, 250 } },
	  { mistyrose,            { 255, 228, 225 } },
	  { moccasin,             { 255, 228, 181 } },
	  { navajowhite,          { 255, 222, 173 } },
	  { navy,                 {   0,   0, 128 } },
	  { navyblue,             { 159, 175, 223 } },
	  { oldlace,              { 253, 245, 230 } },
	  { olive,                { 128, 128,   0 } },
	  { olivedrab,            { 107, 142,  35 } },
	  { orange,               { 255, 165,   0 } },
	  { orangered,            { 255,  69,   0 } },
	  { orchid,               { 218, 112, 214 } },
	  { palegoldenrod,        { 238, 232, 170 } },
	  { palegreen,            { 152, 251, 152 } },
	  { paleturquoise,        { 175, 238, 238 } },
	  { palevioletred,        { 219, 112, 147 } },
	  { papayawhip,           { 255, 239, 213 } },
	  { peachpuff,            { 255, 218, 185 } },
	  { peru,                 { 205, 133,  63 } },
	  { pink,                 { 255, 192, 203 } },
	  { plum,                 { 221, 160, 221 } },
	  { powderblue,           { 176, 224, 230 } },
	  { purple,               { 128,   0, 128 } },
	  { red,                  { 255,   0,   0 } },
	  { rosybrown,            { 188, 143, 143 } },
	  { royalblue,            {  65, 105, 225 } },
	  { saddlebrown,          { 139,  69,  19 } },
	  { salmon,               { 250, 128, 114 } },
	  { sandybrown,           { 244, 164,  96 } },
	  { seagreen,             { 46,  139,  87 } },
	  { seashell,             { 255, 245, 238 } },
	  { sienna,               { 160,  82,  45 } },
	  { silver,               { 192, 192, 192 } },
	  { skyblue,              { 135, 206, 235 } },
	  { slateblue,            { 106,  90, 205 } },
	  { slategray,            { 112, 128, 144 } },
	  { snow,                 { 255, 250, 250 } },
	  { springgreen,          {   0, 255, 127 } },
	  { steelblue,            {  70, 130, 180 } },
	  { tan,                  { 210, 180, 140 } },
	  { teal,                 {   0, 128, 128 } },
	  { thistle,              { 216, 191, 216 } },
	  { tomato,               { 255,  99,  71 } },
	  { turquoise,            {  64, 224, 208 } },
	  { violet,               { 238, 130, 238 } },
	  { wheat,                { 245, 222, 179 } },
	  { white,                { 255, 255, 255 } },
	  { whitesmoke,           { 245, 245, 245 } },
	  { yellow,               { 255, 255,   0 } },
	  { yellowgreen,          { 139, 205,  50 } } ].



-doc """
Returns the RGB definition of the color specified by name (atom) or directly as
a triplet of color components.

No 'undefined' color (meaning transparent) accepted.
""".
-spec get_color( color() ) -> color_by_decimal().
get_color( Color={ _R, _G, _B } ) ->

	% Optimised for this most frequent form (first pattern); coordinates are
	% supposed to be integers:

	Color;

% Covers the logical colors as well:
get_color( _LogicalColor=window_frame_color ) ->
	wxSystemSettings:getColour( ?wxSYS_COLOUR_WINDOWFRAME );

get_color( ColorName ) when is_atom( ColorName ) ->
	case lists:member( ColorName, get_logical_colors() ) of

		true ->
			get_logical_color( ColorName );

		false ->
			case proplists:get_value( ColorName, get_colors() ) of

				undefined ->
					throw( { unknown_color, ColorName } );

				Color ->
					Color

			end

	end;

% For example "#0b5474":
get_color( _RGBHexastr=[ $#, R1, R2, G1, G2, B1, B2 ] ) ->
	Red   = text_utils:hexastring_to_integer( [ R1, R2 ] ),
	Green = text_utils:hexastring_to_integer( [ G1, G2 ] ),
	Blue  = text_utils:hexastring_to_integer( [ B1, B2 ] ),
	{ Red, Green, Blue }.




-doc """
Returns the RGB or RGBA definition of the color specified by name (atom), or
directly as a triplet or quadruplet of color components.

No 'undefined' color (meaning transparent) accepted.
""".
-spec get_any_color( any_color() ) -> any_color_by_decimal().
get_any_color( Color={ _R, _G, _B, _A } ) ->
	% Only RGBA case; coordinates are supposed to be integers:
	Color;

% Then a RGB case:
get_any_color( Color ) ->
	get_color( Color ).



-doc "Returns the floating-point RGB definition of the specified color.".
-spec get_render_rgb_color( color() ) -> render_rgb_color().
get_render_rgb_color( Color ) ->
	RGBCol = get_color( Color ),
	decimal_to_render( RGBCol ).


-doc """
Returns the floating-point RGBA definition, considering it fully opaque, of the
specified color.
""".
-spec get_render_rgba_color( render_rgba_color() ) -> render_rgba_color().
get_render_rgba_color( Color ) ->
	RGBACol = add_alpha_opaque( get_color( Color ) ),
	decimal_to_render( RGBACol ).



-doc "Returns the known logical colors.".
-spec get_logical_colors() -> [ logical_color() ].
get_logical_colors() ->
	[ window_frame_color ].



-doc """
Returns the RGB definition of the specified logical color.

Note that the underlying GUI backend shall be initialised and usable by this
process first.
""".
-spec get_logical_color( logical_color() ) -> color_by_decimal_with_alpha().
% Requires a process-level wx environment:
get_logical_color( _LogicalColor=window_frame_color ) ->
	wxSystemSettings:getColour( ?wxSYS_COLOUR_WINDOWFRAME );

get_logical_color( Other ) ->
	throw( { unknown_logical_color, Other } ).



-doc """
Returns a stringified representation for gnuplot of the specified color.
""".
-spec get_color_for_gnuplot( color() ) -> color().
get_color_for_gnuplot( Color={ _R, _G, _B } ) ->
	"#" ++ color_by_decimal_to_string( Color );

get_color_for_gnuplot( ColorName ) ->
	text_utils:format( "~ts", [ ColorName ] ).



-doc """
Returns a random list of the specified number of different colors.
""".
-spec get_random_colors( count() ) -> [ color_by_decimal() ].
get_random_colors( ColorCount ) ->

	% {R,G,B} values could have been drawn instead.

	AllColors = get_colors(),

	% Only keep RBG values, not the atom-based name:
	[ RGB || { _Name, RGB }
				<- list_utils:draw_elements_from( AllColors, ColorCount ) ].



-doc """
Returns a textual description of the specified color.
""".
-spec to_string( color() ) -> ustring().
to_string( ColorByName ) when is_atom( ColorByName ) ->
	text_utils:atom_to_string( ColorByName );

% RGB, not RGBA:
to_string( ColorByDecimal ) when is_tuple( ColorByDecimal ) ->
	color_by_decimal_to_string( ColorByDecimal );

to_string( RGBHexaStr ) ->
	RGBHexaStr.



-doc """
Returns a description of the specified RGB color like used for HTML ("#a1710f"
for example).
""".
-spec color_by_decimal_to_string( color_by_decimal() ) -> rgb_hexastring().
color_by_decimal_to_string( { Red, Green, Blue } ) ->

	Strs = %[ RedStr, GreenStr, BlueStr ]
		   [ text_utils:pad_string_right(
				text_utils:integer_to_hexastring( C ), _Width=2, _PadChar=$0 )
					|| C <- [ Red, Green, Blue ] ] ,

	text_utils:format("#~ts~ts~ts", Strs ).



-doc """
Returns a description of the specified RGBA color, a bit like used for HTML
("#a1710f12" for example).
""".
-spec color_by_decimal_with_alpha_to_string( color_by_decimal_with_alpha() ) ->
												rgba_hexastring().
color_by_decimal_with_alpha_to_string( { Red, Green, Blue, Alpha } ) ->

	Strs = %[ RedStr, GreenStr, BlueStr, AlphaStr ]
		   [ text_utils:pad_string_right(
				text_utils:integer_to_hexastring( C ), _Width=2, _PadChar=$0 )
					|| C <- [ Red, Green, Blue, Alpha ] ] ,

	text_utils:format("#~ts~ts~ts~ts", Strs ).



% Section for color conversion.


-doc """
Returns the floating-point RGB(A) color(s) corresponding to the specified
integer-based one(s).
""".
-spec decimal_to_render( color_by_decimal() ) -> render_rgb_color();
					   ( color_by_decimal_with_alpha() ) -> render_rgba_color();
					   ( [ color_by_decimal() ] ) -> [ render_rgb_color() ];
					   ( [ color_by_decimal_with_alpha() ] ) ->
											[ render_rgba_color() ].
decimal_to_render( _T={ Red, Green, Blue } ) ->
	%trace_utils:debug_fmt( "Color: ~w.", [ T ] ),
	Norm = 255,
	%[ Red/Norm, Green/Norm, Blue/Norm ];
	{ Red/Norm, Green/Norm, Blue/Norm };

decimal_to_render( { Red, Green, Blue, Alpha } ) ->
	Norm = 255,
	%[ Red/Norm, Green/Norm, Blue/Norm, Alpha/Norm ];
	{ Red/Norm, Green/Norm, Blue/Norm, Alpha/Norm };

decimal_to_render( Colors ) when is_list( Colors ) ->
	[ decimal_to_render( C ) || C <- Colors ].



-doc """
Adds an alpha, fully opaque coordinate to the specified RGB render color.
""".
-spec add_alpha_opaque( render_rgb_color() ) -> render_rgba_color().
add_alpha_opaque( { R, G, B } ) ->
	{ R, G, B, _Alpha=1.0 }.


-doc "Adds the specified alpha coordinate to the specified RGB render color.".
-spec add_alpha( decimal_coordinate(), render_rgb_color() ) ->
										render_rgba_color().
add_alpha( Alpha, { R, G, B } ) ->
	{ R, G, B, Alpha }.



-doc """
Checks that the specified term is a color_by_decimal() indeed, and returns it.
""".
-spec check_color_by_decimal( term() ) -> color_by_decimal().
check_color_by_decimal( { R, G, B } ) ->
	{ type_utils:check_byte( R ), type_utils:check_byte( G ),
	  type_utils:check_byte( B ) };

check_color_by_decimal( Other ) ->
	throw( { not_color_by_decimal, Other } ).



% Pixel formats.


-doc """
Returns the number of bytes used by each pixel of the specified format.
""".
-spec get_pixel_size( pixel_format() ) -> byte_size().
get_pixel_size( _PixelFormat=rgb ) ->
	3;

get_pixel_size( _PixelFormat=rgba ) ->
	4.



-doc "Returns a textual description of the specified pixel format.".
-spec pixel_format_to_string( pixel_format() ) -> ustring().
pixel_format_to_string( _PixelFormat=rgb ) ->
	"RGB";

pixel_format_to_string( _PixelFormat=rgba ) ->
	"RGBA".
