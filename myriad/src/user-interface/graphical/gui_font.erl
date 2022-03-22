% Copyright (C) 2021-2022 Olivier Boudeville
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
% Creation date: Friday, November 19, 2021.


% @doc Gathering of various facilities for <b>font management</b>.
-module(gui_font).


-type font() :: wxFont:wxFont().
% Designates a font object.


-type font_size() :: gui:dimensions().
% The dimensions, in pixels, of characters drawn from a font.


-type point_size() :: gui:length().
% A font size, in pixels.


-type font_family() :: 'default_font_family'.
% A font family.


-type font_style() :: 'normal'.
% A font style.


-type font_weight() :: 'bold'.
% A font style.

-type text_encoding() :: wx_text_encoding().
% Currently the same as wx.


-type font_option() :: { 'underline', boolean() }
					 | { 'face_name', any_string() }
					 | { 'encoding', text_encoding() }.
% A font option.


-export_type([ font/0, font_size/0, point_size/0, font_family/0, font_style/0,
			   font_weight/0, text_encoding/0, font_option/0 ]).


% Font-related operations.
-export([ create/4, create/5, get_text_extent/2 ]).


% Exported helpers (and silencing):
-export([ to_wx_font_family/1, to_wx_font_style/1, to_wx_font_weight/1 ]).


% For wx defines:
-include("gui_internal_defines.hrl").


-type wx_font_family() :: wx_enum().

-type wx_font_style() :: wx_enum().

-type wx_font_weight() :: wx_enum().

-type wx_text_encoding() :: wx_enum().

-type wx_font_option() :: any().



% Shorthands:

-type ustring() :: text_utils:ustring().

-type any_string() :: text_utils:any_string().

-type dimensions() :: gui:dimensions().

-type wx_enum() :: gui_wx_backend:wx_enum().



% @doc Creates a font object from specified requirements, to determine the
% appearance of rendered text.
%
-spec create( font_size() | point_size(), font_family(), font_style(),
			  font_weight() ) -> font().
create( FontSize, FontFamily, FontStyle, FontWeight ) ->
	create( FontSize, FontFamily, FontStyle, FontWeight, _FontOpts=[] ).



% @doc Creates a font object from specified requirements, to determine the
% appearance of rendered text.
%
-spec create( font_size() | point_size(), font_family(), font_style(),
			  font_weight(), [ font_option() ] ) -> font().
create( FontSize, FontFamily, FontStyle, FontWeight, FontOpts ) ->

	cond_utils:if_defined( myriad_debug_gui_font, trace_utils:debug_fmt(
		"Creating a font of family '~ts', of size ~w, "
		"~ts style, ~ts weight and options ~p.",
		[ FontFamily, FontSize, FontStyle, FontWeight, FontOpts ] ) ),

	WxFontFamily = to_wx_font_family( FontFamily ),

	WxFontStyle = to_wx_font_style( FontStyle ),

	WxFontWeight = to_wx_font_weight( FontWeight ),

	WxFontOpts = to_wx_font_options( FontOpts ),

	Font = wxFont:new( FontSize, WxFontFamily, WxFontStyle, WxFontWeight,
					   WxFontOpts ),

	cond_utils:if_defined( myriad_debug_gui_memory,
						   true = wxFont:isOk( Font ) ),

	Font.



% @doc Returns the extent used by the rendering of the specified single-line
% text with the specified font.
%
-spec get_text_extent( ustring(), font() ) -> dimensions().
get_text_extent( Text, Font ) ->

	cond_utils:if_defined( myriad_debug_gui_font, trace_utils:debug_fmt(
		"Getting extent of text '~ts' for font ~p.", [ Text, Font ] ) ),

	% We have to create dummy bitmap and device contexts in order to determine
	% these dimensions:

	TmpBmp = wxBitmap:new( _W=200, _H=200 ),

	cond_utils:if_defined( myriad_debug_gui_memory,
						   true = wxBitmap:isOk( TmpBmp ) ),

	TmpDC = wxMemoryDC:new( TmpBmp ),

	cond_utils:if_defined( myriad_debug_gui_memory, true = wxDC:isOk( TmpDC ) ),

	wxMemoryDC:setFont( TmpDC, Font ),

	Dims = wxDC:getTextExtent( TmpDC, Text ),

	wxMemoryDC:destroy( TmpDC ),

	wxBitmap:destroy( TmpBmp ),

	Dims.


% @doc Converts the specified font family into a wx one.
-spec to_wx_font_family( font_family() ) -> wx_font_family().
to_wx_font_family( default_font_family ) ->
	?wxFONTFAMILY_DEFAULT;

to_wx_font_family( Other ) ->
	throw( { unknown_font_family, Other } ).



% @doc Converts the specified font style into a wx one.
-spec to_wx_font_style( font_style() ) -> wx_font_style().
to_wx_font_style( normal ) ->
	?wxFONTSTYLE_NORMAL;

to_wx_font_style( Other ) ->
	throw( { unknown_font_style, Other } ).



% @doc Converts the specified font weight into a wx one.
-spec to_wx_font_weight( font_weight() ) -> wx_font_weight().
to_wx_font_weight( bold ) ->
	?wxFONTWEIGHT_BOLD;

to_wx_font_weight( Other ) ->
	throw( { unknown_font_weight, Other } ).


% @doc Converts the specified font options into wx ones.
-spec to_wx_font_options( [ font_option() ] ) -> [ wx_font_option() ].
to_wx_font_options( FontOpts ) ->
	[ to_wx_font_option( FO ) || FO <- FontOpts ].


% @doc Converts the specified font option into a wx one.
-spec to_wx_font_option( font_option() ) -> wx_font_option().
to_wx_font_option( FontOpt={ underline, _Bool } ) ->
	FontOpt;

to_wx_font_option( _FontOpt={ face_name, FaceName } ) ->
	{ faceName, FaceName };

to_wx_font_option( FontOpt={ encoding, _Enum } ) ->
	FontOpt;

to_wx_font_option( OtherFontOpt ) ->
	throw( { unsupported_font_option, OtherFontOpt } ).
