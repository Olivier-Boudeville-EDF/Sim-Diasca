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
% Sunday, November 14, 2021.


% @doc Gathers all elements relative to the management of <b>images</b>
% (including bitmaps, icons, etc.), for loadaing, modifying, saving, scaling,
% resizing, clipping, etc., in link to MyriadGUI, and in a platform-independent
% way.
%
% Images shall be considered as just a generic, platform-independent buffer of
% RGB bytes with an optional buffer for the alpha bytes, whereas bitmaps are
% platform-specific, readily-usable graphical content.
%
% May be useful also for textures.
%
-module(gui_image).


% Implementation notes.
%
% We recommend using the following formats and extensions:
% - PNG (*.png) for lossless, bitmap-like images
% - JPEG (*.jpeg) for images akin to camera snapshots
%
% Note that apparently, according to our test, some images can be loaded fine
% (ex: "erlang.png") whereas some others not (ex: ""myriad-title.png").
%
% Here also, the opaqueness of types is difficult to preserve.


% Relies on the wxWidgets backend.


-export([ create_from_file/1, create_from_file/2,
		  getSize/1,
		  load/2, load/3, scale/3, scale/4,
		  colorize/2,

		  get_standard_icon/1,

		  % from_bitmap/1,
		  create_bitmap/1,
		  create_blank_bitmap/1, create_blank_bitmap/2,
		  create_blank_bitmap_for/1,
		  get_standard_bitmap/1,
		  destruct_bitmap/1,

		  lock_bitmap/1, draw_bitmap/3, unlock_bitmap/1,

		  create_bitmap_display/2, create_bitmap_display/3,
		  destruct_bitmap_display/1,

		  create_text_display/2, create_text_display/3,
		  destruct_text_display/1,

		  lock_window/1, unlock_window/1,
		  clear_device_context/1, blit/5, blit/6,

		  destruct/1 ]).


% For the raw_bitmap record:
-include("gui_image.hrl").


-type image() :: wxImage:wxImage().
% An image is a bitmap buffer of RGB bytes with an optional buffer for the alpha
% bytes.
%
% It is thus generic, independent from platforms and image file formats.


-type image_format() :: 'png' | 'jpeg'.
% Designates a file format able to store images.
%
% We prefer telling explicitly the type rather than trying to guess it from the
% extension of a filename, as it is clearer and more reliable.
%
% Refer to
% https://docs.wxwidgets.org/3.1/gdicmn_8h.html#a90a1eb6d85b5044a99b706fd979f27f5
% for more image formats.


-type image_quality() :: 'normal' | 'high'.
 % The requested quality for an image operation (ex: for a scaling).


-type bitmap() :: wxBitmap:wxBitmap().
% Platform-dependent bitmap, either monochrome or colour (with or without alpha
% channel).
%
% Intended to be a wrapper of whatever is the native image format, which is
% quickest/easiest to draw to a display context.


-opaque bitmap_display() :: wxStaticBitmap:wxStaticBitmap().
% A widget displaying a bitmap; a bitmap display behaves like a panel dedicated
% to the rendering of a bitmap.


-opaque icon() :: wxIcon:wxIcon().
% A small rectangular bitmap usually used for denoting a minimised application.


-opaque text_display() :: wxStaticText:wxStaticText().
% A widget displaying a text; a text display behaves like a panel dedicated
% to the rendering of a text.


-type raw_bitmap() :: #raw_bitmap{}.
% A record describing a raw, ready-to-use, bitmap, as a term, possibly loaded
% from file, or generated, etc., as opposed to an image (which respects a format
% like PNG or JPEG, has metadata, etc.).
%
% Note: currently not used, as a bitmap() offers all services needed.


-type text_display_option() :: { 'pos', point() }
							 | { 'size', size() }
							 | { 'style', [ text_display_style_opt() ] }.
% Text display specific options.


-type text_display_style_opt() ::
		'align_left'   % Align the text to the left.
	  | 'align_right'  % Align the text to the right.
	  | 'center'       % Center the text (horizontally).
	  | 'fixed_size'   % No auto-resize.
	  | 'ellipsize_beginning' % Any shrinking done from the start of the text.
	  | 'ellipsize_middle'    % Any shrinking done at the middle of the text.
	  | 'ellipsize_end'.      % Any shrinking done at the middle of the text.
% Options for text displays. See also
% [http://docs.wxwidgets.org/stable/classwx_static_text.html]


-export_type([ image/0, image_format/0, image_quality/0,
			   bitmap/0, bitmap_display/0, icon/0, text_display/0,
			   raw_bitmap/0, text_display_option/0  ]).


% For the wx defines:
-include_lib("wx/include/wx.hrl").



% Shorthands:

-type any_file_path() :: file_utils:any_file_path().

-type media_type() :: web_utils:media_type().

-type width() :: gui:width().
-type height() :: gui:height().
-type point() :: gui:point().
-type size() :: gui:size().
-type dimensions() :: gui:dimensions().
-type window() :: gui:window().
-type label() :: gui:label().
-type device_context() :: gui:device_context().
-type window_option() :: gui:window_option().
-type standard_icon_name_id() :: gui:standard_icon_name_id().
-type standard_bitmap_name_id() :: gui:standard_bitmap_name_id().

-type color_by_decimal() :: gui_color:color_by_decimal().
-type rgba_color_buffer() :: gui_color:rgba_color_buffer().
-type alpha_buffer() :: gui_color:alpha_buffer().

-type wx_enum() :: gui_wx_backend:wx_enum().



% @doc Creates an image instance whose content is read from the specified file,
% trying to auto-detect the image format of that file.
%
-spec create_from_file( any_file_path() ) -> image().
create_from_file( AnyImagePath ) ->

	ImagePath = text_utils:ensure_string( AnyImagePath ),
	Image = wxImage:new( ImagePath ),

	case wxImage:isOk( Image ) of

		true ->
			Image;

		false ->
			throw( { image_loading_failed, ImagePath } )

	end.



% @doc Creates an image instance whose content is read from the specified file,
% expecting the image format of the file to be specified one.
%
-spec create_from_file( image_format(), any_file_path() ) -> image().
% Currently format is ignored (unclear how to use format, perhaps to be
% translted as a Mimetype):
create_from_file( ImageFormat, AnyImagePath ) ->

	ImagePath = text_utils:ensure_string( AnyImagePath ),

	Image = wxImage:new( ImagePath ),

	case wxImage:isOk( Image ) of

		true ->
			Image;

		false ->
			throw( { image_loading_failed, ImageFormat, ImagePath } )

	end.



% @doc Returns the size of this image.
-spec getSize( image() ) -> dimensions().
getSize( Image ) ->
	{ wxImage:getWidth( Image ), wxImage:getHeight( Image ) }.



% @doc Scales the specified image to the specified dimensions, with a default
% quality.
%
-spec scale( image(), width(), height() ) -> void().
scale( Image, Width, Height ) ->
	wxImage:rescale( Image, Width, Height ).


% @doc Scales the specified image to the specified dimensions, with specified
% quality.
%
-spec scale( image(), width(), height(), image_quality() ) -> void().
scale( Image, Width, Height, Quality ) ->
	WxQuality = to_wx_image_quality( Quality ),
	wxImage:rescale( Image, Width, Height, _Opts=[ { quality, WxQuality } ] ).



% @doc Loads the image stored in the specified file in the specified image
% instance, trying to auto-detect the image format of that file.
%
-spec load( image(), any_file_path() ) -> void().
load( Image, ImagePath ) ->
	case wxImage:loadFile( Image, ImagePath ) of

		true ->
			ok;

		false ->
			throw( { image_loading_failed, ImagePath } )

	end.



% @doc Loads the image stored in the specified file into the specified image
% instance, expecting the image format of the file to be the specified one.
%
-spec load( image(), image_format(), any_file_path() ) -> void().
load( Image, ImageFormat, ImagePath ) ->
	WxImageFormat = to_wx_image_format( ImageFormat ),
	case wxImage:loadFile( Image, ImagePath,
						   _Opts=[ { type, WxImageFormat } ] ) of

		true ->
			ok;

		false ->
			throw( { image_loading_failed, ImagePath } )

	end.



% @doc Returns a colourized image, that is an image of the specified color,
% modulated by the specified alpha coordinates.
%
-spec colorize( color_by_decimal(), alpha_buffer() ) -> rgba_color_buffer().
colorize( AlphaBuffer, _Color={ R, G, B } ) ->
	% Binary comprehension (and wxImage:setData/3 tells that alpha buffer size
	% is width*height*3, hence dropping 2 out of 3 elements):
	%
	<< <<R:8,G:8,B:8,A:8>> || <<A:8,_:8,_:8>> <= AlphaBuffer >>.


% @doc Returns the standard icon corresponding to the specified identifier.
-spec get_standard_icon( standard_icon_name_id() ) -> icon().
get_standard_icon( StdIconId ) ->

	WxArtId = gui_wx_backend:to_wx_icon_id( StdIconId ),

	% Computed (not a literal constant):
	NullIcon = ?wxNullIcon,

	case wxArtProvider:getIcon( WxArtId ) of

		NullIcon ->
			throw( { standard_icon_not_available, StdIconId, WxArtId } );

		Icon ->
			Icon

	end.



% @doc Returns a bitmap created from the specified image path.
-spec create_bitmap( any_file_path() ) -> bitmap().
create_bitmap( ImagePath ) ->
	Image = wxImage:new( ImagePath ),
	ImgBitmap = wxBitmap:new( Image ),
	wxImage:destroy( Image ),
	case wxBitmap:isOk( ImgBitmap ) of

		true ->
			ImgBitmap;

		false ->
			throw( { bitmap_creation_failed, ImgBitmap } )

	end.



% @doc Returns a blank bitmap of the specified size.
-spec create_blank_bitmap( dimensions() ) -> bitmap().
create_blank_bitmap( _Dimensions={ Width, Height } ) ->
	create_blank_bitmap( Width, Height ).


% @doc Returns a blank bitmap of the specified size.
-spec create_blank_bitmap( width(), height() ) -> bitmap().
create_blank_bitmap( Width, Height ) ->
	ImgBitmap = wxBitmap:new( Width, Height ),
	case wxBitmap:isOk( ImgBitmap ) of

		true ->
			ImgBitmap;

		false ->
			throw( { bitmap_creation_failed, { Width, Height } } )

	end.



% @doc Returns a blank bitmap whose size is the client one of the specified
% window.
%
-spec create_blank_bitmap_for( window() ) -> bitmap().
create_blank_bitmap_for( Window ) ->
	ClientSize = wxWindow:getClientSize( Window ),
	create_blank_bitmap( ClientSize ).


% @doc Returns the standard bitmap corresponding to the specified identifier.
-spec get_standard_bitmap( standard_bitmap_name_id() ) -> bitmap().
get_standard_bitmap( StdBitmapId ) ->

	WxArtId = gui_wx_backend:to_wx_bitmap_id( StdBitmapId ),

	% Computed (not a literal constant):
	NullBitmap = ?wxNullBitmap,

	case wxArtProvider:getBitmap( WxArtId ) of

		NullBitmap ->
			throw( { standard_bitmap_not_available, StdBitmapId, WxArtId } );

		Bitmap ->
			Bitmap

	end.


% @doc Destructs the specified bitmap (which must not be locked).
-spec destruct_bitmap( bitmap() ) -> void().
destruct_bitmap( Bitmap ) ->
	wxBitmap:destroy( Bitmap ).



% @doc Locks the specified bitmap, so that direct access to its content can be
% done, through the returned device context.
%
% Once the desired changes will have been made, this bitmap must be unlocked.
%
-spec lock_bitmap( bitmap() ) -> device_context().
lock_bitmap( Bitmap ) ->
	DC = wxMemoryDC:new( Bitmap ),
	case wxDC:isOk( DC ) of

		true ->
			DC;

		false ->
			throw( { lock_bitmap_failed, Bitmap } )

	end.



% @doc Draws the specified bitmap in the specified device context, at the
% specified position.
%
-spec draw_bitmap( bitmap(), device_context(), point() ) -> void().
draw_bitmap( SourceBitmap, TargetDC, PosInTarget ) ->
	wxDC:drawBitmap( TargetDC, SourceBitmap, PosInTarget ).



% @doc Unlocks the specified bitmap, based on the specified device context
% obtained from a previous locking.
%
-spec unlock_bitmap( device_context() ) -> void().
unlock_bitmap( DC ) ->
	wxMemoryDC:destroy( DC ).



% @doc Creates a bitmap display from the specified bitmap.
-spec create_bitmap_display( window(), bitmap() ) -> bitmap_display().
create_bitmap_display( Parent, Bitmap ) ->
	create_bitmap_display( Parent, Bitmap, _Opts=[] ).


% @doc Creates a bitmap display from the specified bitmap and with the specified
% options.
%
-spec create_bitmap_display( window(), bitmap(), [ window_option() ] ) ->
												bitmap_display().
create_bitmap_display( Parent, Bitmap, Options ) ->
	wxStaticBitmap:new( Parent, _Id=?wxID_ANY, Bitmap, Options ).


% @doc Destructs the specified bitmap display.
-spec destruct_bitmap_display( bitmap_display() ) -> void().
destruct_bitmap_display( BitmapDisplay ) ->
	wxStaticBitmap:destroy( BitmapDisplay ).



% @doc Creates a text display from the specified label.
-spec create_text_display( window(), label() ) -> text_display().
create_text_display( Parent, Label ) ->
	create_text_display( Parent, Label, _Opts=[] ).


% @doc Creates a text display from the specified label and with the specified
% options.
%
-spec create_text_display( window(), label(), [ window_option() ] ) ->
												text_display().
create_text_display( Parent, Label, Options ) ->
	wxStaticText:new( Parent, _Id=?wxID_ANY, Label,
					  to_wx_static_text_options( Options ) ).


% @doc Destructs the specified text display.
-spec destruct_text_display( text_display() ) -> void().
destruct_text_display( TextDisplay ) ->
	wxStaticText:destroy( TextDisplay ).



% @doc Locks the specified window, so that direct access to its content can be
% done, through the returned device context.
%
% Once the desired changes will have been made, this window must be unlocked.
%
-spec lock_window( window() ) -> device_context().
lock_window( Window ) ->
	DC = wxWindowDC:new( Window ),
	case wxDC:isOk( DC ) of

		true ->
			DC;

		false ->
			throw( { lock_window_failed, Window } )

	end.



% @doc Unlocks the specified window, based on the specified device context
% obtained from a previous locking.
%
-spec unlock_window( device_context() ) -> void().
unlock_window( DC ) ->
	wxWindowDC:destroy( DC ).



% @doc Clears the specified device context, using the current background brush.
% If none was set, a solid white brush is used.
%
-spec clear_device_context( device_context() ) -> void().
clear_device_context( DC ) ->
	wxDC:clear( DC ).



% @doc Blits (copies) the specified area of the source device context at the
% specified position in the target device context.
%
% Returns a boolean of unspecified meaning.
%
-spec blit( device_context(), point(), width(), height(), device_context(),
			point() ) -> boolean().
blit( SourceDC, SrcTopLeft, Width, Height, TargetDC, TgtTopLeft ) ->
	blit( SourceDC, SrcTopLeft, _Size={ Width, Height }, TargetDC, TgtTopLeft ).



% @doc Blits (copies) the specified area of the source device context at the
% specified position in the target device context.
%
% Returns a boolean of unspecified meaning.
%
-spec blit( device_context(), point(), dimensions() , device_context(),
			point() ) -> boolean().
blit( SourceDC, SrcTopLeft, Size, TargetDC, TgtTopLeft ) ->
	wxDC:blit( TargetDC, TgtTopLeft, Size, SourceDC, SrcTopLeft ).




% @doc Converts a MyriadGUI image format into a wx one.
-spec to_wx_image_format( image_format() ) -> media_type().
to_wx_image_format( png ) ->
	?wxBITMAP_TYPE_PNG;

to_wx_image_format( jpeg ) ->
	?wxBITMAP_TYPE_JPEG;

to_wx_image_format( Other ) ->
	throw( { unknown_image_format, Other } ).



% @doc Converts a MyriadGUI image format into a wx one.
-spec to_wx_image_quality( image_quality() ) -> wx_enum().
to_wx_image_quality( normal ) ->
	?wxIMAGE_QUALITY_NORMAL;

to_wx_image_quality( high ) ->
	?wxIMAGE_QUALITY_HIGH;

to_wx_image_quality( Other ) ->
	throw( { unknown_image_quality, Other } ).



% @doc Converts specified MyriadGUI text display options into wx static text
% ones.
%
-spec to_wx_static_text_options( [ text_display_option() ] ) -> list().
to_wx_static_text_options( Options ) ->
	[ to_wx_static_text_option( Opt ) || Opt <- Options ].



% @doc Converts specified MyriadGUI text display option into a wx static text
% one.
%
-spec to_wx_static_text_option( text_display_option() ) -> pair:pair().
to_wx_static_text_option( { style, TextDisplayStyle }  ) ->
	{ style, to_wx_static_text_style( TextDisplayStyle ) };

% pos, size are to go through:
to_wx_static_text_option( Opt ) ->
	Opt.



% @doc Converts specified MyriadGUI text display style into a wx static text
% one.
%
-spec to_wx_static_text_style( text_display_style_opt() ) -> wx:wx_enum().
to_wx_static_text_style( _TextDisplayStyle=align_left ) ->
	?wxALIGN_LEFT;

to_wx_static_text_style( _TextDisplayStyle=align_right ) ->
	?wxALIGN_RIGHT;

to_wx_static_text_style( _TextDisplayStyle=center ) ->
	?wxALIGN_CENTRE_HORIZONTAL;

to_wx_static_text_style( _TextDisplayStyle=fixed_size ) ->
	?wxST_NO_AUTORESIZE;

to_wx_static_text_style( _TextDisplayStyle=ellipsize_beginning ) ->
	?wxST_ELLIPSIZE_START;

to_wx_static_text_style( _TextDisplayStyle=ellipsize_middle ) ->
	?wxST_ELLIPSIZE_MIDDLE;

to_wx_static_text_style( _TextDisplayStyle=ellipsize_end ) ->
	?wxST_ELLIPSIZE_END.


% @doc Declares that the specified instance can be destructed.
%
% Such it can be reference-counted, it may or may not result in an actual
% deallocation.
%
-spec destruct( image() ) -> void().
destruct( Image ) ->
	wxImage:destroy( Image ).
