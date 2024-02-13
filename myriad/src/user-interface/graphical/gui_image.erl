% Copyright (C) 2021-2024 Olivier Boudeville
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
% (including icons, etc.), for loading, modifying, saving, scaling,
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


% For the raw_bitmap record:
-include("gui_image.hrl").

% For ?gui_any_id:
-include("gui_internal_defines.hrl").


-opaque image() :: wxImage:wxImage().
% An image is a bitmap buffer of RGB bytes with an optional buffer for the alpha
% bytes.
%
% It is thus generic, independent from platforms and image file formats (as
% opposed to bitmaps).



-type image_bitmap_format() :: 'png'
							 | 'jpeg'
							 | 'bmp'
							 | 'gif'
							 | 'pcx'
							 | 'tiff'
							 | 'tga'
							 | 'pnm'
							 | 'iff'
							 | 'xpm'
							 | 'ico'
							 | 'cur'
							 | 'ani'.
% Designates a supported file format able to store bitmap images.


-type image_vector_format() :: 'svg'.
% Designates a supported file format able to store vector-based images.


-type image_format() :: image_bitmap_format() | image_vector_format().
% Designates a supported file format able to store (bitmap or vector) images.
%
% We prefer telling explicitly the type rather than trying to guess it from the
% extension of a filename, as it is clearer and more reliable.
%
% Refer to
% https://docs.wxwidgets.org/stable/gdicmn_8h.html#a90a1eb6d85b5044a99b706fd979f27f5
% for more image formats and to
% https://docs.wxwidgets.org/stable/classwx_image.html for the available image
% handlers (e.g. BMP, PNG, JPEG, GIF, PCX, TIFF, TGA).
%
% We recommend PNG for bitmap-like images (as lossless), JPEG for snapshot-like
% images (as is lossy but compact) and SVG for vector-based images.


-type image_quality() :: 'normal' | 'high'.
 % The requested quality for an image operation (e.g. for a scaling).


-type image_path() :: file_path().
% A path to an image, as a plain string.

-type bin_image_path() :: bin_file_path().
% A path to an image, as a binary string.

-type any_image_path() :: any_file_path().
% Any kind of path to an image.


-opaque icon() :: wxIcon:wxIcon().
% A small rectangular bitmap usually used for denoting a minimised application.


-export_type([ image/0,
			   image_bitmap_format/0, image_vector_format/0, image_format/0,
			   image_quality/0,
			   image_path/0, bin_image_path/0, any_image_path/0,
			   icon/0 ]).


% For images:
-export([ image_format_to_extension/1,
		  load_from_file/1, load_from_file/2,
		  destruct/1, destruct_multiple/1,

		  get_width/1, get_height/1, get_size/1,

		  has_alpha/1,
		  load/2, load/3, save/2, save/3,
		  scale/3, scale/4, mirror/2,
		  colorize/2,
		  to_bitmap/1,
		  to_string/1 ]).


% Other functions:
-export([ create_bitmap/1 ]).

% For icons:
-export([ get_standard_icon/1 ]).


% Implementation notes.
%
% We recommend using the following formats and extensions:
% - PNG (*.png) for lossless, bitmap-like images
% - JPEG (*.jpeg) for images akin to camera snapshots

% Relies on the wxWidgets backend.

% Here also, the opaqueness of types is difficult to preserve.

% We test whether images exist before specifying them to wx, in order to have
% more proper error reports (otherwise just '{bitmap_creation_failed,
% IMG_PATH}').


% Shorthands:

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().
-type any_file_path() :: file_utils:any_file_path().
-type extension() :: file_utils:extension().

-type ustring() :: text_utils:ustring().

-type media_type() :: web_utils:media_type().

-type width() :: gui:width().
-type height() :: gui:height().
-type orientation() :: gui:orientation().
-type dimensions() :: gui:dimensions().
-type standard_icon_name_id() :: gui:standard_icon_name_id().

-type color_by_decimal() :: gui_color:color_by_decimal().
-type rgba_color_buffer() :: gui_color:rgba_color_buffer().

-type bitmap() :: gui_bitmap:bitmap().


-type wx_enum() :: gui_wx_backend:wx_enum().



% @doc Creates an image instance whose content is read from the specified file,
% trying to auto-detect the image format of that file.
%
-spec load_from_file( any_image_path() ) -> image().
load_from_file( AnyImagePath ) ->

	ImagePath = text_utils:ensure_string( AnyImagePath ),

	check_image_path( ImagePath ),

	Image = wxImage:new( ImagePath ),

	%trace_utils:debug_fmt( "Image has alpha channel: ~ts.",
	%                       [ has_alpha( Image ) ] ),

	case wxImage:isOk( Image ) of

		true ->
			Image;

		false ->
			throw( { image_loading_failed, ImagePath } )

	end.



% @doc Creates an image instance whose content is read from the specified file,
% expecting the image format of the file to be specified one.
%
-spec load_from_file( image_format(), any_image_path() ) -> image().
% Currently format is ignored (unclear how to use format, perhaps to be
% translated as a Mimetype):
load_from_file( ImageFormat, AnyImagePath ) ->

	ImagePath = text_utils:ensure_string( AnyImagePath ),

	check_image_path( ImagePath ),

	Image = wxImage:new( ImagePath ),

	case wxImage:isOk( Image ) of

		true ->
			Image;

		false ->
			throw( { image_loading_failed, ImagePath, ImageFormat } )

	end.


% @doc Declares that the specified image can be destructed.
%
% As it can be reference-counted, this may or may not result in an actual
% deallocation.
%
-spec destruct( maybe_list( image() ) ) -> void().
destruct( Image ) ->
	wxImage:destroy( Image ).


% @doc Declares that the specified images can be destructed.
%
% As it can be reference-counted, this may or may not result in actual
% deallocations.
%
-spec destruct_multiple( [ image() ] ) -> void().
destruct_multiple( Images ) ->
	[ wxImage:destroy( Img ) || Img <- Images ].


% @doc Returns the width of the specified image.
-spec get_width( image() ) -> width().
get_width( Image ) ->
	wxImage:getWidth( Image ).


% @doc Returns the height of the specified image.
-spec get_height( image() ) -> height().
get_height( Image ) ->
	wxImage:getHeight( Image ).



% @doc Returns the size of the specified image.
-spec get_size( image() ) -> dimensions().
get_size( Image ) ->
	{ wxImage:getWidth( Image ), wxImage:getHeight( Image ) }.


% @doc Tells whether the specified image has an alpha channel.
-spec has_alpha( image() ) -> boolean().
has_alpha( Image ) ->
	wxImage:hasAlpha( Image ).


% @doc Scales the specified image to the specified dimensions, with a default
% quality.
%
-spec scale( image(), width(), height() ) -> void().
scale( Image, Width, Height ) ->
	% In-place; default, unknown quality:
	wxImage:rescale( Image, Width, Height ).


% @doc Scales the specified image to the specified dimensions, with the
% specified quality.
%
-spec scale( image(), width(), height(), image_quality() ) -> void().
scale( Image, Width, Height, Quality ) ->
	WxQuality = to_wx_image_quality( Quality ),
	wxImage:rescale( Image, Width, Height, _Opts=[ { quality, WxQuality } ] ).



% @doc Flips the specified image: returns a new image, corresponding to the
% specified one once mirrored as requested.
%
-spec mirror( image(), orientation() ) -> image().
mirror( Image, _Orientation=horizontal ) ->
	wxImage:mirror( Image, [ { horizontally, true } ] );

mirror( Image, _Orientation=vertical ) ->
	wxImage:mirror( Image, [ { horizontally, false } ] ).



% @doc Loads the image stored in the specified file in the specified image
% instance, trying to auto-detect the image format of that file.
%
-spec load( image(), any_image_path() ) -> void().
load( Image, ImagePath ) ->

	check_image_path( ImagePath ),

	wxImage:loadFile( Image, ImagePath ) orelse
		throw( { image_loading_failed, ImagePath } ).



% @doc Loads the image stored in the specified file into the specified image
% instance, expecting the image format of the file to be the specified one.
%
-spec load( image(), image_format(), any_image_path() ) -> void().
load( Image, ImageFormat, ImagePath ) ->

	check_image_path( ImagePath ),

	WxImageFormat = to_wx_image_format( ImageFormat ),

	wxImage:loadFile( Image, ImagePath,
					  _Opts=[ { type, WxImageFormat } ] ) orelse
		throw( { image_loading_failed, ImagePath, ImageFormat } ).



% @doc Saves the image stored in the specified image instance in the specified
% file, trying to auto-detect the image format for that file, based on its
% extension.
%
-spec save( image(), any_image_path() ) -> void().
save( Image, ImagePath ) ->

	wxImage:saveFile( Image, ImagePath ) orelse
		throw( { image_saving_failed, ImagePath } ).

	% Could be added: check_image_path( ImagePath ).


% @doc Saves the image stored in the specified image instance in the specified
% file, according to the specified image format.
%
-spec save( image(), image_format(), any_image_path() ) -> void().
save( Image, ImageFormat, ImagePath ) ->

	WxImageFormat = to_wx_image_format( ImageFormat ),

	wxImage:saveFile( Image, ImagePath, WxImageFormat ) orelse
		throw( { image_saving_failed, ImagePath, ImageFormat } ).

	% Could be added: check_image_path( ImagePath ).



% @doc Returns a colorized image, that is an image of the specified color,
% modulated by the alpha coordinates found in the specified RGBA buffer.
%
-spec colorize( rgba_color_buffer(), color_by_decimal() ) ->
										rgba_color_buffer().
colorize( SrcBuffer, _Color={ R, G, B } ) ->
	% Binary comprehension (and wxImage:setData/3 tells that alpha buffer size
	% is width*height*3, hence dropping 2 out of the 3 elements):
	%
	<< <<R:8, G:8, B:8, A:8>> || <<A:8, _:8, _:8>> <= SrcBuffer >>.



% @doc Returns a bitmap corresponding to the specified image.
%
% Does not alter that image.
%
-spec to_bitmap( image() ) -> bitmap().
to_bitmap( Image ) ->
	ImgBitmap = wxBitmap:new( Image ),
	wxBitmap:isOk( ImgBitmap ) orelse
		throw( { conversion_to_bitmap_failed, Image } ),
	ImgBitmap.



% @doc Returns a textual representation of the specified image.
-spec to_string( image() ) -> ustring().
to_string( Image ) ->

	AlphaStr = case wxImage:hasAlpha( Image ) of

		true ->
			"RGBA";

		false ->
			"RGB"

	end,

	text_utils:format( "~Bx~B ~ts image",
		[ wxImage:getWidth( Image ), wxImage:getHeight( Image ), AlphaStr ] ).



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
-spec create_bitmap( any_image_path() ) -> bitmap().
create_bitmap( ImagePath ) ->

	check_image_path( ImagePath ),

	Image = wxImage:new( ImagePath ),

	% Not using to_bitmap/1 to offer a more precise exception if needed:
	ImgBitmap = wxBitmap:new( Image ),
	wxImage:destroy( Image ),
	case wxBitmap:isOk( ImgBitmap ) of

		true ->
			ImgBitmap;

		false ->
			throw( { bitmap_creation_failed,
					 text_utils:ensure_string( ImagePath ) } )

	end.


% @doc Returns the file extension corresponding to the specified image format.
%
% Note though that often the specified atom may be used directly, instead of the
% corresponding plain string.
%
-spec image_format_to_extension( image_format() ) -> extension().
image_format_to_extension( ImgFormat ) ->
	% Currently sufficient:
	text_utils:atom_to_string( ImgFormat ).



% @doc Converts the specified MyriadGUI image format into a wx one.
-spec to_wx_image_format( image_format() ) -> media_type().
to_wx_image_format( png ) ->
	?wxBITMAP_TYPE_PNG;

to_wx_image_format( jpeg ) ->
	?wxBITMAP_TYPE_JPEG;

to_wx_image_format( bmp ) ->
	?wxBITMAP_TYPE_BMP;

to_wx_image_format( gif ) ->
	?wxBITMAP_TYPE_GIF;

to_wx_image_format( pcx ) ->
	?wxBITMAP_TYPE_PCX;

to_wx_image_format( tiff ) ->
	?wxBITMAP_TYPE_TIFF;

to_wx_image_format( tga ) ->
	?wxBITMAP_TYPE_TGA;

to_wx_image_format( pnm ) ->
	?wxBITMAP_TYPE_PNM;

to_wx_image_format( iff  ) ->
	?wxBITMAP_TYPE_IFF;

to_wx_image_format( xpm ) ->
	?wxBITMAP_TYPE_XPM;

to_wx_image_format( ico ) ->
	?wxBITMAP_TYPE_ICO;

to_wx_image_format( cur ) ->
	?wxBITMAP_TYPE_CUR;

to_wx_image_format( ani ) ->
	?wxBITMAP_TYPE_ANI;

to_wx_image_format( Other ) ->
	throw( { unknown_image_format, Other } ).



% @doc Converts the specified MyriadGUI image format into a wx one.
-spec to_wx_image_quality( image_quality() ) -> wx_enum().
to_wx_image_quality( normal ) ->
	?wxIMAGE_QUALITY_NORMAL;

to_wx_image_quality( high ) ->
	?wxIMAGE_QUALITY_HIGH;

to_wx_image_quality( Other ) ->
	throw( { unknown_image_quality, Other } ).



% Helper section.

-spec check_image_path( any_image_path() ) -> void().
check_image_path( ImagePath ) ->

	file_utils:is_existing_file_or_link( ImagePath ) orelse
		begin
			% Useful for relative paths:
			CurrentDir = file_utils:get_current_directory(),

			trace_utils:error_fmt( "The image path '~ts' "
				"does not exist as a file or a symbolic link "
				"(while current directory is '~ts').",
				[ ImagePath, CurrentDir ] ),

			throw( { non_existing_image_path, ImagePath } )

		end.
