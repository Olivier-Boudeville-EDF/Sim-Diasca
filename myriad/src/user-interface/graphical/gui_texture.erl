% Copyright (C) 2023-2024 Olivier Boudeville
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
% Creation date: Monday, March 13, 2023.


% @doc Gathering of various facilities for the <b>support of (OpenGL)
% textures</b>, mostly 2D ones, either old-style (compatibility one) or
% according to current, modern OpenGL.
%
-module(gui_texture).


% Notably for the numerous GL defines and the texture record:
-include("gui_opengl.hrl").


-type texture_id() :: non_neg_integer().
% An OpenGL texture "name" (meant to be unique), an identifier thereof.


-type texture_dimension() :: ?GL_TEXTURE_1D | ?GL_TEXTURE_2D | ?GL_TEXTURE_3D.
% The dimensionality of a texture.
%
% Most textures are 2D ones.


-type texture() :: #texture{}.
% Information regarding a (2D) texture.


-type texture_unit() :: enum(). % Actually ?GL_TEXTUREn, where n is an integer
								% in [0, ?GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS-1]
								% (the initial value is GL_TEXTURE0)
% A texture unit corresponds to the location of a texture for a shader.
%
% This allows shaders notably to access, through samplers, to more than one
% texture; a texture unit can thus be seen as a bridge between a texture and a
% sampler.
%
% The default texture unit for a texture is ?GL_TEXTURE0 (it is the initial
% active texture unit).
%
% A texture unit n is to be activated thanks to ?GL_TEXTUREn (whose actual value
% is not n; for example ?GL_TEXTURE0 may be equal to 33984), but, like in/out
% shader parameters, its location is to be specified directly as n - not as
% ?GL_TEXTUREn (and as a signed integer, not an unsigned one).


-type gl_color_format() :: 1 | 2 | 3 | 4
	| ?GL_ALPHA | ?GL_ALPHA4 | ?GL_ALPHA8 | ?GL_ALPHA12 | ?GL_ALPHA16
	| ?GL_LUMINANCE | ?GL_LUMINANCE4 | ?GL_LUMINANCE8 | ?GL_LUMINANCE12
	| ?GL_LUMINANCE16 | ?GL_LUMINANCE_ALPHA | ?GL_LUMINANCE4_ALPHA4
	| ?GL_LUMINANCE6_ALPHA2 | ?GL_LUMINANCE8_ALPHA8 | ?GL_LUMINANCE12_ALPHA4
	| ?GL_LUMINANCE12_ALPHA12 | ?GL_LUMINANCE16_ALPHA16 | ?GL_INTENSITY
	| ?GL_INTENSITY4 | ?GL_INTENSITY8 | ?GL_INTENSITY12 | ?GL_INTENSITY16
	| ?GL_R3_G3_B2 | ?GL_RGB | ?GL_RGB4 | ?GL_RGB5 | ?GL_RGB8 | ?GL_RGB10
	| ?GL_RGB12 | ?GL_RGB16 | ?GL_RGBA | ?GL_RGBA2 | ?GL_RGBA4
	| ?GL_RGB5_A1 | ?GL_RGBA8 | ?GL_RGB10_A2 | ?GL_RGBA12 | ?GL_RGBA16.
% An OpenGL color format (e.g. ?GL_RGBA), which specifies the number of color
% components in a texture.


-type gl_pixel_format() :: ?GL_COLOR_INDEX | ?GL_RED | ?GL_GREEN | ?GL_BLUE
	| ?GL_ALPHA | ?GL_RGB | ?GL_RGBA | ?GL_LUMINANCE | ?GL_LUMINANCE_ALPHA.
% A format of a pixel data.


% A texel is a texture pixel.


-type uv_coordinate() :: float().
% A texture UV (or {S,T}) coordinate. They do not depend on resolution, and
% generally are in [0.0, 1.0]. They are used to sample the source image (texels)
% in order to fill a given polygon to texture with corresponding pixels /
% fragments.
%
% OpenGL expects the 0.0 coordinate on the Y axis to be on the bottom side of
% the image, yet images usually have 0.0 at the top of this axis, so images may
% have to be flipped upside-down first, when loaded.


-type uv_point() :: { uv_coordinate(), uv_coordinate() }.
% Designates a point in a (2D) texture; not a vector per se.
%
% Texture coordinates start at {0.0, 0.0} for the lower left corner of a
% texture image to {1.0, 1.0} for its upper right corner.
%
% The fragment shader interpolates the texture coordinates for each fragment.


-type gl_texture_wrapping_mode() ::

	?GL_REPEAT % The default behavior for textures. Repeats the texture image.

  | ?GL_MIRRORED_REPEAT % Same as GL_REPEAT, but mirrors the image with each
						% repeat.

  | ?GL_CLAMP_TO_EDGE % Clamps the coordinates between 0 and 1. The result
					  % is that higher coordinates become clamped to the edge,
					  % resulting in a stretched edge pattern.

  | ?GL_CLAMP_TO_BORDER. % Coordinates outside the range are now given a
						 % user-specified border color.



-type gl_texture_filtering_mode() ::

	?GL_NEAREST % Also known as nearest neighbor or point filtering); this is
				% the default texture filtering method of OpenGL, where the
				% texel whose center is closest to the texture coordinate is
				% selected.

  | ?GL_LINEAR  % Also known as (bi)linear filtering; takes an interpolated
				% value from the texture coordinate's neighboring texels,
				% approximating a color between the texels.

  | enum().
% Designates a method to select a texel based on UV coordinates.
%
% Texture filtering can be set for magnifying and minifying operations.
%
% Other modes exist.


-type mipmap_level() :: non_neg_integer().
% Uses to designate a level of scale regarding a given texture.
%
% A mipmap is a collection of texture images in which each subsequent texture is
% twice as small compared to the previous one. Use gl:generateMipmap/1 to
% generate scaled down version of a given texture.
%
% 0 is the base level (full, original size).


-type gl_mipmap_filtering_mode() ::

	?GL_NEAREST_MIPMAP_NEAREST % Takes the nearest mipmap to match the pixel
							   % size and uses nearest neighbor interpolation
							   % for texture sampling.

  | ?GL_LINEAR_MIPMAP_NEAREST  % Takes the nearest mipmap level and samples
							   % that level using linear interpolation.

  | ?GL_NEAREST_MIPMAP_LINEAR  % Linearly interpolates between the two mipmaps
							   % that most closely match the size of a pixel,
							   % and samples the interpolated level via nearest
							   % neighbor interpolation.

  | ?GL_LINEAR_MIPMAP_LINEAR.  % Linearly interpolates between the two closest
							   % mipmaps and samples the interpolated level via
							   % linear interpolation.
% To specify the filtering method between mipmap levels, for a more seamless
% switching between them.
%
% Note: ensure that any texture to which such filtering mode is applied has
% mipmaps generated indeed, otherwise no texturing may be done.


-export_type([ texture_id/0, texture_dimension/0, texture/0, texture_unit/0,
			   gl_color_format/0, gl_pixel_format/0,
			   uv_coordinate/0, uv_point/0,

			   gl_texture_wrapping_mode/0, gl_texture_filtering_mode/0,

			   mipmap_level/0, gl_mipmap_filtering_mode/0 ]).


-export([ set_basic_general_settings/0,

		  create_from_image/1, create_from_image/2,
		  load_from_file/1, load_from_file/2,

		  create_from_text/4, create_from_text/5,

		  destruct/1,

		  apply_basic_settings_on_current/0,

		  generate_mipmaps/0, generate_mipmaps/1, generate_mipmaps_for_id/1,

		  set_as_current/1, set_new_as_current/0, set_as_current_from_id/1,
		  assign_current/4,

		  set_current_texture_unit/1,

		  recalibrate_coordinates_for/2,

		  render/2, render/3,

		  get_color_buffer/1, get_color_buffer/3,


		  get_dimensions/1, get_dimensions/2,
		  generate_id/0, to_string/1,

		  get_pixel_size/1, gl_pixel_format_to_pixel_format/1 ]).



% Either we try to underline any error (typically in terms of texture
% coordinates) for troubleshooting, or to hide it as much as possible:

-ifdef(myriad_debug_opengl).

% Pure green:
-define( padding_rgb_bin, <<0, 255, 0>> ).

% Fully opaque pure green:
-define( padding_rgba_bin, <<0, 255, 0, 255>> ).

-else.

% Pure black:
-define( padding_rgb_bin, <<0, 0, 0>> ).

% Fully transparent pure black:
-define( padding_rgba_bin, <<0, 0, 0, 0>> ).

-endif.




% Implementation notes:
%
% When using padded RGB (not RGBA, as a null alpha should solve this issue)
% textures, despite properly computed Min/Max coordinates, a thin border of the
% padding color can be noticed on all edges (see when using pure green for
% padding), especially on the padded edges - right and bottom (but also on the
% non-padded ones, presumably due to the periodicity being reproduced).
%
% No specific solution has been identified (adjusting Min/Max UV coordinates
% would not be a good idea; any preprocessing does not offer much leeway, as not
% all textures can be of power-of-two dimensions); the best approach (unless
% relying on any OpenGL extension removing the power-of-two restriction) is
% probably to use RGBA, padding with a transparent color and possibly a blending
% mode in the spirit of gl:blendFunc(?GL_ONE, ?GL_ONE_MINUS_SRC_ALPHA) and/or
% possibly using a modified fragment shader.

% Refer to https://learnopengl.com/Getting-started/Textures for further
% information about textures in general.


% Shorthands:

-type count() :: basic_utils:count().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type any_file_path() :: file_utils:any_file_path().

-type ustring() :: text_utils:ustring().

-type byte_size() :: system_utils:byte_size().

-type dimensions() :: gui:dimensions().
-type width() :: gui:width().
-type height() :: gui:height().

-type coordinate() :: gui:coordinate().
-type position() :: gui:position().

-type color_by_decimal() :: gui_color:color_by_decimal().

-type color_buffer() :: gui_color:color_buffer().
-type rgb_color_buffer() :: gui_color:rgb_color_buffer().
-type rgba_color_buffer() :: gui_color:rgba_color_buffer().
-type alpha_buffer() :: gui_color:alpha_buffer().
-type pixel_format() :: gui_color:pixel_format().

-type image() :: gui_image:image().
-type image_format() :: gui_image:image_format().

-type font() :: gui_font:font().

-type brush() :: gui_render:brush().

-type enum() :: gui_opengl:enum().



% @doc Sets basic, general parameters regarding (2D) textures.
%
% Refer to apply_basic_settings_on_current/0 to define basic settings to a given
% texture instance.
%
-spec set_basic_general_settings() -> void().
set_basic_general_settings() ->
	gl:enable( ?GL_TEXTURE_2D ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ).



% @doc Creates a texture from the specified image instance, applies the default
% texture settings on it, and makes it the currently active texture.
%
% The image instance is safe to be deallocated afterwards.
%
% Mipmaps are automatically generated.
%
-spec create_from_image( image() ) -> texture().
create_from_image( Image ) ->
	create_from_image( Image, _GenMipmaps=true ).



% @doc Creates a texture from the specified image instance, flipping its Y-axis
% according to OpenGL conventions, generating mipmaps if requested, applies the
% default texture settings on it, and makes it the currently active texture.
%
% The image instance is safe to be deallocated afterwards.
%
-spec create_from_image( image(), boolean() ) -> texture().
create_from_image( Image, GenMipmaps ) ->

	cond_utils:if_defined( myriad_debug_textures,
		trace_utils:debug_fmt( "Loading texture from a ~ts.",
							   [ gui_image:to_string( Image ) ] ) ),

	% As OpenGL expects the 0.0 coordinate on the Y-axis to be on the bottom
	% side of the image:
	%
	FlippedImage = gui_image:mirror( Image, _Orientation=vertical ),

	OrigDims = { ImgWidth, ImgHeight } = gui_image:get_size( FlippedImage ),

	% Going for power-of-two (larger) dimensions:
	TargetDims = { TexWidth, TexHeight } = get_dimensions( OrigDims ),

	% The wxImage is either RGB or RGBA; we have to expand it in buffer if
	% needed, so that it has the right (power-of-two) dimensions:
	%
	ColorBuffer = get_color_buffer( FlippedImage, OrigDims, TargetDims ),
	%trace_utils:debug_fmt( "ColorBuffer : ~p", [ ColorBuffer ] ),

	% Let's create the OpenGL texture:

	TextureId = set_new_as_current(),

	PixFormat = case gui_image:has_alpha( FlippedImage ) of

		true ->
			%trace_utils:debug( "RGBA image detected." ),
			?GL_RGBA;

		false ->
			%trace_utils:debug( "RGB image detected." ),
			?GL_RGB

	end,

	gui_image:destruct( FlippedImage ),

	assign_current( TexWidth, TexHeight, PixFormat, ColorBuffer ),

	% Add mipmaps if requested:
	GenMipmaps andalso
		begin
			gl:generateMipmap( ?GL_TEXTURE_2D ),
			cond_utils:if_defined( myriad_check_textures,
								   gui_opengl:check_error() )
		end,

	Zero = 0.0,

	% This was quite strange but there seemed to be an off-by-one error
	% regarding the Y coordinates: if selecting bright-green padding, we could
	% see that the first row of the texture (hence the bottom one) was full
	% green, whereas the color buffer was apparently correct (not starting at
	% all with green but with the correct image pixels - see the ColorBuffer
	% trace above; just ending with the proper, expected padding).
	%
	% So, instead of using a zero min_y, we used to offset it by one (scaled)
	% source pixel (hence 1/TexWidth), however hackish it was. The culprit has
	% been found since then: all our buffers and coordinates were right, it was
	% just an artifact of the GL_REPEAT wrap parameter. Now relying on
	% GL_CLAMP_TO_EDGE and there is no need to tweek coordinates anymore.
	%
	T = #texture{
			id=TextureId, width=ImgWidth, height=ImgHeight,
			min_x=Zero,
			%min_x=1/TexWidth,
			min_y=Zero,
			%min_y=1/TexHeight,

			% These were other clumsy attempts of removing padding borders:
			% (note that textures are filled upside-down, so padding occurs at
			% the top; and on the right)
			%
			%max_x=ImgWidth / (TexWidth+1), max_y=ImgHeight / (TexHeight+1) }.
			%max_x=(ImgWidth-1) / TexWidth, max_y=(ImgHeight-1) / TexHeight }.
			max_x=ImgWidth / TexWidth, max_y=ImgHeight / TexHeight },

	%trace_utils:debug_fmt( "~Bx~B texture created from image: ~ts",
	%   [ TexWidth, TexHeight, to_string( T ) ] ),

	T.



% @doc Creates a texture from the specified image file, applies the default
% texture settings on it, and makes it the currently active texture.
%
% Prefer load_from_file/2 if applicable.
%
-spec load_from_file( any_file_path() ) -> texture().
load_from_file( ImagePath ) ->

	Image = gui_image:load_from_file( ImagePath ),

	%trace_utils:debug_fmt( "Image loaded from '~ts' of size ~Bx~B.",
	%   [ ImagePath, wxImage:getWidth( Image ), wxImage:getHeight( Image ) ] ),

	Tex = create_from_image( Image ),
	gui_image:destruct( Image ),
	Tex.



% @doc Creates a texture from the specified image file of the specified type,
% applies the default texture settings on it, and makes it the currently active
% texture.
%
-spec load_from_file( image_format(), any_file_path() ) -> texture().
load_from_file( ImageFormat, ImagePath ) ->
	Image = gui_image:load_from_file( ImageFormat, ImagePath ),
	Tex = create_from_image( Image ),
	gui_image:destruct( Image ),
	Tex.



% @doc Creates a texture corresponding to the specified text, rendered with the
% specified font, brush and color, applies the default texture settings on it,
% and makes it the currently active texture.
%
-spec create_from_text( ustring(), font(), brush(), color_by_decimal() ) ->
											texture().
create_from_text( Text, Font, Brush, Color ) ->
	create_from_text( Text, Font, Brush, Color, _Flip=false ).



% @doc Creates a texture corresponding to the specified text, rendered with the
% specified font, brush and color, flipping it vertically (upside down) if
% requested, applies the default texture settings on it, and makes it the
% currently active texture.
%
-spec create_from_text( ustring(), font(), brush(), color_by_decimal(),
						boolean() ) -> texture().
create_from_text( Text, Font, Brush, TextColor, Flip ) ->
	% Directly deriving from lib/wx/examples/demo/ex_gl.erl:

	StrDims = { StrW, StrH } = gui_font:get_text_extent( Text, Font ),

	{ Width, Height } = get_dimensions( StrDims ),

	%trace_utils:debug_fmt( "Text dimensions: {~B,~B}; "
	%   "texture dimensions: {~B,~B}.", [ StrW, StrH, Width, Height ] ),

	% Supposing no alpha information here:
	Bmp = wxBitmap:new( Width, Height ),
	cond_utils:assert( myriad_debug_gui_memory, wxBitmap:isOk( Bmp ) ),

	DC = wxMemoryDC:new( Bmp ),
	cond_utils:assert( myriad_debug_gui_memory, wxDC:isOk( DC ) ),

	wxMemoryDC:setFont( DC, Font ),

	wxMemoryDC:setBackground( DC, Brush ),

	wxMemoryDC:clear( DC ),

	wxMemoryDC:setTextForeground( DC, _WhiteRGB={ 255, 255, 255 } ),

	wxMemoryDC:drawText( DC, Text, _Origin={ 0, 0 } ),

	TextImg = wxBitmap:convertToImage( Bmp ),

	BaseImg = case Flip of

		true ->
			FlippedImg = wxImage:mirror( TextImg, [ { horizontally, false } ] ),
			wxImage:destroy( TextImg ),
			FlippedImg;

		false ->
			TextImg

	end,

	% If wanting to check:
	%TestFilename = text_utils:format( "test-texture-~B.png",
	%                                  [ length( Text) ] ),
	%gui_image:save( BaseImg, TestFilename ),

	% Expected to be RGBA:
	BaseBuffer = wxImage:getData( BaseImg ),

	ReadyBuffer = gui_image:colorize( BaseBuffer, TextColor ),

	wxImage:destroy( BaseImg ),
	wxBitmap:destroy( Bmp ),
	wxMemoryDC:destroy( DC ),

	% To assign its buffer afterwards:
	TextureId = set_new_as_current(),

	assign_current( Width, Height, _TexFormat=?GL_RGBA, ReadyBuffer ),

	apply_basic_settings_on_current(),

	% Could be done if using a mipmap filtering such as GL_LINEAR_MIPMAP_LINEAR
	% afterwards:
	%
	%generate_mipmaps(),

	%trace_utils:debug( "Texture loaded from text." ),

	% Not supposed to be null dimensions:
	#texture{ id=TextureId, width=StrW, height=StrH, min_x=0.0, min_y=0.0,
			  max_x=StrW/Width, max_y=StrH/Height }.



% @doc Destructs (that is: deletes) the specified texture(s).
-spec destruct( maybe_list( texture() ) ) -> void().
destruct( Textures ) when is_list( Textures ) ->
	gl:deleteTextures( [ Id || #texture{ id=Id } <- Textures ] ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() );

destruct( #texture{ id=Id } ) ->
	gl:deleteTextures( [ Id ] ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ).




% Mipmap generation.
%
% Once mipmaps are generated for the active texture, a better filtering mode can
% be set, typically with:
%   gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER,
%                     ?GL_LINEAR_MIPMAP_LINEAR )


% @doc Generates mipmaps for the currently active (2D) texture.
-spec generate_mipmaps() -> void().
generate_mipmaps() ->
	generate_mipmaps( _Dim=?GL_TEXTURE_2D ).


% @doc Generates mipmaps for the currently active texture.
-spec generate_mipmaps( texture_dimension() ) -> void().
generate_mipmaps( TexDim ) ->
	gl:generateMipmap( TexDim ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ).


% @doc Generates mipmaps for the texture whose identifier is specified.
-spec generate_mipmaps_for_id( texture_id() ) -> void().
generate_mipmaps_for_id( TextureId ) ->
	gl:generateTextureMipmap( TextureId ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ).


% @doc Sets the specified (2D) texture as the currently active one.
%
% Returns, if useful, its identifier.
%
-spec set_as_current( texture() ) -> texture_id().
set_as_current( #texture{ id=TextureId } ) ->
	set_as_current_from_id( TextureId ).


% @doc Creates a texture identifier, sets it as the currently active (2D) one,
% and returns it.
%
-spec set_new_as_current() -> texture_id().
set_new_as_current() ->
	set_as_current_from_id( generate_id() ).



% @doc Sets the specified (2D) texture identifier as the currently active
% one.
%
% From now on, all operations done regarding (2D) textures (that is the
% ?GL_TEXTURE_2D target) will be applied to this texture, that moreover will be
% registered in the currently active texture unit.
%
% Lower-level, defined to centralise calls.
%
% Returns, if useful, the specified identifier.
%
-spec set_as_current_from_id( texture_id() ) -> texture_id().
set_as_current_from_id( TextureId ) ->

	% To attach the texture specified from its ID to the currently active (2D)
	% texture object in the GL context:
	%
	% (thus binding is a synonym of making currently active)
	%
	gl:bindTexture( ?GL_TEXTURE_2D, TextureId ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ),

	TextureId.



% @doc Assigns the specified settings and buffer to the currently active (2D)
% texture, and applies basic settings on it.
%
-spec assign_current( width(), height(), gl_pixel_format(), color_buffer() ) ->
								void().
assign_current( TexWidth, TexHeight, PixelFormat, ColorBuffer ) ->

	% Specifies this two-dimensional texture image:
	%
	% (note that the first format is a color one, the second one is the
	% specified pixel one; in the general case they may not match))
	%
	% (this is typically a call that may result in a SEGV)
	%
	gl:texImage2D( _BindTarget=?GL_TEXTURE_2D, _MipmapLvl=0,
		_InternalTexFormat=PixelFormat, TexWidth, TexHeight, _LegacyBorder=0,
		_SrcBufferFormat=PixelFormat, _SrcPixelDataType=?GL_UNSIGNED_BYTE,
		ColorBuffer ),

	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ),

	apply_basic_settings_on_current().



% @doc Sets the current texture unit.
%
% The next texture that will be set current will be associated to this texture
% unit.
%
% The initial (default) value is ?GL_TEXTURE0.
%
-spec set_current_texture_unit( texture_unit() ) -> void().
set_current_texture_unit( TexUnit ) ->
	gl:activeTexture( TexUnit ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ).



% @doc Recalibrates the specified logical texture coordinates, supposed to
% correspond to the original texture, to the specified one, which is typically
% padded, hence of different dimensions.
%
-spec recalibrate_coordinates_for( [ uv_point() ], texture() ) ->
										[ uv_point() ].
recalibrate_coordinates_for( TexCoords, #texture{ min_x=MinX, min_y=MinY,
												  max_x=MaxX, max_y=MaxY } ) ->
	% We remap [0,1] to [Min,Max] in each dimension; generally MinX=MinY=0.0.

	XDiff = MaxX - MinX,
	YDiff = MaxY - MinY,

	R = recalibrate_coordinates_for( TexCoords, MinX, MinY, XDiff, YDiff,
									 _Acc=[] ),

	trace_utils:debug_fmt( "Coordinates ~p once recalibrated:~n ~p.",
						   [ TexCoords, R ] ),

	R.


% (helper)
recalibrate_coordinates_for( _TexCoords=[], _MinX, _MinY, _XDiff, _YDiff,
							 Acc ) ->
	lists:reverse( Acc );

recalibrate_coordinates_for( _TexCoords=[ { X, Y } | T ], MinX, MinY,
							 XDiff, YDiff, Acc ) ->
	XPadded = MinX + XDiff*X,
	YPadded = MinY + YDiff*Y,

	recalibrate_coordinates_for( T, MinX, MinY, XDiff, YDiff,
								 [ { XPadded, YPadded } | Acc ] ).



% @doc Sets basic parameters on the currently active (2D) texture.
%
% Refer to set_basic_general_settings/0 for overall settings (transverse to all
% texture instances).
%
-spec apply_basic_settings_on_current() -> void().
apply_basic_settings_on_current() ->

	% These settings are not general, they will be assigned specifically to the
	% currently active texture, see
	% https://registry.khronos.org/OpenGL-Refpages/gl4/html/glTexParameter.xhtml
	%
	% (GL_LINEAR generally better than GL_NEAREST, see
	% https://learnopengl.com/Getting-started/Textures)
	%

	% No ?GL_LINEAR_MIPMAP* suitable yet, as they relate to down (not up)
	% scaling:
	%
	gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ),

	% GL_LINEAR_MIPMAP_LINEAR has been disabled, as no actual texturing will be
	% done if the current texture has no mipmap generated:
	%
	gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER,
					  %?GL_LINEAR_MIPMAP_LINEAR ),
					  ?GL_LINEAR ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ),

	% Otherwise the current color will be applied to the textured polygons as
	% well (as the default texture environment mode is GL_MODULATE, which
	% multiplies the texture color with the vertex color):
	%
	% (another option is to reset the modulation at rendering with:
	%  gl:color4f( 1.0, 1.0, 1.0, 1.0 ))
	%
	%gl:texEnvi( ?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE ),
	gl:texEnvi( ?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ),

	%gl:pixelStorei( ?GL_UNPACK_ROW_LENGTH, 0 ),
	%gl:pixelStorei( ?GL_UNPACK_ALIGNMENT, 2 ),

	% Possibly not that useful, UV coordinates remaining generally in [0.0,
	% 1.0]; moreover enabling padding with a bright green shows that because of
	% tiling the first texture row (hence its bottom one) becomes a padding one:
	%
	%WrapParameter = ?GL_REPEAT

	% Not unwanting tiling:
	WrapParameter = ?GL_CLAMP_TO_EDGE,

	gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, WrapParameter ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ),

	gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, WrapParameter ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ).



% @doc Renders the specified texture, at the specified position.
-spec render( texture(), position() ) -> void().
render( Texture, _Pos={X,Y} ) ->
	render( Texture, X, Y ).



% @doc Renders the specified texture, at the specified position.
-spec render( texture(), coordinate(), coordinate() ) -> void().
render( #texture{ id=TextureId,
				  width=Width,
				  height=Height,
				  min_x=MinXt,
				  min_y=MinYt,
				  max_x=MaxXt,
				  max_y=MaxYt }, Xp, Yp ) ->

	%trace_utils:debug_fmt( "Rendering texture ~w (size: ~wx~w), from {~w,~w} "
	%   "to {~w,~w}.",
	%   [ TextureId, Width, Height, MinXt, MinYt, MaxXt, MaxYt ] ),

	set_as_current_from_id( TextureId ),

	% Covers the rectangular texture area thanks to two (right-angled) triangles
	% sharing an edge:
	%
	% (a glRect*() might be relevant as well)
	%
	gl:'begin'( ?GL_TRIANGLE_STRIP ),

	OtherXp = Xp + Width,
	OtherYp = Yp + Height,

	% Associating a (2D) texture coordinate to each vertex:
	gl:texCoord2f( MinXt, MinYt ), gl:vertex2i( Xp,      Yp ),
	gl:texCoord2f( MaxXt, MinYt ), gl:vertex2i( OtherXp, Yp ),
	gl:texCoord2f( MinXt, MaxYt ), gl:vertex2i( Xp,      OtherYp ),
	gl:texCoord2f( MaxXt, MaxYt ), gl:vertex2i( OtherXp, OtherYp ),

	gl:'end'(),

	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ).



% @doc Returns texture dimensions that are suitable for a (2D) content of the
% specified dimensions.
%
-spec get_dimensions( dimensions() ) -> dimensions().
get_dimensions( _Dims={ W, H } ) ->
	get_dimensions( W, H ).


% @doc Returns texture dimensions that are suitable for a (2D) content of the
% specified dimensions.
%
-spec get_dimensions( width(), height() ) -> dimensions().
get_dimensions( Width, Height ) ->
	{ math_utils:get_next_power_of_two( Width ),
	  math_utils:get_next_power_of_two( Height ) }.



% @doc Returns a new, unique, texture identifier.
-spec generate_id() -> texture_id().
generate_id() ->
	[ TextureId ] = gl:genTextures( _Count=1 ),
	cond_utils:if_defined( myriad_check_textures, gui_opengl:check_error() ),
	TextureId.


-spec to_string( texture() ) -> ustring().
to_string( #texture{ id=TexId,
					 width=Width,
					 height=Height,
					 min_x=MinX,
					 min_y=MinY,
					 max_x=MaxX,
					 max_y=MaxY } ) ->
	text_utils:format( "texture #~B, whose content size is ~Bx~B pixels, "
		"and in-buffer coordinates are {~f,~f} to {~f,~f}",
		[ TexId, Width, Height, MinX, MinY, MaxX, MaxY ] ).



% @doc Returns the RGB or RGBA color buffer corresponding to the specified
% image.
%
% The returned buffer shall be "const", in the sense of being neither
% deallocated nor assigned to any image.
%
-spec get_color_buffer( image() ) -> color_buffer().
get_color_buffer( Image ) ->

	RGBBuffer = wxImage:getData( Image ),

	case gui_image:has_alpha( Image ) of

		true ->
			% Obtain a pointer to the array storing the alpha coordinates for
			% the pixels of this image:
			%
			AlphaBuffer = wxImage:getAlpha( Image ),
			merge_alpha( RGBBuffer, AlphaBuffer );

		false ->
			RGBBuffer

	end.


% @doc Merges the specified RGB and alpha buffers into a single RGBA one.
-spec merge_alpha( rgb_color_buffer(), alpha_buffer() ) -> rgba_color_buffer().
merge_alpha( RGBBuffer, AlphaBuffer ) ->
	% These are bitstring generators:
	list_to_binary(
		lists:zipwith( fun( {R,G,B}, A ) ->
							<<R,G,B,A>>
					   end,
					   [ {R,G,B} || <<R,G,B>> <= RGBBuffer ],
					   [ A || <<A>> <= AlphaBuffer ] ) ).



% @doc Returns the RGB or RGBA color buffer corresponding to the specified
% image, once padded according to the specified current and target dimensions.
%
% The returned buffer shall be "const", in the sense of being neither
% deallocated nor assigned to any image.
%
-spec get_color_buffer( image(), dimensions(), dimensions() ) -> color_buffer().
get_color_buffer( Image, CurrentDimensions, TargetDimensions ) ->

	RGBBuffer = wxImage:getData( Image ),

	case gui_image:has_alpha( Image ) of

		true ->
			% Obtain a pointer to the array storing the alpha coordinates for
			% the pixels of this image:
			%
			AlphaBuffer = wxImage:getAlpha( Image ),
			pad_buffer_with_alpha( RGBBuffer, AlphaBuffer, CurrentDimensions,
								   TargetDimensions  );

		false ->
			pad_buffer( RGBBuffer, CurrentDimensions, TargetDimensions )

	end.



% @doc Returns the specified RGB buffer once padded to the specified dimensions,
% expected to be at least as large as its own.
%
-spec pad_buffer( rgb_color_buffer(), dimensions(), dimensions() ) ->
			rgb_color_buffer().
pad_buffer( Buffer, CurrentDimensions={ CurrentW, CurrentH },
			_TargetDimensions={ TargetW, TargetH } ) ->

	cond_utils:if_defined( myriad_debug_textures,
		trace_utils:debug_fmt( "Padding a RGB buffer from {~B,~B} "
			"to {~B,~B}.", [ CurrentW, CurrentH, TargetW, TargetH ] ) ),

	cond_utils:if_defined( myriad_check_textures,
		begin
			TargetW < CurrentW andalso
				throw( { invalid_width_to_pad, TargetW, CurrentW } ),

			TargetH < CurrentH andalso
				throw( { invalid_height_to_pad, TargetH, CurrentH } ),

			BufSize = byte_size( Buffer ),
			ExpectedBufSize = 3 * CurrentW * CurrentH,
			BufSize =:= ExpectedBufSize orelse
				throw( { invalid_buffer_dimensions, CurrentDimensions,
						 ExpectedBufSize, BufSize } )
		end,
		basic_utils:ignore_unused( CurrentDimensions ) ),

	PadPixel = ?padding_rgb_bin,

	BinPadRow = bin_utils:replicate( PadPixel, _Count=TargetW - CurrentW ),

	RowPaddedBuffer = pad_rows( Buffer, _OrigRowSize=3*CurrentW, BinPadRow ),

	BinBlankRow = bin_utils:replicate( PadPixel, TargetW ),

	bin_utils:concatenate( RowPaddedBuffer, _BlankRowCount=TargetH-CurrentH,
						   BinBlankRow ).


-spec pad_rows( rgb_color_buffer(), count(), binary() ) -> rgb_color_buffer().
pad_rows( Buffer, OrigRowSize, BinPadRow ) ->
	pad_rows( Buffer, OrigRowSize, BinPadRow, _BinAcc= <<>> ).


% Pads each row with the specified padding binary.
%
% (helper)
%
pad_rows( _Buffer= <<>>, _OrigRowSize, _BinPadRow, BinAcc ) ->
	BinAcc;
%
% Matching not supported:
%pad_rows( _Buffer= <<BinRow:OrigRowSize/binary,T/binary>>, OrigRowSize,
%          BinPadRow, BinAcc )->
%   pad_rows( T, OrigRowSize, BinPadRow,
%             <<BinAcc/binary, BinRow/binary, BinPadRow/binary>> ).

pad_rows( Buffer, OrigRowSize, BinPadRow, BinAcc ) ->

	%trace_utils:debug_fmt( "Padding row of original size ~B bytes.",
	%                       [ OrigRowSize ] ),

	%<<BinRow:OrigRowSize/binary,T/binary>> = Buffer,
	case Buffer of

		<<BinRow:OrigRowSize/binary, T/binary>> ->
			pad_rows( T, OrigRowSize, BinPadRow,
					  <<BinAcc/binary, BinRow/binary, BinPadRow/binary>> );

		_Other ->
			throw( { unmatching_buffer, byte_size( Buffer ), OrigRowSize } )

	end.


% @doc Returns the specified RGB buffer once padded to the specified dimensions,
% expected to be at least as large as its own.
%
-spec pad_buffer_with_alpha( rgb_color_buffer(), alpha_buffer(), dimensions(),
							 dimensions() ) -> rgb_color_buffer().
pad_buffer_with_alpha( RGBBuffer, AlphaBuffer,
		CurrentDimensions={ CurrentW, CurrentH },
		_TargetDimensions={ TargetW, TargetH } ) ->

	cond_utils:if_defined( myriad_debug_textures,
		trace_utils:debug_fmt( "Padding a RGBA buffer from {~B,~B} "
			"to {~B,~B}.", [ CurrentW, CurrentH, TargetW, TargetH ] ) ),

	cond_utils:if_defined( myriad_check_textures,
		begin
			TargetW < CurrentW andalso
				throw( { invalid_width_to_pad, TargetW, CurrentW } ),

			TargetH < CurrentH andalso
				throw( { invalid_height_to_pad, TargetH, CurrentH } ),

			RGBBufSize = byte_size( RGBBuffer ),
			ExpectedRGBBufSize = 3 * CurrentW * CurrentH,
			RGBBufSize =:= ExpectedRGBBufSize orelse
				throw( { invalid_rgb_buffer_dimensions, CurrentDimensions,
						 ExpectedRGBBufSize, RGBBufSize } ),

			AlphaBufSize = byte_size( AlphaBuffer ),
			ExpectedAlphaBufSize = 1 * CurrentW * CurrentH,
			AlphaBufSize =:= ExpectedAlphaBufSize orelse
				throw( { invalid_alpha_buffer_dimensions, CurrentDimensions,
						 ExpectedAlphaBufSize, AlphaBufSize } )
		end,
		basic_utils:ignore_unused( CurrentDimensions ) ),


	PadPixel = ?padding_rgba_bin,

	% The binary used to pad each row on its right:
	BinPadRow = bin_utils:replicate( PadPixel, _Count=TargetW - CurrentW ),

	%basic_utils:assert_equal( byte_size( BinPadRow ),
	%                          4*(TargetW - CurrentW) ),

	% Right-extended buffer:
	RowPaddedBuffer =
		pad_rows_with_alpha( RGBBuffer, AlphaBuffer, CurrentW, BinPadRow ),

	%basic_utils:assert_equal( byte_size( RowPaddedBuffer ),
	%                          4*CurrentH*TargetW ),

	% The binary used to add full padding rows at the end (hence at the top) of
	% the target texture:
	%
	BinBlankRow = bin_utils:replicate( PadPixel, TargetW ),

	basic_utils:assert_equal( byte_size( BinBlankRow ),
							  4*TargetW ),

	ResBuffer = bin_utils:concatenate( RowPaddedBuffer,
		_BlankRowCount=TargetH-CurrentH, BinBlankRow ),

	cond_utils:if_defined( myriad_debug_textures,
		trace_utils:debug_fmt( "Size of the resulting buffer: ~B bytes.",
							   [ byte_size( ResBuffer ) ] ) ),

	ResBuffer.



% Pads each row, once the alpha coordinates have been interleaved, with the
% specified padding binary.
%
-spec pad_rows_with_alpha( rgb_color_buffer(), alpha_buffer(), count(),
						   binary() ) -> rgba_color_buffer().
pad_rows_with_alpha( RGBBuffer, AlphaBuffer, Width, BinPadRow ) ->
	pad_rows_with_alpha( RGBBuffer, AlphaBuffer, _PixCount=0, Width, BinPadRow,
						 _BinAcc= <<>> ).


% (helper)
%
% All mixels/rows scanned:
pad_rows_with_alpha( _RGBBuffer= <<>>, _AlphaBuffer= <<>>,
					 _PixCount=Width, Width, BinPadRow, BinAcc ) ->
	 <<BinAcc/binary, BinPadRow/binary>>;

% Reached the end of the current row:
pad_rows_with_alpha( _RGBBuffer= <<R:8, G:8, B:8, T/binary>>,
		_AlphaBuffer= <<A:8, Ta/binary>>, _PixCount=Width, Width, BinPadRow,
		BinAcc ) ->
	NewBinAcc = <<BinAcc/binary, BinPadRow/binary, R:8, G:8, B:8, A:8>>,
	pad_rows_with_alpha( T, Ta, _NextPixCount=1, Width, BinPadRow, NewBinAcc );


% Still filling the current row:
pad_rows_with_alpha( _RGBBuffer= <<R:8, G:8, B:8, T/binary>>,
		_AlphaBuffer= <<A:8, Ta/binary>>, PixCount, Width, BinPadRow,
		BinAcc ) ->
	NewBinAcc = <<BinAcc/binary, R:8, G:8, B:8, A:8>>,
	pad_rows_with_alpha( T, Ta, PixCount+1, Width, BinPadRow, NewBinAcc ).




% @doc Returns the number of bytes used by each pixel of the specified GL
% format.
%
-spec get_pixel_size( gl_pixel_format() ) -> byte_size().
get_pixel_size( GLPixFormat ) ->
	gui_color:get_pixel_size( gl_pixel_format_to_pixel_format( GLPixFormat ) ).



% @doc Returns our "standard" pixel format corresponding to the specified GL
% one.
%
-spec gl_pixel_format_to_pixel_format( gl_pixel_format() ) -> pixel_format().
gl_pixel_format_to_pixel_format( _GLPixFormat=?GL_RGB ) ->
	rgb;

gl_pixel_format_to_pixel_format( _GLPixFormat=?GL_RGBA ) ->
	rgba.
