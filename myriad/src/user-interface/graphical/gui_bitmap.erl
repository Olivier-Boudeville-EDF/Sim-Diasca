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
% Creation date: Saturday, September 2, 2023.


% @doc Gathering of various facilities related to the <b>management of bitmap
% content</b>.
%
% Bitmaps correspond to the locally-native image format that is the
% quickest/easiest to operate with - as opposed to images, which are generic
% elements, independent from platform and image file format (these are just
% buffers of RGB bytes, with an optional buffer for the alpha bytes).
%
% See also the gui_canvas and gui_render (e.g. for draw_bitmap/*) modules.
%
-module(gui_bitmap).


-type bitmap() :: gui_image:bitmap().
% Platform-dependent bitmap, either monochrome or colour (with or without alpha
% channel).
%
% Intended to be a wrapper of whatever is the native image format, which is
% quickest/easiest to draw to a display context (as opposed to images, which are
% platform-independent).
%
% A given bitmap may or may not have its internal data buffer set.


-type empty_bitmap() :: bitmap().
% A bitmap with no internal data buffer set.
%
% When a bitmap is constructed as empty, it embeds no data buffer; use typically
% populate_buffer/4 to add such data.



% For the raw_bitmap record:
-include("gui_image.hrl").

-type raw_bitmap() :: #raw_bitmap{}.


% A record describing a raw, ready-to-use, bitmap, as a term, possibly loaded
% from file, or generated, etc., as opposed to an image (which respects a format
% like PNG or JPEG, has metadata, etc.).
%
% Note: currently not used, as a bitmap() offers all services needed.


-opaque bitmap_display() :: gui_image:bitmap_display().
% A widget displaying a bitmap; a bitmap display behaves like a panel dedicated
% to the rendering of a bitmap.


% In the same order as listed in
% https://docs.wxwidgets.org/stable/classwx_art_provider.html:
%
-type standard_bitmap_name_id() ::
	'error_bitmap' | 'question_bitmap' | 'warning_bitmap'
 | 'information_bitmap' | 'add_bookmark_bitmap'
 | 'delete_bookmark_bitmap' | 'help_side_panel_bitmap'
 | 'help_settings_bitmap' | 'help_book_bitmap' | 'help_folder_bitmap'
 | 'help_page_bitmap' | 'go_back_bitmap' | 'go_forward_bitmap'
 | 'go_up_bitmap' | 'go_down_bitmap' | 'go_to_parent_bitmap'
 | 'go_home_bitmap' | 'goto_first_bitmap' | 'goto_last_bitmap'
 | 'print_bitmap' | 'help_bitmap' | 'tip_bitmap' | 'report_view_bitmap'
 | 'list_view_bitmap' | 'new_folder_bitmap' | 'folder_bitmap'
 | 'open_folder_bitmap' | 'go_folder_up_bitmap' | 'executable_file_bitmap'
 | 'normal_file_bitmap' | 'tick_mark_bitmap' | 'cross_mark_bitmap'
 | 'missing_image_bitmap' | 'new_bitmap' | 'file_open_bitmap'
 | 'file_save_bitmap' | 'file_save_as_bitmap' | 'file_delete_bitmap'
 | 'copy_bitmap' | 'cut_bitmap' | 'paste_bitmap' | 'undo_bitmap'
 | 'redo_bitmap' | 'plus_bitmap' | 'minus_bitmap' | 'close_bitmap'
 | 'quit_bitmap' | 'find_bitmap' | 'find_and_replace_bitmap'
 | 'full_screen_bitmap' | 'edit_bitmap' | 'hard_disk_bitmap'
 | 'floppy_bitmap' | 'cdrom_bitmap' | 'removable_bitmap'
 | 'backend_logo_bitmap'.
% The name identifiers of the standard bitmaps.
%
% See also gui_constants:get_bitmap_id_topic_spec/0.


-type bitmap_name_id() :: standard_bitmap_name_id() | id().


-export_type([ bitmap/0, empty_bitmap/0, raw_bitmap/0,
			   bitmap_display/0, standard_bitmap_name_id/0,
			   bitmap_name_id/0 ]).



% Functions on bitmaps:
-export([ create/2, create/3,
		  create_from/1,
		  create_empty/1, create_empty/2, create_empty_for/1,
		  get_standard/1, get_standard/2,
		  destruct/1,

		  populate_buffer/4,
		  get_width/1, get_height/1, get_size/1,

		  lock/1, unlock/1,
		  draw/3 ]).


% Functions about bitmap displays:
-export([ create_static_display/2, create_static_display/3,
		  destruct_static_display/1 ]).


% For the wxBITMAP_SCREEN_DEPTH defined:
-include_lib("wx/include/wx.hrl").


% Shorthands:

-type any_file_path() :: file_utils:any_file_path().

-type width() :: gui:width().
-type height() :: gui:height().
-type size() :: gui:size().
-type dimensions() :: gui:dimensions().

-type parent() :: gui:parent().
-type point() :: gui:point().

-type color_depth() :: gui_color:color_depth().

-type id() :: gui_id:id().

-type widget() :: gui_widget:widget().

-type window_option() :: gui_window:window_option().

-type device_context() :: gui_render:device_context().



% @doc Returns a bitmap of the specified dimensions, using the current system
% color depth.
%
-spec create( width(), height() ) -> bitmap().
create( Width, Height ) ->
	create( Width, Height, ?wxBITMAP_SCREEN_DEPTH ).


% @doc Returns a bitmap of the specified dimensions and color depth.
-spec create( width(), height(), color_depth() ) -> bitmap().
create( Width, Height, ColorDepth ) ->
	NewBitmap = create_empty( Width, Height ),
	populate_buffer( NewBitmap, Width, Height, ColorDepth ),
	NewBitmap.


% @doc Returns a bitmap created from the image at the specified path.
-spec create_from( any_file_path() ) -> bitmap().
create_from( ImagePath ) ->
	gui_image:create_bitmap( ImagePath ).



% @doc Returns an empty (buffer-less) bitmap of the specified size.
-spec create_empty( size() ) -> empty_bitmap().
create_empty( _Size={ Width, Height } ) ->
	create_empty( Width, Height ).


% @doc Returns an empty (buffer-less) bitmap of the specified dimensions.
-spec create_empty( width(), height() ) -> empty_bitmap().
create_empty( Width, Height ) ->
	ImgBitmap = wxBitmap:new( Width, Height ),

	case wxBitmap:isOk( ImgBitmap ) of

		true ->
			ImgBitmap;

		false ->
			throw( { bitmap_creation_failed, { Width, Height } } )

	end.


% @doc Returns an empty bitmap whose size is the client one of the specified
% widget.
%
-spec create_empty_for( widget() ) -> empty_bitmap().
create_empty_for( Widget ) ->
	ClientSize = wxWindow:getClientSize( Widget ),
	create_empty( ClientSize ).


% @doc Destructs the specified bitmap (which must not be locked).
-spec destruct( bitmap() ) -> void().
destruct( Bitmap ) ->
	wxBitmap:destroy( Bitmap ).



% @doc Returns the standard bitmap of default dimensions, corresponding to the
% specified identifier.
%
-spec get_standard( standard_bitmap_name_id() ) -> bitmap().
get_standard( StdBitmapId ) ->

	WxArtId = gui_wx_backend:to_wx_bitmap_id( StdBitmapId ),

	NullBitmap = get_wx_null_bitmap(),

	% Using default size:
	case wxArtProvider:getBitmap( WxArtId ) of

		NullBitmap ->
			throw( { standard_bitmap_not_available, StdBitmapId, WxArtId } );

		Bitmap ->
			Bitmap

	end.


% @doc Returns the standard bitmap corresponding to the specified identifier and
% dimensions.
%
-spec get_standard( standard_bitmap_name_id(), dimensions() ) -> bitmap().
get_standard( StdBitmapId, Dimensions ) ->

	WxArtId = gui_wx_backend:to_wx_bitmap_id( StdBitmapId ),

	NullBitmap = get_wx_null_bitmap(),

	% Using default size:
	case wxArtProvider:getBitmap( WxArtId, _Opts=[ { size, Dimensions } ] ) of

		NullBitmap ->
			throw( { standard_bitmap_not_available, StdBitmapId, Dimensions,
					 WxArtId } );

		Bitmap ->
			Bitmap

	end.



% @doc Creates the internal data buffer of the specific (supposedly empty)
% bitmap according to the specified settings.
%
-spec populate_buffer( empty_bitmap(), width(), height(), color_depth() ) ->
											void().
populate_buffer( EmptyBitmap, Width, Height, ColorDepth ) ->
	wxBitmap:create( EmptyBitmap, Width, Height,
					 [ { depth, ColorDepth } ] ) orelse
		throw( { bitmap_buffer_creation_failed, Width, Height, ColorDepth } ).



% @doc Returns the current width of the specified bitmap.
-spec get_width( bitmap() ) -> width().
get_width( Bitmap ) ->
	wxBitmap:getWidth( Bitmap ).


% @doc Returns the current height of the specified bitmap.
-spec get_height( bitmap() ) -> height().
get_height( Bitmap ) ->
	wxBitmap:getHeight( Bitmap ).


% @doc Returns the current size of the specified bitmap.
-spec get_size( bitmap() ) -> size().
get_size( Bitmap ) ->
	{ wxBitmap:getWidth( Bitmap ), wxBitmap:getHeight( Bitmap ) }.



% @doc Locks the specified bitmap, so that direct access to its content can be
% done, through the returned device context.
%
% Once the desired changes will have been made, this bitmap must be unlocked.
%
-spec lock( bitmap() ) -> device_context().
lock( Bitmap ) ->
	DC = wxMemoryDC:new( Bitmap ),
	case wxDC:isOk( DC ) of

		true ->
			DC;

		false ->
			throw( { lock_bitmap_failed, Bitmap } )

	end.


% @doc Unlocks the specified bitmap, based on the specified device context
% obtained from a previous locking.
%
-spec unlock( device_context() ) -> void().
unlock( DC ) ->
	wxMemoryDC:destroy( DC ).


% @doc Draws the specified bitmap on the specified device context, at the
% specified position.
%
-spec draw( bitmap(), device_context(), point() ) -> void().
draw( SourceBitmap, TargetDC, PosInTarget ) ->
	wxDC:drawBitmap( TargetDC, SourceBitmap, PosInTarget ).




% Bitmap display section.


% @doc Creates a static bitmap display from the specified bitmap.
-spec create_static_display( bitmap(), parent() ) -> bitmap_display().
create_static_display( Bitmap, Parent ) ->
	create_static_display( Bitmap, _Opts=[], Parent ).


% @doc Creates a bitmap display from the specified bitmap and with the specified
% options.
%
-spec create_static_display( bitmap(), [ window_option() ], parent() ) ->
												bitmap_display().
create_static_display( Bitmap, Options, Parent ) ->
	wxStaticBitmap:new( Parent, gui_id:get_any_id(), Bitmap, Options ).


% @doc Destructs the specified bitmap display.
-spec destruct_static_display( bitmap_display() ) -> void().
destruct_static_display( BitmapDisplay ) ->
	wxStaticBitmap:destroy( BitmapDisplay ).



% @doc Return the wx null bitmap, which is typically returned whenever a bitmap
% is not found.
%
-spec get_wx_null_bitmap() -> bitmap().
get_wx_null_bitmap() ->
	% Computed (not a literal constant):
	?wxNullBitmap.
