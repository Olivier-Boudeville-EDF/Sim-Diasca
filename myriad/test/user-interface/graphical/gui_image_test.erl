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
% Creation date: Thursday, December 23, 2021.


% @doc Testing the <b>support from images</b>.
%
% Useful for example to check whether a given format is correctly supported.
%
% See the gui_image.erl tested module.
%
-module(gui_image_test).


% Implementation notes:
%
% We favor mostly PNG and JPEG images.
%
% Here, rather than using our canvas, we directly paint of the panel defined
% within the main frame.


% For run/0 export and al:
-include("test_facilities.hrl").


% For re-use by other tests:
-export([ get_test_image_directory/0 ]).



% Shorthands:

-type directory_path() :: file_utils:directory_path().

-type frame() :: gui:frame().
-type panel() :: gui:panel().
-type bitmap() :: gui:bitmap().



% State of the test application, kept and updated by its main loop.
-record( my_test_state, {

	main_frame :: frame(),

	% A panel, used here as a canvas:
	panel :: panel(),

	% The off-screen bitmaps where all renderings take place:
	backbuffer :: bitmap(),

	% The ready-to-use in-memory data corresponding to an image to be displayed:
	image_bitmap :: bitmap() } ).

-type my_test_state() :: #my_test_state{}.



% Silencing now that not subscribing to onRepaintNeeded:
-export([ update_panel/2 ]).



% @doc Returns the path to a test image directory.
-spec get_test_image_directory() -> directory_path().
get_test_image_directory() ->
	% Points to myriad/doc; relative to this test directory:
	file_utils:join( [ "..", "..", "..", "doc" ] ).


% @doc Returns the path to the main test image.
-spec get_test_main_image_path() -> directory_path().
get_test_main_image_path() ->
	file_utils:join( get_test_image_directory(), "myriad-title.png" ).



% @doc Runs the actual test.
-spec run_image_test() -> void().
run_image_test() ->

	ImagePath = get_test_main_image_path(),

	test_facilities:display( "Starting the image test, "
		"simply by displaying the '~ts' image in a resizable window.",
		[ ImagePath ] ),

	trace_utils:notice( "A resizable frame displaying the Myriad logo shall "
		"appear. The test will end as soon as this frame is closed." ),

	gui:start(),

	MainFrame = gui:create_frame( _Title="MyriadGUI Image Test",
								  gui_overall_test:get_main_window_size() ),

	% No need to add _Opts=[ { style, full_repaint_on_resize } ]:
	Panel = gui:create_panel( MainFrame ),

	% The backbuffer on which panel content will be drawn:
	BackbufferBitmap = gui:create_blank_bitmap_for( Panel ),

	% The image bitmap, kept to regenerate the backbuffer as needed:
	ImgBitmap = gui:create_bitmap( ImagePath ),

	% Initialisation:
	render_scene( Panel, BackbufferBitmap, ImgBitmap ),

	StatusBar = gui:create_status_bar( MainFrame ),

	gui:push_status_text( "Displaying image.", StatusBar ),

	% No need to subscribe to 'onRepaintNeeded' for the panel:
	gui:subscribe_to_events( [ { onWindowClosed, MainFrame },
							   { onResized, Panel } ] ),

	% Renders the GUI:
	gui:show( MainFrame ),

	test_main_loop( #my_test_state{ main_frame=MainFrame,
									panel=Panel,
									backbuffer=BackbufferBitmap,
									image_bitmap=ImgBitmap } ),

	gui:stop().



% The main loop of this test.
-spec test_main_loop( my_test_state() ) -> void().
test_main_loop( TestState=#my_test_state{ main_frame=MainFrame,
										  panel=Panel,
										  backbuffer=BackbufferBitmap,
										  image_bitmap=ImgBitmap } ) ->

	receive

		% Not subscribed to onRepaintNeeded so not never activated:
		%{ onRepaintNeeded, [ Panel, _Context ] } ->
		%   trace_utils:debug( "Repainting test panel." ),
		%
		%   % No size change, backbuffer still legit:
		%   update_panel( Panel, BackbufferBitmap ),
		%
		%   %trace_utils:debug( "Test panel repainted (blit)." ),
		%
		%   test_main_loop( TestState );


		{ onResized, [ Panel, NewSize, Context ] } ->

			%trace_utils:debug( "Resizing test panel." ),

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Test panel '~ts' resized to ~p (~ts).",
					[ gui:object_to_string( Panel ), NewSize,
					  gui:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( [ NewSize, Context ] ) ),

			% We have to resize the framebuffer first:
			NewBackbufferBitmap = gui:create_blank_bitmap( NewSize ),

			render_scene( Panel, NewBackbufferBitmap, ImgBitmap ),

			gui:destruct_bitmap( BackbufferBitmap ),

			%trace_utils:debug( "Test panel resized (render)." ),

			test_main_loop(
				TestState#my_test_state{ backbuffer=NewBackbufferBitmap } );


		{ onWindowClosed, [ MainFrame, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt( "Test main frame ~ts has been closed "
					"(~ts), test success.",
					[ gui:object_to_string( MainFrame ),
					  gui:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( Context ) ),

			gui:destruct_window( MainFrame );


		Other ->
			% Extra newline for better separation:
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message:~n ~p.~n", [ Other ] ),
			test_main_loop( TestState )

	end.



% @doc Renders the scene: updates the (bitmap) backbuffer accordingly, and blits
% it to the specified panel.
%
render_scene( TargetPanel, BackbufferBitmap, ImageBitmap ) ->

	% Updates the backbuffer with the stored image:

	% Locks the target surface (device context):
	BackbufferDC = gui:lock_bitmap( BackbufferBitmap ),

	gui:clear_device_context( BackbufferDC ),

	gui:draw_bitmap( _Source=ImageBitmap, BackbufferDC, _PosInTarget={15,130} ),


	% Then blits this updated backbuffer to the panel:
	TopLeftPos = {0,0},

	TargetPanelDC = gui:lock_window( TargetPanel ),

	gui:blit( _From=BackbufferDC, _FromPos=TopLeftPos,
			  _BlitArea=gui:get_size( BackbufferBitmap ),
			  _To=TargetPanelDC, _ToPos=TopLeftPos ),

	gui:unlock_window( TargetPanelDC ),

	gui:unlock_bitmap( BackbufferDC ).



% @doc Blits the current backbuffer bitmap to the specified panel once cleared.
update_panel( TargetPanel, BackbufferBitmap ) ->

	% No need to update the update the framebuffer.

	% Locks the target surface (device context):
	BackbufferDC = gui:lock_bitmap( BackbufferBitmap ),

	% Then blits this updated backbuffer to the panel:
	TopLeftPos = {0,0},

	TargetPanelDC = gui:lock_window( TargetPanel ),

	gui:blit( BackbufferDC, TopLeftPos, gui:get_size( BackbufferBitmap ),
			  TargetPanelDC, TopLeftPos ),

	gui:unlock_window( TargetPanelDC ),
	gui:unlock_bitmap( BackbufferDC ).



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the image test, being in batch mode)" );

		false ->
			run_image_test()

	end,

	test_facilities:stop().
