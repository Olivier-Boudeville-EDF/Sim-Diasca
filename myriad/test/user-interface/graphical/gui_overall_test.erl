% Copyright (C) 2003-2022 Olivier Boudeville
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


% @doc More <b>global testing</b> of the MyriadGUI toolbox.
%
% See the gui.erl tested module.
%
-module(gui_overall_test).


% For run/0 export and al:
-include("test_facilities.hrl").

% For the circle record:
-include("bounding_box2.hrl").


% For reuse by other tests:
-export([ get_main_window_width/0, get_main_window_height/0,
		  get_main_window_size/0 ]).



% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type length() :: gui:length().
-type dimensions() :: gui:dimensions().

-type button() :: gui:button().
-type canvas() :: gui:canvas().


-type render_mode() :: 'test_shape_rendering' | 'test_dynamic_mec'.


% State of the test application, kept and updated by its main loop.
-record( my_test_state, {

	main_frame :: gui:frame(),

	render_shape_button :: button(),
	render_mec_button   :: button(),
	clear_canvas_button :: button(),
	add_point_button    :: button(),

	% Convenient to detect canvas repaints (as disappears then):
	paste_image_button  :: button(),

	quit_button         :: button(),

	canvas :: canvas(),

	% Allows to keep track of how many renderings were done:
	render_count = 0 :: count(),

	point_count = 0 :: count(),

	render_mode = test_shape_rendering :: render_mode() } ).

-type my_test_state() :: #my_test_state{}.


-spec test_state_to_string( my_test_state() ) -> ustring().
test_state_to_string( #my_test_state{ main_frame=MainFrame,
									  render_shape_button=RenderShapeButton,
									  render_mec_button=RenderMECButton,
									  clear_canvas_button=ClearCanvasButton,
									  add_point_button=AddPointButton,
									  paste_image_button=PasteImageButton,
									  quit_button=QuitButton,
									  canvas=Canvas,
									  render_count=RenderCount,
									  point_count=PointCount,
									  render_mode=RenderMode } ) ->
	text_utils:format( "test state: ~ts", [
		text_utils:strings_to_string( [
			text_utils:format( "MainFrame: ~p", [ MainFrame ] ),
			text_utils:format( "RenderShapeButton: ~p", [ RenderShapeButton ] ),
			text_utils:format( "RenderMECButton: ~p", [ RenderMECButton ] ),
			text_utils:format( "ClearCanvasButton: ~p", [ ClearCanvasButton ] ),
			text_utils:format( "AddPointButton: ~p", [ AddPointButton ] ),
			text_utils:format( "PasteImageButton: ~p", [ PasteImageButton ] ),
			text_utils:format( "QuitButton: ~p", [ QuitButton ] ),
			text_utils:format( "Canvas: ~p", [ Canvas ] ),
			text_utils:format( "RenderCount: ~p", [ RenderCount ] ),
			text_utils:format( "PointCount: ~p", [ PointCount ] ),
			text_utils:format( "RenderMode: ~p", [ RenderMode ] ) ] ) ] ).



-spec get_main_window_width() -> length().
get_main_window_width() ->
	800.


-spec get_main_window_height() -> length().
get_main_window_height() ->
	600.


% @doc Returns the dimensions of the main test window.
-spec get_main_window_size() -> dimensions().
get_main_window_size() ->
	{ get_main_window_width(), get_main_window_height() }.



% Canvas dimensions automatically determined based on parent panel.


-spec run_test_gui() -> void().
run_test_gui() ->

	test_facilities:display( "Starting the actual overall MyriadGUI test, "
							 "from user process ~w.", [ self() ] ),

	gui:start(),

	MainFrame = gui:create_frame( _Title="MyriadGUI Overall Test",
								  get_main_window_size() ),

	% This process will subscribe to following event:
	MainFrameEvents = { onWindowClosed, MainFrame },


	StatusBar = gui:create_status_bar( MainFrame ),

	gui:push_status_text( "Waiting for points to be added.", StatusBar ),

	LeftPanel = gui:create_panel( MainFrame ),

	RightPanel = gui:create_panel( MainFrame ),

	% To check surfaces:
	%gui:set_background_color( MainFrame, red ),
	%gui:set_background_color( LeftPanel, blue ),
	%gui:set_background_color( RightPanel, green ),

	MainSizer = gui:create_sizer( _Orientation=horizontal ),

	% Constant width:
	gui:add_to_sizer( MainSizer, LeftPanel,
					  [ { proportion, 0 }, { flag, [ expand_fully ] } ] ),

	% Grows with the window:
	gui:add_to_sizer( MainSizer, RightPanel,
					  [ { proportion, 2 }, { flag, [ expand_fully ] } ] ),


	ControlBoxSizer = gui:create_sizer_with_labelled_box( vertical, LeftPanel,
														  "Controls" ),

	% Adding the buttons to the control panel:

	ButtonLabels = [ "Render a few random shapes", "Render MEC", "Add point",
					 "Paste image", "Clear canvas", "Quit" ],

	ControlButtons = [ RenderShapeButton, RenderMECButton, AddPointButton,
					   PasteImageButton, ClearCanvasButton, QuitButton ] =
		gui:create_buttons( ButtonLabels, _Parent=LeftPanel ),

	ButtonEvents = { onButtonClicked, ControlButtons },

	gui:set_tooltip( LeftPanel, "Controls for the GUI test" ),

	gui:set_tooltip( RenderShapeButton, "Render shape" ),
	gui:set_tooltip( RenderMECButton, "Render Minimal Enclosing Circle" ),
	gui:set_tooltip( AddPointButton, "Add a point to the\ncurrent polygon" ),
	gui:set_tooltip( PasteImageButton, "Paste image" ),
	gui:set_tooltip( ClearCanvasButton, "Clear canvas" ),
	gui:set_tooltip( QuitButton, "Quit" ),

	ButtonOpt = [ { flag, [ expand_fully ] } ],

	gui:add_to_sizer( ControlBoxSizer, ControlButtons, ButtonOpt ),

	gui:set_sizer( LeftPanel, ControlBoxSizer ),

	PolyBoxSizer = gui:create_sizer_with_labelled_box( vertical, RightPanel,
													   "Polygon View" ),

	Canvas = gui:create_canvas( RightPanel ),

	gui:set_background_color( Canvas, pink ),

	% Generally back-buffered canvases need to subscribe to onRepaintNeeded but
	% also to onResized, if they just *blit* their back-buffer (not re-render
	% it) when receiving an onRepaintNeeded event; otherwise the resized canvas
	% will blit a different-sized back-buffer and show random parts of memory.
	%
	% An alternative (less efficient) option is not to subscribe to onResized
	% events and to perform a full rendering at each onRepaintNeeded.
	%
	%CanvasEvents = { onRepaintNeeded, Canvas },
	CanvasEvents = { [ onRepaintNeeded, onResized ], Canvas },

	gui:add_to_sizer( PolyBoxSizer, Canvas,
					  [ { proportion, 1 }, { flag, [ expand_fully ] } ] ),

	gui:set_tooltip( Canvas, "Random polygons and their MEC\n"
							 "(Minimum Enclosing Circle Box) are drawn here." ),

	gui:set_sizer( RightPanel, PolyBoxSizer ),

	gui:set_sizer( MainFrame, MainSizer ),

	EventsOfInterest = [ MainFrameEvents, ButtonEvents, CanvasEvents ],

	% To be done before rendering the GUI (with gui:show/1), as it may result in
	% events to be emitted (e.g. onRepaintNeeded) that would not be received, if
	% not already subscribed to:
	%
	gui:subscribe_to_events( EventsOfInterest ),

	InitialPointCount = 3,

	InitialRenderMode = test_shape_rendering,

	% First rendering not relevant here (still in default, initial size of
	% 20x20, and a onRepaintNeeded event will happen first):
	%
	%render( InitialRenderMode, InitialPointCount, Canvas ),

	InitialTestState = #my_test_state{ main_frame=MainFrame,
									   render_shape_button=RenderShapeButton,
									   render_mec_button=RenderMECButton,
									   clear_canvas_button=ClearCanvasButton,
									   add_point_button=AddPointButton,
									   paste_image_button=PasteImageButton,
									   quit_button=QuitButton,
									   canvas=Canvas,
									   point_count=InitialPointCount,
									   render_mode=InitialRenderMode },

	trace_utils:debug_fmt( "Initial ~ts",
						   [ test_state_to_string( InitialTestState ) ] ),


	% Renders the GUI:
	gui:show( MainFrame ),

	test_main_loop( InitialTestState ).



% The main loop of this test.
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( TestState=#my_test_state{ main_frame=MainFrame,
										  render_shape_button=RenderShapeButton,
										  render_mec_button=RenderMECButton,
										  add_point_button=AddButton,
										  paste_image_button=PasteImageButton,
										  clear_canvas_button=ClearCanvasButton,
										  quit_button=QuitButton,
										  canvas=Canvas,
										  render_count=RenderCount,
										  render_mode=RenderMode } ) ->

	cond_utils:if_defined( myriad_gui_test_verbose,
		trace_utils:info_fmt( "Test main loop running, render mode is ~p, "
			"render count is ~B, point count is ~B.",
			[ RenderMode, RenderCount,
			  TestState#my_test_state.point_count ] ) ),

	% We use trace_utils:notice* to discriminate more easily the traces
	% originating from this test from any MyriadGUI ones:

	receive

		{ onWindowClosed, [ MainFrame, Context ] } ->

			trace_utils:notice_fmt( "Test main frame ~ts has been closed "
				"(~ts), test success.",
				[ gui:object_to_string( MainFrame ),
				  gui:context_to_string( Context ) ] ),

			gui:destruct_window( MainFrame ),

			gui:stop();


		{ onButtonClicked, [ RenderShapeButton, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Render shape test button ~ts has been clicked (~ts).",
					[ gui:object_to_string( QuitButton ),
					  gui:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( Context ) ),

			NewTestState = TestState#my_test_state{
								render_mode=test_shape_rendering },

			render_shapes( Canvas ),

			test_main_loop( NewTestState );


		{ onButtonClicked, [ RenderMECButton, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Render MEC test button ~ts has been clicked (~ts).",
					[ gui:object_to_string( QuitButton ),
					  gui:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( Context ) ),

			NewTestState = TestState#my_test_state{
								render_mode=test_dynamic_mec },

			render_mec( Canvas, TestState#my_test_state.point_count ),

			test_main_loop( NewTestState );


		{ onButtonClicked, [ AddButton, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Add point test button ~ts has been clicked (~ts).",
					[ gui:object_to_string( QuitButton ),
					  gui:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( Context ) ),

			NewPointCount = TestState#my_test_state.point_count + 1,

			NewTestState = TestState#my_test_state{ point_count=NewPointCount },

			test_main_loop( NewTestState );


		{ onButtonClicked, [ PasteImageButton, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Paste image button ~ts has been clicked (~ts).",
					[ gui:object_to_string( QuitButton ),
					  gui:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( Context ) ),

			ImagePath = "../../../doc/myriad-small.png",

			gui:load_image( Canvas, _Pos={150,50}, ImagePath ),
			gui:blit( Canvas ),

			test_main_loop( TestState );


		{ onButtonClicked, [ ClearCanvasButton, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Clear canvas button ~ts has been clicked (~ts).",
					[ gui:object_to_string( QuitButton ),
					  gui:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( Context ) ),

			gui:clear( Canvas ),
			gui:blit( Canvas ),

			test_main_loop( TestState );


		{ onButtonClicked, [ QuitButton, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt( "Quit test button ~ts has been clicked "
					"(~ts), test success.",
					[ gui:object_to_string( QuitButton ),
					  gui:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( Context ) ),

			gui:destruct_window( MainFrame ),

			gui:stop();


		{ onRepaintNeeded, [ Canvas, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Test canvas '~ts' needing repaint (~ts).",
					[ gui:object_to_string( Canvas ),
					  gui:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( Context ) ),

			gui:blit( Canvas ),

			test_main_loop( TestState#my_test_state{
								render_count=RenderCount+1 } );


		{ onResized, [ Canvas, NewSize, Context ] } ->

			cond_utils:if_defined( myriad_gui_test_verbose,
				trace_utils:notice_fmt(
					"Test canvas '~ts' resized to ~p (~ts).",
					[ gui:object_to_string( Canvas ), NewSize,
					  gui:context_to_string( Context ) ] ),
				basic_utils:ignore_unused( [ NewSize, Context ] ) ),

			render( RenderMode, TestState#my_test_state.point_count, Canvas ),

			test_main_loop( TestState#my_test_state{
								render_count=RenderCount+1 } );

		Other ->
			% Extra newline for better separation:
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message:~n ~p.~n", [ Other ] ),
			test_main_loop( TestState )

	end.



% @doc Renders the specified canvas.
-spec render( render_mode(), count(), canvas() ) -> void().
render( _RenderMode=test_shape_rendering, _PointCount, Canvas ) ->
	render_shapes( Canvas );

render( _RenderMode=test_dynamic_mec, PointCount, Canvas ) ->
	render_mec( Canvas, PointCount ).



% @doc Renders the shape examples onto the specified canvas.
-spec render_shapes( canvas() ) -> void().
render_shapes( Canvas ) ->

	%trace_utils:info_fmt(
	%   "Rendering example shapes, redrawing canvas ~w, of size ~w.",
	%   [ Canvas, gui:get_size( Canvas ) ] ),

	%gui:set_background_color( Canvas, yellow ),

	gui:clear( Canvas ),

	P1 = { 20,10 },
	P2 = { 100, 200 },

	gui:draw_line( Canvas, P1, P2 ),

	P3 = {300,50},
	Purple = gui_color:get_color( blue ),

	gui:draw_line( Canvas, P2, P3, Purple ),
	P4 = {400,250},

	gui:set_draw_color( Canvas, red ),
	gui:draw_lines( Canvas, [ P1, P3, P4 ] ),


	gui:set_draw_color( Canvas, black ),
	gui:draw_cross( Canvas, {36,26}, _FirstEdgeLength=6 ),

	LabelPosition = {72,300},

	LabelText = "A simple label, the cross indicating its specified location",

	gui:draw_label( Canvas, LabelPosition, LabelText ),
	gui:draw_cross( Canvas, LabelPosition ),

	gui:draw_labelled_cross( Canvas, {36,86}, _SecondEdgeLength=4,
							 "Cross label" ),

	gui:set_draw_color( Canvas, firebrick ),
	gui:set_fill_color( Canvas, chartreuse ),
	gui:draw_circle( Canvas, _CircleCenter={80,80}, _Radius=80 ),

	gui:set_fill_color( Canvas, undefined ),
	gui:draw_circle( Canvas, _OtherCircleCenter={180,180}, _OtherRadius=180 ),

	% Taken from polygon_test.erl:
	MyTriangle = polygon:update_bounding_box( lazy_circle,
		polygon:set_edge_color( fuchsia,
			polygon:get_triangle( {110,110}, {550,155}, {420,335} ) ) ),

	MyUprightSquare = polygon:update_bounding_box( lazy_circle,
		polygon:set_edge_color( steelblue,
			polygon:get_upright_square( _Center={250,250}, _EdgeLength=50 ) ) ),

	polygon:render( MyTriangle, Canvas ),
	polygon:render( MyUprightSquare, Canvas ),


	UnitRoots = linear_2D:get_roots_of_unit( _N=7, _StartingAngle=math:pi()/4),

	ScaledRoundRoots = [ point2:roundify( point2:scale( P, _Factor=50 ) )
									|| P <- UnitRoots ],

	TransVec = [ 80, 400 ],
	TransRoots = [ point2:translate( P, TransVec ) || P <- ScaledRoundRoots ],

	RootPoly = polygon:get_polygon( TransRoots ),

	polygon:render( RootPoly, Canvas ),

	gui:blit( Canvas ).



% @doc Renders the MEC (Minimal Enclosing Circle) view, for a polygon of
% specified number of vertices, whose coordinates are randomly determined at
% each invocation.
%
-spec render_mec( canvas(), count() ) -> void().
render_mec( Canvas, PointCount ) ->

	%trace_utils:info_fmt( "Rendering MEC for ~B random points.",
	%                      [ PointCount ] ),

	%gui:set_background_color( Canvas, blue ),

	gui:clear( Canvas ),

	gui:set_draw_color( Canvas, white ),

	RandomPoints = [ { random_utils:get_random_value( 200 ) + 300,
					   random_utils:get_random_value( 300 ) + 100 }
							|| _Count <- lists:seq( 1, PointCount ) ],

	%trace_utils:debug_fmt( "Random points: ~w.", [ RandomPoints ] ),

	{ Pivot, RemainingPoints } = linear_2D:find_pivot( RandomPoints ),

	%trace_utils:debug_fmt( "Pivot: ~w, remaining: ~w.",
	%   [ Pivot, RemainingPoints ] ),

	SortedPoints = linear_2D:sort_by_angle( Pivot, RemainingPoints ),

	%trace_utils:debug_fmt( "Sorted points: ~w.", [ SortedPoints ] ),

	gui:draw_lines( Canvas, [ Pivot | SortedPoints ] ++ [ Pivot ], green ),

	HullPoints = linear_2D:compute_convex_hull( RandomPoints ),

	%trace_utils:debug_fmt( "Hull points: ~w.", [ HullPoints ] ),

	%trace_utils:debug_fmt( "Number of hull/set points: ~B/~B.",
	%                       [ length( HullPoints ), PointCount ] ),

	#circle{ center=ExactCenter, square_radius=SquareRadius } =
		bounding_box2:get_minimal_enclosing_circle_box( HullPoints ),

	Center = point2:roundify( ExactCenter ),

	Radius = math:sqrt( SquareRadius ),

	%trace_utils:debug_fmt( "Bounding Minimal Enclosing Circle: "
	%                       "center = ~p, radius = ~f.", [ Center, Radius ] ),

	gui:draw_labelled_cross( Canvas, Center, 5, purple, "MEC center" ),

	gui:draw_circle( Canvas, Center, round( Radius ) ),

	gui:draw_lines( Canvas, [ Pivot | HullPoints ], blue ),

	% Draws the crosses last, to have them on top:
	gui:draw_labelled_cross( Canvas, Pivot, _OtherEdgeLength=10, black,
							 "Pivot" ),

	gui:set_draw_color( Canvas, blue ),

	gui:draw_numbered_points( Canvas, SortedPoints ),

	gui:blit( Canvas ).




% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the MyriadGUI test, being in batch mode)" );

		false ->
			run_test_gui()

	end,

	test_facilities:stop().
