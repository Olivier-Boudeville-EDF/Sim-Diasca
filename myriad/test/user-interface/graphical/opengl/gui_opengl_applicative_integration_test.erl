% Copyright (C) 2023-2025 Olivier Boudeville
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
% Creation date: Tuesday, August 15, 2023.

-module(gui_opengl_applicative_integration_test).

-moduledoc """
Testing the **OpenGL support**, as an integration test, designated as
"applicative" since using application events and higher-level applicative GUI
states (as opposed to direct event messages, like
gui_opengl_direct_integration_test.erl does).

This test relies on the old OpenGL (the one obtained with the "compatibility"
profile), as opposed to more modern versions of OpenGL (e.g. 3.1) that rely on
shaders and GLSL.

See the gui_opengl and gui_texture tested modules.

See also gui_opengl_mvc_test.erl for a cleaner decoupling of concerns.
""".



% Implementation notes:
%
% Directly inspired (rewrite) from:
% - wx:demo/0: lib/wx/examples/demo/ex_gl.erl
% - test suite: lib/wx/test/wx_opengl_SUITE.erl
%
% As the OpenGL canvas is not resized when its containers are resized, the test
% listens to the resizing of the parent window and adapts accordingly.


% For GL/GLU defines; the sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").


% For other tests or for silencing:
-export([ update_clock_texture/2, render/1 ]).


% The duration, in milliseconds, between two updates of the OpenGL rendering:
%
% (hence presumably at 50 Hz)
%
-define( interframe_duration, 20 ).


% This is the test-specific overall GUI information:
-record( my_test_gui_info, {

	% Subsection for MyriadGUI-level test information:

	% The main frame of this test:
	main_frame :: frame(),

	% The panel occupying the client area of the main window:
	panel :: panel(),

	% An image used as a material:
	image :: image(),

	% Records the current time to update the clock texture when relevant:
	time :: option( time() ),


	% Subsection for OpenGL-related GUI test information:

	% The place where the rendering is to occur - typically a GL canvas:
	%
	% (explicitly stored, as not all rendering triggers pass such a canvas as
	% event elements - typically if performing a time-based rendering update)
	%
	client_widget :: widget(),

	mesh :: mesh(),

	angle = 0.0 :: degrees(),

	material_texture :: texture(),

	alpha_texture :: texture(),

	text_texture :: texture(),

	clock_texture :: texture(),

	font :: font(),

	brush :: brush(),

	sphere :: glu_id() } ).


-doc "This is the test-specific overall GUI information.".
-type my_test_gui_info() :: #my_test_gui_info{}.




% Type shorthands:

-type time() :: time_utils:time().

-type degrees() :: unit_utils:degrees().


-type frame() :: gui_frame:frame().
-type widget() :: gui_widget:widget().
-type panel() :: gui_panel:panel().
-type image() :: gui_image:image().
-type font() :: gui_frame:font().

-type app_gui_state() :: gui_event:app_gui_state().
-type event_elements() :: gui_event:event_elements().
-type app_event_return() :: gui_event:app_event_return().

-type brush() :: gui_render:brush().

-type glu_id() :: gui_opengl:glu_id().

-type texture() :: gui_texture:texture().

-type mesh() :: mesh:mesh().



-doc "Runs the actual test.".
-spec run_actual_test() -> void().
run_actual_test() ->

	test_facilities:display( "Starting the actual OpenGL MyriadGUI "
		"integration test, from user process ~w.", [ self() ] ),

	trace_utils:notice( "A resizable frame will be shown, "
		"comprising moving, textured rectangle, cube and sphere, "
		"displaying the current time as well, until closed by the user." ),

	gui:start(),

	%gui:set_debug_level( [ calls, life_cycle ] ),

	% Postpone the processing of first events to accelerate the initial setup:
	%InitialAppGUIState = gui:batch( fun() -> init_test_gui() end ),
	InitialAppGUIState = init_test_gui(),

	TestSpecificInfo = InitialAppGUIState#app_gui_state.app_specific_info,

	MainFrame = TestSpecificInfo#my_test_gui_info.main_frame,

	gui_frame:show( MainFrame ),

	% Uncomment to check that a no_gl_context error report is triggered indeed,
	% as expected (as no current GL context exists yet):
	%
	%gl:viewport( 0, 0, 50, 50 ),

	% OpenGL will be initialised only when the corresponding frame will be ready
	% (that is once first reported as resized):
	%
	gui_main_loop( InitialAppGUIState ).



-doc """
Creates the initial test GUI: a main frame containing a panel to which an OpenGL
canvas is associated, in which an OpenGL context is created.
""".
-spec init_test_gui() -> app_gui_state().
init_test_gui() ->

	MainFrame =
		gui_frame:create( "MyriadGUI OpenGL Applicative Integration Test" ),

	% No way found to properly clear its content before onShow is processed:
	Panel = gui_panel:create( MainFrame ),

	% Could not be cleared early:
	GLCanvas = gui_opengl:create_canvas( _Parent=Panel ),

	% Created, yet not bound yet (must wait for the main frame to be shown):
	GLContext = gui_opengl:create_context( GLCanvas ),

	% Subscribing to the panel instead would not catch any key press:
	gui:subscribe_to_events( { onKeyPressed, GLCanvas } ),

	GLBaseInfo = { GLCanvas, GLContext },

	gui:subscribe_to_events(
		{ [ onShown, onResized, onWindowClosed ], MainFrame } ),

	% (on Apple's Cocoa, subscribing to onRepaintNeeded might be required)
	gui:subscribe_to_events( { onRepaintNeeded, GLCanvas } ),

	StatusBar = gui_statusbar:create( MainFrame ),

	gui_statusbar:push_text( StatusBar, "Testing OpenGL now." ),

	Image = gui_image:load_from_file(
		gui_opengl_for_testing:get_test_image_path() ),

	% No need to mirror images for textures anymore, as already done by
	% gui_image:load_from_file/1:
	%
	%InvImage = gui_image:mirror( Image, _Orientation=horizontal ),

	%gui_image:destruct( InvImage ),

	% It is not necessary to scale to dimensions that are powers of two;
	% moreover even downscaling results in an image quite far from the original:
	%
	%gui_image:scale( Image, _NewWidth=128, _NewHeight=128 ),

	% No OpenGL-related GUI information yet (GL context cannot be set as current
	% yet):
	%
	TestSpecificGUIInfo = #my_test_gui_info{ main_frame=MainFrame,
											 panel=Panel,
											 image=Image },

	% Setting the focus on the panel would work as well:
	gui_widget:set_focus( GLCanvas ),

	AppEventSpecs = gui_event:get_base_application_event_specs(),

	InitAppGUIState = gui_event:create_app_gui_state( AppEventSpecs,
		GLBaseInfo, TestSpecificGUIInfo ),

	% Overrides the default drivers with ours:
	gui_event:set_event_drivers( [
		{ onShown,         fun test_onShown_driver/2 },
		{ onRepaintNeeded, fun test_onRepaintNeeded_driver/2 },
		{ onResized,       fun test_onResized_driver/2 },
		{ onWindowClosed,  fun test_onWindowClosed/2 } ],
								 InitAppGUIState ).



-doc """
The main loop of this test, driven by the receiving of MyriadGUI messages.
""".
-spec gui_main_loop( app_gui_state() ) -> void().
gui_main_loop( AppGUIState ) ->

	%trace_utils:debug( "Main loop." ),

	% Triggered whenever a user event is received and can be promoted to an
	% application event; we use the non-blocking version as this GUI is to be
	% updated even in the absence of user actions:
	%
	case gui_event:get_maybe_application_event( AppGUIState,
			_Timeout=?interframe_duration ) of

		{ { toggle_fullscreen, _BaseEvent }, ToggleAppGUIState } ->
			AppSpecificInfo = ToggleAppGUIState#app_gui_state.app_specific_info,
			MainFrame = AppSpecificInfo#my_test_gui_info.main_frame,

			IsFullscreen = gui_frame:is_fullscreen( MainFrame ),

			% Toggle:
			true = gui_frame:set_fullscreen( MainFrame, not IsFullscreen ),

			%trace_utils:info_fmt( "Toggle fullscreen just requested "
			%   "(event of origin: ~w), whereas fullscreen status is ~ts.",
			%   [ BaseEvent, IsFullscreen ] ),

			gui_main_loop( ToggleAppGUIState );

		{ { quit_requested, BaseEvent }, QuitAppGUIState } ->
			trace_utils:info_fmt( "Quit just requested (event of origin: ~w).",
								  [ BaseEvent ] ),
			AppSpecificInfo = QuitAppGUIState#app_gui_state.app_specific_info,
			gui_frame:destruct( AppSpecificInfo#my_test_gui_info.main_frame ),
			gui:stop();

		{ _MaybeAppEventPair=undefined, EventAppGUIState } ->
			% User event processed without generating an application one:
			% (we could trigger a rendering from there as well)
			%
			gui_main_loop( EventAppGUIState );

		undefined ->
			% As this GUI is to be updated even in the absence of user actions:
			case AppGUIState#app_gui_state.opengl_base_state of

				{ uninitialised, _GLCanvas, _GLContext } ->
					trace_utils:debug( "(OpenGL not initialised yet)" ),
					gui_main_loop( AppGUIState );

				{ initialised, _GLCanvas, _GLContext } ->
					RenderAppGUIState = update_rendering( AppGUIState ),
					gui_main_loop( RenderAppGUIState )

			end

	end.



-doc """
The test-specific event driver for the onShown (user) event type, overriding
default_onShown_driver/2: sets up OpenGL, once for all, now that a proper OpenGL
context is available.

Its type is event_driver().
""".
-spec test_onShown_driver( event_elements(), app_gui_state() ) ->
								app_event_return().
% Here OpenGL is to be used, but is not initialised yet.
%
% This is the most suitable first location to initialise OpenGL, as making a GL
% context current requires a shown window. So, as soon as the main frame is
% shown, OpenGL is initialised once from all, and the onShown event is not
% listened to anymore (anyway it is not expected to be triggered again).
%
% As a result, this is the only clause defined: neither GLStatus=initialised nor
% a disabled OpenGL base state are to support.
%
test_onShown_driver( _Elements=[ Frame, FrameId, EventContext ],
		AppGUIState=#app_gui_state{
			opengl_base_state={ _GLStatus=uninitialised, GLCanvas,
								GLContext },
			app_specific_info=TestSpecificInfo } ) ->

	% A transient former content for frame and canvas can be seen briefly, as we
	% did not succeed in clearing it early at start-up.

	trace_utils:debug_fmt( "Frame ~ts (ID: ~ts) is shown (~ts), with an "
		"initial size of ~w; using OpenGL, which as expected is not "
		"initialised yet; initialising it.",
		[ gui:object_to_string( Frame ), gui_id:id_to_string( FrameId ),
		  gui_event:context_to_string( EventContext ),
		  gui_widget:get_size( Frame ) ] ),

	% Optional yet better:
	gui:unsubscribe_from_events( { onShown, Frame } ),

	% Initial size of GL canvas is typically 20x20 pixels:
	Size = gui_widget:get_client_size( GLCanvas ),

	trace_utils:debug_fmt( "Test initialising OpenGL "
		"(whereas canvas is of initial size ~w).", [ Size ] ),

	% So done only once:
	gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

	test_facilities:display( "Reported OpenGL settings: "
		"vendor is '~ts', renderer is '~ts'; OpenGL version is '~ts', "
		"and the one of the shading language is '~ts'.",
		[ gui_opengl:get_vendor_name(), gui_opengl:get_renderer_name(),
		  text_utils:version_to_string( gui_opengl:get_version() ),
		  gui_shader:get_shading_language_version() ] ),

	% These settings will not change afterwards (they are set once for all):

	gui_texture:set_basic_general_settings(),

	gl:enable( ?GL_DEPTH_TEST ),
	gl:depthFunc( ?GL_LESS ),

	% Solid white:
	gl:clearColor( 1.0, 1.0, 1.0, 1.0 ),

	MatTexture = gui_texture:create_from_image(
		TestSpecificInfo#my_test_gui_info.image ),

	AlphaTexture = gui_texture:load_from_file(
		gui_opengl_for_testing:get_logo_image_path() ),

	Font = gui_font:create( _PointSize=32, _Family=default_font_family,
							_Style=normal, _Weight=bold ),

	Brush = gui_render:create_brush( _BlackRGB={ 0, 0, 0 } ),

	% Myriad RGB dark blue:
	TextColor = { 0, 39, 165 },

	TextTexture = gui_texture:create_from_text( "MyriadGUI rocks!", Font,
												Brush, TextColor, _Flip=true ),

	ClockTexture =
		get_clock_texture( time_utils:get_local_time(), Font, Brush ),


	TestMesh = gui_opengl_for_testing:get_test_colored_cube_mesh(
		_EdgeLength=1.0, _FaceGranularity=per_vertex ),

	%TestMesh = gui_opengl_for_testing:get_test_tetra_mesh(),

	% We cannot call here mesh_render:initialise_for_opengl/2, as it is
	% shader-based.

	SphereId = glu:newQuadric(),

	InitTestSpecificInfo = TestSpecificInfo#my_test_gui_info{
		client_widget=GLCanvas,
		mesh=TestMesh,
		angle=0.0,
		material_texture=MatTexture,
		alpha_texture=AlphaTexture,
		text_texture=TextTexture,
		clock_texture=ClockTexture,
		font=Font,
		brush=Brush,
		sphere=SphereId },

	InitAppGUIState = AppGUIState#app_gui_state{
		opengl_base_state={ initialised, GLCanvas, GLContext },
		app_specific_info=InitTestSpecificInfo },

	% As the initial onResized was triggered whereas no OpenGL state was
	% already available, we trigger a new rendering now.
	%
	% (we do not call the test_onResized_driver/2 from this driver, as they are
	% to be parametrised with different event elements; this is the purpose of
	% on_main_frame_resized/1)
	%
	ResizedAppGUIState = on_main_frame_resized( InitAppGUIState ),

	{ _MaybeAppEventPair=undefined, ResizedAppGUIState }.



% Overrides default_onRepaintNeeded_driver/2:
test_onRepaintNeeded_driver(
		_Elements=[ _GLCanvas, _GLCanvasId, _EventContext ],
		AppGUIState=#app_gui_state{
			opengl_base_state={ _GLStatus=uninitialised, _SecondGLCanvas,
								_GLContext } } ) ->

	%trace_utils:debug_fmt( "Test GL canvas ~w to be repainted, "
	%   "however OpenGL is not initialised yet.", [ GLCanvas ] ),

	{ _MaybeAppEventPair=undefined, AppGUIState };


test_onRepaintNeeded_driver( _Elements=[ GLCanvas, _GLCanvasId, _EventContext ],
		AppGUIState=#app_gui_state{
			opengl_base_state={ _GLStatus=initialised, SameGLCanvas,
								_GLContext },
			app_specific_info=TestSpecificInfo } ) ->

	%trace_utils:debug_fmt( "Test GL canvas ~w to be repainted, "
	%   "which can be done as OpenGL is already initialised.", [ GLCanvas ] ),

	basic_utils:assert_equal( GLCanvas, SameGLCanvas ),

	% A rendering is not strictly necessary in this case, as anyway a regular
	% redraw is to happen soon afterwards.

	gui_widget:enable_repaint( GLCanvas ),

	% Includes the GL flushing and the buffer swaping:
	%
	% (no extra element needed, and no change operated on this term)
	%
	render( TestSpecificInfo ),

	{ _MaybeAppEventPair=undefined, AppGUIState }.



-doc """
The test-specific event driver for the onResized (user) event type.

Its type is event_driver().
""".
-spec test_onResized_driver( event_elements(), app_gui_state() ) ->
											app_event_return().
test_onResized_driver( _Elements=[ _ParentWindow, _ParentWindowId,
								   NewParentSize, _EventContext ],
		AppGUIState=#app_gui_state{
			opengl_base_state={ uninitialised, GLCanvas, _GLContext } } ) ->

	% For a window, the first resizing event happens (just) before its onShown
	% one: not ready yet (first onResized, before onShown).

	trace_utils:debug_fmt( "Test GL canvas ~w to be resized to ~w, "
		"however OpenGL is not initialised yet.", [ GLCanvas, NewParentSize ] ),

	{ _MaybeAppEventPair=undefined, AppGUIState };


test_onResized_driver( _Elements=[ _ParentWindow, _ParentWindowId,
								   _NewParentSize, _EventContext ],
		AppGUIState=#app_gui_state{
			opengl_base_state={ initialised, _GLCanvas, _GLContext } } ) ->

	ResizedAppGUIState = on_main_frame_resized( AppGUIState ),

	{ _MaybeAppEventPair=undefined, ResizedAppGUIState }.


% Is actually a frame:
test_onWindowClosed(
		Elements=[ ParentWindow, _ParentWindowId, _EventContext ],
		AppGUIState ) ->

	trace_utils:info_fmt( "Main frame ~w closed, test success.",
						  [ ParentWindow ] ),

	% Very final check, while there is still an OpenGL context:
	gui_opengl:check_error(),

	gui_window:destruct( ParentWindow ),

	BaseGUIEvent = { onWindowClosed, Elements },

	AppEventPair = { quit_requested, BaseGUIEvent },

	{ AppEventPair, AppGUIState }.




-doc """
Managing a resizing of the main frame.

Defined as a separate function, as to be called from two contexts: when the main
frame is shown and when a resizing is needed.

OpenGL context expected here to have already been set.
""".
-spec on_main_frame_resized( app_gui_state() ) -> app_gui_state().
on_main_frame_resized( GUIState=#app_gui_state{
		opengl_base_state={ initialised, GLCanvas, _GLContext },
		app_specific_info=#my_test_gui_info{ panel=Panel } } ) ->

	% Maximises widgets in their respective area:

	% First, panel in main frame:
	gui_widget:maximise_in_parent( Panel ),

	% Then OpenGL canvas in panel:
	{ CanvasWidth, CanvasHeight } = gui_widget:maximise_in_parent( GLCanvas ),

	%trace_utils:debug_fmt( "New client canvas size: {~B,~B}.",
	%                       [ CanvasWidth, CanvasHeight ] ),

	% Lower-left corner and size of the viewport in the current window:
	gl:viewport( 0, 0, CanvasWidth, CanvasHeight ),

	% Apparently, at least on a test setting, a race condition (discovered
	% thanks to the commenting-out of a debug trace) seems to exist between the
	% moment when the canvas is resized and the one when a new OpenGL rendering
	% is triggered afterwards; the cause is probably that maximising involves an
	% (Erlang) asynchronous message to be sent from this user process and to be
	% received and applied by the process of the target window, whereas a GL
	% (NIF-based) operation is immediate; without a sufficient delay, the
	% rendering will thus take place according to the former (e.g. minimised)
	% canvas size, not according to the one that was expected to be already
	% resized.
	%
	gui_widget:sync( GLCanvas ),

	gl:matrixMode( ?GL_PROJECTION ),

	gl:loadIdentity(),

	Left = -2.0,
	Bottom = -2.0 * CanvasHeight / CanvasWidth,
	Near = -20.00,
	gl:ortho( Left, _Right=-Left, Bottom, _Top=-Bottom, Near, _Far=-Near ),

	gl:matrixMode( ?GL_MODELVIEW ),
	gl:loadIdentity(),

	cond_utils:if_defined( myriad_check_opengl_support,
						   gui_opengl:check_error() ),

	% Includes the swapping of buffers:
	update_rendering( GUIState ).



-doc """
Updates the rendering.

Expected to be called periodically.
""".
-spec update_rendering( app_gui_state() ) -> app_gui_state().
update_rendering( GUIState=#app_gui_state{
		app_specific_info=AppSpecificInfo=#my_test_gui_info{
			time=PreviousTime,
			angle=Angle } } ) ->

	%trace_utils:debug( "Updating rendering." ),

	% First update the state needed:

	NewTime = time_utils:get_local_time(),
	NewAngle = Angle + 1.0,

	TimeAppSpecificInfo = case NewTime of

		% Still in the same second:
		PreviousTime ->
			AppSpecificInfo;

		% Time changed:
		_ ->
			update_clock_texture( NewTime, AppSpecificInfo )

	end,

	% Then call OpenGL accordingly:
	render( TimeAppSpecificInfo ),

	UpdatedAppSpecificInfo = TimeAppSpecificInfo#my_test_gui_info{
		time=NewTime,
		angle=NewAngle },

	GUIState#app_gui_state{ app_specific_info=UpdatedAppSpecificInfo }.



-doc "Updates the texture of the clock according to the specified time.".
-spec update_clock_texture( time(), my_test_gui_info() ) -> my_test_gui_info().
update_clock_texture( Time, TestGUIInfo=#my_test_gui_info{
		clock_texture=ClockTexture,
		font=Font,
		brush=Brush } ) ->

	gui_texture:destruct( ClockTexture ),
	NewClockTexture = get_clock_texture( Time, Font, Brush ),
	TestGUIInfo#my_test_gui_info{ clock_texture=NewClockTexture }.



-doc "Returns a texture corresponding to the specified clock time.".
-spec get_clock_texture( time(), font(), brush() ) -> texture().
get_clock_texture( Time, Font, Brush ) ->

	TimeStr = time_utils:time_to_string( Time ),

	gui_texture:create_from_text( TimeStr, Font, Brush,
		_OrangeTextColor={ 255, 40, 40 }, _Flip=true ).



-doc """
Performs a ("pure OpenGL") rendering, based on the specified (const)
OpenGL-based GUI information.
""".
-spec render( my_test_gui_info() ) -> void().
render( #my_test_gui_info{ client_widget=GLCanvas,
						   mesh=CubeMesh,
						   angle=Angle,
						   material_texture=MatTexture,
						   alpha_texture=AlphaTexture,
						   text_texture=TextTexture,
						   clock_texture=ClockTexture,
						   sphere=SphereId } ) ->

	%trace_utils:debug( "Rendering now." ),

	% See also https://www.khronos.org/opengl/wiki/Common_Mistakes#Swap_Buffers:
	gl:clear( ?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT ),

	gl:matrixMode( ?GL_MODELVIEW ),
	gl:loadIdentity(),
	gl:pushMatrix(),
	gl:translatef( 0.0, 0.5, 0.0 ),

	% In degrees, around the first diagonal axis:
	%
	% (matrix recreated from scratch rather than being updated, to avoid the
	% accumulation of numerical errors)
	%
	gl:rotatef( Angle, _X=1.0, _Y=1.0, _Z=1.0 ),

	gui_texture:set_as_current( MatTexture ),
	gl:disable( ?GL_BLEND ),

	cond_utils:if_defined( myriad_check_opengl, gui_opengl:check_error() ),

	% Specifies a texture environment:
	gl:texEnvi( ?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE ),

	gl:disable( ?GL_CULL_FACE ),

	gui_opengl:render_mesh( CubeMesh ),

	gl:popMatrix(),


	% Modified texture environment:
	gl:texEnvi( ?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE ),

	cond_utils:if_defined( myriad_check_opengl, gui_opengl:check_error() ),

	gui_opengl:enter_2d_mode( GLCanvas ),

	{ Width, Height } = gui_widget:get_client_size( GLCanvas ),

	Move = abs( 90 - ( trunc( Angle ) rem 180 ) ),

	gui_texture:render( ClockTexture, _Xc=(Width div 2) - 50,
		_Yc=(Height div 2) - 130 + Move ),

	gui_texture:render( AlphaTexture, _Xa=(Width div 2) - 80,
		_Ya=(Height div 2) - Move ),

	gui_opengl:leave_2d_mode(),

	gl:pushMatrix(),
	gl:enable( ?GL_CULL_FACE ),
	gl:enable( ?GL_BLEND ),
	gl:blendFunc( ?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA ),
	gl:translatef( 0.0, -0.8, 0.0 ),
	gui_texture:set_as_current( TextTexture ),

	% Texture coordinates should be generated:
	glu:quadricTexture( SphereId, ?GLU_TRUE ),
	glu:quadricNormals( SphereId, ?GLU_SMOOTH ),
	glu:quadricDrawStyle( SphereId, ?GLU_FILL ),
	glu:quadricOrientation( SphereId, ?GLU_OUTSIDE ),
	gl:scalef( 1.5, 0.6, 1.0 ),
	gl:rotatef( -90.0, 1.0, 0.0, 0.0 ),
	gl:rotatef( -Angle, 0.0, 0.0, 1.0 ),
	glu:sphere( SphereId, 0.8, 50, 40 ),
	gl:popMatrix(),

	cond_utils:if_defined( myriad_check_opengl, gui_opengl:check_error() ),

	% Can be done here, as window-related (actually: GLCanvas) information were
	% already necessary anyway; includes a gl:flush/0:
	%
	gui_opengl:swap_buffers( GLCanvas ).



-doc "Runs the test.".
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	gui_opengl_for_testing:can_be_run(
			"the applicative integration test of OpenGL support" ) =:= yes
		andalso run_actual_test(),

	test_facilities:stop().
