% Copyright (C) 2021-2026 Olivier Boudeville
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
% Creation date: 2021.

-module(gui_opengl_direct_integration_test).

-moduledoc """
Testing the **OpenGL support**, as an integration test, designated as "direct"
since directly listening to incoming event messages, that is not using
application events and higher-level applicative GUI states (see app_gui_state())
like gui_opengl_applicative_integration_test.erl does.

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
% As the OpenGL canvas is not resized when its containers are resized, we listen
% to the resizing of the parent window and adapt accordingly.


% For GL/GLU defines; the sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").



% For silencing:
-export([ update_clock_texture/2, render/1 ]).


% The duration, in milliseconds, between two updates of the OpenGL rendering:
%
% (hence presumably at 50 Hz)
%
-define( interframe_duration, 20 ).


% This is the test-specific overall GUI state:
-record( my_gui_state, {

    % The main frame of this test:
    main_frame :: frame(),

    % The panel occupying the client area of the main frame:
    panel :: panel(),

    % The OpenGL canvas on which rendering will be done:
    canvas :: gl_canvas(),

    % The OpenGL context being used:
    context :: gl_context(),

    % An image used as a material:
    image :: image(),

    % The various OpenGL information kept by this test once initialised:
    opengl_state :: option( my_opengl_state() ),

    % Records the current time to update the clock texture when relevant:
    time :: option( time() ),

    mesh :: mesh() } ).


-doc "Test-specific overall GUI state.".
-type my_gui_state() :: #my_gui_state{}.



% OpenGL-specific GUI test state:
-record( my_opengl_state, {

    % The widget where the rendering is to occur, typically the GL canvas:
    render_target :: frame(),

    mesh :: mesh(),

    angle = 0.0 :: degrees(),

    material_texture :: texture(),

    alpha_texture :: texture(),

    text_texture :: texture(),

    clock_texture :: texture(),

    font :: font(),

    brush :: brush(),

    sphere :: glu_id() } ).


-doc "OpenGL-specific GUI test state.".
-type my_opengl_state() :: #my_opengl_state{}.




% Type shorthands:

-type time() :: time_utils:time().

-type degrees() :: unit_utils:degrees().

-type frame() :: gui_frame:frame().
-type panel() :: gui_panel:panel().
-type image() :: gui_image:image().
-type font() :: gui_font:font().
-type brush() :: gui_render:brush().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().

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

    % Postpone the processing of first events to accelerate initial setup:
    InitialGUIState = gui:batch( fun() -> init_test_gui() end ),
    %InitialGUIState = init_test_gui(),

    gui_frame:show( InitialGUIState#my_gui_state.main_frame ),

    % Uncomment to check that a no_gl_context error report is triggered indeed,
    % as expected (as no current GL context exists yet):
    %
    %gl:viewport( 0, 0, 50, 50 ),

    % OpenGL will be initialised only when the corresponding frame will be ready
    % (that is once first reported as resized):
    %
    gui_main_loop( InitialGUIState ),

    gui:stop().



-doc """
Creates the initial test GUI: a main frame containing a panel to which an OpenGL
canvas is associated, in which an OpenGL context is created.
""".
-spec init_test_gui() -> my_gui_state().
init_test_gui() ->

    MainFrame = gui_frame:create( "MyriadGUI OpenGL Direct Integration Test" ),

    Panel = gui_panel:create( MainFrame ),

    GLCanvas = gui_opengl:create_canvas( _Parent=Panel ),

    % Created, yet not bound yet (must wait for the main frame to be shown):
    GLContext = gui_opengl:create_context( GLCanvas ),

    gui:subscribe_to_events( { [ onShown, onResized, onWindowClosed ],
                               MainFrame } ),

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

    TestMesh = gui_opengl_for_testing:get_test_colored_cube_mesh(
        _EdgeLength=1.0, _FaceGranularity=per_vertex ),

    trace_utils:debug_fmt( "The test mesh is a ~ts",
                           [ mesh:to_string( TestMesh ) ] ),

    %TestMesh = gui_opengl_for_testing:get_test_tetra_mesh(),

    % No OpenGL state yet (GL context cannot be set as current yet):
    #my_gui_state{ main_frame=MainFrame,
                   panel=Panel,
                   canvas=GLCanvas,
                   context=GLContext,
                   image=Image,
                   mesh=TestMesh }.



-doc """
The main loop of this test, driven by the receiving of MyriadGUI messages.
""".
-spec gui_main_loop( my_gui_state() ) -> void().
gui_main_loop( GUIState ) ->

    %trace_utils:debug( "Main loop." ),

    % Matching the least-often received messages last:
    receive

        % Not strictly necessary, as anyway a regular redraw is to happen soon
        % afterwards:
        %
        { onRepaintNeeded, [ GLCanvas, _GLCanvasId, _EventContext ] } ->

            %trace_utils:debug_fmt( "Repaint needed for OpenGL canvas ~w.",
            %                       [ GLCanvas ] ),

            RepaintedGUIState = case GUIState#my_gui_state.opengl_state of

                % Not ready yet:
                undefined ->
                    trace_utils:debug(
                        "To be repainted, yet no OpenGL state yet." ),
                    GUIState;

                GLState ->
                    gui_widget:enable_repaint( GLCanvas ),
                    % Includes the GL flushing and the buffer swaping:
                    render( GLState ),
                    GUIState

            end,
            gui_main_loop( RepaintedGUIState );


        % For a window, the first resizing event happens (just) before its
        % onShown one:
        %
        { onResized, [ _ParentWindow, _ParentWindowId, _NewParentSize,
                       _EventContext ] } ->

            %trace_utils:debug_fmt( "Resizing of the parent window "
            %   (main frame) "to ~w detected.", [ NewParentSize ] ),

            ResizedGUIState = case GUIState#my_gui_state.opengl_state of

                % Not ready yet (first onResized, before onShown):
                undefined ->
                    trace_utils:debug( "Resized, yet no OpenGL state yet." ),
                    GUIState;

                _ ->
                    on_main_frame_resized( GUIState )

            end,

            gui_main_loop( ResizedGUIState );


        % The most suitable first location to initialise OpenGL, as making a GL
        % context current requires a shown window:
        %
        { onShown, [ ParentWindow, _ParentWindowId, _EventContext ] } ->

            trace_utils:debug_fmt( "Parent window (main frame) just shown "
                "(initial size of ~w).",
                [ gui_widget:get_size( ParentWindow ) ] ),

            % Optional, yet better:
            gui:unsubscribe_from_events( { onShown, ParentWindow } ),

            % Done once for all:
            InitGUIState = initialise_opengl( GUIState ),

            test_facilities:display( "Reported OpenGL settings: "
                "vendor is '~ts', renderer is '~ts'; OpenGL version is '~ts', "
                "and the one of the shading language is '~ts'.",
                [ gui_opengl:get_vendor_name(), gui_opengl:get_renderer_name(),
                  text_utils:version_to_string( gui_opengl:get_version() ),
                  gui_shader:get_shading_language_version() ] ),

            gui_main_loop( InitGUIState );


        { onWindowClosed, [ ParentFrame, _ParentWindowId, _EventContext ] } ->
            trace_utils:info( "Main frame closed, test success." ),

            % Very final check, while there is still an OpenGL context:
            gui_opengl:check_error(),

            gui_frame:destruct( ParentFrame );


        OtherEvent ->
            trace_utils:warning_fmt( "Test ignored following event:~n ~p",
                                     [ OtherEvent ] ),

            gui_main_loop( GUIState )


    % As this GUI is to be updated even in the absence of user actions:
    after ?interframe_duration ->

        RenderGUIState = case GUIState#my_gui_state.opengl_state of

            undefined ->
                trace_utils:debug( "(not ready yet)" ),
                GUIState;

            _ ->
                update_rendering( GUIState )

        end,

        gui_main_loop( RenderGUIState )

    end.



-doc """
Sets up OpenGL, once for all, once a proper OpenGL context is available.
""".
-spec initialise_opengl( my_gui_state() ) -> my_gui_state().
initialise_opengl( GUIState=#my_gui_state{ canvas=GLCanvas,
                                           context=GLContext,
                                           image=Image,
                                           % Check:
                                           opengl_state=undefined } ) ->

    % Initial size of canvas is typically 20x20 pixels:
    Size = gui_widget:get_client_size( GLCanvas ),

    trace_utils:debug_fmt( "Initialising OpenGL "
        "(whereas canvas is of initial size ~w).", [ Size ] ),

    % So done only once:
    gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

    % These settings will not change afterwards (set once for all):

    gui_texture:set_basic_general_settings(),

    gl:enable( ?GL_DEPTH_TEST ),
    gl:depthFunc( ?GL_LESS ),

    % Solid white:
    gl:clearColor( 1.0, 1.0, 1.0, 1.0 ),

    MatTexture = gui_texture:create_from_image( Image ),

    AlphaTexture = gui_texture:load_from_file(
        gui_opengl_for_testing:get_logo_image_path() ),

    Font = gui_font:create( _PointSize=32, _Family=default_font_family,
                            _Style=normal, _Weight=bold ),

    Brush = gui_render:create_brush( _BlackRGB={ 0, 0, 0 } ),

    % Myriad RGB dark blue:
    TextColor = { 0, 39, 165 },

    TextTexture = gui_texture:create_from_text(
        "MyriadGUI rocks!", Font, Brush, TextColor, _Flip=true ),

    ClockTexture =
        get_clock_texture( time_utils:get_local_time(), Font, Brush ),


    TestMesh = gui_opengl_for_testing:get_test_colored_cube_mesh(
        _EdgeLength=1.0, _FaceGranularity=per_vertex ),

    %TestMesh = gui_opengl_for_testing:get_test_tetra_mesh(),

    trace_utils:debug_fmt( "Test mesh: ~ts.", [ mesh:to_string( TestMesh ) ] ),

    SphereId = glu:newQuadric(),

    InitialGLState = #my_opengl_state{ render_target=GLCanvas,
                                       mesh=TestMesh,
                                       angle=0.0,
                                       material_texture=MatTexture,
                                       alpha_texture=AlphaTexture,
                                       text_texture=TextTexture,
                                       clock_texture=ClockTexture,
                                       font=Font,
                                       brush=Brush,
                                       sphere=SphereId },

    InitGUIState = GUIState#my_gui_state{ opengl_state=InitialGLState },

    % As the initial onResized was triggered whereas no OpenGL state was
    % already available:
    %
    on_main_frame_resized( InitGUIState ).



-doc """
Managing a resizing of the main frame.

OpenGL context expected here to have already been set.
""".
-spec on_main_frame_resized( my_gui_state() ) -> my_gui_state().
on_main_frame_resized( GUIState=#my_gui_state{ panel=Panel,
                                               canvas=GLCanvas } ) ->

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
-spec update_rendering( my_gui_state() ) -> my_gui_state().
update_rendering( GUIState=#my_gui_state{ opengl_state=GLState,
                                          time=PreviousTime } ) ->

    %trace_utils:debug( "Updating rendering." ),

    % First update the state needed:

    NewAngle = GLState#my_opengl_state.angle + 1.0,

    AngleGLState = GLState#my_opengl_state{ angle=NewAngle },

    NewTime = time_utils:get_local_time(),

    NewGLState = case NewTime of

        % Still in the same second:
        PreviousTime ->
            AngleGLState;

        % Time changed:
        _ ->
            update_clock_texture( NewTime, AngleGLState )

    end,

    % Then call OpenGL accordingly:
    render( NewGLState ),

    GUIState#my_gui_state{ opengl_state=NewGLState, time=NewTime }.



-doc """
Updates the texture of the clock according to the specified time.
""".
-spec update_clock_texture( time(), my_opengl_state() ) ->
                                                my_opengl_state().
update_clock_texture( Time, GLState=#my_opengl_state{
        clock_texture=ClockTexture, font=Font, brush=Brush } ) ->

    gui_texture:destruct( ClockTexture ),
    NewClockTexture = get_clock_texture( Time, Font, Brush ),
    GLState#my_opengl_state{ clock_texture=NewClockTexture }.



-doc "Returns a texture corresponding to the specified clock time.".
-spec get_clock_texture( time(), font(), brush() ) -> texture().
get_clock_texture( Time, Font, Brush ) ->

    TimeStr = time_utils:time_to_string( Time ),

    gui_texture:create_from_text( TimeStr, Font, Brush,
        _OrangeTextColor={ 255, 40, 40 }, _Flip=true ).



-doc """
Performs a ("pure OpenGL") rendering, based on the specified (const) OpenGL
state.
""".
-spec render( my_opengl_state() ) -> void().
render( #my_opengl_state{ render_target=Widget,
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

    % No effect:
    %gl:disable( ?GL_CULL_FACE ),
    gl:enable( ?GL_CULL_FACE ),

    % This is not the support for modern OpenGL; here, with legacy operations,
    % the gui_texture actual coordinates are not used, leading to textures
    % covering only partially the mesh surfaces (as textures have been extended
    % so that their dimensions are power of two, but 1.0 UV coordinates are
    % used) and a (black) background to be seen:
    %
    gui_opengl:render_mesh( CubeMesh ),

    gl:popMatrix(),


    % Modified texture environment:
    gl:texEnvi( ?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE ),

    cond_utils:if_defined( myriad_check_opengl, gui_opengl:check_error() ),

    gui_opengl:enter_2d_mode( Widget ),

    { Width, Height } = gui_widget:get_client_size( Widget ),

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
    gui_opengl:swap_buffers( Widget ).



-doc "Runs the test.".
-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    gui_opengl_for_testing:can_be_run(
            "the integration test of OpenGL support" ) =:= yes
        andalso run_actual_test(),

    test_facilities:stop().
