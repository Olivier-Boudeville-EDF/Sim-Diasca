% Copyright (C) 2024-2026 Olivier Boudeville
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
% Creation date: Monday, April 8, 2024.

-module(gui_opengl_include_shader_test).

-moduledoc """
Testing of the feature for basic **header include** for shaders.
""".



% For GL/GLU defines; the sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").



% Test-specific overall GUI state:
%
% (no OpenGL-specific state to store, like vertices, textures or alike)
%
-record( my_gui_state, {

    % The main frame of this test:
    main_frame :: frame(),

    % The OpenGL canvas on which rendering will be done:
    canvas :: gl_canvas(),

    % The OpenGL context being used:
    context :: gl_context(),

    % In more complex cases, would store the loaded textures, etc.
    opengl_state :: option( my_opengl_state() ) } ).


-doc "Test-specific overall GUI state.".
-type my_gui_state() :: #my_gui_state{}.



-record( my_opengl_state, {

    % The identifier of our GLSL program:
    program_id :: program_id()

                          } ).


-doc """
Test-specific overall OpenGL state.

Storing VBOs and EBOs is probably only of use in order to deallocate them
properly once not needed anymore.
""".
-type my_opengl_state() :: #my_opengl_state{}.



% Tupe shorthands:

-type frame() :: gui:frame().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().

-type program_id() :: gui_shader:program_id().


% As we use the same vertex shaders for the triangle and the square, both have
% to specify vertices and texture coordinates.


% The attribute in the vertex stream that will be passed to the our (vertex)
% shader for the vertices; attribute 0 was chosen, yet no particular reason for
% this index, it just must match the layout (cf. 'location = 0') in the shader.
%
-define( my_vertex_attribute_index, 0 ).

% The attribute in the vertex stream that will be passed to the our (vertex)
% shader for the texture coordinates.
%
-define( my_texture_coords_attribute_index, 1 ).




-doc "Runs the actual test.".
-spec run_actual_test() -> void().
run_actual_test() ->

    test_facilities:display( "This test checks that includes within shaders "
                             "work as expected." ),

    gui:start(),

    % Could be batched (see gui:batch/1) to be more effective:
    InitialGUIState = init_test_gui(),

    gui_frame:show( InitialGUIState#my_gui_state.main_frame ),

    % OpenGL will be initialised only when the corresponding frame will be ready
    % (that is once first reported as resized):
    %
    gui_main_loop( InitialGUIState ),

    gui:stop().



-doc """
Creates the initial test GUI: a main frame containing an OpenGL canvas to which
an OpenGL context is associated.

Once the rendering is done, the buffers are swapped, and the content is
displayed.
""".
-spec init_test_gui() -> my_gui_state().
init_test_gui() ->

    MainFrame = gui_frame:create( "MyriadGUI OpenGL Shader-based Texture Test",
                                  _Size={ 1024, 768 } ),

    % Using mostly default GL attributes:
    GLCanvasAttrs =
        [ use_core_profile | gui_opengl:get_default_canvas_attributes() ],

    GLCanvas = gui_opengl:create_canvas(
        _CanvasOpts=[ { gl_attributes, GLCanvasAttrs } ], _Parent=MainFrame ),

    % Created, yet not bound yet (must wait for the main frame to be shown):
    GLContext = gui_opengl:create_context( GLCanvas ),

    gui:subscribe_to_events( { [ onShown, onWindowClosed ], MainFrame } ),

    % No OpenGL state yet (GL context cannot be set as current yet), actual
    % OpenGL initialisation to happen when available, i.e. when the main frame
    % is shown:
    %
    #my_gui_state{ main_frame=MainFrame, canvas=GLCanvas, context=GLContext }.



-doc """
The main loop of this test, driven by the receiving of MyriadGUI messages.
""".
-spec gui_main_loop( my_gui_state() ) -> void().
gui_main_loop( GUIState ) ->

    %trace_utils:debug( "Main loop." ),

    % Matching the least-often received messages last:
    receive

        % Less frequent messages looked up last:

        % The most suitable first location to initialise OpenGL, as making a GL
        % context current requires a shown window:
        %
        { onShown, [ ParentFrame, _ParentFrameId, _EventContext ] } ->

            trace_utils:debug_fmt( "Parent window (main frame) just shown "
                "(initial size of ~w).",
                [ gui_widget:get_size( ParentFrame ) ] ),

            % Optional yet better:
            gui:unsubscribe_from_events( { onShown, ParentFrame } ),

            % Done once for all:
            _InitGUIState = initialise_opengl( GUIState ),

            % Test over, no more recursing:
            gui_frame:destruct( ParentFrame );



        { onWindowClosed, [ ParentFrame, _ParentFrameId, _EventContext ] } ->

            cleanup_opengl( GUIState ),
            trace_utils:info( "Main frame closed, test success." ),

            % Very final check, while there is still an OpenGL context:
            gui_opengl:check_error(),

            % No more recursing:
            gui_frame:destruct( ParentFrame );


        OtherEvent ->
            trace_utils:warning_fmt( "Test ignored following event:~n ~p",
                                     [ OtherEvent ] ),

            gui_main_loop( GUIState )

    % No 'after': no spontaneous action taken here, in the absence of events.

    end.



-doc """
Sets up OpenGL, once for all (regardless of next resizings), once a proper
OpenGL context is available.
""".
-spec initialise_opengl( my_gui_state() ) -> my_gui_state().
initialise_opengl( _GUIState=#my_gui_state{ canvas=GLCanvas,
                                            context=GLContext,
                                            opengl_state=undefined } ) ->

    % Initial size of canvas is typically 20x20 pixels:
    trace_utils:debug_fmt( "Initialising OpenGL (whereas canvas is of initial "
                           "size ~w).", [ gui_widget:get_size( GLCanvas ) ] ),

    % So done only once, with appropriate measures for a first setting:
    gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

    % First possible moment:
    test_facilities:display( "Description of the current OpenGL support: ~ts",
                             [ gui_opengl:get_support_description() ] ),

    % These test shaders are in 3.3 core (cf. their '#version 330 core'):
    MinOpenGLVersion = { 3, 3 },
    %MinOpenGLVersion = { 4, 6 },
    %MinOpenGLVersion = { 99, 0 },

    % Not found available at least in some configurations:
    %TargetProfile = core,

    TargetProfile = compatibility,
    %TargetProfile = non_existing_profile,

    %RequiredExts = [ non_existing_extension ],
    %RequiredExts = [ 'GL_ARB_draw_buffers' ],
    RequiredExts = [],

    gui_opengl:check_requirements( MinOpenGLVersion, TargetProfile,
                                   RequiredExts ),

    % These settings will not change afterwards here (hence set once for all):

    % Clears in white (otherwise black background):
    gl:clearColor( _R=1.0, _G=1.0, _B=1.0, ?alpha_fully_opaque ),

    % Specifies the location of the vertex attributes, so that the vertex shader
    % will be able to match its input variables with the vertex attributes of
    % the application:
    %
    UserVertexAttrs = [
        { "my_input_vertex",    ?my_vertex_attribute_index },
        { "my_input_tex_coord", ?my_texture_coords_attribute_index } ],

    % For test_include_shader.glsl.h (MyriadGUI ones automatically found):
    IncludeSearchDirs = [ "." ],

    % Creates, compiles and links our GLSL program from the two specified
    % shaders, that are, in the same movement, automatically attached and
    % linked, then detached and deleted:
    %
    ProgramId = gui_shader:generate_program_from(
        "gui_opengl_include_shader.vertex.glsl",
        "gui_opengl_include_shader.fragment.glsl", UserVertexAttrs,
        IncludeSearchDirs ),


    % Rely on our shaders; can be used from now:
    gui_shader:install_program( ProgramId ),

    test_facilities:display(
      "Shader fully built and installed, test success." ).



-doc "Cleans up OpenGL.".
-spec cleanup_opengl( my_gui_state() ) -> void().
cleanup_opengl( #my_gui_state{ opengl_state=undefined } ) ->
    ok;

cleanup_opengl( #my_gui_state{ opengl_state=#my_opengl_state{
                                                program_id=ProgramId } } ) ->

    trace_utils:debug( "Cleaning up OpenGL." ),

    gui_shader:delete_program( ProgramId ).



-doc "Runs the test.".
-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    gui_opengl_for_testing:can_be_run(
            "the test of texture support with OpenGL shaders" ) =:= yes
        andalso run_actual_test(),

    test_facilities:stop().
