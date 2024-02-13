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
% Creation date: Sunday, April 9, 2023.


% @doc Minimal testing of <b>shader-based transformation rendering</b>: applies
% a transformation matrix created from the application and displays a textured
% square based on it that can be moved with the keyboard to test transformations
% (translations, rotations and shearings) and directions thereof in the current
% referential.
%
% This test relies on shaders and thus on modern versions of OpenGL (e.g. 3.3),
% as opposed to the compatibility mode for OpenGL 1.x, and on Myriad's
% conventions (e.g. a Z-UP referential).
%
-module(gui_opengl_transformation_shader_test).


% Implementation notes:
%
% Inspired from https://learnopengl.com/Getting-started/Transformations.
%
% Here, as we used NDC (normalised coordinates), no need to render based on the
% dimensions of the canvas.
%
% Refer to https://myriad.esperide.org/#geometric-conventions to better
% understand the referential and transformations involved.
%
% A Myriad-textured square will be initially located "centered on the floor":
% its center will be the origin, it will belong to the Z=0 plane (zero altitude)
% and its axes will be parallel to the X and Y ones.
%
% The camera will be located at the origin, looking towards the -Z axis
% ("downward"), and its up direction will be the Y axis.
%
% No specific lighting applies.
%
% Refer to myriad-opengl-transformation-setting.png for a (Blender-based) view
% of the initial setting of this test.

% An improvement could be not to texture the backside of the square (just
% rendered as pure black); easily done with legacy OpenGL, maybe less with
% shaders.

% With a square of null thickness, shearing along the Z axis will not change
% anything.

% Due to how 4x4 matrix multiplication works, the translations will be applied
% last, whereas rotations and shearings will happen in their reverse
% specification order. As a result, for example if rotating the square whereas
% it has been translated far away from the origin, it will not rotate around the
% origin, but on itself, in its local referential (around its center).


% For GL/GLU defines; the sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").

-include_lib("myriad/include/matrix4.hrl").
-include_lib("myriad/include/projection.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").


-type transformation_mode() :: 'translation' | 'rotation' | 'shearing'.


% Test-specific overall GUI state:
%
% (no OpenGL-specific state to store, like vertices, textures or alike)
%
-record( my_gui_state, {

	% The main frame of this test:
	main_frame :: frame(),

	% The OpenGL canvas on which rendering will be done:
	canvas :: gl_canvas(),

	% The aspect ratio of that canvas:
	aspect_ratio :: aspect_ratio(),

	% The OpenGL context being used:
	context :: gl_context(),

	% The image as loaded from file, to be transformed in a texture:
	image :: image(),

	% Currently, we directly update (translate, rotate, etc.) the previous
	% model-view matrix based on requested the changes; this is prone to the
	% accumulation of rounding errors, hence a better practice would be to
	% recompute the model-view matrix from the next higher-level parameters:

	% The 3D position of the center of the model (textured square) in the world
	% referential:
	%
	%center_pos :: point3(),

	% The angle of the model (textured square) along the X axis of the world
	% referential:
	%
	%x_angle :: radians(),

	% The angle of the model (textured square) along the Y axis of the world
	% referential:
	%
	%y_angle :: radians(),

	% The angle of the model (textured square) along the Z axis of the world
	% referential:
	%
	%z_angle :: radians(),


	% The model-view matrix for the square of interest:
	model_view :: matrix4(),

	% The currect projection settings that apply:
	projection_settings :: projection_settings(),

	% The corresponding projection matrix of interest:
	projection :: matrix4(),

	% The currently active transformation mode (translation, rotation,
	% or shearing):
	%
	transformation_mode :: transformation_mode(),

	% In more complex cases, would store the loaded textures, etc.:
	opengl_state :: maybe( my_opengl_state() ) } ).

-type my_gui_state() :: #my_gui_state{}.
% Test-specific overall GUI state.



-record( my_opengl_state, {

	% The identifier of our GLSL program:
	program_id :: program_id(),

	% Needs an OpenGL context:
	texture :: maybe( texture() ),

	% The identifier of the Model-View uniform matrix:
	model_view_id :: uniform_id(),

	% The identifier of the Projection uniform matrix:
	projection_id :: uniform_id(),

	% For the square, which has indexed coordinates:

	square_vao_id :: vao_id(),

	square_vertex_count :: vertex_count(),


	% Stored only so that they can be deallocated once the test is over:

	% The VBO concentrating vertices and texture coordinates:
	square_merged_vbo_id :: vbo_id(),

	% Indices for the vertex:
	square_ebo_id :: ebo_id() } ).

-type my_opengl_state() :: #my_opengl_state{}.
% Test-specific overall OpenGL state.
%
% Storing VBOs and EBOs is probably only of use in order to deallocate them
% properly once not needed anymore.


% Key bindings (Z-being-altitude conventions, i.e. Z-UP); note that the
% user-triggered movements are by default the ones of the model (the square),
% not the ones of the view (the camera), and that they are defined in absolute
% terms, relatively to the global referential (as opposed to, for example, based
% on the camera).
%
% First supposing that a keypad is available:

-define( has_keypad, true ).
%-define( has_keypad, false ).


-if( ?has_keypad =:= true ).

% X (abscissa in the Z-up referential) is controlled by left-right keypad
% numbers/arrows:

% Square moving along the +X axis (to the right on the screen, with the default
% camera) when hitting the key labelled "6" on keypad:
%
-define( increase_x_scan_code, ?MYR_SCANCODE_KP_6 ).

% Square moving along the -X axis (to the left on the screen, with the default
% camera) when hitting the key labelled "4" on keypad:
%
-define( decrease_x_scan_code, ?MYR_SCANCODE_KP_4 ).


% Y (depth in the Z-up referential)

% Square moving along the +Y axis (to the top of the screen, with the default
% camera) when hitting the key labelled "8" on keypad:
%
-define( increase_y_scan_code, ?MYR_SCANCODE_KP_8 ).

% Square moving along the -Y axis (to the bottom of the screen, with the default
% camera) when hitting the key labelled "2" on keypad:
%
-define( decrease_y_scan_code, ?MYR_SCANCODE_KP_2 ).


% Z (ordinate / altitude in the Z-up referential)

% Square moving along the +Z axis (from front to behind, with the default
% camera) when hitting the key labelled "9" on keypad:
%
-define( increase_z_scan_code, ?MYR_SCANCODE_KP_9 ).

% Square moving along the -Z axis (from behind to front, with the default
% camera) when hitting the key labelled "9" on keypad:
%
-define( decrease_z_scan_code, ?MYR_SCANCODE_KP_3 ).


% Re-center all:
-define( reset_scan_code, ?MYR_SCANCODE_KP_5 ).


% Switch to the next transformation mode:
-define( mode_switch_scan_code, ?MYR_SCANCODE_KP_ENTER ).



-else. % Not using keypad here:


% X (abscissa in the Z-up referential) is controlled by left-right keypad
% numbers/arrows:

% Square seen moving to the right with the default camera:
-define( increase_x_scan_code, ?MYR_SCANCODE_RIGHT ).

% To the left:
-define( decrease_x_scan_code, ?MYR_SCANCODE_LEFT ).


% Y (ordinate)

% Up:
-define( increase_y_scan_code, ?MYR_SCANCODE_UP ).

% Down:
-define( decrease_y_scan_code, ?MYR_SCANCODE_DOWN ).


% Z (depth/altitude)

% Moving nearer/upward:
-define( increase_z_scan_code, ?MYR_SCANCODE_PAGEUP ).

% Moving farther/downward:
-define( decrease_z_scan_code, ?MYR_SCANCODE_PAGEDOWN ).


% Re-center all:
-define( reset_scan_code, ?MYR_SCANCODE_SPACE ).


% Switch to the next transformation mode:
-define( mode_switch_scan_code, ?MYR_SCANCODE_RETURN ).

-endif. % has_keypad



-define( projection_mode_scan_code, ?MYR_SCANCODE_P ).

% End test:
-define( quit_scan_code, ?MYR_SCANCODE_ESCAPE ).




% An increment on a given dimension:
-define ( delta_coord, 0.1 ).

% An increment on a given angle, in degrees:
-define ( delta_angle, 2.0 ).

% A factor of a given scaling:
-define ( delta_scale, 0.1 ).


% Shorthands:

-type matrix4() :: matrix4:matrix4().
-type projection_settings() :: projection:projection_settings().

-type orthographic_settings() ::
	projection:orthographic_settings().

-type perspective_settings() ::
	projection:perspective_settings().

-type frame() :: gui_frame:frame().
-type aspect_ratio() :: gui:aspect_ratio().

-type image() :: gui_image:image().

-type scancode() :: gui_keyboard:scancode().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().

-type texture() :: gui_texture:texture().

-type program_id() :: gui_shader:program_id().
-type vao_id() :: gui_shader:vao_id().
-type vbo_id() :: gui_shader:vbo_id().
-type ebo_id() :: gui_shader:ebo_id().
-type uniform_id() :: gui_shader:uniform_id().

-type vertex_count() :: mesh:vertex_count().



% The attribute in the vertex stream that will be passed to the our (vertex)
% shader for the vertices; attribute 0 was chosen, yet no particular reason for
% this index, it just must match the layout (cf. 'location = 0') in the shader.
%
-define( my_vertex_attribute_index, 0 ).

% The attribute in the vertex stream that will be passed to the our (vertex)
% shader for the texture coordinates.
%
-define( my_texture_coords_attribute_index, 1 ).


% @doc Prepares all information needed to render the square, and returns them.
%
% Here a single VBO is used, merging the vertices and the texture coordinates;
% additionally an EBO is used.
%
-spec prepare_square( texture() ) -> { vao_id(), vbo_id(), ebo_id() }.
prepare_square( Texture ) ->

	% Creates the VAO context we need for the upcoming VBO (vertices and texture
	% coordinates) and EBO (for indices in the VBO):
	%
	SquareVAOId = gui_shader:set_new_vao(),

	% Half edge length:
	H = 0.5,

	Z = 0.0,

	% Square defined as [vertex3()], directly in normalized device coordinates
	% here, in the XY plane (Z=0); CCW order (bottom left, bottom right, top
	% right, top left):
	%
	%         S3--S2
	%         |    |
	%         S0--S1
	%
	SquareVertices = [ _SV2={  H,  H, Z }, _SV1={  H, -H, Z },
					   _SV0={ -H, -H, Z }, _SV3={ -H,  H, Z } ],

	O = 1.0,

	OrigSquareTexCoords = [ _STC2={ O, O }, _STC1={ O, Z },
							_STC0={ Z, Z }, _STC3={ Z, O } ],

	% To have correct texture coordinates in spite of padding:
	ActualSquareTexCoords = gui_texture:recalibrate_coordinates_for(
		OrigSquareTexCoords, Texture ),

	SquareAttrSeries= [ SquareVertices, ActualSquareTexCoords ],

	% Creates a VBO from these two series.
	%
	% We start at vertex attribute index #0 in this VAO; as there are two
	% series, the vertex attribute indices will be 0 and 1:
	%
	SquareMergedVBOId = gui_shader:assign_new_vbo_from_attribute_series(
		SquareAttrSeries ),

	% We describe now our square as two triangles in CCW order; the first,
	% S0-S1-S3 on the bottom left, the second, S1-S2-S3 on the top right; we
	% have just a plain list of indices (not for example a list of triplets of
	% indices):
	%
	SquareIndices = [ 0, 1, 3,   % As the first  triangle is S0-S1-S3
					  1, 2, 3 ], % As the second triangle is S1-S2-S3

	SquareEBOId = gui_shader:assign_indices_to_new_ebo( SquareIndices ),


	% As the (single, here) VBO and the EBO were created whereas this VAO was
	% active, they are tracked by this VAO, which will rebind them automatically
	% the next time it will be itself bound:
	%
	gui_shader:unset_current_vao(),

	{ SquareVAOId, SquareMergedVBOId, SquareEBOId }.




% @doc Runs the OpenGL test if possible.
-spec run_opengl_test() -> void().
run_opengl_test() ->

	test_facilities:display(
		"~nStarting the test of transformation support with OpenGL shaders." ),

	case gui_opengl:get_glxinfo_strings() of

		undefined ->
			test_facilities:display( "No proper OpenGL support detected on host"
				" (no GLX visual reported), thus no test performed." );

		GlxInfoStr ->
			test_facilities:display( "Checking whether OpenGL hardware "
				"acceleration is available: ~ts.",
				[ gui_opengl:is_hardware_accelerated( GlxInfoStr ) ] ),
			run_actual_test()

	end.



% @doc Runs the actual test.
-spec run_actual_test() -> void().
run_actual_test() ->

	% Only true if keypad is enabled

	% Using a Myriad 3D referential here with Z-up, where the camera is fixed at
	% the origin, pointing to the -Z axis, with its up direction being the +Y
	% axis; so:
	% - X increases from, onscreen, left to right
	% - Y increases from bottom of screen to top
	% - Z increases as getting from farther to nearer the observer
	%
	test_facilities:display( "This test will display a square textured with a Myriad image, whose center is at the origin, which is belonging to the Z=0 plane (using Z-up conventions), and that can be moved by hitting keys on the numerical keypad (while the rendering window has the focus):~n"
		"  - to translate it of ~f units along (if in translation mode):~n"
		"    * the X (abscissa) axis: hit '4' to move it, on the left, '6' on the right~n"
		"    * the Y (ordinate) axis: hit '2' to move it down, '8' up~n"
		"    * the Z (depth/altitude) axis: hit '3' to move it further/downward, '9' nearer/upward~n"
		"  - to rotate of ~f degrees around (if in rotation mode):~n"
		"    * the X axis: hit '4' to turn it clockwise (CW), '6' counter-clockwise (CCW)~n"
		"    * the Y axis: hit '2' to turn it CW, '8' CCW~n"
		"    * the Z axis: hit '3' to turn it CW, '9' CCW~n"
		"  - to shear it of a ~f factor along (if in shearing mode):~n"
		"    * the X axis: hit '4' to scale it down, '6' up~n"
		"    * the Y axis: hit '2' to scale it down, '8' up~n"
		"    * the Z axis: hit '3' to scale it down, '9' up~n~n"
		" Hit '5' to reset its position and direction, 'Enter' on the keypad "
		"to switch to the next transformation mode (cycling between translation, rotation, shearing), 'p' to toggle the projection mode (cycling between orthographic and perspective), 'Escape' to quit.~n~n"
		"Hints:~n"
		" - with the (default) orthographic projection mode, the square will remain the same for any Z in [-1.0, 1.0] (no perspective division) and, out of this range (past either the near or far clipping plane), it will fully disappear~n"
		" - with the perspective projection, the square will appear iff its Z is below -0.1 (as ZNear=0.1), and will then progressively shrink when progressing along the -Z axis; as a result, from the default position, first make the square go further/downward to make it appear~n",
		[ ?delta_coord, ?delta_angle, ?delta_scale ] ),

	gui:start(),

	% Could be batched (see gui:batch/1) to be more effective:
	InitialGUIState = init_test_gui(),

	gui_frame:show( InitialGUIState#my_gui_state.main_frame ),

	% OpenGL will be initialised only when the corresponding frame will be ready
	% (that is once first reported as resized):
	%
	gui_main_loop( InitialGUIState ),

	gui:stop().



% @doc Creates the initial test GUI: a main frame containing an OpenGL canvas to
% which an OpenGL context is associated.
%
% Once the rendering is done, the buffers are swapped, and the content is
% displayed.
%
-spec init_test_gui() -> my_gui_state().
init_test_gui() ->

	MainFrame = gui_frame:create(
		"MyriadGUI OpenGL Shader-based Transformation Test",

		% Preferring a square frame/viewport, otherwise due to aspect ratio the
		% square will be a rectangle:
		_Size={ 800, 800 } ),

	% Better:
	gui_frame:center_on_screen( MainFrame ),

	% Using mostly default GL attributes:
	GLCanvasAttrs =
		[ use_core_profile | gui_opengl:get_default_canvas_attributes() ],

	GLCanvas = gui_opengl:create_canvas(
		_CanvasOpts=[ { gl_attributes, GLCanvasAttrs } ], _Parent=MainFrame ),

	% Created, yet not bound yet (must wait for the main frame to be shown):
	GLContext = gui_opengl:create_context( GLCanvas ),

	gui:subscribe_to_events( { [ onResized, onShown, onWindowClosed ],
							   MainFrame } ),

	% Needed, otherwise if that frame is moved out of the screen or if another
	% window overlaps, the OpenGL canvas gets garbled and thus must be redrawn:
	%
	% (key events collected at the canvas-level, as frames do not handle them)
	%
	gui:subscribe_to_events( { [ onRepaintNeeded, onKeyPressed ], GLCanvas } ),

	% Would be too early for gui_texture:load_from_file (no GL context yet):
	TestImage = gui_image:load_from_file(
		gui_opengl_texture_test:get_test_texture_path() ),

	ProjSettings = get_base_orthographic_settings(),

	_Zero = 0.0,

	% No OpenGL state yet (GL context cannot be set as current yet), actual
	% OpenGL initialisation to happen when available, i.e. when the main frame
	% is shown:
	%
	#my_gui_state{ main_frame=MainFrame,
				   canvas=GLCanvas,
				   context=GLContext,
				   image=TestImage,
				   %center_pos=point3:null(),
				   %x_angle=Zero,
				   %y_angle=Zero,
				   %z_angle=Zero,
				   model_view=matrix4:identity(),
				   projection_settings=ProjSettings,
				   projection=projection:projection( ProjSettings ),
				   transformation_mode=translation }.



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages.
%
-spec gui_main_loop( my_gui_state() ) -> void().
gui_main_loop( GUIState ) ->

	%trace_utils:debug( "Main loop." ),

	% Matching the least-often received messages last:
	receive


		{ onKeyPressed, [ GLCanvas, _GLCanvasId, Context ] } ->
			% Using here scancodes, not to depend on any keyboard layout or
			% modifier:
			%
			Scancode = gui_keyboard:event_context_to_scancode( Context ),

			%trace_utils:debug_fmt( "Scan code pressed: ~B on ~w.",
			%                       [ Scancode, GLCanvas ] ),

			case update_scene( Scancode, GUIState ) of

				{ NewGUIState, _DoQuit=true } ->
					terminate( NewGUIState ),
					trace_utils:info( "Requested to quit, test success." );

				{ NewGUIState, _DoQuit } ->
					% Supposing the OpenGL state is already available:
					render( GUIState#my_gui_state.opengl_state ),
					gui_opengl:swap_buffers( GLCanvas ),
					gui_main_loop( NewGUIState )

			end;

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
					render( GLState ),
					gui_opengl:swap_buffers( GLCanvas ),
					GUIState

			end,
			gui_main_loop( RepaintedGUIState );


		% For a window, the first resizing event happens immediately before its
		% onShown one:
		%
		{ onResized, [ _ParentFrame, _ParentFrameId, _NewParentSize,
					   _EventContext ] } ->

			%trace_utils:debug_fmt( "Resizing of the parent window "
			%   "(main frame) to ~w detected.", [ NewParentSize ] ),

			ResizedGUIState = case GUIState#my_gui_state.opengl_state of

				% Not ready yet:
				undefined ->
					trace_utils:debug( "Resized, yet no OpenGL state yet." ),
					GUIState;

				_ ->
					on_main_frame_resized( GUIState )

			end,

			gui_main_loop( ResizedGUIState );


		% Less frequent messages looked up last:

		% This is the most suitable first location to initialise OpenGL, as
		% making a GL context current requires a shown window:
		%
		{ onShown, [ ParentFrame, _ParentFrameId, _EventContext ] } ->

			trace_utils:debug_fmt( "Parent window (main frame) just shown "
				"(initial size of ~w).",
				[ gui_widget:get_size( ParentFrame ) ] ),

			% Optional yet better:
			gui:unsubscribe_from_events( { onShown, ParentFrame } ),

			% Done once for all:
			InitGUIState = initialise_opengl( GUIState ),

			% A onRepaintNeeded event message is expected just afterwards.

			gui_main_loop( InitGUIState );


		{ onWindowClosed, [ _ParentFrame, _ParentFrameId, _EventContext ] } ->
			terminate( GUIState ),
			trace_utils:info( "Main frame closed, test success." );


		OtherEvent ->
			trace_utils:warning_fmt( "Test ignored following event:~n ~p",
									 [ OtherEvent ] ),

			gui_main_loop( GUIState )

	% No 'after': no spontaneous action taken here, in the absence of events.

	end.



% @doc Sets up OpenGL, once for all (regardless of next resizings), once a
% proper OpenGL context is available.
%
-spec initialise_opengl( my_gui_state() ) -> my_gui_state().
initialise_opengl( GUIState=#my_gui_state{ canvas=GLCanvas,
										   context=GLContext,
										   image=Image,
										   model_view=ModelViewMat4,
										   projection=ProjMat4,
										   % Check:
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

	% Not found available at least in some configurations:
	%TargetProfile = core,

	TargetProfile = compatibility,
	%TargetProfile = non_existing_profile,

	gui_opengl:check_requirements( MinOpenGLVersion, TargetProfile ),

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

	% Creates, compiles and links our GLSL program from the two specified
	% shaders, that are, in the same movement, automatically attached and
	% linked, then detached and deleted:
	%
	ProgramId = gui_shader:generate_program_from(
		"gui_opengl_transformation_shader.vertex.glsl",
		"gui_opengl_transformation_shader.fragment.glsl", UserVertexAttrs ),

	% Uniform locations can be fetched as soon as the program is linked:

	% Refer to the vertex shader:
	ModelViewMatUnifId = gui_shader:get_uniform_id(
		_MVUnifName="my_model_view_matrix", ProgramId ),

	ProjMatUnifId = gui_shader:get_uniform_id(
		_ProjUnifName="my_projection_matrix", ProgramId ),

	% Refer to the fragment shader:
	SamplerUnifId = gui_shader:get_uniform_id(
		_SamplerUnifName="my_texture_sampler", ProgramId ),

	Texture = gui_texture:create_from_image( Image ),

	% To showcase that we can use other texture units (locations) than the
	% default ?GL_TEXTURE0 one:
	%
	gui_texture:set_current_texture_unit( ?GL_TEXTURE2 ),

	% Thus associated to the previous texture unit:
	gui_texture:set_as_current( Texture ),

	trace_utils:debug_fmt( "Prepared ~ts.",
						   [ gui_texture:to_string( Texture ) ] ),

	% Rely on our shaders; can be used from now:
	gui_shader:install_program( ProgramId ),

	% Uniforms can be set as soon as the GLSL program is installed:

	% Initial setting of the model-view matrix:
	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, ModelViewMat4 ),

	% Same for the projection one:
	gui_shader:set_uniform_matrix4( ProjMatUnifId, ProjMat4 ),


	% Set the texture location of the sampler uniform:
	gui_shader:set_uniform_i( SamplerUnifId, _TextureUnit=2 ),


	% Prepare the textured square, whose vertices are specified through indices.
	%
	% We also have here to manage texture coordinates in addition to vertices,
	% so we merge them in a single VBO (that will be accessed thanks to an EBO):
	%
	{ SquareVAOId, SquareMergedVBOId, SquareEBOId } = prepare_square( Texture ),

	InitOpenGLState = #my_opengl_state{
		program_id=ProgramId,

		texture=Texture,
		model_view_id=ModelViewMatUnifId,
		projection_id=ProjMatUnifId,

		square_vao_id=SquareVAOId,

		% Two basic triangles referenced in the associated VBO:
		square_vertex_count=6,

		square_merged_vbo_id=SquareMergedVBOId,
		square_ebo_id=SquareEBOId },

	% Note that the default projection is orthographic; as a result, moving the
	% square along the Z (depth) will not change anything (until going out of
	% the NDC [-1.0, 1.0] range, and having the square disappear).

	trace_utils:debug( "Starting with an orthographic projection "
					   "with Z-up conventions, and in translation mode." ),

	InitGUIState = GUIState#my_gui_state{
		% Start at the origin:
		opengl_state=InitOpenGLState },

	% As the initial onResized was triggered whereas no OpenGL state was
	% already available:
	%
	on_main_frame_resized( InitGUIState ).



% @doc Cleans up OpenGL.
-spec cleanup_opengl( my_gui_state() ) -> void().
cleanup_opengl( #my_gui_state{ opengl_state=undefined } ) ->
	ok;

cleanup_opengl( #my_gui_state{ opengl_state=#my_opengl_state{
		program_id=ProgramId,
		square_vao_id=SquareVAOId,
		square_merged_vbo_id=SquareMergedVBOId,
		square_ebo_id=SquareEBOId } } ) ->

	trace_utils:debug( "Cleaning up OpenGL." ),

	% Deleting the VAO does not delete the VBO or the EBO (that are just
	% referenced); deleting first the VAO is preferred:

	gui_shader:delete_vao( SquareVAOId ),

	gui_shader:delete_vbo( SquareMergedVBOId ),

	gui_shader:delete_ebo( SquareEBOId ),

	gui_shader:delete_program( ProgramId ).



% @doc Managing a resizing of the main frame.
%
% OpenGL context expected here to have already been set.
%
-spec on_main_frame_resized( my_gui_state() ) -> my_gui_state().
on_main_frame_resized( GUIState=#my_gui_state{ canvas=GLCanvas,
											   opengl_state=GLState } ) ->

	% Maximises the canvas in the main frame:
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
	% canvas size - not according to the one that was expected to be already
	% resized.
	%
	gui_widget:sync( GLCanvas ),

	% No specific projection settings enforced.

	% Any OpenGL reset to be done because of the resizing should take place
	% here.
	%
	% Using here normalised coordinates (in [0.0,1.0]), so no need to update the
	% projection.

	render( GLState ),

	% Includes a gl:flush/0:
	gui_opengl:swap_buffers( GLCanvas ),

	% No null canvas height expected:
	GUIState#my_gui_state{ aspect_ratio=CanvasWidth/CanvasHeight }.



% @doc Performs a (pure OpenGL; no gui_* involved) rendering.
-spec render( my_opengl_state() ) -> void().
render( #my_opengl_state{
			square_vao_id=SquareVAOId,
			square_vertex_count=SquareVCount

			% Both bound thanks to the VAO:
			%square_merged_vbo_id=SquareMergedVBOId,
			%square_ebo_id=SquareEBOId

		  } ) ->

	%trace_utils:debug_fmt( "Rendering now for size {~B,~B}.",
	%                       [ Width, Height ] ),

	gl:clear( ?GL_COLOR_BUFFER_BIT ),

	% We already use (enabled) our shader program.

	PrimType = ?GL_TRIANGLES,

	% From now, all operations must be performed at each rendering; displaying
	% the square:

	% Sets the vertex attribute; this binds as well the square EBO (and the
	% VBO), as they were still tracked by the VAO when this VAO was unset:
	%
	gui_shader:set_current_vao_from_id( SquareVAOId ),

	% No offset in the start index needed:
	gui_shader:render_from_enabled_ebo( PrimType, SquareVCount ),

	gui_shader:unset_current_vao(),

	% Not swapping buffers here, as would involve GLCanvas, whereas this
	% function is meant to remain pure OpenGL.
	%
	% gl:flush/0 done when swapping buffers.

	ok.



% @doc Terminates the test.
-spec terminate( my_gui_state() ) -> void().
terminate( GUIState=#my_gui_state{ main_frame=MainFrame } ) ->

	cleanup_opengl( GUIState ),
	trace_utils:info( "Terminating test." ),

	% Very final check, while there is still an OpenGL context:
	gui_opengl:check_error(),

	% No more recursing:
	gui_frame:destruct( MainFrame ).



% @doc Updates the scene, based on the specified user-entered (keyboard) scan
% code.
%
%
% First managing translations:
-spec update_scene( scancode(), my_gui_state() ) ->
						{ my_gui_state(), DoQuit :: boolean() }.
update_scene( _Scancode=?increase_x_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=translation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,

	% Translation on the X axis:
	VT = [ Inc, 0.0, 0.0 ],

	NewModelViewMat4 = matrix4:translate_homogeneous( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Increasing X of ~f, resulting in: MV = ~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };

update_scene( _Scancode=?decrease_x_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=translation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,

	% Translation on the X axis:
	VT = [ -Inc, 0.0, 0.0 ],

	NewModelViewMat4 = matrix4:translate_homogeneous( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Decreasing X of ~f, resulting in: MV = ~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?increase_y_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=translation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,

	% Translation on the Y axis:
	VT = [ 0.0, Inc, 0.0 ],
	NewModelViewMat4 = matrix4:translate_homogeneous( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Increasing Y of ~f, resulting in: MV = ~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };

update_scene( _Scancode=?decrease_y_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=translation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,

	% Translation on the Y axis:
	VT = [ 0.0, -Inc, 0.0 ],
	NewModelViewMat4 = matrix4:translate_homogeneous( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Decreasing Y of ~f, resulting in: MV = ~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };



% Note that moving along the Z axis whereas the projection is orthographic will
% show no difference:

update_scene( _Scancode=?increase_z_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=translation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,

	% Translation on the Z axis:
	VT = [ 0.0, 0.0, Inc ],
	NewModelViewMat4 = matrix4:translate_homogeneous( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Increasing Z of ~f, resulting in: MV = ~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?decrease_z_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=translation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = ?delta_coord,

	% Translation on the Z axis:
	VT = [ 0.0, 0.0, -Inc ],
	NewModelViewMat4 = matrix4:translate_homogeneous( ModelViewMat4, VT ),

	trace_utils:debug_fmt( "Decreasing Z of ~f, resulting in: MV = ~ts",
						   [ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };



% Secondly managing rotations:

update_scene( _Scancode=?increase_x_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=rotation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	% Rotation around the X axis:
	RotAxis = vector3:x_axis(),

	Angle = math_utils:degrees_to_radians( ?delta_angle ),

	NewModelViewMat4 =
		matrix4:rotate_homogeneous( ModelViewMat4, RotAxis, Angle ),

	trace_utils:debug_fmt( "Rotating around the X axis of an angle of ~f "
		"radians, resulting in: MV = ~ts",
		[ Angle, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };

update_scene( _Scancode=?decrease_x_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=rotation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	% Rotation around the X axis:
	RotAxis = vector3:x_axis(),

	Angle = - math_utils:degrees_to_radians( ?delta_angle ),

	NewModelViewMat4 =
		matrix4:rotate_homogeneous( ModelViewMat4, RotAxis, Angle ),

	trace_utils:debug_fmt( "Rotating around the X axis of an angle of ~f "
		"radians, resulting in: MV = ~ts",
		[ Angle, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?increase_y_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=rotation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	% Rotation around the Y axis:
	RotAxis = vector3:y_axis(),

	Angle = math_utils:degrees_to_radians( ?delta_angle ),

	NewModelViewMat4 =
		matrix4:rotate_homogeneous( ModelViewMat4, RotAxis, Angle ),

	trace_utils:debug_fmt( "Rotating around the Y axis of an angle of ~f "
		"radians, resulting in: MV = ~ts",
		[ Angle, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?decrease_y_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=rotation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	% Rotation around the Y axis:
	RotAxis = vector3:y_axis(),

	Angle = - math_utils:degrees_to_radians( ?delta_angle ),

	NewModelViewMat4 =
		matrix4:rotate_homogeneous( ModelViewMat4, RotAxis, Angle ),

	trace_utils:debug_fmt( "Rotating around the Y axis of an angle of ~f "
		"radians, resulting in: MV = ~ts",
		[ Angle, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?increase_z_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=rotation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	% Rotation around the Z axis:
	RotAxis = vector3:z_axis(),

	Angle = math_utils:degrees_to_radians( ?delta_angle ),

	NewModelViewMat4 =
		matrix4:rotate_homogeneous( ModelViewMat4, RotAxis, Angle ),

	trace_utils:debug_fmt( "Rotating around the Z axis of an angle of ~f "
		"radians, resulting in: MV = ~ts",
		[ Angle, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?decrease_z_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=rotation,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	% Rotation around the Z axis:
	RotAxis = vector3:z_axis(),

	Angle = - math_utils:degrees_to_radians( ?delta_angle ),

	NewModelViewMat4 =
		matrix4:rotate_homogeneous( ModelViewMat4, RotAxis, Angle ),

	trace_utils:debug_fmt( "Rotating around the Z axis of an angle of ~f "
		"radians, resulting in: MV = ~ts",
		[ Angle, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


% Thirdly managing shearings:
update_scene( _Scancode=?increase_x_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=shearing,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = 1.0 + ?delta_coord,

	NewModelViewMat4 = matrix4:scale_homogeneous_x( ModelViewMat4, Inc ),

	trace_utils:debug_fmt( "Shearing on the X axis of a factor ~f, "
		"resulting in: MV = ~ts",
		[ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };

update_scene( _Scancode=?decrease_x_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=shearing,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = 1.0 - ?delta_coord,

	NewModelViewMat4 = matrix4:scale_homogeneous_x( ModelViewMat4, Inc ),

	trace_utils:debug_fmt( "Shearing on the X axis of a factor ~f, "
		"resulting in: MV = ~ts",
		[ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?increase_y_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=shearing,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = 1.0 + ?delta_coord,

	NewModelViewMat4 = matrix4:scale_homogeneous_y( ModelViewMat4, Inc ),

	trace_utils:debug_fmt( "Shearing on the Y axis of a factor ~f, "
		"resulting in: MV = ~ts",
		[ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };

update_scene( _Scancode=?decrease_y_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=shearing,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = 1.0 - ?delta_coord,

	NewModelViewMat4 = matrix4:scale_homogeneous_y( ModelViewMat4, Inc ),

	trace_utils:debug_fmt( "Shearing on the Y axis of a factor ~f, "
		"resulting in: MV = ~ts",
		[ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?increase_z_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=shearing,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = 1.0 + ?delta_coord,

	NewModelViewMat4 = matrix4:scale_homogeneous_z( ModelViewMat4, Inc ),

	trace_utils:debug_fmt( "Shearing on the Z axis of a factor ~f, "
		"resulting in: MV = ~ts",
		[ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?decrease_z_scan_code,
			  GUIState=#my_gui_state{
				model_view=ModelViewMat4,
				transformation_mode=shearing,
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	Inc = 1.0 - ?delta_coord,

	NewModelViewMat4 = matrix4:scale_homogeneous_z( ModelViewMat4, Inc ),

	trace_utils:debug_fmt( "Shearing on the Z axis of a factor ~f, "
		"resulting in: MV = ~ts",
		[ Inc, matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };



update_scene( _Scancode=?reset_scan_code,
			  GUIState=#my_gui_state{
				opengl_state=#my_opengl_state{
					model_view_id=ModelViewMatUnifId } } ) ->

	NewModelViewMat4 = identity_4,

	trace_utils:debug_fmt(
		"Resetting the modelview matrix, resulting in: MV = ~ts",
		[ matrix4:to_string( NewModelViewMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ModelViewMatUnifId, NewModelViewMat4 ),

	{ GUIState#my_gui_state{ model_view=NewModelViewMat4 }, _DoQuit=false };


update_scene( _Scancode=?mode_switch_scan_code,
			  GUIState=#my_gui_state{ transformation_mode=TransfoMode } ) ->

	NewTransfoMode = case TransfoMode of

		translation ->
			rotation;

		rotation ->
			shearing;

		shearing ->
			translation

	end,

	trace_utils:debug_fmt( "Switching transformation mode from ~ts to ~ts.",
						   [ TransfoMode, NewTransfoMode ] ),

	{ GUIState#my_gui_state{ transformation_mode=NewTransfoMode },
	  _DoQuit=false };


update_scene( _Scancode=?projection_mode_scan_code,
			  GUIState=#my_gui_state{ aspect_ratio=AspectRatio,
									  projection_settings=ProjSettings,
									  opengl_state=#my_opengl_state{
										projection_id=ProjMatUnifId } } ) ->
	% Swapping the projection type:
	{ NewProjSettings, NewProjMat4 } =
			case type_utils:get_record_tag( ProjSettings ) of

		orthographic_settings ->
			PerspSettings =
				get_base_perspective_settings( AspectRatio ),
			{ PerspSettings, projection:perspective( PerspSettings ) };

		perspective_settings ->
			OrthoSettings = get_base_orthographic_settings(),
			{ OrthoSettings, projection:orthographic( OrthoSettings ) }

	end,

	trace_utils:debug_fmt( "Switching to ~ts, the corresponding matrix "
		"being: ~ts.",
		[ projection:settings_to_string( NewProjSettings ),
		  matrix4:to_string( NewProjMat4 ) ] ),

	gui_shader:set_uniform_matrix4( ProjMatUnifId, NewProjMat4 ),

	{ GUIState#my_gui_state{ projection_settings=NewProjSettings,
							 projection=NewProjMat4 },
	  _DoQuit=false };


update_scene( _Scancode=?quit_scan_code, GUIState ) ->
	trace_utils:debug( "Requested to quit." ),
	{ GUIState, _DoQuit=true };

update_scene( _Scancode, GUIState ) ->
	%trace_utils:debug_fmt( "(scancode ~B ignored)", [ Scancode ] ),
	{ GUIState, _DoQuit=false }.



-spec get_base_orthographic_settings() ->
						orthographic_settings().
get_base_orthographic_settings() ->
	%#orthographic_settings{
	%	left=0.0,
	%	right=800.0,
	%	bottom=0.0,
	%	top=600.0,
	%	z_near=0.1,
	%	z_far=100.0 }.

	% Corresponds to a default identity matrix:
	#orthographic_settings{
		left=-1.0,
		right=1.0,
		bottom=-1.0,
		top=1.0,
		z_near=1.0,
		z_far=-1.0 }.


-spec get_base_perspective_settings( aspect_ratio() ) ->
						perspective_settings().
get_base_perspective_settings( AspectRatio ) ->
	#perspective_settings{
		fov_y_angle=math_utils:degrees_to_radians( 45 ),
		aspect_ratio=AspectRatio,
		z_near=0.1,
		z_far=100.0 }.



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running this OpenGL test, being in batch mode)" );

		false ->
			run_opengl_test()

	end,

	test_facilities:stop().
