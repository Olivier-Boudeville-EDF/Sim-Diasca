% Copyright (C) 2022-2023 Olivier Boudeville
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
% Creation date: Sunday, January 9, 2022.


% @doc Minimal testing of <b>shader-based texture rendering</b>: displays, based
% on GLSL shaders, a textured polygon.
%
% It is therefore a non-interactive, passive test (no spontaneous/scheduled
% behaviour) whose main interest is to show a simple yet generic, appropriate
% use of textures.
%
% This test relies on shaders and thus on modern versions of OpenGL (e.g. 3.3),
% as opposed to the compatibility mode for OpenGL 1.x.
%
% See also the gui_opengl_texture_test module for a corresponding test with the
% legacy versions of OpenGL (compatibility mode).
%
-module(gui_opengl_texture_shader_test).


% Implementation notes:
%
% Directly inspired from https://learnopengl.com/Getting-started/Textures.


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

	% The image as loaded from file, to be transformed in a texture:
	image :: image(),

	% Needs an OpenGL context:
	texture :: maybe( texture() ),

	% In more complex cases, would store the loaded textures, etc.
	opengl_state :: maybe( my_opengl_state() ) } ).

-type my_gui_state() :: #my_gui_state{}.
% Test-specific overall GUI state.



-record( my_opengl_state, {

	% The identifier of our GLSL program:
	program_id :: program_id(),

	% For the triangle, which has direct coordinates:

	% The identifier of the VAO (Vertex Array Object) used in this test for the
	% triangle:
	%
	triangle_vao_id :: vao_id(),

	% A single triangle referenced in the associated VBOs:
	triangle_vertex_count :: vertex_count(),

	% The identifier of the VBO (Vertex Buffer Object) storing the vertices of
	% the triangle:
	%
	triangle_vertex_vbo_id :: vbo_id(),

	% The identifier of the VBO storing the texture coordinates for the
	% triangle:
	%
	triangle_tex_coord_vbo_id :: vbo_id(),


	% For the square, which has indexed coordinates:

	square_vao_id :: vao_id(),

	square_vertex_count :: vertex_count(),

	% The VBO concentrating vertices and texture coordinates:
	square_merged_vbo_id :: vbo_id(),

	% Indices for the vertex:
	square_ebo_id :: ebo_id() } ).

-type my_opengl_state() :: #my_opengl_state{}.
% Test-specific overall OpenGL state.
%
% Storing VBOs and EBOs is probably only of use in order to deallocate them
% properly once not needed anymore.



% Shorthands:

-type frame() :: gui:frame().

-type image() :: gui_image:image().

-type width() :: gui:width().
-type height() :: gui:height().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().

-type texture() :: gui_texture:texture().

-type program_id() :: gui_shader:program_id().
-type vao_id() :: gui_shader:vao_id().
-type vbo_id() :: gui_shader:vbo_id().
-type ebo_id() :: gui_shader:ebo_id().

-type vertex_count() :: mesh:vertex_count().


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



% @doc Prepares all information needed to render the triangle, and returns them.
%
% Here separate VBOs are used for the vertices and for the texture coordinates.
%
% The texture of interest is specified, as we need to use its inner (original)
% dimensions, not the ones that were obtained after padding to powers of two.
%
-spec prepare_triangle( texture() ) -> { vao_id(), vbo_id(), vbo_id() }.
prepare_triangle( Texture ) ->

	TriangleVAOId = gui_shader:set_new_vao(),

	Z = 0.0,
	O = 1.0,

	% Triangle defined as [vertex3()], directly in normalised device coordinates
	% here; CCW order (T0 bottom left, T1 bottom right, T2 top, knowing that the
	% texture referential has its Y ordinate axis up, see
	% https://learnopengl.com/Getting-started/Hello-Triangle); we define here an
	% upright triangle so that the texture is not deformed:
	%
	%                 T2
	%               /  |
	%             T0--T1
	%
	TriangleVertices =
		[ _T0={ -O, -O, Z }, _T1={ O, -O, Z }, _T2={ O, O, Z } ],


	% Targeting vertex attributes in a VBO, created and made active once for all
	% here, and declared properly to the specified vertex attribute, which is
	% also enabled (specified while the VAO is still active so that it can
	% record that attribute specification):
	%
	TriangleVertexVBOId = gui_shader:assign_vertex_attribute_as(
		?my_vertex_attribute_index, TriangleVertices ),

	% We have to take into account that, due to the padding, the actual texture
	% is smaller than the technical one:
	%
	OriginalTriangleTexCoords = [ _TC0={ Z, Z }, _TC1={ O, Z }, _TC2={ O, O } ],

	%ActualTriangleTexCoords = [ _TC0={ MinX, MinY }, _TC1={ MaxX, MinY },
	%                            _TC2={ MaxX, MaxY } ],

	ActualTriangleTexCoords = gui_texture:recalibrate_coordinates_for(
		OriginalTriangleTexCoords, Texture ),

	TriangleTexCoordVBOId = gui_shader:assign_vertex_attribute_as(
		?my_texture_coords_attribute_index, ActualTriangleTexCoords ),


	% As the two VBOs were created whereas this VAO was active, they are tracked
	% by it; it will rebind them automatically the next time it will be itself
	% bound:
	%
	gui_shader:unset_current_vao(),

	{ TriangleVAOId, TriangleVertexVBOId, TriangleTexCoordVBOId }.



% @doc Prepares all information needed to render the square, and returns them.
%
% Here a single VBO is used, merging the vertices and the texture coordinates;
% additionally an EBO is used.
%
-spec prepare_square( texture() ) -> { vao_id(), vbo_id(), ebo_id() }.
prepare_square( Texture ) ->

	SquareVAOId = gui_shader:set_new_vao(),

	% Half edge length:
	H = 0.5,

	Z = 0.0,

	% Square defined as [vertex3()], directly in normalized device coordinates
	% here; CCW order (bottom left, bottom right, top right, top left)::
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

	ActualSquareTexCoords = gui_texture:recalibrate_coordinates_for(
		OrigSquareTexCoords, Texture ),

	SquareAttrSeries= [ SquareVertices, ActualSquareTexCoords ],

	% We start at vertex attribute index #0 in this VAO; as there are two
	% series, the vertex attribute indices will be 0 and 1:
	%
	SquareMergedVBOId = gui_shader:assign_new_vbo_from_attribute_series(
		SquareAttrSeries ),

	% We describe now our square as two triangles in CCW order; the first,
	% S0-S1-S3 on the bottom left, the second, S1-S2-S3 on the top right; we
	% have just a list of indices (not for example a list of triplets of
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
		"~nStarting the test of texture support with OpenGL shaders." ),

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

	test_facilities:display( "This test will display two textured polygons: "
							 "an upright triangle and a rectangle." ),

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

	MainFrame = gui_frame:create( "MyriadGUI OpenGL Shader-based Texture Test",
								  _Size={ 1024, 768 } ),

	% Using mostly default GL attributes:
	GLCanvasAttrs =
		[ use_core_profile | gui_opengl:get_default_canvas_attributes() ],

	GLCanvas = gui_opengl:create_canvas( _Parent=MainFrame,
										 [ { gl_attributes, GLCanvasAttrs } ] ),


	% Created, yet not bound yet (must wait for the main frame to be shown):
	GLContext = gui_opengl:create_context( GLCanvas ),

	gui:subscribe_to_events( { [ onResized, onShown, onWindowClosed ],
							   MainFrame } ),

	% Needed, otherwise if that frame is moved out of the screen or if another
	% windows overlaps, the OpenGL canvas gets garbled and thus must be redrawn:
	%
	gui:subscribe_to_events( { onRepaintNeeded, GLCanvas } ),

	% Would be too early for gui_texture:load_from_file (no GL context yet):
	TestImage = gui_image:load_from_file(
		gui_opengl_texture_test:get_test_texture_path() ),

	% No OpenGL state yet (GL context cannot be set as current yet), actual
	% OpenGL initialisation to happen when available, i.e. when the main frame
	% is shown:
	%
	#my_gui_state{ main_frame=MainFrame, canvas=GLCanvas, context=GLContext,
				   image=TestImage }.



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages.
%
-spec gui_main_loop( my_gui_state() ) -> void().
gui_main_loop( GUIState ) ->

	%trace_utils:debug( "Main loop." ),

	% Matching the least-often received messages last:
	receive

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

					% Simpler than storing these at each resize:
					{ CanvasWidth, CanvasHeight } =
						gui_widget:get_size( GLCanvas ),

					render( CanvasWidth, CanvasHeight, GLState ),
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
			InitGUIState = initialise_opengl( GUIState ),

			% A onRepaintNeeded event message expected just afterwards.

			gui_main_loop( InitGUIState );


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



% @doc Sets up OpenGL, once for all (regardless of next resizings), once a
% proper OpenGL context is available.
%
-spec initialise_opengl( my_gui_state() ) -> my_gui_state().
initialise_opengl( GUIState=#my_gui_state{ canvas=GLCanvas,
										   context=GLContext,
										   image=Image,
										   % Checks:
										   texture=undefined,
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

	% Creates, compiles and links our GLSL program from the two specified
	% shaders, that are, in the same movement, automatically attached and
	% linked, then detached and deleted:
	%
	ProgramId = gui_shader:generate_program_from(
		"gui_opengl_texture_shader.vertex.glsl",
		"gui_opengl_texture_shader.fragment.glsl", UserVertexAttrs ),

	% Usable as soon as the program is linked; refer to the fragment shader:
	SamplerUnifId = gui_shader:get_uniform_id(
		_SamplerUnifName="my_texture_sampler", ProgramId ),

	Texture = gui_texture:create_from_image( Image ),

	% To showcase that we can use other texture units (locations) than the
	% default ?GL_TEXTURE0 one:
	%
	gui_texture:set_current_texture_unit( ?GL_TEXTURE4 ),

	% Thus associated to the previous texture unit:
	gui_texture:set_as_current( Texture ),

	trace_utils:debug_fmt( "Prepared ~ts.",
						   [ gui_texture:to_string( Texture ) ] ),

	% Rely on our shaders; can be used from now:
	gui_shader:install_program( ProgramId ),

	% Set the texture location of the sampler uniform:
	%
	% (as expected, specifying other texture units would result in no texture
	% being applied; yet, for some reason, using specifically TextureUnit=0
	% still results in the expected texture to be applied)
	%
	% No texture: gui_shader:set_uniform_i( SamplerUnifId, _TextureUnit=1 ),
	% Right:
	gui_shader:set_uniform_i( SamplerUnifId, _TextureUnit=4 ),

	% Texture shown as well for:
	% gui_shader:set_uniform_i( SamplerUnifId, _TextureUnit=0 ),


	% Uncomment to switch to wireframe and see how the square decomposes in two
	% triangles:
	%
	% (?GL_FRONT_AND_BACK not needed as our vertices are in CCW order)
	%
	%gui_opengl:set_polygon_raster_mode( ?GL_FRONT, ?GL_LINE ),

	% First, a triangle, whose vertices and texture coordinates are specified
	% separately:
	%
	{ TriangleVAOId, TriangleVertexVBOId, TriangleTexCoordVBOId } =
		prepare_triangle( Texture ),

	% Second, a square, whose vertices are specified this time through
	% indices.
	%
	% We also have here to manage texture coordinates in addition to vertices,
	% so we merge them in a single VBO (that will be accessed thanks to an EBO):
	%
	{ SquareVAOId, SquareMergedVBOId, SquareEBOId } = prepare_square( Texture ),

	InitOpenGLState = #my_opengl_state{
		program_id=ProgramId,

		triangle_vao_id=TriangleVAOId,
		% A single basic triangle referenced in the associated VBOs:
		triangle_vertex_count=3,
		triangle_vertex_vbo_id=TriangleVertexVBOId,
		triangle_tex_coord_vbo_id=TriangleTexCoordVBOId,

		square_vao_id=SquareVAOId,
		% Two basic triangles referenced in the associated VBO:
		square_vertex_count=6,
		square_merged_vbo_id=SquareMergedVBOId,
		square_ebo_id=SquareEBOId },

	%trace_utils:debug_fmt( "Managing a resize of the main frame to ~w.",
	%                       [ gui:get_size( MainFrame ) ] ),

	InitGUIState = GUIState#my_gui_state{
		texture=Texture,
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
		triangle_vao_id=TriangleVAOId,
		triangle_vertex_vbo_id=TriangleVertexVBOId,
		triangle_tex_coord_vbo_id=TriangleTexCoordVBOId,
		square_vao_id=SquareVAOId,
		square_merged_vbo_id=SquareMergedVBOId,
		square_ebo_id=SquareEBOId } } ) ->

	trace_utils:debug( "Cleaning up OpenGL." ),

	gui_shader:delete_vbos( [ TriangleVertexVBOId, TriangleTexCoordVBOId,
							  SquareMergedVBOId ] ),

	gui_shader:delete_vaos( [ TriangleVAOId, SquareVAOId ] ),

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
	% canvas size, not according to the one that was expected to be already
	% resized.
	%
	gui_widget:sync( GLCanvas ),

	% No specific projection settings enforced.

	% Any OpenGL reset to be done because of the resizing should take place
	% here.
	%
	% Using here normalised coordinates (in [0.0,1.0]), so no need to update the
	% orthographic projection.

	render( CanvasWidth, CanvasHeight, GLState ),

	% Includes a gl:flush/0:
	gui_opengl:swap_buffers( GLCanvas ),

	% Const here:
	GUIState.



% @doc Performs a (pure OpenGL) rendering.
-spec render( width(), height(), my_opengl_state()  ) -> void().
render( _Width, _Height, #my_opengl_state{
			triangle_vao_id=TriangleVAOId,
			triangle_vertex_count=TriangleVCount,
			triangle_vertex_vbo_id=_TriangleVertexVBOId,
			triangle_tex_coord_vbo_id=_TriangleTexCoordVBOId,
			square_vao_id=SquareVAOId,
			square_vertex_count=SquareVCount,
			square_merged_vbo_id=_SquareMergedVBOId,
			square_ebo_id=_SquareEBOId } ) ->

	%trace_utils:debug_fmt( "Rendering now for size {~B,~B}.",
	%                       [ Width, Height ] ),

	gl:clear( ?GL_COLOR_BUFFER_BIT ),

	% We already use (enabled) our shader program.

	PrimType = ?GL_TRIANGLES,

	% From now, all operations must be performed at each rendering; first
	% starting with the triangle:

	% Sets the VBO and the two vertex attributes:
	gui_shader:set_current_vao_from_id( TriangleVAOId ),

	% So calls like these are useless:
	%gui_shader:set_current_vbo_from_id( TriangleVertexVBOId ),
	%gui_shader:enable_vertex_attribute( ?my_vertex_attribute_index ),

	% Draws our splendid triangle (from 3 slots, starting at index 0), using the
	% currently active shaders, vertex attribute configuration and with the
	% VBOs' vertex data and texture coordinates (indirectly bound via the VAO):
	%
	gui_shader:render_from_enabled_vbos( PrimType, TriangleVCount ),


	% Second, rendering the square:

	% Sets the vertex attribute; binds as well the square EBO, as it was still
	% tracked by the VAO when this VAO was unset:
	%
	gui_shader:set_current_vao_from_id( SquareVAOId ),

	% Useless as well, as the VAO takes care of them:
	%gui_shader:set_current_vbo_from_id( SquareVBOId ),
	%gui_shader:enable_vertex_attribute( ?my_vertex_attribute_index ),
	%gui_shader:set_current_ebo_from_id( SquareEBOId ),

	% No offset:
	gui_shader:render_from_enabled_ebo( PrimType, SquareVCount ),

	gui_shader:unset_current_vao(),

	% Useless, as reset at each rendering through VAOs:
	%gui_shader:disable_vertex_attribute( ?my_vertex_attribute_index ),

	% Not swapping buffers here, as would involve GLCanvas, whereas this
	% function is meant to remain pure OpenGL.
	%
	% gl:flush/0 done when swapping buffers.

	ok.



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
