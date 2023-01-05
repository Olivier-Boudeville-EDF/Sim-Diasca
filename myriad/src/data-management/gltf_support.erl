% Copyright (C) 2021-2023 Olivier Boudeville
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
% Creation date: Monday, October 4, 2021.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% @doc Gathering of various facilities about the <b>glTF 2.0</b> file format (GL
% Transmission Format).
%
% See [https://en.wikipedia.org/wiki/GlTF].
%
-module(gltf_support).


% Design notes:
%
% Only the glTF 2.0 format is supported, the JSON/ASCII version (*.gltf) - not
% the binary (*.glb) one.
%
% Only the most common, basic concepts are supported (namely: overall content,
% scene, node, mesh, primitive, attribute, material, camera, accessor, buffer,
% buffer-view), not more "advanced" ones (namely textures, images, samplers,
% skins or animations - all of which can be added relatively easily now that the
% overall structure is functional).
%
% Materials are defined based on the Physically-Based Rendering (PBR)
% methodology.

% Orientation section:
%
% In terms of orientation, conventions may differ; Myriad considers that the
% "up" direction is the +Z axis direction, whereas glTF defines +Y as "up"; as a
% consequence (Myriad, Z-up) coordinates (ex: point3:point3()) will be
% transformed here in Y-up ones (ex: point3:yup_point3()). Refer to the design
% notes in linear_3D.erl for more details.


% Implementation notes:
%
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#binary-data-storage
%
% All buffer data MUST use little endian byte order.
%
% Floating-point data MUST use IEEE-754 single (not double) precision format,
% hence on 32 bit.
%
% Values of NaN, +Infinity, and -Infinity MUST NOT be present.

% In order to encode/decode, we study each primitive defined a given mesh. A
% primitive defines in its attributes typically where position, normal and
% texture coordinates can be found, by designating for each the index of an
% accessor.

% Types are now prefixed by 'gltf_' to avoid risks of shorthand-related
% confusion with similar types defined in Myriad (ex: gltf_mesh() is used, not
% mesh() that could be mesh:mesh()).


% Implementations taken as references:
%
% - Blender:
% https://github.com/blender/blender-addons/tree/master/io_scene_gltf2;
% typically in /usr/share/blender/x.y/scripts/addons/io_scene_gltf2
%
% - Wings3D:
% https://github.com/dgud/wings/blob/master/plugins_src/import_export/wpc_gltf.erl
%
% - pygltflib: https://gitlab.com/dodgyville/pygltflib/-/tree/master/pygltflib


-type object_name() :: ustring().
% Any top-level glTF object MAY have a name string property.


-type base_name() :: ustring().
% A base name to be used for named glTF objects.

-type gltf_object_name() :: bin_string().
% Any top-level glTF object MAY have a name string property.


-type mesh_name() :: gltf_object_name().
% The name of a glTF mesh.

-type primitive_name() :: gltf_object_name().
% There exists no name per se for glTF primitives, yet their buffers can be
% named accordingly.


-type gltf_enum() :: pos_integer().
% An enumeration, in the glTF sense.


% For the various gltf_* records:
-include("gltf_support.hrl").


-type gltf_content() :: #gltf_content{}.
% Gathers all information regarding a glTF 2.0 content.


-type gltf_index() :: basic_utils:zero_index().
% Note that glTF indexes start at zero, unlike most of the Myriad indices
% (including the ones defined in the linear module).
%
% So, for glTF, "index"/"indexes" have been preferred to "indice"/"indices" to
% further reduce the risk of being mixed with linear:indice() and other Myriad
% conventions.


-type scene_index() :: gltf_index().
% Index of a scene in a glTF content.


-type gltf_scene() :: #gltf_scene{}.
% A scene defined in a glTF content.



-type node_index() :: gltf_index().
% Index of a node in a glTF content.


-type gltf_node() :: #gltf_node{}.
% A node defined in a glTF content.



-type mesh_index() :: gltf_index().
% Index of a mesh in a glTF content.


-type gltf_mesh() :: #gltf_mesh{}.
% A mesh defined in a glTF content.


-type primitive_index() :: gltf_index().
% Index of a primitive in a glTF mesh.


-type gltf_primitive() :: #gltf_primitive{}.
% A primitive defined in a mesh in a glTF content, corresponding to the data
% required for GPU draw calls.



-type gltf_attributes() :: #gltf_attributes{}.
% The (glTF) attributes of a primitive, corresponding to the vertex attributes
% used in the draw calls.


-type material_index() :: gltf_index().
% Index of a material in a glTF content.


-type gltf_material() :: #gltf_material{}.
% A material defined in a glTF content.


-type gltf_pbr_metallic_roughness() :: #gltf_pbr_metallic_roughness{}.
% Describes the (glTF) metallic roughness of a material, based on the
% Physically-Based Rendering (PBR) methodology.



-type light_index() :: gltf_index().
% Index of a light (actually: a node, as no specific type exists for light) in a
% glTF content.


-type gltf_light() :: #gltf_node{}.
% A light defined in a glTF content.
%
% As no specific type exists for light, it is a mere glTF node.



-type camera_type_index() :: gltf_index().
% Index of a camera type in a glTF content.


-type gltf_orthographic_camera() :: #gltf_orthographic_camera{}.
% A type of orthographic camera defined in a glTF content.

-type gltf_perspective_camera():: #gltf_perspective_camera{}.
% A type of perspective camera defined in a glTF content.


-type gltf_camera_type() :: gltf_orthographic_camera()
						  | gltf_perspective_camera().
% A type of camera defined in a glTF content.
%
% This corresponds to a camera type rather than a camera instance, as the actual
% cameras are created based on nodes each referring to a camera type.


-type camera_node_index() :: node_index().
% Index of a node of a camera in a glTF content.



-type buffer_index() :: gltf_index().
% Index of a buffer in a glTF content.


-type gltf_buffer() :: #gltf_buffer{}.
% A (glTF) buffer of raw data, whose elements could be vertex indexes, vertex
% attributes, animation keyframes, etc.


-type raw_buffer() :: binary().
% An actual, raw (binary) buffer, to which a glTF buffer object applies.



-type buffer_view_index() :: gltf_index().
% Index of a buffer view in a glTF content.


-type gltf_buffer_view() :: #gltf_buffer_view{}.
% A view onto a given (glTF) buffer.



-type accessor_index() :: gltf_index().
% Index of an accessor in a glTF content.


-type gltf_accessor() :: #gltf_accessor{}.
% A typed view into a (glTF) buffer view that contains raw binary data.



-type element_type() :: 'scalar'
					  | 'vector2' | 'vector3' | 'vector4'
					  | 'matrix2' | 'matrix3' | 'matrix4'.
% Specifies if the glTF elements of an accessor are scalars, vectors, or
% matrices.
%
% The (Myriad-defined) datatype of a component of that element is to be
% specified with component_type/0.


-type gltf_element_type() :: bin_string().
% Lower-level glTF specification of the datatype of a component.



-type component_type() :: type_utils:low_level_type().
% The (Myriad) datatype of a component of an accessor, for instance 'uint16'.

-type gltf_component_type() :: gltf_enum().
% A glTF lower-level type identifier. Ex: '5120' for sint8.


-type component_value() :: number().
% The value of a component of an accessor.


-type gltf_topology_type() :: gltf_enum().
% Lower-level glTF topology ("mode") of a primitive (ex: corresponding to point,
% line_loop, etc.).


-type gltf_topology() :: [ gltf_indexed_triangle() ].
% An index-based actual glTF topology of a glTF mesh.
% Note that these indexes start at 0 for glTF, unlike ours.


-type gltf_vertex_index() :: gltf_index().
% The (glTF) index of a vertex (they start at zero).


-type gltf_indexed_triangle() ::
		{ gltf_vertex_index(), gltf_vertex_index(), gltf_vertex_index() }.
% An index-based glTF triangle.
%
% Note that these indexes start at 0 for glTF, unlike ours, and that usually the
% vertex order matters (regarding culling).


-type buffer_view_target() :: 'array_buffer' | 'element_array_buffer'.
% The hint representing the intended GPU buffer type to use with this buffer
% view.


-type gltf_buffer_view_target() :: gltf_enum().
% The glTF lower-level hint representing the intended GPU buffer type to use
% with this buffer view.


-type generator_name() :: ustring().
% The name chosen for this glTF generator.


-type basic_content_settings() :: { scene_index(), material_index(),
		light_index(), camera_type_index(), camera_node_index(),
		gltf_content() }.
% All settings corresponding to a basic content.


-export_type([ object_name/0, base_name/0, gltf_object_name/0,
			   mesh_name/0, primitive_name/0,
			   gltf_enum/0, gltf_content/0, gltf_index/0,
			   scene_index/0, gltf_scene/0,
			   node_index/0, gltf_node/0,
			   mesh_index/0, gltf_mesh/0,
			   primitive_index/0, gltf_primitive/0,
			   gltf_attributes/0,
			   material_index/0, gltf_material/0,
			   gltf_pbr_metallic_roughness/0,
			   light_index/0, gltf_light/0,
			   camera_type_index/0,
			   gltf_orthographic_camera/0, gltf_perspective_camera/0,
			   gltf_camera_type/0, camera_node_index/0,
			   buffer_index/0, gltf_buffer/0, raw_buffer/0,
			   buffer_view_index/0, gltf_buffer_view/0,
			   accessor_index/0, gltf_accessor/0,
			   element_type/0, gltf_element_type/0,
			   component_type/0, gltf_component_type/0,
			   component_value/0,
			   gltf_topology_type/0, gltf_topology/0, gltf_vertex_index/0,
			   buffer_view_target/0, gltf_buffer_view_target/0,
			   generator_name/0, basic_content_settings/0 ]).


-export([ get_blank_content/0, get_basic_content/0, get_basic_content/1,

		  % Basic additions:

		  add_basics_to_content/1, add_basics_to_content/2,

		  add_basic_mesh_to_content/2,

		  add_metallic_material_to_content/1,
		  add_metallic_material_to_content/2,

		  add_basic_light_to_content/1, add_basic_light_to_content/2,

		  add_basic_camera_to_content/1, add_basic_camera_to_content/2,
		  add_camera_to_content/3, add_camera_to_content/4,

		  add_full_scene_to_content/1,


		  % Generic getters:

		  get_mesh/2,

		  % Generic setters:

		  set_default_scene/2, set_node_mesh/2,

		  add_scene_to_content/2,
		  add_node_to_content/2, add_node_to_scene/3,
		  add_light_to_content/2,
		  add_mesh_to_content/2, add_primitive_to_mesh/2,
		  add_material_to_content/2, add_camera_type_to_content/2,
		  add_accessor_to_content/2,
		  add_buffer_to_content/2, add_buffer_view_to_content/2,

		  update_mesh/3,


		  % Basic, default glTF elements:

		  get_basic_node/0, get_basic_node/1,
		  get_basic_mesh/0, get_basic_mesh/1,
		  get_metallic_material/0, get_metallic_material/1,
		  get_basic_camera_settings/0, get_basic_camera_type/0,
		  get_basic_orthographic_camera_type/0,
		  get_basic_perspective_camera_type/0,
		  get_basic_camera_node/0,

		  write_gltf_content/3, write_gltf_content/4,
		  read_gltf_content/2,

		  add_empty_mesh/2, add_primitive/8, add_primitive/9,
		  decode_primitive/4,
		  decode_vertices/5,

		  generate_buffer/3,
		  extract_points/4, extract_vectors/4,

		  file_to_gltf_buffer_embedded/1,
		  raw_buffer_to_gltf_buffer_embedded/1,
		  raw_buffer_to_gltf_buffer_embedded/2,
		  gltf_buffer_embedded_to_raw_buffer/1,

		  gltf_content_to_json/3, json_to_gltf_content/2,

		  get_element_type_associations/0,
		  element_type_to_gltf/1, gltf_to_element_type/1,

		  get_component_type_associations/0,
		  component_type_to_gltf/1, gltf_to_component_type/1,

		  get_topology_type_associations/0,
		  topology_type_to_gltf/1, gltf_to_topology_type/1,

		  get_buffer_view_target_associations/0,
		  buffer_view_target_to_gltf/1, gltf_to_buffer_view_target/1,

		  triangles_to_indexes/1, indexes_to_triangles/1,
		  indexed_triangles_to_gltf/1, indexed_triangle_to_gltf/1,


		  % Lower-level operations:
		  extract_all_uint16_little/1, extract_all_float32_little/1 ]).



% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type any_file_path() :: file_utils:any_file_path().

-type byte_size() :: system_utils:byte_size().
-type byte_offset() :: system_utils:byte_offset().


-type bijective_table( F, S ) :: bijective_table:bijective_table( F, S ).

-type json() :: json_utils:json().
-type json_term() :: json_utils:json_term().
-type parser_state() :: json_utils:parser_state().


-type dimension() :: linear:dimension().
-type indexed_triangle() :: linear:indexed_triangle().

% Default is 'triangles':
-type topology_type() :: linear_2D:topology_type().


-type coordinate() :: linear:coordinate().

-type specialised_point() :: linear:specialised_point().
-type specialised_vector() :: linear:specialised_vector().

-type specialised_vertex() :: linear:specialised_vertex().

-type specialised_normal() :: linear:specialised_normal().

-type specialised_texture_coordinates() ::
		linear:specialised_texture_coordinates().

-type specialised_type() :: linear:specialised_type().


-type scalar() :: linear:scalar().

-type point2() :: point2:point2().
-type point3() :: point3:point3().
-type point4() :: point4:point4().

-type vector2() :: vector2:vector2().
-type vector3() :: vector3:vector3().
-type vector4() :: vector4:vector4().

-type matrix2() :: matrix2:matrix2().
-type matrix3() :: matrix3:matrix3().
-type matrix4() :: matrix4:matrix4().

%-type quaternion() :: quaternion:quaternion().

-type texture_coordinate2() :: mesh:texture_coordinate2().


% Local types:

-type final_type() :: 'point' | 'vector'.
% As, when decoding, we prefer discriminating points (ex: vertices) from vectors
% (ex: normals).


% No index() here (they are uint16 scalar()):
-type buffer_elements() :: scalar()
						 | point2() | point3() | point4()
						 | vector2() | vector3() | vector4()
						 | matrix2() | matrix3() | matrix4().
% The elements that can be written to or extracted from a buffer-view.


-type buffer_table() :: table( buffer_index(), raw_buffer() ).
% A table associating to a given buffer index its in-memory, decoded,
% readily-usable binary.



% So that we can use the 'table' pseudo-module, knowing that JSON parsers rely
% on maps:
%
-define( table_type, map_hashtable ).



-define( default_generator_name, "Ceylan-Myriad glTF exporter" ).



% @doc Returns a blank glTF content.
-spec get_blank_content() -> gltf_content().
get_blank_content() ->
	#gltf_content{}.



% @doc Returns all settings regarding a basic glTF content, with defaults in
% terms of scene, material, light, camera, etc.
%
-spec get_basic_content() -> basic_content_settings().
get_basic_content() ->
	add_basics_to_content( get_blank_content() ).



% @doc Returns all settings regarding a basic glTF content, with defaults in
% terms of scene, material, light, camera, etc., and specified base name for the
% various named elements.
%
-spec get_basic_content( base_name() ) -> basic_content_settings().
get_basic_content( BaseName ) ->
	add_basics_to_content( get_blank_content(), BaseName ).



% @doc Adds the basics to the specified glTF content: default scene, material,
% light, camera, etc.
%
-spec add_basics_to_content( gltf_content() ) -> basic_content_settings().
add_basics_to_content( Content  ) ->

	{ MaterialIndex, MatContent } = add_metallic_material_to_content( Content ),

	{ LightNodeIndex, LightContent } = add_basic_light_to_content( MatContent ),

	{ CameraTypeIndex, CameraNodeIndex, CamContent } =
		add_basic_camera_to_content( LightContent ),

	{ SceneIndex, SceneContent } = add_full_scene_to_content( CamContent ),

	DefContent = set_default_scene( SceneIndex, SceneContent ),

	{ SceneIndex, MaterialIndex, LightNodeIndex,
	  CameraTypeIndex, CameraNodeIndex, DefContent }.



% @doc Adds named basics to the specified glTF content: default scene, material,
% light, camera, etc.
%
-spec add_basics_to_content( gltf_content(), base_name() ) ->
										basic_content_settings().
add_basics_to_content( Content, BaseName ) ->

	{ MaterialIndex, MatContent } =
		add_metallic_material_to_content( Content,
										  "Material for " ++ BaseName ),

	{ LightNodeIndex, LightContent } =
		add_basic_light_to_content( MatContent, "Light for " ++ BaseName ),

	{ CameraTypeIndex, CameraNodeIndex, CamContent } =
		add_basic_camera_to_content( LightContent, "Camera for " ++ BaseName ),

	{ SceneIndex, SceneContent } = add_full_scene_to_content( CamContent ),

	DefContent = set_default_scene( SceneIndex, SceneContent ),

	{ SceneIndex, MaterialIndex, LightNodeIndex,
	  CameraTypeIndex, CameraNodeIndex, DefContent }.



% @doc Adds a basic mesh, based on specified primitive, to the specified glTF
% content, by creating a dedicated basic node; returns the index of that mesh,
% and the updated content.
%
-spec add_basic_mesh_to_content( gltf_primitive(), gltf_content() ) ->
		{ node_index(), mesh_index(), primitive_index(), gltf_content() }.
add_basic_mesh_to_content( Primitive, Content ) ->

	% By design PrimIndex=0:
	{ PrimIndex, MeshWithPrim } =
		add_primitive_to_mesh( Primitive, get_basic_mesh() ),

	% Registers this new mesh:
	{ MeshIndex, ContentWithMesh } =
		add_mesh_to_content( MeshWithPrim, Content ),

	% Creates a suitable node referencing that new mesh:
	NodeWithMesh = set_node_mesh( MeshIndex, get_basic_node() ),

	{ NodeIndex, ContentWithNode } =
		add_node_to_content( NodeWithMesh, ContentWithMesh ),

	{ NodeIndex, MeshIndex, PrimIndex, ContentWithNode }.



% @doc Adds a basic, metallic material to the specified glTF content; returns
% the index of that material and the updated content.
%
-spec add_metallic_material_to_content( gltf_content() ) ->
								{ material_index(), gltf_content() }.
add_metallic_material_to_content( Content ) ->
	add_material_to_content( get_metallic_material(), Content ).


% @doc Adds a basic, named, metallic material to the specified glTF content;
% returns the index of that material and the updated content.
%
-spec add_metallic_material_to_content( gltf_content(), base_name() ) ->
								{ material_index(), gltf_content() }.
add_metallic_material_to_content( Content, BaseName ) ->
	add_material_to_content( get_metallic_material( BaseName ), Content ).



% @doc Adds a basic light to the specified glTF content; returns the index of
% that light and the updated content.
%
-spec add_basic_light_to_content( gltf_content() ) ->
									{ light_index(), gltf_content() }.
add_basic_light_to_content( Content ) ->
	add_light_to_content( get_basic_light(), Content ).


% @doc Adds a basic, named, light to the specified glTF content; returns the
% index of that light and the updated content.
%
-spec add_basic_light_to_content( gltf_content(), base_name() ) ->
									{ light_index(), gltf_content() }.
add_basic_light_to_content( Content, BaseName ) ->
	add_light_to_content( get_basic_light( BaseName ), Content ).


% @doc Adds a basic unamed camera to the specified glTF content; returns the
% index of that camera type and node, and the updated content.
%
-spec add_basic_camera_to_content( gltf_content() ) ->
			{ camera_type_index(), camera_node_index(), gltf_content() }.
add_basic_camera_to_content( Content ) ->
	{ BasicCamType, BasicCamNode } = get_basic_camera_settings(),
	add_camera_to_content( BasicCamType, BasicCamNode, Content ).


% @doc Adds a basic named camera to the specified glTF content; returns the
% index of that camera type and node, and the updated content.
%
-spec add_basic_camera_to_content( gltf_content(), base_name() ) ->
			{ camera_type_index(), camera_node_index(), gltf_content() }.
add_basic_camera_to_content( Content, BaseName ) ->
	{ BasicCamType, BasicCamNode } = get_basic_camera_settings(),
	add_camera_to_content( BasicCamType, BasicCamNode, Content, BaseName ).



% @doc Adds specified unamed camera to the specified glTF content; returns the
% index of that camera type and of a node instantiating it, and the updated
% content.
%
% Updates the specified node so that it references this camera type (hence
% creating an instance thereof); any previous camera is replaced.
%
-spec add_camera_to_content( gltf_camera_type(), gltf_node(),
							 gltf_content() ) ->
			{ camera_type_index(), node_index(), gltf_content() }.
add_camera_to_content( CameraType, CameraNode, Content ) ->
	add_camera_to_content( CameraType, CameraNode, Content,
						   _BaseName="Basic Myriad camera node instance" ).



% @doc Adds specified named camera to the specified glTF content; returns the
% index of that camera type and of a node instantiating it, and the updated
% content.
%
% Updates the specified node so that it references this camera type (hence
% creating an instance thereof); any previous camera is replaced.
%
-spec add_camera_to_content( gltf_camera_type(), gltf_node(),
							 gltf_content(), base_name() ) ->
			{ camera_type_index(), node_index(), gltf_content() }.
add_camera_to_content( CameraType, CameraNode,
					   Content=#gltf_content{ nodes=Nodes,
											  camera_types=CameraTypes },
					   BaseName )
  when ( is_record( CameraType, gltf_orthographic_camera )
		 orelse is_record( CameraType, gltf_perspective_camera ) )
	   andalso is_record( CameraNode, gltf_node ) ->

	% As these indexes start at 0:
	CameraTypeIndex = length( CameraTypes ),

	NewCameraTypes = list_utils:append_at_end( CameraType, CameraTypes ),

	CameraNodeIndex = length( Nodes ),

	UpdatedCameraNode = CameraNode#gltf_node{
		name=BaseName,
		camera=CameraTypeIndex },

	NewNodes = list_utils:append_at_end( UpdatedCameraNode, Nodes ),

	{ CameraTypeIndex, CameraNodeIndex, Content#gltf_content{
											nodes=NewNodes,
											camera_types=NewCameraTypes } }.



% @doc Adds a full scene to the specified glTF content, that is a scene
% comprising all known nodes.
%
-spec add_full_scene_to_content( gltf_content() ) ->
										{ scene_index(), gltf_content() }.
add_full_scene_to_content( Content=#gltf_content{ nodes=Nodes } ) ->

	% Enumerate all nodes:
	NodeIndexes = lists:seq( _From=0, _To=length( Nodes )-1 ),

	% All content nodes selected:
	FullScene = #gltf_scene{ name="Basic Myriad full scene",
							 nodes=NodeIndexes },

	add_scene_to_content( FullScene, Content ).



% Section for generic getters of glTF elements.


-spec get_mesh( mesh_index(), gltf_content() ) -> gltf_mesh().
get_mesh( MeshIndex, Content ) ->
	list_utils:get_element_at( Content#gltf_content.meshes, MeshIndex+1 ).



% Section for generic setters of glTF elements.


% @doc Sets the specified scene as the default one (overriding any prior).
-spec set_default_scene( scene_index(), gltf_content() ) -> gltf_content().
set_default_scene( SceneIndex, Content ) ->
	Content#gltf_content{ default_scene=SceneIndex }.



% @doc Sets the specified mesh index of the specified glTF node; returns the
% updated node.
%
-spec set_node_mesh( mesh_index(), gltf_node() ) -> gltf_node().
set_node_mesh( MeshIndex, Node ) ->
	Node#gltf_node{ mesh=MeshIndex }.



% @doc Adds the specified scene to the specified glTF content; returns the index
% of that scene, and the updated content.
%
-spec add_scene_to_content( gltf_scene(), gltf_content() ) ->
								{ scene_index(), gltf_content() }.
add_scene_to_content( Scene,
					  Content=#gltf_content{ scenes=Scenes } )
							when is_record( Scene, gltf_scene ) ->

	SceneIndex = length( Scenes ),

	NewScenes = list_utils:append_at_end( Scene, Scenes ),

	{ SceneIndex, Content#gltf_content{ scenes=NewScenes } }.



% @doc Adds the specified node to the specified glTF content; returns the index
% of that node, and the updated content.
%
-spec add_node_to_content( gltf_node(), gltf_content() ) ->
							{ node_index(), gltf_content() }.
add_node_to_content( Node,
					 Content=#gltf_content{ nodes=Nodes } )
						when is_record( Node, gltf_node ) ->

	NodeIndex = length( Nodes ),

	NewNodes = list_utils:append_at_end( Node, Nodes ),

	{ NodeIndex, Content#gltf_content{ nodes=NewNodes } }.



% @doc Adds the specified node to the specified scene of specified glTF content;
% returns the updated content.
%
-spec add_node_to_scene( node_index(), scene_index(), gltf_content() ) ->
													gltf_content().
add_node_to_scene( NodeIndex, SceneIndex,
				   Content=#gltf_content{ scenes=Scenes } ) ->

	ActualIndex = SceneIndex+1,

	Scene = list_utils:get_element_at( Scenes, ActualIndex ),

	% Yet order not expected to matter:
	NewNodes = list_utils:append_at_end( NodeIndex, Scene#gltf_scene.nodes ),

	NewScene = Scene#gltf_scene{ nodes=NewNodes },

	NewScenes = list_utils:set_element_at( NewScene, Scenes, ActualIndex ),

	Content#gltf_content{ scenes=NewScenes }.



% @doc Adds the specified light to the specified glTF content; returns the index
% of that light and the updated content.
%
% Note that lights do not exist per se for glTF: they are mere nodes.
%
-spec add_light_to_content( gltf_light(), gltf_content() ) ->
								  { light_index(), gltf_content() }.
add_light_to_content( Light,
					  Content=#gltf_content{ nodes=Nodes } )
							when is_record( Light, gltf_node )->

	% As these indexes start at 0:
	LightNodeIndex = length( Nodes ),

	NewNodes = list_utils:append_at_end( Light, Nodes ),

	{ LightNodeIndex, Content#gltf_content{ nodes=NewNodes } }.



% @doc Adds the specified material to the specified glTF content; returns the
% index of that material, and the updated content.
%
-spec add_material_to_content( gltf_material(), gltf_content() ) ->
									{ material_index(), gltf_content() }.
add_material_to_content( Material,
						 Content=#gltf_content{ materials=Materials } )
		when is_record( Material, gltf_material ) ->

	MaterialIndex = length( Materials ),

	NewMaterials = list_utils:append_at_end( Material, Materials ),

	{ MaterialIndex, Content#gltf_content{ materials=NewMaterials } }.



% @doc Adds the specified camera type to the specified glTF content; returns the
% index of that camera type, and the updated content.
%
-spec add_camera_type_to_content( gltf_camera_type(), gltf_content() ) ->
									{ camera_type_index(), gltf_content() }.
add_camera_type_to_content( CameraType,
		Content=#gltf_content{ camera_types=CameraTypes } )
  when is_record( CameraType, gltf_orthographic_camera )
	   orelse is_record( CameraType, gltf_perspective_camera ) ->

	CameraTypeIndex = length( CameraTypes ),

	NewCameraTypes = list_utils:append_at_end( CameraType, CameraTypes ),

	{ CameraTypeIndex, Content#gltf_content{ camera_types=NewCameraTypes } }.




% @doc Adds the specified mesh to the specified glTF content; returns the index
% of that mesh, and the updated content.
%
-spec add_mesh_to_content( gltf_mesh(), gltf_content() ) ->
										{ mesh_index(), gltf_content() }.
add_mesh_to_content( Mesh, Content=#gltf_content{ meshes=Meshes } )
								when is_record( Mesh, gltf_mesh ) ->

	MeshIndex = length( Meshes ),

	NewMeshes = list_utils:append_at_end( Mesh, Meshes ),

	{ MeshIndex, Content#gltf_content{ meshes=NewMeshes } }.



% @doc Adds the specified primitive to the specified glTF mesh; returns the
% index of that primitive, and the updated mesh.
%
-spec add_primitive_to_mesh( gltf_primitive(), gltf_mesh() ) ->
									{ primitive_index(), gltf_mesh() }.
add_primitive_to_mesh( Primitive,
					   Mesh=#gltf_mesh{ primitives=Primitives } )
								when is_record( Primitive, gltf_primitive ) ->

	PrimitiveIndex = length( Primitives ),

	NewPrimitives = list_utils:append_at_end( Primitive, Primitives ),

	{ PrimitiveIndex, Mesh#gltf_mesh{ primitives=NewPrimitives } }.



% @doc Adds the specified accessor to the specified glTF content; returns the
% index of that accessor, and the updated content.
%
-spec add_accessor_to_content( gltf_accessor(), gltf_content() ) ->
										{ accessor_index(), gltf_content() }.
add_accessor_to_content( Accessor,
						 Content=#gltf_content{ accessors=Accessors } )
					when is_record( Accessor, gltf_accessor ) ->

	AccessorIndex = length( Accessors ),

	NewAccessors = list_utils:append_at_end( Accessor, Accessors ),

	{ AccessorIndex, Content#gltf_content{ accessors=NewAccessors } }.



% @doc Adds the specified buffer to the specified glTF content; returns the
% index of that buffer, and the updated content.
%
-spec add_buffer_to_content( gltf_buffer(), gltf_content() ) ->
										{ buffer_index(), gltf_content() }.
add_buffer_to_content( Buffer,
					   Content=#gltf_content{ buffers=Buffers } )
					when is_record( Buffer, gltf_buffer ) ->

	BufferIndex = length( Buffers ),

	NewBuffers = list_utils:append_at_end( Buffer, Buffers ),

	{ BufferIndex, Content#gltf_content{ buffers=NewBuffers } }.



% @doc Adds the specified buffer-view to the specified glTF content; returns the
% index of that buffer-view, and the updated content.
%
-spec add_buffer_view_to_content( gltf_buffer_view(), gltf_content() ) ->
							{ buffer_view_index(), gltf_content() }.
add_buffer_view_to_content( BufferView,
							Content=#gltf_content{ buffer_views=BufferViews } )
				when is_record( BufferView, gltf_buffer_view ) ->

	BufferViewIndex = length( BufferViews ),

	NewBufferViews = list_utils:append_at_end( BufferView, BufferViews ),

	{ BufferViewIndex, Content#gltf_content{ buffer_views=NewBufferViews } }.



% @doc Updates the specified mesh in content with the specified one.
-spec update_mesh( gltf_mesh(), mesh_index(), gltf_content() ) ->
											gltf_content().
update_mesh( Mesh, MeshIndex, Content=#gltf_content{ meshes=Meshes } ) ->
	NewMeshes = list_utils:set_element_at( Mesh, Meshes, MeshIndex ),
	Content#gltf_content{ meshes=NewMeshes }.



% Section for basic, default glTF elements.



% @doc Returns a basic, empty, unregistered glTF scene node.
-spec get_basic_node() -> gltf_node().
get_basic_node() ->
	get_basic_node( "Basic Myriad node" ).


% @doc Returns a basic, empty, named, unregistered glTF scene node.
-spec get_basic_node( object_name() ) -> gltf_node().
get_basic_node( NodeName ) ->
	#gltf_node{ name=NodeName }.



% @doc Returns a basic, empty, unregistered glTF mesh.
-spec get_basic_mesh() -> gltf_mesh().
get_basic_mesh() ->
	get_basic_mesh( "Basic Myriad mesh" ).


% @doc Returns a basic, empty, named, unregistered glTF mesh.
-spec get_basic_mesh( object_name() ) -> gltf_mesh().
get_basic_mesh( MeshName ) ->
	#gltf_mesh{ name=MeshName }.



% @doc Returns a basic, metallic, double-sided, unregistered glTF material.
-spec get_metallic_material() -> gltf_material().
get_metallic_material() ->
	get_metallic_material( "Basic Myriad metallic material" ).



% @doc Returns a basic, metallic, double-sided, named, unregistered glTF
% material.
%
-spec get_metallic_material( object_name() ) -> gltf_material().
get_metallic_material( MaterialName ) ->

	ColorCoord = 0.800000011920929,

	MetalRoughness = #gltf_pbr_metallic_roughness{
		base_color_factor={ ColorCoord, ColorCoord, ColorCoord, 1.0 },
		metallic_factor=0.0,
		roughness_factor=0.4000000059604645 },

	#gltf_material{ name=MaterialName,
					double_sided=true,
					pbr_metallic_roughness=MetalRoughness }.



% @doc Returns a basic, unregistered glTF light (as a node).
-spec get_basic_light() -> gltf_light().
get_basic_light() ->
	get_basic_light( "Basic Myriad light" ).


% @doc Returns a basic, unregistered, named glTF light (as a node).
-spec get_basic_light( object_name() ) -> gltf_light().
get_basic_light( LightName ) ->

	% In glTF, lights are not first-class citizens, as they shall be described
	% as meshes having at least one light-emitting material.
	%
	% So here we only return a (named, currently empty) node, a corresponding
	% light-emitting mesh could/should be added for this light to become
	% functional:

	LightRotQuaternion = [ 0.16907575726509094,
						   0.7558803558349609,
						   -0.27217137813568115,
						   0.570947527885437 ],

	LightPosition = [ 4.076245307922363,
					  5.903861999511719,
					  -1.0054539442062378 ],

	#gltf_node{ name=LightName,
				rotation=LightRotQuaternion,
				translation=LightPosition }.



% @doc Returns basic glTF camera settings: a (perspective) camera type and a
% node (not yet referencing it, as the camera type has not been registered and
% thus has not an index yet).
%
-spec get_basic_camera_settings() -> { gltf_camera_type(), gltf_node() }.
get_basic_camera_settings() ->
	{ get_basic_camera_type(), get_basic_camera_node() }.



% @doc Returns a basic (perspective) glTF camera type.
-spec get_basic_camera_type() -> gltf_camera_type().
get_basic_camera_type() ->
	get_basic_perspective_camera_type().




% @doc Returns a basic orthographic glTF camera type.
-spec get_basic_orthographic_camera_type() -> gltf_camera_type().
get_basic_orthographic_camera_type() ->
	#gltf_orthographic_camera{ name="Basic Myriad orthographic camera type",
							   x_magnification=1.0,
							   y_magnification=1.0,
							   z_near_distance=0.01,
							   z_far_distance=1000.0 }.



% @doc Returns a basic perspective glTF camera type.
-spec get_basic_perspective_camera_type() -> gltf_camera_type().
get_basic_perspective_camera_type() ->
	#gltf_perspective_camera{ name="Basic Myriad perspective camera type",
							  aspect_ratio=1.5,
							  % About 37.8°:
							  y_field_of_view=0.660593,
							  % Ought not be out of range: z_near_distance=0.0,
							  z_near_distance=0.1,
							  % Preferring here infinite perspective:
							  z_far_distance=undefined }.



% @doc Returns a basic perspective glTF camera node.
-spec get_basic_camera_node() -> gltf_node().
get_basic_camera_node() ->

	CameraRotQuaternion = [ 0.483536034822464,
							0.33687159419059753,
							-0.20870360732078552,
							0.7804827094078064 ],

	CameraPosition = [ 7.358891487121582,
					   4.958309173583984,
					   6.925790786743164 ],

	#gltf_node{ name="Basic Myriad camera node",
				rotation=CameraRotQuaternion,
				translation=CameraPosition }.



% @doc Writes the specified glTF content in the specified file, which is
% expected not to exist already, using a default generator name and the
% specified state of the JSON parser.
%
-spec write_gltf_content( gltf_content(), any_file_path(), parser_state() ) ->
			void().
write_gltf_content( GlTfContent, OutputFilePath, ParserState ) ->
	write_gltf_content( GlTfContent, OutputFilePath, ?default_generator_name,
						ParserState ).



% @doc Writes the specified glTF content in the specified file, which is
% expected not to exist, using specified generator name and the specified state
% of the JSON parser.
%
-spec write_gltf_content( gltf_content(), any_file_path(), generator_name(),
						  parser_state() ) -> void().
write_gltf_content( GlTfContent, OutputFilePath, GeneratorName,
					ParserState ) ->

	cond_utils:if_defined( myriad_debug_gltf_support, trace_utils:debug_fmt(
		"Writing glTF content in '~ts' as '~ts'.",
		[ OutputFilePath, GeneratorName ] ) ),

	BinJsonContent = gltf_content_to_json( GlTfContent, GeneratorName,
										   ParserState ),

	file_utils:write_whole( OutputFilePath, BinJsonContent ).



% @doc Reads the glTF content defined in the specified glTF file.
-spec read_gltf_content( any_file_path(), parser_state() ) -> gltf_content().
read_gltf_content( InputFilePath, ParserState ) ->

	cond_utils:if_defined( myriad_debug_gltf_support, trace_utils:debug_fmt(
		"Reading glTF content from '~ts'.", [ InputFilePath ] ) ),

	file_utils:is_existing_file_or_link( InputFilePath ) orelse
		begin
			trace_utils:error_fmt( "Error, input glTF file '~ts' not found.",
								   [ InputFilePath ] ),
			throw( { gltf_file_not_found, InputFilePath } )
		end,

	BinJsonFContent = file_utils:read_whole( InputFilePath ),

	json_to_gltf_content( BinJsonFContent, ParserState ).



% @doc Returns a glTF buffer corresponding to the specified (binary) file,
% embedding the file content directly into a relevant base64-encoded URI, and
% named accordingly.
%
-spec file_to_gltf_buffer_embedded( any_file_path() ) -> gltf_buffer().
file_to_gltf_buffer_embedded( FilePath ) ->

	BinContent = file_utils:read_whole( FilePath ),

	cond_utils:if_defined( myriad_debug_gltf_support, trace_utils:debug_fmt(
		"Embedding file '~ts' (size: ~B bytes) in a glTF buffer.",
		[ FilePath, size( BinContent ) ] ) ),

	BufferName = text_utils:format(
		"Buffer whose content was in the '~ts' file.", [ FilePath ] ),

	raw_buffer_to_gltf_buffer_embedded( BinContent, BufferName ).



% @doc Returns an (anonymous) glTF buffer corresponding to the specified binary,
% embedding its content directly into a relevant base64-encoded URI.
%
-spec raw_buffer_to_gltf_buffer_embedded( raw_buffer() ) -> gltf_buffer().
raw_buffer_to_gltf_buffer_embedded( BinContent ) ->
	raw_buffer_to_gltf_buffer_embedded( BinContent,
										_MaybeBufferName=undefined ).



% @doc Returns a glTF buffer corresponding to the specified binary, embedding
% its content directly into a relevant base64-encoded URI.
%
-spec raw_buffer_to_gltf_buffer_embedded( raw_buffer(),
								maybe( object_name() ) ) -> gltf_buffer().
raw_buffer_to_gltf_buffer_embedded( BinContent, MaybeBufferName ) ->

	Base64Uri = "data:application/octet-stream;base64,"
					++ base64:encode_to_string( BinContent ),

	ByteCount = size( BinContent ),

	#gltf_buffer{ name=MaybeBufferName,
				  uri=Base64Uri,
				  size=ByteCount }.



% @doc Returns a binary corresponding to the specified glTF buffer.
-spec gltf_buffer_embedded_to_raw_buffer( gltf_buffer() ) -> raw_buffer().
gltf_buffer_embedded_to_raw_buffer( #gltf_buffer{ uri=Base64Uri,
												  size=ByteCount } ) ->

	case Base64Uri of

		"data:application/octet-stream;base64," ++ Base64Content ->
			Bin = base64:decode( Base64Content ),

			case size( Bin ) of

				ByteCount ->
					Bin;

				OtherCount ->
					throw( { binary_decoding_failed, wrong_size,
						{ expected, ByteCount }, { got, OtherCount } } )

			end;

		_ ->
			throw( { mime_prefix_not_found, Base64Uri } )

	end.




% Subsection to convert from internal (glTF) representation to JSON.


% @doc Converts the specified glTF content into a JSON counterpart.
-spec gltf_content_to_json( gltf_content(), generator_name(),
							parser_state() ) -> json().
gltf_content_to_json( #gltf_content{ default_scene=DefaultSceneId,
									 scenes=Scenes,
									 nodes=Nodes,
									 materials=Materials,
									 camera_types=CameraTypes,
									 meshes=Meshes,
									 accessors=Accessors,
									 buffer_views=BufferViews,
									 buffers=Buffers },
					  GeneratorName, ParserState ) ->

	% We create a (table-based) json_term() according to the mapping rules:

	BinGeneratorName = text_utils:string_to_binary( GeneratorName ),
	BinGlTfVersionString = text_utils:string_to_binary( ?gltf_version_string ),

	BaseTable = table:new( [

		{ <<"asset">>, #{ <<"generator">> => BinGeneratorName,
						  <<"version">> => BinGlTfVersionString } },

		{ <<"scene">>, DefaultSceneId },

		{ <<"scenes">>, gltf_scenes_to_json( Scenes ) },

		{ <<"nodes">>, gltf_nodes_to_json( Nodes ) },

		{ <<"materials">>, gltf_materials_to_json( Materials ) },

		{ <<"cameras">>, gltf_camera_types_to_json( CameraTypes ) },

		{ <<"meshes">>, gltf_meshes_to_json( Meshes ) },

		{ <<"accessors">>, gltf_accessors_to_json( Accessors ) },

		{ <<"bufferViews">>, gltf_buffer_views_to_json( BufferViews ) },

		{ <<"buffers">>, gltf_buffers_to_json( Buffers ) } ] ),

	% Thus from json_term() to json():
	JsonContent = json_utils:to_json( BaseTable, ParserState ),

	%cond_utils:if_defined( myriad_debug_gltf_support, trace_utils:debug_fmt(
	%   "Converted glTF content in following JSON:~n~p", [ JsonContent ] ) ),

	JsonContent.



% @doc Converts the specified glTF scenes into JSON counterparts.
-spec gltf_scenes_to_json( [ gltf_scene() ] ) -> json_term().
gltf_scenes_to_json( Scenes ) ->
	[ gltf_scene_to_json( S ) || S <- Scenes ].


% @doc Converts the specified glTF scene into a JSON counterpart.
-spec gltf_scene_to_json( gltf_scene() ) -> json_term().
gltf_scene_to_json( #gltf_scene{ name=MaybeName,
								 nodes=NodeIds } ) ->

	BaseTable = table:new( [ { <<"nodes">>, NodeIds } ] ),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) } ],
		BaseTable ).


% @doc Converts the specified glTF nodes into JSON counterparts.
-spec gltf_nodes_to_json( [ gltf_node() ] ) -> json_term().
gltf_nodes_to_json( Nodes ) ->
	[ gltf_node_to_json( N ) || N <- Nodes ].


% @doc Converts the specified glTF node into a JSON counterpart.
-spec gltf_node_to_json( gltf_node() ) -> json_term().
gltf_node_to_json( #gltf_node{ name=MaybeName,
							   mesh=MaybeMeshId,
							   rotation=MaybeRotQuat,
							   translation=MaybeTransVec,
							   camera=MaybeCamId } ) ->

	BaseTable = table:new(),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) },
		{ <<"mesh">>, MaybeMeshId },
		{ <<"rotation">>, MaybeRotQuat },
		{ <<"translation">>, MaybeTransVec },
		{ <<"camera">>, MaybeCamId } ], BaseTable ).



% @doc Converts the specified glTF materials into JSON counterparts.
-spec gltf_materials_to_json( [ gltf_material() ] ) -> json_term().
gltf_materials_to_json( Materials ) ->
	[ gltf_material_to_json( M ) || M <- Materials ].


% @doc Converts the specified glTF material into a JSON counterpart.
-spec gltf_material_to_json( gltf_material() ) -> json_term().
gltf_material_to_json( #gltf_material{ name=MaybeName,
									   double_sided=MaybeDoubleSided,
									   pbr_metallic_roughness=Roughness } ) ->

	BaseTable = table:new(),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) },
		{ <<"doubleSided">>, MaybeDoubleSided },
		{ <<"pbrMetallicRoughness">>, gltf_roughness_to_json( Roughness ) } ],
							 BaseTable ).


% @doc Converts the specified glTF roughness into a JSON counterpart.
-spec gltf_roughness_to_json( gltf_pbr_metallic_roughness() ) -> json_term().
gltf_roughness_to_json( #gltf_pbr_metallic_roughness{
							base_color_factor=BaseRenderColor,
							metallic_factor=MetalF,
							roughness_factor=RoughF } ) ->

	table:new( [ { <<"baseColorFactor">>, tuple_to_list( BaseRenderColor ) },
				 { <<"metallicFactor">>, MetalF },
				 { <<"roughnessFactor">>, RoughF } ] ).



% @doc Converts the specified glTF camera types into JSON counterparts.
-spec gltf_camera_types_to_json( [ gltf_camera_type() ] ) -> json_term().
gltf_camera_types_to_json( CameraTypes ) ->
	[ gltf_camera_type_to_json( CT ) || CT <- CameraTypes ].


% @doc Converts the specified glTF camera type into a JSON counterpart.
-spec gltf_camera_type_to_json( gltf_camera_type() ) -> json_term().
gltf_camera_type_to_json( #gltf_orthographic_camera{ name=MaybeName,
													 x_magnification=XMag,
													 y_magnification=YMag,
													 z_near_distance=ZNear,
													 z_far_distance=ZFar } ) ->

	OrthoTable = table:new( [ { <<"xmag">>, XMag },
							  { <<"ymag">>, YMag },
							  { <<"znear">>, ZNear },
							  { <<"zfar">>, ZFar } ] ),

	BaseTable = table:new( [ { <<"type">>, <<"orthographic">> },
							 { <<"orthographic">>, OrthoTable } ] ),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) } ],
							 BaseTable );

gltf_camera_type_to_json( #gltf_perspective_camera{
		name=MaybeName,
		aspect_ratio=MaybeAspectRatio,
		y_field_of_view=YFoV,
		z_near_distance=ZNear,
		z_far_distance=MaybeZFar } ) ->

	BasePerspTable = table:new( [ { <<"yfov">>, YFoV },
								  { <<"znear">>, ZNear } ] ),

	PerspTable = table:add_maybe_entries( [
					{ <<"aspectRatio">>, MaybeAspectRatio },
					{ <<"zfar">>, MaybeZFar } ], BasePerspTable ),

	BaseTable = table:new( [ { <<"type">>, <<"perspective">> },
							 { <<"perspective">>, PerspTable } ] ),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) } ],
							 BaseTable ).



% @doc Converts the specified glTF meshes into JSON counterparts.
-spec gltf_meshes_to_json( [ gltf_mesh() ] ) -> json_term().
gltf_meshes_to_json( Meshes ) ->
	[ gltf_mesh_to_json( M ) || M <- Meshes ].


% @doc Converts the specified glTF camera mesh into a JSON counterpart.
-spec gltf_mesh_to_json( gltf_mesh() ) -> json_term().
gltf_mesh_to_json( #gltf_mesh{ name=MaybeName,
							   primitives=Primitives } ) ->

	BaseTable = table:new( [
		{ <<"primitives">>, gltf_primitives_to_json( Primitives )  } ] ),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) } ],
							 BaseTable ).



% @doc Converts the specified glTF primitives into JSON counterparts.
-spec gltf_primitives_to_json( [ gltf_primitive()] ) -> json_term().
gltf_primitives_to_json( Primitives ) ->
	[ gltf_primitive_to_json( P ) || P <- Primitives ].


% @doc Converts the specified glTF primitive into a JSON counterpart.
gltf_primitive_to_json( #gltf_primitive{ attributes=Attributes,
										 indexes=MaybeAccessIdx,
										 material=MaybeMaterialIdx,
										 mode=MaybeTopologyType } ) ->

	BaseTable = table:new(
		[ { <<"attributes">>, gltf_attributes_to_json( Attributes ) } ] ),

	table:add_maybe_entries( [
		{ <<"indices">>, MaybeAccessIdx },
		{ <<"material">>, MaybeMaterialIdx },
		{ <<"mode">>, maybe_topology_to_gltf( MaybeTopologyType ) } ],
							 BaseTable ).



% @doc Converts the specified glTF attributes into JSON counterparts.
-spec gltf_attributes_to_json( gltf_attributes() ) -> json_term().
gltf_attributes_to_json( #gltf_attributes{ position=MaybePosition,
										   normal=MaybeNormal,
										   tangent=MaybeTangent,
										   texcoord_0=MaybeTexCoord0 } ) ->

	BaseTable = table:new(),

	table:add_maybe_entries( [
		{ <<"POSITION">>, MaybePosition },
		{ <<"NORMAL">>, MaybeNormal },
		{ <<"TANGENT">>, MaybeTangent },
		{ <<"TEXCOORD_0">>, MaybeTexCoord0 } ], BaseTable ).


% @doc Converts the specified glTF accessors into JSON counterparts.
-spec gltf_accessors_to_json( [ gltf_accessor() ] ) -> json_term().
gltf_accessors_to_json( Accessors ) ->
	[ gltf_accessor_to_json( A ) || A <- Accessors ].


% @doc Converts the specified glTF accessor into a JSON counterpart.
gltf_accessor_to_json( #gltf_accessor{ name=MaybeName,
									   buffer_view=MaybeBufferViewIndex,
									   element_type=ElemType,
									   component_type=ComponentType,
									   count=ElemCount,
									   max=MaybeMax,
									   min=MaybeMin } ) ->
	BaseTable = table:new( [
		{ <<"type">>, element_type_to_gltf( ElemType ) },
		{ <<"componentType">>, component_type_to_gltf( ComponentType ) },
		{ <<"count">>, ElemCount } ] ),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) },
		{ <<"bufferView">>, MaybeBufferViewIndex },
		{ <<"max">>, MaybeMax },
		{ <<"min">>, MaybeMin } ], BaseTable ).



% @doc Converts the specified glTF buffer-views into JSON counterparts.
-spec gltf_buffer_views_to_json( [ gltf_buffer_view() ] ) -> json_term().
gltf_buffer_views_to_json( BufferViews ) ->
	[ gltf_buffer_view_to_json( BV ) || BV <- BufferViews ].


% @doc Converts the specified glTF buffer-view into a JSON counterpart.
-spec gltf_buffer_view_to_json( gltf_buffer_view() ) -> json_term().
gltf_buffer_view_to_json( #gltf_buffer_view{ buffer=BufferIdx,
											 offset=MaybeOffset,
											 size=Size } ) ->

	BaseTable = table:new( [ { <<"buffer">>, BufferIdx },
							 { <<"byteLength">>, Size } ] ),

	table:add_maybe_entries( [ { <<"byteOffset">>, MaybeOffset } ], BaseTable );

gltf_buffer_view_to_json( Other ) ->
	throw( { unexpected_buffer_view, Other } ).



% @doc Converts the specified glTF buffers into JSON counterparts.
-spec gltf_buffers_to_json( [ gltf_buffer() ] ) -> json_term().
gltf_buffers_to_json( Buffers ) ->
	[ gltf_buffer_to_json( B ) || B <- Buffers ].


% @doc Converts the specified glTF buffer into a JSON counterpart.
-spec gltf_buffer_to_json( gltf_buffer() ) -> json_term().
gltf_buffer_to_json( #gltf_buffer{ name=MaybeName,
								   uri=UriStr,
								   size=Size } ) ->

	BinUri = text_utils:string_to_binary( UriStr ),

	BaseTable = table:new( [ { <<"byteLength">>, Size },
							 { <<"uri">>, BinUri } ] ),

	table:add_maybe_entries( [
		{ <<"name">>, text_utils:maybe_string_to_binary( MaybeName ) } ],
							 BaseTable ).




% Subsection to convert from JSON to internal (glTF) representation.


% @doc Converts the specified JSON content into an (internal) glTF counterpart.
%
% Note: not implemented yet.
%
-spec json_to_gltf_content( json(), parser_state() ) -> gltf_content().
json_to_gltf_content( JSonContent, _ParserState ) ->

	%trace_utils:debug_fmt( "Decoding following raw JSON to glTF content:~n ~p",
	%                       [ JSonContent ] ),

	JsonTerm = json_utils:from_json( JSonContent ),

	%trace_utils:debug_fmt( "Decoded JSON term:~n ~p", [ JsonTerm ] ),

	BufferEntries = case table:lookup_entry( _K= <<"buffers">>, JsonTerm ) of

		{ value, V } ->
			V;

		key_not_found ->
			[]

	end,

	GltfBuffers = [ json_to_gltf_buffer( BE ) || BE <- BufferEntries ],

	DefaultSceneId = fixme,
	Scenes  = fixme,
	Nodes = fixme,
	Materials = fixme,
	Meshes = fixme,
	Accessors = fixme,
	BufferViews = fixme,

	%throw( not_implemented_yet ),

	#gltf_content{ default_scene=DefaultSceneId,
				   scenes=Scenes,
				   nodes=Nodes,
				   materials=Materials,
				   meshes=Meshes,
				   accessors=Accessors,
				   buffer_views=BufferViews,
				   buffers=GltfBuffers }.


% @doc Converts the specified JSON term into a glTF buffer.
-spec json_to_gltf_buffer( json_term() ) -> gltf_buffer().
json_to_gltf_buffer( JsonTerm ) ->

	[ ByteLen, BinUri ] = table:get_values( [ <<"byteLength">>, <<"uri">> ],
											JsonTerm ),

	MaybeBinName =
		table:get_value_with_default( <<"name">>, undefined, JsonTerm ),

	#gltf_buffer{ name=MaybeBinName,
				  uri=text_utils:binary_to_string( BinUri ),
				  size=ByteLen }.




% Conversions between lower-level (glTF) and higher-level (Myriad) symbols.
%
% They are done through bijective tables whose first entries are Myriad ones,
% and whose second ones are glTF ones.



% @doc Returns the two-way associations regarding Myriad/glTF element types.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#accessor-data-types
%
-spec get_element_type_associations() ->
					 bijective_table( element_type(), gltf_element_type() ).
get_element_type_associations() ->
	% glTF has no notion of points/vertices (just vectors):
	bijective_table:new( [ { scalar, <<"SCALAR">> },
						   { vector2, <<"VEC2">> },
						   { vector3, <<"VEC3">> },
						   { vector4, <<"VEC4">> },
						   { matrix2, <<"MAT2">> },
						   { matrix3, <<"MAT3">> },
						   { matrix4, <<"MAT4">> } ] ).


% @doc Converts a (Myriad-level) component type into a (lower-level) glTF one.
-spec element_type_to_gltf( element_type() ) -> gltf_element_type().
% Not really bijective as Myriad discriminates between points and vectors:
element_type_to_gltf( _ElemType=point2 ) ->
	element_type_to_gltf( vector2 );

element_type_to_gltf( _ElemType=point3 ) ->
	element_type_to_gltf( vector3 );

element_type_to_gltf( ElemType ) ->
	bijective_table:get_second_for( ElemType, get_element_type_associations() ).


% @doc Converts a (lower-level) glTF component type into a Myriad-level one.
-spec gltf_to_element_type( gltf_element_type() ) -> element_type().
gltf_to_element_type( GltfElemType ) ->
	bijective_table:get_first_for( GltfElemType,
								   get_element_type_associations() ).



% @doc Returns the two-way associations regarding Myriad/glTF component types.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#accessor-data-types
%
-spec get_component_type_associations() ->
			bijective_table( component_type(), gltf_component_type() ).
get_component_type_associations() ->
	bijective_table:new( [ { uint8, 5121 },
						   { sint8, 5120 },
						   { uint16, 5123 },
						   { sint16, 5122 },
						   % No sint32 supported by glTF.
						   { uint32, 5125 },
						   { float32, 5126} ] ).


% @doc Converts a (Myriad-level) component type into a (lower-level) glTF one.
-spec component_type_to_gltf( component_type() ) -> gltf_component_type().
component_type_to_gltf( ComponentType ) ->
	bijective_table:get_second_for( ComponentType,
									get_component_type_associations() ).


% @doc Converts a (lower-level) glTF component type into a Myriad-level one.
-spec gltf_to_component_type( gltf_component_type() ) -> component_type().
gltf_to_component_type( GltfComponentType ) ->
	bijective_table:get_first_for( GltfComponentType,
								   get_component_type_associations() ).




% @doc Returns the two-way associations regarding Myriad/glTF topology types.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#_mesh_primitive_mode
%
-spec get_topology_type_associations() ->
					  bijective_table( topology_type(), gltf_topology_type() ).
get_topology_type_associations() ->
	bijective_table:new( [ { points, 0 },
						   { lines, 1 },
						   { line_loop, 2 },
						   { line_strip, 3 },
						   { triangles, 4 },
						   { triangle_strip, 5 },
						   { triangle_fan, 6 } ] ).


% @doc Converts a (Myriad-level) topology type (if any) into a (lower-level)
% glTF one.
%
-spec maybe_topology_to_gltf( maybe( topology_type() ) ) ->
								 maybe( gltf_topology_type() ).
maybe_topology_to_gltf( _MaybeTopologyType=undefined ) ->
	% Preferred to default glTF value (4, for triangles):
	undefined;

maybe_topology_to_gltf( TopologyType ) ->
	topology_type_to_gltf( TopologyType ).



% @doc Converts a (Myriad-level) topology type into a (lower-level) glTF one.
-spec topology_type_to_gltf( topology_type() ) -> gltf_topology_type().
topology_type_to_gltf( TopologyType ) ->
	bijective_table:get_second_for( TopologyType,
									get_topology_type_associations() ).


% @doc Converts a (lower-level) glTF component type into a Myriad-level one.
-spec gltf_to_topology_type( gltf_topology_type() ) -> topology_type().
gltf_to_topology_type( GltfTopologyTypeType ) ->
	bijective_table:get_first_for( GltfTopologyTypeType,
								   get_topology_type_associations() ).



% @doc Returns the two-way associations regarding Myriad/glTF buffer-view
% targets.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#_buffer_view_target
%
-spec get_buffer_view_target_associations() ->
			bijective_table( buffer_view_target(), gltf_buffer_view_target() ).
get_buffer_view_target_associations() ->
	bijective_table:new( [ { array_buffer, 34962 },
						   { element_array_buffer, 34963 } ] ).


% @doc Converts a (Myriad-level) buffer-view target into a (lower-level) glTF
% one.
%
-spec buffer_view_target_to_gltf( buffer_view_target() ) ->
										   gltf_buffer_view_target().
buffer_view_target_to_gltf( BufferViewTarget ) ->
	bijective_table:get_second_for( BufferViewTarget,
									get_buffer_view_target_associations() ).


% @doc Converts a (lower-level) glTF buffer-view target into a Myriad-level one.
-spec gltf_to_buffer_view_target( gltf_buffer_view_target() ) ->
										   buffer_view_target().
gltf_to_buffer_view_target( GltfBufferViewTarget ) ->
	bijective_table:get_first_for( GltfBufferViewTarget,
								   get_buffer_view_target_associations() ).




% @doc Decodes the specified primitive of the specified mesh, as defined in the
% specified glTF content: returns its vertices, normals, texture coordinates and
% (glTF) indexes (which may account for vertices and/or normals and/or texture
% coordinates).
%
-spec decode_primitive( mesh_index(), primitive_index(), gltf_content(),
						buffer_table() ) ->
		{ [ specialised_vertex() ], [ specialised_normal() ],
		  [ specialised_texture_coordinates() ], [ gltf_index() ],
		  buffer_table() }.
decode_primitive( MeshIndex, PrimitiveIndex, #gltf_content{
												meshes=Meshes,
												accessors=Accessors,
												buffers=Buffers,
												buffer_views=BufferViews },
				  BufferTable ) ->

	trace_utils:debug_fmt( "Decoding primitive ~B of mesh ~B.",
						   [ PrimitiveIndex, MeshIndex ] ),

	_Mesh = #gltf_mesh{ primitives=Primitives } =
					list_utils:get_element_at( Meshes, MeshIndex+1 ),

	Prim = #gltf_primitive{ attributes=Attributes } =
		list_utils:get_element_at( Primitives, PrimitiveIndex+1 ),


	{ Vertices, VertBufferTable } = case Attributes#gltf_attributes.position of

		undefined ->
			BufferTable;

		PositionAccessorIndex ->
			VertP = { Verts, _VertBuffTable } = decode_vertices(
				PositionAccessorIndex, Accessors, Buffers, BufferViews,
				BufferTable ),

			trace_utils:debug_fmt( "The ~B extracted vertices are:~n~p",
								   [ length( Verts ), Verts ] ),

			VertP

	end,


	{ Normals, NormBufferTable } = case Attributes#gltf_attributes.normal of

		undefined ->
			VertBufferTable;

		NormalPositionAccessorIndex ->

			NormP = { Norms, _NormBuffTable } = decode_normals(
				NormalPositionAccessorIndex, Accessors, Buffers, BufferViews,
				VertBufferTable ),

			trace_utils:debug_fmt( "The ~B extracted normals are:~n~p",
								   [ length( Norms ), Norms ] ),

			NormP

	end,


	% No gltf_attributes.tangent managed here.


	{ TexCoords, Tex0BufferTable } =
			case Attributes#gltf_attributes.texcoord_0 of

		undefined ->
			NormBufferTable;

		TexCoord0AccessorIndex ->

			TexP = { TexCs, _Tex0BuffTable } = decode_texture_coordinates(
				TexCoord0AccessorIndex, Accessors, Buffers, BufferViews,
				NormBufferTable ),

			trace_utils:debug_fmt( "The ~B extracted texture coordinates "
				"are:~n~p", [ length( TexCs ), TexCs ] ),

			TexP

	end,


	{ Indexes, IndexesBufferTable } = case Prim#gltf_primitive.indexes of

		undefined ->
			Tex0BufferTable;

		IndexesPositionAccessorIndex ->

			InP = { Inds, _IndexesBuffTable } = decode_indexes(
				IndexesPositionAccessorIndex, Accessors, Buffers, BufferViews,
				Tex0BufferTable ),

			trace_utils:debug_fmt( "The ~B extracted glTF indexes are:~n~p",
								   [ length( Inds ), Inds ] ),

			InP

	end,

	{ Vertices, Normals, TexCoords, Indexes, IndexesBufferTable }.



% @doc Decodes the vertices defined in the specified glTF content.
-spec decode_vertices( accessor_index(), [ gltf_accessor() ], [ gltf_buffer() ],
					   [ gltf_buffer_view() ], buffer_table() ) ->
							{ [ specialised_vertex() ], buffer_table() }.
decode_vertices( AccessorIndex, Accessors, Buffers, BufferViews,
				 BufferTable ) ->

	_PositionAccessor = #gltf_accessor{
			buffer_view=BufferViewIndex,
			element_type=AccessorElemType,
			component_type=AccessorComponentType,
			count=PointCount,
			max=PMax,
			min=PMin } =
		list_utils:get_element_at( Accessors, AccessorIndex+1 ),

	trace_utils:debug_fmt( "To decode vertices, expecting ~B ~ts elements "
		"of component type ~ts, whose minimum is ~ts and maximum is ~ts.",
		[ PointCount, AccessorElemType, AccessorComponentType,
		  point3:to_string( point3:from_vector( PMin ) ),
		  point3:to_string( point3:from_vector( PMax ) ) ] ),

	{ BinViewContent, NewBufferTable } = get_buffer_view_binary(
		BufferViewIndex, BufferViews, Buffers, BufferTable ),

	%trace_utils:debug_fmt( "Binary content of view is:~n~p",
	%                       [ BinViewContent ] ),

	{ extract_points( BinViewContent, PointCount, AccessorElemType,
					  AccessorComponentType ), NewBufferTable }.



% @doc Decodes the normals defined in the specified glTF content.
-spec decode_normals( accessor_index(), [ gltf_accessor() ], [ gltf_buffer() ],
					  [ gltf_buffer_view() ], buffer_table() ) ->
							   { [ specialised_vertex() ], buffer_table() }.
decode_normals( AccessorIndex, Accessors, Buffers, BufferViews,
				BufferTable ) ->

	_NormalAccessor = #gltf_accessor{
			buffer_view=BufferViewIndex,
			element_type=AccessorElemType,
			component_type=AccessorComponentType,
			count=VectorCount,
			max=NMax,
			min=NMin } =
		list_utils:get_element_at( Accessors, AccessorIndex+1 ),

	trace_utils:debug_fmt( "To decode normals, expecting ~B ~ts elements "
		"of component type ~ts, whose minimum is ~w and maximum is ~w.",
		[ VectorCount, AccessorElemType, AccessorComponentType,
		  NMin, NMax ] ),

	{ BinViewContent, NewBufferTable } = get_buffer_view_binary(
		BufferViewIndex, BufferViews, Buffers, BufferTable ),

	%trace_utils:debug_fmt( "Binary content of view is:~n~p",
	%                       [ BinViewContent ] ),

	{ extract_vectors( BinViewContent, VectorCount, AccessorElemType,
					   AccessorComponentType ), NewBufferTable }.



% @doc Decodes the texture coordinates defined in the specified glTF content.
-spec decode_texture_coordinates( accessor_index(), [ gltf_accessor() ],
		[ gltf_buffer() ], [ gltf_buffer_view() ], buffer_table() ) ->
				{ [ specialised_texture_coordinates() ], buffer_table() }.
decode_texture_coordinates( AccessorIndex, Accessors, Buffers, BufferViews,
							BufferTable ) ->

	_TexCoordAccessor = #gltf_accessor{
			buffer_view=BufferViewIndex,
			element_type=AccessorElemType,
			component_type=AccessorComponentType,
			count=CoordCount,
			max=TCMax,
			min=TCMin } =
		list_utils:get_element_at( Accessors, AccessorIndex+1 ),

	trace_utils:debug_fmt( "To decode texture coordinates, expecting ~B ~ts "
		"elements of component type ~ts, whose minimum is ~w "
		"and maximum is ~w.",
		[ CoordCount, AccessorElemType, AccessorComponentType,
		  TCMin, TCMax ] ),

	{ BinViewContent, NewBufferTable } = get_buffer_view_binary(
		BufferViewIndex, BufferViews, Buffers, BufferTable ),

	%trace_utils:debug_fmt( "Binary content of view is:~n~p",
	%                       [ BinViewContent ] ),

	{ extract_vectors( BinViewContent, CoordCount, AccessorElemType,
					   AccessorComponentType ), NewBufferTable }.



% @doc Decodes the (glTF) indexes defined in the specified glTF content.
-spec decode_indexes( accessor_index(), [ gltf_accessor() ], [ gltf_buffer() ],
					  [ gltf_buffer_view() ], buffer_table() ) ->
									{ [ gltf_index() ], buffer_table() }.
decode_indexes( AccessorIndex, Accessors, Buffers, BufferViews,
				BufferTable ) ->

	_PositionAccessor = #gltf_accessor{
			buffer_view=BufferViewIndex,
			element_type=AccessorElemType,
			component_type=AccessorComponentType,
			count=PointCount,
			max=IMax,
			min=IMin } =
		list_utils:get_element_at( Accessors, AccessorIndex+1 ),

	trace_utils:debug_fmt( "To decode indexes, expecting ~B ~ts elements "
		"of component type ~ts, whose minimum is ~ts and maximum is ~ts.",
		[ PointCount, AccessorElemType, AccessorComponentType, IMin, IMax ] ),

	{ BinViewContent, NewBufferTable } = get_buffer_view_binary(
		BufferViewIndex, BufferViews, Buffers, BufferTable ),

	%trace_utils:debug_fmt( "Binary content of view is:~n~p",
	%                       [ BinViewContent ] ),

	% Check:
	AccessorElemType = scalar,

	{ extract_indexes( BinViewContent, PointCount, AccessorComponentType ),
	  NewBufferTable }.



% To be added later:
% add_primitive( gltf_primitive(), gltf_content() ) -> gltf_content().
% add_mesh( gltf_mesh(), gltf_content() ) -> gltf_content().


% @doc Adds a named, empty mesh (that is: a mesh with no primitive) to the
% specified content; returns the index of this mesh and the corresponding
% updated glTF content.
%
% Does not create any node referencing that mesh.
%
-spec add_empty_mesh( mesh_name(), gltf_content() ) ->
											{ mesh_index(), gltf_content() }.
add_empty_mesh( MeshName, Content=#gltf_content{ meshes=Meshes } ) ->
	NewMesh = #gltf_mesh{ name=MeshName },
	NewMeshes = list_utils:append_at_end( NewMesh, Meshes ),
	% As starts at zero:
	NewMeshIndex = length( Meshes ),
	{ NewMeshIndex, Content#gltf_content{ meshes=NewMeshes } }.



% @doc Encodes the specified primitive information as an additional primitive of
% the specified mesh of the specified glTF content.
%
% Directly embeds the resulting buffer; returns the index of the new primitive
% (in that mesh) and an updated glTF content.
%
-spec add_primitive( [ specialised_vertex() ], [ specialised_normal() ],
		[ specialised_texture_coordinates() ], topology_type(), gltf_topology(),
		  material_index(), mesh_index(), gltf_content() ) ->
							{ primitive_index(), gltf_content() }.
add_primitive( Vertices, Normals, TexCoords, TopologyType, IndexedTriangles,
			   MaterialAccessorIndex, MeshIndex, Content ) ->
	add_primitive( _MaybeName=undefined, Vertices, Normals, TexCoords,
		TopologyType, IndexedTriangles, MaterialAccessorIndex, MeshIndex,
		Content ).



% @doc Encodes the specified primitive information as an additional primitive of
% the specified already-existing mesh of the specified glTF content.
%
% Primitives do not have stored names, yet buffer names can use that
% information.
%
% Directly embeds the resulting buffer; returns the index of the new primitive
% (in that mesh) and an updated glTF content.
%
-spec add_primitive( maybe( primitive_name() ), [ specialised_vertex() ],
		[ specialised_normal() ], [ specialised_texture_coordinates() ],
		topology_type(), gltf_topology(), material_index(), mesh_index(),
		gltf_content() ) -> { primitive_index(), gltf_content() }.
add_primitive( MaybePrimName, Vertices, Normals, TexCoords,
			   TopologyType=triangles, IndexedTriangles, MaterialAccessorIndex,
			   MeshIndex, Content=#gltf_content{ meshes=Meshes,
												 buffers=Buffers } ) ->

	% Here, we rely on the following conventions: in the final buffer, first all
	% vertices ("positions") are listed (if any), then all normals (if any),
	% then all texture coordinates (if any), then all indexes (if any).

	cond_utils:if_defined( gltf_exporter_verbose,
		begin
			trace_utils:debug_fmt( "MaybePrimName = ~p", [ MaybePrimName ] ),
			trace_utils:debug_fmt( "Vertices = ~p", [ Vertices ] ),
			trace_utils:debug_fmt( "Normals = ~p", [ Normals ] ),
			trace_utils:debug_fmt( "TexCoords = ~p", [ TexCoords ] ),
			trace_utils:debug_fmt( "IndexedTriangles = ~p",
								   [ IndexedTriangles ] )
		end ),

	% For the upcoming buffer; as these indexes start at 0:
	PrimBufferIndex = length( Buffers ),

	InitialBuffer = <<>>,
	InitialBufferOffset = 0,

	{ MaybePosAccessorIndex, PosBuffer, PosBufferOffset, PosContent } =
		integrate_vertices( Vertices, MaybePrimName, PrimBufferIndex,
							InitialBuffer, InitialBufferOffset, Content ),

	{ MaybeNormAccessorIndex, NormBuffer, NormBufferOffset, NormContent } =
		integrate_normals( Normals, MaybePrimName, PrimBufferIndex, PosBuffer,
						   PosBufferOffset, PosContent ),

	{ MaybeTexAccessorIndex, TexBuffer, TexBufferOffset, TexContent } =
		integrate_texture_coordinates( TexCoords, MaybePrimName,
			PrimBufferIndex, NormBuffer, NormBufferOffset, NormContent ),


	Indexes = triangles_to_indexes( IndexedTriangles ),

	%trace_utils:debug_fmt( "Indexes: ~p", [ Indexes ] ),

	{ MaybeIdxAccessorIndex, IdxBuffer, _IdxBufferOffset, IdxContent } =
		integrate_indexes( Indexes, MaybePrimName, PrimBufferIndex, TexBuffer,
						   TexBufferOffset, TexContent ),


	FinalRawBuffer = IdxBuffer,
	FinalContent = IdxContent,

	MaybeBufferName = forge_maybe_name( "Generated buffer", MaybePrimName ),

	FinalGltfBuffer =
		raw_buffer_to_gltf_buffer_embedded( FinalRawBuffer, MaybeBufferName ),

	NewBuffers = list_utils:append_at_end( FinalGltfBuffer, Buffers ),

	Attributes = #gltf_attributes{ position=MaybePosAccessorIndex,
								   normal=MaybeNormAccessorIndex,
								   texcoord_0=MaybeTexAccessorIndex },

	NewPrimitive = #gltf_primitive{ attributes=Attributes,
									indexes=MaybeIdxAccessorIndex,
									material=MaterialAccessorIndex,
									mode=TopologyType },

	TargetMeshPos = MeshIndex+1,

	Mesh = #gltf_mesh{ primitives=Primitives }
				= list_utils:get_element_at( Meshes, TargetMeshPos ),

	NewPrimitives = list_utils:append_at_end( NewPrimitive, Primitives ),

	PrimIndex = length( Primitives ),

	UpdatedMesh = Mesh#gltf_mesh{ primitives=NewPrimitives },

	NewMeshes = list_utils:set_element_at( UpdatedMesh, Meshes, TargetMeshPos ),

	NewContent = FinalContent#gltf_content{ meshes=NewMeshes,
											buffers=NewBuffers },

	{ PrimIndex, NewContent }.



% @doc Integrates the specified vertices (if any) in the specified buffer and
% content, which are returned.
%
% These vertices are expected to be already transformed to Y-UP conventions (see
% the 'Orientation section' at the top of this file).
%
-spec integrate_vertices( [ specialised_vertex() ], maybe( object_name() ),
				buffer_index(), raw_buffer(), byte_offset(), gltf_content() ) ->
		{ maybe( accessor_index() ), raw_buffer(), byte_offset(),
		  gltf_content() }.
integrate_vertices( _Vertices=[], _MaybeName, _PrimBufferIndex, Buffer,
					BufferOffset, Content ) ->
	% Do not define empty elements, glTF importers may not support that:
	{ _MaybeAccessorIndex=undefined, Buffer, BufferOffset, Content };

integrate_vertices( Vertices, MaybeName, PrimBufferIndex, Buffer,
					BufferOffset, Content=#gltf_content{
												accessors=Accessors,
												buffer_views=BufferViews } ) ->

	% Vertices are integrated in terms of "Positions"; positions are referenced
	% through PosAccessor, referencing PosBufferView, whose elements are
	% vector3() (hence components are float()).

	% As still zero-based:
	PosAccessorIndex = length( Accessors ),
	PosBufferViewIndex = length( BufferViews ),

	% Transforms these vertices to Y-UP conventions (refer to the 'Orientation
	% section' at the top of this file to understand these transformations):
	%
	YupVertices = point3:point3_to_yups( Vertices ),

	{ MinVec, MaxVec } = compute_gltf_extremas( YupVertices ),

	PosElementType = point3,
	PosComponentType = float32,

	PosCount = length( YupVertices ),

	MaybePosName = forge_maybe_name( "Position accessor", MaybeName ),

	PosAccessor = #gltf_accessor{ name=MaybePosName,
								  buffer_view=PosBufferViewIndex,
								  element_type=PosElementType,
								  component_type=PosComponentType,
								  count=PosCount,
								  min=MinVec,
								  max=MaxVec },

	NewAccessors = list_utils:append_at_end( PosAccessor, Accessors ),

	PosSize = get_size( PosElementType, PosComponentType, PosCount ),

	PosBufferView = #gltf_buffer_view{ buffer=PrimBufferIndex,
									   offset=BufferOffset,
									   size=PosSize },

	NewBufferOffset = BufferOffset + PosSize,

	NewBufferViews = list_utils:append_at_end( PosBufferView, BufferViews ),

	NewBuffer = append_to_buffer( PosElementType, PosComponentType, YupVertices,
								  Buffer ),

	NewContent = Content#gltf_content{ accessors=NewAccessors,
									   buffer_views=NewBufferViews },

	{ PosAccessorIndex, NewBuffer, NewBufferOffset, NewContent }.



% @doc Integrates the specified normals (if any) in the specified buffer and
% content, which are returned.
%
-spec integrate_normals( [ specialised_vertex() ], maybe( object_name() ),
				buffer_index(), raw_buffer(), byte_offset(), gltf_content() ) ->
		{ maybe( accessor_index() ), raw_buffer(), byte_offset(),
		  gltf_content() }.
integrate_normals( _Normals=[], _MaybeName, _PrimBufferIndex, Buffer,
					BufferOffset, Content ) ->
	% Do not define empty elements, glTF importers may not support that:
	{ _MaybeAccessorIndex=undefined, Buffer, BufferOffset, Content };

integrate_normals( Normals, MaybeName, PrimBufferIndex, Buffer,
				   BufferOffset, Content=#gltf_content{
												accessors=Accessors,
												buffer_views=BufferViews } ) ->

	% Normals are referenced through NormAccessor, referencing NormBufferView,
	% whose elements are vector3() (hence components are float()).

	% As still zero-based:
	NormAccessorIndex = length( Accessors ),
	NormBufferViewIndex = length( BufferViews ),

	% Transforms these vertices to Y-UP conventions (refer to the 'Orientation
	% section' at the top of this file to understand these transformations):
	%
	YupNormals = vector3:vector3_to_yups( Normals ),

	NormElementType = vector3,
	NormComponentType = float32,

	NormCount = length( YupNormals ),

	MaybeNormName = forge_maybe_name( "Normal accessor", MaybeName ),

	NormAccessor = #gltf_accessor{ name=MaybeNormName,
								   buffer_view=NormBufferViewIndex,
								   element_type=NormElementType,
								   component_type=NormComponentType,
								   count=NormCount },

	NewAccessors = list_utils:append_at_end( NormAccessor, Accessors ),

	NormSize = get_size( NormElementType, NormComponentType, NormCount ),

	NormBufferView = #gltf_buffer_view{ buffer=PrimBufferIndex,
										offset=BufferOffset,
										size=NormSize },

	NewBufferOffset = BufferOffset + NormSize,

	NewBufferViews = list_utils:append_at_end( NormBufferView, BufferViews ),

	NewBuffer = append_to_buffer( NormElementType, NormComponentType,
								  YupNormals, Buffer ),

	NewContent = Content#gltf_content{ accessors=NewAccessors,
									   buffer_views=NewBufferViews },

	{ NormAccessorIndex, NewBuffer, NewBufferOffset, NewContent }.



% @doc Integrates the specified texture coordinates (if any) in the specified
% buffer and content, which are returned.
%
-spec integrate_texture_coordinates( [ texture_coordinate2() ],
			maybe( object_name() ), buffer_index(), raw_buffer(), byte_offset(),
			gltf_content() ) ->
		{ maybe( accessor_index() ), raw_buffer(), byte_offset(),
		  gltf_content() }.
integrate_texture_coordinates( _TexCoords=[], _MaybeName, _PrimBufferIndex,
							   Buffer, BufferOffset, Content ) ->
	% Do not define empty elements, glTF importers may not support that:
	{ _MaybeAccessorIndex=undefined, Buffer, BufferOffset, Content };

integrate_texture_coordinates( TexCoords, MaybeName, PrimBufferIndex, Buffer,
		BufferOffset, Content=#gltf_content{ accessors=Accessors,
											 buffer_views=BufferViews } ) ->

	% Texture coordinates are referenced through TexCoordAccessor, referencing
	% TexCoordBufferView, whose elements are vector2() (hence components are
	% float()).

	% As still zero-based:
	TexCoordAccessorIndex = length( Accessors ),
	TexCoordBufferViewIndex = length( BufferViews ),

	TexCoordElementType = vector2,
	TexCoordComponentType = float32,

	TexCoordCount = length( TexCoords ),

	MaybeTexCoordName =
		forge_maybe_name( "Texture coordinate accessor", MaybeName ),

	TexCoordAccessor = #gltf_accessor{ name=MaybeTexCoordName,
									   buffer_view=TexCoordBufferViewIndex,
									   element_type=TexCoordElementType,
									   component_type=TexCoordComponentType,
									   count=TexCoordCount },

	NewAccessors = list_utils:append_at_end( TexCoordAccessor, Accessors ),

	TexCoordSize =
		get_size( TexCoordElementType, TexCoordComponentType, TexCoordCount ),

	TexCoordBufferView = #gltf_buffer_view{ buffer=PrimBufferIndex,
											offset=BufferOffset,
											size=TexCoordSize },

	NewBufferOffset = BufferOffset + TexCoordSize,

	NewBufferViews = list_utils:append_at_end( TexCoordBufferView,
											   BufferViews ),

	% Translating to vectors:
	TexCoordsAsVecs = [ vector2:from_point( P ) || P <- TexCoords ],

	NewBuffer = append_to_buffer( TexCoordElementType, TexCoordComponentType,
								  TexCoordsAsVecs, Buffer ),

	NewContent = Content#gltf_content{ accessors=NewAccessors,
									   buffer_views=NewBufferViews },

	{ TexCoordAccessorIndex, NewBuffer, NewBufferOffset, NewContent }.



% @doc Integrates the specified indexes (if any) in the specified buffer and
% content, which are returned.
%
-spec integrate_indexes( [ gltf_index() ], maybe( object_name() ),
		buffer_index(), raw_buffer(), byte_offset(), gltf_content() ) ->
				{ maybe( accessor_index() ), raw_buffer(), byte_offset(),
				  gltf_content() }.
integrate_indexes( _Indexes=[], _MaybeName, _PrimBufferIndex, Buffer,
				   BufferOffset, Content ) ->
	% Do not define empty elements, glTF importers may not support that:
	{ _MaybeAccessorIndex=undefined, Buffer, BufferOffset, Content };

integrate_indexes( Indexes, MaybeName, PrimBufferIndex, Buffer,
				   BufferOffset, Content=#gltf_content{
												accessors=Accessors,
												buffer_views=BufferViews } ) ->

	% Indexes are referenced through IdxAccessor, referencing IdxBufferView,
	% whose elements are scalar() (components are uint16).

	IdxAccessorIndex = length( Accessors ),
	IdxBufferViewIndex = length( BufferViews ),

	IdxElementType = scalar,
	IdxComponentType = uint16,

	IdxCount = length( Indexes ),

	MaybeIdxName = forge_maybe_name( "Index accessor", MaybeName ),

	IdxAccessor = #gltf_accessor{ name=MaybeIdxName,
								  buffer_view=IdxBufferViewIndex,
								  element_type=IdxElementType,
								  component_type=IdxComponentType,
								  count=IdxCount },

	NewAccessors = list_utils:append_at_end( IdxAccessor, Accessors ),

	IdxSize = get_size( IdxElementType, IdxComponentType, IdxCount ),

	%trace_utils:debug_fmt( "Size of index buffer: ~B bytes.",
	%                       [ IdxSize ] ),

	IdxBufferView = #gltf_buffer_view{ buffer=PrimBufferIndex,
									   offset=BufferOffset,
									   size=IdxSize },

	NewBufferOffset = BufferOffset + IdxSize,

	NewBufferViews = list_utils:append_at_end( IdxBufferView, BufferViews ),

	NewBuffer = append_to_buffer( IdxElementType, IdxComponentType, Indexes,
								  Buffer ),

	NewContent = Content#gltf_content{ accessors=NewAccessors,
									   buffer_views=NewBufferViews },

	{ IdxAccessorIndex, NewBuffer, NewBufferOffset, NewContent }.




% @doc Returns a pair of vectors whose coordinates reflect the overall minimum
% and maximum values found in the specified list of points.
%
-spec compute_gltf_extremas( [ point3() ] ) -> { point3(), point3() }.
% No wanting to let 'undefined' go through:
compute_gltf_extremas( _Points=[] )->
	throw( no_points );

compute_gltf_extremas( Points )->

	% Cannot rely on term order, as number() is already the smallest.

	Undef3 = { undefined, undefined, undefined },

	compute_gltf_extremas( Points, Undef3, Undef3 ).



% (helper)
compute_gltf_extremas( _Points=[], MinP, MaxP ) ->
	{ tuple_to_list( MinP ), tuple_to_list( MaxP ) };


compute_gltf_extremas( _Points=[ {X,Y,Z} | T ], _MinP={ XMin, YMin, ZMin },
					   _MaxP={ XMax, YMax, ZMax } ) ->

	{ NewXMin, NewXMax } = update_coord( X, XMin, XMax ),
	{ NewYMin, NewYMax } = update_coord( Y, YMin, YMax ),
	{ NewZMin, NewZMax } = update_coord( Z, ZMin, ZMax ),

	NewMinP = { NewXMin, NewYMin, NewZMin },
	NewMaxP = { NewXMax, NewYMax, NewZMax },

	compute_gltf_extremas( T, NewMinP, NewMaxP ).



% (helper)
update_coord( C, Min, Max ) ->

	NewMin = case Min of

		undefined ->
			C;

		Min when C < Min ->
			C;

		_ ->
			Min

	end,

	NewMax = case Max of

		undefined ->
			C;

		Max when C > Max ->
			C;

		_ ->
			Max

	end,

	{ NewMin, NewMax }.



% @doc Returns the size in bytes of the specified array of elements.
-spec get_size( element_type(), component_type(), count() ) -> byte_size().
get_size( ElementType, ComponentType, Count ) ->
	linear:get_element_count( ElementType ) *
		type_utils:get_low_level_type_size( ComponentType ) * Count.



% @doc Returns a binary corresponding to the specified buffer-view.
-spec get_buffer_view_binary( buffer_view_index(), [ gltf_buffer_view() ],
							  [ gltf_buffer() ], buffer_table() ) ->
										{ raw_buffer(), buffer_table() }.
get_buffer_view_binary( BufferViewIndex, BufferViews, Buffers, BufferTable ) ->

	_BufferView = #gltf_buffer_view{ buffer=BufferIndex,
									 offset=MaybeViewOffset,
									 size=ViewSize }
		= list_utils:get_element_at( BufferViews, BufferViewIndex+1 ),

	ViewOffset = case MaybeViewOffset of

		undefined ->
			0;

		Offset ->
			Offset

	end,

	trace_utils:debug_fmt( "Buffer view to access buffer ~B with offset ~B, "
		"of view size ~B bytes.", [ BufferIndex, ViewOffset, ViewSize ] ),

	{ BinBufferContent, NewBufferTable } =
		get_buffer( BufferIndex, Buffers, BufferTable ),

	%trace_utils:debug_fmt( "Buffer content decoded from base-64 is:~n~p",
	%                       [ BinBufferContent ] ),

	% No stride managed:
	BinViewContent =
		binary:part( BinBufferContent, _Pos=ViewOffset, _Len=ViewSize ),

	{ BinViewContent, NewBufferTable }.




% @doc Returns an appropriate object name (if any), based on specified string
% and deriving name (ex: the one of a primitive).
%
-spec forge_maybe_name( ustring(), maybe( object_name() ) ) ->
													maybe( object_name() ).
forge_maybe_name( _BaseStr, _MaybeName=undefined ) ->
	undefined;

forge_maybe_name( BaseStr, Name ) ->
	text_utils:format( "~ts for ~ts", [ BaseStr, Name ] ).



% @doc Returns the binary context of specified buffer, either cached, or decoded
% and cached once for all.
%
-spec get_buffer( buffer_index(), [ gltf_buffer() ], buffer_table() ) ->
										{ raw_buffer(), buffer_table() }.
get_buffer( BufferIndex, Buffers, BufferTable ) ->
	case table:lookup_entry( _K=BufferIndex, BufferTable ) of

		key_not_found ->
			Buffer = #gltf_buffer{ uri=Uri, size=BufferSize }
				= list_utils:get_element_at( Buffers, BufferIndex+1 ),

			trace_utils:debug_fmt( "Caching, as entry ~B, buffer "
				"of size ~B bytes, whose URI is:~n~ts.",
				[ BufferIndex, BufferSize, Uri ] ),

			BinBufferContent = gltf_buffer_embedded_to_raw_buffer( Buffer ),

			% Check:
			BufferSize = size( BinBufferContent ),

			NewBufferTable = table:add_entry( BufferIndex, BinBufferContent,
											  BufferTable ),

			{ BinBufferContent, NewBufferTable };

		{ value, BinBufferContent } ->
			trace_utils:debug_fmt( "Returning buffer cached as entry ~B.",
								   [ BufferIndex ] ),
			{ BinBufferContent, BufferTable }

	end.



% @doc Generates a binary buffer corresponding to the specified elements.
-spec generate_buffer( element_type(), component_type(),
					   [ specialised_type() ] ) -> raw_buffer().
generate_buffer( ElementType, ComponentType, Elements ) ->
	append_to_buffer( ElementType, ComponentType, Elements, _AccBin= <<>> ).



% @doc Appends the binary version of the specified elements to the specified
% binary buffer.
%
-spec append_to_buffer( element_type(), component_type(),
						[ specialised_type() ], raw_buffer() ) -> raw_buffer().
append_to_buffer( _ElementType=scalar, _ComponentType=uint16, Elements,
				  Bin ) ->

	%trace_utils:debug_fmt( "Appending following ~B uint16 (probably indexes) "
	%   "to buffer:~n ~p", [ length( Elements ), Elements ] ),

	append_all_uint16_little( Elements, Bin );


append_to_buffer( _ElementType=vector3, _ComponentType=float32, Elements,
				  Bin ) ->

	% Commented out, as all vectors expected to be already Y-UP'ed:
	% YUPVecs = vector3:vector3_to_yups( Elements ),

	ComponentFloats = list_utils:flatten_once( Elements ),
	%trace_utils:debug_fmt( "Appending following floats for YUP vector3:"
	%                       "~n~p", [ ComponentFloats ] ),
	append_all_float32_little( ComponentFloats, Bin );

append_to_buffer( ElementType, _ComponentType=float32, Elements, Bin )
		when ElementType =:= vector2
	  orelse ElementType =:= vector4 ->
	ComponentFloats = list_utils:flatten_once( Elements ),
	%trace_utils:debug_fmt( "Appending following floats for vectors:~n~p",
	%                       [ ComponentFloats ] ),
	append_all_float32_little( ComponentFloats, Bin );

append_to_buffer( _ElementType=point4, _ComponentType=float32, Elements,
				  Bin ) ->
	ComponentFloats = get_coordinates_from_point4( Elements ),
	append_all_float32_little( ComponentFloats, Bin );


append_to_buffer( _ElementType=point3, _ComponentType=float32, Elements,
				  Bin ) ->
	% Commented out, as all points expected to be already Y-UP'ed:
	%YUPPoints = point3:point3_to_yups( Elements ),

	ComponentFloats = get_coordinates_from_point3( Elements ),
	%trace_utils:debug_fmt( "Appending following floats for YUP point3:"
	%                       "~n~p", [ ComponentFloats ] ),
	append_all_float32_little( ComponentFloats, Bin );

append_to_buffer( _ElementType=point2, _ComponentType=float32, Elements,
				  Bin ) ->
	ComponentFloats = get_coordinates_from_point2( Elements ),
	append_all_float32_little( ComponentFloats, Bin ).




% @doc Returns a list of all, in-order coordinates of the specified points.
-spec get_coordinates_from_point4( [ point4() ] ) -> [ coordinate() ].
get_coordinates_from_point4( Elements ) ->
	get_coordinates_from_point4( Elements, _Acc=[]  ).


% (helper)
get_coordinates_from_point4( _Elements=[], Acc ) ->
	lists:reverse( Acc );

get_coordinates_from_point4( _Elements=[ {X,Y,Z,W} | T ], Acc ) ->
	% Will be reversed:
	get_coordinates_from_point4( T, [ W, Z, Y, X | Acc ] ).



% @doc Returns a list of all, in-order coordinates of the specified points.
-spec get_coordinates_from_point3( [ point3() ] ) -> [ coordinate() ].
get_coordinates_from_point3( Elements ) ->
	get_coordinates_from_point3( Elements, _Acc=[] ).


% (helper)
get_coordinates_from_point3( _Elements=[], Acc ) ->
	lists:reverse( Acc );

get_coordinates_from_point3( _Elements=[ {X,Y,Z} | T ], Acc ) ->
	% Will be reversed:
	get_coordinates_from_point3( T, [ Z, Y, X | Acc ] ).



% @doc Returns a list of all, in-order coordinates of the specified points.
-spec get_coordinates_from_point2( [ point2() ] ) -> [ coordinate() ].
get_coordinates_from_point2( Elements ) ->
	get_coordinates_from_point2( Elements, _Acc=[] ).


% (helper)
get_coordinates_from_point2( _Elements=[], Acc ) ->
	lists:reverse( Acc );

get_coordinates_from_point2( _Elements=[ {X,Y} | T ], Acc ) ->
	% Will be reversed:
	get_coordinates_from_point2( T, [ Y, X | Acc ] ).



% @doc Appends to the specified binary all specified integers as 16 bit unsigned
% ones, and returns the resulting binary.
%
-spec append_all_uint16_little( [ integer() ], raw_buffer() ) -> raw_buffer().
append_all_uint16_little( _Elements=[], Bin ) ->
	Bin;

append_all_uint16_little( _Elements=[ UI | T ], Bin ) ->
	NewBin = <<Bin/binary,UI:16/little-unsigned-integer>>,
	%trace_utils:debug_fmt( "New binary: ~p", [ NewBin ] ),
	append_all_uint16_little( T, NewBin ).



% @doc Appends to the specified binary all specified floats, as single-precision
% (32 bit) ones, and returns the resulting binary.
%
-spec append_all_float32_little( [ float() ], raw_buffer() ) -> raw_buffer().
append_all_float32_little( _Elements=[], Bin ) ->
	Bin;

append_all_float32_little( _Elements=[ F | T ], Bin ) ->
	%trace_utils:debug_fmt( "Appending float ~w.", [ F ] ),
	NewBin = <<Bin/binary,F:32/float-little>>,
	append_all_float32_little( T, NewBin ).



% @doc Extracts specified points from specified binary (typically obtained from
% a buffer view).
%
-spec extract_points( raw_buffer(), count(), element_type(),
					  component_type() ) -> [ specialised_point() ].
extract_points( Bin, ElementCount, ElemType, ComponentType ) ->
	extract_elements( Bin, ElementCount, ElemType, ComponentType,
					  _FinalType=point ).


% @doc Extracts specified vectors from specified binary (typically obtained from
% a buffer view).
%
-spec extract_vectors( raw_buffer(), count(), element_type(),
					   component_type() ) -> [ specialised_vector() ].
extract_vectors( Bin, ElementCount, ElemType, ComponentType ) ->
	extract_elements( Bin, ElementCount, ElemType, ComponentType,
					  _FinalType=vector ).


% @doc Extracts specified indexes from specified binary (typically obtained from
% a buffer view).
%
-spec extract_indexes( raw_buffer(), count(), component_type() ) ->
													[ gltf_index() ].
extract_indexes( Bin, ElementCount, ComponentType ) ->
	extract_elements( Bin, ElementCount, _ElemType=scalar, ComponentType,
					  _FinalType=undefined ).



% @doc Extracts specified elements from specified binary (typically obtained
% from a buffer view).
%
-spec extract_elements( raw_buffer(), count(), element_type(), component_type(),
						final_type() ) -> [ buffer_elements() ].
extract_elements( Bin, ElementCount, _ElemType=scalar,
				  _ComponentType=uint16, _FinalType ) ->

	% No final type applies here.

	ComponentInts = extract_all_uint16_little( Bin ),

	% Check:
	ElementCount = length( ComponentInts ),

	ComponentInts;


extract_elements( Bin, ElementCount, _ElemType=scalar,
				  _ComponentType=float32, _FinalType ) ->

	ComponentInts = extract_all_float32_little( Bin ),

	% Check:
	ElementCount = length( ComponentInts ),

	ComponentInts;


extract_elements( Bin, ElementCount, _ElemType=vector2,
				  _ComponentType=float32, FinalType ) ->

	ComponentFloats = extract_all_float32_little( Bin ),

	Elems = gather_as( FinalType, _Dim=2, ComponentFloats ),

	% Check:
	ElementCount = length( Elems ),

	Elems;


extract_elements( Bin, ElementCount, _ElemType=vector3,
				  _ComponentType=float32, FinalType ) ->

	ComponentFloats = extract_all_float32_little( Bin ),

	Elems = gather_as( FinalType, _Dim=3, ComponentFloats ),

	% Check:
	ElementCount = length( Elems ),

	Elems.



% @doc Extracts from the specified binary all 16 bit unsigned integers, supposed
% encoded in little-endian.
%
-spec extract_all_uint16_little( raw_buffer() ) -> [ integer() ].
extract_all_uint16_little( Bin ) ->
	extract_all_uint16_little( Bin, _Acc=[] ).


% (helper)
extract_all_uint16_little( _Bin= <<>>, Acc ) ->
	lists:reverse( Acc );

extract_all_uint16_little( _Bin= <<UI:16/little-unsigned-integer,Rest/binary>>,
						   Acc ) ->
	extract_all_uint16_little( Rest, [ UI | Acc ] ).



% @doc Extracts from the specified binary all single-precision (32 bit) floats,
% supposed encoded in little-endian.
%
-spec extract_all_float32_little( raw_buffer() ) -> [ float() ].
extract_all_float32_little( Bin ) ->
	extract_all_float32_little( Bin, _Acc=[] ).


% (helper)
extract_all_float32_little( _Bin= <<>>, Acc ) ->
	lists:reverse( Acc );

extract_all_float32_little( _Bin= <<F:32/float-little,Rest/binary>>, Acc ) ->
	extract_all_float32_little( Rest, [ F | Acc ] ).



% @doc Gathers specified components as a list of the elements of specified
% dimension of the specified final type.
%
-spec gather_as( final_type(), dimension(), [ number() ] ) -> [ term() ].
gather_as( FinalType, Dim, Components ) ->
	gather_as( FinalType, Dim, Components, _Acc=[] ).


% (helper)
gather_as( _FinalType, _Dim, _Components=[], Acc ) ->
	lists:reverse( Acc );

gather_as( FinalType, Dim, Components, Acc ) ->

	{ Coords, Rest } = lists:split( _PrefixLen=Dim, Components ),

	Element = case FinalType of

		point ->
			list_to_tuple( Coords );

		vector ->
			Coords

	end,

	gather_as( FinalType, Dim, Rest, [ Element | Acc ] ).



% @doc Returns a flat list of vertex indexes corresponding to the specified list
% of (indexed) triangles.
%
-spec triangles_to_indexes( [ indexed_triangle() ] ) -> [ gltf_vertex_index() ].
triangles_to_indexes( Triangles ) ->
	triangles_to_indexes( Triangles, _Acc=[] ).


% (helper)
triangles_to_indexes( _Triangles=[], Acc ) ->
	lists:reverse( Acc );


triangles_to_indexes( _Triangles=[ { I1, I2, I3 } | T ], Acc ) ->
	% As indexes will be reversed as a whole:
	triangles_to_indexes( T, [ I3, I2, I1 | Acc ] ).



% @doc Returns the list of (indexed) triangles corresponding to the specified
% flat list of vertex indexes.
%
-spec indexes_to_triangles( [ gltf_vertex_index() ] ) -> [ indexed_triangle() ].
indexes_to_triangles( Indexes ) ->
	indexes_to_triangles( Indexes, _Acc=[] ).


% (helper)
indexes_to_triangles( _Indexes=[], Acc ) ->
	lists:reverse( Acc );

indexes_to_triangles( _Indexes=[ I1, I2, I3 | T ], Acc ) ->
	Triangle = { I1, I2, I3 },
	indexes_to_triangles( T, [ Triangle | Acc ] ).



% @doc Returns (zero-based) glTF indexed triangles from the specified ones.
-spec indexed_triangles_to_gltf( [ indexed_triangle() ] ) ->
										[ gltf_indexed_triangle() ].
indexed_triangles_to_gltf( IndTriangles ) ->
	[ indexed_triangle_to_gltf( IT ) || IT <- IndTriangles ].



% @doc Returns a (zero-based) glTF indexed triangle from the specified one.
-spec indexed_triangle_to_gltf( indexed_triangle() ) -> gltf_indexed_triangle().
indexed_triangle_to_gltf( _IndTriangle={ I1, I2, I3 } ) ->
	{ I1-1, I2-1, I3-1 }.
