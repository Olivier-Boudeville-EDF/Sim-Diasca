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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Friday, October 8, 2021.


% The glTF version supported:
-define( gltf_version_string, "2.0" ).


% Gathers all information regarding a glTF 2.0 content.
-record( gltf_content, {

	% No content name defined, as no place in format was such information.

	% The index of the default scene (if any) of this content:
	default_scene = undefined :: maybe( gltf_support:scene_index() ),

	% The in-order definition of all known scenes:
	scenes = [] :: [ gltf_support:gltf_scene() ],

	% The in-order definition of all known (scene) nodes:
	nodes = [] :: [ gltf_support:gltf_node() ],

	% The in-order definition of all known materials:
	materials = [] :: [ gltf_support:gltf_material() ],

	% The in-order definition of all known types of cameras:
	camera_types = [] :: [ gltf_support:gltf_camera_type() ],

	% The in-order definition of all known meshes:
	meshes = [] :: [ gltf_support:gltf_mesh() ],

	% The in-order definition of all known (buffer) accessors:
	accessors = [] :: [ gltf_support:gltf_accessor() ],

	% The in-order definition of all known buffers:
	buffers = [] :: [ gltf_support:gltf_buffer() ],

	% The in-order definition of all known buffer-views:
	buffer_views = [] :: [ gltf_support:gltf_buffer_view() ] } ).



% A scene defined in a glTF content.
-record( gltf_scene, {

	% The name (if any) of this scene:
	name :: maybe( gltf_support:object_name() ),

	% The indexes of the nodes of this scene:
	nodes = [] :: [ gltf_support:node_index() ] } ).



% A node defined in a glTF content.
-record( gltf_node, {

	% The name (if any) of this node:
	name :: maybe( gltf_support:object_name() ),

	% The index of the mesh (if any) associated to this node:
	mesh :: maybe( gltf_support:mesh_index() ),

	% The quaternion (if any) defining the rotation associated to this node:
	rotation :: maybe( quaternion:quaternion() ),

	% The translation (if any) associated to this node:
	translation :: maybe( vector3:vector3() ),

	% The type of camera (if any) attached to this node:
	camera :: maybe( gltf_support:camera_type_index() ) } ).



% A light defined in a glTF content.
%
% Note that the core glTF format does not define lights (there are just
% represented as nodes); so no gltf_light record.



% A mesh defined in a glTF content.
%
% Meshes are included in nodes, and are themselves defined as arrays of
% primitives. Primitives correspond to the data required for GPU draw calls.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#meshes-overview
%
-record( gltf_mesh, {

	% The name (if any) of this mesh:
	name :: maybe( gltf_support:object_name() ),

	primitives = [] :: [ gltf_support:gltf_primitive() ] } ).



% A primitive defined in a mesh, corresponding to the data required for GPU draw
% calls.
%
% Primitives specify one or more attributes, corresponding to the vertex
% attributes used in the draw calls.
%
% Refer to
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-mesh-primitive
%
-record( gltf_primitive, {

	% No name possibly defined.

	attributes :: gltf_support:gltf_attributes(),

	indexes :: maybe( gltf_support:accessor_index() ),

	material :: maybe( gltf_support:accessor_index() ),

	% The topology type of primitives to render:
	mode :: maybe( linear_2D:topology_type() ) } ).



% Defines the attributes of a primitive, corresponding to the vertex attributes
% used in the draw calls.
%
-record( gltf_attributes, {

	position   :: gltf_support:accessor_index(),
	normal     :: maybe( gltf_support:accessor_index() ),
	tangent    :: maybe( gltf_support:accessor_index() ),
	texcoord_0 :: maybe( gltf_support:accessor_index() )

	% Also: TEXCOORD_n, COLOR_n, JOINTS_n, WEIGHTS_n.

						  } ).



% A material defined in a glTF content.
-record( gltf_material, {

	% The name (if any) of this material:
	name :: maybe( gltf_support:object_name() ),

	% Tells whether this mesh is double-sided:
	double_sided :: maybe( boolean() ),

	% The Physically-Based Rendering (PBR) metallic roughness of this material:
	pbr_metallic_roughness ::
					maybe( gltf_support:gltf_pbr_metallic_roughness() ) } ).



% Describes the metallic roughtness of a material, based on the Physically-Based
% Rendering (PBR) methodology.
%
-record( gltf_pbr_metallic_roughness, {

	base_color_factor :: gui_color:render_color(),

	metallic_factor :: math_utils:factor(),

	roughness_factor :: math_utils:factor() } ).



% No gltf_light exist (they are just nodes).



% A type of orthographic camera, from which, based on a node, actual camera
% instances can be derived.
%
-record( gltf_orthographic_camera, {

	% The name (if any) of this camera:
	name :: maybe( gltf_support:object_name() ),

	x_magnification :: math_utils:positive_factor(),

	y_magnification :: math_utils:positive_factor(),

	z_near_distance :: linear:distance(),

	% Must be greater than z_near_distance:
	z_far_distance :: linear:distance() } ).




% A type of perspective camera, from which, based on a node, actual camera
% instances can be derived.
%
-record( gltf_perspective_camera, {

	% The name (if any) of this camera:
	name :: maybe( gltf_support:object_name() ),

	% The aspect ratio of the field of view:
	aspect_ratio :: maybe( math_utils:ratio() ),

	% The floating-point vertical field of view:
	y_field_of_view  :: unit_utils:radians(),

	% Must be strictly positive:
	z_near_distance :: linear:distance(),

	% Must be greater than z_near_distance; as both are mapped to a cube whose
	% edge length is 1.0, their ratio matters the most):
	%
	z_far_distance :: maybe( linear:distance() ) } ).





% A typed view into a buffer view that contains raw binary data.
%
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-accessor
%
-record( gltf_accessor, {

	% The name (if any) of this accessor:
	name :: maybe( gltf_support:object_name() ),

	buffer_view :: maybe( gltf_support:buffer_view_index() ),

	% Specifies if the accessor’s elements are scalars, vectors, or matrices:
	% (ex: 'vector4')
	%
	element_type :: gltf_support:element_type(),

	% The datatype of a component of an accessor (ex: 'uint8').
	component_type :: gltf_support:component_type(),

	% The number of elements referenced by this accessor:
	count :: basic_utils:count(),

	% The maximum value (if any) of each component in this accessor:
	max :: maybe( [ gltf_support:component_value() ] ),

	% The minimum value (if any) of each component in this accessor:
	min :: maybe( [ gltf_support:component_value() ] ) } ).



% A buffer of raw data, stored as a binary blob. The buffer may contain any
% combination of binary geometry, animation, skins, and images.
%
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-buffer
%
-record( gltf_buffer, {

	% The name (if any) of this buffer:
	name :: maybe( gltf_support:object_name() ),

	% The URI designating the data to be fetched for the content of this buffer:
	uri :: web_utils:uri(),

	% The total size of this buffer:
	size :: system_utils:byte_size() } ).




% A view into a buffer generally representing a subset of the buffer.
%
% https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#reference-bufferview
%
-record( gltf_buffer_view, {

	% The name (if any) of this buffer-view:
	name :: maybe( gltf_support:object_name() ),

	% The index of the target buffer:
	buffer :: gltf_support:buffer_index(),

	% The byte offset of the beginning of this view compared to the beginning of
	% its buffer:
	%
	offset :: maybe( system_utils:byte_offset() ),

	% The size of this view into its buffer:
	size :: system_utils:byte_size() } ).
