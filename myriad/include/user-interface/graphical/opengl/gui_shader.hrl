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
% Creation date: Sunday, April 14, 2024.


% Header to export shader-related GLSL elements, notably defines, which could be
% useful for deriving code.


% Default usage profile for VBOs:
-define( default_vbo_usage_hint, { draw, static } ).

% Default usage profile for EBOs:
-define( default_ebo_usage_hint, { draw, static } ).



% We define here once for all the conventional MyriadGUI numbering regarding
% attribute locations (see vertex_attribute_index(), abbreviated here as VAI)
% used in the base shaders:


% The index of the vertex attribute in the vertex stream that will be passed to
% the MyriadGUI (vertex) shader for the vertices (as vec3/vertex3()).
%
% Attribute 0 was chosen here, yet there is no particular reason for this index
% value, it just must match the one of the layout (cf. 'location = 0') in the
% shader (see gui_opengl_base_shader.vertex.glsl).
%
-define( myriad_gui_input_vertex_vai, 0 ).


% The index of the vertex attribute in the vertex stream that will be passed to
% the MyriadGUI shaders for the normal vectors (as vec3/vector3()), if any; see
% myriad_gui_input_vertex_vai for more details.
%
-define( myriad_gui_input_normal_vai, 1 ).


% The index of the vertex attribute in the vertex stream that will be passed to
% the MyriadGUI shaders for the texture coordinates (as
% vec3/render_rgb_color()); see myriad_gui_input_vertex_vai for more details.
%
-define( myriad_gui_input_color_vai, 2 ).

% The index of the vertex attribute in the vertex stream that will be passed to
% the MyriadGUI shaders for the texture coordinates (as vec2/uv_point()), if
% any; see myriad_gui_input_vertex_vai for more details.
%
-define( myriad_gui_input_texcoord_vai, 3 ).


% Well-known MyriadGUI uniform variables:

% To specify the currently MyriadGUI VBO layout being used:
-define( myriad_gui_vbo_layout_unif_name, "myriad_gui_vbo_layout" ).

% To specify the color to be used by VBO layouts not specifying any color:
-define( myriad_gui_global_color_unif_name, "myriad_gui_global_color" ).

% To specify any texture sampler:
-define( myriad_gui_texture_sampler_unif_name, "myriad_gui_texture_sampler" ).

% The texture unit used by the sampler of the MyriadGUI base fragment shader:
% (thus ?GL_TEXTURE1 instead of the ?GL_TEXTURE0 default one)
%
-define( myriad_gui_base_texture_unit, 1 ).
