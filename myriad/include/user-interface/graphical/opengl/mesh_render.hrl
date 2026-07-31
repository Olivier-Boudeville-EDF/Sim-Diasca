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
% Creation date: Saturday, November 13, 2021.


% Describes a mesh rendering.
%
% Refer to mesh.hrl for the definition of the actual mesh.



% Rendering (OpenGL-related) state of a mesh.
-record( rendering_state, {

    % The identifier of the corresponding GLSL program (typically needed to
    % locate uniform variables):
    %
    program_id :: gui_shader:program_id(),


    % The overall VAO used for that mesh:
    vao_id :: gui_shader:vao_id(),


    % The VBO holding the vertex-related data of that mesh:
    vbo_id :: gui_shader:vbo_id(),

    % The layout respected by the VBO and the corresponding shaders:
    vbo_layout :: gui_shader:vbo_layout(),

    % The number of compounds of vertex attributes (VBO entries) for this mesh:
    compound_count :: gui_shader:compound_count(),


    % The EBO holding the face indices of that mesh:
    ebo_id :: gui_shader:ebo_id() } ).
