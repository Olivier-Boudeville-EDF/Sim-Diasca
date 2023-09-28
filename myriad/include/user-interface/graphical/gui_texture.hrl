% Copyright (C) 2023-2023 Olivier Boudeville
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
% Creation date: Monday, March 13, 2023.


% For the base alpha coordinates (clearer; OpenGL conventions):

-define( alpha_fully_opaque,      1.0 ).
-define( alpha_fully_transparent, 0.0 ).


% Corresponds to an OpenGL texture.
-record( texture, {

	% The identifier (OpenGL "name") of that texture.
	id :: gui_texture:texture_id(),


	% The pixel width of this texture:
	width :: gui:length(),

	% The pixel height of this texture:
	height :: gui:length(),


	% The minimum abscissa of this texture relatively to its color buffer.
	min_x :: gui_opengl:length_factor(),

	% The minimum ordinate of this texture relatively to its color buffer.
	min_y :: gui_opengl:length_factor(),


	% The maximum abscissa of this texture relatively to its color buffer.
	max_x :: gui_opengl:length_factor(),

	% The maximum ordinate of this texture relatively to its color buffer.
	max_y :: gui_opengl:length_factor() } ).
