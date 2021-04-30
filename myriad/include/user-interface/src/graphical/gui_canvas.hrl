% Copyright (C) 2003-2021 Olivier Boudeville
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
% Creation date: Tuesday, January 29, 2013


% Header to export (internally to MyriadGUI) canvas-related defines.
%
% See gui_canvas.erl for the corresponding implementation.



% wxCanvas does not exist, we emulate it.
%
% Canvas are back-buffered: drawing operations on them will not be visible until
% their blit/1 function is called.
%
-record( canvas_state, {

	% Receives repaint, resize, etc. events:
	panel :: gui:panel(),

	% Displayed area:
	bitmap :: gui:bitmap(),

	% Actual place for rendering:
	back_buffer :: gui:back_buffer(),

	% As apparently we cannot retrieve the size of the underlying bitmap and
	% back buffer (typically useful when the panel may have been resized):
	%
	size :: gui:size()

}).

% The actual canvas type we are to use:
-type canvas_state() :: #canvas_state{}.


% An OpenGL-based canvas:
-type gl_canvas():: wxGLCanvas:wxGLCanvas().
