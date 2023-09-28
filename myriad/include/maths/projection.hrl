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
% Creation date: Sunday, May 21, 2023.


% The parameters defining an orthographic projection.
-record( orthographic_settings, {

	% The coordinate of the left vertical clipping plane:
	left :: linear:coordinate(),

	% The coordinate of the right vertical clipping plane:
	right :: linear:coordinate(),

	% The coordinate of the bottom horizontal clipping plane:
	bottom :: linear:coordinate(),

	% The coordinate of the top horizontal clipping plane:
	top :: linear:coordinate(),

	% The (signed) distance to the nearer depth clipping plane; negative if the
	% plane is to be behind the viewer.
	%
	z_near :: linear:signed_distance(),

	% The (signed) distance to the farther depth clipping plane; negative if the
	% plane is to be behind the viewer.
	%
	z_far :: linear:signed_distance() } ).



% The parameters defining a perspective projection.
-record( perspective_settings, {

	% The field of view angle in the Y (vertical) direction:
	fov_y_angle :: unit_utils:radians(),

	% Determines the field of view in the X (horizontal) direction:
	% AspectRatio = Width/Height.
	%
	aspect_ratio :: math_utils:ratio(),

	% The distance from the viewer to the near clipping plane (always strictly
	% positive).
	%
	z_near :: linear:distance(),

	% The distance from the viewer to the far clipping plane (always positive).
	z_far :: linear:distance() } ).
