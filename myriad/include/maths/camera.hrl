% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Saturday, September 28, 2024.



% Describes a fly-style camera that can freely move around in a 3D scene.
-record( camera, {


	% The origin of the viewpoint corresponding to this camera, in world space:
	%
	% (generally designated as P={Px,Py,Pz})
	%
	position :: point3:point3(),


	% The target position at which this camera is pointing at, in world space:
	%
	% (generally designated as T={Tx,Ty,Tz}; stored in order to be able to move
	% the camera whereas pointing at the same target)
	%
	target :: point3:point3(),


	% The actual direction at which this camera points, in world space, from its
	% position to its target, as a unit vector computed by look_at/3 (thus a
	% value to be read, not set).
	%
	% (this direction will thus correspond to the -Z axis of the local
	% coordinate system of this camera)
	%
	aim :: vector3:unit_vector3(),


	% The direction, in world space, of the upper part of this camera, as a unit
	% vector computed by look_at/3 (thus a value to be read, not set).
	%
	% (this direction will thus correspond to the +Y axis of the local
	% coordinate system of this camera)
	%
	up :: vector3:unit_vector3(),


	% The direction, in world space, of the leftmost part of this camera, as a
	% unit vector computed by look_at/3 (thus a value to be read, not set).
	%
	% (this direction will thus correspond to the +X axis of the local
	% coordinate system of this camera)
	%
	left :: vector3:unit_vector3(),


	% The view transformation, based on the current camera settings, between its
	% coordinate system and another one - generally the world one.
	%
	% More precisely, its reference matrix4 is Pcw, the transition one from
	% camera to world, and Pwc, its inverse - generally the most useful one - is
	% the transition matrix4 from world to camera space.
	%
	% As a consequence, any change in the previous camera fields should result
	% in invalidating this one (i.e. setting it to 'undefined'), so that it gets
	% recomputed whenever needed (refer to look_at/3 for that).
	%
	view_transf4 :: camera:view_transform4() } ).
