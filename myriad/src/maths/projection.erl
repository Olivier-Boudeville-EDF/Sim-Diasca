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


% @doc Gathering of various facilities for <b>projection management</b>.
-module(projection).


% For the projection records:
-include("projection.hrl").


-type orthographic_settings() :: #orthographic_settings{}.
-type perspective_settings()  :: #perspective_settings{}.

-type projection_settings() :: orthographic_settings()
							 | perspective_settings().


-export_type([ orthographic_settings/0,
			   perspective_settings/0,
			   projection_settings/0 ]).



-export([ orthographic/1, orthographic/6, perspective/1, perspective/4,
		  projection/1,
		  frustum/6,
		  settings_to_string/1 ]).



% For records like matrix4:
-include("matrix4.hrl").


% Shorthands:

-type ustring() :: text_utils:ustring().

-type ratio() :: math_utils:ratio().

-type radians() :: unit_utils:radians().

-type coordinate() :: linear:coordinate().
-type distance() :: linear:distance().
-type signed_distance() :: linear:signed_distance().

-type compact_matrix4() :: matrix4:compact_matrix4().
-type matrix4() :: matrix4:matrix4().


% Implementation notes:

% Note that the eye coordinates are defined in the right-handed coordinate
% system, but NDC uses the left-handed coordinate system. That is, the camera at
% the origin is looking along -Z axis in eye space, but it is looking along +Z
% axis in NDC.

% The default projection matrix is the identity matrix, which is the same as
% orthographic( _Left=-1.0, _Right=1.0, _Bottom=-1.0, _Top=1.0,
%               _ZNear=-1.0, _ZFar=1.0). % Not _ZNear=1.0, _ZFar=-1.0).



% @doc Returns a matrix for orthographic projection corresponding to the
% specified settings.
%
-spec orthographic( orthographic_settings() ) -> compact_matrix4().
orthographic( #orthographic_settings{
				left=Left, right=Right, bottom=Bottom, top=Top,
				z_near=ZNear, z_far=ZFar } ) ->
	orthographic( Left, Right, Bottom, Top, ZNear, ZFar ).


% @doc Returns a matrix for orthographic projection corresponding to the
% specified settings.
%
% Parameters are:
%  - Left and Right are the coordinates for the left and right vertical clipping
% planes
%  - Bottom and Top are the coordinates for the bottom and top horizontal
% clipping planes
%  - ZNear and ZFar are the signed distances to the nearer and farther depth
%  clipping planes; these values are negative if the plane is to be behind the
%  viewer
%
% Note that the context is a right-handed referential with a clip space in
% [-1.0, 1.0].
%
-spec orthographic( coordinate(), coordinate(), coordinate(), coordinate(),
					signed_distance(), signed_distance() ) -> compact_matrix4().
orthographic( Left, Right, Bottom, Top, ZNear, ZFar ) ->

	% References:
	% https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glOrtho.xml
	% and glm:orthoRH_NO.

	Zero = 0.0,

	VFactorInv =      1.0 / (Right - Left),
	HFactorInv =      1.0 / (Top - Bottom),
	MinusZFactorInv = 1.0 / (ZNear - ZFar),

	M11 = 2 * VFactorInv,
	M22 = 2 * HFactorInv,
	M33 = 2 * MinusZFactorInv,

	Tx = - (Right + Left) * VFactorInv,
	Ty = - (Top + Bottom) * HFactorInv,
	Tz =   (ZNear + ZFar) * MinusZFactorInv,

	#compact_matrix4{ m11=M11,  m12=Zero, m13=Zero, tx=Tx,
					  m21=Zero, m22=M22,  m23=Zero, ty=Ty,
					  m31=Zero, m32=Zero, m33=M33,  tz=Tz }.



% @doc Returns a matrix for perspective projection corresponding to the
% specified settings.
%
-spec perspective( perspective_settings() ) -> compact_matrix4().
perspective( #perspective_settings{
				fov_y_angle=FoVYAngle, aspect_ratio=AspectRatio,
				z_near=ZNear, z_far=ZFar } ) ->
	perspective( FoVYAngle, AspectRatio, ZNear, ZFar ).


% @doc Returns a matrix for perspective projection corresponding to the
% specified settings.
%
% Parameters are:

%  - FoVYAngle is the field of view angle, in radians, in the Y (vertical)
%  direction (the angle from the top of the screen to the bottom); often set to
%  45 degrees (then converted in radians)
%  - AspectRatio determines the field of view in the X (horizontal) direction:
%  AspectRatio = Width/Height
%  - ZNear specifies the distance from the viewer to the near clipping plane,
%  along the -Z axis (always strictly positive)
%  - ZFar specifies the distance from the viewer to the far clipping plane along
%  the -Z axis (always positive)
%
% For example Mp = perspective( _FoVYAngle=60.0,
% _AspectRatio=WindowWidth/WindowHeight, _ZNear=1.0, _ZFar=100.0 )
%
% Note that the context is a right-handed referential with a clip space in
% [-1.0, 1.0].
%
-spec perspective( radians(), ratio(), distance(), distance() ) -> matrix4().
perspective( FoVYAngle, AspectRatio, ZNear, ZFar ) ->

	% References:
	% https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPerspective.xml
	% and glm:perspectiveRH_NO (see glm/ext/matrix_clip_space.inl) and https://www.khronos.org/opengl/wiki/GluPerspective_code

	cond_utils:assert( myriad_check_linear,
					   not math_utils:is_null( AspectRatio ) ),

	% cotan(A) = 1/tan(A)

	Zero = 0.0,

	ZFactor = 1.0 / (ZNear - ZFar),

	TanHalfFovyInv = 1 / math:tan( FoVYAngle / 2.0 ),

	M11 = TanHalfFovyInv / AspectRatio,

	M22 = TanHalfFovyInv,

	M33 = ( ZNear + ZFar ) * ZFactor,

	M34 = 2 * ZFar * ZNear * ZFactor,

	% No possible compact form:
	#matrix4{ m11=M11,  m12=Zero, m13=Zero, m14=Zero,
			  m21=Zero, m22=M22,  m23=Zero, m24=Zero,
			  m31=Zero, m32=Zero, m33=M33,  m34=M34,
			  m41=Zero, m42=Zero, m43=-1.0, m44=Zero }.


% From https://www.khronos.org/opengl/wiki/GluPerspective_code:
%perspective2( FoVYAngle, AspectRatio, ZNear, ZFar ) ->
%	Ymax = ZNear * math:tan( FoVYAngle ),
%	Xmax = Ymax * AspectRatio,
%	frustum( -Xmax, Xmax, -Ymax, Ymax, ZNear, ZFar ).


% @doc Returns a matrix for projection corresponding to the specified settings.
-spec projection( projection_settings() ) -> matrix4().
projection( Settings=#orthographic_settings{} ) ->
	orthographic( Settings );

projection( Settings=#perspective_settings{} ) ->
	perspective( Settings ).



% @doc Returns a matrix for perspective projection corresponding to the
% specified settings.
%
% Parameters are:
%  - Left and Right are the coordinates for the left and right vertical clipping
% planes
%  - Bottom and Top are the coordinates for the bottom and top horizontal
% clipping planes
%  - ZNear specifies the distance from the viewer to the near clipping plane
%  (always strictly positive)
%  - ZFar specifies the distance from the viewer to the far clipping plane
%  (always positive)
%
% Note that the context is a right-handed referential with a clip space in
% [-1.0, 1.0].
%
-spec frustum( coordinate(), coordinate(), coordinate(), coordinate(),
			   distance(), distance() ) -> matrix4().
frustum( Left, Right, Bottom, Top, ZNear, ZFar ) ->

	% References:
	% https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFrustum.xml
	% and glm:frustumRH_NO.

	Zero = 0.0,

	VFactorInv = 1.0 / (Right - Left),
	HFactorInv = 1.0 / (Top - Bottom),
	MinusZFactorInv = 1.0 / (ZNear - ZFar),
	TwoNear = 2.0 * ZNear,

	M11 = TwoNear * VFactorInv,
	M13 = (Right + Left) * VFactorInv,

	M22 = TwoNear * HFactorInv,
	M23 = (Top + Bottom) * HFactorInv,
	M33 = (ZFar + ZNear) * MinusZFactorInv,

	M34 = TwoNear * ZFar * MinusZFactorInv,

	% No possible compact form:
	#matrix4{ m11=M11,  m12=Zero, m13=M13,  m14=Zero,
			  m21=Zero, m22=M22,  m23=M23,  m24=Zero,
			  m31=Zero, m32=Zero, m33=M33,  m34=M34,
			  m41=Zero, m42=Zero, m43=-1.0, m44=Zero }.



% @doc Returns a textual (approximate) representation of the specified
% projection settings.
%
-spec settings_to_string( projection_settings() ) -> ustring().
settings_to_string( #orthographic_settings{
		left=Left, right=Right, bottom=Bottom, top=Top,
		z_near=ZNear, z_far=ZFar } ) ->
	text_utils:format( "orthographic projection whose left vertical "
		"clipping plane is ~.2f, right is ~.2f, bottom is ~.2f, top is ~.2f, "
		"while the distance to the nearer depth clipping plane is ~.2f "
		"and ~.2f to the farther",
		[ Left, Right, Bottom, Top, ZNear, ZFar ] );

settings_to_string( #perspective_settings{
		fov_y_angle=FoVYAngle, aspect_ratio=AspectRatio,
		z_near=ZNear, z_far=ZFar } ) ->
	text_utils:format( "perspective projection whose field of view "
		"is ~.2f radians, aspect ratio is ~.2f, while the distance "
		"to the nearer depth clipping plane is ~.2f and ~.2f to the farther",
		[ FoVYAngle, AspectRatio, ZNear, ZFar ] ).
