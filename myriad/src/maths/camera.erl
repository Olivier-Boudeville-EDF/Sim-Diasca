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

-module(camera).

-moduledoc """
Gathering of various facilities for **camera** management, to define a view
point on a 3D world.

We adopted here the OpenGL camera conventions, where the camera points down to
the -Z axis of its coordinate system.
""".


% For the camera record:
-include("camera.hrl").


-doc "Describes a camera.".
-type camera() :: #camera{}.


-doc """
A view matrix, that is a transition matrix from the world space to a camera
space.
""".
-type view_matrix4() :: transition_matrix4().


-doc """
A transformation matrix, between a camera coordinate system and the world one.

More precisely, its reference matrix4 is Pcw, the transition one from camera to
world, and Pwc, its inverse - generally the most useful one - is the transition
matrix4 from world to camera space.
""".
-type view_transform4() :: transform4().


-export_type([ camera/0, view_matrix4/0, view_transform4/0 ]).



% Construction-related section.
-export([ create/3, set_position/2, look_at/3 ]).


% Operations on cameras:
-export([ get_view_matrix/1, get_view_matrix_inverse/1,
		  to_string/1, to_string/2 ]).



% Other operations, lower-level:
-export([ ]).


% For the compact_matrix4 record:
-include("matrix4.hrl").

% For the transform4 record:
-include("transform4.hrl").



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type point3() :: point3:point3().
-type vector3() :: vector3:vector3().

%-type matrix4() :: matrix4:matrix4().
-type transition_matrix4() :: matrix4:transition_matrix4().

-type transform4() :: transform4:transform4().

%-type unit_normal3() :: vector3:unit_normal3().

%-type rendering_info() :: mesh_render:rendering_info() .



% Implementation notes:




% Construction-related section.


-doc """
Returns a camera whose position is the specified one, looking at the specified
target point, relying on the specified vector for its up direction, which is
neither required to be unitary nor orthogonal to the aim vector (as obtained
from the position and the target point).
""".
-spec create( point3(), point3(), vector3() ) -> camera().
create( Position, TargetPoint, UpDir ) ->
	look_at( Position, TargetPoint, UpDir ).



-doc """
Sets the position of the camera to the specified point, and updates it
accordingly, so that it points to the same position.
""".
-spec set_position( point3(), camera() ) -> camera().
set_position( NewPos, #camera{ target=TargetPoint, up=Up } ) ->
	look_at( NewPos, TargetPoint, Up ).



-doc """
Returns a camera whose position is the specified one, looking at the specified
target point, relying on the specified vector for its up direction, which is
neither required to be unitary nor orthogonal to the aim vector (as obtained
from the position and the target point).

The returned transformation is especially interesting for its inverse matrix,
which is the transition one from the current (generally world) coordinate system
to the one of the corresponding camera (hence Pwc), according to our
conventions.
""".
-spec look_at( point3(), point3(), vector3() ) -> camera().
look_at( Position, TargetPoint, Up ) ->

	% Needed as such anyway:
	Aim = point3:unit_vectorize( Position, TargetPoint ),

	% Knowing that with OpenGL the camera shall point to its -Z axis, Aim
	% corresponds to -Z:
	%
	Z = vector3:negate( Aim ),


	% To obtain Y, knowing that Up is not necessarily orthogonal to Aim, so
	% Y = Up - (Up.Aim).Aim (and then normalised):
	%
	% (Up does not need to be unit)
	Y = vector3:get_unit_orthogonal( Up, Aim ),

	X = vector3:cross_product( Y, Z ),

	% Now we have the position and axes of the camera coordinate system in the
	% world one, and we need notably to determine the transition matrix from
	% world to coordinate, Pwc; knowing that Pcw is straightforward to obtain,
	% we go for the Tcw={reference=Pcw, inverse=Pwc} transformation (thus for
	% which R1=c, R2=w):
	%
	Tcw = transform4:basis_change( _O1InR2=Position, _FwdDir1InR2=X,
								   _UpDir1InR2=Z ),

	#camera{ position=Position,
			 target=TargetPoint,
			 aim=Aim,
			 up=Y,
			 left=X,
			 view_transf4=Tcw }.




% First attempt:

% -doc """
% Returns an updated camera in which the view matrix has been computed from its
% position and orientation.
% """.
% -spec compute_view_matrix( camera() ) -> { view_matrix4(), camera() }.
% compute_view_matrix( Cam=#camera{ position=_Position={ Px, Py, Pz },
%								  aim=UnitAimVec=[ Ax, Ay, Az ],
%								  up=UnitUpVec=[ Ux, Uy, Uz ],
%								  right=MaybeUnitRightVec } ) ->

%	UnitRightVec=[ Rx, Ry, Rz ] = case MaybeUnitRightVec of

%		undefined ->
%			RV = vector3:cross_product( UnitUpVec, UnitAimVec ),
%			cond_utils:if_defined( myriad_check_linear,
%								   vector:is_unitary( RV ) ),
%			RV;

%		RV ->
%			RV

%	end,

%	% Could be done based on a quaternion, or as
%	% https://learnopengl.com/Getting-started/Camera, or as glm (see
%	% ext/matrix_transform.inl); knowing that 'up' is already supposed to be
%	% orthogonal to 'aim', we preferred directly computing the transition matrix
%	% from world (w) to camera (c) coordinates, Pw->c, that can easily be
%	% computed from the coordinates of the axes of the world system in the
%	% camera one. Unfortunately we have only the reciprocal information (the
%	% coordinates of the camera in the world), and can only describe directly
%	% the inverse, Pc->w.
%	%
%	% However, using the naming conventions specified in the camera record:
%	% ViewMat4 = Ma.Mb where Ma:matrix([Rx,Ry,Rz,0], [Ux,Uy,Uz,0], [Ax,Ay,Az,0],
%	% [0,0,0,1]) and Mb:matrix([1,0,0,-Px], [0,1,0,-Py], [0,0,1,-Pz],
%	% [0,0,0,1]); so:
%	% ViewMat4 = matrix([Rx, Ry, Rz, Pz*Rz-Py*Ry-Px*Rx],
%	%                   [Ux, Uy, Uz, Pz*Uz-Py*Uy-Px*Ux],
%	%                   [Ax, Ay, Az, Az*Pz-Ay*Py-Ax*Px],
%	%                   [ 0,  0,  0,                 1])
%	%
%	% So:

%	Tx = Pz*Rz - Py*Ry - Px*Rx,
%	Ty = Pz*Uz - Py*Uy - Px*Ux,
%	Tz = Az*Pz - Ay*Py - Ax*Px,

%	ViewMat4 = #compact_matrix4{ m11=Rx, m12=Ry, m13=Rz, tx=Tx,
%								 m21=Ux, m22=Uy, m23=Uz, ty=Ty,
%								 m31=Ax, m32=Ay, m33=Az, tz=Tz },
% % For a camera located at
% % { 0.0 }
% % { 0.0 }
% % { 0.0 }
% % , aiming at
% % [ 0.0  ]
% % [ 0.0  ]
% % [ -1.0 ]
% %  and whose up direction is
% % [ 0.0 ]
% % [ 1.0 ]
% % [ 0.0 ]
% % , the view matrix is:
% % [ -1.0 0.0  0.0  0.0  ]
% % [ 0.0  1.0  0.0  0.0  ]
% % [ 0.0  0.0  -1.0 -0.0 ]
% % [ 0.0  0.0  0.0  1.0  ]

%	NewCam = Cam#camera{ right=UnitRightVec,
%						 view_transf4=ViewMat4 },

%	{ ViewMat4, NewCam }.



-doc """
Returns the view matrix of this camera, corresponding to the transition matrix4
from world to camera space.
""".
-spec get_view_matrix( camera() ) -> view_matrix4().
get_view_matrix( #camera{ view_transf4=#transform4{ inverse=InvM } } ) ->
	InvM.


-doc """
Returns the inverse of the view matrix of this camera, corresponding to the
transition matrix4 from camera to world space.
""".
-spec get_view_matrix_inverse( camera() ) -> view_matrix4().
get_view_matrix_inverse( #camera{
		view_transf4=#transform4{ reference=RefM } } ) ->
	RefM.




-doc "Returns a textual description of the specified camera.".
-spec to_string( camera() ) -> ustring().
to_string( Camera ) ->
	to_string( Camera, _Verbose=false ).



-doc """
Returns a textual description of the specified camera, with the specified
verbosity.
""".
-spec to_string( camera(), boolean() ) -> ustring().
to_string( #camera{ position=Pos,
					target=TargetPoint
					%aim=UnitAimVec
				  }, _Verbose=false ) ->
	text_utils:format( "camera located at ~ts, aimed at ~ts",
		[ point3:to_compact_string( Pos ),
		  point3:to_compact_string( TargetPoint )
		  %vector3:to_compact_string( UnitAimVec )
		] );

to_string( #camera{ position=Pos,
					target=TargetPoint,
					%aim=UnitAimVec,
					up=UpVec,
					view_transf4=Transf4 }, _Verbose=true ) ->
	text_utils:format( "camera located at ~ts, aimed at ~ts, "
		"whose up vector is ~ts, with a ~ts",
		[ point3:to_compact_string( Pos ),
		  point3:to_compact_string( TargetPoint ),
		  %vector3:to_compact_string( UnitAimVec ),
		  vector3:to_compact_string( UpVec ),
		  transform4:to_string( Transf4 ) ] ).
