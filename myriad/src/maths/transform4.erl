% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Wednesday, February 16, 2022.


% @doc Module implementing the support for <b>4x4 transformations</b>.
%
% See also matrix4.erl
%
-module(transform4).



% Implementation notes:
%
% Requiring the inverse of a transformation happens frequently, so a
% transformation is a pair of matrices: a given matrix and its precomputed
% inverse, so that either can be easily returned.
%
% When applying to a transformation T an "operation" O (ex: translation,
% rotation, scaling, an), we consider that this operation shall happen after the
% transformation.
%
% So, in terms of matrices, if T' is the resulting transformation, Mt' = Mo.Mt,
% so that for a vector V we have V'= Mt'.V = Mo.Mt.V = Mo.(Mt.v) i.e. first
% apply T, then O.
%
% As for the stored inverse, InvMt' = InvMt.InvMo.
%
% The various types of matrix4 (canonical, compact, identity) are transparently
% managed.


% For records like matrix4:
-include("matrix4.hrl").

% For the #transform4 record:
-include("transform4.hrl").

-type transform4() :: #transform4{}.
% A 4x4 transformation, storing both its corresponding 4x4 matrix and its
% inverse.



-export_type([ transform4/0 ]).


-export([ identity/0, new/1,
		  get_reference/1, get_inverse/1,
		  translate/2, rotate/2, rotate/3, scale/2, mult/2, mult/1,
		  basis_change/3,
		  inverse/1,
		  to_string/1 ] ).


%-import( math_utils, [ is_null/1, are_close/2 ] ).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type radians() :: unit_utils:radians().

-type point3() :: point3:point3().

-type vector3() :: vector3:vector3().
-type unit_vector3() :: vector3:unit_vector3().

-type matrix4() :: matrix4:matrix4().



% @doc Returns the (4D) identity transformation.
-spec identity() -> transform4().
identity() ->
	#transform4{}.



% @doc Returns the (4D) identity transformation whose reference matrix is the
% specified one.
%
-spec new( matrix4() ) -> transform4().
new( M ) ->
	case matrix4:inverse( M ) of

		undefined ->
			throw( { non_invertible_matrix, M } );

		InvM ->
			#transform4{ matrix=M, inverse=InvM }

	end.



% @doc Returns the reference matrix corresponding to the specified
% transformation.
%
-spec get_reference( transform4() ) -> matrix4().
get_reference( #transform4{ matrix=M } ) ->
	M.



% @doc Returns the inverse of the matrix corresponding to the specified
% transformation.
%
-spec get_inverse( transform4() ) -> matrix4().
get_inverse( #transform4{ inverse=InvM } ) ->
	InvM.



% @doc Returns the specified transformation once composed with an additional
% translation (applied last) of the specified 3D vector.
%
-spec translate( transform4(), vector3() ) -> transform4().
translate( #transform4{ matrix=M, inverse=InvM }, VT ) ->

	% TO-DO: hardcode/inline directly these operations:

	% This new translation is to be applied last:
	NewM = matrix4:mult( matrix4:translation( VT ), M ),

	MinusVT = vector3:negate( VT ),

	NewInvM = matrix4:mult( InvM, matrix4:translation( MinusVT ) ),

	#transform4{ matrix=NewM, inverse=NewInvM }.



% @doc Returns the specified transformation once composed with an additional
% rotation (applied last) directly specified as a matrix.
%
-spec rotate( transform4(), matrix4() ) -> transform4().
rotate( #transform4{ matrix=M, inverse=InvM }, MRot ) ->

	% For orthogonal matrices like rotations, the inverse are their transpose:
	InvMRot = matrix4:transpose( MRot ),

	#transform4{ matrix=matrix4:mult( MRot, M ),
				 inverse=matrix4:mult( InvM, InvMRot ) }.



% @doc Returns the specified transformation once composed with an additional
% rotation (applied last), specified as a (unit) axis and an angle.
%
-spec rotate( transform4(), unit_vector3(), radians() ) -> transform4().
rotate( Transform, UnitAxis, RadAngle ) ->
	MRot = matrix4:rotation( UnitAxis, RadAngle ),
	rotate( Transform, MRot ).



% @doc Returns the specified transformation once composed with an additional
% scaling (applied last), specified as a vector (of non-null factors).
%
-spec scale( transform4(), vector3() ) -> transform4().
scale( #transform4{ matrix=M, inverse=InvM }, VS=[ Vx, Vy, Vz ] ) ->

	MS = matrix4:scaling( VS ),

	One = 1.0,

	InvMS = matrix4:scaling( [ One/Vx, One/Vy, One/Vz ] ),

	#transform4{ matrix=matrix4:mult( MS, M ),
				 inverse=matrix4:mult( InvM, InvMS ) }.



% @doc Returns the transformation corresponding to the in-order multiplication
% of the two specified ones.
%
-spec mult( transform4(), transform4() ) -> transform4().
mult( #transform4{ matrix=M1, inverse=InvM1 },
	  #transform4{ matrix=M2, inverse=InvM2 } ) ->
	M = matrix4:mult( M2, M1 ),
	InvM = matrix4:mult( InvM1, InvM2 ),
	#transform4{ matrix=M, inverse=InvM }.


% @doc Returns the transformation corresponding to the in-order multiplication
% of the specified ones.
%
-spec mult( [ transform4() ] ) -> transform4().
mult( _Transforms=[ T1, T2 | T ] ) ->
	mult( mult( T1, T2 ), T );

mult( _Transforms=[ T ] ) ->
	T.



% @doc Returns a transition transformation whose reference matrix is a
% change-of-basis matrix from the current referential (R1) to one (R2) whose
% origin, forward and up directions are the specified ones (still in the current
% referential R1).
%
% So returns P1->2, allowing, for an (homogeneous) point P, to convert P1, its
% representation in current referential R1, into P2, its counterpart in R2:
% P2 = P1->2.P1.
%
% The inverse matrix in this transformation is thus P2->1.
%
-spec basis_change( point3(), vector3(), vector3() ) -> transform4().
basis_change( _O2InR1={ XO2, YO2, ZO2 }, FwdDir2InR1, UpDir2InR1 ) ->

	% A point whose coordinates are to convert from R1 to R2 shall first be
	% rotated, then translated; we determine here the axis vectors of R2, as
	% expressed in R1; refer to
	% https://howtos.esperide.org/ThreeDimensional.html#summary for more
	% information.

	% We now inline the definition of both matrices:

	_X2InR1 = [ XI2, YI2, ZI2 ] = FwdDir2InR1,

	_Z2InR1 = [ XK2, YK2, ZK2 ] = UpDir2InR1,

	% Y = Z^X:
	%_Y2InR1 = [ XJ2, YJ2, ZJ2 ] = vector3:cross_product( Z2InR1, X2InR1 ),

	XJ2 = YK2*ZI2 - ZK2*YI2,
	YJ2 = ZK2*XI2 - XK2*ZI2,
	ZJ2 = XK2*YI2 - YK2*XI2,

	%M = matrix4:compact_from_columns( X2InR1, Y2InR1, Z2InR1, O2InR1 ),
	M = #compact_matrix4{ m11=XI2, m12=XJ2, m13=XK2, tx=XO2,
						  m21=YI2, m22=YJ2, m23=YK2, ty=YO2,
						  m31=ZI2, m32=ZJ2, m33=ZK2, tz=ZO2 },

	% Reversed, reciprocal operations; we compute the inverse of M by applying
	% https://howtos.esperide.org/ThreeDimensional.html#summary:

	% Negation of the scalar product of new rows with new origin:
	InvTx = - ( XI2*XO2 + YI2*YO2 + ZI2*ZO2 ),
	InvTy = - ( XJ2*XO2 + YJ2*YO2 + ZJ2*ZO2 ),
	InvTz = - ( XK2*XO2 + YK2*YO2 + ZK2*ZO2 ),

	InvM = #compact_matrix4{ m11=XI2, m12=YI2, m13=ZI2, tx=InvTx,
							 m21=XJ2, m22=YJ2, m23=ZJ2, ty=InvTy,
							 m31=XK2, m32=YK2, m33=ZK2, tz=InvTz },

	#transform4{ matrix=M, inverse=InvM }.



% @doc Returns the inverse transformation of the specified one.
-spec inverse( transform4() ) -> transform4().
inverse( #transform4{ matrix=M, inverse=InvM } ) ->
	#transform4{ matrix=InvM, inverse=M }.



% @doc Returns a textual representation of the specified (4x4) transformation.
-spec to_string( transform4() ) -> ustring().
to_string( #transform4{ matrix=M, inverse=InvM } ) ->
	text_utils:format( "4x4 transformation recording reference matrix ~ts and "
		"its inverse ~ts",
		[ matrix4:to_string( M ), matrix4:to_string( InvM ) ] ).
