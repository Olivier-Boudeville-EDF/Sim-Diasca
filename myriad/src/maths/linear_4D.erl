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
% Creation date: Monday, February 15, 2010.


% Gathering of various four dimensional linear facilities, mostly dealing with
% homogeneous matrices for 3D.
%
% See linear_4D_test.erl for the corresponding test.
%
-module(linear_4D).


% Relatively aggressive inlining for basic operations:
-compile( inline ).
-compile( { inline_size, 48 } ).


% For the mat4 record:
-include("linear_4D.hrl").

% For the mat3 record:
-include("linear_3D.hrl").


% Shorthands:

-type coordinate() :: linear:coordinate().
-type factor() :: linear:factor().


% A 4D vector, with floating-point coordinates.
%
% They are typically referenced as [X, Y, Z, W].
%
-type vector() :: { coordinate(), coordinate(), coordinate(), coordinate() }.


% Alias for 4x4 canonical matrices:
-type mat4() :: #mat4{}.
-type canonical_matrix() :: mat4().



% Aliases for 4x4 compact matrices:
-type cpt_mat4() :: #cpt_mat4{}.
-type compact_matrix() :: cpt_mat4().


-type matrix() :: 'identity_4' | canonical_matrix() | compact_matrix().


-export_type([ mat4/0, canonical_matrix/0,
			   cpt_mat4/0, compact_matrix/0,
			   matrix/0 ]).


% Operations common to vectors and matrices:
-export([ scale/2 ]).


% Vector-related operations:
-export([ null_vector/0, add/2, add/1 ]).


% Matrix-related operations:
-export([ null_matrix/0, identity/0, from_columns/4, from_rows/4,
		  from_coordinates/16, from_coordinates/12, from_3D/2,
		  to_canonical/1, to_compact/1, mult/2, are_equal/2,
		  to_string/1 ]).


-import( math_utils, [ is_null/1, are_close/2 ] ).
-import( linear, [ coord_to_string/1 ]).



% Implementation of vector-related operations.


% Returns the null (4D) vector.
-spec null_vector() -> vector().
null_vector() ->
	{ 0.0, 0.0, 0.0, 0.0 }.



% Adds the two specified (4D) vectors.
-spec add( vector(), vector() ) -> vector().
add( _Va={Xa,Ya,Za,Wa}, _Vb={Xb,Yb,Zb,Wb} ) ->
	{ Xa+Xb, Ya+Yb, Za+Zb, Wa+Wb }.



% Adds the specified (non-empty) list of (4D) vectors.
-spec add( [ vector() ] ) -> vector().
add( _Vectors=[ V | T ] ) ->
	add_vec_list( T, _Acc=V );

add( _Vectors=[] ) ->
	throw( cannot_add_empty_list ).


% (helper)
add_vec_list( _Vec=[], AccV ) ->
	AccV;

add_vec_list( _Vec=[ V | T ], AccV ) ->
	NewAccV = add( V, AccV ),
	add_vec_list( T, NewAccV ).



% Implementation of matrix-related operations.


% Returns the null (4x4) matrix.
-spec null_matrix() -> canonical_matrix().
null_matrix() ->
	#mat4{ m11=0.0, m12=0.0, m13=0.0, m14=0.0,
		   m21=0.0, m22=0.0, m23=0.0, m24=0.0,
		   m31=0.0, m32=0.0, m33=0.0, m34=0.0,
		   m41=0.0, m42=0.0, m43=0.0, m44=0.0 }.



% Returns the identity (4x4) matrix.
-spec identity() -> matrix().
identity() ->
	identity_4.



% Scales specified (4D) vector or matrix of specified factor.
-spec scale( vector(), factor() ) -> vector();
		   ( matrix(), factor() ) -> matrix().
scale( _V={X,Y,Z,W}, Factor ) ->
	{ Factor*X, Factor*Y, Factor*Z, Factor*W };

scale( #cpt_mat4{ m11=M11, m12=M12, m13=M13, tx=Tx,
				  m21=M21, m22=M22, m23=M23, ty=Ty,
				  m31=M31, m32=M32, m33=M33, tz=Tz }, Factor ) ->
	#cpt_mat4{ m11=Factor*M11, m12=Factor*M12, m13=Factor*M13, tx=Factor*Tx,
			   m21=Factor*M21, m22=Factor*M22, m23=Factor*M23, ty=Factor*Ty,
			   m31=Factor*M31, m32=Factor*M32, m33=Factor*M33, tz=Factor*Tz };

scale( #mat4{ m11=M11, m12=M12, m13=M13, m14=Tx,
			  m21=M21, m22=M22, m23=M23, m24=Ty,
			  m31=M31, m32=M32, m33=M33, m34=Tz,
			  m41=M41, m42=M42, m43=M43, m44=Tw }, Factor ) ->
	#mat4{ m11=Factor*M11, m12=Factor*M12, m13=Factor*M13, m14=Factor*Tx,
		   m21=Factor*M21, m22=Factor*M22, m23=Factor*M23, m24=Factor*Ty,
		   m31=Factor*M31, m32=Factor*M32, m33=Factor*M33, m34=Factor*Tz,
		   m41=Factor*M41, m42=Factor*M42, m43=Factor*M43, m44=Factor*Tw };

scale( identity_4, Factor ) ->
	scale( to_canonical( identity_4 ), Factor ).




% Returns the (4x4) matrix whose columns correspond to the specified 4 vectors:
%  [ Va Vb Vc Vd ]
%  [ |  |  |  |  ]
%  [ |  |  |  |  ]
%  [ |  |  |  |  ]
%
-spec from_columns( vector(), vector(), vector(), vector() ) -> 
						    canonical_matrix().
from_columns( _Va={Xa,Ya,Za,Wa}, _Vb={Xb,Yb,Zb,Wb},
			  _Vc={Xc,Yc,Zc,Wc}, _Vd={Xd,Yd,Zd,Wd} ) ->
	#mat4{ m11=Xa, m12=Xb, m13=Xc, m14=Xd,
		   m21=Ya, m22=Yb, m23=Yc, m24=Yd,
		   m31=Za, m32=Zb, m33=Zc, m34=Zd,
		   m41=Wa, m42=Wb, m43=Wc, m44=Wd }.



% Returns the (4x4) matrix whose columns correspond to the specified 4 vectors:
% [ Va - - - ]
% [ Vb - - - ]
% [ Vc - - - ]
% [ Vd - - - ]
%
-spec from_rows( vector(), vector(), vector(), vector() ) -> canonical_matrix().
from_rows( _Va={Xa,Ya,Za,Wa}, _Vb={Xb,Yb,Zb,Wb},
		   _Vc={Xc,Yc,Zc,Wc}, _Vd={Xd,Yd,Zd,Wd} ) ->
	#mat4{ m11=Xa, m12=Ya, m13=Za, m14=Wa,
		   m21=Xb, m22=Yb, m23=Zb, m24=Wb,
		   m31=Xc, m32=Yc, m33=Zc, m34=Wc,
		   m41=Xd, m42=Yd, m43=Zd, m44=Wd }.



% Returns the (4x4, canonical) matrix whose (16) coordinates are the specified
% ones, specified rows after rows.
%
-spec from_coordinates( coordinate(), coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(), coordinate() )
					  -> canonical_matrix().
from_coordinates( A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
				  A15, A16 ) ->
	#mat4{ m11=A1,  m12=A2,  m13=A3,  m14=A4,
		   m21=A5,  m22=A6,  m23=A7,  m24=A8,
		   m31=A9,  m32=A10, m33=A11, m34=A12,
		   m41=A13, m42=A14, m43=A15, m44=A16 }.



% Returns the (4x4, compact) matrix whose (12) coordinates are the specified
% ones, specified rows after rows.
%
-spec from_coordinates( coordinate(), coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(), coordinate() )
					  -> compact_matrix().
from_coordinates( A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12 ) ->
	#cpt_mat4{ m11=A1,  m12=A2,  m13=A3,  tx=A4,
			   m21=A5,  m22=A6,  m23=A7,  ty=A8,
			   m31=A9,  m32=A10, m33=A11, tz=A12 }.



% Returns the (4x4) compact matrix obtained from specified 3x3 matrix and
% vector.
%
-spec from_3D( linear_3D:matrix(), linear_3D:vector() ) -> compact_matrix().
from_3D( #mat3{ m11=M11, m12=M12, m13=M13,
				m21=M21, m22=M22, m23=M23,
				m31=M31, m32=M32, m33=M33 },
		 _Vec3={ X, Y, Z } ) ->
	#cpt_mat4{ m11=M11, m12=M12, m13=M13, tx=X,
			   m21=M21, m22=M22, m23=M23, ty=Y,
			   m31=M31, m32=M32, m33=M33, tz=Z }.


% Returns the canonical form of specified (4x4) matrix.
-spec to_canonical( matrix() ) -> canonical_matrix().
to_canonical( #cpt_mat4{ m11=M11, m12=M12, m13=M13, tx=Tx,
						 m21=M21, m22=M22, m23=M23, ty=Ty,
						 m31=M31, m32=M32, m33=M33, tz=Tz } ) ->
	#mat4{ m11=M11, m12=M12, m13=M13, m14=Tx,
		   m21=M21, m22=M22, m23=M23, m24=Ty,
		   m31=M31, m32=M32, m33=M33, m34=Tz,
		   m41=0.0, m42=0.0, m43=0.0, m44=1.0 };

to_canonical( identity_4 ) ->
	#mat4{ m11=1.0, m12=0.0, m13=0.0, m14=0.0,
		   m21=0.0, m22=1.0, m23=0.0, m24=0.0,
		   m31=0.0, m32=0.0, m33=1.0, m34=0.0,
		   m41=0.0, m42=0.0, m43=0.0, m44=1.0 };

to_canonical( M ) when is_record( M, mat4 ) ->
	M.




% Returns the compact form of specified (4x4) matrix.
%
% Throws an exception if the specified matrix cannot be expressed as a compact
% one.
%
-spec to_compact( matrix() ) -> compact_matrix().
to_compact( M=#mat4{ m11=M11, m12=M12, m13=M13, m14=M14,
					 m21=M21, m22=M22, m23=M23, m24=M24,
					 m31=M31, m32=M32, m33=M33, m34=M34,
					 m41=M41, m42=M42, m43=M43, m44=M44 }) ->
	case is_null( M41 ) andalso is_null( M42 ) andalso is_null( M43 )
		andalso are_close( M44, 1.0 ) of

		true ->
			% Just drop the last row:
			#cpt_mat4{ m11=M11, m12=M12, m13=M13, tx=M14,
					   m21=M21, m22=M22, m23=M23, ty=M24,
					   m31=M31, m32=M32, m33=M33, tz=M34 };

		false ->
			trace_utils:error_fmt( "Canonical matrix~n~ts~ncannot be expressed "
								   "as a compact one.", [ to_string( M ) ] ),

			throw( { not_compactable, M } )

	end;


to_compact( identity_4 ) ->
	#cpt_mat4{ m11=1.0, m12=0.0, m13=0.0, tx=0.0,
			   m21=0.0, m22=1.0, m23=0.0, ty=0.0,
			   m31=0.0, m32=0.0, m33=1.0, tz=0.0 };


to_compact( M ) when is_record( M, cpt_mat4 ) ->
	M.



% Multiplies the first matrix by the second one: returns Mc = Ma.Mb.
-spec mult( matrix(), matrix() ) -> matrix().
mult( identity_4, M ) ->
	M;

mult( M, identity_4 ) ->
	M;

mult( _Ma=#mat4{ m11=A11, m12=A12, m13=A13, m14=A14,
				 m21=A21, m22=A22, m23=A23, m24=A24,
				 m31=A31, m32=A32, m33=A33, m34=A34,
				 m41=A41, m42=A42, m43=A43, m44=A44 },
	  _Mb=#mat4{ m11=B11, m12=B12, m13=B13, m14=B14,
				 m21=B21, m22=B22, m23=B23, m24=B24,
				 m31=B31, m32=B32, m33=B33, m34=B34,
				 m41=B41, m42=B42, m43=B43, m44=B44 } ) ->

	C11 = A11*B11 + A12*B21 + A13*B31 + A14*B41,
	C12 = A11*B12 + A12*B22 + A13*B32 + A14*B42,
	C13 = A11*B13 + A12*B23 + A13*B33 + A14*B43,
	C14 = A11*B14 + A12*B24 + A13*B34 + A14*B44,

	C21 = A21*B11 + A22*B21 + A23*B31 + A24*B41,
	C22 = A21*B12 + A22*B22 + A23*B32 + A24*B42,
	C23 = A21*B13 + A22*B23 + A23*B33 + A24*B43,
	C24 = A21*B14 + A22*B24 + A23*B34 + A24*B44,

	C31 = A31*B11 + A32*B21 + A33*B31 + A34*B41,
	C32 = A31*B12 + A32*B22 + A33*B32 + A34*B42,
	C33 = A31*B13 + A32*B23 + A33*B33 + A34*B43,
	C34 = A31*B14 + A32*B24 + A33*B34 + A34*B44,

	C41 = A41*B11 + A42*B21 + A43*B31 + A44*B41,
	C42 = A41*B12 + A42*B22 + A43*B32 + A44*B42,
	C43 = A41*B13 + A42*B23 + A43*B33 + A44*B43,
	C44 = A41*B14 + A42*B24 + A43*B34 + A44*B44,

	#mat4{ m11=C11, m12=C12, m13=C13, m14=C14,
		   m21=C21, m22=C22, m23=C23, m24=C24,
		   m31=C31, m32=C32, m33=C33, m34=C34,
		   m41=C41, m42=C42, m43=C43, m44=C44 };

mult( _Ma=#cpt_mat4{ m11=A11, m12=A12, m13=A13, tx=Tx,
					 m21=A21, m22=A22, m23=A23, ty=Ty,
					 m31=A31, m32=A32, m33=A33, tz=Tz },
	  _Mb=#mat4{ m11=B11, m12=B12, m13=B13, m14=B14,
				 m21=B21, m22=B22, m23=B23, m24=B24,
				 m31=B31, m32=B32, m33=B33, m34=B34,
				 m41=B41, m42=B42, m43=B43, m44=B44 } ) ->


	C11 = A11*B11 + A12*B21 + A13*B31 + Tx*B41,
	C12 = A11*B12 + A12*B22 + A13*B32 + Tx*B42,
	C13 = A11*B13 + A12*B23 + A13*B33 + Tx*B43,
	C14 = A11*B14 + A12*B24 + A13*B34 + Tx*B44,

	C21 = A21*B11 + A22*B21 + A23*B31 + Ty*B41,
	C22 = A21*B12 + A22*B22 + A23*B32 + Ty*B42,
	C23 = A21*B13 + A22*B23 + A23*B33 + Ty*B43,
	C24 = A21*B14 + A22*B24 + A23*B34 + Ty*B44,

	C31 = A31*B11 + A32*B21 + A33*B31 + Tz*B41,
	C32 = A31*B12 + A32*B22 + A33*B32 + Tz*B42,
	C33 = A31*B13 + A32*B23 + A33*B33 + Tz*B43,
	C34 = A31*B14 + A32*B24 + A33*B34 + Tz*B44,

	C41 = B41,
	C42 = B42,
	C43 = B43,
	C44 = B44,

	#mat4{ m11=C11, m12=C12, m13=C13, m14=C14,
		   m21=C21, m22=C22, m23=C23, m24=C24,
		   m31=C31, m32=C32, m33=C33, m34=C34,
		   m41=C41, m42=C42, m43=C43, m44=C44 };

mult( _Ma=#mat4{ m11=A11, m12=A12, m13=A13, m14=A14,
				 m21=A21, m22=A22, m23=A23, m24=A24,
				 m31=A31, m32=A32, m33=A33, m34=A34,
				 m41=A41, m42=A42, m43=A43, m44=A44 },
	  _Mb=#cpt_mat4{ m11=B11, m12=B12, m13=B13, tx=Tx,
					 m21=B21, m22=B22, m23=B23, ty=Ty,
					 m31=B31, m32=B32, m33=B33, tz=Tz } ) ->

	C11 = A11*B11 + A12*B21 + A13*B31,
	C12 = A11*B12 + A12*B22 + A13*B32,
	C13 = A11*B13 + A12*B23 + A13*B33,
	C14 = A11*Tx  + A12*Ty  + A13*Tz + A14,

	C21 = A21*B11 + A22*B21 + A23*B31,
	C22 = A21*B12 + A22*B22 + A23*B32,
	C23 = A21*B13 + A22*B23 + A23*B33,
	C24 = A21*Tx  + A22*Ty  + A23*Tz + A24,

	C31 = A31*B11 + A32*B21 + A33*B31,
	C32 = A31*B12 + A32*B22 + A33*B32,
	C33 = A31*B13 + A32*B23 + A33*B33,
	C34 = A31*Tx  + A32*Ty  + A33*Tz + A34,

	C41 = A41*B11 + A42*B21 + A43*B31,
	C42 = A41*B12 + A42*B22 + A43*B32,
	C43 = A41*B13 + A42*B23 + A43*B33,
	C44 = A41*Tx  + A42*Ty  + A43*Tz + A44,

	#mat4{ m11=C11, m12=C12, m13=C13, m14=C14,
		   m21=C21, m22=C22, m23=C23, m24=C24,
		   m31=C31, m32=C32, m33=C33, m34=C34,
		   m41=C41, m42=C42, m43=C43, m44=C44 };

mult( _Ma=#cpt_mat4{ m11=A11, m12=A12, m13=A13, tx=Ax,
					 m21=A21, m22=A22, m23=A23, ty=Ay,
					 m31=A31, m32=A32, m33=A33, tz=Az },
	  _Mb=#cpt_mat4{ m11=B11, m12=B12, m13=B13, tx=Bx,
					 m21=B21, m22=B22, m23=B23, ty=By,
					 m31=B31, m32=B32, m33=B33, tz=Bz } ) ->

	C11 = A11*B11 + A12*B21 + A13*B31,
	C12 = A11*B12 + A12*B22 + A13*B32,
	C13 = A11*B13 + A12*B23 + A13*B33,
	Cx  = A11*Bx  + A12*By  + A13*Bz + Ax,

	C21 = A21*B11 + A22*B21 + A23*B31,
	C22 = A21*B12 + A22*B22 + A23*B32,
	C23 = A21*B13 + A22*B23 + A23*B33,
	Cy  = A21*Bx  + A22*By  + A23*Bz + Ay,

	C31 = A31*B11 + A32*B21 + A33*B31,
	C32 = A31*B12 + A32*B22 + A33*B32,
	C33 = A31*B13 + A32*B23 + A33*B33,
	Cz  = A31*Bx  + A32*By  + A33*Bz + Az,

	% C4{1,2,3} are 0.0, C44 is 1.0.

	#cpt_mat4{ m11=C11, m12=C12, m13=C13, tx=Cx,
			   m21=C21, m22=C22, m23=C23, ty=Cy,
			   m31=C31, m32=C32, m33=C33, tz=Cz }.



% Tells whether the two specified (4x4) matrices are equal.
-spec are_equal( matrix(), matrix() ) -> boolean().
are_equal( _Ma=identity_4, _Mb=identity_4 ) ->
	true;

are_equal( _Ma=#mat4{ m11=A11, m12=A12, m13=A13, m14=A14,
					  m21=A21, m22=A22, m23=A23, m24=A24,
					  m31=A31, m32=A32, m33=A33, m34=A34,
					  m41=A41, m42=A42, m43=A43, m44=A44 },
		   _Mb=#mat4{ m11=B11, m12=B12, m13=B13, m14=B14,
					  m21=B21, m22=B22, m23=B23, m24=B24,
					  m31=B31, m32=B32, m33=B33, m34=B34,
					  m41=B41, m42=B42, m43=B43, m44=B44 } ) ->
				are_close( A11, B11 ) andalso are_close( A12, B12 )
		andalso are_close( A13, B13 ) andalso are_close( A14, B14 )
		andalso are_close( A21, B21 ) andalso are_close( A22, B22 )
		andalso are_close( A23, B23 ) andalso are_close( A24, B24 )
		andalso are_close( A31, B31 ) andalso are_close( A32, B32 )
		andalso are_close( A33, B33 ) andalso are_close( A34, B34 )
		andalso are_close( A41, B41 ) andalso are_close( A42, B42 )
		andalso are_close( A43, B43 ) andalso are_close( A44, B44 );

are_equal( _Ma=#cpt_mat4{ m11=A11, m12=A12, m13=A13, tx=Ax,
						  m21=A21, m22=A22, m23=A23, ty=Ay,
						  m31=A31, m32=A32, m33=A33, tz=Az },
		   _Mb=#cpt_mat4{ m11=B11, m12=B12, m13=B13, tx=Bx,
						  m21=B21, m22=B22, m23=B23, ty=By,
						  m31=B31, m32=B32, m33=B33, tz=Bz } ) ->
				are_close( A11, B11 ) andalso are_close( A12, B12 )
		andalso are_close( A13, B13 ) andalso are_close( Ax,  Bx )
		andalso are_close( A21, B21 ) andalso are_close( A22, B22 )
		andalso are_close( A23, B23 ) andalso are_close( Ay,  By )
		andalso are_close( A31, B31 ) andalso are_close( A32, B32 )
		andalso are_close( A33, B33 ) andalso are_close( Az,  Bz );

are_equal( _Ma=#mat4{ m11=A11, m12=A12, m13=A13, m14=A14,
					  m21=A21, m22=A22, m23=A23, m24=A24,
					  m31=A31, m32=A32, m33=A33, m34=A34,
					  m41=A41, m42=A42, m43=A43, m44=A44 },
		   _Mb=#cpt_mat4{ m11=B11, m12=B12, m13=B13, tx=Bx,
						  m21=B21, m22=B22, m23=B23, ty=By,
						  m31=B31, m32=B32, m33=B33, tz=Bz } ) ->
				are_close( A11, B11 ) andalso are_close( A12, B12 )
		andalso are_close( A13, B13 ) andalso are_close( A14, Bx  )
		andalso are_close( A21, B21 ) andalso are_close( A22, B22 )
		andalso are_close( A23, B23 ) andalso are_close( A24, By  )
		andalso are_close( A31, B31 ) andalso are_close( A32, B32 )
		andalso are_close( A33, B33 ) andalso are_close( A34, Bz  )
		andalso is_null( A41 ) andalso is_null( A42 )
		andalso is_null( A43 ) andalso are_close( A44, 1.0 );

are_equal( Ma=#cpt_mat4{}, Mb=#mat4{} ) ->
	are_equal( Mb, Ma );

are_equal( Ma, _Mb=identity_4 ) ->
	are_equal( Ma, to_compact( identity_4 ) );

are_equal( _Ma=identity_4, Mb ) ->
	are_equal( Mb, identity_4 ).




% Returns a textual representation of specified (4x4) vector or matrix.
-spec to_string( vector() | matrix() ) -> text_utils:ustring().
to_string( _Vector={X,Y,Z,W} ) ->
	text_utils:format( "[ ~ts, ~ts, ~ts, ~ts ]",
					   [ coord_to_string( X ), coord_to_string( Y ),
						 coord_to_string( Z ), coord_to_string( W ) ] );

to_string( _Matrix=identity_4 ) ->
	"4x4 identity";

to_string( _Matrix=#mat4{ m11=M11, m12=M12, m13=M13, m14=M14,
						  m21=M21, m22=M22, m23=M23, m24=M24,
						  m31=M31, m32=M32, m33=M33, m34=M34,
						  m41=M41, m42=M42, m43=M43, m44=M44 } ) ->
	Elements = [ M11, M12, M13, M14,
				 M21, M22, M23, M24,
				 M31, M32, M33, M34,
				 M41, M42, M43, M44 ],

	ElemStrings = [ coord_to_string( E ) || E <- Elements ],

	text_utils:format( "[ ~ts, ~ts, ~ts, ~ts ]~n"
					   "[ ~ts, ~ts, ~ts, ~ts ]~n"
					   "[ ~ts, ~ts, ~ts, ~ts ]~n"
					   "[ ~ts, ~ts, ~ts, ~ts ]~n",
					   ElemStrings );

to_string( _Matrix=#cpt_mat4{ m11=M11, m12=M12, m13=M13, tx=Tx,
							  m21=M21, m22=M22, m23=M23, ty=Ty,
							  m31=M31, m32=M32, m33=M33, tz=Tz } ) ->
	Elements = [ M11, M12, M13, Tx,
				 M21, M22, M23, Ty,
				 M31, M32, M33, Tz ],

	ElemStrings = [ coord_to_string( E ) || E <- Elements ],

	text_utils:format( "[ ~ts, ~ts, ~ts, ~ts ]~n"
					   "[ ~ts, ~ts, ~ts, ~ts ]~n"
					   "[ ~ts, ~ts, ~ts, ~ts ]~n",
					   ElemStrings ).
