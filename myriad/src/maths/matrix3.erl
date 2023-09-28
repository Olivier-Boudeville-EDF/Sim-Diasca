% Copyright (C) 2021-2023 Olivier Boudeville
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
% Creation date: Friday, October 8, 2021.


% @doc Module implementing the support for <b>3x3 matrices</b>.
%
% See also:
% - the corresponding (3D) vectors, in vector3.erl
% - the (unspecialised) matrices of arbitrary dimensions, in matrix.erl
%
-module(matrix3).



% Implementation notes:
%
% These 3x3 matrices come in various forms:
%
% - the canonical one (3x3 coordinates)
%
% - the compact one (3x3) where the last row is (implicitly) [0.0, 0.0, 1.0],
% typical of transformations (translation+rotation) in 2D
%
% - special ones, at least the identity matrix


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For records like matrix2:
-include("matrix2.hrl").

% For records like matrix3:
-include("matrix3.hrl").


-type user_matrix3() :: user_matrix().
% A matrix3 can be specified as a list of same-size rows (akin to a user
% arbitrary matrix) containing any kind of numerical coordinates.


-type matrix3() :: 'identity_3' | canonical_matrix3() | compact_matrix3().


% Alias for 3x3 canonical matrices:
-type canonical_matrix3() :: #matrix3{}.


% Aliases for 3x3 compact matrices:
-type compact_matrix3() :: #compact_matrix3{}.


-type rot_matrix3() :: canonical_matrix3().
% A matrix describing a 3D rotation.

-type tuple_matrix3() :: % Not exported yet: gl:m9() | gl:m6().
						 type_utils:tuple( coordinate(), 9 )
					   | type_utils:tuple( coordinate(), 6 ).
% A tuple of 6 or 9 coordinates.


-export_type([ user_matrix3/0, matrix3/0, canonical_matrix3/0,
			   compact_matrix3/0, rot_matrix3/0, tuple_matrix3/0 ]).


-export([ new/1, new/2, null/0, identity/0, translation/1, scaling/1,
		  rotation/2,
		  from_columns/3, from_rows/3,
		  from_coordinates/9, from_compact_coordinates/6,
		  from_2D/2,
		  from_arbitrary/1, to_arbitrary/1,
		  dimension/0, dimensions/0,
		  row/2, column/2,
		  get_element/3, set_element/4,
		  transpose/1,
		  scale/2,
		  add/2, sub/2, mult/2, mult/1, apply/2,
		  are_equal/2,
		  determinant/1, comatrix/1, inverse/1,
		  to_canonical/1, to_compact/1,
		  check/1,
		  to_string/1 ] ).


-import( math_utils, [ is_null/1, are_close/2 ] ).

% To avoid clash with BIF:
-compile( { no_auto_import, [ apply/2 ] } ).


-define( dim, 3 ).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type factor() :: math_utils:factor().

-type radians() :: unit_utils:radians().

-type coordinate() :: linear:coordinate().
-type dimension() :: linear:dimension().
-type scalar() :: linear:scalar().

-type vector2() :: vector2:vector2().
-type point2() :: point2:point2().

-type user_vector3() :: vector3:user_vector3().
-type vector3() :: vector3:vector3().
-type unit_vector3() :: vector3:unit_vector3().
-type point3() :: point3:point3().

-type dimensions() :: matrix:dimensions().

-type matrix2() :: matrix2:matrix2().

-type user_matrix() :: matrix:user_matrix().
-type matrix() :: matrix:matrix().



% @doc Returns a 3D (canonical) matrix corresponding to the user-specified
% matrix.
%
-spec new( user_matrix3() ) -> canonical_matrix3().
new( UserMatrix ) ->

	CoordList = [ type_utils:ensure_float( C )
					|| C <- list_utils:flatten_once( UserMatrix ) ],

	% Returns a #matrix3 record (i.e. a tagged tuple):
	list_to_tuple( [ 'matrix3' | CoordList ] ).



% @doc Returns a 3D compact matrix corresponding to the user-specified
% row vectors.
%
-spec new( user_vector3(), user_vector3() ) -> compact_matrix3().
new( UserVecRow1, UserVecRow2 ) ->

	Rows = [ vector3:new( UVR ) || UVR <- [ UserVecRow1, UserVecRow2 ] ],

	CoordList = list_utils:flatten_once( Rows ),

	% Returns a #compact_matrix3 record (i.e. a tagged tuple):
	list_to_tuple( [ 'compact_matrix3' | CoordList ] ).



% @doc Returns the null (3x3) matrix.
-spec null() -> canonical_matrix3().
null() ->
	Zero = 0.0,
	CoordList = lists:duplicate( _N=?dim * ?dim, Zero ),
	list_to_tuple( [ 'matrix3' | CoordList ] ).



% @doc Returns the identity (3x3) matrix.
-spec identity() -> matrix3().
identity() ->
	identity_3.



% @doc Returns the (3x3) homogeneous (thus compact) matrix corresponding to a
% translation of the specified vector.
%
-spec translation( vector2() ) -> compact_matrix3().
translation( _VT=[ Tx, Ty ] ) ->
	Zero = 0.0,
	One = 1.0,
	#compact_matrix3{ m11=One, m12=Zero, tx=Tx,
					  m21=Zero, m22=One, ty=Ty }.



% @doc Returns the (3x3) homogeneous (thus compact) matrix corresponding to the
% scaling of the specified factors.
%
-spec scaling( { factor(), factor() } ) -> compact_matrix3().
scaling( { Sx, Sy } ) ->
	Zero = 0.0,
	#compact_matrix3{ m11=Sx,   m12=Zero, tx=Zero,
					  m21=Zero, m22=Sy,   ty=Zero }.



% @doc Returns the (3x3) matrix corresponding to a rotation of the specified
% angle around the axis specified as a unit vector.
%
% This will be a counterclockwise rotation for an observer placed so that the
% specified axis points towards it.
%
% A rotation matrix is orthogonal, its inverse is its transpose, and its
% determinant is 1.0.
%
% These 3D rotation matrices form a group known as the special orthogonal group
% SO(3).
%
% See also: quaternion:rotation/2.
%
-spec rotation( unit_vector3(), radians() ) -> rot_matrix3().
rotation( UnitAxis=[ Ux, Uy, Uz ], RadAngle ) ->

	% Not an assertion, as UnitAxis must be ignored if no check is done:
	cond_utils:if_defined( myriad_check_linear,
						   true = vector3:is_unitary( UnitAxis ),
						   basic_utils:ignore_unused( UnitAxis ) ),

	% Source:
	% https://en.wikipedia.org/wiki/Rotation_matrix#Rotation_matrix_from_axis_and_angle

	% Also in matrix4:rotation/2.

	C = math:cos( RadAngle ),
	S = math:sin( RadAngle ),

	% One minus C:
	OmC = 1 - C,

	Uxy = Ux * Uy,
	Uxz = Ux * Uz,
	Uyz = Uy * Uz,

	M11 = C + Ux*Ux*OmC,
	M12 = Uxy*OmC - Uz*S,
	M13 = Uxz*OmC + Uy*S,

	M21 = Uxy*OmC + Uz*S,
	M22 = C + Uy*Uy*OmC,
	M23 = Uyz*OmC - Ux*S,

	M31 = Uxz*OmC - Uy*S,
	M32 = Uyz*OmC + Ux*S,
	M33 = C + Uz*Uz*OmC,

	#matrix3{ m11=M11, m12=M12, m13=M13,
			  m21=M21, m22=M22, m23=M23,
			  m31=M31, m32=M32, m33=M33 }.


% @doc Returns the 3x3 matrix whose columns correspond to the specified 3 3D
% vectors.
%
% Returns thus:
%  ```
%  [ Va Vb Vc ]
%  [ |  |  |  ]
%  [ |  |  |  ]
%  '''
%
-spec from_columns( vector3(), vector3(), vector3() ) -> canonical_matrix3().
from_columns( _Va=[Xa,Ya,Za], _Vb=[Xb,Yb,Zb], _Vc=[Xc,Yc,Zc] ) ->
	#matrix3{ m11=Xa, m12=Xb, m13=Xc,
			  m21=Ya, m22=Yb, m23=Yc,
			  m31=Za, m32=Zb, m33=Zc }.



% @doc Returns the 3x3 matrix whose rows correspond to the specified 3 3D
% vectors.
%
% Returns thus:
%  ```
% [ Va - - - ]
% [ Vb - - - ]
% [ Vc - - - ]
%  '''
%
-spec from_rows( vector3(), vector3(), vector3() ) -> canonical_matrix3().
from_rows( _Va=[Xa,Ya,Za], _Vb=[Xb,Yb,Zb], _Vc=[Xc,Yc,Zc] ) ->
	#matrix3{ m11=Xa, m12=Ya, m13=Za,
			  m21=Xb, m22=Yb, m23=Zb,
			  m31=Xc, m32=Yc, m33=Zc }.



% @doc Returns the (3x3, canonical) matrix whose (9) coordinates are the
% specified ones, as listed row after row.
%
-spec from_coordinates( coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate(),
						coordinate(), coordinate(), coordinate() ) ->
							canonical_matrix3().
from_coordinates( M11, M12, M13,
				  M21, M22, M23,
				  M31, M32, M33 ) ->
	#matrix3{ m11=M11, m12=M12, m13=M13,
			  m21=M21, m22=M22, m23=M23,
			  m31=M31, m32=M32, m33=M33 }.



% @doc Returns the "3x3" (actually 2x3) compact matrix whose 6 actual
% coordinates are the specified ones, as listed row after row.
%
-spec from_compact_coordinates(
					coordinate(), coordinate(), coordinate(),
					coordinate(), coordinate(), coordinate() ) ->
						compact_matrix3().
from_compact_coordinates( M11, M12, Tx,
						  M21, M22, Ty ) ->
	#compact_matrix3{ m11=M11, m12=M12, tx=Tx,
					  m21=M21, m22=M22, ty=Ty }.



% @doc Returns the 3x3 matrix corresponding to the specified
% arbitrary-dimensioned matrix.
%
-spec from_arbitrary( matrix() ) -> matrix3().
from_arbitrary( Matrix ) ->
	erlang:apply( fun from_rows/?dim, Matrix ).


% @doc Returns the arbitrary-dimensioned matrix corresponding to the specified
% 3x3 matrix.
%
-spec to_arbitrary( matrix3() ) -> matrix().
to_arbitrary( Matrix3 ) ->
	M = to_canonical( Matrix3 ),
	[ _RecordTag | Coords ] = tuple_to_list( M ),
	matrix:from_coordinates( Coords, _ColumCount=?dim ).



% @doc Returns the 3x3 compact matrix obtained from specified 2x2 matrix and
% 2D (translation) vector.
%
-spec from_2D( matrix2(), vector2() ) -> compact_matrix3().
from_2D( #matrix2{ m11=M11, m12=M12,
				   m21=M21, m22=M22 },
		 _Vec2=[ X, Y ] ) ->
	#compact_matrix3{ m11=M11, m12=M12, tx=X,
					  m21=M21, m22=M22, ty=Y };

from_2D( OtherMatrix2, Vec2 ) ->
	CanOtherMatrix2 = matrix2:to_canonical( OtherMatrix2 ),
	from_2D( CanOtherMatrix2, Vec2 ).



% @doc Returns the dimension of these matrices.
%
% Not useless, when using polymorphism based on module name.
%
-spec dimension() -> dimension().
dimension() ->
	?dim.



% @doc Returns the dimensions of these matrices.
%
% Not useless, when using polymorphism based on module name.
%
-spec dimensions() -> dimensions().
dimensions() ->
	{ ?dim, ?dim }.



% @doc Returns the specified row of the specified matrix.
-spec row( dimension(), matrix3() ) -> vector3().
row( _RowCount=1, #matrix3{ m11=M11, m12=M12, m13=M13 } ) ->
	[ M11, M12, M13 ];

row( _RowCount=2, #matrix3{ m21=M21, m22=M22, m23=M23 } ) ->
	[ M21, M22, M23 ];

row( _RowCount=3, #matrix3{ m31=M31, m32=M32, m33=M33 } ) ->
	[ M31, M32, M33 ];

row( RowCount, OtherMatrix ) ->
	row( RowCount, to_canonical( OtherMatrix ) ).



% @doc Returns the specified column of the specified matrix.
-spec column( dimension(), matrix3() ) -> vector3().
column( _ColumnCount=1, #matrix3{ m11=M11, m21=M21, m31=M31 } ) ->
	[ M11, M21, M31 ];

column( _ColumnCount=2, #matrix3{ m12=M12, m22=M22, m32=M32 } ) ->
	[ M12, M22, M32 ];

column( _ColumnCount=3, #matrix3{ m13=M13, m23=M23, m33=M33 } ) ->
	[ M13, M23, M33 ];

column( ColCount, OtherMatrix ) ->
	column( ColCount, to_canonical( OtherMatrix ) ).



% @doc Returns the element at specified row and column of the specified matrix.
-spec get_element( dimension(), dimension(), matrix3() ) -> coordinate().
get_element( _R=1, _C=1, #matrix3{ m11=M11 } ) ->
	M11;

get_element( _R=1, _C=2, #matrix3{ m12=M12 } ) ->
	M12;

get_element( _R=1, _C=3, #matrix3{ m13=M13 } ) ->
	M13;


get_element( _R=2, _C=1, #matrix3{ m21=M21 } ) ->
	M21;

get_element( _R=2, _C=2, #matrix3{ m22=M22 } ) ->
	M22;

get_element( _R=2, _C=3, #matrix3{ m23=M23 } ) ->
	M23;


get_element( _R=3, _C=1, #matrix3{ m31=M31 } ) ->
	M31;

get_element( _R=3, _C=2, #matrix3{ m32=M32 } ) ->
	M32;

get_element( _R=3, _C=3, #matrix3{ m33=M33 } ) ->
	M33;


get_element( R, C, OtherMatrix ) ->
	get_element( R, C, to_canonical( OtherMatrix ) ).



% @doc Returns a matrix identical to the specified one except that its specified
% element at specified location has been set to the specified value.
%
-spec set_element( dimension(), dimension(), coordinate(), matrix3() ) ->
									matrix3().
set_element( _R=1, _C=1, Value, Matrix=#matrix3{} ) ->
	Matrix#matrix3{ m11=Value };

set_element( _R=1, _C=2, Value, Matrix=#matrix3{} ) ->
	Matrix#matrix3{ m12=Value };

set_element( _R=1, _C=3, Value, Matrix=#matrix3{} ) ->
	Matrix#matrix3{ m13=Value };


set_element( _R=2, _C=1, Value, Matrix=#matrix3{} ) ->
	Matrix#matrix3{ m21=Value };

set_element( _R=2, _C=2, Value, Matrix=#matrix3{} ) ->
	Matrix#matrix3{ m22=Value };

set_element( _R=2, _C=3, Value, Matrix=#matrix3{} ) ->
	Matrix#matrix3{ m23=Value };


set_element( _R=3, _C=1, Value, Matrix=#matrix3{} ) ->
	Matrix#matrix3{ m31=Value };

set_element( _R=3, _C=2, Value, Matrix=#matrix3{} ) ->
	Matrix#matrix3{ m32=Value };

set_element( _R=3, _C=3, Value, Matrix=#matrix3{} ) ->
	Matrix#matrix3{ m33=Value };


set_element( R, C, Value, OtherMatrix ) ->
	set_element( R, C, Value, to_canonical( OtherMatrix ) ).



% @doc Returns the transpose of the specified matrix.
-spec transpose( matrix3() ) -> matrix3().
% Diagonal untouched:
transpose( M=#matrix3{ m12=M12, m13=M13,
					   m21=M21, m23=M23,
					   m31=M31, m32=M32 } ) ->
	M#matrix3{ m12=M21, m13=M31,
			   m21=M12, m23=M32,
			   m31=M13, m32=M23 };

transpose( M=identity_3 ) ->
	M;

% A compact matrix is not anymore compact:
transpose( CompactMatrix ) ->
	transpose( to_canonical( CompactMatrix ) ).



% @doc Scales specified (3D) matrix of the specified factor.
-spec scale( matrix3(), factor() ) -> matrix3().
scale( #compact_matrix3{ m11=M11, m12=M12, tx=Tx,
						 m21=M21, m22=M22, ty=Ty }, Factor ) ->
	% Not anymore a compact matrix, as last (bottom-right) implicit coordinate
	% (1.0) must be scaled as well:
	%
	#matrix3{
		 m11=Factor*M11, m12=Factor*M12, m13=Factor*Tx,
		 m21=Factor*M21, m22=Factor*M22, m23=Factor*Ty,
		 m31=0.0,        m32=0.0,        m33=Factor     };

scale( #matrix3{ m11=M11, m12=M12, m13=M13,
				 m21=M21, m22=M22, m23=M23,
				 m31=M31, m32=M32, m33=M33 }, Factor ) ->
	#matrix3{ m11=Factor*M11, m12=Factor*M12, m13=Factor*M13,
			  m21=Factor*M21, m22=Factor*M22, m23=Factor*M23,
			  m31=Factor*M31, m32=Factor*M32, m33=Factor*M33 };

scale( M=identity_3, Factor ) ->
	scale( to_canonical( M ), Factor ).



% @doc Returns the sum of the two specified matrices: M = Ma + Mb.
-spec add( matrix3(), matrix3() ) -> matrix3().
add( _Ma=#matrix3{ m11=A11, m12=A12, m13=A13,
				   m21=A21, m22=A22, m23=A23,
				   m31=A31, m32=A32, m33=A33 },
	 _Mb=#matrix3{ m11=B11, m12=B12, m13=B13,
				   m21=B21, m22=B22, m23=B23,
				   m31=B31, m32=B32, m33=B33 } ) ->

	#matrix3{ m11=A11+B11, m12=A12+B12, m13=A13+B13,
			  m21=A21+B21, m22=A22+B22, m23=A23+B23,
			  m31=A31+B31, m32=A32+B32, m33=A33+B33 };


add( _Ma=#compact_matrix3{ m11=A11, m12=A12, tx=Tx,
						   m21=A21, m22=A22, ty=Ty },
	 _Mb=#matrix3{ m11=B11, m12=B12, m13=B13,
				   m21=B21, m22=B22, m23=B23,
				   m31=B31, m32=B32, m33=B33 } ) ->
	#matrix3{ m11=A11+B11, m12=A12+B12, m13=Tx+B13,
			  m21=A21+B21, m22=A22+B22, m23=Ty+B23,
			  m31=B31,     m32=B32,     m33=1.0+B33 };


add( Ma=#matrix3{}, Mb=#compact_matrix3{} ) ->
	add( Mb, Ma );


% Preserve compactness:
add( _Ma=#compact_matrix3{ m11=A11, m12=A12, tx=Ax,
						   m21=A21, m22=A22, ty=Ay },
	 _Mb=#compact_matrix3{ m11=B11, m12=B12, tx=Bx,
						   m21=B21, m22=B22, ty=By } ) ->

	#compact_matrix3{ m11=A11+B11, m12=A12+B12, tx=Ax+Bx,
					  m21=A21+B21, m22=A22+B22, ty=Ay+By };

add( Ma, Mb ) ->
	add( to_canonical( Ma ), to_canonical( Mb ) ).



% @doc Returns the subtraction of the two specified matrices: M = Ma - Mb.
-spec sub( matrix3(), matrix3() ) -> matrix3().
%sub( Ma, Mb ) ->
	%MinusMb = scale( Mb, -1.0 ),
	%add( Ma, MinusMb ).
sub( _Ma=#matrix3{ m11=A11, m12=A12, m13=A13,
				   m21=A21, m22=A22, m23=A23,
				   m31=A31, m32=A32, m33=A33 },
	 _Mb=#matrix3{ m11=B11, m12=B12, m13=B13,
				   m21=B21, m22=B22, m23=B23,
				   m31=B31, m32=B32, m33=B33 } ) ->

	#matrix3{ m11=A11-B11, m12=A12-B12, m13=A13-B13,
			  m21=A21-B21, m22=A22-B22, m23=A23-B23,
			  m31=A31-B31, m32=A32-B32, m33=A33-B33 };


sub( _Ma=#compact_matrix3{ m11=A11, m12=A12, tx=Ax,
						   m21=A21, m22=A22, ty=Ay },
	 _Mb=#matrix3{ m11=B11, m12=B12, m13=B13,
				   m21=B21, m22=B22, m23=B23,
				   m31=B31, m32=B32, m33=B33 } ) ->
	#matrix3{ m11=A11-B11, m12=A12-B12, m13=Ax-B13,
			  m21=A21-B21, m22=A22-B22, m23=Ay-B23,
			  m31=-B31, m32=-B32, m33=1.0-B33 };


sub( _Ma=#matrix3{ m11=A11, m12=A12, m13=A13,
				   m21=A21, m22=A22, m23=A23,
				   m31=A31, m32=A32, m33=A33 },
	 _Mb=#compact_matrix3{ m11=B11, m12=B12, tx=Bx,
						   m21=B21, m22=B22, ty=By } ) ->
	#matrix3{ m11=A11-B11, m12=A12-B12, m13=A13-Bx,
			  m21=A21-B21, m22=A22-B22, m23=A23-By,
			  m31=A31,     m32=A32,     m33=A33-1.0 };


% Preserve compactness:
sub( _Ma=#compact_matrix3{ m11=A11, m12=A12, tx=Ax,
						   m21=A21, m22=A22, ty=Ay },
	 _Mb=#compact_matrix3{ m11=B11, m12=B12, tx=Bx,
						   m21=B21, m22=B22, ty=By } ) ->

	#compact_matrix3{ m11=A11-B11, m12=A12-B12, tx=Ax-Bx,
					  m21=A21-B21, m22=A22-B22, ty=Ay-By };


% At least one identity:
sub( Ma, Mb ) ->
	sub( to_canonical( Ma ), to_canonical( Mb ) ).



% @doc Multiplies the first matrix by the second one: returns Mc = Ma.Mb.
-spec mult( Ma:: matrix3(), Mb :: matrix3() ) -> matrix3().
mult( _Ma=identity_3, Mb ) ->
	Mb;

mult( Ma, _Mb=identity_3 ) ->
	Ma;

mult( _Ma=#matrix3{ m11=A11, m12=A12, m13=A13,
					m21=A21, m22=A22, m23=A23,
					m31=A31, m32=A32, m33=A33 },
	  _Mb=#matrix3{ m11=B11, m12=B12, m13=B13,
					m21=B21, m22=B22, m23=B23,
					m31=B31, m32=B32, m33=B33 } ) ->

	C11 = A11*B11 + A12*B21 + A13*B31,
	C12 = A11*B12 + A12*B22 + A13*B32,
	C13 = A11*B13 + A12*B23 + A13*B33,

	C21 = A21*B11 + A22*B21 + A23*B31,
	C22 = A21*B12 + A22*B22 + A23*B32,
	C23 = A21*B13 + A22*B23 + A23*B33,

	C31 = A31*B11 + A32*B21 + A33*B31,
	C32 = A31*B12 + A32*B22 + A33*B32,
	C33 = A31*B13 + A32*B23 + A33*B33,

	#matrix3{ m11=C11, m12=C12, m13=C13,
			  m21=C21, m22=C22, m23=C23,
			  m31=C31, m32=C32, m33=C33 };

mult( _Ma=#compact_matrix3{ m11=A11, m12=A12, tx=Tx,
							m21=A21, m22=A22, ty=Ty },
	  _Mb=#matrix3{ m11=B11, m12=B12, m13=B13,
					m21=B21, m22=B22, m23=B23,
					m31=B31, m32=B32, m33=B33 } ) ->

	C11 = A11*B11 + A12*B21 + Tx*B31,
	C12 = A11*B12 + A12*B22 + Tx*B32,
	C13 = A11*B13 + A12*B23 + Tx*B33,

	C21 = A21*B11 + A22*B21 + Ty*B31,
	C22 = A21*B12 + A22*B22 + Ty*B32,
	C23 = A21*B13 + A22*B23 + Ty*B33,

	C31 = B31,
	C32 = B32,
	C33 = B33,

	#matrix3{ m11=C11, m12=C12, m13=C13,
			  m21=C21, m22=C22, m23=C23,
			  m31=C31, m32=C32, m33=C33 };

mult( _Ma=#matrix3{ m11=A11, m12=A12, m13=A13,
					m21=A21, m22=A22, m23=A23,
					m31=A31, m32=A32, m33=A33 },
	  _Mb=#compact_matrix3{ m11=B11, m12=B12, tx=Tx,
							m21=B21, m22=B22, ty=Ty } ) ->

	C11 = A11*B11 + A12*B21,
	C12 = A11*B12 + A12*B22,
	C13 = A11*Tx + A12*Ty + A13,

	C21 = A21*B11 + A22*B21,
	C22 = A21*B12 + A22*B22,
	C23 = A21*Tx + A22*Ty + A23,

	C31 = A31*B11 + A32*B21,
	C32 = A31*B12 + A32*B22,
	C33 = A31*Tx + A32*Ty + A33,

	#matrix3{ m11=C11, m12=C12, m13=C13,
			  m21=C21, m22=C22, m23=C23,
			  m31=C31, m32=C32, m33=C33 };

mult( _Ma=#compact_matrix3{ m11=A11, m12=A12, tx=Ax,
							m21=A21, m22=A22, ty=Ay },
	  _Mb=#compact_matrix3{ m11=B11, m12=B12, tx=Bx,
							m21=B21, m22=B22, ty=By } ) ->

	C11 = A11*B11 + A12*B21,
	C12 = A11*B12 + A12*B22,
	Cx  = A11*Bx  + A12*By + Ax,

	C21 = A21*B11 + A22*B21,
	C22 = A21*B12 + A22*B22,
	Cy  = A21*Bx  + A22*By + Ay,

	% C3{1,2} are 0.0, C33 is 1.0.

	#compact_matrix3{ m11=C11, m12=C12, tx=Cx,
					  m21=C21, m22=C22, ty=Cy }.



% @doc Multiplies (in-order) the specified matrices.
%
% Ex: mult([Ma, Mb, Mc]) = mult(mult(Ma,Mb),Mc) = Ma.Mb.Mc
%
-spec mult( [ matrix3() ] ) -> matrix3().
mult( [ Ma, Mb | T ] ) ->
	mult( [ mult( Ma, Mb ) | T ] );

mult( [ M ] ) ->
	M.



% @doc Applies (on the right) the specified 2D or 3D vector V or point P to the
% specified matrix M: returns M.V.
%
% If the specified vector is a 2D one (i.e. not a 3D one), we assume that its
% third (Vz) coordinate is 0.0, whereas if the specified point is a 2D one
% (i.e. not a 3D one), we assume that its third (Pz) coordinate is 1.0, and
% returns a 2D point whose coordinates have been normalised regarding the Z
% coordinate resulting from the application of that extended point.
%
% Not a clause of mult/2 for an increased clarity.
%
-spec apply( matrix3(), vector2() ) -> vector2();
		   ( matrix3(), vector3() ) -> vector3();
		   ( matrix3(), point2() ) -> point2();
		   ( matrix3(), point3() ) -> point3().
apply( _M=identity_3, VorP ) ->
	VorP;

% A nice feature is that the actual, lowest-level types of vectors and points
% are different (list vs tuple) and thus can be discriminated.
%
% First with a vector2 (implicitly Vz is 0.0):
apply( _M=#matrix3{ m11=M11,  m12=M12,  m13=_M13,
					m21=M21,  m22=M22,  m23=_M23,
					m31=_M31, m32=_M32, m33=_M33 },
	   _V=[ Vx, Vy ] ) ->

	%Vz = 0.0,
	ResX = M11*Vx + M12*Vy,
	ResY = M21*Vx + M22*Vy,
	%ResZ = M31*Vx + M32*Vy,

	[ ResX, ResY ];


apply( _M=#compact_matrix3{ m11=M11, m12=M12, tx=_Tx,
							m21=M21, m22=M22, ty=_Ty },
	   _V=[ Vx, Vy ] ) ->
	%Vz = 0.0,
	ResX = M11*Vx + M12*Vy,
	ResY = M21*Vx + M22*Vy,
	% Here ResZ = Vz = 0.0,

	[ ResX, ResY ];


% Then with a point2 (implicitly Pz is 1.0):
apply( _M=#matrix3{ m11=M11, m12=M12, m13=M13,
					m21=M21, m22=M22, m23=M23,
					m31=M31, m32=M32, m33=M33 },
	   _P={ Px, Py } ) ->

	%Pz = 1.0,
	ResX = M11*Px + M12*Py + M13,
	ResY = M21*Px + M22*Py + M23,
	ResZ = M31*Px + M32*Py + M33,

	% A point shall be normalised:
	case math_utils:is_null( ResZ ) of

		true ->
			throw( null_z_coordinate );

		false ->
			{ ResX/ResZ, ResY/ResZ }

	end;


apply( _M=#compact_matrix3{ m11=M11, m12=M12, tx=Tx,
							m21=M21, m22=M22, ty=Ty },
	   _P={ Px, Py } ) ->

	%Pz = 1.0,
	ResX = M11*Px + M12*Py + Tx,
	ResY = M21*Px + M22*Py + Ty,
	% Here ResZ = Pz = 1.0,

	% Already normalised:
	{ ResX, ResY };


% Then with a vector3:
apply( _M=#matrix3{ m11=M11, m12=M12, m13=M13,
					m21=M21, m22=M22, m23=M23,
					m31=M31, m32=M32, m33=M33 },
	   _V=[ Vx, Vy, Vz ] ) ->

	ResX = M11*Vx + M12*Vy + M13*Vz,
	ResY = M21*Vx + M22*Vy + M23*Vz,
	ResZ = M31*Vx + M32*Vy + M33*Vz,

	[ ResX, ResY, ResZ ];


apply( _M=#compact_matrix3{ m11=M11, m12=M12, tx=Tx,
							m21=M21, m22=M22, ty=Ty },
	   _V=[ Vx, Vy, Vz ] ) ->
	ResX = M11*Vx + M12*Vy + Tx*Vz,
	ResY = M21*Vx + M22*Vy + Ty*Vz,
	ResZ = Vz,

	[ ResX, ResY, ResZ ];


% Finally with a point3 (mostly the same as for vector3):
apply( _M=#matrix3{ m11=M11, m12=M12, m13=M13,
					m21=M21, m22=M22, m23=M23,
					m31=M31, m32=M32, m33=M33 },
	   _P={ Px, Py, Pz } ) ->

	ResX = M11*Px + M12*Py + M13*Pz,
	ResY = M21*Px + M22*Py + M23*Pz,
	ResZ = M31*Px + M32*Py + M33*Pz,

	{ ResX, ResY, ResZ };


apply( _M=#compact_matrix3{ m11=M11, m12=M12, tx=Tx,
							m21=M21, m22=M22, ty=Ty },
	   _P={ Px, Py, Pz } ) ->
	ResX = M11*Px + M12*Py + Tx*Pz,
	ResY = M21*Px + M22*Py + Ty*Pz,
	ResZ = Pz,

	{ ResX, ResY, ResZ }.



% @doc Tells whether the two specified (3x3) matrices are equal.
-spec are_equal( matrix3(), matrix3() ) -> boolean().
are_equal( _Ma=#matrix3{ m11=A11, m12=A12, m13=A13,
						 m21=A21, m22=A22, m23=A23,
						 m31=A31, m32=A32, m33=A33 },
		   _Mb=#matrix3{ m11=B11, m12=B12, m13=B13,
						 m21=B21, m22=B22, m23=B23,
						 m31=B31, m32=B32, m33=B33 } ) ->
	are_close( A11, B11 ) andalso are_close( A12, B12 )
		andalso are_close( A13, B13 ) andalso are_close( A21, B21 )
		andalso are_close( A22, B22 ) andalso are_close( A23, B23 )
		andalso are_close( A31, B31 ) andalso are_close( A32, B32 )
		andalso are_close( A33, B33 );

are_equal( _Ma=#compact_matrix3{ m11=A11, m12=A12, tx=Ax,
								 m21=A21, m22=A22, ty=Ay },
		   _Mb=#compact_matrix3{ m11=B11, m12=B12, tx=Bx,
								 m21=B21, m22=B22, ty=By } ) ->
	are_close( A11, B11 ) andalso are_close( A12, B12 )
		andalso are_close( Ax, Bx ) andalso are_close( A21, B21 )
		andalso are_close( A22, B22 ) andalso are_close( Ay, By );

are_equal( _Ma=#matrix3{ m11=A11, m12=A12, m13=A13,
						 m21=A21, m22=A22, m23=A23,
						 m31=A31, m32=A32, m33=A33 },
		   _Mb=#compact_matrix3{ m11=B11, m12=B12, tx=Bx,
								 m21=B21, m22=B22, ty=By } ) ->
	are_close( A11, B11 ) andalso are_close( A12, B12 )
		andalso are_close( A13, Bx )
		andalso are_close( A21, B21 ) andalso are_close( A22, B22 )
		andalso are_close( A23, By )
		andalso is_null( A31 ) andalso is_null( A32 )
		andalso are_close( A33, 1.0 );

are_equal( Ma=#compact_matrix3{}, Mb=#matrix3{} ) ->
	are_equal( Mb, Ma );

are_equal( _Ma=identity_3, _Mb=identity_3 ) ->
	true;

are_equal( Ma, Mb=identity_3 ) ->
	are_equal( Ma, to_canonical( Mb ) );

are_equal( Ma=identity_3, Mb ) ->
	are_equal( Mb, Ma ).



% @doc Returns the determinant of the specified matrix.
-spec determinant( matrix3() ) -> scalar().
determinant( _M=#matrix3{ m11=M11, m12=M12, m13=M13,
						  m21=M21, m22=M22, m23=M23,
						  m31=M31, m32=M32, m33=M33 } ) ->
	M11*M22*M33 + M12*M23*M31 + M13*M21*M32
		- M13*M22*M31 - M12*M21*M33 - M11*M23*M32;

determinant( _M=#compact_matrix3{ m11=M11, m12=M12, tx=_Tx,
								  m21=M21, m22=M22, ty=_Ty } )->
	% Thanks to the mostly-zero last row, a 2x2 determinant:
	M11*M22 - M12*M21;

determinant( _M=identity_3 ) ->
	1.0.



% @doc Returns the comatrix of the specified matrix (that is the matrix of its
% cofactors).
%
-spec comatrix( matrix3() ) -> matrix3().
comatrix( identity_3 ) ->
	identity_3;

comatrix( _M=#matrix3{ m11=M11, m12=M12, m13=M13,
					   m21=M21, m22=M22, m23=M23,
					   m31=M31, m32=M32, m33=M33 } ) ->

	CM11 = M22*M33 - M23*M32,

	% - ( M21*M33 - M23*M31 )
	CM12 = M23*M31 - M21*M33,

	CM13 = M21*M32 - M22*M31,

	% - ( M12*M33 - M13*M32 )
	CM21 = M13*M32 - M12*M33,

	CM22 = M11*M33 - M13*M31,

	% - (M11*M32 - M12*M31 )
	CM23 = M12*M31 - M11*M32,

	CM31 = M12*M23 - M13*M22,

	% - (M11*M23 - M13*M21 )
	CM32 = M13*M21 - M11*M23,

	CM33 = M11*M22 - M12*M21,

	#matrix3{ m11=CM11, m12=CM12, m13=CM13,
			  m21=CM21, m22=CM22, m23=CM23,
			  m31=CM31, m32=CM32, m33=CM33 };

comatrix( _M=#compact_matrix3{ m11=M11, m12=M12, tx=Tx,
							   m21=M21, m22=M22, ty=Ty } ) ->

	% Large simplification; yet not even the transpose of a compact matrix, as
	% the last coordinate (M33) is not necessarily 1.0 (being the determinant):

	CM11 = M22,

	CM12 = -M21,

	CM13 = 0.0,

	CM21 = -M12,

	CM22 = M11,

	CM23 = 0.0,

	CM31 = M12*Ty - Tx*M22,

	CM32 = Tx*M21 - M11*Ty,

	% Not necessarily 1.0:
	CM33 = M11*M22 - M12*M21,

	#matrix3{ m11=CM11, m12=CM12, m13=CM13,
			  m21=CM21, m22=CM22, m23=CM23,
			  m31=CM31, m32=CM32, m33=CM33 }.



% @doc Returns the inverse of the specified matrix, if it is invertible (that is
% iff its determinant is non-null), otherwise returns undefined.
%
-spec inverse( matrix3() ) -> maybe( matrix3() ).
inverse( M ) when is_record( M, matrix3 ) ->
	Det = determinant( M ),
	case math_utils:is_null( Det ) of

		true ->
			undefined;

		false ->
			scale( transpose( comatrix( M ) ), 1/Det )

	end;

% Special-cased as performs less operations, and returns a compact form:
inverse( M ) when is_record( M, compact_matrix3 ) ->
	Det = determinant( M ),
	case math_utils:is_null( Det ) of

		true ->
			undefined;

		false ->
			% We take advantage of the fact that the comatrix of a compact
			% matrix requires less computations, and/but it returns a canonical
			% matrix:
			%
			InvCan = scale( transpose( comatrix( M ) ), 1/Det ),

			% As expected to be ultimately a compact matrix:
			% (to_compact/1 must come last)
			%
			to_compact( InvCan )

	end.



% @doc Returns the canonical form of the specified 3x3 matrix.
-spec to_canonical( matrix3() ) -> canonical_matrix3().
to_canonical( #compact_matrix3{ m11=M11, m12=M12, tx=Tx,
								m21=M21, m22=M22, ty=Ty } ) ->
	Zero = 0.0,
	#matrix3{ m11=M11,  m12=M12,  m13=Tx,
			  m21=M21,  m22=M22,  m23=Ty,
			  m31=Zero, m32=Zero, m33=1.0 };

to_canonical( identity_3 ) ->
	Zero = 0.0,
	One = 1.0,
	#matrix3{ m11=One,  m12=Zero, m13=Zero,
			  m21=Zero, m22=One,  m23=Zero,
			  m31=Zero, m32=Zero, m33=One };

to_canonical( M ) when is_record( M, matrix3 ) ->
	M.



% @doc Returns the compact form of specified (3x3) matrix.
%
% Throws an exception if the specified matrix cannot be expressed as a compact
% one.
%
-spec to_compact( matrix3() ) -> compact_matrix3().
to_compact( M=#matrix3{ m11=M11, m12=M12, m13=M13,
						m21=M21, m22=M22, m23=M23,
						m31=M31, m32=M32, m33=M33 } ) ->
	case is_null( M31 ) andalso is_null( M32 ) andalso are_close( M33, 1.0 ) of

		true ->
			% Just drop the last row then:
			#compact_matrix3{ m11=M11, m12=M12, tx=M13,
							  m21=M21, m22=M22, ty=M23 };

		false ->
			trace_utils:error_fmt( "Canonical 3D matrix ~ts cannot be "
				"expressed as a compact one.", [ to_string( M ) ] ),

			throw( { not_compactable, M } )

	end;

to_compact( identity_3 ) ->
	Zero = 0.0,
	One = 1.0,
	#compact_matrix3{ m11=One,  m12=Zero, tx=Zero,
					  m21=Zero, m22=One,  ty=Zero };

to_compact( CM ) when is_record( CM, compact_matrix3 ) ->
	CM.



% @doc Checks that the specified matrix is legit, and returns it.
-spec check( matrix3() ) -> matrix3().
check( M=identity_3 ) ->
	M;

check( M ) ->
	Coords = case tuple_to_list( M ) of

		[ matrix3 | Cs ] ->
			Cs;

		[ compact_matrix3 | Cs ] ->
			Cs

	end,

	[ type_utils:check_float( C ) || C <- Coords ],

	M.



% @doc Returns a textual representation of the specified (3x3) matrix.
-spec to_string( matrix3() ) -> ustring().
to_string( _Matrix=identity_3 ) ->
	"3x3 identity";

to_string( Matrix3=#matrix3{} ) ->

	[ _AtomMatrix3 | AllCoords ] = tuple_to_list( Matrix3 ),

	Strs = linear:coords_to_best_width_strings( AllCoords ),

	% No need for ~ts here:
	RowFormatStr = "[ " ++ text_utils:duplicate( ?dim, "~s " ) ++ "]~n",

	FormatStr = "~n" ++ text_utils:duplicate( ?dim, RowFormatStr ),

	text_utils:format( FormatStr, Strs );


to_string( CptMatrix=#compact_matrix3{} ) ->

	[ _AtomCompactMatrix3 | CompactCoords ] = tuple_to_list( CptMatrix ),

	AllCoords = CompactCoords ++ [ 0.0, 0.0, 1.0 ],

	Strs = linear:coords_to_best_width_strings( AllCoords ),

	% No need for ~ts here:
	RowFormatStr = "[ " ++ text_utils:duplicate( ?dim, "~s " ) ++ "]~n",

	FormatStr = "~n" ++ text_utils:duplicate( ?dim, RowFormatStr ),

	text_utils:format( FormatStr, Strs ).
