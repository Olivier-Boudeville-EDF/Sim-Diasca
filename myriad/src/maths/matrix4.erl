% Copyright (C) 2021-2026 Olivier Boudeville
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

-module(matrix4).

-moduledoc """
Module implementing the support for **4x4 matrices**.

See also:
 - the corresponding (4D) vectors, in `vector4.erl`
 - the (unspecialised) matrices of arbitrary dimensions, in `matrix.erl`
""".



% Implementation notes:
%
% These 4x4 matrices come in various forms:
%
% - the canonical one (4x4 coordinates)
%
% - the compact one (3x4) where the last row is (implicitly) [0.0, 0.0, 0.0,
% 1.0], typical of 3D transformations (translation+rotation+scaling)
%
% - special ones, at least the identity matrix


% We assume here a right-handed coordinate system, row-major order and matrix
% multiplication on the right (post-multiplication).
%
% As a result, matrices here are the transposed versions of GLM or e3d ones.

% Potential sources of inspiration:
% - glm: glm/ext/matrix_clip_space.inl
% - Wings3D: e3d/e3d_transform.erl for project, unproject, lookat, pick


% We define translate_homogeneous/2, rotate_homogeneous/3, scale_homogeneous/2
% that operate on an homogeneous matrix and return an updated version
% thereof. Note though that updating repeatedly a matrix (e.g. by applying it a
% new rotation each frame) is prone to the accumulation of rounding errors. A
% better practice is to each time recompute the matrix from scratch from
% higher-level parameters, which can then be safely updated.

% Note that translations are commutative, but not rotations (R1.R2 is in general
% different from R2.R1).

% Shearing (offset of one component with respect to another component) could be
% implemented. The shearing of a non-null factor s of a dimension Dx relatively
% to a dimension Dy corresponds to adding to the identity matrix an
% extradiagonal element at position (x,y) of value s.
%
% The inverse of Sxy(s) is Sxy(-s).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For records like matrix3:
-include("matrix3.hrl").

% For records like matrix4:
-include("matrix4.hrl").



-doc """
A matrix4 can be specified as a list of same-size rows (akin to a user arbitrary
matrix) containing any kind of numerical coordinates.
""".
-type user_matrix4() :: user_matrix().



-doc "A 4x4 matrix.".
-type matrix4() :: 'identity_4' | canonical_matrix4() | compact_matrix4().



-doc "Alias for 4x4 canonical matrices.".
-type canonical_matrix4() :: #matrix4{}.
% Alias for 4x4 canonical matrices.



-doc "Alias for 4x4 compact matrices.".
-type compact_matrix4() :: #compact_matrix4{}.
% Alias for 4x4 compact matrices.



-doc "A matrix describing a 4D rotation.".
-type rot_matrix4() :: canonical_matrix4().



-doc """
A matrix4 that corresponds to an homogeneous one.

So we do not expect canonical matrices here.

A notation [R|T] designates for these matrices for R a 3x3 (possibly rotation)
matrix, and for T its right-side corresponding 3D (translation) vector.
""".
-type homogeneous_matrix4() :: 'identity_4' | compact_matrix4().



-doc """
An homogeneous matrix4 that corresponds to a transition from a 3D coordinate
system to another.

It can be noted TM = [R|T] where R is a 3x3 rotation matrix and T its associated
translation vector.

It can be represented as an orthogonal compact matrix, made of the following
four (3D) rows, in that order: I, J, K and O, for the three unit axes and the
origin.
""".
-type transition_matrix4() :: homogeneous_matrix4().



-doc "A tuple of 12 or 16 coordinates.".
-type tuple_matrix4() :: % Not exported yet: gl:m12() | gl:m16().
                         type_utils:tuple( coordinate(), 12 )
                       | type_utils:tuple( coordinate(), 16 ).



-doc "Scale factors for 3D content.".
-type scale_factors() :: { scale_factor(), scale_factor(), scale_factor() }.


-export_type([ user_matrix4/0, matrix4/0, canonical_matrix4/0,
               compact_matrix4/0, rot_matrix4/0, homogeneous_matrix4/0,
               transition_matrix4/0, tuple_matrix4/0,
               scale_factors/0 ]).



-export([ new/1, new/3, null/0, identity/0, translation/1, scaling/1,
          rotation/2, transition/4,
          from_columns/4, from_rows/4,
          compact_from_columns/4,
          from_coordinates/16, from_compact_coordinates/12,
          from_arbitrary/1, to_arbitrary/1,
          dimension/0, dimensions/0,
          row/2, column/2,
          compact_column/2,

          get_column_i/1, get_column_j/1, get_column_k/1, get_column_o/1,
          get_translation/1,

          set_column_i/2, set_column_j/2, set_column_k/2, set_column_o/2,

          get_element/3, set_element/4,
          transpose/1,
          scale/2,
          add/2, sub/2, mult/2, mult/1,
          apply/2, apply_homogeneous_left/2, apply_homogeneous_right/2,
          are_equal/2,
          translate_homogeneous_left/2, translate_homogeneous_right/2,
          rotate_homogeneous_left/3, rotate_homogeneous_right/3,

          scale_homogeneous/2,

          scale_homogeneous_left/2, scale_homogeneous_right/2,

          scale_homogeneous_x/2, scale_homogeneous_y/2, scale_homogeneous_z/2,

          scale_homogeneous_x_t/2, scale_homogeneous_y_t/2,
          scale_homogeneous_z_t/2,

          determinant/1, comatrix/1, inverse/1,
          to_canonical/1, to_compact/1,
          from_tuple/1, to_tuple/1,
          check/1,
          to_string/1 ] ).


% For homogeneous matrices:
-export([ from_3D/2, to_3D/1 ]).




-import( math_utils, [ is_null/1, are_close/2 ] ).

% To avoid clash with BIF:
-compile( { no_auto_import, [ apply/2 ] } ).

-define( dim, 4 ).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type scale_factor() :: math_utils:scale_factor().

-type radians() :: unit_utils:radians().

-type coordinate() :: linear:coordinate().
-type dimension() :: linear:dimension().
-type scalar() :: linear:scalar().

-type point3() :: point3:point3().
-type unit_vector3() :: vector3:unit_vector3().
-type vector3() :: vector3:vector3().


-type user_vector4() :: vector4:user_vector4().
-type vector4() :: vector4:vector4().
-type point4() :: point4:point4().

-type dimensions() :: matrix:dimensions().

-type matrix3() :: matrix3:matrix3().

-type user_matrix() :: matrix:user_matrix().
-type matrix() :: matrix:matrix().



-doc """
Returns a 4D (canonical) matrix corresponding to the user-specified matrix.
""".
-spec new( user_matrix4() ) -> canonical_matrix4().
new( UserMatrix ) ->

    CoordList = [ type_utils:ensure_float( C )
                    || C <- list_utils:flatten_once( UserMatrix ) ],

    % Returns a #matrix4 record (i.e. a tagged tuple):
    list_to_tuple( [ 'matrix4' | CoordList ] ).



-doc """
Returns a 4D compact matrix corresponding to the user-specified row vectors.
""".
-spec new( user_vector4(), user_vector4(), user_vector4() ) ->
                                            compact_matrix4().
new( UserVecRow1, UserVecRow2, UserVecRow3 ) ->

    Rows = [ vector4:new( UVR )
                || UVR <- [ UserVecRow1, UserVecRow2, UserVecRow3 ] ],

    CoordList = list_utils:flatten_once( Rows ),

    % Returns a #compact_matrix4 record (i.e. a tagged tuple):
    list_to_tuple( [ 'compact_matrix4' | CoordList ] ).



-doc "Returns the null (4x4) matrix.".
-spec null() -> canonical_matrix4().
null() ->
    Zero = 0.0,
    CoordList = lists:duplicate( _N=?dim * ?dim, Zero ),
    list_to_tuple( [ 'matrix4' | CoordList ] ).



-doc "Returns the identity (4x4) matrix.".
-spec identity() -> matrix4().
identity() ->
    identity_4.



-doc """
Returns the (4x4) homogeneous (thus compact) matrix corresponding to a
translation of the specified vector.
""".
-spec translation( vector3() ) -> compact_matrix4().
translation( _VT=[ Tx, Ty, Tz ] ) ->
    Zero = 0.0,
    One = 1.0,
    #compact_matrix4{ m11=One,  m12=Zero, m13=Zero, tx=Tx,
                      m21=Zero, m22=One,  m23=Zero, ty=Ty,
                      m31=Zero, m32=Zero, m33=One,  tz=Tz }.



-doc """
Returns the (4x4) homogeneous (thus compact) matrix corresponding to the scaling
of the specified factors.
""".
-spec scaling( scale_factors() ) -> compact_matrix4();
             ( vector3() ) -> compact_matrix4().
scaling( { Sx, Sy, Sz } ) ->
    Zero = 0.0,
    #compact_matrix4{ m11=Sx,   m12=Zero, m13=Zero, tx=Zero,
                      m21=Zero, m22=Sy,   m23=Zero, ty=Zero,
                      m31=Zero, m32=Zero, m33=Sz,   tz=Zero };

scaling( [ Sx, Sy, Sz ] ) ->
    Zero = 0.0,
    #compact_matrix4{ m11=Sx,   m12=Zero, m13=Zero, tx=Zero,
                      m21=Zero, m22=Sy,   m23=Zero, ty=Zero,
                      m31=Zero, m32=Zero, m33=Sz,   tz=Zero }.



-doc """
Returns the (4x4) homogeneous (thus compact) matrix corresponding to a rotation
of the specified angle around the 3D axis specified as a unit vector, and to no
translation.

This will be a counterclockwise rotation for an observer placed so that the
specified axis points towards it.

Note that this is not the general case of a rotation in 4D (which is of little
use, at least here), but this corresponds to (4x4) homogeneous matrices.
""".
-spec rotation( unit_vector3(), radians() ) -> compact_matrix4().
rotation( UnitAxis=[ Ux, Uy, Uz ], RadAngle ) ->

    % Not an assertion, as UnitAxis must be ignored if no check is done:
    cond_utils:if_defined( myriad_check_linear,
                           true = vector3:is_unitary( UnitAxis ),
                           basic_utils:ignore_unused( UnitAxis ) ),

    % Directly taken from matrix3:rotation/2.

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

    Zero = 0.0,

    #compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Zero,
                      m21=M21, m22=M22, m23=M23, ty=Zero,
                      m31=M31, m32=M32, m33=M33, tz=Zero }.



-doc """
Returns the 4x4 transition matrix from the current orthonormal basis (R1) to one
(R2) in which the origin and axes of the current basis (R1) are expressed:
P1->2.

Refer to
<http://howtos.esperide.org/ThreeDimensional.html#computing-transition-matrices>
for further details.
""".
-spec transition( point3(), unit_vector3(), unit_vector3(), unit_vector3() ) ->
                                compact_matrix4().
transition( Origin, X, Y, Z ) ->

    cond_utils:if_defined( myriad_debug_linear,
        trace_utils:debug_fmt( "Computing the transition matrix to "
            "a coordinate system of origin ~w, axes being ~w, ~w and ~w.",
            [ Origin, X, Y, Z ] ) ),

    cond_utils:if_defined( myriad_check_linear,
        begin
            point3:check( Origin ),
            vector3:check_unit_vectors( [ X, Y, Z ] ),
            vector3:check_orthogonal( X, Y ),
            vector3:check_orthogonal( X, Z ),
            vector3:check_orthogonal( Y, Z )
        end ),

    compact_from_columns( X, Y, Z, Origin ).



-doc """
Returns the 4x4 matrix whose columns correspond to the 4 specified 4D vectors.

Returns thus:
```
[ Va Vb Vc Vd ]
[ |  |  |  |  ]
[ |  |  |  |  ]
[ |  |  |  |  ]
```
""".
-spec from_columns( vector4(), vector4(), vector4(), vector4() ) ->
                                                        canonical_matrix4().
from_columns( _Va=[Xa,Ya,Za,Wa], _Vb=[Xb,Yb,Zb,Wb],
              _Vc=[Xc,Yc,Zc,Wc], _Vd=[Xd,Yd,Zd,Wd] ) ->
    #matrix4{ m11=Xa, m12=Xb, m13=Xc, m14=Xd,
              m21=Ya, m22=Yb, m23=Yc, m24=Yd,
              m31=Za, m32=Zb, m33=Zc, m34=Zd,
              m41=Wa, m42=Wb, m43=Wc, m44=Wd }.



-doc """
Returns the 4x4 compact matrix whose columns correspond to the 3 specified
vectors and 1 point (all expressed as 3D ones).

Returns thus:
```
[ Va Vb Vc P ]
[ |  |  |  | ]
[ |  |  |  | ]
[ 0  0  0  1 ]
```
""".
-spec compact_from_columns( vector3(), vector3(), vector3(), point3() ) ->
                                                        compact_matrix4().
compact_from_columns( _Va=[Xa,Ya,Za], _Vb=[Xb,Yb,Zb],
                      _Vc=[Xc,Yc,Zc], _P={Xp,Yp,Zp} ) ->
    #compact_matrix4{ m11=Xa, m12=Xb, m13=Xc, tx=Xp,
                      m21=Ya, m22=Yb, m23=Yc, ty=Yp,
                      m31=Za, m32=Zb, m33=Zc, tz=Zp }.



-doc """
Returns the 4x4 matrix whose rows correspond to the 4 specified 4D vectors.

Returns thus:
```
[ Va - - - ]
[ Vb - - - ]
[ Vc - - - ]
[ Vd - - - ]
```
""".
-spec from_rows( vector4(), vector4(), vector4(), vector4() ) ->
                            canonical_matrix4().
from_rows( _Va=[Xa,Ya,Za,Wa], _Vb=[Xb,Yb,Zb,Wb],
           _Vc=[Xc,Yc,Zc,Wc], _Vd=[Xd,Yd,Zd,Wd] ) ->
    #matrix4{ m11=Xa, m12=Ya, m13=Za, m14=Wa,
              m21=Xb, m22=Yb, m23=Zb, m24=Wb,
              m31=Xc, m32=Yc, m33=Zc, m34=Wc,
              m41=Xd, m42=Yd, m43=Zd, m44=Wd }.



-doc """
Returns the (4x4, canonical) matrix whose (16) coordinates are the specified
ones, as listed row after row.
""".
-spec from_coordinates( coordinate(), coordinate(), coordinate(), coordinate(),
                        coordinate(), coordinate(), coordinate(), coordinate(),
                        coordinate(), coordinate(), coordinate(), coordinate(),
                        coordinate(), coordinate(), coordinate(), coordinate() )
                                            -> canonical_matrix4().
from_coordinates( M11, M12, M13, M14,
                  M21, M22, M23, M24,
                  M31, M32, M33, M34,
                  M41, M42, M43, M44 ) ->
    #matrix4{ m11=M11, m12=M12, m13=M13, m14=M14,
              m21=M21, m22=M22, m23=M23, m24=M24,
              m31=M31, m32=M32, m33=M33, m34=M34,
              m41=M41, m42=M42, m43=M43, m44=M44 }.



-doc """
Returns the "4x4" (actually 3x4) compact matrix whose 12 actual coordinates are
the specified ones, as listed row after row.
""".
-spec from_compact_coordinates(
                    coordinate(), coordinate(), coordinate(), coordinate(),
                    coordinate(), coordinate(), coordinate(), coordinate(),
                    coordinate(), coordinate(), coordinate(), coordinate() ) ->
                            compact_matrix4().
from_compact_coordinates( M11, M12, M13, Tx,
                          M21, M22, M23, Ty,
                          M31, M32, M33, Tz ) ->
    #compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
                      m21=M21, m22=M22, m23=M23, ty=Ty,
                      m31=M31, m32=M32, m33=M33, tz=Tz }.



-doc """
Returns the 4x4 matrix corresponding to the specified arbitrary-dimensioned
matrix.
""".
-spec from_arbitrary( matrix() ) -> matrix4().
from_arbitrary( Matrix ) ->
    erlang:apply( fun from_rows/?dim, Matrix ).



-doc """
Returns the arbitrary-dimensioned matrix corresponding to the specified 4x4
matrix.
""".
-spec to_arbitrary( matrix4() ) -> matrix().
to_arbitrary( Matrix4 ) ->
    M = to_canonical( Matrix4 ),
    [ _RecordTag | Coords ] = tuple_to_list( M ),
    matrix:from_coordinates( Coords, _ColumCount=?dim ).



-doc """
Returns the dimension of these matrices.

Not useless, when using polymorphism based on module name.
""".
-spec dimension() -> dimension().
dimension() ->
    ?dim.



-doc """
Returns the dimensions of these matrices.

Not useless, when using polymorphism based on module name.
""".
-spec dimensions() -> dimensions().
dimensions() ->
    { ?dim, ?dim }.



-doc "Returns the specified row of the specified matrix.".
-spec row( dimension(), matrix4() ) -> vector4().
row( _RowCount=1, #matrix4{ m11=M11, m12=M12, m13=M13, m14=M14 } ) ->
    [ M11, M12, M13, M14 ];

row( _RowCount=2, #matrix4{ m21=M21, m22=M22, m23=M23, m24=M24 } ) ->
    [ M21, M22, M23, M24 ];

row( _RowCount=3, #matrix4{ m31=M31, m32=M32, m33=M33, m34=M34 } ) ->
    [ M31, M32, M33, M34 ];

row( _RowCount=4, #matrix4{ m41=M41, m32=M42, m33=M43, m34=M44 } ) ->
    [ M41, M42, M43, M44 ];

row( RowCount, OtherMatrix ) ->
    row( RowCount, to_canonical( OtherMatrix ) ).



-doc "Returns the specified column of the specified matrix.".
-spec column( dimension(), matrix4() ) -> vector4().
column( _ColumnCount=1, #matrix4{ m11=M11, m21=M21, m31=M31, m41=M41 } ) ->
    [ M11, M21, M31, M41 ];

column( _ColumnCount=2, #matrix4{ m12=M12, m22=M22, m32=M32, m42=M42 } ) ->
    [ M12, M22, M32, M42 ];

column( _ColumnCount=3, #matrix4{ m13=M13, m23=M23, m33=M33, m43=M43 } ) ->
    [ M13, M23, M33, M43 ];

column( _ColumnCount=4, #matrix4{ m14=M14, m24=M24, m34=M34, m44=M44 } ) ->
    [ M14, M24, M34, M44 ];

column( ColCount, OtherMatrix ) ->
    column( ColCount, to_canonical( OtherMatrix ) ).



-doc "Returns the specified column of the specified compact matrix.".
-spec compact_column( dimension(), compact_matrix4() ) -> vector3().
compact_column( _ColumnCount=1,
                #compact_matrix4{ m11=M11, m21=M21, m31=M31 } ) ->
    [ M11, M21, M31 ];

compact_column( _ColumnCount=2,
                #compact_matrix4{ m12=M12, m22=M22, m32=M32 } ) ->
    [ M12, M22, M32 ];

compact_column( _ColumnCount=3,
                #compact_matrix4{ m13=M13, m23=M23, m33=M33} ) ->
    [ M13, M23, M33 ];

compact_column( _ColumnCount=4,
                #compact_matrix4{ tx=Tx, ty=Ty, tz=Tz } ) ->
    [ Tx, Ty, Tz ].



-doc "Returns the first (3D) column of the specified transition matrix.".
-spec get_column_i( transition_matrix4() ) -> unit_vector3().
get_column_i( identity_4 ) ->
    vector3:x_axis();

get_column_i( CptMatrix ) ->
    compact_column( _ColumnCount=1, CptMatrix ).



-doc """
Returns the specified transition matrix once its first (3D) column has been
updated with the specified one.
""".
-spec set_column_i( transition_matrix4(), unit_vector3() ) -> unit_vector3().
set_column_i( identity_4, _Column=[ X, Y, Z ] ) ->
    Zero = 0.0,
    One = 1.0,
    #compact_matrix4{ m11=X, m12=Zero, m13=Zero, tx=Zero,
                      m21=Y, m22=One,  m23=Zero, ty=Zero,
                      m31=Z, m32=Zero, m33=One,  tz=Zero };

set_column_i( CptMatrix, _Column=[ X, Y, Z ] ) ->
    CptMatrix#compact_matrix4{ m11=X,
                               m21=Y,
                               m31=Z }.



-doc "Returns the second (3D) column of the specified transition matrix.".
-spec get_column_j( transition_matrix4() ) -> unit_vector3().
get_column_j( identity_4 ) ->
    vector3:y_axis();

get_column_j( CptMatrix ) ->
    compact_column( _ColumnCount=2, CptMatrix ).



-doc """
Returns the specified transition matrix once its second (3D) column has been
updated with the specified one.
""".
-spec set_column_j( transition_matrix4(), unit_vector3() ) -> unit_vector3().
set_column_j( identity_4, _Column=[ X, Y, Z ] ) ->
    Zero = 0.0,
    One = 1.0,
    #compact_matrix4{ m11=One,  m12=X, m13=Zero, tx=Zero,
                      m21=Zero, m22=Y, m23=Zero, ty=Zero,
                      m31=Zero, m32=Z, m33=One,  tz=Zero };

set_column_j( CptMatrix, _Column=[ X, Y, Z ] ) ->
    CptMatrix#compact_matrix4{ m12=X,
                               m22=Y,
                               m32=Z }.



-doc "Returns the third (3D) column of the specified transition matrix.".
-spec get_column_k( transition_matrix4() ) -> unit_vector3().
get_column_k( identity_4 ) ->
    vector3:z_axis();

get_column_k( CptMatrix ) ->
    compact_column( _ColumnCount=3, CptMatrix ).



-doc """
Returns the specified transition matrix once its third (3D) column has been
updated with the specified one.
""".
-spec set_column_k( transition_matrix4(), unit_vector3() ) -> unit_vector3().
set_column_k( identity_4, _Column=[ X, Y, Z ] ) ->
    Zero = 0.0,
    One = 1.0,
    #compact_matrix4{ m11=One,  m12=Zero, m13=X, tx=Zero,
                      m21=Zero, m22=One,  m23=Y, ty=Zero,
                      m31=Zero, m32=Zero, m33=Z, tz=Zero };

set_column_k( CptMatrix, _Column=[ X, Y, Z ] ) ->
    CptMatrix#compact_matrix4{ m13=X,
                               m23=Y,
                               m33=Z }.



-doc """
Returns the fourth (3D) column of the specified transition matrix.

This one is generally not a unit vector.
""".
-spec get_column_o( transition_matrix4() ) -> vector3().
get_column_o( identity_4 ) ->
    vector3:null();

get_column_o( CptMatrix ) ->
    compact_column( _ColumnCount=4, CptMatrix ).



-doc """
Returns the translation part (last 3D column) of the specified compact matrix,
as a (3D) point.

For an homogeneous matrix, this corresponds to a translation.
""".
-spec get_translation( compact_matrix4() ) -> point3().
get_translation( _CptMatrix=#compact_matrix4{ tx=Tx, ty=Ty, tz=Tz } ) ->
    { Tx, Ty, Tz };

get_translation( _CptMatrix=identity_4 ) ->
    Zero = 0.0,
    { Zero, Zero, Zero }.



-doc """
Returns the specified transition matrix once its fourth (3D) column has been
updated with the specified one.
""".
-spec set_column_o( transition_matrix4(), unit_vector3() ) -> unit_vector3().
set_column_o( identity_4, _Column=[ X, Y, Z ] ) ->
    Zero = 0.0,
    One = 1.0,
    #compact_matrix4{ m11=One,  m12=Zero, m13=Zero, tx=X,
                      m21=Zero, m22=One,  m23=Zero, ty=Y,
                      m31=Zero, m32=Zero, m33=One,  tz=Z };

set_column_o( CptMatrix, _Column=[ X, Y, Z ] ) ->
    CptMatrix#compact_matrix4{ tx=X,
                               ty=Y,
                               tz=Z }.



-doc """
Returns the element at the specified row and column of the specified matrix.
""".
-spec get_element( dimension(), dimension(), matrix4() ) -> coordinate().
get_element( _R=1, _C=1, #matrix4{ m11=M11 } ) ->
    M11;

get_element( _R=1, _C=2, #matrix4{ m12=M12 } ) ->
    M12;

get_element( _R=1, _C=3, #matrix4{ m13=M13 } ) ->
    M13;

get_element( _R=1, _C=4, #matrix4{ m14=M14 } ) ->
    M14;


get_element( _R=2, _C=1, #matrix4{ m21=M21 } ) ->
    M21;

get_element( _R=2, _C=2, #matrix4{ m22=M22 } ) ->
    M22;

get_element( _R=2, _C=3, #matrix4{ m23=M23 } ) ->
    M23;

get_element( _R=2, _C=4, #matrix4{ m24=M24 } ) ->
    M24;


get_element( _R=3, _C=1, #matrix4{ m31=M31 } ) ->
    M31;

get_element( _R=3, _C=2, #matrix4{ m32=M32 } ) ->
    M32;

get_element( _R=3, _C=3, #matrix4{ m33=M33 } ) ->
    M33;

get_element( _R=3, _C=4, #matrix4{ m34=M34 } ) ->
    M34;


get_element( _R=4, _C=1, #matrix4{ m41=M41 } ) ->
    M41;

get_element( _R=4, _C=2, #matrix4{ m42=M42 } ) ->
    M42;

get_element( _R=4, _C=3, #matrix4{ m43=M43 } ) ->
    M43;

get_element( _R=4, _C=4, #matrix4{ m44=M44 } ) ->
    M44;

get_element( R, C, OtherMatrix ) ->
    get_element( R, C, to_canonical( OtherMatrix ) ).



-doc """
Returns a matrix identical to the specified one except that its specified
element at specified location has been set to the specified value.
""".
-spec set_element( dimension(), dimension(), coordinate(), matrix4() ) ->
                                    matrix4().
set_element( _R=1, _C=1, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m11=Value };

set_element( _R=1, _C=2, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m12=Value };

set_element( _R=1, _C=3, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m13=Value };

set_element( _R=1, _C=4, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m14=Value };


set_element( _R=2, _C=1, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m21=Value };

set_element( _R=2, _C=2, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m22=Value };

set_element( _R=2, _C=3, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m23=Value };

set_element( _R=2, _C=4, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m24=Value };


set_element( _R=3, _C=1, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m31=Value };

set_element( _R=3, _C=2, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m32=Value };

set_element( _R=3, _C=3, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m33=Value };

set_element( _R=3, _C=4, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m34=Value };


set_element( _R=4, _C=1, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m41=Value };

set_element( _R=4, _C=2, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m42=Value };

set_element( _R=4, _C=3, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m43=Value };

set_element( _R=4, _C=4, Value, Matrix=#matrix4{} ) ->
    Matrix#matrix4{ m44=Value };

set_element( R, C, Value, OtherMatrix ) ->
    set_element( R, C, Value, to_canonical( OtherMatrix ) ).



-doc "Returns the transpose of the specified matrix.".
-spec transpose( matrix4() ) -> matrix4().
% Diagonal untouched:
transpose( M=#matrix4{ m12=M12, m13=M13, m14=M14,
                       m21=M21, m23=M23, m24=M24,
                       m31=M31, m32=M32, m34=M34,
                       m41=M41, m42=M42, m43=M43 } ) ->
    M#matrix4{ m12=M21, m13=M31, m14=M41,
               m21=M12, m23=M32, m24=M42,
               m31=M13, m32=M23, m34=M43,
               m41=M14, m42=M24, m43=M34 };

transpose( M=identity_4 ) ->
    M;

% A compact matrix is not anymore compact:
transpose( CompactMatrix ) ->
    transpose( to_canonical( CompactMatrix ) ).



-doc "Scales the specified (4D) matrix of the specified factor.".
-spec scale( matrix4(), scale_factor() ) -> matrix4().
scale( #compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
                         m21=M21, m22=M22, m23=M23, ty=Ty,
                         m31=M31, m32=M32, m33=M33, tz=Tz }, Factor ) ->
    % Not anymore a compact matrix, as last (bottom-right) implicit coordinate
    % (1.0) must be scaled as well:
    %
    #matrix4{
        m11=Factor*M11, m12=Factor*M12, m13=Factor*M13, m14=Factor*Tx,
        m21=Factor*M21, m22=Factor*M22, m23=Factor*M23, m24=Factor*Ty,
        m31=Factor*M31, m32=Factor*M32, m33=Factor*M33, m34=Factor*Tz,
        m41=0.0,        m42=0.0,        m43=0.0,        m44=Factor      };

scale( #matrix4{ m11=M11, m12=M12, m13=M13, m14=M14,
                 m21=M21, m22=M22, m23=M23, m24=M24,
                 m31=M31, m32=M32, m33=M33, m34=M34,
                 m41=M41, m42=M42, m43=M43, m44=M44 }, Factor ) ->
    #matrix4{ m11=Factor*M11, m12=Factor*M12, m13=Factor*M13, m14=Factor*M14,
              m21=Factor*M21, m22=Factor*M22, m23=Factor*M23, m24=Factor*M24,
              m31=Factor*M31, m32=Factor*M32, m33=Factor*M33, m34=Factor*M34,
              m41=Factor*M41, m42=Factor*M42, m43=Factor*M43, m44=Factor*M44 };

scale( M=identity_4, Factor ) ->

    % Not necessarily an homogeneous matrix:
    %Zero = 0.0,
    %#compact_matrix4{ m11=Factor, m12=Zero,   m13=Zero,   tx=Zero,
    %                  m21=Zero,   m22=Factor, m23=Zero,   ty=Zero,
    %                  m31=Zero,   m32=Zero,   m33=Factor, tz=Zero }.

    scale( to_canonical( M ), Factor ).



-doc "Returns the sum of the two specified matrices: `M = Ma + Mb`.".
-spec add( matrix4(), matrix4() ) -> matrix4().
add( _Ma=#matrix4{ m11=A11, m12=A12, m13=A13, m14=A14,
                   m21=A21, m22=A22, m23=A23, m24=A24,
                   m31=A31, m32=A32, m33=A33, m34=A34,
                   m41=A41, m42=A42, m43=A43, m44=A44 },
     _Mb=#matrix4{ m11=B11, m12=B12, m13=B13, m14=B14,
                   m21=B21, m22=B22, m23=B23, m24=B24,
                   m31=B31, m32=B32, m33=B33, m34=B34,
                   m41=B41, m42=B42, m43=B43, m44=B44 } ) ->

    #matrix4{ m11=A11+B11, m12=A12+B12, m13=A13+B13, m14=A14+B14,
              m21=A21+B21, m22=A22+B22, m23=A23+B23, m24=A24+B24,
              m31=A31+B31, m32=A32+B32, m33=A33+B33, m34=A34+B34,
              m41=A41+B41, m42=A42+B42, m43=A43+B43, m44=A44+B44 };


add( _Ma=#compact_matrix4{ m11=A11, m12=A12, m13=A13, tx=Tx,
                           m21=A21, m22=A22, m23=A23, ty=Ty,
                           m31=A31, m32=A32, m33=A33, tz=Tz },
     _Mb=#matrix4{ m11=B11, m12=B12, m13=B13, m14=B14,
                   m21=B21, m22=B22, m23=B23, m24=B24,
                   m31=B31, m32=B32, m33=B33, m34=B34,
                   m41=B41, m42=B42, m43=B43, m44=B44 } ) ->
    #matrix4{ m11=A11+B11, m12=A12+B12, m13=A13+B13, m14=Tx+B14,
              m21=A21+B21, m22=A22+B22, m23=A23+B23, m24=Ty+B24,
              m31=A31+B31, m32=A32+B32, m33=A33+B33, m34=Tz+B34,
              m41=B41,     m42=B42,     m43=B43,     m44=1.0+B44 };


add( Ma=#matrix4{}, Mb=#compact_matrix4{} ) ->
    add( Mb, Ma );


% Preserve compactness:
add( _Ma=#compact_matrix4{ m11=A11, m12=A12, m13=A13, tx=Ax,
                           m21=A21, m22=A22, m23=A23, ty=Ay,
                           m31=A31, m32=A32, m33=A33, tz=Az },
     _Mb=#compact_matrix4{ m11=B11, m12=B12, m13=B13, tx=Bx,
                           m21=B21, m22=B22, m23=B23, ty=By,
                           m31=B31, m32=B32, m33=B33, tz=Bz } ) ->

    #compact_matrix4{ m11=A11+B11, m12=A12+B12, m13=A13+B13, tx=Ax+Bx,
                      m21=A21+B21, m22=A22+B22, m23=A23+B23, ty=Ay+By,
                      m31=A31+B31, m32=A32+B32, m33=A33+B33, tz=Az+Bz };

add( Ma, Mb ) ->
    add( to_canonical( Ma ), to_canonical( Mb ) ).



-doc "Returns the subtraction of the two specified matrices: `M = Ma - Mb`.".
-spec sub( matrix4(), matrix4() ) -> matrix4().
% Quick and dirty, yet not satisfactory as expands compact matrices:
%sub( Ma, Mb ) ->
    %MinusMb = scale( Mb, -1.0 ),
    %add( Ma, MinusMb ).
sub( _Ma=#matrix4{ m11=A11, m12=A12, m13=A13, m14=A14,
                   m21=A21, m22=A22, m23=A23, m24=A24,
                   m31=A31, m32=A32, m33=A33, m34=A34,
                   m41=A41, m42=A42, m43=A43, m44=A44 },
     _Mb=#matrix4{ m11=B11, m12=B12, m13=B13, m14=B14,
                   m21=B21, m22=B22, m23=B23, m24=B24,
                   m31=B31, m32=B32, m33=B33, m34=B34,
                   m41=B41, m42=B42, m43=B43, m44=B44 } ) ->

    #matrix4{ m11=A11-B11, m12=A12-B12, m13=A13-B13, m14=A14-B14,
              m21=A21-B21, m22=A22-B22, m23=A23-B23, m24=A24-B24,
              m31=A31-B31, m32=A32-B32, m33=A33-B33, m34=A34-B34,
              m41=A41-B41, m42=A42-B42, m43=A43-B43, m44=A44-B44 };


sub( _Ma=#compact_matrix4{ m11=A11, m12=A12, m13=A13, tx=Tx,
                           m21=A21, m22=A22, m23=A23, ty=Ty,
                           m31=A31, m32=A32, m33=A33, tz=Tz },
     _Mb=#matrix4{ m11=B11, m12=B12, m13=B13, m14=B14,
                   m21=B21, m22=B22, m23=B23, m24=B24,
                   m31=B31, m32=B32, m33=B33, m34=B34,
                   m41=B41, m42=B42, m43=B43, m44=B44 } ) ->
    #matrix4{ m11=A11-B11, m12=A12-B12, m13=A13-B13, m14=Tx-B14,
              m21=A21-B21, m22=A22-B22, m23=A23-B23, m24=Ty-B24,
              m31=A31-B31, m32=A32-B32, m33=A33-B33, m34=Tz-B34,
              m41=-B41,    m42=-B42,    m43=-B43,    m44=1.0-B44 };


sub( _Ma=#matrix4{ m11=A11, m12=A12, m13=A13, m14=A14,
                   m21=A21, m22=A22, m23=A23, m24=A24,
                   m31=A31, m32=A32, m33=A33, m34=A34,
                   m41=A41, m42=A42, m43=A43, m44=A44 },
     _Mb=#compact_matrix4{ m11=B11, m12=B12, m13=B13, tx=Bx,
                           m21=B21, m22=B22, m23=B23, ty=By,
                           m31=B31, m32=B32, m33=B33, tz=Bz } ) ->
    #matrix4{ m11=A11-B11, m12=A12-B12, m13=A13-B13, m14=A14-Bx,
              m21=A21-B21, m22=A22-B22, m23=A23-B23, m24=A24-By,
              m31=A31-B31, m32=A32-B32, m33=A33-B33, m34=A34-Bz,
              m41=A41,     m42=A42,     m43=A43,     m44=A44-1.0 };


% Preserve compactness:
sub( _Ma=#compact_matrix4{ m11=A11, m12=A12, m13=A13, tx=Ax,
                           m21=A21, m22=A22, m23=A23, ty=Ay,
                           m31=A31, m32=A32, m33=A33, tz=Az },
     _Mb=#compact_matrix4{ m11=B11, m12=B12, m13=B13, tx=Bx,
                           m21=B21, m22=B22, m23=B23, ty=By,
                           m31=B31, m32=B32, m33=B33, tz=Bz } ) ->

    #compact_matrix4{ m11=A11-B11, m12=A12-B12, m13=A13-B13, tx=Ax-Bx,
                      m21=A21-B21, m22=A22-B22, m23=A23-B23, ty=Ay-By,
                      m31=A31-B31, m32=A32-B32, m33=A33-B33, tz=Az-Bz };

% At least one identity:
sub( Ma, Mb ) ->
    sub( to_canonical( Ma ), to_canonical( Mb ) ).



-doc "Multiplies the first matrix by the second one: returns `M = Ma.Mb`.".
-spec mult( Ma:: matrix4(), Mb :: matrix4() ) -> matrix4().
mult( _Ma=identity_4, Mb ) ->
    Mb;

mult( Ma, _Mb=identity_4 ) ->
    Ma;

mult( _Ma=#matrix4{ m11=A11, m12=A12, m13=A13, m14=A14,
                    m21=A21, m22=A22, m23=A23, m24=A24,
                    m31=A31, m32=A32, m33=A33, m34=A34,
                    m41=A41, m42=A42, m43=A43, m44=A44 },
      _Mb=#matrix4{ m11=B11, m12=B12, m13=B13, m14=B14,
                    m21=B21, m22=B22, m23=B23, m24=B24,
                    m31=B31, m32=B32, m33=B33, m34=B34,
                    m41=B41, m42=B42, m43=B43, m44=B44 } ) ->

    %trace_utils:debug_fmt( "Multiplying Ma = ~ts by Mb = ~ts",
    %                       [ to_string( Ma ), to_string( Mb ) ] ),

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

    #matrix4{ m11=C11, m12=C12, m13=C13, m14=C14,
              m21=C21, m22=C22, m23=C23, m24=C24,
              m31=C31, m32=C32, m33=C33, m34=C34,
              m41=C41, m42=C42, m43=C43, m44=C44 };

mult( _Ma=#compact_matrix4{ m11=A11, m12=A12, m13=A13, tx=Tx,
                            m21=A21, m22=A22, m23=A23, ty=Ty,
                            m31=A31, m32=A32, m33=A33, tz=Tz },
      _Mb=#matrix4{ m11=B11, m12=B12, m13=B13, m14=B14,
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

    #matrix4{ m11=C11, m12=C12, m13=C13, m14=C14,
              m21=C21, m22=C22, m23=C23, m24=C24,
              m31=C31, m32=C32, m33=C33, m34=C34,
              m41=C41, m42=C42, m43=C43, m44=C44 };

mult( _Ma=#matrix4{ m11=A11, m12=A12, m13=A13, m14=A14,
                    m21=A21, m22=A22, m23=A23, m24=A24,
                    m31=A31, m32=A32, m33=A33, m34=A34,
                    m41=A41, m42=A42, m43=A43, m44=A44 },
      _Mb=#compact_matrix4{ m11=B11, m12=B12, m13=B13, tx=Tx,
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

    #matrix4{ m11=C11, m12=C12, m13=C13, m14=C14,
              m21=C21, m22=C22, m23=C23, m24=C24,
              m31=C31, m32=C32, m33=C33, m34=C34,
              m41=C41, m42=C42, m43=C43, m44=C44 };

mult( _Ma=#compact_matrix4{ m11=A11, m12=A12, m13=A13, tx=Ax,
                            m21=A21, m22=A22, m23=A23, ty=Ay,
                            m31=A31, m32=A32, m33=A33, tz=Az },
      _Mb=#compact_matrix4{ m11=B11, m12=B12, m13=B13, tx=Bx,
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

    #compact_matrix4{ m11=C11, m12=C12, m13=C13, tx=Cx,
                      m21=C21, m22=C22, m23=C23, ty=Cy,
                      m31=C31, m32=C32, m33=C33, tz=Cz }.



-doc """
Multiplies (in-order) all specified matrices.

For example `mult([Ma, Mb, Mc]) = mult(mult(Ma,Mb),Mc) = Ma.Mb.Mc`.
""".
-spec mult( [ matrix4() ] ) -> matrix4().
mult( [ Ma, Mb | T ] ) ->
    mult( [ mult( Ma, Mb ) | T ] );

mult( [ M ] ) ->
    M.



-doc """
Applies (on the right) the specified 3D or 4D vector V or point P to the
specified matrix M: returns M.V or M.P.

If the specified vector is a 3D one (i.e. not a 4D one), we assume that its
fourth (Vw) coordinate is 0.0, whereas if the specified point is a 3D one
(i.e. not a 4D one), we assume that its fourth (Pw) coordinate is 1.0, and
returns a 3D point whose coordinates have been normalised regarding the W
coordinate resulting from the application of that extended point.

Not a clause of mult/2 for an increased clarity.
""".
-spec apply( matrix4(), vector3() ) -> vector3();
           ( matrix4(), vector4() ) -> vector4();
           ( matrix4(), point3() ) -> point3();
           ( matrix4(), point4() ) -> point4().
apply( _M=identity_4, VorP ) ->
    VorP;

% A nice feature is that the actual, lowest-level types of vectors and points
% are different (list vs tuple) and thus can be discriminated.
%
% First with a vector3 (implicitly Vw is 0.0):
apply( _M=#matrix4{ m11=M11,  m12=M12,  m13=M13,  m14=_M14,
                    m21=M21,  m22=M22,  m23=M23,  m24=_M24,
                    m31=M31,  m32=M32,  m33=M33,  m34=_M34,
                    m41=_M41, m42=_M42, m43=_M43, m44=_M44 },
       _V=[ Vx, Vy, Vz ] ) ->

    %Vw = 0.0,
    ResX = M11*Vx + M12*Vy + M13*Vz,
    ResY = M21*Vx + M22*Vy + M23*Vz,
    ResZ = M31*Vx + M32*Vy + M33*Vz,
    %ResW = M41*Vx + M42*Vy + M43*Vz,

    [ ResX, ResY, ResZ ];


apply( _M=#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=_Tx,
                            m21=M21, m22=M22, m23=M23, ty=_Ty,
                            m31=M31, m32=M32, m33=M33, tz=_Tz },
       _V=[ Vx, Vy, Vz ] ) ->
    %Vw = 0.0,
    ResX = M11*Vx + M12*Vy + M13*Vz,
    ResY = M21*Vx + M22*Vy + M23*Vz,
    ResZ = M31*Vx + M32*Vy + M33*Vz,
    % Here ResW = Vw = 0.0,

    [ ResX, ResY, ResZ ];


% Then with a point3 (implicitly Pw is 1.0):
apply( _M=#matrix4{ m11=M11, m12=M12, m13=M13, m14=M14,
                    m21=M21, m22=M22, m23=M23, m24=M24,
                    m31=M31, m32=M32, m33=M33, m34=M34,
                    m41=M41, m42=M42, m43=M43, m44=M44 },
       _P={ Px, Py, Pz } ) ->

    %Pw = 1.0,
    ResX = M11*Px + M12*Py + M13*Pz + M14,
    ResY = M21*Px + M22*Py + M23*Pz + M24,
    ResZ = M31*Px + M32*Py + M33*Pz + M34,
    ResW = M41*Px + M42*Py + M43*Pz + M44,

    % A point shall be normalised:
    case math_utils:is_null( ResW ) of

        true ->
            throw( null_w_coordinate );

        false ->
            { ResX/ResW, ResY/ResW, ResZ/ResW }

    end;


apply( _M=#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
                            m21=M21, m22=M22, m23=M23, ty=Ty,
                            m31=M31, m32=M32, m33=M33, tz=Tz },
       _P={ Px, Py, Pz } ) ->
    %Pw = 1.0,
    ResX = M11*Px + M12*Py + M13*Pz + Tx,
    ResY = M21*Px + M22*Py + M23*Pz + Ty,
    ResZ = M31*Px + M32*Py + M33*Pz + Tz,
    % Here ResW = Pw = 1.0,

    % Already normalised:
    { ResX, ResY, ResZ };


% Then with a vector4:
apply( _M=#matrix4{ m11=M11, m12=M12, m13=M13, m14=M14,
                    m21=M21, m22=M22, m23=M23, m24=M24,
                    m31=M31, m32=M32, m33=M33, m34=M34,
                    m41=M41, m42=M42, m43=M43, m44=M44 },
       _V=[ Vx, Vy, Vz, Vw ] ) ->

    ResX = M11*Vx + M12*Vy + M13*Vz + M14*Vw,
    ResY = M21*Vx + M22*Vy + M23*Vz + M24*Vw,
    ResZ = M31*Vx + M32*Vy + M33*Vz + M34*Vw,
    ResW = M41*Vx + M42*Vy + M43*Vz + M44*Vw,

    [ ResX, ResY, ResZ, ResW ];


apply( _M=#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
                            m21=M21, m22=M22, m23=M23, ty=Ty,
                            m31=M31, m32=M32, m33=M33, tz=Tz },
       _V=[ Vx, Vy, Vz, Vw ] ) ->
    ResX = M11*Vx + M12*Vy + M13*Vz + Tx*Vw,
    ResY = M21*Vx + M22*Vy + M23*Vz + Ty*Vw,
    ResZ = M31*Vx + M32*Vy + M33*Vz + Tz*Vw,
    ResW = Vw,

    [ ResX, ResY, ResZ, ResW ];


% Finally with a point4 (mostly the same as for vector4):
apply( _M=#matrix4{ m11=M11, m12=M12, m13=M13, m14=M14,
                    m21=M21, m22=M22, m23=M23, m24=M24,
                    m31=M31, m32=M32, m33=M33, m34=M34,
                    m41=M41, m42=M42, m43=M43, m44=M44 },
       _P={ Px, Py, Pz, Pw } ) ->

    ResX = M11*Px + M12*Py + M13*Pz + M14*Pw,
    ResY = M21*Px + M22*Py + M23*Pz + M24*Pw,
    ResZ = M31*Px + M32*Py + M33*Pz + M34*Pw,
    ResW = M41*Px + M42*Py + M43*Pz + M44*Pw,

    { ResX, ResY, ResZ, ResW };


apply( _M=#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
                            m21=M21, m22=M22, m23=M23, ty=Ty,
                            m31=M31, m32=M32, m33=M33, tz=Tz },
       _P={ Px, Py, Pz, Pw } ) ->
    ResX = M11*Px + M12*Py + M13*Pz + Tx*Pw,
    ResY = M21*Px + M22*Py + M23*Pz + Ty*Pw,
    ResZ = M31*Px + M32*Py + M33*Pz + Tz*Pw,
    ResW = Pw,

    { ResX, ResY, ResZ, ResW }.



-doc """
Applies on the left the specified 3D point P to the specified homogeneous matrix
HM: returns VPt.HM, as a 3D point, where VPt means the transpose of the 4D
vector corresponding to P.

Note: handling a point, not a vector, so that the translation part of the matrix
is applied as well.

This application is rather uncommon and, because of the normalisation, may not
yield the intended result.
""".
-spec apply_homogeneous_left( point3(), homogeneous_matrix4() ) -> point3().
apply_homogeneous_left( _P={ Px, Py, Pz },
                        _HM=#compact_matrix4{
                                m11=M11, m12=M12, m13=M13, tx=Tx,
                                m21=M21, m22=M22, m23=M23, ty=Ty,
                                m31=M31, m32=M32, m33=M33, tz=Tz } ) ->

    % The fourth (Pw) coordinate of P is considered to be 1.0 (point, not
    % vector), which yields to a renormalisation:

    Pw = Px*Tx + Py*Ty + Pz*Tz + 1,

    math_utils:is_null( Pw ) andalso throw( cannot_normalise_point ),

    { ( Px*M11 + Py*M21 + Pz*M31 + Tx ) / Pw,
      ( Px*M12 + Py*M22 + Pz*M32 + Ty ) / Pw,
      ( Px*M13 + Py*M23 + Pz*M33 + Tz ) / Pw };

apply_homogeneous_left( P, _HM=identity_4 ) ->
    P.



-doc """
Applies on the right the specified 3D point P to the specified homogeneous
matrix HM: returns HM.VP, as a 3D vector, where VP means the 4D vector
corresponding to P.

This is thus a specialised, stripped-down version of apply/2.
""".
-spec apply_homogeneous_right( homogeneous_matrix4(), point3() ) -> vector3().
apply_homogeneous_right( _HM=#compact_matrix4{
                                m11=M11, m12=M12, m13=M13, tx=Tx,
                                m21=M21, m22=M22, m23=M23, ty=Ty,
                                m31=M31, m32=M32, m33=M33, tz=Tz },
                         _P={ Px, Py, Pz } ) ->

    % The fourth (Pw) coordinate of P is considered to be 1.0 (point, not
    % vector):

    { M11*Px + M12*Py + M13*Pz + Tx,
      M21*Px + M22*Py + M23*Pz + Ty,
      M31*Px + M32*Py + M33*Pz + Tz };

apply_homogeneous_right( _HM=identity_4, P ) ->
    P.



-doc "Tells whether the two specified (4x4) matrices are equal.".
-spec are_equal( matrix4(), matrix4() ) -> boolean().
are_equal( _Ma=#matrix4{ m11=A11, m12=A12, m13=A13, m14=A14,
                         m21=A21, m22=A22, m23=A23, m24=A24,
                         m31=A31, m32=A32, m33=A33, m34=A34,
                         m41=A41, m42=A42, m43=A43, m44=A44 },
           _Mb=#matrix4{ m11=B11, m12=B12, m13=B13, m14=B14,
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

are_equal( _Ma=#compact_matrix4{ m11=A11, m12=A12, m13=A13, tx=Ax,
                                 m21=A21, m22=A22, m23=A23, ty=Ay,
                                 m31=A31, m32=A32, m33=A33, tz=Az },
           _Mb=#compact_matrix4{ m11=B11, m12=B12, m13=B13, tx=Bx,
                                 m21=B21, m22=B22, m23=B23, ty=By,
                                 m31=B31, m32=B32, m33=B33, tz=Bz } ) ->
    are_close( A11, B11 ) andalso are_close( A12, B12 )
        andalso are_close( A13, B13 ) andalso are_close( Ax,  Bx )
        andalso are_close( A21, B21 ) andalso are_close( A22, B22 )
        andalso are_close( A23, B23 ) andalso are_close( Ay,  By )
        andalso are_close( A31, B31 ) andalso are_close( A32, B32 )
        andalso are_close( A33, B33 ) andalso are_close( Az,  Bz );

are_equal( _Ma=#matrix4{ m11=A11, m12=A12, m13=A13, m14=A14,
                         m21=A21, m22=A22, m23=A23, m24=A24,
                         m31=A31, m32=A32, m33=A33, m34=A34,
                         m41=A41, m42=A42, m43=A43, m44=A44 },
           _Mb=#compact_matrix4{ m11=B11, m12=B12, m13=B13, tx=Bx,
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

are_equal( Ma=#compact_matrix4{}, Mb=#matrix4{} ) ->
    are_equal( Mb, Ma );

are_equal( _Ma=identity_4, _Mb=identity_4 ) ->
    true;

are_equal( Ma, Mb=identity_4 ) ->
    are_equal( Ma, to_canonical( Mb ) );

are_equal( Ma=identity_4, Mb ) ->
    are_equal( Mb, Ma ).



-doc """
Returns the specified homogeneous matrix (HM) after having multiplied it on its
left by a translation matrix (TM) of a vector VT: returns therefore `HM' =
TM.HM`.

This corresponds to adding the VT vector to the translation part of the
reference matrix of this transformation.

A lot more efficient than creating a dedicated translation matrix TM and
computing from scratch the result.
""".
-spec translate_homogeneous_left( vector3(), HM :: homogeneous_matrix4() ) ->
                                            homogeneous_matrix4().
translate_homogeneous_left( _VT=[ Tx, Ty, Tz ],
                            HM=#compact_matrix4{ tx=M14,
                                                 ty=M24,
                                                 tz=M34 } ) ->
    % NewHM = TM.HM:
    HM#compact_matrix4{ tx=M14+Tx,
                        ty=M24+Ty,
                        tz=M34+Tz };

translate_homogeneous_left( VT, _HM=identity_4 ) ->
    translation( VT ).



-doc """
Returns the specified homogeneous matrix (HM) after having multiplied it on its
right by a translation matrix (TM) of a vector VT: returns therefore `HM' =
HM.TM`.
""".
-spec translate_homogeneous_right( HM :: homogeneous_matrix4(), vector3() ) ->
                                            homogeneous_matrix4().
translate_homogeneous_right( _HM=identity_4, VT ) ->
    translation( VT );

translate_homogeneous_right( HM=#compact_matrix4{
                                m11=M11, m12=M12, m13=M13, tx=Tx,
                                m21=M21, m22=M22, m23=M23, ty=Ty,
                                m31=M31, m32=M32, m33=M33, tz=Tz },
                             VT=[ Vx, Vy, Vz ] ) ->
    % Could be directly:
    %TM = translation( VT ),
    %mult( HM, TM ).

    % A bit more optimised:

    NewTx = Tx + M11*Vx + M12*Vy + M13*Vz,
    NewTy = Ty + M21*Vx + M22*Vy + M23*Vz,
    NewTz = Tz + M31*Vx + M32*Vy + M33*Vz,

    Res = HM#compact_matrix4{ tx=NewTx,
                              ty=NewTy,
                              tz=NewTz },

    % No need to check more, it is correct:
    cond_utils:if_defined( myriad_check_paranoid_linear,
                           begin
                                TM = translation( VT ),
                                CorrectRes = mult( HM, TM ),
                                true = are_equal( Res, CorrectRes )
                           end,
                           basic_utils:ignore_unused( VT ) ),

    Res.




-doc """
Returns the specified homogeneous matrix (HM) after having multiplied it on its
left by a matrix (RotM) corresponding to the rotation around the specified unit
axis of the specified angle: returns therefore HM' = RotM.HM.
""".
-spec rotate_homogeneous_left( unit_vector3(), radians(),
        HM :: homogeneous_matrix4() ) -> homogeneous_matrix4().
rotate_homogeneous_left( UnitAxis, RadAngle, HM ) ->
    RotM = rotation( UnitAxis, RadAngle ),
    mult( RotM, HM ).



-doc """
Returns the specified homogeneous matrix (HM) after having multiplied it on its
right by a matrix (RotM) corresponding to the rotation around the specified unit
axis of the specified angle: returns therefore HM' = HM.RotM.
""".
-spec rotate_homogeneous_right( HM :: homogeneous_matrix4(),
        unit_vector3(), radians() ) -> homogeneous_matrix4().
rotate_homogeneous_right( HM, UnitAxis, RadAngle ) ->
    RotM = rotation( UnitAxis, RadAngle ),
    mult( HM, RotM ).



-doc """
Returns the specified homogeneous matrix (HM) after having multiplied it on its
left by a matrix (SclM) corresponding to a scaling of the specified factors:
returns therefore HM' = SclM.HM.
""".
-spec scale_homogeneous_left( scale_factors(), homogeneous_matrix4() ) ->
                                        homogeneous_matrix4().
scale_homogeneous_left( _ScaleFactors={ Sx, Sy, Sz },
        HM=#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
                             m21=M21, m22=M22, m23=M23, ty=Ty,
                             m31=M31, m32=M32, m33=M33, tz=Tz } ) ->

    % Do not reuse tx, ty and tz as they are, they have to be scaled as well
    % (refer to the actual 4x4 matrix multiplication to check):
    %
    HM#compact_matrix4{ m11=Sx*M11, m12=Sx*M12, m13=Sx*M13, tx=Sx*Tx,
                        m21=Sy*M21, m22=Sy*M22, m23=Sy*M23, ty=Sy*Ty,
                        m31=Sz*M31, m32=Sz*M32, m33=Sz*M33, tz=Sz*Tz };

scale_homogeneous_left( _ScaleFactors={ Sx, Sy, Sz }, _HM=identity_4 ) ->
    Zero = 0.0,
    #compact_matrix4{ m11=Sx,   m12=Zero, m13=Zero, tx=Zero,
                      m21=Zero, m22=Sy,   m23=Zero, ty=Zero,
                      m31=Zero, m32=Zero, m33=Sz,   tz=Zero }.



-doc """
Returns the specified homogeneous matrix (HM) after having multiplied it on its
right by a matrix (SclM) corresponding to a scaling of the specified factors:
returns therefore HM' = HM.SclM.
""".
-spec scale_homogeneous_right( homogeneous_matrix4(), scale_factors() ) ->
                                        homogeneous_matrix4().
scale_homogeneous_right(
        HM=#compact_matrix4{ m11=M11, m12=M12, m13=M13,
                             m21=M21, m22=M22, m23=M23,
                             m31=M31, m32=M32, m33=M33 },
        _ScaleFactors={ Sx, Sy, Sz } ) ->

    % Reuse tx, ty and tz as they are (as multiplied by a 1.0 factor):
    HM#compact_matrix4{ m11=Sx*M11, m12=Sy*M12, m13=Sz*M13,
                        m21=Sx*M21, m22=Sy*M22, m23=Sz*M23,
                        m31=Sx*M31, m32=Sy*M32, m33=Sz*M33 };

scale_homogeneous_right( _ScaleFactors={ Sx, Sy, Sz }, _HM=identity_4 ) ->
    Zero = 0.0,
    #compact_matrix4{ m11=Sx,   m12=Zero, m13=Zero, tx=Zero,
                      m21=Zero, m22=Sy,   m23=Zero, ty=Zero,
                      m31=Zero, m32=Zero, m33=Sz,   tz=Zero }.



-doc """
Updates the specified homogeneous matrix by applying the specified (uniform)
shearing factor to the top-left 3x3 matrix (rightmost column not modified):
returns therefore HM' = f.HM.
""".
-spec scale_homogeneous( HM :: homogeneous_matrix4(), scale_factor() ) ->
                                            compact_matrix4().
% Note that, due to the role of the w (bottom-right) coordinate, a basic matrix
% scaling would have no consequence, in the sense that once renormalised (all
% coordinates divided by w) the original matrix would be obtained back. So the
% rightmost remains untouched as a whole.
%
scale_homogeneous( HM=#compact_matrix4{ m11=M11, m12=M12, m13=M13,   %tx=Tx,
                                        m21=M21, m22=M22, m23=M23,   %ty=Ty,
                                        m31=M31, m32=M32, m33=M33 }, %tz=Tz },
                   Factor ) ->
    HM#compact_matrix4{ m11=Factor*M11, m12=Factor*M12, m13=Factor*M13,
                        m21=Factor*M21, m22=Factor*M22, m23=Factor*M23,
                        m31=Factor*M31, m32=Factor*M32, m33=Factor*M33 };


scale_homogeneous( _HM=identity_4, Factor ) ->
    Zero = 0.0,
    #compact_matrix4{ m11=Factor, m12=Zero,   m13=Zero,   tx=Zero,
                      m21=Zero,   m22=Factor, m23=Zero,   ty=Zero,
                      m31=Zero,   m32=Zero,   m33=Factor, tz=Zero }.



-doc """
Updates the specified homogeneous matrix by applying the specified (uniform)
shearing factor on its leftmost column (X), like when this matrix is multiplied
on its right by a scaling matrix equal to the identity, except for its first
diagonal element, which would be equal to the specified factor; returns
therefore HM' = HM.SxM.
""".
-spec scale_homogeneous_x( HM :: homogeneous_matrix4(), scale_factor() ) ->
                                            compact_matrix4().
scale_homogeneous_x( HM=#compact_matrix4{ m11=M11,   %m12=M12, m13=M13, tx=Tx,
                                          m21=M21,   %m22=M22, m23=M23, ty=Ty,
                                          m31=M31 }, %m32=M32, m33=M33  tz=Tz },
                     Factor ) ->
    HM#compact_matrix4{ m11=Factor*M11,
                        m21=Factor*M21,
                        m31=Factor*M31 };

scale_homogeneous_x( identity_4, Factor ) ->
    Zero = 0.0,
    One =  1.0,
    #compact_matrix4{ m11=Factor, m12=Zero,   m13=Zero, tx=Zero,
                      m21=Zero,   m22=One,    m23=Zero, ty=Zero,
                      m31=Zero,   m32=Zero,   m33=One,  tz=Zero }.



-doc """
Updates the specified homogeneous matrix by applying the specified (uniform)
shearing factor to its topmost row (hence the "x_t" prefix, for transposed X
column), like when this matrix is multiplied on its left by a scaling matrix
equal to the identity, except for its first diagonal element, which would be
scaled by the specified factor.

A lot cheaper than multiplying by a mostly-identity matrix.
""".
-spec scale_homogeneous_x_t( HM :: homogeneous_matrix4(), scale_factor() ) ->
                                            compact_matrix4().
scale_homogeneous_x_t( HM=#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx },
                       Factor ) ->
    HM#compact_matrix4{ m11=Factor*M11, m12=Factor*M12,
                        m13=Factor*M13, tx=Factor*Tx };

% Like for the non-transposed case:
scale_homogeneous_x_t( identity_4, Factor ) ->
    Zero = 0.0,
    One =  1.0,
    #compact_matrix4{ m11=Factor, m12=Zero,   m13=Zero, tx=Zero,
                      m21=Zero,   m22=One,    m23=Zero, ty=Zero,
                      m31=Zero,   m32=Zero,   m33=One,  tz=Zero }.



-doc """
Updates the specified homogeneous matrix by applying the specified (uniform)
shearing factor on its second column (Y), like when this matrix is multiplied on
its right by a scaling matrix equal to the identity, except for its second
diagonal element, which would be equal to the specified factor; returns
therefore HM' = HM.SyM.
""".
-spec scale_homogeneous_y( HM :: homogeneous_matrix4(), scale_factor() ) ->
                                            compact_matrix4().
scale_homogeneous_y( HM=#compact_matrix4{ m12=M12,
                                          m22=M22,
                                          m32=M32 },
                     Factor ) ->
    HM#compact_matrix4{ m12=Factor*M12,
                        m22=Factor*M22,
                        m32=Factor*M32 };

scale_homogeneous_y( identity_4, Factor ) ->
    Zero = 0.0,
    One =  1.0,
    #compact_matrix4{ m11=One,  m12=Zero,   m13=Zero, tx=Zero,
                      m21=Zero, m22=Factor, m23=Zero, ty=Zero,
                      m31=Zero, m32=Zero,   m33=One,  tz=Zero }.



-doc """
Updates the specified homogeneous matrix by applying the specified (uniform)
shearing factor to its second row (hence the "y_t" prefix, for transposed Y
column), like when this matrix is multiplied on its left by a scaling matrix
equal to the identity, except for its second diagonal element, which would be
scaled by the specified factor.

A lot cheaper than multiplying by a mostly-identity matrix.
""".
-spec scale_homogeneous_y_t( HM :: homogeneous_matrix4(), scale_factor() ) ->
                                            compact_matrix4().
scale_homogeneous_y_t( HM=#compact_matrix4{ m21=M21, m22=M22, m23=M23, ty=Ty },
                       Factor ) ->
    HM#compact_matrix4{ m21=Factor*M21, m22=Factor*M22,
                        m23=Factor*M23, ty=Factor*Ty };

% Like for the non-transposed case:
scale_homogeneous_y_t( identity_4, Factor ) ->
    Zero = 0.0,
    One =  1.0,
    #compact_matrix4{ m11=One,  m12=Zero,   m13=Zero, tx=Zero,
                      m21=Zero, m22=Factor, m23=Zero, ty=Zero,
                      m31=Zero, m32=Zero,   m33=One,  tz=Zero }.



-doc """
Updates the specified homogeneous matrix by applying the specified (uniform)
shearing factor on its third column (Z), like when this matrix is multiplied on
its right by a scaling matrix equal to the identity, except for its third
diagonal element, which would be equal to the specified factor; returns
therefore HM' = HM.SzM.
""".
-spec scale_homogeneous_z( HM :: homogeneous_matrix4(), scale_factor() ) ->
                                            compact_matrix4().
scale_homogeneous_z( HM=#compact_matrix4{ m13=M13,
                                          m23=M23,
                                          m33=M33 },
                     Factor ) ->
    HM#compact_matrix4{ m13=Factor*M13,
                        m23=Factor*M23,
                        m33=Factor*M33 };

scale_homogeneous_z( identity_4, Factor ) ->
    Zero = 0.0,
    One =  1.0,
    #compact_matrix4{ m11=One,  m12=Zero,  m13=Zero,   tx=Zero,
                      m21=Zero, m22=One,   m23=Zero,   ty=Zero,
                      m31=Zero, m32=Zero,  m33=Factor, tz=Zero }.



-doc """
Updates the specified homogeneous matrix by applying the specified (uniform)
shearing factor to its third row (hence the "z_t" prefix, for transposed Z
column), like when this matrix is multiplied on its left by a scaling matrix
equal to the identity, except for its third diagonal element, which would be
scaled by the specified factor.

A lot cheaper than multiplying by a mostly-identity matrix.
""".
-spec scale_homogeneous_z_t( HM :: homogeneous_matrix4(), scale_factor() ) ->
                                            compact_matrix4().
scale_homogeneous_z_t( HM=#compact_matrix4{ m31=M31, m32=M32, m33=M33, tz=Tz },
                       Factor ) ->
    HM#compact_matrix4{ m31=Factor*M31, m32=Factor*M32,
                        m33=Factor*M33, tz=Factor*Tz };

% Like for the non-transposed case:
scale_homogeneous_z_t( identity_4, Factor ) ->
    Zero = 0.0,
    One =  1.0,
    #compact_matrix4{ m11=One,  m12=Zero, m13=Zero, tx=Zero,
                      m21=Zero, m22=One,  m23=Zero, ty=Zero,
                      m31=Zero, m32=Zero, m33=Factor,  tz=Zero }.



-doc "Returns the determinant of the specified matrix.".
-spec determinant( matrix4() ) -> scalar().
determinant( _M=#matrix4{ m11=M11, m12=M12, m13=M13, m14=M14,
                          m21=M21, m22=M22, m23=M23, m24=M24,
                          m31=M31, m32=M32, m33=M33, m34=M34,
                          m41=M41, m42=M42, m43=M43, m44=M44 } ) ->
          M11*M22*M33*M44 - M11*M22*M34*M43 - M11*M23*M32*M44 + M11*M23*M34*M42
        + M11*M24*M32*M43 - M11*M24*M33*M42 - M12*M21*M33*M44 + M12*M21*M34*M43
        + M12*M23*M31*M44 - M12*M23*M34*M41 - M12*M24*M31*M43 + M12*M24*M33*M41
        + M13*M21*M32*M44 - M13*M21*M34*M42 - M13*M22*M31*M44 + M13*M22*M34*M41
        + M13*M24*M31*M42 - M13*M24*M32*M41 - M14*M21*M32*M43 + M14*M21*M33*M42
        + M14*M22*M31*M43 - M14*M22*M33*M41 - M14*M23*M31*M42 + M14*M23*M32*M41;

determinant( _M=#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=_Mx,
                                  m21=M21, m22=M22, m23=M23, ty=_My,
                                  m31=M31, m32=M32, m33=M33, tz=_Mz } ) ->
    M11*M22*M33 - M11*M23*M32 - M12*M21*M33 + M12*M23*M31
        + M13*M21*M32 - M13*M22*M31 ;


determinant( _M=identity_4 ) ->
    1.0;

determinant( M ) ->
    % Could be simplified a lot for compact matrices thanks to their zeros:
    determinant( to_canonical( M ) ).



-doc """
Returns the comatrix of the specified matrix (that is the matrix of its
cofactors).
""".
-spec comatrix( matrix4() ) -> matrix4().
comatrix( identity_4 ) ->
    identity_4;

comatrix( _M=#matrix4{ m11=M11, m12=M12, m13=M13, m14=M14,
                       m21=M21, m22=M22, m23=M23, m24=M24,
                       m31=M31, m32=M32, m33=M33, m34=M34,
                       m41=M41, m42=M42, m43=M43, m44=M44 } ) ->

    % Term reordering done in order to possibly remove extra negations: '-A + B'
    % translated to 'B - A'.

    CM11 = M22*M33*M44 + M23*M34*M42 + M24*M32*M43
        - M24*M33*M42 - M23*M32*M44 - M22*M34*M43,

    CM12 = M24*M33*M41 + M23*M31*M44 + M21*M34*M43
        - M21*M33*M44 - M23*M34*M41 - M24*M31*M43,

    CM13 = M21*M32*M44 + M22*M34*M41 + M24*M31*M42
        - M24*M32*M41 - M22*M31*M44 - M21*M34*M42,

    CM14 = M23*M32*M41 + M22*M31*M43 + M21*M33*M42
        - M21*M32*M43 - M22*M33*M41 - M23*M31*M42,


    CM21 = M14*M33*M42 + M13*M32*M44 + M12*M34*M43
        - M12*M33*M44 - M13*M34*M42 - M14*M32*M43,

    CM22 = M11*M33*M44 + M13*M34*M41 + M14*M31*M43
        - M14*M33*M41 - M13*M31*M44 - M11*M34*M43,

    CM23 = M14*M32*M41 + M12*M31*M44 + M11*M34*M42
        - M11*M32*M44 - M12*M34*M41 - M14*M31*M42,

    CM24 = M11*M32*M43 + M12*M33*M41 + M13*M31*M42
        - M13*M32*M41 - M12*M31*M43 - M11*M33*M42,


    CM31 = M12*M23*M44 + M13*M24*M42 + M14*M22*M43
        - M14*M23*M42 - M13*M22*M44 - M12*M24*M43,

    CM32 = M14*M23*M41 + M13*M21*M44 + M11*M24*M43
        - M11*M23*M44 - M13*M24*M41 - M14*M21*M43,

    CM33 = M11*M22*M44 + M12*M24*M41 + M14*M21*M42
        - M14*M22*M41 - M12*M21*M44 - M11*M24*M42,

    CM34 = M13*M22*M41 + M12*M21*M43 + M11*M23*M42
        - M11*M22*M43 - M12*M23*M41 - M13*M21*M42,


    CM41 =  M14*M23*M32 + M13*M22*M34 + M12*M24*M33
        - M12*M23*M34 - M13*M24*M32 - M14*M22*M33,

    CM42 = M11*M23*M34 + M13*M24*M31 + M14*M21*M33
        - M14*M23*M31 - M13*M21*M34 - M11*M24*M33,

    CM43 = M14*M22*M31 + M12*M21*M34 + M11*M24*M32
        - M11*M22*M34 - M12*M24*M31 - M14*M21*M32,

    CM44 = M11*M22*M33 + M12*M23*M31 + M13*M21*M32
        - M13*M22*M31 - M12*M21*M33 - M11*M23*M32,

    #matrix4{ m11=CM11, m12=CM12, m13=CM13, m14=CM14,
              m21=CM21, m22=CM22, m23=CM23, m24=CM24,
              m31=CM31, m32=CM32, m33=CM33, m34=CM34,
              m41=CM41, m42=CM42, m43=CM43, m44=CM44 };


comatrix( _M=#compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
                               m21=M21, m22=M22, m23=M23, ty=Ty,
                               m31=M31, m32=M32, m33=M33, tz=Tz } ) ->

    % Huge simplification; yet not even the transpose of a compact matrix, as
    % the last coordinate (M44) is not necessarily 1.0 (being the determinant):

    % Term reordering done in order to possibly remove extra negations: '-A + B'
    % translated to 'B - A'.

    CM11 = M22*M33 - M23*M32,

    CM12 = M23*M31 - M21*M33,

    CM13 = M21*M32 - M22*M31,

    CM14 = 0.0,


    CM21 = M13*M32 - M12*M33 ,

    CM22 = M11*M33 - M13*M31,

    CM23 = M12*M31 - M11*M32 ,

    CM24 = 0.0,


    CM31 = M12*M23 - M13*M22,

    CM32 = M13*M21 - M11*M23,

    CM33 = M11*M22 - M12*M21,

    CM34 = 0.0,


    CM41 = Tx*M23*M32 + M13*M22*Tz + M12*Ty*M33
        - M12*M23*Tz - M13*Ty*M32 - Tx*M22*M33,

    CM42 =  M11*M23*Tz + M13*Ty*M31 + Tx*M21*M33
        - Tx*M23*M31 - M13*M21*Tz - M11*Ty*M33,

    CM43 = Tx*M22*M31 + M12*M21*Tz + M11*Ty*M32
        - M11*M22*Tz - M12*Ty*M31 - Tx*M21*M32,

    CM44 = M11*M22*M33 + M12*M23*M31 + M13*M21*M32
        - M13*M22*M31 - M12*M21*M33 - M11*M23*M32,

    #matrix4{ m11=CM11, m12=CM12, m13=CM13, m14=CM14,
              m21=CM21, m22=CM22, m23=CM23, m24=CM24,
              m31=CM31, m32=CM32, m33=CM33, m34=CM34,
              m41=CM41, m42=CM42, m43=CM43, m44=CM44 }.



-doc """
Returns the inverse of the specified matrix, if it is invertible (that is iff
its determinant is non-null), otherwise returns undefined.
""".
-spec inverse( matrix4() ) -> option( matrix4() ).
% Special cases as the inverse of a compact_matrix is another one (even if the
% intermediary comatrix is generally not one):
%
inverse( M=identity_4 ) ->
    M;

inverse( M ) when is_record( M, matrix4 ) ->
    Det = determinant( M ),
    case math_utils:is_null( Det ) of

        true ->
            cond_utils:if_defined( myriad_debug_linear,
                trace_utils:warning_fmt( "Cannot determine the inverse of the "
                    "following matrix, considered singular: ~ts",
                    [ to_string( M ) ] ) ),
            undefined;

        false ->
            scale( transpose( comatrix( M ) ), 1/Det )

    end;

% Special-cased as performs less operations, and returns a compact form:
inverse( M ) when is_record( M, compact_matrix4 ) ->
    Det = determinant( M ),
    case math_utils:is_null( Det ) of

        true ->
            cond_utils:if_defined( myriad_debug_linear,
                trace_utils:warning_fmt( "Cannot determine the inverse of the "
                    "following compact matrix, considered singular: ~ts",
                    [ to_string( M ) ] ) ),
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




% Section dedicated to homogeneous matrices.


-doc """
Returns the 4x4 compact matrix obtained from the specified 3x3 matrix (M) and 3D
(translation) vector (V).
""".
-spec from_3D( M :: matrix3(), V :: vector3() ) -> compact_matrix4().
% Never returning identity_4, hence compact_matrix4() is more precise than
% homogeneous_matrix4().
%
from_3D( _M=#matrix3{ m11=M11, m12=M12, m13=M13,
                   m21=M21, m22=M22, m23=M23,
                   m31=M31, m32=M32, m33=M33 },
         _V=[ X, Y, Z ] ) ->
    #compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=X,
                      m21=M21, m22=M22, m23=M23, ty=Y,
                      m31=M31, m32=M32, m33=M33, tz=Z };

% Not specifically managing identity_3:
from_3D( OtherMatrix3, Vec3 ) ->
    CanOtherMatrix3 = matrix3:to_canonical( OtherMatrix3 ),
    from_3D( CanOtherMatrix3, Vec3 ).



-doc """
Returns the top-left 3x3 matrix (M) and the rightmost 3D vector (V) obtained
from the specified 4x4 homogeneous matrix.
""".
-spec to_3D( homogeneous_matrix4() ) -> { M :: matrix3(), V :: vector3() }.
to_3D( identity_4 ) ->
    { _M=identity_3, _V=vector3:null() };

to_3D( #compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
                         m21=M21, m22=M22, m23=M23, ty=Ty,
                         m31=M31, m32=M32, m33=M33, tz=Tz } ) ->
    M = #matrix3{ m11=M11, m12=M12, m13=M13,
                  m21=M21, m22=M22, m23=M23,
                  m31=M31, m32=M32, m33=M33 },

    V = [ Tx, Ty, Tz ],

    { M, V }.



-doc "Returns the canonical form of the specified 4x4 matrix.".
-spec to_canonical( matrix4() ) -> canonical_matrix4().
to_canonical( #compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=Tx,
                                m21=M21, m22=M22, m23=M23, ty=Ty,
                                m31=M31, m32=M32, m33=M33, tz=Tz } ) ->
    Zero = 0.0,
    #matrix4{ m11=M11,  m12=M12,  m13=M13,  m14=Tx,
              m21=M21,  m22=M22,  m23=M23,  m24=Ty,
              m31=M31,  m32=M32,  m33=M33,  m34=Tz,
              m41=Zero, m42=Zero, m43=Zero, m44=1.0 };

to_canonical( identity_4 ) ->
    Zero = 0.0,
    One = 1.0,
    #matrix4{ m11=One,  m12=Zero, m13=Zero, m14=Zero,
              m21=Zero, m22=One,  m23=Zero, m24=Zero,
              m31=Zero, m32=Zero, m33=One,  m34=Zero,
              m41=Zero, m42=Zero, m43=Zero, m44=One };

to_canonical( M ) when is_record( M, matrix4 ) ->
    M.



-doc """
Returns the compact form of the specified (4x4) matrix.

Throws an exception if the specified matrix cannot be expressed as a compact
one.
""".
-spec to_compact( matrix4() ) -> compact_matrix4().
to_compact( M=#matrix4{ m11=M11, m12=M12, m13=M13, m14=M14,
                        m21=M21, m22=M22, m23=M23, m24=M24,
                        m31=M31, m32=M32, m33=M33, m34=M34,
                        m41=M41, m42=M42, m43=M43, m44=M44 } ) ->
    case is_null( M41 ) andalso is_null( M42 ) andalso is_null( M43 )
            andalso are_close( M44, 1.0 ) of

        true ->
            % Just drop the last row then:
            #compact_matrix4{ m11=M11, m12=M12, m13=M13, tx=M14,
                              m21=M21, m22=M22, m23=M23, ty=M24,
                              m31=M31, m32=M32, m33=M33, tz=M34 };

        false ->
            trace_utils:error_fmt( "Canonical 4D matrix ~ts cannot be "
                "expressed as a compact one.", [ to_string( M ) ] ),

            throw( { not_compactable, M } )

    end;

to_compact( identity_4 ) ->
    Zero = 0.0,
    One = 1.0,
    #compact_matrix4{ m11=One,  m12=Zero, m13=Zero, tx=Zero,
                      m21=Zero, m22=One,  m23=Zero, ty=Zero,
                      m31=Zero, m32=Zero, m33=One,  tz=Zero };

to_compact( CM ) when is_record( CM, compact_matrix4 ) ->
    CM.



-doc """
Returns a matrix corresponding to the specified tuple-based one.

The elements are expected to be already floats().
""".
-spec from_tuple( tuple_matrix4() ) -> matrix4().
from_tuple( Tuple ) when is_tuple( Tuple ) andalso size( Tuple ) =:= 12 ->
    erlang:insert_element( _Index=1, Tuple, compact_matrix4 );

from_tuple( Tuple ) when is_tuple( Tuple ) andalso size( Tuple ) =:= 16 ->
    erlang:insert_element( _Index=1, Tuple, matrix4 ).



-doc "Returns a tuple-based version of the specified matrix.".
-spec to_tuple( matrix4() ) -> tuple_matrix4().
to_tuple( M=identity_4 ) ->
    to_tuple( to_compact( M ) );

% Here either a canonical_matrix4() | compact_matrix4() record:
to_tuple( M ) ->
    % Just chop the record tag, returning either m16() or m12():
    erlang:delete_element( _Index=1, M ).



-doc "Checks that the specified matrix is legit, and returns it.".
-spec check( matrix4() ) -> matrix4().
check( M=identity_4 ) ->
    M;

check( M ) ->
    Coords = case tuple_to_list( M ) of

        [ matrix4 | Cs ] ->
            Cs;

        [ compact_matrix4 | Cs ] ->
            Cs

    end,

    [ type_utils:check_float( C ) || C <- Coords ],

    M.



-doc "Returns a textual representation of the specified (4x4) matrix.".
-spec to_string( matrix4() ) -> ustring().
to_string( _Matrix=identity_4 ) ->
    "4x4 identity";

to_string( Matrix4=#matrix4{} ) ->

    [ _AtomMatrix4 | AllCoords ] = tuple_to_list( Matrix4 ),

    Strs = linear:coords_to_best_width_strings( AllCoords ),

    % No need for ~ts here:
    RowFormatStr = "[ " ++ text_utils:duplicate( ?dim, "~s " ) ++ "]~n",

    FormatStr = "~n" ++ text_utils:duplicate( ?dim, RowFormatStr ),

    text_utils:format( FormatStr, Strs );


to_string( CptMatrix=#compact_matrix4{} ) ->

    [ _AtomCompactMatrix4 | CompactCoords ] = tuple_to_list( CptMatrix ),

    AllCoords = CompactCoords ++ [ 0.0, 0.0, 0.0, 1.0 ],

    Strs = linear:coords_to_best_width_strings( AllCoords ),

    % No need for ~ts here:
    RowFormatStr = "[ " ++ text_utils:duplicate( ?dim, "~s " ) ++ "]~n",

    FormatStr = "~n" ++ text_utils:duplicate( ?dim, RowFormatStr ),

    text_utils:format( FormatStr, Strs ).
