% Copyright (C) 2022-2026 Olivier Boudeville
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

-module(transform4).

-moduledoc """
Module implementing the support for **4x4 transformations** (hence for
3D computations), based on pairs (in logical terms - these are actually
records) made of an homogeneous matrix and its inverse, which are updated and
maintained through the operations applied to them.

The operations triggered on a transformation (the said matrix pair) are the
same (name, arguments and their order, contract) that would be triggered
directly on its reference matrix only.

So, instead of a matrix M, a transformation can be seen (logically) as a
{M,InvM} pair, and instead of applying for example an operation O (whose
matrix is Mo) on M with M' = M.Mo, we have here T' = T.Mo = {M.Mo,
InvMo.InvM}.

See also `matrix4.erl`, as this module offers a larger subset of its API (with
any "homogeneous" removed in the function names, as it is implicit here). For
non-covered functions, just get/set the reference/inverse matrices that a
transform4 contains.
""".



% Implementation notes:
%
% Requiring the inverse of an invertible logical transformation happens
% frequently, so our actual transformations are a pair of matrices: a given
% reference matrix and its precomputed inverse; they are both to be updated
% based on each operation applied: the reference by this actual operation and
% the other by its reciprocal (inverse operation applied on the other side), so
% that either remains directly available with no further computation (and that
% of course they remains inverse matrices).
%
% Said differently, thanks to these reciprocal updates, the inverse of a logical
% transformation is always readily available, while never being explicitly
% computed from scratch (which would be expected to be costlier).
%
% If such operations may not be invertible or even linear, we consider here that
% these are actually 3D operations represented thanks to 4D homogeneous matrices
% that may be invertible (e.g. for translations).
%
% The various types of matrix4 (canonical, compact, identity) are transparently
% managed.
%
% We suppose that storing only homogeneous_matrix4 instances (not matrix4
% instances) is sufficient for all uses.
%
% Inverse transformations are needed in many cases, especially when dealing with
% scene graphs (refer to the reference_tree module): if, to compute a transition
% matrix from a reference frame R1 to another R2, only the reference matrices
% from R1 to the closest parent of R1 and R2 are needed, conversely, from this
% parent to R2, we require this time the inverse matrices; these shall thus be
% directly available as well.


% Understanding the role and composition of transformations.
%
% A transformation (e.g. T12) conveys how elements expressed in a coordinate
% system R1 can be expressed in a coordinate system R2, and vice versa: thanks
% to its pair of reference (RefM) / inverse (InvM) matrices, a transformation
% provides a two-way conversion between frames of reference:
%
% - by our conventions, the reference matrix is the transition matrix from R1 to
% R2: a vector expressed in R1 as V1 will be expressed in R2 as V2 = RefM.V1
%
% - conversely, the inverse matrix takes in charge the transition from R2 to
% R1: a vector expressed in R2 as V2 will be expressed in R1 as V1 = InvM.V2.
%
% For example let's suppose that we are in a R1 reference frame, defined
% relatively to another reference frame R2 (noted R1->R2: R1 depends on R2, R1
% being typically represented as a child of R2, hence lower than R2 in the
% reference tree) by a translation of 5 along its X axis (R1 can thus be seen as
% the translated system here). Let's suppose that we have 3D content defined in
% R1, but we need to have them in R2; we are thus interested here in T12 (whence
% of course its inverse T21 can be easily obtained).
%
% Then T12=transform4:translation(_VT=[5,0,0]) will correspond to a
% transformation whose reference matrix RefM is the identity_4 matrix except for
% its top-right element, equal to +5. Applying (on the right) to RefM a vector
% of R1 V1=[X,Y,Z] will indeed result in a vector of R2, V2 = RefM.V1 =
% [X+5,Y,Z].
%
%
% Regarding composition, transformations can be chained by multiplying them on
% the right, based on a parent-to-child order.
%
% For example, if R3 is a base coordinate system, R2 is defined relatively to
% R3, and R1 relatively to R2 (noted R1->R2->R3), and if the T12 and T23
% transformations have been defined like explained just above, then a vector
% expressed in R1 as V1 will be expressed in R3 as V3 = T23_RefM.T12_RefM.V1.
% Conversely, V1 = T21_InvM.T32_InvM.V3 (as (Ma.Mb)^-1 = Mb^-1.Ma^-1).

% Usual transformations.
%
% 3D models (e.g. one of a type of spaceship) are generally defined centered in
% a local coordinate system Rl of their own (as a geometry, which may be common
% to multiple instances), whereas each model instance (a specific, actual
% spaceship) has its own geometrical information (e.g. positioning) computed. So
% a given spaceship may be known to be at a given point, pointing to a given
% direction, and being potentially scaled (defining its own frame of reference)
% - all that relatively to, ultimately, some more global frame of reference Rg
% (e.g. some galactic center).
%
% To place correctly such a model in Rg in one go, a suitable transformation may
% be devised. To convert the coordinates of a point (e.g. the tip of the
% cockpit) Pl in Rl to the ones (Pg) of that point in Rg, we can define the Tlg
% transformation, whose reference matrix RefM=Pl->g allows to determine Pg =
% RefM.Pl.
%
% For that, Tlg.RefM = Pl->g = TrM.RotM.SclM is the transition matrix from l to
% g, where SclM applies the scaling (as determined by three scaling factors
% along the axes of Rl), RotM the rotation (as determined by a unit axis and an
% angle; the corresponding matrix has for columns the axes of Rl expressed in
% Rg) and TrM the translation (as determined by Ol/g, i.e. a vector of Rg
% pointing to the origin of Rl).
%
% This is the recommended order of the operations, otherwise these
% transformations would become coupled and would interfere negatively (e.g. a
% translation vector would be scaled as well).
%
% To help reasoning about coordinate systems, juste consider a base, parent R2
% one, and a child, R1 one, just translated of [+5,0,0]; conversions of
% coordinates are then just a matter of carefully adding or subtracting 5.
%
% To parameter a transformation T12, elements (vectors, angles, etc.) can be
% defined either in R1 or R2. Although using as reference R1 could seem more
% logical, knowing that generally R1->R2, we prefer defining transformations in
% R2, i.e. in the parent (rather than in the child).
%
% More precisely, for each transformation T12 we have (A) to define it from
% parameters and (B) to use it (hence there are two different steps to manage,
% possibly with different needs/conventions). Let's suppose that, following our
% conventions, R1->R2 (e.g. R1 is attached to a spaceship and R2 is absolute; so
% we typically have R2 and wants to define R1 relatively to it) and that we
% consider a point P whose coordinates in R1 are P1, and in R2 are P2.
%
% In general terms, if for a given operation we start from R2 to derive R1, then
% to convert points in R1 to point in R2 we have to apply the opposite
% operation.
%
%
% For a translation:
%
%  - A: T12 is typically used to specify/support R1, and this transformation can
%  be defined only with respect to R2; so the translation vector VT used to
%  create this transformation should correspond to the coordinates of the origin
%  of R1 (child) expressed in R2 (parent)
%
%  - B: P2 = P1->2.P1 = RefM.P1, and also P2 = P1+VT
%
% So for a translation, RefM simply has to include VT as expressed in R2
% (e.g. not -VT / not VT expressed in R1) in its rightmost column; another view
% on this is that we defined R1 from R2 by adding VT'=-VT to convert coordinates
% from R2 to R1, and thus P1->2 should subtract VT' from coordinates of R1 -
% thus adding VT.
%
%
% For a rotation:
%
%  - A: T12 is typically used to specify/support R1, and this transformation can
%  be defined only with respect to R2; so the rotation vector (and angle) should
%  be considered in R2: R1 is obtained from R2 by rotating of an angle alpha
%  around a unit vector V2 defined in R2
%
%  - B: P2 = P1->2.P1 = RefM.P1, and also P2 = Rot(V2,-alpha)
%
% So for a rotation, RefM simply corresponds to the rotation of the opposite
% angle.
%
%
% For a scaling:
%
% We ultimately want that all distances along a given axis (say X) get
% multiplied at step B by a factor f: P2 = {X2,Y2,Z2} = {f.X1,Y1,Z1}; as P2 =
% P1->2.P1 = RefM.P1, P1->2 just has to use directly the expected scaling
% factors (e.g. not their inverse).
%
% Note that the system in which some vectors above are defined is somewhat
% implicit (e.g. is the rotation axis defined in source or target system? Does
% the scaling happens along the source axes or the target ones?). Its depends on
% the "current" system when these operations are applied, i.e. of the operation
% order (see TrM.RotM.SclM above).
%
% Note also that a transformation can be seen as an action (e.g. scale the
% coordinates of that matrix) or a transition between coordinate systems. For
% example, a scaling transformation of a uniform factor S can also be
% interpreted to be a transition to a coordinate system whose axes are scaled of
% a factor 1/S.
%
% Care must also be taken with scalings, as their transformation do not involve
% orthonormal matrices anymore, losing some properties (e.g. inverse of an
% orthonormal matrix being its transpose).


% For records like matrix4:
-include("matrix4.hrl").

% For the #transform4 record:
-include("transform4.hrl").


-doc """
A 4x4 transformation, storing both its corresponding homogeneous 4x4 reference
matrix and its inverse.

Typically designated as T12, the transformation between a frame of reference R1
to another R2 one, storing both its corresponding reference 4x4 matrix (P1->2,
transition from R1 to R2) and its inverse (P2->1, transition from R2 to R1).

We tend to prefer (by convention) "upward" transformations in a reference tree,
i.e the transformations from child to parent (rather than the opposite -
although one can be easily deduced from the other), as then it is directly the
reference matrix of T12 (RefM) that can be used to convert a vector expressed in
the child (R1) into its representation in the parent (R2), as V2 = RefM.V1.
""".
-type transform4() :: #transform4{}.


-export_type([ transform4/0 ]).



-export([ identity/0, new/1, new/2,

          get_reference/1, get_inverse/1,
          inverse/1,

          get_origin/1,

          translation/1, rotation/2, scaling/1, sc_rot_tr/4,
          transition/4,

          translate_left/2, translate_right/2,
          rotate_left/3, rotate_right/3,
          scale_left/2, scale_right/2,
          scale_x/2, scale_y/2, scale_z/2,
          basis_change/3,
          %from_columns/4, compact_from_columns/4, from_rows/4,
          %from_coordinates/16, from_compact_coordinates/12,
          %from_arbitrary/1, to_arbitrary/1, from_3D/2,
          dimension/0, dimensions/0,
          scale/2, %add/2, sub/2,
          apply_left/2, apply_right/2,
          mult/1, mult/2,
          are_equal/2,
          determinant/1,

          is_transform4/1,
          check_type/1, check/1,
          to_string/1 ] ).


-define( dim, 4 ).


% Type shorthands:

-type ustring() :: text_utils:ustring().

-type factor() :: math_utils:factor().

-type radians() :: unit_utils:radians().

-type dimension() :: linear:dimension().
-type scalar() :: linear:scalar().

-type point3() :: point3:point3().

-type vector3() :: vector3:vector3().
-type unit_vector3() :: vector3:unit_vector3().

-type dimensions() :: matrix:dimensions().

-type user_matrix4() :: matrix4:user_matrix4().
-type matrix4() :: matrix4:matrix4().

% Here scale factors are expected to be non-null, otherwise no relevant
% transformation can be defined (not invertible):.
%
-type scale_factors() :: matrix4:scale_factors().

-type homogeneous_matrix4() :: matrix4:homogeneous_matrix4().

-type transition_matrix4() :: matrix4:transition_matrix4().



% No sensible null/0 here.


-doc "Returns the 4x4 identity transformation.".
-spec identity() -> transform4().
identity() ->
    #transform4{}.



-doc """
Returns the 4x4 transformation whose reference matrix is the specified one.
""".
-spec new( user_matrix4() | matrix4() ) -> transform4().
new( _M=identity_4 ) ->
    #transform4{};

new( CM ) when is_record( CM, compact_matrix4 ) ->
    T = case matrix4:inverse( CM ) of

        undefined ->
            throw( { non_invertible_matrix, CM } );

        InvCM ->
            #transform4{ reference=CM, inverse=InvCM }

    end,

    cond_utils:assert( myriad_check_linear, check( T ) ),

    T;

new( UserMatrix ) ->
    M = matrix4:new( UserMatrix ),
    new( matrix4:to_compact( M ) ).



-doc """
Returns the 4x4 transformation whose reference matrix and inverse one are
directly the specified ones.
""".
-spec new( homogeneous_matrix4(), homogeneous_matrix4() ) -> transform4().
new( HM, InvHM ) ->

    T = #transform4{ reference=HM, inverse=InvHM },

    cond_utils:assert( myriad_check_linear, check( T ) ),

    T.



-doc """
Returns the (4x4) reference matrix corresponding to the specified 4x4
transformation.
""".
-spec get_reference( transform4() ) -> homogeneous_matrix4().
get_reference( #transform4{ reference=HM } ) ->
    HM.



-doc """
Returns the inverse of the (4x4) matrix corresponding to the specified 4x4
transformation.
""".
-spec get_inverse( transform4() ) -> homogeneous_matrix4().
get_inverse( #transform4{ inverse=InvHM } ) ->
    InvHM.



-doc """
Returns the inverse transform of the specified one (that is the transform from
the destination to the source of the specified one: from Tab, returns Tba).
""".
-spec inverse( transform4() ) -> transform4().
inverse( #transform4{ reference=HM, inverse=InvHM } ) ->
    % As simple as a swap:
    #transform4{ reference=InvHM, inverse=HM }.



-doc """
Returns, based on the specified transformation from a coordinate system (an
orthonormal basis) R1 to a R2 one (T12), the origin of R1 as expressed in R2.

So, if R1->R2 (R1 is a child reference frame of R2), this function returns the
coordinates of the child (R1) in its parent (R2).

Just negate this vector to obtain the origin of R2 as expressed in R1 (origin of
the parent expressed in the child).
""".
-spec get_origin( transform4() ) -> point3().
% Refer to
% http://howtos.esperide.org/ThreeDimensional.html#computing-transition-matrices
% for further details:
%
get_origin( _HM=#transform4{ reference=M } ) ->
    % No silly inlining:
    matrix4:get_translation( M ).



-doc """
Returns the 4x4 transformation (e.g. T12) corresponding to a translation of the
specified (3D) vector: its reference matrix will be P1->2, the transition matrix
from the current coordinate system (R1) to the translated one (R2), and of
course its inverse matrix will implement to opposite transition (from the
translated to the current, P2->1).

So VT is here the coordinates of the origin of R1 as expressed in R2 (we prefer
expressing the transformation parameters in R2 rather than in R1, should R1->R2,
i.e. expressing the child in its parent, rather than the opposite).

Said differently, the origin of R2 expressed in R1 is -VT.

Refer to the "Understanding the role and composition of transformations" section
for more details.
""".
-spec translation( vector3() ) -> transform4().
translation( VT ) ->

    % P1->2, hence a compact matrix:
    HM = matrix4:translation( VT ),

    % Inverse of a translation of VT is one of -VT:
    InvHM = matrix4:translation( vector3:negate( VT ) ),

    T = #transform4{ reference=HM, inverse=InvHM },

    cond_utils:assert( myriad_check_linear, check( T ) ),

    T.



-doc """
Returns the 4x4 transformation corresponding to a rotation of the specified
angle around the 3D axis specified as a unit vector (and to no translation or
scaling): its reference matrix will be the transition matrix from the rotated
coordinate system (R1) to the current one (R2) (and of course its inverse matrix
will implement to opposite transition, from R2 to R1).

This will be a counterclockwise rotation for an observer placed so that the
specified axis points towards it.

Note that this is not the general case of a rotation in 4D (which is of little
use, at least here); this corresponds to (4x4) homogeneous matrices.

Returns thus T12, the transformation between R1 and R2, for which the reference
matrix is P1->2, corresponding to a rotation of the opposite angle around the
specified axis.

If for example R1 is defined relatively to R2 (R1->R2) based on a rotation whose
(unit) axis in R2 is [1,0,0] of an angle of +90°, a point P whose coordinates in
R2 are P2={0,1,0} (corresponding to its Y axis) will have for coordinates in R1
P1={0,0,-1} (opposite of its Z axis).

We prefer expressing the transformation parameters in R2 rather than in R1,
should R1->R2 (i.e. expressing the child in its parent, rather than the
opposite).

Refer to the "Understanding the role and composition of transformations" section
for more details.
""".
-spec rotation( unit_vector3(), radians() ) -> transform4().
rotation( UnitAxis, RadAngle ) ->

    HM = #compact_matrix4{ m12=M12, m13=M13,
                           m21=M21, m23=M23,
                           m31=M31, m32=M32 }
       = matrix4:rotation( UnitAxis, RadAngle ),

    % More expensive:
    %InvHM = matrix4:rotation( UnitAxis, RadAngle ),

    % A transpose of the 3D part should do the trick as well; just having to
    % mirror-swap the extra-diagonal terms of the inner 3x3 matrix, knowing
    % that:
    % HM = #compact_matrix4{ m11, m12, m13, tx=Zero,
    %                        m21, m22, m23, ty=Zero,
    %                        m31, m32, m33, tz=Zero }.
    %
    InvHM = HM#compact_matrix4{ m12=M21, m13=M31,
                                m21=M12, m23=M32,
                                m31=M13, m32=M23 },

    T = #transform4{ reference=HM, inverse=InvHM },

    cond_utils:assert( myriad_check_linear, check( T ) ),

    T.



-doc """
Returns the 4x4 transformation corresponding to the scaling of the specified
(supposedly non-null) factors: its reference matrix can be interpreted:

- either as the transition matrix from the scaled coordinate system to the
current one (and of course its inverse matrix implementing to opposite
transition, from current to scaled); we prefer expressing the transformation
parameters in R2 rather than in R1, should R1->R2 (i.e. expressing the child in
its parent, rather than the opposite); so for example if all scale factors are
equal to S, then a [X,Y,Z] point in R1 will be transformed in [S.X, S.Y, S.Z] in
R2 (not [X/S, Y/S, Z/S]); said differently, the length of the axes of R1 being
multiplied by S, the coordinates expressed in it shall be divided by S, so that
the position of points does not change; and thus a point P expressed as [X/S,
Y/S, Z/S] in R1 becomes [X,Y,Z] in R2

- or, maybe more often, as a scaling operation applying directly the specified
factors (not their opposite)

Due to homogeneous matrices, the inverse of a scaling matrix is not the matrix
of the inverse factors, this function is thus not a proper transform. See
scale_{left,right}/2 instead.

Refer to the "Understanding the role and composition of transformations" section
for more details.
""".
-spec scaling( scale_factors() ) -> transform4().
scaling( ScaleFactors ) ->

   % Hence a compact matrix:
    HM = matrix4:scaling( ScaleFactors ),

    InvFactors = inverse_factors( ScaleFactors ),

    InvHM = matrix4:scaling( InvFactors ),

    T = #transform4{ reference=HM, inverse=InvHM },

    cond_utils:if_defined( myriad_check_linear, check( T ) ),

    T.



-doc "Returns the inverse of the specified factors.".
-spec inverse_factors( scale_factors() ) -> scale_factors().
inverse_factors( _Factors={ Sx, Sy, Sz } ) ->
    { 1/Sx, 1/Sy, 1/Sz }.



-doc """
Returns the 4x4 transformation corresponding to the scaling of the specified
factors, followed by a rotation of the specified axis and angle, and finally the
translation of the specified vector.

Refer to the 'Usual transformations' section for further details.
""".
-spec sc_rot_tr( scale_factors(), unit_vector3(), radians(), vector3() ) ->
                                        transition_matrix4().
sc_rot_tr( ScaleFactors, RotUnitAxis, RotRadAngle, TransVec ) ->

    % Let's R1 be the current coordinate system, and R2 the target one (R1->R2),
    % we are computing T12.
    %
    % For a vector expressed as V1 in R1 and to be expressed as V2 in R2, we
    % want V2 = RefM.V1 = TrM.RotM.SclM.V1.

    TScl = scaling( ScaleFactors ),
    TSclRot = rotate_left( RotUnitAxis, RotRadAngle, TScl ),
    _TSclRotTr = translate_left( TransVec, TSclRot ).



-doc """
Returns T12, the 4x4 transition transformation from the current orthonormal
basis (R1) to one (R2) in which the origin and axes of the current basis (R1)
are expressed; its reference matrix will be the transition matrix from R1 to R2
(and of course its inverse matrix will implement to opposite transition, from R2
to R1).

Refer to the "Understanding the role and composition of transformations" section
and to matrix4:transition/4 for further details.

""".
-spec transition( point3(), unit_vector3(), unit_vector3(), unit_vector3() ) ->
                                        transition_matrix4().
transition( Origin, X, Y, Z ) ->

    % Hence a compact matrix:
    HM = matrix4:transition( Origin, X, Y, Z ),

    % The inverse of a transition matrix [R|T] is [Rt|-Rt.T] where Rt is the
    % transpose of R; so:

    { R, T } = matrix4:to_3D( HM ),

    % As orthogonal:
    InvR = matrix3:transpose( R ),

    MinusInvT = matrix3:apply( InvR, T ),

    InvT = vector3:negate( MinusInvT ),

    % Hence a compact matrix:
    InvHM = matrix4:from_3D( InvR, InvT ),

    TRes = #transform4{ reference=HM, inverse=InvHM },

    cond_utils:if_defined( myriad_check_linear, check( TRes ) ),

    % TO-DO: fully inline the computation of InvHM, and check that it is equal
    % to the previous computation.

    TRes.



-doc """
Returns the dimension of these transformations.

Not useless, when using polymorphism based on module name.
""".
-spec dimension() -> dimension().
dimension() ->
    ?dim.



-doc """
Returns the dimensions of these transformations.

Not useless, when using polymorphism based on module name.
""".
-spec dimensions() -> dimensions().
dimensions() ->
    { ?dim, ?dim }.



-doc """
Returns the specified 4x4 transformation (T) once multiplied on its left by a
translation matrix (TM) corresponding to the specified vector (VT): returns
therefore T' = TM.T.

Corresponds to adding the specified translation vector to the right-most column
of the reference homogeneous matrix, and updating its inverse accordingly.
""".
-spec translate_left( vector3(), transform4() ) -> transform4().
translate_left( VT, #transform4{ reference=HM, inverse=InvHM } ) ->

    % So NewM = TrM.HM:
    NewHM = matrix4:translate_homogeneous_left( VT, HM ),

    MinusVT = vector3:negate( VT ),

    % So NewInvHM = InvHM.InvTrM; nothing simpler than:
    NewInvHM = matrix4:mult( InvHM, matrix4:translation( MinusVT ) ),

    T = #transform4{ reference=NewHM, inverse=NewInvHM },

    cond_utils:if_defined( myriad_check_linear, check( T ) ),

    T.



-doc """
Returns the specified 4x4 transformation (T) once multiplied on its right by a
translation matrix (TM) corresponding to the specified vector (VT): returns
therefore T' = T.TM.
""".
-spec translate_right( transform4(), vector3() ) -> transform4().
% Not helpful: corresponds to adding the opposite of the specified translation
% vector to the right-most column of the inverse homogeneous matrix, and
% updating its reference accordingly.
%
translate_right( #transform4{ reference=HM, inverse=InvHM }, VT ) ->

    % So NewHM = HM.TrM; nothing simpler than:
    NewHM = matrix4:mult( HM, matrix4:translation( VT ) ),

    % For the inverse now:
    MinusVT = vector3:negate( VT ),

    % So NewInvHM = InvTrM.InvHM:
    NewInvHM = matrix4:translate_homogeneous_left( MinusVT, InvHM ),

    T = #transform4{ reference=NewHM, inverse=NewInvHM },

    cond_utils:if_defined( myriad_check_linear, check( T ) ),

    T.


% No real meaning/use with homogeneous matrices: transpose/1.



-doc """
Updates the specified 4x4 transformation by applying on its left the specified
rotation: returns therefore T' = RotM.T.
""".
-spec rotate_left( unit_vector3(), radians(), transform4() ) -> transform4().
rotate_left( UnitAxis, RadAngle, #transform4{ reference=HM, inverse=InvHM } ) ->

    % NewHM = RotM.HM:
    NewHM = matrix4:rotate_homogeneous_left( UnitAxis, RadAngle, HM ),

    % NewInvM = InvHM.InvRotM, the inverse of a rotation being the opposite
    % angle around the same axis:
    %
    NewInvHM = matrix4:rotate_homogeneous_right( InvHM, UnitAxis, -RadAngle ),

    T = #transform4{ reference=NewHM, inverse=NewInvHM },

    cond_utils:if_defined( myriad_check_linear, check( T ) ),

    T.



-doc """
Updates the specified 4x4 transformation by applying on its right the specified
rotation: returns therefore T' = T.RotM.
""".
-spec rotate_right( transform4(), unit_vector3(), radians() ) -> transform4().
rotate_right( #transform4{ reference=HM, inverse=InvHM },
              UnitAxis, RadAngle ) ->

    % NewHM = HM.RotM:
    NewHM = matrix4:rotate_homogeneous_right( HM, UnitAxis, RadAngle ),

    % NewInvHM = InvRotM.InvHM, the inverse of a rotation being the opposite
    % angle around the same axis:
    %
    NewInvHM = matrix4:rotate_homogeneous_left( UnitAxis, -RadAngle, InvHM ),

    T = #transform4{ reference=NewHM, inverse=NewInvHM },

    cond_utils:if_defined( myriad_check_linear, check( T ) ),

    T.



-doc """
Updates the specified 4x4 transformation by applying on its left the scaling of
the specified factors: returns therefore T' = SclM.T.
""".
-spec scale_left( scale_factors(), transform4() ) -> transform4().
scale_left( ScaleFactors, #transform4{ reference=HM, inverse=InvHM } ) ->

    % NewHM = SclM.HM:
    NewHM = matrix4:scale_homogeneous_left( ScaleFactors, HM ),

    % NewInvM = InvHM.InvSclM, the inverse of a scaling being the scaling of
    % inverse factors:

    InvScaleFactors = inverse_factors( ScaleFactors ),
    NewInvHM = matrix4:scale_homogeneous_right( InvHM, InvScaleFactors ),

    T = #transform4{ reference=NewHM, inverse=NewInvHM },

    cond_utils:if_defined( myriad_check_linear, check( T ) ),

    T.



-doc """
Updates the specified 4x4 transformation by applying on its right the scaling of
the specified factors: returns therefore T' = T.SclM.
""".
-spec scale_right( transform4(), scale_factors() ) -> transform4().
scale_right( #transform4{ reference=HM, inverse=InvHM }, ScaleFactors ) ->

    % NewHM = HM.SclM:
    NewHM = matrix4:scale_homogeneous_right( HM, ScaleFactors ),

    % NewInvHM = InvSclM.InvHM, the inverse of a scaling being the scaling of
    % inverse factors:

    InvScaleFactors = inverse_factors( ScaleFactors ),
    NewInvHM = matrix4:scale_homogeneous_left( InvScaleFactors, InvHM ),

    T = #transform4{ reference=NewHM, inverse=NewInvHM },

    cond_utils:if_defined( myriad_check_linear, check( T ) ),

    T.



-doc """
Returns the specified 4x4 transformation once scaled by the specified (uniform,
non-null) factor.

Note that no left/right variations apply here.
""".
-spec scale( transform4(), factor() ) -> transform4().
scale( #transform4{ reference=HM, inverse=InvHM }, Factor ) ->

    NewHM = matrix4:scale_homogeneous( HM, Factor ),

    NewInvHM = matrix4:scale_homogeneous( InvHM, 1/Factor ),

    T = #transform4{ reference=NewHM, inverse=NewInvHM },

    cond_utils:if_defined( myriad_check_linear, check( T ) ),

    T.



-doc """
Updates the specified 4x4 transformation by applying the specified (uniform)
shearing factor on the leftmost column (X) of its reference matrix, like when
this matrix is multiplied on its right by a scaling matrix equal to the
identity, except for its first diagonal element, which would be equal to the
specified factor; returns therefore T' = T.SxM.
""".
-spec scale_x( transform4(), factor() ) -> transform4().
scale_x( #transform4{ reference=HM, inverse=InvHM }, Factor ) ->
    NewHM = matrix4:scale_homogeneous_x( HM, Factor ),

    % If HM' = HM.SxM, then InvHM' = InvSxM.InvHM; InvSxM is like SxM but with
    % an inverse factor, so (transposed function version to account for the
    % reversed multiplication order):
    %
    NewInvHM = matrix4:scale_homogeneous_x_t( InvHM, 1/Factor ),

    T = #transform4{ reference=NewHM, inverse=NewInvHM },

    cond_utils:if_defined( myriad_check_linear, check( T ) ),

    T.



-doc """
Updates the specified 4x4 transformation by applying the specified (uniform)
shearing factor on the second column (Y) of its reference matrix, like when this
matrix is multiplied on its right by a scaling matrix equal to the identity,
except for its second diagonal element, which would be equal to the specified
factor; returns therefore T' = T.SyM.
""".
-spec scale_y( transform4(), factor() ) -> transform4().
scale_y( #transform4{ reference=HM, inverse=InvHM }, Factor ) ->
    NewHM = matrix4:scale_homogeneous_y( HM, Factor ),

    % If HM' = HM.SyM, then InvHM' = InvSyM.InvHM; InvSyM is like SyM but with
    % an inverse factor, so (transposed function version to account for the
    % reversed multiplication order):
    %
    NewInvHM = matrix4:scale_homogeneous_y_t( InvHM, 1/Factor ),

    T = #transform4{ reference=NewHM, inverse=NewInvHM },

    cond_utils:if_defined( myriad_check_linear, check( T ) ),

    T.



-doc """
Updates the specified 4x4 transformation by applying the specified (uniform)
shearing factor on the third column (Z) of its reference matrix, like when this
matrix is multiplied on its right by a scaling matrix equal to the identity,
except for its third diagonal element, which would be equal to the specified
factor; returns therefore T' = T.SzM.
""".
-spec scale_z( transform4(), factor() ) -> transform4().
scale_z( #transform4{ reference=HM, inverse=InvHM }, Factor ) ->
    NewHM = matrix4:scale_homogeneous_z( HM, Factor ),

    % If HM' = HM.SzM, then InvHM' = InvSzM.InvHM; InvSzM is like SzM but with
    % an inverse factor, so (transposed function version to account for the
    % reversed multiplication order):
    %
    NewInvHM = matrix4:scale_homogeneous_z_t( InvHM, 1/Factor ),

    T = #transform4{ reference=NewHM, inverse=NewInvHM },

    cond_utils:if_defined( myriad_check_linear, check( T ) ),

    T.



-doc """
Applies the specified 3D point on the left of the reference matrix of the
specified transformation.

Note: handling a point, not a vector, so that the translation part of the
transformation is applied as well.
""".
-spec apply_left( point3(), transform4() ) -> point3().
apply_left( P3, #transform4{ reference=HM } ) ->
    matrix4:apply_homogeneous_left( P3, HM ).



-doc """
Applies the specified 3D point on the right of the reference matrix of the
specified transformation.

Note: handling a point, not a vector, so that the translation part of the
transformation is applied as well.
""".
-spec apply_right( transform4(), point3() ) -> point3().
apply_right( #transform4{ reference=HM }, P3 ) ->
    matrix4:apply_homogeneous_right( HM, P3 ).



-doc "Returns the determinant of the specified matrix.".
-spec determinant( transform4() ) -> scalar().
determinant( #transform4{ reference=HM } ) ->
    matrix4:determinant( HM ).



-doc """
Returns the (4x4) transformation corresponding to the in-order multiplication of
the two specified ones: returns therefore T = T1.T2.
""".
-spec mult( transform4(), transform4() ) -> transform4().
mult( _T1=#transform4{ reference=HM1, inverse=InvHM1 },
      _T2=#transform4{ reference=HM2, inverse=InvHM2 } ) ->
    M = matrix4:mult( HM1, HM2 ),
    InvM = matrix4:mult( InvHM2, InvHM1 ),
    T = #transform4{ reference=M, inverse=InvM },
    cond_utils:if_defined( myriad_check_linear, check( T ) ),
    T.



-doc """
Returns the (4x4) transformation corresponding to the in-order multiplication of
the specified ones.
""".
-spec mult( [ transform4() ] ) -> transform4().
mult( _Transforms=[ T1, T2 | T ] ) ->
    mult( mult( T1, T2 ), T );

mult( _Transforms=[ T ] ) ->
    T.



-doc "Tells whether the two specified (4x4) transformations are equal.".
-spec are_equal( transform4(), transform4() ) -> boolean().
are_equal( T1=#transform4{ reference=HM1, inverse=InvHM1 },
           T2=#transform4{ reference=HM2, inverse=InvHM2 } ) ->
    cond_utils:if_defined( myriad_check_linear,
        begin
            FirstTest = matrix4:are_equal( HM1, HM2 ),
            SecondTest = matrix4:are_equal( InvHM1, InvHM2 ),
            FirstTest =:= SecondTest orelse
                throw( { inconsistent_equality, T1, T2 } ),
            FirstTest
        end,
        begin
            basic_utils:ignore_unused( [ T1, T2, InvHM1, InvHM2 ] ),
            matrix4:are_equal( HM1, HM2 )
        end ).



-doc """
Returns a transition transformation T12 whose reference matrix is a
change-of-basis matrix from the current coordinate system (reference frame R1)
to the R2 one; the origin, forward (X axis) and up directions (Z axis) of R1 are
specified, relatively to R2 (typically R1->R2).

So returns, as reference matrix, P1->2, allowing, for an (homogeneous) point P,
to convert P1, its representation in current coordinate system R1, into P2, its
counterpart in R2: `P2 = P1->2 . P1`.

The inverse matrix of this transformation corresponds thus to P2->1.
""".
-spec basis_change( point3(), unit_vector3(), unit_vector3() ) -> transform4().
basis_change( O1InR2={ XO1, YO1, ZO1 }, FwdDir1InR2, UpDir1InR2 ) ->

    cond_utils:if_defined( myriad_debug_linear,
        trace_utils:debug_fmt( "Computing a transformation to a coordinate "
            "system located at ~ts, whose forward direction is ~ts and "
            "up direction is ~ts",
            [ point3:to_compact_string( O1InR2 ),
              vector3:to_compact_string( FwdDir1InR2 ),
              vector3:to_compact_string( UpDir1InR2 ) ] ),
        basic_utils:ignore_unused( O1InR2 ) ),


    % A point whose coordinates are to be converted from R1 to R2 shall first be
    % rotated, then translated; we determine here the axis vectors of R1, as
    % expressed in R2; refer to
    % https://howtos.esperide.org/ThreeDimensional.html#summary for more
    % information.

    % We now inline the definition of both matrices.

    % I, J, K: I for forward, K for upward, J determined from them.

    _X1InR2 = [ XI1, YI1, ZI1 ] = FwdDir1InR2,

    _Z1InR2 = [ XK1, YK1, ZK1 ] = UpDir1InR2,

    % Y = Z^X:
    % _Y1InR2 = [ XJ1, YJ1, ZJ1 ] = vector3:cross_product( Z1InR2, X1InR2 ),
    % Inlined:
    XJ1 = YK1*ZI1 - ZK1*YI1,
    YJ1 = ZK1*XI1 - XK1*ZI1,
    ZJ1 = XK1*YI1 - YK1*XI1,

    % Ref = matrix4:compact_from_columns( X1InR2, Y1InR2, Z1InR2, O1InR2 ),
    % Inlined:
    Ref = #compact_matrix4{ m11=XI1, m12=XJ1, m13=XK1, tx=XO1,
                            m21=YI1, m22=YJ1, m23=YK1, ty=YO1,
                            m31=ZI1, m32=ZJ1, m33=ZK1, tz=ZO1 },

    % Reversed, reciprocal operations; we compute the inverse of Ref by applying
    % https://howtos.esperide.org/ThreeDimensional.html#summary:

    % Negation of the scalar product of new rows with new origin:
    InvTx = - ( XI1*XO1 + YI1*YO1 + ZI1*ZO1 ),
    InvTy = - ( XJ1*XO1 + YJ1*YO1 + ZJ1*ZO1 ),
    InvTz = - ( XK1*XO1 + YK1*YO1 + ZK1*ZO1 ),

    InvRef = #compact_matrix4{ m11=XI1, m12=YI1, m13=ZI1, tx=InvTx,
                               m21=XJ1, m22=YJ1, m23=ZJ1, ty=InvTy,
                               m31=XK1, m32=YK1, m33=ZK1, tz=InvTz },

    #transform4{ reference=Ref, inverse=InvRef }.



-doc "Tells whether the specified term is a 4D transformation.".
-spec is_transform4( term() ) -> boolean().
is_transform4( Transf4 ) when is_record( Transf4, transform4 ) ->
    true;

is_transform4( _Other ) ->
    false.



-doc """
Checks that the specified term is a transform4, and returns it.

Does not check its consistency (see check/1).
""".
-spec check_type( term() ) -> transform4().
check_type( T ) when is_record( T, transform4 ) ->
    T;

check_type( Other ) ->
    throw( { not_transform4, Other } ).



-doc """
Checks that this transformation is consistent; throws an exception if not.
""".
-spec check( transform4() ) -> transform4().
check( T=#transform4{ reference=M, inverse=InvM } ) ->

    Mult = matrix4:mult( M, InvM ),

    matrix4:are_equal( Mult, matrix4:identity() ) orelse
        begin

            cond_utils:if_defined( myriad_debug_linear,
                trace_utils:error_fmt( "Not close enough to identity_4: ~ts",
                                       [ matrix4:to_string( Mult ) ] ) ),

            cond_utils:if_defined( myriad_debug_linear,
                trace_utils:error_fmt( "Reference: ~ts~nInverse: ~ts",
                    [ matrix4:to_string( M ), matrix4:to_string( InvM ) ] ) ),

            throw( { inconsistent_transform4, T, Mult } )

        end.



-doc "Returns a textual representation of the specified (4x4) transformation.".
-spec to_string( transform4() ) -> ustring().
to_string( #transform4{ reference=M, inverse=InvM } ) ->
    text_utils:format( "4x4 transformation recording reference matrix: ~ts and "
        "its inverse: ~ts",
        [ matrix4:to_string( M ), matrix4:to_string( InvM ) ] ).
