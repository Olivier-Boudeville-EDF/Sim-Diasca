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
% Creation date: Saturday, October 9, 2021.

-module(vector3).

-moduledoc """
Module implementing the support for **3D vectors**.

See also:
- the corresponding points (in `point3.erl`) and matrices (in `matrix3.erl`)
- the (unspecialised) vectors of arbitrary dimensions, in `vector.erl`
- the various 3D extra services in `linear_3D.erl`
""".



% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For the epsilon define:
-include("math_utils.hrl").


-doc """
A user-specified 3D vector, as a list (hence not a tuple) with 3 integer or
floating-point coordinates.
""".
-type user_vector3() :: [ any_coordinate() ].



-doc """
An (internal) 3D vector, with (exactly) 3 floating-point coordinates.

They are typically referenced as [X, Y, Z].
""".
-type vector3() :: [ coordinate() ].



-doc """
A 3D vector, with (exactly) 3 integer coordinates.

They are typically referenced as [X, Y, Z].
""".
-type integer_vector3() :: [ integer_coordinate() ].



-doc """
A 3D vector, with any (homogeneous) type of numerical coordinates (hence a
special case of user_vector3/0).

They are typically referenced as [X, Y, Z].
""".
-type any_vector3() :: vector3() | integer_vector3().



-doc """
A unit 3D vector, that is a vector of magnitude 1.0.

Defined for documentation purpose.
""".
-type unit_vector3() :: vector3().



-doc """
A vector in a 3D space, yet with Y-UP conventions (as opposed to Myriad's Z-UP
ones). Refer to the design notes in linear_3D.erl for further details.
""".
-type yup_vector3() :: vector3().



-doc """
A 3D vector orthogonal to a plane.

Defined for documentation purpose.
""".
-type normal3() :: vector3().



-doc """
A unit 3D vector orthogonal to a plane.

Defined for documentation purpose.
""".
-type unit_normal3() :: unit_vector3().



-doc "A 3D vector containing texture coordinates.".
-type texture_vector3() :: vector3().



-export_type([ user_vector3/0, vector3/0, integer_vector3/0, any_vector3/0,
               unit_vector3/0, yup_vector3/0,
               normal3/0, unit_normal3/0, texture_vector3/0 ]).


-export([ new/1, new/3, new_integer/3, null/0,
          x_axis/0, y_axis/0, z_axis/0,
          from_point/1, to_point/1,
          vector3_to_yup/1, yup_to_vector3/1,
          vector3_to_yups/1, yup_to_vector3s/1,
          add/2, add/1, subtract/2, cross_product/2,
          are_close/2, are_equal/2,
          square_magnitude/1, magnitude/1, negate/1, scale/2,
          normalise/1,
          dot_product/2,
          are_orthogonal/2, get_orthogonal/1, get_unit_orthogonal/2,
          check_orthogonal/2, compute_normal/3,
          is_unitary/1,
          check/1, check_vector/1, check_vectors/1,
          check_integer/1, check_unit_vector/1, check_unit_vectors/1,
          to_string/1, to_compact_string/1, to_basic_string/1,
          to_user_string/1, list_to_string/1 ] ).



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type factor() :: math_utils:factor().

-type coordinate() :: linear:coordinate().
-type integer_coordinate() :: linear:integer_coordinate().
-type any_coordinate() :: linear:any_coordinate().
-type user_coordinate() :: linear:user_coordinate().

-type distance() :: linear:distance().
-type any_square_distance() :: linear:any_square_distance().

-type any_point3() :: any_point3().
-type vertex3() :: point3:vertex3().



-doc "Returns a 3D vector corresponding to the user-specified one.".
-spec new( user_vector3() ) -> vector3().
%new( UserVector ) when is_tuple( UserVector ) ->
%   new( tuple_to_list( UserVector ) );
% Throws bad_generator anyway if a tuple:
new( UserVector ) -> %when is_list( UserVector ) ->
    [ type_utils:ensure_float( UC ) || UC <- UserVector ].



-doc "Returns a 3D vector corresponding to the user-specified one.".
-spec new( user_coordinate(), user_coordinate(), user_coordinate() ) ->
            vector3().
new( X, Y, Z ) ->
    [ type_utils:ensure_float( X ), type_utils:ensure_float( Y ),
      type_utils:ensure_float( Z ) ].



-doc "Returns an integer 3D vector corresponding to the user-specified one.".
-spec new_integer( integer_coordinate(), integer_coordinate(),
                   integer_coordinate() ) -> integer_vector3().
new_integer( X, Y, Z ) when is_integer( X ) andalso is_integer( Y )
                            andalso is_integer( Z ) ->
    [ X, Y, Z ].



-doc """
Returns the null (3D) vector, that is a 3D vector whose coordinates are all
null.
""".
-spec null() -> vector3().
null() ->
    Zero = 0.0,
    [ Zero, Zero, Zero ].



-doc """
Returns a 3D vector corresponding to the X axis of the current coordinate
system.
""".
-spec x_axis() -> vector3().
x_axis() ->
    Zero = 0.0,
    [ 1.0, Zero, Zero ].



-doc """
Returns a 3D vector corresponding to the Y axis of the current coordinate
system.
""".
-spec y_axis() -> vector3().
y_axis() ->
    Zero = 0.0,
    [ Zero, 1.0, Zero ].



-doc """
Returns a 3D vector corresponding to the Z axis of the current coordinate
system.
""".
-spec z_axis() -> vector3().
z_axis() ->
    Zero = 0.0,
    [ Zero, Zero, 1.0 ].



-doc "Returns a 3D vector corresponding to the specified 3D point.".
-spec from_point( any_point3() ) -> vector3().
from_point( P3 ) ->
    [ type_utils:ensure_float( C ) || C <- tuple_to_list( P3 ) ].



-doc "Returns a 3D point corresponding to the specified 3D vector.".
-spec to_point( any_vector3() ) -> any_point3().
to_point( V3 ) ->
    point3:from_vector( V3 ).



-doc """
Converts a usual 3D vector into one that follows the Y-UP convention (as opposed
to the Myriad Z-UP one, see the design notes in linear_3D).
""".
-spec vector3_to_yup( vector3() ) -> yup_vector3().
vector3_to_yup( _V=[X,Y,Z] ) ->
    [X,Z,-Y].



-doc """
Converts usual 3D vectors into ones that follow the Y-UP convention (as opposed
to the Myriad Z-UP one, see the design notes in linear_3D).
""".
-spec vector3_to_yups( [ vector3() ] ) -> [ yup_vector3() ].
vector3_to_yups( Vectors ) ->
    [ vector3_to_yup( V ) || V <- Vectors ].



-doc """
Converts a 3D following the Y-UP convention (as opposed to the Myriad Z-UP one,
see the design notes in linear_3D) into a usual one.
""".
-spec yup_to_vector3( yup_vector3() ) -> vector3().
yup_to_vector3( _V=[X,Y,Z] ) ->
    [X,-Z,Y].



-doc """
Converts a 3D following the Y-UP convention (as opposed to the Myriad Z-UP one,
see the design notes in linear_3D) into a usual one.
""".
-spec yup_to_vector3s( [ yup_vector3() ] ) -> [ vector3() ].
yup_to_vector3s( Vectors ) ->
    [ yup_to_vector3( V ) || V <- Vectors ].



-doc "Returns the sum of the two specified 3D vectors: `V = V1 + V2`.".
-spec add( vector3(), vector3() ) -> vector3().
add( _V1=[ X1, Y1, Z1 ], _V2=[ X2, Y2, Z2 ] ) ->
    [ X1+X2, Y1+Y2, Z1+Z2 ].



-doc """
Returns the sum of all 3D vectors in the specified (supposedly non-empty) list.
""".
-spec add( [ vector3() ] ) -> vector3().
add( _Vectors=[ VFirst | VOthers ]  ) ->
    lists:foldl( fun( [ X, Y, Z ], _AccVec=[ Xa, Ya, Za ] ) ->
                    [ X+Xa, Y+Ya, Z+Za ]
                 end,
                 _InitialAcc=VFirst,
                 _List=VOthers ).


-doc "Returns the difference of the two specified 3D vectors: `V = V1 - V2`.".
-spec subtract( vector3(), vector3() ) -> vector3().
subtract( _V1=[ X1, Y1, Z1 ], _V2=[ X2, Y2, Z2 ] ) ->
    [ X1-X2, Y1-Y2, Z1-Z2 ].



-doc """
Returns the cross-product of the two specified vectors (V1xV2; with French
conventions: V1^V2), that is the result of a regular 3D cross product of these
input vectors.
""".
-spec cross_product( vector3(), vector3() ) -> vector3().
cross_product( _V1=[X1,Y1,Z1], _V2=[X2,Y2,Z2] ) ->
    [ Y1*Z2 - Z1*Y2, Z1*X2 - X1*Z2, X1*Y2 - Y1*X2 ].



-doc """
Returns whether the two specified 3D vectors are close, that is if they could be
considered as representing the same vector (equality operator on vectors).
""".
-spec are_close( vector3(), vector3() ) -> boolean().
are_close( V1, V2 ) ->
    are_equal( V1, V2 ).



-doc """
Returns whether the two specified 3D vectors are equal, that is if they could be
considered as representing the same vector (equality operator on vectors).
""".
-spec are_equal( vector3(), vector3() ) -> boolean().
are_equal( _V1=[X1,Y1,Z1], _V2=[X2,Y2,Z2] ) ->
    math_utils:are_close( X1, X2 ) andalso math_utils:are_close( Y1, Y2 )
        andalso math_utils:are_close( Z1, Z2 ).



-doc "Returns the square of the magnitude of the 3D specified vector.".
-spec square_magnitude( vector3() ) -> any_square_distance().
square_magnitude( _V=[X,Y,Z] ) ->
    X*X + Y*Y + Z*Z.



-doc "Returns the magnitude of the specified 3D vector.".
-spec magnitude( vector3() ) -> distance().
magnitude( V ) ->
    math:sqrt( square_magnitude( V ) ).



-doc """
Negates the specified vector: returns the opposite one (of the same magnitude).
""".
-spec negate( vector3() ) -> vector3().
negate( _V=[X,Y,Z] ) ->
    [ -X, -Y, -Z ].



-doc "Scales the specified 3D vector of the specified scalar factor.".
-spec scale( any_vector3(), factor() ) -> vector3().
scale( _V=[X,Y,Z], Factor ) ->
    [ Factor*X, Factor*Y, Factor*Z ].



-doc """
Normalises the specified non-null 3D vector, that is returns it once scaled to
an unit length (whose magnitude is thus 1.0).

Could have been named get_unitary/1 as well.
""".
-spec normalise( vector3() ) -> unit_vector3().
normalise( V ) ->
    case magnitude( V ) of

        M when M < ?epsilon ->
            throw( cannot_normalise_null_vector );

        M ->
            scale( V, 1.0 / M )

    end.



-doc "Returns the dot-product of the two specified 3D vectors: `D = V1.V2`.".
-spec dot_product( vector3(), vector3() ) -> float().
dot_product( _V1=[ X1, Y1, Z1 ], _V2=[ X2, Y2, Z2 ] ) ->
    X1*X2 + Y1*Y2 + Z1*Z2.



-doc "Returns whether the two specified vectors are orthogonal.".
-spec are_orthogonal( vector3(), vector3() ) -> boolean().
are_orthogonal( V1, V2 ) ->
    math_utils:is_null( dot_product( V1, V2 ) ).



-doc """
Returns a vector (among an infinity) that is orthogonal to the specified
(non-null) vector.

The returned vector belongs to the plane orthogonal to the specified one.

Mostly useful for tests.
""".
-spec get_orthogonal( vector3() ) -> vector3().
get_orthogonal( V=[ A, B, C ] ) ->
    % We want a vector _Vo=[ X, Y, Z ] so that V.Vo=0, i.e. A.X + B.Y + C.Z = 0;
    % so:
    %
    case math_utils:is_null( C ) of

        true ->
            % C = 0 here.
            case math_utils:is_null( B ) of

                true ->
                    % B = 0, C = 0:
                    case math_utils:is_null( A ) of

                        true ->
                            throw( { null_vector, V } );

                        _False ->
                            % B = C = 0, so any Y and Z are adequate, provided
                            % that are not both null:
                            %
                            [ 0.0, 1.0, 1.0 ]

                        end;

                _False ->
                    % B =/= 0, C = 0, so any Z is adequate, and Y = -A.X/B for
                    % any X, so taking X=1:
                    %
                    [ 1.0, -A/B, 0.0 ]

            end;

        false ->
            % C =/= 0; Z = -(A.X+B.Y)/C for any X and Y; so we take X = Y = 1:
            [ 1.0, 1.0, -(A+B)/C ]

    end.



-doc """
Checks that the two specified vectors are orthogonal: throws an exception if
not.
""".
-spec check_orthogonal( vector3(), vector3() ) -> boolean().
check_orthogonal( V1, V2 ) ->
    are_orthogonal( V1, V2 ) orelse
        throw( { not_orthogonal, V1, V2, dot_product( V1, V2 ) } ).


-doc """
Projects the specified vector onto the specified target one, which is supposed
to be unitary.
""".
-spec project( vector3(), unit_vector3() ) -> vector3().
project( V, VTarget=[ X, Y, Z ] ) ->

    cond_utils:if_defined( myriad_check_linear,
                           check_unit_vector( VTarget ) ),

    DotP = dot_product( V, VTarget ),

    % A bit better than with scale/1:
    [ DotP*X, DotP*Y, DotP*Z ].



-doc """
Returns a version of the vector V that is orthogonal to Vbase.

Can be seen as applying a basic Gram-Schmidt process.
""".
-spec get_orthogonal( vector3(), unit_vector3() ) -> vector3().
get_orthogonal( V, Vbase ) ->
    % We simply remove the component of V that is colinear to Vbase, i.e. V -
    % ProjVbase(V):

    ProjVbase = project( V, Vbase ),
    subtract( V, ProjVbase ).



-doc """
Returns a version of the vector V that is orthogonal to Vbase and unitary.

Can be seen as applying a Gram-Schmidt process.
""".
-spec get_unit_orthogonal( vector3(), unit_vector3() ) -> unit_vector3().
get_unit_orthogonal( V, Vbase ) ->
    Orth = get_orthogonal( V, Vbase ),
    normalise( Orth ).



-doc """
Returns a unit vector that is orthogonal to the plane defined by the three
specified vertices.

If these vertices are listed in a CCW order as seen for a given viewpoint, then
the returned normal is pointing towards this viewpoint (i.e. this is an outward
normal if the specified points are vertices of a meshed object).
""".
-spec compute_normal( vertex3(), vertex3(), vertex3() ) -> unit_vector3().
compute_normal( V1, V2, V3 ) ->
    VecA = point3:vectorize( V1, V2 ),
    VecB = point3:vectorize( V2, V3 ),
    normalise( cross_product( VecA, VecB ) ).



-doc """
Returns whether the specified vector is unitary, that is whether it is of
magnitude 1.0.
""".
-spec is_unitary( vector3() ) -> boolean().
is_unitary( V ) ->
    % No specific need of computing the square root thereof:
    math_utils:are_equal( 1.0, square_magnitude( V ) ).



-doc "Checks that the specified 3D vector is legit, and returns it.".
-spec check( vector3() ) -> vector3().
check( V ) ->
    check_vector( V ).



-doc "Checks that the specified 3D vector is legit, and returns it.".
-spec check_vector( vector3() ) -> vector3().
check_vector( V ) ->
    3 = length( V ),
    type_utils:check_floats( V ).



-doc "Checks that the specified 3D vectors are legit, and returns them.".
-spec check_vectors( [ vector3() ] ) -> [ vector3() ].
check_vectors( Vs ) ->
    [ check_vector( V ) || V <- Vs ],
    Vs.



-doc "Checks that the specified 3D integer vector is legit, and returns it.".
-spec check_integer( integer_vector3() ) -> integer_vector3().
check_integer( V ) ->
    3 = length( V ),
    type_utils:check_integers( V ).



-doc "Checks that the specified 3D vector is normalised, and returns it.".
-spec check_unit_vector( vector3() ) -> unit_vector3().
check_unit_vector( V ) ->
    is_unitary( V ) orelse throw( { non_unitary_vector3, V, magnitude( V ) } ),
    V.



-doc "Checks that the specified 3D vectors are normalised, and returns them.".
-spec check_unit_vectors( [ vector3() ] ) -> [ unit_vector3() ].
check_unit_vectors( Vs ) ->
    [ check_unit_vector( V ) || V <- Vs ],
    Vs.



-doc """
Returns a textual representation of the specified 3D vector; full float
precision is shown.

For example, for a vector `V=[4.0, 3.142, -1.0]`, returns:
```
[  4.0  ]
[ 3.142 ]
[ -1.0  ]
```
""".
-spec to_string( user_vector3() ) -> ustring().
to_string( Vector ) ->
    to_user_string( Vector ).



-doc """
Returns a compact, textual, informal representation of the specified 3D vector.

For example, for a vector `V=[4.0, 3.142, -1.0]`, returns: `[4.0,3.142,-1.0]`.
""".
-spec to_compact_string( user_vector3() ) -> ustring().
to_compact_string( Vector ) ->

    %Ws = [ "~w" || _ <- Vector ],
    %FormatStr = "[ " ++ text_utils:join( _Sep=", ", Ws ) ++ " ]",
    %text_utils:format( FormatStr, Vector ).

    text_utils:format( "~w", [ Vector ] ).



-doc """
Returns a basic, not even fixed-width for floating-vector coordinates (see
linear.hrl for width and precision) representation of the specified 3D vector.

For example, for a vector `V=[4.0, 3.142, -1.0]`, returns:
```
[           4.00 ]
[           3.14 ]
[          -1.00 ]
```
""".
-spec to_basic_string( user_vector3() ) -> ustring().
to_basic_string( Vector ) ->

    % Vectors supposed to be lists of floats:
    ElemFormatStr = "[ " ++ ?coord_float_format ++ " ]~n",

    FormatStr = "~n" ++ text_utils:duplicate( 3, ElemFormatStr ),

    %trace_utils:debug_fmt( "FormatStr: ~ts; CoordList: ~w.",
    %                       [ FormatStr, CoordList ] ),

    text_utils:format( FormatStr, Vector ).



-doc """
Returns a textual, more user-friendly representation of the specified 3D vector;
full float precision is shown.

This is the recommended representation.

For example, for a vector `V=[4.0, 3.142, -1.0]`, returns:
```
[  4.0  ]
[ 3.142 ]
[ -1.0  ]
```
""".
-spec to_user_string( user_vector3() ) -> ustring().
to_user_string( Vector ) ->

    Strs = linear:coords_to_best_width_strings( Vector ),

    % No need for ~ts here:
    ElemFormatStr = "[ ~s ]~n",

    FormatStr = "~n" ++ text_utils:duplicate( 3, ElemFormatStr ),

    %trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
    %                       [ FormatStr, Strs ] ),

    text_utils:format( FormatStr, Strs ).



-doc """
Returns a textual representation of the specified list of 3D vectors; full float
precision is shown.
""".
-spec list_to_string( [ user_vector3() ] ) -> ustring().
list_to_string( Vectors ) ->
    VecStrs = [ to_string( V ) || V <- Vectors ],
    "~n" ++ text_utils:join( _Sep=",\n", VecStrs ).
