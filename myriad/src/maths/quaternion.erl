% Copyright (C) 2021-2022 Olivier Boudeville
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


% @doc Gathering of various facilities for <b>quaternion-related</b> operations.
-module(quaternion).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For the epsilon define:
-include("math_utils.hrl").


% For #matrix3:
-include("matrix3.hrl").

% For #matrix4:
-include("matrix4.hrl").



-type quaternion() :: { A :: coordinate(), B :: coordinate(),
						C :: coordinate(), D :: coordinate() }.
% Q = A + B.i + C.j + D.k, where i, j and k are the basic quaternions.
% A quaternion encodes an axis-angle representation of a 3D rotation.


-type unit_quaternion() :: quaternion().
% A unit quaternion, known as versor, is a quaternion of magnitude (norm) 1.0.
%
% For a unit quaternion, its inverse is its conjugate.
%
% Defined for documentation purpose.


-type vector_quaternion() :: quaternion().
% A quaternion whose scalar part (A) is null.
%
% Defined for documentation purpose.


-export_type([ quaternion/0, unit_quaternion/0, vector_quaternion/0 ]).


-export([ new/4, null/0, rotation/2,
		  add/2, mult/2,
		  are_close/2, are_equal/2,
		  square_magnitude/1, magnitude/1,
		  negate/1, scale/2, normalise/1, conjugate/1, inverse/1, rotate/2,
		  from_vector3/1, to_vector3/1,
		  to_matrix4/1, to_rot_matrix3/1,
		  is_unitary/1,
		  to_string/1, to_compact_string/1, to_user_string/1 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type factor() :: math_utils:factor().

-type radians() :: unit_utils:radians().

-type coordinate() :: linear:coordinate().
-type any_coordinate() :: linear:any_coordinate().

-type distance() :: linear:distance().
-type square_distance() :: linear:square_distance().

-type vector3() :: vector3:vector3().
-type unit_vector3() :: vector3:unit_vector3().

-type rot_matrix3() :: matrix3:rot_matrix3().

-type matrix4() :: matrix4:matrix4().


% Implementation notes:
%
% Sources:
%  - https://en.wikipedia.org/wiki/Quaternion
%  - https://www.omnicalculator.com/math/quaternion



% @doc Returns a corresponding (checked) quaternion.
-spec new( any_coordinate(), any_coordinate(), any_coordinate(),
		   any_coordinate() ) -> quaternion().
new( A, B, C, D ) ->
	{ type_utils:ensure_float( A ), type_utils:ensure_float( B ),
	  type_utils:ensure_float( C ), type_utils:ensure_float( D ) }.



% @doc Returns the null quaternion.
-spec null() -> quaternion().
null() ->
	Zero = 0.0,
	{ Zero, Zero, Zero, Zero }.



% @doc Returns the quaternion corresponding to a rotation of the specified angle
% around the axis specified as a unit vector.
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
% See also: rotate/2.
%
-spec rotation( unit_vector3(), radians() ) -> quaternion().
rotation( UnitAxis=[ Ux, Uy, Uz ], RadAngle ) ->

	% Not an assertion, as UnitAxis must be ignored if no check is done:
	cond_utils:if_defined( myriad_check_linear,
						   true = vector3:is_unitary( UnitAxis ),
						   basic_utils:ignore_unused( UnitAxis ) ),

	HalfAngle = RadAngle / 2.0,

	C = math:cos( HalfAngle ),
	S = math:sin( HalfAngle ),

	{ C, S*Ux, S*Uy, S*Uz }.



% @doc Returns the sum of the two specified quaternions: Q = Q1 + Q2.
%
% Addition is associative, commutative, and every quaternion Q has its opposite
% -Q.
%
-spec add( quaternion(), quaternion() ) -> quaternion().
add( _Q1={A1,B1,C1,D1}, _Q2={A2,B2,C2,D2} ) ->
	{ A1 + A2, B1 + B2, C1 + C2, D1 + D2 }.



% @doc Returns the (Hamilton) product of the two specified quaternions.
%
% This product is not commutative, but is associative.
%
% Multiplying imaginary quaternions corresponds to computing the cross-product
% of their coordinates.
%
% Dividing Q1 / Q2 would be multiplying Q1 by Q2^-1, the inverse of Q2, yet
% multiplication is not commutative so Q1 / Q2 could be either Q1*Q2^-1 or
% Q2^-1*Q1.
%
-spec mult( quaternion(), quaternion() ) -> quaternion().
mult( _Q1={A1,B1,C1,D1}, _Q2={A2,B2,C2,D2} ) ->

	A = A1*A2 - B1*B2 - C1*C2 - D1*D1,
	B = A1*B2 + B1*A2 + C1*D2 -D1*C2,
	C = A1*C2 - B1*D2 + C1*A2 + D1*B2,
	D = A1*D2 + B1*C2 - C1*B2 + D1*A2,

	{ A, B, C, D }.



% @doc Returns whether the two specified quaternions are close, that is if they
% could be considered as representing the same quaternion (equality operator on
% quaternions).
%
-spec are_close( quaternion(), quaternion() ) -> boolean().
are_close( Q1, Q2 ) ->
	are_equal( Q1, Q2 ).


% @doc Returns whether the two specified quaternions are equal, that is if they
% could be considered as representing the same quaternion (equality operator on
% quaternions).
%
-spec are_equal( quaternion(), quaternion() ) -> boolean().
are_equal( _Q1={A1,B1,C1,D1}, _Q2={A2,B2,C2,D2} ) ->
	math_utils:are_close( A1, A2 ) andalso math_utils:are_close( B1, B2 )
		andalso math_utils:are_close( C1, C2 )
		andalso math_utils:are_close( D1, D2 ).



% @doc Returns the square of the magnitude of the specified quaternion.
-spec square_magnitude( quaternion() ) -> square_distance().
square_magnitude( _Q={A,B,C,D} ) ->
	A*A + B*B + C*C + D*D.


% @doc Returns the magnitude of the specified quaternion.
-spec magnitude( quaternion() ) -> distance().
magnitude( Q ) ->
	math:sqrt( square_magnitude( Q ) ).



% @doc Negates the specified quaternion: returns the opposite one (of the same
% magnitude).
%
% See also: conjugate/1.
%
-spec negate( quaternion() ) -> quaternion().
negate( _Q={A,B,C,D} ) ->
	{ -A, -B, -C, -D }.



% @doc Scales the specified quaternion of the specified scalar factor.
-spec scale( quaternion(), factor() ) -> quaternion().
scale( _Q={A,B,C,D}, Factor ) ->
	{ Factor*A, Factor*B, Factor*C, Factor*D }.



% @doc Normalises the specified non-null quaternion, that is returns it once
% scaled to an unit length (whose magnitude is thus 1.0).
%
-spec normalise( quaternion() ) -> unit_quaternion().
normalise( Q ) ->
	case magnitude( Q ) of

		M when M < ?epsilon ->
			throw( cannot_normalise_null_quaternion );

		M ->
			scale( Q, 1.0 / M )

	end.



% @doc Returns the conjugate of the specified quaternion.
-spec conjugate( quaternion() ) -> quaternion().
conjugate( _Q={A,B,C,D} ) ->
	{ A, -B, -C, -D }.



% @doc Returns the inverse of the specified quaternion.
-spec inverse( quaternion() ) -> quaternion().
inverse( Q ) ->
	case square_magnitude( Q ) of

		SM when SM < ?epsilon ->
			throw( cannot_inverse_null_quaternion );

		SM ->
			scale( conjugate( Q ), 1.0 / SM )

	end.



% @doc Returns the specified vector once rotated according to the specified
% unitary quaternion.
%
% Another option, if multiple vectors have to be rotated, is to go through the
% corresponding 3x3 matrix, computed once for all:
%    matrix3:apply( quaternion:to_rot_matrix3( Q ), V )
%
-spec rotate( unit_quaternion(), vector3() ) -> vector3().
rotate( _Q={ A, B, C, D }, V ) ->

	cond_utils:if_defined( myriad_check_linear,
						   true = is_unitary( Q ) ),

	% Basic implementation:

	% QRotv = Q * Qv * Q^-1

	%Qv = from_vector3( V ),

	% Q being unitary:
	%InvQ = conjugate( Q ),

	% Multiplication is associative:
	%QRotv = mult( Q, mult( Qv, InvQ ) ),

	%to_vector3( QRotv ).


	% Optimised implementation (1) derived from
	% https://gamedev.stackexchange.com/questions/28395/rotating-vector3-by-a-quaternion
	% returning V' = 2.(Vq.V).Vq + (A*A-Vq.Vq).V + 2.A.(Vq^V)


	% A variation (2), not used here, based only on cross-products (not dot
	% products) exist also: V' = V + A.Vq^V + 2.U^(U^V)

	Vq = [ B, C, D ],

	% Constructing (1):

	V1 = vector3:scale( Vq, 2.0 * vector3:dot_product( Vq, V ) ),

	% V2 = vector3:scale( V, A*A - vector3:square_magnitude( Vq ) ),
	%
	% However, as Q is normalised, A*A - vector3:square_magnitude( Vq ) =
	% A*A - ( B*B + C*C + D*D ) = A*A - (1 - A*A) = 2* AA - 1
	%
	% So:
	V2 = vector3:scale( V, 2.0*A*A - 1.0 ),

	V3 = vector3:scale( vector3:cross_product( Vq, V ), 2.0*A ),

	vector3:add( [ V1, V2, V3 ] ).



% @doc Returns the quaternion obtained from the specified 3D vector, that is the
% vector (a.k.a. imaginary) quaternion whose vector part is this one.
-spec from_vector3( vector3() ) -> vector_quaternion().
from_vector3( _V=[ Vx, Vy, Vz ] ) ->
	{ 0.0, Vx, Vy, Vz }.



% @doc Returns the vector part of the specified quaternion, expected to be a
% vector quaternion (that is to have is scalar part null).
%
-spec to_vector3( vector_quaternion() ) -> vector3().
to_vector3( _Q={ A, B, C, D } ) ->

	% Not an assertion, as A must be ignored if no check is done:
	cond_utils:if_defined( myriad_check_linear,
						   true = math_utils:is_null( A ),
						   basic_utils:ignore_unused( A ) ),

	[ B, C, D ].



% @doc Returns the 4x4 matrix representation of the specified quaternion.
-spec to_matrix4( quaternion() ) -> matrix4().
to_matrix4( _Q={A,B,C,D} ) ->
	#matrix4{ m11=A, m12=-B, m13=-C, m14=-D,
			  m21=B, m22=A,  m23=-D, m24=C,
			  m31=C, m32=D,  m33=A,  m34=-B,
			  m41=D, m42=-C, m43=B,  m44=A }.



% @doc Returns the 3x3 rotation matrix corresponding to the specified unitary
% quaternion.
%
-spec to_rot_matrix3( unit_quaternion() ) -> rot_matrix3().
to_rot_matrix3( Q={A,B,C,D} ) ->

	cond_utils:if_defined( myriad_check_linear,
						   true = is_unitary( Q ),
						   basic_utils:ignore_unused( Q ) ),

	AB = A*B,
	AC = A*C,
	AD = A*D,

	BB = B*B,
	BC = B*C,
	BD = B*D,

	CC = C*C,
	CD = C*D,
	DD = D*D,

	M11 = 1 - 2*(CC+DD),
	M12 = 2*(BC-AD),
	M13 = 2*(BD+AC),

	M21 = 2*(BC+AD),
	M22 = 1 - 2*(BB+DD),
	M23 = 2*(CD-AB),

	M31 = 2*(BD-AC),
	M32 = 2*(CD+AB),
	M33 = 1 - 2*(BB+CC),

	#matrix3{ m11=M11, m12=M12, m13=M13,
			  m21=M21, m22=M22, m23=M23,
			  m31=M31, m32=M32, m33=M33 }.



% @doc Returns whether the specified quaternion is unitary, that is whether it
% is of magnitude (norm) 1.0.
%
-spec is_unitary( quaternion() ) -> boolean().
is_unitary( Q ) ->
	% No specific need of computing the square root thereof:
	math_utils:are_equal( 1.0, square_magnitude( Q ) ).



% @doc Returns a textual description of specified quaternion; full float
% precision is shown.
%
-spec to_string( quaternion() ) -> ustring().
to_string( Q ) ->
	to_compact_string( Q ).



% @doc Returns a compact, textual, informal representation of the specified
% quaternion.
%
% This is the recommended representation.
%
-spec to_compact_string( quaternion() ) -> ustring().
to_compact_string( _Q={ A, B, C, D } ) ->

	% Not the following, as would induce unwanting spacing:
	%text_utils:format( ?coord_float_format ++ " + " ++ ?coord_float_format
	%   ++ ".i + " ++ ?coord_float_format ++ ".j + "
	%   ++ ?coord_float_format ++ ".k", [ A, B, C, D ] ).

	text_utils:format( "~w + ~w.i + ~w.j + ~w.k", [ A, B, C, D ] ).



% @doc Returns a textual, more user-friendly representation of the specified
% quaternion; full float precision is shown.
%
-spec to_user_string( quaternion() ) -> ustring().
to_user_string( Q ) ->

	Coords = tuple_to_list( Q ),

	Strs = linear:coords_to_best_width_strings( Coords ),

	% No need for ~ts here (and a different representation from vector):
	ElemFormatStr = "| ~s |~n",

	FormatStr = "~n" ++ text_utils:duplicate( length( Coords ), ElemFormatStr ),

	%trace_utils:debug_fmt( "FormatStr: ~ts; Strs: ~p.",
	%                       [ FormatStr, Strs ] ),

	text_utils:format( FormatStr, Strs ).
