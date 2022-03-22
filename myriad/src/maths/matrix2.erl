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


% @doc Module implementing the support for <b>2x2 matrices</b>.
%
% See also:
% - the corresponding (2D) vectors, in vector2.erl
% - the (unspecialised) matrices of arbitrary dimensions, in matrix.erl
%
-module(matrix2).



% Implementation notes:
%
% These 2x2 matrices come in various forms:
%
% - the canonical one (2x2 coordinates)
%
% - special ones, at least the identity matrix


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For records like matrix2:
-include("matrix2.hrl").


-type user_matrix2() :: user_matrix().
% A matrix2 can be specified as a list of same-size rows (akin to a user
% arbitrary matrix) containing any kind of numerical coordinates.


% Possibly to be added: {'rotation_2', radians()}.
-type matrix2() :: 'identity_2' | canonical_matrix2().


% Alias for 2x2 canonical matrices:
-type canonical_matrix2() :: #matrix2{}.


-type rot_matrix2() :: canonical_matrix2().
% A matrix describing a 2D rotation.


-export_type([ user_matrix2/0, matrix2/0, canonical_matrix2/0, rot_matrix2/0 ]).


-export([ new/1, null/0, identity/0, rotation/1,
		  from_columns/2, from_rows/2,
		  from_coordinates/4,
		  from_arbitrary/1, to_arbitrary/1,
		  dimension/0, dimensions/0,
		  row/2, column/2,
		  get_element/3, set_element/4,
		  transpose/1,
		  scale/2,
		  add/2, sub/2, mult/2, mult/1, apply/2,
		  are_equal/2,
		  determinant/1, comatrix/1, inverse/1,
		  to_canonical/1,
		  check/1,
		  to_string/1 ] ).


-import( math_utils, [ are_close/2 ] ).

% To avoid clash with BIF:
-compile( { no_auto_import, [ apply/2 ] } ).


-define( dim, 2 ).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type factor() :: math_utils:factor().

-type radians() :: unit_utils:radians().

-type coordinate() :: linear:coordinate().
-type dimension() :: linear:dimension().
-type scalar() :: linear:scalar().

-type vector2() :: vector2:vector2().

-type dimensions() :: matrix:dimensions().

-type user_matrix() :: matrix:user_matrix().
-type matrix() :: matrix:matrix().



% @doc Returns a 2D (canonical) matrix corresponding to the user-specified
% matrix.
%
-spec new( user_matrix2() ) -> canonical_matrix2().
new( UserMatrix ) ->

	CoordList = [ type_utils:ensure_float( C )
					|| C <- list_utils:flatten_once( UserMatrix ) ],

	% Returns a #matrix2 record (i.e. a tagged tuple):
	list_to_tuple( [ 'matrix2' | CoordList ] ).



% @doc Returns the null (2x2) matrix.
-spec null() -> canonical_matrix2().
null() ->
	Zero = 0.0,
	CoordList = lists:duplicate( _N=?dim * ?dim, Zero ),
	list_to_tuple( [ 'matrix2' | CoordList ] ).



% @doc Returns the identity (2x2) matrix.
-spec identity() -> matrix2().
identity() ->
	identity_2.



% @doc Returns the (2x2) matrix corresponding to a counterclockwise rotation
% about the origin of the specified angle with respect to the abscissa (X) axis.
%
% A rotation matrix is orthogonal, its inverse is its transpose, and its
% determinant is 1.0.
%
% These 2D rotation matrices form a group known as the special orthogonal group
% SO(2).
%
-spec rotation( radians() ) -> rot_matrix2().
rotation( RadAngle ) ->

	Cos = math:cos( RadAngle ),
	Sin = math:sin( RadAngle ),
	#matrix2{ m11=Cos, m12=-Sin,
			  m21=Sin, m22=Cos }.



% @doc Returns the 2x2 matrix whose columns correspond to the specified 2 2D
% vectors.
%
% Returns thus:
%  ```
%  [ Va Vb ]
%  [ |  |  ]
%  '''
%
-spec from_columns( vector2(), vector2() ) -> canonical_matrix2().
from_columns( _Va=[Xa,Ya], _Vb=[Xb,Yb] ) ->
	#matrix2{ m11=Xa, m12=Xb,
			  m21=Ya, m22=Yb }.



% @doc Returns the 2x2 matrix whose rows correspond to the specified 2 2D
% vectors.
%
% Returns thus:
%  ```
% [ Va - - - ]
% [ Vb - - - ]
%  '''
%
-spec from_rows( vector2(), vector2() ) -> canonical_matrix2().
from_rows( _Va=[Xa,Ya], _Vb=[Xb,Yb] ) ->
	#matrix2{ m11=Xa, m12=Ya,
			  m21=Xb, m22=Yb }.



% @doc Returns the (2x2, canonical) matrix whose (4) coordinates are the
% specified ones, as listed row after row.
%
-spec from_coordinates( coordinate(), coordinate(),
						coordinate(), coordinate() ) -> canonical_matrix2().
from_coordinates( M11, M12,
				  M21, M22 ) ->
	#matrix2{ m11=M11, m12=M12,
			  m21=M21, m22=M22 }.



% @doc Returns the 2x2 matrix corresponding to the specified
% arbitrary-dimensioned matrix.
%
-spec from_arbitrary( matrix() ) -> matrix2().
from_arbitrary( Matrix ) ->
	erlang:apply( fun from_rows/?dim, Matrix ).


% @doc Returns the arbitrary-dimensioned matrix corresponding to the specified
% 2x2 matrix.
%
-spec to_arbitrary( matrix2() ) -> matrix().
to_arbitrary( Matrix2 ) ->
	M = to_canonical( Matrix2 ),
	[ _RecordTag | Coords ] = tuple_to_list( M ),
	matrix:from_coordinates( Coords, _ColumCount=?dim ).



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
-spec row( dimension(), matrix2() ) -> vector2().
row( _RowCount=1, #matrix2{ m11=M11, m12=M12 } ) ->
	[ M11, M12 ];

row( _RowCount=2, #matrix2{ m21=M21, m22=M22 } ) ->
	[ M21, M22 ];

row( RowCount, OtherMatrix ) ->
	row( RowCount, to_canonical( OtherMatrix ) ).



% @doc Returns the specified column of the specified matrix.
-spec column( dimension(), matrix2() ) -> vector2().
column( _ColumnCount=1, #matrix2{ m11=M11, m21=M21 } ) ->
	[ M11, M21 ];

column( _ColumnCount=2, #matrix2{ m12=M12, m22=M22 } ) ->
	[ M12, M22 ];

column( ColCount, OtherMatrix ) ->
	column( ColCount, to_canonical( OtherMatrix ) ).



% @doc Returns the element at specified row and column of the specified matrix.
-spec get_element( dimension(), dimension(), matrix2() ) -> coordinate().
get_element( _R=1, _C=1, #matrix2{ m11=M11 } ) ->
	M11;

get_element( _R=1, _C=2, #matrix2{ m12=M12 } ) ->
	M12;


get_element( _R=2, _C=1, #matrix2{ m21=M21 } ) ->
	M21;

get_element( _R=2, _C=2, #matrix2{ m22=M22 } ) ->
	M22;

get_element( R, C, OtherMatrix ) ->
	get_element( R, C, to_canonical( OtherMatrix ) ).


% @doc Returns a matrix identical to the specified one except that its specified
% element at specified location has been set to the specified value.
%
-spec set_element( dimension(), dimension(), coordinate(), matrix2() ) ->
									matrix2().
set_element( _R=1, _C=1, Value, Matrix=#matrix2{} ) ->
	Matrix#matrix2{ m11=Value };

set_element( _R=1, _C=2, Value, Matrix=#matrix2{} ) ->
	Matrix#matrix2{ m12=Value };


set_element( _R=2, _C=1, Value, Matrix=#matrix2{} ) ->
	Matrix#matrix2{ m21=Value };

set_element( _R=2, _C=2, Value, Matrix=#matrix2{} ) ->
	Matrix#matrix2{ m22=Value };

set_element( R, C, Value, OtherMatrix ) ->
	set_element( R, C, Value, to_canonical( OtherMatrix ) ).



% @doc Returns the transpose of the specified matrix.
-spec transpose( matrix2() ) -> matrix2().
% Diagonal untouched:
transpose( M=#matrix2{ m12=M12, m21=M21 } ) ->
	M#matrix2{ m12=M21, m21=M12 };

transpose( M=identity_2 ) ->
	M.



% @doc Scales specified (2D) matrix of the specified factor.
-spec scale( matrix2(), factor() ) -> matrix2().
scale( #matrix2{ m11=M11, m12=M12,
				 m21=M21, m22=M22 }, Factor ) ->
	#matrix2{ m11=Factor*M11, m12=Factor*M12,
			  m21=Factor*M21, m22=Factor*M22 };

scale( M=identity_2, Factor ) ->
	scale( to_canonical( M ), Factor ).



% @doc Returns the sum of the two specified matrices: M = Ma + Mb.
-spec add( matrix2(), matrix2() ) -> matrix2().
add( _Ma=#matrix2{ m11=A11, m12=A12,
				   m21=A21, m22=A22 },
	 _Mb=#matrix2{ m11=B11, m12=B12,
				   m21=B21, m22=B22 } ) ->

	#matrix2{ m11=A11+B11, m12=A12+B12,
			  m21=A21+B21, m22=A22+B22 };

add( Ma, Mb ) ->
	add( to_canonical( Ma ), to_canonical( Mb ) ).



% @doc Returns the subtraction of the two specified matrices: M = Ma - Mb.
-spec sub( matrix2(), matrix2() ) -> matrix2().
sub( _Ma=#matrix2{ m11=A11, m12=A12,
				   m21=A21, m22=A22 },
	 _Mb=#matrix2{ m11=B11, m12=B12,
				   m21=B21, m22=B22 } ) ->

	#matrix2{ m11=A11-B11, m12=A12-B12,
			  m21=A21-B21, m22=A22-B22 };

sub( Ma, Mb ) ->
	% Quick and dirty:
	MinusMb = scale( Mb, -1.0 ),
	add( Ma, MinusMb ).



% @doc Multiplies the first matrix by the second one: returns Mc = Ma.Mb.
-spec mult( Ma:: matrix2(), Mb :: matrix2() ) -> matrix2().
mult( _Ma=identity_2, Mb ) ->
	Mb;

mult( Ma, _Mb=identity_2 ) ->
	Ma;

mult( _Ma=#matrix2{ m11=A11, m12=A12,
					m21=A21, m22=A22 },
	  _Mb=#matrix2{ m11=B11, m12=B12,
					m21=B21, m22=B22 } ) ->

	C11 = A11*B11 + A12*B21,
	C12 = A11*B12 + A12*B22,

	C21 = A21*B11 + A22*B21,
	C22 = A21*B12 + A22*B22,

	#matrix2{ m11=C11, m12=C12,
			  m21=C21, m22=C22 }.



% @doc Multiplies (in-order) the specified matrices.
%
% Ex: mult([Ma, Mb, Mc]) = mult(mult(Ma,Mb),Mc) = Ma.Mb.Mc
%
-spec mult( [ matrix2() ] ) -> matrix2().
mult( [ Ma, Mb | T ] ) ->
	mult( [ mult( Ma, Mb ) | T ] );

mult( [ M ] ) ->
	M.



% @doc Applies (on the right) the specified vector V to the specified matrix M:
% returns M.V.
%
% Not a clause of mult/2 for an increased clarity.
%
-spec apply( matrix2(), vector2() ) -> vector2().
apply( _M=identity_2, V ) ->
	V;

apply( _M=#matrix2{ m11=M11, m12=M12,
					m21=M21, m22=M22 }, _V=[ Vx, Vy ] ) ->
	ResX = M11*Vx + M12*Vy,
	ResY = M21*Vx + M22*Vy,
	[ ResX, ResY ].



% @doc Tells whether the two specified (2x2) matrices are equal.
-spec are_equal( matrix2(), matrix2() ) -> boolean().
are_equal( _Ma=#matrix2{ m11=A11, m12=A12,
						 m21=A21, m22=A22 },
		   _Mb=#matrix2{ m11=B11, m12=B12,
						 m21=B21, m22=B22 } ) ->
	are_close( A11, B11 ) andalso are_close( A12, B12 )
		andalso are_close( A21, B21 ) andalso are_close( A22, B22 );

are_equal( _Ma=identity_2, _Mb=identity_2 ) ->
	true;

are_equal( Ma, Mb=identity_2 ) ->
	are_equal( Ma, to_canonical( Mb ) );

are_equal( Ma=identity_2, Mb ) ->
	are_equal( Mb, Ma ).



% @doc Returns the determinant of the specified matrix.
-spec determinant( matrix2() ) -> scalar().
determinant( _M=#matrix2{ m11=M11, m12=M12,
						  m21=M21, m22=M22 } ) ->
	M11 * M22 - M12 * M21;

determinant( _M=identity_2 ) ->
	1.0.



% @doc Returns the comatrix of the specified matrix (that is the matrix of its
% cofactors).
%
-spec comatrix( matrix2() ) -> matrix2().
comatrix( identity_2 ) ->
	identity_2;

comatrix( _M=#matrix2{ m11=M11, m12=M12,
					   m21=M21, m22=M22 } ) ->
	#matrix2{ m11=M22,  m12=-M21,
			  m21=-M12, m22=M11 }.



% @doc Returns the inverse of the specified matrix, if it is inversible (that is
% iff its determinant is non-null), otherwise returns undefined.
%
% Note: often the inverse can be obtained differently (ex: by applying reverse
% operations starting from identity) or computed differently (ex: by Gaussian
% elimination), or can be replaced by a mere lower–upper (LU) decomposition.
%
-spec inverse( matrix2() ) -> maybe( matrix2() ).
inverse( identity_2 ) ->
	identity_2;

inverse( M=#matrix2{ m11=M11, m12=M12,
					 m21=M21, m22=M22 } ) ->
	Det = determinant( M ),
	case math_utils:is_null( Det ) of

		true ->
			undefined;

		false ->
			#matrix2{ m11=M22/Det,  m12=-M12/Det,
					  m21=-M21/Det, m22=M11/Det }

	end.



% @doc Returns the canonical form of the specified 2x2 matrix.
-spec to_canonical( matrix2() ) -> canonical_matrix2().
to_canonical( identity_2 ) ->
	Zero = 0.0,
	One = 1.0,
	#matrix2{ m11=One,  m12=Zero,
			  m21=Zero, m22=One  };

to_canonical( M ) when is_record( M, matrix2 ) ->
	M.



% @doc Checks that the specified matrix is legit, and returns it.
-spec check( matrix2() ) -> matrix2().
check( M=identity_2 ) ->
	M;

check( M ) ->
	[ matrix2 | Coords ] = tuple_to_list( M ),

	[ type_utils:check_float( C ) || C <- Coords ],

	M.



% @doc Returns a textual representation of the specified (2x2) matrix.
-spec to_string( matrix2() ) -> ustring().
to_string( _Matrix=identity_2 ) ->
	"2x2 identity";

to_string( Matrix2=#matrix2{} ) ->

	[ _AtomMatrix2 | AllCoords ] = tuple_to_list( Matrix2 ),

	Strs = linear:coords_to_best_width_strings( AllCoords ),

	% No need for ~ts here:
	RowFormatStr = "[ " ++ text_utils:duplicate( ?dim, "~s " ) ++ "]~n",

	FormatStr = "~n" ++ text_utils:duplicate( ?dim, RowFormatStr ),

	text_utils:format( FormatStr, Strs ).
