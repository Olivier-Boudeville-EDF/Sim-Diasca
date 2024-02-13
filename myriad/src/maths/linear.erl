% Copyright (C) 2010-2024 Olivier Boudeville
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


% @doc Gathering of various facilities for <b>linear-related</b> operations.
-module(linear).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% General notes: linear operations (matrices, vectors, etc.) could be done based
% on Blas (see https://github.com/erlef/blas).


% These type names are too general to be defined in the hrl file (i.e. in the
% root namespace).

% By default (unless specified), most values (e.g. coordinates, distances) are
% floating-point ones.
%
% Integer counterparts are usually defined (integer_*), as well as values that
% may be either integer or floating-point ones (any_*).
%
% As a result, even if many operations could have remained polymorphic if
% relying on the any_* types, here we chose to emphasize on floating-point
% numbers.


-type dimension() :: count().
% The dimension of a space (e.g. number of rows or columns for a matrix).


-type coordinate() :: float().
% Cartesian real (actually double-precision floating-point) coordinates in a
% referential.


-type integer_coordinate() :: integer().
% Cartesian integer coordinates in a referential.


-type any_coordinate() :: number().
% Cartesian coordinates in a referential.


-type user_coordinate() :: number().
% User-specified coordinates in a referential.


-type distance() :: float().
% Distance (as floating-point) between two points (e.g. to express lengths).


-type integer_distance() :: integer().
% Integer distance between two points (e.g. to express lengths).


-type any_distance() :: number().
% Distance between two points (e.g. to express lengths).


-type signed_distance() :: float().
% Signed distance (positive or negative).



% Mostly for clarity:

-type radius() :: distance().
% Radius (as floating-point).


-type integer_radius() :: integer_distance().
% Radius (as integer).


-type any_radius() :: any_distance().
% Radius.


-type square_distance() :: float().
% Square of a distance between two points, as a floating-point value (cheaper to
% compute, when applying the square root operator is not needed, like when
% comparing distances).


-type integer_square_distance() :: integer().
% Square of a distance between two points, as an integer value (cheaper to
% compute, when applying the square root operator is not needed, like when
% comparing distances).



-type any_square_distance() :: number().
% Square of a distance between two points (cheaper to compute, when applying the
% square root operator is not needed, like when comparing distances).



-type area() :: float().
% Area of a surface.


-type scalar() :: float().
% A scalar, that is a 1-D vector / square matrix.


-type specialised_point() :: point2() | point3() | point4().
% Any kind of specialised (fixed-size - not of arbitrary dimension) point.

-type specialised_vector() :: vector2() | vector3() | vector4().
% Any kind of specialised (fixed-size - not of arbitrary dimension) vector.
% Any kind of basic, fixed-size (not of arbitrary dimension) vector.

-type specialised_matrix() :: matrix2() | matrix3() | matrix4().
% Any kind of specialised (fixed-size - not of arbitrary dimension) matrix.


-type specialised_type() :: scalar() | specialised_point()
						  | specialised_vector() | specialised_matrix().
% The well-known specialised (fixed-size) linear types that are of use when
% performing geometry.


-type specialised_vertex() :: specialised_point().
% Any kind of specialised (fixed-size - not of arbitrary dimension) vertex.


-type specialised_normal() :: specialised_vector().
% Any kind of specialised (fixed-size - not of arbitrary dimension) vertex.


-type specialised_texture_coordinates() :: specialised_point().
% Any kind of specialised (fixed-size - not of arbitrary dimension) texture
% coordinates.


-type indice() :: basic_utils:positive_index().
% The indice/index of an element (e.g. of a vertex in an array thereof).


-type indexed_triangle() :: { indice(), indice(), indice() }.
% A definition of a triangle, based on the indices of its three vertices, in a
% vertex container (e.g. a list).


-type bounding_space() :: bounding_surface:bounding_surface()
						| bounding_volume:bounding_volume().
% A bounding space, for a given dimensionality.


-export_type([ dimension/0,
			   coordinate/0, integer_coordinate/0, any_coordinate/0,
			   user_coordinate/0,

			   distance/0, integer_distance/0, any_distance/0,
			   signed_distance/0,

			   radius/0, integer_radius/0, any_radius/0,
			   square_distance/0, integer_square_distance/0,
			   any_square_distance/0,
			   area/0,
			   scalar/0, specialised_point/0, specialised_vector/0,
			   specialised_matrix/0, specialised_type/0,
			   specialised_vertex/0, specialised_normal/0,
			   specialised_texture_coordinates/0,
			   indice/0, indexed_triangle/0,
			   bounding_space/0 ]).


-export([ get_element_count/1,
		  coord_to_string/1, coords_to_best_width_strings/1 ] ).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type count() :: basic_utils:count().

-type point2() :: point2:point2().
-type point3() :: point3:point3().
-type point4() :: point4:point4().

-type vector2() :: vector2:vector2().
-type vector3() :: vector3:vector3().
-type vector4() :: vector4:vector4().

-type matrix2() :: matrix2:matrix2().
-type matrix3() :: matrix3:matrix3().
-type matrix4() :: matrix4:matrix4().



% @doc Returns the number of elements (coordinates) stored in the specified
% specialised type.
%
-spec get_element_count( specialised_type() ) -> count().
get_element_count( _Type=scalar ) -> 1;

get_element_count( _Type=point2 ) -> 2;
get_element_count( _Type=point3 ) -> 3;
get_element_count( _Type=point4 ) -> 4;

get_element_count( _Type=vector2 ) -> 2;
get_element_count( _Type=vector3 ) -> 3;
get_element_count( _Type=vector4 ) -> 4;

get_element_count( _Type=matrix2 ) -> 4;
get_element_count( _Type=matrix3 ) -> 9;
get_element_count( _Type=matrix4 ) -> 16.



% @doc Returns a textual representation of a coordinate.
-spec coord_to_string( any_coordinate() ) -> ustring().
coord_to_string( Coord ) when is_float( Coord ) ->

	% For testing:
	%text_utils:format( "XX~*.*.*fXX~n", [ 14, 12, $a, 1/3 ] ).

	text_utils:format( ?coord_float_format, [ Coord ] );

coord_to_string( Coord ) when is_integer( Coord ) ->
	text_utils:format( ?coord_integer_format, [ Coord ] ).



% @doc Returns textual representations of the specified coordinates of a common,
% best (maximal) width; full float precision is shown in returned strings.
%
-spec coords_to_best_width_strings( [ any_coordinate() ] ) -> [ ustring() ].
coords_to_best_width_strings( Coords ) ->

	% No need to force a specific precision or width here; floats are printed
	% accurately as the shortest, correctly rounded string (WYSIWYG):
	%
	Strs = [ text_utils:format( "~w", [ C ] ) || C <- Coords ],

	Len = lists:max( [ length( S ) || S <- Strs ] ),

	% To test:
	%[ text_utils:center_string( S, Len, _PaddingChar=$+ ) || S <- Strs ].

	[ text_utils:center_string( S, Len ) || S <- Strs ].
