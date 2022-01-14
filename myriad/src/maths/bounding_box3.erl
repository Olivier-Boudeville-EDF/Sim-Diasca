% Copyright (C) 2003-2022 Olivier Boudeville
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


% @doc Gathering of various facilities for <b>3D bounding box</b> management.
%
% Currently the types of supported 3D bounding boxes are:
% - right-cuboids, see <a href="http://en.wikipedia.org/wiki/Cuboid">cuboids</a>
% - spheres
%
% See `bounding_box3_test.erl' for the corresponding test.
%
-module(bounding_box3).


-export([ get_lazy_sphere_box/1, get_minimal_enclosing_sphere_box/1,
		  to_string/1 ]).


% For record declarations of 3D bounding boxes:
-include("bounding_box3.hrl").


-type bounding_box3_type() :: 'right_cuboid' | 'sphere'.
% Allows to designate a type of 3D bounding-box.


-type right_cuboid() :: #right_cuboid{}.
% A (3D) bounding box defined based on a right cuboid.


-type sphere() :: #sphere{}.
% A (3D) bounding box defined based on a sphere.


-type bounding_box3() :: right_cuboid() | sphere().
% All supported types of 3D bounding boxes.


-export_type([ bounding_box3_type/0,
			   right_cuboid/0, sphere/0, bounding_box3/0 ]).


% Implementation notes:
%
% Bounding-boxes, at least here, rely on floating-point coordinates and
% distances (rather than on integer ones).
%
% Refer to the notes in bounding_box2.erl for an explanation about why a circle
% or sphere-based bounding box cannot be computed as easily from the diameter of
% its points (maximal distance between a set of points, which would be the
% diameter of the bounding box).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type point3() :: point3:point3().
%-type integer_point3() :: point3:integer_point3().




% @doc Returns a sphere that is a (3D) pseudo bounding-box for the specified
% list of points, which must not be empty.
%
% Note: this pseudo bounding box is very lightweight to compute.
%
% Returns the sphere information: {Center, SquareRadius}.
%
-spec get_lazy_sphere_box( [ point3() ] ) -> sphere().
get_lazy_sphere_box( Points ) ->

	{ TopLeftNearP, BottomRightFarP } =
		linear_3D:compute_smallest_enclosing_cuboid( Points ),

	Center = point3:get_center( TopLeftNearP, BottomRightFarP ),

	% We divide by 4, as we are dealing with squared quantities:
	SquareRadius = point3:square_distance( TopLeftNearP, BottomRightFarP ) / 4,

	#sphere{ center=Center, square_radius=SquareRadius }.



% @doc Returns {Center, SquareRadius} which defines a bounding-box consisting on
% the minimal enclosing sphere (MEC) for the specified list of points.
%
% Note: this bounding box is the smallest possible sphere, but requires quite a
% lot of computations.
%
% Apparently there is now way of adding a point to an existing MEC without
% recomputing everything from scratch. So we do not rely on an 'updateMEC'
% function.
%
-spec get_minimal_enclosing_sphere_box( [ point3() ] ) -> sphere().
get_minimal_enclosing_sphere_box( _Points=[] ) ->
	throw( no_point_to_enclose );

get_minimal_enclosing_sphere_box( _Points=[ _P ] ) ->
	throw( todo ).



% @doc Returns a textual description of the specified 3D bounding box.
-spec to_string( bounding_box3() ) -> ustring().
to_string( #right_cuboid{ base_vertex=BaseVertex, abscissa_length=XLen,
						  ordinate_length=YLen, elevation_length=ZLen } ) ->
	text_utils:format( "bounding right cuboid whose base vertex is ~ts, "
		"and lengths along the X, Y and Z axes are respectively ~w, ~w and ~w",
		[ point3:to_string( BaseVertex ), XLen, YLen, ZLen ] );

to_string( #sphere{ center=Center, square_radius=SquareRadius } ) ->
	text_utils:format( "bounding sphere whose center is ~ts and "
		"square radius is ~w", [ Center, SquareRadius ] ).
