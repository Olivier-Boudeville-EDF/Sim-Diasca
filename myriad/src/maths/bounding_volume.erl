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


% @doc Gathering of various facilities for (3D) <b>bounding volume</b>
% management.
%
% Currently the types of supported bounding volumes are:
% - right-cuboids, see <a href="http://en.wikipedia.org/wiki/Cuboid">cuboids</a>
% - spheres
%
% See `bounding_volume_test.erl' for the corresponding test.
%
-module(bounding_volume).


-export([ get_lazy_bounding_sphere/1, get_minimal_enclosing_sphere/1,
		  to_string/1 ]).


% For record declarations of bounding volumes:
-include("bounding_volume.hrl").


-type bounding_volume_type() :: 'right_cuboid' | 'sphere'.
% Allows to designate a type of bounding volume.


-type right_cuboid() :: #right_cuboid{}.
% A bounding volume defined based on a right cuboid.


-type sphere() :: #sphere{}.
% A bounding volume defined based on a sphere.


-type bounding_volume() :: right_cuboid() | sphere().
% All supported types of bounding volumes.


-export_type([ bounding_volume_type/0,
			   right_cuboid/0, sphere/0, bounding_volume/0 ]).


% Implementation notes:
%
% Bounding volumes, at least here, rely on floating-point coordinates and
% distances (rather than on integer ones).
%
% Refer to the notes in bounding_surface.erl for an explanation about why a
% circle or sphere-based bounding space cannot be computed as easily from the
% diameter of its points (maximal distance between a set of points, which would
% be the diameter of the bounding space).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type point3() :: point3:point3().



% @doc Returns a sphere that is a bounding volume for the specified list of
% points, which must not be empty.
%
% Note: this bounding volume is very lightweight to compute, yet not a minimal
% one.
%
% Returns the sphere information: {Center, SquareRadius}.
%
-spec get_lazy_bounding_sphere( [ point3() ] ) -> sphere().
get_lazy_bounding_sphere( Points ) ->

	{ TopLeftNearP, BottomRightFarP } =
		linear_3D:compute_smallest_enclosing_cuboid( Points ),

	Center = point3:get_center( TopLeftNearP, BottomRightFarP ),

	% We divide by 4, as we are dealing with squared quantities:
	SquareRadius = point3:square_distance( TopLeftNearP, BottomRightFarP ) / 4,

	#sphere{ center=Center, square_radius=SquareRadius }.



% @doc Returns {Center, SquareRadius} which defines a bounding volume consisting
% on the Minimal Enclosing Sphere (MES) for the specified list of points.
%
% Note: this bounding volume is the smallest possible sphere, but requires quite
% a lot of computations.
%
% Apparently there is now way of adding a point to an existing MES without
% recomputing everything from scratch. So we do not rely on an 'updateMES'
% function.
%
-spec get_minimal_enclosing_sphere( [ point3() ] ) -> sphere().
get_minimal_enclosing_sphere( _Points=[] ) ->
	throw( no_point_to_enclose );

get_minimal_enclosing_sphere( _Points=[ _P ] ) ->
	throw( todo ).



% @doc Returns a textual description of the specified bounding volume.
-spec to_string( bounding_volume() ) -> ustring().
to_string( #right_cuboid{ base_vertex=BaseVertex, abscissa_length=XLen,
						  ordinate_length=YLen, elevation_length=ZLen } ) ->
	text_utils:format( "bounding right cuboid whose base vertex is ~ts, "
		"and lengths along the X, Y and Z axes are respectively ~w, ~w and ~w",
		[ point3:to_string( BaseVertex ), XLen, YLen, ZLen ] );

to_string( #sphere{ center=Center, square_radius=SquareRadius } ) ->
	text_utils:format( "bounding sphere whose center is ~ts and "
		"square radius is ~w", [ Center, SquareRadius ] ).
