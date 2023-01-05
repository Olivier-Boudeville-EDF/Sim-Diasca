% Copyright (C) 2010-2023 Olivier Boudeville
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


% @doc Gathering of various facilities for (2D) <b>bounding surface</b>
% management.
%
% Currently the types of supported bounding surfaces are:
% - bounding rectangles, which can be quickly determined
% - "lazy" circles, directly deriving from the previous rectangle
% - MEC (Minimal Enclosing Circles), whose processing, based on convex hull,
% is more demanding
%
% With the lazy algorithm, circle parameters are simply deduced from the
% smallest enclosing rectangle; it is fast and easy, yet less precise than the
% <em>Minimal Enclosing Circle</em> (MEC).
%
% Determining the MEC involves computing the convex hull of the points. It is
% expensive, but not a problem if precomputing it.
%
% Bounding surfaces operate on floating-point (not integer) coordinates.
%
% See `bounding_surface_test.erl' for the corresponding test.
%
-module(bounding_surface).


-export([ get_bounding_rectangle/1,
		  get_lazy_bounding_circle/1, get_minimal_enclosing_circle/1,
		  get_circle_if_in_range/3, is_within/2, is_within/3,
		  get_circumscribed_circle_for/3,
		  to_string/1 ]).


% For record declarations of bounding surfaces:
-include("bounding_surface.hrl").


-type bounding_algorithm() :: 'rectangle' | 'lazy_circle' | 'mec'.
% Allows to designate an algorithm in charge of computing a bounding surface.
% For example several algorithms allow, with different trade-offs, to compute
% (different) instances of bounding surfaces (of the same type, ex: circle, or
% of different types).


-type rectangle() :: #rectangle{}.
% A bounding surface defined based on a rectangle.


-type circle() :: #circle{}.
% A bounding surface defined based on any circle (ex: lazy or MEC).


-type bounding_surface() :: rectangle() | circle().
% All supported types of bounding surfaces.


-export_type([ bounding_algorithm/0,
			   rectangle/0, circle/0, bounding_surface/0 ]).


% Design notes:
%
% An implicit 2D referential applies here, where:
% - abscissas (often denoted as X) increase from left to right
% - ordinates (often denoted as Y) increase from top to bottom
%
% See https://myriad.esperide.org/#conventions

% Implementation notes:
%
% Bounding surfaces, at least here, rely on floating-point coordinates and
% distances (rather than on integer ones).
%
% Often square distances are as useful as mere distances for algorithms, and
% they require less computations (x -> x² is an increasing function).


% About Minimal Enclosing Circles

% One may believe that a solution in order to compute the Minimal Enclosing
% Circle of a set of points is simply to determine the diameter of this set
% (i.e. the maximum distance between any two of its points) and to use it as the
% diameter of the (circle) bounding surface. This solution is not correct, as
% other points could still lie outside of this bounding surface.

% Indeed, let's name A and B two points that are (at least among, if not the
% only) the mutually farthest of the set, C the center of the [AB] line segment,
% and l the AC=CB distance. If a circle bounding surface was centered on C and
% had l for radius, then, any point simultaneously distant of up to 2.l from A
% and up to 2.l from B could be in that set; in the general case there are
% points in the intersection of these two larger discs that are not in the
% aforementioned disc of radius l centered in C.



% Shorthands:

-type ustring() :: text_utils:ustring().

-type int_degrees() :: unit_utils:int_degrees().

-type square_distance() :: linear:square_distance().

-type point2() :: point2:point2().
-type any_point2() :: point2:any_point2().



% @doc Returns a rectangle that is a bounding surface for the specified list of
% points, which must not be empty.
%
% Note: this bounding surface is not the smallest one, but the most lightweight
% to compute.
%
-spec get_bounding_rectangle( [ any_point2() ] ) -> rectangle().
get_bounding_rectangle( Points ) ->

	{ TopLeftP, BottomRightP } =
		linear_2D:compute_smallest_enclosing_rectangle( Points ),

	#rectangle{ top_left=TopLeftP, bottom_right=BottomRightP }.



% @doc Returns a circle that is a bounding surface for the specified list of
% points, which must not be empty.
%
% Note: this bounding surface is not the smallest one, but is very lightweight
% to compute.
%
-spec get_lazy_bounding_circle( [ any_point2() ] ) -> circle().
get_lazy_bounding_circle( Points ) ->

	{ TopLeftP, BottomRightP } =
		linear_2D:compute_smallest_enclosing_rectangle( Points ),

	Center = point2:get_center( TopLeftP, BottomRightP ),

	% We divide by 4, as we are dealing with squared quantities:
	SquareRadius = point2:square_distance( TopLeftP, BottomRightP ) / 4,

	#circle{ center=Center, square_radius=SquareRadius }.



% @doc Returns {Center, SquareRadius} which defines a bounding surface
% consisting in the Minimal Enclosing Circle (MEC) for the specified list of
% points.
%
% Note: this bounding surface is the unique, smallest possible circle, but
% requires non-negligible computations.
%
% Apparently there is no way of adding a point to an existing MEC without
% recomputing everything from scratch. So we do not provide a
% update_minimal_enclosing_circle_surface/2 function.
%
-spec get_minimal_enclosing_circle( [ point2() ] ) -> circle().
get_minimal_enclosing_circle( _Points=[] ) ->
	throw( no_point_to_enclose );

get_minimal_enclosing_circle( _Points=[ P ] ) ->
	% Only one point, epsilon-based comparison allows for a null radius:
	{ _Center=P, _SquareRadius=0.0 };

get_minimal_enclosing_circle( _Points=[ P1, P2 ] ) ->

	% Here we have two points; this defines the circle:
	CenterP = point2:get_center( P1, P2 ),

	% Division by 4, not 2, as we deal with square quantities:
	SquareRadius = point2:square_distance( P1, P2 ) / 4,

	#circle{ center=CenterP, square_radius=SquareRadius };


% This clause is necessary, as the next one may end with a MEC for 2 or 3
% points; without the current clause, the next one may thus recurse indefinitely
% (ex: MEC for [{387,106},{474,143},{363,305}]).
%
% Moreover this clause, i.e. the computing of the MEC for a triangle, used to be
% incorrect, as it clearly returned sub-optimal circles, i.e. always the
% circumscribed circle - whereas in general the circle whose diameter is made of
% the two most distant points of the three is the MEC.
%
get_minimal_enclosing_circle( _Points=[ P1, P2, P3 ] ) ->

	cond_utils:if_defined( bounding_surfaces, trace_utils:debug_fmt(
		"get_minimal_enclosing_circle for 3 points: "
		"~w, ~w and ~w.", [ P1, P2, P3 ] ) ),

	% As discussed for example in
	% https://www.geeksforgeeks.org/minimum-enclosing-circle-set-1/, the MEC for
	% 3 points intersects either 2 or 3 of these points.

	% So we test first the "intersects exactly 2 of the points", by testing in
	% turn the 3 possibilities in terms of sides: if, for a given tested side,
	% the third point lies in the circle having that side for diameter, then we
	% have found the (unique) MEC. Otherwise we know it intersects the 3 points,
	% and handle it by determining their circumscribed circle, which is thus the
	% MEC.

	% First, let's test the P1P2 side:
	case get_circle_if_in_range( P3, P1, P2 ) of

		undefined ->
			% Nope, let's test the P1P3 side:
			case get_circle_if_in_range( P2, P1, P3 ) of

				undefined ->
					% Nope, let's test the P2P3 side:
					case get_circle_if_in_range( P1, P2, P3 ) of

						undefined ->
							% Nope, let's go for the circumscribed circle then:
							case get_circumscribed_circle_for( P1, P2, P3 ) of

								% Were aligned...
								undefined ->
									get_circle_for_aligned( P1, P2, P3 );

								CircumscribedCircle ->
									CircumscribedCircle

							end;

						P2P3Circle ->
							P2P3Circle

					end;

				P1P3Circle ->
					P1P3Circle

			end;

		P1P2Circle ->
			P1P2Circle

	end;


get_minimal_enclosing_circle( Points ) ->

	% Here we have at least three points, let's work an the hull instead:
	% See http://www.cs.mcgill.ca/~cs507/projects/1998/jacob/solutions.html
	% for the solution.

	%trace_utils:debug_fmt( "MEC for ~w.", [ Points ] ),

	% This allows to operate on potentially a lot fewer points:
	case linear_2D:compute_convex_hull( Points ) of

		[ P1, P2 | OtherPoints ] ->
			% We start with a side S defined by P1 and P2 here:
			try_side( P1, P2, OtherPoints );

		Other ->
			throw( { unexpected_hull, Other } )

	end.



% @doc Returns the circle whose diameter is the [A,B] segment if the specified
% point P is in it (bounds of the circle included), otherwise 'undefined'.
%
-spec get_circle_if_in_range( P :: any_point2(), A :: any_point2(),
							  B :: any_point2() ) -> maybe( circle() ).
get_circle_if_in_range( P, A, B ) ->
	Center = point2:get_center( A, B ),
	SquareRadius = point2:square_distance( A, B ) / 4.0,
	SquareDistOfP = point2:square_distance( Center, P ),
	case SquareDistOfP > SquareRadius of

		true ->
			undefined;

		false ->
			#circle{ center=Center, square_radius=SquareRadius }

	end.



% @doc Tells whether the specified point P is within the specified circle.
-spec is_within( any_point2(), circle() ) -> boolean().
is_within( P, #circle{ center=C, square_radius=SR } ) ->
	is_within( P, C, SR ).



% @doc Tells whether specified the point P is within the specified circle.
-spec is_within( any_point2(), any_point2(), square_distance() ) -> boolean().
is_within( P, Center, SquareRadius ) ->
	point2:square_distance( P, Center ) =< SquareRadius.




% @doc Returns the circumscribed circle of the three specified points, that is
% the (unique) circle intersecting them all, provided that they are not aligned
% (otherwise returns undefined).
%
-spec get_circumscribed_circle_for( any_point2(), any_point2(),
									any_point2() ) -> maybe( circle() ).
get_circumscribed_circle_for( P1, P2, P3 ) ->

	% Here we have three points, a triangle, which defines the circumscribed
	% circle, whose center is the intersection of the three perpendicular
	% bisectors.
	%
	% Let La be the perpendicular bisector of [P1,P2] and Lb the one of [P1,P3]:

	Pa = point2:get_center( P1, P2 ),
	La = linear_2D:get_line( Pa, point2:vectorize( P1, P2 ) ),

	Pb = point2:get_center( P1, P3 ),
	Lb = linear_2D:get_line( Pb, point2:vectorize( P1, P3 ) ),

	case linear_2D:intersect( La, Lb ) of

		no_point ->
			% Here the two bisectors are parallel, the three vertices must be
			% aligned, no circumscribed circle exists:
			%
			%throw( { flat_triangle, Points } );
			undefined;

		% Necessarily with floating-point coordinates:
		CenterP ->
			#circle{ center=CenterP,
					 square_radius=point2:square_distance( CenterP, P1 ) }

	end.



% @doc Returns the MEC of the three specified points, supposed to be aligned.
-spec get_circle_for_aligned( any_point2(), any_point2(), any_point2() ) ->
														circle().
get_circle_for_aligned( P1, P2, P3 ) ->

	% Computing the square distances:
	SDForP1P2 = point2:square_distance( P1, P2 ),
	SDForP1P3 = point2:square_distance( P1, P3 ),
	SDForP2P3 = point2:square_distance( P2, P3 ),

	% Let's be A and B the most distant points of the three, and SD their square
	% distance:
	%
	{ A, B, SquareRadius } = case SDForP1P2 > SDForP1P3 of

		true ->
			case SDForP1P2 > SDForP2P3 of

				true ->
					{ P1, P2, SDForP1P2 };

				false ->
					{ P2, P3, SDForP2P3 }

			end;

		false ->
			case SDForP1P3 > SDForP2P3 of

				true ->
					{ P1, P3, SDForP1P3 };

				false ->
					{ P2, P3, SDForP2P3 }

			end

	end,

	Center = point2:get_center( A, B ),

	#circle{ center=Center, square_radius=SquareRadius }.



% Tries the side of the convex hull H defined by vertices P1 and P2.
%
% (helper)
try_side( P1, P2, OtherPoints ) ->

	{ MinAngle, MinVertex } = find_minimal_angle( P1, P2, OtherPoints ),

	%trace_utils:debug_fmt( "Trying side [~w, ~w], min vertex: ~w, "
	%                       "others: ~w.", [ P1, P2, MinVertex, OtherPoints ] ),

	case MinAngle of

		FirstAngle when FirstAngle > 90 ->
			% Finished, P1 and P2 determine the diametric circle, reusing the
			% code for that:
			%
			get_minimal_enclosing_circle( [ P1, P2 ] );

		_FirstAngleTooSmall ->

			SecondAngle = linear_2D:abs_angle_deg( P1, MinVertex, P2 ),

			case linear_2D:is_obtuse( SecondAngle ) of

				false ->
					ThirdAngle = linear_2D:abs_angle_deg( P2, MinVertex, P1 ),

					case linear_2D:is_obtuse( ThirdAngle ) of

						false ->
							% MEC determined by P1, P2 and MinVertex:
							get_minimal_enclosing_circle(
								[ P1, P2, MinVertex ] );

						true ->
							% Here we try the new side defined by the opposite
							% points of P2, i.e. MinVertex and P1.

							% We must however reconstruct beforehand the list of
							% remaining points, since OtherPoints contains
							% MinVertex but not P2:
							%
							NewOtherPoints =
								[ P2 | lists:delete( MinVertex, OtherPoints ) ],
							try_side( MinVertex, P1, NewOtherPoints )

					end;

				true ->
					% Here we try the new side defined by the opposite points of
					% P1, i.e. MinVertex and P2.

					% We must however reconstruct beforehand the list of
					% remaining points, since OtherPoints contains MinVertex but
					% not P1:
					%
					NewOtherPoints =
						[ P1 | lists:delete( MinVertex, OtherPoints ) ],
					try_side( MinVertex, P2, NewOtherPoints )

			end

	end.



% @doc Returns {MinAngle, MinVertex}, the minimum angle (in canonical degrees)
% subtended by the segment [P1, P2] among the specified list of points.
%
-spec find_minimal_angle( point2(), point2(), [ point2() ] ) ->
											{ int_degrees(), point2() }.
find_minimal_angle( _P1, _P2, _Points=[] ) ->
	throw( { find_minimal_angle, not_enough_points } );

find_minimal_angle( P1, P2, _Points=[ Pfirst | OtherPoints ] ) ->

	%trace_utils:debug_fmt( "Trying to find minimal angle in ~w for ~w "
	%    "and ~w.", [ Points, P1, P2 ] ),

	BootstrapAngle = linear_2D:abs_angle_deg( Pfirst, P1, P2 ),

	find_minimal_angle( P1, P2, OtherPoints, _MinAngle=BootstrapAngle,
						_MinVertex=Pfirst ).


% Points was not empty, thus not 'undefined' in the returned pair:
find_minimal_angle( _P1, _P2, _Points=[], MinAngle, MinVertex ) ->
	{ MinAngle, MinVertex };

find_minimal_angle( P1, P2, [ P | OtherPoints ], MinAngle, MinVertex ) ->

	case linear_2D:abs_angle_deg( P, P1, P2 ) of

		Angle when Angle =< MinAngle ->
			% We have a new winner here:
			find_minimal_angle( P1, P2, OtherPoints, Angle, P );

		_NonMinimalAngle ->
			find_minimal_angle( P1, P2, OtherPoints, MinAngle, MinVertex )

	 end.



% @doc Returns a textual description of the specified bounding surface.
-spec to_string( bounding_surface() ) -> ustring().
to_string( #rectangle{ top_left=TopLeft, bottom_right=BottomRight } ) ->
	text_utils:format( "bounding rectangle whose top-left corner is ~ts "
		"and bottom-right one is ~ts",
		[ point2:to_string( TopLeft ), point2:to_string( BottomRight ) ] );

to_string( #circle{ center=Center, square_radius=SquareRadius } ) ->
	text_utils:format( "bounding circle whose center is ~ts and "
		"square radius is ~w", [ point2:to_string( Center ), SquareRadius ] ).
