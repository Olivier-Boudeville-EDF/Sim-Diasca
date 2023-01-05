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


% @doc Gathering of various <b>two-dimensional "linear"</b> facilities.
%
% See `linear_2D_test.erl' for the corresponding test.
%
-module(linear_2D).


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).



% Operations on lines:
-export([ get_line/2, intersect/2, get_abscissa_for_ordinate/2 ]).


% Operations related to angles:
-export([ is_strictly_on_the_right/3, is_obtuse/1, abs_angle_rad/3, angle_rad/3,
		  abs_angle_deg/3, angle_deg/3 ]).


% Operations on sets of points:
-export([ compute_smallest_enclosing_rectangle/1,
		  compute_max_overall_distance/1, compute_convex_hull/1,
		  get_roots_of_unit/1, get_roots_of_unit/2 ]).


% Only useful for tests:
-export([ find_pivot/1, sort_by_angle/2 ]).



% For epsilon:
-include("math_utils.hrl").


% Shorthands:

-type count() :: basic_utils:count().

-type int_degrees() :: unit_utils:int_degrees().
-type radians() :: unit_utils:radians().

-type factor() :: math_utils:factor().

-type coordinate() :: linear:coordinate().
-type any_coordinate() :: linear:any_coordinate().

-type distance() :: linear:distance().
-type integer_distance() :: linear:integer_distance().
-type square_distance() :: linear:square_distance().

-type point2() :: point2:point2().
-type integer_point2() :: point2:integer_point2().
-type any_point2() :: point2:any_point2().

-type vector2() :: vector2:vector2().


-type rect_dimensions() :: { Width :: distance(), Height :: distance() }.
% Dimensions of a rectangular area, as floating-point coordinates.

-type integer_rect_dimensions() :: { Width  :: integer_distance(),
									 Height :: integer_distance() }.
% Dimensions of a rectangular area, as integer distances.

-type dimensions() :: integer_rect_dimensions().
% Shorter form of integer_rect_dimensions/0.


-type line2() :: { A :: factor(), B :: factor(), C :: factor() }.
% A 2D line, whose equation A.x + B.y + C =0, can be defined by its three
% factors {A,B,C}.


-type shape() :: 'circle' | 'rectangle' | 'square' | 'triangle' | 'polygon'.
% A collection of 2D shapes.


-type topology_type() :: 'points' | 'lines' | 'line_loop' | 'line_strip'
					   | 'triangles' | 'triangle_strip' | 'triangle_fan'.
% A 2D topology type, typically of a primitive to render.


-export_type([ rect_dimensions/0, integer_rect_dimensions/0, dimensions/0,
			   line2/0, shape/0, topology_type/0 ]).



% Section for sets of points.
%
% Most if not all functions here can operate with any points, i.e. integer
% and/or floating-point ones.



% @doc Computes the smallest rectangle that encloses the specified list of
% points.
%
% Returns {TopLeft, BottomRight}.
%
-spec compute_smallest_enclosing_rectangle( [ any_point2() ] ) ->
											{ any_point2(), any_point2() }.
compute_smallest_enclosing_rectangle( Points ) ->
	compute_smallest_enclosing_rectangle( Points, _TopLeft=undefined,
										  _BottomRight=undefined ).



% (helper)
compute_smallest_enclosing_rectangle( _Points=[], TopLeft, BottomRight ) ->
	{ TopLeft, BottomRight };

compute_smallest_enclosing_rectangle( _Points=[ P | Others ], undefined,
									  undefined ) ->
	% First point found initializes best, knowing that at least two points are
	% expected:
	%
	compute_smallest_enclosing_rectangle( Others, _TopLeft=P, _BottomRight=P );

compute_smallest_enclosing_rectangle( [ _Points={ X, Y } | Others ], { Xt, Yt },
									  { Xb, Yb } ) ->
	Xmin = erlang:min( X, Xt ),
	Ymin = erlang:min( Y, Yt ),
	Xmax = erlang:max( X, Xb ),
	Ymax = erlang:max( Y, Yb ),
	compute_smallest_enclosing_rectangle( Others, { Xmin, Ymin },
										  { Xmax, Ymax } ).



% @doc Computes the maximum distance between two points in the specified list of
% points.
%
% Returns {P1,P2,square_distance(P1,P2)} so that this (square) distance is
% maximal among points.
%
% We ensure that each internal edge is examined only once: when the distances
% between a given vertex V and all other vertices have been computed, V is
% removed from the list and a new maximum is searched within this subset.
%
% @end
%
-spec compute_max_overall_distance( [ any_point2() ] ) ->
							{ any_point2(), any_point2(), square_distance() }.
compute_max_overall_distance( Points ) when length( Points ) < 2 ->
	throw( { no_computable_overall_distance, Points } );

compute_max_overall_distance( Points ) ->
	compute_max_overall_distance( Points, _LongestInfo=undefined ).


% (helper)
%
% Here there is only one point left:
compute_max_overall_distance( _Points=[ _P ], LongestInfo ) ->
	LongestInfo;

% Here we have not computed a distance yet:
compute_max_overall_distance( _Points=[ P | Others ],
							  _LongestInfo=undefined ) ->
	FirstInfo = compute_max_distance_between( P, Others ),
	compute_max_overall_distance( Others, FirstInfo );

% At least one other point remains, and at least one distance was computed:
compute_max_overall_distance( _Points=[ P | Others ],
					LongestInfo={ _P1, _P2, LongestSquareDistance } ) ->

	case compute_max_distance_between( P, Others ) of

		NewLongestInfo={ _P, _PmaxForP, LongestSquareDistanceFromP }
		  when LongestSquareDistanceFromP > LongestSquareDistance ->
			% We have a new winner:
			compute_max_overall_distance( Others, NewLongestInfo );

		_Other ->
			% Here LongestSquareDistance is not beaten:
			compute_max_overall_distance( Others, LongestInfo )

	end.



% @doc Computes the maximum distance between a point (P) and a list of other
% points.
%
% Returns {P, Pmax, LongestSquareDistance} with LongestSquareDistance being the
% distance between P and Pmax, Pmax being chosen so that LongestSquareDistance
% is maximal.
%
% As there must have been at least one point in the list, Pmax exists here
% (never undefined).
%
-spec compute_max_distance_between( any_point2(), [ any_point2() ] )->
							{ any_point2(), any_point2(), square_distance() }.
compute_max_distance_between( _P, _Points=[] ) ->
	throw( no_computable_max_distance );

compute_max_distance_between( P, Points ) ->
	compute_max_distance_between( P, Points, _Info=undefined ).


% (helper)
compute_max_distance_between( P, _Points=[],
							  _Info={ Pmax, LongestSquareDistance } ) ->
	{ P, Pmax, LongestSquareDistance };

compute_max_distance_between( P, _Points=[ Pnew | OtherPoints ],
							  _Info=undefined ) ->
	% The first point examined is (at first) by construction the first best:
	compute_max_distance_between( P, OtherPoints,
		_NewInfo={ Pnew, point2:square_distance( P, Pnew ) } );

compute_max_distance_between( P, _Points=[ Pnew | OtherPoints ],
							  Info={ _Pmax, LongestSquareDistance } ) ->

	case point2:square_distance( P, Pnew ) of

		SquareDistance when SquareDistance > LongestSquareDistance ->
			% We have a new winner:
			compute_max_distance_between( P, OtherPoints,
										  _NewInfo={ Pnew, SquareDistance } );

		_LesserSquareDistance ->
			% Previous best not beaten, let's keep it:
			compute_max_distance_between( P, OtherPoints, Info )

	end.




% Sorting by angle section.


% @doc Finds the pivot, that is the leftmost point with the highest ordinate.
%
% The point list is supposed not having duplicates.
%
% Returns {Pivot, PivotLessList} where PivotLessList is the (unordered) input
% list, without the Pivot.
%
-spec find_pivot( [ any_point2() ] ) -> { any_point2(), [ any_point2() ] }.
find_pivot( _PointList=[ FirstPivot | Others ] ) ->
	% First found is the first pivot:
	find_pivot( Others, FirstPivot, _PList=[] ).


% (helper)
find_pivot( _Points=[], Pivot, PList ) ->
	{ Pivot, PList };

% Higher than the pivot, thus not wanted as pivot:
find_pivot( [ Point={_X,Y} | Others ], Pivot={ _Xp, Yp }, PList ) when Y < Yp ->
	find_pivot( Others, Pivot, [ Point | PList ] );

% Lower than the pivot, thus wanted:
find_pivot( [ Point={_X,Y} | Others ], PreviousPivot={ _Xp, Yp }, PList )
		when Y > Yp ->
	find_pivot( Others, Point, [ PreviousPivot | PList ] );

% Same level as the pivot, but at its right, thus not wanted:
find_pivot( [ Point={X,_Yp} | Others ], Pivot={ Xp, _UselessMatchYp }, PList )
  when X > Xp ->
	find_pivot( Others, Pivot, [ Point | PList ] );

% Same level as the pivot, but at its left, thus wanted:
find_pivot( [ Point={X,_Yp} | Others ], PreviousPivot={ Xp, _UselessMatchYp },
			PList ) when X < Xp ->
	find_pivot( Others, Point, [ PreviousPivot | PList ] );

% Duplicated pivot, abnormal:
find_pivot( [ Pivot | _Others ], Pivot, _PList ) ->
	throw( { duplicate_pivot, Pivot } ).




% @doc Returns a list containing the points sorted according to an increasing
% angle between the abscissa axis and the vector from the pivot to each of these
% points.
%
% Note: all points having the same abscissa as the pivot, except the highest
% one, will be removed from the returned list.
%
-spec sort_by_angle( integer_point2(), [ integer_point2() ] ) ->
												[ integer_point2() ].
sort_by_angle( Pivot, Points ) ->
	sort_by_angle( Pivot, Points, _LeftPoints=[], _MiddlePoint=undefined,
				   _RightPoints=[] ).


-type angle_pair() :: { number(), integer_point2() }.
% {AngleTangent,Point2} pairs; AngleTangent is used as f: a -> tan a is
% increasing on ]-Pi/2, Pi/2[ (mod Pi).


% (helper)
%
-spec sort_by_angle( integer_point2(), [ integer_point2() ], [ angle_pair() ],
		maybe( integer_point2() ), [ angle_pair() ] ) ->  [ integer_point2() ].
sort_by_angle( _Pivot, _Points=[], LeftPairs, _MaybeP=undefined, RightPairs ) ->

	cond_utils:if_defined( bounding_spaces, trace_utils:debug(
								"sort_by_angle: no middle point found." ) ),

	% Not having a middle point to integrate here:
	Index = 1,
	SortedPairs = lists:keysort( Index, LeftPairs )
										++ lists:keysort( Index, RightPairs ),

	cond_utils:if_defined( bounding_spaces,
		trace_utils:debug_fmt( "Full sorted list: ~w.", [ L ] ) ),

	reverse_and_drop_angle( SortedPairs, _Acc=[] );


sort_by_angle( _Pivot, _Points=[], LeftPairs, MiddlePoint, RightPairs ) ->

	cond_utils:if_defined( bounding_spaces, trace_utils:debug(
		"sort_by_angle: at least one middle point found." ) ),

	Index = 1,

	SortedPairs = lists:keysort( Index, LeftPairs )
		++ [ {dummy,MiddlePoint} | lists:keysort( Index, RightPairs ) ],

	reverse_and_drop_angle( SortedPairs, _Acc=[] );

% Note that Y <= Yp by definition of the pivot, hence Y - Yp <= 0:
sort_by_angle( Pivot={Xp,Yp}, [ Point={X,Y} | T ], LeftPairs, MiddlePoint,
			   RightPairs ) ->

	case X-Xp of

		0 ->
			% Here we are just above the pivot, tan(Pi/2) is infinite.
			case MiddlePoint of

				undefined ->
					% First found is first best:
					sort_by_angle( Pivot, T, LeftPairs, Point, RightPairs );

				{ _Xm, Ym } ->

					case Y < Ym of

						true ->
							% This point is above the previous highest middle
							% point, previous middle point can be dropped on the
							% floor:
							%
							sort_by_angle( Pivot, T, LeftPairs, Point,
										   RightPairs );

						false ->
							% The current point can be dropped on the floor, as
							% it is below the highest middle point:
							%
							sort_by_angle( Pivot, T, LeftPairs, MiddlePoint,
										   RightPairs )

					end
			end;

		DeltaX when DeltaX > 0 ->
			% This is a point on the right of the pivot, stores the tangent of
			% the angle the vector defined by the pivot and that point makes
			% with the abscissa axis:
			%
			sort_by_angle( Pivot, T, LeftPairs, MiddlePoint,
						   [ { (Y-Yp) / DeltaX, Point } | RightPairs ] );

		NegativeDeltaX ->
			% This is a point on the left of the pivot:
			sort_by_angle( Pivot, T,
						   [ { (Y-Yp) / NegativeDeltaX, Point } | LeftPairs ],
						   MiddlePoint, RightPairs )

	end.



% (helper)
reverse_and_drop_angle( _AnglePairs=[], Acc ) ->
	Acc;

reverse_and_drop_angle( [ _AnglePairs={ _Tangent, Point } | T ], Acc ) ->
	reverse_and_drop_angle( T, [ Point | Acc ] ).




% Line section.


% @doc Returns the three coefficients {A,B,C} for the line passing by point P
% and being perpendicular to vector V, whose equation is A.x + B.y + C = 0.
%
-spec get_line( point2(), vector2() ) -> line2().
get_line( _P={Xp,Yp}, _V=[Vx,Vy] ) ->

	% Here we know that:
	% P is on the line: A.Xp+B.Yp+C=0  (I)
	% Let M be (X,Y). If M is on the line, the PM.V=0 (null dot product), thus:
	% (X-Xp)*Vx+(Y-Yp)*Vy=0 (II)
	% We want to determine A, B and C:
	% (II) is: Vx.X + Vy*Y - (Xp.Vx+Yp.Vy) = 0 thus:
	%
	A = Vx,
	B = Vy,
	C = - ( Xp*Vx + Yp*Vy ),

	% Necessarily all floating-points:
	{ A, B, C }.



% @doc Returns the intersection of the two specified lines, if it is a point,
% otherwise the 'no_point' atom (the intersection can be void, if the lines are
% parallel but different, or a full line, if they are the same line).
%
% First line has for equation A.x+B.y+C=0, second has for equation U.x+V.y+W=0.
%
-spec intersect( line2(), line2() ) -> 'no_point' | point2().
intersect( _L1={A,B,C}, _L2={U,V,W} ) ->

	% We will try to substitute y, as determined from first equation, into the
	% second one:
	%
	% (math_utils:is_null/0 also allows to handle integers and floats)
	%
	% Returns necessarily floating-point coordinates:

	case math_utils:is_null( B ) of

		true ->
			% Thus A.X = -C
			case math_utils:is_null( A ) of

				true ->
					% Either empty or the same:
					no_point;

				_ANotNull ->
					X = -C/A,
					case math_utils:is_null( V ) of

						true ->
							no_point;

						_VNotNull ->
							Y= -(W+U*X) / V,
							{X,Y}

					end

			end;

		_BNotNull ->
			% General case: Y = - (C+A.X)/B (I), will be replaced in second
			% equation:
			%
			case math_utils:is_null( U ) of

				true ->
					% Thus Y:
					Y= -W/V,
					% Now X from first:
					case math_utils:is_null( A ) of

						true ->
							no_point;

						_ANotNull ->
							X= - (B*Y+C)/A,
							{X,Y}

					end;

				_UNotNull ->
					% General case, substituing (I) in second equation we have:
					% (B.U-V.A).X = V.C-B.D
					%
					Denom = B*U - V*A,

					case math_utils:is_null( Denom ) of

						true ->
							no_point;

						_DenomNotNull ->
							X = (V*C-B*W) / Denom,
							Y = - (C+A*X) / B,
							{X,Y}

					end

			end

	end.



% @doc Returns the abscissa of a point on line L having Y for ordinate.
%
% Line L must not have for equation Y=constant (i.e. its A parameter must not be
% null).
%
-spec get_abscissa_for_ordinate( line2(), any_coordinate() ) -> coordinate().
get_abscissa_for_ordinate( _L={A,B,C}, Y ) ->
	% For y=K, x=-(C+BK)/A
	-(C+B*Y) / A.




% Angle section.


% @doc Returns true iff P is strictly on the right of the oriented segment going
% from P1 to P2.
%
-spec is_strictly_on_the_right( point2(), point2(), point2() ) -> boolean();
							  ( integer_point2(), integer_point2(),
								integer_point2() ) -> boolean().
is_strictly_on_the_right( P, P1, P2 ) ->

	Vec_P1P2 = point2:vectorize( P1, P2 ),

	RightNormal = vector2:normal_right( Vec_P1P2 ),

	Vec_P1P  = point2:vectorize( P1, P ),

	DotProduct = vector2:dot_product( Vec_P1P, RightNormal ),

	DotProduct > 0.



% @doc Returns whether specified angle (in degrees, canonical form) is obtuse.
-spec is_obtuse( int_degrees() ) -> boolean().
is_obtuse( AngleInDegrees ) ->
	AngleInDegrees > 90 andalso AngleInDegrees < 180.



% @doc Returns the angle, in radians, between the vector AB and AC.
%
% Note: with this function we cannot tell whether one vector is ahead of the
% other, ie if we should use the returned angle or its opposite to go from AB
% to AC.
%
-spec abs_angle_rad( point2(), point2(), point2() ) -> radians();
				   ( integer_point2(), integer_point2(), integer_point2() ) ->
										radians().
abs_angle_rad( A, B, C ) ->

	AB = point2:vectorize( A, B ),

	M1 = vector2:magnitude( AB ),

	case math_utils:is_null( M1 ) of

		true ->
			throw( { degenerate_angle, { A, B } } );

		_ ->
			ok

	end,

	AC = point2:vectorize( A, C ),

	M2 = vector2:magnitude( AC ),

	case math_utils:is_null( M2 ) of

		true ->
			throw( { degenerate_angle,{ A, C } } );
		_ ->
			ok

	end,

	%trace_utils:debug_fmt( "AB=~w, AC=~w, M1=~f, M2=~f.", [AB,AC,M1,M2] ),

	math:acos( vector2:dot_product( AB, AC )
					/ ( vector2:magnitude( AB ) * vector2:magnitude( AC ) ) ).



% @doc Returns the signed (oriented) angle, in radians, between the vector AB
% and AC.
%
% Note: with this function we can tell that we must rotate counter-clockwise of
% the returned angle to go from AB to AC.
%
-spec angle_rad( point2(), point2(), point2() ) -> radians();
			   ( integer_point2(), integer_point2(), integer_point2() ) ->
										radians().
angle_rad( A, B, C ) ->

	[ X1, Y1 ] = point2:vectorize( A, B ),

	[ X2, Y2 ] = point2:vectorize( A, C ),

	math:atan2( Y2, X2 ) - math:atan2( Y1, X1 ).



% @doc Returns the angle, in canonical degrees, between the vector AB and AC.
%
% Note: with this function we cannot tell whether one vector is ahead of the
% other, that is if we should use the returned angle or its opposite to go from
% AB to AC.
%
-spec abs_angle_deg( point2(), point2(), point2() ) -> int_degrees();
				   ( integer_point2(), integer_point2(), integer_point2() ) ->
										int_degrees().
abs_angle_deg( A, B, C ) ->
	math_utils:canonify(
		math_utils:radian_to_degree( abs_angle_rad( A, B, C ) ) ).



% @doc Returns the signed (oriented) angle, in canonical degrees, between the
% vector AB and AC.
%
% Note: with this function we can tell that we must rotate counter-clockwise of
% the returned angle to go from AB to AC.
%
-spec angle_deg( any_point2(), any_point2(), any_point2() ) -> int_degrees().
angle_deg( A, B, C ) ->
	math_utils:canonify(
		math_utils:radian_to_degree( angle_rad( A, B, C ) ) ).





% Convex hull section.


% @doc Computes the convex hull corresponding to the specified list of points.
%
% Returns a list of the points that define the hull.
%
-spec compute_convex_hull( [ any_point2() ] ) -> [ any_point2() ].
compute_convex_hull( Points ) ->

	{ Pivot, RemainingPoints } = find_pivot( Points ),

	case length( RemainingPoints ) of

		Len when Len < 2 ->
			throw( not_enough_points_for_convex_hull );

		_Other ->
			% We have at least 2 points in addition to the pivot.
			%trace_utils:debug_fmt( "Pivot is ~w, remaining points: ~w.",
			%    [ Pivot, RemainingPoints ] ),

			[ P1, P2 | T ] = sort_by_angle( Pivot, RemainingPoints ),

			% Initially only the pivot is known to belong to the convex hull.
			% We had P1, next to be validated against P2.
			%
			% We also add the pivot to the end of the NextPoints list, so that
			% the hull can be closed.
			compute_graham_scan_hull( _ToValidate=[ P1, Pivot ],
				_NewPoint=P2, _NextPoints=( T ++ [ Pivot ] ) )

	end.



% @doc Computes the Graham scan for the specified list of points, expected to be
% already sorted by increasing angle between the abscissa axis and the vector
% from the pivot to each of these points (that is in increasing order of the
% angle they and the point P make with the x-axis, in counter-clockwise order).
%
% See [http://en.wikipedia.org/wiki/Graham_scan].
%
% Returns the corresponding convex hull, in clockwise order.
%
-spec compute_graham_scan_hull( [ any_point2() ], any_point2(),
								[ any_point2() ] ) -> [ any_point2() ].
compute_graham_scan_hull( ToValidate, _Pivot, _NextPoints=[] ) ->
	% Last new point is by construction always to pivot.

	%trace_utils:debug_fmt( "compute_graham_scan_hull: "
	%    "exhausted input points, returning: ~w.", [ ToValidate ] ),

	ToValidate;


compute_graham_scan_hull( ToValidate=[ P2, P1 | T ], NewPoint,
						  NextPoints=[ Next | OtherNext ] ) ->

	% Should P2 be on the line defined by P1 and NewPoint, then P2 will be
	% considered as not being on the left: in the convex hull, only necessary
	% points will be kept, ie no point on the boundary of the hull will be
	% kept.
	%
	% Note: The test seems to be wrongly negated; however it is correct as
	% actually we describe the algorithm as seen if represented in a basis whose
	% ordinates are increasing when going from the top to the bottom (ie like
	% when rendered on a classical GUI, the Y axis going then downward).
	%
	case is_strictly_on_the_right( P2, P1, NewPoint ) of

		false ->

			%trace_utils:debug_fmt( "compute_graham_scan_hull: point ~w "
			%   "is on the right of segment from ~w to ~w, keeping ~w.",
			%   [ P2, P1, NewPoint, P2 ] ),

			% Here, the point P2 is on the right of the segment going from P1 to
			% the Next point, thus P2 can be kept and we can continue with the
			% next points:
			%
			compute_graham_scan_hull( [ NewPoint | ToValidate ], Next,
									  OtherNext );

		true ->

			%trace_utils:debug_fmt( "compute_graham_scan_hull: "
			%   [ "point ~w is on the left of segment from ~w to ~w, "
			%     "eliminating ~w.", [ P2, P1, NewPoint, P2 ] ),

			% Here, the point P2 is on the left of (or in) the segment going
			% from P1 to the Next point, thus P2 is to be discarded, and will
			% have to check predecessor(s) of P2 against the Next point.

			compute_graham_scan_hull( [ P1 | T ], NewPoint, NextPoints )


	end;

% Note however that the first point examined after the pivot (FP1) may have to
% be discarded because of the second. If we just removed FP1, then the
% ToValidate list would just contain the pivot, thus triggering a function
% clause. In that case we just have to replace FP1 by the next(FP1)=P2 here, and
% thus ToValidate will always contain at least two elements:

% This clause matches whenever we just removed the first point in the list
% examined after the pivot. So the first parameter (ToValidate) is just a list
% with one element, the pivot, that was added to close the hull. As we have no
% intermediate point, we accept directly the next point (Next), knowing it will
% be checked at the next recursion:
%compute_graham_scan_hull( [ Pivot ], NewPoint, [ Next | OtherNext ] ) ->
%    compute_graham_scan_hull( [ NewPoint, Pivot ], Next, OtherNext ).
% A bit faster as we know L is actually [ Pivot ]:
%
compute_graham_scan_hull( L, NewPoint, [ Next | OtherNext ] ) ->
	compute_graham_scan_hull( [ NewPoint | L ], Next, OtherNext ).



% @doc Returns a list of points forming the nth roots of unity, in the unit
% circle centered on the origin, the first one being in the X axis.
%
-spec get_roots_of_unit( count() ) -> [ point2() ].
get_roots_of_unit( N ) ->
	get_roots_of_unit( N, _StartingAngle=0.0 ).


% @doc Returns a list of points forming the nth roots of unity, in the unit
% circle centered on the origin, the first one making the specified angle with
% the X axis.
%
-spec get_roots_of_unit( count(), radians() ) -> [ point2() ].
get_roots_of_unit( N, StartingAngle ) ->
	AngleIncRad = 2 * math:pi() / N,
	get_roots_of_unit( N, StartingAngle, AngleIncRad, _Acc=[] ).


% (helper)
get_roots_of_unit( _N=0, _CurrentAngle, _AngleIncRad, Acc ) ->
	Acc;

get_roots_of_unit( N, CurrentAngle, AngleIncRad, Acc ) ->

	% As in radians:
	P = { math:cos( CurrentAngle ), math:sin( CurrentAngle ) },

	get_roots_of_unit( N-1, CurrentAngle+AngleIncRad, AngleIncRad,
					   [ P | Acc ] ).
