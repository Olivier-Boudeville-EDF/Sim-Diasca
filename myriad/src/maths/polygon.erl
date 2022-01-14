% Copyright (C) 2010-2022 Olivier Boudeville
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



% @doc Gathering of various facilities for <b>polygon</b> management.
%
% When used for rendering, coordinates are expected to be often integers.
%
% See `polygon_test.erl' for the corresponding test.
%
-module(polygon).


-include("polygon.hrl").


-type polygon() :: #polygon{}.
% Describes a polygon, convex or not, simple or not.

-export_type([ polygon/0 ]).


% For the circle record and al:
-include("bounding_box2.hrl").


% Construction-related section.
-export([ get_triangle/3, get_upright_square/2, get_polygon/1 ]).


% Operations on polygons.
-export([ get_diameter/1, get_smallest_enclosing_rectangle/1, get_area/1,
		  is_in_clockwise_order/1, is_convex/1,
		  render/2, to_string/1 ]).


% Color-related section.
-export([ set_edge_color/2, get_edge_color/1,
		  set_fill_color/2, get_fill_color/1 ]).


% Bounding-box related section.
-export([ update_bounding_box/2 ]).


% Shorthands:

-type option_list() :: option_list:option_list().

-type ustring() :: text_utils:ustring().

-type any_point2() :: point2:any_point2().
-type any_vertex2() :: point2:any_vertex2().

-type color() :: gui_color:color().
-type canvas() :: gui:canvas().

-type distance() :: linear:distance().
-type square_distance() :: linear:square_distance().
-type area() :: linear:area().

-type bounding_algorithm() :: bounding_box2:bounding_algorithm().


% Construction-related section.


% @doc Returns a triangle (defined as a polygon) corresponding to the specified
% three vertices.
%
-spec get_triangle( any_point2(), any_point2(), any_point2() ) -> polygon().
get_triangle( P1, P2, P3 ) ->
	#polygon{ vertices=[ P1, P2, P3 ] }.



% @doc Returns an upright square corresponding to the specified center and edge
% length.
%
-spec get_upright_square( any_vertex2(), distance() ) -> polygon().
get_upright_square( _Center={Xc,Yc}, EdgeLength ) ->
	Offset = erlang:round( EdgeLength / 2 ),
	X1 = Xc - Offset,
	X2 = Xc + Offset,
	Y1 = Yc - Offset,
	Y2 = Yc + Offset,
	#polygon{ vertices=[ {X1,Y1}, {X2,Y1}, {X2,Y2}, {X1,Y2} ] }.



% @doc Returns a new polygon whose vertices are the specified ones.
-spec get_polygon( [ any_vertex2() ] ) -> polygon().
get_polygon( Vertices ) ->
	#polygon{ vertices=Vertices }.



% Operations on polygons.


% @doc Returns a polygon diameter, that is two points in the polygon that are at
% the maximum distance one of the other.
%
% Returns {P1,P2,D} when P1 and P2 are the endpoints of a diameter and SquareD
% is its square length: `SquareD = square_distance(P1, P2)'.
%
-spec get_diameter( polygon() ) ->
				{ any_vertex2(), any_vertex2(), square_distance() }.
get_diameter( Polygon ) ->

	case Polygon#polygon.vertices of

		[] ->
			throw( no_vertex );

		[ _Vertex ] ->
			throw( single_vertex );

		ListWithAtLeastTwoVertices ->
			% There are at least two vertices:
			linear_2D:compute_max_overall_distance( ListWithAtLeastTwoVertices )

	end.



% @doc Returns the smallest upright rectangle that encompasses the specified
% polygon.
%
% More precisely a {TopLeftCorner, BottomRightCorner} pair is returned, which
% defines the rectangle from two opposite points.
%
-spec get_smallest_enclosing_rectangle( polygon() ) ->
										    { any_point2(), any_point2() }.
get_smallest_enclosing_rectangle( Polygon ) ->

	case Polygon#polygon.vertices of

		[] ->
			throw( no_vertex );

		[ _Vertex ] ->
			throw( single_vertex );

		ListWithAtLeastTwoVertices ->
			linear_2D:compute_smallest_enclosing_rectangle(
				ListWithAtLeastTwoVertices )

	end.



% @doc Returns the (unsigned) area enclosed of the polygon, supposed to be
% non-self-intersecting and having at least two vertices.
%
% Vertices can be listed clockwise or counter-clockwise.
%
% Should there be no absolute value computed, and should the polygon be convex,
% then the area would be positive iff vertices were listed in counter-clockwise
% order.
%
% See [http://en.wikipedia.org/wiki/Polygon#Area_and_centroid].
%
-spec get_area( polygon() ) -> area().
get_area( Polygon ) ->
	erlang:abs( get_signed_area( Polygon#polygon.vertices ) ).



% @doc Tells whether the specified polygon has its vertices in clockwise order
% (otherwise they are in counter-clockwise order).
%
-spec is_in_clockwise_order( polygon() ) -> boolean().
is_in_clockwise_order( Polygon ) ->
	case get_signed_area( Polygon#polygon.vertices ) of

		Area when Area > 0 ->
			true;

		_Negative ->
			false

	end.



% @doc Tells whether the specified polygon is convex (iff true) or concave
% (otherwise).
%
% Polygon must have at least one vertex.
%
-spec is_convex( polygon() ) -> boolean().
is_convex( Polygon ) ->
	[ First | T ] = Polygon#polygon.vertices,
	is_convex( T ++ [ First ], _Previous=First, _Sign=undefined ).



% (helper)
%
% Sign is either 'undefined' (initially), or 'positive', or 'negative'.
is_convex( [], _Previous, _Sign ) ->
	% Not interrupted, thus convex (includes polygon having only one vertex):
	true;

is_convex( [ V={X,Y} | T ], _Previous={Xp,Yp}, _Sign=undefined ) ->

	% Setting the first sign:
	%trace_utils:debug_fmt( "initial: previous= ~w, next= ~w, sum=~w.",
	%                       [ {Xp,Yp}, V, Xp*Y-X*Y ] ),

	FirstSign = case Xp*Y-X*Yp of

		PositiveSum when PositiveSum > 0 ->
			positive;

		_NegativeSum ->
			negative

	end,

	is_convex( T, _NewPrevious=V, FirstSign );


is_convex( [ V={X,Y} | T ], _Previous={Xp,Yp}, Sign ) ->

	%trace_utils:debug_fmt( "Iteration points: previous= ~w, next= ~w, "
	%                       "sum=~w.", [ {Xp,Yp}, P, Xp*Y-X*Yp ] ),

	% Checking if still obtaining the same sign:
	NewSign = case Xp*Y-X*Yp of

		PositiveSum when PositiveSum > 0 ->
			positive;

		 _NegativeSum ->
			negative

	end,

	%trace_utils:debug_fmt( "Current sign: ~ts, new one: ~ts.",
	%                       [ Sign, NewSign ] ),

	case NewSign of

		Sign ->
			% Can still be convex:
			is_convex( T, _NewPrevious=V, Sign );

		_OppositeSign ->
			% Finished, as is concave:
			false

	end.




% Color-related section.


% @doc Sets the edge color of specified polygon.
-spec set_edge_color( color(), polygon() ) -> polygon().
set_edge_color( Color, Polygon ) ->
	Polygon#polygon{ rendering=option_list:set(
			_Entry={ edge_color, gui_color:get_color( Color ) },
			_OptionList=Polygon#polygon.rendering ) }.



% @doc Returns the current edge color of the specified polygon, if specified,
% otherwise 'undefined'.
%
-spec get_edge_color( polygon() ) -> maybe( color() ).
get_edge_color( Polygon ) ->
	option_list:lookup( edge_color, Polygon#polygon.rendering ).



% @doc Sets the fill color of the specified polygon.
%
% Use 'none' to disable filling.
%
-spec set_fill_color( color(), polygon() ) -> polygon().
set_fill_color( Color, Polygon ) ->
	Polygon#polygon{ rendering=option_list:set(
			_Entry={ fill_color, gui_color:get_color( Color ) },
			_OptionList=Polygon#polygon.rendering ) }.



% @doc Returns the current fill color of the specified polygon, if specified,
% otherwise 'undefined'.
%
-spec get_fill_color( polygon() ) -> maybe( color() ).
get_fill_color( Polygon ) ->
	option_list:lookup( _Key=fill, _OptionList=Polygon#polygon.rendering ).



% @doc Returns options for the rendering of this polygon that can be directly
% passed to the graphical back-end.
%
-spec get_rendering_options( polygon() ) -> option_list().
get_rendering_options( Polygon ) ->
	Polygon#polygon.rendering.



% @doc Renders specified polygon in specified canvas.
%
% Throws an exception if the polygon is not valid.
%
-spec render( polygon(), canvas() ) -> void().
render( Polygon, Canvas ) ->

	%trace_utils:debug_fmt( "Rendering polygon:~n~ts.",
	%                       [ to_string( Polygon ) ] ),

	case Polygon#polygon.vertices of

		[] ->
			throw( null_polygon );

		[ _Vertex ] ->
			throw( one_vertex_polygon );

		Vertices ->

			Opts = get_rendering_options( Polygon ),

			case option_list:lookup( _K=edge_color, Opts ) of

				undefined ->
					ok;

				DrawColor ->
					%trace_utils:debug_fmt( "DrawColor = ~p.", [ DrawColor ] ),
					gui:set_draw_color( Canvas, DrawColor )

			end,

			case option_list:lookup( fill_color, Opts ) of

				undefined ->
					ok;

				FillColor ->
					gui:set_fill_color( Canvas, FillColor )

			end,

			gui:draw_polygon( Canvas, Vertices ),

			case Polygon#polygon.bounding_box of

				#circle{ center=Center, square_radius=SquareRadius } ->
					IntCenter = point2:roundify( Center ),
					gui:draw_circle( Canvas, IntCenter,
									 round( math:sqrt( SquareRadius ) ) ),
					gui:draw_cross( Canvas, IntCenter, _EdgeLength=4 );

				undefined ->
					ok

			end

	end.



% @doc Returns a textual description of the specified polygon.
-spec to_string( polygon() ) -> ustring().
to_string( Polygon ) ->

	BBText = case Polygon#polygon.bounding_box of

		undefined ->
			"none available";

		BB ->
			bounding_box2:to_string( BB )

	end,

	text_utils:format( "polygon defined by:~n"
	  " - vertices: ~w~n - edge color: ~w~n - fill color: ~w~n"
	  " - bounding-box: ~ts~n",
	  [ Polygon#polygon.vertices, get_edge_color( Polygon ),
		get_fill_color( Polygon ), BBText ] ).



% Bounding-box related section.


% @doc Updates, for the specified polygon, its internal bounding-box, with
% regard to the specified bounding-box algorithm.
%
% Returns a polygon with updated information.
%
% @end
%
% The lazy circle bounding box is fast to determine, but not optimal:
-spec update_bounding_box( bounding_algorithm(), polygon() ) -> polygon().
update_bounding_box( lazy_circle, Polygon ) ->

	CircleBBox = bounding_box2:get_lazy_circle_box( Polygon#polygon.vertices ),

	Polygon#polygon{ bounding_box=CircleBBox }.




% Helper functions.


% @doc Returns the signed area enclosed of the polygon, supposed to be
% non-self-intersecting and having at least two vertices.
%
% Vertices can be listed clockwise or counter-clockwise.
%
-spec get_signed_area( [ any_vertex2() ] ) -> area().
get_signed_area( _Vertices=[ First | T ] ) ->

	% We will start from the second point, as we always deal with the current
	% one and its predecessor (avoid to add an element at end of list):

	get_signed_area( T, _FirstOfAll=First, _Previous=First, _Area=0 ).



get_signed_area( _Vertices=[ _Last={X,Y} ], _FirstOfAll={Xf,Yf},
				 _Previous={Xp,Yp}, Area ) ->

	% Here we reached the last point of the polygon, so, first, we compute its
	% product with the previous point, then we do the same with the FirstOfAll
	% point, as if it was following this Last point:

	LastTwoSumTerms = Xp*Y-X*Yp + X*Yf-Xf*Y,

	( Area + LastTwoSumTerms ) / 2 ;


get_signed_area( [ V={X,Y} | T ], FirstOfAll, _Previous={Xp,Yp}, Area ) ->
	% Here we are not managing the last point:
	get_signed_area( T, FirstOfAll, _NewPrevious=V, Area + Xp*Y-X*Yp ).
