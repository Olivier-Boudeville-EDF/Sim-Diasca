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
% Creation date: 2010.


% @doc Unit tests for the (2D) <b>bounding surface facilities</b>.
%
% See the bounding_surface tested module.
%
-module(bounding_surface_test).


% For run/0 export and al:
-include("test_facilities.hrl").




% Number of random tests to do for each given number of points:
-define( test_count, 1000 ).
%-define( test_count, 100000 ).

test_mec() ->

	PointCountMin = 3,

	%PointCountMax = 5,
	PointCountMax = 15,
	%PointCountMax = 50,
	%PointCountMax = 500,

	test_facilities:display( "Testing the Minimal Enclosing Circles, "
		"from ~B to ~B points (each with ~B random tests).",
		[ PointCountMin, PointCountMax, ?test_count ] ),

	test_mec( PointCountMin, PointCountMax, _TestCount=0 ),


	test_facilities:display( "Testing the special case of flat triangles." ),

	AlignedPoints = [ {0,0}, {7,0}, {11,0} ],

	% First check:
	MECircle = bounding_surface:get_lazy_bounding_circle( AlignedPoints ),
	[ true = bounding_surface:is_within( P, MECircle ) || P <- AlignedPoints ],

	AllPerms = list_utils:get_all_permutations( AlignedPoints ),

	[ % We recompute what is expected to be always the same MEC:
	  MECircle = bounding_surface:get_lazy_bounding_circle( Perm )
			|| Perm <- AllPerms ].



-define( min_coord, 0 ).

% To boost special cases:
%-define( max_coord, 50 ).
-define( max_coord, 1000 ).



% Will progressively slow down as the number of points increases:
test_mec( PointCountMax, PointCountMax, _TestCount=?test_count ) ->
	ok;

test_mec( PointCount, PointCountMax, _TestCount=?test_count ) ->

	NewPointCount = PointCount+1,

	test_facilities:display( " - testing for ~B random points",
							 [ NewPointCount ] ),

	test_mec( NewPointCount, PointCountMax, _TCount=0 );


test_mec( PointCount, PointCountMax, TestCount ) ->

	%test_facilities:display( "   * test #~B for ~B random points",
	%                         [ TestCount, PointCount ] ),

	Points = point2:draw_integer_random( ?min_coord, ?max_coord, PointCount ),

	MECircle = bounding_surface:get_lazy_bounding_circle( Points ),

	% Was so quick that wanted to check:
	%test_facilities:display( "For points ~p, got MEC=~p.",
	%                         [ Points, MECircle ] ),

	[ true = bounding_surface:is_within( P, MECircle ) || P <- Points ],

	test_mec( PointCount, PointCountMax, TestCount+1 ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_mec(),

	test_facilities:stop().
