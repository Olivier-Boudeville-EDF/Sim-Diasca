% Copyright (C) 2022-2023 Olivier Boudeville
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
% Creation date: Saturday, June 4, 2022.

-record( sequential_octants, {

	% Top-level, North-East octant:
	o1 :: maybe( octree:sequential_octree() ),

	% Top-level, South-East octant:
	o2 :: maybe( octree:sequential_octree() ),

	% Top-level, South-West octant:
	o3 :: maybe( octree:sequential_octree() ),

	% Top-level, North-West octant:
	o4 :: maybe( octree:sequential_octree() ),


	% Bottom-level, North-East octant:
	o5 :: maybe( octree:sequential_octree() ),

	% Bottom-level, South-East octant:
	o6 :: maybe( octree:sequential_octree() ),

	% Bottom-level, South-West octant:
	o7 :: maybe( octree:sequential_octree() ),

	% Bottom-level, North-West octant:
	o8 :: maybe( octree:sequential_octree() ) } ).
% Datastructure for pure sequential octrees, that is: octrees-as-terms.
%
% Stores the 8 sub-octrees (children cells) that any sequential octree node may
% reference.
%
% In a conventional MyriadGUI absolute referential, space is divided that way:
%
%
%  Z
%  ^
%  |    Y
%  |  /
%  | /
%  |/
%  + ---------> X
%  O
%
% Octants are numbered according to two layers, the top one (Z>=0, child O1 to
% O4) and the bottom one (Z<0, child O5 to O8). In each layer, the children are
% listed in countercloocwise order, when considering the Z axis, starting from
% the one whose left side is then along the +X axis.
%
% So, looking from the axis Z downward to the origin, when the obserser is above
% plane OXY (upper half-space):
%
%        ^ Y
%        |
%        |     . Z
%        |
%     O3 | O4
% -------O-------> X
%     O2 | O1
%        |
%        |
%
%
% Looking from the axis Z downward to the origin, when the obserser is lower,
% below plane OXY (lower half-space):
%
%        ^ Y
%        |
%        |     . Z
%        |
%     O7 | O8
% -------O-------> X
%     O6 | O5
%        |
%        |
%
% See http://myriad.esperide.org/#octrees for additional information.



-record( concurrent_octants, {

	% Top-level, North-East octant:
	o1 :: maybe( octree:octree_pid() ),

	% Top-level, South-East octant:
	o2 :: maybe( octree:octree_pid() ),

	% Top-level, South-West octant:
	o3 :: maybe( octree:octree_pid() ),

	% Top-level, North-West octant:
	o4 :: maybe( octree:octree_pid() ),


	% Bottom-level, North-East octant:
	o5 :: maybe( octree:octree_pid() ),

	% Bottom-level, South-East octant:
	o6 :: maybe( octree:octree_pid() ),

	% Bottom-level, South-West octant:
	o7 :: maybe( octree:octree_pid() ),

	% Bottom-level, North-West octant:
	o8 :: maybe( octree:octree_pid() ) } ).
% Datastructure for pure concurrent octrees, that is: octrees-as-processes.
%
% Stores the 8 sub-concurrent octrees (children cell processes) that any
% concurrent octree node may reference.
%
% See the sequential_octants record for further details.



-record( hybrid_octants, {

	% Top-level, North-East octant:
	o1 :: maybe( octree:octree_designator() ),

	% Top-level, South-East octant:
	o2 :: maybe( octree:octree_designator() ),

	% Top-level, South-West octant:
	o3 :: maybe( octree:octree_designator() ),

	% Top-level, North-West octant:
	o4 :: maybe( octree:octree_designator() ),


	% Bottom-level, North-East octant:
	o5 :: maybe( octree:octree_designator() ),

	% Bottom-level, South-East octant:
	o6 :: maybe( octree:octree_designator() ),

	% Bottom-level, South-West octant:
	o7 :: maybe( octree:octree_designator() ),

	% Bottom-level, North-West octant:
	o8 :: maybe( octree:octree_designator() ) } ).
% Datastructure for hybrid octrees, that is: octrees as terms or processes.
%
% The 8 sub-hybrid octrees (children cell processes) that may partition any
% hybrid octant.
%
% See the sequential_octants record for further details.


-record( hybrid_octree_term, {

	% The overall octant identifier (octant_id()) of this octant in the overall
	% octree can be determined from the two next fields:

	height :: octree:height(),
	% The height of this octant in the overall octree.

	short_id :: octree:octant_short_id(),
	% The identifier of this octant, for its height in the overall octree.

	parent_id :: maybe( octree:octant_short_id() ),
	% The identifier of the parent (if any) of this octant, at a decremented
	% height in the overall octree.

	children :: octree:hybrid_octants(),
	% The up to 8 child (incremented-level) octants that may partition the
	% current octant.

	%siblings :: [ octree:hybrid_octant_short_id() ],
	% A list of the up to 26 sibling (same-level) neighbour octants of the
	% current octant.

	center :: point3:point3(),
	% The (geometrical) center of this octant.

	half_len :: linear:distance()
	% The half of the side length of this octant.

} ).
