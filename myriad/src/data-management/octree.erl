% Copyright (C) 2022-2024 Olivier Boudeville
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


% @doc Gathering of facilities to manage <b>octrees</b>, a tree datastructure in
% which each internal node has exactly 8 children (octants, i.e. sub-octrees or
% "cells" - a naming that is probably clearer) that partition the parent space.
%
% We consider here that the octree acts as a server in charge of resolving
% spatial queries emitted by many clients having to interact in a shared space.
%
% A global axis-aligned right cuboid is recursively split in 8 smaller,
% same-sized octants, (which are its direct child right cuboids), dividing the
% space thanks to a tree until some criterion is met (volume/number of elements
% in the resulting leaves below some threshold, maximum tree depth reached,
% etc.).
%
% See https://en.wikipedia.org/wiki/Octree.
%
-module(octree).


% Design notes:
%
% Other related datastructures:
% - kd-trees (see https://en.wikipedia.org/wiki/K-d_tree)
% - "Bounding Volume Hierarchy" (BVH):
% https://en.wikipedia.org/wiki/Bounding_volume_hierarchy
% - "Binary space partitioning" (BSP):
% https://en.wikipedia.org/wiki/Binary_space_partitioning


% Octrees vs kd-trees: octrees are tries, kd-trees are balanced binary trees.

% Advantages of octrees:
%
% - high branching factor (8 for octrees) means shallower trees that incur fewer
% indirections, so searching may be fast, especially for homogeneous
% distributions
%
% - insertions and deletions are costly in kd-trees (rebalancing having to be
% done, whereas octrees do not have to be rebalanced); yet balancing handles
% heterogeneity better because it is adaptive, while searches in imbalanced
% tries (octrees) may require many indirections
%
% - bisection (as in octrees) lends itself to trivial implementation in terms of
% bit-twiddling; octrees can benefit greatly from precomputed distances when
% doing range lookups
%
% - kd-trees can have high aspect ratio (dissimilardimensions), whereas octree
% cells are guaranteed to be cubical; it makes it impossible to use volume
% bounds to control the number of cells that you have to examine when solving
% approximate nearest neighbor queries


% Advantages of kd-trees:
%
% - constructing and querying k-nearest neighbours: kd-trees are typically
% superior in performance for most datasets
%
% - kd-trees are guaranteed to have at most logarithmic depth (yet in general
% are deeper than octrees due to theiur binary subdivision); worst time
% complexity is lower than octrees


% Sources:
%
% - https://cstheory.stackexchange.com/questions/8470/why-would-one-ever-use-an-octree-over-a-kd-tree
% - https://observablehq.com/@2talltim/spatial-data-structures-octrees-bsp-and-k-d-trees
% - https://observablehq.com/@2talltim/spatial-data-structures-octrees-bsp-and-k-d-trees
% - https://en.wikipedia.org/wiki/Quadtree (to be generalised from 2D to 3D, in
% octrees), notably https://en.wikipedia.org/wiki/Quadtree#Region_quadtree

% Some "octrees" may be unevenly divided (like in
% https://en.wikipedia.org/wiki/Quadtree#Point_quadtree).


% Specific requirements addressed here:

% We want a datastructure to track a large number of *moving* objects in a
% larger space, so octrees may be more relevant as migrating in a kd-tree is
% costlier); yet the likely heterogeneous distribution of objects would plead
% for kd-trees.
%
% The rather high branching factor of an octree (8) is probably an advantage in
% languages like Erlang where no pointer arithmetics or even reference-to-term
% exists; more importantly, each octant may be a process, transforming the
% overall octree in a concurrent datastructure; this module offers such an
% octree.
%
% With larger global spaces, to each octant an absolute, fixed referential can
% be associated, whose origin is defined relatively to the center/origin of its
% parent. The corresponding transformation, possibly also together with another
% precomputed one from the local octant to the (unique, top-level) root one,
% would allow each object to be defined only relatively to the (fixed)
% referential corresponding to its (current) octant; by enabling smaller
% floating-point distances, numerical errors should be significantly lessened.
%
% Note that multiple, different datastructures may be used (created and updated)
% simultaneously, so that each type of operation (e.g. query) can be performed
% on the most efficient one.


% Our octrees:
%
% - are not balanced, in the sense that their octants will be split according to
% local criteria - rather than uniformly
%
% - internal nodes (i.e. non-leaf nodes) may contain their own elements as well,
% if they are container nodes); this may be convenient for:
%
%  (1) (non-punctual, typically rigid-body) elements that would not fit in
%  smaller cells (that is: larger objects)
%
%  (2) rapidly-moving objects that would keep on transitioning between neighbour
%  cells (e.g. a planet orbiting across multiple elementary cells)
%
%  (3) to store aggregate information, variable resolution representation of a
%  data field (e.g. average value - like color, temperature, mass/gravitational
%  field - of the elements recursively contained)
%
%  However queries then have to take into account additionally the full path
%  from the current cell to the root one - which might not be desirable; this is
%  why some nodes, typically static ones, do not store any dynamic content
%  (these are pure nodes). A static octant can then be replicated (duplicated
%  verbatim across multiple process, for increased scalability).
%
%  As for container nodes, they may by default contain any number of elements
%  (typically as a set of references to objects); this translates to "unlimited
%  bucket capacity" (as opposed to, ay, exactly one object per cell)
%


% An octree partitions a space: at any height of the octree, any point of this
% space must be contained by is inside exactly one octant.
%
% The faces of an octant whose normals, pointing from inside to outside this
% octant, are the +X, +Y and +Z axes, are considered belonging to that octant,
% as opposed to the faces whose normals are the -X, -Y and -Z axes.
%
% To resolve proximity queries, at best the search remains in the current octant
% (if the corresponding sphere is fully included in it), otherwise, depending on
% the search radius, any number of other octants may have to be visited.
%
% At worst, for adjacent searches, 26 sibling neighbours have to be considered,
% belonging to the current parent octant and to up to 3+4=7 of its siblings
% (should all octants be instantiated). Searches of longer ranges will have to
% traverse more octants.
%
% To facilitate such searches, an octant must know its direct neighbours
% (including the diagonal ones) - yet only at its own subdivision level in the
% octree. Octants have to be referenced, and for that a (per-height) octant
% table is maintained and used.


%-type sibling_table() :: table( octant_short_id(), octree_designator() ).
% A table of all octants available at a given height of an octree.


% For an octree of height H (defined as the maximum number of edges (e.g. not
% vertices) traversed from root to leaves, its leaves define (up to) 8^H
% elementary cells; for a global space that would be a cube of side length L
% (hence of volume L^3), each of these cells would be of length L/2^H (hence of
% volume L^3/8^H).
%
% So the side length of a leaf cell of an octree of height H in a world whose
% diameter is D is given by: SL = fun(H) -> D/math:pow(2,H) end.
%
% The total number of cells (leaves or not) for a (totally populated) octree of
% height H is: NC(H)=(8^(H+1)-1)/7, i.e.:
% 'NC = fun(H) -> (math:pow(8,H+1)-1)/7 end.'.
%
% Ex: let's D be the diameter of the observable universe (8.8.10^26 m,
% i.e. D=8.8e26). SL(64) is about 48 000 km, yet NC(64) is 7.1e57. Luckly the
% universe is mostly empty.



% How static, persistent terms could be used for nodes but should not.

% For larger spaces to partition, a setting could be to define a depth threshold
% DT where:
%
% - before DT (i.e. in the top part of the overall octree, the one closest to
% its root) there would be the static part of the octree, where all nodes (and
% the direct process-based children of their leaves) would be pre-created (so
% that this overall term can never be updated), each node being static (hence
% pure, empty of content) and directly represented as a term, held by each
% client process (and shared as much as possible thanks to persistent_term, as
% this part of the overall octree, once constructed, is by design never updated)
%
% - after DT (i.e. in the lower part of the overall octree, closer to its
% leaves), each node would be dynamic (process-based container node), for
% maximum scalability
%
% Note that the use of persistent_term would then be a lot more efficient than
% the use of pool-based replicated processes, in charge of resolving requests
% regarding the constant part of the octree on behalf of the subsets of the
% clients that were allocated to them (such a process-based top level could
% indeed exist in multiple copies in order to share the load across the
% clients).
%
% In this prospect, DT would have to be maximised, so that the dynamic part of
% the octree is mostly concentrated near the leaves.
%
% However the catch is that the number of pre-instantiated, static nodes becomes
% very quickly huge as the height grows, which makes such an organisation mostly
% impracticable.


% Why our octrees are fully concurrent, instead of being hybrid:

% We considered that octants could be hybrid, in the sense that they could be
% sequential and/or concurrent, each octant being either a direct, simple term,
% or a PID referencing a dedicated process in charge of that octant; a criterion
% would then have to be applied to decide whether a given octant shall be
% represented as a term or as a process; a general rule of thumb is, for the
% top-level octrees (the closer to the root), to prefer octants-as-a-term (for
% direct, quicker traversability - not involving message exchanges; and pure
% nodes) whereas for bottom-level octrees (closer to the leaves)
% octants-as-processes may be preferred (as they tend to be a lot busier, to
% manage migrations, queries, etc.; hence container, dynamic nodes) for
% scalability
%
% However from a given octant, not only do we want to reference its (direct)
% children, but also its (direct) siblings. This would involve maintaining
% per-height (dynamic) table whose keys would be octant_short_id() and whose
% values would be octree_term instances. These tables could not be easily shared
% and could become huge.
%
% More the logic to simultaneously handle octants as terms or processes becomes
% quickly quite complex and possibly not so efficient, so finally we opted for
% having all octants as processes, and thus a fully concurrent octree/

% Another way to look at this is that ultimately, the overall octree will be
% managed by a process, most probably a dedicated one - so an octree can be as a
% process before being seen as a term.
%
% The default for an octree could be then to be concurrent, knowing that at the
% level of any of its processes, more than one level of subdivision could be
% collapsed, in order to find a good sweet spot regarding spatial subdivision,
% number of locatables per cells, and term-level (process-local) computings (as
% opposed to inter-process, message-based ones).
%
% For example, knowing that the octree will already be quite disaggregated into
% many (concurrent) processes (whose exchanges might become demanding), maybe it
% could be worthwhile to have each octree manage internally 2 levels of the
% hierarchy, that is: a given octant at height H and also its (up to) 8 children
% (at height H+1), all stored as a term. That way, the computations involving
% this parent and its direct children would be purely local (operating on a term
% managed by a single process): larger and more connected process, fewer
% messages.
%
% So each of these processes could respect two limits in terms of maximum
% locatable population: the process-level one (Lp) and the cell-level (Lc)
% one. As long as Lp is not exceeded, operations remain in the current process,
% cells are filled, and split in finer cells (as terms) each time one exceeds
% Lc. If/when, Lp is reached, then the process is split into further child
% process so that the limits of all are respected.
%
% Reciprocally, should a sufficient number of locatables leave the corresponding
% space, subdividing processes could be collapsed. To avoid costly oscillations
% between two successive collapsing degrees, a bit of hysteresis shall be
% implemented: expansion would happen for example only after exceeding 120% of a
% given limit (Lp or Lc), whereas collapsing would happen only when falling
% below 80% of the same limit.

% So an octant process at height H would manage Nl>=1 levels of subdivision. It
% would maintain a table of (direct or not) child (term-based) octants for each
% of its sublevels that it manages.
%
% For example, for height H'=H+k, with k <= Nl, it would manage the table Tk of
% all its up to 8^k internal octants. Finally, a more global table would held
% all these per-level tables, associating to an internal level k the
% corresponding table Tk: table(k, Tk).


% So that we can defined our own size/1:
-compile( { no_auto_import, [ size/1 ] } ).


-type node_content() :: maybe( any() ).
% The content of a node of an octree ('undefined' meaning empty content).


% For the octant records of all sorts:
%-include("octree.hrl").



%-type sequential_octants() :: #sequential_octants{}.
% The 8 possible sequential sub-octrees.

%-type sequential_octants( _T ) :: #sequential_octants{}.
% The 8 possible sequential sub-octrees, of content T.


%-opaque sequential_octree() ::
%  { node_content(), maybe( sequential_octants() ) }.
% A sequential octree is made of its own content and of up to 8 child sequential
% octrees.
%
% Such an octant may have content of its own, even if it has at least one
% non-empty child (sub-octant). This may be useful for example for planets known
% to revolve in the current octant without being confined in any of its child
% octants; or objects large enough/positioned so that they pertain partly to one
% octant, partly to at least one another.
%
% This octree type is sequential (as opposed to concurrent), in the sense that
% it is defined as a single term (used by a given process), unlike
% concurrent_octree/0.


%-opaque sequential_octree( T ) :: { T , maybe( sequential_octants( T )  ) }.
% A typed sequential octree, polymorphic regarding its node content.
%
% See sequential_octree/0 for further details.




%-type concurrent_octants() :: #concurrent_octants{}.
% The 8 possible concurrent sub-octrees.

%-type concurrent_octants( _T ) :: #concurrent_octants{}.
% The 8 possible concurrent sub-octrees, of content T.


%-opaque concurrent_octree() ::
%  { node_content(), maybe( concurrent_octants() ) }.
% A concurrent octree is made of its own content and of up to 8 child concurrent
% octrees.
%
% Such an octant may have content of its own, even if it has at least one
% non-empty child (sub-octant). This may be useful for example for planets known
% to revolve in the current octant without being confined in any of its child
% octants; or objects large enough/positioned so that they pertain partly to one
% octant, partly to at least one another.
%
% This octree type is concurrent (as opposed to sequential), in the sense that
% it is defined as a (single) process, unlike sequential_octree/0.


%-opaque concurrent_octree( T ) :: { T , maybe( concurrent_octants( T )  ) }.
% A typed concurrent octree, polymorphic regarding its node content.
%
% See concurrent_octree/0 for further details.





%-type hybrid_octants() :: #hybrid_octants{}.
% The 8 possible hybrid sub-octrees.

%-type hybrid_octants( _T ) :: #hybrid_octants{}.
% The 8 possible hybrid sub-octrees, of content T.


%-opaque hybrid_octree() ::
%  { node_content(), maybe( hybrid_octants() ) }.
% A hybrid octree is made of its own content and of up to 8 child hybrid
% octrees.
%
% Such an octant may have content of its own, even if it has at least one
% non-empty child (sub-octant). This may be useful for example for planets known
% to revolve in the current octant without being confined in any of its child
% octants; or objects large enough/positioned so that they pertain partly to one
% octant, partly to at least one another.
%
% This octree type is hybrid (as opposed to purely sequential or concurrent), in
% the sense that it is defined either as a term or as a process.


%-opaque hybrid_octree( T ) :: { T , maybe( hybrid_octants( T )  ) }.
% A typed hybrid octree, polymorphic regarding its node content.
%
% See hybrid_octree/0 for further details.





%-type octree_term() :: #octree_term{}.
% A (thus sequential) octree-as-a-term, containing possibly a content, an octant
% record and probably more information.

%-type octree_pid() :: pid().
% The PID of a concurrent octree, in charge of a given octant.


%-type octree_designator() :: octree_term() | octree_pid().
% Designates any octree, as a term or a process.


-type height() :: count().
% The height of an octree, as the maximum number of edges between the root and a
% leaf.


-type octant_index() :: integer().
% The index of an octant is a non-null integer.

-type x_index() :: octant_index().
% The index of an octant along the X axis.

-type y_index() :: octant_index().
% The index of an octant along the Y axis.

-type z_index() :: octant_index().
% The index of an octant along the Z axis.


-type octant_id() ::
		{ H :: height(), Xi :: x_index(), Yi :: y_index(), Zi :: z_index() }.
% The identifier of an octant, expressed in terms of octant coordinates.
%
% For example {H=2, Xi=-1, Yi=2, Zi=1}.
%
% For H=0, Xi=Yi=Zi=1.
% For H=1, Xi, Yi, Zi each are either -1 or +1.
% For H=2, Xi, Yi, Zi each are in -2, -1, +1, +2.
% For H=n, Xi, Yi, Zi each are in -n, -n+1, [...], -2, -1, +1; +2, [...], n-1,
% n.


-type octant_short_id() ::
		{ Xi :: x_index(), Yi :: y_index(), Zi :: z_index() }.
% The identifier of an octant at a given height in the octree, expressed in
% terms of octant coordinates.
%
% For example {Xi=-1, Yi=2, Zi=1}, for H=2.



%-type concurrent_octree_state( T ) :: { T, concurrent_octants( T ) }.
% The state of a concurrent octree, that is a state kept by a process in charge
% of a concurrent octree.


-type content_fold_fun() ::
		fun( ( node_content(), accumulator() ) -> accumulator() ).
% Describes a function that can be folded onto the content of an octree.



%-type locatable_container() :: set( locatable_pid() ).
% Each container node (dynamic octant) references the set of the locatable
% elements whose center lies inside this octant.


-export_type([ node_content/0,

			   %sequential_octree/0, sequential_octree/1,
			   %sequential_octants/0, sequential_octants/1,

			   %concurrent_octree/0, concurrent_octree/1,
			   %concurrent_octants/0, concurrent_octants/1,
			   %concurrent_octree_state/1,

			   %hybrid_octree/0, hybrid_octree/1,
			   %hybrid_octants/0, hybrid_octants/1,

			   %octree_term/0,
			   %ctree_pid/0, octree_designator/0,

			   octant_index/0, x_index/0, y_index/0, z_index/0, octant_id/0,
			   octant_short_id/0,

			   %octree_state/1,

			   content_fold_fun/0, height/0 ]).


-export([ new/0, new/1 ]).
%% , new/2, set_content/2, append_child/2, append_children/2,
%%		  map/2, fold_breadth_first/3, fold_depth_first/3,
%%		  height/1, size/1, to_string/1 ]).



% Shorthands:

-type count() :: basic_utils:count().
-type accumulator() :: basic_utils:accumulator().

%-type ustring() :: text_utils:ustring().

%-type set( T ) :: set_utils:set( T ).


%-type point3() :: point3:point3().


% May actually be better defined by upper layers:
%-type locatable_pid() :: pid().

%-type locatable_center() :: point3().
% Designates the center of a locatable, to be understood here as the center of
% the diameter (largest distance between two points) of this locatable.



% @doc Creates an empty (regarding content and octants), sequential, octree.
-spec new() -> any().%octree().
new() ->
	%{ _NodeContent=undefined, #octants{} }.
	fixme.


% @doc Creates a sequential, octree having specified content, and no octants.
-spec new( node_content() ) -> any(). %octree().
new( NodeContent ) ->
	{ NodeContent, _Octants=undefined }.
