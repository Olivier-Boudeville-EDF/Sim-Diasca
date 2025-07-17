% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Saturday, March 2, 2024.

-module(reference_tree).

-moduledoc """
Module implementing the support for **trees of reference frames** (frames of
references).

Such a tree (abbreviated as a "ref tree"; and also known as a "scene graph")
keeps track of the parent/child relationships between reference frames.

The root of a reference tree corresponds to the identity transform.

The tree holds notably an associative table of reference frames, so that each of
them can be easily looked up based on its identifier.

We expect mostly transition matrices to be held by a reference tree (e.g. no
scaling expected), as child frames would inherit them, which may not be
desirable (for example if a camera was attached to such a scaled frame).

A reference tree also caches information (e.g. paths between frames).

See `reference_frame3` for example.
""".


% Implementation notes:
%
% Each node of a reference tree of interest is a reference frame, with a
% "pointer" (an identifier-based reference, corresponding to a table) to the
% parent one (if any), and a transformation (whose reference matrix is the
% transition matrix from the node to its parent - the directed arrow is their
% representation, and whose inverse one is thus the transition matrix from the
% parent to this node). So by design, in terms of transition matrices (not
% reference designators) we can freely navigate the branches of the reference
% tree both ways.

% Initially the root node was implicit (node #0, yet not existing) and a frame
% may not have a parent, yet for optimisations like children or path caching, it
% became an actual node (and thus it is the only frame not having a parent).

% All examples given relate to the following reference tree (R for local
% reference frames, C for cameras; identifiers prefixed with '#'):
%
% Root
% ├── R1
% │   ├── C2
% │   └── R2
% │       └── R3
% └── R4
%     └── C1
%
% Let's suppose that we want to use the transition matrix from a reference frame
% R3 (e.g. a local coordinate system of an object) to another C2 (e.g. the one
% of a camera). The general mode of operation is first to climb the reference
% tree from the source (R3), then to go down to reach the target (C2).
%
% The shortest path is to go from R3 to the nearest common parent to R3 and C2,
% namely R1, and then to go from R1 to C2. Pros: most direct computations, less
% numerical errors. Cons: no easy reuse of them.
%
% Another path is to go from R3 to the tree root, and from there to C2. Cons:
% includes possibly many multiplications by a matrix and later by its inverse,
% like here for the transformation between R1 and the root frame (with caching,
% almost no runtime processing overhead incurred, but possibly more rounding
% errors). Pros: then, for each node, an (optional) transformation to the root
% frame can be stored (cached). We could then obtain the transform from R3 to C2
% thanks to only one matrix multiplication. The root node would then have to
% record up to one transformation for each of the other nodes.
%
% In both cases we should precompute or cache the paths involved for each pair
% of endpoints, as a triplet made of a "up moves" (climbing upward to the root)
% list and then of "down moves" (going downward, root-to-leaves direction), and
% then any already computed transform:
%
% ref_path() :: {UpMoves :: [designated_ref()], DownMoves :: [designated_ref()],
% option(transform())}.
%
% For example to reach C2 from R3, the shortest path that includes these
% endpoints would be: PAs={[R3,R2,R1],[C2]} (meaning R3 -> R2 -> R1 -> C2),
% while the path through root would be: PAr={[R3,R2,R1,Root],[R1,C2]}.
%
% Either way such a path_table({From :: designated_ref(), To ::
% designated_ref()}) -> ref_path() could store the known paths.
%
% When a reference frame would change, it would invalidate all paths involving
% its node or any of its child ones (direct or not). This would a bit complex to
% determine directly, thus the dependency of paths w.r.t to nodes would have to
% be tracked, typically by assigning identifiers to paths and maintaining a
% dedicated table(designated_ref(), path_id()) in order to, whenever a
% designated frame changes, invalidate the corresponding paths easily.
%
% Yet such caching is a bit complex to implement and, at least in some cases
% (e.g. most frames of reference rotating, including the rather top-level ones),
% this optimisation may be counter-productive (invalidating most of the time
% most of the paths, each one being actually used once, caching being then
% counter-productive).
%
% So, at least for the moment, we rely on dynamic algorithm to compute
% transformations across paths (not caching these transformations), knowing that
% nevertheless child nodes and paths are cached, and that only shortest paths
% are used. Also we cache only the requested end-to-end paths, not their
% intermediate subpaths, as the latter are unlikely to be requested.
%
% Anyway all algorithms may respect the same API, see get_transform/3.

% Note also that the actual reference frames currently supported by ref trees
% are 3D ones (ref3), yet other dimensions could easily be supported; also all
% reference designators are currently records, never PIDs; this is to be
% generalised later.

% We devised both optimised, "in-line" algorithms (e.g. to determine children
% and paths based on cached datastructures being maintained at each change) and
% "direct" algorithms (stateless, non-cached, brute-force, recursive ones). The
% latter ones, intentionally unoptimised but safe, may be used to check the
% former ones.



-doc """
An identifier of a reference frame.

This is typically a key in an (implicit) reference table, at least generally a
ref3_table().

The null (zero) identifier is reserved; it is used by reference trees to
designate the root, absolute reference frame.
""".
-type ref_id() :: count().



-doc """
An identifier of a 3D reference frame.

This is typically a key in an (implicit) ref3_table().

The null (zero) identifier is reserved; it is used by reference trees to
designate the root, absolute reference frame.
""".
-type ref3_id() :: ref_id().



-doc """
A list of the identifier of the (direct) child frames of reference of a given
frame.
""".
-type child_ids() :: [ ref_id() ].



-doc """
A table associating to a given identifier a designated reference frame.

It includes the root node, as we may want for example to record its name, its
children, etc.
""".
-type ref_table() :: table( ref_id(), designated_ref() ).



-doc """
A path from a frame A to a frame B is described as the (ordered) "Up" list of
all reference frames from A to the selected common parent (not including A), and
the (ordered) "Down" list of all reference frames from that parent to B (not
including B).

Two different lists are used as we need to record, for each node, the direction
of the corresponding transformation, that is whether we shall apply Tuv or its
inverse Tvu.

For instance, for the path from the example frame g to the frame e, Up=[Tfa,Tas]
(as Tgf is not included) and Down=[Tsb] (as Teb is not included).

With a reference frame-based notation (knowing that the previous
transformation-based notation is more relevant), this corresponds to Up=[Rf,Ra]
Down=[Rb] (Rs is not included either as it is only meant to deal with its own
parent).
""".
-type id_path() :: { Up :: [ ref_id() ], Down :: [ ref_id() ] }.



-doc """
The endpoints corresponding to an id_path/0 (and are not included in it), that
is a path between two reference frames.
""".
-type path_endpoints() :: { From :: ref_id(), To :: ref_id() }.



-doc """
A table recording known paths, queried based on their reference frame endpoints.
""".
-type path_table() :: table( path_endpoints(), id_path() ).


% For record and define:
-include("reference_frame3.hrl").



-doc """
A reference tree, also known as a scene graph.

Note that the root, absolute reference frame is to be designated by the null
(zero) reference frame identifier (see the root_ref_id define).
""".
-type reference_tree() :: #reference_tree{}.




-export_type([ ref_id/0, ref3_id/0, child_ids/0, ref_table/0,
			   id_path/0, path_endpoints/0, path_table/0,
			   reference_tree/0 ]).


-export([ new/0,
		  register/2, resolve_path/3, get_transform/3,
		  get_reference_table/1, set_reference_table/2,

		  check/1,

		  ref3_to_string/2, ref3_to_short_string/2, id_path_to_string/2,
		  to_string/1, to_string/2, to_full_string/1 ] ).

% Silencing:
-export([ get_children_direct/2, get_path_from_root/2 ]).


-compile({ no_auto_import, [ register/2 ] }).



% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type verbosity_level() :: text_utils:verbosity_level().

-type reference_frame() :: reference_frame:reference_frame().

-type designated_ref() :: reference_frame:designated_ref().

-type transform4() :: transform4:transform4().



-doc "Creates an (empty) reference tree.".
-spec new() -> reference_tree().
new() ->
	% Identity:
	RootRef3 = reference_frame3:new(),

	InitRefTable = table:singleton( _RefIdK=?root_ref_id, _V=RootRef3 ),

	#reference_tree{ ref_table=InitRefTable,
					 path_table=table:new() }.



-doc """
Registers the specified reference frame(s) in the specified reference tree;
returns the identifier allocated to that frame or these frames (then in the same
order as specified) in the tree, and the corresponding updated version of that
tree.
""".
-spec register( reference_frame(), reference_tree() ) ->
								{ ref_id(), reference_tree() };
			  ( [ reference_frame() ], reference_tree() ) ->
								{ [ ref_id() ], reference_tree() }.
register( Ref3=#reference_frame3{ parent=MaybeParentId },
		  RefTree=#reference_tree{ ref_table=RefTable,
								   next_ref_id=NewId } ) ->

	% Recording in our parent that we are one of its children with {NewParentId,
	% NewParent}:
	%
	ParentPair = case MaybeParentId of

		undefined ->
			% Implicitly the root node then, which cannot be registered that
			% way (only registered once, at RefTable creation):
			%
			throw( { not_registering_root_reference_frame, Ref3 } );

		ParentId ->
			ParentRef3 = table:get_value( ParentId, RefTable ),
			NewChildren = [ NewId | ParentRef3#reference_frame3.children ],
			{ ParentId, ParentRef3#reference_frame3{ children=NewChildren } }

	end,

	AugmRefTable = table:add_entries( [ { _K=NewId, _V=Ref3 }, ParentPair ],
									  RefTable ),

	NewRefTree = RefTree#reference_tree{ ref_table=AugmRefTable,
										 next_ref_id=NewId+1 },

	{ NewId, NewRefTree };

register( Ref3s, RefTree ) ->
	register( Ref3s, _AccIds=[], RefTree ).


% (helper)
register( _Ref3s=[], AccIds, AccRefTree ) ->
	{ lists:reverse( AccIds ), AccRefTree };

register( _Ref3s=[ Ref3 | T ], AccIds, AccRefTree ) ->
	{ ThisId, ThisRefTree } = register( Ref3, AccRefTree ),
	register( T, [ ThisId | AccIds ], ThisRefTree ).



-doc """
Returns the resolved (cached otherwise computed) path from the first reference
frame whose identifier is specified to the second one.

As the reference tree may be updated in the process, returns one.
""".
-spec resolve_path( ref_id(), ref_id(), reference_tree() ) ->
										{ id_path(), reference_tree() }.
resolve_path( FromRefId, ToRefId,
			  RefTree=#reference_tree{ path_table=PathTable } ) ->

	PathEndpoints = { FromRefId, ToRefId },

	case table:get_value_with_default( _K=PathEndpoints, _DefValue=undefined,
									   PathTable ) of

		undefined ->
			compute_path( PathEndpoints, PathTable, RefTree );

		Path ->
			{ Path, RefTree }

	end.



-doc """
Returns the computed path from the first reference frame whose identifier is
specified to the second one, and an updated tree.
""".
-spec compute_path( path_endpoints(), path_table(), reference_tree() ) ->
										{ id_path(), reference_tree() }.
compute_path( PathEndpoints={ FromRefId, ToRefId }, PathTable,
			  RefTree=#reference_tree{ ref_table=RefTable } ) ->

	% As paths are quite cheap to compute, it is probably not interesting to
	% derive this path from any of its possibly cached subpaths.

	% To determine such path, we combine two root-to-node paths; we refer here
	% to the example at the top of this file, for example resolving path from
	% (source, S) R3 to (target, T) C2, based on a root pivot (R1).

	% Either cached or computed; for example: [R1] (Root and C2 implicit).
	{ RootToTargetPath, FirstRefTable } =
		get_path_from_root( ToRefId, RefTable ),

	% For example: [R1, R2].
	{ RootToSourcePath, SecondRefTable } =
		get_path_from_root( FromRefId, FirstRefTable ),

	% The actual shortest path is going up with R3, R2, and R1, then going down
	% with C2, thus an id_path() of {[R2],[C]} (endpoints not listed, and parent
	% R1 neither as deals with the upward part of the tree). To obtain it, we
	% skip the common prefix to both root-to-node paths and branch them based on
	% their last common node (if any), the root-to-source part being reversed:

	IdPath = skip_and_branch( RootToSourcePath, RootToTargetPath ),

	% As paths are quite cheap to compute, it is probably not interesting to
	% derive and store the subpaths of this path.

	% (add_new_entry suitable as well):
	AugmentedPathTable = table:add_entry( _K=PathEndpoints, _V=IdPath,
										  PathTable ),

	NewRefTree = RefTree#reference_tree{ ref_table=SecondRefTable,
										 path_table=AugmentedPathTable },

	{ IdPath, NewRefTree }.


% (helper)
% Skip common prefixes (including the closest common parent, not kept):
skip_and_branch( _FirstPath=[ H | FirstT ], _SecondPath=[ H | SecondT ] ) ->
	skip_and_branch( FirstT, SecondT );

% From here their heads do not match anymore:
skip_and_branch( FirstPath, SecondPath ) ->
	_IdPath={ lists:reverse( FirstPath ), SecondPath }.



-doc """
Returns the (4x4) transformation between the first specified frame of reference
and the second one (based on their identifier in the specified reference tree),
together with a (possibly updated, for example regarding to paths) version of
the specified reference tree.

More precisely, for two reference frames Ra and Rb, returns Tab so that its
reference matrix corresponds to the transition from Ra to Rb (and thus its
inverse corresponds to: from Rb to Ra).

Does not cache this resulting reference frame (at least for the moment).
""".
-spec get_transform( designated_ref(), designated_ref(), reference_tree() ) ->
										{ transform4(), reference_tree() }.
get_transform( FromRefId, ToRefId, RefTree ) ->

	{ IdPath={ Up, Down }, PathedRefTree } =
		resolve_path( FromRefId, ToRefId, RefTree ),

	trace_utils:debug_fmt( "Getting transform from frame #~B to #~B: up is ~w, "
						   "down is ~w.", [ FromRefId, ToRefId, Up, Down ] ),

	% Preferring any latest version of tree, just in case:
	RefTable = PathedRefTree#reference_tree.ref_table,

	trace_utils:debug_fmt(
		"Path from ~ts to ~ts: ~ts",
		[ ref3_to_short_string( FromRefId, RefTable ),
		  ref3_to_short_string( ToRefId, RefTable ),
		  id_path_to_string( IdPath, RefTable ) ] ),


	[ FromRef, ToRef ] = table:get_values( [ FromRefId, ToRefId ], RefTable ),

	% To understand the proper multiplication of the matrices of the reference
	% frames, let's suppose we have the reference tree presented in the
	% Ceylan-Howtos
	% (http://howtos.esperide.org/ThreeDimensional.html#a-tree-of-coordinate-systems)
	% with an additional Ri, child of Re, and that we want to determine Tgi.
	%
	% For a vector V, Vi = Tgi.Vg, and
	% Tgi = (Tie^-1.Teb^-1.Tbs^-1).(Tas.Tfa.Tgf) = Tdown.Tup.

	% Tdown is the left-multiplication of the inverse matrices along the down
	% path (Tbs^-1, then Teb^-1.Tbs^-1, etc.), while Tup is the
	% right-multiplication of the reference matrices along the up path (Tas,
	% then Tas.Tfa, etc.); so we can start for example from:

	DownTransf4 = mult_down_transforms( Down, transform4:identity(), RefTable ),

	% As the final endpoint is not in Down:
	FullDownTransf4 = transform4:mult(
		reference_frame3:get_inverse_transform( ToRef ), DownTransf4 ),

	% Now the up:
	UpTransf4 = mult_up_transforms( Up, FullDownTransf4, RefTable ),

	% As the initial endpoint is not in Up:
	FinalTransf4 = transform4:mult( UpTransf4,
									FromRef#reference_frame3.transform ),

	{ FinalTransf4, PathedRefTree }.


% Version for transformations from leaves to parent.
%
% (helper)
mult_up_transforms( _Up=[], Transf4, _RefTable ) ->
	Transf4;

mult_up_transforms( _Up=[ RefId | T ], Transf4, RefTable ) ->
	Ref = table:get_value( _K=RefId, RefTable ),
	NewTransf4 = transform4:mult( Transf4, Ref#reference_frame3.transform ),
	mult_down_transforms( T, NewTransf4, RefTable ).


% Version for transformations from parent to leaves.
%
% (helper)
mult_down_transforms( _Down=[], Transf4, _RefTable ) ->
	Transf4;

mult_down_transforms( _Down=[ RefId | T ], Transf4, RefTable ) ->
	Ref = table:get_value( _K=RefId, RefTable ),
	InvRefTransf4 = reference_frame3:get_inverse_transform( Ref ),
	NewTransf4 = transform4:mult( InvRefTransf4, Transf4 ),
	mult_down_transforms( T, NewTransf4, RefTable ).



-doc """
Returns the reference table of the specified reference tree.

Useful to avoid that the caller has to include this corresponding header file,
typically for tests.
""".
-spec get_reference_table( reference_tree() ) -> ref_table().
get_reference_table( #reference_tree{ ref_table=RefTable } ) ->
	RefTable.



-doc """
Sets the specified reference table in the specified reference tree.

Useful to avoid that the caller has to include this corresponding header file,
typically for tests.
""".
-spec set_reference_table( ref_table(), reference_tree() ) -> reference_tree().
set_reference_table( RefTable, RefTree ) ->
	RefTree#reference_tree{ ref_table=RefTable }.



-doc """
Returns the children of the specified 3D frame of reference, based on a direct,
stateless look-up (hence the reference table is not modified and thus not
returned).
""".
-spec get_children_direct( ref_id(), ref_table() ) -> child_ids().
get_children_direct( RefId, RefTable ) ->
	% Brute-force, as speed is irrelevant for this implementation:
	RefPairs = table:enumerate( RefTable ),

	% Deemes clearer than a filtering list comprehension:
	get_children_direct( RefId, RefPairs, _ChildAcc=[] ).


% (helper)
get_children_direct( _RefId, _RefPairs=[], ChildAcc ) ->
	ChildAcc;

% Target parent matching:
get_children_direct( RefId,
		_RefPairs=[ { RId, _Ref3=#reference_frame3{ parent=RefId } } | T ],
					 ChildAcc ) ->
	get_children_direct( RefId, T, [ RId | ChildAcc ] );

get_children_direct( RefId, _RefPairs=[ _OtherPair | T ], ChildAcc ) ->
	get_children_direct( RefId, T, ChildAcc ).



-doc """
Returns the path from the root reference frame to the specified one, using any
cached information or caching any newly processed path, together with a possibly
updated reference table.
""".
-spec get_path_from_root( ref_id(), ref_table() ) -> { id_path(), ref_table() }.
get_path_from_root( _TargetRefId=?root_ref_id, RefTable ) ->
	{ _IdPath=[], RefTable };

get_path_from_root( TargetRefId, RefTable ) ->
	TargetRef3 = table:get_value( TargetRefId, RefTable ),
	case TargetRef3#reference_frame3.path_from_root of

		undefined ->
			% Not wanting to include the specified node in the path:
			StartRefId = TargetRef3#reference_frame3.parent,
			ComputedPath = get_path_from_root_direct( StartRefId, RefTable ),

			% Storing this path:
			PathedRef3 =
				TargetRef3#reference_frame3{ path_from_root=ComputedPath },

			ThisNodeRefTable =
				table:add_entry( _K=TargetRefId, PathedRef3, RefTable ),

			% Reversed to chop the unit/head more efficiently afterwards, hence
			% from leaf to node:
			%
			RevComputedPath = lists:reverse( ComputedPath ),

			% Applies the sub-path to all intermediate nodes:
			PathedRefTable = apply_path( StartRefId, ThisNodeRefTable,
										 RevComputedPath ),

			{ ComputedPath, PathedRefTable };

		IdPath ->
			% Trust the cached paths:
			{ IdPath, RefTable }

	end.



% Applies the relevant shortened version of the specified identifier path to
% each node from the specified node to the root one.
%
% (helper)
%
% Stopped before:
%apply_path( _RefId=?root_ref_id, RefTable, _RevIdPath=[] ) ->
%   RefTable;
%
apply_path( _RefId, RefTable, _RevIdPath=[] ) ->
	RefTable;

apply_path( RefId, RefTable, _RevIdPath=[ _PrevRefId | ThisRevIdPath ] ) ->
	ThisIdPath = lists:reverse( ThisRevIdPath ),

	%trace_utils:debug_fmt( "At frame #~B, path is ~w.",
	%    [ RefId, ThisIdPath ] ),

	Ref3 = table:get_value( RefId, RefTable ),
	PathedRef3 = Ref3#reference_frame3{ path_from_root=ThisIdPath },
	ThisNodeRefTable = table:add_entry( _K=RefId, PathedRef3, RefTable ),

	apply_path( Ref3#reference_frame3.parent, ThisNodeRefTable, ThisRevIdPath ).



-doc """
Returns the path from the root reference frame to the specified one, not
including the root frame but including the current node (hence not the same
convention as id_path()) based on a direct, stateless look-up (hence the
reference table is not modified and thus not returned).
""".
-spec get_path_from_root_direct( ref_id(), ref_table() ) -> id_path().
get_path_from_root_direct( RefId, RefTable ) ->
	get_path_from_root_direct( RefId, RefTable, _AccIds=[] ).


% (helper)
%
% Root reached:
get_path_from_root_direct( _RefId=?root_ref_id, _RefTable, AccIds ) ->
	% Already in the expected order:
	AccIds;

get_path_from_root_direct( NonRootRefId, RefTable, AccIds ) ->
	NonRootRef = table:get_value( NonRootRefId, RefTable ),
	ParentRefId = NonRootRef#reference_frame3.parent,
	get_path_from_root_direct( ParentRefId, RefTable,
							   [ NonRootRefId | AccIds ] ).



-doc """
Checks the integrity of the specified reference tree.

Throws an exception if found inconsistent.

Designed for safety, not for speed.
""".
-spec check( reference_tree() ) -> void().
check( #reference_tree{ ref_table=RefTable, next_ref_id=NextId } ) ->
	% No duplicate identifier possible:
	AllRefPairs = table:enumerate( RefTable ),

	AllRefIds = pair:firsts( AllRefPairs ),

	MaxId = lists:max( AllRefIds ),
	MaxId >= NextId andalso throw( { invalid_ref_id, NextId, MaxId } ),

	% No more range check, as reference frames may have been removed.

	[ check_node( RefId, RefDes, AllRefIds, RefTable )
		|| { RefId, RefDes } <- AllRefPairs ],

	% Now that children are checked, crawl the whole tree, checking that all
	% nodes are connected; will not terminate if a cycle exists:
	%
	FinalRefTable = check_all_connected( _CurrentNodeId=?root_ref_id,
										 RefTable ),

	table:is_empty( FinalRefTable ) orelse
		throw( { unconnected_nodes, table:enumerate( FinalRefTable ) } ),

	[ check_path( RefId, RefDes, RefTable )
		|| { RefId, RefDes } <- AllRefPairs ].



-doc """
Checks the integrity of the specified node (reference frame) of the specified
reference tree.

Throws an exception if found inconsistent.

Designed for safety, not for speed.
""".
-spec check_node( ref_id(), designated_ref(), [ ref_id() ],
				  ref_table() ) -> void().
check_node( RefId, #reference_frame3{ parent=MaybeParent,
									  children=StoredChildren,
									  path_from_root=_FromRootPath,
									  transform=Transform4 },
			AllRefIds, RefTable ) ->
	% Any parent must be known:
	case MaybeParent of

		undefined ->
			ok;

		ParentId ->
			lists:member( ParentId, AllRefIds ) orelse
				throw( { non_registered_parent, MaybeParent, RefId } )

	end,

	DeducedChildren = get_children_direct( RefId, RefTable ),

	list_utils:unordered_compare( StoredChildren, DeducedChildren ) orelse
		throw( { inconsistent_children, RefId, StoredChildren,
				 DeducedChildren } ),

	transform4:is_transform4( Transform4 ) orelse
		throw( { invalid_transform4, Transform4 } ).


% Checks by removing all nodes traversed from the root.
%
% (helper)
-spec check_all_connected( ref_id(), ref_table() ) -> ref_table().
check_all_connected( CurrentNodeId, RefTable ) ->
	{ Ref3, ShrunkRefTable } =
		table:extract_entry( _K=CurrentNodeId, RefTable ),

	Children = Ref3#reference_frame3.children,

	lists:foldl( fun( NId, AccRefTable ) ->
					_ShrkAccRefTable=check_all_connected( NId, AccRefTable )
				 end,
				 _Acc0=ShrunkRefTable,
				 Children ).



% Checks that any cached path in this reference frame is correct.
check_path( _RefId, #reference_frame3{ path_from_root=undefined },
			_RefTable ) ->
	ok;

check_path( RefId, #reference_frame3{ path_from_root=IdPath }, RefTable ) ->

	%trace_utils:debug_fmt( "Checking for frame #~B path ~w.",
	%                       [ RefId, IdPath ] ),

	follow_path( _CurrentNodeId=?root_ref_id, _ToNodeId=RefId, RefTable,
				 list_utils:append_at_end( RefId, IdPath ) ).



% Follows the specified path, from node to node, for checking.
%
% (helper)
%
follow_path( CurrentNodeId, _ToNodeId=CurrentNodeId, _RefTable, _IdPath=[]) ->
	ok;

follow_path( CurrentNodeId, ToNodeId, RefTable,
			 _IdPath=[ NextNodeId | NextPath ] ) ->
	CurrentNode = table:get_value( CurrentNodeId, RefTable ),
	CurrentChildren = CurrentNode#reference_frame3.children,
	case lists:member( NextNodeId, CurrentChildren ) of

		true ->
			follow_path( NextNodeId, ToNodeId, RefTable, NextPath );

		false ->
			throw( { invalid_path, CurrentNode, NextNodeId, CurrentChildren } )

	end.



-doc "Returns a textual representation of the specified 3D frame of reference.".
-spec ref3_to_string( ref_id(), ref_table() ) -> ustring().
ref3_to_string( RefId, RefTable ) ->
	Ref = table:get_value( RefId, RefTable ),
	case Ref#reference_frame3.name of

		undefined ->
			text_utils:format( "frame #~B", [ RefId ] );

		BinRefName ->
			text_utils:format( "frame '~ts' (#~B)", [ BinRefName, RefId ] )

	end.



-doc """
Returns a compact textual representation of the specified 3D frame of reference.
""".
-spec ref3_to_short_string( ref_id(), ref_table() ) -> ustring().
ref3_to_short_string( RefId, RefTable ) ->
	Ref = table:get_value( RefId, RefTable ),
	case Ref#reference_frame3.name of

		undefined ->
			text_utils:format( "#~B", [ RefId ] );

		BinRefName ->
			text_utils:format( "'~ts'", [ BinRefName ] )

	end.



-doc """
Returns a textual representation of the specified identifier path.

Note that internally the transformations from a given frame to its parent are
managed, thus the closest parent ("top frame" of this path) of the endpoints of
a path is not listed in the path.
""".
-spec id_path_to_string( id_path(), ref_table() ) -> ustring().
id_path_to_string( _IdPath={ _Up=[], _Down=[] }, _RefTable ) ->
	"empty path";

id_path_to_string( _IdPath={ Up, Down }, RefTable ) ->
	% No reversing needed:
	Path = Up ++ Down,
	IdStrs = [ ref3_to_short_string( RefId, RefTable ) || RefId <- Path ],
	text_utils:join( _Sep=" -> ", IdStrs ).



-doc """
Returns a (short) textual representation of the specified reference tree.
""".
-spec to_string( reference_tree() ) -> ustring().
to_string( RefTree ) ->
	to_string( RefTree, _VerbLevel=low ).



-doc """
Returns a (short) textual representation of the specified reference tree, for
the specified verbosity level.
""".
-spec to_string( reference_tree(), verbosity_level() ) -> ustring().
to_string( #reference_tree{ ref_table=RefTable,
							path_table=PathTable }, _VerbLevel=low ) ->
	text_utils:format( "reference tree tracking ~ts, and caching ~ts",
		[ text_utils:table_to_string( RefTable, _EntryDesc="reference frame" ),
		  text_utils:table_to_string( PathTable, _EntDesc="path" ) ] );

to_string( _RT=#reference_tree{ ref_table=RefTable,
								path_table=PathTable }, _VerbLevel=high ) ->
	%trace_utils:debug_fmt( "Ref tree: ~p", [ RT ] ),
	NodeToStringDefFun = fun ref3_to_string/2,
	NodeToChildrenFun = fun get_children_direct/2,

	text_utils:format( "reference tree caching ~ts: ~ts", [
		text_utils:table_to_string( PathTable, _EntryDesc="path" ),
		tree:forest_to_string( RefTable, _FromNodeId=?root_ref_id,
							   NodeToStringDefFun, NodeToChildrenFun ) ] ).



-doc """
Returns a rather full textual representation of the specified reference tree,
typically for debugging purposes.
""".
-spec to_full_string( reference_tree() ) -> ustring().
to_full_string( _RT=#reference_tree{ ref_table=RefTable,
									 path_table=PathTable } ) ->

	FrameStrs = [ reference_frame3:node_to_string( Ref3Id, Ref3 )
					|| { Ref3Id, Ref3 } <- table:enumerate( RefTable ) ],

	text_utils:format( "reference tree registering ~B frames of reference: ~ts"
		"~nand caching paths based on: ~ts",
		[ table:size( RefTable ), text_utils:strings_to_string( FrameStrs ),
		  table:to_string( PathTable ) ] ).
