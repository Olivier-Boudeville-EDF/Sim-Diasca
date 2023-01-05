% Copyright (C) 2008-2023 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: 2008.


% @doc Mesh class, to manage all kinds of <b>graph-based systems</b> (such as
% networks).
%
-module(class_Mesh).


-define( class_description, "Mesh class, to manage all kinds of graph-based "
		 "systems (such as networks)." ).



% A mesh is composed of nodes and links.
%
% Each node and each link can have an associated content (any term).
%
% If generateTopologicalView is used, then each node and each link is expected
% to be the PID of a process that respects the class_Graphable API (see
% class_Graphable.erl), and each associated content will be a cached value of
% that rendering information, i.e. a list of options:
%
% {Vertex, Label} = {NodePid, NodeOptions} or {NodePid, undefined}
%
% and
%
% {Edge, Label} = {LinkPid, LinkOptions} or {LinkPid, undefined}
%
% However links can also be defined once for all, in this case they are static.
%
% Such a link is not defined by a PID (an atom is usually used instead), and its
% associated content will never be updated.
%
% The up_to_date mesh attribute corresponds to the fact that the content
% associated to all nodes and links, cached in the mesh, corresponds to the
% exact content that would be notified by these processes themselves.
%
% Note that some operations lead to the mesh being tagged as non up-to-date,
% thus they might result in having the mesh send update requests to any
% processes associated to its nodes and links.
%
% The up-to-date status regards *content* only, not the fact that for example
% the rendering of the mesh is expected to be different because of a recent
% operation.



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).



% The class-specific attributes:
-define( class_attributes, [

	{ graph_filename, file_name(),
	  "name of the file in which the graph information will be written" },

	{ graph_directory, directory_path(),
	  "directory in which graph files will be output" },

	{ graph_directory_created, bool(),
	  "tells whether the graph directory has already been created" },

	{ digraph, digraph:graph(), "the actual digraph instance" },

	{ marked_links, [ any_link() ], "links that are marked" },

	{ marked_nodes, [ any_node() ], "nodes that are marked" } ] ).



% Exported as conveniences to simplify user code or case:
-export([ generate_text_panel/2, generate_topological_view_for/1 ]).



% Implementation notes:
%
% This module is mostly an user-friendly object-oriented encapsulation of the
% digraph module, with some added features.
%
% File writings should be made with write/2, rather than format/2.
%
% Implementation based on labeled directed graphs, see the digraph module.
%
% Mesh nodes are graph vertices, and mesh links are graph edges.
%
% Content associated to a mesh node or link corresponds to the dot options
% (including dot label) of a graph vertex or graph edge, according to the
% information returned the last time the mesh element was requested.
%
% Such a graph is mutable and modified by digraph calls, thus there is no need
% to re-set the graph once modified (no new graph returned, existing one
% updated).
%
% Undirected graphs could be managed with the same API by duplicating created
% links (both ways).
%
% Due to digraph being mostly a reference (process), many methods updating it
% are pseudo-const, i.e. they are const only (and at most) on technical terms,
% not semantically.


% We do not use parametric types currently.


-type pure_node() :: any().
% node() is already a standalone type.


%-type node_content(X) :: maybe( X ).
%-type node_content() :: node_content( any() ).
-type node_content() :: maybe( any() ).


%-type node_with_content(X) :: { pure_node(), node_content(X) }.
-type node_with_content() :: { pure_node(), node_content() }.


%-type any_node(X) :: pure_node() | node_with_content(X).
%-type any_node() :: any_node( any() ).
-type any_node() :: pure_node() | node_with_content().



-type pure_link() :: any().

%-type link_content(X) :: maybe( X ).
%-type link_content() :: link_content( any() ).
-type link_content() :: maybe( any() ).

%-type link_with_content(X) :: {pure_link(),link_content(X)}.
-type link_with_content() :: { pure_link(), link_content() }.

%-type any_link(X) :: pure_link() | link_with_content(X).
%-type any_link() :: any_link( any() ).
-type any_link() :: pure_link() | link_with_content().

-type link_connectivity() :: { pure_node(), pure_node(), link_content() }.


-type edge() :: { pure_node(), pure_node() }.



-type node_style() :: 'filled'.
-type link_style() :: 'solid'.


-type elem_color() :: gui_color:color_by_name().

-type node_color() :: elem_color().
-type link_color() :: elem_color().


-type mesh_pid() :: pid().
% A PID of a mesh instance.


-export_type([ pure_node/0, node_content/0, node_with_content/0,
			   pure_link/0, link_content/0, link_with_content/0,
			   mesh_pid/0 ]).


-type graphable_pid() :: pid().
% A PID of a Graphable instance.



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_EngineBaseObject header:
-define( trace_emitter_categorization , "Core.Mesh" ).



% For notify* trace macros:
-include_lib("traces/include/traces.hrl").



% Unmarked settings.


% The color used by default to represent non-marked nodes:
-define( default_node_color, orange ).

% The style used by default to represent non-marked nodes:
-define( default_node_style, filled ).


% The color used by default to represent non-marked links:
-define( default_link_color, black ).

% The style used by default to represent non-marked links:
-define( default_link_style, solid ).



% Marked settings.


% The color used by default to represent marked nodes:
-define( default_marked_node_color, "forestgreen" ).

% The style used by default to represent marked nodes:
-define( default_marked_node_style, "filled" ).


% The color used by default to represent marked links:
-define( default_marked_link_color, "forestgreen" ).

% The style used by default to represent marked links:
%-define( default_marked_link_style, "bold" ).
-define( default_marked_link_style, "solid" ).



-type mesh_layout() :: 'dot' | 'neato' | 'twopi' | 'circo' | 'fdp' | 'sfdp'.
% See layout commands in [http://graphviz.org/Documentation.php] (default is
% 'dot').


-type mesh_option() :: { 'can_be_cyclic', boolean() }
					 | { 'layout', mesh_layout() }.

-type mesh_options() :: [ mesh_option() ].


% Shorthands:

-type ustring() :: text_utils:ustring().

-type file_name() :: file_utils:file_name().
-type directory_path() :: file_utils:directory_path().



% @doc Constructs a mesh instance.
%
% Parameters are:
%
% - Name: the name of the mesh
%
% - OutputDirectory: the directory path to which rendered views will be stored
% (the latter directory element in this path will be created if necessary)
%
% - MeshOptions:
%
%   - a graph layout may be chosen (see mesh_layout())
%
%   - a mesh is allowed to be cyclic by default, unless {can_be_cyclic, false}
%   is specified
%
% Note: when a directory is specified to the constructor, it will be created
% regardless of renderings being requested or not.
%
-spec construct( wooper:state(), ustring() | { ustring(), directory_path() },
				 mesh_options() ) -> wooper:state().
construct( State, { Name, OutputDirectory }, MeshOptions ) ->

	% First the direct mother classes, then this class-specific actions:
	TraceState =
		class_EngineBaseObject:construct( State, ?trace_categorize(Name) ),

	%trace_utils:debug_fmt( "## Mesh dir: ~ts", [ OutputDirectory ] ),

	Filename = file_utils:convert_to_filename( Name ),

	init_common( MeshOptions, setAttributes( TraceState, [
		{ graph_filename, Filename },
		{ graph_directory, OutputDirectory },
		{ graph_directory_created, false },
		{ digraph, undefined },
		{ marked_links, [] },
		{ marked_nodes, [] } ] ) );


construct( State, Name, MeshOptions ) ->

	% First the direct mother classes, then this class-specific actions:
	TraceState =
		class_EngineBaseObject:construct( State, ?trace_categorize(Name) ),

	init_common( MeshOptions, setAttributes( TraceState, [
		{ graph_filename, file_utils:convert_to_filename( Name ) },
		{ graph_directory, undefined },
		{ graph_directory_created, true },
		{ digraph, undefined },
		{ marked_links, [] },
		{ marked_nodes, [] } ] ) ).



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?notice( "Deleting mesh." ),

	digraph:delete( ?getAttr(digraph) ),

	?debug( "Mesh deleted." ),

	% Then allow chaining:
	State.




% Methods section.


% Section common to nodes and links.


% @doc Updates the whole mesh content, that is the content of all nodes and
% links, by requesting each PID to send its current graph options.
%
-spec update( wooper:state() ) -> oneway_return().
update( State ) ->

	%?debug( "Updating mesh." ),
	%trace_utils:debug( "Updating mesh." ),

	Digraph = ?getAttr(digraph),

	Nodes = digraph:vertices( Digraph ),

	[ update_content_for_node( Node, State ) || Node <- Nodes ],

	Links = digraph:edges( Digraph ),

	[ update_content_for_link( Link, State ) || Link <- Links ],

	%trace_utils:debug( "Mesh updated." ),

	% State returned almost as was (but nodes and links might be altered):
	wooper:return_state( setAttribute( State, update_status, up_to_date ) ).



% @doc Blanks specified mesh: resets its content to an empty digraph, and
% unmarks all nodes and links.
%
% Keeps the original digraph options.
%
% Note: update_status not changed.
%
-spec blank( wooper:state() ) -> oneway_return().
blank( State ) ->

	digraph:delete( ?getAttr(digraph) ),

	NewDigraph = digraph:new( [ ?getAttr(cyclic_option), private ] ),

	wooper:return_state( setAttributes( State, [
		{ digraph, NewDigraph },
		{ marked_links, [] },
		{ marked_nodes, [] } ] ) ).



% @doc Validates current cached content for nodes and links, so that for example
% the next topological rendering does not trigger an update for any Graphable
% element from the mesh.
%
% Note: useful for example if the mesh was blanked and recreated from scratch
% from outside, and then rendered.
%
-spec validate( wooper:state() ) -> oneway_return().
validate( State ) ->
	wooper:return_state( setAttribute( State, update_status, up_to_date ) ).



% @doc Invalidates current cached content for nodes and links, so that for
% example the next topological rendering triggers an update from any Graphable
% element from the mesh.
%
% Note: the connectivity will remain, only the contents are invalidated.
%
-spec invalidate( wooper:state() ) -> oneway_return().
invalidate( State ) ->
	wooper:return_state( setAttribute( State, update_status, out_of_sync ) ).




% Nodes section.


% @doc Adds specified node to the mesh, with possibly an associated content.
%
% AddedNode can be a simple node N (in this case the associated content will be
% the 'undefined' atom), or a {N, AssociatedNodeContent} pair.
%
-spec addNode( wooper:state(), any_node() ) -> oneway_return().
addNode( State, { AddedNode, AssociatedNodeContent } ) ->
	wooper:return_state( add_node( AddedNode, AssociatedNodeContent, State ) );

addNode( State, AddedNode ) ->
	wooper:return_state(
		add_node( AddedNode, _AssociatedNodeContent=undefined, State ) ).



% @doc Adds specified node to the mesh, with possibly an associated content.
-spec addNode( wooper:state(), pure_node(), node_content() ) -> oneway_return().
addNode( State, AddedNode, AssociatedNodeContent ) ->
	wooper:return_state( add_node( AddedNode, AssociatedNodeContent, State ) ).



% @doc Adds specified list of nodes to the mesh.
%
% Each added node can be either a simple node N, or a {N, AssociatedNodeContent}
% pair.
%
-spec addNodes( wooper:state(), [ any_node() ] ) -> oneway_return().
addNodes( State, NodeList ) ->
	wooper:return_state( add_nodes( NodeList, State ) ).




% @doc Returns the content associated to target node, or the atom
% 'node_not_found'.
%
-spec getContentForNode( wooper:state(), pure_node() ) ->
								const_request_return( node_content() ).
getContentForNode( State, Node ) ->
	wooper:const_return_result(
	  get_content_for_node( ?getAttr(digraph), Node ) ).



% @doc Updates the content associated to target node, by asking directly it for
% an update.
%
-spec updateContentForNode( wooper:state(), pure_node() ) -> oneway_return().
updateContentForNode( State, Node ) ->
	wooper:return_state( update_content_for_node( Node, State ) ).



% @doc Returns a list of all the nodes of this mesh.
-spec getAllNodes( wooper:state() ) ->
							const_request_return( [ node_with_content() ] ).
getAllNodes( State ) ->
	wooper:const_return_result( digraph:vertices( ?getAttr(digraph) ) ).




% Links section.


% @doc Adds an (anonymous, therefore static) directed link between FromNode and
% ToNode, with no associated content (undefined).
%
% Note:
%
% - the nodes must exist in the mesh already, otherwise {invalid_mesh_node, N}
% will be returned, if node N does not exist
%
% - if the mesh has been declared as acyclic, then {invalid_mesh_link, Path}
% will be returned if a cycle would be created by this node addition
%
-spec addLink( wooper:state(), pure_node(), pure_node() ) -> oneway_return().
addLink( State, FromNode, ToNode ) ->
	AddState = addStaticLink( State, FromNode, ToNode, undefined ),
	wooper:return_state( AddState ).



% @doc Adds a directed link between FromNode and ToNode, or updates it, with no
% specific content initially set.
%
% If the specified link is a PID, then next time the mesh will be updated, the
% PID will be expected to correspond to a Graphable instance, whose graph
% information will be requested and stored in the link content, for later use.
%
% Note:
%
% - no content will be associated to this link initially
%
% - the nodes must exist in the mesh already, otherwise {invalid_mesh_node, N}
% will be returned, if node N does not exist
%
% - if the mesh has been declared as acyclic, then {invalid_mesh_link, Path}
% will be returned if a cycle would be created
%
-spec addLink( wooper:state(), pure_link(), pure_node(), pure_node() ) ->
					oneway_return().
addLink( State, AddedLink, FromNode, ToNode ) ->
	AddState = addLink( State, AddedLink, FromNode, ToNode,
						_Content=undefined ),
	wooper:return_state( AddState ).




% @doc Adds a directed link between FromNode and ToNode, or updates it, and
% associates this link to specified content.
%
% Note:
%
% - if AddedLink is a PID, next time the mesh will be updated, the content of
% this link will be updated from that PID
%
% - the nodes must exist in the mesh already, otherwise {invalid_mesh_node, N}
% will be returned, if node N does not exist
%
% - if the mesh is acyclic, then {invalid_mesh_link, Path} will be returned if
% a cycle would be created
%
-spec addLink( wooper:state(), pure_link(), pure_node(), pure_node(),
			   link_content() ) -> oneway_return().
addLink( State, AddedLink, FromNode, ToNode, AssociatedLinkContent ) ->
	wooper:return_state(
		add_link( FromNode, ToNode, AddedLink, AssociatedLinkContent, State ) ).



% @doc Adds a directed static link between FromNode and ToNode, or updates it,
% and associates AssociatedLinkContent to this link.
%
% The link itself is not specifically set, it is static, but content is
% associated to it nevertheless.
%
% If generateTopologicalView is to be used, AssociatedLinkContent is expected to
% be of the form {Name, OptionList}, or just Name.
%
% Note:
%
% - content will be associated to this link
%
% - both nodes must exist in the mesh already, otherwise {invalid_mesh_node, N}
% will be returned, if node N does not exist
%
% - if the mesh has been declared as acyclic, then {invalid_mesh_link, Path}
% will be returned if a cycle would be created
%
% - named 'addStaticLink', as addLink/4 already exists.
%
% - is only pseudo-const (digraph modified)
%
-spec addStaticLink( wooper:state(), pure_node(), pure_node(),
					 link_content() ) -> const_oneway_return().
addStaticLink( State, FromNode, ToNode, AssociatedLinkContent ) ->

	%trace_utils:debug_fmt(
	%   "addStaticLink: FromNode =~w, ToNode=~w, AssociatedLinkContent=~w.",
	%   [ FromNode, ToNode, AssociatedLinkContent ] ),

	case digraph:add_edge( ?getAttr(digraph), FromNode, ToNode,
						   AssociatedLinkContent ) of

		{ error, { bad_edge, Path } } ->
			throw( { invalid_mesh_link, Path } );

		{ error, { bad_vertex, N } } ->
			throw( { invalid_mesh_node, N } );

		_ ->
			% Not more out-of-date than before because of this operation:
			wooper:const_return()

	end.



% @doc Returns the content associated to target link, or the 'link_not_found'
% atom.
%
% Note: this is the value cached in the mesh for this link, no update is
% performed.
%
-spec getContentForLink( wooper:state(), pure_link() ) ->
			const_request_return( link_content() | 'link_not_found' ).
getContentForLink( State, Link ) ->
	wooper:const_return_result(
		get_content_for_link( ?getAttr(digraph), Link ) ).



% @doc Updates the content associated to target node, by asking directly it for
% an update.
%
% Expected the specified link to be a Graphable PID.
%
-spec updateContentForLink( wooper:state(), graphable_pid() ) ->
									oneway_return().
updateContentForLink( State, LinkPid ) when is_pid( LinkPid ) ->
	wooper:return_state( update_content_for_link( LinkPid, State ) ).



% @doc Returns a list of all the links of this mesh.
-spec getAllLinks( wooper:state() ) -> const_request_return( [ any_link() ] ).
getAllLinks( State ) ->
	wooper:const_return_result( digraph:edges( ?getAttr(digraph) ) ).



% @doc Searches in this mesh for a link from FromNode to ToNode.
%
% Returns either the 'no_link_found' atom if no link was found, otherwise
% {Link, LinkContent}.
%
-spec findLink( wooper:state(), pure_node(), pure_node() ) ->
				const_request_return( 'no_link_found' | link_with_content() ).
findLink( State, FromNode, ToNode ) ->
	wooper:const_return_result(
		find_link_between( FromNode, ToNode, ?getAttr(digraph) ) ).



% @doc Returns the connectivity and state information for specified link:
% {FromNode, ToNode, LinkContent}.
%
-spec getLinkInformation( wooper:state(), pure_link() ) ->
								const_request_return( link_connectivity() ).
getLinkInformation( State, Link ) ->
	wooper:const_return_result(
		get_link_graph_informations( ?getAttr(digraph), Link ) ).



% @doc Tries to find a path between the source node and the target one.
%
% Returns either an ordered list of nodes (the path) or false, if no path was
% found.
%
-spec findPath( wooper:state(), pure_node(), pure_node() ) ->
						const_request_return( [ pure_node() ] | 'false' ).
findPath( State, SourceNode, TargetNode ) ->
	wooper:const_return_result( digraph:get_path( ?getAttr(digraph),
												  SourceNode, TargetNode ) ).



% @doc Tries to find the shortest path between the source node and the target
% one.
%
% Returns either an ordered list of nodes (the path) or false, if no path was
% found.
%
-spec findShortestPath( wooper:state(), pure_node(), pure_node() ) ->
							const_request_return( [ pure_node() ] | 'false' ).
findShortestPath( State, SourceNode,TargetNode ) ->

	ShortPath =
		digraph:get_short_path( ?getAttr(digraph), SourceNode, TargetNode ),

	wooper:const_return_result( ShortPath ).



% @doc Returns the list of links corresponding to the specified node path.
-spec getLinksInPath( wooper:state(), [ pure_node() ] ) ->
							const_request_return( [ pure_link() ] ).
getLinksInPath( State, NodeList ) ->
	wooper:const_return_result( get_links_from( NodeList, ?getAttr(digraph) ) ).



% @doc Returns the list of all node names corresponding to nodes that can be
% reached from the specified one.
%
-spec findReachableFrom( wooper:state(), pure_node() ) ->
								const_request_return( [ pure_node() ] ).
findReachableFrom( State, NodeName ) ->
	wooper:const_return_result(
		digraph_utils:reachable( [ NodeName ], ?getAttr(digraph) ) ).



% @doc Eliminates all nodes and links that are not reachable from the specified
% node.
%
-spec pruneFrom( wooper:state(), pure_node() ) -> const_oneway_return().
pruneFrom( State, Node ) ->

	Digraph = ?getAttr(digraph),

	AllNodes = digraph:vertices( Digraph ),
	NodesToKeep = digraph_utils:reachable( [ Node ], Digraph ),
	NodesToRemove = lists:subtract( AllNodes, NodesToKeep ),

	%trace_utils:debug_fmt( "Keeping ~B nodes, removing ~B.",
	%    [ length( NodesToKeep ), length( NodesToRemove ) ] ),

	% Appropriate links will be removed as well:
	[ digraph:del_vertex( Digraph, N ) || N <- NodesToRemove ],

	% Not more out-of-date than before because of this operation:
	wooper:const_return().



% @doc Eliminates all nodes and links that cannot reach the specified node.
-spec pruneTo( wooper:state(), pure_node() ) -> const_oneway_return().
pruneTo( State, Node ) ->

	Digraph = ?getAttr(digraph),

	AllNodes = digraph:vertices( Digraph ),
	NodesToKeep = digraph_utils:reaching( [ Node ], Digraph ),
	NodesToRemove = lists:subtract( AllNodes, NodesToKeep ),

	%trace_utils:debug_fmt( "Keeping ~B nodes, removing ~B.",
	%    [ length( NodesToKeep ), length( NodesToRemove ) ] ),

	% Appropriate links will be removed as well:
	[ digraph:del_vertex( Digraph, N ) || N <- NodesToRemove ],

	% Not more out-of-date than before because of this operation:
	wooper:const_return().



% @doc Sets the list of marked nodes.
%
% These nodes, once the topological view will be generated, will be visually
% marked.
%
-spec setMarkedNodes( wooper:state(), [ pure_node() ] ) -> oneway_return().
setMarkedNodes( State, NodeList ) ->
	wooper:return_state( setAttribute( State, marked_nodes, NodeList ) ).



% @doc Sets the list of marked links.
%
% These links, once the topological view will be generated, will be visually
% marked.
%
-spec setMarkedLinks( wooper:state(), [ pure_link() ] ) -> oneway_return().
setMarkedLinks( State, LinkList ) ->
	wooper:return_state( setAttribute( State, marked_links, LinkList ) ).



% @doc Sets the list of marked links from the specified list of endpoints pairs.
%
% These links, once the topological view will be generated, will be visually
% marked.
%
-spec setMarkedLinksFromEndpoints( wooper:state(), [ edge() ] ) ->
											oneway_return().
setMarkedLinksFromEndpoints( State, EndpointList ) ->

	LinkList = determine_links_from_endpoints( EndpointList,
											   ?getAttr(digraph), [] ),

	wooper:return_state( setAttribute( State, marked_links, LinkList ) ).



% @doc Adds specified element expressed in raw dot notation to the rendering of
% this mesh.
%
% See also: generate_text_panel/2.
%
-spec addRenderingRawElement( wooper:state(), ustring() ) -> oneway_return().
addRenderingRawElement( State, RawRenderingElement ) ->
	wooper:return_state( appendToAttribute( State,
		rendering_raw_elements, RawRenderingElement ) ).



% @doc Sets the settings for node and link rendering.
-spec setRenderingSettings( wooper:state(),
		{ { node_style(), node_color() }, { node_style(), node_color() } },
		{ { link_style(), link_color() }, { link_style(), link_color() } } ) ->
									oneway_return().
setRenderingSettings( State,
		{ { NodeStyle, NodeColor }, { MarkedNodeStyle, MarkedNodeColor } },
		{ { LinkStyle, LinkColor }, { MarkedLinkStyle, MarkedLinkColor } } ) ->

	wooper:return_state( setAttributes( State,  [
		{ node_style, NodeStyle },
		{ node_color, NodeColor },
		{ marked_node_style, MarkedNodeStyle },
		{ marked_node_color, MarkedNodeColor },
		{ link_style, LinkStyle },
		{ link_color, LinkColor },
		{ marked_link_style, MarkedLinkStyle },
		{ marked_link_color, MarkedLinkColor } ] ) ).



% @doc Returns a string describing the state of this mesh.
-spec getMeshInformation( wooper:state() ) -> const_request_return( ustring() ).
getMeshInformation( State ) ->

	Digraph = ?getAttr(digraph),

	wooper:const_return_result( text_utils:format(
		"Mesh ~w status: ~B nodes and ~B links",
		[ self(), length( digraph:vertices( Digraph ) ),
		  length( digraph:edges( Digraph ) ) ] ) ).



% @doc Sets the directory in which this mesh will be rendered.
-spec setRenderingDirectory( wooper:state(), directory_path() ) ->
													oneway_return().
setRenderingDirectory( State, NewOutputDirectory ) ->

	SetState = setAttributes( State, [
					{ graph_directory, NewOutputDirectory },
					{ graph_directory_created, false } ] ),

	wooper:return_state( SetState ).



% @doc Generates a view of current topology of this mesh.
%
% DisplayWanted is a boolean telling whether the generated view will be
% displayed to the user (if true).
%
-spec generateTopologicalView( wooper:state(), boolean() ) ->
				request_return( 'topological_view_generated' ).
generateTopologicalView( State, DisplayWanted ) ->

	BaseName = text_utils:binary_to_string( ?getAttr(name) ),

	NewState = generate_topological_view( DisplayWanted,
		% Necessary conversion:
		file_utils:convert_to_filename( BaseName ), State ),

	wooper:return_state_result( NewState, topological_view_generated ).



% @doc Generates a view of current topology of this mesh.
%
% Parameters are:
%
% - DisplayWanted: boolean() telling whether the generated view will be
% displayed to the user (if true)
%
% - FilenameSuffix: a string to add to the base filename (ex: '-0012'), useful
% to iterate on a set of numbered images (ex: 'xx-1.png', 'xx-2.png', etc.)
%
-spec generateTopologicalView( wooper:state(), boolean(), ustring() ) ->
					request_return( 'topological_view_generated' ).
generateTopologicalView( State, DisplayWanted, FilenameSuffix ) ->

	NewState = generate_topological_view( DisplayWanted,
						?getAttr(graph_filename) ++ FilenameSuffix, State ),

	wooper:return_state_result( NewState, topological_view_generated ).



% @doc Copies the probe rendering to specified directory and displays it.
%
% Useful for example to copy a rendering done in a temporary directory into a
% result one, and to trigger then its display.
%
% A request, for synchronisation purpose.
%
-spec displayRenderingIn( wooper:state(), directory_path() ) ->
								const_request_return( 'rendering_displayed' ).
displayRenderingIn( State, TargetDirectoryPath ) ->

	Basename = text_utils:binary_to_string( ?getAttr(name) ),

	RenderingFilename = file_utils:convert_to_filename( Basename ) ++ ".png",

	file_utils:is_existing_file( RenderingFilename )
		orelse throw( { no_rendering_found, RenderingFilename } ),

	CopiedFilename =
		file_utils:copy_file_in( RenderingFilename, TargetDirectoryPath ),

	executable_utils:display_png_file( CopiedFilename ),

	wooper:const_return_result( rendering_displayed ).




% Section for helper functions (not methods).



% @doc Adds a node in mesh.
%
% Returns an updated state.
%
add_node( Node, Content, State ) ->

	% Side-effect:
	digraph:add_vertex( ?getAttr(digraph), Node, Content ),

	setAttribute( State, update_status, out_of_sync ).



% @doc Adds nodes in mesh.
%
% Returns an updated state.
%
add_nodes( _NodeList=[], State ) ->
	State;

add_nodes( _NodeList=[ { Node, Content } | T ], State ) ->
	add_nodes( T, add_node( Node, Content, State ) );

add_nodes(_NodeList= [ Node | T ], State ) ->
	add_nodes( T, add_node( Node, _Content=undefined, State ) ).



% @doc Adds a link in mesh.
%
% Returns an updated state.
%
add_link( FromNode, ToNode, Link, Content, State ) ->

	%trace_utils:debug_fmt( "add_link for ~w from ~w to ~w.",
	%    [ Link, FromNode, ToNode ] ),

	% Side-effect:
	case digraph:add_edge( ?getAttr(digraph), Link, FromNode, ToNode,
						   Content ) of

		{ error, { bad_edge, Path } } ->
			throw( { invalid_mesh_link, Path } );

		{ error, { bad_vertex, N } } ->
			throw( { invalid_mesh_node, N } );

		_ ->
			setAttribute( State, update_status, out_of_sync )

	end.



% @doc Generates a topological view for this mesh.
-spec generate_topological_view( boolean(), file_name(), wooper:state() ) ->
													wooper:state().
generate_topological_view( DisplayWanted, BaseFileName, State ) ->

	UpdatedState = case ?getAttr(update_status) of

		up_to_date ->
			State;

		_OtherStatus ->
			% Beware of silent unexpected asynchronous updates...
			%trace_utils:debug_fmt( "class_Mesh: updating." ),

			% Returns an updated state:
			executeOneway( State, update )

	end,

	{ DirState, BaseFilePath } = case ?getAttr(graph_directory_created) of

		false ->

			case ?getAttr(graph_directory) of

				undefined ->
					{ UpdatedState, BaseFileName };


				OutputDirectory ->

					case file_utils:is_existing_directory( OutputDirectory ) of

						true ->
							ok ;

						false ->
							file_utils:create_directory( OutputDirectory )

					end,

					CreatedState = setAttribute( UpdatedState,
											graph_directory_created, true ),

					FilePath = filename:join( OutputDirectory, BaseFileName ),

					{ CreatedState, FilePath }

			end;


		true ->

			FilePath = case ?getAttr(graph_directory) of

				undefined ->
					BaseFileName;

				OutputDirectory ->
					filename:join( OutputDirectory, BaseFileName )

			end,

			{ UpdatedState, FilePath }

	end,

	DigraphFilename = BaseFilePath ++ ".graph",
	PNGFilename = BaseFilePath ++ ".png",

	?debug_fmt( "Generating topology for '~ts': graph in '~ts', view in '~ts', "
		"while in '~ts'.~n",
		[ ?getAttr(name), DigraphFilename, PNGFilename,
		  file_utils:get_current_directory() ] ),

	% Not file_utils:get_default_encoding_option(), managing encoding by
	% ourselves:
	%
	DigraphFile = file_utils:open( DigraphFilename, [ write, raw ] ),

	write_graph_header( DigraphFile, DirState ),
	write_graph_nodes(  DigraphFile, DirState ),
	write_graph_links(  DigraphFile, DirState ),
	write_raw_elements( DigraphFile, DirState ),

	write_graph_footer( DigraphFile, DirState ),

	file_utils:close( DigraphFile ),

	% false as Dot might issue non-serious warnings:
	executable_utils:generate_png_from_graph_file( PNGFilename,
												   DigraphFilename, false ),

	case DisplayWanted of

		true ->
			executable_utils:display_png_file( PNGFilename );

		false ->
			ok

	end,

	% Removes the intermediate graph file:
	file_utils:remove_file( DigraphFilename ),

	DirState.



% @doc Writes the graph header for the topology of this mesh in the specified
% file.
%
write_graph_header( DigraphFile, State ) ->

	file_utils:write_ustring( DigraphFile,
							  "digraph Mesh_topological_view~n", [] ),

	file_utils:write_ustring( DigraphFile, "{~n~n", [] ),

	file_utils:write_ustring( DigraphFile, "    layout = ~ts~n~n",
							  [ ?getAttr(layout_option) ] ),

	% size = \"10,10\", fontsize = \"14.0\",
	file_utils:write_ustring( DigraphFile, "    graph [ label = \"~ts\", "
			"fontsize = \"20.0\"]~n~n", [ ?getAttr(graph_label) ] ),

	file_utils:write_ustring( DigraphFile, "    node [ height = 1, width = 1, "
			"fixedsize = true, fontsize = \"10.0\" ]~n~n", [] ).



% @doc Writes the description of the graph nodes of this mesh in the specified
% file.
%
write_graph_nodes( DigraphFile, State ) ->

	file_utils:write_ustring( DigraphFile, "~n/* Node definitions */~n~n", [] ),

	Nodes = digraph:vertices( ?getAttr(digraph) ),

	write_graph_nodes( DigraphFile, Nodes, State ).


% (helper)
write_graph_nodes( DigraphFile, _NodeList=[], _State ) ->
	file_utils:write_ustring( DigraphFile, "\n", [] );

write_graph_nodes( DigraphFile, _NodeList=[ Node | T ], State ) ->

	case get_content_for_node( ?getAttr(digraph), Node ) of

		undefined ->
			throw( { contentless_node, Node } );

		NodeOptions ->

			%file_utils:write_ustring( "Node ~w, options: ~p.~n",
			%                          [ Node, NodeOptions ] ),

			NodeName = class_Graphable:forge_node_name( Node ),

			Options = case lists:member( Node, ?getAttr(marked_nodes) ) of

				false ->
					format_options( NodeOptions );

				true ->
					% Overrides node informations with marked settings:
					format_marked_options_for_nodes( NodeOptions, State )

			end,

			file_utils:write_ustring( DigraphFile, "\"~ts\" [~ts]~n",
									  [ NodeName, Options ] )

	end,

	write_graph_nodes( DigraphFile, T, State ).



% @doc Formats specified options: [{a,a_value}, {b,b_value}, {c,c_value}] must
% become: a = "a_value", b = "b_value", c = "c_value".
%
format_options( undefined ) ->
	"";

format_options( NodeOptions ) ->

	%trace_utils:debug_fmt( "Formatting ~p.", [ NodeOptions ] ),

	Strings = [ text_utils:format( "~ts = \"~ts\"", [ Name, Value ] )
					|| { Name, Value } <- NodeOptions ],

	text_utils:join( ", ", Strings ).



% @doc Formats specified options: [{a,a_value}, {b,b_value}, {c,c_value}] must
% become: a = "a_value", b = "b_value", c = "c_value".
%
% Same as format_options/1, except that some options are overridden for nodes.
%
format_marked_options_for_nodes( undefined, _State ) ->
	"";

format_marked_options_for_nodes( Options, State ) ->

	FilteredOptions =
		filter_marked_options_for_nodes( Options, _Acc=[], State ),

	format_options( FilteredOptions ).


% (helper)
filter_marked_options_for_nodes( _Options=[], Acc,_State ) ->
	Acc;

filter_marked_options_for_nodes( _Options=[ { color, _Color } | T ] , Acc,
								 State ) ->
	filter_marked_options_for_nodes( T,
		[ { color, ?getAttr(marked_node_color) } | Acc ], State );

filter_marked_options_for_nodes( _Options=[ { style, _Style } | T ], Acc,
								 State ) ->
	filter_marked_options_for_nodes( T,
		[ { style, ?getAttr(marked_node_style) } | Acc ], State );

filter_marked_options_for_nodes( _Options=[ Elem | T ], Acc, State ) ->
	filter_marked_options_for_nodes( T, [ Elem | Acc ], State ).



% @doc Formats specified options: [{a,a_value}, {b,b_value}, {c,c_value}] must
% become: a = "a_value", b = "b_value", c = "c_value".
%
% Same as format_options/1, except that some options are overridden for links.
%
format_marked_options_for_links( undefined, _State ) ->
	"";

format_marked_options_for_links( Options, State ) ->
	FilteredOptions =
		filter_marked_options_for_links( Options, _Acc=[], State ),
	format_options( FilteredOptions ).


% (helper)
filter_marked_options_for_links( _Options=[], Acc, _State ) ->
	Acc;

filter_marked_options_for_links( _Options=[ { color, _Color } | T ] , Acc,
								 State ) ->
	filter_marked_options_for_links( T,
		[ { color, ?getAttr(marked_link_color) } | Acc ], State );

filter_marked_options_for_links( _Options=[ { style, _Style } | T ], Acc,
								 State ) ->
	filter_marked_options_for_links( T,
		[ { style, ?getAttr(marked_link_style) } | Acc ], State );

filter_marked_options_for_links( _Options=[ Elem | T ], Acc, State ) ->
	filter_marked_options_for_links( T, [ Elem | Acc ], State ).



% @doc Writes the description of graph links of this mesh in specified file.
write_graph_links( DigraphFile, State ) ->

	file_utils:write_ustring( DigraphFile, "~n/* Link definitions */~n~n", [] ),

	Links = digraph:edges( ?getAttr(digraph) ),

	% Sorts links as well, for increased safety in rendering stability:
	write_graph_links( DigraphFile, Links, State ).


% (helper)
write_graph_links( DigraphFile, _Links=[], _State ) ->
	file_utils:write_ustring( DigraphFile, "\n", [] );

write_graph_links( DigraphFile, _Links=[ Link | T ], State ) ->

	{ SourceNode, TargetNode, LinkOptions } =
		get_link_graph_informations( ?getAttr(digraph), Link ),

	SourceNodeName = class_Graphable:forge_node_name( SourceNode ),
	TargetNodeName = class_Graphable:forge_node_name( TargetNode ),

	Options = case lists:member( Link, ?getAttr(marked_links) ) of

		false ->
			format_options( LinkOptions );

		true ->
			% Overrides link informations with marked settings:
			format_marked_options_for_links( LinkOptions, State )

	end,

	file_utils:write_ustring( DigraphFile, "\"~ts\" -> \"~ts\" [~ts]~n",
							  [ SourceNodeName, TargetNodeName, Options ] ),

	write_graph_links( DigraphFile, T, State ).



% @doc Writes the graph legend of this mesh in specified file.
write_raw_elements( DigraphFile, State ) ->
	[ file_utils:write_ustring( DigraphFile, Elem, [] )
			|| Elem <- ?getAttr(rendering_raw_elements) ].



% @doc Writes the graph footer for the topology of this mesh in specified file.
write_graph_footer( DigraphFile, _State ) ->
	file_utils:write_ustring( DigraphFile, "~n}~n", [] ).



% @doc Returns the graph information associated to specified link, that is
% {SourceNode, TargetNode, LinkOptions}.
%
get_link_graph_informations( Digraph, Link ) ->

	{ Link, SourceNode, TargetNode, LinkOptions } =
		digraph:edge( Digraph, Link ),

	{ SourceNode, TargetNode, LinkOptions }.



% @doc Returns the digraph link (if any), and its associated content, existing
% between the specified nodes.
%
find_link_between( SourceNode, TargetNode, Digraph ) ->

	% Get all edges emanating from SourceNode:
	Links = digraph:out_edges( Digraph, SourceNode ),

	find_link_targeting( TargetNode, Links, Digraph ).



% @doc Returns the first link found among specified links targeting
% 'TargetNode', or, if none is found, 'no_link_found'.
%
% Returns, if found, the digraph link and its associated content.
%
find_link_targeting( _TargetNode, _Links=[], _Digraph ) ->
	no_link_found;

find_link_targeting( TargetNode, _Links=[ L | T ], Digraph ) ->

	case digraph:edge( Digraph, L ) of

		{ L, _SourceNode, TargetNode, LinkContent } ->
			% Found, stop the look-up:
			{ L, LinkContent };

		_ ->
			find_link_targeting( TargetNode, T, Digraph )

	end.



% @doc Returns the list of links between the specified nodes.
get_links_from( NodeList, Digraph ) ->
	get_links_from( NodeList, Digraph, _Acc=[] ).


% (helper)
get_links_from( [ N1, N2 | T ], Digraph, Acc ) ->

	OutLinks = digraph:out_edges( Digraph, N1 ),

	{ Link, _LinkContent } = find_link_targeting( N2, OutLinks, Digraph ),

	get_links_from( [ N2 | T ], Digraph, [ Link | Acc ] );


get_links_from( _LastNode, _Digraph, Acc ) ->
	lists:reverse( Acc ).



% (helper)
determine_links_from_endpoints( _EndpointList=[], _Digraph, Acc ) ->
	Acc;


determine_links_from_endpoints( _EndpointList=[ { Source, Target } | H ],
								Digraph, Acc ) ->
	{ Link, _LinkContent } = find_link_between( Source, Target, Digraph ),
	determine_links_from_endpoints( H, Digraph, [ Link | Acc ] ).



% @doc Returns the content associated to the specified node.
get_content_for_node( Digraph, Node ) ->

	case digraph:vertex( Digraph, Node ) of

		{ Node, Content } ->
			Content;

		false ->
			node_not_found

	end.



% @doc Returns the content associated to the specified link.
get_content_for_link( Digraph, Link ) ->

	 case digraph:edge( Digraph, Link ) of

		{ Link, _SourceNode, _TargetNode, Content } ->
			Content;

		false ->
			link_not_found

	end.



% @doc Returns the content associated to specified node.
update_content_for_node( Node, State ) ->

	Node ! { getGraphOptions, [], self() },

	NodeContent = receive

		{ wooper_result, Content } ->
			Content

	end,

	% It is actually an update:
	add_node( Node, NodeContent, State ).



% @doc Updates the content associated to specified link.
update_content_for_link( Link, State ) when is_pid( Link ) ->

	Link ! { getGraphOptions, [], self() },

	LinkContent = receive

		{ wooper_result, Content } ->
			Content

	end,

	% Needing to retrieve its endpoints to update it:
	{ Link, SourceNode, TargetNode, _LinkContent } =
		digraph:edge( ?getAttr(digraph), Link ),

	% It is actually an update:
	add_link( SourceNode, TargetNode, Link, LinkContent, State );

update_content_for_link( _Link, State ) ->
	State.



% @doc Factors parts common to all constructors.
%
% (helper)
%
init_common( Options, State ) ->

	Name = ?getAttr(name),

	Index = 1,

	{ CycleInfo, CycleString } =
		case lists:keyfind( can_be_cyclic, Index, Options ) of

		{ can_be_cyclic, false } ->
			{ acyclic, "acyclic" };

		% Includes {can_be_cyclic, true} and false (i.e. not found):
		_ ->
			{ cyclic, "possibly cyclic" }

	end,

	LayoutInfo = case lists:keyfind( layout, Index, Options ) of

		{ layout, Layout } ->
			case is_known_layout( Layout ) of

				true ->
					Layout;

				false ->
					throw( { unknown_layout, Layout } )

			end;

		_ ->
			dot

	end,

	?notice_fmt( "Creating a mesh whose name is ~ts, which will be ~ts.",
				 [ Name, CycleString ] ),

	GraphLabel =
		text_utils:format( "Topological view of mesh '~ts'", [ Name ] ),

	% As long as the mesh will remain empty, it will be up-to-date:
	setAttributes( State, [
		{ digraph, digraph:new( [ CycleInfo, private ] ) },
		{ cyclic_option, CycleInfo },
		{ layout_option, LayoutInfo },
		{ graph_label, GraphLabel },
		{ rendering_raw_elements, [] },
		{ node_style, ?default_node_style },
		{ node_color, ?default_node_color },
		{ marked_node_style, ?default_marked_node_style },
		{ marked_node_color, ?default_marked_node_color },
		{ link_style, ?default_link_style },
		{ link_color, ?default_link_color },
		{ marked_link_style, ?default_marked_link_style },
		{ marked_link_color, ?default_marked_link_color },
		{ marked_nodes, [] },
		{ update_status, up_to_date } ] ).



% @doc Generates a string, suitable to be used with addRenderingRawElement/2, to
% represent a text panel whose title and internal content are the specified
% ones.
%
% The content must respect the syntax specified in
% [http://www.graphviz.org/doc/info/shapes.html#record].
%
% See addRenderingRawElement/2.
%
-spec generate_text_panel( ustring(), ustring() ) -> ustring().
generate_text_panel( Title, Content ) ->

	"subgraph cluster_" ++ text_utils:generate_text_name_from( Title )
		++ "\n"
		++ "{\n\n"
		++ "    graph [ rankdir = \"LR\" ];\n"
		++ "    fontsize = 25\n"
		++ "    pencolor = white\n"
		++ "    label = \"" ++ Title ++ "\"\n\n"
		++ "    \"node0\" [\n"
		++ "        fixedsize= true\n"
		++ "        fontsize = 20\n"
		++ "        width = 8\n"
		++ "        height = 6\n"
		++ "        shape = \"Mrecord\"\n"
		++ "        label = \"" ++ Content ++ "\"\n"
		++ "    ];\n\n"
		++ "}\n\n".



% @doc Allows to define whether the topological view should be displayed to the
% user, after generation.
%
-spec generate_topological_view_for( pid() ) -> void().
generate_topological_view_for( MeshPid ) ->

	% Avoids adding a bound variable:
	case executable_utils:is_batch() of

		true ->
			% Boolean means 'display wanted':
			MeshPid ! { generateTopologicalView, false, self() };

		false ->
			MeshPid ! { generateTopologicalView, true, self() }

	end,

	receive

		{ wooper_result, topological_view_generated } ->
			?notify_info( "Topological view correctly generated." )

	end.


% @doc Tells whether a type of layout is known.
is_known_layout( Layout ) ->
	% See mesh_layout():
	lists:member( Layout, [ dot, neato, twopi, circo, fdp, sfdp ] ).
