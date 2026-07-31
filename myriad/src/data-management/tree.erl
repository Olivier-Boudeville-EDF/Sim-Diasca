% Copyright (C) 2015-2026 Olivier Boudeville
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
% Creation date: Monday, May 25, 2015.

-module(tree).

-moduledoc """
Gathering of facilities to manage **trees**, binary or not, balanced or not.

The support comprises the one of classical, functional, recursive trees, but
also ones (actually forests) whose nodes are indexed by an associative table
(typically for faster look-up).
""".


% So that we can define our own size/1:
-compile( { no_auto_import, [ size/1 ] } ).




% Section transverse to all kinds of trees.


-doc "The content of a node of a tree ('undefined' meaning empty content).".
-type node_content() :: option( any() ).



-doc "Describes a function that can be folded onto the content of trees.".
-type content_fold_fun() ::
        fun( ( node_content(), accumulator() ) -> accumulator() ).



-doc """
Height of a tree.

Zero for a single node, one for a parent node and its child, etc.
""".
-type height() :: count().


-export_type([ node_content/0, content_fold_fun/0, height/0 ]).



% Section for classical, functional, recursive trees.


-doc """
Any tree is made of the content of its root and of any number of (ordered)
subtrees (children trees).
""".
-opaque tree() :: { node_content(), [ tree() ] }.



-doc "A typed tree is polymorphic according to its node content.".
-opaque tree( T ) :: { T, [ tree( T ) ] }.


-export_type([ tree/0, tree/1 ]).


-export([ new/0, new/1, new/2, set_content/2, append_child/2, append_children/2,
          map/2, fold_breadth_first/3, fold_depth_first/3, height/1, size/1,
          to_string/1 ]).




% Section for trees/forests whose nodes are indexed by an associative table.
%
% No parametric type, as not useful enough.


-doc """
Designates the identifier of a node of a tree (typically in a forest table).
""".
-type node_id() :: term().



-doc """
A list of node identifiers, usually semantically equivalent to a set thereof.
""".
-type child_ids() :: [ node_id() ].



-doc """
Another type of tree, based on an indexing table describing, in the general
case, a forest.

The index of the root node (if any) is implicit (not specified) here.
""".
-type forest_table() :: table( node_id(), node_content() ).



-doc """
A table keeping track of the child nodes of each node, based on node
identifiers.
""".
-type children_table() :: table( node_id(), child_ids() ).



-doc """
A function able to return, from a given node identifier and its corresponding
forest table, a string describing it.
""".
-type node_to_string_fun() :: fun( ( node_id(), forest_table() ) -> ustring() ).



-doc """
A function able to return, from a given node identifier and its corresponding
forest table, the identifier of its children.
""".
-type node_to_children_fun() ::
        fun( ( node_id(), forest_table() ) -> [ node_id() ] ).


-export_type([ node_id/0, child_ids/0, forest_table/0, children_table/0,
               node_to_string_fun/0, node_to_children_fun/0 ]).


-export([ to_string/3, forest_to_string/4 ]).




% Shorthands:

-type count() :: basic_utils:count().
-type accumulator() :: basic_utils:accumulator().

-type ustring() :: text_utils:ustring().
-type verbosity_level() :: text_utils:verbosity_level().






% Section for classical, functional, recursive trees.


-doc "Creates a single-node, empty, tree.".
-spec new() -> tree().
new() ->
    { _NodeContent=undefined, _Subtrees=[] }.



-doc "Creates a tree with a single node containing the specified content.".
-spec new( node_content() ) -> tree().
new( NodeContent ) ->
    { NodeContent, _Subtrees=[] }.



-doc "Creates a tree with the specified content and child trees.".
-spec new( node_content(), [ tree() ] ) -> tree().
new( NodeContent, Subtrees ) ->
    { NodeContent, Subtrees }.



-doc "Sets the content of the specified node.".
-spec set_content( node_content(), tree() ) -> tree().
set_content( Content, _Tree= { _PastContent, ChildTrees } ) ->
    { Content, ChildTrees }.



-doc "Appends the specified child tree as first child of the specified tree.".
-spec append_child( tree(), tree() ) -> tree().
append_child( NewChildTree, _TargetTree={ Content, ChildTrees } ) ->
    { Content, [ NewChildTree | ChildTrees ] }.



-doc """
Appends the specified child trees as first children of the specified tree.
""".
-spec append_children( [ tree() ], tree() ) -> tree().
append_children( NewChildTrees, _TargetTree={ Content, ChildTrees } ) ->
    { Content, NewChildTrees ++ ChildTrees }.



-doc """
Maps the specified function to the content of all nodes of the specified tree.

Performs breadth-first mapping.
""".
-spec map( fun( ( node_content() ) -> node_content() ), tree() ) -> tree().
map( Fun, _Tree={ Content, Subtrees } ) ->
    NewContent = Fun( Content ),
    { NewContent, [ map( Fun, T ) || T <- Subtrees ] }.



-doc """
Folds the specified function breadth-first onto the content of all nodes of the
specified tree, and returns the corresponding result.

At a given height, siblings will be traversed from most recent to oldest
attached.
""".
-spec fold_breadth_first( content_fold_fun(), accumulator(), tree() ) ->
                                accumulator().
fold_breadth_first( ContentFun, InitialAcc, _Tree={ Content, Subtrees } ) ->

    NodeAcc = ContentFun( Content, InitialAcc ),

    lists:foldl( fun( ChildTree, Acc ) ->
                    fold_breadth_first( ContentFun, Acc, ChildTree )
                 end,
                 NodeAcc, Subtrees ).



-doc """
Folds the specified function depth-first onto the content of all nodes of
specified tree, and returns the corresponding result.

In case of unbalanced trees, there is no guarantee that the deepest element is
examined first, as the first branch examined may not be the deepest.
""".
-spec fold_depth_first( content_fold_fun(), accumulator(), tree() ) ->
                                                        accumulator().
fold_depth_first( ContentFun, InitialAcc, _Tree={ Content, Subtrees } ) ->

    ChildAcc = lists:foldl( fun( ChildTree, Acc ) ->
                                fold_depth_first( ContentFun, Acc, ChildTree )
                            end,
                            InitialAcc, Subtrees ),
    ContentFun( Content, ChildAcc ).



-doc """
Returns the height (maximum depth) of the specified tree, that is the number of
edges on the longest downward path between the root and any leaf.
""".
-spec height( tree() ) -> height().
height( _Tree={ _Content, _Subtrees=[] } ) ->
    0;

height( _Tree={ _Content, Subtrees } ) ->
    1 + lists:max( [ height( S ) || S <- Subtrees ] ).

%height( Tree ) ->
%   height( Tree, _CurrentHeight=0 ).
%
%height( _Tree={ _Content, _Subtrees=[] }, CurrentHeight ) ->
%   CurrentHeight;
%
%height( _Tree={ _Content, Subtrees }, CurrentHeight ) ->
%   CurrentHeight + 1 + lists:max( [ height( S ) || S <- Subtrees ] ).



-doc "Returns the total number of nodes that the specified tree contains.".
-spec size( tree() ) -> count().
%size( _Tree={ _Content, _Subtrees=[] } ) ->
%   1;
%
%size( _Tree={ _Content, Subtrees } ) ->
%   1 + lists:sum( [ size( S ) || S <- Subtrees ] ).

size( Tree ) ->
    size( Tree, _Acc=1 ).

size( _Tree={ _Content, _Subtrees=[] }, Acc ) ->
    Acc;

size( _Tree={ _Content, Subtrees }, Acc ) ->
    Acc + lists:sum( [ size( S ) || S <- Subtrees ] ).



-doc "Returns a textual description of the specified tree.".
-spec to_string( tree() ) -> ustring().
to_string( Tree ) ->
    % Is an io_list():
    lists:flatten( to_string( Tree, _Prefix="" ) ).


% Helper (ad hoc fold_breadth_first):
-spec to_string( tree(), ustring() ) -> ustring().
to_string( _Tree={ Content, _SubTrees=[] }, Prefix ) ->
    Prefix ++ text_utils:format( "+ leaf node '~p'~n", [ Content ] );

to_string( _Tree={ Content, SubTrees }, Prefix ) ->

    ContentString = Prefix ++ text_utils:format(
        "+ node '~p' with ~B child node(s):~n",
        [ Content, length( SubTrees ) ] ),

    ChildPrefix = [ "  " | Prefix ],

    AllStrings = lists:foldl(
        fun( ChildTree, AccStrings ) ->
            [ to_string( ChildTree, ChildPrefix ) | AccStrings ]
        end,
        _Acc0=[ ContentString ],
        _List=SubTrees ),

    lists:reverse( AllStrings ).




% Section for trees/forests whose nodes are indexed by an associative table.


-doc """
Returns a textual description of the specified forest, based on its main table,
the one of its children, and the identifier of the root node to consider.
""".
-spec to_string( forest_table(), node_id(), verbosity_level() ) -> ustring().
to_string( ForestTable, RootNodeId, _VerbLevel=low ) ->
    text_utils:format( "forest table of ~B nodes, whose root node is #~B",
                       [ table:size( ForestTable ), RootNodeId ] );

to_string( ForestTable, RootNodeId, _VerbLevel=high ) ->

    % Basic defaults:

    NodeToStringDefFun = fun( NodeId, _FTable ) ->
        %NodeContent = table:get_value( NodeId, FTable ),
        %text_utils:format( "node #~B: ~p", [ NodeId, NodeContent ] )
        text_utils:format( "node #~B", [ NodeId ] )
                         end,

    % Here we expect the node content to be a list of the children of the
    % corresponding node:
    %
    NodeToChildrenDefFun = fun( NodeId, FTable ) ->
                            table:get_value( NodeId, FTable )
                           end,

    forest_to_string( ForestTable, RootNodeId, NodeToStringDefFun,
                      NodeToChildrenDefFun ).



-doc """
Returns a textual description of the specified forest table, using the specified
starting node, and stringification and children-listing functions.
""".
-spec forest_to_string( forest_table(), node_id(), node_to_string_fun(),
                        node_to_children_fun() ) -> ustring().
forest_to_string( ForestTable, FromNodeId, NodeToStringFun,
                  NodeToChildrenFun ) ->
    forest_to_string( ForestTable, _ParentNodeId=FromNodeId, NodeToStringFun,
                      NodeToChildrenFun, _Level=0 ).


-define( spacer, "  " ).


% (helper)
forest_to_string( ForestTable, ParentNodeId, NodeToStringFun, NodeToChildrenFun,
                  Level ) ->

    ParentStr = case Level of
            0 ->
                % Was: "~n~ts"
                text_utils:format( "~ts",
                    [ NodeToStringFun( ParentNodeId, ForestTable ) ] );

            _ ->
                text_utils:format( "~ts- ~ts",
                    [ text_utils:duplicate( Level, ?spacer ),
                      NodeToStringFun( ParentNodeId, ForestTable ) ] )

    end,

    Children = NodeToChildrenFun( ParentNodeId, ForestTable ),

    %trace_utils:debug_fmt( "Children of #~B: ~w.",
    %                       [ ParentNodeId, Children ] ),

    ChildrenStrs = [ forest_to_string( ForestTable, C, NodeToStringFun,
                        NodeToChildrenFun, Level+1 ) || C <- Children ],

    text_utils:join( _Sep=$\n, [ ParentStr | ChildrenStrs ] ).
