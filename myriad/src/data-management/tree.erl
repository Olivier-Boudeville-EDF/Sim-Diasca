% Copyright (C) 2015-2021 Olivier Boudeville
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
% Creation date: Monday, May 25, 2015
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Gathering of facilities to manage trees that may be unbalanced.
%
-module(tree).


% So that we can defined our own size/1:
%
-compile( { no_auto_import, [ size/1 ] } ).


% The content of a node of a tree ('undefined' means empty content):
%
-type node_content() :: maybe( any() ).


% Any tree is made of the content of its root and of any number of (ordered)
% subtrees (children trees):
%
-opaque tree() :: { node_content(), [ tree() ] }.


% A typed tree is polymorphic according to its node content:
%
-opaque tree( T ) :: { T, [ tree(T) ] }.



% Describes a function that can be folded onto the content of trees:
%
-type content_fold_fun() :: fun( ( node_content(), basic_utils:accumulator() )
								 -> basic_utils:accumulator() ).


% Height of a tree:
%
-type height() :: basic_utils:count().


-export_type([ tree/0, tree/1, node_content/0, content_fold_fun/0, height/0 ]).


-export([ new/0, new/1, new/2, set_content/2, append_child/2, append_children/2,
		  map/2, fold_breadth_first/3, fold_depth_first/3,
		  height/1, size/1, to_string/1 ]).



% Creates a new, single-node, empty, tree.
%
-spec new() -> tree().
new() ->
	{ _NodeContent=undefined, _Subtrees=[] }.



% Creates a new tree with a single node containing specified content.
%
-spec new( node_content() ) -> tree().
new( NodeContent ) ->
	{ NodeContent, _Subtrees=[] }.



% Creates a new tree with specified content and child trees.
%
-spec new( node_content(), [ tree() ] ) -> tree().
new( NodeContent, Subtrees ) ->
	{ NodeContent, Subtrees }.



% Sets the content of specified node.
%
-spec set_content( node_content(), tree() ) -> tree().
set_content( Content, _Tree= { _PastContent, ChildrenTrees } ) ->
	{ Content, ChildrenTrees }.



% Appends specified child tree as first child of specified tree.
%
-spec append_child( tree(), tree() ) -> tree().
append_child( NewChildTree, _TargetTree={ Content, ChildrenTrees } ) ->
	{ Content, [ NewChildTree | ChildrenTrees ] }.



% Appends specified child trees as first children of specified tree.
%
-spec append_children( [ tree() ], tree() ) -> tree().
append_children( NewChildTrees, _TargetTree={ Content, ChildrenTrees } ) ->
	{ Content, NewChildTrees ++ ChildrenTrees }.



% Maps specified function to the content of all nodes of specified tree.
%
% Performs breadth-first mapping.
%
-spec map( fun( ( node_content() ) -> node_content() ), tree() ) -> tree().
map( Fun, _Tree={ Content, Subtrees } ) ->
	NewContent = Fun( Content ),
	{ NewContent, [ map( Fun, T ) || T <- Subtrees ] }.



% Folds specified function breadth-first onto the content of all nodes of
% specified tree, and returns the corresponding result.
%
% At a given height, siblings will be traversed from most recent to oldest
% attached.
%
-spec fold_breadth_first( content_fold_fun(), basic_utils:accumulator(),
						  tree() ) -> basic_utils:accumulator().
fold_breadth_first( ContentFun, InitialAcc, _Tree={ Content, Subtrees } ) ->

	NodeAcc = ContentFun( Content, InitialAcc ),

	lists:foldl( fun( ChildTree, Acc ) ->
					 fold_breadth_first( ContentFun, Acc, ChildTree )
				 end,
				 NodeAcc, Subtrees ).



% Folds specified function depth-first onto the content of all nodes of
% specified tree, and returns the corresponding result.
%
% In case of unbalanced trees, there is no guarantee that the deepest element is
% examined first, as the first branch examined may not be the deepest.
%
-spec fold_depth_first( content_fold_fun(), basic_utils:accumulator(),
						tree() ) -> basic_utils:accumulator().
fold_depth_first( ContentFun, InitialAcc, _Tree={ Content, Subtrees } ) ->

	ChildAcc = lists:foldl( fun( ChildTree, Acc ) ->
								fold_depth_first( ContentFun, Acc, ChildTree )
							end,
							InitialAcc, Subtrees ),
	ContentFun( Content, ChildAcc ).



% Returns the height (maximum depth) of specified tree, i.e. the number of edges
% on the longest downward path between the root and a leaf.
%
-spec height( tree() ) -> height().
height( _Tree={ _Content, _Subtrees=[] } ) ->
	0;

height( _Tree={ _Content, Subtrees } ) ->
	1 + lists:max( [ height( S ) || S <- Subtrees ] ).

%height( Tree ) ->
%	height( Tree, _CurrentHeight=0 ).
%
%height( _Tree={ _Content, _Subtrees=[] }, CurrentHeight ) ->
%	CurrentHeight;
%
%height( _Tree={ _Content, Subtrees }, CurrentHeight ) ->
%	CurrentHeight + 1 + lists:max( [ height( S ) || S <- Subtrees ] ).



% Returns the total number of nodes that specified tree contains.
%
-spec size( tree() ) -> basic_utils:count().
%size( _Tree={ _Content, _Subtrees=[] } ) ->
%	1;
%
%size( _Tree={ _Content, Subtrees } ) ->
%	1 + lists:sum( [ size( S ) || S <- Subtrees ] ).

size( Tree ) ->
	size( Tree, _Acc=1 ).

size( _Tree={ _Content, _Subtrees=[] }, Acc ) ->
	Acc;

size( _Tree={ _Content, Subtrees }, Acc ) ->
	Acc + lists:sum( [ size( S ) || S <- Subtrees ] ).



% Returns a textual description of specified tree.
%
-spec to_string( tree() ) -> text_utils:ustring().
to_string( Tree ) ->
	% Is an io_list():
	lists:flatten( to_string( Tree, _Prefix="" ) ).


% Helper (ad hoc fold_breadth_first):
-spec to_string( tree(), string() ) -> text_utils:ustring().
to_string( _Tree={ Content, _SubTrees=[] }, Prefix ) ->
	Prefix ++ text_utils:format( "+ leaf node '~p'~n", [ Content ] );

to_string( _Tree={ Content, SubTrees }, Prefix ) ->

	ContentString = Prefix ++ text_utils:format(
								"+ node '~p' with ~B child node(s):~n",
								[ Content, length( SubTrees ) ] ),

	ChildPrefix = [ "  " | Prefix ],

	AllStrings = lists:foldl( fun( ChildTree, AccStrings ) ->
										[ to_string( ChildTree, ChildPrefix ) |
										  AccStrings ]
								end,
								_Acc0=[ ContentString ],
								_List=SubTrees ),

	lists:reverse( AllStrings ).
