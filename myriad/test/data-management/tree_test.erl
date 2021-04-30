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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Monday, May 25, 2015


% Test of the tree implementation.
%
% See the tree.erl tested module.
%
-module(tree_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% We will create here following tree:
	%
	% iota ->
	%     gamma ->
	%        beta ->
	%            alpha
	%        delta

	FirstTree = tree:new(),

	test_facilities:display( "First tree: ~ts",
							 [ tree:to_string( FirstTree ) ] ),

	0 = tree:height( FirstTree ),
	1 = tree:size( FirstTree ),

	AlphaTree = tree:set_content( "Content of my alpha tree", FirstTree ),

	test_facilities:display( "Alpha tree: ~ts",
							 [ tree:to_string( AlphaTree ) ] ),

	BetaTree = tree:new( "Content of my beta tree" ),

	test_facilities:display( "Beta tree: ~ts", [ tree:to_string( BetaTree ) ] ),


	SecondTree = tree:append_child( AlphaTree, BetaTree ),

	1 = tree:height( SecondTree ),
	2 = tree:size( SecondTree ),

	GammaTree = tree:new( "Content of my gamma tree" ),

	FifthTree = tree:append_child( SecondTree, GammaTree ),

	2 = tree:height( FifthTree ),
	3 = tree:size( FifthTree  ),

	DeltaTree = tree:new( "Content of my delta tree" ),

	IotaTree = tree:new( "Content of my iota tree" ),

	FinalTree = tree:append_child( tree:append_child( DeltaTree, FifthTree ),
								   IotaTree ),

	test_facilities:display( "Final tree: ~n~ts",
							 [ tree:to_string( FinalTree ) ] ),

	3 = tree:height( FinalTree ),
	5 = tree:size( FinalTree  ),

	% Map operates on content directly, not on nodes:
	MapFun = fun( Content ) ->
				"Mapped " ++ Content
			 end,

	%MapFun = fun( _Tree={ Content, Subtrees } ) ->
	%			 NewContent = "Mapped " ++ Content,
	%			 % Better than { NewContent, Subtrees }:
	%			 tree:new( NewContent, Subtrees )
	%		  end,

	MappedTree = tree:map( MapFun, FinalTree ),

	test_facilities:display( "Mapped tree: ~n~ts",
							 [ tree:to_string( MappedTree ) ] ),

	FoldFun = fun( Node, Acc ) ->
				io:format( " - examining node '~ts'~n", [ Node ] ),
				Acc + 1
			  end,

	test_facilities:display( "Breadth-first walk:" ),

	InitialCount = 0,

	BreadthRes = tree:fold_breadth_first( FoldFun, InitialCount, FinalTree ),

	test_facilities:display( "Result: ~p~n", [ BreadthRes ] ),


	test_facilities:display( "Depth-first walk:" ),

	DepthRes = tree:fold_depth_first( FoldFun, InitialCount, FinalTree ),

	test_facilities:display( "Result: ~p~n", [ DepthRes ] ),

	test_facilities:stop().
