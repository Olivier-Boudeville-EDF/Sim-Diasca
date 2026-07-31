% Copyright (C) 2025-2026 Olivier Boudeville
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
% Creation date: Wednesday, September 10, 2025.

-module(spell_tree_test).

-moduledoc """
Unit tests for the `spell_tree` features.

See the `spell_tree` tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    test_facilities:display( "Testing spell trees." ),

    EmptyST = spell_tree:create(),

    test_facilities:display( "Empty spell tree: ~ts.",
                             [ spell_tree:to_string( EmptyST ) ] ),

    AllStrs = [ "test", "testing", "place", "platypus", "porridge", "plate",
                "placebo", "eric", "erlang", "eric", "stop", "stop_other" ],

    OneWST = spell_tree:register_string( hd( AllStrs ), EmptyST ),

    test_facilities:display( "One-word spell tree:~n ~ts.",
                             [ spell_tree:to_string( OneWST ) ] ),


    MultiWST = spell_tree:register_strings( AllStrs, OneWST ),

    test_facilities:display( "Multiple-word spell tree:~n ~ts.",
                             [ spell_tree:to_string( MultiWST ) ] ),


    ToCompleteStr = "pl",

    Candidates = spell_tree:find_candidates( ToCompleteStr, MultiWST ),

    test_facilities:display( "Candidates determined for prefix '~ts' "
        "in the following tree:~n ~ts are:~n ~p~n",
        [ ToCompleteStr, spell_tree:to_string( MultiWST ), Candidates ] ),

    list_utils:check_unordered_match( Candidates,
        [ "place", "platypus", "plate", "placebo" ] ),


    TestStrs = AllStrs
        ++ [ "", "t", "other", "p", "pla", "te", "er", "erica", "xyz" ],

    test_facilities:display( "Testing a series of candidates, all based "
        "on the registering of:~n ~p.~nPrefixes to be completed are:~n ~p",
        [ lists:sort( AllStrs ), TestStrs ] ),


    % Each list of candidates corresponds to the ones for the test string of the
    % same index in TestStrs:
    %
    TestCandidates = [ [ "test", "testing" ], [ "testing" ],
        [ "place", "placebo" ], [ "platypus" ], [ "porridge" ], [ "plate" ],
        [ "placebo" ], [ "eric" ], [ "erlang" ], [ "eric" ],
        [ "stop", "stop_other" ], [ "stop_other" ], lists:uniq( AllStrs ),
        [ "test", "testing" ], _Other=[],
        _P=[ "place", "platypus", "porridge", "plate", "placebo" ],
        _Pla=[ "place", "platypus", "plate", "placebo" ],
        _Te=[ "test", "testing" ], _Er=[ "eric", "erlang" ], _Erica=[],
        _Xyz=[] ],

    TestPairs = lists:zip( TestStrs, TestCandidates ),

    [ begin
        Cands = spell_tree:find_candidates( TStr, MultiWST ),
        test_facilities:display(
            "Candidates determined for prefix '~ts': ~p.",
            [ TStr, Candidates ] ),
          list_utils:check_unordered_match( Cands, TCorrectCands )
      end || { TStr, TCorrectCands } <- TestPairs  ],


    Splitters = spell_tree:get_splitters( MultiWST ),

    test_facilities:display( "~B splitters found for:~n ~ts~nand they are: ~ts",
        [ length( Splitters ), spell_tree:to_string( MultiWST ),
          text_utils:strings_to_string(
              [ spell_tree:splitter_to_string( S ) || S <- Splitters ] ) ] ),

    ToResolveStrs = [ "", "foo",
         "t", "te", "tes", "test", "testo", "testi", "testin", "testing",
         "testingxx",
         "p", "pl", "pla", "plat", "plate", "placeb", "erk", "erl" ],

    ResolvedStrs = [ spell_tree:resolve( S, MultiWST ) || S <- ToResolveStrs ],

    test_facilities:display( "Resolving the following strings: ~ts",
        [ text_utils:strings_to_string(
              [ text_utils:format( "'~ts' in ~p", [ S, Rs ] )
                    || { S, Rs } <- lists:zip( ToResolveStrs,
                                               ResolvedStrs ) ] ) ] ),

    test_facilities:stop().
