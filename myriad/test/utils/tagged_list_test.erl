% Copyright (C) 2024-2026 Olivier Boudeville
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
% Creation date: Saturday, September 21, 2024.

-module(tagged_list_test).

-moduledoc """
Unit tests for the tagged_lists.

See the tagged_list.erl tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").



test_tagged() ->

    TL = [ f, { a, 1 }, b, { c, 2 }, {a, 2 }, b, a, { d, 1 }, e ],


    test_facilities:display( "Testing tagged lists." ),

    tagged_list:check_tagged_list( TL ),

    % No (first) b:
    NobTL = [ f, { a, 1 }, { c, 2 }, {a, 2 }, b, a, { d, 1 }, e ],

    NobTL = tagged_list:extract_atom_if_existing( b, TL ),
    not_found = tagged_list:extract_atom_if_existing( z, TL ),

    { b, NobTL } = tagged_list:extract_atom_with_default( b, w, TL ),
    { w, TL } = tagged_list:extract_atom_with_default( z, w, TL ),

    % No first a:
    NoFirstaTL = [ f, b, { c, 2 }, {a, 2 }, b, a, { d, 1 }, e ],

    { 1, NoFirstaTL } = tagged_list:extract_pair_if_existing( a, TL ),
    { w, TL } = tagged_list:extract_pair_with_default( z, w, TL ),

    true = list_utils:unordered_compare( [ { a, 3 }, { b, 2 } ],
        tagged_list:check_duplicate_keys( TL ) ).



test_strict_tagged() ->

    STL = [ { f, 1 }, { a, 1 }, { c, 2 }, {a, 2 }, { d, 1 }, { c, 4 },
            { c, 3 } ],

    test_facilities:display( "Testing strict tagged lists." ),

    tagged_list:check_strict_tagged_list( STL ),


    % No first a:
    NoFirstaSTL = [ { f, 1 }, { c, 2 }, {a, 2 }, { d, 1 }, { c, 4 }, { c, 3 } ],

    { 1, NoFirstaSTL } = tagged_list:extract_pair_if_existing_strict( a, STL ),
    { w, STL } = tagged_list:extract_pair_with_default_strict( z, w, STL ),

    true = list_utils:unordered_compare( [ { a, 2 }, { c, 3 } ],
        tagged_list:check_strict_duplicate_keys( STL ) ).



-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    test_tagged(),
    test_strict_tagged(),

    test_facilities:stop().
