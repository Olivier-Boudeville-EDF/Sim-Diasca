% Copyright (C) 2018-2021 Olivier Boudeville
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
% Creation date: Monday, April 30, 2018



% Testing of the support of the process dictionary.
%
% See process_dictionary tested module.
%
-module(process_dictionary_test).

% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the support of the process dictionary." ),

	test_facilities:display( "Initial state: ~ts",
							 [ process_dictionary:to_string() ] ),

	InitTable = process_dictionary:get_dictionary(),

	test_facilities:display( "Initial content: ~ts",
							 [ list_table:to_string( InitTable ) ] ),

	FirstKey = hello,

	FirstValue = self(),

	undefined = process_dictionary:get( FirstKey ),

	undefined = process_dictionary:put( FirstKey, FirstValue ),

	FirstValue = process_dictionary:get( FirstKey ),


	SecondValue = 42,

	FirstValue = process_dictionary:put( FirstKey, SecondValue ),

	SecondValue = process_dictionary:remove( FirstKey ),

	[] = process_dictionary:blank(),


	ThirdValue = "world",

	undefined = process_dictionary:put( FirstKey, ThirdValue ),

	[ FirstKey ] = process_dictionary:get_keys(),

	[ FirstKey ] = process_dictionary:get_keys_for( ThirdValue ),

	FinalTable = process_dictionary:get_dictionary(),

	test_facilities:display( "Final content: ~ts",
							 [ list_table:to_string( FinalTable ) ] ),

	test_facilities:display( "Final state: ~ts",
							 [ process_dictionary:to_string() ] ),

	test_facilities:stop().
