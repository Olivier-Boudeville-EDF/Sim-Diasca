% Copyright (C) 2016-2021 Olivier Boudeville
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


% Unit tests for the set management utilities.
%
% See the list_utils.erl tested module.
%
-module(set_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Testing set management:

	EmptySet = set_utils:new(),

	true = set_utils:is_empty( EmptySet ),
	0 = set_utils:size( EmptySet ),

	test_facilities:display( "Empty: ~ts", 
							 [ set_utils:to_string( EmptySet ) ] ),


	Singleton = set_utils:singleton( first ),

	false = set_utils:is_empty( Singleton ),
	1 = set_utils:size( Singleton ),

	test_facilities:display( "Singleton: ~ts",
							 [ set_utils:to_string( Singleton ) ] ),


	TwoElements = set_utils:add( second, Singleton ),

	false = set_utils:is_empty( TwoElements ),
	2 = set_utils:size( TwoElements ),

	test_facilities:display( "Two elements: ~ts",
							 [ set_utils:to_string( TwoElements ) ] ),


	% Expected stable (not a real set comparison):
	TwoElements = set_utils:add_element_list( [], TwoElements ),

	FourElements = set_utils:add_element_list( [ third, fourth ], TwoElements ),

	false = set_utils:is_empty( FourElements ),
	4 = set_utils:size( FourElements ),

	test_facilities:display( "Four elements: ~ts",
							 [ set_utils:to_string( FourElements ) ] ),

	ThreeElements = set_utils:delete_existing( third, FourElements ),

	false = set_utils:is_empty( ThreeElements ),
	3 = set_utils:size( ThreeElements ),

	test_facilities:display( "Three elements: ~ts",
							 [ set_utils:to_string( ThreeElements ) ] ),

	OtherTwoElements = set_utils:delete_existing( first, ThreeElements ),

	false = set_utils:is_empty( OtherTwoElements ),
	2 = set_utils:size( OtherTwoElements ),

	test_facilities:display( "Other two elements: ~ts",
							 [ set_utils:to_string( OtherTwoElements ) ] ),

	test_facilities:stop().
