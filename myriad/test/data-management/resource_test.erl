% Copyright (C) 2022-2023 Olivier Boudeville
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
% Creation date: Wednesday, April 6, 2022.


% @doc Test of the <b>resource</b> implementation.
%
% See the bijective_table.erl tested module.
%
-module(resource_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec test_referentials() -> void().
test_referentials() ->

	test_facilities:display( "Testing resource referentials." ),

	EmptyRef = resource:create_referential( _RootDir="." ),

	test_facilities:display( "Initial anchored referential: ~ts.",
							 [ resource:referential_to_string( EmptyRef ) ] ),

	TestFileId = "resource_test.erl",

	false = resource:has( TestFileId, EmptyRef ),

	{ TestFileRsc, FirstRef } = resource:get( TestFileId, EmptyRef ),

	test_facilities:display( "Read for '~ts': ~p",
							 [ TestFileId, TestFileRsc ] ),

	test_facilities:display( "First non-empty referential: ~ts.",
							 [ resource:referential_to_string( FirstRef ) ] ),

	true = resource:has( TestFileId, FirstRef ),

	test_facilities:display( "Path of '~ts' is '~ts'.",
		[ TestFileId, resource:get_path( TestFileId, FirstRef ) ] ),


	TestLogId = 'my_resource_id',

	false = resource:has( TestLogId, FirstRef ),

	SecondRef = resource:register( TestLogId, 42.0, FirstRef ),

	test_facilities:display( "Second non-empty referential: ~ts.",
							 [ resource:referential_to_string( SecondRef ) ] ),

	true = resource:has( TestLogId, SecondRef ),


	ThirdRef = resource:remove( TestFileId, SecondRef ),

	test_facilities:display( "Third non-empty referential: ~ts.",
							 [ resource:referential_to_string( ThirdRef ) ] ),

	false = resource:has( TestFileId, ThirdRef ),


	InitialNonAnchoredRef = resource:create_referential(),

	test_facilities:display( "Initial non-anchored referential: ~ts.",
		[ resource:referential_to_string( InitialNonAnchoredRef ) ] ),

	false = resource:has( TestFileId, InitialNonAnchoredRef ),

	{ TestFileRsc, FirstNonAncRef } =
		resource:get( TestFileId, InitialNonAnchoredRef ),

	true = resource:has( TestFileId, FirstNonAncRef ),

	test_facilities:display( "Final non-anchored referential: ~ts.",
		[ resource:referential_to_string( FirstNonAncRef ) ] ).



-spec test_servers() -> void().
test_servers() ->

	test_facilities:display( "Testing resource servers." ),

	RootDir = ".",

	RscSrvPid = resource:create_linked_server( RootDir ),

	test_facilities:display( "Created resource server ~w based on "
							 "directory '~ts'.", [ RscSrvPid, RootDir ] ),

	TestFileId = "resource_test.erl",

	false = resource:has( TestFileId, RscSrvPid ),

	TestFileRsc = resource:get( TestFileId, RscSrvPid ),

	%test_facilities:display( "Read for '~ts': ~p",
	%                         [ TestFileId, TestFileRsc ] ),


	true = resource:has( TestFileId, RscSrvPid ),

	test_facilities:display( "Path of '~ts' is '~ts'.",
		[ TestFileId, resource:get_path( TestFileId, RscSrvPid ) ] ),


	TestLogId = 'my_resource_id',

	false = resource:has( TestLogId, RscSrvPid ),

	resource:register( TestLogId, 42.0, RscSrvPid ),

	true = resource:has( TestLogId, RscSrvPid ),

	resource:remove( TestFileId, RscSrvPid ),

	false = resource:has( TestFileId, RscSrvPid ),


	NonAnchoredRscSrvPid = resource:create_linked_server(),

	false = resource:has( TestFileId, NonAnchoredRscSrvPid ),

	TestFileRsc = resource:get( TestFileId, NonAnchoredRscSrvPid ),

	true = resource:has( TestFileId, NonAnchoredRscSrvPid ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_referentials(),

	test_servers(),

	test_facilities:stop().
