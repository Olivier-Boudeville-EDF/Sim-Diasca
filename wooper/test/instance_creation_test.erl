% Copyright (C) 2014-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER library.
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
% Creation date: Wednesday, October 31, 2018.



% This module allows to test with as few dependencies as possible the creation
% of class instances, using for that the various new operators.
%
-module(instance_creation_test).


-export([ run/0 ]).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	TestedClass = class_BaseTestClass,

	test_facilities:display( "Testing instance creation for class '~ts'.",
							 [ TestedClass ] ),

	% We reuse here the conventional naming of the V* new operators introduced
	% in the WOOPER parse transform:

	V1Name = "created with new",
	V1Pid = TestedClass:new( V1Name, male ),
	test_instance( V1Pid, V1Name ),

	V1LinkName = "created with new_link",
	V1LinkPid = TestedClass:new_link( V1LinkName, male ),
	test_instance( V1LinkPid, V1LinkName ),


	V2Name = "created with synchronous_new",
	V2Pid = TestedClass:synchronous_new( V2Name, male ),
	test_instance( V2Pid, V2Name ),

	V2LinkName = "created with synchronous_new_link",
	V2LinkPid = TestedClass:synchronous_new_link( V2LinkName, male ),
	test_instance( V2LinkPid, V2LinkName ),


	V3Name = "created with synchronous_timed_new",
	V3Pid = TestedClass:synchronous_timed_new( V3Name, male ),
	test_instance( V3Pid, V3Name ),

	V3LinkName = "created with synchronous_timed_new_link",
	V3LinkPid = TestedClass:synchronous_timed_new_link( V3LinkName, male ),
	test_instance( V3LinkPid, V3LinkName ),


	TargetNode = node(),

	V4Name = "created with remote_new",
	V4Pid = TestedClass:remote_new( TargetNode, V4Name, male ),
	test_instance( V4Pid, V4Name ),

	V4LinkName = "created with remote_new_link",
	V4LinkPid = TestedClass:remote_new_link( TargetNode, V4LinkName,
													 male ),
	test_instance( V4LinkPid, V4LinkName ),


	V5Name = "created with remote_synchronous_new",
	V5Pid = TestedClass:remote_synchronous_new( TargetNode, V5Name, male ),
	test_instance( V5Pid, V5Name ),

	V5LinkName = "created with remote_synchronous_new_link",
	V5LinkPid = TestedClass:remote_synchronous_new_link( TargetNode, V5LinkName,
														 male ),
	test_instance( V5LinkPid, V5LinkName ),


	V6Name = "created with remote_synchronisable_new",
	V6Pid = TestedClass:remote_synchronisable_new( TargetNode, V6Name, male ),
	test_instance( V6Pid, V6Name ),

	% Done after the test for a clearer trace ordering:
	receive

		{ spawn_successful, V6Pid } ->
			test_facilities:display(
			  "   (and '~ts' (~w) synchronised adequately)",
			  [ V6Name, V6Pid ] )

	end,

	V6LinkName = "created with remote_synchronisable_new_link",
	V6LinkPid = TestedClass:remote_synchronisable_new_link( TargetNode,
															V6LinkName, male ),
	test_instance( V6LinkPid, V6LinkName ),

	% Done after the test for a clearer trace ordering:
	receive

		{ spawn_successful, V6LinkPid } ->
			test_facilities:display(
			  "   (and '~ts' (~w) synchronised adequately)",
			  [ V6LinkName, V6LinkPid ] )

	end,


	V7Name = "created with remote_synchronous_timed_new",
	V7Pid = TestedClass:remote_synchronous_timed_new( TargetNode, V7Name,
													  male ),
	test_instance( V7Pid, V7Name ),


	V7LinkName = "created with remote_synchronous_timed_new_link",
	V7LinkPid = TestedClass:remote_synchronous_timed_new_link( TargetNode,
														   V7LinkName, male ),
	test_instance( V7LinkPid, V7LinkName ),

	V8Name = "created with new_passive",
	_V8 = TestedClass:new_passive( V8Name, female ),
	test_facilities:display( " - '~ts' created adequately", [ V8Name ] ),

	wooper:delete_synchronously_instances( [ V1Pid, V1LinkPid,
		V2Pid, V2LinkPid, V3Pid, V3LinkPid, V4Pid, V4LinkPid,
		V5Pid, V5LinkPid, V6Pid, V6LinkPid, V7Pid, V7LinkPid ] ),

	test_facilities:stop().



% Tests whether the specified instance is operational.
test_instance( Pid, Name ) ->

	Pid ! { getName, [], self() },

	receive

		{ wooper_result, Name } ->
			test_facilities:display(
			  " - '~ts' (~w) alive and answering adequately", [ Name, Pid ] )

	end.
