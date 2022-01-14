% Copyright (C) 2003-2022 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Unit tests for the Creature class implementation.
% See the class_Creature.erl tested module.
%
-module(class_Creature_test).


-include_lib("myriad/include/test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	test_facilities:display( "Debug mode: ~ts.",
							[ class_Creature:is_wooper_debug() ] ),

	test_facilities:display( "Class name is , superclasses are ~w.",
							 [ class_Creature:get_superclasses() ] ),

	MyC = class_Creature:new_link( 30, male ),

	MyC ! { getAge, [], self() },
	receive

		{ wooper_result, 30 } ->
			test_facilities:display(
				"After constructor, getAge returned 30 as expected." );

		{ wooper_result, UnexpectedAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedAge ] )

	end,

	MyC ! { getGender, [], self() },
	receive

		{ wooper_result, male } ->
			test_facilities:display(
				"After constructor, getGender returned male as expected." );

		{ wooper_result, UnexpectedGender } ->
			test_facilities:fail( "wrong gender: ~p", [ UnexpectedGender ] )

	end,

	MyC ! { setAge, 5 },

	% class_Creature:getAge returns always 36 for executeRequest test purposes:
	MyC ! { getAge, [], self() },
	receive

		{ wooper_result, 36 } ->
			test_facilities:display(
				"After setAge, getAge returned 36 as expected.");

		{ wooper_result, UnexpectedNewAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedNewAge ] )

	end,
	MyC ! declareBirthday,

	MyC ! { getAge, [], self() },
	receive

		{ wooper_result, 37 }->
			test_facilities:display(
				"After declareBirthday, getAge returned 37 as expected." );

		{ wooper_result, UnexpectedLastAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedLastAge ] )

	end,

	MyC ! declareBirthday,


	% Some more technical checkings:

	% Note to be called here, otherwise will fail:
	%MyC ! { testDirectMethodExecution, 36 },

	MyC ! testSingleExecution,


	% Ensures a synchronous ending:
	wooper:delete_synchronously_instance( MyC ),

	test_facilities:display( "Synchronous deletion succeedeed." ),

	test_facilities:stop().
