% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Unit tests for the Mammal class implementation.
%
% See the class_Mammal.erl tested module.
%
-module(class_Mammal_test).


-include_lib("myriad/include/test_facilities.hrl").


-export([ run/1 ]).


-spec run() -> no_return().
run() ->
	run( class_Mammal:is_wooper_debug() ).


-spec run( boolean() ) -> no_return().
run( IsDebug ) ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	test_facilities:display( "Debug mode: ~ts.",
							 [ class_Mammal:is_wooper_debug() ] ),


	MyM = class_Mammal:synchronous_new_link( 30, male, brown ),

	MyM ! { getClassname, [], self() },
	receive

		{ wooper_result, class_Mammal } ->
			test_facilities:display(
				"After constructor, getClassname/1 returned 'class_Mammal' "
				"as expected.");

		{ wooper_result, UnexpectedClass } ->
			test_facilities:fail( "wrong class: ~p", [ UnexpectedClass ] )

	end,

	MyM ! { getSuperclasses, [], self() },
	receive

		{ wooper_result, [ class_Creature ] } ->
			test_facilities:display(
				"After constructor, getSuperclasses/1 returned: class_Creature "
				"as expected.");

		{ wooper_result, UnexpectedSuperclasses } ->
			test_facilities:fail( "wrong superclasses: ~p",
								  [ UnexpectedSuperclasses ] )

	end,

	MyM ! { getAge, [], self() },
	receive

		{ wooper_result, 30 } ->
			test_facilities:display(
				"After constructor, getAge/1 returned 30 as expected.");

		{ wooper_result, UnexpectedAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedAge ] )

	end,

	MyM ! { getGender, [], self() },
	receive

		{ wooper_result, male } ->
			test_facilities:display(
				"After constructor, getGender/1 returned male as expected." );

		{ wooper_result, UnexpectedGender } ->
			test_facilities:fail( "wrong gender: ~p", [ UnexpectedGender ] )

	end,

	MyM ! { setAge, 5 },

	MyM ! { getAge, [], self() },
	receive

		 { wooper_result, 5 }->
			test_facilities:display(
				"After setAge/2, getAge/1 returned 5 as expected." );

		{ wooper_result, UnexpectedNewAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedNewAge ] )

	end,

	MyM ! declareBirthday,

	MyM ! { getAge, [], self() },
	receive

		 { wooper_result, 6 }->
			test_facilities:display(
				"After declareBirthday/1, getAge/1 returned 6 as expected." );

		{ wooper_result, UnexpectedLastAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedLastAge ] )

	end,

	MyM ! declareBirthday,

	MyM ! { isHotBlooded, [], self() },
	receive

		 { wooper_result, true }->
			test_facilities:display(
				"isHotBlooded/1 returned true as expected." );

		{ wooper_result, UnexpectedBlood } ->
			test_facilities:fail( "wrong blood type: ~p",
								  [ UnexpectedBlood ] )

	end,

	% Not too late in the test to have enough time to execute fully:
	test_facilities:display( "Testing direct method invocation." ),

	% Inherited from Creature:
	MyM ! { testDirectMethodExecution, 347 },

	MyM ! testExplicitClassSelection,

	MyM ! { getFurColor, [], self() },
	receive

		 { wooper_result, brown }->
			test_facilities:display(
				"getFurColor/1 returned brown as expected." );

		{ wooper_result, UnexpectedFurColor } ->
			test_facilities:fail( "wrong fur color: ~p",
								  [ UnexpectedFurColor ] )

	end,


	case IsDebug of

		true ->

			MyM ! { wooper_get_instance_description, [], self() },
			receive

				{ wooper_result, InspectString } ->
					test_facilities:display( "Instance description: ~ts",
											 [ InspectString ] )
			end;

		false ->
			ok

	end,

	wooper:delete_synchronously_instance( MyM ),

	test_facilities:stop().
