% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Unit tests for the Reptile class implementation.
%
% See the class_Reptile.erl tested module.
%
-module(class_Reptile_test).


-include_lib("myriad/include/test_facilities.hrl").


-export([ run/1 ]).


-spec run() -> no_return().
run() ->
	run( class_Reptile:is_wooper_debug() ).


-spec run( boolean() ) -> no_return().
run( IsDebug ) ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	test_facilities:display( "Debug mode: ~ts.",
							 [ class_Reptile:is_wooper_debug() ] ),


	MyR = class_Reptile:new_link( 1, male ),

	MyR ! { getClassname, [], self() },
	receive

		{ wooper_result, class_Reptile } ->
			test_facilities:display(
				"After constructor, getClassname/1 returned 'class_Reptile' "
				"as expected." );

		{ wooper_result, UnexpectedClass } ->
			test_facilities:fail( "wrong class: ~p", [ UnexpectedClass ] )

	end,

	MyR ! { getSuperclasses, [], self() },
	receive

		{ wooper_result, [ class_Creature ] } ->
			test_facilities:display(
				"After constructor, getSuperclasses/1 returned class_Creature "
				"as expected." );

		{ wooper_result, UnexpectedSuperclasses } ->
			test_facilities:fail( "wrong superclasses: ~p",
								  [ UnexpectedSuperclasses ] )

	end,

	MyR ! { getAge, [], self() },
	receive

		{ wooper_result, 1 } ->
			test_facilities:display(
				"After constructor, getAge/1 returned 1 as expected." );

		{ wooper_result, UnexpectedAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedAge ] )

	end,

	MyR ! { getGender, [], self() },
	receive

		{ wooper_result, male } ->
			test_facilities:display(
				"After constructor, getGender/1 returned male as expected." );

		{ wooper_result, UnexpectedGender } ->
			test_facilities:fail( "wrong gender: ~p", [ UnexpectedGender ] )

	end,

	MyR ! { setAge, 2 },

	MyR ! { getAge, [], self() },
	receive

		{ wooper_result, 2 }->
			test_facilities:display(
				"After setAge, getAge/1 returned 2 as expected." );

		{ wooper_result, UnexpectedNewAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedNewAge ] )

	end,

	MyR ! declareBirthday,

	MyR ! { getAge, [], self() },
	receive

		{ wooper_result, 3 }->
			test_facilities:display(
				"After declareBirthday, getAge/1 returned 3 as expected." );

		{ wooper_result, UnexpectedLastAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedLastAge ] )

	end,

	MyR ! declareBirthday,

	MyR ! { isHotBlooded, [], self() },
	receive

		{ wooper_result, false }->
			test_facilities:display(
				"isHotBlooded/1 returned false as expected." );

		{ wooper_result, UnexpectedBlood } ->
			test_facilities:fail( "wrong blood type: ~p", [ UnexpectedBlood ] )

	end,

	MyR ! { canMoult, [], self() },
	receive

		{ wooper_result, true }->
			test_facilities:display(
				"canMoult/1 returned true as expected." );

		{ wooper_result, UnexpectedMoultType } ->
			test_facilities:fail( "wrong moult type: ~p",
								  [ UnexpectedMoultType ] )

	end,

	case IsDebug of

		true ->

			MyR ! { wooper_get_instance_description, [], self() },
			receive

				{ wooper_result, InspectString } ->
					test_facilities:display( "Instance description: ~ts",
											 [ InspectString ] )
			end;

		false ->
			ok

	end,

	% To check the result when using a faulty destructor:
	test_facilities:display( "synchronous deletion of the instance." ),

	wooper:delete_synchronously_instance( MyR ),

	test_facilities:stop().
