% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Unit tests for the ViviparousBeing class implementation.
% See the class_ViviparousBeing.erl tested module.
%
-module(class_ViviparousBeing_test).


-include_lib("myriad/include/test_facilities.hrl").


-export([ run/1 ]).


-spec run() -> no_return().
run() ->
	run( class_ViviparousBeing:is_wooper_debug() ).


-spec run( boolean() ) -> no_return().
run( IsDebug ) ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	test_facilities:display( "Debug mode: ~ts.",
							 [ class_ViviparousBeing:is_wooper_debug() ] ),

	MyV = class_ViviparousBeing:new_link(),

	MyV ! { getClassname, [], self() },
	receive

		{ wooper_result, class_ViviparousBeing } ->
			test_facilities:display(
				"After constructor, getClassname/1 returned "
				"'class_ViviparousBeing' as expected." );

		{ wooper_result, UnexpectedClass } ->
			test_facilities:fail( "wrong class: ~p", [ UnexpectedClass ] )

	end,

	MyV ! { getSuperclasses, [], self() },
	receive

		{ wooper_result, [] } ->
			test_facilities:display( "After constructor, "
							"getSuperclasses/1 returned [] as expected." );

		{ wooper_result, UnexpectedSuperclasses } ->
			test_facilities:fail( "wrong superclasses: ~p",
								  [ UnexpectedSuperclasses ] )

	end,

	MyV ! { getMeanChildrenCount, [], self() },
	receive

		{ wooper_result, 4 } ->
			test_facilities:display(
				"After constructor, getMeanChildrenCount/1 returned 4 "
				"as expected." );

		{ wooper_result, UnexpectedMeanCount } ->
			test_facilities:fail( "wrong mean children count: ~p",
								  [ UnexpectedMeanCount ] )


	end,

	MyV ! { getBirthGivenCount, [], self() },
	receive

		{ wooper_result,0 } ->
			test_facilities:display( "After constructor, getBirthGivenCount/1 "
									 "returned 0 as expected." );

		{ wooper_result, UnexpectedFirstCount } ->
			test_facilities:fail( "wrong first children count: ~p",
								  [ UnexpectedFirstCount ] )

	end,

	MyV ! { giveBirth, 7 },

	MyV ! { getBirthGivenCount, [], self() },
	receive

		{ wooper_result, 7 }->
			test_facilities:display( "After giveBirth/2, getBirthGivenCount/1 "
									 "returned 7 as expected." );

		{ wooper_result, UnexpectedSecondCount } ->
			test_facilities:fail( "wrong second children count: ~p",
								  [ UnexpectedSecondCount ] )

	end,

	case IsDebug of

		true ->
			MyV ! { wooper_get_instance_description, [], self() },
			receive

				{ wooper_result, InspectString } ->
					test_facilities:display( "Instance description: ~ts",
											 [ InspectString ] )
			end;

		false ->
			ok

	end,

	% Better than 'MyV ! delete':
	wooper:delete_synchronously_instance( MyV ),

	test_facilities:stop().
