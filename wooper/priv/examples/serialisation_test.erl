% Copyright (C) 2012-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Testing the implementation of the serialisation of WOOPER instances.
-module(serialisation_test).


% For run/0 export and al:
-include_lib("myriad/include/test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	MyC = class_Cat:new_link( 3, female, sand, white ),

	MyC ! { toString, [], self() },

	receive

		{ wooper_result, FirstDescription } ->
			test_facilities:display( "Created following cat: ~ts~n" ,
									 [ FirstDescription ] )

	end,


	MyC ! { getAge, [], self() },
	receive

		{ wooper_result, 3 } ->
			ok

	end,

	ActualUserData = none,

	% This is a do-nothing transformer, except that it outputs on the console
	% the attributes it filters:
	%
	TextTransformer = fun( Entry={ AttributeName, AttributeValue },
						   _Acc={ AccEntries, AccUserData } ) ->

			test_facilities:display( " - attribute name '~ts' is associated "
				"to value '~p'", [ AttributeName, AttributeValue ] ),

			% New accumulator:
			{ [ Entry | AccEntries ], AccUserData }

	end,

	MyC ! { serialise, [ TextTransformer, ActualUserData ], self() },

	CatBinSerialisation = receive

		{ wooper_result, { Bin, UserData } } ->

			{ Class, _TransformedEntries } = binary_to_term( Bin ),

			test_facilities:display( "Text transformer returned:~n"
									 " - class name: ~p~n"
									 " - binary serialisation: ~p~n"
									 " - binary size: ~B bytes~n"
									 " - user data: ~p~n",
									 [ Class, Bin, size( Bin ), UserData ] ),

			Bin

	end,


	MyC ! { synchronous_delete, self() },
	receive

		{ deleted, MyC } ->
			ok

	end,


	test_facilities:display( "Testing also serialisation hooks, "
							 "with a reptile." ),

	MyR = class_Reptile:new_link( 35, female ),

	MyR ! { serialise, [ _TextTransformer=undefined,
						 _ActualUserData=undefined ], self() },

	receive

		{ wooper_result, { ReptileBin, ReptileUserData } } ->

			{ ReptileClass, _ReptileEntries } = binary_to_term( ReptileBin ),

			test_facilities:display( "Text transformer returned:~n"
									 " - class name: ~p~n"
									 " - binary serialisation: ~p~n"
									 " - binary size: ~B bytes~n"
									 " - user data: ~p~n",
									 [ ReptileClass, ReptileBin,
									   size( ReptileBin ),  ReptileUserData ] )

	end,

	test_facilities:display( "Recreating an instance corresponding to "
							 "previous information." ),

	NewC = wooper_serialisation:load_link( CatBinSerialisation ),

	NewC ! { toString, [], self() },

	receive

		{ wooper_result, SecondDescription } ->
			test_facilities:display( "Deserialised following cat: ~ts~n" ,
									 [ SecondDescription ] )

	end,

	NewC ! { getAge, [], self() },
	receive

		{ wooper_result, 3 } ->
			ok

	end,

	NewC ! { synchronous_delete, self() },
	receive

		{ deleted, NewC } ->
			ok

	end,

	test_facilities:stop().
