% Copyright (C) 2012-2025 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: 2012.

-module(serialisable_test).

-moduledoc """
Testing the implementation of the **serialisation of WOOPER instances**, that is
 of the default implementation of the Serialisable interface.
""".



% For run/0 export and al:
-include_lib("myriad/include/test_facilities.hrl").


test_individual_serialisations() ->

	test_facilities:display( "~nTesting individual serialisations." ),

	Age = 3,

	MyC = class_Cat:new_link( Age, female, sand, white ),

	MyC ! { toString, [], self() },

	receive

		{ wooper_result, FirstDescription } ->
			test_facilities:display( "Created following cat: ~ts~n" ,
									 [ FirstDescription ] )

	end,


	MyC ! { getAge, [], self() },
	receive

		{ wooper_result, Age } ->
			ok

	end,

	ActualUserData = none,

	% This is a do-nothing transformer, except that it outputs on the console
	% the attributes that it filters:
	%
	TextTransformer =
		fun( Entry={ AttributeName, AttributeValue },
			 _Acc={ AccEntries, AccUserData } ) ->

			test_facilities:display( " - text transformer: "
				"attribute name '~ts' is associated to value '~p'",
				[ AttributeName, AttributeValue ] ),

			% New accumulator:
			{ [ Entry | AccEntries ], AccUserData }

		end,

	MyC ! { serialise, [ TextTransformer, ActualUserData ], self() },

	CatSerialisation = receive

		{ wooper_result, { CatSerial, SerialUserData, MyC } } ->
			test_facilities:display( "Text transformer returned:~n"
				" - serialisation of ~ts:~n ~p~n"
				" - resulting user data: ~p~n",
				[ system_utils:interpret_byte_size(
					system_utils:get_size( CatSerial ) ), CatSerial,
				  SerialUserData ] ),

			CatSerial

	end,


	MyC ! { synchronous_delete, self() },
	receive

		{ deleted, MyC } ->
			ok

	end,


	test_facilities:display( "Testing also serialisation overridden method "
							 "(hooks), with a reptile." ),

	MyR = class_Reptile:new_link( 35, female ),

	MyR ! { serialise, [], self() },

	ReptileSerialisation = receive

		{ wooper_result, { ReptSerial, MyR } } ->

			test_facilities:display( "Default transformer returned a "
				"serialisation of ~ts:~n ~p~n",
				[ system_utils:interpret_byte_size(
					system_utils:get_size( ReptSerial ) ), ReptSerial ] ),

			ReptSerial

	end,


	test_facilities:display(
		"Recreating an instance corresponding to previous information." ),

	NewC = class_Serialisable:load_link( CatSerialisation ),

	NewC ! { toString, [], self() },

	receive

		{ wooper_result, SecondDescription } ->
			test_facilities:display( "Deserialised following cat: ~ts~n" ,
									 [ SecondDescription ] )

	end,

	NewC ! { getAge, [], self() },
	receive

		{ wooper_result, Age } ->
			ok

	end,

	NewC ! { synchronous_delete, self() },
	receive

		{ deleted, NewC } ->
			ok

	end,


	NewR = class_Serialisable:load_link( ReptileSerialisation ),

	NewR ! { synchronous_delete, self() },
	receive

		{ deleted, NewR } ->
			ok

	end.



test_wsf_format() ->

	ReptileCount = 5,

	test_facilities:display( "~nTesting the support of a stream of "
							 "~B serialisations.", [ ReptileCount ] ),

	OriginalReptilePids = [
		class_Reptile:new_link( _Age=random_utils:get_uniform_value( 20 ),
			_Gender= case random_utils:get_uniform_value( 2 ) of
				1 -> male;
				2 -> female end ) || _ <- lists:seq( 1, ReptileCount ) ],

	% Obtaining a basic binary:
	ReptileSerials =
		wooper_serialisation:serialise_instances( OriginalReptilePids ),

	SerialFilename = "serialisation-test.wsf",

	% Should any prior test have crashed:
	file_utils:remove_file_if_existing( SerialFilename ),

	file_utils:write_whole( SerialFilename, ReptileSerials ),

	test_facilities:display( "Serialisation file '~ts' written, "
		"deleting the corresponding serialised instances, and deserialing "
		"them from file.", [ SerialFilename ] ),

	wooper:delete_synchronously_instances( OriginalReptilePids ),

	ReadSerial = file_utils:read_whole( SerialFilename ),

	FileSize = file_utils:get_size( SerialFilename ),

	test_facilities:display( "The size of the serialisation file '~ts' is ~ts, "
		"this corresponds to an average size per Reptile instance of ~ts.",
		[ SerialFilename, system_utils:interpret_byte_size( FileSize ),
		  system_utils:interpret_byte_size( round( FileSize / ReptileCount ) )
		] ),

	file_utils:remove_file( SerialFilename ),


	% All combinations (synchronous or not, linked or not) can be tested:

	%ReadReptilePids = wooper_serialisation:load_instances( ReadSerial ),
	%ReadReptilePids = wooper_serialisation:load_link_instances( ReadSerial ),

	%ReadReptileLoadInfos =
	%   wooper_serialisation:synchronous_load_instances( ReadSerial ),

	ReadReptileLoadInfos =
		wooper_serialisation:synchronous_load_link_instances( ReadSerial ),

	ReadReptilePids = [ RepPid
		|| { RepPid, class_Reptile, _UpdatedUserData=undefined }
										<- ReadReptileLoadInfos ],


	[ R ! { getDescription, [], self() } || R <- ReadReptilePids ],

	% No specific order expected:
	Descs = [ receive
				{ wooper_result, D } -> D
			  end || _R <- ReadReptilePids ],

	test_facilities:display( "Read descriptions: ~ts",
		[ text_utils:strings_to_string( Descs ) ] ),

	wooper:delete_synchronously_instances( ReadReptilePids ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	test_individual_serialisations(),

	test_wsf_format(),

	test_facilities:stop().
