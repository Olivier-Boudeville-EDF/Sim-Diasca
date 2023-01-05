% Copyright (C) 2007-2023 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: 2007.


% @doc Class modelling any kind of <b>reptile</b>.
%
% This class allows to test serialisation for an exploratory resilience support.
%
-module(class_Reptile).


-define( class_description, "Class modelling any kind of reptile." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_OvoviviparousBeing, class_Serialisable,
						 class_StaticDescribable ] ).


-define( class_attributes, [

	reptile_family

						   ] ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").



% Shorthands:

-type user_data() :: class_Serialisable:user_data().
-type serialisation() :: class_Serialisable:serialisation().



% @doc Constructs a reptile.
-spec construct( wooper:state(), age(), gender() ) -> wooper:state().
construct( State, Age, Gender ) ->

	OvivipState = class_ViviparousBeing:construct( State ),
	SerialState = class_Serialisable:construct( OvivipState ),

	Family = cobra,

	Description = text_utils:format( "a ~ts ~ts of age ~B",
									 [ Gender, Family, Age ] ),

	DescState = class_StaticDescribable:construct( SerialState, Description ),

	% To test constructor checking:
	%an_unexpected_initial_state.

	setAttributes( DescState, [
		{ age, Age },
		{ gender, Gender },
		{ reptile_family, Family }

		% To test whether direct or compounded transient terms will be detected
		% at serialisation time as expected:
		%
		%{ test_pid, { hello, self() } }
		%{ test_ref, [ foo, erlang:make_ref() ] }
		%{ test_port, erlang:open_port( { spawn, "echo Hello!" }, _Opts=[] ) }

								] ).



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	trace_utils:info_fmt( "Deleting ~ts, of PID ~w.",
		[ class_Describable:get_maybe_description( State ), self() ] ),

	State.
	% To test destructor checking use instead:
	%an_unexpected_final_state.



% Method implementations.


% @doc Sets correctly the age of this Mammal (not like faulty implementation of
% the Creature mother class).
%
% Overridden from Creature, useful to show the use of executeOneway.
% Note: used to test WOOPER management of error conditions.
%
-spec setAge( wooper:state(), age() ) -> oneway_return().
setAge( State, NewAge ) ->
	%throw( exception_throw_test_from_oneway ),
	%exit( exception_exit_test_from_oneway ),
	wooper:return_state( setAttribute( State, age, NewAge ) ).



% @doc All reptiles are cold-blooded.
%
% Note: used to test WOOPER management of error conditions.
%
-spec isHotBlooded( wooper:state() ) -> const_request_return( boolean() ).
isHotBlooded( State ) ->
	%throw( exception_throw_test_from_request ),
	%exit( exception_exit_test_from_request ),
	wooper:const_return_result( false ).


% @doc All reptiles can moult.
-spec canMoult( wooper:state() ) -> const_request_return( boolean() ).
canMoult( State ) ->
	wooper:const_return_result( true ).



% Serialisable interface.


% @doc Triggered just before serialisation.
-spec onPreSerialisation( wooper:state(), user_data() ) ->
					const_request_return( { wooper:state(), user_data() } ).
onPreSerialisation( State, UserData ) ->

	trace_utils:info_fmt( "Pre-serialising a reptile! (user data: ~p);~n ~ts",
						  [ UserData, wooper:state_to_string( State ) ] ),

	wooper:const_return_result( { State, UserData } ).



% @doc Triggered just after serialisation.
-spec onPreSerialisation( wooper:state(), serialisation(), user_data() ) ->
			const_request_return( { serialisation(), user_data() } ).
onPreSerialisation( State, SerialisationTerm, UserData ) ->

	trace_utils:info_fmt( "Post-serialising a reptile! "
		"(serialisation term: ~p, user data: ~p);~n ~ts",
		[ SerialisationTerm, UserData, wooper:state_to_string( State ) ] ),

	wooper:const_return_result( { SerialisationTerm, UserData } ).



% No relevant onPreDeserialisation/2 request can exist, as no instance is
% available at this point.


% @doc Triggered just after serialisation.
-spec onPostDeserialisation( wooper:state(), user_data() ) ->
			const_request_return( user_data() ).
onPostDeserialisation( State, UserData ) ->

	trace_utils:info_fmt( "Post-deserialising a reptile! (user data: ~p);"
		"~n ~ts", [ UserData, wooper:state_to_string( State ) ] ),

	wooper:const_return_result( UserData ).
