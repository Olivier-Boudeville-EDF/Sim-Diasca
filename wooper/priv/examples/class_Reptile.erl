% Copyright (C) 2003-2022 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% @doc Class modelling any kind of <b>reptile</b>.
%
% This class allows to test an exploratory resilience support.
%
-module(class_Reptile).


-define( class_description, "Class modelling any kind of reptile." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Creature ] ).


-define( class_attributes, [] ).

% With this class, we will test serialisation hooks:
-define( wooper_serialisation_hooks, ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").


% @doc Triggered just before serialisation.
-spec pre_serialise_hook( wooper:state() ) -> wooper:state().
pre_serialise_hook( State ) ->
	trace_utils:info( "Pre-serialising a reptile!" ),
	State.



% @doc Triggered just after serialisation.
%
% (using WOOPER default hook implementation augmented of an trace_utils:info)
%
-spec post_serialise_hook( classname(),
						   wooper_serialisation:term_serialisation(),
						   wooper:state() ) -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	trace_utils:info( "Post-serialising a reptile!" ),
	{ Classname, Entries }.



% @doc Triggered just before deserialisation.
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
							wooper_serialisation:term_serialisation().
pre_deserialise_hook( _SerialisedEntries={ _Classname, Entries }, _UserData ) ->
	trace_utils:info( "Pre-deserialising a reptile!" ),
	Entries.



% @doc Triggered just after deserialisation.
-spec post_deserialise_hook( wooper:state() ) -> wooper:state().
post_deserialise_hook( State ) ->
	trace_utils:info( "Post-deserialising a reptile!" ),
	State.



% @doc Constructs a reptile.
-spec construct( wooper:state(), age(), gender() ) -> wooper:state().
construct( State, Age, Gender ) ->
	class_Creature:construct( State, Age, Gender ).
	% To test constructor checking:
	%an_unexpected_initial_state.


% @doc Overridden destructor
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	trace_utils:info( "Deleting a Reptile." ),
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
