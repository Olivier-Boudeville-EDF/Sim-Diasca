% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
-module(class_Creature).


-define( class_description, "Top-level class modelling any kind of creature." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).


-define( class_attributes, [ { age, "Age of this creature" },
							 { gender, union( foo:bar( float() ), 'baz' ),
							   "Incorrectly-typed gender" } ] ).

% Non-method exported functions:
-export([ example_fun/0, toString/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").


% Shorthands:
-type ustring() :: text_utils:ustring().


% Constructs a new Creature.
-spec construct( wooper:state(), age(), gender() ) -> wooper:state().
construct( State, Age, Gender ) ->
	% No mother class.
	setAttributes( State, [ { age, Age }, { gender, Gender } ] ).



% Method implementations.


% Returns the age of this creature.
-spec getAge( wooper:state() ) -> const_request_return( age() ).
getAge( State ) ->
	wooper:const_return_result( ?getAttr(age) ).



% Sets the age of this creature.
-spec setAge( wooper:state(), age() ) -> oneway_return().
setAge( State, _NewAge ) ->
	% Mother implementation chosen faulty to check override:
	wooper:return_state( setAttribute( State, age, 36 ) ).



% Increments the age of this creature.
-spec declareBirthday( wooper:state() ) -> oneway_return().
declareBirthday( State ) ->
	wooper:return_state(
		setAttribute( State, age, ?getAttr(age)+1 ) ).



% Returns the gender of this creature.
-spec getGender( wooper:state() ) -> const_request_return( gender() ).
getGender( State ) ->
	wooper:const_return_result( ?getAttr(gender) ).



% Returns a class-specific arbitrary number.
getArbitraryNumber( State ) ->
	wooper:const_return_result( 10 ).



% Tests direct (synchronous) self-invocation of methods (oneway).
%
% To be called only from a Mammal instance, as there is an hardcoded
% pattern-matching that should work only for a Mammal.
%
% Must not be called from the Creature test, otherwise will fail.
%
-spec testDirectMethodExecution( wooper:state(), age() ) -> oneway_return().
testDirectMethodExecution( State, NewAge ) ->

	trace_utils:info( "Testing executeOneway." ),

	% Note: the version of setAge called in the context of a Creature sets in on
	% purpose to a fixed value (36), regardless of the specified age, whereas
	% the Mammal version of setAge behaves as expected:
	NewState = executeOneway( State, setAge, NewAge ),

	% Use this instead to test error management:
	%NewState = executeOneway(test_not_a_state,setAge,NewAge),
	%NewState = executeOneway(State,42,NewAge),

	% NewAge is expected to be 347:
	NewAge = getAttribute( NewState, age ),

	trace_utils:info( "Testing executeRequest." ),
	% 15 from Mammal child classes, not 10 from here:

	{ OtherState, 15 } = executeRequest( NewState, getArbitraryNumber ,[] ),

	%{ OtherState, 15 } = executeRequest( test_not_a_state, getArbitraryNumber,
	% [] ),

	%{ OtherState, 15 } = executeRequest( NewState, 43, [] ),

	trace_utils:info( "Direct self-invocation success." ),

	wooper:return_state( OtherState ).



% Allows to test that calling an attribute macro with a state parameter returned
% by a function will trigger that function only once.
%
% Indeed a faulty implementation, due to a macro pitfall, used to make a
% statement like 'setAttribute( f(State), attr, value )' call f/1 twice.
%
% The returned value of the setAttribute call was correct, but any side-effect
% triggered by f/1 (sending a message, writing a trace, etc.) happened twice.
%
-spec testSingleExecution( wooper:state() ) -> oneway_return().
testSingleExecution( State ) ->
	wooper:return_state(
	  setAttribute( side_effect_function( State ), age, 10 ) ).



-spec side_effect_function( wooper:state() ) -> wooper:state().
side_effect_function( State ) ->
	trace_utils:warning(
	   "### This message must be displayed exactly once." ),
	State.





% Helper function.


% Just to show it can exist:
-spec example_fun() -> 'ok'.
example_fun() ->
	ok.


% This looks like a method, but it is not (returning only a string):
-spec toString( wooper:state() ) -> ustring().
toString( State ) ->
	table:to_string( State#state_holder.attribute_table ).
