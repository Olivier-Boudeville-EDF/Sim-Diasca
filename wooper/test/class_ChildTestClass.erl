% Copyright (C) 2007-2021 Olivier Boudeville
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
%
-module(class_ChildTestClass).


-define( class_description, "Test class, defined notably in relationship with "
							"inheritance (see class_BaseTestClass)." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_BaseTestClass ] ).

-define( class_attributes, [ name, age, { gender, "Some description" } ] ).


% Non-method exported functions:
-export([ example_fun/0, toString/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-type name() :: text_utils:ustring().
-type gender() :: maybe( 'male' | 'female' ).
-type age() :: non_neg_integer().


% Shorthands:
-type ustring() :: text_utils:ustring().


% Constructs a new child test instance.
-spec construct( wooper:state(), age(), gender() ) -> wooper:state().
construct( State, Age, Gender ) ->
	% No mother class.
	setAttributes( State,
				   [ { name, "Bob" }, { age, Age }, { gender, Gender } ] ).


% Allows to test also the automatic destructor generation:
%-spec destruct( wooper:state() ) -> wooper:state().
%destruct( State ) ->
%	State.



% Method implementations.


% Returns the name of this creature.
-spec getName( wooper:state() ) -> const_request_return( name() ).
getName( State ) ->
	wooper:const_return_result( ?getAttr(name) ).


% Sets the name of this creature.
-spec setName( wooper:state(), name() ) -> oneway_return().
setName( State, NewName ) ->
	% Mother implementation chosen faulty to check override:
	wooper:return_state( setAttribute( State, name, NewName ) ).



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
-spec getArbitraryNumber( wooper:state() ) -> const_request_return( number() ).
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

	io:format( "Testing executeOneway.~n" ),

	% Note: the version of setAge called in the context of a Creature sets in on
	% purpose to a fixed value (36), regardless of the specified age, whereas
	% the Mammal version of setAge behaves as expected:
	%
	NewState = executeOneway( State, setAge, NewAge ),

	% Use this instead to test error management:
	%NewState = executeOneway(test_not_a_state,setAge,NewAge),
	%NewState = executeOneway(State,42,NewAge),

	% NewAge is expected to be 347:
	NewAge = getAttribute( NewState, age ),

	io:format( "Testing executeRequest.~n" ),
	% 15 from Mammal child classes, not 10 from here:

	{ OtherState, 15 } = executeRequest( NewState, getArbitraryNumber ,[] ),

	%{ OtherState, 15 } = executeRequest( test_not_a_state, getArbitraryNumber,
	% [] ),

	%{ OtherState, 15 } = executeRequest( NewState, 43, [] ),

	io:format( "Direct self-invocation success.~n" ),

	wooper:return_state( OtherState ).



% Allows to test that calling an attribute macro with a state parameter returned
% by a function will trigger that function only once.
%
% Indeed a faulty implementation, due to a macro pitfall, used to make a
% statement like 'setAttribute( f(State), attr, value )' call f/1 twice.
%
% The returned value of the setAttribute call was correct, but any side-effect
% triggered by f (sending a message, writing a trace, etc.) happened twice.
%
-spec testSingleExecution( wooper:state() ) -> oneway_return().
testSingleExecution( State ) ->
	wooper:return_state( setAttribute( side_effect_function( State ),
		age, 10 ) ).


test_of_const_req_as( State ) ->

	% Not 50!
	17 = executeConstRequestAs( State, class_BaseTestClass, someRequest, 10 ),

	wooper:const_return().



-spec side_effect_function( wooper:state() ) -> wooper:state().
side_effect_function( State ) ->
	io:format( "~n### This message must not be displayed more than once.~n" ),
	State.


-spec test_multi_clause_const_request( wooper:state(), integer(), integer() ) ->
											const_request_return( integer() ).
test_multi_clause_const_request( State, _X=1, _Y ) ->
	wooper:const_return_result( 1 );

test_multi_clause_const_request( State, _X, _Y ) ->
	wooper:const_return_result( 2 ).



-spec test_multi_clause_non_const_request( wooper:state(), integer(),
								   integer() ) -> request_return( integer() ).
test_multi_clause_non_const_request( State, _X=1, _Y ) ->
	wooper:const_return_result( 1 );

test_multi_clause_non_const_request( State, X=2, _Y ) ->
	NewState = setAttribute( State, age, 1 ),
	wooper:return_state_result( NewState, X );

test_multi_clause_non_const_request( State, _X, _Y ) ->
	wooper:const_return_result( 3 ).



-spec test_multi_clause_const_oneway( wooper:state(), integer(),
									  integer() ) -> const_oneway_return().
test_multi_clause_const_oneway( State, _X=1, _Y ) ->
	% Do some side-effect.
	wooper:const_return();

test_multi_clause_const_oneway( State, _X, _Y ) ->
	wooper:const_return().



-spec test_multi_clause_non_const_oneway( wooper:state(), integer(),
										  integer() ) -> oneway_return().
test_multi_clause_non_const_oneway( State, _X=1, _Y ) ->
	wooper:const_return();

test_multi_clause_non_const_oneway( State, X=2, _Y ) ->
	NewState = setAttribute( State, age, X ),
	wooper:return_state( NewState );

test_multi_clause_non_const_oneway( State, _X, _Y ) ->
	wooper:const_return().



% Overridden request.
-spec someRequest( wooper:state(), integer() ) ->
						const_request_return( integer() ).
someRequest( State, _Arg ) ->
		wooper:const_return_result( 50 ).



% Returns a value established in a static context.
-spec get_static_info( integer(), integer() ) -> static_return( integer() ).
get_static_info( A, B ) ->

	trace_utils:info( "get_static_info/2 called" ),

	wooper:return_static( A + B + 10 ).



% Helper function.


% Just to show it can exist:
-spec example_fun() -> 'ok'.
example_fun() ->
	ok.


% This looks like a method, but it is not (returning only a string):
-spec toString( wooper:state() ) -> ustring().
toString( State ) ->
	table:to_string( State#state_holder.attribute_table ).
