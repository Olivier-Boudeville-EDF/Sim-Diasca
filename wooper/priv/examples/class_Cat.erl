% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Cat-based example. Those are the ones that work best.
%
% Guaranteed to be implemented by a cat.
%
-module(class_Cat).


-define( class_description,
		 "Class modelling any kind of cat, and there are many." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Mammal, class_ViviparousBeing ] ).


-define( class_attributes, [
   { whisker_color, whisker_color(), none, "50 shades of whiskers" } ] ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").


% Shorthands:
-type ustring() :: text_utils:ustring().


% Constructs a new Cat.
-spec construct( wooper:state(), age(), gender(), fur_color(),
				 whisker_color() ) -> wooper:state().
construct( State, Age, Gender, FurColor, WhiskerColor ) ->

	% First the direct mother classes:
	MammalState = class_Mammal:construct( State, Age, Gender, FurColor ),
	ViviparousMammalState = class_ViviparousBeing:construct( MammalState ),

	% Then the class-specific attributes:
	setAttribute( ViviparousMammalState, whisker_color, WhiskerColor ).



-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	io:format( "Deleting cat ~w! (overridden destructor)~n", [ self() ] ),

	% To test:
	% basic_utils:crash(),

	State.



% No guarantee on biological fidelity:
-spec getTeatCount( wooper:state() ) -> const_request_return( teat_count() ).
getTeatCount( State ) ->
	wooper:const_return_result( 6 ).


% Cats are supposed carnivorous though:
-spec canEat( wooper:state(), food() ) -> const_request_return( boolean() ).
canEat( State, soup ) ->
	wooper:const_return_result( true );

canEat( _State, chocolate ) ->
	throw( { harmful_food_detected, chocolate } );

canEat( State, croquette ) ->
	wooper:const_return_result( true );

canEat( State, meat ) ->
	wooper:const_return_result( true );

canEat( State, _OtherFood ) ->
	wooper:const_return_result( false ).



% Returns the whisker color of this cat.
-spec getWhiskerColor( wooper:state() ) -> const_request_return( color() ).
getWhiskerColor( State )->

	io:format( "getWhiskerColor/1 request called by ~w.~n", [ ?getSender() ] ),

	wooper:const_return_result( ?getAttr(whisker_color) ).



% Requests this cat to terminate, based on specified halting procedure.
-spec terminate( wooper:state(), 'crash' ) -> const_oneway_return().
terminate( State, crash ) ->
	basic_utils:crash(),
	wooper:const_return().



-spec toString( wooper:state() ) -> const_request_return( ustring() ).
toString( State ) ->

	% Would be available only on debug mode:
	%Description = wooper:instance_to_string( State ),

	Description = text_utils:format( "cat instance with whiskers of color ~p.",
									 [ ?getAttr(whisker_color) ] ),

	wooper:const_return_result( Description ).



% Static section.


% Returns the mean life expectancy of a cat, in years.
-spec get_mean_life_expectancy() -> static_return( age() ).
get_mean_life_expectancy() ->
	wooper:return_static( 18 ).
