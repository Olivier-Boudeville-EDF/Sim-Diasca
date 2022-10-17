% Copyright (C) 2007-2022 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: 2007.


% @doc Class modelling any kind of <b>ovoviviparous being</b>.
-module(class_OvoviviparousBeing).


-define( class_description,
		 "Class modelling any kind of ovoviviparous being." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Creature ] ).


-define( class_attributes, [ { eggs_count, "A number of eggs" } ] ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").



% @doc Constructs an ovoviviparous being.
-spec construct( wooper:state(), age(), gender() ) -> wooper:state() .
construct( State, Age, Gender ) ->

	% In order to test the crash of a constructor:
	%non_existing:crash(),

	CreatureState = class_Creature:construct( State, Age, Gender ),

	setAttribute( CreatureState, eggs_count, 0 ).



% @doc This is a useless destructor that may not have been defined.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% In order to test the crash of a destructor:
	%non_existing:crash(),

	State.


% Method implementations.


% @doc Let's say an average means something here.
%
% (this ought to be a static method, as it does not depend on a state)
%
-spec getMeanEggsCount( wooper:state() ) -> const_request_return( egg_count() ).
getMeanEggsCount( State ) ->
	wooper:const_return_result( 1000 ).


% @doc Returns the number of eggs that this ovoviviparous being laid.
-spec getEggsLaidCount( wooper:state() ) -> const_request_return( egg_count() ).
getEggsLaidCount( State ) ->
	wooper:const_return_result( ?getAttr(eggs_count) ).


% @doc Increases the number of eggs that this ovoviviparous being already laid.
-spec layEggs( wooper:state(), egg_count() ) -> oneway_return().
layEggs( State, NumberOfNewEggs ) ->
	wooper:return_state( addToAttribute( State, eggs_count, NumberOfNewEggs ) ).
