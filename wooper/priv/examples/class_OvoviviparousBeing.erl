% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


-module(class_OvoviviparousBeing).


-define( class_description,
		 "Class modelling any kind of ovoviviparous being." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).


-define( class_attributes, [ { eggs_count, "A number of eggs" } ] ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").



% Constructs a new Ovoviviparous being (parameter-less constructor).
-spec construct( wooper:state() ) -> wooper:state() .
construct( State ) ->
	% In order to test the crash of a constructor: non_existing:crash(),
	setAttribute( State, eggs_count, 0 ).



% This useless destructor overriding was made to silence Dialyzer (which is not
% able to determine that this function will never be called, as WOOPER performs
% the appropriate test is made beforehand):
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	% In order to test the crash of a destructor: non_existing:crash(),
	State.


% Method implementations.


% Let's say an average means something here:
% (this ought to be a static method, as it does not depend on a state)
-spec getMeanEggsCount( wooper:state() ) -> const_request_return( egg_count() ).
getMeanEggsCount( State ) ->
	wooper:const_return_result( 1000 ).


% Returns the number of eggs this ovoviviparous laid:
-spec getEggsLaidCount( wooper:state() ) -> const_request_return( egg_count() ).
getEggsLaidCount( State ) ->
	wooper:const_return_result( ?getAttr(eggs_count) ).


% Increases the number of eggs that this ovoviviparous being laid:
-spec layEggs( wooper:state(), egg_count() ) -> oneway_return().
layEggs( State, NumberOfNewEggs ) ->

	NewEggCount = ?getAttr(eggs_count) + NumberOfNewEggs,

	wooper:return_state( setAttribute( State, eggs_count, NewEggCount ) ).
