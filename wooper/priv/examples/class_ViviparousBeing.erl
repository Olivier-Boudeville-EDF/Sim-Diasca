% Copyright (C) 2007-2026 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: 2007.

-module(class_ViviparousBeing).

-moduledoc """
Class modelling any kind of **viviparous being**.
""".


-define( class_description, "Class modelling any kind of viviparous being." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).

-define( class_attributes, [

    { birth_given_count, non_neg_integer(), "Birth count" } ] ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").



-doc "Constructs a viviparous being (parameter-less constructor).".
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->
    setAttribute( State, birth_given_count, 0 ).



% Method implementations.


-doc """
Let's say an average means something here.

(request; actually this ought to be a static method, as it does not depend on a
state here)
""".
-spec getMeanChildrenCount( wooper:state() ) ->
                                const_request_return( children_count() ).
getMeanChildrenCount( State ) ->
    wooper:const_return_result( 4 ).



-doc "Returns the number of times this viviparous being gave birth.".
-spec getBirthGivenCount( wooper:state() ) ->
                                const_request_return( children_count() ).
getBirthGivenCount( State ) ->
    wooper:const_return_result( getAttribute( State, birth_given_count ) ).



-doc "Increases the number of times this viviparous being gave birth.".
-spec giveBirth( wooper:state(), children_count() ) -> oneway_return().
giveBirth( State, NumberOfNewChildren ) ->

    NewChildrenCount = ?getAttr(birth_given_count) + NumberOfNewChildren,

    BirthState = setAttribute( State, birth_given_count, NewChildrenCount ),

    wooper:return_state( BirthState ).
