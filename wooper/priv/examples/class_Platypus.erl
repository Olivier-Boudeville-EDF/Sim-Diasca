% Copyright (C) 2007-2026 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: 2007.

-module(class_Platypus).

-moduledoc """
Class modelling any kind of **platypus**.
""".


-define( class_description, "Class modelling any kind of platypus." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Mammal, class_OvoviviparousBeing ] ).


-define( class_attributes, [

    { nozzle_color, "the color of the nozzle" },

    alternate_names,

    { cat_pid, pid(), "PID of a cat" } ] ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").



-doc "Constructs a platypus.".
-spec construct( wooper:state(), age(), gender(), fur_color(),
                 nozzle_color() ) -> wooper:state().
construct( State, Age, Gender, FurColor, NozzleColor ) ->

    % First the direct mother classes:
    MammalState = class_Mammal:construct( State, Age, Gender, FurColor ),

    % To test onWOOPERExitReceived/3 (comment to check that the test fails):
    process_flag( trap_exit, true ),

    % Note that age and gender here will thus be initialised twice:
    OvoviviparousMammalState =
        class_OvoviviparousBeing:construct( MammalState, Age, Gender ),

    io:format( "Synchronous time-out is ~ts.~n",
               [ time_utils:duration_to_string( ?synchronous_time_out ) ] ),

    % Then the class-specific attributes:
    setAttributes( OvoviviparousMammalState, [
        { nozzle_color, NozzleColor },
        { alternate_names, [ hector, edgar, roger, sean ] },
        { cat_pid, undefined } ] ).



-spec getMeanEggsCount( wooper:state() ) -> const_request_return( egg_count() ).
getMeanEggsCount( State ) ->
    wooper:const_return_result( 2 ).



-doc """
Returns the number of teats a platypus has.
%
It is a mammal, though!
""".
-spec getTeatCount( wooper:state() ) -> const_request_return( teat_count() ).
getTeatCount( State ) ->
    wooper:const_return_result( 0 ).



-doc """
Tells whether this platypus can eat specified food.

Platypuses are supposed carnivorous though:
""".
-spec canEat( wooper:state(), food() ) -> const_request_return( boolean() ).
canEat( State, leaf ) ->
    wooper:const_return_result( true );

canEat( State,chocolate ) ->
    wooper:const_return_result( true );

canEat( State,weed ) ->
    wooper:const_return_result( true );

canEat( State,fish ) ->
    wooper:const_return_result( true );

canEat( State, _OtherFood ) ->
    wooper:const_return_result( false ).



-doc "Returns the color of the nozzle of this platypus.".
-spec getNozzleColor( wooper:state() ) ->
                            const_request_return( nozzle_color() ).
getNozzleColor( State )->

    % If needing to test the crash of a request:
    %A=1,
    %B=2,
    %A=B,

    wooper:const_return_result( getAttribute( State, nozzle_color ) ).



-doc "Returns the list of alternate names for this platypus.".
-spec getAlternateNames( wooper:state() ) -> const_request_return( [ atom() ] ).
getAlternateNames( State ) ->
    wooper:const_return_result( ?getAttr(alternate_names) ).



-doc "Returns the first alternate name for this platypus and forget it.".
-spec popFirstAlternateName( wooper:state() ) -> request_return( atom() ).
popFirstAlternateName( State ) ->
    { NewState, Name } = popFromAttribute( State, alternate_names ),
    wooper:return_state_result( NewState, Name ).



-doc "Allows to test the creation and deletion of other WOOPER instances.".
-spec testCreationDeletion( wooper:state() ) -> oneway_return().
testCreationDeletion( State ) ->

    % Initially do-nothing:
    FirstState =
        wooper:delete_synchronously_any_instance_referenced_in( [], State ),

    CatPid = class_Cat:synchronous_new_link( _Age=1, _Gender=male,
        _FurColor=pink, _WhiskerColor=black ),

    io:format( "Cat ~p just created from platypus.~n", [ CatPid ] ),

    CatState = setAttribute( FirstState, cat_pid, CatPid ),

    % Comment in order to test normal exits (should not trigger the default or
    % user-defined EXIT handler):
    %
    CatPid ! { terminate, this_is_an_intentional_crash },

    io:format( "Deleting cat ~p created from platypus.~n", [ CatPid ] ),

    trace_utils:warning( "Deletion time-out and crash triggered on purpose "
        "(see the very next intentional error report, and wait a bit; note "
        "though that the 5-second time-out in debug mode is a 30-minute one in "
        "normal mode)." ),

    DeleteState = wooper:delete_synchronously_any_instance_referenced_in(
        cat_pid, CatState ),

    undefined = getAttribute( DeleteState, cat_pid ),

    trace_utils:warning( "Reminder: these deletion time-out and crash are "
        "triggered on purpose (see above); note though that the 5-second "
        "time-out in debug mode is a 30-minute one in normal mode)." ),

    wooper:return_state( DeleteState ).



-doc """
Callback triggered, as we trap exits, whenever a linked process stops (here, the
created cat instance).
""".
-spec onWOOPERExitReceived( wooper:state(), pid(),
                        basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, Pid, ExitType ) ->

    % Typically: "Received exit message '{{nocatch,
    %   {wooper_oneway_failed,<0.44.0>,class_Cat,
    %      terminate,2,
    %      [crash],
    %      badarith}},
    % [...]"

    trace_utils:debug_fmt( "Received exit message '~p' from ~w.",
                           [ ExitType, Pid ] ),

    wooper:const_return().


-spec test_static() -> static_return( text_utils:ustring() ).
test_static() ->
    wooper:return_static( "Hello!" ).
