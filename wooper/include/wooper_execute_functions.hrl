% Copyright (C) 2007-2026 Olivier Boudeville
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
% Creation date: 2007.


% Modular WOOPER header gathering all execute{Request,Oneway}* primitives that
% shall be (explicitly) used when having to call a method of this class from
% this class (i.e. when implementing methods calling other methods).


% Implementation notes:
%
% The initial request_sender field shall be preserved; for example, the body of
% a request might include a executeOneway call; if nothing was done, the oneway
% would detect a request/oneway mismatch; if request_sender was set to
% 'undefined', then the request would have lost the memory of its caller. Hence
% that field must be saved and restored in each execute* call, to allow nesting.
%
% Note: the execute* functions branch to a stripped-down version of
% wooper_main_loop.


% Implementation section (debug mode managed in the called wooper_* helpers)



% Section for requests.


-doc """
Executes the specified parameter-less request of the current instance, calling
implicitly any overridden version of the method.

Allows to call synchronously from the code of a given class its actual
overridden methods (requests, here), including from child classes.

Example: If in some start method of an EngineVehicle class one wants to call the
(possibly overridden by, say, a class Car) `startEngine/1` request, then
`executeRequest/2` should be used: `MyVehicle ! {startEngine, ...` would not be
synchronous, `startEngine(State)` would call `EngineVehicle:startEngine/1`
instead of `Car:startEngine/1` when called from a Car instance, and of course
EngineVehicle should know nothing from its Car child class.

Returns an updated state and a result.
""".
-spec executeRequest( wooper:state(), request_name() ) ->
                            { wooper:state(), method_internal_result() }.
% Legit case:
executeRequest( State, RequestAtom ) when is_record( State, state_holder )
                                          andalso is_atom( RequestAtom ) ->

    %trace_utils:debug_fmt( "executeRequest/2: executing ~ts() from ~ts.",
    %   [ RequestAtom, State#state_holder.actual_class ] ),

    wooper_handle_local_request_execution( RequestAtom, State,
                                           _ArgumentList=[] );

% Invalid request name:
executeRequest( State, RequestAtomError )
                                    when is_record( State, state_holder ) ->

    wooper:log_error( " when executing request locally, its name shall "
        "be an atom, not:~n ~p", [ RequestAtomError ], State ),

    throw( { wooper_invalid_request_call,
             { invalid_request_name, RequestAtomError } } );

% Invalid state:
executeRequest( StateError, SomeRequestName ) ->

    wooper:log_error( " when executing request ~ts/1 locally, "
        "first parameter should be a state, not:~n ~p",
        [ wooper:method_name_to_string( SomeRequestName ), StateError ],
        ?MODULE ),

    throw( { wooper_invalid_request_call,
             { SomeRequestName, { invalid_state, StateError } } } ).



-doc """
Executes the specified parameter-less const request of the current instance,
calling implicitly any overridden version of the method.

Allows to call synchronously from the code of a given class its actual
overridden methods (requests, here), including from child classes.

See `executeRequest/2`.

Returns only a result.
""".
-spec executeConstRequest( wooper:state(), request_name() ) ->
                                method_internal_result().
executeConstRequest( State, RequestAtom ) ->

    % Checks made by the callee; actual constness not checked yet shall derive
    % from the transformed, corresponding method terminator:
    %
    { _State, Result } = executeRequest( State, RequestAtom ),
    Result.



-doc """
Executes the specified request of the current instance, calling implicitly any
overridden version of the method.

Allows to call synchronously from the code of a given class its actual
overridden methods (requests, here), including from child classes.

See `executeRequest/2`.

Returns an updated state and a result.
""".
-spec executeRequest( wooper:state(), request_name(), method_arguments() ) ->
                            { wooper:state(), method_internal_result() }.
% Most usual legit case:
executeRequest( State, RequestAtom, ArgumentList ) when
        is_record( State, state_holder ) andalso is_atom( RequestAtom )
        andalso is_list( ArgumentList ) ->

    %trace_utils:debug_fmt( "executeRequest/3 with list: executing ~ts(~w) "
    %   "from ~ts.",
    %   [ RequestAtom, ArgumentList, State#state_holder.actual_class ] ),

    wooper_handle_local_request_execution( RequestAtom, State, ArgumentList );


% Here having a single, standalone argument, to be promoted to list:
executeRequest( State, RequestAtom, StandaloneArgument ) when
        is_record( State, state_holder ) andalso is_atom( RequestAtom )->

    %trace_utils:debug_fmt( "executeRequest/3 with standalone argument: "
    %   "executing ~ts(~w) from ~ts.",
    %   [ RequestAtom, StandaloneArgument, State#state_holder.actual_class ] ),

    wooper_handle_local_request_execution( RequestAtom, State,
        _ArgumentList=[ StandaloneArgument ] );


% Invalid request name:
executeRequest( State, RequestAtomError, _ArgMaybeList )
                            when is_record( State, state_holder ) ->

    wooper:log_error( " when executing request locally, its name shall "
        "be an atom, not:~n ~p", [ RequestAtomError ], State ),

    throw( { wooper_invalid_request_call,
             { invalid_request_name, RequestAtomError } } );


% At least an invalid state:
executeRequest( StateError, RequestAtom, ArgMaybeList ) ->

    wooper:log_error( " when executing request ~ts locally, "
        "first parameter should be a state, not:~n ~p",
        [ wooper:method_call_to_string( RequestAtom, ArgMaybeList ),
          StateError ], ?MODULE ),

    throw( { wooper_invalid_request_call,
             { RequestAtom, { invalid_state, StateError } } } ).



-doc """
Executes the specified const request of the current instance, calling implicitly
any overridden version of the method.

Allows to call synchronously from the code of a given class its actual
overridden methods (const requests, here), including from child classes.

Ssee `executeRequest/2`.

Returns only a result.
""".
-spec executeConstRequest( wooper:state(), request_name(),
                           method_arguments() ) -> method_internal_result().
executeConstRequest( State, RequestAtom, ArgumentMaybeList ) ->

    % Checks made by the callee; actual constness not checked yet shall derive
    % from the transformed, corresponding method terminator:
    %
    { _State, Result } =
        executeRequest( State, RequestAtom, ArgumentMaybeList ),

    Result.



-doc """
Executes the version of the specified parameter-less request, as it has been
defined by the specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeRequest/2`.

Returns an updated state and a result.
""".
-spec executeRequestAs( classname(), wooper:state(), request_name() ) ->
                                { wooper:state(), method_internal_result() }.
% Legit case:
executeRequestAs( ParentClassname, State, RequestAtom )
        when is_atom( ParentClassname ) andalso is_record( State, state_holder )
             andalso is_atom( RequestAtom ) ->

    %trace_utils:debug_fmt( "executeRequestAs/3: executing ~ts() from ~ts "
    %   "as parent class ~ts.",
    %   [ RequestAtom, State#state_holder.actual_class, ParentClassname ]),

    wooper_handle_local_request_execution_as( RequestAtom, State,
        _ArgumentList=[], ParentClassname );

% Invalid request name:
executeRequestAs( ParentClassname, State, RequestAtomError )
        when is_atom( ParentClassname )
             andalso is_record( State, state_holder ) ->

    wooper:log_error( " when executing request locally as ~ts, the request "
        "name shall be an atom, not:~n ~p",
        [ ParentClassname, RequestAtomError ], State ),

    throw( { wooper_invalid_request_call,
             { invalid_request_name, RequestAtomError } } );


% Invalid state:
executeRequestAs( ParentClassname, StateError, SomeRequestName )
        when is_atom( ParentClassname ) ->

    wooper:log_error( " when executing request ~ts/1 locally as ~ts, "
        "second parameter should be a state, not:~n ~p",
        [ wooper:method_name_to_string( SomeRequestName ), ParentClassname,
          StateError ], ?MODULE ),

    throw( { wooper_invalid_request_call,
             { SomeRequestName, { invalid_state, StateError } } } );


% Invalid parent classname:
executeRequestAs( ParentClassnameError, _State, SomeRequestName ) ->

    wooper:log_error( " when executing request ~ts/1 locally, "
        "parent classname shall be an atom, not:~n ~p",
        [ wooper:method_name_to_string( SomeRequestName ),
            ParentClassnameError ], ?MODULE ),

    throw( { wooper_invalid_request_call, { SomeRequestName,
            { invalid_parent_classname, ParentClassnameError } } } ).




-doc """
Executes the version of the specified parameter-less const request, as it has
been defined by the specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeRequest/2`.

Returns only a result.
""".
-spec executeConstRequestAs( classname(), wooper:state(), request_name() ) ->
                                method_internal_result().
executeConstRequestAs( ParentClassname, State, RequestAtom ) ->

    % Checks made by the callee; actual constness not checked yet shall derive
    % from the transformed, corresponding method terminator:
    %
    { _State, Result } =
        executeRequestAs( ParentClassname, State, RequestAtom ),

    Result.



-doc """
Executes the version of the specified request, as it has been defined by the
specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeRequest/2`.

Returns an updated state and a result.
""".
-spec executeRequestAs( classname(), wooper:state(), request_name(),
        method_arguments() ) -> { wooper:state(), method_internal_result() }.
% Most usual legit case:
executeRequestAs( ParentClassname, State, RequestAtom, ArgumentList ) when
        is_atom( ParentClassname ) andalso is_record( State, state_holder )
        andalso is_atom( RequestAtom ) andalso is_list( ArgumentList ) ->

    %trace_utils:debug_fmt( "executeRequestAs/4 with list: executing ~ts(~w) "
    %  "from ~ts with ~ts.", [ RequestAtom, ArgumentList,
    % State#state_holder.actual_class, Classname ] ),

    wooper_handle_local_request_execution_as( RequestAtom, State,
                                              ArgumentList, ParentClassname );


% Here having a single, standalone argument, to be promoted to list:
executeRequestAs( ParentClassname, State, RequestAtom,
                  StandaloneArgument ) when is_atom( ParentClassname )
        andalso is_record( State, state_holder )
        andalso is_atom( RequestAtom ) ->

    %trace_utils:debug_fmt( "executeRequestAs/3 with standalone argument: "
    %   "executing ~ts(~w) from ~ts with ~ts.",
    %   [ RequestAtom, StandaloneArgument, State#state_holder.actual_class,
    % Classname ] ),

    wooper_handle_local_request_execution_as( RequestAtom, State,
        _ArgumentList=[ StandaloneArgument ], ParentClassname );


% Invalid request name:
executeRequestAs( ParentClassname, State, RequestAtomError, _ArgMaybeList )
        when is_atom( ParentClassname )
             andalso is_record( State, state_holder ) ->

    wooper:log_error( " when executing request locally as ~ts, the request "
        "name shall be an atom, not:~n ~p",
        [ ParentClassname, RequestAtomError ], State ),

    throw( { wooper_invalid_request_call,
             { invalid_request_name, RequestAtomError } } );


% Invalid state:
executeRequestAs( ParentClassname, StateError, RequestAtom, ArgMaybeList )
        when is_atom( ParentClassname ) ->

    wooper:log_error( " when executing request ~ts locally as ~ts, "
        "second parameter should be a state, not:~n ~p",
        [ wooper:method_call_to_string( RequestAtom, ArgMaybeList ),
          ParentClassname, StateError ], ?MODULE ),

    throw( { wooper_invalid_request_call,
             { RequestAtom, { invalid_state, StateError } } } );


% Invalid parent classname:
executeRequestAs( ParentClassnameError, _State, RequestAtom, ArgMaybeList ) ->

    wooper:log_error( " when executing request ~ts locally, "
        "parent classname shall be an atom, not:~n ~p",
        [ wooper:method_call_to_string( RequestAtom, ArgMaybeList ),
          ParentClassnameError ], ?MODULE ),

    throw( { wooper_invalid_request_call, { RequestAtom,
            { invalid_parent_classname, ParentClassnameError } } } ).



-doc """
Executes the version of the specified const request, as it has been defined by
the specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeRequest/2`.

Returns only a result.
""".
-spec executeConstRequestAs( classname(), wooper:state(), request_name(),
            method_arguments() ) -> method_internal_result().
executeConstRequestAs( ParentClassname, State, RequestAtom,
                       MaybeArgumentList ) ->

    % Checks made by the callee; actual constness not checked yet shall derive
    % from the transformed, corresponding method terminator:
    %
    { _State, Result } = executeRequestAs( ParentClassname, State, RequestAtom,
                                           MaybeArgumentList ),

    Result.





% Section for oneways:


-doc """
Executes the specified parameter-less oneway of the current instance, calling
implicitly any overridden version of the method.

Allows to call synchronously from the code of a given class its actual
overridden methods (oneways, here), including from child classes.

Example: If in some start method of an EngineVehicle class one wants to call the
(possibly overridden by, say, a class Car) `startEngine/1` oneway, then
`executeOneway/2` should be used: `MyVehicle ! startEngine` would not be
synchronous, `startEngine(State)` would call `EngineVehicle:startEngine/1`
instead of `Car:startEngine/1` when called from a Car instance, and of course
EngineVehicle should know nothing from its Car child class.

Returns an updated state.
""".
-spec executeOneway( wooper:state(), oneway_name() ) -> wooper:state().
% Legit case:
executeOneway( State, OnewayAtom ) when is_record( State, state_holder )
                                        andalso is_atom( OnewayAtom ) ->

    %trace_utils:debug_fmt( "executeOneway/2: executing ~ts() from ~ts.",
    %   [ OnewayAtom, State#state_holder.actual_class ] ),

    wooper_handle_local_oneway_execution( OnewayAtom, State, _ArgumentList=[] );


% Invalid oneway name:
executeOneway( State, OnewayAtomError ) when is_record( State, state_holder ) ->

    wooper:log_error( " when executing oneway locally, its name shall "
        "be an atom, not:~n ~p", [ OnewayAtomError ], State ),

    throw( { wooper_invalid_oneway_call,
             { invalid_oneway_name, OnewayAtomError } } );

% Invalid state:
executeOneway( StateError, SomeOnewayName ) ->

    wooper:log_error( " when executing oneway ~ts/1 locally, "
        "first parameter should be a state, not:~n ~p",
        [ wooper:method_name_to_string( SomeOnewayName ), StateError ],
        ?MODULE ),

    throw( { wooper_invalid_oneway_call,
             { SomeOnewayName, { invalid_state, StateError } } } ).



-doc """
Executes the specified const parameter-less oneway of the current instance,
calling implicitly any overridden version of the method.

Allows to call synchronously from the code of a given class its actual
overridden methods (oneways, here), including from child classes.

See `executeOneway/2`.

Const oneways return nothing.
""".
-spec executeConstOneway( wooper:state(), oneway_name() ) -> void().
executeConstOneway( State, OnewayAtom ) ->

    % Checks made by the callee; actual constness not checked yet shall derive
    % from the transformed, corresponding method terminator:
    %
    _State = executeOneway( State, OnewayAtom ).



-doc """
Executes the specified oneway of the current instance, calling implicitly any
overridden version of the method.

Allows to call synchronously from the code of a given class its actual
overridden methods (oneways, here), including from child classes.

See `executeOneway/2`.

Returns an updated state.
""".
-spec executeOneway( wooper:state(), oneway_name(), method_arguments() ) ->
                                wooper:state().
% Most usual legit case:
executeOneway( State, OnewayAtom, ArgumentList ) when
        is_record( State, state_holder ) andalso is_atom( OnewayAtom )
        andalso is_list( ArgumentList ) ->

    %trace_utils:debug_fmt( "executeOneway/3 with list: executing ~ts(~w) "
    %   "from ~ts.",
    %   [ OnewayAtom, ArgumentList, State#state_holder.actual_class ] ),

    wooper_handle_local_oneway_execution( OnewayAtom, State, ArgumentList );


% Here having a single, standalone argument, to be promoted to list:
executeOneway( State, OnewayAtom, StandaloneArgument ) when
        is_record( State, state_holder ) andalso is_atom( OnewayAtom ) ->

    %trace_utils:debug_fmt( "executeOneway/3 with standalone argument: "
    %   "executing ~ts(~w) from ~ts.",
    %   [ OnewayAtom, StandaloneArgument, State#state_holder.actual_class ] ),

    wooper_handle_local_oneway_execution( OnewayAtom, State,
                                          [ StandaloneArgument ] );

% Invalid oneway name:
executeOneway( State, OnewayAtomError, _ArgMaybeList )
                        when is_record( State, state_holder )  ->

    wooper:log_error( " when executing oneway locally, its name shall "
        "be an atom, not:~n ~p", [ OnewayAtomError ], State ),

    throw( { wooper_invalid_oneway_call,
             { invalid_oneway_name, OnewayAtomError } } );


% Invalid state:
executeOneway( StateError, OnewayAtom, ArgMaybeList ) ->

    wooper:log_error( " when executing oneway ~ts locally, "
        "first parameter should be a state, not:~n ~p",
        [ wooper:method_call_to_string( OnewayAtom, ArgMaybeList ),
          StateError ],
        ?MODULE ),

    throw( { wooper_invalid_oneway_call,
             { OnewayAtom, { invalid_state, StateError } } } ).




-doc """
Executes the specified const oneway of the current instance, calling implicitly
any overridden version of the method.

Allows to call synchronously from the code of a given class its actual
overridden methods (oneways, here), including from child classes.

See `executeOneway/2`.

Const oneways return nothing.
""".
-spec executeConstOneway( wooper:state(), oneway_name(), method_arguments() ) ->
                                void().
executeConstOneway( State, OnewayAtom, ArgumentMaybeList ) ->

    % Checks made by the callee; actual constness not checked yet shall derive
    % from the transformed, corresponding method terminator:
    %
    _State = executeOneway( State, OnewayAtom, ArgumentMaybeList ).



-doc """
Executes the version of the specified parameter-less oneway, as it has been
defined by the specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeOneway/2`.

Returns an updated state.
""".
-spec executeOnewayAs( classname(), wooper:state(), oneway_name() ) ->
                                wooper:state().
% Legit case:
executeOnewayAs( ParentClassname, State, OnewayAtom )
        when is_atom( ParentClassname ) andalso is_record( State, state_holder )
             andalso is_atom( OnewayAtom ) ->

    %trace_utils:debug_fmt( "executeOnewayAs/3: executing ~ts() from ~ts "
    %   "as parent class ~ts.",
    %   [ OnewayAtom, State#state_holder.actual_class, ParentClassname ] ),

    wooper_handle_local_oneway_execution_as( OnewayAtom, State,
        _ArgumentList=[], ParentClassname );


% Invalid oneway name:
executeOnewayAs( ParentClassname, State, OnewayAtomError )
        when is_atom( ParentClassname )
             andalso is_record( State, state_holder ) ->

    wooper:log_error( " when executing oneway locally as ~ts, the oneway name "
        "shall be an atom, not:~n ~p", [ ParentClassname, OnewayAtomError ],
        State ),

    throw( { wooper_invalid_oneway_call,
             { invalid_oneway_name, OnewayAtomError } } );


% Invalid state:
executeOnewayAs( ParentClassname, StateError, SomeOnewayName )
        when is_atom( ParentClassname ) ->

    wooper:log_error( " when executing oneway ~ts/1 locally as ~ts, "
        "second parameter should be a state, not:~n ~p",
        [ wooper:method_name_to_string( SomeOnewayName ), ParentClassname,
          StateError ], ?MODULE ),

    throw( { wooper_invalid_oneway_call, SomeOnewayName } );


% Invalid parent classname:
executeOnewayAs( ParentClassnameError, _State, SomeOnewayName ) ->

    wooper:log_error( " when executing oneway ~ts/1 locally, parent "
        "classname shall be an atom, not:~n ~p",
        [ wooper:method_name_to_string( SomeOnewayName ),
          ParentClassnameError ], ?MODULE ),

    throw( { wooper_invalid_oneway_call, { SomeOnewayName,
            { invalid_parent_classname, ParentClassnameError } } } ).



-doc """
Executes the specified const oneway of the current instance, calling implicitly
any overridden version of the method.

Allows to call synchronously from the code of a given class its actual
overridden methods (oneways, here), including from child classes.

See `executeOneway/2`.

Const oneways return nothing.
""".
-spec executeConstOnewayAs( classname(), wooper:state(), oneway_name() ) ->
                                void().
executeConstOnewayAs( ParentClassname, State, OnewayAtom ) ->

    % Checks made by the callee; actual constness not checked yet shall derive
    % from the transformed, corresponding method terminator:
    %
    _State = executeOnewayAs( ParentClassname, State, OnewayAtom ).



-doc """
Executes the version of the specified oneway, as it has been defined by the
specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeOneway/2`.

Returns an updated state.
""".
-spec executeOnewayAs( classname(), wooper:state(), oneway_name(),
                       method_arguments() ) -> wooper:state().
% Most usual legit case:
executeOnewayAs( ParentClassname, State, OnewayAtom, ArgumentList ) when
        is_atom( ParentClassname ) andalso is_record( State, state_holder )
        andalso is_atom( OnewayAtom ) andalso is_list( ArgumentList ) ->

    %trace_utils:debug_fmt( "executeOnewayAs/4 with list: executing ~ts(~w) "
    %   "from ~ts with ~ts.",
    %   [ OnewayAtom, ArgumentList, State#state_holder.actual_class,
    %     ParentClassname ] ),

    wooper_handle_local_oneway_execution_as( OnewayAtom, State,
                                             ArgumentList, ParentClassname );


% Here having a single, standalone argument, to be promoted to list:
executeOnewayAs( ParentClassname, State, OnewayAtom, StandaloneArgument ) when
        is_atom( ParentClassname ) andalso is_record( State, state_holder )
        andalso is_atom( OnewayAtom ) ->

    %trace_utils:debug_fmt( "executeOnewayAs/4 with standalone argument: "
    %   "executing ~ts(~w) from ~ts with ~ts.",
    %   [ OnewayAtom, StandaloneArgument, State#state_holder.actual_class,
    %     ParentClassname ] ),

    wooper_handle_local_oneway_execution_as( OnewayAtom, State,
        _ArgumentList=[ StandaloneArgument ], ParentClassname );


% Invalid oneway name:
executeOnewayAs( ParentClassname, State, OnewayAtomError, _ArgMaybeList )
        when is_atom( ParentClassname )
             andalso is_record( State, state_holder ) ->

    wooper:log_error( " when executing oneway locally as ~ts, the oneway "
        "name shall be an atom, not:~n ~p",
        [ ParentClassname, OnewayAtomError ], State ),

    throw( { wooper_invalid_oneway_call,
             { invalid_oneway_name, OnewayAtomError } } );


% Invalid state:
executeOnewayAs( ParentClassname, StateError, OnewayAtom, ArgMaybeList )
        when is_atom( ParentClassname ) ->

    wooper:log_error( " when executing oneway ~ts locally as ~ts, "
        "second parameter should be a state, not:~n ~p",
        [ wooper:method_call_to_string( OnewayAtom, ArgMaybeList ),
          ParentClassname, StateError ], ?MODULE ),

    throw( { wooper_invalid_oneway_call,
             { OnewayAtom, { invalid_state, StateError } } } );


% Invalid parent classname:
executeOnewayAs( ParentClassnameError, _State, OnewayAtom, ArgMaybeList ) ->

    wooper:log_error( " when executing oneway ~ts locally, "
        "parent classname shall be an atom, not:~n ~p",
        [ wooper:method_call_to_string( OnewayAtom, ArgMaybeList ),
          ParentClassnameError ], ?MODULE ),

    throw( { wooper_invalid_oneway_call, { OnewayAtom,
            { invalid_parent_classname, ParentClassnameError } } } ).



-doc """
Executes the version of the specified const oneway, as it has been defined by
the specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeOneway/2`.

Const oneways return nothing.
""".
-spec executeConstOnewayAs( classname(), wooper:state(), oneway_name(),
                            method_arguments() ) -> void().
executeConstOnewayAs( ParentClassname, State, OnewayAtom, MaybeArgumentList ) ->

    % Checks made by the callee; actual constness not checked yet shall derive
    % from the transformed, corresponding method terminator:
    %
    _State = executeOnewayAs( ParentClassname, State, OnewayAtom,
                              MaybeArgumentList ).
