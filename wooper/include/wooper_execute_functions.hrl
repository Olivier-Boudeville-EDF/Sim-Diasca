% Copyright (C) 2007-2025 Olivier Boudeville
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

executeRequest( State, RequestAtom ) when is_record( State, state_holder )
										  andalso is_atom( RequestAtom ) ->

	%trace_utils:debug_fmt( "executeRequest/2: executing ~ts() from ~ts.",
	%   [ RequestAtom, State#state_holder.actual_class ] ),

	wooper_handle_local_request_execution( RequestAtom, State,
										   _ArgumentList=[] );

executeRequest( State, RequestAtomError )
                                        when is_record( State, state_holder ) ->

	wooper:log_error( "when executing local request: '~p' is not an atom.",
					  [ RequestAtomError ], State ),

	throw( { wooper_invalid_request_call, RequestAtomError } );


executeRequest( StateError, RequestAtom ) when is_atom( RequestAtom ) ->

	wooper:log_error( "when executing request ~p: "
		"first parameter should be a state, not '~p'.",
		[ RequestAtom, StateError ], ?MODULE ),

	throw( { wooper_invalid_request_call, RequestAtom } );


executeRequest( StateError, RequestAtomError ) ->

	wooper:log_error( "when executing request: '~p' is not a state and "
		"'~p' is not an atom.",
		[ StateError, RequestAtomError ], ?MODULE ),

	throw( { wooper_invalid_request_call, StateError, RequestAtomError } ).



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
executeRequest( State, RequestAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom( RequestAtom )
		andalso is_list( ArgumentList ) ->

	%trace_utils:debug_fmt( "executeRequest/3 with list: executing ~ts(~w) "
	%   "from ~ts.",
	%   [ RequestAtom, ArgumentList, State#state_holder.actual_class ] ),

	wooper_handle_local_request_execution( RequestAtom, State, ArgumentList );


% Here the third parameter is not a list:
executeRequest( State, RequestAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom( RequestAtom )->

	%trace_utils:debug_fmt( "executeRequest/3 with standalone argument: "
	%   "executing ~ts(~w) from ~ts.",
	%   [ RequestAtom, StandaloneArgument, State#state_holder.actual_class ] ),

	wooper_handle_local_request_execution( RequestAtom, State,
		_ArgumentList=[ StandaloneArgument ] );


% Catches all errors:
executeRequest( StateError, RequestAtom, _LastArg )
							when is_atom( RequestAtom ) ->

	wooper:log_error( "when executing request ~p: "
		"first parameter should be a state, not '~p'.",
		[ RequestAtom, StateError ], ?MODULE ),

	throw( { wooper_invalid_request_call, RequestAtom } );


executeRequest( State, RequestAtomError, _LastArg )
							when is_record( State, state_holder ) ->

	wooper:log_error( "when executing request: '~p' is not an atom.",
					  [ RequestAtomError ], State ),

	throw( { wooper_invalid_request_call, RequestAtomError } );


executeRequest( StateError, RequestAtomError, _LastArg ) ->

	wooper:log_error( "when executing request: first parameter should "
		"be a state, not '~p', and '~p' is not an atom.",
		[ StateError, RequestAtomError ], ?MODULE ),

	throw( { wooper_invalid_request_call, RequestAtomError } ).



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
executeConstRequest( State, RequestAtom, ArgumentList ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	{ _State, Result } = executeRequest( State, RequestAtom, ArgumentList ),
	Result.



-doc """
Executes the version of the specified parameter-less request, as it has been
defined by the specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See executeRequest/2.

Returns an updated state and a result.
""".
-spec executeRequestAs( wooper:state(), classname(), request_name() ) ->
								{ wooper:state(), method_internal_result() }.
executeRequestAs( State, ParentClassname, RequestAtom )
		when is_record( State, state_holder ) andalso is_atom( ParentClassname )
			 andalso is_atom( RequestAtom ) ->

	%trace_utils:debug_fmt( "executeRequestAs/3: executing ~ts() from ~ts "
	%   "as parent class ~ts.",
	%   [ RequestAtom, State#state_holder.actual_class, ParentClassname ]),

	wooper_handle_local_request_execution_as( RequestAtom, State,
		_ArgumentList=[], ParentClassname );


executeRequestAs( StateError, ParentClassname, RequestAtom )
		when is_atom( ParentClassname ) andalso is_atom( RequestAtom ) ->

	wooper:log_error( "when executing request ~p  as parent "
		"class ~ts: first parameter should be a state, not '~p'.",
		[ RequestAtom, ParentClassname, StateError ], ?MODULE ),

	throw( { wooper_invalid_request_call, RequestAtom } );


executeRequestAs( _State, ParentClassnameError, RequestAtomError ) ->

	wooper:log_error( "when executing request as a parent class: "
		"'~p' and '~p' should both be atoms.",
		[ ParentClassnameError, RequestAtomError ], ?MODULE ),

	throw( { wooper_invalid_request_call, ParentClassnameError,
			 RequestAtomError } ).



-doc """
Executes the version of the specified parameter-less const request, as it has
been defined by the specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeRequest/2`.

Returns only a result.
""".
-spec executeConstRequestAs( wooper:state(), classname(), request_name() ) ->
								method_internal_result().
executeConstRequestAs( State, ParentClassname, RequestAtom ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	{ _State, Result } = executeRequestAs( State, ParentClassname,
										   RequestAtom ),
	Result.



-doc """
Executes the version of the specified request, as it has been defined by the
specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeRequest/2`.

Returns an updated state and a result.
""".
-spec executeRequestAs( wooper:state(), classname(), request_name(),
		method_arguments() ) -> { wooper:state(), method_internal_result() }.
executeRequestAs( State, Classname, RequestAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom( Classname )
		andalso is_atom( RequestAtom ) andalso is_list( ArgumentList ) ->

	%trace_utils:debug_fmt( "executeRequestAs/4 with list: executing ~ts(~w) "
	%  "from ~ts with ~ts.", [ RequestAtom, ArgumentList,
	% State#state_holder.actual_class, Classname ] ),

	wooper_handle_local_request_execution_as( RequestAtom, State,
											  ArgumentList, Classname );


% Here the third parameter is not a list:
executeRequestAs( State, Classname, RequestAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom( Classname )
		andalso is_atom( RequestAtom ) ->

	%trace_utils:debug_fmt( "executeRequestAs/3 with standalone argument: "
	%   "executing ~ts(~w) from ~ts with ~ts.",
	%   [ RequestAtom, StandaloneArgument, State#state_holder.actual_class,
	% Classname ] ),

	wooper_handle_local_request_execution_as( RequestAtom, State,
		_ArgumentList=[ StandaloneArgument ], Classname );


% Error cases below:
executeRequestAs( StateError, Classname, RequestAtom, _LastArg )
		when is_atom( Classname ) andalso is_atom( RequestAtom ) ->

	wooper:log_error( "when executing request ~p: "
		"first parameter should be a state, not '~p'.",
		[ RequestAtom, StateError ], ?MODULE ),

	throw( { wooper_invalid_request_call, RequestAtom } );


% Catches all remaining errors:
executeRequestAs( _State, ClassnameError, RequestAtomError, _LastArg ) ->

	wooper:log_error( "when executing request: both '~p' (classname) and "
		"'~p' (request name) should be atoms.",
		[ ClassnameError, RequestAtomError ], ?MODULE ),

	throw( { wooper_invalid_request_call, ClassnameError, RequestAtomError } ).



-doc """
Executes the version of the specified const request, as it has been defined by
the specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeRequest/2`.

Returns only a result.
""".
-spec executeConstRequestAs( wooper:state(), classname(), request_name(),
			method_arguments() ) -> method_internal_result().
executeConstRequestAs( State, Classname, RequestAtom, ArgumentList ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	{ _State, Result } = executeRequestAs( State, Classname, RequestAtom,
										   ArgumentList ),
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
executeOneway( State, OnewayAtom ) when is_record( State, state_holder )
										andalso is_atom( OnewayAtom ) ->

	%trace_utils:debug_fmt( "executeOneway/2: executing ~ts() from ~ts.",
	%   [ OnewayAtom, State#state_holder.actual_class ] ),

	wooper_handle_local_oneway_execution( OnewayAtom, State, _ArgumentList=[] );


executeOneway( State, OnewayError ) when is_record( State, state_holder ) ->

	wooper:log_error( "when executing a oneway: its name should be an atom, "
					  "not '~p'.", [ OnewayError ], State ),

	throw( { wooper_invalid_oneway_call, OnewayError } );


executeOneway( StateError, OnewayAtom ) when is_atom( OnewayAtom ) ->

	wooper:log_error( "when executing oneway ~p: "
		"first parameter should be a state, not '~p'.",
		[ OnewayAtom, StateError ], ?MODULE ),

	throw( { wooper_invalid_oneway_call, OnewayAtom } );


executeOneway( StateError, OnewayError ) ->

	wooper:log_error( "when executing oneway: '~ts' is not a state and "
		"'~p' is not an atom.", [ StateError, OnewayError ], ?MODULE ),

	throw( { wooper_invalid_oneway_call, OnewayError } ).



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
executeOneway( State, OnewayAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom( OnewayAtom )
		andalso is_list( ArgumentList ) ->

	%trace_utils:debug_fmt( "executeOneway/3 with list: executing ~ts(~w) "
	%   "from ~ts.",
	%   [ OnewayAtom, ArgumentList, State#state_holder.actual_class ] ),

	wooper_handle_local_oneway_execution( OnewayAtom, State, ArgumentList );


% Here third parameter is not a list:
executeOneway( State, OnewayAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom( OnewayAtom ) ->

	%trace_utils:debug_fmt( "executeOneway/3 with standalone argument: "
	%   "executing ~ts(~w) from ~ts.",
	%   [ OnewayAtom, StandaloneArgument, State#state_holder.actual_class ] ),

	wooper_handle_local_oneway_execution( OnewayAtom, State,
										  [ StandaloneArgument ] );


% All errors caught below:
executeOneway( StateError, OnewayAtom, _LastArg ) when is_atom( OnewayAtom ) ->

	wooper:log_error( "when executing oneway ~p: "
		"first parameter should be a state, not '~p'.",
		[ OnewayAtom, StateError ], ?MODULE ),

	throw( { wooper_invalid_oneway_call, OnewayAtom } );


executeOneway( State, OnewayAtomError, _LastArg )
		when is_record( State, state_holder ) ->

	wooper:log_error( "when executing oneway: '~p' is not an atom.",
					  [ OnewayAtomError ], State ),

	throw( { wooper_invalid_oneway_call, OnewayAtomError } );


executeOneway( _State, OnewayAtomError, _LastArg ) ->

	wooper:log_error( "when executing oneway: '~p' is not an atom.",
					  [ OnewayAtomError ], ?MODULE ),

	throw( { wooper_invalid_oneway_call, OnewayAtomError } ).



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
executeConstOneway( State, OnewayAtom, ArgumentList ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	_State = executeOneway( State, OnewayAtom, ArgumentList ).



-doc """
Executes the version of the specified parameter-less oneway, as it has been
defined by the specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeOneway/2`.

Returns an updated state.
""".
-spec executeOnewayAs( wooper:state(), classname(), oneway_name() ) ->
								wooper:state().
executeOnewayAs( State, ParentClassname, OnewayAtom )
		when is_record( State, state_holder ) andalso is_atom( ParentClassname )
			 andalso is_atom( OnewayAtom ) ->

	%trace_utils:debug_fmt( "executeOnewayAs/3: executing ~ts() from ~ts "
	%   "as parent class ~ts.",
	%   [ OnewayAtom, State#state_holder.actual_class, ParentClassname ] ),

	wooper_handle_local_oneway_execution_as( OnewayAtom, State,
		_ArgumentList=[], ParentClassname );


executeOnewayAs( StateError, ParentClassname, OnewayAtom )
								when is_record( StateError, state_holder ) ->

	wooper:log_error( "when executing oneway ~p as parent "
		"class ~ts: first parameter should be a state, not '~p'.",
		[ OnewayAtom, ParentClassname, StateError ], ?MODULE ),

	throw( { wooper_invalid_oneway_call, OnewayAtom } );


executeOnewayAs( _StateError, ParentClassnameError, OnewayAtomError ) ->

	wooper:log_error( "when executing oneway as a parent class: "
		"'~p' and '~p' should both be atoms.",
		[ ParentClassnameError, OnewayAtomError ], ?MODULE ),

	throw( { wooper_invalid_oneway_call, ParentClassnameError,
			 OnewayAtomError } ).



-doc """
Executes the specified const oneway of the current instance, calling implicitly
any overridden version of the method.

Allows to call synchronously from the code of a given class its actual
overridden methods (oneways, here), including from child classes.

See `executeOneway/2`.

Const oneways return nothing.
""".
-spec executeConstOnewayAs( wooper:state(), classname(), oneway_name() ) ->
								void().
executeConstOnewayAs( State, Classname, OnewayAtom ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	_State = executeOnewayAs( State, Classname, OnewayAtom ).



-doc """
Executes the version of the specified oneway, as it has been defined by the
specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeOneway/2`.

Returns an updated state.
""".
-spec executeOnewayAs( wooper:state(), classname(), oneway_name(),
					   method_arguments() ) -> wooper:state().
executeOnewayAs( State, Classname, OnewayAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom( Classname )
		andalso is_atom( OnewayAtom ) andalso is_list( ArgumentList ) ->

	%trace_utils:debug_fmt( "executeOneway/4 with list: executing ~ts(~w) "
	%   "from ~ts with ~ts.",
	%   [ OnewayAtom, ArgumentList, State#state_holder.actual_class,
	%     Classname ] ),

	wooper_handle_local_oneway_execution_as( OnewayAtom, State,
											 ArgumentList, Classname );


% Here third parameter is not a list:
executeOnewayAs( State, Classname, OnewayAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom( Classname )
		andalso is_atom( OnewayAtom ) ->

	%trace_utils:debug_fmt( "executeOnewayAs/4 with standalone argument: "
	%   "executing ~ts(~w) from ~ts with ~ts.",
	%   [ OnewayAtom, StandaloneArgument, State#state_holder.actual_class,
	%     Classname ] ),

	wooper_handle_local_oneway_execution_as( OnewayAtom, State,
		_ArgumentList=[ StandaloneArgument ], Classname );


executeOnewayAs( StateError, Classname, OnewayAtom, _LastArg )
			when is_atom( Classname ) andalso is_atom( OnewayAtom ) ->

	wooper:log_error( "when executing oneway ~p with ~ts: "
		"first parameter should be a state, not '~p'.",
		[ OnewayAtom, Classname, StateError ], ?MODULE ),

	throw( { wooper_invalid_oneway_call, OnewayAtom } );


% Catches all remaining errors:
executeOnewayAs( _State, Classname, OnewayAtomError, _LastArg ) ->

	wooper:log_error( "when executing oneway with ~ts: both '~p' "
		"and '~p' should be atoms.", [ Classname, OnewayAtomError ], ?MODULE ),

	throw( { wooper_invalid_oneway_call, OnewayAtomError } ).



-doc """
Executes the version of the specified const oneway, as it has been defined by
the specified parent class of the current instance.

Allows to call synchronously from the code of a given class a version defined
through its inheritance tree.

See `executeOneway/2`.

Const oneways return nothing.
""".
-spec executeConstOnewayAs( wooper:state(), classname(), oneway_name(),
							method_arguments() ) -> void().
executeConstOnewayAs( State, Classname, OnewayAtom, ArgumentList ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	_State = executeOnewayAs( State, Classname, OnewayAtom, ArgumentList ).
