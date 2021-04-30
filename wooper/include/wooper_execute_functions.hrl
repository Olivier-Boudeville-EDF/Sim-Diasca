% Copyright (C) 2003-2021 Olivier Boudeville
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



% Implementation section (debug mode managed in the called wooper_* helpers)



% Section for requests.


% Parameter-less request, calling implicitly any overridden version of the
% method.
%
% Returns an updated state and a result.
%
-spec executeRequest( wooper:state(), request_name() ) ->
							{ wooper:state(), method_internal_result() }.

executeRequest( State, RequestAtom ) when is_record( State, state_holder )
										  andalso is_atom( RequestAtom ) ->

	%trace_utils:debug_fmt( "executeRequest/2: executing ~ts() from ~ts.",
	%	[ RequestAtom, State#state_holder.actual_class ] ),

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



% Parameter-less const request, calling implicitly any overridden version of the
% method.
%
% Returns only a result.
%
-spec executeConstRequest( wooper:state(), request_name() ) ->
								method_internal_result().
executeConstRequest( State, RequestAtom ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	{ _State, Result } = executeRequest( State, RequestAtom ),
	Result.




% Allows to call synchronously from the code of a given class its actual
% overridden methods (requests, here), including from child classes.
%
% Example: If in a start method of an EngineVehicle class one wants to call the
% (possibly overridden by, say, a class Car) startEngine method, then
% executeRequest should be used: 'MyVehicle ! {startEngine..' would not be
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
%
% Note: Stripped-down version of wooper_main_loop.
%
-spec executeRequest( wooper:state(), request_name(), method_arguments() ) ->
							{ wooper:state(), method_internal_result() }.
executeRequest( State, RequestAtom, ArgumentList ) when
	  is_record( State, state_holder ) andalso is_atom( RequestAtom )
	  andalso is_list( ArgumentList ) ->

	%trace_utils:debug_fmt( "executeRequest/3 with list: executing ~ts(~w) "
	%   "from ~ts.",
	%	[ RequestAtom, ArgumentList, State#state_holder.actual_class ] ),

	wooper_handle_local_request_execution( RequestAtom, State, ArgumentList );


% Here the third parameter is not a list:
executeRequest( State, RequestAtom, StandaloneArgument ) when
	  is_record( State, state_holder ) andalso is_atom( RequestAtom )->

	%trace_utils:debug_fmt( "executeRequest/3 with standalone argument: "
	%	"executing ~ts(~w) from ~ts.",
	%	[ RequestAtom, StandaloneArgument, State#state_holder.actual_class ] ),

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



% Allows to call synchronously from the code of a given class its actual
% overridden methods (const requests, here), including from child classes.
%
-spec executeConstRequest( wooper:state(), request_name(),
						   method_arguments() ) -> method_internal_result().
executeConstRequest( State, RequestAtom, ArgumentList ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	{ _State, Result } = executeRequest( State, RequestAtom, ArgumentList ),
	Result.



% Parameter-less request, calling the version of the method as defined in the
% specified class.
%
-spec executeRequestAs( wooper:state(), classname(), request_name() ) ->
								{ wooper:state(), method_internal_result() }.
executeRequestAs( State, Classname, RequestAtom )
  when is_record( State, state_holder ) andalso is_atom( Classname )
	   andalso is_atom( RequestAtom ) ->

	%trace_utils:debug_fmt( "executeRequestAs/3: executing ~ts() from ~ts "
	%   "with ~ts.",
	%	[ RequestAtom, State#state_holder.actual_class, Classname ]),

	wooper_handle_local_request_execution_as( RequestAtom, State,
							_ArgumentList=[], Classname );


executeRequestAs( StateError, Classname, RequestAtom )
		when is_atom( Classname ) andalso is_atom( RequestAtom ) ->

	wooper:log_error( "when executing request ~p in the context of class ~ts: "
		"first parameter should be a state, not '~p'.",
		[ RequestAtom, Classname, StateError ], ?MODULE ),

	throw( { wooper_invalid_request_call, RequestAtom } );


executeRequestAs( _State, ClassnameError, RequestAtomError ) ->

	wooper:log_error( "when executing request in a class context: "
		"'~p' and '~p' should both be atoms.",
		[ ClassnameError, RequestAtomError ], ?MODULE ),

	throw( { wooper_invalid_request_call, ClassnameError, RequestAtomError } ).



% Parameter-less const request, calling the version of the method as defined in
% the specified class.
%
-spec executeConstRequestAs( wooper:state(), classname(), request_name() ) ->
								method_internal_result().
executeConstRequestAs( State, Classname, RequestAtom ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	{ _State, Result } = executeRequestAs( State, Classname, RequestAtom ),
	Result.



% Allows to call synchronously from the code of a given class overridden methods
% (requests, here) as defined in specified classes.
%
% Note: Stripped-down version of wooper_main_loop.
%
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
	%	"executing ~ts(~w) from ~ts with ~ts.",
	%	[ RequestAtom, StandaloneArgument, State#state_holder.actual_class,
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



% Allows to call synchronously from the code of a given class overridden methods
% (const requests, here) as defined in specified classes.
%
% Note: Stripped-down version of wooper_main_loop.
%
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



% Parameter-less oneway.
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



% Parameter-less const oneway.
-spec executeConstOneway( wooper:state(), oneway_name() ) -> void().
executeConstOneway( State, OnewayAtom ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	_State = executeOneway( State, OnewayAtom ).



% Most classical oneway.
%
-spec executeOneway( wooper:state(), oneway_name(), method_arguments() ) ->
								wooper:state().
executeOneway( State, OnewayAtom, ArgumentList ) when
	  is_record( State, state_holder ) andalso is_atom( OnewayAtom )
	  andalso is_list( ArgumentList ) ->

	%trace_utils:debug_fmt( "executeOneway/3 with list: executing ~ts(~w) "
	%   "from ~ts.",
	%	[ OnewayAtom, ArgumentList, State#state_holder.actual_class ] ),

	wooper_handle_local_oneway_execution( OnewayAtom, State, ArgumentList );


% Here third parameter is not a list:
executeOneway( State, OnewayAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom( OnewayAtom ) ->

	%trace_utils:debug_fmt( "executeOneway/3 with standalone argument: "
	%	"executing ~ts(~w) from ~ts.",
	%	[ OnewayAtom, StandaloneArgument, State#state_holder.actual_class ] ),

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



% Most classical const oneway.
-spec executeConstOneway( wooper:state(), oneway_name(), method_arguments() ) ->
								void().
executeConstOneway( State, OnewayAtom, ArgumentList ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	_State = executeOneway( State, OnewayAtom, ArgumentList ).




% Allows to call synchronously from the code of a given class its actual
% overridden methods (oneways, here), including from child classes.
%
% Example: If in a start method of a EngineVehicle class one wants to call the
% (possibly overridden by, say, a class Car) startEngine method, then
% executeOneway should be used: 'MyVehicle ! startEngine' would not be
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
%
-spec executeOnewayAs( wooper:state(), classname(), oneway_name() ) ->
								wooper:state().
executeOnewayAs( State, Classname, OnewayAtom )
  when is_record( State, state_holder ) andalso is_atom( Classname )
	   andalso is_atom( OnewayAtom ) ->

	%trace_utils:debug_fmt( "executeOnewayAs/3: executing ~ts() from ~ts.",
	%	[ OnewayAtom, State#state_holder.actual_class ] ),

	wooper_handle_local_oneway_execution_as( OnewayAtom, State,
											 _ArgumentList=[], Classname );



executeOnewayAs( State, Classname, OnewayAtom )
  when is_record( State, state_holder ) ->

	wooper:log_error( "when executing oneway: '~p' (oneway name) and "
		"'~p' (class name) should be both atoms.", [ OnewayAtom, Classname ] ),

	throw( { wooper_invalid_oneway_call, OnewayAtom } );


executeOnewayAs( StateError, Classname, OnewayAtom ) ->

	wooper:log_error( "when executing oneway ~p with ~ts: "
		"first parameter should be a state, not '~p'.",
		[  OnewayAtom, Classname, StateError ] ),

	throw( { wooper_invalid_oneway_call, OnewayAtom } ).



% Allows to call synchronously from the code of a given class its actual
% overridden methods (const oneways, here), including from child classes.
%
-spec executeConstOnewayAs( wooper:state(), classname(), oneway_name() ) ->
								void().
executeConstOnewayAs( State, Classname, OnewayAtom ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	_State = executeOnewayAs( State, Classname, OnewayAtom ).



% Allows to call synchronously from the code of a given class the oneway defined
% in specified class, instead of determining it from the instance virtual table.
%
% Note: Stripped-down version of wooper_main_loop.
%
-spec executeOnewayAs( wooper:state(), classname(), oneway_name(),
					   method_arguments() ) -> wooper:state().
executeOnewayAs( State, Classname, OnewayAtom, ArgumentList ) when
	  is_record( State, state_holder ) andalso is_atom( Classname )
	  andalso is_atom( OnewayAtom ) andalso is_list( ArgumentList ) ->

	%trace_utils:debug_fmt( "executeOneway/4 with list: executing ~ts(~w) "
	%   "from ~ts with ~ts.",
	%	[ OnewayAtom, ArgumentList, State#state_holder.actual_class,
	%     Classname ] ),

	wooper_handle_local_oneway_execution_as( OnewayAtom, State,
											 ArgumentList, Classname );



% Here third parameter is not a list:
executeOnewayAs( State, Classname, OnewayAtom, StandaloneArgument ) when
	  is_record( State, state_holder ) andalso is_atom( Classname )
	  andalso is_atom( OnewayAtom ) ->

	%trace_utils:debug_fmt( "executeOnewayAs/4 with standalone argument: "
	%	"executing ~ts(~w) from ~ts with ~ts.",
	%	[ OnewayAtom, StandaloneArgument, State#state_holder.actual_class,
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



% Allows to call synchronously from the code of a given class the const oneway
% defined in specified class, instead of determining it from the instance
% virtual table.
%
% Note: Stripped-down version of wooper_main_loop.
%
-spec executeConstOnewayAs( wooper:state(), classname(), oneway_name(),
							method_arguments() ) -> void().
executeConstOnewayAs( State, Classname, OnewayAtom, ArgumentList ) ->

	% Checks made by the callee; actual constness not checked yet shall derive
	% from the transformed, corresponding method terminator:
	%
	_State = executeOnewayAs( State, Classname, OnewayAtom, ArgumentList ).
