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


% Modular WOOPER header gathering all direct primitives for method execution,
% both in debug and non-debug mode (the two versions shall be one after the
% other and shall share their documentation):
%
% - wooper_execute_method/3
% - wooper_execute_method_as/4
% - wooper_effective_method_execution/4

% - wooper_handle_remote_request_execution/4
% - wooper_handle_local_request_execution/3
% - wooper_handle_local_request_execution_as/4

% - wooper_handle_remote_oneway_execution/3
% - wooper_handle_local_oneway_execution/3
% - wooper_handle_local_oneway_execution_as/3



% Much code is duplicated because no '-ifdef' clause can be defined in case
% clauses.


% Most of these functions should be macros but cannot, as their returned value
% would be the one of their first statement, not the one of their last. These
% functions are requested to be inlined instead.



% In general terms: if the method is not found or if its execution fails (ex:
% making a faulty return):
%
% - if the method is *called* (from outside) as a oneway, then an error trace is
% output and an exception is thrown (presumably crashing the instance process)
%
% - if the method is *called* (from outside) as a request, then an error trace
% is output, an appropriate error term is returned to the caller (so that it can
% manage it) and an exception is thrown (presumably crashing the instance
% process)
%
% - if a method is called locally (i.e. with either of
% execute{Oneway|Request}[As]), then an error trace is output and an exception
% is thrown (possibly crashing the instance process)


% Implementation notes:

% Requests and oneways can be arbitrarily nested: for example a request can be
% called, and its body may call executeOneway/2, and so on. All of them
% ultimately rely on wooper_execute_method.
%
% Following constraints apply:
%
% 1. we want errors to be detected as early as possible
%
% 2. requests having to send back a result to their caller, they cannot let
% exceptions freely propagate
%
% As a result, the only try/catch clause used at this level is for the
% management of remote requests, i.e. wooper_handle_remote_request_execution/3.


% Requests send results as they are (with 'CallerPid ! Result'). They could
% specify additionally the PID of the called instance ('CallerPid !
% {_InstancePID=self(), Result}') to discriminate possible parallel requests
% triggered by a single caller.




% Documentations and signatures, common to debug and non-debug modes:



% Executes the specified method, designated by its atom, with specified instance
% state and list of parameters.



% This helper function in all cases will trace any error, and return a pair made
% of a state and a result (possibly an error).



% If the method is not found (either in the class module or in its ancestor
% trees), an error tuple whose first element is an atom in
% 'wooper_{request,oneway}_not_found' is returned along with an unchanged state.
%
% If the method fails,
% If it does not fail but returns an unexpected result (i.e. not a tuple
% beginning with the atom 'return'), an error tuple beginning with the atom
% 'wooper_method_faulty_return' is returned with an unchanged state.
%
% If its execution succeeds, then {wooper_result,Result} is returned (with
% Result being the actual result of the method call) with an updated state.
%
% Finally, if the method does not return any result, the atom
% 'wooper_method_returns_void' is returned, which allows a caller that sent his
% PID (i.e. called this method as a request) to be warned it is a mistake, as no
% answer should be expected.
%
% In debug mode, the error logs have been added, as debugging faulty oneways is
% more difficult: they cannot return any error to the caller, they can just
% crash and notify any linked or monitoring process.
%
% Note: atom and state checking in guards should be superfluous.
%
-spec wooper_execute_method( method_name(), method_arguments(),
							 wooper:state() ) ->
		  { wooper:state(), method_internal_result() }.



% Exactly as wooper_execute_method, except that the target module (class) is
% directly specified, instead of being determined from the instance virtual
% table.
%
-spec wooper_execute_method_as( classname(), method_name(),
								method_arguments(), wooper:state() ) ->
		  { wooper:state(), method_internal_result() }.





% Section for wooper_execute_method/3.
%
% Note that Parameters must be already a list (no promotion of single elements).


-ifdef(wooper_debug_mode).


wooper_execute_method( MethodAtom, Parameters, State )
  when is_atom( MethodAtom ) andalso is_list( Parameters )
	   andalso is_record( State, state_holder ) ->

	%trace_utils:debug_fmt( "wooper_execute_method: looking up ~ts(~w) "
	%   "from ~ts (A).",	[ MethodAtom, Parameters, ?MODULE ] ),

	% +1: take into account the State additional parameter:
	MethodArity = length( Parameters ) + 1,

	case wooper_lookup_method( State, MethodAtom, MethodArity ) of

		{ value, LocatedModule } ->

			%trace_utils:debug_fmt( "wooper_execute_method: executing "
			%   "~ts:~ts(~w) from ~ts.",
			%   [ ?MODULE, MethodAtom, Parameters, LocatedModule ] ),

			% Returns {NewState, PossibleResult}:
			wooper_effective_method_execution( LocatedModule, MethodAtom,
											   State, Parameters );


		key_not_found ->

			Classname = State#state_holder.actual_class,

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					% method name and arity returned as separate tuple
					% elements, as if in a single string ("M/A"), the result
					% is displayed as a list:
					%
					wooper:log_error(
					  "oneway ~ts:~ts/~B not found, parameters were:~n~p~n",
					  [ Classname, MethodAtom, MethodArity, Parameters ],
					  State ),

					throw( { wooper_oneway_not_found, self(),
							 Classname, MethodAtom, MethodArity, Parameters } );

				_ ->

					% This is a request, returns the state and an error term,
					% and rely on the calling function (ex: wooper_main_loop) to
					% crash *after* having performed any relevant action (ex:
					% send back a relevant answer):
					%
					wooper:log_error(
					  "request ~ts:~ts/~B not found, parameters were:~n~p~n",
					  [ Classname, MethodAtom, MethodArity, Parameters ],
					  State ),

					throw( { wooper_request_not_found, self(),
							 Classname, MethodAtom, MethodArity, Parameters } )


			% No other term can be returned.

			end

	end.



-else. % not in wooper_debug_mode:



wooper_execute_method( MethodAtom, Parameters, State ) ->

	%trace_utils:debug_fmt( "wooper_execute_method: looking up ~ts(~w) "
	%	"from ~ts (B).", [ MethodAtom, Parameters, ?MODULE ] ),

	% +1: take into account the State additional parameter:
	MethodArity = length( Parameters ) + 1,

	case wooper_lookup_method( State, MethodAtom, MethodArity ) of

		{ value, LocatedModule } ->

			%trace_utils:debug_fmt( "wooper_execute_method: executing ~ts:~ts"
			% "(~w) from ~ts.",
			% [ ?MODULE, MethodAtom, Parameters, LocatedModule ] ),

			wooper_effective_method_execution( LocatedModule, MethodAtom,
											   State, Parameters );


		key_not_found ->

			Classname = State#state_holder.actual_class,

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash.
					%
					% Method name and arity returned as separate tuple elements,
					% as if in a single string ("M/A"), the result is displayed
					% as a list:
					%
					wooper:log_error(
					  "oneway ~ts:~ts/~B not found, parameters were:~n~p",
					  [ Classname, MethodAtom, MethodArity, Parameters ] ),

					throw( { wooper_oneway_not_found, self(), Classname,
							 MethodAtom, MethodArity, Parameters } );

				_ ->

					% This is a request, returns the state and an error term,
					% and rely on the calling function (ex: wooper_main_loop) to
					% crash after having performed any relevant action (ex: send
					% back a relevant answer):
					%
					wooper:log_error(
					  "request ~ts:~ts/~B not found, parameters were:~n~p~n",
					  [ Classname, MethodAtom, MethodArity, Parameters ] ),

					throw( { wooper_request_not_found, self(), Classname,
							 MethodAtom, MethodArity, Parameters } )

			end

		% No other term can be returned.

	end.


-endif. % not wooper_debug_mode.



% Looks-up specified method (Method/Arity, ex: toString/1) to be found in
% inheritance tree and returns either { 'value', Module } with Module
% corresponding to the class that implements that method, or 'key_not_found'.
%
% Note: uses the pre-built virtual table for this class.
%
% (helper)
%
-spec wooper_lookup_method( wooper:state(), method_name(), arity() ) ->
								{ 'value', classname() } | 'key_not_found'.
wooper_lookup_method( State, MethodAtom, Arity ) ->
	?wooper_table_type:lookup_entry( { MethodAtom, Arity },
		%persistent_term:get( State#state_holder.virtual_table_key ) ).
		State#state_holder.virtual_table ).




% Section for wooper_execute_method_as/4.
%
% (same implementation in debug or not)



wooper_execute_method_as( Classname, MethodAtom, Parameters, State )
  when is_atom( Classname ) andalso is_atom( MethodAtom )
	   andalso is_list( Parameters )
	   andalso is_record( State, state_holder ) ->

	% One check should be added: Classname must be a super-class
	% (direct or not) of the actual class.
	%
	wooper_effective_method_execution( Classname, MethodAtom, State,
									   Parameters ).






% Section for wooper_effective_method_execution/4.



% Triggers the actual method execution.
%
% In case of runtime error, this function can log and throw both for oneways and
% requests, as the former have no specific action to take in that case, and the
% latter enclose the full processing (from the message that triggered it) in a
% try/catch clause in order to be able to nevertheless send back a wooper_error
% to the caller before crashing, if necessary.
%
-spec wooper_effective_method_execution( module(), method_name(),
										 wooper:state(), method_arguments() ) ->
	{ wooper:state(), method_internal_result() }.



-ifdef(wooper_debug_mode).


wooper_effective_method_execution( SelectedModule, MethodAtom, State,
								   Parameters ) ->

	%trace_utils:debug_fmt( "WOOPER: effective execution of ~p:~p.",
	%			[ SelectedModule, MethodAtom ] ),

	% Of course the executed method may throw, we let exceptions propagate:
	case apply( SelectedModule, MethodAtom, [ State | Parameters ] ) of

		% Void method (no result returned, only a state - thus a oneway):
		% ?wooper_return_state:
		NewState when is_record( NewState, state_holder ) ->
			{ NewState, wooper_method_returns_void };

		% Method returning a result (and a state of course) - hence a request:
		% ?wooper_return_state_result:
		{ NewState, Result } when is_record( NewState, state_holder ) ->
			{ NewState, { wooper_result, Result } };

		% Neither a oneway or request result, nor an exception: faulty return.
		Other ->

			MethodArity = length( Parameters ) + 1,

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					wooper:log_error( "oneway ~ts:~ts/~B made a faulty return "
					  "'~p', parameters were:~n~p",
					  [ SelectedModule, MethodAtom, MethodArity, Other,
						Parameters ] ),

					throw( { wooper_oneway_faulty_return, self(),
							 SelectedModule, MethodAtom, MethodArity,
							 Parameters, Other } );

				_ ->

					% This is a request, log and throw, the try/catch clause of
					% the main loop will intercept it, log and rethrow:
					%
					wooper:log_error( "request ~ts:~ts/~B made a faulty return "
					  "'~p', parameters were:~n~p",
					  [ SelectedModule, MethodAtom, MethodArity, Other,
						Parameters ] ),

					% We do not include anymore 'Other', as it is best kept
					% internal (and not sent back to the caller):
					%
					throw( { wooper_request_faulty_return, self(),
							 SelectedModule, MethodAtom, MethodArity,
							 Parameters } )

			end

	end.



-else. % not in wooper_debug_mode:



wooper_effective_method_execution( SelectedModule, MethodAtom, State,
								   Parameters ) ->

	%trace_utils:debug_fmt( "WOOPER: effective execution of ~p:~p.~n",
	%					   [ SelectedModule, MethodAtom ] ),

	case apply( SelectedModule, MethodAtom, [ State | Parameters ] ) of

		% Method returning a result (and a state of course) - hence a request:
		% ?wooper_return_state_result:
		%
		{ NewState, Result }  ->
			{ NewState, { wooper_result, Result } };

		% Void method (no result returned, only a state) - thus a oneway:
		% ?wooper_return_state:
		NewState ->
			{ NewState, wooper_method_returns_void }

	end.


-endif. % not wooper_debug_mode.



%
% Macro-section for request handling.
%



% Section for wooper_handle_remote_request_execution/4.



% Executes the specified remotely-triggered request: returns an updated state
% and sends back the result to the caller.
%
% A specific function is used for *remote* request execution, as local ones have
% a different structure (they return the result, they do not send it; they do
% not intercept exceptions, etc.).
%
-spec wooper_handle_remote_request_execution( method_name(), wooper:state(),
						method_arguments(), pid() ) -> wooper:state().


-compile( { inline, [ wooper_handle_remote_request_execution/4 ] } ).


-ifdef(wooper_debug_mode).


% In debug mode, we perform additional checkings:
wooper_handle_remote_request_execution( RequestAtom, State, ArgumentList,
										CallerPid ) ->

	%trace_utils:debug( "case A" ),

	SenderAwareState = case State#state_holder.request_sender of

		% Normal case:
		undefined when is_pid( CallerPid ) ->
			State#state_holder{ request_sender=CallerPid };

		UnexpectedSender ->
			throw( { request_with_invalid_sender, UnexpectedSender } )

	end,

	% Only case where (all) exceptions shall be caught, as we need to send back
	% a result to the caller:
	%
	RequestState = try

	  wooper_execute_method( RequestAtom, ArgumentList, SenderAwareState ) of

		% Most likely case:
		{ ExecState, Outcome={ wooper_result, _Result } } ->
			CallerPid ! Outcome,
			ExecState;

		{ _ExecState, wooper_method_returns_void } ->
			wooper:log_error( "method ~ts:~ts/~B, which was called (by ~w) "
				"with parameters ~p, did not return a result whereas, "
				"according to its call, it was expected to be a request.~n"
				"Either the request implementation is incorrect or it is a "
				"oneway that has been incorrectly called as a request.",
				[ State#state_holder.actual_class, RequestAtom,
				  length( ArgumentList ) + 1, CallerPid, ArgumentList ],
							  State ),

			ErrorReason = { request_void_return, self(),
							State#state_holder.actual_class, RequestAtom,
							length( ArgumentList ) + 1, ArgumentList },

			CallerPid ! { wooper_error, ErrorReason },

			% Mismatch used to be ignored, but now throwing an exception is
			% believed more reasonable as safer:
			%
			%ExecState
			throw( { wooper_error, ErrorReason } )

	catch

		Reason:ErrorTerm:Stacktrace ->

			% Reports it (to console and caller), and exits:
			wooper:on_failed_request( RequestAtom, ArgumentList, CallerPid,
									  Reason, ErrorTerm, Stacktrace, State )

	end,

	RequestState#state_holder{ request_sender=undefined }.


-else. % wooper_debug_mode


% Not in debug mode, hence minimum checking:
wooper_handle_remote_request_execution( RequestAtom, State, ArgumentList,
										CallerPid ) ->

	%trace_utils:debug( "case B" ),

	SenderAwareState = State#state_holder{ request_sender=CallerPid },

	% Result assumed to be correct here:
	RequestState = try

	  wooper_execute_method( RequestAtom, ArgumentList, SenderAwareState ) of

		{ ExecState, Outcome } ->
			CallerPid ! Outcome,
			ExecState

		% Request/oneway mismatches not supposed to happen in non-debug mode.

	catch

		Reason:ErrorTerm:StackTrace ->

			% Reports it (to console and caller), and exits:
			wooper:on_failed_request( RequestAtom, ArgumentList, CallerPid,
									  Reason, ErrorTerm, StackTrace, State )

	end,

	RequestState#state_holder{ request_sender=undefined }.


-endif. % wooper_debug_mode




% Section for wooper_handle_local_request_execution/3.


% Executes the specified locally-triggered request: returns an updated state and
% the corresponding result.
%
% A specific function is used for *local* request executions, as they must have
% a different structure (ex: not sending messages back, restoring the previous
% request_sender, not catching exceptions).
%
-spec wooper_handle_local_request_execution( method_name(), wooper:state(),
		method_arguments() ) -> { wooper:state(), method_internal_result() }.


-compile( { inline, [ wooper_handle_local_request_execution/3 ] } ).


-ifdef(wooper_debug_mode).


% In debug mode, we perform additional checkings:
wooper_handle_local_request_execution( RequestAtom, State, ArgumentList ) ->

	% Due to nesting, can be licitly 'undefined' or a PID:
	PreviousRequestSender = State#state_holder.request_sender,

	SenderAwareState = State#state_holder{ request_sender=self() },

	% Not using 'try', as we have to let exceptions propagate:
	{ RequestState, Result } =
		wooper_execute_method( RequestAtom, ArgumentList, SenderAwareState ),

	% Forces a crash if instance-side error detected:
	ActualResult = case Result of

		{ wooper_result, R } ->
			R;

		wooper_method_returns_void ->
			wooper:log_error( "method ~ts/~B, which was called with "
				"parameters ~p, did not return a result whereas, according to "
				"its call, it was expected to be a request.~n"
				"Either the request implementation is incorrect or it is a "
				"oneway that has been incorrectly called as a request.",
				[ RequestAtom, length( ArgumentList ) + 1, ArgumentList ],
				  State ),

			throw( { oneway_request_mismatch, RequestAtom, ArgumentList } )

	end,

	ReturnedState = RequestState#state_holder{
						request_sender=PreviousRequestSender },

	{ ReturnedState, ActualResult }.


-else. % wooper_debug_mode


% Not in debug mode, hence minimum checking:
wooper_handle_local_request_execution( RequestAtom, State, ArgumentList ) ->

	% Due to nesting, can be licitly 'undefined' or a PID:
	PreviousRequestSender = State#state_holder.request_sender,

	SenderAwareState = State#state_holder{ request_sender=self() },

	{ RequestState, { wooper_result, ActualResult } } =
		wooper_execute_method( RequestAtom, ArgumentList, SenderAwareState ),


	ReturnedState = RequestState#state_holder{
					  request_sender=PreviousRequestSender },

	{ ReturnedState, ActualResult }.


-endif. % wooper_debug_mode




% Section for wooper_handle_local_request_execution_as/4.



% Executes the specified locally-triggered request, using an explicit class to
% select its implementation: returns an updated state and the corresponding
% result.
%
% Only local calls can select their implementation class.
%
-spec wooper_handle_local_request_execution_as( method_name(), wooper:state(),
		method_arguments(), classname() ) ->
								{ wooper:state(), method_internal_result() }.


-compile( { inline, [ wooper_handle_local_request_execution_as/4 ] } ).


-ifdef(wooper_debug_mode).


% In debug mode, we perform additional checkings:
wooper_handle_local_request_execution_as( RequestAtom, State, ArgumentList,
										  Classname ) ->

	% Due to nesting, can be licitly 'undefined' or a PID:
	PreviousRequestSender = State#state_holder.request_sender,

	SenderAwareState = State#state_holder{ request_sender=self() },

	% Local request, hence no try/catch:
	{ RequestState, Result } = wooper_execute_method_as( Classname,
								RequestAtom, ArgumentList, SenderAwareState ),

	ActualResult = case Result of

		{ wooper_result, R } ->
			R;

		wooper_method_returns_void ->
			wooper:log_error( "method explicitly called as ~ts:~ts/~B, "
				"which was called with parameters ~p, "
				"did not return a result whereas, according to "
				"its call, it was expected to be a request.~n"
				"Either the request implementation is incorrect or it is a "
				"oneway that has been incorrectly called as a request.",
				[ Classname, RequestAtom, length( ArgumentList ) + 1,
				  ArgumentList ], State ),
			throw( { oneway_request_mismatch, RequestAtom, ArgumentList } )

	end,

	ReturnedState = RequestState#state_holder{
					  request_sender=PreviousRequestSender },

	{ ReturnedState, ActualResult }.


-else. % wooper_debug_mode


% Not in debug mode, hence minimum checking:
wooper_handle_local_request_execution_as( RequestAtom, State, ArgumentList,
											Classname ) ->

	% Due to nesting, can be licitly 'undefined' or a PID:
	PreviousRequestSender = State#state_holder.request_sender,

	SenderAwareState = State#state_holder{ request_sender=self() },

	{ RequestState, { wooper_result, ActualResult } } =
		wooper_execute_method_as( Classname, RequestAtom, ArgumentList,
								  SenderAwareState ),

	ReturnedState = RequestState#state_holder{
						request_sender=PreviousRequestSender },

	{ ReturnedState, ActualResult }.


-endif. % wooper_debug_mode




%
% Macro-section for oneway handling.
%




% Section for wooper_handle_remote_oneway_execution/3.



% Executes the specified remotely-triggered oneway, and returns an updated
% state.
%
-spec wooper_handle_remote_oneway_execution( method_name(), wooper:state(),
								method_arguments() ) -> wooper:state().


-compile( { inline, [ wooper_handle_remote_oneway_execution/3 ] } ).


-ifdef(wooper_debug_mode).


% In debug mode, we perform additional checkings:
wooper_handle_remote_oneway_execution( OnewayAtom, State, ArgumentList ) ->

	% We rely here on the property that, in-between two calls, request_sender is
	% set to 'undefined', so it does not need to be updated for oneways - yet we
	% check here:
	%
	undefined = State#state_holder.request_sender,

	% We intercept exceptions only when called remotely, and in order to
	% generate better error messages:
	%
	OnewayState = try wooper_execute_method( OnewayAtom, ArgumentList,
											 State ) of

		% This is the normal, most likely, expected case:
		{ ExecState, wooper_method_returns_void } ->

			%trace_utils:debug( "Normal oneway return." ),

			% Just an additional checking that it was not changed
			% (post-condition):
			undefined = ExecState#state_holder.request_sender,
			ExecState;


		% This is a oneway/request mismatch apparently:
		{ _OnewayState, { wooper_result, UnexpectedResult } } ->

			%trace_utils:error( "Incorrect, request-like oneway return." ),

			Class = State#state_holder.actual_class,
			Arity = length( ArgumentList ) + 1,

			wooper:log_error( "method ~ts:~ts/~B, which was called with "
				"following parameters:~n~p~n returned a result (~p) whereas, "
				"according to its call, it was expected to be a oneway.~n"
				"So either the oneway implementation of ~ts:~ts/~B is "
				"incorrect, or it is actually a request that has been "
				"incorrectly called as a oneway.",
				[ Class, OnewayAtom, Arity, ArgumentList,
				  UnexpectedResult, Class, OnewayAtom, Arity ], State ),

			% No wooper_error can be returned (caller not known).

			throw( { oneway_request_mismatch, OnewayAtom, ArgumentList } )

	catch

		Reason:ErrorTerm:StackTrace ->

			% Reports it to console and exits:
			wooper:on_failed_oneway( OnewayAtom, ArgumentList, Reason,
									 ErrorTerm, StackTrace, State )

	end,

	% Post-condition:
	undefined = OnewayState#state_holder.request_sender,

	OnewayState.



-else. % wooper_debug_mode


% Not in debug mode, hence minimum checking:
wooper_handle_remote_oneway_execution( OnewayAtom, State, ArgumentList ) ->

	% We rely here on the property that, in-between two calls, request_sender is
	% set to 'undefined', so it does not need to be updated for oneways.

	% Result expected to be 'wooper_method_returns_void' here:
	OnewayState = try wooper_execute_method( OnewayAtom, ArgumentList,
											 State ) of

		{ ExecState, _Result } ->
			ExecState

	 catch

		 Reason:ErrorTerm:Stacktrace ->

			% Reports it to console and exits:
			 wooper:on_failed_oneway( OnewayAtom, ArgumentList, Reason,
									  ErrorTerm, Stacktrace, State )

	end,

	OnewayState.

-endif. % wooper_debug_mode






% Section for wooper_handle_local_oneway_execution/3.

% Executes the specified locally-triggered oneway, and returns an updated state.
-spec wooper_handle_local_oneway_execution( method_name(), wooper:state(),
								method_arguments() ) -> wooper:state().


-compile( { inline, [ wooper_handle_local_oneway_execution/3 ] } ).


-ifdef(wooper_debug_mode).


% In debug mode, we perform additional checkings:
wooper_handle_local_oneway_execution( OnewayAtom, State, ArgumentList ) ->

	% Due to nesting, can be licitly 'undefined' or a PID:
	PreviousRequestSender = State#state_holder.request_sender,

	SenderAwareState = State#state_holder{ request_sender=undefined },

	case wooper_execute_method( OnewayAtom, ArgumentList, SenderAwareState ) of


		% This is the normal, expected case:
		{ OnewayState, wooper_method_returns_void } ->
			% Just an additional checking that it was not changed
			% (post-condition):
			undefined = OnewayState#state_holder.request_sender,
			OnewayState#state_holder{ request_sender=PreviousRequestSender };


		% This is a oneway/request mismatch apparently:
		{ _OnewayState, { wooper_result, UnexpectedResult } } ->

			wooper:log_error( "method ~ts/~B, which was called with "
				"parameters ~p, returned a result (~p) whereas, according to "
				"its call, it was expected to be a oneway.~n"
				"Either the oneway implementation is incorrect "
				"or it is a request that has been incorrectly "
				"called as a oneway.",
				[ OnewayAtom, length( ArgumentList ) + 1, ArgumentList,
				  UnexpectedResult ], State ),

			throw( { oneway_request_mismatch, OnewayAtom, ArgumentList } )

	end.



-else. % wooper_debug_mode


% Not in debug mode, hence minimum checking:
wooper_handle_local_oneway_execution( OnewayAtom, State, ArgumentList ) ->

	% Due to nesting, can be licitly 'undefined' or a PID:
	PreviousRequestSender = State#state_holder.request_sender,

	SenderAwareState = State#state_holder{ request_sender=undefined },

	% Result expected to be 'wooper_method_returns_void' here:
	{ OnewayState, _Result } =
		wooper_execute_method( OnewayAtom, ArgumentList, SenderAwareState ),

	OnewayState#state_holder{ request_sender=PreviousRequestSender }.


-endif. % wooper_debug_mode







% Section for wooper_handle_local_oneway_execution_as/4.


% Executes the specified locally-triggered oneway, using an explicit class to
% select its implementation, and returns an updated state.
%
% No 'when is_list(ArgumentList) -> ...' as the caller must have ensured that
% we have already a list.
%
-spec wooper_handle_local_oneway_execution_as( method_name(), wooper:state(),
					method_arguments(), classname() ) -> wooper:state().


-compile( { inline, [ wooper_handle_local_oneway_execution_as/4 ] } ).


-ifdef(wooper_debug_mode).


% In debug mode, we perform additional checkings:
wooper_handle_local_oneway_execution_as( OnewayAtom, State, ArgumentList,
										 Classname ) ->

	% Due to nesting, can be licitly 'undefined' or a PID:
	PreviousRequestSender = State#state_holder.request_sender,

	SenderAwareState = State#state_holder{ request_sender=undefined },

	case wooper_execute_method_as( Classname, OnewayAtom,
								   ArgumentList, SenderAwareState ) of


		% This is the normal, expected case:
		{ OnewayState, wooper_method_returns_void } ->
			% Just an additional checking that it was not changed
			% (post-condition):
			%
			undefined = OnewayState#state_holder.request_sender,
			OnewayState#state_holder{ request_sender=PreviousRequestSender };


		% This is a oneway/request mismatch apparently:
		{ _OnewayState, { wooper_result, UnexpectedResult } } ->
			wooper:log_error( "method explicitly called as ~ts:~ts/~B, "
				"which was called with parameters ~p, "
				"returned a result (~p) whereas, according to "
				"its call, it was expected to be a oneway.~n"
				"Either the oneway implementation is incorrect "
				"or it is a request that has been incorrectly "
				"called as a oneway.",
				[ Classname, OnewayAtom, length( ArgumentList ) + 1,
				  ArgumentList, UnexpectedResult ], State ),

			throw( { oneway_request_mismatch, OnewayAtom, ArgumentList } )

	end.


-else. % wooper_debug_mode


% Not in debug mode, hence minimum checking:
wooper_handle_local_oneway_execution_as( OnewayAtom, State, ArgumentList,
										   Classname ) ->

	% Due to nesting, can be licitly 'undefined' or a PID:
	PreviousRequestSender = State#state_holder.request_sender,

	SenderAwareState = State#state_holder{ request_sender=undefined },

	% Result expected to be 'wooper_method_returns_void' here:
	{ OnewayState, _Result } = wooper_execute_method_as( Classname,
								OnewayAtom, ArgumentList, SenderAwareState ),

	OnewayState#state_holder{ request_sender=PreviousRequestSender }.


-endif. % wooper_debug_mode
