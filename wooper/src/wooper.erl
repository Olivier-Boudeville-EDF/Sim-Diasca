% Copyright (C) 2012-2021 Olivier Boudeville
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


% Module containing some facilities for WOOPER users, better placed in a
% standalone module than duplicated in each class.
%
-module(wooper).



% First, all the various exports:


% Very generic:
-export([ get_classname/1, get_attribute_pairs/1, state_to_string/1,
		  get_class_filename/1 ]).


% Settings helpers:
-export([ get_synchronous_time_out/1 ]).



% Single/multi call communication helpers for active instances:
-export([ execute_request/3, execute_request/4, send_requests/3,

		  send_request_in_turn/3, send_request_in_turn/4,

		  send_requests_and_wait_acks/4, send_requests_and_wait_acks/5,
		  send_acknowledged_oneway_in_turn/5,
		  wait_for_request_answers/2, wait_for_request_answers/3,
		  wait_for_request_acknowledgements/2,
		  wait_for_request_acknowledgements/3,

		  obtain_results_for_requests/3,

		  send_request_series/2, obtain_results_for_request_series/2,

		  send_and_listen/3, receive_result/0 ]).



% Creation helpers:
-export([ create_hosting_process/2,
		  construct_and_run/2, construct_and_run_synchronous/3,
		  construct_passive/2 ]).



% Destruction helpers:
-export([ delete_any_instance_referenced_in/2,
		  delete_synchronously_any_instance_referenced_in/2,
		  safe_delete_synchronously_any_instance_referenced_in/2,
		  delete_synchronously_instance/1, delete_synchronously_instances/1,
		  safe_delete_synchronously_instances/1,
		  delete_passive/1 ]).



% Method execution for passive instances:
-export([ execute_request/2, % already exported: execute_request/3,
		  execute_const_request/2, execute_const_request/3,
		  execute_oneway/2, execute_oneway/3,
		  execute_const_oneway/2, execute_const_oneway/3 ]).


% Infrequently-called functions for state management:
-export([ get_all_attributes/1, check_undefined/2, check_all_undefined/2 ]).


% Extra features:
-export([ declare_beam_dirs_for_wooper/0, retrieve_virtual_table_key/1,
		  get_execution_target/0 ]).



% Traps to detect any method terminator that would be left untransformed.
%
% Note: neither exported or even defined anymore, as now WOOPER checks at
% compile time whether a given wooper:SomeFun(...) call is a terminator.
%
-export([ return_state_result/2, return_state/1, return_static/1,
		  const_return_result/1, const_return/0 ]).



% For log and error reporting:
-export([ log_info/1, log_info/2,
		  log_warning/1, log_warning/2,
		  log_error/1, log_error/2, log_error/3,
		  on_failed_request/7, on_failed_oneway/6 ]).


-ifdef(wooper_debug_mode).

% State-related helpers (only available in debug mode):
-export([ virtual_table_to_string/1, instance_to_string/1,
		  display_state/1, display_virtual_table/1, display_instance/1 ]).

-else. % wooper_debug_mode

% Exported as otherwise reported as unused:
-export([ check_classname_and_arity/2 ]).

-endif. % wooper_debug_mode



% Basics:
-export([ default_exit_handler/3, default_down_handler/5,
		  default_node_up_handler/3, default_node_down_handler/3 ]).


% To allow for finer checking:
-export([ get_exported_functions_set/0 ]).


% The record defining the state of a passive instance:
-define( passive_record, state_holder ).



% At least for error cases, ellipsing traces is not a good idea; it may be done
% later in the trace chain, by the actual logger itself (the default here is to
% ellipse traces of lesser severity, supposing a basic console-like logger):
%
-ifdef(wooper_unellipsed_traces).

	% Disables the ellipsing of traces (typically if having a suitable trace
	% handler):
	%
	-define( ellipse_length, unlimited ).

-else. % wooper_unellipsed_traces

	% Default:
	-define( ellipse_length, 2000 ).

-endif. % wooper_unellipsed_traces



% For the name of the registered process that keeps the per-class method
% hashtables:
%
-include("wooper_class_manager.hrl").


% For synchronous_time_out, state_holder and al:
-include("wooper_defines_exports.hrl").


% Actual definitions the shorthands in wooper_types_exports.hrl refer to:


% An atom prefixed with 'class_':
-type classname() :: atom().


% The key in the persistent_term registry corresponding to a class, to fetch its
% virtual table:
%
-type class_key() :: term().



% A method name (ex: 'setColor'):
-type method_name() :: meta_utils:function_name().


% Name of a request method:
-type request_name() :: method_name().

% Name of a oneway method:
-type oneway_name()  :: method_name().

% Name of a static method:
-type static_name()  :: method_name().


% Arity of a method:
-type method_arity() :: meta_utils:function_arity().

-type request_arity() :: method_arity().
-type oneway_arity() ::  method_arity().
-type static_arity() ::  method_arity().


-type method_id() :: { method_name(), method_arity() }.

-type request_id() :: { request_name(), request_arity() }.
-type oneway_id() ::  { oneway_name(),  oneway_arity() }.
-type static_id() ::  { static_name(),  static_arity() }.


% Access qualifier, applying either to a method or to an attribute:
-type access_qualifier() :: 'public' | 'protected' | 'private'.


% A method argument can be any type:
-type method_argument() :: any().

% Standalone (non-list) arguments may also be specified:
-type method_arguments() :: method_argument() | [ method_argument() ].


% Qualifiers applying to methods:
-type method_qualifier() :: access_qualifier()

							% This method cannot be overridden:
						  | 'final'

							% This method does not change the state of the
							% instance it is applied on:
							%
							% (only meaningful for requests and oneways)
							%
						  | 'const'.


% The qualifiers applying to a method:
-type method_qualifiers() :: [ method_qualifier() ].


% Special case of construction parameters:
-type construction_parameter() :: method_argument().

-type construction_parameters() :: [ construction_parameter() ].


-type requests_outcome() :: 'success' | { 'failure', [ pid() ] }.

% To be specified more closely maybe:
-type method_internal_result() :: any().

% To describe all kinds of results:

-type request_result( T ) :: T.
-type request_result() :: request_result( any() ).

-type static_result( T ) :: T.
-type static_result() :: static_result( any() ).




% For method specs:

-type request_return( T ) :: { state(), request_result( T ) }.
-type const_request_return( T ) :: request_result( T ).

-type oneway_return() :: state().
-type const_oneway_return() :: void().

-type static_return( T ) :: static_result( T ).

% Constness irrelevant for static methods.



-type attribute_name() :: atom().
-type attribute_value() :: any().

-type attribute_entry() :: { attribute_name(), attribute_value() }.

% The type-as-a-term of an attribute:
-type attribute_type() :: type_utils:type().


% Qualifiers applying to attributes:
-type attribute_qualifier() ::
		% The initial value of that attribute cannot be modified:
		'const'.


% PID of a WOOPER instance.
-type instance_pid() :: pid().


% Passive instance:
-type passive_instance() :: state().


% A term (often an atom) used to denote an acknowlegment (i.e. a conventional
% symbol to ensure that a request was synchronously executed):
%
-type ack_term() :: term().


% PID of a process (not necessarily a WOOPER instance) interacting with such an
% instance:
%
% (clearer that just pid())
%
-type caller_pid() :: pid().

-type request_call() :: { request_name(), method_arguments(), caller_pid() }.
-type oneway_call()  :: { oneway_name(), method_arguments() } | oneway_name().


% Otherwise wooper_execute_method_as/4 is unused:
-include("wooper_execute_internal_exports.hrl").


%-opaque state() :: #state_holder{}.
-type state() :: #state_holder{}.


% Allows to record the functions exported by a module (typically the 'wooper'
% one):
%
-type function_export_set() :: set_utils:set( meta_utils:function_id() ).


% We prefer having it prefixed by wooper:
-export_type([ classname/0, class_key/0,
			   method_name/0, request_name/0, oneway_name/0, static_name/0,
			   method_arity/0,
			   method_id/0, request_id/0, oneway_id/0, static_id/0,
			   access_qualifier/0,
			   method_argument/0, method_arguments/0,
			   method_qualifier/0, method_qualifiers/0,
			   construction_parameter/0, construction_parameters/0,
			   requests_outcome/0, method_internal_result/0,

			   request_result/1, request_result/0,
			   static_result/1, static_result/0,

			   request_return/1, const_request_return/1,
			   oneway_return/0, const_oneway_return/0,
			   static_return/1,

			   attribute_name/0, attribute_value/0, attribute_entry/0,
			   attribute_type/0, attribute_qualifier/0,
			   instance_pid/0, passive_instance/0,
			   caller_pid/0, request_call/0, oneway_call/0,
			   state/0, function_export_set/0 ]).


% Shorthands:

-type count() :: basic_utils:count().
-type error_type() :: basic_utils:error_type().
-type error_term() :: basic_utils:error_term().
-type exit_reason() :: basic_utils:exit_reason().

-type ustring() :: text_utils:ustring().
-type format_string() :: text_utils:format_string().

-type stack_trace() :: code_utils:stack_trace().
-type stack_location() :: code_utils:stack_location().
-type stack_item() :: code_utils:stack_item().

% In milliseconds, if finite:
-type time_out() :: time_utils:time_out().


% Note that the {attribute, request, oneway, static, class}_info/0 types are
% exported from wooper_info.


% For getAttr and al:
-include("wooper_state_exports.hrl").


% Otherwise executeRequest/3 and all reported as unused:
-include("wooper_execute_exports.hrl").

%-include("wooper_execute_internal_exports.hrl").


% To define get_execution_target/0:
-include_lib("myriad/include/utils/basic_utils.hrl").



% Now, function definitions:


% For getAttribute/2 and al:
-include("wooper_state_functions.hrl").

% For executeRequest/2:
-include("wooper_execute_functions.hrl").

% For wooper_execute_method/3:
-include("wooper_execute_internal_functions.hrl").

% For get_superclasses/0:
%-include("wooper_classes_functions.hrl").


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").



% Section for communication helpers.
%
% Generally no wooper result expected to be already in the message queue or to
% be received during these operations.



% A WOOPER request execution (hence a synchronous call) not yielding a result in
% specified duration (in milliseconds) will lead to a notification to be
% displayed on the console (yet the result will still be waited for
% indefinitively)
%
-ifdef(wooper_debug_mode).

	-define( notify_long_wait_after, 2500 ).

-else. % wooper_debug_mode

	-define( notify_long_wait_after, 60000 ).

-endif. % wooper_debug_mode



% First, just a simple shorthand: one caller, one request, one result, waited
% indefinitively.



% Sends specified request to specified (active or passive) instance; if active,
% waits indefinitively for its returned value (supposing none is already waiting
% among the received messages), and returns it.
%
% (public helper, as a convenience wrapper for passive instances)
%
-spec execute_request( instance_pid(), request_name() ) -> request_result();
					 ( passive_instance(), request_name() ) ->
							{ passive_instance(), method_internal_result() }.
execute_request( TargetInstancePID, RequestName )
  when is_pid( TargetInstancePID ) andalso is_atom( RequestName ) ->

	RequestArgs = [],

	TargetInstancePID ! { RequestName, RequestArgs, self() },

	execute_request_waiter( TargetInstancePID, RequestName, RequestArgs );


execute_request( PassiveInstance, RequestName )
  when is_record( PassiveInstance, ?passive_record )
	   andalso is_atom( RequestName ) ->

	{ NewPassiveInstance, { wooper_result, R } } =
		wooper_execute_method( RequestName, _RequestArgs=[], PassiveInstance ),

	{ NewPassiveInstance, R }.



% Sends specified request to specified (active or passive) instance; if active,
% waits indefinitively for its returned value (supposing none is already waiting
% among the received messages), and returns it.
%
% (public helper, as a convenience wrapper for passive instances)
%
-spec execute_request( instance_pid(), request_name(), method_arguments() ) ->
							 request_result();
					 ( passive_instance(), request_name(),
					   method_arguments() ) ->
							 { passive_instance(), method_internal_result() }.
execute_request( TargetInstancePID, RequestName, RequestArgs )
  when is_pid( TargetInstancePID ) andalso is_atom( RequestName ) ->

	TargetInstancePID ! { RequestName, RequestArgs, self() },

	execute_request_waiter( TargetInstancePID, RequestName, RequestArgs );

execute_request( PassiveInstance, RequestName, RequestArgs )
  when is_record( PassiveInstance, ?passive_record )
	   andalso is_atom( RequestName ) ->

	{ NewPassiveInstance, { wooper_result, R } } =
		wooper_execute_method( RequestName, RequestArgs, PassiveInstance ),

	{ NewPassiveInstance, R }.



% (helper)
execute_request_waiter( TargetInstancePID, RequestName, RequestArgs ) ->

	receive

		{ wooper_result, Res } ->
			Res

	after ?notify_long_wait_after ->

		trace_bridge:warning_fmt( "Still awaiting an answer from WOOPER "
			"instance ~p, after having called request '~ts' on it with "
			"following parameters:~n~p",
			[ TargetInstancePID, RequestName, RequestArgs ] ),

		execute_request_waiter( TargetInstancePID, RequestName, RequestArgs )

	end.



% Sends specified request to specified instance and waits indefinitively for its
% specified, expected returned value (supposing none is already waiting among
% the received messages) that is usually an atom.
%
% (helper)
%
-spec execute_request( instance_pid(), request_name(), method_arguments(),
					   method_internal_result() ) -> void().
execute_request( TargetInstancePID, RequestName, RequestArgs,
				 ExpectedResult ) ->

	TargetInstancePID ! { RequestName, RequestArgs, self() },

	execute_request_waiter( ExpectedResult, TargetInstancePID, RequestName,
							RequestArgs ).



% (helper)
execute_request_waiter( ExpectedResult, TargetInstancePID, RequestName,
						RequestArgs ) ->

	receive

		{ wooper_result, ExpectedResult } ->
			ok

	after ?notify_long_wait_after ->

		trace_bridge:warning_fmt( "Still awaiting the expected answer '~p' "
			"from WOOPER instance ~p, after having called request '~ts' on it "
			"with following parameters:~n~p",
			[ ExpectedResult, TargetInstancePID, RequestName, RequestArgs ] ),

		execute_request_waiter( ExpectedResult, TargetInstancePID,
								RequestName, RequestArgs )

	end.



% Triggers the specified const request on the specified passive instance, and
% returns it this result, knowing that the state of this passive instance is
% expected to remain the same (and thus will not be returned).
%
% Note: the called method is checked for constness only in debug mode.
%
% (public helper, as a convenience wrapper for passive instances)
%
-spec execute_const_request( passive_instance(), request_name() ) ->
								method_internal_result().

-ifdef(wooper_debug_mode).

execute_const_request( PassiveInstance, RequestName )
  when is_record( PassiveInstance, ?passive_record )
	   andalso is_atom( RequestName ) ->

	% Matching PassiveInstance:
	{ PassiveInstance, { wooper_result, Res } } =
		wooper_execute_method( RequestName, _RequestArgs=[], PassiveInstance ),

	Res.

-else. % wooper_debug_mode

execute_const_request( PassiveInstance, RequestName )
  when is_record( PassiveInstance, ?passive_record )
	   andalso is_atom( RequestName ) ->

	{ _ExpectedSamePassiveInstance, { wooper_result, Res } } =
		wooper_execute_method( RequestName, _RequestArgs=[], PassiveInstance ),

	Res.

-endif. % wooper_debug_mode



% Sends specified const request to specified passive instance, and returns it
% this result, knowing that the state of this passive instance is expected to
% remain the same (and thus will not be returned).
%
% Note: the called method is checked for constness only in debug mode.
%
% (public helper, as a convenience wrapper for passive instances)
%
-spec execute_const_request( passive_instance(), request_name(),
							 method_arguments() ) -> method_internal_result().
-ifdef(wooper_debug_mode).

execute_const_request( PassiveInstance, RequestName, RequestArgs )
  when is_record( PassiveInstance, ?passive_record )
	   andalso is_atom( RequestName ) ->

	% Matching PassiveInstance:
	{ PassiveInstance, { wooper_result, Res } } =
		wooper_execute_method( RequestName, RequestArgs, PassiveInstance ),

	Res.

-else. % wooper_debug_mode

execute_const_request( PassiveInstance, RequestName, RequestArgs )
  when is_record( PassiveInstance, ?passive_record )
	   andalso is_atom( RequestName ) ->

	{ _ExpectedSamePassiveInstance, { wooper_result, Res } } =
		wooper_execute_method( RequestName, RequestArgs, PassiveInstance ),

	Res.

-endif. % wooper_debug_mode



% Triggers the specified const oneway on the specified passive instance, and
% returns nothing at all, knowing that the state of this passive instance is
% expected to remain the same (and thus will not be returned).
%
% Note: the called method is checked for constness only in debug mode.
%
% (public helper, as a convenience wrapper for passive instances)
%
-spec execute_const_oneway( passive_instance(), oneway_name() ) -> void().

-ifdef(wooper_debug_mode).

execute_const_oneway( PassiveInstance, OnewayName )
  when is_record( PassiveInstance, ?passive_record )
	   andalso is_atom( OnewayName ) ->

	% Matching PassiveInstance:
	{ PassiveInstance, wooper_method_returns_void } =
		wooper_execute_method( OnewayName, _OnewayArgs=[], PassiveInstance ).

-else. % wooper_debug_mode

execute_const_oneway( PassiveInstance, OnewayName )
  when is_record( PassiveInstance, ?passive_record )
	   andalso is_atom( OnewayName ) ->

	% Reckless:
	%{ _ExpectedSamePassiveInstance, wooper_method_returns_void } =
	wooper_execute_method( OnewayName, _OnewayArgs=[], PassiveInstance ).

-endif. % wooper_debug_mode



% Sends specified const oneway to specified passive instance, and returns
% nothing at all, knowing that the state of this passive instance is expected to
% remain the same (and thus will not be returned).
%
% Note: the called method is checked for constness only in debug mode.
%
% (public helper, as a convenience wrapper for passive instances)
%
-spec execute_const_oneway( passive_instance(), oneway_name(),
							method_arguments() ) -> void().

-ifdef(wooper_debug_mode).

execute_const_oneway( PassiveInstance, OnewayName, OnewayArgs )
  when is_record( PassiveInstance, ?passive_record )
	   andalso is_atom( OnewayName ) ->

	% Matching PassiveInstance:
	{ PassiveInstance, wooper_method_returns_void } =
		wooper_execute_method( OnewayName, OnewayArgs, PassiveInstance ).

-else. % wooper_debug_mode

execute_const_oneway( PassiveInstance, OnewayName, OnewayArgs )
  when is_record( PassiveInstance, ?passive_record )
	   andalso is_atom( OnewayName ) ->

	% Reckless:
	%{ _ExpectedSamePassiveInstance, wooper_method_returns_void } =
	wooper_execute_method( OnewayName, OnewayArgs, PassiveInstance ).

-endif. % wooper_debug_mode




% Second: one caller, one request, multiple callees, sequentially or in
% parallel, with a time-out or not.


% Triggers a request in turn on the specified series of instances, with no
% overlapping of their processing, and returns their respective, ordered
% results.
%
% No time-out applies: blocks indefinitely if an instance fails to answer.
%
-spec send_request_in_turn( request_name(), method_arguments(),
							[ instance_pid() ] ) -> [ request_result() ].
send_request_in_turn( RequestName, RequestArgs, TargetInstancePIDs ) ->

	trace_utils:debug_fmt( "Sending request '~ts' (no time-out) in turn to ~B "
		"instances (~p), with arguments ~p .",
		[ RequestName, length( TargetInstancePIDs ), TargetInstancePIDs,
		  RequestArgs ] ),

	send_request_in_turn( RequestName, RequestArgs, TargetInstancePIDs,
						  _AccRes=[], _Timeout=infinity ).



% Triggers a request in turn on the specified series of instances, with no
% overlapping of their processing, and returns their respective, ordered
% results.
%
% Throws an exception if an instance fails to answer within specified time-out.
%
-spec send_request_in_turn( request_name(), method_arguments(),
		[ instance_pid() ], time_out() ) -> [ request_result() ].
send_request_in_turn( RequestName, RequestArgs, TargetInstancePIDs, Timeout ) ->

	trace_utils:debug_fmt( "Sending request '~ts', with a ~ts, in turn "
		"to ~B instances (~p), with arguments ~p.",
		[ RequestName, time_utils:time_out_to_string( Timeout ),
		  length( TargetInstancePIDs ), TargetInstancePIDs, RequestArgs ] ),

	send_request_in_turn( RequestName, RequestArgs, TargetInstancePIDs,
						  _AccRes=[], Timeout ).


% (helper)
send_request_in_turn( _RequestName, _RequestArgs, _TargetInstancePIDs=[],
					  AccRes, _Timeout ) ->
	lists:reverse( AccRes );

send_request_in_turn( RequestName, RequestArgs,
			  _TargetInstancePIDs=[ InstancePid | H ], AccRes, Timeout ) ->

	InstancePid ! { RequestName, RequestArgs, self() },

	trace_utils:debug_fmt( "Sent request '~ts' to ~w, waiting for result.",
						   [ RequestName, InstancePid ] ),

	receive

		{ wooper_result, R } ->
			trace_utils:debug_fmt( "For request '~ts' sent to ~w, "
				"got following result: ~p.", [ RequestName, InstancePid, R ] ),
			send_request_in_turn( RequestName, RequestArgs, H, [ R | AccRes ],
								  Timeout )

	after Timeout ->

		trace_utils:error_fmt( "Time-out reached (~ts) for call of "
			"request '~ts' with arguments ~p to instance ~w.",
			[ time_utils:time_out_to_string( Timeout ), RequestName,
			  RequestArgs, InstancePid ] ),

		throw( { request_time_out, RequestName, InstancePid, Timeout } )

	end.



% Sends (in parallel) the specified request (based on its name and arguments) to
% each of the specified target instances.
%
% No waiting/receiving of the request results performed here.
%
% (helper)
%
-spec send_requests( request_name(), method_arguments(), [ instance_pid() ] ) ->
						void().
send_requests( RequestName, RequestArgs, TargetInstancePIDs ) ->

	Request = { RequestName, RequestArgs, self() },

	[ InstancePid ! Request || InstancePid <- TargetInstancePIDs ].




% Sends (in parallel) the specified request (based on its name and arguments) to
% each of the specified target instances, and waits (indefinitively) for their
% acknowledgement, which shall be their only result (i.e. these methods should
% be requests only for synchronisation).
%
% No time-out: answers will be waited indefinitely.
%
-spec send_requests_and_wait_acks( request_name(), method_arguments(),
								   [ instance_pid() ], ack_term() ) -> void().
send_requests_and_wait_acks( RequestName, RequestArgs, TargetInstancePIDs,
							 AckTerm ) ->

	send_requests( RequestName, RequestArgs, TargetInstancePIDs ),

	wait_indefinitively_for_request_answers( TargetInstancePIDs, AckTerm ).



% Sends (in parallel) the specified request (based on its name and arguments) to
% each of the specified target instances, and waits for their acknowledgement;
% returns whether it succeeded or if some instances triggered a time-out.
%
-spec send_requests_and_wait_acks( request_name(), method_arguments(),
		[ instance_pid() ], time_out(), ack_term() ) -> requests_outcome().
send_requests_and_wait_acks( RequestName, RequestArgs, TargetInstancePIDs,
							 Timeout, AckTerm ) ->

	send_requests( RequestName, RequestArgs, TargetInstancePIDs ),

	wait_for_request_answers( TargetInstancePIDs, Timeout, AckTerm ).



% Triggers a oneway on the specified series of instances, with no overlapping of
% their processing, and returns the ones that failed to report on time that they
% were executed.
%
% More precisely, for each of the specified instances, sends the specified
% oneway (expecting one of the specified arguments to contain the PID of the
% caller process so that the instance can send back an answer) and waits for an
% acknowledgement thereof (as {AckTerm, InstancePid}}), then proceeds to the
% next instance. Returns an (ordered, according to the input one) list of the
% PIDs of the instances that failed to answer on time, based on specified
% time-out.
%
-spec send_acknowledged_oneway_in_turn( oneway_name(), method_arguments(),
		[ instance_pid() ], time_out(), ack_term() ) -> [ instance_pid() ].
send_acknowledged_oneway_in_turn( OnewayName, OnewayArgs, TargetInstancePIDs,
								  Timeout, AckTerm ) ->

	OnewayCall = { OnewayName, OnewayArgs },

	Res = send_acked_oneway_in_turn_helper( OnewayCall, TargetInstancePIDs,
											Timeout, AckTerm, _FailedAcc=[] ),

	trace_bridge:debug_fmt( "For oneway call ~p, failed instances were ~p.",
							[ OnewayCall, Res ] ),

	Res.


% (helper)
send_acked_oneway_in_turn_helper( _OnewayCall, _TargetInstancePIDs=[], _Timeout,
								  _AckTerm, FailedAcc ) ->
	% Order maintained:
	lists:reverse( FailedAcc );

send_acked_oneway_in_turn_helper( OnewayCall,
	  _TargetInstancePIDs=[ InstancePid | T ], Timeout, AckTerm, FailedAcc ) ->

	trace_bridge:debug_fmt( "Sending oneway call ~p to ~w, to be acknowledged "
		"with the term '~p'.", [ OnewayCall, InstancePid, AckTerm ] ),

	InstancePid ! OnewayCall,

	receive

		{ AckTerm, InstancePid } ->

			trace_bridge:debug_fmt( "Received ack term '~p' for ~w.",
									[ AckTerm, InstancePid ] ),

			send_acked_oneway_in_turn_helper( OnewayCall, T, Timeout, AckTerm,
											  FailedAcc )

		% Just to debug:
		%Other ->
		%	trace_bridge:debug_fmt( "Received ~w instead of ack.", [ Other ] ),
		%	throw( { unexpected_ack, Other, OnewayCall, InstancePid } )

	after Timeout ->

		trace_bridge:error_fmt( "Ack term '~p' not received for oneway call ~p "
			"from ~w after a ~ts.", [ AckTerm, OnewayCall,
			InstancePid, time_utils:time_out_to_string( Timeout ) ] ),

		send_acked_oneway_in_turn_helper( OnewayCall, T, Timeout, AckTerm,
										  [ InstancePid | FailedAcc ] )

	end.





% Waits for an acknowledgement answer, based on specified term and on the PID of
% each of the specified requested instances, indefinitively (no time-out).
%
% Allows to trigger requests (supposing returning all the same, specified term)
% in parallel yet being able to wait synchronously for them, and know which, if
% any, did not answer.
%
% (helper)
%
-spec wait_for_request_answers( [ instance_pid() ], ack_term() ) ->
									requests_outcome().
wait_for_request_answers( RequestedPidList, AckTerm ) ->
	wait_indefinitively_for_request_answers( RequestedPidList, AckTerm ).



% Waits for an acknowledgement answer, based on specified term, from the
% specified requested instances, unless the specified time-out is exceeded
% (specified as integer milliseconds or as the 'infinity' atom).
%
% Allows to trigger requests in parallel yet being able to wait synchronously
% for them.
%
% (helper)
%
-spec wait_for_request_answers( [ instance_pid() ], time_out(), ack_term() ) ->
									requests_outcome().
wait_for_request_answers( RequestedPidList, _Timeout=infinity, AckTerm ) ->
	wait_indefinitively_for_request_answers( RequestedPidList, AckTerm );

wait_for_request_answers( RequestedPidList, Timeout, AckTerm ) ->

	InitialTimestamp = time_utils:get_timestamp(),

	wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
							  AckTerm ).


% Waits until end of time if necessary.
%
% (helper)
%
wait_indefinitively_for_request_answers( _RequestedPidList=[], _AckTerm ) ->
	success;

wait_indefinitively_for_request_answers( RequestedPidList, AckTerm ) ->

	receive

		{ wooper_result, { AckTerm, SenderPid } } ->

			NewPidList = list_utils:delete_existing( SenderPid,
													 RequestedPidList ),

			wait_indefinitively_for_request_answers( NewPidList, AckTerm )

	end.



% Wait until specified time-out is reached.
%
% (helper)
%
wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
						  AckTerm ) ->
	wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
							  _DefaultPollDuration=1000, AckTerm ).


wait_for_request_answers( _RequestedPidList=[], _InitialTimestamp, _Timeout,
						  _PollDuration, _AckTerm ) ->
	success;

wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
						  PollDuration, AckTerm ) ->

	receive

		{ wooper_result, { AckTerm, SenderPid } } ->

			NewPidList = list_utils:delete_existing( SenderPid,
													 RequestedPidList ),

			wait_for_request_answers( NewPidList, InitialTimestamp, Timeout,
									  PollDuration, AckTerm )

	after PollDuration ->

			NewDuration = time_utils:get_duration_since( InitialTimestamp ),

			case NewDuration > Timeout of

				true ->
					{ failure, RequestedPidList };

				false ->
					% Still waiting then:
					wait_for_request_answers( RequestedPidList,
						   InitialTimestamp, Timeout, PollDuration, AckTerm )

			end

	end.



% Waits (indefinitively) that the specified number of requests returned as
% result the specified acknowledgement term.
%
-spec wait_for_request_acknowledgements( count(), ack_term() ) -> void().
wait_for_request_acknowledgements( Count, AckTerm ) ->
	wait_for_request_acknowledgements( Count, AckTerm, _Timeout=infinity ).



% Waits, with the specified time-out, that the specified number of requests
% returned as result the specified acknowledgement term.
%
-spec wait_for_request_acknowledgements( count(), ack_term(), time_out() ) ->
											void().
wait_for_request_acknowledgements( _Count=0, _AckTerm, _Timeout ) ->

	%trace_bridge:debug_fmt(
	%  "[~w] No more waiting of the '~p' acknowledgement term.",
	%  [ self(), AckTerm ] ),

	ok;


wait_for_request_acknowledgements( Count, AckTerm, Timeout ) ->

	%trace_bridge:debug_fmt( "[~w] Waiting for ~B '~p' acknowledgement "
	%    term(s).", [ self(), Count, AckTerm ] ),

	receive

		{ wooper_result, AckTerm } ->

			%trace_bridge:debug_fmt(
			%  "[~w] Received a '~p' acknowledgement term.",
			%  [ self(), AckTerm ] ),

			wait_for_request_acknowledgements( Count-1, AckTerm )

	after Timeout ->

		trace_bridge:error_fmt( "Time-out after ~ts, while still waiting "
			"for ~B '~p' request acknowledgements.",
			[ time_utils:duration_to_string( Timeout ), Count, AckTerm ] ),

		throw( { wooper_request_ack_time_out, Count, AckTerm, Timeout } )

	end.




% Sends the specified request to all specified instances for execution, in
% parallel, and returns the corresponding results, in indiscriminated order.
%
% Note: no specified order is enforced in the result list; hence this helper is
% meant to be used when we can collect each result regardless of its specific
% sender.
%
% No time-out enforced.
%
% (exported helper)
%
-spec obtain_results_for_requests( request_name(), method_arguments(),
								   [ instance_pid() ] ) -> [ request_result() ].
obtain_results_for_requests( RequestName, RequestArgs, TargetInstancePIDs ) ->

	send_requests( RequestName, RequestArgs, TargetInstancePIDs ),

	% Of course we expect that no previously received WOOPER message is
	% remaining in the queue.

	collect_wooper_messages( _Count=length( TargetInstancePIDs ), _Acc=[] ).



% Collects specified number of WOOPER messages, and returns a list of the
% corresponding results.
%
% (helper)
%
collect_wooper_messages( _Count=0, Acc ) ->
	Acc;

collect_wooper_messages( Count, Acc ) ->

	receive

		{ wooper_result, Res } ->
			collect_wooper_messages( Count-1, [ Res | Acc ] )

	end.



% Second: one caller, multiple requests, one callee (expected to answer them in
% order - which is the general case in WOOPER where there is no selective
% receive).
%
% No other results expected to be received once the request series has been
% triggered. No time-out enforced.



% Sends directly the specified series of requests (based on their respective
% names and arguments) to the specified (single) target instance.
%
% Request answers not specifically managed by this function.
%
% (helper)
%
-spec send_request_series( [ { request_name(), method_arguments() } ],
						   instance_pid() ) -> void().
send_request_series( _Requests=[], _TargetInstancePID ) ->

	% A list comprehension could have been used (but then no check that elements
	% are pairs indeed)
	%
	ok;

send_request_series( _Requests=[ { RequestName, RequestArgs } | T ],
					 TargetInstancePID ) ->

	ActualRequest = { RequestName, RequestArgs, self() },

	TargetInstancePID ! ActualRequest,

	send_request_series( T, TargetInstancePID ).



% Sends directly the specified series of requests (based on their respective
% names and arguments) to the specified (single) target instance, and returns
% the corresponding results, in the specified order for the requests.
%
% (exported helper)
%
-spec obtain_results_for_request_series(
		[ { request_name(), method_arguments() } ], instance_pid() ) ->
											[ request_result() ].
obtain_results_for_request_series( Requests, TargetInstancePID ) ->

	send_request_series( Requests, TargetInstancePID ),

	% Requests sent in-order, so answers will be received in the same order:
	wait_request_series( _WaitCount=length( Requests ), _Acc=[] ).



% (helper)
wait_request_series( _WaitCount=0, Acc ) ->
	lists:reverse( Acc );

wait_request_series( WaitCount, Acc ) ->

	receive

		{ wooper_result, R } ->
			wait_request_series( WaitCount-1, [ R | Acc ] )

	end.




% Section for creation helpers.


% Creates (asynchronously) a blank process, waiting to embody a WOOPER instance
% once it will have received its class and construction parameters, and link it
% to the caller and to the specified process.
%
-spec create_hosting_process( net_utils:node_name(), pid() ) ->
									instance_pid().
create_hosting_process( Node, ToLinkWithPid ) ->

	WaitFun = fun() ->

		% Closure; not atomic:
		erlang:link( ToLinkWithPid ),

		receive

			  { embody, [ Class, ConstructionParameters ] } ->

				%trace_bridge:debug_fmt(
				%   "Process ~w becoming asynchronously an instance "
				%	"of class '~ts', constructed from following "
				%	"parameters:~n~p.",
				%	[ self(), Class, ConstructionParameters ] ),

				% Never returns:
				construct_and_run( Class, ConstructionParameters );


			  % We might need to notify another process than the caller:
			  { embody, [ Class, ConstructionParameters ], ToNotifyPid } ->

				%trace_bridge:debug_fmt(
				%   "Process ~w becoming synchronously an instance "
				%	"of class '~ts', constructed from following "
				%	"parameters:~n~p.",
				%	[ self(), Class, ConstructionParameters ] ),

				% Never returns:
				construct_and_run_synchronous( Class, ConstructionParameters,
											   ToNotifyPid )

		end

	end,

	?myriad_spawn_link( Node, WaitFun ).



% Checks, for the specified classname and construction parameters, that a
% corresponding module exists and that it has the relevant arity.
%
-spec check_classname_and_arity( classname(), construction_parameters() ) ->
									void().
check_classname_and_arity( Classname, ConstructionParameters ) ->

	% Normally useless, as called by the module itself:
	case code_utils:is_beam_in_path( Classname ) of

		not_found ->
			throw( { beam_not_found_for, Classname } );

		_ ->
			ok

	end,

	% Includes the state:
	ArgCount = length( ConstructionParameters ) + 1,

	case meta_utils:is_function_exported( _Module=Classname,
						_Function=construct, _Arity=ArgCount ) of

		true ->
			ok;

		false ->

			ExportedFunctions = meta_utils:list_exported_functions( Classname ),

			case _ConstructArities=[ Arity
					|| { construct, Arity } <- ExportedFunctions ] of


				[] ->
					trace_bridge:error_fmt( "Error, no 'construct' exported "
						"in '~ts' (regardless of arity).", [ Classname ] ),
					throw( { no_exported_construct, Classname } );

				[ FoundArity ] when FoundArity > ArgCount ->

					ExtraCount  = FoundArity - ArgCount,

					trace_bridge:error_fmt( "Error, no ~ts:construct/~B found, "
						"whereas construct/~B is exported; ~B extra "
						"construction parameter(s) specified.",
						[ Classname, ArgCount, FoundArity, ExtraCount ] ),

					throw( { extra_construction_parameters_specified,
							 Classname, ExtraCount } );


				% Here ArgCount > FoundArity:
				[ FoundArity ] ->

					LackingCount  = ArgCount - FoundArity,

					trace_bridge:error_fmt( "Error, no ~ts:construct/~B found, "
						"whereas construct/~B is exported; ~B lacking "
						"construction parameter(s) specified.",
						[ Classname, ArgCount, FoundArity, LackingCount ] ),

					throw( { lacking_construction_parameters_specified,
							 Classname, LackingCount } );


				ConstructArities ->

					trace_bridge:error_fmt( "Error, no ~ts:construct/~B found, "
						"whereas this function is exported for following "
						"arities: ~w.",
						[ Classname, ArgCount, ConstructArities ] ),

					throw( { invalid_construction_parameters_specified,
							 Classname, ArgCount, ConstructArities } )

			end

	end.



% Constructs the initial state of an instance of specified class, using
% specified construction parameters, and enters its main loop.
%
% (helper)
%
-spec construct_and_run( classname(), construction_parameters() ) ->
								no_return().


-ifdef(wooper_debug_mode).

construct_and_run( Classname, ConstructionParameters ) ->

	%trace_bridge:debug_fmt( "wooper:construct_and_run for class ~p "
	%	"and following parameters:~n ~p",
	%	[ Classname, ConstructionParameters ] ),

	%check_classname_and_arity( Classname, ConstructionParameters ),

	BlankState = get_blank_state( Classname ),

	try apply( Classname, construct,
			   [ BlankState | ConstructionParameters ] ) of

		ConstructState when is_record( ConstructState, state_holder ) ->

			% Enforces a closer-to-ideal load factor of the hashtable if needed,
			% as by convention no attribute should be introduced outside of the
			% constructor:
			%
			% (now useless with more advanced tables)
			%
			%TunedTable = ?wooper_table_type:optimise(
			%AttrTable = ConstructState#state_holder.attribute_table,

			%ReadyState = ConstructState#state_holder{
			%			   attribute_table=AttrTable },

			% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper'
			% instead of the right class:
			%
			Classname:wooper_main_loop( ConstructState );


		Other ->

			log_error( "~nWOOPER error for PID ~w of class ~ts: "
				"constructor did not return a state, but returned '~p' "
				"instead. Construction parameters were:~n~p.",
				[ self(), Classname, Other, ConstructionParameters ] ),

			Arity = length( ConstructionParameters ) + 1,

			throw( { invalid_constructor, Classname, { construct, Arity } } )

	catch

		Reason:ErrorTerm:Stacktrace ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters, Stacktrace )

	end.



-else. % wooper_debug_mode


construct_and_run( Classname, ConstructionParameters ) ->

	BlankState = get_blank_state( Classname ),

	ConstructState = try

		apply( Classname, construct, [ BlankState | ConstructionParameters ] )

	catch

		Reason:ErrorTerm:Stacktrace ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters, Stacktrace )

	end,

	% Enforces a closer-to-ideal load factor of the hashtable if needed, as by
	% convention no attribute should be introduced outside of the constructor:
	%
	%TunedTable = ?wooper_table_type:optimise(
	%						ConstructState#state_holder.attribute_table ),


	%ReadyState = ConstructState#state_holder{ attribute_table=TunedTable },

	% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper' instead
	% of the right class:
	%
	Classname:wooper_main_loop( ConstructState ).


-endif. % wooper_debug_mode






% Constructs synchronously the initial state of an instance of specified class,
% using specified construction parameters, and enters its main loop.
%
% (helper)
%
-spec construct_and_run_synchronous( classname(), construction_parameters(),
									 pid() ) -> no_return().


-ifdef(wooper_debug_mode).

construct_and_run_synchronous( Classname, ConstructionParameters,
							   SpawnerPid ) ->

	%check_classname_and_arity( Classname, ConstructionParameters ),

	BlankState = get_blank_state( Classname ),

	try apply( Classname, construct,
			   [ BlankState | ConstructionParameters ] ) of

		ConstructState when is_record( ConstructState, state_holder ) ->

			% Notify early:
			SpawnerPid ! { spawn_successful, self() },

			% Enforces a closer-to-ideal load factor of the hashtable if needed,
			% as by convention no attribute should be introduced outside of the
			% constructor:
			%
			% (now useless with more advanced tables)
			%
			% TunedTable = ?wooper_table_type:optimise(
			AttrTable = ConstructState#state_holder.attribute_table,

			ReadyState = ConstructState#state_holder{
							attribute_table=AttrTable },

			% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper'
			% instead of the right class:
			%
			% (never returns)
			%
			Classname:wooper_main_loop( ReadyState );



		Other ->

			log_error( "~nWOOPER error for PID ~w of class ~ts: "
				"constructor did not return a state, but returned '~p' "
				"instead. Construction parameters were:~n~p.~n",
				[ self(), Classname, Other, ConstructionParameters ] ),

			Arity = length( ConstructionParameters ) + 1,

			throw( { invalid_constructor, Classname, { construct, Arity } } )

	catch

		Reason:ErrorTerm:Stacktrace ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters, Stacktrace )

	end.



-else. % not in wooper_debug_mode:


construct_and_run_synchronous( Classname, ConstructionParameters,
							   SpawnerPid ) ->

	BlankState = get_blank_state( Classname ),

	% Faulty returns (non-state) not detected here:
	ConstructState = try

			 apply( Classname, construct,
					[ BlankState | ConstructionParameters ] )

	catch

		Reason:ErrorTerm:Stacktrace ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters, Stacktrace )

	end,

	% Notify early:
	SpawnerPid ! { spawn_successful, self() },

	% Enforces a closer-to-ideal load factor of the hashtable if needed, as by
	% convention no attribute should be introduced outside of the constructor:
	%
	% (now useless with more advanced tables)
	%TunedTable = ?wooper_table_type:optimise(
	AttrTable = ConstructState#state_holder.attribute_table,

	ReadyState = ConstructState#state_holder{ attribute_table=AttrTable },

	% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper' instead
	% of the right class:
	%
	% (never returns)
	%
	Classname:wooper_main_loop( ReadyState ).


-endif. % not wooper_debug_mode.



% Section for passive instances.


% Constructs a passive instance: returns the initial state thereof.
-spec construct_passive( classname(), construction_parameters() ) ->
							passive_instance().
construct_passive( Classname, ConstructionParameters ) ->

	%trace_bridge:debug_fmt( "wooper:construct_passive for class ~ts "
	%					   "and parameters ~p.",
	%					   [ Classname, ConstructionParameters ] ),

	cond_utils:if_defined( wooper_debug_mode,
			check_classname_and_arity( Classname, ConstructionParameters ) ),

	BlankState = get_blank_state( Classname ),

	try apply( Classname, construct,
			   [ BlankState | ConstructionParameters ] ) of

		ConstructState when is_record( ConstructState, state_holder ) ->
			ConstructState;

		Other ->
			log_error( "WOOPER error when creating a passive instance "
				"of class ~ts: constructor did not return a state, "
				"but returned '~p' instead. "
				"Construction parameters were:~n~p",
				[ Classname, Other, ConstructionParameters ] ),

			Arity = length( ConstructionParameters ) + 1,

			throw( { invalid_constructor, Classname, { construct, Arity } } )

	catch

		Reason:ErrorTerm:Stacktrace ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters, Stacktrace )

	end.



% execute_request/3 defined together with its convenience helper counterpart
% (same name, same arity, different purposes).


% Executes specified oneway on specified passive instance.
-spec execute_oneway( instance_pid(), oneway_name() ) -> void();
					( passive_instance(), oneway_name() ) ->
							passive_instance().
execute_oneway( TargetInstancePID, OnewayName )
   when is_pid( TargetInstancePID ) andalso is_atom( OnewayName ) ->

	% Hardly useful:
	TargetInstancePID ! OnewayName;


execute_oneway( PassiveInstance, OnewayName )
   when is_record( PassiveInstance, ?passive_record )
		andalso is_atom( OnewayName ) ->

	{ NewPassiveInstance, { wooper_method_returns_void, R } } =
		wooper_execute_method( OnewayName, _OnewayArgs=[], PassiveInstance ),

	{ NewPassiveInstance, R }.



% Executes specified oneway on specified passive instance.
-spec execute_oneway( instance_pid(), oneway_name(),
					  method_arguments() ) -> void();
					( passive_instance(), oneway_name(), method_arguments() ) ->
							passive_instance().
execute_oneway( TargetInstancePID, OnewayName, OnewayArgs )
   when is_pid( TargetInstancePID ) andalso is_atom( OnewayName ) ->

	% Hardly useful:
	TargetInstancePID ! { OnewayName, OnewayArgs };


execute_oneway( PassiveInstance, OnewayName, OnewayArgs )
   when is_record( PassiveInstance, ?passive_record )
		andalso is_atom( OnewayName ) andalso is_list( OnewayArgs ) ->

	%trace_bridge:info_fmt( "Executing oneway ~ts/~B on passive instance",
	%					   [ OnewayName, length( OnewayArgs ) ] ),

	{ NewPassiveInstance, wooper_method_returns_void } =
		wooper_execute_method( OnewayName, OnewayArgs, PassiveInstance ),

	NewPassiveInstance;

% Promote non-list argument to list:
execute_oneway( PassiveInstance, OnewayName, OnewayArg )
   when is_record( PassiveInstance, ?passive_record )
		andalso is_atom( OnewayName ) ->

	%trace_bridge:info_fmt( "Executing oneway ~ts on passive instance",
	%					   [ OnewayName ] ),

	{ NewPassiveInstance, wooper_method_returns_void } =
		wooper_execute_method( OnewayName, [ OnewayArg ], PassiveInstance ),

	NewPassiveInstance.



% Helpers.


% Returns the state of a blank WOOPER instance of specified class.
%
% (helper)
%
-spec get_blank_state( classname() ) -> wooper:state().
get_blank_state( Classname ) ->

	TableKey = retrieve_virtual_table_key( Classname ),

	#state_holder{

		% Here we fetch once for all that table (as a "reference"):
		virtual_table=persistent_term:get( TableKey ),

		attribute_table=
		   ?wooper_table_type:new( ?wooper_attribute_count_upper_bound ),

		actual_class=Classname,
		request_sender=undefined }.




% Section for default handlers.


% WOOPER default EXIT message handler; called if trapping EXIT signals.
%
% Returns an updated state.
%
% Can be overridden by defining or inheriting the onWOOPERExitReceived/3 oneway.
%
% (helper)
%
-spec default_exit_handler( basic_utils:pid_or_port(), exit_reason(),
							wooper:state() ) -> wooper:state().
default_exit_handler( PidOrPort, ExitReason, State ) ->

	log_warning( "WOOPER default EXIT handler of the ~w instance ~w "
		"ignored the following EXIT message from ~w:~n'~p'.",
		[ State#state_holder.actual_class, self(), PidOrPort, ExitReason ] ),

	State.



% WOOPER default DOWN handler, for process monitors.
%
% Returns an updated state.
%
% Can be overridden by defining or inheriting the onWOOPERDownNotified/5 oneway.
%
% Note: not to be mixed up with the default_node_down_handler/3 /
% onWOOPERNodeDisconnection/3 pair (which is node-related).
%
% (helper)
%
-spec default_down_handler( monitor_utils:monitor_reference(),
	monitor_utils:monitored_element_type(), monitor_utils:monitored_element(),
	exit_reason(), wooper:state() ) -> wooper:state().
default_down_handler( _MonitorReference, _MonitoredType,
					  _MonitoredElement, _ExitReason=normal, State ) ->
	% Normal exits not notified:
	State;

default_down_handler( MonitorReference, MonitoredType, MonitoredElement,
					  ExitReason, State ) ->

	log_warning( "WOOPER default DOWN handler of the ~w "
		"instance ~w ignored the following down notification "
		"'~ts' for monitored element ~p of type '~p' (monitor reference: ~w).",
		[ State#state_holder.actual_class, self(), ExitReason,
		  MonitoredElement, MonitoredType, MonitorReference ] ),

	State.




% WOOPER default node up handler.
%
% Returns an updated state.
%
% Can be overridden by defining or inheriting the onWOOPERNodeConnection/3
% oneway.
%
% (helper)
%
-spec default_node_up_handler( net_utils:atom_node_name(),
			monitor_utils:monitor_node_info(), wooper:state() ) ->
										wooper:state().
default_node_up_handler( Node, MonitorNodeInfo, State ) ->

	log_warning( "WOOPER default node up handler of the ~w "
		"instance ~w ignored the connection notification "
		"for node '~ts' (information: ~p).",
		[ State#state_holder.actual_class, self(), Node, MonitorNodeInfo ] ),

	State.



% WOOPER default node down handler.
%
% Returns an updated state.
%
% Can be overridden by defining or inheriting the onWOOPERNodeDisconnection/3
% oneway.
%
% Note: not to be mixed up with the default_down_handler/5 /
% onWOOPERDownNotified/5 pair (which is process-related).
%
-spec default_node_down_handler( net_utils:atom_node_name(),
		monitor_utils:monitor_node_info(), wooper:state() ) -> wooper:state().
default_node_down_handler( Node, MonitorNodeInfo, State ) ->

	log_warning( "WOOPER default node down handler of the ~w "
		"instance ~w ignored the disconnection notification "
		"for node '~ts' (information: ~p).",
		[ State#state_holder.actual_class, self(), Node, MonitorNodeInfo ] ),

	State.



% Returns the key in persistent_term for the virtual table corresponding to the
% specified class.
%
% Note: the key could be directly guessed by the instance; the interest here is
% mostly for synchronisation (to ensure that a suitable entry for the current
% class exists in the persistent_term registry, otherwise race conditions could
% happen).
%
% (helper)
%
-spec retrieve_virtual_table_key( classname() ) -> class_key().

-if( ?wooper_enable_otp_integration =:= true ).

retrieve_virtual_table_key( Classname ) ->

	%trace_bridge:debug_fmt( "Retrieving the OTP-way the virtual table "
	%						"key for '~ts'.", [ Classname ] ),

	% The OTP way, through a gen_server:call/2:
	wooper_class_manager:get_table_key( Classname ).



-elif( ?wooper_enable_otp_integration =:= false ).

retrieve_virtual_table_key( Classname ) ->

	%trace_bridge:debug_fmt(
	%  "Retrieving classically (non-OTP way) the virtual table key for '~ts'.",
	%  [ Classname ] ),

	% For per-instance virtual table: wooper_create_method_table_for(?MODULE).

	% The non-OTP way:
	wooper_class_manager:get_manager() ! { get_table_key, Classname, self() },
	receive

		{ wooper_virtual_table_key, TableKey } ->
			%?wooper_table_type:display( Table ),
			TableKey

	end.

-endif. % wooper_enable_otp_integration



% Triggers specified construction error (notify and throw).
%
% (helper)
%
-spec trigger_error( basic_utils:exception_class(), term(), classname(),
			[ method_arguments() ], stack_trace() ) -> no_return().
trigger_error( _Reason, _ErrorTerm=undef, Classname, ConstructionParameters,
	   _Stacktrace=[ _UndefCall={ ModuleName, FunctionName, UndefArgs, Loc }
					 | NextCalls ] ) ->

	%trace_bridge:debug_fmt( "NextCalls: ~p", [ NextCalls ] ),

	% An undef error is difficult to investigate (multiple possible reasons
	% behind), let's be nice to the developer:

	Arity = length( ConstructionParameters ) + 1,

	UndefArity = length( UndefArgs ),

	%trace_bridge:info_fmt( "Construction failed (undef) in ~ts:construct/~B, "
	%           "for ~ts:~ts/~B.",
	%			[ Classname, Arity, ModuleName, FunctionName, UndefArity ] ),

	Diagnosis = code_utils:interpret_undef_exception( ModuleName, FunctionName,
													  UndefArity ),

	LocString = get_location_string( Loc, NextCalls ),

	log_error( "~nWOOPER error for PID ~w, "
		"constructor (~ts:construct/~B) failed due to an 'undef' "
		"call to ~ts:~ts/~B.~nDiagnosis: ~ts~ts.",
		[ self(), Classname, Arity, ModuleName, FunctionName,
		  UndefArity, Diagnosis, LocString ] ),

	throw( { wooper_constructor_failed, self(), Classname, Arity,
			 { undef, { ModuleName, FunctionName, UndefArity } } } );


trigger_error( Reason, ErrorTerm, Classname, ConstructionParameters,
			   Stacktrace ) ->

	% Construction failed:
	% (error term would often be unreadable with ~p)

	Arity = length( ConstructionParameters ) + 1,

	%trace_bridge:info_fmt( "Construction failed for ~ts:construct/~B.",
	%					   [ Classname, Arity ] ),

	log_error( "~nWOOPER error for PID ~w, "
		"constructor (~ts:construct/~B) failed (cause: ~p):~n~n"
		" - with error term:~n  ~p~n~n"
		" - stack trace was (latest calls first):~n~ts~n"
		" - for construction parameters:~n  ~p~n",
		[ self(), Classname, Arity, Reason, ErrorTerm,
		  code_utils:interpret_stacktrace( Stacktrace ),
		  ConstructionParameters ] ),

	throw( { wooper_constructor_failed, self(), Classname, Arity,
			 ConstructionParameters, ErrorTerm } ).



% Returns the description of the best location found from specified stacktrace
% excerpts.
%
-spec get_location_string( stack_location(), stack_item() ) -> ustring().
get_location_string( _Loc=[], _NextCalls=[ { _M, _F, _A, NextLoc } | _ ] ) ->
	LocStr = code_utils:stack_location_to_string( NextLoc ),
	text_utils:format( " (call location: ~ts)", [ LocStr ] );

% Includes _NextCalls=[] and any unexpected pattern:
get_location_string( _Loc=[], _NextCalls ) ->
	"";

get_location_string( Loc, _NextCalls ) ->
	LocStr = code_utils:stack_location_to_string( Loc ),
	text_utils:format( " (call location: ~ts)", [ LocStr ] ).




% Methods for getting information about an instance.


% Returns the actual classname of the specified instance.
%
% Can be trusted.
%
% (helper)
%
-spec get_classname( wooper:state() ) -> classname().
get_classname( State ) ->
	State#state_holder.actual_class.



% Returns the (user-level) attributes known of WOOPER for the specified state
% (i.e. all attributes except the ones used internally by WOOPER).
%
% (helper)
%
-spec get_attribute_pairs( wooper:state() ) -> [ attribute_entry() ].
get_attribute_pairs( State ) ->

	AllAttrs = get_all_attributes( State ),

	ReservedAttrs = get_wooper_reserved_attribute_names(),

	% Remove WOOPER internals:
	filter_wooper_attributes( AllAttrs, ReservedAttrs, _Acc=[] ).



% Removes from the specified atttributes the ones used internally by WOOPER (so
% that only class-specific ones remain).
%
% (internal helper)
%
filter_wooper_attributes( _AttrPairs=[], _ReservedAttrs, Acc ) ->
	Acc;

filter_wooper_attributes( _AttrPairs=[ AttrEntry={ Name, _Value } | T ],
						  ReservedAttrs, Acc ) ->

	case lists:member( Name, ReservedAttrs ) of

		true ->
			filter_wooper_attributes( T, ReservedAttrs, Acc );

		false ->
			filter_wooper_attributes( T, ReservedAttrs, [ AttrEntry | Acc ] )

	end.



% Returns a list of the attribute names that are used internally by WOOPER.
-spec get_wooper_reserved_attribute_names() -> [ attribute_name() ].
get_wooper_reserved_attribute_names() ->
	[].



% Returns a textual representation of the attributes of the specified state.
-spec state_to_string( wooper:state() ) -> ustring().
state_to_string( State ) ->

	% Not using get_attribute_pairs/1 to rely on the full state:
	Attributes = get_all_attributes( State ),

	% We prefer having the attributes sorted by their name, in alphabetical
	% order:
	%
	SortedAttributes = lists:keysort( _Index=1, Attributes ),

	lists:foldl(

		fun( { AttName, AttrValue }, Acc ) ->
			Acc ++ text_utils:format( "     * ~ts = ~ts~n",
					[ text_utils:term_to_string( AttName ),
					  text_utils:term_to_string( AttrValue, _MaxDepth=16,
												 _MaxLength=100 ) ] )

		end,

		io_lib:format( "State of ~w:~nInstance of ~ts with ~B attribute(s):~n",
			[ self(), get_classname( State ), length( Attributes ) ] ),

		SortedAttributes ).



% Returns the source filename associated to specified class.
%
% Ex: get_class_filename( 'class_Foo' ) returns simply "class_Foo.erl".
%
-spec get_class_filename( classname() ) -> file_utils:filename().
get_class_filename( Classname ) ->
	text_utils:format( "~ts.erl", [ Classname ] ).



% Returns the time-out to be used for synchronous operations, depending on the
% debug mode.
%
-spec get_synchronous_time_out( boolean() ) -> time_out().
get_synchronous_time_out( _IsDebugMode=true ) ->

	% Suitable for most applications (5 seconds, to benefit from earlier
	% reports):
	%
	5000;

get_synchronous_time_out( _IsDebugMode=false ) ->
	% Better for applications in production (30 minutes):
	30*60*1000.





-ifdef(wooper_debug_mode).


% Returns a textual representation of the virtual table corresponding to the
% specified state.
%
% (helper)
%
-spec virtual_table_to_string( wooper:state() ) -> ustring().
virtual_table_to_string( State ) ->

	lists:foldl(

	  fun( { { Name, Arity }, Module }, String ) ->
			  String ++ text_utils:format( "     * ~ts/~B -> ~ts~n",
										   [ Name, Arity, Module ] )
	  end,

	  _Acc=text_utils:format( "Virtual table of ~w:~n(method name/arity -> "
							  "module defining that method)~n", [ self() ] ),

	  _List=?wooper_table_type:enumerate(
			 %persistent_term:get( State#state_holder.virtual_table_key ) ) ).
			State#state_holder.virtual_table ) ).



% Returns a textual representation of this instance, including its state and
% virtual table.
%
% (helper)
%
-spec instance_to_string( wooper:state() ) -> ustring().
instance_to_string( State ) ->
	text_utils:format( "Inspection of instance ~w:~n~n  + ~ts~n  + ~ts",
		[ self(), state_to_string( State ),
		  virtual_table_to_string( State ) ] ).



% Displays the inner state of this instance.
%
% This is not a method.
%
-spec display_state( wooper:state() ) -> void().
display_state( State ) ->
	logger:info( "~ts~n", [ state_to_string( State ) ] ).



% Displays the virtual table of this instance.
%
% This is not a method.
%
-spec display_virtual_table( wooper:state() ) -> void().
display_virtual_table( State ) ->
	logger:info( "~ts~n", [ virtual_table_to_string( State ) ] ).


% Displays information about this instance.
%
% This is not a method.
%
-spec display_instance( wooper:state() ) -> void().
display_instance( State ) ->
	logger:info( "~ts~n", [ instance_to_string( State ) ] ).


-endif. % wooper_debug_mode



% Returns all the attributes of this instance, as a list of {AttributeName,
% AttributeValue} pairs.
%
-spec get_all_attributes( wooper:state() ) -> [ attribute_entry() ].
get_all_attributes( State ) ->
	?wooper_table_type:enumerate( State#state_holder.attribute_table ).



% Declares automatically the relevant BEAM directories in the code path, so that
% Ceylan-WOOPER can be fully usable from then on.
%
% Note:
%
% - the code_utils.beam module of Ceylan-Myriad must be available from the
% current code path
%
% - the CEYLAN_MYRIAD and CEYLAN_WOOPER environment variables must be defined
% and must point to the respective root directories
%
% - the determined directories are not specifically checked for existence,
% and are added at the end of the code path
%
-spec declare_beam_dirs_for_wooper() -> void().
declare_beam_dirs_for_wooper() ->
	code_utils:declare_beam_dirs_for_myriad(),
	code_utils:declare_beam_dirs_for( "CEYLAN_WOOPER" ).



% Log section.
%
% Note that, depending on the context, the standard logger might be configured
% to rely on a dedicated handler (typically see Ceylan-Traces for integrated,
% advanced log services).



% Reports (on a best-effort basis) the specified information to the user,
% typically by displaying an information report on the console.
%
-spec log_info( ustring() ) -> void().
log_info( String ) ->
	logger:info(
	  text_utils:ellipse( String, ?ellipse_length ) ++ "\n" ).


% Reports (on a best-effort basis) the specified information to the user,
% typically by displaying an information report on the console.
%
-spec log_info( format_string(), [ term() ] ) -> void().
log_info( FormatString, ValueList ) ->
	Str = text_utils:format( FormatString, ValueList ),
	logger:info( text_utils:ellipse( Str, ?ellipse_length ) ++ "\n" ).



% Reports (on a best-effort basis) the specified warning to the user, typically
% by displaying a warning report on the console.
%
-spec log_warning( ustring() ) -> void().
log_warning( String ) ->

	logger:warning( text_utils:ellipse( String, ?ellipse_length ) ++ "\n" ),

	% Wait a bit, as logger (at least former error_logger) seems asynchronous:
	system_utils:await_output_completion( ?wooper_warning_display_waiting ).


% Reports (on a best-effort basis) the specified warning to the user, typically
% by displaying a warning report on the console.
%
-spec log_warning( format_string(), [ term() ] ) -> void().
log_warning( FormatString, ValueList ) ->

	Str = text_utils:format( FormatString, ValueList ),

	logger:warning( text_utils:ellipse( Str, ?ellipse_length ) ++ "\n" ),

	% Wait a bit, as logger (at least former error_logger) seems asynchronous:
	system_utils:await_output_completion( ?wooper_warning_display_waiting ).



% Reports (as synchronously as possible, in order to avoid loosing this
% notification) the specified error to the user, typically by displaying an
% error report on the console (non-halting function, ex: no exception thrown).
%
-spec log_error( ustring() ) -> void().
log_error( Message ) ->

	% Never ellipsing for errors now:
	%logger:error( text_utils:ellipse( Message, ?ellipse_length ) ++ "\n" ),
	logger:error( Message ++ "\n" ),

	% Wait a bit, as logger (at least former error_logger) seems asynchronous:
	system_utils:await_output_completion( ?wooper_error_display_waiting ).



% Reports (as synchronously as possible, in order to avoid loosing this
% notification) the specified error to the user, typically by displaying an
% error report on the console (non-halting function, ex: no exception thrown).
%
-spec log_error( format_string(), [ term() ] ) -> void().
log_error( FormatString, ValueList ) ->

	Str = text_utils:format(
			FormatString ++ "~n=END OF WOOPER ERROR REPORT FOR ~w ===~n",
			ValueList ++ [ self() ] ),

	%trace_bridge:debug_fmt( "Error message: ~p.", [ Str ] ),

	% Never ellipsing for errors now:
	%logger:error( text_utils:ellipse( Str, ?ellipse_length ) ),
	logger:error( Str ),

	% Wait a bit, as logger (at least former error_logger) seems asynchronous:
	system_utils:await_output_completion( ?wooper_error_display_waiting ).



% Reports (as synchronously as possible, in order to avoid loosing this
% notification) the specified error about the current WOOPER instance
% (preferably thanks to its state, otherwise with the current executed module,
% so with fewer information) to the user, typically by displaying an error
% report on the console (non-halting function, ex: no exception thrown).
%
-spec log_error( format_string(), [ term() ],
				 wooper:state() | basic_utils:module_name() ) -> void().
log_error( FormatString, ValueList, State )
  when is_record( State, state_holder ) ->

	io:format( "~n", [] ),

	% Node information would be uselessly distracting:
	%log_error( "WOOPER error for ~ts instance of PID ~w on node ~ts: "
	%		   ++ FormatString,
	%		   [ State#state_holder.actual_class, self(),
	%            node() | ValueList ] );
	log_error( "WOOPER error for ~ts instance of PID ~w: " ++ FormatString,
			   [ State#state_holder.actual_class, self() | ValueList ] );

log_error( FormatString, ValueList, ModuleName ) when is_atom( ModuleName ) ->

	io:format( "~n", [] ),

	% Node information would be uselessly distracting:
	%log_error( "WOOPER error for instance of PID ~w on node ~ts triggered "
	%		   "in module ~ts: " ++ FormatString,
	%		   [ self(), ModuleName, node() | ValueList ] ).
	log_error( "WOOPER error for instance of PID ~w triggered "
		"in module ~ts: " ++ FormatString,
		[ self(), ModuleName | ValueList ] ).




% Called by WOOPER whenever a request fails, to report it on the console and to
% the caller, and have the process instance exit.
%
-spec on_failed_request( request_name(), method_arguments(), pid(),
	error_type(), error_term(), stack_trace(), wooper:state() ) -> no_return().
on_failed_request( RequestName, ArgumentList, CallerPid, ErrorType,
	ErrorTerm=undef,
	_Stacktrace=[ _UndefCall={ ModuleName, FunctionName, UndefArgs, Loc }
				  | NextCalls ], State ) ->

	Arity = length( ArgumentList ) + 1,

	ModulePrefix = lookup_method_prefix( RequestName, Arity, State ),

	% An undef error is difficult to investigate (multiple possible reasons
	% behind), let's be nice to the developer:

	UndefArity = length( UndefArgs ),

	Diagnosis = code_utils:interpret_undef_exception( ModuleName, FunctionName,
													  UndefArity ),

	LocString = get_location_string( Loc, NextCalls ),

	log_error( "request ~ts~ts/~B failed due to an 'undef' "
		"call to ~ts:~ts/~B.~nDiagnosis: ~ts~ts.",
		[ ModulePrefix, RequestName, Arity, ModuleName, FunctionName,
		  UndefArity, Diagnosis, LocString ],
		State ),

	% ArgumentList and actual method module not propagated back to the caller:
	ErrorReason = { request_failed, State#state_holder.actual_class,
					self(), RequestName, { ErrorType, ErrorTerm } },

	CallerPid ! { wooper_error, ErrorReason },

	% Investigating a transient case where no message other than request_failed
	% was output:
	%
	%timer:sleep( 1000 ),

	% We do not want a duplicate error message, yet we cannot use 'normal' as
	% linked processes would not be triggered:
	%
	exit( request_failed );


on_failed_request( RequestName, ArgumentList, CallerPid, ErrorType, ErrorTerm,
				   Stacktrace, State ) ->

	Arity = length( ArgumentList ) + 1,

	ModulePrefix = lookup_method_prefix( RequestName, Arity, State ),

	log_error( "request ~ts~ts/~B failed (cause: ~ts):~n~n"
		" - with error term:~n  ~p~n~n"
		" - stack trace was (latest calls first):~n~ts~n"
		" - caller being process ~w~n~n"
		" - for request parameters:~n  ~p~n",
		[ ModulePrefix, RequestName, Arity, ErrorType, ErrorTerm,
		  code_utils:interpret_stacktrace( Stacktrace ), CallerPid,
		  ArgumentList ],
		State ),

	% ArgumentList and actual method module not propagated back to the caller:
	ErrorReason = { request_failed, State#state_holder.actual_class,
					self(), RequestName, { ErrorType, ErrorTerm } },

	CallerPid ! { wooper_error, ErrorReason },

	% Investigating a transient case where no message other than request_failed
	% was output:
	%
	%timer:sleep( 1000 ),

	% We do not want a duplicate error message, yet we cannot use 'normal' as
	% linked processes would not be triggered:
	%
	exit( request_failed ).



% Called by WOOPER whenever a oneway fails, to report it on the console and to
% the caller, and have the process instance exit.
%
-spec on_failed_oneway( oneway_name(), method_arguments(),
	error_type(), error_term(), stack_trace(), wooper:state() ) -> no_return().
on_failed_oneway( OnewayName, ArgumentList, _ErrorType, _ErrorTerm=undef,
	   _Stacktrace=[ _UndefCall={ ModuleName, FunctionName, UndefArgs, Loc }
					 | NextCalls ], State ) ->

	Arity = length( ArgumentList ) + 1,

	ModulePrefix = lookup_method_prefix( OnewayName, Arity, State ),

	% An undef error is difficult to investigate (multiple possible reasons
	% behind), let's be nice to the developer:

	UndefArity = length( UndefArgs ),

	Diagnosis = code_utils:interpret_undef_exception( ModuleName, FunctionName,
													  UndefArity ),

	LocString = get_location_string( Loc, NextCalls ),

	log_error( "oneway ~ts~ts/~B failed due to an 'undef' "
		"call to ~ts:~ts/~B.~nDiagnosis: ~ts~ts.",
		[ ModulePrefix, OnewayName, Arity, ModuleName, FunctionName,
		  UndefArity, Diagnosis, LocString ], State ),

	% No caller to notify, for oneways.

	% We do not want a duplicate error message, yet we cannot use 'normal' as
	% linked processes would not be triggered:
	%
	exit( oneway_failed );

on_failed_oneway( OnewayName, ArgumentList, ErrorType, ErrorTerm, Stacktrace,
				  State ) ->

	Arity = length( ArgumentList ) + 1,

	ModulePrefix = lookup_method_prefix( OnewayName, Arity, State ),

	% PID managed by log_error:
	log_error( "oneway ~ts~ts/~B failed (cause: ~ts):~n~n"
		" - with error term:~n  ~p~n~n"
		" - stack trace was (latest calls first):~n~ts~n"
		" - for oneway parameters:~n  ~p~n",
		[ ModulePrefix, OnewayName, Arity, ErrorType, ErrorTerm,
		  code_utils:interpret_stacktrace( Stacktrace ), ArgumentList ],
		State ),

	% No caller to notify, for oneways.

	% We do not want a duplicate error message, yet we cannot use 'normal' as
	% linked processes would not be triggered:
	%
	exit( oneway_failed ).



% Looks up the module defining specified method, and returns a textual prefix
% specifying it, if found.
%
% Used for error management, hence designed not to fail.
%
% (helper)
%
-spec lookup_method_prefix( method_name(), arity(), wooper:state() ) ->
									ustring().
lookup_method_prefix( MethodAtom, Arity, State ) ->

	try wooper_lookup_method( State, MethodAtom, Arity ) of

		{ value, Module } ->
			text_utils:format( "~ts:", [ Module ] );

		key_not_found ->
			""

	catch

		_:_ ->
			""

	end.



% Helper function to test requests.
%
% Allows to test from the shell an instance by sending it requests (hence
% needing a receive, whereas the caller is the shell), and waiting for any kind
% of message sent back.
%
% Returns the actual result or received value.
%
% Available even when debug mode is off.
%
-spec send_and_listen( instance_pid(), request_name(), method_arguments() ) ->
							term().
send_and_listen( InstancePid, RequestName, Arguments ) ->

	InstancePid ! { RequestName, Arguments, self() },

	receive

		{ wooper_result, Result } ->

			%trace_bridge:debug_fmt(
			%   "Result of call to '~w' with arguments '~w': ~ts",
			%	[ RequestName, Arguments,
			%	 text_utils:term_to_string( Result ) ] ),

			Result;

		Anything ->

			%trace_bridge:debug_fmt(
			% "Answer to call to '~w' with arguments '~w': ~ts",
			%	[ RequestName, Arguments,
			%	  text_utils:term_to_string( Anything ) ] ),

			Anything

	end.



% Returns the result corresponding to the first pending WOOPER request (the
% latest sent one), or blocks.
%
% (helper)
%
-spec receive_result() -> request_result( any() ).
receive_result() ->

	receive

		{ wooper_result, R } ->
			R

	end.




% Deletion-related section.



% Deletes (asynchronously: "fire and forget") the WOOPER instance(s) potentially
% stored in the specified attribute list.
%
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
%
% Ex: in a destructor: DeleteState = delete_any_instance_referenced_in( [
% first_pid_attr, second_pid_attr ], State ) or
% delete_any_instance_referenced_in( my_pid_attr, State ).
%
% (helper)
%
-spec delete_any_instance_referenced_in( [ attribute_name() ],
										 wooper:state() ) -> wooper:state().
delete_any_instance_referenced_in( _Attributes=[], State ) ->
	State;


delete_any_instance_referenced_in( [ PidAttribute | T ], State ) ->

	NewState = case ?getAttr(PidAttribute) of

		undefined ->
			State;

		Pid when is_pid( Pid ) ->
			Pid ! delete,
			setAttribute( State, PidAttribute, undefined )

	end,
	delete_any_instance_referenced_in( T, NewState );


delete_any_instance_referenced_in( PidAttribute, State ) ->

	case ?getAttr(PidAttribute) of

		undefined ->
			State;

		Pid when is_pid( Pid ) ->
			Pid ! delete,
			setAttribute( State, PidAttribute, undefined )

	end.





% Deletes (synchronously, in a parallel yet blocking manner) the WOOPER
% instance(s) potentially stored in specified attribute list (a standalone
% attribute may be specified as well).
%
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
%
% Ex: in a destructor: NewState =
% delete_synchronously_any_instance_referenced_in( [ first_pid_attr,
% second_pid_attr ], State ) or
% delete_synchronously_any_instance_referenced_in( my_pid_attr, State ).
%
-spec delete_synchronously_any_instance_referenced_in(
	[ attribute_name() ] | attribute_name(), wooper:state() ) -> wooper:state().
delete_synchronously_any_instance_referenced_in( Attributes, State ) ->
	delete_synchronously_any_instance_referenced_in( Attributes,
										_PreTestLiveliness=false, State ).


% Deletes safely (pre-testing whether the specified process still exists before
% attempting to delete it, in order to avoid having to wait for a synchronous
% time-out) and synchronously, in a parallel yet blocking manner, the WOOPER
% instance(s) potentially stored in the specified attribute list (a standalone
% attribute may be specified as well instead).
%
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
%
% Ex: in a destructor: NewState =
% delete_synchronously_any_instance_referenced_in( [ first_pid_attr,
% second_pid_attr ], SomeState ) or
% safe_delete_synchronously_any_instance_referenced_in( my_pid_attr, State ).
%
-spec safe_delete_synchronously_any_instance_referenced_in(
	[ attribute_name() ] | attribute_name(), wooper:state() ) -> wooper:state().
safe_delete_synchronously_any_instance_referenced_in( Attributes, State ) ->
	delete_synchronously_any_instance_referenced_in( Attributes,
										 _PreTestLiveliness=true, State ).



% Deletes safely (if requested, pre-testing whether the specified process still
% exists before attempting to delete it, in order to avoid having to wait for a
% synchronous time-out) and synchronously, in a parallel yet blocking manner,
% the WOOPER instance(s) potentially stored in the specified attribute list (a
% standalone attribute may be specified as well instead).
%
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
%
% Ex: in a destructor: NewState =
% delete_synchronously_any_instance_referenced_in( [ first_pid_attr,
% second_pid_attr ], SomeState ) or
% delete_synchronously_any_instance_referenced_in( my_pid_attr, State ).
%
delete_synchronously_any_instance_referenced_in( _Attributes=[],
												 _PreTestLiveliness, State ) ->
	State;


delete_synchronously_any_instance_referenced_in( Attributes, PreTestLiveliness,
									State ) when is_list( Attributes ) ->

	% Triggers the deletion of selected instances:
	{ TargetAttributes, TargetPids } =
		delete_pid_from( Attributes, PreTestLiveliness, State ),

	%trace_bridge:debug_fmt(
	%  "delete_synchronously_any_instance_referenced_in:~n"
	%  " - attributes are: ~p~n"
	%  " - PIDs are: ~p~n"
	%  " - time-out is ~p (ms), i.e. ~ts",
	%  [ TargetAttributes, TargetPids, ?synchronous_time_out,
	%	time_utils:duration_to_string( ?synchronous_time_out ) ] ),

	% Waits for their completion:
	wait_for_deletion_ack( TargetPids ),

	%trace_bridge:debug_fmt( "(all deletion acks received for ~p)",
	%                       [ TargetAttributes ] ),

	% Erases deleted PIDs:
	UndefinedAttributes = [ { AttrName, undefined }
								|| AttrName <- TargetAttributes ],

	setAttributes( State, UndefinedAttributes );


delete_synchronously_any_instance_referenced_in( Attribute, PreTestLiveliness,
												 State ) ->
	delete_synchronously_any_instance_referenced_in( [ Attribute ],
									   PreTestLiveliness, State ).




% Helper, which sends delete messages to all PIDs found in the list of
% attributes, and returns a list of the attributes and a list of the PIDs.
%
% If PreTestLiveliness is true, checks first that the process is not already
% dead, to avoid waiting for a synchronous time-out.
%
delete_pid_from( Attributes, PreTestLiveliness, State ) ->

	DeleteMessage = { synchronous_delete, self() },

	delete_pid_from( Attributes, DeleteMessage, PreTestLiveliness, State,
					 _AccAttr=[], _AccPid=[] ).


delete_pid_from( _Attributes=[], _DeleteMessage, _PreTestLiveliness, _State,
				 AccAttr, AccPid ) ->
	{ AccAttr, AccPid };

delete_pid_from( [ Attr | T ], DeleteMessage, PreTestLiveliness, State,
				 AccAttr, AccPid ) ->

	case ?getAttr( Attr ) of

		undefined ->
			delete_pid_from( T, DeleteMessage, PreTestLiveliness, State,
							 AccAttr, AccPid ) ;

		Pid when is_pid( Pid ) ->

			NodeOfPid = node( Pid ),

			case PreTestLiveliness andalso
				not basic_utils:is_alive( Pid, NodeOfPid, _Verbose=false ) of

				% Only case where no deletion oneway shall be sent:
				true ->
					%trace_bridge:debug_fmt(
					%  "(PID ~w was already dead, nothing done)", [ Pid ] ),
					delete_pid_from( T, DeleteMessage, PreTestLiveliness,
									 State, [ Attr | AccAttr ], AccPid );

				false ->
					%trace_bridge:debug_fmt( "Sending sync delete now ~ts "
					%                       "(PID: ~w).", [ Attr, Pid ] ),
					Pid ! DeleteMessage,
					delete_pid_from( T, DeleteMessage, PreTestLiveliness,
							 State, [ Attr | AccAttr ], [ Pid | AccPid ] )

			end

	end.



% Deletes specified instance synchronously.
%
% Will wait forever the effective termination of the specified instance.
%
-spec delete_synchronously_instance( instance_pid() ) -> void().
delete_synchronously_instance( InstancePid ) ->

	%trace_bridge:debug_fmt( "delete_synchronously_instance for ~w.",
	%                       [ InstancePid ] ),

	InstancePid ! { synchronous_delete, self() },

	receive

		{ deleted, InstancePid } ->
			%trace_bridge:debug_fmt( "Synchronous deletion of ~w confirmed.",
			%						[ Pid ] ),
			ok

	end.



% Deletes specified instances synchronously (yet in parallel).
%
% Will wait forever the effective termination of all instances (and will
% regularly write a message on the console if waiting for too long).
%
% (exported helper)
%
-spec delete_synchronously_instances( [ instance_pid() ] ) -> void().
delete_synchronously_instances( InstanceList ) ->

	%trace_bridge:debug_fmt( "delete_synchronously_instances for ~p.",
	%                       [ InstanceList ] ),

	DeleteMessage = { synchronous_delete, self() },

	[ I ! DeleteMessage || I <- InstanceList ],

	wait_for_deletion_ack( InstanceList ).



% Helper used to wait for the receiving of all deletion acknowledgements:
%
% Could almost use basic_utils:wait_for_acks/3.
%
wait_for_deletion_ack( _WaitedPids=[] ) ->
	ok;

wait_for_deletion_ack( WaitedPids ) ->

	receive

		{ deleted, Pid } ->

			case lists:member( Pid, WaitedPids ) of

				false ->
					throw( { unexpected_pid_deletion, Pid } );

				true ->
					NewWaitedPids = lists:delete( Pid, WaitedPids ),
					wait_for_deletion_ack( NewWaitedPids )

			end

	% Note that this time-out is reset at each ack:
	after ?synchronous_time_out ->

		case examine_waited_deletions( WaitedPids, _Acc=[] ) of

			[] ->
				ok;

			NewWaitedPids ->
				trace_bridge:debug_fmt(
				  "(still waiting for the synchronous deletion of "
				  "following live WOOPER instance(s): ~p)", [ NewWaitedPids ] ),

				% Warns, but does not trigger failures:
				wait_for_deletion_ack( NewWaitedPids )

		end

	end.



examine_waited_deletions( _WaitedPids=[], Acc ) ->
	Acc;

examine_waited_deletions( _WaitedPids=[ Pid | T ], Acc ) ->

	%trace_bridge:debug_fmt( "Testing whether ~p is alive...", [ Pid ] ),

	% Manages processes that are not local as well:
	case basic_utils:is_alive( Pid ) of

		true ->
			examine_waited_deletions( T, [ Pid | Acc ] );

		false ->
			trace_bridge:debug_fmt(
			  "Stopped waiting for the deletion of instance "
			  "whose PID is ~p: not found alive.", [ Pid ] ),

			examine_waited_deletions( T, Acc )

	end.



% Deletes specified instances synchronously (yet in parallel), safely, knowing
% there might be duplicates in the specified list and that some instances may
% even be already dead.
%
% Will wait forever the effective termination of all instances (and will
% regularly write a message on the console if waiting for too long) .
%
% (exported helper)
%
-spec safe_delete_synchronously_instances( [ instance_pid() ] ) -> void().
safe_delete_synchronously_instances( InstanceList ) ->

	% Testing for liveliness allows to avoid synchronous time-outs:
	FilteredInstanceList = [ InstancePid
					 || InstancePid <- list_utils:uniquify( InstanceList ),
						basic_utils:is_alive( InstancePid ) ],

	delete_synchronously_instances( FilteredInstanceList ).



% Deletes specified passive instance.
-spec delete_passive( passive_instance() ) -> void().
delete_passive( _PassiveInstance ) ->
	%trace_bridge:info( "Passive instance deleted." ),
	ok.



% These functions are stubs, they shall never be called, as the WOOPER parse
% transform is supposed to have replaced them at compilation-time.


-spec return_state_result( any(), any() ) -> no_return().
return_state_result( _State, _Result ) ->
	throw( { untransformed_method_terminator, return_state_result } ).


-spec return_state( any() ) -> no_return().
return_state( _State ) ->
	throw( { untransformed_method_terminator, return_state_result } ).


-spec return_static( any() ) -> no_return().
return_static( _Value ) ->
	throw( { untransformed_method_terminator, return_static } ).


-spec const_return_result( any() ) -> no_return().
const_return_result( _Value ) ->
	throw( { untransformed_method_terminator, const_return_result } ).


-spec const_return() -> no_return().
const_return() ->
	throw( { untransformed_method_terminator, const_return } ).



% Returns a set containing pairs whose first element is the name of a function
% exported from the wooper module, and whose second element is its corresponding
% arity (of course multiple functions might share the same name but rely on
% different arities).
%
% Typically useful to better intercept user errors in method terminators.
%
-spec get_exported_functions_set() -> function_export_set().
get_exported_functions_set() ->
	set_utils:new( meta_utils:list_exported_functions( ?MODULE ) ).



% Checks that specified attribute is indeed equal to 'undefined'.
-spec check_undefined( attribute_name(), wooper:state() ) -> void().
check_undefined( AttributeName, State ) ->

	try

		undefined = ?getAttr(AttributeName)

	catch

		exit:{ { badmatch, UnexpectedValue }, Stack } ->

			% Attribute value was not equal to 'undefined':
			throw( { attribute_was_not_undefined,
					 { AttributeName, UnexpectedValue }, Stack } );

		exit:Error ->
			% Other error (ex: unknown attribute):
			throw( { attribute_error, AttributeName, Error } );

		OtherError ->
			throw( { unexpected_attribute_error, AttributeName, OtherError } )

	end.


% Checks that all specified attributes are indeed equal to 'undefined'.
-spec check_all_undefined( [ attribute_name() ], wooper:state() ) -> void().
check_all_undefined( AttributeNames, State ) ->
	[ check_undefined( Attr, State ) || Attr <- AttributeNames ].
