% Copyright (C) 2016-2021 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Authors: Robin Huart (robin-externe.huart@edf.fr)
%          Olivier Boudeville (olivier.boudeville@edf.fr)


% Module storing all the helper functions facilitating the support of the Java
% binding API, relying on JInterface.
%
-module(java_binding_utils).


% Exports of helpers:
-export([ execute_request/4, execute_request_locally/3,
		  execute_request_locally/4 ]).


% For trace generations (involving a WOOPER state):
-include_lib("traces/include/class_TraceEmitter.hrl").



% For trace notifications (stand-alone):
-include_lib("traces/include/traces.hrl").


% Implementation notes:
%
% Based on java_utils.


% Shorthands:

-type request_name() :: java_utils:request_name().
-type request_result() :: java_utils:request_result().
-type result() :: java_utils:request_result().

-type java_mbox_pid() :: java_utils:java_mbox_pid().

-type emitter_categorization() :: traces:emitter_categorization().



% Sends a request to the specified Java OtpMailbox (i.e. typically a worker
% mailbox, otherwise the controller one).
%
% Note: trace messages received while this request is being processed are
% managed on the fly (directly, i.e. their processing is not postponed).
%
-spec execute_request( java_mbox_pid(), request_name(), request_result(),
		emitter_categorization() | wooper:state() ) -> result().
% Here we have just a trace categorization:
execute_request( MailboxPid, RequestName, RequestParams, TraceCat )
  when is_list( TraceCat ) ->

	trace_utils:debug_fmt( "Executing request '~ts' with parameters ~p "
		"(categorization: ~ts).",
		[ RequestName, RequestParams, TraceCat ] ),

	java_utils:send_oneway( MailboxPid, RequestName, RequestParams ),

	handle_request_results( MailboxPid, RequestName, TraceCat );


% Here we have an actual State to rely on:
execute_request( MailboxPid, RequestName, RequestParams, State ) ->

	trace_utils:debug_fmt( "Executing request '~ts' with parameters ~p.",
						   [ RequestName, RequestParams ] ),

	java_utils:send_oneway( MailboxPid, RequestName, RequestParams ),

	handle_request_results( MailboxPid, RequestName, State ).



% Executes the same kind of request as above, but in any locally-available Java
% worker mailbox.
%
% Hence this function is only relevant for "stateless" requests, i.e. requests
% that do not rely on the state of a particular Jinterface mailbox (ex: static
% methods only).
%
-spec execute_request_locally( request_name(), request_result(),
			emitter_categorization() | wooper:state() ) ->
									{ result(), java_mbox_pid() }.
execute_request_locally( RequestName, RequestParams, TraceCatOrState ) ->

	% Gets the PID of the global instance managing Java resources:
	JavaBindingManagerPid = class_JavaBindingManager:get_registered_manager(),

	execute_request_locally( RequestName, RequestParams, JavaBindingManagerPid,
							 TraceCatOrState ).



% Executes the same kind of request as above, but in any locally-available Java
% worker mailbox.
%
% Hence this function is only relevant for requests that do not rely on the
% state of a particular Jinterface mailbox (ex: static methods only).
%
-spec execute_request_locally( request_name(), request_result(),
			class_JavaBindingManager:manager_pid(),
			emitter_categorization() | wooper:state() ) ->
										{ result(), java_mbox_pid() }.
execute_request_locally( RequestName, RequestParams, JavaBindingManagerPid,
						 TraceCatOrState ) ->

	% We know here the Java binding manager.

	% Selects one of the active local mailboxes (could have been the controller
	% one as well, yet we prefer a worker one):
	%
	% FIXME: re-enable once workers are ready, Java-side.
	%MailboxPid = class_JavaBindingManager:get_any_worker_mailbox(
	MailboxPid =
		class_JavaBindingManager:get_controller_mbox( JavaBindingManagerPid ),

	% Executes a classical request in the selected worker thread:
	Result = execute_request( MailboxPid, RequestName, RequestParams,
							  TraceCatOrState ),

	{ Result, MailboxPid }.



% Recursive listener transmitting trace messages sent from Java, to be used
% while performing a request to a Java mailbox in order to wait for its
% corresponding answer.
%
% Stops as soon as the request is successfully completed, or an error message is
% received, or an exception has been raised by the process hosting the mailbox.
%
-spec handle_request_results( java_mbox_pid(), request_name(),
				emitter_categorization() | wooper:state() ) -> result().
handle_request_results( MailboxPid, RequestName, TraceEmitterCategorization )
  when is_list( TraceEmitterCategorization ) ->

	trace_utils:debug_fmt( "Waiting for the result of request '~ts', from ~w.",
						   [ RequestName, MailboxPid ] ),

	case java_utils:wait_for_request_result( MailboxPid, RequestName ) of

		{ request_completed, ReceivedData } ->
			trace_utils:debug_fmt( "Result from ~w, for request '~ts':~p.",
								   [ MailboxPid, RequestName, ReceivedData ] ),
			ReceivedData;

		{ trace_emitted, debug, TraceFormattedMessage } ->
			?notify_debug_cat( TraceFormattedMessage,
							   TraceEmitterCategorization ),
			handle_request_results( MailboxPid, RequestName,
									TraceEmitterCategorization );

		{ trace_emitted, info, TraceFormattedMessage } ->
			?notify_info_cat( TraceFormattedMessage,
							  TraceEmitterCategorization ),
			handle_request_results( MailboxPid, RequestName,
									TraceEmitterCategorization );

		{ trace_emitted, notice, TraceFormattedMessage } ->
			?notify_notice_cat( TraceFormattedMessage,
								TraceEmitterCategorization ),
			handle_request_results( MailboxPid, RequestName,
									TraceEmitterCategorization );

		{ trace_emitted, warning, TraceFormattedMessage } ->
			?notify_warning_cat( TraceFormattedMessage,
								 TraceEmitterCategorization ),
			handle_request_results( MailboxPid, RequestName,
									TraceEmitterCategorization );

		{ trace_emitted, error, TraceFormattedMessage } ->
			?notify_error_cat( TraceFormattedMessage,
							   TraceEmitterCategorization ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_error_raised, TraceFormattedMessage } );

		{ trace_emitted, critical, TraceFormattedMessage } ->
			?notify_critical_cat( TraceFormattedMessage,
							   TraceEmitterCategorization ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_critical_error_raised, TraceFormattedMessage } );

		{ trace_emitted, alert, TraceFormattedMessage } ->
			?notify_alert_cat( TraceFormattedMessage,
							   TraceEmitterCategorization ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_alert_error_raised, TraceFormattedMessage } );

		{ trace_emitted, emergency, TraceFormattedMessage } ->
			?notify_emergency_cat( TraceFormattedMessage,
							   TraceEmitterCategorization ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_emergency_error_raised, TraceFormattedMessage } );

		{ trace_emitted, OtherTraceType, TraceFormattedMessage } ->
			?notify_warning_fmt_cat(
			   "Invalid trace received from Java: the trace type '~p' is not "
			   "known; the original trace message is:~n~n'~ts'.",
			   [ OtherTraceType, TraceFormattedMessage ],
			   TraceEmitterCategorization ),
			handle_request_results( MailboxPid, RequestName,
									TraceEmitterCategorization );

		{ exception_raised, ExceptionType, ExceptionFormattedMessage } ->
			?notify_error_cat( ExceptionFormattedMessage,
							   TraceEmitterCategorization ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_exception_raised, ExceptionType } )

	end;

% Here the third element is a state, not a TraceEmitterCategorization:
handle_request_results( MailboxPid, RequestName, State ) ->

	case java_utils:wait_for_request_result( MailboxPid, RequestName ) of

		{ request_completed, ReceivedData } ->
			ReceivedData;

		{ trace_emitted, debug, TraceFormattedMessage } ->
			?debug( TraceFormattedMessage ),
			handle_request_results( MailboxPid, RequestName, State );

		{ trace_emitted, info, TraceFormattedMessage } ->
			?info( TraceFormattedMessage ),
			handle_request_results( MailboxPid, RequestName, State );

		{ trace_emitted, notice, TraceFormattedMessage } ->
			?notice( TraceFormattedMessage ),
			handle_request_results( MailboxPid, RequestName, State );

		{ trace_emitted, warning, TraceFormattedMessage } ->
			?warning( TraceFormattedMessage ),
			handle_request_results( MailboxPid, RequestName, State );

		{ trace_emitted, error, TraceFormattedMessage } ->
			?error( TraceFormattedMessage ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_error, TraceFormattedMessage } );

		{ trace_emitted, critical, TraceFormattedMessage } ->
			?critical( TraceFormattedMessage ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_critical_raised, TraceFormattedMessage } );

		{ trace_emitted, alert, TraceFormattedMessage } ->
			?alert( TraceFormattedMessage ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_alert_raised, TraceFormattedMessage } );

		{ trace_emitted, emergency, TraceFormattedMessage } ->
			?emergency( TraceFormattedMessage ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_emergency_raised, TraceFormattedMessage } );

		{ trace_emitted, OtherTraceType, TraceFormattedMessage } ->
			?warning_fmt( "Invalid trace received from Java: the trace type "
				"'~p' is not known; the original trace message is: ~n~n'~ts'.",
				[ OtherTraceType, TraceFormattedMessage ] ),
			handle_request_results( MailboxPid, RequestName, State );

		{ exception_raised, ExceptionType, ExceptionFormattedMessage } ->
			?error( ExceptionFormattedMessage ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_exception_raised, ExceptionType } )

	end.
