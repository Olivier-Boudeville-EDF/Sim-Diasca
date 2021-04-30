% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-Traces library.
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
% Creation date: July 1, 2007.


% This module gathers all code, common to tests and applications, that allows to
% lighten the trace macros or to share defines and types.
%
-module(traces).


-export([ get_trace_filename/1,
		  receive_applicative_message/0, receive_applicative_message/1,
		  check_pending_wooper_results/0, declare_beam_dirs_for_traces/0,
		  manage_supervision/0, get_execution_target/0 ]).


-type emitter_name() :: text_utils:ustring().
-type emitter_categorization() :: text_utils:ustring().

-type emitter_info() :: { emitter_name(), emitter_categorization() }.

-type app_timestamp() :: trace_utils:trace_timestamp().

-type time() :: text_utils:ustring().

-type location() :: text_utils:ustring().


% A message may or may not (which is the default) by categorized:
-type message_categorization() :: text_utils:ustring() | 'uncategorized'.


% Numerical:
-type priority() :: trace_utils:trace_priority().

-type message() :: text_utils:ustring().

% 'emergency', 'alert', and all:
-type trace_severity() :: trace_utils:trace_severity().



% Text traces are either in pure, raw text, or in PDF:
-type trace_text_type() :: 'text_only' | 'pdf'.



% A trace type must be selected so that, when the traces are aggregated, the
% corresponding output is compliant with the tools to be used for supervision.
%
% So the trace type to select depends on whether a dedicated, advanced trace
% tool should be used to browse the execution traces, or just a text viewer
% (possibly with a PDF displaying thereof); indeed it is:
%
% - either 'advanced_traces', for traces typically expected to be read from the
% LogMX tool (relying then on our parser); see http://logmx.com/
%
% - or {'text_traces', trace_text_type()}
%
-type trace_supervision_type() :: 'advanced_traces'
								| { 'text_traces', trace_text_type() }.


-export_type([ emitter_name/0, emitter_categorization/0, emitter_info/0,
			   app_timestamp/0, time/0, location/0, message_categorization/0,
			   priority/0, message/0, trace_severity/0,
			   trace_supervision_type/0 ]).


% Logger-related API (see https://erlang.org/doc/apps/kernel/logger_chapter.html
% abd Myriad's trace_utils):
%
-export([ set_handler/0, set_handler/1, add_handler/0, add_handler/1,
		  reset_handler/0, log/2 ]).


% Handler id:
-define( traces_logger_id, ceylan_traces_logger_handler_id ).


% To define get_execution_target/0:
-include_lib("myriad/include/utils/basic_utils.hrl").


% For notify_warning_fmt:
-include("traces.hrl").


% Shorthand:
-type aggregator_pid() :: class_TraceAggregator:aggregator_pid().



% Returns the name of the file in which traces will be written:
-spec get_trace_filename( basic_utils:module_name() ) ->
								file_utils:file_name().
get_trace_filename( ModuleName ) ->
	atom_to_list( ModuleName ) ++ ?TraceExtension.





% Receives an applicative, non-trace message, to protect user messages from the
% trace ones.
%
-spec receive_applicative_message() -> any().
receive_applicative_message() ->

	receive

		{ wooper_result, V } when V /= monitor_ok ->
			V

	end.



% Receives specified applicative, non-trace message, to protect user messages
% from the trace ones.
%
% Used for synchronization purpose.
%
-spec receive_applicative_message( any() ) -> void().
receive_applicative_message( Message=monitor_ok ) ->
	% Would interfere with the monitoring system:
	throw( { invalid_applicative_message, Message } );

receive_applicative_message( Message ) ->
	receive

		{ wooper_result, Message } ->
			message_received

	end.



% Displays and flushes all remaining WOOPER results.
%
% Defined here, since uses a trace.
%
-spec check_pending_wooper_results() -> void().
check_pending_wooper_results() ->

	receive

		{ wooper_result, AResult }  ->

			?notify_warning_fmt( "Following WOOPER result was unread: ~p.~n",
								 [ AResult ] ),

			check_pending_wooper_results()

	after

		0 ->
			ok

	end.



% Declares automatically the relevant BEAM directories in the code path, so that
% Ceylan-Traces can be fully usable from then on.
%
% Note:
%
% - the code_utils.beam module of Ceylan-Myriad must be available from the
% current code path
%
% - the CEYLAN_MYRIAD, CEYLAN_WOOPER and CEYLAN_TRACES environment variables
% must be defined and must point to the respective root directories
%
% - the determined directories are not specifically checked for existence,
% and are added at the end of the code path
%
-spec declare_beam_dirs_for_traces() -> void().
declare_beam_dirs_for_traces() ->

	% Not wanting to depend also on wooper.beam:
	%wooper:declare_beam_dirs_for_wooper(),
	code_utils:declare_beam_dirs_for( "CEYLAN_WOOPER" ),

	code_utils:declare_beam_dirs_for( "CEYLAN_TRACES" ).



% Manages the supervision of traces, typically in an OTP context, where:
% - the trace aggregator is expected to be already running
% - by default no specific trace file can be defined by the user, as
% applications are just started or not
%
% Note: currently not useful, as implicitly managed by traces_app:start/2.
%
-spec manage_supervision() -> maybe( class_TraceSupervisor:supervisor_pid() ).
manage_supervision() ->

	case executable_utils:is_batch() of

		true ->
			trace_utils:debug( "In batch mode, no trace supervisor launched." ),
			undefined;

		false ->
			trace_utils:debug(
			  "In interactive mode, so launching trace supervisor." ),

			% Expected to be already created:
			TraceAggregatorPid = class_TraceAggregator:get_aggregator(
									_CreateIfNotAvailable=false ),

			% Not blocking the calling process until the supervision is over:
			TraceAggregatorPid !
				{ launchTraceSupervisor, [], self() },

			% test_receive/1 not appropriate here (would filter the atom that we
			% expect):
			%
			receive

				{ wooper_result,
				  { trace_supervisor_launched, TraceSupervisorPid } } ->
					TraceSupervisorPid

			end

	end.




% Handler section for the integration of Erlang (newer) logger.
%
% Refer to https://erlang.org/doc/man/logger.html.


% Replaces the current (probably default) logger handler with this Traces one
% (registered as 'default'), based on the (supposedly already-existing) trace
% aggregator.
%
-spec set_handler() -> void().
set_handler() ->

	TargetHandler = default,

	case logger:remove_handler( TargetHandler ) of

		ok ->
			ok;

		{ error, RemoveErrReason } ->
			throw( { unable_to_remove_log_handler, RemoveErrReason,
					 TargetHandler } )

	end,

	case logger:add_handler( _HandlerId=default, _Module=?MODULE,
							 get_handler_config() ) of

		ok ->
			ok;

		{ error, AddErrReason, TargetHandler } ->
			throw( { unable_to_set_traces_log_handler, AddErrReason,
					 TargetHandler } )

	end.



% Replaces the current (probably default) logger handler with this Traces one
% (registered as 'default'), based on the specified trace aggregator.
%
-spec set_handler( aggregator_pid() ) -> void().
set_handler( AggregatorPid ) ->

	%trace_utils:debug_fmt( "Setting default handler for logger to "
	%	"aggregator ~w, on node: ~ts.", [ AggregatorPid, node() ] ),

	TargetHandler = default,

	case logger:remove_handler( TargetHandler ) of

		ok ->
			ok;

		{ error, RemoveErrReason } ->
			throw( { unable_to_remove_log_handler, RemoveErrReason,
					 TargetHandler } )

	end,

	case logger:add_handler( _HandlerId=default, _Module=?MODULE,
							 get_handler_config( AggregatorPid ) ) of

		ok ->
			ok;

		{ error, AddErrReason, TargetHandler } ->
			throw( { unable_to_set_traces_log_handler, AddErrReason,
					 TargetHandler } )

	end.



% Registers this Traces logger handler as an additional one (not replacing the
% default one), based on the (supposedly already-existing) trace aggregator.
%
-spec add_handler() -> void().
add_handler() ->

	case logger:add_handler( _HandlerId=?traces_logger_id, _Module=?MODULE,
							 get_handler_config() ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { unable_to_add_traces_log_handler, Reason } )

	end.



% Registers this Traces logger handler as an additional one (not replacing the
% default one), based on the specified trace aggregator.
%
-spec add_handler( aggregator_pid() ) -> void().
add_handler( AggregatorPid ) ->

	case logger:add_handler( _HandlerId=?traces_logger_id, _Module=?MODULE,
							 get_handler_config( AggregatorPid ) ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { unable_to_add_traces_log_handler, Reason } )

	end.



% Returns the (initial) configuration of the Traces logger handler, branching to
% the (supposedly already-existing) trace aggregator.
%
-spec get_handler_config() -> logger:handler_config().
get_handler_config() ->

	AggregatorPid = case class_TraceAggregator:get_aggregator() of

		APid when is_pid( APid ) ->
			APid;

		% Most probably trace_aggregator_not_found:
		Error ->
			throw( Error )

	end,

	get_handler_config( AggregatorPid ).



% Returns the (initial) configuration of the Traces logger handler, branching to
% the specified trace aggregator.
%
-spec get_handler_config( aggregator_pid() ) -> logger:handler_config().
get_handler_config( AggregatorPid ) ->

	#{ config => AggregatorPid
	   % Defaults:
	   % level => all,
	   % filter_default => log | stop,
	   % filters => [],
	   % formatter => {logger_formatter, DefaultFormatterConfig}

	   % Set by logger:
	   %id => HandlerId
	   %module => Module
	 }.



% Unsets the Traces logger handler, returns to the base trace_utils one.
-spec reset_handler() -> void().
reset_handler() ->

	%trace_utils:debug( "Resetting logger handler." ),

	% Remove then add:
	trace_utils:set_handler().


% Mandatory callback for log handlers.
%
% See https://erlang.org/doc/man/logger.html#HModule:log-2
%
-spec log( logger:log_event(), logger:handler_config() ) -> void().
log( _LogEvent=#{ level := Level,
				  %meta => #{error_logger => #{emulator => [...]
				  msg := Msg },
	 _Config=#{ config := TraceAggregatorPid } ) ->

	%io:format( "### Logging following event:~n ~p~n(with config: ~p).~n",
	%		   [ LogEvent, Config ] ),

	 TraceMsg = case Msg of

		{ report, Report } ->
			{ FmtStr, FmtValues } = logger:format_report( Report ),
			text_utils:format( FmtStr, FmtValues );

		  { string, S } ->
			S;

		{ FmtStr, FmtValues } ->
			text_utils:format( FmtStr, FmtValues );

		Other ->
			throw( { unexpected_log_message, Other } )

	 end,

	% Directly the same now:
	Severity = Level,

	%io:format( "### Logging following event:~n ~p~n(with config: ~p)~n "
	%  "resulting in: '~ts' (severity: ~p).",
	%  [ LogEvent, Config, TraceMsg, Severity ] ),

	BinEmitterCategorization = <<"Erlang logger">>,

	% Trying to induce as low overhead as possible (alternatively separate
	% per-level handlers could be set); yet finally we prefer never losing any
	% message (even of lower priority), and anyway messages going through logger
	% are quite infrequent - at least in our use cases - so we have them all
	% properly synchronised so that none can be lost in case of crash:
	%
	%case Level =:= notice orelse Level =:= info orelse Level =:= debug of
	%
	%	% Lighter, quicker:
	%	true ->
	%		class_TraceEmitter:send_direct( Severity, TraceMsg,
	%			BinEmitterCategorization, TraceAggregatorPid );
	%
	%	_False ->
			% Sent as soon as possible:
			class_TraceEmitter:send_direct_synchronisable( Severity, TraceMsg,
				BinEmitterCategorization, TraceAggregatorPid ),

	%end,

	trace_utils:echo( TraceMsg, Severity, "erlang_logger" ),

	% Received (from send_direct_synchronisable/4) as late as possible:
	receive

		{ wooper_result, trace_aggregator_synchronised } ->
			%trace_utils:debug_fmt( "Synchronised from logger for "
			%	"message '~p'.", [ TraceMsg ] ),
			ok

	end;


log( LogEvent, _Config ) ->
	throw( { unexpected_log_event, LogEvent } ).
