% Copyright (C) 2017-2021 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
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
% Creation date: Friday, November 1, 2013.



% Gathering of various very low-level trace-related facilities on the console.
% These are runtime logs; they are not related to the Erlang tracing subsystem.
%
% The API functions provided by the current module are mostly useful so that
% their call can be replaced by calls to the far more advanced facilities of the
% 'Ceylan-Traces' layer with no change in their parameters.
%
% For example, 'trace_utils:debug_fmt("I am the ~B.", [1])' may be replaced by
% '?debug_fmt("I am the ~B.", [1])' to switch, in a trace emitter instance, from
% basic traces output on the console to traces sent through the Traces
% subsystem.
%
% As a result, for a trace severity S in [debug, info, notice, warning, error,
% critical, alert, emergency, void], 'trace_utils:S' may be replaced as a whole
% by '?S' to promote a very debug-oriented trace into a potentially more durable
% one.
%
% Note that a given trace emission can be fully disabled (with no remaining
% resource consumption at all) thanks to the cond_utils:if_defined* primitives.
%
% This module is also a logger one, see
% https://erlang.org/doc/apps/kernel/logger_chapter.html.
%
% See trace_utils_test.erl for testing.
%
-module(trace_utils).


% To resolve name clash:
-compile( { no_auto_import, [ error/1 ] } ).


% Shorthand:
-type ustring() :: text_utils:ustring().


% An actual trace message:
-type trace_message() :: ustring().


% Defining, according to the Erlang newer logger API and thus in accordance with
% the Syslog protocol (RFC 5424), 8 levels of severity (plus a 'void' one), from
% least important to most: debug, info, notice, warning, error, critical, alert
% and emergency (void being always muted):
%
% See also standard_logger_level_to_severity/1 for a proper conversion.
%
-type trace_severity() ::

		% For debug-level messages:
		'debug'

		% For informational, lower-level messages:
	  | 'info'

		% For normal yet significant conditions:
	  | 'notice'

		% For warning conditions:
	  | 'warning'

		% For error conditions:
	  | 'error'

		% For critical conditions:
	  | 'critical'

		% For actions that must be taken immediately:
	  | 'alert'

		% Highest criticity, when system became unusable:
	  | 'emergency'

	  % For messages that shall be fully muted (disabled):
	  | 'void'.



% A format with quantifiers (such as ~p):
-type trace_format() :: text_utils:format_string().

% Values corresponding to format quantifiers:
-type trace_values() :: text_utils:format_values().

% Categorization of a trace message:
-type trace_message_categorization() :: ustring().

% An applicative timestamp for a trace; it can be anything (ex: integer() |
% 'none'), no constraint applies on purpose, so that any kind of
% application-specific timestamps can be elected.
%
-type trace_timestamp() :: any().

% Not including the 'void' severity here:
-type trace_priority() :: 0..7.

-export_type([ trace_message/0, trace_severity/0,
			   trace_format/0, trace_values/0, trace_message_categorization/0,
			   trace_timestamp/0, trace_priority/0 ]).



-export([ debug/1, debug_fmt/2, debug_categorized/2, debug_categorized_timed/3,

		  info/1, info_fmt/2, info_categorized/2, info_categorized_timed/3,

		  notice/1, notice_fmt/2, notice_categorized/2,
		  notice_categorized_timed/3,

		  warning/1, warning_fmt/2, warning_categorized/2,
		  warning_categorized_timed/3,

		  error/1, error_fmt/2, error_categorized/2, error_categorized_timed/3,

		  critical/1, critical_fmt/2, critical_categorized/2,
		  critical_categorized_timed/3,

		  alert/1, alert_fmt/2, alert_categorized/2, alert_categorized_timed/3,

		  emergency/1, emergency_fmt/2, emergency_categorized/2,
		  emergency_categorized_timed/3,

		  void/1, void_fmt/2, void_categorized/2, void_categorized_timed/3,

		  echo/2, echo/3, echo/4,

		  get_priority_for/1, get_severity_for/1, is_error_like/1 ]).


% Logger-related API (see
% https://erlang.org/doc/apps/kernel/logger_chapter.html):
%
-export([ set_handler/0, add_handler/0, log/2 ]).


% Handler id:
-define( myriad_logger_id, ceylan_myriad_logger_handler_id ).


% At least for error cases, ellipsing traces is not a good idea; as we are
% relying here on mere console outputs, the ellipsing of other traces may be
% relevant (this is the default here):
%
-ifdef(myriad_unellipsed_traces).

 % Disables the ellipsing of traces:
 -define( ellipse_length, unlimited ).

-else.

 % Default:
 -define( ellipse_length, 2000 ).

-endif.



% Implementation notes:
%
% Compared to mere io:format/{1,2} calls, these trace primitives add
% automatically the trace type (ex: "[debug] ") at the beginning of the message,
% finish it with a carriage-return/line-feed, and for the most important trace
% types, try to ensure that they are synchronous (blocking).
%
% Traces of lesser importance are ellipsed, as the console output does not allow
% to browse them conveniently.


% Outputs specified debug message.
-spec debug( trace_message() ) -> void().
debug( Message ) ->
	actual_display( "[debug] " ++ Message ).


% Outputs specified formatted debug message.
-spec debug_fmt( trace_format(), trace_values() ) -> void().
debug_fmt( Format, Values ) ->
	actual_display( "[debug] " ++ Format, Values ).


% Outputs specified debug message, with specified message categorization.
-spec debug_categorized( trace_message(), trace_message_categorization() ) ->
							void().
debug_categorized( Message, _MessageCategorization=uncategorized ) ->
	actual_display( "[debug] ~ts", [ Message ] );

debug_categorized( Message, MessageCategorization ) ->
	actual_display( "[debug][~ts] ~ts", [ MessageCategorization, Message ] ).


% Outputs specified debug message, with specified message categorization and
% time information.
%
-spec debug_categorized_timed( trace_message(), trace_message_categorization(),
							   trace_timestamp() ) -> void().
debug_categorized_timed( Message, _MessageCategorization=uncategorized,
						 Timestamp ) ->
	actual_display( "[debug][at ~ts] ~ts", [ Timestamp, Message ] );

debug_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	actual_display( "[debug][~ts][at ~ts] ~ts",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified info message.
-spec info( trace_message() ) -> void().
info( Message ) ->
	actual_display( "[info] " ++ Message ).


% Outputs specified formatted info message.
-spec info_fmt( trace_format(), trace_values() ) -> void().
info_fmt( Format, Values ) ->
	actual_display( "[info] " ++ Format, Values ).


% Outputs specified info message, with specified message categorization.
-spec info_categorized( trace_message(), trace_message_categorization() ) ->
							void().
info_categorized( Message, _MessageCategorization=uncategorized ) ->
	actual_display( "[info] ~ts", [ Message ] );

info_categorized( Message, MessageCategorization ) ->
	actual_display( "[info][~ts] ~ts", [ MessageCategorization, Message ] ).


% Outputs specified info message, with specified message categorization and
% time information.
%
-spec info_categorized_timed( trace_message(), trace_message_categorization(),
							  trace_timestamp() ) -> void().
info_categorized_timed( Message, _MessageCategorization=uncategorized,
						 Timestamp ) ->
	actual_display( "[info][at ~ts] ~ts", [ Timestamp, Message ] );

info_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	actual_display( "[info][~ts][at ~ts] ~ts",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified notice message.
-spec notice( trace_message() ) -> void().
notice( Message ) ->
	actual_display( "[notice] " ++ Message ).


% Outputs specified formatted notice message.
-spec notice_fmt( trace_format(), trace_values() ) -> void().
notice_fmt( Format, Values ) ->
	actual_display( "[notice] " ++ Format, Values ).


% Outputs specified notice message, with specified message categorization.
-spec notice_categorized( trace_message(), trace_message_categorization() ) ->
							void().
notice_categorized( Message, _MessageCategorization=uncategorized ) ->
	actual_display( "[notice] ~ts", [ Message ] );

notice_categorized( Message, MessageCategorization ) ->
	actual_display( "[notice][~ts] ~ts", [ MessageCategorization, Message ] ).


% Outputs specified notice message, with specified message categorization and
% time information.
%
-spec notice_categorized_timed( trace_message(), trace_message_categorization(),
								trace_timestamp() ) -> void().
notice_categorized_timed( Message, _MessageCategorization=uncategorized,
						Timestamp ) ->
	actual_display( "[notice][at ~ts] ~ts", [ Timestamp, Message ] );

notice_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	actual_display( "[notice][~ts][at ~ts] ~ts",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified warning message.
-spec warning( trace_message() ) -> void().
warning( Message ) ->
	severe_display( "[warning] " ++ Message ),
	system_utils:await_output_completion().


% Outputs specified formatted warning message.
-spec warning_fmt( trace_format(), trace_values() ) -> void().
warning_fmt( Format, Values ) ->
	severe_display( "[warning] " ++ Format, Values ),
	system_utils:await_output_completion().


% Outputs specified warning message, with specified message categorization.
-spec warning_categorized( trace_message(), trace_message_categorization() ) ->
								void().
warning_categorized( Message, _MessageCategorization=uncategorized ) ->
	severe_display( "[warning] ~ts", [ Message ] );

warning_categorized( Message, MessageCategorization ) ->
	severe_display( "[warning][~ts] ~ts", [ MessageCategorization, Message ] ).


% Outputs specified warning message, with specified message categorization and
% time information.
%
-spec warning_categorized_timed( trace_message(),
		trace_message_categorization(), trace_timestamp() ) -> void().
warning_categorized_timed( Message, _MessageCategorization=uncategorized,
						   Timestamp ) ->
	severe_display( "[warning][at ~ts] ~ts", [ Timestamp, Message ] );

warning_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	severe_display( "[warning][~ts][at ~ts] ~ts",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified error message.
-spec error( trace_message() ) -> void().
error( Message ) ->
	severe_display( "[error] " ++ Message ),
	system_utils:await_output_completion().


% Outputs specified formatted error message.
-spec error_fmt( trace_format(), trace_values() ) -> void().
error_fmt( Format, Values ) ->
	severe_display( "[error] " ++ Format, Values ),
	system_utils:await_output_completion().


% Outputs specified error message, with specified message categorization.
-spec error_categorized( trace_message(), trace_message_categorization() ) ->
							void().
error_categorized( Message, _MessageCategorization=uncategorized ) ->
	severe_display( "[error] ~ts", [ Message ] );

error_categorized( Message, MessageCategorization ) ->
	severe_display( "[error][~ts] ~ts", [ MessageCategorization, Message ] ).


% Outputs specified error message, with specified message categorization and
% time information.
%
-spec error_categorized_timed( trace_message(), trace_message_categorization(),
							   trace_timestamp() ) -> void().
error_categorized_timed( Message, _MessageCategorization=uncategorized,
						 Timestamp ) ->
	severe_display( "[error][at ~ts] ~ts", [ Timestamp, Message ] );

error_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	severe_display( "[error][~ts][at ~ts] ~ts",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified critical message.
-spec critical( trace_message() ) -> void().
critical( Message ) ->
	severe_display( "[critical] " ++ Message ),
	system_utils:await_output_completion().


% Outputs specified formatted critical message.
-spec critical_fmt( trace_format(), trace_values() ) -> void().
critical_fmt( Format, Values ) ->
	severe_display( "[critical] " ++ Format, Values ),
	system_utils:await_output_completion().


% Outputs specified critical message, with specified message categorization.
-spec critical_categorized( trace_message(), trace_message_categorization() ) ->
							void().
critical_categorized( Message, _MessageCategorization=uncategorized ) ->
	severe_display( "[critical] ~ts", [ Message ] );

critical_categorized( Message, MessageCategorization ) ->
	severe_display( "[critical][~ts] ~ts", [ MessageCategorization, Message ] ).


% Outputs specified critical message, with specified message categorization and
% time information.
%
-spec critical_categorized_timed( trace_message(),
		trace_message_categorization(), trace_timestamp() ) -> void().
critical_categorized_timed( Message, _MessageCategorization=uncategorized,
						 Timestamp ) ->
	severe_display( "[critical][at ~ts] ~ts", [ Timestamp, Message ] );

critical_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	severe_display( "[critical][~ts][at ~ts] ~ts",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified alert message.
-spec alert( trace_message() ) -> void().
alert( Message ) ->
	severe_display( "[alert] " ++ Message ),
	system_utils:await_output_completion().


% Outputs specified formatted alert message.
-spec alert_fmt( trace_format(), trace_values() ) -> void().
alert_fmt( Format, Values ) ->
	severe_display( "[alert] " ++ Format, Values ),
	system_utils:await_output_completion().


% Outputs specified alert message, with specified message categorization.
-spec alert_categorized( trace_message(), trace_message_categorization() ) ->
							void().
alert_categorized( Message, _MessageCategorization=uncategorized ) ->
	severe_display( "[alert] ~ts", [ Message ] );

alert_categorized( Message, MessageCategorization ) ->
	severe_display( "[alert][~ts] ~ts", [ MessageCategorization, Message ] ).


% Outputs specified alert message, with specified message categorization and
% time information.
%
-spec alert_categorized_timed( trace_message(), trace_message_categorization(),
							   trace_timestamp() ) -> void().
alert_categorized_timed( Message, _MessageCategorization=uncategorized,
						 Timestamp ) ->
	severe_display( "[alert][at ~ts] ~ts", [ Timestamp, Message ] );

alert_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	severe_display( "[alert][~ts][at ~ts] ~ts",
					[ MessageCategorization, Timestamp, Message ] ).



% Outputs specified emergency message.
-spec emergency( trace_message() ) -> void().
emergency( Message ) ->
	severe_display( "[emergency] " ++ Message ),
	system_utils:await_output_completion().


% Outputs specified formatted emergency message.
-spec emergency_fmt( trace_format(), trace_values() ) -> void().
emergency_fmt( Format, Values ) ->
	severe_display( "[emergency] " ++ Format, Values ),
	system_utils:await_output_completion().


% Outputs specified emergency message, with specified message categorization.
-spec emergency_categorized( trace_message(),
							 trace_message_categorization() ) -> void().
emergency_categorized( Message, _MessageCategorization=uncategorized ) ->
	severe_display( "[emergency] ~ts", [ Message ] );

emergency_categorized( Message, MessageCategorization ) ->
	severe_display( "[emergency][~ts] ~ts",
					[ MessageCategorization, Message ] ).


% Outputs specified emergency message, with specified message categorization and
% time information.
%
-spec emergency_categorized_timed( trace_message(),
			   trace_message_categorization(), trace_timestamp() ) -> void().
emergency_categorized_timed( Message, _MessageCategorization=uncategorized,
							 Timestamp ) ->
	severe_display( "[emergency][at ~ts] ~ts", [ Timestamp, Message ] );

emergency_categorized_timed( Message, MessageCategorization, Timestamp ) ->
	severe_display( "[emergency][~ts][at ~ts] ~ts",
					[ MessageCategorization, Timestamp, Message ] ).



% "Outputs" specified void message.
-spec void( trace_message() ) -> void().
void( _Message ) ->
	ok.


% "Outputs" specified formatted void message.
-spec void_fmt( trace_format(), trace_values() ) -> void().
void_fmt( _Format, _Values ) ->
	ok.


% "Outputs" specified void message, with specified message categorization.
-spec void_categorized( trace_message(), trace_message_categorization() ) ->
							void().
void_categorized( _Message, _MessageCategorization ) ->
	ok.



% "Outputs" specified void message, with specified message categorization and
% time information.
%
-spec void_categorized_timed( trace_message(), trace_message_categorization(),
							  trace_timestamp() ) -> void().
void_categorized_timed( _Message, _MessageCategorization, _Timestamp ) ->
	ok.


-define( echo_prefix, "[echoed] " ++ ).

% Echoes specified trace in specified trace channel.
%
% Defined notably to perform integrated operations (a trace being sent through
% both a basic system and a more advanced one), in order that the trace macros
% of upper layers (ex: send_alert_fmt/3, in the Traces layer) do not need to
% bind variables in their body (which may trigger bad matches as soon as more
% than once trace is sent in the same scope).
%
-spec echo( trace_message(), trace_severity() ) -> void().
echo( TraceMessage, _TraceSeverity=debug ) ->
	debug( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=info ) ->
	info( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=notice ) ->
	notice( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=warning ) ->
	warning( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=error ) ->
	error( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=critical ) ->
	critical( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=alert ) ->
	alert( ?echo_prefix TraceMessage );

echo( TraceMessage, _TraceSeverity=emergency ) ->
	emergency( ?echo_prefix TraceMessage );

echo( _TraceMessage, _TraceSeverity=void ) ->
	ok.



% Echoes specified trace in the specified trace severity channel, for specified
% message categorization.
%
% Defined notably to perform integrated operations (a trace being sent through
% both a basic system and a more advanced one), in order that the trace macros
% of upper layers (ex: send_alert_fmt/3, in the Traces layer) do not need to
% bind variables in their body (which may trigger bad matches as soon as more
% than once trace is sent in the same scope).
%
-spec echo( trace_message(), trace_severity(),
			trace_message_categorization() ) -> void().
echo( TraceMessage, _TraceSeverity=debug, MessageCategorization ) ->
	debug_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=info, MessageCategorization ) ->
	info_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=notice, MessageCategorization ) ->
	notice_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=warning, MessageCategorization ) ->
	warning_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=error, MessageCategorization ) ->
	error_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=critical, MessageCategorization ) ->
	critical_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=alert, MessageCategorization ) ->
	alert_categorized( TraceMessage, MessageCategorization );

echo( TraceMessage, _TraceSeverity=emergency, MessageCategorization ) ->
	emergency_categorized( TraceMessage, MessageCategorization );

echo( _TraceMessage, _TraceSeverity=void, _MessageCategorization ) ->
	ok.



% Echoes specified trace in specified trace channel, for specified message
% categorization and timestamp.
%
% Defined notably to perform integrated operations (a trace being sent through
% both a basic system and a more advanced one), in order that the trace macros
% of upper layers (ex: send_alert_fmt/3, in the Traces layer) do not need to
% bind variables in their body (which may trigger bad matches as soon as more
% than once trace is sent in the same scope).
%
-spec echo( trace_message(), trace_severity(), trace_message_categorization(),
			trace_timestamp() ) -> void().
echo( TraceMessage, _TraceSeverity=debug, MessageCategorization, Timestamp ) ->
	debug_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=info, MessageCategorization, Timestamp ) ->
	info_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=notice, MessageCategorization, Timestamp ) ->
	notice_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=warning, MessageCategorization,
	  Timestamp ) ->
	warning_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=error, MessageCategorization, Timestamp ) ->
	error_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=critical, MessageCategorization,
	  Timestamp ) ->
	critical_categorized_timed( TraceMessage, MessageCategorization,
								Timestamp );

echo( TraceMessage, _TraceSeverity=alert, MessageCategorization, Timestamp ) ->
	alert_categorized_timed( TraceMessage, MessageCategorization, Timestamp );

echo( TraceMessage, _TraceSeverity=emergency, MessageCategorization,
	  Timestamp ) ->
	emergency_categorized_timed( TraceMessage, MessageCategorization,
								 Timestamp );

echo( _TraceMessage, _TraceSeverity=void, _MessageCategorization,
	  _Timestamp ) ->
	ok.



% Returns the (numerical) priority associated to specified trace severity
% (i.e. emergency, alert, etc.).
%
% See also: its reciprocal get_severity_for/1.
%
-spec get_priority_for( trace_severity() ) -> trace_priority().
% From most common to least:
get_priority_for( debug ) ->
	7;

get_priority_for( info ) ->
	6;

get_priority_for( notice ) ->
	5;

get_priority_for( warning ) ->
	4;

get_priority_for( error ) ->
	3;

get_priority_for( critical ) ->
	2;

get_priority_for( alert ) ->
	1;

get_priority_for( emergency ) ->
	0;

get_priority_for( Other ) ->
	throw( { unexpected_trace_priority, Other } ).

% 'void' not expected here.



% Returns the trace severity (i.e. emergency, error, etc.) associated to
% specified (numerical) severity (which corresponds also to a log level, in
% terms of the newer standard logger).
%
% See also: its reciprocal get_priority_for/1.
%
-spec get_severity_for( trace_priority() ) -> trace_severity().
% From most common to least:
get_severity_for( 7 ) ->
	debug;

get_severity_for( 6 ) ->
	info;

get_severity_for( 5 ) ->
	notice;

get_severity_for( 4 ) ->
	warning;

get_severity_for( 3 ) ->
	error;

get_severity_for( 2 ) ->
	critical;

get_severity_for( 1 ) ->
	alert;

get_severity_for( 0 ) ->
	emergency;

get_severity_for( Other ) ->
	throw( { unexpected_trace_severity, Other } ).

% 'void' never returned here.



% Tells whether specified severity belongs to the error-like ones (typically the
% ones that must never be missed).
%
-spec is_error_like( trace_severity() ) -> boolean().
is_error_like( Severity ) ->
	lists:member( Severity, [ warning, error, critical, alert, emergency ] ).



% Handler section for the integration of Erlang (newer) logger.
%
% Refer to https://erlang.org/doc/man/logger.html.


% Replaces the current (probably default) logger handler with this Myriad one
% (registered as 'default').
%
-spec set_handler() -> void().
set_handler() ->

	%debug( "Setting trace_utils logger handler." ),

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
			throw( { unable_to_set_myriad_log_handler, AddErrReason,
					 TargetHandler } )

	end.



% Registers this Myriad logger handler as an additional one (not replacing the
% default one).
%
-spec add_handler() -> void().
add_handler() ->

	case logger:add_handler( _HandlerId=?myriad_logger_id, _Module=?MODULE,
							 get_handler_config() ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { unable_to_add_myriad_log_handler, Reason } )

	end.



% Returns the (initial) configuration of the Myriad logger handler.
-spec get_handler_config() -> logger:handler_config().
get_handler_config() ->

	#{ % No configuration needed here by our handler:
	   config => undefined
	   % Defaults:
	   % level => all,
	   % filter_default => log | stop,
	   % filters => [],
	   % formatter => {logger_formatter, DefaultFormatterConfig}

	   % Set by logger:
	   %id => HandlerId
	   %module => Module
	 }.



% Mandatory callback for log handlers.
%
% See https://erlang.org/doc/man/logger.html#HModule:log-2
%
-spec log( logger:log_event(), logger:handler_config() ) -> void().
log( _LogEvent=#{ level := Level,
				  %meta => #{error_logger => #{emulator => [...]
				  msg := Msg }, _Config ) ->

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


	%io:format( "### Logging following event:~n ~p~n(with config: ~p)~n "
	%  "resulting in: '~ts' (severity: ~p).",
	%  [ LogEvent, Config, TraceMsg, Severity ] ),

	% No the standard level corresponds directly to our severity:
	echo( TraceMsg, _Severity=Level, "erlang_logger" );

log( LogEvent, _Config ) ->
	throw( { unexpected_log_event, LogEvent } ).







% Helper section.



% Displays specified message.
%
% Note: adds a carriage-return/line-feed at the end of the message.
%
% (helper, to provide a level of indirection)
%
-spec severe_display( trace_message() ) -> void().
severe_display( Message ) ->

	Bar = "----------------",

	% Could be also error_logger:info_msg/1 for example:
	actual_display( "\n<" ++ Bar ++ "\n" ++ Message ++ "\n" ++ Bar ++ ">\n" ).



% Displays specified format-based message.
%
% Note: adds a carriage-return/line-feed at the end of the message.
%
% (helper, to provide a level of indirection)
%
-spec severe_display( trace_format(), trace_values() ) -> void().
severe_display( Format, Values ) ->
	Message = text_utils:format( Format, Values ),
	severe_display( Message ).



% Displays specified message.
%
% Note: adds a carriage-return/line-feed at the end of the message.
%
% (helper, to provide a level of indirection)
%
-spec actual_display( trace_message() ) -> void().
actual_display( Message ) ->

	% Default-timeout may not be sufficient (30 seconds, in milliseconds)
	%basic_utils:display_timed( Message, _TimeOut=30000 ).

	% If wanting a faster, less safe version:
	io:format( "~ts~n", [ Message ] ).



% Displays specified format-based message.
%
% Note: adds a carriage-return/line-feed at the end of the message.
%
% (helper, to provide a level of indirection)
%
-spec actual_display( trace_format(), trace_values() ) -> void().
actual_display( Format, Values ) ->

	%basic_utils:display( Format, Values ).

	% If wanting a faster, less safe version:
	io:format( Format ++ "~n", Values ).
