% Copyright (C) 2020-2021 Olivier Boudeville
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
% Creation date: Sunday, October 18, 2020.



% The trace bridge allows a software to depend only on the Ceylan-Myriad layer,
% yet to be able (optionally) to be using another piece of software (possibly
% the Ceylan-Traces layer, refer to http://traces.esperide.org/) at runtime for
% its logging, so that in all cases exactly one (and the most appropriate)
% logging system is used, even when lower-level libraries are involved, and with
% no change of source code to be operated on that software.
%
% It is useful to provide native, integrated, higher-level logging to basic
% libraries (ex: LEEC, see https://github.com/Olivier-Boudeville/Ceylan-LEEC),
% should their user require it - while being able to remain lean and mean if
% wanted (e.g while keeping the dependency to Ceylan-Traces optional).
%
% Switching to a more advanced trace system (typically Ceylan-Traces) is just a
% matter of having the process of interest call the register/3 function below.
%
% For usage examples, refer to:
%  - Ceylan-Myriad: trace_bridge_test.erl (directly tracing through basic
%  trace_utils)
%  - Ceylan-Traces: trace_bridging_test.erl (using then our advanced trace
%  system)
%
-module(trace_bridge).


% Suspiciously akin to Ceylan-Traces conventions:

%-type trace_emitter_name() :: bin_string().
%-type trace_categorization() :: bin_string().

% Possibly a class_TraceAggregator:aggregator_pid():
-type bridge_pid() :: pid().

% An actual trace message:
-type trace_message() :: ustring().

-export_type([ bridge_pid/0 ]).


-export([ get_bridge_spec/3, register/1, register_if_not_already/1,
		  get_bridge_info/0, set_bridge_info/1,
		  set_application_timestamp/1, unregister/0,

		  debug/1, debug_fmt/2,
		  info/1, info_fmt/2,
		  notice/1, notice_fmt/2,
		  warning/1, warning_fmt/2,
		  error/1, error_fmt/2,
		  critical/1, critical_fmt/2,
		  alert/1, alert_fmt/2,
		  emergency/1, emergency_fmt/2,
		  void/1, void_fmt/2 ]).


% Keys defined in the process dictionary:
-define( myriad_trace_bridge_key, "_myriad_trace_bridge" ).



% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type trace_severity() :: trace_utils:trace_severity().
-type trace_timestamp() :: trace_utils:trace_timestamp().



% Implementation notes:
%
% The process dictionary is used in order to avoid carrying along too many
% parameters.
%
% No special-casing the 'void' severity, as not used frequently enough.


% Typically the information transmitted by a trace emitter when creating a
% lower-level process that may or may not use fancy tracing:
%
-opaque bridge_spec() :: { TraceEmitterName :: bin_string(),
						   TraceCategory :: bin_string(),
						   BridgePid :: bridge_pid() }.

-export_type([ bridge_spec/0 ]).



% The bridging information stored in the target process dictionary:
-opaque bridge_info() :: { TraceEmitterName :: bin_string(),
						   TraceCategory :: bin_string(),
						   Location :: bin_string(),
						   BridgePid :: bridge_pid(),
						   ApplicationTimestamp :: maybe( trace_timestamp() ) }.

% To silence warning:
-export_type([ bridge_info/0 ]).



% Returns the information to pass to a process so that it can register to the
% corresponding trace bridge and use it automatically from then.
%
-spec get_bridge_spec( any_string(), any_string(), bridge_pid() ) ->
							bridge_spec().
get_bridge_spec( TraceEmitterName, TraceCategory, BridgePid ) ->
	{ text_utils:ensure_binary( TraceEmitterName ),
	  text_utils:ensure_binary( TraceCategory ), BridgePid }.



% Registers the current process to the specified trace bridge (if any).
%
% To be called by the process wanting to use such a trace bridge.
%
% Throws an exception of a bridge is already set.
%
% See also: register_if_not_already/1.
%
-spec register( maybe( bridge_spec() ) ) -> void().
register( _MaybeBridgeSpec=undefined ) ->
	ok;

register( BridgeSpec ) ->

	BridgeKey = ?myriad_trace_bridge_key,

	BridgeInfo = bridge_spec_to_info( BridgeSpec ),

	case process_dictionary:get( BridgeKey ) of

		% Normal case:
		undefined ->
			process_dictionary:put( BridgeKey, BridgeInfo ),
			info_fmt( "Trace bridge registered (spec: ~p).", [ BridgeSpec ] );

		UnexpectedInfo ->
			error_fmt( "Myriad trace bridge already registered (as ~p), "
				%"ignoring newer registration (as ~p).",
				"whereas a newer registration (as ~p) was requested.",
				[ BridgeInfo, UnexpectedInfo ] ),
			throw( { myriad_trace_bridge_already_registered, UnexpectedInfo,
					 BridgeInfo } )

	end.



% Registers the current process to the specified trace bridge (if any), and
% provided that no bridge was already registered (otherwise maintains the
% previous bridge and ignores silently that extraneous call).
%
% To be called by the process wanting to use such a trace bridge.
%
% Useful to have a bridge yet accept that the caller may have already set its
% bridge.
%
-spec register_if_not_already( maybe( bridge_spec() ) ) -> void().
register_if_not_already( _MaybeBridgeSpec=undefined ) ->
	ok;

register_if_not_already( BridgeSpec ) ->

	BridgeKey = ?myriad_trace_bridge_key,

	case process_dictionary:get( BridgeKey ) of

		undefined ->

			BridgeInfo = bridge_spec_to_info( BridgeSpec ),

			process_dictionary:put( BridgeKey, BridgeInfo ),
			info_fmt( "Trace bridge registered (spec: ~p).", [ BridgeSpec ] );

		_ ->
			ok

	end.



% (helper)
%
-spec bridge_spec_to_info( bridge_spec() ) -> bridge_info().
bridge_spec_to_info( _BridgeSpec={ BinTraceEmitterName, BinTraceCategory,
								   BridgePid } ) when is_pid( BridgePid ) ->

	Location = net_utils:localnode_as_binary(),
	DefaultApplicationTimestamp = undefined,

	% BridgeInfo:
	{ BinTraceEmitterName, BinTraceCategory, Location, BridgePid,
	  DefaultApplicationTimestamp };

bridge_spec_to_info( OtherBridgeSpec ) ->
	throw( { invalid_bridge_spec, OtherBridgeSpec } ).



% Returns the bridge information of the current process.
%
% May be useful for example if spawning processes and wanting that they use the
% same bridge.
%
-spec get_bridge_info() -> maybe( bridge_info() ).
get_bridge_info() ->
	process_dictionary:get( ?myriad_trace_bridge_key ).


% Sets the specified bridge information for the current process.
%
% May be useful for example for a spawned process to adopt the same bridge as
% the one of its caller (obtained thanks to get_bridge_info/0).
%
% Any local pre-existing bridge information will be overwritten.
%
-spec set_bridge_info( maybe( bridge_info() ) ) -> void().
set_bridge_info( MaybeBridgeInfo ) ->
	process_dictionary:put( ?myriad_trace_bridge_key, MaybeBridgeInfo ).



% Sets the current application timestamp.
%
% Note: if no trace bridge is registered, does nothing.
%
-spec set_application_timestamp( trace_timestamp() ) -> void().
set_application_timestamp( NewAppTimestamp ) ->

	BridgeKey = ?myriad_trace_bridge_key,

	case process_dictionary:get( BridgeKey ) of

		undefined ->
			ok;

		BridgeInfo ->
			NewBridgeInfo = setelement( _AppTmspIndex=5, BridgeInfo,
										NewAppTimestamp ),

			process_dictionary:put( BridgeKey, NewBridgeInfo )

	end.



% Unregisters the current process, which acted as a trace bridge; never fails.
-spec unregister() -> void().
unregister() ->
	% No-op if not set:
	process_dictionary:remove( _K=?myriad_trace_bridge_key ).



% Primitives for trace emission.


% Outputs specified debug message.
-spec debug( trace_message() ) -> void().
debug( Message ) ->
	send( debug, Message ).


% Outputs specified debug message to format.
-spec debug_fmt( format_string(), format_values() ) -> void().
debug_fmt( MessageFormat, MessageValues ) ->
	send( debug, MessageFormat, MessageValues ).



% Outputs specified info message.
-spec info( trace_message() ) -> void().
info( Message ) ->
	send( info, Message ).


% Outputs specified info message to format.
-spec info_fmt( format_string(), format_values() ) -> void().
info_fmt( MessageFormat, MessageValues ) ->
	send( info, MessageFormat, MessageValues ).



% Outputs specified notice message.
-spec notice( trace_message() ) -> void().
notice( Message ) ->
	send( notice, Message ).


% Outputs specified notice message to format.
-spec notice_fmt( format_string(), format_values() ) -> void().
notice_fmt( MessageFormat, MessageValues ) ->
	send( notice, MessageFormat, MessageValues ).



% Outputs specified warning message.
-spec warning( trace_message() ) -> void().
warning( Message ) ->
	send( warning, Message ).


% Outputs specified warning message to format.
-spec warning_fmt( format_string(), format_values() ) -> void().
warning_fmt( MessageFormat, MessageValues ) ->
	send( warning, MessageFormat, MessageValues ).



% Outputs specified error message.
-spec error( trace_message() ) -> void().
error( Message ) ->
	send( error, Message ).


% Outputs specified error message to format.
-spec error_fmt( format_string(), format_values() ) -> void().
error_fmt( MessageFormat, MessageValues ) ->
	send( error, MessageFormat, MessageValues ).



% Outputs specified critical message.
-spec critical( trace_message() ) -> void().
critical( Message ) ->
	send( critical, Message ).


% Outputs specified critical message to format.
-spec critical_fmt( format_string(), format_values() ) -> void().
critical_fmt( MessageFormat, MessageValues ) ->
	send( critical, MessageFormat, MessageValues ).



% Outputs specified critical message.
-spec alert( trace_message() ) -> void().
alert( Message ) ->
	send( alert, Message ).


% Outputs specified alert message to format.
-spec alert_fmt( format_string(), format_values() ) -> void().
alert_fmt( MessageFormat, MessageValues ) ->
	send( alert, MessageFormat, MessageValues ).



% Outputs specified emergency message.
-spec emergency( trace_message() ) -> void().
emergency( Message ) ->
	send( emergency, Message ).


% Outputs specified emergency message to format.
-spec emergency_fmt( format_string(), format_values() ) -> void().
emergency_fmt( MessageFormat, MessageValues ) ->
	send( emergency, MessageFormat, MessageValues ).



% "Outputs" specified void message.
-spec void( trace_message() ) -> void().
void( _Message ) ->
	ok.


% "Outputs" specified void message to format.
-spec void_fmt( format_string(), format_values() ) -> void().
void_fmt( _MessageFormat, _MessageValues ) ->
	ok.



% (helper)
-spec send( trace_severity(), trace_message() ) -> void().
send( SeverityType, Message ) ->

	case process_dictionary:get( ?myriad_trace_bridge_key ) of

		% No bridge set, using the direct, basic Myriad traces:
		undefined ->
			trace_utils:SeverityType( Message );

		% A bridge is available; mimicking the Ceylan-Traces protocol:
		BridgeInfo ->
			send_bridge( SeverityType, Message, BridgeInfo )

	end.



% (helper)
-spec send( trace_severity(), format_string(), format_values() ) -> void().
send( SeverityType, MessageFormat, MessageValues ) ->

	Message = text_utils:format( MessageFormat, MessageValues ),

	case process_dictionary:get( ?myriad_trace_bridge_key ) of

		% No bridge set, using the direct, basic Myriad traces:
		undefined ->
			trace_utils:SeverityType( Message );

		% A bridge is available:
		BridgeInfo ->
			send_bridge( SeverityType, Message, BridgeInfo )

	end.



% Mimicking the Ceylan-Traces protocol.
%
% (helper)
send_bridge( SeverityType, Message,
			 _BridgeInfo={ TraceEmitterName, TraceEmitterCategorization,
						   BinLocation, BridgePid, AppTimestamp } ) ->

	AppTimestampString = text_utils:term_to_binary( AppTimestamp ),

	TimestampText = text_utils:string_to_binary(
					time_utils:get_textual_timestamp() ),

	BridgePid ! { send,
		[ _TraceEmitterPid=self(),
		  TraceEmitterName,
		  TraceEmitterCategorization,
		  AppTimestampString,
		  _Time=TimestampText,
		  _Location=BinLocation,
		  _MessageCategorization='Trace Bridge',
		  %_MessageCategorization=uncategorized,
		  _Priority=trace_utils:get_priority_for( SeverityType ),
		  _Message=text_utils:string_to_binary( Message ) ] }.
