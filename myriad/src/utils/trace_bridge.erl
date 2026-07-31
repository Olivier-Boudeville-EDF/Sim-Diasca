% Copyright (C) 2020-2026 Olivier Boudeville
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

-module(trace_bridge).

-moduledoc """
The **trace bridge** allows modules to depend only on the Ceylan-Myriad layer,
yet to rely optionally on a non-Myriad code for traces/logs (possibly the
Ceylan-Traces layer, refer to [http://traces.esperide.org/]) at runtime for
**its logging**, so that in all cases exactly one (and the most appropriate)
logging system is used, even when lower-level libraries are involved (they have
to be designed to operate with or without an advanced trace system), and with no
change in the source code of these user modules to be operated.

It is useful to provide native, integrated, higher-level logging to basic
libraries (e.g. Ceylan-LEEC, see [http://leec.esperide.org] or Ceylan-Oceanic,
see [http://oceanic.esperide.org]), should their user require it - while being
able to remain lean and mean if wanted (e.g while keeping the dependency to
Ceylan-Traces optional).

Switching to a more advanced trace system (typically Ceylan-Traces) is then just
a matter of having the process of interest call the `register/3` function below.

For usage examples, refer to:
- Ceylan-Myriad: `trace_bridge_test.erl` (directly tracing through basic
  `trace_utils`)
- Ceylan-Traces: `trace_bridging_test.erl` (using then our advanced trace
  system); for example:
```
BridgeSpec = trace_bridge:get_bridge_spec(_MyEmitterName="MyBridgeTester",
  _MyCateg="MyTraceCategory",
  _BridgePid=class_TraceAggregator:get_aggregator()),

trace_bridge:register(BridgeSpec), [...]
```
- Ceylan-LEEC: most modules, including `leec`
""".



% The conventions retained here are suspiciously similar to the Ceylan-Traces
% ones.


-doc "Possibly a `class_TraceAggregator:aggregator_pid()`.".
-type bridge_pid() :: pid().



-doc "An actual trace message.".
-type trace_message() :: ustring().

-export_type([ bridge_pid/0 ]).


-export([ get_bridge_spec/2, get_bridge_spec/3,
          register/1, register_if_not_already/1,
          get_bridge_info/0, set_bridge_info/1,
          set_application_timestamp/1, unregister/0,
          wait_bridge_sync/0,

          inhibit_any/0, restore_any/1,

          debug/1, debug_fmt/2,
          info/1, info_fmt/2,
          notice/1, notice_fmt/2,
          warning/1, warning_fmt/2,
          error/1, error_fmt/2,
          critical/1, critical_fmt/2,
          alert/1, alert_fmt/2,
          emergency/1, emergency_fmt/2,
          void/1, void_fmt/2,

          send/2, send/3 ]).


% Keys defined in the process dictionary:
-define( myriad_trace_bridge_key, "_myriad_trace_bridge" ).


% Implementation notes:
%
% The process dictionary is used in order to avoid carrying along too many
% parameters: the myriad_trace_bridge_key define corresponds to the key to which
% a bridge_info() term may be associated so that the current process is bridged
% to a Trace system.
%
% Not special-casing the 'void' severity, as not used frequently enough.



% Type shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type trace_severity() :: trace_utils:trace_severity().
-type trace_timestamp() :: trace_utils:trace_timestamp().




-doc "Bridging information typically specified by the user.".
-type user_bridge_info() ::
    { TraceCategory :: any_string(), BridgePid :: bridge_pid() }.



-doc """
Typically the information transmitted by a trace emitter when creating a
lower-level process that may or may not use advanced logging.

Note that this type is opaque; use `get_bridge_spec/{2,3}` to obtain an instance
thereof.
""".
-opaque bridge_spec() :: { TraceEmitterName :: bin_string(),
                           TraceCategory :: bin_string(),
                           BridgePid :: bridge_pid() }.


-export_type([ user_bridge_info/0, bridge_spec/0 ]).



-doc "A bridging information stored in a target process dictionary.".
-opaque bridge_info() :: {
        TraceEmitterName :: bin_string(),
        TraceCategory :: bin_string(),
        Location :: bin_string(),
        BridgePid :: bridge_pid(),
        ApplicationTimestamp :: option( trace_timestamp() ) }

  % In some very rare cases, we want to inhibit a trace bridge (e.g. if needing
  % to determine whether a service is available by triggering an operation that
  % may fail with an error trace that should not be reported overall):
  %
  | 'inhibited_trace_bridge'.



% To silence warning:
-export_type([ bridge_info/0 ]).



-doc """
Returns the information to pass to a process so that it can register to the
corresponding trace bridge and use it automatically from then.
""".
-spec get_bridge_spec( any_string(), user_bridge_info() ) -> bridge_spec().
get_bridge_spec( TraceEmitterName,
                 _UserBridgeInfo={ TraceCategory, BridgePid } ) ->
    get_bridge_spec( TraceEmitterName, TraceCategory, BridgePid ).



-doc """
Returns the information to pass to a process so that it can register to the
corresponding trace bridge and use it automatically from then.

With Ceylan-Traces, BridgePid is typically obtained thanks to
`class_TraceAggregator:get_aggregator()`.

Allows not to break the opaqueness of the `bridge_spec()` type.
""".
-spec get_bridge_spec( any_string(), any_string(), bridge_pid() ) ->
                            bridge_spec().
get_bridge_spec( TraceEmitterName, TraceCategory, BridgePid ) ->
    { text_utils:ensure_binary( TraceEmitterName ),
      text_utils:ensure_binary( TraceCategory ), BridgePid }.



-doc """
Registers the current process to the specified trace bridge (if any).

To be called by the process wanting to use such a trace bridge.

Throws an exception of a bridge is already set.

See also: `register_if_not_already/1`.
""".
-spec register( option( bridge_spec() ) ) -> void().
register( _MaybeBridgeSpec=undefined ) ->
    ok;

register( BridgeSpec ) ->

    BridgeKey = ?myriad_trace_bridge_key,

    BridgeInfo = bridge_spec_to_info( BridgeSpec ),

    case process_dictionary:get( BridgeKey ) of

        % Normal case:
        undefined ->
            process_dictionary:put( BridgeKey, BridgeInfo ),
            cond_utils:if_defined( myriad_debug_traces,
                debug_fmt( "Trace bridge registered (spec: ~p).",
                           [ BridgeSpec ] ) );

        UnexpectedInfo ->
            error_fmt( "Myriad trace bridge already registered (as ~p), "
                %"ignoring newer registration (as ~p).",
                "whereas a newer registration (as ~p) was requested.",
                [ BridgeInfo, UnexpectedInfo ] ),

            throw( { myriad_trace_bridge_already_registered, UnexpectedInfo,
                     BridgeInfo } )

    end.



-doc """
Registers the current process to the specified trace bridge (if any), and
provided that no bridge was already registered (otherwise maintains the previous
bridge and ignores silently that extraneous call).

To be called by the process wanting to use such a trace bridge.

Useful to have a bridge yet accept that the caller may have already set its
bridge.
""".
-spec register_if_not_already( option( bridge_spec() ) ) -> void().
register_if_not_already( _MaybeBridgeSpec=undefined ) ->
    ok;

register_if_not_already( BridgeSpec ) ->

    BridgeKey = ?myriad_trace_bridge_key,

    process_dictionary:get( BridgeKey ) =:= undefined andalso
        begin
            BridgeInfo = bridge_spec_to_info( BridgeSpec ),

            process_dictionary:put( BridgeKey, BridgeInfo ),
            debug_fmt( "Trace bridge registered (spec: ~p).", [ BridgeSpec ] )
        end.



% (helper)
-spec bridge_spec_to_info( bridge_spec() ) -> bridge_info().
bridge_spec_to_info( _BridgeSpec={ BinTraceEmitterName, BinTraceCategory,
                                   BridgePid } ) when is_pid( BridgePid ) ->

    Location = net_utils:localnode_as_binary(),
    DefaultApplicationTimestamp = undefined,

    % BridgeInfo:
    { BinTraceEmitterName, BinTraceCategory, Location, BridgePid,
      DefaultApplicationTimestamp };

bridge_spec_to_info( _BridgeSpec={ _BinTraceEmitterName, _BinTraceCategory,
                                   NotAPid } ) ->
    throw( { invalid_bridge_spec, no_bridge_pid, NotAPid } );

bridge_spec_to_info( OtherBridgeSpec ) ->
    throw( { invalid_bridge_spec, OtherBridgeSpec } ).



-doc """
Returns the bridge information of the current process.

May be useful for example if spawning processes and wanting that they use the
same bridge.
""".
-spec get_bridge_info() -> option( bridge_info() ).
get_bridge_info() ->
    process_dictionary:get( ?myriad_trace_bridge_key ).



-doc """
Sets the specified bridge information for the current process.

May be useful for example for a spawned process to adopt the same bridge as the
one of its caller (obtained thanks to `get_bridge_info/0`).

Any local pre-existing bridge information will be overwritten.
""".
-spec set_bridge_info( option( bridge_info() ) ) -> void().
set_bridge_info( MaybeBridgeInfo ) ->
    process_dictionary:put( ?myriad_trace_bridge_key, MaybeBridgeInfo ).



-doc """
Sets the current application timestamp.

Note: if no trace bridge is registered, or if it is inhibited, does nothing.
""".
-spec set_application_timestamp( trace_timestamp() ) -> void().
set_application_timestamp( NewAppTimestamp ) ->

    BridgeKey = ?myriad_trace_bridge_key,

    case process_dictionary:get( BridgeKey ) of

        undefined ->
            ok;

        inhibited_trace_bridge ->
            ok;

        BridgeInfo ->
            NewBridgeInfo = setelement( _AppTmspIndex=5, BridgeInfo,
                                        NewAppTimestamp ),

            process_dictionary:put( BridgeKey, NewBridgeInfo )

    end.



-doc """
Unregisters the current process, which acted as a trace bridge; never fails.
""".
-spec unregister() -> void().
unregister() ->
    % No-op if not set:
    process_dictionary:remove( _K=?myriad_trace_bridge_key ).



-doc """
Inhibits any trace bridge, muting all traces sent based on this module from the
current process.

Useful if some called code could send undesirable traces (e.g. if failing during
a test of the availability of a feature).

This bridge can be restored with `restore/1`.
""".
-spec inhibit_any() -> option( bridge_info() ).
inhibit_any() ->

    BridgeKey = ?myriad_trace_bridge_key,

    MaybeBridgeInfo = process_dictionary:get( BridgeKey ),

    process_dictionary:put( BridgeKey, inhibited_trace_bridge ),

    MaybeBridgeInfo.



-doc "Restores any inhibited bridge, as it was obtained from `inhibit_any/0`.".
-spec restore_any( option( bridge_info() ) ) -> void().
restore_any( MaybeBridgeInfo ) ->

    BridgeKey = ?myriad_trace_bridge_key,

    case process_dictionary:get( BridgeKey ) of

        inhibited_trace_bridge ->
            process_dictionary:put( BridgeKey, MaybeBridgeInfo );

        UnexpectedBridgeInfo ->
            throw( { myriad_trace_bridge_not_inhibited, UnexpectedBridgeInfo,
                     MaybeBridgeInfo } )

    end.





% Primitives for trace emission.


-doc "Outputs the specified debug message.".
-spec debug( trace_message() ) -> void().
debug( Message ) ->
    send( debug, Message ).


-doc "Outputs the specified debug message to format.".
-spec debug_fmt( format_string(), format_values() ) -> void().
debug_fmt( MessageFormat, MessageValues ) ->
    send( debug, MessageFormat, MessageValues ).



-doc "Outputs the specified info message.".
-spec info( trace_message() ) -> void().
info( Message ) ->
    send( info, Message ).



-doc "Outputs the specified info message to format.".
-spec info_fmt( format_string(), format_values() ) -> void().
info_fmt( MessageFormat, MessageValues ) ->
    send( info, MessageFormat, MessageValues ).



-doc "Outputs the specified notice message.".
-spec notice( trace_message() ) -> void().
notice( Message ) ->
    send( notice, Message ).



-doc "Outputs the specified notice message to format.".
-spec notice_fmt( format_string(), format_values() ) -> void().
notice_fmt( MessageFormat, MessageValues ) ->
    send( notice, MessageFormat, MessageValues ).



-doc "Outputs the specified warning message.".
-spec warning( trace_message() ) -> void().
warning( Message ) ->
    send( warning, Message ).



-doc "Outputs the specified warning message to format.".
-spec warning_fmt( format_string(), format_values() ) -> void().
warning_fmt( MessageFormat, MessageValues ) ->
    send( warning, MessageFormat, MessageValues ).



-doc "Outputs the specified error message.".
-spec error( trace_message() ) -> void().
error( Message ) ->
    send( error, Message ).



-doc "Outputs the specified error message to format.".
-spec error_fmt( format_string(), format_values() ) -> void().
error_fmt( MessageFormat, MessageValues ) ->
    send( error, MessageFormat, MessageValues ).



-doc "Outputs the specified critical message.".
-spec critical( trace_message() ) -> void().
critical( Message ) ->
    send( critical, Message ).



-doc "Outputs the specified critical message to format.".
-spec critical_fmt( format_string(), format_values() ) -> void().
critical_fmt( MessageFormat, MessageValues ) ->
    send( critical, MessageFormat, MessageValues ).



-doc "Outputs the specified critical message.".
-spec alert( trace_message() ) -> void().
alert( Message ) ->
    send( alert, Message ).



-doc "Outputs the specified alert message to format.".
-spec alert_fmt( format_string(), format_values() ) -> void().
alert_fmt( MessageFormat, MessageValues ) ->
    send( alert, MessageFormat, MessageValues ).



-doc "Outputs the specified emergency message.".
-spec emergency( trace_message() ) -> void().
emergency( Message ) ->
    send( emergency, Message ).



-doc "Outputs the specified emergency message to format.".
-spec emergency_fmt( format_string(), format_values() ) -> void().
emergency_fmt( MessageFormat, MessageValues ) ->
    send( emergency, MessageFormat, MessageValues ).



-doc """
"Outputs" the specified void message.
""".
-spec void( trace_message() ) -> void().
void( _Message ) ->
    ok.



-doc """
"Outputs" the specified void message to format.
""".
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

    %io:format( "Sending ~ts message of format '~ts' and values ~p.",
    %           [ SeverityType, MessageFormat, MessageValues ] ),

    Message = text_utils:format( MessageFormat, MessageValues ),

    case process_dictionary:get( ?myriad_trace_bridge_key ) of

        % No bridge set, using the direct, basic Myriad traces:
        undefined ->
            trace_utils:SeverityType( Message );

        % A bridge is available:
        BridgeInfo ->

            % No need to add here an additional trace_utils-based sending, it
            % will be done for error-like messages by the next call:
            %
            send_bridge( SeverityType, Message, BridgeInfo )

    end.



% Mimicking the Ceylan-Traces protocol.
send_bridge( SeverityType, Message,
             _BridgeInfo={ TraceEmitterName, TraceEmitterCategorization,
                           BinLocation, BridgePid, AppTimestamp } ) ->

    cond_utils:if_defined( myriad_debug_traces,
        trace_utils:debug_fmt( "Sending '~ts', with ~ts severity, to ~w.",
                               [ Message, SeverityType, BridgePid ] ) ),

    AppTimestampString = text_utils:term_to_binary( AppTimestamp ),

    BinTimestampText = time_utils:get_bin_textual_timestamp(),

    MessageCategorization = 'Trace Bridge',

    Msg = [ _TraceEmitterPid=self(),
            TraceEmitterName,
            TraceEmitterCategorization,
            AppTimestampString,
            BinTimestampText,
            _Location=BinLocation,
            MessageCategorization,
            _Priority=trace_utils:get_priority_for( SeverityType ),
            _Message=text_utils:string_to_binary( Message ) ],

    % Error-like messages are echoed on the console and made synchronous, to
    % ensure that they are not missed:
    %
    case trace_utils:is_error_like( SeverityType ) of

        true ->
            % A bit of interleaving:
            BridgePid ! { sendSync, Msg, self() },

            trace_utils:echo( Message, SeverityType, MessageCategorization,
                              BinTimestampText ),

            wait_bridge_sync();

        false ->
            % Unechoed fire and forget here:
            BridgePid ! { send, Msg }

    end;

send_bridge( SeverityType, Message, _BridgeInfo=inhibited_trace_bridge ) ->
    cond_utils:if_defined( myriad_debug_traces,
        trace_utils:debug_fmt(
            "Not sending trace '~ts' with ~ts severity: inhibited bridge.",
            [ Message, SeverityType ] ),
        basic_utils:ignore_unused( [ SeverityType, Message ] ) ).




-doc """
Waits for the bridge to report that a trace synchronisation has been completed.
""".
-spec wait_bridge_sync() -> void().
wait_bridge_sync() ->

    receive

        % A little breach of opaqueness; any actual bridge aggregator shall
        % acknowledge a trace sending with such a message:
        %
        % (we could have recorded in the bridge the module name of the
        % aggregator at hand - e.g. class_TraceAggregator - and called an
        % exported function thereof to determine the acknowledgement message to
        % expect, like in 'class_TraceAggregator:get_acknowledge_message() ->
        % {wooper_result, trace_aggregator_synchronised}' yet, at least
        % currently, we prefer being bound only by message structures, not by
        % module calls)
        %
        { wooper_result, trace_aggregator_synchronised } ->
            ok

    end.
