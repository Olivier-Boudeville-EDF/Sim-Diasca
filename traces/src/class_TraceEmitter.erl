% Copyright (C) 2007-2023 Olivier Boudeville
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


% @doc The <b>trace emitter</b> class, base class for all (WOOPER-based)
% emitters of traces.
%
-module(class_TraceEmitter).


-define( class_description,
		 "Base class for all (WOOPER-based) emitters of traces. "
		 "See class_TestTraceEmitter.erl and class_TraceEmitter_test.erl." ).


% Determines what are the direct mother classes of this class (if any):
%
% (preferred to be defined as empty rather than not be defined at all, as
% referenced by a static method)
%
-define( superclasses, [] ).


% Describes the class-specific attributes:
-define( class_attributes, [

	{ name, bin_string(), "name of this trace emitter (not named "
	  "trace_name, in order to be more versatile)" },

	% As this attribute should never be accessed directly (use emitter_init() at
	% construction-time, and get_categorization/1 and set_categorization/2
	% afterwards), in order to reduce the risk of clash 'trace_categorization'
	% has been renamed in:
	%
	{ trace_emitter_categorization, bin_string(),
	  "categorization of this trace emitter" },

	{ emitter_node, net_utils:bin_node_name(),
	  "the name of the Erlang node of this emitter" },

	{ trace_aggregator_pid, aggregator_pid(),
	  "the PID of the trace aggregator collecting the traces emitted" },

	{ trace_timestamp, maybe( app_timestamp() ),
	  "current application-specific timestamp" } ] ).



% Helper functions:
-export([ init/1, register_bridge/1,
		  get_categorization/1, set_categorization/2,
		  send/3, send_safe/3, send/4, send_safe/4, send/5, send_safe/5,
		  send_synchronised/3, send_synchronised/4, send_synchronised/5,
		  send_categorized_emitter/4, send_named_emitter/4,
		  get_trace_timestamp/1, get_trace_timestamp_as_binary/1,
		  get_plain_name/1, get_short_description/1,
		  sync/1, await_output_completion/0 ]).


% The class-specific trace_emitter_categorization define will be set in the
% trace_emitter_categorization attribute of each child class when coming down
% the inheritance hierarchy, so that the latest child class sets its targeted
% trace_emitter_categorization value).


-type emitter_name() :: text_utils:any_string().
% The name of a trace emitter.
%
% It is a plain string or a binary one, containing the name of a trace emitter.
%
% Note: dots are not allowed in an emitter name (they are used as naming
% separator).
%
% For example  "MyObject 16", or `<<"First Talker">>'.


-type emitter_init() :: emitter_name()
					  | { emitter_name(), emitter_categorization() }.
% Initializing a trace emitter is specifying its name to the constructor of its
% actual class, which will augment that information with the correspond
% class-specific emitter categorization. Then, the pair resulting from this
% one-shot, initial operation will climb up the class hierarchy until reaching
% the class_TraceEmitter constructor.
%
% See also the trace_categorize/1 macro.


-type emitter_pid() :: wooper:instance_pid().
% PID of a trace emitter.

-type preformatted_trace() :: bin_string().
% A preformatted trace that can be sent to the trace aggregator and directly
% stored/written by it.


-export_type([ emitter_name/0,
			   emitter_categorization/0, bin_emitter_categorization/0,
			   emitter_init/0, emitter_pid/0, preformatted_trace/0 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% For send_from_* and all:
-include("class_TraceAggregator.hrl").


% For trace_aggregator_name:
-include("class_TraceEmitter.hrl").


-define( LogPrefix, "[Trace Emitter]" ).



% Implementation notes:

% A trace emitter used to have a specific notion of time (execution tick) as it
% needs to timestamp its traces. Now it relies on the content of an opaque
% 'trace_timestamp' attribute, which is stringified and used as it is, allowing
% for mostly any kind of application-level timestamp.


% To reduce the memory footprint in the trace aggregator mailbox and the size of
% messages sent over the network, most of the time binaries are used instead of
% plain strings.
%
% Notably the 'name' attribute is stored as a binary.
%
% Use text_utils:binary_to_string/1 to get back a plain string or, preferably,
% the class_TraceEmitter:get_plain_name/1 static method.
%
% The same applies for the 'trace_emitter_categorization' attribute.

% The constructor of this class is idempotent, in the sense that it can be
% applied more than once with no undesirable consequence.
%
% By default, a trace emitter locates its aggregator through a global naming
% look-up, but an arbitrary PID may be specified.


% The send_safe/{3,4,5} variations differ from their basic send/{3,4,5}
% counterparts on two aspects:
% - they are synchronous (blocking, hence safer)
% - they are echoed on the console as well


% Regarding the look-up scope of the trace aggregator:
%
% Its most common registration scope is global, yet in some (less common) cases,
% we prefer having one aggregator per node (e.g. multiple applications able to
% interact, yet each with its own autonomy and node); for scalability reasons,
% we do not want all trace emitters to have to specify an extra parameter at
% construction or to read each the configuration information in order to select
% a relevant look-up scope; so all go first for the general case ('global'
% registration of the aggregator) and fall back to 'local' in the other cases.
%
% To avoid any unnecessary global look-up or to designate a specific trace
% aggregator, use construct/3.
%
-define( emitter_look_up_scope, global_otherwise_local ).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type emitter_info() :: traces:emitter_info().
-type trace_severity() :: traces:trace_severity().
-type message() :: traces:message().
-type message_categorization() :: traces:message_categorization().

-type emitter_categorization() :: traces:emitter_categorization().
-type bin_emitter_categorization() :: traces:bin_emitter_categorization().
-type emitter_any_categorization() :: traces:emitter_any_categorization().

-type app_timestamp() :: traces:app_timestamp().
-type priority() :: traces:priority().

-type aggregator_pid() :: class_TraceAggregator:aggregator_pid().


% @doc Constructs a trace emitter, from EmitterInit, which must be here a pair
% made of this name and another plain string, its emitter categorization,
% listing increasingly detailed sub-categories about this trace emitter,
% separated by dots (e.g. "topics.sports.basketball.coach").
%
% Note: this constructor should be idempotent, as a given instance might very
% well inherit (directly or not) from that class more than once.
%
-spec construct( wooper:state(),
			{ emitter_name(), emitter_categorization() } ) -> wooper:state().
construct( State, _EmitterInit={ EmitterName, EmitterCategorization } ) ->
  % Useless, as checks done just afterwards:
  % when is_list( EmitterName ) andalso is_list( EmitterCategorization ) ->

	%trace_utils:debug_fmt( "~ts Creating a trace emitter whose name is '~ts', "
	%   "whose PID is ~w and whose categorization is '~ts'.",
	%   [ ?LogPrefix, EmitterName, self(), EmitterCategorization ] ),

	% Often classes inherit more than once from trace emitter; in this case, to
	% avoid multiple initialisations, we consider that the first construction is
	% the one that shall be retained, all next ones (if any) being then skipped:
	%
	case hasAttribute( State, trace_aggregator_pid ) of

		% Already initialised:
		true ->
			State;

		false ->
			InitState = init( State ),

			BinName = check_and_binarise_name( EmitterName ),

			BinCategorization =
				text_utils:ensure_binary( EmitterCategorization ),

			setAttributes( InitState, [
				{ name, BinName },
				{ trace_emitter_categorization, BinCategorization },
				% (emitter_node and trace_aggregator_pid set by init/1)
				{ trace_timestamp, undefined } ] )

	end;

% Should no mother class have set it:
construct( State, EmitterName ) ->
	% Useless, as checked afterwards: when is_list( EmitterName ) ->
	construct( State, _EmitterInit={ EmitterName,
									 ?default_trace_emitter_categorization } ).

% Useless, as already checked:
%construct( _State, InvalidEmitterName ) ->
%   throw( { invalid_emitter_name, InvalidEmitterName } ).



% @doc Constructs a trace emitter, from EmitterInit, which must be here a pair
% made of this name and another plain string, its emitter categorization,
% listing increasingly detailed sub-categories about this trace emitter,
% separated by dots (e.g. "topics.sports.basketball.coach"), and from the
% specified trace aggregator.
%
% Notes:
%  - this constructor should be idempotent, as a given instance might very
% well inherit (directly or not) from that class more than once
%
%  - being able to provide the PID of the target trace aggregator allows to use
%  any aggregator of interest, regardless of choices in terms of naming
%  registration
%
-spec construct( wooper:state(),
			{ emitter_name(), emitter_categorization() }, aggregator_pid() ) ->
		wooper:state().
construct( State, _EmitterInit={ EmitterName, EmitterCategorization },
		   TraceAggregatorPid ) ->

	%trace_utils:debug_fmt( "~ts Creating a trace emitter whose name is '~ts', "
	%   "whose PID is ~w, whose categorization is '~ts' and using trace "
	%   "aggregator ~w.",
	%   [ ?LogPrefix, EmitterName, self(), EmitterCategorization,
	%     TraceAggregatorPid ] ),

	BinName = check_and_binarise_name( EmitterName ),

	BinCategorization = text_utils:ensure_binary( EmitterCategorization ),

	setAttributes( State, [
		{ name, BinName },
		{ trace_emitter_categorization, BinCategorization },
		{ trace_timestamp, undefined },
		{ emitter_node, net_utils:localnode_as_binary() },
		{ trace_aggregator_pid, TraceAggregatorPid } ] );


% Should no mother class have set it:
construct( State, EmitterName, TraceAggregatorPid ) ->

	EmitterInit = { EmitterName, ?default_trace_emitter_categorization },

	construct( State, EmitterInit, TraceAggregatorPid ).




% @doc Checks the emitter name, and, if needed, returns a binary version
% thereof.
%
% Note: we used to fail should at least one dot be found, now we convert the
% string name so that it becomes legit.
%
% (helper)
%
-spec check_and_binarise_name( emitter_name() ) -> bin_string().
check_and_binarise_name( StringName ) when is_list( StringName ) ->
	LegitStringName = check_string_name( StringName ),
	text_utils:string_to_binary( LegitStringName );

check_and_binarise_name( BinName ) when is_binary( BinName ) ->
	StringName = text_utils:binary_to_string( BinName ),
	check_and_binarise_name( StringName ).



% Helper:
check_string_name( Name ) ->

	% Can be an io_list():
	FlatName = text_utils:format( "~ts", [ Name ] ),

	% Dots are not allowed in emitter names (as they are interpreted as
	% subcategories), whereas for example FQDNs have such characters:

	% text_utils:split_at_first( _Marker=$., Name ) =:= none_found orelse
	%     throw( { no_dot_allowed_in_emitter_name, FlatName }

	text_utils:substitute( _Source=$., _Target=$:, FlatName ).



% Methods section.



% Generic interface.


% @doc Returns the name of this trace emitter, as a binary.
%
% Note: use text_utils:binary_to_string/1 to get back a plain string.
%
-spec getName( wooper:state() ) -> const_request_return( bin_string() ).
getName( State ) ->
	wooper:const_return_result( ?getAttr(name) ).



% @doc Sets the name of this trace emitter from the specified string.
-spec setName( wooper:state(), emitter_name() ) -> oneway_return().
setName( State, NewName ) ->

	BinName = text_utils:ensure_binary( NewName ),

	%trace_utils:debug_fmt( "Renaming trace emitter from '~ts' to '~ts'.",
	%                       [ ?getAttr(name), BinName ] ),

	wooper:return_state( setAttribute( State, name, BinName ) ).



% @doc Sets the trace categorization for this trace emitter to specified plain
% string.
%
% Setting the trace categorization early in the constructor, before sending any
% trace, allows to have all traces for a given emitter correctly gathered in the
% same trace category, which is a lot clearer when browsing afterwards.
%
-spec setCategorization( wooper:state(), emitter_categorization() ) ->
							oneway_return().
setCategorization( State, TraceCategorization ) ->

	NewState = set_categorization( TraceCategorization, State ),

	wooper:return_state( NewState ).



% @doc Displays the state of this instance in the console.
-spec display( wooper:state() ) -> const_oneway_return().
display( State ) ->
	wooper:display_instance( State ),
	wooper:const_return().


% @doc Returns a textual description of this trace emitter.
-spec toString( wooper:state() ) -> const_request_return( ustring() ).
toString( State ) ->
	wooper:const_return_result( wooper:state_to_string( State ) ).




% @doc Callback triggered whenever a linked process stops, if this instance is
% to trap exits (not true by default).
%
-spec onWOOPERExitReceived( wooper:state(), pid(),
						basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, PidOrPort, _ExitReason=normal ) ->

	?debug_fmt( "The TraceEmitter default EXIT handler of this instance "
		"ignored a normal EXIT message from ~w.", [ PidOrPort ] ),

	wooper:const_return();

onWOOPERExitReceived( State, PidOrPort, ExitReason ) ->

	?warning_fmt( "The TraceEmitter default EXIT handler of this instance "
		"ignored the following EXIT message from ~w:~n'~p'.",
		[ PidOrPort, ExitReason ] ),

	wooper:const_return().



% @doc Callback triggered whenever a linked process stops, if this instance is
% to trap exits (not true by default).
%
-spec onWOOPERDownNotified( wooper:state(), monitor_utils:monitor_reference(),
		monitor_utils:monitored_element_type(),
		monitor_utils:monitored_element(), monitor_utils:monitor_info() ) ->
								const_oneway_return().
onWOOPERDownNotified( State, MonitorRef, MonitoredType, MonitoredElement,
					  MonitorInfo ) ->

	?warning_fmt( "The TraceEmitter default EXIT handler of this instance "
		"ignored the following DOWN notification '~p' "
		"for monitored element ~p of type '~p' (monitor reference: ~w).",
		[ MonitorInfo, MonitoredElement, MonitoredType, MonitorRef ] ),

	wooper:const_return().



% @doc Callback triggered if this instance requested to be notified whenever a
% new node is connected.
%
-spec onWOOPERNodeConnection( wooper:state(), net_utils:atom_node_name(),
			  monitor_utils:monitor_node_info() ) -> const_oneway_return().
onWOOPERNodeConnection( State, Node, MonitorNodeInfo ) ->

	?warning_fmt( "The TraceEmitter default node up handler of this instance "
		"ignored the connection notification for node '~ts' (information: ~p).",
		[ Node, MonitorNodeInfo ] ),

	wooper:const_return().



% @doc Callback triggered if this instance requested to be notified whenever a
% node disconnects.
%
-spec onWOOPERNodeDisconnection( wooper:state(), net_utils:atom_node_name(),
			  monitor_utils:monitor_node_info() ) -> const_oneway_return().
onWOOPERNodeDisconnection( State, Node, MonitorNodeInfo ) ->

	?warning_fmt( "The TraceEmitter default node down handler of this instance "
		"ignored the disconnection notification for node '~ts' "
		"(information: ~p).", [ Node, MonitorNodeInfo ] ),

	wooper:const_return().




% Static section.



% @doc Registers in the caller process a trace bridge suitable to integrate to
% this Traces subsystem.
%
% Dedicated to normal (non-TraceEmitter, probably not even non-WOOPER) processes
% that nevertheless need to send traces, to centralise them.
%
% See also the register_bridge/1 helper for trace emitter instances that need to
% define additionally their trace bridge, in order that the lower-level
% libraries/functions that they call can send such traces as well.
%
% Note: no bridge is expected to be already set, otherwise an exception is
% thrown.
%
-spec register_as_bridge( emitter_name(), emitter_categorization() ) ->
								static_void_return().
register_as_bridge( TraceEmitterName, TraceCategory ) ->

	TraceAggregatorPid =
		class_TraceAggregator:get_aggregator( _LaunchAggregator=false ),

	register_as_bridge( TraceEmitterName, TraceCategory, TraceAggregatorPid ),

	wooper:return_static_void().



% @doc Registers in the caller process a trace bridge suitable to integrate to
% this Traces subsystem.
%
% Dedicated to normal (non-TraceEmitter, probably not even non-WOOPER) processes
% that nevertheless need to send traces, to centralise them.
%
% See also the register_as_bridge/1 helper for trace emitter instances that need
% to define additionally their trace bridge, in order that the lower-level
% libraries/functions that they call can send such traces as well.
%
% Note: no bridge is expected to be already set, otherwise an exception is
% thrown.
%
-spec register_as_bridge( emitter_name(), emitter_categorization(),
						  aggregator_pid() ) -> static_void_return().

register_as_bridge( TraceEmitterName, TraceCategory, TraceAggregatorPid ) ->

	trace_bridge:register( _BridgeSpec={
		check_and_binarise_name( TraceEmitterName ),
		text_utils:ensure_binary( TraceCategory ), TraceAggregatorPid } ),

	wooper:return_static_void().



% @doc Subcategorizes a given emitter name, by introducing an additional
% grouping level, based on the first character of the trace emitter name.
%
% This is typically useful to avoid that too many emitters are listed at a given
% categorization level, if using a trace supervisor that groups the emitter
% names based on the dots that they include.
%
% For example if the specified emitter name is "foobar", returns "f.foobar",
% resulting in this name to be sorted among all entries whose name starts with
% "f".
%
-spec subcategorize( emitter_name() ) -> static_return( emitter_name() );
				   ( emitter_info() ) -> static_return( emitter_info() ).
% We do not alter the name, as any dot in it will be replaced with ':'; we
% update the categorization instead:
%
%subcategorize( _EmitterInfo={ Name, Categ } ) ->
%   wooper:return_static( { subcategorize_name( Name ), Categ } );

%subcategorize( EmitterName ) ->
%   wooper:return_static( subcategorize_name( EmmiterName ) ).

subcategorize( _EmitterInfo={ EmitterName=[ FirstChar | _T ], Categ } ) ->
	NewCateg = text_utils:format( "~ts.~c", [ Categ, FirstChar ] ),
	wooper:return_static( { EmitterName, NewCateg } );

subcategorize( EmmiterName ) ->
	wooper:return_static( EmmiterName ).


% (helper)
%subcategorize_name( EmitterName=[ FirstChar | _T ] ) ->
%   text_utils:format( "~c.~ts", [ FirstChar, EmitterName ] ).



% @doc Returns the names of all the base state attributes (be they defined by
% this class or inherited).
%
-spec get_all_base_attribute_names() ->
							static_return( [ wooper:attribute_name() ] ).
get_all_base_attribute_names() ->

	AttrNames =
			wooper_introspection:get_class_specific_attribute_names( ?MODULE )
		++ list_utils:flatten_once(
			[ wooper_introspection:get_class_specific_attribute_names( C )
				|| C <- ?superclasses ] ),

	wooper:return_static( AttrNames ).



% @doc Sends all types of traces on behalf of a test, thus without requiring a
% class_TraceEmitter state.
%
% Uses the default trace aggregator, supposed to be already available and
% registered.
%
-spec send_from_test( trace_severity(), message() ) -> static_void_return().
send_from_test( TraceSeverity, Message ) ->
	send_from_test( TraceSeverity, Message,
					?default_test_emitter_categorization ),
	wooper:return_static_void().



% @doc Sends all types of traces on behalf of a test, thus without requiring a
% class_TraceEmitter state.
%
% Uses the default trace aggregator, supposed to be already available and
% registered.
%
-spec send_from_test( trace_severity(), message(), emitter_categorization() ) ->
							static_void_return().
send_from_test( TraceSeverity, Message, EmitterCategorization ) ->

	% Follows the order of our trace format; oneway call:
	case naming_utils:get_registered_pid_for( ?trace_aggregator_name,
											  ?emitter_look_up_scope ) of

		undefined ->

			trace_utils:error( "class_TraceEmitter:send_from_test/3: "
							   "trace aggregator not found." ),

			throw( trace_aggregator_not_found );

		AggregatorPid ->

			cond_utils:if_defined( traces_are_preformatted,

				AggregatorPid ! { sendPreformatted, [
					class_TraceAggregator:format_as_advanced_trace(
						_TraceEmitterPid=self(),
						_TraceEmitterName= <<"test">>,
						_TraceEmitterCategorization=
							text_utils:string_to_binary(
								EmitterCategorization ),
						_AppTimestamp=none,
						_Time=time_utils:get_bin_textual_timestamp(),
						% No State available here
						_Location=net_utils:localnode_as_binary(),
						_MessageCategorization=
							?default_test_message_categorization,
						_Priority=trace_utils:get_priority_for( TraceSeverity ),
						_Message=text_utils:string_to_binary( Message ) ) ] },

				AggregatorPid ! { send, [
					_TraceEmitterPid=self(),
					_TraceEmitterName= <<"test">>,
					_TraceEmitterCategorization=
						text_utils:string_to_binary( EmitterCategorization ),
					_AppTimestamp=none,
					_Time=time_utils:get_bin_textual_timestamp(),
					% No State available here
					_Location=net_utils:localnode_as_binary(),
					_MessageCategorization=?default_test_message_categorization,
					_Priority=trace_utils:get_priority_for( TraceSeverity ),
					_Message=text_utils:string_to_binary( Message ) ] } )

	end,

	wooper:return_static_void().




% @doc Sends all types of traces on behalf of a case, thus without requiring a
% class_TraceEmitter state.
%
% Uses the default trace aggregator, supposed to be already available and
% registered.
%
-spec send_from_case( trace_severity(), message() ) -> static_void_return().
send_from_case( TraceSeverity, Message ) ->
	send_from_case( TraceSeverity, Message,
					?default_case_emitter_categorization ),
	wooper:return_static_void().



% @doc Sends all types of traces on behalf of a case, thus without requiring a
% class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
-spec send_from_case( trace_severity(), message(), emitter_categorization() ) ->
							static_void_return().
send_from_case( TraceSeverity, Message, EmitterCategorization ) ->

	% Follows the order of our trace format; oneway call:
	case naming_utils:get_registered_pid_for( ?trace_aggregator_name,
											  ?emitter_look_up_scope ) of

		undefined ->

			trace_utils:error( "class_TraceEmitter:send_from_case/3: "
							   "trace aggregator not found." ),

			throw( trace_aggregator_not_found );

		AggregatorPid ->

			cond_utils:if_defined( traces_are_preformatted,

				AggregatorPid ! { sendPreformatted, [
					class_TraceAggregator:format_as_advanced_trace(
						_TraceEmitterPid=self(),
						_TraceEmitterName= <<"case">>,
						_TraceEmitterCategorization=
							text_utils:string_to_binary(
								EmitterCategorization ),
						_AppTimestamp=none,
						_Time=time_utils:get_bin_textual_timestamp(),
						% No State available here
						_Location=net_utils:localnode_as_binary(),
						_MessageCategorization=
							?default_case_message_categorization,
						_Priority=trace_utils:get_priority_for( TraceSeverity ),
						_Message=text_utils:string_to_binary( Message ) ) ] },

				AggregatorPid ! { send, [
					_TraceEmitterPid=self(),
					_TraceEmitterName= <<"case">>,
					_TraceEmitterCategorization=
						text_utils:string_to_binary( EmitterCategorization ),
					_AppTimestamp=none,
					_Time=time_utils:get_bin_textual_timestamp(),
					% No State available here
					_Location=net_utils:localnode_as_binary(),
					_MessageCategorization=?default_case_message_categorization,
					_Priority=trace_utils:get_priority_for( TraceSeverity ),
					_Message=text_utils:string_to_binary( Message ) ] } )

	end,

	wooper:return_static_void().



% @doc Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses the default trace aggregator, supposed to be already available and
% registered.
%
-spec send_standalone( trace_severity(), message() ) -> static_void_return().
send_standalone( TraceSeverity, Message ) ->
	send_standalone( TraceSeverity, Message,
					 ?default_standalone_emitter_categorization ),
	wooper:return_static_void().



% @doc Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses the default trace aggregator, supposed to be already available and
% registered.
%
-spec send_standalone( trace_severity(), message(),
					   emitter_categorization() ) -> static_void_return().
send_standalone( TraceSeverity, Message, EmitterCategorization ) ->

	% Follows the order of our trace format; oneway call:
	case naming_utils:get_registered_pid_for( ?trace_aggregator_name,
											  ?emitter_look_up_scope ) of

		undefined ->

			trace_utils:error( "class_TraceEmitter:send_standalone/3: "
							   "trace aggregator not found." ),

			throw( trace_aggregator_not_found );


		AggregatorPid ->

			PidName = get_emitter_name_from_pid(),

			cond_utils:if_defined( traces_are_preformatted,

				AggregatorPid ! { sendPreformatted, [
					class_TraceAggregator:format_as_advanced_trace(
						_TraceEmitterPid=self(),
						_TraceEmitterName=
							text_utils:string_to_binary( PidName ),
						_TraceEmitterCategorization=
							text_utils:string_to_binary(
								EmitterCategorization ),
						_AppTimestamp=none,
						_Time=time_utils:get_bin_textual_timestamp(),
						% No State available here
						_Location=net_utils:localnode_as_binary(),
						_MessageCategorization=
							?default_standalone_message_categorization,
						_Priority=trace_utils:get_priority_for( TraceSeverity ),
						_Message=text_utils:string_to_binary( Message ) ) ] },

				AggregatorPid ! { send, [
					_TraceEmitterPid=self(),
					_TraceEmitterName=text_utils:string_to_binary( PidName ),
					_TraceEmitterCategorization=
						text_utils:string_to_binary( EmitterCategorization ),
					_AppTimestamp=none,
					_Time=time_utils:get_bin_textual_timestamp(),
					% No State available here
					_Location=net_utils:localnode_as_binary(),
					_MessageCategorization=
						?default_standalone_message_categorization,
					_Priority=trace_utils:get_priority_for( TraceSeverity ),
					_Message=text_utils:string_to_binary( Message ) ] } )

	end,

	wooper:return_static_void().



% @doc Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses the default trace aggregator, supposed to be already available and
% registered.
%
-spec send_standalone( trace_severity(), message(), emitter_name(),
					   emitter_categorization() ) -> static_void_return().
send_standalone( TraceSeverity, Message, EmitterName, EmitterCategorization ) ->
	send_standalone( TraceSeverity, Message, EmitterName, EmitterCategorization,
					 _MessageCategorization=uncategorized ),
	wooper:return_static_void().



% @doc Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses the default trace aggregator, supposed to be already available and
% registered.
%
-spec send_standalone( trace_severity(), message(), emitter_name(),
					   emitter_categorization(), message_categorization() ) ->
									static_void_return().
send_standalone( TraceSeverity, Message, EmitterName, EmitterCategorization,
				 MessageCategorization ) ->

	% Follows the order of our trace format; oneway call:
	case naming_utils:get_registered_pid_for( ?trace_aggregator_name,
											  ?emitter_look_up_scope ) of

		undefined ->
			trace_utils:error( "class_TraceEmitter:send_standalone/5: "
							   "trace aggregator not found." ),

			throw( trace_aggregator_not_found );

		AggregatorPid ->

			ActualMsgCateg = case is_atom( MessageCategorization ) of

				true ->
					MessageCategorization;

				_False ->
					text_utils:string_to_binary( MessageCategorization )

			end,

			cond_utils:if_defined( traces_are_preformatted,

				AggregatorPid ! { sendPreformatted, [
					class_TraceAggregator:format_as_advanced_trace(
						_TraceEmitterPid=self(),
						_TraceEmitterName=
							text_utils:string_to_binary( EmitterName ),
						_TraceEmitterCategorization=
							text_utils:string_to_binary(
								EmitterCategorization ),
						_AppTimestamp=none,
						_Time=time_utils:get_bin_textual_timestamp(),
						% No State available here
						_Location=net_utils:localnode_as_binary(),
						_MessageCategorization=ActualMsgCateg,
						_Priority=trace_utils:get_priority_for( TraceSeverity ),
						_Message=text_utils:string_to_binary( Message ) ) ] },

				AggregatorPid ! { send, [
					_TraceEmitterPid=self(),
					_TraceEmitterName=
						text_utils:string_to_binary( EmitterName ),
					_TraceEmitterCategorization=
						text_utils:string_to_binary( EmitterCategorization ),
					_AppTimestamp=none,
					_Time=time_utils:get_bin_textual_timestamp(),
					% No State available here
					_Location=net_utils:localnode_as_binary(),
					_MessageCategorization=ActualMsgCateg,
					_Priority=trace_utils:get_priority_for( TraceSeverity ),
					_Message=text_utils:string_to_binary( Message ) ] } )

	end,

	wooper:return_static_void().



% @doc Sends all types of traces without requiring a class_TraceEmitter state,
% in a safe manner (synchronously and, if their severity is error-like, echoed
% on the console).
%
% Uses the default trace aggregator, supposed to be already available and
% registered.
%
-spec send_standalone_safe( trace_severity(), message() ) ->
								static_void_return().
send_standalone_safe( TraceSeverity, Message ) ->

	EmitterCategorization = ?trace_emitter_categorization,

	ApplicationTimestamp = time_utils:get_textual_timestamp(),

	send_standalone_safe( TraceSeverity, Message, EmitterCategorization,
						  ApplicationTimestamp ),

	wooper:return_static_void().



% @doc Sends all types of traces without requiring a class_TraceEmitter state,
% in a safe manner (synchronously and, if their severity is error-like, echoed
% on the console).
%
% Uses the default trace aggregator, supposed to be already available and
% registered.
%
-spec send_standalone_safe( trace_severity(), message(),
							emitter_categorization() ) -> static_void_return().
send_standalone_safe( TraceSeverity, Message, EmitterCategorization ) ->

	ApplicationTimestamp = time_utils:get_textual_timestamp(),

	send_standalone_safe( TraceSeverity, Message, EmitterCategorization,
						  ApplicationTimestamp ),

	wooper:return_static_void().



% @doc Sends all types of traces without requiring a class_TraceEmitter state,
% in a safe manner (synchronously and, if their severity is error-like, echoed
% on the console).
%
% Uses the default trace aggregator, supposed to be already available and
% registered.
%
-spec send_standalone_safe( trace_severity(), message(),
			emitter_categorization(), app_timestamp() ) -> static_void_return().
send_standalone_safe( TraceSeverity, Message, EmitterCategorization,
					  ApplicationTimestamp ) ->

	EmitterName = get_emitter_name_from_pid(),

	send_standalone_safe( TraceSeverity, Message, EmitterName,
		EmitterCategorization, ?default_standalone_message_categorization,
		ApplicationTimestamp ),

	wooper:return_static_void().



% @doc Sends all types of traces without requiring a class_TraceEmitter state,
% in a safe manner (synchronously and, if their severity is error-like, echoed
% on the console).
%
% Uses the default trace aggregator, supposed to be already available and
% registered.
%
-spec send_standalone_safe( trace_severity(), message(), emitter_name(),
			emitter_categorization(), message_categorization() ) ->
						static_void_return().
send_standalone_safe( TraceSeverity, Message, EmitterName,
					  EmitterCategorization, MessageCategorization ) ->

	ApplicationTimestamp = time_utils:get_textual_timestamp(),

	send_standalone_safe( TraceSeverity, Message, EmitterName,
		EmitterCategorization, MessageCategorization, ApplicationTimestamp ),

	wooper:return_static_void().




% @doc Sends all types of traces without requiring a class_TraceEmitter state,
% in a safe manner (synchronously and, if their severity is error-like, echoed
% on the console).
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
-spec send_standalone_safe( trace_severity(), message(), emitter_name(),
		emitter_categorization(), message_categorization(), app_timestamp() ) ->
									static_void_return().
send_standalone_safe( TraceSeverity, Message, EmitterName,
		EmitterCategorization, MessageCategorization, ApplicationTimestamp ) ->

	% Follows the order of our trace format; request call:
	AggregatorPid = naming_utils:get_registered_pid_for( ?trace_aggregator_name,
		?emitter_look_up_scope ),

	TimestampText = text_utils:string_to_binary( ApplicationTimestamp ),

	ActualMsgCateg = case is_atom( MessageCategorization ) of

		true ->
			MessageCategorization;

		false ->
			% Must be a string then:
			text_utils:string_to_binary( MessageCategorization )

	end,

	BinMessage = text_utils:string_to_binary( Message ),

	%trace_utils:debug_fmt( "Sending in sync message '~ts'.", [ BinMessage ] ),

	cond_utils:if_defined( traces_are_preformatted,

		AggregatorPid ! { sendPreformattedSync, [
			class_TraceAggregator:format_as_advanced_trace(
				_TraceEmitterPid=self(),
				_TraceEmitterName=
					text_utils:string_to_binary( EmitterName ),
				_TraceEmitterCategorization=
					text_utils:string_to_binary( EmitterCategorization ),
				_AppTimestamp=none,
				_Time=TimestampText,
				% No State available here:
				_Location=net_utils:localnode_as_binary(),
				_BinMessageCategorization=ActualMsgCateg,
				_Priority=trace_utils:get_priority_for( TraceSeverity ),
				BinMessage ) ], self() },

		AggregatorPid ! { sendSync, [
			_TraceEmitterPid=self(),
			_TraceEmitterName=
				text_utils:string_to_binary( EmitterName ),
			_TraceEmitterCategorization=
				text_utils:string_to_binary( EmitterCategorization ),
			_AppTimestamp=none,
			_Time=TimestampText,
			% No State available here:
			_Location=net_utils:localnode_as_binary(),
			_BinMessageCategorization=ActualMsgCateg,
			_Priority=trace_utils:get_priority_for( TraceSeverity ),
			BinMessage ], self() } ),

	trace_utils:is_error_like( TraceSeverity ) andalso
		trace_utils:echo( Message, TraceSeverity, MessageCategorization,
						  TimestampText ),

	wait_aggregator_sync(),

	wooper:return_static_void().



% @doc Sends all types of traces without requiring a class_TraceEmitter state,
% based on a specified trace aggregator.
%
% For example useful with a logger handler, for lower message severities.
%
-spec send_direct( trace_severity(), message(), emitter_categorization(),
				   aggregator_pid() ) -> static_void_return().
send_direct( TraceSeverity, Message, BinEmitterCategorization,
			 AggregatorPid ) ->

	% Follows the order of our trace format; oneway call:

	PidName = get_emitter_name_from_pid(),

	cond_utils:if_defined( traces_are_preformatted,

		AggregatorPid ! { sendPreformatted, [
			class_TraceAggregator:format_as_advanced_trace(
				_TraceEmitterPid=self(),
				_TraceEmitterName=text_utils:string_to_binary( PidName ),
				_TraceEmitterCategorization=BinEmitterCategorization,
				_AppTimestamp=none,
				_Time=time_utils:get_bin_textual_timestamp(),
				% No State available here:
				_Location=net_utils:localnode_as_binary(),
				_BinMsgCateg=?default_standalone_message_categorization,
				_Priority=trace_utils:get_priority_for( TraceSeverity ),
				_Message=text_utils:string_to_binary( Message ) ) ] },

		AggregatorPid ! { send, [
			_TraceEmitterPid=self(),
			_TraceEmitterName=text_utils:string_to_binary( PidName ),
			_TraceEmitterCategorization=BinEmitterCategorization,
			_AppTimestamp=none,
			_Time=time_utils:get_bin_textual_timestamp(),
			% No State available here:
			_Location=net_utils:localnode_as_binary(),
			_BinMsgCateg=?default_standalone_message_categorization,
			_Priority=trace_utils:get_priority_for( TraceSeverity ),
			_Message=text_utils:string_to_binary( Message ) ] } ),

	wooper:return_static_void().



% @doc Sends all types of traces without requiring a class_TraceEmitter state,
% based on a specified trace aggregator.
%
% Allows to ensure synchronicity of the operation with the caller operation,
% typically to ensure no crash can affect a given emitted trace: the caller may
% wait for the trace_aggregator_synchronised WOOPER result message it will
% received before continuing on its operation.
%
% For example useful with a logger handler, for higher message severities.
%
-spec send_direct_synchronisable( trace_severity(), message(),
		emitter_categorization(), aggregator_pid() ) -> static_void_return().
send_direct_synchronisable( TraceSeverity, Message,
							BinEmitterCategorization, AggregatorPid ) ->

	% Follows the order of our trace format; oneway call:

	PidName = get_emitter_name_from_pid(),

	% This is a request, so the caller of this static method is expected to
	% perform a receive of its result, i.e. {wooper_result,
	% trace_aggregator_synchronised}:
	%
	cond_utils:if_defined( traces_are_preformatted,

		AggregatorPid ! { sendPreformattedSync, [
			class_TraceAggregator:format_as_advanced_trace(
				_TraceEmitterPid=self(),
				_TraceEmitterName=text_utils:string_to_binary( PidName ),
				_TraceEmitterCategorization=BinEmitterCategorization,
				_AppTimestamp=none,
				_Time=time_utils:get_bin_textual_timestamp(),
				% No State available here:
				_Location=net_utils:localnode_as_binary(),
				_BinMsgCateg=?default_standalone_message_categorization,
				_Priority=trace_utils:get_priority_for( TraceSeverity ),
				_Message=text_utils:string_to_binary( Message ) ) ],
							self() },

		AggregatorPid ! { sendSync, [
			_TraceEmitterPid=self(),
			_TraceEmitterName=text_utils:string_to_binary( PidName ),
			_TraceEmitterCategorization=BinEmitterCategorization,
			_AppTimestamp=none,
			_Time=time_utils:get_bin_textual_timestamp(),
			% No State available here:
			_Location=net_utils:localnode_as_binary(),
			_BinMsgCateg=?default_standalone_message_categorization,
			_Priority=trace_utils:get_priority_for( TraceSeverity ),
			_Message=text_utils:string_to_binary( Message ) ],
						  self() } ),

	wooper:return_static_void().


% @doc Returns the name of the trace channel corresponding to the trace
% priority.
%
% See also: trace_utils:get_priority_for/1.
%
-spec get_channel_name_for_priority( priority() ) ->
										static_return( trace_severity() ).
get_channel_name_for_priority( Priority ) ->
	wooper:return_static( trace_utils:get_severity_for( Priority ) ).




% Section for helper functions.


% @doc Returns a default emitter name, deduced from the PID of the corresponding
% process.
%
% (helper)
%
-spec get_emitter_name_from_pid() -> emitter_name().
get_emitter_name_from_pid() ->

	% Not wanting dots in PID here (otherwise this would be interpreted as
	% sub-categories in the traces):
	%
	text_utils:substitute( $., $-, pid_to_list( self() ) ).



% @doc Initialises some context-specific information.
%
% (helper)
%
-spec init( wooper:state() ) -> wooper:state().
init( State ) ->

	% Context-specific, useful to re-use, for example for deserialisation:

	% Retrieves the trace aggregator:
	%
	% - do not launch it if not available, otherwise the quasi-simultaneous
	% creations of multiple emitters would result in a race condition that would
	% lead to the creation of multiple aggregators
	%
	% - see implementation notes regarding the trace aggregator look-up scope
	%
	AggregatorPid = class_TraceAggregator:get_aggregator(
		_LaunchAggregator=false, ?emitter_look_up_scope ),

	setAttributes( State, [
		{ emitter_node, net_utils:localnode_as_binary() },
		{ trace_aggregator_pid, AggregatorPid } ] ).



% @doc Declares additionally a trace bridge in the process of this emitter
% instance.
%
% Allows functions implemented in lower-level libraries (typically relying on
% Myriad) that are called directly from this instance process, or helper
% functions with no corresponding WOOPER state, to plug to the same trace
% aggregator as used by this instance with mostly the same settings, through a
% corresponding trace bridge.
%
% See also: the register_as_bridge/{2,3} static methods, offered to normal
% (non-TraceEmitter, probably not even non-WOOPER) processes that nevertheless
% need to send traces.
%
% (helper)
%
-spec register_bridge( wooper:state() ) -> void().
register_bridge( State ) ->
	trace_bridge:register( _BridgeSpec={ ?getAttr(name),
		?getAttr(trace_emitter_categorization),
		?getAttr(trace_aggregator_pid) } ).



% Implementation of functions used by trace macros.


% @doc Returns the categorization of this trace emitter.
%
% (helper)
%
-spec get_categorization( wooper:state() ) -> bin_emitter_categorization().
get_categorization( State ) ->
	?getAttr(trace_emitter_categorization).



% @doc Sets the categorization of this trace emitter.
%
% Setting that categorization early in the constructor, before sending any
% trace, allows to have all traces for a given emitter be correctly gathered in
% the same category, which is a lot clearer when browsing traces afterwards.
%
% (helper)
%
-spec set_categorization( emitter_any_categorization(), wooper:state() ) ->
								wooper:state().
set_categorization( TraceEmitterAnyCategorization, State ) ->
	setAttribute( State, trace_emitter_categorization,
				  text_utils:ensure_binary( TraceEmitterAnyCategorization ) ) .




% @doc Sends the specified (unsynchronised) trace message from this emitter.
%
% Message is a plain string.
%
% All information are available here, except the trace timestamp and the message
% categorization.
%
% (helper)
%
-spec send( trace_severity(), wooper:state(), message() ) -> void().
send( TraceSeverity, State, Message ) ->
	send( TraceSeverity, State, Message, _MessageCategorization=uncategorized ).



% @doc Sends the specified (unsynchronised) trace message from this emitter,
% echoing it through basic traces as well.
%
% Message is a plain string.
%
% All information are available here, except the trace timestamp and the message
% categorization.
%
% (helper)
%
-spec send_safe( trace_severity(), wooper:state(), message() ) -> void().
send_safe( TraceSeverity, State, Message ) ->
	send_safe( TraceSeverity, State, Message,
			   _MessageCategorization=uncategorized ).



% @doc Sends the specified synchronised trace message (the synchronisation
% answer is requested and waited) from this emitter.
%
% All information are available here, except the trace timestamp and the message
% categorization.
%
% (helper)
%
-spec send_synchronised( trace_severity(), wooper:state(), message() ) ->
								void().
send_synchronised( TraceSeverity, State, Message ) ->
	send_synchronised( TraceSeverity, State, Message,
					   _MessageCategorization=uncategorized ).



% @doc Sends the specified (unsynchronised) trace message from this emitter.
%
% All information available but the timestamp, determining its availability.
%
% (helper)
%
-spec send( trace_severity(), wooper:state(), message(),
			message_categorization() ) -> void().
send( TraceSeverity, State, Message, MessageCategorization ) ->
	send( TraceSeverity, State, Message, MessageCategorization,
		  get_trace_timestamp( State ) ).



% @doc Sends the specified (unsynchronised) trace message from this emitter.
%
% All information available but the timestamp, determining its availability.
%
% (helper)
%
-spec send_safe( trace_severity(), wooper:state(), message(),
				 message_categorization() ) -> void().
send_safe( TraceSeverity, State, Message, MessageCategorization ) ->

	send_synchronisable( TraceSeverity, State, Message, MessageCategorization,
						 get_trace_timestamp( State ) ),

	trace_utils:echo( Message, TraceSeverity, MessageCategorization ),

	wait_aggregator_sync().



% @doc Sends the specified synchronised trace message (the synchronisation
% answer is requested and waited) from this emitter.
%
% All information available but the timestamp, determining its availability.
%
% (helper)
%
-spec send_synchronised( trace_severity(), wooper:state(), message(),
						 message_categorization() ) -> void().
send_synchronised( TraceSeverity, State, Message, MessageCategorization ) ->
	send_synchronised( TraceSeverity, State, Message, MessageCategorization,
					   get_trace_timestamp( State ) ).



% @doc Sends the specified (unsynchronised) trace message from this emitter.
%
% By far the most used sending primitive.
%
% (helper)
%
-spec send( trace_severity(), wooper:state(), message(),
			message_categorization(), app_timestamp() ) -> void().
send( TraceSeverity, State, Message, MessageCategorization, AppTimestamp ) ->

	ActualMsgCateg = case is_atom( MessageCategorization ) of

		true ->
			MessageCategorization;

		_False ->
			text_utils:string_to_binary( MessageCategorization )

	end,

	AppTimestampString = text_utils:term_to_binary( AppTimestamp ),

	% Follows the order of our trace format; oneway call:

	% Will be checked in terms of binary:
	TraceEmitterName = ?getAttr(name),

	% (this debug printout shall match the actual message sending)

	%trace_utils:debug_fmt( "Sending trace: PID=~w, emitter name='~p', "
	%   "emitter categorization='~p', "
	%   "app timestamp='~p', user time='~p', location='~p', "
	%   "message categorization='~p', trace type='~w', message='~p'~n",
	%   [ _TraceEmitterPid=self(),
	%     BinTraceEmitterName,
	%     _TraceEmitterCategorization=?getAttr(trace_emitter_categorization),
	%     AppTimestampString,
	%     _Time=TimestampText,
	%     _Location=?getAttr(emitter_node),
	%     _MessageCategorization=ActualMsgCateg,
	%     _Priority=trace_utils:get_priority_for( TraceSeverity ),
	%     _Message=text_utils:string_to_binary( Message ) ] ),

	% Just for extra debugging; typically usuful should a child class set again
	% its 'name' attribute, moreover with a faulty value (typically with the
	% name provided to its constructor - whereas it might not be a proper name
	% but a pair with the trace categorization, or it may be a string whereas we
	% expect now a binary string)
	%
	cond_utils:if_debug(
		text_utils:is_bin_string( TraceEmitterName ) orelse
			begin

				trace_utils:notice( "Hint: did you set the 'name' attribute "
					"after the construction of mother classes to, for example, "
					"a value of type string (instead of binary string)?" ),

				throw( { non_binary_string_emitter_name, TraceEmitterName } )

			end ),

	cond_utils:if_defined( traces_are_preformatted,

		?getAttr(trace_aggregator_pid) ! { sendPreformatted, [
			class_TraceAggregator:format_as_advanced_trace(
				_TraceEmitterPid=self(),
				TraceEmitterName,
				_TraceEmitterCategorization=
					?getAttr(trace_emitter_categorization),
				AppTimestampString,
				_Time=time_utils:get_bin_textual_timestamp(),
				_Location=?getAttr(emitter_node),
				_MessageCategorization=ActualMsgCateg,
				_Priority=trace_utils:get_priority_for( TraceSeverity ),
				_Message=text_utils:string_to_binary( Message ) ) ] },

		% Anyway most of the types will be checked by the aggregator (if the
		% traces_check_types token is set):
		%
		?getAttr(trace_aggregator_pid) ! { send, [
				_TraceEmitterPid=self(),
				TraceEmitterName,
				_TraceEmitterCategorization=
					?getAttr(trace_emitter_categorization),
				AppTimestampString,
				_Time=time_utils:get_bin_textual_timestamp(),
				_Location=?getAttr(emitter_node),
				_MessageCategorization=MsgCateg,
				_Priority=trace_utils:get_priority_for( TraceSeverity ),
				_Message=text_utils:string_to_binary( Message ) ] } ).



% Sends the specified (unsynchronised) trace message from this emitter, based on
% the specified emitter categorization.
%
% Primitive defined to be able to override the emitter categorization, typically
% to create from this emitter "sub-channels". Note that the emitter name will be
% still added at the end of it.
%
% (helper)
%
-spec send_categorized_emitter( trace_severity(), wooper:state(), message(),
								emitter_categorization() ) -> void().
send_categorized_emitter( TraceSeverity, State, Message,
						  EmitterCategorization ) ->

	AppTimestamp = get_trace_timestamp( State ),
	AppTimestampString = text_utils:term_to_binary( AppTimestamp ),

	cond_utils:if_defined( traces_are_preformatted,

		?getAttr(trace_aggregator_pid) ! { sendPreformatted, [
			class_TraceAggregator:format_as_advanced_trace(
				_TraceEmitterPid=self(),
				_TraceEmitterName=?getAttr(name),
				% Not: ?getAttr(trace_emitter_categorization)
				EmitterCategorization,
				AppTimestampString,
				_Time=time_utils:get_bin_textual_timestamp(),
				_Location=?getAttr(emitter_node),
				_MessageCategorization=uncategorized,
				_Priority=trace_utils:get_priority_for( TraceSeverity ),
				_Message=text_utils:string_to_binary( Message ) ) ] },

		% Anyway most of the types will be checked by the aggregator (if the
		% traces_check_types token is set):
		%
		?getAttr(trace_aggregator_pid) ! { send, [
			_TraceEmitterPid=self(),
			_TraceEmitterName=?getAttr(name),
			EmitterCategorization, % Not: ?getAttr(trace_emitter_categorization)
			AppTimestampString,
			_Time=time_utils:get_bin_textual_timestamp(),
			_Location=?getAttr(emitter_node),
			_MessageCategorization=uncategorized,
			_Priority=trace_utils:get_priority_for( TraceSeverity ),
			_Message=text_utils:string_to_binary( Message ) ] } ).



% Sends the specified (unsynchronised) trace message from this emitter, based on
% the specified emitter name.
%
% Primitive defined to be able to override the emitter full name.
%
% (helper)
%
-spec send_named_emitter( trace_severity(), wooper:state(), message(),
						  emitter_name() ) -> void().
send_named_emitter( TraceSeverity, State, Message, EmitterName ) ->

	AppTimestamp = get_trace_timestamp( State ),
	AppTimestampString = text_utils:term_to_binary( AppTimestamp ),

	cond_utils:if_defined( traces_are_preformatted,

		?getAttr(trace_aggregator_pid) ! { sendPreformatted, [
			class_TraceAggregator:format_as_advanced_trace(
				_TraceEmitterPid=self(),
				text_utils:ensure_binary( EmitterName ),
				_TraceEmitterCategorization=
					?getAttr(trace_emitter_categorization),
				AppTimestampString,
				_Time=time_utils:get_bin_textual_timestamp(),
				_Location=?getAttr(emitter_node),
				_MessageCategorization=uncategorized,
				_Priority=trace_utils:get_priority_for( TraceSeverity ),
				_Message=text_utils:string_to_binary( Message ) ) ] },

		% Anyway most of the types will be checked by the aggregator (if the
		% traces_check_types token is set):
		%
		?getAttr(trace_aggregator_pid) ! { send, [
			_TraceEmitterPid=self(),
			text_utils:ensure_binary( EmitterName ),
			_TraceEmitterCategorization=?getAttr(trace_emitter_categorization),
			AppTimestampString,
			_Time=time_utils:get_bin_textual_timestamp(),
			_Location=?getAttr(emitter_node),
			_MessageCategorization=uncategorized,
			_Priority=trace_utils:get_priority_for( TraceSeverity ),
			_Message=text_utils:string_to_binary( Message ) ] } ).



% @doc Sends the specified synchronisable trace message from this emitter.
%
% The synchronisation answer is requested yet not awaited here, to allow for any
% interleaving.
%
-spec send_synchronisable( trace_severity(), wooper:state(), message(),
					message_categorization(), app_timestamp() ) -> void().
send_synchronisable( TraceSeverity, State, Message, MessageCategorization,
					 AppTimestamp ) ->

	% Almost exactly the same as send/5, except that the sendSync/10 agggregator
	% request is called instead of the send/10 oneway, so that it sends an
	% acknowlegment when done.

	ActualMsgCateg = case is_atom( MessageCategorization ) of

		true ->
			MessageCategorization;

		_False ->
			text_utils:string_to_binary( MessageCategorization )

	end,

	AppTimestampString = text_utils:term_to_binary( AppTimestamp ),

	% Follows the order of our trace format; request call:
	% (toggle the comment for the two blocks below to debug)

	cond_utils:if_defined( traces_are_preformatted,

		?getAttr(trace_aggregator_pid) ! { sendPreformattedSync, [
			class_TraceAggregator:format_as_advanced_trace(
				_TraceEmitterPid=self(),
				_TraceEmitterName=?getAttr(name),
				_TraceEmitterCategorization=
					?getAttr(trace_emitter_categorization),
				AppTimestampString,
				_Time=time_utils:get_bin_textual_timestamp(),
				_Location=?getAttr(emitter_node),
				_MessageCategorization=ActualMsgCateg,
				_Priority=trace_utils:get_priority_for( TraceSeverity ),
				_Message=text_utils:string_to_binary( Message ) ) ],
				self() },

		?getAttr(trace_aggregator_pid) ! { sendSync, [
		%trace_utils:debug_fmt( "Sending trace: PID=~w, emitter name='~p', "
		%   "emitter categorization='~p', "
		%   "app timestamp='~p', user time='~p', location='~p', "
		%   "message categorization='~p', trace type='~w', message='~p'.",
			_TraceEmitterPid=self(),
			_TraceEmitterName=?getAttr(name),
			_TraceEmitterCategorization=?getAttr(trace_emitter_categorization),
			AppTimestampString,
			_Time=time_utils:get_bin_textual_timestamp(),
			_Location=?getAttr(emitter_node),
			_MessageCategorization=MsgCateg,
			_Priority=trace_utils:get_priority_for( TraceSeverity ),
			_Message=text_utils:string_to_binary( Message ) ],
			self()
		% ).
										 } ).




% @doc Sends the specified synchronised trace message (the synchronisation
% answer is requested and awaited) from this emitter.
%
% (helper)
%
-spec send_synchronised( trace_severity(), wooper:state(), message(),
		message_categorization(), app_timestamp() ) -> void().
send_synchronised( TraceSeverity, State, Message, MessageCategorization,
				   AppTimestamp ) ->

	send_synchronisable( TraceSeverity, State, Message, MessageCategorization,
						 AppTimestamp ),

	wait_aggregator_sync().



% @doc Sends the specified synchronised trace message that is furthermore
% echoed, from this emitter.
%
% Sends all types of synchronised traces (the synchronisation answer is
% requested and awaited).
%
% (helper)
%
-spec send_safe( trace_severity(), wooper:state(), message(),
				 message_categorization(), app_timestamp() ) -> void().
send_safe( TraceSeverity, State, Message, MessageCategorization,
		   AppTimestamp ) ->

	send_synchronisable( TraceSeverity, State, Message, MessageCategorization,
						 AppTimestamp ),

	trace_utils:echo( Message, TraceSeverity, MessageCategorization,
					  text_utils:term_to_string( AppTimestamp ) ),

	wait_aggregator_sync().



% @doc Waits for the aggregator to report that a trace synchronisation has been
% completed.
%
% (helper)
%
-spec wait_aggregator_sync() -> void().
wait_aggregator_sync() ->
	receive

		{ wooper_result, trace_aggregator_synchronised } ->
			ok

	end.



% @doc Returns the current trace-level timestamp (that is possibly an execution
% tick offset), or the atom 'none' if the emitter time is not known.
%
% (helper)
%
-spec get_trace_timestamp( wooper:state() ) -> app_timestamp().
get_trace_timestamp( State ) ->

	% Note: if an exception "No key 'trace_timestamp' found in following table:
	% empty hashtable" is triggered, probably that this State is not (yet?) a
	% TraceEmitter one (e.g. if using the blank state of a constructor in
	% ?debug(...) instead of using ?send_debug(ATraceState,...)).
	%
	?getAttr(trace_timestamp).



% @doc Returns the current trace-level timestamp, as a binary string.
%
% (helper)
%
-spec get_trace_timestamp_as_binary( wooper:state() ) -> bin_string().
get_trace_timestamp_as_binary( State ) ->
	text_utils:term_to_binary( ?getAttr(trace_timestamp) ).




% @doc Returns the name of this trace emitter, as a plain string (not as a
% binary).
%
% (helper)
%
-spec get_plain_name( wooper:state() ) -> ustring().
get_plain_name( State ) ->
	text_utils:binary_to_string( ?getAttr(name) ).



% @doc Returns a short description of this trace emitter.
%
% Typically useful in traces.
%
-spec get_short_description( wooper:state() ) -> ustring().
get_short_description( State ) ->
	text_utils:format( "named '~ts', categorised as '~ts'",
		[ ?getAttr(name), ?getAttr(trace_emitter_categorization) ] ).



% @doc Synchronises the caller with the trace aggregator, ensuring that all
% (asynchronous) operations it triggered on this aggregator are over.
%
% Useful to ensure that traces have been fully received and stored before
% continuing (possibly with a VM crash).
%
-spec sync( wooper:state() ) -> void().
sync( State ) ->

	?getAttr(trace_aggregator_pid) ! { sync, [], self() },

	receive

		{ wooper_result, trace_aggregator_synchronised } ->
			ok

	end.



% @doc Awaits for the completion of trace (echoed) outputs.
%
% No firm guarantee, done of a best-effort basis.
%
-spec await_output_completion() -> void().
await_output_completion() ->
	system_utils:await_output_completion( _Milliseconds=200 ).
