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


-module(class_TestTraceEmitter).


-define( class_description,
		 "Trace emitter child class introduced for testing."
		 "See class_TraceEmitter.hrl and class_TraceEmitter.erl." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_TraceEmitter ] ).


-define( class_attributes, [] ).


% Used by the trace_categorize/1 macro to use the right emitter:
-define( trace_emitter_categorization, "TraceEmitter.Test" ).


% Exported helpers:
-export([ send_traces/1, send_emergency_trace/1, send_debug_trace/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Only to test the trace system:
-define( LogPrefix, "[Test TraceEmitter]" ).


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new test trace emitter.
-spec construct( wooper:state(), class_TraceEmitter:emitter_init() ) ->
					 wooper:state().
construct( State, TraceEmitterName ) ->

	trace_utils:notice_fmt( "~ts Creating a new test trace emitter, whose name "
		"is ~p, whose PID is ~w.", [ ?LogPrefix, TraceEmitterName, self() ] ),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State,
								?trace_categorize(TraceEmitterName) ),

	% From now on, traces can be sent (but, from the constructor, send_* traces
	% only should be sent, to be able to refer to a trace-enabled state):

	?send_emergency( TraceState, "Hello emergency world!" ),
	?send_alert( TraceState, "Hello alert world!" ),
	?send_critical( TraceState, "Hello critical world!" ),
	?send_error( TraceState, "Hello error world!" ),
	?send_warning( TraceState, "Hello warning world!" ),
	?send_notice( TraceState, "Hello notice world!" ),
	?send_info( TraceState, "Hello trace world!" ),
	?send_debug( TraceState, "Hello debug world!" ),
	?send_void( TraceState, "Hello void world!" ),

	TraceState.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	trace_utils:notice_fmt( "~ts Deleting test trace emitter ~ts.",
							[ ?LogPrefix, ?getAttr(name) ] ),

	% Last moment to send traces:
	?emergency( "Goodbye emergency world!" ),
	?alert( "Goodbye alert world!" ),
	?critical( "Goodbye critical world!" ),
	?error( "Goodbye error world!" ),
	?warning( "Goodbye warning world!" ),
	?notice( "Goodbye notice world!" ),
	?info( "Goodbye info world!" ),
	?debug( "Goodbye debug world!" ),
	?void( "Goodbye void world!" ),

	trace_utils:notice_fmt( "~ts Test trace emitter ~ts deleted.",
							[ ?LogPrefix, ?getAttr(name) ] ),

	% Allows chaining:
	State.




% Methods section.


-spec sendTraces( wooper:state() ) -> const_request_return( 'ok' ).
sendTraces( State ) ->

	%trace_utils:notice_fmt( "~ts Sending some traces.", [ ?LogPrefix ] ),

	%send_traces( State ),

	send_traces_benchmark( State ),

	wooper:const_return_result( ok ).



-spec sendAsyncTraces( wooper:state() ) -> const_oneway_return().
sendAsyncTraces( State ) ->

	%trace_utils:notice_fmt( "~ts Sending some asynchronous traces.",
	% [ ?LogPrefix ] ),

	%send_traces( State ),

	send_traces_benchmark( State ),

	wooper:const_return().



% Helper functions.


% We should be testing all forms of traces here.
-spec send_traces( wooper:state() ) -> void().
send_traces( State ) ->

	%trace_utils:notice_fmt( "~ts Sending some traces.", [ ?LogPrefix ] ),

	% We used to replace emergency, alert, critical and error traces by warning
	% ones, as the former ones induced fixed waitings (i.e. timer:sleep/1
	% calls), yet now failure-oriented traces (warning, error, critical, alert
	% and emergency) rely on an explicit synchronisation with the trace
	% aggregator.

	% With no formatting:

	?emergency( "Still livin' in an emergency world! (plain)" ),
	?alert( "Still livin' in an alert world! (plain)" ),
	?critical( "Still livin' in a critical world! (plain)" ),
	?error( "Still livin' in an error world! (plain)" ),
	?warning( "Still livin' in a warning world! (plain)" ),
	?notice( "Still livin' in a notice world! (plain)" ),
	?info( "Still livin' in an info world! (plain)" ),
	?debug( "Still livin' in a debug world! (plain)" ),
	?void( "Still livin' in a void world! (plain)" ),


	?emergency_cat( "Still livin' in an emergency world! (cat)",
					?application_start ),

	?alert_cat( "Still livin' in an alert world! (cat)",
				?application_start ),

	?critical_cat( "Still livin' in a critical world! (cat)",
				   ?application_start ),

	?error_cat( "Still livin' in an error world! (cat)",
				 ?application_save ),

	?warning_cat( "Still livin' in a warning world! (cat)", ?time ),

	?notice_cat( "Still livin' in a notice world! (cat)", ?execution ),

	?info_cat( "Still livin' in an info world! (cat)",
			   ?application_start ),

	?debug_cat( "Still livin' in a debug world! (cat)",
				?application_start ),

	?void_cat( "Still livin' in a void world! (cat)",
			   ?application_start ),


	?emergency_full( "Still livin' in an emergency world! (full)",
					 ?application_start, 5 ),

	?alert_full( "Still livin' in an alert world! (full)",
				 ?application_start, 5 ),

	?critical_full( "Still livin' in a critical world! (full)",
					?application_start, 5 ),

	?error_full( "Still livin' in an error world! (full)",
				 ?application_save, 6 ),

	?warning_full( "Still livin' in a warning world! (full)",
				   ?time, 7 ),

	?notice_full( "Still livin' in a notice world! (full)",
				  ?execution, 8 ),

	?info_full( "Still livin' in an info world! (full)",
				?application_start, 10 ),

	?debug_full( "Still livin' in a debug world! (full)",
				 ?application_start, 10 ),

	?void_full( "Still livin' in a void world! (full)",
				?application_start, 11 ),


	% With formatting:

	?emergency_fmt( "Yes, still livin' in a ~w world! (plain)", [emergency] ),
	?alert_fmt( "Yes, still livin' in a ~w world! (plain)", [alert] ),
	?critical_fmt( "Yes, still livin' in a ~w world! (plain)", [critical] ),
	?error_fmt( "Yes, still livin' in a ~w world! (plain)", [error] ),
	?warning_fmt( "Yes, still livin' in a ~w world! (plain)", [warning] ),
	?notice_fmt( "Yes, still livin' in a ~w world! (plain)", [notice] ),
	?info_fmt( "Yes, still livin' in a ~w world! (plain)", [info] ),
	?debug_fmt( "Yes, still livin' in a ~w world! (plain)", [debug] ),
	?void_fmt( "Yes, still livin' in a ~w world! (plain)", [void] ),


	?emergency_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [emergency], ?application_start ),
	?alert_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [alert], ?application_start ),
	?critical_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [critical], ?application_start ),
	?error_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [error], ?application_save ),
	?warning_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [warning], ?time ),
	?notice_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [notice], ?execution ),
	?info_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [info], ?application_start ),
	?debug_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [debug], ?application_start ),
	?void_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [void], ?application_start ),


	?emergency_fmt_full( "Oh yeah ~w", [emergency], ?application_start, 5 ),
	?alert_fmt_full( "Oh yeah ~w", [alert], ?application_start, 5 ),
	?critical_fmt_full( "Oh yeah ~w", [critical], ?application_start, 5 ),
	?error_fmt_full( "Oh yeah ~w", [error], ?application_save, 6 ),
	?warning_fmt_full( "Oh yeah ~w", [warning], ?time, 7 ),
	?notice_fmt_full( "Oh yeah ~w", [notice], ?execution, 8 ),
	?info_fmt_full( "Oh yeah ~w", [info], ?application_start, 9 ),
	?debug_fmt_full( "Oh yeah ~w", [debug], ?application_start, 10 ),
	?void_fmt_full( "Oh yeah ~w", [void], ?application_start, 11 ).



% To test compilation problems when only one non-maskable trace is used (ex:
% variable unused, or term constructed whereas not used either).
%
-spec send_emergency_trace( wooper:state() ) -> void().
send_emergency_trace( State ) ->

	Message = "Unique ~w trace!",

	Format = [ emergency ],

	Categ = ?application_start,

	?emergency_fmt_cat( Message, Format, Categ ).



% To test compilation problems when only one maskable trace is used (ex:
% variable unused, or term constructed whereas not used either).
%
-spec send_debug_trace( wooper:state() ) -> void().
send_debug_trace( State ) ->

	Message = "Unique ~w trace!",

	Format = [ debug ],

	Categ = ?application_start,

	?debug_fmt_cat( Message, Format, Categ ).



-spec send_traces_benchmark( wooper:state() ) -> void().
send_traces_benchmark( State ) ->

	%trace_utils:notice_fmt( "~ts Sending some traces.", [ ?LogPrefix ] ),

	% We used to replace emergency, alert, critical and error traces by warning
	% ones, as the former ones induced fixed waitings (i.e. timer:sleep/1
	% calls), yet now failure-oriented traces (warning, error, critical, alert
	% and emergency) rely on an explicit synchronisation with the trace
	% aggregator.

	% With no formatting:

	?warning( "Still livin' in an emergency world! (plain)" ),
	?warning( "Still livin' in an alert world! (plain)" ),
	?warning( "Still livin' in a critical world! (plain)" ),
	?warning( "Still livin' in an error world! (plain)" ),
	?warning( "Still livin' in a warning world! (plain)" ),
	?notice( "Still livin' in a notice world! (plain)" ),
	?info( "Still livin' in a info world! (plain)" ),
	?debug( "Still livin' in a debug world! (plain)" ),
	?void( "Still livin' in a void world! (plain)" ),


	% Not replaced:

	?emergency_cat( "Still livin' in an emergency world! (cat)",
					?application_start ),

	?alert_cat( "Still livin' in an alert world! (cat)",
				?application_start ),

	?critical_cat( "Still livin' in a critical world! (cat)",
				   ?application_start ),

	?error_cat( "Still livin' in an error world! (cat)",
				?application_save ),

	?warning_cat( "Still livin' in a warning world! (cat)",
				  ?time ),

	?notice_cat( "Still livin' in a notice world! (cat)",
				 ?execution ),

	?info_cat( "Still livin' in an info world! (cat)",
				?application_start ),

	?debug_cat( "Still livin' in a debug world! (cat)",
				?application_start ),

	?void_cat( "Still livin' in a void world! (cat)",
			   ?application_start ),



	?emergency_full( "Still livin' in an emergency world! (full)",
					 ?application_start, 5 ),

	?alert_full( "Still livin' in an alert world! (full)",
				 ?application_start, 5 ),

	?critical_full( "Still livin' in a critical world! (full)",
					?application_start, 5 ),

	?error_full( "Still livin' in an error world! (full)",
				 ?application_save, 6 ),

	?warning_full( "Still livin' in a warning world! (full)",
				   ?time, 7 ),

	% Useful also to test non-integer timestamps (works correctly with the trace
	% supervisors as they are):
	%
	?notice_full( "Still livin' in a notice world! (full)",
				  ?execution, {8,2} ),

	?info_full( "Still livin' in an info world! (full)",
				 ?application_start, 9 ),

	?debug_full( "Still livin' in a debug world! (full)",
				 ?application_start, 10 ),

	?void_full( "Still livin' in a void world! (full)",
				?application_start, 11 ),



	% With formatting:

	?emergency_fmt( "Yes, still livin' in a ~w world! (plain)", [emergency] ),
	?alert_fmt( "Yes, still livin' in a ~w world! (plain)", [alert] ),
	?critical_fmt( "Yes, still livin' in a ~w world! (plain)", [critical] ),
	?error_fmt( "Yes, still livin' in a ~w world! (plain)", [error] ),
	?warning_fmt( "Yes, still livin' in a ~w world! (plain)", [warning] ),
	?notice_fmt( "Yes, still livin' in a ~w world! (plain)", [notice] ),
	?info_fmt( "Yes, still livin' in a ~w world! (plain)", [info] ),
	?debug_fmt( "Yes, still livin' in a ~w world! (plain)", [debug] ),
	?void_fmt( "Yes, still livin' in a ~w world! (plain)", [void] ),



	?emergency_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [emergency], ?application_start ),
	?alert_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [alert], ?application_start ),
	?critical_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [critical], ?application_start ),
	?error_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [error], ?application_save ),
	?warning_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [warning], ?time ),
	?notice_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [notice], ?execution ),
	?info_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [info], ?application_start ),
	?debug_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [debug], ?application_start ),
	?void_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [void], ?application_start ),


	?emergency_fmt_full( "Oh yeah ~w", [emergency], ?application_start, 5 ),
	?alert_fmt_full( "Oh yeah ~w", [alert], ?application_start, 5 ),
	?critical_fmt_full( "Oh yeah ~w", [critical], ?application_start, 5 ),
	?error_fmt_full( "Oh yeah ~w", [error], ?application_save, 6 ),
	?warning_fmt_full( "Oh yeah ~w", [warning], ?time, 7 ),
	?notice_fmt_full( "Oh yeah ~w", [notice], ?execution, 8 ),
	?info_fmt_full( "Oh yeah ~w", [info], ?application_start, 9 ),
	?debug_fmt_full( "Oh yeah ~w", [debug], ?application_start, 10 ),
	?void_fmt_full( "Oh yeah ~w", [void], ?application_start, 11 ).
