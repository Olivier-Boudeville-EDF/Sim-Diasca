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


% Note: this header file must be included *after* the WOOPER one, as, in
% production mode, this file has to define functions (namely
% trace_disabled/{1,2,3,4,5}).



% This header centralizes notably all macros related to the sending of traces
% from a class_TraceEmitter.

% See also traces.hrl, for the standalone sending of traces.


-ifndef(class_TraceEmitter_hrl_guard).
-define(class_TraceEmitter_hrl_guard,).




% Allows to set the right emitter categorization, which is the first, childmost
% class of the hierarchy of an instance.
%
% Otherwise this category would have to be explicitly set as an attribute
% afterwards in the constructor (more cumbersome, and any trace sent in-between,
% typically from the constructors of mother classes, would then not bear the
% right emitter category).
%
% As a consequence, each trace-emitting class shall:
%
%  - define its emitter categorization, thanks to the
%  trace_emitter_categorization define; for example:
%      -define( trace_emitter_categorization, "vehicle.car.sedan" ).
%
%  - call the trace_categorize/1 macro with the
%  class_TraceEmitter:emitter_init/0 construction parameter; for example:
%      ?trace_categorize(InstanceName)
%
% See the class_TestTraceEmitter.erl file for an actual example.
%
% Should this macro not be used in a constructor, the actual trace
% categorization will be the first one (if any) set through the upstream
% inheritance. If none of these classes ever set its
% trace_emitter_categorization define, it will be default to the
% default_trace_emitter_categorization one.
%
% -macrospec trace_categorize( emitter_name() | emitter_info() ) ->
%                                   emitter_info().
-define( trace_categorize( TracesInitialisationTermInternal ),

		 % As few variables bound as possible, and longer variable names chosen
		 % (prefixed with 'Traces'), to avoid clashes with user-defined
		 % variables:
		 %
		 case TracesInitialisationTermInternal of

			 % Emitter categorization already set, just propagate as is to next
			 % mother class:
			 %
			 %{ _TraceName, _TraceCategorization } ->
			 { _, _ } ->
				 TracesInitialisationTermInternal;

			 % Expecting here only a string (we are at the level of the actual,
			 % effective class), it is the place where the emitter
			 % categorisation shall be introduced:
			 %
			 %TracesStandaloneEmitterName ->
			 _ ->
				 { TracesInitialisationTermInternal,
				   ?trace_emitter_categorization }

		 end
).


% To obtain a proper string-like name, whether a trace categorization has been
% specified or not (typically useful when wanting to designate with '~ts' the
% name of a trace emitter from its constructor, i.e. when its specified name
% (actually, emitter_info()) may still include its trace categorization):
%
% -macrospec trace_name( emitter_name() | emitter_info() ) ->
%                                   emitter_name().
-define( trace_name( TracesInitialisationTermForNameInternal ),

		 % As few variables bound as possible, and longer variable names chosen
		 % (prefixed with 'Traces'), to avoid clashes with user-defined
		 % variables:
		 %
		 case TracesInitialisationTermForNameInternal of

			 %{ TraceName, _TraceCategorization } ->
			 { TraceNameForNameInternal, _ } ->
				 TraceNameForNameInternal;

			 TraceNameForNameInternal ->
				 TraceNameForNameInternal

		 end
).




% Conventions section.


% For failure-oriented traces (warning, error, critical, alert and emergency),
% an explicit synchronisation with the trace aggregator is made (plus an echo on
% the console), so that the trace emission is more reliable; as a result it
% becomes itself blocking, for a minimal needed duration (incomparably superior
% to a timer:sleep/1).
%
% That way, even if the very next operation of the process sending such a
% failure-oriented trace is to halt the VM (ex: with an uncaught throw/1), the
% corresponding trace will be duely recorded and thus available (otherwise such
% critical messages could get lost).



% Technical identifier will be the PID of the trace emitter.

% Name will be the name specified at the creation of the trace emitter.

% EmitterCategorization will the one specified at the creation of the trace
% emitter. It could be deduced from superclasses as well, although it would be
% generally uselessly long and would cause issues in case of multiple
% inheritance.


% User timestamp will be the current date, as determined by the trace emitter.

% Location will be the current Erlang node, as determined by the trace emitter.

% MessageCategorization will be either specified in the send macro, or
% set the default categorization.

% Following built-in message categorizations are available:

-define( system, "System" ).

	-define( management, ?system".Management" ).

		-define( application_start,    ?management".ExecutionStart"    ).
		-define( application_resume,   ?management".ExecutionResume"   ).
		-define( application_continue, ?management".ExecutionContinue" ).
		-define( application_stop,     ?management".ExecutionStop"     ).
		-define( application_save,     ?management".ExecutionSave"     ).
		-define( application_load,     ?management".ExecutionLoad"     ).

	-define( time,      ?system".Time"      ).
	-define( random,    ?system".Random"    ).
	-define( lifecycle, ?system".Lifecycle" ).


-define( execution, "Execution" ).

	-define( update, ?execution".Update" ).
	-define( state,  ?execution".State"  ).

-define( default_message_categorization, ?execution ).


% Severity will be determined from the name of the chosen macro: emergency,
% alert, critical, error, warning, notice, info or debug (or void).

% Message will be directly specified in the macro call.


-ifndef(trace_emitter_categorization).

	-define( trace_emitter_categorization, "Traces.Uncategorized" ).

-endif. % trace_emitter_categorization



% Section dedicated to trace emitters that are not WOOPER-based and are
% dedicated to tests.
%
% See also: test_constructs.hrl.
%
-define( default_test_message_categorization, "Test" ).


% Section dedicated to trace emitters that are not WOOPER-based and are
% dedicated to cases.
%
% See also: case_constructs.hrl.
%
-define( default_case_message_categorization, "Case" ).


% Section dedicated to trace emitters that are not WOOPER-based and dedicated to
% classical functions (as opposed to methods from class_TraceEmitter).
%
% See also: traces.hrl.
%
-define( default_standalone_message_categorization, "Standalone" ).



% When no emitter is specified:
%
-define( default_test_emitter_categorization, "Test" ).

-define( default_case_emitter_categorization, "Case" ).

-define( default_standalone_emitter_categorization, "Standalone" ).

-define( default_trace_emitter_categorization, "uncategorized" ).


% We moved away from the tracing_activated conditional sections the most severe
% trace sendings (namely emergency, alert, critical, error and warning), as in
% all cases (whether or not the traces are activated), we want them, and both as
% actual traces and as console outputs, as they should not be missed.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Emergency section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Emergency, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'emergency' type with specified parameters and an explicit
% state.
%
-define( send_emergency( State, Message ),
		 class_TraceEmitter:send_safe( emergency, State, Message )
).



% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State'.
%
-define( emergency( Message ),
		 class_TraceEmitter:send_safe( emergency, State, Message )
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'emergency' type with specified parameters and an explicit
% state.
%
-define( send_emergency_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( emergency, State, Message,
									   MessageCategorization )
).



% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State'.
%
-define( emergency_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( emergency, State, Message,
									   MessageCategorization )
).




% Categorized with application-specific timestamp, with 2 variations regarding
% state: explicit or implicit.



% Sends a trace of 'emergency' type with specified parameters and an explicit
% state.
%
-define( send_emergency_full( State, Message, MessageCategorization,
						  ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( emergency, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State'.
%
-define( emergency_full( Message, MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( emergency, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).




% Subsection for Emergency, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'emergency' type (echoed on the console) with specified
% parameters and an explicit state.
%
-define( send_emergency_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( emergency, State,
								text_utils:format( Message, FormatValues ) )

).


% Sends a trace of 'emergency' type with specified parameters and an explicit
% state, with no console echo (message just recorded in the traces).
%
-define( send_emergency_no_echo_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_synchronised( emergency, State,
						  text_utils:format( Message, FormatValues ) )
).



% Sends a trace of 'emergency' type (echoed on the console) with specified
% parameters and implicit use of a variable named 'State'.
%
-define( emergency_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( emergency, State,
								text_utils:format( Message, FormatValues ) )
).


% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State', with no console echo (message just recorded in
% the traces).
%
-define( emergency_no_echo_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_synchronised( emergency, State,
							text_utils:format( Message, FormatValues ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'emergency' type with specified parameters and an explicit
% state.
%
-define( send_emergency_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send_safe( emergency, State,
		   text_utils:format( Message, FormatValues ), MessageCategorization )
).



% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State'.
%
-define( emergency_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( emergency, State,
		   text_utils:format( Message, FormatValues ), MessageCategorization )
).




% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'emergency' type with specified parameters and an explicit
% state.
%
-define( send_emergency_fmt_full( State, Message, FormatValues,
							  MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( emergency, State,
				text_utils:format( Message, FormatValues ),
				MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State'.
%
-define( emergency_fmt_full( Message, FormatValues, MessageCategorization,
						 ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( emergency, State,
				text_utils:format( Message, FormatValues ),
				MessageCategorization, ApplicationTimestamp )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Alert section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Alert, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'alert' type with specified parameters and an explicit state.
-define( send_alert( State, Message ),
		 class_TraceEmitter:send_safe( alert, State, Message )
).



% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( alert( Message ),
		 class_TraceEmitter:send_safe( alert, State, Message )
).




% Categorized, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'alert' type with specified parameters and an explicit state.
-define( send_alert_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( alert, State, Message,
									   MessageCategorization )
).



% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( alert_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( alert, State, Message,
									   MessageCategorization )
).




% Categorized with application-specific timestamp, with 2 variations regarding
% state: explicit or implicit.



% Sends a trace of 'alert' type with specified parameters and an explicit state.
-define( send_alert_full( State, Message, MessageCategorization,
						  ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( alert, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( alert_full( Message, MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( alert, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).




% Subsection for Alert, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'alert' type (echoed on the console) with specified
% parameters and an explicit state.
%
-define( send_alert_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( alert, State,
								text_utils:format( Message, FormatValues ) )

).


% Sends a trace of 'alert' type with specified parameters and an explicit state,
% with no console echo (message just recorded in the traces).
%
-define( send_alert_no_echo_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_synchronised( alert, State,
						  text_utils:format( Message, FormatValues ) )
).



% Sends a trace of 'alert' type (echoed on the console) with specified
% parameters and implicit use of a variable named 'State'.
%
-define( alert_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( alert, State,
								text_utils:format( Message, FormatValues ) )
).


% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State', with no console echo (message just recorded in the
% traces).
%
-define( alert_no_echo_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_synchronised( alert, State,
							text_utils:format( Message, FormatValues ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'alert' type with specified parameters and an explicit state.
-define( send_alert_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send_safe( alert, State,
		   text_utils:format( Message, FormatValues ), MessageCategorization )
).



% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( alert_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( alert, State,
		   text_utils:format( Message, FormatValues ), MessageCategorization )
).




% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'alert' type with specified parameters and an explicit state.
-define( send_alert_fmt_full( State, Message, FormatValues,
							  MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( alert, State,
				text_utils:format( Message, FormatValues ),
				MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( alert_fmt_full( Message, FormatValues, MessageCategorization,
						 ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( alert, State,
				text_utils:format( Message, FormatValues ),
				MessageCategorization, ApplicationTimestamp )
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Critical section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Critical, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'critical' type with specified parameters and an explicit
% state.
%
-define( send_critical( State, Message ),
		 class_TraceEmitter:send_safe( critical, State, Message )
).



% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( critical( Message ),
		 class_TraceEmitter:send_safe( critical, State, Message )
).




% Categorized, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'critical' type with specified parameters and an explicit
% state.
%
-define( send_critical_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( critical, State, Message,
									   MessageCategorization )
).



% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( critical_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( critical, State, Message,
									   MessageCategorization )
).




% Categorized with application-specific timestamp, with 2 variations regarding
% state: explicit or implicit.



% Sends a trace of 'critical' type with specified parameters and an explicit
% state.
%
-define( send_critical_full( State, Message, MessageCategorization,
							 ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( critical, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( critical_full( Message, MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( critical, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).




% Subsection for Critical, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'critical' type (echoed on the console) with specified
% parameters and an explicit state.
%
-define( send_critical_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( critical, State,
								text_utils:format( Message, FormatValues ) )

).


% Sends a trace of 'critical' type with specified parameters and an explicit
% state, with no console echo (message just recorded in the traces).
%
-define( send_critical_no_echo_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_synchronised( critical, State,
						text_utils:format( Message, FormatValues ) )
).



% Sends a trace of 'critical' type (echoed on the console) with specified
% parameters and implicit use of a variable named 'State'.
%
-define( critical_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( critical, State,
								text_utils:format( Message, FormatValues ) )
).


% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State', with no console echo (message just recorded in the
% traces).
%
-define( critical_no_echo_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_synchronised( critical, State,
							text_utils:format( Message, FormatValues ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'critical' type with specified parameters and an explicit
% state.
%
-define( send_critical_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send_safe( critical, State,
		   text_utils:format( Message, FormatValues ), MessageCategorization )
).



% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( critical_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( critical, State,
		   text_utils:format( Message, FormatValues ), MessageCategorization )
).




% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'critical' type with specified parameters and an explicit
% state.
%
-define( send_critical_fmt_full( State, Message, FormatValues,
							  MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( critical, State,
				text_utils:format( Message, FormatValues ),
				MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( critical_fmt_full( Message, FormatValues, MessageCategorization,
						 ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( critical, State,
				text_utils:format( Message, FormatValues ),
				MessageCategorization, ApplicationTimestamp )
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Error, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error( State, Message ),
		 class_TraceEmitter:send_safe( error, State, Message )
).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error( Message ),
		 class_TraceEmitter:send_safe( error, State, Message )
).


% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State', with no console echo (message just recorded in the
% traces).
%
-define( error_no_echo_fmt( Message ),
		 class_TraceEmitter:send_synchronised( error, State, Message ) )
).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( error, State,
		   text_utils:format( Message, FormatValues ), MessageCategorization )
).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( error, State, Message,
									   MessageCategorization )
).




% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_full( State, Message, MessageCategorization,
						  ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( error, State, Message,
								MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_full( Message, MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( error, State, Message,
								MessageCategorization, ApplicationTimestamp )
).




% Subsection for Error, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type (echoed on the console) with specified
% parameters and an explicit state.
%
-define( send_error_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( error, State,
				   text_utils:format( Message, FormatValues ) )
).


% Sends a trace of 'error' type with specified parameters and an explicit
% state, with no console echo (message just recorded in the
% traces).
%
-define( send_error_no_echo_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_synchronised( error, State,
						  text_utils:format( Message, FormatValues ) )
).



% Sends a trace of 'error' type (echoed on the console) with specified
% parameters and implicit use of a variable named 'State'.
%
-define( error_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( error, State,
							text_utils:format( Message, FormatValues ) )
).


% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State', with no console echo (message just recorded in the
% traces).
%
-define( error_no_echo_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_synchronised( error, State,
							text_utils:format( Message, FormatValues ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send_safe( error, State,
						  text_utils:format( Message, FormatValues ),
						  MessageCategorization )
).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( error, State,
					text_utils:format( Message, FormatValues ),
					MessageCategorization )
).





% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.



% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_fmt_full( State, Message, FormatValues,
							  MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( error, State,
					text_utils:format( Message, FormatValues ),
					MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_fmt_full( Message, FormatValues, MessageCategorization,
						 ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( error, State,
					text_utils:format( Message, FormatValues ),
					MessageCategorization, ApplicationTimestamp )
).









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Warning section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Warning, without formatting.



% When not in debug mode (like here), one may or may not want warning messages
% to be echoed in the console (as are the messages with the error, critical,
% etc. severities). By default they are echoed (better safe than sorry).
%
% Swap the display_warning/{1,2} implementations as preferred (and recompile):


% Unused now:
%-define( display_warning( Message ),
%
%		 trace_utils:warning( Message ),
%
%		 % To ensure the asynchronous sending of the trace has a chance to
%		 % complete, possibly before the interpreter is crashed:
%		 %
%		 class_TraceEmitter:await_output_completion()
%
%).


% Unused now:
%-define( display_warning_fmt( Message, FormatValues ),
%
%		 trace_utils:warning_fmt( Message, FormatValues ),
%
%		 % To ensure the asynchronous sending of the trace has a chance to
%		 % complete, possibly before the interpreter is crashed:
%		 %
%		 class_TraceEmitter:await_output_completion()
%
%).



% Alternate (default) implementation:

%% -define( display_warning( Message ),
%%	ok
%% ).


%% -define( display_warning_fmt( Message, FormatValues ),
%%	ok
%% ).



% Plain, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning( State, Message ),
		 class_TraceEmitter:send_safe( warning, State, Message )
).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning( Message ),
		 class_TraceEmitter:send_safe( warning, State, Message )
).






% Categorized, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( warning, State, Message,
									   MessageCategorization )
).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( warning, State, Message,
									   MessageCategorization )
).




% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.



% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_full( State, Message, MessageCategorization,
							ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( warning, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_full( Message, MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( warning, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).








% Subsection for Warning, with formatting.



% Plain, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'warning' type (echoed on the console) with specified
% parameters and an explicit state.
%
-define( send_warning_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( warning, State,
						  text_utils:format( Message, FormatValues ) )
).


% Sends a trace of 'warning' type with specified parameters and an explicit
% state, with no console echo (message just recorded in the traces).
%
-define( send_warning_no_echo_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_synchronised( warning, State,
						  text_utils:format( Message, FormatValues ) )
).



% Sends a trace of 'warning' type (echoed on the console) with specified
% parameters and implicit use of a variable named 'State'.
%
-define( warning_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( warning, State,
						   text_utils:format( Message, FormatValues ) )
).


% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State', with no console echo (message just recorded in the
% traces).
%
-define( warning_no_echo_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_synchronised( warning, State,
							text_utils:format( Message, FormatValues ) )
).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_fmt_cat( State, Message, FormatValues,
							   MessageCategorization ),
		 class_TraceEmitter:send_safe( warning, State,
			text_utils:format( Message, FormatValues ), MessageCategorization )
).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( warning, State,
				text_utils:format( Message, FormatValues ),
				MessageCategorization )
).





% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_fmt_full( State, Message, FormatValues,
								MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( warning, State,
					text_utils:format( Message, FormatValues ),
					MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_fmt_full( Message, FormatValues, MessageCategorization,
						   ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( warning, State,
					text_utils:format( Message, FormatValues ),
					MessageCategorization, ApplicationTimestamp )
).









% See the ENABLE_TRACES make variable to enable/disable tracing:
-ifdef(tracing_activated).


% The type of trace output (ex: LogMX, PDF, etc.) is defined in the traces.hrl
% file.


% The first version of macros uses an explicit state.
%
% The second version of macros uses an implicit state, named 'State', as, except
% in constructors, WOOPER conventions imply such a state exists and, provided
% informations stored in state have not changed (notably emitter name and
% categorization), the initial state declared in a method can be used instead of
% any newer one.



% Selecting a macro implies selecting a level of severity.

% An issue is that the definition of Erlang macros does not take into account
% arities, thus LOG(X) and LOG(X,Y) cannot be defined without a name clash.
%
% This explains why the trace informations have to be specified between brakets:
% using LOG([X,Y]) instead of LOG(X,Y).
%
% Anonymous functions could be used as well.
%
% See http://www.nabble.com/question%3A-macro-definition-tt14840873.html

% For each severity of trace (emergency, alert, critical, error, warning, etc.),
% a few variations of the set of specified trace informations are supported.




% Taking 'notice' as an example:
% - '?notice( "Hello" )'
% - '?notice_cat( "Hello", "My Category" )' ('cat' stands for 'categorized')
% - '?notice_full( "Hello", "My Category", 125 )'
%
% The use of text_utils:format/2 involved too much typing, so we defined shorter
% forms instead. Taking 'notice' again as an example:
% - '?notice_fmt( "Hello ~w.", [V] )' (most frequently used form; 'fmt' stands
% for 'format')
% - '?notice_fmt_cat( "Hello ~w.", [V], "My Category" )'
% - '?notice_fmt_full( "Hello ~w.", [V], "My Category", 125 )'
%
% (knowing we cannot define macros with a same name but a different arity)



% Some delay were added when error-like traces are sent, so that they can be
% stored before the virtual machine is stopped, should it happen (ex: if an
% exception is thrown); they have been since then replaced by synchronous
% operations.


% If traces are enabled, only error-like ones will be echoed in the terminal,
% whereas, if the traces are disabled, warning ones will be echoed too.


% No variable is to be bound in these macros, otherwise sending more than one
% trace from the same scope would lead to 'badmatch' errors.


% For the most severe traces, we still use class_TraceEmitter:sync/1 rather than
% the once used by trace_utils, as the former is surely strictly synchronous,
% whereas the latter probably not.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Notice section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Notice, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
%
-define( send_notice( State, Message ),
		 class_TraceEmitter:send( notice, State, Message )
).



% Sends a trace of 'notice' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( notice( Message ),
		 class_TraceEmitter:send( notice, State, Message )
).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
%
-define( send_notice_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send( notice, State, Message,
								  MessageCategorization )
).



% Sends a trace of 'notice' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( notice_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send( notice, State, Message,
								  MessageCategorization )
).





% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
%
-define( send_notice_full( State, Message, MessageCategorization,
						   ApplicationTimestamp ),
		 class_TraceEmitter:send( notice, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'notice' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( notice_full( Message, MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send( notice, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).






% Subsection for Notice, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
%
-define( send_notice_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send( notice, State,
								  text_utils:format( Message, FormatValues ) )
).



% Sends a trace of 'notice' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( notice_fmt( Message, FormatValues ),
		 class_TraceEmitter:send( notice, State,
								  text_utils:format( Message, FormatValues ) )
).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
%
-define( send_notice_fmt_cat( State, Message, FormatValues,
							MessageCategorization ),
		 class_TraceEmitter:send( notice, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization )
).


% Sends a trace of 'notice' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( notice_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send( notice, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization )
).






% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
%
-define( send_notice_fmt_full( State, Message, FormatValues,
							   MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send( notice, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'notice' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( notice_fmt_full( Message, FormatValues, MessageCategorization,
						  ApplicationTimestamp ),
		 class_TraceEmitter:send( notice, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization, ApplicationTimestamp )
 ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Info, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
%
-define( send_info( State, Message ),
		 class_TraceEmitter:send( info, State, Message )
).



% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( info( Message ),
		 class_TraceEmitter:send( info, State, Message )
).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
%
-define( send_info_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send( info, State, Message,
								  MessageCategorization )
).



% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( info_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send( info, State, Message,
								  MessageCategorization )
).





% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
%
-define( send_info_full( State, Message, MessageCategorization,
						 ApplicationTimestamp ),
		 class_TraceEmitter:send( info, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( info_full( Message, MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send( info, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).






% Subsection for Info, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit state.
%
-define( send_info_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send( info, State,
								  text_utils:format( Message, FormatValues ) )
).



% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info_fmt( Message, FormatValues ),
		 class_TraceEmitter:send( info, State,
								  text_utils:format( Message, FormatValues ) )
).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit state.
%
-define( send_info_fmt_cat( State, Message, FormatValues,
							MessageCategorization ),
		 class_TraceEmitter:send( info, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization )
).


% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send( info, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization )
).






% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit state.
%
-define( send_info_fmt_full( State, Message, FormatValues,
							 MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send( info, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization, ApplicationTimestamp )
).



% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info_fmt_full( Message, FormatValues, MessageCategorization,
						ApplicationTimestamp ),
		 class_TraceEmitter:send( info, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization, ApplicationTimestamp )
 ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Debug, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a message of 'debug' type with specified parameters and an explicit
% state.
%
-define( send_debug( State, Message ),
		 class_TraceEmitter:send( debug, State, Message )
).



% Sends a message of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( debug( Message ),
		 class_TraceEmitter:send( debug, State, Message )
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a message of 'debug' type with specified parameters and an explicit
% state.
%
-define( send_debug_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send( debug, State, Message,
								  MessageCategorization )
).



% Sends a message of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( debug_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send( debug, State, Message,
								  MessageCategorization )
).






% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a message of 'debug' type with specified parameters and an explicit
% state.
%
-define( send_debug_full( State, Message, MessageCategorization,
						  ApplicationTimestamp ),
		 class_TraceEmitter:send( debug, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).



% Sends a message of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( debug_full( Message, MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send( debug, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).





% Subsection for Debug, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a message of 'debug' type with specified parameters and an explicit
% state.
%
-define( send_debug_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send( debug, State,
								  text_utils:format( Message, FormatValues ) )
).



% Sends a message of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( debug_fmt( Message, FormatValues ),
		 class_TraceEmitter:send( debug, State,
								  text_utils:format( Message, FormatValues ) )
).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a message of 'debug' type with specified parameters and an explicit
% state.
%
-define( send_debug_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send( debug, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization )
).


% Sends a message of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( debug_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send( debug, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization )
).





% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a message of 'debug' type with specified parameters and an explicit
% state.
%
-define( send_debug_fmt_full( State, Message, FormatValues,
							  MessageCategorization, ApplicationTimestamp ),
		 class_TraceEmitter:send( debug, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization, ApplicationTimestamp )
).



% Sends a message of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( debug_fmt_full( Message, FormatValues, MessageCategorization,
						 ApplicationTimestamp ),
		 class_TraceEmitter:send( debug, State,
								  text_utils:format( Message, FormatValues ),
								  MessageCategorization, ApplicationTimestamp )
 ).



% Void section placed near the end of this file, in a later section, as does not
% depend on whether the traces are activated (will be muted in all cases).











-else. % tracing_activated not defined below:




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traces are disabled here.
% This 'else' branch will be used iff tracing_activated is not defined above.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% trace_disabled functions are defined to avoid warnings about variables not
% being used.

% Hopefully they will be inlined, and then optimized out as a whole by the
% compiler (hopefully).

% If not using the trace_disabled functions, deactivating the traces will result
% in variables in classes possibly being declared unused (warning).
%
% Using these functions will cause another problem: if not all macro arities are
% used in that class, the remaining trace_disabled functions will be declared
% themselves as unused.
%
% Exporting them is not a solution, as WOOPER defined already some functions,
% thus no additional exports can be made. And the trace emitter include and the
% WOOPER one cannot be permuted (as this header defines functions as well).
%
% Specifying the parameters 'as are' instead of wrapping them in a
% trace_disabled function (ex: 'State, Message' instead of
% 'trace_disabled(State,Message)' results in the following warning:
% 'Warning: a term is constructed, but never used'



% Used to be exported (otherwise will be themselves determined 'unused'),
% however as explained above could not mix with WOOPER exports, triggering
% "attribute 'export' after function definitions":
%
%-export([ trace_disabled/1, trace_disabled/2, trace_disabled/3,
% trace_disabled/4 ])

% As a result, Dialyzer will complain that 'Function trace_disabled/{1,2,3,4,5}
% will never be called', but it is still the best approach.

% Forced inlining so that trace_disabled functions are optimized out.
%
% It was finally commented out, as it triggered for each trace macro:
% "Warning: a term is constructed, but never used" when traces were deactivated.
% We believe that nonetheless these local do-nothing functions will be optimized
% out by the compiler.
%-compile( {inline,[ trace_disabled/1, trace_disabled/2, trace_disabled/3,
%					 trace_disabled/4, trace_disabled/5 ] } ).

% Final solution: replacing calls to trace_disabled/[1..5] by tuples containing
% these variables (ex: 'trace_disabled( State, Message )' -> { State, Message })
% (and not defining these trace_disabled functions)


% We also use trace_disabled/1 on any parameter Foobar unused by the macro,
% otherwise the compiler would report that the "variable 'Foobar' is unused".

% We do the same also whenever the 'State' variable is to be implicitly used
% (ex: with '?error("Hello")'), otherwise user code would also have it
% reported as unused.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Notice section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Notice, without formatting.


% Most important trace categories cannot be disabled:

-define( send_notice( State, Message ),
		 trace_disabled( State, Message )
).


-define( notice( Message ),
		 trace_disabled( State, Message )
).





-define( send_notice_cat( State, Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).


-define( notice_cat( Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).



-define( send_notice_full( State, Message, MessageCategorization,
						   ApplicationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 ApplicationTimestamp )
).


-define( notice_full( Message, MessageCategorization, ApplicationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 ApplicationTimestamp )
).




% Subsection for Notice, with formatting.


-define( send_notice_fmt( State, Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).


-define( notice_fmt( Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).





-define( send_notice_fmt_cat( State, Message, FormatValues,
							  MessageCategorization ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization )
).


-define( notice_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization )
).






-define( send_notice_fmt_full( State, Message, FormatValues,
							   MessageCategorization, ApplicationTimestamp ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization, ApplicationTimestamp )
).


-define( notice_fmt_full( Message, FormatValues, MessageCategorization,
						  ApplicationTimestamp ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization,
						 ApplicationTimestamp )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Info, without formatting.


% Most important trace categories cannot be disabled:

-define( send_info( State, Message ),
		 trace_disabled( State, Message )
).


-define( info( Message ),
		 trace_disabled( State, Message )
).




-define( send_info_cat( State, Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).


-define( info_cat( Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).



-define( send_info_full( State, Message, MessageCategorization,
						 ApplicationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 ApplicationTimestamp )
).


-define( info_full( Message, MessageCategorization, ApplicationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 ApplicationTimestamp )
).




% Subsection for Info, with formatting.


-define( send_info_fmt( State, Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).


-define( info_fmt( Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).



-define( send_info_fmt_cat( State, Message, FormatValues,
							MessageCategorization ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization )
).


-define( info_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization )
).






-define( send_info_fmt_full( State, Message, FormatValues,
							 MessageCategorization, ApplicationTimestamp ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization, ApplicationTimestamp )
).


-define( info_fmt_full( Message, FormatValues, MessageCategorization,
						ApplicationTimestamp ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization,
						 ApplicationTimestamp )
).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Debug, without formatting.


% Most important debug categories cannot be disabled:

-define( send_debug( State, Message ),
		 trace_disabled( State, Message )
).


-define( debug( Message ),
		 trace_disabled( State, Message )
).






-define( send_debug_cat( State, Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).


-define( debug_cat( Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).





-define( send_debug_full( State, Message, MessageCategorization,
						  ApplicationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 ApplicationTimestamp )
).


-define( debug_full( Message, MessageCategorization, ApplicationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 ApplicationTimestamp )
).








% Subsection for Debug, with formatting.


-define( send_debug_fmt( State, Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).


-define( debug_fmt( Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).





-define( send_debug_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization )
).


-define( debug_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization )
).






-define( send_debug_fmt_full( State, Message, FormatValues,
							  MessageCategorization, ApplicationTimestamp ),
		 trace_disabled( State, Message, FormatValues,
							  MessageCategorization, ApplicationTimestamp )
).


-define( debug_fmt_full( Message, FormatValues, MessageCategorization,
						 ApplicationTimestamp ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization,
						 ApplicationTimestamp )
).



-endif. % tracing_activated


% End of the tracing_activated branch.



% trace_disabled functions are defined to avoid warnings about variables not
% being used.

% Hopefully they will be inlined, and then optimized out as a whole by the
% compiler.

% If not using the trace_disabled functions, deactivating the traces will result
% in variables in classes possibly being declared unused (warning).
%
% Using these functions will cause another problem: if not all macro arities are
% used in that class, the remaining trace_disabled functions will be declared
% themselves as unused.
%
% Exporting them is not a solution, as WOOPER defined already some functions,
% thus no additional exports can be made. And the trace emitter include and the
% WOOPER one cannot be permuted (as this header defines functions as well).
%
% Specifying the parameters 'as are' instead of wrapping them in a
% trace_disabled function (ex: 'State, Message' instead of
% 'trace_disabled(State,Message)' results in the following warning:
% 'Warning: a term is constructed, but never used'
%
% Hence: unique, best solution seems to declare and define them in all cases,
% and to disable any report of them being unused.



% Unable to declare them as command-line options anyway (not taken into account
% apparently, see ERLANG_COMPILER_OPT in GNUmakevars.inc):
%
% (these functions cannot be omittted or exported)
%
-compile( [ { nowarn_unused_function, [ { trace_disabled, 1 },
										{ trace_disabled, 2 },
										{ trace_disabled, 3 },
										{ trace_disabled, 4 },
										{ trace_disabled, 5 } ] } ] ).


% We used to attempt a forced inlining of these pseudo-functions so that they
% can be fully optimized out.
%
% It was finally commented out, as it triggered for each trace macro:
% "Warning: a term is constructed, but never used" when traces were deactivated.
%
% We believe that nonetheless these local do-nothing functions will be optimized
% out by the compiler.
%
%-compile( {inline,[ trace_disabled/1, trace_disabled/2, trace_disabled/3,
%					 trace_disabled/4, trace_disabled/5 ] } ).



% Defined here once for all (warning about their being unused suppressed),
% regardless of the tracing_activated flag as always needed for void_* traces:

-spec trace_disabled( any() ) -> void().
trace_disabled( _ ) ->
	ok.

-spec trace_disabled( any(), any() ) -> void().
trace_disabled( _, _ ) ->
	ok.

-spec trace_disabled( any(), any(), any() ) -> void().
trace_disabled( _, _, _ ) ->
	ok.

-spec trace_disabled( any(), any(), any(), any() ) -> void().
trace_disabled( _, _, _, _ ) ->
	ok.

-spec trace_disabled( any(), any(), any(), any(), any() ) -> void().
trace_disabled( _, _, _, _, _ ) ->
	ok.



%
% Section for non-maskable traces.
%



% These tracing primitives are always activated, regardless of the
% tracing_activated setting.
%
% They are sent to the 'info' channel, and are also echoed on the console.

% These report* primitives are the emitter-level counterparts of the standalone
% notify* primitives.



% Plain version, with implicit state.


% Sends a report with specified parameters and implicit use of a variable named
% 'State'.
%
-define( report( Message ),
		 class_TraceEmitter:send_safe( info, State, Message )
).



% Sends a report with specified parameters and implicit use of a variable named
% 'State'.
%
-define( report_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( info, State,
						text_utils:format( Message, FormatValues ) )
).




% Categorized version, with implicit state.


% Sends a report with specified categorization and implicit use of a variable
% named 'State'.
%
-define( report_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( info, State, Message,
									   MessageCategorization )
).



% Sends a report with specified categorization and implicit use of a variable
% named 'State'.
%
-define( report_cat_fmt( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( info, State,
			text_utils:format( Message, FormatValues ),
			MessageCategorization )
).




% Categorized version with application timestamp, with implicit state.



% Sends a report with specified categorization and application timestamp, and
% implicit use of a variable named 'State'.
%
-define( report_full( State, Message, MessageCategorization,
					  ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( info, State, Message,
								  MessageCategorization, ApplicationTimestamp )
).



% Sends a report with specified categorization and application timestamp, and
% implicit use of a variable named 'State'.
%
-define( report_full_fmt( State, Message, FormatValues, MessageCategorization,
						  ApplicationTimestamp ),
		 class_TraceEmitter:send_safe( info, State,
				text_utils:format( Message, FormatValues ),
				MessageCategorization, ApplicationTimestamp )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Void section (muted traces).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Void traces, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'void' type with specified parameters and an explicit state.
%
-define( send_void( State, Message ),
		 trace_disabled( State, Message )
).



% Sends a trace of 'void' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( void( Message ),
		 trace_disabled( State, Message )
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'void' type with specified parameters and an explicit state.
%
-define( send_void_cat( State, Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).



% Sends a trace of 'void' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( void_cat( Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).






% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'void' type with specified parameters and an explicit state.
%
-define( send_void_full( State, Message, MessageCategorization,
						 ApplicationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 ApplicationTimestamp )
).



% Sends a trace of 'void' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( void_full( Message, MessageCategorization, ApplicationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 ApplicationTimestamp )
).





% Subsection for Void traces, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a message of 'void' type with specified parameters and an explicit
% state.
%
-define( send_void_fmt( State, Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).



% Sends a message of 'void' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( void_fmt( Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a message of 'void' type with specified parameters and an explicit
% state.
%
-define( send_void_fmt_cat( State, Message, FormatValues,
							MessageCategorization ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization )
).


% Sends a message of 'void' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( void_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization )
).





% Categorized with application timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a message of 'void' type with specified parameters and an explicit
% state.
%
-define( send_void_fmt_full( State, Message, FormatValues,
							 MessageCategorization, ApplicationTimestamp ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization, ApplicationTimestamp )
).



% Sends a message of 'void' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( void_fmt_full( Message, FormatValues, MessageCategorization,
						ApplicationTimestamp ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization,
						 ApplicationTimestamp )
 ).



-endif. % class_TraceEmitter_hrl_guard
