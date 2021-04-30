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


% Include guard:
%
-ifndef(traces_hrl_guard).
-define(traces_hrl_guard,).



% Extension to be used for trace file names:
-define( TraceExtension, ".traces" ).


% Per-test trace file (must be defined before the TraceSupervisor include):
%
-define( TraceFilename, ( atom_to_list( ?MODULE ) ++ ?TraceExtension ) ).



% Defines the type of requested execution traces.

% The trace type can be either:
%
% - advanced_traces, for LogMX-compliant traces (the default): then the trace
% aggregator will use a proper encoding so that the Ceylan Java trace parser,
% plugged into LogMX, allows this tool to be used with Ceylan
%
% - {text_traces,TraceTextOutputType} for more basic text-based traces: then the
% trace aggregator will do its best to format the traces as a human-readable
% trace text file; this is mostly useful when LogMX cannot be used for any
% reason, like needing to generate a report; TraceTextOutputType can be:
%
%   * 'text_only', if wanting to have traces be directly written to disk as pure
%   yet human-readable text
%
%   * 'pdf', if wanting to read finally the traces in a generated PDF file
%
% Note:
%
% - if you change (ex: comment/uncomment) the trace type, then you must
% recompile your modules to take it into account
%
% - check in the class_TraceEmitter.hrl file whether tracing_activated is
% defined
%
-ifndef(TraceType).
	-define( TraceType, advanced_traces ).
	%-define( TraceType, { text_traces, pdf } ).
	%-define( TraceType, { text_traces, text_only } ).

-endif. % TraceType



% Defines the trace title (ex: for PDF output), if not already specified:
-ifndef(TraceTitle).
	-define( TraceTitle, "Ceylan" ).
-endif. % TraceTitle


% For supervisor macros (ex: init_trace_supervisor):
-include("class_TraceSupervisor.hrl").


% For tracing_activated:
-include("class_TraceEmitter.hrl").


% Defines some macros to emit standalone traces, i.e. not from a TraceEmitter,
% and not for test purpose (ex: when writing classical, non-OOP, code).
%
% Note: using 'notify' instead of 'send' to prevent name clashes.

% Usage: '?notify_debug( "Starting!" )'


% We moved away from the tracing_activated conditional sections the most severe
% trace sendings (namely emergency, alert, critical, error and warning), as in
% all cases (whether or not the traces are activated), we want them, and both as
% actual traces and as console outputs, as they should not be missed.


% Note: when no emitter categorization is specified, 'atom_to_list( ?MODULE )'
% might be used as a default.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Emergency section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Emergency, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_emergency( Message ),
		 class_TraceEmitter:send_standalone_safe( emergency, Message )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_emergency_cat( Message, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( emergency, Message,
												  EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: from
% a static method):
%
% (last parameter: MessageCategorization)
%
-define( notify_emergency_named( Message, EmitterName, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( emergency, Message, EmitterName,
				EmitterCategorization, uncategorized )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method). ApplicationTimestamp corresponds to any measure of time
% according to the application.
%
-define( notify_emergency_full( Message, EmitterCategorization,
							ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone_safe( emergency, Message,
								EmitterCategorization, ApplicationTimestamp )
).




% Subsection for Emergency, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_emergency_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( emergency,
							  text_utils:format( Message, FormatValues ) )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_emergency_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( emergency,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_emergency_fmt_full( Message, FormatValues, EmitterCategorization,
								ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone_safe( emergency,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization, ApplicationTimestamp )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Alert section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Alert, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_alert( Message ),
		 class_TraceEmitter:send_standalone_safe( alert, Message )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_alert_cat( Message, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( alert, Message,
												  EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: from
% a static method):
%
% (last parameter: MessageCategorization)
%
-define( notify_alert_named( Message, EmitterName, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( alert, Message, EmitterName,
				EmitterCategorization, uncategorized )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method). ApplicationTimestamp corresponds to any measure of time
% according to the application.
%
-define( notify_alert_full( Message, EmitterCategorization,
							ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone_safe( alert, Message,
								EmitterCategorization, ApplicationTimestamp )
).




% Subsection for Alert, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_alert_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( alert,
							  text_utils:format( Message, FormatValues ) )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_alert_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( alert,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_alert_fmt_full( Message, FormatValues, EmitterCategorization,
								ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone_safe( alert,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization, ApplicationTimestamp )
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Critical section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Critical, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_critical( Message ),
		 class_TraceEmitter:send_standalone_safe( critical, Message )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_critical_cat( Message, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( critical, Message,
												  EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: from
% a static method):
%
% (last parameter: MessageCategorization)
%
-define( notify_critical_named( Message, EmitterName, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( critical, Message, EmitterName,
				EmitterCategorization, uncategorized )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method). ApplicationTimestamp corresponds to any measure of time
% according to the application.
%
-define( notify_critical_full( Message, EmitterCategorization,
							ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone_safe( critical, Message,
								EmitterCategorization, ApplicationTimestamp )
).




% Subsection for Critical, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_critical_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( critical,
							  text_utils:format( Message, FormatValues ) )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_critical_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( critical,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_critical_fmt_full( Message, FormatValues, EmitterCategorization,
								ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone_safe( critical,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization, ApplicationTimestamp )
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Error, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error( Message ),
		 class_TraceEmitter:send_standalone_safe( error, Message )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_cat( Message, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( error, Message,
												  EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: from
% a static method):
%
% (last parameter: MessageCategorization)
%
-define( notify_error_named( Message, EmitterName, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( error, Message, EmitterName,
				EmitterCategorization, uncategorized )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_full( Message, EmitterCategorization,
							ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone_safe( error, Message,
							EmitterCategorization, ApplicationTimestamp )
).




% Subsection for Error, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( error,
							  text_utils:format( Message, FormatValues ) )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( error,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_fmt_full( Message, FormatValues, EmitterCategorization,
								ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone_safe( error,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization, ApplicationTimestamp )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Warning section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Warning, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning( Message ),
		 class_TraceEmitter:send_standalone_safe( warning, Message )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_cat( Message, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( warning, Message,
												  EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: from
% a static method):
%
% (last parameter: MessageCategorization)
%
-define( notify_warning_named( Message, EmitterName, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( warning, Message, EmitterName,
				EmitterCategorization, uncategorized )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_full( Message, EmitterCategorization,
							  ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone_safe( warning, Message,
								 EmitterCategorization, ApplicationTimestamp )
).





% Subsection for Warning, with formatting.



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( warning,
							  text_utils:format( Message, FormatValues ) )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( warning,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_fmt_full( Message, FormatValues, EmitterCategorization,
								  ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone_safe( warning,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization, ApplicationTimestamp )
).








-ifdef(tracing_activated).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Notice section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Notice, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_notice( Message ),
		 class_TraceEmitter:send_standalone( notice, Message )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_notice_cat( Message, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( notice, Message,
											 EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_notice_em( Message, EmitterName, EmitterCategorization,
						 MessageCategorization ),
		 class_TraceEmitter:send_standalone( notice, Message, EmitterName,
							 EmitterCategorization, MessageCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: from
% a static method):
%
% (last parameter: MessageCategorization)
%
-define( notify_notice_named( Message, EmitterName, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( notice, Message, EmitterName,
				EmitterCategorization, uncategorized )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_notice_full( Message, EmitterCategorization,
						   ApplicationTimestamp ),
		class_TraceEmitter:send_standalone( notice, Message,
								EmitterCategorization, ApplicationTimestamp )
).





% Subsection for Notice, with formatting.



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_notice_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone( notice,
							  text_utils:format( Message, FormatValues ) )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_notice_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( notice,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization )
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_notice_fmt_em( Message, FormatValues, EmitterName,
							 EmitterCategorization, MessageCategorization ),
		 class_TraceEmitter:send_standalone( notice,
					  text_utils:format( Message, FormatValues ), EmitterName,
					  EmitterCategorization, MessageCategorization )
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_notice_fmt_full( Message, FormatValues, EmitterCategorization,
							   ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone( notice,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization, ApplicationTimestamp )
).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trace section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Info, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info( Message ),
		 class_TraceEmitter:send_standalone( info, Message )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_cat( Message, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( info, Message,
											 EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_em( Message, EmitterName, EmitterCategorization,
						 MessageCategorization ),
		 class_TraceEmitter:send_standalone( info, Message, EmitterName,
							 EmitterCategorization, MessageCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: from
% a static method):
%
% (last parameter: MessageCategorization)
%
-define( notify_info_named( Message, EmitterName, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( info, Message, EmitterName,
				EmitterCategorization, uncategorized )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_full( Message, EmitterCategorization,
						   ApplicationTimestamp ),
		class_TraceEmitter:send_standalone( info, Message,
								EmitterCategorization, ApplicationTimestamp )
).





% Subsection for Info, with formatting.



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone( info,
							  text_utils:format( Message, FormatValues ) )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( info,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization )
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_fmt_em( Message, FormatValues, EmitterName,
							 EmitterCategorization, MessageCategorization ),
		 class_TraceEmitter:send_standalone( info,
					  text_utils:format( Message, FormatValues ), EmitterName,
					  EmitterCategorization, MessageCategorization )
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_fmt_full( Message, FormatValues, EmitterCategorization,
							   ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone( info,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization, ApplicationTimestamp )
).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Debug, without formatting.


% To send debug traces neither from a TraceEmitter instance nor from a test (ex:
% from a static method):
%
-define( notify_debug( Message ),
		 class_TraceEmitter:send_standalone( debug, Message )
).




% To send debug traces neither from a TraceEmitter instance nor from a test (ex:
% from a static method):
%
-define( notify_debug_cat( Message, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( debug, Message,
											 EmitterCategorization )
).



% To send debug traces neither from a TraceEmitter instance nor from a test (ex:
% from a static method):
%
% (last parameter: MessageCategorization)
%
-define( notify_debug_named( Message, EmitterName, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( debug, Message, EmitterName,
				EmitterCategorization, uncategorized )
).



% To send debug traces neither from a TraceEmitter instance nor from a test (ex:
% from a static method):
%
-define( notify_debug_full( Message, EmitterCategorization,
							ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone( debug, Message,
								EmitterCategorization, ApplicationTimestamp )
).



% Subsection for Debug, with formatting.

% To send debug traces neither from a TraceEmitter instance nor from a test (ex:
% from a static method):
%
-define( notify_debug_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone( debug,
							  text_utils:format( Message, FormatValues ) )
).


% To send debug traces neither from a TraceEmitter instance nor from a test (ex:
% from a static method):
%
-define( notify_debug_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( debug,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization )
).


% To send debug traces neither from a TraceEmitter instance nor from a test (ex:
% from a static method):
%
-define( notify_debug_fmt_full( Message, FormatValues, EmitterCategorization,
								ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone( debug,
							  text_utils:format( Message, FormatValues ),
							  EmitterCategorization, ApplicationTimestamp )
).


% 'void' section muted in all cases, thus placed near the end of this file.











-else. % tracing_activated


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traces are disabled here (non-critical ones).
% This 'else' branch will be used iff tracing_activated is not defined above.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Notice section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_notice( Message ),
		 trace_disabled( Message )
).


-define( notify_notice_cat( Message, EmitterCategorization ),
		 trace_disabled( Message, EmitterCategorization )
).


-define( notify_notice_em( Message, EmitterName, EmitterCategorization,
						 MessageCategorization ),
		 trace_disabled( Message, EmitterName, EmitterCategorization,
						 MessageCategorization )
).


-define( notify_notice_named( Message, EmitterName, EmitterCategorization ),
		 trace_disabled( Message, EmitterName, EmitterCategorization )
).


-define( notify_notice_full( Message, EmitterCategorization,
						   ApplicationTimestamp ),
		 trace_disabled( Message, EmitterCategorization,
						   ApplicationTimestamp )
).



-define( notify_notice_fmt( Message, FormatValues ),
		 trace_disabled( Message, FormatValues )
).


-define( notify_notice_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 trace_disabled( Message, FormatValues, EmitterCategorization )
).


-define( notify_notice_fmt_full( Message, FormatValues, EmitterCategorization,
							   ApplicationTimestamp ),
		 trace_disabled( Message, FormatValues, EmitterCategorization,
						 ApplicationTimestamp )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define( notify_info( Message ),
		 trace_disabled( Message )
).


-define( notify_info_cat( Message, EmitterCategorization ),
		 trace_disabled( Message, EmitterCategorization )
).


-define( notify_info_em( Message, EmitterName, EmitterCategorization,
						 MessageCategorization ),
		 trace_disabled( Message, EmitterName, EmitterCategorization,
						 MessageCategorization )
).


-define( notify_info_named( Message, EmitterName, EmitterCategorization ),
		 trace_disabled( Message, EmitterName, EmitterCategorization )
).


-define( notify_info_full( Message, EmitterCategorization,
						   ApplicationTimestamp ),
		 trace_disabled( Message, EmitterCategorization,
						   ApplicationTimestamp )
).



-define( notify_info_fmt( Message, FormatValues ),
		 trace_disabled( Message, FormatValues )
).


-define( notify_info_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 trace_disabled( Message, FormatValues, EmitterCategorization )
).


-define( notify_info_fmt_full( Message, FormatValues, EmitterCategorization,
							   ApplicationTimestamp ),
		 trace_disabled( Message, FormatValues, EmitterCategorization,
						 ApplicationTimestamp )
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_debug( Message ),
		 trace_disabled( Message )
).


-define( notify_debug_cat( Message, EmitterCategorization ),
		 trace_disabled( Message, EmitterCategorization )
).


-define( notify_debug_named( Message, EmitterName, EmitterCategorization ),
		 trace_disabled( Message, EmitterName, EmitterCategorization )
).


-define( notify_debug_full( Message, EmitterCategorization,
							ApplicationTimestamp ),
		 trace_disabled( Message, EmitterCategorization,
						 ApplicationTimestamp )
).



-define( notify_debug_fmt( Message, FormatValues ),
		 trace_disabled( Message, FormatValues )
).


-define( notify_debug_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 trace_disabled( Message, FormatValues, EmitterCategorization )
).


-define( notify_debug_fmt_full( Message, FormatValues, EmitterCategorization,
								ApplicationTimestamp ),
		 trace_disabled( Message, FormatValues, EmitterCategorization,
						 ApplicationTimestamp )
).


-endif. % here no tracing_activated


% End of the tracing_activated branch.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Void section, for traces that are muted in all cases.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_void( Message ),
		 trace_disabled( Message )
).


-define( notify_void_cat( Message, EmitterCategorization ),
		 trace_disabled( Message, EmitterCategorization )
).


-define( notify_void_full( Message, EmitterCategorization,
						   ApplicationTimestamp ),
		 trace_disabled( Message, EmitterCategorization,
						 ApplicationTimestamp )
).



-define( notify_void_fmt( Message, FormatValues ),
		 trace_disabled( Message, FormatValues )
).


-define( notify_void_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 trace_disabled( Message, FormatValues, EmitterCategorization )
).


-define( notify_void_fmt_full( Message, FormatValues, EmitterCategorization,
							   ApplicationTimestamp ),
		 trace_disabled( Message, FormatValues, EmitterCategorization,
						 ApplicationTimestamp )
).






% Section for non-maskable traces.
%
% These tracing primitives are always activated, regardless of the
% tracing_activated setting.
%
% They are sent to the 'info' channel, and are also echoed on the console.
%
% These notify* primitives are the standalone counterparts of the
% class_TraceEmitter-level report* primitives.



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify( Message ),
		 class_TraceEmitter:send_standalone_safe( info, Message )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_cat( Message, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( info, Message,
												  EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_em( Message, EmitterName, EmitterCategorization,
					MessageCategorization ),
		 class_TraceEmitter:send_standalone_safe( info, Message, EmitterName,
							EmitterCategorization, MessageCategorization )
).




% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( info,
							text_utils:format( Message, FormatValues ) )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 class_TraceEmitter:send_standalone_safe( info,
				text_utils:format( Message, FormatValues ),
				EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fmt_em( Message, FormatValues, EmitterName,
						EmitterCategorization, MessageCategorization ),
		 class_TraceEmitter:send_standalone_safe( info,
				text_utils:format( Message, FormatValues ),
				EmitterName, EmitterCategorization, MessageCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fmt_full( Message, FormatValues, EmitterCategorization,
						  ApplicationTimestamp ),
		 class_TraceEmitter:send_standalone_safe( info,
			text_utils:format( Message, FormatValues ),
			EmitterCategorization, ApplicationTimestamp )
).



-endif. % traces_hrl_guard
