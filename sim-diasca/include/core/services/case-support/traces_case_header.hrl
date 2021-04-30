% Copyright (C) 2012-2021 EDF R&D
%
% This file is part of Sim-Diasca.
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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
%
% Creation date: Tuesday, January 11, 2011



% Defines some macros and functions useful for trace-using cases.

% Note: directly obtained from traces_test_header.hrl.


-ifndef(trace_emitter_categorization).

 -define( trace_emitter_categorization, "case" ).

-endif. % trace_emitter_categorization



% Allows to define exports before functions:
-ifndef(tracing_activated).

 -export([ case_trace_disabled/1, case_trace_disabled/2 ]).

-endif. % tracing_activated




% Section for trace output macros.


-ifdef(tracing_activated).



-define( case_emergency( Message ),

		 io:format( "Emergency case trace message: ~ts~n", [ Message ] ),

		 class_TraceEmitter:send_from_case( emergency, Message ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_emergency_fmt( MessageFormat, FormatValues ),

		 io:format( "Emergency trace message: " ++ MessageFormat ++ "~n",
					FormatValues ),

		 class_TraceEmitter:send_from_case( emergency,
							io_lib:format( MessageFormat, FormatValues ) ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).



-define( case_alert( Message ),

		 io:format( "Alert case trace message: ~ts~n", [ Message ] ),

		 class_TraceEmitter:send_from_case( alert, Message ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_alert_fmt( MessageFormat, FormatValues ),

		 io:format( "Alert trace message: " ++ MessageFormat ++ "~n",
					FormatValues ),

		 class_TraceEmitter:send_from_case( alert,
							io_lib:format( MessageFormat, FormatValues ) ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).



-define( case_critical( Message ),

		 io:format( "Critical case trace message: ~ts~n", [ Message ] ),

		 class_TraceEmitter:send_from_case( critical, Message ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_critical_fmt( MessageFormat, FormatValues ),

		 io:format( "Critical trace message: " ++ MessageFormat ++ "~n",
					FormatValues ),

		 class_TraceEmitter:send_from_case( critical,
							io_lib:format( MessageFormat, FormatValues ) ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).



-define( case_error( Message ),

		 io:format( "Error case trace message: ~ts~n", [ Message ] ),

		 class_TraceEmitter:send_from_case( error, Message ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_error_fmt( MessageFormat, FormatValues ),

		 io:format( "Error case trace message: " ++ MessageFormat ++ "~n",
					FormatValues ),

		 class_TraceEmitter:send_from_case( error,
						io_lib:format( MessageFormat, FormatValues ) ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).



-define( case_warning( Message ),

		 io:format( "Warning case trace message: ~ts~n", [ Message ] ),

		 class_TraceEmitter:send_from_case( warning, Message ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_warning_fmt( MessageFormat, FormatValues ),

		 io:format( "Warning case trace message: " ++ MessageFormat ++ "~n",
					FormatValues ),

		 class_TraceEmitter:send_from_case( warning,
						io_lib:format( MessageFormat, FormatValues ) ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).



-define( case_notice( Message ),
		 class_TraceEmitter:send_from_case( notice, Message )

).


-define( case_notice_fmt( MessageFormat, FormatValues ),
		 class_TraceEmitter:send_from_case( notice,
						io_lib:format( MessageFormat, FormatValues ) )

).



-define( case_info( Message ),
		 class_TraceEmitter:send_from_case( info, Message )

).


-define( case_info_fmt( MessageFormat, FormatValues ),
		 class_TraceEmitter:send_from_case( info,
						io_lib:format( MessageFormat, FormatValues ) )

).



-define( case_debug( Message ),
		 class_TraceEmitter:send_from_case( debug, Message )

).


-define( case_debug_fmt( MessageFormat, FormatValues ),
		 class_TraceEmitter:send_from_case( debug,
							io_lib:format( MessageFormat, FormatValues ) )

).




-else. % tracing_activated



% Here tracing_activated is not defined: non-critical traces are disabled.



% Message is returned, as otherwise some variables in calling code could be
% determined as unused, and thus would trigger a warning:



-define( case_emergency( Message ),

		 io:format( "Emergency case trace message: ~ts~n", [ Message ] ),

		 class_TraceEmitter:send_from_case( emergency, Message ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_alert( Message ),

		 io:format( "Alert case trace message: ~ts~n", [ Message ] ),

		 class_TraceEmitter:send_from_case( alert, Message ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_critical( Message ),

		 io:format( "Critical case trace message: ~ts~n", [ Message ] ),

		 class_TraceEmitter:send_from_case( critical, Message ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_error( Message ),

		 io:format( "Error case trace message: ~ts~n", [ Message ] ),

		 class_TraceEmitter:send_from_case( error, Message ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_warning( Message ),

		 io:format( "Warning case trace message: ~ts~n", [ Message ] ),

		 class_TraceEmitter:send_from_case( warning, Message ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_notice( Message ), case_trace_disabled( Message ) ).

-define( case_info( Message ), case_trace_disabled( Message ) ).

-define( case_debug( Message ), case_trace_disabled( Message ) ).




-define( case_emergency_fmt( MessageFormat, FormatValues ),

		 io:format( "Emergency case trace message: " ++ MessageFormat ++ "~n",
					FormatValues ),

		 class_TraceEmitter:send_from_case( emergency,
						io_lib:format( MessageFormat, FormatValues ) ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).



-define( case_alert_fmt( MessageFormat, FormatValues ),

		 io:format( "Alert case trace message: " ++ MessageFormat ++ "~n",
					FormatValues ),

		 class_TraceEmitter:send_from_case( alert,
						io_lib:format( MessageFormat, FormatValues ) ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).



-define( case_critical_fmt( MessageFormat, FormatValues ),

		 io:format( "Critical case trace message: " ++ MessageFormat ++ "~n",
					FormatValues ),

		 class_TraceEmitter:send_from_case( critical,
						io_lib:format( MessageFormat, FormatValues ) ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).



-define( case_error_fmt( MessageFormat, FormatValues ),

		 io:format( "Error case trace message: " ++ MessageFormat ++ "~n",
					FormatValues ),

		 class_TraceEmitter:send_from_case( error,
						io_lib:format( MessageFormat, FormatValues ) ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_warning_fmt( MessageFormat, FormatValues ),

		 io:format( "Warning case trace message: " ++ MessageFormat ++ "~n",
					FormatValues ),

		 class_TraceEmitter:send_from_case( warning,
						io_lib:format( MessageFormat, FormatValues ) ),

		 % To ensure the asynchronous output of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()
).


-define( case_notice_fmt( Message, FormatValues ),
		 case_trace_disabled( Message, FormatValues ) ).


-define( case_info_fmt( Message, FormatValues ),
		 case_trace_disabled( Message, FormatValues ) ).


-define( case_debug_fmt( Message, FormatValues ),
		 case_trace_disabled( Message, FormatValues ) ).


-endif. % tracing_activated
