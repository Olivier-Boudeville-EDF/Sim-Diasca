% Copyright (C) 2012-2024 EDF R&D
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
% Creation date: Tuesday, January 11, 2011.



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


% We moved away from the tracing_activated conditional sections the most severe
% trace sendings (namely emergency, alert, critical, error and warning), as in
% all cases (whether or not the traces are activated), we want them, and both as
% actual traces and as console outputs.

% All traces sent from a case are safe (i.e. explicitly synchronised), yet only
% error-like ones are echoed on the console.



-define( case_emergency( Message ),
		 class_TraceEmitter:send_standalone_safe( emergency, Message )
).


-define( case_emergency_fmt( MessageFormat, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( emergency,
					text_utils:format( MessageFormat, FormatValues ) )
).



-define( case_alert( Message ),
		 class_TraceEmitter:send_standalone_safe( alert, Message )
).


-define( case_alert_fmt( MessageFormat, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( alert,
					text_utils:format( MessageFormat, FormatValues ) )
).



-define( case_critical( Message ),
		 class_TraceEmitter:send_standalone_safe( critical, Message )
).


-define( case_critical_fmt( MessageFormat, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( critical,
					text_utils:format( MessageFormat, FormatValues ) )
).



-define( case_error( Message ),
		 class_TraceEmitter:send_standalone_safe( error, Message )
).


-define( case_error_fmt( MessageFormat, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( error,
					text_utils:format( MessageFormat, FormatValues ) )
).



-define( case_warning( Message ),
		 class_TraceEmitter:send_standalone_safe( warning, Message )
).


-define( case_warning_fmt( MessageFormat, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( warning,
					text_utils:format( MessageFormat, FormatValues ) )
).




-ifdef(tracing_activated).


-define( case_notice( Message ),
		 class_TraceEmitter:send_standalone_safe( notice, Message )
).


-define( case_notice_fmt( MessageFormat, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( notice,
						text_utils:format( MessageFormat, FormatValues ) )
).



-define( case_info( Message ),
		 class_TraceEmitter:send_standalone_safe( info, Message )

).


-define( case_info_fmt( MessageFormat, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( info,
						text_utils:format( MessageFormat, FormatValues ) )
).



-define( case_debug( Message ),
		 class_TraceEmitter:send_standalone_safe( debug, Message )
).


-define( case_debug_fmt( MessageFormat, FormatValues ),
		 class_TraceEmitter:send_standalone_safe( debug,
						text_utils:format( MessageFormat, FormatValues ) )
).


% 'void' section put near the end of this file.



-else. % tracing_activated



% Here tracing_activated is not defined: non-critical traces are disabled.


-define( case_notice( Message ), case_trace_disabled( Message ) ).


-define( case_notice_fmt( Message, FormatValues ),
		 case_trace_disabled( Message, FormatValues ) ).



-define( case_info( Message ), case_trace_disabled( Message ) ).


-define( case_info_fmt( Message, FormatValues ),
		 case_trace_disabled( Message, FormatValues ) ).



-define( case_debug( Message ), case_trace_disabled( Message ) ).


-define( case_debug_fmt( Message, FormatValues ),
		 case_trace_disabled( Message, FormatValues ) ).



-endif. % tracing_activated
