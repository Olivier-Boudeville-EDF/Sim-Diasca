% Copyright (C) 2014-2021 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% This header file is a variation of the class_TraceEmitter.hrl header of the
% Traces layer, made to add spatial information to the traces sent by
% spatialised actors.


% Note: this header file must be included *after* the WOOPER one, as, in
% production mode, this file has to define functions
% (namely trace_disabled/{1,2,3,4,5}).



% This header centralizes notably all macros related to the sending of traces
% from a class_SpatialisedActor.

% These macros must operate on states that are spatialised, i.e. coming directly
% or not from the constructor of class_SpatialisedActor.


% See also class_TraceEmitter.hrl, for plain (non-spatial) actors.


-ifndef(class_SpatialTraceEmitter_hrl_guard).
-define(class_SpatialTraceEmitter_hrl_guard,).


% To avoid that Dialyzer thinks that it will never be called:
%
% (finally commented-out, as this export would have to come in most cases after
% function definitions)
%
% See the explanation about trace_disabled, in class_TraceEmitter.hrl, for more
% details (summary: we have to live with this Dialyzer complaint) .
%
% -export([ get_spatial_message/2 ]).



% Conventions section.



% Technical identifier will be the PID of the trace emitter.

% Name will be the name specified at the creation of the trace emitter.

% EmitterCategorization will the one specified at the creation of the trace
% emitter. It could be deduced from superclasses as well, although it would be
% generally uselessly long and would cause issues in case of multiple
% inheritance.


% Taken verbatim from class_TraceEmitter.hrl:
-define( trace_categorize( TracesInitialisationTermInternal ),

		 % As few variables bound, and longer variable names chosen (prefixed
		 % with 'Traces'), to avoid clashes with user-defined variables:
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


% We moved away from the tracing_activated conditional sections the most severe
% trace sendings (the error-like ones), as in all cases (whether or not the
% traces are activated), we want them, and both as actual traces and as console
% outputs.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Emergency section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Emergency, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'emergency' type with specified parameters and an explicit
% state.
%
-define( send_emergency( State, Message ),
		 class_TraceEmitter:send_safe( emergency, State,
						get_spatial_message( Message, State ) )
).



% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State'.
%
-define( emergency( Message ),
		 class_TraceEmitter:send_safe( emergency, State,
						get_spatial_message( Message, State ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'emergency' type with specified parameters and an explicit
% state.
%
-define( send_emergency_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( emergency, State,
				get_spatial_message( Message, State ), MessageCategorization )
).



% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State'.
%
-define( emergency_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( emergency, State,
				get_spatial_message( Message, State ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'emergency' type with specified parameters and an explicit
% state.
%
-define( send_emergency_full( State, Message, MessageCategorization,
							  SimulationTimestamp ),
		 class_TraceEmitter:send_safe( emergency, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State'.
%
-define( emergency_full( Message, MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( emergency, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).




% Subsection for Emergency, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'emergency' type with specified parameters and an explicit
% state.
%
-define( send_emergency_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( emergency, State, text_utils:format(
					get_spatial_message( Message, State ), FormatValues ) )
).



% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State'.
%
-define( emergency_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( emergency, State, text_utils:format(
					get_spatial_message( Message, State ), FormatValues ) )
).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'emergency' type with specified parameters and an explicit
% state.
%
-define( send_emergency_fmt_cat( State, Message, FormatValues,
								 MessageCategorization ),
		 class_TraceEmitter:send_safe( emergency, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization ) ).



% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State'.
%
-define( emergency_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( emergency, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'emergency' type with specified parameters and an explicit
% state.
%
-define( send_emergency_fmt_full( State, Message, FormatValues,
								  MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( emergency, State,
				text_utils:format(
					get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'emergency' type with specified parameters and implicit use
% of a variable named 'State'.
%
-define( emergency_fmt_full( Message, FormatValues, MessageCategorization,
							 SimulationTimestamp ),
		 class_TraceEmitter:send_safe( emergency, State,
				text_utils:format(
				  get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Alert section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Alert, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'alert' type with specified parameters and an explicit state.
-define( send_alert( State, Message ),
		 class_TraceEmitter:send_safe( alert, State,
						get_spatial_message( Message, State ) )
).



% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( alert( Message ),
		 class_TraceEmitter:send_safe( alert, State,
									   get_spatial_message( Message, State ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'alert' type with specified parameters and an explicit state.
-define( send_alert_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( alert, State,
				get_spatial_message( Message, State ), MessageCategorization )
).



% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( alert_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( alert, State,
				get_spatial_message( Message, State ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'alert' type with specified parameters and an explicit state.
-define( send_alert_full( State, Message, MessageCategorization,
						  SimulationTimestamp ),
		 class_TraceEmitter:send_safe( alert, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( alert_full( Message, MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( alert, State,
				get_spatial_message( Message, State ), MessageCategorization,
				SimulationTimestamp )
).




% Subsection for Alert, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'alert' type with specified parameters and an explicit state.
-define( send_alert_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( alert, State, text_utils:format(
			get_spatial_message( Message, State ), FormatValues ) )
).



% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( alert_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( alert, State, text_utils:format(
			get_spatial_message( Message, State ), FormatValues ) )
).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'alert' type with specified parameters and an explicit state.
-define( send_alert_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send_safe( alert, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).



% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( alert_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( alert, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'alert' type with specified parameters and an explicit state.
-define( send_alert_fmt_full( State, Message, FormatValues,
							  MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( alert, State,
				text_utils:format(
				  get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'alert' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( alert_fmt_full( Message, FormatValues, MessageCategorization,
						 SimulationTimestamp ),
		 class_TraceEmitter:send_safe( alert, State,
				text_utils:format(
				  get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
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
		 class_TraceEmitter:send_safe( critical, State,
						get_spatial_message( Message, State ) )
).



% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( critical( Message ),
		 class_TraceEmitter:send_safe( critical, State,
						get_spatial_message( Message, State ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'critical' type with specified parameters and an explicit
% state.
%
-define( send_critical_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( critical, State,
			   get_spatial_message( Message, State ),
			   MessageCategorization )
).



% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( critical_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( critical, State,
			   get_spatial_message( Message, State ),
			   MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'critical' type with specified parameters and an explicit
% state.
%
-define( send_critical_full( State, Message, MessageCategorization,
						  SimulationTimestamp ),
		 class_TraceEmitter:send_safe( critical, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( critical_full( Message, MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( critical, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).




% Subsection for Critical, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'critical' type with specified parameters and an explicit
% state.
%
-define( send_critical_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( critical, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( critical_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( critical, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'critical' type with specified parameters and an explicit
% state.
-define( send_critical_fmt_cat( State, Message, FormatValues,
								MessageCategorization ),
		 class_TraceEmitter:send_safe( critical, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).



% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( critical_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( critical, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'critical' type with specified parameters and an explicit
% state.
%
-define( send_critical_fmt_full( State, Message, FormatValues,
							  MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( critical, State,
				text_utils:format(
				  get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'critical' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( critical_fmt_full( Message, FormatValues, MessageCategorization,
						 SimulationTimestamp ),
		 class_TraceEmitter:send_safe( critical, State,
				text_utils:format(
				  get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





% Subsection for Error, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error( State, Message ),
		 class_TraceEmitter:send_safe( error, State,
						get_spatial_message( Message, State ) )
).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error( Message ),
		 class_TraceEmitter:send_safe( error, State,
						get_spatial_message( Message, State ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( error, State,
			   get_spatial_message( Message, State ),
			   MessageCategorization )
).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( error, State,
			   get_spatial_message( Message, State ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_full( State, Message, MessageCategorization,
						  SimulationTimestamp ),
		 class_TraceEmitter:send_safe( error, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_full( Message, MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( error, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).




% Subsection for Error, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( error, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( error, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send_safe( error, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( error, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_fmt_full( State, Message, FormatValues,
							  MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( error, State,
				text_utils:format(
					get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_fmt_full( Message, FormatValues, MessageCategorization,
						 SimulationTimestamp ),
		 class_TraceEmitter:send_safe( error, State,
				text_utils:format(
				  get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).










%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Warning section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% Subsection for Warning, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning( State, Message ),
		 class_TraceEmitter:send_safe( warning, State,
						get_spatial_message( Message, State ) )
).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning( Message ),
		 class_TraceEmitter:send_safe( warning, State,
						get_spatial_message( Message, State ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( warning, State,
			   get_spatial_message( Message, State ),
			   MessageCategorization )
).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( warning, State,
			   get_spatial_message( Message, State ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_full( State, Message, MessageCategorization,
							SimulationTimestamp ),
		 class_TraceEmitter:send_safe( warning, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_full( Message, MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( warning, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )

).




% Subsection for Warning, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( warning, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( warning, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_fmt_cat( State, Message, FormatValues,
							   MessageCategorization ),
		 class_TraceEmitter:send_safe( warning, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( warning, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_fmt_full( State, Message, FormatValues,
								MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( warning, State,
				text_utils:format(
					get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_fmt_full( Message, FormatValues, MessageCategorization,
						   SimulationTimestamp ),
		 class_TraceEmitter:send_safe( warning, State,
				text_utils:format(
					get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
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



% Selecting a macro implies selecting a level of priority.

% An issue is that the definition of Erlang macros does not take into account
% arities, thus LOG(X) and LOG(X,Y) cannot be defined without a name clash.
%
% This explains why the trace informations have to be specified between brakets:
% using LOG([X,Y]) instead of LOG(X,Y).
%
% Anonymous functions could be used as well.
%
% See http://www.nabble.com/question%3A-macro-definition-tt14840873.html

% For each type of trace (emergency, alert, etc.), a few variations of the set
% of specified trace informations are supported.




% Taking 'trace' as an example:
%
% - '?info("Hello")'
%
% - '?info_cat("Hello", "My Category")' ('cat' stands for 'categorized')
%
% - '?info_full("Hello", "My Category", 125 )'
%
%
% The use of io_lib:format involved too much typing, so we defined shorter forms
% instead. Taking 'trace' again as an example:
%
% - '?info_fmt("Hello ~w.", [V] )' (most frequently used form; 'fmt' stands
% for 'format')
%
% - '?info_fmt_cat("Hello ~w.", [V], "My Category")'
%
% - '?info_fmt_full("Hello ~w.", [V], "My Category", 125 )'
%
% (knowing we cannot define macros with same name but different arity)



% Some delay are added when error traces are sent, so that they can be stored
% before the virtual machine is stopped, should it happen (ex: if an exception
% is thrown).
%
% Delays should better be replaced by synchronous operations.


% If traces are enabled, only error-like ones will be echoed in the terminal,
% whereas, if the traces are disabled, warning ones will be echoed too.


% No variable is to be bound in these macros, otherwise sending more than one
% trace from the same scope would lead to 'badmatch' errors.


% Macros shall not bind variables in their body, otherwise using more than one
% in the same scope would result in the two versions of that variable being
% matched, usually resulting on a badmatch.
%
% So, here, spatial messages are either fed as one-liners to output mechanisms,
% or, when having to be used more than once, they are generated as many times as
% needed (not a real problem, as this extra work happens only in very infrequent
% situations).



% Macros to lighten repetitions induced by macro limitations:
%
% Oddly enough, the macro did not seem correctly expanded (variable 'Message' is
% unbound), so we finally replaced its reference but its actual value.
%
%-define( get_spatial_message_helper, get_spatial_message( Message, State ) ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Notice section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% Subsection for Notice, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
%
-define( send_notice( State, Message ),
		 class_TraceEmitter:send_safe( notice, State,
									   get_spatial_message( Message, State ) )
).



% Sends a trace of 'notice' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( notice( Message ),
		 class_TraceEmitter:send_safe( notice, State,
									   get_spatial_message( Message, State ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
%
-define( send_notice_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( notice, State,
				get_spatial_message( Message, State ), MessageCategorization )
).



% Sends a trace of 'notice' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( notice_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( notice, State,
				get_spatial_message( Message, State ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
%
-define( send_notice_full( State, Message, MessageCategorization,
						   SimulationTimestamp ),
		 class_TraceEmitter:send_safe( notice, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'notice' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( notice_full( Message, MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( notice, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).




% Subsection for Notice, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
-define( send_notice_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( notice, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Sends a trace of 'notice' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( notice_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( notice, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
-define( send_notice_fmt_cat( State, Message, FormatValues,
							  MessageCategorization ),
		 class_TraceEmitter:send_safe( notice, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).



% Sends a trace of 'notice' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( notice_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( notice, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'notice' type with specified parameters and an explicit
% state.
%
-define( send_notice_fmt_full( State, Message, FormatValues,
							  MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( notice, State,
				text_utils:format(
					get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'notice' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( notice_fmt_full( Message, FormatValues, MessageCategorization,
						  SimulationTimestamp ),
		 class_TraceEmitter:send_safe( notice, State,
				text_utils:format(
					get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% Subsection for Info, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit state.
-define( send_info( State, Message ),
		 class_TraceEmitter:send_safe( info, State,
						get_spatial_message( Message, State ) )
).



% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info( Message ),
		 class_TraceEmitter:send_safe( info, State,
						get_spatial_message( Message, State ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'info' type with specified parameters and an explicit state.
-define( send_info_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( info, State,
			   get_spatial_message( Message, State ), MessageCategorization )
).



% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( info, State,
			   get_spatial_message( Message, State ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit state.
-define( send_info_full( State, Message, MessageCategorization,
						 SimulationTimestamp ),
		 class_TraceEmitter:send_safe( info, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info_full( Message, MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( info, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )

).




% Subsection for Info, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit state.
-define( send_info_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( info, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( info, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit state.
-define( send_info_fmt_cat( State, Message, FormatValues,
							MessageCategorization ),
		 class_TraceEmitter:send_safe( info, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).



% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( info, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit state.
-define( send_info_fmt_full( State, Message, FormatValues,
							 MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( info, State,
				text_utils:format(
					get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info_fmt_full( Message, FormatValues, MessageCategorization,
						SimulationTimestamp ),
		 class_TraceEmitter:send_safe( info, State,
				text_utils:format(
				  get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% Subsection for Debug, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'debug' type with specified parameters and an explicit state.
-define( send_debug( State, Message ),
		 class_TraceEmitter:send_safe( debug, State,
						get_spatial_message( Message, State ) )
).



% Sends a trace of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug( Message ),
		 class_TraceEmitter:send_safe( debug, State,
						get_spatial_message( Message, State ) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'debug' type with specified parameters and an explicit state.
-define( send_debug_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( debug, State,
				get_spatial_message( Message, State ), MessageCategorization )
).



% Sends a trace of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( debug, State,
				get_spatial_message( Message, State ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'debug' type with specified parameters and an explicit state.
-define( send_debug_full( State, Message, MessageCategorization,
						  SimulationTimestamp ),
		 class_TraceEmitter:send_safe( debug, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug_full( Message, MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( debug, State,
				get_spatial_message( Message, State ),
				MessageCategorization, SimulationTimestamp )
).




% Subsection for Debug, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'debug' type with specified parameters and an explicit state.
-define( send_debug_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send_safe( debug, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Sends a trace of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( debug, State,
			text_utils:format( get_spatial_message( Message, State ),
							   FormatValues ) )
).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'debug' type with specified parameters and an explicit state.
-define( send_debug_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send_safe( debug, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).



% Sends a trace of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( debug, State,
		   text_utils:format( get_spatial_message( Message, State ),
							  FormatValues ), MessageCategorization )
).




% Categorized with simulation timestamp, with 2 variations regarding state:
% explicit or implicit.


% Sends a trace of 'debug' type with specified parameters and an explicit state.
-define( send_debug_fmt_full( State, Message, FormatValues,
							  MessageCategorization, SimulationTimestamp ),
		 class_TraceEmitter:send_safe( debug, State,
				text_utils:format(
					get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).



% Sends a trace of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug_fmt_full( Message, FormatValues, MessageCategorization,
						 SimulationTimestamp ),
		 class_TraceEmitter:send_safe( debug, State,
				text_utils:format(
				  get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, SimulationTimestamp )
).








-else. % tracing_activated




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traces are disabled here.
% This 'else' branch will be used iff tracing_activated is not defined above.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







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
						 SimulationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 SimulationTimestamp )
).


-define( notice_full( Message, MessageCategorization, SimulationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 SimulationTimestamp )
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
		trace_disabled( State, Message, FormatValues, MessageCategorization )
).


-define( notice_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization )
).






-define( send_notice_fmt_full( State, Message, FormatValues,
							   MessageCategorization, SimulationTimestamp ),
		trace_disabled( State, Message, FormatValues,
						MessageCategorization, SimulationTimestamp )
).


-define( notice_fmt_full( Message, FormatValues, MessageCategorization,
						  SimulationTimestamp ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization,
						 SimulationTimestamp )
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
						 SimulationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 SimulationTimestamp )
).


-define( info_full( Message, MessageCategorization, SimulationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 SimulationTimestamp )
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
		trace_disabled( State, Message, FormatValues, MessageCategorization )
).


-define( info_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization )
).






-define( send_info_fmt_full( State, Message, FormatValues,
							 MessageCategorization, SimulationTimestamp ),
		trace_disabled( State, Message, FormatValues,
						MessageCategorization, SimulationTimestamp )
).


-define( info_fmt_full( Message, FormatValues, MessageCategorization,
						SimulationTimestamp ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization,
						 SimulationTimestamp )
).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Debug, without formatting.


% Most important debug categories cannot be disabled:

-define( send_debug( State, Message ),
		 trace_disabled( State, Message )
).


-define( debug(Message),
		 trace_disabled( State, Message )
).






-define( send_debug_cat( State, Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).


-define( debug_cat( Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).





-define( send_debug_full( State, Message, MessageCategorization,
						  SimulationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 SimulationTimestamp )
).


-define( debug_full( Message, MessageCategorization, SimulationTimestamp ),
		 trace_disabled( State, Message, MessageCategorization,
						 SimulationTimestamp )
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
							  MessageCategorization, SimulationTimestamp ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization, SimulationTimestamp )
).


-define( debug_fmt_full( Message, FormatValues, MessageCategorization,
						 SimulationTimestamp ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization,
						 SimulationTimestamp )
).



-endif. % tracing_activated




% End of the tracing_activated branch.






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
		 class_TraceEmitter:send_safe( info, State,
									   get_spatial_message( Message, State ) )
).



% Sends a report with specified parameters and implicit use of a variable named
% 'State'.
%
-define( report_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_safe( info, State,
			  text_utils:format( get_spatial_message( Message, State ),
								 FormatValues ) )
).




% Categorized version, with implicit state.


% Sends a report with specified categorization and implicit use of a variable
% named 'State'.
%
-define( report_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_safe( info, State,
				get_spatial_message( Message, State ), MessageCategorization )
).



% Sends a report with specified categorization and implicit use of a variable
% named 'State'.
%
-define( report_cat_fmt( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_safe( info, State,
			text_utils:format(
				get_spatial_message( Message, State ), FormatValues ),
			MessageCategorization )
).




% Categorized version with simulation timestamp, with implicit state.



% Sends a report with specified categorization and simulation timestamp, and
% implicit use of a variable named 'State'.
%
-define( report_full( State, Message, MessageCategorization,
					  SimulationTimestamp ),
		 class_TraceEmitter:send_safe( info, State,
				   get_spatial_message( Message, State ),
				   MessageCategorization, SimulationTimestamp )
).



% Sends a report with specified categorization and simulation timestamp, and
% implicit use of a variable named 'State'.
%
-define( report_full_fmt( State, Message, FormatValues, MessageCategorization,
						  SimulationTimestamp ),
		 class_TraceEmitter:send_safe( info, State, text_utils:format(
					get_spatial_message( Message, State ), FormatValues ),
					MessageCategorization, SimulationTimestamp )
).



% Cannot be exported here, as must be included after WOOPER header (which
% defines functions)

%-export([ get_spatial_message/2 ]).


% Returns an updated message prefixed by any available spatial information.
%
% Defined as a function not to duplicate too much code, and also to avoid
% introducing new bindings in functions using spatial traces.

% (helper)
%
-spec get_spatial_message( text_utils:ustring(), wooper:state() ) ->
									text_utils:string().
get_spatial_message( Message, State ) ->

	case ?getAttr(position) of

		undefined ->
			Message;

		Pos ->
			ApproxPosString = linear_2D:to_string( Pos, _Precision=1 ),
			Message ++ text_utils:format( "~n[while located around ~ts] ",
										  [ ApproxPosString ] )

	end.


% Not to be reported as unused should no trace sending refer to it:
-compile( [ { nowarn_unused_function, [ { get_spatial_message, 2 } ] } ] ).



% See traces/src/class_TraceEmitter.hrl for comments and information regarding
% that (counterpart) section:

-compile( [ { nowarn_unused_function, [ { trace_disabled, 1 },
										{ trace_disabled, 2 },
										{ trace_disabled, 3 },
										{ trace_disabled, 4 },
										{ trace_disabled, 5 } ] } ] ).


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


-endif. % class_SpatialTraceEmitter_hrl_guard
