% Copyright (C) 2022-2024 Olivier Boudeville
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
% Creation date: Saturday, August 6, 2022.


% @doc Interface class implementing the <b>Traceable trait</b>, so that the
% instances having that trait can <b>emit traces</b> by whichever means is the
% most appropriate, whether or not they are actually full-blown
% class_TraceEmitter instances, or have a trace_bridge registered, or just have
% to rely on the most basic traces.
%
% By providing the most lightweight way of emitting traces, such a trait favors
% composition over multiple inheritance; otherwise many concrete classes would
% derive more than once from the class_TraceEmitter one (this would be a minor
% nuisance) or, worse, other traits would induce in turn a class_TraceEmitter
% inheritance, each time they would have to send a trace (whereas interfaces
% introducing the use of actual classes is not felt desirable; we prefer that
% such interfaces derive from this Traceable one).
%
% This interface does not define attributes or methods of its own, and does not
% require specific initialisation or termination (it is mostly useful thanks to
% its header file). As such, it could even be omitted in the declarations of
% superclasses and possibly any composed interfaces, although we recommend
% listing it explicitly, for clarity reasons.
%
-module(class_Traceable).


-define( class_description,
		 "Interface to be implemented by all instances supporting the Trace "
		 "API, that is are able to emit some kind of traces." ).


% No superclasses.


% Declaration of the interface-specific attributes:
%
% (as it is a WOOPER builtin, they are all prefixed with 'wooper' and the
% interface name)
%
% No specific attribute involved: -define( class_attributes, [] ).


% Implementation notes:
%
% So this interface is to work in all cases, whether or not it plugs to the
% facilities offered by a real trace emitter, a trace bridge or just basic
% traces.
%
% It was not introduced at the Myriad-level (see its trace_{utils,bridge}
% modules), as dealing with (WOOPER) instances. Moreover this trait loosely
% depends on class_TraceEmitter (for its attributes and API), so should best be
% in Ceylan-Traces.
%
% This class could almost be reduced to a mere header (as class_Traceable.hrl is
% self-standing), with no need of specific attribute, constructor, destructor or
% even methods (just convenience helpers). Only the key trace sending macros are
% made available by this interface (four for each trace severity: with or
% without an explict state, and with or without a trace format with associated
% values; features very infrequently used such as emitter/messag categorization
% or sending with no echo have not been integrated here).
%
% Its only overhead of this interface compared to a direct use of
% class_TraceEmitter is an extra attribute lookup.


% Helper functions:
-export([ send/3, send_safe/3 ]).

% Exported helper functions that can be applied to any WOOPER state:
-export([ to_maybe_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").



% Shorthands:

-type ustring() :: text_utils:ustring().

-type trace_severity() :: traces:trace_severity().
-type message() :: traces:message().



% @doc Constructs a traceable instance.
%
% Note: if wanting this instance to use any trace emitter machinery (that it
% would have for any reason), this constructor (and more generally any
% trace-sending operation) one should be called *after* the class_TraceEmitter
% one (that is on a state that is already the one of a trace emitter).
%
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% No-op: all actual operations decide directly which trace emission they
	% should perform, not relying on any trait-specific attribute.
	%
	% So even calling this constructor or deriving from this class is actually
	% optional (but recommended, as clearer).

	State.


% No specific destructor needed.


% No member method.




% Generic interface.


% The name of the chosen attribute to determine whether the current instance is
% a class_TraceEmitter one.
%
-define( trace_emitter_characteristic_attr, trace_aggregator_pid ).


% No static methods.



% Helper section.


% @doc Sends a trace from that traceable instance.
%
% Message is a plain string.
%
% (helper)
%
-spec send( trace_severity(), wooper:state(), message() ) -> void().
send( Severity, State, Message ) ->
	% Switching to the best option:
	case hasAttribute( State, ?trace_emitter_characteristic_attr ) of

		true ->
			class_TraceEmitter:send( Severity, State, Message );

		false ->
			trace_bridge:send( Severity, Message )

	end.


% @doc Sends a trace from that traceable instance, echoing it through basic
% traces as well.
%
% Message is a plain string.
%
% (helper)
%
-spec send_safe( trace_severity(), wooper:state(), message() ) -> void().
send_safe( Severity, State, Message ) ->
	% Switching to the best option:
	case hasAttribute( State, ?trace_emitter_characteristic_attr ) of

		true ->
			class_TraceEmitter:send_safe( Severity, State, Message );

		false ->
			trace_bridge:send( Severity, Message )

	end.



% The following helper functions can be used in the context of any class,
% whether or not it implements the Traceable interface.


% @doc Returns a textual element of description of the corresponding instance,
% should it implement the Traceable interface.
%
% (exported helper)
%
-spec to_maybe_string( wooper:state() ) -> maybe( ustring() ).
to_maybe_string( State ) ->
	case ?getMaybeAttr(name) of

		undefined ->
			undefined;

		Name ->
			case ?getMaybeAttr(trace_emitter_categorization) of

				undefined ->
					text_utils:format( "named '~ts'", [ Name ] );

				EmitterCateg ->
					text_utils:format( "named '~ts', categorized as ~ts",
									   [ Name, EmitterCateg ] )

			end

	end.
