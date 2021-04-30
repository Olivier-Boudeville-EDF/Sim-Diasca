% Copyright (C) 2019-2021 Olivier Boudeville
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
% Creation date: Saturday, July 20, 2019.


% Module implementing the Traces (active) application behaviour.
%
% Note that, thanks to the automatic creation of the class manager, Traces will
% still work flawlessly even if not specifically started (typically then out of
% any OTP context).
%
-module(traces_app).


% Implementing the (active, OTP) application behaviour:
%
% (see https://erlang.org/doc/design_principles/applications.html)
%
-behaviour(application).


% Callbacks of the application behaviour:
-export([ start/2, stop/1 ]).



% Starts the Traces services.
%
% Note: RestartType and StartArgs at least currently ignored.
%
-spec start( application:start_type(), StartArgs :: term() ) -> { 'ok', pid() }
		| { 'ok', pid(), State :: term() } | { 'error', Reason :: term() }.
start( RestartType, StartArgs ) ->

	% See any {is_batch, boolean()} entry for the 'traces' application in any
	% conf/sys.config defined for the current OTP release (or override it with
	% --batch on the command-line):
	%
	TraceSupervisorWanted = not executable_utils:is_batch(),

	trace_utils:debug_fmt( "Starting Traces application (restart type: ~w, "
		"start arguments: ~w, supervisor wanted: ~ts).",
		[ RestartType, StartArgs, TraceSupervisorWanted ] ),

	% Previously, no specific root supervisor was to launch, but:
	%class_TraceAggregator:start().

	% We now create a root supervisor that has a supervisor_bridge child, which
	% takes care of the interface to the (non-OTP) trace aggregator:
	%
	case traces_sup:start_link( TraceSupervisorWanted ) of

		R={ ok, _RootSupervisorPid } ->
			R;

		Other ->
			trace_utils:error_fmt( "The Traces root supervisor did not start "
								   "properly:~n  ~p.", [ Other ] ),
			{ error, Other }

	end.



% Stops the Traces services.
-spec stop( State :: term() ) -> void().
stop( State ) ->

	trace_utils:debug_fmt( "Stopping Traces application (state: ~w).",
						   [ State ] ),

	% Previously: (now managed by the root supervisor)
	%class_TraceAggregator:stop(),

	ok.
