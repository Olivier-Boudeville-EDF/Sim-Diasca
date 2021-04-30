% Copyright (C) 2020-2021 Olivier Boudeville
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
% Creation date: Wednesday, May 6, 2020.


% Module implementing the supervisor bridge of Traces, so that the (singleton)
% trace aggregator is attached to the Traces supervision tree, through the
% Traces root supervisor, defined in the traces_sup module.
%
-module(traces_bridge_sup).


% The trace aggregator is not a gen_server but a WOOPER instance, therefore a
% supervisor bridge is needed in order to connect this aggregator to an OTP
% supervision tree.
%
% As a result, the process whose code is defined in the current module, being a
% supervisor bridge, behaves like a real supervisor to its own supervisor (the
% root supervisor of Traces, namely traces_sup), but has a different interface
% than a real supervisor to the Traces subsystem.
%
% Hence used for (optional) OTP compliance (see
% http://erlang.org/doc/man/supervisor_bridge.html).
%
% We suppose that such a supervisor bridge cannot be used directly as a root
% supervisor.
%
% See also within the Erlang codebase itself, as an example, the user_sup
% supervisor bridge, created by kernel:init/1.
%
-behaviour(supervisor_bridge).


% User API:
-export([ start_link/1 ]).


% Callbacks of the supervisor_bridge behaviour:
-export([ init/1, terminate/2 ]).


-define( bridge_name, ?MODULE ).


% For otp_application_module_name:
-include("class_TraceAggregator.hrl").



% Starts and links the Traces supervision bridge to the trace aggregator.
%
% Note: typically spawned as a supervised child of the Traces root supervisor
% (see traces_sup:init/1), hence generally triggered by the application
% initialisation.
%
-spec start_link( boolean() ) -> term().
start_link( TraceSupervisorWanted ) ->

	% Apparently not displayed in a release context, yet executed:
	trace_utils:debug_fmt( "Starting the Traces supervisor bridge, from ~w.",
						   [ self() ] ),

	supervisor_bridge:start_link( { local, ?bridge_name },
			_Module=?MODULE, _Args=TraceSupervisorWanted ).



% Callback to initialise this supervisor bridge, typically in answer to
% start_link/1 above being executed.
%
-spec init( boolean() ) -> { 'ok', pid(), State :: term() }
						 | 'ignore' | { 'error', Error :: term() }.
init( TraceSupervisorWanted ) ->

	trace_utils:info_fmt( "Initializing the Traces supervisor bridge ~w "
		"(trace supervisor wanted: ~ts).", [ self(), TraceSupervisorWanted ] ),

	% This is an OTP blind start, the Traces application being started with no
	% parameter - so with no trace filename possibly specified.
	%
	% As a result, knowing that no safe renaming of the trace filename can be
	% done once a trace supervisor is launched (the trace aggregator would be
	% fine, but at least most trace supervisors not), the creation of that trace
	% file is deferred. It may then be (re)named once the configuration of the
	% Traces-using application will be read, before being created and writing
	% the pending first traces:
	%
	InitTraceSupervisor = case TraceSupervisorWanted of

		true ->
			later;

		false ->
			false

	end,

	% Not initializing our trace supervisor (not OTP related, referring to
	% class_TraceSupervisor here) now, as we may have to adopt a non-default
	% trace filename afterwards (ex: after any parent applications read its own
	% configuration file to select a specific name/path), and as mentioned above
	% any already running trace supervisor would not be able to cope with it.
	%
	% The next call must not disable the trapping of EXIT messages, as the
	% supervisor (bridge) behaviour implies (and had made so) that this process
	% already traps EXITs: otherwise for example the bridged process will not be
	% properly shutdown.
	%
	TraceAggregatorPid = traces_for_apps:app_start(
		_ModuleName=?otp_application_module_name, InitTraceSupervisor,
		_DisableExitTrapping=false ),

	trace_utils:debug_fmt( "Traces supervisor bridge initialised, "
		"with trace aggregator ~w.", [ TraceAggregatorPid ] ),

	{ ok, TraceAggregatorPid, _State=TraceAggregatorPid }.



% Callback to terminate this supervisor bridge.
-spec terminate( Reason :: 'shutdown' | term(), State :: term() ) -> void().
terminate( Reason, _State=TraceAggregatorPid )
  when is_pid( TraceAggregatorPid ) ->

	trace_utils:info_fmt( "Terminating the Traces supervisor bridge "
		"(reason: ~w, trace aggregator: ~w).", [ Reason, TraceAggregatorPid ] ),

	% Works whether or not a trace supervisor is used:
	traces_for_apps:app_stop( _ModuleName=?otp_application_module_name,
		TraceAggregatorPid, _WaitForTraceSupervisor=false );

terminate( Reason, State ) ->
	trace_utils:info_fmt( "Terminating the Traces supervisor bridge "
						  "(reason: ~w, state: ~w).", [ Reason, State ] ).
