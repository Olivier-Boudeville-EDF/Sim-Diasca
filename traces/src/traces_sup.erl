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


% Module implementing the root supervisor of Traces.
%
% In practice, it will supervise a single process, the one of the (singleton)
% trace aggregator, through a dedicated supervision bridge, defined in the
% traces_bridge_sup module.
%
% Refer to the documentation of the supervisor_bridge module for further
% details.
%
-module(traces_sup).


% The root supervisor is a supervisor per se:
-behaviour(supervisor).


% User API, typically triggered from traces_app:
-export([ start_link/1 ]).


% Callback of the supervisor behaviour:
%
% (see https://erlang.org/doc/design_principles/sup_princ.html)
%
-export([ init/1 ]).


-define( root_supervisor_name, ?MODULE ).


% Starts and links the Traces root supervisor, creating in turn a proper
% supervision bridge.
%
% Note: typically called by traces_app:start/2, hence generally triggered by the
% application initialisation.
%
-spec start_link( boolean() ) -> supervisor:startlink_ret().
start_link( TraceSupervisorWanted ) ->

	trace_utils:debug_fmt( "Starting the Traces root supervisor, from ~w.",
						   [ self() ] ),

	% A local registration is better in order to avoid inter-node clashes:
	supervisor:start_link( _Reg={ local, ?root_supervisor_name },
						   _Mod=?MODULE, _Args=TraceSupervisorWanted ).



% Callback to initialise the Traces supervisor bridge (supervised by this root
% supervisor), typically in answer to start_link/1 above being executed.
%
-spec init( boolean() ) -> { 'ok',
	   { supervisor:sup_flags(), [ supervisor:child_spec() ] } } | 'ignore'.
init( TraceSupervisorWanted ) ->

	trace_utils:info_fmt( "Initializing the Traces root supervisor ~w "
		"(trace supervisor wanted: ~ts).", [ self(), TraceSupervisorWanted ] ),

	% Restart only children that terminate.
	% Never expected to fail, though:
	%
	SupSettings = otp_utils:get_supervisor_settings(
				_RestartStrategy=one_for_one, traces:get_execution_target() ),

	% One child, a supervisor bridge in charge of the trace aggregator:
	BridgeChildSpec = #{

	  id => traces_bridge_id,

	  start => { _Mod=traces_bridge_sup, _Fun=start_link,
				 _Args=[ TraceSupervisorWanted ] },

	  % Always restarted:
	  restart => permanent,

	  % 2-second termination was allowed before brutal killing; yet now this
	  % child process is of the 'supervisor' type, and, in
	  % https://erlang.org/doc, the
	  % design_principles/sup_princ.html#child-specification page explains that
	  % 'infinity' is required here:
	  %
	  shutdown => infinity,

	  type => supervisor,

	  modules => [ traces_bridge_sup ] },

	{ ok, { SupSettings, [ BridgeChildSpec ] } }.
