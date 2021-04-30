% Copyright (C) 2019-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER library.
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
% Creation date: Sunday, July 14, 2019.


% Module implementing the root supervisor of WOOPER.
%
% In practice, it will supervise a single process, the one of the (singleton)
% WOOPER class manager (which implements the gen_server behaviour).
%
-module(wooper_sup).


% Implementing the OTP supervisor behaviour:
-behaviour(supervisor).


% User API:
-export([ start_link/0 ]).


% Callback of the supervisor behaviour:
-export([ init/1 ]).


-define( wooper_supervisor_name, ?MODULE ).


% Starts and links the WOOPER root supervisor.
-spec start_link() -> supervisor:startlink_ret().
start_link() ->

	trace_utils:debug( "Starting the WOOPER root supervisor." ),

	% A local registration is better in order to avoid inter-node clashes:
	supervisor:start_link( _Reg={ local, ?wooper_supervisor_name },
						   _Module=?MODULE, _Args=undefined ).



% Callback to initialise this supervisor.
-spec init( boolean() ) -> { 'ok',
	   { supervisor:sup_flags(), [ supervisor:child_spec() ] } } | 'ignore'.
init( Args=undefined ) ->

	trace_utils:debug_fmt(
	  "Initializing the WOOPER root supervisor (args: ~p).", [ Args ] ),

	% Restart only children that terminate.
	% Never expected to fail, though:
	%
	SupSettings = otp_utils:get_supervisor_settings(
					_RestartStrategy=one_for_one,
					wooper:get_execution_target() ),

	% The WOOPER class manager is a rather basic gen_server:
	ClassManagerChildSpec = #{

	  id => wooper_class_manager_id,

	  start => { _Mod=wooper_class_manager, _Fun=start_link, _Args=[] },

	  % Always restarted:
	  restart => permanent,

	  % 2-second termination allowed before brutal killing:
	  shutdown => 2000,

	  type => worker,

	  modules => [ wooper_class_manager ] },

	ChildrenSpec = [ ClassManagerChildSpec ],


	{ ok, { SupSettings, ChildrenSpec } }.
