% Copyright (C) 2010-2023 Olivier Boudeville
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
% Creation date: 2010.


% @doc Module to create <b>WOOPER instance proxies</b>.
%
% The purpose of a proxy P is to be a process that acts as a man-in-the-middle
% for a WOOPER target instance T: all processes interacting with P will actually
% interact transparently with T.
%
% This can be useful when, for example, a local process (P) is needed (ex: must
% be locally registered), whereas the actual service is implemented by a remote
% instance (T).
%
% Note that this proxy is only as transparent as reasonably achievable and,
% that, anyway, proxies are seldom satisfactory solutions.
%
-module(wooper_instance_proxy).


-type proxy_pid() :: pid().

-export_type([ proxy_pid/0 ]).


-export([ start/1, start_link/1 ]).


% Shorthand:
-type instance_pid() :: wooper:instance_pid().


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").


% @doc Starts a proxy for the specified WOOPER instance, designated by the
% specified PID.
%
-spec start( instance_pid() ) -> proxy_pid().
start( TargetInstancePid ) ->

	trace_utils:notice_fmt( "Creating a proxy for WOOPER instance ~w.",
							[ TargetInstancePid ] ),

	?myriad_spawn( fun() ->
					proxy_main_loop( TargetInstancePid )
				   end ).



% @doc Starts and links a proxy for the specified WOOPER instance, designated by
% the specified PID.
%
-spec start_link( instance_pid() ) -> proxy_pid().
start_link( TargetInstancePid ) ->

	trace_utils:notice_fmt( "Creating a linked proxy for WOOPER instance ~w.",
							[ TargetInstancePid ] ),

	?myriad_spawn_link( fun() ->
							proxy_main_loop( TargetInstancePid )
						end ).



% @doc Main loop of the proxy.
-spec proxy_main_loop( instance_pid() ) -> no_return().
proxy_main_loop( TargetInstancePid ) ->

	trace_utils:debug_fmt(
		"Proxy ~w waiting for a call to WOOPER target instance ~w.",
		[ self(), TargetInstancePid ] ),


	% This proxy is expected to receive (only) either requests or oneways:
	receive

		{ RequestName, Args, SenderPid } ->

			trace_utils:debug_fmt( "Proxy ~w processing request ~p.",
				[ self(), { RequestName, Args, SenderPid } ] ),

			TargetInstancePid ! { RequestName, Args, self() },
			receive

				% Expecting a {wooper_result, Res} pair:
				R ->
					trace_utils:debug_fmt(
						"Proxy ~w returning ~p to caller ~w.",
						[ self(), R, SenderPid ] ),

					SenderPid ! R

			end,
			proxy_main_loop( TargetInstancePid ) ;


		{ OnewayName, Args } ->

			trace_utils:debug_fmt( "Proxy ~w processing oneway ~p.",
								   [ self(), { OnewayName, Args } ] ),

			TargetInstancePid ! { OnewayName, Args },

			proxy_main_loop( TargetInstancePid );


		delete ->

			trace_utils:debug_fmt(
				"Deleting proxy ~w for WOOPER target instance ~w.",
				[ self(), TargetInstancePid ] ),

			% No looping here:
			TargetInstancePid ! delete;


		OnewayName when is_atom( OnewayName ) ->

			trace_utils:debug_fmt( "Proxy ~w processing oneway ~p.",
								   [ self(), OnewayName ] ),

			TargetInstancePid ! OnewayName,
			proxy_main_loop( TargetInstancePid );


		Other ->

			trace_utils:debug_fmt(
				"Warning: WOOPER instance proxy (~w) for ~w ignored "
				"following message: ~p.",
			  [ self(), TargetInstancePid, Other ] ),

			proxy_main_loop( TargetInstancePid )

	end.
