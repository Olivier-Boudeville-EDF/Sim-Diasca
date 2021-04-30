% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
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


% Gathering of various facilities about naming services (local and global).
%
% See naming_utils_test.erl for the corresponding test.
%
-module(naming_utils).


% Registration functions.
-export([ register_as/2, register_as/3, register_or_return_registered/2,
		  unregister/2,

		  registration_to_look_up_scope/1,

		  get_registered_pid_for/1, get_registered_pid_for/2,
		  get_locally_registered_pid_for/2,

		  get_registered_names/1,

		  is_registered/1, is_registered/2,
		  wait_for_registration_of/2,
		  wait_for_global_registration_of/1, wait_for_global_registration_of/2,
		  wait_for_local_registration_of/1, wait_for_local_registration_of/2,
		  wait_for_remote_local_registrations_of/2,
		  display_registered/0 ]).


% Necessarily an atom:
-type registration_name() :: atom().


-type registration_scope() :: 'global_only'
							| 'local_only'
							| 'local_and_global'
							| 'none'.


-type look_up_scope() :: 'global'
					   | 'local'
					   | 'local_and_global'
					   | 'local_otherwise_global'.


-export_type([ registration_name/0, registration_scope/0, look_up_scope/0 ]).



% Registration functions.
%
% Note that:
% - only local processes can be registered locally
% - a given PID cannot be registered globally under more than one name
% - if a registered process terminates (for any of the two scopes), its name is
% automatically unregistered (see
% http://erlang.org/doc/reference_manual/processes.html and
% http://erlang.org/doc/man/global.html)


% Shorthands:

-type atom_node_name() :: net_utils:atom_node_name().


% Registers the current process under specified name, which must be an atom.
%
% Declaration is register_as( Name, RegistrationScope ) with RegistrationScope
% in 'local_only', 'global_only', 'local_and_global', 'none' depending on what
% kind of registration is requested.
%
% Throws an exception on failure (ex: if that name is already registered).
%
-spec register_as( registration_name(), registration_scope() ) -> void().
register_as( Name, RegistrationScope ) ->
	register_as( self(), Name, RegistrationScope ).



% Registers specified (local) PID under specified name, which must be an atom.
%
% Declaration is: register_as( Pid, Name, RegistrationScope ) with
% RegistrationScope in 'local_only', 'global_only', 'local_and_global', 'none',
% depending on what kind of registration is requested.
%
% Throws an exception on failure.
%
-spec register_as( pid(), registration_name(), registration_scope() ) -> void().
register_as( Pid, Name, local_only ) when is_atom( Name ) ->

	%trace_utils:debug_fmt( "register_as: local_only, "
	%                       "with PID=~w and Name='~p'.", [ Pid, Name ] ),

	try erlang:register( Name, Pid ) of

		true ->
			ok

	catch

		error:badarg ->

			case is_registered( Name, local ) of

				% No more information obtained:
				not_registered ->
					throw( { local_registration_failed, Name,
							 { error, badarg } } );

				Pid ->
					throw( { local_registration_failed, Name,
							 already_registered, Pid } );

				RegPid ->
					throw( { local_registration_failed, Name,
							 already_registered, { Pid, RegPid } } )

			end;

		ExceptionType:Exception ->
			throw( { local_registration_failed, Name,
					 { ExceptionType, Exception } } )

	end;


register_as( Pid, Name, global_only ) when is_atom( Name ) ->

	%trace_utils:debug_fmt( "register_as: global_only, with PID=~w "
	%                       "and Name='~p'.", [ Pid, Name ] ),

	case global:register_name( Name, Pid ) of

		yes ->
			ok;

		no ->
			throw( { global_registration_failed, Name } )

	end;

register_as( Pid, Name, local_and_global ) when is_atom( Name ) ->
	register_as( Pid, Name, local_only ),
	register_as( Pid, Name, global_only );

register_as( _Pid, _Name, none ) ->
	ok;

register_as( _Pid, Name, Other ) when is_atom( Name ) ->
	throw( { invalid_registration_scope, Other } );

register_as( _Pid, Name, _Other ) ->
	throw( { invalid_type_for_name, Name } ).



% Registers specified PID under specified name (which must be an atom) and scope
% (only local_only and global_only registration scopes permitted), and returns
% 'registered', or returns the PID of any process already registered.
%
% This is an atomic operation, which is not meant to fail.
%
% Allows for example a series of non-synchronised processes to all attempt to
% register: the first will succeed, all the others will get its PID, none will
% fail.
%
-spec register_or_return_registered( registration_name(),
	'global_only' | 'local_only' ) -> 'registered' | pid().
register_or_return_registered( Name, Scope ) when is_atom( Name ) ->

	% Minor annoyance: we ensured that looking up a process relied generally on
	% a different atom than registering it (ex: 'global' vs 'global_only').
	%
	% Here, we expect the user to specify a registration atom; we need to
	% convert it for look-up purposes:
	%
	LookUpScope = registration_to_look_up_scope( Scope ),

	case is_registered( Name, LookUpScope ) of

		not_registered ->

			try

				register_as( Name, Scope ),
				registered

			catch

				throw:_ ->
					% Another process must have registered in-between, let's
					% restart:
					%
					% (a small random waiting could be added here)
					%
					register_or_return_registered( Name, Scope )

			end;


		Pid ->
			Pid

	end.



% Unregisters specified name from specified registry.
%
% Throws an exception in case of failure.
%
% Note: when a process terminates, it unregisters its name (if any)
% automatically from all scopes.
%
-spec unregister( registration_name(), registration_scope() ) -> void().
unregister( Name, local_only ) ->

	try erlang:unregister( Name ) of

		true ->
			ok

	catch

		ExceptionType:Exception ->
			throw( { local_unregistration_failed, Name,
					 { ExceptionType, Exception } } )

	end;

unregister( Name, global_only ) ->
	% Documentation says it returns "void" (actually 'ok'):
	try

		global:unregister_name( Name )

	catch

		ExceptionType:Exception ->
			throw( { global_unregistration_failed, Name,
					 { ExceptionType, Exception } } )

	end;

unregister( Name, local_and_global ) ->
	unregister( Name, local_only ),
	unregister( Name, global_only );

unregister( _Name, none ) ->
	ok.



% Returns the PID that should be already registered, as specified name.
%
% Local registering will be requested first, if not found global one will be
% tried.
%
% No specific waiting for registration will be performed, see
% wait_for_*_registration_of instead.
%
-spec get_registered_pid_for( registration_name() ) -> pid().
get_registered_pid_for( Name ) ->
	get_registered_pid_for( Name, _RegistrationScope=local_otherwise_global ).



-spec get_registered_pid_for( registration_name(), look_up_scope() ) ->  pid().
get_registered_pid_for( Name, _RegistrationScope=local_otherwise_global ) ->

	try

		get_registered_pid_for( Name, local )

	catch

		{ not_registered_locally, _Name } ->

			try

				get_registered_pid_for( Name, global )

			catch

				{ not_registered_globally, Name } ->
					throw( { neither_registered_locally_nor_globally, Name } )

			end

	end;

get_registered_pid_for( Name, _RegistrationScope=local ) ->
	case erlang:whereis( Name ) of

		undefined ->
			throw( { not_registered_locally, Name } );

		Pid ->
			Pid

	end;

get_registered_pid_for( Name, _RegistrationScope=global ) ->
	case global:whereis_name( Name ) of

		undefined ->
			throw( { not_registered_globally, Name } );

		Pid ->
			Pid

	end;

% So that the atom used for registration can be used for look-up as well,
% notably in static methods (see the registration_scope defines).
%
get_registered_pid_for( Name, _RegistrationScope=local_and_global ) ->
	get_registered_pid_for( Name, local_otherwise_global ).



% Returns the PID of the process corresponding to the specified local name on
% specified node: that process is expected to be locally registered on that
% specified node.
%
% Throws an exception on failure.
%
-spec get_locally_registered_pid_for( registration_name(), atom_node_name() ) ->
											pid().
get_locally_registered_pid_for( Name, TargetNode ) ->

	case rpc:call( TargetNode, _Mod=erlang, _Fun=whereis, _Args=[ Name ] ) of

		{ badrpc, Reason } ->
			throw( { not_registered_locally, Name, TargetNode, Reason } );

		Res ->
			Res

	end.



% Returns a list of the names of the registered processes, for specified look-up
% scope.
%
-spec get_registered_names( look_up_scope() ) -> [ registration_name() ].
get_registered_names( _LookUpScope=global ) ->
	global:registered_names();

get_registered_names( _LookUpScope=local ) ->
	erlang:registered().



% Tells whether specified name is registered in the specified local/global
% context: if no, returns the 'not_registered' atom, otherwise returns the
% corresponding PID.
%
% Local registering will be requested first, if not found global one will be
% tried.
%
% No specific waiting for registration will be performed, see
% wait_for_*_registration_of instead.
%
-spec is_registered( registration_name() ) -> pid() | 'not_registered'.
is_registered( Name ) ->
	is_registered( Name, _RegistrationScope=local_otherwise_global ).



-spec is_registered( registration_name(), look_up_scope() ) ->
						pid() | 'not_registered'.
is_registered( Name, _LookUpScope=global ) ->

	case global:whereis_name( Name ) of

		undefined ->
			not_registered ;

		Pid ->
			Pid

	end;


is_registered( Name, _LookUpScope=local ) ->

	case erlang:whereis( Name ) of

		undefined ->
			not_registered;

		Pid ->
			Pid

	end;



% Returns a PID iff both local and global look-ups returns a PID, and the same
% one.
%
is_registered( Name, _LookUpScope=local_and_global ) ->

	case is_registered( Name, local ) of

		not_registered ->
			not_registered;

		Pid ->

			case is_registered( Name, global ) of

				% Already bound!
				Pid ->
					Pid;

				not_registered ->
					not_registered

			end

	end;


is_registered( Name, _LookUpScope=local_otherwise_global ) ->

	case is_registered( Name, local ) of

		not_registered ->
			is_registered( Name, global );

		Pid ->
			Pid

	end;

% Normally, 'local_only', 'global_only' and 'none' should only be specified for
% registration (not for looking-up); nevertheless the following clauses allow to
% use the same parameter for reading as for registration, even if we do not know
% which.
%
% So that the atom used for registration can be used for look-up as well,
% notably in static methods (see the registration_scope defines).
%
is_registered( Name, _LookUpScope=local_only ) ->
	is_registered( Name, local );

is_registered( Name, _LookUpScope=global_only ) ->
	is_registered( Name, global ).



% Waits (up to a few seconds) until specified name is registered, within
% specified scope.
%
% Returns the resolved PID, or throws an exception.
%
-spec wait_for_registration_of( registration_name(), look_up_scope() ) -> pid().
wait_for_registration_of( Name, _LookUpScope=global ) ->
	wait_for_global_registration_of( Name );

wait_for_registration_of( Name, _LookUpScope=local ) ->
	wait_for_local_registration_of( Name );

wait_for_registration_of( Name, _LookUpScope=local_and_global ) ->
	% Then we go for the cheapest, the local one:
	wait_for_local_registration_of( Name );

wait_for_registration_of( Name, _LookUpScope=local_otherwise_global ) ->
	try
		wait_for_local_registration_of( Name )
	catch _ ->
			trace_utils:debug_fmt( "Time-out when waiting for a local "
				"registration of '~ts', switching to a global look-up.",
				[ Name ] ),
			wait_for_global_registration_of( Name )
	end;

wait_for_registration_of( Name, _LookUpScope=none ) ->
	throw( { no_look_up_scope_for, Name } );

wait_for_registration_of( Name, InvalidLookUpScope ) ->
	% Probably a registration one:
	throw( { invalid_lookup_scope, InvalidLookUpScope, Name } ).



% Waits (up to 10 seconds) until specified name is globally registered.
%
% Returns the resolved PID, or throws a {registration_waiting_timeout, Scope,
% Name} exception.
%
-spec wait_for_global_registration_of( registration_name() ) -> pid().
wait_for_global_registration_of( Name ) ->
	wait_for_global_registration_of( Name, _Seconds=10 ).


% (helper)
wait_for_global_registration_of( Name, _Seconds=0 ) ->
	throw( { registration_waiting_timeout, Name, global } );

wait_for_global_registration_of( Name, SecondsToWait ) ->
	case global:whereis_name( Name ) of

		undefined ->
			timer:sleep( 1000 ),
			wait_for_global_registration_of( Name, SecondsToWait-1 );

		Pid ->
			Pid

	end.




% Waits (up to 5 seconds) until specified name is locally registered.
%
% Returns the resolved PID, or throws {registration_waiting_timeout, Scope,
% Name}.
%
-spec wait_for_local_registration_of( registration_name() ) -> pid() | port().
wait_for_local_registration_of( Name ) ->
	wait_for_local_registration_of( Name , _Seconds=5 ).


wait_for_local_registration_of( Name, _Seconds=0 ) ->
	throw( { registration_waiting_timeout, Name, local } );

wait_for_local_registration_of( Name, SecondsToWait ) ->

	case erlang:whereis( Name ) of

		undefined ->
			timer:sleep( 1000 ),
			wait_for_local_registration_of( Name, SecondsToWait-1 );

		Pid ->
			Pid

	end.



% Waits for specified name RegisteredName (an atom) to be locally registered on
% all specified nodes before returning.
%
% A time-out is triggered if the waited duration exceeds 10 seconds.
%
-spec wait_for_remote_local_registrations_of( registration_name(),
											  [ atom_node_name() ] ) -> void().
wait_for_remote_local_registrations_of( RegisteredName, Nodes ) ->

	% Up to 10 seconds, 0.5 seconds of waiting between two, thus 20 attempts:
	RemainingAttempts = round( 10 / 0.5 ),

	wait_for_remote_local_registrations_of( RegisteredName, Nodes,
											RemainingAttempts ).


% Helper function.
wait_for_remote_local_registrations_of( RegisteredName, Nodes,
										_RemainingAttempts=0 ) ->
	throw( { time_out_while_waiting_remote_registration, local, RegisteredName,
			 Nodes } );

wait_for_remote_local_registrations_of( RegisteredName, Nodes,
										RemainingAttempts ) ->

	{ ResList, BadNodes } = rpc:multicall( Nodes, erlang, whereis,
										   [ RegisteredName  ], _Timeout=2000 ),

	case BadNodes of

		[] ->
			ok;

		_ ->
			throw( { bad_nodes_while_waiting_remote_registration, local,
					 RegisteredName, BadNodes } )

	end,

	case lists:member( undefined, ResList ) of

		true ->

			% Happens regularly on some settings:
			%trace_utils:debug_fmt( "wait_for_remote_local_registrations_of: "
			%   "for ~p, retry needed.", [ Nodes ] ),

			% At least one node not ready (we do not know which one), waiting a
			% bit for it:
			%
			timer:sleep( 500 ),
			wait_for_remote_local_registrations_of( RegisteredName, Nodes,
													RemainingAttempts - 1 );

		false ->
			ok

	end.



% Displays registered processes.
-spec display_registered() -> void().
display_registered() ->

	io:format( "On a total of ~B existing processes on node '~ts':~n",
			   [ length( processes() ), node() ] ),

	case global:registered_names() of

		[] ->
			io:format( " - no process is globally-registered~n" );

		Globals ->
			io:format( " - ~B processes are globally-registered:~n~p~n",
					   [ length( Globals ), Globals ] )

	end,

	case registered() of

		[] ->
			io:format( " - no process is locally-registered~n" );

		Locals ->
			io:format( " - ~B processes are locally-registered:~n~p~n",
					   [ length( Locals ), Locals ] )

	end.



% Converts a registration scope into a look-up one.
%
% Note: only legit for a subset of the registration scopes, otherwise a case
% clause is triggered.
%
% (helper)
%
-spec registration_to_look_up_scope( registration_scope() ) -> look_up_scope().
registration_to_look_up_scope( _Scope=global_only ) ->
	global;

registration_to_look_up_scope( _Scope=local_only ) ->
	local;

registration_to_look_up_scope( _Scope=local_and_global ) ->
	local_and_global.
