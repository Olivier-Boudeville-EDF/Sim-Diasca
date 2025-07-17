% Copyright (C) 2007-2025 Olivier Boudeville
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

-module(naming_utils).

-moduledoc """
Gathering of various facilities about **naming services** (local and
global).

See `naming_utils_test.erl` for the corresponding test.
""".



% Registration functions.
-export([ register_as/2, register_as/3, register_or_return_registered/2,
		  unregister/2,

		  vet_registration_name/1, check_registration_name/1,
          vet_registration_scope/1, check_registration_scope/1 ]).



% Lookup-related functions.
-export([ vet_lookup_scope/1, check_lookup_scope/1,

		  registration_to_lookup_scope/1,

		  get_registered_pid_for/1, get_maybe_registered_pid_for/1,
          get_registered_pid_for/2, get_maybe_registered_pid_for/2,

		  get_locally_registered_pid_for/2,
          get_maybe_locally_registered_pid_for/2,

          get_registered_pid_from/2, get_maybe_registered_pid_from/2,

		  get_registered_names/1,

		  is_registered/1, is_registered/2,
		  wait_for_registration_of/2,
		  wait_for_global_registration_of/1, wait_for_global_registration_of/2,
		  wait_for_local_registration_of/1, wait_for_local_registration_of/2,
		  wait_for_remote_local_registrations_of/2,
		  display_registered/0, lookup_info_to_string/1 ]).




-doc "Necessarily an atom.".
-type registration_name() :: atom().



-doc """
The two ways according to which a locally-registered process can be designated:
either directly thanks to its PID or to the name under which it is locally
registered.
""".
-type local_designator() :: pid() | registration_name().



-doc """
Tells where/how a given process shall be registered.

Not to be mixed up with a look-up scope.
""".
-type registration_scope() :: 'global_only' % Registers only globally.
							| 'local_only'  % Registers only locally.
							| 'local_and_global' % Registers on both scopes.
							| 'none'. % Do not register at all.



-doc """
Tells how a given process shall be looked-up.

Not to be mixed up with a registration scope.
""".
-type lookup_scope() ::
    'global' % Expected to be only globally registered.

  | 'local'  % Only locally registered (on the current node, if not told
             % otherwise).

  | 'local_and_global' % Registered in both (free choice).
  | 'local_otherwise_global'  % First possible order.
  | 'global_otherwise_local'. % Second order.


-doc "The information sufficient to look up a registered process.".
-type lookup_info() :: { registration_name(), lookup_scope() }.


-export_type([ registration_name/0, local_designator/0,
			   registration_scope/0, lookup_scope/0, lookup_info/0 ]).



% Registration information.
%
% Note that:
% - only local processes can be registered locally
% - a given PID cannot be registered globally under more than one name
% - if a registered process terminates (for any of the two scopes), its name is
% automatically unregistered (see
% http://erlang.org/doc/reference_manual/processes.html and
% http://erlang.org/doc/man/global.html)

% - designating a process by its PID is certainly the most effective approach;
% designating it by a name certainly involves a round-trip communication to
% a process in charge of the naming service (two messages), yet provides
% interesting features:
%
%   * such designated process becomes a singleton by design (either on the local
%   node or globally)
%
%   * if the designated process offers a service, this additional level of
%   indirection allows this process to be transparently restarted / upgraded



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type atom_node_name() :: net_utils:atom_node_name().



-doc """
Registers the current process under the specified name and scope.

Various kinds of registrations can be requested, depending on the targeted
visibility and the possibilities in terms of desired multiplicities (at any
scope, up to one process can register a given name).

Throws an exception on failure (e.g. if that name is already registered).
""".
-spec register_as( registration_name(), registration_scope() ) -> void().
register_as( RegName, RegScope ) ->
	register_as( self(), RegName, RegScope ).



-doc """
Registers the specified process under the specified name and scope.

Various kinds of registrations can be requested, depending on the targeted
visibility and the possibilities in terms of desired multiplicities (at any
scope, up to one process can register a given name).

Throws an exception on failure.
""".
-spec register_as( pid(), registration_name(), registration_scope() ) -> void().
register_as( Pid, RegName, local_only ) when is_atom( RegName ) ->

	%trace_utils:debug_fmt( "register_as: local_only, "
	%                       "with PID=~w and RegName='~p'.", [ Pid, RegName ] ),

	try erlang:register( RegName, Pid ) of

		true ->
			ok

	catch

		error:badarg ->

			case is_registered( RegName, _RegScope=local ) of

				% No more information obtained:
				not_registered ->
					throw( { local_registration_failed, RegName,
							 { error, badarg } } );

				Pid ->
					throw( { local_registration_failed, RegName,
							 already_registered, Pid } );

				RegPid ->
					throw( { local_registration_failed, RegName,
							 already_registered, { Pid, RegPid } } )

			end;

		ExceptionType:Exception ->
			throw( { local_registration_failed, RegName,
					 { ExceptionType, Exception } } )

	end;


register_as( Pid, RegName, global_only ) when is_atom( RegName ) ->

	%trace_utils:debug_fmt( "register_as: global_only, with PID=~w "
	%                       "and RegName='~p'.", [ Pid, RegName ] ),

	global:register_name( RegName, Pid ) =:= yes orelse
		throw( { global_registration_failed, RegName } );

register_as( Pid, RegName, local_and_global ) when is_atom( RegName ) ->
	register_as( Pid, RegName, local_only ),
	register_as( Pid, RegName, global_only );

register_as( _Pid, _RegName, none ) ->
	ok;

register_as( _Pid, RegName, Other ) when is_atom( RegName ) ->
	throw( { invalid_registration_scope, Other } );

register_as( _Pid, RegName, _Other ) ->
	throw( { invalid_type_for_name, RegName } ).



-doc """
Registers the specified PID under specified name (which must be an atom) and
scope (only `local_only` and `global_only` registration scopes permitted), and
returns `registered`, or returns the PID of any process already registered.

This is an atomic operation, which is not meant to fail.

Allows for example a series of non-synchronised processes to all attempt to
register: the first will succeed, all the others will get its PID, none will
fail.
""".
-spec register_or_return_registered( registration_name(),
	'global_only' | 'local_only' ) -> 'registered' | pid().
register_or_return_registered( RegName, RegScope ) when is_atom( RegName ) ->

	% Minor annoyance: we ensured that looking up a process relied generally on
	% a different atom than registering it (e.g. 'global' vs 'global_only').
	%
	% Here, we expect the user to specify a registration atom; we need to
	% convert it for look-up purposes:
	%
	LookUpScope = registration_to_lookup_scope( RegScope ),

	case is_registered( RegName, LookUpScope ) of

		not_registered ->

			try
				register_as( RegName, RegScope ),
				registered

			catch

				throw:_ ->
					% Another process must have registered in-between, let's
					% restart:
					%
					% (a small random waiting could be added here)
					%
					register_or_return_registered( RegName, RegScope )

			end;


		Pid ->
			Pid

	end.



-doc """
Unregisters the specified name from the specified registry.

Throws an exception in case of failure.

Note: when a process terminates, it unregisters its name (if any) automatically
from all scopes.
""".
-spec unregister( registration_name(), registration_scope() ) -> void().
unregister( RegName, local_only ) ->

	try erlang:unregister( RegName ) of

		true ->
			ok

	catch

		ExceptionType:Exception ->
			throw( { local_unregistration_failed, RegName,
						{ ExceptionType, Exception } } )

	end;

unregister( RegName, global_only ) ->
	% Documentation says it returns "void" (actually 'ok'):
	try

		global:unregister_name( RegName )

	catch

		ExceptionType:Exception ->
			throw( { global_unregistration_failed, RegName,
					 { ExceptionType, Exception } } )

	end;

unregister( RegName, local_and_global ) ->
	unregister( RegName, local_only ),
	unregister( RegName, global_only );

unregister( _RegName, none ) ->
	ok.



-doc "Tells whether the specified term is a legit registration name.".
-spec vet_registration_name( term() ) -> boolean().
vet_registration_name( RegName ) when is_atom( RegName ) ->
	true;

vet_registration_name( _Other ) ->
	false.


-doc "Checks that the specified term is a legit registration name.".
-spec check_registration_name( term() ) -> void().
check_registration_name( Term ) ->
    vet_registration_name( Term ) orelse
        throw( { invalid_registration_name, Term } ).


-doc "Tells whether the specified term is a legit registration scope.".
-spec vet_registration_scope( term() ) -> boolean().
vet_registration_scope( global_only ) ->
	true;

vet_registration_scope( local_only ) ->
	true;

vet_registration_scope( local_and_global ) ->
	true;

vet_registration_scope( none ) ->
	true;

vet_registration_scope( _Other ) ->
	false.



-doc "Checks that the specified term is a legit registration scope.".
-spec check_registration_scope( term() ) -> void().
check_registration_scope( Term ) ->
    vet_registration_scope( Term ) orelse
        throw( { invalid_registration_scope, Term } ).



-doc "Tells whether the specified term is a legit lookup scope.".
-spec vet_lookup_scope( term() ) -> boolean().
vet_lookup_scope( global ) ->
	true;

vet_lookup_scope( local ) ->
	true;

vet_lookup_scope( local_and_global ) ->
	true;

vet_lookup_scope( local_otherwise_global ) ->
	true;

vet_lookup_scope( global_otherwise_local ) ->
	true;

vet_lookup_scope( _Other ) ->
	false.


-doc "Checks that the specified term is a legit lookup scope.".
-spec check_lookup_scope( term() ) -> void().
check_lookup_scope( Term ) ->
    vet_lookup_scope( Term ) orelse
        throw( { invalid_registration_lookup_scope, Term } ).



-doc """
Converts a registration scope into a lookup one.

Note: only legit for a subset of the registration scopes, otherwise a case
clause is triggered.
""".
-spec registration_to_lookup_scope( registration_scope() ) -> lookup_scope().
registration_to_lookup_scope( _RegScope=global_only ) ->
	global;

registration_to_lookup_scope( _RegScope=local_only ) ->
	local;

registration_to_lookup_scope( _RegScope=local_and_global ) ->
	local_and_global;

registration_to_lookup_scope( _RegScope=none ) ->
	throw( no_possible_lookup_for_none ).



% Lookup based on arity one:


-doc """
Returns any corresponding PID that should be already registered, either resolved
from the specified look-up information or from the specified registration name,
in which case a local registering will be tried first, and if not found a global
one will be tried then.

No specific waiting for registration will be performed, see
`wait_for_*_registration_of` instead.
""".
-spec get_maybe_registered_pid_for( lookup_info() | registration_name() ) ->
                                                option( pid() ).
get_maybe_registered_pid_for( _LookUpInfo={ RegName, LookupScope } ) ->
     get_maybe_registered_pid_for( RegName, LookupScope );

get_maybe_registered_pid_for( RegName ) ->
	get_maybe_registered_pid_for( RegName,
                                  _LookupScope=local_otherwise_global ).



-doc """
Returns the PID that should be already registered, either resolved from the
specified look-up information or from the specified registration name, in which
case a local registering will be tried first, and if not found a global one will
be tried then.

Throws an exception on failure.

No specific waiting for registration will be performed, see
`wait_for_*_registration_of` instead.
""".
-spec get_registered_pid_for( lookup_info() | registration_name() ) -> pid().
get_registered_pid_for( _LookUpInfo={ RegName, LookupScope } ) ->
    get_registered_pid_for( RegName, LookupScope );

get_registered_pid_for( RegName ) ->
	get_registered_pid_for( RegName, _LookupScope=local_otherwise_global ).



% Lookup based on arity two:


-doc """
Returns any PID that should be already registered, as specified name, at the
specified scope.

No specific waiting for registration will be performed, see
`wait_for_*_registration_of` instead.
""".
-spec get_maybe_registered_pid_for( registration_name(), lookup_scope() ) ->
                                                option( pid() ).
get_maybe_registered_pid_for( RegName, _LookupScope=global ) ->
	global:whereis_name( RegName );

get_maybe_registered_pid_for( RegName, _LookupScope=local ) ->
	erlang:whereis( RegName );

get_maybe_registered_pid_for( RegName, _LookupScope=local_and_global ) ->
    % We have the choice, and local must be cheaper:
    get_maybe_registered_pid_for( RegName, local );

get_maybe_registered_pid_for( RegName, _LookupScope=local_otherwise_global ) ->
    case get_maybe_registered_pid_for( RegName, local ) of

        undefined ->
            get_maybe_registered_pid_for( RegName, global );

        Pid ->
            Pid

    end;

get_maybe_registered_pid_for( RegName, _LookupScope=global_otherwise_local ) ->
    case get_maybe_registered_pid_for( RegName, global ) of

        undefined ->
            get_maybe_registered_pid_for( RegName, local );

        Pid ->
            Pid

    end.



-doc """
Returns the PID that should be already registered, as specified name, at
specified scope.

Throws an exception on failure.

No specific waiting for registration will be performed, see
`wait_for_*_registration_of` instead.
""".
-spec get_registered_pid_for( registration_name(), lookup_scope() ) -> pid().
get_registered_pid_for( RegName, LookupScope ) ->
    case get_maybe_registered_pid_for( RegName, LookupScope ) of

        undefined ->
            throw( { registration_lookup_failed, RegName, LookupScope } );

        Pid ->
            Pid

    end.



% Lookup relative to another node:


-doc """
Returns any PID of a process corresponding to the specified local name on the
specified node: that process is expected to be locally registered on that
specified node.
""".
-spec get_maybe_locally_registered_pid_for( registration_name(),
                                atom_node_name() ) -> option( pid() ).
get_maybe_locally_registered_pid_for( RegName, TargetNode ) ->

	case rpc:call( TargetNode, _Mod=erlang, _Fun=whereis, _Args=[ RegName ] ) of

		{ badrpc, _Reason } ->

            %trace_utils:format( "No process found registered locally as '~ts' "
            %    "on node '~ts'; reason: ~w.",
            %    [ RegName, TargetNode, Reason ] ),

            undefined;

        % option(pid()):
		Res  ->
			Res

	end.



-doc """
Returns the PID of the process corresponding to the specified local name on the
specified node: that process is expected to be locally registered on that
specified node (which thus may not be the current one).

Throws an exception on failure.
""".
-spec get_locally_registered_pid_for( registration_name(), atom_node_name() ) ->
											pid().
get_locally_registered_pid_for( RegName, TargetNode ) ->
    case get_maybe_locally_registered_pid_for( RegName, TargetNode ) of

        undefined ->
            throw( { not_registered_locally, RegName, TargetNode } );

        Pid ->
            Pid

    end.




% Most general form of lookup (with name and scope, relative to another node):


-doc """
Returns any PID that should be already registered, as resolved from the
specified look-up information, to be evaluated (regarding local registration)
relatively to the specified node.

The point is that the target process may be locally-registered, but on a remote
node.
""".
-spec get_maybe_registered_pid_from( lookup_info(), atom_node_name() ) ->
                                                option( pid() ).
get_maybe_registered_pid_from( _LookUpInfo={ RegName, LookupScope=global },
                               _TargetNode ) ->
    get_maybe_registered_pid_for( RegName, LookupScope );

get_maybe_registered_pid_from( _LookUpInfo={ RegName, _LookupScope=local },
                              TargetNode ) ->
    get_maybe_locally_registered_pid_for( RegName, TargetNode );

get_maybe_registered_pid_from(
        _LookUpInfo={ RegName, _LookupScope=local_and_global }, _TargetNode ) ->
    % Open choice; presumably cheaper:
    get_maybe_registered_pid_for( RegName, global );

get_maybe_registered_pid_from(
        _LookUpInfo={ RegName, _LookupScope=local_otherwise_global },
        TargetNode ) ->

    case get_maybe_locally_registered_pid_for( RegName, TargetNode ) of

        undefined ->
            get_maybe_registered_pid_for( RegName, global );

        Pid ->
            Pid

    end;

get_maybe_registered_pid_from(
        _LookUpInfo={ RegName, _LookupScope=global_otherwise_local },
        TargetNode ) ->

    case get_maybe_registered_pid_for( RegName, global ) of

        undefined ->
            get_maybe_locally_registered_pid_for( RegName, TargetNode );

        Pid ->
            Pid

    end.



-doc """
Returns the PID that should be already registered, as resolved from the
specified look-up information, to be evaluated (regarding local registration)
relatively to the specified node.

Throws an exception on failure.

No specific waiting for registration will be performed, see
`wait_for_*_registration_of` instead.
""".
-spec get_registered_pid_from( lookup_info(), atom_node_name() ) -> pid().
get_registered_pid_from( LookUpInfo, TargetNode ) ->
    case get_maybe_registered_pid_from( LookUpInfo, TargetNode ) of

        undefined ->
            throw( { registration_lookup_failed_from, LookUpInfo,
                     TargetNode } );

         Pid ->
            Pid

    end.



-doc """
Returns a list of the names of the registered processes, for specified global or
local look-up scope.
""".
-spec get_registered_names( lookup_scope() ) -> [ registration_name() ].
% Preferring not matching other look-up scopes:
get_registered_names( _LookUpScope=global ) ->
	global:registered_names();

get_registered_names( _LookUpScope=local ) ->
	erlang:registered().



-doc """
Tells whether specified name is registered in the specified local/global
context: if no, returns the `not_registered` atom, otherwise returns the
corresponding PID.

Local registering will be requested first, if not found global one will be
tried.

No specific waiting for registration will be performed, see
`wait_for_*_registration_of` instead.
""".
-spec is_registered( registration_name() ) -> pid() | 'not_registered'.
is_registered( Name ) ->
	is_registered( Name, _RegScope=local_otherwise_global ).



-doc """
Tells whether specified name is registered in the specified scope: if no,
returns the `not_registered` atom, otherwise returns the corresponding PID.

No specific waiting for registration will be performed, see
`wait_for_*_registration_of` instead.
""".
-spec is_registered( registration_name(), lookup_scope() ) ->
						pid() | 'not_registered'.
is_registered( Name, _LookUpScope=global ) ->

	case global:whereis_name( Name ) of

		undefined ->
			cond_utils:if_defined( myriad_debug_registration,
				trace_utils:debug_fmt( "The name '~ts' is not globally "
					"registered; the ones that are are:~n  ~p",
					[ Name, get_registered_names( global ) ] ) ),

			not_registered ;

		Pid ->
			Pid

	end;


is_registered( Name, _LookUpScope=local ) ->

	case erlang:whereis( Name ) of

		undefined ->
			cond_utils:if_defined( myriad_debug_registration,
				trace_utils:debug_fmt( "The name '~ts' is not locally "
					"registered; the ones that are are:~n  ~p",
					[ Name, get_registered_names( local ) ] ) ),

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
			cond_utils:if_defined( myriad_debug_registration,
				trace_utils:debug_fmt( "First, the name '~ts' is not locally "
					"registered; the ones that are are:~n  ~p",
					[ Name, get_registered_names( local ) ] ) ),

			not_registered;

		Pid ->
			case is_registered( Name, global ) of

				% Already bound!
				Pid ->
					Pid;

				not_registered ->
					cond_utils:if_defined( myriad_debug_registration,
						trace_utils:debug_fmt( "Second, the name '~ts' is not "
							"globally registered; the ones that are are:~n  ~p",
							[ Name, get_registered_names( global ) ] ) ),

					not_registered;

				OtherPid ->
					cond_utils:if_defined( myriad_debug_registration,
						trace_utils:warning_fmt( "The name '~ts' is "
							"globally registered, but to a PID (~w) different "
							"from the local one (~w), hence considered NOT "
							"registered.", [ Name, OtherPid, Pid ] ),
						basic_utils:ignore_unused( OtherPid	) ),

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

is_registered( Name, _LookUpScope=global_otherwise_local ) ->

	case is_registered( Name, global ) of

		not_registered ->
			is_registered( Name, local );

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



-doc """
Waits (up to a few seconds) until specified name is registered, within specified
scope.

Returns the resolved PID, or throws an exception.
""".
-spec wait_for_registration_of( registration_name(), lookup_scope() ) -> pid().
wait_for_registration_of( Name, _LookUpScope=global ) ->
	wait_for_global_registration_of( Name );

wait_for_registration_of( Name, _LookUpScope=local ) ->
	wait_for_local_registration_of( Name );

wait_for_registration_of( Name, _LookUpScope=local_and_global ) ->
	% Then we go for the cheapest, the local one:
	wait_for_local_registration_of( Name );

wait_for_registration_of( Name, _LookUpScope=local_otherwise_global ) ->
	wait_for_local_otherwise_global_registration_of( Name );

wait_for_registration_of( Name, _LookUpScope=global_otherwise_local ) ->
	wait_for_global_otherwise_local_registration_of( Name );

wait_for_registration_of( Name, _LookUpScope=none ) ->
	throw( { no_lookup_scope_for, Name } );

wait_for_registration_of( Name, InvalidLookUpScope ) ->
	% Probably a registration one:
	throw( { invalid_lookup_scope, InvalidLookUpScope, Name } ).



-doc """
Waits (up to 10 seconds) until specified name is globally registered.

Returns the resolved PID, or throws a `{registration_waiting_timeout, RegScope,
Name}` exception.
""".
-spec wait_for_global_registration_of( registration_name() ) -> pid().
wait_for_global_registration_of( Name ) ->
	wait_for_global_registration_of( Name, _Seconds=10 ).



-doc """
Waits (up to to the specified number of seconds) until specified name is
globally registered.

Returns the resolved PID, or throws a `{registration_waiting_timeout, RegScope,
Name}` exception.
""".
wait_for_global_registration_of( Name, _Seconds=0 ) ->

	cond_utils:if_defined( myriad_debug_registration,
		trace_utils:error_fmt( "Global registration of '~ts' timed-out; "
			"globally registered processes: ~w",
			[ Name, lists:sort(
				get_registered_names( _LookUpScope=global ) ) ] ) ),

	throw( { registration_waiting_timeout, Name, global } );

wait_for_global_registration_of( Name, SecondsToWait ) ->
	case global:whereis_name( Name ) of

		undefined ->
			timer:sleep( 1000 ),
			wait_for_global_registration_of( Name, SecondsToWait-1 );

		Pid ->
			Pid

	end.



-doc """
Waits (up to 5 seconds) until specified name is locally registered.

Returns the resolved PID, or throws `{registration_waiting_timeout, RegScope,
Name}`.
""".
-spec wait_for_local_registration_of( registration_name() ) -> pid() | port().
wait_for_local_registration_of( Name ) ->
	wait_for_local_registration_of( Name, _Seconds=5 ).



-doc """
Waits (up to the specified number of seconds) until specified name is locally
registered.

Returns the resolved PID, or throws `{registration_waiting_timeout, RegScope,
Name}`.
""".
wait_for_local_registration_of( Name, _Seconds=0 ) ->

	cond_utils:if_defined( myriad_debug_registration,
		trace_utils:error_fmt( "Local registration of '~ts' timed-out; "
			"locally registered processes: ~w",
			[ Name, lists:sort(
				get_registered_names( _LookUpScope=local ) ) ] ) ),

	throw( { registration_waiting_timeout, Name, local } );

wait_for_local_registration_of( Name, SecondsToWait ) ->

	case erlang:whereis( Name ) of

		undefined ->
			timer:sleep( 1000 ),
			wait_for_local_registration_of( Name, SecondsToWait-1 );

		Pid ->
			Pid

	end.



-doc """
Waits (up to 10 seconds) until specified name is locally, otherwise globally,
registered.

Returns the resolved PID, or throws a `{registration_waiting_timeout, RegScope,
Name}` exception.
""".
-spec wait_for_local_otherwise_global_registration_of( registration_name() ) ->
																pid().
wait_for_local_otherwise_global_registration_of( Name ) ->
	wait_for_local_otherwise_global_registration_of( Name, _Seconds=10 ).



-doc """
Waits (up to to the specified number of seconds) until the specified name is
locally, otherwise globally registered.

Returns the resolved PID, or throws a `{registration_waiting_timeout, RegScope,
Name}` exception.
""".
wait_for_local_otherwise_global_registration_of( Name, _Seconds=0 ) ->
	throw( { registration_waiting_timeout, Name, local_otherwise_global } );

wait_for_local_otherwise_global_registration_of( Name, SecondsToWait ) ->
	case erlang:whereis( Name ) of

		undefined ->
			case global:whereis_name( Name ) of

				undefined ->
					timer:sleep( 1000 ),
					wait_for_local_otherwise_global_registration_of( Name,
						SecondsToWait-1 );

				Pid ->
					Pid

			end;

		Pid ->
			Pid

	end.



-doc """
Waits (up to 10 seconds) until specified name is globally, otherwise locally,
registered.

Returns the resolved PID, or throws a `{registration_waiting_timeout, Scope,
Name}` exception.
""".
-spec wait_for_global_otherwise_local_registration_of( registration_name() ) ->
																pid().
wait_for_global_otherwise_local_registration_of( Name ) ->
	wait_for_global_otherwise_local_registration_of( Name, _Seconds=10 ).



-doc """
Waits (up to to the specified number of seconds) until specified name is
globally, otherwise locally registered.

Returns the resolved PID, or throws a `{registration_waiting_timeout, RegScope,
Name}` exception.
""".
wait_for_global_otherwise_local_registration_of( Name, _Seconds=0 ) ->
	throw( { registration_waiting_timeout, Name, global_otherwise_local } );

wait_for_global_otherwise_local_registration_of( Name, SecondsToWait ) ->
	case global:whereis_name( Name ) of

		undefined ->
			case erlang:whereis( Name ) of

				undefined ->
					timer:sleep( 1000 ),
					wait_for_global_otherwise_local_registration_of( Name,
						SecondsToWait-1 );

				Pid ->
					Pid

			end;

		Pid ->
			Pid

	end.



-doc """
Waits for specified name RegisteredName (an atom) to be locally registered on
all specified nodes before returning.

A time-out is triggered if the waited duration exceeds 10 seconds.
""".
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

	BadNodes =/= [] andalso
			throw( { bad_nodes_while_waiting_remote_registration, local,
					 RegisteredName, BadNodes } ),

	lists:member( undefined, ResList ) andalso
		begin

			% Happens regularly on some settings:
			%trace_utils:debug_fmt( "wait_for_remote_local_registrations_of: "
			%   "for ~p, retry needed.", [ Nodes ] ),

			% At least one node not ready (we do not know which one), waiting a
			% bit for it:
			%
			timer:sleep( 500 ),
			wait_for_remote_local_registrations_of( RegisteredName, Nodes,
													RemainingAttempts - 1 )

		end.



-doc "Displays the currently registered processes.".
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



-doc "Returns a textual representation of the specified lookup information.".
-spec lookup_info_to_string( lookup_info() ) -> ustring().
lookup_info_to_string( _LookUpInfo={ RegName, LookupScope } ) ->
    text_utils:format( "~ts lookup of registration name '~ts'",
                       [ LookupScope, RegName ] ).
