% Copyright (C) 2022-2023 Olivier Boudeville
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
% Creation date: Sunday, February 27, 2022.


% @doc Service dedicated to the <b>management of application environments</b>.
%
% An application environment is a server-like process that stores static or
% dynamic information (possibly initialised from an ETF file), as key/value
% entries (not unlike an ETS table), on behalf of an application or of a subset
% of its components, and makes it available to client processes.
%
% An environment entry is designated by a key (an atom), associated to a value
% (that can be any term) in a pair.
%
% Environments hold application-specific or component-specific data, obtained
% from any source (file included); they may also start blank and be exclusively
% fed at runtime by the application or the components. Environments are used
% afterwards to maintain these pieces of data (read/write), before possibly
% storing them on file at application exit or component stop.
%
% As a whole, an environment server can be seen as a process holding state
% information meant to be potentially common to various processes of a given
% application or component.
%
% Environment data can be read from or written to file(s) in the ETF format,
% hence as a series of Erlang terms written as strings, each ending with a dot
% (i.e. it is the basic, standard format understood by `file:consult/1').
%
% Example of content of an environment file:
% ```
% {my_first_color, red}.
% {myHeight, 1.80}.
% {'My name', "Sylvester the cat"}.
% '''
%
% The server process corresponding to an environment is locally registered; as a
% consequence it can be designated either directly through its PID or through
% its conventional (atom) registration name (like 'my_foobar_env_server' in
% `environment:get(my_first_color, my_foobar_env_server'). No specific global
% registration of servers is made.
%
% A (single) explicit start (with one of the start* functions) shall be
% preferred to implicit ones (typically triggered thanks to the get* functions)
% to avoid any risk of race conditions (should multiple processes attempt
% concurrently to create the same environment server), and also to be able to
% request that the server is also linked to the calling process.
%
% See our 'preferences' module, corresponding to the user preferences, which is
% implemented as a specific case of environment.
%
% See also the (unrelated) resource module for the sharing of any kind of data
% resource.
%
-module(environment).


% Designating environments:
%
% An environment server can be designated either directly through its PID or
% through its conventional (atom) registration name (potentially deriving from
% its associated filename). The former approach is a bit more effective, but
% the later one is more robust (the server can be transparently
% restarted/upgraded).
%
% An environment may also be designated from any filename that it uses.


% About the caching of environment entries:

% For faster accesses (not involving any inter-process messaging), and if
% considering that their changes are rather infrequent (or even never
% happening), at least some entries managed by an environment server may be
% cached directly in any client process of choice.
%
% In this case, the process dictionary of these clients is used to store the
% cached entries, and when updating a cached key from a client process the
% corresponding environment server is updated in turn. However note that any
% other client process caching that key will not be aware of this change until
% it explicitly requests an update to this environment server (as such servers
% do not keep track of their clients).
%
% So a client process should cache a key mainly if no other is expected to
% update that key, i.e. typically if the associated value is const, or if this
% process is considered as the "owner" (sole controller) of that key (or if some
% other organisation ensures, possibly thanks to sync/1, that its cache is kept
% consistent with the corresponding environment server).
%
% As soon as a key is declared to be cached, its value has to be set in the
% cache; there is thus always an actual value associated to a cached key
% (i.e. it is never a maybe-value because of the cache), and thus cached values
% are allowed to be set to 'undefined'.
%
% Two ways of setting values are provided:
%  - regular set/{2,3,4}, where new values are unconditionally assigned
%  - set_cond/{2,3,4}, where each new value is compared to any currently cached
%  one, and sent to the environment iff not matching
%
% This last option allows, for the cost of an extra comparison, to potentially
% prevent useless message sendings to the environment server.
%
% Multiple environments may be used concurrently. A specific case of environment
% corresponds to the user preferences. See our preferences module for that.
%
% Refer to https://myriad.esperide.org/#etf for more information.


% About the API:
%
% The API hides the actual messaging taking place between the helper executed by
% the caller and the environment server.
%
% We rely here on a custom (ad hoc) protocol, rather than following WOOPER
% message conventions.


-export([ start/1, start/2, start_link/1, start_link/2,
		  start_with_defaults/2, start_link_with_defaults/2,
		  start_cached/2, start_link_cached/2,

		  get_server/1, get_server_info/1, wait_available/1,

		  get/2, get/3,
		  set/2, set/3, set/4, set_cond/2, set_cond/3, set_cond/4,
		  update_from_etf/2,
		  remove/2, extract/2,
		  cache/2, cache_return/2,
		  uncache/1, uncache/0, sync/1,
		  ensure_binary/2,
		  store/1, store/2,
		  to_string/1, to_bin_string/1, to_string/0,
		  stop/1 ]).


-type env_reg_name() :: registration_name().
% The name under which an environment server can be (locally) registered.

-type env_pid() :: pid().
% The PID of an environment server, the most direct reference to an environment.


-type env_info() :: { env_reg_name(), env_pid() }.
% The full reference to an environment.
%
% The registration name is needed as a key of any local cache, and the PID
% allows to spare extra naming look-ups.


-type env_designator() :: env_pid() | env_reg_name().
% The two standard ways according to which an environment server can be
% designated: either directly thanks to its PID or to the name under which it is
% locally registered.


-type env_data() :: env_info() | env_designator().
% Any element designating an environment (most general handle).


-type cache_info() :: { env_reg_name(), env_cache_table() }.
% Information stored regarding an environment, typically to be cached in the
% process dictionary in an all_env_table() table, and indexed by the PID of the
% corresponding environment server.
%
% Specifying the registration name is required for caching.



-type key() :: atom().


-type value() :: table:value().
% Can be 'undefined' (no difference between a non-registered key and a key
% registered to 'undefined').


-type entry() :: table:entry().
% An entry is a key/value pair.

-type entries() :: table:entries().
% Entries are lists of entry/0 terms, i.e. lists of pairs.

-type cache_spec() :: maybe_list( [ key() | entry() ] ).
% A specification of the environment keys (possibly with their initial values)
% that shall be cached in a client process (hence locally, in addition to the
% environment server).


-export_type([ env_pid/0, env_reg_name/0, env_info/0, env_designator/0,
			   env_data/0,
			   key/0, value/0, entry/0, entries/0, cache_spec/0 ]).


% For myriad_spawn*:
-include("spawn_utils.hrl").


% The name of the key in the process dictionary corresponding to environment
% information, notably for caching:
%
-define( env_dictionary_key, 'myriad_environment_cache' ).


% A list_table, as it is expected to reference only very few environments:
-type all_env_table() :: list_table( env_pid(), cache_info() ).
% Corresponds to the value associated to the env_dictionary_key key in
% the process dictionary of a process using environment caching.


-type env_cache_table() :: table( atom(), term() ).
% A table storing the (local) cached entries for a given environment.



% Just for silencing:
-export_type([ all_env_table/0 ]).



% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type list_table( K, V ) :: list_table:list_table( K, V ).

-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().
-type any_file_path() :: file_utils:any_file_path().

-type registration_name() :: naming_utils:registration_name().



% Implementation notes:
%
% Each environment server is managed through a locally registered process,
% maintaining an associative table whose content can be defined programmatically
% and/or thanks to data files.

% There is a slight potential race condition with the implicit starting of this
% service: a process could trigger its creation whereas it is already in
% progress, due to an earlier trigger.

% When requiring that an environment server is started, it will be returned
% directly if it is found already available; yet then the link status may not be
% honored (e.g. a start_link may thus return the PID of a non-linked process).

% More generally, relying on transparent, implicit launching is generally not
% recommended, as it is more error-prone.

% Some accessors accept both a registration name and a file path, whereas the
% former could be deduced from the latter. The idea is to avoid, in the case of
% potentially frequent operations, unnecessary conversions.
%
% See also, in the file_utils module, the get_data_directory/1 and
% get_extra_data_directories/1 functions in order to locate as environment data
% file.

% When using caching, the corresponding cached entries for a given environment
% will be stored in the process dictionary (of each client process using that
% feature), under the dictionary key designated by the
% env_dictionary_key define.
%
% The value associated to this dictionary key is a AllEnvTable ::
% all_env_table() table associating to each cached environment (designated by
% the PID of its server) the registration name of that server and a table of its
% cached entries.

% May start functions look the same, but are not, minor variations prevent much
% factorisation.



% @doc Ensures explicitly that, if not running already, a specified environment
% server is started.
%
% If a name is specified: if no server already registered it, starts it with a
% blank state.
%
% If instead a filename is specified: if a server with a deriving name is not
% already registered, starts a corresponding server and initialises it with the
% corresponding file content.
%
% Does not link any started environment server to the calling process.
%
% Returns in any case the PID of the corresponding environment server, already
% existing or not, blank or not.
%
-spec start( env_reg_name() | file_path() ) -> env_pid().
start( ServerRegName ) when is_atom( ServerRegName ) ->
	case naming_utils:is_registered( ServerRegName, _LookupScope=local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			% No link to be created here, so one must beware of any silent crash
			% of this server:
			%
			?myriad_spawn( fun() ->
							server_run( CallerPid, ServerRegName,
										_MaybeDefaultEntries=[] )
						   end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end;


start( FilePath ) when is_list( FilePath ) ->

	RegistrationName = get_env_reg_name_from( FilePath ),

	case naming_utils:is_registered( RegistrationName, _LookupScope=local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:string_to_binary( FilePath ),

			% No link to be created here, so one must beware of any silent crash
			% of this server:
			%
			?myriad_spawn( fun() ->
							server_run( CallerPid, RegistrationName,
										BinFilePath, _MaybeDefaultEntries=[] )
						   end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% @doc Starts and initialises an environment server based first on the specified
% defaults.
%
% If a name is specified: registers it under that name, and starts it just with
% the specified defaults.
%
% If instead a filename is specified: starts a corresponding server with a
% deriving name, and initialises it first with the specified defaults, then with
% the corresponding file content (thus potentially overriding these defaults).
%
% Ensures that no prior corresponding environment server already exists, as the
% specified defaults could not then be properly taken into account.
%
% Does not link the started environment server to the calling process.
%
% Returns the PID of the corresponding, just created, environment server.
%
-spec start_with_defaults( env_reg_name() | file_path(), entries() ) ->
													env_pid().
start_with_defaults( ServerRegName, DefaultEntries )
										when is_atom( ServerRegName ) ->
	case naming_utils:is_registered( ServerRegName, _LookupScope=local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			% No link to be created here, so one must beware of any silent crash
			% of this server:
			%
			?myriad_spawn( fun() ->
							server_run( CallerPid, ServerRegName,
										DefaultEntries )
						   end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			throw( { already_existing_environment, ServerRegName, Pid } )

	end;

start_with_defaults( FilePath, DefaultEntries ) when is_list( FilePath ) ->

	RegistrationName = get_env_reg_name_from( FilePath ),

	case naming_utils:is_registered( RegistrationName, _LookupScope=local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:string_to_binary( FilePath ),

			% No link to be created here, so one must beware of any silent crash
			% of this server:
			%
			?myriad_spawn( fun() ->
							server_run( CallerPid, RegistrationName,
										BinFilePath, DefaultEntries )
						   end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			throw( { already_existing_environment, FilePath, Pid } )

	end.



% @doc Ensures explicitly that, if not running already, a specified environment
% server is started and linked.
%
% If a name is specified: if no server already registered it, starts it with a
% blank state, and links that server to the calling process.
%
% If instead a filename is specified, if a server with a deriving name is not
% already registered, starts a corresponding server, links it to the calling
% process and initialises it with the file content.
%
% Returns in any case the PID of the corresponding environment server, already
% existing or not, blank or not.
%
-spec start_link( env_reg_name() | file_path() ) -> env_pid().
start_link( ServerRegName ) when is_atom( ServerRegName ) ->
	case naming_utils:is_registered( ServerRegName, _LookupScope=local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			?myriad_spawn_link( fun() ->
									server_run( CallerPid, ServerRegName,
												_MaybeDefaultEntries=[] )
								end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end;

start_link( FilePath ) when is_list( FilePath ) ->

	RegistrationName = get_env_reg_name_from( FilePath ),

	case naming_utils:is_registered( RegistrationName, _LookupScope=local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:string_to_binary( FilePath ),

			?myriad_spawn_link( fun() ->
									server_run( CallerPid, RegistrationName,
										BinFilePath, _MaybeDefaultEntries=[] )
								end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% @doc Starts, links and initialises an environment server based first on the
% specified defaults.
%
% If a name is specified: registers it under that name, and starts-linked it
% just with the specified defaults.
%
% If instead a filename is specified: starts-linked a corresponding server with
% a deriving name, and initialises it first with the specified defaults, then
% with the corresponding file content (thus potentially overriding these
% defaults).
%
% Ensures that no prior corresponding environment server already exists, as the
% specified defaults could not then be properly taken into account.
%
% Returns the PID of the corresponding, just created, environment server.
%
-spec start_link_with_defaults( env_reg_name() | file_path(), entries() ) ->
													env_pid().
start_link_with_defaults( ServerRegName, DefaultEntries )
								when is_atom( ServerRegName ) ->
	case naming_utils:is_registered( ServerRegName, _LookupScope=local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			?myriad_spawn_link( fun() ->
									server_run( CallerPid, ServerRegName,
												DefaultEntries )
								end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			throw( { already_existing_environment, ServerRegName, Pid } )

	end;

start_link_with_defaults( FilePath, DefaultEntries )
										when is_list( FilePath ) ->

	RegistrationName = get_env_reg_name_from( FilePath ),

	case naming_utils:is_registered( RegistrationName, _LookupScope=local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:string_to_binary( FilePath ),

			?myriad_spawn_link( fun() ->
									server_run( CallerPid, RegistrationName,
												BinFilePath, DefaultEntries )
								end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			throw( { already_existing_environment, FilePath, Pid } )

	end.



% @doc Ensures explicitly that, if not running already, an environment server is
% started with the specified registration name, and based on the specified file.
%
% If a server with the specified name is not already registered, starts a
% corresponding server and initialises it with the content of the specified
% file.
%
% Does not link the started environment server to the calling process.
%
% Returns in any case the PID of the corresponding environment server, already
% existing or not.
%
-spec start( env_reg_name(), any_file_path() ) -> env_pid().
start( ServerRegName, AnyFilePath ) when is_atom( ServerRegName ) ->

	case naming_utils:is_registered( ServerRegName, _LookupScope=local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:ensure_binary( AnyFilePath ),

			% No link to be created here, so we must beware of any silent crash
			% of this server:
			%
			?myriad_spawn( fun() ->
							server_run( CallerPid, ServerRegName, BinFilePath,
										_MaybeDefaultEntries=[] )
						   end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% @doc Ensures explicitly that, if not running already, an environment server is
% started and linked with the specified registration name, and based on the
% specified file.
%
% If a server with the specified name is not already registered, starts a
% corresponding server, links it to the calling process and initialises it with
% the content of the specified file.
%
% Returns in any case the PID of the corresponding environment server, already
% existing or not.
%
-spec start_link( env_reg_name(), any_file_path() ) -> env_pid().
start_link( ServerRegName, AnyFilePath ) when is_atom( ServerRegName ) ->

	case naming_utils:is_registered( ServerRegName, _LookupScope=local ) of

		not_registered ->

			% A goal is to acquire the "lock" (the local name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			BinFilePath = text_utils:ensure_binary( AnyFilePath ),

			?myriad_spawn_link( fun() ->
									server_run( CallerPid, ServerRegName,
										BinFilePath, _MaybeDefaultEntries=[] )
								end ),

			receive

				{ environment_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% @doc Ensures explicitly that, if not running already, a specified environment
% server is started, and caches the specified elements.
%
% If a name is specified: if no server already registered it, starts it with a
% blank state.
%
% If instead a filename is specified: if a server with a deriving name is not
% already registered, starts a corresponding server and initialises it with the
% corresponding file content.
%
% See the cache_spec/0 type and the cache/2 function for related information
% regarding caching.
%
% Does not link any started environment server to the calling process.
%
% Returns in any case the PID of the corresponding environment server, already
% existing or not, and caching specified elements.
%
-spec start_cached( env_reg_name(), cache_spec() ) -> env_pid().
start_cached( ServerRegName, CacheSpec ) ->
	EnvPid = start( ServerRegName ),
	cache( CacheSpec, ServerRegName ),
	EnvPid.



% @doc Ensures explicitly that, if not running already, a specified environment
% server is started and linked, and caches the specified elements.
%
% If a name is specified: if no server already registered it, starts it with a
% blank state.
%
% If instead a filename is specified: if a server with a deriving name is not
% already registered, starts a corresponding server and initialises it with the
% corresponding file content.
%
% See the cache_spec/0 type and the cache/2 function for related information
% regarding caching.
%
% Links any started environment server to the calling process.
%
% Returns in any case the PID of the corresponding environment server, already
% existing or not, and caching specified elements.
%
-spec start_link_cached( env_reg_name(), cache_spec() ) -> env_pid().
start_link_cached( ServerRegName, CacheSpec ) ->
	EnvPid = start_link( ServerRegName ),
	cache( CacheSpec, ServerRegName ),
	EnvPid.



% @doc Returns the PID of a supposedly already-running environment server,
% specified based on either its name or content filename.
%
-spec get_server( env_reg_name() | file_path() ) -> env_pid().
get_server( ServerRegName ) when is_atom( ServerRegName ) ->

	case naming_utils:is_registered( ServerRegName, _LookupScope=local ) of

		not_registered ->
			throw( { environment_server_not_registered, ServerRegName } );

		SrvPid ->
			SrvPid

	end;

get_server( FilePath ) when is_list( FilePath ) ->

	% Not reusing the previous clause for clearer exception:
	ServerRegName = get_env_reg_name_from( FilePath ),

	case naming_utils:is_registered( ServerRegName, _LookupScope=local ) of

		not_registered ->
			throw( { environment_server_not_registered, ServerRegName,
					 FilePath } );

		SrvPid ->
			SrvPid

	end.



% @doc Returns the server information of a supposedly already-running
% environment server, specified based on either its name or content filename.
%
-spec get_server_info( env_reg_name() | file_path() ) -> env_info().
get_server_info( ServerRegName ) when is_atom( ServerRegName ) ->

	case naming_utils:is_registered( ServerRegName, local ) of

		not_registered ->
			throw( { environment_server_not_registered, ServerRegName } );

		SrvPid ->
			{ ServerRegName, SrvPid }

	end;

get_server_info( FilePath ) when is_list( FilePath ) ->

	% Not reusing the previous clause for clearer exception:
	ServerRegName = get_env_reg_name_from( FilePath ),

	case naming_utils:is_registered( ServerRegName, _LookupScope=local ) of

		not_registered ->
			throw( { environment_server_not_registered, ServerRegName,
					 FilePath } );

		SrvPid ->
			{ ServerRegName, SrvPid }

	end.



% @doc Waits (up to 5 seconds, otherwise throws an exception) until the
% specified environment server becomes available, then returns its PID.
%
% Allows to synchronise to an environment server typically launched
% concurrently, before being able to look-up values from it.
%
-spec wait_available( env_reg_name() ) -> env_pid().
wait_available( ServerRegName ) ->

	trace_utils:debug_fmt( "Waiting until environment server '~ts' becomes "
						   "available.", [ ServerRegName ] ),

	naming_utils:wait_for_registration_of( ServerRegName, _RegScope=local ).



% @doc Returns the value associated to each of the specified key(s) in the
% environment (if any), otherwise 'undefined', based on the specified
% environment file (and possibly launching a corresponding, non-linked,
% environment server if needed) or on an already-running environment server
% (specified as a registration name, an environment information or a PID).
%
% Any cached key will be read from the local process cache, not from the
% environment server.
%
% Examples:
%
%  "Hello!" = environment:get(hello, "/var/foobar.etf")
%
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], "/var/foobar.etf")
%
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], my_env_server_name)
%
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], MyEnvServerPid)
%
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], MyEnvInfo)
%
-spec get( maybe_list( key() ), env_data() | file_path() ) ->
										maybe_list( maybe( value() ) ).
get( Key, AnyEnvData ) when is_atom( Key ) ->
	hd( get( [ Key ], AnyEnvData ) );

get( Keys, _EnvInfo={ _EnvRegName, EnvPid } ) ->
	get( Keys, EnvPid );

% A designator is either a PID or a registration atom:
get( Keys, EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	get( Keys, EnvPid );

get( KeyMaybes, FilePath ) when is_list( FilePath ) ->
	EnvSrvPid = start( FilePath ),
	get( KeyMaybes, EnvSrvPid );

% Hence EnvPid expected to be a PID here:
get( Keys, EnvPid ) when is_list( Keys ) ->
	case process_dictionary:get( ?env_dictionary_key ) of

		undefined ->
			% No caching wanted or applied, so necessarily a message-based
			% request to the server:
			%
			get_from_environment( Keys, EnvPid );

		AllEnvTable ->
			case list_table:lookup_entry( EnvPid, AllEnvTable ) of

				% Caching used, but not for that environment; server needed:
				key_not_found ->
					get_from_environment( Keys, EnvPid );

				% Caching activated, including for this environment, at least
				% for some keys:
				%
				{ value, { _EnvRegName, EnvCacheTable } } ->
					% Having to select the remote entries needed (if any):
					DictCachedKeys = table:keys( EnvCacheTable ),
					case list_utils:difference( Keys, DictCachedKeys ) of

						% All entries in cache; returning (in order) their
						% value directly:
						%
						[] ->
							table:get_values( Keys, EnvCacheTable );

						% At least some will have to be requested from the
						% server (and will still not be cached):
						%
						LackingKeys ->
							LackingValues =
								get_from_environment( LackingKeys, EnvPid ),

							aggregate_values( Keys, LackingKeys, LackingValues,
											  EnvCacheTable, _AccValues=[] )

					end

			end

	end.



% Merges cached entries with the specified, immediate ones.
%
% (helper)
-spec aggregate_values( [ key() ], [ key() ], [ value() ], table(),
						[ value() ] ) -> [ value() ].
% Checks that Immediate* are also empty out of safety (not necessary):
aggregate_values( _TargetKeys=[], _ImmediateKeys=[], _ImmediateValues=[],
				  _CacheTable, AccValues ) ->
	lists:reverse( AccValues );

% Current key is an immediate one:
aggregate_values( _TargetKeys=[ K | Tt ], _ImmediateKeys=[ K | Tl ],
				  _ImmediateValues=[ V | Tv ], CacheTable, Acc ) ->
	aggregate_values( Tt, Tl, Tv, CacheTable, _NewAcc=[ V | Acc ] );

% Current key is thus in cache:
aggregate_values( _TargetKeys=[ K | Tt ], ImmediateKeys, ImmediateValues,
				  CacheTable, Acc ) ->
	V = table:get_value( K, CacheTable ),
	aggregate_values( Tt, ImmediateKeys, ImmediateValues, CacheTable,
					  _NewAcc=[ V | Acc ] ).



% @doc Returns the value associated to each of the specified key(s) in the
% environment (if any), otherwise 'undefined', based on the specified
% registration name: uses any server registered with that name, otherwise uses
% the specified filename to start a corresponding server.
%
% Any cached key will be read from the local process cache, not from the
% environment server.
%
% Examples:
%
%  "Hello!" = environment:get(hello, my_env_server_name, "/var/foobar.etf")
%
%  ["Hello!", 42, undefined] =
%     environment:get([hello, my_number, some_maybe], my_env_server_name,
%                      "/var/foobar.etf")
%
-spec get( maybe_list( key() ), env_reg_name(), file_path() ) ->
										maybe_list( maybe( value() ) ).
get( KeyMaybes, ServerRegName, FilePath ) ->
	EnvSrvPid = case naming_utils:is_registered( ServerRegName,
												 _LookupScope=local ) of

		not_registered ->
			start( FilePath );

		ServerPid ->
			ServerPid

	end,
	get( KeyMaybes, EnvSrvPid ).



% @doc Gets the specified entries unconditionally from the environment server
% (not taking into account any cache).
%
% (helper)
%
-spec get_from_environment( maybe_list( maybe_list( key() ) ),
					env_designator() ) -> maybe_list( maybe( value() ) ).
get_from_environment( _KeyMaybes=[], _EnvDesignator ) ->
	[];

get_from_environment( KeyMaybes, EnvDesignator ) ->
	EnvDesignator ! { get_environment, KeyMaybes, self() },
	receive

		{ notify_get_environment, ValueMaybes } ->
			ValueMaybes

	end.



% @doc Sets (unconditionally) the specified key/value pairs (possibly
% overwriting any previous values) in the specified environment, based on the
% specified environment file (and possibly launching a corresponding environment
% server if needed) or on the designated already-running environment server
% (specified by registration name, environment information or PID).
%
% Any cached key will be updated in the local process cache, in addition to the
% environment server.
%
-spec set( [ entry() ], env_data() | file_path() ) -> void().
set( Entries, _EnvInfo={ _EnvRegName, EnvPid } ) ->
	set( Entries, EnvPid );

set( Entries, EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	set( Entries, EnvPid );

set( Entries, FilePath ) when is_list( FilePath ) ->
	EnvPid = start( FilePath ),
	set( Entries, EnvPid );

% Implicitly: when is_pid( EnvPid ).
set( Entries, EnvPid ) when is_list( Entries ) ->
	% Unconditional:
	EnvPid ! { set_environment, Entries },

	% Now update any locally cached keys:
	EnvDictKey = ?env_dictionary_key,
	case process_dictionary:get( EnvDictKey ) of

		undefined ->
			% No caching wanted or applied, no cache to update:
			ok;

		AllEnvTable ->
			case list_table:lookup_entry( EnvPid, AllEnvTable ) of

				% Caching used, but not for that environment; thus we are done:
				key_not_found ->
					ok;

				% Caching activated, including for this environment, at least
				% for some keys:
				%
				{ value, { EnvRegName, EnvCacheTable } } ->

					% We update only the cached keys:
					NewEnvCacheTable =
						table:update_existing_entries( Entries, EnvCacheTable ),

					NewAllEnvTable = list_table:add_entry( EnvPid,
						{ EnvRegName, NewEnvCacheTable }, AllEnvTable ),

					process_dictionary:put( EnvDictKey, NewAllEnvTable )

			end

	end.



% @doc Associates (unconditionally), in the specified environment, the specified
% value to the specified key (possibly overwriting any previous value), based on
% the specified environment file (and possibly launching a corresponding
% environment server if needed) or on the designated already-running environment
% server (specified by registration name, environment information or PID).
%
% Any cached key will be updated in the local process cache, in addition to the
% environment server.
%
-spec set( key(), value(), env_data() | file_path() ) -> void().
set( Key, Value, AnyEnvElem ) ->
	set( _Entries=[ { Key, Value } ], AnyEnvElem ).



% @doc Associates (unconditionally), in the specified environment, the specified
% value to the specified key (possibly overwriting any previous value), based on
% the specified registration name: uses any server registered with that name,
% otherwise uses the specified filename to start a corresponding server.
%
% Any cached key will be updated in the local process cache, in addition to the
% environment server.
%
-spec set( key(), value(), env_reg_name(), file_path() ) -> void().
set( Key, Value, ServerRegName, FilePath ) ->
	EnvPid = case naming_utils:is_registered( ServerRegName,
											  _LookupScope=local ) of

		not_registered ->
			start( FilePath );

		ServerPid ->
			ServerPid

	end,
	set( _Entries=[ { Key, Value } ], EnvPid ).




% @doc Sets conditionally the specified key/value pairs (that is, only if
% necessary, meaning only if the specified value does not match any currently
% cached one for that key; possibly overwriting any previous values) in the
% specified environment, based on the specified environment file (and possibly
% launching a corresponding environment server if needed) or on the designated
% already-running environment server (specified by registration name,
% environment information or PID).
%
% Any cached key will be updated in the local process cache, in addition to the
% environment server.
%
-spec set_cond( [ entry() ], env_data() | file_path() ) -> void().
set_cond( Entries, _EnvInfo={ _EnvRegName, EnvPid } ) ->
	set_cond( Entries, EnvPid );

set_cond( Entries, EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	set_cond( Entries, EnvPid );

set_cond( Entries, FilePath ) when is_list( FilePath ) ->
	EnvPid = start( FilePath ),
	set_cond( Entries, EnvPid );


% Implicitly: when is_pid( EnvPid ).
set_cond( Entries, EnvPid ) when is_list( Entries ) ->

	% Check any locally cached keys:
	EnvDictKey = ?env_dictionary_key,
	case process_dictionary:get( EnvDictKey ) of

		undefined ->
			% No caching wanted or applied, no cache to update, full sending
			% needed:
			%
			EnvPid ! { set_environment, Entries };

		AllEnvTable ->
			case list_table:lookup_entry( EnvPid, AllEnvTable ) of

				% Caching used, but not for that environment; thus we are done:
				key_not_found ->
					EnvPid ! { set_environment, Entries };

				% Caching activated, including for this environment, at least
				% for some keys:
				%
				{ value, { EnvRegName, EnvCacheTable } } ->

					NewEnvCacheTable = case get_needed_sendings( Entries,
											EnvCacheTable, _AccEntries=[] ) of

						% Spared sending:
						{ _ToSendEntries=[], UpdatedEnvCacheTable } ->
							UpdatedEnvCacheTable;

						% Minimal sending:
						{ ToSendEntries, UpdatedEnvCacheTable } ->
							EnvPid ! { set_environment, ToSendEntries },
							UpdatedEnvCacheTable

					end,

					NewAllEnvTable = list_table:add_entry( EnvPid,
						{ EnvRegName, NewEnvCacheTable }, AllEnvTable ),

					process_dictionary:put( EnvDictKey, NewAllEnvTable )

			end

	end.



% @doc Updates the specified environment with the entries found in the specified
% ETF file.
%
% Loaded entries supersede any pre-existing ("default") ones.
%
-spec update_from_etf( any_file_path(), env_data() ) -> void().
update_from_etf( AnyETFFilePath, AnyEnvData ) ->
	LoadedEntries = file_utils:read_etf_file( AnyETFFilePath ),
	set( LoadedEntries, AnyEnvData ).



% @doc Returns the non-matching entries (that thus shall be sent to the
% environment server), and an updated environment cache.
%
-spec get_needed_sendings( entries(), env_cache_table(), entries() ) ->
										{ entries(), env_cache_table() }.
get_needed_sendings( _NewEntries=[], EnvCacheTable, AccEntries ) ->
	{ _ToSendEntries=AccEntries, EnvCacheTable };

get_needed_sendings( _NewEntries=[ E={ K, V } | T ], EnvCacheTable,
					 AccEntries ) ->

	case table:lookup_entry( K, EnvCacheTable ) of

		% Not (to be) cached, thus to be sent:
		key_not_found ->
			get_needed_sendings( T, EnvCacheTable, [ E | AccEntries ] );

		% Cached and already matching, thus nothing to do:
		{ value, V } ->
			get_needed_sendings( T, EnvCacheTable, AccEntries );

		% Cached and not matching, thus to be updated in cache and sent:
		{ value, _PreviousOtherV } ->
			NewEnvCacheTable = table:add_entry( K, V, EnvCacheTable ),
			get_needed_sendings( T, NewEnvCacheTable, [ E | AccEntries ] )

	end.



% @doc Associates conditionally (that is, only if necessary, meaning only if the
% specified value does not match any currently cached one for that key), in the
% specified environment, the specified value to the specified key (possibly
% overwriting any previous value), based on the specified environment file (and
% possibly launching a corresponding environment server if needed) or on the
% designated already-running environment server (specified by registration name,
% environment information or PID).
%
% Any cached key will be updated in the local process cache, in addition to the
% environment server.
%
-spec set_cond( key(), value(), env_data() | file_path() ) -> void().
set_cond( Key, Value, AnyEnvElem ) ->
	set_cond( _Entries=[ { Key, Value } ], AnyEnvElem ).



% @doc Associates conditionally (that is, only if necessary, meaning only if the
% specified value does not match any currently cached one for that key), in the
% specified environment, the specified value to the specified key (possibly
% overwriting any previous value), based on the specified registration name:
% uses any server registered with that name, otherwise uses the specified
% filename to start a corresponding server.
%
% Any cached key will be updated in the local process cache, in addition to the
% environment server.
%
-spec set_cond( key(), value(), env_reg_name(), file_path() ) -> void().
set_cond( Key, Value, ServerRegName, FilePath ) ->
	EnvPid = case naming_utils:is_registered( ServerRegName,
											  _LookupScope=local ) of

		not_registered ->
			start( FilePath );

		ServerPid ->
			ServerPid

	end,
	set_cond( _Entries=[ { Key, Value } ], EnvPid ).



% @doc Removes the specified entries (a single one or multiple ones) from the
% specified environment, based on the designated already-running environment
% server (specified by registration name, environment information or PID).
%
-spec remove( key(), env_data() ) -> void();
			( [ key() ], env_data() ) -> void().
remove( Key, EnvPid ) when is_atom( Key ) ->
	remove( [ Key ], EnvPid );

remove( Keys, _EnvInfo={ _EnvRegName, EnvPid } ) ->
	remove( Keys, EnvPid );

remove( Keys, EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	remove( Keys, EnvPid );

remove( Keys, EnvPid ) when is_list( Keys ) ->
	EnvPid ! { remove_from_environment, Keys },

	% Cache update as well:
	EnvDictKey = ?env_dictionary_key,
	case process_dictionary:get( EnvDictKey ) of

		undefined ->
			% No environment cached at all, no cache to update:
			ok;

		AllEnvTable ->
			case list_table:lookup_entry( EnvPid, AllEnvTable ) of

				% Caching used, but not for that environment; thus we are done:
				key_not_found ->
					ok;

				% Caching activated, including for this environment, at least
				% for some keys:
				%
				{ value, { EnvRegName, EnvCacheTable } } ->

					% We remove only the cached keys (as it is not an error to
					% remove a non-existing key):
					%
					NewEnvCacheTable =
						table:remove_entries( Keys, EnvCacheTable ),

					NewAllEnvTable = case table:is_empty( NewEnvCacheTable ) of

						true ->
							list_table:remove_entry( EnvPid, AllEnvTable );

						false ->
							CacheInfo = { EnvRegName, NewEnvCacheTable },
							list_table:add_entry( EnvPid, CacheInfo,
												  AllEnvTable )

					end,

					process_dictionary:put( EnvDictKey, NewAllEnvTable )

			end

	end.



% @doc Extracts the specified entries (a single one or multiple ones) from the
% specified environment, based on the designated already-running environment
% server (specified by registration name, environment information or PID):
% removes these entries (including from any local cache) and returns their
% value.
%
-spec extract( key(), env_data() ) -> value();
			 ( [ key() ], env_data() ) -> [ value() ].
extract( Key, EnvPid ) when is_atom( Key ) ->
	hd( extract( [ Key ], EnvPid ) );

extract( Keys, _EnvInfo={ _EnvRegName, EnvPid } ) ->
	extract( Keys, EnvPid );

extract( Keys, EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	extract( Keys, EnvPid );


extract( Keys, EnvPid ) when is_list( Keys ) ->
	% Ignoring cache first, as the server has to be updated anyway:
	EnvPid ! { extract_from_environment, Keys, self() },

	% Interleaving with cache update:
	EnvDictKey = ?env_dictionary_key,
	case process_dictionary:get( EnvDictKey ) of

		undefined ->
			% No environment cached at all, no cache to update:
			ok;

		AllEnvTable ->
			case list_table:lookup_entry( EnvPid, AllEnvTable ) of

				% Caching used, but not for that environment; thus we are done:
				key_not_found ->
					ok;

				% Caching activated, including for this environment, at least
				% for some keys:
				%
				{ value, { EnvRegName, EnvCacheTable } } ->

					% We remove only the cached keys (as it is not an error to
					% remove a non-existing key); removal, not extraction, as
					% the values will be received from the server anyway:
					%
					NewEnvCacheTable =
						table:remove_entries( Keys, EnvCacheTable ),

					NewAllEnvTable = case table:is_empty( NewEnvCacheTable ) of

						true ->
							list_table:remove_entry( EnvPid, AllEnvTable );

						false ->
							CacheInfo = { EnvRegName, NewEnvCacheTable },
							list_table:add_entry( EnvPid, CacheInfo,
												  AllEnvTable )

					end,

					process_dictionary:put( EnvDictKey, NewAllEnvTable )

			end

	end,

	% From the extract_from_environment request:
	receive

		{ notify_extract_environment, Values } ->
			Values

	end.



% @doc Requests the calling process to cache the entries corresponding to the
% specified key(s), which will thus be appropriately synchronised with the
% server from now on, in addition to any already cached keys.
%
% Either single keys or full entries can be specified there. Both will lead the
% corresponding keys to be cached, yet a single key, if it is not already cached
% (otherwise, its update will be ignored), will trigger its value to be fetched
% from the environment server whereas the value of a full entry will be cached
% and also sent to the environment server (therefore being equivalent to set/2).
%
% Any next setting by this process of one of these cached keys will update its
% local cache as well as the specified environment server; as a consequence,
% here the cache is expected to start consistent with its server; afterwards by
% default only the entries not already in cache will be requested from the
% server.
%
% The environment can be specified through its information (recommended), or
% from its registration name, otherwise directly through its PID, in which case
% at least one of its entries shall already be cached (indeed a client cache
% stores the registration name of the cached environments, which cannot be
% deduced from their PID).
%
-spec cache( cache_spec(), env_data() ) -> void().
cache( AnyElem, _EnvInfo={ EnvRegName, EnvPid } ) ->
	cache( AnyElem, EnvRegName, EnvPid );

cache( Key, AnyEnvData ) when is_atom( Key ) ->
	cache( [ Key ], AnyEnvData );

cache( Entry, AnyEnvData ) when is_tuple( Entry ) ->
	cache( [ Entry ], AnyEnvData );

% Not having a registration name here; hopefully this environment is already
% known of the cache:
%
cache( KeysOrEntries, EnvPid ) when is_pid( EnvPid ) ->

	cond_utils:if_defined( myriad_debug_environments,
		trace_utils:debug_fmt( "Client ~w caching, "
			"regarding environment ~w:~n~p",
			[ self(), EnvPid, KeysOrEntries ] ) ),

	% Separates single keys from entries:
	{ ToCacheKeys, ToCacheEntries } = lists:foldl(
		fun( E={ _K, _V }, { AccK, AccE } ) ->
			{ AccK, [ E | AccE ] };

		   ( K, { AccK, AccE } ) ->
			{ [ K | AccK ], AccE }

		end,
		_Acc0={ _AccK0=[], _AccE0=[] },
		_List=KeysOrEntries ),

	EnvDictKey = ?env_dictionary_key,

	% We have to determine the new keys to cache whose values must be fetched
	% from server:
	%
	case process_dictionary:get( EnvDictKey ) of

		undefined ->
				trace_utils:error_fmt( "Cannot cache for environment server ~w,"
					" as the corresponding environment registration name is "
					"not known (none is known).~nElements were: ~p",
					[ EnvPid, KeysOrEntries ] ),
				throw( { unknown_environment_for, EnvPid } );

		PrevAllEnvTable ->
			case list_table:lookup_entry( EnvPid, PrevAllEnvTable ) of

				key_not_found ->
					% Quite same as before, this environment was not cached yet:
					trace_utils:error_fmt( "Cannot cache for environment "
						"server ~w, as the corresponding environment "
						"registration name is not known.",
						[ EnvPid, KeysOrEntries ] ),
					throw( { unknown_environment_for, EnvPid } );

				{ value, { EnvRegName, PrevEnvCacheTable } } ->
					cond_utils:if_defined( myriad_debug_environments,
						trace_utils:debug_fmt( "Updating cache for environment "
							"~ts (server: PID: ~w): ~ts", [ EnvRegName, EnvPid,
								table:to_string( PrevEnvCacheTable ) ] ) ),

					% Having to add all single keys not already present in the
					% cache table:
					%
					DictCachedKeys = table:keys( PrevEnvCacheTable ),

					NewToCacheKeys =
						list_utils:difference( ToCacheKeys, DictCachedKeys ),

					finish_caching( EnvPid, EnvRegName, NewToCacheKeys,
						ToCacheEntries, PrevEnvCacheTable, PrevAllEnvTable,
						EnvDictKey )

			end

	end;

cache( KeysOrEntries, EnvRegName ) when is_atom( EnvRegName ) ->

	cond_utils:if_defined( myriad_debug_environments,
		trace_utils:debug_fmt( "Client ~w caching, "
			"regarding environment ~ts:~n~p",
			[ self(), EnvRegName, KeysOrEntries ] ) ),

	% As a PID will be needed (at least as key for the environment cache):
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),

	cache( KeysOrEntries, EnvRegName, EnvPid ).



% @doc Requests the calling process to cache the entries corresponding to the
% specified key(s), which will thus be appropriately synchronised with the
% server from now on, in addition to any already cached keys.
%
% Either single keys or full entries can be specified there. Both will lead the
% corresponding keys to be cached, yet a single key, if it is not already cached
% (otherwise, it will be ignored), will trigger its value to be fetched from the
% environment server whereas the value of a full entry will be cached and also
% sent to the environment server (therefore being equivalent to set/2).
%
% Any next setting by this process of one of these cached keys will update its
% local cache as well as the specified environment server; as a consequence,
% here the cache is expected to start consistent with its server; afterwards by
% default only the entries not already in cache will be requested from the
% server.
%
-spec cache( cache_spec(), env_reg_name(), env_pid() ) -> void().
cache( Key, EnvRegName, EnvPid ) when is_atom( Key ) ->
	cache( [ Key ], EnvRegName, EnvPid );

cache( Entry, EnvRegName, EnvPid ) when is_tuple( Entry ) ->
	cache( [ Entry ], EnvRegName, EnvPid );

cache( KeysOrEntries, EnvRegName, EnvPid ) ->

	% Separates single keys from entries:
	{ ToCacheKeys, ToCacheEntries } = lists:foldl(
		fun( E={ _K, _V }, { AccK, AccE } ) ->
			{ AccK, [ E | AccE ] };

		   ( K, { AccK, AccE } ) ->
			{ [ K | AccK ], AccE }

		end,
		_Acc0={ _AccK0=[], _AccE0=[] },
		_List=KeysOrEntries ),

	EnvDictKey = ?env_dictionary_key,

	% We have to determine the new keys to cache whose values must be fetched
	% from server:
	%
	{ SingleKeys, Entries, EnvCacheTable, AllEnvTable } =
			case process_dictionary:get( EnvDictKey ) of

		undefined ->
			% All these new elements shall then be cached:
			cond_utils:if_defined( myriad_debug_environments,
				trace_utils:debug( "No environment was cached." ) ),

			{ ToCacheKeys, ToCacheEntries, table:new(), list_table:new() };

		PrevAllEnvTable ->
			case list_table:lookup_entry( EnvPid, PrevAllEnvTable ) of

				key_not_found ->
					% Quite same as before, this environment was not cached yet:
					cond_utils:if_defined( myriad_debug_environments,
						trace_utils:debug_fmt( "Environment ~ts "
							"(server: PID: ~w) was not cached.",
							[ EnvRegName, EnvPid ] ) ),

					{ ToCacheKeys, ToCacheEntries, table:new(),
					  PrevAllEnvTable };

				{ value, { _EnvRegName, PrevEnvCacheTable } } ->
					cond_utils:if_defined( myriad_debug_environments,
						trace_utils:debug_fmt( "Updating cache for environment "
							"~ts (server: PID: ~w): ~ts", [ EnvRegName, EnvPid,
								table:to_string( PrevEnvCacheTable ) ] ) ),

					% Having to add all single keys not already present in the
					% cache table:
					%
					DictCachedKeys = table:keys( PrevEnvCacheTable ),

					NewToCacheKeys =
						list_utils:difference( ToCacheKeys,	DictCachedKeys ),

					{ NewToCacheKeys, ToCacheEntries, PrevEnvCacheTable,
					  PrevAllEnvTable }

			end

	end,

	finish_caching( EnvPid, EnvRegName, SingleKeys, Entries, EnvCacheTable,
					AllEnvTable, EnvDictKey ).




% Part common to the two cache clauses.
%
% (helper)
%
finish_caching( EnvPid, EnvRegName, SingleKeys, Entries, EnvCacheTable,
				AllEnvTable, EnvDictKey ) ->

	% First the environment must be aware of these new entries:
	EnvPid ! { set_environment, Entries },

	% And the local cache as well:
	WithEntriesEnvCacheTable = table:add_entries( Entries, EnvCacheTable ),

	% Then update based on the single keys:
	NewEnvCacheTable = case SingleKeys of

		[] ->
			WithEntriesEnvCacheTable;

		_ ->
			SingleValues = get_from_environment( SingleKeys, EnvPid ),
			SingleEntries = lists:zip( SingleKeys, SingleValues ),
			table:add_entries( SingleEntries, WithEntriesEnvCacheTable )

	end,

	%trace_utils:debug_fmt( "New cache for environment ~ts (server: ~w): ~ts",
	%   [ EnvRegName, EnvPid, table:to_string( NewEnvCacheTable ) ] ),

	NewAllEnvTable = list_table:add_entry( EnvPid,
		{ EnvRegName, NewEnvCacheTable }, AllEnvTable ),

	%trace_utils:debug_fmt( "New environment table for client ~w: ~ts",
	%   [ self(), list_table:to_string( NewAllEnvTable ) ] ),

	process_dictionary:put( EnvDictKey, NewAllEnvTable ).



% @doc Caches in the calling process the specified keys, and returns their
% associated value.
%
% Equivalent to a call to cache/2 followed by one to get/2 with the same keys.
%
-spec cache_return( maybe_list( key() ), env_data() ) ->
								maybe_list( maybe( value() ) ).
cache_return( Key, AnyEnvData ) when is_atom( Key ) ->
	cache_return( [ Key ], AnyEnvData );

cache_return( Keys, AnyEnvData ) ->
	cache( Keys, AnyEnvData ),
	get( Keys, AnyEnvData ).



% @doc Removes any (local) cache regarding the specified environment, resulting
% in all subsequent operations on it to happen at the level of its server.
%
-spec uncache( env_data() ) -> void().
uncache( _EnvInfo={ _EnvRegName, EnvPid } ) ->
	uncache( EnvPid );

uncache( EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	uncache( EnvPid );

uncache( EnvPid ) ->

	EnvDictKey = ?env_dictionary_key,

	% Removing any local caching:
	case process_dictionary:get( EnvDictKey ) of

		undefined ->
			ok;

		AllEnvTable ->
			case list_table:extract_entry_if_existing( EnvPid, AllEnvTable ) of

				false ->
					ok;

				% It was the last environment:
				{ _EnvPair, _ShrunkAllEnvTable=[] } ->
					process_dictionary:remove( EnvDictKey );

				% At least one environment remaining:
				{ _EnvPair, ShrunkAllEnvTable } ->
					process_dictionary:put( EnvDictKey, ShrunkAllEnvTable )

			end

	end.



% @doc Removes all (local) environment caches (if any), resulting in all
% subsequent operations on environments to happen at the level of their server.
%
-spec uncache() -> void().
uncache() ->
	process_dictionary:remove( ?env_dictionary_key ).



% @doc Synchronises the local cache (if any) from the specified environment
% server, assuming the server entries are references (that is more recent than
% the cached ones).
%
-spec sync( env_data() ) -> void().
sync( _EnvInfo={ _EnvRegName, EnvPid } ) ->
	sync( EnvPid ) ;

sync( EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	sync( EnvPid );

sync( EnvPid ) ->
	EnvDictKey = ?env_dictionary_key,
	case process_dictionary:get( EnvDictKey ) of

		undefined ->
			% No caching wanted or applied, nothing to sync:
			ok;

		AllEnvTable ->
			case list_table:lookup_entry( EnvPid, AllEnvTable ) of

				% Caching used, but not for that environment; thus we are done:
				key_not_found ->
					ok;

				% Caching activated, including for this environment, at least
				% partially:
				%
				{ value, { EnvRegName, EnvCacheTable } } ->

					AllCachedKeys = table:keys( EnvCacheTable ),

					AllValues = get_from_environment( AllCachedKeys, EnvPid ),

					AllCachedEntries = lists:zip( AllCachedKeys, AllValues ),

					NewEnvCacheTable = table:new( AllCachedEntries ),

					NewAllEnvTable = list_table:add_entry( EnvPid,
						{ EnvRegName, NewEnvCacheTable }, AllEnvTable ),

					process_dictionary:put( EnvDictKey, NewAllEnvTable )

			end

	end.



% @doc Ensures that the specified key(s), expected to be some kind of strings,
% are binary strings.
%
% Typically useful so that default plain strings may be specified, even if
% internally they should be binary ones.
%
-spec ensure_binary( maybe_list( key() ), env_data() ) -> void().
ensure_binary( KeyMaybeList, _EnvInfo={ _EnvRegName, EnvPid } ) ->
	ensure_binary( KeyMaybeList, EnvPid );

ensure_binary( KeyMaybeList, EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	ensure_binary( KeyMaybeList, EnvPid );

ensure_binary( KeyMaybeList, EnvPid ) ->
	EnvPid ! { ensure_binary, [ KeyMaybeList ] }.



% @doc Stores (asynchronously) the current state of the specified environment in
% the file whence it supposedly was loaded initially.
%
-spec store( env_data() ) -> void().
store( _EnvInfo={ _EnvRegName, EnvPid } ) ->
	store( EnvPid );

store( EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	store( EnvPid );

store( EnvPid ) ->
	EnvPid ! store.



% @doc Stores (asynchronously) the current state of the specified environment in
% the specified file, regardless of any file from which it was loaded.
%
% The specified path becomes the reference one.
%
-spec store( env_data(), any_file_path() ) -> void().
store( _EnvInfo={ _EnvRegName, EnvPid }, TargetFilePath ) ->
	store( EnvPid, TargetFilePath );

store( EnvRegName, TargetFilePath ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	store( EnvPid, TargetFilePath );

store( EnvPid, TargetFilePath ) ->
	BinTargetFilePath = text_utils:ensure_binary( TargetFilePath ),
	EnvPid ! { store, BinTargetFilePath }.



% @doc Returns a textual description of the environments known of the calling
% process, based on its cache (if any).
%
-spec to_string() -> ustring().
to_string() ->
	case process_dictionary:get( ?env_dictionary_key ) of

		undefined ->
			"no caching of environments";

		AllEnvTable ->
			case list_table:enumerate( AllEnvTable ) of

				[] ->
					"no environment cached";

				[ { EnvPid, CacheInfo } ] ->
					text_utils:format( "a single environment cached, ~ts",
						[ cache_info_to_string( CacheInfo, EnvPid ) ] );

				EnvPairs ->
					text_utils:format( "~B environments cached: ~ts",
						[ length( EnvPairs ), text_utils:strings_to_string(
							[ cache_info_to_string( CInf, EPid )
								|| { CInf, EPid } <- EnvPairs ] ) ] )


			end

	end.



% @doc Returns a textual description of the environment server (if any)
% specified from an information or designator thereof, or from its environment
% file.
%
% Prefer calling to_bin_string/1.
%
-spec to_string( env_data() | file_path() ) -> ustring().
to_string( EnvData ) ->
	text_utils:binary_to_string( to_bin_string( EnvData ) ).



% @doc Returns a textual description of the environment server (if any)
% specified from an information or designator thereof, or from its environment
% file.
%
% To be preferred to to_string/1.
%
-spec to_bin_string( env_data() | file_path() ) -> bin_string().
to_bin_string( _EnvInfo={ EnvRegName, _EnvPid } ) ->
	to_bin_string( EnvRegName );

to_bin_string( EnvRegAtom ) when is_atom( EnvRegAtom ) ->

	case naming_utils:is_registered( EnvRegAtom, local ) of

		not_registered ->
			text_utils:bin_format( "environment server for '~ts' not running",
								   [ EnvRegAtom ] );

		EnvSrvPid->
			to_bin_string( EnvSrvPid )

	end;


to_bin_string( FilePath ) when is_list( FilePath ) ->

	EnvRegAtom = get_env_reg_name_from( FilePath ),

	to_bin_string( EnvRegAtom );


% Then supposed to be existing:
to_bin_string( EnvSrvPid ) when is_pid( EnvSrvPid ) ->

	EnvSrvPid ! { to_bin_string, self() },

	receive

		{ notify_environment_status, EnvBinString } ->
			EnvBinString

	end.



-spec cache_info_to_string( cache_info(), env_pid() ) -> ustring().
cache_info_to_string( _CacheInfo={ EnvRegName, EnvCacheTable }, EnvPid ) ->
	text_utils:format( "environment '~ts' (whose server is ~w), caching ~ts",
		[ EnvRegName, EnvPid, cache_to_string( EnvCacheTable ) ] ).


-spec cache_to_string( env_cache_table() ) -> ustring().
cache_to_string( EnvCacheTable ) ->
	case table:enumerate( EnvCacheTable ) of

		[] ->
			"no entry";

		[ { K, V } ] ->
			text_utils:format( "a single entry, whose key ~p is associated "
							   "to value ~p", [ K, V ] );

		Entries ->
			text_utils:format( "~B entries: ~ts", [ length( Entries ),
				text_utils:strings_to_string( [ text_utils:format(
					"key '~ts' associated to value ~p", [ K, V ] )
						|| { K, V} <- Entries ], _IdentLevel=2 ) ] )

	end.



% @doc Returns the automatic naming used for registering an environment server,
% as deduced from the specified environment filename.
%
% For example, the registration name associated to the
% "/var/opt/foobar.application.etf" environment filename is
% 'foobar_application'.
%
-spec get_env_reg_name_from( file_path() ) -> env_reg_name().
get_env_reg_name_from( FilePath ) ->

	CoreFilePath = file_utils:remove_upper_levels_and_extension( FilePath ),

	RegistrationName =
		file_utils:path_to_variable_name( CoreFilePath, _Prefix="" ),

	text_utils:string_to_atom( RegistrationName ).



% @doc Stops (asynchronously) the environment server designated by the specified
% registration name or file, if it is running.
%
% Never fails.
%
-spec stop( env_data() | file_path() ) -> void().
stop( _EnvInfo={ _EnvRegName, EnvPid } ) ->
	stop( EnvPid );

stop( FilePath ) when is_list( FilePath ) ->

	EnvRegAtom = get_env_reg_name_from( FilePath ),

	stop( EnvRegAtom );

stop( EnvRegName ) when is_atom( EnvRegName ) ->
	EnvPid = naming_utils:get_registered_pid_for( EnvRegName, _Scope=local ),
	stop( EnvPid );

stop( EnvPid ) ->
	uncache( EnvPid ),
	EnvPid ! stop.



% Section for the environment server itself.


% Launcher of the environment server, to start with either a blank state or with
% default entries.
%
-spec server_run( pid(), env_reg_name(), maybe( entries() ) ) -> no_return().
server_run( SpawnerPid, RegistrationName, MaybeDefaultEntries ) ->

	cond_utils:if_defined( myriad_debug_environments, trace_utils:debug_fmt(
		"Spawning an environment server named '~ts' from ~w.",
		[ RegistrationName, SpawnerPid ] ) ),

	case naming_utils:register_or_return_registered( RegistrationName,
													 local_only ) of

		registered ->

			% We gained the shared name, we are the one and only server for that
			% name.

			InitialTable = case MaybeDefaultEntries of

				undefined ->
					table:new();

				DefaultEntries ->
					table:new( DefaultEntries )

			end,

			% Spawner could already know that PID in this case:
			SpawnerPid ! { environment_server_pid, self() },

			% Never returns:
			server_main_loop( InitialTable, RegistrationName,
							  _MaybeBinFilePath=undefined );


		ServerPid ->
			% Notifies and terminates:
			SpawnerPid ! { environment_server_pid, ServerPid }

	end.



% Launcher of the environment server, to be initialised with any specified
% default entries, then with the specified file.
%
-spec server_run( pid(), env_reg_name(), bin_string(), maybe( entries() ) ) ->
												no_return().
server_run( SpawnerPid, RegistrationName, BinFilePath, MaybeDefaultEntries ) ->

	cond_utils:if_defined( myriad_debug_environments, trace_utils:debug_fmt(
		"Spawning environment server named '~ts' from ~w, based on file '~ts'.",
		[ RegistrationName, SpawnerPid, BinFilePath ] ) ),

	case naming_utils:register_or_return_registered( RegistrationName,
													 local_only ) of

		registered ->

			% We gained the shared name, we are the one and only server for that
			% name.

			InitialTable = case MaybeDefaultEntries of

				undefined ->
					table:new();

				DefaultEntries ->
					table:new( DefaultEntries )

			end,

			FinalTable =
					case file_utils:is_existing_file_or_link( BinFilePath ) of

				true ->
					add_environment_from( BinFilePath, InitialTable );

				false ->
					trace_bridge:info_fmt( "No environment file found "
						"(searched for '~ts').", [ BinFilePath ] ),
					InitialTable

			end,

			% Spawner could already know that PID in this case:
			SpawnerPid ! { environment_server_pid, self() },

			% Never returns:
			server_main_loop( FinalTable, RegistrationName, BinFilePath );


		ServerPid ->
			% Notifies and terminates:
			SpawnerPid ! { environment_server_pid, ServerPid }

	end.



% (helper)
-spec get_value_maybes( key(), table() ) -> value();
					  ( [ key() ], table() ) -> [ value() ].
get_value_maybes( Key, Table ) when is_atom( Key ) ->
	table:get_value_with_default( Key, _Def=undefined, Table );

get_value_maybes( Keys, Table ) ->
	get_value_maybes( Keys, Table, _Acc=[] ).



% (helper)
get_value_maybes( _Keys=[], _Table, Acc ) ->
	lists:reverse( Acc );

get_value_maybes( _Keys=[ K | T ], Table, Acc ) ->
	V = get_value_maybes( K, Table ),
	get_value_maybes( T, Table, [ V | Acc ] ).


% (helper)
-spec ensure_binaries( [ key() ], table() ) -> table().
ensure_binaries( Keys, Table ) ->
	AnyStrs = table:get_values( Keys, Table ),
	BinStrs = text_utils:ensure_binaries( AnyStrs ),
	NewEntries = lists:zip( Keys, BinStrs ),
	table:add_entries( NewEntries, Table ).



% Main loop of the environment server.
-spec server_main_loop( table(), env_reg_name(), maybe( bin_file_path() ) ) ->
												no_return().
server_main_loop( Table, EnvRegName, MaybeBinFilePath ) ->

	% Short:
	%trace_bridge:debug_fmt( "Waiting for environment-related request, "
	%    "while having ~B recorded entries.", [ table:size( Table ) ] ),

	% Detailed:
	%trace_bridge:debug_fmt( "Waiting for environment-related request, "
	%   "storing a ~ts.", [ table:to_string( Table ) ] ),

	% Ad-hoc message conventions are a bit more flexible and involve no external
	% naming (like wooper_result atoms):

	receive

		{ get_environment, KeyMaybes, SenderPid } ->

			Answer = get_value_maybes( KeyMaybes, Table ),

			SenderPid ! { notify_get_environment, Answer },

			server_main_loop( Table, EnvRegName, MaybeBinFilePath );


		{ set_environment, Key, Value } ->

			NewTable = table:add_entry( Key, Value, Table ),

			server_main_loop( NewTable, EnvRegName, MaybeBinFilePath );


		{ set_environment, Entries } ->

			%trace_utils:debug_fmt( "[environment ~ts (~w)] Setting entries "
			%                       "~p.", [ EnvRegName, self(), Entries ] ),

			NewTable = table:add_entries( Entries, Table ),

			server_main_loop( NewTable, EnvRegName, MaybeBinFilePath );


		{ remove_from_environment, Keys } ->

			ShrunkTable = table:remove_entries( Keys, Table ),

			server_main_loop( ShrunkTable, EnvRegName, MaybeBinFilePath );


		{ extract_from_environment, Keys, SenderPid } ->

			{ Values, ShrunkTable } = table:extract_entries( Keys, Table ),

			SenderPid ! { notify_extract_environment, Values },

			server_main_loop( ShrunkTable, EnvRegName, MaybeBinFilePath );


		store ->
			case MaybeBinFilePath of

				undefined ->
					trace_utils:warning_fmt( "Store request for environment "
						"server ~w ignored (no file path set).", [ self() ] );

				BinFilePath ->

					cond_utils:if_defined( myriad_debug_environments,
						trace_utils:debug_fmt( "Storing environment state "
							"in current file '~ts'.", [ BinFilePath ] ) ),

					file_utils:write_etf_file( table:enumerate( Table ),
											   BinFilePath )

			end,
			server_main_loop( Table, EnvRegName, MaybeBinFilePath );


		{ store, BinTargetFilePath } ->

			cond_utils:if_defined( myriad_debug_environments,
				trace_utils:debug_fmt( "Storing environment state "
					"in specified file '~ts'.", [ BinTargetFilePath ] ) ),

			file_utils:write_etf_file( table:enumerate( Table ),
									   BinTargetFilePath ),

			server_main_loop( Table, EnvRegName, BinTargetFilePath );


		{ ensure_binary, SingleKey } when is_atom( SingleKey ) ->
			NewTable = ensure_binaries( [ SingleKey ], Table ),
			server_main_loop( NewTable, EnvRegName, MaybeBinFilePath );

		{ ensure_binary, KeyMaybes } ->
			NewTable = ensure_binaries( KeyMaybes, Table ),
			server_main_loop( NewTable, EnvRegName, MaybeBinFilePath );


		{ to_bin_string, SenderPid } ->

			Msg = case table:enumerate( Table ) of

				[] ->
					text_utils:format( "no entry recorded for environment "
						"'~ts' (server: ~w)", [ EnvRegName, self() ] )
							++ case MaybeBinFilePath of

						undefined ->
							"";

						BinfilePath ->
							text_utils:format( " (associated to file '~ts')",
											   [ BinfilePath ] )

							end;

				L ->

					% Enforces a consistent order:
					Strings = [ text_utils:format( "for key '~ts', value is: "
							"~p", [ K, V ] ) || { K, V } <- lists:sort( L ) ],

					text_utils:format( "~B entries recorded in environment "
						"'~ts' (server: ~w): ~ts~n",
						[ length( L ), EnvRegName, self(),
						  text_utils:strings_to_string( Strings ) ] )

			end,

			BinMsg = text_utils:string_to_binary( Msg ),
			SenderPid ! { notify_environment_status, BinMsg },

			server_main_loop( Table, EnvRegName, MaybeBinFilePath );


		stop ->
			%trace_bridge:debug_fmt( "Stopping environment server ~w.",
			%                        [ self() ] ),
			stopped

	end.



% Helper functions.


% Adds the environment found in specified file into the specified table, and
% returns it.
%
% (helper)
%
add_environment_from( FilePath, Table ) ->

	case file:consult( FilePath ) of

		{ ok, Entries } ->

			case check_entries( Entries ) of

				ok ->
					NewTable = table:add_entries( Entries, Table ),

					%trace_bridge:debug_fmt( "Loaded from environment file "
					%    "'~ts' following entries: ~ts",
					%    [ FilePath, table:to_string( NewTable ) ] ),

					%trace_bridge:debug_fmt( "Environment file '~ts' loaded.",
					%                        [ FilePath ] ),

				   NewTable;

				ErrorString ->
					trace_bridge:error_fmt( "Error when reading environment "
						"file '~ts' (~ts), no environment read.",
						[ FilePath, ErrorString ] ),
					Table

			end;


		{ error, { Line, _Mod, Term } } ->
			FlattenError = text_utils:format( "~p", [ Term ] ),
			trace_bridge:error_fmt( "Error in environment file '~ts' "
				"at line ~B (~ts), no environment read.",
				[ FilePath, Line, FlattenError ] ),
			Table;


		{ error, Reason } ->
			trace_bridge:error_fmt( "Error when reading environment file "
				"'~ts' (~p), no environment read.", [ FilePath, Reason ] ),
			Table

	end.



% Checks specified entries.
check_entries( _Entries=[] ) ->
	ok;

check_entries( _Entries=[ { K, _V } | T ] ) when is_atom( K ) ->
	check_entries( T );

check_entries( _Entries=[ { K, _V } | _T ] ) ->
	text_utils:format( "key '~p' is not an atom", [ K ] );

check_entries( _Entries=[ E | _T ] ) ->
	text_utils:format( "entry '~p' is not a key/value pair", [ E ] ).
