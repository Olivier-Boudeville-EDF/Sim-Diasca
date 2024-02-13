% Copyright (C) 2013-2024 Olivier Boudeville
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
% Creation date: Thursday, October 31, 2013.


% @doc Service dedicated to the <b>management of user preferences</b> (for
% user-level configuration), thanks to a corresponding server process.
%
% Preferences are generally default, static settings, like user general
% preferences, defined in a file meant to be potentially read by multiple
% applications. This is a way of storing durable information in one's user
% account in a transverse way regarding programs and versions thereof, and of
% sharing these elements (settings, credentials, etc.) conveniently. See also,
% in the file_utils module, the get_configuration_directory/1 and
% get_extra_configuration_directories/1 functions in order to locate first such
% a configuration file.
%
% Preferences can be application-specific or component-specific, and obtained
% from any source (file included); they may also start blank and be exclusively
% fed by the application or component itself. In any case they are meant to be
% accessed (read/written) in the course of program execution, before possibly
% being stored at application exit or component stop.
%
% A preferences entry is designated by a key (an atom), associated to a value
% (that can be any term). No difference is made between a non-registered key and
% a key registered to 'undefined'.
%
% Preferences can be stored in file(s), in the ETF format. This format of
% preferences is a series of Erlang terms as strings, each ended with a dot
% (i.e. it is the basic, standard format understood by `file:consult/1').
%
% Example of content for a preferences file:
% ```
% {my_first_color, red}.
% {myHeight, 1.80}.
% {'My name', "Sylvester the cat"}.
% '''
%
% Such a file may be used to create a preferences server (e.g. with start/0) or
% to update a pre-existing one (with update_from_etf/{1,2}).
%
% The corresponding server process is locally registered, generally under a
% fixed name, which may be the default Myriad one (see the
% default_preferences_filename define), or a user-defined one (e.g.
% foobar_preferences).
%
% As a consequence, a preferences server can be designated either directly
% through its PID or through its conventional (atom) registration name (e.g.
% implicitly with `preferences:get(hello)' for the default one, or explicitly,
% via its name with `preferences:get(hello, foobar_preferences)'. The former
% approach is a bit more effective, but the latter one is more robust (the
% server can be transparently restarted/upgraded).
%
% No specific global registration of that server is made here.
%
% A (single) explicit start (with one of the start* functions) shall be
% preferred to implicit ones (typically directly thanks to the get* functions)
% to avoid any risk of race conditions (should multiple processes attempt
% concurrently to create the same preferences server), and also in order to be
% able to request that it is also linked to the calling process.
%
% For faster accesses (not involving any inter-process messaging), and if
% considering that their changes are at least rather infrequent (or never
% happening), at least some entries managed by a preferences server may be
% cached directly in client processes.
%
% In this case the process dictionary of these clients is used, and when
% updating from a client process a cached key, the corresponding preferences
% server is updated in turn. However any other client process caching that key
% will not be aware of this change until it explicitly requests an update to
% this preferences server.
%
% In practice, now preferences are a special case of environment (see our
% environment module for more details). So each preferences server is an
% environment process (not unlike an ETS table) able to read, store, modify,
% save the data that it manages.
%
-module(preferences).


-export([ start/0, start/1, start/2, start_with_defaults/1,
		  start_link/0, start_link/1, start_link/2, start_link_with_defaults/1,
		  wait_available/0, wait_available/1,
		  get/1, get/2, set/2, set/3, update_from_etf/1, update_from_etf/2,
		  cache/1, cache/2, cache_return/1, cache_return/2,
		  ensure_binary/2,
		  to_string/0, to_bin_string/0, to_bin_string/1,

		  get_default_preferences_path/0,
		  is_preferences_default_file_available/0,
		  check_preferences_default_file/0,

		  get_application_preferences_filename/1,

		  get_application_preferences_file/1,
		  get_application_preferences_file/2,
		  get_application_preferences_file/3,
		  get_application_preferences_file/4,

		  get_default_preferences_registration_name/0,

		  stop/0 ]).


-type pref_reg_name() :: environment:env_reg_name().
% The name under which a preferences server can be (locally) registered.


-type preferences_pid() :: environment:env_pid().
% The PID of a preferences server.


-type preferences_info() :: { pref_reg_name(), preferences_pid() }.
% The full reference to a preferences server.
%
% The registration name is needed as a key of any local cache, and the PID
% allows to spare extra naming look-ups.


-type preferences_designator() :: preferences_pid() | pref_reg_name().
% The two standard ways according to which a preferences server can be
% designated: either directly thanks to its PID or to the name under which it is
% locally registered.


-type preferences_data() :: preferences_info() | preferences_designator().
% Any element designating a preferences server (most general handle).



-type key() :: environment:key().

-type value() :: environment:value().
% Can be 'undefined' (no difference between a non-registered key and a key
% associated to 'undefined').

-type entry() :: table:entry().
-type entries() :: table:entries().


-type app_pref_lookup_outcome() :: any_file_path()
		| { 'not_found', any_file_path(), [ any_directory_path() ] }.
% The outcome of the lookup of an application preferences file.


-export_type([ pref_reg_name/0, preferences_pid/0, preferences_info/0,
			   preferences_designator/0, preferences_data/0,
			   key/0, value/0, entry/0, entries/0,
			   app_pref_lookup_outcome/0 ]).


-compile( { inline, [ get_default_preferences_path/0,
					  get_default_preferences_registration_name/0] } ).


% For the app_info record:
-include("app_facilities.hrl").

% Shorthands:

-type file_name() :: file_utils:file_name().
-type file_path() :: file_utils:file_path().
-type any_file_path() :: file_utils:any_file_path().
-type any_directory_path() :: file_utils:any_directory_path().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type cache_spec() :: environment:cache_spec().

-type any_app_info() :: app_facilities:app_info().


% Implementation notes:
%
% Preferences are managed thanks to an environment through a singleton, locally
% registered process, maintaining an associative table whose content can be
% defined programmatically and/or thanks to data files.

% There is a slight potential race condition with the implicit starting of this
% service: a process could trigger its creation while its creation is in
% progress, due to an earlier trigger.
%
% When requiring that the preferences server is started, it will be returned
% directly if it is found already available; yet then the link status may not be
% honored (e.g. a start_link may thus return the PID of a non-linked process).

% Defaults can be specified; specific functions (e.g. start_with_defaults/1)
% have been specifically defined (rather than relying on update functions to be
% called after starting) as such defaults have to be applied first, so that such
% entries can be superseded (e.g. by an ETF file) afterwards (or not).


% Default name for local registration:
%
% (not used anymore so that multiple preferences servers, based on different
% data files/names, can coexist)
%
%-define( default_preferences_server_name, myriad_preferences_server ).


% The extension used by default by preferences ETF files:
-define( default_preferences_extension, "etf" ).


% Name of the default Ceylan-level preferences ETF file (searched at the root of
% the user account):
%
% (must be consistent with the default_preferences_pref_reg_name define)
%
-define( default_preferences_filename,
		 ".ceylan-settings."  ++ ?default_preferences_extension ).


% Registration name of the default preferences server:
%
% (must be consistent with the default_preferences_filename define - see
% get_default_preferences_registration_name/0 for that; defined to shortcut
% useless conversions from filename done by potentially frequently called
% getters and setters)
%
-define( default_preferences_pref_reg_name, '_ceylan_settings' ).



% @doc Ensures explicitly that, if not running already, the preferences service
% is started and initialised immediately, based on the default preferences file
% path and registered with a deriving name, if wanting an explicit start rather
% than one implied by the first operation onto it.
%
% Does not link the started preferences server to the calling process.
%
% Returns in any case the PID of the corresponding preferences server, already
% existing or not.
%
-spec start() -> preferences_pid().
start() ->
	environment:start( get_default_preferences_path() ).



% @doc Starts and initialises the preferences service based first on the
% specified defaults, and then also on the default preferences file path,
% registering it with a deriving name.
%
% Ensures that no prior preferences service already exists, as the specified
% defaults could not then be properly taken into account.
%
% Does not link the started preferences server to the calling process.
%
% Returns the PID of the corresponding, just created, preferences server.
%
-spec start_with_defaults( entries() ) -> preferences_pid().
start_with_defaults( DefaultEntries ) ->
	environment:start_with_defaults( get_default_preferences_path(),
									 DefaultEntries ).



% @doc Ensures explicitly that, if not running already, the preferences service
% is started and linked, and initialised immediately, based on the default
% preferences file path and registered with a deriving name, if wanting an
% explicit start rather than one implied by the first operation onto it.
%
% Returns in any case the PID of the corresponding preferences server, already
% existing or not.
%
-spec start_link() -> preferences_pid().
start_link() ->
	PrefPid = environment:start_link( get_default_preferences_path() ),
	trace_utils:debug_fmt( "Preferences started, as server ~w.", [ PrefPid ] ),
	PrefPid.



% @doc Starts, links and initialises the preferences service based first on the
% specified defaults, and then also on the default preferences file path,
% registering it with a deriving name.
%
% Ensures that no prior preferences service already exists, as the specified
% defaults could not then be properly taken into account.
%
% Returns the PID of the corresponding, just created, preferences server.
%
-spec start_link_with_defaults( entries() ) -> preferences_pid().
start_link_with_defaults( DefaultEntries ) ->

	PrefPid = environment:start_link_with_defaults(
		get_default_preferences_path(), DefaultEntries ),

	trace_utils:debug_fmt( "Preferences started, as server ~w.", [ PrefPid ] ),

	PrefPid.



% @doc Ensures explicitly that, if not running already, the preferences service
% is started.
%
% If a name is specified: if no server already registered it, starts it with a
% blank state.
%
% If instead a filename is specified: if a server with a deriving name is not
% already registered, starts a corresponding server and initialises it with the
% corresponding file content.
%
% Does not link the started preferences server to the calling process.
%
% Returns in any case the PID of the corresponding preferences server, already
% existing or not, blank or not.
%
-spec start( pref_reg_name() | file_path() ) -> preferences_pid().
start( AnyPrefName ) ->
	environment:start( AnyPrefName ).


% start_with_defaults/2 could be added here.


% @doc Ensures explicitly that, if not running already, the preferences service
% is started and linked.
%
% If a name is specified: if no server already registered it, starts it with a
% blank state, and links that server to the calling process.
%
% If instead a filename is specified, if a server with a deriving name is not
% already registered, starts a corresponding server, links it to the calling
% process and initialises it with the file content.
%
% Returns in any case the PID of the corresponding preferences server, already
% existing or not, blank or not.
%
-spec start_link( pref_reg_name() | file_path() ) -> preferences_pid().
start_link( AnyPrefName ) ->
	environment:start_link( AnyPrefName ).


% start_link_with_defaults/2 could be added here.


% @doc Ensures explicitly that, if not running already, the preferences service
% is started with the specified registration name, and based on the specified
% file.
%
% If a server with the specified name is not already registered, starts a
% corresponding server and initialises it with the content of the specified
% file.
%
% Does not link the started preferences server to the calling process.
%
% Returns in any case the PID of the corresponding preferences server, already
% existing or not.
%
-spec start( pref_reg_name(), file_path() ) -> preferences_pid().
start( ServerRegName, FilePath ) ->
	environment:start( ServerRegName, FilePath ).


% start_with_defaults/3 could be added here.


% @doc Ensures explicitly that, if not running already, the preferences service
% is started with the specified registration name and based on the specified
% file, and linked.
%
% If a server with the specified name is not already registered, starts a
% corresponding server, links it to the calling process and initialises it with
% the content of the specified file.
%
% Returns in any case the PID of the corresponding preferences server, already
% existing or not.
%
-spec start_link( pref_reg_name(), file_path() ) -> preferences_pid().
start_link( ServerRegName, FilePath ) ->
	environment:start_link( ServerRegName, FilePath ).


% start_link_with_defaults/3 could be added here.


% @doc Waits (up to 5 seconds, otherwise throws an exception) until the default
% preferences server becomes available, then returns its PID.
%
% Allows to synchronise to a preferences server typically launched concurrently,
% before being able to look-up preferences.
%
-spec wait_available() -> preferences_pid().
wait_available() ->
	wait_available( get_default_preferences_registration_name() ).



% @doc Waits (up to 5 seconds, otherwise throws an exception) until the default
% preferences server becomes available, then returns its PID.
%
% Allows to synchronise to a preferences server typically launched concurrently,
% before being able to look-up preferences.
%
-spec wait_available( pref_reg_name() ) -> preferences_pid().
wait_available( ServerRegName ) ->
	environment:wait_available( ServerRegName ).



% @doc Returns the value associated to each of the specified key(s) in the
% preferences (if any), otherwise 'undefined', based on the default preferences
% file, and possibly launching a corresponding (non-linked) preferences server
% if needed.
%
% Any cached key will be read from the local process cache, not from the
% preferences server.
%
% Examples:
%
% "Hello!" = preferences:get(hello)
%
% ["Hello!", 42, undefined] = preferences:get([hello, my_number, some_maybe])
%
-spec get( maybe_list( key() ) ) -> maybe_list( maybe( value() ) ).
get( KeyMaybes ) ->
	environment:get( KeyMaybes, get_default_preferences_registration_name(),
					 get_default_preferences_path() ).



% @doc Returns the value associated to each of the specified key(s) in the
% preferences (if any), otherwise 'undefined', based on the specified
% preferences file (and possibly launching a corresponding (non-linked)
% preferences server if needed) or on the specified PID of an already-running
% preferences server.
%
% Any cached key will be read from the local process cache, not from the
% preferences server.
%
% Examples:
%
%  "Hello!" = preferences:get(hello, "/var/foobar.etf")
%
%  ["Hello!", 42, undefined] =
%     preferences:get([hello, my_number, some_maybe], my_foobar_preferences)
%
%  ["Hello!", 42, undefined] =
%     preferences:get([hello, my_number, some_maybe], MyPrefServerPid)
%
-spec get( maybe_list( key() ), preferences_designator() | file_path() ) ->
										maybe_list( maybe( value() ) ).
get( KeyMaybes, EnvData ) ->
	environment:get( KeyMaybes, EnvData ).



% @doc Associates, if the first argument is a key, in default preferences, the
% specified value to the specified key (possibly overwriting any previous
% value); otherwise the first argument is expected to be a list of key/value
% pairs to be set in the specified preferences server.
%
% Any cached key will be updated in the local process cache, in addition to the
% preferences server.
%
-spec set( key(), value() ) -> void();
		 ( [ entry() ], preferences_designator() | file_path() ) -> void().
set( Key, Value ) when is_atom( Key ) ->
	environment:set( Key, Value, get_default_preferences_registration_name(),
					 get_default_preferences_path() );

set( Entries, EnvData ) ->
	environment:set( Entries, EnvData ).



% @doc Associates, in the specified preferences, the specified value to the
% specified key (possibly overwriting any previous value), based on the
% specified preferences file (and possibly launching a corresponding preferences
% server if needed) or on the specified PID of an already-running preferences
% server.
%
-spec set( key(), value(), preferences_designator() | file_path() ) -> void().
set( Key, Value, EnvData ) ->
	environment:set( Key, Value, EnvData ).



% @doc Updates the default preferences with the entries found in the specified
% ETF file.
%
% Loaded entries supersede any pre-existing ones.
%
-spec update_from_etf( any_file_path() ) -> void().
update_from_etf( AnyETFFilePath ) ->
	update_from_etf( AnyETFFilePath,
					 get_default_preferences_registration_name() ).


% @doc Updates the specified preferences with the entries found in the specified
% ETF file.
%
% Loaded entries supersede any pre-existing ones.
%
-spec update_from_etf( any_file_path(), preferences_designator() ) -> void().
update_from_etf( AnyETFFilePath, PrefDesignator ) ->

	%trace_utils:debug_fmt( "Updating preferences designated by ~w from "
	%                       "file '~ts'.", [ PrefDesignator, AnyETFFilePath ] ),

	environment:update_from_etf( AnyETFFilePath, PrefDesignator ).



% @doc Requests the calling process to cache the entries corresponding to the
% specified key(s), which will thus be appropriately synchronised with the
% default preferences server from now on, in addition to any already cached
% keys.
%
% Either single keys or full entries can be specified there. Both will lead the
% corresponding keys to be cached, yet a single key, if it is not already cached
% (otherwise, its updated will be ignored), will trigger its value to be fetched
% from the preferences server whereas the value of a full entry will be cached
% and also sent to the preferences server (therefore being equivalent to set/2).
%
% Any next setting by this process of one of these cached keys will update its
% local cache as well as the specified preferences server; as a consequence,
% here the cache is expected to start consistent with its server; afterwards by
% default only the entries not already in cache will be requested from the
% server.
%
-spec cache( cache_spec() ) -> void().
cache( CacheSpec ) ->
	cache( CacheSpec, _PrefData=get_default_preferences_registration_name() ).


% @doc Requests the calling process to cache the entries corresponding to the
% specified key(s), which will thus be appropriately synchronised with the
% preferences server from now on, in addition to any already cached keys.
%
% Either single keys or full entries can be specified there. Both will lead the
% corresponding keys to be cached, yet a single key, if it is not already cached
% (otherwise, its updated will be ignored), will trigger its value to be fetched
% from the preferences server whereas the value of a full entry will be cached
% and also sent to the preferences server (therefore being equivalent to set/2).
%
% Any next setting by this process of one of these cached keys will update its
% local cache as well as the specified preferences server; as a consequence,
% here the cache is expected to start consistent with its server; afterwards by
% default only the entries not already in cache will be requested from the
% server.
%
% The preferences server can be specified through its information, or from its
% registration name, otherwise directly through its PID. Use cache/1 if leaving
% it implicit (in which case the default preferences server will be used).
%
-spec cache( cache_spec(), preferences_data() ) -> void().
cache( CacheSpec, PrefData ) ->
	environment:cache( CacheSpec, PrefData ).



% @doc Caches in the calling process the specified keys, and returns their
% associated value.
%
% Equivalent to a call to cache/1 followed by one to get/1 with the same keys.
%
-spec cache_return( maybe_list( key() ) ) -> maybe_list( maybe( value() ) ).
cache_return( KeyMaybeList ) ->
	cache_return( KeyMaybeList,
				  _PrefData=get_default_preferences_registration_name() ).


% @doc Caches in the calling process the specified keys, and returns their
% associated value.
%
% Equivalent to a call to cache/2 followed by one to get/2 with the same keys.
%
-spec cache_return( maybe_list( key() ), preferences_data() ) ->
								maybe_list( maybe( value() ) ).
cache_return( KeyMaybeList, PrefData ) ->
	environment:cache_return( KeyMaybeList, PrefData ).



% @doc Ensures that the specified key(s), expected to be some kind of strings,
% are binary strings.
%
% Typically useful so that in ETF files plain strings may be specified, even if
% internally they should be binary ones.
%
-spec ensure_binary( maybe_list( key() ), preferences_data() ) -> void().
ensure_binary( KeyMaybeList, PrefData ) ->
	environment:ensure_binary( KeyMaybeList, PrefData ).



% @doc Returns a textual description of the preferences server (if any), for
% the default preferences file.
%
-spec to_string() -> ustring().
to_string() ->
	environment:to_string( get_default_preferences_path() ).


% @doc Returns a textual description of the preferences server (if any), for
% the default preferences file.
%
-spec to_bin_string() -> bin_string().
to_bin_string() ->
	environment:to_bin_string( get_default_preferences_path() ).


% @doc Returns a textual description of the specified preferences server.
-spec to_bin_string( preferences_data() ) -> bin_string().
to_bin_string( EnvData ) ->
	environment:to_bin_string( EnvData ).



% @doc Returns the full, absolute path to the default preferences filename.
-spec get_default_preferences_path() -> file_path().
get_default_preferences_path() ->
	file_utils:join( system_utils:get_user_home_directory(),
					 ?default_preferences_filename ).





% @doc Returns whether the default preferences file is available, together with
% its full path.
%
-spec is_preferences_default_file_available() -> { boolean(), file_path() }.
is_preferences_default_file_available() ->
	PrefFile = get_default_preferences_path(),
	Res = file_utils:is_existing_file_or_link( PrefFile ),
	{ Res, PrefFile }.



% @doc Checks that the default preferences file exists; throws an exception
% otherwise.
%
-spec check_preferences_default_file() -> void().
check_preferences_default_file() ->

	case is_preferences_default_file_available() of

		{ true, _FilePath } ->
			ok;

		{ false, FilePath } ->
			throw( { no_default_preferences_file_found, FilePath } )

	end.



% @doc Returns the name of the preferences file corresponding to the specified
% application.
%
-spec get_application_preferences_filename( any_app_info() ) -> file_name().
get_application_preferences_filename( #app_info{ name=BinAppName } ) ->
	text_utils:format( "~ts." ++ ?default_preferences_extension,
					   [ BinAppName ] );

get_application_preferences_filename( _AppInfoMap=#{ name := BinAppName } ) ->
	text_utils:format( "~ts." ++ ?default_preferences_extension,
					   [ BinAppName ] ).



% @doc Returns the found preferences file (if any) relevant for the specified
% application, otherwise the preferences filename and the ordered list of the
% full paths explored.
%
% For example, for an application named "foobar", of version 0.0.1, following
% paths may be looked up in turn:
%  - "~/.config/foobar/0.0.1/foobar.etf"
%  - "~/.config/foobar/foobar.etf"
%
% So for example either "/home/john/.config/foobar/foobar.etf" is returned, or a
% {not_found, "foobar.etf", ["/home/john/.config/foobar/0.0.1/foobar.etf",
% "/home/john/.config/foobar/foobar.etf"]} triplet.
%
-spec get_application_preferences_file( any_app_info() ) ->
											app_pref_lookup_outcome().
get_application_preferences_file( AnyAppInfo ) ->
	get_application_preferences_file( AnyAppInfo, _AddDefaultPrefPath=false ).


% @doc Returns the found preferences file (if any) relevant for the specified
% application, otherwise the preferences filename and the ordered list of the
% full paths explored.
%
% For example, for an application named "foobar", of version 0.0.1, following
% paths may be looked up in turn:
%  - "~/.config/foobar/0.0.1/foobar.etf"
%  - "~/.config/foobar/foobar.etf"
%  - if AddDefaultPrefPath is true: "/home/john/.ceylan-settings.etf"
%
% So for example either "/home/john/.config/foobar/foobar.etf" is returned, or a
% {not_found, "foobar.etf", ["/home/john/.config/foobar/0.0.1/foobar.etf",
% "/home/john/.config/foobar/foobar.etf", "/home/john/.ceylan-settings.etf"]}
% triplet.
%
-spec get_application_preferences_file( any_app_info(), boolean() ) ->
											app_pref_lookup_outcome().
get_application_preferences_file( AnyAppInfo, AddDefaultPrefPath ) ->
	get_application_preferences_file( AnyAppInfo, _PrioritaryDirs=[],
									  _ExtraDirs=[], AddDefaultPrefPath ).


% @doc Returns the found preferences file (if any) relevant for the specified
% application, otherwise the preferences filename and the ordered list of the
% full paths explored, using first the application-related directories, then the
% specified extra directory candidates and, if requested, as a last resort, the
% default preferences path.
%
% For example, for an application named "foobar", of version 0.0.1 and
% extra directories ["/home/a", "/var/b"], following paths may be looked up
% in turn:
%  - "~/.config/foobar/0.0.1/foobar.etf"
%  - "~/.config/foobar/foobar.etf"
%  - "/home/a/foobar.etf"
%  - "/var/b/foobar.etf"
%  - if AddDefaultPrefPath is true: "/home/john/.ceylan-settings.etf"
%
% So for example either "/var/b/foobar.etf" is returned, or a {not_found,
% "foobar.etf", ["/home/john/.config/foobar/0.0.1/foobar.etf",
%   "/home/john/.config/foobar/foobar.etf", "/home/a/foobar.etf",
%   "/var/b/foobar.etf", "/home/john/.ceylan-settings.etf"]} triplet.
%
%
-spec get_application_preferences_file( any_app_info(),
			[ any_directory_path() ], boolean() ) -> app_pref_lookup_outcome().
get_application_preferences_file( AnyAppInfo, ExtraDirs, AddDefaultPrefPath ) ->
	get_application_preferences_file( AnyAppInfo, _PrioritaryDirs=[], ExtraDirs,
								  AddDefaultPrefPath ).


% @doc Returns the found preferences file (if any) relevant for the specified
% application, otherwise the preferences filename and the ordered list of the
% full paths explored, using the specified prioritary directories as (ordered)
% first locations, then the application-related directories, then the extra
% (ordered) directories and, if requested, as a last resort, the default
% preferences path.
%
% For example, for an application named "foobar", of version 0.0.1 and
% prioritary directories ["/home/a", "/var/b"] and extra directories ["/opt/c"],
% following paths may be looked up in turn:
%  - "/home/a/foobar.etf"
%  - "/var/b/foobar.etf"
%  - "~/.config/foobar/0.0.1/foobar.etf"
%  - "~/.config/foobar/foobar.etf"
%  - "/opt/c/foobar.etf"
%  - if AddDefaultPrefPath is true: "/home/john/.ceylan-settings.etf"
%
% So for example either "/var/b/foobar.etf" is returned, or a {not_found,
% "foobar.etf", ["/home/a/foobar.etf", "/var/b/foobar.etf",
%   "/home/john/.config/foobar/0.0.1/foobar.etf",
%   "/home/john/.config/foobar/foobar.etf", "/opt/c/foobar.etf"
%   "/home/john/.ceylan-settings.etf"]} triplet.
%
-spec get_application_preferences_file( any_app_info(),
	[ any_directory_path() ], [ any_directory_path() ], boolean() ) ->
											app_pref_lookup_outcome().
get_application_preferences_file( AppInfo=#app_info{}, PrioritaryDirs,
								  ExtraDirs, AddDefaultPrefPath ) ->
	AppInfoMap = app_facilities:get_app_info_map( AppInfo ),
	get_application_preferences_file( AppInfoMap, PrioritaryDirs, ExtraDirs,
									  AddDefaultPrefPath );

get_application_preferences_file( AppInfoMap=#{ name := BinAppName },
		PrioritaryDirs, ExtraDirs, AddDefaultPrefPath ) ->

	PathType = user_config,

	AppCandidateDir =
		filename:basedir( PathType, BinAppName, _Opts=AppInfoMap ),

	% A second choice could be without the version:
	AppDirs = [ AppCandidateDir ] ++ case maps:find( _K=version, AppInfoMap ) of

		{ ok, _Version } ->
			NoVersionMap = maps:remove( version, AppInfoMap ),

			SecondAppCandidateDir =
				filename:basedir( PathType, BinAppName, NoVersionMap ),

			[ SecondAppCandidateDir ];

		error ->
			[]

						end,

	OrderedDirs = PrioritaryDirs ++ AppDirs ++ ExtraDirs,

	PrefFilename = get_application_preferences_filename( AppInfoMap ),

	case file_utils:get_first_file_or_link_for( PrefFilename, OrderedDirs ) of

		undefined ->
			case AddDefaultPrefPath of

				true ->
					DefPath = get_default_preferences_path(),
					case file_utils:is_existing_file_or_link( DefPath ) of

						true ->
							DefPath;

						_False ->
							BasePathCandidates = [
								file_utils:ensure_path_is_absolute(
									file_utils:join( D, PrefFilename ) )
														|| D <- OrderedDirs ],

							{ not_found, PrefFilename,
							  BasePathCandidates ++ [ DefPath ] }

					end;


				false ->
					BasePathCandidates = [ file_utils:ensure_path_is_absolute(
						file_utils:join( D, PrefFilename ) )
											|| D <- OrderedDirs ],

					{ not_found, PrefFilename, BasePathCandidates }

			end;

		PrefPath ->
			PrefPath

	end.



% @doc Returns the default (local) registration name for the preferences.
-spec get_default_preferences_registration_name() -> pref_reg_name().
get_default_preferences_registration_name() ->
	?default_preferences_pref_reg_name.



% @doc Stops (asynchronously) the default preferences server, if it is running.
%
% Never fails.
%
-spec stop() -> void().
stop() ->
	environment:stop( get_default_preferences_path() ).
