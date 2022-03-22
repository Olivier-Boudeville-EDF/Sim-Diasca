% Copyright (C) 2013-2022 Olivier Boudeville
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


% @doc Service dedicated to the <b>management of user preferences</b>, thanks to
% a corresponding server process.
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
% accessed (read/write) in the course of program execution, before possibly
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
% Such a file may be used to create a preferences server (ex: with start/0) or
% to update a pre-existing one (with update_from_etf/{1,2}).
%
% The corresponding server process is locally registered, generally under a
% fixed name, which may be the default Myriad one (see the
% default_preferences_filename define), or a user-defined one (ex:
% foobar_preferences).
%
% As a consequence, a preferences server can be designated either directly
% through its PID or through its conventional (atom) registration name (ex:
% implicitly with `preferences:get(hello)' for the default one, or explicitly
% with `preferences:get(hello, foobar_preferences)'. No specific global
% registration of that server is made here.
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
% will not be aware of this change until it requests an update to this
% preferences server.
%
% In practice, now preferences are a special case of environment (see our
% environment module for more details). So each preferences server is an
% environment process (not unlike an ETS table) able to read, store, modify,
% save the data that it manages.
%
-module(preferences).


-export([ start/0, start/1, start/2, start_link/0, start_link/1, start_link/2,
		  get/1, get/2, set/2, set/3, update_from_etf/1, update_from_etf/2,
		  to_string/0, to_bin_string/0,

		  get_default_preferences_path/0,
		  get_default_preferences_registration_name/0,

		  is_preferences_default_file_available/0,
		  check_preferences_default_file/0,
		  stop/0 ]).


-type preferences_pid() :: environment:env_pid().
% The PID of a preferences server.

-type preferences_designator() :: environment:env_designator().
% The two standard ways according to which a preferences server can be
% designated: either directly thanks to its PID or to the name under which it is
% locally registered.


-type pref_reg_name() :: environment:env_reg_name().
% The name under which a preferences server can be (locally) registered.


-type key() :: environment:key().

-type value() :: environment:value().
% Can be 'undefined' (no difference between a non-registered key and a key
% associated to 'undefined').

-type entry() :: table:entry().
-type entries() :: table:entries().


-export_type([ preferences_pid/0, preferences_designator/0,
			   key/0, value/0, entry/0, entries/0 ]).


-compile( { inline, [ get_default_preferences_path/0,
					  get_default_preferences_registration_name/0] } ).



% Shorthands:

-type file_path() :: file_utils:file_path().
-type any_file_path() :: file_utils:any_file_path().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type maybe_list( T ) :: list_utils:maybe_list( T ).



% Implementation notes:
%
% Preferences are managed thanks to an environment through a singleton, locally
% registered process, maintaining an associative table whose content can be
% defined programmatically and/or thanks to data files.

% There is a slight potential race condition with the implicit starting of this
% service: a process could trigger its creation while its creation is in
% progress, due to an earlier trigger.



% Default name for local registration:
%
% (not used anymore so that multiple preferences servers, based on different
% data files/names, can coexist)
%
%-define( default_preferences_server_name, myriad_preferences_server ).


% Name of the default Ceylan-level preferences ETF file (searched at the root of
% the user account):
%
% (must be consistent with the default_preferences_pref_reg_name define)
%
-define( default_preferences_filename, ".ceylan-settings.etf" ).


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
	environment:start_link( get_default_preferences_path() ).



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
start( AnyParam ) ->
	environment:start( AnyParam ).



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
start_link( Param ) ->
	environment:start_link( Param ).



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
start( ServerName, FilePath ) ->
	environment:start( ServerName, FilePath ).



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
start_link( ServerName, FilePath ) ->
	environment:start_link( ServerName, FilePath ).



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
%  "Hello!" = preferences:get(hello)
%
%  ["Hello!", 42, undefined] = preferences:get([hello, my_number, some_maybe])
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
	environment:update_from_etf( AnyETFFilePath, PrefDesignator ).




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



% @doc Returns the full, absolute path to the default preferences filename.
-spec get_default_preferences_path() -> file_path().
get_default_preferences_path() ->
	file_utils:join( system_utils:get_user_home_directory(),
					 ?default_preferences_filename ).



% @doc Returns the default registration name for the preferences.
-spec get_default_preferences_registration_name() -> pref_reg_name().
get_default_preferences_registration_name() ->
	?default_preferences_pref_reg_name.



% @doc Returns whether the default preferences file is available and its full
% path.
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



% @doc Stops (asynchronously) the default preferences server, if it is running.
%
% Never fails.
%
-spec stop() -> void().
stop() ->
	environment:stop( get_default_preferences_path() ).
