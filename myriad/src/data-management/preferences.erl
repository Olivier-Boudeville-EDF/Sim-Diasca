% Copyright (C) 2013-2021 Olivier Boudeville
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
% Creation date: Thursday, October 31, 2013.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Service dedicated to the management of user-defined preferences.
%
% A preferences element is designated by a key (an atom), associated to a value
% (that can be any term).
%
% Preferences can be stored in file(s).
%
% This is typically a way of storing durable information in one's user account
% in a transverse way compared to programs and versions thereof, and of sharing
% them conveniently (ex: for passwords, settings).
%
% The format of preferences is a series of Erlang terms as strings, each ended
% with a dot (i.e. the format understood by file:consult/1).
%
% Example of content of a preferences file:
% """
% {my_first_color, red}.
% {myheight, 1.80}.
% {myName, "Sylvester the cat"}.
% """
%
% (of course without the quotes and the leading percent sign)
%
-module(preferences).


-export([ start/0, start/1, start_link/0, start_link/1,
		  get/1, get/2, set/2, set/3, to_string/0,
		  to_string/1, get_default_preferences_path/0,
		  is_preferences_default_file_available/0,
		  check_preferences_default_file/0,
		  stop/0, stop/1 ]).


-type registration_name() :: atom(). % Private type


-type key() :: atom().


% Can be 'undefined' (no difference between a non-registered key and a key
% registered to 'undefined'):
%
-type value() :: table:value().


-type entry() :: table:entry().
-type entries() :: table:entries().

% The PID of a preferences server:
-type preferences_pid() :: pid().


-export_type([ key/0, value/0, entry/0, entries/0, preferences_pid/0 ]).


% For myriad_spawn*:
-include("spawn_utils.hrl").


% Shorthands:

-type file_path() :: file_utils:file_path().
-type ustring() :: text_utils:ustring().



% Implementation notes:
%
% Preferences are managed through a singleton, globally registered process,
% maintaining an associative table whose content can be defined programmatically
% and/or thanks to data files.

% There is a potential race condition for the starting of this service: a
% process could trigger its creation while its creation is in progress, due to
% an earlier trigger.



% Default name for global registration:
%
% (not used anymore so that multiple preferences servers, based on different
% data files, can coexist)
%
%-define( default_preferences_server_name, ceylan_preferences_server ).


% Name of the default preferences file (searched at the root of the user
% account):
%
-define( default_preferences_filename, ".ceylan-settings.txt" ).



% Ensures that, if not done already, the preferences service is started and
% initialised immediately (based on the default preferences path), if wanting an
% explicit start rather than one implied by the use of an operation onto it.
%
% Returns in any case the PID of the corresponding preferences server.
%
-spec start() -> preferences_pid().
start() ->
	start( get_default_preferences_path() ).


% Ensures that, if not done already, the preferences service is started and
% linked, and initialised immediately (based on the default preferences path),
% if wanting an explicit start rather than one implied by the use of an
% operation onto it.
%
% Returns in any case the PID of the corresponding preferences server.
%
-spec start_link() -> preferences_pid().
start_link() ->
	start_link( get_default_preferences_path() ).



% Ensures that, if not done already, the preferences service is started and
% initialised immediately with the specified filename.
%
% Returns in any case the PID of the corresponding preferences server.
%
-spec start( file_path() ) -> preferences_pid().
start( FileName ) ->

	RegistrationName = get_registration_name( FileName ),

	case naming_utils:is_registered( RegistrationName, global ) of

		not_registered ->

			% A goal is to acquire the "lock" (the global name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			% No sensible link to be created here, so we must beware of a silent
			% crash of this server:
			%
			?myriad_spawn( fun() ->
					   server_main_run( CallerPid, RegistrationName, FileName )
						   end ),

			receive

				{ preferences_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% Ensures that, if not done already, the preferences service is started and
% linked, and initialised immediately with the specified filename.
%
% Returns in any case the PID of the corresponding preferences server.
%
-spec start_link( file_path() ) -> preferences_pid().
start_link( FileName ) ->

	RegistrationName = get_registration_name( FileName ),

	case naming_utils:is_registered( RegistrationName, global ) of

		not_registered ->

			% A goal is to acquire the "lock" (the global name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			% No sensible link to be created here, so we must beware of a silent
			% crash of this server:
			%
			?myriad_spawn_link( fun() ->
				server_main_run( CallerPid, RegistrationName, FileName )
								end ),

			receive

				{ preferences_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% Returns the value associated to specified key in the preferences (if any),
% otherwise 'undefined', based on the default preferences file, and possibly
% launching a corresponding preferences server if needed.
%
-spec get( key() ) -> maybe( value() ).
get( Key ) ->
	get( Key, get_default_preferences_path() ).



% Returns the value associated to specified key in the preferences (if any),
% otherwise 'undefined', based on the specified preferences file, and possibly
% launching a corresponding preferences server if needed.
%
-spec get( key(), file_path() ) -> maybe( value() ).
get( Key, FileName ) ->

	ServerPid = start( FileName ),

	ServerPid ! { get_preference, Key, self() },

	receive

		{ notify_preference, V } ->
			V

	end.



% Associates, in default preferences, specified value to specified key (possibly
% overwriting any previous value).
%
-spec set( key(), value() ) -> void().
set( Key, Value ) ->
	set( Key, Value, get_default_preferences_path() ).



% Associates, in specified preferences, specified value to specified key
% (possibly overwriting any previous value).
%
-spec set( key(), value(), file_path() ) -> void().
set( Key, Value, FilePath ) ->

	ServerPid = start( FilePath ),

	ServerPid ! { set_preference, Key, Value }.




% Returns a textual description of the preferences server (if any), for
% the default preferences file.
%
-spec to_string() -> ustring().
to_string() ->
	to_string( get_default_preferences_path() ).



% Returns a textual description of the preferences server (if any), for
% specified preferences file.
%
-spec to_string( file_path() ) -> ustring().
to_string( FilePath ) ->

	RegistrationName = get_registration_name( FilePath ),

	case naming_utils:is_registered( RegistrationName, global ) of

		not_registered ->
			"no preferences server is running";

		ServerPid ->
			ServerPid ! { to_string, self() },

			receive

				{ notify_preferences_status, PrefString } ->
					PrefString

			end

	end.



% Returns the full, absolute path to the default preferences filename.
-spec get_default_preferences_path() -> file_utils:path().
get_default_preferences_path() ->
	file_utils:join( system_utils:get_user_home_directory(),
					 ?default_preferences_filename ).



% Returns the automatic naming used for registering the process (deduced from
% the preferences filename).
%
-spec get_registration_name( file_path() ) -> registration_name().
get_registration_name( FilePath ) ->
	CoreFilePath = file_utils:remove_upper_levels_and_extension( FilePath ),
	RegistrationName = file_utils:path_to_variable_name( CoreFilePath, "" ),
	text_utils:string_to_atom( RegistrationName ).



% Returns whether the default preferences file is available and its full path.
-spec is_preferences_default_file_available() -> { boolean(), file_path() }.
is_preferences_default_file_available() ->

	PrefFile = get_default_preferences_path(),

	Res = file_utils:is_existing_file_or_link( PrefFile ),

	{ Res, PrefFile }.



% Checks that the default preferences file exists, throws an exception
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



% Stops (asynchronously) the preferences server, if it is running.
%
% Never fails.
%
-spec stop() -> void().
stop() ->
	stop( get_default_preferences_path() ).


-spec stop( file_path() ) -> void().
stop( FilePath ) ->

	RegistrationName = get_registration_name( FilePath ),

	case naming_utils:is_registered( RegistrationName, global ) of

		not_registered ->
			ok;

		Pid ->
			Pid ! stop

	end.



% Section for the preferences server itself.


% Launcher of the preferences server.
server_main_run( SpawnerPid, RegistrationName, FilePath ) ->

	case naming_utils:register_or_return_registered( RegistrationName,
													 global_only ) of

		registered ->

			% We gain the shared name, we are the one and only server:
			EmptyTable = table:new(),

			FinalTable = case file_utils:is_existing_file_or_link( FilePath ) of

				true ->
					add_preferences_from( FilePath, EmptyTable );

				false ->
					io:format( "No preferences file found "
							   "(searched for '~ts').~n", [ FilePath ] ),
					EmptyTable

			end,

			% Spawner could already know that PID in this case:
			SpawnerPid ! { preferences_server_pid, self() },

			% Never returns:
			server_main_loop( FinalTable );


		ServerPid ->
			% Notifies and terminates:
			SpawnerPid ! { preferences_server_pid, ServerPid }

	end.



% Main loop of the preferences server.
server_main_loop( Table ) ->

	%trace_utils:debug_fmt( "Waiting for preferences-related request, "
	%    "having ~B recorded preferences.",[ table:size( Table ) ] ),

	receive

		{ get_preference, Key, SenderPid } ->

			Answer = case table:lookup_entry( Key, Table ) of

				key_not_found ->
					undefined;

				{ value, V } ->
					V

			end,

			SenderPid ! { notify_preference, Answer },

			server_main_loop( Table );


		{ set_preference, Key, Value } ->

			NewTable = table:add_entry( Key, Value, Table ),

			server_main_loop( NewTable );


		{ to_string, SenderPid } ->

			Res = case table:enumerate( Table ) of

				[] ->
					"no preferences recorded";

				L ->

					% Enforces a consistent order:
					Strings = [ text_utils:format( "~p: ~p", [ K, V ] )
								|| { K, V } <- lists:sort( L ) ],

					text_utils:format( "~B preferences recorded: ~ts~n",
						[ length( L ),
						  text_utils:strings_to_string( Strings ) ] )

			end,

			SenderPid ! { notify_preferences_status, Res },

			server_main_loop( Table );

		stop ->
			%io:format( "Stopping preferences server.~n" ),
			stopped

	end.



% Helper functions.


% Adds preferences found in specified file into specified table, and returns it.
%
% (helper)
%
add_preferences_from( FilePath, Table ) ->

	case file:consult( FilePath ) of

		{ ok, Entries } ->

			case check_entries( Entries ) of

				ok ->
					NewTable = table:add_entries( Entries, Table ),

					%io:format( "Loaded from preferences file '~ts' "
					%           "following entries: ~ts",
					% [ PrefFilePath, table:to_string( NewTable ) ] ),

				   %io:format( "Preferences file '~ts' loaded.~n",
				   %	[ FilePath ] ),

				   NewTable;

				ErrorString ->
					trace_utils:error_fmt( "Error when reading preferences "
						"file '~ts' (~ts), no preferences read.",
						[ FilePath, ErrorString ] ),
					Table

			end;


		{ error, { Line, _Mod, Term } } ->
			FlattenError = text_utils:format( "~p", [ Term ] ),
			trace_utils:error_fmt( "Error in preferences file '~ts' "
				"at line ~B (~ts), no preferences read.",
					   [ FilePath, Line, FlattenError ] ),
			Table;


		{ error, Reason } ->
			trace_utils:error_fmt( "Error when reading preferences file "
				"'~ts' (~p), no preferences read.", [ FilePath, Reason ] ),
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
