% Copyright (C) 2019-2022 Olivier Boudeville
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
% Creation date: Monday, July 15, 2019.


% @doc Various helpers for <b>OTP applications, releases</b>, and so on.
%
% Useful notably to test an OTP application from within it, from a non-OTP
% context, i.e. from a simple test without needing to create a separate,
% dedicated OTP release for that.
%
% See myriad_otp_application_test.erl as an example.
%
% Following convention is supposed to apply for testing: all applications
% (i.e. the tested one and its prerequisites) are expected to have their build
% trees (typically GIT clones) located in the same parent directory (as
% siblings), each named as its application (ex: "myriad" root directory for the
% myriad application, not for example "Ceylan-Myriad"), so that, from the build
% tree of a tested application, the build trees of its prerequisites can be
% found (ex: as "../myriad").
%
-module(otp_utils).


-type application_name() :: atom().
% Name of an OTP application (ex: 'myriad').


-type string_application_name() :: atom().
% Name of an OTP application as a string (ex: "myriad").


-type any_application_name() :: application_name() | string_application_name().
% Name of an OTP application, as any legit type.


% Not exported by the standard module:
%-type restart_type() :: application:restart_type().
-type restart_type() :: 'permanent' | 'transient' | 'temporary'.


-type supervisor_pid() :: pid().
% The PID of an OTP supervisor.

-type worker_pid() :: pid().
% The PID of an OTP worker, managed by a supervisor.


-type application_run_context() ::
		% If using Ceylan native build/run system:
		'as_native'
		% If using an OTP release (hence OTP applications as well):
	  | 'as_otp_release'.
% Designates how an (OTP) application is run.


% Naming deemed clearer:
-type supervisor_settings() :: supervisor:sup_flags().
% Settings of an OTP supervisor.



-type otp_state() :: term().
% The state of a process implementing an OTP behaviour, typically the gen_server
% one.


-type continue_data() :: term().
% Data specified to be used in a continue callback.


-type termination_reason() :: 'normal' | 'shutdown' | { 'shutdown', term() }
							| term().
% Denotes the reason for stopping.


-type handle_return() ::
	{ 'noreply', otp_state() }
  | { 'noreply', otp_state(), time_out() }
  | { 'noreply', otp_state(), 'hibernate' }
  | { 'noreply', otp_state(), { 'continue', continue_data() } }
  | { 'stop', termination_reason(), otp_state() }.
% The type corresponding to the values to be returned by
% gen_server:handle_{cast,continue,info}/2.


-export_type([ application_name/0, string_application_name/0,
			   any_application_name/0, restart_type/0,
			   supervisor_pid/0, worker_pid/0,
			   application_run_context/0, supervisor_settings/0,
			   otp_state/0, continue_data/0, termination_reason/0,
			   handle_return/0 ]).


-export([ get_string_application_name/1,

		  prepare_for_execution/2, prepare_for_execution/3,

		  start_application/1, start_application/2, start_application/3,
		  start_applications/1, start_applications/2, start_applications/3,

		  stop_application/1, stop_applications/1, stop_user_applications/1,

		  get_supervisor_settings/2, get_child_supervisor_settings/1,
		  get_restart_setting/1, get_maximum_shutdown_duration/1,

		  check_application_run_context/1, application_run_context_to_string/1,

		  get_application_root/1,

		  get_priv_root/1, get_priv_root/2 ]).


% Silencing:
-export([ app_info_to_string/1 ]).


% Shorthands:

-type module_name() :: basic_utils:module_name().
-type execution_target() :: basic_utils:execution_target().

-type file_name() :: file_utils:file_name().
-type file_path() :: file_utils:file_path().

-type directory_path() :: file_utils:directory_path().
-type bin_directory_path() :: file_utils:bin_directory_path().
-type abs_directory_path() :: file_utils:abs_directory_path().

-type ustring() :: text_utils:ustring().

-type milliseconds() :: unit_utils:milliseconds().

-type app_spec() :: list_table:list_table().
% Entries corresponding to the application specifications (see
% [https://erlang.org/doc/man/app.html]) read from a .app file.

-type time_out() :: time_utils:time_out().


-record( app_info, {

	% Stored here also only for convenience:
	app_name :: application_name(),

	% The (absolute) base root of that application:
	root_dir :: bin_directory_path(),

	% The (absolute) ebin directory root of that application:
	ebin_dir :: bin_directory_path(),

	% If set, means that it is an active application:
	start_mod_args ::
		maybe( { module_name(), basic_utils:arguments() } ),

	% As contained in its .app file:
	spec :: app_spec() } ).


-type app_info() :: #app_info{}.
% Information regarding a (generally prerequisite, OTP) application.


-type app_table() :: table( application_name(), app_info() ).
% A table allowing to look-up dependencies only once (and not many times, like
% for the kernel application).


% To easily activate/deactivate a type of traces as a whole:

% See GNUmakevars.inc to enable:

-ifdef(myriad_debug_otp_integration).

  -define( debug( M ), trace_bridge:debug( M ) ).
  -define( debug_fmt( F, V ), trace_bridge:debug_fmt( F, V ) ).

  -define( info( M ), trace_bridge:info( M ) ).
  -define( info_fmt( F, V ), trace_bridge:info_fmt( F, V ) ).

-else. % myriad_debug_otp_integration

  -define( debug( M ), trace_disabled ).
  -define( debug_fmt( F, V ), trace_disabled ).

  -define( info( M ), trace_disabled ).
  -define( info_fmt( F, V ), trace_disabled ).

-endif. % myriad_debug_otp_integration



% Implementation notes

% We found very useful to be able to stick to pure Erlang and, at least for some
% use cases, to escape from rebar3 and even OTP releases. Updating and testing
% an application was then a lot easier and quicker, with shortened fix/run
% iterations.

% As an example, see, in US-Main (http://us-main.esperide.org),
% us_main_app:exec/0.


% About the reporting of application failures.
%
% When the top-level user initial process has started the applications (ex: with
% start_application{,s}/{1,2,3}) and one of them fails, this process will
% apparently not be notified of that crash (and by default it will not even know
% the PID of the root supervisors involved).
%
% While it is certainly relevant in a production context (where that process
% shall resist application-level issues, and where separation of concerns is
% useful), this is typically a problem when this process is a test one: this
% process is then unaware of any failure (even if linking all launched processes
% and if setting all restart types to 'temporary') and the test succeeds whereas
% it should not.
%
% A solution may be, from the test, to link to the root supervisors - provided
% that some way exists to determine their PID, i.e., often, provided that these
% supervisors and/or their children are registered.



% About the blacklisting of applications.
%
% Although doing so allows not to search, load, initialise for and start such
% applications, as long as a blacklisted application is listed in the
% 'applications' key of the .app file of a parent application, it will not be
% possible to start this last application with OTP (using, in the 'application'
% module, start/{1,2} or the ensure_* functions) without starting in turn the
% blacklisted ones.
%
% In these cases we rely on the workaround of not listing such applications in
% the 'applications' key of the .app file of that parent application, and to
% start by ourselves (explicitly) these applications only if/when needed.
%
% For an example of use of that workaround, one may refer to US-Web using LEEC
% whereas shotgun was not wanted (as inducing a clash in cowlib versions when
% cowboy was needed): LEEC does not list shotgun anymore among its prerequisite
% applications, and starts it iff not relying on an alternate implementation in
% terms of http client.
%
% In a nutshell, we currently see little use in blacklisting applications, it is
% often better not to include them in the prerequisites at the first place and
% to start them explicitly only if needed.



% @doc Prepares the VM environment so that the specified top-level prerequisite
% application(s) to be involved in a non-OTP execution - and also all their own
% prerequisites recursively in turn - can be later started, based on the
% specified current base tree.
%
% @see prepare_for_execution/2
%
-spec prepare_for_execution( application_name() | [ application_name() ],
							 directory_path() ) -> [ application_name() ].
prepare_for_execution( AppName, BaseDir ) ->
	prepare_for_execution( AppName, BaseDir, _BlacklistedApps=[] ).


% @doc Prepares the VM environment so that the specified top-level prerequisite
% application(s) to be involved in a non-OTP execution - and also all their own
% prerequisites recursively in turn - can be later started, based on the
% specified current base tree, provided that they are not blacklisted.
%
% Ensures that their .app can be found (supposing thus that they are available
% and built, checks being performed), including afterwards by OTP, when starting
% them (thanks to updates to the current code path).
%
% The possibility of blacklisting an application is useful whenever a .app file
% lists applications that are actually not useful for the current application,
% or that even may induce clashes among prerequisites. Blacklisting them allows
% not searching for them, not modifying the overall code path accordingly, and
% ignoring their own dependencies. They shall then be blacklisted with
% start_applications/3 as well (otherwise an attempt of starting them will be
% done and will fail).
%
% Returns an ordered, complete list (with no duplicates) of applications names
% that shall be started in turn (otherwise throws an exception), so that each
% application is started once if active (or not at all if non-active), and after
% all its prerequisites.
%
% The specified, ordered, application list is somehow semantically similar to
% the 'deps' entry of a rebar.config file.
%
% Prerequisites that are common to multiple dependencies are supposed to be of a
% single version.
%
% Note that our previous implementation (refer to commit 5fffa10) used to
% discriminate between applications that are active or not, and to collect their
% start module/arguments; this is not necessary though, as application:start/1
% seems able to support non-active applications, and for active ones to trigger
% by itself the right start call.
%
% Application information is now cached, so that base applications (ex: kernel)
% are not repeatedly searched, but just once.
%
-spec prepare_for_execution( application_name() | [ application_name() ],
		directory_path(), [ application_name() ] ) -> [ application_name() ].
prepare_for_execution( AppName, BaseDir, BlacklistedApps )
											when is_atom( AppName ) ->
	prepare_for_execution( [ AppName ], BaseDir, BlacklistedApps );

prepare_for_execution( AppNames, BaseDir, BlacklistedApps )
			when is_list( AppNames ) andalso is_list( BlacklistedApps ) ->

	% From this entry point, we prefer to deal with absolute, normalised paths:
	AbsBaseDir = file_utils:ensure_path_is_absolute( BaseDir ),

	?debug_fmt( "Preparing for the execution from '~ts' of following top-level "
		"applications:~n  ~p, blacklisted ones being: ~p "
		"(from base directory '~ts').",
		[ AbsBaseDir, AppNames, BlacklistedApps,
		  file_utils:ensure_path_is_absolute( BaseDir ) ] ),

	{ FullDeps, _FinalAppTable } = prepare_for_exec( AppNames, AbsBaseDir,
		BlacklistedApps, _AccDeps=[], _AppTable=table:new() ),

	% After a depth-first traversal resulting in listing paths from leaves to
	% roots, we reverse the results in order to enforce a top to bottom order
	% (still with duplicates):
	%
	PreparedApps = lists:reverse( FullDeps ),

	?debug_fmt( "Pre-deduplication start lists:~n  ~p", [ PreparedApps ] ),

	% Now each prerequisite shall be started once, the first time it is useful:
	FinalApps = list_utils:uniquify_ordered( PreparedApps ),

	% Probably the most useful trace:
	?debug_fmt( "Final application list to be started in turn:~n ~p",
				[ FinalApps ] ),

	FinalApps.



% Manages specified application and, recursively, all its prerequisites (direct
% or not), if any: checks that their .app specification can be found, that they
% are built, updates the code path accordingly and lists the active ones.
%
% Called recursively (through parse_app_spec/3).
%
% (helper)
%
-spec prepare_for_exec( application_name(), abs_directory_path(),
			[ application_name() ], app_table(), [ application_name() ] ) ->
								{ [ application_name() ], app_table() }.
prepare_for_exec( _AppNames=[], _AbsBaseDir, _BlacklistedApps, AccDeps,
				  AppTable ) ->
	% Merges all prerequisites, in their (currently bottom-up) order (duplicates
	% taken care of later):
	%
	{ AccDeps, AppTable };


prepare_for_exec( [ AppName | T ], AbsBaseDir, BlacklistedApps, AccDeps,
				  AppTable ) ->

	case lists:member( AppName, BlacklistedApps ) of

		false ->
			case get_app_info( AppName, AbsBaseDir, AppTable ) of

				undefined ->

					trace_bridge:error_fmt( "No application information found "
						"for the '~ts' OTP application (searched in turn in "
						"local ebin, rebar3 _checkouts or _build directory, "
						"through any sibling applications or as a standard "
						"application; this execution thus cannot be performed "
						"(one may run beforehand, if relevant, "
						"'make rebar3-compile' at the root of the ~ts source "
						"tree for a more relevant testing).",
						[ AppName, AbsBaseDir ] ),

					throw( { lacking_app, no_relevant_directory_found, AppName,
							 AbsBaseDir } );

				{ #app_info{ root_dir=BinAppBaseDir, spec=AppEntries },
				  DirectAppTable } ->

					% All checks already performed, ebin directory already added
					% in the code path when generating application information
					% (we need to include this ebin path in the VM code path so
					% that the corresponding .app file and also the BEAM files
					% of that application can be found by OTP when starting it).
					%
					DepAppNames = list_table:get_value_with_default(
						applications, _DefNoDep=[], AppEntries ),

					?debug_fmt( "Preparing for the execution of application "
						"'~ts', whose direct dependencies are: ~w.",
						[ AppName, DepAppNames ] ),

					{ CompleteDepApps, DepAppTable } = prepare_for_exec(
						DepAppNames, BinAppBaseDir, BlacklistedApps,
						_NestedAppDeps=[], DirectAppTable ),

					prepare_for_exec( T, AbsBaseDir, BlacklistedApps,
						[ AppName | CompleteDepApps ] ++ AccDeps, DepAppTable )

			end;

		true ->
			?debug_fmt( "Ignoring application '~ts', as it is blacklisted.",
						[ AppName ] ),
			prepare_for_exec( T, AbsBaseDir, BlacklistedApps, AccDeps,
							  AppTable )

	end.



% @doc Returns the information about the specified OTP application, namely the
% executed application itself or one of its own (direct or indirect)
% prerequisites (supposing the 'default' rebar3 profile being used, if
% relevant), based on the specified root of the current base tree.
%
% Following locations will be searched for the relevant directories, from the
% root of the specified base directory, and in that order:
%
%  1. local ebin (for the executed application itself)
%  2. any local _checkouts/APP_NAME/ebin (2.1) or
%  _checkouts/APP_NAME/_build/default/lib/APP_NAME/ebin (2.2)
%  3. any local _build/default/lib/APP_NAME/ebin (for the dependencies of the
%  executed application)
%  4. any sibling ../APP_NAME/ebin (4.1) or
%  ../APP_NAME/_build/default/lib/APP_NAME/ebin (4.2)
%  5. the installed OTP system tree itself, where standard applications are
%  available (i.e. in ${ERLANG_ROOT}/lib/erlang/lib)
%
% Returning also the application base directory of this application allows to
% locate in turn any prerequisite it would have (typically through checkouts,
% local _build or siblings).
%
-spec get_app_info( application_name(), abs_directory_path(), app_table() ) ->
							{ app_info(), app_table() }.
get_app_info( AppName, AbsBaseDir, AppTable ) ->

	case table:lookup_entry( AppName, AppTable ) of

		% Already cached:
		{ value, AppInfo } ->
			{ AppInfo, AppTable };

		key_not_found ->
			generate_app_info( AppName, AbsBaseDir, AppTable )

	end.



% @doc Generates and stores the information regarding the specified application.
% Modifies the code path.
%
-spec generate_app_info( application_name(), abs_directory_path(),
						 app_table() ) -> { app_info(), app_table() }.
generate_app_info( AppName, AbsBaseDir, AppTable ) ->

	?info_fmt( "Generating information for application '~ts' from '~ts'.",
			   [ AppName, AbsBaseDir ] ),

	% We used to search only for an 'ebin' path, yet, for example in the case of
	% sibling directories, a wrong ebin directory could be selected. Now, to
	% avoid any mistake, we look up the full path of the target .app file.

	AppNameStr = get_string_application_name( AppName ),

	AppFilename = AppNameStr ++ ".app",

	% Trying location #1 (see get_app_info/3 comment):
	ThisEBinDir = file_utils:join( AbsBaseDir, "ebin" ),

	ThisAppFilePath = file_utils:join( ThisEBinDir, AppFilename ),

	?debug_fmt( "[1] Application '~ts' looked up locally based "
				"on '~ts'.", [ AppName, ThisAppFilePath ] ),

	{ AppFilePath, EBinDir, NewBaseDir } =
		case file_utils:is_existing_file_or_link( ThisAppFilePath ) of

		true ->
			?info_fmt( "Using, for the application '~ts', the directly "
					   "local '~ts' file.", [ AppName, ThisAppFilePath ] ),
			{ ThisAppFilePath, ThisEBinDir, AbsBaseDir };

		% Trying location #2.1, if this application is in a local checkout:
		false ->
			CheckBaseDir = file_utils:join(
							[ AbsBaseDir, "_checkouts", AppNameStr ] ),

			CheckLocalEBinDir = file_utils:join( CheckBaseDir, "ebin" ),

			CheckLocalAppPath =
					file_utils:join( CheckLocalEBinDir, AppFilename ),

			?debug_fmt( "[2.1] Application '~ts' not found directly "
				"in local ebin, trying in local checkout, based on '~ts'.",
				[ AppName, CheckLocalAppPath ] ),

			case file_utils:is_existing_file_or_link( CheckLocalAppPath ) of

				true ->
					?info_fmt( "Using, for the application '~ts', "
							   "the local checkout '~ts' file.",
							   [ AppName, CheckLocalAppPath ] ),
					{ CheckLocalAppPath, CheckLocalEBinDir, CheckBaseDir };

				% Then trying #2.2, i.e. as a _build checkout:
				false ->
					CheckBuildEBinDir =
						get_build_ebin_from_lib( CheckBaseDir, AppNameStr ),

					CheckBuildAppPath =
						file_utils:join( CheckBuildEBinDir, AppFilename ),

					?debug_fmt( "[2.2] Application '~ts' not found directly "
						"in local checkout, trying in _build checkout, based "
						"on '~ts'.", [ AppName, CheckBuildAppPath ] ),

					case file_utils:is_existing_file_or_link(
							CheckBuildAppPath ) of

						true ->
							?info_fmt( "Using, for the application '~ts', the "
								"the _build checkout '~ts' file.",
								[ AppName, CheckBuildAppPath ] ),
							{ CheckBuildAppPath, CheckBuildEBinDir,
							  CheckBaseDir };

						% Then trying #3, i.e. as a local build dependency:
						false ->
							DepEBinDir = get_build_ebin_from_lib( AbsBaseDir,
																  AppNameStr ),

							DepAppPath =
								file_utils:join( DepEBinDir, AppFilename ),

							?debug_fmt( "[3] Application '~ts' not found in "
								"local checkout, trying as a local build "
								"dependency, based on '~ts'.",
								[ AppName, DepAppPath ] ),

							% To avoid insane nesting:
							try_next_locations( AppName, AppNameStr,
								AppFilename, DepEBinDir, DepAppPath,
								AbsBaseDir )

					end

			end

	end,

	AppInfo = interpret_app_file( AppFilePath, AppName, EBinDir, NewBaseDir ),

	?debug( app_info_to_string( AppInfo ) ),

	NewAppTable = table:add_entry( AppName, AppInfo, AppTable ),

	% We take advantage that, for each application needed, this code is executed
	% exactly once to update the VM code path for it (so that, on start-up, the
	% OTP procedure finds its .app file and also its BEAM files):
	%
	?debug_fmt( "Expanding the code path with '~ts' for application "
				"information.", [ EBinDir ] ),

	code_utils:declare_beam_directory( EBinDir, last_position ),

	{ AppInfo, NewAppTable }.



% (helper)
try_next_locations( AppName, AppNameStr, AppFilename, DepEBinDir, DepAppPath,
					AbsBaseDir ) ->

	case file_utils:is_existing_file_or_link( DepAppPath ) of

		true ->
			?info_fmt( "Using, for the application '~ts', the "
				"local build dependency '~ts' file.", [ AppName, DepAppPath ] ),
			{ DepAppPath, DepEBinDir, AbsBaseDir };

		% Trying #4, i.e. as a sibling application:
		false ->
			% Still absolute (../APP_NAME), and normalised:
			SibBaseDir = file_utils:join(
				file_utils:get_base_path( AbsBaseDir ), AppNameStr ),

			% 4.1:
			SibLocalEbinDir = file_utils:join( SibBaseDir, "ebin" ),

			SibLocalAppPath= file_utils:join( SibLocalEbinDir, AppFilename ),

			?debug_fmt( "[4.1] Application '~ts' not found as a local "
				"build dependency, trying first as local ebin sibling, "
				"based on '~ts'.", [ AppName, SibLocalAppPath ] ),

			case file_utils:is_existing_file_or_link( SibLocalAppPath ) of

				true ->
					?info_fmt( "Using, for the application '~ts', "
						"the local ebin sibling '~ts' file.",
						[ AppName, SibLocalAppPath ] ),
					{ SibLocalAppPath, SibLocalEbinDir, SibBaseDir };

				% Trying #4.2: in the _build tree of a sibling:
				false ->
					SibBuildEbinDir =
						get_build_ebin_from_lib( SibBaseDir, AppNameStr ),

					SibBuildAppPath=
						file_utils:join( SibBuildEbinDir, AppFilename ),

					?debug_fmt( "[4.2] Application '~ts' not found as a local "
						"ebin sibling, trying as a _build sibling, "
						"based on '~ts'.", [ AppName, SibBuildAppPath ] ),

					case file_utils:is_existing_file_or_link(
										SibBuildAppPath ) of

						true ->
							?info_fmt( "Using, for the application '~ts', "
								"the _build sibling '~ts' file.",
								[ AppName, SibBuildAppPath ] ),
							{ SibBuildAppPath, SibBuildEbinDir, SibBaseDir };

						% Trying #5, i.e. as a standard OTP application:
						false ->
							?debug_fmt( "[5] Application '~ts' not found as a "
								"sibling, trying as a standard OTP "
								"application.", [ AppName ] ),

							case code:lib_dir( AppName ) of

								{ error, bad_name } ->
									trace_bridge:error_fmt( "Application '~ts' "
										"not found in any of the supported "
										"locations.", [ AppName ] ),
									throw( { application_not_found, AppName,
										text_utils:ensure_string( AbsBaseDir )
										   } );

								AbsStdPath ->
									StdEbinDir =
										file_utils:join( AbsStdPath, "ebin" ),

									StdAppPath = file_utils:join( StdEbinDir,
																  AppFilename ),

									case file_utils:is_existing_file(
												StdAppPath ) of

										true ->
											?info_fmt( "Using, for the "
												"application '~ts', "
												"the OTP '~ts' file.",
												[ AppName, StdAppPath ] ),

											{ StdAppPath, StdEbinDir,
											  AbsStdPath };

												% Abnormal:
										false ->
											throw( { otp_app_file_not_found,
													 StdAppPath } )

									end

							end

					end

			end

	end.



% @doc Returns a string version of specified application name.
-spec get_string_application_name( application_name() ) ->
										string_application_name().
get_string_application_name( AppName ) ->
	text_utils:atom_to_string( AppName ).



% @doc Returns the ebin in 'lib' subdirectory in the _build tree from specified
% base directory, for specified application.
%
-spec get_build_ebin_from_lib( directory_path(), string_application_name() ) ->
									directory_path().
get_build_ebin_from_lib( BaseDir, AppNameStr ) ->
	file_utils:join( [ BaseDir, "_build", "default", "lib", AppNameStr,
					   "ebin" ] ).


% @doc Returns the ebin in 'checkouts' directory in the _build tree from
% specified base directory, for specified application.
%
-spec get_build_ebin_from_checkouts( directory_path(),
							string_application_name() ) -> directory_path().
get_build_ebin_from_checkouts( BaseDir, AppNameStr ) ->
	file_utils:join(
		[ BaseDir, "_build", "default", "checkouts", AppNameStr, "ebin" ] ).



% @doc Searches for the BEAM file corresponding to the specified module in the
% specified ebin or build directory, otherwise through current code path.
%
-spec look_up_beam( module_name(), abs_directory_path(),
			abs_directory_path(), file_path(), application_name() ) -> void().
look_up_beam( ModuleName, EBinPath, BaseDir, AppFilePath, AppName ) ->

	TestedBeamFilename = code_utils:get_beam_filename( ModuleName ),

	ExpectedModPath = file_utils:join( EBinPath, TestedBeamFilename ),

	case file_utils:is_existing_file( ExpectedModPath ) of

		true ->
			ok;

		false ->
			% This might be normal in the case of a Ceylan application, as they
			% do not gather their BEAM files in a single (ebin) directory.
			% However then their build system shall have ensured that the right
			% directories are already in the code path. Checking that:
			%
			case code_utils:is_beam_in_path( ModuleName ) of

				not_found ->
					look_up_beam_last_chance( EBinPath, BaseDir, AppFilePath,
						AppName, TestedBeamFilename, ExpectedModPath );

				_DirPaths ->
					ok

			end

	end.


% Last chance to find a right BEAM path.
-spec look_up_beam_last_chance( file_path(), abs_directory_path(), file_path(),
					application_name(), file_name(), file_path() ) -> void().
look_up_beam_last_chance( EBinPath, BaseDir, AppFilePath, AppName,
						  TestedBeamFilename, ExpectedModPath ) ->

	% Last chance: at least with some applications such as cowboy (taken as an
	% example here), the .app is in the local ebin, with most but not all BEAM
	% files - namely then cowboy.beam is only in _build/default/lib/cowboy/ebin;
	% so we test that case and, if found, add that _build directory in the code
	% path.
	%
	% As actually the local ebin is then a strict subset of the _build one
	% (notably the .app file is also in the _build one), the best approach is
	% also to remove the local ebin from the code path, otherwise some BEAM
	% files could be found twice, which is not desirable (and detected as an
	% error in some cases, like in the case of the json_utils check, with
	% 'multiple_jsx_json_backends_found').
	%
	% Note also that :
	%
	% - a dependency may be found not (only) in 'lib' but in 'checkouts'
	% instead; for example: _build/default/checkouts/cowboy/ebin/cowboy.beam...
	%
	% - ExpectedModPath and BuildModPath may be the same path, as EBinPath and
	% BuildEbinDir may be the same apparently

	AppNameStr = text_utils:atom_to_string( AppName ),

	EbinBuildLibDir = get_build_ebin_from_lib( BaseDir, AppNameStr ),
	BuildLibModPath = file_utils:join( EbinBuildLibDir, TestedBeamFilename ),

	EbinBuildCoDir = get_build_ebin_from_checkouts( BaseDir, AppNameStr ),
	BuildCoModPath = file_utils:join( EbinBuildCoDir, TestedBeamFilename ),

	% So, maybe in _build/default/lib/cowboy/ebin?
	MaybeEbinBuildDir = case file_utils:is_existing_file( BuildLibModPath ) of

		true ->
			EbinBuildLibDir;

		% No, then maybe in _build/default/checkouts/cowboy/ebin?
		false ->

			case file_utils:is_existing_file( BuildCoModPath ) of

				true ->
					EbinBuildCoDir;

				false ->
					undefined

			end

	end,

	case MaybeEbinBuildDir of

		undefined ->
			trace_bridge:error_fmt( "The application '~ts' whose information "
				"is in '~ts' does not seem compiled, as its tested '~ts' "
				"module cannot be found as '~ts', '~ts' or '~ts', "
				"nor through the code path, which is: ~ts",
				[ AppName, AppFilePath, TestedBeamFilename,
				  ExpectedModPath, BuildLibModPath, BuildCoModPath,
				  code_utils:code_path_to_string() ] ),

			throw( { app_not_compiled, AppName, TestedBeamFilename } );


		EbinBuildDir ->
			?debug_fmt( "Replacing in code path local ebin '~ts' with the "
				"_build one '~ts' for '~ts'.",
				[ EBinPath, EbinBuildDir, AppName ] ),

			% First removing local ebin (if set):
			code_utils:remove_beam_directory_if_set( EBinPath ),

			% Then adding the more complete _build one:
			code_utils:declare_beam_directory( EbinBuildDir, last_position )

	end.



% @doc Interprets the specification of the application whose .app file is
% specified.
%
% Returns the specification of the specified application, as read from its
% specified (supposedly already checked for existence) .app file.
%
% (helper)
%
-spec interpret_app_file( file_path(), application_name(),
				abs_directory_path(), abs_directory_path() ) -> app_spec().
interpret_app_file( AppFilePath, AppName, EBinPath, BaseDir ) ->

	?debug_fmt( "Examining application specification in '~ts'.",
				[ AppFilePath ] ),

	case file_utils:read_terms( AppFilePath ) of

		[ { application, AppName, Entries } ] ->

			ActiveInfo = list_table:get_value_with_default( mod,
												_Def=undefined, Entries ),

			% To check whether this application is compiled, we cannot rely on
			% the 'mod' entry, which is defined only for *active* applications,
			% so:
			%
			case list_table:lookup_entry( modules, Entries ) of

				% Abnormal, as mandatory:
				key_not_found ->
					throw( { no_modules_entry, AppName, AppFilePath } );

				{ value, [] } ->
					% No module declared (weird); supposing that alles gut:
					%trace_bridge:warning_fmt( "Application '~ts' did not "
					%   "declare any module; supposing that it is fully built.",
					%   [ AppName ] ),
					ok;

				% Testing just the first module found:
				{ value, [ FirstModName | _ ] } ->
					look_up_beam( FirstModName, EBinPath, BaseDir, AppFilePath,
								  AppName )

			end,

			#app_info{ app_name=AppName,
					   % They might be already binaries if having gone through
					   % another app_info:
					   %
					   root_dir=text_utils:ensure_binary( BaseDir ),
					   ebin_dir=text_utils:ensure_binary( EBinPath ),
					   start_mod_args=ActiveInfo,
					   spec=Entries };

		_ ->
			throw( { invalid_app_spec, AppFilePath, AppName } )

	end.



% @doc Returns a textual representation of specified application information.
-spec app_info_to_string( app_info() ) -> ustring().
app_info_to_string( #app_info{ app_name=AppName,
							   root_dir=RootDir,
							   ebin_dir=EBinDir,
							   start_mod_args=ActiveInfo,
							   spec=Entries } ) ->

	ActStr = case ActiveInfo of

		undefined ->
			"not an active application";

		{ ModName, Args } ->
			text_utils:format( "active application (to be run as "
				"~ts:start(~p))", [ ModName, Args ] )

	end,

	%Longer = true,
	Longer = false,

	case Longer of

		true ->
			text_utils:format( "Application information for '~ts': root "
				"directory is '~ts', ebin one is '~ts', ~ts, "
				"and spec is:~n  ~p",
				[ AppName, RootDir, EBinDir, ActStr, Entries ] );

		false ->
			text_utils:format( "Application information for '~ts': root "
				"directory is '~ts', ebin one is '~ts', ~ts, spec having "
				"~B entries",
				[ AppName, RootDir, EBinDir, ActStr, length( Entries ) ] )

	end.



% @doc Starts the specified OTP application, with the 'temporary' restart type.
%
% Note: all prerequisite applications shall have been started beforehand
% (not relying on OTP here, hence no automatic start of dependencies).
%
-spec start_application( application_name() ) -> void().
start_application( AppName ) ->
	start_application( AppName, _RestartType=temporary ).


% @doc Starts the specified OTP application, with the specified restart type.
%
% Note: all prerequisite applications shall have been started beforehand
% (not relying on OTP here, hence no automatic start of dependencies).
%
-spec start_application( application_name(), restart_type() ) -> void().
start_application( AppName, RestartType ) ->
	start_application( AppName, RestartType, _BlacklistedApps=[] ).



% @doc Starts the specified OTP application, with the specified restart type,
% blacklisting specified applications.
%
% Note: all prerequisite applications shall have been started beforehand
% (not relying on OTP here, hence no automatic start of dependencies).
%
-spec start_application( application_name(), restart_type(),
						 [ application_name() ] ) -> void().
start_application( AppName, RestartType, BlacklistedApps ) ->

	?info_fmt( "Starting the '~ts' OTP application, with restart "
		"type '~ts', whereas blacklisted applications are: ~p.",
		[ AppName, RestartType, BlacklistedApps ] ),

	case lists:member( AppName, BlacklistedApps ) of

		false ->

			case application:start( AppName, RestartType ) of

				ok ->
					?info_fmt( "Application '~ts' successfully started.",
							   [ AppName ] ),
					ok;

				{ error, Reason } ->
					trace_bridge:error_fmt(
						"Application '~ts' failed to start: ~p",
						[ AppName, Reason ] ),

					throw( { app_start_failed, AppName, RestartType, Reason } )

			end;

		true ->
			?debug_fmt( "Not starting application '~ts', as it is blacklisted.",
						[ AppName ] )

	end.



% @doc Starts each of the specified OTP applications (if not started yet),
% sequentially and in the specified order, all with the 'temporary' restart
% type.
%
% Note: any non-included prerequisite application shall have been started
% beforehand (not relying on OTP here, hence no automatic start of
% dependencies).
%
-spec start_applications( [ application_name() ] ) -> void().
start_applications( AppNames ) ->
	start_applications( AppNames, _RestartType=temporary ).



% @doc Starts each of the specified OTP applications (if not started yet),
% sequentially and in the specified order, all with the specified restart type,
% while not blacklisting any specific application.
%
% Note: any non-included prerequisite application shall have been started
% beforehand (not relying on OTP here, hence no automatic start of
% dependencies).
%
-spec start_applications( [ application_name() ], restart_type() ) -> void().
start_applications( AppNames, RestartType ) ->
	start_applications( AppNames, RestartType, _BlacklistedApps=[] ).



% @doc Starts each of the specified OTP applications (if not started yet, and
% provided it is not blacklisted), sequentially and in the specified order, all
% with the specified restart type.
%
% Note: any non-included prerequisite application shall have been started
% beforehand (not relying on OTP here, hence no automatic start of
% dependencies).
%
-spec start_applications( [ application_name() ], restart_type(),
						  [ application_name() ] ) -> void().
start_applications( _AppNames=[], _RestartType, _BlacklistedApps ) ->
	ok;

start_applications( [ AppName | T ], RestartType, BlacklistedApps ) ->

	%?debug_fmt( "Starting OTP application '~ts' with restart type '~ts', "
	%   "whereas blacklisted applications are: ~p.",
	%   [ AppName, RestartType, BlacklistedApps ] ),

	case lists:member( AppName, BlacklistedApps ) of

		false ->
			% Not needing to use our knowledge about this application being
			% active or not:
			%
			case application:ensure_started( AppName, RestartType ) of

				ok ->
					start_applications( T, RestartType, BlacklistedApps );

				{ error, Reason } ->
					throw( { start_failed, AppName, Reason } )

			end;

		true ->
			?debug_fmt( "Not starting application '~ts', as it is blacklisted.",
						[ AppName ] ),

			start_applications( T, RestartType, BlacklistedApps )

	end.



% @doc Stops the specified OTP application.
%
% Note: not relying on OTP here, hence dependencies shall be explicitly stopped,
% in the reverse order of the starting of these applications.
%
-spec stop_application( application_name() ) -> void().
stop_application( _AppName=kernel ) ->
	% No output of any sort as the VM is then stopped, otherwise an
	% {terminated,[{io,format,... exception is raised:
	%
	?info( "Stopping finally the 'kernel' application." ),
	application:stop( kernel );

stop_application( AppName ) ->

	?info_fmt( "Stopping the '~ts' OTP application.", [ AppName ] ),
	%trace_utils:debug_fmt( "Stopping the '~ts' application.", [ AppName ] ),

	case application:stop( AppName ) of

		ok ->
			?info_fmt( "OTP Application '~ts' successfully stopped.",
					   [ AppName ] );

		{ error, { not_started, AppName } } ->

			% This is not even a warning, as a given prerequisite application
			% (ex: 'ssl') may be used by multiple applications that, when they
			% will stop, cannot each succeed in stopping that single
			% prerequisitea application.

			?info_fmt( "The OTP application '~ts' was already stopped.",
					   [ AppName ] );

		{ error, Reason } ->

			trace_bridge:error_fmt( "Application '~ts' failed to stop: ~p",
									[ AppName, Reason ] ),

			throw( { app_stop_failed, AppName, Reason } )

	end.



% @doc Stops the specified OTP applications, sequentially and in the *reverse*
% order of the specified one (so that the same list of prerequisite applications
% can be used both for starting and stopping).
%
% Note: not relying on OTP here, hence dependencies shall be explicitly stopped,
% in the reverse order of the starting of these applications.
%
-spec stop_applications( [ application_name() ] ) -> void().
stop_applications( AppNames ) ->
	[ stop_application( App ) || App <- lists:reverse( AppNames ) ].



% @doc Stops the specified user (non-VM essential) OTP applications,
% sequentially and in the *reverse* order of the specified one (so that the same
% list of prerequisite applications can be used both for starting and stopping).
%
% Not stopping the base Erlang applications allows to remain with a functional
% VM (ex: able to finish a test, to perform console outputs, etc.).
%
% Note: not relying on OTP here, hence dependencies shall be explicitly stopped,
% in the reverse order of the starting of these applications.
%
-spec stop_user_applications( [ application_name() ] ) -> void().
stop_user_applications( AppNames ) ->
	BaseVMApps = [ kernel, stdlib ],
	[ stop_application( App ) || App <- lists:reverse( AppNames ),
								 not lists:member( App, BaseVMApps ) ].



% @doc Returns the supervisor-level settings corresponding to the specified
% restart strategy and execution context.
%
% Note that the execution context must be explicitly specified here (typically
% by calling a get_execution_target/0 function defined in a key module of that
% layer, based on Myriad's basic_utils.hrl), as otherwise the one that would
% apply is the one of Myriad - not the one of the calling layer.
%
% See [https://erlang.org/doc/design_principles/sup_princ.html#supervisor-flags]
% for further information.
%
-spec get_supervisor_settings( supervisor:strategy(), execution_target() ) ->
												supervisor_settings().
get_supervisor_settings( RestartStrategy, _ExecutionTarget=development ) ->

	% No restart wanted in development mode; we do not want the supervisor to
	% hide crashes, and when an error occurs we want to see it logged once, not
	% as a longer series of that same error:
	%
	#{ strategy      => RestartStrategy,
	   intensity     => _MaxRestarts=0,
	   period        => _WithinSeconds=3600,
	   auto_shutdown => never };

get_supervisor_settings( RestartStrategy, _ExecutionTarget=production ) ->

	% In production mode, we apply here basic defaults (actually the same as
	% used by the 'kernel' standard module in safe mode):
	%
	#{ strategy      => RestartStrategy,
	   intensity     => _MaxRestarts=4,
	   period        => _WithinSeconds=3600,
	   auto_shutdown => never }.



% @doc Returns supervisor-level settings for a supervisor that would be
% supervised by a supervisor relying on the specified settings.
%
% Not tremendously useful.
%
-spec get_child_supervisor_settings( supervisor_settings() ) ->
												supervisor_settings().
get_child_supervisor_settings(
				ParentSupSettings=#{ intensity := ParentIntensity } ) ->

	ChildIntensity = max( ParentIntensity div 2, 1 ),

	ParentSupSettings#{ intensity := ChildIntensity }.



% @doc Returns default, base restart settings depending on the specified
% execution target.
%
% Note that the execution context must be explicitly specified here (typically
% by calling a get_execution_target/0 function defined in a key module of that
% layer, based on Myriad's basic_utils.hrl), as otherwise the one that would
% apply is the one of Myriad - not the one of the calling layer.
%
-spec get_restart_setting( execution_target() ) -> supervisor:restart().
get_restart_setting( _ExecutionTarget=development ) ->
	% In development, failing as clearly as possible; here at least so that
	% tests fail in case of problem (ex: if no configuration file is found):
	%
	_NeverRestarted=temporary;

get_restart_setting( _ExecutionTarget=production ) ->
	% In production, as reliable as possible:
	_AlwaysRestarted=permanent.



% @doc Returns a default, reasonable shutdown maximum duration to be used by a
% supervisor for its basic OTP workers, depending on the specified execution
% target.
%
-spec get_maximum_shutdown_duration( execution_target() ) -> milliseconds().
get_maximum_shutdown_duration( _ExecutionTarget=development ) ->
	2000;

get_maximum_shutdown_duration( _ExecutionTarget=production ) ->
	10000.



% @doc Checks that the specified application run context is legit.
-spec check_application_run_context( application_run_context() ) -> void().
check_application_run_context( _AppRunContext=as_native ) ->
	ok;

check_application_run_context( _AppRunContext=as_otp_release ) ->
	ok;

check_application_run_context( OtherAppRunContext ) ->
	throw( { invalid_application_run_context, OtherAppRunContext } ).



% @doc Returns a textual representation of specified application run context.
-spec application_run_context_to_string( application_run_context() ) ->
												ustring().
application_run_context_to_string( _AppRunContext=as_native ) ->
	"based on the Ceylan native build/run system";

application_run_context_to_string( _AppRunContext=as_otp_release ) ->
	"as an OTP application".



% @doc Returns the path to the root of the specified OTP application (i.e. its
% top directory, a.k.a. its "library directory"), for an application located
% under $OTPROOT/lib or on a directory referred to with environment variable
% ERL_LIBS.
%
% Ex: "/home/bond/Software/Erlang/Erlang-24.2/lib/erlang/lib/mnesia-4.20.1" =
%               otp_utils:get_application_root( mnesia ).
%
% For this look-up, it is expected that an application named 'foobar' defined a
% module of the same name (which is thus expected to be compiled as
% "foobar.beam").
%
-spec get_application_root( application_name() ) -> directory_path().
get_application_root( AppName ) ->
	case code:lib_dir( AppName ) of

		{ error, bad_name } ->
			% Alternative look-up (workaround):
			AppMod = text_utils:format( "~ts.beam", [ AppName ] ),
			case code:which( AppName ) of

				ModPath when is_list( ModPath ) ->
					% Hopefully a full path:
					[ AppMod, "ebin" | RevPath ] = lists:reverse(
						file_utils:split( ModPath ) ),
					file_utils:join( lists:reverse( RevPath ) );

				non_existing ->
					throw( { unable_to_locate_application, AppName,
							 no_main_module } );

				Other -> % i.e.cover_compiled | preloaded
					throw( { unable_to_locate_application, AppName,
							 Other } )

			end;

		DirPath ->
			DirPath

	end.



% @doc Returns the root directory of the application-private tree ('priv'),
% based on specified module (expected to belong to that application).
%
% It may be useful to fetch data or NIF code for example.
%
% One may specify ?MODULE as argument, provided that this module belongs to the
% application of interest.
%
-spec get_priv_root( module_name() ) -> directory_path().
get_priv_root( ModuleName ) ->
	get_priv_root( ModuleName, _BeSilent=false ).



% @doc Returns the root directory of the application-private tree ('priv'),
% based on specified module (expected to belong to that application), being
% silent (no trace) if requested.
%
% It may be useful to fetch data or NIF code for example.
%
% One may specify ?MODULE as argument, provided that this module belongs to the
% application of interest.
%
-spec get_priv_root( module_name(), boolean() ) -> directory_path().
get_priv_root( ModuleName, BeSilent ) ->

   case code:priv_dir( ModuleName ) of

		% May happen even if being listed in the 'modules' entry of the relevant
		% .app/.app.src files.
		%
		{ error, PError } ->

			% PError=bad_name, not that useful:
			case BeSilent of

				true ->
					ok;

				false ->
					trace_bridge:warning_fmt( "Unable to determine the 'priv' "
						"directory from module '~ts': ~w.",
						[ ModuleName, PError ] )

			end,

			case code:which( ModuleName ) of

				WError when is_atom( WError ) ->
					throw( { priv_lookup_failed, WError } );

				ObjectCodePath -> % when is_list( ObjectCodePath ) ->
					EbinDir = file_utils:get_base_path( ObjectCodePath ),
					AppPath = file_utils:get_base_path( EbinDir ),
					file_utils:join( AppPath, "priv" )

			end;

		Path ->
			Path

	end.
