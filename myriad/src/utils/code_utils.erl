% Copyright (C) 2007-2021 Olivier Boudeville
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


% Gathering of various facilities regarding the management of Erlang code
% (typically BEAM files).
%
% See code_utils_test.erl for the corresponding test.
%
-module(code_utils).


-export([ get_code_for/1, get_md5_for_loaded_module/1,
		  get_md5_for_stored_module/1, is_loaded_module_same_on_filesystem/1,
		  deploy_modules/2, deploy_modules/3,
		  declare_beam_directory/1, declare_beam_directory/2,
		  declare_beam_directories/1, declare_beam_directories/2,
		  remove_beam_directory/1, remove_beam_directory_if_set/1,
		  get_beam_dirs_for/1, get_beam_dirs_for_myriad/0,
		  declare_beam_dirs_for/1, declare_beam_dirs_for_myriad/0,
		  get_code_path/0, get_code_path_as_string/0,
		  code_path_to_string/0, code_path_to_string/1,
		  list_beams_in_path/0, get_beam_filename/1, is_beam_in_path/1,
		  get_erlang_root_path/0,
		  get_stacktrace/0, get_stacktrace/1,
		  interpret_stacktrace/0,
		  interpret_stacktrace/1,
		  interpret_stacktrace/2,
		  interpret_stack_item/2,
		  interpret_shortened_stacktrace/1,
		  display_stacktrace/0,
		  interpret_undef_exception/3, stack_location_to_string/1 ]).



% The code path used by a language, i.e. a list of directories to scan for
% runtime elements (Erlang -pa/-pz, Python sys.path with PYTHONPATH, Java
% classpath, etc.)
%
-type code_path() :: [ directory_path() ].

-type code_path_position() :: 'first_position' | 'last_position'.

%-type stack_location() :: [ { file, file_path() },
%							 { line, meta_utils:line() } ].

-type stack_location() :: [ { atom(), any() } ].


-type stack_item() :: { module_name(), function_name(), arity(),
						stack_location() }.


-type stack_trace() :: [ stack_item() ].


-export_type([ code_path/0, code_path_position/0,
			   stack_location/0, stack_item/0, stack_trace/0 ]).


% The file extension of a BEAM file:
-define( beam_extension, ".beam" ).


% For the file_info record:
-include_lib("kernel/include/file.hrl").


% Shorthands:

-type module_name() :: basic_utils:module_name().
-type function_name() :: basic_utils:function_name().
-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type directory_path() :: file_utils:directory_path().
-type file_name() :: file_utils:file_name().
-type file_path() :: file_utils:file_path().

-type atom_node_name() :: net_utils:atom_node_name().

-type env_variable_name() :: system_utils:env_variable_name().

-type time_out() :: time_utils:time_out().

-type md5_sum() :: executable_utils:md5_sum().


% Code-related functions.


% Returns, by searching the code path, the in-file object code for specified
% module, i.e. a {ModuleBinary, ModuleFilename} pair for the module specified as
% an atom, or throws an exception.
%
-spec get_code_for( module_name() ) -> { binary(), file_path() }.
get_code_for( ModuleName ) ->

	%trace_utils:debug_fmt( "Getting code for module '~ts', "
	%					   "from current working directory '~ts'.",
	%					   [ ModuleName, file_utils:get_current_directory() ] ),

	case code:get_object_code( ModuleName ) of

		{ ModuleName, ModuleBinary, ModuleFilename } ->
			{ ModuleBinary, ModuleFilename };

		error ->

			FoundBeams = list_beams_in_path(),

			ModString= text_utils:atoms_to_string( FoundBeams ),

			trace_utils:error_fmt( "Unable to find object code for '~ts' "
				"on '~ts', knowing that the current directory is ~ts and "
				"the ~ts~n The corresponding found BEAM files are: ~ts",
				[ ModuleName, node(), file_utils:get_current_directory(),
				  get_code_path_as_string(), ModString ] ),

			throw( { module_code_lookup_failed, ModuleName } )

	end.



% Returns the MD5 for the specified loaded (in-memory, used by the VM) module.
%
% Otherwise returns a undefined function exception (ModuleName:module_info/1).
%
-spec get_md5_for_loaded_module( module_name() ) -> md5_sum().
get_md5_for_loaded_module( ModuleName ) ->
	ModuleName:module_info( md5 ).



% Returns the MD5 for the specified stored (on filesystem, found through the
% code path) module.
%
-spec get_md5_for_stored_module( module_name() ) -> md5_sum().
get_md5_for_stored_module( ModuleName ) ->
	{ BinCode, _ModuleFilename } = get_code_for( ModuleName ),
	{ ok, { ModuleName, MD5SumBin } } = beam_lib:md5( BinCode ),
	binary_to_integer( MD5SumBin, _Base=16 ).



% Tells whether the specified (supposedly loaded) module is the same as the one
% found through the code path.
%
-spec is_loaded_module_same_on_filesystem( module_name() ) -> boolean().
is_loaded_module_same_on_filesystem( ModuleName ) ->
	LoadedMD5 = get_md5_for_loaded_module( ModuleName ),
	StoredMD5 = get_md5_for_stored_module( ModuleName ),
	%io:format( "Loaded MD5: ~p~nStored MD5: ~p~n", [ LoadedMD5, StoredMD5 ] ),
	LoadedMD5 == StoredMD5.



% RPC default time-out, in milliseconds:
% (45s, could be infinity as well)
-define( rpc_timeout, 45*1000 ).



% Deploys the specified list of modules on the specified list of nodes
% (specified as atoms): sends them these modules (as a binary), and loads them
% so that they are ready for future use, using a default time-out.
%
% If an exception is thrown with 'badfile' being reported as the error, this may
% be caused by a version mistmatch between the Erlang environments in the source
% and at least one of the remote target hosts (ex: ERTS 5.5.2 vs 5.8.2).
%
-spec deploy_modules( [ module() ], [ atom_node_name() ] ) -> void().
deploy_modules( Modules, Nodes ) ->
	deploy_modules( Modules, Nodes, _Timeout=?rpc_timeout ).



% Deploys the specified list of modules on the specified list of nodes
% (specified as atoms): sends them these modules (as a binary), and loads them
% so that they are ready for future use.
%
% Timeout is the time-out duration, either an integer number of milliseconds, or
% the 'infinity' atom.
%
% If an exception is thrown with 'badfile' being reported as the error, this may
% be caused by a version mistmatch between the Erlang environments in the source
% and at least one of the remote target hosts (ex: ERTS 5.5.2 vs 5.8.2).
%
-spec deploy_modules( [ module() ], [ atom_node_name() ], time_out() ) ->
							void().
deploy_modules( Modules, Nodes, Timeout ) ->

	% At least until the next version to come after R14B02, there was a possible
	% race condition here, as, on an a just-launched (local) node, the rpc
	% server could start to serve requests (ex: load_binary ones for file_utils)
	% whereas the code server was not registered yet (as code_server), resulting
	% in following type of error:
	%
	% {badrpc,{'EXIT',{badarg,[{code_server,call,2},
	% {rpc,'-handle_call_call/6-fun-0-',5}]}}}
	%
	% So here we should poll until the code_server can be found registered on
	% each of the remote nodes:
	%
	naming_utils:wait_for_remote_local_registrations_of( code_server, Nodes ),

	%trace_utils:debug_fmt( "Getting code for modules ~p, on ~ts, "
	%	"whereas code path (evaluated from ~ts) is:~n  ~p",
	%	[ Modules, node(), file_utils:get_current_directory(),
	%     code:get_path() ] ),

	% Then for each module in turn, contact each and every node, in parallel:
	[ deploy_module( M, get_code_for( M ), Nodes, Timeout ) || M <- Modules ].



% (helper function)
-spec deploy_module( module(), { binary(), file_path() }, [ atom_node_name() ],
					 time_out() ) -> void().
deploy_module( ModuleName, { ModuleBinary, ModuleFilename }, Nodes, Timeout ) ->

	%trace_utils:debug_fmt( "Deploying module '~ts' (filename '~ts') "
	%   "'on nodes ~p with time-out ~p.",
	%   [ ModuleName, ModuleFilename, Nodes, Timeout ] ),

	{ ResList, BadNodes } = rpc:multicall( Nodes, code, load_binary,
				[ ModuleName, ModuleFilename, ModuleBinary ], Timeout ),

	%trace_utils:debug_fmt( "ResList = ~p, BadNodes = ~p~n",
	%                       [ ResList, BadNodes ] ),

	ReportedErrors = [ E || E <- ResList, E =/= { module, ModuleName } ],
	%trace_utils:debug_fmt( "Reported errors: ~p~n", [ ReportedErrors ] ),

	case BadNodes of

		[] ->
			case ReportedErrors of

				[] ->
					%trace_utils:debug_fmt( "Module '~ts' successfully "
					%    "deployed on ~p.~n", [ ModuleName, Nodes ] ),
					ok;

				_ ->
					% Preferring returning the full list, rather than
					% ReportedErrors:
					%
					throw( { module_deployment_failed, ModuleName, ResList } )

			end;

		_ ->
			throw( { module_deployment_failed, ModuleName,
						{ ResList, BadNodes } } )

	end.

	% Optionally, do some checking:
	% Check = [ { N, rpc:call( N, code, is_loaded, [ ModuleName ] ) }
	%   || N <- Nodes ],

	% % Performs two tasks, error selection and badrpc removal:
	% RPCErrors = [ {N,Reason} || { N, {badrpc,Reason} } <- Check ],
	% LoadFailingNodes = [ N || { N, false } <- Check ],
	% case RPCErrors of

	%	[] ->

	%		case LoadFailingNodes of

	%			[] ->
	%				ok;

	%			_ ->
	%				throw( { deploy_module_checking_failed, LoadFailingNodes } )

	%		end;

	%	_ ->
	%		throw( { deploy_module_checking_error, RPCErrors, LoadFailingNodes }
	% )

	% end.



% Declares specified directory as an additional code path where BEAM files will
% be looked up by the VM, adding it at first position in the code path.
%
% If this directory is already present in the code path, it is removed from its
% old position and put first.
%
% Throws an exception if the directory does not exist.
%
-spec declare_beam_directory( directory_path() ) -> void().
declare_beam_directory( Dir ) ->
	declare_beam_directory( Dir, first_position ).



% Declares specified directory as an additional code path where BEAM files will
% be looked up by the VM, adding it as specified, at either first or last
% position in the code path.
%
% These functions ensure not to offset any given directory that was already in
% the code path further in the code path.
%
% Indeed, if this directory is already present in the code path:
% - if first_position is specified, it is removed from its old position and put
% first
% - if last_position is specified, code path is not changed
%
% Throws an exception if the directory does not exist.
%
-spec declare_beam_directory( directory_path(), code_path_position() ) ->
									void().
declare_beam_directory( Dir, first_position ) ->

	cond_utils:if_defined( myriad_debug_code_path,
		trace_utils:debug_fmt( "Declaring in first position BEAM directory "
			"'~ts' in VM code path.", [ Dir ] ) ),

	% No need to check directory for existence, code:add_patha/1 will do it:
	case code:add_patha( Dir ) of

		true ->
			ok;

		{ error, bad_directory } ->
			throw( { non_existing_beam_directory, Dir } )

	end;

declare_beam_directory( Dir, last_position ) ->

	cond_utils:if_defined( myriad_debug_code_path,
	  trace_utils:debug_fmt( "Declaring in last position BEAM directory '~ts' "
							 "in VM code path.", [ Dir ] ) ),

	% No need to check directory for existence, code:add_pathz/1 will do it:
	case code:add_pathz( Dir ) of

		true ->
			ok;

		{ error, bad_directory } ->
			throw( { non_existing_beam_directory, Dir } )

	end.



% Declares specified directories as additional code paths where BEAM files will
% be looked up by the VM, adding them at first position in the code path.
%
% Throws an exception if at least one of the directories does not exist.
%
-spec declare_beam_directories( code_path() ) -> void().
declare_beam_directories( Dirs ) ->
	declare_beam_directories( Dirs, first_position ).



% Declares specified directories as additional code paths where BEAM files will
% be looked up by the VM, adding them either at first or last position in the
% code path.
%
% Throws an exception if at least one of the directories does not exist.
%
-spec declare_beam_directories( code_path(), code_path_position() ) -> void().
declare_beam_directories( Dirs, first_position ) ->

	cond_utils:if_defined( myriad_debug_code_path,
	  trace_utils:debug_fmt( "Declaring in first position BEAM directories ~ts "
		"in VM code path.", [ text_utils:strings_to_listed_string( Dirs ) ] ) ),

	% As code:add_pathsa/1 does not report non-existing directories:
	check_beam_dirs( Dirs ),

	code:add_pathsa( Dirs );


declare_beam_directories( Dirs, last_position ) ->

	cond_utils:if_defined( myriad_debug_code_path,
	  trace_utils:debug_fmt( "Declaring in last position BEAM directories ~ts "
		"in VM code path.", [ text_utils:strings_to_listed_string( Dirs ) ] ) ),

	% As code:add_pathsz/1 does not report non-existing directories:
	check_beam_dirs( Dirs ),

	code:add_pathsz( Dirs ).



% Checks that specified directories exist.
%
% (helper)
%
check_beam_dirs( _Dirs=[] ) ->
	ok;

check_beam_dirs( _Dirs=[ D | T ] ) ->

	% We allow symlinks (ex: for ~/Software/X/X-current-install):
	case file_utils:is_existing_directory_or_link( D ) of

		true ->
			check_beam_dirs( T );

		false ->
			throw( { non_existing_beam_directory, D } )

	end.



% Removes specified directory (either specified verbatim or designated as the
% ebin directory of a specified application) from the current code path.
%
% Throws an exception if the operation failed, including if it was not already
% set.
%
-spec remove_beam_directory(
		directory_path() | otp_utils:application_name() ) -> void().
remove_beam_directory( NameOrDir ) ->

	cond_utils:if_defined( myriad_debug_code_path,
	  trace_utils:debug_fmt( "Removing directory designated by '~ts' "
							 "from VM code path.", [ NameOrDir ] ) ),

	case code:del_path( NameOrDir ) of

		true ->
			ok;

		false ->
			throw( { directory_not_found_in_code_path, NameOrDir } );

		{ error, bad_name }  ->
			throw( { invalid_app_name_for_code_path_removal, NameOrDir } )

	end.



% Removes specified directory (either specified verbatim or designated as the
% ebin directory of a specified application) from the current code path, if it
% was already set (otherwise does nothing).
%
% Throws an exception if the operation failed otherwise.
%
-spec remove_beam_directory_if_set(
			directory_path() | otp_utils:application_name() ) -> void().
remove_beam_directory_if_set( NameOrDir ) ->

	cond_utils:if_defined( myriad_debug_code_path,
	  trace_utils:debug_fmt( "Removing directory designated by '~ts' "
							 "from VM code path (if set).", [ NameOrDir ] ) ),

	case code:del_path( NameOrDir ) of

		true ->
			ok;

		false ->
			ok;

		{ error, bad_name }  ->
			throw( { invalid_app_name_for_code_path_removal, NameOrDir } )

	end.



% Returns the (ordered) list of (absolute) runtime BEAM directories obtained
% from the build system located in the directory designated as the value
% associated to the specified environment variable name.
%
% Allows to obtain the code path that shall be declared to the VM so that all
% the corresponding BEAMs become available.
%
% Note: all code run from that function shall rely on plain Erlang, so that
% Myriad itself can be made available with that module. As a result, this module
% can be copied or simply symlinked from any directory, and will be usable
% (regarding the get_beam_dirs_for* functions) from there as such (i.e. with no
% specific extra prerequisite to take into account).
%
% Ex: get_beam_dirs_for( "CEYLAN_MYRIAD" ).
%
-spec get_beam_dirs_for( env_variable_name() ) -> code_path().
get_beam_dirs_for( VariableName ) ->

	case os:getenv( VariableName ) of

		false ->
			throw( { env_variable_not_set, VariableName } );

		BaseDir ->
			case file:read_link_info( BaseDir ) of

				{ ok, #file_info{ type=directory } } ->
					ok;

				{ ok, #file_info{ type=symlink } } ->
					ok;

				{ ok , #file_info{ type=OtherType } } ->
					throw( { invalid_filesystem_entry, OtherType, BaseDir } );

				{ error, E } ->
					throw( { directory_lookup_error, E, BaseDir } )

			end,

			Command = io_lib:format(
						"cd ~ts && make list-beam-dirs 2>/dev/null",
						[ BaseDir ] ),

			Dirs = string:tokens( os:cmd( Command ), _Sep="\n" ),
			%io:format( "Dirs:~n~p", [ Dirs ] )
			Dirs

	end.



% Returns the (ordered) list of (absolute) runtime BEAM directories
% corresponding to this layer (i.e. the Ceylan-Myriad one).
%
% Allows to obtain the code path that shall be declared to the VM so that all
% the corresponding BEAMs become available.
%
% The CEYLAN_MYRIAD environment variable must be defined and must point to the
% corresponding root directory.
%
% The layer top-level 'ebin' directory could be used for that now that OTP
% conventions are used.
%
% Note: all code run from that function shall rely on plain Erlang, so that
% Myriad itself can be made available with that module.
%
-spec get_beam_dirs_for_myriad() -> code_path().
get_beam_dirs_for_myriad() ->
	% Expected to be set by convention in the environment:
	get_beam_dirs_for( "CEYLAN_MYRIAD" ).



% Declares automatically the relevant BEAM directories in the code path so that
% the layer whose base directory is designated as the value associated to the
% specified environment variable name is fully usable from then on.
%
% Note: the determined directories are not specifically checked for existence,
% and are added at the end of the code path.
%
-spec declare_beam_dirs_for( env_variable_name() ) -> void().
declare_beam_dirs_for( VariableName ) ->
	code:add_pathsz( get_beam_dirs_for( VariableName ) ).



% Declares automatically the relevant BEAM directories in the code path so that
% Ceylan-Myriad can be fully usable from then on.
%
% Note:
%
% - the CEYLAN_MYRIAD environment variable must be defined and must point to the
% corresponding root directory
%
% - the determined directories are not specifically checked for existence, and
% are added at the end of the code path
%
-spec declare_beam_dirs_for_myriad() -> void().
declare_beam_dirs_for_myriad() ->
	code:add_pathsz( get_beam_dirs_for_myriad() ).



% Returns a normalised, sorted list of directories in the current code path
% (without duplicates).
%
-spec get_code_path() -> code_path().
get_code_path() ->

	NormalisedPaths =
		[ file_utils:normalise_path( P ) || P <- code:get_path() ],

	lists:sort( list_utils:uniquify( NormalisedPaths ) ).



% Returns a textual representation of the current code path.
-spec get_code_path_as_string() -> ustring().
get_code_path_as_string() ->
	text_utils:format( "current code path is: ~ts",
					   [ text_utils:strings_to_string( get_code_path() ) ] ).



% Returns a textual description of the current code path.
-spec code_path_to_string() -> ustring().
code_path_to_string() ->
	code_path_to_string( get_code_path() ).


% Returns a textual description of the specified code path.
-spec code_path_to_string( code_path() ) -> ustring().
code_path_to_string( _CodePath=[] ) ->
	% Initial space intended for caller-side consistency:
	"empty code path";

code_path_to_string( CodePath ) ->
	text_utils:strings_to_enumerated_string( CodePath ).



% Lists (in alphabetical order) all modules that exist in the current
% code path, based on the BEAM files found.
%
-spec list_beams_in_path() -> [ module_name() ].
list_beams_in_path() ->

	% Directly inspired from:
	% http://alind.io/post/5664209650/all-erlang-modules-in-the-code-path

	Files = [ list_to_atom( filename:basename( File, ?beam_extension ) )
			  || Path <- code:get_path(),
				 File <- filelib:wildcard( "*.beam", Path ) ],

	lists:sort( Files ).



% Returns the filename of the BEAM file corresponding to specified module.
-spec get_beam_filename( module_name() ) -> file_name().
get_beam_filename( ModuleName ) when is_atom( ModuleName ) ->

	ModuleNameString = text_utils:atom_to_string( ModuleName ),

	ModuleNameString ++ ?beam_extension.




% Tells whether specified module has its BEAM file in the current code path.
%
% Returns either a list of its absolute, canonicalised, unordered paths (if
% being available at least once), or 'not_found' (hence: this is not a boolean
% return!).
%
-spec is_beam_in_path( module_name() ) -> 'not_found' | [ directory_path() ].
is_beam_in_path( ModuleName ) when is_atom( ModuleName ) ->

	ModuleFilename = text_utils:atom_to_string( ModuleName ) ++ ?beam_extension,

	%trace_utils:info_fmt( "Paths for module filename '~ts':~n  ~p",
	%					   [ ModuleFilename, code:get_path() ] ),

	% We have to ensure that all paths are absolute and normalised, so that we
	% can eliminate any duplicates among them (otherwise some module files could
	% be erroneously reported as being themselves duplicated):

	CurDirPath = file_utils:get_current_directory(),

	% Includes normalisation:
	VetPaths = list_utils:uniquify( [ file_utils:ensure_path_is_absolute(
		file_utils:join( P, ModuleFilename ), _BasePath=CurDirPath )
									  || P <- code:get_path() ] ),

	ExistingFilePaths = [ P || P <- VetPaths,
							   file_utils:is_existing_file_or_link( P ) ],

	case ExistingFilePaths of

		[] ->
			not_found;

		Paths ->
			Paths

	end;

is_beam_in_path( Other ) ->
	throw( { non_atom_module_name, Other } ).



% Returns the root directory of Erlang/OTP, where it is installed.
%
% Ex: "/home/joe/Software/Erlang/Erlang-23.1/lib/erlang" or
% "/usr/local/otp/lib".
%
-spec get_erlang_root_path() -> directory_path().
get_erlang_root_path() ->
   code:root_dir().



% Returns (without crashing the program) the current stack trace.
%
% A replacement for deprecated erlang:get_stacktrace/0.
%
-spec get_stacktrace() -> stack_trace().
get_stacktrace() ->
	get_stacktrace( _SkipLastElemCount=0 ).



% Returns (without crashing the program) the current stack trace, whose
% SkipLastElemCount first elements have been dropped (to output a cleaner, more
% relevant stacktrace).
%
-spec get_stacktrace( count() ) -> stack_trace().
get_stacktrace( SkipLastElemCount ) ->
	try

		throw( generate_stacktrace )

	catch throw:generate_stacktrace:StackTrace ->

		% To remove the initial code_utils:get_stacktrace/0, by design at the
		% top of the stack:
		%
		list_utils:remove_first_elements( StackTrace, SkipLastElemCount+1 )

	end.



% Returns a "smart" textual representation of the current stacktrace.
-spec interpret_stacktrace() -> ustring().
interpret_stacktrace() ->

	% We do not want to include interpret_stacktrace/0 in the stack:
	StackTrace = get_stacktrace( _SkipLastElemCount=1 ),

	interpret_stacktrace( StackTrace ).


% Returns a "smart" textual representation of specified stacktrace.
-spec interpret_stacktrace( stack_trace() ) -> ustring().
interpret_stacktrace( StackTrace ) ->
	interpret_stacktrace( StackTrace, _FullPathsWanted=false ).


% Returns a "smart" textual representation of specified stacktrace, listing
% either the full path of the corresponding source files, or just their
% filename.
%
-spec interpret_stacktrace( stack_trace(), boolean() ) -> ustring().
interpret_stacktrace( StackTrace, FullPathsWanted ) ->

	%io:format( "Interpreting stack trace:~n~p~n", [ StackTrace ] ),

	StringItems = [ interpret_stack_item( I, FullPathsWanted )
					|| I <- StackTrace ],

	text_utils:strings_to_enumerated_string( StringItems ).



% Returns a "smart" textual representation of the current stacktrace, once
% specified extra depth has been skipped (not counting this call).
%
% Removing the specified number of last calls allows to skip unwanted
% error-reporting functions and to return only a relevant stacktrace.
%
-spec interpret_shortened_stacktrace( basic_utils:count() ) -> ustring().
interpret_shortened_stacktrace( SkipLastElemCount ) ->
	interpret_stacktrace( get_stacktrace( SkipLastElemCount ) ).



% Helper:
interpret_stack_item( { Module, Function, Arity,
						[ { file, FilePath }, { line, Line } ] },
					  _FullPathsWanted=true ) when is_integer( Arity ) ->
	text_utils:format( "~ts:~ts/~B   [defined in ~ts (line ~B)]",
		[ Module, Function, Arity, file_utils:normalise_path( FilePath ),
		  Line ] );

interpret_stack_item( { Module, Function, Arity,
						[ { file, FilePath }, { line, Line } ] },
					  _FullPathsWanted=false ) when is_integer( Arity ) ->
	text_utils:format( "~ts:~ts/~B   [defined in ~ts (line ~B)]",
		[ Module, Function, Arity, filename:basename( FilePath ), Line ] );

interpret_stack_item( { Module, Function, Args,
						[ { file, FilePath }, { line, Line } ] },
					  _FullPathsWanted=false ) when is_list( Args ) ->
	text_utils:format( "~ts:~ts/~B   [defined in ~ts (line ~B)]",
		[ Module, Function, length( Args ), filename:basename( FilePath ),
		  Line ] );

interpret_stack_item( { Module, Function, Arity, Location },
					  _FullPathsWanted ) when is_integer( Arity ) ->
	text_utils:format( "~ts:~ts/~B located in ~p",
					   [ Module, Function, Arity, Location ] );

interpret_stack_item( { Module, Function, Arguments, _Location=[] },
					  _FullPathsWanted ) when is_list( Arguments ) ->
	text_utils:format( "~ts:~ts/~B",
					   [ Module, Function, length( Arguments ) ] );

interpret_stack_item( { Module, Function, Arguments, Location },
					  _FullPathsWanted ) when is_list( Arguments ) ->
	text_utils:format( "~ts:~ts/~B located in ~p",
					   [ Module, Function, length( Arguments ), Location ] );

% Never fail:
interpret_stack_item( I, _FullPathsWanted ) ->
	text_utils:format( "~p", [ I ] ).



% Displays the current stacktrace (not stopping the execution).
-spec display_stacktrace() -> void().
display_stacktrace() ->

	% We do not want to include display_stacktrace/0 in the stack:
	StackTrace = get_stacktrace( _SkipLastElemCount=1 ),

	trace_utils:info_fmt( "Current stacktrace is (latest calls first): ~ts~n",
						  [ interpret_stacktrace( StackTrace ) ] ).



% Interprets an undef exception, typically after it has been raised.
-spec interpret_undef_exception( module_name(),
			basic_utils:function_name(), arity() ) -> ustring().
interpret_undef_exception( ModuleName, FunctionName, Arity ) ->

	case code_utils:is_beam_in_path( ModuleName ) of

		not_found ->
			text_utils:format( "no module ~ts found in code path, "
				"which explains why its ~ts/~B function is reported "
				"as being undefined; ~ts",
				[ ModuleName, FunctionName, Arity,
				  code_utils:get_code_path_as_string() ] );


		ModulePath ->

			case meta_utils:get_arities_for( ModuleName,
											 FunctionName ) of

				[] ->
					text_utils:format( "module ~ts found in code path "
						"(as '~ts'), yet it does not export a '~ts' function "
						"(for any arity)",
						[ ModuleName, ModulePath, FunctionName ] );

				Arities ->
					interpret_arities( ModuleName, FunctionName, Arity,
									   Arities )

			end

	end.


% (helper)
interpret_arities( ModuleName, FunctionName, Arity, Arities ) ->

	case lists:member( Arity, Arities ) of

		true ->
			% Should never happen?
			text_utils:format( "module ~ts found in code path, and it exports "
				"the ~ts/~B function indeed",
				[ ModuleName, FunctionName, Arity ] );

		false ->
			ArStr = case Arities of

				[ A ] ->
					text_utils:format( "another arity (~B)", [ A ] );

				_ ->
					Ars = [ text_utils:integer_to_string( I )
							|| I <- lists:sort( Arities ) ],

					ArsStr = text_utils:strings_to_listed_string( Ars ),

					text_utils:format( "other arities (i.e. ~ts)", [ ArsStr ] )

			end,

			text_utils:format( "module ~ts found in code path, yet it does "
				"export a ~ts/~B function; as it exports this function "
				"for ~ts, maybe the call to that function was made with "
				"a wrong number of parameters",
				[ ModuleName, FunctionName, Arity, ArStr ] )

	end.


% Returns a textual description of specified stack location.
-spec stack_location_to_string( stack_location() ) -> ustring().
stack_location_to_string( [ { file, Filename }, { line, Line } ] ) ->
	text_utils:format( "in file ~ts, at line ~B", [ Filename, Line ] );

% Catch-all:
stack_location_to_string( OtherLoc ) ->
	text_utils:format( "~p", [ OtherLoc ] ).
