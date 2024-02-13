% Copyright (C) 2016-2024 Olivier Boudeville
%
% Include file meant to simplify the writing of Myriad-using escripts.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Released as LGPL software.



% To silence unused warnings:
-export([ get_script_base_directory/0,
		  get_myriad_base_directory/0,
		  update_code_path_for_myriad/0,
		  update_code_path_for_myriad_from_module/0 ]).


% For the file_info record:
-include_lib("kernel/include/file.hrl").


% Verbatim section.


% Copied verbatim from Ceylan-Myriad/src/utils/script_utils.erl, for bootstrap:
% Functions are declared private as they are included within each escript.


% @doc Tells whether the currently running Erlang code is executed as an escript
% or as a regular Erlang program.
%
-spec is_running_as_escript() -> boolean().
is_running_as_escript() ->

	% We thought that escript:script_name/0 was only meant to succeed iff
	% executed from an escript; yet, if simply run from a module, it will still
	% succeed if at least an extra command-line line was specified.
	%
	% So escript:script_name() will fail if erl is launched with no option,
	% whereas it will succeed if launched with 'erl -extra foobar' for example.
	%
	% The best solution we see currently is to look whether the returned script
	% name bears the '.escript' extension (better than just including a path
	% separator or not): from 'erl -extra foobar', escript:script_name/0 will
	% return "foobar", whereas from hello.escript it will return
	% "./hello.escript"; so:

	try

		case escript:script_name() of

			Name ->
				%io:format( "Script name: '~ts'.~n", [ Name ] ),
				filename:extension( Name ) =:= ".escript"

		end

	% Typically {badmatch,[]} from escript.erl:
	catch error:_Error ->

		false

	end.



% @doc Returns the base directory of that script, that is where it is stored
% (regardless of the possibly relative path whence it was launched).
%
% Note: useful to locate resources (e.g. other modules) defined in link to that
% script and needed by it.
%
% @private
% @hidden
%
-spec get_script_base_directory() -> file_utils:directory_path().
get_script_base_directory() ->

	case is_running_as_escript() of

		true ->

			%io:format( "Found running as escript.~n" ),

			% filename:absname/1 could be used instead:
			FullPath = case escript:script_name() of

				ScriptPath=( "/" ++ _ ) ->
					% Is already absolute here:
					ScriptPath;

				RelativePath ->
					% Let's make it absolute then:
					{ ok, CurrentDir } = file:get_cwd(),
					filename:join( CurrentDir, RelativePath )

			end,

			%file_utils:get_base_path( FullPath );

			BaseDir = filename:dirname( FullPath ),

			%io:format( "Script base directory: '~ts'.~n", [ BaseDir ] ),

			BaseDir;


		false ->

			%io:format( "Found not running as escript.~n" ),

			% Supposing Myriad is already available then?
			CodePath = code_utils:get_code_path(),

			MyriadPath = get_myriad_path_from( CodePath ),

			% We cannot use file_utils:normalise_path/1 here: Myriad not usable
			% from that point yet!
			%
			file_utils:join( [ MyriadPath, "src", "scripts" ] )

	end.




% (helper)
get_myriad_path_from( CodePath ) ->

	% Two base directories are licit for Myriad, a reference one and a
	% shorthand:
	%
	case get_myriad_path_from( CodePath, "Ceylan-Myriad" ) of

		undefined ->

			case get_myriad_path_from( CodePath, "myriad" ) of

				undefined ->
					throw( unable_to_determine_myriad_root );

				Path ->
					%trace_utils:debug_fmt( "Found from myriad: '~ts'.",
					%                       [ Path ] ),
					Path

			end;

		Path ->
			%trace_utils:debug_fmt( "Found from Ceylan-Myriad: '~ts'.",
			%                       [ Path ] ),
			Path

	end.



% (sub-helper)
get_myriad_path_from( _Paths=[], _BaseDirName ) ->
	undefined;

get_myriad_path_from( [ Path | T ], BaseDirName ) ->
	case string:split( Path, BaseDirName ) of

		[ Prefix, _Suffix ] ->
			% Just the full path to the root wanted:
			file_utils:join( Prefix, BaseDirName );

		% Layer name not found:
		_ ->
			get_myriad_path_from( T, BaseDirName )

	end.



% @doc Updates the VM code path so that all modules of the 'Myriad' layer can be
% readily used from an escript.
%
% Returns as well the Myriad base directory, for any further use
% (e.g. determining other sibling base directories).
%
% Note: this function and its helpers might be copied verbatim to the target
% escript so that it can really be used from anywhere (not only from the
% directory in which it is stored).
%
% (original version located in script_utils.erl, copied verbatim here)
%
% @private
%
-spec update_code_path_for_myriad() -> file_utils:directory_path().
update_code_path_for_myriad() ->
	update_code_path_for_myriad( get_myriad_base_directory() ).



% @doc Updates the VM code path so that all modules of the 'Myriad' layer can be
% readily used from a module run as an escript.
%
% Returns as well the Myriad base directory, for any further use
% (e.g. determining other sibling base directories).
%
% @private
%
% (original version located in script_utils.erl, copied verbatim here)
%
-spec update_code_path_for_myriad_from_module() -> file_utils:directory_path().
update_code_path_for_myriad_from_module() ->
	{ ok, CurrentDir } = file:get_cwd(),
	MyriadBaseDirectory = filename:join( [ CurrentDir, "..", ".." ] ),
	update_code_path_for_myriad( MyriadBaseDirectory ).



% @doc Updates the VM code path so that all modules of the 'Myriad' layer can be
% readily used from an escript.
%
% The specified root directory is supposed correct (no further checking made).
%
% Returns as well the Myriad base directory, for any further use
% (e.g. determining other sibling base directories).
%
% Note: this function and its helpers might be copied verbatim to the target
% escript so that it can really be used from anywhere (not only from the
% directory it is stored).
%
% @private

% (original version located in script_utils.erl, copied verbatim here)
%
-spec update_code_path_for_myriad( file_utils:directory_path() ) ->
										file_utils:directory_path().
update_code_path_for_myriad( MyriadRootDir ) ->

	% Should not use trace_utils for that, as Myriad not found yet here:
	%io:format( "Root of 'Myriad': '~ts'.~n", [ MyriadRootDir ] ),

	MyriadSrcDir = filename:join( MyriadRootDir, "src" ),

	% An up-to-date version can be obtained thanks to the
	% 'list-beam-relative-paths' make target:
	%
	MyriadBeamSubDirs = [ "data-management", "maths", "meta", "scripts",
		"user-interface/graphical", "user-interface/textual",
		"user-interface/audio", "user-interface", "utils" ],


	MyriadBeamDirs =
		[ filename:join( MyriadSrcDir, D ) || D <- MyriadBeamSubDirs ],

	%io:format( "'Myriad' beam dirs: ~p.~n", [ MyriadBeamDirs ] ),

	ok = code:add_pathsa( MyriadBeamDirs ),

	% One thing is that the relevant paths are declared, another one is that
	% they have been built:
	%
	try

		test = basic_utils:identity( test )

	catch error:undef ->

		io:format( "Error, Ceylan-Myriad is not fully built.~n", [] ),

		exit( myriad_not_built )

	end,

	MyriadRootDir.



% @doc Returns the root directory of the Myriad layer.
%
% (note that a double path conversion between root and script directories can
% hardly be avoided)
%
% @private
%
% (original version located in script_utils.erl, copied verbatim here)
%
-spec get_myriad_base_directory() -> file_utils:directory_path().
get_myriad_base_directory() ->

	% We cannot use file_utils:normalise_path/1 here, as Myriad is not usable
	% from that point yet.
	%
	% Two main possibilities here: the current escript is located in src/scripts
	% or in src/apps/SOME_APP; trying them in turn, using src/meta as an
	% indicator, a candidate designating a possible Myriad root.
	%
	% So, maybe script is in src/scripts:

	ScriptBaseDir = get_script_base_directory(),

	FirstBaseCandidate =
		filename:join( [ ScriptBaseDir, "..", "..", "..", "myriad" ] ),

	FirstMetaPath = filename:join( [ FirstBaseCandidate, "src", "meta" ] ),

	case file:read_file_info( FirstMetaPath ) of

		{ ok, #file_info{ type=directory } } ->
			FirstBaseCandidate;

		{ error, _FirstReason } ->

			% Defined specifically, for any error report:
			SecondBaseCandidate = filename:join(
				[ ScriptBaseDir, "..", "..", "..", "..", "myriad" ] ),

			% Maybe in src/apps/SOME_APP then:
			SecondMetaPath =
				filename:join( [ SecondBaseCandidate, "src", "meta" ] ),

			case file:read_file_info( SecondMetaPath ) of

				{ ok, #file_info{ type=directory } } ->
					SecondBaseCandidate;

				{ error, _SecondReason } ->
					throw( { myriad_base_directory_not_found,
							 FirstBaseCandidate, SecondBaseCandidate } )

			end

	end.
