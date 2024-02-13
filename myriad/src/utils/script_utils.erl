% Copyright (C) 2012-2024 Olivier Boudeville
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
% Creation date: Wednesday, October 24, 2012.


% @doc Gathering helper functions for the <b>development and use of all kinds of
% scripts</b> (Erlang escripts and shell scripts alike).
%
% Intended use for escripts: add, in the script directory, a symbolic link to
% this module so that the script can readily call it and thus bootstrap the use
% of all others.
%
-module(script_utils).


% Implementation notes:
%
% The code path is not supposed to be updated with the one for 'Myriad', so
% extra care must be taken not to call Myriad helper modules for implementations
% here meant to be run before the update of the code path.
%
-export([ is_running_as_escript/0, get_script_base_directory/0,
		  get_myriad_base_directory/0,
		  update_code_path_for_myriad/0,
		  update_code_path_for_myriad_from_module/0,
		  get_arguments/1 ]).


% For the file_info record:
-include_lib("kernel/include/file.hrl").


-define( reference_myriad_dir, "Ceylan-Myriad" ).
-define( shorthand_myriad_dir, "myriad" ).



% Shorthands to be avoided here, as at least some functions are meant to be
% copied verbatim in headers, such as myriad_script_include.hrl.

%-type ustring() :: text_utils:ustring().

%-type directory_path() :: file_utils:directory_path().


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
	% whereas it will succeed if launched with 'erl -extra foobar' for example
	% (and for some reason will return then "foobar").
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
		%io:format( "Script name error: '~p'.~n", [ Error ] ),
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

				% Possibly actually a dummy value (e.g. "--batch"):
				RelativePath ->
					% Let's make it absolute then:
					{ ok, CurrentDir } = file:get_cwd(),
					filename:join( CurrentDir, RelativePath )

			end,

			%file_utils:get_base_path( FullPath );

			BaseDir = filename:dirname( FullPath ),

			%io:format( "Script base directory (as escript): '~ts'.~n",
			%           [ BaseDir ] ),

			BaseDir;


		false ->
			%io:format( "Found not running as escript.~n" ),

			% Supposing Myriad is already available then?
			CodePath = code_utils:get_code_path(),

			MyriadPath = get_myriad_path_from( CodePath ),

			% We cannot use file_utils:normalise_path/1 here: Myriad not usable
			% from that point yet!
			%
			BaseDir = file_utils:join( [ MyriadPath, "src", "scripts" ] ),

			%io:format( "Script base directory (not as escript): '~ts'.~n",
			%           [ BaseDir ] ),

			BaseDir

	end.



% (helper)
get_myriad_path_from( CodePath ) ->

	% Two base directories are licit for Myriad, a reference one and a
	% shorthand:
	%
	case get_myriad_path_from( CodePath, ?reference_myriad_dir ) of

		undefined ->

			case get_myriad_path_from( CodePath, ?shorthand_myriad_dir ) of

				undefined ->
					throw( unable_to_determine_myriad_root );

				Path ->
					%trace_utils:debug_fmt( "Found from ~ts '~ts'.",
					%                       [ ?shorthand_myriad_dir, Path ] ),
					Path

			end;

		Path ->
			%trace_utils:debug_fmt( "Found from ~ts: '~ts'.",
			%                       [ ?reference_myriad_dir, Path ] ),
			Path

	end.



% (sub-helper)
get_myriad_path_from( _Paths=[], _BaseDirName ) ->
	undefined;

get_myriad_path_from( [ Path | T ], BaseDirName ) ->
	% Of course continuous integration had to use
	% '/__w/Ceylan-Myriad/Ceylan-Myriad' as base directory...
	%
	case string:split( Path, BaseDirName, _Where=trailing ) of

		[ Prefix, _Suffix ] ->
			% Just the full path to the root wanted:
			%io:format( "Split path '~ts' with base dir '~ts', "
			%   "got prefix '~ts'.", [ Path, BaseDirName, Prefix ] ),
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
	% they have been built; testing it:
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
	% from that point yet. Yet at least in Github CI,
	% "/__w/Ceylan-Myriad/Ceylan-Myriad" is found existing whereas
	% "/__w/Ceylan-Myriad/src/scripts/../../Ceylan-Myriad" not (!), so we had to
	% include verbatim file_utils:normalise_path/1 and its dependencies.
	%
	% Two main possibilities here: the current escript is located in src/scripts
	% or in src/apps/SOME_APP; trying them in turn, using src/meta as an
	% indicator, a candidate designating a possible Myriad root.
	%
	% So, maybe script is in src/scripts:

	ScriptBaseDir = get_script_base_directory(),

	FirstPrefixPath = [ ScriptBaseDir, "..", ".." ],

	FirstBaseCandidate = normalise_path(
		filename:join( FirstPrefixPath ++ [ ?reference_myriad_dir ] ) ),

	case is_legit_path( FirstBaseCandidate ) of

		true ->
			FirstBaseCandidate;

		false ->
			FirstAltBaseCandidate = normalise_path(
				filename:join( FirstPrefixPath ++ [ ?shorthand_myriad_dir ] ) ),

			case is_legit_path( FirstAltBaseCandidate ) of

				true ->
					FirstAltBaseCandidate;

				false ->
					SecondPrefixPath = FirstPrefixPath ++ [ ".." ],

					SecondBaseCandidate = normalise_path( filename:join(
						SecondPrefixPath ++ [ ?reference_myriad_dir ] ) ),

					case is_legit_path( SecondBaseCandidate ) of

						true ->
							SecondBaseCandidate;

						false ->
							SecondAltBaseCandidate = normalise_path(
								filename:join( SecondPrefixPath
											   ++ [ ?shorthand_myriad_dir ] ) ),

							case is_legit_path( SecondAltBaseCandidate ) of

								true ->
									SecondAltBaseCandidate;

								false ->
									throw( { myriad_base_directory_not_found,
										FirstBaseCandidate,
										FirstAltBaseCandidate,
										SecondBaseCandidate,
										SecondAltBaseCandidate } )

							end

					end

			end

	end.


%-spec test_directory( file_utils:path() ) -> basic_utils:void().
%test_directory( D ) ->
%   io:format( "Testing ~ts: ~p~n", [ D, file:read_file_info( D ) ] ).




% Included from file_utils (shortened as no bin_string() support, using
% filename:join/2 instead of the one of file_utils, and with type prefixes):


% @doc Normalises specified path (canonicalises it), by translating it so that
% no intermediate, superfluous '.' or '..' is present afterwards.
%
% For example, "/home/garfield/../lisa/./src/.././tube" shall be normalised in
% "/home/lisa/tube".
%
% Returns a path of the same string type as the specified parameter.
%
-spec normalise_path( file_utils:path() ) -> file_utils:path();
					( file_utils:bin_path() ) -> file_utils:bin_path().
normalise_path( _Path="." ) ->
	".";
	%get_current_directory();

normalise_path( Path ) when is_list( Path ) ->

	ElemList = filename:split( Path ),

	%trace_utils:debug_fmt( "ElemList: ~p", [ ElemList ] ),

	ResPath = filename:join( filter_elems_plain( ElemList, _Acc=[] ) ),

	%trace_utils:debug_fmt( "Normalising path '~ts' as '~ts'.",
	%                       [ Path, ResPath ] ),

	ResPath.

% (helper)
filter_elems_plain( _ElemList=[], Acc ) ->
	lists:reverse( Acc );

filter_elems_plain( _ElemList=[ "." | T ], Acc ) ->
	filter_elems_plain( T, Acc );

% We can remove one level iff there is at least one accumulated *and* this one
% is not already ".." (otherwise the ".." will cancel out):
%
filter_elems_plain( _ElemList=[ ".." | T ], _Acc=[ PrevElem | AccT ] )
						when PrevElem =/= ".." ->
	filter_elems_plain( T, AccT );


% No level left, so this ".." should not be filtered out:
%
% (however this clause is a special case of the next, hence can be commented
% out)
%
%filter_elems_plain( _ElemList=[ PathElement=".." | T ], Acc ) ->
%   filter_elems_plain( T, [ PathElement | Acc ] );

filter_elems_plain( _ElemList=[ E | T ], Acc ) ->
	filter_elems_plain( T, [ E | Acc ] ).



% @doc Tests whether the specified path is a legit candidate one.
%
% (helper)
%
-spec is_legit_path( file_utils:path() ) -> boolean().
is_legit_path( BaseCandidatePath ) ->

	% An indicator for testing this candidate base directory:
	MetaPath = filename:join( [ BaseCandidatePath | [ "src", "meta" ] ] ),

	case file:read_file_info( MetaPath ) of

		{ ok, #file_info{ type=directory } } ->
			true;

		{ ok, #file_info{ type=symlink } } ->
			true;

		% Error or other type:
		_Other ->
			%io:format( "Candidate path '~p' not legit (~p).~n",
			%           [ MetaPath, Other ] ),
			false

	end.



% @doc Returns a table allowing to manage the specified command-line arguments
% more easily.
%
% These arguments are simply to be transmitted as a list of the corresponding
% strings, typically as directly obtained through the main/1 function of an
% escript) once transformed into our "canonical", more convenient form, which is
% the same as the one used by shell_utils:get_argument_table/0 and is itself
% similar to the one used by Erlang for its user/system flags (i.e. for all its
% non-plain options).
%
% In this form, which by default is not available for escripts, options start
% with a dash, may have any number of arguments, and may be specified more than
% once in the command-line.
%
% Note: switches to the Unicode encoding (e.g. use "~tp" then).
%
% Allows to write code that can be seamlessly triggered by a erl interpreter or
% by an escript, by putting them in the latter case in our "canonical" form.
%
-spec get_arguments( [ text_utils:ustring() ] ) -> shell_utils:argument_table().
get_arguments( Args ) ->
	shell_utils:get_argument_table_from_strings( Args ).
