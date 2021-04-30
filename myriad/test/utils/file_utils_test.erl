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


% Unit tests for the file_utils toolbox.
%
% See the file_utils.erl tested module.
%
-module(file_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	CurrentDir = file_utils:get_current_directory(),

	{ _RegularFiles, _Symlinks, _Directories, _OtherFiles, _Devices } =
		Elements = file_utils:list_dir_elements( CurrentDir ),

	BeamExtension = ".beam",

	test_facilities:display(
	  "File elements in the current directory (~ts):~n~p",
	  [ CurrentDir, Elements ] ),

	% Too many outputs:
	%test_facilities:display( "Regular BEAM files in the current directory: "
	% ~n~p", [ file_utils:filter_by_extension( RegularFiles, BeamExtension) ] ),

	test_facilities:display( "All files found recursively "
		"from the current directory:~n~p",
		[ file_utils:find_files_from( CurrentDir ) ] ),


	test_facilities:display( "All BEAM files found recursively "
		"from the current directory:~n~p",
		[ file_utils:find_files_with_extension_from( CurrentDir,
													 BeamExtension ) ] ),

	ExcludedDirs = [ ".svn", "non-existing-dir" ],

	test_facilities:display( "All files found recursively "
		"from the current directory, with directories ~p excluded:~n~p",
		[ ExcludedDirs, file_utils:find_files_with_excluded_dirs( CurrentDir,
															ExcludedDirs ) ] ),


	ExcludedSuffixes = [ ".erl", ".beam", "non-existing-suffix" ],

	test_facilities:display( "All files found recursively "
		"from the current directory, with suffixes ~p excluded:~n~p",
		[ ExcludedSuffixes, file_utils:find_files_with_excluded_suffixes(
						CurrentDir, ExcludedSuffixes ) ] ),


	test_facilities:display( "All files found recursively "
		"from the current directory, with directories ~p and suffixes ~p "
		"excluded:~n~p",
		[ ExcludedDirs, ExcludedSuffixes,
		  file_utils:find_files_with_excluded_dirs_and_suffixes(
						CurrentDir, ExcludedDirs, ExcludedSuffixes ) ] ),


	true  = file_utils:is_absolute_path( "/etc/host.conf" ),
	false = file_utils:is_absolute_path( "my-dir/my-file" ),
	false = file_utils:is_absolute_path( "" ),

	RelativePath = "my-local-dir/a-file",

	test_facilities:display( "Ensuring '~ts' is absolute: ~ts", [ RelativePath,
					file_utils:ensure_path_is_absolute( RelativePath ) ] ),

	BasePath ="/etc",

	test_facilities:display(
	  "Ensuring '~ts' is absolute with base path '~ts': '~ts'",
	  [ RelativePath, BasePath,
		file_utils:ensure_path_is_absolute( RelativePath, BasePath ) ] ),


	"/home/lisa/tube" =
		file_utils:normalise_path( "/home/garfield/../lisa/./src/.././tube" ),

	"../homer/bart.beam" =
		file_utils:normalise_path( "../src/../homer/./bart.beam" ),

	% Define suitable paths for testing:

	LowerPath = "../../a/b",
	LowerPath = file_utils:make_relative( LowerPath ),

	LowerPathAbs = file_utils:ensure_path_is_absolute( LowerPath ),
	LowerPath = file_utils:make_relative( LowerPathAbs ),

	HigherPath = "c/d/e",
	HigherPath = file_utils:make_relative( HigherPath ),

	HigherPathAbs = file_utils:ensure_path_is_absolute( HigherPath ),
	HigherPath = file_utils:make_relative( HigherPathAbs ),

	file_utils:make_relative( "/etc/host.conf" ),

	OtherRelativePath = "my-dir/my-file",
	OtherRelativePath = file_utils:make_relative( OtherRelativePath ),

	LeafName = "hello.txt",

	LeafPath = "/tmp/test/" ++ LeafName,


	false = file_utils:is_leaf_among( LeafName, [ "aa", "bb" ] ),

	TestPaths = [ "test", "/test/foobar", LeafPath, "another_element" ],

	LeafPath = file_utils:is_leaf_among( LeafName, TestPaths ),



	FirstFilename = "media/frame/1-23-2-98.oaf",

	test_facilities:display(
	  "Path '~ts', once transformed into a variable name, results in: ~ts",
	  [ FirstFilename, file_utils:path_to_variable_name( FirstFilename ) ] ),



	SecondFilename = "./mnt/zadok/44_12.oaf",

	test_facilities:display( "Path '~ts', once transformed into a variable "
		"name, results in: ~ts", [ SecondFilename,
						file_utils:path_to_variable_name( SecondFilename ) ] ),


	FirstString = "My name is Bond",
	test_facilities:display( "String '~ts', "
		"once transformed into a file name, results in: '~ts'",
		[ FirstString, file_utils:convert_to_filename( FirstString ) ] ),


	SecondString = "James,  James <Bond> ('Special' \"Agent\"), Sir",
	test_facilities:display( "String '~ts', once transformed into a file name,"
		"results in: '~ts'",
		[ SecondString, file_utils:convert_to_filename( SecondString ) ] ),


	NoExtensionFilename = "my_foobar",

	no_extension = file_utils:get_extensions( NoExtensionFilename ),
	no_extension = file_utils:get_extension( NoExtensionFilename ),


	ExtensionFilename = "foobar.baz.json",

	[ "baz", "json" ] = file_utils:get_extensions( ExtensionFilename ),
	"json" = file_utils:get_extension( ExtensionFilename ),


	SourceFilename  = "/home/jack/rosie.ttf",
	SourceExtension = ".ttf",
	TargetExtension = ".wav",

	NewFilename = file_utils:replace_extension( SourceFilename, SourceExtension,
												TargetExtension ),

	test_facilities:display( "Replacing extension '~ts' by '~ts' in '~ts' "
		"results in: '~ts'.",
		[ SourceExtension, TargetExtension, SourceFilename, NewFilename ] ),


	% Commented out, as not wanting to have too many side-effects:

	%file_utils:create_directory( "tmp-tst" ),
	%file_utils:create_directory( "tmp-tst/first/second", create_parents ),

	Bin = file_utils:read_whole( "GNUmakefile" ),
	test_facilities:display( "Read file: ~p.", [ Bin ] ),
	%file_utils:write_whole( "test.dat", Bin ),

	% On some systems, ls, cat, etc. are found in /usr/bin/ before being found
	% in /bin:
	%
	%LsPath = "/bin/ls" = executable_utils:find_executable( "ls" ),
	LsPath = executable_utils:find_executable( "ls" ),
	true = file_utils:is_owner_executable( LsPath ),

	NonExistingPath = "ls-non-existing-exec",
	false = executable_utils:lookup_executable( NonExistingPath ),
	false = file_utils:is_owner_executable( NonExistingPath ),


	test_facilities:display( "Testing compression support." ),

	TargetFile = "GNUmakefile",

	OriginalContent = file_utils:read_whole( TargetFile ),

	ZippedFile = file_utils:compress( TargetFile, zip ),
	UnzippedFile = file_utils:decompress( ZippedFile, zip ),
	UnzippedContent = file_utils:read_whole( UnzippedFile ),

	case UnzippedContent =:= OriginalContent of

		true ->
			test_facilities:display( "Original file and unzipped one "
									 "(~ts) match.", [ UnzippedFile ] );

		false ->
			throw( unzipped_content_differs )

	end,


	Bzip2File = file_utils:compress( TargetFile, bzip2 ),
	Unbzip2File = file_utils:decompress( Bzip2File, bzip2 ),
	UnbzippedContent = file_utils:read_whole( Unbzip2File ),

	case UnbzippedContent =:= OriginalContent of

		true ->
			test_facilities:display( "Original file and unbzip2-ed one "
									 "(~ts) match.", [ Unbzip2File ] );

		false ->
			throw( unbzip2ed_content_differs )

	end,

	XzFile = file_utils:compress( TargetFile, xz ),
	UnxzFile = file_utils:decompress( XzFile, xz ),
	UnxzContent = file_utils:read_whole( UnxzFile ),

	case UnxzContent =:= OriginalContent of

		true ->
			test_facilities:display(
			  "Original file and unxz-ed one (~ts) match.", [ UnxzFile ] );

		false ->
			throw( unxz_content_differs )

	end,

	InfoPath = UnxzFile,

	test_facilities:display( "Information about '~ts': owner_id=~B, "
		"group_id=~B, permissions=~w.",
		[ InfoPath, file_utils:get_owner_of( InfoPath ),
		  file_utils:get_group_of( InfoPath ),
		  file_utils:get_permissions_of( InfoPath ) ] ),

	TargetPath = "/foo",

	% Shall fail with eacces (of not root of course):
	Caught = try

		F = file_utils:open( TargetPath, _Opts=[ write ] ),

		UserName = system_utils:get_user_name_safe(),

		test_facilities:display( "Unexpectedly able to open '~ts' (as '~ts').",
			[ TargetPath, UserName ] ),

		file_utils:write_ustring( F, "I should not be able to write there." ),

		file_utils:close( F ),

		case UserName of

			% This happens with Github CI:
			"root" ->
				true;

			_ ->
				false

		end

	catch _:E ->

		test_facilities:display( "Error intercepted as expected, when writing "
								 "'~ts':~n ~p.", [ TargetPath, E ] ),
		true

	end,

	% Check:
	Caught = true,

	file_utils:remove_files( [ ZippedFile, Bzip2File, XzFile ] ),

	test_facilities:stop().
