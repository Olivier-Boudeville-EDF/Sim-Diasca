% Copyright (C) 2008-2021 Olivier Boudeville
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
% Creation date: Saturday, July 12, 2008.


% Gathering of various convenient facilities regarding executing of third-party
% tools.
%
% See executable_utils_test.erl for the corresponding test, and shell_utils.erl
% for the management of the shells and command lines.
%
-module(executable_utils).



% Section for the searching and checking of executables:
-export([ lookup_executable/1, lookup_executable/2, find_executable/1 ]).




% Section for most usual commands:
-export([ can_generate_png_from_graph/0, generate_png_from_graph_file/2,
		  generate_png_from_graph_file/3, display_png_file/1,
		  browse_images_in/1, display_pdf_file/1, display_text_file/1,
		  display_wide_text_file/2, get_ssh_mute_option/0,
		  compute_md5_sum/1, compute_sha1_sum/1, compute_sha_sum/2 ]).


% Section about default tools:
-export([

		 get_default_image_viewer_name/0,
		 get_default_image_viewer_path/0,

		 get_default_image_browser_name/0,
		 get_default_image_browser_path/0,

		 get_default_web_browser_name/0,
		 get_default_web_browser_path/0,

		 get_default_pdf_viewer_name/0,
		 get_default_pdf_viewer_path/0,

		 get_default_text_viewer_name/0,
		 get_default_text_viewer_path/0,

		 get_default_wide_text_viewer_name/1,
		 get_default_wide_text_viewer_path/1,

		 get_default_trace_viewer_name/0,
		 get_default_trace_viewer_path/0,

		 get_default_erlang_root/0,
		 get_default_erlang_interpreter_name/0,
		 get_default_erlang_interpreter_path/0,

		 get_default_ssh_client_name/0,
		 get_default_ssh_client_path/0,

		 get_default_scp_executable_name/0,
		 get_default_scp_executable_path/0,

		 get_default_openssl_executable_name/0,
		 get_default_openssl_executable_path/0,

		 get_gnuplot_path/0,
		 get_current_gnuplot_version/0,

		 get_default_zip_compress_tool/0,
		 get_default_zip_decompress_tool/0,

		 get_default_bzip2_compress_tool/0,
		 get_default_bzip2_decompress_tool/0,

		 get_default_xz_compress_tool/0,
		 get_default_xz_decompress_tool/0,

		 get_default_md5_tool/0,
		 get_default_sha_tool/0,

		 get_default_java_runtime/0,
		 get_default_jinterface_path/0 ]).


% A name, not a path:
-type executable_name() :: ustring().

% MD5 sum, a 128-bit hash value:
-type md5_sum() :: non_neg_integer().


% SHA1 sum, a 160-bit hash value:
-type sha1_sum() :: non_neg_integer().


% SHA sum, a hash value of unspecified size:
-type sha_sum() :: non_neg_integer().


-export_type([ executable_name/0, md5_sum/0, sha1_sum/0, sha_sum/0 ]).



% Miscellaneous section:
-export([ is_batch/0 ]).


-define( dot_tool, "dot" ).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type width() :: text_utils:width().

-type file_name() :: file_utils:file_name().
-type file_path() :: file_utils:file_path().
-type any_file_path() :: file_utils:any_file_path().
-type executable_path() :: file_utils:executable_path().
-type directory_path() :: file_utils:directory_path().

-type command_output() :: system_utils:command_output().



% Looks-up specified executable program, whose name is specified as a string
% (ex: "gcc") in the current user PATH.
%
% Returns an absolute filename of the executable program (ex: "/usr/bin/gcc"),
% or the 'false' atom if it was not found.
%
-spec lookup_executable( executable_name() ) -> executable_path() | 'false'.
lookup_executable( ExecutableName ) ->
	% Similar to a call to 'type' / 'which':
	os:find_executable( ExecutableName ).



% Looks-up specified executable program, whose name is specified as a string
% (ex: "gcc") in the current user PATH, augmented of the specified list of
% directories (whose existence is not checked), placed at first position.
%
% Returns an absolute filename of the executable program (ex: "/usr/bin/gcc"),
% or the 'false' atom if it was not found.
%
% Ex: lookup_executable("my-foo-program", [".", "/tmp"])
%
-spec lookup_executable( executable_name(), [ directory_path() ] ) ->
								executable_path() | 'false'.
lookup_executable( ExecutableName, ExtraDirs ) ->

	% Let's reconstruct a proper PATH-like string:
	ExtraStr = text_utils:join( $:, ExtraDirs ),

	FullStr = case system_utils:get_environment_variable( "PATH" ) of

		false ->
			ExtraStr;

		PathValue ->
			text_utils:join( $:, [ ExtraStr, PathValue ] )

	end,

	% Similar to a call to 'type' / 'which':
	os:find_executable( ExecutableName, FullStr ).



% Finds specified executable program, whose name is specified as a string (ex:
% "gcc") in the current user PATH.
%
% Returns an absolute filename of the executable program (ex: "/usr/bin/gcc") or
% throws an exception {executable_not_found,ExecutableName} if it was not found.
%
-spec find_executable( executable_name() ) -> executable_path().
find_executable( ExecutableName ) ->

	case lookup_executable( ExecutableName) of

		false ->
			throw( { executable_not_found, ExecutableName } );

		Path ->
			Path

	end.




% Section for most usual commands.


% Returns whether a PNG file can be generated from a graph file: either confirms
% it, or returns an hint why not.
%
-spec can_generate_png_from_graph() -> 'true' | ustring().
can_generate_png_from_graph() ->

	Tool = ?dot_tool,

	case lookup_executable( Tool ) of

		false ->
			text_utils:format( "no '~s' tool found (to be installed on many "
				"distributions with the 'graphviz' package)", [ Tool ] );

		_Path ->
			true

	end.



% By default does not crash if dot outputs some warnings but does not yield an
% error exit status.
%
-spec generate_png_from_graph_file( file_path(), file_path() ) ->
											command_output().
generate_png_from_graph_file( PNGFilename, GraphFilename ) ->
	generate_png_from_graph_file( PNGFilename, GraphFilename,
								  _HaltOnDotOutput=false ).



% Generates a PNG image file from specified graph file, that must respect the
% dot (graphviz) syntax:
%
%  - PNGFilePath the filename of the PNG to generate
%
%  - GraphFilePath the filename corresponding to the source graph
%
%  - HaltOnDotOutput tells whether the process should throw an exception should
%  dot output an error or a warning
%
% Returns the (possibly empty) string output by dot, or throws an exception.
%
-spec generate_png_from_graph_file( file_path(), file_path(), boolean() ) ->
			command_output().
generate_png_from_graph_file( PNGFilePath, GraphFilePath,
							  _HaltOnDotOutput=true ) ->

	case execute_dot( PNGFilePath, GraphFilePath ) of

		[] ->
			% Most correct case:
			[];

		ErrorMessage ->
			throw( { graph_generation_failed, PNGFilePath, GraphFilePath,
					 ErrorMessage } )

	end;

% Any output remains available to the caller.
generate_png_from_graph_file( PNGFilePath, GraphFilePath,
							  _HaltOnDotOutput=false ) ->
	execute_dot( PNGFilePath, GraphFilePath ).



% Displays (without blocking) to the user the specified PNG, using an external
% viewer.
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec display_png_file( file_path() ) -> void().
display_png_file( PNGFilename ) ->
	% Viewer output is ignored:
	system_utils:run_background_command(
		get_default_image_viewer_path() ++ " " ++ PNGFilename  ).



% Allows to browse (without blocking) the images available in specified
% directory (specified as a plain string)
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec browse_images_in( file_path() ) -> void().
browse_images_in( DirectoryName ) ->
	system_utils:run_background_command(
		get_default_image_browser_path() ++ " " ++ DirectoryName ).



% Displays (without blocking) to the user the specified PNG, using an external
% viewer.
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec display_pdf_file( file_path() ) -> void().
display_pdf_file( PDFFilename ) ->
	system_utils:run_background_command(
		get_default_pdf_viewer_path() ++ " " ++ PDFFilename ).



% Displays, with blocking, a text file.
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec display_text_file( file_path() ) -> command_output().
display_text_file( TextFilename ) ->

	case system_utils:run_command(
			get_default_text_viewer_path() ++ " " ++ TextFilename ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { display_failed_for_text_file, ExitCode, ErrorOutput } )

	end.



% Displays, with blocking, a wide text file.
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec display_wide_text_file( file_path(), width() ) -> command_output().
display_wide_text_file( TextFilename, CharacterWidth ) ->

	case system_utils:run_command(
		   get_default_wide_text_viewer_path( CharacterWidth )
		   ++ " " ++ TextFilename ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { wide_display_failed_for_text_file, ExitCode,
					 ErrorOutput } )

	end.



% Returns a string to be inserted into a command-line call to ssh/scp so that it
% can run as much as possible non-interactively.
%
% Tries notably to avoid following message: "The authenticity of host 'Server
% (XXXXX)' can't be established.  RSA key fingerprint is YYYYY. Are you sure you
% want to continue connecting (yes/no)?".
%
% Note: only to be used in a trusted environment.
%
% Returns the text output by the tool (if any).
%
% Throws an exception if an error occurs.
%
-spec get_ssh_mute_option() -> ustring().
get_ssh_mute_option() ->
  " -o \"StrictHostKeyChecking no\" ".



% Returns the MD5 sum computed from the content of the specified file, as an
% unsigned integer, actually of 128 bits (ex:
% 96950473382892364268626543336313804804, corresponding to hexadecimal string
% "48effb631c66e93c7054c10f798f5804").
%
-spec compute_md5_sum( file_path() ) -> md5_sum().
compute_md5_sum( Filename ) ->

	case file_utils:is_existing_file( Filename ) of

		true ->
			ok;

		false ->
			throw( { file_for_md5_not_found, Filename } )

	end,

	% erlang:md5/1 not used here, would be probably slower:

	% Removes the filename after the MD5 code:
	Cmd = system_utils:run_command( get_default_md5_tool() ++ " '"
	  ++ shell_utils:protect_from_shell( Filename ) ++ "' | sed 's|  .*$||1'" ),

	case Cmd of

		{ _ExitCode=0, OutputString } ->
			text_utils:hexastring_to_integer( OutputString,
											  _ExpectPrefix=false );

		{ ExitCode, ErrorOutput } ->
			throw( { md5_computation_failed, ExitCode, ErrorOutput, Filename } )

	end.



% Returns the SHA1 sum computed from the content of the specified file, as an
% unsigned integer, actually of 160 bits (ex:
% 189271338729529876450691804503218393830331783574, corresponding to hexadecimal
% string "212738699f721d8d8c3e58dac2b113bb8d0c1996").
%
-spec compute_sha1_sum( file_path() ) -> sha1_sum().
compute_sha1_sum( Filename ) ->

	%trace_utils:info_fmt( "Computing SHA1 sum of '~ts'.", [ Filename ] ),

	compute_sha_sum( Filename, _SizeOfSHAAlgorithm=1 ).



% Returns the SHA sum computed from the content of the specified file, as an
% unsigned integer, whose size depends on the specified algorithm: 1, 224, 256,
% 384, 512, 512224, 512256 (see 'man shasum' for more details).
%
-spec compute_sha_sum( any_file_path(), basic_utils:count() ) -> sha_sum().
compute_sha_sum( Filename, SizeOfSHAAlgorithm )
  when is_integer( SizeOfSHAAlgorithm ) ->

	case file_utils:is_existing_file( Filename ) of

		true ->
			ok;

		false ->
			throw( { file_for_sha_not_found, Filename } )

	end,

	%trace_utils:debug_fmt( "Computing SHA~B sum of '~ts'.",
	%					  [ SizeOfSHAAlgorithm, Filename ] ),

	% 'utf8' expected as terminal default:
	%trace_utils:debug_fmt( "Filename encoding mode: ~ts",
	%					   [ file:native_name_encoding() ] ),

	% We used to rely on run_command/n, yet correct Unicode arguments ("raw
	% filenames") could not then be properly passed to the executable. So now we
	% rely on run_executable/n.

	% Already a full, resolved executable path:
	ExecPath = get_default_sha_tool(),

	% (not using '++' anymore, as (raw) filenames might have to be binaries;
	% using bin_format/2 to follow Unicode hint in open_port/2:
	%
	Args = [ "--algorithm", text_utils:integer_to_string( SizeOfSHAAlgorithm ),
			 % No need for quoting here:
			 shell_utils:protect_from_shell( Filename ) ],

	%trace_utils:debug_fmt( "SHA executable is: '~ts', arguments are ~p.",
	%					   [ ExecPath, Args ] ),

	case system_utils:run_executable( ExecPath, Args ) of

		{ _ExitCode=0, OutputString } ->
			% Removes the filename after the SHA code:
			{ SHAFullStr, _Rest } =
				text_utils:split_at_first( $ , OutputString ),

			% Workaround for the *sum tools that for some reason (most probably
			% a bug) add a \ before the sum of a file named for example
			% foobar\\.text:
			%
			SHAStr = case SHAFullStr of

				[ $\\ | T ] ->
					T;

				_ ->
					SHAFullStr

			end,

			case text_utils:try_string_to_integer( SHAStr, _Base=16 ) of

				undefined ->
					trace_utils:error_fmt(
					  "SHA interpretation failed for '~ts', based on:~n~ts.",
					  [ Filename, SHAStr ] ),
					throw( { sha_computation_failed, invalid_result,
							 Filename, SHAStr } );

				Sum ->
					Sum

			end;

		{ ExitCode, ErrorOutput } ->
			trace_utils:error_fmt( "SHA computation failed for '~ts': ~ts.",
								   [ Filename, ErrorOutput ] ),
			throw( { sha_computation_failed, ExitCode, ErrorOutput, Filename } )

	end.



% Section about default tools.


% For each supported third-party feature X (ex: X=image_viewer), two functions
% are to be defined:
%
%  - get_default_X_name() -> ustring() that returns the name of the tool (useful
%  for error messages)
%
%  - get_default_X_path() -> executable_path() that returns the full path
%  to the corresponding executable



% Returns the name of the default image viewer tool.
%
% Could be also: xv, firefox, etc.
%
-spec get_default_image_viewer_name() -> executable_name().
get_default_image_viewer_name() ->
	% Viewer is 'eye of gnome' here:
	"eog".


% Returns an absolute path to the default image viewer tool.
-spec get_default_image_viewer_path() -> executable_path().
get_default_image_viewer_path() ->
	find_executable( get_default_image_viewer_name() ).



% Returns the name of the default image browser tool.
%
% Used to be: gqview (renamed since then).
%
-spec get_default_image_browser_name() -> executable_name().
get_default_image_browser_name() ->
	% Was a mere compatibility alias for gqview:
	"geeqie".


% Returns an absolute path to the default image browser tool.
-spec get_default_image_browser_path() -> executable_path().
get_default_image_browser_path() ->
	case get_default_image_browser_name() of

		% Workaround for some distributions:
		Tool="geeqie" ->
			find_executable( Tool ) ++ " --disable-clutter";

		OtherTool ->
			find_executable( OtherTool )

	end.


% Returns the name of the default web browser.
-spec get_default_web_browser_name() -> executable_name().
get_default_web_browser_name() ->
	"firefox".


% Returns an absolute path to the default web browser tool.
-spec get_default_web_browser_path() -> executable_path().
get_default_web_browser_path() ->
	find_executable( get_default_web_browser_name() ).



% Returns the name of the default PDF viewer tool.
% Could be also: xpdf, acroread, etc.
-spec get_default_pdf_viewer_name() -> executable_name().
get_default_pdf_viewer_name() ->
	"evince".


% Returns an absolute path to the default PDF viewer tool.
-spec get_default_pdf_viewer_path() -> executable_path().
get_default_pdf_viewer_path() ->
	find_executable( get_default_pdf_viewer_name() ).



% Returns the name of the default text viewer tool.
% Could be also: nedit, emacs, etc.
-spec get_default_text_viewer_name() -> executable_name().
get_default_text_viewer_name() ->
	"gedit".


% Returns an absolute path to the default text viewer tool.
-spec get_default_text_viewer_path() -> executable_path().
get_default_text_viewer_path() ->
	find_executable( get_default_text_viewer_name() ).



% Returns the name of the default viewer tool for wider texts.
-spec get_default_wide_text_viewer_name( width() ) -> executable_name().
get_default_wide_text_viewer_name( _CharacterWidth ) ->
	% Could be: "nedit":
	"gedit".


% Returns an absolute path to the default viewer tool for wider texts.
-spec get_default_wide_text_viewer_path( width() ) -> executable_path().
get_default_wide_text_viewer_path( CharacterWidth ) ->
	% Could be: io_lib:format( "nedit -column ~B", [ CharacterWidth ] )
	find_executable( get_default_wide_text_viewer_name( CharacterWidth ) ).



% Returns the name of the default trace viewer tool.
%
% Could be also: nedit, gedit, etc.
%
-spec get_default_trace_viewer_name() -> executable_name().
get_default_trace_viewer_name() ->

	case system_utils:has_graphical_output() of

		true ->
			find_executable( "logmx.sh" );

		false ->
			% Poor's man solution:
			"/bin/cat"

	end.



% Returns an absolute path to the default trace viewer tool.
%
% Could be also: nedit, gedit, etc.
%
-spec get_default_trace_viewer_path() -> executable_path().
get_default_trace_viewer_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_trace_viewer_name() ).



% Returns an absolute path to the root directory of the current Erlang
% installation.
%
% Ex: if 'erl' is to be found in
% ~/Software/Erlang/Erlang-current-install/bin/erl, will return:
% ~/Software/Erlang/Erlang-current-install.
%
-spec get_default_erlang_root() -> directory_path().
get_default_erlang_root() ->
	file_utils:normalise_path( file_utils:join( [
		get_default_erlang_interpreter_path(), "..", "..", "..", "..", ".." ]
											  ) ).



% Returns the name of the default Erlang interpreter.
-spec get_default_erlang_interpreter_name() -> executable_name().
get_default_erlang_interpreter_name() ->
	"erl".


% Returns an absolute path to the default Erlang interpreter.
-spec get_default_erlang_interpreter_path() -> executable_path().
get_default_erlang_interpreter_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_erlang_interpreter_name() ).



% Returns the name of the default SSH client.
-spec get_default_ssh_client_name() -> executable_name().
get_default_ssh_client_name() ->
	"ssh".


% Returns an absolute path to the default SSH client.
-spec get_default_ssh_client_path() -> executable_path().
get_default_ssh_client_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_ssh_client_name() ).



% Returns the name default SSH-based scp executable.
-spec get_default_scp_executable_name() -> executable_name().
get_default_scp_executable_name() ->
	"scp".


% Returns an absolute path to the default SSH-based scp executable.
-spec get_default_scp_executable_path() -> executable_path().
get_default_scp_executable_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_scp_executable_name() ).



% Returns the name default openssl-based executable.
-spec get_default_openssl_executable_name() -> executable_name().
get_default_openssl_executable_name() ->
	"openssl".


% Returns an absolute path to the default openssl-based executable.
-spec get_default_openssl_executable_path() -> executable_path().
get_default_openssl_executable_path() ->
	% Note: expected to be on the PATH:
	find_executable( get_default_openssl_executable_name() ).



% Returns an absolute path to a gnuplot executable.
-spec get_gnuplot_path() -> executable_path().
get_gnuplot_path() ->
	% Note: expected to be on the PATH:
	find_executable( "gnuplot" ).



% Returns, as a tuple (ex: {4,2} for the 4.2 version), the gnuplot version
% actually available on the computer.
%
-spec get_current_gnuplot_version() -> basic_utils:two_digit_version().
get_current_gnuplot_version() ->

	% gnuplot -V returns information like "gnuplot 4.4 patchlevel 0"; rather
	% that evaluation a shell expression like:
	%
	% Cmd = get_gnuplot_path() ++ " -V | awk '{print $2}'",
	%
	% we prefer executing directly gnuplot, have then the exit status, and parse
	% the result in Erlang:
	%
	Cmd = get_gnuplot_path() ++ " -V",

	% The returned value of following command is like "4.2":
	%
	case system_utils:run_command( Cmd ) of

			{ _ExitCode=0, Output } ->
				GnuplotVersionInString = lists:nth( _IndexVersion=2,
								text_utils:split_per_element( Output, " " ) ),
				basic_utils:parse_version( GnuplotVersionInString );

			{ ExitCode, ErrorOutput } ->
				throw( { gnuplot_version_detection_failed, ExitCode,
						 ErrorOutput } )

	end.



% Returns the default tool to use to compress in the ZIP format.
-spec get_default_zip_compress_tool() -> executable_path().
get_default_zip_compress_tool() ->
	find_executable( "zip" ).


% Returns the default tool to use to decompress in the ZIP format.
-spec get_default_zip_decompress_tool() -> executable_path().
get_default_zip_decompress_tool() ->
	find_executable( "unzip" ).


% Returns the default tool to use to decompress in the BZIP2 format.
-spec get_default_bzip2_compress_tool() -> executable_path().
get_default_bzip2_compress_tool() ->
	find_executable( "bzip2" ).


% Returns the default tool to use to decompress in the BZIP2 format.
-spec get_default_bzip2_decompress_tool() -> executable_path().
get_default_bzip2_decompress_tool() ->
	find_executable( "bunzip2" ).


% Returns the default tool to use to compress in the XZ format.
-spec get_default_xz_compress_tool() -> executable_path().
get_default_xz_compress_tool() ->
	find_executable( "xz" ).


% Returns the default tool to use to decompress in the XZ format.
-spec get_default_xz_decompress_tool() -> executable_path().
get_default_xz_decompress_tool() ->
	find_executable( "unxz" ).


% Returns the default tool to compute MD5 sums.
-spec get_default_md5_tool() -> executable_path().
get_default_md5_tool() ->
	find_executable( "md5sum" ).


% Returns the default tool to compute SHA sums.
-spec get_default_sha_tool() -> executable_path().
get_default_sha_tool() ->
	find_executable( "shasum" ).



% Returns the default tool to execute Java programs.
-spec get_default_java_runtime() -> executable_path().
get_default_java_runtime() ->
	find_executable( "java" ).


% Returns the default path to the .jar file implementing JInterface,
% i.e. 'OtpErlang.jar'.
%
% Indeed, to make use of JInterface, OtpErlang.jar must be found by the
% counterpart Java program.
%
% We chose conventionally its location to be
% $(ERLANG_ROOT)/lib/erlang/jinterface/priv/OtpErlang.jar,
% with ERLANG_ROOT being typically ~/Software/Erlang/Erlang-current-install.
%
% Indeed, we expect that in $(ERLANG_ROOT)/lib/erlang/ a symbolic link named
% 'jinterface' has been specifically created in order to point to the directory
% of the corresponding version of JInterface (ex: lib/jinterface-1.8/); our
% install-erlang.sh script automatically enforces that convention.
%
-spec get_default_jinterface_path() -> file_path().
get_default_jinterface_path() ->

	JInterfaceBase = file_utils:join(
			[ get_default_erlang_root(), "lib", "erlang", "jinterface" ] ),

	% Can be directory or, more probably, symlink:
	case file_utils:is_existing_directory_or_link( JInterfaceBase ) of

		true ->
			JInterfaceJar = file_utils:join(
							[ JInterfaceBase, "priv", "OtpErlang.jar" ] ),

			case file_utils:is_existing_file( JInterfaceJar ) of

				true ->
					JInterfaceJar;

				false ->
					throw( { jinterface_jar_not_found, JInterfaceJar } )

			end;

		false ->
			trace_utils:error_fmt( "The JInterface base path (~ts) does not "
				"exist; conventionally this is a symbolic link pointing to, "
				"typically, 'lib/jinterface-x.y/'.", [ JInterfaceBase ] ),
			throw( { jinterface_base_path_not_found, JInterfaceBase } )

	end.


% Miscellaneous section:


% Tells whether the program is run in batch mode.
%
% By default, a program is not in batch mode (hence is in interactive mode,
% meaning it might trigger graphical displays).
%
% The most prioritary setting is if the "--batch" command line argument has been
% specified, provided it has been set as a plain argument, i.e. one that it is
% specified *after* either "--" or, preferably, "-extra".
%
% Otherwise, the application configuration will be read for the is_batch key
% (typically set from any conf/sys.config file defined by the application; see
% also the "-config" command-line option in
% https://erlang.org/doc/man/config.html).
%
% Finally, if not set elsewhere, the application resource file (*.app) will be
% searched for such an is_batch key.
%
% Note that, if relying on application configuration, the result will depend on
% the {application name, callsite} pair. Indeed, if application foo depends on
% application bar, and foo defined in its conf/sys.config file {is_batch,false}
% whereas bar defined in its own configuration file {is_batch,true}, should a
% process belonging to bar call this function, it will return false.
%
-spec is_batch() -> boolean().
is_batch() ->

	% Corresponds to the '--batch' command-line option (a *plain* argument,
	% hence expected to be after a -extra command-line switch):
	%
	case shell_utils:get_command_arguments_for_option( '-batch' ) of

		% Normal case if set on the command-line:
		[ [] ] ->
			%trace_utils:debug( "Batch mode activated through command line." ),
			true;

		L when is_list( L ) ->
			trace_utils:error_fmt( "The '--batch' option does not imply any "
				"associated value, whereas the following was specified: '~p'.",
				[ L ] ),
			throw( { unexpected_batch_options, L } );

		% Normal case if not set on the command-line:
		undefined ->
			case application:get_env( is_batch ) of

				{ ok, true } ->
					%trace_utils:debug(
					%  "Batch mode enabled through configuration." ),
					true;

				{ ok, false } ->
					%trace_utils:debug(
					%  "Batch mode disabled through configuration." ),
					false;

				undefined ->
					%trace_utils:debug("Batch mode disabled (default)." ),
					% Default then is:
					false

			end

	end.




% Helper functions.

-spec execute_dot( file_name(), file_name() ) -> command_output().
execute_dot( PNGFilename, GraphFilename ) ->

	DotExec = find_executable( ?dot_tool ),

	Cmd = DotExec ++ " -o" ++ PNGFilename ++ " -Tpng " ++ GraphFilename,

	% Dot might issue non-serious warnings:
	case system_utils:run_command( Cmd ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { rendering_failed, GraphFilename, PNGFilename, ExitCode,
					 ErrorOutput } )

	end.
