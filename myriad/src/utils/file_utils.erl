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


% Gathering of various convenient facilities regarding files.
%
% See file_utils_test.erl for the corresponding test.
%
-module(file_utils).


% Related standard modules: file, filename.


% Note: using the file module has been known to cause synchronization overheads,
% often prim_file is used instead.


% Filename-related operations.
-export([ join/1, join/2, bin_join/1, bin_join/2, any_join/1, any_join/2,

		  get_base_path/1, get_last_path_element/1,

		  convert_to_filename/1, escape_path/1,

		  get_extensions/1, get_extension/1, remove_extension/1,
		  replace_extension/3,

		  exists/1, get_type_of/1, get_owner_of/1, get_group_of/1,
		  is_file/1,
		  is_existing_file/1, is_existing_link/1,
		  is_existing_file_or_link/1,
		  is_owner_readable/1, is_owner_writable/1, is_owner_executable/1,
		  is_user_readable/1, is_user_writable/1, is_user_executable/1,

		  is_directory/1, is_existing_directory/1,
		  is_existing_directory_or_link/1,
		  list_dir_elements/1, list_dir_elements/2,

		  get_size/1, get_last_modification_time/1, touch/1,
		  create_empty_file/1,

		  get_current_directory/0, get_bin_current_directory/0,
		  set_current_directory/1,

		  get_first_existing_directory_in/1,

		  filter_by_extension/2, filter_by_extensions/2,
		  filter_by_included_suffixes/2, filter_by_excluded_suffixes/2,
		  has_matching_suffix/2,

		  find_files_from/1, find_files_from/2, find_files_from/3,
		  find_regular_files_from/1, find_links_from/1, find_links_from/2,
		  find_files_with_extension_from/2, find_files_with_extension_from/3,
		  find_files_with_excluded_dirs/2,
		  find_files_with_excluded_suffixes/2,
		  find_files_with_excluded_suffixes/3,
		  find_files_with_excluded_dirs_and_suffixes/3,
		  find_directories_from/1,

		  create_directory/1, create_directory/2,
		  create_directory_if_not_existing/1,
		  create_directory_if_not_existing/2,
		  create_temporary_directory/0,

		  remove_file/1, remove_file_if_existing/1,
		  remove_files/1, remove_files_if_existing/1,

		  remove_empty_directory/1, remove_empty_path/1, remove_empty_tree/1,
		  remove_directory/1,

		  copy_file/2, try_copy_file/2, copy_file_if_existing/2, copy_file_in/2,
		  copy_tree/2,

		  rename/2, move_file/2, create_link/2,

		  get_non_clashing_entry_name_from/1,

		  append_file/2,

		  list_permission_pairs/0, to_permission_mask/1, from_permission_mask/1,

		  get_permissions_of/1, change_permissions/2,

		  is_absolute_path/1,
		  ensure_path_is_absolute/1, ensure_path_is_absolute/2,
		  normalise_path/1, make_relative/1, make_relative/2,
		  get_longest_common_path/1,

		  is_leaf_among/2,

		  update_with_keywords/3, update_with_keywords/4,

		  path_to_variable_name/1, path_to_variable_name/2,

		  remove_upper_levels_and_extension/1,

		  get_image_extensions/0, get_image_file_png/1, get_image_file_gif/1 ]).



% I/O section.
-export([ get_default_encoding/0, get_default_encoding_option/0,
		  open/2, open/3, close/1, close/2,
		  read/2, write/2, write_ustring/2, write_ustring/3,
		  read_whole/1, write_whole/2, write_whole/3,
		  read_terms/1, write_terms/2, write_terms/4, write_direct_terms/2 ]).


% Compression-related operations.
-export([ get_extension_for/1,
		  compress/1, compress/2, decompress/1, decompress/2,
		  file_to_zipped_term/1, zipped_term_to_unzipped_file/1,
		  zipped_term_to_unzipped_file/2,
		  files_to_zipped_term/1, files_to_zipped_term/2,
		  zipped_term_to_unzipped_files/1, zipped_term_to_unzipped_files/2 ]).



% For the file_info record:
-include_lib("kernel/include/file.hrl").


% For default_encoding*:
-include("system_utils.hrl").



% Type declarations:

% A path may designate either a file or a directory (in both case with leading,
% root directories possibly specified).
%
-type path() :: ustring().
-type bin_path() :: binary().

% We do not believe that atoms shall be legit paths:
-type any_path() :: path() | bin_path().


% Designates a filename, generally without a path (ex: "foobar.txt"):
-type file_name() :: path().

% Just a convenience alias:
-type filename() :: file_name().


% Designates a path to a file (including its filename); ex:
% "../my_dir/other/foobar.txt".
%
-type file_path() :: path().


-type bin_file_name() :: binary().
-type bin_file_path() :: binary().


% Could also be the more general file:name_all():
-type any_file_name() :: file_name() | bin_file_name().

-type any_file_path() :: file_path() | bin_file_path().


% The name of a (symbolic) link:
-type link_name() :: ustring().


% Designates an executable, generally without a path (ex: "foobar"):
-type executable_name() :: file_name().


% Designates a path to an executable; ex: "../my_dir/other/run.exe").
-type executable_path() :: file_path().

% Designates a path to an executable, as a binary.
-type bin_executable_path() :: bin_file_path().


% Designates a path to an (executable) script; ex: "../my_dir/other/run.sh").
-type script_path() :: file_path().

% Designates a path to an (executable) script, as a binary.
-type bin_script_path() :: bin_file_path().


-type directory_name() :: path().
-type bin_directory_name() :: binary().

-type any_directory_name() :: directory_name() | bin_directory_name().


-type directory_path() :: path().
-type bin_directory_path() :: binary().

-type any_directory_path() :: directory_path() | bin_directory_path().

% Sometimes useful:
-type abs_directory_path() :: directory_path().


% An extension in a filename (ex: "baz", in "foobar.baz.json"):
-type extension() :: ustring().

% The suffix (final part) in a path element:
-type any_suffix() :: any_string().

% A part of a path (ex: "local" in "/usr/local/share"):
-type path_element() :: ustring().


% A part of a path (ex: <<"local">> in "/usr/local/share"):
-type bin_path_element() :: text_utils:bin_string().


% Any type of a part of a path (ex: <<"local">> in "/usr/local/share"):
-type any_path_element() :: path_element() | bin_path_element().


% A leaf name, i.e. the final element of a path (possibly a file or directory).
%
% Ex: in 'aaa/bbb/ccc', 'aaa' is the root, and 'ccc' is the leaf.
%
-type leaf_name() :: path_element().



% All known types of file entries:
-type entry_type() :: 'device' | 'directory' | 'other' | 'regular' | 'symlink'.


% Tells whether parent directories shall be created:
-type parent_creation() :: 'create_no_parent' | 'create_parents'.


% Relevant flags when opening a file (ex: read, write, append, exclusive, raw,
% etc.).
%
% See http://erlang.org/doc/man/file.html#open-2 for their detailed description.
%
% (file:mode() not exported currently unfortunately, see
% lib/kernel/src/file.erl)
%
%-type file_open_mode() :: file:mode() | 'ram'.
-type file_open_mode() :: tuple() | atom() | 'ram'.


% The supported compression formats:
-type compression_format() :: 'zip' | 'bzip2' | 'xz'.


% Corresponds to the handle to an open file (typically a file descriptor
% counterpart), but also, possibly, 'standard_io' (for standard output,
% descriptor 1), 'standard_error' (for standard error, descriptor 2), a
% registered name (as an atom), or any PID handling the I/O protocols:
%
-type file() :: file:io_device().

-type file_info() :: #file_info{}.



% The various permissions that can be combined for file-like elements:
-type permission() :: 'owner_read'  | 'owner_write' | 'owner_execute'
					| 'group_read'  | 'group_write' | 'group_execute'
					| 'other_read'  | 'other_write' | 'other_execute'
					| 'set_user_id' | 'set_group_id'.


% The binary mask corresponding to a filesystem permission:
-type permission_mask() :: non_neg_integer().


% Action to be trigger whenever a file element has not a proper Unicode
% filename:
%
% (refer to
%https://erlang.org/doc/apps/stdlib/unicode_usage.html#notes-about-raw-filenames
% for further information)
%
-type improper_encoding_action() ::

		'throw'    % Throw an exception as soon as a raw filename is found

	  | 'warn'     % Emit a warning trace if a raw filename is found, and do
				   % not consider the corresponding file element

	  | 'ignore'   % Ignore as a whole such a file element (do not even emit
				   % a trace)

	  | 'include'. % Return such raw filenames (thus as binaries) among the
				   % other ones (which are plain strings)

% We previously considered also (was not satisfactory, as introducing a
% different return type):
%	  | 'list'.    % List separately (as a returned pair) the raw filenames
%                  % (regardless of their actual filesystem-level type)


-export_type([ path/0, bin_path/0, any_path/0,
			   file_name/0, filename/0, file_path/0,
			   bin_file_name/0, bin_file_path/0,
			   any_file_name/0, any_file_path/0,
			   any_directory_name/0, any_directory_path/0, abs_directory_path/0,
			   executable_name/0, executable_path/0, bin_executable_path/0,
			   script_path/0, bin_script_path/0,
			   directory_name/0, bin_directory_name/0,
			   directory_path/0, bin_directory_path/0,
			   extension/0, any_suffix/0,
			   path_element/0, bin_path_element/0, any_path_element/0,
			   leaf_name/0,
			   entry_type/0, parent_creation/0,
			   permission/0, permission_mask/0, improper_encoding_action/0,
			   compression_format/0,
			   file/0, file_info/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type any_string() :: text_utils:any_string().
-type format_string() :: text_utils:format_string().



% Regarding encodings and Unicode:
%
% - their support may be specified when opening a file, notably for writing
% (then a transparent encoding will be done), yet we found it safer and offering
% more control not to request such an automatic encoding, and to secure it by
% ourselves, either by relying on write_ustring/{2,3} or by calling write/2 with
% a content that is already properly encoded (see
% text_utils:to_unicode_{list,binary}/{1,2}); otherwise for example a double
% encoding could easily happen or, possibly, the encoding may fail with little
% control; so we tend now to stay away from get_default_encoding_option/0 for
% example and use the previous functions instead
%
% - more precisely, specifying an encoding like {encoding, utf8} at file opening
% was troublesome in our test cases, as we were not able to properly write
% strings like "cœur" afterwards (no matter any encoding or lack thereof was
% experimented); as mentioned, it proved useful to open such a file for writing
% without specifying any encoding, and then only to write it directly with
% pre-encoded content (a "~ts" formatter then sufficed); so the 'encoding'
% options, at least for writing, may not be that convenient
%
% - so the content itself may have to be encoded before writing; for example,
% writing "éèôù" (interpreted to be latin1 or alike) in a file opened as utf8
% will result in a garbled content, unless it has been converted beforehand,
% typically thanks to our to_unicode_{list,binary}/{1,2}
%
% - it seems possible that in some cases specifying the 'raw' option result in
% the requested encoding (ex: utf8) not being respected (ex: having ISO-8859
% instead); with newer versions of Myriad and of Erlang, we believe this issue
% does not exist anymore
%
% - some file elements may be improperly named regarding Unicode encoding ("raw
% filenames"); use list_dir_elements/2 to decide how they should be handled
%
% - notably in this module, calls akin to text_utils:binary_to_string/1 shall be
% carefully studied, as conversions from binaries to strings shall be avoided
% whenever possible due to their limitations

% - the way the VM is started matters; see the comment about the "-noinput"
% option, in open/{2,3}; one may use the following to check the current settings
% of the VM:
%
% trace_utils:info_fmt( "Encoding: ~p.",
%					  [ lists:keyfind(encoding, 1, io:getopts()) ] ),
%
% See also:
% https://erlang.org/doc/apps/stdlib/unicode_usage.html#unicode-data-in-files
% Summary: use the 'file' module only for files opened for bytewise access
% ({encoding,latin1}) - otherwise use the 'io' module.


% Regarding identifiers (ex: user_id), they can be converted in actual names,
% yet apparently with nothing simpler than:

% awk -v val=USER_ID -F ":" '$3==val{print $1}' /etc/passwd



% Filename-related operations.


% Platform-specific:

% UNIX conventions:
-define( directory_separator, $/ ).

% Windows conventions:
%-define( directory_separator, $\ ).


% For proper naming in filesystems:

% Replaces each series of spaces (' '), lower than ('<'), greater than ('>'),
% comma (','), left ('(') and right (')') parentheses, single (''') and double
% ('"') quotes, forward ('/') and backward ('\') slashes, ampersand ('&'), tilde
% ('~'), sharp ('#'), at sign ('@'), all other kinds of brackets ('{', '}', '[',
% ']'), pipe ('|'), dollar ('$'), star ('*'), marks ('?' and '!'), plus ('+'),
% other punctation signs (';' and ':') by exactly one underscore:
%
% (see also: net_utils:generate_valid_node_name_from/1)
%
-define( patterns_to_replace_for_paths, "( |<|>|,|\\(|\\)|'|\"|/|\\\\|\&|~|"
		 "#|@|{|}|\\[|\\]|\\||\\$|\\*|\\?|!|\\+|;|:)+" ).

-define( replacement_for_paths, "_" ).



% No cross-module inlining unfortunately (parse-transforms...):
-compile( { inline, [ get_base_path/1, get_last_path_element/1 ] } ).



% Joins the specified list of path elements.
%
% This function has been added back to this module; filename:join( Components )
% could be used instead (at least to some extent), however filename:join( [ "",
% "my_dir"] ) results in "/my_dir", whereas often we would want "my_dir"
% instead - which is returned by our function; moreover, if one of the
% components includes an absolute path (such as "/xxx" with Unix conventions),
% the preceding components, if any, were removed from the result (which does not
% seem desirable); here we throw an exception instead.
%
% So we deem our version simpler and less prone to surprise (least
% astonishment).
%
% Plain and binary strings can be freely used as arguments, and a plain string
% is returned in all cases.
%
% See filename:split/1 for the reverse operation.
%
-spec join( [ any_path_element() ] ) -> path().
join( ComponentList ) when is_list( ComponentList ) ->
	lists:foldr( fun join/2, _Acc0="", _List=ComponentList );

join( NonList ) ->
	throw( { cannot_join, NonList } ).



% Joins the two specified path elements, returns a corresponding plain string.
%
% This function has been added back to this module; filename:join(Name1, Name2)
% could be used instead (at least to some extent); however filename:join("",
% "my_dir") results in "/my_dir", whereas often we would want "my_dir" - which
% is returned by our function ; moreover filename:join(SomePath, AbsPath=[
% ?directory_separator | _ ]) returns AbsPath, dropping SomePath for some reason
% (which does not seem desirable); here we throw an exception instead.
%
% So we deem our version simpler and less prone to surprise (least
% astonishment).
%
% Plain and binary strings can be freely used as arguments; a plain string is
% returned in all cases.
%
% See filename:split/1 for the reverse operation.
%
% Prefer bin_join/2 if having to possibly deal with so-called "raw filenames".
%
-spec join( any_path(), any_path() ) -> path().
% Skips only initial empty paths of all sorts:
join( _FirstPath="", SecondPath ) ->
	SecondPath ;

join( _FirstPath= <<"">>, SecondPath ) ->
	SecondPath ;

% If second is string (already):
join( FirstPath, SecondPath=[ ?directory_separator | _ ] ) ->
	throw( { rightwise_absolute_directory, SecondPath,
			 { leftwise, FirstPath } } );

join( FirstPath, SecondPath ) when is_binary( FirstPath ) ->
	join( text_utils:binary_to_string( FirstPath ), SecondPath );

% Second as string to match separator above:
join( FirstPath, SecondPath ) when is_binary( SecondPath ) ->
	join( FirstPath, text_utils:binary_to_string( SecondPath ) );

% Both are strings from this point:
join( FirstPath, _SecondPath="" ) ->
	% We do not want to add a trailing separator (ex: we want "/home/lisa", not
	% "/home/lisa/"):
	%
	FirstPath;

join( FirstPath, SecondPath ) ->

	% First as at least one element by design; avoid adding an extra separator:
	%
	% (we do not use list_utils:get_last_element/1 here as we do not want this
	% very common function of this file_utils module to depend on a list_utils
	% one):
	%
	case get_last_element( FirstPath ) of

		?directory_separator ->
			text_utils:format( "~ts~ts", [ FirstPath, SecondPath ] );

		_ ->
			text_utils:format( "~ts~c~ts",
				[ FirstPath, ?directory_separator, SecondPath ] )

	end.



% Joins the specified list of path elements, returns a corresponding binary
% string.
%
% See join/1 for API details.
%
% Plain and binary strings can be freely used as arguments, and a binary string
% is returned in all cases.
%
% See filename:split/1 for the reverse operation.
%
-spec bin_join( [ any_path_element() ] ) -> bin_path().
bin_join( ComponentList ) when is_list( ComponentList ) ->
	lists:foldr( fun bin_join/2, _Acc0="", _List=ComponentList );

bin_join( NonList ) ->
	throw( { cannot_join, NonList } ).



% Joins the two specified path elements, returns a corresponding binary string.
%
% Never attempts a binary-to-string conversion.
%
% Introduced to support the case where at least one argument is an
% improperly-encoded Unicode binary path: any operation implying a conversion to
% string of it will fail, so the operation must take place exclusively among
% binaries.
%
-spec bin_join( any_path(), any_path() ) -> bin_path().
% Use the same semantics as join/2:
bin_join( _FirstPath="", SecondPath ) ->
	text_utils:ensure_binary( SecondPath );

bin_join( _FirstPath= <<"">>, SecondPath ) ->
	text_utils:ensure_binary( SecondPath ) ;

bin_join( FirstPath, SecondPath )
			when is_binary( FirstPath ) orelse is_binary( SecondPath ) ->
	% As soon as at least one argument of filename:join/2 is a binary, returns a
	% binary, which is what we want:
	%
	filename:join( FirstPath, SecondPath );

% Here both are expected to be plain strings, cannot be a problem:
bin_join( FirstPath, SecondPath ) ->
	filename:join( text_utils:to_unicode_binary( FirstPath ), SecondPath ).



% Joins the specified list of path elements; returns a corresponding binary
% string if at least one element is a binary string itself, otherwise returns a
% plain string.
%
% Never attempts a binary-to-string conversion; introduced to promote to binary
% string only when necessary.
%
% See join/1 for API details.
%
% Plain and binary strings can be freely used as arguments.
%
% See filename:split/1 for the reverse operation.
%
-spec any_join( [ any_path_element() ] ) -> any_path().
any_join( ComponentList ) when is_list( ComponentList ) ->
	lists:foldr( fun any_join/2, _Acc0="", _List=ComponentList );

any_join( NonList ) ->
	throw( { cannot_join, NonList } ).



% Joins the two specified path elements; returns a corresponding binary string
% if at least one element is a binary string itself, otherwise returns a plain
% string.
%
% Never attempts a binary-to-string conversion; introduced to promote to binary
% string only when necessary.
%
-spec any_join( any_path(), any_path() ) -> any_path().
% Use the same semantics as join/2:
any_join( _FirstPath="", SecondPath ) ->
	SecondPath;

any_join( _FirstPath= <<"">>, SecondPath ) ->
	text_utils:ensure_binary( SecondPath ) ;

% As soon as at least one argument of filename:join/2 is a binary, returns a
% binary, otherwise returns a plain string:
%
any_join( FirstPath, SecondPath )  ->
	filename:join( FirstPath, SecondPath ).



% Duplicated verbatim from list_utils, so that file_utils can remain a mostly
% autonomous, pioneer module.
%
% Returns the last element of the specified list.
%
% Note: not computationnally efficient, usually having to retrieve the last
% element suggests a bad code design.
%
% Crashes (with 'no function clause') if the input list is empty.
%
%-spec get_last_element( list() ) -> element().
get_last_element( _List=[ SingleElement ] ) ->
	SingleElement;

get_last_element( _List=[ _H | T ] ) ->
	get_last_element( T ).



% Returns the complete leading, "directory" part of specified path,
% i.e. the one with all its element but the last one.
%
% Ex: "/aaa/bbb/ccc" =
%          file_utils:get_base_path("/aaa/bbb/ccc/foobar.txt").
%
% Note that the return type is the same of the input path, i.e. plain string or
% binary string.
%
% Alias name for filename:dirname/1 (better in file_utils, and hopefully
% clearer).
%
-spec get_base_path( any_path() ) -> any_path().
get_base_path( AnyPath ) ->
	filename:dirname( AnyPath ).



% Returns the final, "file" part of specified path, i.e. its last element, as a
% one-element path, corresponding either to a file or a directory.
%
% Ex: <<"foobar.txt">> =
%          file_utils:get_last_path_element(<<"/aaa/bbb/ccc/foobar.txt">>).
%
% Note that the return type is the same of the input path, i.e. plain string or
% binary string.
%
% Replacement name for filename:basename/1 (in file_utils, and hopefully
% clearer).
%
-spec get_last_path_element( any_path() ) -> any_path().
get_last_path_element( AnyPath ) ->
	filename:basename( AnyPath ).




% Converts specified name to an acceptable filename, filesystem-wise.
% Returns the same string type as the parameter.
%
-spec convert_to_filename( any_string() ) -> any_file_name().
convert_to_filename( BinName ) when is_binary( BinName ) ->
	re:replace( BinName, ?patterns_to_replace_for_paths, ?replacement_for_paths,
				[ global, { return, binary } ] );

convert_to_filename( Name ) ->

	% Currently we use exactly the same translation rules both for node names
	% and file names (see net_utils:generate_valid_node_name_from/1).

	% Note however that now we duplicate the code instead of calling the
	% net_utils module from here, as otherwise there would be one more module
	% to deploy under some circumstances.

	re:replace( lists:flatten( Name ), ?patterns_to_replace_for_paths,
				?replacement_for_paths, [ global, { return, list } ] ).



% Escapes specified path so that it can safely be included as string content.
%
% Returns the same type of string as the specified one.
%
-spec escape_path( any_path() ) -> any_string().
escape_path( Path ) when is_list( Path ) ->
	% To properly flatten:
	text_utils:to_unicode_list(
	  string:replace( Path, _SearchPattern="\"", _Replacement="\\\"",
					  _Direction=all), _CanFail=true );

escape_path( BinPath ) ->

	Direction = all,

	DoubleQuoteEscapedBin =
		string:replace( BinPath, _DQSearchPattern="\.", _DQReplacement="\\.",
						Direction ),

	AntiSlashesEscapedBin =
		string:replace( DoubleQuoteEscapedBin, _ASSearchPattern="\"",
						_ASReplacement="\\\"", Direction ),

	% Not wanting for example [<<XX>>, 92, 34, <<YY>>]:
	text_utils:to_unicode_binary( AntiSlashesEscapedBin, _CanFail=true ).



% Returns the (ordered) extension(s) of the specified filename.
%
% Ex: ["baz", "json"] = get_extensions("foobar.baz.json")
%
-spec get_extensions( file_name() ) -> [ extension() ] | 'no_extension'.
get_extensions( Filename ) ->

	case text_utils:split( Filename, _Delimiters=[ $. ] ) of

		[] ->
			no_extension;

		[ _Basename ] ->
			no_extension;

		[ _Basename | Extensions ] ->
			Extensions;

		_ ->
			no_extension

	end.



% Returns the (last) extension of the specified filename.
%
% Ex: "json" = get_extension( "foobar.baz.json" )
%
-spec get_extension( file_name() ) -> extension() | 'no_extension'.
get_extension( Filename ) ->

	case get_extensions( Filename ) of

		no_extension ->
			no_extension;

		Extensions ->
			list_utils:get_last_element( Extensions )

	end.



% Removes the (last) extension of the specified filename.
%
% Ex: "/home/jack/rosie.tmp" = remove_extension( "/home/jack/rosie.tmp.ttf" )
%
-spec remove_extension( file_path() ) -> file_path().
remove_extension( FilePath ) ->

	case text_utils:split( FilePath, _Delimiters=[ $. ] ) of

		% Returning an empty string for an empty string:
		[] ->
			FilePath;

		[ FilePath ] ->
			FilePath;

		[ Basename | Extensions ] ->
			text_utils:join( $.,
				[ Basename | list_utils:remove_last_element( Extensions ) ] )

	end.



% Returns a new filename whose extension has been updated.
%
% Ex: replace_extension("/home/jack/rosie.ttf", ".ttf", ".wav") should return
% "/home/jack/rosie.wav".
%
-spec replace_extension( file_path(), extension(), extension() ) -> file_path().
replace_extension( FilePath, SourceExtension, TargetExtension ) ->

	case string:rstr( FilePath, SourceExtension ) of

		0 ->
			throw( { extension_not_found, SourceExtension, FilePath } );

		Index ->
			string:substr( FilePath, 1, Index-1 ) ++ TargetExtension

	end.



% Tells whether specified file entry exists, regardless of its type.
-spec exists( any_path() ) -> boolean().
exists( EntryName ) ->

	case file:read_file_info( EntryName ) of

		{ ok, _FileInfo } ->
			true;

		{ error, _Reason } ->
			false

	end.



% Returns the type of the specified file entry.
-spec get_type_of( any_path() ) -> entry_type().
get_type_of( EntryName ) ->

	% We used to rely on file:read_file_info/1, but an existing symlink pointing
	% to a non-existing entry was triggering the enoent error, while we just
	% wanted to know that the specified entry is an existing (yet dead) symlink.

	% Some tools (e.g. emacs) used thus to get in the way, as apparently they
	% create dead symlinks on purpose, to store information.

	case file:read_link_info( EntryName ) of

		{ ok, #file_info{ type=FileType } } ->
			FileType;

		{ error, eloop } ->
			% Probably a recursive symlink:
			throw( { too_many_symlink_levels, EntryName } );

		{ error, enoent } ->
			throw( { non_existing_entry, EntryName } )

	end.



% Returns the user identifier (uid) of the owner of the specified file entry.
-spec get_owner_of( any_path() ) -> system_utils:user_id().
get_owner_of( EntryName ) ->

	case file:read_file_info( EntryName ) of

		{ ok, #file_info{ uid=UID } } ->
			UID;

		{ error, Reason } ->
			throw( { file_info_failure, Reason, EntryName } )

	end.



% Returns the group identifier (gid) of the group of the specified file entry.
-spec get_group_of( any_path() ) -> system_utils:group_id().
get_group_of( EntryName ) ->

	case file:read_file_info( EntryName ) of

		{ ok, #file_info{ gid=GID } } ->
			GID;

		{ error, Reason } ->
			throw( { file_info_failure, Reason, EntryName } )

	end.



% Returns whether the specified entry, supposedly existing, is a regular file.
%
% If the specified entry happens not to exist, a {non_existing_entry, EntryName}
% exception will be thrown.
%
-spec is_file( any_path() ) -> boolean().
is_file( EntryName ) ->

	case get_type_of( EntryName ) of

		regular ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is a regular file.
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_existing_file( any_path() ) -> boolean().
is_existing_file( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		regular ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is a symbolic file.
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_existing_link( any_path() ) -> boolean().
is_existing_link( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		symlink ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is either a regular file or a
% symbolic link.
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_existing_file_or_link( any_path() ) -> boolean().
is_existing_file_or_link( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		regular ->
			true ;

		symlink ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is readable for its current
% owner (can be either a regular file or a symbolic link) - not telling anything
% about whether the current user can read it.
%
% Returns true or false, and cannot trigger an exception.
%
% See also: is_user_readable/1.
%
-spec is_owner_readable( any_path() ) -> boolean().
is_owner_readable( EntryPath ) ->

	case file:read_file_info( EntryPath ) of

		{ ok, FileInfo } ->

			#file_info{ type=FileType, mode=Mode } = FileInfo,

			case FileType of

				regular ->

					OwnerReadMask = to_permission_mask( owner_read ),
					case Mode band OwnerReadMask of

						0 ->
							% Not readable:
							false;

						_ ->
							% One positive case:
							true

					end;

				_ ->

					false

			end;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is writable for its current
% owner (can be either a regular file or a symbolic link) - not telling anything
% about whether the current user can write it.
%
% Returns true or false, and cannot trigger an exception.
%
% See also: is_user_writable/1.
%
-spec is_owner_writable( any_path() ) -> boolean().
is_owner_writable( EntryPath ) ->

	case file:read_file_info( EntryPath ) of

		{ ok, FileInfo } ->

			#file_info{ type=FileType, mode=Mode } = FileInfo,

			case FileType of

				regular ->

					OwnerWriteMask = to_permission_mask( owner_write ),
					case Mode band OwnerWriteMask of

						0 ->
							% Not writable:
							false;

						_ ->
							% One positive case:
							true

					end;

				_ ->

					false

			end;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is executable for its current
% owner (can be either a regular file or a symbolic link) - not telling anything
% about whether the current user can execute it.
%
% Returns true or false, and cannot trigger an exception.
%
% See also: is_owner_writable/1.
%
-spec is_owner_executable( any_path() ) -> boolean().
is_owner_executable( EntryPath ) ->

	case file:read_file_info( EntryPath ) of

		{ ok, FileInfo } ->

			#file_info{ type=FileType, mode=Mode } = FileInfo,

			case FileType of

				regular ->

					OwnerExecMask = to_permission_mask( owner_execute ),
					case Mode band OwnerExecMask of

						0 ->
							% Not executable:
							false;

						_ ->
							% One positive case:
							true

					end;

				_ ->

					false

			end;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is readable for the current
% user (can be either a regular file or a symbolic link).
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_user_readable( any_path() ) -> boolean().
is_user_readable( EntryPath ) ->

	% Rather than using file:read_file_info/1 and having to try to fetch and
	% filter user/group information, it is easier, maybe more efficient and
	% reliable to try to open it for reading:

	case file:open( _File=EntryPath, _Mode=[ read ] ) of

		{ ok, File } ->
			file:close( File ),
			true;

		{ error, _Reason } ->
			false

	end.



% Returns whether the specified entry exists and is writable for the current
% user (can be either a regular file or a symbolic link).
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_user_writable( any_path() ) -> boolean().
is_user_writable( EntryPath ) ->

	% Rather than using file:read_file_info/1 and having to try to fetch and
	% filter user/group information, it is easier, maybe more efficient and
	% reliable to try to open it for writing:

	case file:open( _File=EntryPath, _Mode=[ write ] ) of

		{ ok, File } ->
			file:close( File ),
			true;

		{ error, _Reason } ->
			false

	end.



% Returns whether the specified entry exists and is executable for the current
% user (can be either a regular file or a symbolic link).
%
% Returns true or false, and cannot trigger an exception.
%
% See also: is_owner_executable/1.
%
-spec is_user_executable( any_path() ) -> boolean().
is_user_executable( EntryPath ) ->
	% WARNING: not properly implemented yet.
	is_owner_executable( EntryPath ).


% Returns whether the specified entry, supposedly existing, is a directory.
%
% If the specified entry happens not to exist, a {non_existing_entry, EntryName}
% exception will be thrown.
%
-spec is_directory( any_path() ) -> boolean().
is_directory( EntryName ) ->

	case get_type_of( EntryName ) of

		directory ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is a directory.
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_existing_directory( any_path() ) -> boolean().
is_existing_directory( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		directory ->
			true ;

		_ ->
			false

	end.



% Returns whether the specified entry exists and is a directory or a symbolic
% link.
%
% Returns true or false, and cannot trigger an exception.
%
-spec is_existing_directory_or_link( any_path() ) -> boolean().
is_existing_directory_or_link( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		directory ->
			true ;

		symlink ->
			true ;

		_ ->
			false

	end.



% Returns a tuple containing five lists corresponding to the per-type
% dispatching of all filesystem elements local to specified directory (hence not
% recursively traversed), namely: {RegularFiles, Symlinks, Directories,
% OtherFiles, Devices}.
%
% If a raw filename is found (i.e. a file element whose name is not properly
% Unicode-encoded), a warning trace is emitted and the corresponding file
% elemen
%
% Note that:
% - symbolic links may or may not be dead
% - only plain strings are returned (any raw filename found will trigger a
% warning trace and then will be ignored)
%
-spec list_dir_elements( directory_name() ) ->
			{ [ file_name() ], [ file_name() ], [ directory_name() ],
			  [ file_name() ], [ file_name() ] }.
list_dir_elements( DirName ) ->
	list_dir_elements( DirName, _ImproperEncodingAction=warn ).



% Returns a tuple containing five lists corresponding to the per-type
% dispatching of all filesystem elements local to specified directory (hence not
% recursively traversed), namely: {RegularFiles, Symlinks, Directories,
% OtherFiles, Devices}.
%
% If a raw filename is found (i.e. a file element whose name is not properly
% Unicode-encoded), ImproperEncodingAction will determine how it will be
% handled.
%
% Note that:
% - symbolic links may or may not be dead
% - generally the returned elements are strings, yet, if ImproperEncodingAction
% is 'include', binaries may also returned, should raw ("incorrectly-encoded")
% filenames be found (they are then returned verbatim, short of being able to
% stringify them)
%
-spec list_dir_elements( directory_name(), improper_encoding_action() ) ->
		{ [ any_file_name() ], [ any_file_name() ], [ any_directory_name() ],
		  [ any_file_name() ], [ any_file_name() ] }.
list_dir_elements( DirName, ImproperEncodingAction ) ->

	%trace_utils:debug_fmt( "list_dir_elements for '~ts'.", [ DirName ] ),

	% Previously file:list_dir_all/1 was tested in order to collect raw
	% filenames as well (hoping to avoid warning reports such as "Non-unicode
	% filename <<"XXX">> ignored"), yet the returned names were mangled, leading
	% to enoent whenever trying to open them :

	% Sometimes filenames are "wrongly encoded" (ex: 'foo'$'\200\246''bar'); if
	% using file:list_dir/1 instead of file:list_dir_all/1, warnings are
	% then issued (only on the console) like:
	%
	% =WARNING REPORT==== 25-Mar-2021::22:41:11.577989 ===
	% Non-unicode filename <<XXX>> ignored
	%
	% Now such elements are listed as binaries (refer to the implementation
	% notes at top, for further detail)

	LocalDirElements = case file:list_dir_all( DirName ) of

		{ ok, LElems } ->
			LElems;

		{ error, enoent } ->
			throw( { directory_not_found, DirName } );

		{ error, Other } ->
			throw( { Other, DirName } )

	end,

	%trace_utils:debug_fmt( "LocalDirElements: ~p", [ LocalDirElements ] ),

	classify_dir_elements( DirName, LocalDirElements, _Devices=[],
		_Directories=[], _Files=[], _Symlinks=[], _OtherFiles=[],
		ImproperEncodingAction ).



% Returns the size, in bytes, of the specified file entry.
-spec get_size( any_file_name() ) -> system_utils:byte_size().
get_size( Filename ) ->

	case file:read_file_info( Filename ) of

		{ ok, #file_info{ size=Size } } ->
			Size;

		{ error, Reason } ->
			throw( { file_info_failure, Reason, Filename } )

	end.



% Returns the last time at which the content of specified file entry was
% modified (not counting attribute or permission changes), according to the
% filesystem.
%
% Said time will be expressed as an integer number of seconds since (or before)
% Unix time epoch, which is 1970-01-01 00:00 UTC.
%
-spec get_last_modification_time( any_path() ) -> time_utils:posix_seconds().
get_last_modification_time( Filename ) ->

	case file:read_file_info( Filename, [ { time, posix } ] ) of

		{ ok, #file_info{ mtime=Seconds } } ->
			Seconds;

		{ error, Reason } ->
			throw( { file_info_failure, Reason, Filename } )

	end.



% Updates the modification time (the last time at which its content was reported
% as modified according to the filesystem) of the specified file entry, which
% must already exist.
%
% Note: leaves last access time unchanged, updates both modification and change
% times.
%
% See also: create_empty_file/1
%
-spec touch( any_path() ) -> void().
touch( FileEntry ) ->

	case exists( FileEntry ) of

		true ->
			% -c: do not create any file
			% -m: change only the modification time
			%
			case system_utils:run_executable( _ExecPath="/bin/touch",
					_Args=[ "-c", "-m", FileEntry ] ) of

				{ 0, _Output } ->
					ok;

				{ ErrorCode, Output } ->
					throw( { touch_failed, Output, ErrorCode, FileEntry } )

			end;

		false ->
			throw( { non_existing_file_element_to_touch, FileEntry } )

	end.



% Creates an empty file bearing the specified filename (other use of touch).
%
% Potentially useful as a last-resort debugging tool (when no console output or
% applicative trace can be relied upon, we can at least leave side-effects on
% the filesystem).
%
% Note: of course a simple 'os:cmd( "/bin/touch ~/my-message.debug" ).' may be
% of use as well.
%
% See also: touch/1.
%
-spec create_empty_file( file_path() ) -> void().
create_empty_file( FilePath ) ->

	case system_utils:run_command( "/bin/touch '" ++ FilePath ++ "'" ) of

		{ 0, _Output } ->
			ok;

		{ ErrorCode, Output } ->
			throw( { empty_file_creation_failed, Output, ErrorCode, FilePath } )

	end.



% Returns the current directory, as a plain string.
%
% Throws an exception on failure.
%
-spec get_current_directory() -> directory_path().
get_current_directory() ->

	case file:get_cwd() of

		{ ok, Dir } ->
			Dir;

		{ error, Reason } ->
			throw( { failed_to_determine_current_directory, Reason } )

	end.



% Returns the current directory, as a binary string.
%
% Throws an exception on failure.
%
-spec get_bin_current_directory() -> bin_directory_path().
get_bin_current_directory() ->
	text_utils:string_to_binary( get_current_directory() ).



% Sets the specified directory as current directory.
%
% Throws an exception on failure.
%
-spec set_current_directory( directory_name() ) -> void().
set_current_directory( DirName ) ->

	 % For more detail of { 'error', atom() }, refer to type specifications of
	 % erlang files: file.erl and file.hrl.

	case file:set_cwd( DirName ) of

		ok ->
			ok;

		{ error, Error } ->
			throw( { set_current_directory_failed, DirName, Error } )

	end.



% Returns the first (if any) existing directory found in specified list, or
% throws an exception if none is found.
%
% Typically useful when having multiple possible paths depending on settings,
% one of them being relevant.
%
-spec get_first_existing_directory_in( [ any_directory_path() ] ) ->
												any_directory_path().
get_first_existing_directory_in( DirPaths ) ->
	get_first_existing_dir( DirPaths, _Acc=[] ).


% (helper)
get_first_existing_dir( _DirPaths=[], Acc ) ->
	throw( { no_existing_directory_found_in, lists:reverse( Acc ),
			 get_current_directory() } );


get_first_existing_dir( _DirPaths=[ Dir | T ], Acc ) ->

	case is_existing_directory( Dir ) of

		true ->
			Dir;

		false ->
			get_first_existing_dir( T, [ Dir | Acc ] )

	end.



% (helper)
%
% Returns a tuple containing five lists corresponding to the sorting of all
% filesystem elements, namely {RegularFiles, Symlinks, Directories, OtherFiles,
% Devices}, depending on ImproperEncodingAction.
%
classify_dir_elements( _DirName, _Elements=[], Devices, Directories, Files,
					   Symlinks, OtherFiles, _ImproperEncodingAction ) ->
	% Note the reordering:
	{ Files, Symlinks, Directories, OtherFiles, Devices };

classify_dir_elements( DirName, _Elements=[ Str | T ], Devices, Directories,
					   Files, Symlinks, OtherFiles, ImproperEncodingAction )
						   when is_list( Str ) ->

	case get_type_of( filename:join( DirName, Str ) ) of

		device ->
			classify_dir_elements( DirName, T, [ Str | Devices ], Directories,
				Files, Symlinks, OtherFiles, ImproperEncodingAction );

		directory ->
			classify_dir_elements( DirName, T, Devices, [ Str | Directories ],
				Files, Symlinks, OtherFiles, ImproperEncodingAction );

		regular ->
			classify_dir_elements( DirName, T, Devices, Directories,
				[ Str | Files ], Symlinks, OtherFiles, ImproperEncodingAction );

		% Used to be managed as regular files:
		symlink ->
			classify_dir_elements( DirName, T, Devices, Directories, Files,
				[ Str | Symlinks ], OtherFiles, ImproperEncodingAction );

		other ->
			classify_dir_elements( DirName, T, Devices, Directories,
				Files, Symlinks, [ Str | OtherFiles ], ImproperEncodingAction )

	end;

% Implicit: when is_binary( Bin ) ->
classify_dir_elements( DirName, _Elements=[ Bin | _T ], _Devices, _Directories,
		_Files, _Symlinks, _OtherFiles, _ImproperEncodingAction=throw ) ->
	throw( { improperly_encoded_file_element, Bin, DirName } );


classify_dir_elements( DirName, _Elements=[ Bin | T ], Devices, Directories,
		Files, Symlinks, OtherFiles, ImproperEncodingAction=warn ) ->

	trace_bridge:warning_fmt( "Ignoring improperly-encoded "
		"file element found in directory '~s': '~s'.", [ DirName, Bin ] ),

	% Then ignore:
	classify_dir_elements( DirName, T, Devices, Directories, Files, Symlinks,
						   OtherFiles, ImproperEncodingAction );


classify_dir_elements( DirName, _Elements=[ _Bin | T ], Devices, Directories,
		Files, Symlinks, OtherFiles, ImproperEncodingAction=ignore ) ->
	classify_dir_elements( DirName, T, Devices, Directories, Files, Symlinks,
						   OtherFiles, ImproperEncodingAction );


classify_dir_elements( DirName, _Elements=[ Bin | T ], Devices, Directories,
		Files, Symlinks, OtherFiles, ImproperEncodingAction=include ) ->
	% We cannot re-use the clause for plain string (Bin cannot be converted), we
	% have to remain in the world of binaries instead, so:
	%
	case get_type_of( filename:join( DirName, Bin ) ) of

		device ->
			classify_dir_elements( DirName, T, [ Bin | Devices ], Directories,
				Files, Symlinks, OtherFiles, ImproperEncodingAction );

		directory ->
			classify_dir_elements( DirName, T, Devices, [ Bin | Directories ],
				Files, Symlinks, OtherFiles, ImproperEncodingAction );

		regular ->
			classify_dir_elements( DirName, T, Devices, Directories,
				[ Bin | Files ], Symlinks, OtherFiles, ImproperEncodingAction );

		% Used to be managed as regular files:
		symlink ->
			classify_dir_elements( DirName, T, Devices, Directories, Files,
				[ Bin | Symlinks ], OtherFiles, ImproperEncodingAction );

		other ->
			classify_dir_elements( DirName, T, Devices, Directories,
				Files, Symlinks, [ Bin | OtherFiles ], ImproperEncodingAction )

	end.



% Regarding extensions: we could canonicalise their case, so that ".png" and
% ".PNG" are treated the same.


% Returns a list containing all elements of the Filenames list whose extension
% is the specified one (ex: ".dat").
%
-spec filter_by_extension( [ file_path() ], extension() ) -> [ file_path() ].
filter_by_extension( Filenames, Extension ) ->
	filter_by_extension( Filenames, Extension, _Acc=[] ).


filter_by_extension( _Filenames=[], _Extension, Acc ) ->
	Acc ;

filter_by_extension( _Filenames=[ H | T ], Extension, Acc ) ->

	case filename:extension( H ) of

		Extension ->
			filter_by_extension( T, Extension, [ H | Acc ] ) ;

		_Other ->
			filter_by_extension( T, Extension, Acc )

	end.



% Returns a list containing all elements of Filenames list whose extension
% corresponds to one of the specified extensions (ex: [".dat", ".png"]).
%
-spec filter_by_extensions( [ file_path() ], [ extension() ] ) ->
								  [ file_path() ].
filter_by_extensions( Filenames, Extensions ) ->
	filter_by_extensions( Filenames, Extensions, _Acc=[] ).


filter_by_extensions( _Filenames=[], _Extensions, Acc ) ->
	Acc ;

filter_by_extensions( _Filenames=[ F | T ], Extensions, Acc ) ->

	case lists:member( filename:extension( F ), Extensions ) of

		true ->
			filter_by_extensions( T, Extensions, [ F | Acc ] ) ;

		false ->
			filter_by_extensions( T, Extensions, Acc )

	end.



% Returns a list containing all paths in the specified list (in an unspecified
% order) that match any of the specified suffixes.
%
-spec filter_by_included_suffixes( [ any_path() ], [ any_suffix() ] ) ->
											[ any_path() ].
filter_by_included_suffixes( Paths, IncludedSuffixes ) ->
	% Not a list comprehension to better detect any non-matching element:
	Res = filter_by_included_suffixes( Paths, IncludedSuffixes, _Acc=[] ),
	%trace_utils:debug_fmt( "Filtering by included suffixes ~p~n  * input: ~p"
	%	"~n * output: ~p", [ IncludedSuffixes, Paths, Res ] ),
	Res.


% (helper)
filter_by_included_suffixes( _Paths=[], _IncludedSuffixes, Acc ) ->
	% Order does not matter:
	Acc;

filter_by_included_suffixes( _Paths=[ P | T ], IncludedSuffixes, Acc ) ->

	NewAcc = case has_matching_suffix( P, IncludedSuffixes ) of

		true ->
			[ P | Acc ];

		false ->
			Acc

	end,

	filter_by_included_suffixes( T, IncludedSuffixes, NewAcc ).




% Returns a list containing all paths in the specified list (in an unspecified
% order) that do not match any of the specified suffixes.
%
-spec filter_by_excluded_suffixes( [ any_path() ], [ any_suffix() ] ) ->
											[ any_path() ].
% Below there is at least one excluded suffix:
filter_by_excluded_suffixes( Paths, ExcludedSuffixes ) ->
	% Not a list comprehension to better detect any non-matching element:
	filter_by_excluded_suffixes( Paths, ExcludedSuffixes, _Acc=[] ).


% (helper)
filter_by_excluded_suffixes( _Paths=[], _ExcludedSuffixes, Acc ) ->
	% Order does not matter:
	Acc;

filter_by_excluded_suffixes( _Paths=[ P | T ], ExcludedSuffixes, Acc ) ->

	NewAcc = case has_matching_suffix( P, ExcludedSuffixes ) of

		true ->
			Acc;

		false ->
			[ P | Acc ]

	end,

	filter_by_excluded_suffixes( T, ExcludedSuffixes, NewAcc ).




% (exported helper)
-spec has_matching_suffix( any_path(), [ any_suffix() ] ) -> boolean().
has_matching_suffix( _Path, _Suffixes=[] ) ->
	false;

has_matching_suffix( Path, [ Suffix | T ] ) ->

	% Deadly bugs may happen if plain and binary strings are mixed:
	%cond_utils:assert( myriad_check_strings,
	%				   text_utils:are_of_same_string_type( Path, Suffix ) ),

	% Path and Suffix must be of the same type of strings (either plain or
	% binary). If not, as a conversion from binary to plain may fail (raw
	% filenames), we promote the plain to binary instead:
	%
	{ ActualPath, ActualSuffix } = case text_utils:is_string( Path ) of

		true ->
			case text_utils:is_string( Suffix ) of

				true ->
					{ Path, Suffix };

				false ->
					{ text_utils:string_to_binary( Path ), Suffix }

			end;

		false ->
			case text_utils:is_string( Suffix ) of

				true ->
					{ Path, text_utils:string_to_binary( Suffix ) };

				false ->
					{ Path , Suffix }

			end

	end,

	% To work both for plain and binary strings:
	LenPath = string:length( ActualPath ),
	LenSuffix = string:length( ActualSuffix ),

	case LenPath - LenSuffix  of

		StartPos when StartPos >= 0 ->

			case string:slice( ActualPath, StartPos ) of

				% Thus Suffix must be of the same type of string as Path:
				ActualSuffix ->
					true;

				_ ->
					% Not specifically ActualPath:
					has_matching_suffix( Path, T )

			end;

		_ ->
			% Not specifically ActualPath:
			has_matching_suffix( Path, T )

	end.




% Section dedicated to the look-up of files, with various variations (with or
% without extensions, with or without excluded directories, etc.)

% Excluded directories are all promoted to binary strings at first, so that no
% upcoming lists:member( D, ExcludedDirs ) can fail if ever D happens to be a
% binary (because of a 'raw directory').



% Returns a list of all files (regular ones and symlinks) found from the root,
% in the whole subtree (i.e. recursively).
%
% All extensions and suffixes accepted, no excluded directories. Elements whose
% name is improperly encoded are notified thanks to a warning trace, and then
% are ignored.
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_from( any_directory_path() ) -> [ file_path() ].
find_files_from( RootDir ) ->
	Res = find_files_from( RootDir, _IncludeSymlinks=true ),
	%trace_utils:debug_fmt( "All files found from '~ts':~n~p",
	%                       [ RootDir, Res ] ),
	Res.



% Returns a list of all regular files (hence not including symlinks) found from
% the root, in the whole subtree (i.e. recursively).
%
% All extensions and suffixes accepted, no excluded directories. Elements whose
% name is improperly encoded are notified thanks to a warning trace, and then
% are ignored.
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_regular_files_from( any_directory_path() ) -> [ file_path() ].
find_regular_files_from( RootDir ) ->
	find_files_from( RootDir, _IncludeSymlinks=false ).



% Returns a list of all files (regular ones and, if requested, symlinks) found
% from the root, in the whole subtree (i.e. recursively).
%
% All extensions and suffixes accepted, no excluded directories. Elements whose
% name is improperly encoded are notified thanks to a warning trace, and then
% are ignored.
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_from( any_directory_path(), boolean() ) -> [ file_path() ].
find_files_from( RootDir, IncludeSymlinks ) ->
	find_files_from( RootDir, IncludeSymlinks, _IfImproperEncoding=warn ).



% Returns a list of all files (regular ones and, if requested, symlinks) found
% from the root, in the whole subtree (i.e. recursively).
%
% All extensions and suffixes accepted, no excluded directories. Elements whose
% name is improperly encoded are managed according to the IfImproperEncoding
% parameter; if set to 'include', the return type of this function is the more
% general [any_file_path()], otherwise it is [file_path()].
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_from( any_directory_path(), boolean(),
					   improper_encoding_action() ) -> [ any_file_path() ].
find_files_from( RootDir, IncludeSymlinks, IfImproperEncoding ) ->
	Res = find_files_from( RootDir, _CurrentRelativeDir="", IncludeSymlinks,
						   IfImproperEncoding, _Acc=[] ),
	%trace_utils:debug_fmt( "Files found from '~ts':~n~p", [ RootDir, Res ] ),
	Res.



% (helper)
find_files_from( RootDir, CurrentRelativeDir, IncludeSymlinks,
				 IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "find_files_from with root = '~ts', "
	%    "current = '~ts'.", [ RootDir, CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	Acc ++ list_files_in_subdirs( Directories, RootDir, CurrentRelativeDir,
							IncludeSymlinks, IfImproperEncoding, _NextAcc=[] )
		++ prefix_files_with( CurrentRelativeDir, Files ).



% Specific helper for find_files_from/5 above:
list_files_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir,
					   _IncludeSymlinks, _IfImproperEncoding, Acc ) ->
	Acc;

list_files_in_subdirs( _Dirs=[ D | T ], RootDir, CurrentRelativeDir,
					   IncludeSymlinks, IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "list_files_in_subdirs with root = '~ts', "
	% "current = '~ts' and D='~ts'.", [ RootDir, CurrentRelativeDir, D ] ),

	NewAcc = find_files_from( RootDir, any_join( CurrentRelativeDir, D ),
					IncludeSymlinks, IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_files_in_subdirs( T, RootDir, CurrentRelativeDir, IncludeSymlinks,
						   IfImproperEncoding, NewAcc ).



% Returns a list of all symlinks found from the root, in the whole subtree
% (i.e. recursively).
%
% All extensions and suffixes accepted, no excluded directories. Elements whose
% name is improperly encoded are notified thanks to a warning trace, and then
% are ignored.
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_links_from( any_directory_path() ) -> [ file_path() ].
find_links_from( RootDir ) ->
	find_links_from( RootDir, _IfImproperEncoding=warn ).



% Returns a list of all symlinks found from the root, in the whole subtree
% (i.e. recursively).
%
% All extensions and suffixes accepted, no excluded directories. Elements whose
% name is improperly encoded are managed according to the IfImproperEncoding
% parameter; if set to 'include', the return type of this function is the more
% general [any_file_path()], otherwise it is [file_path()].
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_links_from( any_directory_path(), improper_encoding_action() ) ->
								[ file_path() ].
find_links_from( RootDir, IfImproperEncoding ) ->
	find_links_from( RootDir, _CurrentRelativeDir="", IfImproperEncoding,
					 _Acc=[] ).


% (helper)
find_links_from( RootDir, CurrentRelativeDir, IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "find_links_from with root = '~ts', "
	%  "current = '~ts'.", [ RootDir, CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ _RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Acc ++ list_links_in_subdirs( Directories, RootDir, CurrentRelativeDir,
								  IfImproperEncoding, _NextAcc=[] )
		++ prefix_files_with( CurrentRelativeDir, Symlinks ).



% Specific helper for find_links_from/4 above:
list_links_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir,
					   _IfImproperEncoding, Acc ) ->
	Acc;

list_links_in_subdirs( _Dirs=[ D | T ], RootDir, CurrentRelativeDir,
					   IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "list_links_in_subdirs with root = '~ts', "
	%   "current = '~ts' and D='~ts'.", [ RootDir, CurrentRelativeDir, D ] ),

	NewAcc = find_links_from( RootDir, any_join( CurrentRelativeDir, D ),
							  IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_links_in_subdirs( T, RootDir, CurrentRelativeDir, IfImproperEncoding,
						   NewAcc ).



% Returns a list of all files (regular ones and symlinks) found from the root
% with specified extension, in the whole subtree (i.e. recursively).
%
% All suffixes accepted, no excluded directories. Elements whose name is
% improperly encoded are notified thanks to a warning trace, and then are
% ignored.
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_extension_from( any_directory_path(), extension() ) ->
											[ file_path() ].
find_files_with_extension_from( RootDir, Extension ) ->
	find_files_with_extension_from( RootDir, Extension,
									_IfImproperEncoding=warn ).


% Returns a list of all files (regular ones and symlinks) found from the root
% with specified extension, in the whole subtree (i.e. recursively).
%
% All suffixes accepted, no excluded directories. Elements whose name is
% improperly encoded are managed according to the IfImproperEncoding parameter;
% if set to 'include', the return type of this function is the more general
% [any_file_path()], otherwise it is [file_path()].
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_extension_from( any_directory_path(), extension(),
							improper_encoding_action() ) -> [ file_path() ].
find_files_with_extension_from( RootDir, Extension, IfImproperEncoding ) ->
	find_files_with_extension_from( RootDir, Extension, _IncludeSymlinks=true,
									IfImproperEncoding ).



% Returns a list of all files (regular ones and, if requested, symlinks) found
% from the root with specified extension, in the whole subtree
% (i.e. recursively).
%
% All suffixes accepted, no excluded directories. Elements whose name is
% improperly encoded are managed according to the IfImproperEncoding parameter;
% if set to 'include', the return type of this function is the more general
% [any_file_path()], otherwise it is [file_path()].
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_extension_from( any_directory_path(), extension(),
			boolean(), improper_encoding_action() ) -> [ file_path() ].
find_files_with_extension_from( RootDir, Extension, IncludeSymlinks,
								IfImproperEncoding ) ->
	find_files_with_extension_from( RootDir, _CurrentRelativeDir="",
			Extension, IncludeSymlinks, IfImproperEncoding, _Acc=[] ).


% (helper)
find_files_with_extension_from( RootDir, CurrentRelativeDir, Extension,
								IncludeSymlinks, IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "find_files_with_extension_from in '~ts'.",
	%           [ CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	Acc ++ list_files_in_subdirs_with_extension( Directories, Extension,
			RootDir, CurrentRelativeDir, IncludeSymlinks, IfImproperEncoding,
			_NextAcc=[] )
		++ prefix_files_with( CurrentRelativeDir,
							  filter_by_extension( Files, Extension ) ).



% Helper for find_files_with_extension_from/6:
list_files_in_subdirs_with_extension( _Dirs=[], _Extension, _RootDir,
		_CurrentRelativeDir, _IncludeSymlinks, _IfImproperEncoding, Acc ) ->
	Acc;

list_files_in_subdirs_with_extension( _Dirs=[ H | T ], Extension, RootDir,
		CurrentRelativeDir, IncludeSymlinks, IfImproperEncoding, Acc ) ->

	NewAcc = find_files_with_extension_from( RootDir,
		any_join( CurrentRelativeDir, H ), Extension, IncludeSymlinks,
		IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_files_in_subdirs_with_extension( T, Extension, RootDir,
		CurrentRelativeDir, IncludeSymlinks, IfImproperEncoding, NewAcc ).



% Returns a list of all files (regular ones and symlinks) found from the root,
% in the whole subtree (i.e. recursively), with specified directories excluded.
%
% Note that an excluded directory can be specified as a full (relative) path
% (ex: "foo/bar/not-wanted"), or just as a final directory name (ex:
% "my-excluded-name"). In the latter case, all directories bearing that name
% (ex: "foo/bar/any/my-excluded-name") will be excluded as well.
%
% Thus when a directory D is specified in the excluded list, each traversed
% directory T will be compared twice to D: T will be matched against D, and
% against filename:basename(T), i.e. its final name, as well. As soon as one
% matches, T will be excluded.
%
% All extensions and suffixes accepted. Elements whose name is improperly
% encoded are notified thanks to a warning trace, and then are ignored.
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_excluded_dirs( any_directory_path(),
									 [ directory_path() ] ) -> [ file_path() ].
find_files_with_excluded_dirs( RootDir, ExcludedDirs ) ->
	find_files_with_excluded_dirs( RootDir, ExcludedDirs,
								   _IncludeSymlinks=true ).



% Returns a list of all files (regular ones and, if requested, symlinks) found
% from the root, in the whole subtree (i.e. recursively), with specified
% directories excluded.
%
% Note that an excluded directory can be specified as a full (relative) path
% (ex: "foo/bar/not-wanted"), or just as a final directory name (ex:
% "my-excluded-name"). In the latter case, all directories bearing that name
% (ex: "foo/bar/any/my-excluded-name") will be excluded as well.
%
% Thus when a directory D is specified in the excluded list, each traversed
% directory T will be compared twice to D: T will be matched against D, and
% against filename:basename(T), i.e. its final name, as well. As soon as one
% matches, T will be excluded.
%
% All extensions and suffixes accepted. Elements whose name is improperly
% encoded are notified thanks to a warning trace, and then are ignored.
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_excluded_dirs( any_directory_path(), [ directory_path() ],
									 boolean() ) -> [ file_path() ].
find_files_with_excluded_dirs( RootDir, ExcludedDirs, IncludeSymlinks ) ->
	find_files_with_excluded_dirs( RootDir, ExcludedDirs, IncludeSymlinks,
								   _IfImproperEncoding=warn ).


% Returns a list of all files (regular ones and, if requested, symlinks) found
% from the root, in the whole subtree (i.e. recursively), with specified
% directories excluded.
%
% Note that an excluded directory can be specified as a full (relative) path
% (ex: "foo/bar/not-wanted"), or just as a final directory name (ex:
% "my-excluded-name"). In the latter case, all directories bearing that name
% (ex: "foo/bar/any/my-excluded-name") will be excluded as well.
%
% Thus when a directory D is specified in the excluded list, each traversed
% directory T will be compared twice to D: T will be matched against D, and
% against filename:basename(T), i.e. its final name, as well. As soon as one
% matches, T will be excluded.
%
% All extensions and suffixes accepted. Elements whose name is improperly
% encoded are managed according to the IfImproperEncoding parameter; if set to
% 'include', the return type of this function is the more general
% [any_file_path()], otherwise it is [file_path()].
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_excluded_dirs( any_directory_path(), [ directory_path() ],
			boolean(), improper_encoding_action() ) -> [ file_path() ].
find_files_with_excluded_dirs( RootDir, ExcludedDirs, IncludeSymlinks,
							   IfImproperEncoding ) ->

	% Not wanting a lists:member/1 to fail because of a wrong string type:
	BinExcludedDirs = text_utils:ensure_binaries( ExcludedDirs ),

	find_files_with_excluded_dirs( RootDir, _CurrentRelativeDir="",
		BinExcludedDirs, IncludeSymlinks, IfImproperEncoding, _Acc=[] ).


% (helper)
find_files_with_excluded_dirs( RootDir, CurrentRelativeDir, BinExcludedDirs,
							   IncludeSymlinks, IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "find_files_with_excluded_dirs in '~ts'.",
	%       [ CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	% If for example ExcludedDirs=[".svn"], we want to eliminate not only
	% ".svn" but also all "foo/bar/.svn", i.e. all directories having the same
	% (last) name:
	%
	FilteredDirectories = [ D || D <- Directories,
		not ( lists:member( bin_join( CurrentRelativeDir, D ), BinExcludedDirs )
			  orelse lists:member( text_utils:ensure_binary( D ),
								   BinExcludedDirs ) ) ],

	Acc ++ list_files_in_subdirs_excluded_dirs( FilteredDirectories, RootDir,
				CurrentRelativeDir, BinExcludedDirs, IncludeSymlinks,
				IfImproperEncoding, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir, Files ).



% Specific helper for find_files_with_excluded_dirs/6 above:
list_files_in_subdirs_excluded_dirs( _Dirs=[], _RootDir,
		_CurrentRelativeDir, _BinExcludedDirs, _IncludeSymlinks,
		_IfImproperEncoding, Acc ) ->
	Acc;

list_files_in_subdirs_excluded_dirs( _Dirs=[ D | T ], RootDir,
		CurrentRelativeDir, BinExcludedDirs, IncludeSymlinks,
		IfImproperEncoding, Acc ) ->

	NewAcc = find_files_with_excluded_dirs( RootDir,
		any_join( CurrentRelativeDir, D ), BinExcludedDirs, IncludeSymlinks,
		IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_files_in_subdirs_excluded_dirs( T, RootDir, CurrentRelativeDir,
		BinExcludedDirs, IncludeSymlinks, IfImproperEncoding, NewAcc ).





% Returns a list of all files (regular ones and symlinks) found from the root
% which do not match any of the specified suffixes, in the whole subtree
% (i.e. recursively).
%
% No excluded directories. Elements whose name is improperly encoded are
% notified thanks to a warning trace, and then are ignored.
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_excluded_suffixes( any_directory_path(),
										 [ any_suffix() ] ) -> [ file_path() ].
find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes ) ->
	find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes,
									   _IfImproperEncoding=warn ).


% Returns a list of all files (regular ones and symlinks) found from the root
% which do not match any of the specified suffixes, in the whole subtree
% (i.e. recursively).
%
% No excluded directories. Elements whose name is
% improperly encoded are managed according to the IfImproperEncoding parameter;
% if set to 'include', the return type of this function is the more general
% [any_file_path()], otherwise it is [file_path()].
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_excluded_suffixes( any_directory_path(),
			[ any_suffix() ], improper_encoding_action() ) -> [ file_path() ].
find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes,
								   IfImproperEncoding  ) ->
	find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes,
								_IncludeSymlinks=true, IfImproperEncoding ).



% Returns a list of all files (regular ones and, if requested, symlinks) found
% from the root which do not match any of the specified suffixes, in the whole
% subtree (i.e. recursively).
%
% No excluded directories. Elements whose name is
% improperly encoded are managed according to the IfImproperEncoding parameter;
% if set to 'include', the return type of this function is the more general
% [any_file_path()], otherwise it is [file_path()].
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_excluded_suffixes( any_directory_path(), [ any_suffix() ],
					boolean(), improper_encoding_action() ) -> [ file_path() ].
find_files_with_excluded_suffixes( RootDir, ExcludedSuffixes, IncludeSymlinks,
								   IfImproperEncoding ) ->
	find_files_with_excluded_suffixes( RootDir, _CurrentRelativeDir="",
		ExcludedSuffixes, IncludeSymlinks, IfImproperEncoding, _Acc=[] ).



% (helper)
find_files_with_excluded_suffixes( RootDir, CurrentRelativeDir,
			ExcludedSuffixes, IncludeSymlinks, IfImproperEncoding, Acc ) ->

	%trace_utils:debug_fmt( "find_files_with_excluded_suffixes in '~ts'.",
	%     [ CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	Acc ++ list_files_in_subdirs_with_excluded_suffixes( Directories,
			ExcludedSuffixes, RootDir, CurrentRelativeDir, IncludeSymlinks,
			IfImproperEncoding, _NextAcc=[] )
		++ prefix_files_with( CurrentRelativeDir,
					filter_by_excluded_suffixes( Files, ExcludedSuffixes ) ).




% Helper for find_files_with_excluded_suffixes/6:
-spec list_files_in_subdirs_with_excluded_suffixes( [ directory_name() ],
		[ any_suffix() ], directory_path(), directory_path(), boolean(),
		improper_encoding_action(), [ file_path() ] ) -> [ file_path() ].
list_files_in_subdirs_with_excluded_suffixes( _Dirs=[], _ExcludedSuffixes,
		_RootDir, _CurrentRelativeDir, _IncludeSymlinks, _IfImproperEncoding,
		Acc ) ->
	Acc;

list_files_in_subdirs_with_excluded_suffixes( _Dirs=[ D | T ], ExcludedSuffixes,
		RootDir, CurrentRelativeDir, IncludeSymlinks, IfImproperEncoding,
		Acc ) ->

	NewAcc = find_files_with_excluded_suffixes( RootDir,
		any_join( CurrentRelativeDir, D ), ExcludedSuffixes, IncludeSymlinks,
		IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_files_in_subdirs_with_excluded_suffixes( T, ExcludedSuffixes, RootDir,
		CurrentRelativeDir, IncludeSymlinks, IfImproperEncoding, NewAcc ).




% Returns a list of all files (regular ones and symlinks) found from the root,
% in the whole subtree (i.e. recursively), with specified directories and
% suffixes excluded.
%
% Note that an excluded directory can be specified as a full (relative) path
% (ex: "foo/bar/not-wanted"), or just as a final directory name (ex:
% "my-excluded-name"). In the latter case, all directories bearing that name
% (ex: "foo/bar/any/my-excluded-name") will be excluded as well.
%
% Thus when a directory D is specified in the excluded list, each traversed
% directory T will be compared twice to D: T will be matched against D, and
% against filename:basename(T), i.e. its final name, as well. As soon as one
% matches, T will be excluded.
%
% Elements whose name is improperly encoded are notified thanks to a warning
% trace, and then are ignored.
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_excluded_dirs_and_suffixes( any_directory_path(),
		[ directory_path() ], [ any_suffix() ] ) -> [ file_path() ].
find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirs,
											ExcludedSuffixes ) ->
	find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirs,
								ExcludedSuffixes, _IncludeSymlinks=true ).



% Returns a list of all files (regular ones and, if requested, symlinks) found
% from the root, in the whole subtree (i.e. recursively), with specified
% directories and suffixes excluded.
%
% Note that an excluded directory can be specified as a full (relative) path
% (ex: "foo/bar/not-wanted"), or just as a final directory name (ex:
% "my-excluded-name"). In the latter case, all directories bearing that name
% (ex: "foo/bar/any/my-excluded-name") will be excluded as well.
%
% Thus when a directory D is specified in the excluded list, each traversed
% directory T will be compared twice to D: T will be matched against D, and
% against filename:basename(T), i.e. its final name, as well. As soon as one
% matches, T will be excluded.
%
% Elements whose name is improperly encoded are notified thanks to a warning
% trace, and then are ignored.
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_excluded_dirs_and_suffixes( any_directory_path(),
		[ directory_path() ], [ any_suffix() ], boolean() ) -> [ file_path() ].
find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirs,
									ExcludedSuffixes, IncludeSymlinks ) ->
	find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirs,
		ExcludedSuffixes, IncludeSymlinks, _IfImproperEncoding=warn ).


% Returns a list of all files (regular ones and, if requested, symlinks) found
% from the root, in the whole subtree (i.e. recursively), with specified
% directories and suffixes excluded.
%
% Note that an excluded directory can be specified as a full (relative) path
% (ex: "foo/bar/not-wanted"), or just as a final directory name (ex:
% "my-excluded-name"). In the latter case, all directories bearing that name
% (ex: "foo/bar/any/my-excluded-name") will be excluded as well.
%
% Thus when a directory D is specified in the excluded list, each traversed
% directory T will be compared twice to D: T will be matched against D, and
% against filename:basename(T), i.e. its final name, as well. As soon as one
% matches, T will be excluded.
%
% Elements whose name is improperly encoded are managed according to the
% IfImproperEncoding parameter; if set to 'include', the return type of this
% function is the more general [any_file_path()], otherwise it is [file_path()].
%
% All returned pathnames are relative to this root.
% Ex: ["./a.txt", "./tmp/b.txt"].
%
-spec find_files_with_excluded_dirs_and_suffixes( any_directory_path(),
			[ directory_path() ], [ any_suffix() ], boolean(),
			improper_encoding_action() ) -> [ file_path() ].
find_files_with_excluded_dirs_and_suffixes( RootDir, ExcludedDirs,
					ExcludedSuffixes, IncludeSymlinks, IfImproperEncoding ) ->

	%trace_utils:debug_fmt( "find_files_with_excluded_dirs_and_suffixes: from "
	%	"'~ts': RootDir = '~ts', ExcludedDirs = ~p, ExcludedSuffixes = ~p",
	%	[ get_current_directory(), RootDir, ExcludedDirs,
	%	  ExcludedSuffixes ] ),

	% Not wanting a lists:member/1 to fail because of a wrong string type:
	BinExcludedDirs = text_utils:ensure_binaries( ExcludedDirs ),

	find_files_with_excluded_dirs_and_suffixes( RootDir,
			_CurrentRelativeDir="", BinExcludedDirs, ExcludedSuffixes,
			IncludeSymlinks, IfImproperEncoding, _Acc=[] ).



% (helper)
find_files_with_excluded_dirs_and_suffixes( RootDir, CurrentRelativeDir,
		ExcludedDirs, ExcludedSuffixes, IncludeSymlinks, IfImproperEncoding,
		Acc ) ->

	%trace_utils:debug_fmt( "find_files_with_excluded_dirs_and_suffixes in "
	%   "~ts / ~ts.", [ RootDir, CurrentRelativeDir ] ),

	CurrentDir = any_join( RootDir, CurrentRelativeDir ),

	{ RegularFiles, Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( CurrentDir, IfImproperEncoding ),

	Files = case IncludeSymlinks of

		true ->
			RegularFiles ++ Symlinks;

		false ->
			RegularFiles

	end,

	% If for example ExcludedDirs=[".svn"], we want to eliminate not only
	% ".svn" but also all "foo/bar/.svn", i.e. all directories having the same
	% (last) name:
	%
	FilteredDirectories = [ D || D <- Directories,
		not ( lists:member( bin_join( CurrentRelativeDir, D ), ExcludedDirs )
			  orelse lists:member( text_utils:ensure_binary( D ),
								   ExcludedDirs ) ) ],

	Acc ++ list_files_in_subdirs_excluded_dirs_and_suffixes(
			FilteredDirectories, RootDir, CurrentRelativeDir,
			ExcludedDirs, ExcludedSuffixes, IncludeSymlinks, IfImproperEncoding,
			_Acc=[] )
		++ prefix_files_with( CurrentRelativeDir,
				filter_by_excluded_suffixes( Files, ExcludedSuffixes ) ).




% Specific helper for find_files_with_excluded_dirs_and_suffixes/7 above:
list_files_in_subdirs_excluded_dirs_and_suffixes( _Dirs=[], _RootDir,
		_CurrentRelativeDir, _ExcludedDirs, _ExcludedSuffixes,
		_IncludeSymlinks, _IfImproperEncoding, Acc ) ->
	Acc;

list_files_in_subdirs_excluded_dirs_and_suffixes( _Dirs=[ D | T ], RootDir,
		CurrentRelativeDir, ExcludedDirs, ExcludedSuffixes, IncludeSymlinks,
		IfImproperEncoding, Acc ) ->

	NewAcc = find_files_with_excluded_dirs_and_suffixes( RootDir,
			any_join( CurrentRelativeDir, D ), ExcludedDirs, ExcludedSuffixes,
			IncludeSymlinks, IfImproperEncoding, _NextAcc=[] ) ++ Acc,

	list_files_in_subdirs_excluded_dirs_and_suffixes( T, RootDir,
		CurrentRelativeDir, ExcludedDirs, ExcludedSuffixes, IncludeSymlinks,
		IfImproperEncoding, NewAcc ).



% Prefixes specified paths with specified root directory.
-spec prefix_files_with( directory_path(), [ file_name() ] ) -> [ file_path() ].
prefix_files_with( RootDir, Files ) ->
	%trace_utils:debug_fmt( "Prefixing ~p with '~ts'.", [ Files, RootDir ] ),
	prefix_files_with( RootDir, Files, _Acc=[] ).


% (helper)
prefix_files_with( _RootDir, _Files=[], Acc ) ->
	Acc;

prefix_files_with( RootDir, [ Str | T ], Acc ) when is_list( Str ) ->
	prefix_files_with( RootDir, T, [ any_join( RootDir, Str ) | Acc ] );

% Trying to overcome weirdly-named files:
prefix_files_with( RootDir, [ BinStr | T ], Acc ) when is_binary( BinStr ) ->
	prefix_files_with( RootDir, T, [ any_join( RootDir, BinStr ) | Acc ] ).



% Returns a list of all directories found from the root, in the whole subtree
% (i.e. recursively).
%
% All returned pathnames are relative to this root.
% Ex: ["./my-dir", "./tmp/other-dir"].
%
-spec find_directories_from( any_directory_name() ) -> [ directory_name() ].
find_directories_from( RootDir ) ->
	find_directories_from( RootDir, "", _Acc=[] ).


% (helper)
find_directories_from( RootDir, CurrentRelativeDir, Acc ) ->

	%trace_utils:debug_fmt( "find_directories_from in ~ts.",
	%                      [ CurrentRelativeDir ] ),

	{ _RegularFiles, _Symlinks, Directories, _OtherFiles, _Devices } =
		list_dir_elements( any_join( RootDir, CurrentRelativeDir ) ),

	Acc ++ list_directories_in_subdirs( Directories, RootDir,
										CurrentRelativeDir, _Acc=[] )
		++ prefix_files_with( CurrentRelativeDir, Directories ).



% (helper)
list_directories_in_subdirs( _Dirs=[], _RootDir, _CurrentRelativeDir, Acc ) ->
	Acc;

list_directories_in_subdirs( _Dirs=[ H | T ], RootDir, CurrentRelativeDir,
							 Acc ) ->
	list_directories_in_subdirs( T, RootDir, CurrentRelativeDir,
		find_directories_from( RootDir, any_join( CurrentRelativeDir, H ),
							   _Acc=[] )
		++ Acc ).



% Creates specified directory ("mkdir"), without creating any intermediate
% (parent) directory that would not exist.
%
% Throws an exception if the operation failed.
%
-spec create_directory( directory_name() ) -> void().
create_directory( DirName ) ->
	create_directory( DirName, create_no_parent ).



% Creates the specified directory.
%
% If 'create_no_parent' is specified, no intermediate (parent) directory will be
% created.
%
% If 'create_parents' is specified, any non-existing intermediate (parent)
% directory will be created.
%
% Throws an exception if the operation fails, for example if the directory is
% already existing ({create_directory_failed, "foobar", eexist}).
%
-spec create_directory( directory_name(), parent_creation() ) -> void().
create_directory( DirName, create_no_parent ) ->

	case file:make_dir( DirName ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { create_directory_failed, DirName, Reason } )

	end;

create_directory( DirName, create_parents ) ->
	create_dir_elem( filename:split( DirName ), "" ).



% (helper)
create_dir_elem( _Elems=[], _Prefix ) ->
	ok;

create_dir_elem( _Elems=[ H | T ], Prefix ) ->

	NewPrefix = join( Prefix, H ),

	case exists( NewPrefix ) of

		true ->
			ok ;

		false ->
			create_directory( NewPrefix, create_no_parent )

	end,
	create_dir_elem( T, NewPrefix ).



% Creates specified directory (but not any parent thereof), if not already
% existing.
%
% Throws an exception if the operation fails.
%
-spec create_directory_if_not_existing( directory_name() ) -> void().
create_directory_if_not_existing( DirName ) ->
	create_directory_if_not_existing( DirName, create_no_parent ).


% Creates specified directory (and, if specified, any needed parent as well), if
% not already existing.
%
% Throws an exception if the operation fails.
%
-spec create_directory_if_not_existing( directory_name(),
										parent_creation() ) -> void().
create_directory_if_not_existing( DirName, ParentCreation ) ->

	case is_existing_directory( DirName ) of

		true ->
			ok;

		false ->
			create_directory( DirName, ParentCreation )

	end.



% Creates a non-previously existing temporary directory, and returns its full
% path.
%
% See also: system_utils:get_default_temporary_directory/0
%
-spec create_temporary_directory() -> directory_name().
create_temporary_directory() ->

	TmpDir = join( [ system_utils:get_default_temporary_directory(),
					 system_utils:get_user_name(), id_utils:generate_uuid() ] ),

	case exists( TmpDir ) of

		true ->
			% Very bad luck apparently, or same random root:
			create_temporary_directory();

		false ->
			create_directory( TmpDir, create_parents ),
			TmpDir

	end.



% Removes (deletes) specified file, specified as any kind of string.
%
% Throws an exception if any problem occurs.
%
-spec remove_file( any_file_path() ) -> void().
remove_file( Filename ) ->

	%trace_utils:warning_fmt( "Removing file '~ts'.", [ Filename ] ),

	case file:delete( Filename ) of
	%case ok of

		ok ->
			ok;

		Error ->
			throw( { remove_file_failed, Filename, Error } )

	end.



% Removes (deletes) specified files, specified as a list of any kind of strings.
-spec remove_files( [ any_file_path() ] ) -> void().
remove_files( FilenameList ) ->

	%trace_utils:warning_fmt( "Removing following files: ~ts",
	%						 [ text_utils:strings_to_string( FilenameList ) ] ),

	[ remove_file( Filename ) || Filename <- FilenameList ].



% Removes specified file, specified as any kind of string, iff it is already
% existing, otherwise does nothing.
%
-spec remove_file_if_existing( any_file_path() ) -> void().
remove_file_if_existing( Filename ) ->

	case is_existing_file( Filename ) of

		true ->
			%trace_bridge:debug_fmt( "Removing existing file '~ts'.",
			%						[ Filename ] ),
			remove_file( Filename );

		false ->
			%trace_bridge:debug_fmt( "No existing file '~ts' to remove.",
			%						[ Filename ] ),
			ok

	end.



% Removes each specified file, in specified list of any kind of strings, iff it
% is already existing.
%
-spec remove_files_if_existing( [ any_file_path() ] ) -> void().
remove_files_if_existing( FilenameList ) ->
	[ remove_file_if_existing( Filename ) || Filename <- FilenameList ].



% Removes specified directory, which must be empty (so: behaves mostly like
% the 'rmdir' shell command).
%
-spec remove_empty_directory( any_directory_path() ) -> void().
remove_empty_directory( DirectoryPath ) ->

	%trace_utils:warning_fmt( "## Removing empty directory '~ts'.",
	%                         [ DirectoryPath ] ),

	case file:del_dir( DirectoryPath ) of

		ok ->
			ok;

		{ error, Reason } ->
			% Probably not so empty:
			throw( { remove_empty_directory_failed, Reason, DirectoryPath } )

	end.



% Removes all (supposedly) empty directories pertaining to the specified local,
% relative directory path, i.e. this path (ex: a/b/c) and all its ancestors
% (hence a/b and a are - if empty - removed as well, and none of their possible
% siblings of course); so behaves mostly like the 'rmdir --parents' shell
% command.
%
% Note: does not remove an (empty) tree, just a given directory and its local
% ancestors.
%
-spec remove_empty_path( any_directory_path() ) -> void().
remove_empty_path( DirectoryPath ) ->

	%trace_utils:warning_fmt( "## Removing empty directory '~ts'.",
	%                         [ DirectoryPath ] ),

	remove_empty_path_helper( DirectoryPath ).


% (helper)
remove_empty_path_helper( _DirectoryPath="." ) ->
	ok;

remove_empty_path_helper( DirectoryPath ) ->
	remove_empty_directory( DirectoryPath ),
	remove_empty_path_helper( filename:dirname( DirectoryPath ) ).



% Removes all (supposedly) empty directories found from specified directory,
% expected to be the root of a tree that contains only (possibly nested)
% directories (and no other kind of filesystem entry).
%
-spec remove_empty_tree( any_directory_path() ) -> void().
remove_empty_tree( DirectoryPath ) ->

	%trace_utils:warning_fmt( "## Removing empty tree '~ts'.",
	%						 [ DirectoryPath ] ),

	% For clarity:
	case is_existing_directory( DirectoryPath ) of

		true ->
			ok;

		false ->
			throw( { directory_not_found, DirectoryPath } )

	end,

	{ RegularFiles, Symlinks, Directories, OtherFiles, Devices } =
		list_dir_elements( DirectoryPath ),

	case RegularFiles of

		[] ->
			ok;

		_ ->
			throw( { regular_files_found, RegularFiles } )

	end,

	case Symlinks of

		[] ->
			ok;

		_ ->
			throw( { symbolic_links_found, Symlinks } )

	end,

	case OtherFiles of

		[] ->
			ok;

		_ ->
			throw( { other_files_found, OtherFiles } )

	end,

	case Devices of

		[] ->
			ok;

		_ ->
			throw( { devices_found, Devices } )

	end,

	[ remove_empty_tree( join( DirectoryPath, D ) ) || D <- Directories ],

	% Now an empty directory, so:
	remove_directory( DirectoryPath ).



% Removes specified (possibly non-empty) directory as a whole, recursively (so:
% behaves mostly like the 'rm -rf ' shell command; of course to use with care).
%
% Note that if any unusual file entry is found in the tree (ex: device or file
% that is neither regular nor a symbolic link), the operation will stop on error
% (whereas elements may already have been removed).
%
-spec remove_directory( any_directory_name() ) -> void().
remove_directory( DirectoryName ) ->

	%trace_utils:warning_fmt( "## Removing recursively directory '~ts'.",
	%                         [ DirectoryName ] ),

	% We do it programmatically, rather than running a command like '/bin/rm -rf
	% ...':

	% All local elements:
	{ RegularFiles, Symlinks, Directories, OtherFiles, Devices } =
		list_dir_elements( DirectoryName ),

	case Devices of

		[] ->
			ok;

		_ ->
			trace_utils:error_fmt( "Interrupting removal of directory '~ts', "
				"as device entries have been found: ~p.", [ Devices ] ),

			throw( { device_entries_found, Devices } )

	end,

	case OtherFiles of

		[] ->
			ok;

		_ ->
			trace_utils:error_fmt( "Interrupting removal of directory '~ts', "
				"as unexpected filesystem entries have been found: ~p.",
				[ OtherFiles ] ),

			throw( { unexpected_entries_found, OtherFiles } )

	end,

	% Depth-first of course:
	[ remove_directory( join( DirectoryName, SubDir ) )
	  || SubDir <- Directories ],

	% Then removing all local regular files and symlinks:
	[ remove_file( join( DirectoryName, F ) )
	  || F <- Symlinks ++ RegularFiles ],

	% Finally removing this (now empty) directory as well:
	remove_empty_directory( DirectoryName ).



% Copies a specified file to a given destination filename (not a directory name,
% see copy_file_in/2 for that), overwriting any previous file.
%
% Note: content is copied and permissions are preserved (ex: the copy of an
% executable file will be itself executable, other permissions as well, unlike
% /bin/cp which relies on umask).
%
-spec copy_file( file_name(), file_name() ) -> void().
copy_file( SourceFilename, DestinationFilename ) ->

	case try_copy_file( SourceFilename, DestinationFilename ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { copy_file_failed, SourceFilename, Reason } )

	end.



% Copies a specified file to a given destination filename (not a directory name,
% see copy_file_in/2 for that), overwriting any previous file.
%
% Note: content is copied and permissions are preserved (ex: the copy of an
% executable file will be itself executable, other permissions as well, unlike
% /bin/cp that relies on umask).
%
-spec try_copy_file( file_name(), file_name() ) -> basic_utils:base_status().
try_copy_file( SourceFilename, DestinationFilename ) ->

	% First, checks the source file exists and retrieves its meta-information:
	case file:read_file_info( SourceFilename ) of

		{ ok, #file_info{ mode=Mode } } ->

			case file:copy( SourceFilename, DestinationFilename ) of

				{ ok, _ByteCount } ->
					% Now sets the permissions of the copy:
					case file:change_mode( DestinationFilename, Mode ) of

						ok ->
							ok;

						ChgModeError ->
							ChgModeError

					end;

				CopyError ->
					CopyError

			end;

		ReadError ->
			ReadError

	end.



% Copies a specified file in a given destination directory, overwriting any
% previous file, and returning the full path of the copied file.
%
% Note: content is copied and permissions are preserved (ex: the copy of an
% executable file will be itself executable, other permissions as well, unlike
% /bin/cp which relies on umask).
%
-spec copy_file_in( file_name(), directory_name() ) -> file_name().
copy_file_in( SourceFilename, DestinationDirectory ) ->

	Filename = filename:basename( SourceFilename ),

	TargetPath = join( DestinationDirectory, Filename ),

	copy_file( SourceFilename, TargetPath ),

	TargetPath.



% Copies a specified file to a given destination iff this source file is already
% existing.
%
% Note: content is copied and permissions are preserved (ex: the copy of an
% executable file will be itself executable).
%
-spec copy_file_if_existing( file_name(), file_name() ) -> void().
copy_file_if_existing( SourceFilename, DestinationFilename ) ->

	case is_existing_file( SourceFilename ) of

		true ->
			copy_file( SourceFilename, DestinationFilename );

		false ->
			ok

	end.



% Copies specified source tree in specified target directory.
-spec copy_tree( directory_path(), directory_path() ) -> void().
copy_tree( SourceTreePath, TargetDirectory ) ->

	case file_utils:is_existing_directory_or_link( SourceTreePath ) of

		true ->
			ok;

		false ->
			throw( { non_existing_source_tree, SourceTreePath } )

	end,

	case file_utils:is_existing_directory_or_link( TargetDirectory ) of

		true ->
			ok;

		false ->
			throw( { non_existing_target_directory, TargetDirectory } )

	end,

	Cmd = text_utils:format( "/bin/cp -r '~ts' '~ts'",
							 [ SourceTreePath, TargetDirectory ] ),

	case system_utils:run_command( Cmd ) of

		{ _ExitCode=0, _Output=[] } ->
			ok;

		{ ExitCode, ErrorOutput } ->
			throw( { copy_tree_failed, { SourceTreePath, TargetDirectory },
					 ExitCode, ErrorOutput } )

	end.


% Renames specified file.
%
% Returns, for convenience, the new name.
%
-spec rename( file_name(), file_name() ) -> file_name().
rename( SourceFilename, DestinationFilename ) ->
	move_file( SourceFilename, DestinationFilename ).



% Moves specified file so that it is now designated by specified filename.
%
% Returns, for convenience, the new name.
%
-spec move_file( file_name(), file_name() ) -> file_name().
move_file( SourceFilename, DestinationFilename ) ->

	%trace_utils:warning_fmt( "## Moving file '~ts' to '~ts'.",
	%						  [ SourceFilename, DestinationFilename ] ),

	%copy_file( SourceFilename, DestinationFilename ),
	%remove_file( SourceFilename ).

	% Simpler, better, yet does not works across filesystems:
	case file:rename( SourceFilename, DestinationFilename ) of

		ok ->
			DestinationFilename;

		{ error, exdev } ->
			%trace_utils:info_fmt( "Moving across filesystems '~ts' to '~ts'.",
			%					   [ SourceFilename, DestinationFilename ] ),
			copy_file( SourceFilename, DestinationFilename ),
			remove_file( SourceFilename );

		Error ->
			throw( { move_file_failed, Error, SourceFilename,
					 DestinationFilename } )

	end.



% Creates a symbolic link pointing to specified target path, bearing specified
% (link) name.
%
-spec create_link( path(), link_name() ) -> void().
create_link( TargetPath, LinkName ) ->

	%trace_utils:debug_fmt( "Creating a link '~ts' to '~ts', while in '~ts'.",
	%					   [ LinkName, TargetPath, get_current_directory() ] ),

	case file:make_symlink( TargetPath, LinkName ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { link_creation_failed, { target, TargetPath },
					 { link, LinkName }, Reason } )

	end.



% Returns a path deriving from specified one so that it is unique, i.e. that it
% does not clash with any pre-existing entry.
%
% Note: of course multiple, parallel calls to this function with the same base
% path will result in potential race conditions and risks of collisions.
%
-spec get_non_clashing_entry_name_from( path() ) -> path().
get_non_clashing_entry_name_from( Path ) ->

	% Ex:
	% - if "aaa/bbb/foobar.txt" is specified, returns "aaa/bbb/foobar.txt-1"
	% - if "aaa/bbb/foobar.txt-4" is specified, returns "aaa/bbb/foobar.txt-5"

	% More reliable than looping over random names forged from for example:
	%Uniq = basic_utils:get_process_specific_value()
	%	+ random_utils:get_random_value( _Min=0, _Max=10000 ),
	% until no collision occurs.

	%trace_utils:debug_fmt( "Testing whether path '~ts' already exists...",
	%					   [ Path ] ),

	case exists( Path ) of

		true ->
			PathToTest = case string:split( Path, _SearchPattern="-",
											_Where=trailing ) of

				[ _Path ] ->
					text_utils:format( "~ts-1", [ Path ] );

				[ BasePath, FinalPart ] ->
					case text_utils:try_string_to_integer( FinalPart ) of

						% Not already ending with a dash plus a number:
						undefined ->
							text_utils:format( "~ts-1", [ Path ] );

						Count ->
							text_utils:format( "~ts-~B", [ BasePath, Count+1 ] )

					end

			end,

			 % As clashes may happen for any name:
			get_non_clashing_entry_name_from( PathToTest );

		false ->
			Path

	end.



% Appends, at the end of the first specified file, the content of the second
% specified one: concatenates the second with the first one.
%
-spec append_file( file_name(), file_name() ) -> void().
append_file( TargetFilename, ToAppendFilename ) ->

	ToAppendBin = read_whole( ToAppendFilename ),

	% Test needed, otherwise the next append could create from scratch the
	% target file (would be a masked failure):
	%
	case is_existing_file_or_link( TargetFilename ) of

		true ->
			TargetFile = open( TargetFilename, _Opts=[ append ] ),

			write( TargetFile, ToAppendBin ),

			close( TargetFile );

		false ->
			throw( { append_target_file_not_found, TargetFilename } )

	end.



% Lists all known permission types, as {Perm,Mask} pairs.
-spec list_permission_pairs() -> [ { permission(), permission_mask() } ].
list_permission_pairs() ->
	[ { owner_read,    8#00400 },
	  { owner_write,   8#00200 },
	  { owner_execute, 8#00100 },

	  { group_read,    8#00040 },
	  { group_write,   8#00020 },
	  { group_execute, 8#00010 },

	  { other_read,    8#00004 },
	  { other_write,   8#00002 },
	  { other_execute, 8#00001 },

	  { set_user_id,  16#800 },
	  { set_group_id, 16#400 } ].



% Encodes the specified symbolic permission(s) into its/their low-level
% counterpart mask(s).
%
-spec to_permission_mask( permission() | [ permission() ] ) ->
			permission_mask().
to_permission_mask( PermissionList ) when is_list( PermissionList ) ->
	PermPairs = list_permission_pairs(),
	lists:foldl( fun( P, Acc ) ->
					 to_permission_mask( P, PermPairs ) + Acc
				 end,
				 _Acc0=0,
				 PermissionList );

to_permission_mask( PermAtom ) ->
	to_permission_mask( PermAtom, _PermPairs=list_permission_pairs() ).


% (helper)
to_permission_mask( PermAtom, PermPairs ) ->
	case lists:keyfind( _K=PermAtom, _Index=1, PermPairs ) of

		false ->
			throw( { invalid_permission, PermAtom } );

		{ _PermAtom, PermMask } ->
			PermMask

	end.



% Decodes the specified permission mask into a list of the corresponding
% permissions.
%
-spec from_permission_mask( permission_mask() ) -> [ permission() ].
from_permission_mask( Mask ) ->
	PermPairs = list_permission_pairs(),
	from_permission_mask( PermPairs, Mask, _AccPerms=[] ).


% (helper)
from_permission_mask( _PermPairs=[], _Mask, AccPerms ) ->
	% In-order preferred:
	lists:reverse( AccPerms );

from_permission_mask( _PermPairs=[ { Perm, PermMask } | T ], Mask, AccPerms ) ->

	%trace_utils:debug_fmt( "From permission mask '~p': testing ~p (i.e. ~p).",
	%					   [ Mask, Perm, PermMask ] ),

	NewAccPerms = case Mask band PermMask of

		0 ->
			AccPerms;

		_ ->
			[ Perm | AccPerms ]

	end,

	from_permission_mask( T, Mask, NewAccPerms ).



% Returns the (UNIX) permissions associated to specified filesystem entry.
-spec get_permissions_of( any_path() ) -> [ permission() ].
get_permissions_of( EntryPath ) ->

	case file:read_file_info( EntryPath ) of

		{ ok, #file_info{ mode=Mode } } ->
			from_permission_mask( Mode );

		{ error, Reason } ->
			throw( { get_permissions_of_failed, EntryPath, Reason } )

	end.



% Changes the permissions ("chmod") of specified filesystem element.
%
% Note: erases any prior permissions, i.e. if specifying [other_read] then a
% corresponding file will end up with (exactly) a -------r-- permission.
%
-spec change_permissions( any_path(), permission() | [ permission() ] ) ->
								void().
change_permissions( Path, NewPermissions ) ->

	NewPermMask = to_permission_mask( NewPermissions ),

	%trace_utils:debug_fmt( "Permissions to be changed to ~p, i.e. ~p.",
	%					   [ NewPermissions, NewPermMask ] ),

	case file:change_mode( Path, NewPermMask ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { change_permission_failed, Reason, Path, NewPermissions } )

	end.



% Tells whether the specified path is an absolute one.
%
% A path is deemed absolute iff it starts with "/".
%
-spec is_absolute_path( any_path() ) -> boolean().
%is_absolute_path( _Path=[ $/ | _Rest ] ) ->
%	true;

% Not wanting to let for example atoms slip through:
%is_absolute_path( Path ) when is_list( Path )->
%	false;

%is_absolute_path( Other ) ->
%	throw( { not_a_string_path, Other } ).
is_absolute_path( AnyPath ) ->
	% To support also binary (and even atom) paths:
	case filename:pathtype( AnyPath ) of

		absolute ->
			true;

		relative ->
			false

		%volumerelative -> intentional case_clause

	end.



% Returns an absolute, normalised path corresponding to specified path.
%
% Returns a string of the same type as the specified one.
%
% If it is not already absolute, it will made so by using the current working
% directory.
%
-spec ensure_path_is_absolute( path() ) -> path();
							 ( bin_path() ) -> bin_path().
ensure_path_is_absolute( Path ) ->

	AbsPath = case is_absolute_path( Path ) of

		true ->
			% Already absolute:
			Path;

		false ->
			% Relative, using current directory as base, and returning the same
			% string type as Path:
			%
			any_join( get_current_directory(), Path )

	end,

	normalise_path( AbsPath ).



% Returns an absolute, normalised path corresponding to the specified target
% path, using base path as root directory (this must be an absolute path) if the
% target path is not absolute.
%
% Returns a plain string iff both specified ones are plain, otherwise returns a
% binary.
%
% Ex: ensure_path_is_absolute("tmp/foo", "/home/dalton") will return
% "/home/dalton/tmp/foo".
%
-spec ensure_path_is_absolute( any_path(), any_path() ) -> any_path().
ensure_path_is_absolute( TargetPath, BasePath ) ->

	case is_absolute_path( TargetPath ) of

		true ->
			% Already absolute:
			NormTargetPath = normalise_path( TargetPath ),

			% Ensure correct string type:
			case is_list( BasePath ) of

				true ->
					NormTargetPath;

				% BasePath expected to be a binary then, so returning such type:
				false ->
					text_utils:ensure_binary( NormTargetPath )

			end;


		false ->
			% Relative, using specified base directory (types follow):
			case is_absolute_path( BasePath ) of

				true ->
					normalise_path( any_join( BasePath, TargetPath ) );

				false ->
					throw( { base_path_not_absolute, BasePath } )

			end

	end.



% Normalises specified path (canonicalises it), by translating it so that no
% intermediate, superfluous '.' or '..' is present afterwards.
%
% For example, "/home/garfield/../lisa/./src/.././tube" shall be normalised in
% "/home/lisa/tube".
%
% Returns a path of the same string type as the specified parameter.
%
-spec normalise_path( path() ) -> path();
					( bin_path() ) -> bin_path().
normalise_path( _Path="." ) ->
	".";
	%get_current_directory();

normalise_path( Path ) when is_list( Path ) ->

	ElemList = filename:split( Path ),

	%trace_utils:debug_fmt( "ElemList: ~p", [ ElemList ] ),

	ResPath = join( filter_elems_plain( ElemList, _Acc=[] ) ),

	%trace_utils:debug_fmt( "Normalising path '~ts' as '~ts'.",
	%					   [ Path, ResPath ] ),

	ResPath;


normalise_path( BinPath ) when is_binary( BinPath ) ->

	ElemList = filename:split( BinPath ),

	%trace_utils:debug_fmt( "ElemList: ~p", [ ElemList ] ),

	ResPath = bin_join( filter_elems_bin( ElemList, _Acc=[] ) ),

	%trace_utils:debug_fmt( "Normalising path '~ts' as '~ts'.",
	%					   [ BinPath, ResPath ] ),

	ResPath.



% (helper)
filter_elems_plain( _ElemList=[], Acc ) ->
	lists:reverse( Acc );

filter_elems_plain( _ElemList=[ "." | T ], Acc ) ->
	filter_elems_plain( T, Acc );

% We can remove one level iff there is at least one:
filter_elems_plain( _ElemList=[ ".." | T ], _Acc=[ _ | AccT ] ) ->
	filter_elems_plain( T, AccT );

% No level left, so this ".." should not be filtered out:
%
% (however this clause is a special case of the next, hence can be commented
% out)
%
%filter_elems_plain( _ElemList=[ PathElement=".." | T ], Acc ) ->
%	filter_elems_plain( T, [ PathElement | Acc ] );

filter_elems_plain( _ElemList=[ E | T ], Acc ) ->
	filter_elems_plain( T, [ E | Acc ] ).


% The approach below would not work with, for example, "X/Y/Z/../../A":

%	RevElemList = lists:reverse( filename:split( Path ) ),

%	% Returns in the right order:
%	join( filter_elems_plain( RevElemList, _Acc=[] ) ).


% filter_elems_plain( _Elems=[], Acc ) ->
%	Acc;

% filter_elems_plain( _Elems=[ "." | T ], Acc ) ->
%	filter_elems_plain( T, Acc );

% filter_elems_plain( _Elems=[ "..", _E | T ], Acc ) ->
%	filter_elems_plain( T, Acc );

% filter_elems_plain( _Elems=[ E | T ], Acc ) ->
%	filter_elems_plain( T, [ E | Acc ] ).



% (helper)
filter_elems_bin( _ElemList=[], Acc ) ->
	lists:reverse( Acc );

filter_elems_bin( _ElemList=[ <<".">> | T ], Acc ) ->
	filter_elems_bin( T, Acc );

% We can remove one level iff there is at least one:
filter_elems_bin( _ElemList=[ <<"..">> | T ], _Acc=[ _ | AccT ] ) ->
	filter_elems_bin( T, AccT );

% No level left, so this <<"..">> should not be filtered out:
%
% (however this clause is a special case of the next, hence can be commented
% out)
%
%filter_elems_bin( _ElemList=[ PathElement=<<"..">> | T ], Acc ) ->
%	filter_elems_bin( T, [ PathElement | Acc ] );

filter_elems_bin( _ElemList=[ E | T ], Acc ) ->
	filter_elems_bin( T, [ E | Acc ] ).




% Returns a version of the specified path that is relative to the current
% directory; returns the same type (plain or binary string) as the one of the
% specified path.
%
-spec make_relative( any_path() ) -> any_path().
make_relative( Path ) ->
	make_relative( Path, _RefDir=get_current_directory() ).



% Returns a version of the first specified path that is relative to the
% specified second reference directory; returns the same type (plain or binary
% string) as the one of the first specified path.
%
-spec make_relative( any_path(), any_directory_path() ) -> any_path().
make_relative( Path, RefDir ) when is_list( Path ) andalso is_list( RefDir ) ->

	% Different from filename:absname/2:
	% file_utils:make_relative("/aa/bb/cc","/aa/bb").
	% "cc"
	% filename:absname("/aa/bb/cc","/aa/bb").
	% "/aa/bb/cc"

	AbsPath = ensure_path_is_absolute( Path ),

	AbsRefDir = ensure_path_is_absolute( RefDir ),

	%trace_utils:debug_fmt( "Making path '~ts' (absolute form: '~ts') relative "
	%   "to reference directory '~ts' (absolute form: '~ts').",
	%   [ Path, AbsPath, RefDir, AbsRefDir ] ),

	TargetPathElems = filename:split( AbsPath ),
	RefPathElems = filename:split( AbsRefDir ),

	% Requires both arguments to be plain strings:
	make_relative_plain( TargetPathElems, RefPathElems );


% At least one of them expected to be a binary, so switching to binary:
make_relative( Path, RefDir ) ->

	BinAbsPath = ensure_path_is_absolute( text_utils:ensure_binary( Path ) ),

	BinAbsRefDir =
		ensure_path_is_absolute( text_utils:ensure_binary( RefDir ) ),

	%trace_utils:debug_fmt( "Making path '~ts' (absolute form: '~ts') relative "
	%   "to reference directory '~ts' (absolute form: '~ts').",
	%   [ Path, BinAbsPath, RefDir, BinAbsRefDir ] ),

	TargetPathElems = filename:split( BinAbsPath ),
	RefPathElems = filename:split( BinAbsRefDir ),

	make_relative_binary( TargetPathElems, RefPathElems ).



% First, drop any common path prefix:
make_relative_plain( [ E | TPathElems ], [ E | TRefPathElems ] ) ->
	make_relative_plain( TPathElems, TRefPathElems );

% Found first non-matching directory element:
make_relative_plain( PathElems, RefPathElems ) ->

	%trace_utils:debug_fmt( "Paths split at: ~p vs ~p.",
	%					   [ PathElems, RefPathElems ] ),

	FromRef = [ ".." || _ <- lists:seq( 1, length( RefPathElems ) ) ],

	Res = join( FromRef ++ PathElems ),

	%trace_utils:debug_fmt( "Returned path: '~ts'.", [ Res ] ),

	Res.


% First, drop any common path prefix:
make_relative_binary( [ E | TPathElems ], [ E | TRefPathElems ] ) ->
	make_relative_binary( TPathElems, TRefPathElems );

% Found first non-matching directory element:
make_relative_binary( PathElems, RefPathElems ) ->

	%trace_utils:debug_fmt( "Paths split at: ~p vs ~p.",
	%					   [ PathElems, RefPathElems ] ),

	FromRef = [ <<"..">> || _ <- lists:seq( 1, length( RefPathElems ) ) ],

	Res = bin_join( FromRef ++ PathElems ),

	%trace_utils:debug_fmt( "Returned path: '~ts'.", [ Res ] ),

	Res.



% Returns a pair made of the longest path common to all specified directory
% paths, and the corresponding suffixes, i.e. an (unordered) list of the input
% paths (as binaries) once the common prefix elements have been removed.
%
% Note: operates per-directory (as a whole), not per-character.
%
% Ex: get_longest_common_path(["/tmp/aa/bb/c1/foobar.txt",
%                              "/tmp/aa/bb/c2/foobar.txt"])
%      returns:
% {"/tmp/aa/bb", ["c1","foobar.txt"], ["c2","foobar.txt"]]}
%
% Like text_utils:get_longest_common_prefix/1, except that operates on whole
% path elements, not individual characters.
%
-spec get_longest_common_path( [ any_path() ] ) ->
										{ any_path(), [ any_path() ] }.
get_longest_common_path( DirPaths ) ->

	%trace_utils:debug_fmt( "Getting longest common path for:~n~p",
	%					   [ DirPaths ] ),

	DirElems = [ filename:split( D ) || D <- DirPaths ],

	get_longest_common_path_helper( DirElems, _AccCommon=[] ).


% (helper)
get_longest_common_path_helper( DirElems, AccCommon ) ->

	%trace_utils:debug_fmt( "get_longest_common_path_helper from ~p "
	%						"(acc being ~p).", [ DirElems, AccCommon ] ),

	case get_common_head_of( DirElems ) of

		{ none, Tails } ->
			%trace_utils:debug_fmt( "Finished, with common path ~ts and "
			%                       "tails: ~p.", [ AccCommon, Tails ] ),

			% As filename:join/1 requires a non-empty list:
			LongestPath = case AccCommon of

				[] ->
					<<"">>;

				_ ->
					filename:join( lists:reverse( AccCommon ) )

			end,

			JoinedTails = [ filename:join( T ) || T <- Tails ],

			{ LongestPath, JoinedTails };


		{ Elem, DirElemsTails } ->
			%trace_utils:debug_fmt( "Adding prefix '~w'.", [ Elem ] ),
			get_longest_common_path_helper( DirElemsTails,
											[ Elem | AccCommon ] )

	end.



% Returns {none, Tails} or {CommonElem, RemainingTails}.
%
% (sub-helper)
%
% Degenerate case where a first list does not exist:
get_common_head_of( _DirElemsTails=[] ) ->
	{ none, _Tails=[] };

% We use the head (if any) of the first list as the one to check at the level of
% the head of all others:
%
get_common_head_of( DirElemsTails=[ _First=[] | _Others ] ) ->
	% No head here for first list; common path ended:
	{ none, DirElemsTails };

get_common_head_of( DirElemsTails=[ _First=[ Elem | T ] | Others ] ) ->
	% See whether the non-first tails also start with Elem:
	case try_behead_with( Elem, Others ) of

		non_matching ->
			% Leave lists as they are:
			{ none, DirElemsTails };

		NewOthers ->
			% Do not drop the tail of the first element:
			{ Elem, [ T | NewOthers ] }

	end.


% (sub-sub-helper)
try_behead_with( Elem, Others ) ->
	%trace_utils:debug_fmt( "Beheading of ~p from '~ts'", [ Others, Elem ] ),
	try_behead_with( Elem, Others, _Acc=[] ).


% Others depleted, success:
try_behead_with( _Elem, _Others=[], Acc ) ->
	% Order does not matter, no need to reverse:
	Acc;

% A good Other:
try_behead_with( Elem, _Others=[ [ Elem | R ] | T ], Acc ) ->
	try_behead_with( Elem, T, [ R | Acc ] );

% A bad Other:
% Corresponds to: try_behead_with( Elem, Others=[ [ OtherElem | _R ] | _T ],
%                                  _Acc ) ->
% or to:          try_behead_with( Elem, Others=[ [] | _T ], _Acc ) ->
try_behead_with( _Elem, _Others, _Acc ) ->
	%trace_utils:debug_fmt( "'~ts' could not be removed from ~p",
	%					  [ Elem, Others ] ),
	non_matching.




% Tells whether specified basename (ex: a pathless filename) is among the
% specified list of full paths; returns either false or the first full path
% found corresponding to that leaf element.
%
% Ex:
%  false = file_utils:is_leaf_among( "xx", [ "a/b/c/yy", "d/e/zz"] )
%  "a/b/c/xx"  = file_utils:is_leaf_among( "xx", [ "a/b/c/xx", "d/e/zz"] )
%
-spec is_leaf_among( leaf_name(), [ path() ] ) -> { 'false' | path() }.
is_leaf_among( _LeafName, _PathList=[] ) ->
	false;

is_leaf_among( LeafName, _PathList=[ Path | T ] ) ->

	case filename:basename( Path ) of

		LeafName ->
			Path;

		_  ->
			is_leaf_among( LeafName, T )

	end.



% Updates specified file with specified keywords, i.e. copies the original file
% into a target, updated one (supposedly non-already existing), in which all the
% specified keywords (the keys of the translation table) have been replaced with
% their associated value (i.e. the value in table corresponding to that key).
%
% Ex: file_utils:update_with_keywords( "original.txt", "updated.txt", table:new(
%  [ {"hello", "goodbye"}, {"Blue", "Red"} ] ).
%
% Note that the resulting file will be written with no additional encoding.
%
-spec update_with_keywords( any_file_path(), any_file_path(),
							text_utils:translation_table() ) -> void().
update_with_keywords( OriginalFilePath, TargetFilePath, TranslationTable ) ->
	update_with_keywords( OriginalFilePath, TargetFilePath, TranslationTable,
						  _EncodingOpts=[] ).



% Updates specified file with specified keywords, i.e. copies the original file
% into a target, updated one (supposedly non-already existing; and with the
% specified encoding), in which all the specified keywords (the keys of the
% translation table) have been replaced with their associated value (i.e. the
% value in table corresponding to that key).
%
% Ex: file_utils:update_with_keywords( "original.txt", "updated.txt", table:new(
%  [ { "hello", "goodbye" }, { "Blue", "Red" } ] ).
%
-spec update_with_keywords( any_file_path(), any_file_path(),
		text_utils:translation_table(), system_utils:encoding_options() ) ->
									void().
update_with_keywords( OriginalFilePath, TargetFilePath, TranslationTable,
					  EncodingOpts ) ->

	case exists( TargetFilePath ) of

		true ->
			throw( { already_existing, TargetFilePath } );

		false ->
			ok

	end,

	BinOrigContent = read_whole( OriginalFilePath ),

	BinUpdatedContent = text_utils:update_with_keywords( BinOrigContent,
														 TranslationTable ),

	%trace_utils:debug_fmt( "Original content: ~ts;~n Translation table: ~p;~n"
	%		" Updated content: ~ts.",
	%		[ BinOrigContent, TranslationTable, BinUpdatedContent ] ),

	write_whole( TargetFilePath, BinUpdatedContent, EncodingOpts ).



% Converts specified path (full filename, like '/home/jack/test.txt' or
% './media/test.txt') into a variable name licit in most programming languages
% (ex: C/C++).
%
% Rule here is:
%  - variable name starts with a prefix, user-supplied or the default one
%  - any leading './' is removed
%  - '-' becomes '_'
%  - '.' becomes '_'
%  - '/' becomes '_'
%
-spec path_to_variable_name( path() ) -> ustring().
path_to_variable_name( Filename ) ->
	path_to_variable_name( Filename, "File_" ).


% (helper)
% Removes any leading './'.
-spec path_to_variable_name( path(), ustring() ) -> ustring().
path_to_variable_name( [ $.,$/ | T ], Prefix ) ->
	convert( T, Prefix );

path_to_variable_name( Filename, Prefix ) ->
	convert( Filename, Prefix ).



% (helper)
convert( Filename, Prefix ) ->

	NoDashName = re:replace( lists:flatten( Filename ), "-+", "_",
		[ global, { return, list } ] ),

	NoDotName = re:replace( NoDashName, "\\.+", "_",
		[ global, { return, list } ] ),

	Prefix ++ re:replace( NoDotName, "/+", "_",
		[ global, { return, list } ] ).



% Removes all upper levels of a path (absolute or not), as well as the extension
% of the resulting file name.
%
% Ex: "foobar" =
%           file_utils:remove_upper_levels_and_extension( "aa/bb/foobar.txt" ).
%
remove_upper_levels_and_extension( FilePath ) ->

	PathLevels = filename:split( FilePath ),

	FileName = lists:last( PathLevels ),

	case string:rchr( FileName, $. ) of

		0 ->
			FileName;

		Index ->
			string:sub_string( FileName, 1, Index-1 )

	end.




% Returns a list of the known file extensions that refer to image files.
-spec get_image_extensions() -> [ extension() ].
get_image_extensions() ->
	% TIFF, TGA and al deemed deprecated:
	[ ".png", ".jpg", ".jpeg", ".bmp"].



-define(ResourceDir,"resources").


% Returns the image path corresponding to the specified file.
-spec get_image_file_png( file_name() ) -> path().
get_image_file_png( Image ) ->
  filename:join( [ ?ResourceDir, "images", Image ++ ".png"] ).



% Returns the image path corresponding to the specified file.
-spec get_image_file_gif( file_name() ) -> path().
get_image_file_gif( Image ) ->
  filename:join( [ ?ResourceDir, "images", Image ++ ".gif"] ).




% I/O section.


% Returns the default recommended encoding, for example when needing to open a
% file for writing.
%
% See the notes above in the 'Regarding encodings and Unicode' section, notably
% about the consequences of specifying an encoding at file opening (generally
% directly writing encoded content is safer and offers more control).
%
-spec get_default_encoding() -> system_utils:encoding().
get_default_encoding() ->
	system_utils:get_default_encoding().


% Returns the default recommended option encoding option, for example when
% needing to open a file for writing - should such an option be used.
%
% See the notes above in the 'Regarding encodings and Unicode' section, notably
% about the consequences of specifying an encoding at file opening (generally
% directly writing encoded content is safer and offers more control).
%
-spec get_default_encoding_option() -> system_utils:encoding_option().
get_default_encoding_option() ->
	system_utils:get_default_encoding_option().




% Opens the file corresponding to the specified filename, with specified list of
% options (as listed for file:open/2 in
% http://erlang.org/doc/man/file.html#open-2, i.e. read, write, append,
% exclusive, raw, etc.).
%
% See read_terms/1 if planning to read that content as terms later, notably with
% regard to encoding.
%
% Returns the file reference, or throws an exception.
%
% Will attempt to open the specified file only once, as looping endlessly does
% not seem a viable solution right now (risk of exhausting the descriptors,
% making the VM fail for example when loading a new BEAM).
%
% As soon as a file is opened for writing, a corresponding empty file appears in
% the filesystem.
%
% Note:
%
% - we used to think that 'raw' may cause problems with encodings; consider
% specifying system_utils:get_default_encoding_option/0; refer to the 'Regarding
% encodings and Unicode' section at the top of this file for further information
%
% - if an opened file fails to be correctly read encoding-wise (characters like
% 'à' being not only displayed but also read garbled, and if setting
% {encoding,unicode} returns an error such as
% {read_error,{no_translation,unicode,unicode}}, then this may be an
% (unfortunate) side-effect of having run the VM with the -noinput option; in
% this case, the best option is to execute once, preferably early (ex: as first
% statement) system_utils:force_unicode_support/0.
%
% Note also that if the 'raw' flag is included among opening flags, any
% specified encoding might be ignored (ex: UTF8 being specified, whereas ISO/IEC
% 8859 being written).
%
-spec open( any_file_name(), [ file_open_mode() ] ) -> file().
open( Filename, Options ) ->
	open( Filename, Options, _Default=try_once ).



% Opens the file corresponding to specified filename (first parameter) with
% specified list of options (second parameter; refer to file:open/2 for detailed
% documentation, see http://erlang.org/doc/man/file.html#open-2).
%
% Third parameter is the "attempt mode", either 'try_once', 'try_endlessly' or
% 'try_endlessly_safer', depending respectively on whether we want to try to
% open the file once (no other attempt will be made), endlessly (until a file
% descriptor can be gained), possibly with a safer setting.
%
% Returns the file reference, or throws an exception.
%
% Will try to obtain a file descriptor iteratively (and endlessly) with
% process-specific random waitings, should no descriptor be available.
%
% A risk of that approach is that all available file descriptors will be
% taken, thus potentially preventing other processes (including the VM itself)
% to perform any file operation, like loading a new BEAM, ex:
% """
% File operation error: system_limit. Target:
% lib/erlang/lib/kernel-x.y.z/ebin/timer.beam. Function: get_file.
% Process: code_server.
% """
%
% This is done in order to support situations where potentially more Erlang
% processes than available file descriptors try to access to files. An effort is
% made to desynchronize these processes to smooth the use of descriptors.
%
% Note: if an opened file fails to be correctly read encoding-wise (characters
% like 'à' being not only displayed but also read garbled, and if setting
% {encoding,unicode} returns an error such as
% {read_error,{no_translation,unicode,unicode}}, then this may be an
% (unfortunate) side-effect of having run the VM with the -noinput option; in
% this case, the best option is to execute once, preferably early (ex: as first
% statement) system_utils:force_unicode_support/0.
%
% Note also that if the 'raw' flag is included among opening flags, any
% specified encoding might be ignored (ex: UTF8 being specified, whereas ISO/IEC
% 8859 being written).
%
-spec open( any_file_name(), [ file_open_mode() ],
			'try_once' | 'try_endlessly' | 'try_endlessly_safer' ) -> file().
open( Filename, Options, _AttemptMode=try_endlessly_safer ) ->

	%trace_utils:debug_fmt( "Opening '~ts' endlessly yet safe, "
	%   "with options ~w.", [ Filename, Options ] ),

	File = open( Filename, Options, try_endlessly ),

	% We could check here that at least one descriptor remains, by adding a
	% dummy file open/close and catching emfile, however one could need more
	% than one spare descriptor.
	%
	% The correct solution would involve knowing the max number of descriptors
	% for that process and the current number of open ones, no information we
	% seems able to know.
	%
	% So for the moment we do not do anything more than 'try_endlessly':
	File;


open( Filename, Options, _AttemptMode=try_endlessly ) ->

	%trace_utils:debug_fmt( "Opening '~ts' endlessly, with options ~w.",
	%					   [ Filename, Options ] ),

	case file:open( Filename, Options ) of

		{ ok, File } ->
			 File;

		{ error, FileError } when FileError == emfile
				orelse FileError == system_limit ->

			% File descriptors exhausted for this OS process.
			% Kludge to desynchronize file opening in order to remain below 1024
			% file descriptor opened:
			%
			Duration =
				basic_utils:get_process_specific_value( _Min=50, _Max=200 ),

			% Attempt not to use timer:sleep (anyway will trigger errors
			% afterwards when the system will try to look-up some BEAMs):
			%
			receive

			after Duration ->

				open( Filename, Options, try_endlessly )

			end;

		{ error, eacces } ->
			throw( { open_failed, { Filename, Options }, access_denied,
					 get_access_denied_info( Filename ) } );

		{ error, OtherFileError } ->
			throw( { open_failed, { Filename, Options }, OtherFileError } )

	end;


% By far the most commonly-used clause:
open( Filename, Options, _AttemptMode=try_once ) ->

	%trace_utils:debug_fmt( "Opening '~ts' once, with the ~w options, "
	%	"from '~ts'.", [ Filename, Options, get_current_directory() ] ),

	case file:open( Filename, Options ) of

		{ ok, File } ->
			 File;

		{ error, eacces } ->
			throw( { open_failed, { Filename, Options }, access_denied,
					 get_access_denied_info( Filename ) } );

		{ error, emfile } ->
			throw( { too_many_open_files, { Filename, Options } } );

		{ error, system_limit } ->
			% Never had system_limit without this cause (yet!):
			throw( { too_many_open_files, { Filename, Options },
					 system_limit } );

		{ error, OtherError } ->
			throw( { open_failed, { Filename, Options }, OtherError } )

	end.


% (helper)
get_access_denied_info( Filename ) ->

	Dir = filename:dirname( Filename ),

	case is_existing_directory( Dir ) of

		true ->
			UserInfo = { actual_user, system_utils:get_user_name_safe(),
						 { user_id, system_utils:get_user_id() } },

			FileInfo = case is_existing_file_or_link( Filename ) of

				true ->
					{ existing_file, { owner_id, get_owner_of( Filename ) },
					  { group_id, get_group_of( Filename ) },
					  { permissions, get_permissions_of( Filename ) } };

				false ->
					non_existing_file

			end,

			DirOwnerInfo = { owner_id, get_owner_of( Dir ) },
			DirGroupInfo = { group_id, get_group_of( Dir ) },
			DirPerms = { permissions, get_permissions_of( Dir ) },

			DirInfo = { existing_directory, Dir, DirOwnerInfo, DirGroupInfo,
						DirPerms },

			{ UserInfo, FileInfo, DirInfo };

		false ->
			{ non_existing_directory, Dir }

	end.



% Closes specified file reference.
%
% Throws an exception on failure.
%
-spec close( file() ) -> void().
close( File ) ->
	close( File, throw_if_failed ).



% Closes specified file reference.
%
% Throws an exception on failure or not, depending on specified failure mode.
%
-spec close( file(), 'overcome_failure' | 'throw_if_failed' ) -> void().
close( File, _FailureMode=throw_if_failed ) ->

	case file:close( File ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { file_closing_failed, Reason, File } )

	end;

close( File, _FailureMode=overcome_failure ) ->
	file:close( File ).



% Reads specified number of bytes/characters from the specified file.
%
% Returns either { ok, Data } if at least some data could be read, or eof if at
% least one element was to read and end of file was reached before anything at
% all could be read.
%
% Throws an exception on failure.
%
-spec read( file(), basic_utils:count() ) ->
					{ 'ok', ustring() | binary() } | 'eof'.
read( File, Count ) ->

	case file:read( File, Count ) of

		R={ ok, _Data } ->
			R;

		eof ->
			eof;

		{ error, Reason } ->
			throw( { read_failed, Reason } )

	end.



% Writes specified byte-oriented content in the specified file.
%
% Operates on files opened in raw mode (only way to do so), or not (works for
% normal mode as well).
%
% Throws an exception on failure.
%
% See write_ustring/{2,3} to write Unicode text.
%
-spec write( file(), iodata() ) -> void().
write( File, Content ) ->

	%trace_utils:debug_fmt( "Writing '~w' to ~p.", [ Content, File ] ),

	case file:write( File, Content ) of

		ok ->
			ok;

		% If Reason is badarg, possibly an encoding issue (ex: having used '~ts'
		% instead of '~ts'):
		%
		{ error, Reason } ->
			throw( { write_failed, Reason, Content, File } )

	end.



% Writes specified Unicode string in the specified file.
%
% Operates on files opened in raw mode (only way to do so), or not (works for
% normal mode as well).
%
% Throws an exception on failure.
%
-spec write_ustring( file(), ustring() ) -> void().
write_ustring( File, Str ) ->

	%trace_utils:debug_fmt( "Writing '~ts' to ~p.", [ Str, File ] ),

	Bin = text_utils:to_unicode_binary( Str ),
	%trace_utils:debug_fmt( " - Bin: ~p.", [ Bin ] ),

	%BinStr = io_lib:format("~ts", [ Bin ] ),
	%trace_utils:debug_fmt( " - BinStr: ~p.", [ BinStr ] ),

	% Using current encoding (i.e. the one that file was opened with):
	case file:write( File, Bin ) of

		ok ->
			ok;

		% If Reason is badarg, possibly an encoding issue (ex: having used '~ts'
		% instead of '~ts'):
		%
		{ error, Reason } ->
			throw( { write_ustring_failed, Reason, Str, File } )

	end.



% Writes specified formatted content in the specified file.
%
% Throws an exception on failure.
%
-spec write_ustring( file(), format_string(), [ term() ] ) -> void().
write_ustring( File, FormatString, Values ) ->
	Text = text_utils:format( FormatString, Values ),
	write_ustring( File, Text ).



% Reads the content of the specified file, based on its filename specified as
% any kind of string (plain, binary, atom, etc.), and returns the corresponding
% binary, or throws an exception on failure.
%
% See also: read_terms/1 to read directly Erlang terms.
%
-spec read_whole( any_file_name() ) -> binary().
read_whole( Filename ) ->

	%trace_utils:debug_fmt( "Reading as a whole '~ts'.", [ Filename ] ),

	case file:read_file( Filename ) of

		{ ok, Binary } ->
			Binary;

		{ error, eacces } ->
			throw( { read_whole_failed, Filename, access_denied,
					 get_access_denied_info( Filename ) } );

		{ error, Error } ->
			throw( { read_whole_failed, Filename, Error } )

	end.



% Writes the specified content in specified file, whose filename is specified as
% any kind of string, using the default encoding.
%
% Throws an exception on failure.
%
-spec write_whole( any_file_name(), ustring() | binary() ) -> void().
write_whole( Filename, Content ) ->

	% Now we prefer no automatic encoding, and ensure it has been done
	% beforehand:
	%
	%Mode = [ system_utils:get_default_encoding_option() ],
	Mode = [],

	write_whole( Filename, Content, Mode ).



% Writes the specified content in specified file, whose filename is specified as
% any kind of string, using the specified encoding for writing.
%
% Note that no transparent encoding is expected to be specified through modes,
% as this function performs (through text_utils:string_to_binary/1) such
% encoding on plain strings.
%
% Throws an exception on failure.
%
-spec write_whole( any_file_name(), ustring() | binary(), [ file:mode() ] ) ->
							void().
write_whole( Filename, StringContent, Modes ) when is_list( StringContent ) ->
	write_whole( Filename, text_utils:string_to_binary( StringContent ),
				 Modes );

write_whole( Filename, BinaryContent, Modes ) ->

	%trace_utils:debug_fmt( "Writing to '~ts', with modes ~p, "
	%	"following content:~n~ts", [ Filename, Modes, BinaryContent ] ),

	% 'write' and 'binary' are implicit here; if relevant BinaryContent must be
	% correctly Unicode-encoded:
	%
	case file:write_file( Filename, BinaryContent, Modes ) of

		ok ->
			% Useless, paranoid checking:
			%case is_existing_file( Filename ) of
			%
			%	true ->
			%		trace_utils:debug_fmt( "'~ts' written as a whole.",
			%							   [ Filename ] ),
			%		ok;
			%
			%	false ->
			%		throw( { write_whole_failed, Filename, no_file } )
			%
			%end;
			ok;

		{ error, eacces } ->
			throw( { write_whole_failed, { Filename, Modes }, access_denied,
					 get_access_denied_info( Filename ) } );

		{ error, Error } ->
			throw( { write_whole_failed, { Filename, Modes }, Error } )

	end.



% Reads specified file, tries to parse a list of terms from it (as
% file:consult/1 does), and returns it.
%
% If expecting to read UTF-8 content from a file, it should:
%
%  - have been then opened for writing typically while including the {encoding,
%  utf8} option, or have been written with content already properly encoded
%  (maybe more reliable that way)
%
%  - start with a '%% -*- coding: utf-8 -*-' header
%
% Throws an exception on error.
%
-spec read_terms( file_path() ) -> [ term() ].
read_terms( Filename ) ->

	case file:consult( Filename ) of

		{ ok, Terms } ->
			Terms;

		{ error, eacces }  ->
			throw( { reading_failed, text_utils:ensure_string( Filename ),
					 access_denied, get_access_denied_info( Filename ) } );

		{ error, Error } when is_atom( Error ) ->
			throw( { reading_failed, text_utils:ensure_string( Filename ),
					 Error } );

		{ error, Error={ Line, Module, Term } } ->
			Reason = file:format_error( Error ),
			throw( { interpretation_failed,
					 text_utils:ensure_string( Filename ), { line, Line },
					 { module, Module }, { term, Term }, Reason } )

	end.



% Writes specified terms into specified file, with no specific header or footer.
%
% Heavily inspired from Joe Armstrong's lib_misc:unconsult/2.
%
-spec write_terms( [ term() ], file_path() ) -> void().
write_terms( Terms, Filename ) ->
	write_terms( Terms, _Header=undefined, _Footer=undefined, Filename ).



% Writes specified terms into specified file, with specified header and footer.
%
% Heavily inspired from Joe Armstrong's lib_misc:unconsult/2.
%
-spec write_terms( [ term() ], maybe( ustring() ), maybe( ustring() ),
				   file_path() ) -> void().
write_terms( Terms, Header, Footer, Filename ) ->

	F = open( Filename, _Opts=[ write, raw, delayed_write ] ),

	case Header of

		undefined ->
			ok;

		_ ->
			write_ustring( F, "% ~ts~n~n~n", [ Header ] )

	end,

	write_direct_terms( Terms, F ),

	case Footer of

		undefined ->
			ok;

		_ ->
			write_ustring( F, "~n~n% ~ts~n", [ Footer ] )

	end,

	close( F ).



% Writes directly specified terms into specified already opened file.
%
% Heavily inspired from Joe Armstrong's lib_misc:unconsult/2.
%
-spec write_direct_terms( file(), [ term() ] ) -> void().
write_direct_terms( File, Terms ) ->
	[ write_ustring( File, "~p.~n", [ T ] ) || T <- Terms ].



% Compression-related operations.


% Returns the file extension corresponding to filenames compressed with
% specified format.
%
-spec get_extension_for( compression_format() ) -> extension().
get_extension_for( _CompressionFormat=zip ) ->
	".zip";

get_extension_for( _CompressionFormat=bzip2 ) ->
	".bz2";

get_extension_for( _CompressionFormat=xz ) ->
	".xz".



% Compresses specified file: creates a new, compressed version thereof (using
% the most efficient, compacity-wise, compression tool available), whose
% filename, established based on usual conventions, is returned. If a file with
% that name already exists, it will be overwritten.
%
% For example, compress( "hello.png" ) will generate a "hello.png.xz"
% file.
%
% The original file remain as is.
%
% Note: this function just takes care of compressing a single file, even if some
% compressors (ex: zip) include features to create an archive of multiple files
% first.
%
-spec compress( file_name() ) -> file_name().
compress( Filename ) ->
	compress( Filename, _CompressionFormat=xz ).



% Compresses specified file: creates a new, compressed version thereof, whose
% filename, established based on usual conventions, is returned. If a file with
% that name already exists, it will be overwritten.
%
% For example, compress( "hello.png", zip ) will generate a "hello.png.zip"
% file.
%
% The original file remain as is.
%
% Note: this function just takes care of compressing a single file, even if some
% compressors (ex: zip) include features to create an archive of multiple files
% first.
%
-spec compress( file_name(), compression_format() ) -> file_name().
compress( Filename, _CompressionFormat=zip ) ->

	% Rather than using a standalone zip tool, we use the Erlang support here:

	%ZipExec = executable_utils:get_default_zip_compress_tool(),

	ZipFilename = Filename ++ get_extension_for( zip ),

	% Exactly this one file in the archive:
	%Command = ZipExec ++ " --quiet " ++ ZipFilename ++ " " ++ Filename,

	%[] = os:cmd( Command ),

	zip:zip( ZipFilename, [ Filename ] ),

	% Check:
	true = is_existing_file( ZipFilename ),

	ZipFilename;


compress( Filename, _CompressionFormat=bzip2 ) ->

	Bzip2Exec = executable_utils:get_default_bzip2_compress_tool(),

	% --keep allows to avoid that bzip2 removes the original file:
	case system_utils:run_command(
			Bzip2Exec ++ " --keep --force --quiet " ++ Filename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			Bzip2Filename = Filename ++ get_extension_for( bzip2 ),
			true = is_existing_file( Bzip2Filename ),
			Bzip2Filename;

		{ _ExitCode=0, Output } ->
			throw( { bzip2_compress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { bzip2_compress_failed, Filename, ExitCode, Output } )

	end;


compress( Filename, _CompressionFormat=xz ) ->

	XZExec = executable_utils:get_default_xz_compress_tool(),

	% --keep allows to avoid that bzip2 removes the original file:
	case system_utils:run_command(
			XZExec ++ " --keep --force --quiet " ++ Filename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			XZFilename = Filename ++ get_extension_for( xz ),
			true = is_existing_file( XZFilename ),
			XZFilename;

		{ _ExitCode=0, Output } ->
			throw( { xz_compress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { xz_compress_failed, Filename, ExitCode, Output } )

	end;

compress( _Filename, CompressionFormat ) ->
	throw( { unsupported_compression_format, CompressionFormat } ).



% Decompresses specified compressed file, expected to bear the extension
% corresponding to the implicit, most compact format: recreates the original,
% decompressed version thereof, whose filename, established based on usual
% conventions, is returned: the name of the input file without its extension.
%
% This function works in pair with compress/2, and as such expects that each
% compressed file contains exactly one file, bear the same filename except the
% compressor extension.
%
% Typically, when a format MY_FORMAT is specified, converts a compressed file
% name foo.extension_of(MY_FORMAT) into an uncompressed version of it named
% 'foo'.
%
% So, for example, decompress( "foo.xz" ) will generate a "foo" file.
%
% If a file with that name already exists, it will be overwritten.
%
% The compressed file remains as is.
%
-spec decompress( file_name() ) -> file_name().
decompress( Filename ) ->
	decompress( Filename, _CompressionFormat=xz ).



% Decompresses specified compressed file, expected to bear the extension
% corresponding to the specified format: recreates the original, decompressed
% version thereof, whose filename, established based on usual conventions, is
% returned: the name of the input file without its extension.
%
% This function works in pair with compress/2, and as such expects that each
% compressed file contains exactly one file, bear the same filename except the
% compressor extension.
%
% Typically, when a format MY_FORMAT is specified, converts a compressed file
% name foo.extension_of(MY_FORMAT) into an uncompressed version of it named
% 'foo'.
%
% So, for example, decompress( "foo.xz", xz ) will generate a "foo" file.
%
% If a file with that name already exists, it will be overwritten.
%
% The compressed file remains as is.
%
-spec decompress( file_name(), compression_format() ) -> file_name().
decompress( ZipFilename, _CompressionFormat=zip ) ->

	% An annoying problem with zip is that the name of the (single) file in the
	% archive might differ from the filename deduced from the archive name (ex:
	% "foo.zi"p might contain "bar" instead of "foo"). We need to return "bar",
	% not "foo".

	% Rather than using a standalone zip tool, we use the Erlang support here:

	%UnzipExec = executable_utils:get_default_zip_decompress_tool(),

	% Checks and removes extension:
	%Filename = replace_extension( ZipFilename, get_extension_for( zip ), "" ),

	% Quiet, overwrite:
	%Command = UnzipExec ++ " -q -o " ++ ZipFilename,

	%[] = os:cmd( Command ),

	% Exactly one file per such archives:
	{ ok, [ Filename ] } = zip:unzip( ZipFilename ),

	% We expect here than only the compression feature (not the archive-making
	% feature) of zip has been used, as for all other compressors:
	%
	true = is_existing_file( Filename ),

	Filename;


decompress( Bzip2Filename, _CompressionFormat=bzip2 ) ->

	Bzip2Exec = executable_utils:get_default_bzip2_decompress_tool(),

	% Checks and removes extension:
	Filename = replace_extension( Bzip2Filename, get_extension_for( bzip2 ),
								  "" ),

	% The result will be named Filename by bunzip2:

	case system_utils:run_command(
			Bzip2Exec ++ " --keep --force --quiet " ++ Bzip2Filename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			Bzip2Filename = Filename ++ get_extension_for( bzip2 ),
			true = is_existing_file( Filename ),
			Filename;

		{ _ExitCode=0, Output } ->
			throw( { bzip2_decompress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { bzip2_decompress_failed, Filename, ExitCode, Output } )

	end;


decompress( XzFilename, _CompressionFormat=xz ) ->

	XZExec = executable_utils:get_default_xz_decompress_tool(),

	% Checks and removes extension:
	Filename = replace_extension( XzFilename, get_extension_for( xz ), "" ),

	case system_utils:run_command(
			XZExec ++ " --keep --force --quiet " ++ XzFilename ) of

		{ _ExitCode=0, _Output=[] } ->
			% Check:
			true = is_existing_file( Filename ),
			Filename;

		{ _ExitCode=0, Output } ->
			throw( { xz_decompress_failed, Filename, Output } );

		{ ExitCode, Output } ->
			throw( { xz_decompress_failed, Filename, ExitCode, Output } )

	end;


decompress( _Filename, CompressionFormat ) ->
	throw( { unsupported_compression_format, CompressionFormat } ).





% Reads in memory the file specified from its filename, zips the corresponding
% term, and returns it, as a compressed binary.
%
% Note: useful for network transfers of small files.
%
% Larger ones should be transferred with TCP/IP and by chunks.
%
% Returns a binary.
%
-spec file_to_zipped_term( file_name() ) -> binary().
file_to_zipped_term( Filename ) ->

	DummyFileName = "dummy",

	{ ok, { _DummyFileName, Bin } } =
		%zip:zip( DummyFileName, [ Filename ], [ verbose, memory ] ),
		zip:zip( DummyFileName, [ Filename ], [ memory ] ),

	Bin.



% Reads specified binary, extracts the zipped file in it and writes it on disk,
% in current directory.
%
% Returns the filename of the unzipped file.
%
-spec zipped_term_to_unzipped_file( binary() ) -> file_name().
zipped_term_to_unzipped_file( ZippedTerm ) ->
	%zip:unzip( ZippedTerm, [ verbose ] ).
	{ ok, [ FileName ] } = zip:unzip( ZippedTerm ),
	FileName.



% Reads specified binary, extracts the zipped file in it and writes it on disk,
% in current directory, under specified filename instead of under filename
% stored in the zip archive.
%
% Any pre-existing file will be overwritten.
%
% Note: only one file is expected to be stored in the specified archive.
%
-spec zipped_term_to_unzipped_file( binary(), file_name() ) -> void().
zipped_term_to_unzipped_file( ZippedTerm, TargetFilename ) ->

	{ ok, [ { _AFilename, Binary } ] } = zip:unzip( ZippedTerm, [ memory ] ),

	% { ok, File } = file:open( TargetFilename, [ write ] ),
	% ok = io:format( File, "~ts", [ binary_to_list(Binary) ] ),
	% ok = file:write_file( File, "~ts", [ binary_to_list(Binary) ] ),
	% ok = file:close( File ).
	write_whole( TargetFilename, Binary ).



% Reads in memory the files specified from their filenames (as plain strings),
% zips the corresponding term, and returns it.
%
% Note: useful for network transfers of small files.
%
% Larger ones should be transferred with TCP/IP and by chunks.
%
% Returns a binary.
%
-spec files_to_zipped_term( [ file_name() ] ) -> binary().
files_to_zipped_term( FilenameList ) ->

	DummyFileName = "dummy",

	{ ok, { _DummyFileName, Bin } } =
		zip:zip( DummyFileName, FilenameList, [ memory ] ),

	Bin.



% Reads in memory the files specified from their filenames (as plain strings),
% assuming their path is relative to the specified base directory, zips the
% corresponding term, and returns it.
%
% Note: useful for network transfers of small files.
%
% Larger ones should be transferred with TCP/IP and by chunks.
%
% Returns a binary.
%
-spec files_to_zipped_term( [ file_name() ], any_directory_name() ) -> binary().
files_to_zipped_term( FilenameList, BaseDirectory ) ->

	DummyFileName = "dummy",

	%trace_utils:notice_fmt( "files_to_zipped_term operating, from '~ts', "
	%						 "on following ~B file(s): ~ts",
	%						 [ BaseDirectory, length( FilenameList ),
	%						   text_utils:terms_to_string( FilenameList ) ] ),

	 case zip:zip( DummyFileName, FilenameList,
				   [ memory, { cwd, BaseDirectory } ] ) of

		 { ok, { _DummyFileName, Bin } } ->
			 Bin;


		 { error, enoent } ->

			 % Such a short error might be difficult to diagnose:

			 %trace_utils:warning_fmt( "files_to_zipped_term/2 failed "
			 %  "from '~ts':~n~n - directory '~p' exists? ~p",
			 %		[ get_current_directory(), BaseDirectory,
			 %		  is_existing_directory( BaseDirectory ) ] ),

			 % [ trace_utils:warning_fmt( "~n - file '~p' exists? ~p", [ F,
			 %	   is_existing_file( F ) ] ) || F <- FilenameList ],

			 throw( { zip_failed, BaseDirectory, FilenameList } );

		 % einval might mean for example that at least some filenames are
		 % binaries rather that plain strings:
		 %
		 { error, Other } ->
			 throw( { zip_failed, Other, BaseDirectory, FilenameList } )

	 end.



% Reads specified binary, extracts the zipped files stored in it and writes them
% on disk, in current directory.
%
% Returns the list of filenames corresponding to the unzipped files.
%
-spec zipped_term_to_unzipped_files( binary() ) -> [ file_name() ].
zipped_term_to_unzipped_files( ZippedTerm ) ->
	%{ ok, FileNames } = zip:unzip( ZippedTerm, [ verbose ] ),
	{ ok, FileNames } = zip:unzip( ZippedTerm ),
	FileNames.



% Reads specified binary, extracts the zipped files in it and writes them on
% disk, in specified directory.
%
% Returns the list of filenames corresponding to the unzipped files.
%
-spec zipped_term_to_unzipped_files( binary(), directory_name() ) ->
											[ file_name() ].
zipped_term_to_unzipped_files( ZippedTerm, TargetDirectory ) ->

	%{ ok, FileNames } = zip:unzip( ZippedTerm, [ verbose ] ),

	case is_existing_directory( TargetDirectory ) of

		true ->
			{ ok, FileNames } =
				zip:unzip( ZippedTerm, [ { cwd, TargetDirectory } ] ),
			FileNames;

		false ->
			throw( { non_existing_unzip_directory, TargetDirectory } )

	end.
