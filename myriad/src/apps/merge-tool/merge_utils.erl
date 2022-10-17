% Copyright (C) 2016-2022 Olivier Boudeville
%
% Transferred from merge-tree.escript to benefit from a more user-friendly
% debugging.
%
% Released as LGPL software.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: 2016.


% @doc Module in charge of providing the actual support for the <b>management of
% filesystem trees</b> (merging, unifiquation, comparison, etc.).
%
% Note that it is currently architectured mostly as a program rather than as a
% library (most services of interest are not exported currently).
%
-module(merge_utils).


% Implementation notes:
%
% - merge cache files:
%  * could/should use a compressed form ('compress' option)
%  * rely only on string-based paths (not binary ones)
%
% - at least currently we only focus on (regular) files, hence the counts for
% directories and all other elements remain null


% Note that transferring a uniquified tree with scp may reintroduce duplicates
% if the previous uniquification resulted in the then duplicates to be replaced
% by symlinks: scp will create a regular file for each symlink.
%
% So prefer using rsync:
% $ rsync --links
%    or
% $ rsync -avz -e "ssh -p MY_PORT" SRC HOST:DEST

-define( merge_cache_filename, <<".merge-tree.cache">> ).


% Version of this tool:
-define( merge_script_version, "0.1.4" ).


% Centralised:
-define( merge_file_options,
		 % We do not want to include the '{encoding,utf8}' option as, strangely
		 % enough, we were not able to write the proper content in file (ex: a
		 % filename including 'cœur' was incorrectly written as 'cÅur' whereas
		 % properly displayed on the console - hence whereas correct); we
		 % suspected a double Unicode conversion yet could not overcome this
		 % issue.
		 %
		 % So now we write directly our own Unicode content:
		 [ write, raw ] ).


-define( default_log_filename, "merge-tree.log" ).

-define( bullet_point, "  + " ).

% To avoid UI issues for example with longer content lists:
-define( max_text_length, 12000 ).


-export([ create_merge_cache_file_for/3,
		  tree_data_to_string/1, file_data_to_string/1,
		  display_tree_data/2, display_tree_data/3,
		  trace/2, trace/3, trace_debug/2, trace_debug/3 ]).


% Shorthands:

-type count() :: basic_utils:count().
-type status_code() :: basic_utils:status_code().

-type ustring() :: text_utils:ustring().
-type format_string() :: text_utils:format_string().


% Thus a binary:
-type sha1() :: hash_utils:sha1_sum().

-type command_line_option() :: shell_utils:command_line_option().
-type command_line_value() :: shell_utils:command_line_value().

-type byte_size() :: system_utils:byte_size().

-type set( T ) :: set_utils:set( T ).

-type directory_path() :: file_utils:directory_path().
-type bin_directory_path() :: file_utils:bin_directory_path().
-type any_directory_path() :: file_utils:any_directory_path().

-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().

-type file() :: file_utils:file().

-type posix_seconds() :: time_utils:posix_seconds().

-type bin_fqdn() :: net_utils:bin_fqdn().



% Data associated (in-memory) to a given file-like element.
%
% Note: these records are typically values stored in tables, whose associated
% key is potentially duplicating their path (not a problem).
%
-record( file_data, {

	% Path of this file (an identifier thereof), relative to the tree
	% root:
	%
	path :: bin_file_path(),

	% Type of the file element:
	type :: file_utils:entry_type(),

	% Precise size, in bytes, of that file:
	size :: byte_size(),

	% Timestamp of the last content modification of this file, as known of the
	% filesystem, and as an integer number of seconds since 1970-01-01 00:00
	% UTC:
	%
	timestamp :: posix_seconds(),

	% SHA1 sum (as a binary) of the content of that file:
	sha1_sum :: sha1() } ).

-type file_data() :: #file_data{}.
% Data associated (in-memory) to a given file-like element.



-type sha1_table() :: table( sha1(), [ file_data() ] ).
% Table referencing file entries based on their SHA1.
%
% (a list of exactly one file_data record per SHA1 key, once the tree is
% uniquified)



-type sha1_entry() :: { sha1(), [ file_data() ] }.
% Pair entries of a sha1_table/0.


% Data associated to a content tree.
-record( tree_data, {

	% The full hostname where the corresponding tree exists (as we may check an
	% actual, local tree against a copy of a cache file for a remote tree)
	%
	hostname :: bin_fqdn(),

	% Base, absolute (binary) path of the root of that tree in the filesystem:
	root :: bin_directory_path(),

	% Each key is the SHA1 sum of a file content, each value is a list of the
	% file entries whose content matches that sum (hence are supposed the same).
	%
	entries = table:new() :: sha1_table(),

	% Total count of the regular files found in this tree:
	file_count = 0 :: count(),

	% Total count of the directories found in this tree:
	directory_count = 0 :: count(),

	% Total count of the symbolic links found in this tree:
	symlink_count = 0 :: count(),

	% Total count of the devices found in this tree:
	device_count = 0 :: count(),

	% Total count of the other elements found in this tree:
	other_count = 0 :: count() } ).

-type tree_data() :: #tree_data{}.
% Data associated to a content tree.


-export_type([ file_data/0, tree_data/0 ]).


% As read from merge cache files:
-type file_info() :: { 'file_info', SHA1 :: sha1(), Path :: file_path(),
					   Size :: byte_size(), Timestamp :: posix_seconds() }.


-record( user_state, {

	% File handle (if any) to write logs:
	log_file = undefined :: maybe( file() ) } ).


-type user_state() :: #user_state{}.
% User-related state.



% In order to run from the interpreter rather than through an escript:
-export([ run/0, scan/3, main/1 ]).


-type analyzer_pid() :: pid().
% The PID of an analyzer process.


-type analyzer_ring() :: ring_utils:ring( analyzer_pid() ).
% Ring of analyzer processes.


% This script depends on the 'Myriad' layer, and only on that code.
%
% Note: ensure it is already built first!



% The subdirectory in a tree in which selected content to integrate will be
% transferred:
%
-define( integrate_dir, <<"content_to_integrate">> ).


% To prevent the risk that the user interface is overwhelmed with too many
% entries (leading to UI crash):
%
-define( max_message_header_len, 1500 ).

% For myriad_spawn*:
-include("spawn_utils.hrl").



% Not run anymore as an escript, as raised issues with term_ui (i.e. dialog):
%-define( exec_name, "merge-tree.escript" ).
-define( exec_name, "merge.sh" ).


% Must match the usage below (not easy to use a single define):

-define( base_dir_opt, '-base-dir' ).
-define( reference_opt, '-reference' ).
-define( input_opt, '-input' ).
-define( equalize_opt, '-equalize' ).
-define( scan_opt, '-scan').
-define( rescan_opt, '-rescan').
-define( resync_opt, '-resync').
-define( uniquify_opt, '-uniquify').
-define( check_opt, '-check-against' ).
-define( help_opt, '-help' ).


% @doc Returns the usage help message.
-spec get_usage() -> ustring().
get_usage() ->
	% Format string is a list:
	text_utils:format( lists:flatten( [
	  " Usage of "?exec_name": following operations can be triggered (details below): \n"
	  "   (1) '"?exec_name" -~ts INPUT_TREE -~ts REFERENCE_TREE': merges input tree into reference one\n"
	  "   (2) '"?exec_name" -~ts FIRST_TREE SECOND_TREE': ensure both trees have the same content\n"
	  "   (3) '"?exec_name" -~ts A_TREE': references, once for all, the content in tree in its cache file\n"
	  "   (4) '"?exec_name" -~ts A_TREE': updates the tree cache, regarding new/modified/removed files\n"
	  "   (5) '"?exec_name" -~ts A_TREE': performs a lighter update of the tree cache\n"
	  "   (6) '"?exec_name" -~ts A_TREE': removes duplicates from tree\n"
	  "   (7) '"?exec_name" -~ts A_TREE A_CACHE_FILE': tells the differences in content between a tree and the cache file of another (possibly remote) one, generating delta files to list the contents that differ on either side\n"
	  "   (8) '"?exec_name" -h' or '"?exec_name" -~ts'\n\n"
	  "   Ensures, for the first form, that all the changes in a possibly more up-to-date, \"newer\" tree (INPUT_TREE) are merged back to the reference tree (REFERENCE_TREE), whence the input tree may have derived. Once executed, only a refreshed, complemented reference tree will exist, as the input tree will have been removed: all its original content (i.e. its content that was not already in the reference tree) will have been transferred in the reference tree.\n"
	  "   In the reference tree, in-tree duplicated content will be either kept as it is, or removed as a whole (to keep only one copy thereof), or replaced by symbolic links in order to keep only a single reference version of each actual content.\n"
	  "   At the root of the reference tree, a '", ?merge_cache_filename, "' file will be stored, in order to avoid any later recomputations of the checksums of the files that it contains, should they have not changed. As a result, once a merge is done, the reference tree may contain an uniquified version of the union of the two specified trees, and the input tree will not exist anymore.\n\n"
	  "   For the second form (-~ts option), the content of the two specified trees are equalized, meaning that they will each contain the union of their respective content? Typically useful to maintain mirrors on different disks.\n\n"
	  "   For the third form (-~ts option), the specified tree will simply be inspected for duplicates, and a corresponding '", ?merge_cache_filename, "' file will be created at its root (to be potentially reused by a later operation).\n\n"
	  "   For the fourth form (-~ts option), an attempt to rebuild an updated '", ?merge_cache_filename, "' file will be performed, computing only the checksum of the files that were not already referenced, or whose timestamp or size changed.\n\n"
	  "   For the fifth form (-~ts option), a rebuild even lighter than the previous rescan of '", ?merge_cache_filename, "' will be done, checking only sizes (not timestamps), and updating these timestamps.\n\n"
	  "   For the sixth form (-~ts option), the specified tree will be scanned first (see the corresponding operation), and then the user will be offered various actions regarding found duplicates (being kept as are, or removed, or replaced with symbolic links to a single copy per content), and once done a corresponding up-to-date '", ?merge_cache_filename, "' file will be created at its root (to be potentially reused by a later operation).\n\n"
	  "   For the seventh form (-~ts option), the specified tree will be compared to the one referenced in the cache file, reporting content differences.\n\n"
	  "   For the eighth form (-h or -~ts option), displays this help.\n\n"
	  "   Note that the -~ts A_BASE_DIR option can be specified by the user to designate the base directory of all relative paths mentioned.\n"
	  "   When a cache file is found, it can be either ignored (and thus recreated) or re-used, either as it is or after a weak check, where only file existence, sizes and timestamps are then verified (not checksums)." ] ),
	 [ ?input_opt, ?reference_opt, ?equalize_opt, ?scan_opt, ?rescan_opt,
	   ?resync_opt, ?uniquify_opt, ?check_opt, ?help_opt,
	   ?equalize_opt, ?scan_opt, ?rescan_opt, ?resync_opt, ?uniquify_opt,
	   ?check_opt, ?help_opt, ?base_dir_opt ] ).




% Implementation notes:
%
% We use everywhere here file_utils:bin_join/n instead file_utils:join/n as we
% may have to deal with so-called "raw filenames" that are obtained as binaries
% and shall never be (attempted to be) converted to plain strings (as the
% operation is bound to fail or to result in incorrect, unusable paths).
%
% So, unless specified otherwise, all paths are binaries (and a Bin prefix can
% be spared in their names), not plain strings.

% A way of validating this implementation is to review all calls impacting a
% filesystem and to check that the counterpart view in memory (to be later
% dumped in a cache file) is updated accordingly; to do that, check file_utils
% calls. In practise, it relates mostly to calling the safe_move/{4,5},
% safe_copy/4 local functions (calls to file_utils:remove_* also can be
% inspected).

% This implementation has been intentionally slowed down to avoid risks of
% overheat, based on following measures:
% - less analyzers than cores are spawned (see create_analyzer_ring/1)
% - each analyzer is further slowed down by a sleep delay (see analyze_loop/1)
%
% Remove these safety measures at your own risk!



% @doc Typically for testing.
-spec run() -> void().
run() ->
	ArgTable = shell_utils:get_argument_table(),
	main( ArgTable ).



% @doc Sole entry point for this merge service, either triggered by `run/0' or
% by the associated escript.
%
-spec main( shell_utils:argument_table() ) -> void().
main( ArgTable ) ->

	%trace_bridge:notice( "Running..." ),

	UIOptions = [ log_console ],
	%UIOptions = [],

	FilteredArgTable = ui:start( UIOptions, ArgTable ),

	% A goal here will be to ensure that all filesystem names are promoted to
	% binaries as soon as possible (for proper Unicode encoding and management
	% of the so-called 'raw' elements).

	%trace_bridge:debug_fmt( "Script-specific argument(s): ~ts",
	%   [ shell_utils:argument_table_to_string( FilteredArgTable ) ] ),

	case list_table:has_entry( 'h', FilteredArgTable )
			orelse list_table:has_entry( ?help_opt, FilteredArgTable ) of

		true ->
			display_usage();

		false ->
			{ BaseDir, BaseArgTable } = case
					list_table:extract_entry_with_default( ?base_dir_opt,
						[ file_utils:get_current_directory() ],
						FilteredArgTable ) of

				{ [ [ InputBaseDir ] ], BaseDirArgTable } ->
					% Implied normalisation for example removes any trailing /:
					{ file_utils:ensure_path_is_absolute( InputBaseDir ),
					  BaseDirArgTable };

				{ UnexpectedBaseDirOpts, _BaseDirArgumentTable } ->
					stop_on_option_error( ?base_dir_opt, _ExpectedCount=1,
										  UnexpectedBaseDirOpts, 50 )

			end,

			BinBaseDir = text_utils:ensure_binary( BaseDir ),

			case list_table:extract_entry_with_default( ?reference_opt,
										undefined, BaseArgTable ) of

				{ [ [ RefTreePath ] ], NoRefArgTable }
								when is_list( RefTreePath ) ->
					handle_reference_option( RefTreePath, NoRefArgTable,
											 BinBaseDir );

				{ undefined, NoRefArgTable } ->
					handle_non_reference_option( NoRefArgTable, BinBaseDir );

				% Typically more than one reference option specified:
				{ UnexpectedRefTreeOpts, _NoRefArgTable } ->
					stop_on_option_error( ?reference_opt, _Expected=1,
										  UnexpectedRefTreeOpts, 51 )

			end


	end.



% @doc Handles the command-line whenever the --reference option was specified,
% with a single corresponding parameter, of type list.
%
handle_reference_option( RefTreePath, ArgumentTable, BinBaseDir ) ->

	ui:set_settings( [ { backtitle, "Merging now..." },
					   { title, "Merging" } ] ),

	% If there is a --reference option, it is a merge, and there must be a
	% --input option as well:

	case list_table:extract_entry_with_default( ?input_opt, undefined,
												ArgumentTable ) of

		% Here, an input tree was specified as well:
		{ [ [ InputTreePath ] ], NewArgumentTable }
						when is_list( InputTreePath ) ->
			%trace_bridge:debug_fmt( "InputTreePath: ~p", [ InputTreePath ] ),

			% RefTreePath is already vetted:
			handle_merge_option( InputTreePath, RefTreePath, NewArgumentTable,
								 BinBaseDir );

		% Typically more than one input option specified:
		{ UnexpectedInputTreeOpts, _NewArgumentTable } ->
			stop_on_option_error( ?input_opt, _ExpectedCount=1,
								  UnexpectedInputTreeOpts, 52 )

	end.



% @doc Handles the command-line whenever the --reference option was not
% specified.
%
handle_non_reference_option( ArgumentTable, BinBaseDir ) ->

	% No reference, it must then be an equalize, a pure scan, a rescan, a resync
	% or a uniquify here:
	%
	case list_table:extract_entry_with_default( ?equalize_opt, undefined,
												ArgumentTable ) of

		% Not an equalize, then a scan?
		{ undefined, NoEqualizeArgTable } ->

			case list_table:extract_entry_with_default( ?scan_opt, undefined,
														NoEqualizeArgTable ) of

				% Not a scan, then a rescan?
				{ undefined, NoScanArgTable } ->

					case list_table:extract_entry_with_default( ?rescan_opt,
												undefined, NoScanArgTable ) of

						% Not a rescan either:
						{ undefined, NoRescanArgTable } ->
							handle_neither_scan_options( NoRescanArgTable,
														 BinBaseDir );

							% A rescan was requested:
						{ [ [ RescanTreePath ] ], RescanArgTable }
									when is_list( RescanTreePath ) ->
							handle_rescan_option( RescanTreePath,
												  RescanArgTable, BinBaseDir );

						{ UnexpectedRescanTreeOpts, _RescanArgTable } ->
							stop_on_option_error( ?rescan_opt, _ExpectedCount=1,
												  UnexpectedRescanTreeOpts, 53 )

					end;

				% A scan was requested:
				{ [ [ ScanTreePath ] ], ScanArgTable }
								when is_list( ScanTreePath ) ->

					% Check no unknown option remains:
					case list_table:is_empty( ScanArgTable ) of

						true ->
							handle_scan_option( ScanTreePath, ScanArgTable,
												BinBaseDir );

						false ->
							Msg = text_utils:format(
								"unexpected extra options specified: ~ts",
								[ shell_utils:argument_table_to_string(
										ScanArgTable ) ] ),
							display_error_and_stop( Msg, 54 )

					end;

				{ UnexpectedScanTreeOpts, _ScanArgTable } ->
					stop_on_option_error( ?scan_opt, _Expected=1,
										  UnexpectedScanTreeOpts, 55 )

			end;

		{ [ [ FirstTreePath, SecondTreePath ] ], EqualizeArgTable }
		  when is_list( FirstTreePath ) andalso is_list( SecondTreePath ) ->
			handle_equalize_option( FirstTreePath, SecondTreePath,
									EqualizeArgTable, BinBaseDir );

		{ UnexpectedEqualizeOpts, _EqualizeArgTable } ->
			stop_on_option_error( ?equalize_opt, _ExpectedC=2,
								  UnexpectedEqualizeOpts, 56 )

	end.



handle_neither_scan_options( ArgTable, BinBaseDir ) ->

	% Not a scan or rescan, then a resync, a uniquify or a check-against?

	case list_table:extract_entry_with_default( ?resync_opt, undefined,
												ArgTable ) of

		{ undefined, NoResyncArgTable } ->

			case list_table:extract_entry_with_default( ?uniquify_opt,
											undefined, NoResyncArgTable ) of

				{ undefined, NoUniqArgTable } ->

					case list_table:extract_entry_with_default(
							?check_opt, undefined, NoUniqArgTable ) of

						{ [ [ TreePath, MergeFilePath ] ], CheckArgTable }
										when is_list( TreePath )
											 andalso is_list( MergeFilePath ) ->
							handle_check_against_option( TreePath,
								MergeFilePath, CheckArgTable, BinBaseDir );


						{ undefined, NoCheckArgTable } ->

							AddedString = case
									list_table:is_empty( NoCheckArgTable ) of

								true ->
									" (no command-line option specified)";

								false ->
									"; instead " ++
						shell_utils:argument_table_to_string( NoUniqArgTable )

							end,

							Msg = text_utils:format( "no sensible operation "
								"specified~ts", [ AddedString ] ),

							display_error_and_stop( Msg, 57 );

						{ UnexpectedCheckOpts, _CheckArgTable } ->
							stop_on_option_error( ?check_opt, _ExpectedCount=2,
												  UnexpectedCheckOpts, 58 )

					end;

				{ [ [ UniqTreePath ] ], NoUniqArgTable }
								when is_list( UniqTreePath ) ->
					handle_uniquify_option( UniqTreePath, NoUniqArgTable,
											BinBaseDir );

				{ UnexpectedUniqTreeOpts, _NoUniqArgTable } ->
					stop_on_option_error( ?uniquify_opt, _Expected=1,
										  UnexpectedUniqTreeOpts, 59 )

			end;

		{ [ [ ResyncTreePath ] ], NoResyncArgTable }
							when is_list( ResyncTreePath ) ->
				handle_resync_option( ResyncTreePath, NoResyncArgTable,
									  BinBaseDir );

		{ UnexpectedResyncTreeOpts, _NoResyncArgTable } ->
			stop_on_option_error( ?resync_opt, _ExpectedC=1,
								  UnexpectedResyncTreeOpts, 60 )

	end.



handle_equalize_option( FirstTreePath, SecondTreePath, EqualizeArgTable,
						BinBaseDir ) ->

	BinFirstTreePath = text_utils:ensure_binary( FirstTreePath ),

	BinAbsFirstTreePath =
		file_utils:ensure_path_is_absolute( BinFirstTreePath, BinBaseDir ),

	BinSecondTreePath = text_utils:ensure_binary( SecondTreePath ),

	BinAbsSecondTreePath =
		file_utils:ensure_path_is_absolute( BinSecondTreePath, BinBaseDir ),

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	check_no_option_remains( EqualizeArgTable ),

	% To avoid the mirroring of a tree in itself:
	BinAbsFirstTreePath =:= BinAbsSecondTreePath andalso
		begin
			ui:display_error( "The same tree ('~ts') is specified for both "
				"trees to equalize.", [ BinAbsFirstTreePath ] ),
			throw( { equalize_on_same_directory, BinAbsFirstTreePath } )
		end,

	trace_debug( "Equalizing trees '~ts' and '~ts'...",
				 [ BinAbsFirstTreePath, BinAbsSecondTreePath ], UserState ),


	AnalyzerRing = create_analyzer_ring( UserState ),

	ui:set_settings( [ { backtitle, "Equalizing trees" },
					   { title, "Updating first tree..." } ] ),

	ui:display_instant( "Equalizing trees '~ts' and '~ts'...",
						[ BinAbsFirstTreePath, BinAbsSecondTreePath ] ),

	FirstTree =
		update_content_tree( BinAbsFirstTreePath, AnalyzerRing, UserState ),

	ui:set_setting( title, "Updating second tree..." ),

	SecondTree =
		update_content_tree( BinAbsSecondTreePath, AnalyzerRing, UserState ),

	ui:set_setting( title, "Equalizing in progress..." ),

	{ FirstTreeData, SecondTreeData } =
		equalize( FirstTree, SecondTree, UserState ),


	% Relevant outputs already done:

	%ui:set_setting( title, "Equalize report for first tree" ),

	% Should be the same content-wise, but not necessarily paths- and/or
	% duplicates-wise:
	%
	%display_tree_data( FirstTreeData, text_utils:format(
	%   "Resulting first equalized tree '~ts'",
	%   [ FirstTreeData#tree_data.root ] ), UserState ),

	%ui:set_setting( title, "Equalize report for second tree" ),

	%display_tree_data( SecondTreeData, text_utils:format(
	%   "Resulting second equalized tree '~ts'",
	%   [ SecondTreeData#tree_data.root ] ), UserState ),

	write_cache_file( FirstTreeData, UserState ),
	write_cache_file( SecondTreeData, UserState ),

	terminate_analyzer_ring( AnalyzerRing, UserState ),

	stop_user_service( UserState ).




handle_scan_option( UserScanTreePath, ScanArgTable, BinBaseDir ) ->

	BinUserScanTreePath = text_utils:ensure_binary( UserScanTreePath ),

	BinAbsScanTreePath =
		file_utils:ensure_path_is_absolute( BinUserScanTreePath, BinBaseDir ),

	check_no_option_remains( ScanArgTable ),

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	AnalyzerRing = create_analyzer_ring( UserState ),

	scan( BinAbsScanTreePath, AnalyzerRing, UserState ),

	terminate_analyzer_ring( AnalyzerRing, UserState ),

	stop_user_service( UserState ).



handle_rescan_option( UserRescanTreePath, RescanArgTable, BinBaseDir ) ->

	BinUserRescanTreePath = text_utils:ensure_binary( UserRescanTreePath ),

	BinAbsRescanTreePath =
		file_utils:ensure_path_is_absolute( BinUserRescanTreePath, BinBaseDir ),

	check_no_option_remains( RescanArgTable ),

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	AnalyzerRing = create_analyzer_ring( UserState ),

	% Already written:
	_NewTreeData = rescan( BinAbsRescanTreePath, AnalyzerRing, UserState ),

	terminate_analyzer_ring( AnalyzerRing, UserState ),

	stop_user_service( UserState ).


handle_resync_option( UserResyncTreePath, ResyncArgTable, BinBaseDir ) ->

	BinUserResyncTreePath = text_utils:ensure_binary( UserResyncTreePath ),

	BinAbsResyncTreePath =
		file_utils:ensure_path_is_absolute( BinUserResyncTreePath, BinBaseDir ),

	check_no_option_remains( ResyncArgTable ),

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	AnalyzerRing = create_analyzer_ring( UserState ),

	% Already written:
	_NewTreeData = resync( BinAbsResyncTreePath, AnalyzerRing, UserState ),

	terminate_analyzer_ring( AnalyzerRing, UserState ),

	stop_user_service( UserState ).



handle_uniquify_option( UserUniqTreePath, UniqArgTable, BinBaseDir ) ->

	BinUserUniqTreePath = text_utils:ensure_binary( UserUniqTreePath ),

	BinAbsUniqTreePath =
		file_utils:ensure_path_is_absolute( BinUserUniqTreePath, BinBaseDir ),

	check_no_option_remains( UniqArgTable ),

	uniquify( BinAbsUniqTreePath ).



handle_merge_option( UserInputTreePath, UserRefTreePath, MergeArgTable,
					 BinBaseDir ) ->

	BinUserInputTreePath = text_utils:ensure_binary( UserInputTreePath ),

	BinAbsInputTreePath =
		file_utils:ensure_path_is_absolute( BinUserInputTreePath, BinBaseDir ),


	BinUserRefTreePath = text_utils:ensure_binary( UserRefTreePath ),

	BinAbsRefTreePath =
		file_utils:ensure_path_is_absolute( BinUserRefTreePath, BinBaseDir ),

	check_no_option_remains( MergeArgTable ),

	merge( BinAbsInputTreePath, BinAbsRefTreePath ).



handle_check_against_option( UserTreePath, UserMergeFilePath, CheckArgTable,
							 BinBaseDir ) ->

	BinUserTreePath = text_utils:ensure_binary( UserTreePath ),

	BinAbsTreePath =
		file_utils:ensure_path_is_absolute( BinUserTreePath, BinBaseDir ),


	BinUserMergeFilePath = text_utils:ensure_binary( UserMergeFilePath ),

	BinAbsCachePath =
		file_utils:ensure_path_is_absolute( BinUserMergeFilePath, BinBaseDir ),

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	check_no_option_remains( CheckArgTable ),

	file_utils:is_existing_directory_or_link( BinAbsTreePath ) orelse
		begin
			ui:display_error( "The specified tree ('~ts') does not exist.",
							  [ BinAbsTreePath ] ),
			throw( { non_existing_directory_to_check, BinAbsTreePath } )
		end,

	file_utils:is_existing_file_or_link( BinAbsCachePath ) orelse
		begin
			ui:display_error( "The specified merge cache file ('~ts') does "
							  "not exist.", [ BinAbsCachePath  ] ),
			throw( { non_existing_merge_cache_file, BinAbsCachePath } )
		end,

	AnalyzerRing = create_analyzer_ring( UserState ),

	trace_debug( "Requested to check tree '~ts' against "
		"cache file '~ts'.", [ UserTreePath, UserMergeFilePath ], UserState ),

	check_against( BinAbsTreePath, BinAbsCachePath, AnalyzerRing, UserState ),

	terminate_analyzer_ring( AnalyzerRing, UserState ),

	stop_user_service( UserState ).



check_no_option_remains( ArgTable ) ->
	list_table:is_empty( ArgTable ) orelse
		begin
			Msg = text_utils:format( "unexpected extra options specified: ~ts",
				[ shell_utils:argument_table_to_string( ArgTable ) ] ),
			display_error_and_stop( Msg, 20 )
		end.



% Displays the usage of this service, and stops (with no error).
display_usage() ->
	ui:display( "~ts", [ get_usage() ] ),
	stop( _StatusCode=0 ).



% Reports an error related to command-line option, reminds the usage, and stops
% (on error).
%
-spec stop_on_option_error( command_line_option(), count(),
					[ command_line_value() ], status_code() ) -> no_return().
stop_on_option_error( Option, ExpectedParamCount, Params, ErrorCode ) ->

	% Parameters are lists of strings:
	Message = case list_utils:flatten_once( Params ) of

		[] ->
			text_utils:format( "no parameter specified for the '-~ts' option, "
				"whereas expecting ~B of them",
				[ Option, ExpectedParamCount ] );

		[ Param ] ->
			text_utils:format( "a single parameter specified for "
				"the '~ts' option, '~ts', whereas expecting ~B of them",
				[ Option, Param, ExpectedParamCount ] );

		ParamList ->
			text_utils:format( "~B parameters specified for the '~ts' "
				"option (~ts), whereas expecting exactly ~B of them",
				[ length( ParamList), Option,
				  text_utils:strings_to_listed_string(
					text_utils:double_quote_strings( ParamList ) ),
				  ExpectedParamCount ] )

	end,

	display_error_and_stop( Message, ErrorCode ).



-spec display_error_and_stop( ustring(), status_code() ) -> no_return().
display_error_and_stop( Message, ErrorCode ) ->
	ui:display_error( "Error, ~ts.~n~n~ts", [ Message, get_usage() ] ),
	stop( ErrorCode ).



% @doc Stops whereas no user state is available.
%
% (helper)
%
-spec stop( basic_utils:status_code() ) -> no_return().
stop( StatusCode ) ->
	%trace_utils:debug( "Direct stop." ),
	ui:stop(),
	basic_utils:stop( StatusCode ).



% @doc Scans specified tree, returning the corresponding datastructure.
-spec scan( bin_directory_path(), analyzer_ring(), user_state() ) ->
					tree_data().
scan( BinTreePath, AnalyzerRing, UserState ) ->

	% TreePath expected to be already absolute and normalised.

	trace_debug( "Requested to scan '~ts'.", [ BinTreePath ], UserState ),

	ui:set_settings( [ { backtitle,
						 text_utils:format( "Scan of ~ts", [ BinTreePath ] ) },
					   { title, "Scan in progress..." } ] ),

	CacheFilename = get_cache_path_for( BinTreePath ),

	{ Prompt, TreeData } = case file_utils:is_existing_file( CacheFilename ) of

		true ->

			ChoicePrompt = text_utils:format(
				"A cache file already exists for '~ts'. We can:",
				[ BinTreePath ] ),

			% No 'strong_check' deemed useful (synonym of recreating from
			% scratch, hence of 'ignore').

			Choices = [
				{ weak_check, "Re-use this file, provided that it passes "
				  "a weak check (based on sizes and timestamps), otherwise "
				  "recreate it" },
				{ ignore, "Ignore this version, and recreate this file "
				  "unconditionally" },
				{ no_check, "Re-use this file as it is, with no specific "
				  "check involved (not recommended)" },
				{ abort, "Abort scan" } ],

			ReadTreeData = case ui:choose_designated_item( ChoicePrompt,
														   Choices ) of

				weak_check ->
					% No need to restate the tree, is in the path of the cache
					% file:
					%
					%ui:display( "Performing a weak check of '~ts'.",
					%            [ CacheFilename ] ),
					UpTreeData = update_content_tree( BinTreePath, AnalyzerRing,
													  UserState ),

					% We leave an up-to-date cache file (ex: if a mismatching
					% root directory had to be updated):
					%
					write_cache_file( UpTreeData, UserState ),

					UpTreeData;


				ignore ->
					ui:display_instant( "Ignoring existing cache file (~ts), "
						"performing now a full scan to recreate it... "
						"(might be long)", [ CacheFilename ] ),
					perform_scan( BinTreePath, AnalyzerRing, UserState );

				no_check ->
					%ui:display( "Re-using '~ts' with no specific check.",
					%            [ CacheFilename ] ),
					read_cache_file( CacheFilename, UserState );

				C when C =:= abort orelse C =:= ui_cancel ->
					ui:unset_setting( title ),
					ui:display( "Scan aborted, cache file (~ts) left "
						"as it was.", [ CacheFilename ] ),
					trace_debug( "(requested to abort the scan)", UserState ),
					stop_user_service( UserState )

			end,

			ReadPrompt = text_utils:format( "Scan result read from '~ts'",
											[ CacheFilename ] ),

			{ ReadPrompt, ReadTreeData };


		false ->
			display_scan_notification( CacheFilename ),

			ScanTreeData = perform_scan( BinTreePath, AnalyzerRing, UserState ),

			ScanPrompt = text_utils:format( "Scan result for '~ts'",
											[ BinTreePath ] ),

			{ ScanPrompt, ScanTreeData }


	end,

	ui:set_setting( title, "Scan report" ),
	display_tree_data( TreeData, Prompt, UserState ),
	TreeData.



% (helper)
perform_scan( TreePath, AnalyzerRing, UserState ) ->

	TreeData = scan_helper( TreePath, AnalyzerRing, UserState ),

	%ui:display( "Scan result stored in '~ts': ~ts",
	%            [ CacheFilename, tree_data_to_string( TreeData ) ] ),

	TreeData.



% @doc Rescans specified tree (as an absolute directory), returning the
% corresponding datastructure.
%
-spec rescan( directory_path(), analyzer_ring(), user_state() ) -> tree_data().
rescan( BinTreePath, AnalyzerRing, UserState ) ->

	% TreePath expected to be already absolute and normalised.

	trace_debug( "Requested to rescan '~ts'.", [ BinTreePath ], UserState ),

	ui:set_settings( [ { backtitle,
					 text_utils:format( "Rescan of ~ts", [ BinTreePath ] ) },
					   { title, "Rescan in progress..." } ] ),

	file_utils:is_existing_directory_or_link( BinTreePath ) orelse
		begin
			ui:display_error( "Specified tree to rescan ('~ts') is not "
				"an existing directory; aborting now.", [ BinTreePath ] ),
			throw( { non_existing_directory_to_rescan, BinTreePath } )
		end,

	CacheFilename = get_cache_path_for( BinTreePath ),

	NewTreeData = case file_utils:is_existing_file( CacheFilename ) of

		true ->
			{ TreeData, Notifications } =
				perform_rescan( BinTreePath, CacheFilename, AnalyzerRing,
								UserState ),

			ui:set_setting( title, "Rescan report" ),

			report_notifications( Notifications, UserState ),

			RescanPrompt = text_utils:format( "Rescan result for '~ts'",
											  [ BinTreePath ] ),

			display_tree_data( TreeData, RescanPrompt, UserState ),

			trace_debug( "Rescanned tree: ~ts",
				[ tree_data_to_string( TreeData, _Verbose=true ) ], UserState ),

			TreeData;


		false ->
			display_scan_notification( CacheFilename ),

			TreeData = perform_scan( BinTreePath, AnalyzerRing, UserState ),

			ui:set_setting( title, "Rescan report" ),

			ScanPrompt = text_utils:format( "Full scan result for '~ts'",
											[ BinTreePath ] ),

			display_tree_data( TreeData, ScanPrompt, UserState ),

			TreeData

	end,

	% We now write/update in all cases a newly determined cache file:
	write_cache_file( NewTreeData, UserState ),

	NewTreeData.




% @doc Performs a rescan and returns {TreeData, Notifications}; does not write
% the corresponding cache file.
%
% (helper)
%
perform_rescan( BinUserTreePath, CacheFilePath, AnalyzerRing, UserState ) ->

	CacheTimestamp = file_utils:get_last_modification_time( CacheFilePath ),

	ReadTreeData=#tree_data{ root=BinCachedTreePath, entries=SHA1Table } =
		read_cache_file( CacheFilePath, UserState ),

	ActualBinTreePath = case BinUserTreePath of

		BinCachedTreePath ->
			BinCachedTreePath;

		_ ->
			Diagnosis = text_utils:format(
				"Root path in cache filename ('~ts') does not match "
				"actual tree to rescan: read as '~ts', whereas user-specified "
				"as '~ts'.",
				[ CacheFilePath, BinCachedTreePath, BinUserTreePath ] ),

			% text_utils:format/2 necessary for newlines:
			UpdatePrompt = Diagnosis ++ text_utils:format(
				"~n~nShall it be automatically updated to the actual one?~n"
				"(otherwise the rescan will stop on failure)~n~n"
				"Such an update is typically relevant if this "
				"tree has been moved since the last inspection.", [] ),

			case ui:ask_yes_no( UpdatePrompt, _BinaryDefault=no ) of

				yes ->
					BinUserTreePath;

				no ->
					trace_utils:error( Diagnosis
						++ ", and no automatic renaming was accepted." ),

					throw( { mismatching_paths,
							 text_utils:binary_to_string( BinCachedTreePath ),
							 text_utils:binary_to_string( BinUserTreePath ) } )

			end

	end,

	ObtainedTreeData = ReadTreeData#tree_data{ root=ActualBinTreePath },

	ui:display_instant( "Rescanning '~ts'...", [ ActualBinTreePath ] ),
	trace_debug( "Rescanning tree '~ts'...", [ ActualBinTreePath ], UserState ),

	% Relative to specified path:
	AllFiles = find_regular_files_from( ActualBinTreePath ),

	% Not wanting to index our own files (if any already exists):
	FilteredFiles = lists:delete( ?merge_cache_filename, AllFiles ),

	trace_debug( "Found in filesystem ~B files to rescan: ~ts~n(end of list)~n",
				 [ length( FilteredFiles ),
				   text_utils:strings_to_string( FilteredFiles ) ], UserState ),

	% For lighter message sendings and storage (raw filenames are already
	% binaries):
	%
	FilteredBinFiles = text_utils:ensure_binaries( FilteredFiles ),

	rescan_files( _FileSet=set_utils:from_list( FilteredBinFiles ),
		table:enumerate( SHA1Table ), ObtainedTreeData,
		ActualBinTreePath, AnalyzerRing, CacheTimestamp, _Notifications=[],
		UserState ).



% @doc Rescans specified content files, using for that the specified analyzers,
% returning the corresponding tree data.
%
-spec rescan_files( set( bin_file_path() ), [ sha1_entry() ],
		tree_data(), bin_directory_path(), analyzer_ring(), posix_seconds(),
		[ ustring() ], user_state() ) -> { tree_data(), [ ustring() ] }.
% All known entries exhausted; maybe extra files were in the filesystem:
rescan_files( FileSet, _Entries=[], TreeData, BinTreePath, AnalyzerRing,
			  _CacheTimestamp, Notifications, UserState ) ->

	case set_utils:to_list( FileSet ) of

		[] ->
			trace_debug( "No extra file found during rescan.", UserState ),
			% Returning directly the updated tree:
			{ TreeData, Notifications };


		ExtraFiles ->

			trace_debug( "Found ~B extra files during rescan that will be "
				"checked now: ~ts", [ length( ExtraFiles ),
					text_utils:binaries_to_binary( ExtraFiles ) ],
				UserState ),

			% Let's have the workers check these extra files (new ring not
			% kept):
			%
			lists:foldl(
				fun( Filename, AccRing ) ->
					{ AnalyzerPid, NewAccRing } = ring_utils:head( AccRing ),
					trace_debug( "Checking new file ~ts", [ Filename ],
								 UserState ),
					AnalyzerPid !
						{ checkNewFile, [ BinTreePath, Filename ], self() },
					NewAccRing
				end,
				_Acc0=AnalyzerRing,
				_List=ExtraFiles ),

			% Waiting for all corresponding file_data elements:
			ExtraFileDatas = lists:foldl(
				fun( _Count, AccFileDatas ) ->
					receive

						{ file_checked, FileData } ->
						  [ FileData | AccFileDatas ]

					end
				end,
				_SecondAcc0=[],
				_SecondList=lists:seq( 1, length( ExtraFiles ) ) ),

			ExtraNotif = text_utils:format( "following ~B extra files were "
				"added (not referenced yet): ~ts",
				[ length( ExtraFiles ),
				  text_utils:binaries_to_binary( ExtraFiles,
												 _Bullet=" + " ) ] ),

			% Here we have a list of data of the files that were not referenced
			% yet; returns an updated tree:
			%
			{ integrate_extra_files( ExtraFileDatas, TreeData, UserState ),
			  [ ExtraNotif | Notifications ] }

	end;

% Extracting next recorded file_data elements:
rescan_files( FileSet, _Entries=[ { SHA1, FileDatas } | T ], TreeData,
		BinTreePath, AnalyzerRing, CacheTimestamp, Notifications, UserState ) ->

	% Not using a ring for punctual updates:
	{ NewFileSet, NewTreeData, ExtraNotifications } =
		check_file_datas_for_scan( FileDatas, SHA1, FileSet, TreeData,
			BinTreePath, _NewFileDatas=[], _ExtraNotifications=[], UserState ),

	rescan_files( NewFileSet, T, NewTreeData, BinTreePath, AnalyzerRing,
		CacheTimestamp, ExtraNotifications ++ Notifications, UserState ).



% @doc Integrates specified file entries into specified tree data.
-spec integrate_extra_files( [ file_data() ], tree_data(), user_state() ) ->
									tree_data().
integrate_extra_files( _ExtraFileDatas=[], TreeData, _UserState ) ->
	TreeData;

integrate_extra_files(
  _ExtraFileDatas=[ FileData=#file_data{ path=FilePath, sha1_sum=SHA1 } | T ],
  TreeData=#tree_data{ entries=Entries,
					   file_count=FileCount },
  UserState ) ->

	NewFileDatas = case table:lookup_entry( SHA1, Entries ) of

		key_not_found ->
			trace_debug( "Extra file '~ts' has a unique content.",
						 [ FilePath ], UserState ),
			[ FileData ];

		{ value, FileDatas } ->
			trace_debug( "Extra file '~ts' is a duplicate of a content "
				"corresponding now to ~B files.",
				[ FilePath, length( FileDatas ) + 1 ], UserState ),
			[ FileData | FileDatas ]

	end,

	NewEntries = table:add_entry( SHA1, NewFileDatas, Entries ),

	NewTreeData = TreeData#tree_data{ entries=NewEntries,
									  file_count=FileCount+1 },

	integrate_extra_files( T, NewTreeData, UserState ).



% @doc Checks whether the file data elements seem up to date: still existing,
% not more recent than cache filename, and of the same size as referenced.
%
check_file_datas_for_scan( _FileDatas=[], SHA1, FileSet,
		TreeData=#tree_data{ entries=PrevEntries,
							 file_count=PrevFileCount },
		_BinTreePath, FileDatas, ExtraNotifications, _UserState ) ->

	NewEntryCount = length( FileDatas ),

	OldEntryCount = length( table:get_value( SHA1, PrevEntries ) ),

	DiffEntryCount = NewEntryCount - OldEntryCount,

	% To be replenished through FileDatas:
	WipedEntries = table:remove_entry( SHA1, PrevEntries ),

	% We should not assign the elements in FileDatas to SHA1 - their checksum
	% might differ now! So:

	NewEntries = lists:foldl(
		fun( FD=#file_data{ sha1_sum=ThisSHA1 }, AccEntries ) ->
			table:append_to_entry( ThisSHA1, FD, AccEntries )
		end,
		_Acc0=WipedEntries,
		_List=FileDatas ),

	NewTreeData = TreeData#tree_data{ entries=NewEntries,
									  file_count=PrevFileCount+DiffEntryCount },

	{ FileSet, NewTreeData, ExtraNotifications };

% Take into account only regular files:
check_file_datas_for_scan( _FileDatas=[
					FileData=#file_data{ path=RelativeBinFilename,
										 type=regular,
										 size=RecordedSize,
										 timestamp=RecordedTimestamp,
										 sha1_sum=SHA1 } | T ],
						   SHA1, FileSet, TreeData, BinTreePath, FileDatas,
						   ExtraNotifications, UserState ) ->

	FullPath = file_utils:bin_join( BinTreePath, RelativeBinFilename ),

	case set_utils:extract_if_existing( RelativeBinFilename, FileSet ) of

		% File not found anymore:
		false ->
			NewNotif = case file_utils:is_existing_link( FullPath ) of

				true ->
					text_utils:format( "regular file '~ts' was replaced "
						"in tree by a symbolic link", [ FullPath ] );

				false ->
					text_utils:format( "no file element '~ts' in tree anymore",
									   [ FullPath ] )

			end,

			% Let's forget this file_data then:
			check_file_datas_for_scan( T, SHA1, FileSet, TreeData, BinTreePath,
				FileDatas, [ NewNotif | ExtraNotifications ], UserState );

		% Here the iterated file still exists as a regular one, let's check
		% whether the other information are still valid:
		%
		ShrunkFileSet ->
			{ UpdatedFileData, UpdatedNotifs } = case
					file_utils:get_last_modification_time( FullPath ) of

				% Time matches here, maybe size as well:
				RecordedTimestamp ->
					case file_utils:get_size( FullPath ) of

						% Same size, in the context of a (light) rescan we
						% consider that the SHA1 must be the same as well then:
						%
						RecordedSize ->
							{ FileData, ExtraNotifications };

						% Different size, recreating the record from scratch:
						OtherSize ->
							NewNotif = text_utils:format(
								"file '~ts' had a different size (moved "
								"from '~ts' to '~ts'), it has thus been "
								"reindexed.",
								[ FullPath, system_utils:interpret_byte_size(
														RecordedSize ),
								  system_utils:interpret_byte_size( OtherSize )
								] ),

							trace_debug( NewNotif, UserState ),

							% Recreating the record from scratch:
							NewFileData = #file_data{
						path=RelativeBinFilename,
						type=regular,
						size=OtherSize,
						timestamp=RecordedTimestamp,
						sha1_sum=hash_utils:compute_file_sha1_sum( FullPath ) },

							{ NewFileData, [ NewNotif | ExtraNotifications ] }

					end;


				% Time does not match here, must have been altered:
				OtherTimestamp ->

					NewNotif = text_utils:format( "file '~ts' had a different "
						"timestamp, it has thus been reindexed.",
						[ FullPath ] ),

					trace_debug( NewNotif, UserState ),

					% Recreating the record from scratch:
					NewFileData = #file_data{
						path=RelativeBinFilename,
						type=regular,
						size=file_utils:get_size( FullPath ),
						timestamp=OtherTimestamp,
						sha1_sum=hash_utils:compute_file_sha1_sum( FullPath ) },

					{ NewFileData, [ NewNotif | ExtraNotifications ] }

			end,

			check_file_datas_for_scan( T, SHA1, ShrunkFileSet, TreeData,
				BinTreePath, [ UpdatedFileData | FileDatas ], UpdatedNotifs,
				UserState )

	end.



% @doc Resyncs specified tree (as an absolute directory), returning the
% corresponding datastructure.
%
-spec resync( bin_directory_path(), analyzer_ring(), user_state() ) ->
					tree_data().
resync( BinTreePath, AnalyzerRing, UserState ) ->

	% BinTreePath expected to be already absolute and normalised.

	trace_debug( "Requested to resync '~ts'.", [ BinTreePath ], UserState ),

	ui:set_settings( [ { backtitle,
					 text_utils:format( "Resync of ~ts", [ BinTreePath ] ) },
					   { title, "Resync in progress..." } ] ),

	file_utils:is_existing_directory_or_link( BinTreePath ) orelse
		begin
			ui:display_error( "Specified tree to resync ('~ts') is not "
				"an existing directory; aborting now.", [ BinTreePath ] ),
			throw( { non_existing_directory_to_resync, BinTreePath } )
		end,

	CacheFilename = get_cache_path_for( BinTreePath ),

	NewTreeData = case file_utils:is_existing_file( CacheFilename ) of

		true ->
			{ TreeData, Notifications } = perform_resync( BinTreePath,
									CacheFilename, AnalyzerRing, UserState ),

			ui:set_setting( title, "Resync report" ),

			report_notifications( Notifications, UserState ),

			ResyncPrompt =
				text_utils:format( "Resync result for '~ts'", [ BinTreePath ] ),

			display_tree_data( TreeData, ResyncPrompt, UserState ),

			trace_debug( "Resynchronised tree: ~ts",
				[ tree_data_to_string( TreeData, _Verbose=true ) ], UserState ),

			TreeData;


		false ->
			display_scan_notification( CacheFilename ),

			TreeData = perform_scan( BinTreePath, AnalyzerRing, UserState ),

			ui:set_setting( title, "Resync report" ),

			ScanPrompt = text_utils:format( "Full resync result for '~ts'",
											[ BinTreePath ] ),

			display_tree_data( TreeData, ScanPrompt, UserState ),

			TreeData

	end,

	% We now write/update in all cases a newly-determined cache file:
	write_cache_file( NewTreeData, UserState ),

	NewTreeData.




% @doc Performs a resync and returns {TreeData, Notifications}; does not write
% the corresponding cache file.
%
% (helper)
%
perform_resync( BinUserTreePath, CacheFilePath, AnalyzerRing, UserState ) ->

	% Very similar to perform_rescan/4:

	ReadTreeData = #tree_data{ root=BinCachedTreePath, entries=SHA1Table } =
		read_cache_file( CacheFilePath, UserState ),

	ActualBinTreePath = case BinUserTreePath of

		BinCachedTreePath ->
			BinCachedTreePath;

		_ ->

			% Same as for 'rescan' except for the operation name:

			Diagnosis = text_utils:format(
				"Root path in cache filename ('~ts') does not match "
				"actual tree to resync: read as '~ts', whereas user-specified "
				"as '~ts'",
				[ CacheFilePath, BinCachedTreePath, BinUserTreePath ] ),

			% text_utils:format/2 necessary for newlines:
			UpdatePrompt = Diagnosis ++ text_utils:format(
				".~n~nShall it be automatically updated to the actual one?~n"
				"(otherwise the resync will stop on failure)~n~n"
				"Such an update is typically relevant if this "
				"tree has been moved since the last inspection.", [] ),

			case ui:ask_yes_no( UpdatePrompt, _BinaryDefault=no ) of

				yes ->
					BinUserTreePath;

				no ->
					trace_utils:error( Diagnosis
						++ ", and no automatic renaming was accepted." ),

					% String conversion could fail:
					throw( { mismatching_paths, BinCachedTreePath,
							 BinUserTreePath } )

			end

	end,

	ObtainedTreeData = ReadTreeData#tree_data{ root=ActualBinTreePath },

	ui:display_instant( "Resynchronising '~ts'...", [ ActualBinTreePath ] ),
	trace_debug( "Resynchronising tree '~ts'...", [ ActualBinTreePath ],
				 UserState ),

	% Relative to specified path:
	AllFiles = find_regular_files_from( ActualBinTreePath ),

	% Not wanting to index our own files (if any already exists):
	FilteredFiles = lists:delete( ?merge_cache_filename, AllFiles ),

	trace_debug( "Found in filesystem ~B files to resync: ~ts~n(end of list)~n",
				 [ length( FilteredFiles ),
				   text_utils:strings_to_string( FilteredFiles ) ], UserState ),

	% For lighter message sendings and storage (raw filenames are already
	% binaries):
	%
	FilteredBinFiles = text_utils:ensure_binaries( FilteredFiles ),

	resync_files( _FileSet=set_utils:from_list( FilteredBinFiles ),
		table:enumerate( SHA1Table ), ObtainedTreeData,
		ActualBinTreePath, AnalyzerRing, _Notifications=[], UserState ).



% @doc Resyncs specified content files, using for that the specified analyzers,
% returning the corresponding tree data.
%
-spec resync_files( set( bin_file_path() ), [ sha1_entry() ],
		tree_data(), bin_directory_path(), analyzer_ring(), [ ustring() ],
		user_state() ) -> { tree_data(), [ ustring() ] }.
% All known entries exhausted; maybe extra files were in the filesystem:
resync_files( FileSet, _Entries=[], TreeData, BinTreePath, AnalyzerRing,
			  Notifications, UserState ) ->

	case set_utils:to_list( FileSet ) of

		[] ->
			trace_debug( "No extra file found during resync.", UserState ),
			% Returning directly the updated tree:
			{ TreeData, Notifications };


		ExtraFiles ->
			trace_debug( "Found ~B extra files during resync that will be "
				"checked now: ~ts", [ length( ExtraFiles ),
					text_utils:binaries_to_binary( ExtraFiles ) ],
				UserState ),

			% Let's have the workers check these extra files (new ring not
			% kept):
			%
			lists:foldl(
				fun( Filename, AccRing ) ->
					{ AnalyzerPid, NewAccRing } = ring_utils:head( AccRing ),
					AnalyzerPid !
						{ checkNewFile, [ BinTreePath, Filename ], self() },
					NewAccRing
				end,
				_Acc0=AnalyzerRing,
				_List=ExtraFiles ),

			% Waiting for all corresponding file_data elements:
			ExtraFileDatas = lists:foldl(
				fun( _Count, AccFileDatas ) ->
					receive

						{ file_checked, FileData } ->
							[ FileData | AccFileDatas ]

					end
				end,
				_SecondAcc0=[],
				_SecondList=lists:seq( 1, length( ExtraFiles ) ) ),

			ExtraNotif = text_utils:format( "following ~B extra files were "
				"added (not referenced yet): ~ts",
				[ length( ExtraFiles ),
				  text_utils:binaries_to_binary( ExtraFiles, _Indent=2 ) ] ),

			% Here we have a list of data of the files that were not referenced
			% yet; returns an updated tree:
			%
			{ integrate_extra_files( ExtraFileDatas, TreeData, UserState ),
			  [ ExtraNotif | Notifications ] }

	end;

% Extracting next recorded file_data elements:
resync_files( FileSet, _Entries=[ { SHA1, FileDatas } | T ], TreeData,
			  BinTreePath, AnalyzerRing, Notifications, UserState ) ->

	% Not using a ring for punctual updates:
	{ NewFileSet, NewTreeData, ExtraNotifications } =
		check_file_datas_for_sync( FileDatas, SHA1, FileSet, TreeData,
			BinTreePath, _NewFileDatas=[], _ExtraNotifications=[], UserState ),

	resync_files( NewFileSet, T, NewTreeData, BinTreePath, AnalyzerRing,
				  ExtraNotifications ++ Notifications, UserState ).



% @doc Checks whether the file data elements seem up to date: still existing and
% of the same size as referenced (timestamp ignored on purpose, not compared to
% cache filename).
%
check_file_datas_for_sync( _FileDatas=[], SHA1, FileSet,
		TreeData=#tree_data{ entries=PrevEntries,
							 file_count=PrevFileCount },
		_BinTreePath, NewFileDatas, ExtraNotifications, _UserState ) ->

	NewEntryCount = length( NewFileDatas ),

	OldEntryCount = length( table:get_value( SHA1, PrevEntries ) ),

	DiffEntryCount = NewEntryCount - OldEntryCount,

	% To be replenished through FileDatas:
	WipedEntries = table:remove_entry( SHA1, PrevEntries ),

	% We should not assign the elements in NewFileDatas to SHA1 - their checksum
	% might differ now! So:

	NewEntries = lists:foldl(
		fun( FD=#file_data{ sha1_sum=ThisSHA1 }, AccEntries ) ->
			table:append_to_entry( ThisSHA1, FD, AccEntries )
		end,
		_Acc0=WipedEntries,
		_List=NewFileDatas ),

	NewTreeData = TreeData#tree_data{ entries=NewEntries,
									  file_count=PrevFileCount+DiffEntryCount },

	{ FileSet, NewTreeData, ExtraNotifications };

% Take into account only regular files:
check_file_datas_for_sync( _FileDatas=[
					FileData=#file_data{ path=RelativeBinFilename,
										 type=regular,
										 size=RecordedSize,
										 sha1_sum=SHA1 } | T ],
						   SHA1, FileSet, TreeData, BinTreePath, NewFileDatas,
						   ExtraNotifications, UserState ) ->

	FullPath = file_utils:bin_join( BinTreePath, RelativeBinFilename ),

	case set_utils:extract_if_existing( RelativeBinFilename, FileSet ) of

		% File not found anymore:
		false ->
			NewNotif = case file_utils:is_existing_link( FullPath ) of

				true ->
					text_utils:format( "regular file '~ts' was replaced "
						"in tree by a symbolic link", [ FullPath ] );

				false ->
					text_utils:format( "no file element '~ts' in tree anymore",
									   [ FullPath ] )

			end,

			% Let's forget this file_data then:
			check_file_datas_for_sync( T, SHA1, FileSet, TreeData, BinTreePath,
				NewFileDatas, [ NewNotif | ExtraNotifications ], UserState );

		% Here the iterated file still exists as a regular one, let's check
		% whether the size information is still valid (timestamp not taken into
		% account here):
		%
		ShrunkFileSet ->
			{ UpdatedFileData, UpdatedNotifs } =
				case file_utils:get_size( FullPath ) of

						% Same size, in the context of a (light) resync we
						% consider that the SHA1 must be the same as well then:
						%
						RecordedSize ->
							{ FileData, ExtraNotifications };

						% Different size, recreating the record from scratch:
						OtherSize ->
							NewNotif = text_utils:format(
								"file '~ts' had a different size (moved "
								"from '~ts' to '~ts'), it has thus been "
								"reindexed.",
								[ FullPath, system_utils:interpret_byte_size(
												RecordedSize ),
								  system_utils:interpret_byte_size( OtherSize )
								] ),

							trace_debug( NewNotif, UserState ),

							% Recreating the record from scratch:
							NewFileData = #file_data{
						path=RelativeBinFilename,
						type=regular,
						size=OtherSize,
						timestamp=
							file_utils:get_last_modification_time( FullPath ),
						sha1_sum=hash_utils:compute_file_sha1_sum( FullPath ) },

							{ NewFileData, [ NewNotif | ExtraNotifications ] }

				end,

			check_file_datas_for_sync( T, SHA1, ShrunkFileSet, TreeData,
				BinTreePath, [ UpdatedFileData | NewFileDatas ], UpdatedNotifs,
				UserState )

	end;

check_file_datas_for_sync( _FileDatas=[ #file_data{ path=RelativeBinFilename,
													sha1_sum=SHA1 } | _T ],
						   OtherSHA1, _FileSet, _TreeData, _BinTreePath,
						   _NewFileDatas, _ExtraNotifications, _UserState ) ->
	throw( { unmatching_sha1, SHA1, OtherSHA1, RelativeBinFilename } ).



% (helper)
-spec create_analyzer_ring( user_state() ) -> ring_utils:ring( analyzer_pid() ).
create_analyzer_ring( UserState ) ->

	% Best, reasonable CPU usage (no CPU melting):
	SpawnCount = system_utils:get_core_count(),
	%SpawnCount = max( 1, system_utils:get_core_count() -1 ),

	Analyzers = spawn_data_analyzers( SpawnCount, UserState ),

	trace_debug( "Spawned ~B data analyzers: ~w.",
				 [ SpawnCount, Analyzers ], UserState ),

	% Returns the ring:
	ring_utils:from_list( Analyzers ).



% @doc Actual scanning of specified path, producing specified cache file from
% scratch.
%
scan_helper( TreePath, AnalyzerRing, UserState ) ->

	TreeData = create_merge_cache_file_for( TreePath, AnalyzerRing, UserState ),

	trace_debug( "Scan finished.", UserState ),

	terminate_analyzer_ring( AnalyzerRing, UserState ),

	TreeData.



% @doc Uniquifies specified tree.
-spec uniquify( directory_path() ) -> void().
uniquify( TreePath ) ->

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	trace_debug( "Requested to uniquify '~ts'.", [ TreePath ], UserState ),

	AbsTreePath = file_utils:ensure_path_is_absolute( TreePath ),

	ui:set_settings( [ { backtitle,
				text_utils:format( "Uniquification of ~ts", [ AbsTreePath ] ) },
					   { title, "Uniquification in progress..." } ] ),

	ui:display_instant( "Uniquifying '~ts'...", [ AbsTreePath ] ),

	AnalyzerRing = create_analyzer_ring( UserState ),

	TreeData = update_content_tree( AbsTreePath, AnalyzerRing, UserState ),

	NewTreeData = deduplicate_tree( TreeData, UserState ),

	ui:set_setting( title, "Uniquification report" ),

	trace_debug( "Uniquification finished, resulting on following tree: ~ts",
				 [ tree_data_to_string( NewTreeData ) ], UserState ),

	Prompt = text_utils:format( "Information about the resulting uniquified "
								"tree '~ts'", [ NewTreeData#tree_data.root ] ),

	display_tree_data( NewTreeData, Prompt, UserState ),

	terminate_analyzer_ring( AnalyzerRing, UserState ),

	% We leave an up-to-date cache file:
	write_cache_file( NewTreeData, UserState ),

	stop_user_service( UserState ).



% @doc Merges the (supposedly more up-to-date) input tree into the target,
% reference one (both supposed to be absolute).
%
-spec merge( bin_directory_path(), bin_directory_path() ) -> void().
merge( InputTreePath, ReferenceTreePath ) ->

	% Prepare for various outputs:
	UserState = start_user_service( ?default_log_filename ),

	% To avoid annihilation of a tree by itself:
	InputTreePath =:= ReferenceTreePath andalso
		begin
			ui:display_error( "The same tree ('~ts') is specified both as "
				"merge reference and input.", [ ReferenceTreePath ] ),
			throw( { merge_on_same_directory, ReferenceTreePath } )
		end,

	trace_debug(
		"Merging (possibly newer) tree '~ts' into reference tree '~ts'...",
		[ InputTreePath, ReferenceTreePath ], UserState ),

	check_content_trees( InputTreePath, ReferenceTreePath ),

	AnalyzerRing = create_analyzer_ring( UserState ),

	ui:set_settings( [ { backtitle,
				text_utils:format( "Merge in ~ts", [ ReferenceTreePath ] ) },
					   { title, "Updating cache of input tree..." } ] ),

	ui:display_instant( "Merging in '~ts'...", [ ReferenceTreePath ] ),

	InputTree = update_content_tree( InputTreePath, AnalyzerRing, UserState ),

	ui:set_setting( title, "Updating cache of reference tree..." ),

	ReferenceTree =
		update_content_tree( ReferenceTreePath, AnalyzerRing, UserState ),

	ui:set_setting( title, "Merge in progress..." ),

	MergeTreeData = merge_trees( InputTree, ReferenceTree, UserState ),

	ui:set_setting( title, "Merge report" ),

	Prompt = text_utils:format( "Resulting merged tree '~ts'",
								[ ReferenceTreePath ] ),

	display_tree_data( MergeTreeData, Prompt, UserState ),

	write_cache_file( MergeTreeData, UserState ),

	terminate_analyzer_ring( AnalyzerRing, UserState ),

	stop_user_service( UserState ).



% @doc Merges the specified input tree into the reference one, returning the
% latter once updated.
%
-spec merge_trees( tree_data(), tree_data(), user_state() ) -> tree_data().
merge_trees( InputTree=#tree_data{ root=InputRootDir,
								   entries=InputEntries },
			 ReferenceTree=#tree_data{ root=ReferenceRootDir,
									   entries=ReferenceEntries },
			 UserState ) ->

	InputSHA1Set = set_utils:new( table:keys( InputEntries ) ),
	ReferenceSHA1Set = set_utils:new( table:keys( ReferenceEntries ) ),

	% Returns the elements in input but not in references:
	LackingInRefSet = set_utils:difference( InputSHA1Set, ReferenceSHA1Set ),

	ui:set_setting( backtitle,
			text_utils:format( "Merging in ~ts...", [ ReferenceRootDir ] ) ),

	case set_utils:size( LackingInRefSet ) of

		0 ->
			ui:display( "The content of the input tree path ('~ts') is "
				"strictly included into the one of the reference tree ('~ts'), "
				"hence nothing special is to merge, removing directly "
				"the input tree.",
				[ InputRootDir, ReferenceRootDir ] ),

			remove_tree( InputRootDir, UserState ),

			% File count expected to be already correct:
			ReferenceTree;


		LackingCount ->

			% We will merge the content in the input tree that is not in the
			% reference tree (removing the rest of the input tree, which is the
			% intersection of the content of both trees), but the "original"
			% content of the input tree may include duplicates, in which case
			% this input content (only) will either be copied as a whole or
			% (probably preferably) be uniquified on the fly:

			ContentInBothSets =
				set_utils:intersection( InputSHA1Set, ReferenceSHA1Set ),

			case clear_input_tree( InputTree, LackingCount, ContentInBothSets,
					InputRootDir, ReferenceRootDir, UserState ) of

				% Here no content from input tree can enrich the reference one:
				undefined ->
					ReferenceTree;

				% Input tree has original content here:
				ClearedInputTree ->
					integrate_content_to_merge( ClearedInputTree, ReferenceTree,
												LackingCount, UserState )

			end

	end.



% @doc Purges specified tree from specified content, removing it from the
% filesystem and returning the corresponding, updated, tree data.
%
-spec purge_tree_from( tree_data(), set( sha1() ), user_state() ) ->
								tree_data().
purge_tree_from( Tree=#tree_data{ root=BinRootDir,
								  entries=Entries,
								  file_count=FileCount },
				 SHA1ToPurge, UserState ) ->

	SHA1s = set_utils:to_list( SHA1ToPurge ),

	{ PurgedEntries, RemoveCount } =
		purge_helper( SHA1s, Entries, BinRootDir, _RemoveCount=0, UserState ),

	Tree#tree_data{ entries=PurgedEntries,
					file_count=FileCount-RemoveCount }.


% (helper)
purge_helper( _SHA1s=[], Entries, _BinRootDir, RemoveCount, _UserState ) ->
	{ Entries, RemoveCount };

purge_helper( _SHA1s=[ SHA1 | T ], Entries, BinRootDir, RemoveCount,
			  UserState ) ->

	{ FileDatas, PurgedEntries } = table:extract_entry( SHA1, Entries ),

	FilesToRemove = [ file_utils:bin_join( BinRootDir, FD#file_data.path )
								|| FD <- FileDatas ],

	trace_debug( "Removing following files corresponding to non-original "
		"input content of SHA1 ~ts: ~ts", [ sha1_to_string( SHA1 ),
			text_utils:strings_to_string( FilesToRemove ) ], UserState ),

	remove_files( FilesToRemove, UserState ),

	purge_helper( T, PurgedEntries, BinRootDir,
				  RemoveCount + length( FileDatas ), UserState ).



% @doc Removes specified content from specified input tree. Returns the
% resulting tree, or 'undefined' if it became empty.
%
-spec clear_input_tree( tree_data(), count(), set( sha1() ),
			bin_directory_path(), bin_directory_path(), user_state() ) ->
								maybe( tree_data() ).
clear_input_tree( InputTree, LackingCount, ContentToClear, InputRootDir,
				  ReferenceRootDir, UserState ) ->

	PurgedInputTree = purge_tree_from( InputTree, ContentToClear, UserState ),

	ContentCount = table:size( PurgedInputTree#tree_data.entries ),

	% Check:
	LackingCount = ContentCount,

	% First, see whether input tree has duplicates now that purged:
	case PurgedInputTree#tree_data.file_count > ContentCount of

		true ->

			UniqPrompt = text_utils:format(
				"There are duplicates among the ~B contents in the "
				"input tree ('~ts') that are original (i.e. that are "
				"not in the reference one, '~ts').~n"
				"Shall we uniquify first that input, original "
				"content?~n"
				"(this is recommended, otherwise for each of these "
				"duplicates a single of them will have to be chosen by "
				"the user afterwards so that it can be moved)",
				[ LackingCount, InputRootDir, ReferenceRootDir ] ),

			case ui:ask_yes_no( UniqPrompt, _BinaryDefault=yes ) of

				yes ->
					DedupTree = deduplicate_tree( PurgedInputTree, UserState ),

					case table:is_empty( DedupTree#tree_data.entries ) of

						% Can happen if content has been removed as a whole:
						true ->
							ui:display( "After uniquification, the input tree "
								"path ('~ts') no longer contains original "
								"content; removing directly the input tree.",
								[ InputRootDir ] ),

							remove_tree( InputRootDir, UserState ),
							undefined;

						false ->
							DedupTree

					end;

				no ->
					PurgedInputTree

			end;

		% No duplicates here:
		false ->
			PurgedInputTree

	end.



% @doc Integrates the content of the specified input tree into the specified
% reference one, and returns this one.
%
-spec integrate_content_to_merge( tree_data(), tree_data(), count(),
								  user_state() ) -> tree_data().
integrate_content_to_merge(
		InputTree=#tree_data{ root=InputRootDir,
							  entries=InputEntries },
		ReferenceTree=#tree_data{ root=ReferenceRootDir,
								  entries=ReferenceEntries },
		ContentCount, UserState ) ->

	% As whole contents may have been removed (by design non-empty, and of size
	% ContentCount):
	%
	ToIntegrate = table:keys( InputEntries ),

	Prompt = case ContentCount  of

		1 ->
			text_utils:format( "A single content is present in the "
				"input tree ('~ts') but not in the reference one ('~ts').",
				[ InputRootDir, ReferenceRootDir ] );

		_ ->
			text_utils:format( "Exactly ~B contents are present in the input "
				"tree ('~ts') but are lacking in the reference one ('~ts').",
				[ ContentCount, InputRootDir, ReferenceRootDir ] )

	end,

	Choices = [ { move, "Move this content as a whole (one file per "
				  "content) in the reference tree" },
				{ cherry_pick,
				  "Cherry-pick which content to move to reference tree "
				  "or to delete" },
				{ delete, "Delete as a whole this content (it will "
				  "thus be permanently lost afterwards)" },
				{ abort, "Abort merge" } ],

	TargetDir = file_utils:bin_join( ReferenceRootDir, ?integrate_dir ),

	case ui:choose_designated_item(
			text_utils:format( "~ts~n~nChoices are:", [ Prompt ] ), Choices ) of

		move ->
			file_utils:create_directory_if_not_existing( TargetDir ),

			% If uniquification was chosen beforehand, no choice shall be left,
			% and thus the unique remaining version will be moved; otherwise it
			% will have to be selected by the user:
			%
			NewReferenceEntries = move_content_to_integrate( ToIntegrate,
				InputRootDir, InputEntries, ReferenceRootDir, ReferenceEntries,
				?integrate_dir, ContentCount, UserState ),

			FileCount = get_file_count_from( NewReferenceEntries ),

			ReferenceTree#tree_data{ entries=NewReferenceEntries,
									 file_count=FileCount };

		cherry_pick ->
			file_utils:create_directory_if_not_existing( TargetDir ),
			NewReferenceEntries = cherry_pick_content_to_merge( ToIntegrate,
				InputRootDir, InputEntries, ReferenceRootDir, ReferenceEntries,
				?integrate_dir, UserState ),

			FileCount = get_file_count_from( NewReferenceEntries ),

			ReferenceTree#tree_data{ entries=NewReferenceEntries,
									 file_count=FileCount };

		delete ->
			% Might happen, typically for empty files:
			%
			% (possibly one content, multiple elements)
			%
			DelPrompt = text_utils:format( "Really delete the ~B "
				"unique content element(s) found in the input tree ('~ts')?",
				[ ContentCount, InputRootDir ] ),

			case ui:ask_yes_no( DelPrompt ) of

				yes ->
					delete_content_to_merge( ToIntegrate, InputRootDir,
											 InputEntries, UserState ),
					% Thus unchanged:
					ReferenceTree;

				no ->
					% Looping, as we cannot skip any content in input tree
					% (otherwise its removal will fail, short of being empty):
					%
					integrate_content_to_merge( InputTree, ReferenceTree,
												ContentCount, UserState )

			end;

		C when C =:= abort orelse C =:= ui_cancel ->
			trace_debug( "(requested to abort the merge)", UserState ),
			stop( 0 )

	end.



% @doc Copies the contents, specified through their SHA1, from the source tree
% to the target one, and returns a corresponding updated version of this target
% tree.
%
% Note: this SHA1 is not expected to already exist in the target tree.
%
-spec copy_content( [ sha1() ], bin_directory_path(), sha1_table(), tree_data(),
					user_state() ) -> tree_data().
copy_content( SHA1sToCopy, SourceRootDir, SourceEntries,
			  TargetTree=#tree_data{ root=TargetRootDir }, UserState ) ->

	ContentCount = length( SHA1sToCopy ),

	trace_debug( case ContentCount of

		1 ->
			text_utils:format( "A single content is to copy from the "
				"'~ts' tree to the '~ts' one.",
				[ SourceRootDir, TargetRootDir ] );

		_ ->
			text_utils:format( "Exactly ~B contents are to copy from the "
				"'~ts' source tree to the '~ts' target tree.",
				[ ContentCount, SourceRootDir, TargetRootDir ] )

	end, UserState ),

	% Returns an updated TargetTree:
	copy_content_helper( SHA1sToCopy, SourceRootDir, SourceEntries,
						 TargetTree, UserState ).



% (helper)
copy_content_helper( _SHA1sToCopy=[], _SourceRootDir, _SourceEntries,
					 TargetTree, _UserState ) ->
	TargetTree;

copy_content_helper( _SHA1sToCopy=[ SHA1 | T ], SourceRootDir,
		SourceEntries,
		TargetTree=#tree_data{ root=TargetRootDir,
							   entries=TargetEntries,
							   file_count=FileCount }, UserState ) ->

	% Selects a suitable target filename; first found one is good:
	[ SourceFileData | _ ] = table:get_value( SHA1, SourceEntries ),
	SourceFilePath = SourceFileData#file_data.path,

	{ ActualAbsTargetPath, ActualTargetPath } = safe_copy( SourceRootDir,
		SourceFilePath, TargetRootDir, UserState ),

	TargetTimestamp =
		file_utils:get_last_modification_time( ActualAbsTargetPath ),

	% Other fields are exactly the same:
	NewTargetFileData = SourceFileData#file_data{ path=ActualTargetPath,
												  timestamp=TargetTimestamp },

	NewTargetEntries = table:add_new_entry( SHA1, [ NewTargetFileData ],
											TargetEntries ),

	NewTargetTree = TargetTree#tree_data{ entries=NewTargetEntries,
										  file_count=FileCount+1 },

	copy_content_helper( T, SourceRootDir, SourceEntries, NewTargetTree,
						 UserState ).



% @doc Moves, in the context of a merge, all specified content in the reference
% tree, and returns an updated view thereof.
%
-spec move_content_to_integrate( [ sha1() ], bin_directory_path(), sha1_table(),
		bin_directory_path(), sha1_table(), bin_directory_path(), count(),
		user_state() ) -> sha1_table().
move_content_to_integrate( ToIntegrate, InputRootDir, InputEntries,
		ReferenceRootDir, ReferenceEntries, TargetSubPath, TotalContentCount,
		UserState ) ->
	move_content_to_integrate( ToIntegrate, InputRootDir, InputEntries,
		ReferenceRootDir, ReferenceEntries, TargetSubPath,
		_Count=1, TotalContentCount, UserState ).



% @doc Moves as a whole all specified content in the reference tree, and returns
% an updated view thereof.
%
-spec move_content_to_integrate( [ sha1() ], bin_directory_path(), sha1_table(),
		bin_directory_path(), sha1_table(), bin_directory_path(),
		count(), count(), user_state() ) -> sha1_table().
% Moves finished here:
move_content_to_integrate( _ToMove=[], InputRootDir, InputEntries,
		ReferenceRootDir, ReferenceEntries, TargetSubPath, _ContentCount,
		_TotalContentCount, UserState ) ->

	% Removing the content that has not been moved (hence that shall be
	% deleted):
	%
	ToRemove = list_utils:flatten_once( [
		[ file_utils:bin_join( InputRootDir, FD#file_data.path ) || FD <- FDL ]
					|| FDL <- table:values( InputEntries ) ] ),

	remove_files( ToRemove, UserState ),

	% Copied to reference as much as possible:
	preserve_symlinks( InputRootDir, ReferenceRootDir, TargetSubPath,
					   UserState ),

	remove_file( get_cache_path_for( InputRootDir ), UserState ),

	% Input tree shall be now void of all content, and can thus be removed:
	file_utils:remove_empty_tree( InputRootDir ),

	ReferenceEntries;


move_content_to_integrate( _ToMove=[ SHA1 | T ], InputRootDir, InputEntries,
		ReferenceRootDir, ReferenceEntries, TargetSubPath,
		ContentCount, TotalContentCount, UserState ) ->

	ui:set_setting( title, text_utils:format( "Merging content ~B/~B",
									[ ContentCount, TotalContentCount ] ) ),

	{ FileDatas, NewInputEntries } = table:extract_entry( SHA1, InputEntries ),

	ElectedFileData = case FileDatas of

		[ FileData ] ->
			FileData;

		_ ->
			Prompt = text_utils:format( "~B files correspond to the same input "
				"content; please select the unique one that shall be "
				"copied to the reference tree:", [ length( FileDatas ) ] ),

			% This order matters:
			Choices = [ FD#file_data.path || FD <- FileDatas ],

			% Avoiding the definition of a default choice as the UI might be a
			% bit misleading:
			%
			%DefaultChoiceIndex = hd( Choices ),

			%Index = case ui:choose_numbered_item_with_default( Prompt, Choices,
			%                               DefaultChoiceIndex ) of

			Index = case ui:choose_numbered_item( Prompt, Choices ) of

				% ui_cancel:
				0 ->
					throw( operation_cancelled );

				I ->
					I

			end,

			{ Selected, Others } =
					list_utils:extract_element_at( FileDatas, Index ),

			ToRemove = [ file_utils:bin_join( InputRootDir, FD#file_data.path )
							|| FD <- Others ],

			remove_files( ToRemove, UserState ),

			Selected

	end,

	% With a check:
	SHA1 = ElectedFileData#file_data.sha1_sum,

	SourceRelPath = ElectedFileData#file_data.path,

	{ NewAbsPath, NewRelPath } = safe_move( InputRootDir, SourceRelPath,
								ReferenceRootDir, TargetSubPath, UserState ),

	% Selective update:
	NewFileData = ElectedFileData#file_data{
		path=NewRelPath,
		% To avoid any kind of discrepancy:
		timestamp=file_utils:get_last_modification_time( NewAbsPath ) },

	% New content in reference:
	NewReferenceEntries =
		table:add_new_entry( SHA1, [ NewFileData ], ReferenceEntries ),

	move_content_to_integrate( T, InputRootDir, NewInputEntries,
		ReferenceRootDir, NewReferenceEntries, TargetSubPath,
		ContentCount+1, TotalContentCount, UserState ).



% @doc Equalizes specified trees.
-spec equalize( tree_data(), tree_data(), user_state() ) ->
						{ tree_data(), tree_data() }.
equalize( FirstTreeData=#tree_data{ root=FirstRootPath,
									entries=FirstEntryTable },
		  SecondTreeData=#tree_data{ root=SecondRootPath,
									 entries=SecondEntryTable }, UserState ) ->

	ui:set_setting( title, "Equalize report" ),

	IsFirstUniquified = is_uniquified( FirstTreeData ),
	IsSecondUniquified = is_uniquified( SecondTreeData ),

	FirstSHA1s = table:keys( FirstEntryTable ),
	SecondSHA1s = table:keys( SecondEntryTable ),

	case list_utils:differences( FirstSHA1s, SecondSHA1s ) of

		{ _OnlyInFirstSHA1=[], _OnlyInSecondSHA1=[] } ->

			ui:display( "Both trees have exactly the same ~ts (and ~ts).",
				[ count_content( FirstSHA1s ),
				  interpret_uniqueness( IsFirstUniquified, IsSecondUniquified,
										FirstRootPath, SecondRootPath ) ] ),

			{ FirstTreeData, SecondTreeData };


		{ OnlyInFirstSHA1, _OnlyInSecondSHA1=[] } ->

			ui:display( "The first tree ('~ts') has ~ts that the second "
				"has not (and ~ts); copying this content.~n~n~ts",
				[ FirstRootPath, count_content( OnlyInFirstSHA1 ),
				  uniquified_to_string( IsFirstUniquified ),
				  list_lacking_content( OnlyInFirstSHA1, FirstEntryTable,
					"second", _MaybeRootPath=undefined,
					_MaybeDeltaFile=undefined ) ] ),

			NewSecondTreeData = copy_content( OnlyInFirstSHA1, FirstRootPath,
				FirstEntryTable, SecondTreeData, UserState ),

			{ FirstTreeData, NewSecondTreeData };


		{ _OnlyInFirstSHA1=[], OnlyInSecondSHA1 } ->

			ui:display( "The second tree ('~ts') has ~ts that the first "
				"has not (and ~ts); copying this content.~n~n~ts",
				[ SecondRootPath, count_content( OnlyInSecondSHA1 ),
				  uniquified_to_string( IsSecondUniquified ),
				  list_lacking_content( OnlyInSecondSHA1, SecondEntryTable,
					"first", _MaybeRootPath=undefined,
					_MaybeDeltaFile=undefined ) ] ),

			NewFirstTreeData = copy_content( OnlyInSecondSHA1, SecondRootPath,
				SecondEntryTable, FirstTreeData, UserState ),

			{ NewFirstTreeData, SecondTreeData };


		{ OnlyInFirstSHA1, OnlyInSecondSHA1 } ->

			MaybeRootPath = undefined,
			MaybeDeltaFile = undefined,

			ui:display( "First tree has ~ts that the second has not, "
				"and the second has ~ts that the first has not (and ~ts). "
				"~n~n~ts~n~ts~nCompleting both trees with the content "
				"they lack.",
				[ count_content( OnlyInFirstSHA1 ),
				  count_content( OnlyInSecondSHA1 ),
				  interpret_uniqueness( IsFirstUniquified,
					IsSecondUniquified, FirstRootPath, SecondRootPath ),
				  list_lacking_content( OnlyInSecondSHA1, SecondEntryTable,
					"first", MaybeRootPath, MaybeDeltaFile ),
				  list_lacking_content( OnlyInFirstSHA1, FirstEntryTable,
					"second", MaybeRootPath, MaybeDeltaFile ) ] ),

			NewFirstTreeData = copy_content( OnlyInSecondSHA1, SecondRootPath,
				SecondEntryTable, FirstTreeData, UserState ),

			NewSecondTreeData = copy_content( OnlyInFirstSHA1, FirstRootPath,
				NewFirstTreeData#tree_data.entries, SecondTreeData, UserState ),

			{ NewFirstTreeData, NewSecondTreeData }

	end.



-spec interpret_uniqueness( boolean(), boolean(), bin_directory_path(),
							bin_directory_path() ) -> ustring().
interpret_uniqueness( _IsFirstUniquified=true, _IsSecondUniquified=true,
					  _FirstRootPath, _SecondRootPath ) ->
	"both trees are uniquified";

interpret_uniqueness( _IsFirstUniquified=true, _IsSecondUniquified=false,
					  FirstRootPath, _SecondRootPath ) ->
	text_utils:format( "only the first tree, '~ts', is uniquified",
					   [ FirstRootPath ] );

interpret_uniqueness( _IsFirstUniquified=false, _IsSecondUniquified=true,
					  _FirstRootPath, SecondRootPath ) ->
	text_utils:format( "only the second tree, '~ts', is uniquified",
					   [ SecondRootPath ] );

interpret_uniqueness( _IsFirstUniquified=false, _IsSecondUniquified=false,
					  _FirstRootPath, _SecondRootPath ) ->
	"neither tree is uniquified".



% @doc Checks the specified tree path against the specified cache file. Displays
% differences, and generates corresponding delta files (if any).
%
-spec check_against( bin_directory_path(), bin_file_path(), analyzer_ring(),
					 user_state() ) -> void().
check_against( AbsTreePath, AbsCachePath, AnalyzerRing, UserState ) ->

	% Local tree:
	ToCheckTreeData = resync( AbsTreePath, AnalyzerRing, UserState ),
	IsToCheckUniquified = is_uniquified( ToCheckTreeData ),
	ToCheckEntries = ToCheckTreeData#tree_data.entries,
	ToCheckSHA1s = table:keys( ToCheckEntries ),
	LocalHostname = ToCheckTreeData#tree_data.hostname,

	% Cache file:
	ReadTreeData = read_cache_file( AbsCachePath, UserState ),
	AbsReadTreePath = ReadTreeData#tree_data.root,
	IsReadUniquified = is_uniquified( ReadTreeData ),
	ReadEntries = ReadTreeData#tree_data.entries,
	ReadSHA1s = table:keys( ReadEntries ),

	ReadHostname = ReadTreeData#tree_data.hostname,

	Title = case net_utils:bin_localhost() of

		ReadHostname ->
			"Check report";

		_ ->
			text_utils:format( "Check report against tree on host '~ts'",
							   [ ReadHostname ] )

	end,

	ui:set_setting( title, Title ),

	InBothSHA1s = list_utils:intersection( ToCheckSHA1s, ReadSHA1s ),

	UniqStr = interpret_uniqueness( IsToCheckUniquified, IsReadUniquified,
									AbsTreePath, AbsReadTreePath ),

	case list_utils:differences( ToCheckSHA1s, ReadSHA1s ) of

		{ _OnlyInCheckedSHA1s=[], _OnlyInCachedSHA1s=[] } ->
			ui:display( "The checked tree '~ts' and  '~ts' "
				"(as represented by its cache file '~ts') have exactly the "
				"same content (~ts), and ~ts.",
				[ AbsTreePath, AbsReadTreePath, AbsCachePath,
				  count_content( InBothSHA1s ), UniqStr ] );


		{ OnlyInCheckedSHA1s, _OnlyInCachedSHA1s=[] } ->

			{ DeltaFilePath, DeltaFile } = prepare_delta( LocalHostname,
				AbsTreePath, ReadHostname, AbsReadTreePath ),

			ui:display( "The checked tree ('~ts') is a strict superset "
				"of '~ts' (as represented by its cache file '~ts'); "
				"they have in common ~ts (~ts), and checked tree has "
				"additionally ~ts.~n~nIts extra content will be written in "
				"'~ts'.~n~n~ts",
				[ AbsTreePath, AbsReadTreePath, AbsCachePath,
				  count_content( InBothSHA1s ), UniqStr,
				  count_content( OnlyInCheckedSHA1s ), DeltaFilePath,
				  list_lacking_content( OnlyInCheckedSHA1s, ToCheckEntries,
					_OtherTreeDesc="cached", AbsTreePath, DeltaFile ) ] );


		{ _OnlyInCheckedSHA1s=[], OnlyInCachedSHA1s } ->

			{ DeltaFilePath, DeltaFile } = prepare_delta( ReadHostname,
				AbsReadTreePath, LocalHostname, AbsTreePath ),

			ui:display( "The checked tree ('~ts') is a strict subset "
				"of '~ts' (as represented by its cache file '~ts'); "
				"they have in common ~ts (~ts), and checked tree lacks ~ts.~n~n"
				"This lacking content will be written in '~ts'.~n~n~ts",
				[ AbsTreePath, AbsReadTreePath, AbsCachePath,
				  count_content( InBothSHA1s ), UniqStr,
				  count_content( OnlyInCachedSHA1s ), DeltaFilePath,
				  list_lacking_content( OnlyInCachedSHA1s, ReadEntries,
					_OtherTreeDesc="checked", AbsReadTreePath, DeltaFile ) ] );


		{ OnlyInCheckedSHA1s, OnlyInCachedSHA1s } ->

			{ LackingInCacheDeltaFilePath, LackingInCacheDeltaFile } =
				prepare_delta( LocalHostname, AbsTreePath, ReadHostname,
							   AbsReadTreePath ),

			{ LackingInCheckedDeltaFilePath, LackingInCheckedDeltaFile } =
				prepare_delta( ReadHostname, AbsReadTreePath,
							   LocalHostname, AbsTreePath ),

			ui:display( "Knowing that they have ~ts in common (and that ~ts), "
				"the checked tree ('~ts') contains ~ts that '~ts' "
				"(as represented by its cache file '~ts') does not have, "
				"but also lacks ~ts.~n~n"
				"The extra content in checked tree will be written in '~ts', "
				"while the extra content in cached tree will be written "
				"in '~ts'.~n~n~ts~n~ts",
				[ count_content( InBothSHA1s ), UniqStr, AbsTreePath,
				  count_content( OnlyInCheckedSHA1s ), AbsReadTreePath,
				  AbsCachePath, count_content( OnlyInCachedSHA1s ),
				  LackingInCacheDeltaFilePath, LackingInCheckedDeltaFilePath,
				  list_lacking_content( OnlyInCheckedSHA1s, ToCheckEntries,
					_FirstTreeDesc="cached", AbsTreePath,
					LackingInCacheDeltaFile ),
				  list_lacking_content( OnlyInCachedSHA1s, ReadEntries,
					_SecondTreeDesc="checked", AbsReadTreePath,
					LackingInCheckedDeltaFile ) ] )

	end.



% @doc Preserves symlinks by moving them from input root directory to target
% directory.
%
-spec preserve_symlinks( bin_directory_path(), bin_directory_path(),
						 bin_directory_path(), user_state() ) -> void().
preserve_symlinks( InputRootDir, TargetRootDir, TargetSubPath, UserState ) ->

	% There may still be symbolic links in the input tree (ex: that were either
	% added the user or created by this tool when electing a reference file and
	% replacing duplicates by links).

	% We try to move them in the merge target directory (possibly breaking them
	% in the process, should they be relative to a moved or removed element, or
	% to content outside of the input tree):
	%
	case file_utils:find_links_from( InputRootDir,
									 _IfImproperEncoding=include ) of

		[] ->
			trace_debug( "No symlink to move from '~ts'.", [ InputRootDir ],
						 UserState ),
			ok;


		SymlinksToMove ->

			MovedLinksStr = [
				try

					{ AbsTargetPath, _RelTargetPath } = safe_move( InputRootDir,
						Lnk, TargetRootDir, TargetSubPath, UserState ),

					AbsTargetPath

				catch _AnyClass:Exception ->

					TargetDir =
						file_utils:bin_join( TargetRootDir, TargetSubPath ),

					ErrorStr = text_utils:format(
						"error while smart moving symbolic "
						"link '~ts' from '~ts' to '~ts':~n  ~p",
						[ Lnk, InputRootDir, TargetDir, Exception ] ),

					trace_utils:error( ErrorStr ),

					ErrorStr

				end || Lnk <- SymlinksToMove ],

			trace_debug( "Moved ~B extraneous symlinks from '~ts', now in: ~ts",
				[ length( SymlinksToMove ), InputRootDir,
				  text_utils:binaries_to_string( MovedLinksStr ) ], UserState )

	end.



% @doc Selects which of the specified elements among the input entries shall be
% merged in the reference content, and how.
%
% Returns the updated reference entries.
%
-spec cherry_pick_content_to_merge( [ sha1() ], bin_directory_path(),
	sha1_table(), bin_directory_path(), sha1_table(), bin_directory_path(),
	user_state() ) -> sha1_table().
cherry_pick_content_to_merge( SHA1sToPick, InputRootDir, InputEntries,
		ReferenceRootDir, ReferenceEntries, TargetSubPath, UserState ) ->

	TotalContentCount = length( SHA1sToPick ),

	PickChoices = [ { move, text_utils:format( "Move this content "
								"in reference tree (in its '~ts' directory)",
								[ TargetSubPath ] ) },
					{ delete, "Delete this content" },
					{ abort, "Abort merge" } ],

	cherry_pick_files_to_merge( SHA1sToPick, InputRootDir, InputEntries,
		ReferenceRootDir, ReferenceEntries, TargetSubPath, PickChoices,
		_Count=1, TotalContentCount, UserState ).



% @doc Allows the user to cherry-pick the files that shall be copied (others
% being removed; so no need to update source tree).
%
cherry_pick_files_to_merge( _SHA1sToPick=[], InputRootDir, _InputEntries,
		ReferenceRootDir, ReferenceEntries, TargetSubPath, _PickChoices,
		_Count, _TotalContentCount, UserState ) ->

	% All input files expected to have been removed.

	preserve_symlinks( InputRootDir, ReferenceRootDir, TargetSubPath,
					   UserState ),

	% Input tree shall be now void of content and thus can be removed as such:
	remove_file( get_cache_path_for( InputRootDir ), UserState ),
	file_utils:remove_empty_tree( InputRootDir ),

	ReferenceEntries;


cherry_pick_files_to_merge( _SHA1sToPick=[ SHA1 | T ], InputRootDir,
		InputEntries, ReferenceRootDir, ReferenceEntries, TargetSubPath,
		PickChoices, Count, TotalContentCount, UserState ) ->

	% In all cases all files for this SHA1 shall be removed from input tree:

	ui:set_setting( title, text_utils:format( "Cherry-picking content ~B/~B",
											  [ Count, TotalContentCount ] ) ),

	NewReferenceEntries = case table:get_value( SHA1, InputEntries ) of

		[ SingleFileData=#file_data{ path=ContentPath } ] ->

			BinFullContentPath =
				file_utils:bin_join( InputRootDir, ContentPath ),

			Prompt = text_utils:format( "Regarding the input content (solely) "
				"in the content tree '~ts', shall we:",
				[ BinFullContentPath ] ),

			% Avoiding defining defaults:
			%case ui:choose_designated_item_with_default( Prompt, PickChoices,
			%   _DefaultChoiceDesignator=move ) of

			case ui:choose_designated_item( Prompt, PickChoices ) of

				move ->
					{ MovedAbsRelPath, MovedRelPath } = safe_move(
						InputRootDir, ContentPath, ReferenceRootDir,
						TargetSubPath, UserState ),

					NewTimestamp =
					  file_utils:get_last_modification_time( MovedAbsRelPath ),

					NewFileData = SingleFileData#file_data{
									path=MovedRelPath,
									timestamp=NewTimestamp },

					table:add_new_entry( SHA1, [ NewFileData ],
										 ReferenceEntries );

				delete ->
					safe_delete( BinFullContentPath, UserState ),

					% Thus unchanged:
					ReferenceEntries;

				C when C =:= abort orelse C =:= ui_cancel ->
					ui:unset_setting( title ),
					ui:display( "Merge (single) cherry-pick aborted." ),
					trace_debug( "(requested to abort the cherry-pick)",
								 UserState ),
					stop_user_service( UserState )

			end;


		MultipleFileData ->

			FileCount = length( MultipleFileData ),

			ContentPaths = [ ContentPath
				|| #file_data{ path=ContentPath } <- MultipleFileData ],

			Prompt = text_utils:format( "The same content can be found in "
				"the following ~B input files (all relative to '~ts'): ~ts~n~n"
				"Regarding that input content, shall we:",
				[ FileCount, InputRootDir,
				  text_utils:binaries_to_binary( ContentPaths,
												 ?bullet_point ) ] ),

			% Default means UI a bit misleading:
			%case ui:choose_designated_item_with_default( Prompt, PickChoices,
			%		_DefaultChoiceDesignator=move ) of
			case ui:choose_designated_item( Prompt, PickChoices ) of

				move ->
					SelectPrompt = "Select the (single) input filename that "
					  "shall be moved in the reference tree (the other "
					  "input files with the same content being then removed):",

					MoveIndex = case ui:choose_numbered_item( SelectPrompt,
									ContentPaths ) of

						0 ->
							ui:unset_setting( title ),
							ui:display( "Merge cherry-pick aborted when "
										"selecting the content to move." ),
							stop_user_service( UserState );

						I ->
							I

					end,

					{ MovedFilePath, ToRemovePaths } =
					   list_utils:extract_element_at( ContentPaths, MoveIndex ),

					trace_debug( "Moving '~ts' to reference tree, "
						"removing '~ts'",
						[ MovedFilePath,
						  text_utils:binaries_to_string( ToRemovePaths ) ],
						UserState ),

					{ MovedAbsRelPath, MovedRelPath } = safe_move(
						InputRootDir, MovedFilePath, ReferenceRootDir,
						TargetSubPath, UserState ),

					ToRemoveFullPaths = [ file_utils:bin_join( InputRootDir, P )
											|| P <- ToRemovePaths ],

					remove_files( ToRemoveFullPaths, UserState ),

					NewTimestamp =
					  file_utils:get_last_modification_time( MovedAbsRelPath ),

					% Any will do:
					FileData = hd( MultipleFileData ),

					NewFileData = FileData#file_data{ path=MovedRelPath,
													  timestamp=NewTimestamp },

					table:add_new_entry( SHA1, [ NewFileData ],
										 ReferenceEntries );

				delete ->

					DelPrompt = text_utils:format( "Really delete following "
						"files from input directory '~ts', losing their "
						"(unique) corresponding content? ~ts",
						[ InputRootDir,
						  text_utils:binaries_to_string( ContentPaths ) ] ),

					case ui:ask_yes_no( DelPrompt ) of

						yes ->
							ToDelFiles =
								[ file_utils:bin_join( InputRootDir, P )
										|| P <- ContentPaths ],

							remove_files( ToDelFiles, UserState );

						no ->
							% Going back to the beginning of this step:
							cherry_pick_files_to_merge( SHA1, InputRootDir,
								InputEntries, ReferenceRootDir,
								ReferenceEntries, TargetSubPath, PickChoices,
								Count, TotalContentCount, UserState )

					end,
					ReferenceEntries;

				C when C =:= abort orelse C =:= ui_cancel ->
					ui:unset_setting( title ),
					ui:display( "Merge (multiple) cherry-pick aborted." ),
					trace_debug( "(requested to abort the cherry-pick)",
								 UserState ),
					stop_user_service( UserState )

			end

	end,

	cherry_pick_files_to_merge( T, InputRootDir, InputEntries,
		ReferenceRootDir, NewReferenceEntries, TargetSubPath, PickChoices,
		Count+1, TotalContentCount, UserState ).




% @doc Deletes all specified content.
%
% Does it on a per-content basic rather than doing nothing before the input tree
% is removed as whole, as more control is preferred (to check that the final
% input tree has been indeed emptied of all content).
%
-spec delete_content_to_merge( [ sha1() ], directory_path(), sha1_table(),
							   user_state() ) -> sha1_table().
delete_content_to_merge( SHA1sToDelete, InputRootDir, InputEntries,
						 UserState ) ->

	Paths = [ begin
				FileDatas = table:get_value( SHA1, InputEntries ),
				[ file_utils:bin_join( InputRootDir, P )
					|| #file_data{ path=P } <- FileDatas ]
			  end || SHA1 <- SHA1sToDelete ],

	remove_files( [ get_cache_path_for( InputRootDir )
					| list_utils:flatten_once( Paths ) ], UserState ),

	% Removing also any symlink left over:
	case file_utils:find_links_from( InputRootDir,
									 _IfImproperEncoding=include ) of

		[] ->
			trace_debug( "No symlink to move from '~ts'.", [ InputRootDir ],
						 UserState ),
			ok;

		SymlinksToRemove ->
			[ begin
				LnkFullPath = file_utils:bin_join( InputRootDir, LnkPath ),
				% Trace emitted just afterwards:
				file_utils:remove_symlink( LnkFullPath )
			  end || LnkPath <- SymlinksToRemove ],
			trace_debug( "Removed ~B extraneous symlinks from '~ts': ~ts",
				[ length( SymlinksToRemove ), InputRootDir,
				  text_utils:strings_to_string( SymlinksToRemove ) ],
				  UserState )

	end,

	% Shall be now void of content:
	file_utils:remove_empty_tree( InputRootDir ).



% @doc Moves "safely" specified file, from path RelFilePath relative to
% SourceRootDir to the same RelFilePath path but this time relatively to
% TargetSubPath, itself relative to TargetRootDir, by ensuring (through any
% renaming needed) that no clash happens at target.
%
% Returns the path to which the file was moved, twice: as a pair made of an
% absolute path and a relative one to TargetRootDir.
%
-spec safe_move( bin_directory_path(), bin_file_path(),
			bin_directory_path(), file_path(), user_state() ) ->
						{ bin_file_path(), bin_file_path() }.
safe_move( SourceRootDir, SourceRelPath, TargetRootDir, TargetSubPath,
		   UserState ) ->

	AbsSourcePath = file_utils:bin_join( SourceRootDir, SourceRelPath ),

	% Check:
	file_utils:is_existing_file_or_link( AbsSourcePath )
		orelse throw( { not_file, AbsSourcePath } ),

	RelTargetPath = file_utils:bin_join( TargetSubPath, SourceRelPath ),

	AbsTargetPath = file_utils:bin_join( TargetRootDir, RelTargetPath ),

	% Avoid any clash:
	P = { AckAbsTargetPath, _AckRelTargetPath } =
			case file_utils:exists( AbsTargetPath ) of

		true ->
			FixedAbsTargetPath =
				file_utils:get_non_clashing_entry_name_from( AbsTargetPath ),

			ui:display_warning(
				"File '~ts', due to a clash in target location '~ts', "
				"had to be moved to '~ts' instead.",
				[ AbsSourcePath, AbsTargetPath, FixedAbsTargetPath ] ),

			% Nothing simpler than:
			FixedRelTargetPath = file_utils:make_relative( FixedAbsTargetPath,
											_RefDir=TargetRootDir ),

			{ FixedAbsTargetPath, FixedRelTargetPath };

		false ->
			{ AbsTargetPath, RelTargetPath }

	end,

	% Ensures subdirectories exist in the target tree:
	file_utils:create_directory( file_utils:get_base_path( AckAbsTargetPath ),
								 create_parents ),

	trace_debug( "Moving file '~ts' to '~ts'.",
				 [ AbsSourcePath, AckAbsTargetPath ], UserState ),

	file_utils:move_file( AbsSourcePath, AckAbsTargetPath ),

	P.



% @doc Copies "safely" the specified file, from path RelFilePath relative to
% SourceRootDir to the same RelFilePath path but this time relatively to
% TargetRootDir, by ensuring (through any renaming needed) that no clash happens
% at target.
%
% Returns the path to which the file was copied, twice: as a pair made of an
% absolute path and as one relative to TargetRootDir.
%
-spec safe_copy( bin_directory_path(), bin_file_path(),
				 bin_directory_path(), user_state() ) ->
						{ bin_file_path(), bin_file_path() }.
safe_copy( SourceRootDir, SourceRelPath, TargetRootDir, UserState ) ->

	AbsSourcePath = file_utils:bin_join( SourceRootDir, SourceRelPath ),

	RelTargetPath = SourceRelPath,

	AbsTargetPath = file_utils:bin_join( TargetRootDir, RelTargetPath ),

	P = { AckAbsTargetPath, _AckRelTargetPath } =
			case file_utils:exists( AbsTargetPath ) of

		true ->
			FixedAbsTargetPath =
				file_utils:get_non_clashing_entry_name_from( AbsTargetPath ),

			ui:display_warning(
				"File '~ts', due to a clash, had to be copied to '~ts'.",
				[ AbsSourcePath, FixedAbsTargetPath ] ),

			% Nothing simpler than:
			FixedRelTargetPath = file_utils:make_relative( FixedAbsTargetPath,
											_RefDir=TargetRootDir ),

			{ FixedAbsTargetPath, FixedRelTargetPath };

		false ->
			{ AbsTargetPath, RelTargetPath }

	end,

	% Ensures subdirectories exist in the target tree:
	file_utils:create_directory( file_utils:get_base_path( AckAbsTargetPath ),
								 create_parents ),

	trace_debug( " - copying file '~ts' to '~ts'.",
				 [ AbsSourcePath, AckAbsTargetPath ], UserState ),

	file_utils:copy_file( AbsSourcePath, AckAbsTargetPath ),

	P.



% @doc Deletes "safely" specified file.
-spec safe_delete( file_path(), user_state() ) -> void().
safe_delete( FilePath, UserState ) ->

	Prompt = text_utils:format( "Really delete file '~ts'?", [ FilePath ] ),

	case ui:ask_yes_no( Prompt ) of

		yes ->
			remove_file( FilePath, UserState );

		no ->
			% Input file hence left over.
			ok

	end.



% @doc Returns the number of files referenced in specified table.
-spec get_file_count_from( sha1_table() ) -> count().
get_file_count_from( SHA1Table ) ->

	lists:foldl( fun( FileDataList, Acc ) ->
					Acc + length( FileDataList )
				 end,
				 _Acc0=0,
				 _List=table:values( SHA1Table ) ).




% Helpers.


% @doc Starts user-related services.
-spec start_user_service( file_path() ) -> user_state().
start_user_service( LogFilename ) ->

	%trace_bridge:debug_fmt( "Logs will be written to '~ts'.",
	%                        [ LogFilename ] ),

	% We append to the log file (not resetting it), if it already exists:
	% (no delayed_write, to avoid missing logs when halting on error)
	%
	% No more file_utils:get_default_encoding_option(), refer to the
	% merge_file_options define for an explanation:
	%
	FileOpts = [ append, raw ],

	LogFile = file_utils:open( LogFilename, FileOpts ),

	file_utils:write_ustring( LogFile,
		"~n~n~n###### Starting new merge session "
		"(merge tool version ~ts) on ~ts at ~ts.~n~n",
		[ ?merge_script_version, net_utils:localhost(),
		  time_utils:get_textual_timestamp() ] ),

	#user_state{ log_file=LogFile }.



% @doc Displays (if set so) and logs specified text.
-spec trace( ustring(), user_state() ) -> user_state().
trace( Message, UserState=#user_state{ log_file=LogFile } ) ->
	file_utils:write_ustring( LogFile, Message ++ "\n" ),
	ui:trace( Message ),
	UserState.



% @doc Displays (if set so) and logs specified formatted text.
-spec trace( format_string(), [ term() ], user_state() ) -> user_state().
trace( FormatString, Values, UserState=#user_state{ log_file=LogFile } ) ->
	Msg = text_utils:format( FormatString, Values ),
	file_utils:write_ustring( LogFile, Msg ++ "\n" ),
	ui:trace( Msg ),
	UserState.



% @doc Logs specified debug text.
-spec trace_debug( ustring(), user_state() ) -> user_state().
trace_debug( Message, UserState=#user_state{ log_file=LogFile } ) ->
	file_utils:write_ustring( LogFile, Message ++ "\n" ),
	UserState.


% @doc Logs specified debug formatted text.
-spec trace_debug( format_string(), [ term() ], user_state() ) -> user_state().
trace_debug( FormatString, Values,
			 UserState=#user_state{ log_file=LogFile } ) ->
	Msg = text_utils:format( FormatString, Values ),
	file_utils:write_ustring( LogFile, Msg ++ "\n" ),
	UserState.



% @doc Stops user-related services (normal exit).
-spec stop_user_service( user_state() ) -> void().
stop_user_service( UserState=#user_state{ log_file=LogFile } ) ->

	trace_debug( "Stopping user service.", UserState ),

	ui:stop(),

	file_utils:write_ustring( LogFile, "Stopping merge session.~n", [] ),

	trace_debug( "Stopped.", UserState ),

	file_utils:close( LogFile ),

	io:format( "~n(execution success)~n" ),

	basic_utils:stop( _ErrorCode=0 ).



% @doc Checks that the source and target (absolute) directories exist.
-spec check_content_trees( bin_directory_path(), bin_directory_path() ) ->
									void().
check_content_trees( InputTreePath, ReferenceTreePath ) ->

	file_utils:is_existing_directory_or_link( InputTreePath ) orelse
		begin
			ui:display_error( "Specified input tree ('~ts') "
				"does not exist, aborting now.", [ InputTreePath ] ),
			throw( { non_existing_input_tree, InputTreePath } )
		end,

	file_utils:is_existing_directory_or_link( ReferenceTreePath ) orelse
		begin
			ui:display_error( "Specified reference tree ('~ts') "
				"does not exist, aborting now.", [ ReferenceTreePath ] ),
			throw( { non_existing_reference_tree, ReferenceTreePath } )
		end.



% @doc Returns the path of the cache file corresponding to the specified tree
% path.
%
-spec get_cache_path_for( bin_directory_path() ) -> bin_file_path().
get_cache_path_for( BinTreePath ) ->
	file_utils:bin_join( BinTreePath, ?merge_cache_filename ).



% @doc Ensures that specified tree path exists.
-spec check_tree_path_exists( any_directory_path() ) -> void().
check_tree_path_exists( AnyTreePath ) ->
	file_utils:is_existing_directory_or_link( AnyTreePath ) orelse
		begin
			ui:display_error( "The path '~ts' does not exist.",
							  [ AnyTreePath ] ),
			throw( { non_existing_content_tree, AnyTreePath } )
		end.



% @doc Updates specified content tree (based on a "weak" check): verifies that
% it exists, that a merge cache file exists and is up to date (otherwise
% rebuilds it), and returns the corresponding tree datastructure.
%
-spec update_content_tree( bin_directory_path(), analyzer_ring(),
						   user_state() ) -> tree_data().
update_content_tree( BinTreePath, AnalyzerRing, UserState ) ->

	CacheFilePath = get_cache_path_for( BinTreePath ),

	MaybeTreeData = case file_utils:is_existing_file( CacheFilePath ) of

		true ->
			trace_debug( "Found existing cache file '~ts'.", [ CacheFilePath ],
						 UserState ),

			% Load it, if trusted (typically if not older from the newest
			% file in tree):
			%
			case find_newest_timestamp_from( BinTreePath, CacheFilePath ) of

				{ _NoNewestTimestamp=undefined, _ContentFiles=[] } ->
					ui:display_instant( "Tree '~ts' is empty, creating a blank "
										"cache file for it.", [ BinTreePath ] ),
					undefined;

				{ NewestTimestamp, ContentFiles } ->
					handle_newest_timestamp( NewestTimestamp, ContentFiles,
						CacheFilePath, BinTreePath, AnalyzerRing, UserState )
			end;

		false ->
			ui:display_instant( "No cache file found for '~ts', creating it.",
								[ BinTreePath ] ),
			undefined

	end,

	%trace_bridge:debug_fmt( "MaybeTreeData: ~ts",
	%						[ type_utils:interpret_type_of( MaybeTreeData ) ] ),

	case MaybeTreeData of

		undefined ->
			create_merge_cache_file_for( BinTreePath, AnalyzerRing, UserState );

		_ ->
			MaybeTreeData

	end.



% @doc Returns the last content modification timestamp of the most recently
% modified file (the merge cache file excluded) in specified tree, and a list of
% the actual files (as relative paths).
%
-spec find_newest_timestamp_from( bin_directory_path(), bin_file_path() ) ->
						{ maybe( posix_seconds() ), [ bin_file_path() ] }.
find_newest_timestamp_from( RootPath, CacheFilePath ) ->

	CacheFilename = file_utils:get_last_path_element( CacheFilePath ),

	ActualFileRelPaths = list_utils:delete_existing( CacheFilename,
										find_regular_files_from( RootPath ) ),

	%trace_bridge:debug_fmt( "ActualFileRelPaths: ~p", [ ActualFileRelPaths ] ),

	MaybeTimestamp = case ActualFileRelPaths of

		% Any atom is deemed superior to any integer, so the cache file will be
		% considered up to date:
		%
		[] ->
			undefined;

		_ ->
			% Any actual timestamp will shadow a null one:
			get_newest_timestamp( ActualFileRelPaths, RootPath,
								  _MostRecentTimestamp=0 )

	end,

	% Files returned to avoid performing multiple traversals:
	{ MaybeTimestamp, ActualFileRelPaths }.



% @doc Returns the lastest modification timestamp among the specified files.
get_newest_timestamp( _ContentFiles=[], _RootPath, MostRecentTimestamp ) ->
	MostRecentTimestamp;

get_newest_timestamp( _ContentFiles=[ F | T ], RootPath,
					  MostRecentTimestamp ) ->

	FilePath = file_utils:bin_join( RootPath, F ),

	case file_utils:get_last_modification_time( FilePath ) of

		Timestamp when Timestamp > MostRecentTimestamp ->
			get_newest_timestamp( T, RootPath, Timestamp );

		_ ->
			get_newest_timestamp( T, RootPath, MostRecentTimestamp )

	end.



% (helper)
-spec handle_newest_timestamp( posix_seconds(), [ file_path() ],
   file_path(), bin_directory_path(), analyzer_ring(), user_state() ) ->
									maybe( tree_data() ).
handle_newest_timestamp( NewestTimestamp, ContentFiles, CacheFilePath,
						 BinTreePath, AnalyzerRing, UserState ) ->

	NewestString = time_utils:timestamp_to_string(
						time_utils:from_posix_timestamp( NewestTimestamp ) ),

	case file_utils:get_last_modification_time( CacheFilePath ) of

		CacheTimestamp when CacheTimestamp < NewestTimestamp ->

			CacheString = time_utils:timestamp_to_string(
				time_utils:from_posix_timestamp( CacheTimestamp ) ),

			Prompt = text_utils:format(
				"Timestamp of cache file (~ts) older "
				"than most recent file timestamp in tree (~ts).~n~n"
				"Rebuilding cache file for tree '~ts'? "
				"(otherwise current cache file will be reused from now on)",
				[ CacheString, NewestString, BinTreePath ] ),

			case ui:ask_yes_no( Prompt ) of

				yes ->
					undefined;

				no ->
					% For future uses as well:
					file_utils:touch( CacheFilePath ),

					% Loops:
					update_content_tree( BinTreePath, AnalyzerRing, UserState )

			end;


		CacheTimestamp ->

			CacheString = time_utils:timestamp_to_string(
				time_utils:from_posix_timestamp( CacheTimestamp ) ),

			trace_debug( "Timestamp of cache file is "
				"acceptable (as ~ts is not older than the "
				"most recent file timestamp in tree, ~ts), "
				"just performing a quick check of file "
				"existences and sizes to further validate it.",
				[ CacheString, NewestString ], UserState ),

			case quick_cache_check( CacheFilePath, ContentFiles, BinTreePath,
									AnalyzerRing, UserState ) of

				undefined ->
					MatchPrompt = text_utils:format(
						"Cache file does not match actual tree ('~ts'), "
						"rebuilding cache file?~n"
						"(otherwise stops on error)", [ BinTreePath ] ),

					case ui:ask_yes_no( MatchPrompt, _Default=yes ) of

						yes ->
							undefined;

						no ->
							throw( { invalid_cache_file_for, BinTreePath } )

					end;


				TreeData ->
					ui:display_instant(
						"Cache file seems to match actual tree '~ts', "
						"considering it legit.", [ BinTreePath ] ),
					TreeData

			end

	end.



% @doc Creates an automatically named merge cache file for specified content
% tree (overwriting any priorly existing merge cache file), and returns that
% tree.
%
-spec create_merge_cache_file_for( directory_path(), analyzer_ring(),
								   user_state() ) -> tree_data().
create_merge_cache_file_for( TreePath, AnalyzerRing, UserState ) ->

	ui:display_instant( "Creating merge cache file for '~ts'...",
						[ TreePath ] ),

	AbsTreePath = file_utils:ensure_path_is_absolute( TreePath ),

	check_tree_path_exists( AbsTreePath ),

	TreeData = scan_tree( AbsTreePath, AnalyzerRing, UserState ),

	trace_debug( "Scanned tree: ~ts.",
				 [ tree_data_to_string( TreeData ) ], UserState ),

	write_cache_file( TreeData, UserState ),

	TreeData.



% @doc Performs the actual writing of a cache file.
-spec write_cache_file( tree_data(), user_state() ) -> void().
write_cache_file( TreeData=#tree_data{ root=BinRootDir }, UserState ) ->

	CacheFilePath = get_cache_path_for( BinRootDir ),

	trace_debug( "Writing merge cache file '~ts'.", [ CacheFilePath ],
				 UserState ),

	MergeFile = file_utils:open( CacheFilePath, ?merge_file_options ),

	write_cache_header( MergeFile ),

	write_tree_data( MergeFile, TreeData, UserState ),

	write_cache_footer( MergeFile ),

	file_utils:close( MergeFile ).



% @doc Writes the header of specified cache file.
-spec write_cache_header( file() ) -> void().
write_cache_header( File ) ->

	%ScriptName = file_utils:get_last_path_element( escript:script_name() ),
	ScriptName = ?MODULE,

	% UTF-8 must be specified there so that this file can be read by
	% file:consult/1 afterwards despite special characters being included in
	% filenames:
	%
	file_utils:write_ustring( File, "%% -*- coding: utf-8 -*-~n"
		"% Merge cache file written by '~ts' (version ~ts),~n"
		"% at ~ts.~n~n"
		"% Structure of the cached file entries (sorted according to their "
		"path):~n"
		"% first the 'file_info' tag, then the SHA1 of the file of interest,~n"
		"% its path (relative to the root entry), its size (in bytes) and~n"
		"% finally its POSIX timestamp.~n~n" ,
		[ ScriptName, ?merge_script_version,
		  time_utils:get_textual_timestamp() ] ).



% @doc Writes the footer of specified cache file.
-spec write_cache_footer( file() ) -> void().
write_cache_footer( File ) ->
	file_utils:write_ustring( File, "~n% End of merge cache file (at ~ts).",
							  [ time_utils:get_textual_timestamp() ] ).



% @doc Writes the specified tree data into specified file.
write_tree_data( MergeFile, #tree_data{ root=BinRootDir,
										entries=Entries }, _UserState ) ->

	RootDir = text_utils:binary_to_string( BinRootDir ),

	% Converting file_data records into file_info elements to be stored
	% in-file (serialised):
	%
	EntryContent = lists:foldl( fun( { SHA1, FileData }, Acc ) ->
									get_file_content_for( SHA1, FileData )
											++ Acc
								end,
								_Acc0=[],
								_List=table:enumerate( Entries ) ),

	% We do not write terms directly ("unconsult") anymore, as the Unicode paths
	% would be translated as lists of numbers, which would not be convenient. So
	% we write this content by ourselves instead now:
	%
	%file_utils:write_direct_terms( MergeFile, lists:reverse( EntryContent ) ).
	file_utils:write_ustring( MergeFile,
		"{hostname, <<\"~ts\"/utf8>>}.~n~n{root_dir, <<\"~ts\"/utf8>>}.~n~n",
		[ net_utils:localhost(), file_utils:escape_path( RootDir ) ] ),

	write_entries( MergeFile, lists:keysort( _PathIndex=2, EntryContent ) ).



write_entries( _File, _Content=[] ) ->
	ok;

write_entries( File,
			   _Content=[ { SHA1, RelativePath, Size, Timestamp } | T ] ) ->

	% To check Unicode:
	%io:format("Writing '~ts'.~n", [ RelativePath ] ),

	% For easier interpretation, we now store SHA1 as hexadecimal in strings
	% instead of direct integers:

	% If ever some SHA1 were shorter than 40 (+3 for the quotes and the comma)
	% characters (alignment for readability):
	%
	TargetSHA1Width = 43,

	SHA1Str = text_utils:pad_string_left( text_utils:format( "\"~ts\",",
				[ sha1_to_string( SHA1 ) ] ), TargetSHA1Width ),

	% 'file_info', to better separate from 'file_data':
	%
	% (we wanted to write human-readable paths, yet some of them, due to raw
	% filenames, cannot be serialised as such strings; so we write the ones that
	% can, and the other are serialised as explicit binaries, containing lists
	% of Unicode codepoints, so that re-reading them will result in the
	% proper content to be obtained)
	%
	{ MaybeComment, PathToWrite } =
			case text_utils:try_convert_to_unicode_binary( RelativePath ) of

		undefined ->
			% Cannot do better to store a list of Unicode codepoints:
			Comment = text_utils:format( "~n% Corresponds to '~ts' "
				"(not an Unicode filename):~n", [ RelativePath ] ),
			{ Comment, text_utils:format( "~w", [ RelativePath ] ) };

		UnicodeStr ->
			% Will be readable and usable afterwards:
			PathDesc = text_utils:format( "<<\"~ts\"/utf8>>",
				[ file_utils:escape_path( UnicodeStr ) ] ),
			{ undefined, PathDesc }

	end,

	EntryToWrite = text_utils:format( "{file_info, ~ts ~ts, ~B, ~B}.~n",
								[ SHA1Str, PathToWrite, Size, Timestamp ] ),

	ToWrite = case MaybeComment of

		undefined ->
			EntryToWrite;

		Comm ->
			[ Comm, EntryToWrite, text_utils:format( "~n", [] ) ]

	end,

	file_utils:write_ustring( File, ToWrite ),

	write_entries( File, T ).



% @doc Checking on the SHA1:
get_file_content_for( SHA1, FileDataElems ) ->
	% Storage format a bit different from working one:
	[ { SHA1, RelativePath, Size, Timestamp }
		|| #file_data{ path=RelativePath,
					   % type: not to store
					   size=Size,
					   timestamp=Timestamp,
					   sha1_sum=RecSHA1 } <- FileDataElems, RecSHA1 =:= SHA1 ].





% @doc Reads as it is specified cache file, performs first checks and returns
% the corresponding tree data.
%
% Cache file expected to be already checked existing.
%
-spec read_cache_file( file_path(), user_state() ) -> tree_data().
read_cache_file( CacheFilePath, UserState ) ->

	trace_debug( "Reading cache file '~ts'.", [ CacheFilePath ], UserState ),

	try file_utils:read_terms( CacheFilePath ) of

		[ _HostInfo={ hostname, BinFQDN },
		  _RootInfo={ root_dir, BinCachedTreePath } | FileInfos ] ->
			#tree_data{ hostname=BinFQDN,
						root=BinCachedTreePath,
						entries=build_entry_table( FileInfos ),
						file_count=length( FileInfos )
						% Not managed (at least yet): the other counts.
					  };

		_Other ->
			trace( "Invalid cache file '~ts' (unexpected content); "
				"shall be removed by the user.", [ CacheFilePath ], UserState ),

			ui:display_error( "Error, cache file '~ts' does not have an "
				"expected content (abnormal).~n"
				"Consider removing this file first, and relaunching the "
				"operation again.", [ CacheFilePath ] ),
			stop( 7 )

	catch C:E ->

		trace( "Error (~ts), cache file '~ts' seems corrupted:~n ~p",
			   [ C, CacheFilePath, E ], UserState ),

		ui:display_error( "Error, cache file '~ts' seems corrupted "
			"(see logs for more details).~n"
			"Consider removing this file first, and relaunching the "
			"operation again.", [ CacheFilePath ] ),
		stop( 8 )

	end.



% @doc Spawns the specified number of data analyzers, and returns a list of
% their PID.
%
-spec spawn_data_analyzers( count(), user_state() ) -> [ analyzer_pid() ].
spawn_data_analyzers( Count, _UserState ) ->
	%trace_debug( "Spawning ~B data analyzers.", [ Count ], UserState ),
	[ ?myriad_spawn_link( fun() -> analyze_loop() end )
				|| _C <- lists:seq( 1, Count ) ].



% @doc Terminates specified data analyzers.
-spec terminate_analyzer_ring( ring_utils:ring( analyzer_pid() ),
							   user_state() ) -> void().
terminate_analyzer_ring( AnalyzerRing, UserState ) ->

	Analyzers = ring_utils:to_list( AnalyzerRing ),

	trace_debug( "Terminating ~B data analyzers (~p).",
				 [ length( Analyzers ), Analyzers ], UserState ),

	[ P ! terminate || P <- Analyzers ].



% @doc Scans for good the specified tree, whose path is expected to exist.
-spec scan_tree( directory_path(), analyzer_ring(), user_state() ) ->
						tree_data().
scan_tree( AbsTreePath, AnalyzerRing, UserState ) ->

	ui:display_instant( "Scanning '~ts'...", [ AbsTreePath ] ),
	trace_debug( "Scanning tree '~ts'...", [ AbsTreePath ], UserState ),

	% Regular ones (symlinks not of interest), even if their filename has a
	% (faulty?) non-Unicode encoding ("raw filename"):
	%
	AllFiles = find_regular_files_from( AbsTreePath ),

	% Not wanting to index our own files (if any already exists):
	FilteredFiles = lists:delete( ?merge_cache_filename, AllFiles ),

	trace_debug( "Found ~B regular files in filesystem: ~ts",
		[ length( FilteredFiles ),
		  text_utils:strings_to_string( FilteredFiles ) ], UserState ),

	% For lighter message sendings and storage, and because any raw filename is
	% already a binary:
	%
	FilteredBinFiles = text_utils:ensure_binaries( FilteredFiles ),

	scan_files( FilteredBinFiles, AbsTreePath, AnalyzerRing, UserState ).



% @doc Scans specified content files, using for that the specified analyzers,
% returning the corresponding tree data.
%
-spec scan_files( [ bin_file_path() ], bin_directory_path(), analyzer_ring(),
				  user_state() ) -> tree_data().
scan_files( Files, BinAbsTreePath, AnalyzerRing, UserState ) ->

	InitialTreeData = #tree_data{ hostname=net_utils:bin_localhost(),
								  root=BinAbsTreePath },

	scan_files( Files, InitialTreeData, AnalyzerRing, _WaitedCount=0,
				UserState ).


% (helper)
scan_files( _Files=[], TreeData, _AnalyzerRing, _WaitedCount=0, _UserState ) ->
	% In final state (none waited), hence directly returned:
	%trace_notice( "All file entries retrieved." ),
	TreeData;

scan_files( _Files=[], TreeData, _AnalyzerRing, WaitedCount, _UserState ) ->
	% Will return an updated tree data, once all answers are received:
	%trace_notice( "Final waiting for ~B entries.", [ WaitedCount ] ),
	wait_entries( TreeData, WaitedCount );

scan_files( _Files=[ Filename | T ], TreeData=#tree_data{ root=BinAbsTreePath },
			AnalyzerRing, WaitedCount, UserState ) ->

	{ AnalyzerPid, NewRing } = ring_utils:head( AnalyzerRing ),

	%trace_debug( "Requesting analysis of '~ts' by ~w.",
	%             [ FullPath, AnalyzerPid ] ),

	trace_debug( " - scanning ~ts", [ Filename ], UserState ),

	% WOOPER-style request:
	AnalyzerPid ! { analyzeFile, [ BinAbsTreePath, Filename ], self() },

	% Helps controlling flow and avoiding too large mailboxes on either side
	% (this main script, being slowed down, or the analyzers), by attempting to
	% receive once after each sending:
	%
	receive

		{ file_analyzed, FileData } ->
			NewTreeData = manage_received_data( FileData, TreeData ),
			% Plus one (sending) minus one (receiving) waited:
			scan_files( T, NewTreeData, NewRing, WaitedCount, UserState )

	after 0 ->

		% One sending, and no receiving here:
		scan_files( T, TreeData, NewRing, WaitedCount+1, UserState )

	end.



% @doc Manages specified received file data, and returns an updated tree data.
-spec manage_received_data( file_data(), tree_data() ) -> tree_data().
manage_received_data( FileData=#file_data{ type=Type, sha1_sum=Sum },
					  TreeData=#tree_data{ entries=Entries,
										   file_count=FileCount,
										   directory_count=DirCount,
										   symlink_count=SymlinkCount,
										   device_count=DeviceCount,
										   other_count=OtherCount } ) ->

	%trace_debug( "Data received: ~ts",
	%             [ file_data_to_string( FileData ) ] ),

	% Ensures that we associate a list to each SHA1 sum:
	NewEntries = case table:lookup_entry( Sum, Entries ) of

		key_not_found ->
			table:add_entry( Sum, [ FileData ], Entries );

		{ value, SumEntries } ->
			table:add_entry( Sum, [ FileData | SumEntries ], Entries )

	end,

	NewTreeData = TreeData#tree_data{ entries=NewEntries },

	case Type of

		regular ->
			NewTreeData#tree_data{ file_count=FileCount+1 };

		directory ->
			NewTreeData#tree_data{ directory_count=DirCount+1 };

		symlink ->
			NewTreeData#tree_data{ symlink_count=SymlinkCount+1 };

		device ->
			NewTreeData#tree_data{ device_count=DeviceCount+1 };

		other ->
			NewTreeData#tree_data{ other_count=OtherCount+1 }

	end.



% @doc Waits for the remaining file entries to be analyzed.
wait_entries( TreeData, _WaitedCount=0 ) ->
	%trace_debug( "All file entries waited for finally obtained." ),
	TreeData;

wait_entries( TreeData, WaitedCount ) ->

	%trace_debug( "Still waiting for ~B file entries.", [ WaitedCount ] ),

	receive

		{ file_analyzed, FileData } ->
			NewTreeData = manage_received_data( FileData, TreeData ),
			wait_entries( NewTreeData, WaitedCount-1 );

		{ file_disappeared, _BinFilePath } ->
			%trace_bridge:debug_fmt( "File '~ts' reported as having "
			%   "disappeared.", [ BinFilePath ] ),
			wait_entries( TreeData, WaitedCount-1 )

	end.



% @doc The loop run by each analyzer process.
-spec analyze_loop() -> void().
analyze_loop() ->

	%trace_bridge:debug_fmt( "Analyzer ~w waiting...", [ self() ] ),

	receive

		{ analyzeFile, [ AbsTreeBinPath, RelativeBinFilename ], SenderPid } ->

			BinFilePath =
				file_utils:bin_join( AbsTreeBinPath, RelativeBinFilename ),

			%trace_bridge:debug_fmt( "Analyzer ~w taking in charge '~ts'...",
			%                        [ self(), BinFilePath ] ),

			case file_utils:is_existing_file( BinFilePath ) of

				true ->
					FileData = #file_data{
						% We prefer storing relative filenames:
						path=RelativeBinFilename,
						type=file_utils:get_type_of( BinFilePath ),
						size=file_utils:get_size( BinFilePath ),
						timestamp=file_utils:get_last_modification_time(
												BinFilePath ),
						sha1_sum=hash_utils:compute_file_sha1_sum( BinFilePath )
								 },

					% To avoid overheating:
					timer:sleep( 50 ),

					SenderPid ! { file_analyzed, FileData },
					analyze_loop();

				false ->
					case file_utils:exists( BinFilePath ) of

						true ->
							Type = file_utils:get_type_of( BinFilePath ),
							trace_bridge:notice_fmt( "The type of entry '~ts' "
								"switched from regular (file) to ~ts.",
								[ BinFilePath, Type ] );

						false ->
							trace_bridge:notice_fmt( "The file '~ts' does not "
								"exist anymore.", [ BinFilePath ] )

					end,

					SenderPid ! { file_disappeared, BinFilePath },
					analyze_loop()

			end;


		{ checkNewFile, [ AbsTreeBinPath, RelativeBinFilename ], SenderPid } ->

			% We stay within binary world, as raw filenames could not be
			% converted to plain strings:

			BinFilePath =
				file_utils:bin_join( AbsTreeBinPath, RelativeBinFilename ),

			%trace_bridge:debug_fmt( "Analyzer ~w checking '~ts' "
			%                        [ self(), BinFilePath ] ),

			FileData = #file_data{
				% We prefer storing relative filenames:
				path=RelativeBinFilename,
				type=file_utils:get_type_of( BinFilePath ),
				size=file_utils:get_size( BinFilePath ),
				timestamp=file_utils:get_last_modification_time( BinFilePath ),
				sha1_sum=hash_utils:compute_file_sha1_sum( BinFilePath ) },

			SenderPid ! { file_checked, FileData },
			analyze_loop();


		terminate ->
			%trace_bridge:debug_fmt( "Analyzer ~w terminated.", [ self() ] ),
			ok

	end.



% @doc Interacts with the user so that the specified tree can be deduplicated
% ("uniquified").
%
-spec deduplicate_tree( tree_data(), user_state() ) -> tree_data().
deduplicate_tree( TreeData=#tree_data{ root=BinRootDir,
									   entries=EntryTable,
									   file_count=FileCount }, UserState ) ->

	InitialEntryCount = table:size( EntryTable ),

	DuplicateCount = FileCount - InitialEntryCount,

	% Check:
	false = DuplicateCount < 0,

	% Actual deduplication:
	{ NewEntryTable, RemovedCount } =
		manage_duplicates( EntryTable, BinRootDir, UserState ),

	% Possibly negative, should whole contents (i.e. all even "the original") be
	% erased:
	%
	RemainingDuplicateCount = DuplicateCount - RemovedCount,

	DupStr = case DuplicateCount of

		0 ->
			"no duplicate was";

		1 ->
			"a single duplicate was";

		_ ->
			text_utils:format( "~B duplicates were", [ DuplicateCount ] )

	end,

	RemoveStr = case RemovedCount of

		0 ->
			"no file was";

		1 ->
			"a single file was";

		_ ->
			text_utils:format( "a total of ~B files were",
							   [ RemovedCount ] )

	end,

	case ( DuplicateCount =/= 0 ) orelse ( RemoveStr =/= 0 ) of

		true ->
			ui:display( "While ~ts detected, ~ts removed.",
						[ DupStr, RemoveStr ] );

		% No useless display wanted:
		false ->
			ok

	end,

	NewFileCount = InitialEntryCount + RemainingDuplicateCount,

	%trace_debug( "~B unique entries remain.", [ NewFileCount ], UserState ),

	TreeData#tree_data{ entries=NewEntryTable,
						file_count=NewFileCount }.



% @doc Manages all duplicates found in specified table, returns an updated table
% and the number of files (usually only extra duplicates, sometimes *all* files
% corresponding to a given content) that have been removed.
%
-spec manage_duplicates( sha1_table(), bin_directory_path(), user_state() ) ->
								{ sha1_table(), count() }.
manage_duplicates( EntryTable, BinRootDir, UserState ) ->

	ContentEntries = table:enumerate( EntryTable ),

	% We could have forced that no duplication at all exists afterwards (and
	% then a given SHA1 sum would be associated to exactly one content), however
	% it would be too strict, hence we kept a list associated to each SHA1 sum.
	%
	% Two passes: one to establish and count the duplications, another to solve
	% them; returns a list of duplications, and a content table referencing all
	% non-duplicated entries.
	%
	{ DuplicationCases, UniqueTable } = filter_duplications( ContentEntries ),

	% UniqueTable contains all unique elements, while DuplicationCases contains
	% all non-unique ones.

	case length( DuplicationCases ) of

		0 ->
			ui:set_setting( title, "Deduplication report" ),
			ui:display( "No duplicated content detected." ),
			{ UniqueTable, _RemoveCount=0 };


		TotalDupCaseCount ->

			Prompt = case TotalDupCaseCount of

				1 ->
					text_utils:format( "A single case of content duplication "
						"detected in tree '~ts'.~n~nShall we:~n",
						[ BinRootDir ] );

				_ ->
					text_utils:format( "~B cases of content duplication "
						"detected in tree '~ts'.~n~nShall we:~n",
						[ TotalDupCaseCount, BinRootDir ] )

			end,

			Choices = [
				{ resolve, "Resolve duplication cases one by one" },
				{ auto, "Elect automatically (based on shortest path) "
					"for each case a reference file to which the other "
					"duplicates are symlinked" },
				{ abort, "Abort deduplication" } ],

			case ui:choose_designated_item( Prompt, Choices ) of

				resolve ->
					process_duplications( DuplicationCases, TotalDupCaseCount,
										  UniqueTable, BinRootDir, UserState );

				auto ->
					auto_deduplicate( DuplicationCases, TotalDupCaseCount,
									  UniqueTable, BinRootDir, UserState );

				C when C =:= abort orelse C =:= ui_cancel ->
					ui:unset_setting( title ),
					ui:display( "Deduplication aborted." ),
					trace_debug( "(requested to abort the deduplication)",
								 UserState ),
					stop_user_service( UserState )

			end

	end.



% @doc Filters the duplications from specified content entries: returns the
% actual duplications in a list, put the unique files in a new table.
%
-spec filter_duplications( [ sha1_entry() ] ) ->
									{ [ sha1_entry() ], sha1_table() }.
filter_duplications( SHA1Entries ) ->
	% Far better than a fold:
	filter_duplications( SHA1Entries, _Acc={ _DupEntries=[], table:new() } ).


% Returns {AccDupEntries, AccUniqueTable}:
filter_duplications( _SHA1Entry=[], Acc ) ->
	Acc;

% By design V is never empty:
filter_duplications( _SHA1Entry=[ { Sha1Key, V=[ _SingleContent ] } | T ],
					 _Acc={ AccDupEntries, AccUniqueTable } ) ->
	% Single content, hence unique (and an extraneous checking):
	NewTable = table:add_new_entry( Sha1Key, V, AccUniqueTable ),
	filter_duplications( T, { AccDupEntries, NewTable } );

% SHA1Entry is {Sha1Key, V} with at least two elements in V here:
filter_duplications( _SHA1Entries=[ SHA1Entry | T ],
					 _Acc={ AccDupEntries, AccUniqueTable } ) ->
	% So at least one duplicate here:
	NewDupEntries = [ SHA1Entry | AccDupEntries ],
	filter_duplications( T, { NewDupEntries, AccUniqueTable } ).



% @doc Processes the spotted duplications by asking the user.
-spec process_duplications( [ sha1_entry() ], count(), sha1_table(),
			bin_directory_path(), user_state() ) -> { sha1_table(), count() }.
process_duplications( DuplicationCases, TotalDupCaseCount, UniqueTable,
					  BinRootDir, UserState ) ->

	%trace_debug( "Pre-deduplicating unique table: ~ts",
	%			 [ table:to_string( UniqueTable ) ], UserState ),

	% DupCount: the number of duplication *cases* (each potentially with several
	% duplicates)
	%
	Acc0 = { UniqueTable, _InitialDupCount=1, _InitialRemoved=0 },

	process_duplications_helper( DuplicationCases, TotalDupCaseCount, Acc0,
								 BinRootDir, UserState ).



% Helper returning {UpdatedTable :: sha1_table(), RemoveCount :: count()}:
process_duplications_helper( _DupCases=[], _TotalDupCount,
		_Acc={ AccTable, _AccDupCount, AccRemoveCount }, _BinRootDir,
		_UserState ) ->

	%trace_debug( "Post-deduplication unique table: ~ts",
	%             [ table:to_string( AccTable ) ], UserState ),

	{ AccTable, AccRemoveCount };

process_duplications_helper( _DupCases=[ { Sha1Key, DuplicateList } | T ],
		TotalDupCount, _Acc={ AccTable, AccDupCount, AccRemoveCount },
		BinRootDir, UserState ) ->

	Size = check_duplicates( Sha1Key, DuplicateList ),

	% Returns an updated table and a list of the files containing that content:
	{ NewAccTable, RemainingFileEntries } = case manage_duplication_case(
			DuplicateList, AccDupCount, TotalDupCount, Size, BinRootDir,
			UserState ) of

		[] ->
			{ table:remove_entry( Sha1Key, AccTable ), [] };

		SelectFileEnts ->
			{ table:add_entry( Sha1Key, SelectFileEnts, AccTable ),
			  SelectFileEnts }

	end,

	NewRemoveCount = AccRemoveCount + length( DuplicateList )
								- length( RemainingFileEntries ),

	NewAcc = { NewAccTable, AccDupCount+1, NewRemoveCount },

	process_duplications_helper( T, TotalDupCount, NewAcc, BinRootDir,
								 UserState ).



% @doc Checks a duplication set: same SHA1 sum and also size must be found for
% all file entries (would most probably detect any SHA1 collision, however
% unlikely it maybe); returns the (common) size.
%
-spec check_duplicates( sha1(), [ file_data() ] ) -> byte_size().
% Not possible: check_duplicates( _SHA1Sum, _DuplicateList=[] ) ->
%	ok;

% Use the first element to determine the (common) size:
check_duplicates( SHA1Sum, _DuplicateList=[
	   #file_data{ path=FirstPath, sha1_sum=SHA1Sum, size=Size } | T ] ) ->
	check_duplicates( SHA1Sum, FirstPath, Size, T ).


% (helper)
check_duplicates( _SHA1Sum, _FirstPath, Size, _DuplicateList=[] ) ->
	Size;

check_duplicates( SHA1Sum, FirstPath, Size, _DuplicateList=[
		#file_data{ sha1_sum=SHA1Sum, size=Size } | T ] ) ->
	check_duplicates( SHA1Sum, FirstPath, Size, T );

% (and a different SHA1 sum would trigger a case clause)
check_duplicates( SHA1Sum, FirstPath, Size, _DuplicateList=[
	  #file_data{ path=OtherPath, sha1_sum=SHA1Sum, size=OtherSize } | _T ] ) ->

	throw( { sha1_collision_detected, SHA1Sum, { FirstPath, Size },
				{ OtherPath, OtherSize } } ).



% @doc Manages specified duplicated entries.
%
% Returns the (regular) files that remain for that content.
%
-spec manage_duplication_case( [ file_data() ], count(), count(),
		byte_size(), bin_directory_path(), user_state() ) -> [ file_data() ].
manage_duplication_case( FileEntries, DuplicationCaseCount, TotalDupCaseCount,
						 Size, BinRootDir, UserState ) ->

	%trace_bridge:debug_fmt( "PathStrings = ~p", [ PathStrings ] ),

	Title = text_utils:format( "Examining duplication case ~B/~B",
							   [ DuplicationCaseCount, TotalDupCaseCount ] ),

	ui:set_setting( title, Title ),

	Count = length( FileEntries ),

	SizeString = system_utils:interpret_byte_size_with_unit( Size ),

	BinPaths = lists:sort( [ E#file_data.path || E <- FileEntries ] ),

	% By design more than one path: text_utils:get_longest_common_path/1 should
	% not be used, as for example a 'foobar-new' directory could be a sibling of
	% a 'foobar' directory, resulting in '-new/...' meaningless suffixes; so we
	% use the path-aware file_utils version thereof:
	%
	{ Prompt, Prefix, BinShortenPaths } =
			case file_utils:get_longest_common_path( BinPaths ) of

		% No common prefix at all here:
		{ _Prfx= <<"">>, Tails } ->

			Lbl = text_utils:format( "Following ~B files have the "
				"exact same content (and thus size, of ~ts)",
				[ Count, SizeString ] ),

			{ Lbl, _Prefix="", _TrimmedPaths=Tails };


		{ Prfx, Tails } ->

			Lbl = text_utils:format( "Following ~B files have the "
				"exact same content (and thus size, of ~ts) and all start "
				"with the same prefix, '~ts' (omitted below)",
				[ Count, SizeString, Prfx ] ),

			{ Lbl, Prfx, _TrimmedPaths=Tails }

	end,

	%trace_bridge:debug_fmt( "BinShortenPaths = ~p", [ BinShortenPaths ] ),

	% Listing the shortened path of all duplicates:
	DuplicateString = text_utils:format( ": ~ts",
		[ text_utils:binaries_to_binary( BinShortenPaths, ?bullet_point ) ] ),

	% Shortened if too long, otherwise might be a problem for the UI:
	FullPrompt = text_utils:format_ellipsed( "~ts~ts",
						[ Prompt, DuplicateString ], ?max_message_header_len ),

	Choices = [ { auto_remove, "Auto-select, and just remove duplicates "
				  "(no symlink created)" },
				{ auto_symlink, "Auto-select shortest path as "
				  "reference, replace other duplicates by symlinks" },
				{ keep, "Elect a reference file, remove other duplicates" },
				{ elect, "Elect a reference file, replace other duplicates "
				  "by symlinks" },
				{ leave, "Leave them as they are" },
				{ delete, "Delete them as a whole" },
				{ abort, "Abort" } ],

	SelectedChoice = ui:choose_designated_item(
		text_utils:format( "~ts~n~nDeduplication choices are:",
						   [ FullPrompt ] ),
		Choices ),

	ui:unset_setting( title ),

	%trace_debug( "Selected choice: ~p", [ SelectedChoice ], UserState ),

	case SelectedChoice of

		auto_remove ->
			KeptFilePath = keep_shortest_path( Prefix, BinShortenPaths,
				BinRootDir, _CreateSymlinks=false, UserState ),

			trace_debug( "Kept only auto-selected reference file '~ts', "
				"with no symlink created.", [ KeptFilePath ], UserState ),

			%trace_bridge:debug_fmt( "Entries to scan: ~p", [ FileEntries ] ),

			% As this is a list of file_data:
			[ find_data_entry_for( KeptFilePath, FileEntries ) ];


		auto_symlink ->
			KeptFilePath = keep_shortest_path( Prefix, BinShortenPaths,
				BinRootDir, _CreateSymlinks=true, UserState ),

			trace_debug( "Kept only auto-selected reference file '~ts', "
				"with symlinks created.", [ KeptFilePath ], UserState ),

			%trace_bridge:debug_fmt( "Entries to scan: ~p", [ FileEntries ] ),

			% As this is a list of file_data:
			[ find_data_entry_for( KeptFilePath, FileEntries ) ];


		keep ->
			KeptFilePath = keep_only_one( Prefix, BinShortenPaths, BinPaths,
										  BinRootDir, UserState ),

			trace_debug( "Kept only selected reference file '~ts'",
						 [ KeptFilePath ], UserState ),

			%trace_bridge:debug_fmt( "Entries to scan: ~p", [ FileEntries ] ),

			% As this is a list of file_data:
			[ find_data_entry_for( KeptFilePath, FileEntries ) ];

		elect ->
			% Symlinks ignored:
			ElectedFilePath = elect_and_link( Prefix, BinShortenPaths, BinPaths,
											  BinRootDir, UserState ),

			% As this is a list of file_data:
			[ find_data_entry_for( ElectedFilePath, FileEntries ) ];


		leave ->
			PrefixString = case Prefix of

				"" ->
					"";

				Prefix ->
					text_utils:format( " (prefix: '~ts')", [ Prefix ] )

			end,

			trace_debug( "[~B/~B] Leaving as they are~ts: ~ts",
				[ DuplicationCaseCount, TotalDupCaseCount, PrefixString,
				  DuplicateString ], UserState ),

			FileEntries;


		delete ->
			DelPrompt = text_utils:format( "Really delete all ~B "
				"elements found in '~ts' corresponding to that same content?~n"
				"(this content would thus be lost by that tree)"
				"~n~nFollowing files would then be removed~ts" ,
				[ Count, BinRootDir, DuplicateString ] ),

			case ui:ask_yes_no( DelPrompt ) of

				yes ->
					Paths =
					  [ file_utils:bin_join( BinRootDir, P ) || P <- BinPaths ],

					remove_files( Paths, UserState ),
					% Deleted in tree:
					[];

				no ->
					% Back to the same:
					manage_duplication_case( FileEntries, DuplicationCaseCount,
								TotalDupCaseCount, Size, BinRootDir, UserState )

			end;


		C when C =:= abort orelse C =:= ui_cancel ->
			ui:unset_setting( title ),
			ui:display( "Uniquification aborted, stopping now." ),
			trace_debug( "(requested to abort the merge)", UserState ),
			stop_user_service( UserState )

	end.



% @doc Deduplicates automatically the specified cases.
%
% Here we resolve automatically all cases, by selecting the shortest of the
% duplicate filenames and by transforming the others into symlinks pointing to
% it.
%
-spec auto_deduplicate( [ sha1_entry() ], count(), sha1_table(),
			bin_directory_path(), user_state() ) -> { sha1_table(), count() }.
auto_deduplicate( DuplicationCases, _TotalDupCaseCount, UniqueTable, BinRootDir,
				  UserState ) ->

	trace_debug( "Auto-deduplicating unique table: ~ts",
				 [ table:to_string( UniqueTable ) ], UserState ),

	auto_dedup( DuplicationCases, UniqueTable, _AccRemoveCount=0, BinRootDir,
				UserState ).


% Helper.
auto_dedup( _DuplicationCases=[], AccTable, AccRemoveCount, _BinRootDir,
			_UserState ) ->
	{ AccTable, AccRemoveCount };

auto_dedup( _DuplicationCases=[ { Sha1Key, DuplicateList } | T ], AccTable,
			AccRemoveCount, BinRootDir, UserState ) ->

	% [{count(), file_path(), file_data()}] (ties are broken by second element
	% of each triplet, the string path):
	%
	SortedTriplets = lists:sort( [
		begin
			PathString = text_utils:binary_to_string( FD#file_data.path ),
			{ length( PathString ), PathString, FD }
		end || FD <- DuplicateList ] ),

	% Shortest (due to Erlang term ordering) to become the reference:
	[ { _RefLen, RefPath, RefFD } | OtherFDTriplets ] = SortedTriplets,

	AbsRefPath = file_utils:bin_join( BinRootDir, RefPath ),

	SymLnkPaths = [ P || { _L, P, _FD } <- OtherFDTriplets ],

	trace_debug( "Transforming, in '~ts', following files into symlinks "
		"pointing to the auto-elected reference version '~ts': ~ts",
		[ BinRootDir, RefPath, text_utils:strings_to_string( SymLnkPaths ) ],
		UserState ),

	[ begin

			AbsLnkPath = file_utils:bin_join( BinRootDir, LnkPath ),

			remove_file( AbsLnkPath, UserState ),

			RelTargetPath = file_utils:make_relative( AbsRefPath,
									file_utils:get_base_path( AbsLnkPath ) ),

			file_utils:create_link( RelTargetPath, AbsLnkPath )

	  end || LnkPath <- SymLnkPaths ],

	NewAccTable = table:add_entry( Sha1Key, [ RefFD ], AccTable ),

	auto_dedup( T, NewAccTable, AccRemoveCount + length( OtherFDTriplets ),
				BinRootDir, UserState ).



% @doc Returns the file_data record in the specified list that corresponds to
% the specified path.
%
-spec find_data_entry_for( file_path(), [ file_data() ] ) -> file_data().
find_data_entry_for( FilePath, _FileEntries=[] ) ->
	throw( { not_found, FilePath } );

% Match:
find_data_entry_for( FilePath,
					 _FileEntries=[ FD=#file_data{ path=FilePath } | _T ] ) ->
	FD;

find_data_entry_for( FilePath, _FileEntries=[ _FD | T ] ) ->
	find_data_entry_for( FilePath, T ).



% @doc Selects among the specified files the one with the shortest path, which
% is kept as is, while, if requested, the others are replaced by symlinks
% pointing to it, and returns its filename as a binary.
%
-spec keep_shortest_path( ustring(), [ bin_file_path() ], bin_directory_path(),
						  boolean(), user_state() ) -> bin_file_path().
keep_shortest_path( Prefix, TrimmedPaths, BinRootDir, CreateSymlinks,
					UserState ) ->

	% They all have the same prefix; we rely here on Erlang term ordering, where
	% 'undefined' is greater than any integer ('raw filenames' cannot even have
	% their length determined; as a result they are naturally eliminated if at
	% least one duplicate has a "normal" filename; ties are resolved also
	% thanks to sorting):
	%
	_AscendingPairs = [ _H={ _SmallestLen, KeptFilePath } | LongerPairs ] =
		lists:sort(
		  [ { text_utils:safe_length( P ), P } || P <- TrimmedPaths ] ),

	ToRemovePaths = [ P || { _Len, P } <- LongerPairs ],

	trace_debug( "Keeping auto-selected '~ts', removing longer ones "
		"(based on common prefix '~ts' and root directory '~ts'): ~ts ",
		[ KeptFilePath, Prefix, BinRootDir,
		  text_utils:binaries_to_string( ToRemovePaths ) ], UserState ),

	FutureLinkPaths =
		[ file_utils:bin_join( Prefix, P ) || P <- ToRemovePaths ],

	ToRemoveFullPaths =
		[ file_utils:bin_join( BinRootDir, P ) || P <- FutureLinkPaths ],

	remove_files( ToRemoveFullPaths, UserState ),

	CreateSymlinks andalso
		begin
			AbsKeptFilePath =
				file_utils:join( [ BinRootDir, Prefix, KeptFilePath ] ),
			create_links_to( AbsKeptFilePath, FutureLinkPaths, BinRootDir )
		end,

	file_utils:bin_join( [ Prefix, KeptFilePath ] ).




% @doc Selects among the specified files the single one that shall be kept while
% the others are removed, and returns its filename as a binary.
%
-spec keep_only_one( ustring(), [ bin_file_path() ], [ bin_file_path() ],
					 bin_directory_path(), user_state() ) -> bin_file_path().
keep_only_one( Prefix, TrimmedPaths, PathStrings, BinRootDir, UserState ) ->

	ui:set_setting( title,
					_Title="Selecting the unique reference version to keep, "
						   "whereas the others are to be removed" ),

	BasePrompt = text_utils:format( "~nPlease choose the (single) file to keep "
									"(others being removed), among:", [] ),

	Prompt = case Prefix of

		"" ->
			BasePrompt;

		_ ->
			text_utils:format( "~ts~n(common prefix '~ts' omitted)",
							   [ BasePrompt, Prefix ] )

	end,

	KeptIndex = case ui:choose_numbered_item( Prompt, _Choices=TrimmedPaths ) of

		% ui_cancel:
		0 ->
			throw( operation_cancelled );

		I ->
			I

	end,

	ui:unset_setting( title ),

	{ BinKeptFilePath, ToRemovePaths } =
		list_utils:extract_element_at( PathStrings, KeptIndex ),

	trace_debug( "Keeping selected '~ts', removing (based on common "
		"prefix '~ts' and root directory '~ts'): ~ts ",
		[ BinKeptFilePath, Prefix, BinRootDir,
		  text_utils:strings_to_string( ToRemovePaths ) ], UserState ),

	ToRemoveFullPaths =
		[ file_utils:bin_join( BinRootDir, P ) || P <- ToRemovePaths ],

	remove_files( ToRemoveFullPaths, UserState ),

	BinKeptFilePath.



% @doc Selects among the specified files the single one that shall be elected
% and kept, while the others are removed and replaced by symlinks pointing to
% that file, and returns it as a binary.
%
-spec elect_and_link( ustring(), [ bin_file_path() ], [ bin_file_path() ],
					  bin_directory_path(), user_state() ) -> bin_file_path().
elect_and_link( Prefix, TrimmedPaths, PathStrings, BinRootDir, UserState ) ->

	ui:set_setting( title, _Title="Selecting the unique version to elect "
					"as a reference, whereas the others will be replaced "
					"by symbolic links pointing to it" ),

	BasePrompt = text_utils:format(
	   "~nPlease choose the (single) file to elect, among:", [] ),

	Prompt = case Prefix of

		"" ->
			BasePrompt;

		_ ->
			text_utils:format( "~ts~n(common prefix '~ts' omitted)",
							   [ BasePrompt, Prefix ] )

	end,

	ElectedIndex = case
			 ui:choose_numbered_item( Prompt, _Choices=TrimmedPaths ) of

		% ui_cancel:
		0 ->
			throw( operation_cancelled );

		I ->
			I

	end,

	ui:unset_setting( title ),

	{ BinElectedFilePath, FutureLinkPaths } =
		list_utils:extract_element_at( PathStrings, ElectedIndex ),

	trace_debug( "Electing '~ts', replacing by symlinks (based on common "
		"prefix '~ts' and root directory '~ts'): ~ts ",
		[ BinElectedFilePath, Prefix, BinRootDir,
		  text_utils:strings_to_string( FutureLinkPaths ) ], UserState ),

	ToRemoveFullPaths =
		[ file_utils:bin_join( BinRootDir, P ) || P <- FutureLinkPaths ],

	remove_files( ToRemoveFullPaths, UserState ),

	create_links_to( BinElectedFilePath, FutureLinkPaths, BinRootDir ),

	BinElectedFilePath.



% Creates relative, symbolic links to the specified file path.
create_links_to( _TargetFilePath, _LinkPaths=[], _BinRootDir ) ->
	ok;

create_links_to( TargetFilePath, _LinkPaths= [ Link | T ], BinRootDir ) ->

	% We want to create the (shortest) relative link, from source to target:

	LinkDir = file_utils:get_base_path( Link ),

	RelativeTargetFilePath =
		file_utils:make_relative( TargetFilePath, LinkDir ),

	file_utils:create_link( RelativeTargetFilePath,
								file_utils:bin_join( BinRootDir, Link ) ),

	create_links_to( TargetFilePath, T, BinRootDir ).



% @doc Performs a quick check (i.e. with no checksum computed of the file
% contents) of the specified tree, against the specified cache file: check that
% both file sets match (no extra element on either size) and that the cached and
% actual file sizes match as well.
%
-spec quick_cache_check( file_path(), [ file_path() ], bin_directory_path(),
					analyzer_ring(), user_state() ) -> maybe( tree_data() ).
quick_cache_check( CacheFilePath, ContentFiles, BinTreePath, AnalyzerRing,
				   UserState ) ->

	try file_utils:read_terms( CacheFilePath ) of

		[ _HostInfo={ hostname, BinFQDN },
		  _RootInfo={ root_dir, BinCachedTreePath } | FileInfos ] ->
				quick_cache_check_helper( BinFQDN, ContentFiles, BinTreePath,
					BinCachedTreePath, FileInfos, AnalyzerRing, UserState );

		_Other ->
			trace_debug( "Invalid cache file '~ts', removing it and "
						 "recreating it.", [ CacheFilePath ], UserState ),
			remove_file( CacheFilePath, UserState ),
			undefined

	catch throw:{ interpretation_failed, _Filename, _Line, _Mod, _Term,
				  Reason } ->

		ui:display( "Error while reading '~ts': \"~ts\", removing it, "
					"and recreating it.", [ CacheFilePath, Reason ] ),

		undefined

	end.



% (helper)
-spec quick_cache_check_helper( bin_fqdn(), [ bin_file_path() ],
		bin_directory_path(), bin_directory_path(), [ file_info() ],
		analyzer_ring(), user_state() ) -> maybe( tree_data() ).
quick_cache_check_helper( BinFQDN, ContentFiles, BinActualTreePath,
			BinCachedTreePath, FileInfos, AnalyzerRing, UserState ) ->

	BinAbsActualTreePath =
		file_utils:ensure_path_is_absolute( BinActualTreePath ),

	case BinCachedTreePath of

		BinAbsActualTreePath ->
			BinActualTreePath;

		_ ->
			NamePrompt = text_utils:format( "The actual tree path ('~ts') does "
				"not match the one found in its cache file ('~ts').~n~n"
				"Shall we override the one in the cache file with the "
				"actual one?", [ BinAbsActualTreePath, BinCachedTreePath ] ),

			case ui:ask_yes_no( NamePrompt ) of

				yes ->
					trace_debug( "Overriding tree path in the cache file "
						"('~ts') with the actual one ('~ts').",
						[ BinCachedTreePath, BinAbsActualTreePath ],
						UserState ),
					BinActualTreePath;

				no ->
					trace_debug( "Not overriding tree path in the cache file "
						"('~ts') with the actual one ('~ts'), failing.",
						[ BinCachedTreePath, BinAbsActualTreePath ],
						UserState ),

					throw( { non_matching_tree_paths, BinCachedTreePath,
							 BinActualTreePath } )

			end

	end,

	ActualFileCount = length( ContentFiles ),

	CachedFileCount = length( FileInfos ),

	CachedFilePairs = [ { Path, Size }
			|| { file_info, _SHA1, Path, Size, _Timestamp } <- FileInfos ],

	case ActualFileCount of

		CachedFileCount ->
			trace_debug( "Cached and actual file counts match (~B files).",
						 [ CachedFileCount ], UserState );

		_ ->
			% Does not fail immediately, for a better error report:
			trace_debug( "The cached and actual file counts do not match: "
				" ~B are referenced in cache, ~B exist in the filesystem.",
				[ CachedFileCount, ActualFileCount ], UserState )

	end,

	CachedFilenames =
		[ FilePath || { FilePath, _FileSize } <- CachedFilePairs ],

	CachedFileset = set_utils:new( CachedFilenames ),
	ActualFileset = set_utils:new( ContentFiles ),

	{ OnlyCachedSet, OnlyActualSet } =
		set_utils:differences( CachedFileset, ActualFileset ),

	FilesystemIsComplete = set_utils:is_empty( OnlyCachedSet ),

	case FilesystemIsComplete of

		true ->
			trace_debug( "Filesystem contains all content referenced "
						 "in cache.", [], UserState );

		false ->
			% Some content disappeared:
			OnlyCacheList = set_utils:to_list( OnlyCachedSet ),
			trace_debug( "Following ~B files are referenced in "
				"cache, yet do not exist on the filesystem: ~ts",
				[ length( OnlyCacheList ),
				  text_utils:binaries_to_binary( OnlyCacheList ) ],
				UserState )

	end,

	CacheIsComplete = set_utils:is_empty( OnlyActualSet ),

	case CacheIsComplete of

		true ->
			trace_debug( "Cache references all content in filesystem.", [],
						 UserState );

		false ->
			% Some content appeared:
			OnlyActualList = set_utils:to_list( OnlyActualSet ),
			trace_debug( "Following ~B files exist on the "
				"filesystem, yet are not referenced in cache: ~ts",
				[ length( OnlyActualList ),
				  text_utils:binaries_to_binary( OnlyActualList ) ],
				UserState )

	end,

	MustRescan = not ( FilesystemIsComplete and CacheIsComplete ),

	trace_debug( "Must rescan: ~ts.", [ MustRescan ], UserState ),

	case MustRescan of

		true ->
			rescan( BinAbsActualTreePath, AnalyzerRing, UserState );

		false ->

			trace_debug( "In '~ts', the file paths and names match the cache.",
						 [ BinActualTreePath ], UserState ),

			% The two sets match, yet do they agree on the file sizes as well?
			%
			% (CachedFilePairs tells us both the paths and the expected sizes,
			% hence no need for ContentFiles)
			%
			case check_file_sizes_match( CachedFilePairs, BinActualTreePath,
										 UserState ) of

				% Alles gut, so create the corresponding receptacle:
				true ->
					trace_debug( "All sizes of the ~B files match in '~ts'.",
						[ CachedFileCount, BinActualTreePath ], UserState ),

					#tree_data{ hostname=BinFQDN,
								root=BinAbsActualTreePath,
								entries=build_entry_table( FileInfos ),
								file_count=CachedFileCount
								% Not managed (at least yet): the other counts.
								};

				false ->
					trace_debug( "At least one file size does not match "
						"in '~ts', rescanning.", [ BinActualTreePath ],
						UserState ),
					rescan( BinAbsActualTreePath, AnalyzerRing, UserState )

			end

	end.



% @doc Builds the entry table from specified terms.
-spec build_entry_table( [ file_info() ] ) -> sha1_table().
build_entry_table( FileInfos ) ->

	EntryTable = table:new(),

	build_entry_table( FileInfos, EntryTable ).


% (helper)
build_entry_table( _FileInfos=[], EntryTable ) ->
	EntryTable;

build_entry_table(
  _FileInfos=[ { file_info, SHA1Str, BinRelativePath, Size, Timestamp } | T  ],
  EntryTable ) ->

	SHA1 = text_utils:hexastring_to_integer( SHA1Str, _ExpectPrefix=false ),

	FileData = #file_data{ path=BinRelativePath,
						   type=regular,
						   size=Size,
						   timestamp=Timestamp,
						   sha1_sum=SHA1 },

	NewEntryTable = table:append_to_entry( SHA1, FileData, EntryTable ),

	build_entry_table( T, NewEntryTable );

build_entry_table( _FileInfos=[ Unexpected | _T ], _EntryTable ) ->
	trace_utils:error_fmt( "Unexpected entry read from cache file:~n  ~p",
						   [ Unexpected ] ),
	throw( { unexpected_entry, Unexpected } ).



% @doc Checks that the actual file sizes match the specified ones.
-spec check_file_sizes_match( [ { file_path(), byte_size() } ],
							  directory_path(), user_state() ) -> boolean().
check_file_sizes_match( _FilePairs=[], _TreePath, _UserState ) ->
	true;

check_file_sizes_match( _FilePairs=[ { FilePath, FileSize } | T ], TreePath,
						UserState ) ->

	FileFullPath = file_utils:bin_join( TreePath, FilePath ),

	case file_utils:get_size( FileFullPath ) of

		FileSize ->
			check_file_sizes_match( T, TreePath, UserState );

		ActualSize ->
			trace_debug( "For file '~ts', cached size is ~ts (~B bytes), "
				"whereas actual size is ~ts (~B bytes), invalidating thus "
				"cache file.",
				[ FileFullPath, system_utils:interpret_byte_size( FileSize ),
				  FileSize, system_utils:interpret_byte_size( ActualSize ),
				  ActualSize ], UserState ),
			false

	end.



% @doc Returns a pair made of the path of the delta file corresponding to the
% content in first space and not in second, and of the (opened) delta file
% itself.
%
% Any pre-existing file on the same name will be removed.
% The returned file shall be closed by the caller.
%
-spec prepare_delta( bin_fqdn(), bin_directory_path(), bin_fqdn(),
		bin_directory_path() ) -> { bin_directory_path(), file() }.
prepare_delta( FirstHostname, FirstRootPath, SecondHostname,
			   SecondRootPath ) ->

	% Avoiding too long FQDNs, yet maintaining them unique if on the same host:
	{ FirstShortPath, SecondShortPath } = case FirstHostname of

		SecondHostname ->
			file_utils:get_shortest_unique_ending_paths( FirstRootPath,
														 SecondRootPath );

		% No collision possible if different hosts:
		_ ->
			{ file_utils:get_last_path_element( FirstRootPath ),
			  file_utils:get_last_path_element( SecondRootPath ) }

	end,

	[ FirstDesc, SecondDesc ] = [ string:replace( P, _SearchPattern="/",
												  _Replacement="-", _Where=all )
								  || P <- [ FirstShortPath, SecondShortPath ] ],

	DeltaFilename = text_utils:format(
		"merge-elements-in-~ts-on-~ts-but-not-in-~ts-on-~ts.txt",
		[ FirstDesc, file_utils:convert_to_filename(
			net_utils:get_hostname( FirstHostname ) ),
		  SecondDesc, file_utils:convert_to_filename(
			net_utils:get_hostname( SecondHostname ) ) ] ),

	% Not wanting this file to be created in the 'merge-tool' directory,
	% preferring the root of the user account (so in neither tree paths):
	%
	%DeltaFilePath = file_utils:any_join( file_utils:get_current_directory(),
	%                                     DeltaFilename ),
	%
	DeltaFilePath = file_utils:any_join(
		system_utils:get_user_home_directory(), DeltaFilename ),

	case file_utils:is_existing_file_or_link( DeltaFilePath ) of

		true ->
			trace_utils:warning_fmt(
			  "Overwriting preexisting delta file '~ts'.", [ DeltaFilePath ] ),
			file_utils:remove_file( DeltaFilePath );

		false ->
			ok

	end,

	% Same options apply:
	DeltaFile = file_utils:open( DeltaFilePath, ?merge_file_options ),

	{ DeltaFilePath, DeltaFile }.



% @doc Returns a textual description of specified lacking cached content, and
% writes a corresponding information in specified opened file (if any).
%
-spec list_lacking_content( [ sha1() ], sha1_table(), ustring(),
			maybe( bin_directory_path() ), maybe( file() ) ) -> ustring().
list_lacking_content( _SHA1s=[ SHA1 ], SHA1Table, OtherTreeDesc, MaybeRootPath,
					  MaybeDeltaFile ) ->

	Str = text_utils:format_ellipsed( "The content lacking in ~ts tree is ~ts.",
		[ OtherTreeDesc,
		  describe_content( SHA1, SHA1Table, MaybeRootPath, MaybeDeltaFile ) ],
		?max_text_length ),

	MaybeDeltaFile =:= undefined orelse file_utils:close( MaybeDeltaFile ),

	Str;


list_lacking_content( SHA1s, SHA1Table, OtherTreeDesc, MaybeRootPath,
					  MaybeDeltaFile ) ->

	Str = text_utils:format_ellipsed(
		"The contents lacking in ~ts tree are: ~ts",
		[ OtherTreeDesc, text_utils:strings_to_enumerated_string(
			[ describe_content( S, SHA1Table, MaybeRootPath, MaybeDeltaFile )
				|| S <- SHA1s ] ) ], ?max_text_length ),

	MaybeDeltaFile =:= undefined orelse file_utils:close( MaybeDeltaFile ),

	Str.



% (helper)
describe_content( SHA1, SHA1Table, RootPath, MaybeDeltaFile ) ->

	{ ContentPath, Str } = case table:get_value( SHA1, SHA1Table ) of

		[ _SingleFileData=#file_data{ path=UniqContentPath } ] ->
			{ UniqContentPath,
			  text_utils:format( "(solely) in '~ts'", [ UniqContentPath ] ) };

		[ FirstFileData, _SecondFileData ] ->
			FirstContentPath = FirstFileData#file_data.path,
			{ FirstContentPath, text_utils:format(
				"in '~ts' (and in another duplicate)",
				[ FirstContentPath ] ) };

		[ FirstFileData| T ] ->
			FirstContentPath = FirstFileData#file_data.path,
			{ FirstContentPath, text_utils:format(
				"in '~ts' (and in ~B other duplicates)",
				[ FirstContentPath, length( T ) ] ) }

	end,

	MaybeDeltaFile =:= undefined orelse
		begin
			AbsContentPath = file_utils:any_join( RootPath, ContentPath ),
			file_utils:write_ustring( MaybeDeltaFile, "~ts~n",
									  [ AbsContentPath ] )
		end,

	Str.



% (centralising helper)
-spec find_regular_files_from( directory_path() ) -> [ bin_file_path() ].
find_regular_files_from( TreePath ) ->

	AllFiles = file_utils:find_files_from( TreePath, _IncludeSymlinks=false,
										   _IfImproperEncoding=include ),

	% Raw filenames are already binaries:
	text_utils:ensure_binaries( AllFiles ).



% @doc Tells whether the specified tree is uniquified.
-spec is_uniquified( tree_data() ) -> boolean().
is_uniquified( #tree_data{ entries=EntryTable } ) ->
	FileDataLists = table:values( EntryTable ),
	is_uniquified_helper( FileDataLists ).


% (helper)
is_uniquified_helper( _FileDataLists=[] ) ->
	true;

is_uniquified_helper( _FileDataLists=[ [ _SingleFileData ] | T ] ) ->
	is_uniquified_helper( T );

% Meaning a list of at least two file_data:
is_uniquified_helper( _FileDataLists ) ->
	false.



% @doc Returns a textual description of the specified niquification status.
-spec uniquified_to_string( boolean() ) -> ustring().
uniquified_to_string( true ) ->
	"is uniquified";

uniquified_to_string( false ) ->
	"is not uniquified".



% @doc Returns a textual description of the count of the specified content.
-spec count_content( [ sha1() ] | count() ) -> ustring().
count_content( L ) when is_list( L ) ->
	count_content( length( L ) );

count_content( _Count=0 ) ->
	"no content";

count_content( _Count=1 ) ->
	"a single unique content";

count_content( Count ) ->
	text_utils:format( "~B unique contents", [ Count ] ).




% @doc Removes specified file.
-spec remove_file( bin_file_path(), user_state() ) -> void().
remove_file( FileToRemove, UserState ) ->
	trace_debug( " - removing file '~ts'", [ FileToRemove ], UserState ),
	file_utils:remove_file( FileToRemove ).


% @doc Removes specified files.
-spec remove_files( [ bin_file_path() ], user_state() ) -> void().
remove_files( _FilesToRemove=[], _UserState ) ->
	ok;

remove_files( _FilesToRemove=[ SingleFile ], UserState ) ->
	remove_file( SingleFile, UserState );

remove_files( FilesToRemove, UserState ) ->

	trace_debug( " - removing ~B files: ~ts", [ length( FilesToRemove ),
		text_utils:binaries_to_string( FilesToRemove ) ], UserState ),

	file_utils:remove_files( FilesToRemove ).



% @doc Removes specified tree as a whole.
-spec remove_tree( bin_directory_path(), user_state() ) -> void().
remove_tree( BinTreePath, UserState ) ->
	% Recursive removal, beware!
	trace_debug( "Removing tree '~ts'.", [ BinTreePath ], UserState ),
	file_utils:remove_directory( BinTreePath ).



% @doc Reports specified notifications.
-spec report_notifications( [ ustring() ], user_state() ) -> void().
report_notifications( _Notifications=[], UserState ) ->
	trace_debug( "No specific notification to report.", UserState );

report_notifications( _Notifications=[ SingleNotifStr ], UserState ) ->

	trace_debug( SingleNotifStr, UserState ),

	% Added for capitalisation:
	ui:display( "Notification: ~ts", [ SingleNotifStr ] );

report_notifications( Notifications, UserState ) ->

	NotifCount = length( Notifications ),

	NotifString = text_utils:format( "~B notifications to report: ~ts",
		[ NotifCount,
		  text_utils:strings_to_string( Notifications, _Indent=1 ) ] ),

	trace_debug( NotifString, UserState ),

	% Otherwise at least some UI backends might fail:
	% (prefer ellipsing if needed)
	%DisplayString = case NotifCount of
	%
	%	L when L > 500 ->
	%		text_utils:format( "~B notifications to report, "
	%						   "see logs for full details.", [ L ] );
	%
	%	_ ->
	%		NotifString
	%
	%end,

	ui:display( NotifString ).



% @doc Displays a scan notification.
-spec display_scan_notification( bin_file_path() ) -> void().
display_scan_notification( CacheFilePath ) ->
	ui:display_instant( "No cache file ('~ts') found, performing a full scan "
		"to recreate it... (might be long)", [ CacheFilePath ] ).


% @doc Displays information about specified tree data, with a default prompt.
-spec display_tree_data( tree_data(), user_state() ) -> void().
display_tree_data( TreeData=#tree_data{ root=RootDir }, UserState ) ->

	Prompt = text_utils:format( "Information about tree '~ts'", [ RootDir ] ),

	display_tree_data( TreeData, Prompt, UserState ).



% @doc Displays information about specified tree data, with specified prompt.
-spec display_tree_data( tree_data(), ui:prompt(), user_state() ) -> void().
display_tree_data( TreeData=#tree_data{ entries=EntryTable,
										file_count=FileCount },
				   Prompt, UserState ) ->

	Suffix = case table:size( EntryTable ) of

		FileCount ->
			text_utils:format( " ~ts, and as many files, "
				"it is therefore uniquified", [ count_content( FileCount ) ] );

		ContentCount ->
			DupCount = FileCount - ContentCount,
			case DupCount > 0 of

				true ->
					text_utils:format( "~ts and ~B regular files"
						" (hence with ~ts)", [ count_content( ContentCount ),
											   FileCount, case DupCount of

							1 ->
								"a single duplicate";

							_ ->
								text_utils:format( "a total of ~B duplicates",
												   [ DupCount ] )
												   end ] );

				false ->
					trace_bridge:error_fmt(
						"~B regular files, ~ts; abnormal: ~ts",
						[ FileCount, count_content( ContentCount ),
						  tree_data_to_string( TreeData ) ] ),
					throw( { inconsistency_detected, FileCount, ContentCount } )

			end

	end,

	String = text_utils:format( "~ts: this tree has ~ts.", [ Prompt, Suffix ] ),

	ui:display( String ),
	trace_debug( String, UserState ).





% @doc Returns a textual description of specified tree data.
-spec tree_data_to_string( tree_data() ) -> ustring().
tree_data_to_string( TreeData ) ->
	tree_data_to_string( TreeData, _Verbose=false ).



% @doc Returns a textual description of specified tree data, with specified
% verbosity.
%
-spec tree_data_to_string( tree_data(), boolean() ) -> ustring().
tree_data_to_string( #tree_data{ root=BinRootDir,
								 entries=Table,
								 file_count=FileCount,
								 directory_count=_DirCount,
								 symlink_count=_SymlinkCount,
								 device_count=_DeviceCount,
								 other_count=_OtherCount },
					 _Verbose=false ) ->

	% Only looking for files:
	%text_utils:format( "tree '~ts' having ~B entries (~B files, "
	%  "~B directories, ~B symbolic links)",
	%  [ BinRootDir, table:size( Table ), FileCount, DirCount, SymlinkCount ] ).

	case table:size( Table ) of

		0 ->
			"empty tree";

		FileCount ->
			text_utils:format(
				"tree '~ts' having ~B files, each with unique content",
				[ BinRootDir, FileCount ] );

		ContentCount ->
			text_utils:format( "tree '~ts' having ~B files, corresponding only "
				"to ~B different contents (hence with ~B duplicates)",
				[ BinRootDir, FileCount, ContentCount,
				  FileCount - ContentCount ] )

	end;

tree_data_to_string( TreeData, _Verbose=true ) ->

	Entries = table:enumerate( TreeData#tree_data.entries ),

	SHA1Strings = [
		begin
			Bins = [ FD#file_data.path || FD <- FDs ],
			text_utils:format( "for SHA1 ~ts: ~ts", [
				sha1_to_string( SHA1 ),
				text_utils:binaries_to_string( Bins, _Indent=1 ) ] )
		end || { SHA1, FDs } <- Entries ],

	DetailString = text_utils:strings_to_enumerated_string( SHA1Strings ),

	tree_data_to_string( TreeData, false ) ++ DetailString.



% @doc Returns a textual description of specified file data.
-spec file_data_to_string( file_data() ) -> ustring().
file_data_to_string( #file_data{ path=Path,
								 size=Size,
								 timestamp=Timestamp,
								 sha1_sum=Sum } ) ->

	SizeString = system_utils:interpret_byte_size_with_unit( Size ),

	text_utils:format( "file '~ts' whose size is ~ts, SHA1 sum is ~ts and "
		"timestamp is ~p", [ Path, SizeString, Sum, Timestamp ] ).


% @doc Returns a textual description of specified SHA1.
-spec sha1_to_string( sha1() ) -> ustring().
sha1_to_string( SHA1 ) ->
	% Mimics the output of the sha1sum executable:
	text_utils:to_lowercase(
		text_utils:binary_to_hexastring( SHA1, _AddPrefix=false ) ).
