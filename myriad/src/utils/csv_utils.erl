% Copyright (C) 2018-2021 Olivier Boudeville
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
%
% Creation date: Thursday, February 22, 2018.



% Management of CSV (Comma-Separated Values) data.
%
% Note that such a data format is not at the state of the art: it is very basic
% if not rudimentary, not formalised and quite limited (no meta-data, escaping
% being required, etc.).
%
% See also: https://en.wikipedia.org/wiki/Comma-separated_values.
%
% See csv_utils_test.erl for the corresponding test.
%
-module(csv_utils).


% Usual extension of CSV files:
-define( csv_extension, ".csv" ).


% The field separator (delimiter) used between the values in a row (often
% comma, sometimes semicolon or alike):
%
-type separator() :: char().


% The default separator of CSV is, well, a comma:
-define( default_separator, $, ).



% The value V, in "CSV":
-type value() :: any().


% A row of a CSV content, as a tuple of values:
%
% (logically a tuple, often more convenient as a list, yet kept as a tuple as
% more compact in memory)
%
-type row() :: tuple().  % tuple( value() ).


% The line number of a row in a file.
%-type row_number() :: basic_utils:count().


% A CSV content, as an (ordered) list of rows:
-type content() :: [ row() ].


% A CSV content, as an (ordered) list of rows that match a row spec (ex:
% expected number of fields) or not (then represented as a mere list of values):
%
-type mixed_content() :: [ row() | [ value() ] ].


% Number of rows:
-type row_count() :: basic_utils:count().


% Number of fields:
-type field_count() :: basic_utils:count().


-export_type([ separator/0, value/0, row/0, content/0, row_count/0,
			   field_count/0 ]).



% The default read-ahead size for CSV files:
-define( ahead_size, 512*1024 ).


% For some reason, if relying on the '-noinput' option, using the following
% options used to result in {read_error,{no_translation,unicode,unicode},...},
% whereas using io:setopts/1 (ex: possibly through
% system_utils:force_unicode_support/0) afterwards did not fail and allowed
% reads to return correctly-encoded lines:
%
% (additionally, even when forcing UTF8 encoding when exporting as CSV an Excel
% spreadsheet, the same ISO-8859 content will be obtained)
%
%-define( read_options, [ read, { read_ahead, ?ahead_size },
%						 { encoding, utf8 } ] ).

% This work-around is still necessary (still true with Erlang 23.0):
-define( read_options, [ read, { read_ahead, ?ahead_size } ] ).


% Defines what are the characters that denote a start/end of quoting in CVS
% files:
%
% (currently, only double quotes; single ones could be added)
%
-define( quoting_characters, [ $" ] ).


% Defines what are the characters that are used to escape quoting characters in
% CVS files:
%
% (currently, only backslash)
%
-define( escaping_characters, [ $\\ ] ).


% A CSV file is seen here as a series of rows ending with a newline, i.e. '\n'.
%
% Any empty row (either blank or containing only whitespaces) or corresponding
% to a comment (i.e. whose first non-blank character is '#') will be dropped
% (ignored).
%
% Each row contains a series of values (possibly non-defined, i.e. empty)
% delimited by a separator character (usually a comma, i.e. ',').
%
% Each value can be enclosed with single (') or double quote ("), and should not
% contain any newline.
%
% Any \r is ignored.
%
% All lines (rows) are expected to contain the same number of values (fields).


% Shorthands:

-type line() :: text_utils:ustring().


% Implementation notes:
%
% Our version of the CSV format supports:
%
% - non-comma separators
%
% - comments (lines starting with the # character, potentially with leading
% whitespaces are ignored)
%
% - blank lines
%
% - larger CSV files (they are not read as a whole first, they are parsed line
% by line)
%
% We used to represent rows as lists (a convenient datatype), now we prefer (at
% least for fixed-sized ones) representing them, more logically, as tuples.


% Future improvements:
%
% - performs an actual parsing, so that a field of type 'string' may contain the
% separator (ex: if the separator is ',', a field could then be "hello, you!"),
% and be possibly defined over multiple lines
%
% - read from a compressed file
%
% - support character encoding


-export([  % For regular, homogeneous CSV files, supposing the separator known a
		   % priori, but not the number of fields to expect :
		   %
		   read_file/1, read_file/2,

		   % For CSV files that shall be filtered before use (ex: with some rows
		   % obeying different rules):

		   % If the separator and number of fields to expect are known a priori:
		   interpret_file/3,

		   % If the separator is known a priori, but not the number of fields to
		   % expect:
		   %
		   interpret_file/2,

		   % If neither the separator nor the number of fields to expect is
		   % known a priori:
		   %
		   interpret_file/1,

		   write_file/2, write_file/3,

		   get_usual_separators/0,

		   check_all_empty/1, are_all_empty/1,

		   %write_file/2, write_file/3
		   content_to_string/1 ]).



% Reads specified file, expected to be in CSV format and supposed to be
% homogeneous (all non-dropping rows having the same number of fields -
% otherwise an exception is raised), using the default separator.
%
% Returns:
% - the corresponding content, as an ordered list of rows
% - the number of rows found
% - the number of fields they all have
%
-spec read_file( file_utils:any_file_path() ) ->
					   { content(), row_count(), field_count() }.
read_file( Filename ) ->
	read_file( Filename, ?default_separator ).



% Reads specified file, expected to be in CSV format and supposed to be
% homogeneous (all non-dropped rows having the same number of fields - otherwise
% an exception is raised), using the specified separator.
%
% Returns:
% - the corresponding content, as an ordered list of rows
% - the number of rows found
% - the number of fields they all have
%
-spec read_file( file_utils:any_file_path(), separator() ) ->
					   { content(), row_count(), field_count() }.
read_file( FilePath, Separator ) when is_integer( Separator ) ->

	File = get_file_for_reading( FilePath ),

	Res = { _Content, _RowCount, _FieldCount } = read_rows( File, Separator ),

	%trace_utils:debug_fmt( "Read content (~B rows, each having ~p fields): "
	%           "~n ~p", [ RowCount, FieldCount, Content ] ),

	file_utils:close( File ),

	Res.



% Interprets specified file, based on specified separator and number of fields
% per row; returns {MixedContent, MatchCount, UnmatchCount, DropCount}, i.e.:
%
% - a list (respecting the in-file order) whose elements are either a list of
% the specified number of fields, or a pair whose first element is the
% 'non_matching' atom, and whose second element is a list whose elements were
% obtained based on the same separator - yet in a different number than the
% expected one
%
% - the number of rows that match the row spec (i.e. the specified number of
% fields), based on specified separator
%
% - the number of rows that do not match said constraints
%
% - the number of rows that were dropped (typically because they were either
% empty or containing a comment)
%
-spec interpret_file( file_utils:any_file_path(), separator(),
					  field_count() ) ->
		{ mixed_content(), row_count(), row_count(), row_count() }.
interpret_file( FilePath, Separator, ExpectedFieldCount )
  when is_integer( Separator ) ->

	File = get_file_for_reading( FilePath ),

	%{ MixedContent, MatchCount, UnmatchingCount, DropCount } =
	Res = interpret_rows( File, Separator, ExpectedFieldCount ),

	%trace_utils:debug_fmt( "Read mixed content (matching count: ~B, "
	%        "unmatching count: ~B, drop count: ~B):~n ~p",
	%        [ MatchCount, UnmatchingCount, DropCount, MixedContent ] ),

	file_utils:close( File ),

	Res.



% Interprets specified file, based on specified separator, with no prior
% knowledge about the number of fields per row; returns {FieldCount,
% MixedContent, MatchCount, UnmatchCount, DropCount}, i.e.:
%
% - the number of fields in each row, based on the first one
%
% - a list (respecting the in-file order) whose elements are either a list of
% the specified number of fields, or a pair whose first element is the
% 'non_matching' atom, and whose second element is a list whose elements were
% obtained based on the same separator - yet in a different number than the
% expected one
%
% - the number of rows that match the row spec (i.e. the specified number of
% fields), based on specified separator
%
% - the number of rows that do not match said constraints
%
% - the number of rows that were dropped (typically because they were either
% empty or containing a comment)
%
-spec interpret_file( file_utils:any_file_path(), separator() ) ->
		{ field_count(), mixed_content(), row_count(), row_count(), row_count() }.
interpret_file( FilePath, Separator ) when is_integer( Separator ) ->

	File = get_file_for_reading( FilePath ),

	{ FirstRow, FieldCount, GuessDropCount } =
		guess_field_count( File, Separator, _InitialDropCount=0 ),

	% Branch to the helper with a correct initial state:
	{ MixedContent, MatchCount, UnmatchingCount, DropCount } = interpret_rows(
		_Device=File, Separator, FieldCount, _MatchCount=1, _UnmatchCount=0,
		GuessDropCount, _Acc=[ FirstRow ] ),

	%trace_utils:debug_fmt( "Read mixed content with detected field count ~B "
	%	"(matching count: ~B, unmatching count: ~B, drop count: ~B):~n ~p",
	%	[ FieldCount, MatchCount, UnmatchingCount, DropCount, MixedContent ] ),

	file_utils:close( File ),

	{ FieldCount, MixedContent, MatchCount, UnmatchingCount, DropCount }.



% Interprets specified file, with no prior knowledge about the separator or the
% number of fields per row; returns {Separator, FieldCount, MixedContent,
% MatchCount, UnmatchCount, DropCount}, i.e.:
%
% - the most likely separator in use
%
% - the number of fields in each row, based on the first one
%
% - a list (respecting the in-file order) whose elements are either a list of
% the specified number of fields, or a pair whose first element is the
% 'non_matching' atom, and whose second element is a list whose elements were
% obtained based on the same separator - yet in a different number than the
% expected one
%
% - the number of rows that match the row spec (i.e. the specified number of
% fields), based on specified separator
%
% - the number of rows that do not match said constraints
%
% - the number of rows that were dropped (typically because they were either
% empty or containing a comment)
%
-spec interpret_file( file_utils:any_file_path() ) ->
		{ separator(), field_count(), mixed_content(), row_count(),
		  row_count(), row_count() }.
interpret_file( FilePath ) ->

	File = get_file_for_reading( FilePath ),

	{ FirstRow, Separator, FieldCount, FirstDropCount } =
		guess_separator_and_field_count( File, _InitialDropCount=0 ),

	trace_utils:debug_fmt( "First row: '~p', separator: '~ts', "
		"field count: ~B, first drop count: ~B.",
		[ FirstRow, [Separator], FieldCount, FirstDropCount ] ),

	% Branch to the helper with a correct initial state:
	{ MixedContent, MatchCount, UnmatchingCount, DropCount } = interpret_rows(
		_Device=File, Separator, FieldCount, _MatchCount=1, _UnmatchCount=0,
		FirstDropCount, _Acc=[ FirstRow ] ),

	% Full version with content:
	%trace_utils:debug_fmt( "Read mixed content with detected separator '~ts' "
	%   "and field count ~B "
	%	"(matching count: ~B, unmatching count: ~B, drop count: ~B):~n ~p",
	%	[ [Separator], FieldCount, MatchCount, UnmatchingCount, DropCount,
	%	  MixedContent ] ),

	% Summary:
	trace_utils:debug_fmt( "Read mixed content with detected separator '~ts' "
		"and field count ~B "
		"(matching count: ~B, unmatching count: ~B, drop count: ~B).",
		[ [Separator], FieldCount, MatchCount, UnmatchingCount, DropCount ] ),

	file_utils:close( File ),

	{ Separator, FieldCount, MixedContent, MatchCount, UnmatchingCount,
	  DropCount }.



% Helper section.


% Returns the context read from specified device/file.
-spec read_rows( file_utils:file(), separator() ) ->
					   { content(), row_count(), field_count() }.
read_rows( File, Separator ) ->
	read_rows( _Device=File, Separator, _RowCount=0, _FieldCount=undefined,
			   _Acc=[] ).



% (helper)
read_rows( Device, Separator, RowCount, FieldCount, Acc ) ->

	case io:get_line( Device, _Prompt="" ) of

		eof  ->
			file_utils:close( Device ),
			Content = lists:reverse( Acc ),
			{ Content, RowCount, FieldCount };

		{ error, Error } ->
			throw( { read_error, Error } );

		Line ->

			case parse_row( Line, Separator ) of

				{ Values, ThisFieldCount } ->

					%trace_utils:debug_fmt( "For line '~ts', "
					%     "~B field(s) found.", [ Line, ThisFieldCount ] ),

					NewFieldCount = case FieldCount of

						undefined ->
							ThisFieldCount;

						ThisFieldCount ->
							ThisFieldCount;

						_OtherFieldCount ->
							trace_utils:error_fmt(
							  "Non matching line (row #~B): '~ts'.",
							  [ RowCount, Line ] ),

							throw( { non_uniform_field_count,
									 { FieldCount, ThisFieldCount } } )

					end,

					%trace_utils:debug_fmt( "Read ~B fields: ~p.",
					%					   [ ThisFieldCount, Values ] ),

					read_rows( Device, Separator, RowCount+1, NewFieldCount,
							   [ Values | Acc ] );

				dropped ->
					read_rows( Device, Separator, RowCount, FieldCount, Acc )


			end

	end.



% Guesses the number of fields per row, and returns also the first read line so
% that the rest of the file can be read in the same movement.
%
% (helper)
%
-spec guess_field_count( file_utils:file(), separator(), row_count() ) ->
							   { row(), field_count(), row_count() }.
guess_field_count( Device, Separator, DropCount ) ->

	case io:get_line( Device, _Prompt="" ) of

		eof ->
			% Nothing can be determined in this case:
			throw( empty_csv_file );

		{ error, Error } ->
			throw( { read_error, Error } );

		Line ->
			case parse_row( Line, Separator ) of

				dropped ->
					guess_field_count( Device, Separator, DropCount+1 );

				{ Values, FieldCount } ->
					{ Values, FieldCount, DropCount }

			end

	end.



% Guesses the separator and number of fields per row, and returns also the first
% read line so that the rest of the file can be read in the same movement.
%
% (helper)
%
-spec guess_separator_and_field_count( file_utils:file(), row_count() ) ->
					   { row(), separator(), field_count(), row_count() }.
guess_separator_and_field_count( Device, DropCount ) ->

	case io:get_line( Device, _Prompt="" ) of

		eof ->
			% Nothing can be determined in this case:
			throw( empty_csv_file );

		{ error, Error } ->
			throw( { read_error, Error } );

		Line ->
			case parse_row_no_separator( Line ) of

				dropped ->
					guess_separator_and_field_count( Device, DropCount+1 );

				{ Values, Separator, FieldCount } ->
					{ Values, Separator, FieldCount, DropCount }

			end

	end.



% Returns the context read from specified device/file.
-spec interpret_rows( file_utils:file(), separator(), field_count() ) ->
				{ mixed_content(), row_count(), row_count(), row_count() }.
interpret_rows( File, Separator, ExpectedFieldCount ) ->
	interpret_rows( _Device=File, Separator, ExpectedFieldCount, _MatchCount=0,
					_UnmatchCount=0, _DropCount=0, _Acc=[] ).



% (helper)
interpret_rows( Device, Separator, ExpectedFieldCount, MatchCount, UnmatchCount,
				DropCount, Acc ) ->

	case io:get_line( Device, _Prompt="" ) of

		eof ->
			file_utils:close( Device ),
			MixedContent = lists:reverse( Acc ),
			{ MixedContent, MatchCount, UnmatchCount, DropCount };

		{ error, Error } ->
			throw( { read_error, Error } );

		Line ->

			%io:format( "Read line '~ts'.", [ Line ] ),

			case parse_row( Line, Separator ) of

				% Matching:
				{ Values, ExpectedFieldCount } ->

					%trace_utils:debug_fmt( "Read matching row: ~p.",
					%					   [ Values ] ),

					interpret_rows( Device, Separator, ExpectedFieldCount,
					  MatchCount+1, UnmatchCount, DropCount, [ Values | Acc ] );


				% Not matching:
				{ Values, _OtherFieldCount } ->

					%trace_utils:debug_fmt( "Read non-matching row:~n~ts",
					%					   [ Line ] ),

					interpret_rows( Device, Separator, ExpectedFieldCount,
									MatchCount, UnmatchCount+1, DropCount,
									[ { non_matching, Values } | Acc ] );


				dropped ->
					interpret_rows( Device, Separator, ExpectedFieldCount,
									MatchCount, UnmatchCount, DropCount+1, Acc )

			end

	end.



% Parses the specified line into a proper row, guessing the most likely
% separator (that is that not known).
%
-spec parse_row_no_separator( line() ) ->
							'dropped' | { row(), separator(), field_count() }.
parse_row_no_separator( Line ) ->

	% Useful also to remove the ending newline:
	TrimmedLine = text_utils:trim_whitespaces( Line ),

	case TrimmedLine of

		[] ->
			%trace_utils:debug( "Dropped blank line" ),
			dropped;


		[ $# | _ ] ->
			%trace_utils:debug_fmt( "Dropped following comment: '~ts'.",
			%					   [ Line ] ),
			dropped;

		_ ->
			GuessedSep = guess_separator_from( TrimmedLine ),

			%trace_utils:debug_fmt( "Guessed separator: '~ts'.",
			%					   [ [GuessedSep] ] ),

			Values = parse_line( TrimmedLine, GuessedSep ),

			FieldCount = length( Values ),
			{ list_to_tuple( Values ), GuessedSep, FieldCount }

	end.



% Parses the specified line into a proper row, based on the specified separator.
-spec parse_row( line(), separator() ) -> maybe( { row(), field_count() } ).
parse_row( Line, Separator ) ->

	% Useful also to remove the ending newline:
	TrimmedLine = text_utils:trim_whitespaces( Line ),

	case TrimmedLine of

		[] ->
			%trace_utils:debug( "Dropped blank line" ),
			dropped;


		[ $# | _ ] ->
			%trace_utils:debug_fmt( "Dropped following comment: '~ts'.",
			%					   [ Line ] ),
			dropped;

		_ ->
			Values = parse_line( TrimmedLine, Separator ),
			FieldCount = length( Values ),
			{ list_to_tuple( Values ), FieldCount }

	end.



% Parsing allows to see quoted sequences as a single, opaque element in which
% any presence of the separator is ignored.
%
-spec parse_line( line(), separator() ) -> [ value() ].
parse_line( Line, Separator ) ->

	% Allows not to consider as unmatched the lines that happen to have, in a
	% quote, the separator in use (which, in this case, shall be considered as
	% any other character):

	% First consider each quoted sequence as a single element:
	ParseLine = text_utils:parse_quoted( Line, ?quoting_characters,
										 ?escaping_characters ),

	% Now split with the separator, respecting quoted elements:
	text_utils:split_parsed( ParseLine, [ Separator ] ).



% Checks that specified row or list of values (typically coming from a row of
% unspecified field count) contains only empty values (empty strings).
%
-spec check_all_empty( row() | [ value() ] ) -> void().
check_all_empty( Row ) when is_tuple( Row ) ->
	check_all_empty( tuple_to_list( Row ) );

check_all_empty( _List=[] ) ->
	ok;

check_all_empty( _List=[ "" | T ] ) ->
	check_all_empty( T );

check_all_empty( _List=[ H | _T ] ) ->
	throw( { non_empty_value, H } ).



% Returns whether the specified list of values contains only empty ones.
-spec are_all_empty( row() | [ value() ] ) -> boolean().
are_all_empty( Row ) when is_tuple( Row ) ->
	are_all_empty( tuple_to_list( Row ) );

are_all_empty( [] ) ->
	true;

are_all_empty( [ "" | T ] ) ->
	are_all_empty( T );

are_all_empty( [ _H | _T ] ) ->
	false.



% Returns the most usual separators used in CSV files.
-spec get_usual_separators() -> [ separator() ].
get_usual_separators() ->
	[ $,, $; ].


% Determines the separator used in specified line.
-spec guess_separator_from( line() ) -> separator().
guess_separator_from( Line ) ->
	%trace_utils:debug_fmt( "Guessing separator used in '~ts'...", [ Line ] ),
	SepPairs = gather_potential_separators( Line ),
	select_most_likely_separator( SepPairs ).


gather_potential_separators( Line ) ->
	evaluate_separators_on( _Seps=get_usual_separators(), Line, _SepAcc=[] ).


% (helper)
evaluate_separators_on( _Seps=[], _Line, SepAcc ) ->
	SepAcc;

evaluate_separators_on( _Seps=[ Sep | T ], Line, SepAcc ) ->
	SubStrings = text_utils:split( Line, _Delimiters=[ Sep ] ),
	NewSepAcc = [ { Sep, length( SubStrings ) - 1 } | SepAcc ],
	evaluate_separators_on( T, Line, NewSepAcc ).


% (helper)
select_most_likely_separator( SepPairs ) ->

	%trace_utils:debug_fmt( "Separator pairs: ~w.", [ SepPairs ] ),

	% Returns the separator having the higher number of occurrences:
	{ Sep, _Count } = list_utils:get_last_element(
						lists:keysort( _CountIndex=2, SepPairs ) ),

	Sep.



% Writes specified content (i.e. a list of homogeneous row tuples) in specified
% CSV file, using the default separator for that.
%
-spec write_file( content(), file_utils:any_file_path() ) -> void().
write_file( Content, TargetFilePath ) ->
	write_file( Content, TargetFilePath, ?default_separator ).



% Writes specified content (i.e. a list of homogeneous row tuples) in specified
% CSV file, using specified separator for that.
%
-spec write_file( content(), file_utils:any_file_path(), separator() ) ->
						void().
write_file( Content, TargetFilePath, Separator ) ->

	case file_utils:exists( TargetFilePath ) of

		true ->
			throw( { already_existing, TargetFilePath } );

		false ->
			ok

	end,

	WriteOpts = [ write, raw, delayed_write ],

	File = file_utils:open( TargetFilePath, WriteOpts ),

	write_rows( Content, Separator, File ),

	file_utils:close( File ).



% (helper)
write_rows( _Content=[], _Separator, _File ) ->
	ok;

write_rows( _Content=[ Row | T ], Separator, File ) ->

	Elems = tuple_to_list( Row ),

	Line = text_utils:join( Separator, Elems ),

	file_utils:write_ustring( File, "~ts~n", [ Line ] ),

	write_rows( T, Separator, File ).



% Returns a file handle to read specified file.
-spec get_file_for_reading( file_utils:any_file_path() ) -> file_utils:file().
get_file_for_reading( FilePath ) ->

	%trace_utils:debug_fmt( "Opening '~ts' with options ~w.",
	%					   [ FilePath, ?read_options ] ),

	case file_utils:is_existing_file_or_link( FilePath ) of

		true ->
			ok;

		false ->
			throw( { csv_file_not_found, FilePath,
					 file_utils:get_current_directory() } )

	end,

	File = file_utils:open( FilePath, ?read_options ),

	% Refer to the note in file_utils:open/2 for explanation:
	% (still needed)
	%
	system_utils:force_unicode_support(),

	File.



% Returns a textual representation of specified content.
-spec content_to_string( content() ) -> text_utils:ustring().
content_to_string( Content ) ->
	text_utils:format( "content of ~B rows: ~ts", [ length( Content ),
					   text_utils:terms_to_enumerated_string( Content ) ] ).
