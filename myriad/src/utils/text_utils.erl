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
% Creation date: July 1, 2007.


% Gathering of various convenient facilities.
%
% See text_utils_test.erl for the corresponding test.
%
-module(text_utils).


% Note: this a boostrap module, so its build is only to be triggered from the
% root of Myriad, and it should not depend at runtime on non-bootstrapped
% modules.



% Note that string:tokens/1 can be used to split strings.


% String management functions.


% Conversions between terms and strings (both ways).
-export([ term_to_string/1, term_to_string/2, term_to_string/3,
		  term_to_bounded_string/1, term_to_bounded_string/2,
		  term_to_binary/1,

		  integer_to_string/1,
		  integer_to_hexastring/1, integer_to_hexastring/2,
		  hexastring_to_integer/1, hexastring_to_integer/2,

		  atom_to_string/1,

		  pid_to_string/1, pids_to_string/1,
		  pid_to_short_string/1, pids_to_short_string/1, pid_to_core_string/1,

		  record_to_string/1,

		  strings_to_string/1, strings_to_sorted_string/1,
		  strings_to_string/2, strings_to_sorted_string/2,
		  strings_to_enumerated_string/1, strings_to_enumerated_string/2,
		  strings_to_listed_string/1, strings_to_listed_string/2,

		  binaries_to_string/1, binaries_to_string/2,
		  binaries_to_sorted_string/1, binaries_to_listed_string/1,
		  binaries_to_binary/1, binaries_to_binary/2,

		  atoms_to_string/1, atoms_to_sorted_string/1, atoms_to_listed_string/1,
		  integers_to_listed_string/1,
		  proplist_to_string/1, version_to_string/1,
		  atom_to_binary/1,

		  string_to_binary/1, string_to_binary/2,
		  binary_to_string/1, binary_to_string/2,
		  strings_to_binaries/1, binaries_to_strings/1,
		  string_to_integer/1, try_string_to_integer/1, try_string_to_integer/2,
		  string_to_float/1, try_string_to_float/1,
		  string_to_atom/1, strings_to_atoms/1,
		  terms_to_string/1, terms_to_enumerated_string/1,
		  terms_to_listed_string/1,
		  binary_to_atom/1,
		  float_to_string/1, float_to_string/2, number_to_string/1,
		  percent_to_string/1, percent_to_string/2,
		  distance_to_string/1, distance_to_short_string/1,

		  format/2, bin_format/2, atom_format/2, format/3,
		  format_ellipsed/2, format_ellipsed/3,
		  format_as_comment/1, format_as_comment/2, format_as_comment/3,
		  format_as_comment/4,

		  ensure_string/1, ensure_string/2,
		  ensure_strings/1, ensure_strings/2,

		  ensure_binary/1, ensure_binary/2,
		  ensure_binaries/1, ensure_binaries/2 ]).



% Other string operations:
-export([ get_lexicographic_distance/2, get_longest_common_prefix/1,
		  safe_length/1,
		  uppercase_initial_letter/1, to_lowercase/1, to_uppercase/1,
		  join/2,
		  split/2, split_per_element/2, split_parsed/2, split_at_whitespaces/1,
		  split_at_first/2, split_camel_case/1, tokenizable_to_camel_case/2,

		  find_substring_index/2, find_substring_index/3,

		  substitute/3, filter/2, split_after_prefix/2,
		  update_with_keywords/2,

		  list_whitespaces/0,

		  escape_single_quotes/1, escape_double_quotes/1,
		  escape_all_quotes/1, escape_with/3,
		  remove_newlines/1,

		  parse_quoted/1, parse_quoted/3,

		  is_uppercase/1, is_figure/1,
		  remove_ending_carriage_return/1, remove_last_characters/2,
		  remove_whitespaces/1,

		  trim_whitespaces/1, trim_leading_whitespaces/1,
		  trim_trailing_whitespaces/1,

		  ellipse/1, ellipse/2, ellipse_fmt/2,

		  get_default_bullet/0, get_bullet_for_level/1,
		  format_text_for_width/2,
		  pad_string/2, pad_string_left/2, pad_string_right/2,
		  is_string/1, is_non_empty_string/1, are_strings/1,
		  is_bin_string/1, are_binaries/1,
		  are_of_same_string_type/2,
		  try_convert_to_unicode_list/1, to_unicode_list/1, to_unicode_list/2,
		  try_convert_to_unicode_binary/1, to_unicode_binary/1,
		  to_unicode_binary/2 ]).


% Restructured-Text (RST) related functions.
-export([ generate_title/2 ]).


% To report properly (i.e. with a location) at runtime type errors:
-export([ report_not_a_string/1, report_not_a_binary_string/1,
		  report_not_a_list/1, report_not_a_number/1 ]).

% Miscellaneous functions.
-export([ generate_text_name_from/1, match_types/3 ]).


% This module being a bootstrap one, the 'table' pseudo-module is not available
% (as this module is by design not processed by the 'Myriad' parse transform):
%
-define( table, map_hashtable ).


% Prefix for text corresponding to hexadecimal values:
-define( hexa_prefix, "0x" ).


% Type section.

% These strings are supposed to contain Erlang-fashioned format characters, like
% in "hello ~p!":
%
-type format_string() :: ustring().


% In a format string (ex: "~n"):
-type control_sequence() :: ustring().


% Lists of terms corresponding to a format string:
-type format_values() :: [ term() ].


% These strings are supposed to contain Regular Expressions, like in:
% "*-emitter-(first|second)-*".
%
% Patterns are to be expressed according to the “Perl Compatible Regular
% Expressions” conventions, or PCRE for short.
% For more information, see following cheat sheet:
% http://www.bitcetera.com/page_attachments/0000/0030/regex_in_a_nutshell.pdf
%
% See also: http://erlang.org/doc/man/re.html
%
-type regex_string() :: ustring().


% A string that describes a title:
-type title() :: ustring().


% A string that describes a label:
-type label() :: ustring().


% A binary corresponding to a string:
-type bin_string() :: binary().


% Any kind of string (a.k.a chardata() :: charlist() | unicode_string()):
-type any_string() :: ustring() | bin_string().



% A Unicode string.
%
% This is our new default.
%
% We mean [char()] where char() must be 0..16#10ffff:
-type unicode_string() :: unicode:chardata().


-type unicode_data() :: unicode:latin1_chardata()
					  | unicode:chardata() | unicode:external_chardata().


% A Unicode codepoint for a character.
%
% (unfortunately we cannot define a text_utils:char/0 type, as "type char()
% is a builtin type; it cannot be redefined").
%
-type uchar() :: integer().


% Index in a Unicode string, in terms of grapheme clusters (ex: not codepoints,
% not bytes).
%
-type gc_index() :: non_neg_integer().


-type direction() :: 'leading' | 'trailing'.


% A plain (Unicode) string:
-type plain_string() :: [ uchar() ].


% Now our default:
%
% (unfortunately we cannot define a text_utils:string/0 type, as "type ustring()
% is a builtin type; it cannot be redefined").
%
-type ustring() :: unicode_string().


% Any kind of terms that can be directly mapped to a string (typically accepted
% by ~s in format strings):
%
-type string_like() :: ustring() | unicode_string() | bin_string() | atom().


% The specific type of iolist resulting from a parsing:
-type parse_string() :: [ uchar() | plain_string() ].


% To convert keywords:
-type translation_table() :: ?table:?table( any_string(), any_string() ).


% The length of a string, typically in terms of number of characters / graphene
% clusters:
%
-type length() :: pos_integer().


% A width, typically in terms of number of characters:
-type width() :: pos_integer().


% The level of indentation (starts at zero, and the higher, the most nested).
-type indentation_level() :: basic_utils:level().

% A (nesting) depth, typically to keep track of indentation levels:
-type depth() :: pos_integer().


% A bullet, to denote the elements of a list.
-type bullet() :: ustring().


% Either an indentation level, or directly a bullet:
-type indentation_level_or_bullet() :: indentation_level() | bullet().


% Lexicographic (Levenshtein) distance, i.e. minimum number of single-character
% edits (i.e. insertions, deletions or substitutions) required to change one
% string into the other:
%
-type distance() :: non_neg_integer().


% See https://erlang.org/doc/man/erlang.html#float_to_list-2 for more
% information.
%
-type float_option() :: { 'decimals', 0..253 }
					  | { 'scientific', 0..249 }
					  | 'compact'.


-export_type([ format_string/0, format_values/0,
			   regex_string/0, title/0, label/0,
			   bin_string/0, any_string/0, unicode_string/0, unicode_data/0,
			   uchar/0, plain_string/0, ustring/0, string_like/0,
			   parse_string/0,
			   translation_table/0, length/0, width/0, indentation_level/0,
			   distance/0 ]).




% Maybe at least format/2 would be better inlined, however it is no cross-module
% inlining (just inside this module), so a parse-transform may be used in order
% to transform text_utils:format/2 into io_lib:format/2 or into its actual,
% safer code.
%
-compile( { inline, [ format/2 ] } ).



% String management functions.


% Returns a human-readable string describing specified term.
-spec term_to_string( term() ) -> ustring().
term_to_string( _Term=[] ) ->
	% Otherwise would be an empty string:
	"[]";

term_to_string( Term ) ->

	case io_lib:printable_list( Term ) of

		true ->
			io_lib:format( "~ts", [ Term ] );

		_    ->
			io_lib:format( "~p", [ Term ] )

	end.



% Returns a human-readable string describing specified term, within a bounded,
% default length.
%
-spec term_to_bounded_string( term() ) -> ustring().
% Does not happen, as empty set is actually {0,nil}:
%term_to_bounded_string( _AttrValue=[] ) ->
%	% To avoid being it interpreted as a set:
%	"(empty list or set)";
term_to_bounded_string( Term ) ->
	term_to_bounded_string( Term, _MaxLen=2000 ).



% Returns a human-readable string describing specified term, within the
% specified length.
%
% See also term_to_string/3.
%
-spec term_to_bounded_string( term(), length() | 'unlimited' ) -> ustring().
term_to_bounded_string( Term, _MaxLen=unlimited ) ->
	Term;

term_to_bounded_string( Term, MaxLen ) ->

	FullString = case set_utils:is_set( Term ) of

		true ->
			format( "[as set] ~p", [ set_utils:to_list( Term ) ] );

		false ->
			format( "~p", [ Term ] )

	end,

	% To avoid that gigantic terms saturate the outputs:
	ellipse( FullString, MaxLen ).



% Returns a human-readable binary string describing specified term.
-spec term_to_binary( term() ) -> bin_string().
term_to_binary( Term ) ->
	String = term_to_string( Term ),
	string_to_binary( String ).



% Returns a human-readable string describing specified term, up to the specified
% nesting depth.
%
-spec term_to_string( term(), depth() ) -> ustring().
term_to_string( _Term=[], _MaxDepthCount ) ->
	% Otherwise would be an empty string:
	"[]";

term_to_string( Term, MaxDepthCount ) ->

	case io_lib:printable_list( Term ) of

		true ->
			io_lib:format( "~ts", [ Term ] );

		_    ->
			io_lib:format( "~P", [ Term, MaxDepthCount ] )

	end.



% Returns a human-readable string describing specified term, up to the specified
% nesting depth, and up to specified string length (at least 3, so that the
% "..." marker can be inserted).
%
% See also term_to_bounded_string/{1,2}.
%
-spec term_to_string( term(), depth(), basic_utils:count() )-> ustring().
term_to_string( _Term=[], _MaxDepthCount, _MaxLength ) ->
	% Otherwise would be an empty string:
	"[]";

term_to_string( Term, MaxDepthCount, MaxLength ) when MaxLength >= 3 ->

	% First limit the depth (beware of IO-lists!):
	FullString = case io_lib:printable_list( Term ) of

		true ->
			% The '*' character in the format string is not suitable here:
			lists:flatten( io_lib:format( "~ts", [ Term ] ) );

		_ ->
			lists:flatten( io_lib:format( "~P", [ Term, MaxDepthCount ] ) )

	end,

	% Then limit the length:
	case length( FullString ) of

		L when L > MaxLength ->
			% We have to truncate here, length( "..." ) = 3
			% MaxLength - 3 = 0 is allowed there:
			string:sub_string( FullString, 1, MaxLength - 3 ) ++ " ..";

		_ ->
			FullString

	end.



% Avoids to have to use lists:flatten when converting an integer to a string.
% Useless when using functions like io:format, that accept iolists as
% parameters.
%
-spec integer_to_string( integer() ) -> ustring().
integer_to_string( IntegerValue ) ->
	% Nonsensical: hd( io_lib:format( "~B", [ IntegerValue ] ) ).
	%io_lib:format( "~B", [ IntegerValue ] ).
	erlang:integer_to_list( IntegerValue ).


% Returns a plain string corresponding to the specified integer, in hexadecimal
% form, with a "0x" prefix.
%
% Ex: "0x75BCD15".
%
-spec integer_to_hexastring( integer() ) -> ustring().
integer_to_hexastring( IntegerValue ) ->
	integer_to_hexastring( IntegerValue, _AddPrefix=true ).


% Returns a plain string corresponding to the specified integer, in hexadecimal
% form, with a "0x" prefix if requested.
%
% Ex: "0x75BCD15".
%
-spec integer_to_hexastring( integer(), boolean() ) -> ustring().
integer_to_hexastring( IntegerValue, _AddPrefix=true ) ->
	?hexa_prefix ++ integer_to_hexastring( IntegerValue, _Prefix=false );

integer_to_hexastring( IntegerValue, _AddPrefix=false ) ->
	erlang:integer_to_list( IntegerValue, _Base=16 ).



% Returns an integer corresponding to the specified string containing an
% hexadecimal number as a text, and expected to start with a "0x" prefix.
%
% Note: both uppercase and lowercase letters are supported.
%
-spec hexastring_to_integer( ustring() ) -> integer().
hexastring_to_integer( HexaString ) ->
	hexastring_to_integer( HexaString, _ExpectPrefix=true ).


% Returns an integer corresponding to the specified string containing an
% hexadecimal number as a text, expected to start with a "0x" prefix if
% specified.
%
% Note: both uppercase and lowercase letters are supported.
%
-spec hexastring_to_integer( ustring(), boolean() ) -> integer().
hexastring_to_integer( ?hexa_prefix ++ HexaString, _ExpectPrefix=true ) ->
	hexastring_to_integer( HexaString, _HasPrefix=false );

hexastring_to_integer( HexaString, _ExpectPrefix=false ) ->
	list_to_integer( HexaString, _Base=16).



% Returns a plain string corresponding to the specified atom.
-spec atom_to_string( atom() ) -> ustring().
atom_to_string( Atom ) ->
	atom_to_list( Atom ).



% Returns a plain string corresponding to the specified PID.
-spec pid_to_string( pid() ) -> ustring().
pid_to_string( Pid ) ->
	io_lib:format( "~w", [ Pid ] ).


% Returns a plain string corresponding to the specified list of PIDs.
-spec pids_to_string( [ pid() ] ) -> ustring().
pids_to_string( PidList ) ->
	io_lib:format( "~w", [ PidList ] ).


% Returns a short, plain string corresponding to the specified PID.
%
% For example, <0.33.0> returned as "|33|" (half size).
%
% Note though that the pipe character may be better avoided on some systems (ex:
% trace ones).
%
-spec pid_to_short_string( pid() ) -> ustring().
pid_to_short_string( Pid ) ->
	% Could be used: list_utils:flatten_once/1:
	%[ $< | pid_to_core_string( Pid ) ] ++ ">".
	[ $| | pid_to_core_string( Pid ) ] ++ "|".


% Returns a short, plain string corresponding to the specified PIDs.
%
% For example, [<0.33.0>,<0.35.0>] returned as "|33,35|" (7 characters instead
% of 19, almost one-third).
%
-spec pids_to_short_string( [ pid() ] ) -> ustring().
pids_to_short_string( PidList ) ->
	% Could be used: list_utils:flatten_once/1:

	% Preferring an extra character, as better allowing to break longer lines:
	%Sep = ",",
	Sep = ", ",

	[ $| | join( Sep, [ pid_to_core_string( P ) || P <- PidList ] ) ] ++ "|".




% Returns a very short plain string corresponding to the specified PID.
%
% For example, for <0.33.0>, will return "33".
%
-spec pid_to_core_string( pid() ) -> ustring().
pid_to_core_string( Pid ) ->

	% A PID is akin to <X.Y.Z>.

	% Needed otherwise returans ["<0.78.0>"], not "<0.78.0>":
	PidAsText = lists:flatten( io_lib:format( "~w", [ Pid ] ) ),

	%trace_utils:debug_fmt( "PidAsText = '~p'.", [ PidAsText ] ),

	[ $< | Rest ] = PidAsText,

	% PidCore is thus "X.Y.Z":
	PidCore = list_utils:remove_last_element( Rest ),

	%trace_utils:debug_fmt( "PidCore = '~w'.", [ PidCore ] ),

	% Ex: ["0","33","0"]:
	[ First, Second, Third ] = split( PidCore, [ _Sep=$. ] ),

	% Automatic truncating if defaults:
	ActualFirst = case First of

		"0" ->
			[];

		_ ->
			First ++ "."

	 end,

	 ActualThird = case Third of

		"0" ->
			[];

		_ ->
			"." ++ Third

	end,

	% Ex: "33", "1.33", or "1.33.2":
	ActualFirst ++ Second ++ ActualThird.



% Returns a string describing the specified record.
%
% Hugely inspired from a Ulf Wiger's snippet. described in
% http://erlang.org/pipermail/erlang-questions/2006-September/023181.html
%
% Apparently, as records are compile-time structures only, there is no simple
% way of determining the name of their fields at runtime.
%
-spec record_to_string( _ ) -> none().
record_to_string( _Record ) -> % No 'when is_record( Record, Tag ) ->' here.

	throw( { not_implemented, record_to_string } ).

	%RF = fun(R,L) when R == element(1,Record) ->
	%	% Needs apparently a parse transform:
	%   Fields = '#info-'(Record),
	%	true = (L == length(Fields)),
	%	Fields
	%end,
	%
	%io_lib_pretty:print( Record, RF ).



% Returns the default bullet to be used for top-level lists.
-spec get_default_bullet() -> ustring().
get_default_bullet() ->
	get_bullet_for_level( 0 ).



% Returns the bullet to be used for specified indentation level.
-spec get_bullet_for_level( indentation_level() ) -> bullet().
get_bullet_for_level( 0 ) ->
	" + ";

get_bullet_for_level( 1 ) ->
	"   - ";

get_bullet_for_level( 2 ) ->
	"     * ";

get_bullet_for_level( N ) when is_integer( N ) andalso N > 0 ->
	Base = get_bullet_for_level( N rem 3 ),
	string:copies( "   ", ( N div 3 ) + 1 ) ++ Base.



% Returns the indentation offset to be used for specified indentation level of
% enumerated lists.
%
-spec get_indentation_offset_for_level( indentation_level() ) ->  ustring().
get_indentation_offset_for_level( N ) ->
	string:copies( _BaseString=" ", _Count=N+1 ).



% (helper)
%
% Note: the caller should have already vetted the specified arguments.
%
strings_to_string_helper( _Strings=[], Acc, _Bullet ) ->
	Acc;

% We do not want an extra newline at the end:
strings_to_string_helper( _Strings=[ LastString ], Acc, Bullet )
  when is_list( LastString ) orelse is_binary( LastString ) ->
	%Pattern = "~ts~n",
	% Added back, as makes sense?
	% Nope:
	Pattern = "~ts",
	Acc ++ Bullet ++ io_lib:format( Pattern, [ LastString ] );

% We allow also for bin_string():
strings_to_string_helper( _Strings=[ H | T ], Acc, Bullet )
  when is_list( H ) orelse is_binary( H ) ->
	% Byproduct of the trailing newline: an empty line at the end if nested.
	strings_to_string_helper( T,
		Acc ++ Bullet ++ io_lib:format( "~ts~n", [ H ] ), Bullet );

strings_to_string_helper( _Strings=[ H | _T ], _Acc, _Bullet ) ->
	report_not_a_string( H ).




% Returns a string that pretty-prints specified list of strings, with
% enumerated (i.e. 1, 2, 3) bullets.
%
-spec strings_to_enumerated_string( [ ustring() ] ) -> ustring().
strings_to_enumerated_string( Strings ) ->
	strings_to_enumerated_string( Strings, _DefaultIndentationLevel=0 ).


-spec strings_to_enumerated_string( [ ustring() ], indentation_level() ) ->
											ustring().
strings_to_enumerated_string( Strings, IndentationLevel ) ->

	Prefix = get_indentation_offset_for_level( IndentationLevel ),

	{ _FinalCount, ReversedStrings } = lists:foldl(
				fun( String, _Acc={ Count, Strs } ) ->

					NewStrs = [ format( "~ts~B. ~ts~n",
										[ Prefix, Count, String ] ) | Strs ],
					{ Count+1, NewStrs }

				end,
				_Acc0={ 1, "" },
				_List=Strings ),

	OrderedStrings = lists:reverse( ReversedStrings ),

	format( "~n~ts", [ lists:flatten( OrderedStrings ) ] ).



% Returns a plain string that pretty-prints specified list of strings (actually
% the list may contain also binary strings), with default bullets.
%
-spec strings_to_string( [ any_string() ] ) -> ustring().
strings_to_string( _Strings=[] ) ->
	"(empty list)";

strings_to_string( Strings=[ SingleString ] )
  when is_list( SingleString ) orelse is_binary( SingleString ) ->

	% Not retained, as the single string may itself correspond to a full, nested
	% list and no dangling final quote is desirable:
	%io_lib:format( " '~ts'", Strings );

	% No leading space, the caller is expected to have it specified by himself,
	% like in: "foo: ~ts", not as "foo:~ts":

	% To force a plain string:
	%io_lib:format( " ~ts", Strings );
	io_lib:format( "~ts", Strings );

strings_to_string( Strings ) when is_list( Strings ) ->

	%trace_utils:debug_fmt( "Stringifying ~p.", [ Strings ] ),

	% Leading '~n' had been removed for some unknown reason:
	io_lib:format( "~n~ts~n",
	  [ strings_to_string_helper( Strings, _Acc=[], get_default_bullet() ) ] );

strings_to_string( ErrorTerm ) ->
	report_not_a_list( ErrorTerm ).



% Returns a string that pretty-prints specified list of strings (actually, any
% element that can be processed with ~ts will do; ex: atoms) once reordered (and
% with default bullets).
%
-spec strings_to_sorted_string( [ ustring() ] ) -> ustring().
strings_to_sorted_string( Strings ) when is_list( Strings ) ->
	strings_to_string( lists:sort( Strings ) );

strings_to_sorted_string( ErrorTerm ) ->
	report_not_a_list( ErrorTerm ).



% Returns a string that pretty-prints specified list of strings (actually, any
% element that can be processed with ~ts will do; ex: atoms), with
% user-specified bullets or indentation level.
%
% This can be a solution to nest bullet lists, by specifying a bullet with an
% offset, such as " * ".
%
-spec strings_to_string( [ ustring() ], indentation_level_or_bullet() ) ->
								ustring().
strings_to_string( _Strings=[], _IndentationOrBullet ) ->
	"(empty list)";

strings_to_string( _Strings=[ SingleString ], _IndentationOrBullet )
									when is_list( SingleString ) ->
	% For a single string, no need for leading and trailing newlines, but it
	% used to be separated (with single quotes) from the surrounding text
	% (not done anymore, as this single element may be itself a bullet list)
	%
	SingleString;

strings_to_string( Strings, IndentationLevel )
									when is_integer( IndentationLevel ) ->
	Bullet = get_bullet_for_level( IndentationLevel ),
	strings_to_string( Strings, Bullet );

strings_to_string( Strings, Bullet )
			when is_list( Strings ) andalso is_list( Bullet ) ->
	% Leading '~n' had been removed for some unknown reason:

	% Trailing '~n' was removed (as was inducing a too large final blank space),
	% yet proved necessary (otherwise text may continue just at the right of the
	% last bullet; only drawback: indeed, many intermediary and final blank
	% lines inserted when nesting lists):
	%
	% Finally we were not able to reproduce the continuing text on a simple
	% test, so:
	%Pattern = "~n~ts~n",
	Pattern = "~n~ts",

	io_lib:format( Pattern,
		[ strings_to_string_helper( Strings, _Acc=[], Bullet ) ] );

strings_to_string( Strings, Bullet ) when is_list( Bullet ) ->
	report_not_a_list( Strings );

strings_to_string( _Strings, IncorrectBullet ) ->
	throw( { bullet_not_a_string, IncorrectBullet } ).



% Returns a string that pretty-prints specified list of strings (actually, any
% element that can be processed with ~ts will do; ex: atoms) once reordered,
% with user-specified indentation level or bullet.
%
-spec strings_to_sorted_string( [ ustring() ],
								indentation_level_or_bullet() ) -> ustring().
strings_to_sorted_string( Strings, IndentationOrBullet )
  when is_list( Strings ) ->
	strings_to_string( lists:sort( Strings ), IndentationOrBullet );

strings_to_sorted_string( ErrorTerm, _IndentationOrBullet ) ->
	report_not_a_list( ErrorTerm ).



% Returns a plain string that pretty-prints specified list of binary strings,
% with default bullets.
%
-spec binaries_to_string( [ bin_string() ] ) -> ustring().
binaries_to_string( Binaries ) ->
	binaries_to_string( Binaries, _IndentationLevel=0 ).



% Returns a binary string that pretty-prints specified list of binary strings,
% with specified indentation level or bullet.
%
-spec binaries_to_string( [ bin_string() ], indentation_level_or_bullet() ) ->
								ustring().
% See strings_to_string/2 for a counterpart implementation.
%
% A conversion to strings followed by the use of strings_to_string/2 is not the
% way to go as some binary strings (ex: "raw filenames") cannot be converted to
% plain strings, due to a mismatching encoding. strings_to_string/2 cannot be
% used directly either, because of its guards (which should be kept, as it is
% not supposed to support binaries). So we have to mimic it here.
%
binaries_to_string( _Binaries=[ SingleBinString ],
					_IndentationOrBullet ) when is_binary( SingleBinString ) ->
	%binary_to_string( SingleBinString );
	io_lib:format( "~ts", [ SingleBinString ] );

binaries_to_string( Binaries, IndentationLevel )
								when is_integer( IndentationLevel ) ->
	Bullet = get_bullet_for_level( IndentationLevel ),
	binaries_to_string( Binaries, Bullet );

binaries_to_string( Binaries, Bullet )
			when is_list( Binaries ) andalso is_list( Bullet ) ->
	Pattern = "~n~ts~n",
	% Actually no need for a dedicated binaries_to_string_helper/3:
	io_lib:format( Pattern,
		[ strings_to_string_helper( Binaries, _Acc=[], Bullet ) ] );

binaries_to_string( Binaries, Bullet ) when is_list( Bullet ) ->
	report_not_a_list( Binaries );

binaries_to_string( _Binaries, IncorrectBullet ) ->
	throw( { bullet_not_a_string, IncorrectBullet } ).



% Returns a string that pretty-prints specified list of sorted binary strings,
% with default bullets.
%
-spec binaries_to_sorted_string( [ bin_string() ] ) -> ustring().
binaries_to_sorted_string( Binaries ) ->
	Strings = binaries_to_strings( Binaries ),
	strings_to_string( lists:sort( Strings ) ).



% Returns a string that pretty-prints the specified list of binary strings,
% listed directly along the text (not one item per line).
%
% Ex: binaries_to_listed_string([<<"red">>, <<"blue">>, <<"green">>])
% returns "red, blue and green".
%
-spec binaries_to_listed_string( [ bin_string() ] ) -> ustring().
binaries_to_listed_string( Binaries ) ->
	strings_to_listed_string( [ binary_to_string( B ) || B <- Binaries ] ).



% Returns a binary string that pretty-prints specified list of binary strings,
% with default bullets.
%
-spec binaries_to_binary( [ bin_string() ] ) -> bin_string().
binaries_to_binary( Binaries ) ->
	binaries_to_binary( Binaries, get_default_bullet() ).



% Returns a binary string that pretty-prints specified list of binary strings,
% with user-specified bullets or indentation level.
%
-spec binaries_to_binary( [ bin_string() ], indentation_level_or_bullet() ) ->
								bin_string().
% Not wanting to ever convert to plain strings (to avoid any encoding mismatch):
binaries_to_binary( _Binaries=[], _Bullet ) ->
	<<"(empty list)">>;

% Hopefully a binary:
binaries_to_binary( _Binaries=[ SingleBin ], _Bullet ) ->
	SingleBin;

binaries_to_binary( Binaries, IndentationLevel )
  when is_integer( IndentationLevel ) ->
	Bullet = get_bullet_for_level( IndentationLevel ),
	binaries_to_binary( Binaries, Bullet );

binaries_to_binary( Binaries, Bullet )
					when is_list( Binaries ) andalso is_list( Bullet ) ->

	%trace_utils:debug_fmt( "Binaries: ~p, Bullet: '~p'.",
	%					   [ Binaries, Bullet ] ),

	% Operating first on a list of binaries:
	BinNewline = <<"\n">>,
	Inter = [ BinNewline, Bullet ],
	L = [ [ Inter, Bin ] || Bin <- Binaries ],
	Res = erlang:list_to_binary( L ++ [ BinNewline ] ),
	%trace_utils:debug_fmt( "Returned binary: ~p", [ Res ] ),
	Res;

binaries_to_binary( Binaries, Bullet ) when is_list( Bullet ) ->
	report_not_a_list( Binaries );

binaries_to_binary( _Binaries, IncorrectBullet ) ->
	throw( { bullet_not_a_string, IncorrectBullet } ).



% Returns a string that pretty-prints specified list of atoms, with default
% bullets.
%
-spec atoms_to_string( [ atom() ] ) -> ustring().
atoms_to_string( ListOfAtoms ) ->
	io_lib:format( "~n~ts", [ atoms_to_string( ListOfAtoms, [] ) ] ).


atoms_to_string( [], Acc ) ->
	 Acc;

atoms_to_string( [ H | T ], Acc ) when is_atom( H )  ->
	atoms_to_string( T, Acc ++ get_default_bullet()
						 ++ io_lib:format(  "~ts~n", [ H ] ) ).



% Returns a string that pretty-prints the specified list of atoms once ordered,
% with default bullets.
%
-spec atoms_to_sorted_string( [ atom() ] ) -> ustring().
atoms_to_sorted_string( ListOfAtoms ) ->
	atoms_to_string( lists:sort( ListOfAtoms ) ).



% Returns a string that pretty-prints the specified list of atoms, listed
% directly in the returned text.
%
% Ex: atoms_to_listed_string( [ red, blue, green ] ) returns "red, blue and
% green".
%
-spec atoms_to_listed_string( [ atom() ] ) -> ustring().
atoms_to_listed_string( ListOfAtoms ) ->
	Strings = [ atom_to_string( A ) || A <- ListOfAtoms ],
	strings_to_listed_string( Strings ).


% Returns a string that pretty-prints the specified list of integers, listed
% directly in the returned text.
%
% Ex: integers_to_listed_string( [ 1, 13, 8 ] ) returns "1, 13 and 8".
%
-spec integers_to_listed_string( [ integer() ] ) -> ustring().
integers_to_listed_string( ListOfIntegers ) ->
	Strings = [ integer_to_string( A ) || A <- ListOfIntegers ],
	strings_to_listed_string( Strings ).




% Returns a string that pretty-prints the specified list of strings, listed
% directly along the text (not one item per line).
%
% Ex: strings_to_listed_string( [ "red", "blue", "green" ] ) returns "red, blue
% and green".
%
%strings_to_listed_string( _Strings=[] ) ->
%	throw( empty_list_of_strings_to_list );
% Probably more relevant:
-spec strings_to_listed_string( [ ustring() ] ) -> ustring().
strings_to_listed_string( Strings ) ->
	strings_to_listed_string( Strings, _Lang=english ).



% Returns a string that pretty-prints the specified list of strings, listed
% directly along the text (not one item per line), according to specified
% (human) language.
%
% Ex: strings_to_listed_string( [ "red", "blue", "green" ] ) returns "red, blue
% and green".
%
%strings_to_listed_string( _Strings=[] ) ->
%	throw( empty_list_of_strings_to_list );
% Probably more relevant:
-spec strings_to_listed_string( [ ustring() ],
								language_utils:human_language() ) -> ustring().
strings_to_listed_string( _Strings=[], _Lang ) ->
	"";

strings_to_listed_string( _Strings=[ SingleString ], _Lang ) ->
	SingleString;

strings_to_listed_string( Strings, Lang ) ->

	% Here all strings shall be separated with commas, except the last, starting
	% with "and":

	% We do not want here a dependency onto list_utils, which is not
	% bootstrapped, as this current function might be called from the Myriad
	% parse transform.

	%{ LastString, OtherStrings } = list_utils:extract_last_element(
	%								 Strings ),

	% A somewhat inlined version of it:
	[ LastString | RevOtherStrings ] = lists:reverse( Strings ),

	OtherStrings = lists:reverse( RevOtherStrings ),

	OtherStringsString = join( ", ", OtherStrings ),

	case Lang of

		french ->
			format( "~ts et ~ts", [ OtherStringsString, LastString ] );

		english ->
			format( "~ts and ~ts", [ OtherStringsString, LastString ] )

	end.



% Returns a list whose elements are atoms corresponding to the plain strings
% supposedly composing the specified list.
%
% Ex: strings_to_atoms( ["abc","def"] ) should return [ abc, def ].
%
% Note that only a bounded number of atoms should be created that way, lest the
% atom table gets saturated.
%
-spec strings_to_atoms( [ ustring() ] ) -> [ atom() ].
strings_to_atoms( StringList ) when is_list( StringList ) ->
	[ list_to_atom( X ) || X <- StringList ].



% Returns a string that pretty-prints specified list of key (as binary, string
% or atom) / value pairs, with bullets, after having been sorted.
%
% Ex: proplist_to_string( [ { ccc, 42 }, { "beta", 1.0 } ] ) returns a bullet
% list like:
%
%  + beta: 1.0
%  + ccc: 42
%
-spec proplist_to_string( list_table:list_table() ) -> ustring().
proplist_to_string( Proplist ) ->

	% In this context, key and value known to be strings or atoms:
	Strings = [ io_lib:format( "~ts: ~ts", [ K, V ] )
				|| { K, V } <- lists:sort( Proplist ) ],

	strings_to_string( Strings ).



% Returns a string describing the specified three-element version.
-spec version_to_string( basic_utils:version() ) -> ustring().
version_to_string( { V1, V2, V3 } ) ->
	io_lib:format( "~B.~B.~B", [ V1, V2, V3 ] ).



% Returns a binary string corresponding to the specified atom.
-spec atom_to_binary( atom() ) -> bin_string().
atom_to_binary( Atom ) ->
	% Note: options may apply, like in: erlang:atom_to_binary( X, utf8 ).
	string_to_binary( atom_to_string( Atom ) ).



% Returns a textual description of the specified percentage, expected to be a
% float in [0,1], with the default number of digits after the decimal point.
%
-spec percent_to_string( math_utils:percent() ) -> ustring().
percent_to_string( Value ) ->
	percent_to_string( Value, _DefaultPrecision=1 ).


% Returns a textual description of the specified percentage, expected to be a
% float in [0,1], with the specified number of digits after the decimal point.
%
-spec percent_to_string( math_utils:percent(), integer() ) -> ustring().
percent_to_string( Value, Precision ) ->
	% Awful format string to determine:
	io_lib:format( "~.*f%", [ Precision, Value * 100 ] ).



% Returns a textual description of the specified (dot-based, not comma-based)
% float.
%
-spec float_to_string( float() ) -> ustring().
float_to_string( Float ) ->
	erlang:float_to_list( Float ).


% Returns a textual description of the specified (dot-based, not comma-based)
% float.
%
-spec float_to_string( float(), [ float_option() ] ) -> ustring().
float_to_string( Float, Options ) ->
	erlang:float_to_list( Float, Options ).


% Returns a textual description of the specified (dot-based, not comma-based)
% number.
%
-spec number_to_string( number() ) -> ustring().
number_to_string( I ) when is_integer( I ) ->
	erlang:integer_to_list( I );

number_to_string( F ) when is_float( F ) ->
	erlang:float_to_list( F );

number_to_string( Other ) ->
	report_not_a_number( Other ).



% Returns an exact rounded textual description of the specified distance,
% expected to be expressed as a floating-point number of millimeters, which will
% be first rounded to nearest integer.
%
% Ex: for a distance of 1001.5 millimeters, returns "1m and 2mm".
%
-spec distance_to_string( unit_utils:millimeters()
						 | unit_utils:int_millimeters() ) -> ustring().
distance_to_string( Millimeters ) when is_float( Millimeters ) ->
	distance_to_string( round( Millimeters ) );

% Returns an exact textual description of the specified distance, expected to be
% expressed as an integer number of millimeters.
%
% Ex: for an integer distance of 1000001 millimeters, returns "1km and 1mm".
%
distance_to_string( Millimeters ) ->

	Centimeters = 10,
	Meters = 100 * Centimeters,
	Km = Meters*Meters,

	ListWithKm = case Millimeters div Km of

		0 ->
			[];

		KmNonNull->
			[ io_lib:format( "~Bkm", [ KmNonNull ] ) ]

   end,

	DistAfterKm = Millimeters rem Km,
	%io:format( "DistAfterKm = ~B.~n", [ DistAfterKm ] ),

	ListWithMeters = case DistAfterKm div Meters of

		0 ->
			ListWithKm;

		MetersNonNull->
			[ io_lib:format( "~Bm", [ MetersNonNull ] ) | ListWithKm ]

	end,

	DistAfterMeters = DistAfterKm rem Meters,
	%io:format( "DistAfterMeters = ~B.~n", [ DistAfterMeters ] ),

	ListWithCentimeters = case DistAfterMeters div Centimeters of

		0 ->
			ListWithMeters;

		CentNonNull->
			[ io_lib:format( "~Bcm", [ CentNonNull ] ) | ListWithMeters ]

   end,

	DistAfterCentimeters = DistAfterMeters rem Centimeters,
	%io:format( "DistAfterCentimeters = ~B.~n", [ DistAfterCentimeters ] ),

	ListWithMillimeters = case DistAfterCentimeters of

		0 ->
			ListWithCentimeters;

		AtLeastOneMillimeter ->
			 [ io_lib:format( "~Bmm", [ AtLeastOneMillimeter ] )
			   | ListWithCentimeters ]

	end,

	%io:format( "Unit list is: ~w.~n", [ ListWithMillimeters ] ),

	% Preparing for final display:
	case ListWithMillimeters of

		[] ->
			"0mm";

		[ OneElement ] ->
			OneElement;

		[ Smaller | Bigger ] ->
			join( ", ", lists:reverse( Bigger ) ) ++ " and " ++ Smaller

	end.



% Returns an approximate textual description of the specified distance, expected
% to be expressed as a floating-point number of millimeters, which will be first
% rounded to nearest integer.
%
% Only one unit, the most appropriate one, will be used, with up to 1 figure
% after the comma.
%
% Ex: for a distance of 1000.5 millimeters, returns "1.0m".
%
-spec distance_to_short_string( unit_utils:millimeters()
							   | unit_utils:int_millimeters() ) -> ustring().
distance_to_short_string( Millimeters ) when is_float( Millimeters ) ->
	distance_to_short_string( round( Millimeters ) );

% Returns an approximate textual description of the specified distance, expected
% to be expressed as an integer number of millimeters.
%
% Only one unit, the most appropriate one, will be used, with up to 1 figure
% after the comma.
%
% Ex: for a distance of 1000001 millimeters, returns "1.0km".
%
distance_to_short_string( Millimeters ) ->

	% Note: very specific limit distances could be better managed.
	% Ex: 999999 millimeters is 999m, 99cm and 9mm, and "1000.0m" due to
	% rounding, whereas we would have preferred "1km".

	Centimeters = 10,
	Meters = 100 * Centimeters,
	Km = Meters * Meters,

	% First, guess the most suitable unit, then use it:

	case Millimeters div Km of

		0 ->
			% Kilometers are too big:
			case Millimeters div Meters of

				0 ->
					% Meters are too big:
					case Millimeters div Centimeters of

						0 ->
							% Centimeters are too big, stick to mm:
							io_lib:format( "~Bmm", [ Millimeters ] );

						_CmNonNull ->
							io_lib:format( "~.1fcm",
										   [ Millimeters / Centimeters ] )

					end;

				 _MetersNonNull ->
					io_lib:format( "~.1fm", [ Millimeters / Meters ] )

			end;

		_KmNonNull->
			io_lib:format( "~.1fkm", [ Millimeters / Km ] )

	end.



% Formats specified string as io_lib:format/2 would do, except it returns a
% flattened version of it and cannot fail (so that for example a badly formatted
% log cannot crash anymore its emitter process).
%
% Note: rely preferably on '~ts' rather than on '~s', to avoid unexpected
% Unicode inputs resulting on crashes afterwards.
%
-spec format( format_string(), format_values() ) -> ustring().

-ifdef(exec_target_is_production).

format( FormatString, Values ) ->

	String =
		try

			io_lib:format( FormatString, Values )

		catch

			_:_ ->

				Msg = io_lib:format( "[error: badly formatted string output] "
						"Format string was '~p', values were '~ts'.~n",
						[ FormatString, basic_utils:describe_term( Values ) ] ),

				% Not wanting to be extra verbose in this mode:
				%io:format( Msg ++ "~n", [] ),

				% Useful to obtain the stacktrace of a culprit or to check for
				% silent errors:
				%
				% (note: we are in production mode here)
				%
				%throw( { badly_formatted, FormatString, Values } ),

			   ellipse( Msg, _HighThreshold=2500 )

		end,

	% Using 'flatten' allows for example to have clearer string outputs in case
	% of error (at a rather low cost):
	%
	lists:flatten( String ).


-else. % exec_target_is_production


% In development mode here:
format( FormatString, Values ) ->

	String =
		try

			io_lib:format( FormatString, Values )

		catch

			_:_ ->

				VString = basic_utils:describe_term( Values ),

				Msg = "[error: badly formatted string output] "
					  ++ case is_string( FormatString ) of

					true ->
						case is_list( Values ) of

							true ->
								io_lib:format( "format specified as '~ts', "
									"values as ~ts~ts", [ FormatString, VString,
									interpret_faulty_format( FormatString,
															 Values ) ] );

							false ->
								io_lib:format(
								  "values were not specified as a list "
								  "(i.e. incorrectly as '~ts'; "
								  "format was '~ts')",
								  [ VString, FormatString ] )

						end;

					false ->
						io_lib:format( "format was not specified as a string "
							"(i.e. incorrectly as '~p'; values were '~ts').",
							[ FormatString, VString ] )

				end,

				EllipsedMsg = ellipse( Msg ),

				% If wanting to be extra verbose, duplicating message on the
				% console:
				%
				io:format( Msg ++ "~n~n", [] ),

				% Useful to obtain the stacktrace of a culprit or to check for
				% silent errors:
				%
				% (in development mode here)
				%
				%throw( { badly_formatted, FormatString, Values } ),

				EllipsedMsg

	end,

	% Using 'flatten' allows for example to have clearer string outputs in case
	% of error (at an acceptable cost):
	%
	lists:flatten( String ).


% (beware, still within an -ifdef...)


% Interprets a faulty format command, based on respectively a string and a list.
-spec interpret_faulty_format( format_string(), format_values() ) -> ustring().
interpret_faulty_format( FormatString, Values ) ->

	ValueCount = length( Values ),

	% The always-existing prefix before the first ~ is of no interest:
	SplitSeqs = tl( split( FormatString, _Delimiters=[ $~ ] ) ),

	%trace_utils:debug_fmt( "SplitSeqs = ~p.", [ SplitSeqs ] ),

	% Rough, but sufficient for at least many cases:
	Delimited = [ _AsStringWanted=[ strip_modifiers( FullSeq ) ]
				  || FullSeq <- SplitSeqs ],

	%trace_utils:debug_fmt( "Delimited = ~p", [ Delimited ] ),

	Diagnosis = case Delimited of

		% Not even one control sequence, strange:
		[] ->
			% Avoid any infinite recursion:
			io_lib:format( " (no control sequence detected in format "
						   "string '~s')", [ FormatString ] );

		Seqs ->
			% We filter out "autonomous" control sequences, i.e. the ones that
			% require no specific value:
			%
			VSeqs = [ S || S <- Seqs, requires_value( S ) ],

			SeqCount = length( VSeqs ),

			% Counting value-based control sequences:
			case ValueCount - SeqCount of

				0 ->
					"; apparently the correct number of values "
					"has been specified, so the types may not all match: "
					++ match_types( VSeqs, Values, _Count=1 ); % ++ ".";

				% Very common case:
				1 ->
					io_lib:format( " (expecting ~B values, got ~B, hence an "
						"extra value has been specified)",
						[ SeqCount, ValueCount ] );

				TooMany when TooMany > 1 ->
					io_lib:format( " (expecting ~B values, got ~B, hence ~B "
						"extra values have been specified)",
						[ SeqCount, ValueCount, TooMany ] );

				% Very common case:
				-1 ->
					io_lib:format( " (expecting ~B values, got ~B, hence an "
						"additional value ought to have been specified)",
						[ SeqCount, ValueCount ] );

				TooFew when TooFew < 1 ->
					io_lib:format( " (expecting ~B values, got ~B, hence ~B "
						"additional values ought to have been specified)",
						[ SeqCount, ValueCount, -TooFew ] )

			end

	end,

	% To track origin (not always obvious):
	Diagnosis ++ "; corresponding stack trace was: "
		++ code_utils:interpret_shortened_stacktrace( _SkipLastElemCount=2 ).



% Removes any leading modifier from a format sequence (ex: remove 't' from "ts",
% as if having '~ts' specified, we want to retain only 's').
%
strip_modifiers( [ $t, Next | _T ] ) ->
	Next;

strip_modifiers( [ H | _T ] ) ->
	H.


% Tells whether specified control sequence (without its ~ prefix) requires a
% value (ex: ~B) or not (ex: ~n, ~i).
%
requires_value( "n" ++ _ ) ->
	% ~n does not use a value:
	false;

% Ignore:
requires_value( "i" ++ _ ) ->
	false;

requires_value( _ ) ->
	true.


-endif. % exec_target_is_production




% Formats specified string as io_lib:format/2 would do, except it returns a
% flattened, ellipsed version of it and cannot fail (so that for example a badly
% formatted log cannot crash anymore its emitter process).
%
% Tries to never crash.
%
% Note: rely preferably on '~ts' rather than on '~s', to avoid unexpected
% Unicode inputs resulting on crashes afterwards.
%
%-spec format_ellipsed( format_string(), format_values() ) -> ustring().
format_ellipsed( FormatString, Values ) ->
	ellipse( format( FormatString, Values ), _MaxLen=400 ).



% Formats specified string as io_lib:format/2 would do, except it returns a
% flattened, ellipsed (based on specified length) version of it, and cannot fail
% (so that for example a badly formatted log cannot crash anymore its emitter
% process).
%
% Tries to never crash.
%
% Note: rely preferably on '~ts' rather than on '~s', to avoid unexpected
% Unicode inputs resulting on crashes afterwards.
%
-spec format_ellipsed( format_string(), format_values(), length() ) ->
							 ustring().
format_ellipsed( FormatString, Values, MaxLen ) ->
	ellipse( format( FormatString, Values ), MaxLen ).



% Compares the types specified through control sequences (typically emanating
% from a format string) to the types of specified, numbered values (expected to
% correspond), and detect some mismatches.
%
% Fancy sequences not taken into account: X, x, ts, etc.
%
% Note: beware to the output error messages comprising ~XXX not be afterwards
% interpreted as control sequences; we finally gave up including a ~ character
% in the output sequence, as it has to be escaped a number of times that
% depended on how many io*:format/* it was to go through (fragile at best).
%
-spec match_types( [ control_sequence() ], format_values(),
					 basic_utils:count() ) -> ustring().
match_types( _Seqs=[], _Values=[], _Count ) ->
	"yet no mismatch detected";

% String-like:
match_types( _Seqs=[ _Seq="s" | Ts ], _Values=[ V | Tv ], Count ) ->

	VType = type_utils:get_type_of( V ),

	VString = basic_utils:describe_term( V ),

	% String-compliant primitive types:
	CompliantTypes = [ 'boolean', 'atom', 'binary', 'string', '[string]' ],

	case lists:member( VType, CompliantTypes ) of

		true ->
			%trace_utils:debug_fmt
			%io:format( "[debug] For value #~B (i.e. '~ts'), detected type "
			%	%"is ~ts, which is compliant with the control sequence '~~s'.",
			%	"is ~ts, which is compliant with the control sequence 'ts'.~n",
			%	[ Count, VString, VType ] ),
			match_types( Ts, Tv, Count+1 );

		false ->
			io_lib:format( "type mismatch for value #~B (i.e. '~ts'); got ~ts, "
					"whereas expecting string-like, as the control "
					% Correct, but commented-out for homogeneity with the other
					% clauses:
					% "sequence is ~~~~ts)", [ Count, VString, VType ] )
					"sequence is 's'", [ Count, VString, VType ] )

	end;

% With an Unicode prefix that can be dropped here:
match_types( _Seqs=[ _Seq="ts" | Ts ], Values, Count ) ->
	match_types( [ "s" | Ts ], Values, Count );


% Float:
match_types( _Seqs=[ Seq | Ts ], _Values=[ V | Tv ], Count )
  when Seq =:= "e" orelse Seq =:= "f" orelse Seq =:= "g" ->

	VType = type_utils:get_type_of( V ),

	VString = basic_utils:describe_term( V ),

	case VType =:= float of

		true ->
			%trace_utils:debug_fmt
			%io:format( "[debug] For value #~B (i.e. '~ts'), detected type "
			%	"is ~ts, which is compliant with a control sequence "
			%	%"for floats ('~~~ts').", [ Count, VString, VType, Seq ] ),
			%	"for floats ('~ts').~n", [ Count, VString, VType, Seq ] ),
			match_types( Ts, Tv, Count+1 );

		false ->
			io_lib:format( "type mismatch for value #~B (i.e. '~ts'); got ~ts, "
					"whereas expecting float, as the control "
					%"sequence is ~~~ts)", [ Count, VString, VType, Seq ] )
					"sequence is '~ts'", [ Count, VString, VType, Seq ] )

	end;


% Integer:
match_types( _Seqs=[ Seq | Ts ], _Values=[ V | Tv ], Count )
  when Seq =:= "B" orelse Seq =:= "#"  orelse Seq =:= "b" ->

	VType = type_utils:get_type_of( V ),

	VString = basic_utils:describe_term( V ),

	case VType =:= integer of

		true ->
			%trace_utils:debug_fmt
			%io:format( "[debug] For value #~B (i.e. '~ts'), detected type "
			%	"is ~ts, which is compliant with the control sequence "
			%	"for integers ('~B or # or b').~n",
			%   [ Count, VString, VType, Seq ] ),
			match_types( Ts, Tv, Count+1 );

		false ->
			io_lib:format( "type mismatch for value #~B (i.e. '~ts'): got ~ts, "
					"whereas expecting integer, as the control "
					%"sequence is ~~~ts)", [ Count, VString, VType, Seq ] )
					"sequence is '~ts'", [ Count, VString, VType, Seq ] )

	end;


% Char:
match_types( _Seqs=[ Seq="c" | Ts ], _Values=[ V | Tv ], Count ) ->

	VType = type_utils:get_type_of( V ),

	VString = basic_utils:describe_term( V ),

	case VType =:= integer of

		true ->
			%trace_utils:debug_fmt
			%io:format( "[debug] For value #~B (i.e. '~ts'), detected type "
			%	"is ~ts, which is compliant with the control sequence "
			%	"for chars ('~c').~n", [ Count, VString, VType, Seq ] ),
			match_types( Ts, Tv, Count+1 );

		false ->
			io_lib:format( "type mismatch for value #~B (i.e. '~ts'): got ~ts, "
					"whereas expecting char, as the control "
					%"sequence is ~~~c)", [ Count, VString, VType, Seq ] )
					"sequence is '~ts'", [ Count, VString, VType, Seq ] )

	end;


% Always correct:
match_types( _Seqs=[ Seq | Ts ], _Values=[ _V | Tv ], Count )
  when Seq =:= "w" orelse Seq =:= "p" orelse Seq =:= "P" ->
	match_types( Ts, Tv, Count+1 );


% Not recognised:
match_types( _Seqs=[ Seq | Ts ], _Values=[ V | Tv ], Count ) ->

	VString = basic_utils:describe_term( V ),

	%trace_utils:debug_fmt( "Control sequence '~~~p' (i.e. '~~~w') not "
	%trace_utils:debug_fmt
	io:format( "[warning] Control sequence '~p' (i.e. '~w') not "
		"recognised, accepting value '~ts'.~n", [ Seq, Seq, VString ] ),

	match_types( Ts, Tv, Count+1 ).




% Formats specified text as a comment, based on the default character denoting
% comments (i.e. "%"), for a line width of 80 characters.
%
-spec format_as_comment( ustring() ) -> ustring().
format_as_comment( Text ) ->
	format_as_comment( Text, _CommentChar=$% ).


% Formats specified format string with values as a comment, based on the default
% character denoting comments (i.e. "%"), for a line width of 80 characters.
%
-spec format_as_comment( format_string(), [ term() ] ) -> ustring();
					   ( ustring(), char() ) -> ustring().
format_as_comment( FormatString, Values ) when is_list( Values ) ->
	Text = format( FormatString, Values ),
	format_as_comment( Text );

% Formats specified text as a comment, based on specified character denoting
% comments, for a line width of 80 characters.
%
format_as_comment( Text, CommentChar ) ->
	format_as_comment( Text, CommentChar, _LineWidth=80 ).



% Formats specified text as a comment, based on specified character denoting
% comments, for specified line width.
%
-spec format_as_comment( any_string(), char(), width() ) -> ustring().
format_as_comment( Text, CommentChar, LineWidth ) when is_binary( Text ) ->
	format_as_comment( binary_to_string( Text ), CommentChar, LineWidth );

format_as_comment( Text, CommentChar, LineWidth ) when is_list( Text ) ->

	% To account for the (for example) "% " prefix:
	RemainWidth = LineWidth - 2,

	Elems = split_at_whitespaces( Text ),

	format_as_comment_helper( Elems, CommentChar, RemainWidth, _AccLines=[],
							  _AccLine=[], RemainWidth ).



% Formats specified format string with values as a comment, based on specified
% character denoting comments, for specified line width.
%
-spec format_as_comment( format_string(), [ term() ] , char(), width() ) ->
							   ustring().
format_as_comment( FormatString, Values, CommentChar, LineWidth ) ->
	Text = format( FormatString, Values ),
	format_as_comment( Text, CommentChar, LineWidth ).




% (helper)
format_as_comment_helper( _Text=[], CommentChar, _LineWidth, AccLines, AccLine,
						  _RemainWidth ) ->
	join( _Separator=$\n, lists:reverse(
			  [ get_formatted_line( CommentChar, AccLine ) | AccLines ] ) );

format_as_comment_helper( _Text=[ Word | T ], CommentChar, LineWidth, AccLines,
						  AccLine, RemainWidth ) ->

	WordWidth = length( Word ),

	case WordWidth >= RemainWidth of

		true ->
			%trace_utils:debug_fmt( "Word '~ts' too long, hence to be put on "
			%					   "next line.", [ Word ] ),
			NewAccLines =
				[ get_formatted_line( CommentChar, AccLine ) | AccLines ],

			format_as_comment_helper( T, CommentChar, LineWidth,
				NewAccLines, _AccLine=[ Word ],
				_RemainWidth=LineWidth-WordWidth );

		false ->
			%trace_utils:debug_fmt( "Word '~ts' still fits on the current "
			%   "line.", [ Word ] ),
			format_as_comment_helper( T, CommentChar, LineWidth,
				% Decremented width to account for the space *before* this word:
				AccLines, [ Word | AccLine ], RemainWidth - WordWidth - 1 )

	end.


% (helper)
get_formatted_line( CommentChar, Line ) ->
	[ CommentChar, $ ] ++ join( _Separator=$ , lists:reverse( Line ) ).



% Formats specified string as a (flattened) binary, as io_lib:format/2 would do,
% except it cannot fail (so that for example a badly formatted log cannot crash
% anymore its emitter process).
%
% Note: rely preferably on '~ts' rather than on '~s', to avoid unexpected
% Unicode inputs resulting on crashes afterwards.
%
-spec bin_format( format_string(), [ term() ] ) -> bin_string().
bin_format( FormatString, Values ) ->

	String = format( FormatString, Values ),

	% No flattening needed here:
	%erlang:list_to_binary( String ).
	to_unicode_binary( String ).



% Formats specified string as an atom; cannot fail (so that for example a badly
% formatted log cannot crash anymore its emitter process).
%
% Note: rely preferably on '~ts' rather than on '~s', to avoid unexpected
% Unicode inputs resulting on crashes afterwards.
%
-spec atom_format( format_string(), [ term() ] ) -> atom().
atom_format( FormatSt, FormatValues ) ->
	string_to_atom( format( FormatSt, FormatValues ) ).



% Useful to catch silly mistakes involving an extra comma in a format string:
-spec format( term(), term(), term() ) -> no_return().
format( A, B, C ) ->

	trace_utils:error_fmt( "Call to non-existing function text_utils:format/3; "
		"extra comma in format string? Parameters were: ~ts",
		[ strings_to_enumerated_string( [
				basic_utils:describe_term( T ) || T <- [ A, B, C ] ] ) ] ),

	throw( { faulty_format_call, { A, B, C } } ).



% Note: we deemed safer to consider for ensure_*/1 that atoms shall not be
% directly seen as possible inputs.



% Returns a (plain) string version of the specified text-like parameter.
%
% Never fails because of any transcoding involved.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. We now output
% warning traces whenever the specified element happens not to be a string-like
% element. It is however convenient to define functions whose string parameters
% may be of any possible type (plain or binary).
%
-spec ensure_string( any_string() ) -> ustring().
ensure_string( String ) ->
	ensure_string( String, _CanFailDueToTranscoding=false ).


% Returns a (plain) string version of the specified text-like parameter.
%
% CanFailDueToTranscoding tells whether, should a transcoding fail, this
% function is allowed to fail in turn.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. We now output
% warning traces whenever the specified element happens not to be a string-like
% element. It is however convenient to define functions whose string parameters
% may be of any possible type (plain or binary).
%
-spec ensure_string( any_string(), boolean() ) -> ustring().
ensure_string( String, _CanFailDueToTranscoding ) when is_list( String ) ->
	String;

ensure_string( BinString, CanFailDueToTranscoding )
  when is_binary( BinString ) ->
	binary_to_string( BinString, CanFailDueToTranscoding );

%ensure_string( Int, _CanFailDueToTranscodin ) when is_integer( Int ) ->
%	trace_utils:warning_fmt( "Implicit conversion of integer (here '~B') "
%		"to plain string is now discouraged. "
%		"Use text_utils:integer_to_string/1 instead.", [ Int ] ),
%	integer_to_list( Int );

%ensure_string( F, _CanFailDueToTranscodin ) when is_float( F ) ->
%	trace_utils:warning_fmt( "Implicit conversion of float (here '~f') "
%		"to plain string is now discouraged. "
%		"Use text_utils:float_to_string/1 instead.", [ F ] ),
%	float_to_list( F );

ensure_string( U, _CanFailDueToTranscodin ) ->
	throw( { invalid_value, U } ).



% Returns a list of (plain) string versions of the string-like elements of the
% specified list.
%
% Never fails because of any transcoding involved.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. We now output
% warning traces whenever the specified element happens not to be a string-like
% element. It is however convenient to define functions whose string parameters
% may be of any possible type (plain or binary).
%
-spec ensure_strings( [ term() ] ) -> [ ustring() ].
ensure_strings( Elems ) ->
	ensure_strings( Elems, _CanFailDueToTranscoding=false ).



% Returns a list of (plain) string versions of the string-like elements of the
% specified list.
%
% CanFailDueToTranscoding tells whether, should a transcoding fail, this
% function is allowed to fail in turn.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. We now output
% warning traces whenever the specified element happens not to be a string-like
% element. It is however convenient to define functions whose string parameters
% may be of any possible type (plain or binary).
%
-spec ensure_strings( [ term() ], boolean() ) -> [ ustring() ].
ensure_strings( Elems, CanFailDueToTranscoding ) ->
	[ ensure_string( E, CanFailDueToTranscoding ) || E <- Elems ].






% Returns a binary string version of the specified text-like parameter (binary
% or plain string).
%
% Never fails because of any transcoding involved.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. It is however
% convenient to define functions whose string parameters may be of any possible
% type (plain or binary).
%
-spec ensure_binary( any_string() ) -> bin_string().
ensure_binary( AnyString ) ->
	ensure_binary( AnyString, _CanFailDueToTranscoding=false ).



% Returns a binary string version of the specified text-like parameter (binary
% or plain string).
%
% CanFailDueToTranscoding tells whether, should a transcoding fail, this
% function is allowed to fail in turn.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. It is however
% convenient to define functions whose string parameters may be of any possible
% type (plain or binary).
%
-spec ensure_binary( any_string(), boolean() ) -> bin_string().
ensure_binary( BinString, _CanFailDueToTranscoding )
  when is_binary( BinString ) ->
	BinString;

ensure_binary( String, CanFailDueToTranscoding ) when is_list( String ) ->
	string_to_binary( String, CanFailDueToTranscoding );

ensure_binary( String, _CanFailDueToTranscoding ) ->
	throw( { invalid_value, String } ).



% Returns a list of binary string versions of the string-like elements of the
% specified list.
%
% Never fails because of any transcoding involved.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. It is however
% convenient to define functions whose string parameters may be of any possible
% type (plain or binary).
%
-spec ensure_binaries( [ term() ] ) -> [ bin_string() ].
ensure_binaries( Elems ) ->
	ensure_binaries( Elems, _CanFailDueToTranscoding=false ).



% Returns a list of binary string versions of the string-like elements of the
% specified list.
%
% CanFailDueToTranscoding tells whether, should a transcoding fail, this
% function is allowed to fail in turn.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. It is however
% convenient to define functions whose string parameters may be of any possible
% type (plain or binary).
%
-spec ensure_binaries( [ term() ], boolean() ) -> [ bin_string() ].
ensure_binaries( Elems, CanFailDueToTranscoding ) ->
	[ ensure_binary( E, CanFailDueToTranscoding ) || E <- Elems ].



% Returns the lexicographic distance between the two specified strings, i.e. the
% minimal number of single-character changes in order to transform one string
% into the other one.
%
% The strings are equal iff returns zero.
%
% Directly inspired from
% https://rosettacode.org/wiki/Levenshtein_distance#Erlang and, on
% https://en.wikibooks.org,
% wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Erlang.
%
% See also: https://en.wikipedia.org/wiki/Levenshtein_distance
%
%-spec get_lexicographic_distance_variant( ustring(), ustring() ) -> distance().

% This basic implementation is correct, yet way too inefficient:
%get_lexicographic_distance_variant( FirstString, _SecondString=[] ) ->
%	length( FirstString );

%get_lexicographic_distance_variant( _FirstString=[], SecondString ) ->
%	length( SecondString );

%get_lexicographic_distance_variant( _FirstString=[ H | T1 ],
%									_SecondString=[ H | T2 ] ) ->
%	get_lexicographic_distance_variant( T1, T2 );

%get_lexicographic_distance_variant( FirstString=[ _H1 | T1 ],
%									SecondString=[ _H2 | T2 ] ) ->
%	1 + lists:min( [ get_lexicographic_distance_variant( FirstString, T2 ),
%					 get_lexicographic_distance_variant( T1, SecondString ),
%					 get_lexicographic_distance_variant( T1, T2 ) ] ).


% Significantly more efficient version, using memoization:
-spec get_lexicographic_distance( ustring(), ustring() ) -> distance().
get_lexicographic_distance( FirstString, SecondString ) ->
	{ Distance, _NewAccTable } = get_lexicographic_distance( FirstString,
										 SecondString, _AccTable=?table:new() ),
	Distance.


% Actual helper:
get_lexicographic_distance( _FirstString=[], SecondString, AccTable ) ->
	Len = length( SecondString ),
	NewTable = ?table:add_entry( _K={ [], SecondString }, _V=Len, AccTable ),
	{ Len, NewTable };

get_lexicographic_distance( FirstString, _SecondString=[], AccTable ) ->
	Len = length( FirstString ),
	NewTable = ?table:add_entry( _K={ FirstString, [] }, _V=Len, AccTable ),
	{ Len, NewTable };

get_lexicographic_distance( _FirstString=[ H | T1 ], _SecondString=[ H | T2 ],
							AccTable ) ->
	get_lexicographic_distance( T1, T2 , AccTable );

get_lexicographic_distance( FirstString=[ _H1 | T1 ], SecondString=[ _H2 | T2 ],
							AccTable ) ->
	Key = { FirstString, SecondString },
	case ?table:lookup_entry( Key, AccTable ) of

		{ value, Distance } ->
			{ Distance, AccTable };

		key_not_found ->
			{ Len1, Table1 } = get_lexicographic_distance( FirstString, T2,
														   AccTable ),
			{ Len2, Table2 } = get_lexicographic_distance( T1, SecondString,
														   Table1 ),
			{ Len3, Table3 } = get_lexicographic_distance( T1, T2, Table2 ),
			Len = 1 + lists:min( [ Len1, Len2, Len3 ] ),
			{ Len, ?table:add_entry( Key, Len, Table3 ) }

	end.


% Returns the longest prefix that is common to all of the specified strings, and
% a list of the specified strings with this prefix removed, in the same order.
%
% See also: file_utils:get_longest_common_path/1.
%
-spec get_longest_common_prefix( [ ustring() ] ) ->
										{ ustring(), [ ustring() ] }.
get_longest_common_prefix( _Strings=[] ) ->
	throw( empty_string_list );

get_longest_common_prefix( _Strings=[ S ] ) ->
	{ S, [ "" ] };

get_longest_common_prefix( _Strings=[ S | T ] ) ->
	% If having more than one string, take the first as the reference:
	get_prefix_helper( T, _RefString=S, _AccPrefix=[] ).


% (helper)
get_prefix_helper( Strings, _RefString=[], AccPrefix ) ->
	% Characters of the reference exhausted, it is the prefix as a whole:
	{ lists:reverse( AccPrefix ), [ "" | Strings ] };


get_prefix_helper( Strings, RefString=[ C | T ], AccPrefix ) ->

	case are_all_starting_with( C, Strings ) of

		{ true, NewStrings } ->
			get_prefix_helper( NewStrings, T, [ C | AccPrefix ] );

		false ->
			% Do not forget the reference one:
			{ lists:reverse( AccPrefix ), [ RefString | Strings ] }

	end.



% (helper)
are_all_starting_with( C, Strings ) ->
	are_all_starting_with( C, Strings, _Acc=[] ).


are_all_starting_with( _C, _Strings=[], Acc ) ->
	% String order does not matter:
	{ true, Acc };

% This string matches:
are_all_starting_with( C, _Strings=[ [ C | Rest ] | T ], Acc ) ->
	are_all_starting_with( C, T, [ Rest | Acc ] );

% Either _Strings=[ [] | T ] or _Strings=[ [ NonC | Rest ] | T ]:
are_all_starting_with( _C, _Strings, _Acc ) ->
	false.



% Returns, if possible, the length of the specified string-like argument,
% otherwise returns 'undefined'.
%
% Never fails, but thus may report only indicative lengths (where
% string:length/1 would have thrown a badarg exception).
%
-spec safe_length( unicode_data() ) -> basic_utils:maybe( length() ).
safe_length( PseudoStr ) ->
	try string:length( PseudoStr ) of

		L ->
			L

	catch _:_ ->
		undefined

	end.



% Converts a plain (list-based) string into a binary.
%
% Never fails because of any transcoding involved.
%
-spec string_to_binary( ustring() ) -> bin_string().
string_to_binary( String ) ->
	string_to_binary( String, _CanFailDueToTranscoding=false ).


% Converts a plain (list-based) string into a binary.
%
% CanFailDueToTranscoding tells whether, should a transcoding fail, this
% function is allowed to fail in turn.
%
-spec string_to_binary( ustring(), boolean() ) -> bin_string().
string_to_binary( String, CanFailDueToTranscoding ) when is_list( String ) ->

	%try
	%
	%	% No specific encoding needed:
	%	%Bin = erlang:list_to_binary( String ),
	%
	%	%io:format( "String '~ts' converted to binary '~ts'.",
	%	%		   [ String, Bin ] ),
	%
	%	Bin
	%
	%catch Class:Exception ->
	%
	%	% Ex: might be triggered if String=[8364] ('euro' character), possibly
	%	% if being fed with Unicode string.
	%	%
	%	throw( { invalid_string, String, Class, Exception } )
	%
	%end;

	% Yes, encodings must be managed:
	to_unicode_binary( String, CanFailDueToTranscoding );

string_to_binary( Other, _CanFailDueToTranscoding ) ->
	report_not_a_string( Other ).



% Converts a binary into a plain (list-based) string.
%
% Never fails because of any transcoding involved.
%
-spec binary_to_string( bin_string() ) -> ustring().
binary_to_string( Binary ) when is_binary( Binary ) ->
	binary_to_string( Binary, _CanFailDueToTranscoding=false ).



% Converts a binary into a plain (list-based) string.
%
% CanFailDueToTranscoding tells whether, should a transcoding fail, this
% function is allowed to fail in turn.
%
binary_to_string( Binary, CanFailDueToTranscoding ) when is_binary( Binary ) ->
	%erlang:binary_to_list( Binary );
	to_unicode_list( Binary, CanFailDueToTranscoding );

binary_to_string( Other, _CanFailDueToTranscoding ) ->
	report_not_a_binary_string( Other ).



% Converts a list of plain (list-based) strings into a list of binaries.
%
% Order of items remains unaffected.
%
-spec strings_to_binaries( [ ustring() ] ) -> [ bin_string() ].
strings_to_binaries( StringList ) ->
	% Order must be preserved:
	[ string_to_binary( S ) || S <- StringList ].



% Converts a list of binaries into list of plain (list-based) strings.
%
% Order of items remains unaffected.
%
-spec binaries_to_strings( [ bin_string() ] ) -> [ ustring() ].
binaries_to_strings( BinaryList ) ->

	%trace_utils:debug_fmt( "binaries_to_strings: ~p", [ BinaryList ] ),

	% Order must be preserved:
	%[ erlang:binary_to_list( B ) || B <- BinaryList ].
	[ %try
	  %
	  %    erlang:binary_to_list( B )
	  %
	  %catch _:E ->
	  %
	  %   throw( { binary_conversion_failed, E, B } )
	  %
	  %end

	  to_unicode_list( B ) || B <- BinaryList ].
	  %lists:flatten( io_lib:format( "~ts", [ BinStr ] ) )
	  %    || BinStr <- BinaryList ].



% Returns an integer that corresponds to the specified text.
%
% Throws an exception if the conversion failed.
%
-spec string_to_integer( ustring() ) -> integer().
string_to_integer( String ) ->

	try list_to_integer( String ) of

		I ->
			I

	catch

		error:badarg ->
			throw( { integer_conversion_failed , String } )

	end.



% Returns an integer that corresponds to the specified text (expected to rely on
% our usual base 10).
%
% Returns the 'undefined' atom if the conversion failed.
%
-spec try_string_to_integer( ustring() ) -> basic_utils:maybe( integer() ).
try_string_to_integer( String ) ->
	try_string_to_integer( String, _Base=10 ).



% Returns an integer that corresponds to the specified text, expected to rely on
% the specified base.
%
% Returns the 'undefined' atom if the conversion failed.
%
-spec try_string_to_integer( ustring(), 2..36 ) ->
								   basic_utils:maybe( integer() ).
try_string_to_integer( String, Base ) when is_list( String ) ->
	try list_to_integer( String, Base ) of

		I ->
			I

	catch

		error:badarg ->
			undefined

	end;

try_string_to_integer( Other, _Base ) ->
	report_not_a_string( Other ).



% Returns a float that corresponds to the specified text, not depending on its
% being defined as an integer or as a float.
%
% Throws an exception if the conversion failed.
%
-spec string_to_float( ustring() ) -> float().
string_to_float( String ) ->

	case try_string_to_float( String ) of

		undefined ->
			throw( { float_conversion_failed, String } );

		F ->
			F

	end.



% Returns a float that corresponds to the specified text, not depending on its
% being defined as an integer or as a float.
%
% Returns the 'undefined' atom if the conversion failed.
%
-spec try_string_to_float( ustring() ) -> basic_utils:maybe( float() ).
try_string_to_float( String ) when is_list( String ) ->

	% Erlang is very picky (too much?) when interpreting floats-as-a-string: if
	% there is an exponent, it shall be 'e' (preferably that 'E' which is
	% nevertheless tolerated), and the mantissa must be a floating-point number
	% (hence with a point, such as 3.0e2, not 3e2) and at least one figure must
	% exist after the point (ex: 1.0e2 is accepted, 1.e2 not). Moreover the
	% decimal mark must be '.' (ex: not ',').

	% We overcome all these limitations here, so that for example -1,2E-4, 40E2
	% and 1,E3 are accepted and interpreted correctly.

	% Indeed, 'list_to_float("1e-4")' will raise badarg, whereas
	% 'list_to_float("1.0e-4")' will be accepted.
	%
	% So: if there is no dot on the left of a 'e' or a 'E', add ".0".
	% Moreover, "1.E-4" is also rejected, it must be fixed as well.

	% First, normalise the string, by transforming any 'E' into 'e', and by
	% converting any comma-based decimal mark into a dot:
	%
	LowerString = substitute( _SourceChar=$E, _TargetChar=$e, String ),

	DotString = substitute( $,, $., LowerString ),

	CandidateString = case split_at_first( $e, DotString ) of

		none_found ->
			% There was no exponent here:
			String;

		{ Left, Right } ->
			NewLeft = case split_at_first( $., Left ) of

				none_found ->
					Left ++ ".0";

				% Here there is a dot, yet there is no number afterward (ex:
				% 1.E2), we fix it (to have 1.0E2):
				%
				{ DotLeft, _DotRight="" } ->
					DotLeft ++ ".0";

				{ _DotLeft, _DotRight } ->
					% Already a dot, continue as is:
					Left

			end,
			NewLeft ++ "e" ++ Right

	end,

	try list_to_float( CandidateString ) of

		F ->
			F

	catch

		error:badarg ->

			try list_to_integer( String ) of

				I ->
					float( I )

			catch

				error:badarg ->
					undefined

			end

	end;

% An error (not 'undefined'):
try_string_to_float( Other ) ->
	report_not_a_string( Other ).



% Converts specified plain string into an atom.
%
% Note that only a bounded number of atoms should be created that way, lest the
% atom table gets saturated.
%
-spec string_to_atom( ustring() ) -> atom().
string_to_atom( String ) ->
	try

		erlang:list_to_atom( String )

	catch

		error:badarg ->
			report_not_a_string( String )

	end.



% Returns a textual representation of the specified terms, as a list of their
% user-friendly (i.e. based on ~p) default representation.
%
-spec terms_to_string( [ term() ] ) -> ustring().
terms_to_string( Terms ) ->
	strings_to_string( [ format( "~p", [ T ] ) || T <- Terms ] ).



% Returns a textual representation of the specified terms, as an enumerated list
% of their user-friendly (i.e. based on ~p) default representation.
%
-spec terms_to_enumerated_string( [ term() ] ) -> ustring().
terms_to_enumerated_string( Terms ) ->
	strings_to_enumerated_string( [ format( "~p", [ T ] ) || T <- Terms ] ).



% Returns a textual representation of the specified terms, as a listed
% representation of their user-friendly (i.e. based on ~p) default
% representation.
%
-spec terms_to_listed_string( [ term() ] ) -> ustring().
terms_to_listed_string( Terms ) ->
	strings_to_listed_string( [ format( "~p", [ T ] ) || T <- Terms ] ).



% Converts specified list of plain strings into a corresponding list of atoms.
%
% Note that a bounded number of atoms should be created that way, lest the atom
% table gets saturated.
%
-spec binary_to_atom( bin_string() ) -> atom().
binary_to_atom( Binary ) ->
	String = binary_to_string( Binary ),
	string_to_atom( String ).



% Returns the specified string, ensuring that its first letter is a majuscule,
% uppercasing it if necessary.
%
-spec uppercase_initial_letter( ustring() ) -> ustring().
uppercase_initial_letter( _Letters=[] ) ->
	[];

uppercase_initial_letter( _Letters=[ First | Others ] ) ->
	[ string:to_upper( First ) | Others ].



% Sets the specified string to lowercase, i.e. downcase it (as a whole).
-spec to_lowercase( ustring() ) -> ustring().
to_lowercase( String ) ->
	string:to_lower( String ).


% Sets the specified string to uppercase.
-spec to_uppercase( ustring() ) -> ustring().
to_uppercase( String ) ->
	string:to_upper( String ).



% join(Separator, StringsToJoin), to be used like in:
%      join($-, ["Barbara", "Ann"]) = "Barbara-Ann".
%
% Separator can be a character, like $a, or a string, like ", ".
%
% Python-like 'join', combines items in a list into a string using a separator
% between each item representation.
%
% Inspired from http://www.trapexit.org/String_join_with.
%
% For file-related paths, you are expected to use portable standard
% filename:join/{1,2} functions instead.
%
% Note: conversely, use split/2 to split the string.
%
-spec join( ustring() | uchar(), [ ustring() ] ) -> ustring().
join( _Separator, _ListToJoin=[] ) ->
	"";

join( Separator, ListToJoin ) ->
	%io:format( "ListToJoin = ~p~n", [ ListToJoin ] ),
	lists:flatten( lists:reverse( join( Separator, ListToJoin, _Acc=[] ) ) ).


% Helper:
join( _Separator, _ListToJoin=[], Acc) ->
	Acc;

join( _Separator, _ListToJoin=[ H | [] ], Acc ) ->
	[ H | Acc ];

join( Separator, _ListToJoin=[ H | T ], Acc ) ->
	join( Separator, T, [ Separator, H | Acc ] ).




% Splits the specified string into a list of strings, based on the list of
% specified characters to be interpreted as delimiters.
%
% Note that a series of contiguous delimiters (ex: two spaces in a row) will
% result in inserting empty strings (i.e. []) in the returned list. Use
% split_per_element/2 if wanting to handle series of delimeters as if there was
% only one of them (i.e. if not wanting the returned list to include empty
% strings).
%
% Defined here not to chase anymore after string:tokens/2 and friends.
%
% See also: split_at_whitespaces/0.
%
-spec split( ustring(), [ uchar() ] ) -> [ ustring() ].
split( String, Delimiters ) ->

	%trace_utils:debug_fmt( "Splitting '~ts' with '~ts'.",
	%					   [ String, Delimiters ] ),

	% Note: string:tokens/2 is now deprecated in favor of string:lexemes/2, and
	% and anyway both treat two or more adjacent separator graphemes clusters as
	% only one, which is generally not what we want; so we now use:

	% Would be quite different, as Delimiters here would be understood as a
	% search pattern (i.e. a "word" as a whole) instead of a list of delimiters:
	%
	%string:split( String, _SearchPattern=Delimiters, _Where=all ).

	% Would lead to a breach of contract (no empty string ever inserted):
	%string:lexemes( String, Delimiters ).

	% So we go for a multi-pass splitting (one pass per delimiter):
	split_helper( Delimiters, _Acc=[ String ] ).



% (helper)
split_helper( _Delimiters=[], Acc ) ->
	Acc;

split_helper( _Delimiters=[ D | T ], Acc ) ->
	SplitStrs = [ string:split( S, _SearchPattern=[ D ], _Where=all )
				  || S <- Acc ],
	NewAcc = list_utils:flatten_once( SplitStrs ),
	split_helper( T, NewAcc ).



% Splits the specified string into a list of strings, based on the list of
% specified characters to be interpreted as delimiters.
%
% Note that a series of contiguous delimiters (ex: two spaces in a row) will be
% handled as if there was only one of them (i.e. if the returned list should not
% include empty strings).
%
% See also: split/2.
%
-spec split_per_element( ustring(), [ uchar() ] ) -> [ ustring() ].
split_per_element( String, Delimiters ) ->
	%[ Elem || Elem <- split( String, Delimiters ), Elem =/= [] ].
	string:lexemes( String, Delimiters ).



% Splits the specified parse string (typically returned by parse_quoted/{1,3})
% into a list of plain strings, based on the list of specified characters to be
% interpreted as delimiters.
%
% Note: implemented in an ad hoc way, so that any plain string found in the
% input character stream is properly handled (i.e. not searched for any
% delimiter).
%
% In this example, parsing is needed so that the comma just after the first
% "Bond" is not considered as a delimiter (since it is in a quoted context):
%
% ParsedString = text_utils:parse_quoted( "Hello,'Mr Bond,James Bond',MI6",
%                   _QuotingChars=[ $' ], _EscapingChars=[] ),
%
% ParsedString = "Hello," ++ [ "Mr Bond,James Bond" ] ++ ",MI6",
%
% text_utils:split_parsed( ParsedString, [ $, ] ) =
%      [ "Hello", "Mr Bond, James Bond", "MI6" ]
%
% This allows extracting here three comma-separated fields, while taking into
% account any quoting involved.
%
% See also: split/2, split_per_element/2.
%
-spec split_parsed( parse_string(), [ uchar() ] ) -> [ ustring() ].
split_parsed( ParseString, Delimiters ) ->

	%trace_utils:debug_fmt( "Splitting '~p' with delimiters '~p'...",
	%					   [ ParseString, Delimiters ] ),

	Res = split_parsed( ParseString, Delimiters, _AccElem=[], _AccStrs=[] ),

	%trace_utils:debug_fmt( "... returned: ~p.", [ Res ] ),

	Res.


% Collecting chars in elements (AccElem), then elements in the overall
% accumulator (AccStrs).
%
% We used to avoid adding any empty element, yet this may happen (typically in
% CSV files), hence re-enabled (previous version left commented).
%
% (helper)
%split_parsed( _ParseString=[], _Delimiters, _AccElem=[], AccStrs ) ->
%	lists:reverse( AccStrs );

split_parsed( _ParseString=[], _Delimiters, AccElem, AccStrs ) ->
	lists:reverse( [ lists:reverse( AccElem ) | AccStrs ] );

split_parsed( _ParseString=[ C | T ], Delimiters, AccElem, AccStrs )
	   when is_integer( C ) ->
	case lists:member( C, Delimiters ) of

		true ->
			split_parsed( T, Delimiters, _AccElem=[],
						  [ lists:reverse( AccElem ) | AccStrs ] );

			%case AccElem of
			%
			%	[] ->
			%		split_parsed( T, Delimiters, _AccElem=[], AccStrs );
			%
			%	_ ->
			%		split_parsed( T, Delimiters, _AccElem=[],
			%					  [ lists:reverse( AccElem ) | AccStrs ] )
			%
			%end;

		false ->
			split_parsed( T, Delimiters, [ C | AccElem ], AccStrs )

	end;

split_parsed( _ParseString=[ Str | T ], Delimiters, AccElem, AccStrs )
	   when is_list( Str ) ->
	split_parsed( T, Delimiters, lists:reverse( Str ) ++ AccElem, AccStrs ).



% Splits the specified string into a list of strings, using whitespaces as
% delimiters.
%
-spec split_at_whitespaces( ustring() ) -> [ ustring() ].
split_at_whitespaces( String ) ->
	split( String, list_whitespaces() ).



% Splits the specified string according to the first occurrence of specified
% character: returns a pair of two strings, containing respectively all
% characters strictly before and strictly after the first occurrence of the
% marker (which thus is not kept).
%
% Ex: split_at_first( $x, "  aaaxbbbxccc" ) shall return { "  aaa", "bbbxccc" }.
%
-spec split_at_first( uchar(), ustring() ) ->
							'none_found' | { ustring(), ustring() }.
split_at_first( Marker, String ) ->
	split_at_first( Marker, String, _Acc=[] ).


% Helper:
split_at_first( _Marker, _ToRead=[], _Read ) ->
	none_found;

split_at_first( Marker, _ToRead=[ Marker | T ], Read ) ->
	{ lists:reverse( Read ), T };

split_at_first( Marker, _ToRead=[ Other | T ], Read ) ->
	split_at_first( Marker, T, [ Other | Read ] ).




% Splits the specified string, expected to be containing a word in CamelCase,
% into a list of strings, based on the internal words (delimited by uppercases,
% knowing a series of uppercase letters, except the last one, is considered as
% an acronym, hence as a single word), in their original order.
%
% Ex: split_camel_case( "IndustrialWasteSource" ) shall return [ "Industrial",
% "Waste", "Source" ], while split_camel_case( "TheySaidNYCWasGreat" ) shall
% return [ "They", "Said", "NYC", "Was", "Great" ].
%
-spec split_camel_case( ustring() ) -> [ ustring() ].
split_camel_case( String ) ->

	case is_uppercase( hd( String ) ) of

		true ->
			split_camel_case( String, [] );

		false ->
			throw( { not_camel_case_string, String } )

	end.


% (helper)
split_camel_case( _String=[], Acc ) ->
	lists:reverse( Acc );

split_camel_case( _String=[ HeadChar | MoreChars ], Acc ) ->

	case is_uppercase( HeadChar ) of

		true ->

			% is_uppercase rertuns 'true' if a char is unchanged by 'to_upper',
			% hence non-letter characters will be let in the second string:
			%
			IsLowercase = fun( C ) ->
							  not is_uppercase( C )
						  end,

			{ TailOfWord, MoreWords } =
				lists:splitwith( IsLowercase, MoreChars ),

			NewWord = [ HeadChar | TailOfWord ],

			split_camel_case( MoreWords, [ NewWord | Acc ] );

		false ->

			% Discards the non-letter characters:
			split_camel_case( MoreChars, Acc )

	end.



% Splits the specified string into a list of strings, based on the list of
% separating characters provided in SeparatorsList, then turns these resulting
% strings in the Capitalized Case (all lower-case except for the first letter)
% and finally joins them to get a long CamelCased string.
%
% Ex: tokenizable_to_camel_case( "industrial_WASTE_sOuRCe", "_" ) shall return
% "IndustrialWasteSource", while tokenizable_to_camel_case( "ME HAZ READ J.R.R",
% ". " ) shall return "MeHazReadJRR".
%
-spec tokenizable_to_camel_case( ustring(), ustring() ) -> ustring().
tokenizable_to_camel_case( String, SeparatorsList ) ->

	% Separates the tokens:
	Tokens = string:tokens( String, SeparatorsList ),

	% Makes all the tokens lower-case if needed:
	LowerCaseTokens = [ string:to_lower( Str ) || Str <- Tokens ],

	% Capitalizes all lower-cased tokens:
	CamelCaseTokens = [ uppercase_initial_letter( Str )
						|| Str <- LowerCaseTokens ],

	% Concatenates the capitalized tokens:
	lists:concat( CamelCaseTokens ).




% Substitutes in specified string the source character with the target one (all
% occurrences thereof).
%
% Note: simpler and probably more efficient that a regular expression.
%
-spec substitute( uchar(), uchar(), ustring() | bin_string() ) -> ustring().
substitute( SourceChar, TargetChar, BinString ) when is_binary( BinString ) ->
	substitute( SourceChar, TargetChar, binary_to_string( BinString ) );

substitute( SourceChar, TargetChar, String ) ->
	substitute( SourceChar, TargetChar, String, _Acc=[] ).


substitute( _SourceChar, _TargetChar, _String=[], Acc ) ->
	lists:reverse( Acc );

substitute( SourceChar, TargetChar, _String=[ SourceChar | T ], Acc ) ->
	substitute( SourceChar, TargetChar, T, [ TargetChar | Acc ] );

substitute( SourceChar, TargetChar, _String=[ OtherChar | T ], Acc ) ->
	substitute( SourceChar, TargetChar, T, [ OtherChar | Acc ] ).


% Returns the index, in terms of grapheme clusters, of the first occurrence of
% the specified pattern substring (if any) in the specified string.
%
% An (attempt of) Unicode-aware replacement of string:str/2 and string:rstr/2.
%
-spec find_substring_index( unicode:chardata(), unicode:chardata() ) ->
									gc_index() | 'nomatch'.
find_substring_index( String, SearchPattern ) ->
	find_substring_index( String, SearchPattern, _Direction=leading ).


% Returns the index, in terms of grapheme clusters, of the first or last
% occurrence (depending on the specified direction) of the specified pattern
% substring (if any) in the specified string.
%
% An (attempt of) Unicode-aware replacement of string:str/2 and string:rstr/2.
%
-spec find_substring_index( unicode:chardata(), unicode:chardata(),
							direction() ) -> gc_index() | 'nomatch'.
find_substring_index( String, SearchPattern, Direction ) ->
	GCString = string:to_graphemes( String ),
	GCSearchPattern = string:to_graphemes( SearchPattern ),
	PseudoIndex = case Direction of

		leading ->
			string:str( GCString, GCSearchPattern );

		trailing ->
			string:rstr( GCString, GCSearchPattern )

	end,

	case PseudoIndex of

		0 ->
			nomatch;

		% Indexes of grapheme clusters are to start at 0, not 1:
		I ->
			I-1

	end.



% Filters out in specified string the specified character, so that it does not
% occur anymore on the returned string.
%
% Note: simpler and probably more efficient that a regular expression.
%
-spec filter( uchar(), ustring() ) -> ustring().
filter( CharToRemove, String ) ->
	filter( CharToRemove, String, _Acc=[] ).


filter( _CharToRemove, _String=[], Acc ) ->
	lists:reverse( Acc );

filter( CharToRemove, _String=[ CharToRemove | T ], Acc ) ->
	% Just drop that character:
	filter( CharToRemove, T, Acc );

filter( CharToRemove, _String=[ OtherChar | T ], Acc ) ->
	filter( CharToRemove, T, [ OtherChar | Acc ] ).



% Splits the specified string after specified prefix and returns the remaining
% part, otherwise returns that the prefix was not found.
%
% Ex: split_after_prefix("Foo", "Foobar is baz.") returns "bar is baz.";
% split_after_prefix("ABC", "Foobar is baz.") returns 'no_prefix'.
%
-spec split_after_prefix( ustring(), ustring() ) -> ustring() | 'no_prefix'.
split_after_prefix( _Prefix=[], String ) ->
	String;

split_after_prefix( _Prefix=[ C | T ], _String=[ C | StringT ] ) ->
	split_after_prefix( T, StringT );

split_after_prefix( _Prefix, _String ) ->
	no_prefix.



% Updates specified text with specified keywords, returning a version of which
% where all the specified keywords (the keys of the translation table) have been
% replaced with their associated value (the corresponding value in table).
%
% Ex: text_utils:update_with_keywords("Hello word!", table:new(
%  [{"foo", "bar"}, {"ord", "orld"}])).
%
% See also: file_utils:update_with_keywords/3.
%
-spec update_with_keywords( any_string(), translation_table() ) ->
									[ string_like() ].
update_with_keywords( Content, TranslationTable ) ->

	TransPairs = ?table:enumerate( TranslationTable ),

	% As many passes as keyword pairs:
	lists:foldl(

		fun( { SearchP, Replacement }, ContentAcc ) ->
			string:replace( ContentAcc, SearchP, Replacement, _Where=all )

		end,
		_Acc0=Content,
		_List=TransPairs ).



% Returns a list of all known whitespaces.
-spec list_whitespaces() -> [ char() ].
list_whitespaces() ->
	" \t\n".



% Returns specified text, in which single quotes have been escaped (i.e. "'" has
% been replaced with "\'" - ignore the double quotes in this example).
%
-spec escape_single_quotes( ustring() ) -> ustring().
escape_single_quotes( Text ) ->
	escape_single_quotes_helper( Text, _Acc=[] ).


escape_single_quotes_helper( _Text=[], Acc ) ->
	lists:reverse( Acc );

escape_single_quotes_helper( _Text=[ $' | T ], Acc ) ->
	% As will be reversed:
	escape_single_quotes_helper( T, "'\\" ++ Acc );

escape_single_quotes_helper( _Text=[ C | T ], Acc ) ->
	escape_single_quotes_helper( T, [ C | Acc ] ).



% Returns specified text, in which double quotes have been escaped (i.e. '"' has
% been replaced with '\"' - ignore the single quotes in this example).
%
-spec escape_double_quotes( ustring() ) -> ustring().
escape_double_quotes( Text ) ->
	escape_double_quotes_helper( Text, _Acc=[] ).


escape_double_quotes_helper( _Text=[], Acc ) ->
	lists:reverse( Acc );

escape_double_quotes_helper( _Text=[ $" | T ], Acc ) ->
	% As will be reversed:
	escape_double_quotes_helper( T, "\"\\" ++ Acc );

escape_double_quotes_helper( _Text=[ C | T ], Acc ) ->
	escape_double_quotes_helper( T, [ C | Acc ] ).



% Returns specified text, in which all quotes have been escaped (i.e. ' and "
% have been replaced respectively with \' and \").
%
-spec escape_all_quotes( ustring() ) -> ustring().
escape_all_quotes( Text ) ->
	escape_all_quotes_helper( Text, _Acc=[] ).


escape_all_quotes_helper( _Text=[], Acc ) ->
	lists:reverse( Acc );

escape_all_quotes_helper( _Text=[ $' | T ], Acc ) ->
	% As will be reversed:
	escape_all_quotes_helper( T, "'\\" ++ Acc );

escape_all_quotes_helper( _Text=[ $" | T ], Acc ) ->
	% As will be reversed:
	escape_all_quotes_helper( T, "\"\\" ++ Acc );

escape_all_quotes_helper( _Text=[ C | T ], Acc ) ->
	escape_all_quotes_helper( T, [ C | Acc ] ).



% Escapes, in specified text, all characters in the specified list, with
% specified escaping char.
%
% Ex: "baz\.foobar\.org" = text_utils:escape_with( "baz.foobar.org",
%        [ $. ], $\\ ).
%
-spec escape_with( ustring(), [ char() ], char() ) -> ustring().
escape_with( Text, CharsToEscape, EscapingChar ) ->
	escape_with( Text, CharsToEscape, EscapingChar, _Acc=[] ).


% (helper)
escape_with( _Text=[], _CharsToEscape, _EscapingChar, Acc ) ->
	lists:reverse( Acc );

escape_with( _Text=[ C | T ], CharsToEscape, EscapingChar, Acc ) ->
	NewAcc = case lists:member( C, CharsToEscape ) of

		true ->
			% As will be ultimately reversed:
			[ C, EscapingChar | Acc ];

		false ->
			[ C | Acc ]

	end,

	escape_with( T, CharsToEscape, EscapingChar, NewAcc ).



% Removes all newlines from specified string.
-spec remove_newlines( ustring() ) -> ustring().
remove_newlines( String ) ->
	lists:flatten( string:replace( String, "\n", "", all ) ).



% Parses specified plain (non-iolist) string (i.e. a mere list of characters),
% based on two quoting characters (single and double quotes) and one escaping
% character (backslash), returning a specific kind of iolist containing either
% characters or plain strings, the latter corresponding to the found quoted
% texts, provided they were not escaped.
%
% For example, let's consider an input string such as (using from now @ to
% delimit strings):
%
% @This is an "example \" 'convoluted" string' with various 'quoting elements'.@
%
% Once parsed with this function, it shall be translated to a list containing
% the following series of characters:
%
% @This is an @, then: @example " 'convoluted@, then the series of characters
% corresponding to: @ string' with various 'quoting elements'.@
%
% i.e.: "This is an " ++ [ "example \" 'convoluted" | "string' with
% various 'quoting elements' ].
%
% Note: any escaping character is to escape any of the quoting characters, and
% only them, if being in an unquoted context (i.e. otherwise both will be added
% verbatim in the resulting string).
%
% See text_utils_test.erl for a full example with additional explanations.
%
-spec parse_quoted( plain_string() ) -> parse_string().
parse_quoted( InputStr ) ->
	parse_quoted( InputStr, _QuotingChars=[ $', $" ], _EscapingChars=[ $\\ ] ).



% Parses specified plain (non-iolist) string (i.e. a mere list of characters),
% returning a specific kind of iolist containing either characters or plain
% strings, the latter corresponding to the found quoted texts, provided they
% were not escaped.
%
% Supports user-specified quoting characters and escaping ones.
%
% For example, let's consider an input string such as (using from now @ to
% delimit strings):
%
% @This is an "example \" 'convoluted" string' with various 'quoting elements'.@

% Once parsed with this function when declaring a single quoting character that
% is the double quote (i.e. $") and a single escaping character that is the
% backslash (i.e. $\), it shall be translated to a list containing the following
% series of characters:
%
% @This is an @, then: @example " 'convoluted@, then the series of characters
% corresponding to: @ string' with various 'quoting elements'.@
%
% i.e.: "This is an " ++ [ "example \" 'convoluted" | "string' with
% various 'quoting elements' ].
%
% Note: any escaping character is to escape any of the quoting characters, and
% only them (i.e. otherwise both will be added verbatim in the resulting
% string).
%
% See text_utils_test.erl for a full example with additional explanations, and
% also split_parsed/2..
%
-spec parse_quoted( plain_string(), [ uchar() ], [ uchar() ] ) ->
						  parse_string().
parse_quoted( InputStr, QuotingChars, EscapingChars ) ->

	%trace_utils:debug_fmt( "Parsing @~ts@, with quoting @~ts@ and "
	%    "escaping @~ts@:", [ InputStr, QuotingChars, EscapingChars ] ),

	parse_helper( InputStr, QuotingChars, EscapingChars,
		_CurrentQuoteChar=undefined, _CurrentQuotedText=undefined,
		_PreviousChar=undefined, _Acc=[] ).


% In examples below, double quotes are a quoting character, and backslash an
% escaping one.
%
% The general principal here is to read one character ahead and include the one
% just before it iff relevant.

% Normal endings:

% Ending while a quoting sequence is still open, but here the last (previous)
% character was a closing quoting one:
%
parse_helper( _InputStr=[], _QuotingChars, _EscapingChars, CurrentQuoteChar,
			  CurrentQuotedText, _PreviousChar=CurrentQuoteChar, Acc ) ->

	% Closing for good then:
	RevQuoted = lists:reverse( CurrentQuotedText ),
	lists:reverse( [ RevQuoted | Acc ] );

% Never add 'undefined' chars:
parse_helper( _InputStr=[], _QuotingChars, _EscapingChars,
			  _CurrentQuoteChar=undefined, _CurrentQuotedText=undefined,
			  _PreviousChar=undefined, Acc ) ->
	lists:reverse( Acc );

% Most usual (normal) ending (not in a quoted context):
parse_helper( _InputStr=[], _QuotingChars, _EscapingChars,
			  _CurrentQuoteChar=undefined, _CurrentQuotedText=undefined,
			  PreviousChar, Acc ) ->
	lists:reverse( [ PreviousChar | Acc ] );


parse_helper( _InputStr=[], _QuotingChars, _EscapingChars, CurrentQuoteChar,
			  CurrentQuotedText, PreviousChar, Acc ) ->

	RevQuoted = case PreviousChar of

		undefined ->
			lists:reverse( CurrentQuotedText );

		_ ->
			lists:reverse( [ PreviousChar | CurrentQuotedText ] )

	end,

	CurrentStr = lists:reverse( [ RevQuoted | Acc ] ),

	throw( { unmatched_quoting_char, CurrentQuoteChar,
			 { still_in, RevQuoted },
			 lists:flatten( CurrentStr ) } );


% Still iterating below:

% While not being in a quoted context and reading a character, possibly a
% quoting one:
%
parse_helper( _InputStr=[ C | T ], QuotingChars, EscapingChars,
			  CurrentQuoteChar=undefined, CurrentQuotedText=undefined,
			  _PreviousChar=PrevC, Acc ) ->

	%trace_utils:debug_fmt( "Out of quoted context, read @~ts@ "
	%    "(previous: @~p@), while current, reversed accumulator is:~n  @~p@.",
	%	[ [C], [PrevC], lists:reverse( Acc ) ] ),

	% lists:member/2 not a valid guard, so:
	%
	% (note that having PreviousChar=undefined is nicely handled as well by this
	% code)
	%
	case lists:member( C, QuotingChars ) of

		true ->
			% Read char is a quoting one (while not in a quoted text), so:
			case lists:member( PrevC, EscapingChars ) of

				% The quoting char is escaped, keep it (and only it).
				%
				% Ex: found @\"@; then just retaining @"@ verbatim (we used to
				% drop PrevC=@\@ but it should not):
				%
				true ->

					%trace_utils:debug_fmt( "Out of quoted context, read "
					%	 "quoting char @~ts@ while previous was an escaping "
					%	 "one (@~p@), while current, reversed accumulator "
					%	 "is:~n  @~p@.",
					%    [ [C], [PrevC], lists:reverse( Acc ) ] ),

					parse_helper( T, QuotingChars, EscapingChars,
						CurrentQuoteChar, CurrentQuotedText, _PrevChar=C,
						%Acc );
						[ PrevC | Acc ] );

				% Here, unescaped quoting char while not in quoted text, thus
				% entering a quoting section:
				%
				% (PrevC possibly equal to 'undefined' here)
				%
				false ->
					NewAcc = [ PrevC | Acc ],

					%trace_utils:debug_fmt( "Entering a quoting section with "
					%	"@~ts@, while current, reversed accumulator is:~n  "
					%   "@~p@", [ [C], lists:reverse( NewAcc ) ] ),

					parse_helper( T, QuotingChars, EscapingChars,
						_CurrentQuoteChar=C, _CurrentQuotedText=[],
						_PrevChar=undefined, NewAcc )

			end;

		% The just-read char (C) is not a quoting one, still out of quoted
		% context then:
		%
		false ->
			case PrevC of

				undefined ->
					parse_helper( T, QuotingChars, EscapingChars,
						CurrentQuoteChar, CurrentQuotedText, _PrevChar=C, Acc );

				_ ->
					parse_helper( T, QuotingChars, EscapingChars,
						CurrentQuoteChar, CurrentQuotedText, _PrevChar=C,
						[ PrevC | Acc ] )

			end

	end;


% Here, we are already in a quoted context, and we found a matching quoting
% char:
%
parse_helper( _InputStr=[ C | T ], QuotingChars, EscapingChars,
		CurrentQuoteChar=C, CurrentQuotedText, _PreviousChar=PrevC, Acc ) ->

	%trace_utils:debug_fmt( "In quoted context, read @~ts@ (previous: @~p@) "
	%	"while current quoted text is @~ts@",
	%	[ [C], [PrevC], CurrentQuotedText ] ),

	% Maybe found a closing quoting char - unless it is escaped:
	case lists:member( C, QuotingChars ) of

		true ->
			case lists:member( PrevC, EscapingChars ) of

				% For example @\"@.
				%
				% This quoting char is escaped, thus not counting as such:
				% (quoting C kept in previous, escaping PrevC used to be
				% dropped but should not)
				true ->

					%trace_utils:debug_fmt( "Adding quoting character '~ts' as "
					%	"such, as was escaped (by '~ts').", [ [C], [PrevC] ] ),

					parse_helper( T, QuotingChars, EscapingChars,
						%CurrentQuoteChar, CurrentQuotedText,
						CurrentQuoteChar, [ PrevC | CurrentQuotedText ],
						_PrevChar=C, Acc );

				% For example @A"@.
				% Here, unescaped quoting char while in quoted text, thus
				% closing a quoting section:
				%
				% (PrevC possibly equal to 'undefined' here)
				%
				false ->
					Quoted = case PrevC of

						undefined ->
							lists:reverse( CurrentQuotedText );

						_ ->
							lists:reverse( [ PrevC | CurrentQuotedText ] )

					end,

					%trace_utils:debug_fmt( "Closing a quoting section "
					%	"(result:'~ts') with '~ts', while reversed accumulator "
					%	"is:~n~p", [ Quoted, [C], lists:reverse( Acc ) ] ),

					parse_helper( T, QuotingChars, EscapingChars,
						_CurrentQuoteChar=undefined,
						_CurrentQuotedText=undefined,
						_PrevChar=undefined, [ Quoted | Acc ] )

			end;

		% Is not a quoting char here, thus continuing in quoted:
		false ->
			case PrevC of

				undefined ->
					parse_helper( T, QuotingChars, EscapingChars,
						CurrentQuoteChar, CurrentQuotedText, _PrevChar=C, Acc );

				_ ->
					parse_helper( T, QuotingChars, EscapingChars,
						CurrentQuoteChar, [ PrevC | CurrentQuotedText ],
						_PrevChar=C, Acc )

			end

	end;


% In quoted context, read char not being a matching quoting char, and not having
% a previous char:
%
parse_helper( _InputStr=[ C | T ], QuotingChars, EscapingChars,
		CurrentQuoteChar, CurrentQuotedText, _PreviousChar=undefined, Acc ) ->

	%trace_utils:debug_fmt( "Just recording, in quoted context, "
	%	"current char: @~ts@", [ [C] ] ),

	parse_helper( T, QuotingChars, EscapingChars, CurrentQuoteChar,
				  CurrentQuotedText, _PrevChar=C, Acc );

% Same but with a previous char:
parse_helper( _InputStr=[ C | T ], QuotingChars, EscapingChars,
			  CurrentQuoteChar, CurrentQuotedText, PreviousChar, Acc ) ->

	%trace_utils:debug_fmt( "Recording, in quoted context, "
	%	"current char: @~ts@", [ [C] ] ),

	parse_helper( T, QuotingChars, EscapingChars, CurrentQuoteChar,
				  [ PreviousChar | CurrentQuotedText ], _PrevChar=C, Acc ).



% Tells whether specified character is an uppercase one.
-spec is_uppercase( uchar() ) -> boolean().
is_uppercase( Char ) ->

	% Simplistic but working:

	OneCharacterString = [ Char ],

	case string:to_upper( OneCharacterString ) of

		OneCharacterString ->
			true;

		_ ->
			false

	end.



% Tells whether specified character is a figure (in 0..9).
-spec is_figure( char() ) -> boolean().
is_figure( Char ) when is_integer( Char ) andalso Char >= $0
					   andalso Char =< $9 ->
	true;

is_figure( Char ) when is_integer( Char ) ->
	false.



% Removes the ending "\n" character(s) of specified string.
-spec remove_ending_carriage_return( ustring() ) -> ustring().
remove_ending_carriage_return( String ) when is_list( String ) ->

	% See also: list_utils:remove_last_element/1.

	% 'Res ++ "\n" = String,Res' will not work:
	string:strip( String, right, $\n ).



% Removes the last Count characters from specified string, and returns the
% result.
%
-spec remove_last_characters( ustring(), basic_utils:count() ) -> ustring().
remove_last_characters( String, Count ) ->

	% Not necessarily the most efficient, but at least it is not an illegal
	% pattern:
	%
	case length( String ) of

		C when C >= Count ->
			string:substr( String, 1, C - Count );

		_->
			throw( { removal_failed, String, Count } )

	end.


% Removes all whitespaces from specified string, and returns the result.
-spec remove_whitespaces( ustring() ) -> ustring().
remove_whitespaces( String ) ->
	re:replace( String, "\s", "", [ global, unicode, { return, list } ] ).


% Removes all leading and trailing whitespaces from specified string, and
% returns the result.
%
-spec trim_whitespaces( ustring() ) -> ustring().
trim_whitespaces( String ) ->

	% Should be done in one pass:
	trim_leading_whitespaces( trim_trailing_whitespaces( String ) ).



% Removes all leading whitespaces from specified string, and returns the result.
-spec trim_leading_whitespaces( ustring() ) -> ustring().
trim_leading_whitespaces( String ) ->

	% Largely inspired from http://www.trapexit.org/Trimming_Blanks_from_String:
	re:replace( String, "^\\s*", "", [ unicode, { return, list } ] ).



% Removes all trailing whitespaces from specified string, and returns the
% result.
%
-spec trim_trailing_whitespaces( ustring() ) -> ustring().
trim_trailing_whitespaces( String ) ->

	% The $ confuses some syntax highlighting systems (like the one of some
	% emacs):
	%
	re:replace( String, "\\s*$", "", [ unicode, { return, list } ] ).



% Ellipses (shortens) specified string, so that its total length remains up to
% the default threshold.
%
-spec ellipse( ustring() ) -> ustring().
ellipse( String ) ->
	ellipse( String, _DefaultMaxLen=800 ).



% Ellipses (shortens) specified string, so that its total length remains up to
% specified threshold.
%
-spec ellipse( ustring(), length() | 'unlimited' ) -> ustring().
ellipse( String, _MaxLen=unlimited ) ->
	String;

ellipse( String, MaxLen ) ->

	Suffix = " [...]",

	% To avoid countless computations of a constant:
	SuffixLen = 6,

	case length( String ) of

		L when L > MaxLen ->
			TargetLen = MaxLen - SuffixLen,
			string:slice( String, _Start=0, TargetLen ) ++ Suffix;

		_ ->
			String

	end.



% Ellipses (shortens) specified string to format, so that its total length
% remains up to specified threshold.
%
-spec ellipse_fmt( format_string(), format_values() ) -> ustring().
ellipse_fmt( FormatString, Values ) ->
	ellipse( format( FormatString, Values ) ).



% Formats specified text according to specified width, expressed in characters.
%
% Returns a list of strings, each of which having Width characters.
%
-spec format_text_for_width( ustring(), width() ) -> [ ustring() ].
format_text_for_width( Text, Width ) ->

	% Whitespaces converted to spaces:
	CleanedTest = re:replace( lists:flatten( Text ), "\\s+", " ",
							  [ global, { return, list } ] ),

	WordList = string:tokens( CleanedTest, " " ),

	%io:format( "Formatting ~p.~n", [ WordList ] ),
	join_words( WordList, Width ).



% Joins words from the list, line by line.
join_words( WordList, Width ) ->
	join_words( WordList, Width, _Lines=[], _CurrentLine="",
				_CurrentLineLen=0 ).


join_words( [], _Width, AccLines, _CurrentLine, _CurrentLineLen=0 ) ->
	% Ended with a full line:
	lists:reverse( AccLines );

join_words( [], Width, AccLines, CurrentLine, _CurrentLineLen ) ->
	% Ended with a partial line:
	lists:reverse( [ pad_string( CurrentLine, Width ) | AccLines ] );

join_words( [ Word | RemainingWords ], Width, AccLines, CurrentLine,
			CurrentLineLen ) ->

	%io:format( "Managing word '~ts' (len=~B), current line is '~ts' (len=~B), "
	%	"width = ~B.~n", [ Word, length( Word ), CurrentLine, CurrentLineLen,
	% Width ] ),

	% Length should be incremented, as a space must be inserted before that
	% word, however we want to accept words whose width would be exactly equal
	% to the line width:
	%
	ActualLength = case CurrentLine of

		"" ->
			length( Word );

		_NonEmpty ->
			% Already at least a letter, we therefore must add a space before
			% the new word:
			length( Word ) + 1

	end,

	case ActualLength of

		CompatibleWidth when CompatibleWidth =< Width ->
			% Word width is manageable.
			% Will this word fit on the current line?
			%
			case CurrentLineLen + CompatibleWidth of

				FittingLen when FittingLen =< Width ->
					% Yes, this word fits on the current line.
					% Avoids adding a space at the beginning of a new line:
					{ NewCurrentLine, NewLineLen } = case CurrentLineLen of

						0 ->
							{ Word, CompatibleWidth };

						Len ->
							{ CurrentLine ++ " " ++ Word,
							  Len + CompatibleWidth + 1 }

					end,

					%io:format("Current line is now '~ts'.~n",
					%  [NewCurrentLine]),
					join_words( RemainingWords, Width, AccLines, NewCurrentLine,
								NewLineLen );

				_ExceedingLen ->
					% No, with this word the current line would be too wide,
					% inserting it on new line instead:
					PaddedCurrentLine = pad_string( CurrentLine, Width ),
					%io:format( "Inserting line '~ts'.~n",
					%    [ PaddedCurrentLine ] ),
					join_words( RemainingWords, Width,
					  [ PaddedCurrentLine | AccLines ], Word, CompatibleWidth )

			end;


		_TooLargeWidth ->

			% Will break words as many times as needed:
			%io:format( "Word '~ts' is too large (len=~B), breaking it.~n",
			%	[ Word, length( Word ) ] ),
			Subwords = break_word( Word, Width ),

			PaddedCurrentLine = pad_string( CurrentLine, Width ),

			join_words( Subwords ++ RemainingWords, Width,
						[ PaddedCurrentLine | AccLines ], "", 0 )

	end.



% Returns the specified string, padded with spaces to specified width,
% left-justified (i.e. with spaces added to the right).
%
-spec pad_string( ustring(), width() ) -> ustring().
pad_string( String, Width ) when length( String ) =< Width ->
	pad_string_left( String, Width ).



% Returns the specified string, padded with spaces to specified width,
% left-justified (i.e. with spaces added to the right).
%
-spec pad_string_left( ustring(), width() ) -> ustring().
pad_string_left( String, Width ) when length( String ) =< Width ->

	% Note that the settings listed in
	% http://erlang.org/doc/apps/stdlib/unicode_usage.html shall be enforced so
	% that character encoding is properly supported (with Unicode), otherwise
	% characters such as "e" with an accent are considered as two characters
	% instead of one, leading to incorrect (insufficient) padding:
	%
	lists:flatten( io_lib:format( "~*.ts", [ -Width, String ] ) );

pad_string_left( String, Width ) ->

	Len = length( String ),

	trace_utils:error_fmt( "String '~s' already too long (~B characters) to be "
		"padded (left) to width ~B.", [ String, Len, Width ] ),

	throw( { string_to_pad_left_too_long, String, Len, Width } ).



% Returns the specified string, padded with spaces to specified width,
% right-justified (i.e. with spaces added to the left).
%
-spec pad_string_right( ustring(), width() ) -> ustring().
pad_string_right( String, Width ) when length( String ) =< Width ->
	lists:flatten( io_lib:format( "~*.ts", [ Width, String ] ) );

pad_string_right( String, Width ) ->

	Len = length( String ),

	trace_utils:error_fmt( "String '~s' already too long (~B characters) to be "
		"padded (right) to width ~B.", [ String, Len, Width ] ),

	throw( { string_to_pad_right_too_long, String, Len, Width } ).



% Returns true iff the parameter is a (non-nested) string (actually a plain list
% of integers).
%
% Taken from http://lethain.com
% (see distinguishing-strings-from-lists-in-erlang)
%
% Note: something like [ $e, 1, 2, $r ] is deemed to be a string.
%
-spec is_string( term() ) -> boolean().
is_string( [] ) ->
	true;

is_string( [ H | _ ] ) when not is_integer( H ) ->
	false;

is_string( [ _ | T ] ) ->
	is_string( T );

is_string( _Other ) ->
	false.

% Alternate, less efficient version:
%is_string( Term ) when is_list( Term ) ->
%			lists:all( fun erlang:is_integer/1, Term );
%
%is_string( _Term ) -> false.


% Returns true iif the parameter is a (non-nested) non-empty string (actually a
% plain list of at least one integer).
%
-spec is_non_empty_string( term() ) -> boolean().
is_non_empty_string( [] ) ->
	% Shall be not empty:
	false;

is_non_empty_string( [ H ] ) when is_integer( H ) ->
	true;

is_non_empty_string( [ H | T ] ) when is_integer( H ) ->
	is_non_empty_string( T );

is_non_empty_string( _Other ) ->
	false.



% Returns true iff the specified parameter is a list whose all elements are
% (all) plain strings.
%
% Note: especially useful knowing that a string is itself a list, hence a string
% can easily be mistaken for a list of strings, in which case each of these
% strings would actually be found being an integer instead (corresponding to
% each of the characters of the overall string).
%
-spec are_strings( list() ) -> boolean().
are_strings( [] ) ->
	true;

are_strings( [ H | T ] ) ->

	case is_string( H ) of

		true ->
			are_strings( T );

		false ->
			false

	end;

are_strings( _Other ) ->
	false.




% Returns true iff the specified parameter is a binary string.
-spec is_bin_string( term() ) -> boolean().
is_bin_string( Term ) when is_binary( Term ) ->
	%is_string( binary_to_list( Term ) );
	true;

is_bin_string( _Term ) ->
	false.



% Tells whether specified term is a list of binary strings.
-spec are_binaries( term() ) -> boolean().
are_binaries( List ) when is_list( List ) ->
	lists:all( fun is_bin_string/1, List );

are_binaries( _NotList ) ->
	false.



% Returns whether the two specified strings are of the same type (both plain or
% both binary ones).
%
-spec are_of_same_string_type( any_string(), any_string() ) -> boolean().
are_of_same_string_type( S1, S2 ) when is_list( S1 ) andalso is_list( S2 ) ->
	true;

are_of_same_string_type( S1, S2 ) when is_binary( S1 )
									   andalso is_binary( S2 ) ->
	true;

are_of_same_string_type( _S1, _S2 ) ->
	false.



% Returns a list of words obtained from the breaking of specified word,
% according to specified maximum width.
%
% Parts of that word will use a separating dash.
%
% Ex: break_word( "simulator", 5 ) returns [ "simu-", "lator" ].
%
break_word( Word, Width ) ->

	% We do not want to have underscores in the word, as if the word happens
	% to be broken just after an underscore, RST will interpret it as a link.
	% Therefore we escape underscores:
	%
	% Used to cut into halves, then preferring truncating a first full-length
	% chunk, finally directly cutting the word into appropriate pieces:
	% CutIndex = length(Word) div 2,
	% CutIndex = Width-1,
	cut_into_chunks( Word, Width, _Acc=[] ).



% Cuts specified string into pieces, each of them having to fit in specified
% width.
%
cut_into_chunks( _String=[], _ChunkSize, Acc ) ->
	%io:format( "cut_into_chunks return ~p.", [ lists:reverse( Acc ) ] ),
	lists:reverse( Acc );

% Last word may take the full width (no dash to add):
cut_into_chunks( String, ChunkSize, Acc ) when length( String ) =< ChunkSize ->
	cut_into_chunks( [], ChunkSize, [ String | Acc ] );

% Here we have to cut the string anyway:
cut_into_chunks( String, ChunkSize, Acc ) ->

	% Rule is to add (and convert) characters until the end of line:
	% (ChunkSize decremented as "-" will be added)

	{ FirstPart, Remaining } = aggregate_word( String, ChunkSize-1, [] ),

	% Each underscore will result into another character (\) being added:
	%io:format( "FirstPart = '~ts' (~B), Remaining = '~ts'.~n",
	%	[ FirstPart, length( FirstPart ), Remaining ] ),
	cut_into_chunks( Remaining, ChunkSize, [ FirstPart ++ "-" | Acc ] ).



aggregate_word( String, 0, Acc ) ->
	{ lists:reverse( Acc ), String };


% An underscore once escaped would not fit, as it would result into two
% characters ('\_'):
%
aggregate_word( String=[ $_ | _T ], 1, Acc ) ->
	aggregate_word( String, 0, Acc );

% An escaped underscore will fit:
aggregate_word( [ $_ | T ], Count, Acc ) ->
	% Adding '_\' as it will reversed (into the expected '\_'):
	aggregate_word( T, Count-2, [ $\_, $\\ | Acc ] );

aggregate_word( [ H | T ], Count, Acc ) ->
	aggregate_word( T, Count-1, [ H | Acc ] ).




% Tries to convert specified Unicode-related datastructure into a flat, plain
% Unicode string.
%
% (exported helper, for re-use)
%
-spec try_convert_to_unicode_list( unicode:unicode_data() ) ->
											basic_utils:maybe( ustring() ).
try_convert_to_unicode_list( Data ) ->

	% A binary_to_list/1 would not be sufficient here.

	% It seems that using io_lib:format( "~ts", [ Data ] ) could still be an
	% option.

	% Possibly a deep list:
	case unicode:characters_to_list( Data ) of

		Str when is_list( Str ) ->
			Str;

		_ ->
			undefined

	end.



% Converts specified Unicode-related datastructure into a flat, plain Unicode
% string.
%
% Never fails yet can return a bogus string.
%
% (exported helper, for re-use)
%
-spec to_unicode_list( unicode_data() ) -> ustring().
to_unicode_list( Data ) ->
	to_unicode_list( Data, _CanFail=false ).


% Converts specified Unicode-related datastructure into a flat, plain Unicode
% string.
%
% If enabled, fails if the conversion cannot be properly done, otherwise can
% return a bogus string.
%
% (exported helper, for re-use)
%
-spec to_unicode_list( unicode_data(), boolean() ) -> ustring().
to_unicode_list( Data, CanFail ) ->

	% A binary_to_list/1 would not be sufficient here.

	% It seems that using io_lib:format( "~ts", [ Data ] ) could still be an
	% option.

	% Possibly a deep list:
	case unicode:characters_to_list( Data ) of

		Str when is_list( Str ) ->
			Str;

		{ error, Prefix, Remaining } ->
			trace_bridge:error_fmt( "Cannot transform data '~p' into "
				"a proper Unicode string:~nafter prefix '~s', "
				"cannot convert '~w'.~nStacktrace was: ~s",
				[ Data, Prefix, Remaining,
				  code_utils:interpret_shortened_stacktrace( 1 ) ] ),
			case CanFail of

				true ->
					throw( { improper_data_for_string, Data, Prefix,
							 Remaining } );

				false ->
					% Best effort:
					io_lib:format( "~ts## SUFFIX COULD NOT BE CONVERTED",
								   [ Prefix ] )

			end;

		{ incomplete, Prefix, Bin } ->
			trace_bridge:error_fmt( "Cannot transform data '~p' into "
				"a proper Unicode string:~nafter prefix '~s', "
				"'~p' is incomplete.", [ Data, Prefix, Bin ] ),
			case CanFail of

				true ->
					throw( { incomplete_data_for_string, Data, Prefix, Bin } );

				false ->
					% Best effort:
					io_lib:format( "~ts## A SUFFIX WAS LACKING", [ Prefix ] )

			end

	end.




% Tries to convert specified Unicode-related datastructure into a Unicode binary
% string.
%
% (exported helper, for re-use)
%
-spec try_convert_to_unicode_binary( unicode_data() ) ->
											basic_utils:maybe( bin_string() ).
try_convert_to_unicode_binary( Data ) ->

	% A list_to_binary/1 would not be sufficient here.

	% Possibly a deep list:
	case unicode:characters_to_binary( Data ) of

		Bin when is_binary( Bin ) ->
			Bin;

		_Other ->
			%trace_utils:debug_fmt( "For '~p', got:~n~p", [ Data, Other ] ),
			undefined

	end.


% (exported helper, for re-use)
-spec to_unicode_binary( unicode_data() ) -> bin_string().
to_unicode_binary( Data ) ->
	to_unicode_binary( Data, _CanFail=false ).


% (exported helper, for re-use)
-spec to_unicode_binary( unicode_data(), boolean() ) -> bin_string().
to_unicode_binary( Data, CanFail ) ->

	% A list_to_binary/1 would not be sufficient here.

	% Possibly a deep list:
	case unicode:characters_to_binary( Data ) of

		Bin when is_binary( Bin ) ->
			Bin;

		{ error, Prefix, Remaining } ->
			trace_bridge:error_fmt( "Cannot transform data '~p' into "
				"a proper Unicode binary:~nafter prefix '~s', "
				"cannot convert '~p'.", [ Data, Prefix, Remaining ] ),
			case CanFail of

				true ->
					throw( { improper_data_for_binary, Data, Prefix,
							 Remaining } );

				false ->
					% Best effort; hopefully relevant:
					list_to_binary( io_lib:format(
						"~ts## SUFFIX COULD NOT BE CONVERTED", [ Prefix ] ) )

			end;

		{ incomplete, Prefix, Bin } ->
			trace_bridge:error_fmt( "Cannot transform data '~p' into "
				"a proper Unicode binary:~nafter prefix '~s', "
				"'~p' is incomplete.", [ Data, Prefix, Bin ] ),
			case CanFail of

				true ->
					throw( { incomplete_data_for_binary, Data, Prefix, Bin } );

				false ->
					% Best effort; hopefully relevant:
					list_to_binary( io_lib:format(
						"~ts## A SUFFIX WAS LACKING", [ Prefix ] ) )

			end

	end.



% Restructured-Text (RST) related functions.


% Generates a RST-compatible standard title, with the proper ASCII art.
% Follows our general conventions regarding title level, from H1 to Hn.
%
-spec generate_title( ustring(), 1..9 ) -> ustring().
generate_title( Title, Level ) ->

	{ Char, Layout } = get_title_rendering_for( Level ),

	TitleLine = get_line_of( Char, length( Title ) ) ++ "\n",

	case Layout of

		only_below ->
			Title ++ "\n" ++ TitleLine ++ "\n";

		below_and_on_top ->
			TitleLine ++ Title ++ "\n" ++ TitleLine ++ "\n"

	end.



% Returns how a title with specified level can be rendered.
% See demo-for-css-testing.rst for the convention.
%
get_title_rendering_for( 1 ) ->
	{ $=, below_and_on_top };

get_title_rendering_for( 2 ) ->
	{ $-, below_and_on_top };

get_title_rendering_for( 3 ) ->
	{ $=, only_below };

get_title_rendering_for( 4 ) ->
	{ $-, only_below };

get_title_rendering_for( 5 ) ->
	{ $., only_below };

get_title_rendering_for( 6 ) ->
	{ $_, only_below };

get_title_rendering_for( 7 ) ->
	{ $*, only_below };

get_title_rendering_for( 8 ) ->
	{ $:, only_below };

get_title_rendering_for( 9 ) ->
	{ $+, only_below }.




% Returns a line made of Length characters "Character".
% Ex: get_line_of( $+, 5 ) = "+++++".
%
get_line_of( Character, Length ) ->
	lists:flatten( [ Character || _X <- lists:seq( 1, Length ) ] ).



% Miscellaneous functions.


% Tries to return a string adequate to form a simple name (mostly alphanumerical
% with underscores) from specified term.
%
% See also: file_utils:convert_to_filename/1.
%
-spec generate_text_name_from( term() ) -> ustring().
generate_text_name_from( Term ) ->
	String = term_to_string( Term ),
	fix_characters( String ).




% Non-exported helper functions.

fix_characters( String ) ->
	lists:reverse( fix_characters( lists:flatten( String ), [] ) ).


fix_characters( [], Acc ) ->
	Acc;

% 32 corresponds to space ('$ '):
fix_characters( [ 32 | T ], Acc ) ->
	fix_characters( T, [ "_" | Acc ] );

fix_characters( [ $' | T ], Acc ) ->
	fix_characters( T, [ "_" | Acc ] );

fix_characters( [ H | T ], Acc ) ->
	fix_characters( T, [ H | Acc ] ).




% As too often (ex: with gen_statem) no relevant origin location is specified:

-spec report_not_a_string( any() ) -> no_return().
report_not_a_string( Term ) ->
	report_wrong_type( not_a_string, Term ).


-spec report_not_a_binary_string( any() ) -> no_return().
report_not_a_binary_string( Term ) ->
	report_wrong_type( not_a_binary_string, Term ).


-spec report_not_a_list( any() ) -> no_return().
report_not_a_list( Term ) ->
	report_wrong_type( not_a_list, Term ).


-spec report_not_a_number( any() ) -> no_return().
report_not_a_number( Term ) ->
	report_wrong_type( not_a_number, Term ).


% Allows to report at runtime a wrong type, with or without a stacktrace.
-spec report_wrong_type( atom(), term() ) -> no_return().

-ifdef(myriad_add_stacktraces).

report_wrong_type( NotThisType, Term ) ->

	% Not wanting the stacktrace to include these last error-reporting
	% functions:
	%
	Stacktrace = code_utils:get_stacktrace( _SkipLastElemCount=2 ),

	throw( { NotThisType, Term, { stacktrace, Stacktrace } } ).

-else. % myriad_add_stacktraces

report_wrong_type( NotThisType, Term ) ->
	throw( { NotThisType, Term } ).

-endif. % myriad_add_stacktraces
