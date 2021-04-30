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


% Unit tests for the text utils toolbox.
%
% See the text_utils.erl tested module.
%
-module(text_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



% For pretty-printing test:
%-record( my_test_record, {

%	first_field,
%	second_field = 1,
%	third_file = "This is a test"

%} ).

% To comment-out more easily:
-export([ test_format_error/0 ]).


print_title( Title, Level ) ->
	test_facilities:display( "Title level ~B:~n~ts",
		[ Level, text_utils:generate_title( Title, Level ) ] ).


test_format_error() ->

	% To test the error management and interpretation done by
	% text_utils:format/2:

	test_facilities:display(
	  "~nTesting on purpose 5 mismatching text_utils:format/2 calls:" ),

	% One too few:
	_ = text_utils:format( "aaaa~tsbbbb", [] ),

	% One too many:
	_ = text_utils:format( "aaaa~tsbb~wbb", [ u, v, w ] ),

	% Wrong types:
	_ = text_utils:format( "~aaaa~tsbbbb", [ 1.2 ] ),
	_ = text_utils:format( "~Baaaa~tsbbbb", [ 1.2, "hello" ] ),
	_ = text_utils:format( "~Baaaa~tsbb~tsbb", [ 2, self(), "hello" ] ),

	ok.



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	IndentLevel = 0,

	AStrings = [ "A1", "A2", "A3" ],

	AString = "the As are: "
		++ text_utils:strings_to_string( AStrings, IndentLevel + 1 ),

	BStrings = [ "B1", "B2", "B3" ],

	BString = "the Bs are: "
		++ text_utils:strings_to_string( BStrings, IndentLevel + 1 ),

	NestedStrings = text_utils:strings_to_string( [ AString, BString ] ),

	test_facilities:display( "Test of nested strings: ~ts and that's it!",
							 [ NestedStrings ] ),

	test_facilities:display( "Converting an integer to a string: ~ts.",
		[ text_utils:integer_to_string( 3245 ) ] ),

	test_facilities:display( "Converting an atom to a string: ~ts.",
		[ text_utils:atom_to_string( 'hello world' ) ] ),


	test_facilities:display( "Converting a PID to a string: '~ts'.",
							 [ text_utils:pid_to_string( self() ) ] ),

	test_facilities:display( "Converting a PID to a short string: '~ts'.",
							 [ text_utils:pid_to_short_string( self() ) ] ),


	PidList = [ self(), self(), self() ],

	test_facilities:display( "Converting PIDs to a string: '~ts'.",
							 [ text_utils:pids_to_string( PidList ) ] ),

	test_facilities:display( "Converting PIDs to a short string: '~ts'.",
							 [ text_utils:pids_to_short_string( PidList ) ] ),


	%MyTestRecord = #my_test_record{},

	%test_facilities:display( "Converting a record instance to a string: "
	% "~ts.", [ text_utils:record_to_string( MyTestRecord ) ] ),


	test_facilities:display( "Output with term_to_string : ~ts, ~ts and ~ts.",
		[ text_utils:term_to_string( an_atom ),
		  text_utils:term_to_string( [ 1, 2 ] ),
		  text_utils:term_to_string( "A string" ) ] ),


	MaxDepth = 1,
	MaxLen = 5,

	test_facilities:display( "More output with term_to_string "
		"with max depth ~B and max length ~B: ~ts, ~ts and ~ts.",
		[ MaxDepth, MaxLen,
		  text_utils:term_to_string( an_atom, MaxDepth, MaxLen ),
		  text_utils:term_to_string( [ 1, 2 ], MaxDepth, MaxLen ),
		  text_utils:term_to_string( "A string", MaxDepth, MaxLen ) ] ),


	ListOfStrings = [ "Hello", "World", "Vampire" ],

	test_facilities:display( "Displaying list ~p as a string: ~ts",
		[ ListOfStrings, text_utils:strings_to_string( ListOfStrings ) ] ),


	NestedStringsForIndent = [ [ "A1" ] ],
	%NestedStringsForIndent = [ [ "A1", "A2", "A3" ], [ "B1" ], [ "C1", "C2" ],
	%                           [ "D1" ] ],

	Strings = [ text_utils:format( "blah: ~ts",
				 [ text_utils:strings_to_string( N, _IndentationLevel=1 ) ] )
				|| N <- NestedStringsForIndent ],

	% Emulating the way it is used in practice:
	test_facilities:display( "Displaying nested strings: ~ts and continuing.",
		[ text_utils:strings_to_string( Strings ) ] ),

	test_format_error(),

	LongLine = "This is a long line to test the paragraph formatting.",

	% So that "formatting." has a chance to fit:
	TargetWidth = 10,

	test_facilities:display( "Displaying text '~ts' once formatted "
		"for a width of ~B:~n~p", [ LongLine, TargetWidth,
			text_utils:format_text_for_width( LongLine, TargetWidth ) ] ),


	JustWideEnoughLine = "<0.33.0>",

	% So that "formatting." has a chance to fit:
	NewTargetWidth = 8,

	test_facilities:display( "Displaying text '~ts' once formatted "
		"for a width of ~B:~n~p",
		[ JustWideEnoughLine, NewTargetWidth, text_utils:format_text_for_width(
								 JustWideEnoughLine, NewTargetWidth ) ] ),


	test_facilities:display( "Displaying atom list, obtained from string "
		"list ~p: ~p.",
		[ ListOfStrings, text_utils:strings_to_atoms( ListOfStrings ) ] ),


	FirstTestString = "Hello world!",

	test_facilities:display( "Determining whether '~p' is a string: ~w; "
		"a non-empty string: ~w",
		[ FirstTestString, text_utils:is_string( FirstTestString ),
		  text_utils:is_non_empty_string( FirstTestString ) ] ),

	true = text_utils:is_string( FirstTestString ),
	true = text_utils:is_non_empty_string( FirstTestString ),


	SecondTestString = [ $o, [ $s, $d ], $l ],

	test_facilities:display( "Determining whether '~p' is a string: ~w; "
		"a non-empty string: ~w", [ SecondTestString,
		text_utils:is_string( SecondTestString ),
		text_utils:is_non_empty_string( SecondTestString ) ] ),

	false = text_utils:is_string( SecondTestString ),
	false = text_utils:is_non_empty_string( SecondTestString ),


	ThirdTestString = [ $e, 1, 2, $r ],

	test_facilities:display( "Determining whether '~p' is a string: ~w; "
		"a non-empty string: ~w", [ ThirdTestString,
		text_utils:is_string( ThirdTestString ),
		text_utils:is_non_empty_string( ThirdTestString ) ] ),

	true = text_utils:is_string( ThirdTestString ),
	true = text_utils:is_non_empty_string( ThirdTestString ),


	FourthTestString = an_atom,

	test_facilities:display( "Determining whether '~p' is a string: ~w; "
		"a non-empty string: ~w", [ FourthTestString,
		text_utils:is_string( FourthTestString ),
		text_utils:is_non_empty_string( FourthTestString ) ] ),

	false = text_utils:is_string( FourthTestString ),
	false = text_utils:is_non_empty_string( FourthTestString ),


	FifthTestString = "",

	test_facilities:display( "Determining whether '~p' is a string: ~w; "
		"a non-empty string: ~w", [ FifthTestString,
		text_utils:is_string( FifthTestString ),
		text_utils:is_non_empty_string( FifthTestString ) ] ),

	true = text_utils:is_string( FifthTestString ),
	false = text_utils:is_non_empty_string( FifthTestString ),


	FirstList = [],
	test_facilities:display(
		"Determining whether '~p' is a list of strings: ~w.",
		[ FirstList, text_utils:are_strings( FirstList ) ] ),
	true = text_utils:are_strings( FirstList ),

	SecondList = [ FirstTestString ],
	test_facilities:display( "Determining whether '~p' is "
		"a list of strings: ~w.", [ SecondList,
		text_utils:are_strings( SecondList ) ] ),

	true = text_utils:are_strings( SecondList ),

	ThirdList = [ FirstTestString, ThirdTestString ],

	test_facilities:display(
		"Determining whether '~p' is a list of strings: ~w.",
		[ ThirdList, text_utils:are_strings( ThirdList ) ] ),
	true = text_utils:are_strings( ThirdList ),

	FourthList = [ FirstTestString, SecondTestString ],
	test_facilities:display(
		"Determining whether '~p' is a list of strings: ~w.",
		[ FourthList, text_utils:are_strings( FourthList ) ] ),
	false = text_utils:are_strings( FourthList ),


	Title = "Alien creatures invaded Ireland!",

	[ print_title( Title, Level ) || Level <- lists:seq( 1, 9 ) ],

	Percent = 0.1234,

	test_facilities:display( " Displaying ~p as a percentage: ~ts.",
			  [ Percent, text_utils:percent_to_string( Percent ) ] ),


	test_facilities:display( " Checking string/binary conversions." ),

	"hello" = text_utils:binary_to_string( <<"hello">> ),
	 <<"hello">> = text_utils:string_to_binary( "hello" ),

	StringList = [ "hello", "world" ],
	BinList = [ <<"hello">>, <<"world">> ],

	% Order matters:
	BinList = text_utils:strings_to_binaries( StringList ),
	StringList = text_utils:binaries_to_strings( BinList ),

	10.0 = text_utils:string_to_float( "10" ),
	10.0 = text_utils:string_to_float( "10.0" ),
	-1.2e-4 = text_utils:string_to_float( "-1,2E-4" ),
	4.0e3 = text_utils:string_to_float( "40E2"),
	1.0e3 = text_utils:string_to_float( "1,E3"),

	try

		text_utils:string_to_float( "Not a float" )

	catch

		throw:_ ->
			ok

	end,

	123 = text_utils:string_to_integer( "123" ),

	% Test also failures:
	%text_utils:string_to_integer( "aa123bb" ),
	%text_utils:string_to_integer( "123.45" ),

	test_facilities:display( " Checking string/atom conversions." ),

	OtherStringList = [ "The", "little red", "wolf" ],
	test_facilities:display(
		"When strings: ~ts are converted into atoms, we have: ~w.",
		[ text_utils:strings_to_string( OtherStringList ),
		  text_utils:strings_to_atoms( OtherStringList ) ] ),

	Colors = [ red, blue, green ],

	ListedColors = "red, blue and green" =
		text_utils:atoms_to_listed_string( Colors ),

	test_facilities:display( "Listing ~p: '~ts'.", [ Colors, ListedColors ] ),

	RefString = "Hello world",

	CompareStrings = [ RefString, "Hello", "HELLO WORLD", "Hello Walter",
					   "Little red rooster", RefString ++ " foobar" ],

	ResultStrings = [ text_utils:format( "'~ts': ~B", [ S,
			text_utils:get_lexicographic_distance( RefString, S ) ] )
					  || S <- CompareStrings ],

	test_facilities:display( "Lexicographic distance between '~ts' and: ~ts",
		[ RefString, text_utils:strings_to_string( ResultStrings ) ] ),

	% Variant tested yet way too slow, hence fully disabled:
	%VariantResultStrings = [ text_utils:format( "'~ts': ~B", [ S,
	%		text_utils:get_lexicographic_distance_variant( RefString, S )
	%					] ) || S <- CompareStrings ],

	%test_facilities:display( "Lexicographic distance between '~ts' "
	%						 "and (variant): ~ts",
	%	[ RefString, text_utils:strings_to_string( VariantResultStrings ) ] ),


	FirstInput = [ "abca", "xyz" ],
	{ "", FirstInput } = text_utils:get_longest_common_prefix( FirstInput ),

	SecondInput = [ "abca", "xyz", "abca" ],
	{ "", SecondInput } = text_utils:get_longest_common_prefix( SecondInput ),

	{ "ab", [ "" ] } = text_utils:get_longest_common_prefix( [ "ab" ] ),

	{ "abc", [ "a", "b" ] } =
		text_utils:get_longest_common_prefix( [ "abca", "abcb" ] ),

	{ "abc", [ "", "b" ] } =
		text_utils:get_longest_common_prefix( [ "abc", "abcb" ] ),


	IndentationLevel = 3,
	NumberedString = text_utils:strings_to_enumerated_string( CompareStrings,
														  IndentationLevel ),

	test_facilities:display( "Numbered list with indentation level ~B: ~ts",
							 [ IndentationLevel, NumberedString ] ),


	test_facilities:display( "Testing the textual conversion of distances:" ),

	% In millimeters:
	Distances = [ -1001.5, -1001.0, -1000.5, -1000.0, -999.5, -999.0,
				  -1001, -1000, -999, -1.6, -1.4, -1.0, -0.9, -1, 0,
				  1, 0.9, 2, 999, 1000, 1001, 999999, 1000000, 1000001 ],

	[ test_facilities:display( " - an integer distance of ~w millimeters "
		"is ~ts, and roughly ~ts",
		[ D, text_utils:distance_to_string( D ),
		  text_utils:distance_to_short_string( D ) ] ) || D <- Distances ],


	test_facilities:display( "Testing the textual conversion of durations:" ),

	% In milliseconds:

	Durations = [ -100000, -1000, -1, 0 , 1, 2, 10, 3000, 3599,
				  3600, 3601, 36000, 59000, 60000, 61000, 100000,
				  12345678, 1234567890123 ],

	[ test_facilities:display(
		" - an integer duration of ~w milliseconds is ~ts",
		[ D, time_utils:duration_to_string( D ) ] ) || D <- Durations ],


	test_facilities:display( "Testing the upper-casing of first letter:" ),

	[ test_facilities:display( " - '~ts' becomes '~ts'",
				[ T, text_utils:uppercase_initial_letter( T ) ] )
	  || T <- [ [], "a", "A", "Hello", "hello" ] ],

	WesternText = "I am a lonesome cowboy",

	UUIDText = "93171810-95a0-4382-ad73",

	LongerText =
		"I am a lonesome cowboy whose name is 93171810-95a0-4382-ad73" =
		text_utils:join( _Sep=" ", [ WesternText, "whose name is", UUIDText ] ),


	[ "93171810", "95a0", "4382", "ad73" ] =
		text_utils:split( UUIDText, _OtherSep="-" ),

	TestSplit = "  abcxdefxgh ",

	{ "  abc", "defxgh " } = text_utils:split_at_first( $x, TestSplit ),

	none_found = text_utils:split_at_first( $y, TestSplit ),

	{ "  ", "bcxdefxgh " } = text_utils:split_at_first( $a, TestSplit ),

	{ "", " abcxdefxgh " } = text_utils:split_at_first( $ , TestSplit ),

	"Helli wirld" = text_utils:substitute( $o, $i, "Hello world" ),

	"bar is baz." = text_utils:split_after_prefix( "Foo", "Foobar is baz." ),

	no_prefix = text_utils:split_after_prefix( "ABC", "Foobar is baz." ),

	"I am a lonesome cowboy" =
		text_utils:ellipse( WesternText, _FirstMaxLen=22 ),

	"I am a lonesome cowboy [...]" =
		text_utils:ellipse( LongerText, _SecondMaxLen=28 ),



	EscapeString = "I *am* to be \"escaped\", as 'I shall be escaped'",

	test_facilities:display( "Single-quote escaping '~ts' results in: '~ts'.",
		[ EscapeString, text_utils:escape_single_quotes( EscapeString ) ] ),

	test_facilities:display( "Double-quote escaping '~ts' results in: '~ts'.",
		[ EscapeString, text_utils:escape_double_quotes( EscapeString ) ] ),

	test_facilities:display( "All-quote escaping '~ts' results in: '~ts'.",
		[ EscapeString, text_utils:escape_all_quotes( EscapeString ) ] ),


	% Note that an additional layer of obfuscation comes from that quoting
	% characters may themselves have to be escaped in literal strings like here:

	% Actual string taken into account is thus:
	% @I *am* to be "escaped", as \'I shall be escaped as well.@
	%
	StringToParse =
		"I *am* to be \"escaped\", as \\'I shall be escaped as well.",

	% In the character stream, we just want that the series of characters
	% corresponding to @escaped@ is replaced by a single (non-char) element,
	% which is a list of chars, i.e. "escaped", i.e. the input plain list is to
	% become a specific iolist, precisely whose elements are all characters
	% excepted one that is a list of chars.

	ParsedString = text_utils:parse_quoted( StringToParse ),

	test_facilities:display( "Parsing '~ts' with defaults results in: '~ts'.~n",
							 [ StringToParse, ParsedString ] ),

	% Verbatim : [ $I, $\, $*, $a, ..., $b, $e, $\ , [ $e, $s, $c, ..., $d ], $,
	% $\ , $a, $s, ..., $. ].

	Expected = "I *am* to be " ++ [ "escaped" ]
		++ ", as \\'I shall be escaped as well.",

	test_facilities:display( "Read    : @~ts@", [ StringToParse ] ),
	test_facilities:display( "Expected: @~ts@", [ Expected ] ),
	test_facilities:display( "Got     : @~ts@~n", [ ParsedString ] ),

	test_facilities:display( "Read    : @~w@", [ StringToParse ] ),
	test_facilities:display( "Expected: @~w@", [ Expected ] ),
	test_facilities:display( "Got     : @~w@~n", [ ParsedString ] ),

	Expected = ParsedString,


	RemovalCount = 3,

	"I am a lonesome cow" =
		text_utils:remove_last_characters( WesternText, RemovalCount ),

	false = text_utils:are_binaries( [ "Foo", "Bar" ] ),

	true = text_utils:are_binaries( [ <<"Foo">>, <<"Bar">> ] ),

	TestText = "This is a longer text, used notably to test how it could be "
		"formatted as a comment. Word-wrapping and comment prefix shall be "
		"correct hopefully, and the whole shall spread over three lines.",

	TextAsComment = text_utils:format_as_comment( TestText ),

	test_facilities:display( "Displaying test text as comment:~n~ts",
							 [ TextAsComment ] ),

	TwoElemSeq = "aa ~w bb~n ~w cc",

	test_facilities:display(
	  "Testing the detection of faulty control sequences." ),

	%FirstValues = [ first ],
	FirstValues = [ first, second ],

	test_facilities:display( "Feeding sequence '~p' with ~p: '~ts'.",
		[ TwoElemSeq, FirstValues,
		  text_utils:format( TwoElemSeq, FirstValues ) ] ),

	test_facilities:stop().
