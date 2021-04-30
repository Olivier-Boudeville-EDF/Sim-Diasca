% Copyright (C) 2018-2021 Olivier Boudeville
%
% Transferred from generate-password.escript to benefit from a more
% user-friendly debugging.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Released as LGPL software.
%
-module(password_generation).


-define( exec_name, "generate-password.escript" ).


-export([ run/0, main/1 ]).


-type alphabet() :: [ char() ].

-type password() :: text_utils:ustring().

-export([ generate_password/2 ]).


% Typically for testing:
-spec run() -> void().
run() ->
	ArgTable = shell_utils:get_argument_table(),
	main( ArgTable ).


% Defaults:

-define( default_min_length, "15" ).
-define( default_max_length, "20" ).

-define( default_alphabet, "extended" ).


-spec get_usage() -> void().
get_usage() ->
	text_utils:format( "Usage: ~ts "
	%text_utils:format( "Usage: ~ts [-i|--interactive] "
		"[-a ALPHABET|--alphabet ALPHABET] "
		"[-l MIN_LEN MAX_LEN|--length MIN_LEN MAX_LEN] "
		"[-h|--help]~n"
		"  Generates a suitable password, where:~n"
		"    - ALPHABET designates the set of characters to draw from "
		"(default one being '~ts'), among:~n"
		"       * 'base': alphanumeric letters, all cases [A-Za-z0-9]~n"
		"       * 'extended': 'base' + basic punctuation (i.e. '~ts')~n"
		"       * 'full': 'base' + all punctuation "
		"(i.e. basic + '~ts')~n"
		"    - MIN_LEN and MAX_LEN are the respective minimum and maximum "
		"numbers of characters (bounds included) used to "
		"generate this password [default: between ~ts and ~ts]~n",
		[ ?exec_name, ?default_alphabet, get_alphabet( basic_punctuation ),
		  get_alphabet( extra_punctuation ), ?default_min_length,
		  ?default_max_length ] ).



% Sole entry point for this generation service, either triggered by run/0 or by
% the associated escript.
%
-spec main( shell_utils:argument_table() ) -> void().
main( ArgTable ) ->

	%trace_utils:debug_fmt( "Original script-specific arguments: ~ts",
	%	[ shell_utils:argument_table_to_string( ArgTable ) ] ),

	[ %InteractiveRefKey,
	  LengthRefKey, AlphaRefKey, HelpRefKey ] =
		[ %'-interactive',
		  '-length', '-alphabet', '-help' ],

	% Standardises command-line options:
	MergedTable = list_table:merge_in_keys( [
			%{ InteractiveRefKey, [ 'i' ] },
			{ LengthRefKey, [ 'l' ] },
			{ AlphaRefKey, [ 'a' ] },
			{ HelpRefKey, [ 'h' ] } ], ArgTable ),

	%trace_utils:debug_fmt( "Canonicalized script-specific arguments: ~ts",
	%	   [ shell_utils:argument_table_to_string( MergedTable ) ] ),

	case list_table:has_entry( HelpRefKey, MergedTable ) of

		true ->
			display_usage();

		false ->
			ok

	end,

	%{ IsInteractive, InterTable } =
	%  case list_table:extract_entry_with_defaults( InteractiveRefKey,
	%    _DefaultInter=false, MergedTable ) of
	%
	%	{ [], ShrunkTable } ->
	%		{ true, ShrunkTable };
	%
	%	P={ false, _ShrunkTable } ->
	%		P
	%
	%end,

	%trace_utils:debug_fmt( "Interactive: ~ts", [ IsInteractive ] ),

	{ [ LengthStrings ], LenTable } =
		list_table:extract_entry_with_defaults(
		  LengthRefKey,
		  _LenDefault=[ [ ?default_min_length, ?default_max_length ] ],
		  %InterTable ),
		  MergedTable ),

	{ MinLengthString, MaxLengthString } = case LengthStrings of

		[ Min, Max ] ->
			{ Min, Max };

		Other ->
			trace_utils:error( "Error, a minimum and maximum lengths must "
							   "be specified." ),
			throw( { invalid_length_specification, Other } )

	end,

	MinLength = text_utils:string_to_integer( MinLengthString ),
	MaxLength = text_utils:string_to_integer( MaxLengthString ),

	case MinLength > MaxLength of

		true ->
			throw( { invalid_length_order, MinLength, MaxLength } );

		false ->
			ok

	end,

	case MinLength > 0 of

		true ->
			ok;

		false ->
			throw( { invalid_minimum_length, MinLength } )

	end,

	%trace_utils:debug_fmt( "Min length: ~B", [ MinLength ] ),
	%trace_utils:debug_fmt( "Max length: ~B", [ MaxLength ] ),

	{ [ [ AlphabetStringSpec ] ], AlphaTable } =
		list_table:extract_entry_with_defaults( AlphaRefKey,
						_AlphaDefault=[ [ ?default_alphabet ] ], LenTable ),

	AlphabetSpec = text_utils:string_to_atom( AlphabetStringSpec ),

	%trace_utils:debug_fmt( "Alphabet spec: ~ts", [ AlphabetSpec ] ),

	[ Length ] =
		random_utils:get_random_values( MinLength, MaxLength, _Count=1 ),

	case list_table:keys( AlphaTable ) of

		[] ->
			ok;

		UnexpectedOpts ->
			trace_utils:error_fmt( "Unexpected user input: ~ts~n~ts",
			  [ shell_utils:argument_table_to_string( AlphaTable ),
				get_usage() ] ),
			throw( { unexpected_command_line_options, UnexpectedOpts } )

	end,

	Alphabet = get_alphabet( AlphabetSpec ),

	%trace_utils:debug_fmt( "Input alphabet corresponding to spec ~p: "
	%					   "'~w' (i.e. '~ts').",
	%					   [ AlphabetSpec, Alphabet, Alphabet ] ),

	Password = generate_password( Alphabet, Length ),

	io:format( "Generated password (alphabet spec: ~ts, length: ~B) is:    "
			   "~ts    ~n", [ AlphabetSpec, Length, Password ] ),

	basic_utils:stop( _ErrorCode=0 ).



% Displays the usage of this service, and stops (with no error).
display_usage() ->
	io:format( get_usage(), [] ),
	basic_utils:stop( _ErrorCode=0 ).



% Returns the corresponding alphabet, based on its spec, expressed as an atom
% (ex: 'numeric' for all numeric literals) or as a list thereof.
%
get_alphabet( AlphabetSpecs ) when is_list( AlphabetSpecs ) ->
	list_utils:flatten_once( [ get_alphabet( A ) || A <- AlphabetSpecs ] );

get_alphabet( _AlphabetSpec=base ) ->
	get_alphabet( [ lower_case, upper_case, numeric ] );

get_alphabet( _AlphabetSpec=extended ) ->
	get_alphabet( [ lower_case, upper_case, numeric, basic_punctuation ] );

get_alphabet( _AlphabetSpec=full ) ->
	get_alphabet( [ lower_case, upper_case, numeric, basic_punctuation,
					extra_punctuation ] );

get_alphabet( _AlphabetSpec=lower_case ) ->
	lists:seq( $a, $z );

get_alphabet( _AlphabetSpec=upper_case ) ->
	lists:seq( $A, $Z );

get_alphabet( _AlphabetSpec=numeric ) ->
	lists:seq( $0, $9 );

get_alphabet( _AlphabetSpec=basic_punctuation ) ->
	[ $[, $], $(, $), ${, $}, $:, $,, $;, $-, $_, $., $!, $? ];

get_alphabet( _AlphabetSpec=extra_punctuation ) ->
	[ $", $', $@, $ , $/, $&, $$, $*, $\\, $^, $%, $=, $+, $| ].



% Generates a password of specified exact length, from specified alphabet.
-spec generate_password( alphabet(), basic_utils:count() ) -> password().
generate_password( Alphabet, CharCount ) ->

	% Of course we do not want a reproducible seeding:
	random_utils:start_random_source( time_based_seed ),

	AlphaSize = length( Alphabet ),

	%trace_utils:debug_fmt( "Alphabet size: ~B", [ AlphaSize ] ),

	generate_helper( CharCount, Alphabet, AlphaSize, _Acc=[] ).



% (helper)
generate_helper( _CharCount=0, _Alphabet, _AlphaSize, Acc ) ->
	% No order matters, no reverse useful:
	Acc;

generate_helper( CharCount, Alphabet, AlphaSize, Acc ) ->
	NewCharIndex = random_utils:get_random_value( AlphaSize ),
	NewChar = list_utils:get_element_at( Alphabet, NewCharIndex ),
	%trace_utils:debug_fmt( "Drawn '~B' (~ts), at index #~B",
	%					   [ NewChar, [ NewChar ], NewCharIndex ] ),

	generate_helper( CharCount-1, Alphabet, AlphaSize, [ NewChar | Acc ] ).
