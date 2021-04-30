% Copyright (C) 2015-2021 Olivier Boudeville
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


% Unit tests for the JSON services.
%
% For a more proper testing, each JSON backend shall be tested separately.
%
% See the json_utils.erl tested module.
%
% If running directly with the makefile system (i.e. not from an OTP/rebar3
% context), see, in GNUmakevars.inc, the USE_{JSON,JSX,JIFFY} variables to
% enable/disable JSON support and/or backend ones.
%
-module(json_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



% Returns a term-to-encode and its expected JSON-encoded form.
-spec get_encoding_sample() -> { term(), json_utils:bin_json() }.
get_encoding_sample() ->
	get_encoding_sample( as_map ).


-spec get_encoding_sample( 'as_list' | 'as_map' ) ->
								 { term(), json_utils:bin_json() }.
get_encoding_sample( as_list ) ->

	TermToEncode = { [ { foo, [ <<"bing">>, 2.3, true ] } ] },
	JsonEncoded =  <<"{\"foo\":[\"bing\",2.3,true]}">>,

	{ TermToEncode, JsonEncoded };

get_encoding_sample( as_map ) ->

	TermToEncode = #{ <<"library">> => <<"hello">>, <<"awesome">> => true },
	JsonEncoded = <<"{\"awesome\":true,\"library\":\"hello\"}">>,

	{ TermToEncode, JsonEncoded }.



% Returns the local test JSON file:
-spec get_test_file_path() -> file_utils:file_path().
get_test_file_path() ->
	"example.json".



% Returns a term decoded from JSON test file, if a parser is available.
-spec run_stateless_testing() -> maybe( json_utils:decoded_json() ).
run_stateless_testing() ->

	BackendName = json_utils:get_parser_backend_name(),

	test_facilities:display( "Parser backend name (if any): '~ts'.",
							 [ BackendName ] ),

	case BackendName of

		undefined ->
			test_facilities:display( "No JSON parser backend found, "
									 "not testing further stateless support." ),
			undefined;

		_ ->
			test_facilities:display( "Available parser backend name: '~ts'.",
				[ json_utils:get_available_parser_backend_name() ] ),

			json_utils:start_parser(),

			json_utils:check_parser_operational(),

			{ TermToEncode, ExpectedJsonEncoded } = get_encoding_sample(),

			case json_utils:to_json( TermToEncode ) of

				ExpectedJsonEncoded ->
					ok;

				OtherJsonEncoded ->
					throw( { unexpected_encoding_of, TermToEncode,
							 { expected, ExpectedJsonEncoded },
							 { got, OtherJsonEncoded } } )

			end,

			case json_utils:from_json( ExpectedJsonEncoded ) of

				TermToEncode ->
					ok;

				OtherTerm ->
					throw( { unexpected_decoding_of, ExpectedJsonEncoded,
							 { expected, TermToEncode }, { got, OtherTerm } } )


			end,

			TestFilePath = get_test_file_path(),

			test_facilities:display( "Reading test file '~ts'.",
									 [ TestFilePath ] ),

			ReadTestTerm = json_utils:from_json_file( TestFilePath ),

			Type = type_utils:get_type_of( ReadTestTerm ),

			test_facilities:display(
			  "Test file read, type of corresponding term is: '~ts'.",
			  [ Type ] ),

			test_facilities:display( "The read term is:~n ~p",
									 [ ReadTestTerm ] ),

			test_facilities:display( "Interpreted type: '~ts'.",
				[ type_utils:interpret_type_of( ReadTestTerm,
												_Level=infinite ) ] ),

			ReadTestTerm

	end,

	json_utils:stop_parser().



% Returns a term decoded from JSON test file.
-spec run_stateful_testing( json_utils:parser_state() ) ->
								  json_utils:decoded_json().
run_stateful_testing( ParserState ) ->

	BackendName = json_utils:get_parser_backend_name( ParserState ),

	test_facilities:display( "Parser backend name: '~ts'.", [ BackendName ] ),

	JsonDecodedTerm = case BackendName of

		undefined ->
			throw( stateful_parser_lacking );

		_ ->

			json_utils:check_parser_operational( ParserState ),

			{ TermToEncode, ExpectedJsonEncoded } = get_encoding_sample(),

			case json_utils:to_json( TermToEncode, ParserState ) of

				ExpectedJsonEncoded ->
					ok;

				OtherJsonEncoded ->

					% Too strong, encoding may still depend on the parser:
					%
					%throw( { unexpected_encoding_of, TermToEncode,
					%	{ expected, ExpectedJsonEncoded },
					%   { got, OtherJsonEncoded } } )

					test_facilities:display_fmt( "Note that the encoding of ~p "
						"with '~ts' is different from the one obtained with "
						"the default parser (~ts): former one returned ~p, "
						"latter one ~p.",
						[ TermToEncode, BackendName,
						  json_utils:get_parser_backend_name(),
						  OtherJsonEncoded, ExpectedJsonEncoded ] )

			end,

			% Note that, if the actual JSON encoding of a given Erlang term
			% depends on the parser backend (ex: the order of JSON keys might
			% differ), for each parser, for each valid Erlang term T, we expect
			% that from_json( to_json( T ) ) = T:
			%
			case json_utils:from_json( ExpectedJsonEncoded, ParserState ) of

				TermToEncode ->
					ok;

				OtherTerm ->
					throw( { unexpected_decoding_of, ExpectedJsonEncoded,
						{ expected, TermToEncode }, { got, OtherTerm } } )


			end,

			TestFilePath = get_test_file_path(),

			test_facilities:display( "Reading test file '~ts'.",
									 [ TestFilePath ] ),

			ReadTestTerm =
				json_utils:from_json_file( TestFilePath, ParserState ),

			Type = type_utils:get_type_of( ReadTestTerm ),

			test_facilities:display(
			  "Test file read, type of corresponding term is: '~ts'.",
			  [ Type ] ),

			test_facilities:display( "The read term is:~n ~p",
									 [ ReadTestTerm ] ),

			test_facilities:display( "Interpreted type: '~ts'.",
				[ type_utils:interpret_type_of( ReadTestTerm,
												_Level=infinite ) ] ),

			ReadTestTerm

	end,

	json_utils:stop_parser( ParserState ),

	JsonDecodedTerm.



% Compares the decoding done by specified parser with the expected decoded term.
-spec compare_with_if_available( json_utils:decoded_json(),
					json_utils:parser_backend_name() ) -> void().
compare_with_if_available( JsonDecodedTerm, BackendName ) ->

	case json_utils:is_parser_backend_available( BackendName ) of

			false ->
				test_facilities:display_fmt( "No comparison done with backend "
					"'~ts' (not found available).", [ BackendName ] ),
				ok;

			_ ->
			BackendState = json_utils:start_parser( BackendName ),

			BackendJsonDecodedTerm = run_stateful_testing( BackendState ),

			% By design none of the decoded terms is undefined:
			case BackendJsonDecodedTerm of

				JsonDecodedTerm ->
					% Possibly comparing a backend to itself, if is the default
					% one:
					%
					test_facilities:display_fmt( "Comparison success with "
						"backend '~ts' (not found available).",
						[ BackendName ] ),

					ok;

				_ ->
					% Might be a too strong property:
					throw( { non_matching_decoded_terms,
							 { default, JsonDecodedTerm },
							 { BackendName, BackendJsonDecodedTerm } } )

			end

	end.



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing JSON support." ),


	% First, stateless testing:
	run_stateless_testing(),


	% Second, stateful testing:

	MaybeAnyJsonDecodedTerm = case json_utils:get_parser_backend_name() of

		undefined ->
			test_facilities:display( "No JSON parser backend found, "
				"not testing further stateful support." ),
			ok;

		_ ->
			test_facilities:display( "Default available parser backend "
				"name: '~ts'.",
				[ json_utils:get_available_parser_backend_name() ] ),

			% Could have been named 'InitialState', yet is supposed const:
			AnyParserState = json_utils:start_parser(),
			run_stateful_testing( AnyParserState )

	end,

	compare_with_if_available( MaybeAnyJsonDecodedTerm, jsx ),
	compare_with_if_available( MaybeAnyJsonDecodedTerm, jiffy ),

	test_facilities:stop().
