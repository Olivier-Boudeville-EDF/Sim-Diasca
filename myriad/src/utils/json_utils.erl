% Copyright (C) 2020-2021 Olivier Boudeville
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
% Authors: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Creation date: Friday, February 21, 2020.


% Gathering of management facilities for JSON processing.
%
% See json_utils_test.erl for the corresponding test.
%
-module(json_utils).



% Implementation notes:
%
% We rely here on a JSON parser, namely by default JSX
% (https://github.com/talentdeficit/jsx/), version 3.0.0 at the time of this
% writing; we expect the BEAM files from JSX to be available on the code path
% (out of a rebar3 context, we typically expect to find them in
% ~/Software/jsx/jsx-current-install).
%
% Jiffy (https://github.com/davisp/jiffy) is the second supported backend
% option (with no specific action needed to be able to use it).
%
% Indeed, as no static linking is performed, the parser selection can happen at
% runtime rather than at compilation-time, reducing the need for preprocessor
% directives and early configuration choices.
%
% The parser state (typically returned first by start_parser/0) may or may not
% be used by the caller; its interest is to allow for a slightly more efficient
% mode of operation at runtime. Not using such a state also implies that the
% backend is stateless; we also consider that this state is const (ex: like a
% PID or any reference), in the sense that a JSON operation is not supposed to
% impact it (otherwise each would have to return a new state).
%
% As a result, the current module is not cluttered by (rigid) preprocessor
% directives, but the user may have to pass along a parser state. Another option
% could be to use the process dictionary to store such a state.

% Note that:
%
% - the actual JSON encoding of a given Erlang term depends on the parser
% backend (ex: the order of JSON keys might differ)

% - for each parser, we expect that from_json . to_json = Id, i.e. for each
% valid Erlang term T, from_json( to_json( T ) ) = T


% Curently no extra (transverse) user-specified encoding/decoding options are
% supported.


% The typical type of (Erlang) terms to be encoded in JSON is a map whose keys
% are binary strings (we would have preferred atoms, which is supported by JSX
% through its {labels, atom} option - yet Jiffy does not support it).

% Comments are not supported in JSON; for them we rely on (non-duplicated)
% "_comment" entries.

% As the JSX mapping hardcodes the 'null' atom for the JSON null value, we
% enforce the same setting with Jiffy (that can set it).



-export([ % Stateless versions:

		  start_parser/0, stop_parser/0,

		  get_parser_backend_name/0,
		  get_available_parser_backend_name/0,

		  check_parser_operational/0,

		  to_json/1,

		  from_json/1, from_json_file/1,


		  % Stateful versions (preferred):

		  start_parser/1, stop_parser/1,
		  get_parser_backend_name/1,
		  % No get_available_parser_backend_name/0: available by design here.

		  check_parser_operational/1,

		  to_json/2,
		  from_json/2, from_json_file/2,


		  % General services:

		  is_parser_available/0, is_parser_available/1,
		  is_parser_backend_available/1,

		  get_base_json_encoding_options/1,
		  get_base_json_decoding_options/1 ]).



% Module-local inlining:
-compile( { inline, [ get_base_json_encoding_options/1,
					  get_base_json_decoding_options/1 ] } ).


% The known, and potentially supported, backends in terms of JSON parsers:
-type parser_backend_name() :: 'jsx' | 'jiffy' | otp_utils:application_name().


% Often no internal state is really needed:
-type parser_state() :: { parser_backend_name(),
						  InternalBackendState :: maybe( term() ) }.


-type string_json() :: ustring().

-type bin_json() :: text_utils:bin_string().


% JSON document:
-type json() :: bin_json() | string_json().


% A term obtained from a decoded JSON document:
-type decoded_json() :: term().


% Options for the JSON encoding:
%
% (they shall be usable transparently with all supported backends)
%
-type json_encoding_option() :: any().


% Options for the JSON parsing (decoding):
%
% (they shall be usable transparently with all supported backends)
%
-type json_decoding_option() :: any().


-export_type([ parser_backend_name/0, parser_state/0,

			   string_json/0, bin_json/0, json/0,
			   decoded_json/0,

			   json_encoding_option/0, json_decoding_option/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type any_file_path() :: file_utils:any_file_path().
-type directory_path() :: file_utils:directory_path().




% Starts the JSON parser found by default (if any), and returns its initial
% state, which optionally may be used afterwards.
%
-spec start_parser() -> parser_state().
start_parser() ->

	ParserName = get_available_parser_backend_name(),

	%trace_utils:info_fmt( "Selected JSON parser: '~ts'.", [ ParserName ] ),

	start_parser( ParserName ).



% Starts the specified JSON parser, returns its initial state, which may be used
% optionally afterwards.
%
-spec start_parser( parser_backend_name() ) -> parser_state().
start_parser( BackendName ) when BackendName =:= jsx
								 orelse BackendName =:= jiffy ->

	% Appropriate for both JSX and Jiffy:

	% No specific initialisation needed.

	% No particular backend state needed here:
	InitialState = { BackendName, undefined },

	check_parser_operational( InitialState ).




% Returns (as an atom) the JSON parser (as an OTP application name) that would
% currently be used, if any (returns 'undefined' if none was found available).
%
% So this function is also a way of testing whether JSON support is available at
% all.
%
-spec get_parser_backend_name() -> maybe( parser_backend_name() ).
get_parser_backend_name() ->

	% Useful to detect repeated initializations that may be unwanted (then rely
	% on the stateful mode of operation):
	%
	%trace_utils:info( "Determining the JSON backend to use." ),

	% We prioritize JSX over Jiffy:
	case is_parser_backend_available( jsx ) of

		 false->
				case is_parser_backend_available( jiffy ) of

					false ->
						undefined;

					[ _JiffyPath ] ->
						%trace_utils:debug_fmt( "Selected JSON parser is "
						%	"Jiffy, in '~ts'.", [ JiffyPath ] ),
						jiffy ;

					JiffyPaths ->
						throw( { multiple_jiffy_json_backends_found,
								 JiffyPaths } )

				end;

		[ _JsxPath ] ->
			%trace_utils:debug_fmt( "Selected JSON parser is JSX, in '~ts'.",
			%					   [ JsxPath ] ),
			jsx ;

		JsxPaths ->
			throw( { multiple_jsx_json_backends_found, JsxPaths } )

	end.



% Tells whether a suitable JSON parser is available.
-spec is_parser_available() -> boolean().
is_parser_available() ->

	case get_parser_backend_name() of

		undefined ->
			false;

		_ ->
			true

	end.



% Tells whether a suitable JSON parser is available, based on the specified
% (maybe) parser state.
%
-spec is_parser_available( maybe( parser_state() ) ) -> boolean().
is_parser_available( undefined ) ->
	false;

% A bit of implicit checking:
is_parser_available( { _ParserBackendName, _MaybeInternalState } ) ->
	true.



% Returns whether specified parser backend is available.
%
% Useful for testing for example.
%
-spec is_parser_backend_available( parser_backend_name() ) ->
										'false' | [ directory_path() ].
is_parser_backend_available( BackendName ) ->

	case code_utils:is_beam_in_path( BackendName ) of

		not_found ->
			false;

		Paths ->
			Paths

	end.



% Returns (as an atom) the JSON parser (as an OTP application name) that
% corresponds to specified parser state.
%
-spec get_parser_backend_name( parser_state() ) -> parser_backend_name().
get_parser_backend_name(
  _ParserState={ BackendName, _InternalBackendState } ) ->
	BackendName.



% Returns the name of the JSON parser found by default and available (if any;
% otherwise throws an exception).
%
-spec get_available_parser_backend_name() -> parser_backend_name().
get_available_parser_backend_name() ->

	% Auto-selects based on backend availability and order:
	case get_parser_backend_name() of

		undefined ->
			trace_utils:error( "No JSON parser found available "
				"(neither JSX nor Jiffy). "
				++ system_utils:get_json_unavailability_hint() ),
			throw( no_json_parser_backend_found );

		ParserName ->
			%trace_utils:info_fmt( "Selected JSON parser: ~ts.",
			%					   ParserName ] ),
			ParserName

	end.



% Checks whether the JSON parser found by default (if any) is operational;
% throws an exception if not.
%
-spec check_parser_operational() -> void().
check_parser_operational() ->

	ParserState = get_parser_backend_state(),

	check_parser_operational( ParserState ).



% Checks whether the specified JSON parser is operational; returns an updated
% state if yes, otherwise throws an exception.
%
-spec check_parser_operational( parser_state() ) -> parser_state().
check_parser_operational( ParserState={ jsx, _InternalBackendState } ) ->

	% This is a way to check that its BEAMs are available and fully usable:
	try jsx:is_json( <<"\"test\"">> ) of

		true ->
			% Const:
			ParserState

	catch

		error:undef ->
			trace_utils:error_fmt(
			  "The JSX JSON parser is not operational.~n~ts",
			  [ system_utils:get_json_unavailability_hint( jsx ) ] ),
			throw( { json_parser_not_operational, jsx } );

		OtherError ->
			trace_utils:error_fmt(
			  "The JSX JSON parser does not work properly: ~p.",
			  [ OtherError ] ),
			throw( { json_parser_dysfunctional, jsx, OtherError } )

	end;

check_parser_operational( ParserState={ jiffy, _InternalBackendState } ) ->

	% This is a way to check that its BEAMs are available and fully usable:
	try jiffy:decode( <<"{\"foo\": \"bar\"}">> ) of

		{ [ { <<"foo">>, <<"bar">> } ] } ->
			% Const:
			ParserState

	catch

		error:undef ->
			trace_utils:error_fmt(
			  "The Jiffy JSON parser is not operational.~n~ts",
			  [ system_utils:get_json_unavailability_hint( jiffy ) ] ),
			throw( { json_parser_not_operational, jiffy } );

		OtherError ->
			trace_utils:error_fmt(
			  "The Jiffy JSON parser does not work properly: ~p.",
			  [ OtherError ] ),
			throw( { json_parser_dysfunctional, jiffy, OtherError } )

	end.




% Encoding section.


% Converts (encodes) specified Erlang term into a JSON counterpart element,
% using the looked-up default JSON backend for that.
%
% Ex: json_utils:to_json( #{ <<"protected">> => Protected,
%							 <<"payload">> => Payload,
%							 <<"signature">> => EncSigned } ).
%
-spec to_json( term() ) -> json().
to_json( Term ) ->

	% The call that would be spared if using an explicit parser state:
	ParserState = get_parser_backend_state(),

	to_json( Term, ParserState ).



% Converts (encodes) specified Erlang term into a JSON counterpart element,
% using directly the JSON backend designated by the specified parser state.
%
% Ex: json_utils:to_json( #{ <<"protected">> => Protected,
%							 <<"payload">> => Payload,
%							 <<"signature">> => EncSigned }, _ParserName=jsx ).
%
-spec to_json( term(), parser_state() ) -> json().
to_json( Term, _ParserState={ jsx, _UndefinedInternalBackendState } ) ->

	Opts = get_base_json_encoding_options( jsx ),

	%trace_utils:debug_fmt( "JSX is to encode, with options ~p:~n ~p",
	%					   [ Opts, Term ] ),

	R = jsx:encode( Term, Opts ),

	%trace_utils:debug_fmt( "JSX returned encoded term:~n ~p", [ R ] ),

	R;

to_json( Term, _ParserState={ jiffy, _UndefinedInternalBackendState } ) ->

	Opts = get_base_json_encoding_options( jiffy ),

	%trace_utils:debug_fmt( "Jiffy is to encode, with options ~p:~n ~p",
	%					   [ Opts, Term ] ),

	jiffy:encode( Term, Opts ).



% Returns the default options for the JSON encoding.
-spec get_base_json_encoding_options( parser_backend_name() ) ->
												[ json_encoding_option() ].
get_base_json_encoding_options( _BackendName=jsx ) ->
	[];

get_base_json_encoding_options( _BackendName=jiffy ) ->

	% Jiffy only understands UTF-8 in binaries; force strings to encode as UTF-8
	% by fixing broken surrogate pairs and/or using the replacement character to
	% remove broken UTF-8 sequences in data:
	%
	% We do not specify here 'use_nil' as we want to use 'null' as JSX does.
	%
	[ force_utf8 ].





% Decoding section.


% Converts (decodes) specified JSON element into an Erlang term counterpart,
% recursively so that it returns a table containing tables, themselves
% containing potentially tables, etc.
%
% Note that if in a given scope a key is present more than once, only one of its
% values will be retained (actually the lastly defined one).
%
-spec from_json( json() ) -> term().
from_json( Json ) ->

	ParserState = get_parser_backend_state(),

	from_json( Json, ParserState ).



% Converts (decodes) specified JSON element into an Erlang term counterpart,
% recursively so that it returns a table containing tables, themselves
% containing potentially tables, etc.
%
% Note that if in a given scope a key is present more than once, only one of its
% values will be retained (actually the lastly defined one).
%
-spec from_json( json(), parser_state() ) -> term().
from_json( Json, _ParserState={ jsx, _UndefinedInternalBackendState } ) ->

	BinJson = case is_binary( Json ) of

		true ->
			Json;

		% Supposedly then a plain string:
		false ->
			text_utils:string_to_binary( Json )

	end,

	%trace_utils:debug_fmt( "Decoding '~p' with JSX.", [ BinJson ] ),

	% Note that at least some errors in the JSON file (ex: missing comma) will
	% lead only to an exception such as:
	%
	% ** exception error: bad argument
	%  in function jsx_decoder:maybe_done/4
	%
	% (not even returning a line number for the faulty JSON part...)

	jsx:decode( BinJson, get_base_json_decoding_options( jsx ) );


from_json( Json, _ParserState={ jiffy, _UndefinedInternalBackendState } ) ->
	%trace_utils:debug_fmt( "Decoding '~p' with Jiffy.", [ Json ] ),
	jiffy:decode( Json, get_base_json_decoding_options( jiffy ) ).



% Returns the default options for the JSON decoding.
-spec get_base_json_decoding_options( parser_backend_name() ) ->
												[ json_decoding_option() ].
get_base_json_decoding_options( _BackendName=jsx ) ->
	% We used to prefer {state,<<"PUBLISHED">>} to
	% {<<"state">>,<<"PUBLISHED">>}, yet for compatibility with jiffy we stick
	% to binaries now, so [ { labels, atom } ] is not used anymore.
	%
	% return_maps is default:
	[];

get_base_json_decoding_options( _BackendName=jiffy ) ->

	% dedupe_keys: if a key is repeated in a JSON object this flag will ensure
	% that the parsed object only contains a single entry containing the last
	% value seen.
	%
	[ return_maps, dedupe_keys ].






% Converts (decodes) specified JSON file recursively into an Erlang term
% counterpart, so that it returns a table containing tables, themselves
% containing potentially tables, etc., with specified parser state.
%
% Note that if in a given scope a key is present more than once, only one of its
% values will be retained (actually the lastly defined one).
%
-spec from_json_file( any_file_path() ) -> term().
from_json_file( JsonFilePath ) ->
	BinJson = file_utils:read_whole( JsonFilePath ),
	from_json( BinJson ).



% Converts (decodes) specified JSON file recursively into an Erlang term
% counterpart, so that it returns a table containing tables, themselves
% containing potentially tables, etc., with specified parser state.
%
% Note that if in a given scope a key is present more than once, only one of its
% values will be retained (actually the lastly defined one).
%
-spec from_json_file( any_file_path(), parser_state() ) -> term().
from_json_file( JsonFilePath, ParserState ) ->
	BinJson = file_utils:read_whole( JsonFilePath ),
	from_json( BinJson, ParserState ).



% Returns a (blank) parser state corresponding to the default parser.
%
% (helper)
%
-spec get_parser_backend_state() -> maybe( parser_state() ).
get_parser_backend_state() ->

	ParserName = get_available_parser_backend_name(),

	% Supposed stateless:
	{ ParserName, _InternalBackendState=undefined }.



% Stops the JSON parser.
-spec stop_parser() -> void().
stop_parser() ->
	ok.


% Stops the specified JSON parser.
-spec stop_parser(  parser_state() ) -> void().
stop_parser( _ParserState ) ->
	ok.
