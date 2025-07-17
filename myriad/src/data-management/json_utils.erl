% Copyright (C) 2020-2025 Olivier Boudeville
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
% Creation date: Friday, February 21, 2020.

-module(json_utils).

-moduledoc """
Gathering of management facilities for **JSON** processing.

See json_utils_test.erl for the corresponding test.

Refer to http://myriad.esperide.org/#json-use for more details.

Note that, since Erlang 27.0, a built-in JSON parser is available, see the
`json` module; we now rely on it by default (`jsx` or `jiffy` were used
beforehand, and remain possible choices).
""".


% Implementation notes:
%
% We rely here on a JSON parser, namely by default now the native Erlang one,
% built-in since Erlang 27.0, in the `json` module.
%
% Previously we used jsx (https://github.com/talentdeficit/jsx/), version 3.0.0
% at the time of this writing; the BEAM files from jsx were expected to be
% available on the code path (out of a rebar3 context, they may be expected to
% be found in ~/Software/jsx/jsx-current-install/ebin; in a rebar3 context, they
% were expected to be readily found in _checkouts or as a sibling
% dependency). See the 'USE_JSON' and 'USE_JSX' sections in GNUmakevars.inc for
% all possible locations.
%
% Refer to the 'jsx Installation' section in GNUmakevars.inc in order to perform
% an installation thereof according to our standards - which used to be strongly
% recommended, before the native 'json' module was used.
%
% Jiffy (https://github.com/davisp/jiffy) used to be the second supported
% backend option (with no specific action needed to be able to use it).
%
% Indeed, as no static linking is performed, the parser selection can happen at
% runtime rather than at compilation-time, reducing the need for preprocessor
% directives and early configuration choices.
%
% The parser state (typically returned first by start_parser/0) may or may not
% be used by the caller; its interest is to allow for a slightly more efficient
% mode of operation at runtime. Not using such a state also implies that the
% backend is stateless; we also consider that this state is const (e.g. like a
% PID or any reference), in the sense that a JSON operation is not supposed to
% impact it (otherwise each of them would have to return a new state).
%
% As a result, the current module is not cluttered by (rigid) preprocessor
% directives, but the user may have to pass along a parser state. Another option
% could be to use the process dictionary to store such a state.

% Note that:
%
% - the actual JSON encoding of a given Erlang term depends on the parser
% backend (e.g. the order of JSON keys might differ - note that for example the
% JSON RFC (RFC 4627) indicates that order of object members should not matter)
%
% - for each parser, we expect that from_json . to_json = Id, i.e. for each
% valid Erlang term T, from_json(to_json(T)) = T

% Curently no extra (transverse) user-specified encoding/decoding options are
% supported.


% The typical type of (Erlang) terms to be encoded in JSON is a map whose keys
% are binary strings (we would have preferred atoms, which is supported by jsx
% through its {labels, atom} option - yet Jiffy does not support it).

% Comments are not supported in JSON; for them we rely on (non-duplicated)
% "_comment" entries.

% As the jsx mapping hardcodes the 'null' atom for the JSON null value, we
% enforce the same setting with Jiffy (that can set it).

% The JSON generated may be either optimised for compactness (less whitespaces)
% or readibility, depending on the requested formatting.


-export([ get_parser_name_paths/0, get_paths_for/1,


		  % Stateless versions:

		  start_parser/0, stop_parser/0,

		  get_parser_backend_name/0,
		  get_available_parser_backend_name/0,

		  check_parser_operational/0,

		  to_json/1, to_json_file/2,

		  from_json/1, from_json_file/1,


		  % Stateful versions (preferred):

		  start_parser/1, stop_parser/1,
		  get_parser_backend_name/1,
		  % No get_available_parser_backend_name/0: available by design here.

		  check_parser_operational/1,

		  to_json/2, to_json_file/3, to_json_file/4,

		  from_json/2, from_json_file/2,


		  % General services:

		  is_parser_available/0, is_parser_available/1,
		  is_parser_backend_available/1,

		  get_base_json_encoding_options/1,
		  get_base_json_decoding_options/1,

		  get_json_unavailability_hint/1 ]).



% Module-local inlining:
-compile( { inline, [ get_base_json_encoding_options/1,
					  get_base_json_decoding_options/1 ] } ).


-doc "The known, and potentially supported, backends in terms of JSON parsers.".
-type parser_backend_name() :: 'json'  % Built-in (recommended now)
							 | 'jsx'   % Possible dependency
							 | 'jiffy' % Possible dependency
							 | otp_utils:application_name(). % Other



-doc "Often no internal state is really needed.".
-type parser_state() ::
		{ parser_backend_name(), InternalBackendState :: option( term() ) }.



-doc "A (plain) string containing JSON content.".
-type string_json() :: ustring().



-doc "A binary string containing JSON content.".
-type bin_json() :: bin_string().



-doc "A JSON document.".
-type json() :: bin_json() | string_json() | iolist().



-doc "A key in a decoded JSON table.".
-type decoded_json_key() :: bin_string().



-doc "A value in a decoded JSON table.".
-type decoded_json_value() :: decoded_json().



-doc "A decoded entry.".
-type decoded_json_pair() :: { decoded_json_key(), decoded_json_value() }.



-doc "A term that was decoded from JSON.".
-type decoded_json() :: json_term().



-doc """
An (Erlang) term corresponding to a JSON document (e.g. a decoded one, or one
not encoded yet), at least often a map whose keys are binary strings and whose
values are json_term() or basic types such as integers, floats, strings, etc.).
""".
-type json_term() ::
	map_hashtable:map_hashtable( decoded_json_key(), decoded_json_value() )
  | integer() | float() | binary() | atom() | term().



-doc """
Options for the JSON encoding (they shall be usable transparently with all
supported backends).
""".
-type json_encoding_option() :: any().



-doc """
Options for the JSON parsing, that is decoding (they shall be usable
transparently with all supported backends).
""".
-type json_decoding_option() :: any().



-export_type([ parser_backend_name/0, parser_state/0,

			   string_json/0, bin_json/0, json/0,

			   decoded_json_key/0, decoded_json_value/0, decoded_json_pair/0,
			   decoded_json/0, json_term/0,

			   json_encoding_option/0, json_decoding_option/0 ]).



% Type shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type file_path() :: file_utils:file_path().
-type any_file_path() :: file_utils:any_file_path().

-type directory_path() :: file_utils:directory_path().
-type resolvable_path() :: file_utils:resolvable_path().




-doc """
Returns information regarding any JSON parser found, as a triplet made of its
name, any resolvable path to any non-standard ebin directory (for example useful
to any upcoming deployment of a vanilla node) and any directly-resolved path;
otherwise throws an exception.
""".
-spec get_parser_name_paths() ->
	{ parser_backend_name(), option( resolvable_path() ),
	  option( directory_path() ) }.
get_parser_name_paths() ->

	% Pre-Erlang 27.0 versions do not have it:
	case code_utils:is_beam_in_path( json ) of

		not_found ->
			case get_paths_for( jsx ) of

				undefined ->
					case get_paths_for( jiffy ) of

						undefined ->
							throw( unresolvable_json_parser );

						{ JiffyRes, JiffyPlain } ->
							{ jiffy, JiffyRes, JiffyPlain }

					end;

				{ JsxRes, JsxPlain } ->
					{ jsx, JsxRes, JsxPlain }

			end;

		[ _JsonBeamPath | _ ] ->
			% Built-in, so need to update the code path (supposing homogeneous
			% Erlang versions):
			%
			{ json, undefined, undefined }

	end.



-doc """
Returns an existing path (if any, and according to the Myriad conventions), as
both a resolvable one and a directly resolved one, to the ebin directory of the
specified JSON parser.
""".
-spec get_paths_for( parser_backend_name() ) ->
						option( { resolvable_path(), directory_path() } ).
get_paths_for( _ParserName=jsx ) ->

	ResolvablePath = [ home, "Software", "jsx", "jsx-current-install", "_build",
					   "default", "lib", "jsx", "ebin" ],

	ResolvedPath = file_utils:resolve_path( ResolvablePath ),

	case file_utils:is_existing_directory_or_link( ResolvedPath ) of

		true ->
			{ ResolvablePath, ResolvedPath };

		false ->
			undefined

	end;

get_paths_for( _ParserName=jiffy ) ->

	% Maybe to be updated:
	ResolvablePath =
		[ home, "Software", "jiffy", "jiffy-current-install", "ebin" ],

	ResolvedPath = file_utils:resolve_path( ResolvablePath ),

	case file_utils:is_existing_directory_or_link( ResolvedPath ) of

		true ->
			{ ResolvablePath, ResolvedPath };

		false ->
			undefined

	end.



-doc """
Starts the JSON parser found by default (if any), and returns its initial state,
which optionally may be used afterwards.
""".
-spec start_parser() -> parser_state().
start_parser() ->

	ParserName = get_available_parser_backend_name(),

	cond_utils:if_defined( myriad_debug_json, trace_utils:info_fmt(
		"Selected JSON parser: '~ts'.", [ ParserName ] ) ),

	start_parser( ParserName ).



-doc """
Starts the specified JSON parser, returns its initial state, which may be used
optionally afterwards.
""".
-spec start_parser( parser_backend_name() ) -> parser_state().
start_parser( BackendName ) when BackendName =:= json orelse BackendName =:= jsx
								 orelse BackendName =:= jiffy ->

	% Appropriate for json, jsx and Jiffy:

	% No specific initialisation needed.

	% No particular backend state needed here:
	InitialState = { BackendName, undefined },

	check_parser_operational( InitialState ).



-doc """
Returns (as an atom) the JSON parser (as an OTP application name) that would
currently be used, if any (returns 'undefined' if none was found available).

So this function is also a way of testing whether JSON support is available at
all.
""".
-spec get_parser_backend_name() -> option( parser_backend_name() ).
get_parser_backend_name() ->

	% Useful to detect repeated initializations that may be unwanted (then rely
	% on the stateful mode of operation):
	%
	%trace_utils:info( "Determining the JSON backend to use." ),

	% We prioritise json over jsx over Jiffy:
	case is_parser_backend_available( json ) of

		false ->
			case is_parser_backend_available( jsx ) of

				false ->
					case is_parser_backend_available( jiffy ) of

						false ->
							undefined;

						[ JiffyPath ] ->
							cond_utils:if_defined( myriad_debug_json,
								trace_utils:debug_fmt(
									"Selected JSON parser is Jiffy, in '~ts'.",
									[ JiffyPath ] ),
								basic_utils:ignore_unused( JiffyPath ) ),

							jiffy ;


						JiffyPaths ->
							trace_utils:error_fmt( "Multiple Jiffy backends "
								"found (~ts), while ~ts",
								[ text_utils:strings_to_listed_string(
									JiffyPaths ),
								  code_utils:get_code_path_as_string() ] ),

							throw( { multiple_jiffy_json_backends_found,
									 JiffyPaths } )

					end;

				[ JsxPath ] ->
					cond_utils:if_defined( myriad_debug_json,
						trace_utils:debug_fmt( "Selected JSON parser is jsx, "
												"in '~ts'.", [ JsxPath ] ),
						basic_utils:ignore_unused( JsxPath ) ),

					jsx;


				JsxPaths ->
					trace_utils:error_fmt(
						"Multiple jsx backends found (~ts), while ~ts",
						[ text_utils:strings_to_listed_string( JsxPaths ),
						  code_utils:get_code_path_as_string() ] ),

					throw( { multiple_jsx_json_backends_found, JsxPaths } )

			end;


		[ JsonPath ] ->
			cond_utils:if_defined( myriad_debug_json, trace_utils:debug_fmt(
				"Selected JSON parser is (built-in) json, in '~ts'.",
				[ JsonPath ] ),
				basic_utils:ignore_unused( JsonPath ) ),

			json

	end.



-doc "Tells whether a suitable JSON parser is available.".
-spec is_parser_available() -> boolean().
is_parser_available() ->
	get_parser_backend_name() =/= undefined.



-doc """
Tells whether a suitable JSON parser is available, based on the specified
(maybe) parser state.
""".
-spec is_parser_available( option( parser_state() ) ) -> boolean().
is_parser_available( undefined ) ->
	false;

% A bit of implicit checking:
is_parser_available( { _ParserBackendName, _MaybeInternalState } ) ->
	true.



-doc """
Returns whether specified parser backend is available.

Useful for testing for example.
""".
-spec is_parser_backend_available( parser_backend_name() ) ->
										'false' | [ directory_path() ].
is_parser_backend_available( BackendName ) ->

	case code_utils:is_beam_in_path( BackendName ) of

		not_found ->
			false;

		Paths ->
			Paths

	end.



-doc """
Returns (as an atom) the JSON parser (as an OTP application name) that
corresponds to specified parser state.
""".
-spec get_parser_backend_name( parser_state() ) -> parser_backend_name().
get_parser_backend_name(
		_ParserState={ BackendName, _InternalBackendState } ) ->
	BackendName.



-doc """
Returns the name of the JSON parser found by default and available (if any;
otherwise throws an exception).
""".
-spec get_available_parser_backend_name() -> parser_backend_name().
get_available_parser_backend_name() ->

	% Auto-selects based on backend availability and order:
	case get_parser_backend_name() of

		undefined ->
			trace_utils:error( "No JSON parser found available "
				"(no json, jsx or jiffy). "
				++ system_utils:get_json_unavailability_hint() ),
			throw( no_json_parser_backend_found );

		ParserName ->
			%trace_utils:info_fmt( "Selected JSON parser: ~ts.",
			%                      [ ParserName ] ),
			ParserName

	end.



-doc """
Checks whether the JSON parser found by default (if any) is operational; throws
an exception if not.
""".
-spec check_parser_operational() -> void().
check_parser_operational() ->

	ParserState = get_parser_backend_state(),

	check_parser_operational( ParserState ).



-doc """
Checks whether the specified JSON parser is operational; returns an updated
state if yes, otherwise throws an exception.
""".
-spec check_parser_operational( parser_state() ) -> parser_state().
check_parser_operational( ParserState={ json, _InternalBackendState } ) ->

	% This is a way to check that its BEAMs are available and fully usable:
	try json:decode( <<"\"test\"">> ) of

		<<"test">> ->
			% Const:
			ParserState

	catch

		error:undef ->
			trace_utils:error_fmt(
				"The built-in 'json' JSON parser is not operational.~n~ts",
				[ system_utils:get_json_unavailability_hint( json ) ] ),
			throw( { json_parser_not_operational, json } );

		OtherError ->
			trace_utils:error_fmt(
				"The built-in 'json' JSON parser does not work properly: ~p.",
				[ OtherError ] ),
			throw( { json_parser_dysfunctional, json, OtherError } )

	end;


check_parser_operational( ParserState={ jsx, _InternalBackendState } ) ->

	% This is a way to check that its BEAMs are available and fully usable:
	try jsx:is_json( <<"\"test\"">> ) of

		true ->
			% Const:
			ParserState

	catch

		error:undef ->
			trace_utils:error_fmt(
				"The jsx JSON parser is not operational.~n~ts",
				[ system_utils:get_json_unavailability_hint( jsx ) ] ),
			throw( { json_parser_not_operational, jsx } );

		OtherError ->
			trace_utils:error_fmt(
				"The jsx JSON parser does not work properly: ~p.",
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


-doc """
Converts (encodes) the specified JSON-compliant Erlang term into a JSON
counterpart document, using the looked-up default JSON backend for that.

For example `json_utils:to_json( #{
  <<"protected">> => Protected,
  <<"payload">> => Payload,
  <<"signature">> => EncSigned} )`.

No specific whitespace-based formatting is done, hence the result is less
readable but more compact.
""".
-spec to_json( json_term() ) -> json().
to_json( Term ) ->

	% The call that would be spared if using an explicit parser state:
	ParserState = get_parser_backend_state(),

	to_json( Term, ParserState ).



-doc """
Converts (encodes) the specified Erlang term into a JSON counterpart document,
using directly the JSON backend designated by the specified parser state.

For example `json_utils:to_json(#{
  <<"protected">> => Protected,
  <<"payload">> => Payload,
  <<"signature">> => EncSigned}, _ParserName=jsx )`.

No specific whitespace-based formatting is done, hence the result is less
readable but more compact.
""".
-spec to_json( json_term(), parser_state() ) -> json().
to_json( Term, ParserState ) ->
    to_json( Term, _DoFormat=false, ParserState ).


-doc """
Converts (encodes) the specified Erlang term into a JSON counterpart document,
formatting it for readability if requested (then with added whitespaces), using
directly the JSON backend designated by the specified parser state.

For example `json_utils:to_json(#{
  <<"protected">> => Protected,
  <<"payload">> => Payload,
  <<"signature">> => EncSigned}, _ParserName=jsx)`.
""".
-spec to_json( json_term(), boolean(), parser_state() ) -> json().
to_json( Term, DoFormat,
         _ParserState={ json, _UndefinedInternalBackendState } ) ->

	cond_utils:if_defined( myriad_debug_json,
		trace_utils:debug_fmt( "json is to encode:~n ~p", [ Term ] ) ),

	R = case DoFormat of

        true ->
            json:format( Term );

        false ->
            json:encode( Term )

    end,

	cond_utils:if_defined( myriad_debug_json,
		trace_utils:debug_fmt( "json returned encoded term:~n ~p", [ R ] ) ),

	R;


to_json( Term, _DoFormat,
         _ParserState={ jsx, _UndefinedInternalBackendState } ) ->

	Opts = get_base_json_encoding_options( jsx ),

	cond_utils:if_defined( myriad_debug_json,
		trace_utils:debug_fmt( "jsx is to encode, with options ~p:~n ~p",
							   [ Opts, Term ] ) ),

    % No specific formatting applies:
	R = jsx:encode( Term, Opts ),

	cond_utils:if_defined( myriad_debug_json,
		trace_utils:debug_fmt( "jsx returned encoded term:~n ~p", [ R ] ) ),

	R;


to_json( Term, _DoFormat,
         _ParserState={ jiffy, _UndefinedInternalBackendState } ) ->

	Opts = get_base_json_encoding_options( jiffy ),

	cond_utils:if_defined( myriad_debug_json,
		trace_utils:debug_fmt( "Jiffy is to encode, with options ~p:~n ~p",
							   [ Opts, Term ] ) ),

    % No specific formatting applies:
	R = jiffy:encode( Term, Opts ),

	cond_utils:if_defined( myriad_debug_json,
		trace_utils:debug_fmt( "Jiffy returned encoded term:~n ~p", [ R ] ) ),

	R.



-doc """
Converts (encodes) the specified JSON-compliant Erlang term into a JSON file,
using the looked-up default JSON backend for that.

For example `json_utils:to_json_file(#{
   <<"protected">> => Protected,
   <<"payload">> => Payload,
   <<"signature">> => EncSigned}, TargetJsonFilePath)`.

No specific whitespace-based formatting is done, hence the result is less
readable but more compact.
""".
-spec to_json_file( json_term(), file_path() ) -> void().
to_json_file( Term, TargetJsonFilePath ) ->
    to_json_file( Term, _DoFormat=false, TargetJsonFilePath ).



-doc """
Converts (encodes) the specified JSON-compliant Erlang term into a JSON file,
formatting it for readability if requested (then with added whitespaces), using
the specified JSON backend for that.

For example `json_utils:to_json_file(#{
   <<"protected">> => Protected,
   <<"payload">> => Payload,
   <<"signature">> => EncSigned}, TargetJsonFilePath, ParserState)`.
""".
-spec to_json_file( json_term(), file_path(), boolean() ) -> void().
to_json_file( Term, TargetJsonFilePath, DoFormat ) ->

	% The call that would be spared if using an explicit parser state:
	ParserState = get_parser_backend_state(),

    to_json_file( Term, TargetJsonFilePath, DoFormat, ParserState ).



-doc """
Converts (encodes) the specified JSON-compliant Erlang term into a JSON file,
formatting it for readability if requested (then with added whitespaces), using
the specified JSON backend for that.

For example `json_utils:to_json_file(#{
   <<"protected">> => Protected,
   <<"payload">> => Payload,
   <<"signature">> => EncSigned}, TargetJsonFilePath, ParserState)`.
""".
-spec to_json_file( json_term(), file_path(), boolean(), parser_state() ) ->
                                                void().
to_json_file( Term, TargetJsonFilePath, DoFormat, ParserState ) ->
	JsonContent = to_json( Term, DoFormat, ParserState ),
	file_utils:write_whole( TargetJsonFilePath, JsonContent ).




-doc "Returns the default options for the JSON encoding.".
-spec get_base_json_encoding_options( parser_backend_name() ) ->
												[ json_encoding_option() ].
get_base_json_encoding_options( _BackendName=json ) ->
	[];

get_base_json_encoding_options( _BackendName=jsx ) ->
	[];

get_base_json_encoding_options( _BackendName=jiffy ) ->

	% Jiffy only understands UTF-8 in binaries; force strings to encode as UTF-8
	% by fixing broken surrogate pairs and/or using the replacement character to
	% remove broken UTF-8 sequences in data:
	%
	% We do not specify here 'use_nil' as we want to use 'null' as jsx does.
	%
	[ force_utf8 ].





% Decoding section.


-doc """
Converts (decodes) the specified JSON document into an Erlang term counterpart,
recursively so that it can return a table containing tables, themselves
containing potentially tables, and so on, using the looked-up default JSON
backend for that.

Note that if in a given scope a key is present more than once, only one of its
values will be retained (generally the lastly defined one).
""".
-spec from_json( json() ) -> json_term().
from_json( Json ) ->
	ParserState = get_parser_backend_state(),
	from_json( Json, ParserState ).



-doc """
Converts (decodes) the specified JSON document into an Erlang term counterpart,
recursively so that it returns a table containing tables, themselves containing
potentially tables, and so on, using the specified JSON backend for that.

Note that if in a given scope a key is present more than once, only one of its
values will be retained (generally the lastly defined one).
""".
-spec from_json( json(), parser_state() ) -> json_term().
from_json( Json, _ParserState={ json, _UndefinedInternalBackendState } ) ->

	BinJson = case is_binary( Json ) of

		true ->
			Json;

		% Supposedly then a plain string:
		false ->
			text_utils:string_to_binary( Json )

	end,

	cond_utils:if_defined( myriad_debug_json,
		trace_utils:debug_fmt( "Decoding '~p' with json.", [ BinJson ] ) ),

	json:decode( BinJson );


from_json( Json, _ParserState={ jsx, _UndefinedInternalBackendState } ) ->

	BinJson = case is_binary( Json ) of

		true ->
			Json;

		% Supposedly then a plain string:
		false ->
			text_utils:string_to_binary( Json )

	end,

	cond_utils:if_defined( myriad_debug_json,
		trace_utils:debug_fmt( "Decoding '~p' with jsx.", [ BinJson ] ) ),

	% Note that at least some errors in the JSON file (e.g. missing comma) will
	% lead only to an exception such as:
	%
	% ** exception error: bad argument
	%  in function jsx_decoder:maybe_done/4
	%
	% (not even returning a line number for the faulty JSON part...)

	jsx:decode( BinJson, get_base_json_decoding_options( jsx ) );


from_json( Json, _ParserState={ jiffy, _UndefinedInternalBackendState } ) ->

	cond_utils:if_defined( myriad_debug_json,
		trace_utils:debug_fmt( "Decoding '~p' with jiffy.", [ BinJson ] ) ),

	jiffy:decode( Json, get_base_json_decoding_options( jiffy ) ).



-doc "Returns the default options for the JSON decoding.".
-spec get_base_json_decoding_options( parser_backend_name() ) ->
												[ json_decoding_option() ].
get_base_json_decoding_options( _BackendName=json ) ->
	% None applies anyway:
	[];

get_base_json_decoding_options( _BackendName=jsx ) ->
	% We used to prefer {state,<<"PUBLISHED">>} to
	% {<<"state">>,<<"PUBLISHED">>}, yet for compatibility with Jiffy we stick
	% to binaries now, so [{labels, atom}] is not used anymore.
	%
	% return_maps is default:
	[];

get_base_json_decoding_options( _BackendName=jiffy ) ->

	% dedupe_keys: if a key is repeated in a JSON object this flag will ensure
	% that the parsed object only contains a single entry containing the last
	% value seen.
	%
	[ return_maps, dedupe_keys ].



-doc """
Converts (decodes) the specified JSON file recursively into an Erlang term
counterpart, so that it returns typically a table containing tables, themselves
containing potentially tables, and so on, with specified parser state.

Note that if in a given scope a key is present more than once, only one of its
values will be retained (generally the lastly defined one).
""".
-spec from_json_file( any_file_path() ) -> json_term().
from_json_file( JsonFilePath ) ->
	BinJson = file_utils:read_whole( JsonFilePath ),
	from_json( BinJson ).



-doc """
Converts (decodes) the specified JSON file recursively into an Erlang term
counterpart, so that it returns typically a table containing tables, themselves
containing potentially tables, and so on, with specified parser state.

Note that if in a given scope a key is present more than once, only one of its
values will be retained (generally the lastly defined one).
""".
-spec from_json_file( any_file_path(), parser_state() ) -> json_term().
from_json_file( JsonFilePath, ParserState ) ->
	BinJson = file_utils:read_whole( JsonFilePath ),
	from_json( BinJson, ParserState ).



-doc """
Returns a (blank) parser state corresponding to the default parser.

(helper)
""".
-spec get_parser_backend_state() -> option( parser_state() ).
get_parser_backend_state() ->

	ParserName = get_available_parser_backend_name(),

	% Supposed stateless:
	{ ParserName, _InternalBackendState=undefined }.



-doc "Stops the JSON parser.".
-spec stop_parser() -> void().
stop_parser() ->
	ok.


-doc "Stops the specified JSON parser.".
-spec stop_parser( parser_state() ) -> void().
stop_parser( _ParserState ) ->
	ok.



-doc """
Returns a string explaining what to do in order to have the JSON support with
the specified backend available.
""".
-spec get_json_unavailability_hint( parser_backend_name() ) -> ustring().
get_json_unavailability_hint( _Backend=undefined ) ->
	% Note: the hints are *not* truncated here, this is normal:
	"Hint: inspect, in myriad/GNUmakevars.inc, the USE_JSON and "
	"JSX_BASE / JIFFY_BASE runtime variables, knowing that the "
		++ code_utils:get_code_path_as_string();

get_json_unavailability_hint( _Backend=json ) ->
	"Hint: check that using Erlang 27.0 or more recent, and inspect, in "
	"myriad/GNUmakevars.inc, the USE_JSON runtime variables, knowing that the "
	++ code_utils:get_code_path_as_string();

get_json_unavailability_hint( _Backend=jsx ) ->
	"Hint: inspect, in myriad/GNUmakevars.inc, the USE_JSON and "
	"JSX_BASE runtime variables, knowing that the "
		++ code_utils:get_code_path_as_string();

get_json_unavailability_hint( _Backend=jiffy ) ->
	"Hint: inspect, in myriad/GNUmakevars.inc, the USE_JSON and "
	"JIFFY_BASE runtime variables, knowing that the "
		++ code_utils:get_code_path_as_string().
