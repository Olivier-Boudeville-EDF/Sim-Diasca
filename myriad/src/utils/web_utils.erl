% Copyright (C) 2019-2022 Olivier Boudeville
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
% Creation date: Tuesday, June 25, 2019.


% @doc Gathering of services for <b>web-related</b> uses, notably for <b>HTML
% generation</b> or <b>HTTP/HTTPS management</b>.
%
% See web_utils_test.erl for the corresponding test.
%
% See also: rest_utils.erl.
%
-module(web_utils).


% Implementation notes:
%
% The functions based on an HTTP client (ex: request/6, get/3, post/3, post/4,
% post/5, download_file/2) rely here on the Erlang-native 'httpc' module, a
% fairly low-level, basic HTTP/1.1.
%
% More advanced needs may rely instead on Gun or Shotgun. Refer to LEEC
% (https://github.com/Olivier-Boudeville/Ceylan-LEEC in leec_api.erl) for an
% example thereof.


-type ssl_opt() :: 'no_ssl' | 'ssl'.
% Tells whether the SSL support is needed (typically for https).

-type url() :: ustring().

-type bin_url() :: bin_string().

-type any_url() :: url() | bin_url().


-type uri() :: ustring().

-type bin_uri() :: bin_string().

-type any_uri() :: uri() | bin_uri().


-type protocol_type() :: 'http' | 'https' | 'ftp'.
% The possible protocols (schemes) for an URL (use uri_string:parse/1 to extract
% it).


-type path() :: ustring().
% Path of an URL (ex: 'access/login').


% For the (deprecated) url_info record:
-include("web_utils.hrl").


-type url_info() :: #url_info{}.
% Full information about an URL (prefer uri_string:uri_map() now).


-type body() :: string_body() | bin_body().
% The body of a HTTP message. The binary form is strongly recommended (more
% compact, and, more importantly, a lot less problematic regarding encodings).

-type string_body() :: ustring().
% A body as a plain string (beware to encodings).

-type bin_body() :: binary().
% A body in binary form, which is the recommended one.


-type json_body() :: body().
% Encoded in JSON.

-type content_type() :: ustring().


-type old_style_options() :: [ { Field :: ustring(), Value :: ustring() } ].


-type new_style_options() :: maps:maps( bin_string(), bin_string() ).


-type headers_as_list() :: old_style_options().
% Example: {"User-Agent", "Godzilla The Mighty"}.


-type headers_httpc_style() :: headers_as_list().


-type headers_as_maps() :: new_style_options().

-type headers() :: headers_as_list() | headers_as_maps().


% Even if httpc:http_option/0 is not exported:
-type http_option() :: httpc:http_option().

-type options_for_httpc() :: [ http_option() ].

-type list_options() :: [ { atom(), term() } ].
-type map_options() :: maps:maps( atom(), term() ).

-type http_options() :: options_for_httpc() | map_options().

-type ssl_options() :: list_options(). % map_options().


-type request_result() :: { http_status_code(), headers_as_maps(), bin_body() }
						| { 'error', basic_utils:error_reason() }.


-type location() :: atom().

-type nonce() :: bin_string().


-type media_type() :: unicode:chardata().
% A media type (formerly known as "MIME type").
%
% Ex: "audio/ogg".
%
% Refer to [https://en.wikipedia.org/wiki/Media_type].


-type html_element() :: any_string().
% Ex: "<p>Hello!</p>".


-type http_status_class() ::

	% 1xx informational response: the request was received, continuing process:
	'informational_response'

	% 2xx successful: the request was successfully received, understood, and
	% accepted:
	%
	| 'successful'

	% 3xx redirection: further action needs to be taken in order to complete the
	% request:
	%
	| 'redirection'

	% 4xx client error: the request contains bad syntax or cannot be fulfilled
	| 'client_error'

	% 5xx server error: the server failed to fulfill an apparently valid request
	| 'server_error'.
% There are five classes defined by the standard for HTTP status codes.


-type http_status_code() :: non_neg_integer().



% Cloud-related section.

-type cloud_provider() :: 'azure' | 'aws' | 'google_cloud'.

-type service_endpoint() :: bin_string().
% The full, readily usable endpoint of a given service.
%
% Ex:
% `<<"https://francecentral.api.cognitive.microsoft.com/sts/v1.0/issuetoken">>'.


-type api_endpoint() :: bin_string().
% The API endpoint (with no deployment-related prefix such as
% "https://francecentral.") of a given service.
%
% Ex: `<<"api.cognitive.microsoft.com/sts/v1.0/issuetoken">>'.


-type instance_key() :: bin_string().
% The key of an instance hosted by a cloud provider.
% Ex: `<<"a59c98302e7f4a22be4b355125df8e9">>'.


-type cloud_instance_info() :: azure_instance_info().
% Information regarding an instance hosted by a cloud provider.


-type azure_instance_info() :: #azure_instance_info{}.
% Information regarding an instance hosted by the Microsoft Azure cloud
% provider.


-type azure_instance_key() :: instance_key().
% The key of a Microsoft Azure instance.
% Ex: `<<"a59c98302e7f4a22be4b355125df8e9">>'.


-type azure_instance_location() :: bin_string().
% The location of a Microsoft Azure instance.
% Ex: `<<"francecentral">>'.



-export_type([ ssl_opt/0,
			   url/0, bin_url/0, any_url/0,
			   uri/0, bin_uri/0, any_uri/0,
			   protocol_type/0, path/0, url_info/0,

			   body/0, string_body/0, bin_body/0, json_body/0,
			   headers/0, map_options/0,
			   http_option/0, http_options/0, ssl_options/0,
			   location/0, nonce/0,

			   media_type/0,
			   html_element/0, http_status_class/0, http_status_code/0,

			   cloud_provider/0,
			   service_endpoint/0, api_endpoint/0,
			   instance_key/0, cloud_instance_info/0,

			   azure_instance_info/0, azure_instance_key/0,
			   azure_instance_location/0 ]).


% HTTP-related section:

% URL subsection:
-export([ encode_as_url/1, encode_element_as_url/1, escape_as_url/1,
		  get_last_path_element/1,

		  % Deprecated in favor ofthe standard uri_string module:
		  url_info_to_string/1, string_to_url_info/1, string_to_uri_map/1 ]).


% HTML-related section:
-export([ get_ordered_list/1, get_unordered_list/1,

		  escape_as_html_content/1, escape_term_as_html_content/1,

		  get_http_status_class/1, http_status_class_to_string/1,
		  interpret_http_status_code/1 ]).


% HTTP-related operations:
-export([ start/0, start/1,
		  request/6, get/3, post/3, post/4, post/5,
		  download_file/2,
		  stop/0 ]).


% SSL-related operations:
-export([ get_ssl_verify_options/0, get_ssl_verify_options/1 ]).


% Cloud-related operations:
-export([ get_azure_instance_information/2, cloud_instance_info_to_string/1 ]).


-define( default_content_type, "text/html; charset=UTF-8" ).


% Shorthands:

-type activation_switch() :: basic_utils:activation_switch().

-type option_list() :: option_list:option_list().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type any_directory_path() :: file_utils:any_directory_path().
-type file_path() :: file_utils:file_path().
-type file_name() :: file_utils:file_name().

%-type time_out() :: time_utils:time_out().

-type method() :: rest_utils:method().



% HTTP-related operations.


% URL subsection.

% About encoding.

% The character "è" (e with a grave accent, hex code: xE8) might be for example
% either translated as "%C3%A8" or as "%E8". It is apparently the difference
% between encodeURI(chr) and escape(chr) in Javascript.
%
% The most adequate encoding in general seems the first (in which case
% encode_as_url/1 and encode_element_as_url/1 shall be used), however some
% webservers seem to insist on having the second (in which case escape/1 and
% escape_element/1 shall be used).
%
% See also [http://www.javascripter.net/faq/accentedcharacters.htm].



% @doc Encodes specified list of {Key, Value} pairs so that it can used into an
% URL.
%
% Full example:
%
% inets:start(),
% httpc:request(post, {"http://localhost:3000/foo", [],
%  "application/x-www-form-urlencoded",
%  encode_as_url([{"username", "bob"}, {"password", "123456"}])}, [], []).
%
% Directly inspired from:
% http://stackoverflow.com/questions/114196/url-encode-in-erlang
%
% See also escape_as_url/1 for some more specific uses.
%
-spec encode_as_url( option_list() ) -> ustring().
encode_as_url( OptionList ) ->
   encode_as_url( OptionList, _Acc=[] ).

encode_as_url( _OptionList=[], Acc ) ->
	Acc;

% First entry:
encode_as_url( [ { Key, Value } | T ], _Acc=[] ) ->
	encode_as_url( T, encode_element_as_url( Key ) ++ "="
				   ++ encode_element_as_url( Value ) );

encode_as_url( [ { Key, Value } | T ], Acc ) ->
	encode_as_url( T, Acc ++ "&" ++ encode_element_as_url( Key ) ++ "="
				   ++ encode_element_as_url( Value ) ).


% @doc Encodes specified element so that it can be used in an URL.
-spec encode_element_as_url( ustring() ) -> ustring().
encode_element_as_url( E ) ->
	% They seem to produce quite similar results in our few test cases:
	edoc_lib:escape_uri( E ).
	%encode_uri_rfc3986:encode( E ).



% @doc Escapes specified list of {Key,Value} pairs so that it can used into some
% URL.
%
% Note: apparently useful only for quite specific websites; encode_as_url/1
% should be preferred in most cases.
%
-spec escape_as_url( option_list() ) -> ustring().
escape_as_url( OptionList ) ->
	%trace_utils:debug_fmt( "~n~nEscaping '~p'.", [ OptionList ] ),
	escape_as_url( OptionList, _Acc=[] ).

escape_as_url( _OptionList=[], Acc ) ->
	Acc;

% First entry:
escape_as_url( [ { Key, Value } | T ], _Acc=[] ) ->
	escape_as_url( T, escape_key( Key ) ++ "=" ++ escape_value( Value ) );

escape_as_url( [ { Key, Value } | T ], Acc ) ->
	escape_as_url( T, Acc ++ "&" ++ escape_key( Key ) ++ "="
			++ escape_value( Value ) ).



% @doc Escapes specified element so that it can be used in some URL.
-spec escape_key( option_list:key() ) -> ustring().
escape_key( Key ) when is_atom( Key ) ->
	text_utils:atom_to_string( Key ).


-spec escape_value( ustring() ) -> ustring().
escape_value( String ) ->
	R = lists:flatten( [ escape_char( C ) || C <- String ] ),
	%io:format( "'~ts' became '~ts'.~n", [ String, R ] ),
	R.



% @doc Escapes specified character.
%
% Alphanumerical characters left as are:
escape_char( C ) when C >= 48 andalso C =< 57 ->
	% 0..9 kept as is:
	C;

escape_char( C ) when C >= 65 andalso C =< 90 ->
	% A..Z kept as is:
	C;

escape_char( C ) when C >= 97 andalso C =< 122 ->
	% a..z kept as is:
	C;

escape_char( C ) ->
	% Everything else is blindly encoded:
	io_lib:format( "%~ts", [ integer_to_list( C, _HexBase=16 ) ] ).



% @doc Returns the last element, the final path "filename" pointed by specified
% URL.
%
% Ex: "hello.txt" = web_utils:get_last_path_element(
%                         "http://www.foobar.org/baz/hello.txt" )
%
-spec get_last_path_element( url() ) -> file_name().
get_last_path_element( Url ) ->
	% Hackish yet working perfectly:
	filename:basename( Url ).



% @doc Returns a string describing the specified URL information.
-spec url_info_to_string( url_info() ) -> ustring().
url_info_to_string( #url_info{ protocol=Protocol, host_identifier=Host,
							   port=Port, path=Path } ) ->

	text_utils:format( "~ts://~ts:~B/~ts",
		[ Protocol, net_utils:host_to_string( Host ), Port, Path ] ).



% @doc Decodes specified string into an url_info record, by extracting protocol
% (scheme), host, port and path information.
%
% Note that other information (fragment, query, userinfo) will be ignored and
% lost.
%
% Note: using string_to_uri_map/1  might be
% a more complete option; this function remains mostly for backward
% compatibility.
%
-spec string_to_url_info( ustring() ) -> url_info().
string_to_url_info( String ) ->

	% Deprecated http_uri:parse/1 was used previously, now relying on (available
	% since Erlang 23.0):
	%
	#{ % fragment => unicode:chardata(),

	   % host => unicode:chardata(),
	   host := Host,

	   % path => unicode:chardata(),
	   path := Path,

	   % port => integer() >= 0 | undefined,
	   port := MaybePort,

	   % query => unicode:chardata(),

	   % scheme => unicode:chardata(),
	   scheme := Scheme

	   %userinfo => unicode:chardata()

		 } = string_to_uri_map( String ),

	#url_info{ protocol=Scheme, host_identifier=Host, port=MaybePort,
			   path=Path }.



% @doc Decodes specified string into an URI map, by extracting all relevant
% information: protocol (scheme), user information, host, port, path and
% fragment.
%
% Throws an exception on failure.
%
-spec string_to_uri_map( ustring() ) -> uri_string:uri_map().
string_to_uri_map( String ) ->

	case uri_string:parse( String ) of

		{ error, ReasonAtom, ReasonTerm } ->
			throw( { uri_parsing_failed, String, ReasonAtom, ReasonTerm } );

		URIMap ->
			URIMap

	end.




% HTML-related section.


% @doc Returns the HTML code of an ordered (numbered bullets) list corresponding
% to specified list of elements.
%
-spec get_ordered_list( [ html_element() ] ) -> html_element().
get_ordered_list( Elements ) ->

	HTMLElems = [ text_utils:format( "    <li>~ts</li>~n", [ E ] )
					|| E <- Elements ],

	text_utils:format( "  <ol>~n~ts  </ol>~n", [ lists:flatten( HTMLElems ) ] ).



% @doc Returns the HTML code of an unordered list corresponding to specified
% list of elements.
%
-spec get_unordered_list( [ html_element() ] ) -> html_element().
get_unordered_list( Elements ) ->

	HTMLElems = [ text_utils:format( "    <li>~ts</li>~n", [ E ] )
				  || E <- Elements ],

	text_utils:format( "  <ul>~n~ts  </ul>~n", [ lists:flatten( HTMLElems ) ] ).



% @doc Escapes specified text, so that it can be included safely as an HTML
% content.
%
% Returns an HTML element, as a plain string.
%
-spec escape_as_html_content( any_string() ) -> html_element().
escape_as_html_content( BinString ) when is_binary( BinString ) ->
	escape_as_html_content( text_utils:binary_to_string( BinString ) );

escape_as_html_content( String ) ->
	% Flatten needed if having an IO list as input:
	escape_as_html_content( text_utils:to_unicode_list( String ), _Acc=[] ).


% The escaping done is exactly sufficient for XML and correct for HTML; see
% https://stackoverflow.com/questions/7248958/which-are-the-html-and-xml-special-characters
% for further reference.
%
% (helper)
escape_as_html_content( _String=[], Acc ) ->
	lists:reverse( text_utils:to_unicode_list( Acc ) );

% Replacements are pre-reversed:
escape_as_html_content( _String=[ $& | T ], Acc ) ->
	escape_as_html_content( T, [ ";pma&" | Acc ] ) ;

escape_as_html_content( _String=[ $< | T ], Acc ) ->
	escape_as_html_content( T, [ ";tl&" | Acc ] ) ;

% Not strictly necessary for HTML:
escape_as_html_content( _String=[ $> | T ], Acc ) ->
	escape_as_html_content( T, [ ";tg&" | Acc ] ) ;

% These two clauses apply only inside of attribute values, yet are a general,
% safer measure:
%
escape_as_html_content( _String=[ $" | T ], Acc ) ->
	escape_as_html_content( T, [ ";touq&" | Acc ] ) ;

% Not strictly necessary for HTML; even in XML, single quotes only need to be
% escaped if they occur in an attribute value enclosed by single quotes.
%
escape_as_html_content( _String=[ $' | T ], Acc ) ->
	%escape_as_html_content( T, [ ";93#&" | Acc ] ) ;
	escape_as_html_content( T, [ ";sopa&" | Acc ] ) ;

% All others:
escape_as_html_content( _String=[ Other | T ], Acc ) ->
	escape_as_html_content( T, [ Other | Acc ] ).




% @doc Escapes specified term (most probably non-string), so that it can be
% included safely as an HTML content.
%
-spec escape_term_as_html_content( term() ) -> html_element().
escape_term_as_html_content( Term ) ->
	escape_as_html_content( text_utils:term_to_string( Term ) ).


% @doc Returns the status class (if any) corresponding to the specified HTTP
% status code.
%
-spec get_http_status_class( http_status_code() ) ->
								maybe( http_status_class() ).
get_http_status_class( StatusCode )
  when StatusCode >= 100 andalso StatusCode < 200 ->
	informational_response;

get_http_status_class( StatusCode )
  when StatusCode >= 200 andalso StatusCode < 300 ->
	successful;

get_http_status_class( StatusCode )
  when StatusCode >= 300 andalso StatusCode < 400 ->
	redirection;

get_http_status_class( StatusCode )
  when StatusCode >= 400 andalso StatusCode < 500 ->
	client_error;

get_http_status_class( StatusCode )
  when StatusCode >= 500 andalso StatusCode < 600 ->
	server_error;

get_http_status_class( StatusCode ) when is_integer( StatusCode ) ->
	undefined;

get_http_status_class( StatusCode ) ->
	throw( { invalid_status_code, StatusCode } ).


% @doc Returns a textual description of specified HTTP status class.
-spec http_status_class_to_string( maybe( http_status_class() ) ) -> ustring().
http_status_class_to_string( informational_response ) ->
	"informational response";

http_status_class_to_string( successful ) ->
	"action success";

http_status_class_to_string( redirection ) ->
	"additional action needed";

http_status_class_to_string( client_error ) ->
	"client-side error";

http_status_class_to_string( server_error ) ->
	"server-side error";

http_status_class_to_string( undefined ) ->
	"unknown status class";

http_status_class_to_string( Other ) ->
	throw( { invalid_status_class, Other } ).



% @doc Returns a textual description of specified HTTP code.
%
% Source: [https://en.wikipedia.org/wiki/List_of_HTTP_status_codes].
%
-spec interpret_http_status_code( http_status_code() ) -> ustring().
interpret_http_status_code( StatusCode ) ->
	text_utils:format( "~ts (code ~B: ~ts)", [
		interpret_http_status_code_helper( StatusCode ), StatusCode,
		http_status_class_to_string( get_http_status_class( StatusCode ) ) ] ).


% (helper)
%
% informational_response class:
interpret_http_status_code_helper( _StatusCode=100 ) ->
	"continue";

interpret_http_status_code_helper( _StatusCode=101 ) ->
	"switching protocols";

interpret_http_status_code_helper( _StatusCode=102 ) ->
	"processing WebDAV";

interpret_http_status_code_helper( _StatusCode=103 ) ->
	"early hints";


% successful class:
interpret_http_status_code_helper( _StatusCode=200 ) ->
	"successful request";

interpret_http_status_code_helper( _StatusCode=201 ) ->
	"resource created";

interpret_http_status_code_helper( _StatusCode=202 ) ->
	"request accepted";

interpret_http_status_code_helper( _StatusCode=203 ) ->
	"non-authoritative information";

interpret_http_status_code_helper( _StatusCode=204) ->
	"returning no content";

interpret_http_status_code_helper( _StatusCode=205 ) ->
	"reset view requested";

interpret_http_status_code_helper( _StatusCode=206 ) ->
	"partial content delivered";

interpret_http_status_code_helper( _StatusCode=207 ) ->
	"multi-status WebDAV returned";

interpret_http_status_code_helper( _StatusCode=208 ) ->
	"DAV members already reported";

interpret_http_status_code_helper( _StatusCode=226 ) ->
	"IM used";


% redirection class:
interpret_http_status_code_helper( _StatusCode=300 ) ->
	"multiple options fo resource";

interpret_http_status_code_helper( _StatusCode=301 ) ->
	"resource moved permanently";

interpret_http_status_code_helper( _StatusCode=302 ) ->
	"resource found";

interpret_http_status_code_helper( _StatusCode=303 ) ->
	"get from other URI";

interpret_http_status_code_helper( _StatusCode=304 ) ->
	"resource still the same";

interpret_http_status_code_helper( _StatusCode=305 ) ->
	"switch to proxy";

interpret_http_status_code_helper( _StatusCode=306 ) ->
	"use proxy";

interpret_http_status_code_helper( _StatusCode=307 ) ->
	"temporary redirection";

interpret_http_status_code_helper( _StatusCode=308 ) ->
	"permanent redirection";


% client_error class:

interpret_http_status_code_helper( _StatusCode=400 ) ->
	"invalid request";

interpret_http_status_code_helper( _StatusCode=401 ) ->
	"unauthorized";

interpret_http_status_code_helper( _StatusCode=402 ) ->
	"payment required";

interpret_http_status_code_helper( _StatusCode=403 ) ->
	"forbidden action";

interpret_http_status_code_helper( _StatusCode=404 ) ->
	"resource not found";

interpret_http_status_code_helper( _StatusCode=405 ) ->
	"method not allowed";

interpret_http_status_code_helper( _StatusCode=406 ) ->
	"not acceptable";

interpret_http_status_code_helper( _StatusCode=407 ) ->
	"proxy authentication required";

interpret_http_status_code_helper( _StatusCode=408 ) ->
	"request timeout";

interpret_http_status_code_helper( _StatusCode=409 ) ->
	"resource state conflict";

interpret_http_status_code_helper( _StatusCode=410 ) ->
	"resource gone for good";

interpret_http_status_code_helper( _StatusCode=411 ) ->
	"length required";

interpret_http_status_code_helper( _StatusCode=412 ) ->
	"precondition failed";

interpret_http_status_code_helper( _StatusCode=413 ) ->
	"payload too large";

interpret_http_status_code_helper( _StatusCode=414 ) ->
	"URI too long";

interpret_http_status_code_helper( _StatusCode=415 ) ->
	"unsupported media type";

interpret_http_status_code_helper( _StatusCode=416 ) ->
	"range not satisfiable";

interpret_http_status_code_helper( _StatusCode=417 ) ->
	"expectation failed";

interpret_http_status_code_helper( _StatusCode=418 ) ->
	"I'm a teapot";

interpret_http_status_code_helper( _StatusCode=421 ) ->
	"misdirected request";

interpret_http_status_code_helper( _StatusCode=422 ) ->
	"unprocessable entity";

interpret_http_status_code_helper( _StatusCode=423 ) ->
	"resource locked";

interpret_http_status_code_helper( _StatusCode=424 ) ->
	"failed dependency";

interpret_http_status_code_helper( _StatusCode=425 ) ->
	"too early";

interpret_http_status_code_helper( _StatusCode=426 ) ->
	"upgrade required";

interpret_http_status_code_helper( _StatusCode=428 ) ->
	"precondition required";

interpret_http_status_code_helper( _StatusCode=429 ) ->
	"too many requests";

interpret_http_status_code_helper( _StatusCode=431 ) ->
	"request header fields too large";

interpret_http_status_code_helper( _StatusCode=451 ) ->
	"unavailable for legal reasons";


% server_error class:
interpret_http_status_code_helper( _StatusCode=500 ) ->
	"internal server error";

interpret_http_status_code_helper( _StatusCode=501 ) ->
	"not implemented";

interpret_http_status_code_helper( _StatusCode=502 ) ->
	"bad gateway";

interpret_http_status_code_helper( _StatusCode=503 ) ->
	"service unavailable";

interpret_http_status_code_helper( _StatusCode=504 ) ->
	"gateway timeout";

interpret_http_status_code_helper( _StatusCode=505 ) ->
	"HTTP version not supported";

interpret_http_status_code_helper( _StatusCode=506 ) ->
	"variant also negotiates";

interpret_http_status_code_helper( _StatusCode=507 ) ->
	"insufficient storage";

interpret_http_status_code_helper( _StatusCode=508 ) ->
	"loop detected";

interpret_http_status_code_helper( _StatusCode=510 ) ->
	"not extended";

interpret_http_status_code_helper( _StatusCode=511 ) ->
	"network authentication required";

% Unexpected class:
interpret_http_status_code_helper( _StatusCode ) ->
	"unknown HTTP status class".




% HTTP-related operations.


% @doc Starts the HTTP support, with default settings.
%
% Does not fail if already started, throws an exception in case of unrecoverable
% error.
%
-spec start() -> void().
start() ->
	start( no_ssl ).



% @doc Starts the HTTP support, with specified settings.
%
% Does not fail if already started, throws an exception in case of unrecoverable
% error.
%
-spec start( ssl_opt() ) -> void().
start( Option ) ->

	cond_utils:if_defined( myriad_debug_web_exchanges,
		trace_bridge:debug_fmt( "[~w] Starting httpc-based web support "
			"with option ~p.", [ self(), Option ] ) ),

	% Starts the (built-in) HTTP client:
	case inets:start( _DefaultInetsType=temporary ) of

		ok ->
			ok;

		% Module expected to be 'inets':
		{ error, { already_started, _Module } } ->
			%trace_bridge:info_fmt( "Starting web_utils reported that module "
			%   "'~ts' was already started.", [ Module ] ),
			ok;

		{ error, InetsReason } ->
			trace_bridge:error_fmt( "Starting web_utils reported following "
				"error: ~p.", [ InetsReason ] ),
			throw( { start_failed, inets, InetsReason } )

	end,

	% Starts the SSL support if requested:
	case Option of

		no_ssl ->
			ok;

		ssl ->
			case ssl:start( _DefaultSSLType=temporary ) of

				ok ->
					ok;

				{ error, SSLReason } ->
					trace_bridge:error_fmt( "Starting web_utils reported "
						"following error: ~p.", [ SSLReason ] ),
					throw( { start_failed, ssl, SSLReason } )

			end

	end.



% @doc Sends a (synchronous) HTTP/1.1 client request (GET or POST).
%
% The HTTP support (possibly with SSL if needed) must be started.
%
% For HTTPS requests, we recommend that the HttpOptions include a {ssl,
% web_utils:get_ssl_verify_options()} pair.
%
% For more advanced uses (ex: re-using of permanent connections, HTTP/2, etc.),
% consider relying on Gun or Shotgun.
%
-spec request( method(), uri(), headers(), http_options(), maybe( bin_body() ),
			   maybe( content_type() ) ) -> request_result().
request( _Method=get, Uri, Headers, HttpOptions, _MaybeBody=undefined,
		 _MaybeContentType=undefined ) ->
	get( Uri, Headers, HttpOptions );

request( _Method=get, _Uri, _Headers, _HttpOptions, MaybeBody,
		 MaybeContentType ) ->
	throw( { invalid_get_request, { body, MaybeBody },
			 { content_type, MaybeContentType } } );

request( _Method=post, Uri, Headers, HttpOptions, MaybeBody,
		 MaybeContentType ) ->
	post( Uri, Headers, HttpOptions, MaybeBody, MaybeContentType );

% Not supported: head |  put | trace | options | delete | patch:
request( Method, Uri, _Headers, _HttpOptions, _MaybeBody, _MaybeContentType ) ->
	throw( { invalid_method, Method, Uri } ).



% @doc Sends a (synchronous) HTTP/1.1 client GET request.
%
% The HTTP support (possibly with SSL if needed) must be started.
%
% For HTTPS requests, we recommend that the HttpOptions include a {ssl,
% web_utils:get_ssl_verify_options()} pair.
%
% For more advanced uses (ex: re-using of permanent connections, HTTP/2, etc.),
% consider relying on Gun or Shotgun.
%
-spec get( uri(), headers(), http_options() ) -> request_result().
get( Uri, Headers, HttpOptions ) ->

	cond_utils:if_defined( myriad_debug_web_exchanges,
		trace_bridge:debug_fmt( "[~w] GET request to URI "
			"'~ts', with following headers:~n  ~p~nand "
			"HTTP options:~n  ~p.", [ self(), Uri, Headers, HttpOptions ] ) ),

	HeadersForHttpc = to_httpc_headers( Headers ),

	% Any content-type expected in headers, and no specific body for GET:
	Req = { Uri, HeadersForHttpc },

	HttpOptionsForHttpc = to_httpc_options( HttpOptions ),

	% Wanting the resulting body (as a binary rather than as a plain string),
	% headers, and the entire status line:
	%
	Options = [ { full_result, true }, { body_format, binary } ],

	cond_utils:if_defined( myriad_debug_web_exchanges,
		trace_bridge:debug_fmt( "[~w] Actual parameters of the httpc GET "
			"request:~n - request: ~p~n - HTTP options: ~p~n - options: ~p~n",
			[ self(), Req, HttpOptionsForHttpc, Options ] ) ),

	case httpc:request( _Method=get, Req, HttpOptionsForHttpc, Options ) of

		% Ex: HttpVersion="HTTP/1.1", StatusCode=200, ReqReason="OK".
		{ ok, { _StatusLine={ ReqHttpVersion, ReqStatusCode, ReqReason },
				ReqHeaders, ReqBody } } ->

			cond_utils:if_defined( myriad_debug_web_exchanges,
				trace_bridge:debug_fmt( "[~w] Received HTTP version: ~ts, "
					"status code: ~B, reason: ~ts; headers are:~n  ~p"
					"Returned body is ~p", [ self(), ReqHttpVersion,
						ReqStatusCode, ReqReason, ReqHeaders, ReqBody ] ),
				basic_utils:ignore_unused( [ ReqHttpVersion, ReqReason ] ) ),

			MapHeaders = from_httpc_headers( ReqHeaders ),

			{ ReqStatusCode, MapHeaders, ReqBody };

		Err={ error, ErrorReason } ->
			cond_utils:if_defined( myriad_debug_web_exchanges,
				trace_bridge:error_fmt( "[~w] GET failed: ~p ",
										[ self(), ErrorReason ] ),
			basic_utils:ignore_unused( ErrorReason ) ),
			Err

	end.




% @doc Sends a (synchronous, body-less) HTTP/1.1 client POST request.
%
% The HTTP support (possibly with SSL if needed) must be started.
%
% For HTTPS requests, we recommend that the HttpOptions include a {ssl,
% web_utils:get_ssl_verify_options()} pair.
%
% For more advanced uses (ex: re-using of permanent connections, HTTP/2, etc.),
% consider relying on Gun or Shotgun.
%
-spec post( uri(), headers(), http_options() ) -> request_result().
post( Uri, Headers, HttpOptions ) ->
	post( Uri, Headers, HttpOptions, _MaybeBody=undefined ).



% @doc Sends a (synchronous) HTTP/1.1 client POST request.
%
% If a body is specified yet no content-type is set, ?default_content_type will
% be used. To avoid encoding issues, we strongly recommend to pass binary bodies
% rather than string ones.
%
% The HTTP support (possibly with SSL if needed) must be started.
%
% For HTTPS requests, we recommend that the HttpOptions include a {ssl,
% web_utils:get_ssl_verify_options()} pair.
%
% For more advanced uses (ex: re-using of permanent connections, HTTP/2, etc.),
% consider relying on Gun or Shotgun.
%
-spec post( uri(), headers(), http_options(), maybe( body() ) ) ->
								request_result().
post( Uri, Headers, HttpOptions, MaybeBody ) ->
	post( Uri, Headers, HttpOptions, MaybeBody, _MaybeContentType=undefined ).



% @doc Sends a (synchronous) HTTP/1.1 client POST request.
%
% If a body is specified yet no content-type is set, ?default_content_type will
% be used. To avoid encoding issues, we strongly recommend to pass binary bodies
% rather than string ones.
%
% The HTTP support (possibly with SSL if needed) must be started.
%
% For HTTPS requests, we recommend that the HttpOptions include a {ssl,
% web_utils:get_ssl_verify_options()} pair.
%
% For more advanced uses (ex: re-using of permanent connections, HTTP/2, etc.),
% consider relying on Gun or Shotgun.
%
-spec post( uri(), headers(), http_options(), maybe( body() ),
			maybe( content_type() ) ) -> request_result().
post( Uri, Headers, HttpOptions, MaybeBody, MaybeContentType ) ->

	cond_utils:if_defined( myriad_debug_web_exchanges,
		trace_bridge:debug_fmt( "[~w] POST request to URI "
			"'~ts', with following headers:~n  ~p~nHTTP options:~n  ~p~n"
			"Body: ~p~nContent-type: ~ts",
			[ self(), Uri, Headers, HttpOptions, MaybeBody,
			  MaybeContentType ] ) ),

	HeadersForHttpc = to_httpc_headers( Headers ),

	% Any content-type expected in headers:
	Req = case MaybeBody of

		undefined ->
			% Then no content-type applies:
			{ Uri, HeadersForHttpc };

		Body ->
			ContentType = case MaybeContentType of

				undefined ->
					?default_content_type;

				_ ->
					MaybeContentType

			end,
			{ Uri, HeadersForHttpc, ContentType, Body }

	end,

	HttpOptionsForHttpc = to_httpc_options( HttpOptions ),

	% Wanting the resulting body (as a binary rather than as a plain string),
	% headers, and the entire status line:
	%
	Options = [ { full_result, true }, { body_format, binary } ],

	cond_utils:if_defined( myriad_debug_web_exchanges,
		trace_bridge:debug_fmt( "[~w] Actual parameters of the httpc POST "
			"request:~n - request: ~p~n - HTTP options: ~p~n - options: ~p~n",
			[ self(), Req, HttpOptionsForHttpc, Options ] ) ),

	case httpc:request( _Method=post, Req, HttpOptionsForHttpc, Options ) of

		% Ex: HttpVersion="HTTP/1.1", StatusCode=200, ReqReason="OK".
		{ ok, { _StatusLine={ ReqHttpVersion, ReqStatusCode, ReqReason },
				ReqHeaders, ReqBody } } ->

			cond_utils:if_defined( myriad_debug_web_exchanges,
				trace_bridge:debug_fmt( "[~w] Received HTTP version: ~ts, "
					"status code: ~B, reason: ~ts; headers are:~n  ~p"
					"Returned body is:~n ~p", [ self(), ReqHttpVersion,
						ReqStatusCode, ReqReason, ReqHeaders, ReqBody ] ),
				basic_utils:ignore_unused( [ ReqHttpVersion, ReqReason ] ) ),

			MapHeaders = from_httpc_headers( ReqHeaders ),

			{ ReqStatusCode, MapHeaders, ReqBody };

		Err={ error, ErrorReason } ->
			cond_utils:if_defined( myriad_debug_web_exchanges,
				trace_bridge:error_fmt( "[~w] POST failed: ~p ",
										[ self(), ErrorReason ] ),
			basic_utils:ignore_unused( ErrorReason ) ),
			Err

	end.


% @doc Converts headers into suitable ones for httpc.
-spec to_httpc_headers( headers() ) -> headers_httpc_style().
to_httpc_headers( Headers ) when is_list( Headers ) ->
	Headers;

to_httpc_headers( Headers ) when is_map( Headers ) ->
	[ { text_utils:binary_to_string( K ), text_utils:binary_to_string( V ) }
		|| { K, V } <- maps:to_list( Headers ) ].


% @doc Converts httpc headers into map-based ones.
-spec from_httpc_headers( headers_httpc_style() ) -> headers_as_maps().
from_httpc_headers( Headers ) ->
	maps:from_list( [ { text_utils:string_to_binary( K ),
						text_utils:string_to_binary( V ) }
								|| { K, V } <- Headers ] ).



% @doc Returns http options that are suitable for httpc.
-spec to_httpc_options( http_options() ) -> options_for_httpc().
to_httpc_options( HttpOptions ) when is_list( HttpOptions ) ->
	HttpOptions;

to_httpc_options( HttpOptionMap ) when is_map( HttpOptionMap ) ->
	% We have to recursively transform maps into lists (ex: for {ssl,Opts}):
	[ { K, case is_map( V ) of

			   true ->
					maps:to_list( V );

			   false ->
					V

		   end } || { K, V } <- maps:to_list( HttpOptionMap ) ].




% @doc Downloads the file designated by specified URL, in the specified
% directory (under its name in URL), with no specific HTTP options, and returns
% the corresponding full path of that file.
%
% Ex: web_utils:download_file(_Url="https://foobar.org/baz.txt",
%   _TargetDir="/tmp") shall result in a "/tmp/baz.txt" file.
%
% Starts, if needed, the HTTP and SSL supports as a side effect.
%
-spec download_file( url(), any_directory_path() ) -> file_path().
download_file( Url, TargetDir ) ->
	download_file( Url, TargetDir,
				   _HttpOptions=[ { ssl, get_ssl_verify_options() } ] ).



% @doc Downloads the file designated by specified URL, in the specified
% directory (under its name in URL), with specified HTTP options, and returns
% the corresponding full path of that file.
%
% Popular settings are HttpOptions = [{ssl,get_ssl_verify_options()}] to avoid
% any Man-in-the-Middle attack about any target HTTPS server (in addition to TLS
% protection against "casual" eavesdroppers).
%
% Ex: web_utils:download_file(_Url="https://foobar.org/baz.txt",
%   _TargetDir="/tmp", HttpOptions  shall result in a "/tmp/baz.txt" file.
%
% Starts, if needed, the HTTP and SSL supports as a side effect.
%
-spec download_file( url(), any_directory_path(), http_options() ) ->
													file_path().
download_file( Url, TargetDir, HttpOptions ) ->

	% Using only built-in modules:

	#{ scheme := Scheme, path := UrlPath } = case uri_string:parse( Url ) of

		{ error, Error } ->
			throw( { invalid_url, Url, Error } );

		M ->
			M

	end,

	StartOpt = case Scheme of

		"http" ->
			no_ssl;

		"https" ->
			ssl;

		OtherScheme ->
			throw( { unexpected_scheme, OtherScheme } )

	end,

	start( StartOpt ),

	Filename = file_utils:get_last_path_element( UrlPath ),

	FilePath = file_utils:join( TargetDir, Filename ),

	%trace_bridge:debug_fmt( "Downloading '~ts' from '~ts'.",
	%                        [ FilePath, Url ] ),

	case httpc:request( _Method=get, _Req={ Url, _Headers=[] }, HttpOptions,
						_Opts=[ { stream, FilePath } ] ) of

		{ ok, saved_to_file } ->
			FilePath;

		% Ex: {ok, { {"HTTP/1.1", 404, "Not Found" } } }
		{ ok, { { _HTTTP, ErrorCode, Msg }, _RecHeaders, _Body } } ->

			%trace_bridge:error_fmt( "Downloading from '~ts' failed; "
			%   "reason: ~ts, '~ts'.",
			%   [ Url, interpret_http_status_code( ErrorCode ), Msg ] ),

			throw( { download_failed, ErrorCode, Msg, Url } );

		{ error, Reason } ->
			throw( { download_failed, Reason, Url } )

	end.



% @doc Stops the HTTP support.
stop() ->

	% Maybe not launched, hence not pattern matched:
	ssl:stop(),

	ok = inets:stop().



% SSL-related operations.


% @doc Returns default SSL options regarding the verification of remote peers
% for HTTPS connections.
%
% See get_ssl_verify_options/1 for more information.
%
-spec get_ssl_verify_options() -> ssl_options().
get_ssl_verify_options() ->
	get_ssl_verify_options( enable ).



% @doc Returns SSL options regarding the verification of remote peers for HTTPS
% connections:
%
% - if the switch is specified to 'disable', this peer will not be verified
% (exposing the program to a man-in-the-middle attack)
%
% - if the switch is specified to 'enable', the system DER-encoded certificates
% are used (see https://erlang.org/doc/man/ssl.html#type-cert) and trusted in
% order to check peers, so that not only the TLS protection against "casual"
% eavesdroppers applies, but also, here, the one against any Man-in-the-Middle
% (so we check that we indeed interact safely with the *expected* server)
%
-spec get_ssl_verify_options( activation_switch() ) -> ssl_options().
get_ssl_verify_options( _Switch=enable ) ->

  MatchFun = public_key:pkix_verify_hostname_match_fun( https ),

  % Apparently httpc expects list_options(), not map_options():

  %#{ verify => verify_peer,
  %   cacertfile => "/etc/ssl/certs/ca-certificates.crt",
  %   depth => 3,
  %   customize_hostname_check => [ { match_fun, MatchFun } ] };

  [ { verify, verify_peer },
	{ cacertfile, "/etc/ssl/certs/ca-certificates.crt" },
	{ depth, 3 },
	{ customize_hostname_check, [ { match_fun, MatchFun } ] } ];


get_ssl_verify_options( _Switch=disable ) ->

	% Apparently httpc expects list_options(), not map_options():

	%#{ verify => verify_none }.

	[ { verify, verify_none } ].



% @doc Returns a Microsoft Azure instance information based on specified
% settings.
%
-spec get_azure_instance_information( azure_instance_key(),
						azure_instance_location() ) -> azure_instance_info().
get_azure_instance_information( InstKey, InstLoc ) ->
	#azure_instance_info{
		instance_key=text_utils:ensure_binary( InstKey ),
		instance_location=text_utils:ensure_binary( InstLoc ) }.



% @doc Returns a textual description of the specified cloud instance.
-spec cloud_instance_info_to_string( cloud_instance_info() ) -> ustring().
cloud_instance_info_to_string( #azure_instance_info{
									instance_key=_InstKey,
									instance_location=InstLoc } ) ->
	% Not disclosing of the key here:
	text_utils:format( "Microsoft Azure instance located in '~ts'",
					   [ InstLoc ] ).
