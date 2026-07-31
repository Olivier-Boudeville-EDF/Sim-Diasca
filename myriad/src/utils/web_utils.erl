% Copyright (C) 2019-2026 Olivier Boudeville
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

-module(web_utils).

-moduledoc """
Gathering of services for **web-related** uses, notably for **HTML generation**
or **HTTP/HTTPS management**, and also providing a **minimalist webserver**,
which is HTTP 1.1 compliant (as defined in RFC 2616).

See `web_utils_test.erl` for the corresponding test.

See also the `rest_utils` module.
""".



% Implementation notes:
%
% The functions based on an HTTP client (e.g. request/6, get/3, post/3, post/4,
% post/5, download_file/2) rely here on the Erlang-native 'httpc' module, a
% fairly low-level, basic HTTP/1.1.
%
% More advanced needs may rely instead on Gun or Shotgun. Refer to LEEC
% (https://github.com/Olivier-Boudeville/Ceylan-LEEC in leec_api.erl) for an
% example thereof.



-doc """
Tells whether the TLS (formerly SSL) support is needed (typically for the
`https` protocol).
""".
-type ssl_opt() :: 'no_ssl' | 'ssl'.



-type uri() :: ustring().

-type bin_uri() :: bin_string().

-type any_uri() :: uri() | bin_uri().


-type url() :: uri().

-type bin_url() :: bin_uri().

-type any_url() :: url() | bin_url().


-doc """
The possible protocols (schemes) for an URL (use `uri_string:parse/1` to extract
it).
""".
-type protocol_type() :: 'http' | 'https' | 'ftp'.



-doc "Path of an URL (e.g. `access/login`).".
-type path() :: ustring().


% For the (deprecated) url_info record:
-include("web_utils.hrl").


-doc "Full information about an URL (prefer `uri_string:uri_map/0` now).".
-type url_info() :: #url_info{}.



-doc """
The body of a HTTP message.

The binary form is strongly recommended (more compact, and, more importantly, a
lot less problematic regarding encodings).
""".
-type body() :: string_body() | bin_body().



-doc "A body as a plain string (beware to encodings).".
-type string_body() :: ustring().



-doc "A body in binary form, which is the recommended one.".
-type bin_body() :: binary().



-doc "Encoded in JSON.".
-type json_body() :: body().


-type content_type() :: ustring().

-type old_style_options() :: [ { Field :: ustring(), Value :: ustring() } ].

-type new_style_options() :: type_utils:map( bin_string(), bin_string() ).



-doc """
Headers, expressed as a list.

Example: `{"User-Agent", "Godzilla The Mighty"}`.
""".
-type headers_as_list() :: old_style_options().



-type headers_httpc_style() :: headers_as_list().


-type headers_as_maps() :: new_style_options().

-type headers() :: headers_as_list() | headers_as_maps().


% So that it can be further refined later:
-doc """
The key designating a client option.

Refer to the properties in <https://www.erlang.org/doc/apps/inets/httpc.html>
for the many accepted client options.
""".
-type client_option_key() :: atom().

-type client_option_value() :: term().


-doc """
An HTTP client option.

Refer to the properties in <https://www.erlang.org/doc/apps/inets/httpc.html>
for the many accepted client options.

For example `{autoredirect, boolean()}`.
""".
% Not exported yet: httpc:http_option().
-type client_option() :: { client_option_key(), client_option_value() }.

-type client_option_map() ::
    table( client_option_key(), client_option_value() ).

-type client_options() :: [ client_option() ] | client_option_map().

-type ssl_options() :: list_table:tagged_table(). % Cannot be a map.


-type request_result() :: { http_status_code(), headers_as_maps(), bin_body() }
                        | { 'error', basic_utils:error_reason() }.


-type location() :: atom().

-type nonce() :: bin_string().



-doc """
A media type (formerly known as "MIME type").

For example `"audio/ogg"`.

Refer to <https://en.wikipedia.org/wiki/Media_type>.
""".
-type media_type() :: unicode:chardata().


-doc """
Specifies, for a given file extension, the corresponding media type to declare.

For example `{"htm","text/html"}`.

See also `/etc/mime.types`.
""".
-type mime_mapping() :: { extension(), media_type() }.



-doc "Describes a class of usage for MIME types.".
-type mime_usage() ::
    'text'
  | 'javascript'
  | 'data' % Application / Data
  | 'images'
  | 'audio'
  | 'video'
  | 'fonts'
  | 'api' % Web / API
  | 'base'. % for common web usages (MDN / IANA); includes all previous usages



-doc """
An HTML element, typically with markups.

For example `"<p>Hello!</p>"`.
""".
-type html_element() :: any_string().



-doc "There are five classes defined by the standard for HTTP status codes.".
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



-doc "A status code returned by an HTTP operation.".
-type http_status_code() :: non_neg_integer().



% Webserver-related section.

-doc "The PID of a webserver instance.".
-type server_pid() :: pid().


-doc "The profile of a webserver instance (less ambiguous than a server PID).".
-type server_profile() :: atom().


-doc "The full identifier of a webserver instance.".
-type server_id() ::

    % With no server profile specified:
    server_pid() | { BindAddress :: ip_address(), tcp_port() }

    % If having specified a profile:
  | { BindAddress :: ip_address(), tcp_port(), server_profile() }.



% So that it can be further refined later:
-doc """
The key designating a server option.

Refer to the properties in <https://www.erlang.org/doc/apps/inets/httpd.html>
for the many accepted server options.
""".
-type server_option_key() :: atom().

-type server_option_value() :: term().


-doc """
An HTTP server option.

Refer to the properties in <https://www.erlang.org/doc/apps/inets/httpc.html>
for the many accepted client options.

For example:
- `{directory_index, ["index.html" ]}`
- `{modules, [mod_alias, mod_get, mod_head, mod_log]}`
- `{error_log, "error.log"}`
- `{transfer_log, "access.log"}`
- `{mime_types, [{"html", "text/html"}, {"txt", "text/plain"}]}`
""".
-type server_option() :: { server_option_key(), server_option_value() }.




% Cloud-related section.


-doc "A provider of cloud services.".
-type cloud_provider() :: 'azure' | 'aws' | 'google_cloud'.


-doc """
The full, readily usable endpoint of a given service.

For example
`<<"https://francecentral.api.cognitive.microsoft.com/sts/v1.0/issuetoken">>`.
""".
-type service_endpoint() :: bin_string().



-doc """
The API endpoint (with no deployment-related prefix such as
`https://francecentral.`) of a given service.

For example `<<"api.cognitive.microsoft.com/sts/v1.0/issuetoken">>`.
""".
-type api_endpoint() :: bin_string().



-doc """
The key of an instance hosted by a cloud provider.

For example `<<"a59c98302e7f4a22be4b355125df8e9">>`.
""".
-type instance_key() :: bin_string().



-doc "Information regarding an instance hosted by a cloud provider.".
-type cloud_instance_info() :: azure_instance_info().



-doc """
Information regarding an instance hosted by the Microsoft Azure cloud provider.
""".
-type azure_instance_info() :: #azure_instance_info{}.



-doc """
The key of a Microsoft Azure instance.

For example `<<"a59c98302e7f4a22be4b355125df8e9">>`.
""".
-type azure_instance_key() :: instance_key().



-doc """
The location of a Microsoft Azure instance.

For example `<<"francecentral">>`.
""".
-type azure_instance_location() :: bin_string().



-export_type([ ssl_opt/0,
               url/0, bin_url/0, any_url/0,
               uri/0, bin_uri/0, any_uri/0,
               protocol_type/0, path/0, url_info/0,

               body/0, string_body/0, bin_body/0, json_body/0,
               headers/0,

               client_option_key/0, client_option_value/0,
               client_option/0, client_option_map/0, client_options/0,
               ssl_options/0,

               location/0, nonce/0,

               media_type/0,
               html_element/0, http_status_class/0, http_status_code/0,

               server_pid/0, server_profile/0, server_id/0,
               server_option_key/0, server_option_value/0, server_option/0,


               cloud_provider/0,
               service_endpoint/0, api_endpoint/0,
               instance_key/0, cloud_instance_info/0,

               azure_instance_info/0, azure_instance_key/0,
               azure_instance_location/0 ]).


% HTTP-related section:

% URL subsection:
-export([ encode_as_url/1, encode_element_as_url/1, escape_as_url/1,
          get_last_path_element/1,

          % Deprecated in favor of the standard uri_string module:
          url_info_to_string/1, string_to_url_info/1, string_to_uri_map/1 ]).


% HTML-related section:
-export([ get_ordered_list/1, get_unordered_list/1,

          escape_as_html_content/1, escape_term_as_html_content/1,

          get_http_status_class/1, http_status_class_to_string/1,
          interpret_http_status_code/1 ]).


% HTTP-related operations:
-export([ start/0, start/1,
          request/6, get/3, post/3, post/4, post/5,
          download_file/2, download_file/3,
          stop/0 ]).


% TLS-related operations:
-export([ get_ssl_verify_options/0, get_ssl_verify_options/1 ]).


% Webserver-related operations:
-export([ start_server/0, start_server/5, start_server/6, stop_server/1,
          get_mime_mappings/1 ]).


% Cloud-related operations:
-export([ get_azure_instance_information/2, cloud_instance_info_to_string/1 ]).


-define( default_content_type, "text/html; charset=UTF-8" ).



% Type shorthands:

-type activation_switch() :: basic_utils:activation_switch().

-type option_list() :: option_list:option_list().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type any_directory_path() :: file_utils:any_directory_path().
-type file_path() :: file_utils:file_path().
-type file_name() :: file_utils:file_name().
-type extension() :: file_utils:extension().

-type ip_address_spec() :: net_utils:tcp_port().
-type tcp_port() :: net_utils:tcp_port().
-type ip_address() :: net_utils:ip_address().

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
% See also http://www.javascripter.net/faq/accentedcharacters.htm.



-doc """
Encodes the specified list of {Key, Value} pairs so that it can used into an
URL.

Full example:
```
inets:start(),
httpc:request(post, {"http://localhost:3000/foo", [],
 "application/x-www-form-urlencoded",
 encode_as_url([{"username", "bob"}, {"password", "123456"}])}, [], []).
```

Directly inspired from
[http://stackoverflow.com/questions/114196/url-encode-in-erlang>]

See also `escape_as_url/1` for some more specific uses.
""".
-spec encode_as_url( option_list() ) -> ustring().
encode_as_url( OptionList ) ->
    encode_as_url( OptionList, _Acc=[] ).

encode_as_url( _OptionList=[], Acc ) ->
    Acc;

% First entry:
encode_as_url( [ { Key, Value } | T ], _Acc=[] ) ->
    encode_as_url( T,
        encode_element_as_url( Key ) ++ "=" ++ encode_element_as_url( Value ) );

encode_as_url( [ { Key, Value } | T ], Acc ) ->
    encode_as_url( T, Acc ++ "&" ++ encode_element_as_url( Key ) ++ "="
                          ++ encode_element_as_url( Value ) ).



-doc "Encodes specified element so that it can be used in an URL.".
-spec encode_element_as_url( ustring() ) -> ustring().
encode_element_as_url( E ) ->
    % They seem to produce quite similar results in our few test cases:
    edoc_lib:escape_uri( E ).
    %encode_uri_rfc3986:encode( E ).



-doc """
Escapes the specified list of `{Key,Value}` pairs so that it can used into some
URL.

Note: apparently useful only for quite specific websites; `encode_as_url/1`
should be preferred in most cases.
""".
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



-doc "Escapes the specified element so that it can be used in some URL.".
-spec escape_key( option_list:key() ) -> ustring().
escape_key( Key ) when is_atom( Key ) ->
    text_utils:atom_to_string( Key ).


-spec escape_value( ustring() ) -> ustring().
escape_value( String ) ->
    R = lists:flatten( [ escape_char( C ) || C <- String ] ),
    %trace_utils:debug_fmt( "'~ts' became '~ts'.", [ String, R ] ),
    R.



-doc """
Escapes the specified character.

Alphanumerical characters are left as are.
""".
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



-doc """
Returns the last element, the final path "filename" pointed by specified URL.

For example:
```
"hello.txt" = web_utils:get_last_path_element(
   "http://www.foobar.org/baz/hello.txt")
```
""".
-spec get_last_path_element( url() ) -> file_name().
get_last_path_element( Url ) ->
    % Hackish yet working perfectly:
    filename:basename( Url ).



-doc "Returns a string describing the specified URL information.".
-spec url_info_to_string( url_info() ) -> ustring().
url_info_to_string( #url_info{ protocol=Protocol, host_identifier=Host,
                               port=Port, path=Path } ) ->
    text_utils:format( "~ts://~ts:~B/~ts",
        [ Protocol, net_utils:host_to_string( Host ), Port, Path ] ).



-doc """
Decodes the specified string into an `url_info` record, by extracting protocol
(scheme), host, port and path information.

Note that other information (fragment, query, userinfo) will be ignored and
lost.

Note: using `string_to_uri_map/1` might be a more complete option; the current
function remains mostly for backward compatibility.
""".
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

    % Hope for the best:
    SchemeStr = text_utils:string_to_atom( Scheme ),

    #url_info{ protocol=SchemeStr, host_identifier=Host, port=MaybePort,
               path=Path }.



-doc """
Decodes the specified string into an URI map, by extracting all relevant
information: protocol (scheme), user information, host, port, path and fragment.

Throws an exception on failure.
""".
-spec string_to_uri_map( ustring() ) -> uri_string:uri_map().
string_to_uri_map( String ) ->

    case uri_string:parse( String ) of

        { error, ReasonAtom, ReasonTerm } ->
            throw( { uri_parsing_failed, String, ReasonAtom, ReasonTerm } );

        URIMap ->
            URIMap

    end.




% HTML-related section.


-doc """
Returns the HTML code of an ordered (numbered bullets) list corresponding to
specified list of elements.
""".
-spec get_ordered_list( [ html_element() ] ) -> html_element().
get_ordered_list( Elements ) ->

    HTMLElems = [ text_utils:format( "    <li>~ts</li>~n", [ E ] )
                    || E <- Elements ],

    text_utils:format( "  <ol>~n~ts  </ol>~n", [ lists:flatten( HTMLElems ) ] ).



-doc """
Returns the HTML code of an unordered list corresponding to specified list of
elements.
""".
-spec get_unordered_list( [ html_element() ] ) -> html_element().
get_unordered_list( Elements ) ->

    HTMLElems = [ text_utils:format( "    <li>~ts</li>~n", [ E ] )
                    || E <- Elements ],

    text_utils:format( "  <ul>~n~ts  </ul>~n", [ lists:flatten( HTMLElems ) ] ).



-doc """
Escapes the specified text, so that it can be included safely as an HTML
content.

Returns an HTML element, as a plain string.
""".
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
% Newly-introduced (R25) uri_string:quote/1 and uri_string:unquote might be
% used.
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



-doc """
Escapes the specified term (most probably non-string), so that it can be
included safely as an HTML content.
""".
-spec escape_term_as_html_content( term() ) -> html_element().
escape_term_as_html_content( Term ) ->
    escape_as_html_content( text_utils:term_to_string( Term ) ).



-doc """
Returns the status class (if any) corresponding to the specified HTTP status
code.
""".
-spec get_http_status_class( http_status_code() ) ->
                                option( http_status_class() ).
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



-doc "Returns a textual description of the specified HTTP status class.".
-spec http_status_class_to_string( option( http_status_class() ) ) -> ustring().
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



-doc """
Returns a textual description of the specified HTTP code.

Source is [https://en.wikipedia.org/wiki/List_of_HTTP_status_codes].
""".
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


-doc """
Starts the HTTP support, with default settings.

Does not fail if already started, throws an exception in case of unrecoverable
error.
""".
-spec start() -> void().
start() ->
    start( _Option=no_ssl ).



-doc """
Starts the HTTP support, with specified settings.

Does not fail if already started, throws an exception in case of unrecoverable
error.
""".
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
                        "following error for SSL: ~p.", [ SSLReason ] ),
                    throw( { start_failed, ssl, SSLReason } )

            end

    end.



-doc """
Sends a (synchronous) HTTP/1.1 client request (GET or POST).

The HTTP support (possibly with SSL if needed) must be started.

For HTTPS requests, we recommend that the ClientOptions include a `{ssl,
web_utils:get_ssl_verify_options()}` pair.

For more advanced uses (e.g. re-using of permanent connections, HTTP/2, etc.),
consider relying on Gun or Shotgun.
""".
-spec request( method(), uri(), headers(), client_options(),
               option( bin_body() ), option( content_type() ) ) ->
                                            request_result().
request( _Method=get, Uri, Headers, ClientOptions, _MaybeBody=undefined,
         _MaybeContentType=undefined ) ->
    get( Uri, Headers, ClientOptions );

request( _Method=get, _Uri, _Headers, _ClientOptions, MaybeBody,
         MaybeContentType ) ->
    throw( { invalid_get_request, { body, MaybeBody },
             { content_type, MaybeContentType } } );

request( _Method=post, Uri, Headers, ClientOptions, MaybeBody,
         MaybeContentType ) ->
    post( Uri, Headers, ClientOptions, MaybeBody, MaybeContentType );

% Not supported (yet): head | put | trace | options | delete | patch:
request( Method, Uri, _Headers, _ClientOptions, _MaybeBody,
         _MaybeContentType ) ->
    throw( { invalid_method, Method, Uri } ).



-doc """
Sends a (synchronous) HTTP/1.1 client GET request.

The HTTP support (possibly with SSL if needed) must be started.

For HTTPS requests, we recommend that the ClientOptions include a `{ssl,
web_utils:get_ssl_verify_options()}` pair.

For more advanced uses (e.g. re-using of permanent connections, HTTP/2, etc.),
consider relying on Gun or Shotgun.
""".
-spec get( uri(), headers(), client_options() ) -> request_result().
get( Uri, Headers, ClientOptions ) ->

    cond_utils:if_defined( myriad_debug_web_exchanges,
        trace_bridge:debug_fmt( "[~w] GET request to URI "
            "'~ts', with following headers:~n  ~p~nand "
            "HTTP options:~n  ~p.", [ self(), Uri, Headers, ClientOptions ] ) ),

    HeadersForHttpc = to_httpc_headers( Headers ),

    % Any content-type expected in headers, and no specific body for GET:
    Req = { Uri, HeadersForHttpc },

    ClientOptionsForHttpc = to_httpc_options( ClientOptions ),

    % Wanting the resulting body (as a binary rather than as a plain string),
    % headers, and the entire status line:
    %
    Options = [ { full_result, true }, { body_format, binary } ],

    cond_utils:if_defined( myriad_debug_web_exchanges,
        trace_bridge:debug_fmt( "[~w] Actual parameters of the httpc GET "
            "request:~n - request: ~p~n - HTTP options: ~p~n - options: ~p~n",
            [ self(), Req, ClientOptionsForHttpc, Options ] ) ),

    case httpc:request( _Method=get, Req, ClientOptionsForHttpc, Options ) of

        % For example HttpVersion="HTTP/1.1", StatusCode=200, ReqReason="OK".
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



-doc """
Sends a (synchronous, body-less) HTTP/1.1 client POST request.

The HTTP support (possibly with SSL if needed) must be started.

For HTTPS requests, we recommend that the ClientOptions include a `{ssl,
web_utils:get_ssl_verify_options()}` pair.

For more advanced uses (e.g. re-using of permanent connections, HTTP/2, etc.),
consider relying on Gun or Shotgun.
""".
-spec post( uri(), headers(), client_options() ) -> request_result().
post( Uri, Headers, ClientOptions ) ->
    post( Uri, Headers, ClientOptions, _MaybeBody=undefined ).



-doc """
Sends a (synchronous) HTTP/1.1 client POST request.

If a body is specified yet no content-type is set, ?default_content_type will be
used. To avoid encoding issues, we strongly recommend to pass binary bodies
rather than string ones.

The HTTP support (possibly with SSL if needed) must be started.

For HTTPS requests, we recommend that the ClientOptions include a `{ssl,
web_utils:get_ssl_verify_options()}` pair.

For more advanced uses (e.g. re-using of permanent connections, HTTP/2, etc.),
consider relying on Gun or Shotgun.
""".
-spec post( uri(), headers(), client_options(), option( body() ) ) ->
                                request_result().
post( Uri, Headers, ClientOptions, MaybeBody ) ->
    post( Uri, Headers, ClientOptions, MaybeBody, _MaybeContentType=undefined ).



-doc """
Sends a (synchronous) HTTP/1.1 client POST request.

If a body is specified yet no content-type is set, `?default_content_type` will
be used. To avoid encoding issues, we strongly recommend to pass binary bodies
rather than string ones.

The HTTP support (possibly with SSL if needed) must be started.

For HTTPS requests, we recommend that the ClientOptions include a `{ssl,
web_utils:get_ssl_verify_options()}` pair.

For more advanced uses (e.g. re-using of permanent connections, HTTP/2, etc.),
consider relying on Gun or Shotgun.
""".
-spec post( uri(), headers(), client_options(), option( body() ),
            option( content_type() ) ) -> request_result().
post( Uri, Headers, ClientOptions, MaybeBody, MaybeContentType ) ->

    cond_utils:if_defined( myriad_debug_web_exchanges,
        trace_bridge:debug_fmt( "[~w] POST request to URI "
            "'~ts', with following headers:~n  ~p~nHTTP options:~n  ~p~n"
            "Body: ~p~nContent-type: ~ts",
            [ self(), Uri, Headers, ClientOptions, MaybeBody,
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

    ClientOptionsForHttpc = to_httpc_options( ClientOptions ),

    % Wanting the resulting body (as a binary rather than as a plain string),
    % headers, and the entire status line:
    %
    Options = [ { full_result, true }, { body_format, binary } ],

    cond_utils:if_defined( myriad_debug_web_exchanges,
        trace_bridge:debug_fmt( "[~w] Actual parameters of the httpc POST "
            "request:~n - request: ~p~n - HTTP options: ~p~n - options: ~p~n",
            [ self(), Req, ClientOptionsForHttpc, Options ] ) ),

    case httpc:request( _Method=post, Req, ClientOptionsForHttpc, Options ) of

        % For example HttpVersion="HTTP/1.1", StatusCode=200, ReqReason="OK".
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



-doc "Converts the specified headers into suitable ones for httpc.".
-spec to_httpc_headers( headers() ) -> headers_httpc_style().
to_httpc_headers( Headers ) when is_list( Headers ) ->
    Headers;

to_httpc_headers( Headers ) when is_map( Headers ) ->
    [ { text_utils:binary_to_string( K ), text_utils:binary_to_string( V ) }
            || { K, V } <- maps:to_list( Headers ) ].



-doc "Converts the specified httpc headers into map-based ones.".
-spec from_httpc_headers( headers_httpc_style() ) -> headers_as_maps().
from_httpc_headers( Headers ) ->
    maps:from_list( [ { text_utils:string_to_binary( K ),
                        text_utils:string_to_binary( V ) }
                                || { K, V } <- Headers ] ).



-doc "Returns client options that are suitable for `httpc`.".
-spec to_httpc_options( client_options() ) -> [ client_option() ].
to_httpc_options( ClientOptions ) when is_list( ClientOptions ) ->
    % We have to transform maps into lists (e.g. for {ssl,Opts}):
    [ { K, case is_map( V ) of

                true ->
                    maps:to_list( V );

                _False ->
                    V

           end } || { K, V } <- ClientOptions ];

to_httpc_options( ClientOptionMap ) when is_map( ClientOptionMap ) ->
    to_httpc_options( table:enumerate( ClientOptionMap ) ).



-doc """
Downloads the file designated by the specified URL, in the specified directory
(under its name in URL), with no specific HTTP options, and returns the
corresponding full path of that file.

For example:
```
web_utils:download_file(_Url="https://foobar.org/baz.txt",
  _TargetDir="/tmp") shall result in a "/tmp/baz.txt" file.
```

Starts, if needed, the HTTP and SSL supports as a side effect.
""".
-spec download_file( url(), any_directory_path() ) -> file_path().
download_file( Url, TargetDir ) ->
    download_file( Url, TargetDir,
                   _ClientOptions=[ { ssl, get_ssl_verify_options() } ] ).



-doc """
Downloads the file designated by specified URL, in the specified directory
(under its name in URL), with specified HTTP options, and returns the
corresponding full path of that file.

Popular settings are `ClientOptions = [{ssl,get_ssl_verify_options()}]` to avoid
any Man-in-the-Middle attack about any target HTTPS server (in addition to TLS
protection against "casual" eavesdroppers).

For example:
```
web_utils:download_file(_Url="https://foobar.org/baz.txt",
  _TargetDir="/tmp", ClientOptions  shall result in a "/tmp/baz.txt" file.
```

Starts, if needed, the HTTP and SSL supports as a side effect.
""".
-spec download_file( url(), any_directory_path(), client_options() ) ->
                                                    file_path().
download_file( Url, TargetDir, ClientOptions ) ->

    % Using only built-in modules:

    #{ scheme := Scheme, path := UrlPath } = case uri_string:parse( Url ) of

        { error, AtomReason, ExtraInfo } ->
            throw( { invalid_url, Url, AtomReason, ExtraInfo } );

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

    case httpc:request( _Method=get, _Req={ Url, _Headers=[] }, ClientOptions,
                        _Opts=[ { stream, FilePath } ] ) of

        { ok, saved_to_file } ->
            FilePath;

        % For example {ok, { {"HTTP/1.1", 404, "Not Found" } } }
        { ok, { { _HTTTP, ErrorCode, Msg }, _RecHeaders, _Body } } ->

            %trace_bridge:error_fmt( "Downloading from '~ts' failed; "
            %   "reason: ~ts, '~ts'.",
            %   [ Url, interpret_http_status_code( ErrorCode ), Msg ] ),

            throw( { download_failed, ErrorCode, Msg, Url } );

        { error, Reason } ->
            throw( { download_failed, Reason, Url } )

    end.



-doc "Stops the HTTP support.".
-spec stop() -> basic_utils:base_status().
stop() ->

    % Maybe not launched, hence not pattern matched:
    ssl:stop(),

    case inets:stop() of

        ok ->
            ok;

        % Anyway we prefer not crashing on shutdown:
        Error ->
            cond_utils:if_defined( myriad_debug_web_exchanges,
                throw( { inets_stop_failed, Error } ),
                Error )

    end.




% TLS-related operations.


-doc """
Returns default SSL (actually TLS) options regarding the verification of remote
peers for HTTPS connections.

See `get_ssl_verify_options/1` for more information.
""".
-spec get_ssl_verify_options() -> ssl_options().
get_ssl_verify_options() ->
    get_ssl_verify_options( enable ).



-doc """
Returns SSL (actually TLS) options regarding the verification of remote peers
for HTTPS connections:

- if the switch is specified to `disable`, this peer will not be verified
(exposing the program to a man-in-the-middle attack)

- if the switch is specified to `enable`, the system DER-encoded certificates
are used (see [https://erlang.org/doc/man/ssl.html#type-cert]) and trusted in
order to check peers, so that not only the TLS protection against "casual"
eavesdroppers applies, but also, here, the one against any Man-in-the-Middle (so
we check that we indeed interact safely with the *expected* server)
""".
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




% Webserver-related operations.



-doc """
Starts a minimalist, local HTTP webserver on the Myriad default port (see the
`default_webserver_port` define), serving - only on the loopback, i.e. only for
the local host - the content (typically HTML/CSS/JS) found from the current
directory.

Useful for example to server local content, whereas browsers now refuse, based
on CORS, any kind of access to local content (typically with: "Cross-Origin
Request Blocked: The Same Origin Policy disallows reading the remote resource at
file://xxx (Reason: CORS request not http)").

Akin to `python -m http.server`.
""".
-spec start_server() -> server_id().
start_server() ->

    CurrentDir = file_utils:get_current_directory(),

    start_server( _SrvName=?default_webserver_name,
        _LocalBindAddress=localhost, _TCPPort=?default_webserver_port,
        _SrvRootDir=CurrentDir, _DocDir=CurrentDir ).



-doc """
Starts a minimalist, local HTTP webserver directly with the specified options.

Useful for example to server local content, whereas browsers now refuse, based
on CORS, any kind of access to local content (typically with: "Cross-Origin
Request Blocked: The Same Origin Policy disallows reading the remote resource at
file://xxx (Reason: CORS request not http)").

Lowest-level start function. At least the `port`, `server_root`, `document_root`
and probably `mime_types` properties shall be specified in the server options.

Akin to `python -m http.server`.
""".
-spec start_server( [ server_option() ] ) -> server_id().
start_server( SrvOpts ) ->

    cond_utils:if_defined( myriad_debug_webserver,
        trace_utils:debug_fmt(
            "Starting a webserver with the following options:~n ~p",
            [ SrvOpts ] ) ),

    % Prerequisite; no extra dependency (like Cowboy) involved:
    % (either 'ok' or '{error,{already_started,inets}}')
    %
    inets:start(),

    case inets:start( httpd, SrvOpts ) of

        { ok, SrvPid } ->
            case list_table:lookup_entry( _Key=profile, SrvOpts ) of

                key_not_found ->
                    % No profile was specified, so PID (or {BindAddress,
                    % TCPPort}) will be sufficient:
                    %
                    SrvPid;

                { value, SrvProfile } ->

                    BindIPAddress = case list_table:lookup_entry( bind_address,
                                                                SrvOpts ) of
                        key_not_found ->
                            any; % httpd default

                        { value, Address } ->
                            Address

                    end,

                    TCPPort = case list_table:lookup_entry( port, SrvOpts ) of

                        key_not_found ->
                            % Mandatory:
                            throw( no_tcp_port_defined );

                        { value, Port } ->
                            Port

                    end,

                    { BindIPAddress, TCPPort, SrvProfile }

            end;

        { error, Reason } ->
            throw( { webserver_start_failed, Reason, SrvOpts } )

    end.




-doc """
Starts a minimalist, local HTTP webserver on the specified TCP port, bound to
the specified IP address, serving the content (typically HTML/CSS/JS) found from
the specified "document root" directory (the root of the public content exposed
through the HTTP URLs).


The "server root" directory is the internal work directory of the webserver (for
configuration files, logs, etc.).

Generally it is better to have distinct server and document root directories.

Useful for example to server local content, whereas browsers now refuse, based
on CORS, any kind of access to local content (typically with: "Cross-Origin
Request Blocked: The Same Origin Policy disallows reading the remote resource at
file://xxx (Reason: CORS request not http)").

Akin to a parametrised `python -m http.server`.
""".
-spec start_server( ustring(), ip_address_spec(), tcp_port(),
        any_directory_path(), any_directory_path() ) -> server_id().
start_server( SrvName, BindIPAddressSpec, TCPPort,
              AnySrvRootDir, AnyDocRootDir ) ->
    start_server( SrvName, BindIPAddressSpec, TCPPort,
        AnySrvRootDir, AnyDocRootDir, _SrvProfile=myriad_http ).




-doc """
Starts a minimalist, local HTTP webserver on the specified TCP port, bound to
the specified IP address, using the specified profile, serving the content
(typically HTML/CSS/JS) found from the specified "document root" directory (the
root of the public content exposed through the HTTP URLs), with basic MIME
types.

The "server root" directory is the internal work directory of the webserver (for
configuration files, logs, etc.).

Generally it is better to have distinct server and document root directories.

Useful for example to server local content, whereas browsers now refuse, based
on CORS, any kind of access to local content (typically with: "Cross-Origin
Request Blocked: The Same Origin Policy disallows reading the remote resource at
file://xxx (Reason: CORS request not http)").

Akin to a parametrised `python -m http.server`.
""".
-spec start_server( ustring(), ip_address_spec(), tcp_port(),
        any_directory_path(), any_directory_path(), server_profile() ) ->
                                                server_id().
start_server( SrvName, BindIPAddressSpec, TCPPort,
              AnySrvRootDir, AnyDocRootDir, SrvProfile ) ->

    BindIPAddress = net_utils:spec_to_ip( BindIPAddressSpec ),
    net_utils:check_port( TCPPort ),

    SrvRootDir = text_utils:ensure_string( AnySrvRootDir ),

    DocRootDir = text_utils:ensure_string( AnyDocRootDir ),

    is_atom( SrvProfile ) orelse
        throw( { invalid_server_profile, SrvProfile } ),

    % Directories must be plain strings (otherwise: "internal server error"):

    file_utils:is_existing_directory_or_link( SrvRootDir ) orelse
        throw( { non_existing_server_root, SrvRootDir } ),

    file_utils:is_existing_directory_or_link( DocRootDir ) orelse
        throw( { non_existing_document_root, DocRootDir } ),

    % File names in UTF8 can be forced with 'erl +fnu':
    % (commented-out, as equal to 'latin1' in CI, as not using our makefiles)
    %utf8 = file:native_name_encoding(),


    MimeMappings = get_mime_mappings( base ),

    % To avoid an eaddrinuse error, triggered even if a previous server on that
    % port is already dead (the kernel releasing ports asynchronously):
    %
    SocketOpts = [ { reuseaddr, true } ],

    start_server( _SrvOpts = [ { profile, SrvProfile },
                               { port, TCPPort },
                               { server_name, SrvName },
                               { server_root, SrvRootDir },
                               { document_root, DocRootDir },

                               % Not sufficient to solve the problem of
                               % returning UTF8 filenames (e.g. containing "à"):
                               %
                               { file_charset, "utf-8" },
                               { directory, { "/", DocRootDir } },

                               { bind_address, BindIPAddress },
                               { mime_types, MimeMappings },
                               { socket_opts, SocketOpts } ] ).



-doc "Stops the specified minimalist, local HTTP webserver.".
-spec stop_server( server_id() ) -> void().
stop_server( SrvId ) ->

    cond_utils:if_defined( myriad_debug_webserver,
        trace_utils:debug_fmt( "Stopping webserver identified by ~w.",
                               [ SrvId ] ) ),

    case inets:stop( httpd, SrvId ) of

        ok ->
            ok;

        { error, Reason } ->
            throw( { webserver_stop_failed, Reason, SrvId } )

    end.



-doc """
Returns a list of the MIME mappings that correspondin to the specified usage.

Note that some mappings belong to multiple usages.
""".
-spec get_mime_mappings( mime_usage() ) -> [ mime_mapping() ].
get_mime_mappings( _MimeUsage=text ) ->
    [ { Ext, "text/plain" }
        || Ext <- [ "txt", "text", "conf", "def", "list", "log", "in" ] ]
      ++ [ { "html", "text/html" }, { "htm", "text/html" },
            { "css", "text/css" }, { "csv", "text/csv" },
           { "xml", "text/xml" }, { "md", "text/markdown" } ];

get_mime_mappings( _MimeUsage=javascript ) ->
    [ { "js", "text/javascript" }, { "mjs", "text/javascript" } ];

get_mime_mappings( _MimeUsage=data ) ->
    [ { Ext, "application/octet-stream" }
        || Ext <- [ "bin", "exe", "dll", "deb", "dmg", "iso", "img", "msi" ] ]
      ++ [ { "json", "application/json" }, { "jsonld", "application/ld+json" },
           { "xml", "application/xml" }, { "pdf", "application/pdf" },
           { "zip", "application/zip " }, { "gz", "application/gzip" },
           { "tar", "application/x-tar " },
           { "7z", "application/x-7z-compressed" },
           { "rar", "application/x-rar-compressed" },
           { "xls", "application/vnd.ms-excel" },
           { "xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" },
           { "doc", "application/msword" },
           { "docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document" },
           { "ppt", "application/vnd.ms-powerpoint" },
           { "pptx", "application/vnd.openxmlformats-officedocument.presentationml.presentation" },
           { "wasm", "application/wasm" },
           { "webmanifest", "application/manifest+json" } ];

get_mime_mappings( _MimeUsage=images ) ->
    [ { Ext, "image/jpeg" }
        || Ext <- [ "jpeg", "jpg", "jpe" ] ]
      ++ [ { "png", "image/png" }, { "gif", "image/gif" },
           { "svg", "image/svg+xml" }, { "svgz", "image/svg+xml" },
           { "webp", "image/webp" }, { "avif", "image/avif" },
           { "ico", "image/vnd.microsoft.icon" }, { "bmp", "image/bmp" },
           { "tif", "image/tiff" }, { "tiff", "image/tiff" },
           { "apng", "image/apng" } ];

get_mime_mappings( _MimeUsage=audio ) ->
    [ { Ext, "audio/ogg" }
        || Ext <- [ "ogg", "oga", "opus" ] ]
    ++ [ { "mp3", "audio/mpeg" }, { "wav", "audio/wav" },
         { "aac", "audio/aac" }, { "flac", "audio/flac" },
         { "weba", "audio/webm" }, { "mid", "audio/midi" },
         { "midi", "audio/midi" } ];

get_mime_mappings( _MimeUsage=video ) ->
    [ { "mp4", "video/mp4" }, { "webm", "video/webm" }, { "ogv", "video/ogg" },
      { "mpeg", "video/mpeg" }, { "mpg", "video/mpeg" },
      { "avi", "video/x-msvideo" }, { "mov", "video/quicktime" } ];

get_mime_mappings( _MimeUsage=fonts ) ->
    [ { "woff", "font/woff" }, { "woff2", "font/woff2 " },
      { "ttf", "font/ttf" }, { "otf", "font/otf" } ];

get_mime_mappings( _MimeUsage=api ) ->
    [ { "wasm", "application/wasm" },
      { "webmanifest", "application/manifest+json" },
      { "rss", "application/rss+xml" },
      { "graphql", "application/atom+xml" } ];


get_mime_mappings( _MimeUsage=base ) ->
    lists:append( [ get_mime_mappings( U )
        || U <- [ text, javascript, data, images, audio, video,fonts, api ] ] ).




% Cloud-related section.


-doc """
Returns a Microsoft Azure instance information based on the specified settings.
""".
-spec get_azure_instance_information( azure_instance_key(),
                        azure_instance_location() ) -> azure_instance_info().
get_azure_instance_information( InstKey, InstLoc ) ->
    #azure_instance_info{
        instance_key=text_utils:ensure_binary( InstKey ),
        instance_location=text_utils:ensure_binary( InstLoc ) }.



-doc "Returns a textual description of the specified cloud instance.".
-spec cloud_instance_info_to_string( cloud_instance_info() ) -> ustring().
cloud_instance_info_to_string( #azure_instance_info{
                                    %instance_key=InstKey,
                                    instance_location=InstLoc } ) ->
    % No disclosing of the key here:
    text_utils:format( "Microsoft Azure instance located in '~ts'",
                       [ InstLoc ] ).
