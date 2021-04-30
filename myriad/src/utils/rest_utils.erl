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
% Authors: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%		   Samuel Thiriot [samuel (dot) thiriot (at) edf (dot) fr]
%
% Creation date: Tuesday, December 1, 2015.



% Gathering of management facilities for REST architectures (Representational
% State Transfer).
%
% See rest_utils_test.erl for the corresponding test.
%
% See also: web_utils.erl.
%
-module(rest_utils).



% Implementation notes:
%
% We rely here on following prerequisites:
%
% - an HTTP client, namely the built-in one, httpc
% (http://erlang.org/doc/man/httpc.html), with some usage information
% (http://erlang.org/doc/apps/inets/http_client.html)
%
% - the JSON services offered by our json_utils module


-export([ start/0, start/1, stop/0,
		  http_get/1, http_get/2, http_get/4,
		  http_post/1, http_post/2, http_post/4,
		  http_put/1, http_put/2, http_put/4,
		  http_delete/1, http_delete/2, http_delete/4,
		  http_request/1, http_request/2, http_request/3, http_request/5 ]).


% Defines the duration (in milliseconds) to wait before retrying, after a
% connection failed.
%
% A random value between these minimum and maximum values will be used.
%
-define( retry_delay_min, 500 ).
-define( retry_delay_max, 5000 ).



% HTTP/1.1 method:
-type method() :: 'get' | 'head' | 'post' | 'options' | 'connect' | 'trace'
				| 'put' | 'patch' | 'delete'.

% Content type (ex: "text/html;charset=utf-8", "application/json"):
-type content_type() :: ustring().

-type field() :: ustring().

-type value() :: ustring().

-type header() :: { field(), value() }.

-type headers() :: [ header() ].

-type body() :: binary() | ustring().

-type status_code() :: pos_integer().

-type status_line() :: { ustring(), status_code(), ustring() }.

% Type of a request for httpc:request, see http://erlang.org/doc/man/httpc.html:
-type request() :: { web_utils:url(), headers(), content_type(), body() }
				 | { web_utils:url(), headers() }.

-type http_option() :: { atom(), term() }.
-type http_options() :: [ http_option() ].

-type option() :: { atom(), term() }.
-type options() :: [ option() ].


% Type of a result from httpc:request, see http://erlang.org/doc/man/httpc.html:
-type result() :: { status_line(), headers(), body() }
				| { status_code(), body() }
				| reference().


% Context of a REST exchange:
-type context() :: { web_utils:url_info(), headers() }.

-type retries_count() :: basic_utils:count().

-export_type([ ssl_opt/0, method/0, content_type/0, field/0, value/0,
			   header/0, headers/0, body/0, status_code/0, status_line/0,
			   request/0, http_option/0, http_options/0, option/0, options/0,
			   result/0, context/0, retries_count/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type ssl_opt() :: web_utils:ssl_opt().



% Starts the REST service, with default settings.
-spec start() -> void().
start() ->
	start( no_ssl ).


% Starts the REST service.
-spec start( ssl_opt() ) -> json_utils:parser_state().
start( Option ) ->
	web_utils:start( Option ),
	json_utils:start_parser().


% Stops the REST service.
-spec stop() -> void().
stop() ->
	json_utils:stop_parser(),
	web_utils:stop().




% REST requests section.


% Lists all the possible request methods defined by the HTTP/1.1 standard,
% except the 'CONNECT' method that seems not to be part of the function clauses
% appearing in httpc:request:
%
-spec get_supported_http_methods() -> [ method() ].
get_supported_http_methods() ->
	[ get, head, post, options, trace, put, patch, delete ].



% Lists all the supported HTTP/1.1 standard methods whose implementation in
% httpc:request does not allow the Body and (thus) ContentType arguments: they
% must be associated with requests of the form {URL, Headers}.
%
-spec get_no_body_http_methods() -> [ method() ].
get_no_body_http_methods() ->
	[ get, head, options, trace, delete ].



% Lists all the supported HTTP/1.1 standard methods whose implementation in
% httpc:request expects the Body and (thus) ContentType arguments: they must be
% associated with requests of the form {URL, Headers, ContentType, Body}.
%
-spec get_body_allowing_http_methods() -> [ method() ].
get_body_allowing_http_methods() ->
	[ post, put, patch, delete ].



% Shorthands for sending GET HTTP requests:
-spec http_get( request() ) -> { status_code(), term() }.
http_get( Request ) ->
	http_get( Request, _HTTPOpts=[], _Opts=[], _Retries=0 ).


-spec http_get( request(), retries_count() ) -> { status_code(), term() }.
http_get( Request, Retries ) ->
	http_get( Request, _HTTPOpts=[], _Opts=[], Retries ).


-spec http_get( request(), http_options(), options(), retries_count() ) ->
						term().
http_get( Request, HTTPOptions, Options, Retries ) ->
	http_request( get, Request, HTTPOptions, Options, Retries ).


% Shorthands for sending POST HTTP requests:
-spec http_post( request() ) -> { status_code(), term() }.
http_post( Request ) ->
	http_post( Request, _HTTPOpts=[], _Opts=[], _Retries=0 ).


-spec http_post( request(), retries_count() ) -> { status_code(), term() }.
http_post( Request, Retries ) ->
	http_post( Request, _HTTPOpts=[], _Opts=[], Retries ).


-spec http_post( request(), http_options(), options(), retries_count() ) ->
						term().
http_post( Request, HTTPOptions, Options, Retries ) ->
	http_request( post, Request, HTTPOptions, Options, Retries ).



% Shorthands for sending PUT HTTP requests:
-spec http_put( request() ) -> { status_code(), term() }.
http_put( Request ) ->
	http_put( Request, _HTTPOpts=[], _Opts=[], _Retries=0 ).


-spec http_put( request(), retries_count() ) -> { status_code(), term() }.
http_put( Request, Retries ) ->
	http_put( Request, _HTTPOpts=[], _Opts=[], Retries ).


-spec http_put( request(), http_options(), options(), retries_count() ) ->
						term().
http_put( Request, HTTPOptions, Options, Retries ) ->
	http_request( put, Request, HTTPOptions, Options, Retries ).



% Shorthands for sending DELETE HTTP requests:
-spec http_delete( request() ) -> { status_code(), term() }.
http_delete( Request ) ->
	http_delete( Request, _HTTPOpts=[], _Opts=[], _Retries=0 ).


-spec http_delete( request(), retries_count() ) -> { status_code(), term() }.
http_delete( Request, Retries ) ->
	http_delete( Request, _HTTPOpts=[], _Opts=[], Retries ).


-spec http_delete( request(), http_options(), options(), retries_count() ) ->
							term().
http_delete( Request, HTTPOptions, Options, Retries ) ->
	http_request( delete, Request, HTTPOptions, Options, Retries ).



% Another shorthand for sending GET HTTP requests, as suggested by the standard
% 'httpc' module of Erlang:
%
-spec http_request( web_utils:url() ) -> { status_code(), term() }.
http_request( URL ) ->
	http_request( get, { URL, [] }, _HTTPOpts=[], _Opts=[], _Retries=0 ).



% Sends a generic HTTP request.
%
% (Basically just a call to httpc:request/4 surrounded by checking steps)
%
-spec http_request( method(), request() ) -> { status_code(), term() }.
http_request( Method, Request ) ->
	http_request( Method, Request, _HTTPOpts=[], _Opts=[], _Retries=0 ).


-spec http_request( method(), request(), retries_count() ) ->
							{ status_code(), term() }.
http_request( Method, Request, Retries ) ->
	http_request( Method, Request, _HTTPOpts=[], _Opts=[], Retries ).




-spec http_request( method(), request(), http_options(), options(),
					retries_count() ) -> term().
http_request( Method, Request, HTTPOptions, Options, Retries ) ->

	% Note: a null number of retries is managed inside this clause.

	% Checks the HTTP method is a valid one:
	case lists:member( Method, get_supported_http_methods() ) of

		true ->
			ok;

		false when Method =:= connect ->
			throw( connect_method_not_supported_by_httpc );

		false when is_atom( Method ) ->
			throw( { not_a_standard_http_method, Method } );

		_False ->
			throw( { invalid_http_method_specification, Method } )

	end,

	% Checks that the request seems valid:
	check_http_request( Method, Request ),

	% TODO:
	%  - checks the HTTP options look valid
	%  - checks the request options look valid

	MaybeHttpResult = httpc:request( Method, Request, HTTPOptions, Options ),

	case { MaybeHttpResult, Retries } of

		{ { ok, Result }, _ } ->
			return_checked_result( Result );

		{ { error, Reason }, _NoMoreRetries=0 } ->
			trace_utils:error_fmt( "Retries exhausted, HTTP ~p request ~p "
				"(HTTP options: ~p, options: ~p) failed, reason being: ~p.",
				[ Method, Request, HTTPOptions, Options, Reason ] ),
			throw( { http_request_failed, Method, Request, Reason } );

		{ { error, Reason }, _StillRetries } ->

			% We will retry the same query one more time, after a random delay
			% in specified bounds:

			Delay = random_utils:get_random_value( ?retry_delay_min,
												   ?retry_delay_max ),

			trace_utils:warning_fmt( "HTTP ~p request ~p failed (cause: ~p), "
				"retrying after a delay of ~w milliseconds.",
				[ Method, Request, Reason, Delay ] ),

			timer:sleep( Delay ),

			http_request( Method, Request, HTTPOptions, Options, Retries-1 )

	end.



% Converts the Body string of an error message, possibly with a stack trace, to
% a text that is easier to understand, with actual carriage returns.
%
-spec format_body_error( ustring() ) -> ustring().
format_body_error( ContentBody ) ->

	%re:replace( ContentBody, "\\\\n", "\\n", [ global, {return, list} ] ).

	Tokens = string:replace( ContentBody, "\\n \\n ", "", all ),

	NotEmpty = lists:filter( fun( Tok ) -> length( Tok ) > 0 end, Tokens ),

	Formatted = string:join( NotEmpty, "~n" ),

	%trace_utils:debug_fmt( "Formatted: ~p", [ Formatted ] ).

	Formatted.



% Checks and returns the result of an HTTP request (or throws an exception).
-spec return_checked_result( result() ) -> { status_code(), term() }.
return_checked_result( _Result={ StatusLine, _Headers, Body } ) ->

	{ "HTTP/1.1", StatusCode, ReasonPhrase } = StatusLine,

	return_checked_result( { StatusCode,
							 { reason_phrased_body, ReasonPhrase, Body } } );

return_checked_result( _Result={ StatusCode, Body } ) ->

	{ ReasonPhrase, RealBody } = case Body of

		{ reason_phrased_body, Reason, OriginalBody } ->
			{ Reason, OriginalBody };

		_AnyBody ->
			{ "", Body }

	end,

	case StatusCode of

		200 ->
			{ 200, RealBody };

		SuccessCode when SuccessCode > 200 andalso SuccessCode < 300 ->
			{ SuccessCode, RealBody };

		RedirectionErrorCode when RedirectionErrorCode >= 300 andalso
								  RedirectionErrorCode < 400 ->
			throw( { http_redirection_error, RedirectionErrorCode,
					 ReasonPhrase, RealBody } );

		ClientErrorCode when ClientErrorCode >= 400
							 andalso ClientErrorCode < 500 ->
			throw( { http_client_error, ClientErrorCode, ReasonPhrase,
					 RealBody } );

		ServerErrorCode when ServerErrorCode >= 500 ->
			throw( { http_server_error, ServerErrorCode, ReasonPhrase,
					 format_body_error( RealBody ) } )

	end;

return_checked_result( _Result=Reference ) when is_reference( Reference ) ->
	Reference;

return_checked_result( Result ) ->
	throw( { invalid_http_response, Result } ).



% Checks the basic structure of an HTTP request, as needed by httpc.
-spec check_http_request( method(), request() ) -> void().
check_http_request( Method, _Request={ URL, Headers } )
  when is_list( Headers ) ->

	case lists:member( Method, get_no_body_http_methods() ) of

		true ->
			ok;

		false ->
			throw( { http_method_requires_body_in_request, Method } )

	end,

	case text_utils:is_string( URL ) of

		true ->
			ok;

		false ->
			throw( { invalid_url_for_http_request, URL } )

	end;

check_http_request( _Method, Request={ _URL, _Headers } ) ->
	throw( { invalid_headers_for_http_request, Request } );

check_http_request( Method, _Request={ URL, Headers, ContentType, Body } )
  when is_list( Headers ) ->

	case lists:member( Method, get_body_allowing_http_methods() ) of

		true ->
			ok;

		false ->
			throw( { http_method_forbids_body_in_request, Method } )

	end,

	case text_utils:is_string( URL ) of

		true ->
			ok;

		false ->
			throw( { invalid_url_for_http_request, URL } )

	end,

	case text_utils:is_string( ContentType ) of

		true ->
			ok;

		false ->
			throw( { invalid_content_type_for_http_request, ContentType } )

	end,

	case text_utils:is_string( Body ) orelse is_binary( Body ) of

		true ->
			ok;

		false ->
			throw( { invalid_body_for_http_request, Body } )

	end;

check_http_request( _Method, Request={ _URL, _Headers, _CType, _Body } ) ->
	throw( { invalid_headers_for_http_request, Request } );

check_http_request( _Method, Request ) ->
	throw( { invalid_http_request, Request } ).
