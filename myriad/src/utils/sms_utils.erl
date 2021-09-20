% Copyright (C) 2013-2021 Olivier Boudeville
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
% Creation date: Friday, November 1, 2013.


% @doc Gathering of various convenient <b>SMS-related</b> facilities.
%
% They rely mostly on a web gateway for that. See [http://mobile.esperide.org]
% for one based typically on 3G devices.
%
% See sms_utils_test.erl for testing.
%
-module(sms_utils).



% Implementation notes.
%
% Note that, in any module not beginning with the UTF line below, with pre-R17
% Erlang versions, sent text might be garbled due to a wrong encoding.
%
%% -*- coding: utf-8 -*-
%

% Currently supported SMS providers (HTTP gateways):
%
% - VerySMS (http://www.verysms.fr)



% A typical returned header by the verysms service is:

	 %% [{"cache-control",
	 %%   "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"},
	 %%  {"connection","Keep-Alive"},
	 %%  {"date","Mon, 28 Oct 2013 15:18:24 GMT"},
	 %%  {"pragma","no-cache"},
	 %%  {"server",
	 %%   "Apache/2.2.20 (Unix) mod_ssl/2.2.20 OpenSSL/0.9.8o"},
	 %%  {"vary","Accept-Encoding,User-Agent"},
	 %%  {"content-length","2"},
	 %%  {"content-type","text/html"},
	 %%  {"expires","Thu, 19 Nov 1981 08:52:00 GMT"},
	 %%  {"x-powered-by","PHP/5.2.13-pl1-gentoo"},
	 %%  {"set-cookie",
	 %%   "PHPSESSID=9adc709f872cc44f1f4abc1c4931914b; path=/"},
	 %%  {"keep-alive","timeout=15, max=100"}],




% Sending-related functions:
-export([ create_sms/3, create_sms/4, send/2, send/7,
		  update_credits/1,
		  account_to_string/1, sms_to_string/1 ]).



% Type declarations.


-type provider() :: 'verysms'.
% Name of a SMS provider (HTTP gateway), as an atom.


-type service_class() ::
		% Only sensible for the 'verysms' provider:
		'eco' | 'pro'.
% Service class of a SMS being sent.


-type message() :: ustring().
% Message to be sent as SMS (up to 160 bytes).


-type phone_number() :: ustring().
% A phone number, preferably international, preferably with no whitespaces (ex:
% "+330616XXXXXX").


-type recipient() :: phone_number().
% Phone number of a recipient, probably of a mobile phone (ex: "+330616XXXXXX").


-type sender_description() :: ustring().
% Any (short) alphanumerical string describing the sender, if this service is
% provided.
%
% With verysms, supposedly only taken into account in the 'pro' class; but
% actually SMS always shown as sent by the number "38200".


-type failure_reason() ::
		{ 'invalid_content', ustring() }
	  | { 'invalid_phone_number', ustring() }
	  |   'credits_exhausted'
	  |   'insufficient_credits'
	  | { 'invalid_password', ustring() }
	  | { 'invalid_user', ustring() }
	  | { 'invalid_content', ustring() }
	  | { 'invalid_request', ustring() }
	  | { 'error', ustring() }
	  | { 'request_failed', ustring() }.
% Many steps may go wrong.


-type diagnosis() :: any().


-type sending_outcome() :: 'success' | { failure_reason(), diagnosis() }.
% Describes the result of the sending, as reported by the gateway.


-type credits() :: maybe( count() ).
% Number of credits left (if applicable).



-record( sms_account, {

		provider :: provider(),

		user_name :: system_utils:user_name(),

		password :: system_utils:password(),

		default_class :: service_class(),

		credits :: credits(),

		sent_count :: count(),

		sent_success_count :: count() } ).


-type sms_account() :: #sms_account{}.
% Describes a SMS account at a provider.



% Describes a SMS.
-record( sms, {

		message :: message(),

		recipient :: recipient(),

		sender_description :: sender_description(),

		% Default account service class to be used, if not specified here:
		service_class :: maybe( service_class() ) }).


-type sms() :: #sms{}.



-export_type([ provider/0, credits/0, sms_account/0, message/0, phone_number/0,
			   recipient/0, sender_description/0, service_class/0, sms/0,
			   sending_outcome/0 ]).


% Shorthands:
-type count() :: basic_utils:count().
-type ustring() :: text_utils:ustring().



% @doc Creates a SMS record instance from specified information, the service
% class being not defined, so that the default class of the account will
% prevail.
%
-spec create_sms( message(), recipient(), sender_description() ) -> sms().
create_sms( Message, Recipient, SenderDescription ) when is_list( Message )
			andalso is_list(Recipient) andalso is_list(SenderDescription) ->
	create_sms( Message, Recipient, SenderDescription,
				_ServiceClass=undefined ).



% @doc Creates a SMS record instance from specified information.
-spec create_sms( message(), recipient(), sender_description(),
				  maybe( service_class() ) ) -> sms().
create_sms( Message, Recipient, SenderDescription, ServiceClass )
  when is_list( Message ) andalso is_list( Recipient )
	   andalso is_list( SenderDescription ) andalso is_atom( ServiceClass )->

	%trace_utils:debug_fmt( "created '~ts' ~B.",
	%                       [ Message, length( Message ) ] ),

	% More checking should be done:

	% Disabled, as there are 3 sizes: the number of Unicode characters, the
	% number of bytes once encoded in an URL, and the number of bytes once
	% encoded for SMS. Not sure which rule to apply.

	% case length( web_utils:encode_element_as_url( Message ) ) of

	%%	L when L > 160 ->
	%%		% In bytes, not characters:
	%%		throw( { message_too_long, L, Message } );

	%%	_ ->
	%%		ok

	%% end,

	#sms{ message=Message,
		  recipient=Recipient,
		  sender_description=SenderDescription,
		  service_class=ServiceClass }.




% Sending-related functions.


% @doc Sends specified SMS, using specified account.
-spec send( sms(), sms_account() ) -> { sending_outcome(), sms_account() }.
send( #sms{ message=Message, recipient=Recipient,
			sender_description=SenderDescription, service_class=ServiceClass },
	  Account=#sms_account{ provider=Provider,
							user_name=UserName, password=Password,
							default_class=DefaultClass, credits=Credits,
							sent_count=SentCount, sent_success_count=Successes
						  } ) ->

	% From a user perspective, to a technical one:

	% If the SMS does not specify a class, use the default from account:
	ActualServiceClass = case ServiceClass of

		undefined ->
			DefaultClass;

		C ->
			C

	end,

	Outcome = send( Provider, ActualServiceClass, UserName, Password, Message,
					Recipient, SenderDescription ),


	{ NewCredits, NewSuccesses } = case Outcome of

		success ->
			NewC = Credits - get_credit_cost( Provider, ActualServiceClass ),
			NewS = Successes + 1,
			{ NewC, NewS };

		_Failure ->
			{ Credits, Successes }

	end,

	{ Outcome, Account#sms_account{ credits=NewCredits,
									sent_count=SentCount + 1,
									sent_success_count=NewSuccesses } }.



% @doc Sends specified SMS, using most detailed (explicit) settings.
%
% (base, only actual sending function)
%
-spec send( provider(), service_class(), system_utils:user_name(),
			system_utils:password(), message(), recipient(),
			sender_description() ) -> sending_outcome().
send( _Provider=verysms, _ServiceClass=eco, Username, Password, Message,
	  Recipient, SenderDescription ) ->

	% In eco mode, no specific sender can be specified (will be "random" mobile
	% numbers).

	check_recipient( Recipient ),

	% Might be started multiple times:
	inets:start(),

	EcoURL = "http://www.verysms.fr/api_sendsms.php",

	FullData = [ { 'user', Username },
				 { 'pass', Password },
				 { 'dest', Recipient },
				 { 'flash', "" },
				 { 'type', "" },
				 { 'url', "" },
				 { 'msg', Message },
				 { 'origine', SenderDescription } ],

	% For this provider, web_utils:escape_as_html_content/1 must be used instead
	% of web_utils:encode_as_url/1:
	%
	Request = { EcoURL, _Headers=[], _ContentType=get_mime_type(),
				_Body=web_utils:escape_as_html_content( FullData ) },

	execute_request( Request, Username, Password, Recipient );


%% send( Provider=verysms, ServiceClass=eco, Username, Password, Message,
%%	  Recipient, SenderDescription ) ->

%%	% Not allowed in eco mode:
%%	throw( { no_sender_description_supported, { Provider, ServiceClass } } );

send( _Provider=verysms, _ServiceClass=pro, Username, Password, Message,
	  Recipient, SenderDescription ) ->

	check_recipient( Recipient ),

	% Might be started multiple times:
	inets:start(),

	ProURL = "http://www.verysms.fr/api_sendsmspro.php",

	FullData = [ { 'user', Username },
				 { 'pass', Password },
				 { 'dest', Recipient },
				 { 'flash', "" },
				 { 'type', "" },
				 { 'url', "" },
				 { 'msg', Message },
				 { 'senderID', SenderDescription },
				 { 'origine', "ceylan" },
				 { 'idSending', "1" } ],

	% For this provider, web_utils:escape_as_html_content/1 must be used instead
	% of web_utils:encode_as_url/1:
	%
	Request = { ProURL, _Headers=[], _ContentType=get_mime_type(),
				_Body=web_utils:escape_as_html_content( FullData ) },

	% In pro mode, if no sender description is specified, will be "38200":
	%% FullData = case SenderDescription of

	%%	[] ->
	%%		List;

	%%	Desc ->
	%%		[ { "senderID", Desc } | List ]

	%%		  end;

	execute_request( Request, Username, Password, Recipient );


send( Provider=verysms, ServiceClass, _Username, _Password, _Message,
	  _Recipient, _SenderDescription ) ->

	throw( { invalid_service_class, ServiceClass, Provider } );


send( Provider, _ServiceClass, _Username, _Password, _Message, _Recipient,
	  _SenderDescription ) ->

	throw( { unsupported_sms_provider, Provider } ).



% @doc Returns the specified SMS account, whose credit count has been updated,
% telling whether the actual, provider-obtained count corresponds to the
% recorded one ('matching') or not ('overwritten'), in which case the actual one
% replaces the recorded one.
%
% If the operation failed, returns 'failed' with an unchanged account.
%
-spec update_credits( sms_account() ) ->
				{ 'matching' | 'overwritten' | 'failed', sms_account() }.
update_credits( Account=#sms_account{ credits=Credits } ) ->

	case get_credits_for( Account ) of

		undefined ->
			{ failed, Account };

		Credits ->
			{ matching, Account };

		ActualCredits ->
			{ overwritten, Account#sms_account{ credits=ActualCredits } }

	end.



% @doc Returns a textual description of the specified SMS account.
-spec account_to_string( sms_account() ) -> ustring().
account_to_string( #sms_account{ provider=Provider, user_name=Username,
								 password=Password, default_class=DefaultClass,
								 credits=Credits, sent_count=SentCount,
								 sent_success_count=SentSuccessCount } ) ->
	text_utils:format( "SMS account on provider '~ts': user name is '~ts', "
		"password is '~ts', relying on default sending class '~ts', "
		"with stored credits: ~p; ~B success sendings over a total of ~B",
		[ Provider, Username, Password, DefaultClass, Credits,
		  SentSuccessCount, SentCount ] ).



% @doc Returns a textual description of the specified SMS.
-spec sms_to_string( sms() ) -> ustring().
sms_to_string( #sms{ message=Message, recipient=Recipient,
					 sender_description=SenderDesc,
					 service_class=ServiceClass } ) ->

	% Encoding (ex: of accentuated characters) changes the byte size:
	Len = length( Message ),

	text_utils:format( "SMS whose message is '~ts' (character length: "
		"~B bytes), sent to recipient number '~ts' from sender '~p' "
		"with service class ~p",
		[ Message, Len, Recipient, SenderDesc, ServiceClass ] ).



% Helper functions.


% @doc Checks recipient number.
%
% A regex with the re module could be used:
check_recipient( Recipient ) ->
	check_recipient( Recipient, Recipient ).


check_recipient( _Remaining=[], _Recipient ) ->
	ok;

check_recipient( _Remaining=[ C | T ], Recipient ) ->
	case text_utils:is_figure( C ) of

		true ->
			check_recipient( T, Recipient );

		false ->
			throw( { invalid_recipient, Recipient, C } )

	end.



% @doc Returns the MIME type to be used here.
get_mime_type() ->
	"application/x-www-form-urlencoded".



% @doc Returns the current number of credits for the specified account, as
% reported by the provider, or 'undefined' if the operation failed.
%
-spec get_credits_for( sms_account() ) -> credits().
get_credits_for( _Account=#sms_account{ provider=verysms, user_name=Username,
										password=Password } ) ->

	% Might be started multiple times:
	inets:start(),

	CreditURL = "http://www.verysms.fr/credit.php",

	FullData = [ { 'user', Username },
				 { 'pass', Password } ],


	Request = { CreditURL, _ReqHeaders=[], _ContentType=get_mime_type(),
				 % For this provider, web_utils:encode_as_url/1 apparently did
				 % not work, web_utils:escape_as_url/1 shall be used instead:
				 %
				_ReqBody=web_utils:escape_as_url( FullData ) },

	case httpc:request( _Method=post, Request, _HTTPOptions=[], _Options=[] ) of

		% HTTPVersion below: typically equal to "HTTP/1.1";

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="CREDIT OUT" } } ->
			0;

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="CREDIT " ++ RemainingCredit } } ->
			try

				text_utils:string_to_integer( RemainingCredit )

			catch

				throw:_ ->
					undefined

			end;

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="KO" } } ->
			undefined;

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="USER INVALID" } } ->
			undefined;

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="PASS INVALID" } } ->
			undefined;

		{ ok, { _StatusLine, _Headers, _Body } } ->
			undefined;

		{ error, _Reason } ->
			undefined

	end.



% @doc Executes the specified HTTP request.
%
% (helper)
%
execute_request( Request, Username, Password, Recipient ) ->

	% Note: should use web_utils now.

	%trace_utils:debug_fmt( "Request: '~p'.",  [ Request ] ),

	% We strongly prefer POST over GET (safer, stricter):
	case httpc:request( _Method=post, Request, _HTTPOptions=[], _Options=[] ) of

		% HTTPVersion below: typically equal to "HTTP/1.1";
		% Body: "OK 5" if 5 credits are remaining (we do not care here);
		%
		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="OK " ++ _RemainingCredit } } ->
			success;

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="KO" } } ->
			{ invalid_content, "Invalid SMS body, not sent" };

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="BAD NUMBER" } } ->
			{ invalid_phone_number, Recipient };

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="CREDIT OUT" } } ->
			credits_exhausted;

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="CREDIT NOT ENOUGH" } } ->
			insufficient_credits;

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="USER INVALID" } } ->
			{ invalid_user, Username };

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				_Body="PASS INVALID" } } ->
			{ invalid_password, Password };

		{ ok, { _StatusLine={ _HTTPVersion, 200, "OK" }, _Headers,
				Body } } ->
			{ invalid_content, Body };

		{ ok, Res={ _StatusLine, _Headers, _Body } } ->
			{ invalid_request, Res };

		{ error, Reason } ->
			{ request_failed, Reason }

	end.



% @doc Returns the cost in credits of sending one SMS of the specified service
% class from specified provider.
%
get_credit_cost( _Provider=verysms, _ServiceClass=eco ) ->
	5;

get_credit_cost( _Provider=verysms, _ServiceClass=pro ) ->
	10.
