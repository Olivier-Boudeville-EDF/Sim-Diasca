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
% Creation date: Tuesday, June 25, 2019.



% Unit tests for the services related to web content.
%
% See the web_utils.erl tested module.
%
-module(web_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-compile( { nowarn_unused_function, [ test_local/0, test_online/0 ] } ).


% Just an example. A webserver is expected to run at the specified location (see
% US-Web from an example thereof).
%
test_local() ->

	TargetPort = 8080,
	TargetUrl = "index.html",

	test_facilities:display( "As a test, fetching page '~ts' from a webserver "
		"supposedly running on localhost, at TCP port #~B.",
		[ TargetUrl, TargetPort ] ),

	URI = text_utils:format( "http://localhost:~B/~ts",
							 [ TargetPort, TargetUrl ] ),

	web_utils:start(),

	{ _StatusCode=200, _HeaderMap, Body } =
		web_utils:get( URI, _Headers=[], _HttpOptions=[] ),

	trace_bridge:debug_fmt( "Body: '~ts'.", [ Body ] ),

	"This is static website D. This is the one you should see if pointing to "
	"the default virtual host corresponding to the local host. "
	"This shows that the US-Web server is up and running.\n" = Body,

	web_utils:stop().



test_online() ->

	%TargetUrl = "http://nonexisting.com/test/foo.html",
	TargetUrl = "https://en.wikipedia.org/wiki/Main_Page",

	FilePath = web_utils:download_file( TargetUrl, _TargetDir="/tmp" ),

	test_facilities:display( "Reading from URL '~ts': wrote file '~ts'.",
							 [ TargetUrl, FilePath ] ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	ItemList = [ "hello <b>world</b>!", "Once upon a time...", "Goodbye!" ],

	test_facilities:display( "Generating an unordered list:~n~ts~n",
							 [ web_utils:get_unordered_list( ItemList ) ] ),

	TestString = "I'm a \"test\" string & I am more (>) proud of it than <<<.",

	EncodedString = "I&#39;m a &quot;test&quot; string &amp; I am more (&gt;) "
		"proud of it than &lt;&lt;&lt;.",

	% Check:
	EncodedString = web_utils:escape_as_html_content( TestString ),

	test_facilities:display( "Escaping for HTML \"~ts\", getting: \"~ts\" "
		"(outer quotes excluded in both cases).",
		[ TestString, EncodedString ] ),

	% Disabled by default, not wanting a test to fail if no Internet access:
	% test_online(),

	% Disabled by default, no webserver expected to be running locally:
	%test_local(),

	test_facilities:stop().
