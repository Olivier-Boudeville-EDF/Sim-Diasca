% Copyright (C) 2021-2023 Olivier Boudeville
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
% Creation date: Sunday, December 5, 2021.


% Unit tests for the <b>XML services</b>.
%
% See the xml_utils.erl tested module.
%
-module(xml_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For xmerl records:
-include_lib("xml_utils.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the XML support." ),

	TextToEscape = "I'm a believer that 2<3, but 4>3 & I say: \"I like it!\".",

	EscapedText = xml_utils:to_xml_text( TextToEscape ),

	test_facilities:display( "The escaped for XML version of text '~ts' "
							 "is '~ts'.~n", [ TextToEscape, EscapedText ] ),


	TestXMLSimpleContent = [
		myFirstTag,
		{ mySecondTag, [ myNestedTag ] },
		{ myThirdTag,  [ { color, "red" }, { age, 71 } ],
		  [ "This is a text!" ] } ],

	TextPrologValue = xml_utils:get_default_prolog()
								++ "<!DOCTYPE birds SYSTEM \"birds.dtd\">",

	ResultSimpleStr = xml_utils:xml_to_string( TestXMLSimpleContent,
											   TextPrologValue ),

	test_facilities:display( "The XML content defined as:"
		"~n  ~p~nresults, with prolog '~ts', in the following string:~n  ~ts",
		[ TestXMLSimpleContent, TextPrologValue, ResultSimpleStr ] ),

	test_facilities:stop().
