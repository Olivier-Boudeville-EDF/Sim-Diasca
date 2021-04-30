% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER library.
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


% Basic testing of the wooper_utils module.
-module(wooper_utils_test).


% For run/0 export and al:
-include_lib("myriad/include/test_facilities.hrl").


% Actual test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	FirstWOOPERClassname = 'class_BigPackage__MyPackage__MyExample',

	FirstJavaFullClassname = wooper_utils:get_java_package_and_class_for(
							   FirstWOOPERClassname   ),

	test_facilities:display( "To the WOOPER classname '~ts' corresponds "
		"~p, i.e. ~ts.~n",
		[ FirstWOOPERClassname, FirstJavaFullClassname,
		  java_utils:fully_qualified_classname_to_string(
			FirstJavaFullClassname ) ] ),


	SecondWOOPERClassname = 'class_MyPackage__MyExample',

	SecondJavaFullClassname = wooper_utils:get_java_package_and_class_for(
							   SecondWOOPERClassname   ),

	test_facilities:display( "To the WOOPER classname '~ts' corresponds "
		"~p, i.e. ~ts.~n",
		[ SecondWOOPERClassname, SecondJavaFullClassname,
		  java_utils:fully_qualified_classname_to_string(
			SecondJavaFullClassname ) ] ),


	ThirdWOOPERClassname = 'class_MyExample',

	ThirdJavaFullClassname = wooper_utils:get_java_package_and_class_for(
							   ThirdWOOPERClassname ),

	test_facilities:display( "To the WOOPER classname '~ts' corresponds "
		"~p, i.e. ~ts.~n",
		[ ThirdWOOPERClassname, ThirdJavaFullClassname,
		  java_utils:fully_qualified_classname_to_string(
			ThirdJavaFullClassname ) ] ),

	test_facilities:stop().
