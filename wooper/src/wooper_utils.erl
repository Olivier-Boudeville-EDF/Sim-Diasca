% Copyright (C) 2017-2021 Olivier Boudeville
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


% Module containing some extra facilities for WOOPER users.
-module(wooper_utils).


% Section related to the possible use of Python.
%
% (see also: python_utils, in Ceylan-Myriad)
%
-export([ pep8_class_to_wooper_class/1, wooper_class_to_pep8_class/1 ]).



% Section related to the possible use of Java.
%
% (see also: java_utils, in Ceylan-Myriad)
%
-export([ java_class_to_wooper_class/1, wooper_class_to_java_class/1,
		  get_java_package_and_class_for/1 ]).


% Section related to the conversion between types in CamelCase and WOOPER
% classnames.
%
-export([ camelcase_type_to_wooper_class/1, wooper_class_to_camelcase_type/1 ]).



% Various helpers for testing, OTP compliance, etc.
-export([ start_for_test/0, start_for_app/0 ]).


% To determine the key assigned to a given classname as persistent_term:
-export([ get_persistent_key_for/1 ]).


% Ex: 'apple' or 'travelling_salesman'.
-type camelcase_type() :: atom().

-export_type([ camelcase_type/0 ]).



% For wooper_enable_otp_integration:
-include("wooper_defines_exports.hrl").

% Shorthands:
-type ustring() :: text_utils:ustring().



% Deduces the Erlang equivalent name, according to the WOOPER conventions, of a
% class that is actually implemented in Python and whose name follows the PEP8
% convention.
%
% Ex: 'MyFoobarExample' resulting in 'class_MyFoobarExample'.
%
-spec pep8_class_to_wooper_class(
		python_utils:pep8_classname() | ustring() )	-> wooper:classname().
pep8_class_to_wooper_class( Classname ) when is_atom( Classname ) ->
	pep8_class_to_wooper_class( text_utils:atom_to_string( Classname ) );

pep8_class_to_wooper_class( ClassnameStr ) ->
	text_utils:string_to_atom( "class_" ++ ClassnameStr ).



% Deduces the Python equivalent name, according to the PEP8 convention, of an
% Erlang class whose name follows the WOOPER conventions.
%
% Ex: "class_MyFoobarExample" resulting in "MyFoobarExample".
%
-spec wooper_class_to_pep8_class( wooper:classname() | ustring() ) ->
										python_utils:pep8_classname().
wooper_class_to_pep8_class( Classname ) when is_atom( Classname ) ->
	wooper_class_to_pep8_class( text_utils:atom_to_string( Classname ) );

wooper_class_to_pep8_class( ClassnameString ) ->

	case text_utils:split_after_prefix( "class_", ClassnameString ) of

		no_prefix ->
			throw( { invalid_wooper_classname, ClassnameString } );

		PythonClassname ->
			text_utils:string_to_atom( PythonClassname )

	end.





% Deduces the Erlang equivalent name, according to the WOOPER conventions, of a
% class that is actually implemented in Java.
%
% Ex: 'MyFoobarExample' resulting in 'class_MyFoobarExample'.
%
-spec java_class_to_wooper_class(
		java_utils:java_classname() | ustring() ) -> wooper:classname().
java_class_to_wooper_class( Classname ) when is_atom( Classname ) ->
	java_class_to_wooper_class( text_utils:atom_to_string( Classname ) );

java_class_to_wooper_class( ClassnameStr ) ->
	text_utils:string_to_atom( "class_" ++ ClassnameStr ).



% Deduces the Java equivalent name of an Erlang class whose name follows the
% WOOPER conventions.
%
% Ex: "class_MyFoobarExample" resulting in "MyFoobarExample".
%
-spec wooper_class_to_java_class( wooper:classname() ) ->
										java_utils:java_string_classname().
wooper_class_to_java_class( Classname ) when is_atom( Classname ) ->

	ClassnameString = text_utils:atom_to_string( Classname ),

	case text_utils:split_after_prefix( "class_", ClassnameString ) of

		no_prefix ->
			throw( { invalid_wooper_classname, Classname } );

		JavaClassname ->
			JavaClassname

	end.



% Returns (as atoms) the Java package (if any) and class that correspond to the
% specified WOOPER classname.
%
% So for example a WOOPER classname equal to
% 'class_BigPackage__MyPackage__MyExample' is to be translated into: {
% 'bigpackage.mypackage', 'MyExample' }, while for 'class_MyExample' we have {
% undefined,  MyExample } returned.
%
% Note: no Java package shall be named 'undefined'.
%
-spec get_java_package_and_class_for( wooper:classname() ) ->
		java_utils:java_fully_qualified_classname().
get_java_package_and_class_for( WOOPERClassname ) ->

	% For instance, let's suppose WOOPERClassname is
	% 'class_BigPackage__MyPackage__MyExample'.

	% Then JavaPackageAndClass = "BigPackage__MyPackage__MyExample":
	JavaPackageAndClass = wooper_class_to_java_class( WOOPERClassname ),

	% [ "BigPackage", "MyPackage", "MyExample" ]:
	SplitElems = string:split( JavaPackageAndClass, _Pattern="__",
							   _Where=all ),

	{ JavaClassElem, JavaPackageElems } =
		list_utils:extract_last_element( SplitElems ),

	% Then JavaClass is 'MyExample':
	JavaClass = text_utils:string_to_atom( JavaClassElem ),

	% And JavaPackage becomes, based on [ "BigPackage", "MyPackage" ],
	% 'bigpackage.mypackage':
	%
	JavaPackageString = text_utils:join( _Sep=".",
				[ text_utils:to_lowercase( E ) || E <- JavaPackageElems ] ),

	case JavaPackageString of

		"" ->
			JavaClass;

		_ ->
			JavaPackage = text_utils:string_to_atom( JavaPackageString ),
			{ JavaPackage, JavaClass }

	end.



% Converts a simple type specified in CamelCase (ex: 'apple' or
% 'travelling_salesman') into its corresponding WOOPER classname (ex:
% 'class_Apple' of 'class_TravellingSalesman').
%
-spec camelcase_type_to_wooper_class( camelcase_type() ) -> wooper:classname().
camelcase_type_to_wooper_class( CamelcaseType ) ->

	% For example CamelcaseType = travelling_salesman:
	TypeString = case text_utils:atom_to_string( CamelcaseType ) of

		Invalid="class_" ++ _ ->
			throw( { invalid_camelcase_type, already_prefixed, Invalid } );

		Other ->
			Other

	end,

	% Underscores removed:
	CamelCapElems = [ text_utils:uppercase_initial_letter( E )
			   || E <- text_utils:split( TypeString, _Delimiters=[ $_ ] ) ],

	ClassString = "class_" ++ lists:flatten( CamelCapElems ),

	text_utils:string_to_atom( ClassString ).



% Converts a WOOPER classname (ex: 'class_Apple') into its corresponding simple
% type in CamelCase (ex: 'apple').
%
-spec wooper_class_to_camelcase_type( wooper:classname() ) -> camelcase_type().
wooper_class_to_camelcase_type( WOOPERClassname ) ->

	% For example Classname = class_TravellingSalesman:
	case text_utils:atom_to_string( WOOPERClassname ) of

		"class_" ++ Suffix ->
			Elems = [ text_utils:to_lowercase( E )
					  || E <- text_utils:split_camel_case( Suffix ) ],
			text_utils:string_to_atom( text_utils:join( _Sep="_", Elems ) );

		Invalid ->
			throw( { invalid_wooper_classname, Invalid } )

	end.



% Starts WOOPER for a testing possibly involving an OTP-based startup procedure.
-spec start_for_test() -> void().

-if( ?wooper_enable_otp_integration =:= true ).


start_for_test() ->
	trace_utils:info( "Starting WOOPER test OTP environment." ),
	%wooper_class_manager:start().
	ok.

-elif( ?wooper_enable_otp_integration =:= false ).


% Here we intentionally do not start the WOOPER class manager, as an a priori
% creation shall remain optional:
%
start_for_test() ->
	trace_utils:info( "Starting WOOPER test non-OTP environment "
					  "(thus not creating the WOOPER class manager)." ),
	ok.


-endif. % wooper_enable_otp_integration



% Starts WOOPER for an application launch possibly involving an OTP-based
% startup procedure.
%
-spec start_for_app() -> void().

-if( ?wooper_enable_otp_integration =:= true ).


start_for_app() ->
	%trace_utils:info( "Starting WOOPER application OTP environment." ),
	%wooper_class_manager:start().
	ok.

-elif( ?wooper_enable_otp_integration =:= false ).


% Here we intentionally do not start the WOOPER class manager, as an a priori
% creation shall remain optional:
%
start_for_app() ->
	trace_utils:info( "Starting WOOPER application non-OTP environment "
					  "(thus not creating the WOOPER class manager)." ),
	ok.


-endif. % wooper_enable_otp_integration



% Returns the key (as a term) associated to specified classname in the
% persistent_term registry.
%
-spec get_persistent_key_for( wooper:classname() ) -> term().
get_persistent_key_for( Classname ) ->
	% Could be instead {wooper, Classname} should clashes be feared:
	Classname.
