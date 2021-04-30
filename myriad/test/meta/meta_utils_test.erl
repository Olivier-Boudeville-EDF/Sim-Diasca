% Copyright (C) 2014-2021 Olivier Boudeville
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
% Creation date: Friday, December 19, 2014.


% Unit tests for the meta utils toolbox.
%
% See the meta_utils.erl tested module.
%
-module(meta_utils_test).


% Triggers the call to the specified parse transform:
-compile( { parse_transform, example_parse_transform } ).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing typing information." ),

	boolean = type_utils:get_type_of( true ),

	atom = type_utils:get_type_of( 'an atom' ),

	binary = type_utils:get_type_of( <<1>> ),

	float = type_utils:get_type_of( 1.0 ),

	function = type_utils:get_type_of( fun( X ) -> X + 1 end ),

	integer = type_utils:get_type_of( 42 ),

	pid = type_utils:get_type_of( self() ),

	list = type_utils:get_type_of( [ hello, self() ] ),

	string = type_utils:get_type_of( "Hello world!" ),

	%port = type_utils:get_type_of( APort ),

	tuple = type_utils:get_type_of( { a, b } ),

	reference = type_utils:get_type_of( make_ref() ),


	{ false, { boolean, float } } =
		type_utils:is_homogeneous( { true, 1.0, false } ),

	{ true, integer } = type_utils:is_homogeneous( [ 0, -4, 47, 12 ] ),


	test_facilities:display( "Testing term recursive transformation." ),

	% This term transformer does not change anything in the terms it scans, and
	% just comment the traversal it does:
	%
	IdTermTransformer = fun( Term, UserData ) ->

		NewUserData =
			[ io_lib:format( "Inspected '~p', ", [ Term ] ) | UserData ],

		{ Term, NewUserData }

						end,

	TermToTraverse = { pseudo_record, [], { a, 1.0 },
					   [ { b, 42 }, "hello", [ <<"foo">> ] ], self() },

	{ TraversedTerm, InspectData } = ast_transform:transform_term(
		   TermToTraverse, _Type=atom, IdTermTransformer, _UserData=[] ),

	test_facilities:display( "Traversal of term:~n'~p' with "
		"id term transformer yielded:~n'~p', producing user data '~ts'",
		[ TermToTraverse, TraversedTerm, lists:reverse( InspectData ) ] ),


	% This term transformer changes a term into a textual representation, and
	% does not do anything with user data:
	%
	TextTermTransformer = fun( Term, UserData ) ->
							  { io_lib:format( "~w", [ Term ] ), UserData }
						  end,

	% Requested to operate only on PIDs:
	{ NewTraversedTerm, _UselessData } = ast_transform:transform_term(
		TermToTraverse, _OtherType=pid, TextTermTransformer,
		_OtherUserData=undefined ),

	test_facilities:display( "Traversal of term:~n'~p' with "
		"text term transformer yielded:~n'~p'.",
		[ TermToTraverse, NewTraversedTerm ] ),

	% For the fun of being very meta:
	%BeamFilename = "meta_utils_test.beam",
	%BeamFilename = "../utils/basic_utils.beam",
	BeamFilename = "simple_parse_transform_target.beam",

	ModuleAST = ast_utils:beam_to_ast( BeamFilename ),

	io:format( "AST= ~p~n", [ ModuleAST ] ),

	ModuleInfo = ast_info:extract_module_info_from_ast( ModuleAST ),

	TermString = "[ {tiger,[lion,leopard]} ]",

	[ {tiger, [ lion, leopard ] } ] = ast_utils:string_to_value( TermString ),

	test_facilities:display( ast_info:module_info_to_string( ModuleInfo ) ),

	test_facilities:stop().
