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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Friday, December 19, 2014.



% This module allows to test the 'myriad_parse_transform' parse transform as a
% standalone unit, hence with proper error and warning messages.
%
% See the myriad_parse_transform.erl tested module.
%
-module(myriad_parse_transform_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% To be able to silence at will:
-export([ run_parse_transform/1, run_ast_level_operations/1 ]).


run_parse_transform( TargetSourceFile ) ->

	test_facilities:display( "Applying the Myriad parse transform to the "
							 "'~ts' source file.~n", [ TargetSourceFile ] ),

	TransformedAST = myriad_parse_transform:run_standalone( TargetSourceFile ),

	test_facilities:display( "Transformed AST:~n~p~n", [ TransformedAST ] ),

	%WriteFile = true,
	WriteFile = false,

	case WriteFile of

		true ->
			ast_utils:write_ast_to_file( TransformedAST,
										 TargetSourceFile ++ ".ast" );

		false ->
			ok

	end.



run_ast_level_operations( TargetSourceFile ) ->

	test_facilities:display( "Now performing directly AST-level operations." ),

	BaseAST = ast_utils:erl_to_ast( TargetSourceFile ),

	%test_facilities:display( "Base AST:~n~p", [ BaseAST ] ),

	BaseModuleInfo = ast_info:extract_module_info_from_ast( BaseAST ),

	test_facilities:display( "Base module info: ~ts~n",
					[ ast_info:module_info_to_string( BaseModuleInfo ) ] ),

	FinalModuleInfo = BaseModuleInfo,

	test_facilities:display( "Final module info: ~ts~n",
					[ ast_info:module_info_to_string( FinalModuleInfo ) ] ),

	FinalAST = ast_info:recompose_ast_from_module_info( FinalModuleInfo ),

	test_facilities:display( "Final AST:~n~p", [ FinalAST ] ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	%TargetSourceFile = "graph_utils.erl",
	TargetSourceFile = "simple_parse_transform_target.erl",

	run_parse_transform( TargetSourceFile ),

	%run_ast_level_operations( TargetSourceFile ),

	test_facilities:stop().
