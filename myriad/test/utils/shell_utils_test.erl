% Copyright (C) 2020-2021 Olivier Boudeville
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
% Creation date: Wednesday, May 20, 2020.


% Unit tests for the shell_utils toolbox.
%
% See the shell_utils.erl tested module.
%
-module(shell_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Note:
%
% One can test the support of command-line arguments with, for example:
%
% make shell_utils_run CMD_LINE_OPT="a b -my-first-opt u v w -my-other-opt x"



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	ArgTable = shell_utils:get_argument_table(),

	test_facilities:display( "Obtained following argument table: ~ts",
		[ shell_utils:argument_table_to_string( ArgTable ) ] ),

	OptionLessArgs = shell_utils:get_optionless_command_arguments(),

	test_facilities:display( "Option-less arguments are: ~p.",
							 [ OptionLessArgs ] ),


	{ OtherOptionLessArgs, ShrunkArgTable } =
		shell_utils:extract_optionless_command_arguments(),

	test_facilities:display(
	  "Extracted option-less arguments are: ~p (remainder: ~ts).",
	  [ OtherOptionLessArgs,
		shell_utils:argument_table_to_string( ShrunkArgTable) ] ),


	% Not a user-level option (VM-level), hence no supposed to be defined at
	% all:
	%
	PzOption = 'pz',

	{ PzValues=undefined, PzRemainingArguments } =
		shell_utils:extract_command_arguments_for_option( PzOption ),

	test_facilities:display( "Knowing the actual command-line arguments were:~n"
		"~p~nfor (VM, not user) option '~ts', we extracted following value(s), "
		"expected not to be defined:~n~p~n"
		"and got the rest of the arguments:~n~p",
		[ init:get_arguments(), PzOption, PzValues, PzRemainingArguments ] ),


	RealOption = 'my-first-opt',

	{ RealOptValues, RealOptRemainingArguments } =
		shell_utils:extract_command_arguments_for_option( RealOption,
													PzRemainingArguments ),

	test_facilities:display( "For (user) option '~ts', we extracted following "
		"value(s):~n~p~nand got the rest of the arguments: ~ts",
		[ RealOption, RealOptValues,
		  shell_utils:argument_table_to_string( RealOptRemainingArguments ) ] ),


	AdHocCommandLine = "an_optionless_arg another_optionless_arg "
		"--my-first-opt a b c a_third_optionless_arg -my-other-opt e "
		"--my-first-opt d",

	AdHocArgTable = shell_utils:generate_argument_table( AdHocCommandLine ),

	test_facilities:display( "Ad hoc argument table from '~ts':~n~ts",
		[ AdHocCommandLine,
		  shell_utils:argument_table_to_string( AdHocArgTable ) ] ),


	OptionLessSpec = { _Min=1, _Max=3 },

	% Here we specify that the first option is to take only 2 (exactly; not 3)
	% arguments, hence any argument found after the second will be considered as
	% an option-less one (this is the case of 'c' and 'a_third_optionless_arg'):

	OptionSpecs = [ { '-my-first-opt', 2 }, { 'my-other-opt', 1 } ],

	UniqArgTable = shell_utils:get_command_line_arguments( OptionLessSpec,
										OptionSpecs, AdHocArgTable ),

	test_facilities:display( "Resulting argument table, based on option "
		"specs ~p:~n~ts", [ OptionSpecs,
			shell_utils:argument_table_to_string( UniqArgTable ) ] ),

	test_facilities:stop().
