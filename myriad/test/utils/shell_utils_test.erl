% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Wednesday, August 28, 2024.

-module(shell_utils_test).

-moduledoc """
Unit tests for the **shell-related** facilities.

Note that, if run interactively, this test will use a text user interface that
may cause problems on some terminals when terminating.

See the shell_utils.erl tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").



% Type shorthands:

-type shell_pid() :: shell_utils:shell_pid().



-spec test_interactive( shell_pid() ) -> void().
test_interactive( ShellPid ) ->

	test_facilities:display( "Starting shell interaction with ~w "
		"(one may enter 'halt().' to stop).", [ ShellPid ] ),

	text_ui:start(),

	test_main_loop( ShellPid ),

	text_ui:stop(),

	test_facilities:display( "Stopped shell interaction with ~w.",
							 [ ShellPid ] ).



test_main_loop( ShellPid ) ->

	Prompt = "Enter the next Erlang expression to evaluate: ",
	ExprText = text_ui:get_text( Prompt ),

	case shell_utils:execute_command( text_utils:ensure_binary( ExprText ),
									  ShellPid ) of

		{ success, CmdResValue, CmdId, MaybeBinTimestamp } ->
			test_facilities:display( "Shell expression '~ts' (#~B) "
				"evaluated (timestamp: ~ts) to '~p'.",
				[ ExprText, CmdId, MaybeBinTimestamp, CmdResValue ] );

		{ error, ErrorInfo } ->
			test_facilities:display( "The processing of shell expression '~ts' "
				"failed with: '~ts'.", [ ExprText, ErrorInfo ] )

	end,

	test_main_loop( ShellPid ).




-doc "Tests the specified shell.".
-spec test_shell( shell_pid() ) -> void().
test_shell( ShellPid ) ->

	{ processing_success, FirstRes, _FirstNextCmdId=2,
	  MaybeFirstBinTimestamp } = shell_utils:execute_command( "A=1.",
															  ShellPid ),

	test_facilities:display( "First assignment result (timestamp: ~ts): ~p.",
							 [ MaybeFirstBinTimestamp,FirstRes  ] ),
	FirstRes = 1,

	test_facilities:display( "Flushing command history."),
	ShellPid ! flushCommandHistory,

	{ processing_success, SecondRes, _SecondNextCmdId=3,
	  MaybeSecondBinTimestamp } = shell_utils:execute_command( "B=2.",
															   ShellPid ),

	test_facilities:display( "Second assignment result (timestamp: ~ts): ~p.",
							 [ MaybeSecondBinTimestamp, SecondRes ] ),
	SecondRes = 2,


	{ processing_success, ThirdRes, _ThirdNextCmdId=4,
	  MaybeThirdBinTimestamp } = shell_utils:execute_command( "A+B.",
															  ShellPid ),

	test_facilities:display( "Addition result (timestamp: ~ts): ~p.",
							 [ MaybeThirdBinTimestamp, ThirdRes ] ),
	ThirdRes = 3,


	{ processing_success, LRes, _LNextCmdId=5, MaybeLBinTimestamp } =
		shell_utils:execute_command( "L = [3, 2, 1].", ShellPid ),

	test_facilities:display( "List assignment result (timestamp: ~ts): ~p.",
							 [ MaybeLBinTimestamp, LRes ] ),
	LRes = [ 3, 2, 1 ],


	{ processing_success, SortRes, _SortNextCmdId=6, MaybeSortBinTimestamp } =
		shell_utils:execute_command( "lists:sort(L).", ShellPid ),

	test_facilities:display( "Sorting result (timestamp: ~ts): ~p.",
							 [ MaybeSortBinTimestamp, SortRes ] ),
	SortRes = [ 1, 2, 3 ],


	% Therefore typed as "--interactive-shell":
	case cmd_line_utils:get_command_arguments_for_option(
			_Option='-interactive-shell' ) of

		undefined ->
			test_facilities:display( "Not in interactive mode, stopping." );

		_ ->
			test_facilities:display( "Switching to interactive mode." ),
			test_interactive( ShellPid )

	end,

	ShellPid ! { terminateSynch, self() },

	receive

		onShellTerminated ->
			ok

	end.



get_test_shell_opts() ->

	%[].

	%HistOpt = no_history,
	%HistOpt = { histories, _MaybeMaxCmdDepth=0, _MaybeMaxResDepth=0 },
	%HistOpt = { histories,  _MaybeMaxCmdDepth=1, undefined },
	HistOpt = { histories,  _MaybeMaxCmdDepth=10, 2 },

	%HistOpts = [],
	HistOpts = [ HistOpt ],

	%TimestampOpts = [],
	TimestampOpts = [ timestamp ],

	%LogOpts = [],
	LogOpts = [ log ],
	%LogOpts = [ { log, "../test-shell.txt" } ],

	HistOpts ++ TimestampOpts ++ LogOpts.



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	ShellOpts = get_test_shell_opts(),

	test_facilities:display( "Testing the Myriad custom shell, "
		"based on following options:~n ~p.", [ ShellOpts ] ),

	ShellPid = shell_utils:start_link_shell( ShellOpts ),

	test_shell( ShellPid ),

	test_facilities:stop().
