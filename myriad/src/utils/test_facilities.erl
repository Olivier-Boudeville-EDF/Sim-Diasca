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


% This module defines a few basic facilities for tests, at the level of the
% 'Myriad' layer.
%
-module(test_facilities).


% To be output before each displayed message:
-define( display_prefix, "  " ).


-export([ start/1, stop/0, display/1, display/2, display_fmt/2,
		  fail/1, fail/2, finished/0 ] ).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().



% Starts a test; expected to be the first test statement.
%
% Here we disable explicitly the trapping of EXIT events, as a function run
% through "erl -eval" (like our tests) or through "erl -run" will be executed in
% a process which will silently trap EXIT events, which would mean that the
% crash of any process created from the test, even thanks to spawn_link, would
% most probably remain unnoticed (just leading to an EXIT message happily
% sitting in the mailbox of the test process).
%
-spec start( module() | [ module() ] ) -> void().
start( Module ) when is_atom( Module ) ->
	start_common(),
	basic_utils:display( "~n~n--> Testing module ~ts.~n", [ Module ] );

start( Modules ) when is_list( Modules ) ->
	start_common(),
	basic_utils:display( "~n~n--> Testing modules ~p.~n", [ Modules ] ).


% (helper)
start_common() ->

	% We prefer tests to fail (i.e. that they crash through the link(s) that
	% they create with their tested elements), rather than resisting silently to
	% any failure (with EXIT messages sitting in their mailboxes and probably be
	% never read):
	%
	erlang:process_flag( trap_exit, false ),

	% To avoid that special characters are not displayed properly:
	system_utils:force_unicode_support(),

	% We want to ensure that the standard logger behaves synchronously (ex: not
	% wanting an error trace to be lost because we crashed on purpose the VM
	% just after an error was reported whereas it happened not to have been
	% notified already - since a corresponding message was not sent yet, or was
	% received but not yet processed).

	ok = logger:set_handler_config( _HandlerId=default, _Key=sync_mode_qlen,
									_Value=0 ).



% Stops a test; expected to be the last test statement in the normal case.
-spec stop() -> no_return().
stop() ->
	basic_utils:display( "\n--> Successful end of test.\n" ),
	finished().



% Displays a test message.
-spec display( ustring() ) -> void().
display( Message ) ->
	% Carriage return already added in basic_utils:display/1:
	% (empty list added so that ~n are automatically converted)
	basic_utils:display( lists:flatten( Message ), _ValueList=[] ).



% Displays a test message, once formatted.
%
% FormatString is an io:format-style format string, ValueList is the
% corresponding list of field values.
%
-spec display( format_string(), format_values() ) -> void().
display( FormatString, ValueList ) ->
	basic_utils:display( FormatString, ValueList ).


% Displays a test message, once formatted.
%
% Defined for consistency.
%
% FormatString is an io:format-style format string, ValueList is the
% corresponding list of field values.
%
-spec display_fmt( format_string(), format_values() ) -> void().
display_fmt( FormatString, ValueList ) ->
	basic_utils:display( FormatString, ValueList ).


% Comment out to be able to use the interpreter after the test:
%
% (as a result, the default is to immediately exit once a test is over)
%
-define( exit_after_test, ).

-spec finished() -> no_return().


-ifdef(exit_after_test).


finished() ->

	basic_utils:display( "(test finished, interpreter halted)" ),

	% Probably not that useful:
	system_utils:await_output_completion(),

	% Implies flushing as well:
	basic_utils:stop_on_success(),

	% Useless, but otherwise Dialyzer will complain that this function has no
	% local return:
	%
	test_success.


-else. % exit_after_test


finished() ->

	basic_utils:display( "(test finished, interpreter still running)~n"
		"(if the Erlang shell is not available, ensure that "
		"no '-noinput' VM command-line option is used;~n"
		" see EXEC_INTERNAL_OPTIONS in Ceylan-Myriad's "
		"GNUmakevars.inc for that)", _Necessary=[] ),

	%system_utils:await_output_completion(),

	test_success.


-endif. % exit_after_test



% To be called whenever a test is to fail (crash on error) immediately.
%
% Ex: test_facilities:fail( "server on strike" )
%
-spec fail( ustring() ) -> no_return().
fail( Reason ) ->

	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	basic_utils:display( "~n!!!! Test failed, reason: ~ts~n", [ Reason ] ),

	% Never returns:
	erlang:error( "Test failed" ),

	% Hence probably not that useful:
	system_utils:await_output_completion(),

	basic_utils:stop_on_failure(),

	% Useless, but otherwise Dialyzer will complain that this function has no
	% local return:
	%
	test_failed.



% To be called whenever a test is to fail (crash on error) immediately.
%
% FormatString is an io:format-style format string, ValueList is the
% corresponding list of field values.
%
% Ex: test_facilities:fail( "server ~ts on strike", [ "foobar.org" ] )
%
-spec fail( format_string(), format_values() ) -> no_return().
fail( FormatString, ValueList ) ->

	% For some reason, erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	ReasonMessage = text_utils:format( FormatString, ValueList ),

	fail( ReasonMessage ).
