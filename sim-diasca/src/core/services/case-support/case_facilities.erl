% Copyright (C) 2012-2021 EDF R&D
%
% This file is part of Sim-Diasca.
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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]


% Directly obtained from myriad/src/utils/test_facilities.erl.


% This module defines a few basic facilities for cases, at the level of the
% Sim-Diasca layer (first one to introduce the concept of case).
%
-module(case_facilities).


% To be output before each displayed message:
-define( display_prefix, "  " ).


-export([ start/1, stop/0, display/1, display/2, fail/1, fail/2, finished/0 ] ).



% Starts a case; expected to be the first case statement.
%
% Here we disable explicitly the trapping of EXIT events, as a function run
% through "erl -eval" (like our cases) or through "erl -run" will be executed in
% a process which will silently trap EXIT events, which would mean that the
% crash of any process created from the case, even thanks to spawn_link, would
% most probably remain unnoticed (just leading to an EXIT message happily
% sitting in the mailbox of the case process).
%
-spec start( basic_utils:module_name() | [ basic_utils:module_name() ] ) ->
				   void().
start( Module ) when is_atom( Module ) ->
	erlang:process_flag( trap_exit, false ),
	basic_utils:display( "~n~n--> Running case ~s.~n", [ Module ] );

start( Modules ) when is_list( Modules ) ->
	erlang:process_flag( trap_exit, false ),
	basic_utils:display( "~n~n--> Running case ~p.~n", [ Modules ] ).



% Stops a case; expected to be the last case statement in the normal case.
%
-spec stop() -> no_return().
stop() ->
	basic_utils:display( "\n--> Successful termination of case.\n" ),
	finished().



% Displays a case message.
%
-spec display( string() ) -> void().
display( Message ) ->
	% Carriage return already added in basic_utils:display/1:
	basic_utils:display( lists:flatten( Message ) ).



% Displays a case message, once formatted.
%
% FormatString is an io:format-style format string, ValueList is the
% corresponding list of field values.
%
-spec display( string(), list() ) -> void().
display( FormatString, ValueList ) ->
	basic_utils:display( FormatString, ValueList ).


% Comment out to be able to use the interpreter after the case:
-define(ExitAfterCase,).

-spec finished() -> no_return().


-ifdef(ExitAfterCase).


finished() ->

	basic_utils:display( "(case finished, interpreter halted)" ),

	% Probably not that useful:
	system_utils:await_output_completion(),

	% Implies flushing as well:
	basic_utils:stop_on_success(),

	% Useless, but otherwise Dialyzer will complain that this function has no
	% local return:
	case_success.


-else. % ExitAfterCase


finished() ->

	basic_utils:display( "(case finished, interpreter still running)~n" ),

	%system_utils:await_output_completion(),

	case_success.


-endif. % ExitAfterCase



% To be called whenever a case is to fail (crash on error) immediately.
%
% Ex: case_facilities:fail( "server on strike" )
%
-spec fail( string() ) -> no_return().
fail( Reason ) ->

	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	basic_utils:display( "~n!!!! Case failed, reason: ~s.~n~n", [ Reason ] ),

	% Never returns:
	erlang:error( "Case failed" ),

	% Hence probably not that useful:
	system_utils:await_output_completion(),

	basic_utils:stop_on_failure(),

	% Useless, but otherwise Dialyzer will complain that this function has no
	% local return:
	case_failed.



% To be called whenever a case is to fail (crash on error) immediately.
%
% FormatString is an io:format-style format string, ValueList is the
% corresponding list of field values.
%
% Ex: case_facilities:fail( "server ~s on strike", [ "foobar.org" ] )
%
-spec fail( string(), list() ) -> no_return().
fail( FormatString, ValueList ) ->

	% For some reason, erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	ErrorMessage = io_lib:format( "~n!!!! Case failed, reason: ~s.~n~n",
								[ io_lib:format( FormatString, ValueList ) ] ),

	basic_utils:display( "~n!!!! Case failed, reason: ~s.~n~n",
						 [ ErrorMessage ] ),

	erlang:error( "Case failed" ),

	% Hence probably not that useful:
	system_utils:await_output_completion(),

	basic_utils:stop_on_failure(),

	% Useless, but otherwise Dialyzer will complain that this function has no
	% local return:
	case_failed.
