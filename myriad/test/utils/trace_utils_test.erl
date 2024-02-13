% Copyright (C) 2017-2024 Olivier Boudeville
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
% Creation date: 2017.


% @doc Unit tests for the trace_utils toolbox.
%
% See the trace_utils.erl tested module.
%
-module(trace_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Starting with the default logger handler, "
		"using directly logger functions, with following initial "
		"configuration:~n - primary: ~p~n - all handlers: ~p",
		[ logger:get_primary_config(), logger:get_handler_config() ] ),

	% Note that levels lower than 'notice' will not show up due to the default
	% primary log level of logger (operating before the configuration of the
	% logger handler):
	%
	logger:debug( "I am a debug direct logger message "
				  "(this log will *not* show up)." ),

	logger:info( "I am an info direct logger message "
				  "(this log will *not* show up)." ),

	% Seen by default:
	logger:notice( "I am a notice direct logger message." ),
	logger:warning( "I am a warning direct logger message." ),

	% Will probably show up a bit later, as must be written in a log file:
	logger:error( "I am an error direct logger message." ),
	logger:critical( "I am a critical direct logger message." ),
	logger:alert( "I am an alert direct logger message." ),
	logger:emergency( "I am an emergency direct logger message." ),


	% To avoid interleaving of printouts:
	timer:sleep( 200 ),

	test_facilities:display( "~nStill based on the default logger handler, "
							 "testing the trace_utils functions." ),

	% All messages will be shown, as no logger filtering applies:
	trace_utils:debug( "I am a debug simple message." ),
	trace_utils:info( "I am an info simple message." ),
	trace_utils:notice( "I am a notice simple message." ),
	trace_utils:warning( "I am a warning simple message." ),
	trace_utils:error( "I am an error simple message." ),
	trace_utils:critical( "I am a critical simple message." ),
	trace_utils:alert( "I am an alert simple message." ),
	trace_utils:emergency( "I am an emergency simple message." ),

	test_facilities:display( "Setting the default Myriad logger handler." ),
	trace_utils:set_handler(),

	test_facilities:display( "Newer logger configurations:~n ~p.",
							 [ logger:get_handler_config() ] ),

	% Avoid any race condition, if the previous setting was not synchronous:
	timer:sleep( 100 ),

	% Despite the new (Myriad) log handler set with the level 'all', the next
	% two logs will still not be visible, due to the logger primary log level:
	%
	logger:debug( "I am another debug direct logger message "
				  "(this log will *not* show up)." ),

	logger:info( "I am another info direct logger message "
				  "(this log will *not* show up)." ),

	logger:set_primary_config( level, _Lvl=debug ),

	logger:debug( "I am a now visible debug direct logger message." ),
	logger:info( "I am a now visible info direct logger message." ),

	test_facilities:display( "Testing the default, very basic Myriad trace "
		"subsystem, emitting our own traces." ),

	trace_utils:debug( "I am a debug simple message." ),
	trace_utils:debug_fmt( "I am a debug ~ts message.", [ "formatted" ] ),

	trace_utils:info( "I am an info simple message." ),
	trace_utils:info_fmt( "I am an info ~ts message.", [ "formatted" ] ),

	trace_utils:notice( "I am a notice simple message." ),
	trace_utils:notice_fmt( "I am a notice ~ts message.", [ "formatted" ] ),

	trace_utils:warning( "I am a warning simple message." ),
	trace_utils:warning_fmt( "I am a warning ~ts message.", [ "formatted" ] ),

	trace_utils:error( "I am an error simple message." ),
	trace_utils:error_fmt( "I am an error ~ts message.", [ "formatted" ] ),

	trace_utils:critical( "I am a critical simple message." ),
	trace_utils:critical_fmt( "I am a critical ~ts message.", [ "formatted" ] ),

	trace_utils:alert( "I am an alert simple message." ),
	trace_utils:alert_fmt( "I am an alert ~ts message.", [ "formatted" ] ),

	trace_utils:emergency( "I am an emergency simple message." ),
	trace_utils:emergency_fmt( "I am an emergency ~ts message.",
							   [ "formatted" ] ),

	_NonCrasherPid = spawn(
		fun() ->
			trace_utils:notice_fmt( "Hello from non-crasher test process ~w.",
									[ self() ] )
		end ),

	_CrasherPid = spawn(

		fun() ->
			trace_utils:notice_fmt( "Hello from crasher test process ~w.",
									[ self() ] ),
			basic_utils:crash()
		end ),

	trace_utils:debug( "(test waiting a bit; the next badarith error "
					   "is intentional)" ),
	timer:sleep( 2000 ),

	test_facilities:stop().
