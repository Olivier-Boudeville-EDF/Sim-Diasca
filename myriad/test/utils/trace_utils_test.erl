% Copyright (C) 2017-2021 Olivier Boudeville
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


% Unit tests for the trace_utils toolbox.
%
% See the trace_utils.erl tested module.
%
-module(trace_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Setting the default Myriad logger handler." ),
	trace_utils:set_handler(),

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
