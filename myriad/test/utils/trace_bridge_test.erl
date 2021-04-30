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
% Creation date: Wednesday, October 21, 2020.


% Unit tests for the trace_bridge toolbox, when no specific bridge is
% registered.
%
% See the trace_bridge.erl tested module.
%
% For a test with a bridge registered, refer, in Ceylan-Traces, to
% trace_bridging_test.erl.
%
-module(trace_bridge_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-export([ emit_traces/0 ]).


% Defined for external re-use:
emit_traces() ->

	% No bridge spec defined or used, no bridging registration here:

	trace_bridge:debug( "I am a debug simple message." ),
	trace_bridge:debug_fmt( "I am a debug ~ts message.", [ "formatted" ] ),

	trace_bridge:set_application_timestamp( "My applicative timestamp" ),

	trace_bridge:info( "I am an info simple message." ),
	trace_bridge:info_fmt( "I am an info ~ts message.", [ "formatted" ] ),

	trace_bridge:notice( "I am a notice simple message." ),
	trace_bridge:notice_fmt( "I am a notice ~ts message.", [ "formatted" ] ),

	trace_bridge:warning( "I am a warning simple message." ),
	trace_bridge:warning_fmt( "I am a warning ~ts message.", [ "formatted" ] ),

	trace_bridge:error( "I am an error simple message." ),
	trace_bridge:error_fmt( "I am an error ~ts message.", [ "formatted" ] ),

	trace_bridge:critical( "I am a critical simple message." ),
	trace_bridge:critical_fmt( "I am a critical ~ts message.", 
							   [ "formatted" ] ),

	trace_bridge:alert( "I am an alert simple message." ),
	trace_bridge:alert_fmt( "I am an alert ~ts message.", [ "formatted" ] ),

	trace_bridge:emergency( "I am an emergency simple message." ),
	trace_bridge:emergency_fmt( "I am an emergency  ~ts message.",
								[ "formatted" ] ),

	trace_bridge:void( "I am a void simple message." ),
	trace_bridge:void_fmt( "I am a void ~ts message.", [ "formatted" ] ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the trace bridge, here with "
							 "no bridge registered." ),

	emit_traces(),

	test_facilities:stop().
