% Copyright (C) 2019-2021 Olivier Boudeville
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
% Creation date: Friday, July 19, 2019.


% Testing of WOOPER as an OTP active application, directly from within its code
% base (hence without needing to create a separate, mock-up test OTP release for
% that).
%
-module(wooper_otp_application_test).


% For run/0 export and al:
-include_lib("myriad/include/test_facilities.hrl").


% Actual test:
test_wooper_application( OrderedAppNames ) ->

	test_facilities:display( "Starting the WOOPER OTP active application." ),

	% We did not trap EXIT messages, as we wanted this test to crash (thanks to
	% the link below) in case of problem (and not to receive an EXIT message
	% bound not to be read).
	%
	% However this test was crashing even when stopping (normally) applications,
	% as apparently an OTP application has its child processes terminated with
	% reason 'shutdown' (not 'normal').
	%
	% So now this test process traps EXIT messages, and ensures that none
	% besides {'EXIT',P,shutdown}, P being the PID of the WOOPER class manager,
	% is received.
	%
	false = erlang:process_flag( trap_exit, true ),

	otp_utils:start_applications( OrderedAppNames ),

	test_facilities:display( "WOOPER version: ~p.",
				 [ system_utils:get_application_version( wooper ) ] ),

	% The top-level user process may not be aware that an OTP application fails
	% (ex: because its main process crashed), which is a problem for a test. So
	% here we link explicitly this test process to the WOOPER class manager, to
	% increase the chances of detecting any issue:

	WOOPERClassManagerPid = wooper_class_manager:get_existing_manager(),

	erlang:link( WOOPERClassManagerPid ),

	test_facilities:display( "Linked to WOOPER class manager ~w.",
							 [ WOOPERClassManagerPid ] ),

	% To test also a WOOPER module:

	TestClassname = 'class_Tiger',

	test_facilities:display( "Class filename corresponding to '~ts': '~ts'.",
			[ TestClassname, wooper:get_class_filename( TestClassname ) ] ),


	% Including WOOPER:
	test_facilities:display( "Stopping all user applications." ),
	otp_utils:stop_user_applications( OrderedAppNames ),


	test_facilities:display( "Checking pending messages." ),

	receive

		{ 'EXIT', WOOPERClassManagerPid, shutdown } ->
			ok

	end,

	% None expected to be left:
	basic_utils:check_no_pending_message(),

	test_facilities:display(
	  "Successful end of test of the WOOPER OTP application." ).



% Note that the wooper.app and myriad.app files will have to be found and used
% for this test to succeed: WOOPER and Myriad must be already available as
% prerequisite, fully-built OTP applications.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Build root directory from which sibling prerequisite applications may be
	% found:
	%
	BuildRootDir = "..",

	% No dependency specified in this test, yet they are managed:
	OrderedAppNames = otp_utils:prepare_for_execution( _ThisAppName=wooper,
													   BuildRootDir ),

	trace_utils:notice_fmt( "Resulting applications to start, in order: ~w.",
							[ OrderedAppNames ] ),

	test_wooper_application( OrderedAppNames ),

	test_facilities:stop().
