% Copyright (C) 2019-2021 Olivier Boudeville
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
% Creation date: Friday, July 19, 2019.


% Testing of Myriad as an OTP library application, directly from within its code
% base (hence without needing to create a separate, mock-up test OTP release for
% that).
%
-module(myriad_otp_application_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Actual test:
test_myriad_application( OrderedAppNames ) ->

	test_facilities:display( "Starting the Myriad OTP library application." ),
	otp_utils:start_applications( OrderedAppNames ),


	test_facilities:display( "Myriad version: ~p.",
		[ system_utils:get_application_version( myriad ) ] ),

	test_facilities:display( "Current user name: '~ts'.",
							 [ system_utils:get_user_name() ] ),


	% Including Myriad:
	test_facilities:display( "Stopping all user applications." ),
	otp_utils:stop_user_applications( OrderedAppNames ),

	test_facilities:display(
	  "Successful end of test of the Myriad OTP application." ).



% Note that the myriad.app file will have to be found and used for this test to
% succeed: Myriad must be already available as a prerequisite, fully-built OTP
% application.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Build root directory from which prerequisite sibling applications may be
	% found:
	%
	BuildRootDir = file_utils:join( "..", ".." ),

	% No dependency specified in this test, yet they are managed:
	OrderedAppNames =
		otp_utils:prepare_for_execution( _ThisAppName=myriad, BuildRootDir ),

	trace_utils:notice_fmt( "Resulting applications to start, in order: ~w.",
							[ OrderedAppNames ] ),

	test_myriad_application( OrderedAppNames ),

	test_facilities:stop().
