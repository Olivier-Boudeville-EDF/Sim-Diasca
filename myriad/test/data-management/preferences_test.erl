% Copyright (C) 2013-2021 Olivier Boudeville
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


% Unit tests for the preferences service.
%
% See the preferences.erl tested module.
%
-module(preferences_test).



% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Preferences while the service is not running: "
							 "~ts", [ preferences:to_string() ] ),

	% May not be called (automatic launching of the service whenever needed):
	preferences:start(),

	test_facilities:display( "Preferences after the service is just started: "
							 "~ts", [ preferences:to_string() ] ),

	TargetKey = test_key,

	test_facilities:display( "Value associated to ~ts before it is set "
		"from test: ~p", [ TargetKey, preferences:get( TargetKey ) ] ),

	Target_value = "This is a test value!",

	preferences:set( TargetKey, Target_value ),

	test_facilities:display( "Value associated to ~ts after it is set "
		"from test: ~p", [ TargetKey, preferences:get( TargetKey ) ] ),

	Target_value = preferences:get( TargetKey ),

	test_facilities:display( preferences:to_string() ),

	% Useless in the general case (permanent service):
	preferences:stop(),

	test_facilities:stop().
