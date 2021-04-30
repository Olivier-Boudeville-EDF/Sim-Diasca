% Copyright (C) 2015-2021 Olivier Boudeville
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


% Unit tests for the time utils toolbox.
%
% See the time_utils.erl tested module.
%
-module(time_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").




-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	Months = [ time_utils:month_to_string( M ) || M <- lists:seq( 0, 13 ) ],

	test_facilities:display( "Months: ~ts",
							 [ text_utils:strings_to_string( Months ) ] ),

	InitialTimestamp = time_utils:get_timestamp(),

	true = time_utils:is_timestamp( InitialTimestamp ),
	false = time_utils:is_timestamp( { {0,0,0}, true } ),

	InitialPreciseTimestamp = time_utils:get_precise_timestamp(),

	test_facilities:display( "Timestamp is ~ts.", [
		time_utils:get_textual_timestamp( InitialTimestamp ) ] ),

	test_facilities:display( "Timestamp for path is ~ts.", [
		time_utils:get_textual_timestamp_for_path( InitialTimestamp ) ] ),

	SomeTimestamp = { {2017,5,20}, {12,00,17} },
	true = time_utils:is_timestamp( SomeTimestamp ),

	"2017-05-20 12:00:17" =
		time_utils:get_textual_timestamp_with_dashes( SomeTimestamp ),

	TextualTimeStamp = "14/4/2011 18:48:51",
	test_facilities:display( "Parsed timestamp for '~ts' is ~p.", [
		TextualTimeStamp,
		time_utils:string_to_timestamp( TextualTimeStamp ) ] ),

	FinalPreciseTimestamp = time_utils:get_precise_timestamp(),

	test_facilities:display( "Precise duration in test is ~p ms.", [
		time_utils:get_precise_duration( InitialPreciseTimestamp,
										 FinalPreciseTimestamp ) ] ),

	FirstDate = { 2015, 7, 27 },
	SecondDate = { 2015, 8, 4 },

	% One week and one day later (checked as correct):
	DayDifference = 8,
	SecondDate = time_utils:get_date_after( FirstDate, DayDifference ),

	"Tuesday" = time_utils:week_day_to_string( SecondDate ),

	DayDifference = time_utils:get_date_difference( FirstDate, SecondDate ),

	test_facilities:stop().
