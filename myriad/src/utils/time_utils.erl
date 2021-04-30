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
% Creation date: Friday, July 24, 2015.



% Gathering of time management facilities.
%
% See time_utils_test.erl for the corresponding test.
%
-module(time_utils).



% Implementation notes:
%
% Native support in Erlang for time-related operations is mostly located in the
% calendar module.
%
% A typical date format of interest here is: "Friday, July 24, 2015".
%
% A month is a positive integer, a canonical month is in [1,12].


% Day management support:
-export([ is_bank_holiday/2, get_bank_holidays_for/2,
		  find_common_bank_holidays/3 ]).


% Week management support:
-export([ get_week_day/1, week_day_to_string/1 ]).


% Month management support:
-export([ canonicalise_month/1, check_month_canonical/1, check_month_order/2,
		  month_to_string/1 ]).


% Date support:
-export([ compare_dates/2, check_date_order/2, get_date_difference/2 ]).


% As calendar:daynum/0 is not exported:
%
% (Monday is 1, Tuesday is 2, etc.)
%
% Such numerical values are useful to operate based on ranges.
%
-type day_index() :: 1..7.


% User-friendly atom-based version of day_index/0:
-type week_day() :: 'monday' | 'tuesday' | 'wednesday' | 'thursday'
				  | 'friday' | 'saturday' | 'sunday'.


% Calendar date; used to be less precise calendar:date():
-type date() :: { year(), canonical_month(), canonical_day() }.


% Calendar date within a year:
-type date_in_year() :: { canonical_month(), canonical_day() }.


% Time in the day; used to be {hour(), minute(), second()} or calendar:time():
-type time() :: { canonical_hour(), canonical_minute(), canonical_second() }.


% Also known as Gregorian milliseconds:
-type ms_since_year_0() :: unit_utils:milliseconds().

% POSIX conventions:
-type ms_since_epoch() :: unit_utils:milliseconds().

% The internal, duration-friendly monotonic time:
-type ms_monotonic() :: unit_utils:milliseconds().

-type ms_duration() :: unit_utils:milliseconds().


% Day/Hour/Minute/Second duration, for example used with MTTF (not necessarily
% in a canonical form, for example more than 24 hours or 60 minutes can be
% specified):
%
-type dhms_duration() :: { days(), hours(), minutes(), seconds() }.


-export_type([ day_index/0, week_day/0, date/0, date_in_year/0, time/0,
			   ms_since_year_0/0, ms_since_epoch/0, ms_monotonic/0,
			   ms_duration/0,
			   dhms_duration/0 ]).


% Basics:
-export([ get_textual_date/1, from_posix_timestamp/1 ]).


% For rough, averaged conversions:
-export([ years_to_seconds/1, months_to_seconds/1, weeks_to_seconds/1,
		  days_to_seconds/1, hours_to_seconds/1,
		  dhms_to_seconds/1, seconds_to_dhms/1, time_to_seconds/1 ]).


% DHMS-related:
-export([ is_dhms_duration/1, string_to_dhms/1 ]).


% Duration-related section.
-export([ get_intertime_duration/2,
		  duration_to_string/1, duration_to_string/2,
		  duration_to_french_string/1,
		  time_out_to_string/1, time_out_to_string/2 ]).


% Shall be a bit cheaper:
-compile( { inline, [ get_timestamp/0 ] } ).


% Direct time-related section.
-export([ get_monotonic_time/0, get_system_time/0 ]).


% Timestamp-related section.
%
% Note: the base Erlang term comparison allows to compare directly timestamps;
% for example, if T1={{2019,8,26},{17,1,16}} and T2={{2019,8,26},{17,2,5}}, then
% T1 < T2 ("T1 is before T2") is true.
%
-export([ get_timestamp/0,
		  get_epoch_timestamp/0, get_epoch_milliseconds_since_year_0/0,
		  is_timestamp/1, check_timestamp/1, check_maybe_timestamp/1,
		  get_textual_timestamp/0, get_textual_timestamp/1,
		  get_user_friendly_textual_timestamp/1,
		  get_french_textual_timestamp/1,
		  get_time2_textual_timestamp/0, get_time2_textual_timestamp/1,
		  get_textual_timestamp_for_path/0, get_textual_timestamp_for_path/1,
		  get_textual_timestamp_with_dashes/1,
		  timestamp_to_string/1, short_string_to_timestamp/1,
		  gregorian_ms_to_timestamp/1,
		  string_to_timestamp/1, dhms_to_string/1, time_of_day_to_string/1,
		  timestamp_to_seconds/0, timestamp_to_seconds/1,
		  timestamp_to_weekday/1, date_to_weekday/1,
		  local_to_universal_time/1, universal_to_local_time/1,
		  offset_timestamp/2, next_month/1,
		  get_duration/1, get_duration/2,
		  get_duration_since/1,
		  get_textual_duration/2, get_french_textual_duration/2,
		  get_precise_timestamp/0, get_precise_duration/2,
		  get_precise_duration_since/1,
		  get_date_after/2 ]).



% Used to be calendar:datetime(), now uses our types:
-type timestamp() :: { date(), time() }.


-type precise_timestamp() :: { megaseconds(), seconds(), microseconds() }.


% Cannot find the definition of the built-in timeout() type:
-type time_out() :: 'infinity' | milliseconds().


% Designates an integer number of seconds since or before Unix time epoch, which
% is 1970-01-01 00:00 UTC.
%
-type posix_seconds() :: integer().


-export_type([ timestamp/0, precise_timestamp/0, time_out/0, posix_seconds/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type microseconds() :: unit_utils:microseconds().
-type milliseconds() :: unit_utils:milliseconds().
-type seconds() :: unit_utils:seconds().
-type float_seconds() :: unit_utils:float_seconds().
-type megaseconds() :: unit_utils:megaseconds().


-type minutes() :: unit_utils:minutes().
-type hours() :: unit_utils:hours().
-type days() :: unit_utils:days().
-type weeks() :: unit_utils:weeks().

-type month() :: unit_utils:month().
-type months() :: unit_utils:months().
-type absolute_month() :: unit_utils:absolute_month().

-type year() :: unit_utils:year().
-type years() :: unit_utils:years().

-type canonical_second() :: unit_utils:canonical_second().
-type canonical_minute() :: unit_utils:canonical_minute().
-type canonical_hour() :: unit_utils:canonical_hour().
-type canonical_day() :: unit_utils:canonical_day().
-type canonical_month() :: unit_utils:canonical_month().

-type country() :: locale_utils:country().



% Time in the day; used to be {hour(), minute(), second()} or calendar:time():
-type time() :: { canonical_hour(), canonical_minute(), canonical_second() }.



% Returns a string corresponding to the specified date, like: "30/11/2009".
-spec get_textual_date( date() ) -> ustring().
get_textual_date( { Year, Month, Day } ) ->
	io_lib:format( "~B/~B/~B", [ Day, Month, Year ] ).



% Converts specified POSIX timestamp (typically the one obtained through
% file-level operations such as file_utils:get_last_modification_time/1) into a
% standard timestamp.
%
-spec from_posix_timestamp( posix_seconds() ) -> timestamp().
from_posix_timestamp( PosixTimestamp ) ->

	% Relative to 1/1/1970 0:0:0:
	{ _Date={ Post1970Year, Month, Day }, Time } =
		calendar:gregorian_seconds_to_datetime( PosixTimestamp ),

	% For services (ex: filesystem) typically returning their timestamp in local
	% time:
	%
	calendar:universal_time_to_local_time(
	  { { 1970 + Post1970Year, Month, Day }, Time } ).



% Month section.


% Canonicalises specified month.
-spec canonicalise_month( month() ) -> canonical_month().
canonicalise_month( M ) when is_integer( M ) andalso M >= 0 ->

	% Positive guard useful, as -1 rem 12 = -1 (hence not in [0,11]).

	% In [1;12]:
	case M rem 12 of

		0 ->
			12;

		Other ->
			Other

	end.



% Checks that specified month is a canonical one.
-spec check_month_canonical( month() ) -> void().
check_month_canonical( Month ) when is_integer( Month ) andalso Month >= 1
									andalso Month =< 12 ->
	ok;

check_month_canonical( Month ) ->
	throw( { non_canonical_month, Month } ).



% Ensures that the starting canonical month is strictly before the stopping one.
-spec check_month_order( absolute_month(), absolute_month() ) -> void().
check_month_order( Start={ StartYear, StartMonth },
				   Stop= { StopYear, StopMonth } ) ->

	check_month_canonical( StartMonth ),
	check_month_canonical( StopMonth ),

	case ( StartYear < StopYear ) orelse ( StartYear =:= StopYear andalso
										   StartMonth < StopMonth ) of

		true ->
			ok;

		_False ->
			throw( { wrong_month_order, Start, Stop } )

	end.



% Converts a month (an integer in [1,12] or a 12-multiple thereof, like 23) into
% its common name.
%
-spec month_to_string( month() ) -> ustring().
month_to_string( _MonthIndex=1 ) ->
	"January";

month_to_string( _MonthIndex=2 ) ->
	"February";

month_to_string( _MonthIndex=3 ) ->
	"March";

month_to_string( _MonthIndex=4 ) ->
	"April";

month_to_string( _MonthIndex=5 ) ->
	"May";

month_to_string( _MonthIndex=6 ) ->
	"June";

month_to_string( _MonthIndex=7 ) ->
	"July";

month_to_string( _MonthIndex=8 ) ->
	"August";

month_to_string( _MonthIndex=9 ) ->
	"September";

month_to_string( _MonthIndex=10 ) ->
	"October";

month_to_string( _MonthIndex=11 ) ->
	"November";

month_to_string( _MonthIndex=12 ) ->
	"December";

month_to_string( MonthIndex ) ->
	month_to_string( canonicalise_month( MonthIndex ) ).



% Tells whether, for specified country, the specified date is a bank holiday.
-spec is_bank_holiday( date(), country() ) -> boolean().
is_bank_holiday( Date={ _D, _M, Y }, Country ) ->
	lists:member( Date, get_bank_holidays_for( Y, Country ) ).



% Returns a (non chronologically-ordered) list of the dates of the bank
% holidays, for specified year and country.
%
% Sources for France: https://kalendrier.ouest-france.fr/jours-feries/2020.html
% ("fixed" days have then been factored).
%
-spec get_bank_holidays_for( year(), country() ) -> [ date_in_year() ].
get_bank_holidays_for( _Year=2020, Country=france ) ->
	get_fixed_bank_holidays_for( Country )
		++ [ {1,6}, {12,4}, {13,4}, {21,5}, {31,5} ];

get_bank_holidays_for( _Year=2021, Country=france ) ->
	get_fixed_bank_holidays_for( Country )
		++ [ {4,4}, {5,4}, {13,5}, {23,5}, {24,5} ];

get_bank_holidays_for( _Year=2022, Country=france ) ->
	get_fixed_bank_holidays_for( Country )
		++ [ {5,6}, {6,6}, {17,4}, {18,4}, {26,5} ];

get_bank_holidays_for( _Year=2023, Country=france ) ->
	get_fixed_bank_holidays_for( Country )
		++ [ {9,4}, {10,4}, {18,5}, {28,5}, {29,5} ];

get_bank_holidays_for( _Year=2024, Country=france ) ->
	get_fixed_bank_holidays_for( Country )
		++ [ {1,4}, {9,5}, {19,5}, {20,5}, {31,3} ];

get_bank_holidays_for( _Year=2025, Country=france ) ->
	get_fixed_bank_holidays_for( Country )
		++ [ {8,6}, {9,6}, {20,4}, {21,4}, {29,5} ];

get_bank_holidays_for( Year, Country ) ->
	throw( { no_info_for, Year, Country } ).



% Returns the fixed bank holidays (whose date is constant from one year to
% another) for specified country.
%
-spec get_fixed_bank_holidays_for( country() ) -> [ date_in_year() ].
get_fixed_bank_holidays_for( _Country=france ) ->
	% Computed thanks to:
	% time_utils:find_common_bank_holidays( 2020, 2026, france ).
	% (prior to factoring them of course)
	%
	[ {1,1}, {1,5}, {1,11}, {8,5}, {11,11}, {14,7}, {15,8}, {25,12} ].



% Returns the days-of-the-year that are common to the whole year range (start
% year included, stop one excluded).
%
-spec find_common_bank_holidays( year(), year(), country() ) ->
										[ date_in_year() ].
find_common_bank_holidays( StartYear, StopYear, Country ) ->
	AccSet = set_utils:new( get_bank_holidays_for( StartYear, Country ) ),
	find_common_bank_holidays_helper( StartYear+1, StopYear, Country, AccSet ).


% (helper)
find_common_bank_holidays_helper( StopYear, StopYear, _Country, AccSet ) ->
	set_utils:to_list( AccSet );

find_common_bank_holidays_helper( CurrentYear, StopYear, Country, AccSet ) ->
	YearSet = set_utils:new( get_bank_holidays_for( CurrentYear, Country ) ),

	NewAccSet = set_utils:intersection( AccSet, YearSet ),

	find_common_bank_holidays_helper( CurrentYear+1, StopYear, Country,
									  NewAccSet ).



% Returns the symbol (atom) corresponding to specified week day index.
-spec get_week_day( day_index() ) -> week_day().
get_week_day( _DayIndex=1 ) ->
	monday;

get_week_day( _DayIndex=2 ) ->
	tuesday;

get_week_day( _DayIndex=3 ) ->
	wednesday;

get_week_day( _DayIndex=4 ) ->
	thursday;

get_week_day( _DayIndex=5 ) ->
	friday;

get_week_day( _DayIndex=6 ) ->
	saturday;

get_week_day( _DayIndex=7 ) ->
	sunday.



% Returns the common name of a week day (ex: "Tuesday") based on the specified
% date or on the specified index in the week.
%
-spec week_day_to_string( date() | day_index() ) -> ustring().
week_day_to_string( Date={ _Y, _M, _D } ) ->
	Day = calendar:day_of_the_week( Date ),
	week_day_to_string( Day ) ;

week_day_to_string( _DayIndex=1 ) ->
	"Monday";

week_day_to_string( _DayIndex=2 ) ->
	"Tuesday";

week_day_to_string( _DayIndex=3 ) ->
	"Wednesday";

week_day_to_string( _DayIndex=4 ) ->
	"Thursday";

week_day_to_string( _DayIndex=5 ) ->
	"Friday";

week_day_to_string( _DayIndex=6 ) ->
	"Saturday";

week_day_to_string( _DayIndex=7 ) ->
	"Sunday";

week_day_to_string( DayIndex ) ->
	week_day_to_string( ( DayIndex rem 7 ) + 1 ).




% Date section.


% Checks that specified date is a canonical one.
-spec check_date_canonical( date() ) -> void().
check_date_canonical( _Date={ Year, Month, Day } ) when
	  is_integer( Year ) andalso is_integer( Month ) andalso
	  is_integer( Day ) andalso Month >= 1 andalso Month =< 12
	  andalso Day >= 1 andalso Day =< 31 ->
	ok;

check_date_canonical( Date ) ->
	throw( { non_canonical_date, Date } ).




% Compares the specified two dates: tells whether the first date is strictly
% before, after or the same as the second one.
%
% Note: both dates are expected to be in canonical form (ex: not more 12 months
% or 31 days in the specified date).
%
-spec compare_dates( date(), date() ) -> basic_utils:comparison_result().
compare_dates( FirstDate, SecondDate ) ->

	check_date_canonical( FirstDate ),
	check_date_canonical( SecondDate ),

	compare_helper( FirstDate, SecondDate ).



compare_helper( _FirstDate={ Yf, _Mf, _Df },
				_SecondDate={ Ys, _Ms, _Ds } ) when Yf < Ys ->
	lower;

compare_helper( _FirstDate={ Yf, _Mf, _Df },
				_SecondDate={ Ys, _Ms, _Ds } ) when Yf > Ys ->
	higher;

% From here, Yf =:= Ys:
compare_helper( _FirstDate={ _Y, Mf, _Df },
				_SecondDate={ _Y, Ms, _Ds } )  when Ms < Mf ->
	lower;

compare_helper( _FirstDate={ _Y, Mf, _Df },
				_SecondDate={ _Y, Ms, _Ds } ) when Ms > Mf ->
	higher;

% From here, Yf =:= Ys and Mf =:= Ms:
compare_helper( _FirstDate={ _Y, _M, Df },
				_SecondDate={ _Y, _M, Ds } )  when Df < Ds ->
	lower;

compare_helper( _FirstDate={ _Y, _M, Df },
				_SecondDate={ _Y, _M, Ds } ) when Df > Ds ->
	higher;

% Df =:= Ds, equality:
%compare_helper( _FirstDate={ _Y, _M, _D },
%				_SecondDate={ _Y, _M, _D } ) ->
compare_helper( _FirstDate, _SecondDate ) ->
	equal.



% Ensures that the starting canonical date is strictly before the stopping one.
%
% Note: both dates are expected to be in canonical form (ex: not more than 12
% months or 31 days in the specified date).
%
-spec check_date_order( date(), date() ) -> void().
check_date_order( StartDate, StopDate ) ->

	case compare_dates( StartDate, StopDate ) of

		lower ->
			ok;

		% Equal or higher:
		_ ->
			throw( { wrong_date_order, StartDate, StopDate } )

	end.



% Returns the signed duration, in days, between the two specified dates.
-spec get_date_difference( date(), date() ) -> days().
get_date_difference( FirstDate, SecondDate ) ->

	FirstDayCount = calendar:date_to_gregorian_days( FirstDate ),
	SecondDayCount = calendar:date_to_gregorian_days( SecondDate ),

	SecondDayCount - FirstDayCount.



% Time conversion section, based only on rather approximated values (ex: the
% number of days varies from a year to another, so any constant value cannot be
% accurate).


% Converts a duration in years into a duration in seconds, supposing a year has
% 365 days and 6 hours (i.e. a quarter of one day, to account for leap years).
%
-spec years_to_seconds( years() ) -> float_seconds().
years_to_seconds( YearDuration ) ->
	% 365.25 days per year one average here:
	YearDuration * 365.25 * 24 * 3600.


% Converts a duration in months into a duration in seconds, supposing a month is
% 1/12 of an average year.
%
-spec months_to_seconds( months() ) -> float_seconds().
months_to_seconds( MonthDuration ) ->
	MonthDuration * 365.25 / 12 * 24 * 3600.


% Converts a duration in weeks into a duration in seconds.
-spec weeks_to_seconds( weeks() ) -> seconds().
weeks_to_seconds( WeekDuration ) ->
	WeekDuration * 7 * 24 * 3600.


% Converts a duration in days into a duration in seconds.
-spec days_to_seconds( days() ) -> seconds().
days_to_seconds( DayDuration ) ->
	DayDuration * 24 * 3600.


% Converts a duration in hours into a duration in seconds.
-spec hours_to_seconds( unit_utils:hours() ) -> seconds().
hours_to_seconds( HourDuration ) ->
	HourDuration * 3600.



% Tells whether specified term is a DHMS duration.
%
% Note: does not check whether its components are in canonical form (ex: Hours
% in [0,23]).
%
-spec is_dhms_duration( term() ) -> boolean().
is_dhms_duration( { Days, Hours, Minutes, Seconds } ) when
	  is_integer( Days ) andalso is_integer( Hours )
	  andalso is_integer( Minutes ) andalso is_integer( Seconds ) ->
	true;

is_dhms_duration( _Other ) ->
	false.



% Converts specified string (ex: "113j0h10m3s" for a French version,
% "1d12h0m0s" for an English one) to a DHMS duration.
%
-spec string_to_dhms( ustring() ) -> dhms_duration().
string_to_dhms( DurationString ) ->

	TrimmedDurStr = text_utils:trim_whitespaces( DurationString ),

	% For the days, try the French convention first:
	{ D, DRest } = case text_utils:split_at_first( $j, TrimmedDurStr ) of

		none_found ->
			% Enlish one maybe?
			case text_utils:split_at_first( $d, TrimmedDurStr ) of

				none_found ->
					{ 0, TrimmedDurStr };

				% Ex: { "113", "0h10m3s" } (English version)
				{ EnDStr, EnNonDStr } ->
					{ text_utils:string_to_integer( EnDStr ), EnNonDStr }

			end;

		% Ex: { "113", "0h10m3s" } (French version)
		{ FrDStr, FrNonDStr } ->
			{ text_utils:string_to_integer( FrDStr ), FrNonDStr }

	end,


	{ H, HRest } = case text_utils:split_at_first( $h, DRest ) of

		none_found ->
			{ 0, DRest };

		% Ex: { "0", "10m3s" }
		{ HStr, NonHStr } ->
			{ text_utils:string_to_integer( HStr ), NonHStr }

	end,


	{ M, MRest } = case text_utils:split_at_first( $m, HRest ) of

		none_found ->
			{ 0, HRest };

		% Ex: { "10", "3s" }
		{ MStr, NonMStr } ->
			{ text_utils:string_to_integer( MStr ), NonMStr }

	end,


	S = case text_utils:split_at_first( $s, MRest ) of

		none_found ->
			0;

		% Ex: { "3", "" }
		{ SStr, _NonSStr="" } ->
			text_utils:string_to_integer( SStr )

	end,

	{ D, H, M, S }.





% Converts a DHMS duration (in Days/Hours/Minutes/Seconds) into a duration in
% seconds.
%
-spec dhms_to_seconds( dhms_duration() ) -> seconds().
dhms_to_seconds( { Days, Hours, Minutes, Seconds } ) ->
	Seconds + 60 * ( Minutes + 60 * ( Hours + 24 * Days ) ).



% Converts a duration in seconds into a DHMS duration (in
% Days/Hours/Minutes/Seconds).
%
seconds_to_dhms( FullSeconds ) ->
	SecsPerDay= 24 * 3600,
	DayCount = FullSeconds div SecsPerDay,
	SecsAfterDays = FullSeconds rem SecsPerDay,

	SecsPerHour = 3600,
	HourCount = SecsAfterDays div SecsPerHour,
	SecsAfterHours = SecsAfterDays rem SecsPerHour,

	SecsPerMin = 60,
	MinCount = SecsAfterHours div SecsPerMin,
	SecsAfterMin = SecsAfterHours rem SecsPerMin,

	{ DayCount, HourCount, MinCount, SecsAfterMin }.



% Converts a HMS duration (in Hours/Minutes/Seconds) into a duration in seconds.
-spec time_to_seconds( time() ) -> seconds().
time_to_seconds( { Hours, Minutes, Seconds } ) ->
	Seconds + 60 * ( Minutes + 60 * Hours ).



% Time section.


% Returns the signed duration, in integer seconds, between the two specified
% times.
%
% A positive duration will be returned iff the first specified time is before
% the second one.
%
-spec get_intertime_duration( time(), time() ) -> seconds().
get_intertime_duration( { H1, M1, S1 }, { H2, M2, S2 } ) ->
	( ( H2 - H1 ) * 60 + ( M2 - M1 ) ) * 60 + ( S2 - S1 ).



% Direct time-related section.


% Returns the current time, expressed in (absolute) internal time (logically
% equivalent to a millisecond-based timestamp).
%
% This is a "cheap" yet monotonic (never going backward) time, thus used
% internally, mostly useful to determine durations.
%
% Note: due to the VM time offset, this is probably a (large) negative number
% (of milliseconds), and this is not a problem.
%
-spec get_monotonic_time() -> ms_monotonic().
get_monotonic_time() ->

	% Often the native precision is one nanosecond:
	% erlang:convert_time_unit(1, second, native) = 1 000 000 000

	% The starting point of that time does not matter here (used just to compute
	% durations):
	%
	erlang:monotonic_time( millisecond ).



% Returns the (VM) system time (i.e. monotonic + time offset), in milliseconds
% since the Epoch.
%
-spec get_system_time() -> ms_since_epoch().
get_system_time() ->
	erlang:system_time( millisecond ).




% Timestamp section.



% Timestamp-related functions.



% Returns a timestamp tuple describing now, i.e. the current time, the time zone
% and Daylight Saving Time correction depending on the underlying OS.
%
% Ex: { {Year, Month, Day}, {Hour, Minute, Second} } = get_timestamp()
% may return { {2007,9,6}, {15,9,14} }.
%
-spec get_timestamp() -> timestamp().
get_timestamp() ->

	% Was: { erlang:date(), erlang:time() }.
	%
	% Better:
	%
	% (see also http://erlang.org/doc/apps/erts/time_correction.html)
	%
	erlang:localtime().



% Returns the timestamp of the (UNIX) Epoch, defined to be 00:00:00 UTC,
% 1970-01-01.
%
-spec get_epoch_timestamp() -> timestamp().
get_epoch_timestamp() ->
	{ { 1970, 1, 1 }, { 0, 0, 0 } }.


% Returns the number of milliseconds of the Epoch since year 0.
get_epoch_milliseconds_since_year_0() ->
	1000 * calendar:datetime_to_gregorian_seconds( get_epoch_timestamp() ).



% Returns whether specified term is a legit timestamp.
%
% Useful to vet user-specified timestamps.
%
-spec is_timestamp( term() ) -> boolean().
is_timestamp( { Date={ Y, M, D }, _Time={ Hour, Min, Sec } } )
  when is_integer( Y ) andalso is_integer( M ) andalso is_integer( D )
	   andalso is_integer( Hour ) andalso is_integer( Min )
	   andalso is_integer( Sec ) andalso Hour =< 24 andalso Min =< 60
	   andalso Sec =< 60 ->
	calendar:valid_date( Date );

is_timestamp( _Other ) ->
	false.



% Checks that specified term is a timestamp indeed.
-spec check_timestamp( timestamp() ) -> void().
check_timestamp( Term ) ->

	case is_timestamp( Term ) of

		true ->
			ok;

		false ->
			throw( { not_a_timestamp, Term } )

	end.



% Checks that specified term is a maybe-timestamp indeed.
-spec check_maybe_timestamp( maybe( timestamp() ) ) -> void().
check_maybe_timestamp( _Term=undefined ) ->
	ok;

check_maybe_timestamp( Term ) ->

	case is_timestamp( Term ) of

		true ->
			ok;

		false ->
			throw( { not_a_maybe_timestamp, Term } )

	end.



% Returns a string corresponding to the current timestamp, like:
% "2009/9/1 11:46:53".
%
% Note that the display order here is YY-MM-DD (same as when specifying the
% timestamp), as opposed to DD-MM-YY, which is maybe more usual.
%
-spec get_textual_timestamp() -> ustring().
get_textual_timestamp() ->
	get_textual_timestamp( get_timestamp() ).


% Returns a (clear, non-ambiguous) string corresponding to the specified
% timestamp, like: "2009/9/1 11:46:53".
%
-spec get_textual_timestamp( timestamp() ) -> ustring().
get_textual_timestamp( { { Year, Month, Day }, { Hour, Minute, Second } } ) ->
	io_lib:format( "~B/~B/~B ~B:~2..0B:~2..0B",
				   [ Year, Month, Day, Hour, Minute, Second ] ).


% Returns a string corresponding to the specified timestamp in a user-friendly
% manner, like: "Wednesday, January 6, 2021 at 11:46:53".
%
get_user_friendly_textual_timestamp( { Date={ Year, Month, Day }, Time } ) ->
	io_lib:format( "~ts, ~ts ~B, ~B, at ~ts",
		[ week_day_to_string( Date ), month_to_string( Month ), Day,
		  Year, time_of_day_to_string( Time ) ] ).


% Returns a string corresponding to the specified timestamp expressed in French,
% like: "le 1/9/2009, à 11h46m53".
%
-spec get_french_textual_timestamp( timestamp() ) -> ustring().
get_french_textual_timestamp( { { Year, Month, Day },
								{ Hour, Minute, Second } } ) ->

	%trace_utils:debug_fmt( "le ~B/~B/~B, à ~Bh~2..0Bm~2..0Bs",
	%					   [ Day, Month, Year, Hour, Minute, Second ] ),

	case Second of

		0 ->
			case Minute of

				0 ->
					io_lib:format( "le ~B/~B/~B, à ~Bh",
								   [ Day, Month, Year, Hour ] );

				_ ->
					io_lib:format( "le ~B/~B/~B, à ~Bh~2..0B",
								   [ Day, Month, Year, Hour, Minute ] )

			end;

		_ ->
			io_lib:format( "le ~B/~B/~B, à ~Bh~2..0Bm~2..0Bs",
						   [ Day, Month, Year, Hour, Minute, Second ] )

	end.



% Returns a string corresponding to the current timestamp expressed as the
% "%time2" Date and time format, i.e. "yyyy-mm-dd hh-mm-ss"; for example:
% "2020-01-01 00-01-22".
%
% Used by various web-related tools (see
% https://awstats.sourceforge.io/docs/awstats_config.html#LogFormat and
% https://awstats.sourceforge.io/docs/awstats_faq.html#PERSONALIZEDLOG).
%
-spec get_time2_textual_timestamp() -> ustring().
get_time2_textual_timestamp() ->
	get_time2_textual_timestamp( get_timestamp() ).



% Returns a string corresponding to the specified timestamp expressed as the
% "%time2" Date and time format, i.e. "yyyy-mm-dd hh-mm-ss"; for example:
% "2020-01-01 00-01-22".
%
% Used by various web-related tools (see
% https://awstats.sourceforge.io/docs/awstats_config.html#LogFormat and
% https://awstats.sourceforge.io/docs/awstats_faq.html#PERSONALIZEDLOG).
%
-spec get_time2_textual_timestamp( timestamp() ) -> ustring().
get_time2_textual_timestamp( { { Year, Month, Day },
							   { Hour, Minute, Second } } ) ->
	io_lib:format( "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
				   [ Year, Month, Day, Hour, Minute, Second ] ).



% Returns a string corresponding to the current timestamp and able to be a part
% of a path, like: "2010-11-18-at-13h-30m-35s".
%
-spec get_textual_timestamp_for_path() -> ustring().
get_textual_timestamp_for_path() ->
	get_textual_timestamp_for_path( get_timestamp() ).



% Returns a string corresponding to the specified timestamp and able to be a
% part of a path, like: "2010-11-18-at-13h-30m-35s".
%
-spec get_textual_timestamp_for_path( timestamp() ) -> ustring().
get_textual_timestamp_for_path( { { Year, Month, Day },
								  { Hour, Minute, Second } } ) ->
	io_lib:format( "~p-~p-~p-at-~Bh-~2..0Bm-~2..0Bs",
				   [ Year, Month, Day, Hour, Minute, Second ] ).


% Returns a string corresponding to the specified timestamp, with "dash"
% conventions (ex: used by jsgantt), like: "2017-05-20 12:00:17".
%
-spec get_textual_timestamp_with_dashes( timestamp() ) -> ustring().
get_textual_timestamp_with_dashes( { { Year, Month, Day },
									 { Hour, Minute, Second } } ) ->
	io_lib:format( "~B-~2..0B-~2..0B ~B:~2..0B:~2..0B",
				   [ Year, Month, Day, Hour, Minute, Second ] ).



% Alias of get_textual_timestamp/1, defined for clarity.
-spec timestamp_to_string( timestamp() ) -> ustring().
timestamp_to_string( Timestamp ) ->
	get_textual_timestamp( Timestamp ).





% Parses back a timestamp in the form of "14/4/11 18:48" ("11" for 2011, and
% with no seconds specified) into a timestamp(), i.e. { _Date={Year,Month,Day},
% _Time={Hour,Minute,Second} }.
%
-spec short_string_to_timestamp( ustring() ) -> timestamp().
short_string_to_timestamp( TimestampString ) ->

	%trace_utils:debug_fmt( "Converting short string '~ts' to timestamp.",
	%					   [ TimestampString ] ),

	case string:tokens( TimestampString, _Sep=" :/" ) of

		[ DayString, MonthString, YearString, HourString, MinuteString ] ->

			Day   = text_utils:string_to_integer( DayString ),
			Month = text_utils:string_to_integer( MonthString ),
			Year  = text_utils:string_to_integer( YearString ) + 2000,

			Hour   = text_utils:string_to_integer( HourString ),
			Minute = text_utils:string_to_integer( MinuteString ),

			 { { Year, Month, Day }, { Hour, Minute, _Second=0 } };

		_ ->
			throw( { timestamp_parsing_failed, TimestampString } )

	end.



% Converts (with a bit of approximation) the specified number of Gregorian
% milliseconds into a proper, user-level (local, system) timestamp.
%
-spec gregorian_ms_to_timestamp( ms_since_year_0() ) -> timestamp().
gregorian_ms_to_timestamp( GregorianMs ) ->

	GregorianSecs = round( GregorianMs / 1000 ),

	calendar:universal_time_to_local_time(
	  calendar:gregorian_seconds_to_datetime( GregorianSecs ) ).



% Parses back a timestamp in the form of "14/4/2011 18:48:51" into a
% timestamp(), i.e. { _Date={Year,Month,Day}, _Time={Hour,Minute,Second} }.
%
-spec string_to_timestamp( ustring() ) -> timestamp().
string_to_timestamp( TimestampString ) ->

	case string:tokens( TimestampString, _Sep=" :/" ) of

		[ DayString, MonthString, YearString, HourString, MinuteString,
		  SecondString ] ->

			Day   = text_utils:string_to_integer( DayString ),
			Month = text_utils:string_to_integer( MonthString ),
			Year  = text_utils:string_to_integer( YearString ),

			Hour   = text_utils:string_to_integer( HourString ),
			Minute = text_utils:string_to_integer( MinuteString ),
			Second = text_utils:string_to_integer( SecondString ),

			 { { Year, Month, Day }, { Hour, Minute, Second } };

		_ ->
			throw( { timestamp_parsing_failed, TimestampString } )

	end.




% Returns a textual description of the specified DHMS-based duration.
-spec dhms_to_string( dhms_duration() ) -> ustring().
dhms_to_string( DHMS ) ->
	duration_to_string( 1000 * dhms_to_seconds( DHMS ) ).



% Returns a textual description of the specified time of day.
-spec time_of_day_to_string( time() ) -> ustring().
time_of_day_to_string( _Time={ 0, 0, 0 } ) ->
	% To be understood as very beginning of day, as opposed to very end of it:
	"midnight";

time_of_day_to_string( _Time={ 12, 0, 0 } ) ->
	"noon";

time_of_day_to_string( _Time={ H, 0, 0 } ) ->
	io_lib:format( "~B", [ H ] );

time_of_day_to_string( _Time={ H, M, 0 } ) ->
	io_lib:format( "~B:~2..0B", [ H, M ] );

time_of_day_to_string( _Time={ H, M, S } ) ->
	io_lib:format( "~B:~2..0B:~2..0B", [ H, M, S ] ).



% Returns the number of seconds elapsed since year 0 and now.
%
% Useful for example to define an absolute reference in seconds and then only
% compare offsets to it.
%
-spec timestamp_to_seconds() -> seconds().
timestamp_to_seconds() ->
	timestamp_to_seconds( get_timestamp() ).



% Returns the week day (ex: 'Tuesday') corresponding to specified timestamp.
-spec timestamp_to_weekday( timestamp() ) -> week_day().
timestamp_to_weekday( _Timestamp={ Date, _Time } ) ->
	date_to_weekday( Date ).



% Returns the week day (ex: 'Tuesday') corresponding to specified date.
-spec date_to_weekday( date() ) -> week_day().
date_to_weekday( Date ) ->
	DayNum = calendar:day_of_the_week( Date ),
	get_week_day( DayNum ).



% Converts specified timestamp expressed in local time (thus with time zone and
% Daylight Saving Time) into a timestamp expressed in universal time (UTC).
%
% Note: designed to never fail, and performs a conversion as reasonably as
% possible (ex: due to DST, some timestamp in local time shall not exist, like
% when leaping forward of, say, an hour at spring; on the other way round, at
% fall, a given local timestamp may exist twice: before and after, when going
% back of, say, one hour).
%
-spec local_to_universal_time( timestamp() ) -> timestamp().
local_to_universal_time( LocalTimestamp ) ->

	case calendar:local_time_to_universal_time_dst( LocalTimestamp ) of

		% Normal case:
		[ UTCTimestamp ] ->
			UTCTimestamp;

		% Here due to DST we went back in time in that period (must be in Fall),
		% resulting in the same local timestamp to happen twice; we return here
		% (rather arbitrarily) the later, "newer" timestamp (i.e. the one
		% obtained once the DST offset has been subtracted; it should be higher
		% than the one with DST):
		%
		% (an average of the two could also have been considered)
		%
		[ UTCTimestampStillWithDST, UTCNoDSTTimestamp ] ->
			trace_utils:warning_fmt( "UTC conversion returning post-DST "
				"timestamp (~w) rather than the one still with DST (~w).",
				[ UTCNoDSTTimestamp, UTCTimestampStillWithDST ] ),
			UTCNoDSTTimestamp;

		% Here this local timestamp never existed (the local time leapt forward,
		% must be in Spring); we return an approximation thereof (as it is
		% evaluated forward on time in a DST period, it should be smaller than
		% if no DST had been applied):
		%
		[] ->
			SixHours = 6 * 3600,
			LaterLocalTimestamp =
				offset_timestamp( LocalTimestamp, SixHours ),
			LaterUTCTimestamp = local_to_universal_time( LaterLocalTimestamp ),
			Res = offset_timestamp( LaterUTCTimestamp, -SixHours ),
			trace_utils:error_fmt( "Non-existing local timestamp ~w converted "
								   "to UTC ~w.", [ LocalTimestamp, Res ] ),
			Res

	end.



% Converts specified timestamp expressed in universal time (UTC) into a
% timestamp expressedin local time (thus with time zone and Daylight Saving
% Time).
%
-spec universal_to_local_time( timestamp() ) -> timestamp().
universal_to_local_time( UTCTimestamp ) ->
	calendar:universal_time_to_local_time( UTCTimestamp ).



% Returns the number of seconds elapsed since year 0 and specified timestamp.
%
% Useful for example to define an absolute reference in seconds and then only
% compare offsets to it.
%
-spec timestamp_to_seconds( timestamp() ) -> seconds().
timestamp_to_seconds( Timestamp ) ->
	calendar:datetime_to_gregorian_seconds( Timestamp ).



% Offsets specified timestamp of specified (signed) duration: returns a
% timestamp translated accordingly.
%
-spec offset_timestamp( timestamp(), dhms_duration() | seconds() ) ->
								timestamp().
offset_timestamp( Timestamp, DHMS ) when is_tuple( DHMS ) ->
	offset_timestamp( Timestamp, dhms_to_seconds( DHMS ) );

offset_timestamp( Timestamp, Duration ) -> % when is_integer( Duration )

	% We cannot exceed the usual bounds (ex: specifying Month=13), as next
	% calendar function would throw a function_clause; so:

	NewSecs = calendar:datetime_to_gregorian_seconds( Timestamp ) + Duration,
	calendar:gregorian_seconds_to_datetime( NewSecs ).



% Returns the same timestamp as specified, except exactly one month later (hence
% not translated of a fixed duration).
%
% Note that this may still lead to invalid date, if the specified month has more
% days than the next (ex: January, 31 becoming then a nonsensical February, 31).
%
-spec next_month( timestamp() ) -> timestamp().
next_month( _Timestamp={ { Y, _M=12, D }, Time } ) ->
	{ { Y+1, 1, D }, Time };

next_month( _Timestamp={ { Y, M, D }, Time } ) ->
	{ { Y, M+1, D }, Time }.


% Returns the (signed) duration in seconds corresponding to the specified time.
-spec get_duration( time() ) -> seconds().
get_duration( { Hours, Minutes, Seconds } ) ->
	( Hours * 60 + Minutes ) * 60 + Seconds.



% Returns the (signed) duration in seconds between the two specified timestamps,
% using the first one as starting time and the second one as stopping time.
%
-spec get_duration( timestamp(), timestamp() ) -> seconds().
get_duration( FirstTimestamp, SecondTimestamp ) ->

	First  = calendar:datetime_to_gregorian_seconds( FirstTimestamp ),

	Second = calendar:datetime_to_gregorian_seconds( SecondTimestamp ),

	Second - First.



% Returns the (signed) duration in seconds between the specified start timestamp
% and the current time.
%
-spec get_duration_since( timestamp() ) -> seconds().
get_duration_since( StartTimestamp ) ->
	get_duration( StartTimestamp, get_timestamp() ).



% Returns an (english) textual description of the duration between the two
% specified timestamps.
%
-spec get_textual_duration( timestamp(), timestamp() ) -> ustring().
get_textual_duration( FirstTimestamp, SecondTimestamp ) ->

	% As duration_to_string/1 is smarter:
	% { Days, { Hour, Minute, Second } } = calendar:seconds_to_daystime(
	%	get_duration( FirstTimestamp, SecondTimestamp ) ),

	%lists:flatten( io_lib:format( "~B day(s), ~B hour(s), ~B minute(s) "
	%  "and ~B second(s)", [ Days, Hour, Minute, Second ] ) ).

	Duration = get_duration( FirstTimestamp, SecondTimestamp ),

	% Milliseconds:
	duration_to_string( 1000 * Duration ).



% Returns a textual description, in French, of the duration between the two
% specified timestamps.
%
-spec get_french_textual_duration( timestamp(), timestamp() ) -> ustring().
get_french_textual_duration( FirstTimestamp, SecondTimestamp ) ->

	Duration = get_duration( FirstTimestamp, SecondTimestamp ),

	% Milliseconds:
	duration_to_french_string( 1000 * Duration ).



% Returns an approximate textual description, in the specified language, of the
% specified duration, expected to be expressed as a number of milliseconds
% (integer; otherwise, if being floating-point, it will be rounded), or as the
% 'infinity' atom.
%
% Ex: for a duration of 150 012 ms, returns for English:
% "2 minutes, 30 seconds and 12 milliseconds".
%
% Can be fed directly with a time_out() value.
%
% See also: basic_utils:get_textual_duration/2.
%
-spec duration_to_string( milliseconds() | float() | 'infinity',
						  language_utils:human_language() ) -> ustring().
duration_to_string( Duration, _Lang=french ) ->
	duration_to_french_string( Duration );

% Default language is English:
duration_to_string( Duration, _Lang ) ->
	duration_to_string( Duration ).



% Returns an approximate textual (English) description of the specified
% duration, expected to be expressed as a signed number of milliseconds
% (integer; otherwise, if being floating-point, it will be rounded), or as the
% 'infinity' atom.
%
% Ex: for a duration of 150 012 ms, returns:
% "2 minutes, 30 seconds and 12 milliseconds".
%
% See also: basic_utils:get_textual_duration/2.
%
-spec duration_to_string( milliseconds() | float() | 'infinity' ) -> ustring().
duration_to_string( Milliseconds ) when is_float( Milliseconds )->
	duration_to_string( erlang:round( Milliseconds ) );

duration_to_string( Milliseconds )
  when is_integer( Milliseconds ) andalso Milliseconds < 0 ->
	"minus " ++ duration_to_string( -Milliseconds );

duration_to_string( Milliseconds ) when is_integer( Milliseconds )->

	FullSeconds = Milliseconds div 1000,

	{ Days, { Hours, Minutes, Seconds } } =
		calendar:seconds_to_daystime( FullSeconds ),

	ListWithDays = case Days of

		0 ->
			[];

		1 ->
			[ "1 day" ];

		_ ->
			[ io_lib:format( "~B days", [ Days ] ) ]

	end,

	ListWithHours = case Hours of

		0 ->
			ListWithDays;

		1 ->
			[ "1 hour" | ListWithDays ];

		_ ->
			[ io_lib:format( "~B hours", [ Hours ] )
			  | ListWithDays ]

	end,

	ListWithMinutes = case Minutes of

		0 ->
		  ListWithHours;

		1 ->
		  [ "1 minute" | ListWithHours ];

		_ ->
		  [ io_lib:format( "~B minutes", [ Minutes ] ) | ListWithHours ]

	end,

	ListWithSeconds = case Seconds of

		0 ->
			ListWithMinutes;

		1 ->
			[ "1 second" | ListWithMinutes ];

		_ ->
			[ io_lib:format( "~B seconds", [ Seconds ] ) | ListWithMinutes ]

	end,

	ActualMilliseconds = Milliseconds rem 1000,

	ListWithMilliseconds = case ActualMilliseconds of

		0 ->
			ListWithSeconds;

		1 ->
			[ "1 millisecond" | ListWithSeconds ];

		_ ->
			[ io_lib:format( "~B milliseconds", [ ActualMilliseconds ] )
			  | ListWithSeconds ]

	end,

	% Preparing for final display:
	case ListWithMilliseconds of

		[] ->
			"0 millisecond";

		[ OneElement ] ->
			OneElement;

		[ Smaller | Bigger ] ->
			text_utils:join( ", ",
							 lists:reverse( Bigger ) ) ++ " and " ++ Smaller

	end;

duration_to_string( infinity ) ->
	"infinity";

duration_to_string( Other ) ->
	throw( { invalid_duration, Other } ).



% Returns an approximate textual, French description of the specified duration,
% expected to be expressed as a number of milliseconds (integer; otherwise, if
% being floating-point, it will be rounded), or as the 'infinity' atom.
%
% Ex: for a duration of 150 012 ms, returns:
% "2 minutes, 30 secondes et 12 millisecondes".
%
% See also: basic_utils:get_textual_duration/2.
%
-spec duration_to_french_string( milliseconds() | float() | 'infinity' ) ->
										ustring().
duration_to_french_string( Milliseconds ) when is_float( Milliseconds )->
	duration_to_french_string( erlang:round( Milliseconds ) );

duration_to_french_string( Milliseconds ) when is_integer( Milliseconds )->

	FullSeconds = Milliseconds div 1000,

	{ Days, { Hours, Minutes, Seconds } } =
		calendar:seconds_to_daystime( FullSeconds ),

	ListWithDays = case Days of

		0 ->
			[];

		1 ->
			[ "1 jour" ];

		_ ->
			[ io_lib:format( "~B jours", [ Days ] ) ]

	end,

	ListWithHours = case Hours of

		0 ->
			ListWithDays;

		1 ->
			[ "1 heure" | ListWithDays ];

		_ ->
			[ io_lib:format( "~B heures", [ Hours ] )
			  | ListWithDays ]

	end,

	ListWithMinutes = case Minutes of

		0 ->
		  ListWithHours;

		1 ->
		  [ "1 minute" | ListWithHours ];

		_ ->
		  [ io_lib:format( "~B minutes", [ Minutes ] ) | ListWithHours ]

	end,

	ListWithSeconds = case Seconds of

		0 ->
			ListWithMinutes;

		1 ->
			[ "1 seconde" | ListWithMinutes ];

		_ ->
			[ io_lib:format( "~B secondes", [ Seconds ] ) | ListWithMinutes ]

	end,

	ActualMilliseconds = Milliseconds rem 1000,

	ListWithMilliseconds = case ActualMilliseconds of

		0 ->
			ListWithSeconds;

		1 ->
			[ "1 milliseconde" | ListWithSeconds ];

		_ ->
			[ io_lib:format( "~B millisecondes", [ ActualMilliseconds ] )
			  | ListWithSeconds ]

	end,

	% Preparing for final display:
	case ListWithMilliseconds of

		[] ->
			"0 milliseconde";

		[ OneElement ] ->
			OneElement;

		[ Smaller | Bigger ] ->
			text_utils:join( ", ",
							 lists:reverse( Bigger ) ) ++ " et " ++ Smaller

	end;

duration_to_french_string( infinity ) ->
	"infini".



% Returns an approximate textual description of the specified time-out, in the
% specified language, expected to be expressed as a number of milliseconds
% (integer; otherwise, if being floating-point, it will be rounded), or as the
% 'infinity' atom.
%
% Ex: for a time-out of 150 012 ms, returns for English:
% "time-out of 2 minutes, 30 seconds and 12 milliseconds".
%
-spec time_out_to_string( time_out(),
						  language_utils:human_language() ) -> ustring().
time_out_to_string( _Duration=infinity, _Lang=french ) ->
	"délai d'attente maximal illimité";

time_out_to_string( Duration, Lang=french ) ->
	"délai d'attente maximal de " ++ duration_to_string( Duration, Lang );

% Default language is English:
time_out_to_string( Duration, _Lang ) ->
	time_out_to_string( Duration ).



% Returns an approximate textual (English) description of the specified
% time-out, expected to be expressed as a signed number of milliseconds
% (integer; otherwise, if being floating-point, it will be rounded), or as the
% 'infinity' atom.
%
% Ex: for a time-out of 150 012 ms, returns:
% "time-out of 2 minutes, 30 seconds and 12 milliseconds".
%
% See also: basic_utils:get_textual_duration/2.
%
-spec time_out_to_string( time_out() ) -> ustring().
time_out_to_string( _Timeout=infinity ) ->
	"time-out that is unlimited";

time_out_to_string( Duration ) ->
	"time-out of " ++ duration_to_string( Duration ).


% Returns a timestamp that is as precise as possible:
% {MegaSecs, Secs, MicroSecs}, where:
%
% - MegaSecs is an integer number of millions of seconds
% - Secs is an integer number of seconds that is less than one million
% - MicroSecs is an integer number of microseconds
%
-spec get_precise_timestamp() -> precise_timestamp().
get_precise_timestamp() ->
	% Was initially: erlang:now().
	% os:timestamp() was then a bit lighter (not monotonic)
	%
	% Finally preferred (still not monotonic), since release 18.0:
	%
	erlang:timestamp().



% Returns the (signed) duration in milliseconds between the two specified
% precise timestamps (as obtained thanks to get_precise_duration/0), using the
% first one as starting time and the second one as stopping time.
%
-spec get_precise_duration( precise_timestamp(), precise_timestamp() ) ->
								milliseconds().
get_precise_duration( _FirstTimestamp={ A1, A2, A3 },
					  _SecondTimestamp={ B1, B2, B3 } ) ->

	% Seconds to be converted in milliseconds:
	1000 * ( ( B1 - A1 ) * 1000000 + B2 - A2 ) + round( ( B3 - A3 ) / 1000 ).



% Returns the (signed) duration in milliseconds between the specified precise
% timestamp (as obtained thanks to get_precise_duration/0) and the current time.
%
-spec get_precise_duration_since( precise_timestamp() ) -> milliseconds().
get_precise_duration_since( StartTimestamp ) ->
	get_precise_duration( StartTimestamp, get_precise_timestamp() ).



% Returns the date corresponding to the specified one augmented of the specified
% number of days (possibly a negative number).
%
-spec get_date_after( date(), days() ) -> date().
get_date_after( BaseDate, Days ) ->

	DayCount = calendar:date_to_gregorian_days( BaseDate ) + Days,

	calendar:gregorian_days_to_date( DayCount ).
