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
% Creation date: Wednesday, October 24, 2012.


% Gathering of unit management facilities, first in a simple, ad hoc, limited
% form, then on a more formal, heavyweight one.
%
% All kinds of units are listed here, alongside the reference ones (ex: the
% meter is the unit of length in the International System of Units).
%
% One objective is to be able to specify, instead of mere values (ex: "1.14"),
% values with units (ex: "1.14 km/h"), and possibly to convert them into a
% canonical form transparently, for automated checking and exact conversion.
%
% See unit_utils_test.erl for the corresponding test.
%
% Read the 'Management of Units' section of the technical manual of the Myriad
% Layer for more information.
%
-module(unit_utils).



% This first section deals with loose definitions of units (declarations for the
% simpler unit forms).

% Units as such (ex: months(), applicable to durations) are here in plural form,
% while specific quantities are in singular form (ex: canonical_month(), to
% designate a specific month).
%
% As a result we may define for a given unit both forms, singular and plural
% (ex: year() for a specific year, and years() for a duration in years).
%
% We tend to use integer/0 values, not positive_integer/0 ones, to better
% account for differences.



% Time-related section.


% 10^6 seconds:
-type megasecond()  :: integer().
-type megaseconds() :: integer().


-type year()         :: integer().
-type years()        :: integer().

-type month()        :: integer().
-type months()       :: integer().

% Month in the year; could be calendar:month():
-type canonical_month() :: 1..12.

% Absolute months:
-type absolute_month() :: { year(), canonical_month() }.

-type weeks()        :: integer().
-type week()         :: integer().

-type day()          :: integer().
-type days()         :: integer().


% Day in the month:
-type canonical_day() :: 1..31.


-type hour()         :: integer().
-type hours()        :: integer().

% Hour in the day:
-type canonical_hour() :: 0..23.


-type minute()       :: integer().
-type minutes()      :: integer().

% Minute in the hour:
-type canonical_minute() :: 0..59.

-type second()       :: integer().
-type seconds()      :: integer().


% Second in the minute:
-type canonical_second() :: 0..59.

-type float_second()  :: float().
-type float_seconds() :: float().


% Any type of second (integer or float):
-type any_second()  :: second()  | float_second().
-type any_seconds() :: seconds() | float_seconds().


% Square seconds (s^2):
-type square_seconds() :: float().


-type millisecond()  :: integer().
-type milliseconds() :: integer().

% Millisecond in the second:
-type canonical_millisecond()  :: 0..999.


-type microsecond()  :: integer().
-type microseconds() :: integer().

% Microsecond in the second:
-type canonical_microsecond() :: 0..999999.


% Mean Time To Failure:
-type mttf() :: time_utils:dhms_duration().


% Frequency:
-type hertz() :: float().

-type time_reference_unit() :: 'seconds'.

% Months and weeks not specifically useful here:
-type time_units() :: time_reference_unit() | 'years' | 'days' | 'hours'
					| 'minutes' | 'milliseconds' | 'microseconds'.



-export_type([ megasecond/0, megaseconds/0,
			   year/0, years/0,
			   month/0, months/0, canonical_month/0, absolute_month/0,
			   week/0, weeks/0,
			   day/0, days/0, canonical_day/0,
			   hour/0, hours/0, canonical_hour/0,
			   minute/0, minutes/0, canonical_minute/0,
			   second/0, seconds/0, canonical_second/0,
			   float_second/0, float_seconds/0,
			   any_second/0, any_seconds/0, square_seconds/0,
			   millisecond/0, milliseconds/0, canonical_millisecond/0,
			   microsecond/0,microseconds/0, canonical_microsecond/0,
			   mttf/0, hertz/0, time_reference_unit/0, time_units/0 ]).



% Length-related section.

-type meters() :: float().

-type kilometers() :: float().

-type millimeters() :: float().
-type int_millimeters() :: integer().

-type length_reference_unit() :: 'meters'.

-type length_units() :: length_reference_unit() | 'millimeters'
					  | 'int_millimeters'.


-export_type([ meters/0, kilometers/0, millimeters/0, int_millimeters/0,
			   length_reference_unit/0, length_units/0 ]).



% Speed related section.

-type km_per_hour() :: float().
-type meters_per_second() :: float().

-type meters_per_tick() :: float().

-export_type([ km_per_hour/0, meters_per_second/0, meters_per_tick/0 ]).



% Surface-related section.

-type square_meters() :: float().

-type surface_reference_unit() :: square_meters().

-type surface_units() :: square_meters().

-export_type([ square_meters/0, surface_reference_unit/0, surface_units/0 ]).



% Volume-related section.

-type cubic_meters() :: float().
-type litre() :: float().

-type volume_reference_unit() :: cubic_meters().

-type volume_units() :: volume_reference_unit() | 'litre'.


-export_type([ cubic_meters/0, litre/0, volume_reference_unit/0,
			   volume_units/0 ]).



% Mass-related section.

-type tons() :: float().
-type kilograms() :: float().
-type grams() :: float().

-type mass_reference_unit() :: 'kilograms'.

-type mass_units() :: mass_reference_unit() | 'tons' | 'grams'.


-export_type([ tons/0, kilograms/0, grams/0, mass_reference_unit/0,
			   mass_units/0 ]).



% Energy-related section (energy, work, heat).

-type joules() :: float().

-type energy_reference_unit() :: 'joules'.

-type energy_units() :: energy_reference_unit().


-export_type([ joules/0, energy_reference_unit/0, energy_units/0 ]).



% Temperature units.

% In degree Celsius (°C):
-type celsius() :: float().


-export_type([ celsius/0 ]).



% Angle section.

-type radians() :: float().


% Angle in degrees.
%
% Preferably to be kept in [0.0,360.0[.
%
-type degrees() :: float().


% Angle in degrees.
%
% Strictly expected to be in [0,360[.
%
-type int_degrees() :: integer().


-type angle_reference_unit() :: 'radians'.

-type angle_units() :: angle_reference_unit() | 'degrees' | 'int_degrees'.


-export_type([ radians/0, degrees/0, int_degrees/0, angle_reference_unit/0,
			   angle_units/0 ]).



% All kinds of units:
-type units() :: time_units() | length_units() | volume_units() | mass_units()
			   | energy_units() | angle_units().

-export_type([ units/0 ]).





% Second, declarations for the more formal unit forms.


% This second section deals with a more formal representation of values with
% units, to be able to perform checking and conversions.


% We distinguish the unit (ex: base, derived, widely-used or special) from its
% possible prefix (ex: kilo, mega, etc.).


% The seven SI base units are:
% - meter, for length [m]
% - kilogram, for mass [kg]; we use gram ([g]) instead, as no prefix is wanted
% here
% - second, for time [s]
% - ampere, for electric current [A]
% - kelvin, for thermodynamic temperature [K]
% - mole, for the amount of substance [mol]
% - candela, for luminous intensity [cd]
%
-type base_unit_symbol() :: 'm'
						  | 'g'
						  | 's'
						  | 'A'
						  | 'K'
						  | 'mol'
						  | 'cd'.



% The derived base units currently supported:
% - hertz, for frequency [Hz]
% - radian, for angle [rad]
% - steradian, for solid angle [sr]
% - newton, for force, weight [N]
% - pascal, forpressure, stress [Pa]
% - joule, for energy, work, heat [J]
% - watt, for power, radiant flux [W]
% - coulomb, for electric charge or quantity of electricity [C]
% - volt, for voltage, electrical potential difference, electromotive force [V]
% - farad, for electrical capacitance [F]
% - ohm, for electrical resistance, impedance, reactance [ohm]
% - siemens, for electrical conductance [S]
% - tesla, for magnetic field strength, magnetic flux density [T]
% - henry, for inductance [H]
% - degree Celsius, for temperature relative to 273.15 K [°C]
% - lumen, for luminous flux [lm]
% - lux, for illuminance [lx]
% - becquerel, for radioactive decays per unit time [Bq]
% - gray, for absorbed dose of ionizing radiation [Gy]
% - sievert, for equivalent dose of ionizing radiation [Sv]
% - katal, for catalytic activity [kat]
%
-type derived_unit_symbol() :: 'Hz'
							 | 'rad'
							 | 'sr'
							 | 'N'
							 | 'Pa'
							 | 'J'
							 | 'W'
							 | 'C'
							 | 'V'
							 | 'F'
							 | 'Ohm'
							 | 'S'
							 | 'T'
							 | 'H'
							 | '°C'
							 | 'lm'
							 | 'lx'
							 | 'Bq'
							 | 'Gy'
							 | 'Sv'
							 | 'kat'.


% The units widely used in conjunction with SI units:
% - minute, for 60-second durations [min]
% - hour, for 60-minute durations [h]
% - litre, for 10^-3 m^3 volumes [L]
% - tonne, for 1,000 kilogram masses [t]
% - electronvolt, for 1.602176565(35).10-19 joule energies  [eV]
%
-type widely_used_unit_symbol() :: 'min'
								 | 'h'
								 | 'L'
								 | 't'
								 | 'eV'.



% The special units, designating:
% - dimension-less quantities (ex: a count), [dimensionless] (most probably
% clearer than m/m)
% - currencies, either [$] (US Dollar) or [euros] (Euro)
% - values whose unit has not been specified [unspecified_unit]
%
-type special_unit_symbol() :: 'dimensionless'
							 | '$'
							 | 'euros'
							 | 'unspecified_unit'.


% For non-SI units that cannot be anticipated (ex: teqCO2, singaporean dollar of
% 2012, number of people, etc.)
%
-type non_standard_unit_symbol() :: atom().


% All unit symbols (actually not used as such):
-type unit_symbol() :: base_unit_symbol()
					 | derived_unit_symbol()
					 | widely_used_unit_symbol()
					 | special_unit_symbol()
					 | non_standard_unit_symbol().


% The string counterparts of unit symbols (ex: "eV" instead of 'eV'), used for
% parsing:
%
-type unit_string_symbol() :: ustring().


-export_type([ base_unit_symbol/0, derived_unit_symbol/0,
			   widely_used_unit_symbol/0, special_unit_symbol/0,
			   non_standard_unit_symbol/0, unit_symbol/0 ]).


% Metric prefix (like 'kilo', to specify kilograms from grams):
-type metric_prefix() :: 'yotta'
					   | 'zetta'
					   | 'exa'
					   | 'peta'
					   | 'tera'
					   | 'giga'
					   | 'mega'
					   | 'kilo'
					   | 'hecto'
					   | 'deca'
					   % (one)
					   | 'deci'
					   | 'centi'
					   | 'milli'
					   | 'micro'
					   | 'nano'
					   | 'pico'
					   | 'femto'
					   | 'atto'
					   | 'zepto'
					   | 'yocto'.


% Symbol of metric prefix (ex: "da" for 'deca'):
-type prefix_symbol() :: ustring().


% Order of magnitude (exponent of 10):
-type magnitude_order() :: integer().


% Multiplying factor:
-type multiplying_factor() :: float().


% Exponentiation of a unit (ex: 2 for square meters, m^2).
-type exponent() :: integer().


% String containing a unit, in standard form (ex: "km/h", "mW.m^-3").
%
% Read the 'Management of Units' section of the technical manual of the Myriad
% Layer for more information.
%
-type unit_string() :: ustring().


% Binary counterpart of a unit string:
-type unit_bin_string() :: text_utils:bin_string().



% Actual internal, canonical form for any unit (relying on the 7 SI base units,
% an order of magnitude and a multiplying factor):
%
-record( canonical_unit, {

	meter   = 0 :: exponent(),
	gram    = 0 :: exponent(),
	second  = 0 :: exponent(),
	ampere  = 0 :: exponent(),
	kelvin  = 0 :: exponent(),
	mole    = 0 :: exponent(),
	candela = 0 :: exponent(),

	other_units = [] :: [ { non_standard_unit_symbol(), exponent() } ],

	% Exponent of 10:
	%
	% Default is 0, for 10^0=1:
	order = 0 :: magnitude_order(),

	% Multiplying factor, introduced so that special units like hours can
	% nevertheless be managed:
	%
	factor = 1.0 :: multiplying_factor() } ).


-type canonical_unit() :: #canonical_unit{}.


-type base_unit_name() :: 'meter' | 'gram' | 'second' | 'ampere' | 'kelvin'
						| 'mole' | 'candela'.


-type derived_unit_name() :: 'hertz' | 'radian' | 'steradian' | 'newton'
						   | 'pascal' | 'joule' | 'watt' | 'coulomb' | 'volt'
						   | 'farad' | 'ohm' | 'siemens' | 'weber' | 'tesla'
						   | 'henry' | 'degree Celsius' | 'lumen' | 'lux'
						   | 'becquerel' | 'gray' | 'sievert' | 'katal'.


-type widely_used_unit_name() :: 'minute' | 'hour' | 'litre' | 'tonne'
							   | 'electronvolt'.


-type special_unit_name() :: 'dimensionless' | 'dollar' | 'euro'
						   | 'unspecified_unit'.


-type unit_name() :: base_unit_name() | derived_unit_name()
				   | widely_used_unit_name() | special_unit_name().



% The actual value whose unit may be associated to.
-type numerical_value() :: float().


-export_type([ unit_string/0, unit_bin_string/0, canonical_unit/0,
			   numerical_value/0 ]).



% Unit management section.

-export([ get_prefix_information/0, get_prefix_for_order/1,
		  get_order_for_prefix/1,
		  parse_value_with_unit/1, parse_unit/1, is_canonical_unit/1,
		  get_order/1, get_factor/1, are_units_identical/2,
		  unit_to_string/1, pure_unit_to_string/1, value_with_unit_to_string/2
		]).


% After both types of declarations, the implementation section:


% Implementations for the simpler, looser form of units:


% Conversion section.

-export([ km_per_hour_to_meters_per_second/1,
		  meters_per_second_to_km_per_hour/1 ]).


-export([ human_time_to_milliseconds/5 ]).



% Internal types.


% Unit component:
-type unit_component() :: ustring().

% The various supported kinds of component operators:
%
-type operator_kind() :: 'multiply' | 'divide'.


% Shorthands:

-type ustring() :: text_utils:ustring().



% Converting speeds.


-spec km_per_hour_to_meters_per_second( km_per_hour() ) -> meters_per_second().
km_per_hour_to_meters_per_second( K ) ->
	( K * 1000 ) / 3600.


-spec meters_per_second_to_km_per_hour( meters_per_second() ) -> km_per_hour().
meters_per_second_to_km_per_hour( M ) ->
	M * 3600 / 1000.



% Converting durations.


% Converts specified duration, expressed in a user-friendly time (for humans,
% typically obtained from time_utils:duration_to_string/1) into an integer
% number of milliseconds.
%
% Ex: "1 day, 12 hours, 31 minutes, 9 seconds and 235 milliseconds" translates
% to {1, 12, 31, 9, 235} which, applied to this function, returns milliseconds.
%
-spec human_time_to_milliseconds( days(), hours(), minutes(), seconds(),
								  milliseconds() ) -> milliseconds().
human_time_to_milliseconds( Day, Hour, Minute, Second, Millisecond ) ->
	( ( ( Day * 24 + Hour ) * 60 + Minute ) * 60 + Second ) * 1000
		+ Millisecond.





% Implementations for the more elaborate form of units:


% Returns a list of all metric prefixes, together with their symbol and order of
% magnitude.
%
% For example: {'kilo', "k", 3} means that there are 10^3 grams in a kilogram,
% and that this prefix is represented as "k".
%
% We can see that a symbol may span over multiple characters (ex : "da") and
% even use non-ASCII characters (ex: "µ").
%
-spec get_prefix_information() ->
			[ { metric_prefix(), prefix_symbol(), magnitude_order() } ].
get_prefix_information() ->
	[ { 'yotta', "Y",  24  },
	  { 'zetta', "Z",  21  },
	  { 'exa',   "E",  18  },
	  { 'peta',  "P",  15  },
	  { 'tera',  "T",  12  },
	  { 'giga',  "G",  9   },
	  { 'mega',  "M",  6   },
	  { 'kilo',  "k",  3   },
	  { 'hecto', "h",  2   },
	  { 'deca',  "da", 1   },
	  % None for 0
	  { 'deci',  "d",  -1  },
	  { 'centi', "c",  -2  },
	  { 'milli', "m",  -3  },
	  { 'micro', "µ",  -6  },
	  { 'nano',  "n",  -9  },
	  { 'pico',  "p",  -12 },
	  { 'femto', "f",  -15 },
	  { 'atto',  "a",  -18 },
	  { 'zepto', "z",  -21 },
	  { 'yocto', "y",  -24 } ].



% Type of measure corresponding to a unit (ex: "length").
%
% In some cases, multiple measures can apply (ex: a Coulomb is a measure of
% electric charge or quantity of electricity); we retain here only the most
% usual one.
%
-type unit_measure() :: ustring().


% Information about a unit, i.e. its name, symbol and associated measure.
%
-type unit_information() ::
		{ unit_name(), unit_string_symbol(), unit_measure() }.


% Returns information about all units, i.e. their name, symbol and corresponding
% measure.
%
-spec get_unit_information() -> [ unit_information() ].
get_unit_information() ->
	get_base_unit_information() ++ get_derived_unit_information()
		++ get_widely_used_unit_information() ++ get_special_unit_information().


% Returns information about a base unit.
%
% More info: https://en.wikipedia.org/wiki/SI_base_unit
%
-spec get_base_unit_information() -> [ unit_information() ].
get_base_unit_information() ->
	[ { 'meter',   "m",   "length"                    },
	  { 'gram',    "g",   "mass"                      },
	  { 'second',  "s",   "time"                      },
	  { 'ampere',  "A",   "electric current"          },
	  { 'kelvin',  "K",   "thermodynamic temperature" },
	  { 'mole',    "mol", "amount of substance"       },
	  { 'candela', "cd",  "luminous intensity"        } ].



% Returns information about a derived unit.
%
% More info: https://en.wikipedia.org/wiki/SI_derived_unit
%
-spec get_derived_unit_information() -> [ unit_information() ].
get_derived_unit_information() ->
	[

	 { 'hertz',          "Hz",  "frequency"                             },

	 % Dimensionless:
	 { 'radian',         "rad", "angle"                                 },

	 % Dimensionless:
	 { 'steradian',      "sr",  "solid angle"                           },

	 { 'newton',         "N",   "force"                                 },
	 { 'pascal',         "Pa",  "pressure"                              },
	 { 'joule',          "J",   "energy"                                },
	 { 'watt',           "W",   "power"                                 },
	 { 'coulomb',        "C",   "electric charge"                       },
	 { 'volt',           "V",   "voltage"                               },
	 { 'farad',          "F",   "electrical capacitance"                },
	 { 'ohm',            "Ohm", "electrical resistance"                 },
	 { 'siemens',        "S",   "electrical conductance"                },
	 { 'weber',          "Wb",  "magnetic flux"                         },
	 { 'tesla',          "T",   "magnetic field strength"               },
	 { 'henry',          "H",   "inductance"                            },

	 % Not a product of powers of SI base units; relative to 273.15 K:
	 { 'degree Celsius', "°C",  "temperature"                           },

	 { 'lumen',          "lm",  "luminous flux"                         },
	 { 'lux',            "lx",  "illuminance"                           },
	 { 'becquerel',      "Bq",  "radioactive decays per unit time"      },
	 { 'gray',           "Gy",  "absorbed dose of ionizing radiation"   },
	 { 'sievert',        "Sv",  "equivalent dose of ionizing radiation" },
	 { 'katal',          "kat", "catalytic activity"                    } ].



% Returns information about a widely used unit.
-spec get_widely_used_unit_information() -> [ unit_information() ].
get_widely_used_unit_information() ->
	[ { 'minute',       "min", "time"   },
	  { 'hour',         "h",   "time"   },
	  { 'litre',        "L",   "volume" },
	  { 'tonne',        "t",   "mass"   },
	  { 'electronvolt', "eV",  "energy" } ].



% Returns information about a special unit.
-spec get_special_unit_information() -> [ unit_information() ].
get_special_unit_information() ->
	[ { 'dimensionless',    "dimensionless",      "none"     },
	  { 'dollar',           "$",                  "currency" },
	  { 'euro',             "euros",              "currency" },
	  { 'unspecified_unit', "",                   "unknown"  } ].



% Returns the metric prefix (if any) corresponding to the specified magnitude
% order.
%
% Note: could be predetermined at build time.
%
-spec get_prefix_for_order( magnitude_order() ) -> metric_prefix().
get_prefix_for_order( Order ) ->

	% Tuple example: {'milli', "m",  -3}.
	case lists:keyfind( _K=Order, _Index=3, get_prefix_information() ) of

		{ Prefix, _Symbol, Order } ->
			Prefix;

		% Includes false:
		_ ->
			throw( { no_prefix_for_order, Order } )

	end.



% Returns the magnitude order corresponding to the specified symbol (string) of
% metric prefix.
%
% Note: could be predetermined at build time.
%
-spec get_order_for_prefix( prefix_symbol() ) ->
								magnitude_order() | 'unknown_prefix'.
get_order_for_prefix( _PrefixSymbol="" ) ->
	% No prefix means order 0:
	0;

get_order_for_prefix( PrefixSymbol ) ->

	% Tuple example: {'milli', "m",  -3}.
	case lists:keyfind( _K=PrefixSymbol, _Index=2, get_prefix_information() ) of

		{ _Prefix, _PrefixSymbol, Order } ->
			Order;

		% Includes false:
		_ ->
			unknown_prefix

	end.



% Parses specified string (expected to be a unit_string()) containing a value
% and its unit (ex: "-8.15 kW.m/h^2"), and returns them in a program-tractable
% form, i.e. a pair made of the value (as a float) and the corresponding unit,
% in canonical form.
%
% The input format is the following (in order):
%
% - any leading or trimming whitespace are ignored
%
% - a number, either as an integer (ex: "17") or as a floating point value (ex:
% "17.0" or "2.2017764e+0"), possibly negative (hence starting with an optional
% minus, ex: "-8.15")
%
% - at least one whitespace
%
% - a unit (ex: "kW.m/h^2")
%
% Knowing that:
% - a built-in unit is a base, derived, widely used, or special unit (ex: 'W')
% - a prefixed unit is a built-in unit with a prefix (ex: 'kW')
% - a unit component is a prefixed unit with an exponent (ex: 'km^-2')
% - a unit operator is either '.' (dot, for multiply) or '/' (slash, for divide)
%
% The general format of a unit is then: a series of unit components with one
% unit operator intercalated between two successive components (no whitespace
% allowed).
%
-spec parse_value_with_unit( ustring() ) ->
								{ numerical_value(), canonical_unit() }.
parse_value_with_unit( InputString ) ->

	%trace_utils:debug_fmt( "Parsing value with unit '~ts'.", [ InputString ] ),

	TrimString = text_utils:trim_whitespaces( InputString ),

	InternalDelimiters = text_utils:list_whitespaces(),

	% Two strings:
	{ ValueString, UnitString } = case
			 text_utils:split_per_element( TrimString, InternalDelimiters ) of

		% A value and a unit:
		[ V, U ] ->
			{ V, U };

		% Just a value (no unit):
		[ V ] ->
			{ V, "" };

		[] ->
			% Most probably an empty string:
			throw( { empty_value, InputString } );

		Other ->
			throw( { too_many_value_components, Other } )

	end,

	%trace_utils:debug_fmt( "As strings, value is '~ts', unit is '~ts'.",
	%					   [ ValueString, UnitString ] ),

	Value = parse_as_float( ValueString ),

	Unit = parse_unit( UnitString ),

	{ Value, Unit }.




% Parses specified string, expected to contain a number (either an integer or a
% float), as a float, which is returned.
%
-spec parse_as_float( ustring() ) -> float().
parse_as_float( StringValue ) ->

	try

		erlang:list_to_float( StringValue )

	catch

		error:badarg ->

			try

				float( erlang:list_to_integer( StringValue ) )

			catch

				error:badarg ->
					throw( { value_parsing_failed, StringValue } )

			end

	end.



% Parses specified string, expected to contain a unit (ex: "kW.m/h^2"), and
% returns a canonical unit.
%
-spec parse_unit( ustring() ) -> canonical_unit().
parse_unit( UnitString ) ->

	%trace_utils:debug_fmt( "Parsing unit '~ts'.", [ UnitString ] ),

	% We have two lists of components like "km^3":
	{ MultComponents, DivComponents } = split_unit_components( UnitString ),

	%trace_utils:debug_fmt( "Components: multiply=~p, divide=~p.",
	%					   [ MultComponents, DivComponents ] ),

	BlankUnit = #canonical_unit{},

	% Transforms "km^3" into updated fields of the specified unit record:
	MultUnit = interpret_components( MultComponents, multiply, BlankUnit ),
	%trace_utils:debug_fmt( "MultUnit = ~p", [ MultUnit ] ),

	DivUnit = interpret_components( DivComponents, divide, MultUnit ),
	%trace_utils:debug_fmt( "DivUnit = ~p", [ DivUnit ] ),

	%trace_utils:debug_fmt( "Final unit: '~ts'",
	%                       [ unit_to_string( DivUnit ) ] ),

	DivUnit.



% Splits specified string, expected to contain a unit (ex: "kW.m/h^2"), into a
% list of strings corresponding to multiplying unit components (ex: [ "kw", "m"
% ]) and dividing ones (ex: [ "h^2" ]), and returns both lists.
%
-spec split_unit_components( ustring() ) ->
					{ [ unit_component() ], [ unit_component() ] }.
split_unit_components( UnitString ) ->
	% AccString will accumulate the characters of the current component:
	parse_components( UnitString, _MultList=[], _DivList=[], _AccString=[],
					  _AccKind=multiply ).


% (helper)
-spec parse_components( ustring(), [ unit_component() ], [ unit_component() ],
						ustring(), operator_kind() ) ->
							  { [ unit_component() ], [ unit_component() ] }.
parse_components( _UnitString=[], MultList, DivList, AccString, AccKind ) ->
	% All characters of (last) component parsed:
	store_component_acc( AccString, AccKind, MultList, DivList );

parse_components( _UnitString=[ $. | T ], MultList, DivList, AccString,
				  AccKind ) ->
	% Multiply detected, component fully accumulated:
	{ NewMultList, NewDivList } = store_component_acc( AccString, AccKind,
													   MultList, DivList ),

	parse_components( T, NewMultList, NewDivList, _NewAccString="",
					  _NewAccKind=multiply );

parse_components( _UnitString=[ $/ | T ], MultList, DivList, AccString,
				  AccKind ) ->

	% Divide detected component fully accumulated:
	{ NewMultList, NewDivList } =
		store_component_acc( AccString, AccKind, MultList, DivList ),

	parse_components( T, NewMultList, NewDivList, _NewAccString="",
					  _NewAccKind=divide );

parse_components( _UnitString=[ H | T ], MultList, DivList,
				  AccString, AccKind ) ->
	% We are still accumulating characters of the current component:
	parse_components( T, MultList, DivList, [ H | AccString ], AccKind ).



% Stores the parsed component into the relevant list.
%
% (helper)
%
-spec store_component_acc( unit_component(), operator_kind(),
						   [ unit_component() ], [ unit_component() ] ) ->
								{ [ unit_component() ], [ unit_component() ] }.
store_component_acc( ComponentString, _Kind=multiply, MultList, DivList ) ->
	Component = lists:reverse( ComponentString ),
	{ [ Component | MultList ], DivList };


store_component_acc( ComponentString, _Kind=divide, MultList, DivList ) ->
	Component = lists:reverse( ComponentString ),
	{ MultList, [ Component | DivList ] }.



% Updates the specified canonical unit from the list of multiplying components.
%
% (fold)
%
interpret_components( _Components=[], _Kind, CanonicalUnit ) ->
	CanonicalUnit;

interpret_components( _Components=[ C | T ], Kind, CanonicalUnit ) ->
	NewCanonicalUnit = integrate_component( C, Kind, CanonicalUnit ),
	interpret_components( T, Kind, NewCanonicalUnit ).



% Integrates specified string component into specified canonical unit, and
% returns an updated one.
%
integrate_component( ComponentString, Kind, CanonicalUnit ) ->

	%trace_utils:debug_fmt( "Integrating ~ts component '~ts' in unit '~ts'.",
	%			   [ Kind, ComponentString, unit_to_string( CanonicalUnit ) ] ),

	% Respectively, for 'km^2': 3, 'meter', 2:
	{ BaseOrder, UnitAtomName, UnitExponent } =
		parse_component( ComponentString ),

	% Ex: for "kW^-2", the actual order is 3*(-2):
	NormalisedUnitExponent = case Kind of

		multiply ->
			UnitExponent;

		divide ->
			-UnitExponent

	end,

	ActualOrder = BaseOrder * NormalisedUnitExponent,

	%trace_utils:debug_fmt( "~n- for component '~ts': unit_symbol '~ts', "
	%    "actual_order=~B, normalised_unit_exponent=~B.",
	%    [ ComponentString, UnitAtomName, ActualOrder,
	%      NormalisedUnitExponent ] ),

	integrate_to_canonical_unit( UnitAtomName, ActualOrder,
								 NormalisedUnitExponent, CanonicalUnit ).



% Returns {ActualOrder, UnitName, Exponent}:
-spec parse_component( ustring() ) ->
							{ magnitude_order(), unit_name(), exponent() }.
parse_component( ComponentString ) ->

	% Ex: ComponentString="km^-3"; let's see whether we have an exponent:

	{ PrefixedUnitString, UnitExponent } = case text_utils:split(
								ComponentString, _Delimiters=[ $^ ] ) of

		% Returns for example { "km", -3 }:
		[ PfxUnit, ExponentString ] ->
			Exp = text_utils:string_to_integer( ExponentString ),
			{ PfxUnit, Exp };


		% Having no unit exponent set means 1:
		[ PfxUnit ] ->
			{ PfxUnit, _Exp=1 };

		% No unit set:
		[] ->
			{ "", _Exp=1 };

		_Other ->
			throw( { multiple_exponents, ComponentString } )

	end,

	%trace_utils:debug_fmt( "PrefixedUnit='~ts', unit exponent=~B.",
	%					   [ PrefixedUnitString, UnitExponent ] ),

	% The "k" of "km" to be transformed into 'kilo' then into 3:
	{ BaseOrder, UnitName } = extract_prefix_and_unit( PrefixedUnitString ),

	{ BaseOrder, UnitName, UnitExponent }.




% Extracts the prefix and unit from specified exponent-less string (ex:
% "decaA").
%
% We have to scan backward, starting from the unit then only its prefix, as some
% prefix symbols (ex: "m", for 'milli') are actually prefixes of unit symbols
% (ex: "mol"): a forward scan may interpret "m" for 'milli', whereas it was just
% the beginning of "mol".
%
-spec extract_prefix_and_unit( ustring() ) ->
									{ magnitude_order(), unit_name() }.
extract_prefix_and_unit( _PrefixedUnitString="" ) ->
	{ _Order=0, _UnitName=dimensionless };

extract_prefix_and_unit( PrefixedUnitString ) ->

	% So we have to go backward:
	RevPrefixedUnitString = lists:reverse( PrefixedUnitString ),

	% A reversed unit symbol may be a prefix of another one (ex: 't', for tonne,
	% if a prefix of 'tak', for the katal unit 'kat' once reversed); so we need
	% to check for the longer reversed unit symbols first; otherwise we would
	% select 't' instead of 'tak'.
	%
	RevUnitSymbols = get_reversed_ordered_symbols_of_units(),

	{ RevPrefixString, UnitName } =
		scan_for_unit_symbol( RevPrefixedUnitString, RevUnitSymbols ),

	PrefixString = lists:reverse( RevPrefixString ),

	%trace_utils:debug_fmt( "Unit name: '~p', prefix: '~p'.",
	%					   [ UnitName, PrefixString ] ),

	case get_order_for_prefix( PrefixString ) of

		unknown_prefix ->
			% Here, what we thought to be a prefix shall actually be an unknown
			% unit (ex: "teqCO2"), so we accept it as it is:
			%
			{ _Order=0, _UnitName=PrefixString };

		Order ->
			{ Order, UnitName }

	end.



-spec scan_for_unit_symbol( ustring(), [ ustring() ] ) ->
								{ ustring(), unit_name() }.
scan_for_unit_symbol( RevPrefixedUnitString, _RevUnitSymbols=[] ) ->
	% No unit symbol found, so dimension-less, hence the whole is a prefix:
	{ RevPrefixedUnitString, dimensionless };

scan_for_unit_symbol( RevPrefixedUnitString, [ RevUnitSymbol | T ] ) ->

	% Does the RevPrefixedUnitString string starts by RevUnitSymbol?
	case text_utils:split_after_prefix( RevUnitSymbol,
										RevPrefixedUnitString ) of

		no_prefix ->
			% Nope, next unit then:
			scan_for_unit_symbol( RevPrefixedUnitString, T );

		% A (reverse) unit symbol matches; by design it is the longer one,
		% hence the unit is formally identified.
		%
		RevPrefixString ->
			UnitSymbol = lists:reverse( RevUnitSymbol ),
			UnitName = unit_symbol_to_name( UnitSymbol ),
			{ RevPrefixString, UnitName }

	end.



% Returns a list of all the known units, as reversed strings, from the longest
% to the shortest.
%
get_reversed_ordered_symbols_of_units() ->

	UnsortedList = [ lists:reverse( UnitSymbolString )
	  || { _UnitAtom, UnitSymbolString, _Measure } <- get_unit_information() ],

	LongerFun = fun( AString, BString ) ->
					length( AString ) > length( BString )
				end,

	lists:sort( LongerFun, UnsortedList ).



% Updates specified canonical unit with specified information.
%
% To support a new unit, simply add its dedicated clause.
%
% First, the 7 base SI units:
-spec integrate_to_canonical_unit( unit_name(), magnitude_order(), exponent(),
								   canonical_unit() ) -> canonical_unit().
integrate_to_canonical_unit( _UnitName=meter, ActualOrder, NormalisedExponent,
				 CanonicalUnit=#canonical_unit{ meter=Exp, order=Order } ) ->
	CanonicalUnit#canonical_unit{ meter=Exp+NormalisedExponent,
								  order=Order+ActualOrder };

integrate_to_canonical_unit( _UnitName=gram, ActualOrder, NormalisedExponent,
				 CanonicalUnit=#canonical_unit{ gram=Exp, order=Order } ) ->
	CanonicalUnit#canonical_unit{ gram=Exp+NormalisedExponent,
								  order=Order+ActualOrder };

integrate_to_canonical_unit( _UnitName=second, ActualOrder, NormalisedExponent,
				 CanonicalUnit=#canonical_unit{ second=Exp, order=Order } ) ->
	CanonicalUnit#canonical_unit{ second=Exp+NormalisedExponent,
								  order=Order+ActualOrder };

integrate_to_canonical_unit( _UnitName=ampere, ActualOrder, NormalisedExponent,
				 CanonicalUnit=#canonical_unit{ ampere=Exp, order=Order } ) ->
	CanonicalUnit#canonical_unit{ ampere=Exp+NormalisedExponent,
								  order=Order+ActualOrder };

integrate_to_canonical_unit( _UnitName=kelvin, ActualOrder, NormalisedExponent,
				 CanonicalUnit=#canonical_unit{ kelvin=Exp, order=Order } ) ->
	CanonicalUnit#canonical_unit{ kelvin=Exp+NormalisedExponent,
								  order=Order+ActualOrder };

integrate_to_canonical_unit( _UnitName=mole, ActualOrder, NormalisedExponent,
				 CanonicalUnit=#canonical_unit{ mole=Exp, order=Order } ) ->
	CanonicalUnit#canonical_unit{ mole=Exp+NormalisedExponent,
								  order=Order+ActualOrder };

integrate_to_canonical_unit( _UnitName=candela, ActualOrder, NormalisedExponent,
				 CanonicalUnit=#canonical_unit{ candela=Exp, order=Order } ) ->
	CanonicalUnit#canonical_unit{ candela=Exp+NormalisedExponent,
								  order=Order+ActualOrder };



% Then the derived units:

integrate_to_canonical_unit( _UnitName=hertz, ActualOrder, NormalisedExponent,
			CanonicalUnit=#canonical_unit{ second=SecondExp, order=Order } ) ->

	% A Hertz is s^-1:
	CanonicalUnit#canonical_unit{ second= SecondExp + NormalisedExponent * -1,
								  order= Order + ActualOrder };


% Not supported yet: degree, radian, steradian.

integrate_to_canonical_unit( _UnitName=newton, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ gram=GramExp,
															meter=MeterExp,
															second=SecondExp,
															order=Order } ) ->

	% A Newton is kg.m/s^2:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * 1,
								  meter= MeterExp + NormalisedExponent * 1,
								  second= SecondExp + NormalisedExponent * -2,
								  % 3 is because we manage grams internally:
								  order= Order + ActualOrder
									 + NormalisedExponent * 3 };


integrate_to_canonical_unit( _UnitName=pascal, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ gram=GramExp,
															meter=MeterExp,
															second=SecondExp,
															order=Order } ) ->

	% A Pascal is kg.m^-1.s^-2:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * 1,
								  meter= MeterExp + NormalisedExponent * -1,
								  second= SecondExp + NormalisedExponent * -2,
								  % 3 is because we manage grams internally:
								  order= Order + ActualOrder
										+ NormalisedExponent * 3 };


integrate_to_canonical_unit( _UnitName=joule, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ gram=GramExp,
															meter=MeterExp,
															second=SecondExp,
															order=Order } ) ->

	% A Joule is kg.m^2.s^-2:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * 1,
								  meter= MeterExp + NormalisedExponent * 2,
								  second= SecondExp + NormalisedExponent * -2,
								  % 3 is because we manage grams internally:
								  order= Order + ActualOrder
									 + NormalisedExponent * 3 };


integrate_to_canonical_unit( _UnitName=watt, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ gram=GramExp,
															meter=MeterExp,
															second=SecondExp,
															order=Order } ) ->

	% A Watt is kg.m^2.s^-3:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * 1,
								  meter= MeterExp + NormalisedExponent * 2,
								  second= SecondExp + NormalisedExponent * -3,
								  % 3 is because we manage grams internally:
								  order= Order + ActualOrder
									 + NormalisedExponent * 3 };


integrate_to_canonical_unit( _UnitName=coulomb, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ second=SecondExp,
															ampere=AmpereExp,
															order=Order } ) ->

	% A Coulomb is s.A:
	CanonicalUnit#canonical_unit{ second= SecondExp + NormalisedExponent,
								  ampere = AmpereExp + NormalisedExponent,
								  order= Order + ActualOrder };


integrate_to_canonical_unit( _UnitName=volt, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ gram=GramExp,
															meter=MeterExp,
															second=SecondExp,
															ampere=AmpereExp,
															order=Order } ) ->

	% A Volt is kg.m^2.s^-3.A^-1:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * 1,
								  meter= MeterExp + NormalisedExponent * 2,
								  second= SecondExp + NormalisedExponent * -3,
								  ampere= AmpereExp + NormalisedExponent * -1,
								  % 3 is because we manage grams internally:
								  order= Order + ActualOrder
										+ NormalisedExponent * 3 };


integrate_to_canonical_unit( _UnitName=farad, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ gram=GramExp,
															meter=MeterExp,
															second=SecondExp,
															ampere=AmpereExp,
															order=Order } ) ->

	% A Farad is kg^-1.m^-2.s^4.A^2:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * -1,
								  meter= MeterExp + NormalisedExponent * -2,
								  second= SecondExp + NormalisedExponent * 4,
								  ampere= AmpereExp + NormalisedExponent * 2,
								  % -3 is because we manage grams internally:
								  order= Order + ActualOrder
										+ NormalisedExponent * -3 };


integrate_to_canonical_unit( _UnitName=ohm, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ gram=GramExp,
															meter=MeterExp,
															second=SecondExp,
															ampere=AmpereExp,
															order=Order } ) ->

	% A Ohm is kg.m^2.s^-3.A^-2:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * 1,
								  meter= MeterExp + NormalisedExponent * 2,
								  second= SecondExp + NormalisedExponent * -3,
								  ampere= AmpereExp + NormalisedExponent * -2,
								  % 3 is because we manage grams internally:
								  order= Order + ActualOrder
										+ NormalisedExponent * 3 };


integrate_to_canonical_unit( _UnitName=siemens, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ gram=GramExp,
															meter=MeterExp,
															second=SecondExp,
															ampere=AmpereExp,
															order=Order } ) ->

	% A Siemens is kg^-1.m^-2.s^3.A^2:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * -1,
								  meter= MeterExp + NormalisedExponent * -2,
								  second= SecondExp + NormalisedExponent * 3,
								  ampere= AmpereExp + NormalisedExponent * 2,
								  % -3 is because we manage grams internally:
								  order= Order + ActualOrder
										+ NormalisedExponent * -3 };


integrate_to_canonical_unit( _UnitName=weber, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ gram=GramExp,
															meter=MeterExp,
															second=SecondExp,
															ampere=AmpereExp,
															order=Order } ) ->

	% A Weber is kg.m^2.s^-2.A^-1:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * 1,
								  meter= MeterExp + NormalisedExponent * 2,
								  second= SecondExp + NormalisedExponent * -2,
								  ampere= AmpereExp + NormalisedExponent * -1,
								  % 3 is because we manage grams internally:
								  order= Order + ActualOrder
										+ NormalisedExponent * 3 };


integrate_to_canonical_unit( _UnitName=tesla, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{
											  gram=GramExp,
											  second=SecondExp,
											  ampere=AmpereExp,
											  order=Order } ) ->

	% A Tesla is kg.s^2.A^-1:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * 1,
								  second= SecondExp + NormalisedExponent * 2,
								  ampere= AmpereExp + NormalisedExponent * -1,
								  % 3 is because we manage grams internally:
								  order= Order + ActualOrder
										+ NormalisedExponent * 3 };


integrate_to_canonical_unit( _UnitName=henry, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ gram=GramExp,
															meter=MeterExp,
															second=SecondExp,
															ampere=AmpereExp,
															order=Order } ) ->

	% A Henry is kg.m^2.s^-2.A^-2:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * 1,
								  meter= MeterExp + NormalisedExponent * 2,
								  second= SecondExp + NormalisedExponent * -2,
								  ampere= AmpereExp + NormalisedExponent * -2,
								  % 3 is because we manage grams internally:
								  order= Order + ActualOrder
										+ NormalisedExponent * 3 };


% Not supported yet: degree Celsius; problem is that it is not a multiple of the
% K (Kelvin) unit; so even the 'factor' field would not be sufficient to support
% this affine, very unusual relationship between these units).
%
% Either the value would have to be modified (whereas we only have here the
% unit), or additional fields would be required, such as 'offset': ActualValue =
% Value * Factor * (exponent and all) + Offset.
%
% There could even be pre- and post-offsets (ex: ActualValue = ( Value +
% PreOffset) * Factor * (exponent and all) + PostOffset.


integrate_to_canonical_unit( _UnitName=lumen, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ candela=CandelaExp,
															order=Order } ) ->

	% A lumen is cd (exactly)
	CanonicalUnit#canonical_unit{ candela= CandelaExp + NormalisedExponent * 1,
								  order= Order + ActualOrder };


integrate_to_canonical_unit( _UnitName=lux, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ meter=MeterExp,
															candela=CandelaExp,
															order=Order } ) ->

	% A lux is m^-2.cd:
	CanonicalUnit#canonical_unit{ meter= MeterExp + NormalisedExponent * -2,
								  candela= CandelaExp + NormalisedExponent * 1,
								  order= Order + ActualOrder };


integrate_to_canonical_unit( _UnitName=becquerel, ActualOrder,
							 NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ second=SecondExp,
															order=Order } ) ->

	% A Becquerel is s^-1:
	CanonicalUnit#canonical_unit{ second= SecondExp + NormalisedExponent * -1,
								  order= Order + ActualOrder };


integrate_to_canonical_unit( _UnitName=gray, ActualOrder,
							 NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ meter=MeterExp,
															second=SecondExp,
															order=Order } ) ->

	% A Gray is m^2.s^-2:
	CanonicalUnit#canonical_unit{ meter= MeterExp + NormalisedExponent * 2,
								  second= SecondExp + NormalisedExponent * -2,
								  order= Order + ActualOrder };


integrate_to_canonical_unit( _UnitName=sievert, ActualOrder,
							 NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ meter=MeterExp,
															second=SecondExp,
															order=Order } ) ->

	% A Sievert is m^2.s^−2 (like Gray):
	CanonicalUnit#canonical_unit{ meter= MeterExp + NormalisedExponent * 2,
								  second= SecondExp + NormalisedExponent * -2,
								  order= Order + ActualOrder };


integrate_to_canonical_unit( _UnitName=katal, ActualOrder,
							 NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ mole=MoleExp,
															second=SecondExp,
															order=Order } ) ->

	% A Katal is s^-1.mol:
	CanonicalUnit#canonical_unit{ mole= MoleExp + NormalisedExponent * 1,
								  second= SecondExp + NormalisedExponent * -1,
								  order= Order + ActualOrder };




% Then the widely used units:

integrate_to_canonical_unit( _UnitName=minute, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ second=SecondExp,
															order=Order,
															factor=Factor } ) ->
	% A minute is 60 s:
	CanonicalUnit#canonical_unit{ second= SecondExp + NormalisedExponent,
								  order= Order + ActualOrder,
								  factor= Factor *
										math:pow( 60, NormalisedExponent ) };


integrate_to_canonical_unit( _UnitName=hour, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ second=SecondExp,
															order=Order,
															factor=Factor } ) ->
	% An hour is 3600 s:
	CanonicalUnit#canonical_unit{ second= SecondExp + NormalisedExponent,
								  order= Order + ActualOrder
									  + NormalisedExponent * 3,
								  factor= Factor *
										math:pow( 3.6, NormalisedExponent ) };


integrate_to_canonical_unit( _UnitName=litre, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ meter=MeterExp,
															order=Order } ) ->
	% A litre is 10^-3 m^3:
	CanonicalUnit#canonical_unit{ meter= MeterExp + NormalisedExponent * 3,
								  order= Order + ActualOrder
										+ NormalisedExponent * -3 };


integrate_to_canonical_unit( _UnitName=tonne, ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ gram=GramExp,
															order=Order } ) ->
	% A tonne is 10^6 g:
	CanonicalUnit#canonical_unit{ gram= GramExp + NormalisedExponent * 1,
								  order= Order + ActualOrder
										+ NormalisedExponent * 6 };


integrate_to_canonical_unit( _UnitName=electronvolt, ActualOrder,
							 NormalisedExponent,
							 CanonicalUnit=#canonical_unit{ factor=Factor } ) ->

	% An eV is 1.602176620898e-19 J.

	% One TeV (a tera electron volt) is about the energy of motion of a flying
	% mosquito.

	integrate_to_canonical_unit( joule, ActualOrder + NormalisedExponent * -19,
								 NormalisedExponent,
								 CanonicalUnit#canonical_unit{
								   factor = Factor * 1.602176620898 } );


integrate_to_canonical_unit( _UnitName=dimensionless, _ActualOrder=0,
							 _NormalisedExponent=1,
							 CanonicalUnit=#canonical_unit{} ) ->
	CanonicalUnit;

integrate_to_canonical_unit( _UnitName=dimensionless, ActualOrder,
							 NormalisedExponent, _CanonicalUnit ) ->
	throw( { invalid_dimensionless, ActualOrder, NormalisedExponent } );


% Then the special units:


% To catch units that are not explicitly known (at least yet):

integrate_to_canonical_unit( UnitName, _ActualOrder, NormalisedExponent,
							 CanonicalUnit=#canonical_unit{
												other_units=Others} ) ->

	%trace_utils:warning_fmt( "Integrating unknown unit '~ts' of order ~p, "
	%						 "normalised exponent ~p to ~ts.",
	%						 [  UnitName, ActualOrder, NormalisedExponent,
	%							unit_to_string( CanonicalUnit ) ] ),

	% Not merging (yet) other units, actual order ignored:
	UnitAsAtom = text_utils:string_to_atom( UnitName ),

	NewOthers = [ { UnitAsAtom, NormalisedExponent } | Others ],

	CanonicalUnit#canonical_unit{ other_units=NewOthers }.



% Tells whether specified term is an actual, canonical unit.
-spec is_canonical_unit( canonical_unit() ) -> boolean().
is_canonical_unit( Term ) when is_record( Term, canonical_unit ) ->
	true;

is_canonical_unit( _Term ) ->
	false.



% Converts a unit symbol, as a string (ex: "Cd") into a unit name (ex:
% 'candela'):
%
-spec unit_symbol_to_name( unit_string_symbol() ) -> unit_name().
unit_symbol_to_name( UnitSymbol ) ->

	% Tuple example: { 'meter', "m", "length" }:
	case lists:keyfind( _K=UnitSymbol, _Index=2, get_unit_information() ) of

		{ UnitName, _UnitSymbol, _UnitMeasure } ->
			UnitName;

		% Includes false:
		_ ->
			throw( { unknown_unit_symbol, UnitSymbol } )

	end.



% Returns a textual representation of the raw unit only (factor and order
% ignored) for the specified canonical unit.
%
% Note: unit_to_string/1 shall be the relevant function for most uses.
%
-spec pure_unit_to_string( canonical_unit() ) -> ustring().
pure_unit_to_string( Unit ) ->

	% We filter out units with exponent zero, and add exponents in the pair for
	% upcoming sort:
	%
	MeterInfo = case Unit#canonical_unit.meter of

		0 ->
			undefined;

		MeterExp ->
			{ text_utils:format( "m^~B", [ MeterExp ] ), MeterExp }

	end,

	GramInfo = case Unit#canonical_unit.gram of

		0 ->
			undefined;

		GramExp ->
			{ text_utils:format( "g^~B", [ GramExp ] ), GramExp }

	end,

   SecondInfo = case Unit#canonical_unit.second of

		0 ->
			undefined;

		SecondExp ->
			{ text_utils:format( "s^~B", [ SecondExp ] ), SecondExp }

	end,

   AmpereInfo = case Unit#canonical_unit.ampere of

		0 ->
			undefined;

		AmpereExp ->
			{ text_utils:format( "A^~B", [ AmpereExp ] ), AmpereExp }

	end,

   KelvinInfo = case Unit#canonical_unit.kelvin of

		0 ->
			undefined;

		KelvinExp ->
			{ text_utils:format( "K^~B", [ KelvinExp ] ), KelvinExp }

	end,

   MoleInfo = case Unit#canonical_unit.mole of

		0 ->
			undefined;

		MoleExp ->
			{ text_utils:format( "mol^~B", [ MoleExp ] ), MoleExp }

	end,

   CandelaInfo = case Unit#canonical_unit.candela of

		0 ->
			undefined;

		CandelaExp ->
			{ text_utils:format( "cd^~B", [ CandelaExp ] ), CandelaExp }

	end,


	OtherInfos = case Unit#canonical_unit.other_units of

		[] ->
			[];

		UnitExponentList ->
			[ { text_utils:format( "~ts^~B", [ OtherUnit, Exp ] ), Exp }
			  || { OtherUnit, Exp } <- UnitExponentList ]

	end,

	AllBaseInfos = [ MeterInfo, GramInfo, SecondInfo, AmpereInfo, KelvinInfo,
					 MoleInfo, CandelaInfo ] ++ OtherInfos,

	% Strips unused units:
	Infos = lists:filter( fun( undefined ) ->
							  false;

							 ( _ )  ->
							  true

						  end,
						  AllBaseInfos ),

	% Sort in decreasing exponents;
	SortedInfos = lists:reverse( lists:keysort( _Index=2, Infos ) ),

	% We finally prefer not marking specifically dimension-less units:

	%SortedStrings = case [ S || { S, _Exp } <- SortedInfos ] of

	%					[] ->
	%						[ "dimensionless" ];

	%					L ->
	%						L

	%end,

	SortedStrings = [ S || { S, _Exp } <- SortedInfos ],

	% May be an empty string:
	text_utils:join( ".", SortedStrings ).



% Returns the magnitude order of the specified unit.
-spec get_order( canonical_unit() ) -> magnitude_order().
get_order( _Unit=#canonical_unit{ order=Order } ) ->
	Order.



% Returns the multiplying factor of the specified unit.
-spec get_factor( canonical_unit() ) -> multiplying_factor().
get_factor( _Unit=#canonical_unit{ factor=Factor } ) ->
	Factor.



% Tells whether the two specified units are strictly the same.
-spec are_units_identical( canonical_unit(), canonical_unit() ) -> boolean().
are_units_identical( Unit, Unit ) ->
	% Relying on a canonical form simplifies much the comparisons:
	true;

are_units_identical( _FirstUnit, _SecondUnit ) ->
	false.



% Returns a textual representation of the specified canonical unit.
-spec unit_to_string( canonical_unit() ) -> ustring().
unit_to_string( Unit ) ->

	UnitString = pure_unit_to_string( Unit ),

	FactorString = case Unit#canonical_unit.factor of

		% Equality comparison is always problematic with floating-point values:
		1.0 ->
			"";

		Factor ->
			text_utils:format( " with factor ~f", [ Factor ] )

	end,

	OrderString = case Unit#canonical_unit.order of

		0 ->
			"";

		Order ->
			text_utils:format( ", of order ~B", [ Order ] )

	end,

	% At least to flatten:
	case text_utils:format( "~ts~ts~ts",
							[ UnitString, FactorString, OrderString ] ) of

		"" ->
			"dimensionless";

		R ->
			R

	end.



% Returns a textual description of specified unit with a value.
-spec value_with_unit_to_string( numerical_value(), canonical_unit() ) ->
										ustring().
value_with_unit_to_string( Value, Unit ) ->

	Order = unit_utils:get_order( Unit ),
	Factor = unit_utils:get_factor( Unit ),

	ActualValue = Value * Factor * math:pow( 10, Order ),

	% To avoid an extra trimming space with dimension-less units:
	case pure_unit_to_string( Unit ) of

		[] ->
			text_utils:format( "~p", [ ActualValue ] );

		PString ->
			text_utils:format( "~p ~ts", [ ActualValue, PString ] )

	end.
