% Copyright (C) 2016-2021 Olivier Boudeville
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


% Unit tests for the management of units.
%
% See the unit_utils.erl tested module.
%
-module(unit_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



test_parse( InputString, ExpectedString ) ->

	%test_facilities:display( "### Parsing '~ts'...", [ InputString ] ),

	{ Value, Unit } = unit_utils:parse_value_with_unit( InputString ),

	ResultString = unit_utils:value_with_unit_to_string( Value, Unit ),

	test_facilities:display( " + '~ts' translated as '~ts'.",
							 [ InputString, ResultString ] ),


	ExpectedString = ResultString.



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),


	test_facilities:display( "Testing prefix management." ),

	kilo = unit_utils:get_prefix_for_order( 3 ),

	% 'milli' prefix:
	-3 = unit_utils:get_order_for_prefix( "m" ),

	test_parse( "1 m  ",  "1.0 m^1" ),
	test_parse( "1 km  ", "1.0e3 m^1" ),
	test_parse( "1 Tm  ", "1.0e12 m^1" ),



	test_facilities:display( "~nTesting exponent management.",  [] ),

	test_parse( "1 s^2",                   "1.0 s^2" ),
	test_parse( "1 A.A^-1",                "1.0" ),
	test_parse( "7 K.K^2.mol.K^-3.mol^-1", "7.0" ),
	test_parse( "1.1 Gcd^0",               "1.1" ),
	test_parse( "-7.0011 A^1.s^2/km^2",   "-7.0011e-6 s^2.A^1.m^-2" ),


	test_facilities:display( "~nTesting the remaining of the 7 SI "
							 "base units." ),

	% Meter already done.
	test_parse( "1038  kg^2.kg^-3.kg.kg.g", "1.038e6 g^2" ),
	% Second, Ampere, Kelvin, mole, already done.
	test_parse( "1.01 kcd^-1   ",           "0.00101 cd^-1" ),


	test_facilities:display( "~nTesting the supported derived units." ),

	test_parse( " 20  kHz",        "2.0e4 s^-1" ),
	test_parse( "  9.81 N ",       "9810.0 g^1.m^1.s^-2" ),
	test_parse( "  15 hPa",        "1.5e6 g^1.m^-1.s^-2" ),
	test_parse( "  2011.6 kJ    ", "2.0116e9 m^2.g^1.s^-2" ),

	test_parse( "  1 W  ", "1.0e3 m^2.g^1.s^-3" ),
	test_parse( "2.5 W^4", "2.5e12 m^8.g^4.s^-12" ),

	test_parse( "1 C",        "1.0 A^1.s^1" ),
	test_parse( "2.2 C.m^-1", "2.2 A^1.s^1.m^-1" ),

	test_parse( "230 V  ",  "2.3e5 m^2.g^1.A^-1.s^-3" ),
	test_parse( "440 kV  ", "4.4e8 m^2.g^1.A^-1.s^-3" ),


	test_parse( "0.15 µF",    "1.5e-10 s^4.A^2.g^-1.m^-2" ),
	test_parse( "78 kV/nF^2", "7.8e31 m^6.g^3.A^-5.s^-11" ),

	test_parse( "1 mOhm",   "1.0 m^2.g^1.A^-2.s^-3" ),
	test_parse( "5.0 MOhm", "5.0e9 m^2.g^1.A^-2.s^-3" ),

	test_parse( "10 S",        "0.01 s^3.A^2.g^-1.m^-2" ),
	test_parse( "-24 mS.m^-1", "-2.4e-5 s^3.A^2.g^-1.m^-3" ),

	test_parse( "4 Wb",        "4.0e3 m^2.g^1.A^-1.s^-2" ),
	test_parse( "50 Wb.Hz^-1", "5.0e4 m^2.g^1.A^-1.s^-1" ),

	test_parse( "1.00 T", "1.0e3 s^2.g^1.A^-1" ),
	test_parse( "0.3 mT", "0.3 s^2.g^1.A^-1" ),

	test_parse( "15 H",    "1.5e4 m^2.g^1.A^-2.s^-2" ),
	test_parse( "15 µH^1", "0.015 m^2.g^1.A^-2.s^-2" ),

	test_parse( "700.4 mlm/m", "0.7004 cd^1.m^-1" ),

	test_parse( "22 klx", "2.2e4 cd^1.m^-2" ),

	test_parse( "15 kBq", "1.5e4 s^-1" ),

	test_parse( "77.09 mGy", "0.07709 m^2.s^-2" ),

	test_parse( "-30 kSv", "-3.0e4 m^2.s^-2" ),

	test_parse( "  +1.0005e-2 kkat  ", "10.005 mol^1.s^-1" ),


	test_facilities:display( "~nTesting the widely used units." ),

	test_parse( "1 min",   "60.0 s^1" ),
	test_parse( " 2 h  ",  "7.2e3 s^1" ),
	test_parse( "5000 L",  "5.0 m^3" ),
	test_parse( "1.5 Mt",  "1.5e12 g^1" ),

	test_parse( "1 eV",   "1.602176620898e-16 m^2.g^1.s^-2" ),
	test_parse( "10 keV", "1.602176620898e-12 m^2.g^1.s^-2" ),


	test_facilities:display( "~nTesting the special units." ),

	test_parse( "0", "0.0" ),
	test_parse( " -114.9e-3  ", "-0.1149" ),


	test_facilities:display( "~nTesting the non-standard units." ),

	% 'teqCO2' is not a known unit:
	test_parse( "0.4 teqCO2/year", "0.4 teqCO2^1.year^-1" ),

	test_facilities:stop().
