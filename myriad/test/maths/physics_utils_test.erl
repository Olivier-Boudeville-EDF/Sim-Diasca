% Copyright (C) 2023-2024 Olivier Boudeville
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
% Creation date: Saturday, October 7, 2023.


% @doc Unit tests for the <b>physics-related basic toolbox</b> facilities.
%
% See the math_utils tested module.
%
-module(physics_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Silencing:
-export([ test_basics/0 ]).



test_basics() ->

	GEarth = physics_utils:get_gravitational_acceleration(
		_Distance=physics_utils:r_earth(), _Mass=physics_utils:m_earth() ),

	GSun = physics_utils:get_gravitational_acceleration(
		physics_utils:r_sun(), physics_utils:m_sun() ),

	test_facilities:display( "The gravitational acceleration on the surface "
		"of Earth is about ~.2f m/s², while on the Sun it is ~.2f m/s².",
		[ GEarth, GSun ] ),


	EVSun = physics_utils:get_escape_velocity(
		physics_utils:r_sun(), physics_utils:m_sun() ),

	EVEarth = physics_utils:get_escape_velocity(
		physics_utils:r_earth(), physics_utils:m_earth() ),


	% Thus from their surface:
	test_facilities:display( "The escape velocity from Earth is about ~ts,"
		" i.e. ~.2f km/h, while from Sun it is ~ts, i.e. ~.2f km/h.",
		[ unit_utils:speed_to_string( EVEarth ),
		  unit_utils:meters_per_second_to_km_per_hour( EVEarth ),
		  unit_utils:speed_to_string( EVSun ),
		  unit_utils:meters_per_second_to_km_per_hour( EVSun ) ] ),


	SRs = [ physics_utils:get_schwarzschild_radius( R )
		|| R <- [ physics_utils:m_earth(), physics_utils:m_sun(),
				  10 * physics_utils:m_sun(),
				  physics_utils:m_sagittarius_a_star() ] ],

	test_facilities:display( "The Schwarzschild radius of Earth is about ~ts, "
		"the one of Sun about ~ts, the one of a 10 solar-mass black hole ~ts, "
		"and the one of Sagittarius A* about ~ts.",
		[ unit_utils:meters_to_string( SR ) || SR <- SRs ] ),


	M = 10 * physics_utils:m_sun(),

	MinDistance = physics_utils:get_schwarzschild_radius( M ),

	Distances = [ C*MinDistance || C <- lists:seq( 1, 20 ) ],

	test_facilities:display( "Evaluating the time factor experienced by "
		"an observer due to a primary body of 10 solar masses, "
		"starting from the corresponding Schwarzschild radius: ~ts",
		[ text_utils:strings_to_string( [ text_utils:format( "at ~ts: ~.2f%",
			[ unit_utils:meters_to_string( D ),
			  100*physics_utils:get_time_factor( D , M ) ] )
				  || D <- Distances ] ) ] ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_basics(),

	test_facilities:stop().
