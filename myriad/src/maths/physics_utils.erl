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


% @doc Gathering of various <b>general-purpose basic physics-related</b>
% facilities.
%
% See also: for units, the unit_utils module.
%
-module(physics_utils).


% General constants:
-export([ g/0, c/0, h/0, e/0, m_e/0, m_p/0, m_n/0, k_b/0, sigma/0]).

-compile({ inline, [ g/0, c/0, h/0, e/0, m_e/0, m_p/0, m_n/0,
					 k_b/0, sigma/0 ] }).


% Varied functions:
-export([ get_lorentz_factor/1,
		  get_gravitational_acceleration/2, get_escape_velocity/2,
		  get_schwarzschild_radius/1, get_time_factor/2 ]).


% Examples:
-export([ m_earth/0, m_sun/0, m_sagittarius_a_star/0,
		  r_earth/0, r_sun/0 ]).



-type time_factor() :: factor().
% A ratio between the flow of the proper time of an observer and the overall
% time coordinate.
%
% Expected to be in [0,1[; for example ~0 when the observer is close to a black
% hole, and ~1 when infinitely far from it.


-export_type([ time_factor/0 ]).


% Shorthands:

-type kilograms() :: unit_utils:kilograms().
-type meters() :: unit_utils:meters().
-type meters_per_second() :: unit_utils:meters_per_second().

-type factor() :: math_utils:factor().



% Section for general constants.


% @doc G, the Newtonian constant of gravitation, in N.m^2.kg^−2.
-spec g() -> float().
g() ->
	% Best available precision is low, surprisingly:
	6.67430e-11.


% @doc c, the speed of light in vacuum, in m/s.
%
% The upper limit for the speed at which conventional matter or energy (and thus
% any signal carrying information) can travel through space.
%
% This can also be seen as a scale conversion factor for changing distances
% expressed in terms of units of time to ones in terms of units of space: d=c.t.
%
% Approximately 300,000 kilometres per second.
%
-spec c() -> meters_per_second().
c() ->
	% Exact:
	299792458.0.


% @doc Planck's constant, in J.s.
%
% A photon's energy is equal to its frequency multiplied by the Planck constant,
% and the wavelength of a matter wave equals the Planck constant divided by the
% associated particle momentum.
%
% See https://en.wikipedia.org/wiki/Planck_constant
%
-spec h() -> float().
h() ->
	6.62607015e-34.



% @doc Electron charge (in absolute value), in kg^(1/2).m^(3/2)/s.
%
% The electric charge carried by a single proton or, equivalently, the magnitude
% of the negative electric charge carried by a single electron.
%
% See https://en.wikipedia.org/wiki/Elementary_charge.
%
-spec e() -> float().
e() ->
	% In Coulomb: 1.602176634e-19.
	1.5189e-14.


% @doc Electron mass, in kg.
%
% Mass of a stationary electron, also known as the invariant mass of the
% electron.
%
% See https://en.wikipedia.org/wiki/Electron_mass.
%
-spec m_e() -> kilograms().
m_e() ->
	9.1093837015e-31.


% @doc Proton mass, in kg.
%
% See https://en.wikipedia.org/wiki/Proton.
%
-spec m_p() -> kilograms().
m_p() ->
	1.67262192369e-27.


% @doc Neutron mass, in kg.
%
% See https://en.wikipedia.org/wiki/Neutron.
%
-spec m_n() -> kilograms().
m_n() ->
	1.67492749804e-27.


% @doc Boltzmann constant, in J/K.
%
% The proportionality factor that relates the average relative thermal energy
% of particles in a gas with the thermodynamic temperature of the gas.
%
% See https://en.wikipedia.org/wiki/Boltzmann_constant.
%
-spec k_b() -> float().
k_b() ->
	% Exactly:
	1.380649e-23.


% @doc Stefan-Boltzmann constant, in kg.s^-3.K^-4.
%
% The factor of proportionality between the total energy radiated per unit
% surface area per unit time (also known as the radiant exitance) and the fourth
% power of the black body's temperature: M = Sigma.T^4.
%
% See https://en.wikipedia.org/wiki/Stefan%E2%80%93Boltzmann_law.
%
-spec sigma() -> float().
sigma() ->
	5.670374419e-8.





% Astrophysics-related basic functions.


% @doc Returns the Lorentz factor ("gamma"), a quantity that expresses how much
% the measurements of time, length, and other physical properties change for an
% object while that object is moving.
%
% S is the speed, i.e. the magnitude of the relative velocity between the
% inertial reference frame of the object and the one of the observer.
%
% See https://en.wikipedia.org/wiki/Lorentz_factor.
%
-spec get_lorentz_factor( meters_per_second() ) -> factor().
get_lorentz_factor( S ) ->
	math:sqrt( 1 - math_utils:square( S / c() ) ).



% @doc Returns the local gravitational acceleration at the specified distance
% from the center of a spherically symmetric primary body (such as a star or a
% planet) of the specified mass.
%
% See https://en.wikipedia.org/wiki/Escape_velocity.
%
-spec get_gravitational_acceleration( meters(), kilograms() ) ->
		  meters_per_second().
get_gravitational_acceleration( Distance, Mass ) ->
	g() * Mass / math_utils:square( Distance ).


% @doc Returns the escape velocity, that is the minimum speed needed for a free,
% non-propelled object to escape at the specified distance from the
% gravitational influence of a primary body of specified mass, thus reaching an
% infinite distance from it.
%
% See https://en.wikipedia.org/wiki/Escape_velocity.
%
-spec get_escape_velocity( meters(), kilograms() ) -> meters_per_second().
get_escape_velocity( Distance, Mass ) ->
	math:sqrt( 2 * g() * Mass / Distance ).



% @doc Returns the Schwarzschild radius, which corresponds to the radius
% defining the event horizon of a Schwarzschild black hole of the specified
% mass.
%
% See https://en.wikipedia.org/wiki/Schwarzschild_radius.
%
-spec get_schwarzschild_radius( kilograms() ) -> meters().
get_schwarzschild_radius( Mass ) ->
	2 * g() * Mass / math_utils:square( c() ).



% @doc Returns the time factor that shall be applied to the proper time of an
% observer located at the specified distance from a primary body of the
% specified mass, which creates the gravitational field.
%
% The distance is expected to be greater than the Schwarzschild radius of said
% primary body.
%
% See
% https://en.wikipedia.org/wiki/Gravitational_time_dilation#Outside_a_non-rotating_sphere
%
-spec get_time_factor( meters(), kilograms() ) -> time_factor().
get_time_factor( Distance, Mass ) ->
	SR = get_schwarzschild_radius( Mass ) ,
	% Distance expected to be at least SR:
	math:sqrt( 1 - SR / Distance ).



% Section for examples and magnitude order.


% Mass subsection.
%
% Note that the usual unit of mass is kg, not g.


% @doc The mass, in kg, of our planet, the Earth.
%
% See https://en.wikipedia.org/wiki/Earth_mass.
%
-spec m_earth() -> kilograms().
m_earth() ->
	5.9722e24.


% @doc The mass, in kg, of our star, the Sun.
%
% See https://en.wikipedia.org/wiki/Solar_mass.
%
-spec m_sun() -> kilograms().
m_sun() ->
	1.98847e30.


% @doc The mass, in kg, of Sagittarius A*, the supermassive black hole at the
% Galactic Center of the Milky Way.
%
% See https://en.wikipedia.org/wiki/Sagittarius_A*.
%
-spec m_sagittarius_a_star() -> kilograms().
m_sagittarius_a_star() ->
	8.26e36.



% Radius subsection.


% @doc The radius of our planet, the Earth.
%
% See https://en.wikipedia.org/wiki/Earth_radius.
%
-spec r_earth() -> meters().
r_earth() ->
	6.3781e6.


% @doc The radius of our star, the Sun.
%
% See https://en.wikipedia.org/wiki/Sun_radius.
%
-spec r_sun() -> meters().
r_sun() ->
	6.957e8.
