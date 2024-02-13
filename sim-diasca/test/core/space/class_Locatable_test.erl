% Copyright (C) 2008-2024 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]


% @doc Unit tests for the <b>Locatable</b> class implementation.
%
% See the class_Locatable module.
%
-module(class_Locatable_test).


% For facilities common to all cases:
-include_lib("traces/include/traces_for_tests.hrl").



% @doc Runs the tests.
-spec run() -> no_return().
run() ->

	?test_start,

	?test_info( "Creating a test Locatable." ),

	MyLocation = { 1, 2, 3 },

	MyLocatable = class_Locatable:new_link( MyLocation ),

	MyLocatable ! { getLocation, [], self() },
	MyLocation = test_receive(),

	Origin = linear_3D:get_origin(),

	?test_notice_fmt( "Origin is ~p.", [ Origin ] ),

	MyLocatable ! { setLocation, Origin },

	MyLocatable ! { getLocation, [], self() },
	Origin = test_receive(),

	?test_info( "setLocation succeeded." ),

	Abscissa = 7,
	MyLocatable ! { setAbscissa, Abscissa },

	MyLocatable ! { getAbscissa, [], self() },
	Abscissa = test_receive(),

	?test_info( "setAbscissa and getAbscissa succeeded." ),


	Ordinate = 17,
	Altitude = 22,

	MyLocatable ! { setOrdinate, Ordinate },
	MyLocatable ! { setAltitude, Altitude },

	FinalLocation = { Abscissa, Ordinate, Altitude },

	MyLocatable ! { getLocation, [], self() },
	FinalLocation = test_receive(),

	?test_info( "set/get for abscissa, ordinate and altitude succeeded." ),

	wooper:delete_synchronously_instance( MyLocatable ),

	?test_stop.
