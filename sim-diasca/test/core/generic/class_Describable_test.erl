% Copyright (C) 2008-2022 EDF R&D

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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)
% Creation date: 2008.


% @doc Unit tests for the <b>Describable</b> class implementation.
%
% See the class_Describable module.
%
-module(class_Describable_test).


% Not a simulation case per se, no Sim-Diasca include:
-include("traces_for_tests.hrl").



% @doc Runs the tests.
-spec run() -> no_return().
run() ->

	?test_start,

	?test_info( "Creating a test Describable." ),

	Description = "King Of Brittain",

	MyDescribable = class_Describable:new_link( Description ),


	MyDescribable ! { getDescription, [], self() },
	Description = test_receive(),

	?test_info( "Correct description returned." ),

	NewDescription = "King of the United Kingdom",

	MyDescribable ! { setDescription, [ NewDescription ] },

	MyDescribable ! { getDescription, [], self() },
	NewDescription = test_receive(),

	?test_info( "Correct new description returned." ),

	wooper:delete_synchronously_instance( MyDescribable ),

	?test_stop.
