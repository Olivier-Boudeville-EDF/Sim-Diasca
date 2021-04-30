% Copyright (C) 2008-2021 EDF R&D

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


% Unit tests for the Graphable class implementation.
%
% See the class_Graphable module.
%
-module(class_Graphable_test).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").



% Runs the tests.
-spec run() -> no_return().
run() ->

	?case_start,

	?test_info( "Creating a new test Graphable." ),

	MyGraphable =
		class_Graphable:new_link( [	{ label, "hello" }, { color, red } ] ),


	MyGraphable ! { getNodeName, [], self() },
	NodeName = test_receive(),

	?test_notice_fmt( "Node name: ~ts.", [ NodeName ] ),


	MyGraphable ! { getLabel, [], self() },
	Label = test_receive(),

	?test_notice_fmt( "Label: ~ts.", [ Label ] ),


	MyGraphable ! { getGraphInformation, [], self() },
	{ _OtherNodeName, Infos } = test_receive(),

	?test_notice_fmt( "Graph information: ~p.", [ Infos ] ),

	wooper:delete_synchronously_instance( MyGraphable ),

	?case_stop.
