% Copyright (C) 2008-2023 EDF R&D

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
% Creation date: 2008.


% @doc Unit tests for the <b>Graphable</b> class implementation.
%
% See the class_Graphable module.
%
-module(class_Graphable_test).


% Not a simulation case per se, no Sim-Diasca include:
-include("traces_for_tests.hrl").



% @doc Runs the tests.
-spec run() -> no_return().
run() ->

	?test_start,

	?test_info( "Creating a test Graphable." ),

	MyGraphable =
		class_Graphable:new_link( [ { label, "hello" }, { color, red } ] ),


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

	?test_stop.
