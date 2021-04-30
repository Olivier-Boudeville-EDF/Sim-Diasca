% Copyright (C) 2016-2021 EDF R&D

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

% This file is part of Sim-Diasca, on behalf of the EDF City Simulation
% project. It does not pertain to the free software release of Sim-Diasca.

% Creation date: Thursday, March 16, 2017
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% The purpose of this module is to test the support provided by the
% identification server.
%
% See also class_IdentificationServer.erl.
%
-module(identification_server_test).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").



% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").



% Runs the test, determining the settings from the command-line, otherwise using
% defaults.
%
-spec run() -> no_return().
run() ->

	?case_start,

	IdentificationServerPid = class_IdentificationServer:start(),

	IdentificationServerPid ! { getStatus, [], self() },
	InitialStatusString = test_receive(),

	?test_notice_fmt( "Initial status of server: ~s", [ InitialStatusString ] ),


	FirstBlockPid = self(),

	FirstExternalID = class_IdentificationServer:forge_external_identifier(
						FirstBlockPid ),

	IdentificationServerPid ! { declareIdentifierAssociation,
								[ FirstBlockPid, FirstExternalID ], self() },

	identifier_association_declared = test_receive(),


	IdentificationServerPid ! { getExternalIdentifier, [ FirstBlockPid ],
								self() },

	FirstExternalID = test_receive(),

	IdentificationServerPid ! { getBlockPID, [ FirstExternalID ], self() },

	FirstBlockPid = test_receive(),

	IdentificationServerPid ! { getStatus, [], self() },
	IntermediateStatusString = test_receive(),

	?test_notice_fmt( "Intermediate status of server: ~s",
					[ IntermediateStatusString ] ),



	% To have a different (quickly dead) PID:
	SecondBlockPid = ?myriad_spawn( fun() -> ok end ),

	SecondExternalID = class_IdentificationServer:forge_external_identifier(
						 SecondBlockPid ),

	IdAssociations = [ { FirstBlockPid, FirstExternalID },
					   { SecondBlockPid, SecondExternalID } ],

	IdentificationServerPid ! { declareIdentifierAssociations,
								[ IdAssociations ],	self() },

	identifier_associations_declared = test_receive(),

	BlockPids = [ SecondBlockPid, FirstBlockPid ],

	IdentificationServerPid ! { getExternalIdentifiers, [ BlockPids ], self() },

	[ SecondExternalID, FirstExternalID ] = test_receive(),


	% Also useful for synchronous operation of the test:
	IdentificationServerPid ! { getStatus, [], self() },
	FinalStatusString = test_receive(),

	?test_notice_fmt( "Final status of server: ~s", [ FinalStatusString ] ),

	class_IdentificationServer:stop(),

	?case_stop.
