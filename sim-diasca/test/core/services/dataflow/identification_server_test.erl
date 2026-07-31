% Copyright (C) 2017-2026 EDF R&D
%
% This file is part of Sim-Diasca.
%
% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: Thursday, March 16, 2017.

-module(identification_server_test).

-moduledoc """
The purpose of this module is to test the support provided by the
**identification server**.

See also the `class_IdentificationServer` module.
""".


% Not a simulation case, just a test:
-include("traces_for_tests.hrl").



% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").



-doc """
Runs the test, determining the settings from the command-line, otherwise using
defaults.
""".
-spec run() -> no_return().
run() ->

    ?test_start,

    IdSrvPid = class_IdentificationServer:start(),

    IdSrvPid ! { getStatus, [], self() },
    InitialStatusString = test_receive(),

    ?test_notice_fmt( "Initial status of server: ~ts",
                      [ InitialStatusString ] ),

    FirstBlockPid = self(),

    FirstExtId = class_IdentificationServer:forge_external_identifier(
                                            FirstBlockPid ),

    IdSrvPid ! { declareIdentifierAssociation,
                 [ FirstBlockPid, FirstExtId ], self() },

    identifier_association_declared = test_receive(),


    IdSrvPid ! { getExternalIdentifier, [ FirstBlockPid ], self() },

    FirstExtId = test_receive(),

    IdSrvPid ! { getBlockPID, [ FirstExtId ], self() },

    FirstBlockPid = test_receive(),

    IdSrvPid ! { getStatus, [], self() },
    IntermediateStatusString = test_receive(),

    ?test_notice_fmt( "Intermediate status of server: ~ts",
                      [ IntermediateStatusString ] ),



    % To have a different (quickly dead) PID:
    SecondBlockPid = ?myriad_spawn( fun() -> ok end ),

    SecondExtId =
        class_IdentificationServer:forge_external_identifier( SecondBlockPid ),

    IdAssociations = [ { FirstBlockPid, FirstExtId },
                       { SecondBlockPid, SecondExtId } ],

    IdSrvPid !
        { declareIdentifierAssociations, [ IdAssociations ], self() },

    identifier_associations_declared = test_receive(),

    BlockPids = [ SecondBlockPid, FirstBlockPid ],

    IdSrvPid ! { getExternalIdentifiers, [ BlockPids ], self() },

    [ SecondExtId, FirstExtId ] = test_receive(),


    % Also useful for synchronous operation of the test:
    IdSrvPid ! { getStatus, [], self() },
    FinalStatusString = test_receive(),

    ?test_notice_fmt( "Final status of server: ~ts", [ FinalStatusString ] ),

    class_IdentificationServer:stop(),

    ?test_stop.
