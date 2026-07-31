% Copyright (C) 2016-2026 EDF R&D
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
% Creation date: Thursday, August 25, 2016.

-module(semantic_server_test).

-moduledoc """
The purpose of this module is to test the support provided by the semantic
server.

See also the `class_SemanticServer` module.
""".


% Not a simulation case, just a test:
-include("traces_for_tests.hrl").



-doc """
Runs the test, determining the settings from the command-line, otherwise using
defaults.
""".
-spec run() -> no_return().
run() ->

    ?test_start,

    SemanticServerPid = class_SemanticServer:start(),

    % Add 'wheels' to test a failure case:
    FirstWords = text_utils:strings_to_binaries(
        [ "wheel", "motor", "hood", "brakes" ] ),

    [ SemanticServerPid ! { declareSemantics, [ W ] } || W <- FirstWords ],


    SecondWords = text_utils:strings_to_binaries(
        [ "boat", "river", "fish", "wheels", "island" ] ),

    ValidationRequests = [ { validateSemantics, W } || W <- SecondWords ],

    ExpectedOutcomes = [ semantics_accepted, semantics_accepted,
                         semantics_accepted,

                         % Used to be rejected, at least temporarily now:
                         %{ semantics_rejected,
                         %  { semantics_too_close, {"wheels","wheel"} } },

                         semantics_accepted,

                         semantics_accepted ],

    ExpectedOutcomes = wooper:obtain_results_for_request_series(
        ValidationRequests, SemanticServerPid ),

    % Also useful for synchronous operation of the test:
    SemanticServerPid ! { getStatus, [], self() },
    StateString = test_receive(),

    ?test_notice_fmt( "Current state of server: ~ts", [ StateString ] ),

    class_SemanticServer:stop(),

    ?test_stop.
