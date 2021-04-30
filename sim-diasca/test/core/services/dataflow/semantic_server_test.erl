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

% Creation date: Thursday, August 25, 2016
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% The purpose of this module is to test the support provided by the semantic
% server.
%
% See also class_SemanticServer.erl.
%
-module(semantic_server_test).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").



% Runs the test, determining the settings from the command-line, otherwise using
% defaults.
%
-spec run() -> no_return().
run() ->

	?case_start,

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

	?test_notice_fmt( "Current state of server: ~s", [ StateString ] ),

	class_SemanticServer:stop(),

	?case_stop.
