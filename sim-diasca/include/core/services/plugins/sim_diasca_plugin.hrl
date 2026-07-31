% Copyright (C) 2014-2026 EDF R&D
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
% Creation date: 2014.


% Describes a request of configuration change, addressed by a plugin to the
% engine.
%
-record( configuration_changes, {

    % Requests the computing nodes that will be spawned to rely on the specified
    % number of sequencers (number of scheduler threads to create and scheduler
    % threads to set online; no more than 1024).
    %
    % See also: http://erlang.org/doc/man/erl.html
    %
    compute_scheduler_count = undefined :: option( basic_utils:count() ) } ).



% Describes the technical settings to be shared by the engine with the plugins.
-record( technical_settings, {

    % A list of the computing nodes actually used:
    %
    % (user node is the one on which plugins run)
    %
    computing_nodes = [] :: [ net_utils:atom_node_name() ],


    % Cookie used by this instance of the engine:
    cookie :: net_utils:cookie() } ).
