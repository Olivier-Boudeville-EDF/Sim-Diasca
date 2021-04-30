% Copyright (C) 2012-2021 EDF R&D

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


% This is the main Sim-Diasca header file for simulation cases.
%
% It is defined so that actual cases can include a single header, this one.


% For facilities common to all cases:
-include("case_constructs.hrl").

% A base used for models as well:
-include("sim_diasca_base.hrl").

% For deployment_settings and al:
-include("class_DeploymentManager.hrl").

% For simulation_settings and al:
-include("class_TimeManager.hrl").

% For load_balancing_settings and al:
-include("class_LoadBalancer.hrl").
