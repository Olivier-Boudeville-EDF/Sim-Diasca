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


% This is the main Sim-Diasca header file.
%
% It is defined to define a base for user code only, so that it has only to
% include a single header (sim_diasca_for_{models,cases}.hrl).
%
% Note: this main helper header file shall only list includes (i.e. it shall not
% define anything by itself) so that the Sim-Diasca code (as opposed to user
% code) never includes by convention that header (it always can include any of
% its needed subheaders instead).


% For all dataflow-related defines (includes common ones):
%-include("dataflow_defines.hrl").

% For all bindings-related defines:
%-include("bindings.hrl").

% For load_balancer_pid() and all:
%-include("class_LoadBalancer.hrl").

% For deployment_settings and all:
%-include("class_DeploymentManager.hrl").

% For all actor-related types:
%-include("class_TimeManager.hrl").

-include("engine_common_defines.hrl").

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").
